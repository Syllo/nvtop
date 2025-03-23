/*
 *
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
 *
 * This file is part of Nvtop and adapted from igt-gpu-tools from Intel Corporation.
 *
 * Nvtop is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Nvtop is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with nvtop.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "nvtop/device_discovery.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/time.h"

#include "extract_gpuinfo_intel.h"

#include <assert.h>
#include <libdrm/drm.h>
#include <libdrm/xe_drm.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <uthash.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

// Copied from https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/intel/common/intel_gem.h
static inline int intel_ioctl(int fd, unsigned long request, void *arg) {
  int ret;

  do {
    ret = ioctl(fd, request, arg);
  } while (ret == -1 && (errno == EINTR || errno == EAGAIN));
  return ret;
}
// End Copy

// Copied from https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/intel/common/xe/intel_device_query.c
static void *xe_device_query_alloc_fetch(int fd, uint32_t query_id, uint32_t *len) {
  struct drm_xe_device_query query = {
      .query = query_id,
  };
  if (intel_ioctl(fd, DRM_IOCTL_XE_DEVICE_QUERY, &query))
    return NULL;

  void *data = calloc(1, query.size);
  if (!data)
    return NULL;

  query.data = (uintptr_t)data;
  if (intel_ioctl(fd, DRM_IOCTL_XE_DEVICE_QUERY, &query)) {
    free(data);
    return NULL;
  }

  if (len)
    *len = query.size;
  return data;
}
// End Copy

void gpuinfo_intel_xe_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_intel *gpu_info = container_of(_gpu_info, struct gpu_info_intel, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  if (gpu_info->card_fd) {
    uint32_t length = 0;
    struct drm_xe_query_mem_regions *regions =
        xe_device_query_alloc_fetch(gpu_info->card_fd, DRM_XE_DEVICE_QUERY_MEM_REGIONS, &length);
    if (regions) {
      for (unsigned i = 0; i < regions->num_mem_regions; i++) {
        struct drm_xe_mem_region mr = regions->mem_regions[i];
        // ARC will have VRAM and SYSMEM, integrated graphics will have only one SYSMEM region
        if (mr.mem_class == DRM_XE_MEM_REGION_CLASS_VRAM || regions->num_mem_regions == 1) {
          SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mr.total_size);
          // xe will report 0 kb used if we don't have CAP_PERFMON
          if (mr.used != 0) {
            SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, mr.used);
            SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, dynamic_info->total_memory - dynamic_info->used_memory);
            SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, dynamic_info->used_memory * 100 / dynamic_info->total_memory);
          }
          break;
        }
      }
      free(regions);
    }
  }
}

static const char xe_drm_intel_vram[] = "drm-total-vram0";
// static const char xe_drm_intel_gtt[] = "drm-total-gtt";
// Render
static const char xe_drm_intel_cycles_rcs[] = "drm-cycles-rcs";
static const char xe_drm_intel_total_cycles_rcs[] = "drm-total-cycles-rcs";
// Video Decode
static const char xe_drm_intel_cycles_vcs[] = "drm-cycles-vcs";
static const char xe_drm_intel_total_cycles_vcs[] = "drm-total-cycles-vcs";
// Video Enhance
static const char xe_drm_intel_cycles_vecs[] = "drm-cycles-vecs";
static const char xe_drm_intel_total_cycles_vecs[] = "drm-total-cycles-vecs";
// Copy
static const char xe_drm_intel_cycles_bcs[] = "drm-cycles-bcs";
static const char xe_drm_intel_total_cycles_bcs[] = "drm-total-cycles-bcs";
// Compute
static const char xe_drm_intel_cycles_ccs[] = "drm-cycles-ccs";
static const char xe_drm_intel_total_cycles_ccs[] = "drm-total-cycles-ccs";

static const char *cycles_keys[] = {xe_drm_intel_cycles_rcs, xe_drm_intel_cycles_vcs, xe_drm_intel_cycles_vecs,
                                    xe_drm_intel_cycles_bcs, xe_drm_intel_cycles_ccs};

static const char *total_cycles_keys[] = {xe_drm_intel_total_cycles_rcs, xe_drm_intel_total_cycles_vcs,
                                          xe_drm_intel_total_cycles_vecs, xe_drm_intel_total_cycles_bcs,
                                          xe_drm_intel_total_cycles_ccs};

bool parse_drm_fdinfo_intel_xe(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {
  struct gpu_info_intel *gpu_info = container_of(info, struct gpu_info_intel, base);
  static char *line = NULL;
  static size_t line_buf_size = 0;
  ssize_t count = 0;

  bool client_id_set = false;
  unsigned cid;
  nvtop_time current_time;
  nvtop_get_current_time(&current_time);

  union intel_cycles gpu_cycles = {.array = {0}};

  union intel_cycles total_cycles = {.array = {0}};

  while ((count = getline(&line, &line_buf_size, fdinfo_file)) != -1) {
    char *key, *val;
    // Get rid of the newline if present
    if (line[count - 1] == '\n') {
      line[--count] = '\0';
    }

    if (!extract_drm_fdinfo_key_value(line, &key, &val))
      continue;

    if (!strcmp(key, drm_pdev)) {
      if (strcmp(val, gpu_info->base.pdev)) {
        return false;
      }
    } else if (!strcmp(key, drm_client_id)) {
      char *endptr;
      cid = strtoul(val, &endptr, 10);
      if (*endptr)
        continue;
      client_id_set = true;
    } else {
      if (!strcmp(key, xe_drm_intel_vram)) {
        unsigned long mem_int;
        char *endptr;

        mem_int = strtoul(val, &endptr, 10);
        if (endptr == val || (strcmp(endptr, " kB") && strcmp(endptr, " KiB")))
          continue;

        if GPUINFO_PROCESS_FIELD_VALID (process_info, gpu_memory_usage)
          SET_GPUINFO_PROCESS(process_info, gpu_memory_usage, process_info->gpu_memory_usage + (mem_int * 1024));
        else
          SET_GPUINFO_PROCESS(process_info, gpu_memory_usage, mem_int * 1024);
      } else {
        unsigned long cycles;
        char *endptr;

        // Check for cycles
        for (unsigned i = 0; i < ARRAY_SIZE(gpu_cycles.array); i++) {
          if (!strcmp(key, cycles_keys[i])) {
            cycles = strtoull(val, &endptr, 10);
            gpu_cycles.array[i] = cycles;
          }
        }

        // Check for total cycles
        for (unsigned i = 0; i < ARRAY_SIZE(total_cycles_keys); i++) {
          if (!strcmp(key, total_cycles_keys[i])) {
            cycles = strtoull(val, &endptr, 10);
            total_cycles.array[i] = cycles;
          }
        }
      }
    }
  }

  // Sum cycles for overall usage
  {
    uint64_t cycles_sum = 0;
    for (unsigned i = 0; i < ARRAY_SIZE(gpu_cycles.array); i++) {
      cycles_sum += gpu_cycles.array[i];
    }
    SET_GPUINFO_PROCESS(process_info, gpu_cycles, cycles_sum);
  }

  if (!client_id_set)
    return false;

  process_info->type = gpu_process_unknown;
  if (gpu_cycles.rcs != 0)
    process_info->type |= gpu_process_graphical;
  if (gpu_cycles.ccs != 0)
    process_info->type |= gpu_process_compute;

  struct intel_process_info_cache *cache_entry;
  struct unique_cache_id ucid = {.client_id = cid, .pid = process_info->pid, .pdev = gpu_info->base.pdev};
  HASH_FIND_CLIENT(gpu_info->last_update_process_cache, &ucid, cache_entry);
  if (cache_entry) {
    HASH_DEL(gpu_info->last_update_process_cache, cache_entry);

    // TODO: find how to extract global utilization
    // gpu util will be computed as the sum of all the processes utilization for now
    {
      uint64_t cycles_delta = gpu_cycles.rcs - cache_entry->gpu_cycles.rcs;
      uint64_t total_cycles_delta = total_cycles.rcs - cache_entry->total_cycles.rcs;
      if (total_cycles_delta > 0)
        SET_GPUINFO_PROCESS(process_info, gpu_usage, cycles_delta * 100 / total_cycles_delta);
      else
        SET_GPUINFO_PROCESS(process_info, gpu_usage, 0);
    }
    {
      uint64_t cycles_delta = gpu_cycles.ccs - cache_entry->gpu_cycles.ccs;
      uint64_t total_cycles_delta = total_cycles.ccs - cache_entry->total_cycles.ccs;
      if (total_cycles_delta > 0)
        SET_GPUINFO_PROCESS(process_info, gpu_usage, process_info->gpu_usage + cycles_delta * 100 / total_cycles_delta);
    }
    {
      uint64_t cycles_delta = gpu_cycles.vcs - cache_entry->gpu_cycles.vcs;
      uint64_t total_cycles_delta = total_cycles.vcs - cache_entry->total_cycles.vcs;
      if (total_cycles_delta > 0)
        SET_GPUINFO_PROCESS(process_info, decode_usage, cycles_delta * 100 / total_cycles_delta);
    }

  } else {
    cache_entry = calloc(1, sizeof(*cache_entry));
    if (!cache_entry)
      goto parse_fdinfo_exit;
    cache_entry->client_id.client_id = cid;
    cache_entry->client_id.pid = process_info->pid;
    cache_entry->client_id.pdev = gpu_info->base.pdev;
  }

#ifndef NDEBUG
  // We should only process one fdinfo entry per client id per update
  struct intel_process_info_cache *cache_entry_check;
  HASH_FIND_CLIENT(gpu_info->current_update_process_cache, &cache_entry->client_id, cache_entry_check);
  assert(!cache_entry_check && "We should not be processing a client id twice per update");
#endif

  RESET_ALL(cache_entry->valid);
  SET_INTEL_CACHE(cache_entry, gpu_cycles, gpu_cycles);
  SET_INTEL_CACHE(cache_entry, total_cycles, total_cycles);

  HASH_ADD_CLIENT(gpu_info->current_update_process_cache, cache_entry);

parse_fdinfo_exit:
  return true;
}