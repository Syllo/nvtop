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
#include <drm/drm.h>
#include <drm/xe_drm.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <uthash.h>

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
    int32_t length = 0;
    struct drm_xe_query_mem_regions *regions =
        xe_device_query_alloc_fetch(gpu_info->card_fd, DRM_XE_DEVICE_QUERY_MEM_REGIONS, &length);
    if (regions) {
      for (unsigned i = 0; i < regions->num_mem_regions; i++) {
        struct drm_xe_mem_region mr = regions->mem_regions[i];
        if (mr.mem_class == DRM_XE_MEM_REGION_CLASS_VRAM) {
          SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mr.total_size);
          SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, mr.used);
          SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, dynamic_info->total_memory - dynamic_info->used_memory);
          SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, dynamic_info->used_memory * 100 / dynamic_info->total_memory);
          break;
        }
      }
      free(regions);
    }
  }
}

static const char xe_drm_intel_vram[] = "drm-total-vram0";
static const char xe_drm_intel_cycles[] = "drm-cycles-rcs";
static const char xe_drm_intel_total_cycles[] = "drm-total-cycles-rcs";

bool parse_drm_fdinfo_intel_xe(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {
  struct gpu_info_intel *gpu_info = container_of(info, struct gpu_info_intel, base);
  static char *line = NULL;
  static size_t line_buf_size = 0;
  ssize_t count = 0;

  bool client_id_set = false;
  unsigned cid;
  nvtop_time current_time;
  nvtop_get_current_time(&current_time);

  uint64_t total_cycles = 0;

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
        // TODO: do we count "gtt mem" too?
        unsigned long mem_int;
        char *endptr;

        mem_int = strtoul(val, &endptr, 10);
        if (endptr == val || (strcmp(endptr, " kB") && strcmp(endptr, " KiB")))
            continue;

        SET_GPUINFO_PROCESS(process_info, gpu_memory_usage, mem_int * 1024);
      } else if (!strcmp(key, xe_drm_intel_cycles)) {
        unsigned long cycles;
        char *endptr;

        cycles = strtoull(val, &endptr, 10);

        SET_GPUINFO_PROCESS(process_info, gpu_cycles, cycles);
      } else if (!strcmp(key, xe_drm_intel_total_cycles)) {
        unsigned long cycles;
        char *endptr;

        cycles = strtoull(val, &endptr, 10);

        total_cycles = cycles;
      }
    }
  }
  if (!client_id_set)
    return false;
  // The intel driver does not expose compute engine metrics as of yet
  process_info->type |= gpu_process_graphical;

  struct intel_process_info_cache *cache_entry;
  struct unique_cache_id ucid = {.client_id = cid, .pid = process_info->pid, .pdev = gpu_info->base.pdev};
  HASH_FIND_CLIENT(gpu_info->last_update_process_cache, &ucid, cache_entry);
  if (cache_entry) {
    HASH_DEL(gpu_info->last_update_process_cache, cache_entry);
    uint64_t cycles = process_info->gpu_cycles;
    uint64_t cycles_delta = cycles - cache_entry->cycles;
    uint64_t total_cycles_delta = total_cycles - cache_entry->total_cycles;
    SET_GPUINFO_PROCESS(process_info, gpu_usage, cycles_delta * 100 / total_cycles_delta);

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
  SET_INTEL_CACHE(cache_entry, cycles, process_info->gpu_cycles);
  SET_INTEL_CACHE(cache_entry, total_cycles, total_cycles);

  HASH_ADD_CLIENT(gpu_info->current_update_process_cache, cache_entry);

parse_fdinfo_exit:
  return true;
}