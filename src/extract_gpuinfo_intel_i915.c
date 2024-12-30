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
#include <errno.h>
#include <fcntl.h>
#include <libdrm/drm.h>
#include <libdrm/i915_drm.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

#ifndef DRM_I915_QUERY_MEMORY_REGIONS
// The versions of libdrm < 2.4.114 don't provide
// DRM_I915_QUERY_MEMORY_REGIONS.
// Copied from more recent libdrm/i915_drm.h

#define DRM_I915_QUERY_MEMORY_REGIONS 4
#define I915_MEMORY_CLASS_DEVICE 1
struct drm_i915_memory_region_info {
  struct {
    __u16 memory_class;
    __u16 memory_instance;
  } region;
  __u32 rsvd0;
  __u64 probed_size;
  __u64 unallocated_size;
  union {
    __u64 rsvd1[8];
    struct {
      __u64 probed_cpu_visible_size;
      __u64 unallocated_cpu_visible_size;
    };
  };
};
struct drm_i915_query_memory_regions {
  __u32 num_regions;
  __u32 rsvd[3];
  struct drm_i915_memory_region_info regions[];
};
#endif // not defined(DRM_I915_QUERY_MEMORY_REGIONS)

// Copied from https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/intel/common/intel_gem.h
static inline int intel_ioctl(int fd, unsigned long request, void *arg) {
  int ret;

  do {
    ret = ioctl(fd, request, arg);
  } while (ret == -1 && (errno == EINTR || errno == EAGAIN));
  return ret;
}
// End Copy

// Copied from https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/intel/common/i915/intel_gem.h
static inline int intel_i915_query(int fd, uint64_t query_id, void *buffer, int32_t *buffer_len) {
  struct drm_i915_query_item item = {
      .query_id = query_id,
      .length = *buffer_len,
      .flags = 0,
      .data_ptr = (uintptr_t)buffer,
  };

  struct drm_i915_query args = {
      .num_items = 1,
      .flags = 0,
      .items_ptr = (uintptr_t)&item,
  };

  int ret = intel_ioctl(fd, DRM_IOCTL_I915_QUERY, &args);
  if (ret != 0)
    return -errno;
  else if (item.length < 0)
    return item.length;

  *buffer_len = item.length;
  return 0;
}

static inline void *intel_i915_query_alloc_fetch(int fd, uint64_t query_id, int32_t *query_length) {
  if (query_length)
    *query_length = 0;

  int32_t length = 0;
  int ret = intel_i915_query(fd, query_id, NULL, &length);
  if (ret < 0)
    return NULL;

  void *data = calloc(1, length);
  assert(data != NULL); /* This shouldn't happen in practice */
  if (data == NULL)
    return NULL;

  ret = intel_i915_query(fd, query_id, data, &length);
  assert(ret == 0); /* We should have caught the error above */
  if (ret < 0) {
    free(data);
    return NULL;
  }

  if (query_length)
    *query_length = length;

  return data;
}
// End Copy

void gpuinfo_intel_i915_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_intel *gpu_info = container_of(_gpu_info, struct gpu_info_intel, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  if (gpu_info->card_fd) {
    int32_t length = 0;
    struct drm_i915_query_memory_regions *regions =
        intel_i915_query_alloc_fetch(gpu_info->card_fd, DRM_I915_QUERY_MEMORY_REGIONS, &length);
    if (regions) {
      for (unsigned i = 0; i < regions->num_regions; i++) {
        struct drm_i915_memory_region_info mr = regions->regions[i];
        // ARC will have device memory and system memory, integrated graphics will have only one system region
        if (mr.region.memory_class == I915_MEMORY_CLASS_DEVICE || regions->num_regions == 1) {
          SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mr.probed_size);
          // i915 will report the total memory as the unallocated size if we don't have CAP_PERFMON
          if (mr.unallocated_size != mr.probed_size) {
            SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, mr.unallocated_size);
            SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, dynamic_info->total_memory - dynamic_info->free_memory);
            SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate,
                                dynamic_info->used_memory * 100 / dynamic_info->total_memory);
          }
          break;
        }
      }
      free(regions);
    }
  }
}

static const char i915_drm_intel_render[] = "drm-engine-render";
static const char i915_drm_intel_copy[] = "drm-engine-copy";
static const char i915_drm_intel_video[] = "drm-engine-video";
static const char i915_drm_intel_video_enhance[] = "drm-engine-video-enhance";
static const char i915_drm_intel_compute[] = "drm-engine-compute";
static const char i915_drm_intel_vram[] = "drm-total-local0";
static const char i915_drm_intel_gtt[] = "drm-total-system0";

bool parse_drm_fdinfo_intel_i915(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {
  struct gpu_info_intel *gpu_info = container_of(info, struct gpu_info_intel, base);
  static char *line = NULL;
  static size_t line_buf_size = 0;
  ssize_t count = 0;

  bool client_id_set = false;
  unsigned cid;
  nvtop_time current_time;
  nvtop_get_current_time(&current_time);

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
      bool is_render = !strcmp(key, i915_drm_intel_render);
      bool is_copy = !strcmp(key, i915_drm_intel_copy);
      bool is_video = !strcmp(key, i915_drm_intel_video);
      bool is_video_enhance = !strcmp(key, i915_drm_intel_video_enhance);
      bool is_compute = !strcmp(key, i915_drm_intel_compute);

      if (!strcmp(key, i915_drm_intel_vram) || !strcmp(key, i915_drm_intel_gtt)) {
        unsigned long mem_int;
        char *endptr;

        mem_int = strtoul(val, &endptr, 10);
        if (endptr == val || (strcmp(endptr, " kB") && strcmp(endptr, " KiB")))
          continue;

        if GPUINFO_PROCESS_FIELD_VALID (process_info, gpu_memory_usage)
          SET_GPUINFO_PROCESS(process_info, gpu_memory_usage, process_info->gpu_memory_usage + (mem_int * 1024));
        else
          SET_GPUINFO_PROCESS(process_info, gpu_memory_usage, mem_int * 1024);

      } else if (is_render || is_copy || is_video || is_video_enhance || is_compute) {
        char *endptr;
        uint64_t time_spent = strtoull(val, &endptr, 10);
        if (endptr == val || strcmp(endptr, " ns"))
          continue;
        if (is_render) {
          SET_GPUINFO_PROCESS(process_info, gfx_engine_used, time_spent);
        }
        if (is_copy) {
          // TODO: what is copy?
        }
        if (is_video) {
          // Video represents encode and decode
          SET_GPUINFO_PROCESS(process_info, dec_engine_used, time_spent);
          SET_GPUINFO_PROCESS(process_info, enc_engine_used, time_spent);
        }
        if (is_video_enhance) {
          // TODO: what is this
        }
        if (is_compute) {
          SET_GPUINFO_PROCESS(process_info, compute_engine_used, time_spent);
        }
      }
    }
  }
  if (!client_id_set)
    return false;

  process_info->type = gpu_process_unknown;
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, gfx_engine_used) && process_info->gfx_engine_used > 0)
    process_info->type |= gpu_process_graphical;
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, compute_engine_used) && process_info->compute_engine_used > 0)
    process_info->type |= gpu_process_compute;

  struct intel_process_info_cache *cache_entry;
  struct unique_cache_id ucid = {.client_id = cid, .pid = process_info->pid, .pdev = gpu_info->base.pdev};
  HASH_FIND_CLIENT(gpu_info->last_update_process_cache, &ucid, cache_entry);
  // TODO: find how to extract global utilization
  // gpu util will be computed as the sum of all the processes utilization for now
  if (cache_entry) {
    uint64_t time_elapsed = nvtop_difftime_u64(cache_entry->last_measurement_tstamp, current_time);
    HASH_DEL(gpu_info->last_update_process_cache, cache_entry);
    if (GPUINFO_PROCESS_FIELD_VALID(process_info, gfx_engine_used) &&
        INTEL_CACHE_FIELD_VALID(cache_entry, engine_render) &&
        // In some rare occasions, the gfx engine usage reported by the driver is lowering (might be a driver bug)
        process_info->gfx_engine_used >= cache_entry->engine_render &&
        process_info->gfx_engine_used - cache_entry->engine_render <= time_elapsed) {
      SET_GPUINFO_PROCESS(
          process_info, gpu_usage,
          busy_usage_from_time_usage_round(process_info->gfx_engine_used, cache_entry->engine_render, time_elapsed));
    }
    if (GPUINFO_PROCESS_FIELD_VALID(process_info, dec_engine_used) &&
        INTEL_CACHE_FIELD_VALID(cache_entry, engine_video) &&
        process_info->dec_engine_used >= cache_entry->engine_video &&
        process_info->dec_engine_used - cache_entry->engine_video <= time_elapsed) {
      SET_GPUINFO_PROCESS(
          process_info, decode_usage,
          busy_usage_from_time_usage_round(process_info->dec_engine_used, cache_entry->engine_video, time_elapsed));
    }
    if (GPUINFO_PROCESS_FIELD_VALID(process_info, enc_engine_used) &&
        INTEL_CACHE_FIELD_VALID(cache_entry, engine_video_enhance) &&
        process_info->enc_engine_used >= cache_entry->engine_video_enhance &&
        process_info->enc_engine_used - cache_entry->engine_video_enhance <= time_elapsed) {
      SET_GPUINFO_PROCESS(process_info, encode_usage,
                          busy_usage_from_time_usage_round(process_info->enc_engine_used,
                                                           cache_entry->engine_video_enhance, time_elapsed));
    }
    if (GPUINFO_PROCESS_FIELD_VALID(process_info, compute_engine_used) &&
        GPUINFO_PROCESS_FIELD_VALID(process_info, gpu_usage) && INTEL_CACHE_FIELD_VALID(cache_entry, engine_compute) &&
        process_info->compute_engine_used >= cache_entry->engine_compute &&
        process_info->compute_engine_used - cache_entry->engine_compute <= time_elapsed) {
      SET_GPUINFO_PROCESS(process_info, gpu_usage,
                          process_info->gpu_usage + busy_usage_from_time_usage_round(process_info->compute_engine_used,
                                                                                     cache_entry->engine_compute,
                                                                                     time_elapsed));
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
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, gfx_engine_used))
    SET_INTEL_CACHE(cache_entry, engine_render, process_info->gfx_engine_used);
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, dec_engine_used))
    SET_INTEL_CACHE(cache_entry, engine_video, process_info->dec_engine_used);
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, enc_engine_used))
    SET_INTEL_CACHE(cache_entry, engine_video_enhance, process_info->enc_engine_used);
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, compute_engine_used))
    SET_INTEL_CACHE(cache_entry, engine_compute, process_info->compute_engine_used);

  cache_entry->last_measurement_tstamp = current_time;
  HASH_ADD_CLIENT(gpu_info->current_update_process_cache, cache_entry);

parse_fdinfo_exit:
  return true;
}