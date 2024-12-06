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
#include <drm/i915_drm.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

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

static inline void *intel_i915_query_alloc(int fd, uint64_t query_id, int32_t *query_length) {
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
        intel_i915_query_alloc(gpu_info->card_fd, DRM_I915_QUERY_MEMORY_REGIONS, &length);
    if (regions) {
      for (unsigned i = 0; i < regions->num_regions; i++) {
        struct drm_i915_memory_region_info mr = regions->regions[i];
        if (mr.region.memory_class == I915_MEMORY_CLASS_DEVICE) {
          SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mr.probed_size);
          SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, mr.unallocated_size);
          SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, dynamic_info->total_memory - dynamic_info->free_memory);
          break;
        }
      }
      free(regions);
    }
  }
}
