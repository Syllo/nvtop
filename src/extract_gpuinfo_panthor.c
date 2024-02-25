/*
 *
 * Copyright (C) 2023 Adrian Larumbe <adrian.larumbe@collabora.com>
 *
 * This file is part of Nvtop and adapted from the msm implementation.
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
#include <xf86drm.h>

#include "nvtop/device_discovery.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/time.h"

#include "panthor_drm.h"
#include "panthor_utils.h"
#include "mali_common.h"

static bool gpuinfo_panthor_init(void);
static void gpuinfo_panthor_shutdown(void);
static const char *gpuinfo_panthor_last_error_string(void);
static bool gpuinfo_panthor_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_panthor_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_panthor_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_panthor_get_running_processes(struct gpu_info *_gpu_info);

static struct gpu_vendor gpu_vendor_panthor = {
    .init = gpuinfo_panthor_init,
    .shutdown = gpuinfo_panthor_shutdown,
    .last_error_string = gpuinfo_panthor_last_error_string,
    .get_device_handles = gpuinfo_panthor_get_device_handles,
    .populate_static_info = gpuinfo_panthor_populate_static_info,
    .refresh_dynamic_info = gpuinfo_panthor_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_panthor_get_running_processes,
    .refresh_utilisation_rate = gpuinfo_refresh_utilisation_rate,
    .name = "panthor",
};

static struct drmFuncTable drmFuncs;
static struct mali_gpu_state mali_state;

__attribute__((constructor)) static void init_extract_gpuinfo_panthor(void) { register_gpu_vendor(&gpu_vendor_panthor); }

bool gpuinfo_panthor_init(void) {
  return mali_init_drm_funcs(&drmFuncs, &mali_state);
}

void gpuinfo_panthor_shutdown(void) {
  mali_shutdown_common(&mali_state, &drmFuncs);
}

static const char *gpuinfo_panthor_last_error_string(void) {
  static char driver_msg[MAX_ERR_STRING_LEN] = {0};

  return mali_common_last_error_string(&mali_state,
                                       gpu_vendor_panthor.name,
                                       driver_msg);
}

static void panthor_check_fdinfo_keys (bool *is_engine, bool *is_cycles,
                                        bool *is_maxfreq, bool *is_curfreq,
                                        bool *is_resident, char *key)
{
static const char drm_panthor_engine[] = "drm-engine-panthor";
static const char drm_panthor_cycles[] = "drm-cycles-panthor";
static const char drm_panthor_maxfreq[] = "drm-maxfreq-panthor";
static const char drm_panthor_curfreq[] = "drm-curfreq-panthor";
static const char drm_panthor_resident_mem[] = "drm-resident-memory";

  *is_engine = !strcmp(key, drm_panthor_engine);
  *is_cycles = !strcmp(key, drm_panthor_cycles);
  *is_maxfreq = !strcmp(key, drm_panthor_maxfreq);
  *is_curfreq = !strcmp(key, drm_panthor_curfreq);
  *is_resident = !strcmp(key, drm_panthor_resident_mem);
}

static bool parse_drm_fdinfo_panthor(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {

  struct gpu_info_mali *gpu_info = container_of(info, struct gpu_info_mali, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  struct fdinfo_data res = {0};

  nvtop_time current_time;
  nvtop_get_current_time(&current_time);

  if (!mali_common_parse_drm_fdinfo(info, fdinfo_file, process_info, dynamic_info,
                                    panthor_check_fdinfo_keys, &res))
    return false;

  mali_common_parse_fdinfo_handle_cache(gpu_info, process_info, current_time, res.total_cycles,
					res.cid, res.engine_count >= 1 ? true : false);

  return true;
}

static bool gpuinfo_panthor_get_device_handles(struct list_head *devices, unsigned *count) {
        return mali_common_get_device_handles(&mali_state, &drmFuncs, &gpu_vendor_panthor,
                                              parse_drm_fdinfo_panthor, devices, count,
                                              NULL, MALI_PANTHOR);
}

void gpuinfo_panthor_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_mali *gpu_info = container_of(_gpu_info, struct gpu_info_mali, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;

  if (gpu_info->version != MALI_PANTHOR) {
    fprintf(stderr, "Wrong device version: %u\n", gpu_info->version);
    abort();
  }

  static_info->integrated_graphics = true;
  static_info->encode_decode_shared = true;
  RESET_ALL(static_info->valid);

  struct drm_panthor_gpu_info gpu_dev_info = {0};
  struct drm_panthor_dev_query query = {
      .type = DRM_PANTHOR_DEV_QUERY_GPU_INFO,
      .size = sizeof(gpu_dev_info),
      .pointer = (uint64_t)(uintptr_t)&gpu_dev_info,
   };

  int ret = drmFuncs.drmIoctl(gpu_info->fd, DRM_IOCTL_PANTHOR_DEV_QUERY, &query);
  if (ret) {
          fprintf(stderr, "Failed to query Panthor GPU device properties\n");
          snprintf(static_info->device_name, sizeof(static_info->device_name),
                   "Unknown Panthor %x", gpu_dev_info.gpu_id);
          SET_VALID(gpuinfo_device_name_valid, static_info->valid);
          return;
   }

   const char *name = panthor_device_name(gpu_dev_info.gpu_id);
   if (name)
           strncpy(static_info->device_name, name, sizeof(static_info->device_name));
   else
           snprintf(static_info->device_name, sizeof(static_info->device_name),
                    "Unknown Panthor %x", gpu_dev_info.gpu_id);

   SET_VALID(gpuinfo_device_name_valid, static_info->valid);
}

void gpuinfo_panthor_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  static const char *meminfo_total = "MemTotal";
  static const char *meminfo_available = "MemAvailable";

  struct gpu_info_mali *gpu_info = container_of(_gpu_info, struct gpu_info_mali, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  if (gpu_info->version != MALI_PANTHOR) {
    fprintf(stderr, "Wrong device version: %u\n", gpu_info->version);
    abort();
  }

  mali_common_refresh_dynamic_info(dynamic_info, &mali_state, meminfo_total, meminfo_available);
}

void gpuinfo_panthor_get_running_processes(struct gpu_info *_gpu_info) {
  mali_common_get_running_processes(_gpu_info, MALI_PANTHOR);
}
