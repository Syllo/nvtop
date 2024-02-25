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

#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <xf86drm.h>

#include "nvtop/device_discovery.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/time.h"

#include "panfrost_drm.h"
#include "panfrost_utils.h"
#include "mali_common.h"

static bool gpuinfo_panfrost_init(void);
static void gpuinfo_panfrost_shutdown(void);
static const char *gpuinfo_panfrost_last_error_string(void);
static bool gpuinfo_panfrost_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_panfrost_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_panfrost_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_panfrost_get_running_processes(struct gpu_info *_gpu_info);

static struct gpu_vendor gpu_vendor_panfrost = {
    .init = gpuinfo_panfrost_init,
    .shutdown = gpuinfo_panfrost_shutdown,
    .last_error_string = gpuinfo_panfrost_last_error_string,
    .get_device_handles = gpuinfo_panfrost_get_device_handles,
    .populate_static_info = gpuinfo_panfrost_populate_static_info,
    .refresh_dynamic_info = gpuinfo_panfrost_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_panfrost_get_running_processes,
    .refresh_utilisation_rate = gpuinfo_refresh_utilisation_rate,
    .name = "panfrost",
};

static struct drmFuncTable drmFuncs;
static struct mali_gpu_state mali_state;

__attribute__((constructor)) static void init_extract_gpuinfo_panfrost(void) { register_gpu_vendor(&gpu_vendor_panfrost); }

bool gpuinfo_panfrost_init(void) {
  return mali_init_drm_funcs(&drmFuncs, &mali_state);
}

void gpuinfo_panfrost_shutdown(void) {
  for (unsigned i = 0; i < mali_state.mali_gpu_count; ++i) {
    struct panfrost_driver_data *prof_info = &mali_state.gpu_infos[i].model.panfrost;
    char debugfs_profile_file[256] = {0};
    FILE *debugfs_profiling;

    snprintf(debugfs_profile_file, sizeof(debugfs_profile_file),
             "/sys/kernel/debug/dri/%d/profile", prof_info->minor);
    debugfs_profiling = fopen(debugfs_profile_file, "w");
    if (debugfs_profiling == NULL) {
            fprintf(stderr, "Panfrost's profile parameter sysfs hook seems gone\n");
            continue;
    }

    char buf = prof_info->original_profiling_state ? '1' : '0';
    size_t size = fwrite(&buf, sizeof(char), 1, debugfs_profiling);
    if (!size)
            fprintf(stderr, "restoring debugfs state didn't work\n");
    fclose(debugfs_profiling);
  }

  mali_shutdown_common(&mali_state, &drmFuncs);
}

static const char *gpuinfo_panfrost_last_error_string(void) {
  static char driver_msg[MAX_ERR_STRING_LEN] = {0};

  return mali_common_last_error_string(&mali_state,
                                       gpu_vendor_panfrost.name,
                                       driver_msg);
}

static void panfrost_check_fdinfo_keys (bool *is_engine, bool *is_cycles,
                                        bool *is_maxfreq, bool *is_curfreq,
                                        bool *is_resident, char *key)
{
  static const char drm_panfrost_engine_vtx[] = "drm-engine-vertex-tiler";
  static const char drm_panfrost_engine_frg[] = "drm-engine-fragment";
  static const char drm_panfrost_cycles_vtx[] = "drm-cycles-vertex-tiler";
  static const char drm_panfrost_cycles_frg[] = "drm-cycles-fragment";
  static const char drm_panfrost_maxfreq_vtx[] = "drm-maxfreq-vertex-tiler";
  static const char drm_panfrost_maxfreq_frg[] = "drm-maxfreq-fragment";
  static const char drm_panfrost_curfreq_vtx[] = "drm-curfreq-vertex-tiler";
  static const char drm_panfrost_curfreq_frg[] = "drm-curfreq-fragment";
  static const char drm_panfrost_resident_mem[] = "drm-resident-memory";

  *is_engine = !strcmp(key, drm_panfrost_engine_vtx) || !strcmp(key, drm_panfrost_engine_frg);
  *is_cycles = !strcmp(key, drm_panfrost_cycles_vtx) || !strcmp(key, drm_panfrost_cycles_frg);
  *is_maxfreq = !strcmp(key, drm_panfrost_maxfreq_vtx) || !strcmp(key, drm_panfrost_maxfreq_frg);
  *is_curfreq = !strcmp(key, drm_panfrost_curfreq_vtx) || !strcmp(key, drm_panfrost_curfreq_frg);
  *is_resident = !strcmp(key, drm_panfrost_resident_mem);
}

static bool parse_drm_fdinfo_panfrost(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {

  struct gpu_info_mali *gpu_info = container_of(info, struct gpu_info_mali, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  struct fdinfo_data res = {0};

  nvtop_time current_time;
  nvtop_get_current_time(&current_time);

  if (!mali_common_parse_drm_fdinfo(info, fdinfo_file, process_info, dynamic_info,
                                    panfrost_check_fdinfo_keys, &res))
    return false;

  mali_common_parse_fdinfo_handle_cache(gpu_info, process_info, current_time, res.total_cycles,
					res.cid, res.engine_count >= 1 ? true : false);

  return true;
}

static bool panfrost_open_sysfs_profile(struct gpu_info_mali *gpu_info) {
  struct panfrost_driver_data *prof_info = &gpu_info->model.panfrost;
  char debugfs_profile_file[256] = {0};
  FILE *debugfs_profiling;
  struct stat filebuf;
  bool ret = true;

  if (!gpu_info->fd || gpu_info->version != MALI_PANFROST)
    return false;

  fstat(gpu_info->fd, &filebuf);
  prof_info->minor = minor(filebuf.st_rdev);
  snprintf(debugfs_profile_file, sizeof(debugfs_profile_file),
           "/sys/kernel/debug/dri/%d/profile", prof_info->minor);

  debugfs_profiling = fopen(debugfs_profile_file, "r");
  if (debugfs_profiling == NULL) {
    fprintf(stderr, "profile parameter not implemented in Panfrost\n");
    return false;
  }

  char buf = 0;
  size_t size = fread(&buf, sizeof(char), 1, debugfs_profiling);
  if (!size) {
    fprintf(stderr, "Error reading profiling state\n");
    ret = false;
    goto file_error;
  }

  prof_info->original_profiling_state = (buf == '1') ? true : false;

  fclose(debugfs_profiling);
  debugfs_profiling = fopen(debugfs_profile_file, "w");
  if (debugfs_profiling == NULL) {
    fprintf(stderr, "profile parameter not implemented in Panfrost\n");
    return false;
  }

  buf = '1';
  size = fwrite(&buf, sizeof(char), 1, debugfs_profiling);
  if (!size) {
    fprintf(stderr, "Error writing profiling state\n");
    ret = false;
  }

file_error:
  fclose(debugfs_profiling);
  return ret;
}

static bool gpuinfo_panfrost_get_device_handles(struct list_head *devices, unsigned *count) {
        return mali_common_get_device_handles(&mali_state, &drmFuncs, &gpu_vendor_panfrost,
                                              parse_drm_fdinfo_panfrost, devices, count,
                                              panfrost_open_sysfs_profile, MALI_PANFROST);
}

static int gpuinfo_panfrost_query_param(int gpu, uint32_t param, uint64_t *value) {

  struct drm_panfrost_get_param req = {
    .param = param,
  };

  int ret = drmFuncs.drmCommandWriteRead(gpu, DRM_PANFROST_GET_PARAM, &req, sizeof(req));

  if (ret)
    return ret;

  *value = req.value;

  return 0;
}

void gpuinfo_panfrost_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_mali *gpu_info = container_of(_gpu_info, struct gpu_info_mali, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;

  static_info->integrated_graphics = true;
  static_info->encode_decode_shared = true;
  RESET_ALL(static_info->valid);

  uint64_t gpuid;
  if (gpuinfo_panfrost_query_param(gpu_info->fd, DRM_PANFROST_PARAM_GPU_PROD_ID, &gpuid) == 0) {
    const char* name = panfrost_parse_marketing_name(gpuid);
    if (name) {
      strncpy(static_info->device_name, name, sizeof(static_info->device_name));
    }
    else {
      snprintf(static_info->device_name, sizeof(static_info->device_name), "Unknown Mali %lx", gpuid);
    }
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
  }

  uint64_t shader;
  if (gpuinfo_panfrost_query_param(gpu_info->fd, DRM_PANFROST_PARAM_SHADER_PRESENT, &shader) == 0)
    SET_GPUINFO_STATIC(static_info, n_shared_cores, util_last_bit(shader));

  uint64_t l2_cache_features;
  if (gpuinfo_panfrost_query_param(gpu_info->fd, DRM_PANFROST_PARAM_L2_FEATURES, &l2_cache_features) == 0)
    SET_GPUINFO_STATIC(static_info, l2cache_size, l2_cache_features & (0xFF << 16));

  if (GPUINFO_STATIC_FIELD_VALID(static_info, n_shared_cores)) {
    uint64_t core_features, thread_features;
    if (gpuinfo_panfrost_query_param(gpu_info->fd, DRM_PANFROST_PARAM_CORE_FEATURES, &core_features) != 0)
      return;
    if (gpuinfo_panfrost_query_param(gpu_info->fd, DRM_PANFROST_PARAM_THREAD_FEATURES, &thread_features) != 0)
      return;

    SET_GPUINFO_STATIC(static_info, n_exec_engines,
		       get_number_engines(gpuid, static_info->n_shared_cores,
					  core_features, thread_features));
  }
}

void gpuinfo_panfrost_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  static const char *meminfo_total = "MemTotal";
  static const char *meminfo_available = "MemAvailable";

  struct gpu_info_mali *gpu_info = container_of(_gpu_info, struct gpu_info_mali, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  if (gpu_info->version != MALI_PANFROST) {
    fprintf(stderr, "Wrong device version: %u\n", gpu_info->version);
    abort();
  }

  mali_common_refresh_dynamic_info(dynamic_info, &mali_state, meminfo_total, meminfo_available);
}

void gpuinfo_panfrost_get_running_processes(struct gpu_info *_gpu_info) {
  mali_common_get_running_processes(_gpu_info, MALI_PANFROST);
}
