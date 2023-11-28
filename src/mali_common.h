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

#include <uthash.h>

#include "nvtop/device_discovery.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/time.h"

#define MAX_ERR_STRING_LEN 256

struct drmFuncTable {
  typeof(drmGetDevices) *drmGetDevices;
  typeof(drmGetDevices2) *drmGetDevices2;
  typeof(drmFreeDevices) *drmFreeDevices;
  typeof(drmGetVersion) *drmGetVersion;
  typeof(drmFreeVersion) *drmFreeVersion;
  typeof(drmGetMagic) *drmGetMagic;
  typeof(drmAuthMagic) *drmAuthMagic;
  typeof(drmDropMaster) *drmDropMaster;
  typeof(drmCommandWriteRead) *drmCommandWriteRead;
  typeof(drmIoctl) *drmIoctl;
};

enum mali_version {
	MALI_PANFROST,
	MALI_PANTHOR,
	MALI_VERSIONS,
};

struct mali_process_info_cache;

struct panfrost_driver_data {
  bool original_profiling_state;
  bool profiler_enabled;
  unsigned int minor;
};
struct panthor_driver_data {
  uint32_t unused;
};

struct gpu_info_mali {
  drmVersionPtr drmVersion;
  enum mali_version version;
  struct gpu_info base;
  int fd;

   // Cached processes info
  struct mali_process_info_cache *last_update_process_cache;
  struct mali_process_info_cache *current_update_process_cache;

  union {
    struct panfrost_driver_data panfrost;
    struct panthor_driver_data panthor;
  } model;
};

struct mali_gpu_state {
  unsigned mali_gpu_count;
  struct gpu_info_mali *gpu_infos;

  void *libdrm_handle;
  FILE *meminfo_file;

  int last_libdrm_return_status;
  char *didnt_call_gpuinfo_init;
  char *local_error_string;
};

typedef void (*check_fdinfo_keys)(bool *is_engine, bool *is_cycles,
				  bool *is_maxfreq, bool *is_curfreq,
				  bool *is_resident, char *key);

struct fdinfo_data {
  uint64_t total_cycles;
  bool client_id_set;
  unsigned int engine_count;
  unsigned cid;
};

#define HASH_FIND_CLIENT(head, key_ptr, out_ptr) HASH_FIND(hh, head, key_ptr, sizeof(struct unique_cache_id), out_ptr)
#define HASH_ADD_CLIENT(head, in_ptr) HASH_ADD(hh, head, client_id, sizeof(struct unique_cache_id), in_ptr)

#define SET_MALI_CACHE(cachePtr, field, value) SET_VALUE(cachePtr, field, value, mali_cache_)
#define RESET_PANFROST_CACHE(cachePtr, field) INVALIDATE_VALUE(cachePtr, field, mali_cache_)
#define MALI_CACHE_FIELD_VALID(cachePtr, field) VALUE_IS_VALID(cachePtr, field, mali_cache_)

uint64_t parse_memory_multiplier(const char *str);

bool mali_init_drm_funcs(struct drmFuncTable *drmFuncs, struct mali_gpu_state *state);
void mali_deinit_drm(struct mali_gpu_state *state);
void mali_shutdown_common(struct mali_gpu_state *state, struct drmFuncTable *funcs);
const char *mali_common_last_error_string(struct mali_gpu_state *state,
					  const char *drivername,
					  char error_str[]);
bool mali_common_get_device_handles(struct mali_gpu_state *state,
				    struct drmFuncTable *funcs,
				    struct gpu_vendor *vendor,
				    processinfo_fdinfo_callback callback,
				    struct list_head *devices, unsigned *count,
				    bool (*handle_model) (struct gpu_info_mali *),
				    enum mali_version version);
void mali_common_refresh_dynamic_info(struct gpuinfo_dynamic_info *dynamic_info,
				      struct mali_gpu_state *state,
				      const char *meminfo_total,
				      const char *meminfo_available);
void mali_common_get_running_processes(struct gpu_info *_gpu_info, enum mali_version version);

void mali_common_parse_fdinfo_handle_cache(struct gpu_info_mali *gpu_info,
					   struct gpu_process *process_info,
					   nvtop_time current_time,
					   uint64_t total_cycles,
					   unsigned cid,
					   bool engine_count);

bool mali_common_parse_drm_fdinfo(struct gpu_info *info, FILE *fdinfo_file,
				  struct gpu_process *process_info,
				  struct gpuinfo_dynamic_info *dynamic_info,
				  check_fdinfo_keys match_keys,
				  struct fdinfo_data *fid);
