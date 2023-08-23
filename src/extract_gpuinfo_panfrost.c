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

#include <assert.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/sysinfo.h>
#include <unistd.h>
#include <uthash.h>
#include <xf86drm.h>

#include "nvtop/device_discovery.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/time.h"

#include "panfrost_drm.h"
#include "panfrost_utils.h"

#define HASH_FIND_CLIENT(head, key_ptr, out_ptr) HASH_FIND(hh, head, key_ptr, sizeof(unsigned), out_ptr)
#define HASH_ADD_CLIENT(head, in_ptr) HASH_ADD(hh, head, client_id, sizeof(unsigned), in_ptr)

#define SET_PANFROST_CACHE(cachePtr, field, value) SET_VALUE(cachePtr, field, value, panfrost_cache_)
#define RESET_PANFROST_CACHE(cachePtr, field) INVALIDATE_VALUE(cachePtr, field, panfrost_cache_)
#define PANFROST_CACHE_FIELD_VALID(cachePtr, field) VALUE_IS_VALID(cachePtr, field, panfrost_cache_)

enum panfrost_process_info_cache_valid {
  panfrost_cache_engine_render_valid = 0,
  panfrost_cache_process_info_cache_valid_count
};

struct __attribute__((__packed__)) unique_cache_id {
  unsigned client_id;
  pid_t pid;
};

struct panfrost_process_info_cache {
  struct unique_cache_id client_id;
  uint64_t engine_render;
  uint64_t last_cycles;
  nvtop_time last_measurement_tstamp;
  unsigned char valid[(panfrost_cache_process_info_cache_valid_count + CHAR_BIT - 1) / CHAR_BIT];
  UT_hash_handle hh;
};

struct gpu_info_panfrost {
  drmVersionPtr drmVersion;
  struct gpu_info base;
  int fd;

  struct panfrost_process_info_cache *last_update_process_cache, *current_update_process_cache; // Cached processes info
};

static bool gpuinfo_panfrost_init(void);
static void gpuinfo_panfrost_shutdown(void);
static const char *gpuinfo_panfrost_last_error_string(void);
static bool gpuinfo_panfrost_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_panfrost_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_panfrost_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_panfrost_get_running_processes(struct gpu_info *_gpu_info);

struct gpu_vendor gpu_vendor_panfrost = {
    .init = gpuinfo_panfrost_init,
    .shutdown = gpuinfo_panfrost_shutdown,
    .last_error_string = gpuinfo_panfrost_last_error_string,
    .get_device_handles = gpuinfo_panfrost_get_device_handles,
    .populate_static_info = gpuinfo_panfrost_populate_static_info,
    .refresh_dynamic_info = gpuinfo_panfrost_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_panfrost_get_running_processes,
    .name = "panfrost",
};

unsigned panfrost_gpu_count;
static struct gpu_info_panfrost *gpu_infos;

static void *libdrm_handle;
static FILE* meminfo_file = NULL;

static FILE* debugfs_profiling = NULL;
bool original_profiling_state;
static const char *debugfs_profile_file = "/sys/kernel/debug/dri/128/profile";

static int last_libdrm_return_status = 0;
static char didnt_call_gpuinfo_init[] = "uninitialized";
static const char *local_error_string = didnt_call_gpuinfo_init;

// Local function pointers to DRM interface
static typeof(drmGetDevices) *_drmGetDevices;
static typeof(drmGetDevices2) *_drmGetDevices2;
static typeof(drmFreeDevices) *_drmFreeDevices;
static typeof(drmGetVersion) *_drmGetVersion;
static typeof(drmFreeVersion) *_drmFreeVersion;
static typeof(drmGetMagic) *_drmGetMagic;
static typeof(drmAuthMagic) *_drmAuthMagic;
static typeof(drmDropMaster) *_drmDropMaster;
static typeof(drmCommandWriteRead) *_drmCommandWriteRead;

static int wrap_drmGetDevices(drmDevicePtr devices[], int max_devices) {
  assert(_drmGetDevices2 || _drmGetDevices);

  if (_drmGetDevices2)
    return _drmGetDevices2(0, devices, max_devices);
  return _drmGetDevices(devices, max_devices);
}

static void authenticate_drm(int fd) {
  drm_magic_t magic;

  if (_drmGetMagic(fd, &magic) < 0) {
    return;
  }

  if (_drmAuthMagic(fd, magic) == 0) {
    if (_drmDropMaster(fd)) {
      perror("Failed to drop DRM master");
      fprintf(
          stderr,
          "\nWARNING: other DRM clients will crash on VT switch while nvtop is running!\npress ENTER to continue\n");
      fgetc(stdin);
    }
    return;
  }

  // XXX: Ideally I'd implement this too, but I'd need to pull in libxcb and yet
  // more functions and structs that may break ABI compatibility.
  // See radeontop auth_xcb.c for what is involved here
  fprintf(stderr, "Failed to authenticate to DRM; XCB authentication unimplemented\n");
}

#define STRINGIFY(x) STRINGIFY_HELPER_(x)
#define STRINGIFY_HELPER_(x) #x

__attribute__((constructor)) static void init_extract_gpuinfo_panfrost(void) { register_gpu_vendor(&gpu_vendor_panfrost); }

bool gpuinfo_panfrost_init(void) {
  libdrm_handle = dlopen("libdrm.so", RTLD_LAZY);
  if (!libdrm_handle)
    libdrm_handle = dlopen("libdrm.so.2", RTLD_LAZY);
  if (!libdrm_handle)
    libdrm_handle = dlopen("libdrm.so.1", RTLD_LAZY);
  if (!libdrm_handle) {
    local_error_string = dlerror();
    return false;
  }

  _drmGetDevices2 = dlsym(libdrm_handle, "drmGetDevices2");
  if (!_drmGetDevices2)
    _drmGetDevices = dlsym(libdrm_handle, "drmGetDevices");
  if (!_drmGetDevices2 && !_drmGetDevices)
    goto init_error_clean_exit;

  _drmFreeDevices = dlsym(libdrm_handle, "drmFreeDevices");
  if (!_drmFreeDevices)
    goto init_error_clean_exit;

  _drmGetVersion = dlsym(libdrm_handle, "drmGetVersion");
  if (!_drmGetVersion)
    goto init_error_clean_exit;

  _drmFreeVersion = dlsym(libdrm_handle, "drmFreeVersion");
  if (!_drmFreeVersion)
    goto init_error_clean_exit;

  _drmGetMagic = dlsym(libdrm_handle, "drmGetMagic");
  if (!_drmGetMagic)
    goto init_error_clean_exit;

  _drmAuthMagic = dlsym(libdrm_handle, "drmAuthMagic");
  if (!_drmAuthMagic)
    goto init_error_clean_exit;

  _drmDropMaster = dlsym(libdrm_handle, "drmDropMaster");
  if (!_drmDropMaster)
    goto init_error_clean_exit;

  _drmCommandWriteRead = dlsym(libdrm_handle, "drmCommandWriteRead");
  if (!_drmCommandWriteRead)
    goto init_error_clean_exit;

  local_error_string = NULL;

  meminfo_file = fopen("/proc/meminfo", "r");

  /* So far Panfrost is only present in SoCs where it's the only single GPU
    Otherwise, device file name would have to be changed */
  debugfs_profiling = fopen(debugfs_profile_file, "r");
  if (debugfs_profiling == NULL) {
      fprintf(stderr, "profile parameter not implemented in Panfrost\n");
      goto init_error_clean_exit;
  }

  char buf = 0;
  size_t size = fread(&buf, sizeof(char), 1, debugfs_profiling);
  if (!size) {
    fprintf(stderr, "Error reading profiling state\n");
    goto init_error_close_file;
  }

  original_profiling_state = (buf == '1') ? true : false;

  fclose(debugfs_profiling);
  debugfs_profiling = fopen(debugfs_profile_file, "w");
  if (debugfs_profiling == NULL) {
      fprintf(stderr, "profile parameter not implemented in Panfrost\n");
      goto init_error_clean_exit;
  }

  buf = '1';
  size = fwrite(&buf, sizeof(char), 1, debugfs_profiling);
  if (!size) {
    fprintf(stderr, "Error writing profiling state\n");
    goto init_error_close_file;
  }

  fclose(debugfs_profiling);

  return true;

init_error_close_file:
  fclose(debugfs_profiling);
init_error_clean_exit:
  dlclose(libdrm_handle);
  libdrm_handle = NULL;
  return false;
}

void gpuinfo_panfrost_shutdown(void) {
  for (unsigned i = 0; i < panfrost_gpu_count; ++i) {
    struct gpu_info_panfrost *current = &gpu_infos[i];
    _drmFreeVersion(current->drmVersion);
  }

  free(gpu_infos);
  gpu_infos = NULL;
  panfrost_gpu_count = 0;

  if (libdrm_handle) {
    dlclose(libdrm_handle);
    libdrm_handle = NULL;
    local_error_string = didnt_call_gpuinfo_init;
  }

  if (meminfo_file) {
    fclose(meminfo_file);
    meminfo_file = NULL;
  }

  debugfs_profiling = fopen(debugfs_profile_file, "w");
  if (debugfs_profiling == NULL) {
      fprintf(stderr, "profile parameter not implemented in Panfrost\n");
      return;
  }

  char buf = original_profiling_state ? '1' : '0';
  size_t size = fwrite(&buf, sizeof(char), 1, debugfs_profiling);
  if (!size)
    fprintf(stderr, "restoring debugfs state didn't work\n");
  fclose(debugfs_profiling);
}

static const char *gpuinfo_panfrost_last_error_string(void) {
  if (local_error_string) {
    return local_error_string;
  } else if (last_libdrm_return_status < 0) {
    switch (last_libdrm_return_status) {
    case DRM_ERR_NO_DEVICE:
      return "no device\n";
    case DRM_ERR_NO_ACCESS:
      return "no access\n";
    case DRM_ERR_NOT_ROOT:
      return "not root\n";
    case DRM_ERR_INVALID:
      return "invalid args\n";
    case DRM_ERR_NO_FD:
      return "no fd\n";
    default:
      return "unknown error\n";
    }
  } else {
    return "An unanticipated error occurred while accessing Panfrost "
           "information\n";
  }
}

static uint64_t parse_memory_multiplier(const char *str) {
  if (strcmp(str, " B") == 0) {
    return 1;
  }
  else if (strcmp(str, " KiB") == 0 || strcmp(str, " kB") == 0) {
    return 1024;
  }
  else if (strcmp(str, " MiB") == 0) {
    return 1024 * 1024;
  }
  else if (strcmp(str, " GiB") == 0) {
    return 1024 * 1024 * 1024;
  }

  return 1;
}

static const char drm_panfrost_driver[] = "drm-driver";
static const char drm_panfrost_engine_vtx[] = "drm-engine-vertex-tiler";
static const char drm_panfrost_engine_frg[] = "drm-engine-fragment";
static const char drm_panfrost_cycles_vtx[] = "drm-cycles-vertex-tiler";
static const char drm_panfrost_cycles_frg[] = "drm-cycles-fragment";
static const char drm_panfrost_maxfreq_vtx[] = "drm-maxfreq-vertex-tiler";
static const char drm_panfrost_maxfreq_frg[] = "drm-maxfreq-fragment";
static const char drm_panfrost_curfreq_vtx[] = "drm-curfreq-vertex-tiler";
static const char drm_panfrost_curfreq_frg[] = "drm-curfreq-fragment";
static const char drm_panfrost_resident_mem[] = "drm-resident-memory";

static bool parse_drm_fdinfo_panfrost(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {
  struct gpu_info_panfrost *gpu_info = container_of(info, struct gpu_info_panfrost, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  static char *line = NULL;
  static size_t line_buf_size = 0;
  unsigned int engine_count = 0;
  uint64_t total_time = 0;
  uint64_t total_cycles = 0;
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

    if (!strcmp(key, drm_panfrost_driver)) {
      if (strcmp(val, "panfrost")) {
        /* This fdinfo file doesn't come from a Panfrost device */
        return false;
      }
    } else if(!strcmp(key, drm_client_id)) {
      char *endptr;
      cid = strtoul(val, &endptr, 10);
      if (*endptr)
        continue;
      client_id_set = true;
    } else {
      bool is_engine = !strcmp(key, drm_panfrost_engine_vtx) || !strcmp(key, drm_panfrost_engine_frg);
      bool is_cycles = !strcmp(key, drm_panfrost_cycles_vtx) || !strcmp(key, drm_panfrost_cycles_frg);
      bool is_maxfreq = !strcmp(key, drm_panfrost_maxfreq_vtx) || !strcmp(key, drm_panfrost_maxfreq_frg);
      bool is_curfreq = !strcmp(key, drm_panfrost_curfreq_vtx) || !strcmp(key, drm_panfrost_curfreq_frg);
      bool is_resident = !strcmp(key, drm_panfrost_resident_mem);

      if (is_engine) {
        char *endptr;
        uint64_t time_spent = strtoull(val, &endptr, 10);
        if (endptr == val || strcmp(endptr, " ns"))
          continue;

        total_time += time_spent;
        engine_count++;
      } else if (is_maxfreq || is_curfreq) {
        char *endptr;
        uint64_t freq = strtoull(val, &endptr, 10);
        if (endptr == val || strcmp(endptr, " Hz"))
          continue;

        if (is_maxfreq)
          SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, freq / 1000000);
        else
          SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, freq / 1000000);
      } else if (is_cycles) {
        char *endptr;
        uint64_t cycles = strtoull(val, &endptr, 10);

        if (endptr == val)
          continue;

        total_cycles += cycles;
      } else if (is_resident) {
        uint64_t mem_int;
        char *endptr;

        mem_int = strtoull(val, &endptr, 10);
        if (endptr == val)
          continue;

        uint64_t multiplier = parse_memory_multiplier(endptr);
        SET_GPUINFO_PROCESS(process_info, gpu_memory_usage, mem_int * multiplier);
      }
    }
  }

  if (engine_count)
    SET_GPUINFO_PROCESS(process_info, gfx_engine_used, total_time);

  if (!client_id_set)
    return false;

  //  driver does not expose compute engine metrics as of yet
  process_info->type |= gpu_process_graphical;

  struct panfrost_process_info_cache *cache_entry;
  struct unique_cache_id ucid = {.client_id = cid, .pid = process_info->pid};
  HASH_FIND_CLIENT(gpu_info->last_update_process_cache, &ucid, cache_entry);
  if (cache_entry) {
    uint64_t time_elapsed = nvtop_difftime_u64(cache_entry->last_measurement_tstamp, current_time);
    SET_GPUINFO_PROCESS(process_info, sample_delta, time_elapsed);
    if (engine_count)
      SET_GPUINFO_PROCESS(process_info, gpu_cycles, total_cycles - cache_entry->last_cycles);
    cache_entry->last_cycles = total_cycles;
    HASH_DEL(gpu_info->last_update_process_cache, cache_entry);
    if (GPUINFO_PROCESS_FIELD_VALID(process_info, gfx_engine_used) &&
        PANFROST_CACHE_FIELD_VALID(cache_entry, engine_render) &&
        // In some rare occasions, the gfx engine usage reported by the driver is lowering (might be a driver bug)
        process_info->gfx_engine_used >= cache_entry->engine_render &&
        process_info->gfx_engine_used - cache_entry->engine_render <= time_elapsed) {
      SET_GPUINFO_PROCESS(
          process_info, gpu_usage,
          busy_usage_from_time_usage_round(process_info->gfx_engine_used, cache_entry->engine_render, time_elapsed));
    }
  } else {
    cache_entry = calloc(1, sizeof(*cache_entry));
    if (!cache_entry)
      goto parse_fdinfo_exit;
    cache_entry->client_id.client_id = cid;
    cache_entry->client_id.pid = process_info->pid;
    cache_entry->last_cycles = total_cycles;
  }

#ifndef NDEBUG
  // We should only process one fdinfo entry per client id per update
  struct panfrost_process_info_cache *cache_entry_check;
  HASH_FIND_CLIENT(gpu_info->current_update_process_cache, &cid, cache_entry_check);
  assert(!cache_entry_check && "We should not be processing a client id twice per update");
#endif

  RESET_ALL(cache_entry->valid);
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, gfx_engine_used))
    SET_PANFROST_CACHE(cache_entry, engine_render, process_info->gfx_engine_used);

  cache_entry->last_measurement_tstamp = current_time;
  HASH_ADD_CLIENT(gpu_info->current_update_process_cache, cache_entry);

parse_fdinfo_exit:
  return true;
}

static bool gpuinfo_panfrost_get_device_handles(struct list_head *devices, unsigned *count) {
  if (!libdrm_handle)
    return false;

  last_libdrm_return_status = wrap_drmGetDevices(NULL, 0);
  if (last_libdrm_return_status <= 0)
    return false;

  drmDevicePtr devs[last_libdrm_return_status];
  last_libdrm_return_status = wrap_drmGetDevices(devs, last_libdrm_return_status);
  if (last_libdrm_return_status <= 0)
    return false;

  unsigned int libdrm_count = last_libdrm_return_status;
  gpu_infos = calloc(libdrm_count, sizeof(*gpu_infos));
  if (!gpu_infos) {
    local_error_string = strerror(errno);
    return false;
  }

  for (unsigned int i = 0; i < libdrm_count; i++) {
    int fd = -1;

    // Try render node first
    if (1 << DRM_NODE_RENDER & devs[i]->available_nodes) {
      fd = open(devs[i]->nodes[DRM_NODE_RENDER], O_RDWR);
    }
    if (fd < 0) {
      // Fallback to primary node (control nodes are unused according to the DRM documentation)
      if (1 << DRM_NODE_PRIMARY & devs[i]->available_nodes) {
        fd = open(devs[i]->nodes[DRM_NODE_PRIMARY], O_RDWR);
      }
    }

    if (fd < 0)
      continue;

    drmVersionPtr ver = _drmGetVersion(fd);

    if (!ver) {
      close(fd);
      continue;
    }

    bool is_panfrost = !strcmp(ver->name, "panfrost");
    if (!is_panfrost) {
      _drmFreeVersion(ver);
      close(fd);
      continue;
    }

    authenticate_drm(fd);

    gpu_infos[panfrost_gpu_count].drmVersion = ver;
    gpu_infos[panfrost_gpu_count].fd = fd;
    gpu_infos[panfrost_gpu_count].base.vendor = &gpu_vendor_panfrost;

    list_add_tail(&gpu_infos[panfrost_gpu_count].base.list, devices);
    // Register a fdinfo callback for this GPU
    processinfo_register_fdinfo_callback(parse_drm_fdinfo_panfrost, &gpu_infos[panfrost_gpu_count].base);
    panfrost_gpu_count++;
  }

  _drmFreeDevices(devs, libdrm_count);
  *count = panfrost_gpu_count;

  return true;
}

static int gpuinfo_panfrost_query_param(int gpu, uint32_t param, uint64_t *value) {

  struct drm_panfrost_get_param req = {
    .param = param,
  };

  int ret = _drmCommandWriteRead(gpu, DRM_PANFROST_GET_PARAM, &req, sizeof(req));

  if (ret)
    return ret;

  *value = req.value;

  return 0;
}

void gpuinfo_panfrost_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_panfrost *gpu_info = container_of(_gpu_info, struct gpu_info_panfrost, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;

  static_info->integrated_graphics = true;
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

static const char meminfo_total[] = "MemTotal";
static const char meminfo_available[] = "MemAvailable";

void gpuinfo_panfrost_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_panfrost *gpu_info = container_of(_gpu_info, struct gpu_info_panfrost, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  RESET_ALL(dynamic_info->valid);
  dynamic_info->encode_decode_shared = true;

  rewind(meminfo_file);
  fflush(meminfo_file);

  static char *line = NULL;
  static size_t line_buf_size = 0;
  ssize_t count = 0;
  uint64_t mem_total = 0;
  uint64_t mem_available = 0;
  size_t keys_acquired = 0;

  while (keys_acquired != 2 && (count = getline(&line, &line_buf_size, meminfo_file)) != -1) {
    char *key, *val;

    // Get rid of the newline if present
    if (line[count - 1] == '\n') {
      line[--count] = '\0';
    }

    if (!extract_drm_fdinfo_key_value(line, &key, &val))
      continue;

    bool is_total = !strcmp(key, meminfo_total);
    bool is_available = !strcmp(key, meminfo_available);

    if (is_total || is_available) {
      uint64_t mem_int;
      char *endptr;

      mem_int = strtoull(val, &endptr, 10);
      if (endptr == val)
        continue;

      mem_int *= parse_memory_multiplier(endptr);
      if (is_total) {
        mem_total = mem_int;
      }
      else if (is_available) {
        mem_available = mem_int;
      }
      ++keys_acquired;
    }
  }

  SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mem_total);
  SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, mem_total - mem_available);
  SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, mem_available);
  SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate,
                      (dynamic_info->total_memory - dynamic_info->free_memory) * 100 / dynamic_info->total_memory);
}

static void swap_process_cache_for_next_update(struct gpu_info_panfrost *gpu_info) {
  // Free old cache data and set the cache for the next update
  if (gpu_info->last_update_process_cache) {
    struct panfrost_process_info_cache *cache_entry, *tmp;
    HASH_ITER(hh, gpu_info->last_update_process_cache, cache_entry, tmp) {
      HASH_DEL(gpu_info->last_update_process_cache, cache_entry);
      free(cache_entry);
    }
  }
  gpu_info->last_update_process_cache = gpu_info->current_update_process_cache;
  gpu_info->current_update_process_cache = NULL;
}

void gpuinfo_panfrost_get_running_processes(struct gpu_info *_gpu_info) {
  // For Panfrost, we register a fdinfo callback that will fill the gpu_process datastructure of the gpu_info structure
  // for us. This avoids going through /proc multiple times per update for multiple GPUs.
  struct gpu_info_panfrost *gpu_info = container_of(_gpu_info, struct gpu_info_panfrost, base);
  swap_process_cache_for_next_update(gpu_info);
}
