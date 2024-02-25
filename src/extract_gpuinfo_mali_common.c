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
#include <errno.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <unistd.h>
#include <xf86drm.h>

#include "mali_common.h"

enum mali_process_info_cache_valid {
  mali_cache_engine_render_valid = 0,
  mali_cache_process_info_cache_valid_count
};

struct __attribute__((__packed__)) unique_cache_id {
  unsigned client_id;
  pid_t pid;
};

struct mali_process_info_cache {
  struct unique_cache_id client_id;
  uint64_t engine_render;
  uint64_t last_cycles;
  nvtop_time last_measurement_tstamp;
  unsigned char valid[(mali_cache_process_info_cache_valid_count + CHAR_BIT - 1) / CHAR_BIT];
  UT_hash_handle hh;
};

bool mali_init_drm_funcs(struct drmFuncTable *drmFuncs,
			 struct mali_gpu_state *state)
{
    state->libdrm_handle = dlopen("libdrm.so", RTLD_LAZY);
  if (!state->libdrm_handle)
    state->libdrm_handle = dlopen("libdrm.so.2", RTLD_LAZY);
  if (!state->libdrm_handle)
    state->libdrm_handle = dlopen("libdrm.so.1", RTLD_LAZY);
  if (!state->libdrm_handle) {
    state->local_error_string = dlerror();
    return false;
  }

  drmFuncs->drmGetDevices2 = dlsym(state->libdrm_handle, "drmGetDevices2");
  if (!drmFuncs->drmGetDevices2)
    drmFuncs->drmGetDevices = dlsym(state->libdrm_handle, "drmGetDevices");
  if (!drmFuncs->drmGetDevices2 && !drmFuncs->drmGetDevices)
    goto init_error_clean_exit;

  drmFuncs->drmFreeDevices = dlsym(state->libdrm_handle, "drmFreeDevices");
  if (!drmFuncs->drmFreeDevices)
    goto init_error_clean_exit;

  drmFuncs->drmGetVersion = dlsym(state->libdrm_handle, "drmGetVersion");
  if (!drmFuncs->drmGetVersion)
    goto init_error_clean_exit;

  drmFuncs->drmFreeVersion = dlsym(state->libdrm_handle, "drmFreeVersion");
  if (!drmFuncs->drmFreeVersion)
    goto init_error_clean_exit;

  drmFuncs->drmGetMagic = dlsym(state->libdrm_handle, "drmGetMagic");
  if (!drmFuncs->drmGetMagic)
    goto init_error_clean_exit;

  drmFuncs->drmAuthMagic = dlsym(state->libdrm_handle, "drmAuthMagic");
  if (!drmFuncs->drmAuthMagic)
    goto init_error_clean_exit;

  drmFuncs->drmDropMaster = dlsym(state->libdrm_handle, "drmDropMaster");
  if (!drmFuncs->drmDropMaster)
    goto init_error_clean_exit;

  drmFuncs->drmCommandWriteRead = dlsym(state->libdrm_handle, "drmCommandWriteRead");
  if (!drmFuncs->drmCommandWriteRead)
    goto init_error_clean_exit;

  drmFuncs->drmIoctl = dlsym(state->libdrm_handle, "drmIoctl");
  if (!drmFuncs->drmCommandWriteRead)
    goto init_error_clean_exit;

  state->local_error_string = NULL;

  state->meminfo_file = fopen("/proc/meminfo", "r");
  if (!state->meminfo_file)
    goto init_error_clean_exit;

  state->didnt_call_gpuinfo_init = "uninitialized";

  return true;

init_error_clean_exit:
  dlclose(state->libdrm_handle);
  state->libdrm_handle = NULL;
  return false;
}

void mali_deinit_drm(struct mali_gpu_state *state)
{
  dlclose(state->libdrm_handle);
  state->libdrm_handle = NULL;
}

void mali_shutdown_common(struct mali_gpu_state *state,
			  struct drmFuncTable *funcs)
{
  for (unsigned i = 0; i < state->mali_gpu_count; ++i) {
    struct gpu_info_mali *current = &state->gpu_infos[i];
    funcs->drmFreeVersion(current->drmVersion);
  }

  free(state->gpu_infos);
  state->gpu_infos = NULL;
  state->mali_gpu_count = 0;

  if (state->libdrm_handle) {
    dlclose(state->libdrm_handle);
    state->libdrm_handle = NULL;
    state->local_error_string = state->didnt_call_gpuinfo_init;
  }

  if (state->meminfo_file) {
    fclose(state->meminfo_file);
    state->meminfo_file = NULL;
  }
}

const char *mali_common_last_error_string(struct mali_gpu_state *state,
					  const char *drivername,
					  char error_str[])
{
  if (state->local_error_string) {
    return state->local_error_string;
  } else if (state->last_libdrm_return_status < 0) {
    switch (state->last_libdrm_return_status) {
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

    int ret = snprintf(error_str, MAX_ERR_STRING_LEN,
		       "An unanticipated error occurred while accessing %s information\n",
		       drivername);
    if (ret >= MAX_ERR_STRING_LEN)
      error_str[MAX_ERR_STRING_LEN - 1]  = '\0';
    return error_str;
  }
}

static int wrap_drmGetDevices(drmDevicePtr devices[], int max_devices, struct drmFuncTable *funcs) {
  assert(funcs->drmGetDevices2 || funcs->drmGetDevices);

  if (funcs->drmGetDevices2)
    return funcs->drmGetDevices2(0, devices, max_devices);
  return funcs->drmGetDevices(devices, max_devices);
}

static void authenticate_drm(int fd, struct drmFuncTable *funcs) {
  drm_magic_t magic;

  if (funcs->drmGetMagic(fd, &magic) < 0) {
    return;
  }

  if (funcs->drmAuthMagic(fd, magic) == 0) {
    if (funcs->drmDropMaster(fd)) {
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

bool mali_common_get_device_handles(struct mali_gpu_state *state,
				    struct drmFuncTable *funcs,
				    struct gpu_vendor *vendor,
				    processinfo_fdinfo_callback callback,
				    struct list_head *devices, unsigned *count,
				    bool (*handle_model) (struct gpu_info_mali *),
				    enum mali_version version)
{
  if (!state->libdrm_handle || version >= MALI_VERSIONS)
    return false;

  state->last_libdrm_return_status = wrap_drmGetDevices(NULL, 0, funcs);
  if (state->last_libdrm_return_status <= 0)
    return false;

  drmDevicePtr devs[state->last_libdrm_return_status];
  state->last_libdrm_return_status = wrap_drmGetDevices(devs, state->last_libdrm_return_status, funcs);
  if (state->last_libdrm_return_status <= 0)
    return false;

  unsigned int libdrm_count = state->last_libdrm_return_status;
  state->gpu_infos = calloc(libdrm_count, sizeof(*state->gpu_infos));
  if (!state->gpu_infos) {
    state->local_error_string = strerror(errno);
    return false;
  }

  state->gpu_infos->version = version;
  state->mali_gpu_count = 0;

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

    drmVersionPtr ver = funcs->drmGetVersion(fd);

    if (!ver) {
      close(fd);
      continue;
    }

    if (strcmp(ver->name, vendor->name)) {
      funcs->drmFreeVersion(ver);
      close(fd);
      continue;
    }

    authenticate_drm(fd, funcs);

    state->gpu_infos[state->mali_gpu_count].drmVersion = ver;
    state->gpu_infos[state->mali_gpu_count].fd = fd;
    state->gpu_infos[state->mali_gpu_count].base.vendor = vendor;

    list_add_tail(&state->gpu_infos[state->mali_gpu_count].base.list, devices);
    // Register a fdinfo callback for this GPU
    processinfo_register_fdinfo_callback(callback, &state->gpu_infos[state->mali_gpu_count].base);

    if (handle_model) {
      if (!handle_model(&state->gpu_infos[state->mali_gpu_count])) {
              funcs->drmFreeVersion(ver);
              close(fd);
              continue;
      }
    }

    state->mali_gpu_count++;
  }

  funcs->drmFreeDevices(devs, libdrm_count);
  *count = state->mali_gpu_count;

  return true;
}

uint64_t parse_memory_multiplier(const char *str) {
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

void mali_common_refresh_dynamic_info(struct gpuinfo_dynamic_info *dynamic_info,
				      struct mali_gpu_state *state,
				      const char *meminfo_total,
				      const char *meminfo_available)
{
  RESET_ALL(dynamic_info->valid);

  rewind(state->meminfo_file);
  fflush(state->meminfo_file);

  static char *line = NULL;
  static size_t line_buf_size = 0;
  ssize_t count = 0;
  uint64_t mem_total = 0;
  uint64_t mem_available = 0;
  size_t keys_acquired = 0;

  while (keys_acquired != 2 && (count = getline(&line, &line_buf_size, state->meminfo_file)) != -1) {
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

static void swap_process_cache_for_next_update(struct gpu_info_mali *gpu_info) {
  // Free old cache data and set the cache for the next update
  if (gpu_info->last_update_process_cache) {
    struct mali_process_info_cache *cache_entry, *tmp;
    HASH_ITER(hh, gpu_info->last_update_process_cache, cache_entry, tmp) {
      HASH_DEL(gpu_info->last_update_process_cache, cache_entry);
      free(cache_entry);
    }
  }
  gpu_info->last_update_process_cache = gpu_info->current_update_process_cache;
  gpu_info->current_update_process_cache = NULL;
}

void mali_common_get_running_processes(struct gpu_info *_gpu_info, enum mali_version version) {
  // For Mali, we register a fdinfo callback that will fill the gpu_process datastructure of the gpu_info structure
  // for us. This avoids going through /proc multiple times per update for multiple GPUs.
  struct gpu_info_mali *gpu_info = container_of(_gpu_info, struct gpu_info_mali, base);
  if (gpu_info->version != version) {
    fprintf(stderr, "Wrong device version: %u\n", gpu_info->version);
    abort();
  }

  swap_process_cache_for_next_update(gpu_info);
}

void mali_common_parse_fdinfo_handle_cache(struct gpu_info_mali *gpu_info,
					   struct gpu_process *process_info,
					   nvtop_time current_time,
					   uint64_t total_cycles,
					   unsigned cid,
					   bool engine_count)
{
  struct mali_process_info_cache *cache_entry;
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
        MALI_CACHE_FIELD_VALID(cache_entry, engine_render) &&
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
      return;
    cache_entry->client_id.client_id = cid;
    cache_entry->client_id.pid = process_info->pid;
    cache_entry->last_cycles = total_cycles;
  }

#ifndef NDEBUG
  // We should only process one fdinfo entry per client id per update
  struct mali_process_info_cache *cache_entry_check;
  HASH_FIND_CLIENT(gpu_info->current_update_process_cache, &cache_entry->client_id, cache_entry_check);
  assert(!cache_entry_check && "We should not be processing a client id twice per update");
#endif

  RESET_ALL(cache_entry->valid);
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, gfx_engine_used))
    SET_MALI_CACHE(cache_entry, engine_render, process_info->gfx_engine_used);

  cache_entry->last_measurement_tstamp = current_time;
  HASH_ADD_CLIENT(gpu_info->current_update_process_cache, cache_entry);
}

bool mali_common_parse_drm_fdinfo(struct gpu_info *info, FILE *fdinfo_file,
				  struct gpu_process *process_info,
				  struct gpuinfo_dynamic_info *dynamic_info,
				  check_fdinfo_keys match_keys,
				  struct fdinfo_data *fid)
{
  static char *line = NULL;
  static size_t line_buf_size = 0;
  uint64_t total_time = 0;
  bool client_id_set = false;
  ssize_t count = 0;

  fid->engine_count = 0;
  fid->total_cycles = 0;

  while ((count = getline(&line, &line_buf_size, fdinfo_file)) != -1) {
    char *key, *val;
    // Get rid of the newline if present
    if (line[count - 1] == '\n') {
      line[--count] = '\0';
    }

    if (!extract_drm_fdinfo_key_value(line, &key, &val))
      continue;

    if (!strcmp(key, "drm-driver")) {
      if (strcmp(val, info->vendor->name)) {
        return false;
      }
    } else if(!strcmp(key, drm_client_id)) {
      char *endptr;
      fid->cid = strtoul(val, &endptr, 10);
      if (*endptr)
        continue;
      client_id_set = true;
    } else {
      bool is_engine, is_cycles, is_maxfreq, is_curfreq, is_resident;
      match_keys(&is_engine, &is_cycles, &is_maxfreq, &is_curfreq, &is_resident, key);

      if (is_engine) {
        char *endptr;
        uint64_t time_spent = strtoull(val, &endptr, 10);
        if (endptr == val || strcmp(endptr, " ns"))
          continue;

        total_time += time_spent;
        fid->engine_count++;
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

        fid->total_cycles += cycles;
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

  if (fid->engine_count)
    SET_GPUINFO_PROCESS(process_info, gfx_engine_used, total_time);

  if (!client_id_set)
    return false;

  //  driver does not expose compute engine metrics as of yet
  process_info->type |= gpu_process_graphical;

  return true;
}
