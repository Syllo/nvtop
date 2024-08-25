/*
 *
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
 *
 * This file is part of Nvtop and adapted from igt-gpu-tools from v3d Corporation.
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

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <uthash.h>

#define HASH_FIND_CLIENT(head, key_ptr, out_ptr) HASH_FIND(hh, head, key_ptr, sizeof(struct unique_cache_id), out_ptr)
#define HASH_ADD_CLIENT(head, in_ptr) HASH_ADD(hh, head, client_id, sizeof(struct unique_cache_id), in_ptr)

#define SET_v3d_CACHE(cachePtr, field, value) SET_VALUE(cachePtr, field, value, v3d_cache_)
#define RESET_v3d_CACHE(cachePtr, field) INVALIDATE_VALUE(cachePtr, field, v3d_cache_)
#define v3d_CACHE_FIELD_VALID(cachePtr, field) VALUE_IS_VALID(cachePtr, field, v3d_cache_)

enum v3d_process_info_cache_valid {
  v3d_cache_engine_render_valid = 0,
  v3d_cache_engine_copy_valid,
  v3d_cache_engine_video_valid,
  v3d_cache_engine_video_enhance_valid,
  v3d_cache_process_info_cache_valid_count
};

struct __attribute__((__packed__)) unique_cache_id {
  unsigned client_id;
  pid_t pid;
  char *pdev;
};

struct v3d_process_info_cache {
  struct unique_cache_id client_id;
  uint64_t engine_render;
  uint64_t engine_copy;
  uint64_t engine_video;
  uint64_t engine_video_enhance;
  nvtop_time last_measurement_tstamp;
  unsigned char valid[(v3d_cache_process_info_cache_valid_count + CHAR_BIT - 1) / CHAR_BIT];
  UT_hash_handle hh;
};

struct gpu_info_v3d {
  struct gpu_info base;

  unsigned long long last_timestamp, last_val[5];
  struct nvtop_device *card_device;
  struct nvtop_device *driver_device;
  struct v3d_process_info_cache *last_update_process_cache, *current_update_process_cache; // Cached processes info
};

static bool gpuinfo_v3d_init(void);
static void gpuinfo_v3d_shutdown(void);
static const char *gpuinfo_v3d_last_error_string(void);
static bool gpuinfo_v3d_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_v3d_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_v3d_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_v3d_get_running_processes(struct gpu_info *_gpu_info);

struct gpu_vendor gpu_vendor_v3d = {
    .init = gpuinfo_v3d_init,
    .shutdown = gpuinfo_v3d_shutdown,
    .last_error_string = gpuinfo_v3d_last_error_string,
    .get_device_handles = gpuinfo_v3d_get_device_handles,
    .populate_static_info = gpuinfo_v3d_populate_static_info,
    .refresh_dynamic_info = gpuinfo_v3d_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_v3d_get_running_processes,
    .name = "v3d",
};

unsigned v3d_gpu_count;
static struct gpu_info_v3d *gpu_infos;

__attribute__((constructor)) static void init_extract_gpuinfo_v3d(void) { register_gpu_vendor(&gpu_vendor_v3d); }

bool gpuinfo_v3d_init(void) { return true; }
void gpuinfo_v3d_shutdown(void) {
  for (unsigned i = 0; i < v3d_gpu_count; ++i) {
    struct gpu_info_v3d *current = &gpu_infos[i];
    nvtop_device_unref(current->card_device);
    nvtop_device_unref(current->driver_device);
  }
}

const char *gpuinfo_v3d_last_error_string(void) { return "Err"; }

static bool parse_drm_fdinfo_v3d(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {
  struct gpu_info_v3d *gpu_info = container_of(info, struct gpu_info_v3d, base);

  unsigned cid;
  nvtop_time current_time;
  nvtop_get_current_time(&current_time);

  //  The v3d driver does not expose compute engine metrics as of yet
  process_info->type |= gpu_process_graphical;

  struct v3d_process_info_cache *cache_entry;
  struct unique_cache_id ucid = {.client_id = cid, .pid = process_info->pid, .pdev = gpu_info->base.pdev};
  HASH_FIND_CLIENT(gpu_info->last_update_process_cache, &ucid, cache_entry);
  if (cache_entry) {
    uint64_t time_elapsed = nvtop_difftime_u64(cache_entry->last_measurement_tstamp, current_time);
    HASH_DEL(gpu_info->last_update_process_cache, cache_entry);
    if (GPUINFO_PROCESS_FIELD_VALID(process_info, gfx_engine_used) &&
        v3d_CACHE_FIELD_VALID(cache_entry, engine_render) &&
        // In some rare occasions, the gfx engine usage reported by the driver is lowering (might be a driver bug)
        process_info->gfx_engine_used >= cache_entry->engine_render &&
        process_info->gfx_engine_used - cache_entry->engine_render <= time_elapsed) {
      SET_GPUINFO_PROCESS(
          process_info, gpu_usage,
          busy_usage_from_time_usage_round(process_info->gfx_engine_used, cache_entry->engine_render, time_elapsed));
    }
    if (GPUINFO_PROCESS_FIELD_VALID(process_info, dec_engine_used) &&
        v3d_CACHE_FIELD_VALID(cache_entry, engine_video) &&
        process_info->dec_engine_used >= cache_entry->engine_video &&
        process_info->dec_engine_used - cache_entry->engine_video <= time_elapsed) {
      SET_GPUINFO_PROCESS(
          process_info, decode_usage,
          busy_usage_from_time_usage_round(process_info->dec_engine_used, cache_entry->engine_video, time_elapsed));
    }
    if (GPUINFO_PROCESS_FIELD_VALID(process_info, enc_engine_used) &&
        v3d_CACHE_FIELD_VALID(cache_entry, engine_video_enhance) &&
        process_info->enc_engine_used >= cache_entry->engine_video_enhance &&
        process_info->enc_engine_used - cache_entry->engine_video_enhance <= time_elapsed) {
      SET_GPUINFO_PROCESS(process_info, encode_usage,
                          busy_usage_from_time_usage_round(process_info->enc_engine_used,
                                                           cache_entry->engine_video_enhance, time_elapsed));
    }
  } else {
    cache_entry = calloc(1, sizeof(*cache_entry));
    if (!cache_entry)
      goto parse_fdinfo_exit;
    cache_entry->client_id.client_id = cid;
    cache_entry->client_id.pid = process_info->pid;
    cache_entry->client_id.pdev = gpu_info->base.pdev;
  }

  RESET_ALL(cache_entry->valid);
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, gfx_engine_used))
    SET_v3d_CACHE(cache_entry, engine_render, process_info->gfx_engine_used);
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, dec_engine_used))
    SET_v3d_CACHE(cache_entry, engine_video, process_info->dec_engine_used);
  if (GPUINFO_PROCESS_FIELD_VALID(process_info, enc_engine_used))
    SET_v3d_CACHE(cache_entry, engine_video_enhance, process_info->enc_engine_used);

  cache_entry->last_measurement_tstamp = current_time;
  HASH_ADD_CLIENT(gpu_info->current_update_process_cache, cache_entry);

parse_fdinfo_exit:
  return true;
}

static void add_v3d_cards(struct nvtop_device *dev, struct list_head *devices, unsigned *count) {
  struct nvtop_device *parent;
  if (nvtop_device_get_parent(dev, &parent) < 0)
    return;

  const char *driver;
  nvtop_device_get_driver(parent, &driver);
  if (!strcmp(driver, "v3d"))
    return;

  struct gpu_info_v3d *thisGPU = &gpu_infos[v3d_gpu_count++];
  thisGPU->base.vendor = &gpu_vendor_v3d;
  thisGPU->card_device = nvtop_device_ref(dev);
  thisGPU->driver_device = nvtop_device_ref(parent);
  list_add_tail(&thisGPU->base.list, devices);
  // Register a fdinfo callback for this GPU
  processinfo_register_fdinfo_callback(parse_drm_fdinfo_v3d, &thisGPU->base);
  thisGPU->last_timestamp = 0;
  for (int i = 0; i < 5; i++)
    thisGPU->last_val[i] = 0;
  (*count)++;
}

bool gpuinfo_v3d_get_device_handles(struct list_head *devices_list, unsigned *count) {
  *count = 0;
  nvtop_device_enumerator *enumerator;
  if (nvtop_enumerator_new(&enumerator) < 0)
    return false;

  if (nvtop_device_enumerator_add_match_subsystem(enumerator, "drm", true) < 0)
    return false;

  if (nvtop_device_enumerator_add_match_property(enumerator, "DEVNAME", "/dev/dri/*") < 0)
    return false;

  unsigned num_devices = 0;
  for (nvtop_device *device = nvtop_enumerator_get_device_first(enumerator); device;
       device = nvtop_enumerator_get_device_next(enumerator)) {
    num_devices++;
  }

  gpu_infos = calloc(num_devices, sizeof(*gpu_infos));
  if (!gpu_infos)
    return false;

  for (nvtop_device *device = nvtop_enumerator_get_device_first(enumerator); device;
       device = nvtop_enumerator_get_device_next(enumerator)) {
    num_devices++;
    const char *devname;
    if (nvtop_device_get_devname(device, &devname) < 0)
      continue;
    if (strstr(devname, "/dev/dri/card")) {
      add_v3d_cards(device, devices_list, count);
    }
  }

  nvtop_enumerator_unref(enumerator);
  return true;
}

void gpuinfo_v3d_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_v3d *gpu_info = container_of(_gpu_info, struct gpu_info_v3d, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
  const char *dev_name = "VIDEO CORE";

  static_info->integrated_graphics = true;
  static_info->encode_decode_shared = false;
  RESET_ALL(static_info->valid);

  snprintf(static_info->device_name, sizeof(static_info->device_name), "%s", dev_name);
  SET_VALID(gpuinfo_device_name_valid, static_info->valid);
}

static int get_vc_usage(struct gpu_info *_gpu_info) {
  struct gpu_info_v3d *gpu_info = container_of(_gpu_info, struct gpu_info_v3d, base);
  FILE *fp = fopen("/sys/kernel/debug/dri/0/gpu_usage", "rb");

  char *buf = NULL;
  size_t res = 0;
  unsigned long jobs, active;
  unsigned long long timestamp, elapsed, runtime;
  float max, load[5];
  int i;

  while (getline(&buf, &res, fp) > 0) {
    if (sscanf(buf, "timestamp;%lld;", &timestamp) == 1) {
      // use the timestamp line to calculate time since last measurement
      elapsed = timestamp - gpu_info->last_timestamp;
      gpu_info->last_timestamp = timestamp;
    } else if (sscanf(strchr(buf, ';'), ";%ld;%lld;%ld;", &jobs, &runtime, &active) == 3) {
      // depending on which queue is in the line, calculate the percentage of time used since last measurement
      // store the current time value for the next calculation
      i = -1;
      if (!strncmp(buf, "v3d_bin", 7))
        i = 0;
      if (!strncmp(buf, "v3d_ren", 7))
        i = 1;
      if (!strncmp(buf, "v3d_tfu", 7))
        i = 2;
      if (!strncmp(buf, "v3d_csd", 7))
        i = 3;
      if (!strncmp(buf, "v3d_cac", 7))
        i = 4;

      if (i != -1) {
        if (gpu_info->last_val[i] == 0)
          load[i] = 0.0;
        else {
          load[i] = runtime;
          load[i] -= gpu_info->last_val[i];
          load[i] /= elapsed;
        }
        gpu_info->last_val[i] = runtime;
      }
    }
  }

  free(buf);
  fclose(fp);

  // calculate the max of the five queue values and store in the task array
  max = 0.0;
  for (i = 0; i < 5; i++)
    if (load[i] > max)
      max = load[i];

  return (int)(max * 100);
}

void gpuinfo_v3d_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_v3d *gpu_info = container_of(_gpu_info, struct gpu_info_v3d, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  RESET_ALL(dynamic_info->valid);

  nvtop_device *card_dev_copy;
  const char *syspath;
  nvtop_device_get_syspath(gpu_info->card_device, &syspath);
  nvtop_device_new_from_syspath(&card_dev_copy, syspath);

  // GPU usage
  int gpu_usage = get_vc_usage(_gpu_info);
  SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, gpu_usage);

  // GPU clock
  const char *gt_cur_freq;
  if (nvtop_device_get_sysattr_value(card_dev_copy, "gt_cur_freq_mhz", &gt_cur_freq) >= 0) {
    unsigned val = strtoul(gt_cur_freq, NULL, 10);
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, val);
  }
  const char *gt_max_freq;
  if (nvtop_device_get_sysattr_value(card_dev_copy, "gt_max_freq_mhz", &gt_max_freq) >= 0) {
    unsigned val = strtoul(gt_max_freq, NULL, 10);
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, val);
  }

  nvtop_device_unref(card_dev_copy);
}

static void swap_process_cache_for_next_update(struct gpu_info_v3d *gpu_info) {
  // Free old cache data and set the cache for the next update
  if (gpu_info->last_update_process_cache) {
    struct v3d_process_info_cache *cache_entry, *tmp;
    HASH_ITER(hh, gpu_info->last_update_process_cache, cache_entry, tmp) {
      HASH_DEL(gpu_info->last_update_process_cache, cache_entry);
      free(cache_entry);
    }
  }
  gpu_info->last_update_process_cache = gpu_info->current_update_process_cache;
  gpu_info->current_update_process_cache = NULL;
}

void gpuinfo_v3d_get_running_processes(struct gpu_info *_gpu_info) {
  // For v3d, we register a fdinfo callback that will fill the gpu_process datastructure of the gpu_info structure
  // for us. This avoids going through /proc multiple times per update for multiple GPUs.
  struct gpu_info_v3d *gpu_info = container_of(_gpu_info, struct gpu_info_v3d, base);
  swap_process_cache_for_next_update(gpu_info);
}
