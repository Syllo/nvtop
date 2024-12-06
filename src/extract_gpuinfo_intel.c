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

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <uthash.h>

#define HASH_FIND_CLIENT(head, key_ptr, out_ptr) HASH_FIND(hh, head, key_ptr, sizeof(struct unique_cache_id), out_ptr)
#define HASH_ADD_CLIENT(head, in_ptr) HASH_ADD(hh, head, client_id, sizeof(struct unique_cache_id), in_ptr)

#define SET_INTEL_CACHE(cachePtr, field, value) SET_VALUE(cachePtr, field, value, intel_cache_)
#define RESET_INTEL_CACHE(cachePtr, field) INVALIDATE_VALUE(cachePtr, field, intel_cache_)
#define INTEL_CACHE_FIELD_VALID(cachePtr, field) VALUE_IS_VALID(cachePtr, field, intel_cache_)

enum intel_process_info_cache_valid {
  intel_cache_engine_render_valid = 0,
  intel_cache_engine_copy_valid,
  intel_cache_engine_video_valid,
  intel_cache_engine_video_enhance_valid,
  intel_cache_process_info_cache_valid_count
};

struct __attribute__((__packed__)) unique_cache_id {
  unsigned client_id;
  pid_t pid;
  char *pdev;
};

struct intel_process_info_cache {
  struct unique_cache_id client_id;
  uint64_t engine_render;
  uint64_t engine_copy;
  uint64_t engine_video;
  uint64_t engine_video_enhance;
  nvtop_time last_measurement_tstamp;
  unsigned char valid[(intel_cache_process_info_cache_valid_count + CHAR_BIT - 1) / CHAR_BIT];
  UT_hash_handle hh;
};

struct gpu_info_intel {
  struct gpu_info base;
  enum { DRIVER_I915, DRIVER_XE } driver;

  struct nvtop_device *card_device;
  struct nvtop_device *driver_device;
  struct nvtop_device *hwmon_device;
  struct intel_process_info_cache *last_update_process_cache, *current_update_process_cache; // Cached processes info

  struct {
    unsigned energy_uj;
    nvtop_time time;
  } energy;
};

static bool gpuinfo_intel_init(void);
static void gpuinfo_intel_shutdown(void);
static const char *gpuinfo_intel_last_error_string(void);
static bool gpuinfo_intel_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_intel_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_intel_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_intel_get_running_processes(struct gpu_info *_gpu_info);

struct gpu_vendor gpu_vendor_intel = {
    .init = gpuinfo_intel_init,
    .shutdown = gpuinfo_intel_shutdown,
    .last_error_string = gpuinfo_intel_last_error_string,
    .get_device_handles = gpuinfo_intel_get_device_handles,
    .populate_static_info = gpuinfo_intel_populate_static_info,
    .refresh_dynamic_info = gpuinfo_intel_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_intel_get_running_processes,
    .name = "Intel",
};

unsigned intel_gpu_count;
static struct gpu_info_intel *gpu_infos;

#define STRINGIFY(x) STRINGIFY_HELPER_(x)
#define STRINGIFY_HELPER_(x) #x

#define VENDOR_INTEL 0x8086
#define VENDOR_INTEL_STR STRINGIFY(VENDOR_INTEL)
// The integrated Intel GPU is always this device
// Discrete GPU are others
#define INTEGRATED_I915_GPU_PCI_ID "0000:00:02.0"

__attribute__((constructor)) static void init_extract_gpuinfo_intel(void) { register_gpu_vendor(&gpu_vendor_intel); }

bool gpuinfo_intel_init(void) { return true; }
void gpuinfo_intel_shutdown(void) {
  for (unsigned i = 0; i < intel_gpu_count; ++i) {
    struct gpu_info_intel *current = &gpu_infos[i];
    nvtop_device_unref(current->card_device);
    nvtop_device_unref(current->driver_device);
  }
}

const char *gpuinfo_intel_last_error_string(void) { return "Err"; }

static const char i915_drm_intel_render[] = "drm-engine-render";
static const char i915_drm_intel_copy[] = "drm-engine-copy";
static const char i915_drm_intel_video[] = "drm-engine-video";
static const char i915_drm_intel_video_enhance[] = "drm-engine-video-enhance";
static const char i915_drm_intel_vram[] = "drm-total-local0";

static const char xe_drm_intel_vram[] = "drm-total-vram0";

static bool parse_drm_fdinfo_intel(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {
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
      
      if (!strcmp(key, i915_drm_intel_vram) || !strcmp(key, xe_drm_intel_vram)) {
        // TODO: do we count "gtt mem" too?
        unsigned long mem_int;
        char *endptr;

        mem_int = strtoul(val, &endptr, 10);
        if (endptr == val || (strcmp(endptr, " kB") && strcmp(endptr, " KiB")))
            continue;

        SET_GPUINFO_PROCESS(process_info, gpu_memory_usage, mem_int * 1024);
      } else if (is_render || is_copy || is_video || is_video_enhance) {
        char *endptr;
        uint64_t time_spent = strtoull(val, &endptr, 10);
        if (endptr == val || strcmp(endptr, " ns"))
          continue;
        if (is_render) {
          SET_GPUINFO_PROCESS(process_info, gfx_engine_used, time_spent);
        }
        if (is_copy) {
          // TODO: what is copy?
          (void)time_spent;
        }
        if (is_video) {
          // Video represents encode and decode
          SET_GPUINFO_PROCESS(process_info, dec_engine_used, time_spent);
          SET_GPUINFO_PROCESS(process_info, enc_engine_used, time_spent);
        }
        if (is_video_enhance) {
          // TODO: what is this
        }
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

  cache_entry->last_measurement_tstamp = current_time;
  HASH_ADD_CLIENT(gpu_info->current_update_process_cache, cache_entry);

parse_fdinfo_exit:
  return true;
}

static void add_intel_cards(struct nvtop_device *dev, struct list_head *devices, unsigned *count) {
  struct nvtop_device *parent;
  if (nvtop_device_get_parent(dev, &parent) < 0)
    return;
  // Consider enabled Intel cards using the i915 or xe driver
  const char *vendor, *driver, *enabled;
  if (nvtop_device_get_sysattr_value(parent, "vendor", &vendor) < 0 || strcmp(vendor, VENDOR_INTEL_STR))
    return;
  if (nvtop_device_get_driver(parent, &driver) < 0 || (strcmp(driver, "i915") && strcmp(driver, "xe")))
    return;
  if (nvtop_device_get_sysattr_value(parent, "enable", &enabled) < 0 || strcmp(enabled, "1"))
    return;

  struct gpu_info_intel *thisGPU = &gpu_infos[intel_gpu_count++];
  thisGPU->base.vendor = &gpu_vendor_intel;
  thisGPU->driver = !strcmp(driver, "xe") ? DRIVER_XE : DRIVER_I915;
  thisGPU->card_device = nvtop_device_ref(dev);
  thisGPU->driver_device = nvtop_device_ref(parent);
  thisGPU->hwmon_device = nvtop_device_get_hwmon(thisGPU->driver_device);
  const char *pdev_val;
  int retval = nvtop_device_get_property_value(thisGPU->driver_device, "PCI_SLOT_NAME", &pdev_val);
  assert(retval >= 0 && pdev_val != NULL && "Could not retrieve device PCI slot name");
  strncpy(thisGPU->base.pdev, pdev_val, PDEV_LEN);
  list_add_tail(&thisGPU->base.list, devices);
  // Register a fdinfo callback for this GPU
  processinfo_register_fdinfo_callback(parse_drm_fdinfo_intel, &thisGPU->base);
  (*count)++;
}

bool gpuinfo_intel_get_device_handles(struct list_head *devices_list, unsigned *count) {
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
      add_intel_cards(device, devices_list, count);
    }
  }

  nvtop_enumerator_unref(enumerator);
  return true;
}

void gpuinfo_intel_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_intel *gpu_info = container_of(_gpu_info, struct gpu_info_intel, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
  const char *dev_name;

  static_info->integrated_graphics = false;
  static_info->encode_decode_shared = true;
  RESET_ALL(static_info->valid);

  if (nvtop_device_get_property_value(gpu_info->driver_device, "ID_MODEL_FROM_DATABASE", &dev_name) >= 0) {
    snprintf(static_info->device_name, sizeof(static_info->device_name), "%s", dev_name);
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
    for (size_t idx = 0; idx < sizeof(static_info->device_name) && static_info->device_name[idx] != '\0'; ++idx) {
      if (static_info->device_name[idx] == '[')
        static_info->device_name[idx] = '(';
      if (static_info->device_name[idx] == ']')
        static_info->device_name[idx] = ')';
    }
  }

  nvtop_pcie_link max_link_characteristics;
  int ret = nvtop_device_maximum_pcie_link(gpu_info->driver_device, &max_link_characteristics);
  if (ret >= 0) {
    SET_GPUINFO_STATIC(static_info, max_pcie_link_width, max_link_characteristics.width);
    unsigned pcieGen = nvtop_pcie_gen_from_link_speed(max_link_characteristics.speed);
    SET_GPUINFO_STATIC(static_info, max_pcie_gen, pcieGen);
  }

  // Mark integrated GPUs
  if (strcmp(gpu_info->base.pdev, INTEGRATED_I915_GPU_PCI_ID) == 0) {
    static_info->integrated_graphics = true;
  }
}

void gpuinfo_intel_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_intel *gpu_info = container_of(_gpu_info, struct gpu_info_intel, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  RESET_ALL(dynamic_info->valid);

  // We are creating new devices because the device_get_sysattr_value caches its querries
  const char *syspath;
  nvtop_device *card_dev_noncached = NULL;
  if (nvtop_device_get_syspath(gpu_info->card_device, &syspath) >= 0)
    nvtop_device_new_from_syspath(&card_dev_noncached, syspath);
  nvtop_device *driver_dev_noncached = NULL;
  if (nvtop_device_get_syspath(gpu_info->driver_device, &syspath) >= 0)
    nvtop_device_new_from_syspath(&driver_dev_noncached, syspath);
  nvtop_device *hwmon_dev_noncached = NULL;
  if (gpu_info->hwmon_device) {
    if (nvtop_device_get_syspath(gpu_info->hwmon_device, &syspath) >= 0)
      nvtop_device_new_from_syspath(&hwmon_dev_noncached, syspath);
  }

  nvtop_device *clock_device = gpu_info->driver == DRIVER_XE ? driver_dev_noncached : card_dev_noncached;
  // GPU clock
  const char *gt_cur_freq;
  const char *gt_cur_freq_sysattr = gpu_info->driver == DRIVER_XE ? "tile0/gt0/freq0/cur_freq" : "gt_cur_freq_mhz";
  if (nvtop_device_get_sysattr_value(clock_device, gt_cur_freq_sysattr, &gt_cur_freq) >= 0) {
    unsigned val = strtoul(gt_cur_freq, NULL, 10);
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, val);
  }
  const char *gt_max_freq;
  const char *gt_max_freq_sysattr = gpu_info->driver == DRIVER_XE ? "tile0/gt0/freq0/max_freq" : "gt_max_freq_mhz";
  if (nvtop_device_get_sysattr_value(clock_device, gt_max_freq_sysattr, &gt_max_freq) >= 0) {
    unsigned val = strtoul(gt_max_freq, NULL, 10);
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, val);
  }

  // TODO: find how to extract global utilization
  // gpu util will be computed as the sum of all the processes utilization for now

  if (!static_info->integrated_graphics) {
    nvtop_pcie_link curr_link_characteristics;
    int ret = nvtop_device_current_pcie_link(driver_dev_noncached, &curr_link_characteristics);
    if (ret >= 0) {
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_width, curr_link_characteristics.width);
      unsigned pcieGen = nvtop_pcie_gen_from_link_speed(curr_link_characteristics.speed);
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_gen, pcieGen);
    }
  }

  // TODO: Attributes such as memory, fan, temperature, power info should be available once the hwmon patch lands
  if (hwmon_dev_noncached) {
    const char *hwmon_fan;
    // maxFanValue is just a guess, there is no way to get the max fan speed from hwmon
    const unsigned maxFanValue = 4000/100;
    if (nvtop_device_get_sysattr_value(hwmon_dev_noncached, "fan1_input", &hwmon_fan) >= 0) {
      unsigned val = strtoul(hwmon_fan, NULL, 10);
      SET_GPUINFO_DYNAMIC(dynamic_info, fan_speed, val / maxFanValue);
    }
    const char *hwmon_temp;
    if (nvtop_device_get_sysattr_value(hwmon_dev_noncached, "temp1_input", &hwmon_temp) >= 0) {
      unsigned val = strtoul(hwmon_temp, NULL, 10);
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, val / 1000);
    }

    const char *hwmon_power_max;
    // power1 is for i915, power2 is for xe
    if (nvtop_device_get_sysattr_value(hwmon_dev_noncached, "power1_max", &hwmon_power_max) >= 0 ||
        nvtop_device_get_sysattr_value(hwmon_dev_noncached, "power2_max", &hwmon_power_max) >= 0) {
      unsigned val = strtoul(hwmon_power_max, NULL, 10);
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, val / 1000);
    }

    const char *hwmon_energy;
    // energy1 is for i915, energy2 is for xe
    if (nvtop_device_get_sysattr_value(hwmon_dev_noncached, "energy1_input", &hwmon_energy) >= 0 ||
        nvtop_device_get_sysattr_value(hwmon_dev_noncached, "energy2_input", &hwmon_energy) >= 0) {
      nvtop_time ts, ts_diff;
      nvtop_get_current_time(&ts);
      unsigned val = strtoul(hwmon_energy, NULL, 10);
      unsigned old = gpu_info->energy.energy_uj;
      uint64_t time = nvtop_difftime_u64(gpu_info->energy.time, ts);
      // Skip the first update so we have a time delta
      if (gpu_info->energy.time.tv_sec != 0) {
        unsigned power = ((val - old) * 1000000000LL) / time;
        SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, power / 1000);
      }
      gpu_info->energy.energy_uj = val;
      gpu_info->energy.time = ts;
    }
  }

  // Let the temporary devices be garbage collected
  nvtop_device_unref(card_dev_noncached);
  nvtop_device_unref(driver_dev_noncached);
  if (hwmon_dev_noncached)
    nvtop_device_unref(hwmon_dev_noncached);
}

static void swap_process_cache_for_next_update(struct gpu_info_intel *gpu_info) {
  // Free old cache data and set the cache for the next update
  if (gpu_info->last_update_process_cache) {
    struct intel_process_info_cache *cache_entry, *tmp;
    HASH_ITER(hh, gpu_info->last_update_process_cache, cache_entry, tmp) {
      HASH_DEL(gpu_info->last_update_process_cache, cache_entry);
      free(cache_entry);
    }
  }
  gpu_info->last_update_process_cache = gpu_info->current_update_process_cache;
  gpu_info->current_update_process_cache = NULL;
}

void gpuinfo_intel_get_running_processes(struct gpu_info *_gpu_info) {
  // For Intel, we register a fdinfo callback that will fill the gpu_process datastructure of the gpu_info structure
  // for us. This avoids going through /proc multiple times per update for multiple GPUs.
  struct gpu_info_intel *gpu_info = container_of(_gpu_info, struct gpu_info_intel, base);
  swap_process_cache_for_next_update(gpu_info);
}
