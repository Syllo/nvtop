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
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <uthash.h>

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
    if (current->card_fd)
      close(current->card_fd);
    nvtop_device_unref(current->card_device);
    nvtop_device_unref(current->driver_device);
  }
}

const char *gpuinfo_intel_last_error_string(void) { return "Err"; }

static bool parse_drm_fdinfo_intel(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {
  struct gpu_info_intel *gpu_info = container_of(info, struct gpu_info_intel, base);
  switch (gpu_info->driver) {
  case DRIVER_I915:
    return parse_drm_fdinfo_intel_i915(info, fdinfo_file, process_info);
  case DRIVER_XE:
    return parse_drm_fdinfo_intel_xe(info, fdinfo_file, process_info);
  }
  return false;
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

  const char *devname;
  if (nvtop_device_get_devname(thisGPU->card_device, &devname) >= 0)
    thisGPU->card_fd = open(devname, O_WRONLY);

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

  // Mark integrated GPUs
  if (strcmp(gpu_info->base.pdev, INTEGRATED_I915_GPU_PCI_ID) == 0) {
    static_info->integrated_graphics = true;
  }

  nvtop_pcie_link max_link_characteristics;
  int ret = nvtop_device_maximum_pcie_link(gpu_info->driver_device, &max_link_characteristics);
  if (ret >= 0) {
    // Some cards report PCIe GEN 1@ 1x, attempt to detect this and get the card's bridge link speeds
    gpu_info->bridge_device = gpu_info->driver_device;
    struct nvtop_device *parent;
    const char *vendor, *class;
    unsigned attempts = 0;
    while (ret >= 0 && static_info->integrated_graphics == false &&
           // check likely incorrect speed
           max_link_characteristics.width == 1 && max_link_characteristics.speed == 2 &&
           // check vendor
           nvtop_device_get_sysattr_value(gpu_info->bridge_device, "vendor", &vendor) == 0 &&
           strcmp(vendor, VENDOR_INTEL_STR) == 0 &&
           // check class is either VGA or (non-host) PCI Bridge
           nvtop_device_get_sysattr_value(gpu_info->bridge_device, "class", &class) == 0 &&
           (strcmp(class, "0x030000") == 0 || strcmp(class, "0x060400") == 0) &&
           // don't go more than 2 levels up
           attempts++ < 2) {
      ret = nvtop_device_get_parent(gpu_info->bridge_device, &parent);
      if (ret >= 0 && nvtop_device_maximum_pcie_link(parent, &max_link_characteristics) >= 0) {
        gpu_info->bridge_device = parent;
      }
    }
    SET_GPUINFO_STATIC(static_info, max_pcie_link_width, max_link_characteristics.width);
    unsigned pcieGen = nvtop_pcie_gen_from_link_speed(max_link_characteristics.speed);
    SET_GPUINFO_STATIC(static_info, max_pcie_gen, pcieGen);
  }
}

void gpuinfo_intel_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_intel *gpu_info = container_of(_gpu_info, struct gpu_info_intel, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  bool is_xe = gpu_info->driver == DRIVER_XE;

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
  nvtop_device *bridge_dev_noncached = NULL;
  if (gpu_info->bridge_device) {
    if (nvtop_device_get_syspath(gpu_info->bridge_device, &syspath) >= 0)
      nvtop_device_new_from_syspath(&bridge_dev_noncached, syspath);
  } else {
    bridge_dev_noncached = driver_dev_noncached;
  }

  nvtop_device *clock_device = is_xe ? driver_dev_noncached : card_dev_noncached;
  // GPU clock
  const char *gt_cur_freq;
  const char *gt_cur_freq_sysattr = is_xe ? "tile0/gt0/freq0/cur_freq" : "gt_cur_freq_mhz";
  if (nvtop_device_get_sysattr_value(clock_device, gt_cur_freq_sysattr, &gt_cur_freq) >= 0) {
    unsigned val = strtoul(gt_cur_freq, NULL, 10);
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, val);
  }
  const char *gt_max_freq;
  const char *gt_max_freq_sysattr = is_xe ? "tile0/gt0/freq0/max_freq" : "gt_max_freq_mhz";
  if (nvtop_device_get_sysattr_value(clock_device, gt_max_freq_sysattr, &gt_max_freq) >= 0) {
    unsigned val = strtoul(gt_max_freq, NULL, 10);
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, val);
  }

  if (!static_info->integrated_graphics) {
    nvtop_pcie_link curr_link_characteristics;
    int ret = nvtop_device_current_pcie_link(bridge_dev_noncached, &curr_link_characteristics);
    if (ret >= 0) {
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_width, curr_link_characteristics.width);
      unsigned pcieGen = nvtop_pcie_gen_from_link_speed(curr_link_characteristics.speed);
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_gen, pcieGen);
    }
  }

  if (hwmon_dev_noncached) {
    const char *hwmon_fan;
    if (nvtop_device_get_sysattr_value(hwmon_dev_noncached, "fan1_input", &hwmon_fan) >= 0) {
      unsigned val = strtoul(hwmon_fan, NULL, 10);
      SET_GPUINFO_DYNAMIC(dynamic_info, fan_rpm, val);
    }

    const char *hwmon_temp;
    // temp1 is for i915, temp2 is for `pkg` on xe
    if (nvtop_device_get_sysattr_value(hwmon_dev_noncached, is_xe ? "temp2_input" : "temp1_input", &hwmon_temp) >= 0) {
      unsigned val = strtoul(hwmon_temp, NULL, 10);
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, val / 1000);
    }

    const char *hwmon_power_max = NULL;
    const char *hwmon_energy = NULL;
    for (unsigned i = 0; i < (is_xe ? 2 : 1); i++) {
      // Max Power
      if (hwmon_power_max == NULL || hwmon_power_max[0] == '0') {
        // power1 is for i915 and `card` on supported cards on xe, power2 is `pkg` on xe
        nvtop_device_get_sysattr_value(hwmon_dev_noncached, i == 0 ? "power1_max" : "power2_max", &hwmon_power_max);
      }
      if (hwmon_power_max == NULL || hwmon_power_max[0] == '0') {
        // Battlemage (xe) uses power*_crit
        nvtop_device_get_sysattr_value(hwmon_dev_noncached, i == 0 ? "power1_crit" : "power2_crit", &hwmon_power_max);
      }
      if (hwmon_power_max == NULL || hwmon_power_max[0] == '0') {
        // Both drivers have this, but it seems to be 0
        nvtop_device_get_sysattr_value(hwmon_dev_noncached, i == 0 ? "power1_rated_max" : "power2_rated_max", &hwmon_power_max);
      }

      // Energy Usage
      if (hwmon_energy == NULL || hwmon_energy[0] == '0') {
        // energy1 is for i915 and `card` on supported cards on xe, energy2 is `pkg` on xe
        nvtop_device_get_sysattr_value(hwmon_dev_noncached, i == 0 ? "energy1_input" : "energy2_input", &hwmon_energy);
      }
    }

    // Check if we found the max power draw
    if (hwmon_power_max != NULL) {
      unsigned val = strtoul(hwmon_power_max, NULL, 10);
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, val / 1000);
    }

    // Check if we found the energy usage and convert it into a wattage
    if (hwmon_energy != NULL) {
      nvtop_time ts;
      nvtop_get_current_time(&ts);
      unsigned val = strtoul(hwmon_energy, NULL, 10);
      // Skip the first update so we have a time delta
      if (gpu_info->energy.time.tv_sec != 0) {
        unsigned old = gpu_info->energy.energy_uj;
        uint64_t time = nvtop_difftime_u64(gpu_info->energy.time, ts);
        unsigned power = ((val - old) * 1000000000LL) / time;
        SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, power / 1000);
      }
      gpu_info->energy.energy_uj = val;
      gpu_info->energy.time = ts;
    }
  }

  switch (gpu_info->driver) {
  case DRIVER_I915:
    gpuinfo_intel_i915_refresh_dynamic_info(_gpu_info);
    break;
  case DRIVER_XE:
    gpuinfo_intel_xe_refresh_dynamic_info(_gpu_info);
    break;
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
