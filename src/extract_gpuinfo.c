/*
 *
 * Copyright (C) 2017-2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
 *
 * This file is part of Nvtop.
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
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nvtop/extract_gpuinfo.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/get_process_info.h"
#include "nvtop/time.h"
#include "uthash.h"

#define HASH_FIND_PID(head, key_ptr, out_ptr) HASH_FIND(hh, head, key_ptr, sizeof(*key_ptr), out_ptr)

#define HASH_ADD_PID(head, in_ptr) HASH_ADD(hh, head, pid, sizeof(pid_t), in_ptr)

const char drm_pdev[] = "drm-pdev";
const char drm_client_id[] = "drm-client-id";

struct process_info_cache {
  pid_t pid;
  char *cmdline;
  char *user_name;
  double last_total_consumed_cpu_time;
  nvtop_time last_measurement_timestamp;
  UT_hash_handle hh;
};

struct process_info_cache *cached_process_info = NULL;
struct process_info_cache *updated_process_info = NULL;

static LIST_HEAD(gpu_vendors);

void register_gpu_vendor(struct gpu_vendor *vendor) { list_add(&vendor->list, &gpu_vendors); }

bool gpuinfo_init_info_extraction(unsigned *monitored_dev_count, struct list_head *devices) {
  struct gpu_vendor *vendor;

  *monitored_dev_count = 0;
  list_for_each_entry(vendor, &gpu_vendors, list) {
    unsigned vendor_devices_count = 0;

    if (vendor->init()) {
      bool retval = vendor->get_device_handles(devices, &vendor_devices_count);
      if (!retval || (retval && vendor_devices_count == 0)) {
        vendor->shutdown();
        vendor_devices_count = 0;
      }
    }

    *monitored_dev_count += vendor_devices_count;
  }

  return true;
}

bool gpuinfo_shutdown_info_extraction(struct list_head *devices) {
  struct gpu_info *device, *tmp;
  struct gpu_vendor *vendor;

  list_for_each_entry_safe(device, tmp, devices, list) {
    free(device->processes);
    list_del(&device->list);
  }

  list_for_each_entry(vendor, &gpu_vendors, list) { vendor->shutdown(); }
  gpuinfo_clear_cache();
  return true;
}

bool gpuinfo_populate_static_infos(struct list_head *devices) {
  struct gpu_info *device;

  list_for_each_entry(device, devices, list) { device->vendor->populate_static_info(device); }
  return true;
}

bool gpuinfo_refresh_dynamic_info(struct list_head *devices) {
  struct gpu_info *device;

  list_for_each_entry(device, devices, list) { device->vendor->refresh_dynamic_info(device); }
  return true;
}

#undef MYMIN
#define MYMIN(a, b) (((a) < (b)) ? (a) : (b))
bool gpuinfo_fix_dynamic_info_from_process_info(struct list_head *devices) {
  struct gpu_info *device;

  list_for_each_entry(device, devices, list) {

    struct gpuinfo_dynamic_info *dynamic_info = &device->dynamic_info;
    // If the global GPU usage is not available, try computing it from the processes info
    // Some integrated AMDGPU report 100% usage while process usage is actually low

    bool needGpuRate = true;
    bool validReportedGpuRate = GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, gpu_util_rate);
    unsigned reportedGpuRate = dynamic_info->gpu_util_rate;
    RESET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate);

    // AMDGPU does not provide encode and decode utilization through the DRM sensor info.
    // Update them here since per-process sysfs exposes this information.
    bool needGpuEncode = !GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, encoder_rate);
    bool needGpuDecode = !GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, decoder_rate);
    if (needGpuRate || needGpuEncode || needGpuDecode) {
      for (unsigned processIdx = 0; processIdx < device->processes_count; ++processIdx) {
        struct gpu_process *process_info = &device->processes[processIdx];
        if (needGpuRate && GPUINFO_PROCESS_FIELD_VALID(process_info, gpu_usage)) {
          if (GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, gpu_util_rate)) {
            dynamic_info->gpu_util_rate = MYMIN(100, dynamic_info->gpu_util_rate + process_info->gpu_usage);
          } else {
            SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, MYMIN(100, process_info->gpu_usage));
          }
        }
        if (needGpuEncode && GPUINFO_PROCESS_FIELD_VALID(process_info, encode_usage)) {
          if (GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, encoder_rate)) {
            dynamic_info->encoder_rate = MYMIN(100, dynamic_info->encoder_rate + process_info->encode_usage);
          } else {
            SET_GPUINFO_DYNAMIC(dynamic_info, encoder_rate, MYMIN(100, process_info->encode_usage));
          }
        }
        if (needGpuDecode && GPUINFO_PROCESS_FIELD_VALID(process_info, decode_usage)) {
          if (GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, decoder_rate)) {
            dynamic_info->decoder_rate = MYMIN(100, dynamic_info->decoder_rate + process_info->decode_usage);
          } else {
            SET_GPUINFO_DYNAMIC(dynamic_info, decoder_rate, MYMIN(100, process_info->decode_usage));
          }
        }
      }
    }
    if (!GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, gpu_util_rate) && validReportedGpuRate) {
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, reportedGpuRate);
    } else if (GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, gpu_util_rate) && validReportedGpuRate) {
      SET_GPUINFO_DYNAMIC(
          dynamic_info, gpu_util_rate,
          (dynamic_info->gpu_util_rate > reportedGpuRate ? dynamic_info->gpu_util_rate : reportedGpuRate));
    }
  }
  return true;
}
#undef MYMIN

static void gpuinfo_populate_process_info(struct gpu_info *device) {
  for (unsigned j = 0; j < device->processes_count; ++j) {
    pid_t current_pid = device->processes[j].pid;
    struct process_info_cache *cached_pid_info;

    HASH_FIND_PID(cached_process_info, &current_pid, cached_pid_info);
    if (!cached_pid_info) {
      HASH_FIND_PID(updated_process_info, &current_pid, cached_pid_info);
      if (!cached_pid_info) {
        // Newly encountered pid
        cached_pid_info = calloc(1, sizeof(*cached_pid_info));
        cached_pid_info->pid = current_pid;
        get_username_from_pid(current_pid, &cached_pid_info->user_name);
        get_command_from_pid(current_pid, &cached_pid_info->cmdline);
        cached_pid_info->last_total_consumed_cpu_time = -1.;
        HASH_ADD_PID(updated_process_info, cached_pid_info);
      }
    } else {
      // Already encountered so delete from cached list to avoid freeing
      // memory at the end of this function
      HASH_DEL(cached_process_info, cached_pid_info);
      HASH_ADD_PID(updated_process_info, cached_pid_info);
    }

    if (cached_pid_info->cmdline) {
      SET_GPUINFO_PROCESS(&device->processes[j], cmdline, cached_pid_info->cmdline);
    }
    if (cached_pid_info->user_name) {
      SET_GPUINFO_PROCESS(&device->processes[j], user_name, cached_pid_info->user_name);
    }

    struct process_cpu_usage cpu_usage;
    if (get_process_info(current_pid, &cpu_usage)) {
      if (cached_pid_info->last_total_consumed_cpu_time > -1.) {
        double usage_percent = round(
            100. *
            (cpu_usage.total_user_time + cpu_usage.total_kernel_time - cached_pid_info->last_total_consumed_cpu_time) /
            nvtop_difftime(cached_pid_info->last_measurement_timestamp, cpu_usage.timestamp));
        SET_GPUINFO_PROCESS(&device->processes[j], cpu_usage, (unsigned)usage_percent);
      } else {
        SET_GPUINFO_PROCESS(&device->processes[j], cpu_usage, 0);
      }
      SET_GPUINFO_PROCESS(&device->processes[j], cpu_memory_res, cpu_usage.resident_memory);
      SET_GPUINFO_PROCESS(&device->processes[j], cpu_memory_virt, cpu_usage.virtual_memory);
      cached_pid_info->last_measurement_timestamp = cpu_usage.timestamp;
      cached_pid_info->last_total_consumed_cpu_time = cpu_usage.total_kernel_time + cpu_usage.total_user_time;
    } else {
      cached_pid_info->last_total_consumed_cpu_time = -1;
    }

    // Process memory usage percent of total device memory
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, total_memory) &&
        GPUINFO_PROCESS_FIELD_VALID(&device->processes[j], gpu_memory_usage)) {
      float percentage =
          roundf(100.f * (float)device->processes[j].gpu_memory_usage / (float)device->dynamic_info.total_memory);
      SET_GPUINFO_PROCESS(&device->processes[j], gpu_memory_percentage, (unsigned)percentage);
      assert(device->processes[j].gpu_memory_percentage <= 100);
    }
  }
}

static void gpuinfo_clean_old_cache(void) {
  struct process_info_cache *pid_not_encountered, *tmp;
  HASH_ITER(hh, cached_process_info, pid_not_encountered, tmp) {
    HASH_DEL(cached_process_info, pid_not_encountered);
    free(pid_not_encountered->cmdline);
    free(pid_not_encountered->user_name);
    free(pid_not_encountered);
  }
  cached_process_info = updated_process_info;
  updated_process_info = NULL;
}

bool gpuinfo_refresh_processes(struct list_head *devices) {
  struct gpu_info *device;

  list_for_each_entry(device, devices, list) { device->processes_count = 0; }

  // Go through the /proc hierarchy once and populate the processes for all registered GPUs
  processinfo_sweep_fdinfos();

  list_for_each_entry(device, devices, list) {
    device->vendor->refresh_running_processes(device);
    gpuinfo_populate_process_info(device);
  }
  gpuinfo_clean_old_cache();

  return true;
}

bool gpuinfo_utilisation_rate(struct list_head *devices) {
  struct gpu_info *device;

  list_for_each_entry(device, devices, list) {
    if (device->vendor->refresh_utilisation_rate)
      device->vendor->refresh_utilisation_rate(device);
  }

  return true;
}

void gpuinfo_clear_cache(void) {
  if (cached_process_info) {
    struct process_info_cache *pid_cached, *tmp;
    HASH_ITER(hh, cached_process_info, pid_cached, tmp) {
      HASH_DEL(cached_process_info, pid_cached);
      free(pid_cached->cmdline);
      free(pid_cached->user_name);
      free(pid_cached);
    }
  }
}

bool extract_drm_fdinfo_key_value(char *buf, char **key, char **val) {
  char *p = buf;

  p = index(buf, ':');
  if (!p || p == buf)
    return false;
  *p = '\0';

  while (*++p && isspace(*p))
    ;
  if (!*p)
    return false;

  *key = buf;
  *val = p;

  return true;
}

extern inline unsigned busy_usage_from_time_usage_round(uint64_t current_use_ns, uint64_t previous_use_ns,
                                                        uint64_t time_between_measurement);

unsigned nvtop_pcie_gen_from_link_speed(unsigned linkSpeed) {
  unsigned pcieGen = 0;
  switch (linkSpeed) {
  case 2:
    pcieGen = 1;
    break;
  case 5:
    pcieGen = 2;
    break;
  case 8:
    pcieGen = 3;
    break;
  case 16:
    pcieGen = 4;
    break;
  case 32:
    pcieGen = 5;
    break;
  case 64:
    pcieGen = 6;
    break;
  }
  return pcieGen;
}

void gpuinfo_refresh_utilisation_rate(struct gpu_info *gpu_info) {
  /*
   * kernel Documentation/gpu/drm-usage-stats.rst:
   *  - drm-maxfreq-<keystr>: <uint> [Hz|MHz|KHz]
   *  Engine identifier string must be the same as the one specified in the
   *  drm-engine-<keystr> tag and shall contain the maximum frequency for the given
   *  engine. Taken together with drm-cycles-<keystr>, this can be used to calculate
   *  percentage utilization of the engine, whereas drm-engine-<keystr> only reflects
   *  time active without considering what frequency the engine is operating as a
   *  percentage of it's maximum frequency.
   *
   */

  uint64_t gfx_total_process_cycles = 0;
  uint64_t total_delta = 0;
  unsigned int utilisation_rate;
  uint64_t max_freq_hz;
  double avg_delta_secs;

  for (unsigned processIdx = 0; processIdx < gpu_info->processes_count; ++processIdx) {
    struct gpu_process *process_info = &gpu_info->processes[processIdx];

    gfx_total_process_cycles += process_info->gpu_cycles;
    total_delta += process_info->sample_delta;
  }

  if (!gfx_total_process_cycles)
    return;

  avg_delta_secs = ((double)total_delta / gpu_info->processes_count) / 1000000000.0;
  max_freq_hz = gpu_info->dynamic_info.gpu_clock_speed_max * 1000000;
  utilisation_rate = (unsigned int)((((double)gfx_total_process_cycles) / (((double)max_freq_hz) * avg_delta_secs * 2)) * 100);
  utilisation_rate = utilisation_rate > 100 ? 100 : utilisation_rate;

  SET_GPUINFO_DYNAMIC(&gpu_info->dynamic_info, gpu_util_rate, utilisation_rate);
}
