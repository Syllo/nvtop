/*
 *
 * Copyright (C) 2017-2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
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
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nvtop/extract_gpuinfo.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/get_process_info.h"
#include "nvtop/time.h"
#include "uthash.h"

#define HASH_FIND_PID(head, key_ptr, out_ptr)                                  \
  HASH_FIND(hh, head, key_ptr, sizeof(*key_ptr), out_ptr)

#define HASH_ADD_PID(head, in_ptr)                                             \
  HASH_ADD(hh, head, pid, sizeof(pid_t), in_ptr)

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

void register_gpu_vendor(struct gpu_vendor *vendor) {
  list_add(&vendor->list, &gpu_vendors);
}

bool gpuinfo_init_info_extraction(ssize_t mask, unsigned *devices_count,
                                  struct list_head *devices) {
  struct gpu_vendor *vendor;

  *devices_count = 0;
  list_for_each_entry(vendor, &gpu_vendors, list) {
    unsigned vendor_devices_count = 0;

    if (vendor->init()) {
      bool retval = vendor->get_device_handles(
          devices, &vendor_devices_count, &mask);
      if (!retval || (retval && vendor_devices_count == 0)) {
        vendor->shutdown();
        vendor_devices_count = 0;
      }
    }

    *devices_count += vendor_devices_count;
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

  list_for_each_entry(vendor, &gpu_vendors, list) {
    vendor->shutdown();
  }
  gpuinfo_clear_cache();
  return true;
}

bool gpuinfo_populate_static_infos(struct list_head *devices) {
  struct gpu_info *device;

  list_for_each_entry(device, devices, list) {
    device->vendor->populate_static_info(device);
  }
  return true;
}

bool gpuinfo_refresh_dynamic_info(struct list_head *devices) {
  struct gpu_info *device;

  list_for_each_entry(device, devices, list) {
    device->vendor->refresh_dynamic_info(device);
  }
  return true;
}

#undef MYMIN
#define MYMIN(a, b) (((a) < (b)) ? (a) : (b))
bool gpuinfo_fix_dynamic_info_from_process_info(struct list_head *devices) {
  struct gpu_info *device;

  list_for_each_entry(device, devices, list) {

    // If the global GPU usage is not available, try computing it from the processes info
    struct gpuinfo_dynamic_info *dynamic_info = &device->dynamic_info;
    if (!IS_VALID(gpuinfo_gpu_util_rate_valid, dynamic_info->valid)) {
      for (unsigned processIdx = 0; processIdx < device->processes_count; ++processIdx) {
        struct gpu_process *process_info = &device->processes[processIdx];
        if (IS_VALID(gpuinfo_process_gpu_usage_valid, process_info->valid)) {
          if (IS_VALID(gpuinfo_gpu_util_rate_valid, dynamic_info->valid)) {
            dynamic_info->gpu_util_rate = MYMIN(100, dynamic_info->gpu_util_rate + process_info->gpu_usage);
          } else {
            dynamic_info->gpu_util_rate = MYMIN(100, process_info->gpu_usage);
            SET_VALID(gpuinfo_gpu_util_rate_valid, dynamic_info->valid);
          }
        }
      }
    }
  }
  return true;
}
#undef MYMIN

static void gpuinfo_populate_process_infos(struct list_head *devices) {
  struct gpu_info *device;

  list_for_each_entry(device, devices, list) {
    for (unsigned j = 0; j < device->processes_count; ++j) {
      pid_t current_pid = device->processes[j].pid;
      struct process_info_cache *cached_pid_info;

      HASH_FIND_PID(cached_process_info, &current_pid, cached_pid_info);
      if (!cached_pid_info) {
        HASH_FIND_PID(updated_process_info, &current_pid, cached_pid_info);
        if (!cached_pid_info) {
          // Newly encountered pid
          cached_pid_info = malloc(sizeof(*cached_pid_info));
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
        device->processes[j].cmdline = cached_pid_info->cmdline;
        SET_VALID(gpuinfo_process_cmdline_valid, device->processes[j].valid);
      }
      if (cached_pid_info->user_name) {
        device->processes[j].user_name = cached_pid_info->user_name;
        SET_VALID(gpuinfo_process_user_name_valid,
                  device->processes[j].valid);
      }

      struct process_cpu_usage cpu_usage;
      if (get_process_info(current_pid, &cpu_usage)) {
        if (cached_pid_info->last_total_consumed_cpu_time > -1.) {
          double usage_percent =
              round(100. *
                    (cpu_usage.total_user_time + cpu_usage.total_kernel_time -
                     cached_pid_info->last_total_consumed_cpu_time) /
                    nvtop_difftime(cached_pid_info->last_measurement_timestamp,
                                   cpu_usage.timestamp));
          device->processes[j].cpu_usage = (unsigned)usage_percent;
        } else {
          device->processes[j].cpu_usage = 0;
        }
        SET_VALID(gpuinfo_process_cpu_usage_valid,
                  device->processes[j].valid);
        cached_pid_info->last_measurement_timestamp = cpu_usage.timestamp;
        cached_pid_info->last_total_consumed_cpu_time =
            cpu_usage.total_kernel_time + cpu_usage.total_user_time;
        device->processes[j].cpu_memory_res = cpu_usage.resident_memory;
        SET_VALID(gpuinfo_process_cpu_memory_res_valid,
                  device->processes[j].valid);
        device->processes[j].cpu_memory_virt = cpu_usage.virtual_memory;
        SET_VALID(gpuinfo_process_cpu_memory_virt_valid,
                  device->processes[j].valid);
      } else {
        cached_pid_info->last_total_consumed_cpu_time = -1;
      }

      // Process memory usage percent of total device memory
      if (IS_VALID(gpuinfo_total_memory_valid, device->dynamic_info.valid) &&
          IS_VALID(gpuinfo_process_gpu_memory_usage_valid,
                   device->processes[j].valid)) {
        float percentage =
            roundf(100.f * (float)device->processes[j].gpu_memory_usage /
                   (float)device->dynamic_info.total_memory);
        device->processes[j].gpu_memory_percentage = (unsigned)percentage;
        assert(device->processes[j].gpu_memory_percentage <= 100);
        SET_VALID(gpuinfo_process_gpu_memory_percentage_valid,
                  device->processes[j].valid);
      }
    }
  }
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

  list_for_each_entry(device, devices, list) {
    unsigned processes_count = 0;
    struct gpu_process *processes = NULL;
    device->vendor->get_running_processes(device,
                                          &processes_count, &processes);
    free(device->processes);
    device->processes = processes;
    device->processes_count = processes_count;
  }

  gpuinfo_populate_process_infos(devices);

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
