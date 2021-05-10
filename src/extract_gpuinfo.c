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
#include "nvtop/extract_gpuinfo_nvidia.h"
#include "nvtop/get_process_info.h"
#include "nvtop/time.h"
#include "uthash.h"

#define HASH_FIND_PID(head, key_ptr, out_ptr)                                  \
  HASH_FIND(hh, head, key_ptr, sizeof(*key_ptr), out_ptr)

#define HASH_ADD_PID(head, in_ptr)                                             \
  HASH_ADD(hh, head, pid, sizeof(pid_t), in_ptr)

typedef struct process_info_cache_struct {
  pid_t pid;
  char *cmdline;
  char *user_name;
  double last_total_consumed_cpu_time;
  nvtop_time last_measurement_timestamp;
  UT_hash_handle hh;
} process_info_cache;

process_info_cache *cached_process_info = NULL;
process_info_cache *updated_process_info = NULL;

bool gpuinfo_init_info_extraction(uint64_t mask_nvidia, unsigned *devices_count,
                                  gpu_info **devices) {
  unsigned nvidia_devices_count = 0;
  gpuinfo_nvidia_device_handle *nvidia_devices = NULL;
  if (gpuinfo_nvidia_init()) {
    bool retval = gpuinfo_nvidia_get_device_handles(
        &nvidia_devices, &nvidia_devices_count, mask_nvidia);
    if (!retval || (retval && nvidia_devices_count == 0)) {
      gpuinfo_nvidia_shutdown();
      nvidia_devices_count = 0;
      nvidia_devices = NULL;
    }
  }
  unsigned total_devices = nvidia_devices_count;
  *devices = malloc(total_devices * sizeof(**devices));
  if (!*devices) {
    perror("Cannot allocate memory: ");
    free(nvidia_devices);
    return false;
  }
  for (unsigned i = 0; i < nvidia_devices_count; ++i) {
    (*devices)[i].gpu_type = gpuinfo_type_nvidia_proprietary;
    (*devices)[i].nvidia_gpuhandle = nvidia_devices[i];
    (*devices)[i].processes_count = 0;
    (*devices)[i].processes = NULL;
  }
  free(nvidia_devices);
  *devices_count = total_devices;
  return true;
}

bool gpuinfo_shutdown_info_extraction(unsigned device_count,
                                      gpu_info *devices) {
  for (unsigned i = 0; i < device_count; ++i) {
    free(devices[i].processes);
  }
  free(devices);
  gpuinfo_nvidia_shutdown();
  gpuinfo_clear_cache();
  return true;
}

bool gpuinfo_populate_static_infos(unsigned device_count, gpu_info *devices) {
  for (unsigned i = 0; i < device_count; ++i) {
    switch (devices[i].gpu_type) {
    case gpuinfo_type_nvidia_proprietary:
      gpuinfo_nvidia_populate_static_info(devices[i].nvidia_gpuhandle,
                                          &devices[i].static_info);
      break;
    default:
      fprintf(stderr,
              "Unknown GPU type encountered during static initialization\n");
      return false;
    }
  }
  return true;
}

bool gpuinfo_refresh_dynamic_info(unsigned device_count, gpu_info *devices) {
  for (unsigned i = 0; i < device_count; ++i) {
    switch (devices[i].gpu_type) {
    case gpuinfo_type_nvidia_proprietary:
      gpuinfo_nvidia_refresh_dynamic_info(devices[i].nvidia_gpuhandle,
                                          &devices[i].dynamic_info);
      break;
    default:
      fprintf(stderr,
              "Unknown GPU type encountered during static initialization\n");
      return false;
    }
  }
  return true;
}

static void gpuinfo_populate_process_infos(unsigned device_count,
                                           gpu_info *devices) {
  for (unsigned i = 0; i < device_count; ++i) {
    for (unsigned j = 0; j < devices[i].processes_count; ++j) {
      pid_t current_pid = devices[i].processes[j].pid;
      process_info_cache *cached_pid_info;

      HASH_FIND_PID(cached_process_info, &current_pid, cached_pid_info);
      if (!cached_pid_info) {
        // Newly encountered pid
        cached_pid_info = malloc(sizeof(*cached_pid_info));
        cached_pid_info->pid = current_pid;
        get_username_from_pid(current_pid, &cached_pid_info->user_name);
        get_command_from_pid(current_pid, &cached_pid_info->cmdline);
        cached_pid_info->last_total_consumed_cpu_time = -1.;
      } else {
        // Already encountered so delete from cached list to avoid freeing
        // memory at the end of this function
        HASH_DEL(cached_process_info, cached_pid_info);
      }
      HASH_ADD_PID(updated_process_info, cached_pid_info);

      devices[i].processes[j].cmdline = cached_pid_info->cmdline;
      SET_VALID(gpuinfo_process_cmdline_valid, devices[i].processes[j].valid);
      devices[i].processes[j].user_name = cached_pid_info->user_name;
      SET_VALID(gpuinfo_process_user_name_valid, devices[i].processes[j].valid);

      struct process_cpu_usage cpu_usage;
      if (get_process_info(current_pid, &cpu_usage)) {
        if (cached_pid_info->last_total_consumed_cpu_time > -1.) {
          double usage_percent =
              round(100. *
                    (cpu_usage.total_user_time + cpu_usage.total_kernel_time -
                     cached_pid_info->last_total_consumed_cpu_time) /
                    nvtop_difftime(cached_pid_info->last_measurement_timestamp,
                                   cpu_usage.timestamp));
          devices[i].processes[j].cpu_usage = (unsigned)usage_percent;
        } else {
          devices[i].processes[j].cpu_usage = 0;
        }
        SET_VALID(gpuinfo_process_cpu_usage_valid,
                  devices[i].processes[j].valid);
        cached_pid_info->last_measurement_timestamp = cpu_usage.timestamp;
        cached_pid_info->last_total_consumed_cpu_time =
            cpu_usage.total_kernel_time + cpu_usage.total_user_time;
        devices[i].processes[j].cpu_memory_res = cpu_usage.resident_memory;
        SET_VALID(gpuinfo_process_cpu_memory_res_valid,
                  devices[i].processes[j].valid);
        devices[i].processes[j].cpu_memory_virt = cpu_usage.virtual_memory;
        SET_VALID(gpuinfo_process_cpu_memory_virt_valid,
                  devices[i].processes[j].valid);
      } else {
        cached_pid_info->last_total_consumed_cpu_time = -1;
      }

      // Process memory usage percent of total device memory
      if (IS_VALID(gpuinfo_total_memory_valid, devices[i].dynamic_info.valid) &&
          IS_VALID(gpuinfo_process_gpu_memory_usage_valid,
                   devices[i].processes[j].valid)) {
        float percentage =
            roundf(100.f * (float)devices[i].processes[j].gpu_memory_usage /
                   (float)devices[i].dynamic_info.total_memory);
        devices[i].processes[j].gpu_memory_percentage = (unsigned)percentage;
        SET_VALID(gpuinfo_process_gpu_memory_percentage_valid,
                  devices[i].processes[j].valid);
      }
    }
  }
  process_info_cache *pid_not_encountered, *tmp;
  HASH_ITER(hh, cached_process_info, pid_not_encountered, tmp) {
    HASH_DEL(cached_process_info, pid_not_encountered);
    free(pid_not_encountered->cmdline);
    free(pid_not_encountered->user_name);
    free(pid_not_encountered);
  }
  cached_process_info = updated_process_info;
  updated_process_info = NULL;
}

bool gpuinfo_refresh_processes(unsigned device_count, gpu_info *devices) {
  for (unsigned i = 0; i < device_count; ++i) {
    switch (devices[i].gpu_type) {
    case gpuinfo_type_nvidia_proprietary: {
      unsigned processes_count = 0;
      gpu_process *processes = NULL;
      gpuinfo_nvidia_get_running_processes(devices[i].nvidia_gpuhandle,
                                           &processes_count, &processes);
      free(devices[i].processes);
      devices[i].processes = processes;
      devices[i].processes_count = processes_count;
    } break;
    default:
      fprintf(stderr,
              "Unknown GPU type encountered during static initialization\n");
      return false;
    }
  }

  gpuinfo_populate_process_infos(device_count, devices);

  return true;
}

void gpuinfo_clear_cache(void) {
  if (cached_process_info) {
    process_info_cache *pid_cached, *tmp;
    HASH_ITER(hh, cached_process_info, pid_cached, tmp) {
      HASH_DEL(cached_process_info, pid_cached);
      free(pid_cached->cmdline);
      free(pid_cached->user_name);
      free(pid_cached);
    }
  }
}
