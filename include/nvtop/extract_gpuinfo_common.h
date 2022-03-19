/*
 *
 * Copyright (C) 2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#ifndef EXTRACT_GPUINFO_COMMON_H__
#define EXTRACT_GPUINFO_COMMON_H__

#include <limits.h>
#include <stdlib.h>
#include <sys/types.h>

#include "list.h"

#define IS_VALID(x, y) ((y)[(x) / CHAR_BIT] & (1 << ((x) % CHAR_BIT)))
#define SET_VALID(x, y) ((y)[(x) / CHAR_BIT] |= (1 << ((x) % CHAR_BIT)))
#define RESET_VALID(x, y) ((y)[(x) / CHAR_BIT] &= ~(1 << ((x) % CHAR_BIT)))

enum gpuinfo_static_info_valid {
  gpuinfo_device_name_valid = 0,
  gpuinfo_max_pcie_gen_valid,
  gpuinfo_max_link_width_valid,
  gpuinfo_temperature_shutdown_valid,
  gpuinfo_temperature_slowdown_valid,
  gpuinfo_static_info_count,
};

#define MAX_DEVICE_NAME 128

struct gpuinfo_static_info {
  char device_name[MAX_DEVICE_NAME];
  unsigned max_pcie_gen;
  unsigned max_pcie_link_width;
  unsigned temperature_shutdown_threshold;
  unsigned temperature_slowdown_threshold;
  unsigned char valid[gpuinfo_static_info_count / CHAR_BIT + 1];
};

enum gpuinfo_dynamic_info_valid {
  gpuinfo_curr_gpu_clock_speed_valid = 0,
  gpuinfo_max_gpu_clock_speed_valid,
  gpuinfo_curr_mem_clock_speed_valid,
  gpuinfo_max_mem_clock_speed_valid,
  gpuinfo_gpu_util_rate_valid,
  gpuinfo_mem_util_rate_valid,
  gpuinfo_encoder_rate_valid,
  gpuinfo_decoder_rate_valid,
  gpuinfo_total_memory_valid,
  gpuinfo_free_memory_valid,
  gpuinfo_used_memory_valid,
  gpuinfo_pcie_link_gen_valid,
  gpuinfo_pcie_link_width_valid,
  gpuinfo_pcie_rx_valid,
  gpuinfo_pcie_tx_valid,
  gpuinfo_fan_speed_valid,
  gpuinfo_gpu_temp_valid,
  gpuinfo_power_draw_valid,
  gpuinfo_power_draw_max_valid,
  gpuinfo_dynamic_info_count,
};

struct gpuinfo_dynamic_info {
  unsigned int gpu_clock_speed;      // Device clock speed in MHz
  unsigned int gpu_clock_speed_max;  // Maximum clock speed in MHz
  unsigned int mem_clock_speed;      // Device clock speed in MHz
  unsigned int mem_clock_speed_max;  // Maximum clock speed in MHz
  unsigned int gpu_util_rate;        // GPU utilization rate in %
  unsigned int mem_util_rate;        // MEM utilization rate in %
  unsigned int encoder_rate;         // Encoder utilization rate in %
  unsigned int decoder_rate;         // Decoder utilization rate in %
  unsigned long long total_memory;   // Total memory (bytes)
  unsigned long long free_memory;    // Unallocated memory (bytes)
  unsigned long long used_memory;    // Allocated memory (bytes)
  unsigned int curr_pcie_link_gen;   // PCIe link generation used
  unsigned int curr_pcie_link_width; // PCIe line width used
  unsigned int pcie_rx;              // PCIe throughput in KB/s
  unsigned int pcie_tx;              // PCIe throughput in KB/s
  unsigned int fan_speed;            // Fan speed percentage
  unsigned int gpu_temp;             // GPU temperature °celsius
  unsigned int power_draw;           // Power usage in milliwatts
  unsigned int power_draw_max;       // Max power usage in milliwatts
  unsigned char valid[gpuinfo_dynamic_info_count / CHAR_BIT + 1];
};

enum gpu_process_type {
  gpu_process_graphical,
  gpu_process_compute,
  gpu_process_type_count,
};

enum gpuinfo_process_info_valid {
  gpuinfo_process_cmdline_valid,
  gpuinfo_process_user_name_valid,
  gpuinfo_process_gpu_usage_valid,
  gpuinfo_process_gpu_encoder_valid,
  gpuinfo_process_gpu_decoder_valid,
  gpuinfo_process_gpu_memory_usage_valid,
  gpuinfo_process_gpu_memory_percentage_valid,
  gpuinfo_process_cpu_usage_valid,
  gpuinfo_process_cpu_memory_virt_valid,
  gpuinfo_process_cpu_memory_res_valid,
  gpuinfo_process_info_count
};

struct gpu_process {
  enum gpu_process_type type;
  pid_t pid;                           // Process ID
  char *cmdline;                       // Process User Name
  char *user_name;                     // Process User Name
  unsigned gpu_usage;                  // Percentage of GPU used by the process
  unsigned encode_usage;               // Percentage of GPU encoder used by the process
  unsigned decode_usage;               // Percentage of GPU decoder used by the process
  unsigned long long gpu_memory_usage; // Memory used by the process
  unsigned gpu_memory_percentage;      // Percentage of the total device memory
                                       // consumed by the process
  unsigned cpu_usage;
  unsigned long cpu_memory_virt;
  unsigned long cpu_memory_res;
  unsigned char valid[gpuinfo_process_info_count / CHAR_BIT + 1];
};

enum gpuinfo_gputype {
  gpuinfo_type_nvidia_proprietary,
};

struct gpu_info {
  struct list_head list;
  enum gpuinfo_gputype gpu_type;
  struct gpuinfo_static_info static_info;
  struct gpuinfo_dynamic_info dynamic_info;
  unsigned processes_count;
  struct gpu_process *processes;
};

#endif // EXTRACT_GPUINFO_COMMON_H__
