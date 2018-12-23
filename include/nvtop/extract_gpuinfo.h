/*
 *
 * Copyright (C) 2017 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#ifndef EXTRACT_GPUINFO_H_
#define EXTRACT_GPUINFO_H_

#include <limits.h>
#include <nvml.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define IS_VALID(x,y)    ((y)[(x)/CHAR_BIT] & (1<<((x)%CHAR_BIT)))
#define SET_VALID(x,y)   ((y)[(x)/CHAR_BIT] |= (1<<((x)%CHAR_BIT)))
#define RESET_VALID(x,y) ((y)[(x)/CHAR_BIT] &= ~(1<<((x)%CHAR_BIT)))

struct gpu_process {
  intmax_t pid;                    // Process ID
  char *process_name;              // Process Name
  char *user_name;                 // Process User Name
  unsigned long long used_memory;  // Memory used by process
  size_t cpu_memory_virt;
  size_t cpu_memory_res;
  double cpu_usage;
};

enum dev_info_valid {
  device_name_valid = 0,
  gpu_clock_speed_valid,
  gpu_clock_speed_max_valid,
  mem_clock_speed_valid,
  mem_clock_speed_max_valid,
  gpu_util_rate_valid,
  mem_util_rate_valid,
  encoder_rate_valid,
  encoder_sampling_valid,
  decoder_rate_valid,
  decoder_sampling_valid,
  free_memory_valid,
  total_memory_valid,
  used_memory_valid,
  cur_pcie_link_gen_valid,
  max_pcie_link_gen_valid,
  cur_pcie_link_width_valid,
  max_pcie_link_width_valid,
  pcie_rx_valid,
  pcie_tx_valid,
  fan_speed_valid,
  gpu_temp_valid,
  gpu_temp_slowdown_valid,
  gpu_temp_shutdown_valid,
  power_draw_valid,
  power_draw_max_valid,
  valid_max_val,
};

struct device_info {
  nvmlDevice_t device_handle;        // Used to query device
  char device_name[NVML_DEVICE_NAME_BUFFER_SIZE]; // Device Name
  unsigned int gpu_clock_speed;      // Device clock speed in MHz
  unsigned int gpu_clock_speed_max;  // Maximum clock speed in MHz
  unsigned int mem_clock_speed;      // Device clock speed in MHz
  unsigned int mem_clock_speed_max;  // Maximum clock speed in MHz
  unsigned int gpu_util_rate;        // GPU utilization rate in %
  unsigned int mem_util_rate;        // MEM utilization rate in %
  unsigned int encoder_rate;         // Encoder utilization rate in %
  unsigned int encoder_sampling;     // Encoder sampling period in micro sec
  unsigned int decoder_rate;         // Decoder utilization rate in %
  unsigned int decoder_sampling;     // Decoder sampling period in micro sec
  unsigned long long free_memory;    // Unallocated memory (bytes)
  unsigned long long total_memory;   // Total memory (bytes)
  unsigned long long used_memory;    // Allocated memory (bytes)
  unsigned int cur_pcie_link_gen;    // PCIe link generation used
  unsigned int max_pcie_link_gen;    // PCIe link generation max
  unsigned int cur_pcie_link_width;  // PCIe line width used
  unsigned int max_pcie_link_width;  // PCIe line width max
  unsigned int pcie_rx;              // PCIe throughput in KB/s
  unsigned int pcie_tx;              // PCIe throughput in KB/s
  unsigned int fan_speed;            // Fan speed percentage
  unsigned int gpu_temp;             // GPU temperature °c
  unsigned int gpu_temp_slowdown;    // GPU temperature °c
  unsigned int gpu_temp_shutdown;    // GPU temperature °c
  unsigned int power_draw;           // Power usage in milliwatts
  unsigned int power_draw_max;       // Max power usage in milliwatts
  unsigned int size_proc_buffers;    // Number of Compute processes (Cuda)
  unsigned int num_compute_procs;    // Number of Compute processes (Cuda)
  unsigned int num_graphical_procs;  // Number of Graphical processes
  struct gpu_process *graphic_procs; // Graphical process info
  struct gpu_process *compute_procs; // Compute processes info
  nvmlProcessInfo_t *process_infos;  // Internal use
  unsigned char valid[valid_max_val/CHAR_BIT + 1];             // Validity bits
};

bool init_gpu_info_extraction(void);

bool shutdown_gpu_info_extraction(void);

unsigned int initialize_device_info(
    struct device_info **dev_info,
    size_t gpu_mask);

void update_device_infos(
    unsigned int num_devs,
    struct device_info *dev_info);

void clean_device_info(unsigned int num_devs, struct device_info *dev_info);

void clean_pid_cache(void);

#endif // EXTRACT_GPUINFO_H_
