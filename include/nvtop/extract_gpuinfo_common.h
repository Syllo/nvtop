/*
 *
 * Copyright (C) 2021-2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
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
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/types.h>

#include "list.h"

#define STRINGIFY(x) STRINGIFY_HELPER_(x)
#define STRINGIFY_HELPER_(x) #x

#define IS_VALID(x, y) ((y)[(x) / CHAR_BIT] & (1 << ((x) % CHAR_BIT)))
#define SET_VALID(x, y) ((y)[(x) / CHAR_BIT] |= (1 << ((x) % CHAR_BIT)))
#define RESET_VALID(x, y) ((y)[(x) / CHAR_BIT] &= ~(1 << ((x) % CHAR_BIT)))
#define RESET_ALL(y) memset(y, 0, sizeof(y))

#define SET_VALUE(structPtr, field, value, prefix)                                                                     \
  do {                                                                                                                 \
    (structPtr)->field = (value);                                                                                      \
    SET_VALID(prefix##field##_valid, (structPtr)->valid);                                                              \
  } while (0)
#define INVALIDATE_VALUE(structPtr, field, prefix)                                                                     \
  do {                                                                                                                 \
    RESET_VALID(prefix##field##_valid, (structPtr)->valid);                                                            \
  } while (0)
#define VALUE_IS_VALID(structPtr, field, prefix) IS_VALID(prefix##field##_valid, (structPtr)->valid)

#define SET_GPUINFO_STATIC(structPtr, field, value) SET_VALUE(structPtr, field, value, gpuinfo_)
#define RESET_GPUINFO_STATIC(structPtr, field) INVALIDATE_VALUE(structPtr, field, gpuinfo_)
#define GPUINFO_STATIC_FIELD_VALID(structPtr, field) VALUE_IS_VALID(structPtr, field, gpuinfo_)
enum gpuinfo_static_info_valid {
  gpuinfo_device_name_valid = 0,
  gpuinfo_max_pcie_gen_valid,
  gpuinfo_max_pcie_link_width_valid,
  gpuinfo_temperature_shutdown_threshold_valid,
  gpuinfo_temperature_slowdown_threshold_valid,
  gpuinfo_n_shared_cores_valid,
  gpuinfo_l2cache_size_valid,
  gpuinfo_n_exec_engines_valid,
  gpuinfo_static_info_count,
};

#define MAX_DEVICE_NAME 128

struct gpuinfo_static_info {
  char device_name[MAX_DEVICE_NAME];
  unsigned max_pcie_gen;
  unsigned max_pcie_link_width;
  unsigned temperature_shutdown_threshold;
  unsigned temperature_slowdown_threshold;
  unsigned n_shared_cores;
  unsigned l2cache_size;
  unsigned n_exec_engines;
  bool integrated_graphics;
  bool encode_decode_shared;
  unsigned char valid[(gpuinfo_static_info_count + CHAR_BIT - 1) / CHAR_BIT];
};

#define SET_GPUINFO_DYNAMIC(structPtr, field, value) SET_VALUE(structPtr, field, value, gpuinfo_)
#define RESET_GPUINFO_DYNAMIC(structPtr, field) INVALIDATE_VALUE(structPtr, field, gpuinfo_)
#define GPUINFO_DYNAMIC_FIELD_VALID(structPtr, field) VALUE_IS_VALID(structPtr, field, gpuinfo_)
enum gpuinfo_dynamic_info_valid {
  gpuinfo_gpu_clock_speed_valid = 0,
  gpuinfo_gpu_clock_speed_max_valid,
  gpuinfo_mem_clock_speed_valid,
  gpuinfo_mem_clock_speed_max_valid,
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
  gpuinfo_multi_instance_mode_valid,
  gpuinfo_dynamic_info_count,
};

struct gpuinfo_dynamic_info {
  unsigned int gpu_clock_speed;     // Device clock speed in MHz
  unsigned int gpu_clock_speed_max; // Maximum clock speed in MHz
  unsigned int mem_clock_speed;     // Device clock speed in MHz
  unsigned int mem_clock_speed_max; // Maximum clock speed in MHz
  unsigned int gpu_util_rate;       // GPU utilization rate in %
  unsigned int mem_util_rate;       // MEM utilization rate in %
  unsigned int encoder_rate;        // Encoder utilization rate in %
  unsigned int decoder_rate;        // Decoder utilization rate in %
  unsigned long long total_memory;  // Total memory (bytes)
  unsigned long long free_memory;   // Unallocated memory (bytes)
  unsigned long long used_memory;   // Allocated memory (bytes)
  unsigned int pcie_link_gen;       // PCIe link generation used
  unsigned int pcie_link_width;     // PCIe line width used
  unsigned int pcie_rx;             // PCIe throughput in KB/s
  unsigned int pcie_tx;             // PCIe throughput in KB/s
  unsigned int fan_speed;           // Fan speed percentage
  unsigned int gpu_temp;            // GPU temperature °celsius
  unsigned int power_draw;          // Power usage in milliwatts
  unsigned int power_draw_max;      // Max power usage in milliwatts
  bool multi_instance_mode;          // True if the GPU is in multi-instance mode
  unsigned char valid[(gpuinfo_dynamic_info_count + CHAR_BIT - 1) / CHAR_BIT];
};

enum gpu_process_type {
  gpu_process_unknown = 0,
  gpu_process_graphical = 1,
  gpu_process_compute = 2,
  gpu_process_graphical_compute = 3,
  gpu_process_type_count,
};

#define SET_GPUINFO_PROCESS(structPtr, field, value) SET_VALUE(structPtr, field, value, gpuinfo_process_)
#define RESET_GPUINFO_PROCESS(structPtr, field) INVALIDATE_VALUE(structPtr, field, gpuinfo_process_)
#define GPUINFO_PROCESS_FIELD_VALID(structPtr, field) VALUE_IS_VALID(structPtr, field, gpuinfo_process_)
enum gpuinfo_process_info_valid {
  gpuinfo_process_cmdline_valid,
  gpuinfo_process_user_name_valid,
  gpuinfo_process_gfx_engine_used_valid,
  gpuinfo_process_compute_engine_used_valid,
  gpuinfo_process_enc_engine_used_valid,
  gpuinfo_process_dec_engine_used_valid,
  gpuinfo_process_gpu_usage_valid,
  gpuinfo_process_encode_usage_valid,
  gpuinfo_process_decode_usage_valid,
  gpuinfo_process_gpu_memory_usage_valid,
  gpuinfo_process_gpu_memory_percentage_valid,
  gpuinfo_process_cpu_usage_valid,
  gpuinfo_process_cpu_memory_virt_valid,
  gpuinfo_process_cpu_memory_res_valid,
  gpuinfo_process_gpu_cycles_valid,
  gpuinfo_process_sample_delta_valid,
  gpuinfo_process_info_count
};

struct gpu_process {
  enum gpu_process_type type;
  pid_t pid;                           // Process ID
  char *cmdline;                       // Process User Name
  char *user_name;                     // Process User Name
  uint64_t sample_delta;               // Time spent between two successive samples
  uint64_t gfx_engine_used;            // Time in nanoseconds this process spent using the GPU gfx
  uint64_t compute_engine_used;        // Time in nanoseconds this process spent using the GPU compute
  uint64_t enc_engine_used;            // Time in nanoseconds this process spent using the GPU encoder
  uint64_t dec_engine_used;            // Time in nanoseconds this process spent using the GPU decoder
  uint64_t gpu_cycles;                 // Number of GPU cycles spent in the GPU gfx engine
  unsigned gpu_usage;                  // Percentage of GPU used by the process
  unsigned encode_usage;               // Percentage of GPU encoder used by the process
  unsigned decode_usage;               // Percentage of GPU decoder used by the process
  unsigned long long gpu_memory_usage; // Memory used by the process
  unsigned gpu_memory_percentage;      // Percentage of the total device memory
                                       // consumed by the process
  unsigned cpu_usage;
  unsigned long cpu_memory_virt;
  unsigned long cpu_memory_res;
  unsigned char valid[(gpuinfo_process_info_count + CHAR_BIT - 1) / CHAR_BIT];
};

struct gpu_info;

struct gpu_vendor {
  struct list_head list;

  bool (*init)(void);
  void (*shutdown)(void);

  const char *(*last_error_string)(void);

  bool (*get_device_handles)(struct list_head *devices, unsigned *count);

  void (*populate_static_info)(struct gpu_info *gpu_info);
  void (*refresh_dynamic_info)(struct gpu_info *gpu_info);
  void (*refresh_utilisation_rate)(struct gpu_info *gpu_info);

  void (*refresh_running_processes)(struct gpu_info *gpu_info);
  char *name;
};

#define PDEV_LEN 16
struct gpu_info {
  struct list_head list;
  struct gpu_vendor *vendor;
  struct gpuinfo_static_info static_info;
  struct gpuinfo_dynamic_info dynamic_info;
  unsigned processes_count;
  struct gpu_process *processes;
  unsigned processes_array_size;
  char pdev[PDEV_LEN];
};

void register_gpu_vendor(struct gpu_vendor *vendor);

bool extract_drm_fdinfo_key_value(char *buf, char **key, char **val);

void gpuinfo_refresh_utilisation_rate(struct gpu_info *gpu_info);

// fdinfo DRM interface names common to multiple drivers
extern const char drm_pdev[];
extern const char drm_client_id[];

inline unsigned busy_usage_from_time_usage_round(uint64_t current_use_ns, uint64_t previous_use_ns,
                                                 uint64_t time_between_measurement) {
  return ((current_use_ns - previous_use_ns) * UINT64_C(100) + time_between_measurement / UINT64_C(2)) /
         time_between_measurement;
}

unsigned nvtop_pcie_gen_from_link_speed(unsigned linkSpeed);

#endif // EXTRACT_GPUINFO_COMMON_H__
