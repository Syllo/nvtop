/*
 *
 * Copyright (C) 2026 NVTOP contributors
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

#ifndef EXTRACT_GPUINFO_APPLE_UTILS_H_
#define EXTRACT_GPUINFO_APPLE_UTILS_H_

#include "nvtop/extract_gpuinfo_common.h"

#include <CoreFoundation/CoreFoundation.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

struct gpuinfo_apple_process_sample {
  pid_t pid;
  uint64_t gpu_time;
  // Number of AppUsage entries summed into gpu_time; 0 when the client reports
  // a direct accumulatedGPUTime total, which has no per-queue breakdown. Only
  // meaningful when gpu_time_valid is set.
  unsigned gpu_time_queue_count;
  bool gpu_time_valid;
};

struct gpuinfo_apple_performance_sample {
  unsigned gpu_util_rate;
  // AMD discrete GPUs pin "Device Utilization %" at 0 and report real load
  // under "GPU Activity(%)". Both keys are parsed; the caller prefers the
  // activity reading on non-unified devices.
  unsigned gpu_activity_rate;
  uint64_t allocated_system_memory;
  // Per-GPU stats published for discrete AMD cards via the
  // PerformanceStatistics dictionary. Apple Silicon GPUs do not expose these
  // keys, and each value carries its own *_valid flag because the key set
  // varies between driver generations.
  uint64_t used_memory;     // inUseVidMemoryBytes, or vramUsedBytes on older drivers
  unsigned gpu_temp;        // Temperature(C)
  unsigned power_draw_mw;   // Total Power(W), converted to milliwatts
  unsigned gpu_clock_speed; // Core Clock(MHz)
  unsigned mem_clock_speed; // Memory Clock(MHz)
  unsigned fan_speed;       // Fan Speed(%)
  bool gpu_util_rate_valid;
  bool gpu_activity_rate_valid;
  bool allocated_system_memory_valid;
  bool used_memory_valid;
  bool gpu_temp_valid;
  bool power_draw_valid;
  bool gpu_clock_speed_valid;
  bool mem_clock_speed_valid;
  bool fan_speed_valid;
};

bool gpuinfo_apple_parse_performance_sample(CFDictionaryRef properties,
                                            struct gpuinfo_apple_performance_sample *sample);

bool gpuinfo_apple_parse_process_sample(CFDictionaryRef properties, struct gpuinfo_apple_process_sample *sample);

// Parsing helpers for the PCI metadata macOS publishes in the IORegistry.
// They live here, away from the IOKit plumbing, so they can be unit tested.
bool gpuinfo_apple_parse_pci_bdf(const char *pcidebug, unsigned *bus, unsigned *device, unsigned *function);
bool gpuinfo_apple_decode_pcie_link_status(int64_t status, unsigned *link_gen, unsigned *link_width);
bool gpuinfo_apple_decode_slot_name(const uint8_t *data, size_t data_size, char *slot, size_t slot_size);
bool gpuinfo_apple_parse_die_index(const char *acpi_path, unsigned *die_index);

bool gpuinfo_apple_calculate_gpu_usage(uint64_t previous_gpu_time, uint64_t current_gpu_time, uint64_t time_elapsed,
                                       unsigned *gpu_usage);
// `gpu_time` is AppUsage.accumulatedGPUTime, a nanosecond counter (the same one
// Activity Monitor uses for its per-process % GPU column); `time_elapsed` is
// the nanosecond interval reported by nvtop_difftime_u64. GPU time can exceed
// wall time when a process has several queues in flight, so the result is
// capped at 100%.

bool gpuinfo_apple_energy_to_nanojoules(int64_t energy, const char *unit, uint64_t *energy_nanojoules);

bool gpuinfo_apple_calculate_power_draw(uint64_t energy_nanojoules, uint64_t time_elapsed, unsigned *power_draw);

// Returns true when a new sample should be taken: the first call always samples to
// establish a baseline, afterwards the minimum interval applies. The comparison is
// inclusive, so a refresh that lands exactly on the interval samples again.
bool gpuinfo_apple_sample_is_due(bool sample_attempted, uint64_t nanoseconds_since_last_sample,
                                 uint64_t minimum_interval);

// Adds one channel's active-state residencies (indices 1..frequency_count-1) to the
// multi-die aggregate, skipping the zero-frequency OFF state. A channel is rejected
// as a whole, without touching the aggregate, when any residency is negative or the
// sum would overflow.
bool gpuinfo_apple_accumulate_gpu_residencies(uint64_t *residencies, const int64_t *channel_residencies,
                                              size_t frequency_count);

// Parses a little-endian (frequency, voltage) DVFS table and scales the
// frequencies to MHz from Hz or kHz based on the table magnitude.
bool gpuinfo_apple_parse_gpu_frequency_states(const uint8_t *data, size_t data_size, unsigned *frequencies,
                                              size_t frequencies_size, size_t *frequency_count);

bool gpuinfo_apple_calculate_gpu_clock_speed(const uint64_t *residencies, const unsigned *frequencies,
                                             size_t frequency_count, unsigned *clock_speed);

bool gpuinfo_apple_decode_smc_float(const uint8_t *data, size_t data_size, float *value);

bool gpuinfo_apple_average_temperatures(const float *temperatures, size_t temperature_count,
                                        unsigned *average_temperature);

bool gpuinfo_apple_max_fan_rpm(const float *fan_speeds, size_t fan_count, unsigned *fan_rpm);

void gpuinfo_apple_add_process(struct gpu_info *gpu_info, pid_t pid, bool gpu_usage_valid, unsigned gpu_usage);

#ifdef __cplusplus
}
#endif

#endif // EXTRACT_GPUINFO_APPLE_UTILS_H_
