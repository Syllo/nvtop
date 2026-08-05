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

#include "extract_gpuinfo_apple_utils.h"

#include "nvtop/common.h"

#include <stdio.h>
#include <stdlib.h>

#include <Foundation/Foundation.h>
#include <limits.h>
#include <math.h>
#include <string.h>

static bool gpuinfo_apple_get_unsigned_number(id value, uint64_t *number) {
  if (![value isKindOfClass:[NSNumber class]] || [value longLongValue] < 0)
    return false;

  *number = [value unsignedLongLongValue];
  return true;
}

bool gpuinfo_apple_parse_performance_sample(CFDictionaryRef properties,
                                            struct gpuinfo_apple_performance_sample *sample) {
  memset(sample, 0, sizeof(*sample));
  if (!properties || CFGetTypeID(properties) != CFDictionaryGetTypeID())
    return false;

  NSDictionary *gpu_properties = (__bridge NSDictionary *)properties;
  id performance_statistics = [gpu_properties objectForKey:@"PerformanceStatistics"];
  if (![performance_statistics isKindOfClass:[NSDictionary class]])
    return false;

  uint64_t number;
  if (gpuinfo_apple_get_unsigned_number([performance_statistics objectForKey:@"Device Utilization %"], &number)) {
    sample->gpu_util_rate = number > 100 ? 100 : (unsigned)number;
    sample->gpu_util_rate_valid = true;
  }
  if (gpuinfo_apple_get_unsigned_number([performance_statistics objectForKey:@"Alloc system memory"], &number)) {
    sample->allocated_system_memory = number;
    sample->allocated_system_memory_valid = true;
  }

  return true;
}

bool gpuinfo_apple_parse_process_sample(CFDictionaryRef properties, struct gpuinfo_apple_process_sample *sample) {
  memset(sample, 0, sizeof(*sample));
  if (!properties || CFGetTypeID(properties) != CFDictionaryGetTypeID())
    return false;

  NSDictionary *user_client_info = (__bridge NSDictionary *)properties;
  id client_creator_info = [user_client_info objectForKey:@"IOUserClientCreator"];
  if (![client_creator_info isKindOfClass:[NSString class]])
    return false;

  int pid;
  const char *client_creator = [client_creator_info UTF8String];
  // Client creator is in form: pid <pid>, <name>
  if (!client_creator || sscanf(client_creator, "pid %d,", &pid) != 1 || pid < 0)
    return false;
  sample->pid = pid;

  id app_usage = [user_client_info objectForKey:@"AppUsage"];
  if (![app_usage isKindOfClass:[NSArray class]])
    return true;

  uint64_t total_gpu_time = 0;
  for (id app_info in app_usage) {
    if (![app_info isKindOfClass:[NSDictionary class]])
      continue;

    uint64_t gpu_time;
    if (!gpuinfo_apple_get_unsigned_number([app_info objectForKey:@"accumulatedGPUTime"], &gpu_time))
      continue;

    if (UINT64_MAX - total_gpu_time < gpu_time) {
      sample->gpu_time_valid = false;
      return true;
    }
    total_gpu_time += gpu_time;
    sample->gpu_time_valid = true;
  }

  sample->gpu_time = total_gpu_time;
  return true;
}

bool gpuinfo_apple_calculate_gpu_usage(uint64_t previous_gpu_time, uint64_t current_gpu_time, uint64_t time_elapsed,
                                       unsigned *gpu_usage) {
  if (!time_elapsed || current_gpu_time < previous_gpu_time)
    return false;

  const uint64_t gpu_time_delta = current_gpu_time - previous_gpu_time;
  if (gpu_time_delta >= time_elapsed) {
    *gpu_usage = 100;
  } else {
    *gpu_usage = (unsigned)((gpu_time_delta * UINT64_C(100) + time_elapsed / UINT64_C(2)) / time_elapsed);
  }
  return true;
}

bool gpuinfo_apple_energy_to_nanojoules(int64_t energy, const char *unit, uint64_t *energy_nanojoules) {
  if (energy < 0 || !unit || !energy_nanojoules)
    return false;

  uint64_t multiplier;
  if (strcmp(unit, "nJ") == 0)
    multiplier = UINT64_C(1);
  else if (strcmp(unit, "uJ") == 0)
    multiplier = UINT64_C(1000);
  else if (strcmp(unit, "mJ") == 0)
    multiplier = UINT64_C(1000000);
  else if (strcmp(unit, "J") == 0)
    multiplier = UINT64_C(1000000000);
  else
    return false;

  if ((uint64_t)energy > UINT64_MAX / multiplier)
    return false;

  *energy_nanojoules = (uint64_t)energy * multiplier;
  return true;
}

bool gpuinfo_apple_calculate_power_draw(uint64_t energy_nanojoules, uint64_t time_elapsed, unsigned *power_draw) {
  if (!time_elapsed || !power_draw)
    return false;

  const long double milliwatts =
      (long double)energy_nanojoules * 1000.0L / (long double)time_elapsed;
  if (milliwatts > UINT_MAX)
    return false;

  *power_draw = (unsigned)(milliwatts + 0.5L);
  return true;
}

static uint32_t gpuinfo_apple_read_little_endian_uint32(const uint8_t *data) {
  return (uint32_t)data[0] | (uint32_t)data[1] << 8 | (uint32_t)data[2] << 16 | (uint32_t)data[3] << 24;
}

bool gpuinfo_apple_parse_gpu_frequency_states(const uint8_t *data, size_t data_size, unsigned *frequencies,
                                              size_t frequencies_size, size_t *frequency_count) {
  if (!frequency_count)
    return false;
  *frequency_count = 0;

  if (!data || !frequencies || data_size < 2 * 8 || data_size % 8)
    return false;

  const size_t state_count = data_size / 8;
  if (state_count > frequencies_size)
    return false;

  // pmgr stores little-endian (frequency Hz, voltage) pairs. The first pair is the OFF state;
  // subsequent entries align with the active IOReport performance states.
  for (size_t i = 0; i < state_count; ++i) {
    const uint32_t frequency_hz = gpuinfo_apple_read_little_endian_uint32(&data[i * 8]);
    if ((!i && frequency_hz) || (i && frequency_hz < 1000000))
      return false;
    frequencies[i] = frequency_hz / 1000000;
  }

  *frequency_count = state_count;
  return true;
}

bool gpuinfo_apple_calculate_gpu_clock_speed(const uint64_t *residencies, const unsigned *frequencies,
                                             size_t frequency_count, unsigned *clock_speed) {
  if (!residencies || !frequencies || frequency_count < 2 || frequencies[0] || !clock_speed)
    return false;

  uint64_t active_residency = 0;
  long double weighted_frequency = 0;
  for (size_t i = 1; i < frequency_count; ++i) {
    if (!frequencies[i] || UINT64_MAX - active_residency < residencies[i])
      return false;
    active_residency += residencies[i];
    weighted_frequency += (long double)residencies[i] * frequencies[i];
  }

  if (!active_residency) {
    *clock_speed = 0;
    return true;
  }

  const long double average_frequency = weighted_frequency / active_residency;
  if (average_frequency > UINT_MAX)
    return false;

  *clock_speed = (unsigned)(average_frequency + 0.5L);
  return true;
}

bool gpuinfo_apple_decode_smc_float(const uint8_t *data, size_t data_size, float *value) {
  if (!data || data_size != sizeof(float) || !value)
    return false;

  const uint32_t bits = (uint32_t)data[0] | (uint32_t)data[1] << 8 | (uint32_t)data[2] << 16 |
                        (uint32_t)data[3] << 24;
  memcpy(value, &bits, sizeof(*value));
  return true;
}

bool gpuinfo_apple_average_temperatures(const float *temperatures, size_t temperature_count,
                                        unsigned *average_temperature) {
  if (!temperatures || !temperature_count || !average_temperature)
    return false;

  long double sum = 0;
  size_t valid_count = 0;
  for (size_t i = 0; i < temperature_count; ++i) {
    if (!isfinite(temperatures[i]) || temperatures[i] <= 0 || temperatures[i] > 150)
      continue;
    sum += (long double)temperatures[i];
    ++valid_count;
  }
  if (!valid_count)
    return false;

  *average_temperature = (unsigned)(sum / valid_count + 0.5L);
  return true;
}

void gpuinfo_apple_add_process(struct gpu_info *gpu_info, pid_t pid, bool gpu_usage_valid, unsigned gpu_usage) {
  struct gpu_process *process = NULL;
  for (unsigned i = 0; i < gpu_info->processes_count; ++i) {
    if (gpu_info->processes[i].pid == pid) {
      process = &gpu_info->processes[i];
      break;
    }
  }

  if (!process) {
    if (gpu_info->processes_array_size < gpu_info->processes_count + 1) {
      gpu_info->processes_array_size += COMMON_PROCESS_LINEAR_REALLOC_INC;
      gpu_info->processes =
          reallocarray(gpu_info->processes, gpu_info->processes_array_size, sizeof(*gpu_info->processes));
      if (!gpu_info->processes) {
        perror("Could not allocate memory: ");
        exit(EXIT_FAILURE);
      }
    }

    process = &gpu_info->processes[gpu_info->processes_count++];
    RESET_ALL(process->valid);
    process->pid = pid;
    process->type = gpu_process_graphical_compute;
  }

  if (gpu_usage_valid) {
    if (GPUINFO_PROCESS_FIELD_VALID(process, gpu_usage)) {
      const unsigned total_usage = process->gpu_usage + gpu_usage;
      process->gpu_usage = total_usage > 100 ? 100 : total_usage;
    } else {
      SET_GPUINFO_PROCESS(process, gpu_usage, gpu_usage);
    }
  }
}
