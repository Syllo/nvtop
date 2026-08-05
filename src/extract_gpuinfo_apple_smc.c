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

#include "extract_gpuinfo_apple_smc.h"

#include "extract_gpuinfo_apple_utils.h"
#include "nvtop/time.h"

#include <IOKit/IOKitLib.h>
#include <mach/mach.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// AppleSMC has no public user-space protocol. Keep its wire structures and commands isolated
// here so changes make SMC metrics unavailable without affecting the other Apple metrics.
struct apple_smc_version {
  uint8_t major;
  uint8_t minor;
  uint8_t build;
  uint8_t reserved;
  uint16_t release;
};

struct apple_smc_power_limit {
  uint16_t version;
  uint16_t length;
  uint32_t cpu_power_limit;
  uint32_t gpu_power_limit;
  uint32_t memory_power_limit;
};

struct apple_smc_key_info {
  uint32_t data_size;
  uint32_t data_type;
  uint8_t data_attributes;
};

struct apple_smc_key_data {
  uint32_t key;
  struct apple_smc_version version;
  struct apple_smc_power_limit power_limit;
  struct apple_smc_key_info key_info;
  uint8_t result;
  uint8_t status;
  uint8_t data8;
  uint32_t data32;
  uint8_t bytes[32];
};

_Static_assert(sizeof(struct apple_smc_version) == 6, "Unexpected AppleSMC version layout");
_Static_assert(sizeof(struct apple_smc_power_limit) == 16, "Unexpected AppleSMC power limit layout");
_Static_assert(sizeof(struct apple_smc_key_info) == 12, "Unexpected AppleSMC key info layout");
_Static_assert(offsetof(struct apple_smc_key_data, bytes) == 48, "Unexpected AppleSMC payload layout");
_Static_assert(sizeof(struct apple_smc_key_data) == 80, "Unexpected AppleSMC payload size");
_Static_assert(sizeof(float) == 4, "Unexpected AppleSMC float size");

struct apple_smc_sensor_key {
  uint32_t key;
  struct apple_smc_key_info info;
};

struct gpuinfo_apple_smc {
  io_connect_t connection;
  struct apple_smc_sensor_key *temperature_keys;
  size_t temperature_key_count;
  float *temperature_samples;
  struct apple_smc_sensor_key *fan_keys;
  size_t fan_key_count;
  float *fan_samples;
  nvtop_time last_temperature_sample_time;
  nvtop_time last_fan_sample_time;
  unsigned last_temperature;
  unsigned last_fan_rpm;
  bool temperature_sample_attempted;
  bool fan_sample_attempted;
  bool last_temperature_valid;
  bool last_fan_rpm_valid;
};

enum apple_smc_command {
  apple_smc_command_read_bytes = 5,
  apple_smc_command_read_key_by_index = 8,
  apple_smc_command_read_key_info = 9,
};

static const unsigned apple_smc_user_client_method = 2;
static const uint64_t apple_smc_minimum_sample_interval = UINT64_C(1000000000);

static uint32_t gpuinfo_apple_smc_key_id(const char name[4]) {
  return (uint32_t)(uint8_t)name[0] << 24 | (uint32_t)(uint8_t)name[1] << 16 |
         (uint32_t)(uint8_t)name[2] << 8 | (uint32_t)(uint8_t)name[3];
}

static bool gpuinfo_apple_smc_call(struct gpuinfo_apple_smc *smc, const struct apple_smc_key_data *input,
                                   struct apple_smc_key_data *output) {
  size_t output_size = sizeof(*output);
  memset(output, 0, sizeof(*output));
  return IOConnectCallStructMethod(smc->connection, apple_smc_user_client_method, input, sizeof(*input), output,
                                   &output_size) == kIOReturnSuccess &&
         output_size == sizeof(*output) && !output->result;
}

static bool gpuinfo_apple_smc_read_key_info(struct gpuinfo_apple_smc *smc, uint32_t key,
                                            struct apple_smc_key_info *info) {
  const struct apple_smc_key_data input = {.key = key, .data8 = apple_smc_command_read_key_info};
  struct apple_smc_key_data output;
  if (!gpuinfo_apple_smc_call(smc, &input, &output))
    return false;

  *info = output.key_info;
  return true;
}

static bool gpuinfo_apple_smc_read_key(struct gpuinfo_apple_smc *smc, uint32_t key,
                                      const struct apple_smc_key_info *info, uint8_t output_bytes[32]) {
  const struct apple_smc_key_data input = {
      .key = key, .key_info = *info, .data8 = apple_smc_command_read_bytes};
  struct apple_smc_key_data output;
  if (!gpuinfo_apple_smc_call(smc, &input, &output))
    return false;

  memcpy(output_bytes, output.bytes, sizeof(output.bytes));
  return true;
}

static bool gpuinfo_apple_smc_read_key_by_index(struct gpuinfo_apple_smc *smc, uint32_t index, uint32_t *key) {
  const struct apple_smc_key_data input = {
      .data8 = apple_smc_command_read_key_by_index, .data32 = index};
  struct apple_smc_key_data output;
  if (!gpuinfo_apple_smc_call(smc, &input, &output))
    return false;

  *key = output.key;
  return true;
}

static bool gpuinfo_apple_smc_read_key_count(struct gpuinfo_apple_smc *smc, uint32_t *key_count) {
  const uint32_t count_key = gpuinfo_apple_smc_key_id("#KEY");
  struct apple_smc_key_info info;
  uint8_t bytes[32];
  if (!gpuinfo_apple_smc_read_key_info(smc, count_key, &info) || info.data_size < 4 ||
      !gpuinfo_apple_smc_read_key(smc, count_key, &info, bytes))
    return false;

  *key_count = (uint32_t)bytes[0] << 24 | (uint32_t)bytes[1] << 16 | (uint32_t)bytes[2] << 8 | bytes[3];
  return *key_count > 0;
}

static bool gpuinfo_apple_smc_lower_bound(struct gpuinfo_apple_smc *smc, uint32_t key_count, uint32_t target,
                                          uint32_t *lower_bound) {
  uint32_t first = 0;
  uint32_t count = key_count;
  while (first < count) {
    const uint32_t middle = first + (count - first) / 2;
    uint32_t key;
    if (!gpuinfo_apple_smc_read_key_by_index(smc, middle, &key))
      return false;
    if (key < target)
      first = middle + 1;
    else
      count = middle;
  }

  *lower_bound = first;
  return true;
}

static bool gpuinfo_apple_smc_open(struct gpuinfo_apple_smc *smc) {
  CFMutableDictionaryRef matching_services = IOServiceMatching("AppleSMC");
  if (!matching_services)
    return false;

  io_iterator_t services;
  if (IOServiceGetMatchingServices(kIOMainPortDefault, matching_services, &services) != kIOReturnSuccess)
    return false;

  for (io_service_t service = IOIteratorNext(services); service; service = IOIteratorNext(services)) {
    io_name_t name;
    if (IORegistryEntryGetName(service, name) == kIOReturnSuccess &&
        strcmp(name, "AppleSMCKeysEndpoint") == 0 &&
        IOServiceOpen(service, mach_task_self(), 0, &smc->connection) == kIOReturnSuccess) {
      IOObjectRelease(service);
      break;
    }
    IOObjectRelease(service);
  }
  IOObjectRelease(services);
  return MACH_PORT_VALID(smc->connection);
}

static bool gpuinfo_apple_smc_discover_temperature_keys(struct gpuinfo_apple_smc *smc, uint32_t key_count) {
  // SMC enumerates keys in lexicographic FourCC order. Locate only the lowercase Tg range
  // instead of scanning thousands of keys, then validate that ordering while caching the range.
  const uint32_t first_gpu_temperature_key = gpuinfo_apple_smc_key_id("Tg\0\0");
  const uint32_t end_gpu_temperature_key = gpuinfo_apple_smc_key_id("Th\0\0");
  uint32_t first_index, end_index;
  if (!gpuinfo_apple_smc_lower_bound(smc, key_count, first_gpu_temperature_key, &first_index) ||
      !gpuinfo_apple_smc_lower_bound(smc, key_count, end_gpu_temperature_key, &end_index) ||
      first_index >= end_index)
    return false;

  const size_t maximum_key_count = end_index - first_index;
  struct apple_smc_sensor_key *keys = calloc(maximum_key_count, sizeof(*keys));
  if (!keys)
    return false;

  uint32_t previous_key = 0;
  size_t valid_key_count = 0;
  const uint32_t float_type = gpuinfo_apple_smc_key_id("flt ");
  for (uint32_t index = first_index; index < end_index; ++index) {
    uint32_t key;
    if (!gpuinfo_apple_smc_read_key_by_index(smc, index, &key) || key < first_gpu_temperature_key ||
        key >= end_gpu_temperature_key || (index != first_index && key <= previous_key)) {
      free(keys);
      return false;
    }
    previous_key = key;

    struct apple_smc_key_info info;
    if (!gpuinfo_apple_smc_read_key_info(smc, key, &info) || info.data_size != sizeof(float) ||
        info.data_type != float_type)
      continue;
    keys[valid_key_count++] = (struct apple_smc_sensor_key){.key = key, .info = info};
  }

  if (!valid_key_count) {
    free(keys);
    return false;
  }

  float *samples = calloc(valid_key_count, sizeof(*samples));
  if (!samples) {
    free(keys);
    return false;
  }

  smc->temperature_keys = keys;
  smc->temperature_key_count = valid_key_count;
  smc->temperature_samples = samples;
  return true;
}

static bool gpuinfo_apple_smc_discover_fan_keys(struct gpuinfo_apple_smc *smc, uint32_t key_count) {
  // Current fan speeds use F?Ac keys. Search the complete F range, then retain only
  // matching float sensors so unrelated fan limits and metadata are ignored.
  const uint32_t first_fan_key = gpuinfo_apple_smc_key_id("F\0\0\0");
  const uint32_t end_fan_key = gpuinfo_apple_smc_key_id("G\0\0\0");
  uint32_t first_index, end_index;
  if (!gpuinfo_apple_smc_lower_bound(smc, key_count, first_fan_key, &first_index) ||
      !gpuinfo_apple_smc_lower_bound(smc, key_count, end_fan_key, &end_index) ||
      first_index >= end_index)
    return false;

  const size_t maximum_key_count = end_index - first_index;
  struct apple_smc_sensor_key *keys = calloc(maximum_key_count, sizeof(*keys));
  if (!keys)
    return false;

  uint32_t previous_key = 0;
  size_t valid_key_count = 0;
  const uint32_t fan_key_pattern = gpuinfo_apple_smc_key_id("F\0Ac");
  const uint32_t fan_key_mask = UINT32_C(0xff00ffff);
  const uint32_t float_type = gpuinfo_apple_smc_key_id("flt ");
  for (uint32_t index = first_index; index < end_index; ++index) {
    uint32_t key;
    if (!gpuinfo_apple_smc_read_key_by_index(smc, index, &key) || key < first_fan_key ||
        key >= end_fan_key || (index != first_index && key <= previous_key)) {
      free(keys);
      return false;
    }
    previous_key = key;

    struct apple_smc_key_info info;
    if ((key & fan_key_mask) != fan_key_pattern || !gpuinfo_apple_smc_read_key_info(smc, key, &info) ||
        info.data_size != sizeof(float) || info.data_type != float_type)
      continue;
    keys[valid_key_count++] = (struct apple_smc_sensor_key){.key = key, .info = info};
  }

  if (!valid_key_count) {
    free(keys);
    return false;
  }

  float *samples = calloc(valid_key_count, sizeof(*samples));
  if (!samples) {
    free(keys);
    return false;
  }

  smc->fan_keys = keys;
  smc->fan_key_count = valid_key_count;
  smc->fan_samples = samples;
  return true;
}

bool gpuinfo_apple_smc_init(struct gpuinfo_apple_smc **smc) {
  if (!smc)
    return false;
  *smc = NULL;

  struct gpuinfo_apple_smc *new_smc = calloc(1, sizeof(*new_smc));
  if (!new_smc)
    return false;

  uint32_t key_count;
  if (!gpuinfo_apple_smc_open(new_smc) || !gpuinfo_apple_smc_read_key_count(new_smc, &key_count)) {
    gpuinfo_apple_smc_shutdown(new_smc);
    return false;
  }

  const bool temperature_keys_valid = gpuinfo_apple_smc_discover_temperature_keys(new_smc, key_count);
  const bool fan_keys_valid = gpuinfo_apple_smc_discover_fan_keys(new_smc, key_count);
  if (!temperature_keys_valid && !fan_keys_valid) {
    gpuinfo_apple_smc_shutdown(new_smc);
    return false;
  }

  *smc = new_smc;
  return true;
}

void gpuinfo_apple_smc_shutdown(struct gpuinfo_apple_smc *smc) {
  if (!smc)
    return;

  if (MACH_PORT_VALID(smc->connection))
    IOServiceClose(smc->connection);
  free(smc->fan_samples);
  free(smc->fan_keys);
  free(smc->temperature_samples);
  free(smc->temperature_keys);
  free(smc);
}

bool gpuinfo_apple_smc_get_gpu_temperature(struct gpuinfo_apple_smc *smc, unsigned *temperature) {
  if (!smc || !temperature || !smc->temperature_key_count)
    return false;

  nvtop_time current_time;
  nvtop_get_current_time(&current_time);
  if (smc->temperature_sample_attempted &&
      nvtop_difftime_u64(smc->last_temperature_sample_time, current_time) < apple_smc_minimum_sample_interval) {
    if (smc->last_temperature_valid)
      *temperature = smc->last_temperature;
    return smc->last_temperature_valid;
  }
  smc->last_temperature_sample_time = current_time;
  smc->temperature_sample_attempted = true;

  size_t sample_count = 0;
  for (size_t i = 0; i < smc->temperature_key_count; ++i) {
    uint8_t bytes[32];
    float sample;
    if (gpuinfo_apple_smc_read_key(smc, smc->temperature_keys[i].key, &smc->temperature_keys[i].info, bytes) &&
        gpuinfo_apple_decode_smc_float(bytes, smc->temperature_keys[i].info.data_size, &sample))
      smc->temperature_samples[sample_count++] = sample;
  }

  unsigned average_temperature;
  if (!gpuinfo_apple_average_temperatures(smc->temperature_samples, sample_count, &average_temperature)) {
    smc->last_temperature_valid = false;
    return false;
  }

  smc->last_temperature = average_temperature;
  smc->last_temperature_valid = true;
  *temperature = average_temperature;
  return true;
}

bool gpuinfo_apple_smc_get_fan_rpm(struct gpuinfo_apple_smc *smc, unsigned *fan_rpm) {
  if (!smc || !fan_rpm || !smc->fan_key_count)
    return false;

  nvtop_time current_time;
  nvtop_get_current_time(&current_time);
  if (smc->fan_sample_attempted &&
      nvtop_difftime_u64(smc->last_fan_sample_time, current_time) < apple_smc_minimum_sample_interval) {
    if (smc->last_fan_rpm_valid)
      *fan_rpm = smc->last_fan_rpm;
    return smc->last_fan_rpm_valid;
  }
  smc->last_fan_sample_time = current_time;
  smc->fan_sample_attempted = true;

  size_t sample_count = 0;
  for (size_t i = 0; i < smc->fan_key_count; ++i) {
    uint8_t bytes[32];
    float sample;
    if (gpuinfo_apple_smc_read_key(smc, smc->fan_keys[i].key, &smc->fan_keys[i].info, bytes) &&
        gpuinfo_apple_decode_smc_float(bytes, smc->fan_keys[i].info.data_size, &sample))
      smc->fan_samples[sample_count++] = sample;
  }

  unsigned maximum_fan_rpm;
  if (!gpuinfo_apple_max_fan_rpm(smc->fan_samples, sample_count, &maximum_fan_rpm)) {
    smc->last_fan_rpm_valid = false;
    return false;
  }

  smc->last_fan_rpm = maximum_fan_rpm;
  smc->last_fan_rpm_valid = true;
  *fan_rpm = maximum_fan_rpm;
  return true;
}
