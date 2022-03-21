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

#include "nvtop/extract_gpuinfo_nvidia.h"
#include "nvtop/extract_gpuinfo_common.h"

#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#define NVML_SUCCESS 0
#define NVML_ERROR_INSUFFICIENT_SIZE 7

typedef int nvmlReturn_t; // store the enum as int

// Init and shutdown

static nvmlReturn_t (*nvmlInit)(void);

static nvmlReturn_t (*nvmlShutdown)(void);

// Static information and helper functions

static nvmlReturn_t (*nvmlDeviceGetCount)(unsigned int *deviceCount);

static nvmlReturn_t (*nvmlDeviceGetHandleByIndex)(unsigned int index,
                                                  nvmlDevice_t *device);

static const char *(*nvmlErrorString)(nvmlReturn_t);

static nvmlReturn_t (*nvmlDeviceGetName)(nvmlDevice_t device, char *name,
                                         unsigned int length);

static nvmlReturn_t (*nvmlDeviceGetMaxPcieLinkGeneration)(
    nvmlDevice_t device, unsigned int *maxLinkGen);

static nvmlReturn_t (*nvmlDeviceGetMaxPcieLinkWidth)(
    nvmlDevice_t device, unsigned int *maxLinkWidth);

typedef enum {
  NVML_TEMPERATURE_THRESHOLD_SHUTDOWN = 0,
  NVML_TEMPERATURE_THRESHOLD_SLOWDOWN = 1,
} nvmlTemperatureThresholds_t;

static nvmlReturn_t (*nvmlDeviceGetTemperatureThreshold)(
    nvmlDevice_t device, nvmlTemperatureThresholds_t thresholdType,
    unsigned int *temp);

// Dynamic information extraction

typedef enum {
  NVML_CLOCK_GRAPHICS = 0,
  NVML_CLOCK_SM = 1,
  NVML_CLOCK_MEM = 2,
  NVML_CLOCK_VIDEO = 3,
} nvmlClockType_t;

static nvmlReturn_t (*nvmlDeviceGetClockInfo)(nvmlDevice_t device,
                                              nvmlClockType_t type,
                                              unsigned int *clock);

static nvmlReturn_t (*nvmlDeviceGetMaxClockInfo)(nvmlDevice_t device,
                                                 nvmlClockType_t type,
                                                 unsigned int *clock);

typedef struct {
  unsigned int gpu;
  unsigned int memory;
} nvmlUtilization_t;

static nvmlReturn_t (*nvmlDeviceGetUtilizationRates)(
    nvmlDevice_t device, nvmlUtilization_t *utilization);

typedef struct {
  unsigned long long total;
  unsigned long long free;
  unsigned long long used;
} nvmlMemory_t;

static nvmlReturn_t (*nvmlDeviceGetMemoryInfo)(nvmlDevice_t device,
                                               nvmlMemory_t *memory);

static nvmlReturn_t (*nvmlDeviceGetCurrPcieLinkGeneration)(
    nvmlDevice_t device, unsigned int *currLinkGen);

static nvmlReturn_t (*nvmlDeviceGetCurrPcieLinkWidth)(
    nvmlDevice_t device, unsigned int *currLinkWidth);

typedef enum {
  NVML_PCIE_UTIL_TX_BYTES = 0,
  NVML_PCIE_UTIL_RX_BYTES = 1,
} nvmlPcieUtilCounter_t;

static nvmlReturn_t (*nvmlDeviceGetPcieThroughput)(
    nvmlDevice_t device, nvmlPcieUtilCounter_t counter, unsigned int *value);

static nvmlReturn_t (*nvmlDeviceGetFanSpeed)(nvmlDevice_t device,
                                             unsigned int *speed);

typedef enum {
  NVML_TEMPERATURE_GPU = 0,
} nvmlTemperatureSensors_t;

static nvmlReturn_t (*nvmlDeviceGetTemperature)(
    nvmlDevice_t device, nvmlTemperatureSensors_t sensorType,
    unsigned int *temp);

static nvmlReturn_t (*nvmlDeviceGetPowerUsage)(nvmlDevice_t device,
                                               unsigned int *power);

static nvmlReturn_t (*nvmlDeviceGetEnforcedPowerLimit)(nvmlDevice_t device,
                                                       unsigned int *limit);

static nvmlReturn_t (*nvmlDeviceGetEncoderUtilization)(
    nvmlDevice_t device, unsigned int *utilization,
    unsigned int *samplingPeriodUs);

static nvmlReturn_t (*nvmlDeviceGetDecoderUtilization)(
    nvmlDevice_t device, unsigned int *utilization,
    unsigned int *samplingPeriodUs);

// Processes running on GPU

typedef struct {
  unsigned int pid;
  unsigned long long usedGpuMemory;
  // unsigned int gpuInstanceId;      // not supported by older NVIDIA drivers
  // unsigned int computeInstanceId;  // not supported by older NVIDIA drivers
} nvmlProcessInfo_t;

static nvmlReturn_t (*nvmlDeviceGetGraphicsRunningProcesses)(
    nvmlDevice_t device, unsigned int *infoCount, nvmlProcessInfo_t *infos);

static nvmlReturn_t (*nvmlDeviceGetComputeRunningProcesses)(
    nvmlDevice_t device, unsigned int *infoCount, nvmlProcessInfo_t *infos);

static void *libnvidia_ml_handle;

static nvmlReturn_t last_nvml_return_status = NVML_SUCCESS;
static char didnt_call_gpuinfo_init[] =
    "The NVIDIA extraction has not been initialized, please call "
    "gpuinfo_nvidia_init\n";
static const char *local_error_string = didnt_call_gpuinfo_init;

// Processes GPU Utilization

typedef struct {
  unsigned int pid;
  unsigned long long timeStamp;
  unsigned int smUtil;
  unsigned int memUtil;
  unsigned int encUtil;
  unsigned int decUtil;
} nvmlProcessUtilizationSample_t;

nvmlReturn_t (*nvmlDeviceGetProcessUtilization)(
    nvmlDevice_t device, nvmlProcessUtilizationSample_t *utilization,
    unsigned int *processSamplesCount, unsigned long long lastSeenTimeStamp);

/*
 *
 * This function loads the libnvidia-ml.so shared object, initializes the
 * required function pointers and calls the nvidia library initialization
 * function. Returns true if everything has been initialized successfully. If
 * false is returned, the cause of the error can be retrieved by calling the
 * function gpuinfo_nvidia_last_error_string.
 *
 */
bool gpuinfo_nvidia_init(void) {

  libnvidia_ml_handle = dlopen("libnvidia-ml.so", RTLD_LAZY);
  if (!libnvidia_ml_handle)
    libnvidia_ml_handle = dlopen("libnvidia-ml.so.1", RTLD_LAZY);
  if (!libnvidia_ml_handle) {
    local_error_string = dlerror();
    return false;
  }

  // Default to last version
  nvmlInit = dlsym(libnvidia_ml_handle, "nvmlInit_v2");
  if (!nvmlInit)
    nvmlInit = dlsym(libnvidia_ml_handle, "nvmlInit");
  if (!nvmlInit)
    goto init_error_clean_exit;

  nvmlShutdown = dlsym(libnvidia_ml_handle, "nvmlShutdown");
  if (!nvmlShutdown)
    goto init_error_clean_exit;

  // Default to last version if available
  nvmlDeviceGetCount = dlsym(libnvidia_ml_handle, "nvmlDeviceGetCount_v2");
  if (!nvmlDeviceGetCount)
    nvmlDeviceGetCount = dlsym(libnvidia_ml_handle, "nvmlDeviceGetCount");
  if (!nvmlDeviceGetCount)
    goto init_error_clean_exit;

  nvmlDeviceGetHandleByIndex =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetHandleByIndex_v2");
  if (!nvmlDeviceGetHandleByIndex)
    nvmlDeviceGetHandleByIndex =
        dlsym(libnvidia_ml_handle, "nvmlDeviceGetHandleByIndex");
  if (!nvmlDeviceGetHandleByIndex)
    goto init_error_clean_exit;

  nvmlErrorString = dlsym(libnvidia_ml_handle, "nvmlErrorString");
  if (!nvmlErrorString)
    goto init_error_clean_exit;

  nvmlDeviceGetName = dlsym(libnvidia_ml_handle, "nvmlDeviceGetName");
  if (!nvmlDeviceGetName)
    goto init_error_clean_exit;

  nvmlDeviceGetMaxPcieLinkGeneration =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetMaxPcieLinkGeneration");
  if (!nvmlDeviceGetMaxPcieLinkGeneration)
    goto init_error_clean_exit;

  nvmlDeviceGetMaxPcieLinkWidth =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetMaxPcieLinkWidth");
  if (!nvmlDeviceGetMaxPcieLinkWidth)
    goto init_error_clean_exit;

  nvmlDeviceGetTemperatureThreshold =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetTemperatureThreshold");
  if (!nvmlDeviceGetTemperatureThreshold)
    goto init_error_clean_exit;

  nvmlDeviceGetClockInfo = dlsym(libnvidia_ml_handle, "nvmlDeviceGetClockInfo");
  if (!nvmlDeviceGetClockInfo)
    goto init_error_clean_exit;

  nvmlDeviceGetMaxClockInfo = dlsym(
      libnvidia_ml_handle, "nvmlDeviceGetMaxClockInfo");
  if (!nvmlDeviceGetMaxClockInfo)
    goto init_error_clean_exit;

  nvmlDeviceGetUtilizationRates =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetUtilizationRates");
  if (!nvmlDeviceGetUtilizationRates)
    goto init_error_clean_exit;

  nvmlDeviceGetMemoryInfo =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetMemoryInfo");
  if (!nvmlDeviceGetMemoryInfo)
    goto init_error_clean_exit;

  nvmlDeviceGetCurrPcieLinkGeneration =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetCurrPcieLinkGeneration");
  if (!nvmlDeviceGetCurrPcieLinkGeneration)
    goto init_error_clean_exit;

  nvmlDeviceGetCurrPcieLinkWidth =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetCurrPcieLinkWidth");
  if (!nvmlDeviceGetCurrPcieLinkWidth)
    goto init_error_clean_exit;

  nvmlDeviceGetPcieThroughput =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetPcieThroughput");
  if (!nvmlDeviceGetPcieThroughput)
    goto init_error_clean_exit;

  nvmlDeviceGetFanSpeed = dlsym(libnvidia_ml_handle, "nvmlDeviceGetFanSpeed");
  if (!nvmlDeviceGetFanSpeed)
    goto init_error_clean_exit;

  nvmlDeviceGetTemperature =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetTemperature");
  if (!nvmlDeviceGetTemperature)
    goto init_error_clean_exit;

  nvmlDeviceGetPowerUsage =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetPowerUsage");
  if (!nvmlDeviceGetPowerUsage)
    goto init_error_clean_exit;

  nvmlDeviceGetEnforcedPowerLimit =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetEnforcedPowerLimit");
  if (!nvmlDeviceGetEnforcedPowerLimit)
    goto init_error_clean_exit;

  nvmlDeviceGetEncoderUtilization =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetEncoderUtilization");
  if (!nvmlDeviceGetEncoderUtilization)
    goto init_error_clean_exit;

  nvmlDeviceGetDecoderUtilization =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetDecoderUtilization");
  if (!nvmlDeviceGetDecoderUtilization)
    goto init_error_clean_exit;

  nvmlDeviceGetGraphicsRunningProcesses =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetGraphicsRunningProcesses");
  if (!nvmlDeviceGetGraphicsRunningProcesses)
    goto init_error_clean_exit;

  nvmlDeviceGetComputeRunningProcesses =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetComputeRunningProcesses");
  if (!nvmlDeviceGetComputeRunningProcesses)
    goto init_error_clean_exit;

  // This one might not be available
  nvmlDeviceGetProcessUtilization =
      dlsym(libnvidia_ml_handle, "nvmlDeviceGetProcessUtilization");

  last_nvml_return_status = nvmlInit();
  if (last_nvml_return_status != NVML_SUCCESS) {
    return false;
  }
  local_error_string = NULL;

  return true;

init_error_clean_exit:
  dlclose(libnvidia_ml_handle);
  libnvidia_ml_handle = NULL;
  return false;
}

void gpuinfo_nvidia_shutdown(void) {
  if (libnvidia_ml_handle) {
    nvmlShutdown();
    dlclose(libnvidia_ml_handle);
    libnvidia_ml_handle = NULL;
    local_error_string = didnt_call_gpuinfo_init;
  }
}

const char *gpuinfo_nvidia_last_error_string(void) {
  if (local_error_string) {
    return local_error_string;
  } else if (libnvidia_ml_handle && nvmlErrorString) {
    return nvmlErrorString(last_nvml_return_status);
  } else {
    return "An unanticipated error occurred while accessing NVIDIA GPU "
           "information\n";
  }
}

bool gpuinfo_nvidia_get_device_handles(
    nvmlDevice_t **handle_array_ptr, unsigned *count,
    uint64_t mask) {

  if (!libnvidia_ml_handle)
    return false;

  unsigned num_devices;
  last_nvml_return_status = nvmlDeviceGetCount(&num_devices);
  if (last_nvml_return_status != NVML_SUCCESS)
    return false;

  *handle_array_ptr = malloc(num_devices * sizeof(**handle_array_ptr));
  if (!*handle_array_ptr) {
    local_error_string = strerror(errno);
    return false;
  }
  *count = 0;
  for (unsigned int i = 0; i < num_devices; ++i) {
    if (i < CHAR_BIT * sizeof(mask) && (mask & (1 << i)) == 0)
      continue;
    last_nvml_return_status =
        nvmlDeviceGetHandleByIndex(i, &(*handle_array_ptr)[*count]);
    if (last_nvml_return_status == NVML_SUCCESS)
      *count += 1;
  }
  return true;
}

void gpuinfo_nvidia_populate_static_info(nvmlDevice_t device,
                                         struct gpuinfo_static_info *static_info) {
  last_nvml_return_status =
      nvmlDeviceGetName(device, static_info->device_name, MAX_DEVICE_NAME);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
  else
    RESET_VALID(gpuinfo_device_name_valid, static_info->valid);

  last_nvml_return_status =
      nvmlDeviceGetMaxPcieLinkGeneration(device, &static_info->max_pcie_gen);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_max_pcie_gen_valid, static_info->valid);
  else
    RESET_VALID(gpuinfo_max_pcie_gen_valid, static_info->valid);

  last_nvml_return_status =
      nvmlDeviceGetMaxPcieLinkWidth(device, &static_info->max_pcie_link_width);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_max_link_width_valid, static_info->valid);
  else
    RESET_VALID(gpuinfo_max_link_width_valid, static_info->valid);

  last_nvml_return_status = nvmlDeviceGetTemperatureThreshold(
      device, NVML_TEMPERATURE_THRESHOLD_SHUTDOWN,
      &static_info->temperature_shutdown_threshold);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_temperature_shutdown_valid, static_info->valid);
  else
    RESET_VALID(gpuinfo_temperature_shutdown_valid, static_info->valid);

  last_nvml_return_status = nvmlDeviceGetTemperatureThreshold(
      device, NVML_TEMPERATURE_THRESHOLD_SLOWDOWN,
      &static_info->temperature_slowdown_threshold);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_temperature_slowdown_valid, static_info->valid);
  else
    RESET_VALID(gpuinfo_temperature_slowdown_valid, static_info->valid);
}

void gpuinfo_nvidia_refresh_dynamic_info(nvmlDevice_t device,
                                         struct gpuinfo_dynamic_info *dynamic_info) {

  bool graphics_clock_valid = false;
  unsigned graphics_clock;
  bool sm_clock_valid = false;
  unsigned sm_clock;
  nvmlClockType_t getMaxClockFrom = NVML_CLOCK_GRAPHICS;

  // GPU current speed
  // Maximum between SM and Graphical
  last_nvml_return_status =
      nvmlDeviceGetClockInfo(device, NVML_CLOCK_GRAPHICS, &graphics_clock);
  graphics_clock_valid = last_nvml_return_status == NVML_SUCCESS;

  last_nvml_return_status =
      nvmlDeviceGetClockInfo(device, NVML_CLOCK_SM, &sm_clock);
  sm_clock_valid = last_nvml_return_status == NVML_SUCCESS;

  if (graphics_clock_valid && sm_clock_valid && graphics_clock < sm_clock) {
    getMaxClockFrom = NVML_CLOCK_SM;
  } else if (!graphics_clock_valid && sm_clock_valid) {
    getMaxClockFrom = NVML_CLOCK_SM;
  }

  RESET_VALID(gpuinfo_curr_gpu_clock_speed_valid, dynamic_info->valid);
  if (getMaxClockFrom == NVML_CLOCK_GRAPHICS && graphics_clock_valid) {
    dynamic_info->gpu_clock_speed = graphics_clock;
    SET_VALID(gpuinfo_curr_gpu_clock_speed_valid, dynamic_info->valid);
  }
  if (getMaxClockFrom == NVML_CLOCK_SM && sm_clock_valid) {
    dynamic_info->gpu_clock_speed = sm_clock;
    SET_VALID(gpuinfo_curr_gpu_clock_speed_valid, dynamic_info->valid);
  }

  // GPU max speed
  last_nvml_return_status = nvmlDeviceGetMaxClockInfo(
      device, getMaxClockFrom, &dynamic_info->gpu_clock_speed_max);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_max_gpu_clock_speed_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_max_gpu_clock_speed_valid, dynamic_info->valid);

  // Memory current speed
  last_nvml_return_status = nvmlDeviceGetClockInfo(
      device, NVML_CLOCK_MEM, &dynamic_info->mem_clock_speed);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_curr_mem_clock_speed_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_curr_mem_clock_speed_valid, dynamic_info->valid);

  // Memory max speed
  last_nvml_return_status = nvmlDeviceGetMaxClockInfo(
      device, NVML_CLOCK_MEM, &dynamic_info->mem_clock_speed_max);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_max_mem_clock_speed_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_max_mem_clock_speed_valid, dynamic_info->valid);

  // CPU and Memory utilization rates
  nvmlUtilization_t utilization_percentages;
  last_nvml_return_status =
      nvmlDeviceGetUtilizationRates(device, &utilization_percentages);
  if (last_nvml_return_status == NVML_SUCCESS) {
    dynamic_info->gpu_util_rate = utilization_percentages.gpu;
    SET_VALID(gpuinfo_gpu_util_rate_valid, dynamic_info->valid);
  } else {
    RESET_VALID(gpuinfo_gpu_util_rate_valid, dynamic_info->valid);
  }

  // Encoder utilization rate
  unsigned ignored_period;
  last_nvml_return_status = nvmlDeviceGetEncoderUtilization(
      device, &dynamic_info->encoder_rate, &ignored_period);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_encoder_rate_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_encoder_rate_valid, dynamic_info->valid);

  // Decoder utilization rate
  last_nvml_return_status = nvmlDeviceGetDecoderUtilization(
      device, &dynamic_info->decoder_rate, &ignored_period);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_decoder_rate_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_decoder_rate_valid, dynamic_info->valid);

  // Device memory info (total,used,free)
  nvmlMemory_t memory_info;
  last_nvml_return_status = nvmlDeviceGetMemoryInfo(device, &memory_info);
  if (last_nvml_return_status == NVML_SUCCESS) {
    dynamic_info->total_memory = memory_info.total;
    dynamic_info->used_memory = memory_info.used;
    dynamic_info->free_memory = memory_info.free;
    dynamic_info->mem_util_rate = memory_info.used * 100 / memory_info.total;
    SET_VALID(gpuinfo_total_memory_valid, dynamic_info->valid);
    SET_VALID(gpuinfo_used_memory_valid, dynamic_info->valid);
    SET_VALID(gpuinfo_free_memory_valid, dynamic_info->valid);
    SET_VALID(gpuinfo_mem_util_rate_valid, dynamic_info->valid);
  } else {
    RESET_VALID(gpuinfo_total_memory_valid, dynamic_info->valid);
    RESET_VALID(gpuinfo_used_memory_valid, dynamic_info->valid);
    RESET_VALID(gpuinfo_free_memory_valid, dynamic_info->valid);
    RESET_VALID(gpuinfo_mem_util_rate_valid, dynamic_info->valid);
  }

  // Pcie generation used by the device
  last_nvml_return_status = nvmlDeviceGetCurrPcieLinkGeneration(
      device, &dynamic_info->curr_pcie_link_gen);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_pcie_link_gen_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_pcie_link_gen_valid, dynamic_info->valid);

  // Pcie width used by the device
  last_nvml_return_status = nvmlDeviceGetCurrPcieLinkWidth(
      device, &dynamic_info->curr_pcie_link_width);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_pcie_link_width_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_pcie_link_width_valid, dynamic_info->valid);

  // Pcie reception throughput
  last_nvml_return_status = nvmlDeviceGetPcieThroughput(
      device, NVML_PCIE_UTIL_RX_BYTES, &dynamic_info->pcie_rx);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_pcie_rx_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_pcie_rx_valid, dynamic_info->valid);

  // Pcie transmission throughput
  last_nvml_return_status = nvmlDeviceGetPcieThroughput(
      device, NVML_PCIE_UTIL_TX_BYTES, &dynamic_info->pcie_tx);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_pcie_tx_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_pcie_tx_valid, dynamic_info->valid);

  // Fan speed
  last_nvml_return_status =
      nvmlDeviceGetFanSpeed(device, &dynamic_info->fan_speed);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_fan_speed_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_fan_speed_valid, dynamic_info->valid);

  // GPU temperature
  last_nvml_return_status = nvmlDeviceGetTemperature(
      device, NVML_TEMPERATURE_GPU, &dynamic_info->gpu_temp);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_gpu_temp_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_gpu_temp_valid, dynamic_info->valid);

  // Device power usage
  last_nvml_return_status =
      nvmlDeviceGetPowerUsage(device, &dynamic_info->power_draw);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_power_draw_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_power_draw_valid, dynamic_info->valid);

  // Maximum enforced power usage
  last_nvml_return_status =
      nvmlDeviceGetEnforcedPowerLimit(device, &dynamic_info->power_draw_max);
  if (last_nvml_return_status == NVML_SUCCESS)
    SET_VALID(gpuinfo_power_draw_max_valid, dynamic_info->valid);
  else
    RESET_VALID(gpuinfo_power_draw_max_valid, dynamic_info->valid);
}

static void gpuinfo_nvidia_get_process_utilization(
    nvmlDevice_t device, struct gpuinfo_nvidia_internal_data *internal,
    unsigned num_processes_recovered,
    struct gpu_process processes[num_processes_recovered]) {
  if (num_processes_recovered && nvmlDeviceGetProcessUtilization) {
    unsigned samples_count = 0;
    nvmlReturn_t retval = nvmlDeviceGetProcessUtilization(
        device, NULL, &samples_count, internal->last_utilization_timestamp);
    if (retval != NVML_ERROR_INSUFFICIENT_SIZE)
      return;
    nvmlProcessUtilizationSample_t *samples =
        malloc(samples_count * sizeof(*samples));
    retval = nvmlDeviceGetProcessUtilization(
        device, samples, &samples_count, internal->last_utilization_timestamp);
    if (retval != NVML_SUCCESS) {
      free(samples);
      return;
    }
    unsigned long long newest_timestamp_candidate =
        internal->last_utilization_timestamp;
    for (unsigned i = 0; i < samples_count; ++i) {
      bool process_matched = false;
      for (unsigned j = 0; !process_matched && j < num_processes_recovered;
           ++j) {
        // Filter out samples due to inconsistency in the results returned by
        // the function nvmlDeviceGetProcessUtilization (see bug #110 on
        // Github). Check for a valid running process returned by
        // nvmlDeviceGetComputeRunningProcesses or
        // nvmlDeviceGetGraphicsRunningProcesses, filter out inconsistent
        // utilization value greater than 100% and filter out timestamp results
        // that are less recent than what we were asking for
        if ((pid_t)samples[i].pid == processes[j].pid &&
            samples[i].smUtil <= 100 && samples[i].encUtil <= 100 &&
            samples[i].decUtil <= 100 &&
            samples[i].timeStamp > internal->last_utilization_timestamp) {
          // Collect the largest valid timestamp for this device to filter out
          // the samples during the next call to the function
          // nvmlDeviceGetProcessUtilization
          if (samples[i].timeStamp > newest_timestamp_candidate)
            newest_timestamp_candidate = samples[i].timeStamp;

          processes[j].gpu_usage = samples[i].smUtil;
          SET_VALID(gpuinfo_process_gpu_usage_valid, processes[j].valid);
          processes[j].encode_usage = samples[i].encUtil;
          SET_VALID(gpuinfo_process_gpu_encoder_valid, processes[j].valid);
          processes[j].decode_usage = samples[i].decUtil;
          SET_VALID(gpuinfo_process_gpu_decoder_valid, processes[j].valid);
          process_matched = true;
        }
      }
    }
    internal->last_utilization_timestamp = newest_timestamp_candidate;
    free(samples);
  }
}

#define DEFAULT_PROCESS_ARRAY_SIZE 64

void gpuinfo_nvidia_get_running_processes(
    nvmlDevice_t device, struct gpuinfo_nvidia_internal_data *internal,
    unsigned *num_processes_recovered, struct gpu_process **processes_info) {
  *num_processes_recovered = 0;
  size_t array_size = DEFAULT_PROCESS_ARRAY_SIZE;
  nvmlProcessInfo_t *retrieved_infos =
      malloc(array_size * sizeof(*retrieved_infos));
  if (!retrieved_infos) {
    perror("Could not allocate memory: ");
    exit(EXIT_FAILURE);
  }
  unsigned graphical_count = 0, compute_count = 0, recovered_count;
retry_query_graphical:
  recovered_count = array_size;
  last_nvml_return_status = nvmlDeviceGetGraphicsRunningProcesses(
      device, &recovered_count, retrieved_infos);
  if (last_nvml_return_status == NVML_ERROR_INSUFFICIENT_SIZE) {
    array_size += array_size;
    retrieved_infos =
        realloc(retrieved_infos, array_size * sizeof(*retrieved_infos));
    if (!retrieved_infos) {
      perror("Could not re-allocate memory: ");
      exit(EXIT_FAILURE);
    }
    goto retry_query_graphical;
  }
  if (last_nvml_return_status == NVML_SUCCESS) {
    graphical_count = recovered_count;
  }
retry_query_compute:
  recovered_count = array_size - graphical_count;
  last_nvml_return_status = nvmlDeviceGetComputeRunningProcesses(
      device, &recovered_count, retrieved_infos + graphical_count);
  if (last_nvml_return_status == NVML_ERROR_INSUFFICIENT_SIZE) {
    array_size += array_size;
    retrieved_infos =
        realloc(retrieved_infos, array_size * sizeof(*retrieved_infos));
    if (!retrieved_infos) {
      perror("Could not re-allocate memory: ");
      exit(EXIT_FAILURE);
    }
    goto retry_query_compute;
  }
  if (last_nvml_return_status == NVML_SUCCESS) {
    compute_count = recovered_count;
  }

  *num_processes_recovered = graphical_count + compute_count;
  if (*num_processes_recovered > 0) {
    *processes_info =
        malloc(*num_processes_recovered * sizeof(**processes_info));
    if (!*processes_info) {
      perror("Could not allocate memory: ");
      exit(EXIT_FAILURE);
    }
    for (unsigned i = 0; i < graphical_count + compute_count; ++i) {
      if (i < graphical_count)
        (*processes_info)[i].type = gpu_process_graphical;
      else
        (*processes_info)[i].type = gpu_process_compute;
      (*processes_info)[i].pid = retrieved_infos[i].pid;
      (*processes_info)[i].gpu_memory_usage = retrieved_infos[i].usedGpuMemory;
      SET_VALID(gpuinfo_process_gpu_memory_usage_valid,
                (*processes_info)[i].valid);
      RESET_VALID(gpuinfo_process_cmdline_valid, (*processes_info)[i].valid);
      RESET_VALID(gpuinfo_process_user_name_valid, (*processes_info)[i].valid);
      RESET_VALID(gpuinfo_process_gpu_usage_valid, (*processes_info)[i].valid);
      RESET_VALID(gpuinfo_process_gpu_encoder_valid,
                  (*processes_info)[i].valid);
      RESET_VALID(gpuinfo_process_gpu_decoder_valid,
                  (*processes_info)[i].valid);
      RESET_VALID(gpuinfo_process_gpu_memory_percentage_valid,
                  (*processes_info)[i].valid);
      RESET_VALID(gpuinfo_process_cpu_usage_valid, (*processes_info)[i].valid);
      RESET_VALID(gpuinfo_process_cpu_memory_virt_valid,
                  (*processes_info)[i].valid);
      RESET_VALID(gpuinfo_process_cpu_memory_res_valid,
                  (*processes_info)[i].valid);
    }
  } else {
    *processes_info = NULL;
  }
  free(retrieved_infos);
  gpuinfo_nvidia_get_process_utilization(
      device, internal, *num_processes_recovered, *processes_info);
}
