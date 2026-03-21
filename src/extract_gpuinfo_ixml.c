/*
 *
 * Copyright (C) 2017-2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
 * Copyright (C) 2025 The NVTOP Contributors
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

#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"

#include <dlfcn.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define IXML_POWER_DEBUG 0

#if IXML_POWER_DEBUG
#define IXML_POWER_LOG(fmt, ...) fprintf(stderr, "[IXML_POWER] " fmt "\n", ##__VA_ARGS__)
#else
#define IXML_POWER_LOG(fmt, ...) ((void)0)
#endif

#define NVML_SUCCESS 0
#define NVML_ERROR_UNINITIALIZED 1
#define NVML_ERROR_INVALID_ARGUMENT 2
#define NVML_ERROR_NOT_SUPPORTED 3
#define NVML_ERROR_NO_PERMISSION 4
#define NVML_ERROR_ALREADY_INITIALIZED 5
#define NVML_ERROR_NOT_FOUND 6
#define NVML_ERROR_INSUFFICIENT_SIZE 7
#define NVML_ERROR_DRIVER_NOT_LOADED 9
#define NVML_ERROR_GPU_IS_LOST 10
#define NVML_ERROR_UNKNOWN 999

#define IXML_POWER_MIN_VALID_MW 1000
#define IXML_POWER_MAX_VALID_MW 1000000
#define IXML_POWER_W_TO_MW 1000U

static const char *nvml_error_string(int error_code) {
  switch (error_code) {
    case NVML_SUCCESS:
      return "Success";
    case NVML_ERROR_UNINITIALIZED:
      return "Uninitialized";
    case NVML_ERROR_INVALID_ARGUMENT:
      return "Invalid Argument";
    case NVML_ERROR_NOT_SUPPORTED:
      return "Not Supported";
    case NVML_ERROR_NO_PERMISSION:
      return "No Permission";
    case NVML_ERROR_ALREADY_INITIALIZED:
      return "Already Initialized";
    case NVML_ERROR_NOT_FOUND:
      return "Not Found";
    case NVML_ERROR_INSUFFICIENT_SIZE:
      return "Insufficient Size";
    case NVML_ERROR_DRIVER_NOT_LOADED:
      return "Driver Not Loaded";
    case NVML_ERROR_GPU_IS_LOST:
      return "GPU Is Lost";
    case NVML_ERROR_UNKNOWN:
    default:
      return "Unknown Error";
  }
}

typedef struct nvmlDevice *nvmlDevice_t;
typedef int nvmlReturn_t;

static nvmlReturn_t (*nvmlInit)(void);
static nvmlReturn_t (*nvmlShutdown)(void);

static nvmlReturn_t (*nvmlDeviceGetCount)(unsigned int *deviceCount);
static nvmlReturn_t (*nvmlDeviceGetHandleByIndex)(unsigned int index, nvmlDevice_t *device);
static const char *(*nvmlErrorString)(nvmlReturn_t);
static nvmlReturn_t (*nvmlDeviceGetName)(nvmlDevice_t device, char *name, unsigned int length);

typedef struct {
  char busIdLegacy[16];
  unsigned int domain;
  unsigned int bus;
  unsigned int device;
  unsigned int pciDeviceId;
  unsigned int pciSubSystemId;
  char busId[32];
} nvmlPciInfo_t;

static nvmlReturn_t (*nvmlDeviceGetPciInfo)(nvmlDevice_t device, nvmlPciInfo_t *pciInfo);
static nvmlReturn_t (*nvmlDeviceGetMaxPcieLinkGeneration)(nvmlDevice_t device, unsigned int *maxLinkGen);
static nvmlReturn_t (*nvmlDeviceGetMaxPcieLinkWidth)(nvmlDevice_t device, unsigned int *maxLinkWidth);

typedef enum {
  NVML_TEMPERATURE_THRESHOLD_SHUTDOWN = 0,
  NVML_TEMPERATURE_THRESHOLD_SLOWDOWN = 1,
} nvmlTemperatureThresholds_t;

static nvmlReturn_t (*nvmlDeviceGetTemperatureThreshold)(nvmlDevice_t device, nvmlTemperatureThresholds_t thresholdType,
                                                         unsigned int *temp);

typedef enum {
  NVML_CLOCK_GRAPHICS = 0,
  NVML_CLOCK_SM = 1,
  NVML_CLOCK_MEM = 2,
  NVML_CLOCK_VIDEO = 3,
} nvmlClockType_t;

static nvmlReturn_t (*nvmlDeviceGetClockInfo)(nvmlDevice_t device, nvmlClockType_t type, unsigned int *clock);
static nvmlReturn_t (*nvmlDeviceGetMaxClockInfo)(nvmlDevice_t device, nvmlClockType_t type, unsigned int *clock);

typedef struct {
  unsigned int gpu;
  unsigned int memory;
} nvmlUtilization_t;

static nvmlReturn_t (*nvmlDeviceGetUtilizationRates)(nvmlDevice_t device, nvmlUtilization_t *utilization);

typedef struct {
  unsigned long long total;
  unsigned long long free;
  unsigned long long used;
} nvmlMemory_v1_t;

typedef struct {
  unsigned int version;
  unsigned long long total;
  unsigned long long reserved;
  unsigned long long free;
  unsigned long long used;
} nvmlMemory_v2_t;

static nvmlReturn_t (*nvmlDeviceGetMemoryInfo)(nvmlDevice_t device, nvmlMemory_v1_t *memory);
static nvmlReturn_t (*nvmlDeviceGetMemoryInfo_v2)(nvmlDevice_t device, nvmlMemory_v2_t *memory);

static nvmlReturn_t (*nvmlDeviceGetCurrPcieLinkGeneration)(nvmlDevice_t device, unsigned int *currLinkGen);
static nvmlReturn_t (*nvmlDeviceGetCurrPcieLinkWidth)(nvmlDevice_t device, unsigned int *currLinkWidth);

typedef enum {
  NVML_PCIE_UTIL_TX_BYTES = 0,
  NVML_PCIE_UTIL_RX_BYTES = 1,
} nvmlPcieUtilCounter_t;

static nvmlReturn_t (*nvmlDeviceGetPcieThroughput)(nvmlDevice_t device, nvmlPcieUtilCounter_t counter,
                                                   unsigned int *value);

static nvmlReturn_t (*nvmlDeviceGetFanSpeed)(nvmlDevice_t device, unsigned int *speed);

typedef enum {
  NVML_TEMPERATURE_GPU = 0,
} nvmlTemperatureSensors_t;

static nvmlReturn_t (*nvmlDeviceGetTemperature)(nvmlDevice_t device, nvmlTemperatureSensors_t sensorType,
                                                unsigned int *temp);

static nvmlReturn_t (*nvmlDeviceGetPowerUsage)(nvmlDevice_t device, unsigned int *power);
static nvmlReturn_t (*nvmlDeviceGetEnforcedPowerLimit)(nvmlDevice_t device, unsigned int *limit);

static nvmlReturn_t (*nvmlDeviceGetEncoderUtilization)(nvmlDevice_t device, unsigned int *utilization,
                                                       unsigned int *samplingPeriodUs);
static nvmlReturn_t (*nvmlDeviceGetDecoderUtilization)(nvmlDevice_t device, unsigned int *utilization,
                                                       unsigned int *samplingPeriodUs);

typedef struct {
  unsigned int pid;
  unsigned long long usedGpuMemory;
  unsigned int gpuInstanceId;
  unsigned int computeInstanceId;
} nvmlProcessInfo_t;

static nvmlReturn_t (*nvmlDeviceGetGraphicsRunningProcesses)(nvmlDevice_t device, unsigned int *infoCount,
                                                             nvmlProcessInfo_t *infos);
static nvmlReturn_t (*nvmlDeviceGetComputeRunningProcesses)(nvmlDevice_t device, unsigned int *infoCount,
                                                            nvmlProcessInfo_t *infos);

typedef struct {
  unsigned int pid;
  unsigned long long timeStamp;
  unsigned int smUtil;
  unsigned int memUtil;
  unsigned int encUtil;
  unsigned int decUtil;
} nvmlProcessUtilizationSample_t;

static nvmlReturn_t (*nvmlDeviceGetProcessUtilization)(nvmlDevice_t device, nvmlProcessUtilizationSample_t *utilization,
                                                       unsigned int *processSamplesCount,
                                                       unsigned long long lastSeenTimeStamp);

static void *libixml_handle;

static nvmlReturn_t last_nvml_return_status = NVML_SUCCESS;
static char didnt_call_gpuinfo_init[] = "The iXML extraction has not been initialized, please call "
                                        "gpuinfo_ixml_init\n";
static const char *local_error_string = didnt_call_gpuinfo_init;

struct gpu_info_ixml {
  struct gpu_info base;
  struct list_head allocate_list;
  nvmlDevice_t gpuhandle;
  unsigned long long last_utilization_timestamp;
};

static LIST_HEAD(allocations);

static bool gpuinfo_ixml_init(void);
static void gpuinfo_ixml_shutdown(void);
static const char *gpuinfo_ixml_last_error_string(void);
static bool gpuinfo_ixml_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_ixml_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_ixml_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_ixml_get_running_processes(struct gpu_info *_gpu_info);

struct gpu_vendor gpu_vendor_ixml = {
    .init = gpuinfo_ixml_init,
    .shutdown = gpuinfo_ixml_shutdown,
    .last_error_string = gpuinfo_ixml_last_error_string,
    .get_device_handles = gpuinfo_ixml_get_device_handles,
    .populate_static_info = gpuinfo_ixml_populate_static_info,
    .refresh_dynamic_info = gpuinfo_ixml_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_ixml_get_running_processes,
    .name = "Iluvatar",
};

__attribute__((constructor)) static void init_extract_gpuinfo_ixml(void) { register_gpu_vendor(&gpu_vendor_ixml); }

static bool gpuinfo_ixml_init(void) {
  libixml_handle = dlopen("/usr/local/corex/lib/libixml.so", RTLD_LAZY);
  if (!libixml_handle)
    libixml_handle = dlopen("/usr/local/corex/lib64/libixml.so", RTLD_LAZY);
  if (!libixml_handle)
    libixml_handle = dlopen("libixml.so", RTLD_LAZY);
  if (!libixml_handle) {
    local_error_string = dlerror();
    return false;
  }

  nvmlInit = dlsym(libixml_handle, "nvmlInit_v2");
  if (!nvmlInit)
    nvmlInit = dlsym(libixml_handle, "nvmlInit");
  if (!nvmlInit)
    goto init_error_clean_exit;

  nvmlShutdown = dlsym(libixml_handle, "nvmlShutdown");
  if (!nvmlShutdown)
    goto init_error_clean_exit;

  nvmlDeviceGetCount = dlsym(libixml_handle, "nvmlDeviceGetCount_v2");
  if (!nvmlDeviceGetCount)
    nvmlDeviceGetCount = dlsym(libixml_handle, "nvmlDeviceGetCount");
  if (!nvmlDeviceGetCount)
    goto init_error_clean_exit;

  nvmlDeviceGetHandleByIndex = dlsym(libixml_handle, "nvmlDeviceGetHandleByIndex_v2");
  if (!nvmlDeviceGetHandleByIndex)
    nvmlDeviceGetHandleByIndex = dlsym(libixml_handle, "nvmlDeviceGetHandleByIndex");
  if (!nvmlDeviceGetHandleByIndex)
    goto init_error_clean_exit;

  nvmlErrorString = dlsym(libixml_handle, "nvmlErrorString");
  if (!nvmlErrorString)
    goto init_error_clean_exit;

  nvmlDeviceGetName = dlsym(libixml_handle, "nvmlDeviceGetName");
  if (!nvmlDeviceGetName)
    goto init_error_clean_exit;

  nvmlDeviceGetPciInfo = dlsym(libixml_handle, "nvmlDeviceGetPciInfo_v3");
  if (!nvmlDeviceGetPciInfo)
    nvmlDeviceGetPciInfo = dlsym(libixml_handle, "nvmlDeviceGetPciInfo_v2");
  if (!nvmlDeviceGetPciInfo)
    nvmlDeviceGetPciInfo = dlsym(libixml_handle, "nvmlDeviceGetPciInfo");
  if (!nvmlDeviceGetPciInfo)
    goto init_error_clean_exit;

  nvmlDeviceGetMaxPcieLinkGeneration = dlsym(libixml_handle, "nvmlDeviceGetMaxPcieLinkGeneration");
  if (!nvmlDeviceGetMaxPcieLinkGeneration)
    goto init_error_clean_exit;

  nvmlDeviceGetMaxPcieLinkWidth = dlsym(libixml_handle, "nvmlDeviceGetMaxPcieLinkWidth");
  if (!nvmlDeviceGetMaxPcieLinkWidth)
    goto init_error_clean_exit;

  nvmlDeviceGetTemperatureThreshold = dlsym(libixml_handle, "nvmlDeviceGetTemperatureThreshold");
  if (!nvmlDeviceGetTemperatureThreshold)
    goto init_error_clean_exit;

  nvmlDeviceGetClockInfo = dlsym(libixml_handle, "nvmlDeviceGetClockInfo");
  if (!nvmlDeviceGetClockInfo)
    goto init_error_clean_exit;

  nvmlDeviceGetMaxClockInfo = dlsym(libixml_handle, "nvmlDeviceGetMaxClockInfo");
  if (!nvmlDeviceGetMaxClockInfo)
    goto init_error_clean_exit;

  nvmlDeviceGetUtilizationRates = dlsym(libixml_handle, "nvmlDeviceGetUtilizationRates");
  if (!nvmlDeviceGetUtilizationRates)
    goto init_error_clean_exit;

  nvmlDeviceGetMemoryInfo_v2 = dlsym(libixml_handle, "nvmlDeviceGetMemoryInfo_v2");
  nvmlDeviceGetMemoryInfo = dlsym(libixml_handle, "nvmlDeviceGetMemoryInfo");
  if (!nvmlDeviceGetMemoryInfo_v2 && !nvmlDeviceGetMemoryInfo)
    goto init_error_clean_exit;

  nvmlDeviceGetCurrPcieLinkGeneration = dlsym(libixml_handle, "nvmlDeviceGetCurrPcieLinkGeneration");
  if (!nvmlDeviceGetCurrPcieLinkGeneration)
    goto init_error_clean_exit;

  nvmlDeviceGetCurrPcieLinkWidth = dlsym(libixml_handle, "nvmlDeviceGetCurrPcieLinkWidth");
  if (!nvmlDeviceGetCurrPcieLinkWidth)
    goto init_error_clean_exit;

  nvmlDeviceGetPcieThroughput = dlsym(libixml_handle, "nvmlDeviceGetPcieThroughput");
  if (!nvmlDeviceGetPcieThroughput)
    goto init_error_clean_exit;

  nvmlDeviceGetFanSpeed = dlsym(libixml_handle, "nvmlDeviceGetFanSpeed");
  if (!nvmlDeviceGetFanSpeed)
    goto init_error_clean_exit;

  nvmlDeviceGetTemperature = dlsym(libixml_handle, "nvmlDeviceGetTemperature");
  if (!nvmlDeviceGetTemperature)
    goto init_error_clean_exit;

  nvmlDeviceGetPowerUsage = dlsym(libixml_handle, "nvmlDeviceGetPowerUsage");
  nvmlDeviceGetEnforcedPowerLimit = dlsym(libixml_handle, "nvmlDeviceGetEnforcedPowerLimit");

  nvmlDeviceGetEncoderUtilization = dlsym(libixml_handle, "nvmlDeviceGetEncoderUtilization");
  if (!nvmlDeviceGetEncoderUtilization)
    goto init_error_clean_exit;

  nvmlDeviceGetDecoderUtilization = dlsym(libixml_handle, "nvmlDeviceGetDecoderUtilization");
  if (!nvmlDeviceGetDecoderUtilization)
    goto init_error_clean_exit;

  nvmlDeviceGetGraphicsRunningProcesses = dlsym(libixml_handle, "nvmlDeviceGetGraphicsRunningProcesses");
  nvmlDeviceGetComputeRunningProcesses = dlsym(libixml_handle, "nvmlDeviceGetComputeRunningProcesses");

  nvmlDeviceGetProcessUtilization = dlsym(libixml_handle, "nvmlDeviceGetProcessUtilization");

  last_nvml_return_status = nvmlInit();
  if (last_nvml_return_status != NVML_SUCCESS) {
    return false;
  }
  local_error_string = NULL;

  return true;

init_error_clean_exit:
  dlclose(libixml_handle);
  libixml_handle = NULL;
  return false;
}

static void gpuinfo_ixml_shutdown(void) {
  if (libixml_handle) {
    nvmlShutdown();
    dlclose(libixml_handle);
    libixml_handle = NULL;
    local_error_string = didnt_call_gpuinfo_init;
  }

  struct gpu_info_ixml *allocated, *tmp;

  list_for_each_entry_safe(allocated, tmp, &allocations, allocate_list) {
    list_del(&allocated->allocate_list);
    free(allocated);
  }
}

static const char *gpuinfo_ixml_last_error_string(void) {
  if (local_error_string) {
    return local_error_string;
  } else if (libixml_handle && nvmlErrorString) {
    return nvmlErrorString(last_nvml_return_status);
  } else {
    return "An unanticipated error occurred while accessing Iluvatar GPU "
           "information\n";
  }
}

static bool gpuinfo_ixml_get_device_handles(struct list_head *devices, unsigned *count) {
  if (!libixml_handle)
    return false;

  unsigned num_devices;
  last_nvml_return_status = nvmlDeviceGetCount(&num_devices);
  if (last_nvml_return_status != NVML_SUCCESS)
    return false;

  struct gpu_info_ixml *gpu_infos = calloc(num_devices, sizeof(*gpu_infos));
  if (!gpu_infos) {
    local_error_string = strerror(errno);
    return false;
  }

  list_add(&gpu_infos[0].allocate_list, &allocations);

  *count = 0;
  for (unsigned int i = 0; i < num_devices; ++i) {
    last_nvml_return_status = nvmlDeviceGetHandleByIndex(i, &gpu_infos[*count].gpuhandle);
    if (last_nvml_return_status == NVML_SUCCESS) {
      gpu_infos[*count].base.vendor = &gpu_vendor_ixml;
      nvmlPciInfo_t pciInfo;
      nvmlReturn_t pciInfoRet = nvmlDeviceGetPciInfo(gpu_infos[*count].gpuhandle, &pciInfo);
      if (pciInfoRet == NVML_SUCCESS) {
        strncpy(gpu_infos[*count].base.pdev, pciInfo.busId, PDEV_LEN - 1);
        gpu_infos[*count].base.pdev[PDEV_LEN - 1] = '\0';
      }
      list_add_tail(&gpu_infos[*count].base.list, devices);
      *count += 1;
    }
  }

  return true;
}

static void gpuinfo_ixml_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_ixml *gpu_info = container_of(_gpu_info, struct gpu_info_ixml, base);
  struct gpuinfo_static_info *static_info = &_gpu_info->static_info;

  last_nvml_return_status = nvmlDeviceGetName(gpu_info->gpuhandle, static_info->device_name, MAX_DEVICE_NAME);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
  }

  nvmlPciInfo_t pciInfo;
  last_nvml_return_status = nvmlDeviceGetPciInfo(gpu_info->gpuhandle, &pciInfo);
  if (last_nvml_return_status == NVML_SUCCESS) {
    strncpy(_gpu_info->pdev, pciInfo.busId, PDEV_LEN - 1);
    _gpu_info->pdev[PDEV_LEN - 1] = '\0';
  }

  unsigned int value;
  last_nvml_return_status = nvmlDeviceGetMaxPcieLinkGeneration(gpu_info->gpuhandle, &value);
  if (last_nvml_return_status == NVML_SUCCESS) {
    static_info->max_pcie_gen = value;
    SET_VALID(gpuinfo_max_pcie_gen_valid, static_info->valid);
  }

  last_nvml_return_status = nvmlDeviceGetMaxPcieLinkWidth(gpu_info->gpuhandle, &value);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_GPUINFO_STATIC(static_info, max_pcie_link_width, value);
  }

  last_nvml_return_status =
      nvmlDeviceGetTemperatureThreshold(gpu_info->gpuhandle, NVML_TEMPERATURE_THRESHOLD_SLOWDOWN, &value);
  if (last_nvml_return_status == NVML_SUCCESS) {
    static_info->temperature_slowdown_threshold = value;
    SET_VALID(gpuinfo_temperature_slowdown_threshold_valid, static_info->valid);
  }

  last_nvml_return_status =
      nvmlDeviceGetTemperatureThreshold(gpu_info->gpuhandle, NVML_TEMPERATURE_THRESHOLD_SHUTDOWN, &value);
  if (last_nvml_return_status == NVML_SUCCESS) {
    static_info->temperature_shutdown_threshold = value;
    SET_VALID(gpuinfo_temperature_shutdown_threshold_valid, static_info->valid);
  }
}

static bool ixml_power_normalize(unsigned int raw_power, unsigned int *power_mw) {
  if (!raw_power || !power_mw)
    return false;

  /*
   * ixML documents nvmlDeviceGetPowerUsage as returning watts, while nvtop
   * stores power values in milliwatts. Accept both representations to keep the
   * backend compatible with ixML and NVML-like runtimes.
   */
  if (raw_power < IXML_POWER_MIN_VALID_MW) {
    unsigned long long normalized = (unsigned long long)raw_power * IXML_POWER_W_TO_MW;
    if (normalized < IXML_POWER_MIN_VALID_MW || normalized > IXML_POWER_MAX_VALID_MW)
      return false;
    *power_mw = (unsigned int)normalized;
    return true;
  }

  if (raw_power > IXML_POWER_MAX_VALID_MW)
    return false;

  *power_mw = raw_power;
  return true;
}

static void gpuinfo_ixml_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_ixml *gpu_info = container_of(_gpu_info, struct gpu_info_ixml, base);
  struct gpuinfo_dynamic_info *dynamic_info = &_gpu_info->dynamic_info;

  nvmlUtilization_t utilization;
  last_nvml_return_status = nvmlDeviceGetUtilizationRates(gpu_info->gpuhandle, &utilization);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, utilization.gpu);
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, utilization.memory);
    gpu_info->last_utilization_timestamp = 0;
  }

  if (nvmlDeviceGetMemoryInfo_v2) {
    nvmlMemory_v2_t memory;
    memset(&memory, 0, sizeof(memory));
    memory.version = 2;
    last_nvml_return_status = nvmlDeviceGetMemoryInfo_v2(gpu_info->gpuhandle, &memory);
    if (last_nvml_return_status == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, memory.total);
      SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, memory.free);
      SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, memory.used);
    }
  } else if (nvmlDeviceGetMemoryInfo) {
    nvmlMemory_v1_t memory;
    last_nvml_return_status = nvmlDeviceGetMemoryInfo(gpu_info->gpuhandle, &memory);
    if (last_nvml_return_status == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, memory.total);
      SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, memory.free);
      SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, memory.used);
    }
  }

  unsigned int value;
  last_nvml_return_status = nvmlDeviceGetCurrPcieLinkGeneration(gpu_info->gpuhandle, &value);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_gen, value);
  }

  last_nvml_return_status = nvmlDeviceGetCurrPcieLinkWidth(gpu_info->gpuhandle, &value);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_width, value);
  }

  last_nvml_return_status = nvmlDeviceGetPcieThroughput(gpu_info->gpuhandle, NVML_PCIE_UTIL_RX_BYTES, &dynamic_info->pcie_rx);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_VALID(gpuinfo_pcie_rx_valid, dynamic_info->valid);
  }

  last_nvml_return_status = nvmlDeviceGetPcieThroughput(gpu_info->gpuhandle, NVML_PCIE_UTIL_TX_BYTES, &dynamic_info->pcie_tx);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_VALID(gpuinfo_pcie_tx_valid, dynamic_info->valid);
  }

  last_nvml_return_status = nvmlDeviceGetFanSpeed(gpu_info->gpuhandle, &value);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(dynamic_info, fan_speed, value);
  }

  last_nvml_return_status = nvmlDeviceGetTemperature(gpu_info->gpuhandle, NVML_TEMPERATURE_GPU, &value);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, value);
  }

  unsigned int raw_power;
  unsigned int power_mw;
  if (nvmlDeviceGetPowerUsage) {
    last_nvml_return_status = nvmlDeviceGetPowerUsage(gpu_info->gpuhandle, &raw_power);
    if (last_nvml_return_status == NVML_SUCCESS && ixml_power_normalize(raw_power, &power_mw)) {
      IXML_POWER_LOG("power usage raw=%u normalized=%u mW", raw_power, power_mw);
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, power_mw);
    }
  }

  if (nvmlDeviceGetEnforcedPowerLimit) {
    last_nvml_return_status = nvmlDeviceGetEnforcedPowerLimit(gpu_info->gpuhandle, &raw_power);
    if (last_nvml_return_status == NVML_SUCCESS && ixml_power_normalize(raw_power, &power_mw)) {
      IXML_POWER_LOG("power limit raw=%u normalized=%u mW", raw_power, power_mw);
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, power_mw);
    }
  }

  unsigned int graphics_clock;
  last_nvml_return_status = nvmlDeviceGetClockInfo(gpu_info->gpuhandle, NVML_CLOCK_GRAPHICS, &graphics_clock);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, graphics_clock);
  } else {
    last_nvml_return_status = nvmlDeviceGetClockInfo(gpu_info->gpuhandle, NVML_CLOCK_SM, &dynamic_info->gpu_clock_speed);
    if (last_nvml_return_status == NVML_SUCCESS) {
      SET_VALID(gpuinfo_gpu_clock_speed_valid, dynamic_info->valid);
    }
  }

  unsigned int max_graphics_clock;
  last_nvml_return_status = nvmlDeviceGetMaxClockInfo(gpu_info->gpuhandle, NVML_CLOCK_GRAPHICS, &max_graphics_clock);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, max_graphics_clock);
  } else {
    last_nvml_return_status =
        nvmlDeviceGetMaxClockInfo(gpu_info->gpuhandle, NVML_CLOCK_SM, &dynamic_info->gpu_clock_speed_max);
    if (last_nvml_return_status == NVML_SUCCESS) {
      SET_VALID(gpuinfo_gpu_clock_speed_max_valid, dynamic_info->valid);
    }
  }

  last_nvml_return_status = nvmlDeviceGetClockInfo(gpu_info->gpuhandle, NVML_CLOCK_MEM, &dynamic_info->mem_clock_speed);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_VALID(gpuinfo_mem_clock_speed_valid, dynamic_info->valid);
  }

  last_nvml_return_status =
      nvmlDeviceGetMaxClockInfo(gpu_info->gpuhandle, NVML_CLOCK_MEM, &dynamic_info->mem_clock_speed_max);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_VALID(gpuinfo_mem_clock_speed_max_valid, dynamic_info->valid);
  }

  unsigned int samplingPeriodUs;
  last_nvml_return_status = nvmlDeviceGetEncoderUtilization(gpu_info->gpuhandle, &value, &samplingPeriodUs);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(&_gpu_info->dynamic_info, encoder_rate, value);
  }

  last_nvml_return_status = nvmlDeviceGetDecoderUtilization(gpu_info->gpuhandle, &value, &samplingPeriodUs);
  if (last_nvml_return_status == NVML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(&_gpu_info->dynamic_info, decoder_rate, value);
  }
}

static void append_process_infos(struct gpu_process *processes, unsigned *count, unsigned max_count,
                                 enum gpu_process_type type, nvmlProcessInfo_t *infos, unsigned info_count) {
  for (unsigned i = 0; i < info_count && *count < max_count; ++i) {
    processes[*count].type = type;
    processes[*count].pid = infos[i].pid;
    processes[*count].gpu_memory_usage = infos[i].usedGpuMemory;
    SET_VALID(gpuinfo_process_gpu_memory_usage_valid, processes[*count].valid);
    *count += 1;
  }
}

static void gpuinfo_ixml_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_ixml *gpu_info = container_of(_gpu_info, struct gpu_info_ixml, base);

  unsigned int compute_count = 0;
  unsigned int graphics_count = 0;

  if (nvmlDeviceGetGraphicsRunningProcesses) {
    last_nvml_return_status = nvmlDeviceGetGraphicsRunningProcesses(gpu_info->gpuhandle, &graphics_count, NULL);
    if (last_nvml_return_status != NVML_ERROR_INSUFFICIENT_SIZE && last_nvml_return_status != NVML_SUCCESS) {
      graphics_count = 0;
    }
  }

  if (nvmlDeviceGetComputeRunningProcesses) {
    last_nvml_return_status = nvmlDeviceGetComputeRunningProcesses(gpu_info->gpuhandle, &compute_count, NULL);
    if (last_nvml_return_status != NVML_ERROR_INSUFFICIENT_SIZE && last_nvml_return_status != NVML_SUCCESS) {
      compute_count = 0;
    }
  }

  unsigned int total_count = graphics_count + compute_count;
  if (!total_count) {
    _gpu_info->processes_count = 0;
    return;
  }

  if (total_count > _gpu_info->processes_array_size) {
    _gpu_info->processes_array_size = total_count + COMMON_PROCESS_LINEAR_REALLOC_INC;
    _gpu_info->processes =
        reallocarray(_gpu_info->processes, _gpu_info->processes_array_size, sizeof(*_gpu_info->processes));
    if (!_gpu_info->processes) {
      perror("Could not allocate memory: ");
      exit(EXIT_FAILURE);
    }
  }
  memset(_gpu_info->processes, 0, total_count * sizeof(*_gpu_info->processes));

  unsigned int process_count = 0;

  if (graphics_count) {
    nvmlProcessInfo_t *infos = calloc(graphics_count, sizeof(*infos));
    if (infos) {
      unsigned int info_count = graphics_count;
      last_nvml_return_status = nvmlDeviceGetGraphicsRunningProcesses(gpu_info->gpuhandle, &info_count, infos);
      if (last_nvml_return_status == NVML_SUCCESS) {
        append_process_infos(_gpu_info->processes, &process_count, total_count, gpu_process_graphical, infos, info_count);
      }
      free(infos);
    }
  }

  if (compute_count) {
    nvmlProcessInfo_t *infos = calloc(compute_count, sizeof(*infos));
    if (infos) {
      unsigned int info_count = compute_count;
      last_nvml_return_status = nvmlDeviceGetComputeRunningProcesses(gpu_info->gpuhandle, &info_count, infos);
      if (last_nvml_return_status == NVML_SUCCESS) {
        append_process_infos(_gpu_info->processes, &process_count, total_count, gpu_process_compute, infos, info_count);
      }
      free(infos);
    }
  }

  if (process_count && nvmlDeviceGetProcessUtilization) {
    unsigned int sample_count = 0;
    last_nvml_return_status =
        nvmlDeviceGetProcessUtilization(gpu_info->gpuhandle, NULL, &sample_count, gpu_info->last_utilization_timestamp);
    if (last_nvml_return_status != NVML_ERROR_INSUFFICIENT_SIZE) {
      _gpu_info->processes_count = process_count;
      return;
    }
    nvmlProcessUtilizationSample_t *samples = calloc(sample_count, sizeof(*samples));
    if (samples) {
      last_nvml_return_status =
          nvmlDeviceGetProcessUtilization(gpu_info->gpuhandle, samples, &sample_count, gpu_info->last_utilization_timestamp);
      if (last_nvml_return_status == NVML_SUCCESS) {
        unsigned long long newest_timestamp_candidate = gpu_info->last_utilization_timestamp;
        for (unsigned int process_idx = 0; process_idx < process_count; ++process_idx) {
          for (unsigned int sample_idx = 0; sample_idx < sample_count; ++sample_idx) {
            if (_gpu_info->processes[process_idx].pid == samples[sample_idx].pid && samples[sample_idx].smUtil <= 100 &&
                samples[sample_idx].encUtil <= 100 && samples[sample_idx].decUtil <= 100 &&
                samples[sample_idx].timeStamp > gpu_info->last_utilization_timestamp) {
              SET_GPUINFO_PROCESS(&_gpu_info->processes[process_idx], gpu_usage, samples[sample_idx].smUtil);
              SET_GPUINFO_PROCESS(&_gpu_info->processes[process_idx], encode_usage, samples[sample_idx].encUtil);
              SET_GPUINFO_PROCESS(&_gpu_info->processes[process_idx], decode_usage, samples[sample_idx].decUtil);
              if (samples[sample_idx].timeStamp > newest_timestamp_candidate) {
                newest_timestamp_candidate = samples[sample_idx].timeStamp;
              }
              break;
            }
          }
        }
        gpu_info->last_utilization_timestamp = newest_timestamp_candidate;
      }
      free(samples);
    }
  }

  for (unsigned int process_idx = 0; process_idx < process_count; ++process_idx) {
    if (!IS_VALID(gpuinfo_process_gpu_usage_valid, _gpu_info->processes[process_idx].valid))
      SET_GPUINFO_PROCESS(&_gpu_info->processes[process_idx], gpu_usage, 0);
    if (!IS_VALID(gpuinfo_process_encode_usage_valid, _gpu_info->processes[process_idx].valid))
      SET_GPUINFO_PROCESS(&_gpu_info->processes[process_idx], encode_usage, 0);
    if (!IS_VALID(gpuinfo_process_decode_usage_valid, _gpu_info->processes[process_idx].valid))
      SET_GPUINFO_PROCESS(&_gpu_info->processes[process_idx], decode_usage, 0);
  }

  _gpu_info->processes_count = process_count;
}
