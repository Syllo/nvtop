/*
 *
 * Copyright (c) 2025 Enflame Technology (Shanghai) Co., Ltd. All rights reserved.
 *
 * This file is part of Nvtop and adapted from efml from Enflame Technology (Shanghai) Co., Ltd.
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

#define EFML_SUCCESS 0
#define EFML_ERROR_INSUFFICIENT_SIZE 7

typedef struct efmlDevice *efmlDevice_t;
typedef int efmlReturn_v1_t; // store the enum as int

// Init and shutdown

static efmlReturn_v1_t (*efmlInit)(void);

static efmlReturn_v1_t (*efmlShutdown)(void);

// Static information and helper functions

static efmlReturn_v1_t (*efmlDeviceGetCount)(unsigned int *deviceCount);

static efmlReturn_v1_t (*efmlDeviceGetHandleByIndex)(unsigned int index, efmlDevice_t *device);

static const char *(*efmlErrorString)(efmlReturn_v1_t);

static efmlReturn_v1_t (*efmlDeviceGetName)(efmlDevice_t device, char *name, unsigned int length);

typedef struct {
  char busIdLegacy[16];
  unsigned int domain;
  unsigned int bus;
  unsigned int device;
  unsigned int pciDeviceId;
  unsigned int pciSubSystemId;
  char busId[32];
} efmlPciInfo_t;

static efmlReturn_v1_t (*efmlDeviceGetPciInfo)(efmlDevice_t device, efmlPciInfo_t *pciInfo);

static efmlReturn_v1_t (*efmlDeviceGetMaxPcieLinkGeneration)(efmlDevice_t device, unsigned int *maxLinkGen);

static efmlReturn_v1_t (*efmlDeviceGetMaxPcieLinkWidth)(efmlDevice_t device, unsigned int *maxLinkWidth);

typedef enum {
  EFML_TEMPERATURE_THRESHOLD_SHUTDOWN = 0,
  EFML_TEMPERATURE_THRESHOLD_SLOWDOWN = 1,
  EFML_TEMPERATURE_THRESHOLD_MEM_MAX = 2,
  EFML_TEMPERATURE_THRESHOLD_GCU_MAX = 3,
  EFML_TEMPERATURE_THRESHOLD_ACOUSTIC_MIN = 4,
  EFML_TEMPERATURE_THRESHOLD_ACOUSTIC_CURR = 5,
  EFML_TEMPERATURE_THRESHOLD_ACOUSTIC_MAX = 6,
} efmlTemperatureThresholds_t;

static efmlReturn_v1_t (*efmlDeviceGetTemperatureThreshold)(efmlDevice_t device, efmlTemperatureThresholds_t thresholdType,
                                                         unsigned int *temp);

// Dynamic information extraction

typedef enum {
  EFML_CLOCK_GRAPHICS = 0,
  EFML_CLOCK_SM = 1,
  EFML_CLOCK_MEM = 2,
  EFML_CLOCK_VIDEO = 3,
} efmlClockType_t;

static efmlReturn_v1_t (*efmlDeviceGetClockInfo)(efmlDevice_t device, efmlClockType_t type, unsigned int *clock);

static efmlReturn_v1_t (*efmlDeviceGetMaxClockInfo)(efmlDevice_t device, efmlClockType_t type, unsigned int *clock);

typedef struct {
  unsigned int gcu;
  unsigned int memory;
} efmlUtilization_t;

static efmlReturn_v1_t (*efmlDeviceGetUtilizationRates)(efmlDevice_t device, efmlUtilization_t *utilization);

typedef struct {
  unsigned long long total;
  unsigned long long free;
  unsigned long long used;
} efmlMemory_v1_t;

typedef struct {
  unsigned int version;
  unsigned long long total;
  unsigned long long reserved;
  unsigned long long free;
  unsigned long long used;
} efmlMemory_v2_t;

static efmlReturn_v1_t (*efmlDeviceGetMemoryInfo)(efmlDevice_t device, efmlMemory_v1_t *memory);
static efmlReturn_v1_t (*efmlDeviceGetMemoryInfo_v2)(efmlDevice_t device, efmlMemory_v2_t *memory);

static efmlReturn_v1_t (*efmlDeviceGetCurrPcieLinkGeneration)(efmlDevice_t device, unsigned int *currLinkGen);

static efmlReturn_v1_t (*efmlDeviceGetCurrPcieLinkWidth)(efmlDevice_t device, unsigned int *currLinkWidth);

typedef enum {
  EFML_PCIE_UTIL_TX_BYTES = 0,
  EFML_PCIE_UTIL_RX_BYTES = 1,
} efmlPcieUtilCounter_t;

static efmlReturn_v1_t (*efmlDeviceGetPcieThroughput)(efmlDevice_t device, efmlPcieUtilCounter_t counter,
                                                   unsigned int *value);

static efmlReturn_v1_t (*efmlDeviceGetFanSpeed)(efmlDevice_t device, unsigned int *speed);

typedef enum {
  EFML_TEMPERATURE_GCU = 0,
} efmlTemperatureSensors_t;

static efmlReturn_v1_t (*efmlDeviceGetTemperature)(efmlDevice_t device, efmlTemperatureSensors_t sensorType,
                                                unsigned int *temp);

static efmlReturn_v1_t (*efmlDeviceGetPowerUsage)(efmlDevice_t device, unsigned int *power);

static efmlReturn_v1_t (*efmlDeviceGetEnforcedPowerLimit)(efmlDevice_t device, unsigned int *limit);

static efmlReturn_v1_t (*efmlDeviceGetEncoderUtilization)(efmlDevice_t device, unsigned int *utilization,
                                                       unsigned int *samplingPeriodUs);

static efmlReturn_v1_t (*efmlDeviceGetDecoderUtilization)(efmlDevice_t device, unsigned int *utilization,
                                                       unsigned int *samplingPeriodUs);

// Processes running on GCU

typedef struct {
  unsigned int pid;
  unsigned long long usedGcuMemory;
} efmlProcessInfo_v1_t;

typedef struct {
  unsigned int pid;
  unsigned long long usedGcuMemory;
  unsigned int gcuInstanceId;
  unsigned int computeInstanceId;
} efmlProcessInfo_v2_t;

typedef struct {
  unsigned int pid;
  unsigned long long usedGcuMemory;
  unsigned int gcuInstanceId;
  unsigned int computeInstanceId;
} efmlProcessInfo_v3_t;

static efmlReturn_v1_t (*efmlDeviceGetGraphicsRunningProcesses_v1)(efmlDevice_t device, unsigned int *infoCount,
                                                                efmlProcessInfo_v1_t *infos);
static efmlReturn_v1_t (*efmlDeviceGetGraphicsRunningProcesses_v2)(efmlDevice_t device, unsigned int *infoCount,
                                                                efmlProcessInfo_v2_t *infos);
static efmlReturn_v1_t (*efmlDeviceGetGraphicsRunningProcesses_v3)(efmlDevice_t device, unsigned int *infoCount,
                                                                efmlProcessInfo_v3_t *infos);

static efmlReturn_v1_t (*efmlDeviceGetComputeRunningProcesses_v1)(efmlDevice_t device, unsigned int *infoCount,
                                                               efmlProcessInfo_v1_t *infos);
static efmlReturn_v1_t (*efmlDeviceGetComputeRunningProcesses_v2)(efmlDevice_t device, unsigned int *infoCount,
                                                               efmlProcessInfo_v2_t *infos);
static efmlReturn_v1_t (*efmlDeviceGetComputeRunningProcesses_v3)(efmlDevice_t device, unsigned int *infoCount,
                                                               efmlProcessInfo_v3_t *infos);

static efmlReturn_v1_t (*efmlDeviceGetMPSComputeRunningProcesses_v1)(efmlDevice_t device, unsigned int *infoCount,
                                                                  efmlProcessInfo_v1_t *infos);
static efmlReturn_v1_t (*efmlDeviceGetMPSComputeRunningProcesses_v2)(efmlDevice_t device, unsigned int *infoCount,
                                                                  efmlProcessInfo_v2_t *infos);
static efmlReturn_v1_t (*efmlDeviceGetMPSComputeRunningProcesses_v3)(efmlDevice_t device, unsigned int *infoCount,
                                                                  efmlProcessInfo_v3_t *infos);

// Common interface passing void*
static efmlReturn_v1_t (*efmlDeviceGetGraphicsRunningProcesses[4])(efmlDevice_t device, unsigned int *infoCount,
                                                                void *infos);
static efmlReturn_v1_t (*efmlDeviceGetComputeRunningProcesses[4])(efmlDevice_t device, unsigned int *infoCount,
                                                               void *infos);
static efmlReturn_v1_t (*efmlDeviceGetMPSComputeRunningProcesses[4])(efmlDevice_t device, unsigned int *infoCount,
                                                                  void *infos);

#define EFML_DEVICE_DRS_DISABLE 0x0
#define EFML_DEVICE_DRS_ENABLE 0x1
efmlReturn_v1_t (*efmlDeviceGetDrsMode)(efmlDevice_t device, unsigned int *currentMode, unsigned int *pendingMode);

static void *libefml_handle;

static efmlReturn_v1_t last_efml_return_status = EFML_SUCCESS;
static char didnt_call_gcuinfo_init[] = "The ENFLAME extraction has not been initialized, please call "
                                        "gcuinfo_enflame_init\n";
static const char *local_error_string = didnt_call_gcuinfo_init;

// Processes GCU Utilization

typedef struct {
  unsigned int pid;
  unsigned long long timeStamp;
  unsigned int sipUtil;
  unsigned int memUtil;
  unsigned int encUtil;
  unsigned int decUtil;
} efmlProcessUtilizationSample_t;

efmlReturn_v1_t (*efmlDeviceGetProcessUtilization)(efmlDevice_t device, efmlProcessUtilizationSample_t *utilization,
                                                unsigned int *processSamplesCount,
                                                unsigned long long lastSeenTimeStamp);

struct gcu_info_enflame {
  struct gpu_info base;
  struct list_head allocate_list;

  efmlDevice_t gcuhandle;
  bool isInDrsMode;
  unsigned long long last_utilization_timestamp;
};

static LIST_HEAD(allocations);

static bool gcuinfo_enflame_init(void);
static void gcuinfo_enflame_shutdown(void);
static const char *gcuinfo_enflame_last_error_string(void);
static bool gcuinfo_enflame_get_device_handles(struct list_head *devices, unsigned *count);
static void gcuinfo_enflame_populate_static_info(struct gpu_info *_gcu_info);
static void gcuinfo_enflame_refresh_dynamic_info(struct gpu_info *_gcu_info);
static void gcuinfo_enflame_get_running_processes(struct gpu_info *_gcu_info);

struct gpu_vendor gcu_vendor_enflame = {
    .init = gcuinfo_enflame_init,
    .shutdown = gcuinfo_enflame_shutdown,
    .last_error_string = gcuinfo_enflame_last_error_string,
    .get_device_handles = gcuinfo_enflame_get_device_handles,
    .populate_static_info = gcuinfo_enflame_populate_static_info,
    .refresh_dynamic_info = gcuinfo_enflame_refresh_dynamic_info,
    .refresh_running_processes = gcuinfo_enflame_get_running_processes,
    .name = "ENFLAME",
};

__attribute__((constructor)) static void init_extract_gcuinfo_enflame(void) { register_gpu_vendor(&gcu_vendor_enflame); }

/*
 *
 * This function loads the libefml.so shared object, initializes the
 * required function pointers and calls the enflame library initialization
 * function. Returns true if everything has been initialized successfully. If
 * false is returned, the cause of the error can be retrieved by calling the
 * function gcuinfo_enflame_last_error_string.
 *
 */
static bool gcuinfo_enflame_init(void) {

  libefml_handle = dlopen("libefml.so", RTLD_LAZY);
  if (!libefml_handle)
    libefml_handle = dlopen("libefml.so.1", RTLD_LAZY);
  if (!libefml_handle) {
    local_error_string = dlerror();
    return false;
  }

  // Default to last version
  efmlInit = dlsym(libefml_handle, "efmlInit_v2");
  if (!efmlInit)
    efmlInit = dlsym(libefml_handle, "efmlInit");
  if (!efmlInit)
    goto init_error_clean_exit;

  efmlShutdown = dlsym(libefml_handle, "efmlShutdown");
  if (!efmlShutdown)
    goto init_error_clean_exit;

  // Default to last version if available
  efmlDeviceGetCount = dlsym(libefml_handle, "efmlDeviceGetCount_v2");
  if (!efmlDeviceGetCount)
    efmlDeviceGetCount = dlsym(libefml_handle, "efmlDeviceGetCount");
  if (!efmlDeviceGetCount)
    goto init_error_clean_exit;

  efmlDeviceGetHandleByIndex = dlsym(libefml_handle, "efmlDeviceGetHandleByIndex_v2");
  if (!efmlDeviceGetHandleByIndex)
    efmlDeviceGetHandleByIndex = dlsym(libefml_handle, "efmlDeviceGetHandleByIndex");
  if (!efmlDeviceGetHandleByIndex)
    goto init_error_clean_exit;

  efmlErrorString = dlsym(libefml_handle, "efmlErrorString");
  if (!efmlErrorString)
    goto init_error_clean_exit;

  efmlDeviceGetName = dlsym(libefml_handle, "efmlDeviceGetName");
  if (!efmlDeviceGetName)
    goto init_error_clean_exit;

  efmlDeviceGetPciInfo = dlsym(libefml_handle, "efmlDeviceGetPciInfo_v3");
  if (!efmlDeviceGetPciInfo)
    efmlDeviceGetPciInfo = dlsym(libefml_handle, "efmlDeviceGetPciInfo_v2");
  if (!efmlDeviceGetPciInfo)
    efmlDeviceGetPciInfo = dlsym(libefml_handle, "efmlDeviceGetPciInfo");
  if (!efmlDeviceGetPciInfo)
    goto init_error_clean_exit;

  efmlDeviceGetMaxPcieLinkGeneration = dlsym(libefml_handle, "efmlDeviceGetMaxPcieLinkGeneration");
  if (!efmlDeviceGetMaxPcieLinkGeneration)
    goto init_error_clean_exit;

  efmlDeviceGetMaxPcieLinkWidth = dlsym(libefml_handle, "efmlDeviceGetMaxPcieLinkWidth");
  if (!efmlDeviceGetMaxPcieLinkWidth)
    goto init_error_clean_exit;

  efmlDeviceGetTemperatureThreshold = dlsym(libefml_handle, "efmlDeviceGetTemperatureThreshold");
  if (!efmlDeviceGetTemperatureThreshold)
    goto init_error_clean_exit;

  efmlDeviceGetClockInfo = dlsym(libefml_handle, "efmlDeviceGetClockInfo");
  if (!efmlDeviceGetClockInfo)
    goto init_error_clean_exit;

  efmlDeviceGetMaxClockInfo = dlsym(libefml_handle, "efmlDeviceGetMaxClockInfo");
  if (!efmlDeviceGetMaxClockInfo)
    goto init_error_clean_exit;

  efmlDeviceGetUtilizationRates = dlsym(libefml_handle, "efmlDeviceGetUtilizationRates");
  if (!efmlDeviceGetUtilizationRates)
    goto init_error_clean_exit;

  // Get v2 and fallback to v1
  efmlDeviceGetMemoryInfo_v2 = dlsym(libefml_handle, "efmlDeviceGetMemoryInfo_v2");
  efmlDeviceGetMemoryInfo = dlsym(libefml_handle, "efmlDeviceGetMemoryInfo");
  if (!efmlDeviceGetMemoryInfo_v2 && !efmlDeviceGetMemoryInfo)
    goto init_error_clean_exit;

  efmlDeviceGetCurrPcieLinkGeneration = dlsym(libefml_handle, "efmlDeviceGetCurrPcieLinkGeneration");
  if (!efmlDeviceGetCurrPcieLinkGeneration)
    goto init_error_clean_exit;

  efmlDeviceGetCurrPcieLinkWidth = dlsym(libefml_handle, "efmlDeviceGetCurrPcieLinkWidth");
  if (!efmlDeviceGetCurrPcieLinkWidth)
    goto init_error_clean_exit;

  efmlDeviceGetPcieThroughput = dlsym(libefml_handle, "efmlDeviceGetPcieThroughput");
  if (!efmlDeviceGetPcieThroughput)
    goto init_error_clean_exit;

  efmlDeviceGetFanSpeed = dlsym(libefml_handle, "efmlDeviceGetFanSpeed");
  if (!efmlDeviceGetFanSpeed)
    goto init_error_clean_exit;

  efmlDeviceGetTemperature = dlsym(libefml_handle, "efmlDeviceGetTemperature");
  if (!efmlDeviceGetTemperature)
    goto init_error_clean_exit;

  efmlDeviceGetPowerUsage = dlsym(libefml_handle, "efmlDeviceGetPowerUsage");
  if (!efmlDeviceGetPowerUsage)
    goto init_error_clean_exit;

  efmlDeviceGetEnforcedPowerLimit = dlsym(libefml_handle, "efmlDeviceGetEnforcedPowerLimit");
  if (!efmlDeviceGetEnforcedPowerLimit)
    goto init_error_clean_exit;

  efmlDeviceGetEncoderUtilization = dlsym(libefml_handle, "efmlDeviceGetEncoderUtilization");
  if (!efmlDeviceGetEncoderUtilization)
    goto init_error_clean_exit;

  efmlDeviceGetDecoderUtilization = dlsym(libefml_handle, "efmlDeviceGetDecoderUtilization");
  if (!efmlDeviceGetDecoderUtilization)
    goto init_error_clean_exit;

  efmlDeviceGetGraphicsRunningProcesses_v3 = dlsym(libefml_handle, "efmlDeviceGetGraphicsRunningProcesses_v3");
  efmlDeviceGetGraphicsRunningProcesses_v2 = dlsym(libefml_handle, "efmlDeviceGetGraphicsRunningProcesses_v2");
  efmlDeviceGetGraphicsRunningProcesses_v1 = dlsym(libefml_handle, "efmlDeviceGetGraphicsRunningProcesses");
  if (!efmlDeviceGetGraphicsRunningProcesses_v3 && !efmlDeviceGetGraphicsRunningProcesses_v2 &&
      !efmlDeviceGetGraphicsRunningProcesses_v1)
    goto init_error_clean_exit;

  efmlDeviceGetGraphicsRunningProcesses[1] =
      (efmlReturn_v1_t(*)(efmlDevice_t, unsigned int *, void *))efmlDeviceGetGraphicsRunningProcesses_v1;
  efmlDeviceGetGraphicsRunningProcesses[2] =
      (efmlReturn_v1_t(*)(efmlDevice_t, unsigned int *, void *))efmlDeviceGetGraphicsRunningProcesses_v2;
  efmlDeviceGetGraphicsRunningProcesses[3] =
      (efmlReturn_v1_t(*)(efmlDevice_t, unsigned int *, void *))efmlDeviceGetGraphicsRunningProcesses_v3;

  efmlDeviceGetComputeRunningProcesses_v3 = dlsym(libefml_handle, "efmlDeviceGetComputeRunningProcesses_v3");
  efmlDeviceGetComputeRunningProcesses_v2 = dlsym(libefml_handle, "efmlDeviceGetComputeRunningProcesses_v2");
  efmlDeviceGetComputeRunningProcesses_v1 = dlsym(libefml_handle, "efmlDeviceGetComputeRunningProcesses");
  if (!efmlDeviceGetComputeRunningProcesses_v3 && !efmlDeviceGetComputeRunningProcesses_v2 &&
      !efmlDeviceGetComputeRunningProcesses_v1)
    goto init_error_clean_exit;

  efmlDeviceGetComputeRunningProcesses[1] =
      (efmlReturn_v1_t(*)(efmlDevice_t, unsigned int *, void *))efmlDeviceGetComputeRunningProcesses_v1;
  efmlDeviceGetComputeRunningProcesses[2] =
      (efmlReturn_v1_t(*)(efmlDevice_t, unsigned int *, void *))efmlDeviceGetComputeRunningProcesses_v2;
  efmlDeviceGetComputeRunningProcesses[3] =
      (efmlReturn_v1_t(*)(efmlDevice_t, unsigned int *, void *))efmlDeviceGetComputeRunningProcesses_v3;

  efmlDeviceGetMPSComputeRunningProcesses_v3 = dlsym(libefml_handle, "efmlDeviceGetMPSComputeRunningProcesses_v3");
  efmlDeviceGetMPSComputeRunningProcesses_v2 = dlsym(libefml_handle, "efmlDeviceGetMPSComputeRunningProcesses_v2");
  efmlDeviceGetMPSComputeRunningProcesses_v1 = dlsym(libefml_handle, "efmlDeviceGetMPSComputeRunningProcesses");

  efmlDeviceGetMPSComputeRunningProcesses[1] =
      (efmlReturn_v1_t(*)(efmlDevice_t, unsigned int *, void *))efmlDeviceGetMPSComputeRunningProcesses_v1;
  efmlDeviceGetMPSComputeRunningProcesses[2] =
      (efmlReturn_v1_t(*)(efmlDevice_t, unsigned int *, void *))efmlDeviceGetMPSComputeRunningProcesses_v2;
  efmlDeviceGetMPSComputeRunningProcesses[3] =
      (efmlReturn_v1_t(*)(efmlDevice_t, unsigned int *, void *))efmlDeviceGetMPSComputeRunningProcesses_v3;

  efmlDeviceGetProcessUtilization = dlsym(libefml_handle, "efmlDeviceGetProcessUtilization");
  efmlDeviceGetDrsMode = dlsym(libefml_handle, "efmlDeviceGetDrsMode");

  last_efml_return_status = efmlInit();
  if (last_efml_return_status != EFML_SUCCESS) {
    return false;
  }
  local_error_string = NULL;

  return true;

init_error_clean_exit:
  dlclose(libefml_handle);
  libefml_handle = NULL;
  return false;
}

static void gcuinfo_enflame_shutdown(void) {
  if (libefml_handle) {
    efmlShutdown();
    dlclose(libefml_handle);
    libefml_handle = NULL;
    local_error_string = didnt_call_gcuinfo_init;
  }

  struct gcu_info_enflame *allocated, *tmp;

  list_for_each_entry_safe(allocated, tmp, &allocations, allocate_list) {
    list_del(&allocated->allocate_list);
    free(allocated);
  }
}

static const char *gcuinfo_enflame_last_error_string(void) {
  if (local_error_string) {
    return local_error_string;
  } else if (libefml_handle && efmlErrorString) {
    return efmlErrorString(last_efml_return_status);
  } else {
    return "An unanticipated error occurred while accessing ENFLAME GCU "
           "information\n";
  }
}

static bool gcuinfo_enflame_get_device_handles(struct list_head *devices, unsigned *count) {

  if (!libefml_handle)
    return false;

  unsigned num_devices;
  last_efml_return_status = efmlDeviceGetCount(&num_devices);
  if (last_efml_return_status != EFML_SUCCESS)
    return false;

  struct gcu_info_enflame *gcu_infos = calloc(num_devices, sizeof(*gcu_infos));
  if (!gcu_infos) {
    local_error_string = strerror(errno);
    return false;
  }

  list_add(&gcu_infos[0].allocate_list, &allocations);

  *count = 0;
  for (unsigned int i = 0; i < num_devices; ++i) {
    last_efml_return_status = efmlDeviceGetHandleByIndex(i, &gcu_infos[*count].gcuhandle);
    if (last_efml_return_status == EFML_SUCCESS) {
      gcu_infos[*count].base.vendor = &gcu_vendor_enflame;
      efmlPciInfo_t pciInfo;
      efmlReturn_v1_t pciInfoRet = efmlDeviceGetPciInfo(gcu_infos[*count].gcuhandle, &pciInfo);
      if (pciInfoRet == EFML_SUCCESS) {
        strncpy(gcu_infos[*count].base.pdev, pciInfo.busIdLegacy, PDEV_LEN);
        list_add_tail(&gcu_infos[*count].base.list, devices);
        *count += 1;
      }
    }
  }

  return true;
}

static void gcuinfo_enflame_populate_static_info(struct gpu_info *_gcu_info) {
  struct gcu_info_enflame *gcu_info = container_of(_gcu_info, struct gcu_info_enflame, base);
  struct gpuinfo_static_info *static_info = &gcu_info->base.static_info;
  efmlDevice_t device = gcu_info->gcuhandle;

  static_info->integrated_graphics = false;
  static_info->encode_decode_shared = false;
  RESET_ALL(static_info->valid);

  last_efml_return_status = efmlDeviceGetName(device, static_info->device_name, MAX_DEVICE_NAME);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);

  last_efml_return_status = efmlDeviceGetMaxPcieLinkGeneration(device, &static_info->max_pcie_gen);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_max_pcie_gen_valid, static_info->valid);

  last_efml_return_status = efmlDeviceGetMaxPcieLinkWidth(device, &static_info->max_pcie_link_width);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_max_pcie_link_width_valid, static_info->valid);

  last_efml_return_status = efmlDeviceGetTemperatureThreshold(device, EFML_TEMPERATURE_THRESHOLD_SHUTDOWN,
                                                              &static_info->temperature_shutdown_threshold);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_temperature_shutdown_threshold_valid, static_info->valid);

  last_efml_return_status = efmlDeviceGetTemperatureThreshold(device, EFML_TEMPERATURE_THRESHOLD_SLOWDOWN,
                                                              &static_info->temperature_slowdown_threshold);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_temperature_slowdown_threshold_valid, static_info->valid);
}

static void gcuinfo_enflame_refresh_dynamic_info(struct gpu_info *_gcu_info) {
  struct gcu_info_enflame *gcu_info = container_of(_gcu_info, struct gcu_info_enflame, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gcu_info->base.dynamic_info;
  efmlDevice_t device = gcu_info->gcuhandle;

  bool graphics_clock_valid = false;
  unsigned graphics_clock;
  bool sm_clock_valid = false;
  unsigned sm_clock;
  efmlClockType_t getMaxClockFrom = EFML_CLOCK_GRAPHICS;

  RESET_ALL(dynamic_info->valid);

  // GCU current speed
  last_efml_return_status = efmlDeviceGetClockInfo(device, EFML_CLOCK_GRAPHICS, &graphics_clock);
  graphics_clock_valid = last_efml_return_status == EFML_SUCCESS;

  last_efml_return_status = efmlDeviceGetClockInfo(device, EFML_CLOCK_SM, &sm_clock);
  sm_clock_valid = last_efml_return_status == EFML_SUCCESS;

  if (graphics_clock_valid && sm_clock_valid && graphics_clock < sm_clock) {
    getMaxClockFrom = EFML_CLOCK_SM;
  } else if (!graphics_clock_valid && sm_clock_valid) {
    getMaxClockFrom = EFML_CLOCK_SM;
  }

  if (getMaxClockFrom == EFML_CLOCK_GRAPHICS && graphics_clock_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, graphics_clock);
  }
  if (getMaxClockFrom == EFML_CLOCK_SM && sm_clock_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, sm_clock);
  }

  // GCU max speed
  last_efml_return_status = efmlDeviceGetMaxClockInfo(device, getMaxClockFrom, &dynamic_info->gpu_clock_speed_max);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_gpu_clock_speed_max_valid, dynamic_info->valid);

  // Memory current speed
  last_efml_return_status = efmlDeviceGetClockInfo(device, EFML_CLOCK_MEM, &dynamic_info->mem_clock_speed);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_mem_clock_speed_valid, dynamic_info->valid);

  // Memory max speed
  last_efml_return_status = efmlDeviceGetMaxClockInfo(device, EFML_CLOCK_MEM, &dynamic_info->mem_clock_speed_max);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_mem_clock_speed_max_valid, dynamic_info->valid);

  // CPU and Memory utilization rates
  efmlUtilization_t utilization_percentages;
  last_efml_return_status = efmlDeviceGetUtilizationRates(device, &utilization_percentages);
  if (last_efml_return_status == EFML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, utilization_percentages.gcu);
  }

  // Encoder utilization rate
  unsigned ignored_period;
  last_efml_return_status = efmlDeviceGetEncoderUtilization(device, &dynamic_info->encoder_rate, &ignored_period);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_encoder_rate_valid, dynamic_info->valid);

  // Decoder utilization rate
  last_efml_return_status = efmlDeviceGetDecoderUtilization(device, &dynamic_info->decoder_rate, &ignored_period);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_decoder_rate_valid, dynamic_info->valid);

  // Device memory info (total,used,free)
  bool got_meminfo = false;
  bool has_unified_memory = false;

  if (efmlDeviceGetMemoryInfo_v2) {
    efmlMemory_v2_t memory_info;
    memory_info.version = 0x02000028;
    last_efml_return_status = efmlDeviceGetMemoryInfo_v2(device, &memory_info);
    if (last_efml_return_status == EFML_SUCCESS) {
      // Check if this is a unified memory GCU (total == 0 indicates unified memory)
      if (memory_info.total == 0) {
        has_unified_memory = true;
      } else {
        got_meminfo = true;
        SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, memory_info.total);
        SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, memory_info.used);
        SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, memory_info.free);
        SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, memory_info.used * 100 / memory_info.total);
      }
    } else {
      // Memory query failed - likely unified memory GCU (error code 13 = NOT_SUPPORTED)
      has_unified_memory = true;
    }
  }
  if (!got_meminfo && !has_unified_memory && efmlDeviceGetMemoryInfo) {
    efmlMemory_v1_t memory_info;
    last_efml_return_status = efmlDeviceGetMemoryInfo(device, &memory_info);
    if (last_efml_return_status == EFML_SUCCESS) {
      // Check if this is a unified memory GCU (total == 0 indicates unified memory)
      if (memory_info.total == 0) {
        has_unified_memory = true;
      } else {
        SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, memory_info.total);
        SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, memory_info.used);
        SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, memory_info.free);
        SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, memory_info.used * 100 / memory_info.total);
      }
    } else {
      // Memory query failed - likely unified memory GCU
      has_unified_memory = true;
    }
  }

  // Handle unified memory GCUs - query actual GCU allocations and system memory
  if (has_unified_memory) {
    // Get actual GCU memory usage from running processes
    unsigned long long gcu_used_memory = 0;

    // Sum up memory used by compute processes
    if (efmlDeviceGetComputeRunningProcesses_v3 || efmlDeviceGetComputeRunningProcesses_v2 ||
        efmlDeviceGetComputeRunningProcesses_v1) {
      unsigned int process_count = 0;
      efmlReturn_v1_t (*getProcesses)(efmlDevice_t, unsigned int *, void *) = NULL;
      size_t process_info_size = 0;

      // Choose the latest available version
      if (efmlDeviceGetComputeRunningProcesses_v3) {
        getProcesses = efmlDeviceGetComputeRunningProcesses[3];
        process_info_size = sizeof(efmlProcessInfo_v3_t);
      } else if (efmlDeviceGetComputeRunningProcesses_v2) {
        getProcesses = efmlDeviceGetComputeRunningProcesses[2];
        process_info_size = sizeof(efmlProcessInfo_v2_t);
      } else {
        getProcesses = efmlDeviceGetComputeRunningProcesses[1];
        process_info_size = sizeof(efmlProcessInfo_v1_t);
      }

      // First call to get count
      efmlReturn_v1_t ret = getProcesses(device, &process_count, NULL);
      if (ret == EFML_SUCCESS || ret == EFML_ERROR_INSUFFICIENT_SIZE) {
        if (process_count > 0) {
          void *process_infos = malloc(process_count * process_info_size);
          if (process_infos) {
            ret = getProcesses(device, &process_count, process_infos);
            if (ret == EFML_SUCCESS) {
              // Sum up memory from all processes
              for (unsigned int i = 0; i < process_count; i++) {
                if (efmlDeviceGetComputeRunningProcesses_v3) {
                  gcu_used_memory += ((efmlProcessInfo_v3_t *)process_infos)[i].usedGcuMemory;
                } else if (efmlDeviceGetComputeRunningProcesses_v2) {
                  gcu_used_memory += ((efmlProcessInfo_v2_t *)process_infos)[i].usedGcuMemory;
                } else {
                  gcu_used_memory += ((efmlProcessInfo_v1_t *)process_infos)[i].usedGcuMemory;
                }
              }
            }
            free(process_infos);
          }
        }
      }
    }

    // Also check graphics processes
    if (efmlDeviceGetGraphicsRunningProcesses_v3 || efmlDeviceGetGraphicsRunningProcesses_v2 ||
        efmlDeviceGetGraphicsRunningProcesses_v1) {
      unsigned int process_count = 0;
      efmlReturn_v1_t (*getProcesses)(efmlDevice_t, unsigned int *, void *) = NULL;
      size_t process_info_size = 0;

      if (efmlDeviceGetGraphicsRunningProcesses_v3) {
        getProcesses = efmlDeviceGetGraphicsRunningProcesses[3];
        process_info_size = sizeof(efmlProcessInfo_v3_t);
      } else if (efmlDeviceGetGraphicsRunningProcesses_v2) {
        getProcesses = efmlDeviceGetGraphicsRunningProcesses[2];
        process_info_size = sizeof(efmlProcessInfo_v2_t);
      } else {
        getProcesses = efmlDeviceGetGraphicsRunningProcesses[1];
        process_info_size = sizeof(efmlProcessInfo_v1_t);
      }

      efmlReturn_v1_t ret = getProcesses(device, &process_count, NULL);
      if (ret == EFML_SUCCESS || ret == EFML_ERROR_INSUFFICIENT_SIZE) {
        if (process_count > 0) {
          void *process_infos = malloc(process_count * process_info_size);
          if (process_infos) {
            ret = getProcesses(device, &process_count, process_infos);
            if (ret == EFML_SUCCESS) {
              for (unsigned int i = 0; i < process_count; i++) {
                if (efmlDeviceGetGraphicsRunningProcesses_v3) {
                  gcu_used_memory += ((efmlProcessInfo_v3_t *)process_infos)[i].usedGcuMemory;
                } else if (efmlDeviceGetGraphicsRunningProcesses_v2) {
                  gcu_used_memory += ((efmlProcessInfo_v2_t *)process_infos)[i].usedGcuMemory;
                } else {
                  gcu_used_memory += ((efmlProcessInfo_v1_t *)process_infos)[i].usedGcuMemory;
                }
              }
            }
            free(process_infos);
          }
        }
      }
    }

    // Read MemAvailable from /proc/meminfo for available memory
    FILE *meminfo = fopen("/proc/meminfo", "r");
    if (meminfo) {
      unsigned long long available_ram = 0;
      char line[256];

      while (fgets(line, sizeof(line), meminfo)) {
        if (sscanf(line, "MemAvailable: %llu kB", &available_ram) == 1) {
          available_ram *= 1024; // Convert KB to bytes
          break;
        }
      }
      fclose(meminfo);

      if (available_ram > 0) {
        unsigned long long total_memory = gcu_used_memory + available_ram;

        SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, total_memory);
        SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, gcu_used_memory);
        SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, available_ram);
        if (total_memory > 0) {
          SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, gcu_used_memory * 100 / total_memory);
        }
      }
    }
  }

  // Pcie generation used by the device
  last_efml_return_status = efmlDeviceGetCurrPcieLinkGeneration(device, &dynamic_info->pcie_link_gen);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_pcie_link_gen_valid, dynamic_info->valid);

  // Pcie width used by the device
  last_efml_return_status = efmlDeviceGetCurrPcieLinkWidth(device, &dynamic_info->pcie_link_width);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_pcie_link_width_valid, dynamic_info->valid);

  // Pcie reception throughput
  last_efml_return_status = efmlDeviceGetPcieThroughput(device, EFML_PCIE_UTIL_RX_BYTES, &dynamic_info->pcie_rx);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_pcie_rx_valid, dynamic_info->valid);

  // Pcie transmission throughput
  last_efml_return_status = efmlDeviceGetPcieThroughput(device, EFML_PCIE_UTIL_TX_BYTES, &dynamic_info->pcie_tx);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_pcie_tx_valid, dynamic_info->valid);

  // Fan speed
  last_efml_return_status = efmlDeviceGetFanSpeed(device, &dynamic_info->fan_speed);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_fan_speed_valid, dynamic_info->valid);

  // GCU temperature
  last_efml_return_status = efmlDeviceGetTemperature(device, EFML_TEMPERATURE_GCU, &dynamic_info->gpu_temp);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_gpu_temp_valid, dynamic_info->valid);

  // Device power usage
  last_efml_return_status = efmlDeviceGetPowerUsage(device, &dynamic_info->power_draw);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_power_draw_valid, dynamic_info->valid);

  // Maximum enforced power usage
  last_efml_return_status = efmlDeviceGetEnforcedPowerLimit(device, &dynamic_info->power_draw_max);
  if (last_efml_return_status == EFML_SUCCESS)
    SET_VALID(gpuinfo_power_draw_max_valid, dynamic_info->valid);

  // DRS mode
  if (efmlDeviceGetDrsMode) {
    unsigned currentMode, pendingMode;
    last_efml_return_status = efmlDeviceGetDrsMode(device, &currentMode, &pendingMode);
    if (last_efml_return_status == EFML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, multi_instance_mode, currentMode == EFML_DEVICE_DRS_ENABLE);
    }
  }
}

static void gcuinfo_enflame_get_process_utilization(struct gcu_info_enflame *gcu_info, unsigned num_processes_recovered,
                                                   struct gpu_process processes[num_processes_recovered]) {
  efmlDevice_t device = gcu_info->gcuhandle;

  if (num_processes_recovered && efmlDeviceGetProcessUtilization) {
    unsigned samples_count = 0;
    efmlReturn_v1_t retval =
        efmlDeviceGetProcessUtilization(device, NULL, &samples_count, gcu_info->last_utilization_timestamp);
    if (retval != EFML_ERROR_INSUFFICIENT_SIZE)
      return;
    efmlProcessUtilizationSample_t *samples = malloc(samples_count * sizeof(*samples));
    retval = efmlDeviceGetProcessUtilization(device, samples, &samples_count, gcu_info->last_utilization_timestamp);
    if (retval != EFML_SUCCESS) {
      free(samples);
      return;
    }
    unsigned long long newest_timestamp_candidate = gcu_info->last_utilization_timestamp;
    for (unsigned i = 0; i < samples_count; ++i) {
      bool process_matched = false;
      for (unsigned j = 0; !process_matched && j < num_processes_recovered; ++j) {
        if ((pid_t)samples[i].pid == processes[j].pid && samples[i].sipUtil <= 100 && samples[i].encUtil <= 100 &&
            samples[i].decUtil <= 100 && samples[i].timeStamp > gcu_info->last_utilization_timestamp) {
          // Collect the largest valid timestamp for this device to filter out
          // the samples during the next call to the function
          // efmlDeviceGetProcessUtilization
          if (samples[i].timeStamp > newest_timestamp_candidate)
            newest_timestamp_candidate = samples[i].timeStamp;

          SET_GPUINFO_PROCESS(&processes[j], gpu_usage, samples[i].sipUtil);
          SET_GPUINFO_PROCESS(&processes[j], encode_usage, samples[i].encUtil);
          SET_GPUINFO_PROCESS(&processes[j], decode_usage, samples[i].decUtil);
          process_matched = true;
        }
      }
    }
    gcu_info->last_utilization_timestamp = newest_timestamp_candidate;
    free(samples);
  }
  // Mark the ones w/o update since last sample period to 0% usage
  for (unsigned j = 0; j < num_processes_recovered; ++j) {
    if (!IS_VALID(gpuinfo_process_gpu_usage_valid, processes[j].valid))
      SET_GPUINFO_PROCESS(&processes[j], gpu_usage, 0);
    if (!IS_VALID(gpuinfo_process_encode_usage_valid, processes[j].valid))
      SET_GPUINFO_PROCESS(&processes[j], encode_usage, 0);
    if (!IS_VALID(gpuinfo_process_decode_usage_valid, processes[j].valid))
      SET_GPUINFO_PROCESS(&processes[j], decode_usage, 0);
  }
}

static void gcuinfo_enflame_get_running_processes(struct gpu_info *_gcu_info) {
  struct gcu_info_enflame *gcu_info = container_of(_gcu_info, struct gcu_info_enflame, base);
  efmlDevice_t device = gcu_info->gcuhandle;
  bool validProcessGathering = false;
  for (unsigned version = 3; !validProcessGathering && version > 0; version--) {
    // Get the size of the actual function being used
    size_t sizeof_efmlProcessInfo;
    switch (version) {
    case 3:
      sizeof_efmlProcessInfo = sizeof(efmlProcessInfo_v3_t);
      break;
    case 2:
      sizeof_efmlProcessInfo = sizeof(efmlProcessInfo_v2_t);
      break;
    default:
      sizeof_efmlProcessInfo = sizeof(efmlProcessInfo_v1_t);
      break;
    }

    _gcu_info->processes_count = 0;
    static size_t array_size = 0;
    static char *retrieved_infos = NULL;
    unsigned graphical_count = 0, compute_count = 0, recovered_count;
    if (efmlDeviceGetGraphicsRunningProcesses[version]) {
    retry_query_graphical:
      recovered_count = array_size;
      last_efml_return_status =
          efmlDeviceGetGraphicsRunningProcesses[version](device, &recovered_count, retrieved_infos);
      if (last_efml_return_status == EFML_ERROR_INSUFFICIENT_SIZE) {
        array_size += COMMON_PROCESS_LINEAR_REALLOC_INC;
        retrieved_infos = reallocarray(retrieved_infos, array_size, sizeof_efmlProcessInfo);
        if (!retrieved_infos) {
          perror("Could not re-allocate memory: ");
          exit(EXIT_FAILURE);
        }
        goto retry_query_graphical;
      }
      if (last_efml_return_status == EFML_SUCCESS) {
        validProcessGathering = true;
        graphical_count = recovered_count;
      }
    }

    if (efmlDeviceGetComputeRunningProcesses[version]) {
    retry_query_compute:
      recovered_count = array_size - graphical_count;
      last_efml_return_status = efmlDeviceGetComputeRunningProcesses[version](
          device, &recovered_count, retrieved_infos + graphical_count * sizeof_efmlProcessInfo);
      if (last_efml_return_status == EFML_ERROR_INSUFFICIENT_SIZE) {
        array_size += COMMON_PROCESS_LINEAR_REALLOC_INC;
        retrieved_infos = reallocarray(retrieved_infos, array_size, sizeof_efmlProcessInfo);
        if (!retrieved_infos) {
          perror("Could not re-allocate memory: ");
          exit(EXIT_FAILURE);
        }
        goto retry_query_compute;
      }
      if (last_efml_return_status == EFML_SUCCESS) {
        validProcessGathering = true;
        compute_count = recovered_count;
      }
    }

    if (efmlDeviceGetMPSComputeRunningProcesses[version]) {
    retry_query_compute_MPS:
      recovered_count = array_size - graphical_count - compute_count;
      last_efml_return_status = efmlDeviceGetMPSComputeRunningProcesses[version](
          device, &recovered_count, retrieved_infos + (graphical_count + compute_count) * sizeof_efmlProcessInfo);
      if (last_efml_return_status == EFML_ERROR_INSUFFICIENT_SIZE) {
        array_size += COMMON_PROCESS_LINEAR_REALLOC_INC;
        retrieved_infos = reallocarray(retrieved_infos, array_size, sizeof_efmlProcessInfo);
        if (!retrieved_infos) {
          perror("Could not re-allocate memory: ");
          exit(EXIT_FAILURE);
        }
        goto retry_query_compute_MPS;
      }
      if (last_efml_return_status == EFML_SUCCESS) {
        validProcessGathering = true;
        compute_count += recovered_count;
      }
    }

    if (!validProcessGathering)
      continue;

    _gcu_info->processes_count = graphical_count + compute_count;
    if (_gcu_info->processes_count > 0) {
      if (_gcu_info->processes_count > _gcu_info->processes_array_size) {
        _gcu_info->processes_array_size = _gcu_info->processes_count + COMMON_PROCESS_LINEAR_REALLOC_INC;
        _gcu_info->processes =
            reallocarray(_gcu_info->processes, _gcu_info->processes_array_size, sizeof(*_gcu_info->processes));
        if (!_gcu_info->processes) {
          perror("Could not allocate memory: ");
          exit(EXIT_FAILURE);
        }
      }
      memset(_gcu_info->processes, 0, _gcu_info->processes_count * sizeof(*_gcu_info->processes));
      for (unsigned i = 0; i < graphical_count + compute_count; ++i) {
        if (i < graphical_count)
          _gcu_info->processes[i].type = gpu_process_graphical;
        else
          _gcu_info->processes[i].type = gpu_process_compute;
        switch (version) {
        case 2: {
          efmlProcessInfo_v2_t *pinfo = (efmlProcessInfo_v2_t *)retrieved_infos;
          _gcu_info->processes[i].pid = pinfo[i].pid;
          _gcu_info->processes[i].gpu_memory_usage = pinfo[i].usedGcuMemory;
        } break;
        case 3: {
          efmlProcessInfo_v3_t *pinfo = (efmlProcessInfo_v3_t *)retrieved_infos;
          _gcu_info->processes[i].pid = pinfo[i].pid;
          _gcu_info->processes[i].gpu_memory_usage = pinfo[i].usedGcuMemory;
        } break;
        default: {
          efmlProcessInfo_v1_t *pinfo = (efmlProcessInfo_v1_t *)retrieved_infos;
          _gcu_info->processes[i].pid = pinfo[i].pid;
          _gcu_info->processes[i].gpu_memory_usage = pinfo[i].usedGcuMemory;
        } break;
        }
        SET_VALID(gpuinfo_process_gpu_memory_usage_valid, _gcu_info->processes[i].valid);
      }
    }
  }
  // If the GCU is in DRS mode; process utilization is not supported
  if (!gcu_info->base.dynamic_info.multi_instance_mode)
    gcuinfo_enflame_get_process_utilization(gcu_info, _gcu_info->processes_count, _gcu_info->processes);
}
