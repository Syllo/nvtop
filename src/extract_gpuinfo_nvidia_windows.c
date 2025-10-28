/*
 * Windows-specific NVIDIA GPU information extraction using NVML
 * Copyright (C) 2025
 *
 * This file is part of Nvtop.
 */

#include "list.h"
#include "nvtop/extract_gpuinfo.h"
#include "nvtop/extract_gpuinfo_common.h"
#include <stdio.h>
#include <string.h>
#include <windows.h>

#ifdef NVML_SUPPORT

#include <nvml.h>

// Windows NVIDIA GPU info structure extending the base gpu_info
struct gpu_info_nvidia {
  struct gpu_info base;
  nvmlDevice_t gpuhandle;
  struct list_head allocate_list; // For tracking allocations
};

static LIST_HEAD(allocations);

// Forward declaration
static struct gpu_vendor gpu_vendor_nvidia;

// NVML function pointers
typedef nvmlReturn_t (*nvmlInit_v2_t)(void);
typedef nvmlReturn_t (*nvmlShutdown_t)(void);
typedef nvmlReturn_t (*nvmlDeviceGetCount_v2_t)(unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetHandleByIndex_v2_t)(unsigned int, nvmlDevice_t *);
typedef nvmlReturn_t (*nvmlDeviceGetName_t)(nvmlDevice_t, char *, unsigned int);
typedef nvmlReturn_t (*nvmlDeviceGetMemoryInfo_t)(nvmlDevice_t, nvmlMemory_t *);
typedef nvmlReturn_t (*nvmlDeviceGetUtilizationRates_t)(nvmlDevice_t, nvmlUtilization_t *);
typedef nvmlReturn_t (*nvmlDeviceGetTemperature_t)(nvmlDevice_t, nvmlTemperatureSensors_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetPowerUsage_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetClockInfo_t)(nvmlDevice_t, nvmlClockType_t, unsigned int *);

static HMODULE nvmlHandle = NULL;
static nvmlInit_v2_t nvmlInit_v2_ptr = NULL;
static nvmlShutdown_t nvmlShutdown_ptr = NULL;
static nvmlDeviceGetCount_v2_t nvmlDeviceGetCount_v2_ptr = NULL;
static nvmlDeviceGetHandleByIndex_v2_t nvmlDeviceGetHandleByIndex_v2_ptr = NULL;
static nvmlDeviceGetName_t nvmlDeviceGetName_ptr = NULL;
static nvmlDeviceGetMemoryInfo_t nvmlDeviceGetMemoryInfo_ptr = NULL;
static nvmlDeviceGetUtilizationRates_t nvmlDeviceGetUtilizationRates_ptr = NULL;
static nvmlDeviceGetTemperature_t nvmlDeviceGetTemperature_ptr = NULL;
static nvmlDeviceGetPowerUsage_t nvmlDeviceGetPowerUsage_ptr = NULL;
static nvmlDeviceGetClockInfo_t nvmlDeviceGetClockInfo_ptr = NULL;

static bool load_nvml_library(void) {
  // Try to load NVML DLL
  nvmlHandle = LoadLibraryA("nvml.dll");
  if (!nvmlHandle) {
    fprintf(stderr, "Failed to load nvml.dll\n");
    return false;
  }

  // Load function pointers
  nvmlInit_v2_ptr = (nvmlInit_v2_t)GetProcAddress(nvmlHandle, "nvmlInit_v2");
  nvmlShutdown_ptr = (nvmlShutdown_t)GetProcAddress(nvmlHandle, "nvmlShutdown");
  nvmlDeviceGetCount_v2_ptr = (nvmlDeviceGetCount_v2_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetCount_v2");
  nvmlDeviceGetHandleByIndex_v2_ptr =
      (nvmlDeviceGetHandleByIndex_v2_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetHandleByIndex_v2");
  nvmlDeviceGetName_ptr = (nvmlDeviceGetName_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetName");
  nvmlDeviceGetMemoryInfo_ptr = (nvmlDeviceGetMemoryInfo_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetMemoryInfo");
  nvmlDeviceGetUtilizationRates_ptr =
      (nvmlDeviceGetUtilizationRates_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetUtilizationRates");
  nvmlDeviceGetTemperature_ptr = (nvmlDeviceGetTemperature_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetTemperature");
  nvmlDeviceGetPowerUsage_ptr = (nvmlDeviceGetPowerUsage_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetPowerUsage");
  nvmlDeviceGetClockInfo_ptr = (nvmlDeviceGetClockInfo_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetClockInfo");

  if (!nvmlInit_v2_ptr || !nvmlShutdown_ptr || !nvmlDeviceGetCount_v2_ptr) {
    fprintf(stderr, "Failed to load required NVML functions\n");
    FreeLibrary(nvmlHandle);
    nvmlHandle = NULL;
    return false;
  }

  return true;
}

bool gpuinfo_nvidia_init(void) {
  if (!load_nvml_library()) {
    return false;
  }

  nvmlReturn_t result = nvmlInit_v2_ptr();
  if (result != NVML_SUCCESS) {
    fprintf(stderr, "Failed to initialize NVML: %d\n", result);
    FreeLibrary(nvmlHandle);
    nvmlHandle = NULL;
    return false;
  }

  return true;
}

void gpuinfo_nvidia_shutdown(void) {
  if (nvmlShutdown_ptr) {
    nvmlShutdown_ptr();
  }
  if (nvmlHandle) {
    FreeLibrary(nvmlHandle);
    nvmlHandle = NULL;
  }
}

static bool gpuinfo_nvidia_get_device_handles(struct list_head *devices, unsigned *count) {
  unsigned int device_count = 0;
  if (nvmlDeviceGetCount_v2_ptr(&device_count) != NVML_SUCCESS) {
    return false;
  }

  struct gpu_info_nvidia *gpu_infos = calloc(device_count, sizeof(*gpu_infos));
  if (!gpu_infos) {
    return false;
  }

  list_add(&gpu_infos[0].allocate_list, &allocations);

  *count = 0;
  for (unsigned i = 0; i < device_count; i++) {
    if (nvmlDeviceGetHandleByIndex_v2_ptr(i, &gpu_infos[*count].gpuhandle) == NVML_SUCCESS) {
      gpu_infos[*count].base.vendor = &gpu_vendor_nvidia;
      list_add_tail(&gpu_infos[*count].base.list, devices);
      *count += 1;
    }
  }

  return true;
}

static void gpuinfo_nvidia_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_nvidia *gpu_info = container_of(_gpu_info, struct gpu_info_nvidia, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
  nvmlDevice_t device = gpu_info->gpuhandle;

  RESET_ALL(static_info->valid);

  // Get device name
  if (nvmlDeviceGetName_ptr &&
      nvmlDeviceGetName_ptr(device, static_info->device_name, MAX_DEVICE_NAME) == NVML_SUCCESS) {
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
  }
}

static void gpuinfo_nvidia_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_nvidia *gpu_info = container_of(_gpu_info, struct gpu_info_nvidia, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  nvmlDevice_t device = gpu_info->gpuhandle;

  RESET_ALL(dynamic_info->valid);

  // Get utilization rates
  if (nvmlDeviceGetUtilizationRates_ptr) {
    nvmlUtilization_t util;
    if (nvmlDeviceGetUtilizationRates_ptr(device, &util) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, util.gpu);
      SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, util.memory);
    }
  }

  // Get memory info
  if (nvmlDeviceGetMemoryInfo_ptr) {
    nvmlMemory_t mem;
    if (nvmlDeviceGetMemoryInfo_ptr(device, &mem) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, mem.used);
      SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, mem.free);
    }
  }

  // Get temperature
  if (nvmlDeviceGetTemperature_ptr) {
    unsigned int temp;
    if (nvmlDeviceGetTemperature_ptr(device, NVML_TEMPERATURE_GPU, &temp) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, temp);
    }
  }

  // Get power usage
  if (nvmlDeviceGetPowerUsage_ptr) {
    unsigned int power;
    if (nvmlDeviceGetPowerUsage_ptr(device, &power) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, power / 1000); // mW to W
    }
  }

  // Get clock speeds
  if (nvmlDeviceGetClockInfo_ptr) {
    unsigned int clock;
    if (nvmlDeviceGetClockInfo_ptr(device, NVML_CLOCK_GRAPHICS, &clock) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, clock);
    }
    if (nvmlDeviceGetClockInfo_ptr(device, NVML_CLOCK_MEM, &clock) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, mem_clock_speed, clock);
    }
  }
}

static void gpuinfo_nvidia_get_running_processes(struct gpu_info *_gpu_info) {
  // Process enumeration not implemented yet in Windows version
  _gpu_info->processes_count = 0;
}

static struct gpu_vendor gpu_vendor_nvidia = {
    .name = "NVIDIA",
    .init = gpuinfo_nvidia_init,
    .shutdown = gpuinfo_nvidia_shutdown,
    .get_device_handles = gpuinfo_nvidia_get_device_handles,
    .populate_static_info = gpuinfo_nvidia_populate_static_info,
    .refresh_dynamic_info = gpuinfo_nvidia_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_nvidia_get_running_processes,
};

__attribute__((constructor)) static void init_extract_gpuinfo_nvidia_windows(void) {
  register_gpu_vendor(&gpu_vendor_nvidia);
}

#else

bool gpuinfo_nvidia_init(void) { return false; }

void gpuinfo_nvidia_shutdown(void) {}

unsigned gpuinfo_nvidia_get_device_count(void) { return 0; }

#endif // NVML_SUPPORT
