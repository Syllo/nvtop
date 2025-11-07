/*
 * Windows-specific NVIDIA GPU information extraction using NVML
 * Copyright (C) 2025
 *
 * This file is part of Nvtop.
 */

#include "list.h"
#include "nvtop/extract_gpuinfo.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/get_process_info.h"
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
typedef nvmlReturn_t (*nvmlDeviceGetPowerManagementLimit_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetPowerManagementLimitConstraints_t)(nvmlDevice_t, unsigned int *, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetClockInfo_t)(nvmlDevice_t, nvmlClockType_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetFanSpeed_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetNumFans_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetFanSpeed_v2_t)(nvmlDevice_t, unsigned int, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetComputeRunningProcesses_t)(nvmlDevice_t, unsigned int *, nvmlProcessInfo_t *);
typedef nvmlReturn_t (*nvmlDeviceGetGraphicsRunningProcesses_t)(nvmlDevice_t, unsigned int *, nvmlProcessInfo_t *);
typedef nvmlReturn_t (*nvmlDeviceGetPciInfo_v3_t)(nvmlDevice_t, nvmlPciInfo_t *);
typedef nvmlReturn_t (*nvmlDeviceGetMaxPcieLinkGeneration_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetMaxPcieLinkWidth_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetCurrPcieLinkGeneration_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetCurrPcieLinkWidth_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetPcieThroughput_t)(nvmlDevice_t, nvmlPcieUtilCounter_t, unsigned int *);

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
static nvmlDeviceGetPowerManagementLimit_t nvmlDeviceGetPowerManagementLimit_ptr = NULL;
static nvmlDeviceGetPowerManagementLimitConstraints_t nvmlDeviceGetPowerManagementLimitConstraints_ptr = NULL;
static nvmlDeviceGetClockInfo_t nvmlDeviceGetClockInfo_ptr = NULL;
static nvmlDeviceGetFanSpeed_t nvmlDeviceGetFanSpeed_ptr = NULL;
static nvmlDeviceGetNumFans_t nvmlDeviceGetNumFans_ptr = NULL;
static nvmlDeviceGetFanSpeed_v2_t nvmlDeviceGetFanSpeed_v2_ptr = NULL;
static nvmlDeviceGetComputeRunningProcesses_t nvmlDeviceGetComputeRunningProcesses_ptr = NULL;
static nvmlDeviceGetGraphicsRunningProcesses_t nvmlDeviceGetGraphicsRunningProcesses_ptr = NULL;
static nvmlDeviceGetPciInfo_v3_t nvmlDeviceGetPciInfo_v3_ptr = NULL;
static nvmlDeviceGetMaxPcieLinkGeneration_t nvmlDeviceGetMaxPcieLinkGeneration_ptr = NULL;
static nvmlDeviceGetMaxPcieLinkWidth_t nvmlDeviceGetMaxPcieLinkWidth_ptr = NULL;
static nvmlDeviceGetCurrPcieLinkGeneration_t nvmlDeviceGetCurrPcieLinkGeneration_ptr = NULL;
static nvmlDeviceGetCurrPcieLinkWidth_t nvmlDeviceGetCurrPcieLinkWidth_ptr = NULL;
static nvmlDeviceGetPcieThroughput_t nvmlDeviceGetPcieThroughput_ptr = NULL;

static bool load_nvml_library(void) {
  // Secure DLL loading: Use full path to prevent DLL hijacking
  // Try system path first (fastest - usually 99% success rate)
  nvmlHandle = LoadLibraryExA("nvml.dll", NULL, LOAD_LIBRARY_SEARCH_SYSTEM32);

  if (!nvmlHandle) {
    // Fast fallback to common NVIDIA locations
    const char *searchPaths[] = {"C:\\Windows\\System32\\nvml.dll",
                                 "C:\\Program Files\\NVIDIA Corporation\\NVSMI\\nvml.dll", NULL};
    for (int i = 0; searchPaths[i] != NULL; i++) {
      nvmlHandle = LoadLibraryA(searchPaths[i]);
      if (nvmlHandle)
        break;
    }
  }

  if (!nvmlHandle) {
    return false;
  }

  // Load critical function pointers first for fast-fail
  nvmlInit_v2_ptr = (nvmlInit_v2_t)GetProcAddress(nvmlHandle, "nvmlInit_v2");
  nvmlShutdown_ptr = (nvmlShutdown_t)GetProcAddress(nvmlHandle, "nvmlShutdown");
  nvmlDeviceGetCount_v2_ptr = (nvmlDeviceGetCount_v2_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetCount_v2");

  if (!nvmlInit_v2_ptr || !nvmlShutdown_ptr || !nvmlDeviceGetCount_v2_ptr) {
    FreeLibrary(nvmlHandle);
    nvmlHandle = NULL;
    return false;
  }

  // Load remaining function pointers (bulk load for efficiency)
  nvmlDeviceGetHandleByIndex_v2_ptr =
      (nvmlDeviceGetHandleByIndex_v2_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetHandleByIndex_v2");
  nvmlDeviceGetName_ptr = (nvmlDeviceGetName_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetName");
  nvmlDeviceGetMemoryInfo_ptr = (nvmlDeviceGetMemoryInfo_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetMemoryInfo");
  nvmlDeviceGetUtilizationRates_ptr =
      (nvmlDeviceGetUtilizationRates_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetUtilizationRates");
  nvmlDeviceGetTemperature_ptr = (nvmlDeviceGetTemperature_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetTemperature");
  nvmlDeviceGetPowerUsage_ptr = (nvmlDeviceGetPowerUsage_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetPowerUsage");
  nvmlDeviceGetPowerManagementLimit_ptr =
      (nvmlDeviceGetPowerManagementLimit_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetPowerManagementLimit");
  nvmlDeviceGetPowerManagementLimitConstraints_ptr = (nvmlDeviceGetPowerManagementLimitConstraints_t)GetProcAddress(
      nvmlHandle, "nvmlDeviceGetPowerManagementLimitConstraints");
  nvmlDeviceGetClockInfo_ptr = (nvmlDeviceGetClockInfo_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetClockInfo");
  nvmlDeviceGetFanSpeed_ptr = (nvmlDeviceGetFanSpeed_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetFanSpeed");
  nvmlDeviceGetNumFans_ptr = (nvmlDeviceGetNumFans_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetNumFans");
  nvmlDeviceGetFanSpeed_v2_ptr = (nvmlDeviceGetFanSpeed_v2_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetFanSpeed_v2");
  nvmlDeviceGetComputeRunningProcesses_ptr =
      (nvmlDeviceGetComputeRunningProcesses_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetComputeRunningProcesses");
  nvmlDeviceGetGraphicsRunningProcesses_ptr =
      (nvmlDeviceGetGraphicsRunningProcesses_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetGraphicsRunningProcesses");
  nvmlDeviceGetPciInfo_v3_ptr = (nvmlDeviceGetPciInfo_v3_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetPciInfo_v3");
  nvmlDeviceGetMaxPcieLinkGeneration_ptr =
      (nvmlDeviceGetMaxPcieLinkGeneration_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetMaxPcieLinkGeneration");
  nvmlDeviceGetMaxPcieLinkWidth_ptr =
      (nvmlDeviceGetMaxPcieLinkWidth_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetMaxPcieLinkWidth");
  nvmlDeviceGetCurrPcieLinkGeneration_ptr =
      (nvmlDeviceGetCurrPcieLinkGeneration_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetCurrPcieLinkGeneration");
  nvmlDeviceGetCurrPcieLinkWidth_ptr =
      (nvmlDeviceGetCurrPcieLinkWidth_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetCurrPcieLinkWidth");
  nvmlDeviceGetPcieThroughput_ptr =
      (nvmlDeviceGetPcieThroughput_t)GetProcAddress(nvmlHandle, "nvmlDeviceGetPcieThroughput");

  return true;
}

bool gpuinfo_nvidia_init(void) {
  if (!load_nvml_library()) {
    return false;
  }

  nvmlReturn_t result = nvmlInit_v2_ptr();
  if (result != NVML_SUCCESS) {
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

  // Get PCIe max link generation
  if (nvmlDeviceGetMaxPcieLinkGeneration_ptr) {
    unsigned int max_gen;
    if (nvmlDeviceGetMaxPcieLinkGeneration_ptr(device, &max_gen) == NVML_SUCCESS) {
      SET_GPUINFO_STATIC(static_info, max_pcie_gen, max_gen);
    }
  }

  // Get PCIe max link width
  if (nvmlDeviceGetMaxPcieLinkWidth_ptr) {
    unsigned int max_width;
    if (nvmlDeviceGetMaxPcieLinkWidth_ptr(device, &max_width) == NVML_SUCCESS) {
      SET_GPUINFO_STATIC(static_info, max_pcie_link_width, max_width);
    }
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
      SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mem.total);
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

  // Get power usage (NVML returns milliwatts, store as-is)
  if (nvmlDeviceGetPowerUsage_ptr) {
    unsigned int power;
    if (nvmlDeviceGetPowerUsage_ptr(device, &power) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, power);
    }
  }

  // Get power management limit (NVML returns milliwatts, store as-is)
  if (nvmlDeviceGetPowerManagementLimit_ptr) {
    unsigned int power_limit;
    if (nvmlDeviceGetPowerManagementLimit_ptr(device, &power_limit) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, power_limit);
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

  // Get current PCIe link generation
  if (nvmlDeviceGetCurrPcieLinkGeneration_ptr) {
    unsigned int curr_gen;
    if (nvmlDeviceGetCurrPcieLinkGeneration_ptr(device, &curr_gen) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_gen, curr_gen);
    }
  }

  // Get current PCIe link width
  if (nvmlDeviceGetCurrPcieLinkWidth_ptr) {
    unsigned int curr_width;
    if (nvmlDeviceGetCurrPcieLinkWidth_ptr(device, &curr_width) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_width, curr_width);
    }
  }

  // Get PCIe RX throughput (receive - data coming from GPU)
  if (nvmlDeviceGetPcieThroughput_ptr) {
    unsigned int rx_throughput;
    if (nvmlDeviceGetPcieThroughput_ptr(device, NVML_PCIE_UTIL_RX_BYTES, &rx_throughput) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_rx, rx_throughput);
    }
  }

  // Get PCIe TX throughput (transmit - data going to GPU)
  if (nvmlDeviceGetPcieThroughput_ptr) {
    unsigned int tx_throughput;
    if (nvmlDeviceGetPcieThroughput_ptr(device, NVML_PCIE_UTIL_TX_BYTES, &tx_throughput) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_tx, tx_throughput);
    }
  }

  // Get fan speed
  // Try newer API first (supports multiple fans)
  if (nvmlDeviceGetNumFans_ptr && nvmlDeviceGetFanSpeed_v2_ptr) {
    unsigned int num_fans = 0;
    if (nvmlDeviceGetNumFans_ptr(device, &num_fans) == NVML_SUCCESS && num_fans > 0) {
      // Get speed of first fan (most cards have only one, or report average)
      unsigned int fan_speed;
      if (nvmlDeviceGetFanSpeed_v2_ptr(device, 0, &fan_speed) == NVML_SUCCESS) {
        SET_GPUINFO_DYNAMIC(dynamic_info, fan_speed, fan_speed);
      }
    }
  } else if (nvmlDeviceGetFanSpeed_ptr) {
    // Fall back to older API
    unsigned int fan_speed;
    if (nvmlDeviceGetFanSpeed_ptr(device, &fan_speed) == NVML_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, fan_speed, fan_speed);
    }
  }
}

static void gpuinfo_nvidia_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_nvidia *gpu_info = container_of(_gpu_info, struct gpu_info_nvidia, base);
  nvmlDevice_t device = gpu_info->gpuhandle;

  _gpu_info->processes_count = 0;

  if (!nvmlDeviceGetGraphicsRunningProcesses_ptr && !nvmlDeviceGetComputeRunningProcesses_ptr) {
    return; // No process monitoring functions available
  }

  // Buffer for process information
  static unsigned int max_processes = 128;
  static nvmlProcessInfo_t *process_buffer = NULL;

  if (!process_buffer) {
    process_buffer = calloc(max_processes, sizeof(nvmlProcessInfo_t));
    if (!process_buffer) {
      fprintf(stderr, "Failed to allocate memory for process buffer\n");
      return;
    }
  }

  unsigned int graphical_count = 0, compute_count = 0;

  // Get graphics processes
  if (nvmlDeviceGetGraphicsRunningProcesses_ptr) {
    unsigned int count = max_processes;
    nvmlReturn_t ret = nvmlDeviceGetGraphicsRunningProcesses_ptr(device, &count, process_buffer);

    if (ret == NVML_ERROR_INSUFFICIENT_SIZE) {
      // Reallocate buffer
      max_processes = count + 16;
      process_buffer = realloc(process_buffer, max_processes * sizeof(nvmlProcessInfo_t));
      if (!process_buffer) {
        fprintf(stderr, "Failed to reallocate process buffer\n");
        return;
      }
      count = max_processes;
      ret = nvmlDeviceGetGraphicsRunningProcesses_ptr(device, &count, process_buffer);
    }

    if (ret == NVML_SUCCESS) {
      graphical_count = count;
    }
  }

  // Get compute processes
  if (nvmlDeviceGetComputeRunningProcesses_ptr) {
    unsigned int count = max_processes - graphical_count;
    nvmlReturn_t ret = nvmlDeviceGetComputeRunningProcesses_ptr(device, &count, process_buffer + graphical_count);

    if (ret == NVML_ERROR_INSUFFICIENT_SIZE) {
      // Reallocate buffer
      max_processes = graphical_count + count + 16;
      process_buffer = realloc(process_buffer, max_processes * sizeof(nvmlProcessInfo_t));
      if (!process_buffer) {
        fprintf(stderr, "Failed to reallocate process buffer\n");
        return;
      }
      count = max_processes - graphical_count;
      ret = nvmlDeviceGetComputeRunningProcesses_ptr(device, &count, process_buffer + graphical_count);
    }

    if (ret == NVML_SUCCESS) {
      compute_count = count;
    }
  }

  unsigned int total_processes = graphical_count + compute_count;
  _gpu_info->processes_count = total_processes;

  if (total_processes == 0) {
    return;
  }

  // Allocate or resize the processes array
  if (total_processes > _gpu_info->processes_array_size) {
    _gpu_info->processes_array_size = total_processes + 16;
    _gpu_info->processes =
        realloc(_gpu_info->processes, _gpu_info->processes_array_size * sizeof(*_gpu_info->processes));
    if (!_gpu_info->processes) {
      fprintf(stderr, "Failed to allocate GPU process array\n");
      _gpu_info->processes_count = 0;
      return;
    }
  }

  // Clear the array
  memset(_gpu_info->processes, 0, total_processes * sizeof(*_gpu_info->processes));

  // Fill in process information
  for (unsigned int i = 0; i < total_processes; i++) {
    struct gpu_process *proc = &_gpu_info->processes[i];
    nvmlProcessInfo_t *nvml_proc = &process_buffer[i];

    // Set process type
    if (i < graphical_count) {
      proc->type = gpu_process_graphical;
    } else {
      proc->type = gpu_process_compute;
    }

    // Set PID
    proc->pid = nvml_proc->pid;

    // Set GPU memory usage
    proc->gpu_memory_usage = nvml_proc->usedGpuMemory;
    proc->gpu_memory_percentage = 0; // Will be calculated by caller if needed

    // Get process information from Windows
    get_command_from_pid(proc->pid, &proc->cmdline);
    get_username_from_pid(proc->pid, &proc->user_name);

    struct process_cpu_usage cpu_info;
    if (get_process_info(proc->pid, &cpu_info)) {
      proc->cpu_memory_res = cpu_info.resident_memory;
      proc->cpu_memory_virt = cpu_info.virtual_memory;
      // CPU usage calculation would require time deltas, set to 0 for now
      proc->cpu_usage = 0;
    }
  }
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
