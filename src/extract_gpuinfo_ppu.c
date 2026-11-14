/* SPDX-License-Identifier: GPL-3.0-or-later
 * Copyright (C) 2026 The NVTOP Contributors
 */

#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* HGML ABI transcribed from the PPU SDK hgml.h (API version 13). Keep these
 * independent of the SDK so ordinary Linux builds do not require vendor
 * development packages. Recheck this block when the SDK bumps its API
 * version. In particular, HGML PCI info trades four bytes of busId for the
 * extra globalPpuId field of NVML's struct, so busId is 28 bytes rather than
 * 32.
 */
typedef struct {
  struct hgmlDevice_st *handle;
} hgmlDevice_t;
typedef struct {
  char busIdLegacy[16];
  unsigned domain, bus, device, pciDeviceId, pciSubSystemId;
  char busId[28]; /* HGML_DEVICE_PCI_BUS_ID_BUFFER_SIZE is (32 - 4) */
  unsigned globalPpuId;
} hgmlPciInfo_t;
typedef struct { unsigned long long total, free, used; } hgmlMemory_t;
typedef struct { unsigned gpu, memory; } hgmlUtilization_t;
typedef struct {
  unsigned pid;
  unsigned long long usedGpuMemory;
  unsigned gpuInstanceId, computeInstanceId;
} hgmlProcessInfo_t;
typedef struct {
  unsigned pid;
  unsigned long long timeStamp;
  unsigned smUtil, memUtil, encUtil, decUtil;
} hgmlProcessUtilizationSample_t;

typedef enum {
  HGML_CLOCK_GRAPHICS = 0,
  HGML_CLOCK_SM = 1,
  HGML_CLOCK_MEM = 2,
  HGML_CLOCK_VIDEO = 3,
} hgmlClockType_t;
typedef enum {
  HGML_TEMPERATURE_THRESHOLD_SHUTDOWN = 0,
  HGML_TEMPERATURE_THRESHOLD_SLOWDOWN = 1,
} hgmlTemperatureThresholds_t;
typedef enum {
  HGML_TEMPERATURE_GPU = 0,
} hgmlTemperatureSensors_t;
typedef enum {
  HGML_PCIE_UTIL_TX_BYTES = 0,
  HGML_PCIE_UTIL_RX_BYTES = 1,
} hgmlPcieUtilCounter_t;

#define HGML_SUCCESS 0
#define HGML_ERROR_NOT_FOUND 6
#define HGML_ERROR_INSUFFICIENT_SIZE 7

static int (*hgmlInit)(void);
static int (*hgmlShutdown)(void);
static int (*hgmlDeviceGetCount)(unsigned *);
static int (*hgmlDeviceGetHandleByIndex)(unsigned, hgmlDevice_t *);
static int (*hgmlDeviceGetName)(hgmlDevice_t, char *, unsigned);
static int (*hgmlDeviceGetPciInfo)(hgmlDevice_t, hgmlPciInfo_t *);
static int (*hgmlDeviceGetMemoryInfo)(hgmlDevice_t, hgmlMemory_t *);
static int (*hgmlDeviceGetUtilizationRates)(hgmlDevice_t, hgmlUtilization_t *);
static int (*hgmlDeviceGetComputeRunningProcesses)(hgmlDevice_t, unsigned *, hgmlProcessInfo_t *);
static int (*hgmlDeviceGetProcessUtilization)(hgmlDevice_t, hgmlProcessUtilizationSample_t *, unsigned *,
                                            unsigned long long);
static int (*hgmlDeviceGetClockInfo)(hgmlDevice_t, hgmlClockType_t, unsigned *);
static int (*hgmlDeviceGetMaxClockInfo)(hgmlDevice_t, hgmlClockType_t, unsigned *);
static int (*hgmlDeviceGetTemperature)(hgmlDevice_t, hgmlTemperatureSensors_t, unsigned *);
static int (*hgmlDeviceGetTemperatureThreshold)(hgmlDevice_t, hgmlTemperatureThresholds_t, unsigned *);
static int (*hgmlDeviceGetPcieThroughput)(hgmlDevice_t, hgmlPcieUtilCounter_t, unsigned *);
static int (*hgmlDeviceGetPowerUsage)(hgmlDevice_t, unsigned *);
static int (*hgmlDeviceGetEnforcedPowerLimit)(hgmlDevice_t, unsigned *);
static int (*hgmlDeviceGetCurrPcieLinkGeneration)(hgmlDevice_t, unsigned *);
static int (*hgmlDeviceGetCurrPcieLinkWidth)(hgmlDevice_t, unsigned *);
static int (*hgmlDeviceGetMaxPcieLinkGeneration)(hgmlDevice_t, unsigned *);
static int (*hgmlDeviceGetMaxPcieLinkWidth)(hgmlDevice_t, unsigned *);
static int (*hgmlDeviceGetFanSpeed)(hgmlDevice_t, unsigned *);

struct gpu_info_ppu {
  struct gpu_info base;
  hgmlDevice_t handle;
  unsigned long long last_utilization_timestamp;
};
static struct gpu_info_ppu *ppu_devices;
static void *hgml_library;
static bool hgml_initialized;
static const char *ppu_error = "HGML is not initialized";
static struct gpu_vendor gpu_vendor_ppu;

static void ppu_shutdown(void) {
  free(ppu_devices);
  ppu_devices = NULL;
  if (hgml_initialized)
    hgmlShutdown();
  hgml_initialized = false;
  if (hgml_library)
    dlclose(hgml_library);
  hgml_library = NULL;
}

static void *ppu_symbol(const char *preferred, const char *fallback) {
  void *symbol = dlsym(hgml_library, preferred);
  return symbol || !fallback ? symbol : dlsym(hgml_library, fallback);
}

static bool ppu_init(void) {
  const char *paths[] = {"libhgml.so.1", "libhgml.so", "/usr/local/PPU_SDK/lib/libhgml.so",
                        "/usr/local/PPU_SDK/targets/x86_64-linux/lib/libhgml.so",
                        "/usr/local/PPU_SDK/targets/aarch64-linux/lib/libhgml.so"};
  for (unsigned i = 0; i < sizeof(paths) / sizeof(paths[0]) && !hgml_library; ++i)
    hgml_library = dlopen(paths[i], RTLD_LAZY | RTLD_LOCAL);
  if (!hgml_library) {
    ppu_error = "Could not load libhgml.so";
    return false;
  }
  hgmlInit = ppu_symbol("hgmlInit_v2", "hgmlInit");
  hgmlShutdown = ppu_symbol("hgmlShutdown", NULL);
  hgmlDeviceGetCount = ppu_symbol("hgmlDeviceGetCount_v2", "hgmlDeviceGetCount");
  hgmlDeviceGetHandleByIndex = ppu_symbol("hgmlDeviceGetHandleByIndex_v2", "hgmlDeviceGetHandleByIndex");
  if (!hgmlInit || !hgmlShutdown || !hgmlDeviceGetCount || !hgmlDeviceGetHandleByIndex) {
    ppu_error = "Missing required HGML entry points";
    ppu_shutdown();
    return false;
  }
#define LOAD_OPTIONAL(name) name = ppu_symbol(#name, NULL)
  LOAD_OPTIONAL(hgmlDeviceGetName);
  LOAD_OPTIONAL(hgmlDeviceGetMemoryInfo);
  LOAD_OPTIONAL(hgmlDeviceGetUtilizationRates);
  LOAD_OPTIONAL(hgmlDeviceGetProcessUtilization);
  LOAD_OPTIONAL(hgmlDeviceGetClockInfo);
  LOAD_OPTIONAL(hgmlDeviceGetMaxClockInfo);
  LOAD_OPTIONAL(hgmlDeviceGetTemperature);
  LOAD_OPTIONAL(hgmlDeviceGetTemperatureThreshold);
  LOAD_OPTIONAL(hgmlDeviceGetPcieThroughput);
  LOAD_OPTIONAL(hgmlDeviceGetPowerUsage);
  LOAD_OPTIONAL(hgmlDeviceGetEnforcedPowerLimit);
  LOAD_OPTIONAL(hgmlDeviceGetCurrPcieLinkGeneration);
  LOAD_OPTIONAL(hgmlDeviceGetCurrPcieLinkWidth);
  LOAD_OPTIONAL(hgmlDeviceGetMaxPcieLinkGeneration);
  LOAD_OPTIONAL(hgmlDeviceGetMaxPcieLinkWidth);
  LOAD_OPTIONAL(hgmlDeviceGetFanSpeed);
#undef LOAD_OPTIONAL
  hgmlDeviceGetPciInfo = ppu_symbol("hgmlDeviceGetPciInfo_v3", "hgmlDeviceGetPciInfo_v2");
  hgmlDeviceGetComputeRunningProcesses =
      ppu_symbol("hgmlDeviceGetComputeRunningProcesses_v3", "hgmlDeviceGetComputeRunningProcesses_v2");
  /* The unversioned process API has a different array stride; never use it
   * with hgmlProcessInfo_t. */
  if (hgmlInit() != HGML_SUCCESS) {
    ppu_error = "HGML initialization failed";
    ppu_shutdown();
    return false;
  }
  hgml_initialized = true;
  return true;
}

static const char *ppu_last_error(void) { return ppu_error; }

static bool ppu_get_devices(struct list_head *devices, unsigned *count) {
  unsigned available = 0;
  *count = 0;
  if (hgmlDeviceGetCount(&available) != HGML_SUCCESS) {
    ppu_error = "HGML device enumeration failed";
    return false;
  }
  if (!available)
    return true;
  ppu_devices = calloc(available, sizeof(*ppu_devices));
  if (!ppu_devices) {
    ppu_error = "Could not allocate PPU devices";
    return false;
  }
  for (unsigned i = 0; i < available; ++i) {
    struct gpu_info_ppu *ppu = &ppu_devices[i];
    if (hgmlDeviceGetHandleByIndex(i, &ppu->handle) != HGML_SUCCESS)
      continue;
    ppu->base.vendor = &gpu_vendor_ppu;
    snprintf(ppu->base.pdev, sizeof(ppu->base.pdev), "ppu-%u", i);
    hgmlPciInfo_t pci = {0};
    if (hgmlDeviceGetPciInfo && hgmlDeviceGetPciInfo(ppu->handle, &pci) == HGML_SUCCESS) {
      unsigned domain, bus, slot, function;
      pci.busId[sizeof(pci.busId) - 1] = '\0';
      /* HGML uses an eight-digit domain. Normalize it to Linux PCI notation
       * so the function number is not truncated in the common pdev buffer. */
      if (sscanf(pci.busId, "%x:%x:%x.%x", &domain, &bus, &slot, &function) == 4 &&
          domain <= 0xffff && bus <= 0xff && slot <= 0x1f && function <= 7)
        snprintf(ppu->base.pdev, sizeof(ppu->base.pdev), "%04x:%02x:%02x.%x", domain, bus, slot, function);
    }
    list_add_tail(&ppu->base.list, devices);
    ++*count;
  }
  return true;
}

static void ppu_static_info(struct gpu_info *base) {
  struct gpu_info_ppu *ppu = container_of(base, struct gpu_info_ppu, base);
  struct gpuinfo_static_info *info = &base->static_info;
  RESET_ALL(info->valid);
  snprintf(info->device_name, sizeof(info->device_name), "T-Head PPU");
  if (hgmlDeviceGetName)
    hgmlDeviceGetName(ppu->handle, info->device_name, sizeof(info->device_name));
  info->device_name[sizeof(info->device_name) - 1] = '\0';
  SET_VALID(gpuinfo_device_name_valid, info->valid);
  unsigned value;
#define STATIC_QUERY(field, function, ...) \
  if (function && function(ppu->handle, ##__VA_ARGS__, &value) == HGML_SUCCESS) \
    SET_GPUINFO_STATIC(info, field, value)
  STATIC_QUERY(max_pcie_gen, hgmlDeviceGetMaxPcieLinkGeneration);
  STATIC_QUERY(max_pcie_link_width, hgmlDeviceGetMaxPcieLinkWidth);
  STATIC_QUERY(temperature_shutdown_threshold, hgmlDeviceGetTemperatureThreshold,
               HGML_TEMPERATURE_THRESHOLD_SHUTDOWN);
  STATIC_QUERY(temperature_slowdown_threshold, hgmlDeviceGetTemperatureThreshold,
               HGML_TEMPERATURE_THRESHOLD_SLOWDOWN);
#undef STATIC_QUERY
}

static void ppu_dynamic_info(struct gpu_info *base) {
  struct gpu_info_ppu *ppu = container_of(base, struct gpu_info_ppu, base);
  struct gpuinfo_dynamic_info *info = &base->dynamic_info;
  RESET_ALL(info->valid);
  hgmlUtilization_t utilization;
  if (hgmlDeviceGetUtilizationRates && hgmlDeviceGetUtilizationRates(ppu->handle, &utilization) == HGML_SUCCESS &&
      utilization.gpu <= 100)
    SET_GPUINFO_DYNAMIC(info, gpu_util_rate, utilization.gpu);
  hgmlMemory_t memory;
  if (hgmlDeviceGetMemoryInfo && hgmlDeviceGetMemoryInfo(ppu->handle, &memory) == HGML_SUCCESS &&
      memory.total && memory.used <= memory.total && memory.free <= memory.total) {
    SET_GPUINFO_DYNAMIC(info, total_memory, memory.total);
    SET_GPUINFO_DYNAMIC(info, used_memory, memory.used);
    SET_GPUINFO_DYNAMIC(info, free_memory, memory.free);
    SET_GPUINFO_DYNAMIC(info, mem_util_rate, (unsigned)(100.0 * memory.used / memory.total));
  }
  unsigned value;
#define DYNAMIC_QUERY(field, function, ...) \
  if (function && function(ppu->handle, ##__VA_ARGS__, &value) == HGML_SUCCESS) \
    SET_GPUINFO_DYNAMIC(info, field, value)
  /* PPUs expose their compute clock as HGML_CLOCK_SM, not HGML_CLOCK_GRAPHICS. */
  DYNAMIC_QUERY(gpu_clock_speed, hgmlDeviceGetClockInfo, HGML_CLOCK_SM);
  DYNAMIC_QUERY(mem_clock_speed, hgmlDeviceGetClockInfo, HGML_CLOCK_MEM);
  DYNAMIC_QUERY(gpu_clock_speed_max, hgmlDeviceGetMaxClockInfo, HGML_CLOCK_SM);
  DYNAMIC_QUERY(mem_clock_speed_max, hgmlDeviceGetMaxClockInfo, HGML_CLOCK_MEM);
  DYNAMIC_QUERY(gpu_temp, hgmlDeviceGetTemperature, HGML_TEMPERATURE_GPU);
  /* HGML returns power in mW and PCIe throughput in KB/s. */
  DYNAMIC_QUERY(power_draw, hgmlDeviceGetPowerUsage);
  DYNAMIC_QUERY(power_draw_max, hgmlDeviceGetEnforcedPowerLimit);
  DYNAMIC_QUERY(pcie_link_gen, hgmlDeviceGetCurrPcieLinkGeneration);
  DYNAMIC_QUERY(pcie_link_width, hgmlDeviceGetCurrPcieLinkWidth);
  DYNAMIC_QUERY(pcie_tx, hgmlDeviceGetPcieThroughput, HGML_PCIE_UTIL_TX_BYTES);
  DYNAMIC_QUERY(pcie_rx, hgmlDeviceGetPcieThroughput, HGML_PCIE_UTIL_RX_BYTES);
  DYNAMIC_QUERY(fan_speed, hgmlDeviceGetFanSpeed);
#undef DYNAMIC_QUERY
}

/* Process enumeration clears validity every refresh, so an old sample can
 * never be mistaken for a current one. Returns true when the runtime answered
 * the query or reported no samples, meaning PIDs without a sample were idle.
 * Returns false when per-process utilization is unavailable or the query
 * failed, in which case ppu_processes leaves the value unset. */
static bool ppu_process_utilization(struct gpu_info_ppu *ppu) {
  struct gpu_info *base = &ppu->base;
  if (!hgmlDeviceGetProcessUtilization)
    return false;
  if (!base->processes_count)
    return true;

  bool utilization_known = false;
  hgmlProcessUtilizationSample_t *samples = NULL;
  unsigned capacity = 0;
  for (unsigned attempt = 0; attempt < 4; ++attempt) {
    unsigned count = capacity;
    int result = hgmlDeviceGetProcessUtilization(ppu->handle, samples, &count, ppu->last_utilization_timestamp);
    if (result == HGML_ERROR_INSUFFICIENT_SIZE) {
      if (count > UINT_MAX - 16)
        break;
      capacity = count + 16;
      free(samples);
      samples = calloc(capacity, sizeof(*samples));
      if (!samples)
        break;
      continue;
    }
    if (result == HGML_ERROR_NOT_FOUND) {
      utilization_known = true;
      break;
    }
    if (result != HGML_SUCCESS || count > capacity)
      break;

    unsigned long long newest = ppu->last_utilization_timestamp;
    for (unsigned p = 0; p < base->processes_count; ++p) {
      struct gpu_process *process = &base->processes[p];
      unsigned long long latest = ppu->last_utilization_timestamp;
      /* HGML may return several samples per PID, in any order. Select the
       * newest valid SM sample instead of summing utilization percentages. */
      for (unsigned i = 0; i < count; ++i) {
        if (samples[i].pid != (unsigned)process->pid || samples[i].timeStamp <= latest || samples[i].smUtil > 100)
          continue;
        latest = samples[i].timeStamp;
        SET_GPUINFO_PROCESS(process, gpu_usage, samples[i].smUtil);
      }
      if (latest > newest)
        newest = latest;
    }
    ppu->last_utilization_timestamp = newest;
    utilization_known = true;
    break;
  }
  free(samples);
  return utilization_known;
}

static void ppu_processes(struct gpu_info *base) {
  struct gpu_info_ppu *ppu = container_of(base, struct gpu_info_ppu, base);
  base->processes_count = 0;
  if (!hgmlDeviceGetComputeRunningProcesses)
    return;
  hgmlProcessInfo_t *samples = NULL;
  unsigned capacity = 0;
  /* Processes can appear between the size query and the data query. Bound
   * retries so a continuously growing workload cannot stall the UI. */
  for (unsigned attempt = 0; attempt < 4; ++attempt) {
    unsigned count = capacity;
    int result = hgmlDeviceGetComputeRunningProcesses(ppu->handle, &count, samples);
    if (result == HGML_ERROR_INSUFFICIENT_SIZE) {
      if (count > UINT_MAX - 16)
        break;
      capacity = count + 16;
      free(samples);
      samples = calloc(capacity, sizeof(*samples));
      if (!samples)
        break;
      continue;
    }
    if (result != HGML_SUCCESS || count > capacity)
      break;
    if (count > base->processes_array_size) {
      if (count > UINT_MAX - COMMON_PROCESS_LINEAR_REALLOC_INC)
        break;
      unsigned new_size = count + COMMON_PROCESS_LINEAR_REALLOC_INC;
      struct gpu_process *processes = reallocarray(base->processes, new_size, sizeof(*processes));
      if (!processes)
        break;
      base->processes = processes;
      base->processes_array_size = new_size;
    }
    for (unsigned i = 0; i < count; ++i) {
      struct gpu_process *process = &base->processes[i];
      memset(process, 0, sizeof(*process));
      process->pid = samples[i].pid;
      process->type = gpu_process_compute;
      if (samples[i].usedGpuMemory != ULLONG_MAX)
        SET_GPUINFO_PROCESS(process, gpu_memory_usage, samples[i].usedGpuMemory);
    }
    base->processes_count = count;
    break;
  }
  free(samples);
  if (ppu_process_utilization(ppu)) {
    /* Mark the ones without an update since the last sample period as idle. */
    for (unsigned i = 0; i < base->processes_count; ++i) {
      if (!GPUINFO_PROCESS_FIELD_VALID(&base->processes[i], gpu_usage))
        SET_GPUINFO_PROCESS(&base->processes[i], gpu_usage, 0);
    }
  }
}

static struct gpu_vendor gpu_vendor_ppu = {
    .init = ppu_init,
    .shutdown = ppu_shutdown,
    .last_error_string = ppu_last_error,
    .get_device_handles = ppu_get_devices,
    .populate_static_info = ppu_static_info,
    .refresh_dynamic_info = ppu_dynamic_info,
    .refresh_running_processes = ppu_processes,
    .name = "T-Head",
    .processing_unit = gpu_processing_unit_ppu,
};

__attribute__((constructor)) static void register_ppu(void) { register_gpu_vendor(&gpu_vendor_ppu); }
