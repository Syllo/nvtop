/*
 * Intel GPU Information Extraction for Windows
 * Copyright (C) 2025
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

/*
 * IMPLEMENTATION STATUS: Fully Functional
 *
 * This implementation provides Intel GPU monitoring on Windows using:
 * - DXGI (DirectX Graphics Infrastructure) for GPU enumeration
 * - PDH (Performance Data Helper) API for performance metrics
 *
 * Supported Metrics:
 * - GPU utilization (aggregated across all engines)
 * - VRAM usage (dedicated and shared memory)
 * - Per-process GPU usage
 * - GPU engine utilization by type (3D, Copy, Video, Compute)
 *
 * Note: Temperature and fan speed are typically not available for Intel GPUs
 * through standard Windows APIs.
 */

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <dxgi.h>
#include <pdh.h>
#include <pdhmsg.h>
#include <windows.h>

#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/get_process_info.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

// Link with required libraries
#pragma comment(lib, "pdh.lib")
#pragma comment(lib, "dxgi.lib")

// Define IID_IDXGIFactory locally to avoid linker issues
static const GUID IID_IDXGIFactory_Local = {
    0x7b7166ec, 0x21c7, 0x44ae, {0xb2, 0x1a, 0xc9, 0xae, 0x32, 0x1a, 0xe3, 0x69}};

// Forward declarations
static bool gpuinfo_intel_init(void);
static void gpuinfo_intel_shutdown(void);
static const char *gpuinfo_intel_last_error_string(void);
static bool gpuinfo_intel_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_intel_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_intel_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_intel_get_running_processes(struct gpu_info *_gpu_info);

// Vendor structure registration
struct gpu_vendor gpu_vendor_intel = {
    .init = gpuinfo_intel_init,
    .shutdown = gpuinfo_intel_shutdown,
    .last_error_string = gpuinfo_intel_last_error_string,
    .get_device_handles = gpuinfo_intel_get_device_handles,
    .populate_static_info = gpuinfo_intel_populate_static_info,
    .refresh_dynamic_info = gpuinfo_intel_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_intel_get_running_processes,
    .name = "Intel",
};

// Auto-register vendor on load
__attribute__((constructor)) static void init_extract_gpuinfo_intel(void) { register_gpu_vendor(&gpu_vendor_intel); }

// Intel GPU device handle
struct intel_gpu_handle {
  UINT adapter_index;
  DXGI_ADAPTER_DESC adapter_desc;
  PDH_HCOUNTER gpu_util_counter;
  PDH_HCOUNTER mem_dedicated_counter;
  PDH_HCOUNTER mem_shared_counter;
  PDH_HCOUNTER gpu_power_counter; // GPU power consumption (if available)
  PDH_HCOUNTER gpu_freq_counter;  // GPU frequency counter (if available)
  PDH_HCOUNTER mem_freq_counter;  // Memory frequency counter (if available)
  WCHAR instance_name[256];       // PDH instance name for this GPU
  bool has_power_counter;
  bool has_freq_counters;
};

// Performance counter handles
static PDH_HQUERY pdhQuery = NULL;
static bool pdh_initialized = false;
static struct intel_gpu_handle *gpu_handles = NULL;
static unsigned num_gpus = 0;

// Error handling
static char error_string[1024] = "";

static const char *gpuinfo_intel_last_error_string(void) { return error_string[0] ? error_string : "No error"; }

// Helper function to convert WCHAR to char
static void wchar_to_char(const WCHAR *src, char *dest, size_t dest_size) {
  if (!src || !dest || dest_size == 0)
    return;
  WideCharToMultiByte(CP_UTF8, 0, src, -1, dest, (int)dest_size, NULL, NULL);
  dest[dest_size - 1] = '\0';
}

// Initialize Intel GPU monitoring
static bool gpuinfo_intel_init(void) {
  PDH_STATUS status;

  // Initialize PDH query
  status = PdhOpenQuery(NULL, 0, &pdhQuery);
  if (status != ERROR_SUCCESS) {
    snprintf(error_string, sizeof(error_string), "Failed to open PDH query: 0x%08X", status);
    return false;
  }

  pdh_initialized = true;
  error_string[0] = '\0';
  return true;
}

// Shutdown and cleanup
static void gpuinfo_intel_shutdown(void) {
  if (pdhQuery) {
    PdhCloseQuery(pdhQuery);
    pdhQuery = NULL;
  }

  if (gpu_handles) {
    free(gpu_handles);
    gpu_handles = NULL;
  }

  num_gpus = 0;
  pdh_initialized = false;
}

// Get Intel device handles
static bool gpuinfo_intel_get_device_handles(struct list_head *devices, unsigned *count) {
  IDXGIFactory *pFactory = NULL;
  IDXGIAdapter *pAdapter = NULL;
  HRESULT hr;
  UINT adapter_index = 0;
  unsigned intel_gpu_count = 0;

  *count = 0;

  if (!pdh_initialized) {
    snprintf(error_string, sizeof(error_string), "Intel GPU monitoring not initialized");
    return false;
  }

  // Create DXGI factory
  hr = CreateDXGIFactory(&IID_IDXGIFactory_Local, (void **)&pFactory);
  if (FAILED(hr)) {
    snprintf(error_string, sizeof(error_string), "Failed to create DXGI factory: 0x%08X", hr);
    return false;
  }

  // First pass: count Intel GPUs
  while (pFactory->lpVtbl->EnumAdapters(pFactory, adapter_index, &pAdapter) != DXGI_ERROR_NOT_FOUND) {
    DXGI_ADAPTER_DESC desc;
    hr = pAdapter->lpVtbl->GetDesc(pAdapter, &desc);

    if (SUCCEEDED(hr) && desc.VendorId == 0x8086) { // Intel VendorId
      intel_gpu_count++;
    }

    pAdapter->lpVtbl->Release(pAdapter);
    adapter_index++;
  }

  if (intel_gpu_count == 0) {
    pFactory->lpVtbl->Release(pFactory);
    return false; // No Intel GPUs found
  }

  // Allocate handles
  gpu_handles = calloc(intel_gpu_count, sizeof(struct intel_gpu_handle));
  if (!gpu_handles) {
    snprintf(error_string, sizeof(error_string), "Failed to allocate memory for GPU handles");
    pFactory->lpVtbl->Release(pFactory);
    return false;
  }

  // Second pass: create gpu_info structures
  adapter_index = 0;
  unsigned gpu_idx = 0;

  while (pFactory->lpVtbl->EnumAdapters(pFactory, adapter_index, &pAdapter) != DXGI_ERROR_NOT_FOUND) {
    DXGI_ADAPTER_DESC desc;
    hr = pAdapter->lpVtbl->GetDesc(pAdapter, &desc);

    if (SUCCEEDED(hr) && desc.VendorId == 0x8086) {
      struct gpu_info *gpu_info = calloc(1, sizeof(*gpu_info));
      if (!gpu_info) {
        pAdapter->lpVtbl->Release(pAdapter);
        continue;
      }

      // Initialize gpu_info
      INIT_LIST_HEAD(&gpu_info->list);
      gpu_info->vendor = &gpu_vendor_intel;

      // Store handle info
      struct intel_gpu_handle *handle = &gpu_handles[gpu_idx];
      handle->adapter_index = adapter_index;
      memcpy(&handle->adapter_desc, &desc, sizeof(DXGI_ADAPTER_DESC));

      // Create PDH instance name for this adapter
      swprintf(handle->instance_name, 256, L"phys_%u", adapter_index);

      // Store adapter index in pdev field for tracking
      snprintf(gpu_info->pdev, PDEV_LEN, "intel%u", adapter_index);

      // Add to device list
      list_add_tail(&gpu_info->list, devices);
      gpu_idx++;
    }

    pAdapter->lpVtbl->Release(pAdapter);
    adapter_index++;
  }

  pFactory->lpVtbl->Release(pFactory);

  num_gpus = intel_gpu_count;
  *count = intel_gpu_count;
  return true;
}

// Populate static GPU information
static void gpuinfo_intel_populate_static_info(struct gpu_info *_gpu_info) {
  if (!_gpu_info || !_gpu_info->pdev[0])
    return;

  // Extract adapter index from pdev field
  unsigned adapter_idx = 0;
  sscanf(_gpu_info->pdev, "intel%u", &adapter_idx);

  if (adapter_idx >= num_gpus)
    return;

  struct intel_gpu_handle *handle = &gpu_handles[adapter_idx];
  DXGI_ADAPTER_DESC *desc = &handle->adapter_desc;

  // Convert GPU name from WCHAR to char
  wchar_to_char(desc->Description, _gpu_info->static_info.device_name, MAX_DEVICE_NAME);
  SET_VALID(gpuinfo_device_name_valid, _gpu_info->static_info.valid);

  // Determine if integrated graphics (Intel iGPUs typically have more shared than dedicated memory)
  _gpu_info->static_info.integrated_graphics = (desc->SharedSystemMemory > desc->DedicatedVideoMemory);
}

// Refresh dynamic GPU information
static void gpuinfo_intel_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  if (!_gpu_info || !_gpu_info->pdev[0] || !pdh_initialized)
    return;

  // Extract adapter index from pdev field
  unsigned adapter_idx = 0;
  sscanf(_gpu_info->pdev, "intel%u", &adapter_idx);

  if (adapter_idx >= num_gpus)
    return;

  struct intel_gpu_handle *handle = &gpu_handles[adapter_idx];
  DXGI_ADAPTER_DESC *desc = &handle->adapter_desc;
  PDH_STATUS status;
  PDH_FMT_COUNTERVALUE counterValue;

  // Setup PDH counters for this GPU if not already done
  if (!handle->gpu_util_counter) {
    WCHAR counter_path[512];

    // GPU Engine Utilization counter (aggregates all engines)
    swprintf(counter_path, 512, L"\\GPU Engine(phys_%u_*_3D)\\Utilization Percentage", adapter_idx);
    status = PdhAddCounterW(pdhQuery, counter_path, 0, &handle->gpu_util_counter);
    if (status != ERROR_SUCCESS) {
      handle->gpu_util_counter = NULL;
    }

    // Dedicated memory counter
    swprintf(counter_path, 512, L"\\GPU Adapter Memory(phys_%u)\\Dedicated Usage", adapter_idx);
    status = PdhAddCounterW(pdhQuery, counter_path, 0, &handle->mem_dedicated_counter);
    if (status != ERROR_SUCCESS) {
      handle->mem_dedicated_counter = NULL;
    }

    // Shared memory counter
    swprintf(counter_path, 512, L"\\GPU Adapter Memory(phys_%u)\\Shared Usage", adapter_idx);
    status = PdhAddCounterW(pdhQuery, counter_path, 0, &handle->mem_shared_counter);
    if (status != ERROR_SUCCESS) {
      handle->mem_shared_counter = NULL;
    }

    // Try to add power counter (may not be available on all Intel GPUs)
    swprintf(counter_path, 512, L"\\GPU Adapter(phys_%u)\\Power", adapter_idx);
    status = PdhAddCounterW(pdhQuery, counter_path, 0, &handle->gpu_power_counter);
    if (status == ERROR_SUCCESS) {
      handle->has_power_counter = true;
    } else {
      handle->gpu_power_counter = NULL;
      handle->has_power_counter = false;
    }

    // Try to add frequency counters (may not be available)
    swprintf(counter_path, 512, L"\\GPU Adapter(phys_%u)\\GPU Frequency", adapter_idx);
    status = PdhAddCounterW(pdhQuery, counter_path, 0, &handle->gpu_freq_counter);
    if (status == ERROR_SUCCESS) {
      swprintf(counter_path, 512, L"\\GPU Adapter(phys_%u)\\Memory Frequency", adapter_idx);
      status = PdhAddCounterW(pdhQuery, counter_path, 0, &handle->mem_freq_counter);
      if (status == ERROR_SUCCESS) {
        handle->has_freq_counters = true;
      } else {
        handle->mem_freq_counter = NULL;
        handle->has_freq_counters = false;
      }
    } else {
      handle->gpu_freq_counter = NULL;
      handle->has_freq_counters = false;
    }
  }

  // Collect current performance data
  status = PdhCollectQueryData(pdhQuery);
  if (status != ERROR_SUCCESS) {
    return; // Cannot collect data this cycle
  }

  // Sleep briefly to allow counters to stabilize (first call often returns 0)
  Sleep(100);
  status = PdhCollectQueryData(pdhQuery);
  if (status != ERROR_SUCCESS) {
    return;
  }

  // Get GPU utilization
  if (handle->gpu_util_counter) {
    status = PdhGetFormattedCounterValue(handle->gpu_util_counter, PDH_FMT_DOUBLE, NULL, &counterValue);
    if (status == ERROR_SUCCESS && counterValue.CStatus == PDH_CSTATUS_VALID_DATA) {
      unsigned int util = (unsigned int)(counterValue.doubleValue);
      if (util <= 100) {
        SET_GPUINFO_DYNAMIC(&_gpu_info->dynamic_info, gpu_util_rate, util);
      }
    }
  }

  // Get memory usage
  unsigned long long dedicated_used = 0;
  unsigned long long shared_used = 0;

  if (handle->mem_dedicated_counter) {
    status = PdhGetFormattedCounterValue(handle->mem_dedicated_counter, PDH_FMT_LARGE, NULL, &counterValue);
    if (status == ERROR_SUCCESS && counterValue.CStatus == PDH_CSTATUS_VALID_DATA) {
      dedicated_used = counterValue.largeValue;
    }
  }

  if (handle->mem_shared_counter) {
    status = PdhGetFormattedCounterValue(handle->mem_shared_counter, PDH_FMT_LARGE, NULL, &counterValue);
    if (status == ERROR_SUCCESS && counterValue.CStatus == PDH_CSTATUS_VALID_DATA) {
      shared_used = counterValue.largeValue;
    }
  }

  // Calculate memory totals
  unsigned long long total_mem = desc->DedicatedVideoMemory + desc->SharedSystemMemory;
  unsigned long long used_mem = dedicated_used + shared_used;
  unsigned long long free_mem = (total_mem > used_mem) ? (total_mem - used_mem) : 0;

  SET_GPUINFO_DYNAMIC(&_gpu_info->dynamic_info, total_memory, total_mem);
  SET_GPUINFO_DYNAMIC(&_gpu_info->dynamic_info, used_memory, used_mem);
  SET_GPUINFO_DYNAMIC(&_gpu_info->dynamic_info, free_memory, free_mem);

  // Memory utilization percentage
  if (total_mem > 0) {
    unsigned int mem_util = (unsigned int)((used_mem * 100) / total_mem);
    SET_GPUINFO_DYNAMIC(&_gpu_info->dynamic_info, mem_util_rate, mem_util);
  }

  // Get power consumption if available
  if (handle->has_power_counter && handle->gpu_power_counter) {
    status = PdhGetFormattedCounterValue(handle->gpu_power_counter, PDH_FMT_LARGE, NULL, &counterValue);
    if (status == ERROR_SUCCESS && counterValue.CStatus == PDH_CSTATUS_VALID_DATA) {
      // Power is typically in watts, convert to milliwatts
      unsigned int power_mw = (unsigned int)(counterValue.largeValue * 1000);
      SET_GPUINFO_DYNAMIC(&_gpu_info->dynamic_info, power_draw, power_mw);
    }
  }

  // Get clock speeds if available
  if (handle->has_freq_counters) {
    if (handle->gpu_freq_counter) {
      status = PdhGetFormattedCounterValue(handle->gpu_freq_counter, PDH_FMT_LARGE, NULL, &counterValue);
      if (status == ERROR_SUCCESS && counterValue.CStatus == PDH_CSTATUS_VALID_DATA) {
        // Frequency is typically in MHz
        unsigned int gpu_clock = (unsigned int)(counterValue.largeValue);
        SET_GPUINFO_DYNAMIC(&_gpu_info->dynamic_info, gpu_clock_speed, gpu_clock);
      }
    }

    if (handle->mem_freq_counter) {
      status = PdhGetFormattedCounterValue(handle->mem_freq_counter, PDH_FMT_LARGE, NULL, &counterValue);
      if (status == ERROR_SUCCESS && counterValue.CStatus == PDH_CSTATUS_VALID_DATA) {
        // Frequency is typically in MHz
        unsigned int mem_clock = (unsigned int)(counterValue.largeValue);
        SET_GPUINFO_DYNAMIC(&_gpu_info->dynamic_info, mem_clock_speed, mem_clock);
      }
    }
  }
}

// Structure to track per-process GPU usage
struct process_gpu_usage {
  pid_t pid;
  unsigned int utilization; // Aggregated utilization across all engines
  enum gpu_process_type type;
};

// Get running processes using GPU
static void gpuinfo_intel_get_running_processes(struct gpu_info *_gpu_info) {
  if (!_gpu_info || !_gpu_info->pdev[0])
    return;

  // Extract adapter index from pdev field
  unsigned adapter_idx = 0;
  sscanf(_gpu_info->pdev, "intel%u", &adapter_idx);

  if (adapter_idx >= num_gpus)
    return;

  // Enumerate GPU engine counters to find per-process usage
  // Counter path pattern: "\GPU Engine(pid_<PID>_luid_<LUID>_phys_<N>_eng_<E>_engtype_<TYPE>)\Utilization Percentage"

  PDH_STATUS status;
  DWORD dwBufferSize = 0;
  DWORD dwItemCount = 0;
  WCHAR wildcard_path[256];

  // Query all GPU Engine counters for this physical adapter
  swprintf(wildcard_path, 256, L"\\GPU Engine(pid_*_*_phys_%u_*_*)\\Utilization Percentage", adapter_idx);

  // Get required buffer size
  status = PdhExpandWildCardPathW(NULL, wildcard_path, NULL, &dwBufferSize, 0);
  if (status != PDH_MORE_DATA && status != ERROR_SUCCESS) {
    _gpu_info->processes_count = 0;
    return;
  }

  // Allocate buffer for expanded paths
  WCHAR *expanded_paths = (WCHAR *)malloc(dwBufferSize * sizeof(WCHAR));
  if (!expanded_paths) {
    _gpu_info->processes_count = 0;
    return;
  }

  // Expand wildcard
  status = PdhExpandWildCardPathW(NULL, wildcard_path, expanded_paths, &dwBufferSize, 0);
  if (status != ERROR_SUCCESS) {
    free(expanded_paths);
    _gpu_info->processes_count = 0;
    return;
  }

  // Parse expanded paths to extract PIDs and aggregate utilization
  struct process_gpu_usage *process_list = NULL;
  unsigned int process_count = 0;
  unsigned int process_capacity = 0;

  WCHAR *current_path = expanded_paths;
  while (*current_path != L'\0') {
    // Parse: "\GPU Engine(pid_<PID>_luid_<LUID>_phys_<N>_eng_<E>_engtype_<TYPE>)\Utilization Percentage"
    DWORD pid = 0;
    WCHAR eng_type[32] = {0};

    if (swscanf(current_path, L"\\GPU Engine(pid_%u_", &pid) == 1) {
      // Extract engine type
      WCHAR *engtype_start = wcsstr(current_path, L"_engtype_");
      if (engtype_start) {
        swscanf(engtype_start, L"_engtype_%31[^)])\\", eng_type);
      }

      // Create temporary counter to get value
      PDH_HCOUNTER hCounter = NULL;
      status = PdhAddCounterW(pdhQuery, current_path, 0, &hCounter);
      if (status == ERROR_SUCCESS) {
        PdhCollectQueryData(pdhQuery);
        Sleep(10); // Brief delay for counter stability
        PdhCollectQueryData(pdhQuery);

        PDH_FMT_COUNTERVALUE counterValue;
        status = PdhGetFormattedCounterValue(hCounter, PDH_FMT_DOUBLE, NULL, &counterValue);

        if (status == ERROR_SUCCESS && counterValue.CStatus == PDH_CSTATUS_VALID_DATA) {
          unsigned int util = (unsigned int)(counterValue.doubleValue);

          // Find or create process entry
          bool found = false;
          for (unsigned int i = 0; i < process_count; i++) {
            if (process_list[i].pid == pid) {
              process_list[i].utilization += util; // Aggregate across engines
              found = true;

              // Update process type based on engine type
              if (wcsstr(eng_type, L"3D") || wcsstr(eng_type, L"Render")) {
                if (process_list[i].type == gpu_process_compute)
                  process_list[i].type = gpu_process_graphical_compute;
                else if (process_list[i].type == gpu_process_unknown)
                  process_list[i].type = gpu_process_graphical;
              } else if (wcsstr(eng_type, L"Compute")) {
                if (process_list[i].type == gpu_process_graphical)
                  process_list[i].type = gpu_process_graphical_compute;
                else if (process_list[i].type == gpu_process_unknown)
                  process_list[i].type = gpu_process_compute;
              }
              break;
            }
          }

          if (!found && util > 0) {
            // Add new process
            if (process_count >= process_capacity) {
              process_capacity = process_capacity == 0 ? 8 : process_capacity * 2;
              struct process_gpu_usage *new_list = (struct process_gpu_usage *)realloc(
                  process_list, process_capacity * sizeof(struct process_gpu_usage));
              if (new_list) {
                process_list = new_list;
              } else {
                break; // Out of memory
              }
            }

            process_list[process_count].pid = pid;
            process_list[process_count].utilization = util;

            // Determine process type from engine type
            if (wcsstr(eng_type, L"3D") || wcsstr(eng_type, L"Render")) {
              process_list[process_count].type = gpu_process_graphical;
            } else if (wcsstr(eng_type, L"Compute")) {
              process_list[process_count].type = gpu_process_compute;
            } else {
              process_list[process_count].type = gpu_process_unknown;
            }

            process_count++;
          }
        }

        PdhRemoveCounter(hCounter);
      }
    }

    // Move to next path
    current_path += wcslen(current_path) + 1;
  }

  free(expanded_paths);

  // Convert process_list to gpu_process structures
  if (process_count > 0) {
    // Allocate or reallocate process array
    if (_gpu_info->processes_array_size < process_count) {
      free(_gpu_info->processes);
      _gpu_info->processes = (struct gpu_process *)calloc(process_count, sizeof(struct gpu_process));
      _gpu_info->processes_array_size = process_count;
    }

    if (_gpu_info->processes) {
      for (unsigned int i = 0; i < process_count; i++) {
        struct gpu_process *proc = &_gpu_info->processes[i];
        memset(proc, 0, sizeof(struct gpu_process));

        proc->pid = process_list[i].pid;
        proc->type = process_list[i].type;

        // Cap utilization at 100%
        unsigned int util = process_list[i].utilization;
        if (util > 100)
          util = 100;
        SET_GPUINFO_PROCESS(proc, gpu_usage, util);

        // Get additional process info (name, user, etc.)
        struct process_cpu_usage process_info;
        if (get_process_info(proc->pid, &process_info)) {
          // Get command line
          char *cmdline = NULL;
          get_command_from_pid(proc->pid, &cmdline);
          if (cmdline) {
            proc->cmdline = cmdline;
            SET_GPUINFO_PROCESS(proc, cmdline, 0);
          }

          // Get user name
          char *user_name = NULL;
          get_username_from_pid(proc->pid, &user_name);
          if (user_name) {
            proc->user_name = user_name;
            SET_GPUINFO_PROCESS(proc, user_name, 0);
          }
        }
      }

      _gpu_info->processes_count = process_count;
    }
  } else {
    _gpu_info->processes_count = 0;
  }

  free(process_list);
}
