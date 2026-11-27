/* SPDX-License-Identifier: GPL-3.0-or-later */

/* Include the backend to inject NVML entry points without a driver or SDK.
 * This exercises the real static/dynamic probes without exposing a test API.
 */
#include "../src/extract_gpuinfo_nvidia.c"
#include "nvtop/extract_gpuinfo.h"

#define CHECK(condition)                                                                                               \
  do {                                                                                                                 \
    if (!(condition)) {                                                                                                \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #condition);                                                  \
      return false;                                                                                                    \
    }                                                                                                                  \
  } while (0)

static nvmlReturn_t memory_v2_status;
static nvmlReturn_t memory_v1_status;
static unsigned memory_v2_calls;
static unsigned memory_v1_calls;
static unsigned memory_v2_version;
static unsigned long long memory_total;
static bool unavailable_process_memory;
static bool unavailable_graphics_memory;
static unsigned process_version;
static unsigned c2c_link_count_value;

static nvmlReturn_t memory_v2(nvmlDevice_t device, nvmlMemory_v2_t *memory) {
  (void)device;
  ++memory_v2_calls;
  memory_v2_version = memory->version;
  /* Reproduce the driver rejecting the bare revision rather than the ABI. */
  if (memory->version != ((unsigned)sizeof(*memory) | (2U << 24)))
    return (nvmlReturn_t)13; // NVML_ERROR_FUNCTION_NOT_FOUND
  if (memory_v2_status == NVML_SUCCESS) {
    memory->total = memory_total;
    memory->used = memory_total / 4;
    memory->free = memory_total - memory->used;
  }
  return memory_v2_status;
}

static nvmlReturn_t memory_v1(nvmlDevice_t device, nvmlMemory_v1_t *memory) {
  (void)device;
  ++memory_v1_calls;
  if (memory_v1_status == NVML_SUCCESS) {
    memory->total = memory_total;
    memory->used = memory_total / 4;
    memory->free = memory_total - memory->used;
  }
  return memory_v1_status;
}

static nvmlReturn_t unsupported_uint(nvmlDevice_t device, unsigned *value) {
  (void)device;
  (void)value;
  return NVML_ERROR_NOT_SUPPORTED;
}

static nvmlReturn_t unsupported_name(nvmlDevice_t device, char *name, unsigned length) {
  (void)device;
  (void)name;
  (void)length;
  return NVML_ERROR_NOT_SUPPORTED;
}

static nvmlReturn_t unsupported_threshold(nvmlDevice_t device, nvmlTemperatureThresholds_t threshold, unsigned *value) {
  (void)threshold;
  return unsupported_uint(device, value);
}

static nvmlReturn_t unsupported_clock(nvmlDevice_t device, nvmlClockType_t clock, unsigned *value) {
  (void)clock;
  return unsupported_uint(device, value);
}

static nvmlReturn_t unsupported_utilization(nvmlDevice_t device, nvmlUtilization_t *value) {
  (void)device;
  (void)value;
  return NVML_ERROR_NOT_SUPPORTED;
}

static nvmlReturn_t unsupported_codec(nvmlDevice_t device, unsigned *value, unsigned *period) {
  (void)period;
  return unsupported_uint(device, value);
}

static nvmlReturn_t unsupported_pcie(nvmlDevice_t device, nvmlPcieUtilCounter_t counter, unsigned *value) {
  (void)counter;
  return unsupported_uint(device, value);
}

static nvmlReturn_t unsupported_temperature(nvmlDevice_t device, nvmlTemperatureSensors_t sensor, unsigned *value) {
  (void)sensor;
  return unsupported_uint(device, value);
}

static unsigned fan_count_value;
static nvmlReturn_t fan_count_status;
static unsigned fan_speed_v2_values[4];
static nvmlReturn_t fan_speed_v2_status[4];
static unsigned fan_speed_v2_calls;
static unsigned legacy_fan_speed_value;
static nvmlReturn_t legacy_fan_speed_status;
static unsigned legacy_fan_speed_calls;

static nvmlReturn_t fan_count(nvmlDevice_t device, unsigned *numFans) {
  (void)device;
  *numFans = fan_count_value;
  return fan_count_status;
}

static nvmlReturn_t fan_speed_v2(nvmlDevice_t device, unsigned fan, unsigned *speed) {
  (void)device;
  ++fan_speed_v2_calls;
  if (fan >= sizeof(fan_speed_v2_values) / sizeof(*fan_speed_v2_values))
    return NVML_ERROR_INVALID_ARGUMENT;
  *speed = fan_speed_v2_values[fan];
  return fan_speed_v2_status[fan];
}

static nvmlReturn_t legacy_fan_speed(nvmlDevice_t device, unsigned *speed) {
  (void)device;
  ++legacy_fan_speed_calls;
  *speed = legacy_fan_speed_value;
  return legacy_fan_speed_status;
}

static void setup(void) {
  memory_v2_calls = memory_v1_calls = memory_v2_version = 0;
  memory_total = 8ULL * 1024 * 1024 * 1024;
  memory_v2_status = memory_v1_status = NVML_SUCCESS;
  unavailable_process_memory = false;
  unavailable_graphics_memory = false;
  process_version = 3;
  c2c_link_count_value = 0;
  nvmlDeviceGetBusType = NULL;
  nvmlDeviceGetC2cModeInfoV = NULL;
  nvmlDeviceGetFieldValues = NULL;
  nvmlDeviceGetNumFans = NULL;
  nvmlDeviceGetFanSpeed_v2 = NULL;
  fan_count_status = legacy_fan_speed_status = NVML_SUCCESS;
  fan_count_value = legacy_fan_speed_value = 0;
  fan_speed_v2_calls = legacy_fan_speed_calls = 0;
  memset(nvmlDeviceGetGraphicsRunningProcesses, 0, sizeof(nvmlDeviceGetGraphicsRunningProcesses));
  memset(nvmlDeviceGetComputeRunningProcesses, 0, sizeof(nvmlDeviceGetComputeRunningProcesses));
  memset(nvmlDeviceGetMPSComputeRunningProcesses, 0, sizeof(nvmlDeviceGetMPSComputeRunningProcesses));
  nvmlDeviceGetMemoryInfo_v2 = memory_v2;
  nvmlDeviceGetMemoryInfo = memory_v1;
  nvmlDeviceGetName = unsupported_name;
  nvmlDeviceGetMaxPcieLinkGeneration = unsupported_uint;
  nvmlDeviceGetMaxPcieLinkWidth = unsupported_uint;
  nvmlDeviceGetTemperatureThreshold = unsupported_threshold;
  nvmlDeviceGetClockInfo = nvmlDeviceGetMaxClockInfo = unsupported_clock;
  nvmlDeviceGetUtilizationRates = unsupported_utilization;
  nvmlDeviceGetEncoderUtilization = nvmlDeviceGetDecoderUtilization = unsupported_codec;
  nvmlDeviceGetCurrPcieLinkGeneration = nvmlDeviceGetCurrPcieLinkWidth = unsupported_uint;
  nvmlDeviceGetPcieThroughput = unsupported_pcie;
  nvmlDeviceGetFanSpeed = nvmlDeviceGetPowerUsage = nvmlDeviceGetEnforcedPowerLimit = unsupported_uint;
  nvmlDeviceGetTemperature = unsupported_temperature;
}

static bool uma_version(void) {
  setup();
  memory_v2_status = memory_v1_status = NVML_ERROR_NOT_SUPPORTED;
  struct gpu_info_nvidia gpu = {0};
  gpuinfo_nvidia_populate_static_info(&gpu.base);
  CHECK(memory_v2_version == ((unsigned)sizeof(nvmlMemory_v2_t) | (2U << 24)));
  CHECK(gpu.base.static_info.memory_shared_with_host);
  // Shared-memory SoCs use the integrated-device display.
  CHECK(gpu.base.static_info.integrated_graphics);
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, system_total_memory));
  CHECK(gpu.base.dynamic_info.system_total_memory > 0);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, system_free_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, total_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, used_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, mem_util_rate));
  struct gpuinfo_memory_display memory_info = gpuinfo_get_memory_display(&gpu.base, false);
  CHECK(memory_info.system_memory);
  CHECK(memory_info.used_memory == memory_info.total_memory - memory_info.free_memory);
  CHECK(memory_info.total_memory_valid);
  CHECK(memory_info.used_memory_valid);
  CHECK(memory_info.free_memory_valid);
  CHECK(memory_info.mem_util_rate_valid);
  return true;
}

static bool legacy_fallback(void) {
  setup();
  memory_v2_status = (nvmlReturn_t)13; // Versioned entry point unavailable.
  memory_v1_status = NVML_ERROR_NOT_SUPPORTED;
  struct gpu_info_nvidia gpu = {0};
  gpuinfo_nvidia_populate_static_info(&gpu.base);
  CHECK(memory_v1_calls == 1);
  CHECK(gpu.base.static_info.memory_shared_with_host);
  return true;
}

static void set_process(void *infos, unsigned index, unsigned pid, unsigned long long memory) {
  if (process_version == 1)
    ((nvmlProcessInfo_v1_t *)infos)[index] = (nvmlProcessInfo_v1_t){.pid = pid, .usedGpuMemory = memory};
  else if (process_version == 2)
    ((nvmlProcessInfo_v2_t *)infos)[index] = (nvmlProcessInfo_v2_t){.pid = pid, .usedGpuMemory = memory};
  else
    ((nvmlProcessInfo_v3_t *)infos)[index] = (nvmlProcessInfo_v3_t){.pid = pid, .usedGpuMemory = memory};
}

static nvmlReturn_t graphics_processes(nvmlDevice_t device, unsigned *count, void *infos) {
  (void)device;
  if (*count < 2) {
    *count = 2;
    return NVML_ERROR_INSUFFICIENT_SIZE;
  }
  set_process(infos, 0, 101, unavailable_graphics_memory ? ULLONG_MAX : 100);
  set_process(infos, 1, 202, 200);
  *count = 2;
  return NVML_SUCCESS;
}

static nvmlReturn_t compute_processes(nvmlDevice_t device, unsigned *count, void *infos) {
  (void)device;
  if (*count < 2) {
    *count = 2;
    return NVML_ERROR_INSUFFICIENT_SIZE;
  }
  set_process(infos, 0, 101, unavailable_process_memory ? ULLONG_MAX : 150);
  set_process(infos, 1, 303, unavailable_process_memory ? ULLONG_MAX : 300);
  *count = 2;
  return NVML_SUCCESS;
}

static nvmlReturn_t empty_processes(nvmlDevice_t device, unsigned *count, void *infos) {
  (void)device;
  (void)infos;
  *count = 0;
  return NVML_SUCCESS;
}

static nvmlReturn_t process_query_status;

static nvmlReturn_t failed_processes(nvmlDevice_t device, unsigned *count, void *infos) {
  (void)device;
  (void)count;
  (void)infos;
  return process_query_status;
}

static bool process_query(void) {
  for (unsigned version = 3; version > 0; --version) {
    setup();
    struct gpu_info_nvidia gpu = {0};
    nvmlDeviceGetComputeRunningProcesses[version] = empty_processes;
    gpuinfo_nvidia_get_running_processes(&gpu.base);
    CHECK(gpu.base.processes_query_succeeded);
    CHECK(gpu.base.processes_count == 0);

    // A later failed query must clear the previous successful-empty result.
    nvmlDeviceGetComputeRunningProcesses[version] = failed_processes;
    const nvmlReturn_t errors[] = {NVML_ERROR_NOT_SUPPORTED, NVML_ERROR_FUNCTION_NOT_FOUND,
                                  NVML_ERROR_NO_PERMISSION, NVML_ERROR_UNKNOWN};
    for (unsigned i = 0; i < sizeof(errors) / sizeof(*errors); ++i) {
      gpu.base.processes_query_succeeded = true;
      process_query_status = errors[i];
      gpuinfo_nvidia_get_running_processes(&gpu.base);
      CHECK(!gpu.base.processes_query_succeeded);
      CHECK(gpu.base.processes_count == 0);
    }

    // One successful query cannot hide an error in another supported query.
    nvmlDeviceGetComputeRunningProcesses[version] = empty_processes;
    nvmlDeviceGetGraphicsRunningProcesses[version] = failed_processes;
    process_query_status = NVML_ERROR_NO_PERMISSION;
    gpuinfo_nvidia_get_running_processes(&gpu.base);
    CHECK(!gpu.base.processes_query_succeeded);
    process_query_status = NVML_ERROR_NOT_SUPPORTED;
    gpuinfo_nvidia_get_running_processes(&gpu.base);
    CHECK(gpu.base.processes_query_succeeded);
  }

  setup();
  struct gpu_info_nvidia gpu = {0};
  gpu.base.processes_query_succeeded = true;
  gpuinfo_nvidia_get_running_processes(&gpu.base);
  CHECK(!gpu.base.processes_query_succeeded);
  return true;
}

static void no_process_query(struct gpu_info *gpu) { (void)gpu; }

static bool empty_process_memory(void) {
  struct gpu_vendor vendor = {.refresh_running_processes = no_process_query};
  struct gpu_info gpu = {.vendor = &vendor};
  gpu.static_info.memory_shared_with_host = true;
  LIST_HEAD(devices);
  list_add_tail(&gpu.list, &devices);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_total_memory, 2000);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_free_memory, 1000);

  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.dynamic_info, used_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.dynamic_info, free_memory));

  gpu.processes_query_succeeded = true;
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.dynamic_info, used_memory));
  CHECK(gpu.dynamic_info.used_memory == 0);
  CHECK(gpu.dynamic_info.free_memory == 1000);

  // The shared refresh must reset success even for a backend with no query.
  gpuinfo_refresh_processes(&devices);
  CHECK(!gpu.processes_query_succeeded);
  RESET_GPUINFO_DYNAMIC(&gpu.dynamic_info, used_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.dynamic_info, free_memory);

  // A successful nonempty list with unavailable allocation data is unknown.
  struct gpu_process process = {0};
  gpu.processes = &process;
  gpu.processes_count = 1;
  gpu.processes_query_succeeded = true;
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.dynamic_info, used_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.dynamic_info, free_memory));

  // Valid zero is distinct from missing allocation data.
  SET_GPUINFO_PROCESS(&process, gpu_memory_usage, 0);
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.dynamic_info, used_memory));
  CHECK(gpu.dynamic_info.used_memory == 0);

  // Driver-reported memory remains authoritative on an empty successful query.
  gpu.processes_count = 0;
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, used_memory, 250);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, free_memory, 750);
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(gpu.dynamic_info.used_memory == 250);
  CHECK(gpu.dynamic_info.free_memory == 750);
  list_del(&gpu.list);
  return true;
}

static bool mixed_processes(void) {
  for (unsigned version = 3; version > 0; --version) {
    setup();
    process_version = version;
    struct gpu_info_nvidia gpu = {0};
    nvmlDeviceGetGraphicsRunningProcesses[version] = graphics_processes;
    nvmlDeviceGetComputeRunningProcesses[version] = compute_processes;
    nvmlDeviceGetMPSComputeRunningProcesses[version] = compute_processes;
    gpuinfo_nvidia_get_running_processes(&gpu.base);
    CHECK(gpu.base.processes_query_succeeded);
    CHECK(gpu.base.processes_count == 3);
    CHECK(gpu.base.processes[0].pid == 101);
    CHECK(gpu.base.processes[0].type == gpu_process_graphical_compute);
    CHECK(gpu.base.processes[0].gpu_memory_usage == 150);

    LIST_HEAD(devices);
    list_add_tail(&gpu.base.list, &devices);
    // A backend that reports only the total falls back to the process list for
    // used memory, which is where the duplicate-PID merge matters.
    SET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, total_memory, 1000);
    gpuinfo_fix_dynamic_info_from_process_info(&devices);
    CHECK(gpu.base.dynamic_info.used_memory == 650);
    // The graph's rate follows the used memory derived from the process list.
    CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, mem_util_rate));
    CHECK(gpu.base.dynamic_info.mem_util_rate == 65);
    list_del(&gpu.base.list);
    free(gpu.base.processes);
  }
  return true;
}

static bool shared_memory_cap(void) {
  struct gpu_vendor vendor = {.refresh_running_processes = no_process_query};
  struct gpu_info gpu = {.vendor = &vendor};
  gpu.static_info.memory_shared_with_host = true;
  LIST_HEAD(devices);
  list_add_tail(&gpu.list, &devices);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_total_memory, 2000);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_free_memory, 1000);
  // Two processes sharing the same buffer each report it.
  struct gpu_process processes[2] = {0};
  SET_GPUINFO_PROCESS(&processes[0], gpu_memory_usage, 800);
  SET_GPUINFO_PROCESS(&processes[1], gpu_memory_usage, 800);
  gpu.processes = processes;
  gpu.processes_count = 2;
  gpu.processes_query_succeeded = true;

  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(gpu.dynamic_info.used_memory == 1000);
  CHECK(gpu.dynamic_info.free_memory == 1000);
  CHECK(gpu.dynamic_info.total_memory == 2000);
  CHECK(gpu.dynamic_info.mem_util_rate == 50);
  CHECK(processes[0].gpu_memory_percentage == 40);

  // A single process above the capped capacity is shown at 100%.
  RESET_GPUINFO_DYNAMIC(&gpu.dynamic_info, used_memory);
  SET_GPUINFO_PROCESS(&processes[0], gpu_memory_usage, 2500);
  gpu.processes_count = 1;
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(gpu.dynamic_info.total_memory == 2000);
  CHECK(processes[0].gpu_memory_percentage == 100);
  SET_GPUINFO_PROCESS(&processes[0], gpu_memory_usage, 800);
  gpu.processes_count = 2;

  // Without a system total, there is nothing to cap against.
  RESET_GPUINFO_DYNAMIC(&gpu.dynamic_info, used_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_total_memory);
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(gpu.dynamic_info.used_memory == 1600);
  CHECK(gpu.dynamic_info.total_memory == 2600);
  return true;
}

static bool shared_memory_buffers(void) {
  struct gpu_vendor vendor = {.refresh_running_processes = no_process_query};
  struct gpu_info gpu = {.vendor = &vendor};
  gpu.static_info.memory_shared_with_host = true;
  LIST_HEAD(devices);
  list_add_tail(&gpu.list, &devices);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_total_memory, 4000);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_free_memory, 1000);
  // A compositor and a client both report the same 600 shared bytes; a third
  // process reports more shared than resident memory (shared is not resident).
  // Each process has a single client here.
  struct gpu_process processes[4] = {0};
  SET_GPUINFO_PROCESS(&processes[0], gpu_memory_usage, 800);
  SET_GPUINFO_PROCESS(&processes[0], gpu_memory_shared, 600);
  processes[0].gpu_memory_shared_max = 600;
  SET_GPUINFO_PROCESS(&processes[1], gpu_memory_usage, 700);
  SET_GPUINFO_PROCESS(&processes[1], gpu_memory_shared, 600);
  processes[1].gpu_memory_shared_max = 600;
  SET_GPUINFO_PROCESS(&processes[2], gpu_memory_usage, 100);
  SET_GPUINFO_PROCESS(&processes[2], gpu_memory_shared, 500);
  processes[2].gpu_memory_shared_max = 500;
  // Without shared accounting, the whole usage is private.
  SET_GPUINFO_PROCESS(&processes[3], gpu_memory_usage, 50);
  gpu.processes = processes;
  gpu.processes_count = 4;
  gpu.processes_query_succeeded = true;

  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  // Private 200 + 100 + 0 + 50, plus the largest shared amount once.
  CHECK(gpu.dynamic_info.used_memory == 950);
  CHECK(gpu.dynamic_info.total_memory == 1950);
  // Per-process values still include their shared buffers.
  CHECK(processes[0].gpu_memory_usage == 800);
  CHECK(processes[0].gpu_memory_percentage == 41);

  // Clients merged per process, as the fdinfo sweep reports them: the first
  // process has two clients holding the same 64 shared bytes, the second a
  // client with more shared than resident memory (clamped to its 10 resident
  // bytes by the sweep) and one with 70 private bytes, the third a single
  // client holding the largest shared amount.
  RESET_GPUINFO_DYNAMIC(&gpu.dynamic_info, used_memory);
  struct gpu_process merged[3] = {0};
  SET_GPUINFO_PROCESS(&merged[0], gpu_memory_usage, 200);
  SET_GPUINFO_PROCESS(&merged[0], gpu_memory_shared, 128);
  merged[0].gpu_memory_shared_max = 64;
  SET_GPUINFO_PROCESS(&merged[1], gpu_memory_usage, 80);
  SET_GPUINFO_PROCESS(&merged[1], gpu_memory_shared, 10);
  merged[1].gpu_memory_shared_max = 10;
  SET_GPUINFO_PROCESS(&merged[2], gpu_memory_usage, 60);
  SET_GPUINFO_PROCESS(&merged[2], gpu_memory_shared, 60);
  merged[2].gpu_memory_shared_max = 60;
  gpu.processes = merged;
  gpu.processes_count = 3;
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  // Private 72 + 70 + 0, plus the largest shared amount of a single client.
  CHECK(gpu.dynamic_info.used_memory == 206);
  return true;
}

static bool shared_memory_accounting(void) {
  setup();
  struct gpu_info_nvidia gpu = {0};
  gpu.base.static_info.memory_shared_with_host = true;
  nvmlDeviceGetGraphicsRunningProcesses[3] = graphics_processes;
  nvmlDeviceGetComputeRunningProcesses[3] = compute_processes;
  gpuinfo_nvidia_get_running_processes(&gpu.base);
  LIST_HEAD(devices);
  list_add_tail(&gpu.base.list, &devices);
  SET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, system_total_memory, 2000);
  SET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, system_free_memory, 350);
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(gpu.base.dynamic_info.used_memory == 650);
  CHECK(gpu.base.dynamic_info.free_memory == 350);
  CHECK(gpu.base.dynamic_info.total_memory == 1000);
  CHECK(gpu.base.dynamic_info.mem_util_rate == 65);
  CHECK(gpu.base.processes[0].gpu_memory_percentage == 15);
  CHECK(gpu.base.dynamic_info.system_total_memory == 2000);
  struct gpuinfo_memory_display memory_info = gpuinfo_get_memory_display(&gpu.base, false);
  CHECK(!memory_info.system_memory);
  CHECK(memory_info.total_memory == 1000);
  // The user can prefer the system RAM even when the GPU memory is available.
  memory_info = gpuinfo_get_memory_display(&gpu.base, true);
  CHECK(memory_info.system_memory);
  CHECK(memory_info.total_memory == 2000);
  CHECK(memory_info.used_memory == 1650);
  CHECK(memory_info.free_memory == 350);
  CHECK(memory_info.mem_util_rate == 82);
  // Without valid system counters, the GPU memory is still displayed.
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, system_free_memory);
  memory_info = gpuinfo_get_memory_display(&gpu.base, true);
  CHECK(!memory_info.system_memory);
  CHECK(memory_info.total_memory == 1000);
  SET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, system_free_memory, 350);
  // Devices with dedicated memory ignore that preference.
  gpu.base.static_info.memory_shared_with_host = false;
  memory_info = gpuinfo_get_memory_display(&gpu.base, true);
  CHECK(!memory_info.system_memory);
  CHECK(memory_info.total_memory == 1000);
  gpu.base.static_info.memory_shared_with_host = true;

  // Missing allocations are skipped; display the sum of reported entries.
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, used_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, total_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, free_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, mem_util_rate);
  RESET_GPUINFO_PROCESS(&gpu.base.processes[2], gpu_memory_usage);
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, used_memory));
  CHECK(gpu.base.dynamic_info.used_memory == 350);
  CHECK(gpu.base.dynamic_info.total_memory == 700);
  CHECK(gpu.base.dynamic_info.free_memory == 350);
  CHECK(gpu.base.dynamic_info.mem_util_rate == 50);
  memory_info = gpuinfo_get_memory_display(&gpu.base, false);
  CHECK(!memory_info.system_memory);
  CHECK(memory_info.total_memory == 700);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&gpu.base.processes[2], gpu_memory_percentage));

  // A nonempty list with no valid allocation entries falls back to RAM.
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, used_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, total_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, free_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, mem_util_rate);
  RESET_GPUINFO_PROCESS(&gpu.base.processes[0], gpu_memory_usage);
  RESET_GPUINFO_PROCESS(&gpu.base.processes[1], gpu_memory_usage);
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, used_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, total_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, free_memory));
  memory_info = gpuinfo_get_memory_display(&gpu.base, false);
  CHECK(memory_info.system_memory);
  CHECK(memory_info.total_memory == 2000);
  CHECK(memory_info.used_memory == 1650);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, used_memory));

  // fdinfo backends can report valid allocations without an empty-query flag.
  SET_GPUINFO_PROCESS(&gpu.base.processes[2], gpu_memory_usage, 300);
  gpu.base.processes_query_succeeded = false;
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, used_memory));
  CHECK(gpu.base.dynamic_info.used_memory == 300);
  CHECK(gpu.base.dynamic_info.total_memory == 650);
  memory_info = gpuinfo_get_memory_display(&gpu.base, false);
  CHECK(!memory_info.system_memory);

  // A zero available-memory estimate is valid, including a fully idle GPU.
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, used_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, total_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, free_memory);
  gpu.base.processes_count = 0;
  gpu.base.processes_query_succeeded = true;
  SET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, system_free_memory, 0);
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, total_memory));
  CHECK(gpu.base.dynamic_info.total_memory == 0);
  CHECK(gpu.base.dynamic_info.used_memory == 0);
  CHECK(gpu.base.dynamic_info.mem_util_rate == 0);

  // An empty list without query success cannot be interpreted as idle.
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, used_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, total_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, free_memory);
  gpu.base.processes_query_succeeded = false;
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, used_memory));
  memory_info = gpuinfo_get_memory_display(&gpu.base, false);
  CHECK(memory_info.system_memory);

  // Do not wrap when adding available memory to a process allocation.
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, used_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, total_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, free_memory);
  gpu.base.processes_count = 1;
  gpu.base.processes_query_succeeded = true;
  SET_GPUINFO_PROCESS(&gpu.base.processes[0], gpu_memory_usage, ULLONG_MAX - 1);
  SET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, system_free_memory, 2);
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, used_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, total_memory));
  list_del(&gpu.base.list);
  free(gpu.base.processes);
  return true;
}

static bool system_memory_counters(void) {
  struct gpu_info gpu = {0};
  unsigned long long used = 42;
  CHECK(!gpuinfo_get_system_used_memory(&gpu.dynamic_info, &used));
  CHECK(used == 42);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_total_memory, 1000);
  CHECK(!gpuinfo_get_system_used_memory(&gpu.dynamic_info, &used));
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_free_memory, 250);
  CHECK(gpuinfo_get_system_used_memory(&gpu.dynamic_info, &used));
  CHECK(used == 750);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_free_memory, 0);
  CHECK(gpuinfo_get_system_used_memory(&gpu.dynamic_info, &used));
  CHECK(used == 1000);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, system_free_memory, 1001);
  CHECK(!gpuinfo_get_system_used_memory(&gpu.dynamic_info, &used));
  CHECK(used == 1000);

  // Reading system RAM must preserve independently reported GPU memory.
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, total_memory, 8000);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, free_memory, 6000);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, used_memory, 2000);
  SET_GPUINFO_DYNAMIC(&gpu.dynamic_info, mem_util_rate, 25);
  gpuinfo_refresh_system_memory(&gpu.dynamic_info);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.dynamic_info, system_total_memory));
  CHECK(gpu.dynamic_info.total_memory == 8000);
  CHECK(gpu.dynamic_info.used_memory == 2000);
  CHECK(gpu.dynamic_info.free_memory == 6000);
  CHECK(gpu.dynamic_info.mem_util_rate == 25);
  // Repeated reads must return fresh valid counters rather than an empty
  // sample.
  struct gpuinfo_dynamic_info next = {0};
  for (unsigned i = 0; i < 3; ++i) {
    RESET_ALL(next.valid);
    gpuinfo_refresh_system_memory(&next);
    CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&next, system_total_memory));
    CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&next, system_free_memory));
    CHECK(next.system_total_memory > 0);
    CHECK(next.system_free_memory <= next.system_total_memory);
  }
  struct gpuinfo_memory_display memory_info = gpuinfo_get_memory_display(&gpu, false);
  CHECK(!memory_info.system_memory);
  CHECK(memory_info.total_memory == 8000);
  // Even a shared-memory driver retains its own complete GPU measurement.
  gpu.static_info.memory_shared_with_host = true;
  memory_info = gpuinfo_get_memory_display(&gpu, false);
  CHECK(!memory_info.system_memory);
  CHECK(memory_info.used_memory == 2000);

  // Reusing the display object must not retain RAM values or their validity.
  RESET_GPUINFO_DYNAMIC(&gpu.dynamic_info, used_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.dynamic_info, free_memory);
  RESET_GPUINFO_DYNAMIC(&gpu.dynamic_info, mem_util_rate);
  memory_info = gpuinfo_get_memory_display(&gpu, false);
  CHECK(memory_info.system_memory);
  gpu.static_info.memory_shared_with_host = false;
  memory_info = gpuinfo_get_memory_display(&gpu, false);
  CHECK(!memory_info.system_memory);
  CHECK(memory_info.total_memory_valid);
  CHECK(memory_info.total_memory == 8000);
  CHECK(!memory_info.used_memory_valid);
  CHECK(!memory_info.free_memory_valid);
  CHECK(!memory_info.mem_util_rate_valid);
  CHECK(memory_info.used_memory == 0);
  CHECK(memory_info.free_memory == 0);
  CHECK(memory_info.mem_util_rate == 0);
  return true;
}

static bool unavailable_memory(void) {
  setup();
  unavailable_process_memory = true;
  struct gpu_info_nvidia gpu = {0};
  nvmlDeviceGetGraphicsRunningProcesses[3] = graphics_processes;
  nvmlDeviceGetComputeRunningProcesses[3] = compute_processes;
  gpuinfo_nvidia_get_running_processes(&gpu.base);
  CHECK(gpu.base.processes_count == 3);
  CHECK(gpu.base.processes[0].gpu_memory_usage == 100);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&gpu.base.processes[0], gpu_memory_usage));
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&gpu.base.processes[2], gpu_memory_usage));
  free(gpu.base.processes);

  // A valid later sample must also repair an unavailable first sample.
  setup();
  unavailable_graphics_memory = true;
  memset(&gpu, 0, sizeof(gpu));
  nvmlDeviceGetGraphicsRunningProcesses[3] = graphics_processes;
  nvmlDeviceGetComputeRunningProcesses[3] = compute_processes;
  gpuinfo_nvidia_get_running_processes(&gpu.base);
  CHECK(gpu.base.processes_count == 3);
  CHECK(gpu.base.processes[0].gpu_memory_usage == 150);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&gpu.base.processes[0], gpu_memory_usage));
  free(gpu.base.processes);
  return true;
}

static nvmlReturn_t pcie_link(nvmlDevice_t device, unsigned *value) {
  (void)device;
  *value = 1; // A SoC can return successful but nonphysical PCIe link values.
  return NVML_SUCCESS;
}

static nvmlReturn_t pcie_bus(nvmlDevice_t device, unsigned *value) {
  (void)device;
  *value = NVML_BUS_TYPE_PCIE;
  return NVML_SUCCESS;
}

static nvmlReturn_t c2c_enabled(nvmlDevice_t device, nvmlC2cModeInfo_v1_t *info) {
  (void)device;
  info->isC2cEnabled = 1;
  return NVML_SUCCESS;
}

static bool c2c_link(void) {
  setup();
  struct gpu_info_nvidia gpu = {0};
  nvmlDeviceGetBusType = pcie_bus;
  nvmlDeviceGetC2cModeInfoV = c2c_enabled;
  nvmlDeviceGetCurrPcieLinkGeneration = nvmlDeviceGetCurrPcieLinkWidth = pcie_link;
  gpuinfo_nvidia_populate_static_info(&gpu.base);
  CHECK(gpu.base.static_info.c2c_attachment);
  CHECK(!GPUINFO_STATIC_FIELD_VALID(&gpu.base.static_info, c2c_link_count));
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_gen));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_width));
  return true;
}

// The C2C link count is reported through the field-value interface; when it is
// available the PCIe link is a placeholder and must not be shown either.
static nvmlReturn_t field_values(nvmlDevice_t device, unsigned count, nvmlFieldValue_t *values) {
  (void)device;
  for (unsigned i = 0; i < count; ++i) {
    values[i].nvmlReturn = NVML_ERROR_NOT_SUPPORTED;
    if (values[i].fieldId == NVML_FI_DEV_C2C_LINK_COUNT) {
      values[i].valueType = NVML_VALUE_TYPE_UNSIGNED_INT;
      // The driver writes only the member identified by valueType. Leave the
      // remaining bytes nonzero to catch reads through a wider union member.
      memset(&values[i].value, 0xa5, sizeof(values[i].value));
      values[i].value.uiVal = c2c_link_count_value;
      values[i].nvmlReturn = NVML_SUCCESS;
    }
  }
  return NVML_SUCCESS;
}

static bool c2c_link_count(void) {
  setup();
  struct gpu_info_nvidia gpu = {0};
  c2c_link_count_value = 8;
  nvmlDeviceGetFieldValues = field_values;
  // GB10 answers the PCIe queries with successful placeholder values.
  nvmlDeviceGetMaxPcieLinkGeneration = nvmlDeviceGetMaxPcieLinkWidth = pcie_link;
  nvmlDeviceGetBusType = pcie_bus;
  nvmlDeviceGetCurrPcieLinkGeneration = nvmlDeviceGetCurrPcieLinkWidth = pcie_link;

  gpuinfo_nvidia_populate_static_info(&gpu.base);
  CHECK(GPUINFO_STATIC_FIELD_VALID(&gpu.base.static_info, c2c_link_count));
  CHECK(gpu.base.static_info.c2c_link_count == 8);
  CHECK(gpu.base.static_info.c2c_attachment);
  CHECK(!GPUINFO_STATIC_FIELD_VALID(&gpu.base.static_info, max_pcie_gen));
  CHECK(!GPUINFO_STATIC_FIELD_VALID(&gpu.base.static_info, max_pcie_link_width));

  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_gen));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_width));

  // A device without C2C links keeps reporting its PCIe link.
  setup();
  memset(&gpu, 0, sizeof(gpu));
  c2c_link_count_value = 0;
  nvmlDeviceGetFieldValues = field_values;
  nvmlDeviceGetMaxPcieLinkGeneration = nvmlDeviceGetMaxPcieLinkWidth = pcie_link;
  nvmlDeviceGetBusType = pcie_bus;
  nvmlDeviceGetCurrPcieLinkGeneration = nvmlDeviceGetCurrPcieLinkWidth = pcie_link;
  gpuinfo_nvidia_populate_static_info(&gpu.base);
  CHECK(!GPUINFO_STATIC_FIELD_VALID(&gpu.base.static_info, c2c_link_count));
  CHECK(!gpu.base.static_info.c2c_attachment);
  CHECK(GPUINFO_STATIC_FIELD_VALID(&gpu.base.static_info, max_pcie_gen));
  CHECK(GPUINFO_STATIC_FIELD_VALID(&gpu.base.static_info, max_pcie_link_width));
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_gen));
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_width));
  return true;
}

static nvmlFieldValue_t c2c_field_sample;
static nvmlReturn_t c2c_field_call_status;

static nvmlReturn_t typed_field_values(nvmlDevice_t device, unsigned count, nvmlFieldValue_t *values) {
  (void)device;
  for (unsigned i = 0; i < count; ++i) {
    values[i].valueType = c2c_field_sample.valueType;
    values[i].value = c2c_field_sample.value;
    values[i].nvmlReturn = c2c_field_sample.nvmlReturn;
  }
  return c2c_field_call_status;
}

static bool c2c_field_types(void) {
  setup();
  nvmlDeviceGetFieldValues = typed_field_values;
  c2c_field_call_status = NVML_SUCCESS;
  const struct {
    nvmlValueType_t type;
    nvmlValue_t value;
    bool valid;
    unsigned long long expected;
  } cases[] = {
      {NVML_VALUE_TYPE_UNSIGNED_INT, {.uiVal = 8}, true, 8},
      {NVML_VALUE_TYPE_UNSIGNED_LONG, {.ulVal = ULONG_MAX}, true, ULONG_MAX},
      {NVML_VALUE_TYPE_UNSIGNED_LONG_LONG, {.ullVal = ULLONG_MAX}, true, ULLONG_MAX},
      {NVML_VALUE_TYPE_UNSIGNED_SHORT, {.usVal = 8}, true, 8},
      {NVML_VALUE_TYPE_UNSIGNED_INT, {.uiVal = 0}, true, 0},
      {NVML_VALUE_TYPE_DOUBLE, {.dVal = 8.0}, false, 0},
      {NVML_VALUE_TYPE_SIGNED_INT, {.siVal = 8}, false, 0},
      {NVML_VALUE_TYPE_SIGNED_LONG_LONG, {.sllVal = -1}, false, 0},
      {NVML_VALUE_TYPE_COUNT, {.ullVal = 8}, false, 0},
  };
  for (unsigned i = 0; i < sizeof(cases) / sizeof(*cases); ++i) {
    c2c_field_sample = (nvmlFieldValue_t){
        .valueType = cases[i].type, .value = cases[i].value, .nvmlReturn = NVML_SUCCESS};
    unsigned long long value = 42;
    CHECK(gpuinfo_nvidia_get_field_value_u64(NULL, NVML_FI_DEV_C2C_LINK_COUNT, 0, &value) == cases[i].valid);
    CHECK(value == (cases[i].valid ? cases[i].expected : 42));
    unsigned expected_count = cases[i].expected > UINT_MAX ? UINT_MAX : (unsigned)cases[i].expected;
    CHECK(gpuinfo_nvidia_c2c_link_count(NULL) == expected_count);
  }

  // Both the overall query and the individual field must succeed.
  c2c_field_sample = (nvmlFieldValue_t){
      .valueType = NVML_VALUE_TYPE_UNSIGNED_INT, .value.uiVal = 8, .nvmlReturn = NVML_ERROR_NOT_SUPPORTED};
  unsigned long long value = 42;
  CHECK(!gpuinfo_nvidia_get_field_value_u64(NULL, NVML_FI_DEV_C2C_LINK_COUNT, 0, &value));
  CHECK(value == 42);
  CHECK(gpuinfo_nvidia_c2c_link_count(NULL) == 0);
  c2c_field_sample.nvmlReturn = NVML_SUCCESS;
  c2c_field_call_status = NVML_ERROR_NO_PERMISSION;
  CHECK(!gpuinfo_nvidia_get_field_value_u64(NULL, NVML_FI_DEV_C2C_LINK_COUNT, 0, &value));
  CHECK(value == 42);
  CHECK(gpuinfo_nvidia_c2c_link_count(NULL) == 0);
  nvmlDeviceGetFieldValues = NULL;
  CHECK(!gpuinfo_nvidia_get_field_value_u64(NULL, NVML_FI_DEV_C2C_LINK_COUNT, 0, &value));
  CHECK(value == 42);
  CHECK(gpuinfo_nvidia_c2c_link_count(NULL) == 0);
  return true;
}

static bool memory_probes(void) {
  // Unknown errors must not turn a discrete GPU into a system-RAM device.
  const struct {
    nvmlReturn_t v2_status;
    nvmlReturn_t v1_status;
    bool v2_present;
    bool v1_present;
    bool zero_total;
    bool shared;
    unsigned legacy_calls;
  } cases[] = {
      {NVML_SUCCESS, NVML_SUCCESS, true, true, false, false, 0},
      {NVML_SUCCESS, NVML_SUCCESS, true, false, false, false, 0},
      {NVML_SUCCESS, NVML_SUCCESS, true, true, true, true, 0},
      {NVML_ERROR_NOT_SUPPORTED, NVML_SUCCESS, true, true, false, true, 0},
      {(nvmlReturn_t)13, NVML_SUCCESS, true, true, false, false, 1},
      {NVML_ERROR_NO_PERMISSION, NVML_ERROR_NO_PERMISSION, true, true, false, false, 1},
      {(nvmlReturn_t)999, (nvmlReturn_t)999, true, true, false, false, 1},
      {NVML_SUCCESS, NVML_ERROR_NOT_SUPPORTED, false, true, false, true, 1},
      {NVML_SUCCESS, NVML_SUCCESS, false, true, false, false, 1},
      {(nvmlReturn_t)13, NVML_SUCCESS, true, false, false, false, 0},
  };
  for (unsigned i = 0; i < sizeof(cases) / sizeof(*cases); ++i) {
    setup();
    memory_v2_status = cases[i].v2_status;
    memory_v1_status = cases[i].v1_status;
    if (!cases[i].v2_present)
      nvmlDeviceGetMemoryInfo_v2 = NULL;
    if (!cases[i].v1_present)
      nvmlDeviceGetMemoryInfo = NULL;
    if (cases[i].zero_total)
      memory_total = 0;
    struct gpu_info_nvidia gpu = {0};
    gpuinfo_nvidia_populate_static_info(&gpu.base);
    CHECK(gpu.base.static_info.memory_shared_with_host == cases[i].shared);
    CHECK(memory_v1_calls == cases[i].legacy_calls);
    gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
    unsigned legacy_calls = cases[i].legacy_calls;
    if (!cases[i].shared && cases[i].v1_present &&
        (!cases[i].v2_present || cases[i].v2_status != NVML_SUCCESS))
      ++legacy_calls;
    CHECK(memory_v1_calls == legacy_calls);
    if (!cases[i].shared) {
      bool have_memory = cases[i].v2_status == NVML_SUCCESS ||
                         (cases[i].v1_present && cases[i].v1_status == NVML_SUCCESS);
      CHECK(!!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, total_memory) == have_memory);
      if (have_memory) {
        CHECK(gpu.base.dynamic_info.total_memory == memory_total);
        CHECK(gpu.base.dynamic_info.used_memory == memory_total / 4);
        CHECK(gpu.base.dynamic_info.mem_util_rate == 25);
      }
    }
  }
  return true;
}

static nvmlReturn_t c2c_unsupported(nvmlDevice_t device, nvmlC2cModeInfo_v1_t *info) {
  (void)device;
  // Failed queries must not be trusted even if the output buffer was written.
  info->isC2cEnabled = 1;
  return NVML_ERROR_NOT_SUPPORTED;
}

static nvmlReturn_t c2c_disabled(nvmlDevice_t device, nvmlC2cModeInfo_v1_t *info) {
  (void)device;
  info->isC2cEnabled = 0;
  return NVML_SUCCESS;
}

static bool pcie_preserved(void) {
  setup();
  struct gpu_info_nvidia gpu = {0};
  nvmlDeviceGetBusType = pcie_bus;
  nvmlDeviceGetC2cModeInfoV = c2c_unsupported;
  nvmlDeviceGetCurrPcieLinkGeneration = nvmlDeviceGetCurrPcieLinkWidth = pcie_link;
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_gen));
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_width));
  nvmlDeviceGetBusType = NULL;
  nvmlDeviceGetC2cModeInfoV = c2c_disabled;
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_gen));
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_width));
  nvmlDeviceGetC2cModeInfoV = NULL;
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_gen));
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_width));
  return true;
}

static bool uma_idle(void) {
  setup();
  memory_v2_status = memory_v1_status = NVML_ERROR_NOT_SUPPORTED;
  struct gpu_info_nvidia gpu = {0};
  LIST_HEAD(devices);
  list_add_tail(&gpu.base.list, &devices);
  gpuinfo_nvidia_populate_static_info(&gpu.base);
  // A previous sample must not survive the next shared-memory refresh.
  SET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, used_memory, 123456);
  SET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, mem_util_rate, 50);
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  nvmlDeviceGetComputeRunningProcesses[3] = empty_processes;
  gpuinfo_nvidia_get_running_processes(&gpu.base);
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, used_memory));
  CHECK(gpu.base.processes_query_succeeded);
  CHECK(gpu.base.dynamic_info.used_memory == 0);
  CHECK(gpu.base.dynamic_info.total_memory == gpu.base.dynamic_info.system_free_memory);
  CHECK(gpu.base.dynamic_info.free_memory == gpu.base.dynamic_info.system_free_memory);
  CHECK(gpu.base.dynamic_info.mem_util_rate == 0);
  list_del(&gpu.base.list);
  return true;
}

static bool frozen_process_memory(void) {
  setup();
  struct gpu_info_nvidia gpu = {0};
  gpu.base.static_info.memory_shared_with_host = true;
  struct gpu_process process = {0};
  SET_GPUINFO_PROCESS(&process, gpu_memory_usage, 100);
  gpu.base.processes = &process;
  gpu.base.processes_count = 1;
  gpu.base.processes_query_succeeded = true;
  for (unsigned free = 300; free <= 600; free += 300) {
    // The kill menu refreshes dynamic data while retaining the process list.
    gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
    SET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, system_free_memory, free);
    gpuinfo_fix_shared_memory_from_process_info(&gpu.base);
    struct gpuinfo_memory_display memory = gpuinfo_get_memory_display(&gpu.base, false);
    CHECK(!memory.system_memory);
    CHECK(memory.used_memory == 100);
    CHECK(memory.total_memory == free + 100);
    CHECK(memory.free_memory == free);
    CHECK(gpu.base.processes_count == 1);
  }
  // A frozen, successfully queried empty list must also keep reporting MEM.
  gpu.base.processes_count = 0;
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  gpuinfo_fix_shared_memory_from_process_info(&gpu.base);
  struct gpuinfo_memory_display memory = gpuinfo_get_memory_display(&gpu.base, false);
  CHECK(!memory.system_memory);
  CHECK(memory.used_memory == 0);
  CHECK(memory.total_memory == gpu.base.dynamic_info.system_free_memory);
  return true;
}

static unsigned utilization_calls;

static nvmlReturn_t utilization_not_found(nvmlDevice_t device, nvmlProcessUtilizationSample_t *utilization,
                                          unsigned int *count, unsigned long long last_seen) {
  (void)device;
  (void)utilization;
  (void)last_seen;
  ++utilization_calls;
  *count = 0;
  return NVML_ERROR_NOT_FOUND;
}

static nvmlReturn_t utilization_fetch_not_found(nvmlDevice_t device, nvmlProcessUtilizationSample_t *utilization,
                                                unsigned int *count, unsigned long long last_seen) {
  (void)device;
  (void)last_seen;
  ++utilization_calls;
  if (!utilization) {
    *count = 2;
    return NVML_ERROR_INSUFFICIENT_SIZE;
  }
  *count = 0;
  return NVML_ERROR_NOT_FOUND;
}

static nvmlReturn_t utilization_one_sample(nvmlDevice_t device, nvmlProcessUtilizationSample_t *utilization,
                                           unsigned int *count, unsigned long long last_seen) {
  (void)device;
  ++utilization_calls;
  if (!utilization) {
    *count = 1;
    return NVML_ERROR_INSUFFICIENT_SIZE;
  }
  *count = 1;
  utilization[0].pid = 101;
  utilization[0].timeStamp = last_seen + 1;
  utilization[0].smUtil = 50;
  utilization[0].memUtil = 0;
  utilization[0].encUtil = 10;
  utilization[0].decUtil = 20;
  return NVML_SUCCESS;
}

static bool check_idle(struct gpu_process *process) {
  CHECK(GPUINFO_PROCESS_FIELD_VALID(process, gpu_usage));
  CHECK(process->gpu_usage == 0);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(process, encode_usage));
  CHECK(process->encode_usage == 0);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(process, decode_usage));
  CHECK(process->decode_usage == 0);
  return true;
}

// Some drivers report "no samples" from the size probe already.
static bool utilization_probe_not_found(void) {
  setup();
  struct gpu_info_nvidia gpu = {0};
  struct gpu_process procs[2] = {0};
  procs[0].pid = 101;
  procs[1].pid = 202;
  utilization_calls = 0;
  nvmlDeviceGetProcessUtilization = utilization_not_found;
  gpuinfo_nvidia_get_process_utilization(&gpu, 2, procs);
  CHECK(utilization_calls == 1);
  for (unsigned i = 0; i < 2; ++i)
    CHECK(check_idle(&procs[i]));
  return true;
}

// Others report it from the buffered query instead.
static bool utilization_buffered_not_found(void) {
  setup();
  struct gpu_info_nvidia gpu = {0};
  struct gpu_process procs[2] = {0};
  procs[0].pid = 101;
  procs[1].pid = 202;
  utilization_calls = 0;
  nvmlDeviceGetProcessUtilization = utilization_fetch_not_found;
  gpuinfo_nvidia_get_process_utilization(&gpu, 2, procs);
  CHECK(utilization_calls == 2);
  for (unsigned i = 0; i < 2; ++i)
    CHECK(check_idle(&procs[i]));
  return true;
}

// Samples must still be applied when the driver has them.
static bool utilization_samples(void) {
  setup();
  struct gpu_info_nvidia gpu = {0};
  struct gpu_process procs[2] = {0};
  procs[0].pid = 101;
  procs[1].pid = 202;
  utilization_calls = 0;
  nvmlDeviceGetProcessUtilization = utilization_one_sample;
  gpuinfo_nvidia_get_process_utilization(&gpu, 2, procs);
  CHECK(utilization_calls == 2);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&procs[0], gpu_usage));
  CHECK(procs[0].gpu_usage == 50);
  CHECK(procs[0].encode_usage == 10);
  CHECK(procs[0].decode_usage == 20);
  CHECK(check_idle(&procs[1]));
  CHECK(gpu.last_utilization_timestamp == 1);
  return true;
}

// A card with unplugged fan sockets: those report 0%, the plugged one does
// not, and the maximum surfaces the fan that actually spins.
static bool multi_fan_max(void) {
  setup();
  fan_count_value = 3;
  fan_speed_v2_status[0] = fan_speed_v2_status[2] = NVML_SUCCESS;
  fan_speed_v2_status[1] = NVML_ERROR_NOT_SUPPORTED;
  fan_speed_v2_values[0] = 0;
  fan_speed_v2_values[1] = 0;
  fan_speed_v2_values[2] = 63;
  nvmlDeviceGetNumFans = fan_count;
  nvmlDeviceGetFanSpeed_v2 = fan_speed_v2;
  nvmlDeviceGetFanSpeed = legacy_fan_speed;
  struct gpu_info_nvidia gpu = {0};
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(fan_speed_v2_calls == 3);
  CHECK(legacy_fan_speed_calls == 0);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, fan_speed));
  CHECK(gpu.base.dynamic_info.fan_speed == 63);
  return true;
}

// When every per-fan query fails, keep using the single-fan legacy query.
static bool multi_fan_legacy_fallback(void) {
  setup();
  fan_count_value = 2;
  fan_speed_v2_status[0] = fan_speed_v2_status[1] = NVML_ERROR_NOT_SUPPORTED;
  legacy_fan_speed_value = 48;
  nvmlDeviceGetNumFans = fan_count;
  nvmlDeviceGetFanSpeed_v2 = fan_speed_v2;
  nvmlDeviceGetFanSpeed = legacy_fan_speed;
  struct gpu_info_nvidia gpu = {0};
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(fan_speed_v2_calls == 2);
  CHECK(legacy_fan_speed_calls == 1);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, fan_speed));
  CHECK(gpu.base.dynamic_info.fan_speed == 48);
  return true;
}

// Devices without a fan (e.g. shared-memory SoCs) report zero fans and fall
// back to the legacy query, which leaves the field unknown when it fails.
static bool fan_count_zero(void) {
  setup();
  fan_count_value = 0;
  legacy_fan_speed_status = NVML_ERROR_NOT_SUPPORTED;
  nvmlDeviceGetNumFans = fan_count;
  nvmlDeviceGetFanSpeed_v2 = fan_speed_v2;
  nvmlDeviceGetFanSpeed = legacy_fan_speed;
  struct gpu_info_nvidia gpu = {0};
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(fan_speed_v2_calls == 0);
  CHECK(legacy_fan_speed_calls == 1);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, fan_speed));
  return true;
}

int main(int argc, char **argv) {
  const struct {
    const char *name;
    bool (*run)(void);
  } tests[] = {
      {"uma_version", uma_version},
      {"legacy_fallback", legacy_fallback},
      {"mixed_processes", mixed_processes},
      {"process_query", process_query},
      {"empty_process_memory", empty_process_memory},
      {"shared_memory_accounting", shared_memory_accounting},
      {"shared_memory_cap", shared_memory_cap},
      {"shared_memory_buffers", shared_memory_buffers},
      {"system_memory_counters", system_memory_counters},
      {"c2c_link", c2c_link},
      {"c2c_link_count", c2c_link_count},
      {"c2c_field_types", c2c_field_types},
      {"unavailable_memory", unavailable_memory},
      {"memory_probes", memory_probes},
      {"pcie_preserved", pcie_preserved},
      {"uma_idle", uma_idle},
      {"frozen_process_memory", frozen_process_memory},
      {"utilization_probe_not_found", utilization_probe_not_found},
      {"utilization_buffered_not_found", utilization_buffered_not_found},
      {"utilization_samples", utilization_samples},
      {"multi_fan_max", multi_fan_max},
      {"multi_fan_legacy_fallback", multi_fan_legacy_fallback},
      {"fan_count_zero", fan_count_zero},
  };
  for (unsigned i = 0; argc == 2 && i < sizeof(tests) / sizeof(*tests); ++i)
    if (!strcmp(argv[1], tests[i].name))
      return tests[i].run() ? EXIT_SUCCESS : EXIT_FAILURE;
  fprintf(stderr, "Unknown test case\n");
  return EXIT_FAILURE;
}
