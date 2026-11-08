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

static void setup(void) {
  memory_v2_calls = memory_v1_calls = memory_v2_version = 0;
  memory_total = 8ULL * 1024 * 1024 * 1024;
  memory_v2_status = memory_v1_status = NVML_SUCCESS;
  unavailable_process_memory = false;
  unavailable_graphics_memory = false;
  process_version = 3;
  nvmlDeviceGetBusType = NULL;
  nvmlDeviceGetC2cModeInfoV = NULL;
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
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, total_memory));
  CHECK(gpu.base.dynamic_info.total_memory > 0);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, free_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, used_memory));
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

static bool mixed_processes(void) {
  for (unsigned version = 3; version > 0; --version) {
    setup();
    process_version = version;
    struct gpu_info_nvidia gpu = {0};
    nvmlDeviceGetGraphicsRunningProcesses[version] = graphics_processes;
    nvmlDeviceGetComputeRunningProcesses[version] = compute_processes;
    nvmlDeviceGetMPSComputeRunningProcesses[version] = compute_processes;
    gpuinfo_nvidia_get_running_processes(&gpu.base);
    CHECK(gpu.base.processes_count == 3);
    CHECK(gpu.base.processes[0].pid == 101);
    CHECK(gpu.base.processes[0].type == gpu_process_graphical_compute);
    CHECK(gpu.base.processes[0].gpu_memory_usage == 150);

    LIST_HEAD(devices);
    list_add_tail(&gpu.base.list, &devices);
    gpu.base.static_info.memory_shared_with_host = true;
    SET_GPUINFO_DYNAMIC(&gpu.base.dynamic_info, total_memory, 1000);
    gpuinfo_fix_dynamic_info_from_process_info(&devices);
    CHECK(gpu.base.dynamic_info.used_memory == 650);
    CHECK(gpu.base.dynamic_info.mem_util_rate == 65);
    list_del(&gpu.base.list);
    free(gpu.base.processes);
  }
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
  gpuinfo_nvidia_refresh_dynamic_info(&gpu.base);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_gen));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, pcie_link_width));
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
  gpuinfo_fix_dynamic_info_from_process_info(&devices);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&gpu.base.dynamic_info, used_memory));
  CHECK(gpu.base.dynamic_info.used_memory == 0);
  CHECK(gpu.base.dynamic_info.mem_util_rate == 0);
  list_del(&gpu.base.list);
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
      {"c2c_link", c2c_link},
      {"unavailable_memory", unavailable_memory},
      {"memory_probes", memory_probes},
      {"pcie_preserved", pcie_preserved},
      {"uma_idle", uma_idle},
  };
  for (unsigned i = 0; argc == 2 && i < sizeof(tests) / sizeof(*tests); ++i)
    if (!strcmp(argv[1], tests[i].name))
      return tests[i].run() ? EXIT_SUCCESS : EXIT_FAILURE;
  fprintf(stderr, "Unknown test case\n");
  return EXIT_FAILURE;
}
