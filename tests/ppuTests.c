/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/* Exercise the backend with deterministic HGML responses, without a driver. */
#include "../src/extract_gpuinfo_ppu.c"
void register_gpu_vendor(struct gpu_vendor *vendor) { (void)vendor; }

/* HGML defines the device handle as a one-pointer wrapper struct. */
_Static_assert(sizeof(hgmlDevice_t) == sizeof(void *), "unexpected HGML device handle size");
_Static_assert(offsetof(hgmlDevice_t, handle) == 0, "unexpected HGML device handle layout");

/* HGML API version 13 trades four busId bytes for the extra globalPpuId field. */
_Static_assert(sizeof(hgmlPciInfo_t) == 68, "unexpected HGML PCI info size");
_Static_assert(offsetof(hgmlPciInfo_t, globalPpuId) == 64, "unexpected HGML PCI info layout");

#define CHECK(condition)                                                                                               \
  do {                                                                                                                 \
    if (!(condition)) {                                                                                                \
      fprintf(stderr, "Failed at line %d: %s\n", __LINE__, #condition);                                                \
      return 1;                                                                                                        \
    }                                                                                                                  \
  } while (0)

static int device_count(unsigned *count) {
  *count = 1;
  return 0;
}
static int device_handle(unsigned index, hgmlDevice_t *device) {
  (void)index;
  device->handle = NULL;
  return 0;
}
static const char *pci_bus_id = "00000000:A8:00.1";
static int pci_status;
static int pci_query(hgmlDevice_t device, hgmlPciInfo_t *pci) {
  (void)device;
  if (pci_status)
    return pci_status;
  snprintf(pci->busId, sizeof(pci->busId), "%s", pci_bus_id);
  pci->globalPpuId = 42;
  return 0;
}

static int test_pci_info(const char *bus_id, int status, const char *expected_pdev) {
  LIST_HEAD(devices);
  unsigned count;
  pci_bus_id = bus_id;
  pci_status = status;
  hgmlDeviceGetCount = device_count;
  hgmlDeviceGetHandleByIndex = device_handle;
  hgmlDeviceGetPciInfo = pci_query;
  CHECK(ppu_get_devices(&devices, &count));
  CHECK(count == 1);
  CHECK(!strcmp(ppu_devices[0].base.pdev, expected_pdev));
  list_del(&ppu_devices[0].base.list);
  ppu_shutdown();
  pci_bus_id = "00000000:A8:00.1";
  pci_status = 0;
  return 0;
}

static int test_pci_info_missing_symbol(void) {
  LIST_HEAD(devices);
  unsigned count;
  hgmlDeviceGetCount = device_count;
  hgmlDeviceGetHandleByIndex = device_handle;
  hgmlDeviceGetPciInfo = NULL;
  CHECK(ppu_get_devices(&devices, &count));
  CHECK(count == 1);
  CHECK(!strcmp(ppu_devices[0].base.pdev, "ppu-0"));
  list_del(&ppu_devices[0].base.list);
  ppu_shutdown();
  return 0;
}

static int test_double_enumeration(void) {
  LIST_HEAD(first);
  LIST_HEAD(second);
  unsigned count;
  hgmlDeviceGetCount = device_count;
  hgmlDeviceGetHandleByIndex = device_handle;
  hgmlDeviceGetPciInfo = NULL;
  CHECK(ppu_get_devices(&first, &count));
  CHECK(count == 1);
  CHECK(!ppu_get_devices(&second, &count));
  CHECK(count == 0);
  CHECK(strstr(ppu_last_error(), "already enumerated") != NULL);
  list_del(&ppu_devices[0].base.list);
  ppu_shutdown();
  return 0;
}

static int test_shutdown_clears_symbols(void) {
  hgmlDeviceGetCount = device_count;
  hgmlDeviceGetHandleByIndex = device_handle;
  hgmlDeviceGetPciInfo = pci_query;
  ppu_shutdown();
  CHECK(hgmlDeviceGetCount == NULL);
  CHECK(hgmlDeviceGetHandleByIndex == NULL);
  CHECK(hgmlDeviceGetPciInfo == NULL);
  return 0;
}

static bool fail_metrics;
static bool invalid_memory;
static unsigned memory_v1_calls;
static int memory_query(hgmlDevice_t device, hgmlMemory_t *memory) {
  (void)device;
  ++memory_v1_calls;
  if (invalid_memory)
    *memory = (hgmlMemory_t){.total = 4096, .used = 8192, .free = 0};
  else
    *memory = (hgmlMemory_t){.total = 4096, .used = 1024, .free = 3072};
  return fail_metrics ? 3 : 0;
}
static unsigned memory_v2_calls;
static int memory_v2_status;
static unsigned long long memory_v2_total = 8192;
static unsigned long long memory_v2_used = 2048;
static int memory_v2_query(hgmlDevice_t device, hgmlMemory_v2_t *memory) {
  (void)device;
  ++memory_v2_calls;
  if (memory->version != HGML_MEMORY_V2_VERSION)
    return 25; // HGML_ERROR_ARGUMENT_VERSION_MISMATCH
  if (memory_v2_status)
    return memory_v2_status;
  memory->total = memory_v2_total;
  memory->reserved = 0;
  memory->used = memory_v2_used;
  memory->free = memory_v2_total - memory_v2_used;
  return 0;
}
static int test_memory_v2(struct gpu_info_ppu *ppu) {
  hgmlDeviceGetMemoryInfo = memory_query;
  hgmlDeviceGetMemoryInfo_v2 = memory_v2_query;
  memory_v1_calls = 0;
  memory_v2_calls = 0;
  memory_v2_status = 0;
  ppu_dynamic_info(&ppu->base);
  CHECK(memory_v2_calls == 1);
  CHECK(memory_v1_calls == 0);
  CHECK(ppu->base.dynamic_info.total_memory == memory_v2_total);
  CHECK(ppu->base.dynamic_info.used_memory == memory_v2_used);
  CHECK(ppu->base.dynamic_info.free_memory == memory_v2_total - memory_v2_used);
  CHECK(ppu->base.dynamic_info.mem_util_rate == 25);

  memory_v2_status = 3; // Unsupported; fall back to the v1 query.
  memory_v1_calls = 0;
  memory_v2_calls = 0;
  ppu_dynamic_info(&ppu->base);
  CHECK(memory_v2_calls == 1);
  CHECK(memory_v1_calls == 1);
  CHECK(ppu->base.dynamic_info.total_memory == 4096);
  CHECK(ppu->base.dynamic_info.used_memory == 1024);
  CHECK(ppu->base.dynamic_info.mem_util_rate == 25);

  hgmlDeviceGetMemoryInfo_v2 = NULL;
  hgmlDeviceGetMemoryInfo = memory_query;
  ppu_dynamic_info(&ppu->base);
  CHECK(ppu->base.dynamic_info.total_memory == 4096);

  hgmlDeviceGetMemoryInfo = NULL;
  ppu_dynamic_info(&ppu->base);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu->base.dynamic_info, total_memory));

  hgmlDeviceGetMemoryInfo = memory_query;
  memory_v2_status = 0;
  return 0;
}
static unsigned utilization_gpu = 42;
static int utilization_query(hgmlDevice_t device, hgmlUtilization_t *utilization) {
  (void)device;
  *utilization = (hgmlUtilization_t){.gpu = utilization_gpu, .memory = 99};
  return fail_metrics ? 3 : 0;
}
static int clock_query(hgmlDevice_t device, hgmlClockType_t type, unsigned *clock) {
  (void)device;
  if (type != HGML_CLOCK_SM && type != HGML_CLOCK_MEM)
    return 3;
  *clock = type == HGML_CLOCK_SM ? 200 : 1800;
  return 0;
}
static int name_query(hgmlDevice_t device, char *name, unsigned length) {
  (void)device;
  snprintf(name, length, "PPU-ZW810E");
  return 0;
}
static int max_pcie_gen_query(hgmlDevice_t device, unsigned *value) {
  (void)device;
  *value = 5;
  return 0;
}
static int max_pcie_width_query(hgmlDevice_t device, unsigned *value) {
  (void)device;
  *value = 16;
  return 0;
}
static int temperature_threshold_query(hgmlDevice_t device, hgmlTemperatureThresholds_t type, unsigned *value) {
  (void)device;
  if (type == HGML_TEMPERATURE_THRESHOLD_SHUTDOWN)
    *value = 90;
  else if (type == HGML_TEMPERATURE_THRESHOLD_SLOWDOWN)
    *value = 85;
  else
    return 3;
  return 0;
}

static unsigned temperature_v_calls;
static int temperature_v_status;
static int temperature_v_value;
static int temperature_v_query(hgmlDevice_t device, hgmlTemperature_t *temperature) {
  (void)device;
  ++temperature_v_calls;
  if (temperature->version != HGML_TEMPERATURE_V1_VERSION || temperature->sensorType != HGML_TEMPERATURE_GPU)
    return 25; // HGML_ERROR_ARGUMENT_VERSION_MISMATCH
  if (temperature_v_status)
    return temperature_v_status;
  temperature->temperature = temperature_v_value;
  return 0;
}

static unsigned temperature_calls;
static int temperature_query(hgmlDevice_t device, hgmlTemperatureSensors_t sensor, unsigned *temperature) {
  (void)device;
  ++temperature_calls;
  if (sensor != HGML_TEMPERATURE_GPU)
    return 2; // HGML_ERROR_INVALID_ARGUMENT
  *temperature = 61;
  return 0;
}

static int test_temperature_api(struct gpu_info_ppu *ppu) {
  hgmlDeviceGetTemperatureV = temperature_v_query;
  hgmlDeviceGetTemperature = temperature_query;
  temperature_v_status = 0;
  temperature_v_value = 55;
  temperature_v_calls = 0;
  temperature_calls = 0;
  /* Prefer the versioned query and skip the deprecated one. */
  ppu_dynamic_info(&ppu->base);
  CHECK(temperature_v_calls == 1);
  CHECK(temperature_calls == 0);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&ppu->base.dynamic_info, gpu_temp));
  CHECK(ppu->base.dynamic_info.gpu_temp == 55);

  /* A failed versioned query falls back to the deprecated entry point. */
  temperature_v_status = 3; // HGML_ERROR_NOT_SUPPORTED
  temperature_v_calls = 0;
  temperature_calls = 0;
  ppu_dynamic_info(&ppu->base);
  CHECK(temperature_v_calls == 1);
  CHECK(temperature_calls == 1);
  CHECK(ppu->base.dynamic_info.gpu_temp == 61);

  /* A negative temperature means the sensor is unavailable; do not guess. */
  temperature_v_status = 0;
  temperature_v_value = -1;
  temperature_v_calls = 0;
  temperature_calls = 0;
  ppu_dynamic_info(&ppu->base);
  CHECK(temperature_v_calls == 1);
  CHECK(temperature_calls == 0);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu->base.dynamic_info, gpu_temp));

  /* Older runtimes only expose the deprecated entry point. */
  temperature_v_value = 55;
  hgmlDeviceGetTemperatureV = NULL;
  temperature_calls = 0;
  ppu_dynamic_info(&ppu->base);
  CHECK(temperature_calls == 1);
  CHECK(ppu->base.dynamic_info.gpu_temp == 61);

  hgmlDeviceGetTemperature = NULL;
  ppu_dynamic_info(&ppu->base);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu->base.dynamic_info, gpu_temp));
  return 0;
}

static const char *error_string_query(int status) {
  (void)status;
  return "mocked HGML error";
}

static int test_error_reporting(void) {
  hgmlErrorString = NULL;
  ppu_set_hgml_error(3, "PPU operation failed");
  CHECK(strstr(ppu_last_error(), "PPU operation failed") != NULL);
  CHECK(strstr(ppu_last_error(), "HGML error 3") != NULL);

  hgmlErrorString = error_string_query;
  ppu_set_hgml_error(3, "PPU operation failed");
  CHECK(!strcmp(ppu_last_error(), "PPU operation failed: mocked HGML error"));
  hgmlErrorString = NULL;
  ppu_error = NULL;
  return 0;
}

static int test_static_info(struct gpu_info_ppu *ppu) {
  hgmlDeviceGetName = name_query;
  hgmlDeviceGetMaxPcieLinkGeneration = max_pcie_gen_query;
  hgmlDeviceGetMaxPcieLinkWidth = max_pcie_width_query;
  hgmlDeviceGetTemperatureThreshold = temperature_threshold_query;
  ppu_static_info(&ppu->base);
  CHECK(!strcmp(ppu->base.static_info.device_name, "PPU-ZW810E"));
  CHECK(GPUINFO_STATIC_FIELD_VALID(&ppu->base.static_info, device_name));
  CHECK(ppu->base.static_info.max_pcie_gen == 5);
  CHECK(GPUINFO_STATIC_FIELD_VALID(&ppu->base.static_info, max_pcie_gen));
  CHECK(ppu->base.static_info.max_pcie_link_width == 16);
  CHECK(GPUINFO_STATIC_FIELD_VALID(&ppu->base.static_info, max_pcie_link_width));
  CHECK(ppu->base.static_info.temperature_shutdown_threshold == 90);
  CHECK(GPUINFO_STATIC_FIELD_VALID(&ppu->base.static_info, temperature_shutdown_threshold));
  CHECK(ppu->base.static_info.temperature_slowdown_threshold == 85);
  CHECK(GPUINFO_STATIC_FIELD_VALID(&ppu->base.static_info, temperature_slowdown_threshold));

  /* Fall back to a generic name when the runtime cannot report one. */
  hgmlDeviceGetName = NULL;
  ppu_static_info(&ppu->base);
  CHECK(!strcmp(ppu->base.static_info.device_name, "T-Head PPU"));
  CHECK(GPUINFO_STATIC_FIELD_VALID(&ppu->base.static_info, device_name));
  return 0;
}

static unsigned process_calls;
static bool fail_processes, grow_forever, no_processes;
static int process_query(hgmlDevice_t device, unsigned *count, hgmlProcessInfo_t *processes) {
  (void)device;
  ++process_calls;
  if (fail_processes)
    return 3;
  if (no_processes) {
    *count = 0;
    return 0;
  }
  if (!processes || process_calls == 2 || grow_forever) {
    *count += 1;
    return HGML_ERROR_INSUFFICIENT_SIZE;
  }
  *count = 2;
  processes[0] = (hgmlProcessInfo_t){.pid = 123, .usedGpuMemory = 512};
  processes[1] = (hgmlProcessInfo_t){.pid = 456, .usedGpuMemory = ULLONG_MAX};
  return 0;
}

static hgmlProcessUtilizationSample_t utilization_samples[8];
static unsigned utilization_count, utilization_calls;
static unsigned long long requested_timestamp;
static int utilization_error;
static bool utilization_grows;
static int process_utilization_query(hgmlDevice_t device, hgmlProcessUtilizationSample_t *samples, unsigned *count,
                                     unsigned long long since) {
  (void)device;
  ++utilization_calls;
  requested_timestamp = since;
  if (utilization_error)
    return utilization_error;
  if (utilization_grows || *count < utilization_count) {
    *count = utilization_grows ? *count + 1 : utilization_count;
    return HGML_ERROR_INSUFFICIENT_SIZE;
  }
  *count = utilization_count;
  if (*count)
    memcpy(samples, utilization_samples, *count * sizeof(*samples));
  return HGML_SUCCESS;
}

static int test_process_utilization(struct gpu_info_ppu *ppu) {
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  hgmlDeviceGetProcessUtilization = process_utilization_query;
  utilization_samples[0] = (hgmlProcessUtilizationSample_t){.pid = 123, .timeStamp = 30, .smUtil = 73};
  utilization_samples[1] = (hgmlProcessUtilizationSample_t){.pid = 123, .timeStamp = 10, .smUtil = 20};
  utilization_samples[2] = (hgmlProcessUtilizationSample_t){.pid = 456, .timeStamp = 30, .smUtil = 0};
  utilization_samples[3] = (hgmlProcessUtilizationSample_t){.pid = 999, .timeStamp = 90, .smUtil = 100};
  utilization_samples[4] = (hgmlProcessUtilizationSample_t){.pid = 123, .timeStamp = 50, .smUtil = 101};
  utilization_count = 5;
  ppu_processes(&ppu->base);
  CHECK(utilization_calls == 2);
  CHECK(requested_timestamp == 0);
  CHECK(ppu->last_utilization_timestamp == 30);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  CHECK(ppu->base.processes[0].gpu_usage == 73);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[1], gpu_usage));
  CHECK(ppu->base.processes[1].gpu_usage == 0);
  CHECK(ppu->base.processes[0].gpu_memory_usage == 512);
  /* Repeated and out-of-range samples must not preserve the previous 73%;
   * they fall back to 0% like an idle process. */
  ppu_processes(&ppu->base);
  CHECK(requested_timestamp == 30);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  CHECK(ppu->base.processes[0].gpu_usage == 0);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[1], gpu_usage));
  CHECK(ppu->base.processes[1].gpu_usage == 0);
  utilization_samples[0].timeStamp = 40;
  utilization_samples[0].smUtil = 88;
  ppu_processes(&ppu->base);
  CHECK(ppu->base.processes[0].gpu_usage == 88);
  CHECK(ppu->last_utilization_timestamp == 40);
  utilization_error = 3; // Unsupported driver/device
  ppu_processes(&ppu->base);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  CHECK(ppu->last_utilization_timestamp == 40);
  utilization_error = HGML_ERROR_NOT_FOUND; // No samples: all processes were idle
  ppu_processes(&ppu->base);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  CHECK(ppu->base.processes[0].gpu_usage == 0);
  utilization_error = 0;
  utilization_count = 0;
  ppu_processes(&ppu->base);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  CHECK(ppu->base.processes[0].gpu_usage == 0);
  utilization_grows = true;
  utilization_calls = 0;
  ppu_processes(&ppu->base);
  CHECK(utilization_calls == 4);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  utilization_grows = false;
  hgmlDeviceGetProcessUtilization = NULL;
  ppu_processes(&ppu->base);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  return 0;
}

/* Also usable on a PPU host to inspect real measurements without curses. */
static int live_test(void) {
  LIST_HEAD(devices);
  unsigned count;
  CHECK(ppu_init());
  CHECK(ppu_get_devices(&devices, &count));
  CHECK(count > 0);
  printf("devices=%u\n", count);
  struct gpu_info *device, *next;
  list_for_each_entry(device, &devices, list) {
    ppu_static_info(device);
    ppu_dynamic_info(device);
    ppu_processes(device);
    CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, total_memory));
    CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, power_draw));
    printf("%s %s %s memory=%llu/%llu util=%u temp=%u power_mW=%u limit_mW=%u processes=%u\n", DEVICE_UNIT_NAME(device),
           device->pdev, device->static_info.device_name, device->dynamic_info.used_memory,
           device->dynamic_info.total_memory, device->dynamic_info.gpu_util_rate, device->dynamic_info.gpu_temp,
           device->dynamic_info.power_draw, device->dynamic_info.power_draw_max, device->processes_count);
  }
  /* Allow HGML to collect a sample, then exercise multiple incremental reads. */
  for (unsigned round = 0; round < 3; ++round) {
    sleep(1);
    unsigned valid_samples = 0;
    list_for_each_entry(device, &devices, list) {
      ppu_processes(device);
      for (unsigned p = 0; p < device->processes_count; ++p) {
        struct gpu_process *process = &device->processes[p];
        if (!GPUINFO_PROCESS_FIELD_VALID(process, gpu_usage))
          continue;
        CHECK(process->gpu_usage <= 100);
        ++valid_samples;
        printf("round=%u %s pid=%d utilization=%u%%\n", round, device->pdev, process->pid, process->gpu_usage);
      }
    }
    printf("round=%u valid_process_samples=%u\n", round, valid_samples);
  }
  list_for_each_entry_safe(device, next, &devices, list) {
    free(device->processes);
    list_del(&device->list);
  }
  ppu_shutdown();
  ppu_shutdown();
  return 0;
}

int main(int argc, char **argv) {
  if (argc == 2 && !strcmp(argv[1], "--live"))
    return live_test();
  CHECK(test_pci_info("00000000:A8:00.1", 0, "0000:a8:00.1") == 0);
  CHECK(test_pci_info("00010000:A8:00.1", 0, "ppu-0") == 0);
  CHECK(test_pci_info("00000000:A8:00.8", 0, "ppu-0") == 0);
  CHECK(test_pci_info("not-a-pci-address", 0, "ppu-0") == 0);
  CHECK(test_pci_info("00000000:A8:00.1", 3, "ppu-0") == 0);
  CHECK(test_pci_info_missing_symbol() == 0);
  CHECK(test_double_enumeration() == 0);
  CHECK(test_shutdown_clears_symbols() == 0);
  CHECK(test_error_reporting() == 0);

  struct gpu_info_ppu ppu = {0};
  CHECK(test_static_info(&ppu) == 0);
  hgmlDeviceGetClockInfo = clock_query;
  hgmlDeviceGetMemoryInfo = memory_query;
  hgmlDeviceGetUtilizationRates = utilization_query;
  ppu_dynamic_info(&ppu.base);
  CHECK(ppu.base.dynamic_info.gpu_clock_speed == 200);
  CHECK(ppu.base.dynamic_info.mem_clock_speed == 1800);
  CHECK(ppu.base.dynamic_info.gpu_util_rate == 42);
  CHECK(ppu.base.dynamic_info.mem_util_rate == 25);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, gpu_temp));
  utilization_gpu = 101;
  ppu_dynamic_info(&ppu.base);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, gpu_util_rate));
  utilization_gpu = 42;
  invalid_memory = true;
  ppu_dynamic_info(&ppu.base);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, total_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, used_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, free_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, mem_util_rate));
  invalid_memory = false;
  CHECK(test_memory_v2(&ppu) == 0);
  CHECK(test_temperature_api(&ppu) == 0);
  fail_metrics = true;
  ppu_dynamic_info(&ppu.base);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, total_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, gpu_util_rate));
  hgmlDeviceGetComputeRunningProcesses = process_query;
  ppu_processes(&ppu.base);
  CHECK(process_calls == 3);
  CHECK(ppu.base.processes_count == 2);
  CHECK(ppu.base.processes_array_size == ppu.base.processes_count + COMMON_PROCESS_LINEAR_REALLOC_INC);
  CHECK(ppu.base.processes[0].pid == 123);
  CHECK(ppu.base.processes[0].gpu_memory_usage == 512);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&ppu.base.processes[0], gpu_memory_usage));
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&ppu.base.processes[1], gpu_memory_usage));
  CHECK(test_process_utilization(&ppu) == 0);
  fail_processes = true;
  ppu_processes(&ppu.base);
  CHECK(ppu.base.processes_count == 0);
  fail_processes = false;
  grow_forever = true;
  process_calls = 0;
  ppu_processes(&ppu.base);
  CHECK(process_calls == 4);
  CHECK(ppu.base.processes_count == 0);
  grow_forever = false;
  no_processes = true;
  process_calls = 0;
  ppu_processes(&ppu.base);
  CHECK(process_calls == 1);
  CHECK(ppu.base.processes_count == 0);
  no_processes = false;
  free(ppu.base.processes);
  CHECK(!strcmp(processing_unit_name(gpu_processing_unit_ppu), "PPU"));
  ppu_shutdown();
  ppu_shutdown();
  puts("PPU backend tests passed");
  return 0;
}
