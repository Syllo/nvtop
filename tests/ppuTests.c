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

#define CHECK(condition) do { if (!(condition)) { \
  fprintf(stderr, "Failed at line %d: %s\n", __LINE__, #condition); return 1; \
} } while (0)

static int device_count(unsigned *count) { *count = 1; return 0; }
static int device_handle(unsigned index, hgmlDevice_t *device) {
  (void)index;
  device->handle = NULL;
  return 0;
}
static int pci_query(hgmlDevice_t device, hgmlPciInfo_t *pci) {
  (void)device;
  strcpy(pci->busId, "00000000:A8:00.1");
  pci->globalPpuId = 42;
  return 0;
}

static bool fail_metrics;
static int memory_query(hgmlDevice_t device, hgmlMemory_t *memory) {
  (void)device;
  *memory = (hgmlMemory_t){.total = 4096, .used = 1024, .free = 3072};
  return fail_metrics ? 3 : 0;
}
static int utilization_query(hgmlDevice_t device, hgmlUtilization_t *utilization) {
  (void)device;
  *utilization = (hgmlUtilization_t){.gpu = 42, .memory = 99};
  return fail_metrics ? 3 : 0;
}
static int clock_query(hgmlDevice_t device, hgmlClockType_t type, unsigned *clock) {
  (void)device;
  if (type != HGML_CLOCK_SM && type != HGML_CLOCK_MEM)
    return 3;
  *clock = type == HGML_CLOCK_SM ? 200 : 1800;
  return 0;
}
static unsigned process_calls;
static bool fail_processes, grow_forever;
static int process_query(hgmlDevice_t device, unsigned *count, hgmlProcessInfo_t *processes) {
  (void)device;
  ++process_calls;
  if (fail_processes)
    return 3;
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
static int process_utilization_query(hgmlDevice_t device, hgmlProcessUtilizationSample_t *samples,
                                     unsigned *count, unsigned long long since) {
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
  /* Repeated and out-of-range samples must not preserve the previous 73%. */
  ppu_processes(&ppu->base);
  CHECK(requested_timestamp == 30);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[1], gpu_usage));
  utilization_samples[0].timeStamp = 40;
  utilization_samples[0].smUtil = 88;
  ppu_processes(&ppu->base);
  CHECK(ppu->base.processes[0].gpu_usage == 88);
  CHECK(ppu->last_utilization_timestamp == 40);
  utilization_error = 3; // Unsupported driver/device
  ppu_processes(&ppu->base);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  CHECK(ppu->last_utilization_timestamp == 40);
  utilization_error = 6; // No samples found
  ppu_processes(&ppu->base);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
  utilization_error = 0;
  utilization_count = 0;
  ppu_processes(&ppu->base);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&ppu->base.processes[0], gpu_usage));
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
    printf("%s %s %s memory=%llu/%llu util=%u temp=%u power_mW=%u limit_mW=%u processes=%u\n",
           DEVICE_UNIT_NAME(device), device->pdev, device->static_info.device_name,
           device->dynamic_info.used_memory, device->dynamic_info.total_memory,
           device->dynamic_info.gpu_util_rate, device->dynamic_info.gpu_temp,
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
  LIST_HEAD(devices);
  unsigned count;
  hgmlDeviceGetCount = device_count;
  hgmlDeviceGetHandleByIndex = device_handle;
  hgmlDeviceGetPciInfo = pci_query;
  CHECK(ppu_get_devices(&devices, &count));
  CHECK(count == 1);
  CHECK(!strcmp(ppu_devices[0].base.pdev, "0000:a8:00.1"));
  list_del(&ppu_devices[0].base.list);
  ppu_shutdown();
  struct gpu_info_ppu ppu = {0};
  hgmlDeviceGetClockInfo = clock_query;
  hgmlDeviceGetMemoryInfo = memory_query;
  hgmlDeviceGetUtilizationRates = utilization_query;
  ppu_dynamic_info(&ppu.base);
  CHECK(ppu.base.dynamic_info.gpu_clock_speed == 200);
  CHECK(ppu.base.dynamic_info.mem_clock_speed == 1800);
  CHECK(ppu.base.dynamic_info.gpu_util_rate == 42);
  CHECK(ppu.base.dynamic_info.mem_util_rate == 25);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, gpu_temp));
  fail_metrics = true;
  ppu_dynamic_info(&ppu.base);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, total_memory));
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&ppu.base.dynamic_info, gpu_util_rate));
  hgmlDeviceGetComputeRunningProcesses = process_query;
  ppu_processes(&ppu.base);
  CHECK(process_calls == 3);
  CHECK(ppu.base.processes_count == 2);
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
  free(ppu.base.processes);
  CHECK(!strcmp(processing_unit_name(gpu_processing_unit_ppu), "PPU"));
  ppu_shutdown();
  ppu_shutdown();
  puts("PPU backend tests passed");
  return 0;
}
