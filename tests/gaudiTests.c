/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <dlfcn.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/* Exercise the backend with deterministic HLML responses, without a driver. */
#include "../src/extract_gpuinfo_gaudi.c"
void register_gpu_vendor(struct gpu_vendor *vendor) { (void)vendor; }
unsigned nvtop_pcie_gen_from_link_speed(unsigned linkSpeed) {
  (void)linkSpeed;
  return 0;
}

#define CHECK(condition)                                                                                               \
  do {                                                                                                                 \
    if (!(condition)) {                                                                                                \
      fprintf(stderr, "Failed at line %d: %s\n", __LINE__, #condition);                                                \
      return 1;                                                                                                        \
    }                                                                                                                  \
  } while (0)

static hlml_return_t fake_device_count(unsigned *count) {
  *count = 1;
  return HLML_SUCCESS;
}

static hlml_return_t fake_device_handle(unsigned index, hlml_device_t *device) {
  (void)index;
  *device = (void *)0x1;
  return HLML_SUCCESS;
}

static hlml_return_t fake_pci_info(hlml_device_t device, hlml_pci_info_t *pci) {
  (void)device;
  memset(pci, 0, sizeof(*pci));
  snprintf(pci->bus_id, sizeof(pci->bus_id), "0000:19:00.0");
  return HLML_SUCCESS;
}

/* Emulate a newer libhlml whose structure grew by this many bytes. */
#define HLML_TEST_ABI_GROWTH 256

static hlml_return_t growing_pci_info(hlml_device_t device, hlml_pci_info_t *pci) {
  (void)device;
  memset(pci, 0xA5, offsetof(hlml_pci_info_t, reserved) + HLML_TEST_ABI_GROWTH);
  snprintf(pci->bus_id, sizeof(pci->bus_id), "0000:19:00.0");
  return HLML_SUCCESS;
}

static hlml_return_t growing_utilization(hlml_device_t device, hlml_utilization_t *utilization) {
  (void)device;
  memset(utilization, 0xA5, offsetof(hlml_utilization_t, reserved) + HLML_TEST_ABI_GROWTH);
  utilization->aip = 42;
  utilization->memory = 25;
  return HLML_SUCCESS;
}

static hlml_return_t growing_memory_info(hlml_device_t device, hlml_memory_t *memory) {
  (void)device;
  memset(memory, 0xA5, offsetof(hlml_memory_t, reserved) + HLML_TEST_ABI_GROWTH);
  memory->free = 3072;
  memory->total = 4096;
  memory->used = 1024;
  return HLML_SUCCESS;
}

static hlml_return_t fake_minor_number(hlml_device_t device, unsigned *minor) {
  (void)device;
  (void)minor;
  return HLML_ERROR_NOT_SUPPORTED;
}

static unsigned shutdown_calls;
static hlml_return_t fake_shutdown(void) {
  ++shutdown_calls;
  return HLML_SUCCESS;
}

/* The backend calls dlclose at shutdown; use a real handle for the main program. */
static void install_fake_hlml(void) {
  libhlml_handle = dlopen(NULL, RTLD_LAZY);
  hlml_shutdown = fake_shutdown;
  hlml_device_get_count = fake_device_count;
  hlml_device_get_handle_by_index = fake_device_handle;
  hlml_device_get_pci_info = fake_pci_info;
  hlml_device_get_minor_number = fake_minor_number;
}

static int test_double_enumeration(void) {
  LIST_HEAD(first);
  LIST_HEAD(second);
  unsigned count = 0;

  shutdown_calls = 0;
  install_fake_hlml();
  CHECK(libhlml_handle != NULL);

  CHECK(gpuinfo_gaudi_get_device_handles(&first, &count));
  CHECK(count == 1);
  CHECK(gaudi_device_count == 1);
  CHECK(!list_empty(&first));

  /* Repeated enumeration must be rejected instead of indexing past the new array. */
  CHECK(!gpuinfo_gaudi_get_device_handles(&second, &count));
  CHECK(count == 0);
  CHECK(list_empty(&second));
  CHECK(strstr(gpuinfo_gaudi_last_error_string(), "already enumerated") != NULL);

  list_del(&gaudi_devices[0].base.list);
  gpuinfo_gaudi_shutdown();
  CHECK(shutdown_calls == 1);
  CHECK(gaudi_devices == NULL);
  CHECK(gaudi_device_count == 0);

  /* After a shutdown, a new enumeration must be possible. */
  install_fake_hlml();
  CHECK(gpuinfo_gaudi_get_device_handles(&first, &count));
  CHECK(count == 1);
  list_del(&gaudi_devices[0].base.list);
  gpuinfo_gaudi_shutdown();
  CHECK(shutdown_calls == 2);
  return 0;
}

static int test_abi_growth_tolerance(void) {
  LIST_HEAD(devices);
  unsigned count = 0;
  struct gpuinfo_dynamic_info dynamic;

  shutdown_calls = 0;
  install_fake_hlml();
  hlml_device_get_pci_info = growing_pci_info;
  hlml_device_get_utilization_rates = growing_utilization;
  hlml_device_get_memory_info = growing_memory_info;

  CHECK(gpuinfo_gaudi_get_device_handles(&devices, &count));
  CHECK(count == 1);
  CHECK(!strcmp(gaudi_devices[0].base.pdev, "0000:19:00.0"));

  gaudi_sample_device(&gaudi_devices[0], &dynamic);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&dynamic, gpu_util_rate));
  CHECK(dynamic.gpu_util_rate == 42);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&dynamic, total_memory));
  CHECK(dynamic.total_memory == 4096);
  CHECK(dynamic.used_memory == 1024);
  CHECK(dynamic.free_memory == 3072);
  CHECK(dynamic.mem_util_rate == 25);

  list_del(&gaudi_devices[0].base.list);
  gpuinfo_gaudi_shutdown();
  CHECK(shutdown_calls == 1);
  return 0;
}

int main(void) {
  CHECK(test_double_enumeration() == 0);
  CHECK(test_abi_growth_tolerance() == 0);
  puts("Gaudi backend tests passed");
  return 0;
}
