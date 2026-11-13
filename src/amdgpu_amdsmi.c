/* SPDX-License-Identifier: GPL-3.0-or-later */
#include "nvtop/amdgpu_amdsmi.h"

#ifdef HAVE_AMDSMI_PCIE
#include <amd_smi/amdsmi.h>
#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>

static void *library;
static bool initialized;
static typeof(amdsmi_init) *smi_init;
static typeof(amdsmi_shut_down) *smi_shutdown;
static typeof(amdsmi_get_processor_handle_from_bdf) *smi_device_from_bdf;
static typeof(amdsmi_get_pcie_info) *smi_pcie_info;

void nvtop_amdsmi_shutdown(void) {
  if (initialized)
    smi_shutdown();
  initialized = false;
  if (library)
    dlclose(library);
  library = NULL;
  smi_init = NULL;
  smi_shutdown = NULL;
  smi_device_from_bdf = NULL;
  smi_pcie_info = NULL;
}

void nvtop_amdsmi_init(void) {
  if (initialized)
    return;

  /* Load only the ABI major matching the build headers. No hard ROCm dependency. */
  library = dlopen(AMDSMI_PCIE_SONAME, RTLD_NOW | RTLD_LOCAL);
  if (!library)
    return;
  smi_init = dlsym(library, "amdsmi_init");
  smi_shutdown = dlsym(library, "amdsmi_shut_down");
  smi_device_from_bdf = dlsym(library, "amdsmi_get_processor_handle_from_bdf");
  smi_pcie_info = dlsym(library, "amdsmi_get_pcie_info");
  if (!smi_init || !smi_shutdown || !smi_device_from_bdf || !smi_pcie_info ||
      smi_init(AMDSMI_INIT_AMD_GPUS) != AMDSMI_STATUS_SUCCESS) {
    nvtop_amdsmi_shutdown();
    return;
  }
  initialized = true;
}

void *nvtop_amdsmi_device_from_bdf(const char *address) {
  if (!initialized || !address)
    return NULL;
  unsigned domain, bus, slot, function;
  char trailing;
  if (sscanf(address, "%x:%x:%x.%x%c", &domain, &bus, &slot, &function, &trailing) != 4 ||
      domain > UINT16_MAX || bus > UINT8_MAX || slot > 31 || function > 7)
    return NULL;
  amdsmi_bdf_t bdf = {0};
  bdf.domain_number = domain;
  bdf.bus_number = bus;
  bdf.device_number = slot;
  bdf.function_number = function;
  amdsmi_processor_handle device = NULL;
  if (smi_device_from_bdf(bdf, &device) != AMDSMI_STATUS_SUCCESS)
    return NULL;
  return device;
}

bool nvtop_amdsmi_current_pcie_link(void *device, unsigned *generation, unsigned *width) {
  if (!initialized || !device)
    return false;
  amdsmi_pcie_info_t info = {0};
  if (smi_pcie_info(device, &info) != AMDSMI_STATUS_SUCCESS)
    return false;

  /* Current metrics are in MT/s, unlike raw GPU metrics (0.1 GT/s or an enum).
   * Do not use pcie_static: it describes capability, not the negotiated link. */
  static const unsigned speeds[] = {2500, 5000, 8000, 16000, 32000, 64000};
  unsigned gen = 0;
  for (unsigned i = 0; i < sizeof(speeds) / sizeof(speeds[0]); ++i) {
    if (info.pcie_metric.pcie_speed == speeds[i]) {
      gen = i + 1;
      break;
    }
  }
  unsigned lanes = info.pcie_metric.pcie_width;
  if (!gen || (lanes != 1 && lanes != 2 && lanes != 4 && lanes != 8 && lanes != 12 && lanes != 16 && lanes != 32))
    return false;
  *generation = gen;
  *width = lanes;
  return true;
}
#else
#include <stddef.h>

void nvtop_amdsmi_init(void) {}
void nvtop_amdsmi_shutdown(void) {}
void *nvtop_amdsmi_device_from_bdf(const char *address) {
  (void)address;
  return NULL;
}
bool nvtop_amdsmi_current_pcie_link(void *device, unsigned *generation, unsigned *width) {
  (void)device;
  (void)generation;
  (void)width;
  return false;
}
#endif
