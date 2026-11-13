/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <amd_smi/amdsmi.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static bool initialized;
static int gpu;
static uint32_t speed = 8000;
static uint16_t width = 16;
static amdsmi_status_t query_status = AMDSMI_STATUS_SUCCESS;
static unsigned queries;

void fake_amdsmi_set_link(uint32_t new_speed, uint16_t new_width, amdsmi_status_t status) {
  speed = new_speed;
  width = new_width;
  query_status = status;
}

amdsmi_status_t amdsmi_init(uint64_t flags) {
  if (getenv("NVTOP_TEST_SMI_INIT_FAIL") || flags != AMDSMI_INIT_AMD_GPUS)
    return AMDSMI_STATUS_NOT_SUPPORTED;
  initialized = true;
  queries = 0;
  return AMDSMI_STATUS_SUCCESS;
}

amdsmi_status_t amdsmi_shut_down(void) {
  initialized = false;
  return AMDSMI_STATUS_SUCCESS;
}

amdsmi_status_t amdsmi_get_processor_handle_from_bdf(amdsmi_bdf_t bdf, amdsmi_processor_handle *handle) {
  if (!initialized)
    return AMDSMI_STATUS_NOT_INIT;
  /* Match a PCI address, deliberately independent of NVTOP/SMI indices. */
  if (bdf.domain_number || bdf.bus_number != 3 || bdf.device_number || bdf.function_number)
    return AMDSMI_STATUS_NOT_FOUND;
  *handle = &gpu;
  return AMDSMI_STATUS_SUCCESS;
}

#ifndef NVTOP_TEST_MISSING_SYMBOL
amdsmi_status_t amdsmi_get_pcie_info(amdsmi_processor_handle handle, amdsmi_pcie_info_t *info) {
  if (!initialized || handle != &gpu)
    return AMDSMI_STATUS_INVAL;
  ++queries;
  const char *mode = getenv("NVTOP_TEST_SMI_MODE");
  if (mode && !strcmp(mode, "fail"))
    return AMDSMI_STATUS_NOT_SUPPORTED;
  if (mode && !strcmp(mode, "invalid")) {
    speed = UINT32_MAX;
    width = UINT16_MAX;
  }
  if (mode && !strcmp(mode, "sequence")) {
    /* Distinct real nvtop refreshes: Gen1 x8, Gen3 x16, fallback, recovery. */
    if (queries == 3)
      return AMDSMI_STATUS_NOT_SUPPORTED;
    speed = queries == 1 ? 2500 : 8000;
    width = queries == 1 ? 8 : 16;
  }
  if (query_status != AMDSMI_STATUS_SUCCESS)
    return query_status;
  memset(info, 0, sizeof(*info));
  info->pcie_static.max_pcie_speed = 64000;
  info->pcie_static.pcie_interface_version = 6;
  info->pcie_static.max_pcie_width = 32;
  info->pcie_metric.pcie_speed = speed;
  info->pcie_metric.pcie_width = width;
  return AMDSMI_STATUS_SUCCESS;
}
#endif
