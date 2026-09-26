/* SPDX-License-Identifier: GPL-3.0-or-later */
#include "nvtop/amdgpu_amdsmi.h"
#include <amd_smi/amdsmi.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Keep checks active in Release builds. */
#define CHECK(expression)                                                                                               \
  do {                                                                                                                 \
    if (!(expression)) {                                                                                               \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #expression);                                                    \
      exit(EXIT_FAILURE);                                                                                              \
    }                                                                                                                  \
  } while (0)

int main(int argc, char **argv) {
  unsigned gen = 99, lanes = 99;
  CHECK(!nvtop_amdsmi_current_pcie_link(NULL, &gen, &lanes));
  nvtop_amdsmi_init();
  if (argc == 2 && !strcmp(argv[1], "unavailable")) {
    CHECK(!nvtop_amdsmi_device_from_bdf("0000:03:00.0"));
    CHECK(!nvtop_amdsmi_current_pcie_link(NULL, &gen, &lanes));
    nvtop_amdsmi_shutdown();
    puts("PASS: unavailable AMD SMI requests sysfs fallback");
    return 0;
  }
  void *device = nvtop_amdsmi_device_from_bdf("0000:03:00.0");
  CHECK(device);
  CHECK(!nvtop_amdsmi_device_from_bdf("0000:02:00.0"));
  CHECK(!nvtop_amdsmi_device_from_bdf("0000:03:00.0junk"));
  CHECK(!nvtop_amdsmi_device_from_bdf("0000:103:00.0"));
  CHECK(!nvtop_amdsmi_device_from_bdf(NULL));
  void *lib = dlopen(AMDSMI_PCIE_SONAME, RTLD_NOW | RTLD_LOCAL);
  CHECK(lib);
  void (*set_link)(uint32_t, uint16_t, amdsmi_status_t) = dlsym(lib, "fake_amdsmi_set_link");
  CHECK(set_link);

  const unsigned speeds[] = {2500, 5000, 8000, 16000, 32000, 64000};
  for (unsigned i = 0; i < sizeof(speeds) / sizeof(speeds[0]); ++i) {
    set_link(speeds[i], 16, AMDSMI_STATUS_SUCCESS);
    CHECK(nvtop_amdsmi_current_pcie_link(device, &gen, &lanes));
    CHECK(gen == i + 1 && lanes == 16);
  }
  /* Same cached device handle, changing current metrics on every call. */
  set_link(2500, 8, AMDSMI_STATUS_SUCCESS);
  CHECK(nvtop_amdsmi_current_pcie_link(device, &gen, &lanes));
  CHECK(gen == 1 && lanes == 8);
  set_link(8000, 16, AMDSMI_STATUS_SUCCESS);
  CHECK(nvtop_amdsmi_current_pcie_link(device, &gen, &lanes));
  CHECK(gen == 3 && lanes == 16);

  const unsigned bad_speeds[] = {0, 80, 3, 12345, UINT32_MAX};
  for (unsigned i = 0; i < sizeof(bad_speeds) / sizeof(bad_speeds[0]); ++i) {
    set_link(bad_speeds[i], 16, AMDSMI_STATUS_SUCCESS);
    CHECK(!nvtop_amdsmi_current_pcie_link(device, &gen, &lanes));
  }
  const unsigned bad_widths[] = {0, 3, UINT16_MAX};
  for (unsigned i = 0; i < sizeof(bad_widths) / sizeof(bad_widths[0]); ++i) {
    set_link(8000, bad_widths[i], AMDSMI_STATUS_SUCCESS);
    CHECK(!nvtop_amdsmi_current_pcie_link(device, &gen, &lanes));
  }
  set_link(8000, 16, AMDSMI_STATUS_NOT_SUPPORTED);
  CHECK(!nvtop_amdsmi_current_pcie_link(device, &gen, &lanes));
  set_link(5000, 4, AMDSMI_STATUS_SUCCESS);
  CHECK(nvtop_amdsmi_current_pcie_link(device, &gen, &lanes));
  CHECK(gen == 2 && lanes == 4);

  nvtop_amdsmi_shutdown();
  CHECK(!nvtop_amdsmi_current_pcie_link(device, &gen, &lanes));
  nvtop_amdsmi_shutdown();
  CHECK(setenv("NVTOP_TEST_SMI_INIT_FAIL", "1", 1) == 0);
  nvtop_amdsmi_init();
  CHECK(!nvtop_amdsmi_device_from_bdf("0000:03:00.0"));
  CHECK(unsetenv("NVTOP_TEST_SMI_INIT_FAIL") == 0);
  nvtop_amdsmi_init();
  CHECK(nvtop_amdsmi_device_from_bdf("0000:03:00.0"));
  nvtop_amdsmi_shutdown();
  dlclose(lib);
  puts("PASS: BDF matching, live Gen1-6/width updates, invalid data, query failure and recovery");
  return 0;
}
