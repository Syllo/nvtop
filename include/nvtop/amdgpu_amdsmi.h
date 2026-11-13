/* SPDX-License-Identifier: GPL-3.0-or-later */
#ifndef NVTOP_AMDGPU_AMDSMI_H
#define NVTOP_AMDGPU_AMDSMI_H

#include <stdbool.h>

/* Optional AMD SMI support. Handles are matched by PCI address, not GPU index. */
void nvtop_amdsmi_init(void);
void nvtop_amdsmi_shutdown(void);
void *nvtop_amdsmi_device_from_bdf(const char *address);
bool nvtop_amdsmi_current_pcie_link(void *device, unsigned *generation, unsigned *width);

#endif
