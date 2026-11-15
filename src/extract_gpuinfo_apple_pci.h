/*-
 *
 * Copyright (C) 2026 NVTOP contributors
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

#ifndef EXTRACT_GPUINFO_APPLE_PCI_H_
#define EXTRACT_GPUINFO_APPLE_PCI_H_

#include <IOKit/IOKitLib.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// PCI address, negotiated PCIe link status and chassis slot label for a Metal
// GPU, read from the IOPCIDevice that provides the accelerator. Apple Silicon
// accelerators are not PCI devices, so every lookup can fail and leave the
// matching *_valid flag clear; callers must check each flag before using it.
struct gpuinfo_apple_pci_info {
  bool address_valid;
  unsigned bus;
  unsigned device;
  unsigned function;
  bool link_valid;
  unsigned link_gen;
  unsigned link_width;
  bool slot_valid;
  char slot[32];
  bool die_index_valid;
  unsigned die_index;
};

// Query the IORegistry metadata for the IOAccelerator service of a Metal
// device. Returns true when at least one field was filled in.
bool gpuinfo_apple_pci_query(io_service_t gpu_service, struct gpuinfo_apple_pci_info *info);

#ifdef __cplusplus
}
#endif

#endif // EXTRACT_GPUINFO_APPLE_PCI_H_
