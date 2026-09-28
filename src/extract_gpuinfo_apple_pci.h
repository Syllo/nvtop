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

#include <stdbool.h>
#include <stdint.h>

struct gpuinfo_apple_pci;

bool gpuinfo_apple_pci_init(struct gpuinfo_apple_pci **pci);
void gpuinfo_apple_pci_shutdown(struct gpuinfo_apple_pci *pci);

// Look up the PCI bus and slot for the GPU whose IOAccelerator registry entry
// has `registry_id`. Returns false if the bridge or its keys cannot be found
// (common on Apple Silicon); the output parameters are zeroed on failure.
bool gpuinfo_apple_pci_lookup(uint64_t registry_id, unsigned *bus_id, unsigned *slot_id);

#endif // EXTRACT_GPUINFO_APPLE_PCI_H_