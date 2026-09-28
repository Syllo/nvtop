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

// Per-GPU PCI topology + Infinity Fabric metadata. Populated by
// gpuinfo_apple_pci_full() below; *_valid flags indicate whether the field
// is actually present in the IORegistry (false on Apple Silicon iGPUs,
// where the IOAccelerator parent chain has none of these properties).
struct gpuinfo_apple_pci_full {
  // Real PCI BDF address from the parent IOPCIDevice.
  bool bus_valid;
  unsigned bus;
  bool device_valid;
  unsigned device;
  bool function_valid;
  unsigned function;
  // Slot index within the parent bridge (0 for the first slot, etc.).
  bool child_index_valid;
  unsigned child_index;
  // Physical slot identifier, e.g. "EGP0@0", "GFX1@10000", "UPSB@0".
  // Extracted from "attached-gpu-control-path" by stripping the IOService
  // namespace prefix; falls back to the IOACPIPlane "acpi-path" when the
  // service path is unavailable.
  char control_path[128];
  char acpi_path[128];
  // XGMI Infinity Fabric hive for linked cards (Radeon Pro W6800X Duo,
  // W6900X Duo, …). All cards in the same hive report the same XGMI_HiveID.
  char xgmi_hive[24];
  bool xgmi_hive_valid;
  unsigned xgmi_hive_size;
  bool xgmi_hive_size_valid;
  char xgmi_node[24];
  bool xgmi_node_valid;
  unsigned xgmi_node_index;
  bool xgmi_node_index_valid;
  // Apple publishes a stable, chassis-wide "Slot-N" identifier on every
  // IOPCI2PCIBridge as the AAPL,slot-name data property (base64-encoded
  // UTF-8, e.g. U2xvdC0xAA== → "Slot-1"). We walk up from the GPU's PCI
  // device to the nearest bridge carrying that property. This is the
  // same string the "About This Mac → PCI Cards" tab uses, so it matches
  // the chassis labels in the Mac Pro service manual:
  //   Slot-1 / Slot-2 = MPX bay 1 (top), Slot-3 / Slot-4 = MPX bay 2 (bottom),
  //   Slot-5 / Slot-6 / Slot-7 = half-length / single-wide PCIe slots,
  //   Slot-8 = Apple I/O card (Thunderbolt downstream root).
  // For Duo modules both dies share the parent's AAPL,slot-name — they
  // occupy the same physical slot. Use mpx_die_index below to disambiguate
  // them when more than one die appears under the same apple_slot.
  char apple_slot[12];
  bool apple_slot_valid;
  // 0-indexed die position inside the slot, parsed from the "GD<y>" segment
  // of the AMD driver's attached-gpu-control-path / acpi-path (e.g. GD00=0,
  // GD01=1 for a W6800X/W6900X Duo). 0 for single-die cards. Useful to
  // distinguish two GPUs that report the same apple_slot.
  unsigned mpx_die_index;
  bool mpx_die_index_valid;
};

bool gpuinfo_apple_pci_init(struct gpuinfo_apple_pci **pci);
void gpuinfo_apple_pci_shutdown(struct gpuinfo_apple_pci *pci);

// Best-effort bus/slot lookup. Kept for compatibility with the simple call
// site in extract_gpuinfo_apple_populate_static_info; new code should call
// gpuinfo_apple_pci_full().
bool gpuinfo_apple_pci_lookup(uint64_t registry_id, unsigned *bus_id, unsigned *slot_id);

// Rich lookup populating the struct above. Returns true if at least one of
// the *_valid fields was filled in.
bool gpuinfo_apple_pci_full(uint64_t registry_id, struct gpuinfo_apple_pci_full *out);

// Chassis-derived PCIe gen best guess. macOS does not publish the current
// negotiated PCIe gen in the IORegistry; we do know the chassis. Returns
// 3 for AMD dGPUs on a MacPro7,1, 0 ("not reported") otherwise. Callers
// render 0 as "N/A".
unsigned gpuinfo_apple_pci_link_gen_chassis(int is_intel_mac_pro, int is_amd_dgpu);

#endif // EXTRACT_GPUINFO_APPLE_PCI_H_
