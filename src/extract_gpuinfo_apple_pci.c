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

#include "extract_gpuinfo_apple_pci.h"

#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <stdbool.h>
#include <stdint.h>

// Best-effort PCI bus/slot lookup for a Metal GPU on macOS. macOS exposes no
// public user-space PCI config API, so we walk the IORegistry tree from the
// GPU's IOAccelerator node up to its parent IOPCIBridge and read "bus-id" /
// "IOChildIndex" — the same scheme Stats uses in its GPU reader.
//
// PCIe link generation and width are not in the registry tree; PCIe lane
// negotiation is owned by the driver and we have no visibility into it, so
// gpuinfo_pcie_link_gen / gpuinfo_pcie_link_width stay invalid here.

struct gpuinfo_apple_pci {
  // Reserved for future expansion (cached registry handle, etc.). Kept as a
  // struct so the Apple backend matches the pattern set by PR #496's ioreport
  // and smc subsystems, making it cheap to add new helpers later.
  int placeholder;
};

bool gpuinfo_apple_pci_init(struct gpuinfo_apple_pci **pci) {
  if (!pci)
    return false;
  *pci = NULL;
  return true;
}

void gpuinfo_apple_pci_shutdown(struct gpuinfo_apple_pci *pci) { (void)pci; }

bool gpuinfo_apple_pci_lookup(uint64_t registry_id, unsigned *bus_id, unsigned *slot_id) {
  if (!bus_id || !slot_id)
    return false;
  *bus_id = 0;
  *slot_id = 0;

  // Locate the IOAccelerator entry the same way the rest of the Apple backend does.
  CFMutableDictionaryRef matching = IORegistryEntryIDMatching(registry_id);
  if (!matching)
    return false;
  io_service_t gpu_service = IOServiceGetMatchingService(kIOMainPortDefault, matching);
  if (!MACH_PORT_VALID(gpu_service)) {
    // IOServiceGetMatchingService consumes the dictionary on both success and
    // "not found" paths, so nothing else to release here.
    return false;
  }

  // Walk up to the PCI bridge — the immediate parent of an IOAccelerator on a
  // Mac is IOPCIBridge. If for any reason the chain is different (Apple
  // Silicon PCIe root, an M1 USB4 tunnel, …) we just bail without error.
  io_service_t bridge = 0;
  const kern_return_t kr = IORegistryEntryGetParentEntry(gpu_service, kIOServicePlane, &bridge);
  IOObjectRelease(gpu_service);
  if (kr != kIOReturnSuccess || !MACH_PORT_VALID(bridge))
    return false;

  // "bus-id" is a CFNumber carrying the bus on which the bridge lives; the
  // device number within that bus lives in "IOChildIndex" (a CFNumber). Both
  // are stable across reboots for a given PCIe slot.
  CFTypeRef bus_ref = IORegistryEntryCreateCFProperty(bridge, CFSTR("bus-id"), kCFAllocatorDefault, 0);
  if (bus_ref) {
    if (CFGetTypeID(bus_ref) == CFNumberGetTypeID()) {
      uint8_t b = 0;
      if (CFNumberGetValue((CFNumberRef)bus_ref, kCFNumberSInt8Type, &b))
        *bus_id = (unsigned)b;
    }
    CFRelease(bus_ref);
  }

  CFTypeRef slot_ref = IORegistryEntryCreateCFProperty(bridge, CFSTR("IOChildIndex"), kCFAllocatorDefault, 0);
  if (slot_ref) {
    if (CFGetTypeID(slot_ref) == CFNumberGetTypeID()) {
      uint64_t s = 0;
      if (CFNumberGetValue((CFNumberRef)slot_ref, kCFNumberSInt64Type, &s))
        *slot_id = (unsigned)s;
    }
    CFRelease(slot_ref);
  }

  IOObjectRelease(bridge);

  // Treat (0, 0) as a failure so the caller doesn't accidentally render a
  // bogus "PCI 0:0" bus string on cards where the registry keys are missing.
  return *bus_id != 0 || *slot_id != 0;
}