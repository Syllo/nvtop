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

#include "extract_gpuinfo_apple_utils.h"

#include <CoreFoundation/CoreFoundation.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

// PCI metadata for Metal GPUs on macOS.
//
// The IOAccelerator matched through IORegistryEntryIDMatching([MTLDevice
// registryID]) has the GPU's IOPCIDevice as its parent in the service plane.
// That node publishes the properties used here:
//
//   * "pcidebug": the BDF address as a decimal "bus:device:function" string.
//   * "IOPCIExpressLinkStatus": the raw PCIe Link Status register of the
//     negotiated link (decoded by gpuinfo_apple_decode_pcie_link_status).
//   * "AAPL,slot-name": Apple's chassis slot label, on the GPU's IOPCIDevice
//     or on the closest parent bridge that publishes it ("Slot-1" on a
//     MacPro7,1, "PCI Slot 2" on earlier Macs).
//   * "acpi-path" / "attached-gpu-control-path": the ACPI and IOService paths.
//     Either may carry the GD<nn> segment that identifies the die of a
//     multi-die card (W6800X/W6900X Duo).
//
// Apple Silicon accelerators are not PCI devices, so the parent lookup finds
// none of these and the caller reports N/A.

static bool read_number(io_registry_entry_t entry, CFStringRef key, int64_t *out) {
  CFTypeRef value = IORegistryEntryCreateCFProperty(entry, key, kCFAllocatorDefault, 0);
  if (!value)
    return false;

  bool ok = false;
  if (CFGetTypeID(value) == CFNumberGetTypeID()) {
    int64_t number = 0;
    if (CFNumberGetValue((CFNumberRef)value, kCFNumberSInt64Type, &number)) {
      *out = number;
      ok = true;
    }
  }
  CFRelease(value);
  return ok;
}

static bool read_string(io_registry_entry_t entry, CFStringRef key, char *out, size_t out_size) {
  if (!out || out_size == 0)
    return false;
  out[0] = '\0';

  CFTypeRef value = IORegistryEntryCreateCFProperty(entry, key, kCFAllocatorDefault, 0);
  if (!value)
    return false;

  bool ok = false;
  if (CFGetTypeID(value) == CFStringGetTypeID()) {
    const char *cstr = CFStringGetCStringPtr((CFStringRef)value, kCFStringEncodingUTF8);
    if (cstr) {
      const size_t length = strlen(cstr);
      if (length < out_size) {
        memcpy(out, cstr, length + 1);
        ok = true;
      }
    } else if (CFStringGetCString((CFStringRef)value, out, (CFIndex)out_size, kCFStringEncodingUTF8)) {
      ok = true;
    }
  }
  CFRelease(value);
  return ok;
}

static bool read_slot_property(io_registry_entry_t entry, char *slot, size_t slot_size) {
  CFTypeRef value = IORegistryEntryCreateCFProperty(entry, CFSTR("AAPL,slot-name"), kCFAllocatorDefault, 0);
  if (!value)
    return false;

  bool decoded = false;
  if (CFGetTypeID(value) == CFDataGetTypeID()) {
    CFDataRef data = (CFDataRef)value;
    decoded = gpuinfo_apple_decode_slot_name(CFDataGetBytePtr(data), (size_t)CFDataGetLength(data), slot, slot_size);
  } else if (CFGetTypeID(value) == CFStringGetTypeID()) {
    decoded = CFStringGetCString((CFStringRef)value, slot, (CFIndex)slot_size, kCFStringEncodingUTF8);
  }
  CFRelease(value);
  return decoded;
}

// Walk from the GPU's IOPCIDevice towards the root and take the closest
// AAPL,slot-name label. The property is normally on the GPU device itself; the
// walk is a fallback for nodes where it is published on a parent bridge.
static bool find_slot_name(io_registry_entry_t start, char *slot, size_t slot_size) {
  io_registry_entry_t current = start;
  IOObjectRetain(current);
  for (;;) {
    if (read_slot_property(current, slot, slot_size)) {
      IOObjectRelease(current);
      return true;
    }

    io_registry_entry_t parent = 0;
    if (IORegistryEntryGetParentEntry(current, kIOServicePlane, &parent) != kIOReturnSuccess) {
      IOObjectRelease(current);
      return false;
    }
    IOObjectRelease(current);
    current = parent;
  }
}

bool gpuinfo_apple_pci_query(io_service_t gpu_service, struct gpuinfo_apple_pci_info *info) {
  if (!info)
    return false;
  memset(info, 0, sizeof(*info));
  if (!MACH_PORT_VALID(gpu_service))
    return false;

  io_service_t pci_device = 0;
  if (IORegistryEntryGetParentEntry(gpu_service, kIOServicePlane, &pci_device) != kIOReturnSuccess ||
      !MACH_PORT_VALID(pci_device))
    return false;
  // Apple Silicon's accelerator parent is not a PCI device. Check the class
  // explicitly so a device-tree property can never be mistaken for PCI
  // metadata.
  if (!IOObjectConformsTo(pci_device, "IOPCIDevice")) {
    IOObjectRelease(pci_device);
    return false;
  }

  char pcidebug[64];
  if (read_string(pci_device, CFSTR("pcidebug"), pcidebug, sizeof(pcidebug))) {
    unsigned bus, device, function;
    if (gpuinfo_apple_parse_pci_bdf(pcidebug, &bus, &device, &function)) {
      info->bus = bus;
      info->device = device;
      info->function = function;
      info->address_valid = true;
    }
  }

  int64_t link_status;
  if (read_number(pci_device, CFSTR("IOPCIExpressLinkStatus"), &link_status)) {
    info->link_valid = gpuinfo_apple_decode_pcie_link_status(link_status, &info->link_gen, &info->link_width);
  }

  info->slot_valid = find_slot_name(pci_device, info->slot, sizeof(info->slot));

  // The GD<nn> die segment may live in either path, so consult both rather
  // than treating the control path as a fallback for a missing acpi-path:
  // every Intel Mac publishes acpi-path, but it does not always carry GD<nn>.
  char path[256];
  if (read_string(pci_device, CFSTR("acpi-path"), path, sizeof(path)))
    info->die_index_valid = gpuinfo_apple_parse_die_index(path, &info->die_index);
  if (!info->die_index_valid && read_string(pci_device, CFSTR("attached-gpu-control-path"), path, sizeof(path)))
    info->die_index_valid = gpuinfo_apple_parse_die_index(path, &info->die_index);

  IOObjectRelease(pci_device);

  return info->address_valid || info->link_valid || info->slot_valid || info->die_index_valid;
}
