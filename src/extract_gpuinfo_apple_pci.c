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
#include <string.h>

// PCI topology + Infinity Fabric metadata for Metal GPUs on macOS.
//
// Apple Silicon's IOAccelerator tree doesn't expose much (no PCI config space,
// no bus address); the AMD driver on MacPro7,1 does. After
// IORegistryEntryIDMatching + IOServiceGetMatchingService lands on the
// IOAccelerator node, the immediate parent in kIOServicePlane is the
// GPU's IOPCIDevice — and that node carries:
//
//   * "pci-bus-number" / "pci-device-number" / "pci-function-number":
//     real BDF address (function is usually 0).
//   * "IOChildIndex": the slot index within the parent bridge.
//   * "attached-gpu-control-path": a stable IOService path that includes the
//     ACPI device name (e.g. .../BR3A@0/IOPP/GU00@0/IOPP/GD01@1/IOPP/EGP0@0/
//     IO). The leaf segment (EGP0@0, GFX0@10000, UPSB@0, …) is the
//     physical slot identifier. On MacPro7,1 it's an MPX bay letter + GPU
//     + slot; on USB4 / Thunderbolt enclosures the leaf is UPSB/DSxx.
//   * "acpi-path": same identifier but in the IOACPIPlane namespace, often
//     slightly cleaner to display.
//   * "XGMI_HiveID" / "XGMI_HiveSize" / "XGMI_NodeIndex": Infinity Fabric
//     topology for cards that are linked on an XGMI hive (Radeon Pro W6800X
//     Duo, W6900X Duo, …). All cards sharing a hive report the same
//     XGMI_HiveID; XGMI_HiveSize is the number of linked devices.
//
// PCIe link generation is not in the registry on macOS. The AMD driver
// publishes PCI config space registers through IODeviceMapper but never
// queries the *current* negotiated gen/width into a CFProperty; the
// underlying chassis (MacPro7,1) hard-wires PCIe 3.0 for every MPX bay
// and the only PCIe 4.0 source is the USB-C / Thunderbolt tunnel.
// gpuinfo_apple_pci_link_gen returns a chassis-derived best guess
// (PCIe 3.0 for AMD dGPUs on MacPro7,1, N/A for Apple Silicon iGPUs).

struct gpuinfo_apple_pci {
  // Reserved for future caching; matches the ioreport/smc pattern.
  int placeholder;
};

bool gpuinfo_apple_pci_init(struct gpuinfo_apple_pci **pci) {
  if (!pci)
    return false;
  *pci = NULL;
  return true;
}

void gpuinfo_apple_pci_shutdown(struct gpuinfo_apple_pci *pci) { (void)pci; }

// Read a CFNumber property at `service` and write it into `*out`.
// Returns true on success. Returns false if the property is missing, not a
// number, or the conversion fails.
static bool read_cf_number(io_service_t service, const char *key, int64_t *out) {
  CFStringRef cf_key = CFStringCreateWithCString(NULL, key, kCFStringEncodingUTF8);
  if (!cf_key)
    return false;
  CFTypeRef ref = IORegistryEntryCreateCFProperty(service, cf_key, kCFAllocatorDefault, 0);
  CFRelease(cf_key);
  if (!ref)
    return false;
  bool ok = false;
  if (CFGetTypeID(ref) == CFNumberGetTypeID()) {
    int64_t value = 0;
    if (CFNumberGetValue((CFNumberRef)ref, kCFNumberSInt64Type, &value)) {
      *out = value;
      ok = true;
    }
  }
  CFRelease(ref);
  return ok;
}

// Read a CFString property at `service`, copy up to dst_size-1 bytes into
// `dst`, NUL-terminate. Returns true on success.
static bool read_cf_string(io_service_t service, const char *key, char *dst, size_t dst_size) {
  if (!dst || dst_size == 0)
    return false;
  dst[0] = '\0';
  CFStringRef cf_key = CFStringCreateWithCString(NULL, key, kCFStringEncodingUTF8);
  if (!cf_key)
    return false;
  CFTypeRef ref = IORegistryEntryCreateCFProperty(service, cf_key, kCFAllocatorDefault, 0);
  CFRelease(cf_key);
  if (!ref)
    return false;
  bool ok = false;
  if (CFGetTypeID(ref) == CFStringGetTypeID()) {
    const char *p = CFStringGetCStringPtr((CFStringRef)ref, kCFStringEncodingUTF8);
    if (p) {
      strncpy(dst, p, dst_size - 1);
      dst[dst_size - 1] = '\0';
      ok = true;
    } else {
      // CFStringGetCStringPtr returns NULL when the string has non-ASCII bytes
      // and the backing store isn't a C string; fall back to the explicit copy.
      if (CFStringGetCString((CFStringRef)ref, dst, dst_size, kCFStringEncodingUTF8)) {
        ok = true;
      }
    }
  }
  CFRelease(ref);
  return ok;
}

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

  // Walk up to the PCI device node — the immediate parent of an IOAccelerator
  // on Intel Mac Pro AMD dGPUs is the IOPCIDevice that owns the BARs.
  io_service_t pci_device = 0;
  const kern_return_t kr = IORegistryEntryGetParentEntry(gpu_service, kIOServicePlane, &pci_device);
  IOObjectRelease(gpu_service);
  if (kr != kIOReturnSuccess || !MACH_PORT_VALID(pci_device))
    return false;

  int64_t bus = 0, dev = 0;
  if (read_cf_number(pci_device, "pci-bus-number", &bus))
    *bus_id = (unsigned)bus;
  if (read_cf_number(pci_device, "IOChildIndex", &dev))
    *slot_id = (unsigned)dev;
  // pcie slot naming on MacPro7,1
  IOObjectRelease(pci_device);

  // Treat (0, 0) as a failure so the caller doesn't accidentally render a
  // bogus "PCI 0:0" bus string on cards where the registry keys are missing.
  return *bus_id != 0 || *slot_id != 0;
}

// Parse the Mac Pro 7,1 physical slot identifier out of the
// attached-gpu-control-path / acpi-path strings. The Mac Pro 7,1 chassis
// exposes:
//
//   * MPX bays 1..4: .../BR<n>A@0/IOPP/GU00@0/IOPP/GD<m>@x/...
//     where n is the bay number (1..4) and m is the die index within
//     the bay (0 or 1 for a Duo).
//   * I/O board / Thunderbolt (slot 5): .../PC04@0/AppleACPIPCI/MCP0@0/
//     IOPP/USnn@x/IOPP/DSmm@y/IOPP/UPSB@z/IO
//     No BR<x>A segment; the user-facing convention is "slot 5" because
//     the I/O board is the fifth physical slot in the chassis layout.
//
// Walk up from `start` until we find an IORegistry entry whose
// AAPL,slot-name data property is set, or until we run out of parents.
// Apple publishes AAPL,slot-name on every IOPCI2PCIBridge in the
// chassis, encoded as a UTF-8 string padded to 4-byte boundary (e.g.
// "Slot-1\xAA" or similar). The closest ancestor that carries the
// property is the GPU's physical slot; that matches what the Mac Pro
// service manual and About This Mac → PCI Cards both display. For a
// W6800X Duo the two dies share the same parent bridge (the BR<x>A
// bridge itself), so they report the same apple_slot — disambiguate
// them via mpx_die_index below.
//
// Returns true and fills `out` on success. Out is null-terminated and
// truncated to fit. We decode the data ourselves because the Apple PCI
// bridge stores slot names as a raw CFData buffer rather than a
// CFString, presumably for EFI/ACPI compatibility.
static bool walk_for_slot_name(io_registry_entry_t start, char *out, size_t out_size) {
  if (!out || out_size == 0) return false;
  out[0] = '\0';
  io_registry_entry_t cur = start;
  bool released_start = false;
  while (cur) {
    CFMutableDictionaryRef props = NULL;
    if (IORegistryEntryCreateCFProperties(cur, &props, NULL, 0) == kIOReturnSuccess && props) {
      CFTypeRef slot = CFDictionaryGetValue(props, CFSTR("AAPL,slot-name"));
      if (slot && CFGetTypeID(slot) == CFDataGetTypeID()) {
        CFDataRef data = (CFDataRef)slot;
        const CFIndex len = CFDataGetLength(data);
        const UInt8 *bytes = CFDataGetBytePtr(data);
        // Copy up to out_size-1 bytes; strip any trailing non-UTF8 pad bytes
        // (the property is null-padded to a 4-byte boundary).
        size_t copy = (size_t)len;
        if (copy >= out_size) copy = out_size - 1;
        // Trim trailing NULs and high-bit pad bytes (>0x7E) that would
        // not appear in a real "Slot-N" string.
        while (copy > 0 && (bytes[copy - 1] == 0 || bytes[copy - 1] > 0x7E))
          copy--;
        memcpy(out, bytes, copy);
        out[copy] = '\0';
        CFRelease(props);
        if (released_start) IOObjectRelease(cur);
        return true;
      }
      CFRelease(props);
    }
    io_registry_entry_t parent = 0;
    if (IORegistryEntryGetParentEntry(cur, kIOServicePlane, &parent) != kIOReturnSuccess) {
      break;
    }
    if (cur != start) IOObjectRelease(cur);
    else released_start = true;
    cur = parent;
  }
  if (released_start && cur) IOObjectRelease(cur);
  return false;
}

// Parse the 0-indexed GD<y> segment out of either path the AMD driver
// publishes (attached-gpu-control-path on IOService, acpi-path on
// IOACPIPlane). The driver publishes segments like .../BR1A@0/.../
// GD01@10000/.../GFX0@0 where GD01 = the second die of an MPX Duo
// (GD00 = first die, GD01 = second). For a single-GPU card there is
// no GD segment and we return 0 (no override).
static bool parse_gd_index(const char *path, unsigned *die_out) {
  if (die_out) *die_out = 0;
  if (!path || !*path) return false;
  const char *gd = strstr(path, "GD");
  if (!gd) return false;
  char *end = NULL;
  const unsigned long d = strtoul(gd + 2, &end, 10);
  if (end == gd + 2) return false;
  if (die_out) *die_out = (unsigned)d;
  return true;
}

bool gpuinfo_apple_pci_full(uint64_t registry_id, struct gpuinfo_apple_pci_full *out) {
  if (!out)
    return false;
  memset(out, 0, sizeof(*out));

  CFMutableDictionaryRef matching = IORegistryEntryIDMatching(registry_id);
  if (!matching)
    return false;
  io_service_t gpu_service = IOServiceGetMatchingService(kIOMainPortDefault, matching);
  if (!MACH_PORT_VALID(gpu_service))
    return false;

  io_service_t pci_device = 0;
  kern_return_t kr = IORegistryEntryGetParentEntry(gpu_service, kIOServicePlane, &pci_device);
  IOObjectRelease(gpu_service);
  if (kr != kIOReturnSuccess || !MACH_PORT_VALID(pci_device))
    return false;

  int64_t v = 0;
  out->bus_valid = read_cf_number(pci_device, "pci-bus-number", &v);
  out->bus = out->bus_valid ? (unsigned)v : 0;
  out->device_valid = read_cf_number(pci_device, "pci-device-number", &v);
  out->device = out->device_valid ? (unsigned)v : 0;
  out->function_valid = read_cf_number(pci_device, "pci-function-number", &v);
  out->function = out->function_valid ? (unsigned)v : 0;
  out->child_index_valid = read_cf_number(pci_device, "IOChildIndex", &v);
  out->child_index = out->child_index_valid ? (unsigned)v : 0;

  // The "attached-gpu-control-path" IOService string has the leaf ACPI
  // device name we want, but it also has the long prefix. Strip everything
  // before and including the last "IOPP/" but keep enough of the suffix
  // that downstream parsers (format_slot) can find BR<x>A / GD<y> / UPSx
  // segments.
  if (read_cf_string(pci_device, "attached-gpu-control-path", out->control_path,
                     sizeof(out->control_path))) {
    const char *needle = "IOPP/";
    const char *last = NULL;
    for (const char *p = out->control_path; (p = strstr(p, needle)) != NULL; p += 5)
      last = p;
    if (last) {
      // Drop the "IOPP/" prefix; keep the rest of the string (we strip the
      // trailing "/IO" but only when it's the very last segment, otherwise
      // we'd lose intermediate path components the slot parser needs).
      const size_t prefix_len = (size_t)(last - out->control_path) + 5;
      const size_t total_len = strlen(out->control_path);
      const size_t suffix_len = total_len - prefix_len;
      if (suffix_len >= 4 &&
          strcmp(out->control_path + total_len - 3, "/IO") == 0) {
        out->control_path[total_len - 3] = '\0';
      }
      memmove(out->control_path, out->control_path + prefix_len,
               strlen(out->control_path + prefix_len) + 1);
      (void)suffix_len;
    }
  }
  if (!read_cf_string(pci_device, "acpi-path", out->acpi_path, sizeof(out->acpi_path))) {
    out->acpi_path[0] = '\0';
  }

  // Walk up from the GPU's PCI device to the closest bridge carrying
  // AAPL,slot-name. Apple publishes the slot label (Slot-1, Slot-3, …)
  // there. For a W6800X/W6900X Duo both dies share the parent's slot
  // label — disambiguate via mpx_die_index from the GD<y> segment.
  if (walk_for_slot_name(pci_device, out->apple_slot, sizeof(out->apple_slot))) {
    out->apple_slot_valid = true;
  }

  unsigned gd_die = 0;
  if (parse_gd_index(out->control_path, &gd_die) ||
      parse_gd_index(out->acpi_path, &gd_die)) {
    out->mpx_die_index = gd_die;
    out->mpx_die_index_valid = true;
  }

  IOObjectRelease(pci_device);

  // XGMI hive metadata lives on the IOAccelerator itself, not the PCI device.
  matching = IORegistryEntryIDMatching(registry_id);
  if (!matching)
    return out->bus_valid || out->child_index_valid; // partial success is OK
  gpu_service = IOServiceGetMatchingService(kIOMainPortDefault, matching);
  if (!MACH_PORT_VALID(gpu_service))
    return out->bus_valid || out->child_index_valid;

  io_service_t accel = gpu_service;
  uint8_t hive_buf[8];
  CFTypeRef ref = IORegistryEntryCreateCFProperty(accel, CFSTR("XGMI_HiveID"),
                                                  kCFAllocatorDefault, 0);
  if (ref && CFGetTypeID(ref) == CFDataGetTypeID() && CFDataGetLength((CFDataRef)ref) == 8) {
    CFDataGetBytes((CFDataRef)ref, CFRangeMake(0, 8), hive_buf);
    snprintf(out->xgmi_hive, sizeof(out->xgmi_hive),
             "%02x%02x%02x%02x-%02x%02x-%02x%02x",
             hive_buf[0], hive_buf[1], hive_buf[2], hive_buf[3],
             hive_buf[4], hive_buf[5], hive_buf[6], hive_buf[7]);
    out->xgmi_hive_valid = true;
  }
  if (ref) CFRelease(ref);

  ref = IORegistryEntryCreateCFProperty(accel, CFSTR("XGMI_NodeID"),
                                        kCFAllocatorDefault, 0);
  if (ref && CFGetTypeID(ref) == CFDataGetTypeID() && CFDataGetLength((CFDataRef)ref) == 8) {
    CFDataGetBytes((CFDataRef)ref, CFRangeMake(0, 8), hive_buf);
    snprintf(out->xgmi_node, sizeof(out->xgmi_node),
             "%02x%02x%02x%02x-%02x%02x-%02x%02x",
             hive_buf[0], hive_buf[1], hive_buf[2], hive_buf[3],
             hive_buf[4], hive_buf[5], hive_buf[6], hive_buf[7]);
    out->xgmi_node_valid = true;
  }
  if (ref) CFRelease(ref);

  out->xgmi_hive_size_valid = read_cf_number(accel, "XGMI_HiveSize", &v);
  out->xgmi_hive_size = out->xgmi_hive_size_valid ? (unsigned)v : 0;
  out->xgmi_node_index_valid = read_cf_number(accel, "XGMI_NodeIndex", &v);
  out->xgmi_node_index = out->xgmi_node_index_valid ? (unsigned)v : 0;

  IOObjectRelease(accel);

  return out->bus_valid || out->child_index_valid || out->xgmi_hive_valid;
}

unsigned gpuinfo_apple_pci_link_gen_chassis(int is_intel_mac_pro, int is_amd_dgpu) {
  // macOS does not expose the current negotiated PCIe gen. We do know the
  // chassis: a MacPro7,1 has PCIe 3.0 in every MPX bay and the AMD dGPUs
  // (W6800X/W6900X/…) are themselves PCIe 3.0 cards. Apple Silicon iGPUs
  // have no PCIe slot; eGPUs over Thunderbolt may negotiate gen 3 or gen 4
  // but we cannot tell which, so we return 0 ("not reported").
  if (is_amd_dgpu && is_intel_mac_pro)
    return 3;
  return 0;
}
