/*
 * Copyright (C) 2023 Robin Voetter <robin@voetter.nl>
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

#include "nvtop/device_discovery.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/time.h"
#include "extract_gpuinfo_apple_ioreport.h"
#include "extract_gpuinfo_apple_pci.h"
#include "extract_gpuinfo_apple_smc.h"
#include "extract_gpuinfo_apple_utils.h"
#include "uthash.h"

#include <Metal/Metal.h>
#include <IOKit/IOKitLib.h>
#include <QuartzCore/QuartzCore.h>
#include <mach/mach.h>
#include <stdlib.h>
#include <string.h>

#define HASH_FIND_CLIENT(head, key_ptr, out_ptr)                                                                    \
  HASH_FIND(hh, head, key_ptr, sizeof(struct apple_process_cache_id), out_ptr)
#define HASH_ADD_CLIENT(head, in_ptr)                                                                               \
  HASH_ADD(hh, head, client_id, sizeof(struct apple_process_cache_id), in_ptr)

#define SET_APPLE_CACHE(cachePtr, field, value) SET_VALUE(cachePtr, field, value, apple_cache_)
#define APPLE_CACHE_FIELD_VALID(cachePtr, field) VALUE_IS_VALID(cachePtr, field, apple_cache_)

enum apple_process_info_cache_valid {
  apple_cache_gpu_time_valid = 0,
  apple_cache_process_info_cache_valid_count
};

struct __attribute__((__packed__)) apple_process_cache_id {
  uint64_t registry_entry_id;
  pid_t pid;
};

struct apple_process_info_cache {
  struct apple_process_cache_id client_id;
  uint64_t gpu_time;
  nvtop_time last_measurement_tstamp;
  unsigned char valid[(apple_cache_process_info_cache_valid_count + CHAR_BIT - 1) / CHAR_BIT];
  UT_hash_handle hh;
};

struct gpu_info_apple {
  struct gpu_info base;
  id<MTLDevice> device;
  io_service_t gpu_service;
  struct gpuinfo_apple_ioreport *ioreport;
  struct gpuinfo_apple_pci *pci;
  struct gpuinfo_apple_smc *smc;
  struct apple_process_info_cache *last_update_process_cache, *current_update_process_cache;
};

static bool gpuinfo_apple_init(void);
static void gpuinfo_apple_shutdown(void);
static const char *gpuinfo_apple_last_error_string(void);
static bool gpuinfo_apple_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_apple_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_apple_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_apple_get_running_processes(struct gpu_info *_gpu_info);

static struct gpu_vendor gpu_vendor_apple = {
  .init = gpuinfo_apple_init,
  .shutdown = gpuinfo_apple_shutdown,
  .last_error_string = gpuinfo_apple_last_error_string,
  .get_device_handles = gpuinfo_apple_get_device_handles,
  .populate_static_info = gpuinfo_apple_populate_static_info,
  .refresh_dynamic_info = gpuinfo_apple_refresh_dynamic_info,
  .refresh_running_processes = gpuinfo_apple_get_running_processes,
  .name = "apple",
};

static unsigned apple_gpu_count;
static struct gpu_info_apple *gpu_infos;

__attribute__((constructor)) static void init_extract_gpuinfo_apple(void) { register_gpu_vendor(&gpu_vendor_apple); }

static bool gpuinfo_apple_init(void) {
  apple_gpu_count = 0;
  gpu_infos = NULL;
  return true;
}

static void gpuinfo_apple_free_process_cache(struct apple_process_info_cache **process_cache) {
  struct apple_process_info_cache *cache_entry, *tmp;
  HASH_ITER(hh, *process_cache, cache_entry, tmp) {
    HASH_DEL(*process_cache, cache_entry);
    free(cache_entry);
  }
  *process_cache = NULL;
}

static void gpuinfo_apple_swap_process_cache_for_next_update(struct gpu_info_apple *gpu_info) {
  gpuinfo_apple_free_process_cache(&gpu_info->last_update_process_cache);
  gpu_info->last_update_process_cache = gpu_info->current_update_process_cache;
  gpu_info->current_update_process_cache = NULL;
}

static void gpuinfo_apple_shutdown(void) {
  for (unsigned i = 0; i < apple_gpu_count; ++i) {
    struct gpu_info_apple *gpu_info = &gpu_infos[i];
    gpuinfo_apple_free_process_cache(&gpu_info->last_update_process_cache);
    gpuinfo_apple_free_process_cache(&gpu_info->current_update_process_cache);
    gpuinfo_apple_ioreport_shutdown(gpu_info->ioreport);
    gpuinfo_apple_pci_shutdown(gpu_info->pci);
    gpuinfo_apple_smc_shutdown(gpu_info->smc);
    [gpu_info->device release];
    IOObjectRelease(gpu_info->gpu_service);
  }

  free(gpu_infos);
  gpu_infos = NULL;
  apple_gpu_count = 0;
}

static const char *gpuinfo_apple_last_error_string(void) {
  return "An unanticipated error occurred while accessing Apple "
         "information\n";
}

static bool gpuinfo_apple_get_device_handles(struct list_head *devices, unsigned *count) {
  *count = 0;
  NSArray<id<MTLDevice>> *mtl_devices = MTLCopyAllDevices();
  if (!mtl_devices)
    return false;

  const unsigned mtl_count = [mtl_devices count];
  if (mtl_count) {
    gpu_infos = calloc(mtl_count, sizeof(*gpu_infos));
    if (!gpu_infos) {
      [mtl_devices release];
      return false;
    }
  }

  for (unsigned int i = 0; i < mtl_count; ++i) {
    id<MTLDevice> dev = mtl_devices[i];
    const uint64_t registry_id = [dev registryID];
    CFMutableDictionaryRef matching_service = IORegistryEntryIDMatching(registry_id);
    if (!matching_service)
      continue;

    const io_service_t gpu_service = IOServiceGetMatchingService(kIOMainPortDefault, matching_service);
    if (!MACH_PORT_VALID(gpu_service))
      continue;

    struct gpu_info_apple *gpu_info = &gpu_infos[apple_gpu_count];
    gpu_info->base.vendor = &gpu_vendor_apple;
    gpu_info->device = [dev retain];
    gpu_info->gpu_service = gpu_service;
    // PCI bus/slot lookup is unconditional: every Mac Pro dGPU lives behind a
    // walkable IOPCIBridge, while Apple Silicon simply leaves the lookup empty.
    gpuinfo_apple_pci_init(&gpu_info->pci);
    if ([dev hasUnifiedMemory] && [dev location] == MTLDeviceLocationBuiltIn) {
      gpuinfo_apple_ioreport_init(&gpu_info->ioreport);
      gpuinfo_apple_smc_init(&gpu_info->smc);
    }
    list_add_tail(&gpu_info->base.list, devices);
    ++apple_gpu_count;
  }

  *count = apple_gpu_count;

  [mtl_devices release];
  return true;
}

static void gpuinfo_apple_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_apple *gpu_info = container_of(_gpu_info, struct gpu_info_apple, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
  RESET_ALL(static_info->valid);

  const char *name = [[gpu_info->device name] UTF8String];
  if (name) {
    strncpy(static_info->device_name, name, sizeof(static_info->device_name) - 1);
    static_info->device_name[sizeof(static_info->device_name) - 1] = '\0';
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
  }

  static_info->integrated_graphics = [gpu_info->device location] == MTLDeviceLocationBuiltIn;
  static_info->encode_decode_shared = true;

  // Infinity Fabric connectivity topology. macOS does not expose a single
  // "topology" property; we compute the *machine-wide* shape from the
  // per-GPU peerGroupID on every MTLDevice and bucket by hive. The shape
  // on a MacPro7,1 with two MPX Duo bays linked across is one of:
  //
  //   * "single N-way bridge" — every dGPU shares a single XGMI hive
  //     (the chassis inter-bay Infinity Fabric Link is wired). Most
  //     common with two W6800X Duos installed.
  //   * "dual M-way bridges"  — two distinct XGMI hives, each of size M.
  //     (Dual Duos with the inter-bay link disabled.)
  //   * "M independent GPUs" — every dGPU has its own hive (size 1) or
  //     every hive has size 1. (W6900X, single W6800X, RX 6900 XT, …)
  //   * "no XGMI"             — no peerGroupID anywhere (Apple Silicon or
  //     a chassis with no AMD dGPUs).
  //
  // The label describes the *machine*, not the per-GPU state. We only
  // stamp it onto a GPU when that GPU actually participates in a fabric
  // link (peerGroupID != 0); an unlinked card (RX 6900 XT over
  // Thunderbolt, Apple Silicon iGPU, …) leaves device_architecture
  // empty so the TUI does not claim the eGPU is part of the bridge.
  const uint64_t my_hive = (uint64_t)[gpu_info->device peerGroupID];
  if (my_hive != 0) {
    static char topology[64] = "";
    static dispatch_once_t once = 0;
    dispatch_once(&once, ^{
      NSArray<id<MTLDevice>> *all = MTLCopyAllDevices();
      NSMutableDictionary<NSNumber *, NSNumber *> *hive_size = [NSMutableDictionary dictionary];
      for (id<MTLDevice> d in all) {
        uint64_t hive = (uint64_t)[d peerGroupID];
        if (hive == 0) continue;
        NSNumber *key = @(hive);
        NSNumber *cur = hive_size[key];
        hive_size[key] = @([cur unsignedIntValue] + 1);
      }
      NSUInteger distinct_hives = hive_size.count;
      if (distinct_hives == 0) {
        snprintf(topology, sizeof(topology), "no XGMI");
      } else if (distinct_hives == 1) {
        const NSUInteger n = [hive_size.allValues.firstObject unsignedIntValue];
        if (n <= 1) snprintf(topology, sizeof(topology), "1 independent GPU");
        else snprintf(topology, sizeof(topology), "single %lu-way bridge", (unsigned long)n);
      } else {
        // Multi-hive: sort sizes descending, render as "dual M+N" or
        // "M+N+..." for three+ hives. When the two largest hives are
        // equal in size, prefer "dual M-way bridges" for readability.
        NSArray<NSNumber *> *sizes = [hive_size.allValues
            sortedArrayUsingComparator:^NSComparisonResult(NSNumber *a, NSNumber *b) {
              return [b compare:a];
            }];
        if (sizes.count == 2 && [sizes[0] isEqualToNumber:sizes[1]]) {
          snprintf(topology, sizeof(topology), "dual %i-way bridges",
                   [sizes[0] intValue]);
        } else {
          size_t off = 0;
          bool first = true;
          for (NSNumber *s in sizes) {
            const int n = [s intValue];
            const int written = snprintf(topology + off, sizeof(topology) - off,
                                         "%s%i-way", first ? "" : "+", n);
            if (written < 0 || (size_t)written >= sizeof(topology) - off) break;
            off += (size_t)written;
            first = false;
          }
        }
      }
    });
    strncpy(static_info->device_architecture, topology,
            sizeof(static_info->device_architecture) - 1);
    static_info->device_architecture[sizeof(static_info->device_architecture) - 1] = '\0';
    SET_VALID(gpuinfo_device_architecture_valid, static_info->valid);
  }

  // Metal exposes the Infinity Fabric peer group on every MTLDevice; pair
  // members see the same id and a count that matches how many GPUs share it.
  // Cards with no fabric link return peerGroupID == 0 / peerCount == 1, which
  // is the right thing to surface as "no link".
  static_info->peer_group_id = (uint64_t)[gpu_info->device peerGroupID];
  static_info->peer_count = (unsigned)[gpu_info->device peerCount];
  if (static_info->peer_group_id != 0) {
    SET_VALID(gpuinfo_peer_group_id_valid, static_info->valid);
    SET_VALID(gpuinfo_peer_count_valid, static_info->valid);
  }

  struct gpuinfo_apple_pci_full pci;
  if (gpuinfo_apple_pci_full((uint64_t)[gpu_info->device registryID], &pci)) {
    if (pci.bus_valid) {
      static_info->pci_bus_id = pci.bus;
      SET_VALID(gpuinfo_pci_bus_id_valid, static_info->valid);
    }
    if (pci.device_valid) {
      static_info->pci_slot_id = pci.device;
      SET_VALID(gpuinfo_pci_slot_id_valid, static_info->valid);
    }
    if (pci.apple_slot_valid) {
      strncpy(static_info->apple_slot, pci.apple_slot,
              sizeof(static_info->apple_slot) - 1);
      static_info->apple_slot[sizeof(static_info->apple_slot) - 1] = '\0';
      SET_VALID(gpuinfo_apple_slot_valid, static_info->valid);
    }
    if (pci.mpx_die_index_valid) {
      static_info->mpx_die_index = pci.mpx_die_index;
      SET_VALID(gpuinfo_mpx_die_index_valid, static_info->valid);
    }
  }

  // PCIe gen/width are filled in refresh_dynamic_info below: macOS doesn't
  // expose the current negotiated gen, so we publish the chassis-fixed value
  // (PCIe 3.0 x16 for AMD dGPUs on a MacPro7,1) as a best guess. The
  // *_valid bits stay set so the TUI renders "3@16" instead of N/A.
}

static void gpuinfo_apple_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_apple *gpu_info = container_of(_gpu_info, struct gpu_info_apple, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  RESET_ALL(dynamic_info->valid);

  // macOS does not publish the current negotiated PCIe gen/width in the
  // IORegistry, only the chassis-fixed capability. AMD dGPUs on a MacPro7,1
  // are PCIe 3.0 x16; Apple Silicon iGPUs have no PCIe slot at all; an eGPU
  // over Thunderbolt may negotiate either gen 3 or gen 4 and we cannot tell
  // which. Render the gen/width we know about and let the TUI show N/A for
  // anything we can't determine.
  const int is_amd = ([gpu_info->device name].length > 0 &&
                       [[gpu_info->device name] rangeOfString:@"AMD"].location != NSNotFound);
  // All MacPro7,1 chassis variants are Intel-based.
  const int is_intel_macpro = true;
  const unsigned link_gen = gpuinfo_apple_pci_link_gen_chassis(is_intel_macpro, is_amd);
  if (link_gen != 0 && is_amd) {
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_gen, link_gen);
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_width, 16);
  }

  unsigned power_draw;
  if (gpuinfo_apple_ioreport_get_power_draw(gpu_info->ioreport, &power_draw))
    SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, power_draw);

  unsigned clock_speed, max_clock_speed;
  if (gpuinfo_apple_ioreport_get_gpu_clock_speed(gpu_info->ioreport, &clock_speed, &max_clock_speed)) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, clock_speed);
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, max_clock_speed);
  }

  unsigned temperature;
  if (gpuinfo_apple_smc_get_gpu_temperature(gpu_info->smc, &temperature))
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, temperature);

  unsigned fan_rpm;
  if (gpuinfo_apple_smc_get_fan_rpm(gpu_info->smc, &fan_rpm))
    SET_GPUINFO_DYNAMIC(dynamic_info, fan_rpm, fan_rpm);

  CFMutableDictionaryRef cf_props;
  if (IORegistryEntryCreateCFProperties(gpu_info->gpu_service, &cf_props, kCFAllocatorDefault, kNilOptions) != kIOReturnSuccess) {
    return;
  }
  struct gpuinfo_apple_performance_sample sample;
  const bool sample_valid = gpuinfo_apple_parse_performance_sample(cf_props, &sample);
  CFRelease(cf_props);
  if (!sample_valid)
    return;

  if (sample.gpu_util_rate_valid || sample.gpu_activity_rate_valid) {
    // Apple Silicon publishes "Device Utilization %"; discrete AMD cards pin
    // that key at 0 and report real load under "GPU Activity(%)". Either side
    // may be missing, so pick the larger valid reading and clamp to 100.
    unsigned rate = 0;
    if (sample.gpu_util_rate_valid && sample.gpu_util_rate > rate)
      rate = sample.gpu_util_rate;
    if (sample.gpu_activity_rate_valid && sample.gpu_activity_rate > rate)
      rate = sample.gpu_activity_rate;
    if (rate > 100)
      rate = 100;
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, rate);
  }

  // Discrete AMD readings. These are populated only when the corresponding
  // PerformanceStatistics key exists, so missing values simply leave the
  // *_valid bits unset in the dynamic_info and nvtop renders them as N/A.
  if (sample.used_memory_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, sample.used_memory);
  if (sample.gpu_temp_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, sample.gpu_temp);
  if (sample.power_draw_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, sample.power_draw_mw);
  if (sample.gpu_clock_speed_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, sample.gpu_clock_speed);
  if (sample.mem_clock_speed_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_clock_speed, sample.mem_clock_speed);
  if (sample.fan_speed_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, fan_speed, sample.fan_speed);

  if ([gpu_info->device hasUnifiedMemory]) {
    // [gpu_info->device currentAllocatedSize] returns the amount of memory allocated by this process, not
    // as allocated on the GPU globally. The performance statistics dictionary has the real value that we
    // are interested in, the amount of system memory allocated by the GPU.
    if (sample.allocated_system_memory_valid && !sample.used_memory_valid)
      SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, sample.allocated_system_memory);

    // Unified-memory GPUs share the system's physical memory with the CPU. The Metal
    // recommendedMaxWorkingSetSize is a performance budget, not the memory capacity.
    mach_msg_type_number_t host_size = HOST_BASIC_INFO_COUNT;
    host_basic_info_data_t info;
    const mach_port_t host = mach_host_self();
    const kern_return_t host_info_status = host_info(host, HOST_BASIC_INFO, (host_info_t)&info, &host_size);
    mach_port_deallocate(mach_task_self(), host);
    if (host_info_status == KERN_SUCCESS)
      SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, info.max_mem);
  } else {
    // Discrete GPU (e.g. Radeon Pro W6800X/W6900X, RX 6900 XT). Total comes
    // from the recommended working set size, as MoltenVK does. Used VRAM is
    // taken from the AMD PerformanceStatistics "inUseVidMemoryBytes" counter,
    // which `gpuinfo_apple_parse_performance_sample` already populated above.
    const uint64_t mem_total = [gpu_info->device recommendedMaxWorkingSetSize];
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mem_total);
  }

  if (GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, used_memory) &&
      GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, total_memory) && dynamic_info->total_memory &&
      dynamic_info->used_memory <= dynamic_info->total_memory) {
    SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, dynamic_info->total_memory - dynamic_info->used_memory);
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate,
                        (dynamic_info->total_memory - dynamic_info->free_memory) * 100 / dynamic_info->total_memory);

  }
}

static void gpuinfo_apple_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_apple *gpu_info = container_of(_gpu_info, struct gpu_info_apple, base);
  _gpu_info->processes_count = 0;
  gpuinfo_apple_swap_process_cache_for_next_update(gpu_info);

  // We can find out which processes are running on a particular GPU using the IO Registry. The
  // IOService associated to the MTLDevice has "AGXDeviceUserClient" child nodes, which hold some
  // basic information about processes that are running on the GPU.

  io_iterator_t iterator;
  if (IORegistryEntryGetChildIterator(gpu_info->gpu_service, kIOServicePlane, &iterator) != kIOReturnSuccess) {
    return;
  }

  nvtop_time current_time;
  nvtop_get_current_time(&current_time);
  for (io_object_t child = IOIteratorNext(iterator); child; child = IOIteratorNext(iterator)) {
    io_name_t class_name;
    if (IOObjectGetClass(child, class_name) != kIOReturnSuccess)
      continue;
    // Apple Silicon (AGX) and Intel Mac Pro AMD dGPUs (AMDRadeonX6000)
    // both expose user-client children carrying IOUserClientCreator. The
    // creator string has the same "pid <pid>, <name>" format on both;
    // gpuinfo_apple_parse_process_sample picks the PID out of it.
    const bool is_agx_client = strncmp(class_name, "AGXDeviceUserClient", sizeof(class_name)) == 0;
    const bool is_amd_client =
        strncmp(class_name, "AMDRadeonX6000_AMDAccelDevice", sizeof(class_name)) == 0 ||
        strncmp(class_name, "AMDRadeonX6000_AMDAccelSharedUserClient", sizeof(class_name)) == 0;
    if (!is_agx_client && !is_amd_client)
      continue;

    CFMutableDictionaryRef cf_props;
    if (IORegistryEntryCreateCFProperties(child, &cf_props, kCFAllocatorDefault, kNilOptions) == kIOReturnSuccess) {
      struct gpuinfo_apple_process_sample sample = {0};
      if (gpuinfo_apple_parse_process_sample(cf_props, &sample)) {
          bool gpu_usage_valid = false;
          unsigned gpu_usage = 0;
          uint64_t registry_entry_id = 0;
          if (IORegistryEntryGetRegistryEntryID(child, &registry_entry_id) == kIOReturnSuccess) {
            const struct apple_process_cache_id client_id = {.registry_entry_id = registry_entry_id,
                                                              .pid = sample.pid};
            struct apple_process_info_cache *cache_entry;
            HASH_FIND_CLIENT(gpu_info->last_update_process_cache, &client_id, cache_entry);
            if (cache_entry) {
              HASH_DEL(gpu_info->last_update_process_cache, cache_entry);
              if (sample.gpu_time_valid && APPLE_CACHE_FIELD_VALID(cache_entry, gpu_time)) {
                const uint64_t time_elapsed =
                    nvtop_difftime_u64(cache_entry->last_measurement_tstamp, current_time);
                gpu_usage_valid = gpuinfo_apple_calculate_gpu_usage(cache_entry->gpu_time, sample.gpu_time,
                                                                    time_elapsed, &gpu_usage);
              }
            } else {
              cache_entry = calloc(1, sizeof(*cache_entry));
              if (cache_entry)
                cache_entry->client_id = client_id;
            }

            if (cache_entry) {
              RESET_ALL(cache_entry->valid);
              if (sample.gpu_time_valid)
                SET_APPLE_CACHE(cache_entry, gpu_time, sample.gpu_time);
              cache_entry->last_measurement_tstamp = current_time;
              HASH_ADD_CLIENT(gpu_info->current_update_process_cache, cache_entry);
            }
          }

          gpuinfo_apple_add_process(_gpu_info, sample.pid, gpu_usage_valid, gpu_usage);
        }
        CFRelease(cf_props);
      }

      IOObjectRelease(child);
    }

    IOObjectRelease(iterator);
}
