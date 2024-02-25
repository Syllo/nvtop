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

#include "nvtop/common.h"
#include "nvtop/device_discovery.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/time.h"

#include <assert.h>
#include <Metal/Metal.h>
#include <IOKit/IOKitLib.h>
#include <QuartzCore/QuartzCore.h>

struct gpu_info_apple {
  struct gpu_info base;
  id<MTLDevice> device;
  io_service_t gpu_service;
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

static void gpuinfo_apple_shutdown(void) {
  for (unsigned i = 0; i < apple_gpu_count; ++i) {
    struct gpu_info_apple *gpu_info = &gpu_infos[i];
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
  NSArray<id<MTLDevice>> *mtl_devices = MTLCopyAllDevices();

  const unsigned mtl_count = [mtl_devices count];
  gpu_infos = calloc(mtl_count, sizeof(*gpu_infos));
  for (unsigned int i = 0; i < mtl_count; ++i) {
    id<MTLDevice> dev = mtl_devices[i];
    const uint64_t registry_id = [dev registryID];
    const io_service_t gpu_service = IOServiceGetMatchingService(kIOMainPortDefault, IORegistryEntryIDMatching(registry_id));
    assert(MACH_PORT_VALID(gpu_service));

    gpu_infos[apple_gpu_count].base.vendor = &gpu_vendor_apple;
    gpu_infos[apple_gpu_count].device = dev;
    gpu_infos[i].gpu_service = gpu_service;
    list_add_tail(&gpu_infos[apple_gpu_count].base.list, devices);
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
  strncpy(static_info->device_name, name, sizeof(static_info->device_name));
  SET_VALID(gpuinfo_device_name_valid, static_info->valid);

  static_info->integrated_graphics = [gpu_info->device location] == MTLDeviceLocationBuiltIn;
  static_info->encode_decode_shared = true;
}

static void gpuinfo_apple_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_apple *gpu_info = container_of(_gpu_info, struct gpu_info_apple, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  RESET_ALL(dynamic_info->valid);

  CFMutableDictionaryRef cf_props;
  if (IORegistryEntryCreateCFProperties(gpu_info->gpu_service, &cf_props, kCFAllocatorDefault, kNilOptions) != kIOReturnSuccess) {
    return;
  }
  NSDictionary *props = (__bridge NSDictionary*) cf_props;
  NSDictionary *performance_statistics = [props objectForKey:@"PerformanceStatistics"];
  if (!performance_statistics) {
    return;
  }

  id device_utilization_info = [performance_statistics objectForKey:@"Device Utilization %"];
  if (device_utilization_info != nil) {
    const uint64_t gpu_util_rate = [device_utilization_info integerValue];
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, gpu_util_rate);
  }

  if ([gpu_info->device hasUnifiedMemory]) {
    // [gpu_info->device currentAllocatedSize] returns the amount of memory allocated by this process, not
    // as allocated on the GPU globally. The performance statistics dictionary has the real value that we
    // are interested in, the amount of system memory allocated by the GPU.
    id system_memory_info = [performance_statistics objectForKey:@"Alloc system memory"];
    if (system_memory_info != nil) {
      const uint64_t mem_used = [system_memory_info integerValue];
      SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, mem_used);
    }

    // Memory is unified, so query the amount of system memory instead.
    mach_msg_type_number_t host_size = HOST_BASIC_INFO_COUNT;
    host_basic_info_data_t info;
    if (host_info(mach_host_self(), HOST_BASIC_INFO, (host_info_t) &info, &host_size) == KERN_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, info.max_mem);
    }
  } else {
    // TODO: Figure out how to get used memory for this case.

    // It does not really seem to be possible to get the amount of memory of a particular GPU.
    // In this case, just get the recommended working set size. This is what MoltenVK also does.
    const uint64_t mem_total = [gpu_info->device recommendedMaxWorkingSetSize];
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mem_total);
  }

  CFRelease(props);

  if (GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, used_memory) && GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, total_memory)) {
    SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, dynamic_info->total_memory - dynamic_info->used_memory);
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate,
                        (dynamic_info->total_memory - dynamic_info->free_memory) * 100 / dynamic_info->total_memory);

  }
}

static bool gpuinfo_apple_get_process_info(struct gpu_process* process, io_object_t user_client) {
  RESET_ALL(process->valid);
  process->type = gpu_process_graphical_compute;

  CFMutableDictionaryRef cf_props;
  if (IORegistryEntryCreateCFProperties(user_client, &cf_props, kCFAllocatorDefault, kNilOptions) != kIOReturnSuccess) {
    return false;
  }
  NSDictionary* user_client_info = (__bridge NSDictionary*) cf_props;

  id client_creator_info = [user_client_info objectForKey:@"IOUserClientCreator"];
  if (client_creator_info == nil) {
    return false;
  }

  const char* client_creator = [client_creator_info UTF8String];
  // Client creator is in form: pid <pid>, <name>
  if (sscanf(client_creator, "pid %u,", &process->pid) < 1) {
    return false;
  }

  CFRelease(cf_props);

  return true;
}

static void gpuinfo_apple_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_apple *gpu_info = container_of(_gpu_info, struct gpu_info_apple, base);
  _gpu_info->processes_count = 0;

  // We can find out which processes are running on a particular GPU using the IO Registry. The
  // IOService associated to the MTLDevice has "AGXDeviceUserClient" child nodes, which hold some
  // basic information about processes that are running on the GPU.

  io_iterator_t iterator;
  if (IORegistryEntryGetChildIterator(gpu_info->gpu_service, kIOServicePlane, &iterator) != kIOReturnSuccess) {
    return;
  }

  unsigned int count = 0;
  for (io_object_t child = IOIteratorNext(iterator); child; child = IOIteratorNext(iterator)) {
    io_name_t class_name;
    if (IOObjectGetClass(child, class_name) != kIOReturnSuccess) {
      continue;
    } else if (strncmp(class_name, "AGXDeviceUserClient", sizeof(class_name)) != 0) {
      continue;
    }

    if (_gpu_info->processes_array_size < count + 1) {
      _gpu_info->processes_array_size += COMMON_PROCESS_LINEAR_REALLOC_INC;
      _gpu_info->processes = reallocarray(_gpu_info->processes, _gpu_info->processes_array_size, sizeof(*_gpu_info->processes));
      if (!_gpu_info->processes) {
        perror("Could not allocate memory: ");
        exit(EXIT_FAILURE);
      }
    }

    if (gpuinfo_apple_get_process_info(&_gpu_info->processes[count], child)) {
      ++count;
    }

    IOObjectRelease(child);
  }

  _gpu_info->processes_count = count;
}
