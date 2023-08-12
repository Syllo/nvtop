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

  CFRelease(props);
}

static void gpuinfo_apple_get_running_processes(struct gpu_info *_gpu_info) {
  _gpu_info->processes_count = 0;
}
