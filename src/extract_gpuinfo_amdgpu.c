/*
 * Copyright (C) 2012 Lauri Kasanen
 * Copyright (C) 2018 Genesis Cloud Ltd.
 * Copyright (C) 2022 YiFei Zhu <zhuyifei1999@gmail.com>
 *
 * This file is part of Nvtop and adapted from radeontop.
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

#include "nvtop/extract_gpuinfo_common.h"

#include <assert.h>
#include <drm/amdgpu_drm.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

typedef struct amdgpu_device *amdgpu_device_handle;
typedef struct _drmDevice *drmDevicePtr;

// libdrm public structs seems ABI-stable according to latest git blame:
// https://gitlab.freedesktop.org/mesa/drm/-/blame/7c28f528309d/xf86drm.h#L846
// Copying the required struct defs here so we don't have a compile-time dependency

typedef unsigned int drm_magic_t;

#define DRM_NODE_PRIMARY 0
#define DRM_NODE_CONTROL 1
#define DRM_NODE_RENDER  2
#define DRM_NODE_MAX     3

#define DRM_BUS_PCI       0

#define DRM_ERR_NO_DEVICE  (-1001)
#define DRM_ERR_NO_ACCESS  (-1002)
#define DRM_ERR_NOT_ROOT   (-1003)
#define DRM_ERR_INVALID    (-1004)
#define DRM_ERR_NO_FD      (-1005)

typedef struct _drmPciBusInfo {
    uint16_t domain;
    uint8_t bus;
    uint8_t dev;
    uint8_t func;
} drmPciBusInfo, *drmPciBusInfoPtr;

typedef struct _drmPciDeviceInfo {
    uint16_t vendor_id;
    uint16_t device_id;
    uint16_t subvendor_id;
    uint16_t subdevice_id;
    uint8_t revision_id;
} drmPciDeviceInfo, *drmPciDeviceInfoPtr;

typedef struct _drmDevice {
  char **nodes;
  int available_nodes;
  int bustype;
  union {
    drmPciBusInfoPtr pci;
  } businfo;
  union {
    drmPciDeviceInfoPtr pci;
  } deviceinfo;
} drmDevice, *drmDevicePtr;

typedef struct _drmVersion {
  int     version_major;
  int     version_minor;
  int     version_patchlevel;
  int     name_len;
  char    *name;
  int     date_len;
  char    *date;
  int     desc_len;
  char    *desc;
} drmVersion, *drmVersionPtr;

static int (*drmGetDevices)(drmDevicePtr devices[], int max_devices);
static int (*drmGetDevices2)(uint32_t flags, drmDevicePtr devices[], int max_devices);
static void (*drmFreeDevices)(drmDevicePtr devices[], int count);

static drmVersionPtr (*drmGetVersion)(int fd);
static void (*drmFreeVersion)(drmVersionPtr);

static int (*drmGetMagic)(int fd, drm_magic_t * magic);
static int (*drmAuthMagic)(int fd, drm_magic_t magic);
static int (*drmDropMaster)(int fd);

struct amdgpu_gpu_info {
  uint32_t asic_id;
  uint32_t chip_rev;
  uint32_t chip_external_rev;
  uint32_t family_id;
  uint64_t ids_flags;
  uint64_t max_engine_clk;
  uint64_t max_memory_clk;
  uint32_t num_shader_engines;
  uint32_t num_shader_arrays_per_engine;
  uint32_t avail_quad_shader_pipes;
  uint32_t max_quad_shader_pipes;
  uint32_t cache_entries_per_quad_pipe;
  uint32_t num_hw_gfx_contexts;
  uint32_t rb_pipes;
  uint32_t enabled_rb_pipes_mask;
  uint32_t gpu_counter_freq;
  uint32_t backend_disable[4];
  uint32_t mc_arb_ramcfg;
  uint32_t gb_addr_cfg;
  uint32_t gb_tile_mode[32];
  uint32_t gb_macro_tile_mode[16];
  uint32_t pa_sc_raster_cfg[4];
  uint32_t pa_sc_raster_cfg1[4];
  uint32_t cu_active_number;
  uint32_t cu_ao_mask;
  uint32_t cu_bitmap[4][4];
  uint32_t vram_type;
  uint32_t vram_bit_width;
  uint32_t ce_ram_size;
  uint32_t vce_harvest_config;
  uint32_t pci_rev_id;
};

static int (*amdgpu_device_initialize)(int fd,
                                       uint32_t *major_version,
                                       uint32_t *minor_version,
                                       amdgpu_device_handle *device_handle);

static int (*amdgpu_device_deinitialize)(amdgpu_device_handle device_handle);

static const char *(*amdgpu_get_marketing_name)(amdgpu_device_handle dev);

static int (*amdgpu_query_gpu_info)(amdgpu_device_handle dev,
                                    struct amdgpu_gpu_info *info);

static int (*amdgpu_query_info)(amdgpu_device_handle dev, unsigned info_id,
                                unsigned size, void *value);

static int (*amdgpu_query_sensor_info)(amdgpu_device_handle dev, unsigned sensor_type,
                                       unsigned size, void *value);

static void *libdrm_handle;
static void *libdrm_amdgpu_handle;

static int last_libdrm_return_status = 0;
static char didnt_call_gpuinfo_init[] = "uninitialized";
static const char *local_error_string = didnt_call_gpuinfo_init;

struct gpu_info_amdgpu {
  struct gpu_info base;
  struct list_head allocate_list;

  drmVersionPtr drmVersion;
  int fd;
  amdgpu_device_handle amdgpu_device;
};

static LIST_HEAD(allocations);

static bool gpuinfo_amdgpu_init(void);
static void gpuinfo_amdgpu_shutdown(void);
static const char *gpuinfo_amdgpu_last_error_string(void);
static bool gpuinfo_amdgpu_get_device_handles(
    struct list_head *devices, unsigned *count,
    ssize_t *mask);
static void gpuinfo_amdgpu_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_amdgpu_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_amdgpu_get_running_processes(
    struct gpu_info *_gpu_info,
    unsigned *num_processes_recovered, struct gpu_process **processes_info);

struct gpu_vendor gpu_vendor_amdgpu = {
  .init = gpuinfo_amdgpu_init,
  .shutdown = gpuinfo_amdgpu_shutdown,
  .last_error_string = gpuinfo_amdgpu_last_error_string,
  .get_device_handles = gpuinfo_amdgpu_get_device_handles,
  .populate_static_info = gpuinfo_amdgpu_populate_static_info,
  .refresh_dynamic_info = gpuinfo_amdgpu_refresh_dynamic_info,
  .get_running_processes = gpuinfo_amdgpu_get_running_processes,
};

__attribute__((constructor))
static void init_extract_gpuinfo_amdgpu(void) {
  register_gpu_vendor(&gpu_vendor_amdgpu);
}

static int wrap_drmGetDevices(drmDevicePtr devices[], int max_devices) {
  assert(drmGetDevices2 || drmGetDevices);

  if (drmGetDevices2)
    return drmGetDevices2(0, devices, max_devices);
  return drmGetDevices(devices, max_devices);
}

static bool gpuinfo_amdgpu_init(void) {
  libdrm_handle = dlopen("libdrm.so", RTLD_LAZY);
  if (!libdrm_handle)
    libdrm_handle = dlopen("libdrm.so.2", RTLD_LAZY);
  if (!libdrm_handle)
    libdrm_handle = dlopen("libdrm.so.1", RTLD_LAZY);
  if (!libdrm_handle) {
    local_error_string = dlerror();
    return false;
  }

  drmGetDevices2 = dlsym(libdrm_handle, "drmGetDevices2");
  if (!drmGetDevices2)
    drmGetDevices = dlsym(libdrm_handle, "drmGetDevices");
  if (!drmGetDevices2 && !drmGetDevices)
    goto init_error_clean_exit;

  drmFreeDevices = dlsym(libdrm_handle, "drmFreeDevices");
  if (!drmFreeDevices)
    goto init_error_clean_exit;

  drmGetVersion = dlsym(libdrm_handle, "drmGetVersion");
  if (!drmGetVersion)
    goto init_error_clean_exit;

  drmFreeVersion = dlsym(libdrm_handle, "drmFreeVersion");
  if (!drmFreeVersion)
    goto init_error_clean_exit;

  drmGetMagic = dlsym(libdrm_handle, "drmGetMagic");
  if (!drmGetMagic)
    goto init_error_clean_exit;

  drmAuthMagic = dlsym(libdrm_handle, "drmAuthMagic");
  if (!drmAuthMagic)
    goto init_error_clean_exit;

  drmDropMaster = dlsym(libdrm_handle, "drmDropMaster");
  if (!drmDropMaster)
    goto init_error_clean_exit;

  libdrm_amdgpu_handle = dlopen("libdrm_amdgpu.so", RTLD_LAZY);
  if (!libdrm_amdgpu_handle)
    libdrm_amdgpu_handle = dlopen("libdrm_amdgpu.so.1", RTLD_LAZY);

  if (libdrm_amdgpu_handle) {
    amdgpu_device_initialize = dlsym(libdrm_amdgpu_handle, "amdgpu_device_initialize");
    amdgpu_device_deinitialize = dlsym(libdrm_amdgpu_handle, "amdgpu_device_deinitialize");
    amdgpu_get_marketing_name = dlsym(libdrm_amdgpu_handle, "amdgpu_get_marketing_name");
    amdgpu_query_info = dlsym(libdrm_amdgpu_handle, "amdgpu_query_info");
    amdgpu_query_gpu_info = dlsym(libdrm_amdgpu_handle, "amdgpu_query_gpu_info");
    amdgpu_query_sensor_info = dlsym(libdrm_amdgpu_handle, "amdgpu_query_sensor_info");
  }

  local_error_string = NULL;
  return true;

init_error_clean_exit:
  dlclose(libdrm_handle);
  libdrm_handle = NULL;
  return false;
}

static void gpuinfo_amdgpu_shutdown(void) {
  if (libdrm_handle) {
    dlclose(libdrm_handle);
    libdrm_handle = NULL;
    local_error_string = didnt_call_gpuinfo_init;
  }

  if (libdrm_amdgpu_handle) {
    dlclose(libdrm_amdgpu_handle);
    libdrm_amdgpu_handle = NULL;
  }

  struct gpu_info_amdgpu *allocated, *tmp;

  list_for_each_entry_safe(allocated, tmp, &allocations, allocate_list) {
    list_del(&allocated->allocate_list);
    free(allocated);
  }
}

static const char *gpuinfo_amdgpu_last_error_string(void) {
  if (local_error_string) {
    return local_error_string;
  } else if (last_libdrm_return_status < 0) {
    switch (last_libdrm_return_status) {
    case DRM_ERR_NO_DEVICE:
      return "no device\n";
    case DRM_ERR_NO_ACCESS:
      return "no access\n";
    case DRM_ERR_NOT_ROOT:
      return "not root\n";
    case DRM_ERR_INVALID:
      return "invalid args\n";
    case DRM_ERR_NO_FD:
      return "no fd\n";
    default:
      return "unknown error\n";
    }
  } else {
    return "An unanticipated error occurred while accessing NVIDIA GPU "
           "information\n";
  }
}

static void authenticate_drm(int fd) {
  drm_magic_t magic;

  if (drmGetMagic(fd, &magic) < 0) {
    return;
  }

  if (drmAuthMagic(fd, magic) == 0) {
    if (drmDropMaster(fd)) {
      perror("Failed to drop DRM master");
      fprintf(stderr, "\nWARNING: other DRM clients will crash on VT switch while nvtop is running!\npress ENTER to continue\n");
      fgetc(stdin);
    }
    return;
  }

  // XXX: Ideally I'd implement this too, but I'd need to pull in libxcb and yet
  // more functions and structs that may break ABI compatibility.
  // See radeontop auth_xcb.c for what is involved here
  fprintf(stderr, "Failed to authenticate to DRM; XCB authentication unimplemented\n");
}

#define VENDOR_AMD 0x1002

static bool gpuinfo_amdgpu_get_device_handles(
    struct list_head *devices, unsigned *count,
    ssize_t *mask) {
  if (!libdrm_handle)
    return false;

  last_libdrm_return_status = wrap_drmGetDevices(NULL, 0);
  if (last_libdrm_return_status <= 0)
    return false;

  drmDevicePtr devs[last_libdrm_return_status];
  last_libdrm_return_status = wrap_drmGetDevices(devs, last_libdrm_return_status);
  if (last_libdrm_return_status <= 0)
    return false;

  unsigned int libdrm_count = last_libdrm_return_status;
  struct gpu_info_amdgpu *gpu_infos = calloc(libdrm_count, sizeof(*gpu_infos));
  if (!gpu_infos) {
    local_error_string = strerror(errno);
    return false;
  }

  list_add(&gpu_infos[0].allocate_list, &allocations);

  for (unsigned int i = 0; i < libdrm_count; i++) {
    if (devs[i]->bustype != DRM_BUS_PCI ||
        devs[i]->deviceinfo.pci->vendor_id != VENDOR_AMD)
      continue;

    int fd = -1;

    for (unsigned int j = DRM_NODE_MAX - 1; j >= 0; j--) {
      if (!(1 << j & devs[i]->available_nodes))
        continue;

      if ((fd = open(devs[i]->nodes[j], O_RDWR)) < 0)
        continue;

      break;
    }

    if (fd < 0)
      continue;

    drmVersionPtr ver = drmGetVersion(fd);

    if (!ver) {
      close(fd);
      continue;
    }

    bool is_radeon = false; // TODO: !strcmp(ver->name, "radeon");
    bool is_amdgpu = !strcmp(ver->name, "amdgpu");

    if (!is_amdgpu && !is_radeon) {
      drmFreeVersion(ver);
      close(fd);
      continue;
    }

    if ((*mask & 1) == 0) {
      *mask >>= 1;
      drmFreeVersion(ver);
      close(fd);
      continue;
    }
    *mask >>= 1;

    authenticate_drm(fd);

    if (is_amdgpu) {
      if (!libdrm_amdgpu_handle || !amdgpu_device_initialize) {
        drmFreeVersion(ver);
        close(fd);
        continue;
      }

      uint32_t drm_major, drm_minor;
      last_libdrm_return_status = amdgpu_device_initialize(
          fd, &drm_major, &drm_minor,
          &gpu_infos[*count].amdgpu_device);
    } else {
      // TODO: radeon suppport here
      assert(false);
    }
    if (!last_libdrm_return_status) {
      gpu_infos[*count].drmVersion = ver;
      gpu_infos[*count].fd = fd;
      gpu_infos[*count].base.vendor = &gpu_vendor_amdgpu;
      list_add_tail(&gpu_infos[*count].base.list, devices);
      *count += 1;
    } else {
      drmFreeVersion(ver);
      close(fd);
      continue;
    }
  }

  drmFreeDevices(devs, libdrm_count);

  return true;
}

static void gpuinfo_amdgpu_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_amdgpu *gpu_info =
    container_of(_gpu_info, struct gpu_info_amdgpu, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
  bool info_query_success = false;
  struct amdgpu_gpu_info info;
  const char *name = NULL;

  if (libdrm_amdgpu_handle && amdgpu_get_marketing_name)
    name = amdgpu_get_marketing_name(gpu_info->amdgpu_device);

  if (libdrm_amdgpu_handle && amdgpu_query_gpu_info)
    info_query_success = !amdgpu_query_gpu_info(gpu_info->amdgpu_device, &info);

  static_info->device_name[MAX_DEVICE_NAME - 1] = '\0';
  if (name && strlen(name)) {
    strncpy(static_info->device_name, name, MAX_DEVICE_NAME - 1);
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
  } else if (gpu_info->drmVersion->desc && strlen(gpu_info->drmVersion->desc)) {
    strncpy(static_info->device_name, gpu_info->drmVersion->desc, MAX_DEVICE_NAME - 1);
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);

    if (info_query_success) {
      size_t len = strlen(static_info->device_name);
      assert(len < MAX_DEVICE_NAME);

      char *dst = static_info->device_name + len;
      size_t remaining_len = MAX_DEVICE_NAME - 1 - len;
      switch (info.family_id) {
      case AMDGPU_FAMILY_SI:
        strncpy(dst, " (Hainan / Oland / Verde / Pitcairn / Tahiti)", remaining_len);
        break;
      case AMDGPU_FAMILY_CI:
        strncpy(dst, " (Bonaire / Hawaii)", remaining_len);
        break;
      case AMDGPU_FAMILY_KV:
        strncpy(dst, " (Kaveri / Kabini / Mullins)", remaining_len);
        break;
      case AMDGPU_FAMILY_VI:
        strncpy(dst, " (Iceland / Tonga)", remaining_len);
        break;
      case AMDGPU_FAMILY_CZ:
        strncpy(dst, " (Carrizo / Stoney)", remaining_len);
        break;
      case AMDGPU_FAMILY_AI:
        strncpy(dst, " (Vega10)", remaining_len);
        break;
      case AMDGPU_FAMILY_RV:
        strncpy(dst, " (Raven)", remaining_len);
        break;
      case AMDGPU_FAMILY_NV:
        strncpy(dst, " (Navi10)", remaining_len);
        break;
      case AMDGPU_FAMILY_VGH:
        strncpy(dst, " (Van Gogh)", remaining_len);
        break;
      case AMDGPU_FAMILY_YC:
        strncpy(dst, " (Yellow Carp)", remaining_len);
        break;
      }
    }
  } else
    RESET_VALID(gpuinfo_device_name_valid, static_info->valid);
}

static void gpuinfo_amdgpu_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_amdgpu *gpu_info =
    container_of(_gpu_info, struct gpu_info_amdgpu, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  bool info_query_success = false;
  struct amdgpu_gpu_info info;
  uint32_t out32;

  if (libdrm_amdgpu_handle && amdgpu_query_gpu_info)
    info_query_success = !amdgpu_query_gpu_info(gpu_info->amdgpu_device, &info);

  // GPU current speed
  if (libdrm_amdgpu_handle && amdgpu_query_sensor_info)
    last_libdrm_return_status = amdgpu_query_sensor_info(
        gpu_info->amdgpu_device, AMDGPU_INFO_SENSOR_GFX_SCLK,
        sizeof(out32), &out32);
  if (!last_libdrm_return_status) {
    dynamic_info->gpu_clock_speed = out32;
    SET_VALID(gpuinfo_curr_gpu_clock_speed_valid, dynamic_info->valid);
  } else
    RESET_VALID(gpuinfo_curr_gpu_clock_speed_valid, dynamic_info->valid);

  // GPU max speed
  if (info_query_success) {
    dynamic_info->gpu_clock_speed_max = info.max_engine_clk / 1000;
    SET_VALID(gpuinfo_max_gpu_clock_speed_valid, dynamic_info->valid);
  } else
    RESET_VALID(gpuinfo_max_gpu_clock_speed_valid, dynamic_info->valid);

  // Memory current speed
  if (libdrm_amdgpu_handle && amdgpu_query_sensor_info)
    last_libdrm_return_status = amdgpu_query_sensor_info(
        gpu_info->amdgpu_device, AMDGPU_INFO_SENSOR_GFX_MCLK,
        sizeof(out32), &out32);
  if (!last_libdrm_return_status) {
    dynamic_info->mem_clock_speed = out32;
    SET_VALID(gpuinfo_curr_mem_clock_speed_valid, dynamic_info->valid);
  } else
    RESET_VALID(gpuinfo_curr_mem_clock_speed_valid, dynamic_info->valid);

  // Memory max speed
  if (info_query_success) {
    dynamic_info->mem_clock_speed_max = info.max_memory_clk / 1000;
    SET_VALID(gpuinfo_max_mem_clock_speed_valid, dynamic_info->valid);
  } else
    RESET_VALID(gpuinfo_max_mem_clock_speed_valid, dynamic_info->valid);

  // Load
  if (libdrm_amdgpu_handle && amdgpu_query_sensor_info)
    last_libdrm_return_status = amdgpu_query_sensor_info(
        gpu_info->amdgpu_device, AMDGPU_INFO_SENSOR_GPU_LOAD,
        sizeof(out32), &out32);
  if (!last_libdrm_return_status) {
    dynamic_info->gpu_util_rate = out32;
    SET_VALID(gpuinfo_gpu_util_rate_valid, dynamic_info->valid);
  } else
    RESET_VALID(gpuinfo_gpu_util_rate_valid, dynamic_info->valid);

  // Memory usage
  struct drm_amdgpu_memory_info memory_info;
  if (libdrm_amdgpu_handle && amdgpu_query_info)
    last_libdrm_return_status = amdgpu_query_info(
        gpu_info->amdgpu_device, AMDGPU_INFO_MEMORY,
        sizeof(memory_info), &memory_info);
  if (!last_libdrm_return_status) {
    // TODO: Determine if we want to include GTT (GPU accessible system memory)
    dynamic_info->total_memory = memory_info.vram.total_heap_size;
    dynamic_info->used_memory = memory_info.vram.heap_usage;
    dynamic_info->free_memory = memory_info.vram.usable_heap_size - dynamic_info->used_memory;
    dynamic_info->mem_util_rate =
      (dynamic_info->total_memory - dynamic_info->free_memory) * 100
      / dynamic_info->total_memory;
    SET_VALID(gpuinfo_total_memory_valid, dynamic_info->valid);
    SET_VALID(gpuinfo_used_memory_valid, dynamic_info->valid);
    SET_VALID(gpuinfo_free_memory_valid, dynamic_info->valid);
    SET_VALID(gpuinfo_mem_util_rate_valid, dynamic_info->valid);
  } else {
    RESET_VALID(gpuinfo_total_memory_valid, dynamic_info->valid);
    RESET_VALID(gpuinfo_used_memory_valid, dynamic_info->valid);
    RESET_VALID(gpuinfo_free_memory_valid, dynamic_info->valid);
    RESET_VALID(gpuinfo_mem_util_rate_valid, dynamic_info->valid);
  }

  // GPU temperature
  if (libdrm_amdgpu_handle && amdgpu_query_sensor_info)
    last_libdrm_return_status = amdgpu_query_sensor_info(
        gpu_info->amdgpu_device, AMDGPU_INFO_SENSOR_GPU_TEMP,
        sizeof(out32), &out32);
  if (!last_libdrm_return_status) {
    dynamic_info->gpu_temp = out32 / 1000;
    SET_VALID(gpuinfo_gpu_temp_valid, dynamic_info->valid);
  } else
    RESET_VALID(gpuinfo_gpu_temp_valid, dynamic_info->valid);

  // TODO: Fan speed
  // You can get the fan speed from sysfs hwmon pwm1

  // Device power usage
  if (libdrm_amdgpu_handle && amdgpu_query_sensor_info)
    last_libdrm_return_status = amdgpu_query_sensor_info(
        gpu_info->amdgpu_device, AMDGPU_INFO_SENSOR_GPU_AVG_POWER,
        sizeof(out32), &out32);
  if (!last_libdrm_return_status) {
    dynamic_info->power_draw = out32;
    SET_VALID(gpuinfo_power_draw_valid, dynamic_info->valid);
  } else
    RESET_VALID(gpuinfo_power_draw_valid, dynamic_info->valid);
}

static void gpuinfo_amdgpu_get_running_processes(
    struct gpu_info *_gpu_info,
    unsigned *num_processes_recovered, struct gpu_process **processes_info) {
  struct gpu_info_amdgpu *gpu_info =
    container_of(_gpu_info, struct gpu_info_amdgpu, base);

  // TODO
  (void)!gpu_info;
}
