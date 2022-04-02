/*
 * Copyright (C) 2012 Lauri Kasanen
 * Copyright (C) 2018 Genesis Cloud Ltd.
 * Copyright (C) 2022 YiFei Zhu <zhuyifei1999@gmail.com>
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
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
#include <ctype.h>
#include <dirent.h>
#include <dlfcn.h>
#include <drm/amdgpu_drm.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <linux/kcmp.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/sysmacros.h>
#include <sys/types.h>
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

#define PDEV_LEN 20

struct gpu_info_amdgpu {
  struct gpu_info base;
  struct list_head allocate_list;

  drmVersionPtr drmVersion;
  int fd;
  amdgpu_device_handle amdgpu_device;

  char pdev[PDEV_LEN];
  int sysfsFD; // file descriptor for the /sys/bus/pci/devices/<this gpu>/ folder
  int hwmonFD; // file descriptor for the /sys/bus/pci/devices/<this gpu>/hwmon/hwmon[0-9]+ folder

  // We poll the fan frequently enough and want to avoid the open/close overhead of the sysfs file
  FILE *fanSpeedFILE; // FILE* for this device current fan speed
  FILE *PCIeLinkSpeed; // FILE* for this device PCIe link speed
  FILE *PCIeLinkWidth; // FILE* for this device PCIe link width
  FILE *PCIeBW; // FILE* for this device PCIe bandwidth over one second
  // Used to compute the actual fan speed
  unsigned maxFanValue;
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
    return "An unanticipated error occurred while accessing AMDGPU "
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

static void initDeviceSysfsPaths(struct gpu_info_amdgpu *gpu_info) {
  // Open the device sys folder to gather information not available through the DRM driver
  char devicePath[22 + PDEV_LEN];
  snprintf(devicePath, sizeof(devicePath), "/sys/bus/pci/devices/%s", gpu_info->pdev);
  gpu_info->sysfsFD = open(devicePath, O_RDONLY);
  gpu_info->hwmonFD = -1;

  // Open the device hwmon folder (Fan speed are available there)
  static const char hwmon[] = "hwmon";
  if (gpu_info->sysfsFD >= 0) {
    int hwmondirFD = openat(gpu_info->sysfsFD, hwmon, O_RDONLY);
    if (hwmondirFD >= 0) {
      DIR *hwmonDir = fdopendir(hwmondirFD);
      if (!hwmonDir) {
        close(hwmondirFD);
      } else {
        struct dirent *dirEntry;
        while ((dirEntry = readdir(hwmonDir))) {
          // There should be one directory inside hwmon, with a name having the following pattern hwmon[0-9]+
          if (dirEntry->d_type == DT_DIR && strncmp(hwmon, dirEntry->d_name, sizeof(hwmon) - 1) == 0) {
            break;
          }
        }
        if (dirEntry) {
          gpu_info->hwmonFD = openat(dirfd(hwmonDir), dirEntry->d_name, O_RDONLY);
        }
        closedir(hwmonDir);
      }
    }
  }
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

      snprintf(gpu_infos[*count].pdev, PDEV_LEN - 1, "%04x:%02x:%02x.%d",
               devs[i]->businfo.pci->domain,
               devs[i]->businfo.pci->bus,
               devs[i]->businfo.pci->dev,
               devs[i]->businfo.pci->func);
      initDeviceSysfsPaths(&gpu_infos[*count]);
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

static int rewindAndReadPattern(FILE *file, const char *format, ...) {
  va_list args;
  va_start(args, format);
  rewind(file);
  fflush(file);
  int matches = vfscanf(file, format, args);
  va_end(args);
  return matches;
}

static int readValueFromFileAt(int folderFD, const char *fileName, const char *format, ...) {
  va_list args;
  va_start(args, format);
  // Open the file
  int fd = openat(folderFD, fileName, O_RDONLY);
  if (fd < 0)
    return 0;
  FILE *file = fdopen(fd, "r");
  if (!file) {
    close(fd);
    return 0;
  }
  // Read the pattern
  int nread = vfscanf(file, format, args);
  fclose(file);
  va_end(args);
  return nread;
}

// Converts the link speed in MT/s and laneWidth to a PCIe generation
static unsigned pcieGenFromLinkSpeedAndWidth(unsigned linkSpeed, unsigned laneWidth) {
  // Multiply by 100 to distinguish PCIe v1 and v2
  unsigned laneSpeed = linkSpeed / laneWidth;
  unsigned pcieGen = 0;
  switch (laneSpeed) {
  case 250:
    pcieGen = 1;
    break;
  case 500:
    pcieGen = 2;
    break;
  case 1000:
    pcieGen = 3;
    break;
  case 2000:
    pcieGen = 4;
    break;
  case 4000:
    pcieGen = 5;
    break;
  case 8000:
    pcieGen = 6;
    break;
  }
  return pcieGen;
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

  // Retrieve infos from sysfs.

  // 1) Fan
  // If multiple fans are present, use the first one. Some hardware do not wire
  // the sensor for the second fan, or use the same value as the first fan.

  // Look for which fan to use (PWM or RPM)
  gpu_info->fanSpeedFILE = NULL;
  unsigned pwmIsEnabled;
  int NreadPatterns = readValueFromFileAt(gpu_info->hwmonFD, "pwm1_enable", "%u", &pwmIsEnabled);
  bool usePWMSensor = NreadPatterns == 1 && pwmIsEnabled > 0;

  bool useRPMSensor = false;
  if (!usePWMSensor) {
    unsigned rpmIsEnabled;
    NreadPatterns = readValueFromFileAt(gpu_info->hwmonFD, "fan1_enable", "%u", &rpmIsEnabled);
    useRPMSensor = NreadPatterns && rpmIsEnabled > 0;
  }
  // Either RPM or PWM or neither
  assert((useRPMSensor ^ usePWMSensor) || (!useRPMSensor && !usePWMSensor));
  if (usePWMSensor || useRPMSensor) {
    char *maxFanSpeedFile = usePWMSensor ? "pwm1_max" : "fan1_max";
    char *fanSensorFile = usePWMSensor ? "pwm1" : "fan1_input";
    unsigned maxSpeedVal;
    NreadPatterns = readValueFromFileAt(gpu_info->hwmonFD, maxFanSpeedFile, "%u", &maxSpeedVal);
    if (NreadPatterns == 1) {
      gpu_info->maxFanValue = maxSpeedVal;
      // Open the fan file for dynamic info gathering
      int fanSpeedFD = openat(gpu_info->hwmonFD, fanSensorFile, O_RDONLY);
      if (fanSpeedFD >= 0) {
        gpu_info->fanSpeedFILE = fdopen(fanSpeedFD, "r");
        if (!gpu_info->fanSpeedFILE)
          close(fanSpeedFD);
      }
    }
  }

  // Critical temparature
  // temp1_* files should always be the GPU die in millidegrees Celsius
  unsigned criticalTemp;
  NreadPatterns = readValueFromFileAt(gpu_info->hwmonFD, "temp1_crit", "%u", &criticalTemp);
  if (NreadPatterns == 1) {
    static_info->temperature_slowdown_threshold = criticalTemp;
    SET_VALID(gpuinfo_temperature_slowdown_valid, static_info->valid);
  }

  // Emergency/shutdown temparature
  unsigned emergemcyTemp;
  NreadPatterns = readValueFromFileAt(gpu_info->hwmonFD, "temp1_emergency", "%u", &emergemcyTemp);
  if (NreadPatterns == 1) {
    static_info->temperature_shutdown_threshold = emergemcyTemp;
    SET_VALID(gpuinfo_temperature_shutdown_valid, static_info->valid);
  }

  // PCIe max link width
  unsigned maxLinkWidth;
  NreadPatterns = readValueFromFileAt(gpu_info->sysfsFD, "max_link_width", "%u", &maxLinkWidth);
  if (NreadPatterns == 1) {
    static_info->max_pcie_link_width = maxLinkWidth;
    SET_VALID(gpuinfo_max_link_width_valid, static_info->valid);
  }
  // Open current link width for dynamic info gathering
  gpu_info->PCIeLinkWidth = NULL;
  int linkWidthFD = openat(gpu_info->sysfsFD, "current_link_width", O_RDONLY);
  if (linkWidthFD) {
    gpu_info->PCIeLinkWidth = fdopen(linkWidthFD, "r");
  }

  // PCIe max link speed
  // [max|current]_link_speed export the value as "x.y GT/s PCIe" where x.y is a float value.
  float maxLinkSpeedf;
  NreadPatterns = readValueFromFileAt(gpu_info->sysfsFD, "max_link_speed", "%f GT/s PCIe", &maxLinkSpeedf);
  if (NreadPatterns == 1 && IS_VALID(gpuinfo_max_link_width_valid, static_info->valid)) {
    unsigned maxLinkSpeed = (unsigned)(maxLinkSpeedf * 1000.f);
    unsigned pcieGen = pcieGenFromLinkSpeedAndWidth(maxLinkSpeed, static_info->max_pcie_link_width);
    if (pcieGen) {
      static_info->max_pcie_gen = pcieGen;
      SET_VALID(gpuinfo_max_pcie_gen_valid, static_info->valid);
    }
  }
  // Open current link speed
  gpu_info->PCIeLinkSpeed = NULL;
  int linkSpeedFD = openat(gpu_info->sysfsFD, "current_link_speed", O_RDONLY);
  if (linkSpeedFD) {
    gpu_info->PCIeLinkSpeed = fdopen(linkSpeedFD, "r");
  }

  // Open the PCIe bandwidth file for dynamic info gathering
  gpu_info->PCIeBW = NULL;
  int pcieBWFD = openat(gpu_info->sysfsFD, "pcie_bw", O_RDONLY);
  if (pcieBWFD) {
    gpu_info->PCIeBW = fdopen(pcieBWFD, "r");
  }
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
  else
    last_libdrm_return_status = 1;
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
  else
    last_libdrm_return_status = 1;
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
  else
    last_libdrm_return_status = 1;
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
  else
    last_libdrm_return_status = 1;
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
  else
    last_libdrm_return_status = 1;
  if (!last_libdrm_return_status) {
    dynamic_info->gpu_temp = out32 / 1000;
    SET_VALID(gpuinfo_gpu_temp_valid, dynamic_info->valid);
  } else
    RESET_VALID(gpuinfo_gpu_temp_valid, dynamic_info->valid);

  // Fan speed
  if (gpu_info->fanSpeedFILE) {
    unsigned currentFanSpeed;
    int patternsMatched = rewindAndReadPattern(gpu_info->fanSpeedFILE, "%u", &currentFanSpeed);
    if (patternsMatched == 1) {
      dynamic_info->fan_speed = currentFanSpeed * 100 / gpu_info->maxFanValue;
      SET_VALID(gpuinfo_fan_speed_valid, dynamic_info->valid);
    } else
      RESET_VALID(gpuinfo_fan_speed_valid, dynamic_info->valid);
  } else
    RESET_VALID(gpuinfo_fan_speed_valid, dynamic_info->valid);

  // Device power usage
  if (libdrm_amdgpu_handle && amdgpu_query_sensor_info)
    last_libdrm_return_status = amdgpu_query_sensor_info(
        gpu_info->amdgpu_device, AMDGPU_INFO_SENSOR_GPU_AVG_POWER,
        sizeof(out32), &out32);
  else
    last_libdrm_return_status = 1;
  if (!last_libdrm_return_status) {
    dynamic_info->power_draw = out32 * 1000; // watts to milliwatts
    SET_VALID(gpuinfo_power_draw_valid, dynamic_info->valid);
  } else
    RESET_VALID(gpuinfo_power_draw_valid, dynamic_info->valid);

  // Current PCIe link used
  if (gpu_info->PCIeLinkWidth) {
    unsigned currentLinkWidth = 0;
    int NreadPatterns = rewindAndReadPattern(gpu_info->PCIeLinkWidth, "%u", &currentLinkWidth);
    if (NreadPatterns == 1) {
      dynamic_info->curr_pcie_link_width = currentLinkWidth;
      SET_VALID(gpuinfo_pcie_link_width_valid, dynamic_info->valid);
      if (gpu_info->PCIeLinkSpeed) {
        float currentLinkSpeedf;
        NreadPatterns = rewindAndReadPattern(gpu_info->PCIeLinkSpeed, "%f GT/s PCIe", &currentLinkSpeedf);
        if (NreadPatterns == 1) {
          unsigned currentLinkSpeed = (unsigned)(currentLinkSpeedf * 1000.f);
          unsigned pcieGen = pcieGenFromLinkSpeedAndWidth(currentLinkSpeed, currentLinkWidth);
          if (pcieGen) {
            dynamic_info->curr_pcie_link_gen = pcieGen;
            SET_VALID(gpuinfo_pcie_link_gen_valid, dynamic_info->valid);
          }
        }
      }
    }
  }
  // PCIe bandwidth
  if (gpu_info->PCIeBW) {
    // According to https://github.com/torvalds/linux/blob/master/drivers/gpu/drm/amd/pm/amdgpu_pm.c, under the pcie_bw
    // section, we should be able to read the number of packets received and sent by the GPU and get the maximum payload
    // size during the last second. This is untested but should work when the file is populated by the driver.
    uint64_t received, transmitted;
    int maxPayloadSize;
    int NreadPatterns = rewindAndReadPattern(gpu_info->PCIeBW, "%" SCNu64 " %" SCNu64 " %i", &received, &transmitted, &maxPayloadSize);
    if (NreadPatterns == 3) {
      received *= maxPayloadSize;
      transmitted *= maxPayloadSize;
      dynamic_info->pcie_rx = received;
      dynamic_info->pcie_tx = transmitted;
      SET_VALID(gpuinfo_pcie_rx_valid, dynamic_info->valid);
      SET_VALID(gpuinfo_pcie_tx_valid, dynamic_info->valid);
    }
  }
}

static bool is_drm_fd(int fd_dir_fd, const char *name) {
  struct stat stat;
  int ret;

  ret = fstatat(fd_dir_fd, name, &stat, 0);

  return ret == 0 &&
         (stat.st_mode & S_IFMT) == S_IFCHR &&
         major(stat.st_rdev) == 226;
}

static ssize_t read_whole_file(int dirfd, const char *pathname, char **data) {
  ssize_t read_size = 0;
  size_t buf_size = 0;
  char *buf = NULL;
  int fd;
  FILE *f;

  fd = openat(dirfd, pathname, O_RDONLY);
  if (fd < 0)
    return -1;

  f = fdopen(fd, "r");
  if (!f) {
    close(fd);
    return -1;
  }

  while (true) {
    if (read_size + 1024 > buf_size) {
      buf_size += 1024;
      buf = realloc(buf, buf_size);
      if (!buf)
        goto err;
    }

    size_t read_bytes = fread(buf + read_size, 1, 1024, f);
    if (read_bytes) {
      read_size += read_bytes;
      continue;
    }

    if (feof(f))
      break;

    goto err;
  }

  buf = realloc(buf, read_size + 1);
  if (!buf)
    goto err;

  buf[read_size] = 0;
  fclose(f);

  *data = buf;
  return read_size;

err:
  fclose(f);
  free(buf);

  return -1;
}

static bool extract_kv(char *buf, char **key, char **val)
{
  char *p = buf;

  p = index(buf, ':');
  if (!p || p == buf)
    return false;
  *p = '\0';

  while (*++p && isspace(*p));
  if (!*p)
    return false;

  *key = buf;
  *val = p;

  return true;
}

static bool parse_drm_fdinfo(struct gpu_info_amdgpu *gpu_info,
                             struct gpu_process *process_info,
                             int dir, const char *fd) {
  char *buf = NULL, *_buf;
  char *line, *ctx = NULL;
  ssize_t count;

  count = read_whole_file(dir, fd, &buf);
  if (count <= 0)
    return false;

  _buf = buf;

  while ((line = strtok_r(_buf, "\n", &ctx))) {
    char *key, *val;

    _buf = NULL;

    if (!extract_kv(line, &key, &val))
      continue;

    // see drivers/gpu/drm/amd/amdgpu/amdgpu_fdinfo.c amdgpu_show_fdinfo()
    if (!strcmp(key, "pdev")) {
      if (strcmp(val, gpu_info->pdev)) {
        free(buf);
        return false;
      }
    } else if (!strcmp(key, "vram mem")) {
      // TODO: do we count "gtt mem" too?
      unsigned long mem_int;
      char *endptr;

      mem_int = strtoul(val, &endptr, 10);
      if (endptr == val || strcmp(endptr, " kB"))
        continue;

      process_info->gpu_memory_usage = mem_int * 1024;
      SET_VALID(gpuinfo_process_gpu_memory_usage_valid, process_info->valid);
    } else {
      bool is_gfx = !strncmp(key, "gfx", sizeof("gfx") - 1);
      // bool is_compute = !strncmp(key, "compute", sizeof("compute") - 1);
      bool is_dec = !strncmp(key, "dec", sizeof("dec") - 1);
      bool is_enc = !strncmp(key, "enc", sizeof("enc") - 1);
      bool is_enc_1 = !strncmp(key, "enc_1", sizeof("enc_1") - 1);
      unsigned int usage_percent_int;
      char *key_off, *endptr;
      double usage_percent;

      if (is_gfx)
        key_off = key + sizeof("gfx") - 1;
      else if (is_dec)
        key_off = key + sizeof("dec") - 1;
      else if (is_enc_1)
        key_off = key + sizeof("enc_1") - 1;
      else if (is_enc)
        key_off = key + sizeof("enc") - 1;
      else
        continue;

      // The prefix should be followed by a number and only a number
      if (!*key_off)
        continue;
      strtoul(key_off, &endptr, 10);
      if (*endptr)
        continue;

      usage_percent_int = (unsigned int)(usage_percent = strtod(val, &endptr));
      if (endptr == val || strcmp(endptr, "%"))
        continue;

      if (is_gfx) {
        process_info->gpu_usage += usage_percent_int;
        SET_VALID(gpuinfo_process_gpu_usage_valid,
                  process_info->valid);
      } else if (is_dec) {
        process_info->decode_usage += usage_percent_int;
        SET_VALID(gpuinfo_process_gpu_decoder_valid,
                  process_info->valid);
      } else if (is_enc) {
        process_info->encode_usage += usage_percent_int;
        SET_VALID(gpuinfo_process_gpu_encoder_valid,
                  process_info->valid);
      }
    }
  }

  free(buf);
  return true;
}

static void gpuinfo_amdgpu_get_running_processes(
    struct gpu_info *_gpu_info,
    unsigned *num_processes_recovered, struct gpu_process **processes_info) {
  struct gpu_info_amdgpu *gpu_info =
    container_of(_gpu_info, struct gpu_info_amdgpu, base);
  unsigned int processes_info_capacity = 0;
  struct dirent *proc_dent;
  DIR *proc_dir;

  proc_dir = opendir("/proc");
  if (!proc_dir)
    return;

  while ((proc_dent = readdir(proc_dir)) != NULL) {
    int pid_dir_fd = -1, fd_dir_fd = -1, fdinfo_dir_fd = -1;
    DIR *fdinfo_dir = NULL;
    struct gpu_process *process_info = NULL;
    unsigned int seen_fds_capacity = 0;
    unsigned int seen_fds_len = 0;
    int *seen_fds = NULL;
    struct dirent *fdinfo_dent;
    unsigned int client_pid;

    if (proc_dent->d_type != DT_DIR)
      continue;
    if (!isdigit(proc_dent->d_name[0]))
      continue;

    pid_dir_fd = openat(dirfd(proc_dir), proc_dent->d_name, O_DIRECTORY);
    if (pid_dir_fd < 0)
      continue;

    client_pid = atoi(proc_dent->d_name);
    if (!client_pid)
      goto next;

    fd_dir_fd = openat(pid_dir_fd, "fd", O_DIRECTORY);
    if (fd_dir_fd < 0)
      goto next;

    fdinfo_dir_fd = openat(pid_dir_fd, "fdinfo", O_DIRECTORY);
    if (fdinfo_dir_fd < 0)
      goto next;

    fdinfo_dir = fdopendir(fdinfo_dir_fd);
    if (!fdinfo_dir) {
      close(fdinfo_dir_fd);
      goto next;
    }

    while ((fdinfo_dent = readdir(fdinfo_dir)) != NULL) {
      struct gpu_process processes_info_local = {0};
      int fd_num;

      if (fdinfo_dent->d_type != DT_REG)
        continue;
      if (!isdigit(fdinfo_dent->d_name[0]))
        continue;

      if (!is_drm_fd(fd_dir_fd, fdinfo_dent->d_name))
        continue;

      fd_num = atoi(fdinfo_dent->d_name);

      // check if this fd refers to the same open file as any seen ones.
      // we only care about unique opens
      for (unsigned i = 0; i < seen_fds_len; i++) {
        if (syscall(SYS_kcmp, client_pid, client_pid, KCMP_FILE,
                    fd_num, seen_fds[i]) <= 0)
          goto next_fd;
      }

      if (seen_fds_len == seen_fds_capacity) {
        seen_fds_capacity = seen_fds_capacity * 2 + 1;
        seen_fds = reallocarray(seen_fds, seen_fds_capacity, sizeof(*seen_fds));
        if (!seen_fds)
          goto next;
      }
      seen_fds[seen_fds_len++] = fd_num;

      if (!parse_drm_fdinfo(gpu_info, &processes_info_local,
                            fdinfo_dir_fd, fdinfo_dent->d_name))
        continue;

      if (!process_info) {
        if (*num_processes_recovered == processes_info_capacity) {
          processes_info_capacity = processes_info_capacity * 2 + 1;
          *processes_info = reallocarray(*processes_info, processes_info_capacity, sizeof(**processes_info));
          if (!*processes_info) {
            processes_info_capacity /= 2;
            goto next;
          }
        }

        process_info = &(*processes_info)[(*num_processes_recovered)++];
        memset(process_info, 0, sizeof(*process_info));

        process_info->type = gpu_process_graphical;
        // TODO: What condition would I have:
        //   process_info->type = gpu_process_compute;
        process_info->pid = client_pid;
      }

      if (IS_VALID(gpuinfo_process_gpu_memory_usage_valid,
                   processes_info_local.valid)) {
        process_info->gpu_memory_usage += processes_info_local.gpu_memory_usage;
        SET_VALID(gpuinfo_process_gpu_memory_usage_valid,
                  process_info->valid);
      }

      if (IS_VALID(gpuinfo_process_gpu_usage_valid,
                   processes_info_local.valid)) {
        process_info->gpu_usage += processes_info_local.gpu_usage;
        SET_VALID(gpuinfo_process_gpu_usage_valid,
                  process_info->valid);
      }

      if (IS_VALID(gpuinfo_process_gpu_encoder_valid,
                   processes_info_local.valid)) {
        process_info->encode_usage += processes_info_local.encode_usage;
        SET_VALID(gpuinfo_process_gpu_encoder_valid,
                  process_info->valid);
      }

      if (IS_VALID(gpuinfo_process_gpu_decoder_valid,
                   processes_info_local.valid)) {
        process_info->decode_usage += processes_info_local.encode_usage;
        SET_VALID(gpuinfo_process_gpu_decoder_valid,
                  process_info->valid);
      }

next_fd:
    }

next:
    if (fdinfo_dir)
      closedir(fdinfo_dir);

    if (fd_dir_fd >= 0)
      close(fd_dir_fd);
    close(pid_dir_fd);

    free(seen_fds);
  }

  closedir(proc_dir);
}
