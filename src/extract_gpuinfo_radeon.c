/*
 * Copyright (C) 2012 Lauri Kasanen
 * Copyright (C) 2018 Genesis Cloud Ltd.
 * Copyright (C) 2022 YiFei Zhu <zhuyifei1999@gmail.com>
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
 * Copyright (C) 2023 Advanced Micro Devices, Inc. All rights reserved.
 * Copyright (C) 2026 Gustavo Ramos Carvalho <gc5142387@gmail.com>.
 *
 * This file is part of Nvtop.
 * It is adapted from Nvtop (AMDGPU) and radeontop.
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
#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/time.h"
#include <assert.h>

#include <libdrm/radeon_drm.h>

#include <dlfcn.h>

#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <inttypes.h>

#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <xf86drm.h>



#ifndef RADEON_INFO_VRAM_USAGE
#define RADEON_INFO_VRAM_USAGE          0x1e
#endif
#ifndef RADEON_INFO_ACTIVE_CU_COUNT
#define RADEON_INFO_ACTIVE_CU_COUNT     0x20
#define RADEON_INFO_CURRENT_GPU_TEMP    0x21
#define RADEON_INFO_CURRENT_GPU_SCLK    0x22
#define RADEON_INFO_CURRENT_GPU_MCLK    0x23
#endif
#ifndef RADEON_INFO_READ_REG
#define RADEON_INFO_READ_REG            0x24
#endif


#ifndef	GRBM_STATUS
#define	GRBM_STATUS 0x8010
#define	    GUI_ACTIVE              (1 << 31)
#define     CB_BUSY                 (1 << 30)
#define     CP_BUSY                 (1 << 29)
#define     CP_COHERENCY_BUSY       (1 << 28)

#define     DB_BUSY                 (1 << 26)
#define     PA_BUSY                 (1 << 25)
#define     SC_BUSY                 (1 << 24)
#define SI_CIK_BCI_BUSY (1 << 23)
#define     SPI_BUSY                (1 << 22)
#define EVERGREEN_NI_SH_BUSY (1 << 21)
#define CIK_WD_BUSY (1 << 21)
#define     SX_BUSY                 (1 << 20)
#define NI_SI_CIK_IA_BUSY (1 << 19)
#define NI_SI_CIK_IA_BUSY_NO_DMA (1 << 18)
#define     VGT_BUSY                (1 << 17)
#define EVERGREEN_NI_VGT_BUSY_NO_DMA (1 << 16)
#define CIK_WD_BUSY_NO_DMA (1 << 16)
#define	NI_SI_CIK_GDS_BUSY (1 << 15)
#define     TA_BUSY                 (1 << 14)
#define	EVERGREEN_NI_SI_GRBM_EE_BUSY (1 << 10)
#endif

#ifndef SRBM_STATUS
#define SRBM_STATUS 0x0E50
#define CIK_UVD_BUSY (1 << 19)

#define     IH_BUSY                 (1 << 17)

#define EVERGREEN_NI_SI_RLC_BUSY (1 << 15)
#define     SEM_BUSY                (1 << 14)

#define     MCD_BUSY                (1 << 12)
#define     MCC_BUSY                (1 << 11)
#define     MCB_NON_DISPLAY_BUSY    (1 << 10)
#define		MCB_BUSY 				(1 << 9)
#define		VMC_BUSY 				(1 << 8)
#endif


#define PCI_VENDOR_ID_ATI 0x1002

#define PCI_VENDOR_ID_AMD 0x1022

#define RADEON_DRIVER_NAME "radeon"


#ifndef LIBDRM_MAX_DEVICES
#define LIBDRM_MAX_DEVICES 16
#endif

#define DRM_VERSION_IS_AT_LEAST(drm_version, major, minor) \
( \
    (drm_version) != NULL && \
    ( \
        ((drm_version)->version_major >  (major)) || \
        ((drm_version)->version_major == (major) && (drm_version)->version_minor >= (minor)) \
    ) \
)


#define RADEON_GPU_COUNT_MAX 8



enum radeon_chipset_family_e
{

    RADEON_CHIPSET_FAMILY_UNKNOWN = 0,

    RADEON_CHIPSET_FAMILY_R600,
    RADEON_CHIPSET_FAMILY_RV610,
    RADEON_CHIPSET_FAMILY_RV620,
    RADEON_CHIPSET_FAMILY_RV630,
    RADEON_CHIPSET_FAMILY_RV635,
    RADEON_CHIPSET_FAMILY_RV670,

    RADEON_CHIPSET_FAMILY_RV710,
    RADEON_CHIPSET_FAMILY_RV730,
    RADEON_CHIPSET_FAMILY_RV740,
    RADEON_CHIPSET_FAMILY_RV770,

    RADEON_CHIPSET_FAMILY_RS780,
    RADEON_CHIPSET_FAMILY_RS880,

    /* evergreen */
    RADEON_CHIPSET_FAMILY_CEDAR,
    RADEON_CHIPSET_FAMILY_REDWOOD,
    RADEON_CHIPSET_FAMILY_JUNIPER,
    RADEON_CHIPSET_FAMILY_CYPRESS,
    RADEON_CHIPSET_FAMILY_HEMLOCK,

    RADEON_CHIPSET_FAMILY_PALM,
    RADEON_CHIPSET_FAMILY_SUMO,
    RADEON_CHIPSET_FAMILY_SUMO2,

    /* ni */
    RADEON_CHIPSET_FAMILY_CAYMAN,
    RADEON_CHIPSET_FAMILY_BARTS,
    RADEON_CHIPSET_FAMILY_TURKS,
    RADEON_CHIPSET_FAMILY_CAICOS,

    RADEON_CHIPSET_FAMILY_ARUBA,

    /* si */
    RADEON_CHIPSET_FAMILY_TAHITI,
    RADEON_CHIPSET_FAMILY_PITCAIRN,
    RADEON_CHIPSET_FAMILY_VERDE,
    RADEON_CHIPSET_FAMILY_OLAND,
    RADEON_CHIPSET_FAMILY_HAINAN,

    /* cik */
    RADEON_CHIPSET_FAMILY_BONAIRE,
    RADEON_CHIPSET_FAMILY_KABINI,
    RADEON_CHIPSET_FAMILY_MULLINS,
    RADEON_CHIPSET_FAMILY_KAVERI,
    RADEON_CHIPSET_FAMILY_HAWAII

};


struct gpu_info_radeon_s
{

    struct gpu_info base;


    int drm_device_node_fd;


    drmVersionPtr drm_device_driver_version;

    drmVersionPtr drm_device_lib_version;


    uint16_t drm_device_pci_device_id;

    uint8_t drm_device_pci_revision_id;


    enum radeon_chipset_family_e chipset_family;


    unsigned int mem_clock_speed_max_;


    unsigned long long total_memory_;


    // We poll the fan frequently enough and want to avoid the open/close overhead of the sysfs file
    FILE *fanSpeedFILE; // FILE* for this device current fan speed


    nvtop_device *amdgpuDevice; // The AMDGPU driver device
    nvtop_device *hwmonDevice;  // The AMDGPU driver hwmon


    // Used to compute the actual fan speed
    unsigned maxFanValue;

};



bool gpuinfo_radeon_init(void);
void gpuinfo_radeon_shutdown(void);

const char *gpuinfo_radeon_last_error_string(void);

bool gpuinfo_radeon_get_device_handles(struct list_head *devices, unsigned *count);

void gpuinfo_radeon_populate_static_info(struct gpu_info *gpu_info);
void gpuinfo_radeon_refresh_dynamic_info(struct gpu_info *gpu_info);
void gpuinfo_radeon_refresh_running_processes(struct gpu_info *gpu_info);



static void *_libdrm_dl_handle = NULL;
static char *_libdrm_dl_error = NULL;

static typeof(drmGetVersion) *_libdrm_drmGetVersion;
static typeof(drmGetLibVersion) *_libdrm_drmGetLibVersion;

static typeof(drmFreeVersion) *_libdrm_drmFreeVersion;

static typeof(drmGetMagic) *_libdrm_drmGetMagic;

static typeof(drmCommandWriteRead) *_libdrm_drmCommandWriteRead;

static typeof(drmAuthMagic) *_libdrm_drmAuthMagic;

static typeof(drmDropMaster) *_libdrm_drmDropMaster;

static typeof(drmGetDevices) *_libdrm_drmGetDevices;
static typeof(drmFreeDevices) *_libdrm_drmFreeDevices;
static typeof(drmGetDevices2) *_libdrm_drmGetDevices2;

static int  _libdrm_last_return = 0;


static char *errno_last_strerror = NULL;


struct gpu_vendor gpu_vendor_radeon =
{

    .init = gpuinfo_radeon_init,
    .shutdown = gpuinfo_radeon_shutdown,

    .last_error_string = gpuinfo_radeon_last_error_string,

    .get_device_handles = gpuinfo_radeon_get_device_handles,

    .populate_static_info = gpuinfo_radeon_populate_static_info,
    .refresh_dynamic_info = gpuinfo_radeon_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_radeon_refresh_running_processes,

    .name = "radeon"

};


static unsigned radeon_gpu_count = 0;

static struct gpu_info_radeon_s gpu_infos[RADEON_GPU_COUNT_MAX];



__attribute__((constructor)) static void init_extract_gpuinfo_radeon(void) { register_gpu_vendor(&gpu_vendor_radeon); }



static
int _libdrm_drmGetDevicesX(drmDevicePtr devices[], int max_devices)
{

    if (_libdrm_drmGetDevices2 != NULL)
    {

        return _libdrm_drmGetDevices2(0, devices, max_devices);

    }


    return _libdrm_drmGetDevices(devices, max_devices);

}


static
void _amdgpu_authenticate_drm(int fd) {
  drm_magic_t magic;

  if (_libdrm_drmGetMagic(fd, &magic) < 0) {
    return;
  }

  if (_libdrm_drmAuthMagic(fd, magic) == 0) {
    if (_libdrm_drmDropMaster(fd)) {
      perror("Failed to drop DRM master");
      fprintf(
          stderr,
          "\nWARNING: other DRM clients will crash on VT switch while nvtop is running!\npress ENTER to continue\n");
      fgetc(stdin);
    }
    return;
  }

  // XXX: Ideally I'd implement this too, but I'd need to pull in libxcb and yet
  // more functions and structs that may break ABI compatibility.
  // See radeontop auth_xcb.c for what is involved here
  fprintf(stderr, "Failed to authenticate to DRM; XCB authentication unimplemented\n");
}

static
int _amdgpu_readAttributeFromDevice(nvtop_device *dev, const char *sysAttr, const char *format, ...) {
  va_list args;
  va_start(args, format);
  const char *val;
  int ret = nvtop_device_get_sysattr_value(dev, sysAttr, &val);
  if (ret < 0) {
    va_end(args);
    return ret;
  }
  // Read the pattern
  int nread = vsscanf(val, format, args);
  va_end(args);
  return nread;
}

static
int _amdgpu_rewindAndReadPattern(FILE *file, const char *format, ...) {
  if (!file)
    return 0;
  va_list args;
  va_start(args, format);
  rewind(file);
  fflush(file);
  int matches = vfscanf(file, format, args);
  va_end(args);
  return matches;
}

static
void _amdgpu_initDeviceSysfsPaths(struct gpu_info_radeon_s *gpu_info) {
  // Open the device sys folder to gather information not available through the DRM driver
  char devicePath[22 + PDEV_LEN];
  snprintf(devicePath, sizeof(devicePath), "/sys/bus/pci/devices/%s", gpu_info->base.pdev);
  nvtop_device_new_from_syspath(&gpu_info->amdgpuDevice, devicePath);
  assert(gpu_info->amdgpuDevice != NULL);

  gpu_info->hwmonDevice = nvtop_device_get_hwmon(gpu_info->amdgpuDevice);
  if (gpu_info->hwmonDevice) {
    // Open the device hwmon folder (Fan speed are available there)
    const char *hwmonPath;
    nvtop_device_get_syspath(gpu_info->hwmonDevice, &hwmonPath);
    int hwmonFD = open(hwmonPath, O_RDONLY);

    // Look for which fan to use (PWM or RPM)
    gpu_info->fanSpeedFILE = NULL;
    unsigned pwmIsEnabled;
    int NreadPatterns = _amdgpu_readAttributeFromDevice(gpu_info->hwmonDevice, "pwm1_enable", "%u", &pwmIsEnabled);
    bool usePWMSensor = NreadPatterns == 1 && pwmIsEnabled > 0;

    bool useRPMSensor = false;
    if (!usePWMSensor) {
      unsigned rpmIsEnabled;
      NreadPatterns = _amdgpu_readAttributeFromDevice(gpu_info->hwmonDevice, "fan1_enable", "%u", &rpmIsEnabled);
      useRPMSensor = NreadPatterns && rpmIsEnabled > 0;
    }
    // Either RPM or PWM or neither
    assert((useRPMSensor ^ usePWMSensor) || (!useRPMSensor && !usePWMSensor));
    if (usePWMSensor || useRPMSensor) {
      char *maxFanSpeedFile = usePWMSensor ? "pwm1_max" : "fan1_max";
      char *fanSensorFile = usePWMSensor ? "pwm1" : "fan1_input";
      unsigned maxSpeedVal;
      NreadPatterns = _amdgpu_readAttributeFromDevice(gpu_info->hwmonDevice, maxFanSpeedFile, "%u", &maxSpeedVal);
      if (NreadPatterns == 1) {
        gpu_info->maxFanValue = maxSpeedVal;
        // Open the fan file for dynamic info gathering
        int fanSpeedFD = openat(hwmonFD, fanSensorFile, O_RDONLY);
        if (fanSpeedFD >= 0) {
          gpu_info->fanSpeedFILE = fdopen(fanSpeedFD, "r");
          if (!gpu_info->fanSpeedFILE)
            close(fanSpeedFD);
        }
      }
    }
    close(hwmonFD);
  }

}


static
int radeon_get_chipset_name(uint16_t pci_device_id, uint8_t pci_revision_id, const char **name)
{

    (void)pci_revision_id;


    if (name == NULL)
    {

        return EINVAL;

    }


    switch (pci_device_id)
    {

#define CHIPSET(chipset_pci_id, chipset_name, chipset_family) case chipset_pci_id: *name = #chipset_name; break;
#include <libdrm/r600_pci_ids.h>
#undef CHIPSET


    default:
        *name = "UNKNOWN";
        return ERANGE;

    }


    return 0;

}

static
int radeon_get_chipset_family(uint16_t pci_device_id, uint8_t pci_revision_id, enum radeon_chipset_family_e *family)
{

    (void)pci_revision_id;


    if (family == NULL)
    {

        return EINVAL;

    }


    switch (pci_device_id)
    {

#define CHIPSET(chipset_pci_id, chipset_name, chipset_family) case chipset_pci_id: *family = RADEON_CHIPSET_FAMILY_##chipset_family; break;
#include <libdrm/r600_pci_ids.h>
#undef CHIPSET


    default:
        *family = RADEON_CHIPSET_FAMILY_UNKNOWN;
        return ERANGE;

    }


    return 0;

}


static
int radeon_get_drm_value(int fd, unsigned int request, uint32_t *value)
{
    struct drm_radeon_info info;
    memset(&info, 0, sizeof(info));
    info.value = (unsigned long)value;
    info.request = request;
    return _libdrm_drmCommandWriteRead(fd, DRM_RADEON_INFO, &info, sizeof(info));
}

static
int radeon_get_drm_grbm_value(int fd, uint32_t *grbm_status)
{
    *grbm_status = GRBM_STATUS;
    return radeon_get_drm_value(fd, RADEON_INFO_READ_REG, grbm_status);
}


bool gpuinfo_radeon_init(void)
{

    _libdrm_dl_handle = dlopen("libdrm.so"  , RTLD_LAZY);

    if (_libdrm_dl_handle == NULL)
    {

        _libdrm_dl_handle = dlopen("libdrm.so.2", RTLD_LAZY);

    }

    if (_libdrm_dl_handle == NULL)
    {

        _libdrm_dl_handle = dlopen("libdrm.so.1", RTLD_LAZY);

    }

    if (_libdrm_dl_handle == NULL)
    {

        _libdrm_dl_error = dlerror();

        return false;

    }


    _libdrm_drmGetVersion = dlsym(_libdrm_dl_handle, "drmGetVersion");
    _libdrm_drmGetLibVersion = dlsym(_libdrm_dl_handle, "drmGetLibVersion");

    _libdrm_drmFreeVersion = dlsym(_libdrm_dl_handle, "drmFreeVersion");

    _libdrm_drmGetMagic = dlsym(_libdrm_dl_handle, "drmGetMagic");

    _libdrm_drmCommandWriteRead = dlsym(_libdrm_dl_handle, "drmCommandWriteRead");

    _libdrm_drmAuthMagic = dlsym(_libdrm_dl_handle, "drmAuthMagic");

    _libdrm_drmDropMaster = dlsym(_libdrm_dl_handle, "drmDropMaster");

    _libdrm_drmGetDevices = dlsym(_libdrm_dl_handle, "drmGetDevices");
    _libdrm_drmFreeDevices = dlsym(_libdrm_dl_handle, "drmFreeDevices");
    _libdrm_drmGetDevices2 = dlsym(_libdrm_dl_handle, "drmGetDevices2");

    if (
        (_libdrm_drmGetDevices == NULL && _libdrm_drmGetDevices2 == NULL) ||
        (_libdrm_drmFreeDevices == NULL)
        )
    {

        goto gpuinfo_radeon_init__libdrm_dl_error;

    }


    return true;


gpuinfo_radeon_init__libdrm_dl_error:

    _libdrm_dl_error = dlerror();

    dlclose(_libdrm_dl_handle);

    _libdrm_dl_handle = NULL;


    return false;

}

void gpuinfo_radeon_shutdown(void)
{

    for ( ; radeon_gpu_count > 0; --radeon_gpu_count)
    {

        struct gpu_info_radeon_s *gpu_info = &gpu_infos[radeon_gpu_count - 1];


        close(gpu_info->drm_device_node_fd);


        _libdrm_drmFreeVersion(gpu_info->drm_device_driver_version);

        _libdrm_drmFreeVersion(gpu_info->drm_device_lib_version);


    if (gpu_info->fanSpeedFILE)
      fclose(gpu_info->fanSpeedFILE);

    nvtop_device_unref(gpu_info->amdgpuDevice);
    nvtop_device_unref(gpu_info->hwmonDevice);

    }


    if (_libdrm_dl_handle)
    {

        _libdrm_dl_error = NULL;

        dlclose(_libdrm_dl_handle);

        _libdrm_dl_handle = NULL;

    }

}


const char *gpuinfo_radeon_last_error_string(void)
{

    if (_libdrm_dl_error != NULL)
    {

        return (const char *)_libdrm_dl_error;

    }

    if (_libdrm_last_return < 0)
    {

        switch (_libdrm_last_return)
        {

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
            return "LIBDRM: unknown";

        }

    }

    if (errno_last_strerror != NULL)
    {

        return (const char *)errno_last_strerror;

    }


    return  "An unanticipated error occurred while accessing radeon "
            "information\n";;

}


bool gpuinfo_radeon_get_device_handles(struct list_head *devices, unsigned *count)
{

    drmDevicePtr drm_devices[LIBDRM_MAX_DEVICES];

    _libdrm_last_return = _libdrm_drmGetDevicesX(drm_devices, LIBDRM_MAX_DEVICES);

    if (_libdrm_last_return <= 0)
    {

        return false;

    }


    int drm_device_count = _libdrm_last_return;

    for (int i = 0; i < drm_device_count; i++)
    {

        drmDevicePtr drm_device = drm_devices[(size_t)i];

        if (
            drm_device->bustype != DRM_BUS_PCI ||
            drm_device->deviceinfo.pci->vendor_id != PCI_VENDOR_ID_ATI
            )
        {

            continue;

        }


        int drm_device_node_fd = -1;

        if (1 << DRM_NODE_RENDER & drm_device->available_nodes)
        {

            drm_device_node_fd = open(drm_device->nodes[DRM_NODE_RENDER], O_RDWR);

        }

        if (drm_device_node_fd < 0)
        {

            if (1 << DRM_NODE_PRIMARY & drm_device->available_nodes)
            {

                drm_device_node_fd = open(drm_device->nodes[DRM_NODE_PRIMARY], O_RDWR);

            }

        }

        if (drm_device_node_fd < 0)
        {

            continue;

        }


        drmVersionPtr drm_device_driver_version = _libdrm_drmGetVersion(drm_device_node_fd);

        if (drm_device_driver_version == NULL)
        {

            close(drm_device_node_fd);

            continue;

        }

        if (
            strcmp(drm_device_driver_version->name, RADEON_DRIVER_NAME) != 0
            )
        {

            close(drm_device_node_fd);

            continue;

        }


        _amdgpu_authenticate_drm(drm_device_node_fd);


        struct gpu_info_radeon_s *gpu_info = &gpu_infos[(size_t)radeon_gpu_count];

        ++radeon_gpu_count;

        gpu_info->base.vendor = &gpu_vendor_radeon;

        gpu_info->base.processes_count = 0;

        gpu_info->base.processes = NULL;

        gpu_info->base.processes_array_size = 0;


        gpu_info->mem_clock_speed_max_ = 0;


        snprintf(
            gpu_info->base.pdev, PDEV_LEN - 1, "%04x:%02x:%02x.%d",
            drm_device->businfo.pci->domain,
            drm_device->businfo.pci->bus,
            drm_device->businfo.pci->dev,
            drm_device->businfo.pci->func
            );


        gpu_info->drm_device_node_fd = drm_device_node_fd;


        gpu_info->drm_device_driver_version = drm_device_driver_version;

        gpu_info->drm_device_lib_version = _libdrm_drmGetLibVersion(
            gpu_info->drm_device_node_fd
            );


        gpu_info->drm_device_pci_device_id = drm_device->deviceinfo.pci->device_id;

        gpu_info->drm_device_pci_revision_id = drm_device->deviceinfo.pci->revision_id;


        radeon_get_chipset_family(gpu_info->drm_device_pci_device_id, gpu_info->drm_device_pci_revision_id, &gpu_info->chipset_family);


        if (
            DRM_VERSION_IS_AT_LEAST(drm_device_driver_version, 2, 3)
            )
        {
            struct drm_radeon_gem_info drm_radeon_gem_info_;
            _libdrm_last_return = _libdrm_drmCommandWriteRead(
                gpu_info->drm_device_node_fd,
                DRM_RADEON_GEM_INFO,
                &drm_radeon_gem_info_,
                sizeof(drm_radeon_gem_info_)
                );
            if (_libdrm_last_return == 0)
            {
                gpu_info->total_memory_ = (unsigned long long)drm_radeon_gem_info_.vram_size;
            }
        }


        _amdgpu_initDeviceSysfsPaths(gpu_info);


        list_add_tail(&gpu_info->base.list, devices);

    }

    _libdrm_drmFreeDevices(drm_devices, drm_device_count);

    *count = radeon_gpu_count;


    return true;

}


void gpuinfo_radeon_populate_static_info(struct gpu_info *_gpu_info)
{

    struct gpu_info_radeon_s *gpu_info = container_of(_gpu_info, struct gpu_info_radeon_s, base);

    struct gpuinfo_static_info *gpu_info_static_info = &gpu_info->base.static_info;

    RESET_ALL(gpu_info_static_info->valid);


    const char *chipset_name;

    int q = radeon_get_chipset_name(gpu_info->drm_device_pci_device_id, gpu_info->drm_device_pci_revision_id, &chipset_name);

    if (q != 0)
    {

        fprintf(
            stderr,
            "WARNING: unknown radeon chipset (PCI ID: 0x%04x, REVISION ID: 0x%02x).\n"
            "Press ENTER to continue...\n",
            gpu_info->drm_device_pci_device_id,
            gpu_info->drm_device_pci_revision_id
            );

        fgetc(stdin);

    }

    strncpy(
        (char *)gpu_info_static_info->device_name,
        chipset_name,
        MAX_DEVICE_NAME - 1
        );

    gpu_info_static_info->device_name[MAX_DEVICE_NAME - 1] = '\0';

    SET_VALID(gpuinfo_device_name_valid, gpu_info_static_info->valid);


  nvtop_pcie_link max_link_characteristics;
  int ret = nvtop_device_maximum_pcie_link(gpu_info->amdgpuDevice, &max_link_characteristics);
  if (ret >= 0) {
    SET_GPUINFO_STATIC(gpu_info_static_info, max_pcie_link_width, max_link_characteristics.width);
    unsigned pcieGen = nvtop_pcie_gen_from_link_speed(max_link_characteristics.speed);
    SET_GPUINFO_STATIC(gpu_info_static_info, max_pcie_gen, pcieGen);
  }


  // Critical temperature
  // temp1_* files should always be the GPU die in millidegrees Celsius
  if (gpu_info->hwmonDevice) {
    unsigned criticalTemp;
    int nReadPatterns = _amdgpu_readAttributeFromDevice(gpu_info->hwmonDevice, "temp1_crit", "%u", &criticalTemp);
    if (nReadPatterns == 1) {
            if (criticalTemp != 0)
            {

                criticalTemp /= 1000;

            }
      SET_GPUINFO_STATIC(gpu_info_static_info, temperature_slowdown_threshold, criticalTemp);
    }

    // Emergency/shutdown temperature
    unsigned emergencyTemp;
    nReadPatterns = _amdgpu_readAttributeFromDevice(gpu_info->hwmonDevice, "temp1_emergency", "%u", &emergencyTemp);
    if (nReadPatterns == 1) {
            if (emergencyTemp != 0)
            {

                emergencyTemp /= 1000;

            }
      SET_GPUINFO_STATIC(gpu_info_static_info, temperature_shutdown_threshold, emergencyTemp);
    }
  }


    /*
    *   2.39.0 - Add INFO query for number of active CUs
    */
	if (
        DRM_VERSION_IS_AT_LEAST(gpu_info->drm_device_driver_version, 2, 39)
        )
    {

        /**
         * XXX:
         * Ideally, instead of returning the active CU count, this should return
         * the number of shading units, derived from active CU count and chipset family.
         */

        uint32_t radeon_active_cu_count = 0;

        _libdrm_last_return = radeon_get_drm_value(
            gpu_info->drm_device_node_fd,
            RADEON_INFO_ACTIVE_CU_COUNT,
            &radeon_active_cu_count
            );

        SET_GPUINFO_STATIC(gpu_info_static_info, n_shared_cores, radeon_active_cu_count);

    }


    switch (gpu_info->chipset_family)
    {

    case RADEON_CHIPSET_FAMILY_RS780:
    case RADEON_CHIPSET_FAMILY_RS880:

    case RADEON_CHIPSET_FAMILY_SUMO:
    case RADEON_CHIPSET_FAMILY_SUMO2:
    case RADEON_CHIPSET_FAMILY_PALM:

    case RADEON_CHIPSET_FAMILY_KABINI:
    case RADEON_CHIPSET_FAMILY_MULLINS:
    case RADEON_CHIPSET_FAMILY_KAVERI:
        gpu_info_static_info->integrated_graphics = true;
        break;


    default:
        gpu_info_static_info->integrated_graphics = false;
        break;

    }

}

void gpuinfo_radeon_refresh_dynamic_info(struct gpu_info *_gpu_info)
{

    struct gpu_info_radeon_s *gpu_info = container_of(_gpu_info, struct gpu_info_radeon_s, base);

    struct gpuinfo_dynamic_info *gpu_info_dynamic_info = &gpu_info->base.dynamic_info;

    RESET_ALL(gpu_info_dynamic_info->valid);


	if (
        DRM_VERSION_IS_AT_LEAST(gpu_info->drm_device_driver_version, 2, 42)
        )
    {

        uint32_t gpu_sclk = 0;

        radeon_get_drm_value(gpu_info->drm_device_node_fd, RADEON_INFO_CURRENT_GPU_SCLK, &gpu_sclk);

        SET_GPUINFO_DYNAMIC(gpu_info_dynamic_info, gpu_clock_speed, (unsigned int)gpu_sclk);

        uint32_t gpu_sclk_max = 0;

        radeon_get_drm_value(gpu_info->drm_device_node_fd, RADEON_INFO_MAX_SCLK, &gpu_sclk_max);

        if (gpu_sclk_max != 0)
        {

            gpu_sclk_max /= 1000;

        }

        SET_GPUINFO_DYNAMIC(gpu_info_dynamic_info, gpu_clock_speed_max, (unsigned int)gpu_sclk_max);


        uint32_t gpu_mclk = 0;

        radeon_get_drm_value(gpu_info->drm_device_node_fd, RADEON_INFO_CURRENT_GPU_MCLK, &gpu_mclk);

        SET_GPUINFO_DYNAMIC(gpu_info_dynamic_info, mem_clock_speed, (unsigned int)gpu_mclk);

        if (gpu_info->mem_clock_speed_max_ < gpu_mclk)
        {

            gpu_info->mem_clock_speed_max_ = gpu_mclk;

        }

        SET_GPUINFO_DYNAMIC(gpu_info_dynamic_info, mem_clock_speed_max, gpu_info->mem_clock_speed_max_);

    }


    unsigned long long total_memory_ = (unsigned long long)gpu_info->total_memory_;

    if (gpu_info->total_memory_ != 0)
    {

        SET_GPUINFO_DYNAMIC(gpu_info_dynamic_info, total_memory, total_memory_);

    }


    if (
        DRM_VERSION_IS_AT_LEAST(gpu_info->drm_device_driver_version, 2, 39)
        )
    {

        uint64_t radeon_vram_usage = 0;

        radeon_get_drm_value(
            gpu_info->drm_device_node_fd,
            RADEON_INFO_VRAM_USAGE,
            (uint32_t *)&radeon_vram_usage
            );

        unsigned long long free_memory_ = total_memory_ - (unsigned long long)radeon_vram_usage;

        SET_GPUINFO_DYNAMIC(gpu_info_dynamic_info, used_memory, (unsigned long long)radeon_vram_usage);
        SET_GPUINFO_DYNAMIC(gpu_info_dynamic_info, free_memory, free_memory_);

        SET_GPUINFO_DYNAMIC(
            gpu_info_dynamic_info, mem_util_rate,
            gpu_info_dynamic_info->used_memory * 100 / gpu_info_dynamic_info->total_memory
            );

    }


  // Fan speed
  unsigned currentFanSpeed;
  int patternsMatched = _amdgpu_rewindAndReadPattern(gpu_info->fanSpeedFILE, "%u", &currentFanSpeed);
  if (patternsMatched == 1) {
    SET_GPUINFO_DYNAMIC(gpu_info_dynamic_info, fan_speed, currentFanSpeed * 100 / gpu_info->maxFanValue);
  }


    if (
        DRM_VERSION_IS_AT_LEAST(gpu_info->drm_device_driver_version, 2, 42)
        )
    {

        uint32_t radeon_gpu_temp = 0;

        radeon_get_drm_value(gpu_info->drm_device_node_fd, RADEON_INFO_CURRENT_GPU_TEMP, &radeon_gpu_temp);

        if (radeon_gpu_temp != 0)
        {

            radeon_gpu_temp /= 1000;

        }

        SET_GPUINFO_DYNAMIC(gpu_info_dynamic_info, gpu_temp, (unsigned int)radeon_gpu_temp);

    }



    /**
     * XXX:
     * GPU utilization is estimated from a snapshot of GRBM_STATUS bits,
     * counting how many relevant blocks are busy.
     *
     * All bits relevant to the detected GPU family are included.
     *
     * This is a heuristic and does not reflect real load accurately,
     * since it ignores temporal behavior and block weighting.
     *
     * Ideally, this should be implemented using a temporal approach,
     * as done in radeontop.
     */

    uint32_t grbm_status_wanted = 0 | GUI_ACTIVE | SPI_BUSY | SX_BUSY | DB_BUSY | CB_BUSY;


    if (gpu_info->chipset_family >= RADEON_CHIPSET_FAMILY_RV770)
    {

        grbm_status_wanted |= TA_BUSY;

    }


    /* evergreen */
    if (gpu_info->chipset_family >= RADEON_CHIPSET_FAMILY_CEDAR)
    {

        grbm_status_wanted |= EVERGREEN_NI_SI_GRBM_EE_BUSY;

        grbm_status_wanted |= EVERGREEN_NI_SI_RLC_BUSY;
        grbm_status_wanted |= EVERGREEN_NI_VGT_BUSY_NO_DMA;

        grbm_status_wanted |= EVERGREEN_NI_SH_BUSY;

    }

    /* ni */
    if (gpu_info->chipset_family >= RADEON_CHIPSET_FAMILY_CAYMAN)
    {

        grbm_status_wanted |= NI_SI_CIK_GDS_BUSY;

        grbm_status_wanted |= NI_SI_CIK_IA_BUSY_NO_DMA;
        grbm_status_wanted |= NI_SI_CIK_IA_BUSY;

    }

    /* si */
    if (gpu_info->chipset_family >= RADEON_CHIPSET_FAMILY_TAHITI)
    {

        grbm_status_wanted |= SI_CIK_BCI_BUSY;

    }

    /* cik */
    if (gpu_info->chipset_family >= RADEON_CHIPSET_FAMILY_BONAIRE)
    {

        grbm_status_wanted |= CIK_WD_BUSY_NO_DMA;

        grbm_status_wanted |= CIK_WD_BUSY;

    }


    uint32_t grbm_status_wanted_bits = __builtin_popcount(grbm_status_wanted);

    uint32_t grbm_status = 0;

    radeon_get_drm_grbm_value(gpu_info->drm_device_node_fd, &grbm_status);

    grbm_status &= grbm_status_wanted;

    uint32_t grbm_status_bits = __builtin_popcount(grbm_status);

    uint32_t gpu_util_rate_ = (grbm_status_bits * 100) / grbm_status_wanted_bits;


    if (gpu_util_rate_ < gpu_info_dynamic_info->gpu_util_rate)
    {

        float gpu_util_rate__alpha = 0.1f; /* smoothing factor (0.0 - 1.0) */

        gpu_util_rate_ = (uint32_t)(gpu_util_rate__alpha * gpu_util_rate_ + (1.0f - gpu_util_rate__alpha) * gpu_info_dynamic_info->gpu_util_rate);

    }


    SET_GPUINFO_DYNAMIC(gpu_info_dynamic_info, gpu_util_rate, gpu_util_rate_);

}

void gpuinfo_radeon_refresh_running_processes(struct gpu_info *_gpu_info)
{

    _gpu_info->processes_count = 0;

}
