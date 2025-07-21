/*
 *
 * Copyright (c) 2025 MetaX Integrated Circuits (Shanghai) Co., Ltd. All rights reserved.
 *
 * This file is part of Nvtop and adapted from mxsml from MetaX Integrated Circuits (Shanghai) Co., Ltd.
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
#include "nvtop/extract_gpuinfo_common.h"

#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MXSML_SUCCESS 0
#define DEVICE_BDF_ID_SIZE 32           //!< Guaranteed maximum possible size for BDF ID
#define DEVICE_UUID_SIZE 96             //!< Guaranteed maximum possible size for UUID
#define DEVICE_NAME_SIZE 32             //!< Guaranteed maximum possible size for Device name
#define PROCESS_NAME_SIZE 64            //!< Maximum length showed process name
#define MAX_GPU_NUM_USED_BY_PROCESS 64  //!< Maximum stored GPU information used by a process

typedef unsigned int mxSmlReturn_t; // store the enum as unsigned int

// Init

static mxSmlReturn_t (*mxSmlInit)(void);

// Static information and helper functions

static mxSmlReturn_t(*mxSmlGetDeviceCount)(unsigned int *deviceCount);

static const char *(*mxSmlGetErrorString)(mxSmlReturn_t);

typedef enum {
  MXSML_Device_Unknown = 0
} mxSmlDeviceType_t;

typedef enum {
  MXSML_Brand_Unknown = 0,
  MXSML_Brand_N,
  MXSML_Brand_C,
  MXSML_Brand_G
} mxSmlDeviceBrand_t;

typedef enum {
  MXSML_Virtualization_Mode_None = 0,   //!< Represents bare metal
  MXSML_Virtualization_Mode_Pf,         //!< Physical function after virtualization
  MXSML_Virtualization_Mode_Vf          //!< Virtualized device
} mxSmlDeviceVirtualizationMode_t;

typedef struct {
  unsigned int deviceId;
  mxSmlDeviceType_t type;           //!< Deprecated. Do not use.
  char bdfId[DEVICE_BDF_ID_SIZE];
  unsigned int gpuId;
  unsigned int nodeId;
  char uuid[DEVICE_UUID_SIZE];
  mxSmlDeviceBrand_t brand;
  mxSmlDeviceVirtualizationMode_t mode;
  char deviceName[DEVICE_NAME_SIZE];
} mxSmlDeviceInfo_t;

static mxSmlReturn_t (*mxSmlGetDeviceInfo)(unsigned int deviceId, mxSmlDeviceInfo_t *deviceInfo);

static mxSmlReturn_t (*mxSmlGetLimitedDeviceInfo)(unsigned int deviceId, mxSmlDeviceInfo_t *deviceInfo);

typedef struct {
  float speed;
  unsigned int width;
} mxSmlPcieInfo_t;

static mxSmlReturn_t (*mxSmlGetPcieInfo)(unsigned int deviceId, mxSmlPcieInfo_t *pcieInfo);

static mxSmlReturn_t (*mxSmlGetPcieMaxLinkInfo)(unsigned int deviceId, mxSmlPcieInfo_t *_pcieInfo);

typedef enum {
  MXSML_Temperature_Hotspot = 0,
  MXSML_Temperature_HotLimit,
} mxSmlTemperatureSensors_t;

static mxSmlReturn_t (*mxSmlGetTemperatureInfo)(unsigned int deviceId, mxSmlTemperatureSensors_t sensorType,
                                               int *temp);

// Dynamic infomation extraction
typedef enum {
  MXSML_Clock_Dla = 1,            //!< Valid for N-class device
  MXSML_Clock_Mc = 2,             //!< Valid for N-class device
  MXSML_Clock_Mc0 = 3,            //!< Valid for C-class device
  MXSML_Clock_Xcore = 11          //!< Valid for C-class device
} mxSmlClockIp_t;

static mxSmlReturn_t (*mxSmlGetClocks)(unsigned int deviceId, mxSmlClockIp_t type,
                                      unsigned int *clocksSize, unsigned int *clock);

typedef enum {
  MXSML_Dpm_Dla = 0,              //!< Valid for N-class device
  MXSML_Dpm_Xcore,                //!< Valid for C-class device
  MXSML_Dpm_Mc,
} mxSmlDpmIp_t;

static mxSmlReturn_t (*mxSmlGetDpmIpClockInfo)(unsigned int deviceId, mxSmlDpmIp_t dpmIp,
                                              unsigned int *clockInfo, unsigned int *size);

typedef enum {
  MXSML_Usage_Dla,                //!< Valid for N-class device
  MXSML_Usage_Vpue,
  MXSML_Usage_Vpud,
  MXSML_Usage_Xcore = 4,          //!< Valid for C-class device
} mxSmlUsageIp_t;

static mxSmlReturn_t (*mxSmlGetDeviceIpUsage)(unsigned int deviceId, mxSmlUsageIp_t ip, int *usage);

typedef struct {
  long visVramTotal;
  long visVramUse;
  long vramTotal;
  long vramUse;
  long xttTotal;
  long xttUse;
} mxSmlMemoryInfo_t;

static mxSmlReturn_t (*mxSmlGetMemoryInfo)(unsigned int deviceId, mxSmlMemoryInfo_t *memory);

typedef struct {
  int rx;
  int tx;
} mxSmlPcieThroughput_t;

static mxSmlReturn_t (*mxSmlGetPcieThroughput)(unsigned int deviceId, mxSmlPcieThroughput_t *pcieInfo);

static mxSmlReturn_t (*mxSmlGetFanSpeedInfo)(unsigned int deviceId, unsigned int *rpm, unsigned int *pwm);

typedef struct {
  unsigned int voltage;
  unsigned int current;
  unsigned int power;
} mxSmlBoardWayElectricInfo_t;

static mxSmlReturn_t (*mxSmlGetBoardPowerInfo)(unsigned int deviceId, unsigned int *infoSize,
                                              mxSmlBoardWayElectricInfo_t* boardInfo);

static mxSmlReturn_t (*mxSmlGetBoardPowerLimit)(unsigned int deviceId, unsigned int *limit);

// Processes running on GPU

static mxSmlReturn_t (*mxSmlGetNumberOfProcess)(unsigned int *processNumber);

typedef struct {
  char bdfId[DEVICE_BDF_ID_SIZE];
  unsigned int gpuId;
  unsigned long gpuMemoryUsage;
} mxSmlProcessGpuInfo_t;

typedef struct {
  unsigned int processId;
  char processName[PROCESS_NAME_SIZE];
  unsigned int gpuNumber;
  mxSmlProcessGpuInfo_t processGpuInfo[MAX_GPU_NUM_USED_BY_PROCESS];
} mxSmlProcessInfo_t;

static mxSmlReturn_t (*mxSmlGetSingleGpuProcess)(unsigned int deviceId, unsigned int *processNumber,
                                                mxSmlProcessInfo_t* processInfo);

static void *libmxsml_handle;

static mxSmlReturn_t last_mxsml_return_status = MXSML_SUCCESS;
static char didnt_call_gpuinfo_init[] = "The METAX extraction has not been initialized, please call "
                                        "gpuinfo_metax_init\n";
static const char *local_error_string = didnt_call_gpuinfo_init;

struct gpu_info_metax {
  struct gpu_info base;
  struct list_head allocate_list;

  mxSmlDeviceInfo_t device;
};

static LIST_HEAD(allocations);

static bool gpuinfo_metax_init(void);
static void gpuinfo_metax_shutdown(void);
static const char *gpuinfo_metax_last_error_string(void);
static bool gpuinfo_metax_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_metax_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_metax_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_metax_get_running_processes(struct gpu_info *_gpu_info);

struct gpu_vendor gpu_vendor_metax = {
  .init = gpuinfo_metax_init,
  .shutdown = gpuinfo_metax_shutdown,
  .last_error_string = gpuinfo_metax_last_error_string,
  .get_device_handles = gpuinfo_metax_get_device_handles,
  .populate_static_info = gpuinfo_metax_populate_static_info,
  .refresh_dynamic_info = gpuinfo_metax_refresh_dynamic_info,
  .refresh_running_processes = gpuinfo_metax_get_running_processes,
  .name = "METAX",
};

__attribute__((constructor)) static void init_extract_gpuinfo_metax(void) { register_gpu_vendor(&gpu_vendor_metax); }

/*
 *
 * This function loads the libmxsml.so shared object, initializes the
 * required function pointers and calls the mxsml library initialization
 * function. If false is returned, the cause of the error can be retrieved
 * by calling the function gpuinfo_metax_last_error_string.
 *
 */
static bool gpuinfo_metax_init(void) {
  printf("gpuinfo_metax_init\n");
  libmxsml_handle = dlopen("/opt/mxdriver/lib/libmxsml.so", RTLD_LAZY);
  if (!libmxsml_handle)
    libmxsml_handle = dlopen("/opt/maca/lib/libmxsml.so", RTLD_LAZY);
  if (!libmxsml_handle)
    libmxsml_handle = dlopen("/opt/mxn100/lib/libmxsml.so", RTLD_LAZY);
  if (!libmxsml_handle) {
    local_error_string = dlerror();
    return false;
  }

  mxSmlInit = dlsym(libmxsml_handle, "mxSmlInit");
  if (!mxSmlInit)
    goto init_error_clean_exit;

  mxSmlGetDeviceCount = dlsym(libmxsml_handle, "mxSmlGetDeviceCount");
  if (!mxSmlGetDeviceCount)
    goto init_error_clean_exit;

  mxSmlGetErrorString = dlsym(libmxsml_handle, "mxSmlGetErrorString");
  if (!mxSmlGetErrorString)
    goto init_error_clean_exit;

  mxSmlGetDeviceInfo = dlsym(libmxsml_handle, "mxSmlGetDeviceInfo");
  if (!mxSmlGetDeviceInfo)
    goto init_error_clean_exit;

  mxSmlGetLimitedDeviceInfo = dlsym(libmxsml_handle, "mxSmlGetLimitedDeviceInfo");
  if (!mxSmlGetLimitedDeviceInfo)
    goto init_error_clean_exit;

  mxSmlGetPcieInfo = dlsym(libmxsml_handle, "mxSmlGetPcieInfo");
  if (!mxSmlGetPcieInfo)
    goto init_error_clean_exit;

  mxSmlGetPcieMaxLinkInfo = dlsym(libmxsml_handle, "mxSmlGetPcieMaxLinkInfo");
  if (!mxSmlGetPcieMaxLinkInfo)
    goto init_error_clean_exit;

  mxSmlGetTemperatureInfo = dlsym(libmxsml_handle, "mxSmlGetTemperatureInfo");
  if (!mxSmlGetTemperatureInfo)
    goto init_error_clean_exit;

  mxSmlGetClocks = dlsym(libmxsml_handle, "mxSmlGetClocks");
  if (!mxSmlGetClocks)
    goto init_error_clean_exit;

  mxSmlGetDpmIpClockInfo = dlsym(libmxsml_handle, "mxSmlGetDpmIpClockInfo");
  if (!mxSmlGetDpmIpClockInfo)
    goto init_error_clean_exit;

  mxSmlGetDeviceIpUsage = dlsym(libmxsml_handle, "mxSmlGetDeviceIpUsage");
  if (!mxSmlGetDeviceIpUsage)
    goto init_error_clean_exit;

  mxSmlGetMemoryInfo = dlsym(libmxsml_handle, "mxSmlGetMemoryInfo");
  if (!mxSmlGetMemoryInfo)
    goto init_error_clean_exit;

  mxSmlGetPcieThroughput = dlsym(libmxsml_handle, "mxSmlGetPcieThroughput");
  if (!mxSmlGetPcieThroughput)
    goto init_error_clean_exit;

  // for compatibility
  mxSmlGetFanSpeedInfo = dlsym(libmxsml_handle, "mxSmlGetFanSpeedInfo");

  mxSmlGetBoardPowerInfo = dlsym(libmxsml_handle, "mxSmlGetBoardPowerInfo");
  if (!mxSmlGetBoardPowerInfo)
    goto init_error_clean_exit;

  // for compatibility
  mxSmlGetBoardPowerLimit = dlsym(libmxsml_handle, "mxSmlGetBoardPowerLimit");

  mxSmlGetNumberOfProcess = dlsym(libmxsml_handle, "mxSmlGetNumberOfProcess");
  if (!mxSmlGetNumberOfProcess)
    goto init_error_clean_exit;

  mxSmlGetSingleGpuProcess = dlsym(libmxsml_handle, "mxSmlGetSingleGpuProcess");
  if (!mxSmlGetSingleGpuProcess)
    goto init_error_clean_exit;

  last_mxsml_return_status = mxSmlInit();
  if (last_mxsml_return_status != MXSML_SUCCESS)
    return false;

  local_error_string = NULL;
  return true;

init_error_clean_exit:
  dlclose(libmxsml_handle);
  libmxsml_handle = NULL;
  return false;
}

static void gpuinfo_metax_shutdown(void) {
  if (libmxsml_handle) {
    dlclose(libmxsml_handle);
    libmxsml_handle = NULL;
    local_error_string = didnt_call_gpuinfo_init;
  }

  struct gpu_info_metax *allocated, *tmp;

  list_for_each_entry_safe(allocated, tmp, &allocations, allocate_list) {
    list_del(&allocated->allocate_list);
    free(allocated);
  }
}

static const char *gpuinfo_metax_last_error_string(void) {
  if (local_error_string) {
    return local_error_string;
  } else if (libmxsml_handle && mxSmlGetErrorString) {
    return mxSmlGetErrorString(last_mxsml_return_status);
  } else {
    return "An unanticipated error occurred while accessing METAX GPU "
           "information\n";
  }
}

static bool gpuinfo_metax_get_device_handles(struct list_head *devices, unsigned int *count) {
  printf("gpuinfo_metax_get_device_handles...\n");
  if (!libmxsml_handle)
    return false;

  unsigned int num_devices;
  mxSmlGetDeviceCount(&num_devices);
  printf("device count: %u\n", num_devices);
  struct gpu_info_metax *gpu_infos = calloc(num_devices, sizeof(*gpu_infos));
  if (!gpu_infos) {
    local_error_string = strerror(errno);
    return false;
  }

  list_add(&gpu_infos[0].allocate_list, &allocations);

  *count = 0;
  for (unsigned int i = 0; i < num_devices; ++i) {
    mxSmlDeviceInfo_t deviceInfo;
    mxSmlReturn_t deviceInfoRet = mxSmlGetDeviceInfo(i, &deviceInfo);
    if (deviceInfoRet != MXSML_SUCCESS) {
      deviceInfoRet != mxSmlGetLimitedDeviceInfo(i, &deviceInfo);
      // Limited device
      if (deviceInfoRet == MXSML_SUCCESS) {
        gpu_infos[*count].device = deviceInfo;
        gpu_infos[*count].base.vendor = &gpu_vendor_metax;
        strncpy(gpu_infos[*count].base.pdev, deviceInfo.bdfId, PDEV_LEN);
        list_add_tail(&gpu_infos[*count].base.list, devices);
        *count += 1;
      }
    } else {
      gpu_infos[*count].device = deviceInfo;
      gpu_infos[*count].base.vendor = &gpu_vendor_metax;
      strncpy(gpu_infos[*count].base.pdev, deviceInfo.bdfId, PDEV_LEN);
      list_add_tail(&gpu_infos[*count].base.list, devices);
      *count += 1;
    }
  }

  return true;
}

static void gpuinfo_metax_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_metax *gpu_info = container_of(_gpu_info, struct gpu_info_metax, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
  unsigned int deviceId = gpu_info->device.deviceId;

  static_info->integrated_graphics = false;
  static_info->encode_decode_shared = false;
  RESET_ALL(static_info->valid);

  strncpy(static_info->device_name, gpu_info->device.deviceName, MAX_DEVICE_NAME);
  SET_VALID(gpuinfo_device_name_valid, static_info->valid);

  mxSmlPcieInfo_t maxPcieInfo;
  last_mxsml_return_status = mxSmlGetPcieMaxLinkInfo(deviceId, &maxPcieInfo);
  if (last_mxsml_return_status == MXSML_SUCCESS) {
    static_info->max_pcie_gen = maxPcieInfo.speed;
    static_info->max_pcie_link_width = maxPcieInfo.width;
    SET_VALID(gpuinfo_max_pcie_gen_valid, static_info->valid);
    SET_VALID(gpuinfo_max_pcie_link_width_valid, static_info->valid);
  }

  last_mxsml_return_status = mxSmlGetTemperatureInfo(deviceId, MXSML_Temperature_HotLimit,
                                                    &static_info->temperature_shutdown_threshold);
  if (last_mxsml_return_status == MXSML_SUCCESS) {
    static_info->temperature_slowdown_threshold = 9500;
    SET_VALID(gpuinfo_temperature_shutdown_threshold_valid, static_info->valid);
    SET_VALID(gpuinfo_temperature_slowdown_threshold_valid, static_info->valid);
  }
}

static void gpuinfo_metax_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_metax *gpu_info = container_of(_gpu_info, struct gpu_info_metax, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  unsigned int deviceId = gpu_info->device.deviceId;

  RESET_ALL(dynamic_info->valid);

  // GPU current speed
  unsigned int clockSize = 2;
  unsigned int gpuClockFreqs[clockSize];
  mxSmlClockIp_t clockFrom = MXSML_Clock_Xcore;
  if (gpu_info->device.brand == MXSML_Brand_N)
    clockFrom = MXSML_Clock_Dla;

  last_mxsml_return_status = mxSmlGetClocks(deviceId, clockFrom, &clockSize, gpuClockFreqs);
  if (last_mxsml_return_status == MXSML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, gpuClockFreqs[0]);

  // GPU max speed
  clockSize = 12;
  unsigned int dpmGpuClockInfo[clockSize];
  mxSmlDpmIp_t dpmFrom = MXSML_Dpm_Xcore;
  if (gpu_info->device.brand == MXSML_Brand_N)
    dpmFrom = MXSML_Dpm_Dla;

  last_mxsml_return_status = mxSmlGetDpmIpClockInfo(deviceId, dpmFrom, dpmGpuClockInfo, &clockSize);
  if (last_mxsml_return_status == MXSML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, dpmGpuClockInfo[clockSize-1]);

  // Memory current speed
  clockSize = 3;
  unsigned int memClockFreqs[clockSize];
  clockFrom = MXSML_Clock_Mc0;
  if (gpu_info->device.brand == MXSML_Brand_N)
    clockFrom = MXSML_Clock_Mc;

  last_mxsml_return_status = mxSmlGetClocks(deviceId, clockFrom, &clockSize, memClockFreqs);
  if(last_mxsml_return_status == MXSML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_clock_speed, memClockFreqs[0]);

  // Memory max speed
  clockSize = 12;
  unsigned int dpmMemClockInfo[clockSize];
  last_mxsml_return_status = mxSmlGetDpmIpClockInfo(deviceId, MXSML_Dpm_Mc, dpmMemClockInfo, &clockSize);
  if (last_mxsml_return_status == MXSML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_clock_speed_max, dpmMemClockInfo[clockSize-1]);

  // GPU utilization rates
  mxSmlUsageIp_t usageIp = MXSML_Usage_Xcore;
  if (gpu_info->device.brand == MXSML_Brand_N)
    usageIp = MXSML_Usage_Dla;

  last_mxsml_return_status = mxSmlGetDeviceIpUsage(deviceId, usageIp, &dynamic_info->gpu_util_rate);
  if  (last_mxsml_return_status == MXSML_SUCCESS)
    SET_VALID(gpuinfo_gpu_util_rate_valid, dynamic_info->valid);

  // Encoder utilization rate
  usageIp = MXSML_Usage_Vpue;
  last_mxsml_return_status = mxSmlGetDeviceIpUsage(deviceId, usageIp, &dynamic_info->encoder_rate);
  if (last_mxsml_return_status == MXSML_SUCCESS)
    SET_VALID(gpuinfo_encoder_rate_valid, dynamic_info->valid);

  // Decoder utilization rate
  usageIp = MXSML_Usage_Vpud;
  last_mxsml_return_status = mxSmlGetDeviceIpUsage(deviceId, usageIp, &dynamic_info->decoder_rate);
  if (last_mxsml_return_status == MXSML_SUCCESS)
    SET_VALID(gpuinfo_decoder_rate_valid, dynamic_info->valid);

  // Device memory vis_vram info (total,used,free)
  mxSmlMemoryInfo_t memory_info;
  last_mxsml_return_status = mxSmlGetMemoryInfo(deviceId, &memory_info);
  if (last_mxsml_return_status == MXSML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, memory_info.visVramTotal * 1024);
    SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, memory_info.visVramUse * 1024);
    SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, (memory_info.visVramTotal - memory_info.visVramUse) * 1024);
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, memory_info.visVramUse * 100 / memory_info.visVramTotal);
  }

  // Pcie generation and width used by the device
  mxSmlPcieInfo_t pcieInfo;
  last_mxsml_return_status = mxSmlGetPcieInfo(deviceId, &pcieInfo);
  if (last_mxsml_return_status == MXSML_SUCCESS) {
    dynamic_info->pcie_link_width = pcieInfo.width;
    SET_VALID(gpuinfo_pcie_link_width_valid, dynamic_info->valid);

    float pcieRates[] = {2.5, 5, 8, 16, 32};
    for (unsigned int i = 0; i < 5; i++) {
      if (pcieRates[i] == pcieInfo.speed) {
        dynamic_info->pcie_link_gen = i + 1;
        SET_VALID(gpuinfo_pcie_link_gen_valid, dynamic_info->valid);
      }
    }
  }

  // Pcie eception and transmission throughput
  mxSmlPcieThroughput_t pcieThroughput;
  last_mxsml_return_status = mxSmlGetPcieThroughput(deviceId, &pcieThroughput);
  if (last_mxsml_return_status == MXSML_SUCCESS) {
    dynamic_info->pcie_rx = pcieThroughput.rx * 1000;
    dynamic_info->pcie_tx = pcieThroughput.tx * 1000;
    SET_VALID(gpuinfo_pcie_rx_valid, dynamic_info->valid);
    SET_VALID(gpuinfo_pcie_tx_valid, dynamic_info->valid);
  }

  // Fan speed
  if (mxSmlGetFanSpeedInfo) {
    unsigned int fanRpm;
    last_mxsml_return_status = mxSmlGetFanSpeedInfo(deviceId, &fanRpm, &dynamic_info->fan_speed);
    if (last_mxsml_return_status == MXSML_SUCCESS)
      SET_VALID(gpuinfo_fan_speed_valid, dynamic_info->valid);
  }

  // GPU temperature
  int gpuTemp;
  last_mxsml_return_status = mxSmlGetTemperatureInfo(deviceId, MXSML_Temperature_Hotspot, &gpuTemp);
  if (last_mxsml_return_status == MXSML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, gpuTemp / 100);

  // Device power usage
  unsigned int infoSize = 3;
  mxSmlBoardWayElectricInfo_t boardPower[infoSize];
  last_mxsml_return_status = mxSmlGetBoardPowerInfo(deviceId, &infoSize, boardPower);
  if (last_mxsml_return_status == MXSML_SUCCESS) {
    dynamic_info->power_draw = 0;
    for (unsigned int i = 0; i < infoSize; ++i)
      dynamic_info->power_draw += boardPower[i].power;
    SET_VALID(gpuinfo_power_draw_valid, dynamic_info->valid);
  }

  // Maximum enforced power usage
  if (mxSmlGetBoardPowerLimit) {
    last_mxsml_return_status = mxSmlGetBoardPowerLimit(deviceId, &dynamic_info->power_draw_max);
    if (last_mxsml_return_status == MXSML_SUCCESS)
      SET_VALID(gpuinfo_power_draw_max_valid, dynamic_info->valid);
  } else {
    if (gpu_info->device.brand == MXSML_Brand_N)
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, (unsigned int)70000);
    else if (gpu_info->device.brand == MXSML_Brand_C)
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, (unsigned int)350000);
  }
}

static void gpuinfo_metax_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_metax *gpu_info = container_of(_gpu_info, struct gpu_info_metax, base);
  unsigned int deviceId = gpu_info->device.deviceId;

  _gpu_info->processes_count = 0;
  unsigned int processNum = 0;
  last_mxsml_return_status = mxSmlGetNumberOfProcess(&processNum);
  if (last_mxsml_return_status != MXSML_SUCCESS) {
    perror("Could not get processes number: ");
    exit(EXIT_FAILURE);
  }

  mxSmlProcessInfo_t processInfo[processNum];
  last_mxsml_return_status = mxSmlGetSingleGpuProcess(deviceId, &processNum, processInfo);
  if (last_mxsml_return_status != MXSML_SUCCESS) {
    perror("Could not get process info: ");
    exit(EXIT_FAILURE);
  }

  _gpu_info->processes_count = processNum;
  if (_gpu_info->processes_count > 0) {
    if (_gpu_info->processes_count > _gpu_info->processes_array_size) {
      _gpu_info->processes_array_size = _gpu_info->processes_count + COMMON_PROCESS_LINEAR_REALLOC_INC;
      _gpu_info->processes =
        reallocarray(_gpu_info->processes, _gpu_info->processes_array_size, sizeof(*_gpu_info->processes));
      if (!_gpu_info->processes) {
        perror("Could not get processes info: ");
        exit(EXIT_FAILURE);
      }
    }
    memset(_gpu_info->processes, 0, _gpu_info->processes_count * sizeof(*_gpu_info->processes));
    for (unsigned int i = 0; i < processNum; ++i) {
      _gpu_info->processes[i].type = gpu_process_compute;
      _gpu_info->processes[i].pid = processInfo[i].processId;
      _gpu_info->processes[i].gpu_memory_usage = 0;
      for (unsigned int j = 0; j < processInfo[i].gpuNumber; ++j)
        _gpu_info->processes[i].gpu_memory_usage += processInfo[i].processGpuInfo[j].gpuMemoryUsage;
      SET_VALID(gpuinfo_process_gpu_memory_usage_valid, _gpu_info->processes[i].valid);
    }
  }
}
