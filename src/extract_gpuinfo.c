/*
 *
 * Copyright (C) 2017 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include "nvtop/extract_gpuinfo.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "nvtop/get_process_info.h"

static bool nvml_initialized = false;

bool init_gpu_info_extraction(void) {
  if (!nvml_initialized) {
    nvmlReturn_t retval = nvmlInit();
    if (retval != NVML_SUCCESS) {
      fprintf(stderr, "Impossible to initialize nvidia nvml : %s\n",
          nvmlErrorString(retval));
      return false;
    }
    nvml_initialized = true;
  }
  return true;
}

bool shutdown_gpu_info_extraction(void) {
  if (nvml_initialized) {
    nvmlReturn_t retval = nvmlShutdown();
    if (retval != NVML_SUCCESS) {
      fprintf(stderr, "Impossible to shutdown nvidia nvml : %s\n",
          nvmlErrorString(retval));
      return false;
    }
    nvml_initialized = false;
  }
  return true;
}

/**
 * Normaly those informations are not changing over time
 */
static void populate_static_device_infos(struct device_info *dev_info) {

  // GPU NAME
  nvmlReturn_t retval = nvmlDeviceGetName(
      dev_info->device_handle,
      dev_info->device_name,
      NVML_DEVICE_NAME_BUFFER_SIZE);
  SET_VALID(device_name_valid, dev_info->valid);
  if (retval != NVML_SUCCESS) {
    memcpy(dev_info->device_name, "UNKNOWN", strlen("UNKNOWN")+1);
    RESET_VALID(device_name_valid, dev_info->valid);
  }

  // PCIe LINK GEN MAX
  retval = nvmlDeviceGetMaxPcieLinkGeneration(
      dev_info->device_handle,
      &dev_info->max_pcie_link_gen
      );
  SET_VALID(max_pcie_link_gen_valid, dev_info->valid);
  if (retval != NVML_SUCCESS) {
    dev_info->max_pcie_link_gen = 0;
    RESET_VALID(max_pcie_link_gen_valid, dev_info->valid);
  }
  // PCIe LINK WIDTH MAX
  retval = nvmlDeviceGetMaxPcieLinkWidth(
      dev_info->device_handle,
      &dev_info->max_pcie_link_width
      );
  SET_VALID(max_pcie_link_width_valid, dev_info->valid);
  if (retval != NVML_SUCCESS) {
    dev_info->max_pcie_link_width = 0;
    RESET_VALID(max_pcie_link_width_valid, dev_info->valid);
  }

  // GPU TEMP SHUTDOWN
  retval = nvmlDeviceGetTemperatureThreshold(
      dev_info->device_handle,
      NVML_TEMPERATURE_THRESHOLD_SHUTDOWN,
      &dev_info->gpu_temp_shutdown
      );
  SET_VALID(gpu_temp_shutdown_valid, dev_info->valid);
  if (retval != NVML_SUCCESS) {
    dev_info->gpu_temp_shutdown = 0;
    RESET_VALID(gpu_temp_shutdown_valid, dev_info->valid);
  }

  // GPU TEMP SLOWDOWN
  retval = nvmlDeviceGetTemperatureThreshold(
      dev_info->device_handle,
      NVML_TEMPERATURE_THRESHOLD_SLOWDOWN,
      &dev_info->gpu_temp_slowdown
      );
  SET_VALID(gpu_temp_slowdown_valid, dev_info->valid);
  if (retval != NVML_SUCCESS) {
    dev_info->gpu_temp_slowdown = 0;
    RESET_VALID(gpu_temp_slowdown_valid, dev_info->valid);
  }

}

static void update_gpu_process_from_process_info(
    unsigned int num_process,
    nvmlProcessInfo_t *p_info,
    struct gpu_process *gpu_proc_info) {

  nvmlReturn_t retval;
  for (unsigned int i = 0; i < num_process; ++i) {
    gpu_proc_info[i].pid = p_info[i].pid;
    retval =
      nvmlSystemGetProcessName(p_info[i].pid, gpu_proc_info[i].process_name, 64);
    if (retval != NVML_SUCCESS) {
      memcpy(gpu_proc_info[i].process_name, "N/A", 4);
    }
    gpu_proc_info[i].used_memory = p_info[i].usedGpuMemory;
    get_username_from_pid(gpu_proc_info[i].pid, 64, gpu_proc_info[i].user_name);
    if (gpu_proc_info[i].user_name[0] == '\0')
      memcpy(gpu_proc_info[i].user_name, "N/A", 4);
  }
}

static void update_graphical_process(struct device_info *dinfo) {
  nvmlReturn_t retval;

  unsigned int array_size;
retry_querry_graphical:
  array_size = dinfo->size_proc_buffers;
  unsigned int prev_array_size = array_size;
  retval = nvmlDeviceGetGraphicsRunningProcesses(dinfo->device_handle, &array_size, dinfo->process_infos);
  if (retval != NVML_SUCCESS) {
    if (retval == NVML_ERROR_INSUFFICIENT_SIZE) {
      unsigned int new_size = prev_array_size * 2 > array_size * 2 ?
        prev_array_size * 2 : array_size * 2;
      dinfo->size_proc_buffers = new_size;
      dinfo->graphic_procs =
        realloc(dinfo->graphic_procs,
            new_size * sizeof(*dinfo->graphic_procs));
      dinfo->compute_procs =
        realloc(dinfo->compute_procs,
            new_size * sizeof(*dinfo->compute_procs));
      dinfo->process_infos =
        realloc(dinfo->process_infos,
            new_size * sizeof(*dinfo->process_infos));
      goto retry_querry_graphical;
    } else {
      dinfo->num_graphical_procs = 0;
    }
  } else {
    dinfo->num_graphical_procs = array_size;
  }
  update_gpu_process_from_process_info(
      dinfo->num_graphical_procs,
      dinfo->process_infos,
      dinfo->graphic_procs);
}

static void update_compute_process(struct device_info *dinfo) {
  nvmlReturn_t retval;

  unsigned int array_size;
retry_querry_compute:
  array_size = dinfo->size_proc_buffers;
  unsigned int prev_array_size = array_size;
  retval = nvmlDeviceGetComputeRunningProcesses(dinfo->device_handle, &array_size, dinfo->process_infos);
  if (retval != NVML_SUCCESS) {
    if (retval == NVML_ERROR_INSUFFICIENT_SIZE) {
      unsigned int new_size = prev_array_size * 2 > array_size * 2 ?
        prev_array_size * 2 : array_size * 2;
      dinfo->size_proc_buffers = new_size;
      dinfo->graphic_procs =
        realloc(dinfo->graphic_procs,
            new_size * sizeof(*dinfo->graphic_procs));
      dinfo->compute_procs =
        realloc(dinfo->compute_procs,
            new_size * sizeof(*dinfo->compute_procs));
      dinfo->process_infos =
        realloc(dinfo->process_infos,
            new_size * sizeof(*dinfo->process_infos));
      goto retry_querry_compute;
    } else {
      dinfo->num_compute_procs = 0;
    }
  } else {
    dinfo->num_compute_procs = array_size;
  }
  update_gpu_process_from_process_info(
      dinfo->num_compute_procs,
      dinfo->process_infos,
      dinfo->compute_procs);
}

void update_device_infos(
    unsigned int num_devs,
    struct device_info *dev_info) {
  for (unsigned int i = 0; i < num_devs; ++i) {

    // GPU CLK
    nvmlReturn_t retval = nvmlDeviceGetClockInfo(
        dev_info[i].device_handle,
        NVML_CLOCK_GRAPHICS,
        &dev_info[i].gpu_clock_speed);
    SET_VALID(gpu_clock_speed_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].gpu_clock_speed = 0;
      RESET_VALID(gpu_clock_speed_valid, dev_info->valid);
    }

    // MEM CLK
    retval = nvmlDeviceGetClockInfo(
        dev_info[i].device_handle,
        NVML_CLOCK_MEM,
        &dev_info[i].mem_clock_speed);
    SET_VALID(mem_clock_speed_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].mem_clock_speed = 0;
      RESET_VALID(mem_clock_speed_valid, dev_info->valid);
    }

    // GPU CLK MAX
    retval = nvmlDeviceGetMaxClockInfo(
        dev_info[i].device_handle,
        NVML_CLOCK_GRAPHICS,
        &dev_info[i].gpu_clock_speed_max);
    SET_VALID(gpu_clock_speed_max_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].gpu_clock_speed_max = 0;
      RESET_VALID(gpu_clock_speed_max_valid, dev_info->valid);
    }

    // MEM CLK MAX
    retval = nvmlDeviceGetMaxClockInfo(
        dev_info[i].device_handle,
        NVML_CLOCK_MEM,
        &dev_info[i].mem_clock_speed_max);
    SET_VALID(mem_clock_speed_max_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].mem_clock_speed_max = 0;
      RESET_VALID(mem_clock_speed_max_valid, dev_info->valid);
    }

    // GPU / MEM UTIL RATE
    nvmlUtilization_t util_rate;
    retval = nvmlDeviceGetUtilizationRates(
        dev_info[i].device_handle,
        &util_rate);
    if (retval != NVML_SUCCESS) {
      dev_info[i].gpu_util_rate = 0;
      dev_info[i].mem_util_rate = 0;
      RESET_VALID(gpu_util_rate_valid, dev_info->valid);
      RESET_VALID(mem_util_rate_valid, dev_info->valid);
    } else {
      dev_info[i].gpu_util_rate = util_rate.gpu;
      dev_info[i].mem_util_rate = util_rate.memory;
      SET_VALID(gpu_util_rate_valid, dev_info->valid);
      SET_VALID(mem_util_rate_valid, dev_info->valid);
    }

    // FREE / TOTAL / USED MEMORY
    nvmlMemory_t meminfo;
    retval = nvmlDeviceGetMemoryInfo(
        dev_info[i].device_handle,
        &meminfo);
    if (retval != NVML_SUCCESS) {
      dev_info[i].free_memory = 0;
      dev_info[i].total_memory = 0;
      dev_info[i].used_memory = 0;
      RESET_VALID(free_memory_valid, dev_info->valid);
      RESET_VALID(total_memory_valid, dev_info->valid);
      RESET_VALID(used_memory_valid, dev_info->valid);
    } else {
      dev_info[i].free_memory = meminfo.free;
      dev_info[i].total_memory = meminfo.total;
      dev_info[i].used_memory = meminfo.used;
      SET_VALID(free_memory_valid, dev_info->valid);
      SET_VALID(total_memory_valid, dev_info->valid);
      SET_VALID(used_memory_valid, dev_info->valid);
    }

    // PCIe LINK GEN
    retval = nvmlDeviceGetCurrPcieLinkGeneration(
        dev_info[i].device_handle,
        &dev_info[i].cur_pcie_link_gen);
    SET_VALID(cur_pcie_link_gen_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].cur_pcie_link_gen = 0;
      RESET_VALID(cur_pcie_link_gen_valid, dev_info->valid);
    }

    // PCIe LINK WIDTH
    retval = nvmlDeviceGetCurrPcieLinkWidth(
        dev_info[i].device_handle,
        &dev_info[i].cur_pcie_link_width);
    SET_VALID(cur_pcie_link_width_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].cur_pcie_link_width = 0;
      RESET_VALID(cur_pcie_link_width_valid, dev_info->valid);
    }

    // PCIe TX THROUGHPUT
    retval = nvmlDeviceGetPcieThroughput(
        dev_info[i].device_handle,
        NVML_PCIE_UTIL_TX_BYTES,
        &dev_info[i].pcie_tx);
    SET_VALID(pcie_tx_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].pcie_tx = 0;
      RESET_VALID(pcie_tx_valid, dev_info->valid);
    }

    // PCIe RX THROUGHPUT
    retval = nvmlDeviceGetPcieThroughput(
        dev_info[i].device_handle,
        NVML_PCIE_UTIL_RX_BYTES,
        &dev_info[i].pcie_rx);
    SET_VALID(pcie_rx_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].pcie_rx = 0;
      RESET_VALID(pcie_rx_valid, dev_info->valid);
    }

    // FAN SPEED
    retval = nvmlDeviceGetFanSpeed(
        dev_info[i].device_handle,
        &dev_info[i].fan_speed);
    SET_VALID(fan_speed_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].fan_speed = 0;
      RESET_VALID(fan_speed_valid, dev_info->valid);
    }

    // GPU TEMP
    retval = nvmlDeviceGetTemperature(
        dev_info[i].device_handle,
        NVML_TEMPERATURE_GPU,
        &dev_info[i].gpu_temp);
    SET_VALID(gpu_temp_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].gpu_temp = 0;
      RESET_VALID(gpu_temp_valid, dev_info->valid);
    }

    // POWER DRAW
    retval = nvmlDeviceGetPowerUsage(
        dev_info[i].device_handle,
        &dev_info[i].power_draw);
    SET_VALID(power_draw_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].power_draw = 0;
      RESET_VALID(power_draw_valid, dev_info->valid);
    }

    // POWER MAX
    retval = nvmlDeviceGetEnforcedPowerLimit(
        dev_info[i].device_handle,
        &dev_info[i].power_draw_max);
    SET_VALID(power_draw_max_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].power_draw_max = 0;
      RESET_VALID(power_draw_max_valid, dev_info->valid);
    }

    // Encoder infos
    retval = nvmlDeviceGetEncoderUtilization(
        dev_info[i].device_handle,
        &dev_info[i].encoder_rate,
        &dev_info[i].encoder_sampling);
    SET_VALID(encoder_rate_valid, dev_info->valid);
    SET_VALID(encoder_sampling_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].encoder_rate = 0;
      dev_info[i].encoder_sampling = 0;
      RESET_VALID(encoder_rate_valid, dev_info->valid);
      RESET_VALID(encoder_sampling_valid, dev_info->valid);
    }

    // Decoder infos
    retval = nvmlDeviceGetDecoderUtilization(
        dev_info[i].device_handle,
        &dev_info[i].decoder_rate,
        &dev_info[i].decoder_sampling);
    SET_VALID(decoder_rate_valid, dev_info->valid);
    SET_VALID(decoder_sampling_valid, dev_info->valid);
    if (retval != NVML_SUCCESS) {
      dev_info[i].decoder_rate = 0;
      dev_info[i].decoder_sampling = 0;
      RESET_VALID(decoder_rate_valid, dev_info->valid);
      RESET_VALID(decoder_sampling_valid, dev_info->valid);
    }

    // Process informations
    update_graphical_process(&dev_info[i]);
    update_compute_process(&dev_info[i]);

  } // Loop over devices
}

unsigned int initialize_device_info(struct device_info **dev_info) {
  unsigned int num_devices;
  nvmlReturn_t retval = nvmlDeviceGetCount(&num_devices);
  if (retval != NVML_SUCCESS) {
    fprintf(stderr, "Impossible to get the number of devices : %s\n",
        nvmlErrorString(retval));
    return 0;
  }
  *dev_info = malloc(num_devices * sizeof(**dev_info));
  struct device_info *devs = *dev_info;
  unsigned int num_queriable = 0;
  for (unsigned int i = 0; i < num_devices; ++i) {
    retval =
      nvmlDeviceGetHandleByIndex(i, &devs[num_queriable].device_handle);
    if (retval != NVML_SUCCESS) {
      if (retval == NVML_ERROR_NO_PERMISSION) {
        continue;
      } else {
        fprintf(stderr,
            "Impossible to get handle for device number %u : %s\n",
            i, nvmlErrorString(retval));
        free(*dev_info);
        return 0;
      }
    } else {
      populate_static_device_infos(&devs[num_queriable]);
#define DEF_PROC_NUM 25
      devs[num_queriable].size_proc_buffers = DEF_PROC_NUM;
      devs[num_queriable].compute_procs =
        malloc(DEF_PROC_NUM * sizeof(*devs[num_queriable].compute_procs));
      devs[num_queriable].graphic_procs =
        malloc(DEF_PROC_NUM * sizeof(*devs[num_queriable].graphic_procs));
      devs[num_queriable].process_infos =
        malloc(DEF_PROC_NUM * sizeof(*devs[num_queriable].process_infos));
#undef DEF_PROC_NUM
      num_queriable += 1;
    }
  }
  return num_queriable;
}

void clean_device_info(unsigned int num_devs, struct device_info *dev_info) {
  for (unsigned int i = 0; i < num_devs; ++i) {
    free(dev_info[i].graphic_procs);
    free(dev_info[i].compute_procs);
    free(dev_info[i].process_infos);
  }
  free(dev_info);
}
