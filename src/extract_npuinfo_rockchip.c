/*
 *
 * Copyright (C) 2025 YuLong Yao <feilongphone@gmail.com>
 *
 * This file is part of Nvtop and adapted from igt-gpu-tools from Intel Corporation.
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
#include "nvtop/time.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

struct gpu_info_rknpu {
  struct gpu_info base;
};

static struct gpu_info_rknpu *rknpu_info = NULL;

static bool gpuinfo_rknpu_init(void) {
  if (access("/sys/kernel/debug/rknpu/load", R_OK) != 0) {
    return false;
  }
  return true;
}

static void gpuinfo_rknpu_shutdown(void) {
  if (rknpu_info) {
    free(rknpu_info);
    rknpu_info = NULL;
  }
}

static const char *gpuinfo_rknpu_last_error_string(void) {
  return "RK-NPU error";
}

static void add_rknpu_chip(struct list_head *devices, unsigned *count) {
  extern struct gpu_vendor gpu_vendor_rknpu;
  struct gpu_info_rknpu *this_npu = &rknpu_info[*count];
  this_npu->base.vendor = &gpu_vendor_rknpu;
  snprintf(this_npu->base.pdev, PDEV_LEN, "RK-NPU%d", *count);
  list_add_tail(&this_npu->base.list, devices);

  this_npu->base.processes_count = 0;
  this_npu->base.processes = NULL;
  this_npu->base.processes_array_size = 0;

  *count += 1;
}

static bool gpuinfo_rknpu_get_device_handles(struct list_head *devices_list, unsigned *count) {
  *count = 0;
  rknpu_info = calloc(1, sizeof(struct gpu_info_rknpu));
  if (!rknpu_info) return false;
  add_rknpu_chip(devices_list, count);
  return true;
}

static void gpuinfo_rknpu_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_rknpu *gpu_info = container_of(_gpu_info, struct gpu_info_rknpu, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;

  static_info->integrated_graphics = true;
  static_info->encode_decode_shared = false;

  RESET_ALL(static_info->valid);
  snprintf(static_info->device_name, sizeof(static_info->device_name), "%s", gpu_info->base.pdev);
  SET_VALID(gpuinfo_device_name_valid, static_info->valid);
}

static int read_int_from_file(const char *path) {
  int value = 0;
  FILE *fp = fopen(path, "r");
  if (fp) {
    fscanf(fp, "%d", &value);
    fclose(fp);
  }
  return value;
}

static int read_npu_load(const char *file) {
  FILE *fp = fopen(file, "r");
  if (!fp) return -1;

  char line[256]; int sum = 0, load=0, count = 0;
  if (fgets(line, sizeof(line), fp))
      for (char *p = line; (p = strstr(p, "Core")); p++)
          if (sscanf(p, "Core%*d: %d%%", &load) == 1) {
            sum += load;
            count++;
          }

  fclose(fp);
  return count ? sum / count : -1;
}

static int set_gpuinfo_dynamic_memory(struct gpuinfo_dynamic_info *dynamic_info) {
  FILE *fp = fopen("/proc/meminfo", "r");
  if (!fp) return -1;
  char line[256];
  unsigned long mem_total = 0, mem_available = 0;
  while (fgets(line, sizeof(line), fp)) {
    if (sscanf(line, "MemTotal: %lu kB", &mem_total) == 1) {
      mem_total *= 1024;
      SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mem_total);
    } else if (sscanf(line, "MemAvailable: %lu kB", &mem_available) == 1) {
      mem_available *= 1024;
      SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, mem_available);
    }
  }
  fclose(fp);
  if (mem_total > 0 && mem_available > 0) {
    SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, mem_total - mem_available);
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate,
                        (dynamic_info->total_memory - dynamic_info->free_memory) * 100 / dynamic_info->total_memory);
  }
  return 0; 
}

static void gpuinfo_rknpu_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_rknpu *gpu_info = container_of(_gpu_info, struct gpu_info_rknpu, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  int gpu_clock_speed = read_int_from_file("/sys/class/devfreq/fdab0000.npu/cur_freq") / 1000000;
  int gpu_clock_speed_max = read_int_from_file("/sys/class/devfreq/fdab0000.npu/max_freq") / 1000000;
  int gpu_util_rate = read_npu_load("/sys/kernel/debug/rknpu/load");
  if (gpu_util_rate >= 0) 
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, gpu_util_rate);

  SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, gpu_clock_speed);
  SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, gpu_clock_speed_max);

  int gpu_temp = read_int_from_file("/sys/class/thermal/thermal_zone6/temp") / 1000;
  SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, gpu_temp);

  set_gpuinfo_dynamic_memory(dynamic_info);
}

static void gpuinfo_rknpu_get_running_processes(struct gpu_info *_gpu_info) {
  _gpu_info->processes_count = 0;
}

struct gpu_vendor gpu_vendor_rknpu = {
  .init = gpuinfo_rknpu_init,
  .shutdown = gpuinfo_rknpu_shutdown,
  .last_error_string = gpuinfo_rknpu_last_error_string,
  .get_device_handles = gpuinfo_rknpu_get_device_handles,
  .populate_static_info = gpuinfo_rknpu_populate_static_info,
  .refresh_dynamic_info = gpuinfo_rknpu_refresh_dynamic_info,
  .refresh_running_processes = gpuinfo_rknpu_get_running_processes,
  .name = "RK-NPU"
};

__attribute__((constructor)) static void init_extract_gpuinfo_rknpu(void) {
  register_gpu_vendor(&gpu_vendor_rknpu);
}
