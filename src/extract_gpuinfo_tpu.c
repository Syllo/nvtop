/*
 *
 * Copyright (C) 2025 Robert Dyro <robert.dyro@gmail.com>
 *
 * This file is part of Nvtop
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

#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <dlfcn.h>
#include <sys/time.h>

struct gpu_info_tpu {
  struct gpu_info base;
  int device_id;
};

struct tpu_chip_usage_data {
  char name[8];
  int64_t device_id;
  int64_t memory_usage;
  int64_t total_memory;
  double duty_cycle_pct;
  int64_t pid;
};

static bool gpuinfo_tpu_init(void);
static void gpuinfo_tpu_shutdown(void);
static const char *gpuinfo_tpu_last_error_string(void);
static bool gpuinfo_tpu_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_tpu_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_tpu_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_tpu_get_running_processes(struct gpu_info *_gpu_info);
static bool is_cache_valid(void);
static bool refresh_tpu_cache(void);
static void reset_tpu_cache(bool);
static void free_ptr(void **ptr);

struct gpu_vendor gpu_vendor_tpu = {
    .init = gpuinfo_tpu_init,
    .shutdown = gpuinfo_tpu_shutdown,
    .last_error_string = gpuinfo_tpu_last_error_string,
    .get_device_handles = gpuinfo_tpu_get_device_handles,
    .populate_static_info = gpuinfo_tpu_populate_static_info,
    .refresh_dynamic_info = gpuinfo_tpu_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_tpu_get_running_processes,
    .name = "TPU",
};

__attribute__((constructor)) static void init_extract_gpuinfo_tpu(void) { 
  register_gpu_vendor(&gpu_vendor_tpu);
}

int64_t tpu_chip_count = -1;
static struct gpu_info_tpu *gpu_infos;

#define STRINGIFY(x) STRINGIFY_HELPER_(x)
#define STRINGIFY_HELPER_(x) #x

#define VENDOR_TPU 0x1ae0
#define VENDOR_TPU_STR STRINGIFY(VENDOR_TPU)

#define MAX(x, y) ((x >= y) ? (x) : (y))
#define MIN(x, y) ((x <= y) ? (x) : (y))

#define int64 long long

int (*_tpu_chip_count)(void);
int (*_tpu_metrics)(int port, int64 *device_ids, int64 *memory_usage, 
                    int64 *total_memory, double *duty_cycle_pct, int n);
int (*_tpu_pids)(int64 *pids, int n);

char *libname = "libtpuinfo.so";
// -1 means allowing libtpuinfo to select the default port
// env LIBTPUINFO_GRPC_PORT={int} allows setting the port via an environment variable
// $ env LIBTPUINFO_GRPC_PORT=8431 nvtop
int tpu_runtime_monitoring_port = -1;  

/* TPU info cache ------------------------------------------------------------------------------- */
struct tpu_chip_usage_data *latest_chips_usage_data = NULL;
nvtop_time last_cache_refresh;
int64 *_pids, *_device_ids, *_memory_usage, *_total_memory;
double* _duty_cycle_pct;

bool is_cache_valid(void) {
  nvtop_time current_time;
  nvtop_get_current_time(&current_time);
  uint64_t t_diff_ns = nvtop_difftime_u64(last_cache_refresh, current_time);
  return t_diff_ns < 900 * 1000 * 1000; // 900ms
}

bool refresh_tpu_cache(void) {
  if (is_cache_valid()) return true;
  nvtop_get_current_time(&last_cache_refresh);
  if (tpu_chip_count <= 0) return false;
  if (_tpu_pids(_pids, tpu_chip_count) != 0) {
    reset_tpu_cache(false);
    return false;
  }
  for (int64_t i = 0; i < tpu_chip_count; i++) latest_chips_usage_data[i].pid = _pids[i];

  if (_tpu_metrics(tpu_runtime_monitoring_port, _device_ids, _memory_usage, _total_memory,
                   _duty_cycle_pct, tpu_chip_count) != 0) return false;
  for (int64_t i = 0; i < tpu_chip_count; i++) {
    latest_chips_usage_data[i].device_id = _device_ids[i];
    latest_chips_usage_data[i].memory_usage = _memory_usage[i];
    latest_chips_usage_data[i].total_memory = _total_memory[i];
    latest_chips_usage_data[i].duty_cycle_pct = _duty_cycle_pct[i];
  }
  return true;
}

void reset_tpu_cache(bool fully) {
  for (int64_t i = 0; i < tpu_chip_count; i++) {
    latest_chips_usage_data[i].memory_usage = 0;
    latest_chips_usage_data[i].duty_cycle_pct = 0;
    latest_chips_usage_data[i].pid = -1;
    if (fully) {
      snprintf(latest_chips_usage_data[i].name, sizeof(latest_chips_usage_data[i].name), "%s", "N/A");
      latest_chips_usage_data[i].device_id = 0;
      latest_chips_usage_data[i].total_memory = 0;
    }
  }
}
/* TPU info cache ------------------------------------------------------------------------------- */

bool gpuinfo_tpu_init(void) {
  char* error_msg;
  nvtop_get_current_time(&last_cache_refresh);
  // invalidate cache by putting it in the past
  last_cache_refresh = nvtop_substract_time(last_cache_refresh, (nvtop_time){10, 0}); 

  // Load dynamic library symbols
  void *handle = dlopen(libname, RTLD_LAZY);
  if (!handle) {
      error_msg = dlerror();
#ifndef NDEBUG
      if (error_msg != NULL) fprintf(stderr, "TPU support error: %s\n", error_msg);
#endif
      return false;
  }

  // Resolve the necessary symbols within the library
  _tpu_chip_count = dlsym(handle, "tpu_chip_count");
  error_msg = dlerror();
  if (error_msg != NULL) {
#ifndef NDEBUG
      fprintf(stderr, "libtpuinfo can't resolve symbol `tpu_chip_count` with error: %s\n", error_msg);
#endif
      return false;
  }
  _tpu_pids = dlsym(handle, "tpu_pids");
  error_msg = dlerror();
  if (error_msg != NULL) {
#ifndef NDEBUG
      fprintf(stderr, "libtpuinfo can't resolve symbol `tpu_pids` with error: %s\n", error_msg);
#endif
      return false;
  }
  _tpu_metrics = dlsym(handle, "tpu_metrics");
  error_msg = dlerror();
  if (error_msg != NULL) {
#ifndef NDEBUG
      fprintf(stderr, "libtpuinfo can't resolve symbol `tpu_metrics` with error: %s\n", error_msg);
#endif
      return false;
  }

  // Discover TPU devices
  tpu_chip_count = _tpu_chip_count();
  if (tpu_chip_count == 0) {
#ifndef NDEBUG
    fprintf(stderr, "Found 0 TPU devices on the system.\n");
#endif
    return false;
  }

  // Allocate memory for TPU device data cache
  latest_chips_usage_data = (struct tpu_chip_usage_data*)malloc(tpu_chip_count*sizeof(struct tpu_chip_usage_data));
  _pids = (int64*)malloc(sizeof(int64) * tpu_chip_count);
  _device_ids = (int64*)malloc(sizeof(int64) * tpu_chip_count);
  _memory_usage = (int64*)malloc(sizeof(int64) * tpu_chip_count);
  _total_memory = (int64*)malloc(sizeof(int64) * tpu_chip_count);
  _duty_cycle_pct = (double*)malloc(sizeof(double) * tpu_chip_count);
  reset_tpu_cache(true);
  return true;
}

void free_ptr(void **ptr) {
  if (ptr != NULL && *ptr != NULL) {
    free(*ptr);
    *ptr = NULL;
  }
}

void gpuinfo_tpu_shutdown(void) {
  free_ptr((void **)&gpu_infos);
  free_ptr((void **)&latest_chips_usage_data);
  free_ptr((void **)&_pids);
  free_ptr((void **)&_device_ids);
  free_ptr((void **)&_memory_usage);
  free_ptr((void **)&_total_memory);
  free_ptr((void **)&_duty_cycle_pct);
  tpu_chip_count = -1;
}

const char *gpuinfo_tpu_last_error_string(void) { return "Err"; }

static void add_tpu_chip(struct list_head *devices, unsigned *count) {
  struct gpu_info_tpu *this_tpu = &gpu_infos[*count];
  this_tpu->base.vendor = &gpu_vendor_tpu;
  this_tpu->device_id = *count;
  snprintf(this_tpu->base.pdev, PDEV_LEN, "TPU%u", *count);
  list_add_tail(&this_tpu->base.list, devices);

  this_tpu->base.processes_count = 0;
  this_tpu->base.processes = NULL;
  this_tpu->base.processes_array_size = 0;

  *count = *count + 1;
}

bool gpuinfo_tpu_get_device_handles(struct list_head *devices_list, unsigned *count) {
  *count = 0;
  if (tpu_chip_count <= 0) return false;
  gpu_infos = (struct gpu_info_tpu *)calloc(tpu_chip_count, sizeof(*gpu_infos));
  if (!gpu_infos) return false;
  for (int64_t i = 0; i < tpu_chip_count; i++) add_tpu_chip(devices_list, count);
  return true;
}

void gpuinfo_tpu_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_tpu *gpu_info = container_of(_gpu_info, struct gpu_info_tpu, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
  static_info->integrated_graphics = false;
  static_info->encode_decode_shared = false;
  RESET_ALL(static_info->valid);
  snprintf(static_info->device_name, MIN(sizeof(static_info->device_name), PDEV_LEN), "%s", gpu_info->base.pdev);
  SET_VALID(gpuinfo_device_name_valid, static_info->valid);
}

void gpuinfo_tpu_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_tpu *gpu_info = container_of(_gpu_info, struct gpu_info_tpu, base);
  // struct gpuinfo_static_info *static_info = &gpu_info->base.static_info; // unused
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  refresh_tpu_cache();

  if (gpu_info->device_id >= tpu_chip_count) return;
  struct tpu_chip_usage_data usage_data = latest_chips_usage_data[gpu_info->device_id];
  double mem_util = round(1e2 * (double)(usage_data.memory_usage) / (double)MAX(1, usage_data.total_memory));
  double tpu_util = round(usage_data.duty_cycle_pct);
  SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, (int)tpu_util);
  SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, (int)mem_util);
  SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, usage_data.total_memory);
  SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, usage_data.memory_usage);
  SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, usage_data.total_memory - usage_data.memory_usage);

  return;
}

void gpuinfo_tpu_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_tpu *gpu_info = container_of(_gpu_info, struct gpu_info_tpu, base);
  if (gpu_info->device_id >= tpu_chip_count) return;
  if (tpu_chip_count <= 0 || latest_chips_usage_data[gpu_info->device_id].pid < 0) {
    _gpu_info->processes_count = 0;
    return;
  }
  _gpu_info->processes_count = 1;
  if (_gpu_info->processes_array_size == 0) {
    _gpu_info->processes_array_size = 1;
    _gpu_info->processes = (struct gpu_process*)malloc(1 * sizeof(struct gpu_process));
    memset(_gpu_info->processes, 0, _gpu_info->processes_count * sizeof(*_gpu_info->processes));
  }
  _gpu_info->processes[0].type = gpu_process_compute;
  _gpu_info->processes[0].pid = latest_chips_usage_data[gpu_info->device_id].pid;
  _gpu_info->processes[0].gpu_memory_usage = _gpu_info->dynamic_info.used_memory;

  SET_VALID(gpuinfo_process_gpu_memory_usage_valid, _gpu_info->processes[0].valid);
}
