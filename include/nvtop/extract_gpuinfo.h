/*
 *
 * Copyright (C) 2017-2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#ifndef EXTRACT_GPUINFO_H_
#define EXTRACT_GPUINFO_H_

#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/extract_gpuinfo_nvidia.h"

#include <stdbool.h>

enum gpuinfo_gputype {
  gpuinfo_type_nvidia_proprietary,
};

typedef struct gpu_info_struct {
  enum gpuinfo_gputype gpu_type;
  union {
    gpuinfo_nvidia_device_handle nvidia_gpuhandle;
  };
  gpuinfo_static_info static_info;
  gpuinfo_dynamic_info dynamic_info;
  unsigned processes_count;
  gpu_process *processes;
  union {
    gpuinfo_nvidia_internal_data nvidia_internal;
  };
} gpu_info;

bool gpuinfo_init_info_extraction(uint64_t mask_nvidia, unsigned *devices_count, gpu_info **devices);

bool gpuinfo_shutdown_info_extraction(unsigned device_count,
                                      gpu_info *devices);

bool gpuinfo_populate_static_infos(unsigned device_count, gpu_info *devices);

bool gpuinfo_refresh_dynamic_info(unsigned device_count, gpu_info *devices);

bool gpuinfo_refresh_processes(unsigned device_count, user *users, gpu_info *devices);

void gpuinfo_clean(unsigned device_count, gpu_info *devices);

void gpuinfo_clear_cache(void);

#endif // EXTRACT_GPUINFO_H_
