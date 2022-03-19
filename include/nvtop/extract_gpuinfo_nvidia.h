/*
 *
 * Copyright (C) 2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#ifndef EXTRACT_GPUINFO_NVIDIA_H_
#define EXTRACT_GPUINFO_NVIDIA_H_

#include "nvtop/extract_gpuinfo_common.h"

#include <stdbool.h>
#include <stdint.h>

typedef struct nvmlDevice *nvmlDevice_t;

struct gpuinfo_nvidia_internal_data {
  unsigned long long last_utilization_timestamp;
};

bool gpuinfo_nvidia_init(void);

void gpuinfo_nvidia_shutdown(void);

const char *gpuinfo_nvidia_last_error_string(void);

bool gpuinfo_nvidia_get_device_handles(
    nvmlDevice_t **handle_array_ptr, unsigned *count,
    uint64_t mask);

void gpuinfo_nvidia_populate_static_info(nvmlDevice_t device,
                                         struct gpuinfo_static_info *static_info);

void gpuinfo_nvidia_refresh_dynamic_info(nvmlDevice_t device,
                                         struct gpuinfo_dynamic_info *dynamic_info);

void gpuinfo_nvidia_get_running_processes(
    nvmlDevice_t device, struct gpuinfo_nvidia_internal_data *internal,
    unsigned *num_processes_recovered, struct gpu_process **processes_info);

#endif // EXTRACT_GPUINFO_NVIDIA_H_
