/*
 *
 * Copyright (C) 2026 NVTOP contributors
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

#ifndef EXTRACT_GPUINFO_APPLE_UTILS_H_
#define EXTRACT_GPUINFO_APPLE_UTILS_H_

#include "nvtop/extract_gpuinfo_common.h"

#include <CoreFoundation/CoreFoundation.h>
#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

struct gpuinfo_apple_process_sample {
  pid_t pid;
  uint64_t gpu_time;
  bool gpu_time_valid;
};

bool gpuinfo_apple_parse_process_sample(CFDictionaryRef properties, struct gpuinfo_apple_process_sample *sample);

bool gpuinfo_apple_calculate_gpu_usage(uint64_t previous_gpu_time, uint64_t current_gpu_time, uint64_t time_elapsed,
                                       unsigned *gpu_usage);

void gpuinfo_apple_add_process(struct gpu_info *gpu_info, pid_t pid, bool gpu_usage_valid, unsigned gpu_usage);

#ifdef __cplusplus
}
#endif

#endif // EXTRACT_GPUINFO_APPLE_UTILS_H_
