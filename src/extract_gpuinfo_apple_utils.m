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

#include "extract_gpuinfo_apple_utils.h"

#include "nvtop/common.h"

#include <stdio.h>
#include <stdlib.h>

#include <Foundation/Foundation.h>
#include <limits.h>
#include <string.h>

bool gpuinfo_apple_parse_process_sample(CFDictionaryRef properties, struct gpuinfo_apple_process_sample *sample) {
  memset(sample, 0, sizeof(*sample));
  if (!properties || CFGetTypeID(properties) != CFDictionaryGetTypeID())
    return false;

  NSDictionary *user_client_info = (__bridge NSDictionary *)properties;
  id client_creator_info = [user_client_info objectForKey:@"IOUserClientCreator"];
  if (![client_creator_info isKindOfClass:[NSString class]])
    return false;

  int pid;
  const char *client_creator = [client_creator_info UTF8String];
  // Client creator is in form: pid <pid>, <name>
  if (!client_creator || sscanf(client_creator, "pid %d,", &pid) != 1 || pid < 0)
    return false;
  sample->pid = pid;

  id app_usage = [user_client_info objectForKey:@"AppUsage"];
  if (![app_usage isKindOfClass:[NSArray class]])
    return true;

  uint64_t total_gpu_time = 0;
  for (id app_info in app_usage) {
    if (![app_info isKindOfClass:[NSDictionary class]])
      continue;

    id accumulated_gpu_time = [app_info objectForKey:@"accumulatedGPUTime"];
    if (![accumulated_gpu_time isKindOfClass:[NSNumber class]] || [accumulated_gpu_time longLongValue] < 0)
      continue;

    const uint64_t gpu_time = [accumulated_gpu_time unsignedLongLongValue];
    if (UINT64_MAX - total_gpu_time < gpu_time) {
      sample->gpu_time_valid = false;
      return true;
    }
    total_gpu_time += gpu_time;
    sample->gpu_time_valid = true;
  }

  sample->gpu_time = total_gpu_time;
  return true;
}

bool gpuinfo_apple_calculate_gpu_usage(uint64_t previous_gpu_time, uint64_t current_gpu_time, uint64_t time_elapsed,
                                       unsigned *gpu_usage) {
  if (!time_elapsed || current_gpu_time < previous_gpu_time)
    return false;

  const uint64_t gpu_time_delta = current_gpu_time - previous_gpu_time;
  if (gpu_time_delta >= time_elapsed) {
    *gpu_usage = 100;
  } else {
    *gpu_usage = (unsigned)((gpu_time_delta * UINT64_C(100) + time_elapsed / UINT64_C(2)) / time_elapsed);
  }
  return true;
}

void gpuinfo_apple_add_process(struct gpu_info *gpu_info, pid_t pid, bool gpu_usage_valid, unsigned gpu_usage) {
  struct gpu_process *process = NULL;
  for (unsigned i = 0; i < gpu_info->processes_count; ++i) {
    if (gpu_info->processes[i].pid == pid) {
      process = &gpu_info->processes[i];
      break;
    }
  }

  if (!process) {
    if (gpu_info->processes_array_size < gpu_info->processes_count + 1) {
      gpu_info->processes_array_size += COMMON_PROCESS_LINEAR_REALLOC_INC;
      gpu_info->processes =
          reallocarray(gpu_info->processes, gpu_info->processes_array_size, sizeof(*gpu_info->processes));
      if (!gpu_info->processes) {
        perror("Could not allocate memory: ");
        exit(EXIT_FAILURE);
      }
    }

    process = &gpu_info->processes[gpu_info->processes_count++];
    RESET_ALL(process->valid);
    process->pid = pid;
    process->type = gpu_process_graphical_compute;
  }

  if (gpu_usage_valid) {
    if (GPUINFO_PROCESS_FIELD_VALID(process, gpu_usage)) {
      const unsigned total_usage = process->gpu_usage + gpu_usage;
      process->gpu_usage = total_usage > 100 ? 100 : total_usage;
    } else {
      SET_GPUINFO_PROCESS(process, gpu_usage, gpu_usage);
    }
  }
}
