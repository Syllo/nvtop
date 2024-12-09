/*
 *
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include "list.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/info_messages.h"

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/utsname.h>

static int get_linux_kernel_release(unsigned *major, unsigned *minor, unsigned *patch) {
  struct utsname uname_str;
  int retval = uname(&uname_str);
  if (retval)
    return retval;
  int nmatch = sscanf(uname_str.release, "%u.%u.%u", major, minor, patch);
  return nmatch != 3;
}

enum messages {
  AMD_GPU_514,
  INTEL_GPU_519,
  MSM_GPU,
};

static const char *allMessages[] = {
    "Nvtop won't be able to show AMD GPU processes on your kernel version (requires Linux >= 5.14)",
    "Nvtop won't be able to show Intel GPU utilization and processes on your kernel version (requires Linux >= 5.19)",
    "This version of Nvtop does not yet support reporting all data for MSM GPUs, such as power, fan and temperature information",
};
static const char *message_array[sizeof(allMessages) / sizeof(*allMessages)];

void get_info_messages(struct list_head *devices, unsigned *num_messages, const char ***messages) {
  *num_messages = 0;
  unsigned linux_major, linux_minor, linux_patch;
  if (get_linux_kernel_release(&linux_major, &linux_minor, &linux_patch))
    return;

  *messages = message_array;
  bool hasIntel = false;
  bool hasMSM = false;
  bool hasAMD = false;
  struct gpu_info *gpuinfo;
  list_for_each_entry(gpuinfo, devices, list) {
    if (strcmp(gpuinfo->vendor->name, "Intel") == 0) {
      hasIntel = true;
    }
    if (strcmp(gpuinfo->vendor->name, "msm") == 0) {
      hasMSM = true;
    }
    if (strcmp(gpuinfo->vendor->name, "AMD") == 0) {
      hasAMD = true;
    }
  }
  if (hasAMD) {
    if (linux_major < 5 || (linux_major == 5 && linux_minor < 14)) {
      message_array[(*num_messages)++] = allMessages[AMD_GPU_514];
    }
  }
  if (hasIntel) {
    if (linux_major < 5 || (linux_major == 5 && linux_minor < 19)) {
      message_array[(*num_messages)++] = allMessages[INTEL_GPU_519];
    }
  }
  if (hasMSM) {
    message_array[(*num_messages)++] = allMessages[MSM_GPU];
  }
}
