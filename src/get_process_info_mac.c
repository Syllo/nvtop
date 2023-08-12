/*
 *
 * Copyright (C) 2023 Robin Voetter <robin@voetter.nl>
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

#include "nvtop/get_process_info.h"

#include <libproc.h>
#include <sys/sysctl.h>
#include <pwd.h>
#include <mach/mach_time.h>

#include <string.h>
#include <stdio.h>

void get_username_from_pid(pid_t pid, char **buffer) {
  struct proc_bsdshortinfo proc;
  const int st = proc_pidinfo(pid, PROC_PIDT_SHORTBSDINFO, 0, &proc, PROC_PIDT_SHORTBSDINFO_SIZE);
  if (st != PROC_PIDT_SHORTBSDINFO_SIZE) {
    goto error;
  }

  struct passwd *user_info = getpwuid(proc.pbsi_uid);
  if (user_info == NULL) {
    goto error;
  }

  const size_t namelen = strlen(user_info->pw_name) + 1;
  *buffer = malloc(namelen * sizeof(**buffer));
  strncpy(*buffer, user_info->pw_name, namelen);
  return;
error:
  *buffer = NULL;
}

void get_command_from_pid(pid_t pid, char **buffer) {
  // See https://chromium.googlesource.com/crashpad/crashpad/+/360e441c53ab4191a6fd2472cc57c3343a2f6944/util/posix/process_util_mac.cc
  size_t argmax;
  size_t argmax_estimate;
  char *procargs = NULL;
  int tries = 3;
  do {
    int mib[] = {CTL_KERN, KERN_PROCARGS2, pid};
    if (sysctl(mib, 3, NULL, &argmax_estimate, NULL, 0) != 0) {
      goto error_free_procargs;
    }

    argmax = argmax_estimate + 1;
    procargs = realloc(procargs, argmax);
    if (sysctl(mib, 3, procargs, &argmax, NULL, 0) != 0) {
      goto error_free_procargs;
    }
  } while (argmax == argmax_estimate + 1 && --tries != 0);

  unsigned argc;
  memcpy(&argc, procargs, sizeof(argc));

  size_t i = sizeof(argc);
  // Skip executable path.
  while (i < argmax && procargs[i] != 0) {
    ++i;
  }
  // Find the first string
  while (i < argmax && procargs[i] == 0) {
    ++i;
  }

  const size_t argv0 = i;
  // Count the total size of the args by finding the end.
  for (unsigned int arg = 0; arg < argc && i < argmax; ++arg) {
    while (i < argmax && procargs[i] != 0) {
      ++i;
    }
    ++i; // We are going to replace this null character with a space (or a null in the case of the last).
  }
  const size_t args_size = i - argv0;
  if (args_size == 0) {
    goto error_free_procargs;
  }
  char* args = malloc(args_size);
  *buffer = args;

  i = argv0;
  for (unsigned int arg = 0; arg < argc && i < argmax; ++arg) {
    while (i < argmax && procargs[i] != 0) {
      *args++ = procargs[i++];
    }
    *args++ = ' ';
    ++i;
  }
  args[-1] = 0;

  free(procargs);
  return;
error_free_procargs:
  free(procargs);
  *buffer = NULL;
  return;
}

bool get_process_info(pid_t pid, struct process_cpu_usage *usage) {
  struct proc_taskinfo proc;
  const int st = proc_pidinfo(pid, PROC_PIDTASKINFO, 0, &proc, PROC_PIDTASKINFO_SIZE);
  if (st != PROC_PIDTASKINFO_SIZE) {
    return false;
  }

  nvtop_get_current_time(&usage->timestamp);

  // TODO: Should we implement this workaround?
  // https://github.com/htop-dev/htop/blob/main/darwin/PlatformHelpers.c#L98
  mach_timebase_info_data_t info;
  mach_timebase_info(&info);
  const double nanoseconds_per_tick = (double)info.numer / (double)info.denom;

  usage->total_user_time = (proc.pti_total_user * nanoseconds_per_tick) / 1000000000.0;
  usage->total_kernel_time = (proc.pti_total_system * nanoseconds_per_tick) / 1000000000.0;
  usage->virtual_memory = proc.pti_virtual_size;
  usage->resident_memory = proc.pti_resident_size;
  return true;
}
