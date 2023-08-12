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
  (void) pid;
  (void) buffer;
}

void get_command_from_pid(pid_t pid, char **buffer) {
  (void) pid;
  (void) buffer;
}

bool get_process_info(pid_t pid, struct process_cpu_usage *usage) {
  (void) pid;
  (void) usage;
  return false;
}
