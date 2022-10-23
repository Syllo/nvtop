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

#ifndef GET_PROCESS_INFO_H_
#define GET_PROCESS_INFO_H_

#include <stdbool.h>
#include <stdlib.h>
#include <sys/types.h>

#include "nvtop/time.h"

struct process_cpu_usage {
  double total_user_time;   // Seconds
  double total_kernel_time; // Seconds
  size_t virtual_memory;    // Bytes
  size_t resident_memory;   // Bytes
  nvtop_time timestamp;
};

void get_username_from_pid(pid_t pid, char **buffer);

void get_command_from_pid(pid_t pid, char **buffer);

bool get_process_info(pid_t pid, struct process_cpu_usage *usage);

#endif // GET_PROCESS_INFO_H_
