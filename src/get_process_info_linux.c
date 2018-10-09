/*
 *
 * Copyright (C) 2017 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include <pwd.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <inttypes.h>

static char pid_path[64];

void get_username_from_pid(pid_t pid, size_t size_buffer, char *buffer) {
  size_t written = snprintf(pid_path, 64, "/proc/%" PRIdMAX, (intmax_t) pid);
  if (written == 64) {
    buffer[0] = '\0';
    return;
  }
  struct stat folder_stat;
  int starval = stat(pid_path, &folder_stat);
  if (starval == -1) {
    buffer[0] = '\0';
    return;
  }
  uid_t user_id = folder_stat.st_uid;
  struct passwd *user_info = getpwuid(user_id);
  if (user_info == NULL) {
    buffer[0] = '\0';
    return;
  }
  strncpy(buffer, user_info->pw_name, size_buffer);
}

void get_pid_command_line(pid_t pid, size_t size_buffer, char *buffer) {
  size_t written = snprintf(pid_path, 64, "/proc/%" PRIdMAX "/cmdline", (intmax_t) pid);
  if (written == 64) {
    buffer[0] = '\0';
    return;
  }
  FILE *pid_file = fopen(pid_path, "r");
  if (!pid_file) {
    buffer[0] = '\0';
    return;
  }
  size_t read = fread(buffer, sizeof(*buffer), size_buffer, pid_file);
  if (read == size_buffer) {
    read -= 1;
  }
  buffer[read] = '\0';
  for (size_t i = 0; i < read; ++i) {
    if (buffer[i] == '\0')
      buffer[i] = ' ';
  }
}
