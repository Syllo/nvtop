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

static char pid_path[64];

void get_username_from_pid(pid_t pid, size_t size_buffer, char *buffer) {
  size_t written = snprintf(pid_path, 64, "/proc/%d", pid);
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
