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

#include <inttypes.h>
#include <pwd.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define pid_path_size 1024
static char pid_path[pid_path_size];

void get_username_from_pid(pid_t pid, char **buffer) {
  size_t written =
      snprintf(pid_path, pid_path_size, "/proc/%" PRIdMAX, (intmax_t)pid);
  if (written == pid_path_size) {
    *buffer = NULL;
    return;
  }
  struct stat folder_stat;
  int starval = stat(pid_path, &folder_stat);
  if (starval == -1) {
    *buffer = NULL;
    return;
  }
  uid_t user_id = folder_stat.st_uid;
  struct passwd *user_info = getpwuid(user_id);
  if (user_info == NULL) {
    *buffer = NULL;
    return;
  }
  size_t namelen = strlen(user_info->pw_name) + 1;
  *buffer = malloc(namelen * sizeof(**buffer));

  strncpy(*buffer, user_info->pw_name, namelen);
}

#define command_line_increment 32

void get_command_from_pid(pid_t pid, char **buffer) {
  size_t written = snprintf(pid_path, pid_path_size,
                            "/proc/%" PRIdMAX "/cmdline", (intmax_t)pid);
  if (written == pid_path_size) {
    *buffer = NULL;
    return;
  }
  FILE *pid_file = fopen(pid_path, "r");
  if (!pid_file) {
    *buffer = NULL;
    return;
  }

  size_t size_buffer = command_line_increment;
  *buffer = malloc(size_buffer);
  char *current_buffer = *buffer;

  size_t total_read = 0;
  do {
    size_t num_read =
        fread(current_buffer, 1, command_line_increment, pid_file);
    total_read += num_read;
    if (num_read == command_line_increment) {
      size_buffer += command_line_increment;
      *buffer = realloc(*buffer, size_buffer);
      current_buffer = &((*buffer)[total_read]);
    }
  } while (!feof(pid_file) && !ferror(pid_file));
  if (ferror(pid_file)) {
    fclose(pid_file);
    free(*buffer);
    *buffer = NULL;
    return;
  }
  fclose(pid_file);

  for (size_t i = 0; total_read && i < total_read - 1; ++i) {
    if ((*buffer)[i] == '\0')
      (*buffer)[i] = ' ';
  }
}
