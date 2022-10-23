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

#include "nvtop/get_process_info.h"

#include <inttypes.h>
#include <pwd.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define pid_path_size 1024
static char pid_path[pid_path_size];

void get_username_from_pid(pid_t pid, char **buffer) {
  int written = snprintf(pid_path, pid_path_size, "/proc/%" PRIdMAX, (intmax_t)pid);
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
  int written = snprintf(pid_path, pid_path_size, "/proc/%" PRIdMAX "/cmdline", (intmax_t)pid);
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
  current_buffer[0] = '\0';

  size_t total_read = 0;
  do {
    size_t num_read = fread(current_buffer, 1, command_line_increment, pid_file);
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

/*
 *
 * From man 5 proc of /proc/<pid>/stat
 * For clock ticks per second use sysconf(_SC_CLK_TCK)
 *
 *  enum process_state {
 *   process_running = 'R',
 *   process_sleeping = 'S',
 *   process_sleeping_disk = 'D',
 *   process_zombie = 'Z',
 *   process_stopped = 'T',
 *   process_stopped_tracing = 't',
 *   process_dead = 'X',
 *   process_dead2 = 'x',
 *   process_wake_kill = 'K',
 *   process_waking = 'W',
 *   process_parked = 'P',
 * };
 *
 * struct stat_parse {
 *   int process_id;
 *   char *executable_filename;
 *   enum process_state process_state;
 *   int parent_pid;
 *   int process_group_id;
 *   int process_session_id;
 *   int process_tty;
 *   int foreground_process_group_id;
 *   unsigned kernel_flag;
 *   unsigned long num_minor_fault;
 *   unsigned long num_minor_fault_children;
 *   unsigned long num_major_fault;
 *   unsigned long num_major_fault_children;
 *   unsigned long user_time;           // in clock ticks
 *   unsigned long kernel_time;         // in clock ticks
 *   long children_user_time;   // in clock ticks
 *   long children_kernel_time; // in clock ticks
 *   long process_priority;
 *   long process_niceness;
 *   long process_num_threads;
 *   long next_sigalarm_time;
 *   unsigned long long process_start_time;
 *   unsigned long process_virt_mem_usage;
 *   long process_resident_mem_usage;
 *   long process_resident_mem_limit;
 *   unsigned long process_text_address_start;
 *   unsigned long process_text_address_end;
 *   unsigned long process_stack_address_start;
 *   unsigned long process_stack_address_current;
 *   unsigned long process_instruction_pointer;
 *   unsigned long process_signals; // Obsolete
 *   unsigned long process_signals_blocked; // Obsolete
 *   unsigned long process_signals_ignored; // Obsolete
 *   unsigned long process_signals_caught; // Obsolete
 *   unsigned long wait_channel_id;
 *   unsigned long process_num_page_swapped;
 *   unsigned long process_and_child_num_page_swapped;
 *   int process_exit_signal_to_parent;
 *   int process_processor;
 *   unsigned process_real_time_priority;
 *   unsigned policy;
 *   long long unsigned total_io_delays;
 *   long unsigned process_guest_time;
 *   long process_children_guest_time;
 *   unsigned long process_data_address_start;
 *   unsigned long process_data_address_end;
 *   unsigned long process_brk_start;
 *   unsigned long process_arguments_address_start;
 *   unsigned long process_arguments_address_end;
 *   unsigned long process_env_vars_address_start;
 *   unsigned long process_env_vars_address_end;
 *   int thread_exit_code;
 * };
 *
 */

bool get_process_info(pid_t pid, struct process_cpu_usage *usage) {
  double clock_ticks_per_second = sysconf(_SC_CLK_TCK);
  size_t page_size = (size_t)sysconf(_SC_PAGESIZE);
  int written = snprintf(pid_path, pid_path_size, "/proc/%" PRIdMAX "/stat", (intmax_t)pid);
  if (written == pid_path_size) {
    return false;
  }
  FILE *stat_file = fopen(pid_path, "r");
  if (!stat_file) {
    return false;
  }
  nvtop_get_current_time(&usage->timestamp);
  unsigned long total_user_time;   // in clock_ticks
  unsigned long total_kernel_time; // in clock_ticks
  unsigned long virtual_memory;    // In bytes
  long resident_memory;            // In page number?

  int retval = fscanf(stat_file,
                      "%*d %*[^)]) %*c %*d %*d %*d %*d %*d %*u %*u %*u %*u "
                      "%*u %lu %lu %*d %*d %*d %*d %*d %*d %*u %lu %ld",
                      &total_user_time, &total_kernel_time, &virtual_memory, &resident_memory);
  fclose(stat_file);
  if (retval != 4)
    return false;
  usage->total_user_time = total_user_time / clock_ticks_per_second;
  usage->total_kernel_time = total_kernel_time / clock_ticks_per_second;
  usage->virtual_memory = virtual_memory;
  usage->resident_memory = (size_t)resident_memory * page_size;
  return true;
}
