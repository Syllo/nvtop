/*
 * Copyright (C) 2022 YiFei Zhu <zhuyifei1999@gmail.com>
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
 *
 * This file is part of Nvtop and adapted from radeontop.
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

#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"

#include <ctype.h>
#include <dirent.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/sysmacros.h>
#include <unistd.h>

#ifndef KCMP_FILE
#define KCMP_FILE 0
#endif

struct callback_entry {
  struct gpu_info *gpu_info;
  processinfo_fdinfo_callback callback;
  bool active;
};

static unsigned registered_callback_entries;
#define FDINFO_CALLBACK_ARRAY_INCREMENT 4
static unsigned callback_entries_size;
static struct callback_entry *callback_entries;

void processinfo_drop_callback(const struct gpu_info *info) {
  for (unsigned index = 0; index < registered_callback_entries; ++index) {
    if (callback_entries[index].gpu_info == info) {
      memmove(&callback_entries[index], &callback_entries[index + 1],
              (registered_callback_entries - index) * sizeof(*callback_entries));
      return;
    }
  }
}

void processinfo_register_fdinfo_callback(processinfo_fdinfo_callback callback, struct gpu_info *info) {
  if (callback_entries_size == registered_callback_entries) {
    callback_entries_size += FDINFO_CALLBACK_ARRAY_INCREMENT;
    callback_entries = reallocarray(callback_entries, callback_entries_size, sizeof(*callback_entries));
    if (!callback_entries) {
      perror("Could not re-allocate memory: ");
      exit(EXIT_FAILURE);
    }
  }
  callback_entries[registered_callback_entries].gpu_info = info;
  callback_entries[registered_callback_entries].callback = callback;
  callback_entries[registered_callback_entries].active = true;
  registered_callback_entries++;
}

void processinfo_enable_disable_callback_for(const struct gpu_info *info, bool enable) {
  for (unsigned index = 0; index < registered_callback_entries; ++index) {
    if (callback_entries[index].gpu_info == info) {
      callback_entries[index].active = enable;
    }
  }
}

static bool is_drm_fd(int fd_dir_fd, const char *name) {
  struct stat stat;
  int ret;

  ret = fstatat(fd_dir_fd, name, &stat, 0);

  return ret == 0 && (stat.st_mode & S_IFMT) == S_IFCHR && major(stat.st_rdev) == 226;
}

// Increment for the number DRM FD tracked per process
// 8 has been experimentally selected for being small while avoiding multipe allocations in most common cases
#define DRM_FD_LINEAR_REALLOC_INC 8

void processinfo_sweep_fdinfos(void) {
  bool anyActiveCallback = false;
  for (unsigned callback_idx = 0; !anyActiveCallback && callback_idx < registered_callback_entries; ++callback_idx) {
    struct callback_entry *current_callback = &callback_entries[callback_idx];
    anyActiveCallback = anyActiveCallback || current_callback->active;
  }
  if (!anyActiveCallback)
    return;

  DIR *proc_dir = opendir("/proc");
  if (!proc_dir)
    return;

  static unsigned seen_fds_capacity = 0;
  static int *seen_fds = NULL;

  struct dirent *proc_dent;
  while ((proc_dent = readdir(proc_dir)) != NULL) {
    int pid_dir_fd = -1, fd_dir_fd = -1, fdinfo_dir_fd = -1;
    DIR *fdinfo_dir = NULL;
    unsigned int seen_fds_len = 0;
    struct dirent *fdinfo_dent;
    unsigned int client_pid;

    if (proc_dent->d_type != DT_DIR)
      continue;
    if (!isdigit(proc_dent->d_name[0]))
      continue;

    pid_dir_fd = openat(dirfd(proc_dir), proc_dent->d_name, O_DIRECTORY);
    if (pid_dir_fd < 0)
      continue;

    client_pid = atoi(proc_dent->d_name);
    if (!client_pid)
      goto next;

    fd_dir_fd = openat(pid_dir_fd, "fd", O_DIRECTORY);
    if (fd_dir_fd < 0)
      goto next;

    fdinfo_dir_fd = openat(pid_dir_fd, "fdinfo", O_DIRECTORY);
    if (fdinfo_dir_fd < 0)
      goto next;

    fdinfo_dir = fdopendir(fdinfo_dir_fd);
    if (!fdinfo_dir) {
      close(fdinfo_dir_fd);
      goto next;
    }

  next_fd:
    while ((fdinfo_dent = readdir(fdinfo_dir)) != NULL) {
      struct gpu_process processes_info_local = {0};
      int fd_num;

      if (fdinfo_dent->d_type != DT_REG)
        continue;
      if (!isdigit(fdinfo_dent->d_name[0]))
        continue;

      if (!is_drm_fd(fd_dir_fd, fdinfo_dent->d_name))
        continue;

      fd_num = atoi(fdinfo_dent->d_name);

      // check if this fd refers to the same open file as any seen ones.
      // we only care about unique opens
      for (unsigned i = 0; i < seen_fds_len; i++) {
        if (syscall(SYS_kcmp, client_pid, client_pid, KCMP_FILE, fd_num, seen_fds[i]) <= 0)
          goto next_fd;
      }

      if (seen_fds_len == seen_fds_capacity) {
        seen_fds_capacity += DRM_FD_LINEAR_REALLOC_INC;
        seen_fds = reallocarray(seen_fds, seen_fds_capacity, sizeof(*seen_fds));
        if (!seen_fds) {
          perror("Could not re-allocate memory: ");
          exit(EXIT_FAILURE);
        }
      }
      seen_fds[seen_fds_len++] = fd_num;

      int fdinfo_fd = openat(fdinfo_dir_fd, fdinfo_dent->d_name, O_RDONLY);
      if (fdinfo_fd < 0)
        continue;
      FILE *fdinfo_file = fdopen(fdinfo_fd, "r");
      if (!fdinfo_file) {
        close(fdinfo_fd);
        continue;
      }

      bool callback_success = false;
      struct callback_entry *current_callback = NULL;
      processes_info_local.pid = client_pid;
      for (unsigned callback_idx = 0; !callback_success && callback_idx < registered_callback_entries; ++callback_idx) {
        rewind(fdinfo_file);
        fflush(fdinfo_file);
        RESET_ALL(processes_info_local.valid);
        processes_info_local.type = gpu_process_unknown;
        current_callback = &callback_entries[callback_idx];
        if (current_callback->active)
          callback_success = current_callback->callback(current_callback->gpu_info, fdinfo_file, &processes_info_local);
        else
          callback_success = false;
      }
      fclose(fdinfo_file);
      if (!callback_success)
        continue;
      // Default to graphical type
      if (processes_info_local.type == gpu_process_unknown)
        processes_info_local.type = gpu_process_graphical;

      unsigned process_index =
          current_callback->gpu_info->processes_count ? current_callback->gpu_info->processes_count - 1 : 0;
      // Alloc when array is empty or realloc when this pid does not correspond to the last entry and the array is full
      if ((current_callback->gpu_info->processes_count == 0 ||
           current_callback->gpu_info->processes[process_index].pid != (pid_t)client_pid) &&
          current_callback->gpu_info->processes_count == current_callback->gpu_info->processes_array_size) {
        current_callback->gpu_info->processes_array_size += COMMON_PROCESS_LINEAR_REALLOC_INC;
        current_callback->gpu_info->processes =
            reallocarray(current_callback->gpu_info->processes, current_callback->gpu_info->processes_array_size,
                         sizeof(*current_callback->gpu_info->processes));
        if (!current_callback->gpu_info->processes) {
          perror("Could not re-allocate memory: ");
          exit(EXIT_FAILURE);
        }
      new_empty_process_entry:
        process_index = current_callback->gpu_info->processes_count++;
        memset(&current_callback->gpu_info->processes[process_index], 0,
               sizeof(*current_callback->gpu_info->processes));
        current_callback->gpu_info->processes[process_index].pid = client_pid;
      }
      // No alloc/realloc with different pid case
      if (current_callback->gpu_info->processes_count == 0 ||
          current_callback->gpu_info->processes[process_index].pid != (pid_t)client_pid) {
        goto new_empty_process_entry;
      }
      struct gpu_process *process_info = &current_callback->gpu_info->processes[process_index];

      process_info->type |= processes_info_local.type;

      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, gpu_memory_usage)) {
        SET_GPUINFO_PROCESS(process_info, gpu_memory_usage,
                            process_info->gpu_memory_usage + processes_info_local.gpu_memory_usage);
      }

      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, gpu_usage)) {
        SET_GPUINFO_PROCESS(process_info, gpu_usage, process_info->gpu_usage + processes_info_local.gpu_usage);
      }

      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, encode_usage)) {
        SET_GPUINFO_PROCESS(process_info, encode_usage, process_info->encode_usage + processes_info_local.encode_usage);
      }

      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, decode_usage)) {
        SET_GPUINFO_PROCESS(process_info, decode_usage, process_info->decode_usage + processes_info_local.decode_usage);
      }

      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, gfx_engine_used)) {
        SET_GPUINFO_PROCESS(process_info, gfx_engine_used,
                            process_info->gfx_engine_used + processes_info_local.gfx_engine_used);
      }

      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, compute_engine_used)) {
        SET_GPUINFO_PROCESS(process_info, compute_engine_used,
                            process_info->compute_engine_used + processes_info_local.compute_engine_used);
      }

      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, enc_engine_used)) {
        SET_GPUINFO_PROCESS(process_info, enc_engine_used,
                            process_info->enc_engine_used + processes_info_local.enc_engine_used);
      }

      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, dec_engine_used)) {
        SET_GPUINFO_PROCESS(process_info, dec_engine_used,
                            process_info->dec_engine_used + processes_info_local.dec_engine_used);
      }
      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, gpu_cycles)) {
        SET_GPUINFO_PROCESS(process_info, gpu_cycles,
                            process_info->gpu_cycles + processes_info_local.gpu_cycles);
      }
      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, sample_delta)) {
        SET_GPUINFO_PROCESS(process_info, sample_delta,
                            process_info->sample_delta + processes_info_local.sample_delta);
      }
    }

  next:
    if (fdinfo_dir)
      closedir(fdinfo_dir);

    if (fd_dir_fd >= 0)
      close(fd_dir_fd);
    close(pid_dir_fd);
  }

  closedir(proc_dir);
  return;
}
