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
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#ifdef __linux__
// for major() on linux (available in sys/types.h on BSD)
#include <sys/sysmacros.h>
#endif
#include <sys/types.h>
#include <unistd.h>

#ifndef KCMP_FILE
#define KCMP_FILE 0
#endif

#define DRM_MAJOR 226
// A DRM device exposes a primary (card) node and usually a render node.
#define FDINFO_MAX_DRM_NODES 4

struct callback_entry {
  struct gpu_info *gpu_info;
  processinfo_fdinfo_callback callback;
  // Device numbers of the DRM nodes owned by this device. Empty when the
  // backend could not provide a node, in which case the entry is a candidate
  // for every DRM fd that no other device owns.
  dev_t drm_nodes[FDINFO_MAX_DRM_NODES];
  unsigned drm_nodes_count;
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
              (registered_callback_entries - index - 1) * sizeof(*callback_entries));
      --registered_callback_entries;
      return;
    }
  }
}

static void add_drm_node(struct callback_entry *entry, dev_t node) {
  for (unsigned i = 0; i < entry->drm_nodes_count; ++i) {
    if (entry->drm_nodes[i] == node)
      return;
  }
  if (entry->drm_nodes_count < FDINFO_MAX_DRM_NODES)
    entry->drm_nodes[entry->drm_nodes_count++] = node;
}

// Record the node and its siblings listed in /sys/dev/char/<major>:<minor>/device/drm,
// so that processes holding either the primary or the render node are attributed.
static void resolve_drm_nodes(struct callback_entry *entry, const char *drm_node) {
  struct stat node_stat;
  entry->drm_nodes_count = 0;
  if (!drm_node || fstatat(AT_FDCWD, drm_node, &node_stat, 0) != 0 || (node_stat.st_mode & S_IFMT) != S_IFCHR ||
      major(node_stat.st_rdev) != DRM_MAJOR)
    return;
  add_drm_node(entry, node_stat.st_rdev);

  char drm_dir_path[64];
  snprintf(drm_dir_path, sizeof(drm_dir_path), "/sys/dev/char/%u:%u/device/drm", major(node_stat.st_rdev),
           minor(node_stat.st_rdev));
  DIR *drm_dir = opendir(drm_dir_path);
  if (!drm_dir)
    return;
  struct dirent *drm_dent;
  while ((drm_dent = readdir(drm_dir)) != NULL) {
    if (strncmp(drm_dent->d_name, "card", 4) && strncmp(drm_dent->d_name, "renderD", 7))
      continue;
    char dev_path[sizeof(drm_dir_path) + sizeof(drm_dent->d_name) + 8];
    snprintf(dev_path, sizeof(dev_path), "%s/%s/dev", drm_dir_path, drm_dent->d_name);
    FILE *dev_file = fopen(dev_path, "r");
    if (!dev_file)
      continue;
    unsigned node_major, node_minor;
    if (fscanf(dev_file, "%u:%u", &node_major, &node_minor) == 2 && node_major == DRM_MAJOR)
      add_drm_node(entry, makedev(node_major, node_minor));
    fclose(dev_file);
  }
  closedir(drm_dir);
}

void processinfo_register_fdinfo_callback(processinfo_fdinfo_callback callback, struct gpu_info *info,
                                          const char *drm_node) {
  if (callback_entries_size == registered_callback_entries) {
    callback_entries_size += FDINFO_CALLBACK_ARRAY_INCREMENT;
    callback_entries = reallocarray(callback_entries, callback_entries_size, sizeof(*callback_entries));
    if (!callback_entries) {
      perror("Could not re-allocate memory: ");
      exit(EXIT_FAILURE);
    }
  }
  struct callback_entry *entry = &callback_entries[registered_callback_entries];
  entry->gpu_info = info;
  entry->callback = callback;
  entry->active = true;
  resolve_drm_nodes(entry, drm_node);
  registered_callback_entries++;
}

void processinfo_enable_disable_callback_for(const struct gpu_info *info, bool enable) {
  for (unsigned index = 0; index < registered_callback_entries; ++index) {
    if (callback_entries[index].gpu_info == info) {
      callback_entries[index].active = enable;
    }
  }
}

static struct callback_entry *find_drm_node_owner(dev_t node) {
  for (unsigned index = 0; index < registered_callback_entries; ++index) {
    struct callback_entry *entry = &callback_entries[index];
    for (unsigned i = 0; i < entry->drm_nodes_count; ++i) {
      if (entry->drm_nodes[i] == node)
        return entry;
    }
  }
  return NULL;
}

static bool run_callback(struct callback_entry *entry, FILE *fdinfo_file, struct gpu_process *process_info) {
  rewind(fdinfo_file);
  RESET_ALL(process_info->valid);
  process_info->type = gpu_process_unknown;
  return entry->callback(entry->gpu_info, fdinfo_file, process_info);
}

static bool get_drm_fd_node(int fd_dir_fd, const char *name, dev_t *node) {
  struct stat stat;
  if (fstatat(fd_dir_fd, name, &stat, 0) < 0)
    return false;
  if ((stat.st_mode & S_IFMT) != S_IFCHR || major(stat.st_rdev) != DRM_MAJOR)
    return false;
  *node = stat.st_rdev;
  return true;
}

// Increment for the number DRM FD tracked per process
// 8 has been experimentally selected for being small while avoiding multiple allocations in most common cases
#define DRM_FD_LINEAR_REALLOC_INC 8

void processinfo_sweep_fdinfos(void) {
  bool anyActiveCallback = false;
  bool anyActiveWithoutNodes = false;
  for (unsigned callback_idx = 0; callback_idx < registered_callback_entries; ++callback_idx) {
    struct callback_entry *current_callback = &callback_entries[callback_idx];
    anyActiveCallback = anyActiveCallback || current_callback->active;
    anyActiveWithoutNodes = anyActiveWithoutNodes || (current_callback->active && !current_callback->drm_nodes_count);
  }
  if (!anyActiveCallback)
    return;

  DIR *proc_dir = opendir("/proc");
  if (!proc_dir)
    return;

  // DRM fds already handled for the current process, with the node they are
  // opened to: only fds to the same node can refer to the same open file.
  static unsigned seen_fds_capacity = 0;
  static struct {
    int fd;
    dev_t node;
  } *seen_fds = NULL;

  struct dirent *proc_dent;
  while ((proc_dent = readdir(proc_dir)) != NULL) {
    int pid_dir_fd = -1, fd_dir_fd = -1;
    DIR *fd_dir = NULL;
    unsigned int seen_fds_len = 0;
    struct dirent *fd_dent;
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

    // Walk the fds and only open the fdinfo of the few that are DRM fds.
    fd_dir = fdopendir(fd_dir_fd);
    if (!fd_dir)
      goto next;

  next_fd:
    while ((fd_dent = readdir(fd_dir)) != NULL) {
      struct gpu_process processes_info_local = {0};
      int fd_num;
      dev_t drm_node;

      if (!isdigit(fd_dent->d_name[0]))
        continue;

      if (!get_drm_fd_node(fd_dir_fd, fd_dent->d_name, &drm_node))
        continue;

      // Only parse fds of monitored devices. A fd owned by no device belongs to a
      // driver without process reporting (or to an unmonitored device).
      struct callback_entry *owner = find_drm_node_owner(drm_node);
      if (owner ? !owner->active : !anyActiveWithoutNodes)
        continue;

      fd_num = atoi(fd_dent->d_name);

      // check if this fd refers to the same open file as any seen ones.
      // we only care about unique opens
      for (unsigned i = 0; i < seen_fds_len; i++) {
        if (seen_fds[i].node == drm_node &&
            syscall(SYS_kcmp, client_pid, client_pid, KCMP_FILE, fd_num, seen_fds[i].fd) <= 0)
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
      seen_fds[seen_fds_len].fd = fd_num;
      seen_fds[seen_fds_len].node = drm_node;
      seen_fds_len++;

      char fdinfo_path[sizeof("fdinfo/") + sizeof(fd_dent->d_name)];
      snprintf(fdinfo_path, sizeof(fdinfo_path), "fdinfo/%s", fd_dent->d_name);
      int fdinfo_fd = openat(pid_dir_fd, fdinfo_path, O_RDONLY);
      if (fdinfo_fd < 0)
        continue;
      FILE *fdinfo_file = fdopen(fdinfo_fd, "r");
      if (!fdinfo_file) {
        close(fdinfo_fd);
        continue;
      }

      struct callback_entry *current_callback = NULL;
      processes_info_local.pid = client_pid;
      if (owner) {
        if (run_callback(owner, fdinfo_file, &processes_info_local))
          current_callback = owner;
      } else {
        for (unsigned callback_idx = 0; !current_callback && callback_idx < registered_callback_entries;
             ++callback_idx) {
          struct callback_entry *candidate = &callback_entries[callback_idx];
          if (candidate->active && !candidate->drm_nodes_count &&
              run_callback(candidate, fdinfo_file, &processes_info_local))
            current_callback = candidate;
        }
      }
      fclose(fdinfo_file);
      if (!current_callback)
        continue;

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

    // We need to default to 'graphical' for processes with unknown workload types.
    // Do this once all fds of this pid are merged: an idle fd must not mark a process that uses compute
    // on its other fds as graphical. (The entry of the pid being scanned is always the last one.)
    for (unsigned callback_idx = 0; callback_idx < registered_callback_entries; ++callback_idx) {
      // Skip disabled callbacks: their parser did not run, so they hold no entry for this pid.
      if (!callback_entries[callback_idx].active)
        continue;
      struct gpu_info *gpu_info = callback_entries[callback_idx].gpu_info;
      if (gpu_info->processes_count > 0) {
        struct gpu_process *process_info = &gpu_info->processes[gpu_info->processes_count - 1];
        if (process_info->pid == (pid_t)client_pid && process_info->type == gpu_process_unknown)
          process_info->type = gpu_process_graphical;
      }
    }

  next:
    // The directory stream owns fd_dir_fd once it has been created.
    if (fd_dir)
      closedir(fd_dir);
    else if (fd_dir_fd >= 0)
      close(fd_dir_fd);
    close(pid_dir_fd);
  }

  closedir(proc_dir);
}
