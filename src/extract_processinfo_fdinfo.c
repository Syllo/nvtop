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
#include <errno.h>
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

#define FDINFO_MAX_DEVICE_NODES 4

struct callback_entry {
  struct gpu_info *gpu_info;
  processinfo_fdinfo_callback callback;
  // Device numbers of the nodes owned by this device. A registration without
  // a usable node is rejected, so a registered entry always has at least one.
  dev_t device_nodes[FDINFO_MAX_DEVICE_NODES];
  unsigned device_nodes_count;
  bool active;
  bool scan_failed;
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

static void add_device_node(struct callback_entry *entry, dev_t node) {
  for (unsigned i = 0; i < entry->device_nodes_count; ++i) {
    if (entry->device_nodes[i] == node)
      return;
  }
  if (entry->device_nodes_count < FDINFO_MAX_DEVICE_NODES)
    entry->device_nodes[entry->device_nodes_count++] = node;
}

// Record the node and, for DRM devices, the siblings listed in
// /sys/dev/char/<major>:<minor>/device/drm, so that processes holding either
// the primary or the render node are attributed. Accel devices have no such
// directory and simply keep their single node. Return false if the node cannot
// be resolved to a character device.
static bool resolve_device_nodes(struct callback_entry *entry, const char *device_node) {
  const unsigned drm_major = 226;
  struct stat node_stat;
  entry->device_nodes_count = 0;
  if (!device_node || fstatat(AT_FDCWD, device_node, &node_stat, 0) != 0 || (node_stat.st_mode & S_IFMT) != S_IFCHR)
    return false;
  add_device_node(entry, node_stat.st_rdev);

  // Only DRM devices have card/render siblings; accel and other device nodes
  // keep their single node.
  if (major(node_stat.st_rdev) == drm_major) {
    char drm_dir_path[64];
    snprintf(drm_dir_path, sizeof(drm_dir_path), "/sys/dev/char/%u:%u/device/drm", major(node_stat.st_rdev),
             minor(node_stat.st_rdev));
    DIR *drm_dir = opendir(drm_dir_path);
    if (drm_dir) {
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
        // The device/drm directory only lists DRM card/render nodes; ignore
        // anything else rather than trusting an unexpected major.
        if (fscanf(dev_file, "%u:%u", &node_major, &node_minor) == 2 && node_major == drm_major)
          add_device_node(entry, makedev(node_major, node_minor));
        fclose(dev_file);
      }
      closedir(drm_dir);
    }
  }
  return true;
}

bool processinfo_register_fdinfo_callback(processinfo_fdinfo_callback callback, struct gpu_info *info,
                                          const char *device_node) {
  if (callback_entries_size == registered_callback_entries) {
    callback_entries_size += FDINFO_CALLBACK_ARRAY_INCREMENT;
    callback_entries = reallocarray(callback_entries, callback_entries_size, sizeof(*callback_entries));
    if (!callback_entries) {
      perror("Could not re-allocate memory: ");
      exit(EXIT_FAILURE);
    }
  }
  struct callback_entry *entry = &callback_entries[registered_callback_entries];
  if (!resolve_device_nodes(entry, device_node))
    return false;
  entry->gpu_info = info;
  entry->callback = callback;
  entry->active = true;
  entry->scan_failed = false;
  registered_callback_entries++;
  return true;
}

void processinfo_enable_disable_callback_for(const struct gpu_info *info, bool enable) {
  for (unsigned index = 0; index < registered_callback_entries; ++index) {
    if (callback_entries[index].gpu_info == info) {
      callback_entries[index].active = enable;
    }
  }
}

// Processes and descriptors can disappear while /proc is being scanned, and
// the processes of other users cannot be inspected without privileges. Neither
// makes the scan of the visible processes incomplete.
static bool fdinfo_error_is_skippable(void) {
  return errno == ENOENT || errno == ESRCH || errno == EACCES || errno == EPERM;
}

static struct callback_entry *find_node_owner(dev_t node) {
  for (unsigned index = 0; index < registered_callback_entries; ++index) {
    struct callback_entry *entry = &callback_entries[index];
    for (unsigned i = 0; i < entry->device_nodes_count; ++i) {
      if (entry->device_nodes[i] == node)
        return entry;
    }
  }
  return NULL;
}

// A failure while handling a fd only affects the device owning it. A fd that no
// device owns most likely belongs to a driver without nvtop support, even when
// a device without known nodes cannot parse it, so it affects no device.
static void mark_fd_failure(struct callback_entry *owner) {
  if (owner)
    owner->scan_failed = true;
}

// Reading the fdinfo of a descriptor that was closed, or of a process that
// exited, since it was opened fails with ENOENT or ESRCH: the fd is gone.
static bool fdinfo_read_vanished(FILE *fdinfo_file) {
  return ferror(fdinfo_file) && fdinfo_error_is_skippable();
}

static bool run_callback(struct callback_entry *entry, FILE *fdinfo_file, struct gpu_process *process_info) {
  rewind(fdinfo_file);
  RESET_ALL(process_info->valid);
  process_info->type = gpu_process_unknown;
  errno = 0;
  return entry->callback(entry->gpu_info, fdinfo_file, process_info) && !ferror(fdinfo_file);
}

// Any stat failure means the fd is not usable as a registered device node: a
// device node cannot fail stat the way an fd to an unreachable network or FUSE
// file can, so the error does not affect the scan.
static bool get_fd_node(int fd_dir_fd, const char *name, dev_t *node) {
  struct stat stat;
  if (fstatat(fd_dir_fd, name, &stat, 0) < 0)
    return false;
  if ((stat.st_mode & S_IFMT) != S_IFCHR)
    return false;
  *node = stat.st_rdev;
  return true;
}

// Reads the drm-client-id key, which identifies the open device file: duplicated
// and shared descriptors (dup, fork, fd passing) report the same value. The
// driver prints it near the top, so stop as soon as it is found.
static bool read_drm_client_id(FILE *fdinfo_file, unsigned long long *client_id) {
  static const char key[] = "drm-client-id:";
  char line[256];
  while (fgets(line, sizeof(line), fdinfo_file)) {
    if (strncmp(line, key, sizeof(key) - 1))
      continue;
    char *endptr;
    *client_id = strtoull(line + sizeof(key) - 1, &endptr, 10);
    return endptr != line + sizeof(key) - 1;
  }
  return false;
}

// Device clients already accounted for during the current sweep. A client id is
// unique for the node it was opened on.
static struct seen_client {
  dev_t node;
  unsigned long long id;
  struct gpu_info *gpu_info;
} *seen_clients;
static unsigned seen_clients_count, seen_clients_capacity;

static struct seen_client *find_seen_client(dev_t node, unsigned long long id) {
  for (unsigned i = 0; i < seen_clients_count; ++i) {
    if (seen_clients[i].node == node && seen_clients[i].id == id)
      return &seen_clients[i];
  }
  return NULL;
}

// Only record clients whose fdinfo was parsed: if this fd could not be, another
// descriptor of the same client still can.
static void record_client(dev_t node, unsigned long long id, struct gpu_info *gpu_info) {
  if (seen_clients_count == seen_clients_capacity) {
    seen_clients_capacity = seen_clients_capacity ? 2 * seen_clients_capacity : 16;
    seen_clients = reallocarray(seen_clients, seen_clients_capacity, sizeof(*seen_clients));
    if (!seen_clients) {
      perror("Could not re-allocate memory: ");
      exit(EXIT_FAILURE);
    }
  }
  seen_clients[seen_clients_count++] = (struct seen_client){.node = node, .id = id, .gpu_info = gpu_info};
}

// Find the process entry for this pid, appending one when the pid differs from
// the last entry. The sweep handles all the fds of a pid together, so the entry
// of the current pid, if any, is always the last one.
static struct gpu_process *process_entry_for(struct gpu_info *gpu_info, unsigned pid) {
  unsigned process_index = gpu_info->processes_count ? gpu_info->processes_count - 1 : 0;
  if (gpu_info->processes_count > 0 && gpu_info->processes[process_index].pid == (pid_t)pid)
    return &gpu_info->processes[process_index];

  if (gpu_info->processes_count == gpu_info->processes_array_size) {
    gpu_info->processes_array_size += COMMON_PROCESS_LINEAR_REALLOC_INC;
    gpu_info->processes = reallocarray(gpu_info->processes, gpu_info->processes_array_size,
                                       sizeof(*gpu_info->processes));
    if (!gpu_info->processes) {
      perror("Could not re-allocate memory: ");
      exit(EXIT_FAILURE);
    }
  }
  process_index = gpu_info->processes_count++;
  memset(&gpu_info->processes[process_index], 0, sizeof(*gpu_info->processes));
  gpu_info->processes[process_index].pid = pid;
  return &gpu_info->processes[process_index];
}

// Increment for the number device FD tracked per process
// 8 has been experimentally selected for being small while avoiding multiple allocations in most common cases
#define FD_LINEAR_REALLOC_INC 8

void processinfo_sweep_fdinfos(void) {
  bool anyActiveCallback = false;
  for (unsigned callback_idx = 0; callback_idx < registered_callback_entries; ++callback_idx) {
    struct callback_entry *current_callback = &callback_entries[callback_idx];
    current_callback->gpu_info->processes_query_succeeded = false;
    current_callback->scan_failed = false;
    anyActiveCallback = anyActiveCallback || current_callback->active;
  }
  if (!anyActiveCallback)
    return;
  seen_clients_count = 0;

  DIR *proc_dir = opendir("/proc");
  if (!proc_dir)
    return;
  // Failures that cannot be attributed to a device make the whole scan incomplete.
  bool scan_failed = false;

  // Device fds already handled for the current process, with the node they are
  // opened to: only fds to the same node can refer to the same open file.
  static unsigned seen_fds_capacity = 0;
  static struct {
    int fd;
    dev_t node;
  } *seen_fds = NULL;

  struct dirent *proc_dent;
  while (true) {
    errno = 0;
    proc_dent = readdir(proc_dir);
    if (!proc_dent) {
      if (errno)
        scan_failed = true;
      break;
    }
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
    if (pid_dir_fd < 0) {
      if (!fdinfo_error_is_skippable())
        scan_failed = true;
      continue;
    }

    client_pid = atoi(proc_dent->d_name);
    if (!client_pid)
      goto next;

    fd_dir_fd = openat(pid_dir_fd, "fd", O_DIRECTORY);
    if (fd_dir_fd < 0) {
      if (!fdinfo_error_is_skippable())
        scan_failed = true;
      goto next;
    }

    // Walk the fds and only open the fdinfo of the few owned by a device.
    fd_dir = fdopendir(fd_dir_fd);
    if (!fd_dir) {
      scan_failed = true;
      goto next;
    }

  next_fd:
    while (true) {
      errno = 0;
      fd_dent = readdir(fd_dir);
      if (!fd_dent) {
        // The directory of an exiting process can vanish while it is read.
        if (errno && !fdinfo_error_is_skippable())
          scan_failed = true;
        break;
      }
      struct gpu_process processes_info_local = {0};
      int fd_num;
      dev_t device_node;

      if (!isdigit(fd_dent->d_name[0]))
        continue;

      if (!get_fd_node(fd_dir_fd, fd_dent->d_name, &device_node))
        continue;

      // Only parse fds of monitored devices. A fd owned by no device belongs to a
      // driver without process reporting (or to an unmonitored device).
      struct callback_entry *owner = find_node_owner(device_node);
      if (!owner || !owner->active)
        continue;

      fd_num = atoi(fd_dent->d_name);

      char fdinfo_path[sizeof("fdinfo/") + sizeof(fd_dent->d_name)];
      snprintf(fdinfo_path, sizeof(fdinfo_path), "fdinfo/%s", fd_dent->d_name);
      int fdinfo_fd = openat(pid_dir_fd, fdinfo_path, O_RDONLY);
      if (fdinfo_fd < 0) {
        if (!fdinfo_error_is_skippable())
          mark_fd_failure(owner);
        continue;
      }
      FILE *fdinfo_file = fdopen(fdinfo_fd, "r");
      if (!fdinfo_file) {
        mark_fd_failure(owner);
        close(fdinfo_fd);
        continue;
      }

      // Account for each device client once, in the first process found holding
      // it. This must happen before invoking the callback: callbacks are
      // stateful, updating per-client caches from engine counters and merging
      // the client's memory into a process entry. A duplicate would update the
      // cache twice and double-count usage or memory. The client id also lets
      // the sweep attribute the metrics to one process and list the other
      // holders without them.
      //
      // Current kernels expose drm-client-id, which is more robust across
      // processes than kcmp: kcmp can be unavailable or restricted by a
      // seccomp policy. Older kernels without it fall back to kcmp for
      // duplicates within a process.
      unsigned long long client_id;
      errno = 0;
      bool have_client_id = read_drm_client_id(fdinfo_file, &client_id);
      if (ferror(fdinfo_file)) {
        if (!fdinfo_read_vanished(fdinfo_file))
          mark_fd_failure(owner);
        fclose(fdinfo_file);
        continue;
      }
      if (have_client_id) {
        struct seen_client *seen = find_seen_client(device_node, client_id);
        if (seen) {
          // The metrics of this client are already accounted under another pid.
          // Still list every holder, but leave its GPU fields unknown: usage and
          // memory belong to the shared client, not to an individual process.
          (void)process_entry_for(seen->gpu_info, client_pid);
          fclose(fdinfo_file);
          continue;
        }
      } else {
        // Without a client id, detect duplicated descriptors within the
        // process. If kcmp is unavailable (e.g., denied by a seccomp policy),
        // parse the fd rather than drop a possibly distinct client.
        for (unsigned i = 0; i < seen_fds_len; i++) {
          if (seen_fds[i].node == device_node &&
              syscall(SYS_kcmp, client_pid, client_pid, KCMP_FILE, fd_num, seen_fds[i].fd) == 0) {
            fclose(fdinfo_file);
            goto next_fd;
          }
        }
      }

      struct callback_entry *current_callback = NULL;
      processes_info_local.pid = client_pid;
      if (run_callback(owner, fdinfo_file, &processes_info_local))
        current_callback = owner;
      bool vanished = !current_callback && fdinfo_read_vanished(fdinfo_file);
      fclose(fdinfo_file);
      if (!current_callback && !vanished) {
        // The fd may have been closed and its number reused for another file
        // between the stat and the fdinfo read.
        dev_t current_node;
        vanished = !get_fd_node(fd_dir_fd, fd_dent->d_name, &current_node) || current_node != device_node;
      }
      if (!current_callback) {
        // The device's own fd could not be parsed (e.g., a kernel without fdinfo
        // support for it), so its process list cannot establish zero usage.
        if (!vanished)
          mark_fd_failure(owner);
        continue;
      }
      if (have_client_id)
        record_client(device_node, client_id, current_callback->gpu_info);
      else {
        // Record the fd only after its fdinfo was parsed: a copy that could not
        // be parsed must not suppress a later, valid copy of the same client.
        if (seen_fds_len == seen_fds_capacity) {
          seen_fds_capacity += FD_LINEAR_REALLOC_INC;
          seen_fds = reallocarray(seen_fds, seen_fds_capacity, sizeof(*seen_fds));
          if (!seen_fds) {
            perror("Could not re-allocate memory: ");
            exit(EXIT_FAILURE);
          }
        }
        seen_fds[seen_fds_len].fd = fd_num;
        seen_fds[seen_fds_len].node = device_node;
        seen_fds_len++;
      }

      struct gpu_process *process_info = process_entry_for(current_callback->gpu_info, client_pid);

      process_info->type |= processes_info_local.type;

      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, gpu_memory_usage)) {
        SET_GPUINFO_PROCESS(process_info, gpu_memory_usage,
                            process_info->gpu_memory_usage + processes_info_local.gpu_memory_usage);
      }

      // drm-shared-* is not limited to resident buffers: bound the shared part
      // of each client by its resident memory before merging the clients of a
      // process, and keep the largest one, since the clients of one process can
      // share the same buffers.
      if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, gpu_memory_shared)) {
        unsigned long long client_shared = 0;
        if (GPUINFO_PROCESS_FIELD_VALID(&processes_info_local, gpu_memory_usage))
          client_shared = processes_info_local.gpu_memory_shared < processes_info_local.gpu_memory_usage
                              ? processes_info_local.gpu_memory_shared
                              : processes_info_local.gpu_memory_usage;
        if (!GPUINFO_PROCESS_FIELD_VALID(process_info, gpu_memory_shared))
          process_info->gpu_memory_shared_max = 0;
        if (client_shared > process_info->gpu_memory_shared_max)
          process_info->gpu_memory_shared_max = client_shared;
        SET_GPUINFO_PROCESS(process_info, gpu_memory_shared, process_info->gpu_memory_shared + client_shared);
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

    // Default unknown workload types once all fds of this pid are merged: an
    // idle fd must not mark a process that uses compute on its other fds as
    // graphical. NPU clients are compute by default; GPU clients graphical.
    // (The entry of the pid being scanned is always the last one.)
    for (unsigned callback_idx = 0; callback_idx < registered_callback_entries; ++callback_idx) {
      // Skip disabled callbacks: their parser did not run, so they hold no entry for this pid.
      if (!callback_entries[callback_idx].active)
        continue;
      struct gpu_info *gpu_info = callback_entries[callback_idx].gpu_info;
      if (gpu_info->processes_count > 0) {
        struct gpu_process *process_info = &gpu_info->processes[gpu_info->processes_count - 1];
        if (process_info->pid == (pid_t)client_pid && process_info->type == gpu_process_unknown) {
          bool is_npu = gpu_info->vendor && gpu_info->vendor->processing_unit == gpu_processing_unit_npu;
          process_info->type = is_npu ? gpu_process_compute : gpu_process_graphical;
        }
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
  for (unsigned callback_idx = 0; callback_idx < registered_callback_entries; ++callback_idx) {
    struct callback_entry *current_callback = &callback_entries[callback_idx];
    if (current_callback->active)
      current_callback->gpu_info->processes_query_succeeded = !scan_failed && !current_callback->scan_failed;
  }
}
