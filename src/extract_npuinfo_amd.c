/*
 *
 * AMD XDNA NPU (amdxdna accel driver) support for nvtop.
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

/*
 * The amdxdna driver (Ryzen AI NPUs) exposes /dev/accel/accelN. The driver does not expose a busy time, so no
 * utilization is reported. Processes are found by scanning /proc/<pid>/fd for the accel node of the device, and their
 * memory is read from the DRM fdinfo of that file. The generic fdinfo sweep (extract_processinfo_fdinfo.c) only
 * considers DRM nodes (major 226) and hands every node to all the registered GPU parsers, some of which do not check
 * the driver, so the accel nodes are scanned here instead.
 */

#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"

#include <ctype.h>
#include <dirent.h>
#include <fcntl.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_AMD_NPUS 4
#define AMDXDNA_KERNEL_DRIVER "amdxdna"
#define AMDXDNA_FDINFO_DRIVER "amdxdna_accel_driver"
// Distinct DRM clients (open() calls) of one process that are told apart; more are accounted without deduplication
#define AMDXDNA_MAX_CLIENTS_PER_PID 16

struct gpu_info_amd_npu {
  struct gpu_info base;
  char sysfs_dev[PATH_MAX]; // e.g. /sys/class/accel/accel0/device
  dev_t rdev;               // Device number of /dev/accel/accelN
  bool have_rdev;
  unsigned pci_device_id;
  unsigned pci_revision;
};

// What one fdinfo file of an amdxdna client reports (drivers/accel/amdxdna/amdxdna_pci_drv.c amdxdna_show_fdinfo)
struct amdxdna_fdinfo {
  bool is_amdxdna;
  bool have_client_id;
  unsigned long long client_id;
  bool have_total_memory, have_heap, have_internal, have_external;
  uint64_t total_memory; // drm-total-memory, counts the device BOs twice (once alone, once in the heap BO)
  uint64_t heap;         // Device BOs, sub-allocated from the heap; 0 until a hardware context is set up
  uint64_t internal;     // Driver-internal BOs, including the heap BO
  uint64_t external;     // Imported and user-created BOs
};

// Clients already accounted for the process being scanned
struct amdxdna_pid_clients {
  pid_t pid;
  unsigned count;
  unsigned long long ids[AMDXDNA_MAX_CLIENTS_PER_PID];
};

static const char *amd_npu_class_root = "/sys/class/accel";
static const char *amd_npu_proc_root = "/proc";

static struct gpu_info_amd_npu *amd_npu_info = NULL;
static unsigned amd_npu_count = 0;

extern struct gpu_vendor gpu_vendor_amd_npu;

static bool read_str(const char *dir, const char *file, char *buf, size_t len) {
  char path[PATH_MAX];
  snprintf(path, sizeof(path), "%s/%s", dir, file);
  FILE *fp = fopen(path, "r");
  if (!fp)
    return false;
  bool ok = fgets(buf, (int)len, fp) != NULL;
  fclose(fp);
  if (ok)
    buf[strcspn(buf, "\n")] = '\0';
  return ok;
}

// The accel device belongs to AMD's NPU driver if its driver symlink ends in "amdxdna".
static bool is_amdxdna(const char *sysfs_dev) {
  char link[PATH_MAX], target[PATH_MAX];
  snprintf(link, sizeof(link), "%s/driver", sysfs_dev);
  ssize_t n = readlink(link, target, sizeof(target) - 1);
  if (n <= 0)
    return false;
  target[n] = '\0';
  const char *base = strrchr(target, '/');
  return strcmp(base ? base + 1 : target, AMDXDNA_KERNEL_DRIVER) == 0;
}

struct amd_npu_model {
  unsigned device_id;
  unsigned revision;
  const char *architecture; // Name of the configuration the kernel selects (dev_npuN_info)
  const char *codename;     // Only when this (device, revision) pair maps to a single product family
};

// The (device, revision) pairs of amdxdna_ids[] in drivers/accel/amdxdna/amdxdna_pci_drv.c. The driver refuses to bind
// to any other pair. The product behind 0x17f0 is not given by the PCI revision: the kernel names it from the revision
// reported by the firmware (npu4_rev_vbnv_tbl) and exposes that name in the vbnv sysfs attribute. A Krackan NPU, for
// instance, can show PCI revision 0x20 (NPU6).
static const struct amd_npu_model amd_npu_models[] = {
    {0x1502, 0x00, "NPU1", "Phoenix/Hawk Point"},
    {0x17f0, 0x10, "NPU4", NULL},
    {0x17f0, 0x11, "NPU5", NULL},
    {0x17f0, 0x20, "NPU6", NULL},
    {0x17f2, 0x10, "NPU3", NULL},
    {0x17f3, 0x10, "NPU3", NULL},
    {0x1b0b, 0x10, "NPU3", NULL},
    {0x1b0c, 0x10, "NPU3", NULL},
};

static const struct amd_npu_model *amd_npu_lookup_model(unsigned device_id, unsigned revision) {
  for (size_t i = 0; i < sizeof(amd_npu_models) / sizeof(*amd_npu_models); ++i) {
    if (amd_npu_models[i].device_id == device_id && amd_npu_models[i].revision == revision)
      return &amd_npu_models[i];
  }
  return NULL;
}

// Name the device from the vbnv sysfs attribute when the firmware identified the product ("NPU Strix Halo"), else from
// the PCI IDs. The driver's fallback vbnv ("RyzenAI-npu4") only repeats the configuration and is not used.
static void amd_npu_device_name(const char *vbnv, unsigned device_id, unsigned revision, char *name, size_t len) {
  static const char product_prefix[] = "NPU ";
  const struct amd_npu_model *model = amd_npu_lookup_model(device_id, revision);
  if (vbnv && !strncmp(vbnv, product_prefix, sizeof(product_prefix) - 1) && vbnv[sizeof(product_prefix) - 1] != '\0')
    snprintf(name, len, "AMD NPU (%s)", vbnv + sizeof(product_prefix) - 1);
  else if (model && model->codename)
    snprintf(name, len, "AMD NPU (%s)", model->codename);
  else
    snprintf(name, len, "AMD NPU [1022:%04x]", device_id);
}

// Parse a size printed by drm_fdinfo_print_size() or drm_print_memory_stats(): a number of bytes, KiB or MiB.
static bool parse_fdinfo_size(const char *val, uint64_t *size) {
  char *endptr;
  unsigned long long value = strtoull(val, &endptr, 10);
  if (endptr == val)
    return false;
  while (isspace((unsigned char)*endptr))
    endptr++;
  uint64_t unit;
  if (*endptr == '\0')
    unit = 1;
  else if (!strcmp(endptr, "KiB") || !strcmp(endptr, "kB"))
    unit = UINT64_C(1) << 10;
  else if (!strcmp(endptr, "MiB"))
    unit = UINT64_C(1) << 20;
  else if (!strcmp(endptr, "GiB"))
    unit = UINT64_C(1) << 30;
  else
    return false;
  *size = (uint64_t)value * unit;
  return true;
}

static bool amdxdna_parse_fdinfo(FILE *fdinfo_file, struct amdxdna_fdinfo *info) {
  static char *line = NULL;
  static size_t line_buf_size = 0;
  ssize_t count;

  memset(info, 0, sizeof(*info));
  while ((count = getline(&line, &line_buf_size, fdinfo_file)) != -1) {
    char *key, *val;
    if (count > 0 && line[count - 1] == '\n')
      line[--count] = '\0';
    if (!extract_drm_fdinfo_key_value(line, &key, &val))
      continue;

    if (!strcmp(key, "drm-driver")) {
      info->is_amdxdna = !strcmp(val, AMDXDNA_FDINFO_DRIVER);
    } else if (!strcmp(key, drm_client_id)) {
      char *endptr;
      info->client_id = strtoull(val, &endptr, 10);
      info->have_client_id = endptr != val && *endptr == '\0';
    } else if (!strcmp(key, "drm-total-memory")) {
      info->have_total_memory = parse_fdinfo_size(val, &info->total_memory);
    } else if (!strcmp(key, AMDXDNA_FDINFO_DRIVER "-heap-alloc")) {
      info->have_heap = parse_fdinfo_size(val, &info->heap);
    } else if (!strcmp(key, AMDXDNA_FDINFO_DRIVER "-internal-alloc")) {
      info->have_internal = parse_fdinfo_size(val, &info->internal);
    } else if (!strcmp(key, AMDXDNA_FDINFO_DRIVER "-external-alloc")) {
      info->have_external = parse_fdinfo_size(val, &info->external);
    }
  }
  return info->is_amdxdna;
}

// Memory held by the client. The driver documents internal + external as the total without double counting, while
// drm-total-memory counts the device BOs twice; use the latter only when the driver-specific keys are missing.
static bool amdxdna_client_memory(const struct amdxdna_fdinfo *info, uint64_t *memory) {
  if (info->have_internal && info->have_external) {
    *memory = info->internal + info->external;
    return true;
  }
  if (info->have_total_memory) {
    *memory = info->total_memory;
    return true;
  }
  return false;
}

// A client that set up a hardware context to submit work has a device heap. A client that only opened the device
// (e.g. a framework enumerating every accelerator) has none.
static bool amdxdna_client_active(const struct amdxdna_fdinfo *info) { return info->have_heap && info->heap > 0; }

// Account one client of the NPU to the process list of the device. The processes are scanned one after the other, so
// the entry of the process being scanned is always the last one.
static void amd_npu_account_client(struct gpu_info *gpu_info, struct amdxdna_pid_clients *clients, pid_t pid,
                                   const struct amdxdna_fdinfo *info) {
  if (clients->pid != pid) {
    clients->pid = pid;
    clients->count = 0;
  }
  // Duplicated file descriptors (dup, fork, SCM_RIGHTS) share one DRM client: count it once
  if (info->have_client_id) {
    for (unsigned i = 0; i < clients->count; ++i) {
      if (clients->ids[i] == info->client_id)
        return;
    }
    if (clients->count < AMDXDNA_MAX_CLIENTS_PER_PID)
      clients->ids[clients->count++] = info->client_id;
  }

  struct gpu_process *process = NULL;
  if (gpu_info->processes_count > 0 && gpu_info->processes[gpu_info->processes_count - 1].pid == pid) {
    process = &gpu_info->processes[gpu_info->processes_count - 1];
  } else {
    if (gpu_info->processes_count == gpu_info->processes_array_size) {
      gpu_info->processes_array_size += COMMON_PROCESS_LINEAR_REALLOC_INC;
      gpu_info->processes =
          reallocarray(gpu_info->processes, gpu_info->processes_array_size, sizeof(*gpu_info->processes));
      if (!gpu_info->processes) {
        perror("Could not re-allocate memory: ");
        exit(EXIT_FAILURE);
      }
    }
    process = &gpu_info->processes[gpu_info->processes_count++];
    memset(process, 0, sizeof(*process));
    process->pid = pid;
    process->type = gpu_process_unknown;
  }

  uint64_t memory;
  if (amdxdna_client_memory(info, &memory)) {
    unsigned long long previous =
        GPUINFO_PROCESS_FIELD_VALID(process, gpu_memory_usage) ? process->gpu_memory_usage : 0;
    SET_GPUINFO_PROCESS(process, gpu_memory_usage, previous + memory);
  }
  // nvtop's process model has no "idle" kind: a process that submitted work is a compute process, one that only holds
  // the device open keeps the unknown type.
  if (amdxdna_client_active(info))
    process->type = gpu_process_compute;
}

// Scan /proc for the processes holding the accel node of this NPU open
static void amd_npu_scan_processes(struct gpu_info_amd_npu *npu) {
  struct amdxdna_pid_clients clients = {.pid = 0, .count = 0};

  DIR *proc_dir = opendir(amd_npu_proc_root);
  if (!proc_dir)
    return;

  struct dirent *proc_dent;
  while ((proc_dent = readdir(proc_dir)) != NULL) {
    if (!isdigit((unsigned char)proc_dent->d_name[0]))
      continue;
    pid_t pid = (pid_t)atoi(proc_dent->d_name);
    if (pid <= 0)
      continue;

    int pid_dir_fd = openat(dirfd(proc_dir), proc_dent->d_name, O_DIRECTORY | O_RDONLY);
    if (pid_dir_fd < 0)
      continue;
    int fd_dir_fd = openat(pid_dir_fd, "fd", O_DIRECTORY | O_RDONLY);
    int fdinfo_dir_fd = openat(pid_dir_fd, "fdinfo", O_DIRECTORY | O_RDONLY);
    close(pid_dir_fd);
    DIR *fd_dir = fd_dir_fd >= 0 ? fdopendir(fd_dir_fd) : NULL;
    if (!fd_dir || fdinfo_dir_fd < 0) {
      if (fd_dir)
        closedir(fd_dir);
      else if (fd_dir_fd >= 0)
        close(fd_dir_fd);
      if (fdinfo_dir_fd >= 0)
        close(fdinfo_dir_fd);
      continue;
    }

    struct dirent *fd_dent;
    while ((fd_dent = readdir(fd_dir)) != NULL) {
      if (!isdigit((unsigned char)fd_dent->d_name[0]))
        continue;
      // Follows the fd magic link, which resolves even for a process in another mount namespace
      struct stat st;
      if (fstatat(dirfd(fd_dir), fd_dent->d_name, &st, 0) != 0 || !S_ISCHR(st.st_mode) || st.st_rdev != npu->rdev)
        continue;

      int fdinfo_fd = openat(fdinfo_dir_fd, fd_dent->d_name, O_RDONLY);
      if (fdinfo_fd < 0)
        continue;
      FILE *fdinfo_file = fdopen(fdinfo_fd, "r");
      if (!fdinfo_file) {
        close(fdinfo_fd);
        continue;
      }
      struct amdxdna_fdinfo info;
      if (amdxdna_parse_fdinfo(fdinfo_file, &info))
        amd_npu_account_client(&npu->base, &clients, pid, &info);
      fclose(fdinfo_file);
    }
    closedir(fd_dir);
    close(fdinfo_dir_fd);
  }
  closedir(proc_dir);
}

static bool gpuinfo_amd_npu_init(void) { return access(amd_npu_class_root, R_OK) == 0; }

// The process arrays are freed by gpuinfo_shutdown_info_extraction() before the vendors shut down
static void gpuinfo_amd_npu_shutdown(void) {
  free(amd_npu_info);
  amd_npu_info = NULL;
  amd_npu_count = 0;
}

static const char *gpuinfo_amd_npu_last_error_string(void) { return "AMD NPU error"; }

static bool gpuinfo_amd_npu_get_device_handles(struct list_head *devices, unsigned *count) {
  *count = 0;
  if (amd_npu_info)
    return false;

  DIR *dir = opendir(amd_npu_class_root);
  if (!dir)
    return false;

  amd_npu_info = calloc(MAX_AMD_NPUS, sizeof(*amd_npu_info));
  if (!amd_npu_info) {
    closedir(dir);
    return false;
  }

  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL && amd_npu_count < MAX_AMD_NPUS) {
    if (strncmp(entry->d_name, "accel", 5) != 0)
      continue;
    char sysfs_dev[PATH_MAX];
    snprintf(sysfs_dev, sizeof(sysfs_dev), "%s/%s/device", amd_npu_class_root, entry->d_name);
    if (!is_amdxdna(sysfs_dev))
      continue;

    struct gpu_info_amd_npu *npu = &amd_npu_info[amd_npu_count];
    snprintf(npu->sysfs_dev, sizeof(npu->sysfs_dev), "%s", sysfs_dev);

    // pdev = PCI address, e.g. 0000:c5:00.1
    char resolved[PATH_MAX];
    if (realpath(sysfs_dev, resolved)) {
      const char *pci = strrchr(resolved, '/');
      snprintf(npu->base.pdev, PDEV_LEN, "%.*s", PDEV_LEN - 1, pci ? pci + 1 : resolved);
    } else {
      snprintf(npu->base.pdev, PDEV_LEN, "%.*s", PDEV_LEN - 1, entry->d_name);
    }

    char str[32];
    if (read_str(sysfs_dev, "device", str, sizeof(str)))
      npu->pci_device_id = (unsigned)strtoul(str, NULL, 16);
    if (read_str(sysfs_dev, "revision", str, sizeof(str)))
      npu->pci_revision = (unsigned)strtoul(str, NULL, 16);

    // Device number of the accel node ("major:minor"), to find the processes holding it open
    char class_dev[PATH_MAX];
    unsigned major_num, minor_num;
    snprintf(class_dev, sizeof(class_dev), "%s/%s", amd_npu_class_root, entry->d_name);
    if (read_str(class_dev, "dev", str, sizeof(str)) && sscanf(str, "%u:%u", &major_num, &minor_num) == 2) {
      npu->rdev = makedev(major_num, minor_num);
      npu->have_rdev = true;
    }

    npu->base.vendor = &gpu_vendor_amd_npu;
    npu->base.processes_count = 0;
    npu->base.processes = NULL;
    npu->base.processes_array_size = 0;
    list_add_tail(&npu->base.list, devices);
    amd_npu_count++;
  }
  closedir(dir);

  *count = amd_npu_count;
  return true;
}

static void gpuinfo_amd_npu_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_amd_npu *npu = container_of(_gpu_info, struct gpu_info_amd_npu, base);
  struct gpuinfo_static_info *static_info = &npu->base.static_info;

  RESET_ALL(static_info->valid);
  static_info->integrated_graphics = true;
  static_info->encode_decode_shared = false;
  static_info->memory_shared_with_host = true;

  char vbnv[64];
  bool have_vbnv = read_str(npu->sysfs_dev, "vbnv", vbnv, sizeof(vbnv));
  amd_npu_device_name(have_vbnv ? vbnv : NULL, npu->pci_device_id, npu->pci_revision, static_info->device_name,
                      sizeof(static_info->device_name));
  SET_VALID(gpuinfo_device_name_valid, static_info->valid);
  const struct amd_npu_model *model = amd_npu_lookup_model(npu->pci_device_id, npu->pci_revision);
  if (model) {
    snprintf(static_info->device_architecture, sizeof(static_info->device_architecture), "%s", model->architecture);
    SET_VALID(gpuinfo_device_architecture_valid, static_info->valid);
  }
}

static void gpuinfo_amd_npu_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_amd_npu *npu = container_of(_gpu_info, struct gpu_info_amd_npu, base);
  struct gpuinfo_dynamic_info *dynamic_info = &npu->base.dynamic_info;

  RESET_ALL(dynamic_info->valid);
  // The in-tree driver exposes no busy time: the utilization stays unset rather than 0%.
  // The NPU allocates from system RAM, so report system-wide memory pressure.
  gpuinfo_refresh_system_memory(dynamic_info);
}

static void gpuinfo_amd_npu_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_amd_npu *npu = container_of(_gpu_info, struct gpu_info_amd_npu, base);
  _gpu_info->processes_count = 0;
  if (npu->have_rdev)
    amd_npu_scan_processes(npu);
}

struct gpu_vendor gpu_vendor_amd_npu = {.init = gpuinfo_amd_npu_init,
                                        .shutdown = gpuinfo_amd_npu_shutdown,
                                        .last_error_string = gpuinfo_amd_npu_last_error_string,
                                        .get_device_handles = gpuinfo_amd_npu_get_device_handles,
                                        .populate_static_info = gpuinfo_amd_npu_populate_static_info,
                                        .refresh_dynamic_info = gpuinfo_amd_npu_refresh_dynamic_info,
                                        .refresh_running_processes = gpuinfo_amd_npu_get_running_processes,
                                        .name = "AMD-NPU",
                                        .processing_unit = gpu_processing_unit_npu};

__attribute__((constructor)) static void init_extract_npuinfo_amd(void) { register_gpu_vendor(&gpu_vendor_amd_npu); }
