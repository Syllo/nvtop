/*
 *
 * Copyright (C) 2026 Jukrapope Jitpimolmard <jukrapope@kku.ac.th>
 *
 * Intel NPU (intel_vpu / ivpu accel driver) support for nvtop.
 *
 * The driver exposes /dev/accel/accelN. Kernels since 6.19 print the memory of
 * each client in the DRM fdinfo of that node; this backend registers the node
 * with the generic sweep to list the processes holding it open and the memory
 * each one allocated. The driver exposes no per-client busy time, so the
 * utilization stays device-wide.
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

#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/time.h"

#include <dirent.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define ACCEL_DEV_ROOT "/dev/accel"
#define IVPU_DRIVER "intel_vpu"
#define MAX_INTEL_NPUS 4

struct gpu_info_intel_npu {
  struct gpu_info base;
  char sysfs_dev[PATH_MAX]; // e.g. /sys/class/accel/accel0/device
  unsigned pci_device_id;
  bool have_prev_sample;
  unsigned long long prev_busy_us;
  nvtop_time prev_time;
};

static struct gpu_info_intel_npu *intel_npu_info = NULL;
static unsigned intel_npu_count = 0;

static const char *intel_npu_class_root = "/sys/class/accel";
static const char *intel_npu_dev_root = ACCEL_DEV_ROOT;

extern struct gpu_vendor gpu_vendor_intel_npu;

static bool read_ull(const char *dir, const char *file, unsigned long long *value) {
  char path[PATH_MAX];
  snprintf(path, sizeof(path), "%s/%s", dir, file);
  FILE *fp = fopen(path, "r");
  if (!fp)
    return false;
  bool ok = fscanf(fp, "%llu", value) == 1;
  fclose(fp);
  return ok;
}

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

// The accel device belongs to Intel's NPU driver if its driver symlink ends in "intel_vpu".
static bool is_intel_vpu(const char *sysfs_dev) {
  char link[PATH_MAX], target[PATH_MAX];
  snprintf(link, sizeof(link), "%s/driver", sysfs_dev);
  ssize_t n = readlink(link, target, sizeof(target) - 1);
  if (n <= 0)
    return false;
  target[n] = '\0';
  const char *base = strrchr(target, '/');
  return strcmp(base ? base + 1 : target, IVPU_DRIVER) == 0;
}

// What one fdinfo file of an intel_vpu client reports (drm_show_fdinfo and
// drm_show_memory_stats). Kernels before 6.19 print no memory keys; newer ones
// always print drm-total-memory and drm-shared-memory, but drm-resident-memory
// only once one of the buffers of the client is resident: drm_show_memory_stats
// derives the keys it prints from the status of each buffer, not from what the
// driver supports.
struct intel_vpu_fdinfo {
  bool is_intel_vpu;
  bool have_total, have_resident, have_shared;
  unsigned long long total;    // drm-total-memory, private + shared
  unsigned long long resident; // drm-resident-memory, buffers backed by pages (ivpu_bo_is_resident)
  unsigned long long shared;   // drm-shared-memory, buffers exported or imported
};

// Only an fdinfo naming the intel_vpu driver is accepted. Kernels whose accel
// fops lack show_fdinfo print no drm-driver line: the client cannot be
// identified, so the sweep fails and the memory row falls back to system-wide
// RAM rather than claiming an idle NPU. A client naming intel_vpu without
// memory keys is accepted and listed without memory.
static bool intel_vpu_parse_fdinfo(FILE *fdinfo_file, struct intel_vpu_fdinfo *info) {
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
      info->is_intel_vpu = !strcmp(val, IVPU_DRIVER);
    } else if (!strcmp(key, "drm-total-memory")) {
      info->have_total = extract_drm_fdinfo_size(val, &info->total);
    } else if (!strcmp(key, "drm-resident-memory")) {
      info->have_resident = extract_drm_fdinfo_size(val, &info->resident);
    } else if (!strcmp(key, "drm-shared-memory")) {
      info->have_shared = extract_drm_fdinfo_size(val, &info->shared);
    }
  }
  return info->is_intel_vpu;
}

// Memory held by the client. drm-shared-memory counts both the buffers this
// client exported, which are resident and already in drm-resident-memory, and
// those it imported, whose pages belong to the exporting device and are not
// resident here. Neither is reported apart, so the resident counter alone is the
// usage: adding the shared part would count exported buffers twice, and imported
// ones are accounted by their exporter. The shared part is capped at the usage
// because only the resident part of it can be included there; with imports it
// may still overstate the shared part, making the private part a lower bound.
// Without drm-resident-memory, none of the buffers of the client is resident:
// ivpu reports the residency of every buffer (ivpu_gem_status) since it prints
// memory statistics. drm-total-memory is no substitute: it also counts imported
// buffers and buffers not backed by pages yet.
static bool intel_vpu_client_memory(const struct intel_vpu_fdinfo *info, uint64_t *memory, uint64_t *shared) {
  if (!info->have_total)
    return false;
  uint64_t usage = info->have_resident ? info->resident : 0;
  uint64_t shared_memory = info->have_shared ? info->shared : 0;
  *memory = usage;
  *shared = shared_memory > usage ? usage : shared_memory;
  return true;
}

static bool parse_drm_fdinfo_intel_vpu(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {
  (void)info;
  struct intel_vpu_fdinfo client;
  if (!intel_vpu_parse_fdinfo(fdinfo_file, &client))
    return false;

  uint64_t memory, shared;
  // Kernels before 6.19 print no memory keys for intel_vpu clients.
  if (intel_vpu_client_memory(&client, &memory, &shared)) {
    SET_GPUINFO_PROCESS(process_info, gpu_memory_usage, memory);
    if (shared)
      SET_GPUINFO_PROCESS(process_info, gpu_memory_shared, shared);
  }
  // NPU work is compute by definition; the generic sweep also defaults unknown
  // NPU process types to compute.
  process_info->type |= gpu_process_compute;
  return true;
}

// PCI IDs handled by the in-kernel intel_vpu driver (drivers/accel/ivpu/ivpu_drv.h).
static const char *npu_generation(unsigned device_id) {
  switch (device_id) {
  case 0x7d1d:
    return "Meteor Lake";
  case 0xad1d:
    return "Arrow Lake";
  case 0x643e:
    return "Lunar Lake";
  case 0xb03e:
    return "Panther Lake";
  case 0xfd3e:
    return "Wildcat Lake";
  case 0xd71d:
    return "Nova Lake";
  default:
    return NULL;
  }
}

static bool gpuinfo_intel_npu_init(void) { return access(intel_npu_class_root, R_OK) == 0; }

static void gpuinfo_intel_npu_shutdown(void) {
  // The fdinfo sweep keeps a pointer to each registered device: drop them before
  // freeing the array, or a later sweep writes through the freed memory.
  for (unsigned i = 0; i < intel_npu_count; ++i)
    processinfo_drop_callback(&intel_npu_info[i].base);
  free(intel_npu_info);
  intel_npu_info = NULL;
  intel_npu_count = 0;
}

static const char *gpuinfo_intel_npu_last_error_string(void) { return "Intel NPU error"; }

static bool gpuinfo_intel_npu_get_device_handles(struct list_head *devices, unsigned *count) {
  *count = 0;
  if (intel_npu_info)
    return false;

  DIR *dir = opendir(intel_npu_class_root);
  if (!dir)
    return false;

  intel_npu_info = calloc(MAX_INTEL_NPUS, sizeof(*intel_npu_info));
  if (!intel_npu_info) {
    closedir(dir);
    return false;
  }

  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL && intel_npu_count < MAX_INTEL_NPUS) {
    if (strncmp(entry->d_name, "accel", 5) != 0)
      continue;
    char sysfs_dev[PATH_MAX];
    snprintf(sysfs_dev, sizeof(sysfs_dev), "%s/%s/device", intel_npu_class_root, entry->d_name);
    if (!is_intel_vpu(sysfs_dev))
      continue;

    struct gpu_info_intel_npu *npu = &intel_npu_info[intel_npu_count];
    snprintf(npu->sysfs_dev, sizeof(npu->sysfs_dev), "%s", sysfs_dev);

    // pdev = PCI address, e.g. 0000:00:0b.0
    char resolved[PATH_MAX];
    if (realpath(sysfs_dev, resolved)) {
      const char *pci = strrchr(resolved, '/');
      snprintf(npu->base.pdev, PDEV_LEN, "%.*s", PDEV_LEN - 1, pci ? pci + 1 : resolved);
    } else {
      snprintf(npu->base.pdev, PDEV_LEN, "%.*s", PDEV_LEN - 1, entry->d_name);
    }

    unsigned long long dev_id = 0;
    char id_str[16];
    if (read_str(sysfs_dev, "device", id_str, sizeof(id_str)))
      dev_id = strtoull(id_str, NULL, 16);
    npu->pci_device_id = (unsigned)dev_id;

    npu->base.vendor = &gpu_vendor_intel_npu;
    npu->base.processes_count = 0;
    npu->base.processes = NULL;
    npu->base.processes_array_size = 0;

    // The fdinfo dispatcher attributes processes through the accel node. If the
    // node is missing or the user cannot read it (without the render group,
    // say), no processes are reported, but the device is still listed: sysfs
    // keeps exposing its utilization and clocks. Check the permission with
    // access() rather than opening the node: an open creates a driver client,
    // which takes a context from the per-user limit, and its release resumes a
    // runtime-suspended NPU. A full context limit would also fail the open and
    // disable monitoring for the whole session.
    char node_path[PATH_MAX];
    snprintf(node_path, sizeof(node_path), "%s/%s", intel_npu_dev_root, entry->d_name);
    // Without the registration the sweep never reports the device, so its
    // process query never succeeds and the memory row falls back to RAM.
    if (!access(node_path, R_OK))
      processinfo_register_fdinfo_callback(parse_drm_fdinfo_intel_vpu, &npu->base, node_path);
    list_add_tail(&npu->base.list, devices);
    intel_npu_count++;
  }
  closedir(dir);

  *count = intel_npu_count;
  return true;
}

static void gpuinfo_intel_npu_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_intel_npu *npu = container_of(_gpu_info, struct gpu_info_intel_npu, base);
  struct gpuinfo_static_info *static_info = &npu->base.static_info;

  RESET_ALL(static_info->valid);
  static_info->integrated_graphics = true;
  static_info->encode_decode_shared = false;
  static_info->memory_shared_with_host = true;

  const char *gen = npu_generation(npu->pci_device_id);
  if (gen) {
    snprintf(static_info->device_name, sizeof(static_info->device_name), "Intel NPU (%s)", gen);
    snprintf(static_info->device_architecture, sizeof(static_info->device_architecture), "%s", gen);
    SET_VALID(gpuinfo_device_architecture_valid, static_info->valid);
  } else {
    snprintf(static_info->device_name, sizeof(static_info->device_name), "Intel NPU [8086:%04x]", npu->pci_device_id);
  }
  SET_VALID(gpuinfo_device_name_valid, static_info->valid);
}

static void gpuinfo_intel_npu_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_intel_npu *npu = container_of(_gpu_info, struct gpu_info_intel_npu, base);
  struct gpuinfo_dynamic_info *dynamic_info = &npu->base.dynamic_info;
  unsigned long long value;

  RESET_ALL(dynamic_info->valid);

  // Utilization: npu_busy_time_us is a cumulative busy counter; divide its growth by wall time.
  nvtop_time now;
  nvtop_get_current_time(&now);
  if (read_ull(npu->sysfs_dev, "npu_busy_time_us", &value)) {
    if (npu->have_prev_sample && value >= npu->prev_busy_us) {
      uint64_t elapsed_ns = nvtop_difftime_u64(npu->prev_time, now);
      if (elapsed_ns > 0) {
        uint64_t busy_ns = (uint64_t)(value - npu->prev_busy_us) * UINT64_C(1000);
        unsigned rate = busy_usage_from_time_usage_round(busy_ns, 0, elapsed_ns);
        SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, rate > 100 ? 100 : rate);
      }
    }
    npu->prev_busy_us = value;
    npu->prev_time = now;
    npu->have_prev_sample = true;
  }

  // Clock: reported as 0 MHz while the NPU is power-gated (D3hot).
  if (read_ull(npu->sysfs_dev, "npu_current_frequency_mhz", &value))
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, (unsigned)value);
  if (read_ull(npu->sysfs_dev, "npu_max_frequency_mhz", &value))
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, (unsigned)value);

  // The NPU allocates from system RAM. The process sweep supplies the NPU
  // allocations and MemAvailable the headroom, so the memory row reports the
  // device allocation plus the memory still available.
  gpuinfo_refresh_system_memory(dynamic_info);
}

static void gpuinfo_intel_npu_get_running_processes(struct gpu_info *_gpu_info) {
  // The generic fdinfo sweep already filled the process list; there is no
  // per-client cache to rotate. Without privileges the sweep only sees the
  // processes of the user, so the row reports the NPU usage of that user.
  // A sweep that failed, or that never covered the device (an unregistered
  // node), leaves the query unsuccessful: the row falls back to system RAM.
  struct gpu_info_intel_npu *npu = container_of(_gpu_info, struct gpu_info_intel_npu, base);
  if (!npu->base.processes_query_succeeded)
    return;
  // A complete sweep in which no client reports memory (none found, or clients
  // whose fdinfo has no memory keys) reports nothing allocated on the NPU. A
  // client whose fdinfo cannot be parsed (no drm-driver) fails the sweep and
  // the row falls back to system-wide RAM.
  for (unsigned i = 0; i < npu->base.processes_count; ++i)
    if (GPUINFO_PROCESS_FIELD_VALID(&npu->base.processes[i], gpu_memory_usage))
      return;
  struct gpuinfo_dynamic_info *dynamic_info = &npu->base.dynamic_info;
  if (GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, used_memory) ||
      !GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, system_free_memory))
    return;
  SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, 0);
  SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, dynamic_info->system_free_memory);
  SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, dynamic_info->system_free_memory);
  SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, 0);
}

struct gpu_vendor gpu_vendor_intel_npu = {.init = gpuinfo_intel_npu_init,
                                          .shutdown = gpuinfo_intel_npu_shutdown,
                                          .last_error_string = gpuinfo_intel_npu_last_error_string,
                                          .get_device_handles = gpuinfo_intel_npu_get_device_handles,
                                          .populate_static_info = gpuinfo_intel_npu_populate_static_info,
                                          .refresh_dynamic_info = gpuinfo_intel_npu_refresh_dynamic_info,
                                          .refresh_running_processes = gpuinfo_intel_npu_get_running_processes,
                                          .name = "Intel-NPU",
                                          .processing_unit = gpu_processing_unit_npu};

__attribute__((constructor)) static void init_extract_npuinfo_intel(void) {
  register_gpu_vendor(&gpu_vendor_intel_npu);
}
