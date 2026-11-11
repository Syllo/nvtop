/*
 *
 * Intel NPU (intel_vpu / ivpu accel driver) support for nvtop.
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

#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/time.h"

#include <dirent.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define ACCEL_CLASS "/sys/class/accel"
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
  return strcmp(base ? base + 1 : target, "intel_vpu") == 0;
}

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
  default:
    return NULL;
  }
}

static bool gpuinfo_intel_npu_init(void) { return access(ACCEL_CLASS, R_OK) == 0; }

static void gpuinfo_intel_npu_shutdown(void) {
  free(intel_npu_info);
  intel_npu_info = NULL;
  intel_npu_count = 0;
}

static const char *gpuinfo_intel_npu_last_error_string(void) { return "Intel NPU error"; }

static bool gpuinfo_intel_npu_get_device_handles(struct list_head *devices, unsigned *count) {
  extern struct gpu_vendor gpu_vendor_intel_npu;
  *count = 0;

  DIR *dir = opendir(ACCEL_CLASS);
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
    snprintf(sysfs_dev, sizeof(sysfs_dev), "%s/%s/device", ACCEL_CLASS, entry->d_name);
    if (!is_intel_vpu(sysfs_dev))
      continue;

    struct gpu_info_intel_npu *npu = &intel_npu_info[intel_npu_count];
    snprintf(npu->sysfs_dev, sizeof(npu->sysfs_dev), "%s", sysfs_dev);

    // pdev = PCI address, e.g. 0000:00:0b.0
    char resolved[PATH_MAX];
    if (realpath(sysfs_dev, resolved)) {
      const char *pci = strrchr(resolved, '/');
      snprintf(npu->base.pdev, PDEV_LEN, "%s", pci ? pci + 1 : resolved);
    } else {
      snprintf(npu->base.pdev, PDEV_LEN, "%s", entry->d_name);
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
  if (gen)
    snprintf(static_info->device_name, sizeof(static_info->device_name), "Intel NPU (%s)", gen);
  else
    snprintf(static_info->device_name, sizeof(static_info->device_name), "Intel NPU [8086:%04x]",
             npu->pci_device_id);
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

  // Memory: the NPU allocates from system RAM; show its allocations against total RAM.
  unsigned long long mem_total_kb = 0;
  FILE *fp = fopen("/proc/meminfo", "r");
  if (fp) {
    char line[256];
    while (fgets(line, sizeof(line), fp))
      if (sscanf(line, "MemTotal: %llu kB", &mem_total_kb) == 1)
        break;
    fclose(fp);
  }
  if (mem_total_kb > 0 && read_ull(npu->sysfs_dev, "npu_memory_utilization", &value)) {
    unsigned long long total = mem_total_kb * 1024;
    unsigned long long used = value > total ? total : value;
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, total);
    SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, used);
    SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, total - used);
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, (unsigned)(used * 100 / total));
  }
}

static void gpuinfo_intel_npu_get_running_processes(struct gpu_info *_gpu_info) {
  _gpu_info->processes_count = 0;
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
