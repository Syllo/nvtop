/*
 *
 * Copyright (C) 2026 Sungjoon Moon <sumoon at seoulsaram dot com>
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

#include "nvtop/extract_gpuinfo_common.h"

#include <dirent.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define TT_SYSFS_CLASS "/sys/class/tenstorrent"
#define TT_SYSFS_PREFIX "tenstorrent!"
#define TT_PATH_MAX 512

struct gpu_info_tenstorrent {
  struct gpu_info base;
  char sysfs_device_path[TT_PATH_MAX]; // /sys/class/tenstorrent/tenstorrent!N
  char pci_device_path[TT_PATH_MAX];   // /sys/devices/.../0000:XX:YY.Z
  char hwmon_path[TT_PATH_MAX];        // .../hwmon/hwmonN
  unsigned device_id;
  unsigned ordinal;
  bool pcie_counters_valid;
  unsigned long long last_pcie_rx;
  unsigned long long last_pcie_tx;
  struct timespec last_counter_time;
};

static struct gpu_info_tenstorrent *tt_devices;
extern struct gpu_vendor gpu_vendor_tenstorrent;

static bool read_sysfs_u32(const char *path, unsigned *value) {
  FILE *fp = fopen(path, "r");
  if (!fp)
    return false;
  bool ok = fscanf(fp, "%u", value) == 1;
  fclose(fp);
  return ok;
}

static bool read_sysfs_long(const char *path, long *value) {
  FILE *fp = fopen(path, "r");
  if (!fp)
    return false;
  bool ok = fscanf(fp, "%ld", value) == 1;
  fclose(fp);
  return ok;
}

static bool find_hwmon_path(const char *pci_path, char *hwmon_path, size_t size) {
  char hwmon_dir[PATH_MAX];
  snprintf(hwmon_dir, sizeof(hwmon_dir), "%s/hwmon", pci_path);

  DIR *dir = opendir(hwmon_dir);
  if (!dir)
    return false;

  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL) {
    if (strncmp(entry->d_name, "hwmon", 5) == 0 && entry->d_name[5] != '\0') {
      snprintf(hwmon_path, size, "%s/%s", hwmon_dir, entry->d_name);
      closedir(dir);
      return true;
    }
  }
  closedir(dir);
  return false;
}

static const char *chip_name_from_device_id(unsigned device_id) {
  switch (device_id) {
  case 0x401e:
    return "Wormhole";
  case 0xb140:
    return "Blackhole";
  case 0xfaca:
    return "Grayskull";
  default:
    return "Tenstorrent";
  }
}

static bool gpuinfo_tenstorrent_init(void) {
  return access(TT_SYSFS_CLASS, R_OK | X_OK) == 0;
}

static void gpuinfo_tenstorrent_shutdown(void) {
  free(tt_devices);
  tt_devices = NULL;
}

static const char *gpuinfo_tenstorrent_last_error_string(void) {
  return "Tenstorrent error";
}

static bool gpuinfo_tenstorrent_get_device_handles(struct list_head *devices, unsigned *count) {
  *count = 0;

  DIR *dir = opendir(TT_SYSFS_CLASS);
  if (!dir)
    return false;

  unsigned num_devices = 0;
  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL) {
    if (strncmp(entry->d_name, TT_SYSFS_PREFIX, sizeof(TT_SYSFS_PREFIX) - 1) == 0)
      num_devices++;
  }

  if (num_devices == 0) {
    closedir(dir);
    return false;
  }

  tt_devices = calloc(num_devices, sizeof(*tt_devices));
  if (!tt_devices) {
    closedir(dir);
    return false;
  }

  rewinddir(dir);

  while ((entry = readdir(dir)) != NULL) {
    if (strncmp(entry->d_name, TT_SYSFS_PREFIX, sizeof(TT_SYSFS_PREFIX) - 1) != 0)
      continue;
    if (*count >= num_devices)
      break;

    struct gpu_info_tenstorrent *dev = &tt_devices[*count];

    dev->ordinal = (unsigned)atoi(entry->d_name + sizeof(TT_SYSFS_PREFIX) - 1);

    snprintf(dev->sysfs_device_path, sizeof(dev->sysfs_device_path),
             "%s/%s", TT_SYSFS_CLASS, entry->d_name);

    char device_link[PATH_MAX];
    snprintf(device_link, sizeof(device_link), "%s/device", dev->sysfs_device_path);
    char resolved[PATH_MAX];
    if (!realpath(device_link, resolved))
      continue;
    strncpy(dev->pci_device_path, resolved, sizeof(dev->pci_device_path) - 1);
    dev->pci_device_path[sizeof(dev->pci_device_path) - 1] = '\0';

    dev->hwmon_path[0] = '\0';
    find_hwmon_path(dev->pci_device_path, dev->hwmon_path, sizeof(dev->hwmon_path));

    char id_path[PATH_MAX];
    snprintf(id_path, sizeof(id_path), "%s/device", dev->pci_device_path);
    FILE *fp = fopen(id_path, "r");
    if (fp) {
      if (fscanf(fp, "%x", &dev->device_id) != 1)
        dev->device_id = 0;
      fclose(fp);
    }

    const char *bdf = strrchr(dev->pci_device_path, '/');
    bdf = bdf ? bdf + 1 : dev->pci_device_path;
    strncpy(dev->base.pdev, bdf, PDEV_LEN - 1);
    dev->base.pdev[PDEV_LEN - 1] = '\0';

    dev->base.vendor = &gpu_vendor_tenstorrent;
    dev->base.processes_count = 0;
    dev->base.processes = NULL;
    dev->base.processes_array_size = 0;
    list_add_tail(&dev->base.list, devices);
    (*count)++;
  }

  closedir(dir);
  return *count > 0;
}

static void gpuinfo_tenstorrent_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_tenstorrent *tt = container_of(_gpu_info, struct gpu_info_tenstorrent, base);
  struct gpuinfo_static_info *static_info = &tt->base.static_info;
  char attr_path[PATH_MAX];
  long val;

  static_info->integrated_graphics = false;
  static_info->encode_decode_shared = false;
  RESET_ALL(static_info->valid);

  const char *chip = chip_name_from_device_id(tt->device_id);
  char card_type[64] = "";
  snprintf(attr_path, sizeof(attr_path), "%s/tt_card_type", tt->sysfs_device_path);
  FILE *fp = fopen(attr_path, "r");
  if (fp) {
    if (fgets(card_type, sizeof(card_type), fp)) {
      char *nl = strchr(card_type, '\n');
      if (nl)
        *nl = '\0';
    }
    fclose(fp);
  }

  if (card_type[0] && strcmp(card_type, "unknown") != 0)
    snprintf(static_info->device_name, sizeof(static_info->device_name), "%s %s", chip, card_type);
  else
    snprintf(static_info->device_name, sizeof(static_info->device_name), "%s", chip);
  SET_VALID(gpuinfo_device_name_valid, static_info->valid);

  snprintf(attr_path, sizeof(attr_path), "%s/max_link_speed", tt->pci_device_path);
  if (read_sysfs_long(attr_path, &val))
    SET_GPUINFO_STATIC(static_info, max_pcie_gen, nvtop_pcie_gen_from_link_speed(val));

  snprintf(attr_path, sizeof(attr_path), "%s/max_link_width", tt->pci_device_path);
  if (read_sysfs_long(attr_path, &val))
    SET_GPUINFO_STATIC(static_info, max_pcie_link_width, val);

  if (tt->hwmon_path[0]) {
    snprintf(attr_path, sizeof(attr_path), "%s/temp1_max", tt->hwmon_path);
    if (read_sysfs_long(attr_path, &val))
      SET_GPUINFO_STATIC(static_info, temperature_slowdown_threshold, val / 1000);
  }
}

static void gpuinfo_tenstorrent_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_tenstorrent *tt = container_of(_gpu_info, struct gpu_info_tenstorrent, base);
  struct gpuinfo_dynamic_info *dynamic_info = &tt->base.dynamic_info;
  char path[PATH_MAX];
  long val;

  RESET_ALL(dynamic_info->valid);

  if (tt->hwmon_path[0]) {
    snprintf(path, sizeof(path), "%s/temp1_input", tt->hwmon_path);
    if (read_sysfs_long(path, &val))
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, val / 1000);

    snprintf(path, sizeof(path), "%s/power1_input", tt->hwmon_path);
    if (read_sysfs_long(path, &val))
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, val / 1000);

    snprintf(path, sizeof(path), "%s/power1_max", tt->hwmon_path);
    if (read_sysfs_long(path, &val))
      SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, val / 1000);

    snprintf(path, sizeof(path), "%s/fan1_input", tt->hwmon_path);
    if (read_sysfs_long(path, &val))
      SET_GPUINFO_DYNAMIC(dynamic_info, fan_rpm, val);
  }

  snprintf(path, sizeof(path), "%s/tt_aiclk", tt->sysfs_device_path);
  if (read_sysfs_long(path, &val))
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, val);

  snprintf(path, sizeof(path), "%s/current_link_speed", tt->pci_device_path);
  if (read_sysfs_long(path, &val))
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_gen, nvtop_pcie_gen_from_link_speed(val));

  snprintf(path, sizeof(path), "%s/current_link_width", tt->pci_device_path);
  if (read_sysfs_long(path, &val))
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_width, val);

  unsigned rx_lo = 0, rx_hi = 0, tx_lo = 0, tx_hi = 0, tx2_lo = 0, tx2_hi = 0;
  snprintf(path, sizeof(path), "%s/pcie_perf_counters/slv_nonposted_wr_data_word_received0", tt->sysfs_device_path);
  bool have_rx = read_sysfs_u32(path, &rx_lo);
  snprintf(path, sizeof(path), "%s/pcie_perf_counters/slv_nonposted_wr_data_word_received1", tt->sysfs_device_path);
  have_rx = have_rx && read_sysfs_u32(path, &rx_hi);

  snprintf(path, sizeof(path), "%s/pcie_perf_counters/slv_rd_data_word_sent0", tt->sysfs_device_path);
  bool have_tx = read_sysfs_u32(path, &tx_lo);
  snprintf(path, sizeof(path), "%s/pcie_perf_counters/slv_rd_data_word_sent1", tt->sysfs_device_path);
  have_tx = have_tx && read_sysfs_u32(path, &tx_hi);

  snprintf(path, sizeof(path), "%s/pcie_perf_counters/mst_rd_data_word_received0", tt->sysfs_device_path);
  read_sysfs_u32(path, &tx2_lo);
  snprintf(path, sizeof(path), "%s/pcie_perf_counters/mst_rd_data_word_received1", tt->sysfs_device_path);
  read_sysfs_u32(path, &tx2_hi);

  if (have_rx && have_tx) {
    unsigned long long rx_words = ((unsigned long long)rx_hi << 32) | rx_lo;
    unsigned long long tx_words = (((unsigned long long)tx_hi << 32) | tx_lo) +
                                  (((unsigned long long)tx2_hi << 32) | tx2_lo);
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);

    if (tt->pcie_counters_valid) {
      double dt = (now.tv_sec - tt->last_counter_time.tv_sec) +
                  (now.tv_nsec - tt->last_counter_time.tv_nsec) / 1e9;
      if (dt > 0.0) {
        unsigned long long rx_delta = rx_words - tt->last_pcie_rx;
        unsigned long long tx_delta = tx_words - tt->last_pcie_tx;
        SET_GPUINFO_DYNAMIC(dynamic_info, pcie_rx, (unsigned)(rx_delta * 4 / 1024 / dt));
        SET_GPUINFO_DYNAMIC(dynamic_info, pcie_tx, (unsigned)(tx_delta * 4 / 1024 / dt));
      }
    }
    tt->last_pcie_rx = rx_words;
    tt->last_pcie_tx = tx_words;
    tt->last_counter_time = now;
    tt->pcie_counters_valid = true;
  }
}

#define TT_MAX_PIDS 128

static void gpuinfo_tenstorrent_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_tenstorrent *tt = container_of(_gpu_info, struct gpu_info_tenstorrent, base);
  char path[64];
  pid_t pids[TT_MAX_PIDS];
  unsigned n_pids = 0;

  _gpu_info->processes_count = 0;

  snprintf(path, sizeof(path), "/proc/driver/tenstorrent/%u/pids", tt->ordinal);
  FILE *fp = fopen(path, "r");
  if (!fp)
    return;

  long pid;
  while (fscanf(fp, "%ld", &pid) == 1 && n_pids < TT_MAX_PIDS) {
    bool dup = false;
    for (unsigned i = 0; i < n_pids; i++) {
      if (pids[i] == (pid_t)pid) {
        dup = true;
        break;
      }
    }
    if (!dup)
      pids[n_pids++] = (pid_t)pid;
  }
  fclose(fp);

  if (n_pids == 0)
    return;

  if (_gpu_info->processes_array_size < n_pids) {
    _gpu_info->processes = realloc(_gpu_info->processes, n_pids * sizeof(*_gpu_info->processes));
    if (!_gpu_info->processes) {
      _gpu_info->processes_array_size = 0;
      return;
    }
    _gpu_info->processes_array_size = n_pids;
  }

  _gpu_info->processes_count = n_pids;
  for (unsigned i = 0; i < n_pids; i++) {
    memset(&_gpu_info->processes[i], 0, sizeof(_gpu_info->processes[i]));
    _gpu_info->processes[i].type = gpu_process_compute;
    _gpu_info->processes[i].pid = pids[i];
  }
}

struct gpu_vendor gpu_vendor_tenstorrent = {
  .init = gpuinfo_tenstorrent_init,
  .shutdown = gpuinfo_tenstorrent_shutdown,
  .last_error_string = gpuinfo_tenstorrent_last_error_string,
  .get_device_handles = gpuinfo_tenstorrent_get_device_handles,
  .populate_static_info = gpuinfo_tenstorrent_populate_static_info,
  .refresh_dynamic_info = gpuinfo_tenstorrent_refresh_dynamic_info,
  .refresh_running_processes = gpuinfo_tenstorrent_get_running_processes,
  .name = "Tenstorrent",
};

__attribute__((constructor)) static void init_extract_gpuinfo_tenstorrent(void) {
  register_gpu_vendor(&gpu_vendor_tenstorrent);
}
