/*
 *
 * Copyright (C) 2026 Tony
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

/*
 * Intel Gaudi (Habana Labs) HPU support through libhlml.
 *
 * Several HLML queries go through the device firmware and are slow (PCIe throughput takes ~25ms per call and per
 * direction), so the dynamic information is sampled by a background thread and the interface only copies the latest
 * sample. After the first synchronous sample, HLML is only ever called from that thread so we do not rely on libhlml
 * being thread safe.
 *
 * HLML does not list processes. Like hl-smi, we scan /proc for file descriptors opened on the /dev/accel/accelN compute
 * node. Gaudi allows a single compute context per device; the framework (Synapse) publishes the memory used by this
 * context in the shared memory file /dev/shm/mem_usage_accelN, which the process keeps open, so we read it through
 * /proc/<pid>/fd to get the process memory usage (this works for processes living in containers too).
 */

#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"

#include <ctype.h>
#include <dirent.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <fnmatch.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

// Subset of habanalabs/hlml.h; declared here so that building nvtop does not require the Habana SDK headers.

typedef enum {
  HLML_SUCCESS = 0,
  HLML_ERROR_UNINITIALIZED = 1,
  HLML_ERROR_INVALID_ARGUMENT = 2,
  HLML_ERROR_NOT_SUPPORTED = 3,
  HLML_ERROR_ALREADY_INITIALIZED = 5,
  HLML_ERROR_NOT_FOUND = 6,
  HLML_ERROR_INSUFFICIENT_SIZE = 7,
  HLML_ERROR_DRIVER_NOT_LOADED = 9,
  HLML_ERROR_TIMEOUT = 10,
  HLML_ERROR_AIP_IS_LOST = 15,
  HLML_ERROR_MEMORY = 20,
  HLML_ERROR_NO_DATA = 21,
  HLML_ERROR_UNKNOWN = 49,
} hlml_return_t;

typedef void *hlml_device_t;

#define HLML_PCI_DOMAIN_LEN 9
#define HLML_PCI_ADDR_LEN ((HLML_PCI_DOMAIN_LEN) + 10)
#define HLML_PCI_LINK_INFO_LEN 10

typedef struct {
  char link_speed[HLML_PCI_LINK_INFO_LEN];
  char link_width[HLML_PCI_LINK_INFO_LEN];
  char link_max_speed[HLML_PCI_LINK_INFO_LEN];
  char link_max_width[HLML_PCI_LINK_INFO_LEN];
} hlml_pci_cap_t;

typedef struct {
  unsigned int bus;
  char bus_id[HLML_PCI_ADDR_LEN];
  unsigned int device;
  unsigned int domain;
  unsigned int pci_device_id;
  hlml_pci_cap_t caps;
  unsigned int pci_rev;
  unsigned int pci_subsys_id;
} hlml_pci_info_t;

typedef enum {
  HLML_CLOCK_SOC = 0,
} hlml_clock_type_t;

typedef struct {
  unsigned int aip;
  unsigned int memory;
} hlml_utilization_t;

typedef struct {
  unsigned long long free;
  unsigned long long total;
  unsigned long long used;
} hlml_memory_t;

typedef enum {
  HLML_TEMPERATURE_ON_AIP = 0,
} hlml_temperature_sensors_t;

typedef enum {
  HLML_TEMPERATURE_THRESHOLD_SHUTDOWN = 0,
  HLML_TEMPERATURE_THRESHOLD_SLOWDOWN = 1,
} hlml_temperature_thresholds_t;

typedef enum {
  HLML_PCIE_UTIL_TX_BYTES = 0,
  HLML_PCIE_UTIL_RX_BYTES = 1,
} hlml_pcie_util_counter_t;

// Layout of /dev/shm/mem_usage_accelN (habanalabs/hlml_shm.h)
struct hlml_shm_data {
  uint64_t version;
  uint64_t timestamp;
  uint64_t used_mem_in_bytes;
} __attribute__((packed));

#define HLML_SHM_PATH_PREFIX "/dev/shm/mem_usage_accel"

static hlml_return_t (*hlml_init)(void);
static hlml_return_t (*hlml_shutdown)(void);
static hlml_return_t (*hlml_device_get_count)(unsigned int *device_count);
static hlml_return_t (*hlml_device_get_handle_by_index)(unsigned int index, hlml_device_t *device);
static hlml_return_t (*hlml_device_get_name)(hlml_device_t device, char *name, unsigned int length);
static hlml_return_t (*hlml_device_get_pci_info)(hlml_device_t device, hlml_pci_info_t *pci);
static hlml_return_t (*hlml_device_get_minor_number)(hlml_device_t device, unsigned int *minor_number);
static hlml_return_t (*hlml_device_get_utilization_rates)(hlml_device_t device, hlml_utilization_t *utilization);
static hlml_return_t (*hlml_device_get_memory_info)(hlml_device_t device, hlml_memory_t *memory);
// Optional symbols: everything below may be NULL
static hlml_return_t (*hlml_device_get_clock_info)(hlml_device_t device, hlml_clock_type_t type, unsigned int *clock);
static hlml_return_t (*hlml_device_get_max_clock_info)(hlml_device_t device, hlml_clock_type_t type,
                                                       unsigned int *clock);
static hlml_return_t (*hlml_device_get_temperature)(hlml_device_t device, hlml_temperature_sensors_t sensor_type,
                                                    unsigned int *temp);
static hlml_return_t (*hlml_device_get_temperature_threshold)(hlml_device_t device,
                                                              hlml_temperature_thresholds_t threshold_type,
                                                              unsigned int *temp);
static hlml_return_t (*hlml_device_get_power_usage)(hlml_device_t device, unsigned int *power);
static hlml_return_t (*hlml_device_get_power_management_limit)(hlml_device_t device, unsigned int *limit);
static hlml_return_t (*hlml_device_get_pcie_throughput)(hlml_device_t device, hlml_pcie_util_counter_t counter,
                                                        unsigned int *value);

static void *libhlml_handle;

static hlml_return_t last_hlml_return_status = HLML_SUCCESS;
static char didnt_call_gpuinfo_init[] = "The Gaudi extraction has not been initialized, please call "
                                        "gpuinfo_gaudi_init\n";
static const char *local_error_string = didnt_call_gpuinfo_init;

#define GAUDI_MAX_CHIP_SENSORS 16

struct gpu_info_gaudi {
  struct gpu_info base;
  hlml_device_t handle;
  unsigned minor;
  bool rdev_valid;
  dev_t rdev; // Device number of /dev/accel/accel<minor>
  char sysfs_path[64];
  char hwmon_path[128];
  unsigned n_chip_sensors;
  unsigned chip_sensors[GAUDI_MAX_CHIP_SENSORS]; // hwmon temperature channels located on the chip
  unsigned scan_generation_used;

  // Protected by sampler_lock once the sampler thread is running
  bool monitored;
  bool has_sample;
  struct gpuinfo_dynamic_info sample;
};

static unsigned gaudi_device_count;
static struct gpu_info_gaudi *gaudi_devices;

// Background sampler state
static pthread_mutex_t sampler_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t sampler_cond = PTHREAD_COND_INITIALIZER;
static pthread_t sampler_thread;
static bool sampler_started;
static bool sampler_failed;
static bool sampler_stop;
static bool sample_requested;

// Result of the last /proc scan
struct gaudi_process_entry {
  pid_t pid;
  unsigned device_index;
  bool memory_valid;
  unsigned long long memory;
};
static unsigned scan_generation;
static unsigned scan_entries_count;
static unsigned scan_entries_size;
static struct gaudi_process_entry *scan_entries;

static bool gpuinfo_gaudi_init(void);
static void gpuinfo_gaudi_shutdown(void);
static const char *gpuinfo_gaudi_last_error_string(void);
static bool gpuinfo_gaudi_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_gaudi_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_gaudi_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_gaudi_get_running_processes(struct gpu_info *_gpu_info);

struct gpu_vendor gpu_vendor_gaudi = {
    .init = gpuinfo_gaudi_init,
    .shutdown = gpuinfo_gaudi_shutdown,
    .last_error_string = gpuinfo_gaudi_last_error_string,
    .get_device_handles = gpuinfo_gaudi_get_device_handles,
    .populate_static_info = gpuinfo_gaudi_populate_static_info,
    .refresh_dynamic_info = gpuinfo_gaudi_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_gaudi_get_running_processes,
    .name = "Gaudi",
};

__attribute__((constructor)) static void init_extract_gpuinfo_gaudi(void) { register_gpu_vendor(&gpu_vendor_gaudi); }

static const char *hlml_error_string(hlml_return_t status) {
  switch (status) {
  case HLML_SUCCESS:
    return "Success";
  case HLML_ERROR_UNINITIALIZED:
    return "HLML was not initialized";
  case HLML_ERROR_INVALID_ARGUMENT:
    return "Invalid argument";
  case HLML_ERROR_NOT_SUPPORTED:
    return "Operation not supported by the device";
  case HLML_ERROR_ALREADY_INITIALIZED:
    return "HLML already initialized";
  case HLML_ERROR_NOT_FOUND:
    return "Not found";
  case HLML_ERROR_INSUFFICIENT_SIZE:
    return "Insufficient size";
  case HLML_ERROR_DRIVER_NOT_LOADED:
    return "The habanalabs driver is not loaded";
  case HLML_ERROR_TIMEOUT:
    return "Timeout";
  case HLML_ERROR_AIP_IS_LOST:
    return "The device is lost";
  case HLML_ERROR_MEMORY:
    return "Out of memory";
  case HLML_ERROR_NO_DATA:
    return "No data";
  default:
    return "Unknown HLML error";
  }
}

/*
 *
 * This function loads the libhlml.so shared object, initializes the
 * required function pointers and calls the hlml library initialization
 * function. If false is returned, the cause of the error can be retrieved
 * by calling the function gpuinfo_gaudi_last_error_string.
 *
 */
static bool gpuinfo_gaudi_init(void) {
  // Do not bother loading the library on machines without the habanalabs driver
  if (access("/sys/module/habanalabs", F_OK) != 0) {
    local_error_string = "The habanalabs driver is not loaded";
    return false;
  }

  libhlml_handle = dlopen("libhlml.so", RTLD_LAZY);
  if (!libhlml_handle)
    libhlml_handle = dlopen("/usr/lib/habanalabs/libhlml.so", RTLD_LAZY);
  if (!libhlml_handle) {
    local_error_string = dlerror();
    return false;
  }

#define LOAD_REQUIRED(sym)                                                                                             \
  do {                                                                                                                 \
    sym = dlsym(libhlml_handle, #sym);                                                                                 \
    if (!sym)                                                                                                          \
      goto init_error_clean_exit;                                                                                      \
  } while (0)
#define LOAD_OPTIONAL(sym) sym = dlsym(libhlml_handle, #sym)

  LOAD_REQUIRED(hlml_init);
  LOAD_REQUIRED(hlml_shutdown);
  LOAD_REQUIRED(hlml_device_get_count);
  LOAD_REQUIRED(hlml_device_get_handle_by_index);
  LOAD_REQUIRED(hlml_device_get_name);
  LOAD_REQUIRED(hlml_device_get_pci_info);
  LOAD_REQUIRED(hlml_device_get_minor_number);
  LOAD_REQUIRED(hlml_device_get_utilization_rates);
  LOAD_REQUIRED(hlml_device_get_memory_info);

  LOAD_OPTIONAL(hlml_device_get_clock_info);
  LOAD_OPTIONAL(hlml_device_get_max_clock_info);
  LOAD_OPTIONAL(hlml_device_get_temperature);
  LOAD_OPTIONAL(hlml_device_get_temperature_threshold);
  LOAD_OPTIONAL(hlml_device_get_power_usage);
  LOAD_OPTIONAL(hlml_device_get_power_management_limit);
  LOAD_OPTIONAL(hlml_device_get_pcie_throughput);

#undef LOAD_REQUIRED
#undef LOAD_OPTIONAL

  last_hlml_return_status = hlml_init();
  if (last_hlml_return_status != HLML_SUCCESS) {
    local_error_string = hlml_error_string(last_hlml_return_status);
    dlclose(libhlml_handle);
    libhlml_handle = NULL;
    return false;
  }

  local_error_string = NULL;
  return true;

init_error_clean_exit:
  local_error_string = dlerror();
  dlclose(libhlml_handle);
  libhlml_handle = NULL;
  return false;
}

static void gpuinfo_gaudi_shutdown(void) {
  if (sampler_started) {
    pthread_mutex_lock(&sampler_lock);
    sampler_stop = true;
    pthread_cond_signal(&sampler_cond);
    pthread_mutex_unlock(&sampler_lock);
    pthread_join(sampler_thread, NULL);
    sampler_started = false;
    sampler_stop = false;
  }
  sampler_failed = false;
  sample_requested = false;

  if (libhlml_handle) {
    hlml_shutdown();
    dlclose(libhlml_handle);
    libhlml_handle = NULL;
    local_error_string = didnt_call_gpuinfo_init;
  }

  free(gaudi_devices);
  gaudi_devices = NULL;
  gaudi_device_count = 0;
  free(scan_entries);
  scan_entries = NULL;
  scan_entries_count = 0;
  scan_entries_size = 0;
  scan_generation = 0;
}

static const char *gpuinfo_gaudi_last_error_string(void) {
  if (local_error_string)
    return local_error_string;
  return hlml_error_string(last_hlml_return_status);
}

#define HL_SENSORS_CONF "/etc/sensors.d/hl_sensors.conf"
#define GAUDI_MAX_HWMON_CHANNELS 128

/*
 * HLML reports the chip temperature as the hottest of the hwmon sensors labelled "On Chip ..." in the lm-sensors
 * configuration installed with the driver. hlml_device_get_temperature leaks memory at each call (sensors_get_label in
 * libhlml), which adds up quickly in a long running monitor, so we read the same hwmon channels directly.
 */
static void find_chip_temperature_sensors(struct gpu_info_gaudi *gpu_info) {
  char hwmon_parent[96];
  snprintf(hwmon_parent, sizeof(hwmon_parent), "%s/hwmon", gpu_info->sysfs_path);
  DIR *hwmon_dir = opendir(hwmon_parent);
  if (!hwmon_dir)
    return;
  struct dirent *dent;
  while ((dent = readdir(hwmon_dir)) != NULL) {
    if (strncmp(dent->d_name, "hwmon", 5) == 0) {
      snprintf(gpu_info->hwmon_path, sizeof(gpu_info->hwmon_path), "%s/%.31s", hwmon_parent, dent->d_name);
      break;
    }
  }
  closedir(hwmon_dir);
  if (!gpu_info->hwmon_path[0])
    return;

  // The hwmon name is the board type (e.g. HL225), matched by the "chip" statements as "HL225-pci-*"
  char name_path[160], chip_id[80];
  snprintf(name_path, sizeof(name_path), "%s/name", gpu_info->hwmon_path);
  FILE *name_file = fopen(name_path, "r");
  if (!name_file)
    return;
  char name[64] = "";
  bool has_name = fgets(name, sizeof(name), name_file) != NULL;
  fclose(name_file);
  name[strcspn(name, "\n")] = '\0';
  if (!has_name || !name[0])
    return;
  snprintf(chip_id, sizeof(chip_id), "%s-pci-0", name);

  FILE *conf = fopen(HL_SENSORS_CONF, "r");
  if (!conf)
    return;
  bool on_chip[GAUDI_MAX_HWMON_CHANNELS] = {false};
  bool ignored[GAUDI_MAX_HWMON_CHANNELS] = {false};
  bool in_matching_chip = false;
  char line[512];
  while (fgets(line, sizeof(line), conf)) {
    char *statement = line;
    while (isspace((unsigned char)*statement))
      statement++;
    unsigned channel;
    char label[64];
    if (strncmp(statement, "chip", 4) == 0 && isspace((unsigned char)statement[4])) {
      in_matching_chip = false;
      char *pattern = statement + 4;
      while ((pattern = strchr(pattern, '"')) != NULL) {
        char *pattern_end = strchr(pattern + 1, '"');
        if (!pattern_end)
          break;
        *pattern_end = '\0';
        in_matching_chip = in_matching_chip || fnmatch(pattern + 1, chip_id, 0) == 0;
        pattern = pattern_end + 1;
      }
    } else if (!in_matching_chip) {
      continue;
    } else if (sscanf(statement, "label temp%u \"%63[^\"]\"", &channel, label) == 2) {
      if (channel < GAUDI_MAX_HWMON_CHANNELS && strncmp(label, "On Chip", 7) == 0)
        on_chip[channel] = true;
    } else if (sscanf(statement, "ignore temp%u", &channel) == 1) {
      if (channel < GAUDI_MAX_HWMON_CHANNELS)
        ignored[channel] = true;
    }
  }
  fclose(conf);

  for (unsigned channel = 0; channel < GAUDI_MAX_HWMON_CHANNELS && gpu_info->n_chip_sensors < GAUDI_MAX_CHIP_SENSORS;
       ++channel) {
    if (on_chip[channel] && !ignored[channel])
      gpu_info->chip_sensors[gpu_info->n_chip_sensors++] = channel;
  }
}

// Degrees celsius, truncated like HLML does
static bool read_chip_temperature(const struct gpu_info_gaudi *gpu_info, unsigned *temperature) {
  bool found = false;
  long hottest = 0;
  for (unsigned i = 0; i < gpu_info->n_chip_sensors; ++i) {
    char path[160];
    snprintf(path, sizeof(path), "%s/temp%u_input", gpu_info->hwmon_path, gpu_info->chip_sensors[i]);
    FILE *file = fopen(path, "r");
    if (!file)
      continue;
    long millidegrees;
    if (fscanf(file, "%ld", &millidegrees) == 1 && (!found || millidegrees > hottest)) {
      hottest = millidegrees;
      found = true;
    }
    fclose(file);
  }
  if (found)
    *temperature = hottest > 0 ? (unsigned)(hottest / 1000) : 0;
  return found;
}

static bool gpuinfo_gaudi_get_device_handles(struct list_head *devices, unsigned *count) {
  *count = 0;
  if (!libhlml_handle)
    return false;

  unsigned num_devices;
  last_hlml_return_status = hlml_device_get_count(&num_devices);
  if (last_hlml_return_status != HLML_SUCCESS)
    return false;
  if (num_devices == 0)
    return true;

  gaudi_devices = calloc(num_devices, sizeof(*gaudi_devices));
  if (!gaudi_devices) {
    local_error_string = strerror(errno);
    return false;
  }

  for (unsigned i = 0; i < num_devices; ++i) {
    struct gpu_info_gaudi *gpu_info = &gaudi_devices[gaudi_device_count];
    if (hlml_device_get_handle_by_index(i, &gpu_info->handle) != HLML_SUCCESS)
      continue;

    gpu_info->base.vendor = &gpu_vendor_gaudi;

    hlml_pci_info_t pci_info;
    if (hlml_device_get_pci_info(gpu_info->handle, &pci_info) == HLML_SUCCESS) {
      strncpy(gpu_info->base.pdev, pci_info.bus_id, PDEV_LEN - 1);
      gpu_info->base.pdev[PDEV_LEN - 1] = '\0';
    }

    if (hlml_device_get_minor_number(gpu_info->handle, &gpu_info->minor) == HLML_SUCCESS) {
      char dev_path[64];
      struct stat dev_stat;
      snprintf(dev_path, sizeof(dev_path), "/dev/accel/accel%u", gpu_info->minor);
      if (stat(dev_path, &dev_stat) == 0 && S_ISCHR(dev_stat.st_mode)) {
        gpu_info->rdev = dev_stat.st_rdev;
        gpu_info->rdev_valid = true;
      }
      snprintf(gpu_info->sysfs_path, sizeof(gpu_info->sysfs_path), "/sys/class/accel/accel%u/device", gpu_info->minor);
      find_chip_temperature_sensors(gpu_info);
    }

    list_add_tail(&gpu_info->base.list, devices);
    gaudi_device_count++;
  }
  *count = gaudi_device_count;

  return true;
}

// Read a sysfs attribute of the device; returns false if unavailable
static bool read_device_attribute(const struct gpu_info_gaudi *gpu_info, const char *attribute, char *buf,
                                  size_t buf_size) {
  if (!gpu_info->sysfs_path[0])
    return false;
  char path[128];
  snprintf(path, sizeof(path), "%s/%s", gpu_info->sysfs_path, attribute);
  FILE *file = fopen(path, "r");
  if (!file)
    return false;
  bool success = fgets(buf, buf_size, file) != NULL;
  fclose(file);
  if (success)
    buf[strcspn(buf, "\n")] = '\0';
  return success;
}

static bool read_pcie_link(const struct gpu_info_gaudi *gpu_info, const char *speed_attribute,
                           const char *width_attribute, unsigned *gen, unsigned *width) {
  char buf[64];
  // Speed looks like "16.0 GT/s PCIe"
  if (!read_device_attribute(gpu_info, speed_attribute, buf, sizeof(buf)))
    return false;
  *gen = nvtop_pcie_gen_from_link_speed((unsigned)strtoul(buf, NULL, 10));
  if (!*gen || !read_device_attribute(gpu_info, width_attribute, buf, sizeof(buf)))
    return false;
  *width = (unsigned)strtoul(buf, NULL, 10);
  return true;
}

static void gpuinfo_gaudi_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_gaudi *gpu_info = container_of(_gpu_info, struct gpu_info_gaudi, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;

  static_info->integrated_graphics = false;
  static_info->encode_decode_shared = false;
  RESET_ALL(static_info->valid);

  // e.g. "Gaudi2 HL-225": the family comes from sysfs (GAUDI2), the board name from HLML
  char board_name[MAX_DEVICE_NAME] = "";
  char family[32] = "";
  last_hlml_return_status = hlml_device_get_name(gpu_info->handle, board_name, sizeof(board_name));
  if (last_hlml_return_status != HLML_SUCCESS)
    board_name[0] = '\0';
  if (read_device_attribute(gpu_info, "device_type", family, sizeof(family))) {
    for (char *c = family + 1; *c; ++c)
      *c = (char)tolower((unsigned char)*c);
  }
  if (family[0] && board_name[0])
    snprintf(static_info->device_name, MAX_DEVICE_NAME, "%s %s", family, board_name);
  else
    snprintf(static_info->device_name, MAX_DEVICE_NAME, "%s", board_name[0] ? board_name : family);
  if (static_info->device_name[0])
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
  if (family[0]) {
    snprintf(static_info->device_architecture, MAX_DEVICE_NAME, "%s", family);
    SET_VALID(gpuinfo_device_architecture_valid, static_info->valid);
  }

  // Every Gaudi generation uses HBM, which is also what hl-smi reports
  snprintf(static_info->memory_type, sizeof(static_info->memory_type), "HBM");
  SET_VALID(gpuinfo_memory_type_valid, static_info->valid);

  unsigned max_gen, max_width;
  if (read_pcie_link(gpu_info, "max_link_speed", "max_link_width", &max_gen, &max_width)) {
    SET_GPUINFO_STATIC(static_info, max_pcie_gen, max_gen);
    SET_GPUINFO_STATIC(static_info, max_pcie_link_width, max_width);
  }

  if (hlml_device_get_temperature_threshold) {
    unsigned threshold;
    last_hlml_return_status =
        hlml_device_get_temperature_threshold(gpu_info->handle, HLML_TEMPERATURE_THRESHOLD_SHUTDOWN, &threshold);
    if (last_hlml_return_status == HLML_SUCCESS)
      SET_GPUINFO_STATIC(static_info, temperature_shutdown_threshold, threshold);
    last_hlml_return_status =
        hlml_device_get_temperature_threshold(gpu_info->handle, HLML_TEMPERATURE_THRESHOLD_SLOWDOWN, &threshold);
    if (last_hlml_return_status == HLML_SUCCESS)
      SET_GPUINFO_STATIC(static_info, temperature_slowdown_threshold, threshold);
  }
}

// Query all the dynamic information of a device. Must never run concurrently with another HLML call.
static void gaudi_sample_device(struct gpu_info_gaudi *gpu_info, struct gpuinfo_dynamic_info *dynamic_info) {
  hlml_device_t handle = gpu_info->handle;

  RESET_ALL(dynamic_info->valid);
  dynamic_info->multi_instance_mode = false;

  unsigned clock;
  if (hlml_device_get_clock_info && hlml_device_get_clock_info(handle, HLML_CLOCK_SOC, &clock) == HLML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, clock);
  if (hlml_device_get_max_clock_info && hlml_device_get_max_clock_info(handle, HLML_CLOCK_SOC, &clock) == HLML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, clock);

  // The AIP utilization can exceed 100%
  hlml_utilization_t utilization;
  if (hlml_device_get_utilization_rates(handle, &utilization) == HLML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, utilization.aip > 100 ? 100 : utilization.aip);

  hlml_memory_t memory;
  if (hlml_device_get_memory_info(handle, &memory) == HLML_SUCCESS && memory.total > 0) {
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, memory.total);
    SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, memory.used);
    SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, memory.free);
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, (unsigned)(memory.used * 100 / memory.total));
  }

  // HLML does not report the current link on Gaudi2, sysfs does
  unsigned link_gen, link_width;
  if (read_pcie_link(gpu_info, "current_link_speed", "current_link_width", &link_gen, &link_width)) {
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_gen, link_gen);
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_width, link_width);
  }

  // KB/s
  unsigned throughput;
  if (hlml_device_get_pcie_throughput) {
    if (hlml_device_get_pcie_throughput(handle, HLML_PCIE_UTIL_RX_BYTES, &throughput) == HLML_SUCCESS)
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_rx, throughput);
    if (hlml_device_get_pcie_throughput(handle, HLML_PCIE_UTIL_TX_BYTES, &throughput) == HLML_SUCCESS)
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_tx, throughput);
  }

  unsigned temperature;
  if (gpu_info->n_chip_sensors > 0) {
    if (read_chip_temperature(gpu_info, &temperature))
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, temperature);
  } else if (hlml_device_get_temperature &&
             hlml_device_get_temperature(handle, HLML_TEMPERATURE_ON_AIP, &temperature) == HLML_SUCCESS) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, temperature);
  }

  // Milliwatts
  unsigned power;
  if (hlml_device_get_power_usage && hlml_device_get_power_usage(handle, &power) == HLML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, power);
  if (hlml_device_get_power_management_limit && hlml_device_get_power_management_limit(handle, &power) == HLML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, power);
}

static void *gaudi_sampler_main(void *arg) {
  (void)arg;
  pthread_mutex_lock(&sampler_lock);
  while (!sampler_stop) {
    while (!sampler_stop && !sample_requested)
      pthread_cond_wait(&sampler_cond, &sampler_lock);

    for (unsigned i = 0; !sampler_stop && i < gaudi_device_count; ++i) {
      struct gpu_info_gaudi *gpu_info = &gaudi_devices[i];
      if (!gpu_info->monitored)
        continue;
      pthread_mutex_unlock(&sampler_lock);
      struct gpuinfo_dynamic_info sample;
      gaudi_sample_device(gpu_info, &sample);
      pthread_mutex_lock(&sampler_lock);
      gpu_info->sample = sample;
      gpu_info->has_sample = true;
    }
    // The interface refreshes every device in a burst; requests made while sampling are served by this round
    sample_requested = false;
  }
  pthread_mutex_unlock(&sampler_lock);
  return NULL;
}

static void gpuinfo_gaudi_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_gaudi *gpu_info = container_of(_gpu_info, struct gpu_info_gaudi, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  if (sampler_failed) {
    gaudi_sample_device(gpu_info, dynamic_info);
    return;
  }

  if (!sampler_started) {
    // First refresh: sample synchronously so that the first frame has data, then hand over to the sampler thread
    for (unsigned i = 0; i < gaudi_device_count; ++i) {
      gaudi_sample_device(&gaudi_devices[i], &gaudi_devices[i].sample);
      gaudi_devices[i].has_sample = true;
    }
    if (pthread_create(&sampler_thread, NULL, gaudi_sampler_main, NULL) != 0) {
      // Keep sampling synchronously
      sampler_failed = true;
      *dynamic_info = gpu_info->sample;
      return;
    }
    sampler_started = true;
  }

  pthread_mutex_lock(&sampler_lock);
  gpu_info->monitored = true;
  sample_requested = true;
  pthread_cond_signal(&sampler_cond);
  *dynamic_info = gpu_info->sample;
  pthread_mutex_unlock(&sampler_lock);
}

static bool record_scan_entry(pid_t pid, unsigned device_index, bool memory_valid, unsigned long long memory) {
  if (scan_entries_count == scan_entries_size) {
    scan_entries_size += COMMON_PROCESS_LINEAR_REALLOC_INC;
    struct gaudi_process_entry *new_entries = reallocarray(scan_entries, scan_entries_size, sizeof(*scan_entries));
    if (!new_entries)
      return false;
    scan_entries = new_entries;
  }
  scan_entries[scan_entries_count++] = (struct gaudi_process_entry){
      .pid = pid,
      .device_index = device_index,
      .memory_valid = memory_valid,
      .memory = memory,
  };
  return true;
}

// Index N of a "<prefix>N" path, or -1
static int path_index(const char *path, const char *prefix) {
  size_t prefix_len = strlen(prefix);
  if (strncmp(path, prefix, prefix_len) != 0 || !isdigit((unsigned char)path[prefix_len]))
    return -1;
  char *end;
  long index = strtol(path + prefix_len, &end, 10);
  return *end == '\0' ? (int)index : -1;
}

#define GAUDI_MAX_FD_PER_PROCESS 8

// Scan /proc for the processes having the compute node of a Gaudi device open
static void gaudi_scan_processes(void) {
  scan_entries_count = 0;
  scan_generation++;

  DIR *proc_dir = opendir("/proc");
  if (!proc_dir)
    return;

  struct dirent *proc_dent;
  while ((proc_dent = readdir(proc_dir)) != NULL) {
    if (proc_dent->d_type != DT_DIR || !isdigit((unsigned char)proc_dent->d_name[0]))
      continue;
    pid_t pid = (pid_t)atoi(proc_dent->d_name);

    char fd_dir_path[sizeof(proc_dent->d_name) + sizeof("/proc//fd")];
    snprintf(fd_dir_path, sizeof(fd_dir_path), "/proc/%s/fd", proc_dent->d_name);
    DIR *fd_dir = opendir(fd_dir_path);
    if (!fd_dir)
      continue;

    // Devices opened by this process, with the index of the device node as seen by the process (it differs from the
    // host one when a container remaps the devices)
    unsigned n_devices = 0;
    unsigned device_index[GAUDI_MAX_FD_PER_PROCESS];
    int device_ns_index[GAUDI_MAX_FD_PER_PROCESS];
    // Device memory used, as published by the framework
    unsigned n_shm = 0;
    int shm_ns_index[GAUDI_MAX_FD_PER_PROCESS];
    unsigned long long shm_used[GAUDI_MAX_FD_PER_PROCESS];

    struct dirent *fd_dent;
    while ((fd_dent = readdir(fd_dir)) != NULL) {
      if (!isdigit((unsigned char)fd_dent->d_name[0]))
        continue;
      struct stat fd_stat;
      if (fstatat(dirfd(fd_dir), fd_dent->d_name, &fd_stat, 0) != 0)
        continue;

      if (S_ISCHR(fd_stat.st_mode)) {
        for (unsigned i = 0; i < gaudi_device_count; ++i) {
          if (!gaudi_devices[i].rdev_valid || gaudi_devices[i].rdev != fd_stat.st_rdev)
            continue;
          bool already_seen = false;
          for (unsigned j = 0; j < n_devices; ++j)
            already_seen = already_seen || device_index[j] == i;
          if (already_seen || n_devices == GAUDI_MAX_FD_PER_PROCESS)
            break;
          char link[256];
          ssize_t len = readlinkat(dirfd(fd_dir), fd_dent->d_name, link, sizeof(link) - 1);
          link[len > 0 ? len : 0] = '\0';
          device_index[n_devices] = i;
          device_ns_index[n_devices] = path_index(link, "/dev/accel/accel");
          n_devices++;
          break;
        }
      } else if (S_ISREG(fd_stat.st_mode) && fd_stat.st_size >= (off_t)sizeof(struct hlml_shm_data) &&
                 fd_stat.st_size <= 4096 && n_shm < GAUDI_MAX_FD_PER_PROCESS) {
        // Later versions of the shared memory layout may append fields
        char link[256];
        ssize_t len = readlinkat(dirfd(fd_dir), fd_dent->d_name, link, sizeof(link) - 1);
        if (len <= 0)
          continue;
        link[len] = '\0';
        int ns_index = path_index(link, HLML_SHM_PATH_PREFIX);
        if (ns_index < 0)
          continue;
        int shm_fd = openat(dirfd(fd_dir), fd_dent->d_name, O_RDONLY);
        if (shm_fd < 0)
          continue;
        struct hlml_shm_data shm_data;
        if (pread(shm_fd, &shm_data, sizeof(shm_data), 0) == (ssize_t)sizeof(shm_data) && shm_data.version >= 1) {
          shm_ns_index[n_shm] = ns_index;
          shm_used[n_shm] = shm_data.used_mem_in_bytes;
          n_shm++;
        }
        close(shm_fd);
      }
    }
    closedir(fd_dir);

    for (unsigned i = 0; i < n_devices; ++i) {
      bool memory_valid = false;
      unsigned long long memory = 0;
      for (unsigned j = 0; !memory_valid && j < n_shm; ++j) {
        if (device_ns_index[i] >= 0 && device_ns_index[i] == shm_ns_index[j]) {
          memory_valid = true;
          memory = shm_used[j];
        }
      }
      if (!memory_valid && n_devices == 1 && n_shm == 1) {
        memory_valid = true;
        memory = shm_used[0];
      }
      if (!record_scan_entry(pid, device_index[i], memory_valid, memory))
        break;
    }
  }
  closedir(proc_dir);
}

static void gpuinfo_gaudi_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_gaudi *gpu_info = container_of(_gpu_info, struct gpu_info_gaudi, base);
  unsigned device_index = (unsigned)(gpu_info - gaudi_devices);

  // Called for each device at every refresh: scan /proc once per refresh, i.e., when this device already consumed
  // the current scan
  if (scan_generation == 0 || gpu_info->scan_generation_used == scan_generation)
    gaudi_scan_processes();
  gpu_info->scan_generation_used = scan_generation;

  unsigned count = 0;
  for (unsigned i = 0; i < scan_entries_count; ++i)
    count += scan_entries[i].device_index == device_index;

  _gpu_info->processes_count = 0;
  if (count == 0)
    return;

  if (count > _gpu_info->processes_array_size) {
    _gpu_info->processes_array_size = count + COMMON_PROCESS_LINEAR_REALLOC_INC;
    _gpu_info->processes =
        reallocarray(_gpu_info->processes, _gpu_info->processes_array_size, sizeof(*_gpu_info->processes));
    if (!_gpu_info->processes) {
      perror("Could not allocate memory: ");
      exit(EXIT_FAILURE);
    }
  }

  const struct gpuinfo_dynamic_info *dynamic_info = &_gpu_info->dynamic_info;
  for (unsigned i = 0; i < scan_entries_count; ++i) {
    const struct gaudi_process_entry *entry = &scan_entries[i];
    if (entry->device_index != device_index)
      continue;
    struct gpu_process *process = &_gpu_info->processes[_gpu_info->processes_count++];
    memset(process, 0, sizeof(*process));
    process->type = gpu_process_compute;
    process->pid = entry->pid;
    if (entry->memory_valid) {
      SET_GPUINFO_PROCESS(process, gpu_memory_usage, entry->memory);
    } else if (count == 1 && GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, used_memory)) {
      SET_GPUINFO_PROCESS(process, gpu_memory_usage, dynamic_info->used_memory);
    }
    // A Gaudi device runs a single compute context: the device load is the load of this process
    if (count == 1 && GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, gpu_util_rate))
      SET_GPUINFO_PROCESS(process, gpu_usage, dynamic_info->gpu_util_rate);
  }
}
