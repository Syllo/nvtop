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
 * being thread safe. The static information is populated before that first refresh and may call HLML from the main
 * thread; do not re-populate it at runtime after the sampler has started.
 *
 * HLML does not list processes. Like hl-smi, we scan /proc for file descriptors opened on the /dev/accel/accelN compute
 * node. Gaudi allows a single compute context per device; the framework integration (e.g. PyTorch or Paddle) publishes
 * the memory used by this context in the shared memory file /dev/shm/mem_usage_accelN, which the process keeps open, so
 * we read it through /proc/<pid>/fd to get the process memory usage (this works for processes living in containers
 * too).
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
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

/*
 * Subset of habanalabs/hlml.h, transcribed from the 1.24 SDK, declared here so
 * that building nvtop does not require the Habana SDK headers.
 *
 * The output structures are part of the libhlml ABI: the library writes the
 * size of its own structure into the object passed by the caller, and the API
 * has no size negotiation. A newer library that grew a structure would
 * therefore overwrite whatever follows nvtop's object. Each output structure
 * ends with a reserved tail, much larger than any plausible addition, so that
 * ABI growth lands in our object instead of on the stack. The assertions below
 * document the transcribed 1.24 prefix and catch accidental edits; they cannot
 * check the vendor structure size, so bump the reserve if a future header
 * outgrows it.
 */

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

// Much larger than the remaining query structures (24 and 8 bytes). They hold
// a handful of counters; a future version is very unlikely to need more than
// this. The static assertion below refuses to let the reserve shrink.
#define HLML_ABI_RESERVED_SIZE 512
_Static_assert(HLML_ABI_RESERVED_SIZE >= 256, "the reserved space for HLML ABI growth is too small");

typedef enum {
  HLML_CLOCK_SOC = 0,
} hlml_clock_type_t;

typedef struct {
  unsigned int aip;
  unsigned int memory;                            // Part of the ABI; HLML reports it as a percentage of used memory
  unsigned char reserved[HLML_ABI_RESERVED_SIZE]; // Absorbs growth in a newer libhlml; never read
} hlml_utilization_t;

typedef struct {
  unsigned long long free;
  unsigned long long total;
  unsigned long long used;
  unsigned char reserved[HLML_ABI_RESERVED_SIZE]; // Absorbs growth in a newer libhlml; never read
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

// Layout of /dev/shm/mem_usage_accelN (habanalabs/hlml_shm.h, version 1)
struct hlml_shm_data {
  uint64_t version;
  uint64_t timestamp;
  uint64_t used_mem_in_bytes;
} __attribute__((packed));

// The transcribed 1.24 prefix of the structures libhlml writes into, and the
// total size with the reserved tail. Keep the offsets in sync with the
// definitions above; the reserve may grow if a future SDK outgrows it.
_Static_assert(offsetof(hlml_utilization_t, reserved) == 8,
               "hlml_utilization_t prefix does not match the HLML 1.24 ABI");
_Static_assert(sizeof(hlml_utilization_t) == 8 + HLML_ABI_RESERVED_SIZE,
               "hlml_utilization_t reserved tail is not accounted for");
_Static_assert(offsetof(hlml_memory_t, reserved) == 24, "hlml_memory_t prefix does not match the HLML 1.24 ABI");
_Static_assert(sizeof(hlml_memory_t) == 24 + HLML_ABI_RESERVED_SIZE,
               "hlml_memory_t reserved tail is not accounted for");
_Static_assert(sizeof(struct hlml_shm_data) == 24, "hlml_shm_data does not match the HLML shared memory ABI");

static hlml_return_t (*hlml_init)(void);
static hlml_return_t (*hlml_shutdown)(void);
static hlml_return_t (*hlml_device_get_count)(unsigned int *device_count);
static hlml_return_t (*hlml_device_get_handle_by_index)(unsigned int index, hlml_device_t *device);
static hlml_return_t (*hlml_device_get_name)(hlml_device_t device, char *name, unsigned int length);
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
static char already_enumerated_error[] = "The Gaudi devices are already enumerated\n";
static char dynamic_loader_error[256];
static const char *local_error_string = didnt_call_gpuinfo_init;

#define GAUDI_MAX_CHIP_SENSORS 16

struct gpu_info_gaudi {
  struct gpu_info base;
  hlml_device_t handle;
  unsigned minor;
  bool minor_valid;
  bool rdev_valid;
  dev_t rdev; // Device number of /dev/accel/accel<minor>
  char sysfs_path[64];
  char hwmon_path[128];
  unsigned n_chip_sensors;
  unsigned chip_sensors[GAUDI_MAX_CHIP_SENSORS]; // hwmon temperature channels located on the chip
  unsigned scan_generation_used;

  // Protected by sampler_lock once the sampler thread is running.
  // refresh_requested is a one-shot request set by refresh_dynamic_info and
  // cleared by the sampler when it picks the device up.
  bool refresh_requested;
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
static bool sample_pending;

// Result of the last /proc scan
struct gaudi_process_entry {
  pid_t pid;
  unsigned device_index;
  bool memory_valid;
  unsigned long long memory;
};
static unsigned scan_generation;
static const char *gaudi_proc_root = "/proc";
static unsigned scan_entries_count;
static unsigned scan_entries_size;
static struct gaudi_process_entry *scan_entries;

static bool gpuinfo_gaudi_init(void);
static void gpuinfo_gaudi_shutdown(void);
static const char *gpuinfo_gaudi_last_error_string(void);
static bool gpuinfo_gaudi_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_gaudi_populate_static_info(struct gpu_info *_gpu_info);
static bool read_device_attribute(const struct gpu_info_gaudi *gpu_info, const char *attribute, char *buf,
                                  size_t buf_size);
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
    .processing_unit = gpu_processing_unit_hpu,
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

static void clear_hlml_symbols(void) {
  hlml_init = NULL;
  hlml_shutdown = NULL;
  hlml_device_get_count = NULL;
  hlml_device_get_handle_by_index = NULL;
  hlml_device_get_name = NULL;
  hlml_device_get_minor_number = NULL;
  hlml_device_get_utilization_rates = NULL;
  hlml_device_get_memory_info = NULL;
  hlml_device_get_clock_info = NULL;
  hlml_device_get_max_clock_info = NULL;
  hlml_device_get_temperature = NULL;
  hlml_device_get_temperature_threshold = NULL;
  hlml_device_get_power_usage = NULL;
  hlml_device_get_power_management_limit = NULL;
  hlml_device_get_pcie_throughput = NULL;
}

static void set_loader_error(void) {
  const char *error = dlerror();
  snprintf(dynamic_loader_error, sizeof(dynamic_loader_error), "%s", error ? error : "unknown dynamic loader error");
  local_error_string = dynamic_loader_error;
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
    set_loader_error();
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
    clear_hlml_symbols();
    return false;
  }

  local_error_string = NULL;
  return true;

init_error_clean_exit:
  set_loader_error();
  dlclose(libhlml_handle);
  libhlml_handle = NULL;
  clear_hlml_symbols();
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
  sample_pending = false;

  if (libhlml_handle) {
    hlml_shutdown();
    dlclose(libhlml_handle);
    libhlml_handle = NULL;
    local_error_string = didnt_call_gpuinfo_init;
  }
  clear_hlml_symbols();

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
 * HLML classifies the hwmon temperature channels with a hardcoded per-ASIC
 * map (the ON_AIP entries in gaudi2_temp_type etc.) and returns the hottest
 * channel of that group. The lm-sensors configuration shipped with the driver
 * labels those channels "On Chip ...", but libsensors is only used for the
 * labels: hlml_device_get_temperature calls sensors_get_label() for every
 * sensor on every call, leaking about 260 bytes per call. To avoid that leak
 * we use the labels to identify the same channels and read their hwmon
 * values directly.
 *
 * Only the driver's configuration file is read. HLML does not use the labels
 * to classify the sensors, so relabeling or ignoring them in another
 * sensors.d file does not change HLML's result and is intentionally not
 * followed here.
 *
 * The number of selected channels is checked against the size of HLML's
 * per-ASIC ON_AIP map; if it differs, the caller falls back to the HLML
 * temperature call instead of reporting the max of a partial set.
 */

// Number of ON_AIP entries in HLML's hardcoded map, by driver device_type.
// Gaudi2 uses the same map for mezzanine and PCIe (8); Gaudi3 has separate
// maps for the OAM/mezzanine (16), the HL-338 PCIe card (12) and the FPGA
// (16). Unknown/pre-1.24 ASICs return 0 and are not validated.
static unsigned expected_aip_channels(const char *device_type) {
  if (strncmp(device_type, "GAUDI2", sizeof("GAUDI2") - 1) == 0)
    return 8;
  if (strncmp(device_type, "GAUDI3 HL-338", sizeof("GAUDI3 HL-338") - 1) == 0)
    return 12;
  if (strncmp(device_type, "GAUDI3", sizeof("GAUDI3") - 1) == 0)
    return 16;
  return 0;
}

static bool aip_channel_count_matches(const char *device_type, unsigned selected) {
  unsigned expected = expected_aip_channels(device_type);
  return expected == 0 || selected == expected;
}

static void parse_temperature_config(const char *config_path, const char *chip_id, bool *on_chip, bool *ignored) {
  FILE *conf = fopen(config_path, "r");
  if (!conf)
    return;
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
}

static unsigned select_chip_sensors(const bool *on_chip, const bool *ignored, unsigned *sensors, unsigned max) {
  unsigned count = 0;
  for (unsigned channel = 0; channel < GAUDI_MAX_HWMON_CHANNELS && count < max; ++channel) {
    if (on_chip[channel] && !ignored[channel])
      sensors[count++] = channel;
  }
  return count;
}

// libsensors names a PCI hwmon chip "<name>-pci-<address>", with
// address = (domain << 16) + (bus << 8) + (slot << 3) + function.
static bool pci_chip_address(const char *bdf, unsigned *address) {
  unsigned domain, bus, slot, function;
  if (!bdf || sscanf(bdf, "%x:%x:%x.%x", &domain, &bus, &slot, &function) != 4)
    return false;
  if (domain > 0xffff || bus > 0xff || slot > 0x1f || function > 0x7)
    return false;
  *address = (domain << 16) + (bus << 8) + (slot << 3) + function;
  return true;
}

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

  // The hwmon name is the board type (e.g. HL225). libsensors names the chip
  // "<name>-pci-<address>"; use the real address so exact "chip" statements
  // match as well as the wildcard ones.
  char name_path[160], chip_id[96];
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
  char device_type[32] = "";
  read_device_attribute(gpu_info, "device_type", device_type, sizeof(device_type));
  unsigned chip_address;
  if (pci_chip_address(gpu_info->base.pdev, &chip_address))
    snprintf(chip_id, sizeof(chip_id), "%s-pci-%04x", name, chip_address);
  else
    snprintf(chip_id, sizeof(chip_id), "%s-pci-0", name);

  bool on_chip[GAUDI_MAX_HWMON_CHANNELS] = {false};
  bool ignored[GAUDI_MAX_HWMON_CHANNELS] = {false};
  parse_temperature_config(HL_SENSORS_CONF, chip_id, on_chip, ignored);
  gpu_info->n_chip_sensors = select_chip_sensors(on_chip, ignored, gpu_info->chip_sensors, GAUDI_MAX_CHIP_SENSORS);

  if (!aip_channel_count_matches(device_type, gpu_info->n_chip_sensors)) {
    // A partial or relabeled configuration would report the maximum of the
    // wrong sensor set. Let gaudi_sample_device fall back to HLML instead.
    gpu_info->n_chip_sensors = 0;
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
  if (gaudi_devices) {
    local_error_string = already_enumerated_error;
    return false;
  }

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

    snprintf(gpu_info->base.pdev, sizeof(gpu_info->base.pdev), "gaudi-%x", i);

    if (hlml_device_get_minor_number(gpu_info->handle, &gpu_info->minor) == HLML_SUCCESS) {
      char dev_path[64];
      struct stat dev_stat;
      gpu_info->minor_valid = true;
      snprintf(dev_path, sizeof(dev_path), "/dev/accel/accel%u", gpu_info->minor);
      if (stat(dev_path, &dev_stat) == 0 && S_ISCHR(dev_stat.st_mode)) {
        gpu_info->rdev = dev_stat.st_rdev;
        gpu_info->rdev_valid = true;
      }
      snprintf(gpu_info->sysfs_path, sizeof(gpu_info->sysfs_path), "/sys/class/accel/accel%u/device", gpu_info->minor);
      char pci_addr[PDEV_LEN] = "";
      if (read_device_attribute(gpu_info, "pci_addr", pci_addr, sizeof(pci_addr)) && pci_addr[0])
        snprintf(gpu_info->base.pdev, sizeof(gpu_info->base.pdev), "%s", pci_addr);
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

// libhlml reports device_type from sysfs in upper case. Map the known ASIC
// strings to their display names; keep the old lower-casing fallback for
// future devices. The architecture is the ASIC family; the family is what the
// device name starts with and may include the board variant.
static const struct {
  const char *device_type;
  const char *architecture;
  const char *family;
} gaudi_asic_names[] = {
    {"GOYA", "Goya", "Goya"},
    {"GAUDI", "Gaudi", "Gaudi"},
    {"GAUDI SEC", "Gaudi SEC", "Gaudi SEC"},
    {"GAUDI HL2000M", "Gaudi", "Gaudi HL2000M"},
    {"GAUDI HL2000M SEC", "Gaudi SEC", "Gaudi HL2000M SEC"},
    {"GAUDI2", "Gaudi2", "Gaudi2"},
    {"GAUDI2B", "Gaudi2B", "Gaudi2B"},
    {"GAUDI2C", "Gaudi2C", "Gaudi2C"},
    {"GAUDI2D", "Gaudi2D", "Gaudi2D"},
    {"GAUDI3", "Gaudi3", "Gaudi3"},
    {"GAUDI3 FPGA", "Gaudi3 FPGA", "Gaudi3 FPGA"},
    {"GAUDI3 HL-338", "Gaudi3", "Gaudi3 HL-338"},
};

static void normalize_device_type(const char *device_type, char *architecture, size_t architecture_size, char *family,
                                  size_t family_size) {
  for (size_t i = 0; i < sizeof(gaudi_asic_names) / sizeof(gaudi_asic_names[0]); ++i) {
    if (strcmp(device_type, gaudi_asic_names[i].device_type) == 0) {
      snprintf(architecture, architecture_size, "%s", gaudi_asic_names[i].architecture);
      snprintf(family, family_size, "%s", gaudi_asic_names[i].family);
      return;
    }
  }
  snprintf(architecture, architecture_size, "%s", device_type);
  if (architecture[0])
    for (char *c = architecture + 1; *c; ++c)
      *c = (char)tolower((unsigned char)*c);
  snprintf(family, family_size, "%s", architecture);
}

// Whether the family already ends with the board name, ignoring case and
// separators such as the dash in "HL-338" or "HL2000M".
static bool ends_with_ignore_case(const char *family, const char *board_name) {
  size_t family_index = strlen(family);
  size_t board_index = strlen(board_name);
  while (board_index > 0) {
    while (board_index > 0 && !isalnum((unsigned char)board_name[board_index - 1]))
      --board_index;
    if (board_index == 0)
      break;
    while (family_index > 0 && !isalnum((unsigned char)family[family_index - 1]))
      --family_index;
    if (family_index == 0)
      return false;
    if (tolower((unsigned char)family[family_index - 1]) != tolower((unsigned char)board_name[board_index - 1]))
      return false;
    --family_index;
    --board_index;
  }
  return true;
}

static void compose_device_name(const char *family, const char *board_name, char *device_name,
                                size_t device_name_size) {
  if (family[0] && board_name[0] && !ends_with_ignore_case(family, board_name))
    snprintf(device_name, device_name_size, "%s %s", family, board_name);
  else
    snprintf(device_name, device_name_size, "%s", family[0] ? family : board_name);
}

static const char *device_memory_type(const char *family) {
  // Goya, which HLML can also enumerate, is the one Habana ASIC using DDR.
  return strncmp(family, "Goya", 4) == 0 ? "DDR4" : "HBM";
}

// Calls HLML for the device name and temperature thresholds. It must run
// before the first dynamic refresh starts the sampler thread; after that point
// the sampler is the only HLML caller. nvtop populates static info before the
// first refresh.
static void gpuinfo_gaudi_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_gaudi *gpu_info = container_of(_gpu_info, struct gpu_info_gaudi, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;

  static_info->integrated_graphics = false;
  static_info->encode_decode_shared = false;
  RESET_ALL(static_info->valid);

  // e.g. "Gaudi2 HL-225": the family comes from sysfs (GAUDI2), the board name from HLML
  char board_name[MAX_DEVICE_NAME] = "";
  char device_type[32] = "";
  char architecture[32] = "";
  char family[32] = "";
  last_hlml_return_status = hlml_device_get_name(gpu_info->handle, board_name, sizeof(board_name));
  board_name[sizeof(board_name) - 1] = '\0';
  if (last_hlml_return_status != HLML_SUCCESS)
    board_name[0] = '\0';
  if (read_device_attribute(gpu_info, "device_type", device_type, sizeof(device_type)))
    normalize_device_type(device_type, architecture, sizeof(architecture), family, sizeof(family));
  compose_device_name(family, board_name, static_info->device_name, MAX_DEVICE_NAME);
  if (static_info->device_name[0])
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
  if (architecture[0]) {
    snprintf(static_info->device_architecture, MAX_DEVICE_NAME, "%s", architecture);
    SET_VALID(gpuinfo_device_architecture_valid, static_info->valid);
  }

  // Every Gaudi generation uses HBM, which is also what hl-smi reports.
  // Goya, which HLML can also enumerate, uses DDR4.
  snprintf(static_info->memory_type, sizeof(static_info->memory_type), "%s", device_memory_type(family));
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

  // The AIP utilization can exceed 100%. The output structures are zeroed so a
  // library that returns success after writing only the fields of an older,
  // smaller structure cannot leave any read field indeterminate.
  hlml_utilization_t utilization = {0};
  if (hlml_device_get_utilization_rates(handle, &utilization) == HLML_SUCCESS)
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, utilization.aip > 100 ? 100 : utilization.aip);

  hlml_memory_t memory = {0};
  if (hlml_device_get_memory_info(handle, &memory) == HLML_SUCCESS && memory.total > 0) {
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, memory.total);
    SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, memory.used);
    SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, memory.free);
    unsigned mem_util = (unsigned)(memory.used * 100 / memory.total);
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, mem_util > 100 ? 100 : mem_util);
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

// The caller holds sampler_lock.
static bool gaudi_take_sample_request(struct gpu_info_gaudi *gpu_info) {
  if (!gpu_info->refresh_requested)
    return false;
  gpu_info->refresh_requested = false;
  return true;
}

static bool gaudi_any_sample_request(void) {
  bool pending = false;
  for (unsigned i = 0; i < gaudi_device_count; ++i)
    pending = pending || gaudi_devices[i].refresh_requested;
  return pending;
}

static void *gaudi_sampler_main(void *arg) {
  (void)arg;
  pthread_mutex_lock(&sampler_lock);
  while (!sampler_stop) {
    while (!sampler_stop && !sample_pending)
      pthread_cond_wait(&sampler_cond, &sampler_lock);

    for (unsigned i = 0; !sampler_stop && i < gaudi_device_count; ++i) {
      struct gpu_info_gaudi *gpu_info = &gaudi_devices[i];
      if (!gaudi_take_sample_request(gpu_info))
        continue;
      pthread_mutex_unlock(&sampler_lock);
      struct gpuinfo_dynamic_info sample;
      gaudi_sample_device(gpu_info, &sample);
      pthread_mutex_lock(&sampler_lock);
      gpu_info->sample = sample;
    }
    // Requests that arrived while sampling were picked up above if their turn
    // had not come yet. Any remaining request, for a device already visited,
    // keeps the sampler busy for another round.
    sample_pending = gaudi_any_sample_request();
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
  gpu_info->refresh_requested = true;
  sample_pending = true;
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

// Index N of a basename "<prefix>N", or -1. Accepts a " (deleted)" suffix.
static int path_index(const char *path, const char *prefix) {
  const char *name = strrchr(path, '/');
  name = name ? name + 1 : path;
  size_t prefix_len = strlen(prefix);
  size_t name_len = strlen(name);
  if (name_len <= prefix_len || strncmp(name, prefix, prefix_len) != 0 || !isdigit((unsigned char)name[prefix_len]))
    return -1;
  char *end;
  long index = strtol(name + prefix_len, &end, 10);
  if (*end != '\0' && strcmp(end, " (deleted)") != 0)
    return -1;
  return (int)index;
}

#define GAUDI_MAX_FD_PER_PROCESS 8

// Scan /proc for the processes having the compute node of a Gaudi device open
static void gaudi_scan_processes(void) {
  scan_entries_count = 0;
  scan_generation++;

  DIR *proc_dir = opendir(gaudi_proc_root);
  if (!proc_dir)
    return;

  struct dirent *proc_dent;
  while ((proc_dent = readdir(proc_dir)) != NULL) {
    if (proc_dent->d_type != DT_DIR || !isdigit((unsigned char)proc_dent->d_name[0]))
      continue;
    pid_t pid = (pid_t)atoi(proc_dent->d_name);

    char fd_dir_path[512];
    snprintf(fd_dir_path, sizeof(fd_dir_path), "%s/%s/fd", gaudi_proc_root, proc_dent->d_name);
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
          device_ns_index[n_devices] = path_index(link, "accel");
          n_devices++;
          break;
        }
      } else if (S_ISREG(fd_stat.st_mode) && fd_stat.st_size >= (off_t)sizeof(struct hlml_shm_data) &&
                 n_shm < GAUDI_MAX_FD_PER_PROCESS) {
        char link[256];
        ssize_t len = readlinkat(dirfd(fd_dir), fd_dent->d_name, link, sizeof(link) - 1);
        if (len <= 0)
          continue;
        link[len] = '\0';
        int ns_index = path_index(link, "mem_usage_accel");
        if (ns_index < 0)
          continue;
        int shm_fd = openat(dirfd(fd_dir), fd_dent->d_name, O_RDONLY);
        if (shm_fd < 0)
          continue;
        struct hlml_shm_data shm_data;
        // Version 1 is the only layout we understand; a newer layout needs
        // explicit support.
        if (pread(shm_fd, &shm_data, sizeof(shm_data), 0) == (ssize_t)sizeof(shm_data) && shm_data.version == 1) {
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
        // Prefer the process-visible index; also try the host minor in case a
        // container remapped the accel node.
        if ((device_ns_index[i] >= 0 && device_ns_index[i] == shm_ns_index[j]) ||
            (gaudi_devices[device_index[i]].minor_valid &&
             shm_ns_index[j] == (int)gaudi_devices[device_index[i]].minor)) {
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
  unsigned shm_owners = 0;
  for (unsigned i = 0; i < scan_entries_count; ++i) {
    if (scan_entries[i].device_index != device_index)
      continue;
    ++count;
    if (scan_entries[i].memory_valid)
      ++shm_owners;
  }

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
    }
    // If the framework did not publish its usage in the shm file, leave the
    // process memory unset (N/A) instead of reporting the device-wide used
    // memory, which also contains the 768 MiB the device reserves.
    //
    // Gaudi runs a single compute context per device. If only one process has
    // the compute node open, the device load is its load. If several processes
    // have it open (an inherited fd after a fork), the one publishing the
    // framework's memory is the process running the context; other holders are
    // not attributed the load. With no such process, the device load cannot be
    // split and stays unset.
    if (GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, gpu_util_rate) &&
        (count == 1 || (shm_owners == 1 && entry->memory_valid)))
      SET_GPUINFO_PROCESS(process, gpu_usage, dynamic_info->gpu_util_rate);
  }
}
