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
 * utilization is reported. The generic fdinfo sweep finds the processes holding the accel node open; this backend
 * registers that node and parses the driver's fdinfo for the memory each client holds.
 */

#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/time.h"

#include <dirent.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <uthash.h>

#define MAX_AMD_NPUS 4
#define AMDXDNA_KERNEL_DRIVER "amdxdna"
#define AMDXDNA_FDINFO_DRIVER "amdxdna_accel_driver"

struct amdxdna_cache_id {
  unsigned long long client_id;
  pid_t pid;
} __attribute__((__packed__));

#define HASH_FIND_CLIENT(head, key_ptr, out_ptr) HASH_FIND(hh, head, key_ptr, sizeof(struct amdxdna_cache_id), out_ptr)
#define HASH_ADD_CLIENT(head, in_ptr) HASH_ADD(hh, head, id, sizeof(struct amdxdna_cache_id), in_ptr)

struct amdxdna_process_info_cache {
  struct amdxdna_cache_id id;
  uint64_t busy_ns;
  nvtop_time last_measurement_tstamp;
  UT_hash_handle hh;
};

struct gpu_info_amd_npu {
  struct gpu_info base;
  char sysfs_dev[PATH_MAX]; // e.g. /sys/class/accel/accel0/device
  unsigned pci_device_id;
  unsigned pci_revision;
  struct amdxdna_process_info_cache *last_update_process_cache, *current_update_process_cache;
};

// What one fdinfo file of an amdxdna client reports (drivers/accel/amdxdna/amdxdna_pci_drv.c amdxdna_show_fdinfo)
struct amdxdna_fdinfo {
  bool is_amdxdna;
  bool have_client_id;
  unsigned long long client_id;
  bool have_busy;
  uint64_t busy_ns; // drm-engine-npu, cumulative engine busy time
  bool have_total_memory, have_internal, have_external;
  unsigned long long total_memory; // drm-total-memory, counts the device BOs twice (once alone, once in the heap BO)
  unsigned long long internal;     // Driver-internal BOs, including the heap BO
  unsigned long long external;     // Imported and user-created BOs
};

static const char *amd_npu_class_root = "/sys/class/accel";
static const char *amd_npu_dev_root = "/dev/accel";

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
    } else if (!strcmp(key, "drm-engine-npu")) {
      char *endptr;
      uint64_t busy_ns = strtoull(val, &endptr, 10);
      if (endptr != val && !strcmp(endptr, " ns")) {
        info->busy_ns = busy_ns;
        info->have_busy = true;
      }
    } else if (!strcmp(key, "drm-total-memory")) {
      info->have_total_memory = extract_drm_fdinfo_size(val, &info->total_memory);
    } else if (!strcmp(key, AMDXDNA_FDINFO_DRIVER "-internal-alloc")) {
      info->have_internal = extract_drm_fdinfo_size(val, &info->internal);
    } else if (!strcmp(key, AMDXDNA_FDINFO_DRIVER "-external-alloc")) {
      info->have_external = extract_drm_fdinfo_size(val, &info->external);
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

static void amdxdna_account_busy(struct gpu_info_amd_npu *npu, const struct amdxdna_fdinfo *client,
                                 struct gpu_process *process_info) {
  if (!client->have_client_id || !client->have_busy)
    return;

  nvtop_time current_time;
  nvtop_get_current_time(&current_time);
  struct amdxdna_cache_id key = {.client_id = client->client_id, .pid = process_info->pid};
  struct amdxdna_process_info_cache *cache_entry;
  HASH_FIND_CLIENT(npu->last_update_process_cache, &key, cache_entry);
  if (cache_entry) {
    uint64_t time_elapsed = nvtop_difftime_u64(cache_entry->last_measurement_tstamp, current_time);
    HASH_DEL(npu->last_update_process_cache, cache_entry);
    if (client->busy_ns >= cache_entry->busy_ns)
      SET_GPUINFO_PROCESS(process_info, gpu_usage,
                          busy_usage_from_time_usage_round(client->busy_ns, cache_entry->busy_ns, time_elapsed));
  } else {
    cache_entry = calloc(1, sizeof(*cache_entry));
    if (!cache_entry)
      return;
    cache_entry->id = key;
  }

  // The sweep accounts each client once per update, but keep the cache robust
  // if a callback is invoked twice for the same client.
  struct amdxdna_process_info_cache *duplicate;
  HASH_FIND_CLIENT(npu->current_update_process_cache, &key, duplicate);
  if (duplicate) {
    free(cache_entry);
    return;
  }

  cache_entry->busy_ns = client->busy_ns;
  cache_entry->last_measurement_tstamp = current_time;
  HASH_ADD_CLIENT(npu->current_update_process_cache, cache_entry);
}

static bool parse_drm_fdinfo_amdxdna(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {
  struct gpu_info_amd_npu *npu = container_of(info, struct gpu_info_amd_npu, base);
  struct amdxdna_fdinfo client;
  if (!amdxdna_parse_fdinfo(fdinfo_file, &client))
    return false;

  uint64_t memory;
  if (amdxdna_client_memory(&client, &memory))
    SET_GPUINFO_PROCESS(process_info, gpu_memory_usage, memory);
  // NPU work is compute by definition; the generic sweep also defaults unknown
  // NPU process types to compute.
  process_info->type |= gpu_process_compute;
  amdxdna_account_busy(npu, &client, process_info);
  return true;
}

static void free_process_cache(struct amdxdna_process_info_cache **cache) {
  if (!*cache)
    return;
  struct amdxdna_process_info_cache *cache_entry, *tmp;
  HASH_ITER(hh, *cache, cache_entry, tmp) {
    HASH_DEL(*cache, cache_entry);
    free(cache_entry);
  }
  *cache = NULL;
}

static void swap_process_cache_for_next_update(struct gpu_info_amd_npu *npu) {
  free_process_cache(&npu->last_update_process_cache);
  npu->last_update_process_cache = npu->current_update_process_cache;
  npu->current_update_process_cache = NULL;
}

static void gpuinfo_amd_npu_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_amd_npu *npu = container_of(_gpu_info, struct gpu_info_amd_npu, base);
  swap_process_cache_for_next_update(npu);
}

static bool gpuinfo_amd_npu_init(void) { return access(amd_npu_class_root, R_OK) == 0; }

// The process arrays are freed by gpuinfo_shutdown_info_extraction() before the vendors shut down
static void gpuinfo_amd_npu_shutdown(void) {
  for (unsigned i = 0; i < amd_npu_count; ++i) {
    // The fdinfo sweep keeps a pointer to each registered device: drop it before
    // freeing the array, or a later sweep writes through the freed memory.
    processinfo_drop_callback(&amd_npu_info[i].base);
    free_process_cache(&amd_npu_info[i].last_update_process_cache);
    free_process_cache(&amd_npu_info[i].current_update_process_cache);
  }
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

    // Only list an NPU whose device node the user can read. The fdinfo
    // dispatcher attributes processes through that node; without it the device
    // could not be monitored. Check the permission with access() rather than
    // opening the node: an open creates a driver client and binds the address
    // space of nvtop to the IOMMU (SVA), and a refused client (no PASID left
    // without a carveout, say) would hide the device for the whole session.
    char node_path[PATH_MAX];
    snprintf(node_path, sizeof(node_path), "%s/%s", amd_npu_dev_root, entry->d_name);
    if (access(node_path, R_OK))
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

    npu->base.vendor = &gpu_vendor_amd_npu;
    npu->base.processes_count = 0;
    npu->base.processes = NULL;
    npu->base.processes_array_size = 0;
    processinfo_register_fdinfo_callback(parse_drm_fdinfo_amdxdna, &npu->base, node_path);
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
  // Kernels before 7.4 expose no busy time: utilization stays unset rather than
  // 0%. Newer kernels print drm-engine-npu in fdinfo; the process callback turns
  // it into a per-process rate and the generic code sums those rates.
  // The NPU allocates from system RAM, so report system-wide memory pressure.
  gpuinfo_refresh_system_memory(dynamic_info);
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
