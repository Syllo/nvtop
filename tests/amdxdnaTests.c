/* SPDX-License-Identifier: GPL-3.0-or-later */

/* Include the backend to exercise its fdinfo parsing and device discovery on fixture files, without an NPU. */
#include "../src/extract_npuinfo_amd.c"

#include <ftw.h>

#define CHECK(condition)                                                                                               \
  do {                                                                                                                 \
    if (!(condition)) {                                                                                                \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #condition);                                                  \
      return false;                                                                                                    \
    }                                                                                                                  \
  } while (0)

#define MiB(x) ((uint64_t)(x) << 20)

static bool parse_fixture(const char *name, struct amdxdna_fdinfo *info) {
  char path[PATH_MAX];
  snprintf(path, sizeof(path), "%s/%s", AMDXDNA_FIXTURE_DIR, name);
  FILE *file = fopen(path, "r");
  if (!file) {
    perror(path);
    exit(EXIT_FAILURE);
  }
  bool ok = amdxdna_parse_fdinfo(file, info);
  fclose(file);
  return ok;
}

/* A process that only enumerated the NPU (PyTorch ROCm on a Ryzen AI 7 350). */
static bool fdinfo_idle(void) {
  struct amdxdna_fdinfo info;
  uint64_t memory;
  CHECK(parse_fixture("idle.fdinfo", &info));
  CHECK(info.have_client_id && info.client_id == 35632);
  CHECK(info.have_total_memory && info.total_memory == MiB(64));
  CHECK(amdxdna_client_memory(&info, &memory));
  CHECK(memory == MiB(64));
  return true;
}

/* A process with a hardware context: drm-total-memory counts the device BOs twice. */
static bool fdinfo_active(void) {
  struct amdxdna_fdinfo info;
  uint64_t memory;
  CHECK(parse_fixture("active.fdinfo", &info));
  CHECK(info.have_client_id && info.client_id == 35640);
  CHECK(info.have_total_memory && info.total_memory == MiB(84));
  CHECK(amdxdna_client_memory(&info, &memory));
  CHECK(memory == MiB(64) + MiB(12));
  return true;
}

/* Without the driver-specific keys, the total comes from drm-total-memory. */
static bool fdinfo_total_fallback(void) {
  struct amdxdna_fdinfo info;
  uint64_t memory;
  CHECK(parse_fixture("total_only.fdinfo", &info));
  CHECK(!info.have_internal && !info.have_external);
  CHECK(amdxdna_client_memory(&info, &memory));
  CHECK(memory == MiB(8));
  return true;
}

static bool fdinfo_other_driver(void) {
  struct amdxdna_fdinfo info;
  CHECK(!parse_fixture("amdgpu.fdinfo", &info));
  return true;
}

static int remove_entry(const char *path, const struct stat *st, int flag, struct FTW *ftw) {
  (void)st;
  (void)flag;
  (void)ftw;
  return remove(path);
}

static bool write_file(const char *dir, const char *name, const char *content) {
  char path[PATH_MAX];
  snprintf(path, sizeof(path), "%s/%s", dir, name);
  FILE *file = fopen(path, "w");
  if (!file)
    return false;
  fputs(content, file);
  return fclose(file) == 0;
}

/* Lay out /sys/class/accel/<node> -> /sys/devices/<pci> with its driver link, as the kernel does. */
static bool add_accel_node(const char *root, const char *node, const char *pci, const char *driver,
                           const char *device_id, const char *revision, const char *vbnv, const char *dev) {
  char path[PATH_MAX], target[PATH_MAX];
  snprintf(path, sizeof(path), "%s/devices/%s", root, pci);
  if (mkdir(path, 0755))
    return false;
  if (!write_file(path, "device", device_id) || !write_file(path, "revision", revision))
    return false;
  if (vbnv && !write_file(path, "vbnv", vbnv))
    return false;
  snprintf(target, sizeof(target), "../../bus/pci/drivers/%s", driver);
  snprintf(path, sizeof(path), "%s/devices/%s/driver", root, pci);
  if (symlink(target, path))
    return false;
  snprintf(path, sizeof(path), "%s/class/%s", root, node);
  if (mkdir(path, 0755) || !write_file(path, "dev", dev))
    return false;
  snprintf(target, sizeof(target), "../../devices/%s", pci);
  snprintf(path, sizeof(path), "%s/class/%s/device", root, node);
  return symlink(target, path) == 0;
}

static FILE *open_fixture(const char *name) {
  char path[PATH_MAX];
  snprintf(path, sizeof(path), "%s/%s", AMDXDNA_FIXTURE_DIR, name);
  FILE *file = fopen(path, "r");
  if (!file)
    perror(path);
  return file;
}

static bool fdinfo_callback(void) {
  const struct {
    const char *name;
    unsigned long long memory;
    bool accepted;
  } cases[] = {
      {"idle.fdinfo", MiB(64), true},
      {"active.fdinfo", MiB(76), true},
      {"total_only.fdinfo", MiB(8), true},
      {"amdgpu.fdinfo", 0, false},
  };
  for (unsigned i = 0; i < sizeof(cases) / sizeof(*cases); ++i) {
    FILE *file = open_fixture(cases[i].name);
    CHECK(file);
    struct gpu_info_amd_npu npu = {0};
    struct gpu_process process = {0};
    bool accepted = parse_drm_fdinfo_amdxdna(&npu.base, file, &process);
    fclose(file);
    CHECK(accepted == cases[i].accepted);
    if (accepted) {
      CHECK(process.type == gpu_process_compute);
      CHECK(GPUINFO_PROCESS_FIELD_VALID(&process, gpu_memory_usage));
      CHECK(process.gpu_memory_usage == cases[i].memory);
    }
    free_process_cache(&npu.last_update_process_cache);
    free_process_cache(&npu.current_update_process_cache);
  }
  return true;
}

static bool process_utilization(void) {
  struct gpu_info_amd_npu npu = {0};
  struct gpu_process process = {0};
  process.pid = 4242;

  FILE *file = open_fixture("engine_start.fdinfo");
  CHECK(file);
  CHECK(parse_drm_fdinfo_amdxdna(&npu.base, file, &process));
  fclose(file);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&process, gpu_usage));

  swap_process_cache_for_next_update(&npu);
  CHECK(npu.last_update_process_cache != NULL);

  // Make the two samples one second apart so the 500 ms busy delta is ~50%.
  nvtop_time elapsed;
  nvtop_get_current_time(&elapsed);
  npu.last_update_process_cache->last_measurement_tstamp = elapsed;
  npu.last_update_process_cache->last_measurement_tstamp.tv_sec -= 1;

  struct gpu_process next = {0};
  next.pid = 4242;
  file = open_fixture("engine_busy.fdinfo");
  CHECK(file);
  CHECK(parse_drm_fdinfo_amdxdna(&npu.base, file, &next));
  fclose(file);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&next, gpu_usage));
  CHECK(next.gpu_usage >= 45 && next.gpu_usage <= 55);

  free_process_cache(&npu.last_update_process_cache);
  free_process_cache(&npu.current_update_process_cache);
  return true;
}

static bool memory_display(void) {
  struct gpu_info_amd_npu npu = {0};
  struct gpuinfo_dynamic_info *dynamic_info = &npu.base.dynamic_info;
  npu.base.static_info.memory_shared_with_host = true;
  SET_GPUINFO_DYNAMIC(dynamic_info, system_total_memory, MiB(16));
  SET_GPUINFO_DYNAMIC(dynamic_info, system_free_memory, MiB(8));

  // An idle NPU with a successful process query reports zero allocations, and
  // MemAvailable is the total, as for any shared-memory device.
  npu.base.processes_count = 0;
  npu.base.processes_query_succeeded = true;
  gpuinfo_fix_shared_memory_from_process_info(&npu.base);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, used_memory));
  CHECK(dynamic_info->used_memory == 0);
  CHECK(dynamic_info->free_memory == MiB(8));
  CHECK(dynamic_info->total_memory == MiB(8));
  struct gpuinfo_memory_display display = gpuinfo_get_memory_display(&npu.base, false);
  CHECK(!display.system_memory);
  CHECK(display.used_memory_valid && display.used_memory == 0);
  CHECK(display.total_memory_valid && display.total_memory == MiB(8));

  // The used memory is the process sum; the total adds MemAvailable.
  struct gpu_process process = {0};
  SET_GPUINFO_PROCESS(&process, gpu_memory_usage, MiB(2));
  npu.base.processes = &process;
  npu.base.processes_count = 1;
  RESET_GPUINFO_DYNAMIC(dynamic_info, used_memory);
  RESET_GPUINFO_DYNAMIC(dynamic_info, free_memory);
  RESET_GPUINFO_DYNAMIC(dynamic_info, total_memory);
  RESET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate);
  gpuinfo_fix_shared_memory_from_process_info(&npu.base);
  CHECK(dynamic_info->used_memory == MiB(2));
  CHECK(dynamic_info->free_memory == MiB(8));
  CHECK(dynamic_info->total_memory == MiB(10));
  display = gpuinfo_get_memory_display(&npu.base, false);
  CHECK(!display.system_memory);
  CHECK(display.used_memory == MiB(2) && display.total_memory == MiB(10));
  npu.base.processes = NULL;

  // An unavailable process query uses the shared-memory RAM fallback, as for
  // other unified-memory devices.
  npu.base.processes_count = 0;
  npu.base.processes_query_succeeded = false;
  RESET_GPUINFO_DYNAMIC(dynamic_info, used_memory);
  RESET_GPUINFO_DYNAMIC(dynamic_info, free_memory);
  RESET_GPUINFO_DYNAMIC(dynamic_info, total_memory);
  RESET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate);
  gpuinfo_fix_shared_memory_from_process_info(&npu.base);
  CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, used_memory));
  display = gpuinfo_get_memory_display(&npu.base, false);
  CHECK(display.system_memory);
  CHECK(display.total_memory == MiB(16) && display.used_memory == MiB(8));
  return true;
}

static bool device_discovery(void) {
  char root[] = "/tmp/nvtop-amdxdna-XXXXXX";
  char path[PATH_MAX];
  CHECK(mkdtemp(root) != NULL);
  snprintf(path, sizeof(path), "%s/devices", root);
  CHECK(mkdir(path, 0755) == 0);
  snprintf(path, sizeof(path), "%s/class", root);
  CHECK(mkdir(path, 0755) == 0);
  snprintf(path, sizeof(path), "%s/dev", root);
  CHECK(mkdir(path, 0755) == 0);

  CHECK(add_accel_node(root, "accel0", "0000:c5:00.1", "amdxdna", "0x17f0\n", "0x20\n", "NPU Krackan 1\n", "261:0\n"));
  CHECK(add_accel_node(root, "accel1", "0000:00:0b.0", "intel_vpu", "0x7d1d\n", "0x04\n", NULL, "261:1\n"));
  CHECK(add_accel_node(root, "accel2", "0000:c6:00.1", "amdxdna", "0x17f2\n", "0x10\n", NULL, "261:2\n"));
  /* An amdxdna device whose node cannot be accessed is ignored. */
  CHECK(add_accel_node(root, "accel3", "0000:c7:00.1", "amdxdna", "0x17f3\n", "0x10\n", NULL, "261:3\n"));

  static char class_root[PATH_MAX];
  snprintf(class_root, sizeof(class_root), "%s/class", root);
  amd_npu_class_root = class_root;
  static char dev_root[PATH_MAX];
  snprintf(dev_root, sizeof(dev_root), "%s/dev/accel", root);
  CHECK(mkdir(dev_root, 0755) == 0);
  CHECK(write_file(dev_root, "accel0", ""));
  CHECK(write_file(dev_root, "accel2", ""));
  amd_npu_dev_root = dev_root;

  LIST_HEAD(devices);
  unsigned count = 0;
  CHECK(gpuinfo_amd_npu_init());
  CHECK(gpuinfo_amd_npu_get_device_handles(&devices, &count));
  CHECK(count == 2);

  bool seen_krackan = false, seen_npu3 = false;
  for (unsigned i = 0; i < amd_npu_count; ++i) {
    struct gpu_info_amd_npu *npu = &amd_npu_info[i];
    gpuinfo_amd_npu_populate_static_info(&npu->base);
    CHECK(npu->base.vendor->processing_unit == gpu_processing_unit_npu);
    CHECK(GPUINFO_STATIC_FIELD_VALID(&npu->base.static_info, device_name));
    CHECK(GPUINFO_STATIC_FIELD_VALID(&npu->base.static_info, device_architecture));
    if (!strcmp(npu->base.pdev, "0000:c5:00.1")) {
      seen_krackan = true;
      /* As read on a Krackan laptop: the firmware names the product, the PCI revision selects the NPU6 setup. */
      CHECK(!strcmp(npu->base.static_info.device_name, "AMD NPU (Krackan 1)"));
      CHECK(!strcmp(npu->base.static_info.device_architecture, "NPU6"));
    } else if (!strcmp(npu->base.pdev, "0000:c6:00.1")) {
      seen_npu3 = true;
      CHECK(!strcmp(npu->base.static_info.device_name, "AMD NPU [1022:17f2]"));
      CHECK(!strcmp(npu->base.static_info.device_architecture, "NPU3"));
    }
    /* The driver exposes no busy time: the utilization must stay unset. */
    gpuinfo_amd_npu_refresh_dynamic_info(&npu->base);
    CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&npu->base.dynamic_info, gpu_util_rate));
  }
  CHECK(seen_krackan && seen_npu3);

  /* Names without sysfs: only 0x1502 is a single product family; the driver's fallback vbnv is not a name. */
  char name[64];
  amd_npu_device_name(NULL, 0x1502, 0x00, name, sizeof(name));
  CHECK(!strcmp(name, "AMD NPU (Phoenix/Hawk Point)"));
  amd_npu_device_name("RyzenAI-npu4", 0x17f0, 0x10, name, sizeof(name));
  CHECK(!strcmp(name, "AMD NPU [1022:17f0]"));
  amd_npu_device_name("NPU Strix Halo", 0x17f0, 0x20, name, sizeof(name));
  CHECK(!strcmp(name, "AMD NPU (Strix Halo)"));
  amd_npu_device_name("NPU ", 0x17f0, 0x11, name, sizeof(name));
  CHECK(!strcmp(name, "AMD NPU [1022:17f0]"));
  CHECK(amd_npu_lookup_model(0x17f0, 0x30) == NULL);

  /* Enumerating twice is refused instead of leaking the device array. */
  LIST_HEAD(again);
  CHECK(!gpuinfo_amd_npu_get_device_handles(&again, &count));
  CHECK(count == 0);

  gpuinfo_amd_npu_shutdown();
  CHECK(amd_npu_info == NULL && amd_npu_count == 0);
  nftw(root, remove_entry, 16, FTW_DEPTH | FTW_PHYS);
  return true;
}

int main(int argc, char **argv) {
  const struct {
    const char *name;
    bool (*run)(void);
  } tests[] = {
      {"fdinfo_idle", fdinfo_idle},
      {"fdinfo_active", fdinfo_active},
      {"fdinfo_total_fallback", fdinfo_total_fallback},
      {"fdinfo_other_driver", fdinfo_other_driver},
      {"fdinfo_callback", fdinfo_callback},
      {"process_utilization", process_utilization},
      {"memory_display", memory_display},
      {"device_discovery", device_discovery},
  };
  for (unsigned i = 0; argc == 2 && i < sizeof(tests) / sizeof(*tests); ++i)
    if (!strcmp(argv[1], tests[i].name))
      return tests[i].run() ? EXIT_SUCCESS : EXIT_FAILURE;
  fprintf(stderr, "Unknown test case\n");
  return EXIT_FAILURE;
}
