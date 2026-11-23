/* SPDX-License-Identifier: GPL-3.0-or-later */

/* Include the backend to exercise its fdinfo parsing and device discovery on fixture files, without an NPU. */
#include "../src/extract_npuinfo_intel.c"

#include <ftw.h>

#define CHECK(condition)                                                                                               \
  do {                                                                                                                 \
    if (!(condition)) {                                                                                                \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #condition);                                                  \
      return false;                                                                                                    \
    }                                                                                                                  \
  } while (0)

#define MiB(x) ((uint64_t)(x) << 20)

static bool parse_fixture(const char *name, struct intel_vpu_fdinfo *info) {
  char path[PATH_MAX];
  snprintf(path, sizeof(path), "%s/%s", IVPU_FIXTURE_DIR, name);
  FILE *file = fopen(path, "r");
  if (!file) {
    perror(path);
    exit(EXIT_FAILURE);
  }
  bool ok = intel_vpu_parse_fdinfo(file, info);
  fclose(file);
  return ok;
}

/* A client with 32 MiB of private buffers and 32 MiB it exported: the exported
 * buffers are both resident and shared. */
static bool fdinfo_active(void) {
  struct intel_vpu_fdinfo info;
  uint64_t memory, shared;
  CHECK(parse_fixture("active.fdinfo", &info));
  CHECK(info.have_total && info.total == MiB(64));
  CHECK(info.have_resident && info.resident == MiB(64));
  CHECK(info.have_shared && info.shared == MiB(32));
  CHECK(intel_vpu_client_memory(&info, &memory, &shared));
  /* drm-resident-memory already includes the exported buffers: adding
   * drm-shared-memory would count them twice. */
  CHECK(memory == MiB(64));
  CHECK(shared == MiB(32));
  return true;
}

/* A client that opened the node without allocating anything. */
static bool fdinfo_idle(void) {
  struct intel_vpu_fdinfo info;
  uint64_t memory, shared;
  CHECK(parse_fixture("idle.fdinfo", &info));
  CHECK(info.have_total && info.total == 0);
  /* Without any resident buffer, the kernel prints no drm-resident-memory. */
  CHECK(!info.have_resident);
  CHECK(info.have_shared && info.shared == 0);
  CHECK(intel_vpu_client_memory(&info, &memory, &shared));
  CHECK(memory == 0 && shared == 0);
  return true;
}

/* Only imported buffers: nothing resident, everything shared. Their pages belong
 * to the exporting device, which accounts them. */
static bool fdinfo_imported(void) {
  struct intel_vpu_fdinfo info;
  uint64_t memory, shared;
  CHECK(parse_fixture("imported.fdinfo", &info));
  CHECK(!info.have_resident);
  CHECK(info.have_shared && info.shared == MiB(32));
  CHECK(intel_vpu_client_memory(&info, &memory, &shared));
  CHECK(memory == 0 && shared == 0);
  return true;
}

/* Buffers allocated but not backed by pages yet: the kernel prints no
 * drm-resident-memory, and drm-total-memory does not stand in for it. */
static bool fdinfo_unpopulated(void) {
  struct intel_vpu_fdinfo info;
  uint64_t memory, shared;
  CHECK(parse_fixture("unpopulated.fdinfo", &info));
  CHECK(!info.have_resident);
  CHECK(info.have_total && info.total == MiB(16));
  CHECK(intel_vpu_client_memory(&info, &memory, &shared));
  CHECK(memory == 0 && shared == 0);
  return true;
}

static bool fdinfo_no_memory(void) {
  struct intel_vpu_fdinfo info;
  uint64_t memory, shared;
  CHECK(parse_fixture("no_memory.fdinfo", &info));
  CHECK(!info.have_total && !info.have_resident && !info.have_shared);
  CHECK(!intel_vpu_client_memory(&info, &memory, &shared));
  return true;
}

/* The shared DRM size parser rejects signs, leading spaces, unknown units and
 * values that do not fit. */
static bool fdinfo_size(void) {
  const struct {
    const char *val;
    bool ok;
    unsigned long long bytes;
  } cases[] = {
      {"0", true, 0},
      {"123", true, 123},
      {"7 B", true, 7},
      {"4 KiB", true, 4ULL << 10},
      {"3 kB", true, 3ULL << 10},
      {"2 MiB", true, 2ULL << 20},
      {"1 GiB", true, 1ULL << 30},
      {"18446744073709551615", true, ULLONG_MAX},
      {"", false, 0},
      {"-1", false, 0},
      {" 5", false, 0},
      {"abc", false, 0},
      {"5 TiB", true, 5ULL << 40},
      {"5 PiB", false, 0},
      {"16777216 TiB", false, 0},
      {"18446744073709551616", false, 0},
      {"18014398509481984 KiB", false, 0},
  };
  for (unsigned i = 0; i < sizeof(cases) / sizeof(*cases); ++i) {
    unsigned long long bytes = 0;
    CHECK(extract_drm_fdinfo_size(cases[i].val, &bytes) == cases[i].ok);
    if (cases[i].ok)
      CHECK(bytes == cases[i].bytes);
  }
  return true;
}

static bool fdinfo_other_driver(void) {
  struct intel_vpu_fdinfo info;
  CHECK(!parse_fixture("amdxdna.fdinfo", &info));
  return true;
}

static FILE *open_fixture(const char *name) {
  char path[PATH_MAX];
  snprintf(path, sizeof(path), "%s/%s", IVPU_FIXTURE_DIR, name);
  FILE *file = fopen(path, "r");
  if (!file)
    perror(path);
  return file;
}

static bool fdinfo_callback(void) {
  const struct {
    const char *name;
    bool accepted;
    bool have_memory;
    uint64_t memory;
    bool have_shared;
    uint64_t shared;
  } cases[] = {
      {"active.fdinfo", true, true, MiB(64), true, MiB(32)}, {"idle.fdinfo", true, true, 0, false, 0},
      {"imported.fdinfo", true, true, 0, false, 0},          {"unpopulated.fdinfo", true, true, 0, false, 0},
      {"no_memory.fdinfo", true, false, 0, false, 0},        {"amdxdna.fdinfo", false, false, 0, false, 0},
  };
  for (unsigned i = 0; i < sizeof(cases) / sizeof(*cases); ++i) {
    FILE *file = open_fixture(cases[i].name);
    CHECK(file);
    struct gpu_info_intel_npu npu = {0};
    struct gpu_process process = {0};
    bool accepted = parse_drm_fdinfo_intel_vpu(&npu.base, file, &process);
    fclose(file);
    CHECK(accepted == cases[i].accepted);
    if (accepted) {
      CHECK(process.type == gpu_process_compute);
      CHECK(!!GPUINFO_PROCESS_FIELD_VALID(&process, gpu_memory_usage) == cases[i].have_memory);
      if (cases[i].have_memory)
        CHECK(process.gpu_memory_usage == cases[i].memory);
      CHECK(!!GPUINFO_PROCESS_FIELD_VALID(&process, gpu_memory_shared) == cases[i].have_shared);
      if (cases[i].have_shared)
        CHECK(process.gpu_memory_shared == cases[i].shared);
    }
  }
  return true;
}

/* A complete sweep in which no client reports memory, whether it found no
 * client or only clients of a kernel printing no memory keys, reports nothing
 * allocated. A failed or skipped sweep leaves the row to the system-wide RAM
 * fallback. */
static bool zero_usage_claim(void) {
  const struct {
    bool query_succeeded;
    unsigned processes;
    bool process_memory;
    bool succeeded;
    bool zero_used;
  } cases[] = {
      {false, 0, false, false, false},
      {true, 0, false, true, true},
      {true, 1, false, true, true},
      {true, 1, true, true, false},
  };
  for (unsigned i = 0; i < sizeof(cases) / sizeof(*cases); ++i) {
    struct gpu_info_intel_npu npu = {0};
    struct gpuinfo_dynamic_info *dynamic_info = &npu.base.dynamic_info;
    struct gpu_process process = {0};
    if (cases[i].process_memory)
      SET_GPUINFO_PROCESS(&process, gpu_memory_usage, MiB(2));
    npu.base.processes_query_succeeded = cases[i].query_succeeded;
    npu.base.processes = &process;
    npu.base.processes_count = cases[i].processes;
    SET_GPUINFO_DYNAMIC(dynamic_info, system_free_memory, MiB(8));
    gpuinfo_intel_npu_get_running_processes(&npu.base);
    CHECK(npu.base.processes_query_succeeded == cases[i].succeeded);
    CHECK(!!GPUINFO_DYNAMIC_FIELD_VALID(dynamic_info, used_memory) == cases[i].zero_used);
    if (cases[i].zero_used) {
      CHECK(dynamic_info->used_memory == 0);
      CHECK(dynamic_info->free_memory == MiB(8) && dynamic_info->total_memory == MiB(8));
      CHECK(dynamic_info->mem_util_rate == 0);
    }
  }
  return true;
}

static bool memory_display(void) {
  struct gpu_info_intel_npu npu = {0};
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

  // An unavailable process query uses the shared-memory RAM fallback.
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
                           const char *device_id) {
  char path[PATH_MAX], target[PATH_MAX];
  snprintf(path, sizeof(path), "%s/devices/%s", root, pci);
  if (mkdir(path, 0755))
    return false;
  if (!write_file(path, "device", device_id))
    return false;
  snprintf(target, sizeof(target), "../../bus/pci/drivers/%s", driver);
  snprintf(path, sizeof(path), "%s/devices/%s/driver", root, pci);
  if (symlink(target, path))
    return false;
  snprintf(path, sizeof(path), "%s/class/%s", root, node);
  if (mkdir(path, 0755))
    return false;
  snprintf(target, sizeof(target), "../../devices/%s", pci);
  snprintf(path, sizeof(path), "%s/class/%s/device", root, node);
  return symlink(target, path) == 0;
}

static bool device_discovery(void) {
  char root[] = "/tmp/nvtop-ivpu-XXXXXX";
  char path[PATH_MAX];
  CHECK(mkdtemp(root) != NULL);
  snprintf(path, sizeof(path), "%s/devices", root);
  CHECK(mkdir(path, 0755) == 0);
  snprintf(path, sizeof(path), "%s/class", root);
  CHECK(mkdir(path, 0755) == 0);
  snprintf(path, sizeof(path), "%s/dev", root);
  CHECK(mkdir(path, 0755) == 0);

  CHECK(add_accel_node(root, "accel0", "0000:00:0b.0", "intel_vpu", "0x7d1d\n"));
  CHECK(add_accel_node(root, "accel1", "0000:c5:00.1", "amdxdna", "0x17f0\n"));
  /* An intel_vpu device without a device node is still listed; only its process
   * attribution is unavailable. */
  CHECK(add_accel_node(root, "accel2", "0000:00:0b.1", "intel_vpu", "0xad1d\n"));
  /* An accel device of another driver is ignored. */
  CHECK(add_accel_node(root, "accel3", "0000:00:0b.2", "amdgpu", "0x1234\n"));

  static char class_root[PATH_MAX];
  snprintf(class_root, sizeof(class_root), "%s/class", root);
  intel_npu_class_root = class_root;
  static char dev_root[PATH_MAX];
  snprintf(dev_root, sizeof(dev_root), "%s/dev/accel", root);
  CHECK(mkdir(dev_root, 0755) == 0);
  /* The registration needs an accessible character device. */
  snprintf(path, sizeof(path), "%s/accel0", dev_root);
  CHECK(symlink("/dev/null", path) == 0);
  intel_npu_dev_root = dev_root;

  LIST_HEAD(devices);
  unsigned count = 0;
  CHECK(gpuinfo_intel_npu_init());
  CHECK(gpuinfo_intel_npu_get_device_handles(&devices, &count));
  CHECK(count == 2);

  bool seen_meteor_lake = false, seen_arrow_lake = false;
  for (unsigned i = 0; i < intel_npu_count; ++i) {
    struct gpu_info_intel_npu *device = &intel_npu_info[i];
    gpuinfo_intel_npu_populate_static_info(&device->base);
    CHECK(device->base.vendor->processing_unit == gpu_processing_unit_npu);
    CHECK(device->base.static_info.integrated_graphics);
    CHECK(device->base.static_info.memory_shared_with_host);
    /* The driver exposes no busy time in fdinfo, so the device utilization stays
     * unavailable without the sysfs counter. */
    gpuinfo_intel_npu_refresh_dynamic_info(&device->base);
    CHECK(!GPUINFO_DYNAMIC_FIELD_VALID(&device->base.dynamic_info, gpu_util_rate));
    if (!strcmp(device->base.pdev, "0000:00:0b.0")) {
      seen_meteor_lake = true;
      CHECK(!strcmp(device->base.static_info.device_name, "Intel NPU (Meteor Lake)"));
      CHECK(!strcmp(device->base.static_info.device_architecture, "Meteor Lake"));
    } else if (!strcmp(device->base.pdev, "0000:00:0b.1")) {
      seen_arrow_lake = true;
      CHECK(!strcmp(device->base.static_info.device_name, "Intel NPU (Arrow Lake)"));
      CHECK(!strcmp(device->base.static_info.device_architecture, "Arrow Lake"));
    }
  }
  CHECK(seen_meteor_lake && seen_arrow_lake);

  struct gpu_info_intel_npu *npu = &intel_npu_info[0];

  /* Unknown PCI IDs are still listed, named by their ID. */
  unsigned known_id = npu->pci_device_id;
  npu->pci_device_id = 0xffff;
  gpuinfo_intel_npu_populate_static_info(&npu->base);
  CHECK(!strcmp(npu->base.static_info.device_name, "Intel NPU [8086:ffff]"));
  CHECK(!GPUINFO_STATIC_FIELD_VALID(&npu->base.static_info, device_architecture));
  npu->pci_device_id = known_id;

  /* Enumerating twice is refused instead of leaking the device array. */
  LIST_HEAD(again);
  CHECK(!gpuinfo_intel_npu_get_device_handles(&again, &count));
  CHECK(count == 0);

  gpuinfo_intel_npu_shutdown();
  CHECK(intel_npu_info == NULL && intel_npu_count == 0);
  nftw(root, remove_entry, 16, FTW_DEPTH | FTW_PHYS);
  return true;
}

int main(int argc, char **argv) {
  const struct {
    const char *name;
    bool (*run)(void);
  } tests[] = {
      {"fdinfo_active", fdinfo_active},       {"fdinfo_idle", fdinfo_idle},
      {"fdinfo_imported", fdinfo_imported},   {"fdinfo_unpopulated", fdinfo_unpopulated},
      {"fdinfo_no_memory", fdinfo_no_memory}, {"fdinfo_other_driver", fdinfo_other_driver},
      {"fdinfo_size", fdinfo_size},           {"fdinfo_callback", fdinfo_callback},
      {"memory_display", memory_display},     {"device_discovery", device_discovery},
      {"zero_usage_claim", zero_usage_claim},
  };
  for (unsigned i = 0; argc == 2 && i < sizeof(tests) / sizeof(*tests); ++i)
    if (!strcmp(argv[1], tests[i].name))
      return tests[i].run() ? EXIT_SUCCESS : EXIT_FAILURE;
  fprintf(stderr, "Unknown test case\n");
  return EXIT_FAILURE;
}
