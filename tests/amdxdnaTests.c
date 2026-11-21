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

/* A process that only enumerated the NPU (PyTorch ROCm on a Ryzen AI 7 350): no device heap. */
static bool fdinfo_idle(void) {
  struct amdxdna_fdinfo info;
  uint64_t memory;
  CHECK(parse_fixture("idle.fdinfo", &info));
  CHECK(info.have_client_id && info.client_id == 35632);
  CHECK(info.have_total_memory && info.total_memory == MiB(64));
  CHECK(info.have_heap && info.heap == 0);
  CHECK(amdxdna_client_memory(&info, &memory));
  CHECK(memory == MiB(64));
  CHECK(!amdxdna_client_active(&info));
  return true;
}

/* A process with a hardware context: the device heap is allocated and drm-total-memory counts its BOs twice. */
static bool fdinfo_active(void) {
  struct amdxdna_fdinfo info;
  uint64_t memory;
  CHECK(parse_fixture("active.fdinfo", &info));
  CHECK(info.have_client_id && info.client_id == 35640);
  CHECK(info.have_heap && info.heap == MiB(8));
  CHECK(info.have_total_memory && info.total_memory == MiB(84));
  CHECK(amdxdna_client_memory(&info, &memory));
  CHECK(memory == MiB(64) + MiB(12));
  CHECK(amdxdna_client_active(&info));
  return true;
}

/* Without the driver-specific keys, the total comes from drm-total-memory and the activity is unknown. */
static bool fdinfo_total_fallback(void) {
  struct amdxdna_fdinfo info;
  uint64_t memory;
  CHECK(parse_fixture("total_only.fdinfo", &info));
  CHECK(!info.have_heap && !info.have_internal && !info.have_external);
  CHECK(amdxdna_client_memory(&info, &memory));
  CHECK(memory == MiB(8));
  CHECK(!amdxdna_client_active(&info));
  return true;
}

static bool fdinfo_other_driver(void) {
  struct amdxdna_fdinfo info;
  CHECK(!parse_fixture("amdgpu.fdinfo", &info));
  return true;
}

static bool process_accounting(void) {
  struct gpu_info gpu;
  struct amdxdna_pid_clients clients = {.pid = 0, .count = 0};
  struct amdxdna_fdinfo idle, active, dup;

  memset(&gpu, 0, sizeof(gpu));
  CHECK(parse_fixture("idle.fdinfo", &idle));
  CHECK(parse_fixture("active.fdinfo", &active));
  dup = idle;

  /* An idle client keeps the unknown type. */
  amd_npu_account_client(&gpu, &clients, 100, &idle);
  CHECK(gpu.processes_count == 1);
  CHECK(gpu.processes[0].pid == 100);
  CHECK(gpu.processes[0].type == gpu_process_unknown);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&gpu.processes[0], gpu_memory_usage));
  CHECK(gpu.processes[0].gpu_memory_usage == MiB(64));
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&gpu.processes[0], gpu_usage));

  /* A duplicated fd of the same client is not counted twice. */
  amd_npu_account_client(&gpu, &clients, 100, &dup);
  CHECK(gpu.processes_count == 1);
  CHECK(gpu.processes[0].gpu_memory_usage == MiB(64));

  /* A second client of the same process that submitted work makes it a compute process. */
  amd_npu_account_client(&gpu, &clients, 100, &active);
  CHECK(gpu.processes_count == 1);
  CHECK(gpu.processes[0].type == gpu_process_compute);
  CHECK(gpu.processes[0].gpu_memory_usage == MiB(64) + MiB(76));

  /* Another process gets its own entry, even with the same client id seen in the previous process. */
  amd_npu_account_client(&gpu, &clients, 200, &idle);
  CHECK(gpu.processes_count == 2);
  CHECK(gpu.processes[1].pid == 200);
  CHECK(gpu.processes[1].type == gpu_process_unknown);
  CHECK(gpu.processes[1].gpu_memory_usage == MiB(64));

  free(gpu.processes);
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

static bool copy_fixture(const char *name, const char *dest) {
  char path[PATH_MAX], buf[4096];
  snprintf(path, sizeof(path), "%s/%s", AMDXDNA_FIXTURE_DIR, name);
  FILE *in = fopen(path, "r");
  FILE *out = fopen(dest, "w");
  bool ok = in && out;
  size_t n;
  while (ok && (n = fread(buf, 1, sizeof(buf), in)) > 0)
    ok = fwrite(buf, 1, n, out) == n;
  if (in)
    fclose(in);
  if (out)
    ok = fclose(out) == 0 && ok;
  return ok;
}

/* Lay out <proc>/<pid>/fd/<fd> -> device and <proc>/<pid>/fdinfo/<fd> with the content of a fixture. */
static bool add_proc_fd(const char *proc, const char *pid, const char *fd, const char *device, const char *fixture) {
  char path[PATH_MAX];
  snprintf(path, sizeof(path), "%s/%s", proc, pid);
  mkdir(path, 0755);
  snprintf(path, sizeof(path), "%s/%s/fd", proc, pid);
  mkdir(path, 0755);
  snprintf(path, sizeof(path), "%s/%s/fdinfo", proc, pid);
  mkdir(path, 0755);
  snprintf(path, sizeof(path), "%s/%s/fd/%s", proc, pid, fd);
  if (symlink(device, path))
    return false;
  snprintf(path, sizeof(path), "%s/%s/fdinfo/%s", proc, pid, fd);
  return copy_fixture(fixture, path);
}

/* Scan a fake /proc where /dev/null stands for the accel node: the scan only compares device numbers. */
static bool scan_processes(void) {
  char proc[] = "/tmp/nvtop-amdxdna-proc-XXXXXX";
  struct stat st;
  CHECK(mkdtemp(proc) != NULL);
  CHECK(stat("/dev/null", &st) == 0);

  CHECK(add_proc_fd(proc, "100", "3", "/dev/null", "idle.fdinfo"));
  CHECK(add_proc_fd(proc, "100", "4", "/dev/null", "idle.fdinfo"));   /* dup of fd 3 */
  CHECK(add_proc_fd(proc, "100", "5", "/dev/zero", "active.fdinfo")); /* another device */
  CHECK(add_proc_fd(proc, "200", "7", "/dev/null", "active.fdinfo"));
  CHECK(add_proc_fd(proc, "300", "3", "/dev/null", "amdgpu.fdinfo")); /* another driver */
  char path[PATH_MAX];
  snprintf(path, sizeof(path), "%s/self", proc);
  CHECK(mkdir(path, 0755) == 0);

  struct gpu_info_amd_npu npu;
  memset(&npu, 0, sizeof(npu));
  npu.rdev = st.st_rdev;
  npu.have_rdev = true;
  amd_npu_proc_root = proc;
  gpuinfo_amd_npu_get_running_processes(&npu.base);
  amd_npu_proc_root = "/proc";

  CHECK(npu.base.processes_count == 2);
  /* readdir order is not specified */
  struct gpu_process *idle = npu.base.processes[0].pid == 100 ? &npu.base.processes[0] : &npu.base.processes[1];
  struct gpu_process *active = npu.base.processes[0].pid == 200 ? &npu.base.processes[0] : &npu.base.processes[1];
  CHECK(idle->pid == 100 && active->pid == 200);
  CHECK(idle->type == gpu_process_unknown);
  CHECK(idle->gpu_memory_usage == MiB(64));
  CHECK(active->type == gpu_process_compute);
  CHECK(active->gpu_memory_usage == MiB(76));

  free(npu.base.processes);
  nftw(proc, remove_entry, 16, FTW_DEPTH | FTW_PHYS);
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

  CHECK(add_accel_node(root, "accel0", "0000:c5:00.1", "amdxdna", "0x17f0\n", "0x20\n", "NPU Krackan 1\n", "261:0\n"));
  CHECK(add_accel_node(root, "accel1", "0000:00:0b.0", "intel_vpu", "0x7d1d\n", "0x04\n", NULL, "261:1\n"));
  CHECK(add_accel_node(root, "accel2", "0000:c6:00.1", "amdxdna", "0x17f2\n", "0x10\n", NULL, "261:2\n"));

  static char class_root[PATH_MAX];
  snprintf(class_root, sizeof(class_root), "%s/class", root);
  amd_npu_class_root = class_root;

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
    CHECK(npu->have_rdev);
    CHECK(GPUINFO_STATIC_FIELD_VALID(&npu->base.static_info, device_name));
    CHECK(GPUINFO_STATIC_FIELD_VALID(&npu->base.static_info, device_architecture));
    if (!strcmp(npu->base.pdev, "0000:c5:00.1")) {
      seen_krackan = true;
      CHECK(npu->rdev == makedev(261, 0));
      /* As read on a Krackan laptop: the firmware names the product, the PCI revision selects the NPU6 setup. */
      CHECK(!strcmp(npu->base.static_info.device_name, "AMD NPU (Krackan 1)"));
      CHECK(!strcmp(npu->base.static_info.device_architecture, "NPU6"));
    } else if (!strcmp(npu->base.pdev, "0000:c6:00.1")) {
      seen_npu3 = true;
      CHECK(npu->rdev == makedev(261, 2));
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
      {"process_accounting", process_accounting},
      {"scan_processes", scan_processes},
      {"device_discovery", device_discovery},
  };
  for (unsigned i = 0; argc == 2 && i < sizeof(tests) / sizeof(*tests); ++i)
    if (!strcmp(argv[1], tests[i].name))
      return tests[i].run() ? EXIT_SUCCESS : EXIT_FAILURE;
  fprintf(stderr, "Unknown test case\n");
  return EXIT_FAILURE;
}
