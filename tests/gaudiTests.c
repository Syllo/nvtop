/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <dlfcn.h>
#include <fcntl.h>
#include <ftw.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

/* Exercise the backend with deterministic HLML responses, without a driver. */
#include "../src/extract_gpuinfo_gaudi.c"
void register_gpu_vendor(struct gpu_vendor *vendor) { (void)vendor; }
unsigned nvtop_pcie_gen_from_link_speed(unsigned linkSpeed) {
  (void)linkSpeed;
  return 0;
}

#define CHECK(condition)                                                                                               \
  do {                                                                                                                 \
    if (!(condition)) {                                                                                                \
      fprintf(stderr, "Failed at line %d: %s\n", __LINE__, #condition);                                                \
      return 1;                                                                                                        \
    }                                                                                                                  \
  } while (0)

static hlml_return_t fake_device_count(unsigned *count) {
  *count = 1;
  return HLML_SUCCESS;
}

static hlml_return_t fake_device_handle(unsigned index, hlml_device_t *device) {
  (void)index;
  *device = (void *)0x1;
  return HLML_SUCCESS;
}

/* Emulate a newer libhlml whose structure grew by this many bytes. */
#define HLML_TEST_ABI_GROWTH 256

static hlml_return_t growing_utilization(hlml_device_t device, hlml_utilization_t *utilization) {
  (void)device;
  memset(utilization, 0xA5, offsetof(hlml_utilization_t, reserved) + HLML_TEST_ABI_GROWTH);
  utilization->aip = 42;
  utilization->memory = 25;
  return HLML_SUCCESS;
}

static hlml_return_t growing_memory_info(hlml_device_t device, hlml_memory_t *memory) {
  (void)device;
  memset(memory, 0xA5, offsetof(hlml_memory_t, reserved) + HLML_TEST_ABI_GROWTH);
  memory->free = 3072;
  memory->total = 4096;
  memory->used = 1024;
  return HLML_SUCCESS;
}

static hlml_return_t fake_minor_number(hlml_device_t device, unsigned *minor) {
  (void)device;
  (void)minor;
  return HLML_ERROR_NOT_SUPPORTED;
}

static unsigned shutdown_calls;
static hlml_return_t fake_shutdown(void) {
  ++shutdown_calls;
  return HLML_SUCCESS;
}

/* The backend calls dlclose at shutdown; use a real handle for the main program. */
static void install_fake_hlml(void) {
  libhlml_handle = dlopen(NULL, RTLD_LAZY);
  hlml_shutdown = fake_shutdown;
  hlml_device_get_count = fake_device_count;
  hlml_device_get_handle_by_index = fake_device_handle;
  hlml_device_get_minor_number = fake_minor_number;
}

static int test_double_enumeration(void) {
  LIST_HEAD(first);
  LIST_HEAD(second);
  unsigned count = 0;

  shutdown_calls = 0;
  install_fake_hlml();
  CHECK(libhlml_handle != NULL);

  CHECK(gpuinfo_gaudi_get_device_handles(&first, &count));
  CHECK(count == 1);
  CHECK(gaudi_device_count == 1);
  CHECK(!strcmp(gaudi_devices[0].base.pdev, "gaudi-0"));
  CHECK(!list_empty(&first));

  /* Repeated enumeration must be rejected instead of indexing past the new array. */
  CHECK(!gpuinfo_gaudi_get_device_handles(&second, &count));
  CHECK(count == 0);
  CHECK(list_empty(&second));
  CHECK(strstr(gpuinfo_gaudi_last_error_string(), "already enumerated") != NULL);

  list_del(&gaudi_devices[0].base.list);
  gpuinfo_gaudi_shutdown();
  CHECK(shutdown_calls == 1);
  CHECK(gaudi_devices == NULL);
  CHECK(gaudi_device_count == 0);

  /* After a shutdown, a new enumeration must be possible. */
  install_fake_hlml();
  CHECK(gpuinfo_gaudi_get_device_handles(&first, &count));
  CHECK(count == 1);
  list_del(&gaudi_devices[0].base.list);
  gpuinfo_gaudi_shutdown();
  CHECK(shutdown_calls == 2);
  return 0;
}

static int test_abi_growth_tolerance(void) {
  LIST_HEAD(devices);
  unsigned count = 0;
  struct gpuinfo_dynamic_info dynamic;

  shutdown_calls = 0;
  install_fake_hlml();
  hlml_device_get_utilization_rates = growing_utilization;
  hlml_device_get_memory_info = growing_memory_info;

  CHECK(gpuinfo_gaudi_get_device_handles(&devices, &count));
  CHECK(count == 1);

  gaudi_sample_device(&gaudi_devices[0], &dynamic);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&dynamic, gpu_util_rate));
  CHECK(dynamic.gpu_util_rate == 42);
  CHECK(GPUINFO_DYNAMIC_FIELD_VALID(&dynamic, total_memory));
  CHECK(dynamic.total_memory == 4096);
  CHECK(dynamic.used_memory == 1024);
  CHECK(dynamic.free_memory == 3072);
  CHECK(dynamic.mem_util_rate == 25);

  list_del(&gaudi_devices[0].base.list);
  gpuinfo_gaudi_shutdown();
  CHECK(shutdown_calls == 1);
  return 0;
}

static int test_process_memory_without_shm(void) {
  LIST_HEAD(devices);
  unsigned count = 0;

  shutdown_calls = 0;
  install_fake_hlml();
  CHECK(gpuinfo_gaudi_get_device_handles(&devices, &count));
  CHECK(count == 1);

  /* One process on the device, but the framework did not publish an shm value. */
  scan_generation = 5;
  gaudi_devices[0].scan_generation_used = 4;
  SET_GPUINFO_DYNAMIC(&gaudi_devices[0].base.dynamic_info, used_memory, 4096);
  CHECK(record_scan_entry(1234, 0, false, 0));
  gpuinfo_gaudi_get_running_processes(&gaudi_devices[0].base);
  CHECK(gaudi_devices[0].base.processes_count == 1);
  CHECK(gaudi_devices[0].base.processes[0].pid == 1234);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&gaudi_devices[0].base.processes[0], gpu_memory_usage));

  /* A value published in the shm file is still reported. */
  scan_generation = 6;
  scan_entries_count = 0;
  CHECK(record_scan_entry(5678, 0, true, 2048));
  gpuinfo_gaudi_get_running_processes(&gaudi_devices[0].base);
  CHECK(gaudi_devices[0].base.processes_count == 1);
  CHECK(gaudi_devices[0].base.processes[0].pid == 5678);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&gaudi_devices[0].base.processes[0], gpu_memory_usage));
  CHECK(gaudi_devices[0].base.processes[0].gpu_memory_usage == 2048);

  free(gaudi_devices[0].base.processes);
  gaudi_devices[0].base.processes = NULL;
  list_del(&gaudi_devices[0].base.list);
  gpuinfo_gaudi_shutdown();
  CHECK(shutdown_calls == 1);
  return 0;
}

static void prepare_scan(struct gpu_info_gaudi *gpu) {
  static unsigned generation;
  ++generation;
  scan_generation = generation;
  gpu->scan_generation_used = generation - 1;
  scan_entries_count = 0;
}

static int test_process_utilization_attribution(void) {
  LIST_HEAD(devices);
  unsigned count = 0;

  shutdown_calls = 0;
  install_fake_hlml();
  CHECK(gpuinfo_gaudi_get_device_handles(&devices, &count));
  CHECK(count == 1);
  SET_GPUINFO_DYNAMIC(&gaudi_devices[0].base.dynamic_info, gpu_util_rate, 42);

  /* One holder: the device utilization belongs to that process. */
  prepare_scan(&gaudi_devices[0]);
  CHECK(record_scan_entry(100, 0, false, 0));
  gpuinfo_gaudi_get_running_processes(&gaudi_devices[0].base);
  CHECK(gaudi_devices[0].base.processes_count == 1);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&gaudi_devices[0].base.processes[0], gpu_usage));
  CHECK(gaudi_devices[0].base.processes[0].gpu_usage == 42);

  /* Two holders, one shm owner: the shm owner runs the compute context. */
  prepare_scan(&gaudi_devices[0]);
  CHECK(record_scan_entry(200, 0, false, 0));
  CHECK(record_scan_entry(201, 0, true, 2048));
  gpuinfo_gaudi_get_running_processes(&gaudi_devices[0].base);
  CHECK(gaudi_devices[0].base.processes_count == 2);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&gaudi_devices[0].base.processes[0], gpu_usage));
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&gaudi_devices[0].base.processes[1], gpu_usage));
  CHECK(gaudi_devices[0].base.processes[1].gpu_usage == 42);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&gaudi_devices[0].base.processes[1], gpu_memory_usage));
  CHECK(gaudi_devices[0].base.processes[1].gpu_memory_usage == 2048);

  /* Two holders, no shm owner: the device load cannot be attributed. */
  prepare_scan(&gaudi_devices[0]);
  CHECK(record_scan_entry(300, 0, false, 0));
  CHECK(record_scan_entry(301, 0, false, 0));
  gpuinfo_gaudi_get_running_processes(&gaudi_devices[0].base);
  CHECK(gaudi_devices[0].base.processes_count == 2);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&gaudi_devices[0].base.processes[0], gpu_usage));
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&gaudi_devices[0].base.processes[1], gpu_usage));

  /* Two holders publishing the same inherited shm: still ambiguous. */
  prepare_scan(&gaudi_devices[0]);
  CHECK(record_scan_entry(400, 0, true, 2048));
  CHECK(record_scan_entry(401, 0, true, 2048));
  gpuinfo_gaudi_get_running_processes(&gaudi_devices[0].base);
  CHECK(gaudi_devices[0].base.processes_count == 2);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&gaudi_devices[0].base.processes[0], gpu_usage));
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&gaudi_devices[0].base.processes[1], gpu_usage));

  free(gaudi_devices[0].base.processes);
  gaudi_devices[0].base.processes = NULL;
  list_del(&gaudi_devices[0].base.list);
  gpuinfo_gaudi_shutdown();
  CHECK(shutdown_calls == 1);
  return 0;
}

static int test_pci_chip_address(void) {
  unsigned address = 0;
  CHECK(pci_chip_address("0000:19:00.0", &address));
  CHECK(address == 0x1900);
  CHECK(pci_chip_address("0001:19:00.1", &address));
  CHECK(address == 0x11901);
  CHECK(!pci_chip_address("0000:19:00", &address));
  CHECK(!pci_chip_address("not-a-bdf", &address));
  CHECK(!pci_chip_address("0000:100:00.0", &address));
  CHECK(!pci_chip_address("0000:19:20.0", &address));
  CHECK(!pci_chip_address("0000:19:00.8", &address));
  CHECK(!pci_chip_address("10000:19:00.0", &address));
  CHECK(!pci_chip_address(NULL, &address));
  return 0;
}

static int test_device_naming(void) {
  char architecture[64];
  char family[64];
  char name[64];

  normalize_device_type("GAUDI2", architecture, sizeof(architecture), family, sizeof(family));
  CHECK(!strcmp(architecture, "Gaudi2"));
  CHECK(!strcmp(family, "Gaudi2"));
  normalize_device_type("GAUDI2B", architecture, sizeof(architecture), family, sizeof(family));
  CHECK(!strcmp(architecture, "Gaudi2B"));
  CHECK(!strcmp(family, "Gaudi2B"));
  normalize_device_type("GAUDI3 HL-338", architecture, sizeof(architecture), family, sizeof(family));
  CHECK(!strcmp(architecture, "Gaudi3"));
  CHECK(!strcmp(family, "Gaudi3 HL-338"));
  normalize_device_type("GAUDI HL2000M", architecture, sizeof(architecture), family, sizeof(family));
  CHECK(!strcmp(architecture, "Gaudi"));
  CHECK(!strcmp(family, "Gaudi HL2000M"));
  normalize_device_type("GOYA", architecture, sizeof(architecture), family, sizeof(family));
  CHECK(!strcmp(architecture, "Goya"));
  CHECK(!strcmp(family, "Goya"));
  normalize_device_type("FUTURE1B", architecture, sizeof(architecture), family, sizeof(family));
  CHECK(!strcmp(architecture, "Future1b"));
  CHECK(!strcmp(family, "Future1b"));
  /* An empty device_type must not make the lower-casing walk past the string. */
  normalize_device_type("", architecture, sizeof(architecture), family, sizeof(family));
  CHECK(!strcmp(architecture, ""));
  CHECK(!strcmp(family, ""));

  CHECK(ends_with_ignore_case("Gaudi3 HL-338", "HL-338"));
  CHECK(ends_with_ignore_case("Gaudi3 HL-338", "hl-338"));
  CHECK(ends_with_ignore_case("Gaudi HL2000M", "HL-2000M"));
  CHECK(!ends_with_ignore_case("Gaudi2", "HL-225"));
  CHECK(!ends_with_ignore_case("Gaudi3 HL-338", "HL-3380"));

  compose_device_name("Gaudi2", "HL-225", name, sizeof(name));
  CHECK(!strcmp(name, "Gaudi2 HL-225"));
  compose_device_name("Gaudi3 HL-338", "HL-338", name, sizeof(name));
  CHECK(!strcmp(name, "Gaudi3 HL-338"));
  compose_device_name("Gaudi HL2000M", "HL-2000M", name, sizeof(name));
  CHECK(!strcmp(name, "Gaudi HL2000M"));
  compose_device_name("Gaudi2", "", name, sizeof(name));
  CHECK(!strcmp(name, "Gaudi2"));
  compose_device_name("", "HL-225", name, sizeof(name));
  CHECK(!strcmp(name, "HL-225"));
  compose_device_name("", "", name, sizeof(name));
  CHECK(!strcmp(name, ""));

  CHECK(!strcmp(device_memory_type("Goya"), "DDR4"));
  CHECK(!strcmp(device_memory_type("Gaudi2"), "HBM"));
  return 0;
}

static int test_aip_channel_count(void) {
  CHECK(expected_aip_channels("GAUDI2") == 8);
  CHECK(expected_aip_channels("GAUDI2B") == 8);
  CHECK(expected_aip_channels("GAUDI2 Simulator") == 8);
  CHECK(expected_aip_channels("GAUDI3") == 16);
  CHECK(expected_aip_channels("GAUDI3 FPGA") == 16);
  CHECK(expected_aip_channels("GAUDI3 HL-338") == 12);
  CHECK(expected_aip_channels("GAUDI3 HL-338 Simulator") == 12);
  CHECK(expected_aip_channels("GAUDI") == 0);
  CHECK(expected_aip_channels("GOYA") == 0);
  CHECK(expected_aip_channels("") == 0);

  CHECK(aip_channel_count_matches("GAUDI2", 8));
  CHECK(!aip_channel_count_matches("GAUDI2", 7));
  CHECK(aip_channel_count_matches("GAUDI3", 16));
  CHECK(!aip_channel_count_matches("GAUDI3", 12));
  CHECK(aip_channel_count_matches("GAUDI3 HL-338", 12));
  CHECK(!aip_channel_count_matches("GAUDI3 HL-338", 16));
  CHECK(aip_channel_count_matches("GOYA", 3));
  CHECK(aip_channel_count_matches("", 0));
  return 0;
}

static int test_temperature_config(void) {
  char path[] = "gaudi-sensors-XXXXXX";
  int fd = mkstemp(path);
  CHECK(fd >= 0);
  const char *config = "chip \"TEST-pci-*\"\n"
                       "    label temp1 \"On Chip 0\"\n"
                       "    label temp2 \"On Chip 1\"\n"
                       "    label temp3 \"HBM 0\"\n"
                       "    ignore temp2\n"
                       "    set temp1_max 90\n"
                       "chip \"OTHER-pci-*\"\n"
                       "    label temp4 \"On Chip 9\"\n";
  ssize_t written = write(fd, config, strlen(config));
  close(fd);
  CHECK(written == (ssize_t)strlen(config));

  bool on_chip[GAUDI_MAX_HWMON_CHANNELS] = {false};
  bool ignored[GAUDI_MAX_HWMON_CHANNELS] = {false};
  parse_temperature_config(path, "TEST-pci-1900", on_chip, ignored);
  unlink(path);

  CHECK(on_chip[1]);
  CHECK(on_chip[2]);
  CHECK(!on_chip[3]);
  CHECK(!on_chip[4]);
  CHECK(ignored[2]);
  CHECK(!ignored[1]);

  unsigned sensors[GAUDI_MAX_CHIP_SENSORS];
  CHECK(select_chip_sensors(on_chip, ignored, sensors, GAUDI_MAX_CHIP_SENSORS) == 1);
  CHECK(sensors[0] == 1);
  CHECK(!aip_channel_count_matches("GAUDI2", 1));
  return 0;
}

static int test_path_index(void) {
  CHECK(path_index("/dev/accel/accel12", "accel") == 12);
  CHECK(path_index("/dev/accel/accel3 (deleted)", "accel") == 3);
  CHECK(path_index("/dev/accel/accel_controlD0", "accel") == -1);
  CHECK(path_index("/dev/accel/accel", "accel") == -1);
  CHECK(path_index("/dev/accel/accel12x", "accel") == -1);
  CHECK(path_index("/dev/shm/mem_usage_accel5", "mem_usage_accel") == 5);
  CHECK(path_index("/dev/shm/mem_usage_accel5 (deleted)", "mem_usage_accel") == 5);
  CHECK(path_index("accel0", "accel") == 0);
  CHECK(path_index("/tmp/not-accel0", "accel") == -1);
  return 0;
}

static int remove_tree_entry(const char *path, const struct stat *statbuf, int type, struct FTW *ftw) {
  (void)statbuf;
  (void)type;
  (void)ftw;
  return remove(path);
}

static const struct gaudi_process_entry *find_scan_entry(pid_t pid) {
  for (unsigned i = 0; i < scan_entries_count; ++i) {
    if (scan_entries[i].pid == pid)
      return &scan_entries[i];
  }
  return NULL;
}

static int write_shm_file(const char *path, uint64_t used) {
  int fd = open(path, O_CREAT | O_WRONLY | O_TRUNC, 0600);
  if (fd < 0)
    return -1;
  struct hlml_shm_data data = {.version = 1, .timestamp = 0, .used_mem_in_bytes = used};
  ssize_t written = write(fd, &data, sizeof(data));
  close(fd);
  return written == (ssize_t)sizeof(data) ? 0 : -1;
}

static int test_fake_proc_scan(void) {
  char root[] = "gaudi-proc-XXXXXX";
  char root_abs[PATH_MAX];
  char path[PATH_MAX];
  char target[PATH_MAX];
  const char *pids[] = {"123", "456", "457", "458", "460", "789"};
  struct stat null_stat;

  CHECK(mkdtemp(root) != NULL);
  CHECK(realpath(root, root_abs) != NULL);

  snprintf(path, sizeof(path), "%s/dev", root_abs);
  CHECK(mkdir(path, 0700) == 0);
  snprintf(path, sizeof(path), "%s/dev/accel", root_abs);
  CHECK(mkdir(path, 0700) == 0);
  snprintf(path, sizeof(path), "%s/dev/shm", root_abs);
  CHECK(mkdir(path, 0700) == 0);

  for (size_t i = 0; i < sizeof(pids) / sizeof(pids[0]); ++i) {
    snprintf(path, sizeof(path), "%s/%s", root_abs, pids[i]);
    CHECK(mkdir(path, 0700) == 0);
    snprintf(path, sizeof(path), "%s/%s/fd", root_abs, pids[i]);
    CHECK(mkdir(path, 0700) == 0);
  }

  snprintf(path, sizeof(path), "%s/dev/accel/accel0", root_abs);
  CHECK(symlink("/dev/null", path) == 0);
  snprintf(path, sizeof(path), "%s/dev/accel/accel1", root_abs);
  CHECK(symlink("/dev/zero", path) == 0);
  snprintf(path, sizeof(path), "%s/dev/accel/accel5", root_abs);
  CHECK(symlink("/dev/null", path) == 0);

  snprintf(path, sizeof(path), "%s/dev/shm/mem_usage_accel0", root_abs);
  CHECK(write_shm_file(path, 12345) == 0);
  snprintf(path, sizeof(path), "%s/dev/shm/mem_usage_accel5", root_abs);
  CHECK(write_shm_file(path, 54321) == 0);
  snprintf(path, sizeof(path), "%s/dev/shm/mem_usage_accel6", root_abs);
  CHECK(write_shm_file(path, 60000) == 0);

  /* pid 123: accel0 and shm0, matched by index. */
  snprintf(target, sizeof(target), "%s/dev/accel/accel0", root_abs);
  snprintf(path, sizeof(path), "%s/123/fd/0", root_abs);
  CHECK(symlink(target, path) == 0);
  snprintf(target, sizeof(target), "%s/dev/shm/mem_usage_accel0", root_abs);
  snprintf(path, sizeof(path), "%s/123/fd/1", root_abs);
  CHECK(symlink(target, path) == 0);

  /* pid 456: accel0 and shm5; one device and one shm use the fallback. */
  snprintf(target, sizeof(target), "%s/dev/accel/accel0", root_abs);
  snprintf(path, sizeof(path), "%s/456/fd/0", root_abs);
  CHECK(symlink(target, path) == 0);
  snprintf(target, sizeof(target), "%s/dev/shm/mem_usage_accel5", root_abs);
  snprintf(path, sizeof(path), "%s/456/fd/1", root_abs);
  CHECK(symlink(target, path) == 0);

  /* pid 457: different device node. */
  snprintf(target, sizeof(target), "%s/dev/accel/accel1", root_abs);
  snprintf(path, sizeof(path), "%s/457/fd/0", root_abs);
  CHECK(symlink(target, path) == 0);

  /* pid 458: accel0 without shm. */
  snprintf(target, sizeof(target), "%s/dev/accel/accel0", root_abs);
  snprintf(path, sizeof(path), "%s/458/fd/0", root_abs);
  CHECK(symlink(target, path) == 0);

  /* pid 460: accel0 with two unrelated shm files; no match and no fallback. */
  snprintf(target, sizeof(target), "%s/dev/accel/accel0", root_abs);
  snprintf(path, sizeof(path), "%s/460/fd/0", root_abs);
  CHECK(symlink(target, path) == 0);
  snprintf(target, sizeof(target), "%s/dev/shm/mem_usage_accel5", root_abs);
  snprintf(path, sizeof(path), "%s/460/fd/1", root_abs);
  CHECK(symlink(target, path) == 0);
  snprintf(target, sizeof(target), "%s/dev/shm/mem_usage_accel6", root_abs);
  snprintf(path, sizeof(path), "%s/460/fd/2", root_abs);
  CHECK(symlink(target, path) == 0);

  /* pid 789: remapped accel5 plus shm0 and shm6, matched through the host minor. */
  snprintf(target, sizeof(target), "%s/dev/accel/accel5", root_abs);
  snprintf(path, sizeof(path), "%s/789/fd/0", root_abs);
  CHECK(symlink(target, path) == 0);
  snprintf(target, sizeof(target), "%s/dev/shm/mem_usage_accel0", root_abs);
  snprintf(path, sizeof(path), "%s/789/fd/1", root_abs);
  CHECK(symlink(target, path) == 0);
  snprintf(target, sizeof(target), "%s/dev/shm/mem_usage_accel6", root_abs);
  snprintf(path, sizeof(path), "%s/789/fd/2", root_abs);
  CHECK(symlink(target, path) == 0);

  CHECK(stat("/dev/null", &null_stat) == 0);
  free(gaudi_devices);
  gaudi_devices = calloc(1, sizeof(*gaudi_devices));
  CHECK(gaudi_devices != NULL);
  gaudi_device_count = 1;
  gaudi_devices[0].rdev = null_stat.st_rdev;
  gaudi_devices[0].rdev_valid = true;
  gaudi_devices[0].minor = 0;
  gaudi_devices[0].minor_valid = true;

  free(scan_entries);
  scan_entries = NULL;
  scan_entries_count = 0;
  scan_entries_size = 0;
  scan_generation = 0;
  gaudi_proc_root = root_abs;

  gaudi_scan_processes();

  CHECK(scan_entries_count == 5);
  const struct gaudi_process_entry *entry = find_scan_entry(123);
  CHECK(entry != NULL && entry->device_index == 0 && entry->memory_valid);
  CHECK(entry->memory == 12345);

  entry = find_scan_entry(456);
  CHECK(entry != NULL && entry->memory_valid);
  CHECK(entry->memory == 54321);
  CHECK(find_scan_entry(457) == NULL);
  entry = find_scan_entry(458);
  CHECK(entry != NULL && !entry->memory_valid);
  entry = find_scan_entry(460);
  CHECK(entry != NULL && !entry->memory_valid);
  entry = find_scan_entry(789);
  CHECK(entry != NULL && entry->memory_valid);
  CHECK(entry->memory == 12345);

  gaudi_proc_root = "/proc";
  free(gaudi_devices);
  gaudi_devices = NULL;
  gaudi_device_count = 0;
  free(scan_entries);
  scan_entries = NULL;
  scan_entries_count = 0;
  scan_entries_size = 0;
  scan_generation = 0;
  CHECK(nftw(root_abs, remove_tree_entry, 64, FTW_DEPTH | FTW_PHYS) == 0);
  return 0;
}

static int test_sampler_request(void) {
  free(gaudi_devices);
  gaudi_devices = calloc(2, sizeof(*gaudi_devices));
  CHECK(gaudi_devices != NULL);
  gaudi_device_count = 2;

  gaudi_devices[0].refresh_requested = true;
  CHECK(gaudi_take_sample_request(&gaudi_devices[0]));
  CHECK(!gaudi_devices[0].refresh_requested);
  CHECK(!gaudi_take_sample_request(&gaudi_devices[0]));
  CHECK(!gaudi_any_sample_request());

  gaudi_devices[1].refresh_requested = true;
  CHECK(gaudi_any_sample_request());
  CHECK(gaudi_take_sample_request(&gaudi_devices[1]));
  CHECK(!gaudi_any_sample_request());

  gaudi_devices[0].refresh_requested = true;
  gaudi_devices[1].refresh_requested = true;
  CHECK(gaudi_any_sample_request());
  CHECK(gaudi_take_sample_request(&gaudi_devices[0]));
  CHECK(gaudi_any_sample_request());
  CHECK(gaudi_take_sample_request(&gaudi_devices[1]));
  CHECK(!gaudi_any_sample_request());

  free(gaudi_devices);
  gaudi_devices = NULL;
  gaudi_device_count = 0;
  return 0;
}

int main(void) {
  CHECK(test_double_enumeration() == 0);
  CHECK(test_abi_growth_tolerance() == 0);
  CHECK(test_process_memory_without_shm() == 0);
  CHECK(test_process_utilization_attribution() == 0);
  CHECK(test_pci_chip_address() == 0);
  CHECK(test_device_naming() == 0);
  CHECK(test_aip_channel_count() == 0);
  CHECK(test_temperature_config() == 0);
  CHECK(test_path_index() == 0);
  CHECK(test_fake_proc_scan() == 0);
  CHECK(test_sampler_request() == 0);
  puts("Gaudi backend tests passed");
  return 0;
}
