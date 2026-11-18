/* SPDX-License-Identifier: GPL-3.0-or-later */

#include "nvtop/extract_processinfo_fdinfo.h"

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <ftw.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <unistd.h>

// Exercise the real sweep against small /proc and /sys fixtures without GPU
// hardware. A regular fixture file containing a number N stands for the DRM
// character device 226:N; any other file is not a DRM device.
static char fixture_root[] = "/tmp/nvtop-fdinfo-XXXXXX";
static bool fail_proc_open, fail_proc_read;
static const char *failed_path;
static int injected_errno;
static DIR *fixture_dir;

static const char *fixture_path(const char *path, char *buffer, size_t size) {
  if (strcmp(path, "/proc") != 0 && strncmp(path, "/sys/", 5) != 0)
    return path;
  snprintf(buffer, size, "%s%s", fixture_root, path);
  return buffer;
}

static DIR *fixture_opendir(const char *path) {
  char buffer[512];
  if (strcmp(path, "/proc") != 0)
    return opendir(fixture_path(path, buffer, sizeof(buffer)));
  if (fail_proc_open) {
    errno = EACCES;
    return NULL;
  }
  fixture_dir = opendir(fixture_path(path, buffer, sizeof(buffer)));
  return fixture_dir;
}

static struct dirent *fixture_readdir(DIR *dir) {
  if (dir == fixture_dir && fail_proc_read) {
    errno = EIO;
    return NULL;
  }
  return readdir(dir);
}

static FILE *fixture_fopen(const char *path, const char *mode) {
  char buffer[512];
  return fopen(fixture_path(path, buffer, sizeof(buffer)), mode);
}

static int fixture_openat(int dir, const char *path, int flags) {
  if (failed_path && strcmp(path, failed_path) == 0) {
    errno = injected_errno;
    return -1;
  }
  return openat(dir, path, flags);
}

static int fixture_fstatat(int dir, const char *path, struct stat *stat, int flags) {
  int ret = fstatat(dir, path, stat, flags);
  if (ret != 0 || (stat->st_mode & S_IFMT) != S_IFREG)
    return ret;
  int fd = openat(dir, path, O_RDONLY);
  if (fd < 0)
    return ret;
  char contents[16] = {0};
  ssize_t length = read(fd, contents, sizeof(contents) - 1);
  close(fd);
  char *end;
  unsigned long node_minor = strtoul(contents, &end, 10);
  if (length > 0 && end != contents) {
    stat->st_mode = S_IFCHR;
    stat->st_rdev = makedev(226, node_minor);
  }
  return ret;
}

#define opendir fixture_opendir
#define readdir fixture_readdir
#define fopen fixture_fopen
#define openat fixture_openat
#define fstatat fixture_fstatat
#include "../src/extract_processinfo_fdinfo.c"
#undef opendir
#undef readdir
#undef fopen
#undef openat
#undef fstatat

#define CHECK(condition)                                                                                                \
  do {                                                                                                                 \
    if (!(condition)) {                                                                                                \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #condition);                                                    \
      return EXIT_FAILURE;                                                                                             \
    }                                                                                                                  \
  } while (0)

static bool parse_fixture(struct gpu_info *device, FILE *fp, struct gpu_process *process) {
  char name[32];
  unsigned long long memory;
  if (fscanf(fp, "%31s %llu", name, &memory) != 2 || strcmp(name, device->pdev) != 0)
    return false;
  if (memory != ULLONG_MAX)
    SET_GPUINFO_PROCESS(process, gpu_memory_usage, memory);
  return true;
}

static bool make_directory(const char *suffix) {
  char path[256];
  snprintf(path, sizeof(path), "%s/%s", fixture_root, suffix);
  return mkdir(path, 0700) == 0;
}

static bool write_fixture(const char *suffix, const char *contents) {
  char path[256];
  snprintf(path, sizeof(path), "%s/%s", fixture_root, suffix);
  FILE *fp = fopen(path, "w");
  if (!fp)
    return false;
  bool success = fputs(contents, fp) >= 0;
  return fclose(fp) == 0 && success;
}

static int remove_entry(const char *path, const struct stat *stat, int type, struct FTW *ftw) {
  (void)stat;
  (void)type;
  (void)ftw;
  return remove(path);
}

#define DEVICE_COUNT 4
enum { INTEL, MALI, MSM, LEGACY };
static struct gpu_info devices[DEVICE_COUNT];

static void sweep(void) {
  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    devices[i].processes_count = 0;
  processinfo_sweep_fdinfos();
}

int main(void) {
  CHECK(mkdtemp(fixture_root));
  const char *directories[] = {"proc",
                               "proc/123",
                               "proc/123/fd",
                               "proc/123/fdinfo",
                               "dev",
                               "sys",
                               "sys/dev",
                               "sys/dev/char",
                               "sys/dev/char/226:0",
                               "sys/dev/char/226:0/device",
                               "sys/dev/char/226:0/device/drm",
                               "sys/dev/char/226:0/device/drm/card0",
                               "sys/dev/char/226:0/device/drm/renderD128",
                               "sys/dev/char/226:0/device/drm/controlD64"};
  for (unsigned i = 0; i < sizeof(directories) / sizeof(*directories); ++i)
    CHECK(make_directory(directories[i]));
  CHECK(write_fixture("sys/dev/char/226:0/device/drm/card0/dev", "226:0\n"));
  CHECK(write_fixture("sys/dev/char/226:0/device/drm/renderD128/dev", "226:128\n"));
  CHECK(write_fixture("sys/dev/char/226:0/device/drm/controlD64/dev", "226:64\n"));
  CHECK(write_fixture("dev/card0", "0"));
  CHECK(write_fixture("dev/renderD129", "129"));
  CHECK(write_fixture("dev/card2", "2"));

  // The legacy device registers without a usable node.
  const char *names[DEVICE_COUNT] = {"Intel", "Mali", "MSM", "Legacy"};
  const char *nodes[DEVICE_COUNT] = {"dev/card0", "dev/renderD129", "dev/card2", NULL};
  for (unsigned i = 0; i < DEVICE_COUNT; ++i) {
    snprintf(devices[i].pdev, sizeof(devices[i].pdev), "%s", names[i]);
    char node_path[256];
    if (nodes[i])
      snprintf(node_path, sizeof(node_path), "%s/%s", fixture_root, nodes[i]);
    processinfo_register_fdinfo_callback(parse_fixture, &devices[i], nodes[i] ? node_path : NULL);
  }
  // Sibling nodes come from sysfs; control nodes are not DRM client nodes.
  CHECK(callback_entries[INTEL].drm_nodes_count == 2);
  CHECK(callback_entries[INTEL].drm_nodes[0] == makedev(226, 0));
  CHECK(callback_entries[INTEL].drm_nodes[1] == makedev(226, 128));
  CHECK(callback_entries[MALI].drm_nodes_count == 1);
  CHECK(callback_entries[LEGACY].drm_nodes_count == 0);

  // An empty scan reports no processes.
  sweep();
  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    CHECK(devices[i].processes_count == 0);

  // A fd to the render node reaches the device registered with its card node.
  CHECK(write_fixture("proc/123/fd/3", "128"));
  CHECK(write_fixture("proc/123/fdinfo/3", "Intel 64\n"));
  sweep();
  CHECK(devices[INTEL].processes_count == 1);
  CHECK(devices[INTEL].processes[0].gpu_memory_usage == 64);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&devices[INTEL].processes[0], gpu_memory_usage));
  CHECK(devices[MALI].processes_count == 0 && devices[LEGACY].processes_count == 0);

  // Only the owner parses its fds, even when another device would accept them.
  CHECK(write_fixture("proc/123/fdinfo/3", "Mali 64\n"));
  sweep();
  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    CHECK(devices[i].processes_count == 0);

  // A fd of an unmonitored device is not parsed.
  CHECK(write_fixture("proc/123/fdinfo/3", "Intel 64\n"));
  processinfo_enable_disable_callback_for(&devices[INTEL], false);
  sweep();
  CHECK(devices[INTEL].processes_count == 0);
  processinfo_enable_disable_callback_for(&devices[INTEL], true);

  // A fd owned by no registered device (e.g., a driver without nvtop support)
  // is offered to the devices without known nodes only.
  CHECK(write_fixture("proc/123/fd/3", "200"));
  CHECK(write_fixture("proc/123/fdinfo/3", "Legacy 32\n"));
  sweep();
  CHECK(devices[LEGACY].processes_count == 1);
  CHECK(devices[LEGACY].processes[0].gpu_memory_usage == 32);
  processinfo_drop_callback(&devices[LEGACY]);
  sweep();
  CHECK(devices[LEGACY].processes_count == 0);

  // Descriptors that are not DRM devices are ignored without reading their fdinfo.
  CHECK(write_fixture("proc/123/fd/3", "not a device"));
  failed_path = "fdinfo/3";
  injected_errno = EIO;
  sweep();
  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    CHECK(devices[i].processes_count == 0);
  failed_path = NULL;

  // Dropped callbacks must no longer receive writes during future sweeps.
  CHECK(write_fixture("proc/123/fd/3", "128"));
  CHECK(write_fixture("proc/123/fdinfo/3", "Intel 64\n"));
  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    processinfo_drop_callback(&devices[i]);
  sweep();
  CHECK(devices[INTEL].processes_count == 0);
  CHECK(registered_callback_entries == 0);

  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    free(devices[i].processes);
  CHECK(nftw(fixture_root, remove_entry, 16, FTW_DEPTH | FTW_PHYS) == 0);
  return EXIT_SUCCESS;
}
