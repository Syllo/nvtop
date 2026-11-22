/* SPDX-License-Identifier: GPL-3.0-or-later */

#include "nvtop/extract_processinfo_fdinfo.h"

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <ftw.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <unistd.h>

// Exercise the real sweep against small /proc and /sys fixtures without GPU
// hardware. The fixture pids are above the kernel's maximum pid, so that kcmp
// never inspects a real process. A regular fixture file containing a number N stands for the DRM
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

static const char *failed_stat_path;

// Streams whose reads fail with injected_read_errno, as reading the fdinfo of
// a descriptor closed since it was opened does.
static int injected_read_errno;
static ssize_t failing_read(void *cookie, char *buf, size_t size) {
  (void)cookie;
  (void)buf;
  (void)size;
  errno = injected_read_errno;
  return -1;
}
static int failing_close(void *cookie) { return close((int)(intptr_t)cookie); }
static FILE *fixture_fdopen(int fd, const char *mode) {
  if (!injected_read_errno)
    return fdopen(fd, mode);
  return fopencookie((void *)(intptr_t)fd, mode,
                     (cookie_io_functions_t){.read = failing_read, .close = failing_close});
}
static int fixture_fstatat(int dir, const char *path, struct stat *stat, int flags) {
  if (failed_stat_path && strcmp(path, failed_stat_path) == 0) {
    errno = EIO;
    return -1;
  }
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
#define fdopen fixture_fdopen
#include "../src/extract_processinfo_fdinfo.c"
#undef opendir
#undef readdir
#undef fopen
#undef openat
#undef fstatat
#undef fdopen

#define CHECK(condition)                                                                                                \
  do {                                                                                                                 \
    if (!(condition)) {                                                                                                \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #condition);                                                    \
      return EXIT_FAILURE;                                                                                             \
    }                                                                                                                  \
  } while (0)

// Fixture fdinfo files hold an optional "drm-client-id:" line followed by a
// "<device name> <memory> [<shared memory>]" line.
static bool write_fixture(const char *suffix, const char *contents);

// A fdinfo reading "reused" (or "reused-drm") stands for a descriptor whose
// number was reused for a non-DRM file (or another device's DRM node) between
// the sweep's stat and the fdinfo read.
static bool parse_fixture(struct gpu_info *device, FILE *fp, struct gpu_process *process) {
  char line[64], name[32];
  unsigned long long memory, shared;
  do {
    if (!fgets(line, sizeof(line), fp))
      return false;
  } while (!strncmp(line, "drm-client-id:", 14));
  if (!strcmp(line, "reused\n") || !strcmp(line, "reused-drm\n")) {
    write_fixture("proc/4194400/fd/3", line[6] == '-' ? "129" : "not a device");
    return false;
  }
  int fields = sscanf(line, "%31s %llu %llu", name, &memory, &shared);
  if (fields < 2 || strcmp(name, device->pdev) != 0)
    return false;
  if (memory != ULLONG_MAX)
    SET_GPUINFO_PROCESS(process, gpu_memory_usage, memory);
  if (fields == 3)
    SET_GPUINFO_PROCESS(process, gpu_memory_shared, shared);
  // Unrelated errno changes must not turn a later directory EOF into a failure.
  errno = EIO;
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

static bool remove_fixture(const char *suffix) {
  char path[256];
  snprintf(path, sizeof(path), "%s/%s", fixture_root, suffix);
  return remove(path) == 0;
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

static bool only_failed(int failed) {
  for (int i = 0; i < DEVICE_COUNT; ++i) {
    if (devices[i].processes_query_succeeded == (i == failed))
      return false;
  }
  return true;
}

static bool all_succeeded(void) { return only_failed(-1); }

int main(void) {
  CHECK(mkdtemp(fixture_root));
  const char *directories[] = {"proc",
                               "proc/4194400",
                               "proc/4194400/fd",
                               "proc/4194400/fdinfo",
                               "proc/4194401",
                               "proc/4194401/fd",
                               "proc/4194401/fdinfo",
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

  // All fdinfo backends must distinguish a successful empty scan from failure.
  sweep();
  CHECK(all_succeeded());
  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    CHECK(devices[i].processes_count == 0);
  fail_proc_open = true;
  sweep();
  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    CHECK(!devices[i].processes_query_succeeded);
  fail_proc_open = false;
  fail_proc_read = true;
  sweep();
  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    CHECK(!devices[i].processes_query_succeeded);
  fail_proc_read = false;

  // Processes of other users are not visible to unprivileged users; the scan of
  // the visible processes remains complete. Unattributable errors fail it.
  failed_path = "fd";
  injected_errno = EACCES;
  sweep();
  CHECK(all_succeeded());
  injected_errno = EPERM;
  sweep();
  CHECK(all_succeeded());
  injected_errno = EIO;
  sweep();
  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    CHECK(!devices[i].processes_query_succeeded);
  injected_errno = ENOENT;
  sweep();
  CHECK(all_succeeded());
  failed_path = NULL;

  // A fd to the render node reaches the device registered with its card node.
  CHECK(write_fixture("proc/4194400/fd/3", "128"));
  CHECK(write_fixture("proc/4194400/fdinfo/3", "Intel 64\n"));
  sweep();
  CHECK(all_succeeded());
  CHECK(devices[INTEL].processes_count == 1);
  CHECK(devices[INTEL].processes[0].gpu_memory_usage == 64);
  CHECK(GPUINFO_PROCESS_FIELD_VALID(&devices[INTEL].processes[0], gpu_memory_usage));
  CHECK(devices[MALI].processes_count == 0);

  // Only the device owning an unparsable fd loses its zero-usage guarantee.
  CHECK(write_fixture("proc/4194400/fdinfo/3", "unsupported 64\n"));
  sweep();
  CHECK(only_failed(INTEL));
  CHECK(devices[INTEL].processes_count == 0);

  // Query success does not imply that the driver exposes allocation sizes.
  CHECK(write_fixture("proc/4194400/fdinfo/3", "Intel 18446744073709551615\n"));
  sweep();
  CHECK(all_succeeded());
  CHECK(devices[INTEL].processes_count == 1);
  CHECK(!GPUINFO_PROCESS_FIELD_VALID(&devices[INTEL].processes[0], gpu_memory_usage));

  // Errors on a device's own fd are attributed to that device.
  failed_path = "fdinfo/3";
  injected_errno = EIO;
  sweep();
  CHECK(only_failed(INTEL));
  injected_errno = EACCES;
  sweep();
  CHECK(all_succeeded());
  failed_path = NULL;

  // A descriptor closed after its fdinfo was opened is skipped; other read
  // errors fail its device.
  injected_read_errno = ENOENT;
  sweep();
  CHECK(all_succeeded());
  CHECK(devices[INTEL].processes_count == 0);
  injected_read_errno = EIO;
  sweep();
  CHECK(only_failed(INTEL));
  injected_read_errno = 0;

  // A descriptor reused for another file before its fdinfo is read is skipped.
  CHECK(write_fixture("proc/4194400/fdinfo/3", "reused\n"));
  sweep();
  CHECK(all_succeeded());
  CHECK(write_fixture("proc/4194400/fd/3", "128"));
  CHECK(write_fixture("proc/4194400/fdinfo/3", "reused-drm\n"));
  sweep();
  CHECK(all_succeeded());
  CHECK(write_fixture("proc/4194400/fd/3", "128"));
  CHECK(write_fixture("proc/4194400/fdinfo/3", "Intel 64\n"));

  // A stat failure on any fd (e.g., a file on an unreachable network mount)
  // only means that fd is not a DRM fd.
  failed_stat_path = "3";
  sweep();
  CHECK(all_succeeded());
  CHECK(devices[INTEL].processes_count == 0);
  failed_stat_path = NULL;

  // A fd of an unmonitored device does not affect the monitored ones.
  CHECK(write_fixture("proc/4194400/fdinfo/3", "unsupported 64\n"));
  processinfo_enable_disable_callback_for(&devices[INTEL], false);
  sweep();
  CHECK(!devices[INTEL].processes_query_succeeded);
  CHECK(devices[MALI].processes_query_succeeded && devices[MSM].processes_query_succeeded &&
        devices[LEGACY].processes_query_succeeded);
  processinfo_enable_disable_callback_for(&devices[INTEL], true);

  // A fd owned by no registered device (e.g., a driver without nvtop support)
  // is offered to the devices without known nodes only, and does not fail them
  // when they cannot parse it.
  CHECK(write_fixture("proc/4194400/fd/3", "200"));
  sweep();
  CHECK(all_succeeded());
  CHECK(devices[LEGACY].processes_count == 0);
  CHECK(write_fixture("proc/4194400/fdinfo/3", "Legacy 32\n"));
  sweep();
  CHECK(all_succeeded());
  CHECK(devices[LEGACY].processes_count == 1);
  CHECK(devices[LEGACY].processes[0].gpu_memory_usage == 32);
  CHECK(write_fixture("proc/4194400/fdinfo/3", "unsupported 64\n"));
  processinfo_drop_callback(&devices[LEGACY]);
  devices[LEGACY].processes_query_succeeded = true;
  sweep();
  CHECK(all_succeeded());

  // A DRM client is accounted for once, whichever descriptors refer to it:
  // fd 3 and its duplicate fd 4 in one process, and fd 3 of another process which
  // inherited it. A distinct client of the same device is counted separately.
  // The order of the processes in the fixture directory is unspecified.
  CHECK(write_fixture("proc/4194400/fd/3", "128"));
  CHECK(write_fixture("proc/4194400/fdinfo/3", "drm-client-id:\t7\nIntel 64\n"));
  CHECK(write_fixture("proc/4194400/fd/4", "128"));
  CHECK(write_fixture("proc/4194400/fdinfo/4", "drm-client-id:\t7\nIntel 64\n"));
  CHECK(write_fixture("proc/4194401/fd/3", "128"));
  CHECK(write_fixture("proc/4194401/fdinfo/3", "drm-client-id:\t7\nIntel 64\n"));
  sweep();
  // Both holders are listed, but only the first one gets the client's memory.
  CHECK(devices[INTEL].processes_count == 2);
  unsigned with_memory = 0, without_memory = 0;
  for (unsigned i = 0; i < devices[INTEL].processes_count; ++i) {
    if (GPUINFO_PROCESS_FIELD_VALID(&devices[INTEL].processes[i], gpu_memory_usage)) {
      CHECK(devices[INTEL].processes[i].gpu_memory_usage == 64);
      with_memory++;
    } else {
      without_memory++;
    }
  }
  CHECK(with_memory == 1 && without_memory == 1);
  CHECK(write_fixture("proc/4194401/fdinfo/3", "drm-client-id:\t8\nIntel 16\n"));
  sweep();
  CHECK(devices[INTEL].processes_count == 2);
  unsigned long long valid_memory = 0;
  for (unsigned i = 0; i < devices[INTEL].processes_count; ++i) {
    if (GPUINFO_PROCESS_FIELD_VALID(&devices[INTEL].processes[i], gpu_memory_usage))
      valid_memory += devices[INTEL].processes[i].gpu_memory_usage;
  }
  // The shared client is included once, plus the second process' distinct client.
  CHECK(valid_memory == 80);

  // A copy of a client that cannot be parsed does not hide a valid copy, in
  // either directory order.
  CHECK(remove_fixture("proc/4194401/fd/3") && remove_fixture("proc/4194401/fdinfo/3"));
  CHECK(write_fixture("proc/4194400/fdinfo/3", "drm-client-id:\t7\nunsupported 64\n"));
  sweep();
  CHECK(devices[INTEL].processes_count == 1);
  CHECK(devices[INTEL].processes[0].gpu_memory_usage == 64);
  CHECK(write_fixture("proc/4194400/fdinfo/3", "drm-client-id:\t7\nIntel 64\n"));
  CHECK(write_fixture("proc/4194400/fdinfo/4", "drm-client-id:\t7\nunsupported 64\n"));
  sweep();
  CHECK(devices[INTEL].processes_count == 1);
  CHECK(devices[INTEL].processes[0].gpu_memory_usage == 64);

  // Distinct clients of one process are merged, with the shared part of each
  // client bounded by its resident memory, and the largest one kept: both
  // clients may hold the same shared buffers.
  CHECK(write_fixture("proc/4194400/fdinfo/3", "drm-client-id:\t7\nIntel 100 64\n"));
  CHECK(write_fixture("proc/4194400/fdinfo/4", "drm-client-id:\t8\nIntel 100 64\n"));
  sweep();
  CHECK(devices[INTEL].processes_count == 1);
  CHECK(devices[INTEL].processes[0].gpu_memory_usage == 200);
  CHECK(devices[INTEL].processes[0].gpu_memory_shared == 128);
  CHECK(devices[INTEL].processes[0].gpu_memory_shared_max == 64);
  CHECK(write_fixture("proc/4194400/fdinfo/3", "drm-client-id:\t7\nIntel 10 50\n"));
  CHECK(write_fixture("proc/4194400/fdinfo/4", "drm-client-id:\t8\nIntel 70 0\n"));
  sweep();
  CHECK(devices[INTEL].processes[0].gpu_memory_usage == 80);
  CHECK(devices[INTEL].processes[0].gpu_memory_shared == 10);
  CHECK(devices[INTEL].processes[0].gpu_memory_shared_max == 10);

  // Without a client id, duplicates are detected with kcmp. The fixture
  // processes do not exist, so kcmp fails and both fds are parsed rather than
  // dropping a possibly distinct client.
  CHECK(write_fixture("proc/4194400/fdinfo/3", "Intel 64\n"));
  CHECK(write_fixture("proc/4194400/fdinfo/4", "Intel 64\n"));
  sweep();
  CHECK(devices[INTEL].processes_count == 1);
  CHECK(devices[INTEL].processes[0].gpu_memory_usage == 128);
  CHECK(remove_fixture("proc/4194400/fd/4") && remove_fixture("proc/4194400/fdinfo/4"));

  // Descriptors that are not DRM devices are ignored without reading their fdinfo.
  CHECK(write_fixture("proc/4194400/fd/3", "not a device"));
  failed_path = "fdinfo/3";
  injected_errno = EIO;
  sweep();
  CHECK(all_succeeded());
  failed_path = NULL;

  // Dropped callbacks must no longer receive writes during future sweeps.
  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    processinfo_drop_callback(&devices[i]);
  devices[INTEL].processes_query_succeeded = true;
  sweep();
  CHECK(devices[INTEL].processes_query_succeeded);
  CHECK(registered_callback_entries == 0);

  for (unsigned i = 0; i < DEVICE_COUNT; ++i)
    free(devices[i].processes);
  CHECK(nftw(fixture_root, remove_entry, 16, FTW_DEPTH | FTW_PHYS) == 0);
  return EXIT_SUCCESS;
}
