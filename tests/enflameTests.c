/* SPDX-License-Identifier: GPL-3.0-or-later */

// Inject EFML queries to verify empty results without a driver or SDK.
#include "../src/extract_gcuinfo_enflame.c"

#define CHECK(condition)                                                                                                \
  do {                                                                                                                 \
    if (!(condition)) {                                                                                                \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #condition);                                                    \
      return EXIT_FAILURE;                                                                                             \
    }                                                                                                                  \
  } while (0)

static efmlReturn_v1_t query_status;

static efmlReturn_v1_t query_processes(efmlDevice_t device, unsigned *count, void *infos) {
  (void)device;
  (void)infos;
  *count = 0;
  return query_status;
}

static efmlReturn_v1_t failed_processes(efmlDevice_t device, unsigned *count, void *infos) {
  (void)device;
  (void)count;
  (void)infos;
  return -1;
}

int main(void) {
  struct gcu_info_enflame device = {0};
  for (unsigned version = 1; version <= 3; ++version) {
    memset(efmlDeviceGetComputeRunningProcesses, 0, sizeof(efmlDeviceGetComputeRunningProcesses));
    memset(efmlDeviceGetGraphicsRunningProcesses, 0, sizeof(efmlDeviceGetGraphicsRunningProcesses));
    efmlDeviceGetComputeRunningProcesses[version] = query_processes;
    query_status = EFML_SUCCESS;
    gcuinfo_enflame_get_running_processes(&device.base);
    CHECK(device.base.processes_query_succeeded);
    CHECK(device.base.processes_count == 0);
    query_status = -1;
    gcuinfo_enflame_get_running_processes(&device.base);
    CHECK(!device.base.processes_query_succeeded);
    CHECK(device.base.processes_count == 0);
    // A successful empty compute query must not hide a failed graphics query.
    efmlDeviceGetGraphicsRunningProcesses[version] = failed_processes;
    query_status = EFML_SUCCESS;
    gcuinfo_enflame_get_running_processes(&device.base);
    CHECK(!device.base.processes_query_succeeded);
  }
  memset(efmlDeviceGetComputeRunningProcesses, 0, sizeof(efmlDeviceGetComputeRunningProcesses));
  memset(efmlDeviceGetGraphicsRunningProcesses, 0, sizeof(efmlDeviceGetGraphicsRunningProcesses));
  gcuinfo_enflame_get_running_processes(&device.base);
  CHECK(!device.base.processes_query_succeeded);
  return EXIT_SUCCESS;
}
