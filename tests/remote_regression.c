/* Standalone, hardware-free integration test. Run with scripts/test-remote.sh. */
#include <assert.h>
#include <stdatomic.h>
#include <stdio.h>
#include <sys/stat.h>
#include "../src/extract_gpuinfo_remote.c"
void register_gpu_vendor(struct gpu_vendor *vendor) { (void)vendor; }

static atomic_int mode = 0, quit = 0;
struct server_context { int listener; bool can_stall; };
static unsigned char frame[1 << 20];
static size_t frame_size;
static void *server(void *unused) {
  struct server_context *server = unused;
  int listener = server->listener;
  while (!atomic_load(&quit)) {
    struct pollfd p = {listener, POLLIN, 0};
    if (poll(&p, 1, 50) <= 0) continue;
    int fd = accept(listener, NULL, NULL);
    if (fd < 0) continue;
    char byte;
    recv(fd, &byte, 1, 0);
    if (!server->can_stall || !atomic_load(&mode)) {
      assert(send(fd, frame, frame_size, MSG_NOSIGNAL) == (ssize_t)frame_size);
    } else {
      /* Accept but never respond: a firewall/blackhole equivalent after connect. */
      struct timespec delay = {0, 400000000};
      nanosleep(&delay, NULL);
    }
    close(fd);
  }
  return NULL;
}
static void wait_ms(unsigned ms) {
  struct timespec t = {ms / 1000, (ms % 1000) * 1000000}; nanosleep(&t, NULL);
}
int main(void) {
  LIST_HEAD(devices);
  struct gpu_info a = {0}, b = {0};
  struct gpu_process pa = {.pid = 123, .cmdline = "model-a"}, pb = {.pid = 456, .cmdline = "model-b"};
  a.processes = &pa; a.processes_count = 1;
  b.processes = &pb; b.processes_count = 1;
  SET_GPUINFO_DYNAMIC(&a.dynamic_info, gpu_util_rate, 87);
  SET_GPUINFO_DYNAMIC(&b.dynamic_info, gpu_util_rate, 0);
  list_add_tail(&a.list, &devices); list_add_tail(&b.list, &devices);
  frame_size = remote_wire_encode(&devices, frame, sizeof(frame)) + 4;
  assert(frame_size > 4);
  struct wire_gpu gpus[16]; struct wire_static st; struct wire_process procs[16 * 64];
  assert(remote_wire_decode(frame, frame_size, &st, gpus, procs) == 2);
  assert(gpus[0].gpu_util_rate == 87 && gpus[1].gpu_util_rate == 0);
  assert(procs[0].pid == 123 && procs[64].pid == 456);
  assert(!strcmp(procs[64].cmdline, "model-b"));
  /* Every truncation, including a self-consistent shortened length prefix. */
  for (size_t n = 4; n < frame_size; ++n) {
    uint8_t copy[1 << 16]; memcpy(copy, frame, n);
    unsigned len = n - 4;
    copy[0] = len >> 24; copy[1] = len >> 16; copy[2] = len >> 8; copy[3] = len;
    assert(remote_wire_decode(copy, n, &st, gpus, procs) == 0);
  }
  int listener = socket(AF_INET, SOCK_STREAM, 0); assert(listener >= 0);
  struct sockaddr_in addr = {.sin_family = AF_INET, .sin_addr.s_addr = htonl(INADDR_LOOPBACK)};
  assert(bind(listener, (void *)&addr, sizeof(addr)) == 0);
  socklen_t len = sizeof(addr); getsockname(listener, (void *)&addr, &len);
  assert(listen(listener, 8) == 0);
  struct server_context context = {listener, true};
  pthread_t thread; assert(pthread_create(&thread, NULL, server, &context) == 0);
  int second_listener = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr2 = {.sin_family = AF_INET, .sin_addr.s_addr = htonl(INADDR_LOOPBACK)};
  assert(bind(second_listener, (void *)&addr2, sizeof(addr2)) == 0);
  len = sizeof(addr2); getsockname(second_listener, (void *)&addr2, &len);
  assert(listen(second_listener, 8) == 0);
  struct server_context context2 = {second_listener, false};
  pthread_t thread2; assert(pthread_create(&thread2, NULL, server, &context2) == 0);
  char config[] = "/tmp/nvtop-test-XXXXXX";
  int fd = mkstemp(config); assert(fd >= 0);
  FILE *f = fdopen(fd, "w");
  fprintf(f, "[RemoteHost]\nHost=127.0.0.1\nPort=%u\nName=healthy\nGPUs=2\n"
             "[RemoteHost]\nHost=127.0.0.1\nPort=1\nName=unreachable\n", ntohs(addr.sin_port));
  fprintf(f, "[RemoteHost]\nHost=127.0.0.1\nPort=%u\nName=always-online\nGPUs=2\n", ntohs(addr2.sin_port));
  fclose(f); setenv("NVTOP_CONFIG_FILE", config, 1);
  assert(gpuinfo_remote_init()); assert(remote_host_count == 3);
  struct remote_host *host = list_first_entry(&remote_host_list, struct remote_host, list);
  host->expected_gpus = 8; /* An old cached count must not add six phantom panels. */
  assert(host->offline_after_ns == UINT64_C(15000000000));
  LIST_HEAD(remote_devices); unsigned count;
  assert(gpuinfo_remote_get_device_handles(&remote_devices, &count)); assert(count == 5);
  struct gpu_info *d = list_first_entry(&remote_devices, struct gpu_info, list);
  gpuinfo_remote_populate_static_info(d);
  assert(strstr(d->static_info.device_name, "@ 127.0.0.1"));
  gpuinfo_remote_refresh_dynamic_info(d); assert(!d->offline);
  gpuinfo_remote_refresh_running_processes(d); assert(d->processes[0].pid == 123);
  struct gpu_info *second = list_entry(d->list.next, struct gpu_info, list);
  gpuinfo_remote_refresh_dynamic_info(second); assert(second->dynamic_info.gpu_util_rate == 0);
  gpuinfo_remote_refresh_running_processes(second); assert(second->processes[0].pid == 456);
  atomic_store(&mode, 1); wait_ms(1600);
  nvtop_time start, end; nvtop_get_current_time(&start);
  for (unsigned i = 0; i < 10000; ++i) gpuinfo_remote_refresh_dynamic_info(d);
  nvtop_get_current_time(&end);
  assert(nvtop_difftime_u64(start, end) < UINT64_C(200000000));
  struct gpu_info *healthy = list_entry(remote_devices.prev, struct gpu_info, list);
  gpuinfo_remote_refresh_dynamic_info(healthy);
  assert(!healthy->offline && healthy->dynamic_info.gpu_util_rate == 0);
  /* A brief failed poll must preserve the chart, not trigger offline/reset. */
  assert(!d->offline && d->telemetry_stale);
  assert(GPUINFO_DYNAMIC_FIELD_VALID(&d->dynamic_info, gpu_util_rate));
  atomic_store(&mode, 0); wait_ms(1600);
  gpuinfo_remote_refresh_dynamic_info(d);
  assert(!d->offline && !d->telemetry_stale);
  /* The same missing data becomes offline only after the grace period. */
  atomic_store(&mode, 1); wait_ms(1600);
  pthread_mutex_lock(&host->lock);
  host->last_success.tv_sec -= 20;
  pthread_mutex_unlock(&host->lock);
  gpuinfo_remote_refresh_dynamic_info(d);
  assert(d->offline && !d->remote_missing && !GPUINFO_DYNAMIC_FIELD_VALID(&d->dynamic_info, gpu_util_rate));
  atomic_store(&mode, 0); wait_ms(1600);
  gpuinfo_remote_refresh_dynamic_info(d); assert(!d->offline);
  assert(d->dynamic_info.gpu_util_rate == 87);
  /* A missing ordinal is not a network outage; zero-utilization iGPUs stay online. */
  pthread_mutex_lock(&host->lock);
  host->gpu_count = 1;
  pthread_mutex_unlock(&host->lock);
  gpuinfo_remote_refresh_dynamic_info(second);
  assert(second->offline && second->remote_missing);
  pthread_mutex_lock(&host->lock);
  host->gpu_count = 2;
  pthread_mutex_unlock(&host->lock);
  gpuinfo_remote_refresh_dynamic_info(second);
  assert(!second->offline && !second->remote_missing && second->dynamic_info.gpu_util_rate == 0);
  struct gpu_info *item;
  list_for_each_entry(item, &remote_devices, list) {
    for (unsigned i = 0; i < item->processes_array_size; ++i) {
      free(item->processes[i].cmdline); free(item->processes[i].user_name);
    }
    free(item->processes);
  }
  gpuinfo_remote_shutdown();
  atomic_store(&quit, 1); pthread_join(thread, NULL); pthread_join(thread2, NULL); close(listener); close(second_listener); unlink(config);
  puts("PASS: multi-GPU/process decoding, truncated frames, independent polling, offline, reconnect");
}
