/*
 *
 * Copyright (C) 2026 Nvtop contributors
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
 */

#include "ini.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/remote_proto.h"
#include "nvtop/time.h"

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/stat.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

/*
 * Remote GPU vendor: dials remote nvtop exporters (see --export) and
 * renders each remote GPU as a regular `gpu_info` node, mirroring the
 * per-host asynchronous workers so a dead host never stalls the UI.
 *
 * Configuration comes from the existing nvtop INI file (XDG or -c
 * override): repeating `[RemoteHost]` sections:
 *
 *   [RemoteHost]
 *   Host = remote-gpu-host-1
 *   Port = 8765
 *   Name = Host 1
 *
 *   [RemoteHost]
 *   Host = remote-gpu-host-2
 *   Name = Host 2
 */

static unsigned remote_host_count;
static struct list_head remote_host_list;

/* vtable callbacks */
static bool gpuinfo_remote_init(void);
static void gpuinfo_remote_shutdown(void);
static const char *gpuinfo_remote_last_error_string(void);
static bool gpuinfo_remote_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_remote_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_remote_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_remote_refresh_running_processes(struct gpu_info *_gpu_info);

struct gpu_vendor gpu_vendor_remote = {
  .init = gpuinfo_remote_init,
  .shutdown = gpuinfo_remote_shutdown,
  .last_error_string = gpuinfo_remote_last_error_string,
  .get_device_handles = gpuinfo_remote_get_device_handles,
  .populate_static_info = gpuinfo_remote_populate_static_info,
  .refresh_dynamic_info = gpuinfo_remote_refresh_dynamic_info,
  .refresh_running_processes = gpuinfo_remote_refresh_running_processes,
  .name = "Remote",
};

__attribute__((constructor)) static void init_extract_gpuinfo_remote(void) {
  register_gpu_vendor(&gpu_vendor_remote);
}

/* INI handler: collect [RemoteHost] sections */

/* struct remote_host is defined in nvtop/remote_proto.h */

struct remote_ini_ctx {
  char pending_name[REMOTE_NAME_LEN];
  char pending_host[REMOTE_HOST_LEN];
  unsigned short pending_port;
  unsigned pending_gpus;
};

/* Remember discovered slot counts so rebooting with a host offline retains all its panels. */
static void inventory_path(struct remote_host *h, char *path, size_t size) {
  const char *home = getenv("HOME");
  if (!home) { path[0] = 0; return; }
  char dir[4096];
  snprintf(dir, sizeof(dir), "%s/.nvtop-logs", home);
  mkdir(dir, 0700);
  uint64_t hash = UINT64_C(14695981039346656037);
  for (const unsigned char *p = (const unsigned char *)h->host; *p; ++p)
    hash = (hash ^ *p) * UINT64_C(1099511628211);
  snprintf(path, size, "%s/host-%llx-%u.gpus", dir, (unsigned long long)hash, h->port);
}

static void remote_host_commit(struct remote_ini_ctx *c) {
  if (!c->pending_host[0])
    return;
  struct remote_host *h = (struct remote_host *)calloc(1, sizeof(*h));
  if (!h)
    return;
  snprintf(h->name, sizeof(h->name), "%s", c->pending_name[0] ? c->pending_name : c->pending_host);
  snprintf(h->host, sizeof(h->host), "%s", c->pending_host);
  h->port = c->pending_port ? c->pending_port : NVTOP_EXPORT_DEFAULT_PORT;
  h->sock = -1;
  h->expected_gpus = c->pending_gpus;
  char inventory[8192];
  inventory_path(h, inventory, sizeof(inventory));
  FILE *saved = inventory[0] ? fopen(inventory, "r") : NULL;
  if (saved) {
    unsigned count;
    if (fscanf(saved, "%u", &count) == 1 && count <= 16 && count > h->expected_gpus)
      h->expected_gpus = count;
    fclose(saved);
  }
  pthread_mutex_init(&h->lock, NULL);
  h->gpu_count = 0;
  h->gpus = NULL;
  h->last_good = (struct wire_gpu *)calloc(16, sizeof(struct wire_gpu));
  h->last_good_static = (struct wire_static *)calloc(1, sizeof(struct wire_static));
  h->last_good_procs = (struct wire_process *)calloc(16 * 64, sizeof(struct wire_process));
  if (!h->last_good || !h->last_good_static || !h->last_good_procs) {
    free(h->last_good); free(h->last_good_static); free(h->last_good_procs);
    pthread_mutex_destroy(&h->lock); free(h); return;
  }
  h->has_data = false;
  list_add_tail(&h->list, &remote_host_list);
  remote_host_count++;
  c->pending_name[0] = '\0';
  c->pending_host[0] = '\0';
  c->pending_port = 0;
  c->pending_gpus = 0;
}

static int remote_ini_handler(void *ctx, const char *section, const char *name, const char *value) {
  struct remote_ini_ctx *c = (struct remote_ini_ctx *)ctx;
  if (!section)
    return true;
  if (strcmp(section, "RemoteHost") != 0) {
    /* A new non-RemoteHost section ends the previous [RemoteHost] block */
    remote_host_commit(c);
    return true;
  }
  if (name && value) {
    if (strcmp(name, "Name") == 0) {
      snprintf(c->pending_name, sizeof(c->pending_name), "%s", value);
    } else if (strcmp(name, "Host") == 0) {
      snprintf(c->pending_host, sizeof(c->pending_host), "%s", value);
    } else if (strcmp(name, "GPUs") == 0) {
      long n = strtol(value, NULL, 10);
      c->pending_gpus = n > 0 && n <= 16 ? (unsigned)n : 1;
    } else if (strcmp(name, "Port") == 0) {
      c->pending_port = (unsigned short)strtol(value, NULL, 10);
    }
    return true;
  }
  /* [RemoteHost] header line: commit any previously accumulated block */
  remote_host_commit(c);
  return true;
}

/* Connect to a host with a short timeout so a dead host never stalls the
 * UI. Returns the socket fd or -1. */
struct connect_cleanup {
  int *sock;
  struct addrinfo *addresses;
};
static void cleanup_connect(void *arg) {
  struct connect_cleanup *c = arg;
  if (*c->sock >= 0) close(*c->sock);
  freeaddrinfo(c->addresses);
}
static int remote_host_connect(struct remote_host *h) {
  struct addrinfo hints, *res = NULL;
  char port_str[8];
  int sock = -1;

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  snprintf(port_str, sizeof(port_str), "%u", h->port);
  if (getaddrinfo(h->host, port_str, &hints, &res) != 0 || !res)
    return -1;

  struct connect_cleanup cleanup = {&sock, res};
  pthread_cleanup_push(cleanup_connect, &cleanup);
  for (struct addrinfo *ai = res; ai; ai = ai->ai_next) {
    sock = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
    if (sock < 0)
      continue;
    struct timeval tv = {0, 250000}; /* 250 ms */
    setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
    setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    int flags = fcntl(sock, F_GETFL, 0);
    fcntl(sock, F_SETFL, flags | O_NONBLOCK);
    int rc = connect(sock, ai->ai_addr, ai->ai_addrlen);
    if (rc < 0 && errno == EINPROGRESS) {
      struct pollfd fd = {.fd = sock, .events = POLLOUT};
      if (poll(&fd, 1, 250) > 0) {
        int error = 0;
        socklen_t size = sizeof(error);
        if (getsockopt(sock, SOL_SOCKET, SO_ERROR, &error, &size) == 0 && !error)
          rc = 0;
      }
    }
    if (rc == 0) {
      fcntl(sock, F_SETFL, flags);
      break;
    }
    close(sock);
    sock = -1;
  }
  pthread_cleanup_pop(0);
  freeaddrinfo(res);
  return sock;
}

/* Pull one frame: send 1 POLL byte, read the length-prefixed frame, and
 * store it as last-good. Returns true on success. */
static bool remote_host_refresh(struct remote_host *h) {
  h->last_error = "DNS/connect failed or timed out";
  if (h->sock < 0) {
    h->sock = remote_host_connect(h);
    if (h->sock < 0)
      return false;
  }
  h->last_error = "poll write failed";
  if (send(h->sock, "\x01", 1, MSG_NOSIGNAL) != 1) {
    /* A dead/dropped socket: close and reset so the next refresh re-dials. */
    close(h->sock);
    h->sock = -1;
    return false;
  }

  uint8_t *buf = h->frame;
  if (!buf) {
    close(h->sock);
    h->sock = -1;
    return false;
  }
  h->last_error = "frame read failed or timed out";
  nvtop_time started;
  nvtop_get_current_time(&started);
  size_t got = 0;
  /* Read the 4-byte big-endian length prefix first. */
  while (got < 4) {
    nvtop_time now;
    nvtop_get_current_time(&now);
    if (nvtop_difftime_u64(started, now) > UINT64_C(1000000000)) {
      close(h->sock); h->sock = -1; return false;
    }
    ssize_t n = recv(h->sock, buf + got, 4 - got, 0);
    if (n <= 0) {

      close(h->sock);
      h->sock = -1;
      return false;
    }
    got += n;
  }
  uint32_t len = ((uint32_t)buf[0] << 24) | ((uint32_t)buf[1] << 16) | ((uint32_t)buf[2] << 8) | (uint32_t)buf[3];
  h->last_error = "invalid frame length";
  if (len < sizeof(struct wire_header) || len > (1 << 20)) {

    close(h->sock);
    h->sock = -1;
    return false;
  }
  h->last_error = "frame body read failed or timed out";
  got = 4;
  while (got < 4 + len) {
    nvtop_time now;
    nvtop_get_current_time(&now);
    if (nvtop_difftime_u64(started, now) > UINT64_C(1000000000)) {
      close(h->sock); h->sock = -1; return false;
    }
    ssize_t n = recv(h->sock, buf + got, 4 + len - got, 0);
    if (n <= 0) {

      close(h->sock);
      h->sock = -1;
      return false;
    }
    got += n;
  }

  h->last_error = "malformed telemetry frame";
  unsigned gpu_count = remote_wire_decode(buf, 4 + len, h->last_good_static, h->last_good, h->last_good_procs);
  if (gpu_count == 0) {
    close(h->sock);
    h->sock = -1;

    return false;
  }
  close(h->sock); /* Exporters close after every frame. */
  h->sock = -1;
  h->gpu_count = gpu_count;


  nvtop_get_current_time(&h->last_success);
  h->has_data = true;
  h->last_error = NULL;
  return true;
}

/* Each host owns its network worker. DNS and firewall delays never run on the UI thread. */
static pthread_mutex_t log_lock = PTHREAD_MUTEX_INITIALIZER;
static void remote_log(struct remote_host *h, bool online) {
  const char *dir = getenv("NVTOP_LOG_DIR");
  char fallback[4096], path[8192];
  if (!dir) {
    const char *home = getenv("HOME");
    if (!home) return;
    snprintf(fallback, sizeof(fallback), "%s/.nvtop-logs", home);
    dir = fallback;
  }
  pthread_mutex_lock(&log_lock);
  mkdir(dir, 0700);
  time_t now = time(NULL);
  /* 24 hourly slots: truncate a slot when reused the next day. */
  snprintf(path, sizeof(path), "%s/remote-%02ld.log", dir, (long)(now / 3600 % 24));
  struct stat st;
  bool old = stat(path, &st) == 0 && st.st_mtime / 3600 != now / 3600;
  FILE *f = fopen(path, old ? "w" : "a");
  if (f) {
    fprintf(f, "%ld host=%s port=%u state=%s gpus=%u detail=%s\n", (long)now, h->host,
            h->port, online ? "online" : "offline", h->gpu_count,
            online ? "fresh telemetry" : (h->last_error ? h->last_error : "unknown error"));
    fclose(f);
  }
  /* Expire untouched slots as well, including after a long shutdown. */
  for (unsigned i = 0; i < 24; ++i) {
    snprintf(path, sizeof(path), "%s/remote-%02u.log", dir, i);
    if (stat(path, &st) == 0 && now - st.st_mtime >= 86400) unlink(path);
  }
  pthread_mutex_unlock(&log_lock);
}

static void *remote_worker(void *arg) {
  struct remote_host *h = arg;
  struct remote_host *s = h->snapshot;
  bool previous = false, first = true;
  unsigned attempts = 0;
  for (;;) {
    pthread_testcancel();
    bool ok = remote_host_refresh(s);
    /* No cancellation while holding a lock or publishing a snapshot. */
    int old;
    pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &old);
    pthread_mutex_lock(&h->lock);
    h->has_data = ok;
    if (ok) {
      h->gpu_count = s->gpu_count;
      h->last_success = s->last_success;
      memcpy(h->last_good, s->last_good, 16 * sizeof(*s->last_good));
      memcpy(h->last_good_static, s->last_good_static, sizeof(*s->last_good_static));
      memcpy(h->last_good_procs, s->last_good_procs, 16 * 64 * sizeof(*s->last_good_procs));
    }
    h->snapshot_ready = true;
    pthread_mutex_unlock(&h->lock);
    if (ok && s->gpu_count > s->expected_gpus) {
      char path[8192];
      inventory_path(s, path, sizeof(path));
      FILE *saved = path[0] ? fopen(path, "w") : NULL;
      if (saved) { fprintf(saved, "%u\n", s->gpu_count); fclose(saved); }
      s->expected_gpus = s->gpu_count;
    }
    if (first || ok != previous || (!ok && ++attempts % 60 == 0)) remote_log(s, ok);
    first = false;
    previous = ok;
    pthread_setcancelstate(old, NULL);
    struct timespec delay = {0, 900000000};
    nanosleep(&delay, NULL);
  }
  return NULL;
}

/* vtable implementation --------------------------------------------------- */

static bool gpuinfo_remote_init(void) {
  INIT_LIST_HEAD(&remote_host_list);
  remote_host_count = 0;

  /* Reuse the existing config-file path resolution so the remote host list
   * lives in the same INI as the rest of the nvtop settings. */
  struct remote_ini_ctx ctx;
  memset(&ctx, 0, sizeof(ctx));

  /* The remote host list lives in the same INI as the rest of the nvtop
   * settings. The path is resolved the same way `interface_options.c`
   * resolves it: $NVTOP_CONFIG_FILE override, then XDG config dir +
   * `nvtop/interface.ini`. */
  char *config_path = getenv("NVTOP_CONFIG_FILE");
  if (!config_path) {
    const char *xdg_config_dir = getenv("XDG_CONFIG_HOME");
    if (!xdg_config_dir)
      xdg_config_dir = getenv("HOME");
    if (xdg_config_dir) {
      static char default_config_path[4096];
      snprintf(default_config_path, sizeof(default_config_path), getenv("XDG_CONFIG_HOME") ? "%s/nvtop/interface.ini" : "%s/.config/nvtop/interface.ini", xdg_config_dir);
      config_path = default_config_path;
    }
  }
  if (config_path) {
    FILE *f = fopen(config_path, "r");
    if (f) {
      ini_parse_file(f, remote_ini_handler, &ctx);
      fclose(f);
      /* Commit a trailing [RemoteHost] block at EOF */
      remote_host_commit(&ctx);
    }
  }
  /* Mirror TPU: return true if at least one host is configured, even if
   * every host is currently unreachable. */
  return remote_host_count > 0;
}

static void gpuinfo_remote_shutdown(void) {
  struct remote_host *h, *h_safe;
  list_for_each_entry_safe(h, h_safe, &remote_host_list, list) {
    if (h->worker_started) {
      pthread_cancel(h->worker);
      pthread_join(h->worker, NULL);
    }
    if (h->snapshot) {
      if (h->snapshot->sock >= 0) close(h->snapshot->sock);
      free(h->snapshot->frame);
      free(h->snapshot->last_good);
      free(h->snapshot->last_good_static);
      free(h->snapshot->last_good_procs);
      free(h->snapshot);
    }
    pthread_mutex_destroy(&h->lock);
    free(h->gpus);
    if (h->sock >= 0)
      close(h->sock);
    free(h->last_good);
    free(h->last_good_static);
    free(h->last_good_procs);
    free(h);
  }
  INIT_LIST_HEAD(&remote_host_list);
  remote_host_count = 0;
}

static const char *gpuinfo_remote_last_error_string(void) {
  return "Remote GPU telemetry";
}

static bool gpuinfo_remote_get_device_handles(struct list_head *devices, unsigned *count) {
  struct remote_host *h;
  *count = 0;
  list_for_each_entry(h, &remote_host_list, list) {
    h->snapshot = calloc(1, sizeof(*h));
    if (!h->snapshot) continue;
    struct remote_host *s = h->snapshot;
    snprintf(s->host, sizeof(s->host), "%s", h->host);
    s->port = h->port;
    s->sock = -1;
    s->frame = malloc((1 << 20) + 4);
    s->last_good = calloc(16, sizeof(struct wire_gpu));
    s->last_good_static = calloc(1, sizeof(struct wire_static));
    s->last_good_procs = calloc(16 * 64, sizeof(struct wire_process));
    if (s->frame && s->last_good && s->last_good_static && s->last_good_procs)
      h->worker_started = pthread_create(&h->worker, NULL, remote_worker, h) == 0;
  }
  /* One shared startup grace period, not one timeout per host. */
  for (unsigned attempt = 0; attempt < 100; ++attempt) {
    bool ready = true;
    list_for_each_entry(h, &remote_host_list, list) {
      pthread_mutex_lock(&h->lock);
      ready &= h->snapshot_ready || !h->worker_started;
      pthread_mutex_unlock(&h->lock);
    }
    if (ready) break;
    struct timespec delay = {0, 10000000};
    nanosleep(&delay, NULL);
  }
  unsigned host_idx = 0;
  list_for_each_entry(h, &remote_host_list, list) {
    pthread_mutex_lock(&h->lock);
    unsigned slots = h->gpu_count > h->expected_gpus ? h->gpu_count : h->expected_gpus;
    if (!slots) slots = 1;
    h->gpus = calloc(slots, sizeof(*h->gpus));
    if (h->gpus) for (unsigned i = 0; i < slots; ++i) {
      struct gpu_info_remote *gpu = &h->gpus[i];
      gpu->base.vendor = &gpu_vendor_remote;
      gpu->base.offline = true;
      gpu->base.remote = true;
      gpu->host = h;
      gpu->host_idx = host_idx;
      gpu->gpu_idx = i;
      snprintf(gpu->base.pdev, PDEV_LEN, "R%u_%u", host_idx, i);
      list_add_tail(&gpu->base.list, devices);
      ++*count;
    }
    pthread_mutex_unlock(&h->lock);
    ++host_idx;
  }
  return remote_host_count > 0;
}

static void gpuinfo_remote_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_remote *gpu = container_of(_gpu_info, struct gpu_info_remote, base);
  struct gpuinfo_static_info *static_info = &_gpu_info->static_info;
  struct remote_host *h = gpu->host;
  RESET_ALL(static_info->valid);
  pthread_mutex_lock(&h->lock);

  if (h->last_good_static && gpu->gpu_idx < h->gpu_count) {
    struct wire_static *ws = h->last_good_static;
    snprintf(static_info->device_name, sizeof(static_info->device_name), "%s GPU%u", h->name, gpu->gpu_idx);
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
    if (IS_VALID(gpuinfo_max_pcie_gen_valid, ws->valid))
      SET_GPUINFO_STATIC(static_info, max_pcie_gen, ws->max_pcie_gen);
    if (IS_VALID(gpuinfo_max_pcie_link_width_valid, ws->valid))
      SET_GPUINFO_STATIC(static_info, max_pcie_link_width, ws->max_pcie_link_width);
    if (IS_VALID(gpuinfo_temperature_shutdown_threshold_valid, ws->valid))
      SET_GPUINFO_STATIC(static_info, temperature_shutdown_threshold, ws->temperature_shutdown_threshold);
    if (IS_VALID(gpuinfo_temperature_slowdown_threshold_valid, ws->valid))
      SET_GPUINFO_STATIC(static_info, temperature_slowdown_threshold, ws->temperature_slowdown_threshold);
    if (IS_VALID(gpuinfo_n_shared_cores_valid, ws->valid))
      SET_GPUINFO_STATIC(static_info, n_shared_cores, ws->n_shared_cores);
    if (IS_VALID(gpuinfo_l2cache_size_valid, ws->valid))
      SET_GPUINFO_STATIC(static_info, l2cache_size, ws->l2cache_size);
    if (IS_VALID(gpuinfo_n_exec_engines_valid, ws->valid))
      SET_GPUINFO_STATIC(static_info, n_exec_engines, ws->n_exec_engines);
    if (IS_VALID(gpuinfo_engine_count_valid, ws->valid))
      SET_GPUINFO_STATIC(static_info, engine_count, ws->engine_count);
    static_info->integrated_graphics = ws->integrated_graphics;
    static_info->encode_decode_shared = ws->encode_decode_shared;
  } else {
    snprintf(static_info->device_name, sizeof(static_info->device_name), "%s GPU%u", h->name, gpu->gpu_idx);
    SET_VALID(gpuinfo_device_name_valid, static_info->valid);
  }
  pthread_mutex_unlock(&h->lock);
}

static void gpuinfo_remote_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_remote *gpu = container_of(_gpu_info, struct gpu_info_remote, base);
  struct gpuinfo_dynamic_info *dynamic_info = &_gpu_info->dynamic_info;
  struct remote_host *h = gpu->host;

  struct wire_gpu sample;
  nvtop_time now;
  nvtop_get_current_time(&now);
  pthread_mutex_lock(&h->lock);
  bool online = h->has_data && gpu->gpu_idx < h->gpu_count &&
      nvtop_difftime_u64(h->last_success, now) < UINT64_C(3000000000);
  if (online) sample = h->last_good[gpu->gpu_idx];
  pthread_mutex_unlock(&h->lock);
  _gpu_info->offline = !online;
  RESET_ALL(dynamic_info->valid);
  if (!online) { _gpu_info->processes_count = 0; return; }
  struct wire_gpu *w = &sample;

  RESET_ALL(dynamic_info->valid);
  if (IS_VALID(gpuinfo_gpu_clock_speed_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, w->gpu_clock_speed);
  if (IS_VALID(gpuinfo_gpu_clock_speed_max_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, w->gpu_clock_speed_max);
  if (IS_VALID(gpuinfo_mem_clock_speed_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_clock_speed, w->mem_clock_speed);
  if (IS_VALID(gpuinfo_mem_clock_speed_max_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_clock_speed_max, w->mem_clock_speed_max);
  if (IS_VALID(gpuinfo_gpu_util_rate_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, w->gpu_util_rate);
  if (IS_VALID(gpuinfo_mem_util_rate_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, w->mem_util_rate);
  if (IS_VALID(gpuinfo_encoder_rate_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, encoder_rate, w->encoder_rate);
  if (IS_VALID(gpuinfo_decoder_rate_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, decoder_rate, w->decoder_rate);
  if (IS_VALID(gpuinfo_total_memory_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, (unsigned long long)w->total_memory);
  if (IS_VALID(gpuinfo_free_memory_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, (unsigned long long)w->free_memory);
  if (IS_VALID(gpuinfo_used_memory_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, (unsigned long long)w->used_memory);
  if (IS_VALID(gpuinfo_pcie_link_gen_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_gen, w->pcie_link_gen);
  if (IS_VALID(gpuinfo_pcie_link_width_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_width, w->pcie_link_width);
  if (IS_VALID(gpuinfo_pcie_rx_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_rx, w->pcie_rx);
  if (IS_VALID(gpuinfo_pcie_tx_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_tx, w->pcie_tx);
  if (IS_VALID(gpuinfo_fan_speed_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, fan_speed, w->fan_speed);
  if (IS_VALID(gpuinfo_fan_rpm_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, fan_rpm, w->fan_rpm);
  if (IS_VALID(gpuinfo_gpu_temp_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, w->gpu_temp);
  if (IS_VALID(gpuinfo_power_draw_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, w->power_draw);
  if (IS_VALID(gpuinfo_power_draw_max_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, w->power_draw_max);
  if (IS_VALID(gpuinfo_multi_instance_mode_valid, w->valid))
    SET_GPUINFO_DYNAMIC(dynamic_info, multi_instance_mode, w->multi_instance_mode);
}

static void gpuinfo_remote_refresh_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_remote *gpu = container_of(_gpu_info, struct gpu_info_remote, base);
  struct remote_host *h = gpu->host;
  struct wire_gpu sample;
  struct wire_process samples[64];
  pthread_mutex_lock(&h->lock);
  bool online = h->has_data && !_gpu_info->offline && gpu->gpu_idx < h->gpu_count;
  if (online) {
    sample = h->last_good[gpu->gpu_idx];
    memcpy(samples, &h->last_good_procs[gpu->gpu_idx * 64], sizeof(samples));
  }
  pthread_mutex_unlock(&h->lock);
  if (!online) { _gpu_info->processes_count = 0; return; }
  struct wire_gpu *w = &sample;
  if (w->process_count == 0) {
    _gpu_info->processes_count = 0;
    return;
  }
  _gpu_info->processes_count = w->process_count;
  if (_gpu_info->processes_array_size < w->process_count) {
    unsigned old = _gpu_info->processes_array_size;
    struct gpu_process *next = realloc(_gpu_info->processes, w->process_count * sizeof(*next));
    if (!next) { _gpu_info->processes_count = 0; return; }
    memset(next + old, 0, (w->process_count - old) * sizeof(*next));
    _gpu_info->processes = next;
    _gpu_info->processes_array_size = w->process_count;
  }
  for (unsigned i = 0; i < w->process_count; ++i) {
    struct wire_process *wp = &samples[i];
    struct gpu_process *p = &_gpu_info->processes[i];
    RESET_ALL(p->valid);
    p->type = (enum gpu_process_type)wp->type;
    p->pid = (pid_t)wp->pid;
    if (_gpu_info->processes[i].cmdline)
      free(_gpu_info->processes[i].cmdline);
    _gpu_info->processes[i].cmdline = strdup(wp->cmdline[0] ? wp->cmdline : "N/A");
    if (IS_VALID(gpuinfo_process_cmdline_valid, wp->valid))
      SET_VALID(gpuinfo_process_cmdline_valid, p->valid);
    if (wp->user_name[0]) {
      free(p->user_name);
      p->user_name = strdup(wp->user_name);
      SET_VALID(gpuinfo_process_user_name_valid, p->valid);
    }
    p->sample_delta = wp->sample_delta;
    p->gfx_engine_used = wp->gfx_engine_used;
    p->compute_engine_used = wp->compute_engine_used;
    p->enc_engine_used = wp->enc_engine_used;
    p->dec_engine_used = wp->dec_engine_used;
    p->gpu_cycles = wp->gpu_cycles;
    if (IS_VALID(gpuinfo_process_gpu_usage_valid, wp->valid)) {
      p->gpu_usage = wp->gpu_usage;
      SET_VALID(gpuinfo_process_gpu_usage_valid, p->valid);
    }
    if (IS_VALID(gpuinfo_process_encode_usage_valid, wp->valid)) {
      p->encode_usage = wp->encode_usage;
      SET_VALID(gpuinfo_process_encode_usage_valid, p->valid);
    }
    if (IS_VALID(gpuinfo_process_decode_usage_valid, wp->valid)) {
      p->decode_usage = wp->decode_usage;
      SET_VALID(gpuinfo_process_decode_usage_valid, p->valid);
    }
    if (IS_VALID(gpuinfo_process_gpu_memory_usage_valid, wp->valid)) {
      p->gpu_memory_usage = (unsigned long long)wp->gpu_memory_usage;
      SET_VALID(gpuinfo_process_gpu_memory_usage_valid, p->valid);
    }
    if (IS_VALID(gpuinfo_process_gpu_memory_percentage_valid, wp->valid)) {
      p->gpu_memory_percentage = wp->gpu_memory_percentage;
      SET_VALID(gpuinfo_process_gpu_memory_percentage_valid, p->valid);
    }
    if (IS_VALID(gpuinfo_process_cpu_usage_valid, wp->valid)) {
      p->cpu_usage = wp->cpu_usage;
      SET_VALID(gpuinfo_process_cpu_usage_valid, p->valid);
    }
  }
}
