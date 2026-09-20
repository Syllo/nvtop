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
 * TPU backend's 900 ms cache + last-good fallback so a dead host never
 * stalls the UI.
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

static struct remote_host **remote_hosts;
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
};

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
  h->gpu_count = 0;
  h->gpus = NULL;
  h->last_good = (struct wire_gpu *)calloc(8, sizeof(struct wire_gpu));
  h->last_good_static = (struct wire_static *)calloc(1, sizeof(struct wire_static));
  h->last_good_procs = (struct wire_process *)calloc(64, sizeof(struct wire_process));
  h->has_data = false;
  list_add_tail(&h->list, &remote_host_list);
  remote_host_count++;
  remote_hosts = (struct remote_host **)realloc(remote_hosts, remote_host_count * sizeof(*remote_hosts));
  remote_hosts[remote_host_count - 1] = h;
  c->pending_name[0] = '\0';
  c->pending_host[0] = '\0';
  c->pending_port = 0;
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

  for (struct addrinfo *ai = res; ai; ai = ai->ai_next) {
    sock = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
    if (sock < 0)
      continue;
    struct timeval tv = {0, 150 * 1000 * 1000}; /* 150 ms */
    setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
    setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    if (connect(sock, ai->ai_addr, ai->ai_addrlen) == 0)
      break;
    close(sock);
    sock = -1;
  }
  freeaddrinfo(res);
  return sock;
}

/* Pull one frame: send 1 POLL byte, read the length-prefixed frame, and
 * store it as last-good. Returns true on success. */
static bool remote_host_refresh(struct remote_host *h) {
  if (h->sock < 0) {
    h->sock = remote_host_connect(h);
    if (h->sock < 0)
      return false;
  }
  if (send(h->sock, "\x01", 1, MSG_NOSIGNAL) != 1) {
    /* A dead/dropped socket: close and reset so the next refresh re-dials. */
    close(h->sock);
    h->sock = -1;
    return false;
  }

  uint8_t *buf = (uint8_t *)malloc(1 << 16);
  if (!buf) {
    close(h->sock);
    h->sock = -1;
    return false;
  }
  size_t got = 0;
  /* Read the 4-byte big-endian length prefix first. */
  while (got < 4) {
    ssize_t n = recv(h->sock, buf + got, 4 - got, 0);
    if (n <= 0) {
      free(buf);
      close(h->sock);
      h->sock = -1;
      return false;
    }
    got += n;
  }
  uint32_t len = ((uint32_t)buf[0] << 24) | ((uint32_t)buf[1] << 16) | ((uint32_t)buf[2] << 8) | (uint32_t)buf[3];
  if (len == 0 || len > (1 << 16)) {
    free(buf);
    close(h->sock);
    h->sock = -1;
    return false;
  }
  got = 4;
  while (got < 4 + len) {
    ssize_t n = recv(h->sock, buf + got, 4 + len - got, 0);
    if (n <= 0) {
      free(buf);
      close(h->sock);
      h->sock = -1;
      return false;
    }
    got += n;
  }

  /* Decode */
  /* Ensure per-host buffers exist. The header says how many GPUs this
   * host reports; resize the last_good arrays to fit (grow-only). */
  {
    struct wire_header hdr;
    memcpy(&hdr, buf + 4, sizeof(hdr));
    unsigned nc = hdr.gpu_count;
    if (!h->last_good || h->last_good_capacity < nc) {
      h->last_good = (struct wire_gpu *)realloc(h->last_good, nc * sizeof(struct wire_gpu));
      h->last_good_capacity = nc;
    }
    if (!h->last_good_static)
      h->last_good_static = (struct wire_static *)calloc(1, sizeof(struct wire_static));
    if (!h->last_good_procs || h->last_good_procs_capacity < 64) {
      h->last_good_procs = (struct wire_process *)calloc(64, sizeof(struct wire_process));
      h->last_good_procs_capacity = 64;
    }
  }
  unsigned gpu_count = remote_wire_decode(buf, 4 + len, h->last_good_static, h->last_good, h->last_good_procs);
  if (gpu_count == 0) {
    free(buf);
    return false;
  }
  h->gpu_count = gpu_count;
  free(buf);

  nvtop_get_current_time(&h->last_success);
  h->has_data = true;
  return true;
}

static bool remote_cache_valid(struct remote_host *h) {
  nvtop_time now;
  nvtop_get_current_time(&now);
  return nvtop_difftime_u64(h->last_success, now) < NVTOP_REMOTE_CACHE_NS;
}

/* vtable implementation --------------------------------------------------- */

static bool gpuinfo_remote_init(void) {
  INIT_LIST_HEAD(&remote_host_list);
  remote_hosts = NULL;
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
      snprintf(default_config_path, sizeof(default_config_path), "%s/.config/nvtop/interface.ini", xdg_config_dir);
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
    if (h->sock >= 0)
      close(h->sock);
    free(h->last_good);
    free(h->last_good_static);
    free(h->last_good_procs);
    free(h);
  }
  INIT_LIST_HEAD(&remote_host_list);
  free(remote_hosts);
  remote_hosts = NULL;
  remote_host_count = 0;
}

static const char *gpuinfo_remote_last_error_string(void) {
  return "Remote GPU telemetry";
}

static bool gpuinfo_remote_get_device_handles(struct list_head *devices, unsigned *count) {
  struct remote_host *h;
  *count = 0;
  list_for_each_entry(h, &remote_host_list, list) {
    unsigned host_idx = *count;
    /* Try an initial frame so we learn gpu_count + static info. */
    if (h->sock < 0)
      h->sock = remote_host_connect(h);
    if (h->sock >= 0 && remote_host_refresh(h)) {
      /* Allocate one gpu_info per remote GPU on this host. */
      if (!h->gpus)
        h->gpus = (struct gpu_info_remote *)calloc((size_t)h->gpu_count, sizeof(*h->gpus));
      for (unsigned i = 0; i < h->gpu_count; ++i) {
        struct gpu_info_remote *gpu = &h->gpus[i];
        gpu->base.vendor = &gpu_vendor_remote;
        gpu->host = h;
        gpu->host_idx = host_idx;
        gpu->gpu_idx = i;
        snprintf(gpu->base.pdev, PDEV_LEN, "R%u_%u", host_idx, i);
        gpu->base.processes_count = 0;
        gpu->base.processes = NULL;
        gpu->base.processes_array_size = 0;
        list_add_tail(&gpu->base.list, devices);
        *count += 1;
      }
    }
  }
  return remote_host_count > 0;
}

static void gpuinfo_remote_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_remote *gpu = container_of(_gpu_info, struct gpu_info_remote, base);
  struct gpuinfo_static_info *static_info = &_gpu_info->static_info;
  struct remote_host *h = gpu->host;
  RESET_ALL(static_info->valid);

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
}

static void gpuinfo_remote_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_remote *gpu = container_of(_gpu_info, struct gpu_info_remote, base);
  struct gpuinfo_dynamic_info *dynamic_info = &_gpu_info->dynamic_info;
  struct remote_host *h = gpu->host;

  /* TPU-style 900 ms cache + last-good fallback: within the window reuse
   * the cached frame; otherwise pull a fresh one. On failure keep the
   * last-good data (stale) rather than blanking the panel. */
  if (!remote_cache_valid(h))
    remote_host_refresh(h);

  if (!h->has_data || !h->last_good)
    return;
  if (gpu->gpu_idx >= h->gpu_count)
    return;
  struct wire_gpu *w = &h->last_good[gpu->gpu_idx];

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
  if (!h->has_data || !h->last_good)
    return;
  if (gpu->gpu_idx >= h->gpu_count)
    return;
  struct wire_gpu *w = &h->last_good[gpu->gpu_idx];
  if (w->process_count == 0) {
    _gpu_info->processes_count = 0;
    return;
  }
  _gpu_info->processes_count = w->process_count;
  if (_gpu_info->processes_array_size < w->process_count) {
    _gpu_info->processes_array_size = w->process_count;
    _gpu_info->processes = (struct gpu_process *)malloc(_gpu_info->processes_array_size * sizeof(*_gpu_info->processes));
    memset(_gpu_info->processes, 0, _gpu_info->processes_array_size * sizeof(*_gpu_info->processes));
  }
  for (unsigned i = 0; i < w->process_count; ++i) {
    struct wire_process *wp = &h->last_good_procs[i];
    struct gpu_process *p = &_gpu_info->processes[i];
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
