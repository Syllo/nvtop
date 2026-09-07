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

#ifndef NVTOP_REMOTE_PROTO_H__
#define NVTOP_REMOTE_PROTO_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "extract_gpuinfo_common.h"
#include "list.h"
#include "time.h"

/*
 * Remote GPU telemetry wire protocol ("NVT1").
 *
 * A snapshot frame is one length-prefixed message:
 *
 *   [4-byte big-endian total_len]
 *   [wire_header]
 *   [wire_static * static_count]
 *   [ (wire_gpu + wire_process * process_count) * gpu_count ]
 *
 * All multi-byte integers in the payload are explicit little-endian;
 * the length prefix is big-endian (RFC 768 style). The `valid[]`
 * bitmaps are forwarded verbatim so the client can render absent
 * fields as N/A with zero extra logic.
 */

#define NVTOP_EXPORT_DEFAULT_PORT 8765
#define NVTOP_EXPORT_MAGIC "NVT1"

#define NVTOP_REMOTE_CACHE_NS (900 * 1000 * 1000) // 900ms

#define REMOTE_NAME_LEN 64
#define REMOTE_HOST_LEN 64
#define REMOTE_CMDLINE_LEN 512

enum remote_proto_version {
  remote_proto_major = 1,
  remote_proto_minor = 0,
};

/* flags bit 0 set => payload integers are little-endian */
struct wire_header {
  char magic[4];
  uint8_t proto_major;
  uint8_t proto_minor;
  uint8_t flags;
  uint8_t static_count;
  uint16_t gpu_count;
  uint32_t reserved;
};

struct wire_static {
  char device_name[MAX_DEVICE_NAME];
  uint32_t max_pcie_gen;
  uint32_t max_pcie_link_width;
  uint32_t temperature_shutdown_threshold;
  uint32_t temperature_slowdown_threshold;
  uint32_t n_shared_cores;
  uint32_t l2cache_size;
  uint32_t n_exec_engines;
  uint32_t engine_count;
  uint8_t integrated_graphics;
  uint8_t encode_decode_shared;
  uint8_t valid[(gpuinfo_static_info_count + CHAR_BIT - 1) / CHAR_BIT];
};

struct wire_gpu {
  uint32_t gpu_clock_speed;
  uint32_t gpu_clock_speed_max;
  uint32_t mem_clock_speed;
  uint32_t mem_clock_speed_max;
  uint32_t gpu_util_rate;
  uint32_t mem_util_rate;
  uint32_t effective_load_rate;
  uint32_t encoder_rate;
  uint32_t decoder_rate;
  uint64_t total_memory;
  uint64_t free_memory;
  uint64_t used_memory;
  uint32_t pcie_link_gen;
  uint32_t pcie_link_width;
  uint32_t pcie_rx;
  uint32_t pcie_tx;
  uint32_t fan_speed;
  uint32_t fan_rpm;
  uint32_t gpu_temp;
  uint32_t power_draw;
  uint32_t power_draw_max;
  uint8_t multi_instance_mode;
  uint8_t valid[(gpuinfo_dynamic_info_count + CHAR_BIT - 1) / CHAR_BIT];
  uint16_t process_count;
};

struct wire_process {
  uint32_t type;
  uint32_t pid;
  char cmdline[REMOTE_CMDLINE_LEN];
  char user_name[REMOTE_NAME_LEN];
  uint64_t sample_delta;
  uint64_t gfx_engine_used;
  uint64_t compute_engine_used;
  uint64_t enc_engine_used;
  uint64_t dec_engine_used;
  uint64_t gpu_cycles;
  uint32_t gpu_usage;
  uint32_t encode_usage;
  uint32_t decode_usage;
  uint64_t gpu_memory_usage;
  uint32_t gpu_memory_percentage;
  uint32_t cpu_usage;
  uint64_t cpu_memory_virt;
  uint64_t cpu_memory_res;
  uint8_t valid[(gpuinfo_process_info_count + CHAR_BIT - 1) / CHAR_BIT];
};

struct remote_host {
  struct list_head list;
  char name[REMOTE_NAME_LEN];
  char host[REMOTE_HOST_LEN];
  unsigned short port;
  int sock;
  nvtop_time last_refresh, last_success;
  unsigned gpu_count;
  struct gpu_info_remote *gpus;
  struct wire_gpu *last_good;
  unsigned last_good_capacity;
  struct wire_static *last_good_static;
  struct wire_process *last_good_procs;
  unsigned last_good_procs_capacity;
  bool has_data;
};

struct gpu_info_remote {
  struct gpu_info base;
  struct remote_host *host;
  unsigned host_idx;
  unsigned gpu_idx;
};

/* Encode the current monitored GPU list into one length-prefixed frame.
 * The 4-byte length prefix + payload occupies `total_len` bytes; the caller
 * supplies `out` with `total_len + 4` bytes. Returns the payload size on
 * success, 0 on error. */
size_t remote_wire_encode(struct list_head *monitored_gpus, uint8_t *out, size_t out_size);

/* Decode one frame (the 4-byte length prefix + payload) into caller-provided
 * arrays. Returns the number of GPUs on success, 0 on error. */
unsigned remote_wire_decode(const uint8_t *frame, size_t frame_size, struct wire_static *statics,
                            struct wire_gpu *gpus, struct wire_process *procs);

#endif // NVTOP_REMOTE_PROTO_H__
