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

#include "nvtop/remote_proto.h"

#include <stdlib.h>
#include <string.h>

/*
 * Wire codec for the NVT1 remote telemetry protocol.
 *
 * The encode side walks the live `gpu_info` device list and serializes
 * each device into a fixed-layout wire struct with explicit little-endian
 * integers. The `valid[]` bitmaps are forwarded verbatim so the consumer
 * can mark absent fields N/A without extra logic.
 */

/* Fixed sizes (must match the structs) ------------------------------------ */

#define WIRE_HEADER_LEN sizeof(struct wire_header)
#define WIRE_STATIC_LEN (MAX_DEVICE_NAME + 8 * 4 + 2 + ((gpuinfo_static_info_count + CHAR_BIT - 1) / CHAR_BIT))
/* Byte offset to the gpu block's process_count field (not including it);
 * process records begin 4 bytes after this offset. */
#define WIRE_GPU_FIXED_LEN (18 * 4 + 3 * 8 + 1 + ((gpuinfo_dynamic_info_count + CHAR_BIT - 1) / CHAR_BIT))
#define WIRE_PROCESS_LEN sizeof(struct wire_process)
#define WIRE_STATIC_COUNT 1
#define WIRE_MAX_GPUS 16
#define WIRE_MAX_PROCS 64

/* LE encode/decode helpers --------------------------------------------------- */

static inline void le_put32(uint8_t *p, uint32_t v) {
  p[0] = (uint8_t)v;
  p[1] = (uint8_t)(v >> 8);
  p[2] = (uint8_t)(v >> 16);
  p[3] = (uint8_t)(v >> 24);
}

static inline void le_put64(uint8_t *p, uint64_t v) {
  le_put32(p, (uint32_t)v);
  le_put32(p + 4, (uint32_t)(v >> 32));
}

static inline uint32_t le_get32(const uint8_t *p) {
  return (uint32_t)p[0] | ((uint32_t)p[1] << 8) | ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}

static inline uint64_t le_get64(const uint8_t *p) {
  return ((uint64_t)le_get32(p + 4) << 32) | le_get32(p);
}

static size_t encode_wire_static(uint8_t *dst, const struct gpuinfo_static_info *s) {
  size_t off = 0;
  memcpy(dst + off, s->device_name, MAX_DEVICE_NAME);
  off += MAX_DEVICE_NAME;
  le_put32(dst + off, s->max_pcie_gen);
  off += 4;
  le_put32(dst + off, s->max_pcie_link_width);
  off += 4;
  le_put32(dst + off, s->temperature_shutdown_threshold);
  off += 4;
  le_put32(dst + off, s->temperature_slowdown_threshold);
  off += 4;
  le_put32(dst + off, s->n_shared_cores);
  off += 4;
  le_put32(dst + off, s->l2cache_size);
  off += 4;
  le_put32(dst + off, s->n_exec_engines);
  off += 4;
  le_put32(dst + off, s->engine_count);
  off += 4;
  dst[off] = s->integrated_graphics ? 1 : 0;
  dst[off + 1] = s->encode_decode_shared ? 1 : 0;
  off += 2;
  memcpy(dst + off, s->valid, sizeof(s->valid));
  return WIRE_STATIC_LEN;
}

static size_t encode_wire_process(uint8_t *dst, const struct gpu_process *p) {
  size_t off = 0;
  le_put32(dst + off, p->type);
  off += 4;
  le_put32(dst + off, (uint32_t)p->pid);
  off += 4;
  if (p->cmdline) {
    size_t n = strlen(p->cmdline);
    if (n >= REMOTE_CMDLINE_LEN) n = REMOTE_CMDLINE_LEN - 1;
    memcpy(dst + off, p->cmdline, n);
  } else {
    dst[off] = 0;
  }
  off += REMOTE_CMDLINE_LEN;
  if (p->user_name) {
    size_t n = strlen(p->user_name);
    if (n >= REMOTE_NAME_LEN) n = REMOTE_NAME_LEN - 1;
    memcpy(dst + off, p->user_name, n);
  } else {
    dst[off] = 0;
  }
  off += REMOTE_NAME_LEN;
  le_put64(dst + off, p->sample_delta);
  off += 8;
  le_put64(dst + off, p->gfx_engine_used);
  off += 8;
  le_put64(dst + off, p->compute_engine_used);
  off += 8;
  le_put64(dst + off, p->enc_engine_used);
  off += 8;
  le_put64(dst + off, p->dec_engine_used);
  off += 8;
  le_put64(dst + off, p->gpu_cycles);
  off += 8;
  le_put32(dst + off, p->gpu_usage);
  off += 4;
  le_put32(dst + off, p->encode_usage);
  off += 4;
  le_put32(dst + off, p->decode_usage);
  off += 4;
  le_put64(dst + off, p->gpu_memory_usage);
  off += 8;
  le_put32(dst + off, p->gpu_memory_percentage);
  off += 4;
  le_put32(dst + off, p->cpu_usage);
  off += 4;
  le_put64(dst + off, p->cpu_memory_virt);
  off += 8;
  le_put64(dst + off, p->cpu_memory_res);
  off += 8;
  memcpy(dst + off, p->valid, sizeof(p->valid));
  return WIRE_PROCESS_LEN;
}

static size_t encode_wire_gpu(uint8_t *dst, const struct gpu_info *device) {
  const struct gpuinfo_dynamic_info *d = &device->dynamic_info;
  size_t off = 0;
  le_put32(dst + off, d->gpu_clock_speed);
  off += 4;
  le_put32(dst + off, d->gpu_clock_speed_max);
  off += 4;
  le_put32(dst + off, d->mem_clock_speed);
  off += 4;
  le_put32(dst + off, d->mem_clock_speed_max);
  off += 4;
  le_put32(dst + off, d->gpu_util_rate);
  off += 4;
  le_put32(dst + off, d->mem_util_rate);
  off += 4;
  le_put32(dst + off, d->effective_load_rate);
  off += 4;
  le_put32(dst + off, d->encoder_rate);
  off += 4;
  le_put32(dst + off, d->decoder_rate);
  off += 4;
  le_put64(dst + off, d->total_memory);
  off += 8;
  le_put64(dst + off, d->free_memory);
  off += 8;
  le_put64(dst + off, d->used_memory);
  off += 8;
  le_put32(dst + off, d->pcie_link_gen);
  off += 4;
  le_put32(dst + off, d->pcie_link_width);
  off += 4;
  le_put32(dst + off, d->pcie_rx);
  off += 4;
  le_put32(dst + off, d->pcie_tx);
  off += 4;
  le_put32(dst + off, d->fan_speed);
  off += 4;
  le_put32(dst + off, d->fan_rpm);
  off += 4;
  le_put32(dst + off, d->gpu_temp);
  off += 4;
  le_put32(dst + off, d->power_draw);
  off += 4;
  le_put32(dst + off, d->power_draw_max);
  off += 4;
  dst[off] = d->multi_instance_mode ? 1 : 0;
  off += 1;
  memcpy(dst + off, d->valid, sizeof(d->valid));
  off += sizeof(d->valid);
  unsigned pc = device->processes_count;
  if (pc > WIRE_MAX_PROCS) pc = WIRE_MAX_PROCS;
  le_put32(dst + off, pc);
  off += 4;
  for (unsigned i = 0; i < pc; ++i) {
    encode_wire_process(dst + off, &device->processes[i]);
    off += WIRE_PROCESS_LEN;
  }
  return off;
}

/* Public encode: walk the device list, serialize, return total frame length */

size_t remote_wire_encode(struct list_head *monitored_gpus, uint8_t *out, size_t out_size) {
  struct gpu_info *device;
  unsigned gpu_count = 0;
  list_for_each_entry(device, monitored_gpus, list) {
    if (gpu_count < WIRE_MAX_GPUS) gpu_count++;
  }

  struct wire_header header;
  memset(&header, 0, sizeof(header));
  memcpy(header.magic, NVTOP_EXPORT_MAGIC, 4);
  header.proto_major = remote_proto_major;
  header.proto_minor = remote_proto_minor;
  header.flags = 1; /* payload integers are little-endian */
  header.static_count = WIRE_STATIC_COUNT;
  header.gpu_count = gpu_count;

  size_t static_len = WIRE_STATIC_COUNT * WIRE_STATIC_LEN;
  // Actual total = header + statics + per-gpu (fixed part + process_count + processes)
  size_t actual_total = WIRE_HEADER_LEN + static_len;
  {
    list_for_each_entry(device, monitored_gpus, list) {
      unsigned pc = device->processes_count;
      if (pc > WIRE_MAX_PROCS) pc = WIRE_MAX_PROCS;
      /* WIRE_GPU_FIXED_LEN points at the process_count field; add 4 for it,
       * then the process records. */
      actual_total += WIRE_GPU_FIXED_LEN + 4 + (size_t)pc * WIRE_PROCESS_LEN;
    }
  }

  // `out` must hold the 4-byte prefix + payload. Reserve `actual_total + 4`.
  if (actual_total + 4 > out_size) return 0;

  uint8_t *dst = out + 4;
  memcpy(dst, &header, WIRE_HEADER_LEN);
  dst += WIRE_HEADER_LEN;

  // static block: use the first monitored GPU's static info
  struct gpu_info *first = NULL;
  list_for_each_entry(device, monitored_gpus, list) {
    if (!first) first = device;
  }
  if (first) {
    encode_wire_static(dst, &first->static_info);
    dst += WIRE_STATIC_LEN;
  }

  list_for_each_entry(device, monitored_gpus, list) {
    size_t len = encode_wire_gpu(dst, device);
    dst += len;
  }

  // The payload length is exactly the number of bytes actually written.
  size_t payload_len = (size_t)(dst - (out + 4));

  // 4-byte big-endian length prefix = payload length (bytes AFTER the prefix).
  uint32_t total32 = (uint32_t)payload_len;
  out[0] = (uint8_t)(total32 >> 24);
  out[1] = (uint8_t)(total32 >> 16);
  out[2] = (uint8_t)(total32 >> 8);
  out[3] = (uint8_t)total32;

  return payload_len;
}

/* Public decode: parse a frame (4-byte big-endian length + payload) */

unsigned remote_wire_decode(const uint8_t *frame, size_t frame_size, struct wire_static *statics,
                            struct wire_gpu *gpus, struct wire_process *procs) {
  // Minimum valid frame: 4-byte length prefix + header + at least one static block
  size_t min_size = 4 + WIRE_HEADER_LEN + WIRE_STATIC_LEN;
  if (frame_size < min_size) return 0;

  uint32_t declared = ((uint32_t)frame[0] << 24) | ((uint32_t)frame[1] << 16) | ((uint32_t)frame[2] << 8) | (uint32_t)frame[3];
  if ((size_t)declared + 4 > frame_size) return 0;

  const uint8_t *dst = frame + 4;
  struct wire_header header;
  memcpy(&header, dst, sizeof(header));
  if (memcmp(header.magic, NVTOP_EXPORT_MAGIC, 4) != 0) return 0;
  if (header.proto_major > remote_proto_major) return 0;
  if (header.static_count > WIRE_STATIC_COUNT || header.gpu_count > WIRE_MAX_GPUS) return 0;

  dst += sizeof(header);
  for (size_t i = 0; i < header.static_count; ++i) {
    if (statics) {
      struct wire_static *s = &statics[i];
      size_t off = 0;
      memcpy(s->device_name, dst + off, MAX_DEVICE_NAME);
      off += MAX_DEVICE_NAME;
      s->max_pcie_gen = le_get32(dst + off);
      off += 4;
      s->max_pcie_link_width = le_get32(dst + off);
      off += 4;
      s->temperature_shutdown_threshold = le_get32(dst + off);
      off += 4;
      s->temperature_slowdown_threshold = le_get32(dst + off);
      off += 4;
      s->n_shared_cores = le_get32(dst + off);
      off += 4;
      s->l2cache_size = le_get32(dst + off);
      off += 4;
      s->n_exec_engines = le_get32(dst + off);
      off += 4;
      s->engine_count = le_get32(dst + off);
      off += 4;
      s->integrated_graphics = dst[off];
      s->encode_decode_shared = dst[off + 1];
      off += 2;
      memcpy(s->valid, dst + off, sizeof(s->valid));
      dst += WIRE_STATIC_LEN;
    } else {
      dst += WIRE_STATIC_LEN;
    }
  }

  for (size_t i = 0; i < header.gpu_count; ++i) {
    size_t off = 0;
    if (gpus) {
      struct wire_gpu *g = &gpus[i];
      g->gpu_clock_speed = le_get32(dst + off);
      off += 4;
      g->gpu_clock_speed_max = le_get32(dst + off);
      off += 4;
      g->mem_clock_speed = le_get32(dst + off);
      off += 4;
      g->mem_clock_speed_max = le_get32(dst + off);
      off += 4;
      g->gpu_util_rate = le_get32(dst + off);
      off += 4;
      g->mem_util_rate = le_get32(dst + off);
      off += 4;
      g->effective_load_rate = le_get32(dst + off);
      off += 4;
      g->encoder_rate = le_get32(dst + off);
      off += 4;
      g->decoder_rate = le_get32(dst + off);
      off += 4;
      g->total_memory = le_get64(dst + off);
      off += 8;
      g->free_memory = le_get64(dst + off);
      off += 8;
      g->used_memory = le_get64(dst + off);
      off += 8;
      g->pcie_link_gen = le_get32(dst + off);
      off += 4;
      g->pcie_link_width = le_get32(dst + off);
      off += 4;
      g->pcie_rx = le_get32(dst + off);
      off += 4;
      g->pcie_tx = le_get32(dst + off);
      off += 4;
      g->fan_speed = le_get32(dst + off);
      off += 4;
      g->fan_rpm = le_get32(dst + off);
      off += 4;
      g->gpu_temp = le_get32(dst + off);
      off += 4;
      g->power_draw = le_get32(dst + off);
      off += 4;
      g->power_draw_max = le_get32(dst + off);
      off += 4;
      g->multi_instance_mode = dst[off];
      off += 1;
      memcpy(g->valid, dst + off, sizeof(g->valid));
      off += sizeof(g->valid);
      g->process_count = le_get32(dst + off);
      off += 4;
      if (g->process_count > WIRE_MAX_PROCS) g->process_count = WIRE_MAX_PROCS;
      for (unsigned j = 0; j < g->process_count; ++j) {
        if (procs) {
          struct wire_process *w = &procs[j];
          size_t poff = 0;
          w->type = le_get32(dst + poff);
          poff += 4;
          w->pid = le_get32(dst + poff);
          poff += 4;
          memcpy(w->cmdline, dst + poff, REMOTE_CMDLINE_LEN);
          poff += REMOTE_CMDLINE_LEN;
          memcpy(w->user_name, dst + poff, REMOTE_NAME_LEN);
          poff += REMOTE_NAME_LEN;
          w->sample_delta = le_get64(dst + poff);
          poff += 8;
          w->gfx_engine_used = le_get64(dst + poff);
          poff += 8;
          w->compute_engine_used = le_get64(dst + poff);
          poff += 8;
          w->enc_engine_used = le_get64(dst + poff);
          poff += 8;
          w->dec_engine_used = le_get64(dst + poff);
          poff += 8;
          w->gpu_cycles = le_get64(dst + poff);
          poff += 8;
          w->gpu_usage = le_get32(dst + poff);
          poff += 4;
          w->encode_usage = le_get32(dst + poff);
          poff += 4;
          w->decode_usage = le_get32(dst + poff);
          poff += 4;
          w->gpu_memory_usage = le_get64(dst + poff);
          poff += 8;
          w->gpu_memory_percentage = le_get32(dst + poff);
          poff += 4;
          w->cpu_usage = le_get32(dst + poff);
          poff += 4;
          w->cpu_memory_virt = le_get64(dst + poff);
          poff += 8;
          w->cpu_memory_res = le_get64(dst + poff);
          poff += 8;
          memcpy(w->valid, dst + poff, sizeof(w->valid));
        }
        dst += WIRE_PROCESS_LEN;
      }
      dst += WIRE_GPU_FIXED_LEN;
    } else {
      // skip this gpu block: fixed part + process_count + process records
      unsigned pc = le_get32(dst + WIRE_GPU_FIXED_LEN);
      if (pc > WIRE_MAX_PROCS) pc = WIRE_MAX_PROCS;
      dst += WIRE_GPU_FIXED_LEN + (size_t)pc * WIRE_PROCESS_LEN;
    }
  }

  return (unsigned)header.gpu_count;
}
