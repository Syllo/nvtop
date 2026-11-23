/*
 *
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
 *
 * This file is part of Nvtop and adapted from igt-gpu-tools from Intel Corporation.
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
 *
 */

#include "nvtop/device_discovery.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/extract_processinfo_fdinfo.h"
#include "nvtop/time.h"

#include "extract_gpuinfo_intel.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <libdrm/drm.h>
#include <libdrm/xe_drm.h>
#include <libgen.h>
#include <linux/perf_event.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <uthash.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

// Copied from https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/intel/common/intel_gem.h
static inline int intel_ioctl(int fd, unsigned long request, void *arg) {
  int ret;

  do {
    ret = ioctl(fd, request, arg);
  } while (ret == -1 && (errno == EINTR || errno == EAGAIN));
  return ret;
}
// End Copy

// Copied from https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/intel/common/xe/intel_device_query.c
static void *xe_device_query_alloc_fetch(int fd, uint32_t query_id, uint32_t *len) {
  struct drm_xe_device_query query = {
      .query = query_id,
  };
  if (intel_ioctl(fd, DRM_IOCTL_XE_DEVICE_QUERY, &query))
    return NULL;

  void *data = calloc(1, query.size);
  if (!data)
    return NULL;

  query.data = (uintptr_t)data;
  if (intel_ioctl(fd, DRM_IOCTL_XE_DEVICE_QUERY, &query)) {
    free(data);
    return NULL;
  }

  if (len)
    *len = query.size;
  return data;
}
// End Copy

int gpuinfo_intel_xe_has_device_memory(struct gpu_info_intel *gpu_info) {
  uint32_t length = 0;
  struct drm_xe_query_mem_regions *regions =
      xe_device_query_alloc_fetch(gpu_info->card_fd, DRM_XE_DEVICE_QUERY_MEM_REGIONS, &length);
  if (!regions)
    return -1;
  // Every device has a system memory region; the driver reports none at all
  // when its queries are restricted (admin-only mode), leaving it unknown.
  int has_device_memory = -1;
  for (unsigned i = 0; i < regions->num_mem_regions; i++) {
    if (regions->mem_regions[i].mem_class == DRM_XE_MEM_REGION_CLASS_VRAM)
      has_device_memory = 1;
    else if (regions->mem_regions[i].mem_class == DRM_XE_MEM_REGION_CLASS_SYSMEM && has_device_memory < 0)
      has_device_memory = 0;
  }
  free(regions);
  return has_device_memory;
}

// ---- SR-IOV: per-function utilisation from the PF's xe PMU ----------------
// xe_pmu.c packs the event as: event[0:11] engine_instance[12:19]
// engine_class[20:27] function[44:59] gt[60:63]. Function 0 is the PF and
// function N is VF N. active/total ticks are relative to the function's
// scheduling share, so the ratio is utilisation of what the function was given.
#define XE_PMU_EVENT_ACTIVE 0x02
#define XE_PMU_EVENT_TOTAL 0x03

static long xe_perf_open(struct perf_event_attr *attr) { return syscall(SYS_perf_event_open, attr, -1, 0, -1, 0); }

// Resolves the PF PCI address and function number for this device: function 0
// is the PF itself, N >= 1 is the Nth VF (virtfnN-1). Returns false if the VF
// cannot be located under its PF.
static bool xe_fn_resolve(const char *pdev, char *pf_pdev, size_t pf_len, unsigned *function) {
  char path[256], target[256];
  snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/physfn", pdev);
  ssize_t n = readlink(path, target, sizeof(target) - 1);
  if (n < 0) {
    snprintf(pf_pdev, pf_len, "%s", pdev);
    *function = 0;
    return true;
  }
  target[n] = 0;
  snprintf(pf_pdev, pf_len, "%s", basename(target));
  for (unsigned i = 0; i < 256; i++) {
    snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/virtfn%u", pf_pdev, i);
    n = readlink(path, target, sizeof(target) - 1);
    if (n < 0)
      break;
    target[n] = 0;
    if (!strcmp(basename(target), pdev)) {
      *function = i + 1;
      return true;
    }
  }
  return false;
}

void gpuinfo_intel_xe_detect_sriov(struct gpu_info *gpu_info) {
  char pf_pdev[PDEV_LEN];
  unsigned function = 0;
  gpu_info->sriov_pf = gpu_info->sriov_vf = false;
  if (!xe_fn_resolve(gpu_info->pdev, pf_pdev, sizeof(pf_pdev), &function))
    return;
  if (function > 0) {
    gpu_info->sriov_vf = true;
    gpu_info->sriov_vf_index = function;
    snprintf(gpu_info->sriov_pf_pdev, sizeof(gpu_info->sriov_pf_pdev), "%s", pf_pdev);
    return;
  }
  char path[256];
  unsigned numvfs = 0;
  snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/sriov_numvfs", gpu_info->pdev);
  FILE *f = fopen(path, "r");
  if (f) {
    if (fscanf(f, "%u", &numvfs) != 1)
      numvfs = 0;
    fclose(f);
  }
  gpu_info->sriov_pf = numvfs > 0;
}

// Discovers the engines through the DRM engine query rather than assuming a
// topology, then opens the active/total counters for each on the PF's PMU.
static void xe_fn_pmu_init(struct gpu_info_intel *gpu_info) {
  gpu_info->xe_fn_pmu.state = XE_FN_PMU_UNAVAILABLE;
  if (!gpu_info->card_fd)
    return;

  char pf_pdev[PDEV_LEN];
  unsigned function;
  if (!xe_fn_resolve(gpu_info->base.pdev, pf_pdev, sizeof(pf_pdev), &function))
    return;

  char pmu_name[64], path[160];
  snprintf(pmu_name, sizeof(pmu_name), "xe_%s", pf_pdev);
  for (char *ch = pmu_name; *ch; ++ch)
    if (*ch == ':')
      *ch = '_';
  snprintf(path, sizeof(path), "/sys/bus/event_source/devices/%s/type", pmu_name);
  FILE *f = fopen(path, "r");
  unsigned type;
  if (!f)
    return;
  if (fscanf(f, "%u", &type) != 1) {
    fclose(f);
    return;
  }
  fclose(f);

  uint32_t length = 0;
  struct drm_xe_query_engines *engines =
      xe_device_query_alloc_fetch(gpu_info->card_fd, DRM_XE_DEVICE_QUERY_ENGINES, &length);
  if (!engines)
    return;

  for (unsigned i = 0; i < engines->num_engines && gpu_info->xe_fn_pmu.num_engines < XE_FN_MAX_ENGINES; i++) {
    struct drm_xe_engine_class_instance inst = engines->engines[i].instance;
    // Only the user-visible classes have PMU counters (not VM_BIND).
    if (inst.engine_class > DRM_XE_ENGINE_CLASS_COMPUTE)
      continue;
    unsigned slot = gpu_info->xe_fn_pmu.num_engines;
    bool opened = true;
    for (unsigned k = 0; k < 2; k++) {
      uint64_t cfg = (k ? XE_PMU_EVENT_TOTAL : XE_PMU_EVENT_ACTIVE) | ((uint64_t)inst.engine_instance << 12) |
                     ((uint64_t)inst.engine_class << 20) | ((uint64_t)function << 44) | ((uint64_t)inst.gt_id << 60);
      struct perf_event_attr attr = {.type = type, .size = sizeof(attr), .config = cfg};
      gpu_info->xe_fn_pmu.engine[slot].fd[k] = (int)xe_perf_open(&attr);
      if (gpu_info->xe_fn_pmu.engine[slot].fd[k] < 0)
        opened = false;
    }
    if (!opened) {
      for (unsigned k = 0; k < 2; k++)
        if (gpu_info->xe_fn_pmu.engine[slot].fd[k] >= 0)
          close(gpu_info->xe_fn_pmu.engine[slot].fd[k]);
      continue;
    }
    gpu_info->xe_fn_pmu.engine[slot].engine_class = inst.engine_class;
    gpu_info->xe_fn_pmu.engine[slot].engine_instance = inst.engine_instance;
    gpu_info->xe_fn_pmu.engine[slot].gt = inst.gt_id;
    gpu_info->xe_fn_pmu.num_engines++;
  }
  free(engines);
  if (gpu_info->xe_fn_pmu.num_engines)
    gpu_info->xe_fn_pmu.state = XE_FN_PMU_READY;
}

static void xe_fn_pmu_refresh(struct gpu_info_intel *gpu_info, struct gpuinfo_dynamic_info *dynamic_info) {
  if (gpu_info->xe_fn_pmu.state == XE_FN_PMU_UNINIT)
    xe_fn_pmu_init(gpu_info);
  if (gpu_info->xe_fn_pmu.state != XE_FN_PMU_READY)
    return;

  double video = 0, enhance = 0, overall = 0;
  for (unsigned e = 0; e < gpu_info->xe_fn_pmu.num_engines; e++) {
    uint64_t cur[2] = {0, 0};
    for (unsigned k = 0; k < 2; k++)
      if (read(gpu_info->xe_fn_pmu.engine[e].fd[k], &cur[k], sizeof(cur[k])) != sizeof(cur[k]))
        cur[k] = gpu_info->xe_fn_pmu.engine[e].prev[k];

    if (gpu_info->xe_fn_pmu.have_prev) {
      uint64_t da = cur[0] - gpu_info->xe_fn_pmu.engine[e].prev[0];
      uint64_t dt = cur[1] - gpu_info->xe_fn_pmu.engine[e].prev[1];
      if (dt > 0) {
        double pct = 100.0 * (double)da / (double)dt;
        if (pct > 100.0)
          pct = 100.0;
        if (pct > overall)
          overall = pct;
        if (gpu_info->xe_fn_pmu.engine[e].engine_class == DRM_XE_ENGINE_CLASS_VIDEO_DECODE && pct > video)
          video = pct;
        if (gpu_info->xe_fn_pmu.engine[e].engine_class == DRM_XE_ENGINE_CLASS_VIDEO_ENHANCE && pct > enhance)
          enhance = pct;
      }
    }
    gpu_info->xe_fn_pmu.engine[e].prev[0] = cur[0];
    gpu_info->xe_fn_pmu.engine[e].prev[1] = cur[1];
  }

  if (gpu_info->xe_fn_pmu.have_prev) {
    // Overall GPU load is the busiest engine of the function.
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, (unsigned)(overall + 0.5));
    SET_GPUINFO_DYNAMIC(dynamic_info, encoder_rate, (unsigned)(video + 0.5));
    SET_GPUINFO_DYNAMIC(dynamic_info, decoder_rate, (unsigned)(enhance + 0.5));
  }
  gpu_info->xe_fn_pmu.have_prev = true;
}

void gpuinfo_intel_xe_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_intel *gpu_info = container_of(_gpu_info, struct gpu_info_intel, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;

  if (gpu_info->card_fd) {
    uint32_t length = 0;
    struct drm_xe_query_mem_regions *regions =
        xe_device_query_alloc_fetch(gpu_info->card_fd, DRM_XE_DEVICE_QUERY_MEM_REGIONS, &length);
    if (regions) {
      for (unsigned i = 0; i < regions->num_mem_regions; i++) {
        struct drm_xe_mem_region mr = regions->mem_regions[i];
        // Only discrete GPUs get here: report their VRAM region
        if (mr.mem_class == DRM_XE_MEM_REGION_CLASS_VRAM) {
          SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mr.total_size);
          // Kernels before the CAP_PERFMON check was dropped report 0 kb used without it
          if (mr.used != 0) {
            SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, mr.used);
            SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, dynamic_info->total_memory - dynamic_info->used_memory);
            SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, dynamic_info->used_memory * 100 / dynamic_info->total_memory);
          }
          break;
        }
      }
      free(regions);
    }
  }
  xe_fn_pmu_refresh(gpu_info, dynamic_info);
}

// Resident memory of the client, indexed by whether the device shares its
// memory with the host. Such devices only have the system memory regions
// (system and gtt). Discrete GPUs only count the device memory their capacity
// describes, as the amdgpu backend does: the first VRAM region, the only one
// the dynamic info reports. Stolen memory is reserved by the firmware outside
// of the system memory.
static const char *const xe_drm_intel_memory[2][2] = {
    [false] = {"drm-resident-vram0", NULL},
    [true] = {"drm-resident-system", "drm-resident-gtt"},
};
// Part of the same memory in buffers shared with other DRM clients.
static const char *const xe_drm_intel_shared_memory[2][2] = {
    [false] = {"drm-shared-vram0", NULL},
    [true] = {"drm-shared-system", "drm-shared-gtt"},
};
// Render
static const char xe_drm_intel_cycles_rcs[] = "drm-cycles-rcs";
static const char xe_drm_intel_total_cycles_rcs[] = "drm-total-cycles-rcs";
// Video Decode
static const char xe_drm_intel_cycles_vcs[] = "drm-cycles-vcs";
static const char xe_drm_intel_total_cycles_vcs[] = "drm-total-cycles-vcs";
// Video Enhance
static const char xe_drm_intel_cycles_vecs[] = "drm-cycles-vecs";
static const char xe_drm_intel_total_cycles_vecs[] = "drm-total-cycles-vecs";
// Copy
static const char xe_drm_intel_cycles_bcs[] = "drm-cycles-bcs";
static const char xe_drm_intel_total_cycles_bcs[] = "drm-total-cycles-bcs";
// Compute
static const char xe_drm_intel_cycles_ccs[] = "drm-cycles-ccs";
static const char xe_drm_intel_total_cycles_ccs[] = "drm-total-cycles-ccs";

static const char *cycles_keys[] = {xe_drm_intel_cycles_rcs, xe_drm_intel_cycles_vcs, xe_drm_intel_cycles_vecs,
                                    xe_drm_intel_cycles_bcs, xe_drm_intel_cycles_ccs};

static const char *total_cycles_keys[] = {xe_drm_intel_total_cycles_rcs, xe_drm_intel_total_cycles_vcs,
                                          xe_drm_intel_total_cycles_vecs, xe_drm_intel_total_cycles_bcs,
                                          xe_drm_intel_total_cycles_ccs};

bool parse_drm_fdinfo_intel_xe(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info) {
  struct gpu_info_intel *gpu_info = container_of(info, struct gpu_info_intel, base);
  static char *line = NULL;
  static size_t line_buf_size = 0;
  ssize_t count = 0;

  bool client_id_set = false;
  unsigned cid;
  nvtop_time current_time;
  nvtop_get_current_time(&current_time);

  union intel_cycles gpu_cycles = {.array = {0}};

  union intel_cycles total_cycles = {.array = {0}};

  while ((count = getline(&line, &line_buf_size, fdinfo_file)) != -1) {
    char *key, *val;
    // Get rid of the newline if present
    if (line[count - 1] == '\n') {
      line[--count] = '\0';
    }

    if (!extract_drm_fdinfo_key_value(line, &key, &val))
      continue;

    if (!strcmp(key, drm_pdev)) {
      if (strcmp(val, gpu_info->base.pdev)) {
        return false;
      }
    } else if (!strcmp(key, drm_client_id)) {
      char *endptr;
      cid = strtoul(val, &endptr, 10);
      if (*endptr)
        continue;
      client_id_set = true;
    } else {
      bool shared_memory = info->static_info.memory_shared_with_host;
      bool is_memory = false, is_shared = false;
      for (unsigned i = 0; !is_memory && !is_shared && i < ARRAY_SIZE(xe_drm_intel_memory[0]) &&
                           xe_drm_intel_memory[shared_memory][i];
           i++) {
        is_memory = !strcmp(key, xe_drm_intel_memory[shared_memory][i]);
        is_shared = !strcmp(key, xe_drm_intel_shared_memory[shared_memory][i]);
      }
      if (is_memory || is_shared) {
        unsigned long long mem_bytes;
        if (!gpuinfo_intel_parse_fdinfo_size(val, &mem_bytes))
          continue;
        if (is_shared)
          gpuinfo_intel_add_process_shared_memory(process_info, mem_bytes);
        else
          gpuinfo_intel_add_process_memory(process_info, mem_bytes);
      } else {
        unsigned long cycles;
        char *endptr;

        // Check for cycles
        for (unsigned i = 0; i < ARRAY_SIZE(gpu_cycles.array); i++) {
          if (!strcmp(key, cycles_keys[i])) {
            cycles = strtoull(val, &endptr, 10);
            gpu_cycles.array[i] = cycles;
          }
        }

        // Check for total cycles
        for (unsigned i = 0; i < ARRAY_SIZE(total_cycles_keys); i++) {
          if (!strcmp(key, total_cycles_keys[i])) {
            cycles = strtoull(val, &endptr, 10);
            total_cycles.array[i] = cycles;
          }
        }
      }
    }
  }

  // Sum cycles for overall usage
  {
    uint64_t cycles_sum = 0;
    for (unsigned i = 0; i < ARRAY_SIZE(gpu_cycles.array); i++) {
      cycles_sum += gpu_cycles.array[i];
    }
    SET_GPUINFO_PROCESS(process_info, gpu_cycles, cycles_sum);
  }

  if (!client_id_set)
    return false;

  process_info->type = gpu_process_unknown;
  if (gpu_cycles.rcs != 0)
    process_info->type |= gpu_process_graphical;
  if (gpu_cycles.ccs != 0)
    process_info->type |= gpu_process_compute;

  struct intel_process_info_cache *cache_entry;
  struct unique_cache_id ucid = {.client_id = cid, .pid = process_info->pid, .pdev = gpu_info->base.pdev};
  HASH_FIND_CLIENT(gpu_info->last_update_process_cache, &ucid, cache_entry);
  if (cache_entry) {
    HASH_DEL(gpu_info->last_update_process_cache, cache_entry);

    // TODO: find how to extract global utilization
    // gpu util will be computed as the sum of all the processes utilization for now
    {
      uint64_t cycles_delta = gpu_cycles.rcs - cache_entry->gpu_cycles.rcs;
      uint64_t total_cycles_delta = total_cycles.rcs - cache_entry->total_cycles.rcs;
      if (total_cycles_delta > 0)
        SET_GPUINFO_PROCESS(process_info, gpu_usage, cycles_delta * 100 / total_cycles_delta);
      else
        SET_GPUINFO_PROCESS(process_info, gpu_usage, 0);
    }
    {
      uint64_t cycles_delta = gpu_cycles.ccs - cache_entry->gpu_cycles.ccs;
      uint64_t total_cycles_delta = total_cycles.ccs - cache_entry->total_cycles.ccs;
      if (total_cycles_delta > 0)
        SET_GPUINFO_PROCESS(process_info, gpu_usage, process_info->gpu_usage + cycles_delta * 100 / total_cycles_delta);
    }
    {
      uint64_t cycles_delta = gpu_cycles.vcs - cache_entry->gpu_cycles.vcs;
      uint64_t total_cycles_delta = total_cycles.vcs - cache_entry->total_cycles.vcs;
      if (total_cycles_delta > 0)
        SET_GPUINFO_PROCESS(process_info, decode_usage, cycles_delta * 100 / total_cycles_delta);
    }

  } else {
    cache_entry = calloc(1, sizeof(*cache_entry));
    if (!cache_entry)
      goto parse_fdinfo_exit;
    cache_entry->client_id.client_id = cid;
    cache_entry->client_id.pid = process_info->pid;
    cache_entry->client_id.pdev = gpu_info->base.pdev;
  }

  // Check if we already processed this client_id in the current update cycle.
  // This can happen when a process has multiple file descriptors referencing
  // the same DRM client (e.g., via DRM master operations).
  struct intel_process_info_cache *cache_entry_check;
  HASH_FIND_CLIENT(gpu_info->current_update_process_cache, &cache_entry->client_id, cache_entry_check);
  if (cache_entry_check) {
    // Already processed this client_id, free the entry if we allocated it
    if (cache_entry != cache_entry_check)
      free(cache_entry);
    goto parse_fdinfo_exit;
  }

  RESET_ALL(cache_entry->valid);
  SET_INTEL_CACHE(cache_entry, gpu_cycles, gpu_cycles);
  SET_INTEL_CACHE(cache_entry, total_cycles, total_cycles);

  HASH_ADD_CLIENT(gpu_info->current_update_process_cache, cache_entry);

parse_fdinfo_exit:
  return true;
}