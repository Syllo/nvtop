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
// DRM clients on a VF do not export drm-cycles in fdinfo (the VF cannot read
// the timestamp register), so the utilisation of SR-IOV functions is read from
// the per-function engine counters of the PF's PMU instead.
// xe_pmu.c packs the event as: event[0:11] engine_instance[12:19]
// engine_class[20:27] function[44:59] gt[60:63]. Function 0 is the PF and
// function N is VF N; with VFs enabled, function 0 only counts the PF's own
// work, so the functions do not overlap. The total-ticks counter grows with
// wall time scaled by the function's scheduling share
// (xe_guc_engine_activity.c), so active/total is the load on the function's
// own share. Utilisation is instead active ticks over wall-clock ticks: the
// active ticks count at the GT reference clock, so a VF reports its share of
// the whole GPU and the PF row reports every function combined.
//
// The engine percentages are combined like gputop (igt-gpu-tools) does for
// fdinfo: the load of an engine class is the busy time of its engines divided
// by the number of engines of the class. The GPU load is the busier of the
// render and compute classes: they can be busy at the same time, so their sum
// would count that time twice. The fdinfo path takes the same per-class load
// for each process, but the device load then adds up the processes. The shared
// ENC/DEC meter shows the video decode (VCS) class.
//
// Each open engine counter holds GT forcewake (xe_pmu.c), so the PF's GTs do
// not enter RC6 while an SR-IOV device is monitored. The counters are closed
// when the device stops being monitored.
#define XE_PMU_EVENT_ACTIVE 0x02

// Reads an SR-IOV count of a PF: sriov_numvfs (VFs enabled) or sriov_totalvfs
// (VFs supported). Returns false if it cannot be read (the device is no PF, or
// e.g. no file descriptor is left).
static bool xe_sriov_read_count(const char *pdev, const char *attribute, unsigned *count) {
  char path[256];
  snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/%s", pdev, attribute);
  FILE *f = fopen(path, "r");
  if (!f)
    return false;
  bool ok = fscanf(f, "%u", count) == 1;
  fclose(f);
  return ok;
}

// Number of VFs enabled on a PF, 0 if the device is no PF or it cannot be read.
static unsigned xe_sriov_numvfs(const char *pdev) {
  unsigned numvfs;
  return xe_sriov_read_count(pdev, "sriov_numvfs", &numvfs) ? numvfs : 0;
}

void gpuinfo_intel_xe_detect_sriov(struct gpu_info_intel *intel_info) {
  struct gpu_info *gpu_info = &intel_info->base;
  gpu_info->sriov_pf = gpu_info->sriov_vf = false;
  char path[256], target[256];
  snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/physfn", gpu_info->pdev);
  ssize_t n = readlink(path, target, sizeof(target) - 1);
  if (n < 0) {
    // Not a VF: a PF if it has VFs enabled. One that supports VFs is still
    // watched, as nvtop exits if VFs are enabled later.
    // A count that cannot be read leaves the device unwatched: comparing later
    // reads against an unknown count would see a change that never happened.
    unsigned totalvfs;
    intel_info->sriov_capable = xe_sriov_read_count(gpu_info->pdev, "sriov_totalvfs", &totalvfs) && totalvfs > 0 &&
                                xe_sriov_read_count(gpu_info->pdev, "sriov_numvfs", &intel_info->sriov_numvfs);
    if (!intel_info->sriov_capable)
      intel_info->sriov_numvfs = 0;
    gpu_info->sriov_pf = intel_info->sriov_numvfs > 0;
    return;
  }
  // A VF: find its index among the virtfnN links of its PF (VF N is virtfnN-1)
  target[n] = 0;
  char pf_pdev[PDEV_LEN];
  snprintf(pf_pdev, sizeof(pf_pdev), "%s", basename(target));
  for (unsigned i = 0; i < 256; i++) {
    snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/virtfn%u", pf_pdev, i);
    n = readlink(path, target, sizeof(target) - 1);
    if (n < 0)
      break;
    target[n] = 0;
    if (!strcmp(basename(target), gpu_info->pdev)) {
      gpu_info->sriov_vf = true;
      gpu_info->sriov_vf_index = i + 1;
      snprintf(gpu_info->sriov_pf_pdev, sizeof(gpu_info->sriov_pf_pdev), "%s", pf_pdev);
      // Without the VF count of its PF, a VF keeps its identity but is not
      // measured, as for a PF above
      intel_info->sriov_capable = xe_sriov_read_count(pf_pdev, "sriov_numvfs", &intel_info->sriov_numvfs);
      return;
    }
  }
}

// Discovers the engines through the DRM engine query rather than assuming a
// topology, then opens the active-ticks counter for each on the PF's PMU: the
// device's own function for a VF, every function for the PF.
static void xe_fn_pmu_init(struct gpu_info_intel *gpu_info) {
  gpu_info->xe_fn_pmu.state = XE_FN_PMU_UNAVAILABLE;
  if (!gpu_info->card_fd)
    return;

  const char *pf_pdev;
  unsigned first_fn, last_fn;
  if (gpu_info->base.sriov_vf) {
    pf_pdev = gpu_info->base.sriov_pf_pdev;
    first_fn = last_fn = gpu_info->base.sriov_vf_index;
  } else {
    pf_pdev = gpu_info->base.pdev;
    first_fn = 0;
    last_fn = xe_sriov_numvfs(pf_pdev);
  }

  char pmu_name[64];
  uint32_t type;
  gpuinfo_intel_pmu_name(true, pf_pdev, pmu_name, sizeof(pmu_name));
  if (!gpuinfo_intel_pmu_type(pmu_name, &type))
    return;
  // Kernels before 6.16 have no function field and ignore its bits: every
  // function would then read the activity of the whole device
  char format_path[160];
  snprintf(format_path, sizeof(format_path), "/sys/bus/event_source/devices/%s/format/function", pmu_name);
  if (access(format_path, F_OK) != 0)
    return;

  uint32_t length = 0;
  struct drm_xe_query_engines *engines =
      xe_device_query_alloc_fetch(gpu_info->card_fd, DRM_XE_DEVICE_QUERY_ENGINES, &length);
  if (!engines)
    return;

  // Per-GT timestamp frequency, to turn wall time into ticks
  uint32_t ref_clock[8] = {0};
  uint32_t gt_length = 0;
  struct drm_xe_query_gt_list *gts =
      xe_device_query_alloc_fetch(gpu_info->card_fd, DRM_XE_DEVICE_QUERY_GT_LIST, &gt_length);
  if (gts) {
    for (unsigned i = 0; i < gts->num_gt; i++)
      if (gts->gt_list[i].gt_id < 8)
        ref_clock[gts->gt_list[i].gt_id] = gts->gt_list[i].reference_clock;
    free(gts);
  }

  for (unsigned i = 0; i < engines->num_engines; i++) {
    struct drm_xe_engine_class_instance inst = engines->engines[i].instance;
    // Only open the classes nvtop shows: render and compute for the GPU load,
    // video decode for ENC/DEC. Copy and video enhance are not used.
    if (inst.engine_class != DRM_XE_ENGINE_CLASS_RENDER && inst.engine_class != DRM_XE_ENGINE_CLASS_COMPUTE &&
        inst.engine_class != DRM_XE_ENGINE_CLASS_VIDEO_DECODE)
      continue;
    // Every engine of the class counts in its capacity, even if its counter
    // is not read below: it then counts as idle
    gpu_info->xe_fn_pmu.class_capacity[inst.engine_class]++;
    if (gpu_info->xe_fn_pmu.num_engines >= XE_FN_MAX_ENGINES)
      continue;
    // Without the reference clock the ticks cannot be turned into a percentage
    if (inst.gt_id >= 8 || !ref_clock[inst.gt_id])
      continue;
    unsigned slot = gpu_info->xe_fn_pmu.num_engines;
    int *fds = malloc((last_fn - first_fn + 1) * sizeof(*fds));
    if (!fds)
      continue;
    unsigned nfd = 0;
    for (unsigned fn = first_fn; fn <= last_fn; fn++) {
      uint64_t cfg = XE_PMU_EVENT_ACTIVE | ((uint64_t)inst.engine_instance << 12) |
                     ((uint64_t)inst.engine_class << 20) | ((uint64_t)fn << 44) | ((uint64_t)inst.gt_id << 60);
      int fd;
      // A function whose counter cannot be opened is left out of the sum
      if (!gpuinfo_intel_open_perf_event(type, cfg, &fd))
        continue;
      ioctl(fd, PERF_EVENT_IOC_ENABLE, 0);
      fds[nfd++] = fd;
    }
    if (!nfd) {
      free(fds);
      continue;
    }
    if (nfd == last_fn - first_fn + 1)
      gpu_info->xe_fn_pmu.class_complete[inst.engine_class]++;
    gpu_info->xe_fn_pmu.engine[slot].fd = fds;
    gpu_info->xe_fn_pmu.engine[slot].num_fd = nfd;
    gpu_info->xe_fn_pmu.engine[slot].engine_class = inst.engine_class;
    gpu_info->xe_fn_pmu.engine[slot].ref_clock = ref_clock[inst.gt_id];
    gpu_info->xe_fn_pmu.num_engines++;
  }
  free(engines);
  if (gpu_info->xe_fn_pmu.num_engines)
    gpu_info->xe_fn_pmu.state = XE_FN_PMU_READY;
}

void gpuinfo_intel_xe_close_fn_pmu(struct gpu_info_intel *gpu_info) {
  for (unsigned e = 0; e < gpu_info->xe_fn_pmu.num_engines; e++) {
    for (unsigned k = 0; k < gpu_info->xe_fn_pmu.engine[e].num_fd; k++)
      close(gpu_info->xe_fn_pmu.engine[e].fd[k]);
    free(gpu_info->xe_fn_pmu.engine[e].fd);
  }
  memset(&gpu_info->xe_fn_pmu, 0, sizeof(gpu_info->xe_fn_pmu));
  gpu_info->xe_fn_pmu.state = XE_FN_PMU_UNINIT;
}

void gpuinfo_intel_xe_refresh_sriov(struct gpu_info_intel *gpu_info) {
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  gpu_info->base.gpu_util_rate_measured = false;
  if (!gpu_info->sriov_capable)
    return;

  // VFs are usually set up once, when the host starts. Changing their number
  // removes all of them first and creates new devices, which nvtop does not
  // follow: the counters would then read the activity of other functions.
  const char *pf_pdev = gpu_info->base.sriov_vf ? gpu_info->base.sriov_pf_pdev : gpu_info->base.pdev;
  unsigned numvfs;
  // A failed read says nothing about the VFs: check again on the next refresh
  if (xe_sriov_read_count(pf_pdev, "sriov_numvfs", &numvfs) && numvfs != gpu_info->sriov_numvfs) {
    char message[200];
    snprintf(message, sizeof(message),
             "The SR-IOV virtual functions of %s changed (%u enabled, %u when nvtop started). "
             "Restart nvtop to monitor them.",
             pf_pdev, numvfs, gpu_info->sriov_numvfs);
    gpuinfo_request_exit(message);
    return;
  }
  // A PF without VFs keeps the utilisation computed from fdinfo
  if (!gpu_info->base.sriov_vf && !gpu_info->base.sriov_pf)
    return;
  if (gpu_info->xe_fn_pmu.state == XE_FN_PMU_UNINIT)
    xe_fn_pmu_init(gpu_info);
  if (gpu_info->xe_fn_pmu.state != XE_FN_PMU_READY)
    return;

  nvtop_time now;
  nvtop_get_current_time(&now);
  double elapsed = gpu_info->xe_fn_pmu.have_prev ? nvtop_difftime(gpu_info->xe_fn_pmu.prev_time, now) : 0;
  // A gap much longer than the previous interval (e.g. the device was not
  // monitored for a while) would report the average over the whole gap: start
  // a new capture instead.
  bool valid = gpu_info->xe_fn_pmu.have_prev && elapsed > 0 &&
               !(gpu_info->xe_fn_pmu.prev_interval > 0 && elapsed > 4 * gpu_info->xe_fn_pmu.prev_interval);

  uint64_t cur[XE_FN_MAX_ENGINES];
  for (unsigned e = 0; e < gpu_info->xe_fn_pmu.num_engines; e++) {
    cur[e] = 0;
    for (unsigned k = 0; k < gpu_info->xe_fn_pmu.engine[e].num_fd; k++) {
      uint64_t v;
      if (read(gpu_info->xe_fn_pmu.engine[e].fd[k], &v, sizeof(v)) != sizeof(v)) {
        // The ticks of this sample are incomplete: start a new capture
        gpu_info->xe_fn_pmu.have_prev = false;
        gpu_info->xe_fn_pmu.prev_interval = 0;
        return;
      }
      cur[e] += v;
    }
    // A counter going backwards was reset: start a new capture from here
    if (cur[e] < gpu_info->xe_fn_pmu.engine[e].prev_active)
      valid = false;
  }

  if (valid) {
    // Busy percentage of each engine, added up per class
    double class_busy[XE_FN_NUM_ENGINE_CLASSES] = {0};
    double video_busiest = 0;
    for (unsigned e = 0; e < gpu_info->xe_fn_pmu.num_engines; e++) {
      double wall_ticks = elapsed * gpu_info->xe_fn_pmu.engine[e].ref_clock;
      double pct = 100.0 * (double)(cur[e] - gpu_info->xe_fn_pmu.engine[e].prev_active) / wall_ticks;
      if (pct > 100.0)
        pct = 100.0;
      class_busy[gpu_info->xe_fn_pmu.engine[e].engine_class] += pct;
      if (gpu_info->xe_fn_pmu.engine[e].engine_class == DRM_XE_ENGINE_CLASS_VIDEO_DECODE && pct > video_busiest)
        video_busiest = pct;
    }
    double class_pct[XE_FN_NUM_ENGINE_CLASSES] = {0};
    for (unsigned c = 0; c < XE_FN_NUM_ENGINE_CLASSES; c++) {
      if (gpu_info->xe_fn_pmu.class_capacity[c])
        class_pct[c] = class_busy[c] / gpu_info->xe_fn_pmu.class_capacity[c];
    }
    double gpu = class_pct[DRM_XE_ENGINE_CLASS_RENDER] > class_pct[DRM_XE_ENGINE_CLASS_COMPUTE]
                     ? class_pct[DRM_XE_ENGINE_CLASS_RENDER]
                     : class_pct[DRM_XE_ENGINE_CLASS_COMPUTE];
    double video = class_pct[DRM_XE_ENGINE_CLASS_VIDEO_DECODE];
    // A class is only fully measured if the counters of all its engines opened
    // for every function. Otherwise its load is a lower bound.
    const unsigned *complete = gpu_info->xe_fn_pmu.class_complete;
    const unsigned *capacity = gpu_info->xe_fn_pmu.class_capacity;
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, (unsigned)(gpu + 0.5));
    // Keep the GPU load as is only when it covers every render and compute
    // engine; a partial one is still raised to the sum of the processes
    gpu_info->base.gpu_util_rate_measured =
        complete[DRM_XE_ENGINE_CLASS_RENDER] == capacity[DRM_XE_ENGINE_CLASS_RENDER] &&
        complete[DRM_XE_ENGINE_CLASS_COMPUTE] == capacity[DRM_XE_ENGINE_CLASS_COMPUTE];
    // Intel shares one ENC/DEC meter (decoder_rate) for the video (VCS) engines,
    // which do both decode and encode. Video enhance (VECS) is not part of it.
    // A reported value replaces the estimate from the processes, so only
    // report a complete one.
    if (complete[DRM_XE_ENGINE_CLASS_VIDEO_DECODE] == capacity[DRM_XE_ENGINE_CLASS_VIDEO_DECODE]) {
      SET_GPUINFO_DYNAMIC(dynamic_info, encoder_rate, (unsigned)(video + 0.5));
      SET_GPUINFO_DYNAMIC(dynamic_info, decoder_rate, (unsigned)(video + 0.5));
      // The average hides a single saturated engine, as a stream runs on one
      // engine: also give the busiest one when there are several
      if (capacity[DRM_XE_ENGINE_CLASS_VIDEO_DECODE] > 1) {
        SET_GPUINFO_DYNAMIC(dynamic_info, encoder_busiest_engine_rate, (unsigned)(video_busiest + 0.5));
        SET_GPUINFO_DYNAMIC(dynamic_info, decoder_busiest_engine_rate, (unsigned)(video_busiest + 0.5));
      }
    }
  }
  for (unsigned e = 0; e < gpu_info->xe_fn_pmu.num_engines; e++)
    gpu_info->xe_fn_pmu.engine[e].prev_active = cur[e];
  gpu_info->xe_fn_pmu.prev_interval = valid ? elapsed : 0;
  gpu_info->xe_fn_pmu.prev_time = now;
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

// Number of engines of a class, when there is more than one. drm-cycles adds
// up the busy cycles of all the engines of the class, so the load of the class
// is divided by its capacity, as gputop (igt-gpu-tools) does.
static const char *capacity_keys[] = {"drm-engine-capacity-rcs", "drm-engine-capacity-vcs", "drm-engine-capacity-vecs",
                                      "drm-engine-capacity-bcs", "drm-engine-capacity-ccs"};

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

  // A class without a capacity key has a single engine
  union intel_cycles capacity = {.array = {1, 1, 1, 1, 1}};

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
        if (!extract_drm_fdinfo_size(val, &mem_bytes))
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

        // Check for the number of engines of a class (zero is not allowed)
        for (unsigned i = 0; i < ARRAY_SIZE(capacity_keys); i++) {
          if (!strcmp(key, capacity_keys[i])) {
            unsigned long long engines = strtoull(val, &endptr, 10);
            if (endptr != val && !*endptr && engines > 0)
              capacity.array[i] = engines;
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
      // The GPU timestamp of a working device always advances: no total cycles
      // means the kernel does not report them, as for the clients of a VF
      if (total_cycles_delta > 0)
        SET_GPUINFO_PROCESS(process_info, gpu_usage, cycles_delta * 100 / (total_cycles_delta * capacity.rcs));
    }
    {
      uint64_t cycles_delta = gpu_cycles.ccs - cache_entry->gpu_cycles.ccs;
      uint64_t total_cycles_delta = total_cycles.ccs - cache_entry->total_cycles.ccs;
      if (total_cycles_delta > 0) {
        // Render and compute engines feed the same render/compute unit and can be
        // busy at the same time, which their sum would count twice. The busier of
        // the two is the least time either had work, so it never overstates.
        uint64_t compute_usage = cycles_delta * 100 / (total_cycles_delta * capacity.ccs);
        if (!GPUINFO_PROCESS_FIELD_VALID(process_info, gpu_usage) || compute_usage > process_info->gpu_usage)
          SET_GPUINFO_PROCESS(process_info, gpu_usage, compute_usage);
      }
    }
    {
      uint64_t cycles_delta = gpu_cycles.vcs - cache_entry->gpu_cycles.vcs;
      uint64_t total_cycles_delta = total_cycles.vcs - cache_entry->total_cycles.vcs;
      if (total_cycles_delta > 0)
        SET_GPUINFO_PROCESS(process_info, decode_usage, cycles_delta * 100 / (total_cycles_delta * capacity.vcs));
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