#include <stdint.h>
#include <uthash.h>

#define HASH_FIND_CLIENT(head, key_ptr, out_ptr) HASH_FIND(hh, head, key_ptr, sizeof(struct unique_cache_id), out_ptr)
#define HASH_ADD_CLIENT(head, in_ptr) HASH_ADD(hh, head, client_id, sizeof(struct unique_cache_id), in_ptr)

#define SET_INTEL_CACHE(cachePtr, field, value) SET_VALUE(cachePtr, field, value, intel_cache_)
#define RESET_INTEL_CACHE(cachePtr, field) INVALIDATE_VALUE(cachePtr, field, intel_cache_)
#define INTEL_CACHE_FIELD_VALID(cachePtr, field) VALUE_IS_VALID(cachePtr, field, intel_cache_)

enum intel_process_info_cache_valid {
  intel_cache_engine_render_valid = 0,
  intel_cache_engine_copy_valid,
  intel_cache_engine_video_valid,
  intel_cache_engine_video_encode_valid,
  intel_cache_engine_compute_valid,
  intel_cache_gpu_cycles_valid,
  intel_cache_total_cycles_valid,
  intel_cache_process_info_cache_valid_count
};

struct __attribute__((__packed__)) unique_cache_id {
  unsigned client_id;
  pid_t pid;
  char *pdev;
};

union intel_cycles {
  struct {
    uint64_t rcs;
    uint64_t vcs;
    uint64_t vecs;
    uint64_t bcs;
    uint64_t ccs;
  };
  uint64_t array[5];
};

struct intel_process_info_cache {
  struct unique_cache_id client_id;
  uint64_t engine_render;
  uint64_t engine_copy;
  uint64_t engine_video;        // video engine time used for decode
  uint64_t engine_video_encode; // the same video engine time, used for encode
  uint64_t engine_compute;
  union intel_cycles gpu_cycles;
  union intel_cycles total_cycles;
  nvtop_time last_measurement_tstamp;
  unsigned char valid[(intel_cache_process_info_cache_valid_count + CHAR_BIT - 1) / CHAR_BIT];
  UT_hash_handle hh;
};

#define XE_FN_MAX_ENGINES 32
// User-visible xe engine classes: render, copy, video decode, video enhance and
// compute (DRM_XE_ENGINE_CLASS_RENDER to DRM_XE_ENGINE_CLASS_COMPUTE)
#define XE_FN_NUM_ENGINE_CLASSES 5

struct gpu_info_intel {
  struct gpu_info base;
  enum { DRIVER_I915, DRIVER_XE } driver;

  struct nvtop_device *card_device;
  int card_fd;
  // SR-IOV: whether the device is a VF or a PF that supports VFs, and the VFs
  // enabled on the PF (of a VF, or the device itself) when nvtop started.
  // nvtop does not follow a change of the VFs and exits instead.
  bool sriov_capable;
  unsigned sriov_numvfs;

  struct nvtop_device *driver_device;
  struct nvtop_device *hwmon_device;
  struct intel_process_info_cache *last_update_process_cache, *current_update_process_cache; // Cached processes info

  struct nvtop_device *bridge_device;

  struct {
    unsigned energy_uj;
    struct timespec time;
  } energy;

  int perf_event_fd;

  // Per-function engine counters from the PF's xe PMU (SR-IOV). VF clients do
  // not export drm-cycles in fdinfo, so GPU utilisation is read from the PF.
  struct {
    enum { XE_FN_PMU_UNINIT, XE_FN_PMU_READY, XE_FN_PMU_UNAVAILABLE } state;
    unsigned num_engines;
    // Engines of each class on the device (only the render, compute and video
    // decode classes are counted, the ones nvtop shows)
    unsigned class_capacity[XE_FN_NUM_ENGINE_CLASSES];
    // Engines of each class whose counters opened for every function: the load
    // of a class is complete when this equals its capacity
    unsigned class_complete[XE_FN_NUM_ENGINE_CLASSES];
    struct {
      unsigned engine_class;
      unsigned num_fd;
      int *fd;              // active ticks of this device's function (or of every function for the PF)
      uint64_t prev_active; // sum of the counters at the last refresh
      uint32_t ref_clock;   // GT timestamp frequency, Hz
    } engine[XE_FN_MAX_ENGINES];
    nvtop_time prev_time;
    double prev_interval; // seconds between the last two samples, 0 if unknown
    bool have_prev;
  } xe_fn_pmu;
};

extern void gpuinfo_intel_i915_refresh_dynamic_info(struct gpu_info *_gpu_info);
extern void gpuinfo_intel_xe_refresh_dynamic_info(struct gpu_info *_gpu_info);

// Fills the SR-IOV identity (PF/VF, VF index, parent PF) of an xe device.
extern void gpuinfo_intel_xe_detect_sriov(struct gpu_info_intel *gpu_info);

// Closes the per-function PMU counters of an SR-IOV device. They are opened
// again, starting a new capture, on the next refresh.
extern void gpuinfo_intel_xe_close_fn_pmu(struct gpu_info_intel *gpu_info);

// Refreshes the utilization of an SR-IOV device from the PF's PMU, and exits if
// its VFs changed. Called for every xe device, whatever its memory.
extern void gpuinfo_intel_xe_refresh_sriov(struct gpu_info_intel *gpu_info);

// perf helpers shared by the i915 and xe code. The PMU of a device is named
// after its PCI address (e.g. xe_0000_03_00.0). The event is opened disabled.
extern void gpuinfo_intel_pmu_name(bool xe, const char *pdev, char *pmu_name, size_t len);
extern bool gpuinfo_intel_pmu_type(const char *pmu_name, uint32_t *type);
extern bool gpuinfo_intel_open_perf_event(uint32_t type, uint64_t config, int *fd);

// 1 if the device has a dedicated (VRAM) memory region, 0 if it only has the
// system memory region, -1 if the memory regions could not be queried.
extern int gpuinfo_intel_i915_has_device_memory(struct gpu_info_intel *gpu_info);
extern int gpuinfo_intel_xe_has_device_memory(struct gpu_info_intel *gpu_info);

extern bool parse_drm_fdinfo_intel_i915(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info);
extern bool parse_drm_fdinfo_intel_xe(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info);

// Adds bytes to the memory usage of the process, or to the part of it shared
// with other DRM clients.
extern void gpuinfo_intel_add_process_memory(struct gpu_process *process_info, unsigned long long bytes);
extern void gpuinfo_intel_add_process_shared_memory(struct gpu_process *process_info, unsigned long long bytes);
