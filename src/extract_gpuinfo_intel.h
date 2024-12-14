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
  intel_cache_engine_video_enhance_valid,
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
  uint64_t engine_video;
  uint64_t engine_video_enhance;
  uint64_t engine_compute;
  union intel_cycles gpu_cycles;
  union intel_cycles total_cycles;
  nvtop_time last_measurement_tstamp;
  unsigned char valid[(intel_cache_process_info_cache_valid_count + CHAR_BIT - 1) / CHAR_BIT];
  UT_hash_handle hh;
};

struct gpu_info_intel {
  struct gpu_info base;
  enum { DRIVER_I915, DRIVER_XE } driver;

  struct nvtop_device *card_device;
  int card_fd;
  
  struct nvtop_device *driver_device;
  struct nvtop_device *hwmon_device;
  struct intel_process_info_cache *last_update_process_cache, *current_update_process_cache; // Cached processes info

  struct nvtop_device *bridge_device;

  struct {
    unsigned energy_uj;
    struct timespec time;
  } energy;
};

extern void gpuinfo_intel_i915_refresh_dynamic_info(struct gpu_info *_gpu_info);
extern void gpuinfo_intel_xe_refresh_dynamic_info(struct gpu_info *_gpu_info);

extern bool parse_drm_fdinfo_intel_i915(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info);
extern bool parse_drm_fdinfo_intel_xe(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info);
