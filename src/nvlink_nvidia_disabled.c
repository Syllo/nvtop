/*
 * Fallback implementations for NVLink functions when NVIDIA support is disabled.
 * All return 0 / false / no-op to keep the build clean when no NVIDIA GPUs
 * are present at compile time.
 */

#include "nvtop/extract_gpuinfo_common.h"

unsigned nvtop_get_nvlink_info(struct gpu_info *gpu_info, struct nvlink_info *nvlink_info) {
  (void)gpu_info;
  (void)nvlink_info;
  return 0;
}

bool nvtop_get_nvlink_error_counts(struct gpu_info *gpu_info,
                                    unsigned long long *out_errors,
                                    unsigned long long *out_corrections) {
  (void)gpu_info;
  (void)out_errors;
  (void)out_corrections;
  return false;
}

bool nvtop_probe_nvlink_list(struct list_head *devices) {
  (void)devices;
  return false;
}

void nvtop_reset_nvlink_cache(struct gpu_info *gpu_info) {
  (void)gpu_info;
}
