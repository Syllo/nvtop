/*
 * Copyright (C) 2023 klayer <klayer@163.com>
 *
 * This file is part of Nvtop and adapted from Ascend DCMI from Huawei Technologies Co., Ltd.
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

#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "ascend/dcmi_interface_api.h"
#include "list.h"
#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"

#define DCMI_SUCCESS 0
#define MAX_PROC_NUM 64
#define PROC_ALLOC_INC 16
#define ASCEND_PROF_DATA_NUM 3

/* PCIe profiling is optional and may block inside the driver. Keep its
 * sampling short and reuse the last value between samples. */
#define ASCEND_PCIE_PROFILING_TIME_MS 100
#define ASCEND_RATED_POWER_MIN_MW 150000U
#define ASCEND_RATED_POWER_MAX_MW 600000U
#ifndef ASCEND_PCIE_QUERY_INTERVAL_SEC
#define ASCEND_PCIE_QUERY_INTERVAL_SEC 5
#endif

/* CANN ships some releases with a strong declaration for an entry point and
 * others with a weak/optional export.  Converting the address to uintptr_t
 * avoids -Waddress for the strong form while still allowing a missing weak
 * export to be detected at runtime. */
#define DCMI_SYMBOL_PRESENT(symbol) ((uintptr_t)(symbol) != (uintptr_t)0)

/* Per-device state. This backend only publishes telemetry nvtop's interface
 * actually renders, so the persistent state is limited to the handful of
 * values that need caching between samples. */
struct gpu_info_ascend {
  struct gpu_info base;
  struct list_head allocate_list;
  /* The allocation is a contiguous array, but only its first element is
   * linked so shutdown can release the block exactly once. */
  unsigned allocation_count;
  int card_id;
  int device_id;
  int logical_id;
  bool logical_id_valid;
  time_t last_pcie_query;
  unsigned pcie_rx;
  unsigned pcie_tx;
  bool pcie_bandwidth_valid;
  time_t last_power_limit_query;
  unsigned power_draw_max;
  bool power_draw_max_valid;
};

static int last_dcmi_return_status = DCMI_SUCCESS;
static const char *local_error_string = "";
static LIST_HEAD(allocations);
static bool ascend_use_dcmiv2;
static bool ascend_legacy_initialized;

static void ascend_read_pcie_file(const char *pdev, const char *name, unsigned *value);

static bool gpuinfo_ascend_init(void);
static void gpuinfo_ascend_shutdown(void);
static const char *gpuinfo_ascend_last_error_string(void);
static bool gpuinfo_ascend_get_device_handles(struct list_head *devices, unsigned *count);
static void gpuinfo_ascend_populate_static_info(struct gpu_info *_gpu_info);
static void gpuinfo_ascend_refresh_dynamic_info(struct gpu_info *_gpu_info);
static void gpuinfo_ascend_get_running_processes(struct gpu_info *_gpu_info);

struct gpu_vendor gpu_vendor_ascend = {
    .init = gpuinfo_ascend_init,
    .shutdown = gpuinfo_ascend_shutdown,
    .last_error_string = gpuinfo_ascend_last_error_string,
    .get_device_handles = gpuinfo_ascend_get_device_handles,
    .populate_static_info = gpuinfo_ascend_populate_static_info,
    .refresh_dynamic_info = gpuinfo_ascend_refresh_dynamic_info,
    .refresh_running_processes = gpuinfo_ascend_get_running_processes,
    .name = "Ascend",
};

__attribute__((constructor)) static void init_extract_gpuinfo_ascend(void) { register_gpu_vendor(&gpu_vendor_ascend); }

/* ------------------------------------------------------------------ */
/* PCIe / device identifier helpers                                    */
/* ------------------------------------------------------------------ */

static bool ascend_query_pcie(const struct gpu_info_ascend *gpu, struct dcmi_pcie_info_all *pcie_info) {
  memset(pcie_info, 0, sizeof(*pcie_info));

  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_pcie_info)) {
    last_dcmi_return_status = dcmiv2_get_device_pcie_info(gpu->logical_id, pcie_info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return true;
  }

  if (gpu->card_id >= 0 && gpu->device_id >= 0 && DCMI_SYMBOL_PRESENT(dcmi_get_device_pcie_info_v2)) {
    last_dcmi_return_status = dcmi_get_device_pcie_info_v2(gpu->card_id, gpu->device_id, pcie_info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return true;
  }

  /* Older DCMI releases expose the same BDF information through v1. */
  if (gpu->card_id >= 0 && gpu->device_id >= 0 && DCMI_SYMBOL_PRESENT(dcmi_get_device_pcie_info)) {
    struct dcmi_pcie_info legacy_info;
    memset(&legacy_info, 0, sizeof(legacy_info));
    last_dcmi_return_status = dcmi_get_device_pcie_info(gpu->card_id, gpu->device_id, &legacy_info);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      pcie_info->bdf_busid = legacy_info.bdf_busid;
      pcie_info->bdf_deviceid = legacy_info.bdf_deviceid;
      pcie_info->bdf_funcid = legacy_info.bdf_funcid;
      return true;
    }
  }

  return false;
}

static void ascend_set_fallback_pdev(char *pdev, size_t pdev_size, int card_id, int device_id) {
  snprintf(pdev, pdev_size, "%d-%d", card_id, device_id);
}

static void ascend_set_device_fallback_pdev(struct gpu_info_ascend *gpu) {
  if (gpu->logical_id_valid && (gpu->card_id < 0 || gpu->device_id < 0))
    snprintf(gpu->base.pdev, sizeof(gpu->base.pdev), "logical-%d", gpu->logical_id);
  else
    ascend_set_fallback_pdev(gpu->base.pdev, sizeof(gpu->base.pdev), gpu->card_id, gpu->device_id);
}

static void ascend_set_pdev(struct gpu_info_ascend *gpu) {
  struct dcmi_pcie_info_all pcie_info;
  if (ascend_query_pcie(gpu, &pcie_info)) {
    /* PCI BDF is stable across refreshes and is more useful in config files
     * than a DCMI card/device tuple. */
    static const char hex[] = "0123456789abcdef";
    unsigned domain = (unsigned)(pcie_info.domain < 0 ? 0 : pcie_info.domain) & 0xffffu;
    unsigned bus = pcie_info.bdf_busid & 0xffu;
    unsigned device = pcie_info.bdf_deviceid & 0x1fu;
    unsigned function = pcie_info.bdf_funcid & 0x7u;
    if (sizeof(gpu->base.pdev) >= 13) {
      char *pdev = gpu->base.pdev;
      pdev[0] = hex[(domain >> 12) & 0xf];
      pdev[1] = hex[(domain >> 8) & 0xf];
      pdev[2] = hex[(domain >> 4) & 0xf];
      pdev[3] = hex[domain & 0xf];
      pdev[4] = ':';
      pdev[5] = hex[(bus >> 4) & 0xf];
      pdev[6] = hex[bus & 0xf];
      pdev[7] = ':';
      pdev[8] = hex[(device >> 4) & 0xf];
      pdev[9] = hex[device & 0xf];
      pdev[10] = '.';
      pdev[11] = hex[function];
      pdev[12] = '\0';
      return;
    }
  } else {
    ascend_set_device_fallback_pdev(gpu);
    return;
  }
  ascend_set_device_fallback_pdev(gpu);
}

/* ------------------------------------------------------------------ */
/* Generic helpers                                                     */
/* ------------------------------------------------------------------ */

static void ascend_copy_name(char *dst, size_t dst_size, const unsigned char *src, size_t src_size) {
  size_t len;
  if (!dst_size || !src)
    return;
  len = strnlen((const char *)src, src_size);
  if (len >= dst_size)
    len = dst_size - 1;
  memcpy(dst, src, len);
  dst[len] = '\0';
}

static bool ascend_name_is_empty(const unsigned char *name, size_t size) {
  return !name || strnlen((const char *)name, size) == 0;
}

static unsigned long long ascend_to_bytes(unsigned long long value, unsigned long long multiplier) {
  if (value > ULLONG_MAX / multiplier)
    return ULLONG_MAX;
  return value * multiplier;
}

static unsigned long long ascend_used_from_percent(unsigned long long total, unsigned percent) {
  unsigned long long whole;
  unsigned long long remainder;
  if (percent > 100)
    percent = 100;
  whole = total / 100;
  remainder = total % 100;
  return whole * percent + (remainder * percent) / 100;
}

static unsigned ascend_memory_percent(unsigned long long used, unsigned long long total) {
  if (total == 0)
    return 0;
  if (used >= total)
    return 100;
#if defined(__SIZEOF_INT128__)
  return (unsigned)(((__uint128_t)used * 100u) / total);
#else
  return (unsigned)((used / total) * 100u);
#endif
}

static bool ascend_percentage_is_valid(unsigned value) { return value <= 100; }

static bool ascend_has_legacy_ids(const struct gpu_info_ascend *gpu) {
  return gpu && gpu->card_id >= 0 && gpu->device_id >= 0;
}

/* ------------------------------------------------------------------ */
/* DCMI wrapper entry points, newest API first with legacy fallbacks    */
/* ------------------------------------------------------------------ */

static int ascend_get_hbm_info(const struct gpu_info_ascend *gpu, struct dcmi_hbm_info *info) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_hbm_info)) {
    last_dcmi_return_status = dcmiv2_get_device_hbm_info(gpu->logical_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (ascend_has_legacy_ids(gpu) && DCMI_SYMBOL_PRESENT(dcmi_get_device_hbm_info))
    return (last_dcmi_return_status = dcmi_get_device_hbm_info(gpu->card_id, gpu->device_id, info));
  return -1;
}

static int ascend_get_legacy_hbm_info(const struct gpu_info_ascend *gpu, struct dsmi_hbm_info_stru *info) {
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_hbm_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_hbm_info(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_memory_info_v3(const struct gpu_info_ascend *gpu, struct dcmi_get_memory_info_stru *info) {
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_memory_info_v3))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_memory_info_v3(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_memory_info_v2(const struct gpu_info_ascend *gpu, struct dcmi_memory_info *info) {
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_memory_info_v2))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_memory_info_v2(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_memory_info_v1(const struct gpu_info_ascend *gpu, struct dcmi_memory_info_stru *info) {
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_memory_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_memory_info(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_utilization(const struct gpu_info_ascend *gpu, int type, unsigned *value) {
  if (!value)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_utilization_rate)) {
    last_dcmi_return_status = dcmiv2_get_device_utilization_rate(gpu->logical_id, type, value);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_utilization_rate))
    return -1;
  last_dcmi_return_status = dcmi_get_device_utilization_rate(gpu->card_id, gpu->device_id, type, value);
  return last_dcmi_return_status;
}

static int ascend_get_frequency(const struct gpu_info_ascend *gpu, enum dcmi_freq_type type, unsigned *frequency) {
  if (!frequency)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_frequency)) {
    last_dcmi_return_status = dcmiv2_get_device_frequency(gpu->logical_id, type, frequency);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_frequency))
    return -1;
  last_dcmi_return_status = dcmi_get_device_frequency(gpu->card_id, gpu->device_id, type, frequency);
  return last_dcmi_return_status;
}

static int ascend_get_aicore_info(const struct gpu_info_ascend *gpu, struct dcmi_aicore_info *info) {
  int status;
  if (!gpu || !info)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_aicore_info)) {
    status = dcmiv2_get_device_aicore_info(gpu->logical_id, info);
    last_dcmi_return_status = status;
    if (status == DCMI_SUCCESS)
      return status;
  }
  if (!ascend_has_legacy_ids(gpu))
    return -1;
  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_aicore_info)) {
    status = dcmi_get_device_aicore_info(gpu->card_id, gpu->device_id, info);
    last_dcmi_return_status = status;
    if (status == DCMI_SUCCESS)
      return status;
  }
  if (!DCMI_SYMBOL_PRESENT(dcmi_get_aicore_info))
    return -1;

  struct dsmi_aicore_info_stru legacy_info;
  memset(&legacy_info, 0, sizeof(legacy_info));
  status = dcmi_get_aicore_info(gpu->card_id, gpu->device_id, &legacy_info);
  last_dcmi_return_status = status;
  if (status == DCMI_SUCCESS) {
    info->freq = legacy_info.freq;
    info->cur_freq = legacy_info.curfreq;
  }
  return status;
}

static int ascend_get_multi_utilization(const struct gpu_info_ascend *gpu, struct dcmi_multi_utilization_info *info) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_multi_utilization_rate)) {
    last_dcmi_return_status = dcmiv2_get_device_multi_utilization_rate(gpu->logical_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_multi_utilization_rate))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_multi_utilization_rate(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_ecc_info(const struct gpu_info_ascend *gpu, enum dcmi_device_type type,
                               struct dcmi_ecc_info *info) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_ecc_info)) {
    last_dcmi_return_status = dcmiv2_get_device_ecc_info(gpu->logical_id, type, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_ecc_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_ecc_info(gpu->card_id, gpu->device_id, type, info));
}

static int ascend_get_proc_mem_info(const struct gpu_info_ascend *gpu, struct dcmi_proc_mem_info *info, int *count) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_proc_mem_info)) {
    last_dcmi_return_status = dcmiv2_get_device_proc_mem_info(gpu->logical_id, info, count);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_resource_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_resource_info(gpu->card_id, gpu->device_id, info, count));
}

static int ascend_get_chip_info_v2(const struct gpu_info_ascend *gpu, struct dcmi_chip_info_v2 *info) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_chip_info)) {
    last_dcmi_return_status = dcmiv2_get_device_chip_info(gpu->logical_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_chip_info_v2))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_chip_info_v2(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_chip_info(const struct gpu_info_ascend *gpu, struct dcmi_chip_info *info) {
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_chip_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_chip_info(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_sensor_info(const struct gpu_info_ascend *gpu, enum dcmi_manager_sensor_id sensor_id,
                                  union dcmi_sensor_info *info) {
  if (!info)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_sensor_info)) {
    last_dcmi_return_status = dcmiv2_get_device_sensor_info(gpu->logical_id, sensor_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_sensor_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_sensor_info(gpu->card_id, gpu->device_id, sensor_id, info));
}

static int ascend_get_device_info(const struct gpu_info_ascend *gpu, enum dcmi_main_cmd main_cmd, unsigned sub_cmd,
                                  void *buf, unsigned *size) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_info)) {
    last_dcmi_return_status = dcmiv2_get_device_info(gpu->logical_id, main_cmd, sub_cmd, buf, size);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_info(gpu->card_id, gpu->device_id, main_cmd, sub_cmd, buf, size));
}

static int ascend_get_power(const struct gpu_info_ascend *gpu, int *power) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_power_info)) {
    last_dcmi_return_status = dcmiv2_get_device_power_info(gpu->logical_id, power);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_power_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_power_info(gpu->card_id, gpu->device_id, power));
}

static int ascend_get_temperature(const struct gpu_info_ascend *gpu, int *temperature) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_temperature)) {
    last_dcmi_return_status = dcmiv2_get_device_temperature(gpu->logical_id, temperature);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_temperature))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_temperature(gpu->card_id, gpu->device_id, temperature));
}

/* ------------------------------------------------------------------ */
/* Memory and static metadata                                          */
/* ------------------------------------------------------------------ */

enum ascend_memory_source {
  ASCEND_MEMORY_NONE,
  ASCEND_MEMORY_HBM,
  ASCEND_MEMORY_DDR_MB,
};

struct ascend_memory_sample {
  enum ascend_memory_source source;
  unsigned long long total;
  unsigned long long used;
  unsigned long long available;
  unsigned long long unit_multiplier; /* source unit -> bytes */
  unsigned frequency;
  unsigned utilization;
  bool has_used;
  bool has_available;
  bool utilization_valid;
};

static bool ascend_query_memory(const struct gpu_info_ascend *gpu, struct ascend_memory_sample *sample) {
  memset(sample, 0, sizeof(*sample));

  if ((ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_hbm_info)) ||
      ascend_has_legacy_ids(gpu)) {
    struct dcmi_hbm_info hbm_info;
    memset(&hbm_info, 0, sizeof(hbm_info));
    last_dcmi_return_status = ascend_get_hbm_info(gpu, &hbm_info);
    if (last_dcmi_return_status == DCMI_SUCCESS && hbm_info.memory_size > 0) {
      sample->source = ASCEND_MEMORY_HBM;
      sample->unit_multiplier = 1024ULL * 1024ULL; /* dcmi_hbm_info sizes are reported in MB */
      sample->total = hbm_info.memory_size;
      sample->used = hbm_info.memory_usage > hbm_info.memory_size ? hbm_info.memory_size : hbm_info.memory_usage;
      sample->has_used = true;
      sample->frequency = hbm_info.freq;
      sample->utilization = ascend_memory_percent(sample->used, sample->total);
      sample->utilization_valid = true;
      return true;
    }
  }

  if (ascend_has_legacy_ids(gpu)) {
    struct dsmi_hbm_info_stru hbm_info;
    memset(&hbm_info, 0, sizeof(hbm_info));
    last_dcmi_return_status = ascend_get_legacy_hbm_info(gpu, &hbm_info);
    if (last_dcmi_return_status == DCMI_SUCCESS && hbm_info.memory_size > 0) {
      sample->source = ASCEND_MEMORY_HBM;
      sample->unit_multiplier = 1024ULL; /* dsmi_hbm_info_stru sizes are reported in KB */
      sample->total = hbm_info.memory_size;
      sample->used = hbm_info.memory_usage > hbm_info.memory_size ? hbm_info.memory_size : hbm_info.memory_usage;
      sample->has_used = true;
      sample->frequency = hbm_info.freq;
      sample->utilization = ascend_memory_percent(sample->used, sample->total);
      sample->utilization_valid = true;
      return true;
    }
  }

  /* V3 exposes the exact available DDR amount, including huge pages. Prefer
   * it over reconstructing used memory from a rounded percentage. */
  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_memory_info_v3)) {
    struct dcmi_get_memory_info_stru memory_info;
    memset(&memory_info, 0, sizeof(memory_info));
    last_dcmi_return_status = ascend_get_memory_info_v3(gpu, &memory_info);
    if (last_dcmi_return_status == DCMI_SUCCESS && memory_info.memory_size > 0) {
      sample->source = ASCEND_MEMORY_DDR_MB;
      sample->unit_multiplier = 1024ULL * 1024ULL;
      sample->total = memory_info.memory_size;
      bool available_valid =
          memory_info.memory_available <= memory_info.memory_size && ascend_percentage_is_valid(memory_info.utiliza);
      if (available_valid) {
        sample->available = memory_info.memory_available;
        sample->used = memory_info.memory_size - sample->available;
        sample->has_used = true;
        sample->has_available = true;
      }
      sample->frequency = memory_info.freq;
      sample->utilization = memory_info.utiliza;
      sample->utilization_valid = ascend_percentage_is_valid(memory_info.utiliza);
      return true;
    }
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_memory_info_v2)) {
    struct dcmi_memory_info memory_info;
    memset(&memory_info, 0, sizeof(memory_info));
    last_dcmi_return_status = ascend_get_memory_info_v2(gpu, &memory_info);
    if (last_dcmi_return_status == DCMI_SUCCESS && memory_info.memory_size > 0) {
      sample->source = ASCEND_MEMORY_DDR_MB;
      sample->unit_multiplier = 1024ULL * 1024ULL;
      sample->total = memory_info.memory_size;
      sample->frequency = memory_info.freq;
      sample->utilization = memory_info.utiliza;
      sample->utilization_valid = ascend_percentage_is_valid(memory_info.utiliza);
      if (sample->utilization_valid) {
        sample->used = ascend_used_from_percent(memory_info.memory_size, memory_info.utiliza);
        sample->available = sample->total - sample->used;
        sample->has_used = true;
        sample->has_available = true;
      }
      return true;
    }
  }

  /* The v1 API is still present on 310P3/300i driver combinations. Its
   * memory_size is documented in MB. */
  if (DCMI_SYMBOL_PRESENT(dcmi_get_memory_info)) {
    struct dcmi_memory_info_stru memory_info;
    memset(&memory_info, 0, sizeof(memory_info));
    last_dcmi_return_status = ascend_get_memory_info_v1(gpu, &memory_info);
    if (last_dcmi_return_status == DCMI_SUCCESS && memory_info.memory_size > 0) {
      sample->source = ASCEND_MEMORY_DDR_MB;
      sample->unit_multiplier = 1024ULL * 1024ULL;
      sample->total = memory_info.memory_size;
      sample->frequency = memory_info.freq;
      sample->utilization = memory_info.utiliza;
      sample->utilization_valid = ascend_percentage_is_valid(memory_info.utiliza);
      if (sample->utilization_valid) {
        sample->used = ascend_used_from_percent(memory_info.memory_size, memory_info.utiliza);
        sample->available = sample->total - sample->used;
        sample->has_used = true;
        sample->has_available = true;
      }
      return true;
    }
  }

  sample->source = ASCEND_MEMORY_NONE;
  return false;
}

static void ascend_set_memory_type(struct gpuinfo_static_info *static_info, const struct gpu_info_ascend *gpu) {
  struct ascend_memory_sample sample;
  if (!ascend_query_memory(gpu, &sample))
    return;

  if (sample.source == ASCEND_MEMORY_HBM)
    snprintf(static_info->memory_type, sizeof(static_info->memory_type), "HBM");
  else if (sample.source == ASCEND_MEMORY_DDR_MB)
    snprintf(static_info->memory_type, sizeof(static_info->memory_type), "DDR");
  else
    return;
  SET_VALID(gpuinfo_memory_type_valid, static_info->valid);
}

static void ascend_set_aicore_count(struct gpuinfo_static_info *static_info, unsigned count) {
  if (!count)
    return;
  /* AICore is the closest common nvtop equivalent to a shader core. Keep
   * engine_count unset: it is used for DRM process-cycle calculations, which
   * DCMI does not expose for these processes. */
  SET_GPUINFO_STATIC(static_info, n_shared_cores, count);
}

static bool ascend_query_utilization(const struct gpu_info_ascend *gpu, int type, unsigned *value) {
  unsigned utilization = 0;
  if (!value)
    return false;
  last_dcmi_return_status = ascend_get_utilization(gpu, type, &utilization);
  if (last_dcmi_return_status != DCMI_SUCCESS || !ascend_percentage_is_valid(utilization))
    return false;
  *value = utilization;
  return true;
}

/* ------------------------------------------------------------------ */
/* Dynamic telemetry                                                   */
/* ------------------------------------------------------------------ */

static void ascend_read_pcie_file(const char *pdev, const char *name, unsigned *value) {
  char path[PATH_MAX];
  FILE *file;
  if (!pdev || !pdev[0] || strchr(pdev, ':') == NULL || !name || !value)
    return;
  snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/%s", pdev, name);
  file = fopen(path, "r");
  if (!file)
    return;
  if (fscanf(file, "%u", value) != 1)
    *value = 0;
  fclose(file);
}

static void ascend_refresh_pcie_link(struct gpu_info *gpu_info, struct gpuinfo_dynamic_info *dynamic_info) {
  unsigned link_width = 0;
  unsigned link_speed = 0;
  ascend_read_pcie_file(gpu_info->pdev, "current_link_width", &link_width);
  ascend_read_pcie_file(gpu_info->pdev, "current_link_speed", &link_speed);
  if (link_width > 0)
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_width, link_width);
  if (link_speed > 0) {
    unsigned generation = nvtop_pcie_gen_from_link_speed(link_speed);
    if (generation > 0)
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_gen, generation);
  }
}

static unsigned ascend_bandwidth_to_kb_per_second(const unsigned int first[ASCEND_PROF_DATA_NUM],
                                                  const unsigned int second[ASCEND_PROF_DATA_NUM],
                                                  const unsigned int third[ASCEND_PROF_DATA_NUM]) {
  /* The DCMI arrays are [min, max, average], and report MiB/ms after the
   * driver's bytes/us -> MiB/ms conversion. Sum the average components
   * before converting to nvtop's KiB/s unit so rounding does not accumulate
   * across PCIe traffic classes. */
  uint64_t average =
      (uint64_t)first[ASCEND_PROF_DATA_NUM - 1] + second[ASCEND_PROF_DATA_NUM - 1] + third[ASCEND_PROF_DATA_NUM - 1];
  if (average > UINT64_MAX / UINT64_C(1024000))
    return UINT_MAX;
  average *= UINT64_C(1024000); /* 1024 KiB/MiB * 1000 ms/s */
  return average > UINT_MAX ? UINT_MAX : (unsigned)average;
}

static void ascend_refresh_pcie_bandwidth_cache(struct gpu_info_ascend *gpu_info) {
  time_t now;
  bool should_query;

  if (!DCMI_SYMBOL_PRESENT(dcmi_get_pcie_link_bandwidth_info) &&
      !(ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_pcie_link_bandwidth_info)))
    return;

  now = time(NULL);
  should_query = gpu_info->last_pcie_query == (time_t)0 || now < gpu_info->last_pcie_query ||
                 (unsigned long long)(now - gpu_info->last_pcie_query) >= ASCEND_PCIE_QUERY_INTERVAL_SEC;
  if (should_query) {
    gpu_info->pcie_bandwidth_valid = false;
    struct dcmi_pcie_link_bandwidth_info bandwidth;
    memset(&bandwidth, 0, sizeof(bandwidth));
    bandwidth.profiling_time = ASCEND_PCIE_PROFILING_TIME_MS;
    if (ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_pcie_link_bandwidth_info)) {
      last_dcmi_return_status = dcmiv2_get_pcie_link_bandwidth_info(gpu_info->logical_id, &bandwidth);
    } else if (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_pcie_link_bandwidth_info)) {
      last_dcmi_return_status = dcmi_get_pcie_link_bandwidth_info(gpu_info->card_id, gpu_info->device_id, &bandwidth);
    } else {
      return;
    }
    gpu_info->last_pcie_query = now;
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      gpu_info->pcie_tx = ascend_bandwidth_to_kb_per_second(bandwidth.tx_p_bw, bandwidth.tx_np_bw, bandwidth.tx_cpl_bw);
      gpu_info->pcie_rx = ascend_bandwidth_to_kb_per_second(bandwidth.rx_p_bw, bandwidth.rx_np_bw, bandwidth.rx_cpl_bw);
      gpu_info->pcie_bandwidth_valid = true;
    }
  }
}

static void ascend_refresh_fan(struct gpu_info_ascend *gpu_info, struct gpuinfo_dynamic_info *dynamic_info) {
  int fan_count = 0;
  int speed = 0;

  if (!ascend_has_legacy_ids(gpu_info) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_fan_count) ||
      !DCMI_SYMBOL_PRESENT(dcmi_get_device_fan_speed) ||
      dcmi_get_device_fan_count(gpu_info->card_id, gpu_info->device_id, &fan_count) != DCMI_SUCCESS)
    return;
  if (fan_count < 1)
    return;
  /* fan_id == 0 is the driver's average across all fans; individual fan
   * IDs are one-based according to the DCMI contract. */
  if (dcmi_get_device_fan_speed(gpu_info->card_id, gpu_info->device_id, 0, &speed) == DCMI_SUCCESS && speed >= 0)
    SET_GPUINFO_DYNAMIC(dynamic_info, fan_rpm, (unsigned)speed);
}

static void ascend_refresh_dvpp(struct gpu_info_ascend *gpu_info, struct gpuinfo_dynamic_info *dynamic_info) {
  struct dcmi_dvpp_ratio usage;
  if (!ascend_has_legacy_ids(gpu_info) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_dvpp_ratio_info))
    return;
  memset(&usage, 0, sizeof(usage));
  if (dcmi_get_device_dvpp_ratio_info(gpu_info->card_id, gpu_info->device_id, &usage) != DCMI_SUCCESS)
    return;

  if (usage.venc_ratio >= 0) {
    unsigned value = (unsigned)usage.venc_ratio;
    if (ascend_percentage_is_valid(value))
      SET_GPUINFO_DYNAMIC(dynamic_info, encoder_rate, value);
  }
  if (usage.vdec_ratio >= 0) {
    unsigned value = (unsigned)usage.vdec_ratio;
    if (ascend_percentage_is_valid(value))
      SET_GPUINFO_DYNAMIC(dynamic_info, decoder_rate, value);
  }
}

static bool ascend_query_power_limit(const struct gpu_info_ascend *gpu, unsigned *power_limit) {
  struct dcmi_lp_power_info power_info;
  unsigned info_size = sizeof(power_info);
  if (!power_limit || (!DCMI_SYMBOL_PRESENT(dcmi_get_device_info) &&
                       !(ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_info))))
    return false;
  memset(&power_info, 0, sizeof(power_info));
  if (ascend_get_device_info(gpu, DCMI_MAIN_CMD_LP, DCMI_LP_SUB_CMD_GET_POWER_INFO, &power_info, &info_size) !=
          DCMI_SUCCESS ||
      info_size < sizeof(power_info.soc_rated_power) || power_info.soc_rated_power < ASCEND_RATED_POWER_MIN_MW ||
      power_info.soc_rated_power > ASCEND_RATED_POWER_MAX_MW)
    return false;
  *power_limit = power_info.soc_rated_power;
  return true;
}

/* Aggregate HBM and DDR ECC counters into nvtop's generic corrected /
 * uncorrected pair, which is the only ECC representation the interface has. */
static void ascend_refresh_ecc(struct gpu_info_ascend *gpu_info, struct gpuinfo_dynamic_info *dynamic_info) {
  static const enum dcmi_device_type types[] = {DCMI_DEVICE_TYPE_HBM, DCMI_DEVICE_TYPE_DDR};
  unsigned long long corrected = 0;
  unsigned long long uncorrected = 0;
  bool have_ecc = false;

  if (!DCMI_SYMBOL_PRESENT(dcmi_get_device_ecc_info) &&
      !(ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_ecc_info)))
    return;

  for (size_t i = 0; i < sizeof(types) / sizeof(types[0]); ++i) {
    struct dcmi_ecc_info ecc_info;
    memset(&ecc_info, 0, sizeof(ecc_info));
    if (ascend_get_ecc_info(gpu_info, types[i], &ecc_info) != DCMI_SUCCESS)
      continue;
    corrected += ecc_info.total_single_bit_error_cnt;
    uncorrected += ecc_info.total_double_bit_error_cnt;
    have_ecc = true;
  }
  if (have_ecc) {
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_corrected, corrected);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_uncorrected, uncorrected);
  }
}

/* ------------------------------------------------------------------ */
/* Vendor callbacks                                                    */
/* ------------------------------------------------------------------ */

static bool gpuinfo_ascend_init(void) {
  local_error_string = "";
  ascend_use_dcmiv2 = false;
  ascend_legacy_initialized = false;
  if (DCMI_SYMBOL_PRESENT(dcmiv2_init)) {
    last_dcmi_return_status = dcmiv2_init();
    ascend_use_dcmiv2 = last_dcmi_return_status == DCMI_SUCCESS;
  }
  if (!ascend_use_dcmiv2) {
    last_dcmi_return_status = dcmi_init();
    ascend_legacy_initialized = last_dcmi_return_status == DCMI_SUCCESS;
  }
  return last_dcmi_return_status == DCMI_SUCCESS;
}

static void gpuinfo_ascend_shutdown(void) {
  local_error_string = "";
  struct gpu_info_ascend *allocated, *tmp;
  list_for_each_entry_safe(allocated, tmp, &allocations, allocate_list) {
    list_del(&allocated->allocate_list);
    free(allocated);
  }
  ascend_use_dcmiv2 = false;
  ascend_legacy_initialized = false;
}

static const char *gpuinfo_ascend_last_error_string(void) { return local_error_string; }

static bool gpuinfo_ascend_get_device_handles(struct list_head *devices, unsigned *count) {
  int num_cards = 0;
  int card_list[MAX_CARD_NUM] = {0};
  int card_device_list[MAX_CARD_NUM] = {0};
  int logical_device_list[MAX_CARD_NUM] = {0};
  size_t num_devices = 0;
  bool use_dcmiv2 = false;

  if (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_list)) {
    int logical_count = 0;
    last_dcmi_return_status = dcmiv2_get_device_list(logical_device_list, &logical_count, MAX_CARD_NUM);
    if (last_dcmi_return_status == DCMI_SUCCESS && logical_count > 0 && logical_count <= MAX_CARD_NUM) {
      int accepted_count = 0;
      for (int logical_index = 0; logical_index < logical_count; ++logical_index) {
        bool keep_device = true;
        if (DCMI_SYMBOL_PRESENT(dcmiv2_get_device_type)) {
          enum dcmi_unit_type device_type = INVALID_TYPE;
          int type_status = dcmiv2_get_device_type(logical_device_list[logical_index], &device_type);
          /* A logical-device list may include MCU/CPU management devices on
           * newer products. nvtop monitors NPU devices only; preserve an
           * entry when type probing itself is unavailable. */
          if (type_status == DCMI_SUCCESS && device_type != NPU_TYPE)
            keep_device = false;
        }
        if (keep_device)
          logical_device_list[accepted_count++] = logical_device_list[logical_index];
      }
      num_devices = (size_t)accepted_count;
      use_dcmiv2 = accepted_count > 0;
      if (!use_dcmiv2)
        ascend_use_dcmiv2 = false;
    } else {
      /* dcmiv2_init is only implemented for some newer products. A library
       * may export it yet report NOT_SUPPORT for the logical-device list;
       * keep the legacy enumeration path available in that case. */
      ascend_use_dcmiv2 = false;
    }
  }

  if (!use_dcmiv2) {
    if (!ascend_legacy_initialized) {
      last_dcmi_return_status = dcmi_init();
      if (last_dcmi_return_status != DCMI_SUCCESS) {
        local_error_string = "Failed to initialize legacy DCMI";
        return false;
      }
      ascend_legacy_initialized = true;
    }
    last_dcmi_return_status = dcmi_get_card_list(&num_cards, card_list, MAX_CARD_NUM);
    if (last_dcmi_return_status != DCMI_SUCCESS || num_cards <= 0 || num_cards > MAX_CARD_NUM) {
      local_error_string = "Failed to get card list";
      return false;
    }

    for (int card_index = 0; card_index < num_cards; ++card_index) {
      int device_count = 0;
      last_dcmi_return_status = dcmi_get_device_num_in_card(card_list[card_index], &device_count);
      if (last_dcmi_return_status != DCMI_SUCCESS || device_count < 0 || device_count > MAX_CARD_NUM) {
        local_error_string = "Failed to get device num of card";
        return false;
      }
      card_device_list[card_index] = device_count;
      num_devices += (size_t)device_count;
    }
  }

  if (num_devices == 0 || num_devices > UINT_MAX) {
    local_error_string = "Not found NPU(s)";
    return false;
  }

  struct gpu_info_ascend *gpu_infos = calloc(num_devices, sizeof(*gpu_infos));
  if (!gpu_infos) {
    local_error_string = strerror(errno);
    return false;
  }

  *count = 0;
  if (use_dcmiv2) {
    for (int logical_index = 0; logical_index < (int)num_devices; ++logical_index) {
      struct gpu_info_ascend *gpu_info = &gpu_infos[*count];
      gpu_info->base.vendor = &gpu_vendor_ascend;
      gpu_info->card_id = -1;
      gpu_info->device_id = -1;
      gpu_info->logical_id = logical_device_list[logical_index];
      gpu_info->logical_id_valid = true;
      if (DCMI_SYMBOL_PRESENT(dcmi_get_card_id_device_id_from_logicid)) {
        int card_id = -1;
        int device_id = -1;
        last_dcmi_return_status =
            dcmi_get_card_id_device_id_from_logicid(&card_id, &device_id, (unsigned int)gpu_info->logical_id);
        if (last_dcmi_return_status == DCMI_SUCCESS) {
          gpu_info->card_id = card_id;
          gpu_info->device_id = device_id;
        }
      }
      ascend_set_pdev(gpu_info);
      list_add_tail(&gpu_info->base.list, devices);
      *count += 1;
    }
  } else {
    for (int card_index = 0; card_index < num_cards; ++card_index) {
      for (int device_index = 0; device_index < card_device_list[card_index]; ++device_index) {
        if (DCMI_SYMBOL_PRESENT(dcmi_get_device_type)) {
          enum dcmi_unit_type device_type = INVALID_TYPE;
          int type_status = dcmi_get_device_type(card_list[card_index], device_index, &device_type);
          /* Some legacy releases report MCU/CPU management units in the
           * per-card count.  Keep unknown type probes compatible, but never
           * expose a unit explicitly identified as non-NPU. */
          if (type_status == DCMI_SUCCESS && device_type != NPU_TYPE)
            continue;
        }
        struct gpu_info_ascend *gpu_info = &gpu_infos[*count];
        gpu_info->base.vendor = &gpu_vendor_ascend;
        gpu_info->card_id = card_list[card_index];
        gpu_info->device_id = device_index;
        gpu_info->logical_id = -1;
        gpu_info->logical_id_valid = false;
        ascend_set_pdev(gpu_info);
        list_add_tail(&gpu_info->base.list, devices);
        *count += 1;
      }
    }
  }
  if (*count == 0) {
    free(gpu_infos);
    local_error_string = "Not found NPU(s)";
    return false;
  }
  gpu_infos[0].allocation_count = *count;
  list_add(&gpu_infos[0].allocate_list, &allocations);
  return true;
}

static void gpuinfo_ascend_populate_static_info(struct gpu_info *_gpu_info) {
  struct gpu_info_ascend *gpu_info = container_of(_gpu_info, struct gpu_info_ascend, base);
  struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
  bool got_chip = false;

  memset(static_info, 0, sizeof(*static_info));
  static_info->integrated_graphics = false;
  /* DCMI reports VENC and VDEC as independent DVPP engines. */
  static_info->encode_decode_shared = false;

  if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_chip_info)) ||
      DCMI_SYMBOL_PRESENT(dcmi_get_device_chip_info_v2)) {
    struct dcmi_chip_info_v2 chip_info;
    memset(&chip_info, 0, sizeof(chip_info));
    last_dcmi_return_status = ascend_get_chip_info_v2(gpu_info, &chip_info);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      const unsigned char *name =
          ascend_name_is_empty(chip_info.chip_name, MAX_CHIP_NAME_LEN) ? chip_info.npu_name : chip_info.chip_name;
      bool has_name = !ascend_name_is_empty(name, MAX_CHIP_NAME_LEN);
      if (has_name) {
        ascend_copy_name(static_info->device_name, sizeof(static_info->device_name), name, MAX_CHIP_NAME_LEN);
        SET_VALID(gpuinfo_device_name_valid, static_info->valid);
      }
      ascend_set_aicore_count(static_info, chip_info.aicore_cnt);
      got_chip = has_name || chip_info.aicore_cnt > 0;
    }
  }

  if (!got_chip) {
    struct dcmi_chip_info chip_info;
    memset(&chip_info, 0, sizeof(chip_info));
    last_dcmi_return_status = ascend_get_chip_info(gpu_info, &chip_info);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      if (!ascend_name_is_empty(chip_info.chip_name, MAX_CHIP_NAME_LEN)) {
        ascend_copy_name(static_info->device_name, sizeof(static_info->device_name), chip_info.chip_name,
                         MAX_CHIP_NAME_LEN);
        SET_VALID(gpuinfo_device_name_valid, static_info->valid);
      }
      ascend_set_aicore_count(static_info, chip_info.aicore_cnt);
    }
  }

  ascend_set_memory_type(static_info, gpu_info);

  unsigned max_link_width = 0;
  unsigned max_link_speed = 0;
  ascend_read_pcie_file(gpu_info->base.pdev, "max_link_width", &max_link_width);
  ascend_read_pcie_file(gpu_info->base.pdev, "max_link_speed", &max_link_speed);
  if (max_link_width > 0)
    SET_GPUINFO_STATIC(static_info, max_pcie_link_width, max_link_width);
  if (max_link_speed > 0) {
    unsigned max_link_gen = nvtop_pcie_gen_from_link_speed(max_link_speed);
    if (max_link_gen > 0)
      SET_GPUINFO_STATIC(static_info, max_pcie_gen, max_link_gen);
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_sensor_info) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_sensor_info))) {
    union dcmi_sensor_info threshold;
    memset(&threshold, 0, sizeof(threshold));
    if (ascend_get_sensor_info(gpu_info, DCMI_THERMAL_THRESHOLD_ID, &threshold) == DCMI_SUCCESS) {
      if ((unsigned char)threshold.temp[0] > 0 && (unsigned char)threshold.temp[0] <= 200)
        SET_GPUINFO_STATIC(static_info, temperature_slowdown_threshold, (unsigned char)threshold.temp[0]);
      if ((unsigned char)threshold.temp[1] > 0 && (unsigned char)threshold.temp[1] <= 200)
        SET_GPUINFO_STATIC(static_info, temperature_shutdown_threshold, (unsigned char)threshold.temp[1]);
    }
  }
}

static void gpuinfo_ascend_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_ascend *gpu_info = container_of(_gpu_info, struct gpu_info_ascend, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  struct ascend_memory_sample memory_sample;

  memset(dynamic_info, 0, sizeof(*dynamic_info));

  /* AI Core clock, falling back to the generic frequency query. */
  bool got_aicore_current = false;
  bool got_aicore_max = false;
  if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_aicore_info)) ||
      (ascend_has_legacy_ids(gpu_info) &&
       (DCMI_SYMBOL_PRESENT(dcmi_get_device_aicore_info) || DCMI_SYMBOL_PRESENT(dcmi_get_aicore_info)))) {
    struct dcmi_aicore_info aicore_info;
    memset(&aicore_info, 0, sizeof(aicore_info));
    last_dcmi_return_status = ascend_get_aicore_info(gpu_info, &aicore_info);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      if (aicore_info.cur_freq > 0)
        SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, aicore_info.cur_freq);
      if (aicore_info.freq > 0)
        SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, aicore_info.freq);
      got_aicore_current = aicore_info.cur_freq > 0;
      got_aicore_max = aicore_info.freq > 0;
    }
  }
  if (!got_aicore_current) {
    unsigned frequency = 0;
    if (ascend_get_frequency(gpu_info, DCMI_FREQ_AICORE_CURRENT_, &frequency) == DCMI_SUCCESS)
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, frequency);
  }
  if (!got_aicore_max) {
    unsigned frequency = 0;
    if (ascend_get_frequency(gpu_info, DCMI_FREQ_AICORE_MAX, &frequency) == DCMI_SUCCESS)
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, frequency);
  }

  /* HBM or DDR memory: total/used/free plus the memory clock. */
  if (ascend_query_memory(gpu_info, &memory_sample)) {
    unsigned long long total_bytes = ascend_to_bytes(memory_sample.total, memory_sample.unit_multiplier);
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, total_bytes);
    if (memory_sample.has_used) {
      unsigned long long used_bytes = ascend_to_bytes(memory_sample.used, memory_sample.unit_multiplier);
      if (used_bytes > total_bytes)
        used_bytes = total_bytes;
      SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, used_bytes);
      if (memory_sample.has_available) {
        unsigned long long free_bytes = ascend_to_bytes(memory_sample.available, memory_sample.unit_multiplier);
        if (free_bytes > total_bytes)
          free_bytes = total_bytes;
        SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, free_bytes);
      } else {
        SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, total_bytes - used_bytes);
      }
    }
    if (memory_sample.utilization_valid)
      SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, memory_sample.utilization);
    if (memory_sample.frequency > 0)
      SET_GPUINFO_DYNAMIC(dynamic_info, mem_clock_speed, memory_sample.frequency);
    unsigned memory_max_frequency = 0;
    enum dcmi_freq_type memory_frequency_type =
        memory_sample.source == ASCEND_MEMORY_HBM ? DCMI_FREQ_HBM : DCMI_FREQ_DDR;
    if (ascend_get_frequency(gpu_info, memory_frequency_type, &memory_max_frequency) == DCMI_SUCCESS &&
        memory_max_frequency > 0)
      SET_GPUINFO_DYNAMIC(dynamic_info, mem_clock_speed_max, memory_max_frequency);
  }

  /* Single utilization figure for the interface: prefer the driver's
   * aggregate NPU rate, then the AI Core rate, then the combined query. */
  unsigned utilization = 0;
  if (ascend_query_utilization(gpu_info, DCMI_UTILIZATION_RATE_NPU, &utilization) ||
      ascend_query_utilization(gpu_info, DCMI_UTILIZATION_RATE_AICORE, &utilization)) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, utilization);
  } else if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_multi_utilization_rate)) ||
             DCMI_SYMBOL_PRESENT(dcmi_get_device_multi_utilization_rate)) {
    struct dcmi_multi_utilization_info multi_utilization;
    memset(&multi_utilization, 0, sizeof(multi_utilization));
    if (ascend_get_multi_utilization(gpu_info, &multi_utilization) == DCMI_SUCCESS) {
      if (ascend_percentage_is_valid(multi_utilization.npu_util))
        SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, multi_utilization.npu_util);
      else if (ascend_percentage_is_valid(multi_utilization.aicore_util))
        SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, multi_utilization.aicore_util);
    }
  }

  int temperature = 0;
  last_dcmi_return_status = ascend_get_temperature(gpu_info, &temperature);
  if (last_dcmi_return_status == DCMI_SUCCESS && temperature >= 0)
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, (unsigned)temperature);

  int power = 0;
  last_dcmi_return_status = ascend_get_power(gpu_info, &power);
  if (last_dcmi_return_status == DCMI_SUCCESS && power >= 0 && (unsigned)power <= UINT_MAX / 100)
    /* DCMI reports tenths of a watt; nvtop stores milliwatts. */
    SET_GPUINFO_DYNAMIC(dynamic_info, power_draw, (unsigned)power * 100);

  /* The rated power limit changes rarely; re-read it only periodically. */
  time_t now = time(NULL);
  if (gpu_info->last_power_limit_query == (time_t)0 || now < gpu_info->last_power_limit_query ||
      (unsigned long long)(now - gpu_info->last_power_limit_query) >= ASCEND_PCIE_QUERY_INTERVAL_SEC) {
    gpu_info->power_draw_max_valid = false;
    unsigned power_limit = 0;
    if (ascend_query_power_limit(gpu_info, &power_limit)) {
      gpu_info->power_draw_max = power_limit;
      gpu_info->power_draw_max_valid = true;
    }
    gpu_info->last_power_limit_query = now;
  }
  if (gpu_info->power_draw_max_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, gpu_info->power_draw_max);

  ascend_refresh_ecc(gpu_info, dynamic_info);
  ascend_refresh_dvpp(gpu_info, dynamic_info);
  ascend_refresh_fan(gpu_info, dynamic_info);
  ascend_refresh_pcie_link(&gpu_info->base, dynamic_info);

  ascend_refresh_pcie_bandwidth_cache(gpu_info);
  if (gpu_info->pcie_bandwidth_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_rx, gpu_info->pcie_rx);
    SET_GPUINFO_DYNAMIC(dynamic_info, pcie_tx, gpu_info->pcie_tx);
  }
}

static void gpuinfo_ascend_get_running_processes(struct gpu_info *_gpu_info) {
  struct gpu_info_ascend *gpu_info = container_of(_gpu_info, struct gpu_info_ascend, base);
  struct dcmi_proc_mem_info proc_info[MAX_PROC_NUM] = {0};
  int proc_num = 0;

  /* A failed management query must not leave the previous cycle's process
   * list looking current.  Keep the allocation for reuse, but publish an
   * empty snapshot until DCMI returns a fresh result. */
  _gpu_info->processes_count = 0;
  last_dcmi_return_status = ascend_get_proc_mem_info(gpu_info, proc_info, &proc_num);
  if (last_dcmi_return_status != DCMI_SUCCESS || proc_num < 0 || proc_num > MAX_PROC_NUM)
    return;

  unsigned new_array_size = (unsigned)proc_num + PROC_ALLOC_INC;
  struct gpu_process *new_processes = reallocarray(_gpu_info->processes, new_array_size, sizeof(*new_processes));
  if (!new_processes)
    return;
  _gpu_info->processes = new_processes;
  _gpu_info->processes_array_size = new_array_size;
  memset(_gpu_info->processes, 0, new_array_size * sizeof(*_gpu_info->processes));
  _gpu_info->processes_count = (unsigned)proc_num;

  for (int i = 0; i < proc_num; ++i) {
    _gpu_info->processes[i].type = gpu_process_compute;
    _gpu_info->processes[i].pid = proc_info[i].proc_id;
    _gpu_info->processes[i].gpu_memory_usage = proc_info[i].proc_mem_usage;
    SET_VALID(gpuinfo_process_gpu_memory_usage_valid, _gpu_info->processes[i].valid);
  }
}
