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
#include <math.h>
#include <pthread.h>
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
#define ASCEND_CPU_NUM_CONFIG_SIZE 16
#define ASCEND_PROF_DATA_NUM 3

/* PCIe profiling is optional and may block inside the driver. Keep its
 * sampling short and reuse the last value between samples. */
#define ASCEND_PCIE_PROFILING_TIME_MS 100
#define ASCEND_RATED_POWER_MIN_MW 150000U
#define ASCEND_RATED_POWER_MAX_MW 600000U
#ifndef ASCEND_PCIE_QUERY_INTERVAL_SEC
#define ASCEND_PCIE_QUERY_INTERVAL_SEC 5
#endif
#ifndef ASCEND_SLOW_QUERY_INTERVAL_SEC
#define ASCEND_SLOW_QUERY_INTERVAL_SEC 5
#endif

/* CANN ships some releases with a strong declaration for an entry point and
 * others with a weak/optional export.  Converting the address to uintptr_t
 * avoids -Waddress for the strong form while still allowing a missing weak
 * export to be detected at runtime. */
#define DCMI_SYMBOL_PRESENT(symbol) ((uintptr_t)(symbol) != (uintptr_t)0)

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
  time_t last_slow_query;
  time_t last_extended_query;
  unsigned power_draw_max;
  unsigned boot_status;
  unsigned compatibility;
  unsigned network_health;
  unsigned outband_channel_state;
  bool power_draw_max_valid;
  bool boot_status_valid;
  bool compatibility_valid;
  bool network_health_valid;
  bool outband_channel_state_valid;
  bool device_share_enabled;
  bool device_share_enabled_valid;
  bool p2p_enabled;
  bool p2p_enabled_valid;
  unsigned long long cgroup_memory_limit;
  unsigned long long cgroup_memory_usage;
  unsigned long long cgroup_memory_max_usage;
  bool cgroup_memory_limit_valid;
  bool cgroup_memory_usage_valid;
  bool cgroup_memory_max_usage_valid;
  unsigned llc_read_hit_rate;
  unsigned llc_write_hit_rate;
  unsigned llc_throughput;
  bool llc_valid;
  double hccs_tx_bandwidth;
  double hccs_rx_bandwidth;
  bool hccs_valid;
  unsigned ub_link_status;
  bool ub_link_status_valid;
  double ub_tx_bandwidth;
  double ub_rx_bandwidth;
  bool ub_bandwidth_valid;
  unsigned rdma_tx_bandwidth;
  unsigned rdma_rx_bandwidth;
  bool rdma_bandwidth_valid;
  unsigned long long network_tx_packets;
  unsigned long long network_rx_packets;
  unsigned long long network_tx_bytes;
  unsigned long long network_rx_bytes;
  unsigned long long network_tx_errors;
  unsigned long long network_rx_errors;
  unsigned long long network_rx_fcs_errors;
  bool network_stats_valid;
  unsigned device_system_time;
  bool device_system_time_valid;
  unsigned ecc_hbm_history_count;
  unsigned ecc_hbm_last_error_time;
  unsigned ecc_ddr_history_count;
  unsigned ecc_ddr_last_error_time;
  bool ecc_hbm_history_valid;
  bool ecc_ddr_history_valid;
  unsigned ub_port_id;
  unsigned long long ub_port_tx_packets;
  unsigned long long ub_port_rx_packets;
  unsigned long long ub_port_tx_errors;
  unsigned long long ub_port_rx_errors;
  unsigned long long ub_port_crc_errors;
  bool ub_port_stats_valid;
  unsigned long long network_tc_tx_packets;
  unsigned long long network_tc_rx_packets;
  bool network_tc_stats_valid;
  struct gpuinfo_fault_event fault_events[GPUINFO_MAX_FAULT_EVENTS];
  unsigned fault_event_count;
  bool fault_events_valid;
};

static void ascend_invalidate_slow_cache(struct gpu_info_ascend *gpu_info) {
  gpu_info->power_draw_max_valid = false;
  gpu_info->boot_status_valid = false;
  gpu_info->compatibility_valid = false;
  gpu_info->network_health_valid = false;
  gpu_info->outband_channel_state_valid = false;
}

static void ascend_invalidate_extended_cache(struct gpu_info_ascend *gpu_info) {
  gpu_info->device_share_enabled_valid = false;
  gpu_info->p2p_enabled_valid = false;
  gpu_info->cgroup_memory_limit_valid = false;
  gpu_info->cgroup_memory_usage_valid = false;
  gpu_info->cgroup_memory_max_usage_valid = false;
  gpu_info->llc_valid = false;
  gpu_info->hccs_valid = false;
  gpu_info->ub_link_status_valid = false;
  gpu_info->ub_bandwidth_valid = false;
  gpu_info->rdma_bandwidth_valid = false;
  gpu_info->network_stats_valid = false;
  gpu_info->device_system_time_valid = false;
  gpu_info->ecc_hbm_history_valid = false;
  gpu_info->ecc_ddr_history_valid = false;
  gpu_info->ub_port_stats_valid = false;
  gpu_info->network_tc_stats_valid = false;
  gpu_info->fault_events_valid = false;
}

static _Thread_local int last_dcmi_return_status = DCMI_SUCCESS;
static const char *local_error_string = "";
static LIST_HEAD(allocations);
static pthread_t ascend_worker_thread;
static pthread_mutex_t ascend_worker_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t ascend_worker_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t ascend_cache_mutex = PTHREAD_MUTEX_INITIALIZER;
static bool ascend_worker_started;
static bool ascend_worker_stop;
static char ascend_driver_version[MAX_VERSION_STRING];
static char ascend_dcmi_version[MAX_VERSION_STRING];
static bool ascend_driver_version_valid;
static bool ascend_dcmi_version_valid;
static bool ascend_use_dcmiv2;
static bool ascend_legacy_initialized;

static void ascend_read_pcie_file(const char *pdev, const char *name, unsigned *value);
static void ascend_refresh_slow_status(struct gpu_info_ascend *gpu_info);
static void ascend_refresh_extended_status(struct gpu_info_ascend *gpu_info);
static void ascend_refresh_pcie_bandwidth_cache(struct gpu_info_ascend *gpu_info);
static void *ascend_worker_main(void *arg);
static void ascend_start_worker(void);
static void ascend_stop_worker(void);

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

static bool ascend_voltage_is_valid(unsigned voltage) {
  /* CANN uses 0x7ffd..0x7fff as the "not available" voltage range. */
  return voltage < 0x7ffdu || voltage > 0x7fffu;
}

static bool ascend_has_legacy_ids(const struct gpu_info_ascend *gpu) {
  return gpu && gpu->card_id >= 0 && gpu->device_id >= 0;
}

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

static int ascend_get_aicpu_info(const struct gpu_info_ascend *gpu, struct dcmi_aicpu_info *info) {
  int status;
  if (!gpu || !info)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_aicpu_info)) {
    status = dcmiv2_get_device_aicpu_info(gpu->logical_id, info);
    last_dcmi_return_status = status;
    if (status == DCMI_SUCCESS)
      return status;
  }
  if (!ascend_has_legacy_ids(gpu))
    return -1;
  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_aicpu_info)) {
    status = dcmi_get_device_aicpu_info(gpu->card_id, gpu->device_id, info);
    last_dcmi_return_status = status;
    if (status == DCMI_SUCCESS)
      return status;
  }
  if (!DCMI_SYMBOL_PRESENT(dcmi_get_aicpu_info))
    return -1;

  struct dsmi_aicpu_info_stru legacy_info;
  memset(&legacy_info, 0, sizeof(legacy_info));
  status = dcmi_get_aicpu_info(gpu->card_id, gpu->device_id, &legacy_info);
  last_dcmi_return_status = status;
  if (status == DCMI_SUCCESS) {
    info->max_freq = legacy_info.maxFreq;
    info->cur_freq = legacy_info.curFreq;
    info->aicpu_num = legacy_info.aicpuNum;
    for (unsigned i = 0; i < MAX_CORE_NUM; ++i)
      info->util_rate[i] = legacy_info.utilRate[i];
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

static int ascend_get_pcie_error_info(const struct gpu_info_ascend *gpu, struct dcmi_chip_pcie_err_rate *info) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_pcie_error_cnt)) {
    last_dcmi_return_status = dcmiv2_get_device_pcie_error_cnt(gpu->logical_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_pcie_error_cnt))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_pcie_error_cnt(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_pcie_link_bandwidth(const struct gpu_info_ascend *gpu,
                                          struct dcmi_pcie_link_bandwidth_info *info) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_pcie_link_bandwidth_info)) {
    last_dcmi_return_status = dcmiv2_get_pcie_link_bandwidth_info(gpu->logical_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_pcie_link_bandwidth_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_pcie_link_bandwidth_info(gpu->card_id, gpu->device_id, info));
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

static int ascend_get_board_info(const struct gpu_info_ascend *gpu, struct dcmi_board_info *info) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_board_info)) {
    last_dcmi_return_status = dcmiv2_get_device_board_info(gpu->logical_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_board_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_board_info(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_board_id(const struct gpu_info_ascend *gpu, unsigned *board_id) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_board_id)) {
    last_dcmi_return_status = dcmiv2_get_device_board_id(gpu->logical_id, board_id);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_board_id))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_board_id(gpu->card_id, gpu->device_id, board_id));
}

static int ascend_get_mainboard_id(const struct gpu_info_ascend *gpu, unsigned *mainboard_id) {
  if (!mainboard_id)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_mainboard_id)) {
    last_dcmi_return_status = dcmiv2_get_mainboard_id(gpu->logical_id, mainboard_id);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_mainboard_id))
    return -1;
  return (last_dcmi_return_status = dcmi_get_mainboard_id(gpu->card_id, gpu->device_id, mainboard_id));
}

static int ascend_get_ub_id_info(const struct gpu_info_ascend *gpu, struct dcmi_ub_id_info *info) {
  if (!info || !ascend_use_dcmiv2 || !gpu->logical_id_valid || !DCMI_SYMBOL_PRESENT(dcmiv2_get_device_ub_id_info))
    return -1;
  return (last_dcmi_return_status = dcmiv2_get_device_ub_id_info(gpu->logical_id, info));
}

static int ascend_get_pcie_slot_id(const struct gpu_info_ascend *gpu, int *slot_id) {
  if (!slot_id)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_pcie_slot_id)) {
    last_dcmi_return_status = dcmiv2_get_device_pcie_slot_id(gpu->logical_id, slot_id);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_card_pcie_slot))
    return -1;
  return (last_dcmi_return_status = dcmi_get_card_pcie_slot(gpu->card_id, slot_id));
}

static int ascend_get_ub_slot_id(const struct gpu_info_ascend *gpu, int *slot_id) {
  if (!slot_id || !ascend_use_dcmiv2 || !gpu->logical_id_valid || !DCMI_SYMBOL_PRESENT(dcmiv2_get_device_ub_slot_id))
    return -1;
  return (last_dcmi_return_status = dcmiv2_get_device_ub_slot_id(gpu->logical_id, slot_id));
}

/* The logical API returns the physical interconnect slot and chip ID as a
 * pair.  The legacy API has only the chip-position query.  Its device_id is
 * an API selector, not the out-of-band chip ID, so do not infer chip_id from
 * it. */
static int ascend_get_chip_location(const struct gpu_info_ascend *gpu, unsigned *chip_slot, unsigned *chip_id,
                                    bool *chip_id_valid) {
  if (!gpu || !chip_slot || !chip_id || !chip_id_valid)
    return -1;
  *chip_id_valid = false;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_slot_id_and_chip_id_by_dev_id)) {
    last_dcmi_return_status = dcmiv2_get_slot_id_and_chip_id_by_dev_id(gpu->logical_id, chip_slot, chip_id);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      *chip_id_valid = true;
      return last_dcmi_return_status;
    }
  }
  if (!ascend_has_legacy_ids(gpu))
    return -1;
  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_chip_slot)) {
    int position = -1;
    last_dcmi_return_status = dcmi_get_device_chip_slot(gpu->card_id, gpu->device_id, &position);
    if (last_dcmi_return_status == DCMI_SUCCESS && position >= 0) {
      *chip_slot = (unsigned)position;
      return last_dcmi_return_status;
    }
  }
  return -1;
}

static int ascend_get_group_intra_id(const struct gpu_info_ascend *gpu, unsigned *group_intra_id) {
  if (!gpu || !group_intra_id || !ascend_use_dcmiv2 || !gpu->logical_id_valid ||
      !DCMI_SYMBOL_PRESENT(dcmiv2_get_group_intra_id_by_dev_id))
    return -1;
  return (last_dcmi_return_status = dcmiv2_get_group_intra_id_by_dev_id(gpu->logical_id, group_intra_id));
}

static int ascend_get_first_power_on_date(const struct gpu_info_ascend *gpu, unsigned *date) {
  if (!gpu || !date || !ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_first_power_on_date))
    return -1;
  return (last_dcmi_return_status = dcmi_get_first_power_on_date(gpu->card_id, date));
}

static int ascend_get_cpu_num_config(const struct gpu_info_ascend *gpu, unsigned char *config, unsigned size) {
  if (!gpu || !config || size < ASCEND_CPU_NUM_CONFIG_SIZE || !ascend_has_legacy_ids(gpu) ||
      !DCMI_SYMBOL_PRESENT(dcmi_get_device_cpu_num_config))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_cpu_num_config(gpu->card_id, gpu->device_id, config, size));
}

static int ascend_get_aicpu_count_config(const struct gpu_info_ascend *gpu, unsigned char *count) {
  if (!gpu || !count || !ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_aicpu_count_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_aicpu_count_info(gpu->card_id, gpu->device_id, count));
}

static int ascend_get_cpu_freq_mode(const struct gpu_info_ascend *gpu, int *mode) {
  if (!gpu || !mode || !ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_cpu_freq_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_cpu_freq_info(gpu->card_id, gpu->device_id, mode));
}

static int ascend_get_p2p_enable(const struct gpu_info_ascend *gpu, int *enabled) {
  int status;
  if (!gpu || !enabled || !ascend_has_legacy_ids(gpu))
    return -1;
  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_p2p_enable)) {
    status = dcmi_get_device_p2p_enable(gpu->card_id, gpu->device_id, enabled);
    last_dcmi_return_status = status;
    if (status == DCMI_SUCCESS)
      return status;
  }
  if (!DCMI_SYMBOL_PRESENT(dcmi_get_p2p_enable))
    return -1;
  return (last_dcmi_return_status = dcmi_get_p2p_enable(gpu->card_id, gpu->device_id, enabled));
}

static int ascend_get_hbm_product_info(const struct gpu_info_ascend *gpu, struct dcmi_hbm_product_info *info) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_hbm_product_info)) {
    last_dcmi_return_status = dcmiv2_get_device_hbm_product_info(gpu->logical_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_hbm_product_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_hbm_product_info(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_component_version(const struct gpu_info_ascend *gpu, enum dcmi_component_type component_type,
                                        unsigned char *version, unsigned length) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_component_static_version)) {
    last_dcmi_return_status =
        dcmiv2_get_device_component_static_version(gpu->logical_id, component_type, version, length);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_component_static_version))
    return -1;
  return (last_dcmi_return_status =
              dcmi_get_device_component_static_version(gpu->card_id, gpu->device_id, component_type, version, length));
}

static int ascend_get_component_count(const struct gpu_info_ascend *gpu, unsigned *count) {
  if (!count)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_component_cnt)) {
    last_dcmi_return_status = dcmiv2_get_device_component_cnt(gpu->logical_id, count);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_component_count))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_component_count(gpu->card_id, gpu->device_id, count));
}

static int ascend_get_component_list(const struct gpu_info_ascend *gpu, enum dcmi_component_type *components,
                                     unsigned count) {
  if (!components || count == 0)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_component_list)) {
    last_dcmi_return_status = dcmiv2_get_device_component_list(gpu->logical_id, components, count);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_component_list))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_component_list(gpu->card_id, gpu->device_id, components, count));
}

static int ascend_get_elabel_info(const struct gpu_info_ascend *gpu, struct dcmi_elabel_info *info) {
  if (!info)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_elabel_info)) {
    last_dcmi_return_status = dcmiv2_get_device_elabel_info(gpu->logical_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_elabel_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_elabel_info(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_device_share_enable(const struct gpu_info_ascend *gpu, unsigned *enabled) {
  if (!enabled)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_share_enable)) {
    unsigned int value = 0;
    last_dcmi_return_status = dcmiv2_get_device_share_enable(gpu->logical_id, &value);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      *enabled = value;
      return last_dcmi_return_status;
    }
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_share_enable))
    return -1;
  int value = 0;
  last_dcmi_return_status = dcmi_get_device_share_enable(gpu->card_id, gpu->device_id, &value);
  if (last_dcmi_return_status == DCMI_SUCCESS)
    *enabled = (unsigned)(value != 0);
  return last_dcmi_return_status;
}

static int ascend_get_cgroup_info(const struct gpu_info_ascend *gpu, struct dcmi_cgroup_info *info) {
  if (!info)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_cgroup_info)) {
    last_dcmi_return_status = dcmiv2_get_device_cgroup_info(gpu->logical_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_cgroup_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_cgroup_info(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_llc_perf(const struct gpu_info_ascend *gpu, struct dcmi_llc_perf *info) {
  if (!info)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_llc_perf_para)) {
    last_dcmi_return_status = dcmiv2_get_device_llc_perf_para(gpu->logical_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_llc_perf_para))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_llc_perf_para(gpu->card_id, gpu->device_id, info));
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

static int ascend_get_voltage(const struct gpu_info_ascend *gpu, unsigned *voltage) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_voltage)) {
    last_dcmi_return_status = dcmiv2_get_device_voltage(gpu->logical_id, voltage);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_voltage))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_voltage(gpu->card_id, gpu->device_id, voltage));
}

static int ascend_get_health(const struct gpu_info_ascend *gpu, unsigned *health) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_health)) {
    last_dcmi_return_status = dcmiv2_get_device_health(gpu->logical_id, health);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_health))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_health(gpu->card_id, gpu->device_id, health));
}

static int ascend_get_boot_status(const struct gpu_info_ascend *gpu, enum dcmi_boot_status *status) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_boot_status)) {
    last_dcmi_return_status = dcmiv2_get_device_boot_status(gpu->logical_id, status);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_boot_status))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_boot_status(gpu->card_id, gpu->device_id, status));
}

static int ascend_get_compatibility(const struct gpu_info_ascend *gpu, enum dcmi_device_compat *compatibility) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_compatibility)) {
    last_dcmi_return_status = dcmiv2_get_device_compatibility(gpu->logical_id, compatibility);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_compatibility))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_compatibility(gpu->card_id, gpu->device_id, compatibility));
}

static int ascend_get_network_health(const struct gpu_info_ascend *gpu, enum dcmi_rdfx_detect_result *result) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_network_health)) {
    last_dcmi_return_status = dcmiv2_get_device_network_health(gpu->logical_id, result);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_network_health))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_network_health(gpu->card_id, gpu->device_id, result));
}

static int ascend_get_outband_channel_state(const struct gpu_info_ascend *gpu, int *state) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_outband_channel_state)) {
    last_dcmi_return_status = dcmiv2_get_device_outband_channel_state(gpu->logical_id, state);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_outband_channel_state))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_outband_channel_state(gpu->card_id, gpu->device_id, state));
}

static int ascend_get_error_codes(const struct gpu_info_ascend *gpu, int *count, unsigned *codes, unsigned length) {
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_error_code_list)) {
    last_dcmi_return_status = dcmiv2_get_device_error_code_list(gpu->logical_id, count, codes, length);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_errorcode_v2))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_errorcode_v2(gpu->card_id, gpu->device_id, count, codes, length));
}

static int ascend_get_driver_health(unsigned *health) {
  if (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_driver_health)) {
    last_dcmi_return_status = dcmiv2_get_driver_health(health);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!DCMI_SYMBOL_PRESENT(dcmi_get_driver_health))
    return -1;
  return (last_dcmi_return_status = dcmi_get_driver_health(health));
}

static int ascend_get_driver_error_codes(int *count, unsigned *codes, unsigned length) {
  if (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_driver_error_code_list)) {
    last_dcmi_return_status = dcmiv2_get_driver_error_code_list(count, codes, length);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!DCMI_SYMBOL_PRESENT(dcmi_get_driver_errorcode))
    return -1;
  return (last_dcmi_return_status = dcmi_get_driver_errorcode(count, codes, length));
}

static int ascend_get_current_fault_events(const struct gpu_info_ascend *gpu, struct dcmi_event *events, int capacity,
                                           int *count) {
  if (!gpu || !events || capacity <= 0 || !count)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_current_fault_event)) {
    last_dcmi_return_status = dcmiv2_get_device_current_fault_event(gpu->logical_id, events, capacity, count);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_current_fault_event))
    return -1;
  return (last_dcmi_return_status =
              dcmi_get_device_current_fault_event(gpu->card_id, gpu->device_id, events, capacity, count));
}

static int ascend_get_hccs_bandwidth(const struct gpu_info_ascend *gpu, struct dcmi_hccs_bandwidth_info *info) {
  if (!gpu || !info || !ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_hccs_link_bandwidth_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_hccs_link_bandwidth_info(gpu->card_id, gpu->device_id, info));
}

static int ascend_get_ub_status(const struct gpu_info_ascend *gpu, struct dcmi_ub_port_link_status *status) {
  if (!gpu || !status)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_ub_port_link_status)) {
    last_dcmi_return_status = dcmiv2_get_ub_port_link_status(gpu->logical_id, status);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_ub_port_link_status_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_ub_port_link_status_info(gpu->card_id, gpu->device_id, status));
}

static int ascend_get_port_list(const struct gpu_info_ascend *gpu, struct dcmi_port_list_info *port_list) {
  if (!gpu || !port_list || !ascend_use_dcmiv2 || !gpu->logical_id_valid ||
      !DCMI_SYMBOL_PRESENT(dcmiv2_get_device_port_list_info))
    return -1;
  return (last_dcmi_return_status = dcmiv2_get_device_port_list_info(gpu->logical_id, port_list));
}

static int ascend_get_rdma_bandwidth(const struct gpu_info_ascend *gpu, int port_id, unsigned profiling_time,
                                     struct dcmi_network_rdma_bandwidth_info *info) {
  if (!gpu || !info)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_rdma_bandwidth_info)) {
    last_dcmi_return_status = dcmiv2_get_rdma_bandwidth_info(gpu->logical_id, port_id, profiling_time, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_rdma_bandwidth_info))
    return -1;
  return (last_dcmi_return_status =
              dcmi_get_rdma_bandwidth_info(gpu->card_id, gpu->device_id, port_id, profiling_time, info));
}

static int ascend_get_network_pkt_stats(const struct gpu_info_ascend *gpu, int port_id,
                                        struct dcmi_network_pkt_stats_info *info) {
  if (!gpu || !info)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_netdev_pkt_stats_info)) {
    last_dcmi_return_status = dcmiv2_get_netdev_pkt_stats_info(gpu->logical_id, port_id, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_netdev_pkt_stats_info))
    return -1;
  return (last_dcmi_return_status = dcmi_get_netdev_pkt_stats_info(gpu->card_id, gpu->device_id, port_id, info));
}

static int ascend_get_ub_port_stats(const struct gpu_info_ascend *gpu, const struct dcmi_ub_port_info *port,
                                    struct dcmi_port_pkt_stats_info *info) {
  if (!gpu || !port || !info || !ascend_use_dcmiv2 || !gpu->logical_id_valid ||
      !DCMI_SYMBOL_PRESENT(dcmiv2_get_port_pkt_stats_info))
    return -1;
  return (last_dcmi_return_status =
              dcmiv2_get_port_pkt_stats_info(gpu->logical_id, (struct dcmi_ub_port_info *)port, info));
}

static int ascend_get_netdev_list(const struct gpu_info_ascend *gpu, struct dcmi_netdev_list_info *info) {
  if (!gpu || !info || !ascend_use_dcmiv2 || !gpu->logical_id_valid ||
      !DCMI_SYMBOL_PRESENT(dcmiv2_get_device_netdev_list_info))
    return -1;
  return (last_dcmi_return_status = dcmiv2_get_device_netdev_list_info(gpu->logical_id, info));
}

static int ascend_get_affinity_cpu(const struct gpu_info_ascend *gpu, char *buffer, int *length) {
  if (!gpu || !buffer || !length)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_affinity_cpu_info_by_dev_id)) {
    last_dcmi_return_status = dcmiv2_get_affinity_cpu_info_by_dev_id(gpu->logical_id, buffer, length);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_affinity_cpu_info_by_device_id))
    return -1;
  return (last_dcmi_return_status =
              dcmi_get_affinity_cpu_info_by_device_id(gpu->card_id, gpu->device_id, buffer, length));
}

static int ascend_get_die_id(const struct gpu_info_ascend *gpu, enum dcmi_die_type type, struct dcmi_die_id *die_id) {
  if (!gpu || !die_id)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_die_id)) {
    last_dcmi_return_status = dcmiv2_get_device_die_id(gpu->logical_id, type, die_id);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_die_v2))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_die_v2(gpu->card_id, gpu->device_id, type, die_id));
}

static int ascend_get_flash_count(const struct gpu_info_ascend *gpu, unsigned *count) {
  if (!gpu || !count)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_flash_cnt)) {
    last_dcmi_return_status = dcmiv2_get_device_flash_cnt(gpu->logical_id, count);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_device_flash_count))
    return -1;
  return (last_dcmi_return_status = dcmi_get_device_flash_count(gpu->card_id, gpu->device_id, count));
}

static int ascend_get_flash_info(const struct gpu_info_ascend *gpu, unsigned index, struct dcmi_flash_info *info) {
  if (!gpu || !info)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_flash_info)) {
    last_dcmi_return_status = dcmiv2_get_device_flash_info(gpu->logical_id, index, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu))
    return -1;
  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_flash_info_v2)) {
    last_dcmi_return_status = dcmi_get_device_flash_info_v2(gpu->card_id, gpu->device_id, index, info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!DCMI_SYMBOL_PRESENT(dcmi_get_device_flash_info))
    return -1;
  struct dcmi_flash_info_stru legacy_info;
  memset(&legacy_info, 0, sizeof(legacy_info));
  last_dcmi_return_status = dcmi_get_device_flash_info(gpu->card_id, gpu->device_id, index, &legacy_info);
  if (last_dcmi_return_status == DCMI_SUCCESS) {
    info->flash_id = legacy_info.flash_id;
    info->device_id = legacy_info.device_id;
    info->vendor = legacy_info.vendor;
    info->state = legacy_info.state;
    info->size = legacy_info.size;
    info->sector_count = legacy_info.sector_count;
    info->manufacturer_id = legacy_info.manufacturer_id;
  }
  return last_dcmi_return_status;
}

static int ascend_get_vrd_version(const struct gpu_info_ascend *gpu, char *version, int length) {
  if (!gpu || !version || length <= 0)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_vrd_version)) {
    last_dcmi_return_status = dcmiv2_get_vrd_version(gpu->logical_id, version, length);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_vrd_version))
    return -1;
  return (last_dcmi_return_status = dcmi_get_vrd_version(gpu->card_id, version, length));
}

static int ascend_get_system_time(const struct gpu_info_ascend *gpu, unsigned *system_time) {
  int status;
  if (!gpu || !system_time)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_system_time)) {
    status = dcmiv2_get_device_system_time(gpu->logical_id, system_time);
    last_dcmi_return_status = status;
    if (status == DCMI_SUCCESS)
      return status;
  }
  if (!ascend_has_legacy_ids(gpu))
    return -1;
  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_system_time)) {
    status = dcmi_get_device_system_time(gpu->card_id, gpu->device_id, system_time);
    last_dcmi_return_status = status;
    if (status == DCMI_SUCCESS)
      return status;
  }
  if (!DCMI_SYMBOL_PRESENT(dcmi_get_system_time))
    return -1;
  return (last_dcmi_return_status = dcmi_get_system_time(gpu->card_id, gpu->device_id, system_time));
}

static int ascend_get_ecc_records(const struct gpu_info_ascend *gpu, struct dcmi_ecc_record_type type, unsigned *count,
                                  struct dcmi_ecc_common_data *records) {
  if (!gpu || !count || !records)
    return -1;
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_multi_ecc_record_info)) {
    last_dcmi_return_status = dcmiv2_get_multi_ecc_record_info(gpu->logical_id, type, count, records);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      return last_dcmi_return_status;
  }
  if (!ascend_has_legacy_ids(gpu) || !DCMI_SYMBOL_PRESENT(dcmi_get_multi_ecc_record_info_v2))
    return -1;
  return (last_dcmi_return_status =
              dcmi_get_multi_ecc_record_info_v2(gpu->card_id, gpu->device_id, type, count, records));
}

static bool ascend_query_hbm_sensor(const struct gpu_info_ascend *gpu, int *temperature);

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
  unsigned frequency;
  unsigned utilization;
  unsigned bandwidth_utilization;
  int temperature;
  bool has_used;
  bool has_available;
  bool utilization_valid;
  bool bandwidth_utilization_valid;
  bool temperature_valid;
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
      sample->total = hbm_info.memory_size;
      sample->used = hbm_info.memory_usage > hbm_info.memory_size ? hbm_info.memory_size : hbm_info.memory_usage;
      sample->has_used = true;
      sample->frequency = hbm_info.freq;
      sample->utilization = ascend_memory_percent(sample->used, sample->total);
      sample->utilization_valid = true;
      sample->bandwidth_utilization = hbm_info.bandwith_util_rate;
      sample->bandwidth_utilization_valid = ascend_percentage_is_valid(sample->bandwidth_utilization);
      sample->temperature = hbm_info.temp;
      sample->temperature_valid = hbm_info.temp >= 0;
      if (!sample->temperature_valid)
        sample->temperature_valid = ascend_query_hbm_sensor(gpu, &sample->temperature);
      return true;
    }
  }

  if (ascend_has_legacy_ids(gpu)) {
    struct dsmi_hbm_info_stru hbm_info;
    memset(&hbm_info, 0, sizeof(hbm_info));
    last_dcmi_return_status = ascend_get_legacy_hbm_info(gpu, &hbm_info);
    if (last_dcmi_return_status == DCMI_SUCCESS && hbm_info.memory_size > 0) {
      sample->source = ASCEND_MEMORY_HBM;
      sample->total = hbm_info.memory_size;
      sample->used = hbm_info.memory_usage > hbm_info.memory_size ? hbm_info.memory_size : hbm_info.memory_usage;
      sample->has_used = true;
      sample->frequency = hbm_info.freq;
      sample->utilization = ascend_memory_percent(sample->used, sample->total);
      sample->utilization_valid = true;
      sample->bandwidth_utilization = hbm_info.bandwith_util_rate;
      sample->bandwidth_utilization_valid = ascend_percentage_is_valid(sample->bandwidth_utilization);
      sample->temperature = hbm_info.temp;
      sample->temperature_valid = hbm_info.temp >= 0;
      if (!sample->temperature_valid)
        sample->temperature_valid = ascend_query_hbm_sensor(gpu, &sample->temperature);
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

static void ascend_set_static_string(char *dst, size_t dst_size, const unsigned char *src, size_t src_size,
                                     unsigned valid_bit, unsigned char *valid) {
  size_t len;
  if (!dst || !dst_size || !src)
    return;
  len = strnlen((const char *)src, src_size);
  if (len == 0)
    return;
  if (len >= dst_size)
    len = dst_size - 1;
  memcpy(dst, src, len);
  dst[len] = '\0';
  SET_VALID(valid_bit, valid);
}

static void ascend_set_die_id_string(struct gpuinfo_static_info *static_info, const struct dcmi_die_id *die_id,
                                     const char *label, bool *has_value) {
  size_t used;
  if (!static_info || !die_id || !label || !has_value)
    return;
  used = strlen(static_info->die_id);
  if (used > 0 && used + 1 < sizeof(static_info->die_id)) {
    static_info->die_id[used++] = ';';
    static_info->die_id[used] = '\0';
  }
  if (used >= sizeof(static_info->die_id))
    return;
  snprintf(static_info->die_id + used, sizeof(static_info->die_id) - used, "%s=%08x:%08x:%08x:%08x:%08x", label,
           die_id->soc_die[0], die_id->soc_die[1], die_id->soc_die[2], die_id->soc_die[3], die_id->soc_die[4]);
  *has_value = true;
}

static void ascend_populate_die_id(struct gpuinfo_static_info *static_info, const struct gpu_info_ascend *gpu) {
  static const enum dcmi_die_type types[] = {NDIE, VDIE, DDIE};
  static const char *const labels[] = {"NDIE", "VDIE", "DDIE"};
  bool has_value = false;
  if (!static_info || !gpu)
    return;
  static_info->die_id[0] = '\0';
  for (size_t i = 0; i < sizeof(types) / sizeof(types[0]); ++i) {
    struct dcmi_die_id die_id;
    memset(&die_id, 0, sizeof(die_id));
    if (ascend_get_die_id(gpu, types[i], &die_id) == DCMI_SUCCESS)
      ascend_set_die_id_string(static_info, &die_id, labels[i], &has_value);
  }
  if (has_value)
    SET_VALID(gpuinfo_die_id_valid, static_info->valid);
}

static void ascend_populate_flash_inventory(struct gpuinfo_static_info *static_info,
                                            const struct gpu_info_ascend *gpu) {
  unsigned count = 0;
  unsigned captured = 0;
  if (!static_info || !gpu || ascend_get_flash_count(gpu, &count) != DCMI_SUCCESS)
    return;
  static_info->flash_count = count;
  if (count > GPUINFO_MAX_FLASHES)
    count = GPUINFO_MAX_FLASHES;
  for (unsigned i = 0; i < count; ++i) {
    struct dcmi_flash_info flash_info;
    memset(&flash_info, 0, sizeof(flash_info));
    if (ascend_get_flash_info(gpu, i, &flash_info) != DCMI_SUCCESS)
      continue;
    static_info->flashes[captured].flash_id = flash_info.flash_id;
    static_info->flashes[captured].device_id = flash_info.device_id;
    static_info->flashes[captured].vendor = flash_info.vendor;
    static_info->flashes[captured].state = flash_info.state;
    static_info->flashes[captured].size = flash_info.size;
    static_info->flashes[captured].sector_count = flash_info.sector_count;
    static_info->flashes[captured].manufacturer_id = flash_info.manufacturer_id;
    ++captured;
  }
  static_info->flash_inventory_count = captured;
  SET_VALID(gpuinfo_flash_inventory_valid, static_info->valid);
}

static void ascend_populate_netdevs(struct gpuinfo_static_info *static_info, const struct gpu_info_ascend *gpu) {
  struct dcmi_netdev_list_info netdev_list;
  unsigned count;
  if (!static_info || !gpu || ascend_get_netdev_list(gpu, &netdev_list) != DCMI_SUCCESS)
    return;
  if (netdev_list.netdev_nums < 0)
    return;
  count = (unsigned)netdev_list.netdev_nums;
  static_info->netdev_count = count;
  if (count > GPUINFO_MAX_NETDEVS)
    count = GPUINFO_MAX_NETDEVS;
  for (unsigned i = 0; i < count; ++i) {
    ascend_copy_name(static_info->netdev_names[i], sizeof(static_info->netdev_names[i]),
                     (const unsigned char *)netdev_list.netdev_name[i], NETDEV_NAME_MAX_LEN);
  }
  SET_VALID(gpuinfo_netdev_names_valid, static_info->valid);
}

static void ascend_populate_vrd_and_affinity(struct gpuinfo_static_info *static_info,
                                             const struct gpu_info_ascend *gpu) {
  char buffer[MAX_VERSION_STRING];
  int length;
  if (!static_info || !gpu)
    return;
  memset(buffer, 0, sizeof(buffer));
  if (ascend_get_vrd_version(gpu, buffer, (int)sizeof(buffer)) == DCMI_SUCCESS && buffer[0] != '\0') {
    ascend_copy_name(static_info->vrd_version, sizeof(static_info->vrd_version), (const unsigned char *)buffer,
                     sizeof(buffer));
    SET_VALID(gpuinfo_vrd_version_valid, static_info->valid);
  }
  memset(buffer, 0, sizeof(buffer));
  length = (int)sizeof(buffer);
  if (ascend_get_affinity_cpu(gpu, buffer, &length) == DCMI_SUCCESS && buffer[0] != '\0') {
    ascend_copy_name(static_info->affinity_cpu, sizeof(static_info->affinity_cpu), (const unsigned char *)buffer,
                     sizeof(buffer));
    SET_VALID(gpuinfo_affinity_cpu_valid, static_info->valid);
  }
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

static void ascend_set_ecc_info(struct gpuinfo_dynamic_info *dynamic_info, enum dcmi_device_type type,
                                const struct dcmi_ecc_info *ecc_info) {
  bool hbm = type == DCMI_DEVICE_TYPE_HBM;
  if (!dynamic_info || !ecc_info)
    return;
  if (hbm) {
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_hbm_single_bit_errors, ecc_info->single_bit_error_cnt);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_hbm_double_bit_errors, ecc_info->double_bit_error_cnt);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_hbm_total_single_bit_errors, ecc_info->total_single_bit_error_cnt);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_hbm_total_double_bit_errors, ecc_info->total_double_bit_error_cnt);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_hbm_single_bit_isolated_pages, ecc_info->single_bit_isolated_pages_cnt);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_hbm_double_bit_isolated_pages, ecc_info->double_bit_isolated_pages_cnt);
  } else {
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_ddr_single_bit_errors, ecc_info->single_bit_error_cnt);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_ddr_double_bit_errors, ecc_info->double_bit_error_cnt);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_ddr_total_single_bit_errors, ecc_info->total_single_bit_error_cnt);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_ddr_total_double_bit_errors, ecc_info->total_double_bit_error_cnt);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_ddr_single_bit_isolated_pages, ecc_info->single_bit_isolated_pages_cnt);
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_ddr_double_bit_isolated_pages, ecc_info->double_bit_isolated_pages_cnt);
  }
}

static void ascend_refresh_ecc(struct gpu_info_ascend *gpu_info, struct gpuinfo_dynamic_info *dynamic_info) {
  struct dcmi_ecc_info ecc_info;
  if (!DCMI_SYMBOL_PRESENT(dcmi_get_device_ecc_info) &&
      !(ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_ecc_info)))
    return;

  memset(&ecc_info, 0, sizeof(ecc_info));
  last_dcmi_return_status = ascend_get_ecc_info(gpu_info, DCMI_DEVICE_TYPE_HBM, &ecc_info);
  if (last_dcmi_return_status == DCMI_SUCCESS)
    ascend_set_ecc_info(dynamic_info, DCMI_DEVICE_TYPE_HBM, &ecc_info);

  memset(&ecc_info, 0, sizeof(ecc_info));
  last_dcmi_return_status = ascend_get_ecc_info(gpu_info, DCMI_DEVICE_TYPE_DDR, &ecc_info);
  if (last_dcmi_return_status == DCMI_SUCCESS)
    ascend_set_ecc_info(dynamic_info, DCMI_DEVICE_TYPE_DDR, &ecc_info);
}

static void ascend_refresh_pcie_errors(struct gpu_info_ascend *gpu_info, struct gpuinfo_dynamic_info *dynamic_info) {
  struct dcmi_chip_pcie_err_rate error_info;
  bool got_soc_error_info = false;
  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_pcie_error_cnt) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_pcie_error_cnt))) {
    memset(&error_info, 0, sizeof(error_info));
    last_dcmi_return_status = ascend_get_pcie_error_info(gpu_info, &error_info);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_pcs_rx_error_count, error_info.pcs_rx_err_cnt);
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_phy_lane_error_count, error_info.phy_lane_err_counter);
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_symbol_unlock_error_count, error_info.symbol_unlock_counter);
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_lcrc_error_count, error_info.dl_lcrc_err_num);
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_dcrc_error_count, error_info.dl_dcrc_err_num);
      got_soc_error_info = true;
    }
  }

  /* 910B and newer products expose the user-facing PCIe link counters via
   * dcmi_get_device_info. The low-level PCS counter API is separate and may
   * legitimately return NOT_SUPPORT, so keep both representations. */
  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_info) || (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_info))) {
    struct dcmi_pcie_link_error_info link_error_info;
    unsigned int info_size = sizeof(link_error_info);
    memset(&link_error_info, 0, sizeof(link_error_info));
    last_dcmi_return_status = ascend_get_device_info(gpu_info, DCMI_MAIN_CMD_PCIE, DCMI_PCIE_SUB_CMD_PCIE_ERROR_INFO,
                                                     &link_error_info, &info_size);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_tx_error_count, link_error_info.tx_err_cnt);
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_rx_error_count, link_error_info.rx_err_cnt);
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_lcrc_error_count, link_error_info.lcrc_err_cnt);
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_ecrc_error_count, link_error_info.ecrc_err_cnt);
      SET_GPUINFO_DYNAMIC(dynamic_info, pcie_link_retry_count, link_error_info.retry_cnt);
    } else if (!got_soc_error_info) {
      /* Leave all PCIe error fields invalid when neither documented query is
       * supported; unsupported is represented as N/A/null by the caller. */
      return;
    }
  }
}

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
    last_dcmi_return_status = ascend_get_pcie_link_bandwidth(gpu_info, &bandwidth);
    gpu_info->last_pcie_query = now;
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      gpu_info->pcie_tx = ascend_bandwidth_to_kb_per_second(bandwidth.tx_p_bw, bandwidth.tx_np_bw, bandwidth.tx_cpl_bw);
      gpu_info->pcie_rx = ascend_bandwidth_to_kb_per_second(bandwidth.rx_p_bw, bandwidth.rx_np_bw, bandwidth.rx_cpl_bw);
      gpu_info->pcie_bandwidth_valid = true;
    }
  }
}

static void ascend_apply_pcie_bandwidth(const struct gpu_info_ascend *gpu_info,
                                        struct gpuinfo_dynamic_info *dynamic_info) {
  if (!gpu_info->pcie_bandwidth_valid)
    return;
  SET_GPUINFO_DYNAMIC(dynamic_info, pcie_rx, gpu_info->pcie_rx);
  SET_GPUINFO_DYNAMIC(dynamic_info, pcie_tx, gpu_info->pcie_tx);
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

static bool ascend_boot_status_is_valid(enum dcmi_boot_status status) {
  return status == DCMI_BOOT_STATUS_UNINIT || status == DCMI_BOOT_STATUS_BIOS || status == DCMI_BOOT_STATUS_OS ||
         status == DCMI_BOOT_STATUS_FINISH || status == DCMI_SYSTEM_START_FINISH;
}

static bool ascend_compatibility_is_valid(enum dcmi_device_compat compatibility) {
  return compatibility == DCMI_COMPAT_OK || compatibility == DCMI_COMPAT_NOK || compatibility == DCMI_COMPAT_UNKNOWN;
}

static bool ascend_network_health_is_valid(enum dcmi_rdfx_detect_result result) {
  return result >= DCMI_RDFX_DETECT_OK && result <= DCMI_RDFX_DETECT_IP_SET;
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

static bool ascend_query_hbm_sensor(const struct gpu_info_ascend *gpu, int *temperature) {
  union dcmi_sensor_info sensor_info;
  if (!temperature || (!DCMI_SYMBOL_PRESENT(dcmi_get_device_sensor_info) &&
                       !(ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_sensor_info))))
    return false;
  memset(&sensor_info, 0, sizeof(sensor_info));
  if (ascend_use_dcmiv2 && gpu->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_sensor_info)) {
    last_dcmi_return_status = dcmiv2_get_device_sensor_info(gpu->logical_id, DCMI_HBM_TEMP_ID, &sensor_info);
  } else if (ascend_has_legacy_ids(gpu) && DCMI_SYMBOL_PRESENT(dcmi_get_device_sensor_info)) {
    last_dcmi_return_status = dcmi_get_device_sensor_info(gpu->card_id, gpu->device_id, DCMI_HBM_TEMP_ID, &sensor_info);
  } else {
    return false;
  }
  if (last_dcmi_return_status != DCMI_SUCCESS)
    return false;
  /* Older 910 firmware returns HBM temperature in the uchar union member;
   * newer products use the documented signed-int member. Prefer the normal
   * representation and fall back when it is outside a physical range. */
  *temperature = sensor_info.iint;
  if (*temperature < 0 || *temperature > 200)
    *temperature = sensor_info.uchar;
  return *temperature >= 0;
}

/* Boot/compatibility/network/out-of-band state are read-only, but some of
 * these calls enter firmware or IPMI. Cache them and refresh at a low rate so
 * a transient or slow management query cannot stall the normal sampler. */
static void ascend_refresh_slow_status(struct gpu_info_ascend *gpu_info) {
  time_t now = time(NULL);
  bool should_query = gpu_info->last_slow_query == (time_t)0 || now < gpu_info->last_slow_query ||
                      (unsigned long long)(now - gpu_info->last_slow_query) >= ASCEND_SLOW_QUERY_INTERVAL_SEC;
  if (!should_query)
    return;

  gpu_info->last_slow_query = now;
  ascend_invalidate_slow_cache(gpu_info);

  unsigned power_limit = 0;
  if (ascend_query_power_limit(gpu_info, &power_limit)) {
    gpu_info->power_draw_max = power_limit;
    gpu_info->power_draw_max_valid = true;
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_boot_status) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_boot_status))) {
    enum dcmi_boot_status status = DCMI_BOOT_STATUS_UNINIT;
    if (ascend_get_boot_status(gpu_info, &status) == DCMI_SUCCESS && ascend_boot_status_is_valid(status)) {
      gpu_info->boot_status = (unsigned)status;
      gpu_info->boot_status_valid = true;
    }
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_compatibility) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_compatibility))) {
    enum dcmi_device_compat compatibility = DCMI_COMPAT_UNKNOWN;
    if (ascend_get_compatibility(gpu_info, &compatibility) == DCMI_SUCCESS &&
        ascend_compatibility_is_valid(compatibility)) {
      gpu_info->compatibility = (unsigned)compatibility;
      gpu_info->compatibility_valid = true;
    }
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_network_health) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_network_health))) {
    enum dcmi_rdfx_detect_result result = DCMI_RDFX_DETECT_MAX;
    if (ascend_get_network_health(gpu_info, &result) == DCMI_SUCCESS && ascend_network_health_is_valid(result)) {
      gpu_info->network_health = (unsigned)result;
      gpu_info->network_health_valid = true;
    }
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_outband_channel_state) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_outband_channel_state))) {
    int state = -1;
    if (ascend_get_outband_channel_state(gpu_info, &state) == DCMI_SUCCESS && state >= 0) {
      gpu_info->outband_channel_state = (unsigned)state;
      gpu_info->outband_channel_state_valid = true;
    }
  }
}

static void ascend_apply_slow_status(const struct gpu_info_ascend *gpu_info,
                                     struct gpuinfo_dynamic_info *dynamic_info) {
  if (gpu_info->power_draw_max_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, power_draw_max, gpu_info->power_draw_max);
  if (gpu_info->boot_status_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, boot_status, gpu_info->boot_status);
  if (gpu_info->compatibility_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, compatibility, gpu_info->compatibility);
  if (gpu_info->network_health_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, network_health, gpu_info->network_health);
  if (gpu_info->outband_channel_state_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, outband_channel_state, gpu_info->outband_channel_state);
}

static unsigned long long ascend_add_counter(unsigned long long lhs, unsigned long long rhs) {
  return lhs > ULLONG_MAX - rhs ? ULLONG_MAX : lhs + rhs;
}

static void ascend_collect_ub_port_stats(struct gpu_info_ascend *gpu_info, const struct dcmi_ub_port_info *port) {
  struct dcmi_port_pkt_stats_info stats;
  unsigned long long rx_packets = 0;
  unsigned long long tx_packets = 0;
  unsigned long long rx_errors = 0;
  unsigned long long tx_errors = 0;

  if (!gpu_info || !port || !DCMI_SYMBOL_PRESENT(dcmiv2_get_port_pkt_stats_info))
    return;
  memset(&stats, 0, sizeof(stats));
  if (ascend_get_ub_port_stats(gpu_info, port, &stats) != DCMI_SUCCESS || stats.is_uboe_port != 0)
    return;

  rx_packets = ascend_add_counter(rx_packets, stats.ub_ipv4_pkt_cnt_rx);
  rx_packets = ascend_add_counter(rx_packets, stats.ub_ipv6_pkt_cnt_rx);
  rx_packets = ascend_add_counter(rx_packets, stats.unic_ipv4_pkt_cnt_rx);
  rx_packets = ascend_add_counter(rx_packets, stats.unic_ipv6_pkt_cnt_rx);
  rx_packets = ascend_add_counter(rx_packets, stats.ub_compact_pkt_cnt_rx);
  rx_packets = ascend_add_counter(rx_packets, stats.ub_umoc_ctph_cnt_rx);
  rx_packets = ascend_add_counter(rx_packets, stats.ub_umoc_ntph_cnt_rx);
  rx_packets = ascend_add_counter(rx_packets, stats.ub_mem_pkt_cnt_rx);
  rx_packets = ascend_add_counter(rx_packets, stats.unknown_pkt_cnt_rx);
  tx_packets = ascend_add_counter(tx_packets, stats.ub_ipv4_pkt_cnt_tx);
  tx_packets = ascend_add_counter(tx_packets, stats.ub_ipv6_pkt_cnt_tx);
  tx_packets = ascend_add_counter(tx_packets, stats.unic_ipv4_pkt_cnt_tx);
  tx_packets = ascend_add_counter(tx_packets, stats.unic_ipv6_pkt_cnt_tx);
  tx_packets = ascend_add_counter(tx_packets, stats.ub_compact_pkt_cnt_tx);
  tx_packets = ascend_add_counter(tx_packets, stats.ub_umoc_ctph_cnt_tx);
  tx_packets = ascend_add_counter(tx_packets, stats.ub_umoc_ntph_cnt_tx);
  tx_packets = ascend_add_counter(tx_packets, stats.ub_mem_pkt_cnt_tx);
  tx_packets = ascend_add_counter(tx_packets, stats.unknown_pkt_cnt_tx);

  rx_errors = ascend_add_counter(rx_errors, stats.drop_ind_cnt_rx);
  rx_errors = ascend_add_counter(rx_errors, stats.err_ind_cnt_rx);
  rx_errors = ascend_add_counter(rx_errors, stats.route_err_cnt_rx);
  rx_errors = ascend_add_counter(rx_errors, stats.out_err_cnt_rx);
  rx_errors = ascend_add_counter(rx_errors, stats.length_err_cnt_rx);
  tx_errors = ascend_add_counter(tx_errors, stats.drop_ind_cnt_tx);
  tx_errors = ascend_add_counter(tx_errors, stats.err_ind_cnt_tx);
  tx_errors = ascend_add_counter(tx_errors, stats.lpbk_ind_cnt_tx);
  tx_errors = ascend_add_counter(tx_errors, stats.out_err_cnt_tx);
  tx_errors = ascend_add_counter(tx_errors, stats.length_err_cnt_tx);

  gpu_info->ub_port_id = stats.port_id;
  gpu_info->ub_port_tx_packets = tx_packets;
  gpu_info->ub_port_rx_packets = rx_packets;
  gpu_info->ub_port_tx_errors = tx_errors;
  gpu_info->ub_port_rx_errors = rx_errors;
  gpu_info->ub_port_crc_errors = stats.crc_error_sum;
  gpu_info->ub_port_stats_valid = true;
}

static void ascend_collect_ecc_history(struct gpu_info_ascend *gpu_info, enum dcmi_device_type module_type,
                                       unsigned *record_count, unsigned *last_error_time, bool *valid) {
  struct dcmi_ecc_record_type type = {MULTI_ECC_INFO_READ, module_type};
  struct dcmi_ecc_common_data records[GPUINFO_MAX_ECC_RECORDS];
  unsigned count = GPUINFO_MAX_ECC_RECORDS;
  unsigned last = 0;

  if (!gpu_info || !record_count || !last_error_time || !valid)
    return;
  memset(records, 0, sizeof(records));
  if (ascend_get_ecc_records(gpu_info, type, &count, records) != DCMI_SUCCESS || count > GPUINFO_MAX_ECC_RECORDS)
    return;
  for (unsigned i = 0; i < count; ++i) {
    if (records[i].timestamp > 0 && (unsigned)records[i].timestamp > last)
      last = (unsigned)records[i].timestamp;
  }
  *record_count = count;
  *last_error_time = last;
  *valid = true;
}

static void ascend_collect_tc_stats(struct gpu_info_ascend *gpu_info) {
  struct dcmi_tc_stat_data stats;
  unsigned long long tx = 0;
  unsigned long long rx = 0;
  if (!gpu_info || !ascend_has_legacy_ids(gpu_info) || !DCMI_SYMBOL_PRESENT(dcmi_get_netdev_tc_stat_info))
    return;
  memset(&stats, 0, sizeof(stats));
  if (dcmi_get_netdev_tc_stat_info(gpu_info->card_id, gpu_info->device_id, &stats) != DCMI_SUCCESS)
    return;
  for (unsigned i = 0; i < TC_MAX_NUM; ++i) {
    tx = ascend_add_counter(tx, stats.tc_tx[i]);
    rx = ascend_add_counter(rx, stats.tc_rx[i]);
  }
  gpu_info->network_tc_tx_packets = tx;
  gpu_info->network_tc_rx_packets = rx;
  gpu_info->network_tc_stats_valid = true;
}

static void ascend_refresh_extended_status(struct gpu_info_ascend *gpu_info) {
  time_t now = time(NULL);
  bool should_query = gpu_info->last_extended_query == (time_t)0 || now < gpu_info->last_extended_query ||
                      (unsigned long long)(now - gpu_info->last_extended_query) >= ASCEND_SLOW_QUERY_INTERVAL_SEC;
  if (!should_query)
    return;
  gpu_info->last_extended_query = now;
  ascend_invalidate_extended_cache(gpu_info);

  if (ascend_has_legacy_ids(gpu_info) &&
      (DCMI_SYMBOL_PRESENT(dcmi_get_device_p2p_enable) || DCMI_SYMBOL_PRESENT(dcmi_get_p2p_enable))) {
    int enabled = -1;
    if (ascend_get_p2p_enable(gpu_info, &enabled) == DCMI_SUCCESS && (enabled == 0 || enabled == 1)) {
      gpu_info->p2p_enabled = enabled != 0;
      gpu_info->p2p_enabled_valid = true;
    }
  }

  if ((ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_share_enable)) ||
      (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_device_share_enable))) {
    unsigned enabled = 0;
    if (ascend_get_device_share_enable(gpu_info, &enabled) == DCMI_SUCCESS && enabled <= 1) {
      gpu_info->device_share_enabled = enabled != 0;
      gpu_info->device_share_enabled_valid = true;
    }
  }

  if ((ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_cgroup_info)) ||
      (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_device_cgroup_info))) {
    struct dcmi_cgroup_info cgroup_info;
    memset(&cgroup_info, 0, sizeof(cgroup_info));
    if (ascend_get_cgroup_info(gpu_info, &cgroup_info) == DCMI_SUCCESS) {
      gpu_info->cgroup_memory_limit = (unsigned long long)cgroup_info.limit_in_bytes;
      gpu_info->cgroup_memory_usage = (unsigned long long)cgroup_info.usage_in_bytes;
      gpu_info->cgroup_memory_max_usage = (unsigned long long)cgroup_info.max_usage_in_bytes;
      gpu_info->cgroup_memory_limit_valid = true;
      gpu_info->cgroup_memory_usage_valid = true;
      gpu_info->cgroup_memory_max_usage_valid = true;
    }
  }

  if ((ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_llc_perf_para)) ||
      (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_device_llc_perf_para))) {
    struct dcmi_llc_perf llc_info;
    memset(&llc_info, 0, sizeof(llc_info));
    if (ascend_get_llc_perf(gpu_info, &llc_info) == DCMI_SUCCESS && ascend_percentage_is_valid(llc_info.rd_hit_rate) &&
        ascend_percentage_is_valid(llc_info.wr_hit_rate)) {
      gpu_info->llc_read_hit_rate = llc_info.rd_hit_rate;
      gpu_info->llc_write_hit_rate = llc_info.wr_hit_rate;
      gpu_info->llc_throughput = llc_info.throughput;
      gpu_info->llc_valid = true;
    }
  }

  if (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_hccs_link_bandwidth_info)) {
    struct dcmi_hccs_bandwidth_info hccs_info;
    memset(&hccs_info, 0, sizeof(hccs_info));
    hccs_info.profiling_time = ASCEND_PCIE_PROFILING_TIME_MS;
    if (ascend_get_hccs_bandwidth(gpu_info, &hccs_info) == DCMI_SUCCESS && isfinite(hccs_info.total_txbw) &&
        isfinite(hccs_info.total_rxbw) && hccs_info.total_txbw >= 0.0 && hccs_info.total_rxbw >= 0.0) {
      gpu_info->hccs_tx_bandwidth = hccs_info.total_txbw;
      gpu_info->hccs_rx_bandwidth = hccs_info.total_rxbw;
      gpu_info->hccs_valid = true;
    }
  }

  if ((ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_ub_port_link_status)) ||
      (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_ub_port_link_status_info))) {
    struct dcmi_ub_port_link_status ub_status;
    memset(&ub_status, 0, sizeof(ub_status));
    if (ascend_get_ub_status(gpu_info, &ub_status) == DCMI_SUCCESS &&
        ub_status.ub_link_status <= DCMI_UB_NO_NEED_LINK) {
      gpu_info->ub_link_status = (unsigned)ub_status.ub_link_status;
      gpu_info->ub_link_status_valid = true;
    }
  }

  /* UB and RDMA bandwidth calls require a port and are deliberately sampled
   * at the same low rate as the management-plane state. Prefer the first UB
   * port returned by the logical-device API. The legacy 950 card API rejects
   * port 0 and accepts product-dependent ports 4/5/6/8, so probe that bounded
   * set rather than publishing a misleading zero sample. */
  struct dcmi_ub_port_info selected_port = {0, 0};
  bool have_selected_port = false;
  bool ub_bandwidth_queried = false;
  if (ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_port_list_info)) {
    struct dcmi_port_list_info port_list;
    memset(&port_list, 0, sizeof(port_list));
    if (ascend_get_port_list(gpu_info, &port_list) == DCMI_SUCCESS && port_list.die_nums > 0 &&
        port_list.die_nums <= MAX_DIE_NUMS) {
      for (int die = 0; die < port_list.die_nums && !have_selected_port; ++die) {
        int port_count = port_list.die_list_data[die].port_nums;
        if (port_count < 0 || port_count > MAX_PORT_NUMS)
          continue;
        for (int port = 0; port < port_count; ++port) {
          const struct port_info *candidate = &port_list.die_list_data[die].port_list_data[port];
          if (candidate->mode == UB_MODE && candidate->port_index >= 0) {
            selected_port.udie_id = port_list.die_list_data[die].die_index;
            selected_port.port_id = candidate->port_index;
            have_selected_port = true;
            break;
          }
        }
      }
    }
  }
  if (!have_selected_port && ascend_use_dcmiv2 && gpu_info->logical_id_valid &&
      DCMI_SYMBOL_PRESENT(dcmiv2_get_ub_realtime_bandwidth_info)) {
    static const int logical_ports[] = {0, 4, 5, 6, 8};
    for (size_t i = 0; i < sizeof(logical_ports) / sizeof(logical_ports[0]); ++i) {
      selected_port.udie_id = 0;
      selected_port.port_id = logical_ports[i];
      struct dcmi_ub_bandwidth_info ub_bandwidth;
      memset(&ub_bandwidth, 0, sizeof(ub_bandwidth));
      last_dcmi_return_status = dcmiv2_get_ub_realtime_bandwidth_info(
          gpu_info->logical_id, ASCEND_PCIE_PROFILING_TIME_MS, &selected_port, &ub_bandwidth);
      if (last_dcmi_return_status == DCMI_SUCCESS) {
        gpu_info->ub_tx_bandwidth = (double)ub_bandwidth.tx_bandwidth;
        gpu_info->ub_rx_bandwidth = (double)ub_bandwidth.rx_bandwidth;
        gpu_info->ub_bandwidth_valid = true;
        ub_bandwidth_queried = true;
        have_selected_port = true;
        break;
      }
    }
  }
  if (have_selected_port && !ub_bandwidth_queried && ascend_use_dcmiv2 && gpu_info->logical_id_valid &&
      DCMI_SYMBOL_PRESENT(dcmiv2_get_ub_realtime_bandwidth_info)) {
    struct dcmi_ub_bandwidth_info ub_bandwidth;
    memset(&ub_bandwidth, 0, sizeof(ub_bandwidth));
    last_dcmi_return_status = dcmiv2_get_ub_realtime_bandwidth_info(gpu_info->logical_id, ASCEND_PCIE_PROFILING_TIME_MS,
                                                                    &selected_port, &ub_bandwidth);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      gpu_info->ub_tx_bandwidth = (double)ub_bandwidth.tx_bandwidth;
      gpu_info->ub_rx_bandwidth = (double)ub_bandwidth.rx_bandwidth;
      gpu_info->ub_bandwidth_valid = true;
      ub_bandwidth_queried = true;
    }
  }

  /* If the logical-device probe is present but returns NOT_SUPPORT, retry
   * through the physical card/device ABI when the mapping is available. A
   * mixed-version driver can export both entry points while only one of them
   * is implemented for a given product. */
  if (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_ub_realtime_bandwidth_info) &&
      !ub_bandwidth_queried) {
    static const int legacy_ports[] = {4, 5, 6, 8, 0};
    struct dcmi_ub_port_info legacy_port = selected_port;
    for (size_t i = 0; i < sizeof(legacy_ports) / sizeof(legacy_ports[0]); ++i) {
      legacy_port.udie_id = 0;
      legacy_port.port_id = legacy_ports[i];
      struct dcmi_ub_bandwidth_info ub_bandwidth;
      memset(&ub_bandwidth, 0, sizeof(ub_bandwidth));
      last_dcmi_return_status = dcmi_get_ub_realtime_bandwidth_info(
          gpu_info->card_id, gpu_info->device_id, ASCEND_PCIE_PROFILING_TIME_MS, &legacy_port, &ub_bandwidth);
      if (last_dcmi_return_status == DCMI_SUCCESS) {
        /* The legacy card API preserves the hundredths-of-MB/s representation
         * used by its npu-smi formatter. */
        gpu_info->ub_tx_bandwidth = (double)ub_bandwidth.tx_bandwidth / 100.0;
        gpu_info->ub_rx_bandwidth = (double)ub_bandwidth.rx_bandwidth / 100.0;
        gpu_info->ub_bandwidth_valid = true;
        ub_bandwidth_queried = true;
        selected_port = legacy_port;
        have_selected_port = true;
        break;
      }
    }
  }

  if ((ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_rdma_bandwidth_info)) ||
      (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_rdma_bandwidth_info))) {
    struct dcmi_network_rdma_bandwidth_info rdma_bandwidth;
    memset(&rdma_bandwidth, 0, sizeof(rdma_bandwidth));
    /* DCMI's public npu-smi path uses port 0 for the aggregate device view. */
    if (ascend_get_rdma_bandwidth(gpu_info, 0, ASCEND_PCIE_PROFILING_TIME_MS, &rdma_bandwidth) == DCMI_SUCCESS) {
      gpu_info->rdma_tx_bandwidth = rdma_bandwidth.tx_bandwidth;
      gpu_info->rdma_rx_bandwidth = rdma_bandwidth.rx_bandwidth;
      gpu_info->rdma_bandwidth_valid = true;
    }
  }

  if ((ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_netdev_pkt_stats_info)) ||
      (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_netdev_pkt_stats_info))) {
    struct dcmi_network_pkt_stats_info packet_stats;
    memset(&packet_stats, 0, sizeof(packet_stats));
    if (ascend_get_network_pkt_stats(gpu_info, 0, &packet_stats) == DCMI_SUCCESS) {
      gpu_info->network_tx_packets = packet_stats.mac_tx_total_pkt_num;
      gpu_info->network_rx_packets = packet_stats.mac_rx_total_pkt_num;
      gpu_info->network_tx_bytes = packet_stats.mac_tx_total_oct_num;
      gpu_info->network_rx_bytes = packet_stats.mac_rx_total_oct_num;
      gpu_info->network_tx_errors = packet_stats.mac_tx_bad_pkt_num;
      gpu_info->network_rx_errors = packet_stats.mac_rx_bad_pkt_num;
      gpu_info->network_rx_fcs_errors = packet_stats.mac_rx_fcs_err_pkt_num;
      gpu_info->network_stats_valid = true;
    }
  }

  if ((ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_current_fault_event)) ||
      (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_device_current_fault_event))) {
    struct dcmi_event events[GPUINFO_MAX_FAULT_EVENTS];
    int event_count = 0;
    memset(events, 0, sizeof(events));
    if (ascend_get_current_fault_events(gpu_info, events, GPUINFO_MAX_FAULT_EVENTS, &event_count) == DCMI_SUCCESS &&
        event_count >= 0) {
      unsigned count =
          (unsigned)event_count > GPUINFO_MAX_FAULT_EVENTS ? GPUINFO_MAX_FAULT_EVENTS : (unsigned)event_count;
      memset(gpu_info->fault_events, 0, sizeof(gpu_info->fault_events));
      for (unsigned i = 0; i < count; ++i) {
        gpu_info->fault_events[i].event_id = events[i].event_t.dms_event.event_id;
        gpu_info->fault_events[i].severity = events[i].event_t.dms_event.severity;
        gpu_info->fault_events[i].assertion = events[i].event_t.dms_event.assertion;
        gpu_info->fault_events[i].alarm_raised_time = events[i].event_t.dms_event.alarm_raised_time;
        ascend_copy_name(gpu_info->fault_events[i].event_name, sizeof(gpu_info->fault_events[i].event_name),
                         (const unsigned char *)events[i].event_t.dms_event.event_name,
                         sizeof(events[i].event_t.dms_event.event_name));
        ascend_copy_name(gpu_info->fault_events[i].additional_info, sizeof(gpu_info->fault_events[i].additional_info),
                         (const unsigned char *)events[i].event_t.dms_event.additional_info,
                         sizeof(events[i].event_t.dms_event.additional_info));
      }
      gpu_info->fault_event_count = count;
      gpu_info->fault_events_valid = true;
    }
  }

  if ((ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_system_time)) ||
      (ascend_has_legacy_ids(gpu_info) &&
       (DCMI_SYMBOL_PRESENT(dcmi_get_device_system_time) || DCMI_SYMBOL_PRESENT(dcmi_get_system_time)))) {
    unsigned system_time = 0;
    if (ascend_get_system_time(gpu_info, &system_time) == DCMI_SUCCESS) {
      gpu_info->device_system_time = system_time;
      gpu_info->device_system_time_valid = true;
    }
  }

  if ((ascend_use_dcmiv2 && gpu_info->logical_id_valid && DCMI_SYMBOL_PRESENT(dcmiv2_get_multi_ecc_record_info)) ||
      (ascend_has_legacy_ids(gpu_info) && DCMI_SYMBOL_PRESENT(dcmi_get_multi_ecc_record_info_v2))) {
    ascend_collect_ecc_history(gpu_info, DCMI_DEVICE_TYPE_HBM, &gpu_info->ecc_hbm_history_count,
                               &gpu_info->ecc_hbm_last_error_time, &gpu_info->ecc_hbm_history_valid);
    ascend_collect_ecc_history(gpu_info, DCMI_DEVICE_TYPE_DDR, &gpu_info->ecc_ddr_history_count,
                               &gpu_info->ecc_ddr_last_error_time, &gpu_info->ecc_ddr_history_valid);
  }

  ascend_collect_tc_stats(gpu_info);
  if (have_selected_port)
    ascend_collect_ub_port_stats(gpu_info, &selected_port);
}

static void ascend_copy_cached_status(struct gpu_info_ascend *dst, const struct gpu_info_ascend *src) {
#define ASCEND_COPY_CACHE_FIELD(field) dst->field = src->field
  ASCEND_COPY_CACHE_FIELD(last_slow_query);
  ASCEND_COPY_CACHE_FIELD(last_extended_query);
  ASCEND_COPY_CACHE_FIELD(last_pcie_query);
  ASCEND_COPY_CACHE_FIELD(pcie_rx);
  ASCEND_COPY_CACHE_FIELD(pcie_tx);
  ASCEND_COPY_CACHE_FIELD(pcie_bandwidth_valid);
  ASCEND_COPY_CACHE_FIELD(power_draw_max);
  ASCEND_COPY_CACHE_FIELD(boot_status);
  ASCEND_COPY_CACHE_FIELD(compatibility);
  ASCEND_COPY_CACHE_FIELD(network_health);
  ASCEND_COPY_CACHE_FIELD(outband_channel_state);
  ASCEND_COPY_CACHE_FIELD(power_draw_max_valid);
  ASCEND_COPY_CACHE_FIELD(boot_status_valid);
  ASCEND_COPY_CACHE_FIELD(compatibility_valid);
  ASCEND_COPY_CACHE_FIELD(network_health_valid);
  ASCEND_COPY_CACHE_FIELD(outband_channel_state_valid);
  ASCEND_COPY_CACHE_FIELD(device_share_enabled);
  ASCEND_COPY_CACHE_FIELD(device_share_enabled_valid);
  ASCEND_COPY_CACHE_FIELD(p2p_enabled);
  ASCEND_COPY_CACHE_FIELD(p2p_enabled_valid);
  ASCEND_COPY_CACHE_FIELD(cgroup_memory_limit);
  ASCEND_COPY_CACHE_FIELD(cgroup_memory_usage);
  ASCEND_COPY_CACHE_FIELD(cgroup_memory_max_usage);
  ASCEND_COPY_CACHE_FIELD(cgroup_memory_limit_valid);
  ASCEND_COPY_CACHE_FIELD(cgroup_memory_usage_valid);
  ASCEND_COPY_CACHE_FIELD(cgroup_memory_max_usage_valid);
  ASCEND_COPY_CACHE_FIELD(llc_read_hit_rate);
  ASCEND_COPY_CACHE_FIELD(llc_write_hit_rate);
  ASCEND_COPY_CACHE_FIELD(llc_throughput);
  ASCEND_COPY_CACHE_FIELD(llc_valid);
  ASCEND_COPY_CACHE_FIELD(hccs_tx_bandwidth);
  ASCEND_COPY_CACHE_FIELD(hccs_rx_bandwidth);
  ASCEND_COPY_CACHE_FIELD(hccs_valid);
  ASCEND_COPY_CACHE_FIELD(ub_link_status);
  ASCEND_COPY_CACHE_FIELD(ub_link_status_valid);
  ASCEND_COPY_CACHE_FIELD(ub_tx_bandwidth);
  ASCEND_COPY_CACHE_FIELD(ub_rx_bandwidth);
  ASCEND_COPY_CACHE_FIELD(ub_bandwidth_valid);
  ASCEND_COPY_CACHE_FIELD(rdma_tx_bandwidth);
  ASCEND_COPY_CACHE_FIELD(rdma_rx_bandwidth);
  ASCEND_COPY_CACHE_FIELD(rdma_bandwidth_valid);
  ASCEND_COPY_CACHE_FIELD(network_tx_packets);
  ASCEND_COPY_CACHE_FIELD(network_rx_packets);
  ASCEND_COPY_CACHE_FIELD(network_tx_bytes);
  ASCEND_COPY_CACHE_FIELD(network_rx_bytes);
  ASCEND_COPY_CACHE_FIELD(network_tx_errors);
  ASCEND_COPY_CACHE_FIELD(network_rx_errors);
  ASCEND_COPY_CACHE_FIELD(network_rx_fcs_errors);
  ASCEND_COPY_CACHE_FIELD(network_stats_valid);
  ASCEND_COPY_CACHE_FIELD(device_system_time);
  ASCEND_COPY_CACHE_FIELD(device_system_time_valid);
  ASCEND_COPY_CACHE_FIELD(ecc_hbm_history_count);
  ASCEND_COPY_CACHE_FIELD(ecc_hbm_last_error_time);
  ASCEND_COPY_CACHE_FIELD(ecc_ddr_history_count);
  ASCEND_COPY_CACHE_FIELD(ecc_ddr_last_error_time);
  ASCEND_COPY_CACHE_FIELD(ecc_hbm_history_valid);
  ASCEND_COPY_CACHE_FIELD(ecc_ddr_history_valid);
  ASCEND_COPY_CACHE_FIELD(ub_port_id);
  ASCEND_COPY_CACHE_FIELD(ub_port_tx_packets);
  ASCEND_COPY_CACHE_FIELD(ub_port_rx_packets);
  ASCEND_COPY_CACHE_FIELD(ub_port_tx_errors);
  ASCEND_COPY_CACHE_FIELD(ub_port_rx_errors);
  ASCEND_COPY_CACHE_FIELD(ub_port_crc_errors);
  ASCEND_COPY_CACHE_FIELD(ub_port_stats_valid);
  ASCEND_COPY_CACHE_FIELD(network_tc_tx_packets);
  ASCEND_COPY_CACHE_FIELD(network_tc_rx_packets);
  ASCEND_COPY_CACHE_FIELD(network_tc_stats_valid);
  memcpy(dst->fault_events, src->fault_events, sizeof(dst->fault_events));
  ASCEND_COPY_CACHE_FIELD(fault_event_count);
  ASCEND_COPY_CACHE_FIELD(fault_events_valid);
#undef ASCEND_COPY_CACHE_FIELD
}

static void *ascend_worker_main(void *arg) {
  (void)arg;
  for (;;) {
    struct gpu_info_ascend *snapshot[MAX_CARD_NUM];
    size_t snapshot_count = 0;
    pthread_mutex_lock(&ascend_worker_mutex);
    if (ascend_worker_stop) {
      pthread_mutex_unlock(&ascend_worker_mutex);
      break;
    }
    struct gpu_info_ascend *entry;
    list_for_each_entry(entry, &allocations, allocate_list) {
      unsigned allocation_count = entry->allocation_count;
      if (allocation_count == 0)
        allocation_count = 1;
      if (allocation_count > MAX_CARD_NUM - snapshot_count)
        allocation_count = MAX_CARD_NUM - snapshot_count;
      for (unsigned i = 0; i < allocation_count; ++i)
        snapshot[snapshot_count++] = &entry[i];
      if (snapshot_count == MAX_CARD_NUM)
        break;
    }
    pthread_mutex_unlock(&ascend_worker_mutex);

    for (size_t snapshot_index = 0; snapshot_index < snapshot_count; ++snapshot_index) {
      struct gpu_info_ascend *gpu_info = snapshot[snapshot_index];
      struct gpu_info_ascend sample = {0};
      sample.card_id = gpu_info->card_id;
      sample.device_id = gpu_info->device_id;
      sample.logical_id = gpu_info->logical_id;
      sample.logical_id_valid = gpu_info->logical_id_valid;
      pthread_mutex_lock(&ascend_cache_mutex);
      ascend_copy_cached_status(&sample, gpu_info);
      pthread_mutex_unlock(&ascend_cache_mutex);

      /* These calls may enter firmware, IPMI, or management sockets. They
       * run entirely on this worker; the curses sampler only consumes the
       * committed cache below. */
      ascend_refresh_slow_status(&sample);
      ascend_refresh_extended_status(&sample);
      ascend_refresh_pcie_bandwidth_cache(&sample);

      pthread_mutex_lock(&ascend_cache_mutex);
      ascend_copy_cached_status(gpu_info, &sample);
      pthread_mutex_unlock(&ascend_cache_mutex);
    }

    pthread_mutex_lock(&ascend_worker_mutex);
    if (!ascend_worker_stop) {
      struct timespec wake_at;
      clock_gettime(CLOCK_REALTIME, &wake_at);
      wake_at.tv_nsec += 100000000L;
      if (wake_at.tv_nsec >= 1000000000L) {
        wake_at.tv_sec += 1;
        wake_at.tv_nsec -= 1000000000L;
      }
      (void)pthread_cond_timedwait(&ascend_worker_cond, &ascend_worker_mutex, &wake_at);
    }
    bool stop = ascend_worker_stop;
    pthread_mutex_unlock(&ascend_worker_mutex);
    if (stop)
      break;
  }
  return NULL;
}

static void ascend_start_worker(void) {
  pthread_mutex_lock(&ascend_worker_mutex);
  ascend_worker_stop = false;
  if (pthread_create(&ascend_worker_thread, NULL, ascend_worker_main, NULL) == 0)
    ascend_worker_started = true;
  else
    ascend_worker_started = false;
  pthread_mutex_unlock(&ascend_worker_mutex);
}

static void ascend_stop_worker(void) {
  pthread_mutex_lock(&ascend_worker_mutex);
  if (!ascend_worker_started) {
    pthread_mutex_unlock(&ascend_worker_mutex);
    return;
  }
  ascend_worker_stop = true;
  pthread_cond_signal(&ascend_worker_cond);
  pthread_mutex_unlock(&ascend_worker_mutex);
  pthread_join(ascend_worker_thread, NULL);
  pthread_mutex_lock(&ascend_worker_mutex);
  ascend_worker_started = false;
  pthread_mutex_unlock(&ascend_worker_mutex);
}

static void ascend_apply_extended_status(const struct gpu_info_ascend *gpu_info,
                                         struct gpuinfo_dynamic_info *dynamic_info) {
  if (gpu_info->p2p_enabled_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, p2p_enabled, gpu_info->p2p_enabled);
  if (gpu_info->device_share_enabled_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, device_share_enabled, gpu_info->device_share_enabled);
  if (gpu_info->cgroup_memory_limit_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, cgroup_memory_limit, gpu_info->cgroup_memory_limit);
  if (gpu_info->cgroup_memory_usage_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, cgroup_memory_usage, gpu_info->cgroup_memory_usage);
  if (gpu_info->cgroup_memory_max_usage_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, cgroup_memory_max_usage, gpu_info->cgroup_memory_max_usage);
  if (gpu_info->llc_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, llc_read_hit_rate, gpu_info->llc_read_hit_rate);
    SET_GPUINFO_DYNAMIC(dynamic_info, llc_write_hit_rate, gpu_info->llc_write_hit_rate);
    SET_GPUINFO_DYNAMIC(dynamic_info, llc_throughput, gpu_info->llc_throughput);
  }
  if (gpu_info->hccs_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, hccs_tx_bandwidth, gpu_info->hccs_tx_bandwidth);
    SET_GPUINFO_DYNAMIC(dynamic_info, hccs_rx_bandwidth, gpu_info->hccs_rx_bandwidth);
  }
  if (gpu_info->ub_link_status_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, ub_link_status, gpu_info->ub_link_status);
  if (gpu_info->ub_bandwidth_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, ub_tx_bandwidth, gpu_info->ub_tx_bandwidth);
    SET_GPUINFO_DYNAMIC(dynamic_info, ub_rx_bandwidth, gpu_info->ub_rx_bandwidth);
  }
  if (gpu_info->rdma_bandwidth_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, rdma_tx_bandwidth, gpu_info->rdma_tx_bandwidth);
    SET_GPUINFO_DYNAMIC(dynamic_info, rdma_rx_bandwidth, gpu_info->rdma_rx_bandwidth);
  }
  if (gpu_info->network_stats_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, network_tx_packets, gpu_info->network_tx_packets);
    SET_GPUINFO_DYNAMIC(dynamic_info, network_rx_packets, gpu_info->network_rx_packets);
    SET_GPUINFO_DYNAMIC(dynamic_info, network_tx_bytes, gpu_info->network_tx_bytes);
    SET_GPUINFO_DYNAMIC(dynamic_info, network_rx_bytes, gpu_info->network_rx_bytes);
    SET_GPUINFO_DYNAMIC(dynamic_info, network_tx_errors, gpu_info->network_tx_errors);
    SET_GPUINFO_DYNAMIC(dynamic_info, network_rx_errors, gpu_info->network_rx_errors);
    SET_GPUINFO_DYNAMIC(dynamic_info, network_rx_fcs_errors, gpu_info->network_rx_fcs_errors);
  }
  if (gpu_info->device_system_time_valid)
    SET_GPUINFO_DYNAMIC(dynamic_info, device_system_time, gpu_info->device_system_time);
  if (gpu_info->ecc_hbm_history_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_hbm_history_count, gpu_info->ecc_hbm_history_count);
    dynamic_info->ecc_hbm_last_error_time = gpu_info->ecc_hbm_last_error_time;
  }
  if (gpu_info->ecc_ddr_history_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, ecc_ddr_history_count, gpu_info->ecc_ddr_history_count);
    dynamic_info->ecc_ddr_last_error_time = gpu_info->ecc_ddr_last_error_time;
  }
  if (gpu_info->ub_port_stats_valid) {
    SET_GPUINFO_DYNAMIC(dynamic_info, ub_port_id, gpu_info->ub_port_id);
    dynamic_info->ub_port_tx_packets = gpu_info->ub_port_tx_packets;
    dynamic_info->ub_port_rx_packets = gpu_info->ub_port_rx_packets;
    dynamic_info->ub_port_tx_errors = gpu_info->ub_port_tx_errors;
    dynamic_info->ub_port_rx_errors = gpu_info->ub_port_rx_errors;
    dynamic_info->ub_port_crc_errors = gpu_info->ub_port_crc_errors;
  }
  if (gpu_info->network_tc_stats_valid) {
    dynamic_info->network_tc_tx_packets = gpu_info->network_tc_tx_packets;
    dynamic_info->network_tc_rx_packets = gpu_info->network_tc_rx_packets;
    SET_VALID(gpuinfo_network_tc_stats_valid, dynamic_info->valid);
  }
  if (gpu_info->fault_events_valid) {
    dynamic_info->fault_event_count = gpu_info->fault_event_count;
    memcpy(dynamic_info->fault_events, gpu_info->fault_events, sizeof(dynamic_info->fault_events));
    SET_VALID(gpuinfo_fault_events_valid, dynamic_info->valid);
  }
}

static bool gpuinfo_ascend_init(void) {
  local_error_string = "";
  memset(ascend_driver_version, 0, sizeof(ascend_driver_version));
  memset(ascend_dcmi_version, 0, sizeof(ascend_dcmi_version));
  ascend_driver_version_valid = false;
  ascend_dcmi_version_valid = false;
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
  if (last_dcmi_return_status != DCMI_SUCCESS)
    return false;

  if (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_driver_version))
    last_dcmi_return_status = dcmiv2_get_driver_version(ascend_driver_version, sizeof(ascend_driver_version));
  else if (DCMI_SYMBOL_PRESENT(dcmi_get_driver_version))
    last_dcmi_return_status = dcmi_get_driver_version(ascend_driver_version, sizeof(ascend_driver_version));
  else
    last_dcmi_return_status = -1;
  ascend_driver_version_valid = last_dcmi_return_status == DCMI_SUCCESS && ascend_driver_version[0] != '\0';

  if (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_dcmi_version))
    last_dcmi_return_status = dcmiv2_get_dcmi_version(ascend_dcmi_version, sizeof(ascend_dcmi_version));
  else if (DCMI_SYMBOL_PRESENT(dcmi_get_dcmi_version))
    last_dcmi_return_status = dcmi_get_dcmi_version(ascend_dcmi_version, sizeof(ascend_dcmi_version));
  else
    last_dcmi_return_status = -1;
  ascend_dcmi_version_valid = last_dcmi_return_status == DCMI_SUCCESS && ascend_dcmi_version[0] != '\0';
  ascend_start_worker();
  if (!ascend_worker_started) {
    local_error_string = "Failed to start DCMI worker";
    return false;
  }
  return true;
}

static void gpuinfo_ascend_shutdown(void) {
  local_error_string = "";
  ascend_stop_worker();
  struct gpu_info_ascend *allocated, *tmp;
  list_for_each_entry_safe(allocated, tmp, &allocations, allocate_list) {
    list_del(&allocated->allocate_list);
    free(allocated);
  }
  memset(ascend_driver_version, 0, sizeof(ascend_driver_version));
  memset(ascend_dcmi_version, 0, sizeof(ascend_dcmi_version));
  ascend_driver_version_valid = false;
  ascend_dcmi_version_valid = false;
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
  pthread_mutex_lock(&ascend_worker_mutex);
  list_add(&gpu_infos[0].allocate_list, &allocations);
  pthread_cond_signal(&ascend_worker_cond);
  pthread_mutex_unlock(&ascend_worker_mutex);
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
      ascend_set_static_string(static_info->chip_type, sizeof(static_info->chip_type), chip_info.chip_type,
                               MAX_CHIP_NAME_LEN, gpuinfo_chip_type_valid, static_info->valid);
      ascend_set_static_string(static_info->chip_version, sizeof(static_info->chip_version), chip_info.chip_ver,
                               MAX_CHIP_NAME_LEN, gpuinfo_chip_version_valid, static_info->valid);
      ascend_set_static_string(static_info->npu_name, sizeof(static_info->npu_name), chip_info.npu_name,
                               MAX_CHIP_NAME_LEN, gpuinfo_npu_name_valid, static_info->valid);
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
      ascend_set_static_string(static_info->chip_type, sizeof(static_info->chip_type), chip_info.chip_type,
                               MAX_CHIP_NAME_LEN, gpuinfo_chip_type_valid, static_info->valid);
      ascend_set_static_string(static_info->chip_version, sizeof(static_info->chip_version), chip_info.chip_ver,
                               MAX_CHIP_NAME_LEN, gpuinfo_chip_version_valid, static_info->valid);
      ascend_set_aicore_count(static_info, chip_info.aicore_cnt);
    }
  }

  bool got_board_info = false;
  if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_board_info)) ||
      DCMI_SYMBOL_PRESENT(dcmi_get_device_board_info)) {
    struct dcmi_board_info board_info;
    memset(&board_info, 0, sizeof(board_info));
    last_dcmi_return_status = ascend_get_board_info(gpu_info, &board_info);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      SET_GPUINFO_STATIC(static_info, board_id, board_info.board_id);
      SET_GPUINFO_STATIC(static_info, pcb_id, board_info.pcb_id);
      SET_GPUINFO_STATIC(static_info, bom_id, board_info.bom_id);
      SET_GPUINFO_STATIC(static_info, slot_id, board_info.slot_id);
      got_board_info = true;
    }
  }
  if (!got_board_info && ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_board_id)) ||
                          DCMI_SYMBOL_PRESENT(dcmi_get_device_board_id))) {
    unsigned board_id = 0;
    last_dcmi_return_status = ascend_get_board_id(gpu_info, &board_id);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      SET_GPUINFO_STATIC(static_info, board_id, board_id);
  }

  if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_mainboard_id)) ||
      DCMI_SYMBOL_PRESENT(dcmi_get_mainboard_id)) {
    unsigned mainboard_id = 0;
    if (ascend_get_mainboard_id(gpu_info, &mainboard_id) == DCMI_SUCCESS)
      SET_GPUINFO_STATIC(static_info, mainboard_id, mainboard_id);
  }

  if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_pcie_slot_id)) ||
      DCMI_SYMBOL_PRESENT(dcmi_get_card_pcie_slot)) {
    int pcie_slot_id = -1;
    if (ascend_get_pcie_slot_id(gpu_info, &pcie_slot_id) == DCMI_SUCCESS && pcie_slot_id >= 0) {
      SET_GPUINFO_STATIC(static_info, pcie_slot_id, (unsigned)pcie_slot_id);
      if (!GPUINFO_STATIC_FIELD_VALID(static_info, slot_id))
        SET_GPUINFO_STATIC(static_info, slot_id, (unsigned)pcie_slot_id);
    }
  }

  if (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_ub_slot_id)) {
    int ub_slot_id = -1;
    if (ascend_get_ub_slot_id(gpu_info, &ub_slot_id) == DCMI_SUCCESS && ub_slot_id >= 0)
      SET_GPUINFO_STATIC(static_info, ub_slot_id, (unsigned)ub_slot_id);
  }

  if (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_ub_id_info)) {
    struct dcmi_ub_id_info ub_id_info;
    memset(&ub_id_info, 0, sizeof(ub_id_info));
    if (ascend_get_ub_id_info(gpu_info, &ub_id_info) == DCMI_SUCCESS) {
      SET_GPUINFO_STATIC(static_info, ub_device_id, ub_id_info.device_id);
      SET_GPUINFO_STATIC(static_info, ub_vendor_id, ub_id_info.vendor_id);
      SET_GPUINFO_STATIC(static_info, ub_module_vendor_id, ub_id_info.module_vendor_id);
      SET_GPUINFO_STATIC(static_info, ub_module_id, ub_id_info.module_id);
    }
  }

  unsigned chip_slot = 0;
  unsigned chip_id = 0;
  bool chip_id_valid = false;
  if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_slot_id_and_chip_id_by_dev_id)) ||
      DCMI_SYMBOL_PRESENT(dcmi_get_device_chip_slot)) {
    if (ascend_get_chip_location(gpu_info, &chip_slot, &chip_id, &chip_id_valid) == DCMI_SUCCESS) {
      SET_GPUINFO_STATIC(static_info, chip_slot, chip_slot);
      if (chip_id_valid)
        SET_GPUINFO_STATIC(static_info, chip_id, chip_id);
    }
  }

  if (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_group_intra_id_by_dev_id)) {
    unsigned group_intra_id = 0;
    if (ascend_get_group_intra_id(gpu_info, &group_intra_id) == DCMI_SUCCESS)
      SET_GPUINFO_STATIC(static_info, group_intra_id, group_intra_id);
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_first_power_on_date)) {
    unsigned first_power_on_date = 0;
    /* DCMI uses zero when the MCU has not recorded a date yet. */
    if (ascend_get_first_power_on_date(gpu_info, &first_power_on_date) == DCMI_SUCCESS && first_power_on_date != 0)
      SET_GPUINFO_STATIC(static_info, first_power_on_date, first_power_on_date);
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_aicpu_count_info)) {
    unsigned char aicpu_count = 0;
    if (ascend_get_aicpu_count_config(gpu_info, &aicpu_count) == DCMI_SUCCESS)
      SET_GPUINFO_STATIC(static_info, aicpu_count, (unsigned)aicpu_count);
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_cpu_num_config)) {
    unsigned char cpu_config[ASCEND_CPU_NUM_CONFIG_SIZE] = {0};
    if (ascend_get_cpu_num_config(gpu_info, cpu_config, sizeof(cpu_config)) == DCMI_SUCCESS) {
      /* CANN's driver buffer is [Ctrl, Data, AI], despite npu-smi's public
       * display order of AI, Ctrl, Data. */
      SET_GPUINFO_STATIC(static_info, cpu_config_aicpu, (unsigned)cpu_config[2]);
      SET_GPUINFO_STATIC(static_info, cpu_config_ctrlcpu, (unsigned)cpu_config[0]);
      SET_GPUINFO_STATIC(static_info, cpu_config_datacpu, (unsigned)cpu_config[1]);
    }
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_cpu_freq_info)) {
    int cpu_freq_mode = -1;
    if (ascend_get_cpu_freq_mode(gpu_info, &cpu_freq_mode) == DCMI_SUCCESS &&
        (cpu_freq_mode == 0 || cpu_freq_mode == 1))
      SET_GPUINFO_STATIC(static_info, cpu_freq_mode, (unsigned)cpu_freq_mode);
  }

  if (ascend_driver_version_valid) {
    ascend_copy_name(static_info->driver_version, sizeof(static_info->driver_version),
                     (const unsigned char *)ascend_driver_version, sizeof(ascend_driver_version));
    SET_VALID(gpuinfo_driver_version_valid, static_info->valid);
  }
  if (ascend_dcmi_version_valid) {
    ascend_copy_name(static_info->dcmi_version, sizeof(static_info->dcmi_version),
                     (const unsigned char *)ascend_dcmi_version, sizeof(ascend_dcmi_version));
    SET_VALID(gpuinfo_dcmi_version_valid, static_info->valid);
  }

  if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_elabel_info)) ||
      DCMI_SYMBOL_PRESENT(dcmi_get_device_elabel_info)) {
    struct dcmi_elabel_info elabel_info;
    memset(&elabel_info, 0, sizeof(elabel_info));
    if (ascend_get_elabel_info(gpu_info, &elabel_info) == DCMI_SUCCESS) {
      ascend_set_static_string(static_info->elabel_product_name, sizeof(static_info->elabel_product_name),
                               (const unsigned char *)elabel_info.product_name, sizeof(elabel_info.product_name),
                               gpuinfo_elabel_product_name_valid, static_info->valid);
      ascend_set_static_string(static_info->elabel_model, sizeof(static_info->elabel_model),
                               (const unsigned char *)elabel_info.model, sizeof(elabel_info.model),
                               gpuinfo_elabel_model_valid, static_info->valid);
      ascend_set_static_string(static_info->elabel_manufacturer, sizeof(static_info->elabel_manufacturer),
                               (const unsigned char *)elabel_info.manufacturer, sizeof(elabel_info.manufacturer),
                               gpuinfo_elabel_manufacturer_valid, static_info->valid);
      ascend_set_static_string(static_info->elabel_manufacturer_date, sizeof(static_info->elabel_manufacturer_date),
                               (const unsigned char *)elabel_info.manufacturer_date,
                               sizeof(elabel_info.manufacturer_date), gpuinfo_elabel_manufacturer_date_valid,
                               static_info->valid);
      ascend_set_static_string(static_info->elabel_serial_number, sizeof(static_info->elabel_serial_number),
                               (const unsigned char *)elabel_info.serial_number, sizeof(elabel_info.serial_number),
                               gpuinfo_elabel_serial_number_valid, static_info->valid);
    }
  }

  /* Use the driver's component list when available. This avoids assuming a
   * firmware component exists on a product where it is not exposed. */
  if (DCMI_SYMBOL_PRESENT(dcmiv2_get_device_component_static_version) ||
      DCMI_SYMBOL_PRESENT(dcmi_get_device_component_static_version)) {
    enum dcmi_component_type firmware_components[32];
    unsigned component_count = 0;
    int component_status = ascend_get_component_count(gpu_info, &component_count);
    if (component_status == DCMI_SUCCESS && component_count > 0 && component_count <= 32 &&
        ascend_get_component_list(gpu_info, firmware_components, component_count) == DCMI_SUCCESS) {
      for (unsigned i = 0; i < component_count; ++i) {
        unsigned char firmware[MAX_VERSION_STRING];
        memset(firmware, 0, sizeof(firmware));
        if (ascend_get_component_version(gpu_info, firmware_components[i], firmware, sizeof(firmware)) ==
                DCMI_SUCCESS &&
            !ascend_name_is_empty(firmware, sizeof(firmware))) {
          ascend_copy_name(static_info->firmware_version, sizeof(static_info->firmware_version), firmware,
                           sizeof(firmware));
          SET_VALID(gpuinfo_firmware_version_valid, static_info->valid);
          break;
        }
      }
    } else {
      /* Older libraries do not expose component enumeration. Keep a bounded
       * compatibility probe for the component names used by those releases. */
      static const enum dcmi_component_type fallback_components[] = {
          DCMI_COMPONENT_TYPE_HILINK, DCMI_COMPONENT_TYPE_IMU, DCMI_COMPONENT_TYPE_XLOADER};
      for (size_t i = 0; i < sizeof(fallback_components) / sizeof(fallback_components[0]); ++i) {
        unsigned char firmware[MAX_VERSION_STRING];
        memset(firmware, 0, sizeof(firmware));
        if (ascend_get_component_version(gpu_info, fallback_components[i], firmware, sizeof(firmware)) ==
                DCMI_SUCCESS &&
            !ascend_name_is_empty(firmware, sizeof(firmware))) {
          ascend_copy_name(static_info->firmware_version, sizeof(static_info->firmware_version), firmware,
                           sizeof(firmware));
          SET_VALID(gpuinfo_firmware_version_valid, static_info->valid);
          break;
        }
      }
    }
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

  if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_hbm_product_info)) ||
      DCMI_SYMBOL_PRESENT(dcmi_get_device_hbm_product_info)) {
    struct dcmi_hbm_product_info hbm_product_info;
    memset(&hbm_product_info, 0, sizeof(hbm_product_info));
    last_dcmi_return_status = ascend_get_hbm_product_info(gpu_info, &hbm_product_info);
    if (last_dcmi_return_status == DCMI_SUCCESS)
      SET_GPUINFO_STATIC(static_info, hbm_manufacturer_id, hbm_product_info.manufacturer_id);
  }

  ascend_populate_die_id(static_info, gpu_info);
  ascend_populate_vrd_and_affinity(static_info, gpu_info);
  ascend_populate_flash_inventory(static_info, gpu_info);
  ascend_populate_netdevs(static_info, gpu_info);

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
}

static void gpuinfo_ascend_refresh_dynamic_info(struct gpu_info *_gpu_info) {
  struct gpu_info_ascend *gpu_info = container_of(_gpu_info, struct gpu_info_ascend, base);
  struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
  struct ascend_memory_sample memory_sample;
  bool got_aicore_utilization = false;
  bool got_vector_utilization = false;
  bool got_aicube_utilization = false;
  bool got_npu_utilization = false;
  bool got_aicpu_utilization = false;

  memset(dynamic_info, 0, sizeof(*dynamic_info));
  pthread_mutex_lock(&ascend_cache_mutex);
  ascend_apply_slow_status(gpu_info, dynamic_info);
  ascend_apply_extended_status(gpu_info, dynamic_info);
  pthread_mutex_unlock(&ascend_cache_mutex);

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
  if (!got_aicore_current || !got_aicore_max) {
    unsigned frequency = 0;
    if (!got_aicore_current) {
      last_dcmi_return_status = ascend_get_frequency(gpu_info, DCMI_FREQ_AICORE_CURRENT_, &frequency);
      if (last_dcmi_return_status == DCMI_SUCCESS)
        SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, frequency);
    }
    if (!got_aicore_max) {
      last_dcmi_return_status = ascend_get_frequency(gpu_info, DCMI_FREQ_AICORE_MAX, &frequency);
      if (last_dcmi_return_status == DCMI_SUCCESS)
        SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, frequency);
    }
  }

  if (ascend_query_memory(gpu_info, &memory_sample)) {
    /* DCMI memory sizes are documented in MB. */
    unsigned long long multiplier = 1024ULL * 1024ULL;
    unsigned long long total_bytes = ascend_to_bytes(memory_sample.total, multiplier);
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, total_bytes);
    if (memory_sample.has_used) {
      unsigned long long used_bytes = ascend_to_bytes(memory_sample.used, multiplier);
      if (used_bytes > total_bytes)
        used_bytes = total_bytes;
      SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, used_bytes);
      if (memory_sample.has_available) {
        unsigned long long free_bytes = ascend_to_bytes(memory_sample.available, multiplier);
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
    if (memory_sample.source == ASCEND_MEMORY_HBM) {
      if (memory_sample.bandwidth_utilization_valid)
        SET_GPUINFO_DYNAMIC(dynamic_info, mem_bandwidth_util_rate, memory_sample.bandwidth_utilization);
      if (memory_sample.temperature_valid)
        SET_GPUINFO_DYNAMIC(dynamic_info, mem_temp, (unsigned)memory_sample.temperature);
    }

    unsigned bandwidth_utilization = 0;
    int bandwidth_type = memory_sample.source == ASCEND_MEMORY_HBM ? DCMI_UTILIZATION_RATE_HBM_BANDWIDTH
                                                                   : DCMI_UTILIZATION_RATE_DDR_BANDWIDTH;
    if (ascend_query_utilization(gpu_info, bandwidth_type, &bandwidth_utilization))
      SET_GPUINFO_DYNAMIC(dynamic_info, mem_bandwidth_util_rate, bandwidth_utilization);
  }

  unsigned utilization = 0;
  if (ascend_query_utilization(gpu_info, DCMI_UTILIZATION_RATE_AICORE, &utilization)) {
    SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, utilization);
    got_aicore_utilization = true;
  }
  if (ascend_query_utilization(gpu_info, DCMI_UTILIZATION_RATE_VECTORCORE, &utilization)) {
    SET_GPUINFO_DYNAMIC(dynamic_info, vector_util_rate, utilization);
    got_vector_utilization = true;
  }
  if (ascend_query_utilization(gpu_info, DCMI_UTILIZATION_RATE_AICUBE, &utilization)) {
    SET_GPUINFO_DYNAMIC(dynamic_info, aicube_util_rate, utilization);
    got_aicube_utilization = true;
  }
  if (ascend_query_utilization(gpu_info, DCMI_UTILIZATION_RATE_NPU, &utilization)) {
    SET_GPUINFO_DYNAMIC(dynamic_info, npu_util_rate, utilization);
    got_npu_utilization = true;
  }

  if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_multi_utilization_rate)) ||
      DCMI_SYMBOL_PRESENT(dcmi_get_device_multi_utilization_rate)) {
    struct dcmi_multi_utilization_info multi_utilization;
    memset(&multi_utilization, 0, sizeof(multi_utilization));
    last_dcmi_return_status = ascend_get_multi_utilization(gpu_info, &multi_utilization);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      if (!got_aicore_utilization) {
        if (ascend_percentage_is_valid(multi_utilization.aicore_util)) {
          SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, multi_utilization.aicore_util);
          got_aicore_utilization = true;
        }
      }
      if (!got_vector_utilization && ascend_percentage_is_valid(multi_utilization.aiv_util))
        SET_GPUINFO_DYNAMIC(dynamic_info, vector_util_rate, multi_utilization.aiv_util);
      if (!got_aicube_utilization && ascend_percentage_is_valid(multi_utilization.aic_util))
        SET_GPUINFO_DYNAMIC(dynamic_info, aicube_util_rate, multi_utilization.aic_util);
      if (!got_npu_utilization && ascend_percentage_is_valid(multi_utilization.npu_util))
        SET_GPUINFO_DYNAMIC(dynamic_info, npu_util_rate, multi_utilization.npu_util);
    }
  }

  if ((ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_aicpu_info)) ||
      (ascend_has_legacy_ids(gpu_info) &&
       (DCMI_SYMBOL_PRESENT(dcmi_get_device_aicpu_info) || DCMI_SYMBOL_PRESENT(dcmi_get_aicpu_info)))) {
    struct dcmi_aicpu_info aicpu_info;
    memset(&aicpu_info, 0, sizeof(aicpu_info));
    last_dcmi_return_status = ascend_get_aicpu_info(gpu_info, &aicpu_info);
    if (last_dcmi_return_status == DCMI_SUCCESS) {
      if (aicpu_info.cur_freq > 0)
        SET_GPUINFO_DYNAMIC(dynamic_info, aicpu_clock_speed, aicpu_info.cur_freq);
      if (aicpu_info.max_freq > 0)
        SET_GPUINFO_DYNAMIC(dynamic_info, aicpu_clock_speed_max, aicpu_info.max_freq);
      unsigned count = aicpu_info.aicpu_num > MAX_CORE_NUM ? MAX_CORE_NUM : aicpu_info.aicpu_num;
      if (count > 0) {
        unsigned long long total = 0;
        for (unsigned i = 0; i < count; ++i)
          total += aicpu_info.util_rate[i] > 100 ? 100 : aicpu_info.util_rate[i];
        SET_GPUINFO_DYNAMIC(dynamic_info, aicpu_util_rate, (unsigned)(total / count));
        got_aicpu_utilization = true;
      }
    }
  }
  if (!got_aicpu_utilization && ascend_query_utilization(gpu_info, DCMI_UTILIZATION_RATE_AICPU, &utilization))
    SET_GPUINFO_DYNAMIC(dynamic_info, aicpu_util_rate, utilization);
  if (ascend_query_utilization(gpu_info, DCMI_UTILIZATION_RATE_CTRLCPU, &utilization)) {
    SET_GPUINFO_DYNAMIC(dynamic_info, ctrlcpu_util_rate, utilization);
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

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_voltage) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_voltage))) {
    unsigned voltage = 0;
    last_dcmi_return_status = ascend_get_voltage(gpu_info, &voltage);
    if (last_dcmi_return_status == DCMI_SUCCESS && ascend_voltage_is_valid(voltage) && voltage <= UINT_MAX / 10)
      /* DCMI voltage has a 0.01 V resolution; nvtop stores millivolts. */
      SET_GPUINFO_DYNAMIC(dynamic_info, voltage, voltage * 10);
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_health) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_health))) {
    unsigned health = 0;
    last_dcmi_return_status = ascend_get_health(gpu_info, &health);
    if (last_dcmi_return_status == DCMI_SUCCESS && health <= 3)
      SET_GPUINFO_DYNAMIC(dynamic_info, health, health);
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_driver_health) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_driver_health))) {
    unsigned driver_health = 0;
    last_dcmi_return_status = ascend_get_driver_health(&driver_health);
    if (last_dcmi_return_status == DCMI_SUCCESS && driver_health <= 3)
      SET_GPUINFO_DYNAMIC(dynamic_info, driver_health, driver_health);
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_device_errorcode_v2) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_device_error_code_list))) {
    int error_count = 0;
    unsigned error_codes[GPUINFO_MAX_ERROR_CODES] = {0};
    last_dcmi_return_status = ascend_get_error_codes(gpu_info, &error_count, error_codes, GPUINFO_MAX_ERROR_CODES);
    if (last_dcmi_return_status == DCMI_SUCCESS && error_count >= 0) {
      unsigned count =
          (unsigned)error_count > GPUINFO_MAX_ERROR_CODES ? GPUINFO_MAX_ERROR_CODES : (unsigned)error_count;
      dynamic_info->error_code_count = count;
      memcpy(dynamic_info->error_codes, error_codes, count * sizeof(error_codes[0]));
      SET_VALID(gpuinfo_error_codes_valid, dynamic_info->valid);
    }
  }

  if (DCMI_SYMBOL_PRESENT(dcmi_get_driver_errorcode) ||
      (ascend_use_dcmiv2 && DCMI_SYMBOL_PRESENT(dcmiv2_get_driver_error_code_list))) {
    int error_count = 0;
    unsigned error_codes[GPUINFO_MAX_ERROR_CODES] = {0};
    last_dcmi_return_status = ascend_get_driver_error_codes(&error_count, error_codes, GPUINFO_MAX_ERROR_CODES);
    if (last_dcmi_return_status == DCMI_SUCCESS && error_count >= 0) {
      unsigned count =
          (unsigned)error_count > GPUINFO_MAX_ERROR_CODES ? GPUINFO_MAX_ERROR_CODES : (unsigned)error_count;
      dynamic_info->driver_error_code_count = count;
      memcpy(dynamic_info->driver_error_codes, error_codes, count * sizeof(error_codes[0]));
      SET_VALID(gpuinfo_driver_error_codes_valid, dynamic_info->valid);
    }
  }

  ascend_refresh_ecc(gpu_info, dynamic_info);
  ascend_refresh_pcie_errors(gpu_info, dynamic_info);
  ascend_refresh_dvpp(gpu_info, dynamic_info);
  ascend_refresh_fan(gpu_info, dynamic_info);
  ascend_refresh_pcie_link(&gpu_info->base, dynamic_info);
  pthread_mutex_lock(&ascend_cache_mutex);
  ascend_apply_pcie_bandwidth(gpu_info, dynamic_info);
  pthread_mutex_unlock(&ascend_cache_mutex);
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
