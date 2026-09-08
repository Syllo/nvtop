/*
 *
 * Copyright (C) 2021-2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
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
 *
 */

#ifndef EXTRACT_GPUINFO_COMMON_H__
#define EXTRACT_GPUINFO_COMMON_H__

#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/types.h>

#include "list.h"

#define STRINGIFY(x) STRINGIFY_HELPER_(x)
#define STRINGIFY_HELPER_(x) #x

#define IS_VALID(x, y) ((y)[(x) / CHAR_BIT] & (1 << ((x) % CHAR_BIT)))
#define SET_VALID(x, y) ((y)[(x) / CHAR_BIT] |= (1 << ((x) % CHAR_BIT)))
#define RESET_VALID(x, y) ((y)[(x) / CHAR_BIT] &= ~(1 << ((x) % CHAR_BIT)))
#define RESET_ALL(y) memset(y, 0, sizeof(y))

#define SET_VALUE(structPtr, field, value, prefix)                                                                     \
  do {                                                                                                                 \
    (structPtr)->field = (value);                                                                                      \
    SET_VALID(prefix##field##_valid, (structPtr)->valid);                                                              \
  } while (0)
#define INVALIDATE_VALUE(structPtr, field, prefix)                                                                     \
  do {                                                                                                                 \
    RESET_VALID(prefix##field##_valid, (structPtr)->valid);                                                            \
  } while (0)
#define VALUE_IS_VALID(structPtr, field, prefix) IS_VALID(prefix##field##_valid, (structPtr)->valid)

#define SET_GPUINFO_STATIC(structPtr, field, value) SET_VALUE(structPtr, field, value, gpuinfo_)
#define RESET_GPUINFO_STATIC(structPtr, field) INVALIDATE_VALUE(structPtr, field, gpuinfo_)
#define GPUINFO_STATIC_FIELD_VALID(structPtr, field) VALUE_IS_VALID(structPtr, field, gpuinfo_)
enum gpuinfo_static_info_valid {
  gpuinfo_device_name_valid = 0,
  gpuinfo_memory_type_valid,
  gpuinfo_max_pcie_gen_valid,
  gpuinfo_max_pcie_link_width_valid,
  gpuinfo_temperature_shutdown_threshold_valid,
  gpuinfo_temperature_slowdown_threshold_valid,
  gpuinfo_n_shared_cores_valid,
  gpuinfo_l2cache_size_valid,
  gpuinfo_n_exec_engines_valid,
  gpuinfo_engine_count_valid,
  gpuinfo_chip_type_valid,
  gpuinfo_chip_version_valid,
  gpuinfo_npu_name_valid,
  gpuinfo_board_id_valid,
  gpuinfo_pcb_id_valid,
  gpuinfo_bom_id_valid,
  gpuinfo_slot_id_valid,
  gpuinfo_mainboard_id_valid,
  gpuinfo_pcie_slot_id_valid,
  gpuinfo_ub_slot_id_valid,
  gpuinfo_ub_device_id_valid,
  gpuinfo_ub_vendor_id_valid,
  gpuinfo_ub_module_vendor_id_valid,
  gpuinfo_ub_module_id_valid,
  gpuinfo_chip_id_valid,
  gpuinfo_chip_slot_valid,
  gpuinfo_group_intra_id_valid,
  gpuinfo_first_power_on_date_valid,
  gpuinfo_aicpu_count_valid,
  gpuinfo_cpu_config_aicpu_valid,
  gpuinfo_cpu_config_ctrlcpu_valid,
  gpuinfo_cpu_config_datacpu_valid,
  gpuinfo_cpu_freq_mode_valid,
  gpuinfo_driver_version_valid,
  gpuinfo_dcmi_version_valid,
  gpuinfo_firmware_version_valid,
  gpuinfo_hbm_manufacturer_id_valid,
  gpuinfo_elabel_product_name_valid,
  gpuinfo_elabel_model_valid,
  gpuinfo_elabel_manufacturer_valid,
  gpuinfo_elabel_manufacturer_date_valid,
  gpuinfo_elabel_serial_number_valid,
  gpuinfo_die_id_valid,
  gpuinfo_vrd_version_valid,
  gpuinfo_affinity_cpu_valid,
  gpuinfo_flash_inventory_valid,
  gpuinfo_netdev_names_valid,
  gpuinfo_static_info_count,
};

#define MAX_DEVICE_NAME 128
#define MAX_DEVICE_METADATA 32
#define MAX_VERSION_STRING 256
#define GPUINFO_MAX_ERROR_CODES 16
#define GPUINFO_MAX_FAULT_EVENTS 8
#define GPUINFO_MAX_FLASHES 8
#define GPUINFO_MAX_NETDEVS 8
#define GPUINFO_NETDEV_NAME_LEN 16
#define GPUINFO_MAX_ECC_RECORDS 64

struct gpuinfo_fault_event {
  unsigned int event_id;
  unsigned int severity;
  unsigned int assertion;
  unsigned long long alarm_raised_time;
  char event_name[MAX_VERSION_STRING];
  char additional_info[64];
};

struct gpuinfo_flash_info {
  unsigned long long flash_id;
  unsigned short device_id;
  unsigned short vendor;
  unsigned int state;
  unsigned long long size;
  unsigned int sector_count;
  unsigned short manufacturer_id;
};

struct gpuinfo_static_info {
  char device_name[MAX_DEVICE_NAME];
  char memory_type[8];
  char chip_type[MAX_DEVICE_METADATA];
  char chip_version[MAX_DEVICE_METADATA];
  char npu_name[MAX_DEVICE_METADATA];
  char driver_version[MAX_VERSION_STRING];
  char dcmi_version[MAX_VERSION_STRING];
  char firmware_version[MAX_VERSION_STRING];
  char elabel_product_name[MAX_VERSION_STRING];
  char elabel_model[MAX_VERSION_STRING];
  char elabel_manufacturer[MAX_VERSION_STRING];
  char elabel_manufacturer_date[MAX_VERSION_STRING];
  char elabel_serial_number[MAX_VERSION_STRING];
  char die_id[MAX_VERSION_STRING];
  char vrd_version[MAX_VERSION_STRING];
  char affinity_cpu[MAX_VERSION_STRING];
  unsigned board_id;
  unsigned pcb_id;
  unsigned bom_id;
  unsigned slot_id;
  unsigned mainboard_id;
  unsigned pcie_slot_id;
  unsigned ub_slot_id;
  unsigned ub_device_id;
  unsigned ub_vendor_id;
  unsigned ub_module_vendor_id;
  unsigned ub_module_id;
  unsigned chip_id;
  unsigned chip_slot;
  unsigned group_intra_id;
  unsigned first_power_on_date;
  unsigned aicpu_count;
  unsigned cpu_config_aicpu;
  unsigned cpu_config_ctrlcpu;
  unsigned cpu_config_datacpu;
  unsigned cpu_freq_mode;
  unsigned hbm_manufacturer_id;
  unsigned flash_count;
  unsigned flash_inventory_count;
  struct gpuinfo_flash_info flashes[GPUINFO_MAX_FLASHES];
  unsigned netdev_count;
  char netdev_names[GPUINFO_MAX_NETDEVS][GPUINFO_NETDEV_NAME_LEN];
  unsigned max_pcie_gen;
  unsigned max_pcie_link_width;
  unsigned temperature_shutdown_threshold;
  unsigned temperature_slowdown_threshold;
  unsigned n_shared_cores;
  unsigned l2cache_size;
  unsigned n_exec_engines;
  unsigned engine_count;
  bool integrated_graphics;
  bool encode_decode_shared;
  unsigned char valid[(gpuinfo_static_info_count + CHAR_BIT - 1) / CHAR_BIT];
};

#define SET_GPUINFO_DYNAMIC(structPtr, field, value) SET_VALUE(structPtr, field, value, gpuinfo_)
#define RESET_GPUINFO_DYNAMIC(structPtr, field) INVALIDATE_VALUE(structPtr, field, gpuinfo_)
#define GPUINFO_DYNAMIC_FIELD_VALID(structPtr, field) VALUE_IS_VALID(structPtr, field, gpuinfo_)
enum gpuinfo_dynamic_info_valid {
  gpuinfo_gpu_clock_speed_valid = 0,
  gpuinfo_gpu_clock_speed_max_valid,
  gpuinfo_mem_clock_speed_valid,
  gpuinfo_mem_clock_speed_max_valid,
  gpuinfo_gpu_util_rate_valid,
  gpuinfo_mem_util_rate_valid,
  gpuinfo_encoder_rate_valid,
  gpuinfo_decoder_rate_valid,
  gpuinfo_total_memory_valid,
  gpuinfo_free_memory_valid,
  gpuinfo_used_memory_valid,
  gpuinfo_pcie_link_gen_valid,
  gpuinfo_pcie_link_width_valid,
  gpuinfo_pcie_rx_valid,
  gpuinfo_pcie_tx_valid,
  gpuinfo_fan_speed_valid,
  gpuinfo_fan_rpm_valid,
  gpuinfo_gpu_temp_valid,
  gpuinfo_power_draw_valid,
  gpuinfo_power_draw_max_valid,
  gpuinfo_effective_load_rate_valid,
  gpuinfo_multi_instance_mode_valid,
  gpuinfo_aicpu_clock_speed_valid,
  gpuinfo_aicpu_clock_speed_max_valid,
  gpuinfo_aicpu_util_rate_valid,
  gpuinfo_ctrlcpu_util_rate_valid,
  gpuinfo_vector_util_rate_valid,
  gpuinfo_aicube_util_rate_valid,
  gpuinfo_npu_util_rate_valid,
  gpuinfo_mem_bandwidth_util_rate_valid,
  gpuinfo_mem_temp_valid,
  gpuinfo_voltage_valid,
  gpuinfo_health_valid,
  gpuinfo_driver_health_valid,
  gpuinfo_boot_status_valid,
  gpuinfo_compatibility_valid,
  gpuinfo_network_health_valid,
  gpuinfo_outband_channel_state_valid,
  gpuinfo_device_share_enabled_valid,
  gpuinfo_p2p_enabled_valid,
  gpuinfo_cgroup_memory_limit_valid,
  gpuinfo_cgroup_memory_usage_valid,
  gpuinfo_cgroup_memory_max_usage_valid,
  gpuinfo_llc_read_hit_rate_valid,
  gpuinfo_llc_write_hit_rate_valid,
  gpuinfo_llc_throughput_valid,
  gpuinfo_error_codes_valid,
  gpuinfo_driver_error_codes_valid,
  gpuinfo_ecc_hbm_single_bit_errors_valid,
  gpuinfo_ecc_hbm_double_bit_errors_valid,
  gpuinfo_ecc_hbm_total_single_bit_errors_valid,
  gpuinfo_ecc_hbm_total_double_bit_errors_valid,
  gpuinfo_ecc_hbm_single_bit_isolated_pages_valid,
  gpuinfo_ecc_hbm_double_bit_isolated_pages_valid,
  gpuinfo_ecc_ddr_single_bit_errors_valid,
  gpuinfo_ecc_ddr_double_bit_errors_valid,
  gpuinfo_ecc_ddr_total_single_bit_errors_valid,
  gpuinfo_ecc_ddr_total_double_bit_errors_valid,
  gpuinfo_ecc_ddr_single_bit_isolated_pages_valid,
  gpuinfo_ecc_ddr_double_bit_isolated_pages_valid,
  gpuinfo_pcie_pcs_rx_error_count_valid,
  gpuinfo_pcie_phy_lane_error_count_valid,
  gpuinfo_pcie_symbol_unlock_error_count_valid,
  gpuinfo_pcie_lcrc_error_count_valid,
  gpuinfo_pcie_dcrc_error_count_valid,
  gpuinfo_pcie_link_tx_error_count_valid,
  gpuinfo_pcie_link_rx_error_count_valid,
  gpuinfo_pcie_link_lcrc_error_count_valid,
  gpuinfo_pcie_link_ecrc_error_count_valid,
  gpuinfo_pcie_link_retry_count_valid,
  gpuinfo_hccs_tx_bandwidth_valid,
  gpuinfo_hccs_rx_bandwidth_valid,
  gpuinfo_ub_link_status_valid,
  gpuinfo_ub_tx_bandwidth_valid,
  gpuinfo_ub_rx_bandwidth_valid,
  gpuinfo_rdma_tx_bandwidth_valid,
  gpuinfo_rdma_rx_bandwidth_valid,
  gpuinfo_network_tx_packets_valid,
  gpuinfo_network_rx_packets_valid,
  gpuinfo_network_tx_bytes_valid,
  gpuinfo_network_rx_bytes_valid,
  gpuinfo_network_tx_errors_valid,
  gpuinfo_network_rx_errors_valid,
  gpuinfo_network_rx_fcs_errors_valid,
  gpuinfo_device_system_time_valid,
  gpuinfo_ecc_hbm_history_count_valid,
  gpuinfo_ecc_ddr_history_count_valid,
  gpuinfo_ub_port_id_valid,
  gpuinfo_network_tc_stats_valid,
  gpuinfo_fault_events_valid,
  gpuinfo_dynamic_info_count,
};

struct gpuinfo_dynamic_info {
  unsigned int gpu_clock_speed;     // Device clock speed in MHz
  unsigned int gpu_clock_speed_max; // Maximum clock speed in MHz
  unsigned int mem_clock_speed;     // Device clock speed in MHz
  unsigned int mem_clock_speed_max; // Maximum clock speed in MHz
  unsigned int gpu_util_rate;       // GPU utilization rate in %
  unsigned int mem_util_rate;       // MEM utilization rate in %
  unsigned int effective_load_rate; // Effective load rate in %
  unsigned int encoder_rate;        // Encoder utilization rate in %
  unsigned int decoder_rate;        // Decoder utilization rate in %
  unsigned long long total_memory;  // Total memory (bytes)
  unsigned long long free_memory;   // Unallocated memory (bytes)
  unsigned long long used_memory;   // Allocated memory (bytes)
  unsigned int pcie_link_gen;       // PCIe link generation used
  unsigned int pcie_link_width;     // PCIe line width used
  unsigned int pcie_rx;             // PCIe throughput in KB/s
  unsigned int pcie_tx;             // PCIe throughput in KB/s
  unsigned int fan_speed;           // Fan speed percentage
  unsigned int fan_rpm;             // Fan speed RPM
  unsigned int gpu_temp;            // GPU temperature °celsius
  unsigned int power_draw;          // Power usage in milliwatts
  unsigned int power_draw_max;      // Max power usage in milliwatts
  bool multi_instance_mode;          // True if the GPU is in multi-instance mode
  unsigned int aicpu_clock_speed;    // AICPU current clock speed in MHz
  unsigned int aicpu_clock_speed_max;         // AICPU maximum clock speed in MHz
  unsigned int aicpu_util_rate;               // AICPU utilization rate in %
  unsigned int ctrlcpu_util_rate;             // Control CPU utilization rate in %
  unsigned int vector_util_rate;              // VectorCore utilization rate in %
  unsigned int aicube_util_rate;              // AICube utilization rate in %
  unsigned int npu_util_rate;                 // Aggregate NPU utilization rate in %
  unsigned int mem_bandwidth_util_rate;       // Memory bandwidth utilization rate in %
  unsigned int mem_temp;                      // HBM/memory temperature in Celsius
  unsigned int voltage;                       // Device voltage in millivolts
  unsigned int health;                        // DCMI health level: 0 normal, 1 warning, 2 major, 3 critical
  unsigned int driver_health;                 // DCMI driver health level
  unsigned int boot_status;                   // DCMI device boot state
  unsigned int compatibility;                 // DCMI driver/firmware compatibility state
  unsigned int network_health;                // DCMI network health result
  unsigned int outband_channel_state;         // DCMI out-of-band channel state
  bool device_share_enabled;                  // DCMI container/device sharing flag
  bool p2p_enabled;                           // DCMI flash P2P enable flag
  unsigned long long cgroup_memory_limit;     // cgroup memory limit (bytes)
  unsigned long long cgroup_memory_usage;     // cgroup memory usage (bytes)
  unsigned long long cgroup_memory_max_usage; // cgroup peak memory usage (bytes)
  unsigned int llc_read_hit_rate;             // LLC read hit rate (%)
  unsigned int llc_write_hit_rate;            // LLC write hit rate (%)
  unsigned int llc_throughput;                // LLC throughput (KB/s)
  unsigned int error_code_count;
  unsigned int error_codes[GPUINFO_MAX_ERROR_CODES];
  unsigned int driver_error_code_count;
  unsigned int driver_error_codes[GPUINFO_MAX_ERROR_CODES];
  unsigned int ecc_hbm_single_bit_errors;
  unsigned int ecc_hbm_double_bit_errors;
  unsigned int ecc_hbm_total_single_bit_errors;
  unsigned int ecc_hbm_total_double_bit_errors;
  unsigned int ecc_hbm_single_bit_isolated_pages;
  unsigned int ecc_hbm_double_bit_isolated_pages;
  unsigned int ecc_ddr_single_bit_errors;
  unsigned int ecc_ddr_double_bit_errors;
  unsigned int ecc_ddr_total_single_bit_errors;
  unsigned int ecc_ddr_total_double_bit_errors;
  unsigned int ecc_ddr_single_bit_isolated_pages;
  unsigned int ecc_ddr_double_bit_isolated_pages;
  unsigned int pcie_pcs_rx_error_count;
  unsigned int pcie_phy_lane_error_count;
  unsigned int pcie_symbol_unlock_error_count;
  unsigned int pcie_lcrc_error_count;
  unsigned int pcie_dcrc_error_count;
  unsigned int pcie_link_tx_error_count;
  unsigned int pcie_link_rx_error_count;
  unsigned int pcie_link_lcrc_error_count;
  unsigned int pcie_link_ecrc_error_count;
  unsigned int pcie_link_retry_count;
  double hccs_tx_bandwidth;       // HCCS aggregate transmit bandwidth in GB/s
  double hccs_rx_bandwidth;       // HCCS aggregate receive bandwidth in GB/s
  unsigned int ub_link_status;    // DCMI UB whole-chip link state
  double ub_tx_bandwidth;         // Aggregate UB transmit bandwidth in MB/s
  double ub_rx_bandwidth;         // Aggregate UB receive bandwidth in MB/s
  unsigned int rdma_tx_bandwidth; // RDMA transmit bandwidth in MB/s
  unsigned int rdma_rx_bandwidth; // RDMA receive bandwidth in MB/s
  unsigned long long network_tx_packets;
  unsigned long long network_rx_packets;
  unsigned long long network_tx_bytes;
  unsigned long long network_rx_bytes;
  unsigned long long network_tx_errors;
  unsigned long long network_rx_errors;
  unsigned long long network_rx_fcs_errors;
  unsigned int device_system_time;
  unsigned int ecc_hbm_history_count;
  unsigned int ecc_hbm_last_error_time;
  unsigned int ecc_ddr_history_count;
  unsigned int ecc_ddr_last_error_time;
  unsigned int ub_port_id;
  unsigned long long ub_port_tx_packets;
  unsigned long long ub_port_rx_packets;
  unsigned long long ub_port_tx_errors;
  unsigned long long ub_port_rx_errors;
  unsigned long long ub_port_crc_errors;
  unsigned long long network_tc_tx_packets;
  unsigned long long network_tc_rx_packets;
  unsigned int fault_event_count;
  struct gpuinfo_fault_event fault_events[GPUINFO_MAX_FAULT_EVENTS];
  unsigned char valid[(gpuinfo_dynamic_info_count + CHAR_BIT - 1) / CHAR_BIT];
};

enum gpu_process_type {
  gpu_process_unknown = 0,
  gpu_process_graphical = 1,
  gpu_process_compute = 2,
  gpu_process_graphical_compute = 3,
  gpu_process_type_count,
};

#define SET_GPUINFO_PROCESS(structPtr, field, value) SET_VALUE(structPtr, field, value, gpuinfo_process_)
#define RESET_GPUINFO_PROCESS(structPtr, field) INVALIDATE_VALUE(structPtr, field, gpuinfo_process_)
#define GPUINFO_PROCESS_FIELD_VALID(structPtr, field) VALUE_IS_VALID(structPtr, field, gpuinfo_process_)
enum gpuinfo_process_info_valid {
  gpuinfo_process_cmdline_valid,
  gpuinfo_process_user_name_valid,
  gpuinfo_process_gfx_engine_used_valid,
  gpuinfo_process_compute_engine_used_valid,
  gpuinfo_process_enc_engine_used_valid,
  gpuinfo_process_dec_engine_used_valid,
  gpuinfo_process_gpu_usage_valid,
  gpuinfo_process_encode_usage_valid,
  gpuinfo_process_decode_usage_valid,
  gpuinfo_process_gpu_memory_usage_valid,
  gpuinfo_process_gpu_memory_percentage_valid,
  gpuinfo_process_cpu_usage_valid,
  gpuinfo_process_cpu_memory_virt_valid,
  gpuinfo_process_cpu_memory_res_valid,
  gpuinfo_process_gpu_cycles_valid,
  gpuinfo_process_sample_delta_valid,
  gpuinfo_process_info_count
};

struct gpu_process {
  enum gpu_process_type type;
  pid_t pid;                           // Process ID
  char *cmdline;                       // Process User Name
  char *user_name;                     // Process User Name
  uint64_t sample_delta;               // Time spent between two successive samples
  uint64_t gfx_engine_used;            // Time in nanoseconds this process spent using the GPU gfx
  uint64_t compute_engine_used;        // Time in nanoseconds this process spent using the GPU compute
  uint64_t enc_engine_used;            // Time in nanoseconds this process spent using the GPU encoder
  uint64_t dec_engine_used;            // Time in nanoseconds this process spent using the GPU decoder
  uint64_t gpu_cycles;                 // Number of GPU cycles spent in the GPU gfx engine
  unsigned gpu_usage;                  // Percentage of GPU used by the process
  unsigned encode_usage;               // Percentage of GPU encoder used by the process
  unsigned decode_usage;               // Percentage of GPU decoder used by the process
  unsigned long long gpu_memory_usage; // Memory used by the process
  unsigned gpu_memory_percentage;      // Percentage of the total device memory
                                       // consumed by the process
  unsigned cpu_usage;
  unsigned long cpu_memory_virt;
  unsigned long cpu_memory_res;
  unsigned char valid[(gpuinfo_process_info_count + CHAR_BIT - 1) / CHAR_BIT];
};

struct gpu_info;

struct gpu_vendor {
  struct list_head list;

  bool (*init)(void);
  void (*shutdown)(void);

  const char *(*last_error_string)(void);

  bool (*get_device_handles)(struct list_head *devices, unsigned *count);

  void (*populate_static_info)(struct gpu_info *gpu_info);
  void (*refresh_dynamic_info)(struct gpu_info *gpu_info);
  void (*refresh_utilisation_rate)(struct gpu_info *gpu_info);

  void (*refresh_running_processes)(struct gpu_info *gpu_info);
  char *name;
};

#define PDEV_LEN 16
struct gpu_info {
  struct list_head list;
  struct gpu_vendor *vendor;
  struct gpuinfo_static_info static_info;
  struct gpuinfo_dynamic_info dynamic_info;
  unsigned processes_count;
  struct gpu_process *processes;
  unsigned processes_array_size;
  char pdev[PDEV_LEN];
};

void register_gpu_vendor(struct gpu_vendor *vendor);

bool extract_drm_fdinfo_key_value(char *buf, char **key, char **val);

void gpuinfo_refresh_utilisation_rate(struct gpu_info *gpu_info);

// fdinfo DRM interface names common to multiple drivers
extern const char drm_pdev[];
extern const char drm_client_id[];

inline unsigned busy_usage_from_time_usage_round(uint64_t current_use_ns, uint64_t previous_use_ns,
                                                 uint64_t time_between_measurement) {
  return ((current_use_ns - previous_use_ns) * UINT64_C(100) + time_between_measurement / UINT64_C(2)) /
         time_between_measurement;
}

unsigned nvtop_pcie_gen_from_link_speed(unsigned linkSpeed);

// NVLink support
#define NVTOP_NVLINK_MAX_LINKS 36

struct nvlink_info {
  unsigned num_links;                   // Number of NVLink links on this device
  unsigned version;                     // NVLink version (e.g. 3 for NVLink 3.0)
  bool supported;                       // NVLink is supported on this device
  bool has_throughput;                  // Whether throughput data was available this cycle
  unsigned long long aggregate_tx;      // Aggregate TX throughput across all links (KiB/s)
  unsigned long long aggregate_rx;      // Aggregate RX throughput across all links (KiB/s)
  unsigned long long total_errors;      // Cumulative-since-launch flit CRC errors across all links
  unsigned long long total_corrections; // Cumulative-since-launch CRC data errors across all links
  unsigned long long total_ecc_errors;  // Cumulative-since-launch ECC data errors across all links
};

unsigned nvtop_get_nvlink_info(struct gpu_info *gpu_info, struct nvlink_info *nvlink_info);

// Get display-ready NVLink flit CRC / CRC data / ECC counts from the per-device
// persistent struct. Returns true if a baseline has been established at least once.
bool nvtop_get_nvlink_error_counts(struct gpu_info *gpu_info, unsigned long long *out_errors,
                                   unsigned long long *out_corrections, unsigned long long *out_ecc);

// NVLink probe — call before initialize_curses to set layout mode
bool nvtop_probe_nvlink_list(struct list_head *devices);

// Reset per-GPU NVLink cache (probed flag, cached linkcount/version, cached info struct).
// Call when the monitored device set changes so newly-monitored NVLink GPUs get probed fresh.
void nvtop_reset_nvlink_cache(struct gpu_info *gpu_info);

#endif // EXTRACT_GPUINFO_COMMON_H__
