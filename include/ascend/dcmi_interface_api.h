/* Copyright(C) 2021-2023. Huawei Technologies Co.,Ltd. All rights reserved.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#ifndef __DCMI_INTERFACE_API_H__
#define __DCMI_INTERFACE_API_H__

#ifdef __cplusplus
#if __cplusplus
extern "C" {
#endif
#endif /* __cplusplus */

#ifdef __linux__
#define DCMIDLLEXPORT
#else
#define DCMIDLLEXPORT _declspec(dllexport)
#endif

/* Newer DCMI entry points are optional so older Ascend drivers remain usable. */
#if defined(__GNUC__)
#define DCMI_WEAK __attribute__((weak))
#else
#define DCMI_WEAK
#endif

#define MAX_CARD_NUM 64
#define MAX_CHIP_NAME_LEN 32  // Maximum length of chip name
#define MAX_VERSION_LEN 255
#define MAX_LENTH 256
#define MAX_CORE_NUM 16
#define DCMI_CPU_NUM_CFG_LEN 16
#define TEMPLATE_NAME_LEN 32
#define DIE_ID_COUNT 5  // Number of die ID characters
#define DCMI_HCCS_MAX_PCS_NUM 16
#define DCMI_UB_PORT_NUM 36
#define MAX_DIE_NUMS 2
#define MAX_PORT_NUMS 32
#define NETDEV_MAX_NUM 8
#define NETDEV_NAME_MAX_LEN 16
#define TC_MAX_NUM 8
#define DCMI_PORT_PKT_STATS_NUM 48
#define MAX_RECORD_ECC_ADDR_COUNT 64
#define UB_MODE 0
#define UBOE_MODE 1

struct dsmi_computing_power_info;

#define    DCMI_UTILIZATION_RATE_DDR             1
#define    DCMI_UTILIZATION_RATE_AICORE          2
#define    DCMI_UTILIZATION_RATE_AICPU           3
#define    DCMI_UTILIZATION_RATE_CTRLCPU         4
#define    DCMI_UTILIZATION_RATE_DDR_BANDWIDTH   5
#define    DCMI_UTILIZATION_RATE_HBM             6
#define    DCMI_UTILIZATION_RATE_HBM_BANDWIDTH   10
#define    DCMI_UTILIZATION_RATE_VECTORCORE      12
#define DCMI_UTILIZATION_RATE_NPU 13
#define DCMI_UTILIZATION_RATE_AICUBE 14

/*----------------------------------------------*
 * Structure description                        *
 *----------------------------------------------*/
struct dcmi_chip_info {
  unsigned char chip_type[MAX_CHIP_NAME_LEN];
  unsigned char chip_name[MAX_CHIP_NAME_LEN];
  unsigned char chip_ver[MAX_CHIP_NAME_LEN];
  unsigned int aicore_cnt;
};

struct dcmi_chip_info_v2 {
  unsigned char chip_type[MAX_CHIP_NAME_LEN];
  unsigned char chip_name[MAX_CHIP_NAME_LEN];
  unsigned char chip_ver[MAX_CHIP_NAME_LEN];
  unsigned int aicore_cnt;
  unsigned char npu_name[MAX_CHIP_NAME_LEN];
};

struct dcmi_pcie_info {
  unsigned int deviceid;
  unsigned int venderid;
  unsigned int subvenderid;
  unsigned int subdeviceid;
  unsigned int bdf_deviceid;
  unsigned int bdf_busid;
  unsigned int bdf_funcid;
};

  struct dcmi_pcie_info_all {
    unsigned int venderid;
    unsigned int subvenderid;
    unsigned int deviceid;
    unsigned int subdeviceid;
    int domain;
    unsigned int bdf_busid;
    unsigned int bdf_deviceid;
    unsigned int bdf_funcid;
    unsigned char reserve[32];       /* the size of dcmi_pcie_info_all is 64 */
  };

  struct dcmi_ub_id_info {
    unsigned short device_id;
    unsigned short vendor_id;
    unsigned short module_vendor_id;
    unsigned short module_id;
    unsigned char reserved[32];
  };

  struct dcmi_die_id {
    unsigned int soc_die[DIE_ID_COUNT];
  };

  struct dcmi_aicore_info {
    unsigned int freq;
    unsigned int cur_freq;
  };

  struct dcmi_aicpu_info {
    unsigned int max_freq;
    unsigned int cur_freq;
    unsigned int aicpu_num;
    unsigned int util_rate[MAX_CORE_NUM];
  };

  /* V1 compatibility structures used by the legacy alias entry points.  The
   * fields intentionally keep the names from the CANN ABI; their layout is
   * identical to the dcmi_* structures above. */
  struct dsmi_aicore_info_stru {
    unsigned int freq;
    unsigned int curfreq;
  };

  struct dsmi_aicpu_info_stru {
    unsigned int maxFreq;
    unsigned int curFreq;
    unsigned int aicpuNum;
    unsigned int utilRate[MAX_CORE_NUM];
  };

  struct dcmi_multi_utilization_info {
    unsigned int aic_util;
    unsigned int aiv_util;
    unsigned int aicore_util;
    unsigned int npu_util;
    unsigned int reserved[8];
  };

  enum dcmi_device_type {
    DCMI_DEVICE_TYPE_DDR = 0,
    DCMI_DEVICE_TYPE_SRAM,
    DCMI_DEVICE_TYPE_HBM,
    DCMI_DEVICE_TYPE_NPU,
    DCMI_HBM_RECORDED_SINGLE_ADDR,
    DCMI_HBM_RECORDED_MULTI_ADDR,
    DCMI_DEVICE_TYPE_NONE = 0xff
  };

  struct dcmi_ecc_info {
    int enable_flag;
    unsigned int single_bit_error_cnt;
    unsigned int double_bit_error_cnt;
    unsigned int total_single_bit_error_cnt;
    unsigned int total_double_bit_error_cnt;
    unsigned int single_bit_isolated_pages_cnt;
    unsigned int double_bit_isolated_pages_cnt;
  };

  struct dcmi_chip_pcie_err_rate {
    unsigned int reg_deskew_fifo_overflow_intr_status;
    unsigned int reg_symbol_unlock_intr_status;
    unsigned int reg_deskew_unlock_intr_status;
    unsigned int reg_phystatus_timeout_intr_status;
    unsigned int symbol_unlock_counter;
    unsigned int pcs_rx_err_cnt;
    unsigned int phy_lane_err_counter;
    unsigned int pcs_rcv_err_status;
    unsigned int symbol_unlock_err_status;
    unsigned int phy_lane_err_status;
    unsigned int dl_lcrc_err_num;
    unsigned int dl_dcrc_err_num;
  };

  struct dcmi_pcie_link_error_info {
    unsigned int tx_err_cnt;
    unsigned int rx_err_cnt;
    unsigned int lcrc_err_cnt;
    unsigned int ecrc_err_cnt;
    unsigned int retry_cnt;
    unsigned int rsv[32];
  };

  enum dcmi_manager_sensor_id {
    DCMI_CLUSTER_TEMP_ID = 0,
    DCMI_PERI_TEMP_ID = 1,
    DCMI_AICORE0_TEMP_ID,
    DCMI_AICORE1_TEMP_ID,
    DCMI_AICORE_LIMIT_ID,
    DCMI_AICORE_TOTAL_PER_ID,
    DCMI_AICORE_ELIM_PER_ID,
    DCMI_AICORE_BASE_FREQ_ID,
    DCMI_NPU_DDR_FREQ_ID,
    DCMI_THERMAL_THRESHOLD_ID,
    DCMI_NTC_TEMP_ID,
    DCMI_SOC_TEMP_ID,
    DCMI_FP_TEMP_ID,
    DCMI_N_DIE_TEMP_ID,
    DCMI_HBM_TEMP_ID,
    DCMI_SENSOR_INVALID_ID = 255
  };

  union dcmi_sensor_info {
    unsigned char uchar;
    unsigned short ushort;
    unsigned int uint;
    signed int iint;
    signed char temp[2];
    signed int ntc_tmp[4];
    unsigned int data[16];
  };

  struct dcmi_memory_info {
    unsigned long long memory_size; /* unit:MB */
    unsigned int freq;              /* unit:MHz */
    unsigned int utiliza;           /* unit:% */
  };

  struct dcmi_dvpp_ratio {
    int vdec_ratio;
    int vpc_ratio;
    int venc_ratio;
    int jpege_ratio;
    int jpegd_ratio;
  };

  struct dcmi_hbm_info {
    unsigned long long memory_size;
    unsigned int freq;
    unsigned long long memory_usage;
    int temp;
    unsigned int bandwith_util_rate;
  };

  struct dcmi_hbm_product_info {
    unsigned short manufacturer_id;
    unsigned char reserve[62];
  };

  struct dcmi_flash_info {
    unsigned long long flash_id;
    unsigned short device_id;
    unsigned short vendor;
    unsigned int state;
    unsigned long long size;
    unsigned int sector_count;
    unsigned short manufacturer_id;
  };

  /* The legacy symbol uses a separately named structure in the CANN ABI.
   * Keep the tag distinct so calls are type-checked against the public
   * declaration; both layouts are intentionally identical. */
  struct dcmi_flash_info_stru {
    unsigned long long flash_id;
    unsigned short device_id;
    unsigned short vendor;
    unsigned int state;
    unsigned long long size;
    unsigned int sector_count;
    unsigned short manufacturer_id;
  };

  /* ECC history records are part of the current CANN ABI.  They are kept
   * separate from dcmi_ecc_info because the latter is a counter snapshot. */
  enum ECC_INFO_READ {
    MULTI_ECC_TIMES_READ = 0,
    SINGLE_ECC_INFO_READ,
    MULTI_ECC_INFO_READ,
    ECC_ADDRESS_COUNT_READ,
    ECC_MAX_READ_CMD
  };

  struct dcmi_ecc_record_type {
    enum ECC_INFO_READ read_type;
    enum dcmi_device_type module_type;
  };

#pragma pack(push, 1)
  struct dcmi_multi_ecc_time_data {
    unsigned int multi_record_count;
    unsigned int multi_ecc_times[MAX_RECORD_ECC_ADDR_COUNT];
  };

  struct dcmi_ecc_common_data {
    unsigned long long physical_addr;
    unsigned int stack_pc_id;
    unsigned int reg_addr_h;
    unsigned int reg_addr_l;
    unsigned int ecc_count;
    int timestamp;
  };
#pragma pack(pop)

  struct dcmi_get_memory_info_stru {
    unsigned long long memory_size;        /* unit:MB */
    unsigned long long memory_available;   /* free + hugepages_free * hugepagesize */
    unsigned int freq;
    unsigned long hugepagesize;             /* unit:KB */
    unsigned long hugepages_total;
    unsigned long hugepages_free;
    unsigned int utiliza;                  /* ddr memory info usages */
    unsigned char reserve[60];             /* the size of dcmi_memory_info is 96 */
  };

  enum dcmi_ip_addr_type {
    DCMI_IPADDR_TYPE_V4 = 0, /** IPv4 */
    DCMI_IPADDR_TYPE_V6 = 1, /** IPv6 */
    DCMI_IPADDR_TYPE_ANY = 2 /** IPv4+IPv6 ("dual-stack") */
  };

  struct dcmi_ip_addr {
    union {
      unsigned char ip6[16];
      unsigned char ip4[4];
    } u_addr;
    enum dcmi_ip_addr_type ip_type;
  };

  enum dcmi_unit_type {
    NPU_TYPE = 0,
    MCU_TYPE = 1,
    CPU_TYPE = 2,
    INVALID_TYPE = 0xFF
  };

  enum dcmi_rdfx_detect_result {
    DCMI_RDFX_DETECT_OK = 0,
    DCMI_RDFX_DETECT_SOCK_FAIL = 1,
    DCMI_RDFX_DETECT_RECV_TIMEOUT = 2,
    DCMI_RDFX_DETECT_UNREACH = 3,
    DCMI_RDFX_DETECT_TIME_EXCEEDED = 4,
    DCMI_RDFX_DETECT_FAULT = 5,
    DCMI_RDFX_DETECT_INIT = 6,
    DCMI_RDFX_DETECT_THREAD_ERR = 7,
    DCMI_RDFX_DETECT_IP_SET = 8,
    DCMI_RDFX_DETECT_MAX = 0xFF
  };

  enum dcmi_device_compat { DCMI_COMPAT_OK = 1, DCMI_COMPAT_NOK = 2, DCMI_COMPAT_UNKNOWN = 3 };

  enum dcmi_port_type {
    DCMI_VNIC_PORT = 0,
    DCMI_ROCE_PORT = 1,
    DCMI_INVALID_PORT
  };

  enum dcmi_main_cmd {
    DCMI_MAIN_CMD_DVPP = 0,
    DCMI_MAIN_CMD_ISP,
    DCMI_MAIN_CMD_TS_GROUP_NUM,
    DCMI_MAIN_CMD_CAN,
    DCMI_MAIN_CMD_UART,
    DCMI_MAIN_CMD_UPGRADE,
    DCMI_MAIN_CMD_UFS,
    DCMI_MAIN_CMD_OS_POWER,
    DCMI_MAIN_CMD_LP,
    DCMI_MAIN_CMD_MEMORY,
    DCMI_MAIN_CMD_RECOVERY,
    DCMI_MAIN_CMD_TS,
    DCMI_MAIN_CMD_CHIP_INF,
    DCMI_MAIN_CMD_QOS,
    DCMI_MAIN_CMD_SOC_INFO,
    DCMI_MAIN_CMD_SILS,
    DCMI_MAIN_CMD_HCCS,
    DCMI_MAIN_CMD_HOST_AICPU,
    DCMI_MAIN_CMD_TEMP = 50,
    DCMI_MAIN_CMD_SVM = 51,
    DCMI_MAIN_CMD_VDEV_MNG,
    DCMI_MAIN_CMD_SEC,
    DCMI_MAIN_CMD_EX_COMPUTING = 0x8000,
    DCMI_MAIN_CMD_DEVICE_SHARE = 0x8001,
    DCMI_MAIN_CMD_EX_CERT = 0x8003,
    DCMI_MAIN_CMD_PCIE = 55,
    DCMI_MAIN_CMD_SIO = 56,
    DCMI_MAIN_CMD_MAX
  };

  enum dcmi_freq_type {
    DCMI_FREQ_DDR = 1,
    DCMI_FREQ_CTRLCPU = 2,
    DCMI_FREQ_HBM = 6,
    DCMI_FREQ_AICORE_CURRENT_ = 7,
    DCMI_FREQ_AICORE_MAX = 9,
    DCMI_FREQ_VECTORCORE_CURRENT = 12
  };

  enum dcmi_lp_sub_cmd {
    DCMI_LP_SUB_CMD_AICORE_VOLTAGE_CURRENT = 0,
    DCMI_LP_SUB_CMD_HYBIRD_VOLTAGE_CURRENT,
    DCMI_LP_SUB_CMD_TAISHAN_VOLTAGE_CURRENT,
    DCMI_LP_SUB_CMD_DDR_VOLTAGE_CURRENT,
    DCMI_LP_SUB_CMD_ACG,
    DCMI_LP_SUB_CMD_STATUS,
    DCMI_LP_SUB_CMD_TOPS_DETAILS,
    DCMI_LP_SUB_CMD_SET_WORK_TOPS,
    DCMI_LP_SUB_CMD_GET_WORK_TOPS,
    DCMI_LP_SUB_CMD_AICORE_FREQREDUC_CAUSE,
    DCMI_LP_SUB_CMD_GET_POWER_INFO,
    DCMI_LP_SUB_CMD_SET_IDLE_SWITCH,
    DCMI_LP_SUB_CMD_MAX
  };

  struct dcmi_lp_power_info {
    unsigned int soc_rated_power;
    unsigned char reserved[32];
  };

  enum dcmi_component_type {
    DCMI_COMPONENT_TYPE_NVE = 0,
    DCMI_COMPONENT_TYPE_XLOADER,
    DCMI_COMPONENT_TYPE_M3FW,
    DCMI_COMPONENT_TYPE_UEFI,
    DCMI_COMPONENT_TYPE_TEE,
    DCMI_COMPONENT_TYPE_KERNEL,
    DCMI_COMPONENT_TYPE_DTB,
    DCMI_COMPONENT_TYPE_ROOTFS,
    DCMI_COMPONENT_TYPE_IMU,
    DCMI_COMPONENT_TYPE_IMP,
    DCMI_COMPONENT_TYPE_AICPU,
    DCMI_COMPONENT_TYPE_HBOOT1_A,
    DCMI_COMPONENT_TYPE_HBOOT1_B,
    DCMI_COMPONENT_TYPE_HBOOT2,
    DCMI_COMPONENT_TYPE_DDR,
    DCMI_COMPONENT_TYPE_LP,
    DCMI_COMPONENT_TYPE_HSM,
    DCMI_COMPONENT_TYPE_SAFETY_ISLAND,
    DCMI_COMPONENT_TYPE_HILINK
  };

  enum dcmi_reset_channel {
    OUTBAND_CHANNEL = 0, // out-of-band reset
    INBAND_CHANNEL // in-band reset
  };

  enum dcmi_boot_status {
    DCMI_BOOT_STATUS_UNINIT = 0, // not init
    DCMI_BOOT_STATUS_BIOS,       // BIOS starting
    DCMI_BOOT_STATUS_OS,         // OS starting
    DCMI_BOOT_STATUS_FINISH,     // started
    DCMI_SYSTEM_START_FINISH = 16
  };

  enum dcmi_event_type {
    DCMI_DMS_FAULT_EVENT = 0,
  };

  enum dcmi_die_type { NDIE, VDIE, DDIE, INVALID_DIE };

#define DCMI_VDEV_RES_NAME_LEN 16
#define DCMI_VDEV_SIZE 20
#define DCMI_VDEV_FOR_RESERVE 32
#define DCMI_SOC_SPLIT_MAX 32
#define DCMI_PCIE_SUB_CMD_PCIE_ERROR_INFO 1
#define DCMI_MAX_EVENT_NAME_LENGTH 256
#define DCMI_MAX_EVENT_DATA_LENGTH 32
#define DCMI_EVENT_FILTER_FLAG_EVENT_ID (1UL << 0)
#define DCMI_EVENT_FILTER_FLAG_SERVERITY (1UL << 1)
#define DCMI_EVENT_FILTER_FLAG_NODE_TYPE (1UL << 2)
#define DCMI_MAX_EVENT_RESV_LENGTH 32

  struct dcmi_base_resource {
    unsigned long long token;
    unsigned long long token_max;
    unsigned long long task_timeout;
    unsigned int vfg_id;
    unsigned char vip_mode;
    unsigned char reserved[DCMI_VDEV_FOR_RESERVE - 1];  /* bytes aligned */
  };

  /* total types of computing resource */
  struct dcmi_computing_resource {
    /* accelator resource */
    float aic;
    float aiv;
    unsigned short dsa;
    unsigned short rtsq;
    unsigned short acsq;
    unsigned short cdqm;
    unsigned short c_core;
    unsigned short ffts;
    unsigned short sdma;
    unsigned short pcie_dma;

    /* memory resource, MB as unit */
    unsigned long long memory_size;

    /* id resource */
    unsigned int event_id;
    unsigned int notify_id;
    unsigned int stream_id;
    unsigned int model_id;

    /* cpu resource */
    unsigned short topic_schedule_aicpu;
    unsigned short host_ctrl_cpu;
    unsigned short host_aicpu;
    unsigned short device_aicpu;
    unsigned short topic_ctrl_cpu_slot;

    /* vnpu resource */
    unsigned int vdev_aicore_utilization;
    unsigned long long vdev_memory_total;
    unsigned long long vdev_memory_free;

    unsigned char reserved[DCMI_VDEV_FOR_RESERVE-DCMI_VDEV_SIZE];
  };

  struct dcmi_media_resource {
    /* dvpp resource */
    float jpegd;
    float jpege;
    float vpc;
    float vdec;
    float pngd;
    float venc;
    unsigned char reserved[DCMI_VDEV_FOR_RESERVE];
  };

  struct dcmi_create_vdev_out {
    unsigned int vdev_id;
    unsigned int pcie_bus;
    unsigned int pcie_device;
    unsigned int pcie_func;
    unsigned int vfg_id;
    unsigned char reserved[DCMI_VDEV_FOR_RESERVE];
  };

  struct dcmi_create_vdev_res_stru {
    unsigned int vdev_id;
    unsigned int vfg_id;
    char template_name[TEMPLATE_NAME_LEN];
    unsigned char reserved[64];
  };

  struct dcmi_vdev_query_info {
    char name[DCMI_VDEV_RES_NAME_LEN];
    unsigned int status;
    unsigned int is_container_used;
    unsigned int vfid;
    unsigned int vfg_id;
    unsigned long long container_id;
    struct dcmi_base_resource base;
    struct dcmi_computing_resource computing;
    struct dcmi_media_resource media;
  };

  /* for single search */
  struct dcmi_vdev_query_stru {
    unsigned int vdev_id;
    struct dcmi_vdev_query_info query_info;
  };

  struct dcmi_soc_free_resource {
    unsigned int vfg_num;
    unsigned int vfg_bitmap;
    struct dcmi_base_resource base;
    struct dcmi_computing_resource computing;
    struct dcmi_media_resource media;
  };

  struct dcmi_soc_total_resource {
    unsigned int vdev_num;
    unsigned int vdev_id[DCMI_SOC_SPLIT_MAX];
    unsigned int vfg_num;
    unsigned int vfg_bitmap;
    struct dcmi_base_resource base;
    struct dcmi_computing_resource computing;
    struct dcmi_media_resource media;
  };

  struct dcmi_dms_fault_event {
    unsigned int event_id; /* Event ID */
    unsigned short deviceid; /* Device ID */
    unsigned char node_type; /* Node type */
    unsigned char node_id; /* Node ID */
    unsigned char sub_node_type; /* Subnode type */
    unsigned char sub_node_id; /* Subnode ID */
    unsigned char severity; /* Event severity. 0: warning; 1: minor; 2: major; 3: critical */
    unsigned char assertion; /* Event type. 0: fault recovery; 1: fault generation; 2: one-off event */
    int event_serial_num; /* Alarm serial number */
    int notify_serial_num; /* Notification serial number*/
    /* Time when the event occurs, presenting as the number of seconds that have elapsed since the Unix epoch. */
    unsigned long long alarm_raised_time;
    char event_name[DCMI_MAX_EVENT_NAME_LENGTH]; /* Event description */
    char additional_info[DCMI_MAX_EVENT_DATA_LENGTH]; /* Additional event information */
    unsigned char os_id;
    unsigned char resv_1[1];
    unsigned short node_type_ex;
    unsigned short sub_node_type_ex;
    unsigned char resv_2[2];
    unsigned char resv[DCMI_MAX_EVENT_RESV_LENGTH - 8]; /**< Reserves 24 bytes */
  };

  struct dcmi_event {
    enum dcmi_event_type type; /* Event type */
    union {
      struct dcmi_dms_fault_event dms_event; /* Event content */
    } event_t;
  };

  struct dcmi_event_filter {
    /* It can be used to enable one or all filter criteria. The filter criteria are as follows:
    0: disables the filter criteria.
    DCMI_EVENT_FILTER_FLAG_EVENT_ID: receives only specified events.
    DCMI_EVENT_FILTER_FLAG_SERVERITY: receives only the events of a specified level and higher levels.
    DCMI_EVENT_FILTER_FLAG_NODE_TYPE: receives only events of a specified node type. */
    unsigned long long filter_flag;
    /* Receives a specified event. For details, see the Health Management Error Definition. */
    unsigned int event_id;
    /* Receives events of a specified level and higher levels. For details,
    see the severity definition in the struct dcmi_dms_fault_event structure. */
    unsigned char severity;
    /* Receives only events of a specified node type. For details, see the Health Management Error Definition. */
    unsigned char node_type;
    unsigned char resv[DCMI_MAX_EVENT_RESV_LENGTH]; /* < Reserves 32 bytes. */
  };

  struct dcmi_proc_mem_info {
    int proc_id;
    // unit is byte
    unsigned long proc_mem_usage;
  };

  struct dcmi_board_info {
    unsigned int board_id;
    unsigned int pcb_id;
    unsigned int bom_id;
    unsigned int slot_id; // slot_id indicates pcie slot ID of the chip
  };

  struct dcmi_elabel_info {
    char product_name[MAX_LENTH];
    char model[MAX_LENTH];
    char manufacturer[MAX_LENTH];
    char manufacturer_date[MAX_LENTH];
    char serial_number[MAX_LENTH];
  };

  struct dcmi_cgroup_info {
    unsigned long limit_in_bytes;
    unsigned long max_usage_in_bytes;
    unsigned long usage_in_bytes;
  };

  struct dcmi_llc_perf {
    unsigned int wr_hit_rate;
    unsigned int rd_hit_rate;
    unsigned int throughput;
  };

  struct dsmi_hbm_info_stru {
    unsigned long long memory_size;      /**< HBM total size, MB */
    unsigned int freq;                   /**< HBM freq, MHZ */
    unsigned long long memory_usage;     /**< HBM memory_usage, MB */
    int temp;                            /**< HBM temperature */
    unsigned int bandwith_util_rate;
  };

#define AGENTDRV_PROF_DATA_NUM 3
  struct dcmi_pcie_link_bandwidth_info {
    int profiling_time;
    unsigned int tx_p_bw[AGENTDRV_PROF_DATA_NUM];
    unsigned int tx_np_bw[AGENTDRV_PROF_DATA_NUM];
    unsigned int tx_cpl_bw[AGENTDRV_PROF_DATA_NUM];
    unsigned int tx_np_lantency[AGENTDRV_PROF_DATA_NUM];
    unsigned int rx_p_bw[AGENTDRV_PROF_DATA_NUM];
    unsigned int rx_np_bw[AGENTDRV_PROF_DATA_NUM];
    unsigned int rx_cpl_bw[AGENTDRV_PROF_DATA_NUM];
  };

  struct dcmi_hccs_bandwidth_info {
    int profiling_time;
    double total_txbw;
    double total_rxbw;
    double tx_bandwidth[DCMI_HCCS_MAX_PCS_NUM];
    double rx_bandwidth[DCMI_HCCS_MAX_PCS_NUM];
  };

  enum dcmi_entire_ub_status {
    DCMI_UB_ALL_PORT_NO_LINK = 0,
    DCMI_UB_ALL_PORT_LINK,
    DCMI_UB_PARTIAL_PORT_LINK,
    DCMI_UB_NO_NEED_LINK,
  };

  enum dcmi_ub_port_status {
    DCMI_UB_PORT_STATUS_NONE_LANE = 0,
    DCMI_UB_PORT_STATUS_FULL_LANE,
    DCMI_UB_PORT_STATUS_PARTIAL_LANE,
    DCMI_UB_PORT_STATUS_INITIAL,
  };

  struct dcmi_ub_port_link_status {
    enum dcmi_entire_ub_status ub_link_status;
    enum dcmi_ub_port_status ub_port_status[DCMI_UB_PORT_NUM];
  };

  struct dcmi_ub_bandwidth_info {
    unsigned int tx_bandwidth;
    unsigned int rx_bandwidth;
    unsigned int reserved[2];
  };

  struct dcmi_ub_port_info {
    int udie_id;
    int port_id;
  };

  struct dcmi_network_rdma_bandwidth_info {
    unsigned int tx_bandwidth;
    unsigned int rx_bandwidth;
  };

  struct dcmi_port_pkt_stats_info {
    unsigned int port_id;
    unsigned int is_uboe_port;
    union {
      struct {
        unsigned long long ub_ipv4_pkt_cnt_rx;
        unsigned long long ub_ipv6_pkt_cnt_rx;
        unsigned long long unic_ipv4_pkt_cnt_rx;
        unsigned long long unic_ipv6_pkt_cnt_rx;
        unsigned long long ub_compact_pkt_cnt_rx;
        unsigned long long ub_umoc_ctph_cnt_rx;
        unsigned long long ub_umoc_ntph_cnt_rx;
        unsigned long long ub_mem_pkt_cnt_rx;
        unsigned long long unknown_pkt_cnt_rx;
        unsigned long long drop_ind_cnt_rx;
        unsigned long long err_ind_cnt_rx;
        unsigned long long to_host_pkt_cnt_rx;
        unsigned long long to_imp_pkt_cnt_rx;
        unsigned long long to_mar_pkt_cnt_rx;
        unsigned long long to_link_pkt_cnt_rx;
        unsigned long long to_noc_pkt_cnt_rx;
        unsigned long long route_err_cnt_rx;
        unsigned long long out_err_cnt_rx;
        unsigned long long length_err_cnt_rx;
        unsigned long long rx_busi_flit_num;
        unsigned long long rx_send_ack_flit;
        unsigned long long ub_ipv4_pkt_cnt_tx;
        unsigned long long ub_ipv6_pkt_cnt_tx;
        unsigned long long unic_ipv4_pkt_cnt_tx;
        unsigned long long unic_ipv6_pkt_cnt_tx;
        unsigned long long ub_compact_pkt_cnt_tx;
        unsigned long long ub_umoc_ctph_cnt_tx;
        unsigned long long ub_umoc_ntph_cnt_tx;
        unsigned long long ub_mem_pkt_cnt_tx;
        unsigned long long unknown_pkt_cnt_tx;
        unsigned long long drop_ind_cnt_tx;
        unsigned long long err_ind_cnt_tx;
        unsigned long long lpbk_ind_cnt_tx;
        unsigned long long out_err_cnt_tx;
        unsigned long long length_err_cnt_tx;
        unsigned long long tx_busi_flit_num;
        unsigned long long tx_recv_ack_flit;
        unsigned long long retry_req_sum;
        unsigned long long retry_ack_sum;
        unsigned long long crc_error_sum;
        struct {
          unsigned long long core_mib_rxpausepkts;
          unsigned long long core_mib_txpausepkts;
          unsigned long long core_mib_rxpfcpkts;
          unsigned long long core_mib_txpfcpkts;
          unsigned long long core_mib_rxbadpkts;
          unsigned long long core_mib_txbadpkts;
          unsigned long long core_mib_rxbadoctets;
          unsigned long long core_mib_txbadoctets;
        } uboe;
      };
      unsigned long long pkt_num[DCMI_PORT_PKT_STATS_NUM];
    };
    unsigned char reserved[128];
  };

  struct dcmi_credit_info {
    unsigned int link_alloc_port_share_credit;
    unsigned int link_cur_used_port_share_credit;
    unsigned int link_alloc_vl_pri_credit[16];
    unsigned int link_cur_used_pri_credit[16];
  };

  struct dcmi_netdev_list_info {
    int netdev_nums;
    char netdev_name[NETDEV_MAX_NUM][NETDEV_NAME_MAX_LEN];
  };

  struct dcmi_tc_stat_data {
    unsigned long long tc_tx[TC_MAX_NUM];
    unsigned long long tc_rx[TC_MAX_NUM];
    unsigned long long reserved[TC_MAX_NUM];
  };

  struct dcmi_network_pkt_stats_info {
    unsigned long long mac_tx_mac_pause_num;
    unsigned long long mac_rx_mac_pause_num;
    unsigned long long mac_tx_pfc_pkt_num;
    unsigned long long mac_tx_pfc_pri0_pkt_num;
    unsigned long long mac_tx_pfc_pri1_pkt_num;
    unsigned long long mac_tx_pfc_pri2_pkt_num;
    unsigned long long mac_tx_pfc_pri3_pkt_num;
    unsigned long long mac_tx_pfc_pri4_pkt_num;
    unsigned long long mac_tx_pfc_pri5_pkt_num;
    unsigned long long mac_tx_pfc_pri6_pkt_num;
    unsigned long long mac_tx_pfc_pri7_pkt_num;
    unsigned long long mac_rx_pfc_pkt_num;
    unsigned long long mac_rx_pfc_pri0_pkt_num;
    unsigned long long mac_rx_pfc_pri1_pkt_num;
    unsigned long long mac_rx_pfc_pri2_pkt_num;
    unsigned long long mac_rx_pfc_pri3_pkt_num;
    unsigned long long mac_rx_pfc_pri4_pkt_num;
    unsigned long long mac_rx_pfc_pri5_pkt_num;
    unsigned long long mac_rx_pfc_pri6_pkt_num;
    unsigned long long mac_rx_pfc_pri7_pkt_num;
    unsigned long long mac_tx_total_pkt_num;
    unsigned long long mac_tx_total_oct_num;
    unsigned long long mac_tx_bad_pkt_num;
    unsigned long long mac_tx_bad_oct_num;
    unsigned long long mac_rx_total_pkt_num;
    unsigned long long mac_rx_total_oct_num;
    unsigned long long mac_rx_bad_pkt_num;
    unsigned long long mac_rx_bad_oct_num;
    unsigned long long mac_rx_fcs_err_pkt_num;
    unsigned long long roce_rx_rc_pkt_num;
    unsigned long long roce_rx_all_pkt_num;
    unsigned long long roce_rx_err_pkt_num;
    unsigned long long roce_tx_rc_pkt_num;
    unsigned long long roce_tx_all_pkt_num;
    unsigned long long roce_tx_err_pkt_num;
    unsigned long long roce_cqe_num;
    unsigned long long roce_rx_cnp_pkt_num;
    unsigned long long roce_tx_cnp_pkt_num;
    unsigned long long roce_err_ack_num;
    unsigned long long roce_err_psn_num;
    unsigned long long roce_verification_err_num;
    unsigned long long roce_err_qp_status_num;
    unsigned long long roce_new_pkt_rty_num;
    unsigned long long roce_ecn_db_num;
    unsigned long long nic_tx_all_pkg_num;
    unsigned long long nic_tx_all_oct_num;
    unsigned long long nic_rx_all_pkg_num;
    unsigned long long nic_rx_all_oct_num;
    long tv_sec;
    long tv_usec;
    unsigned char reserved[64];
  };

  struct port_info {
    int port_index;
    int mode;
  };

  struct die_port_list_info {
    int die_index;
    int port_nums;
    struct port_info port_list_data[MAX_PORT_NUMS];
  };

  struct dcmi_port_list_info {
    int die_nums;
    struct die_port_list_info die_list_data[MAX_DIE_NUMS];
  };

#define DCMI_VERSION_1
#define DCMI_VERSION_2

#if defined DCMI_VERSION_2

  DCMIDLLEXPORT int dcmi_init(void);

  DCMIDLLEXPORT int dcmi_get_dcmi_version(char *dcmi_ver, unsigned int len) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_driver_version(char *driver_ver, unsigned int len) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_card_list(int *card_num, int *card_list, int list_len);

  DCMIDLLEXPORT int dcmi_get_device_num_in_card(int card_id, int *device_num);

  DCMIDLLEXPORT int dcmi_get_device_id_in_card(int card_id, int *device_id_max, int *mcu_id, int *cpu_id);

  DCMIDLLEXPORT int dcmi_get_device_type(int card_id, int device_id, enum dcmi_unit_type *device_type) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_pcie_info_v2(int card_id, int device_id,
                                                 struct dcmi_pcie_info_all *pcie_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_pcie_info(int card_id, int device_id, struct dcmi_pcie_info *pcie_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_chip_info_v2(int card_id, int device_id,
                                                 struct dcmi_chip_info_v2 *chip_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_chip_info(int card_id, int device_id, struct dcmi_chip_info *chip_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_aicore_info(int card_id, int device_id,
                                                struct dcmi_aicore_info *aicore_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_power_info(int card_id, int device_id, int *power) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_health(int card_id, int device_id, unsigned int *health) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_driver_health(unsigned int *health) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_driver_errorcode(int *error_count, unsigned int *error_code_list,
                                              unsigned int list_len) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_errorcode_v2(int card_id, int device_id, int *error_count,
                                                 unsigned int *error_code_list, unsigned int list_len) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_errorcode_string(int card_id, int device_id, unsigned int error_code,
                                                     unsigned char *error_info, int buf_size) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_temperature(int card_id, int device_id, int *temperature) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_voltage(int card_id, int device_id, unsigned int *voltage) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_board_info(int card_id, int device_id,
                                               struct dcmi_board_info *board_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_board_id(int card_id, int device_id, unsigned int *board_id) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_mainboard_id(int card_id, int device_id, unsigned int *mainboard_id) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_card_pcie_slot(int card_id, int *pcie_slot) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_chip_slot(int card_id, int device_id, int *chip_pos_id) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_first_power_on_date(int card_id, unsigned int *first_power_on_date) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_aicpu_count_info(int card_id, int device_id, unsigned char *count_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_cpu_num_config(int card_id, int device_id, unsigned char *buf,
                                                   unsigned int buf_size) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_cpu_freq_info(int card_id, int device_id, int *enable_flag) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_p2p_enable(int card_id, int device_id, int *enable_flag) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_aicpu_info(int card_id, int device_id,
                                               struct dcmi_aicpu_info *aicpu_info) DCMI_WEAK;

  /* Newer physical-device spelling; older releases may only export the
   * dcmi_get_system_time alias below. */
  DCMIDLLEXPORT int dcmi_get_device_system_time(int card_id, int device_id, unsigned int *system_time) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_multi_utilization_rate(int card_id, int device_id,
                                                           struct dcmi_multi_utilization_info *util_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_sensor_info(int card_id, int device_id, enum dcmi_manager_sensor_id sensor_id,
                                                union dcmi_sensor_info *sensor_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_ecc_info(int card_id, int device_id, enum dcmi_device_type input_type,
                                             struct dcmi_ecc_info *device_ecc_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_pcie_error_cnt(int card_id, int device_id,
                                                   struct dcmi_chip_pcie_err_rate *pcie_err_code_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_frequency(int card_id, int device_id, enum dcmi_freq_type input_type,
                                              unsigned int *frequency) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_hbm_info(int card_id, int device_id, struct dcmi_hbm_info *hbm_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_hbm_product_info(int card_id, int device_id,
                                                     struct dcmi_hbm_product_info *hbm_product_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_memory_info_v2(int card_id, int device_id,
                                                   struct dcmi_memory_info *memory_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_memory_info_v3(int card_id, int device_id,
                                                   struct dcmi_get_memory_info_stru *memory_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_utilization_rate(int card_id, int device_id, int input_type,
                                                     unsigned int *utilization_rate) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_info(int card_id, int device_id, enum dcmi_main_cmd main_cmd, unsigned int sub_cmd,
                                         void *buf, unsigned int *size) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_component_static_version(int card_id, int device_id,
                                                             enum dcmi_component_type component_type,
                                                             unsigned char *version_str, unsigned int len) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_component_count(int card_id, int device_id,
                                                    unsigned int *component_count) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_component_list(int card_id, int device_id,
                                                   enum dcmi_component_type *component_table,
                                                   unsigned int component_count) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_elabel_info(int card_id, int device_id,
                                                struct dcmi_elabel_info *elabel_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_cgroup_info(int card_id, int device_id, struct dcmi_cgroup_info *cg_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_llc_perf_para(int card_id, int device_id,
                                                  struct dcmi_llc_perf *perf_para) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_share_enable(int card_id, int device_id, int *enable_flag) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_ip(int card_id, int device_id, enum dcmi_port_type input_type, int port_id,
                                       struct dcmi_ip_addr *ip, struct dcmi_ip_addr *mask);

  DCMIDLLEXPORT int dcmi_get_device_network_health(int card_id, int device_id,
                                                   enum dcmi_rdfx_detect_result *result) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_outband_channel_state(int card_id, int device_id, int *channel_state) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_current_fault_event(int card_id, int device_id, struct dcmi_event *event_buf,
                                                        int input_event_buf_length, int *output_event_cnt) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_compatibility(int card_id, int device_id,
                                                  enum dcmi_device_compat *compatibility) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_fan_count(int card_id, int device_id, int *count) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_fan_speed(int card_id, int device_id, int fan_id, int *speed) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_logic_id(int *device_logic_id, int card_id, int device_id);

  DCMIDLLEXPORT int dcmi_create_vdevice(int card_id, int device_id, struct dcmi_create_vdev_res_stru *vdev,
                                        struct dcmi_create_vdev_out *out);

  DCMIDLLEXPORT int dcmi_set_destroy_vdevice(int card_id, int device_id, unsigned int vdevid);

  DCMIDLLEXPORT int dcmi_get_device_phyid_from_logicid(unsigned int logicid, unsigned int *phyid);

  DCMIDLLEXPORT int dcmi_get_device_logicid_from_phyid(unsigned int phyid, unsigned int *logicid);

  DCMIDLLEXPORT int dcmi_get_card_id_device_id_from_logicid(int *card_id, int *device_id,
                                                            unsigned int device_logic_id) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_card_id_device_id_from_phyid(int *card_id, int *device_id, unsigned int device_phy_id);

  DCMIDLLEXPORT int dcmi_get_product_type(int card_id, int device_id, char *product_type_str, int buf_size);

  DCMIDLLEXPORT int dcmi_set_device_reset(int card_id, int device_id, enum dcmi_reset_channel channel_type);

  DCMIDLLEXPORT int dcmi_get_device_boot_status(int card_id, int device_id,
                                                enum dcmi_boot_status *boot_status) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_subscribe_fault_event(int card_id, int device_id, struct dcmi_event_filter filter);

  DCMIDLLEXPORT int dcmi_get_npu_work_mode(int card_id, unsigned char *work_mode);

  DCMIDLLEXPORT int dcmi_get_device_die_v2(int card_id, int device_id, enum dcmi_die_type input_type,
                                           struct dcmi_die_id *die_id) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_resource_info(int card_id, int device_id, struct dcmi_proc_mem_info *proc_info,
                                                  int *proc_num) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_dvpp_ratio_info(int card_id, int device_id,
                                                    struct dcmi_dvpp_ratio *usage) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_hbm_info(int card_id, int device_id, struct dsmi_hbm_info_stru *device_hbm_info) DCMI_WEAK;

  DCMIDLLEXPORT int
  dcmi_get_pcie_link_bandwidth_info(int card_id, int device_id,
                                    struct dcmi_pcie_link_bandwidth_info *pcie_link_bandwidth_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_hccs_link_bandwidth_info(int card_id, int device_id,
                                                      struct dcmi_hccs_bandwidth_info *hccs_bandwidth_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_ub_realtime_bandwidth_info(int card_id, int device_id, unsigned int profiling_time,
                                                        struct dcmi_ub_port_info *ub_port_info,
                                                        struct dcmi_ub_bandwidth_info *ub_bandwidth_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_ub_port_link_status_info(int card_id, int device_id,
                                                      struct dcmi_ub_port_link_status *ub_status) DCMI_WEAK;

  /* Logical-device API introduced by newer CANN releases. These symbols are
   * weak on purpose so the same binary remains compatible with older DCMI
   * libraries that only export the card/device API above. */
  DCMIDLLEXPORT int dcmiv2_init(void) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_driver_version(char *driver_ver, unsigned int len) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_dcmi_version(char *dcmi_ver, unsigned int len) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_list(int *device_list, int *device_cnt, int list_len) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_device_chip_info(int dev_id, struct dcmi_chip_info_v2 *chip_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_pcie_info(int dev_id, struct dcmi_pcie_info_all *pcie_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_ub_id_info(int dev_id, struct dcmi_ub_id_info *ub_id_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_board_info(int dev_id, struct dcmi_board_info *board_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_elabel_info(int dev_id, struct dcmi_elabel_info *elabel_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_hbm_product_info(int dev_id,
                                                       struct dcmi_hbm_product_info *hbm_product_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_driver_health(unsigned int *health) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_driver_error_code_list(int *error_count, unsigned int *error_code_list,
                                                      unsigned int list_len) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_error_code_list(int dev_id, int *error_count, unsigned int *error_code_list,
                                                      unsigned int list_len) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_device_error_info(int dev_id, unsigned int error_code, unsigned char *error_info,
                                                 int buf_size) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_flash_cnt(int dev_id, unsigned int *flash_cnt) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_flash_info(int dev_id, unsigned int flash_index,
                                                 struct dcmi_flash_info *flash_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_health(int dev_id, unsigned int *health) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_die_id(int dev_id, enum dcmi_die_type input_type,
                                             struct dcmi_die_id *die_id) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_power_info(int dev_id, int *power_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_voltage(int dev_id, unsigned int *voltage) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_temperature(int dev_id, int *temperature) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_aicore_info(int dev_id, struct dcmi_aicore_info *aicore_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_aicpu_info(int dev_id, struct dcmi_aicpu_info *aicpu_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_type(int dev_id, enum dcmi_unit_type *device_type) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_board_id(int dev_id, unsigned int *board_id) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_mainboard_id(int dev_id, unsigned int *mainboard_id) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_pcie_slot_id(int dev_id, int *pcie_slot) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_ub_slot_id(int dev_id, int *ub_slot) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_slot_id_and_chip_id_by_dev_id(int dev_id, unsigned int *slot_id,
                                                             unsigned int *chip_id) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_group_intra_id_by_dev_id(int dev_id, unsigned int *group_intra_id) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_share_enable(int dev_id, unsigned int *enable_flag) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_device_ecc_info(int dev_id, enum dcmi_device_type input_type,
                                               struct dcmi_ecc_info *device_ecc_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_frequency(int dev_id, enum dcmi_freq_type input_type,
                                                unsigned int *frequency) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_hbm_info(int dev_id, struct dcmi_hbm_info *hbm_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_utilization_rate(int dev_id, int input_type,
                                                       unsigned int *utilization_rate) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_sensor_info(int dev_id, enum dcmi_manager_sensor_id sensor_id,
                                                  union dcmi_sensor_info *sensor_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_multi_utilization_rate(int dev_id,
                                                             struct dcmi_multi_utilization_info *util_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_pcie_error_cnt(int dev_id,
                                                     struct dcmi_chip_pcie_err_rate *pcie_err_code_info) DCMI_WEAK;
  DCMIDLLEXPORT int
  dcmiv2_get_pcie_link_bandwidth_info(int dev_id,
                                      struct dcmi_pcie_link_bandwidth_info *pcie_link_bandwidth_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_proc_mem_info(int dev_id, struct dcmi_proc_mem_info *proc_info,
                                                    int *proc_num) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_multi_ecc_time_info(int dev_id,
                                                   struct dcmi_multi_ecc_time_data *multi_ecc_time_data) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_multi_ecc_record_info(int dev_id, struct dcmi_ecc_record_type type,
                                                     unsigned int *ecc_count,
                                                     struct dcmi_ecc_common_data *ecc_common_data_s) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_computing_power_info(int dev_id, int type,
                                                    struct dsmi_computing_power_info *computing_power) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_device_current_fault_event(int dev_id, struct dcmi_event *event_buf,
                                                          int input_event_buf_length, int *output_event_cnt) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_info(int dev_id, enum dcmi_main_cmd main_cmd, unsigned int sub_cmd, void *buf,
                                           unsigned int *size) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_cgroup_info(int dev_id, struct dcmi_cgroup_info *cg_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_llc_perf_para(int dev_id, struct dcmi_llc_perf *perf_para) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_component_static_version(int dev_id, enum dcmi_component_type component_type,
                                                               unsigned char *version_str, unsigned int len) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_component_cnt(int dev_id, unsigned int *component_count) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_component_list(int dev_id, enum dcmi_component_type *component_table,
                                                     unsigned int component_count) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_boot_status(int dev_id, enum dcmi_boot_status *boot_status) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_compatibility(int dev_id, enum dcmi_device_compat *compatibility) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_network_health(int dev_id, enum dcmi_rdfx_detect_result *result) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_netdev_health(int dev_id, const char *netdev_name, unsigned int netdev_name_len,
                                                    enum dcmi_rdfx_detect_result *result) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_outband_channel_state(int dev_id, int *channel_state) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_ub_port_link_status(int dev_id, struct dcmi_ub_port_link_status *ub_status) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_ub_realtime_bandwidth_info(int dev_id, unsigned int profiling_time,
                                                          struct dcmi_ub_port_info *ub_port_info,
                                                          struct dcmi_ub_bandwidth_info *ub_bandwidth_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_device_port_list_info(int dev_id, struct dcmi_port_list_info *port_list_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_port_pkt_stats_info(int dev_id, struct dcmi_ub_port_info *ub_port_info,
                                                   struct dcmi_port_pkt_stats_info *port_pkt_stats_info) DCMI_WEAK;

  DCMIDLLEXPORT int
  dcmiv2_get_rdma_bandwidth_info(int dev_id, int port_id, unsigned int profiling_time,
                                 struct dcmi_network_rdma_bandwidth_info *network_rdma_bandwidth_info) DCMI_WEAK;

  DCMIDLLEXPORT int
  dcmiv2_get_netdev_pkt_stats_info(int dev_id, int port_id,
                                   struct dcmi_network_pkt_stats_info *network_pkt_stats_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_device_netdev_list_info(int dev_id, struct dcmi_netdev_list_info *netdev_list) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_affinity_cpu_info_by_dev_id(int dev_id, char *affinity_cpu, int *len) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_system_time(int dev_id, unsigned int *system_time) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_vrd_version(int dev_id, char *version, int len) DCMI_WEAK;

  DCMIDLLEXPORT int
  dcmi_get_rdma_bandwidth_info(int card_id, int device_id, int port_id, unsigned int profiling_time,
                               struct dcmi_network_rdma_bandwidth_info *network_rdma_bandwidth_info) DCMI_WEAK;

  DCMIDLLEXPORT int
  dcmi_get_netdev_pkt_stats_info(int card_id, int device_id, int port_id,
                                 struct dcmi_network_pkt_stats_info *network_pkt_stats_info) DCMI_WEAK;

#endif

#if defined DCMI_VERSION_1
  /* The following interfaces are V1 version interfaces. In order to ensure the compatibility is temporarily reserved,
* the later version will be deleted. Please switch to the V2 version interface as soon as possible */

  struct dcmi_memory_info_stru {
    unsigned long long memory_size;
    unsigned int freq;
    unsigned int utiliza;
  };

  struct dsmi_computing_power_info {
    unsigned int data1;
    unsigned int reserve[3];
  };

  DCMIDLLEXPORT int dcmi_get_memory_info(int card_id, int device_id,
                                         struct dcmi_memory_info_stru *device_memory_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_flash_count(int card_id, int device_id, unsigned int *flash_count) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_device_flash_info_v2(int card_id, int device_id, unsigned int flash_index,
                                                  struct dcmi_flash_info *flash_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_device_flash_info(int card_id, int device_id, unsigned int flash_index,
                                               struct dcmi_flash_info_stru *flash_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_multi_ecc_time_info_v2(int card_id, int device_id,
                                                    struct dcmi_multi_ecc_time_data *multi_ecc_time_data) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_multi_ecc_record_info_v2(int card_id, int device_id, struct dcmi_ecc_record_type type,
                                                      unsigned int *ecc_count,
                                                      struct dcmi_ecc_common_data *ecc_common_data_s) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_affinity_cpu_info_by_device_id(int card_id, int device_id, char *affinity_cpu,
                                                            int *length) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_netdev_tc_stat_info(int card_id, int device_id,
                                                 struct dcmi_tc_stat_data *network_tc_stat_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_vrd_version(int card_id, char *version, int len) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_system_time(int card_id, int device_id, unsigned int *system_time) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_aicore_info(int card_id, int device_id,
                                         struct dsmi_aicore_info_stru *aicore_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_aicpu_info(int card_id, int device_id, struct dsmi_aicpu_info_stru *aicpu_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_p2p_enable(int card_id, int device_id, int *enable_flag) DCMI_WEAK;
  DCMIDLLEXPORT int dcmi_get_computing_power_info(int card_id, int device_id, int type,
                                                  struct dsmi_computing_power_info *computing_power) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_errorcode(
      int card_id, int device_id, int *error_count, unsigned int *error_code, int *error_width);

  DCMIDLLEXPORT int dcmi_mcu_get_power_info(int card_id, int *power);
#endif

#ifdef __cplusplus
#if __cplusplus
}
#endif
#endif /* __cplusplus */

#endif /* __DCMI_INTERFACE_API_H__ */
