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

#ifdef __linux
#define DCMIDLLEXPORT
#else
#define DCMIDLLEXPORT _declspec(dllexport)
#endif

#define MAX_CARD_NUM 64
#define MAX_CHIP_NAME_LEN 32  // Maximum length of chip name
#define TEMPLATE_NAME_LEN 32
#define DIE_ID_COUNT 5  // Number of die ID characters

#define    DCMI_UTILIZATION_RATE_DDR             1
#define    DCMI_UTILIZATION_RATE_AICORE          2
#define    DCMI_UTILIZATION_RATE_AICPU           3
#define    DCMI_UTILIZATION_RATE_CTRLCPU         4
#define    DCMI_UTILIZATION_RATE_DDR_BANDWIDTH   5
#define    DCMI_UTILIZATION_RATE_HBM             6
#define    DCMI_UTILIZATION_RATE_HBM_BANDWIDTH   10
#define    DCMI_UTILIZATION_RATE_VECTORCORE      12

  /*----------------------------------------------*
* Structure description                        *
*----------------------------------------------*/
  struct dcmi_chip_info {
    unsigned char chip_type[MAX_CHIP_NAME_LEN];
    unsigned char chip_name[MAX_CHIP_NAME_LEN];
    unsigned char chip_ver[MAX_CHIP_NAME_LEN];
    unsigned int aicore_cnt;
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

  struct dcmi_die_id {
    unsigned int soc_die[DIE_ID_COUNT];
  };

  struct dcmi_hbm_info {
    unsigned long long memory_size;
    unsigned int freq;
    unsigned long long memory_usage;
    int temp;
    unsigned int bandwith_util_rate;
  };

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
    DCMI_MAIN_CMD_TEMP = 50,
    DCMI_MAIN_CMD_SVM = 51,
    DCMI_MAIN_CMD_VDEV_MNG,
    DCMI_MAIN_CMD_DEVICE_SHARE = 0x8001,
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

  enum dcmi_reset_channel {
    OUTBAND_CHANNEL = 0, // out-of-band reset
    INBAND_CHANNEL // in-band reset
  };

  enum dcmi_boot_status {
    DCMI_BOOT_STATUS_UNINIT = 0, // not init
    DCMI_BOOT_STATUS_BIOS, // BIOS starting
    DCMI_BOOT_STATUS_OS, // OS starting
    DCMI_BOOT_STATUS_FINISH // started
  };

  enum dcmi_event_type {
    DCMI_DMS_FAULT_EVENT = 0,
  };

  enum dcmi_die_type {
    NDIE,
    VDIE
  };

#define DCMI_VDEV_RES_NAME_LEN 16
#define DCMI_VDEV_SIZE 20
#define DCMI_VDEV_FOR_RESERVE 32
#define DCMI_SOC_SPLIT_MAX 32
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
    unsigned char resv[DCMI_MAX_EVENT_RESV_LENGTH]; /**< Reserves 32 bytes */
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

  struct dsmi_hbm_info_stru {
    unsigned long long memory_size;      /**< HBM total size, KB */
    unsigned int freq;                   /**< HBM freq, MHZ */
    unsigned long long memory_usage;     /**< HBM memory_usage, KB */
    int temp;                            /**< HBM temperature */
    unsigned int bandwith_util_rate;
  };

#define DCMI_VERSION_1
#define DCMI_VERSION_2

#if defined DCMI_VERSION_2

  DCMIDLLEXPORT int dcmi_init(void);

  DCMIDLLEXPORT int dcmi_get_card_list(int *card_num, int *card_list, int list_len);

  DCMIDLLEXPORT int dcmi_get_device_num_in_card(int card_id, int *device_num);

  DCMIDLLEXPORT int dcmi_get_device_id_in_card(int card_id, int *device_id_max, int *mcu_id, int *cpu_id);

  DCMIDLLEXPORT int dcmi_get_device_type(int card_id, int device_id, enum dcmi_unit_type *device_type);

  DCMIDLLEXPORT int dcmi_get_device_pcie_info_v2(int card_id, int device_id, struct dcmi_pcie_info_all *pcie_info);

  DCMIDLLEXPORT int dcmi_get_device_chip_info(int card_id, int device_id, struct dcmi_chip_info *chip_info);

  DCMIDLLEXPORT int dcmi_get_device_power_info(int card_id, int device_id, int *power);

  DCMIDLLEXPORT int dcmi_get_device_health(int card_id, int device_id, unsigned int *health);

  DCMIDLLEXPORT int dcmi_get_device_errorcode_v2(
      int card_id, int device_id, int *error_count, unsigned int *error_code_list, unsigned int list_len);

  DCMIDLLEXPORT int dcmi_get_device_temperature(int card_id, int device_id, int *temperature);

  DCMIDLLEXPORT int dcmi_get_device_voltage(int card_id, int device_id, unsigned int *voltage);

  DCMIDLLEXPORT int dcmi_get_device_frequency(
      int card_id, int device_id, enum dcmi_freq_type input_type, unsigned int *frequency);

  DCMIDLLEXPORT int dcmi_get_device_hbm_info(int card_id, int device_id, struct dcmi_hbm_info *hbm_info);

  DCMIDLLEXPORT int dcmi_get_device_memory_info_v3(int card_id, int device_id,
                                                   struct dcmi_get_memory_info_stru *memory_info);

  DCMIDLLEXPORT int dcmi_get_device_utilization_rate(
      int card_id, int device_id, int input_type, unsigned int *utilization_rate);

  DCMIDLLEXPORT int dcmi_get_device_info(
      int card_id, int device_id, enum dcmi_main_cmd main_cmd, unsigned int sub_cmd, void *buf, unsigned int *size);

  DCMIDLLEXPORT int dcmi_get_device_ip(int card_id, int device_id, enum dcmi_port_type input_type, int port_id,
                                       struct dcmi_ip_addr *ip, struct dcmi_ip_addr *mask);

  DCMIDLLEXPORT int dcmi_get_device_network_health(int card_id, int device_id, enum dcmi_rdfx_detect_result *result);

  DCMIDLLEXPORT int dcmi_get_device_logic_id(int *device_logic_id, int card_id, int device_id);

  DCMIDLLEXPORT int dcmi_create_vdevice(int card_id, int device_id, struct dcmi_create_vdev_res_stru *vdev,
                                        struct dcmi_create_vdev_out *out);

  DCMIDLLEXPORT int dcmi_set_destroy_vdevice(int card_id, int device_id, unsigned int vdevid);

  DCMIDLLEXPORT int dcmi_get_device_phyid_from_logicid(unsigned int logicid, unsigned int *phyid);

  DCMIDLLEXPORT int dcmi_get_device_logicid_from_phyid(unsigned int phyid, unsigned int *logicid);

  DCMIDLLEXPORT int dcmi_get_card_id_device_id_from_logicid(int *card_id, int *device_id, unsigned int device_logic_id);

  DCMIDLLEXPORT int dcmi_get_card_id_device_id_from_phyid(int *card_id, int *device_id, unsigned int device_phy_id);

  DCMIDLLEXPORT int dcmi_get_product_type(int card_id, int device_id, char *product_type_str, int buf_size);

  DCMIDLLEXPORT int dcmi_set_device_reset(int card_id, int device_id, enum dcmi_reset_channel channel_type);

  DCMIDLLEXPORT int dcmi_get_device_boot_status(int card_id, int device_id, enum dcmi_boot_status *boot_status);

  DCMIDLLEXPORT int dcmi_subscribe_fault_event(int card_id, int device_id, struct dcmi_event_filter filter);

  DCMIDLLEXPORT int dcmi_get_npu_work_mode(int card_id, unsigned char *work_mode);

  DCMIDLLEXPORT int dcmi_get_device_die_v2(
      int card_id, int device_id, enum dcmi_die_type input_type, struct dcmi_die_id *die_id);

  DCMIDLLEXPORT int dcmi_get_device_resource_info (int card_id, int device_id, struct dcmi_proc_mem_info *proc_info,
                                                  int *proc_num);

  DCMIDLLEXPORT int dcmi_get_device_board_info (int card_id, int device_id, struct dcmi_board_info *board_info);

  DCMIDLLEXPORT int dcmi_get_hbm_info(int card_id, int device_id, struct dsmi_hbm_info_stru *device_hbm_info);

#endif

#if defined DCMI_VERSION_1
  /* The following interfaces are V1 version interfaces. In order to ensure the compatibility is temporarily reserved,
* the later version will be deleted. Please switch to the V2 version interface as soon as possible */

  struct dcmi_memory_info_stru {
    unsigned long long memory_size;
    unsigned int freq;
    unsigned int utiliza;
  };

  DCMIDLLEXPORT int dcmi_get_memory_info(int card_id, int device_id, struct dcmi_memory_info_stru *device_memory_info);

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