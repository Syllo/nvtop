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



  struct dcmi_aicore_info {
    unsigned int freq;
    unsigned int cur_freq;
  };


  /* V1 compatibility structures used by the legacy alias entry points.  The
   * fields intentionally keep the names from the CANN ABI; their layout is
   * identical to the dcmi_* structures above. */
  struct dsmi_aicore_info_stru {
    unsigned int freq;
    unsigned int curfreq;
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
    unsigned long long memory_size;      /**< HBM total size, MB */
    unsigned int freq;                   /**< HBM freq, MHZ */
    unsigned long long memory_usage;     /**< HBM memory_usage, MB */
    int temp;                            /**< HBM temperature */
    unsigned int bandwith_util_rate;
  };



  /* The legacy symbol uses a separately named structure in the CANN ABI.
   * Keep the tag distinct so calls are type-checked against the public
   * declaration; both layouts are intentionally identical. */

  /* ECC history records are part of the current CANN ABI.  They are kept
   * separate from dcmi_ecc_info because the latter is a counter snapshot. */


#pragma pack(push, 1)

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



  enum dcmi_unit_type {
    NPU_TYPE = 0,
    MCU_TYPE = 1,
    CPU_TYPE = 2,
    INVALID_TYPE = 0xFF
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


  /* total types of computing resource */





  /* for single search */






  struct dcmi_proc_mem_info {
    int proc_id;
    // unit is byte
    unsigned long proc_mem_usage;
  };





  struct dsmi_hbm_info_stru {
    unsigned long long memory_size;      /**< HBM total size, KB */
    unsigned int freq;                   /**< HBM freq, MHZ */
    unsigned long long memory_usage;     /**< HBM memory_usage, KB */
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
















#define DCMI_VERSION_1
#define DCMI_VERSION_2

#if defined DCMI_VERSION_2

  DCMIDLLEXPORT int dcmi_init(void);



  DCMIDLLEXPORT int dcmi_get_card_list(int *card_num, int *card_list, int list_len);

  DCMIDLLEXPORT int dcmi_get_device_num_in_card(int card_id, int *device_num);


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





  DCMIDLLEXPORT int dcmi_get_device_temperature(int card_id, int device_id, int *temperature) DCMI_WEAK;













  /* Newer physical-device spelling; older releases may only export the
   * dcmi_get_system_time alias below. */

  DCMIDLLEXPORT int dcmi_get_device_multi_utilization_rate(int card_id, int device_id,
                                                           struct dcmi_multi_utilization_info *util_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_sensor_info(int card_id, int device_id, enum dcmi_manager_sensor_id sensor_id,
                                                union dcmi_sensor_info *sensor_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_ecc_info(int card_id, int device_id, enum dcmi_device_type input_type,
                                             struct dcmi_ecc_info *device_ecc_info) DCMI_WEAK;


  DCMIDLLEXPORT int dcmi_get_device_frequency(int card_id, int device_id, enum dcmi_freq_type input_type,
                                              unsigned int *frequency) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_hbm_info(int card_id, int device_id, struct dcmi_hbm_info *hbm_info) DCMI_WEAK;


  DCMIDLLEXPORT int dcmi_get_device_memory_info_v2(int card_id, int device_id,
                                                   struct dcmi_memory_info *memory_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_memory_info_v3(int card_id, int device_id,
                                                   struct dcmi_get_memory_info_stru *memory_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_utilization_rate(int card_id, int device_id, int input_type,
                                                     unsigned int *utilization_rate) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_info(int card_id, int device_id, enum dcmi_main_cmd main_cmd, unsigned int sub_cmd,
                                         void *buf, unsigned int *size) DCMI_WEAK;













  DCMIDLLEXPORT int dcmi_get_device_fan_count(int card_id, int device_id, int *count) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_fan_speed(int card_id, int device_id, int fan_id, int *speed) DCMI_WEAK;






  DCMIDLLEXPORT int dcmi_get_card_id_device_id_from_logicid(int *card_id, int *device_id,
                                                            unsigned int device_logic_id) DCMI_WEAK;








  DCMIDLLEXPORT int dcmi_get_device_resource_info(int card_id, int device_id, struct dcmi_proc_mem_info *proc_info,
                                                  int *proc_num) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_device_dvpp_ratio_info(int card_id, int device_id,
                                                    struct dcmi_dvpp_ratio *usage) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_hbm_info(int card_id, int device_id, struct dsmi_hbm_info_stru *device_hbm_info) DCMI_WEAK;

  DCMIDLLEXPORT int
  dcmi_get_pcie_link_bandwidth_info(int card_id, int device_id,
                                    struct dcmi_pcie_link_bandwidth_info *pcie_link_bandwidth_info) DCMI_WEAK;




  /* Logical-device API introduced by newer CANN releases. These symbols are
   * weak on purpose so the same binary remains compatible with older DCMI
   * libraries that only export the card/device API above. */
  DCMIDLLEXPORT int dcmiv2_init(void) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_device_list(int *device_list, int *device_cnt, int list_len) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_device_chip_info(int dev_id, struct dcmi_chip_info_v2 *chip_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_pcie_info(int dev_id, struct dcmi_pcie_info_all *pcie_info) DCMI_WEAK;


  DCMIDLLEXPORT int dcmiv2_get_device_power_info(int dev_id, int *power_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_temperature(int dev_id, int *temperature) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_aicore_info(int dev_id, struct dcmi_aicore_info *aicore_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_type(int dev_id, enum dcmi_unit_type *device_type) DCMI_WEAK;

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
  DCMIDLLEXPORT int
  dcmiv2_get_pcie_link_bandwidth_info(int dev_id,
                                      struct dcmi_pcie_link_bandwidth_info *pcie_link_bandwidth_info) DCMI_WEAK;
  DCMIDLLEXPORT int dcmiv2_get_device_proc_mem_info(int dev_id, struct dcmi_proc_mem_info *proc_info,
                                                    int *proc_num) DCMI_WEAK;

  DCMIDLLEXPORT int dcmiv2_get_device_info(int dev_id, enum dcmi_main_cmd main_cmd, unsigned int sub_cmd, void *buf,
                                           unsigned int *size) DCMI_WEAK;










#endif

#if defined DCMI_VERSION_1
  /* The following interfaces are V1 version interfaces. In order to ensure the compatibility is temporarily reserved,
* the later version will be deleted. Please switch to the V2 version interface as soon as possible */

  struct dcmi_memory_info_stru {
    unsigned long long memory_size;
    unsigned int freq;
    unsigned int utiliza;
  };


  DCMIDLLEXPORT int dcmi_get_memory_info(int card_id, int device_id,
                                         struct dcmi_memory_info_stru *device_memory_info) DCMI_WEAK;

  DCMIDLLEXPORT int dcmi_get_aicore_info(int card_id, int device_id,
                                         struct dsmi_aicore_info_stru *aicore_info) DCMI_WEAK;


#endif

#ifdef __cplusplus
#if __cplusplus
}
#endif
#endif /* __cplusplus */

#endif /* __DCMI_INTERFACE_API_H__ */
