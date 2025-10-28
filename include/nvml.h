// Minimal NVML header stub for Windows compilation
// This allows compilation without CUDA Toolkit installed
// The actual functions are loaded dynamically at runtime

#ifndef __nvml_nvml_h__
#define __nvml_nvml_h__

#ifdef __cplusplus
extern "C" {
#endif

// NVML Return values
typedef enum nvmlReturn_enum {
  NVML_SUCCESS = 0,
  NVML_ERROR_UNINITIALIZED = 1,
  NVML_ERROR_INVALID_ARGUMENT = 2,
  NVML_ERROR_NOT_SUPPORTED = 3,
  NVML_ERROR_NO_PERMISSION = 4,
  NVML_ERROR_ALREADY_INITIALIZED = 5,
  NVML_ERROR_NOT_FOUND = 6,
  NVML_ERROR_INSUFFICIENT_SIZE = 7,
  NVML_ERROR_INSUFFICIENT_POWER = 8,
  NVML_ERROR_DRIVER_NOT_LOADED = 9,
  NVML_ERROR_TIMEOUT = 10,
  NVML_ERROR_IRQ_ISSUE = 11,
  NVML_ERROR_LIBRARY_NOT_FOUND = 12,
  NVML_ERROR_FUNCTION_NOT_FOUND = 13,
  NVML_ERROR_CORRUPTED_INFOROM = 14,
  NVML_ERROR_GPU_IS_LOST = 15,
  NVML_ERROR_RESET_REQUIRED = 16,
  NVML_ERROR_OPERATING_SYSTEM = 17,
  NVML_ERROR_LIB_RM_VERSION_MISMATCH = 18,
  NVML_ERROR_IN_USE = 19,
  NVML_ERROR_MEMORY = 20,
  NVML_ERROR_NO_DATA = 21,
  NVML_ERROR_VGPU_ECC_NOT_SUPPORTED = 22,
  NVML_ERROR_INSUFFICIENT_RESOURCES = 23,
  NVML_ERROR_UNKNOWN = 999
} nvmlReturn_t;

// Opaque device handle
typedef struct nvmlDevice_st *nvmlDevice_t;

// Constants
#define NVML_DEVICE_NAME_BUFFER_SIZE 64
#define NVML_DEVICE_UUID_BUFFER_SIZE 80

// Memory info structure
typedef struct nvmlMemory_st {
  unsigned long long total;
  unsigned long long free;
  unsigned long long used;
} nvmlMemory_t;

// Utilization rates
typedef struct nvmlUtilization_st {
  unsigned int gpu;
  unsigned int memory;
} nvmlUtilization_t;

// Temperature sensors
typedef enum nvmlTemperatureSensors_enum { NVML_TEMPERATURE_GPU = 0 } nvmlTemperatureSensors_t;

// Clock types
typedef enum nvmlClockType_enum {
  NVML_CLOCK_GRAPHICS = 0,
  NVML_CLOCK_SM = 1,
  NVML_CLOCK_MEM = 2,
  NVML_CLOCK_VIDEO = 3
} nvmlClockType_t;

// PCIe info
typedef struct nvmlPciInfo_st {
  char busId[16];
  unsigned int domain;
  unsigned int bus;
  unsigned int device;
  unsigned int pciDeviceId;
  unsigned int pciSubSystemId;
} nvmlPciInfo_t;

// Process info
typedef struct nvmlProcessInfo_st {
  unsigned int pid;
  unsigned long long usedGpuMemory;
} nvmlProcessInfo_t;

// Function declarations (will be loaded dynamically)
typedef nvmlReturn_t (*nvmlInit_v2_t)(void);
typedef nvmlReturn_t (*nvmlShutdown_t)(void);
typedef nvmlReturn_t (*nvmlDeviceGetCount_v2_t)(unsigned int *deviceCount);
typedef nvmlReturn_t (*nvmlDeviceGetHandleByIndex_v2_t)(unsigned int index, nvmlDevice_t *device);
typedef nvmlReturn_t (*nvmlDeviceGetName_t)(nvmlDevice_t device, char *name, unsigned int length);
typedef nvmlReturn_t (*nvmlDeviceGetMemoryInfo_t)(nvmlDevice_t device, nvmlMemory_t *memory);
typedef nvmlReturn_t (*nvmlDeviceGetUtilizationRates_t)(nvmlDevice_t device, nvmlUtilization_t *utilization);
typedef nvmlReturn_t (*nvmlDeviceGetTemperature_t)(nvmlDevice_t device, nvmlTemperatureSensors_t sensorType,
                                                   unsigned int *temp);
typedef nvmlReturn_t (*nvmlDeviceGetPowerUsage_t)(nvmlDevice_t device, unsigned int *power);
typedef nvmlReturn_t (*nvmlDeviceGetClockInfo_t)(nvmlDevice_t device, nvmlClockType_t type, unsigned int *clock);
typedef nvmlReturn_t (*nvmlDeviceGetPciInfo_v3_t)(nvmlDevice_t device, nvmlPciInfo_t *pci);

#ifdef __cplusplus
}
#endif

#endif // __nvml_nvml_h__
