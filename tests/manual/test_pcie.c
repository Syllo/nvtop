// Test program to verify PCIe monitoring works
#include "../include/nvml.h"
#include <stdio.h>
#include <windows.h>

// NVML function typedefs
typedef nvmlReturn_t (*nvmlInit_v2_t)(void);
typedef nvmlReturn_t (*nvmlShutdown_t)(void);
typedef nvmlReturn_t (*nvmlDeviceGetCount_v2_t)(unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetHandleByIndex_v2_t)(unsigned int, nvmlDevice_t *);
typedef nvmlReturn_t (*nvmlDeviceGetName_t)(nvmlDevice_t, char *, unsigned int);
typedef nvmlReturn_t (*nvmlDeviceGetMaxPcieLinkGeneration_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetMaxPcieLinkWidth_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetCurrPcieLinkGeneration_t)(nvmlDevice_t, unsigned int *);
typedef nvmlReturn_t (*nvmlDeviceGetCurrPcieLinkWidth_t)(nvmlDevice_t, unsigned int *);

int main(void) {
  HMODULE nvml_handle = LoadLibraryA("nvml.dll");
  if (!nvml_handle) {
    printf("Failed to load nvml.dll (error %lu)\n", GetLastError());
    return 1;
  }

  nvmlInit_v2_t init = (nvmlInit_v2_t)GetProcAddress(nvml_handle, "nvmlInit_v2");
  nvmlShutdown_t shutdown = (nvmlShutdown_t)GetProcAddress(nvml_handle, "nvmlShutdown");
  nvmlDeviceGetCount_v2_t getCount = (nvmlDeviceGetCount_v2_t)GetProcAddress(nvml_handle, "nvmlDeviceGetCount_v2");
  nvmlDeviceGetHandleByIndex_v2_t getHandle =
      (nvmlDeviceGetHandleByIndex_v2_t)GetProcAddress(nvml_handle, "nvmlDeviceGetHandleByIndex_v2");
  nvmlDeviceGetName_t getName = (nvmlDeviceGetName_t)GetProcAddress(nvml_handle, "nvmlDeviceGetName");
  nvmlDeviceGetMaxPcieLinkGeneration_t getMaxGen =
      (nvmlDeviceGetMaxPcieLinkGeneration_t)GetProcAddress(nvml_handle, "nvmlDeviceGetMaxPcieLinkGeneration");
  nvmlDeviceGetMaxPcieLinkWidth_t getMaxWidth =
      (nvmlDeviceGetMaxPcieLinkWidth_t)GetProcAddress(nvml_handle, "nvmlDeviceGetMaxPcieLinkWidth");
  nvmlDeviceGetCurrPcieLinkGeneration_t getCurrGen =
      (nvmlDeviceGetCurrPcieLinkGeneration_t)GetProcAddress(nvml_handle, "nvmlDeviceGetCurrPcieLinkGeneration");
  nvmlDeviceGetCurrPcieLinkWidth_t getCurrWidth =
      (nvmlDeviceGetCurrPcieLinkWidth_t)GetProcAddress(nvml_handle, "nvmlDeviceGetCurrPcieLinkWidth");

  if (!init || !getCount || !getHandle || !getName || !getMaxGen || !getMaxWidth || !getCurrGen || !getCurrWidth) {
    printf("Failed to load NVML functions\n");
    FreeLibrary(nvml_handle);
    return 1;
  }

  nvmlReturn_t result = init();
  if (result != NVML_SUCCESS) {
    printf("Failed to initialize NVML (error %d)\n", result);
    FreeLibrary(nvml_handle);
    return 1;
  }

  unsigned int device_count = 0;
  result = getCount(&device_count);
  if (result != NVML_SUCCESS) {
    printf("Failed to get device count (error %d)\n", result);
    shutdown();
    FreeLibrary(nvml_handle);
    return 1;
  }

  printf("Found %u NVIDIA GPU(s)\n\n", device_count);

  for (unsigned int i = 0; i < device_count; i++) {
    nvmlDevice_t device;
    result = getHandle(i, &device);
    if (result != NVML_SUCCESS) {
      printf("Failed to get handle for device %u\n", i);
      continue;
    }

    char name[NVML_DEVICE_NAME_BUFFER_SIZE];
    result = getName(device, name, sizeof(name));
    if (result != NVML_SUCCESS) {
      printf("Failed to get name for device %u\n", i);
      continue;
    }

    printf("Device %u: %s\n", i, name);

    unsigned int max_gen = 0, max_width = 0, curr_gen = 0, curr_width = 0;

    result = getMaxGen(device, &max_gen);
    if (result == NVML_SUCCESS) {
      printf("  Max PCIe Gen: %u\n", max_gen);
    } else {
      printf("  Max PCIe Gen: N/A (error %d)\n", result);
    }

    result = getMaxWidth(device, &max_width);
    if (result == NVML_SUCCESS) {
      printf("  Max PCIe Width: x%u\n", max_width);
    } else {
      printf("  Max PCIe Width: N/A (error %d)\n", result);
    }

    result = getCurrGen(device, &curr_gen);
    if (result == NVML_SUCCESS) {
      printf("  Current PCIe Gen: %u\n", curr_gen);
    } else {
      printf("  Current PCIe Gen: N/A (error %d)\n", result);
    }

    result = getCurrWidth(device, &curr_width);
    if (result == NVML_SUCCESS) {
      printf("  Current PCIe Width: x%u\n", curr_width);
    } else {
      printf("  Current PCIe Width: N/A (error %d)\n", result);
    }

    printf("\n");
  }

  shutdown();
  FreeLibrary(nvml_handle);

  return 0;
}
