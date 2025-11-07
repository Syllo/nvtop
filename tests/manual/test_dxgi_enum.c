// Test program to enumerate all DXGI adapters
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>

// Include DXGI with COBJMACROS for C-style interface
#define COBJMACROS
#include <dxgi.h>

#pragma comment(lib, "dxgi.lib")

// Define IID_IDXGIFactory explicitly
static const GUID IID_IDXGIFactory_Local = {
    0x7b7166ec, 0x21c7, 0x44ae, {0xb2, 0x1a, 0xc9, 0xae, 0x32, 0x1a, 0xe3, 0x69}};

int main() {
  IDXGIFactory *pFactory = NULL;
  IDXGIAdapter *pAdapter = NULL;
  HRESULT hr;
  UINT adapter_index = 0;

  printf("Enumerating DXGI adapters...\n\n");

  hr = CreateDXGIFactory(&IID_IDXGIFactory_Local, (void **)&pFactory);
  if (FAILED(hr)) {
    printf("Failed to create DXGI factory: 0x%08X\n", hr);
    return 1;
  }

  while (pFactory->lpVtbl->EnumAdapters(pFactory, adapter_index, &pAdapter) != DXGI_ERROR_NOT_FOUND) {
    DXGI_ADAPTER_DESC desc;
    hr = pAdapter->lpVtbl->GetDesc(pAdapter, &desc);

    if (SUCCEEDED(hr)) {
      char device_name[256];
      WideCharToMultiByte(CP_UTF8, 0, desc.Description, -1, device_name, sizeof(device_name), NULL, NULL);

      printf("Adapter %u:\n", adapter_index);
      printf("  Name: %s\n", device_name);
      printf("  VendorId: 0x%04X ", desc.VendorId);

      // Identify vendor
      if (desc.VendorId == 0x10DE)
        printf("(NVIDIA)\n");
      else if (desc.VendorId == 0x1002)
        printf("(AMD)\n");
      else if (desc.VendorId == 0x8086)
        printf("(Intel)\n");
      else
        printf("(Unknown)\n");

      printf("  DeviceId: 0x%04X\n", desc.DeviceId);
      printf("  Dedicated Video Memory: %llu bytes (%.2f MB)\n", desc.DedicatedVideoMemory,
             desc.DedicatedVideoMemory / (1024.0 * 1024.0));
      printf("  Shared System Memory: %llu bytes (%.2f MB)\n", desc.SharedSystemMemory,
             desc.SharedSystemMemory / (1024.0 * 1024.0));
      printf("  SubSysId: 0x%08X\n", desc.SubSysId);
      printf("  Revision: %u\n\n", desc.Revision);
    }

    pAdapter->lpVtbl->Release(pAdapter);
    adapter_index++;
  }

  pFactory->lpVtbl->Release(pFactory);

  if (adapter_index == 0) {
    printf("No DXGI adapters found.\n");
  } else {
    printf("Total adapters found: %u\n", adapter_index);
  }

  return 0;
}
