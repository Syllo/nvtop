# AMD and Intel GPU Support for Windows

## Current Status

**NVIDIA**: ✅ Fully implemented via NVML (NVIDIA Management Library)
**AMD**: ❌ Not yet implemented for Windows
**Intel**: ❌ Not yet implemented for Windows

## Implementation Requirements

### AMD GPU Support

AMD GPU monitoring on Windows requires the **AMD Display Library (ADL)** or **AMD GPU Services (AGS)** SDK.

#### ADL SDK
- **Download**: https://github.com/GPUOpen-LibrariesAndSDKs/display-library
- **License**: MIT
- **Features**:
  - GPU utilization
  - Memory usage
  - Temperature
  - Fan speed
  - Clock speeds
  - Power consumption

#### Implementation Steps
1. Download ADL SDK and extract to `include/adl/`
2. Create `src/extract_gpuinfo_amdgpu_windows.c` following NVIDIA pattern
3. Dynamically load `atiadlxx.dll` (64-bit) or `atiadlxy.dll` (32-bit)
4. Implement vendor callbacks:
   - `init()` - Initialize ADL
   - `get_device_handles()` - Enumerate AMD GPUs
   - `populate_static_info()` - Get GPU name, memory capacity
   - `refresh_dynamic_info()` - Get utilization, temp, fan, power
   - `refresh_running_processes()` - Enumerate GPU processes (via Windows APIs)

#### Key ADL Functions
```c
ADL_Main_Control_Create()
ADL_Adapter_NumberOfAdapters_Get()
ADL_Adapter_AdapterInfo_Get()
ADL_Overdrive5_CurrentActivity_Get()      // GPU/Mem utilization
ADL_Overdrive5_Temperature_Get()          // Temperature
ADL_Overdrive5_FanSpeed_Get()             // Fan speed
ADL_Overdrive6_CurrentPower_Get()         // Power consumption
ADL_Main_Control_Destroy()
```

### Intel GPU Support

Intel GPU monitoring on Windows can use **Windows Performance Counters** or Intel's **Graphics Performance Analyzers** APIs.

#### Windows Performance Counters Approach
- Use PDH (Performance Data Helper) API
- Performance counter path: `\GPU Engine(*)\Utilization Percentage`
- Available in Windows 10+
- No external SDK required

#### Implementation Steps
1. Create `src/extract_gpuinfo_intel_windows.c`
2. Use Windows Performance Counters (PDH API)
3. Enumerate Intel GPUs via DXGI (DirectX Graphics Infrastructure)
4. Query metrics via PDH or WMI

#### Key Windows APIs
```c
// DXGI for device enumeration
IDXGIFactory_CreateDXGIFactory()
IDXGIAdapter_GetDesc()

// PDH for performance counters
PdhOpenQuery()
PdhAddCounter()
PdhCollectQueryData()
PdhGetFormattedCounterValue()
```

#### Alternative: Intel GPU Performance API
- More detailed metrics
- Requires Intel GPU drivers with performance monitoring enabled
- Less portable across Intel GPU generations

## Build System Changes

### CMake Configuration

Update `CMakeLists.txt` for Windows:

```cmake
if(WIN32)
  set(NVIDIA_SUPPORT_DEFAULT ON)
  set(AMDGPU_SUPPORT_DEFAULT ON)   # Enable AMD
  set(INTEL_SUPPORT_DEFAULT ON)    # Enable Intel
endif()
```

### Source Files

Add to `src/CMakeLists.txt`:

```cmake
if(AMDGPU_SUPPORT)
  if(WIN32)
    target_sources(nvtop PRIVATE extract_gpuinfo_amdgpu_windows.c)
  else()
    target_sources(nvtop PRIVATE extract_gpuinfo_amdgpu.c extract_gpuinfo_amdgpu_utils.c)
  endif()
endif()

if(INTEL_SUPPORT)
  if(WIN32)
    target_sources(nvtop PRIVATE extract_gpuinfo_intel_windows.c)
  else()
    target_sources(nvtop PRIVATE extract_gpuinfo_intel.c extract_gpuinfo_intel_i915.c extract_gpuinfo_intel_xe.c)
  endif()
endif()
```

## Testing Requirements

### AMD Testing
- Requires AMD GPU hardware (Radeon RX series)
- Test with different GPU generations (RDNA, RDNA2, RDNA3)
- Verify ADL SDK compatibility with current drivers

### Intel Testing
- Requires Intel GPU hardware (Arc discrete or integrated)
- Test on Windows 10 and Windows 11
- Verify performance counter availability

## Development Priority

**Phase 1** (Current): NVIDIA support ✅
**Phase 2** (Next): Intel support (easier - uses Windows APIs)
**Phase 3** (Future): AMD support (requires ADL SDK integration)

## References

- [AMD ADL SDK Documentation](https://gpuopen.com/adl/)
- [Windows Performance Counters](https://docs.microsoft.com/en-us/windows/win32/perfctrs/performance-counters-portal)
- [DXGI Documentation](https://docs.microsoft.com/en-us/windows/win32/direct3ddxgi/dx-graphics-dxgi)
- [Intel Graphics Drivers](https://www.intel.com/content/www/us/en/download/19344/intel-graphics-windows-dch-drivers.html)

## Community Contributions Welcome

This is a significant undertaking and community contributions are welcome! If you have AMD or Intel GPU development experience and want to contribute, please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amd-windows-support`)
3. Implement the vendor-specific code
4. Test with real hardware
5. Submit a pull request

## Workaround for Multi-Vendor Systems

If you have multiple GPU vendors (e.g., NVIDIA + AMD), you can:
1. Use NVTOP for NVIDIA GPUs (fully supported)
2. Use vendor-specific tools for others:
   - AMD: AMD Radeon Software (Ctrl+Shift+O)
   - Intel: Intel Graphics Command Center

## License Considerations

- **NVML**: Distributed with NVIDIA drivers, no SDK license restrictions
- **ADL SDK**: MIT License (permissive, compatible with GPLv3)
- **Windows APIs**: Part of Windows SDK, no licensing issues

All implementations should maintain GPLv3 compatibility.
