# NVTOP Development Guide for AI Coding Agents

## Project Overview

**NVTOP** (Neat Videocard TOP) is a cross-platform GPU monitoring tool similar to htop, supporting NVIDIA, AMD, Intel, and various embedded GPUs. This is a **Windows port** on the `windows` branch of the upstream project.

### Key Architecture: Plugin-Based GPU Vendor System

NVTOP uses a **vendor plugin architecture** where each GPU vendor (NVIDIA, AMD, Intel, etc.) is registered as a separate module:

1. **Registration Pattern**: Each vendor module uses `__attribute__((constructor))` to auto-register via `register_gpu_vendor()` at startup
2. **Core Interface**: All vendors implement `struct gpu_vendor` callbacks defined in `include/nvtop/extract_gpuinfo_common.h`
3. **Platform Separation**: Vendor implementations are split by platform:
   - Linux: `extract_gpuinfo_<vendor>.c` (uses DRM/sysfs/fdinfo)
   - Windows: `extract_gpuinfo_<vendor>_windows.c` (uses vendor SDKs)
   - macOS: `extract_gpuinfo_apple.m` (uses Metal framework)

**Example vendor files**: `extract_gpuinfo_nvidia.c` (Linux), `extract_gpuinfo_nvidia_windows.c` (Windows), `extract_gpuinfo_amdgpu.c`, `extract_gpuinfo_intel.c`

### Windows-Specific Implementation

The Windows port required creating platform abstraction layers in `src/`:
- **`windows_compat.c`**: POSIX compatibility (getopt, signal handling, path separators)
- **`get_process_info_windows.c`**: Process enumeration using Win32 APIs (instead of /proc)
- **`info_messages_windows.c`**: System info via Windows APIs
- **NVML Dynamic Loading**: `extract_gpuinfo_nvidia_windows.c` loads `nvml.dll` at runtime to avoid CUDA Toolkit build dependency

## Build System Conventions

### CMake Build Types by Platform

```cmake
# Platform defaults set in root CMakeLists.txt:
if(WIN32)
  # Windows: NVIDIA only by default
  NVIDIA_SUPPORT=ON, all others OFF
elseif(APPLE)
  # macOS: Metal only
  APPLE_SUPPORT=ON, all others OFF
else()
  # Linux: Multi-vendor by default
  NVIDIA_SUPPORT=ON, AMDGPU_SUPPORT=ON, INTEL_SUPPORT=ON, etc.
endif()
```

### Build Commands

**Windows Native (MinGW/MSYS2)**:
```powershell
# Quick automated build
.\build-windows-native.ps1 -All

# Manual build
mkdir build-windows && cd build-windows
$env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"
cmake .. -G "Ninja" -DCMAKE_C_COMPILER=gcc -DNVIDIA_SUPPORT=ON
ninja
```

**Linux**:
```bash
mkdir build && cd build
cmake .. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON
make -j$(nproc)
```

**WSL2 (Windows alternative)**:
```bash
# Most compatible option for Windows users
# See WINDOWS_BUILD.md for full setup
sudo apt install cmake libncurses5-dev libdrm-dev
mkdir build && cd build
cmake .. -DNVIDIA_SUPPORT=ON
make
```

### Source File Conditional Compilation

The build system in `src/CMakeLists.txt` conditionally includes sources based on platform and vendor flags:

```cmake
if(WIN32)
  target_sources(nvtop PRIVATE get_process_info_windows.c windows_compat.c)
elseif(UNIX AND NOT APPLE)
  target_sources(nvtop PRIVATE get_process_info_linux.c extract_processinfo_fdinfo.c)
endif()

if(NVIDIA_SUPPORT)
  if(WIN32)
    target_sources(nvtop PRIVATE extract_gpuinfo_nvidia_windows.c)
  else()
    target_sources(nvtop PRIVATE extract_gpuinfo_nvidia.c)
  endif()
endif()
```

## Critical Developer Knowledge

### GPU Info Data Flow

1. **Initialization**: `extract_gpuinfo.c:gpuinfo_init_info_extraction()` calls each vendor's `init()` and `get_device_handles()`
2. **Static Info**: One-time device properties (name, PCIe capabilities) via `populate_static_info()`
3. **Dynamic Info**: Per-refresh metrics (utilization, memory, temp) via `refresh_dynamic_info()`
4. **Process Tracking**: 
   - **Linux**: Uses fdinfo callback registration (`processinfo_register_fdinfo_callback()`)
   - **Windows**: Direct process enumeration each refresh cycle
5. **UI Rendering**: `interface.c` reads from `struct gpu_info` list for ncurses display

### Adding New GPU Vendor Support

1. Create `src/extract_gpuinfo_<vendor>.c` implementing `struct gpu_vendor` callbacks
2. Add CMake option in root `CMakeLists.txt` (e.g., `option(NEWVENDOR_SUPPORT ...)`)
3. Add conditional compilation in `src/CMakeLists.txt`
4. Use `__attribute__((constructor))` to call `register_gpu_vendor(&gpu_vendor_newvendor)`
5. Implement at minimum: `init()`, `get_device_handles()`, `populate_static_info()`, `refresh_dynamic_info()`

**Reference implementation**: `src/extract_gpuinfo_v3d.c` (simple, well-structured example)

### Windows Porting Patterns

When adapting Linux code for Windows:

1. **Include guards**: Wrap `#include <unistd.h>`, `<signal.h>`, etc. with `#ifndef _WIN32`
2. **Path handling**: Use `windows_compat.h` functions for path normalization
3. **Process info**: Windows has no /proc - use `CreateToolhelp32Snapshot()` + `Process32First/Next()`
4. **Dynamic library loading**: Use `LoadLibraryA()` + `GetProcAddress()` instead of dlopen
5. **ncurses**: Windows build uses PDCurses/ncursesw from MSYS2

**Example**: See `src/nvtop.c` lines 22-39 for conditional compilation pattern

### Testing Strategy

- **Manual Testing**: Run `nvtop` binary with GPUs present, verify metrics match vendor tools (nvidia-smi, radeontop)
- **Unit Tests**: `tests/interfaceTests.cpp` for UI components (currently limited coverage)
- **Cross-platform verification**: Test same code on Linux/Windows/WSL2 to catch platform-specific issues
- **No automated GPU tests**: Hardware-dependent nature makes CI difficult

## Common Pitfalls

1. **Forgetting to register vendor**: If new GPU vendor isn't detected, check `__attribute__((constructor))` is calling `register_gpu_vendor()`
2. **ncurses header paths on Windows**: Use `-DCMAKE_PREFIX_PATH=C:/msys64/ucrt64` or add `target_include_directories()` for ncursesw
3. **NVML.dll not found**: On Windows, either install CUDA Toolkit or copy nvml.dll to exe directory
4. **Mixing build directories**: Clean builds when switching between MinGW/MSVC: `Remove-Item -Recurse build-windows*`
5. **Vendor-specific #defines**: Check CMake sets `-DNVML_SUPPORT`, `-DAMDGPU_SUPPORT` etc. in compile definitions

## Windows Build Documentation Map

- **Quick Start**: `README_WINDOWS.md` - User-facing setup guide
- **Native Build Details**: `WINDOWS_NATIVE_BUILD.md` - MinGW/MSVC build instructions
- **WSL2 Alternative**: `WINDOWS_BUILD.md` - Recommended for Linux-like environment
- **Build Scripts**: `build-windows-native.ps1` (automated), `build-windows-native.bat` (simple wrapper)
- **Troubleshooting**: See "Troubleshooting" sections in each doc above

## Key Files for Understanding Codebase

- `include/nvtop/extract_gpuinfo_common.h` - Core data structures (`gpu_info`, `gpu_vendor`, `gpu_process`)
- `src/extract_gpuinfo.c` - Vendor orchestration and main extraction loop
- `src/interface.c` - ncurses UI rendering (2000+ lines, handles all display logic)
- `src/nvtop.c` - Main entry point, command-line parsing, signal handling
- `CMakeLists.txt` + `src/CMakeLists.txt` - Build configuration with per-vendor conditionals

## Current Windows Port Status (as of branch)

✅ **Working**: NVIDIA GPU monitoring, basic process list, ncurses UI, MinGW build  
⚠️ **Limited**: Process details less complete than Linux (no fdinfo equivalent)  
❌ **Not Implemented**: AMD GPU support (needs ADL SDK), Intel GPU support (needs Intel GPU APIs)

See `BUILD_STATUS.md` for detailed Windows build state.
