# Native Windows Build Guide for NVTOP

This guide explains how to build NVTOP natively on Windows 10/11 without WSL2.

## ⚠️ Important Notes

**Current Status**: The native Windows build is **EXPERIMENTAL** and has the following limitations:

- ✅ **NVIDIA GPU Support**: Functional (requires CUDA Toolkit for nvml.dll)
- ❌ **AMD GPU Support**: Not implemented (would require AMD Display Library SDK)
- ❌ **Intel GPU Support**: Not implemented (would require Intel GPU Performance APIs)
- ✅ **Basic UI**: ncurses interface works with PDCurses
- ⚠️ **Process Monitoring**: Basic Windows process API integration

## Quick Start

### Option 1: Automated Build (Recommended)

```powershell
# Run in PowerShell
.\build-windows-native.ps1 -All
```

This will:
1. Install MSYS2 dependencies (if using MinGW)
2. Configure the build with CMake
3. Build NVTOP executable

### Option 2: Manual Build

See detailed instructions below.

## Prerequisites

### For MinGW Build (Recommended)

1. **MSYS2**: Download and install from https://www.msys2.org/
   - Install to default location: `C:\msys64`
   - After installation, update MSYS2:
     ```bash
     pacman -Syu
     ```

2. **NVIDIA Drivers** (for NVIDIA GPU support):
   - Install latest NVIDIA drivers
   - Install CUDA Toolkit: https://developer.nvidia.com/cuda-downloads
   - This provides `nvml.dll` required for GPU monitoring

### For MSVC Build

1. **Visual Studio 2019 or 2022**:
   - Install "Desktop development with C++" workload
   - Download from: https://visualstudio.microsoft.com/

2. **vcpkg** (for dependencies):
   - Will be automatically installed by build script

3. **NVIDIA Drivers** (same as above)

## Build Instructions

### MinGW Build (Recommended)

#### Step 1: Install Dependencies

Open **MSYS2 UCRT64** terminal and run:

```bash
pacman -S --needed mingw-w64-ucrt-x86_64-toolchain \
                   mingw-w64-ucrt-x86_64-cmake \
                   mingw-w64-ucrt-x86_64-ninja \
                   mingw-w64-ucrt-x86_64-ncurses \
                   mingw-w64-ucrt-x86_64-pdcurses \
                   git
```

#### Step 2: Configure Build

In PowerShell, navigate to nvtop directory:

```powershell
cd C:\Users\adamm\dev\nvtop

# Add MSYS2 to PATH
$env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"

# Create build directory
mkdir build-windows
cd build-windows

# Configure with CMake
cmake .. -G "Ninja" `
    -DCMAKE_C_COMPILER=gcc `
    -DCMAKE_CXX_COMPILER=g++ `
    -DCMAKE_BUILD_TYPE=Release `
    -DNVIDIA_SUPPORT=ON `
    -DAMDGPU_SUPPORT=OFF `
    -DINTEL_SUPPORT=OFF
```

#### Step 3: Build

```powershell
ninja
```

Or use the automated script:

```powershell
.\build-windows-native.ps1 -Compiler MinGW -All
```

### MSVC Build

#### Step 1: Install vcpkg

```powershell
git clone https://github.com/Microsoft/vcpkg.git
cd vcpkg
.\bootstrap-vcpkg.bat
.\vcpkg install pdcurses:x64-windows
cd ..
```

#### Step 2: Configure and Build

Open **Developer Command Prompt for VS 2022**:

```cmd
cd C:\Users\adamm\dev\nvtop
mkdir build-windows-msvc
cd build-windows-msvc

cmake .. -G "Visual Studio 17 2022" -A x64 ^
    -DCMAKE_TOOLCHAIN_FILE="%CD%\..\vcpkg\scripts\buildsystems\vcpkg.cmake" ^
    -DNVIDIA_SUPPORT=ON ^
    -DAMDGPU_SUPPORT=OFF ^
    -DINTEL_SUPPORT=OFF

cmake --build . --config Release
```

Or use the automated script:

```powershell
.\build-windows-native.ps1 -Compiler MSVC -All
```

## Running NVTOP

After successful build:

```powershell
# MinGW build
.\build-windows\src\nvtop.exe

# MSVC build
.\build-windows-msvc\src\Release\nvtop.exe
```

## Troubleshooting

### "nvml.dll not found"

**Solution**: Install CUDA Toolkit which includes nvml.dll

1. Download CUDA Toolkit: https://developer.nvidia.com/cuda-downloads
2. Install with default options
3. nvml.dll will be in: `C:\Program Files\NVIDIA Corporation\NVSMI\`
4. Add to PATH or copy nvml.dll to nvtop directory

### "MSYS2 not found"

**Solution**: Install MSYS2

1. Download: https://www.msys2.org/
2. Install to `C:\msys64`
3. Run MSYS2 UCRT64 terminal
4. Update: `pacman -Syu`

### "CMake not found" (in MSYS2)

**Solution**:
```bash
pacman -S mingw-w64-ucrt-x86_64-cmake
```

### "ncurses errors" or "display issues"

**Solution**: The build uses PDCurses on Windows which has some limitations

- Try different terminal emulators (Windows Terminal recommended)
- Ensure terminal supports UTF-8
- Set environment variable: `set TERM=xterm-256color`

### Build fails with "undefined reference"

**Solution**: Make sure all source files are being compiled

```powershell
# Clean and rebuild
Remove-Item -Recurse -Force build-windows
mkdir build-windows
cd build-windows
cmake .. -G "Ninja" -DNVIDIA_SUPPORT=ON
ninja
```

### GPU not detected

**NVIDIA GPUs**:
1. Ensure NVIDIA drivers are installed
2. Check nvml.dll is accessible:
   ```powershell
   where.exe nvml.dll
   ```
3. Verify GPU with: `nvidia-smi`

**AMD/Intel GPUs**:
- Not currently supported on Windows
- Requires additional development (see "Future Work" section)

## Runtime Requirements

### For End Users

To run the built `nvtop.exe`, users need:

1. **NVIDIA Drivers** (for NVIDIA GPU support)
2. **CUDA Toolkit** or copy `nvml.dll` next to nvtop.exe
3. **Windows 10 version 1809** or later (for modern console support)

### Creating a Distributable Package

```powershell
# Create distribution directory
mkdir nvtop-windows-dist
cd nvtop-windows-dist

# Copy executable
Copy-Item ..\build-windows\src\nvtop.exe .

# Copy required DLLs (from MSYS2)
Copy-Item C:\msys64\ucrt64\bin\libncursesw6.dll .
Copy-Item C:\msys64\ucrt64\bin\libgcc_s_seh-1.dll .
Copy-Item C:\msys64\ucrt64\bin\libwinpthread-1.dll .

# Copy nvml.dll (from CUDA or NVIDIA driver)
Copy-Item "C:\Program Files\NVIDIA Corporation\NVSMI\nvml.dll" .

# Create README
@"
NVTOP for Windows
=================

To run: nvtop.exe

Requirements:
- NVIDIA GPU with recent drivers
- Windows 10/11

Command line options:
- nvtop -h : Show help
- nvtop -s : Snapshot mode (non-interactive)
- nvtop -d N : Set refresh rate (N * 0.1 seconds)

Press 'q' to quit when running
Press 'F2' for settings
"@ | Out-File -Encoding UTF8 README.txt

# Create a ZIP
Compress-Archive -Path * -DestinationPath ..\nvtop-windows.zip
```

## Performance Considerations

- **Refresh Rate**: Default is 1 second, can be adjusted with `-d` option
- **CPU Usage**: Native Windows build should use less CPU than WSL2
- **GPU Polling**: NVML calls have minimal overhead

## Known Limitations

### Current Limitations

1. **Process Monitoring**: 
   - Basic implementation using Windows Process API
   - Less detailed than Linux /proc filesystem
   - May not show all GPU-using processes

2. **GPU Support**:
   - Only NVIDIA GPUs supported
   - AMD/Intel require additional SDK integration

3. **Terminal Compatibility**:
   - Best in Windows Terminal
   - May have display issues in older cmd.exe
   - Some Unicode characters may not render

4. **Admin Privileges**:
   - May require administrator rights for full process info
   - Run as admin for complete functionality

### Compared to WSL2 Version

| Feature            | Native Windows | WSL2          |
| ------------------ | -------------- | ------------- |
| NVIDIA GPU Support | ✅ Full         | ✅ Full        |
| AMD GPU Support    | ❌ None         | ⚠️ Limited     |
| Intel GPU Support  | ❌ None         | ⚠️ Limited     |
| Process Details    | ⚠️ Basic        | ✅ Full        |
| Setup Complexity   | Medium         | Easy          |
| Performance        | ✅ Native       | ⚠️ VM Overhead |

## Future Work

To add full Windows support, the following would need development:

### AMD GPU Support
- Integrate AMD Display Library (ADL) SDK
- Implement Windows-specific AMD monitoring
- Add device discovery for AMD GPUs

### Intel GPU Support
- Integrate Intel GPU Performance APIs
- Implement Intel-specific metrics collection
- Add Intel GPU device detection

### Enhanced Process Monitoring
- Use WMI for detailed process information
- Integrate with Performance Counters
- Add GPU process affinity detection

### UI Improvements
- Better PDCurses integration
- Windows Terminal-specific optimizations
- Handle console resize events

## Contributing

To contribute Windows support improvements:

1. Test on different Windows versions
2. Add AMD/Intel GPU support
3. Improve process monitoring
4. Enhance terminal compatibility
5. Add Windows-specific features

## Support

For issues with native Windows builds:

1. Check this documentation
2. Verify all prerequisites are installed
3. Try clean rebuild: `.\build-windows-native.ps1 -Clean -All`
4. Check build logs for specific errors
5. For WSL2 alternative, see `WINDOWS_BUILD.md`

## License

Same as NVTOP main project (GPLv3)

---

**Recommendation**: For most users, WSL2 provides better compatibility and features. Use native Windows build if you specifically need native performance or cannot use WSL2.
