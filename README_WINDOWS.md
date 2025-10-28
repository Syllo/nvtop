# NVTOP for Windows - Quick Start

🎮 **GPU monitoring tool for Windows 10/11**

## Choose Your Build Method

### 🚀 Fastest: Pre-built Script

```batch
REM Just double-click or run:
build-windows-native.bat
```

### 📦 PowerShell Automated

```powershell
.\build-windows-native.ps1 -All
```

## Prerequisites (One-time Setup)

### Install MSYS2

1. Download: https://www.msys2.org/
2. Install to `C:\msys64` (default)
3. Run "MSYS2 UCRT64" from Start Menu
4. Update MSYS2:
   ```bash
   pacman -Syu
   ```
   (Close and reopen MSYS2 if prompted)
5. Install build tools:
   ```bash
   pacman -S mingw-w64-ucrt-x86_64-toolchain \
             mingw-w64-ucrt-x86_64-cmake \
             mingw-w64-ucrt-x86_64-ninja \
             mingw-w64-ucrt-x86_64-ncurses
   ```

### For NVIDIA GPU Support

1. Install NVIDIA Drivers (latest)
2. Install CUDA Toolkit: https://developer.nvidia.com/cuda-downloads
   - This provides `nvml.dll` needed for GPU monitoring

## Build NVTOP

### Method 1: Batch Script (Easiest)

```batch
build-windows-native.bat
```

### Method 2: PowerShell Script

```powershell
# All-in-one
.\build-windows-native.ps1 -All

# Or step-by-step
.\build-windows-native.ps1 -InstallDeps  # Install dependencies
.\build-windows-native.ps1 -Configure    # Configure build
.\build-windows-native.ps1 -Build        # Build executable
```

### Method 3: Manual

```powershell
# Add MSYS2 to PATH
$env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"

# Configure
mkdir build-windows
cd build-windows
cmake .. -G "Ninja" -DCMAKE_C_COMPILER=gcc -DCMAKE_BUILD_TYPE=Release -DNVIDIA_SUPPORT=ON

# Build
ninja

# Result: build-windows\src\nvtop.exe
```

## Run NVTOP

```batch
build-windows\src\nvtop.exe
```

Or for easier access:

```batch
nvtop.bat
```

## Create Distributable Package

```batch
make-distribution.bat
```

This creates a `nvtop-windows-x64` folder with all required DLLs.

## Usage

```batch
# Basic run
nvtop.exe

# Show help
nvtop.exe -h

# Snapshot mode (for scripting)
nvtop.exe -s

# Custom refresh rate (in 0.1s, so 10 = 1 second)
nvtop.exe -d 10
```

### Interactive Keys

- `q` - Quit
- `F2` - Settings/Options
- `F12` - Save preferences
- Arrow keys - Navigate

## Troubleshooting

### "MSYS2 not found"
- Install from https://www.msys2.org/
- Must be at `C:\msys64`

### "nvml.dll not found" or "No GPUs detected"
- Install CUDA Toolkit: https://developer.nvidia.com/cuda-downloads
- Or copy `nvml.dll` from `C:\Program Files\NVIDIA Corporation\NVSMI\`
- Verify GPU works: run `nvidia-smi` in command prompt

### "CMake not found"
```bash
# In MSYS2 UCRT64:
pacman -S mingw-w64-ucrt-x86_64-cmake
```

### Build errors
```batch
# Clean and retry
rmdir /s /q build-windows
build-windows-native.bat
```

### Display issues
- Use **Windows Terminal** (recommended)
- Set terminal to UTF-8: `chcp 65001`
- Avoid old cmd.exe

### Permission errors
- Run as Administrator for full process information

## What's Supported

✅ **Working**:
- NVIDIA GPU monitoring (with CUDA Toolkit)
- GPU utilization, temperature, memory
- Process monitoring (basic)
- ncurses interface
- Command-line options

⚠️ **Limited**:
- Process details (less than Linux)
- Some Unicode characters

❌ **Not Yet Supported**:
- AMD GPUs (requires AMD SDK integration)
- Intel GPUs (requires Intel APIs)

## File Structure After Build

```
nvtop/
├── build-windows/           # Build directory
│   └── src/
│       └── nvtop.exe       # Main executable
├── build-windows-native.bat # Quick build script
├── build-windows-native.ps1 # PowerShell build script
├── make-distribution.bat    # Create distributable
└── WINDOWS_NATIVE_BUILD.md  # Detailed documentation
```

## System Requirements

- **OS**: Windows 10 (1809+) or Windows 11
- **GPU**: NVIDIA GPU (for monitoring)
- **RAM**: 50 MB
- **Disk**: 100 MB for build, 10 MB for executable
- **Dependencies**: MSYS2 (for building), CUDA Toolkit (for running with NVIDIA)

## Distribution

To share with others:

1. Run: `make-distribution.bat`
2. Share the `nvtop-windows-x64` folder
3. Recipients need:
   - Windows 10/11
   - NVIDIA drivers
   - CUDA Toolkit (or just nvml.dll)

## Performance

- **CPU Usage**: <1% when idle
- **Memory**: ~10-20 MB
- **Refresh Rate**: Configurable (default 1s)
- **Startup Time**: <1 second

## Comparison: Native vs WSL2

| Feature           | Native Windows | WSL2      |
| ----------------- | -------------- | --------- |
| Setup             | Medium         | Easy      |
| NVIDIA Support    | ✅ Full         | ✅ Full    |
| AMD/Intel Support | ❌              | ⚠️ Limited |
| Performance       | ✅ Best         | Good      |
| Process Info      | ⚠️ Basic        | ✅ Full    |
| File Size         | Small          | N/A       |

**Recommendation**: 
- Use **Native** if you only need NVIDIA monitoring
- Use **WSL2** if you need AMD/Intel or better process info

## Documentation

- **Quick Start**: This file (README_WINDOWS.md)
- **Detailed Build Guide**: WINDOWS_NATIVE_BUILD.md
- **WSL2 Alternative**: WINDOWS_BUILD.md
- **Main README**: README.markdown

## Known Issues

1. **Terminal Compatibility**: Best in Windows Terminal
2. **Admin Rights**: May be needed for full process info
3. **AMD/Intel**: Not supported yet (contributions welcome!)
4. **Unicode**: Some characters may not display in older terminals

## Getting Help

1. Read `WINDOWS_NATIVE_BUILD.md` for detailed instructions
2. Check troubleshooting section above
3. Verify prerequisites are installed
4. Try clean rebuild

## Contributing

Want to help improve Windows support?

- Add AMD GPU support (ADL SDK)
- Add Intel GPU support
- Improve process monitoring
- Enhance terminal compatibility
- Test on different Windows versions

## License

GPLv3 - Same as main NVTOP project

## Credits

- Original NVTOP: Maxime Schmitt
- Windows Port: Community contribution
- Based on NVTOP: https://github.com/Syllo/nvtop

---

**Ready to build?** Just run: `build-windows-native.bat` 🚀
