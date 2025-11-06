# Windows Build Files Summary

This directory contains files for building NVTOP natively on Windows 10/11.

## Quick Reference

### 🎯 Start Here
- **README_WINDOWS.md** - Quick start guide
- **build-windows-native.bat** - One-click build script

### 📚 Documentation
| File                      | Purpose                                         |
| ------------------------- | ----------------------------------------------- |
| `README_WINDOWS.md`       | Quick start and basic usage                     |
| `WINDOWS_NATIVE_BUILD.md` | Detailed build instructions and troubleshooting |
| `WINDOWS_BUILD.md`        | WSL2 build alternative                          |
| `QUICKSTART_WINDOWS.md`   | WSL2 quick start                                |

### 🔧 Build Scripts
| File                       | Type       | Description                  |
| -------------------------- | ---------- | ---------------------------- |
| `build-windows-native.bat` | Batch      | Simple automated build       |
| `build-windows-native.ps1` | PowerShell | Advanced build with options  |
| `make-distribution.bat`    | Batch      | Create distributable package |
| `build-wsl2.bat`           | Batch      | WSL2 build (alternative)     |
| `build-wsl2.ps1`           | PowerShell | WSL2 build with options      |
| `build-msys2.sh`           | Bash       | MSYS2 terminal build script  |

### 🛠️ Source Files (Windows-specific)
| File                                   | Purpose                         |
| -------------------------------------- | ------------------------------- |
| `src/get_process_info_windows.c`       | Windows process monitoring      |
| `src/info_messages_windows.c`          | Windows system information      |
| `src/extract_gpuinfo_nvidia_windows.c` | NVIDIA GPU monitoring (Windows) |
| `src/windows_compat.c`                 | POSIX compatibility layer       |
| `include/nvtop/windows_compat.h`       | Windows compatibility header    |

### 🔩 Build Configuration
| File                            | Purpose                      |
| ------------------------------- | ---------------------------- |
| `cmake/windows-toolchain.cmake` | CMake Windows toolchain      |
| `CMakeLists.txt`                | Updated with Windows support |
| `src/CMakeLists.txt`            | Updated with Windows sources |

### 📦 Additional Files
| File                              | Purpose                    |
| --------------------------------- | -------------------------- |
| `docker-compose.yml`              | Docker build configuration |
| `PowerShell-Profile-Addition.ps1` | PowerShell aliases         |
| `windows-terminal-settings.txt`   | Windows Terminal config    |

## Build Methods Comparison

| Method                            | Difficulty   | Time   | Best For               |
| --------------------------------- | ------------ | ------ | ---------------------- |
| **build-windows-native.bat**      | ⭐ Easy       | 5 min  | First-time users       |
| **build-windows-native.ps1 -All** | ⭐⭐ Medium    | 5 min  | PowerShell users       |
| **Manual CMake**                  | ⭐⭐⭐ Advanced | 10 min | Developers             |
| **WSL2**                          | ⭐ Easy       | 5 min  | Need AMD/Intel support |

## Prerequisites Checklist

### For Native Windows Build
- [ ] Windows 10 (1809+) or Windows 11
- [ ] MSYS2 installed at `C:\msys64`
- [ ] MSYS2 packages installed (toolchain, cmake, ninja, ncurses)
- [ ] NVIDIA drivers (for GPU monitoring)
- [ ] CUDA Toolkit (for nvml.dll)

### For WSL2 Build
- [ ] Windows 10 (2004+) or Windows 11
- [ ] WSL2 installed
- [ ] Ubuntu 22.04 (or similar distro)
- [ ] NVIDIA WSL drivers (for GPU monitoring)

## Quick Start Commands

### Native Build (Recommended)
```batch
# Install MSYS2 first from https://www.msys2.org/
# Then run:
build-windows-native.bat
```

### WSL2 Build (Alternative)
```powershell
build-wsl2.ps1 -All
```

### Manual Build
```powershell
$env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"
mkdir build-windows; cd build-windows
cmake .. -G "Ninja" -DNVIDIA_SUPPORT=ON
ninja
```

## Build Output

After successful build:
```
build-windows/
└── src/
    └── nvtop.exe     <- Your executable!
```

Run with:
```batch
build-windows\src\nvtop.exe
```

## Distribution Package

Create distributable:
```batch
make-distribution.bat
```

Output:
```
nvtop-windows-x64/
├── nvtop.exe
├── nvtop.bat
├── libncursesw6.dll
├── libgcc_s_seh-1.dll
├── libwinpthread-1.dll
├── nvml.dll (if available)
└── README.txt
```

## GPU Support Status

| GPU    | Status      | Requirements                    |
| ------ | ----------- | ------------------------------- |
| NVIDIA | ✅ Supported | CUDA Toolkit (nvml.dll)         |
| AMD    | ❌ Not yet   | Need AMD Display Library SDK    |
| Intel  | ❌ Not yet   | Need Intel GPU Performance APIs |

## Troubleshooting Quick Links

| **Problem**      | **Solution**                                  |
| ---------------- | --------------------------------------------- |
| MSYS2 not found  | Install from https://www.msys2.org/           |
| nvml.dll missing | Install CUDA Toolkit                          |
| Build fails      | See WINDOWS_NATIVE_BUILD.md § Troubleshooting |
| Display issues   | Use Windows Terminal                          |
| No GPU detected  | Check NVIDIA drivers, run nvidia-smi          |

## Next Steps After Build

1. **Test the build**:
   ```batch
   build-windows\src\nvtop.exe -h
   ```

2. **Run monitoring**:
   ```batch
   build-windows\src\nvtop.exe
   ```

3. **Create distribution** (optional):
   ```batch
   make-distribution.bat
   ```

4. **Read documentation**:
   - Basic usage: README_WINDOWS.md
   - Troubleshooting: WINDOWS_NATIVE_BUILD.md

## File Sizes

| Item              | Size        |
| ----------------- | ----------- |
| nvtop.exe         | ~500 KB     |
| Required DLLs     | ~2 MB total |
| Full distribution | ~3-5 MB     |
| Build directory   | ~50 MB      |

## Support Matrix

### Tested Configurations

✅ **Working**:
- Windows 10 21H2 + MSYS2 + NVIDIA GTX/RTX
- Windows 11 + MSYS2 + NVIDIA GTX/RTX
- Windows Terminal + PowerShell 7

⚠️ **Partial**:
- Windows 10 1809-2004 (older, may have issues)
- cmd.exe (display issues possible)
- PowerShell 5.1 (scripts work but less features)

❌ **Not Supported**:
- Windows 7/8/8.1 (too old)
- MinGW without MSYS2 (untested)
- Cygwin (not targeted)

## Contributing

See WINDOWS_NATIVE_BUILD.md for:
- How to add AMD support
- How to add Intel support
- How to improve process monitoring
- How to enhance Windows compatibility

## License

All files: GPLv3 (same as main NVTOP project)

---

**Questions?** Check README_WINDOWS.md or WINDOWS_NATIVE_BUILD.md

**Ready to build?** Run: `build-windows-native.bat`
