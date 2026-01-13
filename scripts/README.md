# Build and Utility Scripts

This directory contains various scripts for building, testing, and running nvtop on different platforms.

## Build Scripts

### Windows Native Build
- **`build-windows-native.ps1`** - PowerShell script for native Windows build with MinGW
- **`build-windows-native.bat`** - Batch wrapper for Windows native build
- **`check-prerequisites.bat`** - Check if all build prerequisites are installed

### WSL2 Build
- **`build-wsl2.ps1`** - PowerShell script to build using WSL2
- **`build-wsl2.bat`** - Batch wrapper for WSL2 build

### MSYS2 Build
- **`build-msys2.sh`** - Shell script for building in MSYS2 environment
- **`install-deps-msys2.sh`** - Install build dependencies in MSYS2

## Distribution Scripts
- **`make-distribution.bat`** - Create a standalone distribution package
- **`make-standalone.ps1`** - Create standalone executable with all dependencies

## Runtime Scripts
- **`run-nvtop.ps1`** - PowerShell script to run nvtop
- **`run-nvtop.bat`** - Batch script to run nvtop
- **`nvtop.bat`** - Simple launcher for nvtop

## Configuration
- **`PowerShell-Profile-Addition.ps1`** - Optional PowerShell profile additions for development

## Usage

### Quick Build (Windows)
```powershell
.\build-windows-native.ps1 -All
```

### Run nvtop
```powershell
.\run-nvtop.ps1
```

### Create Distribution
```powershell
.\make-standalone.ps1
```
