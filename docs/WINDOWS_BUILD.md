# Building NVTOP on Windows 10/11

NVTOP is primarily designed for Linux/macOS systems and uses platform-specific APIs. This guide covers different approaches to build and run NVTOP on Windows.

## Option 1: WSL2 (Recommended)

Windows Subsystem for Linux 2 (WSL2) provides the best compatibility with NVTOP's Linux dependencies.

### Prerequisites

1. **Enable WSL2**:
   ```powershell
   wsl --install
   ```

2. **Install Ubuntu** (or your preferred distribution):
   ```powershell
   wsl --install -d Ubuntu-22.04
   ```

3. **Install NVIDIA WSL Drivers** (for NVIDIA GPU support):
   - Download from: https://developer.nvidia.com/cuda/wsl
   - Install the driver on Windows (not in WSL)

### Building in WSL2

1. **Launch WSL2**:
   ```powershell
   wsl
   ```

2. **Install build dependencies**:
   ```bash
   sudo apt update
   sudo apt install -y cmake libncurses5-dev libncursesw5-dev git gcc g++
   
   # For NVIDIA support
   sudo apt install -y nvidia-cuda-toolkit
   
   # For AMD support
   sudo apt install -y libdrm-dev libsystemd-dev
   
   # For Intel support
   sudo apt install -y libdrm-dev libsystemd-dev
   ```

3. **Navigate to nvtop directory**:
   ```bash
   cd /mnt/c/path/to/nvtop
   ```

4. **Build NVTOP**:
   ```bash
   mkdir -p build && cd build
   cmake .. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON
   make -j$(nproc)
   ```

5. **Install**:
   ```bash
   sudo make install
   ```

6. **Run NVTOP**:
   ```bash
   nvtop
   ```

### Notes for WSL2

- NVIDIA GPUs are accessible in WSL2 with proper drivers
- AMD and Intel GPU support may be limited in WSL2
- Performance monitoring works best for NVIDIA GPUs

---

## Option 2: MSYS2/MinGW (Native Windows Build)

This approach builds a native Windows executable using MSYS2, but has significant limitations.

### Limitations

⚠️ **Important**: This approach has severe limitations:
- No direct GPU hardware access on Windows
- Most features will not work (fdinfo, DRM, sysfs don't exist on Windows)
- Only works as a proof-of-concept for the UI layer
- NVIDIA support would require Windows-specific NVML implementation
- AMD/Intel support not available without Windows driver API integration

### Prerequisites

1. **Install MSYS2**:
   - Download from: https://www.msys2.org/
   - Install to default location (C:\msys64)

2. **Update MSYS2**:
   ```bash
   pacman -Syu
   ```

### Building with MSYS2

1. **Open MSYS2 UCRT64 terminal**

2. **Install dependencies**:
   ```bash
   pacman -S --needed base-devel mingw-w64-ucrt-x86_64-toolchain \
     mingw-w64-ucrt-x86_64-cmake \
     mingw-w64-ucrt-x86_64-ncurses \
     git
   ```

3. **Navigate to nvtop**:
   ```bash
   cd /c/path/to/nvtop
   ```

4. **Create Windows-specific build** (requires code modifications):
   ```bash
   mkdir -p build-windows && cd build-windows
   cmake .. -G "MSYS Makefiles" \
     -DNVIDIA_SUPPORT=OFF \
     -DAMDGPU_SUPPORT=OFF \
     -DINTEL_SUPPORT=OFF \
     -DAPPLE_SUPPORT=OFF
   make
   ```

⚠️ This will likely fail without significant source code modifications for Windows compatibility.

---

## Option 3: Docker Desktop with WSL2 Backend

Use Docker to run NVTOP in a containerized Linux environment.

### Prerequisites

1. **Install Docker Desktop for Windows**:
   - Download from: https://www.docker.com/products/docker-desktop/
   - Enable WSL2 backend during installation

2. **Install NVIDIA Container Toolkit** (for GPU support):
   - Requires WSL2 with NVIDIA drivers

### Building and Running

1. **Build Docker image**:
   ```powershell
   cd C:\path\to\nvtop
   docker build -t nvtop .
   ```

2. **Run NVTOP** (with GPU access):
   ```powershell
   docker run -it --rm --gpus=all --pid=host nvtop
   ```

---

## Option 4: Remote Development with SSH

Run NVTOP on a Linux server and connect via SSH or Remote Desktop.

### Using VS Code Remote SSH

1. **Install "Remote - SSH" extension in VS Code**

2. **Connect to Linux machine** where NVTOP is built

3. **Build and run** NVTOP on the remote machine

---

## Recommended Approach

**For Windows Users**: Use **WSL2** (Option 1)

### Advantages:
- ✅ Full Linux compatibility
- ✅ Native GPU driver support (especially NVIDIA)
- ✅ Easy to set up
- ✅ Integrated with Windows filesystem
- ✅ Can access via Windows Terminal

### Quick Start (WSL2):

```powershell
# Install WSL2 with Ubuntu
wsl --install -d Ubuntu-22.04

# Launch WSL2
wsl

# Install dependencies and build
sudo apt update
sudo apt install -y cmake libncurses5-dev libncursesw5-dev git gcc g++ libdrm-dev libsystemd-dev

cd /mnt/c/path/to/nvtop
mkdir -p build && cd build
cmake .. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON
make -j$(nproc)
sudo make install

# Run
nvtop
```

---

## Troubleshooting

### WSL2 NVIDIA GPU Not Detected

1. Ensure you installed the **WSL-specific NVIDIA driver** on Windows
2. Verify GPU is visible in WSL:
   ```bash
   nvidia-smi
   ```

### Permission Issues in WSL2

1. For Intel GPUs:
   ```bash
   sudo setcap cap_perfmon=ep /usr/local/bin/nvtop
   ```

### ncurses Display Issues

1. Set proper terminal:
   ```bash
   export TERM=xterm-256color
   ```

### Build Errors

1. Clean build directory:
   ```bash
   cd build
   rm -rf *
   cmake .. -DNVIDIA_SUPPORT=ON
   make
   ```

---

## Native Windows Support (Future)

To add native Windows support, the following would need to be implemented:

### Required Changes:

1. **Windows-specific process enumeration**:
   - Replace `/proc` filesystem access with Windows APIs
   - Use `CreateToolhelp32Snapshot`, `Process32First`, etc.

2. **GPU interface abstraction**:
   - NVIDIA: Use NVML on Windows (nvml.dll)
   - AMD: Use AMD Display Library (ADL SDK)
   - Intel: Use Intel GPU Performance APIs

3. **System information**:
   - Replace sysfs/fdinfo with Windows Registry and WMI
   - Use `SetupAPI` for device enumeration

4. **Terminal handling**:
   - PDCurses or Windows-specific ncurses builds
   - Handle Windows console API differences

5. **Build system**:
   - CMake configurations for MSVC
   - Handle Windows-specific libraries and paths

This would be a substantial development effort requiring Windows GPU driver expertise.

---

## Summary

| Method     | Difficulty | GPU Support                           | Recommendation          |
| ---------- | ---------- | ------------------------------------- | ----------------------- |
| WSL2       | Easy       | NVIDIA: Excellent, AMD/Intel: Limited | ⭐ **Recommended**       |
| MSYS2      | Hard       | None without major code changes       | Not recommended         |
| Docker     | Medium     | NVIDIA: Good                          | Good alternative        |
| Remote SSH | Easy       | Full (on remote)                      | Good for remote systems |

**Bottom Line**: Use WSL2 for the best experience on Windows 10/11.
