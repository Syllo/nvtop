# Quick Start Guide for Windows Users

This guide helps you build and run NVTOP on Windows 10/11.

## 🚀 Quickest Method: WSL2

### One-Command Setup

```powershell
# Run this in PowerShell (as Administrator)
wsl --install -d Ubuntu-22.04
```

Restart your computer, then:

```powershell
# Launch WSL2
wsl

# Install and build (run in WSL)
sudo apt update && \
sudo apt install -y cmake libncurses5-dev libncursesw5-dev git gcc g++ libdrm-dev libsystemd-dev && \
cd /mnt/c/path/to/nvtop && \
mkdir -p build && cd build && \
cmake .. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON && \
make -j$(nproc) && \
sudo make install

# Run NVTOP
nvtop
```

### Automated Build Script

Alternatively, use the provided PowerShell script:

```powershell
# From Windows PowerShell in the nvtop directory
.\build-wsl2.ps1 -All
```

Or the batch file:

```cmd
build-wsl2.bat
```

## 🎯 What Works

✅ **WSL2**: Full support for NVIDIA GPUs  
⚠️ **WSL2**: Limited support for AMD/Intel GPUs  
❌ **Native Windows**: Requires significant development work  

## 📊 GPU Support Matrix

| GPU Type | WSL2        | Native Windows    | Docker    |
| -------- | ----------- | ----------------- | --------- |
| NVIDIA   | ✅ Excellent | ❌ Not implemented | ✅ Good    |
| AMD      | ⚠️ Limited   | ❌ Not implemented | ⚠️ Limited |
| Intel    | ⚠️ Limited   | ❌ Not implemented | ⚠️ Limited |

## 🛠️ Prerequisites for WSL2

1. **Windows 10 version 2004+** or **Windows 11**
2. **Virtualization enabled** in BIOS
3. **WSL2 installed** (automatically done with `wsl --install`)
4. **NVIDIA GPU Drivers** (for NVIDIA GPUs only):
   - Download WSL-specific drivers from: https://developer.nvidia.com/cuda/wsl

## 🔧 Alternative: Docker

If you prefer Docker:

```powershell
# Build
docker build -t nvtop .

# Run with GPU access
docker run -it --rm --gpus=all --pid=host nvtop

# Or use docker-compose
docker-compose up nvtop
```

## ❓ Troubleshooting

### "WSL is not installed"
```powershell
wsl --install
# Restart your computer
```

### "GPU not detected in WSL2"
1. Install WSL2-specific NVIDIA drivers on Windows
2. Verify with: `wsl nvidia-smi`

### "Build errors"
```bash
# Clean and rebuild
cd /mnt/c/path/to/nvtop
rm -rf build
mkdir build && cd build
cmake .. -DNVIDIA_SUPPORT=ON
make
```

## 📖 More Information

See [WINDOWS_BUILD.md](WINDOWS_BUILD.md) for detailed instructions and all build options.

## 🎓 Tips

- **Run from Windows**: `wsl nvtop`
- **Add to PATH**: Create an alias in PowerShell profile
- **Windows Terminal**: Use for better display
- **Keyboard shortcut**: Press `F2` in nvtop for settings, `q` to quit

## 🆘 Need Help?

1. Check [WINDOWS_BUILD.md](WINDOWS_BUILD.md) for detailed troubleshooting
2. Verify WSL2 is working: `wsl --version`
3. Check GPU access: `wsl nvidia-smi` (for NVIDIA)
4. Review build logs for specific errors

---

**Recommended**: Use WSL2 for the best experience! 🌟
