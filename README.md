NVTOP
=====

What is NVTOP?
--------------

NVTOP stands for Neat Videocard TOP, a (h)top like task monitor for GPUs and
accelerators. It can handle multiple GPUs and print information about them in a
htop-familiar way.

Currently supported vendors are AMD (Linux amdgpu driver), Apple (limited M1 &
M2 support), Huawei (Ascend), Intel (Linux i915/Xe drivers), NVIDIA (Linux
proprietary divers), Qualcomm Adreno (Linux MSM driver), Broadcom VideoCore (Linux v3d driver).

On Windows, native support is available for NVIDIA, AMD, and Intel GPUs.

Because a picture is worth a thousand words:

![NVTOP interface](/screenshot/NVTOP_ex1.png)

Quick Start
-----------

### Installation

**Windows** (Recommended - via WinGet):
```powershell
winget install Nervosys.Nvtop
```

**Linux** (Ubuntu/Debian):
```bash
sudo apt install nvtop
```

**Linux** (Fedora):
```bash
sudo dnf install nvtop
```

**Linux** (Arch):
```bash
sudo pacman -S nvtop
```

**Other platforms**: See [Distribution Specific Installation](#distribution-specific-installation-process) below.

### Usage

Simply run:
```bash
nvtop
```

**Interactive commands** (while running):
- `F2` - Setup/configuration menu
- `F12` - Save current settings
- `q` or `Esc` - Quit
- Arrow keys - Navigate and select GPUs
- `+/-` - Adjust refresh rate

**Command-line options**:
```bash
nvtop --help           # Show all options
nvtop --color          # Enable color support
nvtop --snapshot       # JSON output for scripting
nvtop --version        # Show version
```

**Windows-specific**:
```powershell
nvtop.exe --snapshot | ConvertFrom-Json  # PowerShell JSON parsing
```

Table of Contents
-----------------

- [NVTOP](#nvtop)
  - [What is NVTOP?](#what-is-nvtop)
  - [Quick Start](#quick-start)
    - [Installation](#installation)
    - [Usage](#usage)
  - [Table of Contents](#table-of-contents)
  - [NVTOP Options and Interactive Commands](#nvtop-options-and-interactive-commands)
    - [Interactive Setup Window](#interactive-setup-window)
    - [Saving Preferences](#saving-preferences)
    - [NVTOP Manual and Command line Options](#nvtop-manual-and-command-line-options)
  - [GPU Support](#gpu-support)
    - [AMD](#amd)
    - [Intel](#intel)
    - [NVIDIA](#nvidia)
    - [Adreno](#adreno)
    - [Apple](#apple)
    - [Ascend](#ascend)
    - [VideoCore](#videocore)
    - [Rockchip](#rockchip)
    - [MetaX](#metax)
  - [Build](#build)
  - [Distribution Specific Installation Process](#distribution-specific-installation-process)
    - [Ubuntu / Debian](#ubuntu--debian)
      - [Ubuntu Focal (20.04), Debian buster (stable) and more recent](#ubuntu-focal-2004-debian-buster-stable-and-more-recent)
      - [Ubuntu PPA](#ubuntu-ppa)
      - [Older](#older)
    - [Fedora / Red Hat / CentOS](#fedora--red-hat--centos)
      - [Fedora 36 and newer](#fedora-36-and-newer)
      - [CentOS Stream, Rocky Linux, AlmaLinux](#centos-stream-rocky-linux-almalinux)
    - [OpenSUSE](#opensuse)
    - [Arch Linux](#arch-linux)
    - [AppImage](#appimage)
- [Go to the download location \*\* The path may differ on your system \*\*](#go-to-the-download-location--the-path-may-differ-on-your-system-)
- [Make the AppImage executable](#make-the-appimage-executable)
- [Enjoy nvtop](#enjoy-nvtop)
    - [Conda-forge](#conda-forge)
      - [conda / mamba / miniforge](#conda--mamba--miniforge)
      - [pixi](#pixi)
    - [Docker](#docker)
      - [Manual Installation](#manual-installation)
      - [Alternative: WSL2 Installation](#alternative-wsl2-installation)
      - [Windows Features](#windows-features)
      - [Windows-Specific Notes](#windows-specific-notes)
  - [NVTOP Build](#nvtop-build)
  - [Troubleshoot](#troubleshoot)
  - [License](#license)

NVTOP Options and Interactive Commands
--------------------------------------
### Interactive Setup Window

NVTOP has a builtin setup utility that provides a way to specialize the interface to your needs.
Simply press ``F2`` and select the options that are the best for you.

![NVTOP Setup Window](/screenshot/Nvtop-config.png)

### Saving Preferences

You can save the preferences set in the setup window by pressing ``F12``.
The preferences will be loaded the next time you run ``nvtop``.

### NVTOP Manual and Command line Options

NVTOP comes with a manpage!
```bash
man nvtop
```
For quick command line arguments help
```bash
nvtop -h
nvtop --help
```

GPU Support
-----------

### AMD

NVTOP supports AMD GPUs using the `amdgpu` driver through the exposed DRM and
sysfs interface.

AMD introduced the fdinfo interface in kernel 5.14 ([browse kernel
source](https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/tree/drivers/gpu/drm/amd/amdgpu/amdgpu_fdinfo.c?h=linux-5.14.y)).
Hence, you will need a kernel with a version greater or equal to 5.14 to see the
processes using AMD GPUs.

Support for recent GPUs are regularly mainlined into the linux kernel, so please
use a recent-enough kernel for your GPU.

### Intel

NVTOP supports Intel GPUs using the `i915` or `xe` linux driver.

Intel introduced the fdinfo interface in kernel 5.19 ([browse kernel
source](https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/tree/drivers/gpu/drm/i915/i915_drm_client.c?h=linux-5.19.y)).
Hence, you will need a kernel with a version greater or equal to 5.19 to see the
processes using Intel GPUs.

Intel requires CAP_PERFMON or CAP_SYS_ADMIN capabilities to access the total memory usage,
you can run `sudo setcap cap_perfmon=ep nvtop` to grant the necessary permissions or run nvtop as root.

### NVIDIA

The *NVML library* does not support some of the queries for GPUs coming before the
Kepler microarchitecture. Anything starting at GeForce 600, GeForce 800M and
successor should work fine. For more information about supported GPUs please
take a look at the [NVML documentation](http://docs.nvidia.com/deploy/nvml-api/nvml-api-reference.html#nvml-api-reference).

### Adreno

NVTOP supports Adreno GPUs using the `msm` linux driver.

msm introduced the fdinfo interface in kernel 6.0 ([browse kernel
source](https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/tree/drivers/gpu/drm/msm/msm_drv.c?h=linux-6.0.y)).
Hence, you will need a kernel with a version greater or equal to 6.0 to see the
processes using Adreno GPUs.

### Apple

NVTOP includes some initial support for Apple using Metal. This is only supported when building for Apple, and when building for Apple only this vendor is supported.

**APPLE SUPPORT STATUS**
- Apple support is still being worked on. Some bugs and limitations may apply.

### Ascend

NVTOP supports Ascend (testing on Altas 800 (910B)) by DCMI API (version 6.0.0).

Currently, the DCMI only supports limited APIs, missing PCIe generation, tx/rx throughput info, max power draw etc.

### VideoCore

NVTOP supports VideoCore (testing on raspberrypi 4B).

Supports GPU frequency, temperature, utilization, per-process utilization, GPU memory usage, and H264 decoding utilization.

On non-raspberry pi os, you need to use the `linux-rpi 6.12.y` kernel and above, and ensure the presence of the `/dev/vcio` device.

### Rockchip

NVTOP supports Rockchip (testing on orangepi 5 plus).

Supports NPU frequency, temperature, utilization.

### MetaX

NVTOP supports MetaX (testing on MXC500) by MXSML LIBRARY.

For more information about GPUs please take a look at the [METAX documentation](https://developer.metax-tech.com/doc/index)

Build
-----

Several libraries are required in order for NVTOP to display GPU info:

* The *ncurses* library driving the user interface.
  * This makes the screen look beautiful.
* For NVIDIA: the *NVIDIA Management Library* (*NVML*) which comes with the GPU driver.
  * This queries the GPU for info.
* For AMD: the libdrm library used to query AMD GPUs through the kernel driver.
* For METAX: the *MetaX System Management Library* (*MXSML*) which comes with the GPU driver.
  * This queries the GPU for info.

## Distribution Specific Installation Process

### Ubuntu / Debian

If your distribution provides the snap utility, follow the [snap installation process](#snap) to obtain an up-to-date version of `nvtop`.

A standalone application is available as [AppImage](#appimage).

#### Ubuntu Focal (20.04), Debian buster (stable) and more recent

```bash
sudo apt install nvtop
```

#### Ubuntu PPA

A [PPA supporting Ubuntu 20.04 and newer](https://launchpad.net/~quentiumyt/+archive/ubuntu/nvtop) is provided by
[Quentin Lienhardt](https://github.com/QuentiumYT) that offers an up-to-date version of `nvtop`, enabled for NVIDIA, AMD and Intel.

```bash
sudo add-apt-repository ppa:quentiumyt/nvtop
sudo apt install nvtop
```

#### Older

- AMD and Intel Dependencies
  ```bash
  sudo apt install libdrm-dev libsystemd-dev
  # Ubuntu 18.04
  sudo apt install libudev-dev
  ```

- NVIDIA Depenency
  - NVIDIA drivers (see [Ubuntu Wiki](https://help.ubuntu.com/community/BinaryDriverHowto/Nvidia) or [Ubuntu PPA](https://launchpad.net/~graphics-drivers/+archive/ubuntu/ppa) or [Debian Wiki](https://wiki.debian.org/NvidiaGraphicsDrivers#NVIDIA_Proprietary_Driver))

- NVTOP Dependencies
 - CMake, ncurses and Git
  ```bash
  sudo apt install cmake libncurses5-dev libncursesw5-dev git
  ```

- NVTOP
  - Follow the [NVTOP Build](#nvtop-build)


### Fedora / Red Hat / CentOS

A standalone application is available as [AppImage](#appimage).

#### Fedora 36 and newer

- ```bash
  sudo dnf install nvtop
  ```

#### Red Hat Enterprise Linux 8 and 9

- ```bash
  sudo dnf install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-$(rpm -E %{rhel}).noarch.rpm
  sudo dnf install nvtop
  ```

#### CentOS Stream, Rocky Linux, AlmaLinux

- ```bash
  sudo dnf install -y epel-release
  sudo dnf install nvtop
  ```

#### Build process for Fedora / Red Hat / CentOS:

- AMD and Intel Dependencies
  ```bash
  sudo dnf install libdrm-devel systemd-devel
  ```

- NVIDIA Depenency
  - NVIDIA drivers, **CUDA required for nvml libraries** (see [RPM Fusion](https://rpmfusion.org/Howto/NVIDIA))

- NVTOP Dependencies
 - CMake, ncurses, C++ and Git
  ```bash
  sudo dnf install cmake ncurses-devel git gcc-c++
  ```

- NVTOP
  - Follow the [NVTOP Build](#nvtop-build)

### OpenSUSE

A standalone application is available as an [AppImage](#appimage).

Build process for OpenSUSE:

- AMD Dependecy
  ```bash
  sudo zypper install libdrm-devel
  ```

- NVIDIA Depenency
  - NVIDIA drivers (see [SUSE Support Database](https://en.opensuse.org/SDB:NVIDIA_drivers))

- NVTOP Dependencies
  - CMake, ncurses and Git
    ```bash
    sudo zypper install cmake ncurses-devel git
    ```

- NVTOP
  - Follow the [NVTOP Build](#nvtop-build)

### Arch Linux

- ```bash
  sudo pacman -S nvtop
  ```

### Gentoo

- ```bash
  sudo emerge -av nvtop
  ```

### AppImage

An AppImage is a standalone application. Just download the AppImage, make it executable and run it!

- Go to the [release page](https://github.com/Syllo/nvtop/releases/latest) and download `nvtop-x86_64.AppImage`

- ```bash
  # Go to the download location ** The path may differ on your system **
  cd $HOME/Downloads
  # Make the AppImage executable
  chmod u+x nvtop-x86_64.AppImage
  # Enjoy nvtop
  ./nvtop-x86_64.AppImage
  ```

If you are curious how that works, please visit the [AppImage website](https://appimage.org/).

### Snap

- ```bash
  snap install nvtop
  # Add the capability to kill processes inside nvtop
  snap connect nvtop:process-control
  # Add the capability to inspect GPU information (fan, PCIe, power, etc)
  snap connect nvtop:hardware-observe
  # AMDGPU process list support (read /proc/<pid>)
  snap connect nvtop:system-observe
  # Temporary workaround to get per-process GPU usage (read /proc/<pid>/fdinfo)
  snap connect nvtop:kubernetes-support
  ```

Notice: The connect commands allow

### Conda-forge

A [conda-forge feedstock for `nvtop`](https://github.com/conda-forge/nvtop-feedstock) is available.

#### conda / mamba / miniforge

```bash
conda install --channel conda-forge nvtop
```

#### pixi

```bash
pixi global install nvtop
```

### Docker

- NVIDIA drivers (same as above)

- [nvidia-docker](https://github.com/NVIDIA/nvidia-docker) (See [Container Toolkit Installation Guide](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/install-guide.html#docker))

- ```bash
  git clone https://github.com/Syllo/nvtop.git && cd nvtop
  sudo docker build --tag nvtop .
  sudo docker run -it --rm --runtime=nvidia --gpus=all --pid=host nvtop
  ```

### Windows

NVTOP now supports native Windows 10/11 with multi-vendor GPU support (NVIDIA, AMD, Intel).

#### Quick Start (Automated Build)

```powershell
# Clone the repository
git clone https://github.com/nervosys/nvtop.git -b windows
cd nvtop

# Run automated build script (installs prerequisites and builds)
.\scripts\build-windows-native.ps1 -All

# Run nvtop
.\build-windows\src\nvtop.exe
```

#### Manual Installation

**Prerequisites:**
- **MSYS2** (UCRT64 environment): https://www.msys2.org/
- **GPU Drivers**: NVIDIA (470+), AMD (with DXGI 1.4+), or Intel (Gen 11+/Arc)

**Install Dependencies (in MSYS2 UCRT64 terminal):**
```bash
pacman -S --needed mingw-w64-ucrt-x86_64-gcc \
                   mingw-w64-ucrt-x86_64-cmake \
                   mingw-w64-ucrt-x86_64-ninja \
                   mingw-w64-ucrt-x86_64-ncurses \
                   git
```

**Build NVTOP:**
```bash
mkdir build-windows && cd build-windows
cmake .. -G Ninja -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON
ninja
```

**Run NVTOP:**
```bash
# From build directory
./src/nvtop.exe

# Or from PowerShell/CMD
.\build-windows\src\nvtop.exe
```

#### Alternative: WSL2 Installation

For a Linux-like experience on Windows, you can use WSL2:

```powershell
# Install WSL2 and Ubuntu
wsl --install

# Inside WSL2, follow the Ubuntu installation instructions above
```

#### Windows Features

- ✅ **Multi-vendor support**: NVIDIA (full), AMD (DXGI/PDH), Intel (DXGI/PDH)
- ✅ **Per-process GPU usage**: All vendors
- ✅ **Color-coded utilization**: Green/Yellow/Red bars based on load
- ✅ **Real-time monitoring**: GPU, memory, power, clocks, temperatures
- ✅ **Native performance**: No WSL required, runs directly on Windows

#### Windows-Specific Notes

- **NVIDIA**: Requires CUDA Toolkit or copy `nvml.dll` to nvtop directory
- **AMD**: Temperature and fan speed require ADL SDK (not included)
- **Intel**: Limited power/clock reporting depending on driver
- **Snapshot mode**: Use `nvtop.exe --snapshot` for JSON output (scripting)

For detailed Windows build instructions, troubleshooting, and features, see:
- [Quick Start Guide](docs/QUICKSTART_WINDOWS.md)
- [Windows Build Guide](docs/WINDOWS_NATIVE_BUILD.md)
- [AMD/Intel Testing Guide](docs/AMD_INTEL_TESTING_GUIDE.md)

## NVTOP Build

```bash
git clone https://github.com/Syllo/nvtop.git
mkdir -p nvtop/build && cd nvtop/build
cmake .. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON
make

# Install globally on the system
sudo make install

# Alternatively, install without privileges at a location of your choosing
# cmake .. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON -DCMAKE_INSTALL_PREFIX=/path/to/your/dir
# make
# make install
```

If you use **conda** as environment manager and encounter an error while building NVTOP, try `conda deactivate` before invoking `cmake`.

The build system supports multiple build types (e.g. -DCMAKE_BUILD_TYPE=RelWithDebInfo):

* Release: Binary without debug info
* RelWithDebInfo: Binary with debug info
* Debug: Compile with warning flags and address/undefined sanitizers enabled (for development purposes)

Troubleshoot
------------

- The plot looks bad:
  - Verify that you installed the wide character version of the ncurses library (libncurses**w**5-dev for Debian / Ubuntu), clean the build directory and restart the build process.
- **Putty**: Tell putty not to lie about its capabilities (`$TERM`) by setting the field ``Terminal-type string`` to ``putty`` in the menu
  ``Connection > Data > Terminal Details``.

License
-------

NVTOP is licensed under the GPLv3 license or any later version.
You will find a copy of the license inside the COPYING file of the repository or
at the GNU website <[www.gnu.org/licenses/](http://www.gnu.org/licenses/)>.
