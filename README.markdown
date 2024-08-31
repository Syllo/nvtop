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

Because a picture is worth a thousand words:

![NVTOP interface](/screenshot/NVTOP_ex1.png)

Table of Contents
-----------------

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
  - [Ascend](#ascend) (only tested on 910B)
  - [VideoCore](#videocore)
- [Build](#build)
- [Distribution Specific Installation Process](#distribution-specific-installation-process)
  - [Ubuntu / Debian](#ubuntu--debian)
    - [Ubuntu Impish (21.10) / Debian buster (stable) and more recent (stable)](#ubuntu-impish-2110-debian-buster-stable-and-more-recent)
  - [Fedora / Red Hat / CentOS](#fedora--red-hat--centos)
  - [OpenSUSE](#opensuse)
  - [Arch Linux](#arch-linux)
  - [AppImage](#appimage)
  - [Snap](#snap)
  - [Conda-forge](#conda-forge)
  - [Docker](#docker)
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

**INTEL SUPPORT STATUS**
- Intel is working on exposing more hardware information through an `HWMON`
interface. The patches are still a work in progress: [see patch
series](https://patchwork.freedesktop.org/series/104278/).
- The fdinfo interface does not expose the memory allocated by the process. The
field in the process list is therefore empty.

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

On non-raspberry pi os, you need to use the `linux-rpi` kernel, ensure the presence of the `/dev/vcio` device, and have access permissions to the `/sys/kernel/debug` directory.

Build
-----

Several libraries are required in order for NVTOP to display GPU info:

* The *ncurses* library driving the user interface.
  * This makes the screen look beautiful.
* For NVIDIA: the *NVIDIA Management Library* (*NVML*) which comes with the GPU driver.
  * This queries the GPU for info.
* For AMD: the libdrm library used to query AMD GPUs through the kernel driver.

## Distribution Specific Installation Process

### Ubuntu / Debian

If your distribution provides the snap utility, follow the [snap installation process](#snap) to obtain an up-to-date version of `nvtop`.

A standalone application is available as [AppImage](#appimage).

#### Ubuntu Impish (21.10), Debian buster (stable) and more recent

- ```bash
  sudo apt install nvtop
  ```

#### Ubuntu PPA

A [PPA supporting Ubuntu 20.04, 22.04 and newer](https://launchpad.net/~flexiondotorg/+archive/ubuntu/nvtop) is provided by
[Martin Wimpress](https://github.com/flexiondotorg) that offers an up-to-date
version of `nvtop`, enabled for NVIDIA, AMD and Intel.

```bash
sudo add-apt-repository ppa:flexiondotorg/nvtop
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
  sudo layman -a guru && sudo emerge -av nvtop
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

## NVTOP Build

```bash
git clone https://github.com/Syllo/nvtop.git
mkdir -p nvtop/build && cd nvtop/build
cmake .. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON
make

# Install globally on the system
sudo make install

# Alternatively, install without privileges at a location of your choosing
# make DESTDIR="/your/install/path" install
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
