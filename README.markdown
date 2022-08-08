NVTOP
=====

What is NVTOP?
--------------

Nvtop stands for Neat Videocard TOP, a (h)top like task monitor for AMD and NVIDIA GPUs. It can
handle multiple GPUs and print information about them in a htop familiar way.

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
  - [NVIDIA](#nvidia)
- [Build](#build)
- [Distribution Specific Installation Process](#distribution-specific-installation-process)
  - [Ubuntu / Debian](#ubuntu--debian)
    - [Ubuntu disco (19.04) / Debian buster (stable)](#ubuntu-disco-1904--debian-buster-stable)
  - [Fedora / RedHat / CentOS](#fedora--redhat--centos)
  - [OpenSUSE](#opensuse)
  - [Arch Linux](#arch-linux)
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

NVTOP supports AMD GPUs using the amdgpu driver through the exposed DRM and sysfs interfaces.

Support for recent GPUs are regularly mainlined into the linux kernel, so please
use a recent-enough kernel for your GPU.

### NVIDIA

The *NVML library* does not support some of the queries for GPUs coming before the
Kepler microarchitecture. Anything starting at GeForce 600, GeForce 800M and
successor should work fine. For more information about supported GPUs please
take a look at the [NVML documentation](http://docs.nvidia.com/deploy/nvml-api/nvml-api-reference.html#nvml-api-reference).

Build
-----

Several libraries are required in order for NVTOP to display GPU information:

* The *ncurses* library driving the user interface.
  * This makes the screen look beautiful.
* For NVIDIA: the *NVIDIA Management Library* (*NVML*) which comes with the GPU driver.
  * This queries the GPU for information.
* For AMD: the libdrm library used to query AMD GPUs through the kernel driver.

## Distribution Specific Installation Process

### Ubuntu / Debian

#### Ubuntu impish (21.10) / Debian buster (stable) and more recent

- ```bash
  sudo apt install nvtop
  ```

#### Older

- AMD Dependecy
  ```bash
  sudo apt install libdrm-dev
  ```

- NVIDIA Depenency
  - NVIDIA drivers (see [Ubuntu Wiki](https://help.ubuntu.com/community/BinaryDriverHowto/Nvidia) or [Ubuntu PPA](https://launchpad.net/~graphics-drivers/+archive/ubuntu/ppa) or [Debian Wiki](https://wiki.debian.org/NvidiaGraphicsDrivers#NVIDIA_Proprietary_Driver))

- NVTOP Dependencies
 - CMake, ncurses and git
  ```bash
  sudo apt install cmake libncurses5-dev libncursesw5-dev git
  ```

- NVTOP
  - Follow the [NVTOP Build](#nvtop-build)


### Fedora / RedHat / CentOS

- AMD Dependecy
  ```bash
  sudo dnf install libdrm-devel
  ```

- NVIDIA Depenency
  - NVIDIA drivers, **CUDA required for nvml libraries** (see [RPM Fusion](https://rpmfusion.org/Howto/NVIDIA))

- NVTOP Dependencies
 - CMake, ncurses, c++ and git
  ```bash
  sudo dnf install cmake ncurses-devel git gcc-c++
  ```

- NVTOP
  - Follow the [NVTOP Build](#nvtop-build)

### OpenSUSE

- AMD Dependecy
  ```bash
  sudo zypper install libdrm-devel
  ```

- NVIDIA Depenency
  - NVIDIA drivers (see [SUSE Support Database](https://en.opensuse.org/SDB:NVIDIA_drivers))

- NVTOP Dependencies
  - CMake, ncurses and git
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

### Docker

- NVIDIA drivers (same as above)

- [nvidia-docker](https://github.com/NVIDIA/nvidia-docker) (See  [Container Toolkit Installation Guide](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/install-guide.html#docker))

- ```bash
  git clone https://github.com/Syllo/nvtop.git && cd nvtop
  sudo docker build --tag nvtop .
  sudo docker run -it --rm --runtime=nvidia --gpus=all --pid=host nvtop
  ```

## NVTOP Build

```bash
git clone https://github.com/Syllo/nvtop.git
mkdir -p nvtop/build && cd nvtop/build
cmake .. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON
make

# Install globally on the system
sudo make install

# Alternatively, install without privileges at a location of your choosing
# make DESTDIR="/your/install/path" install
```

If you use **conda** as environment manager and encounter an error while building nvtop, try `conda deactivate` before invoking `cmake`.

The build system supports multiple build type (e.g. -DCMAKE_BUILD_TYPE=RelWithDebInfo):

* Release: Binary without debug information
* RelWithDebInfo: Binary with debug information
* Debug: Compile with warning flags and address/undefined sanitizers enabled (for development purposes)

Troubleshoot
------------

- The plot looks bad:
  - Verify that you installed the wide character version of the NCurses library (libncurses**w**5-dev for Debian / Ubuntu), clean the build directory
    and restart the build process.
- **Putty**: Tell putty not to lie about its capabilities (`$TERM`) by setting the field ``Terminal-type string`` to ``putty`` in the menu
  ``Connection > Data > Terminal Details``.

License
-------

Nvtop is licensed under the GPLV3 license or any later version.
You will find a copy of the license inside the COPYING file of the repository or
at the gnu website <[www.gnu.org/licenses/](http://www.gnu.org/licenses/)>.
