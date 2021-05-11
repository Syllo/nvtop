NVTOP
=====

What is NVTOP?
--------------

Nvtop stands for NVidia TOP, a (h)top like task monitor for NVIDIA GPUs. It can
handle multiple GPUs and print information about them in a htop familiar way.

Because a picture is worth a thousand words:

![NVTOP interface](/screenshot/NVTOP_ex1.png)

Table of Contents
-----------------

1. [NVTOP Options and Interactive Commands](#nvtop-options-and-interactive-commands)
1. [GPU Support](#gpu-support)
1. [Build](#build)
   1. [Ubuntu / Debian](#ubuntu--debian)
   1. [Fedora / RedHat / CentOS](#fedora--redhat--centos)
   1. [OpenSUSE](#opensuse)
   1. [Arch Linux](#arch-linux)
   1. [Gentoo](#gentoo)
   1. [NVTOP Build](#nvtop-build)
1. [Troubleshoot](#troubleshoot)
1. [License](#license)

NVTOP Options and Interactive Commands
--------------------------------------

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

The *NVML library* does not support some of the queries for GPUs coming before the
Kepler microarchitecture. Anything starting at GeForce 600, GeForce 800M and
successor should work fine. For more information about supported GPUs please
take a look at the [NVML documentation](http://docs.nvidia.com/deploy/nvml-api/nvml-api-reference.html#nvml-api-reference).

Build
-----

Two libraries are required:

* The *NVIDIA Management Library* (*NVML*) which comes with the GPU driver.
  * This queries the GPU for information.
* The *ncurses* library driving the user interface.
  * This makes the screen look beautiful.

## Distribution Specific Installation Process

### Ubuntu / Debian

#### Ubuntu disco (19.04) / Debian buster (stable)

- ```bash
  sudo apt install nvtop
  ```

#### Older

- NVIDIA drivers (see [Ubuntu Wiki](https://help.ubuntu.com/community/BinaryDriverHowto/Nvidia) or [Ubuntu PPA](https://launchpad.net/~graphics-drivers/+archive/ubuntu/ppa) or [Debian Wiki](https://wiki.debian.org/NvidiaGraphicsDrivers#NVIDIA_Proprietary_Driver))
- CMake, ncurses and git
  ```bash
  sudo apt install cmake libncurses5-dev libncursesw5-dev git
  ```
- NVTOP
  - Follow the [NVTOP Build](#nvtop-build)

### Fedora / RedHat / CentOS

- NVIDIA drivers, **CUDA required for nvml libraries** (see [RPM Fusion](https://rpmfusion.org/Howto/NVIDIA))
- CMake, ncurses and git
  ```bash
  sudo dnf install cmake ncurses-devel git
  ```
- NVTOP
  - Follow the [NVTOP Build](#nvtop-build)

### OpenSUSE

- NVIDIA drivers (see [SUSE Support Database](https://en.opensuse.org/SDB:NVIDIA_drivers))
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

## NVTOP Build

```bash
git clone https://github.com/Syllo/nvtop.git
mkdir -p nvtop/build && cd nvtop/build
cmake ..
make
make install # You may need sufficient permission for that (root)
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
