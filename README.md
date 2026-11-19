NVTOP
=====

What is NVTOP?
--------------

NVTOP stands for Neat Videocard TOP, a (h)top like task monitor for GPUs and
accelerators. It can handle multiple GPUs and print information about them in a
htop-familiar way.

Currently supported vendors are AMD (Linux amdgpu driver), Apple (limited M1 &
M2 support), Huawei (Ascend), Intel (Linux i915/Xe GPU drivers and the NPU
through intel_vpu), NVIDIA (Linux proprietary drivers), Qualcomm Adreno (Linux
MSM driver), Broadcom VideoCore (Linux v3d driver), Rockchip, MetaX (MXSML
driver), Enflame (Linux EFML driver), Tenstorrent (Linux tt-kmd driver),
Iluvatar CoreX (ixML / libixml), T-Head PPU (HGML), Intel Gaudi and Goya (Linux
habanalabs driver).

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
  - [Intel NPU](#intel-npu)
  - [NVIDIA](#nvidia)
  - [Adreno](#adreno)
  - [Apple](#apple)
  - [Ascend](#ascend)
  - [Iluvatar CoreX](#iluvatar-corex)
  - [T-Head PPU](#t-head-ppu)
  - [VideoCore](#videocore)
  - [Rockchip](#rockchip)
  - [MetaX](#metax)
  - [Enflame](#enflame)
  - [Tenstorrent](#tenstorrent)
  - [Intel Gaudi](#intel-gaudi)
- [Build](#build)
- [Distribution Specific Installation Process](#distribution-specific-installation-process)
  - [Ubuntu / Debian](#ubuntu--debian)
    - [Ubuntu Jammy (22.04), Debian bullseye (11) and more recent](#ubuntu-jammy-2204-debian-bullseye-11-and-more-recent)
    - [Ubuntu PPA](#ubuntu-ppa)
    - [Build process for Ubuntu / Debian](#build-process-for-ubuntu--debian)
  - [Fedora / Red Hat / CentOS](#fedora--red-hat--centos)
    - [Fedora 43 and newer](#fedora-43-and-newer)
    - [Red Hat Enterprise Linux 8, 9 and 10](#red-hat-enterprise-linux-8-9-and-10)
    - [CentOS Stream, Rocky Linux, AlmaLinux](#centos-stream-rocky-linux-almalinux)
    - [Build process for Fedora / Red Hat / CentOS](#build-process-for-fedora--red-hat--centos)
  - [OpenSUSE](#opensuse)
    - [OpenSUSE 16.0 and newer](#opensuse-160-and-newer)
    - [Build process for OpenSUSE](#build-process-for-opensuse)
  - [Arch Linux](#arch-linux)
  - [Gentoo](#gentoo)
  - [AppImage](#appimage)
  - [Snap](#snap)
  - [Conda-forge](#conda-forge)
    - [conda / mamba / miniforge](#conda--mamba--miniforge)
  - [Pixi](#pixi)
  - [Docker](#docker)
  - [WSL2](#wsl2)
- [NVTOP Build](#nvtop-build)
- [Troubleshoot](#troubleshoot)
- [License](#license)

NVTOP Options and Interactive Commands
--------------------------------------
### Interactive Setup Window

NVTOP has a builtin setup utility that provides a way to specialize the interface to your needs.
Simply press ``F2`` and select the options that are the best for you.

In the ``Chart`` section you can choose which metrics are plotted, including GPU and memory
utilization, temperature, power, clocks, and the **PCIe RX / TX load** (the receive and transmit
throughput as a percentage of the maximum link bandwidth).

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

NVTOP supports AMD GPUs using the `amdgpu` driver and the legacy `radeon` driver (legacy GPUs, limited support) through the exposed DRM and
sysfs interface.

The radeon provides limited metrics compared to amdgpu.

AMD introduced the fdinfo interface in kernel 5.14 ([browse kernel
source](https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/tree/drivers/gpu/drm/amd/amdgpu/amdgpu_fdinfo.c?h=linux-5.14.y)).
Hence, you will need a kernel with a version greater or equal to 5.14 to see the
processes using AMD GPUs.

ROCm/HIP submit work to the AMD compute queues through KFD (`/dev/kfd`, the
Kernel Fusion Driver). These queues are not tracked by the amdgpu DRM context
manager, so their work is not counted by the DRM fdinfo engine counters. When
the kernel exposes per-process compute unit occupancy
(`/sys/class/kfd/kfd/proc/<pid>/stats_<gpu_id>/cu_occupancy`), NVTOP includes it
in the process GPU utilization.

Per-process CU occupancy support depends on the GPU family and kernel version:

* CDNA (GFX9): MI100 (gfx908) since Linux 5.11, MI200 (gfx90a) since 6.2, and
  MI300 (gfx942) since 6.6.
* RDNA 3 (gfx11) and RDNA 4 (gfx12/12.1) since Linux 7.3.
* RDNA 1/2 (gfx10/10.3) since Linux 7.4.
* GCN 1-4 (gfx6-8) is not supported.

The interface also requires kernel support for KFD (`CONFIG_HSA_AMD`). NVTOP
silently skips the KFD contribution when the file is unavailable.

Support for recent GPUs are regularly mainlined into the linux kernel, so please
use a recent-enough kernel for your GPU.

### Intel

NVTOP supports Intel GPUs using the `i915` or `xe` linux driver.

Intel introduced the fdinfo interface in kernel 5.19 ([browse kernel
source](https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/tree/drivers/gpu/drm/i915/i915_drm_client.c?h=linux-5.19.y)).
Hence, you will need a kernel with a version greater or equal to 5.19 to see the
processes using Intel GPUs.

Intel requires CAP_PERFMON or CAP_SYS_ADMIN capabilities to access the total memory usage, and an accurate GPU frequency.
you can run `sudo setcap cap_perfmon=ep $(which nvtop)` to grant the necessary permissions or run nvtop as root.

### Intel NPU

NVTOP supports the NPU driven by the in-kernel `intel_vpu` (ivpu) accel driver
on Meteor Lake, Arrow Lake, Lunar Lake, Panther Lake, Wildcat Lake and Nova
Lake. All metrics are read from sysfs, so no extra library is needed. Support
is enabled by default on Linux and can be disabled with
`-DINTEL_NPU_SUPPORT=OFF`.

It reports NPU utilization, the current and maximum frequency, and system
memory usage. Utilization is derived from the cumulative `npu_busy_time_us`
counter; the driver recommends sampling it at most once per second. The
interactive interface defaults to a one-second update interval, while a smaller
`-d` value (or snapshot mode's 100 ms default) samples it more often. The
memory row reports system-wide RAM usage, like the other NPU backends, rather
than the NPU's own allocations.

Utilization requires kernel 6.11 or newer and frequency information requires
6.15 or newer; older kernels simply show fewer fields. Temperature and
per-process usage are not exposed by the driver and are not reported.

### NVIDIA

The *NVML library* does not support some of the queries for GPUs coming before the
Kepler microarchitecture. Anything starting at GeForce 600, GeForce 800M and
successor should work fine. For more information about supported GPUs please
take a look at the [NVML documentation](http://docs.nvidia.com/deploy/nvml-api/nvml-api-reference.html#nvml-api-reference).

Unified-memory systems such as DGX Spark (GB10) and RTX Spark N1X are supported
through the same NVML backend. When NVML reports no dedicated framebuffer,
memory capacity comes from Linux `MemTotal`, available memory from
`MemAvailable`, and GPU usage from the visible GPU processes' allocations.
These are different counters: total minus GPU usage is not the remaining
allocatable system memory. Processes using both graphics and compute contexts
are counted once. In containers, make host GPU processes visible (for example
with `--pid=host`) to obtain complete per-process accounting.

PCIe link generation and width are left unavailable when NVML reports an active
NVLink-C2C attachment. Driver-unsupported sensors, including memory clocks, fan
speed and link throughput, remain `N/A` rather than being estimated.

The extra GPU info bar (`nvtop -i`) shows the dynamic parameters NVML does not
report, such as the XBAR, SYS and NVD clocks, and `nvtop -X` also shows the
static specs (shader cores, L2 cache, execution engines) and the secondary HUB,
HOST, DISP, MSD and UTILS clocks. The clock domains are read through the *NvAPI
library* (`libnvidia-api.so.1`) that recent drivers install alongside NVML;
without it the rest of the interface is unaffected and the bar takes no room on
a GPU that reports nothing.

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
- On unified-memory systems, the memory meter compares global Metal resource allocations with total physical shared
  memory. It does not represent dedicated VRAM or overall system memory pressure.
- On Apple Silicon, power draw, GPU clock, temperature and fan speed are read through the private IOReport and AppleSMC
  interfaces. These are undocumented and may change between macOS releases; when they are unavailable NVTOP reports the
  affected values as unavailable instead of failing. The fan reading is the chassis fan shared by the SoC, not a fan
  dedicated to the GPU.
- GPU clock speed maps IOReport performance-state residency onto the GPU DVFS table (`voltage-states9`) exposed by the
  `pmgr`/`pmgr-child` device-tree node. The table layout and units differ between chip generations, so this reading is
  best-effort and may be unavailable on models NVTOP has not been validated against.
- On Intel Macs with a discrete AMD GPU, temperature, power draw, core/memory clocks, fan speed and used VRAM come
  from the driver's `PerformanceStatistics` dictionary, and per-process GPU usage from the AMD user-client nodes in
  the I/O Registry. Both property sets are undocumented and vary between driver generations; values the driver does
  not publish are reported as N/A.
- The PCI bus/device address, the negotiated PCIe generation and width (`IOPCIExpressLinkStatus`) and the chassis slot
  label (`AAPL,slot-name`, e.g. `Slot-1` on a Mac Pro) are read from the GPU's IOPCIDevice. Cards that publish a GD
  device segment (such as the Radeon Pro W6800X Duo) report the same slot with a `/dieN` suffix. Apple Silicon
  accelerators are not PCI devices, so these fields stay N/A there.

### Ascend

NVTOP supports Ascend through the DCMI API (tested on 910B3 hardware). It
uses the logical-device API when available and falls back to the legacy
card/device API. Read-only telemetry is reported when supported by the
installed driver; unavailable values remain unset. Enable it with
`-DASCEND_SUPPORT=ON`, and set `-DASCEND_ROOT=/path/to/Ascend` when CANN is
installed outside the standard locations.

### Iluvatar CoreX

NVTOP supports Iluvatar CoreX GPUs through the ixML library.

The backend dynamically loads `libixml.so` from `/usr/local/corex/lib`,
`/usr/local/corex/lib64`, or the default dynamic loader search path. The ixML
runtime exposes an NVML-compatible API surface used by NVTOP to query device,
power, PCIe, clock, temperature, memory, and process information.

### T-Head PPU

NVTOP supports T-Head (平头哥) PPUs through the HGML runtime, tested on
PPU-ZW810E hardware. Enable with `-DPPU_SUPPORT=ON` (the Linux default).
No vendor headers are required to build. The backend loads `libhgml.so.1`
or `libhgml.so` through the dynamic loader, then checks the standard
`/usr/local/PPU_SDK` library directories. For a custom SDK installation,
add its library directory to `LD_LIBRARY_PATH`.

Supported telemetry includes utilization, memory, clocks, temperature, power,
PCIe link/throughput, compute process memory, and per-process PPU utilization.
Metrics unavailable in the installed driver remain unset. Process enumeration
uses the HGML v2 or v3 compute process API. Per-process utilization uses
`hgmlDeviceGetProcessUtilization` when available, selecting the newest SM
sample for each PID. Processes without a fresh sample are reported as idle
(`0%`) when per-process utilization is available; unsupported or failed
queries leave the value unset.

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

### Enflame

NVTOP supports Enflame GCUs (testing on Enflame S60, Enflame L300 and Enflame L600) by EFML LIBRARY

GCU, which refers to General Compute Unit, is a type of accelerator card that is used to perform general-purpose computing tasks just like GPGPU.

### Tenstorrent

NVTOP supports Tenstorrent AI accelerators (Blackhole, Wormhole, Grayskull) through the [tt-kmd](https://github.com/tenstorrent/tt-kmd) kernel driver.

Supports temperature, power draw, AI clock, fan RPM, PCIe link info, and process listing. No external libraries required -- all data is read from sysfs, hwmon, and procfs.

### Intel Gaudi

NVTOP supports Intel Gaudi HPUs (tested on Gaudi2 HL-225 with driver 1.24) through the *Habana Labs Management
Library* (*HLML*, `libhlml.so`), the library behind `hl-smi`. It is loaded at runtime, so building only needs the
`GAUDI_SUPPORT` option (enabled by default on Linux), not the Habana SDK headers.

Supports AIP utilization, memory usage, SoC clock, chip temperature, power draw and limit, PCIe link and
rx/tx throughput, and the processes using each device with their device memory and utilization.

* Some HLML queries go through the device firmware and take tens of milliseconds, so the device information is sampled
  by a background thread and lags behind by at most one refresh interval.
* Processes are found by scanning `/proc` for the `/dev/accel/accelN` device nodes, hence you need to run nvtop as root
  to see the processes of other users. Gaudi runs one compute context per device: the process memory is the value the
  framework integration (e.g. PyTorch or Paddle) publishes in `/dev/shm/mem_usage_accelN` (as `hl-smi` shows it), or
  N/A when it does not publish it. The process utilization is the device utilization when a single process can be
  identified as the compute context (one device holder, or the single process publishing the framework memory).
* Chip temperature is read from the hwmon channels that the driver's `/etc/sensors.d/hl_sensors.conf` labels
  `On Chip ...`, avoiding a memory leak in HLML's temperature call. Only that file is read; relabeling or ignoring the
  sensors in other `sensors.d` files is not followed. If the file is absent, yields no matching sensors, or selects a
  different number of AIP sensors than HLML's per-ASIC map, nvtop falls back to the HLML call.
* HLML also enumerates Goya devices; nvtop reports them as Goya with DDR4 memory. Goya has not been validated on
  hardware.
* The power limit is that of the main (54 V) power supply; `hl-smi` adds the auxiliary 12 V supply to its cap.

Build
-----

Several libraries are required in order for NVTOP to display GPU info:

* The *ncurses* library driving the user interface.
  * This makes the screen look beautiful.
* For NVIDIA: the *NVIDIA Management Library* (*NVML*) which comes with the GPU driver.
  * This queries the GPU for info.
  * The *NvAPI library* (`libnvidia-api.so.1`), also from the driver, is used
    when present for the clock domains NVML does not report. It is optional.
* For AMD: the libdrm library used to query AMD GPUs through the kernel driver.
* For METAX: the *MetaX System Management Library* (*MXSML*) which comes with the GPU driver.
  * This queries the GPU for info.
* For Enflame: the *Enflame Management Library* (*EFML*) which comes with the GCU driver.
* For Iluvatar CoreX: the *ixML* runtime library (`libixml.so`) which comes with the driver.
  * This backend loads the library dynamically at runtime.
* For T-Head PPU: the *HGML* runtime library (`libhgml.so`) which comes with the PPU SDK.
  * This backend loads the library dynamically at runtime.
* For Intel Gaudi: the *Habana Labs Management Library* (*HLML*) which comes with the habanalabs driver packages.
  * This backend loads the library dynamically at runtime.

## Distribution Specific Installation Process

### Ubuntu / Debian

If your distribution provides the snap utility, follow the [snap installation process](#snap) to obtain an up-to-date version of `nvtop`.

A standalone application is available as [AppImage](#appimage).

#### Ubuntu Jammy (22.04), Debian bullseye (11) and more recent

```bash
sudo apt install nvtop
```

#### Ubuntu PPA

A [PPA supporting Ubuntu 22.04 and newer](https://launchpad.net/~quentiumyt/+archive/ubuntu/nvtop) is provided by
[Quentin Lienhardt](https://github.com/QuentiumYT) that offers an up-to-date version of `nvtop`, enabled for NVIDIA, AMD and Intel.

```bash
sudo add-apt-repository ppa:quentiumyt/nvtop
sudo apt install nvtop
```

#### Build process for Ubuntu / Debian

- AMD and Intel Dependencies
  ```bash
  sudo apt install libdrm-dev libsystemd-dev
  ```

- NVIDIA Dependency
  - NVIDIA drivers (see [Ubuntu Wiki](https://help.ubuntu.com/community/BinaryDriverHowto/Nvidia) or [Ubuntu PPA](https://launchpad.net/~graphics-drivers/+archive/ubuntu/ppa) or [Debian Wiki](https://wiki.debian.org/NvidiaGraphicsDrivers#NVIDIA_Proprietary_Driver))

- NVTOP Dependencies
  - CMake, ncurses, GCC/G++ and Git
  ```bash
  sudo apt install cmake libncurses5-dev libncursesw5-dev gcc g++ git
  ```

- NVTOP
  - Follow the [NVTOP Build](#nvtop-build)


### Fedora / Red Hat / CentOS

A standalone application is available as [AppImage](#appimage).

#### Fedora 43 and newer

```bash
sudo dnf install nvtop
```

#### Red Hat Enterprise Linux 8, 9 and 10

```bash
sudo dnf install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-$(rpm -E %{rhel}).noarch.rpm
sudo dnf install nvtop
```

#### CentOS Stream, Rocky Linux, AlmaLinux

```bash
sudo dnf install -y epel-release
sudo dnf install nvtop
```

#### Build process for Fedora / Red Hat / CentOS

- AMD and Intel Dependencies
  ```bash
  sudo dnf install libdrm-devel systemd-devel
  ```

- NVIDIA Dependency
  - NVIDIA drivers, **CUDA required for nvml libraries** (see [RPM Fusion](https://rpmfusion.org/Howto/NVIDIA))

- NVTOP Dependencies
  - CMake, ncurses, GCC/G++ and Git
  ```bash
  sudo dnf install cmake ncurses-devel gcc gcc-c++ git
  ```

- NVTOP
  - Follow the [NVTOP Build](#nvtop-build)

### OpenSUSE

A standalone application is available as an [AppImage](#appimage).

#### OpenSUSE 16.0 and newer

```bash
sudo zypper install nvtop
```

#### Build process for OpenSUSE

- AMD Dependency
  ```bash
  sudo zypper install libdrm-devel
  ```

- NVIDIA Dependency
  - NVIDIA drivers (see [SUSE Support Database](https://en.opensuse.org/SDB:NVIDIA_drivers))

- NVTOP Dependencies
  - CMake, ncurses, GCC/G++ and Git
    ```bash
    sudo zypper install cmake ncurses-devel gcc gcc-c++ git
    ```

- NVTOP
  - Follow the [NVTOP Build](#nvtop-build)

### Arch Linux

```bash
sudo pacman -S nvtop
```

### Gentoo

```bash
sudo emerge -av nvtop
```

### AppImage

An AppImage is a standalone application. Just download the AppImage, make it executable and run it!

- Go to the [release page](https://github.com/Syllo/nvtop/releases/latest) and download `nvtop-<version>-x86_64.AppImage` (or `nvtop-<version>-aarch64.AppImage` on ARM)

```bash
# Go to the download location ** The path may differ on your system **
cd $HOME/Downloads
# Make the AppImage executable
chmod u+x nvtop-<version>-<arch>.AppImage
# Enjoy nvtop
./nvtop-<version>-<arch>.AppImage
```

The AppImage is built on AlmaLinux 8 and uses the host's glibc, udev, libdrm
and GPU driver libraries, so it runs on glibc-based distributions with glibc
2.28 or newer (Ubuntu 20.04+, Debian 10+, RHEL 8+).

If you are curious how that works, please visit the [AppImage website](https://appimage.org/).

### Snap

```bash
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

### Pixi

```bash
pixi global install nvtop
```

### Docker

- NVIDIA drivers (same as above)

- [nvidia-docker](https://github.com/NVIDIA/nvidia-docker) (See [Container Toolkit Installation Guide](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/install-guide.html#docker))
  ```bash
  git clone https://github.com/Syllo/nvtop.git && cd nvtop
  sudo docker build --tag nvtop .
  sudo docker run -it --rm --runtime=nvidia --gpus=all --pid=host nvtop
  ```

### WSL2

Nvtop support in WSL2 mostly boils down to GPU/Accelerator vendor support of
WSL2. Nvtop should work out of the box for the following configurations:

- NVIDIA: Please refer to [NVIDIA's user guide to get started with CUDA on
WSL2](https://docs.nvidia.com/cuda/wsl-user-guide/index.html#getting-started-with-cuda-on-wsl)
to avoid overiding the driver exposed by WSL2 (from Windows) with the NVIDIA
Linux drivers.

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
- If `nvtop` exits with `ncurses: cannot initialize terminal type ($TERM="unknown")`, ensure that `$TERM` is set to a valid terminal type such as `xterm-256color`.
- **Putty**: Tell putty not to lie about its capabilities (`$TERM`) by setting the field ``Terminal-type string`` to ``putty`` in the menu
  ``Connection > Data > Terminal Details``.
- `NO GPU to monitor.` for NVIDIA GPUs:
  - To query device information, `nvtop` tries to load `libnvidia-ml.so.1`
  first, then `libnvidia-ml.so`. If neither NVIDIA driver library can be
  loaded, `nvtop` cannot monitor your NVIDIA device.
  - On `WSL2`, the installation instructions are slightly different since the
  driver is being exposed by Windows to the virtual machine (see instruction
  link in [the WSL2 section](#wsl2)). If you install the NVIDIA linux drivers
  inside WSL2, you may encounter a version mismatch error or nvtop may silently
  not work at all.

License
-------

NVTOP is licensed under the GPLv3 license or any later version.
You will find a copy of the license inside the COPYING file of the repository or
at the GNU website <[www.gnu.org/licenses/](http://www.gnu.org/licenses/)>.
