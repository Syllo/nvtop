NVTOP
=====

What is NVTOP?
--------------

Nvtop stands for NVidia TOP, a (h)top like task monitor for NVIDIA GPUs. It can
handle multiple GPUs and print information about them in a htop familiar way.

Because a picture is worth a thousand words:

![NVTOP interface](/screenshot/NVTOP_ex1.png)

Dependencies
------------

Three libraries are required:

* The NVIDIA Management Library (NVML).
  * This queries the GPU for information.
* The ncurses library driving the user interface.
  * This makes the screen look beautiful.

Limitations
-----------

The NVML library does not support some of the queries for GPUs coming before the
Kepler microarchitecture. Anything starting at GeForce 600, GeForce 800M and
successor should work fine. For more information about supported GPUs please
take a look at the [NVML documentation](http://docs.nvidia.com/deploy/nvml-api/nvml-api-reference.html#nvml-api-reference).

Build
-----

CMAKE is used as build manager.

To build the binary on Linux:

```bash
mkdir build && cd build
cmake ..
make
make install # You may need sufficent permission for that (root)
```

The build system support multiple build type (-DCMAKE_BUILD_TYPE):

* Release: Binary without debug information
* RelWithDebInfo: Binary with debug information
* Debug: Compile warning flags and address/undefined sanitizer (For development only)
* Optimized: Build with architecture specific optimisations (May be not portable across machines with different processor)

Build Options
-------------

None for the moment.

License
-------

Nvtop is licensed under the GPLV3 license or any later version.
You will find a copy of the license inside the COPYING file of the repository or
at the gnu website <[www.gnu.org/licenses/](http://www.gnu.org/licenses/)>.
