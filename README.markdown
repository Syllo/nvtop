NVTOP-Local
=====

What is this repository about?
-----------------

This repository provides the packed nvtop along with its dependency (`ncurses 6.3`) and the installation script for non-privileged users. 

Note that this repo doesn't provide the *NVML* and the CUDA driver as both require root access.


What is NVTOP?
--------------

Nvtop stands for NVidia TOP, a (h)top like task monitor for NVIDIA GPUs. It can
handle multiple GPUs and print information about them in a htop familiar way.

For detailed introduction, please refer to the original [site](https://github.com/Syllo/nvtop/blob/master/README.markdown)


Table of Contents
-----------------

- [Installation](#installation)
- [Troubleshoot](#troubleshoot)
- [License](#license)

Build
-----

Two libraries are required in order for NVTOP to display GPU information:

* The *NVIDIA Management Library* (*NVML*) which comes with the GPU driver.
  * This queries the GPU for information.
  * Ask the administrator if you do not have it installed nor do you have privileged access.
* The *ncurses* library driving the user interface.
  * This makes the screen look beautiful.
  * Provided in this repository.


## Installation

To install under the home directory (`$HOME/bin`), simply run
```bash
bash install.sh
```

As some workstations may require the users to move their exectuables to specific location (such as `/tmp2`), you could specify the path where you want to install by
```bash
bash install.sh $PATH_TO_INSTALLATION
```

After running the script, remember to add the path to the `PATH` environment variable. For example,
```bash
echo export PATH=/tmp2/`whoami`/bin:$PATH >> ~/.zshrc
source ~/.zshrc
```

If you use **conda** as environment manager and encounter an error while building nvtop, try `conda deactivate` before invoking `cmake`.

License
-------

Nvtop is licensed under the GPLV3 license or any later version.
You will find a copy of the license inside the COPYING file of the repository or
at the gnu website <[www.gnu.org/licenses/](http://www.gnu.org/licenses/)>.
