# Build the AppImage

The AppImage is built on AlmaLinux 8 (glibc 2.28) and does not bundle glibc.
The binary therefore runs on any glibc-based distribution with glibc 2.28 or
newer, and uses the host's udev, libdrm and GPU driver libraries.

## x86_64

```bash
podman run --rm -it -v "$PWD:/nvtop" -w /nvtop almalinux:8 ./AppImage/make_appimage.sh
```

## aarch64

On a native aarch64 host, run the command above. On an x86_64 host, install
`qemu-user-static` with binfmt support first (e.g. `qemu-user-static-binfmt` on
Arch, `qemu-user-static` on Debian/Ubuntu), then select the arm64 image:

```bash
podman run --rm -it --arch arm64 -v "$PWD:/nvtop" -w /nvtop almalinux:8 ./AppImage/make_appimage.sh
```

The resulting `nvtop-<version>-<arch>.AppImage` is written to the repository
root.
