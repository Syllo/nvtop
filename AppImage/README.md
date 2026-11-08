# Build the AppImage

The AppImage is built on AlmaLinux 8 (glibc 2.28) and does not bundle glibc.
The binary therefore runs on any glibc-based distribution with glibc 2.28 or
newer, and uses the host's udev, libdrm and GPU driver libraries.

```bash
podman run --rm -it -v "$PWD:/nvtop" -w /nvtop almalinux:8 ./AppImage/make_appimage.sh
```

The resulting `nvtop-<version>-<arch>.AppImage` is written to the repository
root.
