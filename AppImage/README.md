# Build the AppImage

```bash
podman pull ubuntu:18.04
podman run --interactive --tty --rm --volume $PWD:/nvtop ubuntu:24.04
cd nvtop
./AppImage/make_appimage.sh
```
