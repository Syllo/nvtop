#!/usr/bin/env bash

install_deps() {
  apt-get update
  apt-get install -y gcc g++ libncurses5-dev libncursesw5-dev libdrm-dev wget file libudev-dev ninja-build make python3-venv
}

configure_nvtop() {
  cmake -B build -S . -DCMAKE_BUILD_TYPE=Release -DUSE_LIBUDEV_OVER_LIBSYSTEMD=ON -DCMAKE_INSTALL_PREFIX=/usr
}

build_nvtop() {
  cmake --build build
}

install_nvtop_AppDir() {
  DESTDIR=$PWD/AppDir cmake --build build --target install
}

get_linuxdeploy() {
  wget -nc https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
  chmod u+x linuxdeploy-x86_64.AppImage
  ./linuxdeploy-x86_64.AppImage --appimage-extract
}

get_cmake() {
  python3 -m venv .venv
  source .venv/bin/activate
  pip install --upgrade pip
  pip install cmake
}

create_AppImage() {
  install_deps
  get_cmake
  configure_nvtop
  build_nvtop
  install_nvtop_AppDir
  get_linuxdeploy
  ./squashfs-root/AppRun --appdir AppDir --output appimage --exclude-library="*udev*" --desktop-file AppDir/usr/share/applications/nvtop.desktop --icon-file AppDir/usr/share/icons/nvtop.svg
}

create_AppImage
