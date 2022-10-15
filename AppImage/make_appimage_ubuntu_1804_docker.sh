#!/usr/bin/env bash

install_deps() {
  apt-get update
  apt-get install -y cmake gcc g++ libncurses5-dev libncursesw5-dev libdrm-dev wget file libudev-dev
}

configure_nvtop() {
  cmake ../.. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON -DUSE_LIBUDEV_OVER_LIBSYSTEMD=ON -DCMAKE_INSTALL_PREFIX=/usr
}

build_nvtop() {
  cmake --build .
}

install_nvtop_AppDir() {
  DESTDIR=../AppDir cmake --build . --target install
}

get_linuxdeploy() {
  wget -nc https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
  chmod u+x linuxdeploy-x86_64.AppImage
  ./linuxdeploy-x86_64.AppImage --appimage-extract
}

create_AppImage() {
  install_deps
  mkdir nvtop_build
  cd nvtop_build
  configure_nvtop
  build_nvtop
  install_nvtop_AppDir
  cd ..
  get_linuxdeploy
  ./squashfs-root/AppRun --appdir AppDir --output appimage --exclude-library="*udev*"
}

create_AppImage
