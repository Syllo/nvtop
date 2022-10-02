#!/usr/bin/env bash

configure_nvtop() {
  cmake -S .. -B nvtop_build -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DCMAKE_INSTALL_PREFIX=/usr
}

build_nvtop() {
  cmake --build nvtop_build
}

install_nvtop_AppDir() {
  DESTDIR=../AppDir cmake --build nvtop_build --target install
}

get_linuxdeploy() {
  wget -nc https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
  chmod u+x linuxdeploy-x86_64.AppImage
}

create_AppImage() {
  configure_nvtop
  build_nvtop
  install_nvtop_AppDir
  get_linuxdeploy
  ./linuxdeploy-x86_64.AppImage --appdir AppDir --output appimage
}

create_AppImage