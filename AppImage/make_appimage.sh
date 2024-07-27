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

get_appimagetool() {
	ARCH=x86_64
	APPIMAGETOOL=$(wget -q https://api.github.com/repos/probonopd/go-appimage/releases -O - | sed 's/"/ /g; s/ /\n/g' | grep -o 'https.*continuous.*tool.*86_64.*mage$')
	wget -q "$APPIMAGETOOL" -O ./appimagetool
	chmod u+x ./appimagetool
	./appimagetool --appimage-extract
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
  ./squashfs-root/AppRun --appdir AppDir --exclude-library="*udev*" --desktop-file AppDir/usr/share/applications/nvtop.desktop --icon-file AppDir/usr/share/icons/nvtop.svg
  rm -rf ./squashfs-root
  get_appimagetool
  VERSION=$(./AppDir/AppRun --version | awk '{print $NF}') ./squashfs-root/AppRun -s AppDir
}

create_AppImage
