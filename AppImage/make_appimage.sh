#!/usr/bin/env bash

export ARCH="$(uname -m)"
export APPIMAGE_EXTRACT_AND_RUN=1
APPIMAGETOOL="https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-$ARCH.AppImage"

install_deps() {
	apt-get update
	apt-get install -y gcc g++ cmake libncurses5-dev libncursesw5-dev libdrm-dev \
	  wget file libudev-dev ninja-build cmake file desktop-file-utils
}

configure_nvtop() {
	cmake -B build -S . -DCMAKE_BUILD_TYPE=Release \
	  -DUSE_LIBUDEV_OVER_LIBSYSTEMD=ON -DCMAKE_INSTALL_PREFIX=/usr
}

build_nvtop() {
	cmake --build build
}

install_nvtop_AppDir() {
	DESTDIR=$PWD/AppDir cmake --build build --target install
}

bundle_dependencies() {
	mkdir -p AppDir/usr/lib
	ldd AppDir/usr/bin/nvtop | awk -F"[> ]" '{print $4}' \
	  | xargs -I {} cp -vf {} AppDir/usr/lib
	cp -v /lib64/ld-linux-x86-64.so.2 AppDir
}

configure_appdir() {
	cat >> AppDir/AppRun <<- 'EOF'
	#!/bin/sh
	HERE="$(readlink -f "$(dirname "$0")")"
	exec "$HERE/ld-linux-x86-64.so.2" \
	  --library-path "$HERE/usr/lib" "$HERE"/usr/bin/nvtop "$@"
	EOF
	chmod u+x AppDir/AppRun
	ln -s usr/share/applications/nvtop.desktop AppDir
	ln -s usr/share/icons/nvtop.svg AppDir
	ln -s usr/share/icons/nvtop.svg AppDir/.DirIcon
}

get_appimagetool() {
	wget -q "$APPIMAGETOOL" -O ./appimagetool
	chmod u+x ./appimagetool
}

create_AppImage() {
	install_deps
	configure_nvtop
	build_nvtop
	install_nvtop_AppDir
	bundle_dependencies
	configure_appdir
	get_appimagetool
	export VERSION="$(./AppDir/AppRun --version | awk '{print $NF}')"
	./appimagetool -n AppDir
}

create_AppImage
