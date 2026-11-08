#!/usr/bin/env bash
#
# Build the nvtop AppImage.
#
# The AppImage is built on AlmaLinux 8 (glibc 2.28) and does NOT bundle glibc.
# This follows the AppImage recommendation of building on the oldest base you
# intend to support: the payload runs on any glibc >= 2.28 and the host's glibc,
# libudev, libdrm and GPU driver libraries are used at runtime. Bundling glibc
# would mix it with host libraries that nvtop dlopen()s (libnvidia-ml, libdrm,
# ...), which is what broke the AppImage on RHEL 7 and other systems.

set -euo pipefail

cd "$(dirname "$0")/.."

ARCH="$(uname -m)"
case "$ARCH" in
	x86_64|aarch64) ;;
	*)
		echo "Unsupported architecture: $ARCH (only x86_64 and aarch64 are built)" >&2
		exit 1
		;;
esac
export ARCH
export APPIMAGE_EXTRACT_AND_RUN=1
APPIMAGETOOL_URL="https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-$ARCH.AppImage"

install_deps() {
	dnf install -y \
		gcc gcc-c++ make cmake ncurses-devel libdrm-devel systemd-devel \
		wget curl file desktop-file-utils
}

configure_nvtop() {
	cmake -B build -S . -DCMAKE_BUILD_TYPE=Release \
		-DUSE_LIBUDEV_OVER_LIBSYSTEMD=ON -DCMAKE_INSTALL_PREFIX=/usr
}

build_nvtop() {
	cmake --build build --parallel
}

install_nvtop_AppDir() {
	rm -rf AppDir
	DESTDIR="$PWD/AppDir" cmake --install build
}

bundle_dependencies() {
	local libdir="AppDir/usr/lib" lib path

	# Only bundle the ncurses family. Everything else (glibc, libudev, libdrm,
	# libgcc_s, ...) must come from the host so that it stays consistent with
	# the libraries nvtop loads at runtime.
	mkdir -p "$libdir"
	for lib in libncursesw.so.6 libncurses.so.6 libtinfo.so.6 libform.so.6 \
		libmenu.so.6 libpanel.so.6; do
		path="$(ldconfig -p | awk -v lib="$lib" '$1 == lib { print $NF; exit }')"
		if [[ -n "$path" ]]; then
			cp -vL "$path" "$libdir"
		fi
	done
}

configure_appdir() {
	cat > AppDir/AppRun <<- 'EOF'
		#!/bin/sh
		HERE="$(readlink -f "$(dirname "$0")")"
		export LD_LIBRARY_PATH="$HERE/usr/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
		exec "$HERE/usr/bin/nvtop" "$@"
	EOF
	chmod 755 AppDir/AppRun

	ln -sf usr/share/applications/nvtop.desktop AppDir/nvtop.desktop
	cp -f AppDir/usr/share/icons/hicolor/scalable/apps/nvtop.svg AppDir/nvtop.svg
	ln -sf nvtop.svg AppDir/.DirIcon
}

get_appimagetool() {
	local attempt

	for attempt in 1 2 3 4 5; do
		curl -fsSL --retry 3 -o ./appimagetool "$APPIMAGETOOL_URL" || true
		if [[ -s ./appimagetool ]]; then
			break
		fi
		rm -f ./appimagetool
		sleep 2
	done
	if [[ ! -s ./appimagetool ]]; then
		echo "Could not download appimagetool" >&2
		exit 1
	fi
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
