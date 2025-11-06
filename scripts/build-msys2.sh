#!/bin/bash
# MSYS2 Build Script for NVTOP (Experimental)
# WARNING: This is experimental and will require source code modifications
# Run this in MSYS2 UCRT64 terminal

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}========================================${NC}"
echo -e "${YELLOW}NVTOP MSYS2 Build Script (Experimental)${NC}"
echo -e "${YELLOW}========================================${NC}"
echo ""

echo -e "${RED}WARNING: This is experimental!${NC}"
echo -e "${RED}Native Windows builds require significant source code modifications.${NC}"
echo -e "${RED}For production use, please use WSL2 instead.${NC}"
echo ""
read -p "Continue anyway? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    exit 1
fi

# Check if running in MSYS2
if [[ ! "$MSYSTEM" == "UCRT64" ]]; then
    echo -e "${RED}Error: This script must be run in MSYS2 UCRT64 terminal!${NC}"
    echo -e "${YELLOW}Please launch 'MSYS2 UCRT64' and run this script again.${NC}"
    exit 1
fi

echo -e "${GREEN}Step 1: Installing dependencies...${NC}"
pacman -S --needed --noconfirm \
    base-devel \
    mingw-w64-ucrt-x86_64-toolchain \
    mingw-w64-ucrt-x86_64-cmake \
    mingw-w64-ucrt-x86_64-ncurses \
    mingw-w64-ucrt-x86_64-pdcurses \
    git

echo ""
echo -e "${GREEN}Step 2: Creating build directory...${NC}"
cd "$(dirname "$0")"
mkdir -p build-msys2
cd build-msys2

echo ""
echo -e "${GREEN}Step 3: Configuring CMake...${NC}"
echo -e "${YELLOW}Note: All GPU support is disabled for Windows build${NC}"

cmake .. \
    -G "MSYS Makefiles" \
    -DCMAKE_BUILD_TYPE=Release \
    -DNVIDIA_SUPPORT=OFF \
    -DAMDGPU_SUPPORT=OFF \
    -DINTEL_SUPPORT=OFF \
    -DAPPLE_SUPPORT=OFF \
    -DMSM_SUPPORT=OFF \
    -DPANFROST_SUPPORT=OFF \
    -DPANTHOR_SUPPORT=OFF \
    -DASCEND_SUPPORT=OFF \
    -DV3D_SUPPORT=OFF \
    -DTPU_SUPPORT=OFF \
    -DROCKCHIP_SUPPORT=OFF \
    -DMETAX_SUPPORT=OFF

echo ""
echo -e "${YELLOW}========================================${NC}"
echo -e "${YELLOW}CMake configuration complete${NC}"
echo -e "${YELLOW}========================================${NC}"
echo ""
echo -e "${RED}EXPECTED ERRORS:${NC}"
echo -e "${RED}The build will likely fail due to Linux-specific code.${NC}"
echo -e "${RED}To make this work, you need to:${NC}"
echo -e "${RED}  1. Port platform-specific code (process info, device discovery)${NC}"
echo -e "${RED}  2. Implement Windows GPU APIs (NVML, ADL, etc.)${NC}"
echo -e "${RED}  3. Replace Unix-specific system calls${NC}"
echo ""
read -p "Attempt to build anyway? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo -e "${YELLOW}Build cancelled. Configuration files are in build-msys2/${NC}"
    exit 0
fi

echo ""
echo -e "${GREEN}Step 4: Building...${NC}"
make -j$(nproc) || {
    echo ""
    echo -e "${RED}========================================${NC}"
    echo -e "${RED}Build failed as expected!${NC}"
    echo -e "${RED}========================================${NC}"
    echo ""
    echo -e "${YELLOW}To make NVTOP work on native Windows, you need to:${NC}"
    echo ""
    echo -e "${YELLOW}1. Create Windows implementations for:${NC}"
    echo -e "   - src/get_process_info_windows.c"
    echo -e "   - src/device_discovery_windows.c"
    echo -e "   - src/info_messages_windows.c"
    echo ""
    echo -e "${YELLOW}2. Implement GPU support using Windows APIs:${NC}"
    echo -e "   - NVIDIA: nvml.dll (NVIDIA Management Library)"
    echo -e "   - AMD: ADL SDK (AMD Display Library)"
    echo -e "   - Intel: Intel GPU Performance APIs"
    echo ""
    echo -e "${YELLOW}3. Port Unix-specific code:${NC}"
    echo -e "   - Replace getopt with Windows alternatives"
    echo -e "   - Handle ncurses differences (or use PDCurses)"
    echo -e "   - Replace signal handling"
    echo -e "   - Remove /proc filesystem dependencies"
    echo ""
    echo -e "${YELLOW}For now, please use WSL2 for the best Windows experience.${NC}"
    echo -e "${YELLOW}See WINDOWS_BUILD.md for instructions.${NC}"
    exit 1
}

echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Build successful!${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo -e "${GREEN}Binary location: build-msys2/src/nvtop.exe${NC}"
echo -e "${YELLOW}Note: This build will have limited functionality without GPU support.${NC}"
