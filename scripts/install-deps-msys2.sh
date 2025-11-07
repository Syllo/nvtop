#!/bin/bash
# Install NVTOP build dependencies in MSYS2
# Run this in MSYS2 UCRT64 terminal

echo "========================================="
echo "Installing NVTOP Build Dependencies"
echo "========================================="
echo ""

echo "Updating package database..."
pacman -Sy

echo ""
echo "Installing build tools and dependencies..."
pacman -S --needed --noconfirm \
    mingw-w64-ucrt-x86_64-toolchain \
    mingw-w64-ucrt-x86_64-cmake \
    mingw-w64-ucrt-x86_64-ninja \
    mingw-w64-ucrt-x86_64-ncurses \
    mingw-w64-ucrt-x86_64-pdcurses \
    git

if [ $? -eq 0 ]; then
    echo ""
    echo "========================================="
    echo "✅ All dependencies installed!"
    echo "========================================="
    echo ""
    echo "You can now build NVTOP:"
    echo "  cd /path/to/nvtop"
    echo "  ./build-msys2.sh"
    echo ""
    echo "Or from Windows:"
    echo "  build-windows-native.bat"
    echo ""
else
    echo ""
    echo "❌ Installation failed!"
    echo "Please check the error messages above."
    echo ""
fi
