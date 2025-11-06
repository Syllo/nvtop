@echo off
REM Quick Native Windows Build Script for NVTOP
REM This script builds NVTOP using MSYS2/MinGW

echo ========================================
echo NVTOP Native Windows Build
echo ========================================
echo.

REM Check if MSYS2 is installed
if not exist "C:\msys64\usr\bin\bash.exe" (
    echo ERROR: MSYS2 not found!
    echo.
    echo Please install MSYS2 from: https://www.msys2.org/
    echo Install to default location: C:\msys64
    echo.
    echo After installation:
    echo 1. Run MSYS2 UCRT64 terminal
    echo 2. Run: pacman -Syu
    echo 3. Run: pacman -S mingw-w64-ucrt-x86_64-toolchain mingw-w64-ucrt-x86_64-cmake mingw-w64-ucrt-x86_64-ninja mingw-w64-ucrt-x86_64-ncurses
    echo 4. Run this script again
    echo.
    pause
    exit /b 1
)

echo MSYS2 found. Proceeding with build...
echo.

REM Add MSYS2 to PATH
set PATH=C:\msys64\ucrt64\bin;%PATH%

REM Check if build directory exists
if not exist build-windows (
    echo Creating build directory...
    mkdir build-windows
)

cd build-windows

echo Configuring with CMake...
cmake .. -G "Ninja" ^
    -DCMAKE_C_COMPILER=gcc ^
    -DCMAKE_CXX_COMPILER=g++ ^
    -DCMAKE_BUILD_TYPE=Release ^
    -DNVIDIA_SUPPORT=ON ^
    -DAMDGPU_SUPPORT=OFF ^
    -DINTEL_SUPPORT=OFF ^
    -DAPPLE_SUPPORT=OFF

if errorlevel 1 (
    echo.
    echo CMake configuration failed!
    cd ..
    pause
    exit /b 1
)

echo.
echo Building NVTOP...
ninja

if errorlevel 1 (
    echo.
    echo Build failed!
    cd ..
    pause
    exit /b 1
)

cd ..

echo.
echo ========================================
echo Build completed successfully!
echo ========================================
echo.
echo Executable location:
echo   %CD%\build-windows\src\nvtop.exe
echo.
echo To run NVTOP:
echo   build-windows\src\nvtop.exe
echo.
echo To create distributable package:
echo   Run: make-distribution.bat
echo.
pause
