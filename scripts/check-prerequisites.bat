@echo off
REM Check Windows Build Prerequisites for NVTOP
echo ========================================
echo NVTOP Windows Build - Prerequisite Check
echo ========================================
echo.

set ERRORS=0

REM Check Windows version
echo [1/6] Checking Windows version...
ver | findstr /i "10.0" >nul
if errorlevel 1 (
    echo   ❌ Windows 10/11 not detected
    set /a ERRORS+=1
) else (
    echo   ✅ Windows 10/11 detected
)

REM Check MSYS2
echo [2/6] Checking MSYS2...
if exist "C:\msys64\usr\bin\bash.exe" (
    echo   ✅ MSYS2 found at C:\msys64
) else (
    echo   ❌ MSYS2 not found
    echo      Install from: https://www.msys2.org/
    set /a ERRORS+=1
)

REM Check MSYS2 packages
echo [3/6] Checking MSYS2 packages...
if exist "C:\msys64\ucrt64\bin\gcc.exe" (
    echo   ✅ GCC compiler found
) else (
    echo   ❌ GCC not found
    echo      Run in MSYS2 UCRT64: pacman -S mingw-w64-ucrt-x86_64-toolchain
    set /a ERRORS+=1
)

if exist "C:\msys64\ucrt64\bin\cmake.exe" (
    echo   ✅ CMake found
) else (
    echo   ❌ CMake not found
    echo      Run in MSYS2 UCRT64: pacman -S mingw-w64-ucrt-x86_64-cmake
    set /a ERRORS+=1
)

if exist "C:\msys64\ucrt64\bin\ninja.exe" (
    echo   ✅ Ninja build system found
) else (
    echo   ⚠️  Ninja not found (optional but recommended)
    echo      Run in MSYS2 UCRT64: pacman -S mingw-w64-ucrt-x86_64-ninja
)

REM Check for ncurses
echo [4/6] Checking ncurses library...
if exist "C:\msys64\ucrt64\lib\libncursesw.a" (
    echo   ✅ ncurses library found
) else (
    echo   ❌ ncurses not found
    echo      Run in MSYS2 UCRT64: pacman -S mingw-w64-ucrt-x86_64-ncurses
    set /a ERRORS+=1
)

REM Check NVIDIA drivers
echo [5/6] Checking NVIDIA drivers...
where nvidia-smi >nul 2>&1
if errorlevel 1 (
    echo   ⚠️  nvidia-smi not found
    echo      NVIDIA GPU monitoring will not work without drivers
) else (
    echo   ✅ NVIDIA drivers detected
    nvidia-smi --query-gpu=name --format=csv,noheader 2>nul
)

REM Check for NVML
echo [6/6] Checking NVML (nvml.dll)...
where nvml.dll >nul 2>&1
if errorlevel 1 (
    if exist "C:\Program Files\NVIDIA Corporation\NVSMI\nvml.dll" (
        echo   ✅ nvml.dll found in NVIDIA folder
    ) else (
        echo   ⚠️  nvml.dll not found
        echo      Install CUDA Toolkit for NVIDIA GPU support
        echo      Download: https://developer.nvidia.com/cuda-downloads
    )
) else (
    echo   ✅ nvml.dll found in PATH
)

echo.
echo ========================================
echo Summary
echo ========================================

if %ERRORS% EQU 0 (
    echo ✅ All required prerequisites are installed!
    echo.
    echo You can now build NVTOP:
    echo   build-windows-native.bat
) else (
    echo ❌ %ERRORS% error(s) found
    echo.
    echo Please install missing prerequisites:
    echo.
    echo 1. Install MSYS2 from https://www.msys2.org/
    echo 2. Run MSYS2 UCRT64 terminal
    echo 3. Update MSYS2: pacman -Syu
    echo 4. Install packages:
    echo    pacman -S mingw-w64-ucrt-x86_64-toolchain ^
    echo              mingw-w64-ucrt-x86_64-cmake ^
    echo              mingw-w64-ucrt-x86_64-ninja ^
    echo              mingw-w64-ucrt-x86_64-ncurses
    echo.
    echo For NVIDIA support, also install CUDA Toolkit
)

echo.
echo For detailed instructions, see:
echo   README_WINDOWS.md
echo   WINDOWS_NATIVE_BUILD.md
echo.
pause
