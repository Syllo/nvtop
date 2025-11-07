@echo off
REM Quick build script for NVTOP in WSL2
REM Usage: build-wsl2.bat

echo ========================================
echo NVTOP WSL2 Quick Build Script
echo ========================================
echo.

REM Check if WSL is installed
wsl --version >nul 2>&1
if errorlevel 1 (
    echo WSL2 is not installed!
    echo Please install WSL2 first:
    echo   wsl --install -d Ubuntu-22.04
    echo.
    pause
    exit /b 1
)

echo WSL2 detected. Starting build process...
echo.

REM Build NVTOP in WSL2 (using current directory)
set "WSLPATH=%CD:\=/%"
set "WSLPATH=%WSLPATH:C:=/mnt/c%"
wsl bash -c "cd '%WSLPATH%' && mkdir -p build && cd build && cmake .. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON && make -j$(nproc)"

if errorlevel 1 (
    echo.
    echo Build failed! Check the error messages above.
    pause
    exit /b 1
)

echo.
echo ========================================
echo Build completed successfully!
echo ========================================
echo.
echo To install NVTOP:
echo   wsl sudo make -C build install
echo.
echo To run NVTOP:
echo   wsl nvtop
echo.
echo Or run now? (Y/N)
choice /c YN /n /m "Run NVTOP now? "

if errorlevel 2 goto :end
if errorlevel 1 goto :run

:run
wsl "%WSLPATH%/build/src/nvtop"
goto :end

:end
echo.
pause
