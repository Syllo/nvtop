@echo off
REM Launcher script for NVTOP on Windows
REM Ensures required DLLs are in PATH before launching

setlocal

REM Add MSYS2 bin directory to PATH
set "MSYS2_BIN=C:\msys64\ucrt64\bin"
if exist "%MSYS2_BIN%" (
    set "PATH=%MSYS2_BIN%;%PATH%"
    echo Added MSYS2 UCRT64 binaries to PATH
) else (
    echo WARNING: MSYS2 not found at C:\msys64
)

REM Check if nvtop.exe exists
if not exist "build-windows-MinGW\src\nvtop.exe" (
    echo ERROR: nvtop.exe not found
    echo Please build nvtop first using: build-windows-native.ps1 -All
    exit /b 1
)

REM Launch nvtop with all arguments
echo Launching nvtop...
build-windows-MinGW\src\nvtop.exe %*
exit /b %ERRORLEVEL%
