@echo off
REM NVTOP launcher for Windows Terminal
REM This script ensures the executable can find its DLLs

REM Get the directory where this script is located
set SCRIPT_DIR=%~dp0

REM Set TERM for better ncurses compatibility
if not defined TERM set TERM=xterm-256color

REM Run nvtop from the build directory where DLLs are located
cd /d "%SCRIPT_DIR%build-windows\src"
nvtop.exe %*
