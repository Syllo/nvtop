@echo off
REM NVTOP Windows Launcher
REM Adds DLL directory to PATH before launching nvtop.exe

set "SCRIPT_DIR=%~dp0"
set "PATH=%SCRIPT_DIR%;%PATH%"
"%SCRIPT_DIR%nvtop.exe" %*
