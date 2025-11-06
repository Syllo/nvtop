@echo off
REM Create Windows distribution package for NVTOP

echo ========================================
echo Creating NVTOP Windows Distribution
echo ========================================
echo.

if not exist "build-windows\src\nvtop.exe" (
    echo ERROR: nvtop.exe not found!
    echo Please build first: build-windows-native.bat
    pause
    exit /b 1
)

REM Create distribution directory
set DIST_DIR=nvtop-windows-x64
if exist "%DIST_DIR%" (
    echo Cleaning old distribution directory...
    rmdir /s /q "%DIST_DIR%"
)

mkdir "%DIST_DIR%"

echo Copying executable...
copy "build-windows\src\nvtop.exe" "%DIST_DIR%\"

echo Copying required DLLs...
if exist "C:\msys64\ucrt64\bin\libncursesw6.dll" (
    copy "C:\msys64\ucrt64\bin\libncursesw6.dll" "%DIST_DIR%\"
)
if exist "C:\msys64\ucrt64\bin\libgcc_s_seh-1.dll" (
    copy "C:\msys64\ucrt64\bin\libgcc_s_seh-1.dll" "%DIST_DIR%\"
)
if exist "C:\msys64\ucrt64\bin\libwinpthread-1.dll" (
    copy "C:\msys64\ucrt64\bin\libwinpthread-1.dll" "%DIST_DIR%\"
)

echo Copying NVIDIA DLL (if available)...
if exist "C:\Program Files\NVIDIA Corporation\NVSMI\nvml.dll" (
    copy "C:\Program Files\NVIDIA Corporation\NVSMI\nvml.dll" "%DIST_DIR%\"
    echo   - nvml.dll copied
) else (
    echo   - nvml.dll not found (NVIDIA support will not work)
)

echo Creating README...
(
echo NVTOP for Windows x64
echo =====================
echo.
echo Version: 3.2.0 (Windows Native Build^)
echo Build Date: %DATE%
echo.
echo REQUIREMENTS
echo ------------
echo - Windows 10 (1809^) or Windows 11
echo - NVIDIA GPU with recent drivers (for GPU monitoring^)
echo - CUDA Toolkit or nvml.dll (included if available^)
echo.
echo USAGE
echo -----
echo Run: nvtop.exe
echo.
echo Command line options:
echo   -h, --help          : Show help
echo   -v, --version       : Show version
echo   -d, --delay N       : Set refresh rate (N * 0.1 seconds^)
echo   -s, --snapshot      : Snapshot mode (non-interactive^)
echo   -p, --no-plot       : Disable bar plot
echo   -P, --no-processes  : Disable process list
echo   -C, --no-color      : Disable colors
echo   -f, --freedom-unit  : Use Fahrenheit
echo   -i, --gpu-info      : Show additional GPU parameters
echo.
echo INTERACTIVE CONTROLS
echo --------------------
echo   q            : Quit
echo   F2           : Setup/Options
echo   F12          : Save preferences
echo   Arrow keys   : Navigate
echo.
echo TROUBLESHOOTING
echo ---------------
echo 1. GPU not detected:
echo    - Install NVIDIA drivers
echo    - Ensure nvml.dll is present
echo    - Run nvidia-smi to verify GPU is working
echo.
echo 2. Display issues:
echo    - Use Windows Terminal for best results
echo    - Avoid old cmd.exe
echo    - Set: set TERM=xterm-256color
echo.
echo 3. Permission errors:
echo    - Run as Administrator for full process info
echo.
echo KNOWN LIMITATIONS
echo -----------------
echo - Only NVIDIA GPUs supported
echo - Process monitoring is basic compared to Linux
echo - Some Unicode characters may not display correctly
echo.
echo For more information, see WINDOWS_NATIVE_BUILD.md
echo.
echo License: GPLv3
echo Homepage: https://github.com/Syllo/nvtop
) > "%DIST_DIR%\README.txt"

echo Creating launcher script...
(
echo @echo off
echo set PATH=%%~dp0;%%PATH%%
echo "%%~dp0nvtop.exe" %%*
) > "%DIST_DIR%\nvtop.bat"

echo.
echo ========================================
echo Distribution created successfully!
echo ========================================
echo.
echo Location: %CD%\%DIST_DIR%
echo.
echo Contents:
dir /b "%DIST_DIR%"
echo.
echo To create ZIP:
echo   Compress-Archive -Path %DIST_DIR% -DestinationPath nvtop-windows-x64.zip
echo.
echo Or manually zip the %DIST_DIR% folder
echo.
pause
