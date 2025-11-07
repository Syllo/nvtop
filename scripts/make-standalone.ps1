#!/usr/bin/env pwsh
<#
.SYNOPSIS
    Creates a standalone distribution package of NVTOP for Windows
.DESCRIPTION
    This script packages nvtop.exe with all required DLLs and creates a portable
    distribution that can run on any Windows system without MSYS2 installed.
.PARAMETER OutputDir
    Directory where the distribution package will be created (default: nvtop-dist)
.EXAMPLE
    .\make-standalone.ps1
    .\make-standalone.ps1 -OutputDir "C:\Tools\nvtop"
#>

param(
    [string]$OutputDir = "nvtop-dist"
)

# Colors
$esc = [char]27
$green = "${esc}[32m"
$yellow = "${esc}[33m"
$cyan = "${esc}[36m"
$reset = "${esc}[0m"

Write-Host "${cyan}╔════════════════════════════════════════╗${reset}"
Write-Host "${cyan}║  NVTOP Standalone Package Creator     ║${reset}"
Write-Host "${cyan}╚════════════════════════════════════════╝${reset}"
Write-Host ""

# Check if nvtop.exe exists
$nvtopExe = "build-windows-MinGW\src\nvtop.exe"
if (-not (Test-Path $nvtopExe)) {
    Write-Error "nvtop.exe not found at: $nvtopExe"
    Write-Host "${yellow}Please build nvtop first using: .\build-windows-native.ps1 -All${reset}"
    exit 1
}

# Create output directory
Write-Host "${green}Creating distribution directory: $OutputDir${reset}"
if (Test-Path $OutputDir) {
    Remove-Item -Recurse -Force $OutputDir
}
New-Item -ItemType Directory -Path $OutputDir | Out-Null

# Copy nvtop.exe
Write-Host "Copying nvtop.exe..."
Copy-Item $nvtopExe $OutputDir\

# Required DLLs from MSYS2
$requiredDlls = @(
    "libncursesw6.dll",
    "libwinpthread-1.dll"
)

$msys2Bin = "C:\msys64\ucrt64\bin"
if (Test-Path $msys2Bin) {
    Write-Host "Copying required DLLs from MSYS2..."
    foreach ($dll in $requiredDlls) {
        $dllPath = Join-Path $msys2Bin $dll
        if (Test-Path $dllPath) {
            Copy-Item $dllPath $OutputDir\
            Write-Host "  ✓ $dll"
        }
        else {
            Write-Warning "  ✗ $dll not found"
        }
    }
}
else {
    Write-Warning "MSYS2 not found at $msys2Bin"
    Write-Warning "Required DLLs must be provided manually"
}

# Create a simple launcher batch file
$launcherContent = @"
@echo off
REM NVTOP Launcher
REM Run nvtop.exe from the same directory

"%~dp0nvtop.exe" %*
"@
Set-Content -Path "$OutputDir\nvtop.bat" -Value $launcherContent

# Create README
$readmeContent = @"
NVTOP for Windows - Standalone Distribution
============================================

Version: 3.2.0
Build Date: $(Get-Date -Format "yyyy-MM-dd")

Quick Start
-----------
1. Run 'nvtop.bat' or 'nvtop.exe' directly
2. Use Ctrl+C to exit

Usage
-----
  nvtop.exe              # Launch interactive monitoring
  nvtop.exe --help       # Show all options
  nvtop.exe --snapshot   # Output JSON snapshot

Files Included
--------------
  nvtop.exe              - Main executable
  nvtop.bat              - Launcher script
  libncursesw6.dll       - ncurses terminal library
  libwinpthread-1.dll    - Windows POSIX threading
  README.txt             - This file

Requirements
------------
- Windows 10 or later (64-bit)
- NVIDIA GPU with drivers installed (for GPU monitoring)
- Visual C++ Redistributable 2015-2022 (usually pre-installed)

License
-------
GPLv3 - See https://github.com/Syllo/nvtop

For more information: https://github.com/Syllo/nvtop
"@
Set-Content -Path "$OutputDir\README.txt" -Value $readmeContent

Write-Host ""
Write-Host "${green}✓ Standalone distribution created successfully!${reset}"
Write-Host ""
Write-Host "Distribution location: ${cyan}$OutputDir${reset}"
Write-Host ""
Write-Host "Contents:"
Get-ChildItem $OutputDir | ForEach-Object {
    $size = if ($_.PSIsContainer) { "DIR" } else { "{0:N0} bytes" -f $_.Length }
    Write-Host "  $($_.Name.PadRight(25)) $size"
}
Write-Host ""
Write-Host "${green}To test the distribution:${reset}"
Write-Host "  cd $OutputDir"
Write-Host "  .\nvtop.bat --snapshot"
Write-Host ""
