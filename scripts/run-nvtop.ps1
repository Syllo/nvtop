#!/usr/bin/env pwsh
<#
.SYNOPSIS
    Launcher script for NVTOP on Windows
.DESCRIPTION
    This script ensures the required DLLs are in the PATH before launching nvtop.exe
    It handles both development and deployment scenarios.
.EXAMPLE
    .\run-nvtop.ps1
    .\run-nvtop.ps1 --snapshot
    .\run-nvtop.ps1 --help
#>

param(
    [Parameter(ValueFromRemainingArguments)]
    [string[]]$NvtopArgs
)

# Script directory
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

# Check for nvtop.exe in build directory
$nvtopExe = Join-Path $scriptDir "build-windows-MinGW\src\nvtop.exe"

if (-not (Test-Path $nvtopExe)) {
    Write-Error "nvtop.exe not found at: $nvtopExe"
    Write-Host "Please build nvtop first using: .\build-windows-native.ps1 -All"
    exit 1
}

# Add MSYS2 bin directory to PATH for this session
$msys2Bin = "C:\msys64\ucrt64\bin"
if (Test-Path $msys2Bin) {
    $env:PATH = "$msys2Bin;$env:PATH"
    Write-Host "Added MSYS2 UCRT64 binaries to PATH" -ForegroundColor Green
}
else {
    Write-Warning "MSYS2 not found at C:\msys64 - DLLs must be copied manually"
}

# Launch nvtop with provided arguments
Write-Host "Launching nvtop..." -ForegroundColor Cyan
& $nvtopExe @NvtopArgs
exit $LASTEXITCODE
