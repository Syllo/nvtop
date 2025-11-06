# Build NVTOP in WSL2
# This script automates the process of building NVTOP in WSL2 from Windows

param(
    [switch]$InstallWSL,
    [switch]$SetupDeps,
    [switch]$Build,
    [switch]$Install,
    [switch]$Run,
    [switch]$All,
    [string]$Distribution = "Ubuntu-22.04"
)

$ErrorActionPreference = "Stop"

function Write-Header {
    param([string]$Message)
    Write-Host "`n========================================" -ForegroundColor Cyan
    Write-Host $Message -ForegroundColor Cyan
    Write-Host "========================================`n" -ForegroundColor Cyan
}

function Test-WSLInstalled {
    try {
        $wslVersion = wsl --version 2>&1
        return $true
    }
    catch {
        return $false
    }
}

function Install-WSL2 {
    Write-Header "Installing WSL2"
    
    if (Test-WSLInstalled) {
        Write-Host "WSL is already installed." -ForegroundColor Green
        wsl --version
        return
    }
    
    Write-Host "Installing WSL2..." -ForegroundColor Yellow
    Write-Host "Note: This requires administrator privileges and a system restart." -ForegroundColor Yellow
    
    try {
        wsl --install -d $Distribution
        Write-Host "`nWSL2 installation initiated. Please restart your computer and run this script again." -ForegroundColor Green
        Read-Host "Press Enter to exit"
        exit 0
    }
    catch {
        Write-Error "Failed to install WSL2. Please install manually: wsl --install"
    }
}

function Install-Dependencies {
    Write-Header "Installing Build Dependencies in WSL2"
    
    $installScript = @'
#!/bin/bash
set -e

echo "Updating package lists..."
sudo apt update

echo "Installing build dependencies..."
sudo apt install -y cmake libncurses5-dev libncursesw5-dev git gcc g++ libdrm-dev libsystemd-dev

echo "Dependencies installed successfully!"
'@
    
    # Write script to temp file in WSL
    $installScript | wsl bash -c "cat > /tmp/install_deps.sh && chmod +x /tmp/install_deps.sh && /tmp/install_deps.sh"
    
    Write-Host "Dependencies installed successfully!" -ForegroundColor Green
}

function Build-NVTOP {
    Write-Header "Building NVTOP"
    
    $nvtopPath = $PSScriptRoot
    $wslPath = $nvtopPath -replace '\\', '/' -replace 'C:', '/mnt/c'
    
    $buildScript = @"
#!/bin/bash
set -e

cd "$wslPath"

echo "Creating build directory..."
mkdir -p build
cd build

echo "Running CMake..."
cmake .. -DNVIDIA_SUPPORT=ON -DAMDGPU_SUPPORT=ON -DINTEL_SUPPORT=ON

echo "Building NVTOP..."
make -j`$(nproc)

echo "Build completed successfully!"
echo "Binary location: $wslPath/build/src/nvtop"
"@
    
    $buildScript | wsl bash
    
    Write-Host "`nBuild completed successfully!" -ForegroundColor Green
    Write-Host "Binary location: $nvtopPath\build\src\nvtop" -ForegroundColor Cyan
}

function Install-NVTOP {
    Write-Header "Installing NVTOP"
    
    $nvtopPath = $PSScriptRoot
    $wslPath = $nvtopPath -replace '\\', '/' -replace 'C:', '/mnt/c'
    
    $installScript = @"
#!/bin/bash
set -e

cd "$wslPath/build"

echo "Installing NVTOP..."
sudo make install

echo "NVTOP installed successfully!"
echo "You can now run: nvtop"
"@
    
    $installScript | wsl bash
    
    Write-Host "`nNVTOP installed successfully!" -ForegroundColor Green
    Write-Host "Run 'wsl nvtop' to launch NVTOP" -ForegroundColor Cyan
}

function Start-NVTOP {
    Write-Header "Launching NVTOP"
    
    Write-Host "Starting NVTOP in WSL2..." -ForegroundColor Yellow
    Write-Host "Press 'q' to quit NVTOP`n" -ForegroundColor Gray
    
    wsl nvtop
}

# Main script logic
try {
    Write-Host @"
╔════════════════════════════════════════╗
║     NVTOP WSL2 Build Script            ║
║     Windows 10/11 Builder              ║
╚════════════════════════════════════════╝
"@ -ForegroundColor Green

    if ($All) {
        $InstallWSL = $true
        $SetupDeps = $true
        $Build = $true
        $Install = $true
        $Run = $true
    }

    if ($InstallWSL) {
        Install-WSL2
    }

    if (-not (Test-WSLInstalled)) {
        Write-Error "WSL2 is not installed. Run with -InstallWSL flag to install it."
    }

    if ($SetupDeps) {
        Install-Dependencies
    }

    if ($Build) {
        Build-NVTOP
    }

    if ($Install) {
        Install-NVTOP
    }

    if ($Run) {
        Start-NVTOP
    }

    # If no flags provided, show help
    if (-not ($InstallWSL -or $SetupDeps -or $Build -or $Install -or $Run -or $All)) {
        Write-Host @"

Usage: .\build-wsl2.ps1 [OPTIONS]

Options:
    -InstallWSL     Install WSL2 and Ubuntu
    -SetupDeps      Install build dependencies in WSL2
    -Build          Build NVTOP
    -Install        Install NVTOP to system
    -Run            Run NVTOP
    -All            Do all of the above
    -Distribution   WSL distribution to use (default: Ubuntu-22.04)

Examples:
    # Full automated build and run
    .\build-wsl2.ps1 -All

    # Just build
    .\build-wsl2.ps1 -Build

    # Build and run
    .\build-wsl2.ps1 -Build -Run

    # Setup dependencies and build
    .\build-wsl2.ps1 -SetupDeps -Build -Install

"@ -ForegroundColor Yellow
    }

}
catch {
    Write-Error "An error occurred: $_"
    exit 1
}
