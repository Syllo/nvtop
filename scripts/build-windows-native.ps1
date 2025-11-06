# Native Windows Build Script for NVTOP
# Requires: Visual Studio 2019/2022 or MSYS2/MinGW-w64
# Run this in PowerShell

param(
    [ValidateSet('MSVC', 'MinGW')]
    [string]$Compiler = 'MinGW',
    
    [switch]$InstallDeps,
    [switch]$Configure,
    [switch]$Build,
    [switch]$Clean,
    [switch]$All,
    [switch]$Help
)

$ErrorActionPreference = "Stop"

function Write-Header {
    param([string]$Message)
    Write-Host "`n========================================" -ForegroundColor Cyan
    Write-Host $Message -ForegroundColor Cyan
    Write-Host "========================================`n" -ForegroundColor Cyan
}

function Show-Help {
    Write-Host @"
NVTOP Native Windows Build Script
==================================

Usage: .\build-windows-native.ps1 [OPTIONS]

Options:
    -Compiler <MSVC|MinGW>  Choose compiler (default: MinGW)
    -InstallDeps            Install build dependencies
    -Configure              Configure CMake build
    -Build                  Build the project
    -Clean                  Clean build directory
    -All                    Do all steps (Install, Configure, Build)
    -Help                   Show this help message

Examples:
    # Build with MinGW (recommended)
    .\build-windows-native.ps1 -All

    # Build with MSVC
    .\build-windows-native.ps1 -Compiler MSVC -All

    # Just configure
    .\build-windows-native.ps1 -Configure

    # Clean and rebuild
    .\build-windows-native.ps1 -Clean -Build

Prerequisites:
    MinGW: Install MSYS2 from https://www.msys2.org/
    MSVC:  Install Visual Studio 2019 or later with C++ support
    
    For NVIDIA GPU support:
    - Install NVIDIA drivers
    - CUDA Toolkit (includes nvml.dll)

"@ -ForegroundColor Yellow
}

function Install-MSYS2Dependencies {
    Write-Header "Installing MSYS2 Dependencies"
    
    $msys2Path = "C:\msys64\usr\bin\bash.exe"
    if (-not (Test-Path $msys2Path)) {
        Write-Error @"
MSYS2 not found at C:\msys64
Please install MSYS2 from: https://www.msys2.org/

After installation:
1. Run MSYS2 UCRT64
2. Update: pacman -Syu
3. Run this script again with -InstallDeps
"@
    }
    
    Write-Host "Installing packages via MSYS2..." -ForegroundColor Yellow
    
    & $msys2Path -lc @"
pacman -S --needed --noconfirm \
    mingw-w64-ucrt-x86_64-toolchain \
    mingw-w64-ucrt-x86_64-cmake \
    mingw-w64-ucrt-x86_64-ninja \
    mingw-w64-ucrt-x86_64-ncurses \
    mingw-w64-ucrt-x86_64-pdcurses \
    git
"@
    
    Write-Host "Dependencies installed!" -ForegroundColor Green
}

function Install-MSVCDependencies {
    Write-Header "MSVC Build Setup"
    
    Write-Host @"
For MSVC builds, you need:

1. Visual Studio 2019 or 2022 with "Desktop development with C++"
2. vcpkg for dependencies

Installing vcpkg and dependencies...
"@ -ForegroundColor Yellow

    if (-not (Test-Path "vcpkg")) {
        git clone https://github.com/Microsoft/vcpkg.git
        .\vcpkg\bootstrap-vcpkg.bat
    }
    
    .\vcpkg\vcpkg install pdcurses:x64-windows
    
    Write-Host "MSVC dependencies ready!" -ForegroundColor Green
}

function Configure-Build {
    param([string]$CompilerType)
    
    Write-Header "Configuring CMake Build"
    
    $buildDir = "build-windows-$CompilerType"
    
    if (-not (Test-Path $buildDir)) {
        New-Item -ItemType Directory -Path $buildDir | Out-Null
    }
    
    Set-Location $buildDir
    
    if ($CompilerType -eq 'MinGW') {
        $env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"
        
        Write-Host "Configuring with MinGW..." -ForegroundColor Yellow
        cmake .. -G "Ninja" `
            -DCMAKE_C_COMPILER=gcc `
            -DCMAKE_CXX_COMPILER=g++ `
            -DCMAKE_BUILD_TYPE=Release `
            -DNVIDIA_SUPPORT=ON `
            -DAMDGPU_SUPPORT=OFF `
            -DINTEL_SUPPORT=OFF `
            -DAPPLE_SUPPORT=OFF
    }
    else {
        Write-Host "Configuring with MSVC..." -ForegroundColor Yellow
        cmake .. -G "Visual Studio 17 2022" -A x64 `
            -DCMAKE_BUILD_TYPE=Release `
            -DCMAKE_TOOLCHAIN_FILE="vcpkg\scripts\buildsystems\vcpkg.cmake" `
            -DNVIDIA_SUPPORT=ON `
            -DAMDGPU_SUPPORT=OFF `
            -DINTEL_SUPPORT=OFF `
            -DAPPLE_SUPPORT=OFF
    }
    
    Set-Location ..
    Write-Host "Configuration complete!" -ForegroundColor Green
}

function Build-Project {
    param([string]$CompilerType)
    
    Write-Header "Building NVTOP"
    
    $buildDir = "build-windows-$CompilerType"
    
    if (-not (Test-Path $buildDir)) {
        Write-Error "Build directory not found. Run with -Configure first."
    }
    
    Set-Location $buildDir
    
    if ($CompilerType -eq 'MinGW') {
        $env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"
        ninja
    }
    else {
        cmake --build . --config Release
    }
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "`nBuild successful!" -ForegroundColor Green
        Write-Host "Binary location: $buildDir\src\nvtop.exe" -ForegroundColor Cyan
    }
    else {
        Write-Error "Build failed with exit code $LASTEXITCODE"
    }
    
    Set-Location ..
}

function Clean-Build {
    param([string]$CompilerType)
    
    Write-Header "Cleaning Build Directory"
    
    $buildDir = "build-windows-$CompilerType"
    
    if (Test-Path $buildDir) {
        Remove-Item -Recurse -Force $buildDir
        Write-Host "Build directory cleaned!" -ForegroundColor Green
    }
    else {
        Write-Host "Build directory doesn't exist, nothing to clean." -ForegroundColor Yellow
    }
}

# Main script logic
try {
    Write-Host @"
╔════════════════════════════════════════╗
║  NVTOP Native Windows Build Script     ║
║  Compiler: $Compiler                        ║
╚════════════════════════════════════════╝
"@ -ForegroundColor Green

    if ($Help) {
        Show-Help
        exit 0
    }

    if ($All) {
        $InstallDeps = $true
        $Configure = $true
        $Build = $true
    }

    if ($Clean) {
        Clean-Build -CompilerType $Compiler
    }

    if ($InstallDeps) {
        if ($Compiler -eq 'MinGW') {
            Install-MSYS2Dependencies
        }
        else {
            Install-MSVCDependencies
        }
    }

    if ($Configure) {
        Configure-Build -CompilerType $Compiler
    }

    if ($Build) {
        Build-Project -CompilerType $Compiler
    }

    if (-not ($InstallDeps -or $Configure -or $Build -or $Clean)) {
        Show-Help
    }

    Write-Host "`n✅ All operations completed!" -ForegroundColor Green
    Write-Host "`nTo run NVTOP:" -ForegroundColor Cyan
    Write-Host "  .\build-windows-$Compiler\src\nvtop.exe`n" -ForegroundColor White

}
catch {
    Write-Error "An error occurred: $_"
    exit 1
}
