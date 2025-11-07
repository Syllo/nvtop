<#
.SYNOPSIS
    Build MSI installer for NVTOP using WiX Toolset
.DESCRIPTION
    This script builds the NVTOP MSI installer using WiX Toolset 3.x
    Requires WiX Toolset to be installed and in PATH
.PARAMETER Clean
    Clean build artifacts before building
#>

param(
    [switch]$Clean
)

$ErrorActionPreference = "Stop"

# Script directory
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptDir
$InstallerDir = $ScriptDir
$BuildDir = Join-Path $ProjectRoot "build-windows-MinGW"

Write-Host "╔════════════════════════════════════════╗" -ForegroundColor Cyan
Write-Host "║  NVTOP MSI Installer Build Script     ║" -ForegroundColor Cyan
Write-Host "╚════════════════════════════════════════╝" -ForegroundColor Cyan
Write-Host ""

# Check for WiX Toolset
$wixPath = $null
$wixVersion = $null

# Check for WiX 6.x (new unified CLI)
$wixExe = Get-Command "wix.exe" -ErrorAction SilentlyContinue
if ($wixExe) {
    $wixPath = Split-Path -Parent $wixExe.Source
    $wixVersion = 6
    Write-Host "✓ Found WiX Toolset 6.x: $wixPath" -ForegroundColor Green
} else {
    # Check for WiX 3.x (legacy)
    $possiblePaths = @(
        "$env:WIX\bin",
        "${env:ProgramFiles(x86)}\WiX Toolset v3.14\bin",
        "${env:ProgramFiles(x86)}\WiX Toolset v3.11\bin",
        "${env:ProgramFiles}\WiX Toolset v3.14\bin",
        "${env:ProgramFiles}\WiX Toolset v3.11\bin"
    )

    foreach ($path in $possiblePaths) {
        if (Test-Path "$path\candle.exe") {
            $wixPath = $path
            $wixVersion = 3
            Write-Host "✓ Found WiX Toolset 3.x: $wixPath" -ForegroundColor Green
            break
        }
    }
}

if (-not $wixPath) {
    Write-Host "❌ WiX Toolset not found!" -ForegroundColor Red
    Write-Host ""
    Write-Host "Please install WiX Toolset:" -ForegroundColor Yellow
    Write-Host "  winget install WiXToolset.WiXToolset" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Or download from: https://wixtoolset.org/releases/" -ForegroundColor Yellow
    exit 1
}

# Check if build exists
if (-not (Test-Path "$BuildDir\src\nvtop.exe")) {
    Write-Host "❌ Build not found at: $BuildDir\src\nvtop.exe" -ForegroundColor Red
    Write-Host ""
    Write-Host "Please build NVTOP first:" -ForegroundColor Yellow
    Write-Host "  .\scripts\build-windows-native.ps1 -All -Release" -ForegroundColor Cyan
    exit 1
}

Write-Host "✓ Found NVTOP build" -ForegroundColor Green

# Clean if requested
if ($Clean) {
    Write-Host ""
    Write-Host "Cleaning installer build artifacts..." -ForegroundColor Yellow
    Remove-Item "$InstallerDir\*.wixobj" -ErrorAction SilentlyContinue
    Remove-Item "$InstallerDir\*.wixpdb" -ErrorAction SilentlyContinue
    Remove-Item "$InstallerDir\*.msi" -ErrorAction SilentlyContinue
    Write-Host "✓ Cleaned" -ForegroundColor Green
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Building MSI Installer" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

if ($wixVersion -eq 6) {
    # WiX 6.x unified build command
    Write-Host "Building with WiX 6.x..." -ForegroundColor Yellow
    
    # Get absolute path to project root
    $ProjectRoot = (Get-Item $InstallerDir).Parent.FullName
    
    $wixArgs = @(
        "build",
        "-nologo",
        "-ext", "WixToolset.UI.wixext",
        "-d", "ProjectRoot=$ProjectRoot",
        "-out", "$InstallerDir\nvtop-3.3.0-x64.msi",
        "$InstallerDir\nvtop.wxs"
    )
    
    & "wix.exe" @wixArgs
    if ($LASTEXITCODE -ne 0) {
        Write-Host "❌ wix.exe failed with exit code $LASTEXITCODE" -ForegroundColor Red
        exit $LASTEXITCODE
    }
    Write-Host "✓ MSI created successfully" -ForegroundColor Green
} else {
    # WiX 3.x two-step build (candle + light)
    Write-Host "Step 1: Compiling WiX source..." -ForegroundColor Yellow
    $candleArgs = @(
        "-nologo",
        "-ext", "WixUIExtension",
        "-out", "$InstallerDir\nvtop.wixobj",
        "$InstallerDir\nvtop.wxs"
    )

    & "$wixPath\candle.exe" @candleArgs
    if ($LASTEXITCODE -ne 0) {
        Write-Host "❌ candle.exe failed with exit code $LASTEXITCODE" -ForegroundColor Red
        exit $LASTEXITCODE
    }
    Write-Host "✓ Compilation successful" -ForegroundColor Green
    Write-Host ""

    Write-Host "Step 2: Linking MSI..." -ForegroundColor Yellow
    $lightArgs = @(
        "-nologo",
        "-ext", "WixUIExtension",
        "-out", "$InstallerDir\nvtop-3.3.0-x64.msi",
        "$InstallerDir\nvtop.wixobj"
    )

    & "$wixPath\light.exe" @lightArgs
    if ($LASTEXITCODE -ne 0) {
        Write-Host "❌ light.exe failed with exit code $LASTEXITCODE" -ForegroundColor Red
        exit $LASTEXITCODE
    }
    Write-Host "✓ MSI created successfully" -ForegroundColor Green
}

Write-Host ""

# Calculate hash
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "MSI Information" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

$msiPath = "$InstallerDir\nvtop-3.3.0-x64.msi"
$msiInfo = Get-Item $msiPath
$hash = Get-FileHash $msiPath -Algorithm SHA256

Write-Host "Location: $msiPath" -ForegroundColor White
Write-Host "Size:     $([math]::Round($msiInfo.Length / 1MB, 2)) MB" -ForegroundColor White
Write-Host "SHA256:   $($hash.Hash)" -ForegroundColor White
Write-Host ""

Write-Host "✅ MSI installer build complete!" -ForegroundColor Green
Write-Host ""
Write-Host "To test the installer:" -ForegroundColor Yellow
Write-Host "  msiexec /i `"$msiPath`"" -ForegroundColor Cyan
Write-Host ""
Write-Host "To install silently:" -ForegroundColor Yellow  
Write-Host "  msiexec /i `"$msiPath`" /qn" -ForegroundColor Cyan
