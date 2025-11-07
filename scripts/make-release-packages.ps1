#!/usr/bin/env pwsh
# Create Windows release packages for NVTOP
# Generates both ZIP and MSI packages with SHA256 hashes

param(
    [string]$Version = "3.3.0"
)

$ErrorActionPreference = "Stop"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Creating NVTOP Windows Release Packages" -ForegroundColor Cyan
Write-Host "Version: $Version" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Check if build exists
if (-not (Test-Path "build-windows\src\nvtop.exe")) {
    Write-Host "ERROR: nvtop.exe not found!" -ForegroundColor Red
    Write-Host "Please build first with: .\scripts\build-windows-native.ps1 -All" -ForegroundColor Yellow
    exit 1
}

# Clean old packages
Write-Host "Cleaning old packages..." -ForegroundColor Yellow
Remove-Item -Path "nvtop-$Version-windows-x64.zip" -ErrorAction SilentlyContinue
Remove-Item -Path "nvtop-$Version-x64.msi" -ErrorAction SilentlyContinue
Remove-Item -Path "nvtop-windows-x64" -Recurse -Force -ErrorAction SilentlyContinue

# ========================================
# Create ZIP Package
# ========================================
Write-Host ""
Write-Host "Step 1: Creating ZIP Package" -ForegroundColor Green
Write-Host "------------------------------" -ForegroundColor Green

$distDir = "nvtop-windows-x64"
New-Item -ItemType Directory -Path $distDir -Force | Out-Null

Write-Host "  Copying executable..."
Copy-Item "build-windows\src\nvtop.exe" "$distDir\"

Write-Host "  Copying required DLLs..."
$dllsToCopy = @(
    "libncursesw6.dll",
    "libgcc_s_seh-1.dll",
    "libwinpthread-1.dll"
)

foreach ($dll in $dllsToCopy) {
    $dllPath = "C:\msys64\ucrt64\bin\$dll"
    if (Test-Path $dllPath) {
        Copy-Item $dllPath "$distDir\"
        Write-Host "    - $dll" -ForegroundColor Gray
    } else {
        Write-Host "    - $dll NOT FOUND!" -ForegroundColor Red
    }
}

Write-Host "  Copying documentation..."
Copy-Item "README.md" "$distDir\README.md"
Copy-Item "LICENSE" "$distDir\LICENSE.txt"

Write-Host "  Creating Windows README..."
$readmeContent = @"
NVTOP for Windows x64
=====================

Version: $Version (Windows Native Build)
Build Date: $(Get-Date -Format "yyyy-MM-dd")

REQUIREMENTS
------------
- Windows 10 (1809+) or Windows 11
- NVIDIA, AMD, or Intel GPU with recent drivers
- For NVIDIA: CUDA Toolkit or nvml.dll in same directory

USAGE
-----
Run: nvtop.exe

Command line options:
  -h, --help          : Show help
  -v, --version       : Show version
  -d, --delay N       : Set refresh rate (N * 0.1 seconds)
  -s, --snapshot      : JSON snapshot mode (for scripting)
  -C, --no-color      : Disable colors
  --color N           : Force color mode (0-256)

INTERACTIVE CONTROLS
--------------------
  q            : Quit
  F2           : Setup/Options
  F12          : Save preferences
  Arrow keys   : Navigate
  +/-          : Adjust refresh rate

POWERSHELL SCRIPTING
--------------------
# Get GPU info as JSON object
`$gpu = .\nvtop.exe --snapshot | ConvertFrom-Json
Write-Host "GPU: `$(`$gpu.device_name)"
Write-Host "Utilization: `$(`$gpu.gpu_util)"
Write-Host "Memory Used: `$(`$gpu.mem_used) / `$(`$gpu.mem_total)"

TROUBLESHOOTING
---------------
1. GPU not detected:
   - Install latest GPU drivers
   - For NVIDIA: Ensure CUDA Toolkit is installed or nvml.dll is present
   - Run nvidia-smi, intel_gpu_top, or GPU-Z to verify GPU is working

2. Display issues:
   - Use Windows Terminal for best results
   - Avoid legacy cmd.exe
   - Set: `$env:TERM='xterm-256color'

3. Permission errors:
   - Run as Administrator for full process information

SUPPORTED GPUS
--------------
- NVIDIA: Full support via NVML (all metrics)
- AMD: Utilization, memory, clocks via DXGI/PDH
- Intel: Utilization, memory, clocks via DXGI/PDH

For more information:
- GitHub: https://github.com/nervosys/nvtop
- Documentation: See docs/ folder in repository

License: GPLv3
"@

Set-Content -Path "$distDir\README-WINDOWS.txt" -Value $readmeContent

Write-Host "  Creating ZIP archive..."
$zipPath = "nvtop-$Version-windows-x64.zip"
Compress-Archive -Path $distDir -DestinationPath $zipPath -Force
Write-Host "    Created: $zipPath" -ForegroundColor Green

Write-Host "  Calculating ZIP SHA256..."
$zipHash = (Get-FileHash $zipPath -Algorithm SHA256).Hash
Write-Host "    SHA256: $zipHash" -ForegroundColor Cyan

$zipSize = [math]::Round((Get-Item $zipPath).Length / 1MB, 2)
Write-Host "    Size: $zipSize MB" -ForegroundColor Gray

# ========================================
# Create MSI Package
# ========================================
Write-Host ""
Write-Host "Step 2: Creating MSI Package" -ForegroundColor Green
Write-Host "------------------------------" -ForegroundColor Green

$msiScript = ".\installer\build-msi.ps1"
if (Test-Path $msiScript) {
    Write-Host "  Running MSI build script..."
    & $msiScript
    
    $msiPath = "nvtop-$Version-x64.msi"
    if (Test-Path $msiPath) {
        Write-Host "    Created: $msiPath" -ForegroundColor Green
        
        Write-Host "  Calculating MSI SHA256..."
        $msiHash = (Get-FileHash $msiPath -Algorithm SHA256).Hash
        Write-Host "    SHA256: $msiHash" -ForegroundColor Cyan
        
        $msiSize = [math]::Round((Get-Item $msiPath).Length / 1MB, 2)
        Write-Host "    Size: $msiSize MB" -ForegroundColor Gray
    } else {
        Write-Host "    ERROR: MSI was not created!" -ForegroundColor Red
    }
} else {
    Write-Host "  ERROR: MSI build script not found at $msiScript" -ForegroundColor Red
}

# ========================================
# Update WinGet Manifest
# ========================================
Write-Host ""
Write-Host "Step 3: Updating WinGet Manifest" -ForegroundColor Green
Write-Host "----------------------------------" -ForegroundColor Green

$installerManifest = "manifests\n\Nervosys\Nvtop\$Version\Nervosys.Nvtop.installer.yaml"
if (Test-Path $installerManifest) {
    Write-Host "  Reading installer manifest..."
    $content = Get-Content $installerManifest -Raw
    
    # Update SHA256 hash
    if ($content -match 'InstallerSha256:\s*[A-F0-9]{64}') {
        $content = $content -replace 'InstallerSha256:\s*[A-F0-9]{64}', "InstallerSha256: $msiHash"
        Set-Content -Path $installerManifest -Value $content -NoNewline
        Write-Host "    Updated InstallerSha256" -ForegroundColor Green
    } else {
        Write-Host "    WARNING: Could not find InstallerSha256 field" -ForegroundColor Yellow
    }
} else {
    Write-Host "  ERROR: Installer manifest not found at $installerManifest" -ForegroundColor Red
}

# ========================================
# Summary
# ========================================
Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Release Packages Created Successfully!" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "ZIP Package:" -ForegroundColor White
Write-Host "  File: $zipPath" -ForegroundColor Gray
Write-Host "  Size: $zipSize MB" -ForegroundColor Gray
Write-Host "  SHA256: $zipHash" -ForegroundColor Gray
Write-Host ""

if (Test-Path "nvtop-$Version-x64.msi") {
    Write-Host "MSI Package:" -ForegroundColor White
    Write-Host "  File: nvtop-$Version-x64.msi" -ForegroundColor Gray
    Write-Host "  Size: $msiSize MB" -ForegroundColor Gray
    Write-Host "  SHA256: $msiHash" -ForegroundColor Gray
    Write-Host ""
}

Write-Host "Next Steps:" -ForegroundColor Yellow
Write-Host "  1. Test the packages" -ForegroundColor White
Write-Host "  2. Create GitHub release at: https://github.com/nervosys/nvtop/releases/new" -ForegroundColor White
Write-Host "  3. Upload: nvtop-$Version-x64.msi (recommended)" -ForegroundColor White
Write-Host "  4. Upload: $zipPath (alternative)" -ForegroundColor White
Write-Host "  5. Update MSI URL in manifests if needed" -ForegroundColor White
Write-Host "  6. Submit WinGet PR to microsoft/winget-pkgs" -ForegroundColor White
Write-Host ""
