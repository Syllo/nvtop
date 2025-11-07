#!/usr/bin/env pwsh
# WiX 6.x COM Error Workaround Script
# This script attempts multiple methods to build the MSI

param()

$ErrorActionPreference = "Continue"
$ProjectRoot = $PSScriptRoot

Write-Host "WiX 6.x COM Error Workaround" -ForegroundColor Cyan
Write-Host "================================`n" -ForegroundColor Cyan

# Method 1: Reset COM+ catalog
Write-Host "[Method 1] Resetting COM+ Application..." -ForegroundColor Yellow
try {
    $comAdmin = New-Object -ComObject COMAdmin.COMAdminCatalog
    $comAdmin.RefreshComponents()
    Write-Host "  ✓ COM+ refreshed" -ForegroundColor Green
}
catch {
    Write-Host "  ✗ COM+ reset failed: $($_.Exception.Message)" -ForegroundColor Red
}

# Method 2: Ensure MSI service is properly initialized
Write-Host "`n[Method 2] Reinitializing Windows Installer..." -ForegroundColor Yellow
try {
    Stop-Service msiserver -Force -ErrorAction SilentlyContinue
    Start-Sleep -Seconds 2
    Start-Service msiserver
    Write-Host "  ✓ MSI service restarted" -ForegroundColor Green
}
catch {
    Write-Host "  ✗ Service restart failed: $($_.Exception.Message)" -ForegroundColor Red
}

# Method 3: Build with temporary isolated environment
Write-Host "`n[Method 3] Building with isolated environment..." -ForegroundColor Yellow
try {
    $tempBuildDir = Join-Path $env:TEMP "nvtop-msi-build-$(Get-Date -Format 'yyyyMMddHHmmss')"
    New-Item -ItemType Directory -Path $tempBuildDir -Force | Out-Null
    
    $wixArgs = @(
        "build"
        "-nologo"
        "-ext", "WixToolset.UI.wixext"
        "-d", "ProjectRoot=$ProjectRoot"
        "-intermediatefolder", $tempBuildDir
        "-out", "$ProjectRoot\nvtop-3.3.0-x64.msi"
        "$ProjectRoot\installer\nvtop.wxs"
    )
    
    Write-Host "  Running: wix.exe $($wixArgs -join ' ')" -ForegroundColor Gray
    $result = & "wix.exe" @wixArgs 2>&1
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "  ✓ Build successful!" -ForegroundColor Green
        Remove-Item $tempBuildDir -Recurse -Force -ErrorAction SilentlyContinue
        
        if (Test-Path "$ProjectRoot\nvtop-3.3.0-x64.msi") {
            $hash = Get-FileHash "$ProjectRoot\nvtop-3.3.0-x64.msi" -Algorithm SHA256
            $size = [math]::Round((Get-Item "$ProjectRoot\nvtop-3.3.0-x64.msi").Length / 1MB, 2)
            
            Write-Host "`n========================================" -ForegroundColor Cyan
            Write-Host "MSI Package Created Successfully!" -ForegroundColor Green
            Write-Host "========================================" -ForegroundColor Cyan
            Write-Host "File: nvtop-3.3.0-x64.msi" -ForegroundColor White
            Write-Host "Size: $size MB" -ForegroundColor White
            Write-Host "SHA256: $($hash.Hash)" -ForegroundColor Cyan
            exit 0
        }
    }
    else {
        Write-Host "  ✗ Build failed with exit code: $LASTEXITCODE" -ForegroundColor Red
        $result | Where-Object { $_ -match "error|exception" } | ForEach-Object { Write-Host "    $_" -ForegroundColor Red }
    }
}
catch {
    Write-Host "  ✗ Build exception: $($_.Exception.Message)" -ForegroundColor Red
}

# Method 4: Try running WiX as a different user (requires manual intervention)
Write-Host "`n[Method 4] Alternative: Build as SYSTEM user..." -ForegroundColor Yellow
Write-Host "  This requires PsExec from Sysinternals" -ForegroundColor Gray
Write-Host "  Download from: https://live.sysinternals.com/PsExec64.exe" -ForegroundColor Gray
Write-Host "  Then run: PsExec64.exe -s -i powershell.exe" -ForegroundColor Gray

Write-Host "`n========================================" -ForegroundColor Red
Write-Host "All automatic methods failed" -ForegroundColor Red
Write-Host "========================================" -ForegroundColor Red
Write-Host "`nThis appears to be a known WiX 6.x issue on your system." -ForegroundColor Yellow
Write-Host "Possible solutions:" -ForegroundColor Yellow
Write-Host "  1. Try building on a different Windows machine" -ForegroundColor White
Write-Host "  2. Use Windows Sandbox or a clean VM" -ForegroundColor White
Write-Host "  3. Downgrade to WiX 3.x: winget install WiXToolset.Wix3" -ForegroundColor White
Write-Host "  4. Use the ZIP package for manual distribution" -ForegroundColor White

exit 1
