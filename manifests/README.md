# Windows Package Manager (winget) Manifest

This directory contains the winget package manifest files for NVTOP.

## Manifest Files

The manifest consists of three required files following the [Windows Package Manager manifest schema v1.6.0](https://learn.microsoft.com/en-us/windows/package-manager/package/manifest):

1. **Nervosys.Nvtop.yaml** - Version manifest (root file)
2. **Nervosys.Nvtop.installer.yaml** - Installer configuration
3. **Nervosys.Nvtop.locale.en-US.yaml** - Package metadata and descriptions

## Directory Structure

```
manifests/
└── n/
    └── Nervosys/
        └── Nvtop/
            └── 3.3.0/
                ├── Nervosys.Nvtop.yaml
                ├── Nervosys.Nvtop.installer.yaml
                └── Nervosys.Nvtop.locale.en-US.yaml
```

## Before Submitting to winget-pkgs

### 1. Create a GitHub Release

Before submitting to the official winget-pkgs repository, you need to:

1. Build the release binary:
   ```powershell
   .\scripts\build-windows-native.ps1 -All -Release
   ```

2. Create a release package:
   ```powershell
   # Create release directory
   mkdir release
   
   # Copy binary and required DLLs
   copy build-windows\src\nvtop.exe release\
   copy C:\msys64\ucrt64\bin\libncursesw6.dll release\
   copy C:\msys64\ucrt64\bin\libgcc_s_seh-1.dll release\
   copy C:\msys64\ucrt64\bin\libwinpthread-1.dll release\
   
   # Create zip archive
   Compress-Archive -Path release\* -DestinationPath nvtop-3.3.0-windows-x64.zip
   ```

3. Create a GitHub release at: https://github.com/nervosys/nvtop/releases/new
   - Tag: `v3.3.0`
   - Title: `NVTOP 3.3.0 - Windows Native Support`
   - Upload: `nvtop-3.3.0-windows-x64.zip`

4. Calculate SHA256 hash:
   ```powershell
   Get-FileHash nvtop-3.3.0-windows-x64.zip -Algorithm SHA256
   ```

5. Update `Nervosys.Nvtop.installer.yaml`:
   - Replace `<SHA256>` with the actual hash
   - Verify the `InstallerUrl` matches your release URL

### 2. Validate the Manifest

Install winget-create:
```powershell
winget install Microsoft.WingetCreate
```

Validate the manifest:
```powershell
cd manifests\n\Nervosys\Nvtop\3.3.0
winget validate --manifest .
```

### 3. Test Local Installation

**Important**: Local manifest installation requires enabling the feature first:
```powershell
# Run as Administrator
winget settings --enable LocalManifestFiles
```

Test the manifest locally before submitting:
```powershell
# Point to the directory containing the manifest files, not a specific .yaml file
winget install --manifest .\manifests\n\Nervosys\Nvtop\3.3.0\
```

**Note**: If you get "This feature needs to be enabled by administrators", you must run the enable command above in an elevated PowerShell window.

### 4. Submit to winget-pkgs Repository

1. Fork the [microsoft/winget-pkgs](https://github.com/microsoft/winget-pkgs) repository

2. Clone your fork:
   ```powershell
   git clone https://github.com/YOUR-USERNAME/winget-pkgs.git
   cd winget-pkgs
   ```

3. Copy manifest files:
   ```powershell
   # Create the directory structure
   mkdir -p manifests\n\Nervosys\Nvtop\3.3.0
   
   # Copy the manifest files
   copy C:\path\to\nvtop\manifests\n\Nervosys\Nvtop\3.3.0\* manifests\n\Nervosys\Nvtop\3.3.0\
   ```

4. Create a branch and commit:
   ```powershell
   git checkout -b nervosys-nvtop-3.3.0
   git add manifests\n\Nervosys\Nvtop\3.3.0\
   git commit -m "Add Nervosys.Nvtop version 3.3.0"
   git push origin nervosys-nvtop-3.3.0
   ```

5. Create a Pull Request to microsoft/winget-pkgs:
   - Title: `Add Nervosys.Nvtop version 3.3.0`
   - Description: Include information about the package and testing performed

## Installation After winget-pkgs Approval

Once merged into winget-pkgs, users can install NVTOP with:

```powershell
winget install Nervosys.Nvtop
```

Or search for it:
```powershell
winget search nvtop
```

## Updating the Manifest for New Versions

For future releases, use winget-create to update:

```powershell
winget-create update Nervosys.Nvtop --version 3.4.0 --urls https://github.com/nervosys/nvtop/releases/download/v3.4.0/nvtop-3.4.0-windows-x64.zip
```

## References

- [Windows Package Manager Documentation](https://learn.microsoft.com/en-us/windows/package-manager/)
- [Manifest Schema v1.6.0](https://learn.microsoft.com/en-us/windows/package-manager/package/manifest?tabs=minschema%2Cversion-example)
- [winget-pkgs Repository](https://github.com/microsoft/winget-pkgs)
- [winget-create Tool](https://github.com/microsoft/winget-create)
- [Contributing Guidelines](https://github.com/microsoft/winget-pkgs/blob/master/CONTRIBUTING.md)

## Notes

- The manifest uses **portable** installer type since NVTOP is a standalone executable
- SHA256 hash must be updated when the release binary is created
- The installer URL must point to a publicly accessible GitHub release
- winget-pkgs requires all URLs to use HTTPS
- Package validation is automatic when you submit a PR to winget-pkgs
