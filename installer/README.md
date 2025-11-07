# NVTOP MSI Installer

This directory contains the WiX Toolset source files for creating an MSI installer for NVTOP.

## Prerequisites

### Install WiX Toolset 3.x

**Option 1: Using winget (Requires Administrator)**
```powershell
winget install WiXToolset.WiXToolset
```

**Option 2: Manual Download**
Download from: https://wixtoolset.org/releases/

### Build NVTOP First

Before creating the MSI, you must build NVTOP:
```powershell
cd ..
.\scripts\build-windows-native.ps1 -All -Release
```

## Building the MSI

### Quick Build

```powershell
.\build-msi.ps1
```

### Clean Build

```powershell
.\build-msi.ps1 -Clean
```

## Output

The build script creates:
- **nvtop-3.3.0-x64.msi** - The MSI installer package

## Installation

### GUI Installation

Double-click the MSI file or run:
```powershell
msiexec /i nvtop-3.3.0-x64.msi
```

### Silent Installation

```powershell
msiexec /i nvtop-3.3.0-x64.msi /qn
```

### Custom Installation Directory

```powershell
msiexec /i nvtop-3.3.0-x64.msi INSTALLFOLDER="C:\CustomPath\NVTOP"
```

## Uninstallation

### GUI Uninstall

Use Windows "Add or Remove Programs"

### Silent Uninstall

```powershell
msiexec /x nvtop-3.3.0-x64.msi /qn
```

## What Gets Installed

The MSI installer includes:

1. **Main Executable**: `nvtop.exe`
2. **Required DLLs**:
   - `libncursesw6.dll`
   - `libwinpthread-1.dll`
   - `libgcc_s_seh-1.dll`
3. **Documentation**:
   - `COPYING` (GPL-3.0 License)
   - `README.md`
4. **Start Menu Shortcut**: NVTOP
5. **PATH Environment Variable**: Automatically adds installation directory to system PATH

## Default Installation Location

```
C:\Program Files\NVTOP\
```

## WiX Source Files

- **nvtop.wxs** - Main WiX source file defining the installer
- **License.rtf** - GPL-3.0 license in RTF format for installer UI
- **build-msi.ps1** - PowerShell build script

## Updating the Installer

### Change Version

Edit `nvtop.wxs`:
```xml
<Product Id="*" 
         Name="NVTOP" 
         Version="3.4.0.0"  <!-- Update this -->
         ...>
```

### Add New Files

Add new `<Component>` and `<File>` elements in `nvtop.wxs`:
```xml
<Component Id="NewFile" Guid="GENERATE-NEW-GUID">
  <File Id="newfile.ext" Source="..\path\to\newfile.ext" KeyPath="yes" />
</Component>
```

Then add the component reference to the feature:
```xml
<Feature Id="ProductFeature" ...>
  ...
  <ComponentRef Id="NewFile" />
</Feature>
```

**Important**: Always generate new GUIDs for new components using:
```powershell
[guid]::NewGuid()
```

## WiX Toolset Documentation

- Official Docs: https://wixtoolset.org/documentation/
- Tutorial: https://www.firegiant.com/wix/tutorial/
- Schema Reference: http://wixtoolset.org/documentation/manual/v3/xsd/

## Troubleshooting

### "WiX Toolset not found"

Make sure WiX is installed and either:
- `%WIX%` environment variable is set, OR
- WiX is installed in Program Files

### "Build not found"

Run the build script first:
```powershell
cd ..
.\scripts\build-windows-native.ps1 -All -Release
```

### Light.exe warnings about ICE validation

These are usually informational. Common ones:
- **ICE61**: File not versioned - Expected for DLLs without version info
- **ICE69**: Mismatched components - Check GUIDs are unique

To suppress specific warnings:
```powershell
light.exe -sw<number> ...
```

For example, to suppress ICE61:
```powershell
light.exe -sw61 ...
```

## MSI Properties

The installer supports standard MSI properties:

```powershell
# Custom install location
msiexec /i nvtop-3.3.0-x64.msi INSTALLFOLDER="D:\Tools\NVTOP"

# Install for current user only (if supported)
msiexec /i nvtop-3.3.0-x64.msi ALLUSERS=2 MSIINSTALLPERUSER=1

# Enable logging
msiexec /i nvtop-3.3.0-x64.msi /l*v install.log
```

## Updating winget Manifest

After building the MSI, update the winget manifest:

1. Calculate SHA256:
   ```powershell
   Get-FileHash nvtop-3.3.0-x64.msi -Algorithm SHA256
   ```

2. Update `..\manifests\n\Nervosys\Nvtop\3.3.0\Nervosys.Nvtop.installer.yaml`:
   ```yaml
   InstallerType: msi
   Installers:
   - Architecture: x64
     InstallerUrl: https://github.com/nervosys/nvtop/releases/download/v3.3.0/nvtop-3.3.0-x64.msi
     InstallerSha256: <HASH-FROM-STEP-1>
     ProductCode: '{GUID-FROM-MSI}'
   ```

3. Validate:
   ```powershell
   winget validate --manifest ..\manifests\n\Nervosys\Nvtop\3.3.0\
   ```
