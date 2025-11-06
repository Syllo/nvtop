# AMD and Intel GPU Support Testing Guide

## Testing on Systems with AMD GPUs

If you have an AMD GPU (Radeon series), the implementation will:

1. **Detect the GPU** via DXGI enumeration (VendorId 0x1002)
2. **Display GPU name** from DXGI adapter description
3. **Monitor GPU utilization** via Windows Performance Counters
4. **Track VRAM usage** (dedicated and shared memory)
5. **Calculate memory utilization** percentage

### Expected Output Example (AMD Radeon RX 6800 XT)

```json
{
  "device_name": "AMD Radeon RX 6800 XT",
  "gpu_util": "45%",
  "mem_util": "38%",
  "mem_total": "17179869184",
  "mem_used": "6530396672",
  "mem_free": "10649472512"
}
```

### Testing Commands

```powershell
# Check if AMD GPU is detected
.\nvtop.exe --snapshot

# Interactive monitoring
.\nvtop.exe
```

### Verifying AMD GPU Detection Manually

You can verify that your AMD GPU is detectable using the DXGI test tool:

```powershell
cd <nvtop-directory>
.\test_dxgi_enum.exe
```

Look for entries with VendorId `0x1002`.

## Testing on Systems with Intel GPUs

If you have an Intel GPU (integrated or discrete Arc series), the implementation will:

1. **Detect the GPU** via DXGI enumeration (VendorId 0x8086)
2. **Display GPU name** from DXGI adapter description
3. **Monitor GPU utilization** via Windows Performance Counters
4. **Track VRAM usage** (dedicated and shared memory)
5. **Determine integrated/discrete** type based on memory configuration

### Expected Output Example (Intel Iris Xe Graphics)

```json
{
  "device_name": "Intel(R) Iris(R) Xe Graphics",
  "gpu_util": "22%",
  "mem_util": "15%",
  "mem_total": "8589934592",
  "mem_used": "1288490189",
  "mem_free": "7301444403"
}
```

### Expected Output Example (Intel Arc A770)

```json
{
  "device_name": "Intel(R) Arc(TM) A770 Graphics",
  "gpu_util": "68%",
  "mem_util": "52%",
  "mem_total": "17179869184",
  "mem_used": "8925468672",
  "mem_free": "8254400512"
}
```

### Testing Commands

```powershell
# Check if Intel GPU is detected
.\nvtop.exe --snapshot

# Interactive monitoring  
.\nvtop.exe
```

### Verifying Intel GPU Detection Manually

You can verify that your Intel GPU is detectable using the DXGI test tool:

```powershell
cd <nvtop-directory>
.\test_dxgi_enum.exe
```

Look for entries with VendorId `0x8086`.

## Multi-GPU System Testing

If your system has multiple GPUs (e.g., Intel iGPU + NVIDIA dGPU, or AMD + NVIDIA), nvtop will detect and monitor all of them:

### Expected Output Example (Intel + NVIDIA)

```json
[
  {
   "device_name": "Intel(R) UHD Graphics 630",
   "gpu_util": "5%",
   "mem_util": "2%",
   "mem_total": "4294967296",
   "mem_used": "85899345",
   "mem_free": "4209067951"
  },
  {
   "device_name": "NVIDIA GeForce RTX 3090 Ti",
   "gpu_clock": "2025MHz",
   "mem_clock": "10251MHz",
   "temp": "54C",
   "fan_speed": "30%",
   "power_draw": "175W",
   "gpu_util": "70%",
   "mem_util": "65%",
   "pcie_rx": "21923 KB/s",
   "pcie_tx": "8203 KB/s",
   "mem_total": "25757220864",
   "mem_used": "24209178624",
   "mem_free": "1548042240"
  }
]
```

## Performance Counter Verification

You can manually verify that Performance Counters are available for your GPU:

### Using Windows Performance Monitor

1. Open **Performance Monitor** (`perfmon.exe`)
2. Click the **+** button to add counters
3. Expand **GPU Engine** category
4. Look for entries like:
   - `GPU Engine(phys_0_eng_0_engtype_3D)\Utilization Percentage`
   - `GPU Engine(phys_1_eng_0_engtype_3D)\Utilization Percentage`
5. Expand **GPU Adapter Memory** category
6. Look for entries like:
   - `GPU Adapter Memory(phys_0)\Dedicated Usage`
   - `GPU Adapter Memory(phys_0)\Shared Usage`

If these counters are present, nvtop will be able to monitor your GPU.

### Using PowerShell

```powershell
# List all GPU Engine counters
Get-Counter -ListSet "GPU Engine" | Select-Object -ExpandProperty Counter

# List all GPU Adapter Memory counters
Get-Counter -ListSet "GPU Adapter Memory" | Select-Object -ExpandProperty Counter

# Sample GPU utilization (replace phys_0 with your adapter index)
Get-Counter "\GPU Engine(phys_0_*_3D)\Utilization Percentage"

# Sample GPU memory usage
Get-Counter "\GPU Adapter Memory(phys_0)\Dedicated Usage"
```

## Troubleshooting

### GPU Not Detected

**Symptom**: nvtop shows "Found 0 GPU(s)" or only shows some GPUs

**Possible Causes**:

1. **GPU disabled in BIOS/Device Manager**
   - Check Device Manager → Display adapters
   - Ensure GPU is enabled

2. **Driver not installed**
   - Install latest GPU drivers from manufacturer
   - Restart system after installation

3. **GPU not active**
   - Some systems disable iGPU when dGPU is present
   - Check BIOS settings for "Multi-Monitor Support" or "iGPU Multi-Monitor"

**Diagnostic Steps**:

```powershell
# Run DXGI enumeration test
.\test_dxgi_enum.exe

# Check if GPU appears in DXGI but not in nvtop
# This could indicate PDH counter issues
```

### Performance Counters Not Available

**Symptom**: GPU is detected but shows "0%" utilization or no data

**Possible Causes**:

1. **First collection cycle**
   - PDH counters need 2 collection cycles to provide valid data
   - Wait a few seconds and check again

2. **Insufficient permissions**
   - Run nvtop as Administrator
   - Some Performance Counters require elevated privileges

3. **Performance Counter service disabled**
   - Open Services (`services.msc`)
   - Ensure "Performance Counter DLL Host" is running

**Diagnostic Steps**:

```powershell
# Check if Performance Counters are accessible
Get-Counter "\GPU Engine(*)\Utilization Percentage"

# If error, restart Performance Counter service
Restart-Service "pla"
```

### Memory Values Incorrect

**Symptom**: Memory usage shows unusual values or doesn't change

**Possible Causes**:

1. **Shared memory not being used**
   - Integrated GPUs use both dedicated and shared memory
   - Only dedicated memory counter may show activity

2. **Counter scaling issues**
   - Some GPUs report memory in different units
   - Check if values are reasonable when compared to total VRAM

**Diagnostic Steps**:

```powershell
# Manually check memory counters
Get-Counter "\GPU Adapter Memory(phys_0)\Dedicated Usage"
Get-Counter "\GPU Adapter Memory(phys_0)\Shared Usage"

# Compare with total memory from DXGI
.\test_dxgi_enum.exe
```

## Known Limitations

### AMD GPUs

- **Temperature**: Not available (requires ADL SDK)
- **Fan Speed**: Not available (requires ADL SDK)
- **Power Draw**: Not available (requires ADL SDK)
- **Clock Speeds**: Not available via PDH (requires ADL SDK)
- **Per-Process Usage**: Not yet implemented (future enhancement)

### Intel GPUs

- **Temperature**: Not available (not typically exposed by Intel GPUs)
- **Fan Speed**: Not applicable (integrated GPUs have no dedicated fan)
- **Power Draw**: Not available (limited API access)
- **Clock Speeds**: Not available via PDH (may be accessible via Intel GPU SDK)
- **Per-Process Usage**: Not yet implemented (future enhancement)

### Both AMD and Intel

- Performance Counters require Windows 10 version 1809 or later
- Some older GPUs may not expose Performance Counters
- Discrete GPU detection works better than integrated GPU in some systems
- First data collection may show zeros (PDH needs 2 cycles for valid data)

## System Requirements

- **OS**: Windows 10 version 1809 or later, Windows 11
- **Drivers**: Latest GPU drivers installed
- **APIs**: DXGI 1.0+ (built into Windows), PDH API (built into Windows)
- **Permissions**: Standard user (some counters may require Administrator)

## Reporting Issues

If AMD or Intel GPU detection doesn't work on your system, please provide:

1. **DXGI enumeration output**:
   ```powershell
   .\test_dxgi_enum.exe > dxgi_output.txt
   ```

2. **Available Performance Counters**:
   ```powershell
   Get-Counter -ListSet "GPU Engine" | Out-File gpu_counters.txt
   Get-Counter -ListSet "GPU Adapter Memory" | Out-File gpu_memory_counters.txt
   ```

3. **nvtop snapshot output**:
   ```powershell
   .\nvtop.exe --snapshot > nvtop_snapshot.json 2>&1
   ```

4. **System information**:
   - Windows version: `winver`
   - GPU model: From Device Manager
   - Driver version: From Device Manager → GPU Properties → Driver

This information will help diagnose why GPU detection or monitoring isn't working correctly.
