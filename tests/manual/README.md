# Manual Test Programs

This directory contains standalone test programs for verifying GPU detection and functionality on Windows.

## Test Programs

### `test_dxgi_enum.c`
Tests DXGI GPU enumeration to detect all GPUs in the system.

**Purpose**: Verify that DXGI can detect NVIDIA, AMD, and Intel GPUs

**Compile**:
```bash
gcc -o test_dxgi_enum test_dxgi_enum.c -ldxgi -lole32
```

**Run**:
```bash
./test_dxgi_enum
```

**Expected Output**: List of all GPUs with Vendor ID, Device ID, and adapter description

### `test_ncurses.c`
Tests ncurses library functionality and terminal capabilities.

**Purpose**: Verify ncurses installation and color support

**Compile**:
```bash
gcc -o test_ncurses test_ncurses.c -lncursesw
```

**Run**:
```bash
./test_ncurses
```

**Expected Output**: Colored text demonstrating ncurses functionality

### `test_pcie.c`
Tests PCIe throughput monitoring via Windows Performance Counters (PDH).

**Purpose**: Verify PDH can read PCIe bandwidth counters

**Compile**:
```bash
gcc -o test_pcie test_pcie.c -lpdh
```

**Run**:
```bash
./test_pcie
```

**Expected Output**: PCIe RX/TX bandwidth for detected GPUs

## Building Tests

From the nvtop root directory:

```bash
# Build all tests
cd tests/manual
gcc -o test_dxgi_enum test_dxgi_enum.c -ldxgi -lole32
gcc -o test_ncurses test_ncurses.c -lncursesw
gcc -o test_pcie test_pcie.c -lpdh
```

## Usage Notes

- These tests are **Windows-specific** and use Windows APIs (DXGI, PDH)
- Requires MSYS2/MinGW environment for compilation
- Test executables are excluded from git via `.gitignore`
- Run these tests before building nvtop to verify all dependencies are working

## Troubleshooting

If tests fail:
1. **test_dxgi_enum fails**: Check GPU drivers are installed
2. **test_ncurses fails**: Install ncurses: `pacman -S mingw-w64-ucrt-x86_64-ncurses`
3. **test_pcie fails**: Verify GPU performance counters are enabled in Windows
