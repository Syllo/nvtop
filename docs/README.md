# NVTOP Documentation

This directory contains comprehensive documentation for the NVTOP Windows port.

## Quick Start

- **[QUICKSTART_WINDOWS.md](QUICKSTART_WINDOWS.md)** - Fast-track guide to get nvtop running on Windows
- **[README_WINDOWS.md](README_WINDOWS.md)** - Main Windows-specific documentation

## Build Guides

### Windows Build Options
- **[WINDOWS_BUILD.md](WINDOWS_BUILD.md)** - WSL2 build method (recommended for beginners)
- **[WINDOWS_NATIVE_BUILD.md](WINDOWS_NATIVE_BUILD.md)** - Native MinGW/MSYS2 build method
- **[BUILD_STATUS.md](BUILD_STATUS.md)** - Current build status and known issues

## Feature Documentation

### GPU Vendor Support
- **[WINDOWS_AMD_INTEL_SUPPORT.md](WINDOWS_AMD_INTEL_SUPPORT.md)** - AMD and Intel GPU support overview
- **[AMD_INTEL_TESTING_GUIDE.md](AMD_INTEL_TESTING_GUIDE.md)** - Testing guide for AMD and Intel GPUs
- **[PARITY_IMPROVEMENTS_SUMMARY.md](PARITY_IMPROVEMENTS_SUMMARY.md)** - Feature parity improvements (v3.3.0)

### Advanced Features
- **[MULTI_GPU_SUPPORT_IMPLEMENTATION.md](MULTI_GPU_SUPPORT_IMPLEMENTATION.md)** - Multi-GPU system support and implementation details

## Technical Documentation

- **[WINDOWS_FILES_SUMMARY.md](WINDOWS_FILES_SUMMARY.md)** - Overview of Windows-specific files and their purposes
- **[SECURITY_COMPLIANCE_AUDIT.md](SECURITY_COMPLIANCE_AUDIT.md)** - Security audit and compliance information

## Documentation by Topic

### For Users
1. Start with [QUICKSTART_WINDOWS.md](QUICKSTART_WINDOWS.md)
2. Read [README_WINDOWS.md](README_WINDOWS.md) for detailed usage
3. Check [BUILD_STATUS.md](BUILD_STATUS.md) for known limitations

### For Developers
1. Review [WINDOWS_NATIVE_BUILD.md](WINDOWS_NATIVE_BUILD.md) for build setup
2. Read [WINDOWS_AMD_INTEL_SUPPORT.md](WINDOWS_AMD_INTEL_SUPPORT.md) for GPU implementation details
3. Check [MULTI_GPU_SUPPORT_IMPLEMENTATION.md](MULTI_GPU_SUPPORT_IMPLEMENTATION.md) for architecture
4. See [PARITY_IMPROVEMENTS_SUMMARY.md](PARITY_IMPROVEMENTS_SUMMARY.md) for recent enhancements

### For Testers
1. Use [AMD_INTEL_TESTING_GUIDE.md](AMD_INTEL_TESTING_GUIDE.md) to test GPU support
2. Review [BUILD_STATUS.md](BUILD_STATUS.md) for testing focus areas

## Version History

- **v3.3.0** (Current) - Added AMD/Intel GPU support, color-coded bars, per-process monitoring
- **v3.2.0** - Initial Windows port with NVIDIA support

## Contributing

See the main [README.md](../README.md) in the project root for contribution guidelines.

## Additional Resources

- **Scripts**: See [scripts/README.md](../scripts/README.md) for build and utility scripts
- **Tests**: See [tests/manual/README.md](../tests/manual/README.md) for manual test programs
- **Source Code**: See [.github/copilot-instructions.md](../.github/copilot-instructions.md) for developer guide
