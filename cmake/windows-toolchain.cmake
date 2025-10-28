# CMake Toolchain File for Windows (MinGW/MSYS2)
# This is experimental and requires significant source modifications

set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_SYSTEM_PROCESSOR x86_64)

# Compiler settings for MinGW
if(MINGW)
    set(CMAKE_C_COMPILER gcc)
    set(CMAKE_CXX_COMPILER g++)
    
    # Windows-specific definitions
    add_definitions(-D_WIN32_WINNT=0x0A00)  # Windows 10
    add_definitions(-DWIN32_LEAN_AND_MEAN)
    add_definitions(-DNOMINMAX)
    
    # Disable Linux-specific features
    set(NVIDIA_SUPPORT OFF CACHE BOOL "Disable NVIDIA on Windows" FORCE)
    set(AMDGPU_SUPPORT OFF CACHE BOOL "Disable AMD on Windows" FORCE)
    set(INTEL_SUPPORT OFF CACHE BOOL "Disable Intel on Windows" FORCE)
    set(MSM_SUPPORT OFF CACHE BOOL "Disable MSM on Windows" FORCE)
    set(APPLE_SUPPORT OFF CACHE BOOL "Disable Apple on Windows" FORCE)
    set(PANFROST_SUPPORT OFF CACHE BOOL "Disable Panfrost on Windows" FORCE)
    set(PANTHOR_SUPPORT OFF CACHE BOOL "Disable Panthor on Windows" FORCE)
    set(ASCEND_SUPPORT OFF CACHE BOOL "Disable Ascend on Windows" FORCE)
    set(V3D_SUPPORT OFF CACHE BOOL "Disable V3D on Windows" FORCE)
    set(TPU_SUPPORT OFF CACHE BOOL "Disable TPU on Windows" FORCE)
    set(ROCKCHIP_SUPPORT OFF CACHE BOOL "Disable Rockchip on Windows" FORCE)
    set(METAX_SUPPORT OFF CACHE BOOL "Disable MetaX on Windows" FORCE)
endif()

# Find Windows-compatible ncurses (PDCurses)
# Note: This requires PDCurses to be installed
# Download from: https://pdcurses.org/

message(STATUS "Windows toolchain loaded")
message(WARNING "Windows support is experimental and requires source code modifications")
