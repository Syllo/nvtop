include(CheckCSourceCompiles)

configure_file(
  "${PROJECT_SOURCE_DIR}/include/nvtop/version.h.in"
  "${PROJECT_BINARY_DIR}/include/nvtop/version.h"
  IMMEDIATE @ONLY)

add_executable(nvtop
  nvtop.c
  interface.c
  interface_layout_selection.c
  interface_options.c
  interface_setup_win.c
  interface_ring_buffer.c
  extract_gpuinfo.c
  time.c
  plot.c
  ini.c
)

check_c_source_compiles(
  "
  #include <stdlib.h>
  int main() {
    int *buf = NULL;
    buf = reallocarray(buf, 15, sizeof(*buf));
    return EXIT_SUCCESS;
  }
  "
  HAS_REALLOCARRAY
)

if(HAS_REALLOCARRAY)
  target_compile_definitions(nvtop PRIVATE HAS_REALLOCARRAY)
endif()

find_package(UDev)
find_package(Systemd)
option(USE_LIBUDEV_OVER_LIBSYSTEMD "Use libudev, even if libsystemd is present" OFF)

if(UNIX AND NOT APPLE)
  target_sources(nvtop PRIVATE
    get_process_info_linux.c
    extract_processinfo_fdinfo.c
    info_messages_linux.c)
elseif(APPLE)
  target_sources(nvtop PRIVATE
    get_process_info_mac.c
    extract_processinfo_mac.c
    info_messages_mac.c)
endif()

if(NVIDIA_SUPPORT)
  target_sources(nvtop PRIVATE extract_gpuinfo_nvidia.c)
endif()

if(ASCEND_SUPPORT)
  target_sources(nvtop PRIVATE extract_gpuinfo_ascend.c)
  set(DCMI_LIBRARY_PATH /usr/local/Ascend/driver/lib64/driver)
  target_link_libraries(nvtop PRIVATE "${DCMI_LIBRARY_PATH}/libdcmi.so")
endif()

if(AMDGPU_SUPPORT OR INTEL_SUPPORT OR V3D_SUPPORT)
  if((SYSTEMD_FOUND AND UDEV_FOUND AND USE_LIBUDEV_OVER_LIBSYSTEMD) OR(NOT SYSTEMD_FOUND AND UDEV_FOUND))
    target_compile_definitions(nvtop PRIVATE USING_LIBUDEV)
    target_link_libraries(nvtop PRIVATE udev)
  elseif(SYSTEMD_FOUND)
    target_compile_definitions(nvtop PRIVATE USING_LIBSYSTEMD)
    target_link_libraries(nvtop PRIVATE systemd)
  else()
      message(FATAL_ERROR "Neither libsystemd nor libudev were found; These are required for AMDGPU, INTEL and V3D support")
  endif()

  target_sources(nvtop PRIVATE device_discovery_linux.c)
endif()

if(AMDGPU_SUPPORT OR MSM_SUPPORT)
  # Search for libdrm for AMDGPU support
  find_package(Libdrm)

  if(Libdrm_FOUND)
    message(STATUS "Found libdrm; Enabling support")
    target_include_directories(nvtop PRIVATE ${Libdrm_INCLUDE_DIRS})
    if (AMDGPU_SUPPORT)
      target_sources(nvtop PRIVATE extract_gpuinfo_amdgpu.c)
      target_sources(nvtop PRIVATE extract_gpuinfo_amdgpu_utils.c)
    endif()

    if (MSM_SUPPORT)
      target_sources(nvtop PRIVATE extract_gpuinfo_msm.c)
      target_sources(nvtop PRIVATE extract_gpuinfo_msm_utils.c)
    endif()

  else()
    message(FATAL_ERROR "libdrm not found; This library is required for AMDGPU and MSM support")
  endif()
endif()

if(INTEL_SUPPORT)
  target_sources(nvtop PRIVATE extract_gpuinfo_intel.c)
endif()

if(V3D_SUPPORT)
  target_sources(nvtop PRIVATE extract_gpuinfo_v3d.c)
  target_sources(nvtop PRIVATE extract_gpuinfo_v3d_utils.c)
endif()

if(APPLE_SUPPORT)
  target_sources(nvtop PRIVATE extract_gpuinfo_apple.m)
  target_link_libraries(nvtop PRIVATE
    "-framework Metal"
    "-framework AppKit"
    "-framework Foundation"
    "-framework QuartzCore"
    "-framework IOKit")
endif()

if (PANFROST_SUPPORT)
  target_sources(nvtop PRIVATE extract_gpuinfo_panfrost.c)
  target_sources(nvtop PRIVATE extract_gpuinfo_panfrost_utils.c)
endif()

if (PANTHOR_SUPPORT)
  target_sources(nvtop PRIVATE extract_gpuinfo_panthor.c)
  target_sources(nvtop PRIVATE extract_gpuinfo_panthor_utils.c)
endif()

if ((PANFROST_SUPPORT) OR (PANTHOR_SUPPORT))
  target_sources(nvtop PRIVATE extract_gpuinfo_mali_common.c)
endif()

target_include_directories(nvtop PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include)

find_package(Sanitizers)

add_sanitizers(nvtop)

set_property(TARGET nvtop PROPERTY C_STANDARD 11)

target_compile_definitions(nvtop PRIVATE _GNU_SOURCE)

target_link_libraries(nvtop
  PRIVATE ncurses m ${CMAKE_DL_LIBS})

install(TARGETS nvtop
  RUNTIME DESTINATION bin)

include(compile-flags-helpers)
include(${PROJECT_SOURCE_DIR}/cmake/optimization_flags.cmake)

add_compiler_option_to_target_type(nvtop Debug PRIVATE ${ADDITIONAL_DEBUG_COMPILE_OPTIONS})
add_linker_option_to_all_but_target_type(nvtop dummy PRIVATE ${ADDITIONAL_RELEASE_LINK_OPTIONS})
