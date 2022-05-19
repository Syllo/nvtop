include(CheckCSourceCompiles)

configure_file(
  "${PROJECT_SOURCE_DIR}/include/nvtop/version.h.in"
  "${PROJECT_BINARY_DIR}/include/nvtop/version.h"
  IMMEDIATE @ONLY)

add_executable (nvtop
  nvtop.c
  interface.c
  interface_layout_selection.c
  interface_options.c
  interface_setup_win.c
  interface_ring_buffer.c
  get_process_info_linux.c
  extract_gpuinfo.c
  time.c
  plot.c
  ini.c)

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
if (HAS_REALLOCARRAY)
  target_compile_definitions(nvtop PRIVATE HAS_REALLOCARRAY)
endif()

if (NVIDIA_SUPPORT)
  target_sources(nvtop PRIVATE extract_gpuinfo_nvidia.c)
endif()

if (AMDGPU_SUPPORT)
  # Search for libdrm for AMDGPU support
  find_package(Libdrm)
  if (Libdrm_FOUND)
    message(STATUS "Found libdrm; Enabling AMDGPU support")
    target_include_directories(nvtop PRIVATE ${Libdrm_INCLUDE_DIRS})
    target_sources(nvtop PRIVATE extract_gpuinfo_amdgpu.c)
  else()
    message(STATUS "libdrm not found; Disabling AMDGPU support")
  endif()
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

install (TARGETS nvtop
  RUNTIME DESTINATION bin)

include(compile-flags-helpers)
include(${PROJECT_SOURCE_DIR}/cmake/optimization_flags.cmake)

add_compiler_option_to_target_type(nvtop Debug PRIVATE ${ADDITIONAL_DEBUG_COMPILE_OPTIONS})
add_linker_option_to_all_but_target_type(nvtop dummy PRIVATE ${ADDITIONAL_RELEASE_LINK_OPTIONS})

add_sanitizers_to_target(nvtop Debug PRIVATE address undefined)
