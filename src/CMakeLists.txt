configure_file(
  "${PROJECT_SOURCE_DIR}/include/nvtop/version.h.in"
  "${PROJECT_BINARY_DIR}/include/nvtop/version.h"
  IMMEDIATE @ONLY)

add_executable (nvtop
  nvtop.c
  interface.c
  interface_layout_selection.c
  get_process_info_linux.c
  extract_gpuinfo.c
  time.c
  plot.c)

target_include_directories(nvtop PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include)

set_property(TARGET nvtop PROPERTY C_STANDARD 11)

target_link_libraries(nvtop
  PRIVATE nvml ncurses m)

install (TARGETS nvtop
  RUNTIME DESTINATION bin)

include(compile-flags-helpers)
include(${PROJECT_SOURCE_DIR}/cmake/optimization_flags.cmake)

add_compiler_option_to_target_type(nvtop Debug PRIVATE ${ADDITIONAL_DEBUG_COMPILE_OPTIONS})
add_linker_option_to_all_but_target_type(nvtop dummy PRIVATE ${ADDITIONAL_RELEASE_LINK_OPTIONS})

add_sanitizers_to_target(nvtop Debug PRIVATE address undefined)
