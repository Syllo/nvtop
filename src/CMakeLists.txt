include(CheckCSourceCompiles)

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
set(CMAKE_REQUIRED_INCLUDES "${NVML_INCLUDE_DIRS};${PROJECT_BINARY_DIR}/include")
set(CMAKE_REQUIRED_LIBRARIES "${NVML_LIBRARIES}")
unset(COMPILE_RESULT CACHE)
check_c_source_compiles(
  "
#include <stdlib.h>
#include <nvml.h>

int main()
{
    nvmlReturn_t ret = nvmlInit();
    unsigned devCount;
    nvmlReturn_t ret2 = nvmlDeviceGetCount(&devCount);
    nvmlDevice_t device;
    nvmlReturn_t ret3 = nvmlDeviceGetHandleByIndex(0u, &device);
    nvmlReturn_t ret4 = nvmlDeviceGetComputeRunningProcesses(device, NULL, NULL);
    nvmlReturn_t ret5 = nvmlDeviceGetGraphicsRunningProcesses(device, NULL, NULL);
    return EXIT_SUCCESS;
}
  " COMPILE_RESULT)

if(NOT COMPILE_RESULT)
  message(WARNING "The downloaded \"nvml.h\" header does not match the installed library version, unprecise information may be displayed in nvtop as a result."
  "\nYou can solve this by dowloading it from the src repository of https://github.com/NVIDIA/nvidia-settings using the tag that matches your driver version and placing the file in the build/include directory of nvtop.")
  target_compile_definitions(nvtop PRIVATE NVML_NO_UNVERSIONED_FUNC_DEFS)
endif()
unset(COMPILE_RESULT CACHE)

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
