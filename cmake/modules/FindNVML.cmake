# Version 1.2
# Public Domain
# Written by Maxime SCHMITT <maxime.schmitt@etu.unistra.fr>

#/////////////////////////////////////////////////////////////////////////////#
#                                                                             #
# Search for Nvidia nvml library on the system                                #
# Call with find_package(NVML)                                                #
# The module defines:                                                         #
#   - NVML_FOUND        - If NVML was found                                   #
#   - NVML_INCLUDE_DIRS - the NVML include directories                        #
#   - NVML_LIBRARIES    - the NVML library directories                        #
#   - NVML_API_VERSION  - the NVML api version                                #
#                                                                             #
#/////////////////////////////////////////////////////////////////////////////#

if (NVML_INCLUDE_DIRS AND NVML_LIBRARIES)
  set(NVML_FIND_QUIETLY TRUE)
endif()

# Headers
file(GLOB nvml_header_path_hint /usr/include/nvidia*/include /usr/local/cuda*/include /opt/cuda*/include /usr/lib/*linux-gnu /usr/local/cuda*/targets/*/include "$ENV{CUDA_HOME}/include")
find_path(NVML_INCLUDE_DIRS NAMES nvml.h
  PATHS ${nvml_header_path_hint} ${PROJECT_BINARY_DIR}/include)

# library
if("${CMAKE_SIZEOF_VOID_P}" EQUAL "8") # 64bit
  file(GLOB nvml_lib_path_hint /usr/lib64/nvidia*/ /usr/lib/nvidia*/ /usr/local/cuda*/targets/*/lib/stubs/ "$ENV{CUDA_HOME}/lib64/"
    "$ENV{CUDA_HOME}/lib/" "$ENV{CUDA_HOME}/targets/*/lib/stubs")
else() # assume 32bit
  file(GLOB nvml_lib_path_hint /usr/lib32/nvidia*/ /usr/lib/nvidia*/ /usr/local/cuda*/targets/*/lib/stubs/ "$ENV{CUDA_HOME}/lib/"
    "$ENV{CUDA_HOME}/targets/*/lib/stubs")
endif()
find_library(NVML_LIBRARIES NAMES nvidia-ml libnvidia-ml.so.1
  PATHS ${nvml_lib_path_hint})

# Version
set(filename "${NVML_INCLUDE_DIRS}/nvml.h")
if (EXISTS ${filename})
  file(READ "${filename}" nvml_header)
  set(nvml_api_version_match "NVML_API_VERSION")

  string(REGEX REPLACE ".*#[ \t]*define[ \t]*${nvml_api_version_match}[ \t]*([0-9]+).*"
    "\\1" nvml_api_version "${nvml_header}")

  if (nvml_api_version STREQUAL nvml_header AND NOT quiet)
    message(AUTHOR_WARNING "Unable to find nvml api version")
  else()
    set(NVML_API_VERSION   "${nvml_api_version}")
  endif()
endif(EXISTS ${filename})

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(NVML
  FOUND_VAR NVML_FOUND
  REQUIRED_VARS NVML_INCLUDE_DIRS NVML_LIBRARIES
  VERSION_VAR NVML_API_VERSION)

mark_as_advanced(NVML_INCLUDE_DIRS NVML_LIBRARIES NVML_API_VERSION)
