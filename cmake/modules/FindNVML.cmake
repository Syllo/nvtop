# Version 1.1
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
file(GLOB nvml_header_path_hint /usr/local/cuda*/include /opt/cuda*/include)
find_path(NVML_INCLUDE_DIRS NAMES nvml.h
  HINTS ${nvml_header_path_hint})

# library
find_library(NVML_LIBRARIES NAMES nvml nvidia-ml)

# Version
set(filename "${NVML_INCLUDE_DIRS}/nvml.h")
if (NOT EXISTS ${filename} AND NOT quiet)
  message(AUTHOR_WARNING "Unable to find ${filename}")
endif()
file(READ "${filename}" nvml_header)
set(nvml_api_version_match "NVML_API_VERSION")

string(REGEX REPLACE ".*#[ \t]*define[ \t]*${nvml_api_version_match}[ \t]*([0-9]+).*"
  "\\1" nvml_api_version "${nvml_header}")

if (nvml_api_version STREQUAL nvml_header)
  message(AUTHOR_WARNING "Unable to find nvml api version")
else()
  set(NVML_API_VERSION   "${nvml_api_version}")
endif()

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(NVML
  FOUND_VAR NVML_FOUND
  REQUIRED_VARS NVML_INCLUDE_DIRS NVML_LIBRARIES
  VERSION_VAR NVML_API_VERSION)

mark_as_advanced(NVML_INCLUDE_DIRS NVML_LIBRARIES NVML_API_VERSION)
