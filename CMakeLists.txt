cmake_minimum_required(VERSION 3.1)

#///////////////////////////////////////////////////////////////////#
#                              PROJECT                              #
#///////////////////////////////////////////////////////////////////#

project(nvtop VERSION 1.1.0
  LANGUAGES C)

set(default_build_type "Release")
# Default build type
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to '${default_build_type}' as none was specified.")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE
    STRING "Choose the type of build." FORCE)
endif()
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
  "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

#///////////////////////////////////////////////////////////////////#
#                           DEPENDENCIES                            #
#///////////////////////////////////////////////////////////////////#

if(NVML_RETRIEVE_HEADER_ONLINE)
  file(DOWNLOAD "https://raw.githubusercontent.com/NVIDIA/nvidia-settings/master/src/nvml.h" ${PROJECT_BINARY_DIR}/include/nvml.h)
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(NVML REQUIRED)

add_library(nvml INTERFACE IMPORTED)
set_property(TARGET nvml PROPERTY
  INTERFACE_INCLUDE_DIRECTORIES ${NVML_INCLUDE_DIRS})
set_property(TARGET nvml PROPERTY
  INTERFACE_LINK_LIBRARIES ${NVML_LIBRARIES})

set(CURSES_NEED_NCURSES TRUE)
# Try to find ncurses with unicode support first
set(CURSES_NEED_WIDE TRUE)
find_package(Curses QUIET)
if (NOT CURSE_FOUND)
  # Fallback to regular ncurses library, which may also support unicode!
  set(CURSES_NEED_WIDE FALSE)
  find_package(Curses REQUIRED)
endif()

add_library(ncurses INTERFACE IMPORTED)
set_property(TARGET ncurses PROPERTY
  INTERFACE_INCLUDE_DIRECTORIES ${CURSES_INCLUDE_DIRS})
set_property(TARGET ncurses PROPERTY
  INTERFACE_LINK_LIBRARIES ${CURSES_LIBRARIES})

#///////////////////////////////////////////////////////////////////#
#                        COMPILATION OPTIONS                        #
#///////////////////////////////////////////////////////////////////#

# Use full RPATH on build tree
set(CMAKE_SKIP_BUILD_RPATH FALSE)
# Do not build with install RPATH
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
# Set the RPATH when install
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
# Only set RPATH if the installation directory is not a system directory
LIST(FIND
  CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib"
  isSystemDir)
if("${isSystemDir}" STREQUAL "-1")
  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
else()
  set(CMAKE_INSTALL_RPATH "")
endif()

add_subdirectory(src)

#///////////////////////////////////////////////////////////////////#
#                             INSTALL                               #
#///////////////////////////////////////////////////////////////////#

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/manpage/nvtop.in"
  "${CMAKE_CURRENT_BINARY_DIR}/manpage/nvtop"
  IMMEDIATE @ONLY)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/manpage/nvtop"
  DESTINATION share/man/man1/
  PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ
  RENAME nvtop.1)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
  IMMEDIATE @ONLY)
add_custom_target(uninstall
  COMMAND ${CMAKE_COMMAND} -P
  ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)


#///////////////////////////////////////////////////////////////////#
#                            PACKAGING                              #
#///////////////////////////////////////////////////////////////////#

#set(CPACK_PACKAGE_NAME "nvtop")
#set(CPACK_PACKAGE_VENDOR "Maxime Schmitt")
#set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "NVIDIA GPU monitoring tool")
#set(CPACK_PACKAGE_DESCRIPTION
  #"Nvtop is a htop-like monitoring tool for NVIDIA GPUs. It can print GPUs information such as GPU and memory usage, temperature and active processes.")
#set(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/README.markdown")
#set(CPACK_PACKAGE_EXECUTABLES "nvtop")
#set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
#set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
#set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})

#set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "x86-64")
#set(CPACK_DEBIAN_PACKAGE_DEPENDS "")
#set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Maxime Schmitt <maxime.schmitt@gmail.com>")
#set(CPACK_DEBIAN_PACKAGE_SECTION "Utilities")
#set(CPACK_DEBIAN_PACKAGE_PRIORITY "optional")

#set(CPACK_RPM_PACKAGE_ARCHITECTURE "amd64")
#set(CPACK_RPM_PACKAGE_RELEASE 1)
#set(CPACK_RPM_PACKAGE_LICENSE "GPL3+")
#set(CPACK_RPM_PACKAGE_REQUIRES "")

#set(CPACK_GENERATOR "TGZ;TBZ2")
#list(APPEND CPACK_GENERATOR "DEB") # .deb package build
#list(APPEND CPACK_GENERATOR "RPM") # .rpm package build
#set(CPACK_SOURCE_GENERATOR "TGZ;TBZ2;ZIP")
#set(CPACK_SOURCE_IGNORE_FILES
  #.git*
  #/.*build.*
  #)
#include(CPack)
