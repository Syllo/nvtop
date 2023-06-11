cmake_minimum_required(VERSION 3.10)

#///////////////////////////////////////////////////////////////////#
#                              PROJECT                              #
#///////////////////////////////////////////////////////////////////#

project(nvtop VERSION 3.0.2
  LANGUAGES C CXX)

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

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

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

option(NVIDIA_SUPPORT "Build support for NVIDIA GPUs through libnvml" ON)
option(AMDGPU_SUPPORT "Build support for AMD GPUs through amdgpu driver" ON)
option(INTEL_SUPPORT "Build support for Intel GPUs through i915 driver" ON)
option(MSM_SUPPORT "Build support for Adreno GPUs through msm driver" ON)

add_subdirectory(src)

#///////////////////////////////////////////////////////////////////#
#                             INSTALL                               #
#///////////////////////////////////////////////////////////////////#

string(TIMESTAMP TODAY_MANPAGE "%B %Y")
string(TIMESTAMP TODAY_ISO_8601 "%Y-%m-%d")
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/manpage/nvtop.in"
  "${CMAKE_CURRENT_BINARY_DIR}/manpage/nvtop"
  IMMEDIATE @ONLY)
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/desktop/nvtop.metainfo.xml.in"
  "${CMAKE_CURRENT_BINARY_DIR}/desktop/nvtop.metainfo.xml"
  IMMEDIATE @ONLY)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/manpage/nvtop"
  DESTINATION share/man/man1/
  PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ
  RENAME nvtop.1)
install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/desktop/nvtop.svg"
  DESTINATION share/icons
  PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/desktop/nvtop.desktop"
  DESTINATION share/applications
  PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/desktop/nvtop.metainfo.xml"
  DESTINATION share/metainfo
  PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
  IMMEDIATE @ONLY)
add_custom_target(uninstall
  COMMAND ${CMAKE_COMMAND} -P
  ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)

#///////////////////////////////////////////////////////////////////#
#                             TESTING                               #
#///////////////////////////////////////////////////////////////////#


if (NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
  option(BUILD_TESTING "Build tests" OFF)
endif()
include(CTest)
add_subdirectory(tests)
