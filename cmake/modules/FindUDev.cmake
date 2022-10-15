# Configure libudev environment
#
# UDEV_FOUND - system has a libudev
# UDEV_INCLUDE_DIR - where to find header files
# UDEV_LIBRARIES - the libraries to link against udev
# UDEV_STABLE - it's true when is the version greater or equals to 143 - version when the libudev was stabilized in its API
# An "udev" target is created when found
#
# Adapted from a version of Petr Vanek
# copyright (c) 2011 Petr Vanek <petr@scribus.info>
# copyright (c) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
#
# Redistribution and use of this file is allowed according to the terms of the BSD license.
#

pkg_search_module(PC_UDEV QUIET libudev)

find_path(UDEV_INCLUDE_DIR
    NAMES
    libudev.h
    HINTS
    ${PC_UDEV_INCLUDE_DIRS}
)

find_library(UDEV_LIBRARY
    NAMES udev
    HINTS
    ${PC_UDEV_LIBRARY_DIRS}
)
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(UDev
    REQUIRED_VARS UDEV_LIBRARY UDEV_INCLUDE_DIR
    VERSION_VAR PC_UDEV_VERSION)

if(UDEV_FOUND)
    if(PC_UDEV_VERSION GREATER_EQUAL "143")
        set(UDEV_STABLE TRUE)
    else()
        set(UDEV_STABLE FALSE)
    endif()

    set(UDEV_LIBRARIES ${UDEV_LIBRARY})
    set(UDEV_INCLUDE_DIRS ${UDEV_INCLUDE_DIR})

    message(STATUS "Libudev stable: ${UDEV_STABLE}")

    add_library(udev INTERFACE IMPORTED GLOBAL)
    set_target_properties(udev PROPERTIES INTERFACE_INCLUDE_DIRECTORIES ${UDEV_INCLUDE_DIRS})
    target_link_libraries(udev INTERFACE ${UDEV_LIBRARIES})
else()
    set(UDEV_LIBRARIES)
    set(UDEV_INCLUDE_DIRS)
endif()

mark_as_advanced(UDEV_LIBRARIES UDEV_INCLUDE_DIRS)