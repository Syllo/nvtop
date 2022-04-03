#.rst:
# FindLibdrm
# -------
#
# Try to find libdrm on a Unix system.
#
# This will define the following variables:
#
# ``Libdrm_FOUND``
#     True if (the requested version of) libdrm is available
# ``Libdrm_VERSION``
#     The version of libdrm
# ``Libdrm_LIBRARIES``
#     This can be passed to target_link_libraries() instead of the ``Libdrm::Libdrm``
#     target
# ``Libdrm_INCLUDE_DIRS``
#     This should be passed to target_include_directories() if the target is not
#     used for linking
# ``Libdrm_DEFINITIONS``
#     This should be passed to target_compile_options() if the target is not
#     used for linking
#
# If ``Libdrm_FOUND`` is TRUE, it will also define the following imported target:
#
# ``Libdrm::Libdrm``
#     The libdrm library
#
# In general we recommend using the imported target, as it is easier to use.
# Bear in mind, however, that if the target is in the link interface of an
# exported library, it must be made available by the package config file.

#=============================================================================
# SPDX-FileCopyrightText: 2014 Alex Merry <alex.merry@kde.org>
# SPDX-FileCopyrightText: 2014 Martin Gräßlin <mgraesslin@kde.org>
#
# SPDX-License-Identifier: BSD-3-Clause
#=============================================================================

if(CMAKE_VERSION VERSION_LESS 2.8.12)
    message(FATAL_ERROR "CMake 2.8.12 is required by FindLibdrm.cmake")
endif()
if(CMAKE_MINIMUM_REQUIRED_VERSION VERSION_LESS 2.8.12)
    message(AUTHOR_WARNING "Your project should require at least CMake 2.8.12 to use FindLibdrm.cmake")
endif()

if(NOT WIN32)
    # Use pkg-config to get the directories and then use these values
    # in the FIND_PATH() and FIND_LIBRARY() calls
    find_package(PkgConfig)
    pkg_check_modules(PKG_Libdrm QUIET libdrm)

    set(Libdrm_DEFINITIONS ${PKG_Libdrm_CFLAGS_OTHER})
    set(Libdrm_VERSION ${PKG_Libdrm_VERSION})

    find_path(Libdrm_INCLUDE_DIR
        NAMES
            xf86drm.h
        HINTS
            ${PKG_Libdrm_INCLUDE_DIRS}
    )
    find_library(Libdrm_LIBRARY
        NAMES
            drm
        HINTS
            ${PKG_Libdrm_LIBRARY_DIRS}
    )

    include(FindPackageHandleStandardArgs)
    find_package_handle_standard_args(Libdrm
        FOUND_VAR
            Libdrm_FOUND
        REQUIRED_VARS
            Libdrm_LIBRARY
            Libdrm_INCLUDE_DIR
        VERSION_VAR
            Libdrm_VERSION
    )

    if(Libdrm_FOUND AND NOT TARGET Libdrm::Libdrm)
        add_library(Libdrm::Libdrm UNKNOWN IMPORTED)
        set_target_properties(Libdrm::Libdrm PROPERTIES
            IMPORTED_LOCATION "${Libdrm_LIBRARY}"
            INTERFACE_COMPILE_OPTIONS "${Libdrm_DEFINITIONS}"
            INTERFACE_INCLUDE_DIRECTORIES "${Libdrm_INCLUDE_DIR}"
            INTERFACE_INCLUDE_DIRECTORIES "${Libdrm_INCLUDE_DIR}/libdrm"
        )
    endif()

    mark_as_advanced(Libdrm_LIBRARY Libdrm_INCLUDE_DIR)

    # compatibility variables
    set(Libdrm_LIBRARIES ${Libdrm_LIBRARY})
    set(Libdrm_INCLUDE_DIRS ${Libdrm_INCLUDE_DIR} "${Libdrm_INCLUDE_DIR}/libdrm")
    set(Libdrm_VERSION_STRING ${Libdrm_VERSION})

else()
    message(STATUS "FindLibdrm.cmake cannot find libdrm on Windows systems.")
    set(Libdrm_FOUND FALSE)
endif()

include(FeatureSummary)
set_package_properties(Libdrm PROPERTIES
    URL "https://wiki.freedesktop.org/dri/"
    DESCRIPTION "Userspace interface to kernel DRM services"
)
