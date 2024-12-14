#
# - Find the ROCm SMI library
#
# ROCM_SMI_INCLUDE_DIRS - where to find rocm_smi/rocm_smi.h
# ROCM_SMI_LIBRARIES    - List of libraries
# ROCM_SMI_FOUND        - True if ROCm SMI is found.
# A "rocm_smi" target is created when found

find_path(ROCM_SMI_INCLUDE_DIR
    NAMES
    rocm_smi/rocm_smi.h
    PATHS
    "/opt/rocm/include"
)

find_library(ROCM_SMI_LIBRARY
    NAMES
    rocm_smi64
    PATHS
    "/opt/rocm/lib"
)

if (ROCM_SMI_INCLUDE_DIR)
    if (EXISTS "${ROCM_SMI_INCLUDE_DIR}/rocm_version.h")
        set(rocm_version_content "")
        file(READ "${ROCM_SMI_INCLUDE_DIR}/rocm_version.h" rocm_version_content ENCODING UTF-8)
        string(REGEX MATCH "ROCM_VERSION_MAJOR ([0-9]*).*ROCM_VERSION_MINOR ([0-9]*).*ROCM_VERSION_PATCH ([0-9]*)" rocm_version_match ${rocm_version_content})
        if (NOT "${rocm_version_match}" STREQUAL "")
            message("Found version content: ${CMAKE_MATCH_1} ${CMAKE_MATCH_2} ${CMAKE_MATCH_3}")
            set(ROCM_SMI_VERSION "${CMAKE_MATCH_1}.${CMAKE_MATCH_2}.${CMAKE_MATCH_3}")
        endif()
    endif()
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ROCmSMI
    REQUIRED_VARS ROCM_SMI_LIBRARY ROCM_SMI_INCLUDE_DIR
    VERSION_VAR ROCM_SMI_VERSION)

if(ROCmSMI_FOUND)
    set(ROCM_SMI_LIBRARIES ${ROCM_SMI_LIBRARY})
    set(ROCM_SMI_INCLUDE_DIRS ${ROCM_SMI_INCLUDE_DIR})

    add_library(rocm_smi INTERFACE IMPORTED GLOBAL)
    target_include_directories(rocm_smi INTERFACE ${ROCM_SMI_INCLUDE_DIRS})
    set_target_properties(rocm_smi PROPERTIES INTERFACE_INCLUDE_DIRECTORIES ${ROCM_SMI_INCLUDE_DIRS})
    target_link_libraries(rocm_smi INTERFACE ${ROCM_SMI_LIBRARIES})
else()
    set(ROCM_SMI_LIBRARIES)
    set(ROCM_SMI_INCLUDE_DIRS)
endif()

mark_as_advanced(ROCM_SMI_LIBRARIES ROCM_SMI_INCLUDE_DIRS)