find_package(GTest)

# The NVIDIA backend tests use injected NVML entry points and need neither
# GTest nor an NVIDIA driver/SDK, so they can also run on CPU-only CI hosts.
if (BUILD_TESTING AND NVIDIA_SUPPORT AND CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_executable(nvidiaTests
    nvidiaTests.c
    ${PROJECT_SOURCE_DIR}/src/extract_gpuinfo.c
    ${PROJECT_SOURCE_DIR}/src/extract_processinfo_fdinfo.c
    ${PROJECT_SOURCE_DIR}/src/get_process_info_linux.c
    ${PROJECT_SOURCE_DIR}/src/time.c
  )
  target_include_directories(nvidiaTests PRIVATE ${PROJECT_SOURCE_DIR}/include)
  target_compile_definitions(nvidiaTests PRIVATE _GNU_SOURCE)
  if (HAS_REALLOCARRAY)
    target_compile_definitions(nvidiaTests PRIVATE HAS_REALLOCARRAY)
  endif()
  set_property(TARGET nvidiaTests PROPERTY C_STANDARD 11)
  target_link_libraries(nvidiaTests PRIVATE m ${CMAKE_DL_LIBS})
  add_sanitizers(nvidiaTests)
  foreach (test_case uma_version legacy_fallback mixed_processes c2c_link unavailable_memory
                    memory_probes pcie_preserved uma_idle)
    add_test(NAME Nvidia.${test_case} COMMAND nvidiaTests ${test_case})
  endforeach()
endif()
if (BUILD_TESTING AND GTest_FOUND)

  option(THOROUGH_TESTING "Enable extensive testing (takes hours, e.g., use once per release)" OFF)

  # Create a library for testing
  add_library(testLib
    ${PROJECT_SOURCE_DIR}/src/interface_layout_selection.c
    ${PROJECT_SOURCE_DIR}/src/extract_processinfo_fdinfo.c
    ${PROJECT_SOURCE_DIR}/src/interface_options.c
    ${PROJECT_SOURCE_DIR}/src/pcie_utilization.c
    ${PROJECT_SOURCE_DIR}/src/ini.c
  )
  target_include_directories(testLib PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_BINARY_DIR}/include)

  # Tests
  add_executable(
    interfaceTests
    interfaceTests.cpp
  )
  target_link_libraries(interfaceTests PRIVATE testLib GTest::gtest_main)
  gtest_discover_tests(interfaceTests)

  if (THOROUGH_TESTING)
    target_compile_definitions(interfaceTests PRIVATE THOROUGH_TESTING)
  endif()


endif()
