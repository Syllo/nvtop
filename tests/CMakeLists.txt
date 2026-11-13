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
                    memory_probes pcie_preserved uma_idle
                    utilization_probe_not_found utilization_buffered_not_found utilization_samples)
    add_test(NAME Nvidia.${test_case} COMMAND nvidiaTests ${test_case})
  endforeach()
endif()
if (BUILD_TESTING AND GTest_FOUND)

  option(THOROUGH_TESTING "Enable extensive testing (takes hours, e.g., use once per release)" OFF)

  # Create a library for testing
  add_library(testLib
    ${PROJECT_SOURCE_DIR}/src/interface_layout_selection.c
    ${PROJECT_SOURCE_DIR}/src/interface_options.c
    ${PROJECT_SOURCE_DIR}/src/pcie_utilization.c
    ${PROJECT_SOURCE_DIR}/src/ini.c
  )
  if (UNIX AND NOT APPLE)
    target_sources(testLib PRIVATE ${PROJECT_SOURCE_DIR}/src/extract_processinfo_fdinfo.c)
  elseif (APPLE)
    target_sources(testLib PRIVATE ${PROJECT_SOURCE_DIR}/src/extract_processinfo_mac.c)
  endif()
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

# Exercise the runtime-loaded ABI without depending on a particular GPU or GTest.
if(BUILD_TESTING AND AMDGPU_SUPPORT AND AMDSMI_PCIE_AVAILABLE)
  foreach(variant complete incomplete)
    add_library(fakeAmDsmi_${variant} SHARED fake_amdsmi.c)
    target_include_directories(fakeAmDsmi_${variant} PRIVATE "${AMDSMI_PCIE_INCLUDE_DIR}")
    set_target_properties(fakeAmDsmi_${variant} PROPERTIES
      PREFIX "" SUFFIX "" OUTPUT_NAME "${AMDSMI_PCIE_SONAME}"
      LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${variant}")
  endforeach()
  target_compile_definitions(fakeAmDsmi_incomplete PRIVATE NVTOP_TEST_MISSING_SYMBOL)

  foreach(variant normal missing)
    add_executable(amdsmiPcieTests_${variant} amdsmiPcieTests.c ${PROJECT_SOURCE_DIR}/src/amdgpu_amdsmi.c)
    target_include_directories(amdsmiPcieTests_${variant} PRIVATE
      ${PROJECT_SOURCE_DIR}/include "${AMDSMI_PCIE_INCLUDE_DIR}")
    target_link_libraries(amdsmiPcieTests_${variant} PRIVATE ${CMAKE_DL_LIBS})
    if(variant STREQUAL "normal")
      set(test_soname "${AMDSMI_PCIE_SONAME}")
    else()
      set(test_soname "libnvtop-test-amdsmi-does-not-exist.so")
    endif()
    target_compile_definitions(amdsmiPcieTests_${variant} PRIVATE
      HAVE_AMDSMI_PCIE AMDSMI_PCIE_SONAME="${test_soname}")
  endforeach()
  add_test(NAME amdsmi_pcie_current COMMAND amdsmiPcieTests_normal)
  set_tests_properties(amdsmi_pcie_current PROPERTIES
    ENVIRONMENT "LD_LIBRARY_PATH=$<TARGET_FILE_DIR:fakeAmDsmi_complete>")
  add_test(NAME amdsmi_pcie_missing_symbol COMMAND amdsmiPcieTests_normal unavailable)
  set_tests_properties(amdsmi_pcie_missing_symbol PROPERTIES
    ENVIRONMENT "LD_LIBRARY_PATH=$<TARGET_FILE_DIR:fakeAmDsmi_incomplete>")
  add_test(NAME amdsmi_pcie_missing_library COMMAND amdsmiPcieTests_missing unavailable)
endif()
