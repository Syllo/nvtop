find_package(GTest)

if (BUILD_TESTING AND CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_executable(fdinfoTests fdinfoTests.c)
  target_include_directories(fdinfoTests PRIVATE ${PROJECT_SOURCE_DIR}/include)
  target_compile_definitions(fdinfoTests PRIVATE _GNU_SOURCE)
  if (HAS_REALLOCARRAY)
    target_compile_definitions(fdinfoTests PRIVATE HAS_REALLOCARRAY)
  endif()
  set_property(TARGET fdinfoTests PROPERTY C_STANDARD 11)
  add_sanitizers(fdinfoTests)
  add_test(NAME Fdinfo.sweep COMMAND fdinfoTests)
endif()

# Plot rendering, checked through an off-screen ncurses window. The braille
# cases need the wide character ncurses API (NVTOP_HAVE_WIDE_CURSES) and a
# UTF-8 locale; without the API, only the other cases are built.
if (BUILD_TESTING)
  add_executable(plotTests plotTests.c ${PROJECT_SOURCE_DIR}/src/plot.c)
  target_include_directories(plotTests PRIVATE ${PROJECT_SOURCE_DIR}/include ${PROJECT_BINARY_DIR}/include)
  # Same feature-test macros as nvtop, which builds plot.c with them.
  target_compile_definitions(plotTests PRIVATE _GNU_SOURCE)
  set_property(TARGET plotTests PROPERTY C_STANDARD 11)
  target_link_libraries(plotTests PRIVATE ncurses m)
  add_sanitizers(plotTests)
  # A single test running every case, so a case cannot be left out; failing
  # cases are named in the output.
  add_test(NAME Plot COMMAND plotTests)
  # The braille capability is checked once per run: on its own.
  add_test(NAME Plot.linux_console COMMAND plotTests linux_console)
  set_tests_properties(Plot Plot.linux_console PROPERTIES SKIP_RETURN_CODE 77)
endif()

# Utilization meter rendering, checked the same way as the plots.
if (BUILD_TESTING)
  add_executable(meterTests meterTests.c ${PROJECT_SOURCE_DIR}/src/meter.c)
  target_include_directories(meterTests PRIVATE ${PROJECT_SOURCE_DIR}/include ${PROJECT_BINARY_DIR}/include)
  target_compile_definitions(meterTests PRIVATE _GNU_SOURCE)
  set_property(TARGET meterTests PROPERTY C_STANDARD 11)
  target_link_libraries(meterTests PRIVATE ncurses m)
  add_sanitizers(meterTests)
  add_test(NAME Meter COMMAND meterTests)
  set_tests_properties(Meter PROPERTIES SKIP_RETURN_CODE 77)
endif()

if (BUILD_TESTING AND ENFLAME_SUPPORT AND CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_executable(enflameTests
    enflameTests.c
    ${PROJECT_SOURCE_DIR}/src/extract_gpuinfo.c
    ${PROJECT_SOURCE_DIR}/src/extract_processinfo_fdinfo.c
    ${PROJECT_SOURCE_DIR}/src/get_process_info_linux.c
    ${PROJECT_SOURCE_DIR}/src/time.c
  )
  target_include_directories(enflameTests PRIVATE ${PROJECT_SOURCE_DIR}/include)
  target_compile_definitions(enflameTests PRIVATE _GNU_SOURCE)
  if (HAS_REALLOCARRAY)
    target_compile_definitions(enflameTests PRIVATE HAS_REALLOCARRAY)
  endif()
  set_property(TARGET enflameTests PROPERTY C_STANDARD 11)
  target_link_libraries(enflameTests PRIVATE m ${CMAKE_DL_LIBS})
  add_sanitizers(enflameTests)
  add_test(NAME Enflame.query_status COMMAND enflameTests)
endif()

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
  foreach (test_case uma_version legacy_fallback mixed_processes process_query empty_process_memory
                    shared_memory_accounting shared_memory_cap shared_memory_buffers
                    system_memory_counters
                    c2c_link c2c_link_count c2c_field_types unavailable_memory
                    memory_probes pcie_preserved uma_idle frozen_process_memory
                    utilization_probe_not_found utilization_buffered_not_found utilization_samples
                    multi_fan_max multi_fan_legacy_fallback fan_count_zero
                    hotspot_temp hotspot_temp_unavailable mem_temp_field_types)
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

  if(APPLE)
    add_executable(
      appleTests
      appleTests.mm
      ${PROJECT_SOURCE_DIR}/src/extract_gpuinfo_apple_utils.m
      ${PROJECT_SOURCE_DIR}/src/get_process_info_mac_utils.c
    )
    target_include_directories(appleTests PRIVATE
      ${PROJECT_SOURCE_DIR}/include
      ${PROJECT_SOURCE_DIR}/src
      ${PROJECT_BINARY_DIR}/include)
    target_link_libraries(appleTests PRIVATE GTest::gtest_main
      "-framework Foundation")
    if (HAS_REALLOCARRAY)
      target_compile_definitions(appleTests PRIVATE HAS_REALLOCARRAY)
    endif()
    add_sanitizers(appleTests)
    gtest_discover_tests(appleTests)
  endif()

  if (THOROUGH_TESTING)
    target_compile_definitions(interfaceTests PRIVATE THOROUGH_TESTING)
  endif()


endif()

if(BUILD_TESTING AND PPU_SUPPORT)
  add_executable(ppuTests ppuTests.c)
  target_include_directories(ppuTests PRIVATE ${PROJECT_SOURCE_DIR}/include)
  if (HAS_REALLOCARRAY)
    target_compile_definitions(ppuTests PRIVATE HAS_REALLOCARRAY)
  endif()
  set_property(TARGET ppuTests PROPERTY C_STANDARD 11)
  target_link_libraries(ppuTests PRIVATE ${CMAKE_DL_LIBS})
  add_sanitizers(ppuTests)
  add_test(NAME ppuTests COMMAND ppuTests)
endif()

if(BUILD_TESTING AND GAUDI_SUPPORT)
  add_executable(gaudiTests gaudiTests.c)
  target_include_directories(gaudiTests PRIVATE ${PROJECT_SOURCE_DIR}/include)
  if (HAS_REALLOCARRAY)
    target_compile_definitions(gaudiTests PRIVATE HAS_REALLOCARRAY)
  endif()
  set_property(TARGET gaudiTests PROPERTY C_STANDARD 11)
  target_compile_definitions(gaudiTests PRIVATE _GNU_SOURCE)
  find_package(Threads REQUIRED)
  target_link_libraries(gaudiTests PRIVATE Threads::Threads ${CMAKE_DL_LIBS})
  add_sanitizers(gaudiTests)
  add_test(NAME gaudiTests COMMAND gaudiTests)
endif()

# The Intel NPU tests parse fdinfo fixture files and need no NPU.
if(BUILD_TESTING AND INTEL_NPU_SUPPORT AND CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_executable(ivpuTests
    ivpuTests.c
    ${PROJECT_SOURCE_DIR}/src/extract_gpuinfo.c
    ${PROJECT_SOURCE_DIR}/src/extract_processinfo_fdinfo.c
    ${PROJECT_SOURCE_DIR}/src/get_process_info_linux.c
    ${PROJECT_SOURCE_DIR}/src/time.c
  )
  target_include_directories(ivpuTests PRIVATE ${PROJECT_SOURCE_DIR}/include)
  target_compile_definitions(ivpuTests PRIVATE _GNU_SOURCE
    IVPU_FIXTURE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/ivpu")
  if (HAS_REALLOCARRAY)
    target_compile_definitions(ivpuTests PRIVATE HAS_REALLOCARRAY)
  endif()
  set_property(TARGET ivpuTests PROPERTY C_STANDARD 11)
  target_link_libraries(ivpuTests PRIVATE m ${CMAKE_DL_LIBS})
  add_sanitizers(ivpuTests)
  foreach (test_case fdinfo_active fdinfo_idle fdinfo_imported fdinfo_unpopulated
                    fdinfo_no_memory fdinfo_other_driver fdinfo_size fdinfo_callback memory_display device_discovery
                    zero_usage_claim)
    add_test(NAME Ivpu.${test_case} COMMAND ivpuTests ${test_case})
  endforeach()
endif()

# The AMD NPU tests parse fdinfo fixture files and need no NPU.
if(BUILD_TESTING AND AMD_NPU_SUPPORT)
  add_executable(amdxdnaTests
    amdxdnaTests.c
    ${PROJECT_SOURCE_DIR}/src/extract_gpuinfo.c
    ${PROJECT_SOURCE_DIR}/src/extract_processinfo_fdinfo.c
    ${PROJECT_SOURCE_DIR}/src/get_process_info_linux.c
    ${PROJECT_SOURCE_DIR}/src/time.c
  )
  target_include_directories(amdxdnaTests PRIVATE ${PROJECT_SOURCE_DIR}/include)
  target_compile_definitions(amdxdnaTests PRIVATE _GNU_SOURCE
    AMDXDNA_FIXTURE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/amdxdna")
  if (HAS_REALLOCARRAY)
    target_compile_definitions(amdxdnaTests PRIVATE HAS_REALLOCARRAY)
  endif()
  set_property(TARGET amdxdnaTests PROPERTY C_STANDARD 11)
  target_link_libraries(amdxdnaTests PRIVATE m ${CMAKE_DL_LIBS})
  add_sanitizers(amdxdnaTests)
  foreach (test_case fdinfo_idle fdinfo_active fdinfo_total_fallback fdinfo_other_driver
                    fdinfo_callback process_utilization memory_display device_discovery)
    add_test(NAME Amdxdna.${test_case} COMMAND amdxdnaTests ${test_case})
  endforeach()
endif()

# The test target only runs the test programs. check builds the ones of this
# directory first, then runs them all, for the configuration being built
# (multi-configuration generators need it).
if (BUILD_TESTING)
  get_property(test_targets DIRECTORY PROPERTY BUILDSYSTEM_TARGETS)
  add_custom_target(check
    COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure "$<$<BOOL:$<CONFIG>>:-C;$<CONFIG>>"
    COMMAND_EXPAND_LISTS
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
    USES_TERMINAL)
  add_dependencies(check ${test_targets})
endif()
