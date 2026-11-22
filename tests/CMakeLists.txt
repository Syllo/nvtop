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
                    multi_fan_max multi_fan_legacy_fallback fan_count_zero)
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
