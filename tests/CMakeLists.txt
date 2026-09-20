find_package(GTest)
if (BUILD_TESTING AND GTest_FOUND)

  option(THOROUGH_TESTING "Enable extensive testing (takes hours, e.g., use once per release)" OFF)

  # Create a library for testing
  add_library(testLib
    ${PROJECT_SOURCE_DIR}/src/interface_layout_selection.c
    ${PROJECT_SOURCE_DIR}/src/extract_processinfo_fdinfo.c
    ${PROJECT_SOURCE_DIR}/src/interface_options.c
    ${PROJECT_SOURCE_DIR}/src/pcie_utilization.c
    ${PROJECT_SOURCE_DIR}/src/ini.c
    ${PROJECT_SOURCE_DIR}/src/time.c
  )
  if (REMOTE_SUPPORT)
    target_sources(testLib PRIVATE
      ${PROJECT_SOURCE_DIR}/src/remote_wire.c
      ${PROJECT_SOURCE_DIR}/src/extract_gpuinfo_remote.c
      ${PROJECT_SOURCE_DIR}/src/export_server.c)
    target_compile_definitions(testLib PRIVATE REMOTE_SUPPORT=1)
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

  if (REMOTE_SUPPORT)
    add_executable(
      wireTests
      wireTest.cpp
    )
    target_link_libraries(wireTests PRIVATE testLib GTest::gtest_main)
    gtest_discover_tests(wireTests)
  endif()

  if (THOROUGH_TESTING)
    target_compile_definitions(interfaceTests PRIVATE THOROUGH_TESTING)
  endif()


endif()
