find_package(GTest)
if (BUILD_TESTING AND GTest_FOUND)

  option(THOROUGH_TESTING "Enable extensive testing (takes hours, e.g., use once per release)" OFF)

  # Create a library for testing
  add_library(testLib
    ${PROJECT_SOURCE_DIR}/src/interface_layout_selection.c
    ${PROJECT_SOURCE_DIR}/src/interface_options.c
    ${PROJECT_SOURCE_DIR}/src/ini.c
  )
  if(APPLE)
    target_sources(testLib PRIVATE
      ${PROJECT_SOURCE_DIR}/src/extract_processinfo_mac.c)
  else()
    target_sources(testLib PRIVATE
      ${PROJECT_SOURCE_DIR}/src/extract_processinfo_fdinfo.c)
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
    gtest_discover_tests(appleTests)
  endif()

  if (THOROUGH_TESTING)
    target_compile_definitions(interfaceTests PRIVATE THOROUGH_TESTING)
  endif()


endif()
