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

if(BUILD_TESTING AND PPU_SUPPORT)
  add_executable(ppuTests ppuTests.c)
  target_include_directories(ppuTests PRIVATE ${PROJECT_SOURCE_DIR}/include)
  target_link_libraries(ppuTests PRIVATE ${CMAKE_DL_LIBS})
  set_property(TARGET ppuTests PROPERTY C_STANDARD 11)
  add_test(NAME ppuTests COMMAND ppuTests)
endif()
