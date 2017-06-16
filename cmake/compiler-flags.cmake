include (CheckCCompilerFlag)

set (ALL_WARNING_FLAGS
  "-Waggressive-loop-optimizations"
  "-Wall"
  "-Wbad-function-cast"
  "-Wcast-align"
  "-Wcast-qual"
  #"-Wconversion"
  "-Wdisabled-optimization"
  "-Wdouble-promotion"
  "-Wextra"
  "-Wfloat-conversion"
  "-Wfloat-equal"
  "-Whsa"
  "-Winit-self"
  "-Wlogical-op"
  "-Wmissing-declarations"
  "-Wmissing-parameter-type"
  "-Wmissing-prototypes"
  "-Wnested-externs"
  "-Wnormalized=nfc"
  "-Wnull-dereference"
  "-Wold-style-declaration"
  "-Wold-style-definition"
  #"-Wpadded"
  "-Wpedantic"
  "-Wpointer-sign"
  "-Wshadow"
  "-Wstrict-aliasing"
  "-Wstrict-prototypes"
  #"-Wsuggest-attribute=const"
  "-Wswitch-enum"
  "-Wtrampolines"
  "-Wuninitialized"
  "-Wunsafe-loop-optimizations"
  "-Wunused-result"
  )

function (check_all_warning_flags flag_list)

  foreach (warning_flag ${flag_list})
    check_c_compiler_flag(${warning_flag} "Working${warning_flag}")
    if ("${Working${warning_flag}}")
      list(APPEND VALID_WARNINGS "${warning_flag}")
    endif ("${Working${warning_flag}}")
  endforeach (warning_flag)

  string(REPLACE ";" " " COMPILER_AVALIABLE_WARNINGS "${VALID_WARNINGS}")
  set (COMPILER_AVALIABLE_WARNINGS ${COMPILER_AVALIABLE_WARNINGS} PARENT_SCOPE)

endfunction (check_all_warning_flags)

check_all_warning_flags("${ALL_WARNING_FLAGS}")

string(REPLACE ";" " "
  COMPILER_AVALIABLE_WARNINGS "${COMPILER_AVALIABLE_WARNINGS}")

set(CMAKE_REQUIRED_FLAGS "-fsanitize=address")
check_c_compiler_flag("-fsanitize=address" compiler_has_address_sanitizer)
unset(CMAKE_REQUIRED_FLAGS)
if(compiler_has_address_sanitizer)
  set(COMPILER_ADDRESS_SANITIZER_FLAG "-fsanitize=address")
  # Nicer stack trace
  check_c_compiler_flag("-fno-omit-frame-pointer"
    compiler_has_no_omit_frame_pointer)
  if(compiler_has_no_omit_frame_pointer)
    set(COMPILER_ADDRESS_SANITIZER_FLAG
      "${COMPILER_ADDRESS_SANITIZER_FLAG} -fno-omit-frame-pointer")
  endif()
endif()

set(CMAKE_REQUIRED_FLAGS "-fsanitize=undefined")
check_c_compiler_flag("-fsanitize=undefined" compiler_has_undefined_sanitizer)
unset(CMAKE_REQUIRED_FLAGS)
if(compiler_has_undefined_sanitizer)
  set(COMPILER_UNDEFINED_SANITIZER_FLAG "-fsanitize=undefined")
endif()

set(CMAKE_REQUIRED_FLAGS "-flto")
check_c_compiler_flag("-flto" compiler_has_lto)
unset(CMAKE_REQUIRED_FLAGS)
if(compiler_has_lto)
  set(COMPILER_LTO_FLAG "-flto")
endif()

check_c_compiler_flag("-march=native" compiler_has_march_native)
if(compiler_has_march_native)
  set(COMPILER_MARCH_NATIVE "-march=native")
endif()
