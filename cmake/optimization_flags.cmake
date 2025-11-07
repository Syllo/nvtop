set(ADDITIONAL_DEBUG_COMPILE_OPTIONS
  "-Wall"
  #"-Wpedantic"
  "-Wextra"
  "-Waddress"
  "-Waggressive-loop-optimizations"
  #"-Wcast-qual"
  #"-Wcast-align"
  "-Wbad-function-cast"
  "-Wmissing-declarations"
  "-Wmissing-parameter-type"
  "-Wmissing-prototypes"
  "-Wnested-externs"
  "-Wold-style-declaration"
  "-Wold-style-definition"
  "-Wstrict-prototypes"
  "-Wpointer-sign"
  "-Wdouble-promotion"
  "-Wuninitialized"
  "-Winit-self"
  "-Wstrict-aliasing"
  "-Wsuggest-attribute=const"
  "-Wtrampolines"
  "-Wfloat-equal"
  "-Wshadow"
  "-Wunsafe-loop-optimizations"
  "-Wfloat-conversion"
  "-Wlogical-op"
  "-Wnormalized"
  "-Wdisabled-optimization"
  "-Whsa"
  #"-Wconversion"
  "-Wunused-result"
  "-Werror=implicit-function-declaration"
  #"-Wpadded"
  "-Wformat"
  "-Wformat-security"
  CACHE INTERNAL "String"
  )

# Security hardening flags for release builds
if(WIN32)
  # Windows MinGW: Stack protector only (FORTIFY_SOURCE has compatibility issues)
  set(ADDITIONAL_RELEASE_COMPILE_OPTIONS
    "-fstack-protector-strong"
    CACHE INTERNAL "String"
  )
else()
  # Linux/Unix: Full hardening
  set(ADDITIONAL_RELEASE_COMPILE_OPTIONS
    "-fstack-protector-strong"
    "-D_FORTIFY_SOURCE=2"
    "-fPIE"
    CACHE INTERNAL "String"
  )
endif()

set(ADDITIONAL_RELEASE_LINK_OPTIONS
  "-Wl,-z,relro"
  "-Wl,-z,now"
  "-pie"
  CACHE INTERNAL "String"
  )
