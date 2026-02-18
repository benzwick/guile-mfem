# SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
# SPDX-License-Identifier: BSD-3-Clause

# FindGuile.cmake
#
# Find GNU Guile development files.
#
# Sets:
#   GUILE_FOUND
#   GUILE_INCLUDE_DIRS
#   GUILE_LIBRARIES
#   GUILE_VERSION
#   GUILE_SITE_DIR
#   GUILE_EXTENSION_DIR

find_program(GUILE_CONFIG guile-config)
find_program(GUILE_EXECUTABLE guile)

if(GUILE_CONFIG)
  execute_process(COMMAND ${GUILE_CONFIG} compile
    OUTPUT_VARIABLE GUILE_CFLAGS OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${GUILE_CONFIG} link
    OUTPUT_VARIABLE GUILE_LDFLAGS OUTPUT_STRIP_TRAILING_WHITESPACE)

  # Extract include dirs from CFLAGS
  string(REGEX MATCHALL "-I[^ ]+" _guile_includes "${GUILE_CFLAGS}")
  string(REGEX REPLACE "-I" "" GUILE_INCLUDE_DIRS "${_guile_includes}")

  # Extract libraries from LDFLAGS
  string(REGEX MATCHALL "-l[^ ]+" _guile_libs "${GUILE_LDFLAGS}")
  string(REGEX REPLACE "-l" "" _guile_lib_names "${_guile_libs}")

  string(REGEX MATCHALL "-L[^ ]+" _guile_libdirs "${GUILE_LDFLAGS}")
  string(REGEX REPLACE "-L" "" _guile_lib_paths "${_guile_libdirs}")

  find_library(GUILE_LIBRARIES
    NAMES guile-3.0 guile
    HINTS ${_guile_lib_paths})
endif()

if(GUILE_EXECUTABLE)
  execute_process(COMMAND ${GUILE_EXECUTABLE} -c "(display (version))"
    OUTPUT_VARIABLE GUILE_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process(COMMAND ${GUILE_EXECUTABLE} -c
    "(display (string-append (%site-dir)))"
    OUTPUT_VARIABLE GUILE_SITE_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process(COMMAND ${GUILE_EXECUTABLE} -c
    "(display (%extension-dir))"
    OUTPUT_VARIABLE GUILE_EXTENSION_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Guile
  REQUIRED_VARS GUILE_LIBRARIES GUILE_INCLUDE_DIRS
  VERSION_VAR GUILE_VERSION)
