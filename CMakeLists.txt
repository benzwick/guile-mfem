cmake_minimum_required(VERSION 3.18)
project(guile-mfem LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)

# Options matching CI workflow cmake-flags
option(GUILE_MFEM_USE_MPI "Build parallel version" OFF)
option(GUILE_MFEM_USE_CUDA "Build with CUDA" OFF)
option(GUILE_MFEM_USE_LIBCEED "Build with libCEED" OFF)
option(GUILE_MFEM_USE_GSLIB "Build with GSlib" OFF)
set(GUILE_MFEM_MFEM_BRANCH "default" CACHE STRING
  "MFEM branch to build. 'default' uses pinned commit hash below.")

# Pinned MFEM commit (equivalent to PyMFEM's repos_sha)
set(GUILE_MFEM_MFEM_SHA "8efb9483bc8353cfcda3cc2531427f048f606e01" CACHE STRING
  "MFEM commit hash used when GUILE_MFEM_MFEM_BRANCH is 'default'")

# Find dependencies
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
find_package(Guile REQUIRED)
find_package(SWIG REQUIRED)
find_package(MFEM REQUIRED)

message(STATUS "Guile version: ${GUILE_VERSION}")
message(STATUS "Guile includes: ${GUILE_INCLUDE_DIRS}")
message(STATUS "SWIG: ${SWIG_EXECUTABLE}")
message(STATUS "MFEM: ${MFEM_INCLUDE_DIRS}")

include(cmake/SwigGuile.cmake)

# Serial modules
add_guile_mfem_module(version
  SWIG_FILE mfem/_ser/version.i
  DEPENDS mfem)

add_guile_mfem_module(mem_manager
  SWIG_FILE mfem/_ser/mem_manager.i
  DEPENDS mfem)

add_guile_mfem_module(array
  SWIG_FILE mfem/_ser/array.i
  DEPENDS mfem)

add_guile_mfem_module(vector
  SWIG_FILE mfem/_ser/vector.i
  DEPENDS mfem)

# Tests
enable_testing()
add_test(
  NAME test-version
  COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
    "${CMAKE_CURRENT_SOURCE_DIR}/test/unit/test-version.scm")
set_tests_properties(test-version PROPERTIES
  LABELS "serial"
  ENVIRONMENT "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR}")

add_test(
  NAME test-vector
  COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
    "${CMAKE_CURRENT_SOURCE_DIR}/test/unit/test-vector.scm")
set_tests_properties(test-vector PROPERTIES
  LABELS "serial"
  ENVIRONMENT "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR}")
