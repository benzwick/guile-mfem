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

# SWIG Guile -proxy segfaults on linalg/operator.hpp (tested with
# SWIG 4.3.0 and 4.5.0).  Modules that %include or %import operator.hpp
# must use NO_PROXY until the SWIG bug is fixed.
add_guile_mfem_module(operators
  SWIG_FILE mfem/_ser/operators.i
  DEPENDS mfem
  NO_PROXY)

add_guile_mfem_module(matrix
  SWIG_FILE mfem/_ser/matrix.i
  DEPENDS mfem
  NO_PROXY)

add_guile_mfem_module(globals
  SWIG_FILE mfem/_ser/globals.i
  DEPENDS mfem)

add_guile_mfem_module(densemat
  SWIG_FILE mfem/_ser/densemat.i
  DEPENDS mfem
  NO_PROXY)

add_guile_mfem_module(sparsemat
  SWIG_FILE mfem/_ser/sparsemat.i
  DEPENDS mfem
  NO_PROXY)

add_guile_mfem_module(mesh
  SWIG_FILE mfem/_ser/mesh.i
  DEPENDS mfem
  NO_PROXY)

add_guile_mfem_module(fe_coll
  SWIG_FILE mfem/_ser/fe_coll.i
  DEPENDS mfem
  NO_PROXY)

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

add_test(
  NAME test-densemat
  COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
    "${CMAKE_CURRENT_SOURCE_DIR}/test/unit/test-densemat.scm")
set_tests_properties(test-densemat PROPERTIES
  LABELS "serial"
  ENVIRONMENT "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR}")

add_test(
  NAME test-sparsemat
  COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
    "${CMAKE_CURRENT_SOURCE_DIR}/test/unit/test-sparsemat.scm")
set_tests_properties(test-sparsemat PROPERTIES
  LABELS "serial"
  ENVIRONMENT "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR}")

add_test(
  NAME test-mesh
  COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
    "${CMAKE_CURRENT_SOURCE_DIR}/test/unit/test-mesh.scm")
# Find MFEM data directory for tests.
# _SWIG_MFEM_INCDIRS comes from the mfem target's INTERFACE_INCLUDE_DIRECTORIES.
# In the build tree the source dir (e.g. _reference/mfem) is listed; in an
# installed layout the include dir (e.g. mfem-install/include) is listed.
# Either way, data/ is a sibling under the same parent.
list(GET _SWIG_MFEM_INCDIRS 0 _incdir)
get_filename_component(MFEM_DATA_DIR "${_incdir}/../data" ABSOLUTE)
message(STATUS "MFEM data: ${MFEM_DATA_DIR}")
set_tests_properties(test-mesh PROPERTIES
  LABELS "serial"
  ENVIRONMENT "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR};MFEM_DATA_DIR=${MFEM_DATA_DIR}")
