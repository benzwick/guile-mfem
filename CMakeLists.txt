# SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
# SPDX-License-Identifier: BSD-3-Clause

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

add_guile_mfem_module(operators
  SWIG_FILE mfem/_ser/operators.i
  DEPENDS mfem)

add_guile_mfem_module(matrix
  SWIG_FILE mfem/_ser/matrix.i
  DEPENDS mfem)

add_guile_mfem_module(globals
  SWIG_FILE mfem/_ser/globals.i
  DEPENDS mfem)

add_guile_mfem_module(densemat
  SWIG_FILE mfem/_ser/densemat.i
  DEPENDS mfem)

add_guile_mfem_module(sparsemat
  SWIG_FILE mfem/_ser/sparsemat.i
  DEPENDS mfem)

add_guile_mfem_module(mesh
  SWIG_FILE mfem/_ser/mesh.i
  DEPENDS mfem)

add_guile_mfem_module(fe_coll
  SWIG_FILE mfem/_ser/fe_coll.i
  DEPENDS mfem)

add_guile_mfem_module(intrules
  SWIG_FILE mfem/_ser/intrules.i
  DEPENDS mfem)

add_guile_mfem_module(eltrans
  SWIG_FILE mfem/_ser/eltrans.i
  DEPENDS mfem)

add_guile_mfem_module(fe
  SWIG_FILE mfem/_ser/fe.i
  DEPENDS mfem)

add_guile_mfem_module(symmat
  SWIG_FILE mfem/_ser/symmat.i
  DEPENDS mfem)

add_guile_mfem_module(integrator
  SWIG_FILE mfem/_ser/integrator.i
  DEPENDS mfem)

add_guile_mfem_module(nonlininteg
  SWIG_FILE mfem/_ser/nonlininteg.i
  DEPENDS mfem)

add_guile_mfem_module(coefficient
  SWIG_FILE mfem/_ser/coefficient.i
  DEPENDS mfem)

add_guile_mfem_module(fespace
  SWIG_FILE mfem/_ser/fespace.i
  DEPENDS mfem)

add_guile_mfem_module(gridfunc
  SWIG_FILE mfem/_ser/gridfunc.i
  DEPENDS mfem)

add_guile_mfem_module(lininteg
  SWIG_FILE mfem/_ser/lininteg.i
  DEPENDS mfem)

add_guile_mfem_module(bilininteg
  SWIG_FILE mfem/_ser/bilininteg.i
  DEPENDS mfem)

add_guile_mfem_module(linearform
  SWIG_FILE mfem/_ser/linearform.i
  DEPENDS mfem)

add_guile_mfem_module(handle
  SWIG_FILE mfem/_ser/handle.i
  DEPENDS mfem)

add_guile_mfem_module(bilinearform
  SWIG_FILE mfem/_ser/bilinearform.i
  DEPENDS mfem)

add_guile_mfem_module(solvers
  SWIG_FILE mfem/_ser/solvers.i
  DEPENDS mfem)

add_guile_mfem_module(sparsesmoothers
  SWIG_FILE mfem/_ser/sparsesmoothers.i
  DEPENDS mfem)

add_guile_mfem_module(device
  SWIG_FILE mfem/_ser/device.i
  DEPENDS mfem)

# Tests
enable_testing()
add_test(
  NAME test-version
  COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
    "${CMAKE_CURRENT_SOURCE_DIR}/test/unit/test-version.scm")
set_tests_properties(test-version PROPERTIES
  LABELS "serial;unit"
  ENVIRONMENT "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR};GUILE_AUTO_COMPILE=0")

add_test(
  NAME test-vector
  COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
    "${CMAKE_CURRENT_SOURCE_DIR}/test/unit/test-vector.scm")
set_tests_properties(test-vector PROPERTIES
  LABELS "serial;unit"
  ENVIRONMENT "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR};GUILE_AUTO_COMPILE=0")

add_test(
  NAME test-densemat
  COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
    "${CMAKE_CURRENT_SOURCE_DIR}/test/unit/test-densemat.scm")
set_tests_properties(test-densemat PROPERTIES
  LABELS "serial;unit"
  ENVIRONMENT "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR};GUILE_AUTO_COMPILE=0")

add_test(
  NAME test-sparsemat
  COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
    "${CMAKE_CURRENT_SOURCE_DIR}/test/unit/test-sparsemat.scm")
set_tests_properties(test-sparsemat PROPERTIES
  LABELS "serial;unit"
  ENVIRONMENT "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR};GUILE_AUTO_COMPILE=0")

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
  LABELS "serial;unit"
  ENVIRONMENT "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR};GUILE_AUTO_COMPILE=0;MFEM_DATA_DIR=${MFEM_DATA_DIR}")

# Example tests — fast suite (default mesh only)
set(EXAMPLE_ENV "LTDL_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR};GUILE_AUTO_COMPILE=0;MFEM_DATA_DIR=${MFEM_DATA_DIR}")

foreach(ex ex0 ex0-lisp ex1 ex1-lisp ex2 ex2-lisp)
  add_test(
    NAME examples-fast-${ex}
    COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
      -L ${CMAKE_CURRENT_SOURCE_DIR}
      "${CMAKE_CURRENT_SOURCE_DIR}/test/examples/test-${ex}.scm")
  set_tests_properties(examples-fast-${ex} PROPERTIES
    LABELS "serial;examples-fast"
    ENVIRONMENT "${EXAMPLE_ENV};TEST_MODE=fast")
endforeach()

# Example tests — full suite (all sample runs from example headers)
foreach(ex ex0 ex0-lisp)
  add_test(
    NAME examples-full-${ex}
    COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
      -L ${CMAKE_CURRENT_SOURCE_DIR}
      "${CMAKE_CURRENT_SOURCE_DIR}/test/examples/test-${ex}.scm")
  set_tests_properties(examples-full-${ex} PROPERTIES
    LABELS "serial;examples-full"
    ENVIRONMENT "${EXAMPLE_ENV};TEST_MODE=full"
    TIMEOUT 300)
endforeach()

foreach(ex ex1 ex1-lisp ex2 ex2-lisp)
  add_test(
    NAME examples-full-${ex}
    COMMAND ${GUILE_EXECUTABLE} -L ${CMAKE_CURRENT_BINARY_DIR}
      -L ${CMAKE_CURRENT_SOURCE_DIR}
      "${CMAKE_CURRENT_SOURCE_DIR}/test/examples/test-${ex}.scm")
  set_tests_properties(examples-full-${ex} PROPERTIES
    LABELS "serial;examples-full"
    ENVIRONMENT "${EXAMPLE_ENV};TEST_MODE=full"
    TIMEOUT 600)
endforeach()
