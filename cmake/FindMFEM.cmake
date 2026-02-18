# SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
# SPDX-License-Identifier: BSD-3-Clause

# FindMFEM.cmake
#
# Find the MFEM finite element library.
#
# Users can set MFEM_DIR to point to an MFEM install or build directory.
#
# If MFEM_DIR contains MFEMConfig.cmake (build tree or install prefix),
# we delegate to MFEM's own package config which sets up the imported
# target with correct include directories.  Otherwise fall back to
# find_path / find_library.
#
# Sets:
#   MFEM_FOUND
#   MFEM_INCLUDE_DIRS
#   MFEM_LIBRARIES
#   mfem (imported target)

# Try config-mode first: look for MFEMConfig.cmake in MFEM_DIR
find_package(MFEM CONFIG
  HINTS ${MFEM_DIR}
  NO_DEFAULT_PATH)

if(MFEM_FOUND)
  # MFEM's own config was found — target "mfem" and all variables are set.
  return()
endif()

# Fallback: manual search
find_path(MFEM_INCLUDE_DIRS mfem.hpp
  HINTS ${MFEM_DIR} ${MFEM_DIR}/include
  PATH_SUFFIXES include)

find_library(MFEM_LIBRARIES mfem
  HINTS ${MFEM_DIR} ${MFEM_DIR}/lib
  PATH_SUFFIXES lib lib64)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MFEM
  REQUIRED_VARS MFEM_LIBRARIES MFEM_INCLUDE_DIRS)

if(MFEM_FOUND AND NOT TARGET mfem)
  add_library(mfem UNKNOWN IMPORTED)
  set_target_properties(mfem PROPERTIES
    IMPORTED_LOCATION "${MFEM_LIBRARIES}"
    INTERFACE_INCLUDE_DIRECTORIES "${MFEM_INCLUDE_DIRS}")
endif()
