# FindMFEM.cmake
#
# Find the MFEM finite element library.
#
# Users can set MFEM_DIR to point to an MFEM install or build directory.
#
# Sets:
#   MFEM_FOUND
#   MFEM_INCLUDE_DIRS
#   MFEM_LIBRARIES

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
