# SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
# SPDX-License-Identifier: BSD-3-Clause

# SwigGuile.cmake
#
# Provides add_guile_mfem_module() since CMake's UseSWIG does not support Guile.
#
# Usage:
#   add_guile_mfem_module(version
#     SWIG_FILE mfem/_ser/version.i
#     DEPENDS mfem)

# MFEM's MFEMConfig.cmake sets MFEM_INCLUDE_DIRS to just the build dir,
# but SWIG and the compiler also need the source dir for headers like
# general/vector.hpp. The imported mfem target's INTERFACE_INCLUDE_DIRECTORIES
# has both (build-tree) or the install prefix (installed layout).
get_target_property(_SWIG_MFEM_INCDIRS mfem INTERFACE_INCLUDE_DIRECTORIES)
message(STATUS "MFEM include dirs (from target): ${_SWIG_MFEM_INCDIRS}")

# In the installed layout, headers live under <prefix>/include/mfem/
# (e.g. include/mfem/general/version.hpp) but SWIG .i files reference
# them without the mfem/ prefix (e.g. %include "general/version.hpp").
# Add the mfem/ subdirectory to the include path when it exists.
foreach(_dir IN LISTS _SWIG_MFEM_INCDIRS)
  if(IS_DIRECTORY "${_dir}/mfem")
    list(APPEND _SWIG_MFEM_INCDIRS "${_dir}/mfem")
  endif()
endforeach()

list(REMOVE_DUPLICATES _SWIG_MFEM_INCDIRS)
message(STATUS "SWIG include dirs: ${_SWIG_MFEM_INCDIRS}")

# Locate _config.hpp for the MFEM_CONFIG_FILE compile definition.
# In the build tree it is at <build>/config/_config.hpp; in the
# installed layout it is at <prefix>/include/mfem/config/_config.hpp.
find_file(_MFEM_CONFIG_HPP _config.hpp
  HINTS ${_SWIG_MFEM_INCDIRS}
  PATH_SUFFIXES config
  NO_DEFAULT_PATH)
message(STATUS "MFEM _config.hpp: ${_MFEM_CONFIG_HPP}")

# SWIG GOOPS proxy modules require (Swig common).  Copy our custom version
# (with swig-export! and positional constructors) into the build directory
# so Guile can find it via -L ${CMAKE_CURRENT_BINARY_DIR}.
file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/swig/guile/common.scm"
  DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/Swig")

# Proxy .scm files live under mfem/ so modules are (mfem vector) etc.
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/mfem")

# (mfem) umbrella module re-exports all individual modules.
file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/swig/guile/mfem.scm"
  DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")

function(add_guile_mfem_module name)
  cmake_parse_arguments(ARG "" "SWIG_FILE" "DEPENDS;SWIG_FLAGS" ${ARGN})

  if(NOT ARG_SWIG_FILE)
    message(FATAL_ERROR "add_guile_mfem_module: SWIG_FILE is required")
  endif()

  set(_wrap_cxx "${CMAKE_CURRENT_BINARY_DIR}/${name}_wrap.cxx")
  set(_scm_stub "${CMAKE_CURRENT_BINARY_DIR}/mfem/${name}.scm")

  # Build SWIG -I flags from include dirs
  set(_swig_inc_flags "")
  foreach(_dir IN LISTS _SWIG_MFEM_INCDIRS)
    list(APPEND _swig_inc_flags "-I${_dir}")
  endforeach()

  # SWIG Guile proxy flags: -scmstub -proxy -emit-setters generate a
  # Scheme proxy module with GOOPS class hierarchy.
  set(_proxy_flags -scmstub -proxy -emit-setters)
  set(_outputs "${_wrap_cxx}" "${_scm_stub}")

  # Run SWIG to generate wrapper
  add_custom_command(
    OUTPUT ${_outputs}
    COMMAND ${CMAKE_COMMAND} -E env "SWIG_LIB=${SWIG_DIR}"
      ${SWIG_EXECUTABLE}
      -c++ -guile
      -Linkage module
      ${_proxy_flags}
      -DSWIG_TYPE_TABLE=GuileMFEM
      -DMFEM_DEPRECATED=
      ${_swig_inc_flags}
      ${ARG_SWIG_FLAGS}
      -module ${name}
      -o "${_wrap_cxx}"
      -outdir "${CMAKE_CURRENT_BINARY_DIR}/mfem"
      "${CMAKE_CURRENT_SOURCE_DIR}/${ARG_SWIG_FILE}"
    COMMAND ${CMAKE_COMMAND}
      "-DSCM_FILE=${_scm_stub}"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/PostProcessProxy.cmake"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${ARG_SWIG_FILE}"
    COMMENT "SWIG Guile: ${ARG_SWIG_FILE} -> ${name}_wrap.cxx"
    VERBATIM
  )

  # Build shared library from generated wrapper
  add_library(${name} MODULE "${_wrap_cxx}")
  target_include_directories(${name} PRIVATE
    ${GUILE_INCLUDE_DIRS}
    ${_SWIG_MFEM_INCDIRS})
  # Point MFEM's config.hpp at the generated _config.hpp.
  target_compile_definitions(${name} PRIVATE
    "MFEM_CONFIG_FILE=\"${_MFEM_CONFIG_HPP}\""
    "SWIG_TYPE_TABLE=GuileMFEM")
  target_link_libraries(${name} PRIVATE
    ${GUILE_LIBRARIES}
    ${ARG_DEPENDS})

  # Guile expects no "lib" prefix
  set_target_properties(${name} PROPERTIES
    PREFIX ""
    OUTPUT_NAME "${name}")

endfunction()
