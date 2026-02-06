# SwigGuile.cmake
#
# Provides add_guile_mfem_module() since CMake's UseSWIG does not support Guile.
#
# Usage:
#   add_guile_mfem_module(version
#     SWIG_FILE mfem/_ser/version.i
#     DEPENDS mfem)

function(add_guile_mfem_module name)
  cmake_parse_arguments(ARG "" "SWIG_FILE" "DEPENDS;SWIG_FLAGS" ${ARGN})

  if(NOT ARG_SWIG_FILE)
    message(FATAL_ERROR "add_guile_mfem_module: SWIG_FILE is required")
  endif()

  set(_wrap_cxx "${CMAKE_CURRENT_BINARY_DIR}/${name}_wrap.cxx")
  set(_scm_stub "${CMAKE_CURRENT_BINARY_DIR}/${name}.scm")

  # Run SWIG to generate wrapper
  add_custom_command(
    OUTPUT "${_wrap_cxx}" "${_scm_stub}"
    COMMAND ${SWIG_EXECUTABLE}
      -c++ -guile -scmstub
      -Linkage module
      -proxy -emit-setters
      -DSWIG_TYPE_TABLE=GuileMFEM
      -I${MFEM_INCLUDE_DIRS}
      ${ARG_SWIG_FLAGS}
      -module ${name}
      -o "${_wrap_cxx}"
      -outdir "${CMAKE_CURRENT_BINARY_DIR}"
      "${CMAKE_CURRENT_SOURCE_DIR}/${ARG_SWIG_FILE}"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${ARG_SWIG_FILE}"
    COMMENT "SWIG Guile: ${ARG_SWIG_FILE} -> ${name}_wrap.cxx"
    VERBATIM
  )

  # Build shared library from generated wrapper
  add_library(${name} MODULE "${_wrap_cxx}")
  target_include_directories(${name} PRIVATE
    ${GUILE_INCLUDE_DIRS}
    ${MFEM_INCLUDE_DIRS})
  target_link_libraries(${name} PRIVATE
    ${GUILE_LIBRARIES}
    ${ARG_DEPENDS})

  # Guile expects no "lib" prefix
  set_target_properties(${name} PROPERTIES
    PREFIX ""
    OUTPUT_NAME "${name}")

endfunction()
