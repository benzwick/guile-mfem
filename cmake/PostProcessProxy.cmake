# PostProcessProxy.cmake
#
# Post-processes a SWIG-generated Guile proxy .scm file to fix
# cross-module GOOPS generic function dispatch.  Two changes:
#
# 1. Add #:duplicates (merge-generics replace warn-override-core warn last)
#    to the (define-module ...) form, following the G-Golf convention.
#    This merges generic functions imported from multiple modules.
#
# 2. Replace the (export ...) macro with a call to swig-export!
#    (defined in swig/guile/common.scm).  Unlike Guile's export,
#    swig-export! uses module-variable (which searches imports) +
#    module-add! to the public interface, avoiding the creation of
#    #<undefined> locals that shadow imported generics.
#
# Usage: cmake -DSCM_FILE=path/to/module.scm -P PostProcessProxy.cmake

if(NOT SCM_FILE)
  message(FATAL_ERROR "PostProcessProxy: SCM_FILE not set")
endif()

if(NOT EXISTS "${SCM_FILE}")
  return()
endif()

file(READ "${SCM_FILE}" _content)

# 1. Add #:duplicates to (define-module ...)
#    SWIG generates:  (define-module (name))
#    We change it to: (define-module (name)
#                       #:duplicates (merge-generics replace warn-override-core warn last))
string(REGEX MATCH "\\(define-module \\([^)]+\\)\\)" _dm_form "${_content}")
if(_dm_form)
  string(REGEX REPLACE "\\)\\)$"
    ")\n  #:duplicates (merge-generics replace warn-override-core warn last))"
    _dm_fixed "${_dm_form}")
  string(REPLACE "${_dm_form}" "${_dm_fixed}" _content "${_content}")
endif()

# 2. Replace (export ...) with (swig-export! (current-module) '(...))
string(REGEX MATCH "\\(export [^)]+\\)" _export_form "${_content}")
if(_export_form)
  # Extract the exported names
  string(REGEX REPLACE "^\\(export " "" _names "${_export_form}")
  string(REGEX REPLACE "\\)$" "" _names "${_names}")

  set(_fix "(swig-export! (current-module) '(${_names}))")
  string(REPLACE "${_export_form}" "${_fix}" _content "${_content}")
endif()

# 3. Replace (define-method ...) with (define-method/safe ...)
#    so methods referencing GOOPS classes from unbuilt modules are
#    silently skipped instead of causing an unbound-variable error.
string(REPLACE "(define-method " "(define-method/safe " _content "${_content}")

file(WRITE "${SCM_FILE}" "${_content}")
