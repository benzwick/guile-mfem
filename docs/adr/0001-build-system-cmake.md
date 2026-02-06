# ADR-0001: Use CMake as Build System

## Status

Accepted

## Context

PyMFEM uses `setup.py` (setuptools) to build MFEM from source, run SWIG, and
compile Python extension modules. MFEM itself uses CMake. CMake's built-in
`UseSWIG` module does not support Guile as a target language.

## Decision

Use CMake with a custom `add_guile_mfem_module()` function
(in `cmake/SwigGuile.cmake`) that invokes SWIG for Guile and compiles the
resulting wrapper into a shared library.

CMake options (`GUILE_MFEM_USE_MPI`, etc.) mirror the build flags that
PyMFEM's `setup.py` accepted.

## Consequences

- Aligns with MFEM's own build system.
- Requires maintaining a custom SWIG/Guile CMake function.
- CI workflows use `cmake -B build && cmake --build` instead of `pip install`.
