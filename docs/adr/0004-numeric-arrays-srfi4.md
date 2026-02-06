# ADR-0004: Use SRFI-4 Uniform Vectors for Numeric Arrays

## Status

Accepted

## Context

PyMFEM uses NumPy arrays for numeric data exchange between Python and MFEM.
Guile has no NumPy equivalent, but provides SRFI-4 uniform vectors
(`f64vector`, `s32vector`, etc.) with contiguous memory and a C API.

## Decision

Use SRFI-4 `f64vector` for `double*` data and `s32vector` for `int*` data.
SWIG typemaps in `swig/guile/guile_typemaps.i` convert between SRFI-4 vectors
and MFEM types (e.g., `f64vector` <-> `mfem::Vector`).

Access the underlying C array via `scm_f64vector_elements()` /
`scm_array_handle_release()`.

## Consequences

- Zero-copy possible when MFEM doesn't take ownership.
- Familiar to Guile users (SRFI-4 is widely used).
- Less feature-rich than NumPy (no slicing, broadcasting), but sufficient for
  FEM data transfer.
