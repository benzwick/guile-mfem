# ADR-0002: Fork PyMFEM and Adapt for Guile

## Status

Accepted

## Context

PyMFEM provides mature SWIG `.i` files wrapping MFEM's C++ API, CI workflows
that build MFEM from source, and test infrastructure. Writing these from
scratch for Guile would duplicate significant effort.

## Decision

Fork PyMFEM and adapt in-place. The PyMFEM upstream is kept as a read-only
submodule at `_reference/PyMFEM` for diffing. The `pymfem` git remote points
to the original repository.

SWIG `.i` files are modified to target Guile: Python-specific typemaps,
`%pythoncode`, `%pythonappend`, and NumPy references are replaced with Guile
equivalents.

## Consequences

- Fast bootstrap: CI, `.i` files, and project structure inherited from PyMFEM.
- Easy to diff adapted `.i` files against PyMFEM originals.
- Must track PyMFEM upstream changes for `.i` files we haven't yet adapted.
