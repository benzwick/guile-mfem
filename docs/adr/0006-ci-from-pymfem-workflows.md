# ADR-0006: CI Workflows Adapted from PyMFEM

## Status

Accepted

## Context

PyMFEM's GitHub Actions CI builds MFEM from source, builds SWIG wrappers, and
runs tests. This infrastructure is non-trivial and handles MFEM's external
dependencies (METIS, HYPRE), optional features (CUDA, libCEED, GSlib), and
matrix testing across branches.

## Decision

Fork PyMFEM's `.github/workflows/` and adapt for Guile with minimal changes:
- Replace `python-version` matrix with `guile-version`
- Replace `pip install` with `cmake` build
- Replace `python run_examples.py` with `ctest`
- Keep MFEM build options (parallel, CUDA, libCEED, GSlib)
- Keep MFEM branch matrix (master, pinned default)

## Consequences

- Inherits PyMFEM's battle-tested CI for building MFEM from source.
- Easy to diff our workflows against PyMFEM's originals.
- Changes to PyMFEM's CI can be reviewed and merged if applicable.
