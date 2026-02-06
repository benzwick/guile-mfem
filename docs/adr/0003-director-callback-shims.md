# ADR-0003: C++ Callback Shims Instead of SWIG Directors

## Status

Accepted

## Context

SWIG's director feature (allowing target-language subclasses to override C++
virtual methods) has no support for Guile. PyMFEM relies on directors for
user-defined coefficients, integrators, and other callback-driven classes.

## Decision

Hand-write C++ shim classes (e.g., `GuileCoefficient`) that accept Guile `SCM`
procedures and call them from virtual method overrides. These live in `src/`.

This is more idiomatic for Scheme, where closures are preferred over
subclassing.

## Consequences

- Each callback-driven MFEM class needs a hand-written shim.
- More natural Scheme API: pass a lambda instead of defining a subclass.
- GC integration required: `scm_gc_protect_object` / `scm_gc_unprotect_object`.
