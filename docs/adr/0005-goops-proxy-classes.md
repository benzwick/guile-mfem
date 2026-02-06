# ADR-0005: GOOPS Proxy Classes for OO Interface

## Status

Accepted

## Context

MFEM has a deep C++ class hierarchy. SWIG's Guile backend can generate GOOPS
(GNU Object-Oriented Programming System) class definitions that mirror the C++
inheritance tree, using `-proxy -emit-setters`.

## Decision

Use SWIG's `-proxy -emit-setters` flags to generate GOOPS classes. Each module
produces both a `.so` (compiled wrapper) and a `.scm` (GOOPS definitions).

Users load modules with `(use-modules (mfem ser vector))`.

## Consequences

- Idiomatic Guile OO interface with generic functions and slot accessors.
- GOOPS declaration order must match C++ class hierarchy.
- `-DSWIG_TYPE_TABLE=GuileMFEM` required on all modules for cross-module
  type sharing.
