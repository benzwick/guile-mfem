# ADR-0008: Self-Loading Proxy Modules and `(mfem)` Umbrella

## Status

Accepted

## Context

Each SWIG-generated proxy `.scm` file defines a Guile module with GOOPS class
definitions and generic function methods, but it does not load the corresponding
C extension (`.so`). Users and examples must manually call
`(load-extension "vector" "scm_init_vector_module")` for every module before
`(use-modules ...)` works. This creates fragile, order-dependent boilerplate
that grows with every module added.

Additionally, SWIG generates flat module names like `(vector)` and `(mesh)`,
which will collide with other Guile libraries.

## Decision

### Module namespace

All proxy modules live under the `(mfem ...)` namespace: `(mfem vector)`,
`(mfem mesh)`, etc. `PostProcessProxy.cmake` rewrites the SWIG-generated
`(define-module (vector))` to `(define-module (mfem vector))`, and the `.scm`
files are placed in a `mfem/` subdirectory of the build directory. The
`%insert("goops")` directives in `.i` files use `(mfem ...)` names for
cross-module GOOPS class imports.

The C extensions (`.so` files) and their primitive modules
(e.g., `(vector-primitive)`) remain flat — they are internal implementation
details never referenced by user code.

### Self-loading proxy modules

`PostProcessProxy.cmake` inserts a `(load-extension ...)` call into each
generated proxy `.scm` file, immediately after the `(define-module ...)` form.
The module name is extracted from the existing regex match. This makes each
proxy module self-contained: `(use-modules (mfem vector))` loads the C
extension automatically.

### `(mfem)` umbrella module

A hand-written `swig/guile/mfem.scm` defines `(mfem)` as a single entry point
that re-exports all available modules with merged generics. Users write:

    (use-modules (mfem))

instead of listing individual modules. This mirrors the C++ convention where
`#include "mfem.hpp"` pulls in the entire library.

There is a single `(mfem)` module (not `(mfem ser)` / `(mfem par)`). The build
configuration determines which modules are available, just as the C++ `mfem.hpp`
header includes parallel headers only when MFEM is built with MPI.

Individual modules can still be cherry-picked for faster load times:

    (use-modules (mfem vector) (mfem mesh))

## Consequences

- Examples and tests become concise: no `load-extension` boilerplate.
- `(use-modules (mfem))` is the recommended entry point for new users.
- Adding a new module requires adding it to the umbrella `mfem.scm`.
- The `load-extension` init function name must follow SWIG's convention
  (`scm_init_<name>_module`).
