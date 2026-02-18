# ADR-0005: GOOPS Proxy Classes for OO Interface

## Status

Accepted

## Context

MFEM has a deep C++ class hierarchy. SWIG's Guile backend can generate GOOPS
(GNU Object-Oriented Programming System) class definitions that mirror the C++
inheritance tree. Multiple SWIG modules must share type information so that,
e.g., a `<Vector>` created in one module can be passed to a method defined in
another.

## Decision

### Proxy mode

All modules are compiled with `-proxy -scmstub -emit-setters`. This produces
both a `.so` (compiled C++ wrapper) and a `.scm` (GOOPS class definitions and
generic function methods) per module.

### Cross-module type sharing

`-DSWIG_TYPE_TABLE=GuileMFEM` is passed to both SWIG (via `-D`) and the C++
compiler (via `target_compile_definitions`) so that all modules register their
types in a single shared table.

### Cross-module GOOPS dispatch

When module B imports a generic function `Mult` from module A and then adds
new methods via `define-method`, Guile's default duplicate handler raises
warnings and can shadow the imported generic. We follow the G-Golf convention:

1. **`#:duplicates (merge-generics replace warn-override-core warn last)`** on
   every `define-module` — tells Guile to merge imported generics instead of
   shadowing them.

2. **`swig-export!`** (defined in `swig/guile/common.scm`) replaces Guile's
   `export` macro. Unlike `export`, which calls `module-ensure-local-variable!`
   (creating `#<undefined>` locals that shadow imported generics), `swig-export!`
   uses `module-variable` to find the existing binding and adds it directly to
   the public interface.

3. **`define-method/safe`** replaces `define-method` in generated code. It wraps
   the definition in `false-if-exception`, so methods that reference GOOPS
   classes from optional/unbuilt modules are silently skipped instead of causing
   an unbound-variable error.

`PostProcessProxy.cmake` applies all three transformations to SWIG-generated
`.scm` files as a post-processing step.

### Cross-module GOOPS class imports

SWIG `.i` files use `%insert("goops")` to inject `(use-modules ...)` forms
into the generated `.scm`, importing GOOPS classes from dependency modules.
For example, `densemat.i` contains:

    %insert("goops") %{(use-modules (array) (vector) (operators) (matrix))%}

This ensures that when the `densemat` proxy module loads, the GOOPS classes it
inherits from (e.g., `<Operator>` from `(operators)`) are already defined.

### Auto-compilation disabled

`GUILE_AUTO_COMPILE=0` is set in the test environment because Guile's
auto-compiler cannot compile the proxy `.scm` files in isolation — they depend
on the `-primitive` C modules which are only available at runtime via
`load-extension`.

### Ownership transfer

PyMFEM uses `%pythonprepend` to call `_setPermanent(bfi)` before passing
integrators to forms (transferring ownership to C++). For Guile, we use SWIG's
`DISOWN` typemap (`%apply SWIGTYPE *DISOWN { ... }`) which is language-agnostic
and tells SWIG to clear the prevent-GC flag on the proxy object.

## Consequences

- Idiomatic Guile OO interface with generic functions and slot accessors.
- `PostProcessProxy.cmake` must be maintained alongside SWIG upgrades.
- New modules must add `%insert("goops")` with their dependency imports.
- `SWIG_TYPE_TABLE=GuileMFEM` must appear in both SWIG and compiler flags.
