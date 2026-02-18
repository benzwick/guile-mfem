# ADR-0007: Build SWIG from Forked Source Submodule

## Status

Accepted

## Context

SWIG's Guile proxy support (`-proxy -scmstub -emit-setters`) has two bugs that
affect guile-mfem:

1. **Segfault on public member variables** (upstream [#3336][]): generated code
   dereferences a null pointer when wrapping public member variables in proxy
   mode.

2. **Cross-module type cast lookup failure** (upstream [#3337][]): in
   `swiginit.swg`, when adding casts to an existing type, `cast->type` is not
   updated to the shared type pointer, causing cross-module type lookups to
   fail silently.

System-packaged SWIG does not include fixes for either bug.

[#3336]: https://github.com/swig/swig/pull/3336
[#3337]: https://github.com/swig/swig/pull/3337

## Decision

Add a `_reference/swig` git submodule pointing to the `benzwick/swig` fork,
which includes fixes for both bugs. CI and `build.sh` build SWIG from source
and pass `SWIG_EXECUTABLE` and `SWIG_DIR` to CMake.

Once the upstream PRs merge and a SWIG release includes the fixes, switch the
submodule URL back to `https://github.com/swig/swig.git` or remove the
submodule in favor of a system package.

## Consequences

- CI requires `bison` and `libpcre2-dev` build dependencies instead of a
  `swig` package.
- Build time increases by the time to compile SWIG from source.
- Fixes are tracked in a public fork and submitted upstream, minimizing
  long-term maintenance.
