# Known Bugs

## SWIG Guile proxy segfault on linalg/operator.hpp

**Affects:** SWIG 4.3.0, 4.5.0 (and likely all current versions)

**Symptom:** SWIG segfaults when `-proxy -emit-setters -scmstub` flags are used
on modules that `%include` or `%import` `linalg/operator.hpp`.

**Affected modules:** operators, matrix, densemat, sparsemat

**Workaround:** Build these modules without proxy flags (`NO_PROXY` option in
`SwigGuile.cmake`). Add explicit `%extend` wrappers for inherited `Operator`
methods (`Height()`, `Width()`) on derived types that need them.

**Impact:** Without proxy, SWIG does not generate GOOPS class hierarchy, so
C++ inheritance is not reflected in Guile. Each derived type must explicitly
re-export any base class methods it needs.
