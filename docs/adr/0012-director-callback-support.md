# ADR-0012: SWIG Director / Callback Support for User-Defined Coefficients

## Status

Accepted

## Context

MFEM examples 3–40 and most real-world simulations require user-defined
coefficient functions that depend on spatial coordinates. In C++, these are
expressed as:

- `FunctionCoefficient(std::function<double(const Vector&)>)` — scalar
- `VectorFunctionCoefficient(int vdim, std::function<void(const Vector&, Vector&)>)` — vector
- Subclasses of `Coefficient`, `VectorCoefficient`, `MatrixCoefficient` with
  overridden `Eval()` methods

In PyMFEM, these are handled via SWIG **directors** — a mechanism that lets
Python subclasses override C++ virtual methods. PyMFEM provides convenience
classes (`PyCoefficient`, `VectorPyCoefficient`) and also supports Numba JIT
compilation of coefficient callbacks.

In guile-mfem, SWIG director support for the Guile target is not yet available.
The SWIG Guile backend does not implement the director infrastructure that would
allow Scheme procedures to be called from C++ virtual method dispatch. This
means:

1. **Cannot construct `FunctionCoefficient`** — the constructor takes a C++
   function pointer or `std::function`, neither of which can be created from a
   Scheme procedure without director support or custom typemaps.

2. **Cannot subclass `Coefficient`** — GOOPS subclassing of SWIG-wrapped
   classes does not dispatch to C++ virtual methods without directors.

3. **Cannot compute L² error** — `ComputeL2Error` requires a coefficient
   representing the exact solution, which must be user-defined.

### Impact on example porting

| Example | Blocked Feature | Workaround |
|---------|----------------|------------|
| ex0–ex2 | None | Fully ported |
| ex3 | `VectorFunctionCoefficient` for E_exact, f_exact | Constant vector RHS |
| ex4 | `VectorFunctionCoefficient` for F_exact, f_exact | Constant vector RHS |
| ex5 | `FunctionCoefficient`, `VectorFunctionCoefficient` | Not yet ported |
| ex6–ex40 | Various `FunctionCoefficient` uses | Not yet ported |

### Possible approaches

1. **SWIG directors for Guile** — Implement the director infrastructure in
   SWIG's Guile backend. This is the most complete solution but requires
   significant SWIG development.

2. **Custom typemaps for `std::function`** — Write SWIG typemaps that wrap
   Guile procedures into `std::function` objects via C shims. This would
   enable `FunctionCoefficient` and `VectorFunctionCoefficient` without full
   director support.

3. **C-coded Scheme callbacks** — Define a small C library that bridges Guile
   `SCM` procedures to C function pointers using `scm_call_*`. Register
   these as coefficient callbacks. This avoids modifying SWIG but requires
   a separate C helper library.

4. **Precompiled coefficient library** — Provide a library of common
   coefficient functions (sin, cos, exp compositions) as C++ classes wrapped
   via SWIG. Users select from predefined functions rather than writing
   callbacks. Limited but requires no SWIG changes.

## Decision

Acknowledge the blocker and document it. Port examples 3+ with simplified
constant coefficients where possible, clearly noting the limitation. The
examples still demonstrate the finite element API (element types, integrators,
assembly, solve) even without exact error computation.

Pursue approach 2 (custom typemaps for `std::function`) as the near-term fix,
since it enables the most common use case without requiring full director
support. Track progress in BUGS.md.

## Consequences

- Examples 3 and 4 are partially ported — they demonstrate ND/RT finite
  elements, curl-curl/div-div integrators, and the solve workflow, but use
  simplified constant RHS instead of the exact-solution-derived RHS.

- L² error computation against exact solutions is not possible until callback
  support is added.

- Examples 5–40 remain unported. Each has specific coefficient requirements
  documented in this ADR.

- Users who need custom coefficients must currently implement them in C++ and
  wrap via SWIG, which defeats much of the purpose of Scheme bindings.

- Once approach 2 is implemented, examples 3–4 can be upgraded to use the
  exact solutions, and examples 5+ can be ported incrementally.
