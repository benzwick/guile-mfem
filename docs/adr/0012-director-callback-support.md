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

| Example | Status | Blocker |
|---------|--------|---------|
| ex0 | Fully ported | — |
| ex1 | Fully ported | NURBS path blocked by null-pointer truthiness bug |
| ex2 | Fully ported | NURBS path blocked by null-pointer truthiness bug |
| ex3 | Partially ported | `VectorFunctionCoefficient` for E_exact, f_exact → constant RHS |
| ex4 | Partially ported | `VectorFunctionCoefficient` for F_exact, f_exact → constant RHS |
| ex5 | Not ported | `FunctionCoefficient` callbacks + `BlockVector`/`BlockOperator` modules |
| ex6 | Not ported | `ZienkiewiczZhuEstimator`, `ThresholdRefiner` (estimators module) |
| ex7 | Not ported | Custom `PyCoefficient` (analytic_rhs/solution) + `SnapNodes` mesh manipulation |
| ex8 | Not ported | `BlockOperator`, `BlockVector` modules |
| ex9 | Not ported | `PyTimeDependentOperator`, `VectorPyCoefficient` (director) |
| ex10 | Not ported | `PyTimeDependentOperator`, nonlinear callbacks |
| ex14 | Fully ported | — (uses only `ConstantCoefficient`, DG methods) |
| ex15–ex19 | Not ported | `PyTimeDependentOperator` or `VectorPyCoefficient` |
| ex20 | Not ported | Custom `Operator` subclass (director) |
| ex21 | Not ported | `ZienkiewiczZhuEstimator`, `ThresholdRefiner` (estimators module) |
| ex22 | Not ported | `ComplexOperator` module, `FunctionCoefficient` |
| ex23 | Not ported | `SecondOrderTimeDependentOperator` (ODE module) |
| ex24 | Not ported | `PyCoefficient`/`VectorPyCoefficient` + `DiscreteLinearOperator` interpolators |
| ex25 | Not ported | Numba JIT / `FunctionCoefficient` |
| ex26 | Not ported | `PyGeometricMultigrid` (multigrid module, director) |
| ex27 | Not ported | `VectorPyCoefficient` for coordinate transform |
| ex28 | Not ported | `BuildNormalConstraints`, `SchurConstrainedSolver` (constraints module) |
| ex29 | Not ported | `VectorPyCoefficient` for surface transform |
| ex30–ex40 | Not ported | Various: Numba JIT, custom operators, uncompiled modules |

### Blocker categories

1. **Director / callback support** (primary blocker): ex3*, ex4*, ex5, ex7,
   ex9–ex10, ex15–ex20, ex22, ex24–ex27, ex29–ex40
2. **Uncompiled SWIG modules**: ex5 (blockvector), ex6/ex21 (estimators),
   ex8 (blockoperator), ex22 (complex_operator), ex23 (ode),
   ex26 (multigrid), ex28 (constraints)
3. **Both**: ex5, ex9, ex10, ex22, ex26

### Near-portable candidates (detailed analysis)

Several examples were analyzed for partial porting (replacing custom
coefficients with `ConstantCoefficient`, like ex3/ex4):

- **ex7** (surface Laplacian on sphere): Uses only compiled modules
  (`H1_FECollection`, `DiffusionIntegrator`, `MassIntegrator`) but requires
  programmatic mesh construction (`AddVertex`, `AddTriangle`, `AddQuad`) with
  raw pointer arguments whose SWIG wrapping is uncertain. Also needs `SnapNodes`
  which manipulates nodal coordinates via `DofToVDof`. The custom coefficients
  (`analytic_rhs`, `analytic_solution`) could be replaced with
  `ConstantCoefficient`, but the mesh construction is the harder blocker.

- **ex24** (mixed finite elements): Uses `DiscreteLinearOperator` with
  `GradientInterpolator`, `CurlInterpolator`, `DivergenceInterpolator`. While
  `DiscreteLinearOperator` is auto-exposed from `bilinearform.hpp`, the
  interpolators are in `lininteg.hpp` and may not have proper SWIG typemaps for
  the `DiscreteInterpolator` base class. All coefficients are custom
  `PyCoefficient`/`VectorPyCoefficient` subclasses.

- **ex6** (adaptive mesh refinement): Uses only `ConstantCoefficient` but
  requires `ZienkiewiczZhuEstimator` and `ThresholdRefiner` from the uncompiled
  `estimators` module. Would be immediately portable once the estimators module
  is compiled.

- **ex9** (transient advection): Core DG advection algorithm uses standard
  integrators (`ConvectionIntegrator`, `DGTraceIntegrator`) but requires ODE
  solvers (uncompiled `ode` module) and custom coefficient callbacks.

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

- Example 14 is fully ported — DG methods use only `ConstantCoefficient`
  because boundary conditions are imposed weakly through face integrators.

- L² error computation against exact solutions is not possible until callback
  support is added.

- Of 40 serial examples, 4 are fully ported (ex0, ex1, ex2, ex14) and 2 are
  partially ported (ex3, ex4). The remaining 34 are blocked by either
  director/callback support (primary blocker) or uncompiled SWIG modules
  (secondary blocker). See the portability matrix above for details.

- The next examples to become portable would be: ex6 (once the estimators
  module is compiled), ex7 (once mesh construction wrapping is verified and
  constant coefficients substituted), and ex24 (once DiscreteLinearOperator
  interpolator wrapping is verified).

- Users who need custom coefficients must currently implement them in C++ and
  wrap via SWIG, which defeats much of the purpose of Scheme bindings.

- Once approach 2 is implemented, examples 3–4 can be upgraded to use the
  exact solutions, and examples 5+ can be ported incrementally.

- Compiling additional SWIG modules (blockoperator, blockvector, estimators,
  ode, multigrid, constraints, complex_operator) would unblock several more
  examples even before director support is available.
