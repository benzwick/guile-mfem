# ADR-0011: Comprehensive Test Coverage from MFEM and PyMFEM

## Status

Accepted

## Context

guile-mfem had 5 unit tests covering basic types (version, vector, densemat,
sparsemat, mesh). PyMFEM has ~30 test files; MFEM C++ has ~137 unit test files.
The test suite needed to expand to cover all applicable functionality from both
sources, including skip-guarded placeholders for modules not yet compiled or
features not yet available (parallel, external libraries).

## Decision

### Three categories of unit tests

| Category | Label | Skip? | Count |
|----------|-------|-------|-------|
| Serial — modules compiled | `serial;unit` | No | 18 |
| Serial — uncompiled / external deps | `serial;unit` | Yes (exit 77) | 23 |
| Parallel — MPI required | `parallel;unit` | Yes (exit 77) | 18 |

### Skip mechanism

Tests use `SKIP_RETURN_CODE 77` (standard CTest convention):

- **Module guard**: `(unless (false-if-exception (resolve-interface '(mfem mod))) (exit 77))`
- **Unconditional skip**: `(exit 77)` at top of file for features requiring external libraries
- **Per-assertion expected failures**: `test-expect-fail` from SRFI-64 for operations that load but don't work yet

### Test inventory

**Serial unit tests (36 total):**

Existing (5): test-version, test-vector, test-densemat, test-sparsemat, test-mesh

From PyMFEM — active (5): test-array, test-fespace, test-gridfunc,
test-coefficient, test-intrules

From PyMFEM — skip-guarded (10): test-point, test-segment, test-geom,
test-table, test-ncmesh, test-blockmatrix, test-blockoperator,
test-complex-operator, test-periodic-mesh, test-datacollection

From MFEM C++ — active (8): test-bilinearform, test-linearform, test-solvers,
test-operator, test-fe, test-sparsesmoothers, test-domain-int, test-device

From MFEM C++ — skip-guarded (2): test-ode, test-custom-coefficient

From MFEM C++ — external libraries (11): test-exodus-reader, test-exodus-writer,
test-fms, test-ceed, test-ceed-main, test-enzyme, test-umpire,
test-direct-solvers, test-ilu, test-gslib, test-constrained-solver

**Parallel unit tests (18 total, all skip-guarded):**

From PyMFEM (5): test-pfespace, test-merge-gridfunction, test-complexmg,
test-chypre, test-par-datacollection

From MFEM C++ (13): test-hypre-matrix, test-hypre-ilu, test-hypre-prec,
test-hypre-vector, test-pmesh, test-psubmesh, test-project-bdr-par,
test-pgridfunc-save, test-dfem-mass, test-dfem-diffusion,
test-dfem-divergence, test-dfem-lvector, test-amg

### Custom coefficient strategy

PyMFEM uses Numba JIT (`@cfunc`) for user-defined coefficients.
The Guile equivalent uses SWIG director classes + Guile's automatic JIT
(since 3.0). `test-custom-coefficient.scm` is skip-guarded until SWIG
director support is enabled for coefficient classes.

### CMake registration

Tests are registered via `foreach` loops over `SERIAL_UNIT_TESTS` and
`PARALLEL_UNIT_TESTS` lists. All unit tests get `SKIP_RETURN_CODE 77` and
`MFEM_DATA_DIR` in their environment.

### PyMFEM tests NOT ported

| Test | Reason |
|------|--------|
| `test_array2.py` | NumPy `GetDataArray()` memory sharing |
| `test_dofloc.py` | Heavy NumPy array manipulation |
| `test_stringio.py` | Python `io.StringIO` bridge |
| `test_numba.py` | Numba JIT (replaced by test-custom-coefficient.scm concept) |
| `test_memory.py` | Python `tracemalloc` |
| `test_module.py` | Python test runner (CTest replaces this) |
| `test_deprecated.py` | Python-binding-specific deprecated API |
| `test_tmop.py` | Complex + multiple uncompiled modules |

### MFEM C++ tests NOT ported

- **C++ internals** (17 files): memory manager, reduction, scan, text utils,
  ordering, error handling, zlib, partial assembly kernels, full assembly
  determinism, lexicographic ordering, face restriction/permutation,
  Jacobi smoother internals, tet reordering, 1D polynomial basis
- **Shape function internals** (6 files): tested indirectly through assembly
- **Specialized / deferred** (~50 files): NURBS, VTU, LOR, DG mass inverse,
  nonlinear forms, estimators, various FE tests already covered by other tests

## Consequences

- The test suite doubles as a roadmap of what needs to be built.
- Skip-guarded tests make it easy to see which modules are missing.
- Adding a new module just requires removing the skip guard from its test.
- CI stays green — skip-guarded tests show as "Not Run", not "Fail".
- All serial tests that should pass do pass; no false positives.
- Future parallel work has placeholder tests ready to activate.
