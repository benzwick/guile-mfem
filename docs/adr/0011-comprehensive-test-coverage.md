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

### Migrated PyMFEM tests removed from tree

19 Python tests have real Guile equivalents in `test/unit/` and are removed
from `test/`.  Git history preserves them for reference.

| Removed Python test | Guile equivalent |
|---------------------|------------------|
| `test_array.py` | `test-array.scm` |
| `test_blockmatrix.py` | `test-blockmatrix.scm` |
| `test_blockoperator.py` | `test-blockoperator.scm` |
| `test_coefficient.py` | `test-coefficient.scm` |
| `test_complex_operator.py` | `test-complex-operator.scm` |
| `test_datacollection.py` | `test-datacollection.scm` |
| `test_densemat.py` | `test-densemat.scm` |
| `test_fespace.py` | `test-fespace.scm` |
| `test_geom.py` | `test-geom.scm` |
| `test_gridfunc.py` | `test-gridfunc.scm` |
| `test_intrules.py` | `test-intrules.scm` |
| `test_mesh.py` | `test-mesh.scm` |
| `test_ncmesh.py` | `test-ncmesh.scm` |
| `test_periodic_mesh.py` | `test-periodic-mesh.scm` |
| `test_point.py` | `test-point.scm` |
| `test_segment.py` | `test-segment.scm` |
| `test_sparsemat.py` | `test-sparsemat.scm` |
| `test_table.py` | `test-table.scm` |
| `test_vector.py` | `test-vector.scm` |

### PyMFEM tests kept — Guile equivalents are placeholder-only

These Python tests remain in `test/` because their Guile equivalents in
`test/unit/` are stub files (placeholder assertions only).  They serve as
reference for completing the Guile implementations.

| Kept Python test | Guile stub | What it tests |
|------------------|------------|---------------|
| `test_chypre.py` | `test-chypre.scm` | CHypreVec complex Hypre vector ops (add, subtract, dot) via SciPy + MPI |
| `test_complexmg.py` | `test-complexmg.scm` | Complex multigrid Helmholtz solver (~450 lines); Numba JIT, ParComplex* forms, MPI |
| `test_merge_gridfunction.py` | `test-merge-gridfunction.scm` | ParGridFunction → serial GridFunction merging and re-partitioning; Numba JIT, MPI |
| `test_pfespace.py` | `test-pfespace.scm` | ParFiniteElementSpace DOF transforms, Dof_TrueDof_Matrix, HypreParMatrix; SciPy/NumPy |
| `issues/test_3960.py` | `test-par-datacollection.scm` | VisItDataCollection parallel→serial conversion via GetSerialMesh/GetSerialGridFunction; uses test data in `issues/data_3960/` |

### PyMFEM tests NOT ported

| Test | Reason |
|------|--------|
| `test_array2.py` | Tests `uintArray`, `int8Array`, `int64Array`, `boolArray` with NumPy `GetDataArray()` — these alternative integer/bool array types are not wrapped for Guile |
| `test_deprecated.py` | Tests Python-binding-specific deprecated API (`intp()`, `Triangle.GetNFaces` pointer arg) |
| `test_dofloc.py` | DOF location mapping via `GetConformingRestriction`, `BuildDofToArrays`, `GetElementForDof`; uses NumPy arrays for coordinate computation |
| `test_gz.py` | Gzip I/O round-trip for GridFunction and Mesh (`.gz` extension, `PrintGZ`); uses NumPy `GetDataArray()` and Python `io.StringIO` |
| `test_memory.py` | Memory leak detection: runs ex1p 30 times, reports RSS via `resource.getrusage` |
| `test_module.py` | Mini test runner that invokes `test_point.py`, `test_segment.py`, `test_mesh.py`; CTest replaces this |
| `test_numba.py` | Numba JIT `@cfunc` / `@mfem.jit` for scalar, vector, and matrix coefficients with dependency chaining and complex-valued support |
| `test_stringio.py` | Tests `WriteToStream` for Mesh, GridFunction, Vector using Python `io.StringIO` |
| `test_tmop.py` | TMOP mesh optimization (target-matrix paradigm); requires `tmop` module, `NonlinearForm`, `PyCoefficient`, GSLIB |

### PyMFEM runners NOT ported

| Runner | What it does | Status |
|--------|-------------|--------|
| `run_tests.py` | Runs each `test_*.py` in serial and parallel (mpirun); CTest replaces this |
| `run_examples.py` | Cross-validates C++ MFEM examples against Python examples: runs both, compares stdout (last 5 lines, numbers to 3 significant digits) and generated output files; **guile-mfem does not yet have equivalent C++↔Guile output comparison** |

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
