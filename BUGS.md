# Known Bugs

## SWIG Guile proxy segfault on public member variables (FIXED)

**Affects:** SWIG 4.3.0, 4.5.0 (and likely all current versions up to the fix)

**Symptom:** SWIG segfaults when `-proxy` flag is used on modules that wrap
a C++ class with a **public data member** inheriting from a class with virtual
methods. For guile-mfem, this manifests when processing `linalg/operator.hpp`
because `Solver::iterative_mode` is a public `bool` member.

**Root cause:** Bug in `Source/Modules/guile.cxx` line 933 (and 937, 939).
The code uses `%.*s` format in DOH `Printf()`, but DOH's `DohvPrintf` reads
`va_arg` in the order `(int precision, char* string)` while the caller passes
`(char* string, int precision)`. This causes the pointer to be interpreted as
an integer width and the integer as a pointer, leading to a segfault.

The buggy code:
```c
Printf(f_init, "(\"%.*s\", " "scm_make_procedure_with_setter(getter, setter));\n", pc, len - 4);
```

**Fix:** Replace `%.*s` format with explicit substring using DOH's
`NewStringWithSize()`:
```c
String *slot_name = NewStringWithSize(pc, len - 4);
Printf(f_init, "(\"%s\", " "scm_make_procedure_with_setter(getter, setter));\n", slot_name);
Delete(slot_name);
```

**Minimal reproducer:**
```cpp
// test.hpp
class Base {
public:
    virtual void foo() {}
    virtual ~Base() {}
};
class Derived : public Base {
public:
    int member;  // <-- this triggers the segfault
};
```
```swig
// test.i
%module test
%{ #include "test.hpp" %}
%include "test.hpp"
```
```sh
swig -c++ -guile -Linkage module -proxy test.i  # segfault
```

**Fix applied in:** `_reference/swig` submodule, branch `bz/guile-proxy-segfault-fix`
**Upstream PR:** https://github.com/swig/swig/pull/3336

## SWIG cross-module type cast lookup failure (FIXED)

**Affects:** SWIG 4.3.0, 4.5.0 (and likely all versions with `SWIG_TYPE_TABLE`)

**Symptom:** When multiple SWIG modules share a type table
(`-DSWIG_TYPE_TABLE=Name`), casting from a derived type defined in one
module to a base type defined in another module fails with "Wrong type
argument", even though the type hierarchy is correctly registered.

For guile-mfem, this manifests as `Operator-Height` rejecting a
`DenseMatrix` argument: the DenseMatrix→Operator cast entry exists in the
shared cast chain but `SWIG_TypeCheckStruct` can't find it because it uses
pointer comparison (`head->type == from`) and the cast entry still points
to the module-local `swig_type_info` instead of the shared one.

**Root cause:** Bug in `Lib/swiginit.swg`, `SWIG_InitializeModule()`.
When adding a new cast entry to a type that **already exists** in the
shared type table (the `else` branch at ~line 266), the code checks
whether the cast is already present (`SWIG_TypeCheck`) and if not, marks
it for inclusion — but fails to update `cast->type` from the module-local
pointer to the shared `target_type` pointer. The first branch (line 264,
for types **not** yet in the shared table) correctly does
`cast->type = target_type;`.

The buggy code:
```c
if (!ocast) target_type = 0;
```

**Fix:**
```c
if (!ocast) {
  cast->type = target_type;
  target_type = 0;
}
```

**Minimal reproducer:** Any multi-module SWIG setup with `SWIG_TYPE_TABLE`
where module 2 defines a type that inherits from a type in module 1, and
module 1's type is already in the shared table when module 2 initializes.

**Fix applied in:** `_reference/swig` submodule, branch `bz/fix-cross-module-type-cast`
**Upstream PR:** https://github.com/swig/swig/pull/3337

## Null C++ pointers are truthy in Scheme

**Affects:** ex1, ex2 (and any code that branches on a potentially-null pointer)

SWIG Guile wraps null C++ pointers as GOOPS objects that are truthy in
Scheme. Any code that branches on a potentially-null C++ pointer will
take the wrong branch.

**ex1.scm (step 5):** The isoparametric FE branch tests `(GetNodes mesh)`
which returns null for meshes without nodes. In C++ `if (mesh.GetNodes())`
is false for null. In Guile the wrapped null pointer is truthy, so the
`cond` `=>` clause always fires and the fallback `else` branch (which
creates `H1_FECollection` with order=1) is never reached. This only
matters when `order <= 0`.

The examples now error out when `order <= 0` rather than taking the
broken isoparametric path. The corresponding test runs are marked
`test-expect-fail`. When the null-pointer bug is fixed, these tests
will XPASS, signaling that the error guard and xfail can be removed.

**ex2.scm (steps 3, 5, 13):** The NURBS branches test `mesh->NURBSext`
which is null for non-NURBS meshes. Since the wrapped null is truthy,
`(slot-ref mesh 'NURBSext)` always evaluates to true. Calling
`DegreeElevate` on a non-NURBS mesh crashes with "Mesh::DegreeElevate :
Not a NURBS mesh!". The workaround is to skip the NURBS branches entirely.

The examples now error out when a NURBS mesh is detected (by filename)
rather than silently producing wrong results. The corresponding test
runs are marked `test-expect-fail`. When the null-pointer bug is fixed,
the filename guard can be replaced with a proper `NURBSext` check, the
NURBS branches re-enabled, and the xfail annotations removed.

**How to fix:** Add a helper to the SWIG interface that performs the null
check in C++ and returns a Scheme boolean:
```c
// In mesh.i %extend Mesh:
bool HasNURBSExt() const { return self->NURBSext != nullptr; }
```
Or fix SWIG Guile's pointer wrapping to return `#f` for null pointers
(this would be an upstream SWIG change).

## Segfault when importing (mfem gridfunc) with (mfem bilinearform)

**Affects:** test-domain-int (and any code that imports both modules)

Importing `(mfem gridfunc)` in the same script as `(mfem bilinearform)`
causes a segfault during module loading. Each module works independently,
but their combination crashes.

**Minimal reproducer:**
```scheme
(use-modules (mfem bilinearform) (mfem gridfunc))  ;; segfault
```

**Workaround:** Avoid importing both modules in the same script. In
test-domain-int.scm, use raw `<Vector>` with `(Assign x 1.0)` instead of
`GridFunction-ProjectCoefficient`.

**How to fix:** Likely a GOOPS class initialization ordering issue or a
SWIG type table conflict when both modules register overlapping type
hierarchies. Needs investigation — possibly related to the cross-module
GOOPS method dispatch failure below.

## Cross-module GOOPS method dispatch failure

**Affects:** ex2

Methods specialized on types from modules that aren't imported by the
defining module fail to dispatch at runtime. The method exists but its
type specializer is a different class object than the one used to create
the actual argument.

**ex2.scm (step 13):** `SetNodalFESpace` is defined in `mesh.scm` with
a `<FiniteElementSpace>` parameter, but `mesh.scm` does not import
`(mfem fespace)`. The `<FiniteElementSpace>` class in the method
specializer is a locally-created placeholder that doesn't match the
real class. The call `(SetNodalFESpace mesh fespace)` fails with "No
applicable method".

**Workaround in ex2.scm:** Call the C primitive directly:
```scheme
((@@ (mfem mesh) primitive:Mesh-SetNodalFESpace) mesh fespace)
```

**How to fix:** The SWIG code generator (or `PostProcessProxy.cmake`)
needs to add missing `use-modules` imports to generated `.scm` files.
Specifically, `mesh.scm` should import `(mfem fespace)`. This requires
analyzing which types each module's methods reference and ensuring those
modules are imported. Circular dependencies may need care.

## `Vector::Elem` returns a SWIG pointer, not a Scheme number

**Affects:** ex2 (and any code using `Elem` for numeric access)

`(Elem v i)` wraps `Vector::operator()(int i)` which returns `double&`
in C++. SWIG wraps the reference as `#<swig-pointer double *>` which
cannot be used in arithmetic.

**Workaround:** Use `(get v i)` instead, which is defined via `%extend`
in `vector.i` and returns a Scheme number. Similarly use `(set v i val)`
to write.

**How to fix:** This is expected SWIG behavior for reference returns.
The `get`/`set` helpers in `vector.i` are the proper API. Consider
deprecating or hiding `Elem` to avoid confusion.

## Example tests do not verify solution correctness

**Affects:** all example tests (test/examples/test-ex*.scm)

The test suite only checks that each example exits successfully and
produces non-empty output files (`sol.gf`, `mesh.mesh`, etc.). It does
**not** verify that the solution is mathematically correct — e.g. by
comparing against a known reference solution or checking convergence
rates.

This means bugs that produce silently wrong results would not be caught.
To mitigate this, the examples now explicitly error out on known-broken
code paths (NURBS meshes in ex1/ex2) so the tests actually fail (XFAIL)
rather than silently producing wrong output (XPASS).

**How to fix:** Add reference solution comparisons. MFEM's C++ test
suite uses `sol.gf` files checked into the repository; a similar
approach could work here. Alternatively, check L2 error norms against
known analytic solutions (e.g. ex1's `-Delta u = 1` on the unit square
has a known solution).

# Missing Functionality

## GLVis visualization (socketstream)

**Affects:** ex0, ex1, ex2 (all skip GLVis visualization)

The `socketstream.i` SWIG interface file exists in `mfem/_ser/` but is
not compiled into the Guile bindings (no `add_guile_mfem_module` call
in `CMakeLists.txt`).

**How to fix:** Add the socketstream module to `CMakeLists.txt`:
```cmake
add_guile_mfem_module(socketstream
  SWIG_FILE mfem/_ser/socketstream.i
  DEPENDS mfem)
```
Then add `(mfem socketstream)` to the `(mfem)` meta-module and implement
the visualization step in each example.

## `UsesTensorBasis` not wrapped

**Affects:** ex1 (partial assembly path)

The standalone function `UsesTensorBasis(fespace)` is not exposed in the
SWIG interface. This is used in the partial assembly path to decide
between `OperatorJacobiSmoother` + `PCG` and plain `CG`.

**How to fix:** Add to `mfem/_ser/fespace.i` or a suitable `.i` file:
```swig
bool UsesTensorBasis(const FiniteElementSpace &fes);
```

# Features Already Wrapped But Not Used in Examples

These C++ features are already available in the Guile bindings and could
be enabled in the examples with code changes only (no binding work
needed).

## Partial assembly and full assembly (ex1)

The NOTE comments in ex1.scm say partial/full assembly is "not yet
supported", but the required bindings exist:

- `SetAssemblyLevel` on `BilinearForm` is wrapped
- Assembly level constants are exported: `AssemblyLevel-PARTIAL`,
  `AssemblyLevel-FULL`, etc.
- `OperatorJacobiSmoother` is wrapped (for the PA preconditioner)
- `CG` standalone function is wrapped (for the non-preconditioned path)

The only missing piece is `UsesTensorBasis` (see above). Without it, the
PA path could still be implemented by always using
`OperatorJacobiSmoother` (or always using unpreconditioned `CG`), just
without the runtime tensor-basis check.

**To enable:** Add `-p`/`--partial-assembly` and `-f`/`--full-assembly`
command-line options, call `(SetAssemblyLevel a AssemblyLevel-PARTIAL)`,
and use `OperatorJacobiSmoother` + `PCG` or plain `CG` in the solver
step.

## SuiteSparse direct solver (ex1, ex2)

The SuiteSparse path (`UMFPackSolver`) is conditionally compiled in the
SWIG interface (`#ifdef MFEM_USE_SUITESPARSE` in `solvers.i`). If MFEM
is built with SuiteSparse support, the solver is available in the Guile
bindings automatically.

## NURBS degree elevation (ex2)

`DegreeElevate` is wrapped and works correctly. The only reason it is
skipped is the null-pointer check on `NURBSext` (see above). Once that
bug is fixed, the NURBS branches in ex2 can be re-enabled and NURBS
sample runs (`beam-quad-nurbs.mesh`, `beam-hex-nurbs.mesh`) will
produce correct results. Currently these meshes run but produce
silently wrong output (no degree elevation, standard H1 space).

## Isoparametric FE space (ex1)

`OwnFEC` on `GridFunction` and `GetNodes` on `Mesh` are both wrapped.
The isoparametric path (`order <= 0`) is implemented in ex1.scm but
produces silently wrong results due to the null-pointer bug (see
above): the truthy null from `GetNodes` triggers the `cond =>` branch
which calls `OwnFEC` on the null pointer — this happens not to crash
but may return an unintended FE collection. Once null detection is
fixed, NURBS meshes like `square-disc-nurbs.mesh` with `-o -1` will
correctly use the isoparametric/NURBS FE space.
