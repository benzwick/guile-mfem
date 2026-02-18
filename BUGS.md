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
