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

**Fix applied in:** `_reference/swig` submodule, branch `guile-proxy-segfault-fix`
