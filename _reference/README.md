# Reference Submodules

Read-only reference copies of upstream projects. These are **not** build
dependencies — they exist for diffing, browsing, and patching.

| Submodule | Purpose |
|-----------|---------|
| `mfem/`   | MFEM C++ library — browse headers, understand API |
| `PyMFEM/` | Upstream PyMFEM — diff against our adapted `.i` files |
| `swig/`   | SWIG source — pin version, apply Guile backend patches if needed |

## Usage

```sh
# Initial clone (submodules are not fetched by default)
git submodule update --init _reference/mfem
git submodule update --init _reference/PyMFEM
git submodule update --init _reference/swig

# Compare our adapted .i file against PyMFEM's original
diff mfem/_ser/vector.i _reference/PyMFEM/mfem/_ser/vector.i
```
