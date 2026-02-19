# ADR-0010: Test Suite Structure

## Status

Accepted

## Context

All tests lived in `test/unit/` with a single label `serial`. Example tests
only exercised the default mesh. Developers need fast feedback locally while
CI should exercise all sample runs from each example's header.

## Decision

### Three test categories

| Label | Contents |
|-------|----------|
| `unit` | version, vector, densemat, sparsemat, mesh |
| `examples-fast` | each example (+ -lisp variant) with default mesh only |
| `examples-full` | all sample runs from each example's header comments |
| `miniapps` | (reserved for future miniapp tests) |

### Two parallelism labels

| Label | Contents |
|-------|----------|
| `serial` | serial MFEM tests |
| `parallel` | parallel/MPI tests (future exNp examples) |

Each test carries **two labels**, e.g. `LABELS "serial;examples-fast"`.

### One file, two CTest entries

Each test file in `test/examples/` checks the `TEST_MODE` environment variable
(`fast` or `full`, default `full`). CMake creates two CTest entries from each
file — one with `TEST_MODE=fast`, one with `TEST_MODE=full`.

### Expected failures

NURBS meshes that trigger the null-pointer truthiness bug (see BUGS.md) are
marked with SRFI-64 `test-expect-fail`. These count as `xfail` (not `fail`),
so the test exits cleanly. If a bug is later fixed and the test passes, SRFI-64
reports `xpass` — a signal to remove the annotation.

Output file checks use `catch` wrappers so that a crash (where files are never
written) returns `#f` instead of throwing. Output files are deleted before each
run to prevent stale files from masking failures.

### No `--` in system* calls

Guile includes `--` in `(command-line)` when a script is invoked as
`guile -L build script.scm -- -m mesh`. This causes `getopt-long` to treat
everything after `--` as non-option arguments, silently ignoring all flags.
Test runners must invoke examples **without** `--`:
```scheme
(apply system* "guile" "-L" load-path script args)  ; correct
(apply system* "guile" "-L" load-path script "--" args)  ; broken
```

### Developer workflow

`build.sh` runs `ctest -L "unit|examples-fast"` — unit tests plus one fast run
per example. The full suite runs in CI via `ctest -L serial`.

### Useful ctest invocations

```
ctest -L unit            # 5 unit tests
ctest -L examples-fast   # 6 fast example tests
ctest -L examples-full   # 6 full example tests
ctest -L serial          # all 17 serial tests
```

## Consequences

- Developers get fast local feedback (~seconds for unit + fast examples).
- CI exercises every sample run from each example header, catching regressions
  on specific mesh types.
- Known NURBS failures are tracked in-tree and don't break the build.
- Adding a new example requires one test file plus two `foreach` entries in
  CMakeLists.txt.
- Future parallel tests (exNp) slot in with `LABELS "parallel;examples-fast"`.
