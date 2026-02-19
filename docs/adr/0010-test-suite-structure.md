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
| `unit` | 59 unit tests (see ADR-0011 for full inventory) |
| `examples-fast` | each example (+ -lisp variant) with default mesh only |
| `examples-full` | all sample runs from each example's header comments |
| `miniapps` | (reserved for future miniapp tests) |

### Two parallelism labels

| Label | Contents |
|-------|----------|
| `serial` | serial MFEM tests |
| `parallel` | parallel/MPI tests (future exNp examples) |

Each test carries **two labels**, e.g. `LABELS "serial;examples-fast"`.

### Shared harness

`test/example-harness.scm` provides the `(test example-harness)` module used by
all example test files. It exports:

- `test-mode`, `full?` — from the `TEST_MODE` environment variable
- `make-example-runner` — factory that returns `run` and `run/xfail` procedures
- `test-end/exit` — prints total time, ends the SRFI-64 suite, exits

`make-example-runner` takes the example script name and list of output files.
The returned `run` procedure takes command-line args (e.g. `"-m" "star.mesh"`),
automatically expands `-m` mesh filenames to `$MFEM_DATA_DIR/filename`,
generates the test label, cleans up output files, times the run, and reports
PASS/XFAIL/XPASS/FAIL.

### One file, two CTest entries

Each test file in `test/examples/` uses `full?` from the harness to gate
long-running mesh variants. CMake creates two CTest entries from each file —
one with `TEST_MODE=fast`, one with `TEST_MODE=full`.

### Expected failures

NURBS meshes that trigger the null-pointer truthiness bug (see BUGS.md) use
`run/xfail` instead of `run`. These count as `xfail` (not `fail`), so the test
exits cleanly. If a bug is later fixed and the test passes, SRFI-64 reports
`xpass` — a signal to remove the annotation.

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

### Module availability skipping

Tests for uncompiled modules or unavailable external libraries use
`SKIP_RETURN_CODE 77` (the CTest convention for "Not Run"). Each such test
guards itself at the top of the file:

```scheme
(use-modules (test unit-harness))
(skip-unless '(mfem point))
```

CTest shows these as "Not Run" rather than failures, keeping the pass/fail
signal clean. See ADR-0011 for the full inventory of skip-guarded tests.

### Useful ctest invocations

```
ctest -L unit            # 59 unit tests (active + skip-guarded)
ctest -L "serial;unit"   # 41 serial unit tests
ctest -L "parallel;unit" # 18 parallel unit tests (all skip until MPI added)
ctest -L examples-fast   # 6 fast example tests
ctest -L examples-full   # 6 full example tests
ctest -L serial          # all serial tests (unit + examples)
```

## Consequences

- Developers get fast local feedback (~seconds for unit + fast examples).
- CI exercises every sample run from each example header, catching regressions
  on specific mesh types.
- Known NURBS failures are tracked in-tree and don't break the build.
- Adding a new example requires one short test file (using the harness) plus
  adding the name to the `foreach` loops in CMakeLists.txt.
- Future parallel tests (exNp) slot in with `LABELS "parallel;examples-fast"`.
