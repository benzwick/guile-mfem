;; run-tests.scm -- Run all guile-mfem test suites
;;
;; Usage: guile -L <build-dir> test/run-tests.scm

(use-modules (srfi srfi-64))

;; Load test files
(load "test/unit/test-version.scm")

;; Exit with non-zero status on failure
(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))
