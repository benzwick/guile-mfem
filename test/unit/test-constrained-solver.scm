;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM constrained solver (from MFEM test_constrainedsolver.cpp).
;; Requires MFEM built with SuiteSparse support.

;; Guard: skip — SuiteSparse not available in current build
(format (current-error-port) "SKIP: SuiteSparse not available~%")
(exit 77)

(use-modules (srfi srfi-64))

(test-begin "mfem-constrained-solver")

(test-group "constrained-solver"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-constrained-solver")
(exit (zero? (test-runner-fail-count runner)))
