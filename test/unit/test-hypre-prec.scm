;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for Hypre preconditioners — BoomerAMG, etc.
;; (from MFEM test_hypre_prec.cpp).
;; Requires parallel MFEM build with Hypre and MPI bindings.

(use-modules (test unit-harness))
(skip-unless '(mfem par))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-hypre-prec")

(test-group "boomeramg"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-hypre-prec")
(exit (zero? (test-runner-fail-count runner)))
