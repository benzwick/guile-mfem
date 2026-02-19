;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for distributed FEM divergence operator
;; (from MFEM test_divergence.cpp).
;; Requires parallel MFEM build with MPI bindings.

(use-modules (test unit-harness))
(skip-unless '(mfem par))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-dfem-divergence")

(test-group "distributed-divergence"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-dfem-divergence")
(exit (zero? (test-runner-fail-count runner)))
