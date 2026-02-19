;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for distributed FEM mass matrix (from MFEM test_mass.cpp).
;; Requires parallel MFEM build with MPI bindings.

(use-modules (test unit-harness))
(skip-unless '(mfem par))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-dfem-mass")

(test-group "distributed-mass"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-dfem-mass")
(exit (zero? (test-runner-fail-count runner)))
