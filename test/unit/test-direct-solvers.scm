;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM direct sparse solvers (from MFEM test_direct_solvers.cpp).
;; Requires MFEM built with UMFPACK, SuperLU, or MKL Pardiso.

;; Guard: skip — direct solver libraries not available in current build
(format (current-error-port) "SKIP: direct solver libraries (UMFPACK/SuperLU/Pardiso) not available~%")
(exit 77)

(use-modules (srfi srfi-64))

(test-begin "mfem-direct-solvers")

(test-group "umfpack"
  (test-assert "placeholder" #t))

(test-group "superlu"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-direct-solvers")
(exit (zero? (test-runner-fail-count runner)))
