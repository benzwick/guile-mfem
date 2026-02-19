;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM ParFiniteElementSpace (from PyMFEM test_pfespace.py).
;; Requires parallel MFEM build with MPI bindings.

;; Guard: skip if parallel module is not compiled
(unless (false-if-exception (resolve-interface '(mfem par)))
  (format (current-error-port) "SKIP: module (mfem par) not available (requires MPI)~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-pfespace")

;; ParFiniteElementSpace construction and DOF queries
(test-group "construction"
  (test-assert "placeholder" #t))

;; DOF transformations
(test-group "dof-transforms"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-pfespace")
(exit (zero? (test-runner-fail-count runner)))
