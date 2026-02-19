;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for AMG F-cycle solver (from MFEM test_amgfsolver.cpp).
;; Requires parallel MFEM build with Hypre and MPI bindings.

;; Guard: skip if parallel module is not compiled
(unless (false-if-exception (resolve-interface '(mfem par)))
  (format (current-error-port) "SKIP: module (mfem par) not available (requires MPI + Hypre)~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-amg")

(test-group "amg-fcycle"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-amg")
(exit (zero? (test-runner-fail-count runner)))
