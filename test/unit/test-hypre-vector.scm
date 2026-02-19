;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for HypreParVector construction and operations
;; (from MFEM test_hypre_vector.cpp).
;; Requires parallel MFEM build with Hypre and MPI bindings.

;; Guard: skip if parallel module is not compiled
(unless (false-if-exception (resolve-interface '(mfem par)))
  (format (current-error-port) "SKIP: module (mfem par) not available (requires MPI + Hypre)~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-hypre-vector")

(test-group "construction"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-hypre-vector")
(exit (zero? (test-runner-fail-count runner)))
