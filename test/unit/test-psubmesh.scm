;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for parallel submesh extraction (from MFEM test_psubmesh.cpp).
;; Requires parallel MFEM build with MPI bindings.

;; Guard: skip if parallel module is not compiled
(unless (false-if-exception (resolve-interface '(mfem par)))
  (format (current-error-port) "SKIP: module (mfem par) not available (requires MPI)~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-psubmesh")

(test-group "parallel-submesh"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-psubmesh")
(exit (zero? (test-runner-fail-count runner)))
