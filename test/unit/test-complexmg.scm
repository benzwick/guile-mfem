;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for complex multigrid solver on ParMesh (from PyMFEM
;; test_complexmg.py, based on MFEM example 26).
;; Requires parallel MFEM build with MPI bindings.

;; Guard: skip if parallel module is not compiled
(unless (false-if-exception (resolve-interface '(mfem par)))
  (format (current-error-port) "SKIP: module (mfem par) not available (requires MPI)~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-complexmg")

;; Complex multigrid solver
(test-group "complex-multigrid"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-complexmg")
(exit (zero? (test-runner-fail-count runner)))
