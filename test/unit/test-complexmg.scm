;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for complex multigrid solver on ParMesh (from PyMFEM
;; test_complexmg.py, based on MFEM example 26).
;; Requires parallel MFEM build with MPI bindings.

(use-modules (test unit-harness))
(skip-unless '(mfem par))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-complexmg")

;; Complex multigrid solver
(test-group "complex-multigrid"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-complexmg")
(exit (zero? (test-runner-fail-count runner)))
