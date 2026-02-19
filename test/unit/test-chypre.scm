;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for CHypreVec complex Hypre vectors and matrices (from PyMFEM
;; test_chypre.py).  Requires parallel MFEM build with Hypre and MPI bindings.

(use-modules (test unit-harness))
(skip-unless '(mfem par))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-chypre")

;; CHypreVec construction and arithmetic
(test-group "chypre-vec"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-chypre")
(exit (zero? (test-runner-fail-count runner)))
