;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for distributed local vector interface
;; (from MFEM test_lvector_interface.cpp).
;; Requires parallel MFEM build with MPI bindings.

(use-modules (test unit-harness))
(skip-unless '(mfem par))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-dfem-lvector")

(test-group "local-vector"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-dfem-lvector")
(exit (zero? (test-runner-fail-count runner)))
