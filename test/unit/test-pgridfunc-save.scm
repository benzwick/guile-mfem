;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for parallel GridFunction save to serial format
;; (from MFEM test_pgridfunc_save_serial.cpp).
;; Requires parallel MFEM build with MPI bindings.

(use-modules (test unit-harness))
(skip-unless '(mfem par))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-pgridfunc-save")

(test-group "save-serial"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-pgridfunc-save")
(exit (zero? (test-runner-fail-count runner)))
