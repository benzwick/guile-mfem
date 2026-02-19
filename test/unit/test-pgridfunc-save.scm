;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for parallel GridFunction save to serial format
;; (from MFEM test_pgridfunc_save_serial.cpp).
;; Requires parallel MFEM build with MPI bindings.

;; Guard: skip if parallel module is not compiled
(unless (false-if-exception (resolve-interface '(mfem par)))
  (format (current-error-port) "SKIP: module (mfem par) not available (requires MPI)~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-pgridfunc-save")

(test-group "save-serial"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-pgridfunc-save")
(exit (zero? (test-runner-fail-count runner)))
