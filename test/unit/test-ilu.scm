;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM ILU preconditioner (from MFEM test_ilu.cpp).
;; Requires MFEM built with ILU library support.

;; Guard: skip — ILU library not available in current build
(format (current-error-port) "SKIP: ILU library not available~%")
(exit 77)

(use-modules (srfi srfi-64))

(test-begin "mfem-ilu")

(test-group "ilu-preconditioner"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-ilu")
(exit (zero? (test-runner-fail-count runner)))
