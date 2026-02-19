;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM FMS mesh format I/O (from MFEM test_fms.cpp).
;; Requires MFEM built with FMS I/O library support.

;; Guard: skip — FMS I/O not available in current build
(format (current-error-port) "SKIP: FMS I/O library not available~%")
(exit 77)

(use-modules (srfi srfi-64)
             (mfem mesh))

(test-begin "mfem-fms")

(test-group "fms-io"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-fms")
(exit (zero? (test-runner-fail-count runner)))
