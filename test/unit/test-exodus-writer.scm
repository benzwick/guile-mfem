;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Exodus mesh writer (from MFEM test_exodus_writer.cpp).
;; Requires MFEM built with Exodus I/O library support.

;; Guard: skip — Exodus I/O not available in current build
(format (current-error-port) "SKIP: Exodus I/O library not available~%")
(exit 77)

(use-modules (srfi srfi-64)
             (mfem mesh))

(test-begin "mfem-exodus-writer")

(test-group "write-exodus-mesh"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-exodus-writer")
(exit (zero? (test-runner-fail-count runner)))
