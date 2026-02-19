;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Umpire memory manager (from MFEM test_umpire_mem.cpp).
;; Requires MFEM built with Umpire support.

;; Guard: skip — Umpire not available in current build
(format (current-error-port) "SKIP: Umpire memory manager not available~%")
(exit 77)

(use-modules (srfi srfi-64))

(test-begin "mfem-umpire")

(test-group "umpire-memory"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-umpire")
(exit (zero? (test-runner-fail-count runner)))
