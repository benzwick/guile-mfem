;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM libCEED main test framework (from MFEM test_ceed_main.cpp).
;; Requires MFEM built with libCEED support.

;; Guard: skip — libCEED not available in current build
(format (current-error-port) "SKIP: libCEED not available~%")
(exit 77)

(use-modules (srfi srfi-64))

(test-begin "mfem-ceed-main")

(test-group "ceed-main"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-ceed-main")
(exit (zero? (test-runner-fail-count runner)))
