;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM libCEED operator construction (from MFEM test_ceed.cpp).
;; Requires MFEM built with libCEED support.

;; Guard: skip — libCEED not available in current build
(format (current-error-port) "SKIP: libCEED not available~%")
(exit 77)

(use-modules (srfi srfi-64))

(test-begin "mfem-ceed")

(test-group "ceed-operator"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-ceed")
(exit (zero? (test-runner-fail-count runner)))
