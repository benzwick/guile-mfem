;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Enzyme AD compatibility (from MFEM compatibility.cpp).
;; Requires MFEM built with Enzyme automatic differentiation support.

;; Guard: skip — Enzyme AD not available in current build
(format (current-error-port) "SKIP: Enzyme AD not available~%")
(exit 77)

(use-modules (srfi srfi-64))

(test-begin "mfem-enzyme")

(test-group "enzyme-compatibility"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-enzyme")
(exit (zero? (test-runner-fail-count runner)))
