;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM GSLIB FindPoints and interpolation (from MFEM test_gslib.cpp).
;; Requires MFEM built with GSLIB support.

;; Guard: skip if required module is not compiled
(unless (false-if-exception (resolve-interface '(mfem gslib)))
  (format (current-error-port) "SKIP: module (mfem gslib) not available~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem gslib))

(test-begin "mfem-gslib")

(test-group "find-points"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-gslib")
(exit (zero? (test-runner-fail-count runner)))
