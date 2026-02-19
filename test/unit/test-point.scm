;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Point element (from PyMFEM test_point.py).

;; Guard: skip if required module is not compiled
(unless (false-if-exception (resolve-interface '(mfem point)))
  (format (current-error-port) "SKIP: module (mfem point) not available~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem point))
(use-modules (point-primitive))

(test-begin "mfem-point")

;; Point construction and attributes
(test-group "construction"
  (let ((pt (new-Point 3)))
    (test-equal "Point GetVertices" 3 (Point-GetVertices pt))
    (Point-SetAttribute pt 1)
    (test-equal "Point GetAttribute" 1 (Point-GetAttribute pt))))

(define runner (test-runner-current))
(test-end "mfem-point")
(exit (zero? (test-runner-fail-count runner)))
