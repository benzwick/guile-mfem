;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM integration rules (from PyMFEM test_intrules.py).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem intrules))

(test-begin "mfem-intrules")

;; IntRules is a SWIG global variable accessor (thunk)
;; Geometry::TRIANGLE = 2
(test-group "intrules-get"
  (let ((ir (Get (IntRules) 2 2)))
    (test-assert "IntegrationRule exists" ir)
    (test-equal "triangle order 2 has 3 points"
      3 (GetNPoints ir))))

;; IntegrationPoint coordinates — x and y are GOOPS slots
(test-group "integration-point-coords"
  (let* ((ir  (Get (IntRules) 2 2))
         (ip0 (IntPoint ir 0))
         (ip1 (IntPoint ir 1))
         (ip2 (IntPoint ir 2)))
    ;; Expected coordinates for triangle order 2 quadrature
    (test-approximate "ip0.x" (/ 1.0 6.0) (slot-ref ip0 'x) 1e-14)
    (test-approximate "ip0.y" (/ 1.0 6.0) (slot-ref ip0 'y) 1e-14)
    (test-approximate "ip1.x" (/ 1.0 6.0) (slot-ref ip1 'x) 1e-14)
    (test-approximate "ip1.y" (/ 2.0 3.0) (slot-ref ip1 'y) 1e-14)
    (test-approximate "ip2.x" (/ 2.0 3.0) (slot-ref ip2 'x) 1e-14)
    (test-approximate "ip2.y" (/ 1.0 6.0) (slot-ref ip2 'y) 1e-14)))

;; IntRules.Get for segment (Geometry::SEGMENT = 1)
(test-group "intrules-segment"
  (let ((ir (Get (IntRules) 1 3)))
    (test-assert "segment rule has points"
      (> (GetNPoints ir) 0))))

(define runner (test-runner-current))
(test-end "mfem-intrules")
(exit (zero? (test-runner-fail-count runner)))
