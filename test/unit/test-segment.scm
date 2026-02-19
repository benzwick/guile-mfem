;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Segment element (from PyMFEM test_segment.py).

(use-modules (test unit-harness))
(skip-unless '(mfem segment))

(use-modules (srfi srfi-64)
             (mfem segment))
(use-modules (segment-primitive))

(test-begin "mfem-segment")

;; Segment construction and attributes
(test-group "construction"
  (let ((seg (new-Segment)))
    (Segment-SetAttribute seg 1)
    (test-equal "Segment GetAttribute" 1 (Segment-GetAttribute seg))))

(define runner (test-runner-current))
(test-end "mfem-segment")
(exit (zero? (test-runner-fail-count runner)))
