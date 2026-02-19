;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Array types (from PyMFEM test_array.py).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem array))

(test-begin "mfem-array")

;; intArray construction and size
(test-group "intArray-construction"
  (let ((a (make <intArray>)))
    (test-equal "intArray() size" 0 (Size a)))

  (let ((a (make <intArray> 3)))
    (test-equal "intArray(3) size" 3 (Size a))))

;; intArray element access
(test-group "intArray-element-access"
  (let ((a (make <intArray> 3)))
    (set a 0 10)
    (set a 1 20)
    (set a 2 30)
    (test-equal "intArray[0]" 10 (get a 0))
    (test-equal "intArray[1]" 20 (get a 1))
    (test-equal "intArray[2]" 30 (get a 2))))

;; doubleArray construction and size
(test-group "doubleArray-construction"
  (let ((a (make <doubleArray>)))
    (test-equal "doubleArray() size" 0 (Size a)))

  (let ((a (make <doubleArray> 3)))
    (test-equal "doubleArray(3) size" 3 (Size a))))

;; doubleArray element access
(test-group "doubleArray-element-access"
  (let ((a (make <doubleArray> 3)))
    (set a 0 1.1)
    (set a 1 2.2)
    (set a 2 3.0)
    (test-approximate "doubleArray[0]" 1.1 (get a 0) 1e-15)
    (test-approximate "doubleArray[1]" 2.2 (get a 1) 1e-15)
    (test-approximate "doubleArray[2]" 3.0 (get a 2) 1e-15)))

(define runner (test-runner-current))
(test-end "mfem-array")
(exit (zero? (test-runner-fail-count runner)))
