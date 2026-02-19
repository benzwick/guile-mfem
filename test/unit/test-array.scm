;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Array types (from PyMFEM test_array.py).

(use-modules (srfi srfi-64)
             (mfem array))
(use-modules (array-primitive))

(test-begin "mfem-array")

;; intArray construction and size
(test-group "intArray-construction"
  (let ((a (new-intArray)))
    (test-equal "intArray() size" 0 (intArray-Size a))
    (delete-intArray a))

  (let ((a (new-intArray 3)))
    (test-equal "intArray(3) size" 3 (intArray-Size a))
    (delete-intArray a)))

;; intArray element access
(test-group "intArray-element-access"
  (let ((a (new-intArray 3)))
    (intArray-set a 0 10)
    (intArray-set a 1 20)
    (intArray-set a 2 30)
    (test-equal "intArray[0]" 10 (intArray-get a 0))
    (test-equal "intArray[1]" 20 (intArray-get a 1))
    (test-equal "intArray[2]" 30 (intArray-get a 2))
    (delete-intArray a)))

;; doubleArray construction and size
(test-group "doubleArray-construction"
  (let ((a (new-doubleArray)))
    (test-equal "doubleArray() size" 0 (doubleArray-Size a))
    (delete-doubleArray a))

  (let ((a (new-doubleArray 3)))
    (test-equal "doubleArray(3) size" 3 (doubleArray-Size a))
    (delete-doubleArray a)))

;; doubleArray element access
(test-group "doubleArray-element-access"
  (let ((a (new-doubleArray 3)))
    (doubleArray-set a 0 1.1)
    (doubleArray-set a 1 2.2)
    (doubleArray-set a 2 3.0)
    (test-approximate "doubleArray[0]" 1.1 (doubleArray-get a 0) 1e-15)
    (test-approximate "doubleArray[1]" 2.2 (doubleArray-get a 1) 1e-15)
    (test-approximate "doubleArray[2]" 3.0 (doubleArray-get a 2) 1e-15)
    (delete-doubleArray a)))

(define runner (test-runner-current))
(test-end "mfem-array")
(exit (zero? (test-runner-fail-count runner)))
