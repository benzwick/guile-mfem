;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Coefficient types (from PyMFEM test_coefficient.py).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem coefficient) (mfem densemat))
(use-modules (coefficient-primitive))

(test-begin "mfem-coefficient")

;; ConstantCoefficient construction
(test-group "constant-coefficient"
  (let ((c (new-ConstantCoefficient 3.14)))
    (test-assert "ConstantCoefficient created" (is-a? c <ConstantCoefficient>))
    (test-approximate "ConstantCoefficient constant" 3.14
      (slot-ref c 'constant) 1e-15)))

;; ConstantCoefficient with zero
(test-group "constant-coefficient-zero"
  (let ((c (new-ConstantCoefficient 0.0)))
    (test-approximate "ConstantCoefficient zero" 0.0
      (slot-ref c 'constant) 1e-15)))

;; MatrixConstantCoefficient construction
(test-group "matrix-constant-coefficient"
  (let* ((m (make <DenseMatrix> 3 3))
         (_ (begin (Assign m 0.0)
                   (set m 0 0 1.0)
                   (set m 1 1 2.0)
                   (set m 2 2 3.0)))
         (mc (new-MatrixConstantCoefficient m)))
    (test-assert "MatrixConstantCoefficient created"
      (is-a? mc <MatrixConstantCoefficient>))))

(define runner (test-runner-current))
(test-end "mfem-coefficient")
(exit (zero? (test-runner-fail-count runner)))
