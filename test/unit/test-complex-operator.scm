;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM ComplexOperator (from PyMFEM test_complex_operator.py).

(use-modules (test unit-harness))
(skip-unless '(mfem complex_operator))

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem complex_operator)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg)
             (mfem bilinearform) (mfem vector))

(test-begin "mfem-complex-operator")

;; ComplexOperator with assembled BilinearForms
(test-group "complex-operator-assembly"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a1   (make <BilinearForm> fes))
         (a2   (make <BilinearForm> fes)))
    (AddDomainIntegrator a1 (make <DiffusionIntegrator> one))
    (Assemble a1)
    (Finalize a1)
    (AddDomainIntegrator a2 (make <DiffusionIntegrator> one))
    (Assemble a2)
    (Finalize a2)
    (let ((M1 (SpMat a1))
          (M2 (SpMat a2)))
      (let ((Mc (make <ComplexOperator> M1 M2 #f #f)))
        (test-assert "ComplexOperator created" Mc)
        (test-equal "ComplexOperator height = 2*vsize"
          (* 2 (GetVSize fes))
          (Height Mc))))))

(define runner (test-runner-current))
(test-end "mfem-complex-operator")
(exit (zero? (test-runner-fail-count runner)))
