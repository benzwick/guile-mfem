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
(use-modules (complex_operator-primitive)
             (mesh-primitive) (fe_coll-primitive) (fespace-primitive)
             (coefficient-primitive) (bilininteg-primitive)
             (bilinearform-primitive))

(test-begin "mfem-complex-operator")

;; ComplexOperator with assembled BilinearForms
(test-group "complex-operator-assembly"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (a1   (new-BilinearForm fes))
         (a2   (new-BilinearForm fes)))
    (BilinearForm-AddDomainIntegrator a1 (new-DiffusionIntegrator one))
    (BilinearForm-Assemble a1)
    (BilinearForm-Finalize a1)
    (BilinearForm-AddDomainIntegrator a2 (new-DiffusionIntegrator one))
    (BilinearForm-Assemble a2)
    (BilinearForm-Finalize a2)
    (let ((M1 (BilinearForm-SpMat a1))
          (M2 (BilinearForm-SpMat a2)))
      (let ((Mc (new-ComplexOperator M1 M2 #f #f)))
        (test-assert "ComplexOperator created" Mc)
        (test-equal "ComplexOperator height = 2*vsize"
          (* 2 (FiniteElementSpace-GetVSize fes))
          (Height Mc))))))

(define runner (test-runner-current))
(test-end "mfem-complex-operator")
(exit (zero? (test-runner-fail-count runner)))
