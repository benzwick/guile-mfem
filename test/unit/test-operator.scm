;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Operator interface (from MFEM test_operator.cpp).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg)
             (mfem bilinearform) (mfem sparsemat)
             (mfem vector) (mfem operators))

(test-begin "mfem-operator")

;; Operator Height/Width from BilinearForm
(test-group "operator-height-width"
  (let* ((mesh (make <Mesh> 3 3 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes)))
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-equal "operator height = width" (Height mat) (Width mat))
      (test-equal "operator height = vsize"
        (GetVSize fes) (Height mat)))))

;; TransposeOperator
;; NOTE: TransposeOperator wrapping may not be available yet.
;; SparseMatrix transpose is tested instead.
(test-group "sparse-transpose"
  (let* ((mesh (make <Mesh> 2 2 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes)))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let* ((mat (SpMat a))
           (n   (Height mat))
           (x   (make <Vector> n))
           (y1  (make <Vector> n))
           (y2  (make <Vector> n)))
      ;; Mass matrix is symmetric, so A*x should equal A^T*x
      (Assign x 1.0)
      (Mult mat x y1)
      (MultTranspose mat x y2)
      ;; For symmetric mass matrix, y1 and y2 should be equal
      (let ((diff (make <Vector> n)))
        (Assign diff 0.0)
        (subtract-vector y1 y2 diff)
        (test-approximate "symmetric: A*1 = A^T*1" 0.0 (Norml2 diff) 1e-12)))))

(define runner (test-runner-current))
(test-end "mfem-operator")
(exit (zero? (test-runner-fail-count runner)))
