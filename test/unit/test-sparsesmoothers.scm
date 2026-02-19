;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM sparse smoothers (from MFEM test_sparsesmoothers.cpp).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem vector) (mfem operators)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg)
             (mfem bilinearform) (mfem sparsemat)
             (mfem sparsesmoothers))

(test-begin "mfem-sparsesmoothers")

;; GSSmoother construction and Mult
(test-group "gs-smoother"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (n    (GetVSize fes)))
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((gs (make <GSSmoother> (SpMat a)))
          (x  (make <Vector> n))
          (y  (make <Vector> n)))
      (Assign x 1.0)
      (Mult gs x y)
      (test-assert "GSSmoother produces output" (> (Norml2 y) 0.0)))))

;; DSmoother construction and Mult
(test-group "d-smoother"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (n    (GetVSize fes)))
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((ds (make <DSmoother> (SpMat a)))
          (x  (make <Vector> n))
          (y  (make <Vector> n)))
      (Assign x 1.0)
      (Mult ds x y)
      (test-assert "DSmoother produces output" (> (Norml2 y) 0.0)))))

(define runner (test-runner-current))
(test-end "mfem-sparsesmoothers")
(exit (zero? (test-runner-fail-count runner)))
