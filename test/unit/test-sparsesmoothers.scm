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
(use-modules (mesh-primitive) (fe_coll-primitive) (fespace-primitive)
             (coefficient-primitive) (bilininteg-primitive)
             (bilinearform-primitive) (sparsesmoothers-primitive))

(test-begin "mfem-sparsesmoothers")

;; GSSmoother construction and Mult
(test-group "gs-smoother"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (a    (new-BilinearForm fes))
         (n    (FiniteElementSpace-GetVSize fes)))
    (BilinearForm-AddDomainIntegrator a (new-DiffusionIntegrator one))
    (BilinearForm-AddDomainIntegrator a (new-MassIntegrator one))
    (BilinearForm-Assemble a)
    (BilinearForm-Finalize a)
    (let ((gs (new-GSSmoother (BilinearForm-SpMat a)))
          (x  (make <Vector> n))
          (y  (make <Vector> n)))
      (Assign x 1.0)
      (Mult gs x y)
      (test-assert "GSSmoother produces output" (> (Norml2 y) 0.0)))))

;; DSmoother construction and Mult
(test-group "d-smoother"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (a    (new-BilinearForm fes))
         (n    (FiniteElementSpace-GetVSize fes)))
    (BilinearForm-AddDomainIntegrator a (new-DiffusionIntegrator one))
    (BilinearForm-AddDomainIntegrator a (new-MassIntegrator one))
    (BilinearForm-Assemble a)
    (BilinearForm-Finalize a)
    (let ((ds (new-DSmoother (BilinearForm-SpMat a)))
          (x  (make <Vector> n))
          (y  (make <Vector> n)))
      (Assign x 1.0)
      (Mult ds x y)
      (test-assert "DSmoother produces output" (> (Norml2 y) 0.0)))))

(define runner (test-runner-current))
(test-end "mfem-sparsesmoothers")
(exit (zero? (test-runner-fail-count runner)))
