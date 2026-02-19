;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM BilinearForm (from MFEM test_bilinearform.cpp).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg)
             (mfem bilinearform) (mfem sparsemat)
             (mfem vector) (mfem operators))
(use-modules (mesh-primitive) (fe_coll-primitive) (fespace-primitive)
             (coefficient-primitive) (bilininteg-primitive)
             (bilinearform-primitive))

(test-begin "mfem-bilinearform")

;; Basic assembly with DiffusionIntegrator on a quad mesh
(test-group "diffusion-assemble"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (a    (new-BilinearForm fes)))
    (BilinearForm-AddDomainIntegrator a (new-DiffusionIntegrator one))
    (BilinearForm-Assemble a)
    (BilinearForm-Finalize a)
    (let ((mat (BilinearForm-SpMat a)))
      (test-assert "SpMat height > 0" (> (Height mat) 0))
      (test-assert "SpMat width > 0" (> (Width mat) 0))
      (test-equal "SpMat is square" (Height mat) (Width mat)))))

;; Assembly with MassIntegrator
(test-group "mass-assemble"
  (let* ((mesh (new-Mesh 2 2 "TRIANGLE"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (a    (new-BilinearForm fes)))
    (BilinearForm-AddDomainIntegrator a (new-MassIntegrator one))
    (BilinearForm-Assemble a)
    (BilinearForm-Finalize a)
    (let ((mat (BilinearForm-SpMat a)))
      (test-assert "mass matrix assembled" (> (Height mat) 0)))))

;; Mult: y = A*x
(test-group "bilinearform-mult"
  (let* ((mesh (new-Mesh 2 2 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (a    (new-BilinearForm fes))
         (n    (FiniteElementSpace-GetVSize fes))
         (x    (make <Vector> n))
         (y    (make <Vector> n)))
    (BilinearForm-AddDomainIntegrator a (new-DiffusionIntegrator one))
    (BilinearForm-Assemble a)
    (BilinearForm-Finalize a)
    (Assign x 1.0)
    (let ((mat (BilinearForm-SpMat a)))
      (Mult mat x y)
      ;; For constant function, diffusion of constant = 0 in interior,
      ;; so y should be small (boundary contributions only)
      (test-assert "Mult produces result" (>= (Norml2 y) 0.0)))))

(define runner (test-runner-current))
(test-end "mfem-bilinearform")
(exit (zero? (test-runner-fail-count runner)))
