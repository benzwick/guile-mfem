;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for Discontinuous Galerkin (DG) finite elements and integrators.
;; Tests DG_FECollection, DGDiffusionIntegrator, DGDirichletLFIntegrator,
;; AddInteriorFaceIntegrator, and AddBdrFaceIntegrator.

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg) (mfem lininteg)
             (mfem bilinearform) (mfem linearform) (mfem sparsemat)
             (mfem vector) (mfem operators) (mfem array)
             (mfem gridfunc) (mfem handle) (mfem solvers)
             (mfem sparsesmoothers))

(test-begin "mfem-dg")

;; DG_FECollection creation
(test-group "dg-fecollection"
  (let ((fec (make <DG-FECollection> 1 2)))
    (test-assert "DG_FECollection created" fec)))

;; DG_FECollection order 0
(test-group "dg-fecollection-p0"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <DG-FECollection> 0 2))
         (fes  (make <FiniteElementSpace> mesh fec)))
    ;; P0 DG: one DOF per element
    (test-assert "DG P0 dofs = num elements"
      (= (GetVSize fes) (GetNE mesh)))))

;; DiffusionIntegrator on DG space
(test-group "dg-diffusion-volume"
  (let* ((mesh (make <Mesh> 4 4 "TRIANGLE"))
         (fec  (make <DG-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes)))
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "DG volume diffusion assembled" (> (Height mat) 0)))))

;; DGDiffusionIntegrator on interior faces (SIPG, sigma=-1)
(test-group "dg-interior-face"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <DG-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (sigma -1.0)
         (kappa 4.0))
    (AddInteriorFaceIntegrator a
      (make <DGDiffusionIntegrator> one sigma kappa))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "DG interior face integrator assembled"
        (> (Height mat) 0)))))

;; DGDiffusionIntegrator on boundary faces
(test-group "dg-boundary-face"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <DG-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (sigma -1.0)
         (kappa 4.0))
    (AddBdrFaceIntegrator a
      (make <DGDiffusionIntegrator> one sigma kappa))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "DG boundary face integrator assembled"
        (> (Height mat) 0)))))

;; Full DG assembly: volume + interior faces + boundary faces
(test-group "dg-full-assembly"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <DG-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (sigma -1.0)
         (kappa 4.0))
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddInteriorFaceIntegrator a
      (make <DGDiffusionIntegrator> one sigma kappa))
    (AddBdrFaceIntegrator a
      (make <DGDiffusionIntegrator> one sigma kappa))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "full DG matrix size = fes vsize"
        (= (Height mat) (GetVSize fes))))))

;; DGDirichletLFIntegrator for boundary conditions
(test-group "dg-dirichlet-lf"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <DG-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (zero (make <ConstantCoefficient> 0.0))
         (b    (make <LinearForm> fes))
         (sigma -1.0)
         (kappa 4.0))
    (AddBdrFaceIntegrator b
      (make <DGDirichletLFIntegrator> zero one sigma kappa))
    (Assemble b)
    (test-assert "DG Dirichlet LF size = fes vsize"
      (= (Size b) (GetVSize fes)))))

;; DG solve: -Delta u = 1 on unit square with u=0 on boundary
(test-group "dg-solve"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <DG-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (zero (make <ConstantCoefficient> 0.0))
         (sigma -1.0)
         (kappa 4.0)
         (b    (make <LinearForm> fes))
         (x    (make <GridFunction> fes))
         (a    (make <BilinearForm> fes)))
    ;; RHS
    (AddDomainIntegrator b (make <DomainLFIntegrator> one))
    (AddBdrFaceIntegrator b
      (make <DGDirichletLFIntegrator> zero one sigma kappa))
    (Assemble b)
    ;; Solution
    (Assign x 0.0)
    ;; Bilinear form
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddInteriorFaceIntegrator a
      (make <DGDiffusionIntegrator> one sigma kappa))
    (AddBdrFaceIntegrator a
      (make <DGDiffusionIntegrator> one sigma kappa))
    (Assemble a)
    (Finalize a)
    ;; Solve
    (let ((A (SpMat a))
          (M (make <GSSmoother> (SpMat a))))
      (PCG A M b x 0 500 1e-12 0.0)
      ;; Solution should be positive in the interior
      (test-assert "DG solution max > 0" (> (Max x) 0.0)))))

;; DG with higher order
(test-group "dg-higher-order"
  (let* ((mesh (make <Mesh> 2 2 "TRIANGLE"))
         (fec  (make <DG-FECollection> 2 2))
         (fes  (make <FiniteElementSpace> mesh fec)))
    (test-assert "DG P2 dofs > DG P1 dofs"
      (> (GetVSize fes)
         (let* ((fec1 (make <DG-FECollection> 1 2))
                (fes1 (make <FiniteElementSpace> mesh fec1)))
           (GetVSize fes1))))))

;; 3D DG on tet mesh
(test-group "dg-3d"
  (let* ((mesh (make <Mesh> 2 2 2 "TETRAHEDRON"))
         (fec  (make <DG-FECollection> 1 3))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes)))
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddInteriorFaceIntegrator a
      (make <DGDiffusionIntegrator> one -1.0 4.0))
    (Assemble a)
    (Finalize a)
    (test-assert "DG 3D matrix assembled"
      (> (Height (SpMat a)) 0))))

(define runner (test-runner-current))
(test-end "mfem-dg")
(exit (zero? (test-runner-fail-count runner)))
