;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for boundary condition workflows.
;; Tests GetBdrArray, GetEssentialTrueDofs, essential DOF marking,
;; and ProjectBdrCoefficient.

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg) (mfem lininteg)
             (mfem bilinearform) (mfem linearform) (mfem sparsemat)
             (mfem vector) (mfem operators) (mfem array)
             (mfem gridfunc) (mfem handle) (mfem solvers)
             (mfem sparsesmoothers))

(test-begin "mfem-boundary")

;; Mesh boundary attributes
(test-group "mesh-bdr-attributes"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL")))
    (test-assert "GetNBE > 0" (> (GetNBE mesh) 0))))

;; GetEssentialTrueDofs with all boundaries marked
(test-group "essential-tdofs-all"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (ess-bdr  (make <intArray> (Max (slot-ref mesh 'bdr-attributes))))
         (ess-tdof (make <intArray>)))
    (Assign ess-bdr 1)
    (GetEssentialTrueDofs fes ess-bdr ess-tdof)
    (test-assert "essential DOFs found" (> (Size ess-tdof) 0))
    (test-assert "essential DOFs < total DOFs"
      (< (Size ess-tdof) (GetVSize fes)))))

;; GetEssentialTrueDofs with no boundaries marked
(test-group "essential-tdofs-none"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (ess-bdr  (make <intArray> (Max (slot-ref mesh 'bdr-attributes))))
         (ess-tdof (make <intArray>)))
    (Assign ess-bdr 0)
    (GetEssentialTrueDofs fes ess-bdr ess-tdof)
    (test-assert "no essential DOFs when none marked"
      (= (Size ess-tdof) 0))))

;; ProjectBdrCoefficient sets boundary values
(test-group "project-bdr-coefficient"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (x    (make <GridFunction> fes))
         (one  (make <ConstantCoefficient> 1.0))
         (ess-bdr (make <intArray> (Max (slot-ref mesh 'bdr-attributes)))))
    (Assign x 0.0)
    (Assign ess-bdr 1)
    (ProjectBdrCoefficient x one ess-bdr)
    ;; After projecting 1.0 on boundary, grid function should have
    ;; nonzero values (boundary DOFs set to 1.0)
    (test-assert "boundary projection: max > 0" (> (Max x) 0.0))))

;; Solve with Dirichlet BC: -Δu = 1 on [0,1]², u = 0 on boundary
(test-group "solve-with-dirichlet"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (b    (make <LinearForm> fes))
         (x    (make <GridFunction> fes))
         (A    (make <OperatorHandle>))
         (B    (make <Vector>))
         (X    (make <Vector>))
         (ess-bdr  (make <intArray> (Max (slot-ref mesh 'bdr-attributes))))
         (ess-tdof (make <intArray>)))
    ;; Mark all boundary DOFs as essential (u=0)
    (Assign ess-bdr 1)
    (GetEssentialTrueDofs fes ess-bdr ess-tdof)
    ;; Stiffness matrix
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (Assemble a)
    ;; RHS
    (AddDomainIntegrator b (make <DomainLFIntegrator> one))
    (Assemble b)
    ;; Initial guess
    (Assign x 0.0)
    ;; Form system with BC elimination
    (FormLinearSystem a ess-tdof x b A X B)
    ;; Solve
    (let* ((AA (OperatorHandle2SparseMatrix A))
           (M  (make <GSSmoother> AA)))
      (PCG (Ptr A) M B X 0 500 1e-12 0.0)
      (RecoverFEMSolution a X b x)
      ;; Solution should be positive in the interior
      (test-assert "Dirichlet solve: max > 0" (> (Max x) 0.0)))))

;; 3D boundary conditions on tet mesh
(test-group "boundary-3d"
  (let* ((mesh (make <Mesh> 2 2 2 "TETRAHEDRON"))
         (fec  (make <H1-FECollection> 1 3))
         (fes  (make <FiniteElementSpace> mesh fec))
         (ess-bdr  (make <intArray> (Max (slot-ref mesh 'bdr-attributes))))
         (ess-tdof (make <intArray>)))
    (Assign ess-bdr 1)
    (GetEssentialTrueDofs fes ess-bdr ess-tdof)
    (test-assert "3D: essential DOFs found" (> (Size ess-tdof) 0))))

(define runner (test-runner-current))
(test-end "mfem-boundary")
(exit (zero? (test-runner-fail-count runner)))
