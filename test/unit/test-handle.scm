;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for OperatorHandle (OperatorPtr) and FormLinearSystem workflow.
;; Tests OperatorHandle creation, OperatorHandle2SparseMatrix conversion,
;; FormLinearSystem, RecoverFEMSolution, and the Ptr accessor.

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg) (mfem lininteg)
             (mfem bilinearform) (mfem linearform) (mfem sparsemat)
             (mfem vector) (mfem operators) (mfem array)
             (mfem gridfunc) (mfem handle) (mfem solvers)
             (mfem sparsesmoothers))

(test-begin "mfem-handle")

;; OperatorHandle creation
(test-group "operatorhandle-create"
  (let ((A (make <OperatorHandle>)))
    (test-assert "OperatorHandle created" A)))

;; FormLinearSystem with empty essential DOF list (no Dirichlet BC)
(test-group "form-linear-system-no-bc"
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
         (ess  (make <intArray>)))
    ;; Reaction-diffusion (positive definite without BC)
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (AddDomainIntegrator b (make <DomainLFIntegrator> one))
    (Assemble b)
    (Assign x 0.0)
    (FormLinearSystem a ess x b A X B)
    (test-assert "FormLinearSystem: B size > 0" (> (Size B) 0))
    (test-assert "FormLinearSystem: X size > 0" (> (Size X) 0))
    (test-assert "FormLinearSystem: Ptr not null" (Ptr A))))

;; OperatorHandle2SparseMatrix conversion
(test-group "operatorhandle-to-sparsemat"
  (let* ((mesh (make <Mesh> 2 2 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (b    (make <LinearForm> fes))
         (x    (make <GridFunction> fes))
         (A    (make <OperatorHandle>))
         (B    (make <Vector>))
         (X    (make <Vector>))
         (ess  (make <intArray>)))
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (AddDomainIntegrator b (make <DomainLFIntegrator> one))
    (Assemble b)
    (Assign x 0.0)
    (FormLinearSystem a ess x b A X B)
    (let ((AA (OperatorHandle2SparseMatrix A)))
      (test-assert "Converted to SparseMatrix" AA)
      (test-assert "SparseMatrix height > 0" (> (Height AA) 0))
      (test-assert "SparseMatrix is square" (= (Height AA) (Width AA))))))

;; Full solve workflow: FormLinearSystem -> PCG -> RecoverFEMSolution
(test-group "full-solve-workflow"
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
         (ess  (make <intArray>)))
    ;; Reaction-diffusion: -Δu + u = 1
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (AddDomainIntegrator b (make <DomainLFIntegrator> one))
    (Assemble b)
    (Assign x 0.0)
    (FormLinearSystem a ess x b A X B)
    ;; Solve
    (let* ((AA (OperatorHandle2SparseMatrix A))
           (M  (make <GSSmoother> AA)))
      (PCG (Ptr A) M B X 0 200 1e-12 0.0)
      ;; Recover solution
      (RecoverFEMSolution a X b x)
      (test-assert "solution max > 0" (> (Max x) 0.0)))))

;; FormLinearSystem with essential (Dirichlet) boundary conditions
(test-group "form-linear-system-with-bc"
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
         (ess-bdr  (make <intArray> (GetNBE mesh)))
         (ess-tdof (make <intArray>)))
    ;; Mark all boundaries as essential
    (Assign ess-bdr 1)
    (GetEssentialTrueDofs fes ess-bdr ess-tdof)
    ;; Diffusion + mass
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (AddDomainIntegrator b (make <DomainLFIntegrator> one))
    (Assemble b)
    (Assign x 0.0)
    (FormLinearSystem a ess-tdof x b A X B)
    ;; After applying BC, system should be smaller or equal
    (test-assert "FormLinearSystem with BC: B size > 0" (> (Size B) 0))
    ;; Solve
    (let* ((AA (OperatorHandle2SparseMatrix A))
           (M  (make <GSSmoother> AA)))
      (PCG (Ptr A) M B X 0 500 1e-12 0.0)
      (RecoverFEMSolution a X b x)
      ;; With u=0 on boundary and f=1 RHS, solution should be positive inside
      (test-assert "BC solve: solution max > 0" (> (Max x) 0.0)))))

(define runner (test-runner-current))
(test-end "mfem-handle")
(exit (zero? (test-runner-fail-count runner)))
