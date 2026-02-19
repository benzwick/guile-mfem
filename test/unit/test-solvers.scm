;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM iterative solvers (from MFEM test_cg_indefinite.cpp).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem))

(test-begin "mfem-solvers")

;; CGSolver on a simple reaction-diffusion problem
(test-group "cg-solver"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         ;; Bilinear form: stiffness + mass matrix (positive definite)
         (a    (make <BilinearForm> fes))
         ;; Linear form: RHS
         (b    (make <LinearForm> fes))
         (x    (make <GridFunction> fes)))
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (AddDomainIntegrator b (make <DomainLFIntegrator> one))
    (Assemble b)
    (Assign x 0.0)
    ;; Set up CG solver — SetRelTol etc. are inherited from IterativeSolver
    (let ((cg (make <CGSolver>)))
      (SetOperator cg (SpMat a))
      (SetRelTol cg 1e-12)
      (SetMaxIter cg 200)
      (SetPrintLevel cg 0)
      (Mult cg b x)
      (test-assert "CG converged" (GetConverged cg))
      (test-assert "CG iterations > 0" (> (GetNumIterations cg) 0)))))

;; PCG with GSSmoother preconditioner
(test-group "pcg-with-gs"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (b    (make <LinearForm> fes))
         (x    (make <GridFunction> fes)))
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (AddDomainIntegrator b (make <DomainLFIntegrator> one))
    (Assemble b)
    (Assign x 0.0)
    (let ((pcg (make <CGSolver>))
          (gs  (make <GSSmoother> (SpMat a))))
      (SetOperator pcg (SpMat a))
      (SetPreconditioner pcg gs)
      (SetRelTol pcg 1e-12)
      (SetMaxIter pcg 200)
      (SetPrintLevel pcg 0)
      (Mult pcg b x)
      (test-assert "PCG converged" (GetConverged pcg)))))

(define runner (test-runner-current))
(test-end "mfem-solvers")
(exit (zero? (test-runner-fail-count runner)))
