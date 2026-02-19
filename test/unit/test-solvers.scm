;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM iterative solvers (from MFEM test_cg_indefinite.cpp).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem vector) (mfem operators)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg)
             (mfem bilinearform) (mfem lininteg) (mfem linearform)
             (mfem sparsemat)
             (mfem solvers) (mfem sparsesmoothers) (mfem gridfunc))
(use-modules (mesh-primitive) (fe_coll-primitive) (fespace-primitive)
             (coefficient-primitive) (bilininteg-primitive)
             (bilinearform-primitive) (lininteg-primitive)
             (linearform-primitive) (solvers-primitive)
             (sparsesmoothers-primitive) (gridfunc-primitive))

(test-begin "mfem-solvers")

;; CGSolver on a simple reaction-diffusion problem
(test-group "cg-solver"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         ;; Bilinear form: stiffness + mass matrix (positive definite)
         (a    (new-BilinearForm fes))
         ;; Linear form: RHS
         (b    (new-LinearForm fes))
         (x    (new-GridFunction fes)))
    (BilinearForm-AddDomainIntegrator a (new-DiffusionIntegrator one))
    (BilinearForm-AddDomainIntegrator a (new-MassIntegrator one))
    (BilinearForm-Assemble a)
    (BilinearForm-Finalize a)
    (LinearForm-AddDomainIntegrator b (new-DomainLFIntegrator one))
    (LinearForm-Assemble b)
    (GridFunction-Assign x 0.0)
    ;; Set up CG solver — SetRelTol etc. are inherited from IterativeSolver
    (let ((cg (new-CGSolver)))
      (SetOperator cg (BilinearForm-SpMat a))
      (SetRelTol cg 1e-12)
      (SetMaxIter cg 200)
      (SetPrintLevel cg 0)
      (Mult cg b x)
      (test-assert "CG converged" (GetConverged cg))
      (test-assert "CG iterations > 0" (> (GetNumIterations cg) 0)))))

;; PCG with GSSmoother preconditioner
(test-group "pcg-with-gs"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (a    (new-BilinearForm fes))
         (b    (new-LinearForm fes))
         (x    (new-GridFunction fes)))
    (BilinearForm-AddDomainIntegrator a (new-DiffusionIntegrator one))
    (BilinearForm-AddDomainIntegrator a (new-MassIntegrator one))
    (BilinearForm-Assemble a)
    (BilinearForm-Finalize a)
    (LinearForm-AddDomainIntegrator b (new-DomainLFIntegrator one))
    (LinearForm-Assemble b)
    (GridFunction-Assign x 0.0)
    (let ((pcg (new-CGSolver))
          (gs  (new-GSSmoother (BilinearForm-SpMat a))))
      (SetOperator pcg (BilinearForm-SpMat a))
      (SetPreconditioner pcg gs)
      (SetRelTol pcg 1e-12)
      (SetMaxIter pcg 200)
      (SetPrintLevel pcg 0)
      (Mult pcg b x)
      (test-assert "PCG converged" (GetConverged pcg)))))

(define runner (test-runner-current))
(test-end "mfem-solvers")
(exit (zero? (test-runner-fail-count runner)))
