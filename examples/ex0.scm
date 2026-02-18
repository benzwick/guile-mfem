;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;                                MFEM Example 0
;;
;; Sample runs:  guile -L build ex0.scm
;;               guile -L build ex0.scm -- -m ../data/fichera.mesh
;;               guile -L build ex0.scm -- -m ../data/square-disc.mesh -o 2
;;
;; Description: This example code demonstrates the most basic usage of MFEM to
;;              define a simple finite element discretization of the Poisson
;;              problem -Delta u = 1 with zero Dirichlet boundary conditions.
;;              General 2D/3D mesh files and finite element polynomial degrees
;;              can be specified by command line options.

(use-modules (oop goops) (mfem) (ice-9 getopt-long))

;; 1. Parse command line options.
(define option-spec
  '((mesh  (single-char #\m) (value #t))
    (order (single-char #\o) (value #t))))
(define options (getopt-long (command-line) option-spec))
(define mesh-file (option-ref options 'mesh "../data/star.mesh"))
(define order (string->number (option-ref options 'order "1")))

;; 2. Read the mesh from the given mesh file, and refine once uniformly.
(define mesh (make <Mesh> mesh-file 1 1))
(UniformRefinement mesh)

;; 3. Define a finite element space on the mesh. Here we use H1 continuous
;;    high-order Lagrange finite elements of the given order.
(define fec (make <H1-FECollection> order (Dimension mesh)))
(define fespace (make <FiniteElementSpace> mesh fec))
(format #t "Number of unknowns: ~a~%" (GetTrueVSize fespace))

;; 4. Extract the list of all the boundary DOFs. These will be marked as
;;    Dirichlet in order to enforce zero boundary conditions.
(define boundary-dofs (make <intArray>))
(GetBoundaryTrueDofs fespace boundary-dofs)

;; 5. Define the solution x as a finite element grid function in fespace. Set
;;    the initial guess to zero, which also sets the boundary conditions.
(define x (make <GridFunction> fespace))
(Assign x 0.0)

;; 6. Set up the linear form b(.) corresponding to the right-hand side.
(define one (make <ConstantCoefficient> 1.0))
(define b (make <LinearForm> fespace))
(AddDomainIntegrator b (make <DomainLFIntegrator> one))
(Assemble b)

;; 7. Set up the bilinear form a(.,.) corresponding to the -Delta operator.
(define a (make <BilinearForm> fespace))
(AddDomainIntegrator a (make <DiffusionIntegrator>))
(Assemble a)

;; 8. Form the linear system A X = B. This includes eliminating boundary
;;    conditions, applying AMR constraints, and other transformations.
(define A (make <OperatorHandle>))
(define B (make <Vector>))
(define X (make <Vector>))
(FormLinearSystem a boundary-dofs x b A X B)

;; 9. Solve the system using PCG with symmetric Gauss-Seidel preconditioner.
(define M (make <GSSmoother> (OperatorHandle2SparseMatrix A)))
(PCG (Ptr A) M B X 1 200 1e-12 0.0)

;; 10. Recover the solution x as a grid function and save to file. The output
;;     can be viewed using GLVis as follows: "glvis -m mesh.mesh -g sol.gf"
(RecoverFEMSolution a X b x)
(Save x "sol.gf")
(Save mesh "mesh.mesh")
