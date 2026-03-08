;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;                                MFEM Example 14
;;
;; Sample runs:  guile -L build ex14.scm
;;               guile -L build ex14.scm -m ../data/star.mesh
;;               guile -L build ex14.scm -m ../data/escher.mesh
;;               guile -L build ex14.scm -m ../data/fichera.mesh
;;               guile -L build ex14.scm -m ../data/inline-quad.mesh
;;               guile -L build ex14.scm -m ../data/inline-hex.mesh
;;               guile -L build ex14.scm -m ../data/star.mesh -o 2
;;               guile -L build ex14.scm -m ../data/inline-quad.mesh -o 2
;;
;; Description:  This example code demonstrates the use of MFEM to define a
;;               discontinuous Galerkin (DG) finite element discretization of
;;               the Laplace problem -Delta u = 1 with homogeneous Dirichlet
;;               boundary conditions. Finite element spaces of any order,
;;               including zero on regular grids, are supported. The example
;;               highlights the use of discontinuous spaces and DG-specific face
;;               integrators.

(use-modules (oop goops) (mfem) (ice-9 getopt-long))

;; 1. Parse command-line options.
(define option-spec
  '((mesh  (single-char #\m) (value #t))
    (order (single-char #\o) (value #t))))
(define options (getopt-long (command-line) option-spec))
(define mesh-file (option-ref options 'mesh "../data/star.mesh"))
(define order (string->number (option-ref options 'order "1")))

;; Parameters for the DG method:
;;   sigma: symmetry parameter (-1 = SIPG, 0 = IIPG, 1 = NIPG)
;;   kappa: penalty parameter (auto-set to (order+1)^2 if negative)
(define sigma -1.0)
(define kappa (expt (+ order 1.0) 2.0))

;; 2. Read the mesh from the given mesh file. We can handle triangular,
;;    quadrilateral, tetrahedral, hexahedral, surface and volume meshes with
;;    the same code.
(define mesh (make <Mesh> mesh-file 1 1))
(define dim (Dimension mesh))

;; 3. Refine the mesh to increase the resolution. In this example we do
;;    'ref_levels' of uniform refinement. We choose 'ref_levels' to be the
;;    largest number that gives a final mesh with no more than 50,000
;;    elements.
(define ref-levels
  (inexact->exact (floor (/ (log (/ 50000.0 (GetNE mesh)))
                            (* (log 2.0) dim)))))
(do ((l 0 (+ l 1))) ((>= l ref-levels))
  (UniformRefinement mesh))

;; 4. Define a finite element space on the mesh. Here we use discontinuous
;;    finite elements of the specified order >= 0.
(define fec (make <DG-FECollection> order dim))
(define fespace (make <FiniteElementSpace> mesh fec))
(format #t "Number of finite element unknowns: ~a~%" (GetVSize fespace))

;; 5. Set up the linear form b(.) which corresponds to the right-hand side of
;;    the FEM linear system.
(define one (make <ConstantCoefficient> 1.0))
(define zero (make <ConstantCoefficient> 0.0))
(define b (make <LinearForm> fespace))
(AddDomainIntegrator b (make <DomainLFIntegrator> one))
(AddBdrFaceIntegrator b
  (make <DGDirichletLFIntegrator> zero one sigma kappa))
(Assemble b)

;; 6. Define the solution vector x as a finite element grid function
;;    corresponding to fespace. Initialize x with initial guess of zero.
(define x (make <GridFunction> fespace))
(Assign x 0.0)

;; 7. Set up the bilinear form a(.,.) on the finite element space
;;    corresponding to the Laplacian operator -Delta, by adding the Diffusion
;;    domain integrator and the interior and boundary DG face integrators.
;;    Note that boundary conditions are imposed weakly in the form, so there
;;    is no need for dof elimination. After assembly and finalizing we
;;    extract the corresponding sparse matrix A.
(define a (make <BilinearForm> fespace))
(AddDomainIntegrator a (make <DiffusionIntegrator> one))
(AddInteriorFaceIntegrator a
  (make <DGDiffusionIntegrator> one sigma kappa))
(AddBdrFaceIntegrator a
  (make <DGDiffusionIntegrator> one sigma kappa))
(Assemble a)
(Finalize a)
(define A (SpMat a))

;; 8. Define a simple symmetric Gauss-Seidel preconditioner and use it to
;;    solve the system Ax=b with PCG in the symmetric case, and GMRES in the
;;    non-symmetric one.
(define M (make <GSSmoother> A))
(if (= sigma -1.0)
    (PCG A M b x 1 500 1e-12 0.0)
    (GMRES A M b x 1 500 10 1e-12 0.0))

;; 9. Save the refined mesh and the solution. This output can be viewed later
;;    using GLVis: "glvis -m refined.mesh -g sol.gf".
(Print mesh "refined.mesh" 8)
(Save x "sol.gf" 8)

;; NOTE: Skip GLVis visualization (step 10)
