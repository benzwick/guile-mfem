;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;                                MFEM Example 1
;;
;; Sample runs:  guile -L build ex1.scm
;;               guile -L build ex1.scm -m ../data/square-disc.mesh
;;               guile -L build ex1.scm -m ../data/star.mesh
;;               guile -L build ex1.scm -m ../data/star-mixed.mesh
;;               guile -L build ex1.scm -m ../data/escher.mesh
;;               guile -L build ex1.scm -m ../data/fichera.mesh
;;               guile -L build ex1.scm -m ../data/fichera-mixed.mesh
;;               guile -L build ex1.scm -m ../data/toroid-wedge.mesh
;;               guile -L build ex1.scm -m ../data/octahedron.mesh -o 1
;;               guile -L build ex1.scm -m ../data/periodic-annulus-sector.msh
;;               guile -L build ex1.scm -m ../data/periodic-torus-sector.msh
;;               guile -L build ex1.scm -m ../data/square-disc-p2.vtk -o 2
;;               guile -L build ex1.scm -m ../data/square-disc-p3.mesh -o 3
;;               guile -L build ex1.scm -m ../data/square-disc-nurbs.mesh -o -1
;;               guile -L build ex1.scm -m ../data/star-mixed-p2.mesh -o 2
;;               guile -L build ex1.scm -m ../data/disc-nurbs.mesh -o -1
;;               guile -L build ex1.scm -m ../data/pipe-nurbs.mesh -o -1
;;               guile -L build ex1.scm -m ../data/fichera-mixed-p2.mesh -o 2
;;               guile -L build ex1.scm -m ../data/star-surf.mesh
;;               guile -L build ex1.scm -m ../data/square-disc-surf.mesh
;;               guile -L build ex1.scm -m ../data/inline-segment.mesh
;;               guile -L build ex1.scm -m ../data/amr-quad.mesh
;;               guile -L build ex1.scm -m ../data/amr-hex.mesh
;;               guile -L build ex1.scm -m ../data/fichera-amr.mesh
;;               guile -L build ex1.scm -m ../data/mobius-strip.mesh
;;               guile -L build ex1.scm -m ../data/mobius-strip.mesh -o -1 -s
;;               guile -L build ex1.scm -m ../data/nc3-nurbs.mesh -o -1
;;
;; Description:  This example code demonstrates the use of MFEM to define a
;;               simple finite element discretization of the Poisson problem
;;               -Delta u = 1 with homogeneous Dirichlet boundary conditions.
;;               Specifically, we discretize using a FE space of the specified
;;               order, or if order < 1 using an isoparametric/isogeometric
;;               space (i.e. quadratic for quadratic curvilinear mesh, NURBS for
;;               NURBS mesh, etc.)
;;
;;               The example highlights the use of mesh refinement, finite
;;               element grid functions, as well as linear and bilinear forms
;;               corresponding to the left-hand side and right-hand side of the
;;               discrete linear system. We also cover the explicit elimination
;;               of essential boundary conditions, static condensation, and the
;;               optional connection to the GLVis tool for visualization.

(use-modules (oop goops) (mfem) (ice-9 getopt-long))

;; 1. Parse command-line options.
(define option-spec
  '((mesh        (single-char #\m) (value #t))
    (order       (single-char #\o) (value #t))
    (static-cond (single-char #\s) (value #f))
    (device      (single-char #\d) (value #t))))
(define options (getopt-long (command-line) option-spec))
(define mesh-file (option-ref options 'mesh "../data/star.mesh"))
(define order (string->number (option-ref options 'order "1")))
(define static-cond (option-ref options 'static-cond #f))
(define device-config (option-ref options 'device "cpu"))
;; NOTE: Partial assembly (-pa) and full assembly (-fa) options are not yet
;;       supported. Only standard assembly with GSSmoother + PCG is used.

;; 2. Enable hardware devices such as GPUs, and programming models such as
;;    CUDA, OCCA, RAJA and OpenMP based on command line options.
(define dev (make <Device> device-config))
(Print dev)

;; 3. Read the mesh from the given mesh file. We can handle triangular,
;;    quadrilateral, tetrahedral, hexahedral, surface and volume meshes with
;;    the same code.
(define mesh (make <Mesh> mesh-file 1 1))
(define dim (Dimension mesh))

;; 4. Refine the mesh to increase the resolution. In this example we do
;;    'ref_levels' of uniform refinement. We choose 'ref_levels' to be the
;;    largest number that gives a final mesh with no more than 50,000
;;    elements.
(define ref-levels
  (inexact->exact (floor (/ (log (/ 50000.0 (GetNE mesh)))
                            (* (log 2.0) dim)))))
(do ((l 0 (+ l 1))) ((>= l ref-levels))
  (UniformRefinement mesh))

;; 5. Define a finite element space on the mesh. Here we use continuous
;;    Lagrange finite elements of the specified order. If order < 1, we
;;    instead use an isoparametric/isogeometric space.
;; BUG: Isoparametric path (order <= 0) is broken — GetNodes returns a
;;      truthy null pointer (see BUGS.md "Null C++ pointers are truthy in
;;      Scheme"). Error out until the bug is fixed; remove this guard and
;;      the code below will work correctly.
(when (<= order 0)
  (error "Isoparametric/NURBS FE space (order <= 0) is not supported: \
GetNodes returns a truthy null pointer (see BUGS.md)"))
(define fec
  (cond
    ((> order 0)
     (make <H1-FECollection> order dim))
    ((GetNodes mesh)
     => (lambda (nodes)
          (let ((node-fec (OwnFEC nodes)))
            (format #t "Using isoparametric FEs: ~a~%" (Name node-fec))
            node-fec)))
    (else
     (set! order 1)
     (make <H1-FECollection> order dim))))
(define fespace (make <FiniteElementSpace> mesh fec))
(format #t "Number of finite element unknowns: ~a~%" (GetTrueVSize fespace))

;; 6. Determine the list of true (i.e. conforming) essential boundary dofs.
;;    In this example, the boundary conditions are defined by marking all
;;    the external boundary attributes from the mesh as essential (Dirichlet)
;;    and converting them to a list of true dofs.
(define ess-tdof-list (make <intArray>))
(when (> (Size (slot-ref mesh 'bdr-attributes)) 0)
  (let ((ess-bdr (make <intArray> (Max (slot-ref mesh 'bdr-attributes)))))
    (Assign ess-bdr 0)
    ;; Apply boundary conditions on all external boundaries:
    (MarkExternalBoundaries mesh ess-bdr)
    ;; Boundary conditions can also be applied based on named attributes:
    ;; (MarkNamedBoundaries mesh set-name ess-bdr)
    (GetEssentialTrueDofs fespace ess-bdr ess-tdof-list)))

;; 7. Set up the linear form b(.) which corresponds to the right-hand side of
;;    the FEM linear system, which in this case is (1,phi_i) where phi_i are
;;    the basis functions in the finite element fespace.
(define b (make <LinearForm> fespace))
(define one (make <ConstantCoefficient> 1.0))
(AddDomainIntegrator b (make <DomainLFIntegrator> one))
(Assemble b)

;; 8. Define the solution vector x as a finite element grid function
;;    corresponding to fespace. Initialize x with initial guess of zero,
;;    which satisfies the boundary conditions.
(define x (make <GridFunction> fespace))
(Assign x 0.0)

;; 9. Set up the bilinear form a(.,.) on the finite element space
;;    corresponding to the Laplacian operator -Delta, by adding the Diffusion
;;    domain integrator.
;; NOTE: Partial assembly (-pa) and full assembly (-fa) paths
;;       (SetAssemblyLevel, EnableSparseMatrixSorting) are not yet supported.
(define a (make <BilinearForm> fespace))
(AddDomainIntegrator a (make <DiffusionIntegrator> one))

;; 10. Assemble the bilinear form and the corresponding linear system,
;;     applying any necessary transformations such as: eliminating boundary
;;     conditions, applying conforming constraints for non-conforming AMR,
;;     static condensation, etc.
(when static-cond (EnableStaticCondensation a))
(Assemble a)

(define A (make <OperatorHandle>))
(define B (make <Vector>))
(define X (make <Vector>))
(FormLinearSystem a ess-tdof-list x b A X B)

(format #t "Size of linear system: ~a~%" (Height (Ptr A)))

;; 11. Solve the linear system A X = B.
;; NOTE: Skip SuiteSparse path (#ifdef MFEM_USE_SUITESPARSE)
;; NOTE: Skip partial assembly path (UsesTensorBasis, OperatorJacobiSmoother)
;; NOTE: Skip algebraic CEED path (#ifdef MFEM_USE_CEED)
;; Use a simple symmetric Gauss-Seidel preconditioner with PCG.
(define M (make <GSSmoother> (OperatorHandle2SparseMatrix A)))
(PCG (Ptr A) M B X 1 200 1e-12 0.0)

;; 12. Recover the solution as a finite element grid function.
(RecoverFEMSolution a X b x)

;; 13. Save the refined mesh and the solution. This output can be viewed later
;;     using GLVis: "glvis -m refined.mesh -g sol.gf".
(Print mesh "refined.mesh" 8)
(Save x "sol.gf" 8)

;; NOTE: Skip GLVis visualization (step 14)
;; NOTE: Skip memory cleanup (step 15) — Guile GC handles this
