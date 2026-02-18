;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;                                MFEM Example 2
;;
;; Sample runs:  guile -L build ex2.scm
;;               guile -L build ex2.scm -- -m ../data/beam-tri.mesh
;;               guile -L build ex2.scm -- -m ../data/beam-quad.mesh
;;               guile -L build ex2.scm -- -m ../data/beam-tet.mesh
;;               guile -L build ex2.scm -- -m ../data/beam-hex.mesh
;;               guile -L build ex2.scm -- -m ../data/beam-wedge.mesh
;;               guile -L build ex2.scm -- -m ../data/beam-quad.mesh -o 3 -s
;;               guile -L build ex2.scm -- -m ../data/beam-quad-nurbs.mesh
;;               guile -L build ex2.scm -- -m ../data/beam-hex-nurbs.mesh
;;
;; Description:  This example code solves a simple linear elasticity problem
;;               describing a multi-material cantilever beam.
;;
;;               Specifically, we approximate the weak form of -div(sigma(u))=0
;;               where sigma(u)=lambda*div(u)*I+mu*(grad*u+u*grad) is the stress
;;               tensor corresponding to displacement field u, and lambda and mu
;;               are the material Lame constants. The boundary conditions are
;;               u=0 on the fixed part of the boundary with attribute 1, and
;;               sigma(u).n=f on the remainder with f being a constant pull down
;;               vector on boundary elements with attribute 2, and zero
;;               otherwise. The geometry of the domain is assumed to be as
;;               follows:
;;
;;                                 +----------+----------+
;;                    boundary --->| material | material |<--- boundary
;;                    attribute 1  |    1     |    2     |     attribute 2
;;                    (fixed)      +----------+----------+     (pull down)
;;
;;               The example demonstrates the use of high-order and NURBS vector
;;               finite element spaces with the linear elasticity bilinear form,
;;               meshes with curved elements, and the definition of piece-wise
;;               constant and vector coefficient objects. Static condensation is
;;               also illustrated.
;;
;;               We recommend viewing Example 1 before viewing this example.

(use-modules (oop goops) (mfem) (ice-9 getopt-long))

;; 1. Parse command-line options.
(define option-spec
  '((mesh        (single-char #\m) (value #t))
    (order       (single-char #\o) (value #t))
    (static-cond (single-char #\s) (value #f))))
(define options (getopt-long (command-line) option-spec))
(define mesh-file (option-ref options 'mesh "../data/beam-tri.mesh"))
(define order (string->number (option-ref options 'order "1")))
(define static-cond (option-ref options 'static-cond #f))

;; 2. Read the mesh from the given mesh file. We can handle triangular,
;;    quadrilateral, tetrahedral or hexahedral elements with the same code.
(define mesh (make <Mesh> mesh-file 1 1))
(define dim (Dimension mesh))

(when (or (< (Max (slot-ref mesh 'attributes)) 2)
          (< (Max (slot-ref mesh 'bdr-attributes)) 2))
  (format (current-error-port)
    "\nInput mesh should have at least two materials and ~
     two boundary attributes! (See schematic in ex2.cpp)\n\n")
  (exit 3))

;; 3. Select the order of the finite element discretization space. For NURBS
;;    meshes, we increase the order by degree elevation.
;; NOTE: Skip NURBS degree elevation (step 3) — SWIG Guile wraps null
;;       NURBSext as a truthy object, so the null-pointer check does not work.
;;       Non-NURBS meshes (the default beam-tri.mesh) work correctly.

;; 4. Refine the mesh to increase the resolution. In this example we do
;;    'ref_levels' of uniform refinement. We choose 'ref_levels' to be the
;;    largest number that gives a final mesh with no more than 5,000
;;    elements.
(define ref-levels
  (inexact->exact (floor (/ (log (/ 5000.0 (GetNE mesh)))
                            (* (log 2.0) dim)))))
(do ((l 0 (+ l 1))) ((>= l ref-levels))
  (UniformRefinement mesh))

;; 5. Define a finite element space on the mesh. Here we use vector finite
;;    elements, i.e. dim copies of a scalar finite element space. The vector
;;    dimension is specified by the last argument of the FiniteElementSpace
;;    constructor. For NURBS meshes, we use the (degree elevated) NURBS space
;;    associated with the mesh nodes.
;; NOTE: Skip NURBS FE space branch (step 5) — see step 3 note above.
(define fec (make <H1-FECollection> order dim))
(define fespace (make <FiniteElementSpace> mesh fec dim))
(format #t "Number of finite element unknowns: ~a~%" (GetTrueVSize fespace))
(format #t "Assembling: ")

;; 6. Determine the list of true (i.e. conforming) essential boundary dofs.
;;    In this example, the boundary conditions are defined by marking only
;;    boundary attribute 1 from the mesh as essential and converting it to a
;;    list of true dofs.
(define ess-tdof-list (make <intArray>))
(define ess-bdr (make <intArray> (Max (slot-ref mesh 'bdr-attributes))))
(Assign ess-bdr 0)
(set ess-bdr 0 1)
(GetEssentialTrueDofs fespace ess-bdr ess-tdof-list)

;; 7. Set up the linear form b(.) which corresponds to the right-hand side of
;;    the FEM linear system. In this case, b_i equals the boundary integral
;;    of f*phi_i where f represents a "pull down" force on the Neumann part
;;    of the boundary and phi_i are the basis functions in the finite element
;;    fespace. The force is defined by the VectorArrayCoefficient object f,
;;    which is a vector of Coefficient objects. The fact that f is non-zero
;;    on boundary attribute 2 is indicated by the use of piece-wise constants
;;    coefficient for its last component.
(define f (make <VectorArrayCoefficient> dim))
(do ((i 0 (+ i 1))) ((>= i (- dim 1)))
  (Set f i (make <ConstantCoefficient> 0.0)))
(let ((pull-force (make <Vector> (Max (slot-ref mesh 'bdr-attributes)))))
  (Assign pull-force 0.0)
  (set pull-force 1 -1.0e-2)
  (Set f (- dim 1) (make <PWConstCoefficient> pull-force)))

(define b (make <LinearForm> fespace))
(AddBoundaryIntegrator b (make <VectorBoundaryLFIntegrator> f))
(format #t "r.h.s. ... ")
(Assemble b)

;; 8. Define the solution vector x as a finite element grid function
;;    corresponding to fespace. Initialize x with initial guess of zero,
;;    which satisfies the boundary conditions.
(define x (make <GridFunction> fespace))
(Assign x 0.0)

;; 9. Set up the bilinear form a(.,.) on the finite element space
;;    corresponding to the linear elasticity integrator with piece-wise
;;    constants coefficient lambda and mu.
(define lambda-vec (make <Vector> (Max (slot-ref mesh 'attributes))))
(Assign lambda-vec 1.0)
(set lambda-vec 0 (* (get lambda-vec 1) 50))
(define lambda-func (make <PWConstCoefficient> lambda-vec))
(define mu-vec (make <Vector> (Max (slot-ref mesh 'attributes))))
(Assign mu-vec 1.0)
(set mu-vec 0 (* (get mu-vec 1) 50))
(define mu-func (make <PWConstCoefficient> mu-vec))

(define a (make <BilinearForm> fespace))
(AddDomainIntegrator a (make <ElasticityIntegrator> lambda-func mu-func))

;; 10. Assemble the bilinear form and the corresponding linear system,
;;     applying any necessary transformations such as: eliminating boundary
;;     conditions, applying conforming constraints for non-conforming AMR,
;;     static condensation, etc.
(format #t "matrix ... ")
(when static-cond (EnableStaticCondensation a))
(Assemble a)

(define A (make <OperatorHandle>))
(define B (make <Vector>))
(define X (make <Vector>))
(FormLinearSystem a ess-tdof-list x b A X B)
(format #t "done.~%")

(format #t "Size of linear system: ~a~%" (Height (Ptr A)))

;; 11. Define a simple symmetric Gauss-Seidel preconditioner and use it to
;;     solve the system Ax=b with PCG.
;; NOTE: Skip SuiteSparse path (#ifdef MFEM_USE_SUITESPARSE)
(define M (make <GSSmoother> (OperatorHandle2SparseMatrix A)))
(PCG (Ptr A) M B X 1 500 1e-8 0.0)

;; 12. Recover the solution as a finite element grid function.
(RecoverFEMSolution a X b x)

;; 13. For non-NURBS meshes, make the mesh curved based on the finite element
;;     space. This means that we define the mesh elements through a fespace
;;     based transformation of the reference element. This allows us to save
;;     the displaced mesh as a curved mesh when using high-order finite
;;     element displacement field. We assume that the initial mesh (read from
;;     the file) is not higher order curved mesh compared to the chosen FE
;;     space.
;; NOTE: Skip NURBS check — always set the nodal FE space (see step 3 note).
;; NOTE: Use primitive call as workaround for cross-module GOOPS dispatch
;;       issue (mesh module does not import fespace module).
((@@ (mfem mesh) primitive:Mesh-SetNodalFESpace) mesh fespace)

;; 14. Save the displaced mesh and the inverted solution (which gives the
;;     backward displacements to the original grid). This output can be
;;     viewed later using GLVis: "glvis -m displaced.mesh -g sol.gf".
(let ((nodes (GetNodes mesh)))
  (Add nodes 1.0 x)
  (Neg x)
  (Print mesh "displaced.mesh" 8)
  (Save x "sol.gf" 8))

;; NOTE: Skip GLVis visualization (step 15)
;; NOTE: Skip memory cleanup (step 16) — Guile GC handles this
