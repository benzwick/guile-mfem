;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;                                MFEM Example 3
;;
;; Sample runs:  guile -L build ex3.scm
;;               guile -L build ex3.scm -m ../data/fichera.mesh
;;               guile -L build ex3.scm -m ../data/beam-tet.mesh
;;               guile -L build ex3.scm -m ../data/escher.mesh
;;               guile -L build ex3.scm -m ../data/fichera-q2.mesh -o 2
;;               guile -L build ex3.scm -m ../data/fichera-q3.mesh -o 3
;;               guile -L build ex3.scm -m ../data/star-surf.mesh -o 1
;;               guile -L build ex3.scm -m ../data/beam-tet.mesh -s
;;
;; Description:  This example code solves a simple electromagnetic diffusion
;;               problem corresponding to the definite Maxwell problem
;;               curl curl E + E = f with boundary condition E x n = <given>.
;;               Here, we use a given f and set the boundary condition using
;;               the essential boundary method.
;;
;;               The example demonstrates the use of H(curl) finite element
;;               spaces with the curl-curl and the (vector) finite element mass
;;               integrators, as well as the computation of discretization
;;               error when the exact solution is known. Static condensation is
;;               also illustrated.
;;
;;               We recommend viewing examples 1-2 before viewing this example.
;;
;; NOTE: The exact solution E_exact and corresponding right-hand side f_exact
;;       require VectorFunctionCoefficient with Scheme callbacks (SWIG director
;;       support), which is not yet available. This port uses
;;       VectorConstantCoefficient for the RHS and zero initial guess,
;;       demonstrating the Nedelec finite element API without exact error
;;       computation. See BUGS.md "SWIG director support for coefficients".

(use-modules (oop goops) (mfem) (ice-9 getopt-long))

;; 1. Parse command-line options.
(define option-spec
  '((mesh        (single-char #\m) (value #t))
    (order       (single-char #\o) (value #t))
    (static-cond (single-char #\s) (value #f))
    (device      (single-char #\d) (value #t))))
(define options (getopt-long (command-line) option-spec))
(define mesh-file (option-ref options 'mesh "../data/beam-tet.mesh"))
(define order (string->number (option-ref options 'order "1")))
(define static-cond (option-ref options 'static-cond #f))
(define device-config (option-ref options 'device "cpu"))

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

;; 5. Define a finite element space on the mesh. Here we use the Nedelec
;;    finite elements of the specified order.
(define fec (make <ND-FECollection> order dim))
(define fespace (make <FiniteElementSpace> mesh fec))
(format #t "Number of finite element unknowns: ~a~%" (GetTrueVSize fespace))

;; 6. Determine the list of true (i.e. conforming) essential boundary dofs.
;;    In this example, the boundary conditions are defined by marking all
;;    the boundary attributes from the mesh as essential (Dirichlet) and
;;    converting them to a list of true dofs.
(define ess-tdof-list (make <intArray>))
(when (> (Size (slot-ref mesh 'bdr-attributes)) 0)
  (let ((ess-bdr (make <intArray> (Max (slot-ref mesh 'bdr-attributes)))))
    (Assign ess-bdr 1)
    (GetEssentialTrueDofs fespace ess-bdr ess-tdof-list)))

;; 7. Set up the linear form b(.) which corresponds to the right-hand side
;;    of the FEM linear system, which in this case is (f,phi_i) where f is
;;    given by the function f_exact and phi_i are the basis functions in the
;;    finite element fespace.
;; NOTE: The C++ example uses a VectorFunctionCoefficient for f_exact.
;;       Since SWIG director support is not yet available, we use a constant
;;       vector RHS f = (1, 1, 1) (or (1, 1) in 2D) which changes the
;;       problem but still demonstrates the H(curl) FEM workflow.
(define f-vec (make <Vector> dim))
(Assign f-vec 1.0)
(define f-coeff (make <VectorConstantCoefficient> f-vec))
(define b (make <LinearForm> fespace))
(AddDomainIntegrator b (make <VectorFEDomainLFIntegrator> f-coeff))
(Assemble b)

;; 8. Define the solution vector x as a finite element grid function
;;    corresponding to fespace. Initialize x with zero.
;; NOTE: The C++ example projects E_exact onto x. We skip this because
;;       VectorFunctionCoefficient requires director support.
(define x (make <GridFunction> fespace))
(Assign x 0.0)

;; 9. Set up the bilinear form corresponding to the EM diffusion operator
;;    curl muinv curl + sigma I, by adding the curl-curl and the mass domain
;;    integrators.
(define muinv (make <ConstantCoefficient> 1.0))
(define sigma (make <ConstantCoefficient> 1.0))
(define a (make <BilinearForm> fespace))
(AddDomainIntegrator a (make <CurlCurlIntegrator> muinv))
(AddDomainIntegrator a (make <VectorFEMassIntegrator> sigma))

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

;; 11. Solve the system using PCG with symmetric Gauss-Seidel preconditioner.
;; NOTE: Skip SuiteSparse path (#ifdef MFEM_USE_SUITESPARSE)
(define M (make <GSSmoother> (OperatorHandle2SparseMatrix A)))
(PCG (Ptr A) M B X 1 500 1e-12 0.0)

;; 12. Recover the solution as a finite element grid function.
(RecoverFEMSolution a X b x)

;; 13. Compute and print the L^2 norm of the error.
;; NOTE: Skip L^2 error computation — requires VectorFunctionCoefficient
;;       with director support for E_exact.

;; 14. Save the refined mesh and the solution. This output can be viewed later
;;     using GLVis: "glvis -m refined.mesh -g sol.gf".
(Print mesh "refined.mesh" 8)
(Save x "sol.gf" 8)

;; NOTE: Skip GLVis visualization (step 15)
;; NOTE: Skip memory cleanup (step 16) — Guile GC handles this
