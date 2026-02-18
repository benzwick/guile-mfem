;; MFEM Example 0 — Poisson: -Delta u = 1, u|dOmega = 0
(use-modules (oop goops) (mfem) (ice-9 getopt-long))

;; 1. Parse command line
(define option-spec
  '((mesh  (single-char #\m) (value #t))
    (order (single-char #\o) (value #t))))
(define options (getopt-long (command-line) option-spec))
(define mesh-file (option-ref options 'mesh "../data/star.mesh"))
(define order (string->number (option-ref options 'order "1")))

;; 2. Mesh
(define mesh (make <Mesh> mesh-file 1 1))
(UniformRefinement mesh)

;; 3. Finite element space
(define fec (make <H1-FECollection> order (Dimension mesh)))
(define fespace (make <FiniteElementSpace> mesh fec))
(format #t "Number of unknowns: ~a~%" (GetTrueVSize fespace))

;; 4. Essential boundary DOFs
(define ess-dofs (make <intArray>))
(GetBoundaryTrueDofs fespace ess-dofs)

;; 5. Solution vector
(define x (make <GridFunction> fespace))
(Assign x 0.0)

;; 6. Right-hand side: b(v) = (1, v)
(define b (make <LinearForm> fespace))
(AddDomainIntegrator b (make <DomainLFIntegrator> (make <ConstantCoefficient> 1.0)))
(Assemble b)

;; 7. Bilinear form: a(u,v) = (grad u, grad v)
(define a (make <BilinearForm> fespace))
(AddDomainIntegrator a (make <DiffusionIntegrator>))
(Assemble a)

;; 8. Form and solve the linear system
(define A (make <OperatorHandle>))
(define B (make <Vector>))
(define X (make <Vector>))
(FormLinearSystem a ess-dofs x b A X B)

(define M (make <GSSmoother> (OperatorHandle2SparseMatrix A)))
(PCG (Ptr A) M B X 1 200 1e-12 0.0)

;; 9. Recover solution and save
(RecoverFEMSolution a X b x)
(Save x "sol.gf")
(Save mesh "mesh.mesh")
