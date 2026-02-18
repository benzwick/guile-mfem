;; MFEM Example 0 — Poisson: -Delta u = 1, u|dOmega = 0
;;
;; Port of mfem/examples/ex0.cpp
;;
;; Sample runs:
;;   guile -L build ex0.scm
;;   guile -L build ex0.scm -- -m ../data/fichera.mesh

(use-modules (oop goops) (mfem) (ice-9 getopt-long))

;; 1. Parse command line options
(define option-spec
  '((mesh   (single-char #\m) (value #t))
    (order  (single-char #\o) (value #t))))
(define options (getopt-long (command-line) option-spec))
(define mesh-file (option-ref options 'mesh "../data/star.mesh"))
(define order (string->number (option-ref options 'order "1")))

(let* (;; 2. Read mesh, refine once
       (m (make <Mesh> mesh-file 1 1))
       (_ (UniformRefinement m))

       ;; 3. H1 finite element space
       (fec (make <H1-FECollection> order (Dimension m)))
       (fes (make <FiniteElementSpace> m fec)))

  (format #t "Number of unknowns: ~a~%" (GetTrueVSize fes))

  (let* (;; 4. Boundary DOFs
         (boundary-dofs (make <intArray>))
         (_ (GetBoundaryTrueDofs fes boundary-dofs))

         ;; 5. Solution = 0
         (x (make <GridFunction> fes))
         (_ (Assign x 0.0))

         ;; 6. RHS: b(phi) = integral(1 * phi)
         (one (make <ConstantCoefficient> 1.0))
         (b (make <LinearForm> fes))
         (_ (AddDomainIntegrator b (make <DomainLFIntegrator> one)))
         (_ (Assemble b))

         ;; 7. Bilinear form: a(u,phi) = integral(grad u . grad phi)
         (a (make <BilinearForm> fes))
         (_ (AddDomainIntegrator a (make <DiffusionIntegrator>)))
         (_ (Assemble a))

         ;; 8. Form linear system
         (A (make <OperatorHandle>))
         (B (make <Vector>))
         (X (make <Vector>))
         (_ (FormLinearSystem a boundary-dofs x b A X B))

         ;; 9. Solve with PCG + Gauss-Seidel
         (M (make <GSSmoother> (OperatorHandle2SparseMatrix A)))
         (_ (PCG (Ptr A) M B X 1 200 1e-12 0.0))

         ;; 10. Recover solution
         (_ (RecoverFEMSolution a X b x)))

    ;; 11. Save
    (Save x "sol.gf")
    (Save m "mesh.mesh")))
