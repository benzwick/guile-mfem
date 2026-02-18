;; MFEM Example 0 — Poisson: -Delta u = 1, u|dOmega = 0
;;
;; Port of mfem/examples/ex0.cpp
;;
;; Sample runs:
;;   guile -L build ex0.scm
;;   guile -L build ex0.scm -- -m ../data/fichera.mesh

;; Load C extensions (dependencies must be loaded first for SWIG type table)
(load-extension "mem_manager" "scm_init_mem_manager_module")
(load-extension "globals" "scm_init_globals_module")
(load-extension "array" "scm_init_array_module")
(load-extension "vector" "scm_init_vector_module")
(load-extension "operators" "scm_init_operators_module")
(load-extension "matrix" "scm_init_matrix_module")
(load-extension "densemat" "scm_init_densemat_module")
(load-extension "sparsemat" "scm_init_sparsemat_module")
(load-extension "intrules" "scm_init_intrules_module")
(load-extension "eltrans" "scm_init_eltrans_module")
(load-extension "fe" "scm_init_fe_module")
(load-extension "symmat" "scm_init_symmat_module")
(load-extension "integrator" "scm_init_integrator_module")
(load-extension "nonlininteg" "scm_init_nonlininteg_module")
(load-extension "coefficient" "scm_init_coefficient_module")
(load-extension "mesh" "scm_init_mesh_module")
(load-extension "fe_coll" "scm_init_fe_coll_module")
(load-extension "fespace" "scm_init_fespace_module")
(load-extension "gridfunc" "scm_init_gridfunc_module")
(load-extension "lininteg" "scm_init_lininteg_module")
(load-extension "bilininteg" "scm_init_bilininteg_module")
(load-extension "linearform" "scm_init_linearform_module")
(load-extension "handle" "scm_init_handle_module")
(load-extension "bilinearform" "scm_init_bilinearform_module")
(load-extension "solvers" "scm_init_solvers_module")
(load-extension "sparsesmoothers" "scm_init_sparsesmoothers_module")

(define-module (ex0)
  #:duplicates (merge-generics replace warn-override-core warn last))

(use-modules (oop goops)
             (array) (vector) (operators)
             (mesh) (fe_coll)
             (fespace) (gridfunc)
             (coefficient) (linearform)
             (lininteg) (bilinearform)
             (bilininteg) (handle)
             (sparsemat) (solvers) (sparsesmoothers))
(use-modules (ice-9 getopt-long))

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
