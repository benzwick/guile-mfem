;;                                MFEM Example 0 (Lisp style)
;;
;; Sample runs:  guile -L build ex0-lisp.scm
;;
;; Description: Solve -Δu = 1, u|∂Ω = 0 on the star mesh.

(use-modules (oop goops) (mfem))

(let ((mesh (make <Mesh> (or (getenv "MFEM_MESH") "star.mesh") 1 1)))
  (UniformRefinement mesh)
  (let ((fespace (make <FiniteElementSpace> mesh
                   (make <H1-FECollection> 1 (Dimension mesh)))))
    (let ((x (make <GridFunction> fespace))
          (b (make <LinearForm> fespace))
          (a (make <BilinearForm> fespace)))
      (Assign x 0.0)
      (AddDomainIntegrator b
        (make <DomainLFIntegrator> (make <ConstantCoefficient> 1.0)))
      (Assemble b)
      (AddDomainIntegrator a (make <DiffusionIntegrator>))
      (Assemble a)
      (let ((bdr (make <intArray>))
            (A (make <OperatorHandle>))
            (B (make <Vector>))
            (X (make <Vector>)))
        (GetBoundaryTrueDofs fespace bdr)
        (FormLinearSystem a bdr x b A X B)
        (PCG (Ptr A) (make <GSSmoother> (OperatorHandle2SparseMatrix A))
          B X 1 200 1e-12 0.0)
        (RecoverFEMSolution a X b x))
      (Save x "sol.gf")
      (Save mesh "mesh.mesh"))))
