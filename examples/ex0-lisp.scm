;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;                                MFEM Example 0
;;
;; Sample runs:  guile -L build ex0-lisp.scm
;;               guile -L build ex0-lisp.scm -- -m ../data/fichera.mesh
;;               guile -L build ex0-lisp.scm -- -m ../data/square-disc.mesh -o 2
;;
;; Description: This example code demonstrates the most basic usage of MFEM to
;;              define a simple finite element discretization of the Poisson
;;              problem -Delta u = 1 with zero Dirichlet boundary conditions.
;;              General 2D/3D mesh files and finite element polynomial degrees
;;              can be specified by command line options.

(use-modules (oop goops) (mfem) (ice-9 getopt-long))

(define options (getopt-long (command-line)
                 '((mesh  (single-char #\m) (value #t))
                   (order (single-char #\o) (value #t)))))
(define mesh-file (option-ref options 'mesh "../data/star.mesh"))
(define order (string->number (option-ref options 'order "1")))

(let ((mesh (make <Mesh> mesh-file 1 1)))
  (UniformRefinement mesh)
  (let ((fespace (make <FiniteElementSpace> mesh
                   (make <H1-FECollection> order (Dimension mesh)))))
    (format #t "Number of unknowns: ~a~%" (GetTrueVSize fespace))
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
