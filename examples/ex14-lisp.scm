;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;                                MFEM Example 14
;;
;; Sample runs:  guile -L build ex14-lisp.scm
;;               guile -L build ex14-lisp.scm -m ../data/star.mesh
;;               guile -L build ex14-lisp.scm -m ../data/escher.mesh
;;               guile -L build ex14-lisp.scm -m ../data/fichera.mesh
;;               guile -L build ex14-lisp.scm -m ../data/inline-quad.mesh
;;               guile -L build ex14-lisp.scm -m ../data/inline-hex.mesh
;;               guile -L build ex14-lisp.scm -m ../data/star.mesh -o 2
;;               guile -L build ex14-lisp.scm -m ../data/inline-quad.mesh -o 2
;;
;; Description:  This example code demonstrates the use of MFEM to define a
;;               discontinuous Galerkin (DG) finite element discretization of
;;               the Laplace problem -Delta u = 1 with homogeneous Dirichlet
;;               boundary conditions. Finite element spaces of any order,
;;               including zero on regular grids, are supported. The example
;;               highlights the use of discontinuous spaces and DG-specific face
;;               integrators.

(use-modules (oop goops) (mfem) (ice-9 getopt-long))

(define options (getopt-long (command-line)
                 '((mesh  (single-char #\m) (value #t))
                   (order (single-char #\o) (value #t)))))
(define mesh-file (option-ref options 'mesh "../data/star.mesh"))
(define order (string->number (option-ref options 'order "1")))
(define sigma -1.0)
(define kappa (expt (+ order 1.0) 2.0))

(let* ((mesh (make <Mesh> mesh-file 1 1))
       (dim (Dimension mesh))
       (ref-levels (inexact->exact
                     (floor (/ (log (/ 50000.0 (GetNE mesh)))
                               (* (log 2.0) dim))))))
  (do ((l 0 (+ l 1))) ((>= l ref-levels))
    (UniformRefinement mesh))
  (let* ((fec (make <DG-FECollection> order dim))
         (fespace (make <FiniteElementSpace> mesh fec))
         (one (make <ConstantCoefficient> 1.0))
         (zero (make <ConstantCoefficient> 0.0))
         (b (make <LinearForm> fespace))
         (x (make <GridFunction> fespace))
         (a (make <BilinearForm> fespace)))
    (format #t "Number of finite element unknowns: ~a~%" (GetVSize fespace))
    ;; RHS with DG boundary integrator for Dirichlet BC
    (AddDomainIntegrator b (make <DomainLFIntegrator> one))
    (AddBdrFaceIntegrator b
      (make <DGDirichletLFIntegrator> zero one sigma kappa))
    (Assemble b)
    (Assign x 0.0)
    ;; Bilinear form with interior and boundary DG face integrators
    (AddDomainIntegrator a (make <DiffusionIntegrator> one))
    (AddInteriorFaceIntegrator a
      (make <DGDiffusionIntegrator> one sigma kappa))
    (AddBdrFaceIntegrator a
      (make <DGDiffusionIntegrator> one sigma kappa))
    (Assemble a)
    (Finalize a)
    (let ((A (SpMat a))
          (M (make <GSSmoother> (SpMat a))))
      (if (= sigma -1.0)
          (PCG A M b x 1 500 1e-12 0.0)
          (GMRES A M b x 1 500 10 1e-12 0.0)))
    (Print mesh "refined.mesh" 8)
    (Save x "sol.gf" 8)))
