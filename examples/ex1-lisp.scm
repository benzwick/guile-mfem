;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;                                MFEM Example 1
;;
;; Sample runs:  guile -L build ex1-lisp.scm
;;               guile -L build ex1-lisp.scm -- -m ../data/star.mesh
;;               guile -L build ex1-lisp.scm -- -m ../data/square-disc.mesh
;;               guile -L build ex1-lisp.scm -- -m ../data/fichera.mesh
;;               guile -L build ex1-lisp.scm -- -m ../data/square-disc-nurbs.mesh -o -1
;;               guile -L build ex1-lisp.scm -- -m ../data/mobius-strip.mesh -o -1 -s
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

;; NOTE: Partial assembly (-pa), full assembly (-fa), SuiteSparse,
;;       algebraic CEED, and GLVis visualization are not yet supported.

(define options (getopt-long (command-line)
                 '((mesh        (single-char #\m) (value #t))
                   (order       (single-char #\o) (value #t))
                   (static-cond (single-char #\s) (value #f))
                   (device      (single-char #\d) (value #t)))))
(define mesh-file (option-ref options 'mesh "../data/star.mesh"))
(define order (string->number (option-ref options 'order "1")))
(define static-cond (option-ref options 'static-cond #f))

(let ((dev (make <Device> (option-ref options 'device "cpu"))))
  (Print dev))

(let* ((mesh (make <Mesh> mesh-file 1 1))
       (dim (Dimension mesh))
       (ref-levels (inexact->exact
                     (floor (/ (log (/ 50000.0 (GetNE mesh)))
                               (* (log 2.0) dim))))))
  (do ((l 0 (+ l 1))) ((>= l ref-levels))
    (UniformRefinement mesh))
  (let* ((fec (if (> order 0)
                  (make <H1-FECollection> order dim)
                  (let ((nodes (GetNodes mesh)))
                    (if nodes
                        (OwnFEC nodes)
                        (make <H1-FECollection> 1 dim)))))
         (fespace (make <FiniteElementSpace> mesh fec)))
    (format #t "Number of finite element unknowns: ~a~%" (GetTrueVSize fespace))
    (let ((ess-tdof-list (make <intArray>)))
      (when (> (Size (slot-ref mesh 'bdr-attributes)) 0)
        (let ((ess-bdr (make <intArray> (Max (slot-ref mesh 'bdr-attributes)))))
          (Assign ess-bdr 0)
          (MarkExternalBoundaries mesh ess-bdr)
          (GetEssentialTrueDofs fespace ess-bdr ess-tdof-list)))
      (let ((one (make <ConstantCoefficient> 1.0))
            (x (make <GridFunction> fespace))
            (b (make <LinearForm> fespace))
            (a (make <BilinearForm> fespace)))
        (Assign x 0.0)
        (AddDomainIntegrator b (make <DomainLFIntegrator> one))
        (Assemble b)
        (AddDomainIntegrator a (make <DiffusionIntegrator> one))
        (when static-cond (EnableStaticCondensation a))
        (Assemble a)
        (let ((A (make <OperatorHandle>))
              (B (make <Vector>))
              (X (make <Vector>)))
          (FormLinearSystem a ess-tdof-list x b A X B)
          (format #t "Size of linear system: ~a~%" (Height (Ptr A)))
          (PCG (Ptr A) (make <GSSmoother> (OperatorHandle2SparseMatrix A))
            B X 1 200 1e-12 0.0)
          (RecoverFEMSolution a X b x))
        (Print mesh "refined.mesh" 8)
        (Save x "sol.gf" 8)))))
