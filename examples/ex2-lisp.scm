;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;                                MFEM Example 2
;;
;; Sample runs:  guile -L build ex2-lisp.scm
;;               guile -L build ex2-lisp.scm -m ../data/beam-tri.mesh
;;               guile -L build ex2-lisp.scm -m ../data/beam-quad.mesh
;;               guile -L build ex2-lisp.scm -m ../data/beam-tet.mesh
;;               guile -L build ex2-lisp.scm -m ../data/beam-hex.mesh
;;               guile -L build ex2-lisp.scm -m ../data/beam-quad.mesh -o 3 -s
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

;; NOTE: SuiteSparse and GLVis visualization are not yet supported.
;;
;; BUG: NURBS degree elevation is skipped because SWIG Guile wraps null
;;      NURBSext as a truthy object. Error out on NURBS meshes until the
;;      null-pointer bug is fixed (see BUGS.md "Null C++ pointers are
;;      truthy in Scheme").

(define options (getopt-long (command-line)
                 '((mesh        (single-char #\m) (value #t))
                   (order       (single-char #\o) (value #t))
                   (static-cond (single-char #\s) (value #f)))))
(define mesh-file (option-ref options 'mesh "../data/beam-tri.mesh"))
(define order (string->number (option-ref options 'order "1")))
(define static-cond (option-ref options 'static-cond #f))

(when (string-contains mesh-file "nurbs")
  (error "NURBS meshes are not supported: NURBSext null-pointer check is \
unreliable (see BUGS.md \"Null C++ pointers are truthy in Scheme\")"))

(let* ((mesh (make <Mesh> mesh-file 1 1))
       (dim (Dimension mesh)))
  (when (or (< (Max (slot-ref mesh 'attributes)) 2)
            (< (Max (slot-ref mesh 'bdr-attributes)) 2))
    (format (current-error-port)
      "\nInput mesh should have at least two materials and ~
       two boundary attributes! (See schematic in ex2.cpp)\n\n")
    (exit 3))
  (let ((ref-levels (inexact->exact
                      (floor (/ (log (/ 5000.0 (GetNE mesh)))
                                (* (log 2.0) dim))))))
    (do ((l 0 (+ l 1))) ((>= l ref-levels))
      (UniformRefinement mesh)))
  (let* ((fec (make <H1-FECollection> order dim))
         (fespace (make <FiniteElementSpace> mesh fec dim)))
    (format #t "Number of finite element unknowns: ~a~%" (GetTrueVSize fespace))
    (let ((ess-tdof-list (make <intArray>))
          (ess-bdr (make <intArray> (Max (slot-ref mesh 'bdr-attributes)))))
      (Assign ess-bdr 0)
      (set ess-bdr 0 1)
      (GetEssentialTrueDofs fespace ess-bdr ess-tdof-list)
      (let ((f (make <VectorArrayCoefficient> dim))
            (x (make <GridFunction> fespace))
            (b (make <LinearForm> fespace))
            (a (make <BilinearForm> fespace)))
        (do ((i 0 (+ i 1))) ((>= i (- dim 1)))
          (Set f i (make <ConstantCoefficient> 0.0)))
        (let ((pull-force (make <Vector> (Max (slot-ref mesh 'bdr-attributes)))))
          (Assign pull-force 0.0)
          (set pull-force 1 -1.0e-2)
          (Set f (- dim 1) (make <PWConstCoefficient> pull-force)))
        (AddBoundaryIntegrator b (make <VectorBoundaryLFIntegrator> f))
        (Assemble b)
        (Assign x 0.0)
        (let ((lambda-vec (make <Vector> (Max (slot-ref mesh 'attributes))))
              (mu-vec (make <Vector> (Max (slot-ref mesh 'attributes)))))
          (Assign lambda-vec 1.0)
          (set lambda-vec 0 (* (get lambda-vec 1) 50))
          (Assign mu-vec 1.0)
          (set mu-vec 0 (* (get mu-vec 1) 50))
          (AddDomainIntegrator a
            (make <ElasticityIntegrator>
              (make <PWConstCoefficient> lambda-vec)
              (make <PWConstCoefficient> mu-vec))))
        (when static-cond (EnableStaticCondensation a))
        (Assemble a)
        (let ((A (make <OperatorHandle>))
              (B (make <Vector>))
              (X (make <Vector>)))
          (FormLinearSystem a ess-tdof-list x b A X B)
          (format #t "Size of linear system: ~a~%" (Height (Ptr A)))
          (PCG (Ptr A) (make <GSSmoother> (OperatorHandle2SparseMatrix A))
            B X 1 500 1e-8 0.0)
          (RecoverFEMSolution a X b x))
        ;; Primitive call: workaround for cross-module GOOPS dispatch issue.
        ((@@ (mfem mesh) primitive:Mesh-SetNodalFESpace) mesh fespace)
        (Add (GetNodes mesh) 1.0 x)
        (Neg x)
        (Print mesh "displaced.mesh" 8)
        (Save x "sol.gf" 8)))))
