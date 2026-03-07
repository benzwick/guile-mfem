;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;                                MFEM Example 3
;;
;; Sample runs:  guile -L build ex3-lisp.scm
;;               guile -L build ex3-lisp.scm -m ../data/fichera.mesh
;;               guile -L build ex3-lisp.scm -m ../data/beam-tet.mesh
;;               guile -L build ex3-lisp.scm -m ../data/escher.mesh
;;               guile -L build ex3-lisp.scm -m ../data/fichera-q2.mesh -o 2
;;               guile -L build ex3-lisp.scm -m ../data/fichera-q3.mesh -o 3
;;               guile -L build ex3-lisp.scm -m ../data/star-surf.mesh -o 1
;;               guile -L build ex3-lisp.scm -m ../data/beam-tet.mesh -s
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
;; NOTE: Exact solution and L^2 error require VectorFunctionCoefficient with
;;       SWIG director support (not yet available). Uses constant RHS instead.

(use-modules (oop goops) (mfem) (ice-9 getopt-long))

;; NOTE: Partial assembly (-pa), SuiteSparse, and GLVis are not yet supported.
;;
;; BUG: Exact error computation requires VectorFunctionCoefficient callbacks
;;      (SWIG director support). See BUGS.md.

(define options (getopt-long (command-line)
                 '((mesh        (single-char #\m) (value #t))
                   (order       (single-char #\o) (value #t))
                   (static-cond (single-char #\s) (value #f))
                   (device      (single-char #\d) (value #t)))))
(define mesh-file (option-ref options 'mesh "../data/beam-tet.mesh"))
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
  (let* ((fec (make <ND-FECollection> order dim))
         (fespace (make <FiniteElementSpace> mesh fec)))
    (format #t "Number of finite element unknowns: ~a~%" (GetTrueVSize fespace))
    (let ((ess-tdof-list (make <intArray>)))
      (when (> (Size (slot-ref mesh 'bdr-attributes)) 0)
        (let ((ess-bdr (make <intArray> (Max (slot-ref mesh 'bdr-attributes)))))
          (Assign ess-bdr 1)
          (GetEssentialTrueDofs fespace ess-bdr ess-tdof-list)))
      (let ((f-vec (make <Vector> dim))
            (x (make <GridFunction> fespace))
            (b (make <LinearForm> fespace))
            (a (make <BilinearForm> fespace)))
        (Assign f-vec 1.0)
        (AddDomainIntegrator b
          (make <VectorFEDomainLFIntegrator>
            (make <VectorConstantCoefficient> f-vec)))
        (Assemble b)
        (Assign x 0.0)
        (AddDomainIntegrator a
          (make <CurlCurlIntegrator> (make <ConstantCoefficient> 1.0)))
        (AddDomainIntegrator a
          (make <VectorFEMassIntegrator> (make <ConstantCoefficient> 1.0)))
        (when static-cond (EnableStaticCondensation a))
        (Assemble a)
        (let ((A (make <OperatorHandle>))
              (B (make <Vector>))
              (X (make <Vector>)))
          (FormLinearSystem a ess-tdof-list x b A X B)
          (format #t "Size of linear system: ~a~%" (Height (Ptr A)))
          (PCG (Ptr A) (make <GSSmoother> (OperatorHandle2SparseMatrix A))
            B X 1 500 1e-12 0.0)
          (RecoverFEMSolution a X b x))
        (Print mesh "refined.mesh" 8)
        (Save x "sol.gf" 8)))))
