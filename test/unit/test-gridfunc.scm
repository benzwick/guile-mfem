;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM GridFunction (from PyMFEM test_gridfunc.py).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem vector) (mfem operators)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem gridfunc))
(use-modules (mesh-primitive) (fe_coll-primitive) (fespace-primitive)
             (coefficient-primitive) (gridfunc-primitive))

;; MFEM data directory (set by CTest via MFEM_DATA_DIR environment variable)
(define mfem-data-dir (getenv "MFEM_DATA_DIR"))

(test-begin "mfem-gridfunc")

;; Construction and size
(test-group "construction"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (gf   (new-GridFunction fes)))
    (test-assert "GridFunction size > 0" (> (Size gf) 0))
    (test-equal "GridFunction size = fespace vsize"
      (FiniteElementSpace-GetVSize fes)
      (Size gf))))

;; ProjectCoefficient with ConstantCoefficient
(test-group "project-coefficient"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (gf   (new-GridFunction fes))
         (c    (new-ConstantCoefficient 1.0)))
    (GridFunction-ProjectCoefficient gf c)
    ;; After projecting constant 1.0, all DOF values should be 1.0
    (test-approximate "projected value at DOF 0" 1.0 (get gf 0) 1e-15)))

;; Save to file
(test-group "save-to-file"
  (let* ((mesh (new-Mesh (string-append mfem-data-dir "/star.mesh") 1 1))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (gf   (new-GridFunction fes))
         (c    (new-ConstantCoefficient 1.0))
         (outfile "out_test_gridfunc.gf"))
    (GridFunction-ProjectCoefficient gf c)
    (GridFunction-Save gf outfile)
    (test-assert "output file exists"
      (file-exists? outfile))
    ;; Clean up
    (when (file-exists? outfile) (delete-file outfile))))

(define runner (test-runner-current))
(test-end "mfem-gridfunc")
(exit (zero? (test-runner-fail-count runner)))
