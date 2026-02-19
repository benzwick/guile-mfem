;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM FiniteElementSpace (from PyMFEM test_fespace.py).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem mesh) (mfem fe_coll) (mfem fespace))
(use-modules (mesh-primitive) (fe_coll-primitive) (fespace-primitive))

;; MFEM data directory (set by CTest via MFEM_DATA_DIR environment variable)
(define mfem-data-dir (getenv "MFEM_DATA_DIR"))

(test-begin "mfem-fespace")

;; Construction and basic queries
(test-group "construction"
  (let* ((mesh (new-Mesh (string-append mfem-data-dir "/beam-tri.mesh") 1 1))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec)))
    (test-assert "GetVSize > 0" (> (FiniteElementSpace-GetVSize fes) 0))
    (test-assert "GetTrueVSize > 0" (> (FiniteElementSpace-GetTrueVSize fes) 0))
    (test-assert "GetNE > 0" (> (FiniteElementSpace-GetNE fes) 0))))

;; 2D quad mesh with H1
(test-group "2d-quad-h1"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec)))
    (test-assert "quad H1 GetVSize > 0" (> (FiniteElementSpace-GetVSize fes) 0))
    (test-equal "quad H1 GetNE" 16 (FiniteElementSpace-GetNE fes))))

;; 3D hex mesh with H1
(test-group "3d-hex-h1"
  (let* ((mesh (new-Mesh 2 2 2 "HEXAHEDRON"))
         (fec  (new-H1-FECollection 1 3))
         (fes  (new-FiniteElementSpace mesh fec)))
    (test-assert "hex H1 GetVSize > 0" (> (FiniteElementSpace-GetVSize fes) 0))
    (test-equal "hex H1 GetNE" 8 (FiniteElementSpace-GetNE fes))))

(define runner (test-runner-current))
(test-end "mfem-fespace")
(exit (zero? (test-runner-fail-count runner)))
