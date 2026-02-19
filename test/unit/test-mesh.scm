;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem mesh) (mfem fe_coll))

;; MFEM data directory (set by CTest via MFEM_DATA_DIR environment variable)
(define mfem-data-dir (getenv "MFEM_DATA_DIR"))

(test-begin "mfem-mesh")

;; 2D quad mesh
(test-group "2d-quad"
  (let ((m (make <Mesh> 4 4 "QUADRILATERAL")))
    (test-equal "quad Dimension" 2 (Dimension m))
    (test-equal "quad GetNE" 16 (GetNE m))
    (test-assert "quad GetNV > 0" (> (GetNV m) 0))))

;; 2D triangle mesh
(test-group "2d-tri"
  (let ((m (make <Mesh> 2 2 "TRIANGLE")))
    (test-equal "tri Dimension" 2 (Dimension m))
    (test-assert "tri GetNE > 0" (> (GetNE m) 0))))

;; 3D tet mesh
(test-group "3d-tet"
  (let ((m (make <Mesh> 2 2 2 "TETRAHEDRON")))
    (test-equal "tet Dimension" 3 (Dimension m))
    (test-assert "tet GetNE > 0" (> (GetNE m) 0))
    (test-assert "tet GetNV > 0" (> (GetNV m) 0))))

;; 3D hex mesh
(test-group "3d-hex"
  (let ((m (make <Mesh> 2 2 2 "HEXAHEDRON")))
    (test-equal "hex Dimension" 3 (Dimension m))
    (test-equal "hex GetNE" 8 (GetNE m))))

;; Mesh from file
(test-group "from-file"
  (let ((m (make <Mesh> (string-append mfem-data-dir "/star.mesh") 1 1)))
    (test-equal "star Dimension" 2 (Dimension m))
    (test-assert "star GetNE > 0" (> (GetNE m) 0))))

;; H1 FE collection
(test-group "h1-fec"
  (let ((fec (make <H1-FECollection> 1 2)))
    (test-assert "H1 Name is string"
      (string? (Name fec)))))

;; L2 FE collection
(test-group "l2-fec"
  (let ((fec (make <L2-FECollection> 0 2)))
    (test-assert "L2 Name is string"
      (string? (Name fec)))))

(define runner (test-runner-current))
(test-end "mfem-mesh")
(exit (zero? (test-runner-fail-count runner)))
