(use-modules (srfi srfi-64)
             (mfem mesh) (mfem fe_coll))

;; MFEM data directory (set by CTest via MFEM_DATA_DIR environment variable)
(define mfem-data-dir (getenv "MFEM_DATA_DIR"))

(test-begin "mfem-mesh")

;; 2D quad mesh
(test-group "2d-quad"
  (let ((m (new-Mesh 4 4 "QUADRILATERAL")))
    (test-equal "quad Dimension" 2 (Mesh-Dimension m))
    (test-equal "quad GetNE" 16 (Mesh-GetNE m))
    (test-assert "quad GetNV > 0" (> (Mesh-GetNV m) 0))))

;; 2D triangle mesh
(test-group "2d-tri"
  (let ((m (new-Mesh 2 2 "TRIANGLE")))
    (test-equal "tri Dimension" 2 (Mesh-Dimension m))
    (test-assert "tri GetNE > 0" (> (Mesh-GetNE m) 0))))

;; 3D tet mesh
(test-group "3d-tet"
  (let ((m (new-Mesh 2 2 2 "TETRAHEDRON")))
    (test-equal "tet Dimension" 3 (Mesh-Dimension m))
    (test-assert "tet GetNE > 0" (> (Mesh-GetNE m) 0))
    (test-assert "tet GetNV > 0" (> (Mesh-GetNV m) 0))))

;; 3D hex mesh
(test-group "3d-hex"
  (let ((m (new-Mesh 2 2 2 "HEXAHEDRON")))
    (test-equal "hex Dimension" 3 (Mesh-Dimension m))
    (test-equal "hex GetNE" 8 (Mesh-GetNE m))))

;; Mesh from file
(test-group "from-file"
  (let ((m (new-Mesh (string-append mfem-data-dir "/star.mesh") 1 1)))
    (test-equal "star Dimension" 2 (Mesh-Dimension m))
    (test-assert "star GetNE > 0" (> (Mesh-GetNE m) 0))))

;; H1 FE collection
(test-group "h1-fec"
  (let ((fec (new-H1-FECollection 1 2)))
    (test-assert "H1 Name is string"
      (string? (FiniteElementCollection-Name fec)))))

;; L2 FE collection
(test-group "l2-fec"
  (let ((fec (new-L2-FECollection 0 2)))
    (test-assert "L2 Name is string"
      (string? (FiniteElementCollection-Name fec)))))

(define runner (test-runner-current))
(test-end "mfem-mesh")
(exit (zero? (test-runner-fail-count runner)))
