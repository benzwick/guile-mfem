(use-modules (srfi srfi-64))

;; Source directory (two levels up from this test file)
(define source-dir
  (dirname (dirname (dirname (current-filename)))))

;; Load C extensions (dependencies must be loaded first for SWIG type table)
(load-extension "mem_manager" "scm_init_mem_manager_module")
(load-extension "globals" "scm_init_globals_module")
(load-extension "array" "scm_init_array_module")
(load-extension "vector" "scm_init_vector_module")
(load-extension "operators" "scm_init_operators_module")
(load-extension "matrix" "scm_init_matrix_module")
(load-extension "densemat" "scm_init_densemat_module")
(load-extension "sparsemat" "scm_init_sparsemat_module")
(load-extension "mesh" "scm_init_mesh_module")
(use-modules (mesh))
(load-extension "fe_coll" "scm_init_fe_coll_module")
(use-modules (fe_coll))

(test-begin "mfem-mesh")

;; 2D quad mesh
(test-group "2d-quad"
  (let ((m (new-Mesh 4 4 "QUADRILATERAL")))
    (test-equal "quad Dimension" 2 (Mesh-Dimension m))
    (test-equal "quad GetNE" 16 (Mesh-GetNE m))
    (test-assert "quad GetNV > 0" (> (Mesh-GetNV m) 0))
    (delete-Mesh m)))

;; 2D triangle mesh
(test-group "2d-tri"
  (let ((m (new-Mesh 2 2 "TRIANGLE")))
    (test-equal "tri Dimension" 2 (Mesh-Dimension m))
    (test-assert "tri GetNE > 0" (> (Mesh-GetNE m) 0))
    (delete-Mesh m)))

;; 3D tet mesh
(test-group "3d-tet"
  (let ((m (new-Mesh 2 2 2 "TETRAHEDRON")))
    (test-equal "tet Dimension" 3 (Mesh-Dimension m))
    (test-assert "tet GetNE > 0" (> (Mesh-GetNE m) 0))
    (test-assert "tet GetNV > 0" (> (Mesh-GetNV m) 0))
    (delete-Mesh m)))

;; 3D hex mesh
(test-group "3d-hex"
  (let ((m (new-Mesh 2 2 2 "HEXAHEDRON")))
    (test-equal "hex Dimension" 3 (Mesh-Dimension m))
    (test-equal "hex GetNE" 8 (Mesh-GetNE m))
    (delete-Mesh m)))

;; Mesh from file
(test-group "from-file"
  (let ((m (new-Mesh (string-append source-dir "/_reference/mfem/data/star.mesh") 1 1)))
    (test-equal "star Dimension" 2 (Mesh-Dimension m))
    (test-assert "star GetNE > 0" (> (Mesh-GetNE m) 0))
    (delete-Mesh m)))

;; H1 FE collection
(test-group "h1-fec"
  (let ((fec (new-H1-FECollection 1 2)))
    (test-assert "H1 Name is string"
      (string? (FiniteElementCollection-Name fec)))
    (delete-H1-FECollection fec)))

;; L2 FE collection
(test-group "l2-fec"
  (let ((fec (new-L2-FECollection 0 2)))
    (test-assert "L2 Name is string"
      (string? (FiniteElementCollection-Name fec)))
    (delete-L2-FECollection fec)))

(define runner (test-runner-current))
(test-end "mfem-mesh")
(exit (zero? (test-runner-fail-count runner)))
