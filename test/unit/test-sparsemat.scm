(use-modules (srfi srfi-4)
             (srfi srfi-64))

;; Load C extensions (dependencies must be loaded first for SWIG type table)
(load-extension "mem_manager" "scm_init_mem_manager_module")
(load-extension "globals" "scm_init_globals_module")
(load-extension "array" "scm_init_array_module")
(load-extension "vector" "scm_init_vector_module")
(use-modules (vector-primitive))
(load-extension "operators" "scm_init_operators_module")
(load-extension "matrix" "scm_init_matrix_module")
(load-extension "densemat" "scm_init_densemat_module")
(load-extension "sparsemat" "scm_init_sparsemat_module")
(use-modules (sparsemat))

(test-begin "mfem-sparsemat")

;; Construction and Size
(test-group "construction"
  (let ((s (new-SparseMatrix 3 3)))
    (test-equal "SparseMatrix(3,3) height" 3 (SparseMatrix-Height s))
    (test-equal "SparseMatrix(3,3) width" 3 (SparseMatrix-Width s))
    (delete-SparseMatrix s)))

;; Add elements and Finalize
(test-group "add-finalize"
  (let ((s (new-SparseMatrix 3 3)))
    ;; Build a simple 3x3 diagonal matrix: diag(1, 2, 3)
    (SparseMatrix-Add s 0 0 1.0)
    (SparseMatrix-Add s 1 1 2.0)
    (SparseMatrix-Add s 2 2 3.0)
    (SparseMatrix-Finalize s)
    ;; Mult: y = S*x where x = [1,1,1]
    (let ((x (new-Vector 3))
          (y (new-Vector 3)))
      (Vector-Assign x 1.0)
      (SparseMatrix-Mult s x y)
      (test-approximate "diag mult y[0]" 1.0 (Vector-get y 0) 1e-15)
      (test-approximate "diag mult y[1]" 2.0 (Vector-get y 1) 1e-15)
      (test-approximate "diag mult y[2]" 3.0 (Vector-get y 2) 1e-15)
      (delete-Vector x)
      (delete-Vector y))
    (delete-SparseMatrix s)))

;; CSR access: GetIArray, GetJArray, GetDataArray
(test-group "csr-access"
  (let ((s (new-SparseMatrix 3 3)))
    ;; Build: row 0 -> (0,0)=1.0, (0,2)=2.0
    ;;        row 1 -> (1,1)=3.0
    ;;        row 2 -> (2,0)=4.0, (2,2)=5.0
    (SparseMatrix-Add s 0 0 1.0)
    (SparseMatrix-Add s 0 2 2.0)
    (SparseMatrix-Add s 1 1 3.0)
    (SparseMatrix-Add s 2 0 4.0)
    (SparseMatrix-Add s 2 2 5.0)
    (SparseMatrix-Finalize s)

    (let ((iv (SparseMatrix-GetIArray s))
          (jv (SparseMatrix-GetJArray s))
          (dv (SparseMatrix-GetDataArray s)))
      ;; I array: [0, 2, 3, 5]
      (test-assert "GetIArray returns s32vector" (s32vector? iv))
      (test-equal "I length" 4 (s32vector-length iv))
      (test-equal "I[0]" 0 (s32vector-ref iv 0))
      (test-equal "I[1]" 2 (s32vector-ref iv 1))
      (test-equal "I[2]" 3 (s32vector-ref iv 2))
      (test-equal "I[3]" 5 (s32vector-ref iv 3))

      ;; J array: [2, 0, 1, 2, 0] (MFEM stores in reverse insertion order)
      (test-assert "GetJArray returns s32vector" (s32vector? jv))
      (test-equal "J length" 5 (s32vector-length jv))
      (test-equal "J[0]" 2 (s32vector-ref jv 0))
      (test-equal "J[1]" 0 (s32vector-ref jv 1))
      (test-equal "J[2]" 1 (s32vector-ref jv 2))
      (test-equal "J[3]" 2 (s32vector-ref jv 3))
      (test-equal "J[4]" 0 (s32vector-ref jv 4))

      ;; Data array: [2.0, 1.0, 3.0, 5.0, 4.0] (matches J ordering)
      (test-assert "GetDataArray returns f64vector" (f64vector? dv))
      (test-equal "data length" 5 (f64vector-length dv))
      (test-approximate "data[0]" 2.0 (f64vector-ref dv 0) 1e-15)
      (test-approximate "data[1]" 1.0 (f64vector-ref dv 1) 1e-15)
      (test-approximate "data[2]" 3.0 (f64vector-ref dv 2) 1e-15)
      (test-approximate "data[3]" 5.0 (f64vector-ref dv 3) 1e-15)
      (test-approximate "data[4]" 4.0 (f64vector-ref dv 4) 1e-15))
    (delete-SparseMatrix s)))

(define runner (test-runner-current))
(test-end "mfem-sparsemat")
(exit (zero? (test-runner-fail-count runner)))
