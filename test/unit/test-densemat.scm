(use-modules (srfi srfi-4)
             (srfi srfi-64))

;; Load C extensions (dependencies must be loaded first for SWIG type table)
(load-extension "mem_manager" "scm_init_mem_manager_module")
(load-extension "array" "scm_init_array_module")
(load-extension "vector" "scm_init_vector_module")
(use-modules (vector-primitive))
(load-extension "operators" "scm_init_operators_module")
(load-extension "matrix" "scm_init_matrix_module")
(load-extension "densemat" "scm_init_densemat_module")
(use-modules (densemat))

(test-begin "mfem-densemat")

;; Construction and Size
(test-group "construction"
  (let ((m (new-DenseMatrix)))
    (test-equal "DenseMatrix() height" 0 (DenseMatrix-Height m))
    (test-equal "DenseMatrix() width" 0 (DenseMatrix-Width m))
    (delete-DenseMatrix m))

  (let ((m (new-DenseMatrix 3)))
    (test-equal "DenseMatrix(3) height" 3 (DenseMatrix-Height m))
    (test-equal "DenseMatrix(3) width" 3 (DenseMatrix-Width m))
    (delete-DenseMatrix m))

  (let ((m (new-DenseMatrix 2 3)))
    (test-equal "DenseMatrix(2,3) height" 2 (DenseMatrix-Height m))
    (test-equal "DenseMatrix(2,3) width" 3 (DenseMatrix-Width m))
    (delete-DenseMatrix m)))

;; Element access via get/set
(test-group "element-access"
  (let ((m (new-DenseMatrix 2 3)))
    (DenseMatrix-Assign m 0.0)
    (DenseMatrix-set m 0 0 1.0)
    (DenseMatrix-set m 0 1 2.0)
    (DenseMatrix-set m 0 2 3.0)
    (DenseMatrix-set m 1 0 4.0)
    (DenseMatrix-set m 1 1 5.0)
    (DenseMatrix-set m 1 2 6.0)
    (test-approximate "get(0,0)" 1.0 (DenseMatrix-get m 0 0) 1e-15)
    (test-approximate "get(0,2)" 3.0 (DenseMatrix-get m 0 2) 1e-15)
    (test-approximate "get(1,1)" 5.0 (DenseMatrix-get m 1 1) 1e-15)
    (test-approximate "get(1,2)" 6.0 (DenseMatrix-get m 1 2) 1e-15)
    (delete-DenseMatrix m)))

;; Assign scalar and matrix copy
(test-group "assign"
  (let ((m (new-DenseMatrix 2 2)))
    (DenseMatrix-Assign m 3.14)
    (test-approximate "assign scalar (0,0)" 3.14 (DenseMatrix-get m 0 0) 1e-15)
    (test-approximate "assign scalar (1,1)" 3.14 (DenseMatrix-get m 1 1) 1e-15)
    (delete-DenseMatrix m))

  (let ((a (new-DenseMatrix 2 2))
        (b (new-DenseMatrix 2 2)))
    (DenseMatrix-set a 0 0 1.0)
    (DenseMatrix-set a 0 1 2.0)
    (DenseMatrix-set a 1 0 3.0)
    (DenseMatrix-set a 1 1 4.0)
    (DenseMatrix-Assign b a)
    (test-approximate "assign matrix (0,0)" 1.0 (DenseMatrix-get b 0 0) 1e-15)
    (test-approximate "assign matrix (1,1)" 4.0 (DenseMatrix-get b 1 1) 1e-15)
    (delete-DenseMatrix a)
    (delete-DenseMatrix b)))

;; Mult: y = M*x
(test-group "mult"
  (let ((m (new-DenseMatrix 2 2))
        (x (new-Vector 2))
        (y (new-Vector 2)))
    ;; M = [[1, 2], [3, 4]]
    (DenseMatrix-set m 0 0 1.0)
    (DenseMatrix-set m 0 1 2.0)
    (DenseMatrix-set m 1 0 3.0)
    (DenseMatrix-set m 1 1 4.0)
    ;; x = [1, 1]
    (Vector-Assign x 1.0)
    (DenseMatrix-Mult m x y)
    ;; y should be [3, 7]
    (test-approximate "Mult y[0]" 3.0 (Vector-get y 0) 1e-15)
    (test-approximate "Mult y[1]" 7.0 (Vector-get y 1) 1e-15)
    (delete-DenseMatrix m)
    (delete-Vector x)
    (delete-Vector y)))

;; Det and Trace
(test-group "det-trace"
  (let ((m (new-DenseMatrix 2 2)))
    ;; M = [[1, 2], [3, 4]]
    (DenseMatrix-set m 0 0 1.0)
    (DenseMatrix-set m 0 1 2.0)
    (DenseMatrix-set m 1 0 3.0)
    (DenseMatrix-set m 1 1 4.0)
    (test-approximate "Det" -2.0 (DenseMatrix-Det m) 1e-15)
    (test-approximate "Trace" 5.0 (DenseMatrix-Trace m) 1e-15)
    (delete-DenseMatrix m)))

;; GetDataArray (returns f64vector, column-major layout)
(test-group "f64vector-interop"
  (let ((m (new-DenseMatrix 2 2)))
    (DenseMatrix-set m 0 0 1.0)
    (DenseMatrix-set m 0 1 2.0)
    (DenseMatrix-set m 1 0 3.0)
    (DenseMatrix-set m 1 1 4.0)
    (let ((fv (DenseMatrix-GetDataArray m)))
      (test-assert "GetDataArray returns f64vector"
        (f64vector? fv))
      (test-equal "f64vector length" 4 (f64vector-length fv)))
    (delete-DenseMatrix m)))

(define runner (test-runner-current))
(test-end "mfem-densemat")
(exit (zero? (test-runner-fail-count runner)))
