(use-modules (srfi srfi-4)
             (srfi srfi-64)
             (oop goops)
             (mfem vector) (mfem operators) (mfem densemat))

(test-begin "mfem-densemat")

;; Construction and Size
(test-group "construction"
  (let ((m (make <DenseMatrix>)))
    (test-equal "DenseMatrix() height" 0 (Height m))
    (test-equal "DenseMatrix() width" 0 (Width m)))

  (let ((m (make <DenseMatrix> 3)))
    (test-equal "DenseMatrix(3) height" 3 (Height m))
    (test-equal "DenseMatrix(3) width" 3 (Width m)))

  (let ((m (make <DenseMatrix> 2 3)))
    (test-equal "DenseMatrix(2,3) height" 2 (Height m))
    (test-equal "DenseMatrix(2,3) width" 3 (Width m))))

;; Element access via get/set
(test-group "element-access"
  (let ((m (make <DenseMatrix> 2 3)))
    (Assign m 0.0)
    (set m 0 0 1.0)
    (set m 0 1 2.0)
    (set m 0 2 3.0)
    (set m 1 0 4.0)
    (set m 1 1 5.0)
    (set m 1 2 6.0)
    (test-approximate "get(0,0)" 1.0 (get m 0 0) 1e-15)
    (test-approximate "get(0,2)" 3.0 (get m 0 2) 1e-15)
    (test-approximate "get(1,1)" 5.0 (get m 1 1) 1e-15)
    (test-approximate "get(1,2)" 6.0 (get m 1 2) 1e-15)))

;; Assign scalar and matrix copy
(test-group "assign"
  (let ((m (make <DenseMatrix> 2 2)))
    (Assign m 3.14)
    (test-approximate "assign scalar (0,0)" 3.14 (get m 0 0) 1e-15)
    (test-approximate "assign scalar (1,1)" 3.14 (get m 1 1) 1e-15))

  (let ((a (make <DenseMatrix> 2 2))
        (b (make <DenseMatrix> 2 2)))
    (set a 0 0 1.0)
    (set a 0 1 2.0)
    (set a 1 0 3.0)
    (set a 1 1 4.0)
    (Assign b a)
    (test-approximate "assign matrix (0,0)" 1.0 (get b 0 0) 1e-15)
    (test-approximate "assign matrix (1,1)" 4.0 (get b 1 1) 1e-15)))

;; Mult: y = M*x
(test-group "mult"
  (let ((m (make <DenseMatrix> 2 2))
        (x (make <Vector> 2))
        (y (make <Vector> 2)))
    ;; M = [[1, 2], [3, 4]]
    (set m 0 0 1.0)
    (set m 0 1 2.0)
    (set m 1 0 3.0)
    (set m 1 1 4.0)
    ;; x = [1, 1]
    (Assign x 1.0)
    (Mult m x y)
    ;; y should be [3, 7]
    (test-approximate "Mult y[0]" 3.0 (get y 0) 1e-15)
    (test-approximate "Mult y[1]" 7.0 (get y 1) 1e-15)))

;; Det and Trace
(test-group "det-trace"
  (let ((m (make <DenseMatrix> 2 2)))
    ;; M = [[1, 2], [3, 4]]
    (set m 0 0 1.0)
    (set m 0 1 2.0)
    (set m 1 0 3.0)
    (set m 1 1 4.0)
    (test-approximate "Det" -2.0 (Det m) 1e-15)
    (test-approximate "Trace" 5.0 (Trace m) 1e-15)))

;; GetDataArray (returns f64vector, column-major layout)
(test-group "f64vector-interop"
  (let ((m (make <DenseMatrix> 2 2)))
    (set m 0 0 1.0)
    (set m 0 1 2.0)
    (set m 1 0 3.0)
    (set m 1 1 4.0)
    (let ((fv (GetDataArray m)))
      (test-assert "GetDataArray returns f64vector"
        (f64vector? fv))
      (test-equal "f64vector length" 4 (f64vector-length fv)))))

(define runner (test-runner-current))
(test-end "mfem-densemat")
(exit (zero? (test-runner-fail-count runner)))
