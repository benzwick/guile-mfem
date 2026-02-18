(use-modules (srfi srfi-4)
             (srfi srfi-64)
             (oop goops)
             (mfem vector) (mfem operators) (mfem sparsemat))

(test-begin "mfem-sparsemat")

;; Construction and Size
(test-group "construction"
  (let ((s (make <SparseMatrix> 3 3)))
    (test-equal "SparseMatrix(3,3) height" 3 (Height s))
    (test-equal "SparseMatrix(3,3) width" 3 (Width s))))

;; Add elements and Finalize
(test-group "add-finalize"
  (let ((s (make <SparseMatrix> 3 3)))
    ;; Build a simple 3x3 diagonal matrix: diag(1, 2, 3)
    (Add s 0 0 1.0)
    (Add s 1 1 2.0)
    (Add s 2 2 3.0)
    (Finalize s)
    ;; Mult: y = S*x where x = [1,1,1]
    (let ((x (make <Vector> 3))
          (y (make <Vector> 3)))
      (Assign x 1.0)
      (Mult s x y)
      (test-approximate "diag mult y[0]" 1.0 (get y 0) 1e-15)
      (test-approximate "diag mult y[1]" 2.0 (get y 1) 1e-15)
      (test-approximate "diag mult y[2]" 3.0 (get y 2) 1e-15))))

;; CSR access: GetIArray, GetJArray, GetDataArray
(test-group "csr-access"
  (let ((s (make <SparseMatrix> 3 3)))
    ;; Build: row 0 -> (0,0)=1.0, (0,2)=2.0
    ;;        row 1 -> (1,1)=3.0
    ;;        row 2 -> (2,0)=4.0, (2,2)=5.0
    (Add s 0 0 1.0)
    (Add s 0 2 2.0)
    (Add s 1 1 3.0)
    (Add s 2 0 4.0)
    (Add s 2 2 5.0)
    (Finalize s)

    (let ((iv (GetIArray s))
          (jv (GetJArray s))
          (dv (GetDataArray s)))
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
      (test-approximate "data[4]" 4.0 (f64vector-ref dv 4) 1e-15))))

(define runner (test-runner-current))
(test-end "mfem-sparsemat")
(exit (zero? (test-runner-fail-count runner)))
