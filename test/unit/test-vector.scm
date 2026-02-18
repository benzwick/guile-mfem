(use-modules (srfi srfi-4)
             (srfi srfi-64)
             (mfem vector))

(test-begin "mfem-vector")

;; Construction and Size
(test-group "construction"
  (let ((v (new-Vector 5)))
    (test-equal "Vector(int) size" 5 (Vector-Size v))
    (delete-Vector v))

  (let ((v (new-Vector)))
    (test-equal "Vector() size" 0 (Vector-Size v))
    (delete-Vector v)))

;; Element access via get/set
(test-group "element-access"
  (let ((v (new-Vector 3)))
    (Vector-set v 0 1.0)
    (Vector-set v 1 2.0)
    (Vector-set v 2 3.0)
    (test-approximate "get element 0" 1.0 (Vector-get v 0) 1e-15)
    (test-approximate "get element 1" 2.0 (Vector-get v 1) 1e-15)
    (test-approximate "get element 2" 3.0 (Vector-get v 2) 1e-15)
    (delete-Vector v)))

;; Assign scalar
(test-group "assign"
  (let ((v (new-Vector 4)))
    (Vector-Assign v 3.14)
    (test-approximate "assign scalar" 3.14 (Vector-get v 0) 1e-15)
    (test-approximate "assign scalar" 3.14 (Vector-get v 3) 1e-15)
    (delete-Vector v)))

;; add_vector / subtract_vector
(test-group "add-subtract"
  (let ((a (new-Vector 2))
        (b (new-Vector 2))
        (c (new-Vector 2)))
    (Vector-Assign a 1.0)
    (Vector-Assign b 2.0)
    (add-vector a b c)
    (test-approximate "add_vector" 3.0 (Vector-get c 0) 1e-15)
    (subtract-vector a b c)
    (test-approximate "subtract_vector" -1.0 (Vector-get c 0) 1e-15)
    (delete-Vector a)
    (delete-Vector b)
    (delete-Vector c)))

;; GetDataArray (returns f64vector)
(test-group "f64vector-interop"
  (let ((v (new-Vector 3)))
    (Vector-set v 0 10.0)
    (Vector-set v 1 20.0)
    (Vector-set v 2 30.0)
    (let ((fv (Vector-GetDataArray v)))
      (test-assert "GetDataArray returns f64vector"
        (f64vector? fv))
      (test-equal "f64vector length" 3 (f64vector-length fv))
      (test-approximate "f64vector element 0" 10.0 (f64vector-ref fv 0) 1e-15)
      (test-approximate "f64vector element 2" 30.0 (f64vector-ref fv 2) 1e-15))
    (delete-Vector v)))

;; Sub-view constructor
(test-group "sub-view"
  (let ((v (new-Vector 5)))
    (Vector-set v 0 0.0)
    (Vector-set v 1 1.0)
    (Vector-set v 2 2.0)
    (Vector-set v 3 3.0)
    (Vector-set v 4 4.0)
    (let ((sub (new-Vector v 1 3)))
      (test-equal "sub-view size" 3 (Vector-Size sub))
      (test-approximate "sub-view element 0" 1.0 (Vector-get sub 0) 1e-15)
      (test-approximate "sub-view element 2" 3.0 (Vector-get sub 2) 1e-15)
      (delete-Vector sub))
    (delete-Vector v)))

(define runner (test-runner-current))
(test-end "mfem-vector")
(exit (zero? (test-runner-fail-count runner)))
