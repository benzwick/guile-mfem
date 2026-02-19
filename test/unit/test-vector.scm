;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

(use-modules (srfi srfi-4)
             (srfi srfi-64)
             (oop goops)
             (mfem vector) (mfem operators))

(test-begin "mfem-vector")

;; Construction and Size
(test-group "construction"
  (let ((v (make <Vector> 5)))
    (test-equal "Vector(int) size" 5 (Size v)))

  (let ((v (make <Vector>)))
    (test-equal "Vector() size" 0 (Size v))))

;; Element access via get/set
(test-group "element-access"
  (let ((v (make <Vector> 3)))
    (set v 0 1.0)
    (set v 1 2.0)
    (set v 2 3.0)
    (test-approximate "get element 0" 1.0 (get v 0) 1e-15)
    (test-approximate "get element 1" 2.0 (get v 1) 1e-15)
    (test-approximate "get element 2" 3.0 (get v 2) 1e-15)))

;; Assign scalar
(test-group "assign"
  (let ((v (make <Vector> 4)))
    (Assign v 3.14)
    (test-approximate "assign scalar" 3.14 (get v 0) 1e-15)
    (test-approximate "assign scalar" 3.14 (get v 3) 1e-15)))

;; add_vector / subtract_vector
(test-group "add-subtract"
  (let ((a (make <Vector> 2))
        (b (make <Vector> 2))
        (c (make <Vector> 2)))
    (Assign a 1.0)
    (Assign b 2.0)
    (add-vector a b c)
    (test-approximate "add_vector" 3.0 (get c 0) 1e-15)
    (subtract-vector a b c)
    (test-approximate "subtract_vector" -1.0 (get c 0) 1e-15)))

;; GetDataArray (returns f64vector)
(test-group "f64vector-interop"
  (let ((v (make <Vector> 3)))
    (set v 0 10.0)
    (set v 1 20.0)
    (set v 2 30.0)
    (let ((fv (GetDataArray v)))
      (test-assert "GetDataArray returns f64vector"
        (f64vector? fv))
      (test-equal "f64vector length" 3 (f64vector-length fv))
      (test-approximate "f64vector element 0" 10.0 (f64vector-ref fv 0) 1e-15)
      (test-approximate "f64vector element 2" 30.0 (f64vector-ref fv 2) 1e-15))))

;; Sub-view constructor
(test-group "sub-view"
  (let ((v (make <Vector> 5)))
    (set v 0 0.0)
    (set v 1 1.0)
    (set v 2 2.0)
    (set v 3 3.0)
    (set v 4 4.0)
    (let ((sub (make <Vector> v 1 3)))
      (test-equal "sub-view size" 3 (Size sub))
      (test-approximate "sub-view element 0" 1.0 (get sub 0) 1e-15)
      (test-approximate "sub-view element 2" 3.0 (get sub 2) 1e-15))))

(define runner (test-runner-current))
(test-end "mfem-vector")
(exit (zero? (test-runner-fail-count runner)))
