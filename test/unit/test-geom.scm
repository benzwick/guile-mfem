;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Geometry types (from PyMFEM test_geom.py).

(use-modules (test unit-harness))
(skip-unless '(mfem geom))

(use-modules (srfi srfi-64)
             (mfem geom) (mfem mesh))
(use-modules (geom-primitive) (mesh-primitive))

(test-begin "mfem-geom")

;; GetElementGeometry on a triangle mesh
(test-group "element-geometry"
  (let ((mesh (new-Mesh 2 2 "TRIANGLE")))
    (test-assert "GetElementGeometry returns value"
      (integer? (Mesh-GetElementGeometry mesh 0)))))

(define runner (test-runner-current))
(test-end "mfem-geom")
(exit (zero? (test-runner-fail-count runner)))
