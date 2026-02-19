;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Geometry types (from PyMFEM test_geom.py).

;; Guard: skip if required module is not compiled
(unless (false-if-exception (resolve-interface '(mfem geom)))
  (format (current-error-port) "SKIP: module (mfem geom) not available~%")
  (exit 77))

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
