;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM finite element types (from MFEM test_linear_fes.cpp).

(use-modules (srfi srfi-64)
             (mfem mesh) (mfem fe_coll) (mfem fespace))
(use-modules (mesh-primitive) (fe_coll-primitive) (fespace-primitive))

(test-begin "mfem-fe")

;; H1 on various mesh types
(test-group "h1-types"
  ;; 2D triangle
  (let* ((mesh (new-Mesh 2 2 "TRIANGLE"))
         (fec  (new-H1-FECollection 2 2))
         (fes  (new-FiniteElementSpace mesh fec)))
    (test-assert "H1 order 2 tri vsize > 0"
      (> (FiniteElementSpace-GetVSize fes) 0)))

  ;; 2D quad
  (let* ((mesh (new-Mesh 2 2 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 2 2))
         (fes  (new-FiniteElementSpace mesh fec)))
    (test-assert "H1 order 2 quad vsize > 0"
      (> (FiniteElementSpace-GetVSize fes) 0)))

  ;; 3D tet
  (let* ((mesh (new-Mesh 2 2 2 "TETRAHEDRON"))
         (fec  (new-H1-FECollection 1 3))
         (fes  (new-FiniteElementSpace mesh fec)))
    (test-assert "H1 order 1 tet vsize > 0"
      (> (FiniteElementSpace-GetVSize fes) 0)))

  ;; 3D hex
  (let* ((mesh (new-Mesh 2 2 2 "HEXAHEDRON"))
         (fec  (new-H1-FECollection 2 3))
         (fes  (new-FiniteElementSpace mesh fec)))
    (test-assert "H1 order 2 hex vsize > 0"
      (> (FiniteElementSpace-GetVSize fes) 0))))

;; L2 on various mesh types
(test-group "l2-types"
  ;; 2D triangle
  (let* ((mesh (new-Mesh 2 2 "TRIANGLE"))
         (fec  (new-L2-FECollection 0 2))
         (fes  (new-FiniteElementSpace mesh fec)))
    (test-equal "L2 order 0 tri: vsize = num elements"
      (Mesh-GetNE mesh)
      (FiniteElementSpace-GetVSize fes)))

  ;; 2D quad
  (let* ((mesh (new-Mesh 2 2 "QUADRILATERAL"))
         (fec  (new-L2-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec)))
    (test-assert "L2 order 1 quad vsize > 0"
      (> (FiniteElementSpace-GetVSize fes) 0))))

;; Higher order H1
(test-group "h1-higher-order"
  (let* ((mesh (new-Mesh 2 2 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 3 2))
         (fes  (new-FiniteElementSpace mesh fec)))
    (test-assert "H1 order 3 quad vsize > 0"
      (> (FiniteElementSpace-GetVSize fes) 0))))

(define runner (test-runner-current))
(test-end "mfem-fe")
(exit (zero? (test-runner-fail-count runner)))
