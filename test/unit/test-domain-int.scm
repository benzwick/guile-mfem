;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM domain integration (from MFEM test_domain_int.cpp).
;; Tests scalar field integration using mass matrix and SparseMatrix::InnerProduct.
;;
;; BUG: Importing (mfem gridfunc) alongside (mfem bilinearform) causes a
;; segfault during module loading (see BUGS.md).  This test avoids that
;; combination by using raw Vectors instead of GridFunction.

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem vector) (mfem operators)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg)
             (mfem bilinearform) (mfem sparsemat))
(use-modules (mesh-primitive) (fe_coll-primitive) (fespace-primitive)
             (coefficient-primitive) (bilininteg-primitive)
             (bilinearform-primitive))

(test-begin "mfem-domain-int")

;; Integrate constant 1.0 over unit square — should give area = 1.0.
;; Assembles mass matrix M, then computes x^T M x where x = [1,...,1].
;; For H1 linear elements with all DOFs = 1 (constant function),
;; x^T M x = integral of 1^2 over domain = area.
(test-group "constant-integral-quad"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (a    (new-BilinearForm fes))
         (n    (FiniteElementSpace-GetVSize fes))
         (x    (make <Vector> n)))
    (BilinearForm-AddDomainIntegrator a (new-MassIntegrator one))
    (BilinearForm-Assemble a)
    (BilinearForm-Finalize a)
    (Assign x 1.0)
    (let ((integral (InnerProduct (BilinearForm-SpMat a) x x)))
      (test-approximate "integral of 1 over unit square" 1.0 integral 1e-12))))

;; Integrate constant 1.0 over unit square with triangles
(test-group "constant-integral-tri"
  (let* ((mesh (new-Mesh 4 4 "TRIANGLE"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (a    (new-BilinearForm fes))
         (n    (FiniteElementSpace-GetVSize fes))
         (x    (make <Vector> n)))
    (BilinearForm-AddDomainIntegrator a (new-MassIntegrator one))
    (BilinearForm-Assemble a)
    (BilinearForm-Finalize a)
    (Assign x 1.0)
    (let ((integral (InnerProduct (BilinearForm-SpMat a) x x)))
      (test-approximate "integral of 1 over unit square (tri)" 1.0 integral 1e-12))))

;; Integrate constant over unit cube
(test-group "constant-integral-hex"
  (let* ((mesh (new-Mesh 2 2 2 "HEXAHEDRON"))
         (fec  (new-H1-FECollection 1 3))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (a    (new-BilinearForm fes))
         (n    (FiniteElementSpace-GetVSize fes))
         (x    (make <Vector> n)))
    (BilinearForm-AddDomainIntegrator a (new-MassIntegrator one))
    (BilinearForm-Assemble a)
    (BilinearForm-Finalize a)
    (Assign x 1.0)
    (let ((integral (InnerProduct (BilinearForm-SpMat a) x x)))
      (test-approximate "integral of 1 over unit cube" 1.0 integral 1e-12))))

(define runner (test-runner-current))
(test-end "mfem-domain-int")
(exit (zero? (test-runner-fail-count runner)))
