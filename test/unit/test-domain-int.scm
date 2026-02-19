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

(test-begin "mfem-domain-int")

;; Integrate constant 1.0 over unit square — should give area = 1.0.
;; Assembles mass matrix M, then computes x^T M x where x = [1,...,1].
;; For H1 linear elements with all DOFs = 1 (constant function),
;; x^T M x = integral of 1^2 over domain = area.
(test-group "constant-integral-quad"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (n    (GetVSize fes))
         (x    (make <Vector> n)))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (Assign x 1.0)
    (let ((integral (InnerProduct (SpMat a) x x)))
      (test-approximate "integral of 1 over unit square" 1.0 integral 1e-12))))

;; Integrate constant 1.0 over unit square with triangles
(test-group "constant-integral-tri"
  (let* ((mesh (make <Mesh> 4 4 "TRIANGLE"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (n    (GetVSize fes))
         (x    (make <Vector> n)))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (Assign x 1.0)
    (let ((integral (InnerProduct (SpMat a) x x)))
      (test-approximate "integral of 1 over unit square (tri)" 1.0 integral 1e-12))))

;; Integrate constant over unit cube
(test-group "constant-integral-hex"
  (let* ((mesh (make <Mesh> 2 2 2 "HEXAHEDRON"))
         (fec  (make <H1-FECollection> 1 3))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes))
         (n    (GetVSize fes))
         (x    (make <Vector> n)))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (Assign x 1.0)
    (let ((integral (InnerProduct (SpMat a) x x)))
      (test-approximate "integral of 1 over unit cube" 1.0 integral 1e-12))))

(define runner (test-runner-current))
(test-end "mfem-domain-int")
(exit (zero? (test-runner-fail-count runner)))
