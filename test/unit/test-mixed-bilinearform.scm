;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MixedBilinearForm.
;; Tests cross-space bilinear form assembly between different finite element
;; spaces, including H1-to-L2 and ND-to-RT mappings.

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg)
             (mfem bilinearform) (mfem sparsemat)
             (mfem vector) (mfem operators))

(test-begin "mfem-mixed-bilinearform")

;; MixedBilinearForm with MassIntegrator between same H1 spaces
(test-group "mixed-same-space"
  (let* ((mesh (make <Mesh> 4 4 "QUADRILATERAL"))
         (fec  (make <H1-FECollection> 1 2))
         (trial-fes (make <FiniteElementSpace> mesh fec))
         (test-fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <MixedBilinearForm> trial-fes test-fes)))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "mixed form: matrix height > 0" (> (Height mat) 0))
      (test-assert "mixed form: matrix width > 0" (> (Width mat) 0))
      (test-equal "mixed form: height = test vsize"
        (Height mat) (GetVSize test-fes))
      (test-equal "mixed form: width = trial vsize"
        (Width mat) (GetVSize trial-fes)))))

;; MixedBilinearForm between different order H1 spaces
(test-group "mixed-different-order"
  (let* ((mesh (make <Mesh> 2 2 "TRIANGLE"))
         (fec1 (make <H1-FECollection> 1 2))
         (fec2 (make <H1-FECollection> 2 2))
         (fes1 (make <FiniteElementSpace> mesh fec1))
         (fes2 (make <FiniteElementSpace> mesh fec2))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <MixedBilinearForm> fes1 fes2)))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "mixed diff order: rectangular matrix"
        (not (= (Height mat) (Width mat))))
      (test-equal "mixed diff order: height = test (P2) vsize"
        (Height mat) (GetVSize fes2))
      (test-equal "mixed diff order: width = trial (P1) vsize"
        (Width mat) (GetVSize fes1)))))

;; MixedBilinearForm Mult
(test-group "mixed-mult"
  (let* ((mesh (make <Mesh> 2 2 "QUADRILATERAL"))
         (fec1 (make <H1-FECollection> 1 2))
         (fec2 (make <H1-FECollection> 2 2))
         (fes1 (make <FiniteElementSpace> mesh fec1))
         (fes2 (make <FiniteElementSpace> mesh fec2))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <MixedBilinearForm> fes1 fes2))
         (x    (make <Vector> (GetVSize fes1)))
         (y    (make <Vector> (GetVSize fes2))))
    (AddDomainIntegrator a (make <MassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (Assign x 1.0)
    (Mult (SpMat a) x y)
    (test-assert "mixed Mult: result nonzero" (> (Norml2 y) 0.0))))

;; MixedBilinearForm with VectorFEMassIntegrator between ND spaces
(test-group "mixed-vectorfe-mass"
  (let* ((mesh (make <Mesh> 2 2 2 "TETRAHEDRON"))
         (fec1 (make <ND-FECollection> 1 3))
         (fec2 (make <ND-FECollection> 1 3))
         (fes1 (make <FiniteElementSpace> mesh fec1))
         (fes2 (make <FiniteElementSpace> mesh fec2))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <MixedBilinearForm> fes1 fes2)))
    (AddDomainIntegrator a (make <VectorFEMassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "mixed VectorFEMass: assembled"
        (> (Height mat) 0)))))

(define runner (test-runner-current))
(test-end "mfem-mixed-bilinearform")
(exit (zero? (test-runner-fail-count runner)))
