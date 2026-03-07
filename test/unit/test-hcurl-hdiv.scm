;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for H(curl) and H(div) finite elements and integrators.
;; Tests ND_FECollection, RT_FECollection, CurlCurlIntegrator,
;; DivDivIntegrator, VectorFEMassIntegrator, VectorFEDomainLFIntegrator,
;; and VectorConstantCoefficient.

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem bilininteg) (mfem lininteg)
             (mfem bilinearform) (mfem linearform) (mfem sparsemat)
             (mfem vector) (mfem operators) (mfem array))

(test-begin "mfem-hcurl-hdiv")

;; --- H(curl) Nedelec elements ---

;; ND_FECollection creation
(test-group "nd-fecollection"
  (let ((fec (make <ND-FECollection> 1 3)))
    (test-assert "ND_FECollection created" fec)))

;; CurlCurlIntegrator assembly on tet mesh
(test-group "curlcurl-assemble-3d"
  (let* ((mesh (make <Mesh> 2 2 2 "TETRAHEDRON"))
         (fec  (make <ND-FECollection> 1 3))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes)))
    (AddDomainIntegrator a (make <CurlCurlIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "CurlCurl matrix height > 0" (> (Height mat) 0))
      (test-equal "CurlCurl matrix is square" (Height mat) (Width mat)))))

;; VectorFEMassIntegrator assembly on tet mesh with ND space
(test-group "vectorfe-mass-nd"
  (let* ((mesh (make <Mesh> 2 2 2 "TETRAHEDRON"))
         (fec  (make <ND-FECollection> 1 3))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes)))
    (AddDomainIntegrator a (make <VectorFEMassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "VectorFEMass ND matrix assembled" (> (Height mat) 0)))))

;; Combined curl-curl + mass (definite Maxwell operator)
(test-group "maxwell-operator"
  (let* ((mesh (make <Mesh> 2 2 2 "TETRAHEDRON"))
         (fec  (make <ND-FECollection> 1 3))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes)))
    (AddDomainIntegrator a (make <CurlCurlIntegrator> one))
    (AddDomainIntegrator a (make <VectorFEMassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "Maxwell operator height = fes vsize"
        (= (Height mat) (GetVSize fes))))))

;; VectorFEDomainLFIntegrator with VectorConstantCoefficient
(test-group "vectorfe-domain-lf"
  (let* ((mesh (make <Mesh> 2 2 2 "TETRAHEDRON"))
         (fec  (make <ND-FECollection> 1 3))
         (fes  (make <FiniteElementSpace> mesh fec))
         (f-vec (make <Vector> 3))
         (b    (make <LinearForm> fes)))
    (Assign f-vec 1.0)
    (AddDomainIntegrator b
      (make <VectorFEDomainLFIntegrator>
        (make <VectorConstantCoefficient> f-vec)))
    (Assemble b)
    (test-assert "VectorFE LF size = fes vsize"
      (= (Size b) (GetVSize fes)))
    (test-assert "VectorFE LF norm > 0" (> (Norml2 b) 0.0))))

;; --- H(div) Raviart-Thomas elements ---

;; RT_FECollection creation
(test-group "rt-fecollection"
  (let ((fec (make <RT-FECollection> 0 3)))
    (test-assert "RT_FECollection created" fec)))

;; DivDivIntegrator assembly on hex mesh
(test-group "divdiv-assemble-3d"
  (let* ((mesh (make <Mesh> 2 2 2 "HEXAHEDRON"))
         (fec  (make <RT-FECollection> 0 3))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes)))
    (AddDomainIntegrator a (make <DivDivIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "DivDiv matrix height > 0" (> (Height mat) 0))
      (test-equal "DivDiv matrix is square" (Height mat) (Width mat)))))

;; VectorFEMassIntegrator assembly on hex mesh with RT space
(test-group "vectorfe-mass-rt"
  (let* ((mesh (make <Mesh> 2 2 2 "HEXAHEDRON"))
         (fec  (make <RT-FECollection> 0 3))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes)))
    (AddDomainIntegrator a (make <VectorFEMassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "VectorFEMass RT matrix assembled" (> (Height mat) 0)))))

;; Combined div-div + mass (definite H(div) operator)
(test-group "hdiv-operator"
  (let* ((mesh (make <Mesh> 2 2 2 "HEXAHEDRON"))
         (fec  (make <RT-FECollection> 0 3))
         (fes  (make <FiniteElementSpace> mesh fec))
         (one  (make <ConstantCoefficient> 1.0))
         (a    (make <BilinearForm> fes)))
    (AddDomainIntegrator a (make <DivDivIntegrator> one))
    (AddDomainIntegrator a (make <VectorFEMassIntegrator> one))
    (Assemble a)
    (Finalize a)
    (let ((mat (SpMat a)))
      (test-assert "H(div) operator height = fes vsize"
        (= (Height mat) (GetVSize fes))))))

;; VectorFEDomainLFIntegrator with RT space
(test-group "vectorfe-domain-lf-rt"
  (let* ((mesh (make <Mesh> 2 2 "QUADRILATERAL"))
         (fec  (make <RT-FECollection> 0 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (f-vec (make <Vector> 2))
         (b    (make <LinearForm> fes)))
    (Assign f-vec 1.0)
    (AddDomainIntegrator b
      (make <VectorFEDomainLFIntegrator>
        (make <VectorConstantCoefficient> f-vec)))
    (Assemble b)
    (test-assert "RT LF size = fes vsize"
      (= (Size b) (GetVSize fes)))))

;; --- 2D tests ---

;; ND in 2D
(test-group "nd-2d"
  (let* ((mesh (make <Mesh> 4 4 "TRIANGLE"))
         (fec  (make <ND-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec)))
    (test-assert "ND 2D dofs > 0" (> (GetVSize fes) 0))))

;; RT in 2D
(test-group "rt-2d"
  (let* ((mesh (make <Mesh> 4 4 "TRIANGLE"))
         (fec  (make <RT-FECollection> 0 2))
         (fes  (make <FiniteElementSpace> mesh fec)))
    (test-assert "RT 2D dofs > 0" (> (GetVSize fes) 0))))

(define runner (test-runner-current))
(test-end "mfem-hcurl-hdiv")
(exit (zero? (test-runner-fail-count runner)))
