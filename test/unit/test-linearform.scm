;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM LinearForm (from MFEM test_linearform_ext.cpp).

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem vector) (mfem operators)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem coefficient) (mfem lininteg) (mfem linearform))
(use-modules (mesh-primitive) (fe_coll-primitive) (fespace-primitive)
             (coefficient-primitive) (lininteg-primitive)
             (linearform-primitive))

(test-begin "mfem-linearform")

;; Basic assembly with DomainLFIntegrator
(test-group "domain-lf-assemble"
  (let* ((mesh (new-Mesh 4 4 "QUADRILATERAL"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (b    (new-LinearForm fes)))
    (LinearForm-AddDomainIntegrator b (new-DomainLFIntegrator one))
    (LinearForm-Assemble b)
    (test-assert "LinearForm size > 0" (> (Size b) 0))
    ;; Integrating constant 1.0 over unit square should give area = 1.0
    ;; Sum of all DOF contributions approximates the integral
    (test-assert "LinearForm has nonzero entries"
      (> (Norml2 b) 0.0))))

;; Assembly on triangle mesh
(test-group "domain-lf-triangle"
  (let* ((mesh (new-Mesh 2 2 "TRIANGLE"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (one  (new-ConstantCoefficient 1.0))
         (b    (new-LinearForm fes)))
    (LinearForm-AddDomainIntegrator b (new-DomainLFIntegrator one))
    (LinearForm-Assemble b)
    (test-assert "triangle LF assembled" (> (Size b) 0))))

(define runner (test-runner-current))
(test-end "mfem-linearform")
(exit (zero? (test-runner-fail-count runner)))
