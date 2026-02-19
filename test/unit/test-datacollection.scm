;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM VisItDataCollection (from PyMFEM test_datacollection.py).

(use-modules (test unit-harness))
(skip-unless '(mfem datacollection))

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem datacollection)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem gridfunc))

(test-begin "mfem-datacollection")

;; VisItDataCollection: Save and verify files created
(test-group "visit-save"
  (let* ((mesh (make <Mesh> 4 4 "TRIANGLE"))
         (fec  (make <H1-FECollection> 1 2))
         (fes  (make <FiniteElementSpace> mesh fec))
         (gf   (make <GridFunction> fes))
         (dc   (make <VisItDataCollection> "test_dc" mesh)))
    (Assign gf 1.0)
    (RegisterField dc "gf" gf)
    (Save dc)
    (test-assert "collection directory created"
      (file-exists? "test_dc_000000"))))

(define runner (test-runner-current))
(test-end "mfem-datacollection")
(exit (zero? (test-runner-fail-count runner)))
