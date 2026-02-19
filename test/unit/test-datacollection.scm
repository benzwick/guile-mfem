;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM VisItDataCollection (from PyMFEM test_datacollection.py).

(use-modules (test unit-harness))
(skip-unless '(mfem datacollection))

(use-modules (srfi srfi-64)
             (mfem datacollection)
             (mfem mesh) (mfem fe_coll) (mfem fespace)
             (mfem gridfunc))
(use-modules (datacollection-primitive)
             (mesh-primitive) (fe_coll-primitive) (fespace-primitive)
             (gridfunc-primitive))

(test-begin "mfem-datacollection")

;; VisItDataCollection: Save and verify files created
(test-group "visit-save"
  (let* ((mesh (new-Mesh 4 4 "TRIANGLE"))
         (fec  (new-H1-FECollection 1 2))
         (fes  (new-FiniteElementSpace mesh fec))
         (gf   (new-GridFunction fes))
         (dc   (new-VisItDataCollection "test_dc" mesh)))
    (GridFunction-Assign gf 1.0)
    (VisItDataCollection-RegisterField dc "gf" gf)
    (VisItDataCollection-Save dc)
    (test-assert "collection directory created"
      (file-exists? "test_dc_000000"))))

(define runner (test-runner-current))
(test-end "mfem-datacollection")
(exit (zero? (test-runner-fail-count runner)))
