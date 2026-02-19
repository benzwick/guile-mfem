;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM NCMesh / general refinement (from PyMFEM test_ncmesh.py).

;; Guard: skip if required module is not compiled
(unless (false-if-exception (resolve-interface '(mfem ncmesh)))
  (format (current-error-port) "SKIP: module (mfem ncmesh) not available~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem ncmesh) (mfem mesh) (mfem array))
(use-modules (ncmesh-primitive) (mesh-primitive) (array-primitive))

;; MFEM data directory (set by CTest via MFEM_DATA_DIR environment variable)
(define mfem-data-dir (getenv "MFEM_DATA_DIR"))

(test-begin "mfem-ncmesh")

;; GeneralRefinement with RefinementArray
(test-group "general-refinement"
  (let* ((mesh (new-Mesh (string-append mfem-data-dir "/inline-quad.mesh") 1 1))
         (ne-before (Mesh-GetNE mesh))
         (refs (new-RefinementArray)))
    (RefinementArray-Append refs (new-Refinement 0 #b11))
    (Mesh-GeneralRefinement mesh refs)
    (test-assert "NE increased after refinement"
      (> (Mesh-GetNE mesh) ne-before))))

;; GetRefinementTransforms
(test-group "refinement-transforms"
  (let* ((mesh (new-Mesh (string-append mfem-data-dir "/inline-quad.mesh") 1 1)))
    (let ((refs (new-RefinementArray)))
      (RefinementArray-Append refs (new-Refinement 0 #b11))
      (Mesh-GeneralRefinement mesh refs))
    (let ((cft (Mesh-GetRefinementTransforms mesh)))
      (test-assert "GetRefinementTransforms returns object" cft))))

(define runner (test-runner-current))
(test-end "mfem-ncmesh")
(exit (zero? (test-runner-fail-count runner)))
