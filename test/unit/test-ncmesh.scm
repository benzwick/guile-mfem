;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM NCMesh / general refinement (from PyMFEM test_ncmesh.py).

(use-modules (test unit-harness))
(skip-unless '(mfem ncmesh))

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem ncmesh) (mfem mesh) (mfem array))

;; MFEM data directory (set by CTest via MFEM_DATA_DIR environment variable)
(define mfem-data-dir (getenv "MFEM_DATA_DIR"))

(test-begin "mfem-ncmesh")

;; GeneralRefinement with RefinementArray
(test-group "general-refinement"
  (let* ((mesh (make <Mesh> (string-append mfem-data-dir "/inline-quad.mesh") 1 1))
         (ne-before (GetNE mesh))
         (refs (make <RefinementArray>)))
    (Append refs (make <Refinement> 0 #b11))
    (GeneralRefinement mesh refs)
    (test-assert "NE increased after refinement"
      (> (GetNE mesh) ne-before))))

;; GetRefinementTransforms
(test-group "refinement-transforms"
  (let* ((mesh (make <Mesh> (string-append mfem-data-dir "/inline-quad.mesh") 1 1)))
    (let ((refs (make <RefinementArray>)))
      (Append refs (make <Refinement> 0 #b11))
      (GeneralRefinement mesh refs))
    (let ((cft (GetRefinementTransforms mesh)))
      (test-assert "GetRefinementTransforms returns object" cft))))

(define runner (test-runner-current))
(test-end "mfem-ncmesh")
(exit (zero? (test-runner-fail-count runner)))
