;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Table (from PyMFEM test_table.py).

(use-modules (test unit-harness))
(skip-unless '(mfem table))

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem table) (mfem mesh))

(test-begin "mfem-table")

;; GetEdgeVertexTable from a mesh
(test-group "edge-vertex-table"
  (let* ((mesh (make <Mesh> 2 2 "TRIANGLE"))
         (tbl  (GetEdgeVertexTable mesh)))
    (test-assert "Table exists" tbl)
    (test-assert "Table Size > 0" (> (Size tbl) 0))))

(define runner (test-runner-current))
(test-end "mfem-table")
(exit (zero? (test-runner-fail-count runner)))
