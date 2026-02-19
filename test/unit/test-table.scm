;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Table (from PyMFEM test_table.py).

;; Guard: skip if required module is not compiled
(unless (false-if-exception (resolve-interface '(mfem table)))
  (format (current-error-port) "SKIP: module (mfem table) not available~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem table) (mfem mesh))
(use-modules (table-primitive) (mesh-primitive))

(test-begin "mfem-table")

;; GetEdgeVertexTable from a mesh
(test-group "edge-vertex-table"
  (let* ((mesh (new-Mesh 2 2 "TRIANGLE"))
         (tbl  (Mesh-GetEdgeVertexTable mesh)))
    (test-assert "Table exists" tbl)
    (test-assert "Table Size > 0" (> (Table-Size tbl) 0))))

(define runner (test-runner-current))
(test-end "mfem-table")
(exit (zero? (test-runner-fail-count runner)))
