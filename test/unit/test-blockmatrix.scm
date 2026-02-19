;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM BlockMatrix (from PyMFEM test_blockmatrix.py).

(use-modules (test unit-harness))
(skip-unless '(mfem blockmatrix))

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem blockmatrix) (mfem sparsemat) (mfem array) (mfem operators))
(use-modules (blockmatrix-primitive) (array-primitive))

(test-begin "mfem-blockmatrix")

;; BlockMatrix construction
(test-group "construction"
  (let* ((offset (new-intArray 3)))
    (intArray-set offset 0 0)
    (intArray-set offset 1 3)
    (intArray-set offset 2 6)
    (let ((bm (new-BlockMatrix offset)))
      (test-assert "BlockMatrix created" bm)
      (test-equal "BlockMatrix height" 6 (Height bm))
      (test-equal "BlockMatrix width" 6 (Width bm)))))

(define runner (test-runner-current))
(test-end "mfem-blockmatrix")
(exit (zero? (test-runner-fail-count runner)))
