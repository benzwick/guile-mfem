;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM BlockOperator (from PyMFEM test_blockoperator.py).

(use-modules (test unit-harness))
(skip-unless '(mfem blockoperator))

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem blockoperator) (mfem array) (mfem operators))

(test-begin "mfem-blockoperator")

;; BlockOperator construction
(test-group "construction"
  (let* ((offset (make <intArray> 3)))
    (set offset 0 0)
    (set offset 1 3)
    (set offset 2 6)
    (let ((bo (make <BlockOperator> offset)))
      (test-assert "BlockOperator created" bo)
      (test-equal "BlockOperator height" 6 (Height bo))
      (test-equal "BlockOperator width" 6 (Width bo)))))

;; BlockDiagonalPreconditioner construction
(test-group "block-diag-prec"
  (let* ((offset (make <intArray> 3)))
    (set offset 0 0)
    (set offset 1 3)
    (set offset 2 6)
    (let ((bdp (make <BlockDiagonalPreconditioner> offset)))
      (test-assert "BlockDiagonalPreconditioner created" bdp))))

(define runner (test-runner-current))
(test-end "mfem-blockoperator")
(exit (zero? (test-runner-fail-count runner)))
