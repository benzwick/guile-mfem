;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM BlockOperator (from PyMFEM test_blockoperator.py).

;; Guard: skip if required module is not compiled
(unless (false-if-exception (resolve-interface '(mfem blockoperator)))
  (format (current-error-port) "SKIP: module (mfem blockoperator) not available~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem blockoperator) (mfem array) (mfem operators))
(use-modules (blockoperator-primitive) (array-primitive))

(test-begin "mfem-blockoperator")

;; BlockOperator construction
(test-group "construction"
  (let* ((offset (new-intArray 3)))
    (intArray-set offset 0 0)
    (intArray-set offset 1 3)
    (intArray-set offset 2 6)
    (let ((bo (new-BlockOperator offset)))
      (test-assert "BlockOperator created" bo)
      (test-equal "BlockOperator height" 6 (Height bo))
      (test-equal "BlockOperator width" 6 (Width bo)))))

;; BlockDiagonalPreconditioner construction
(test-group "block-diag-prec"
  (let* ((offset (new-intArray 3)))
    (intArray-set offset 0 0)
    (intArray-set offset 1 3)
    (intArray-set offset 2 6)
    (let ((bdp (new-BlockDiagonalPreconditioner offset)))
      (test-assert "BlockDiagonalPreconditioner created" bdp))))

(define runner (test-runner-current))
(test-end "mfem-blockoperator")
(exit (zero? (test-runner-fail-count runner)))
