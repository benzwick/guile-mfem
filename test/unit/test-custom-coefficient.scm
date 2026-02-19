;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for user-defined Scheme coefficients via SWIG directors.
;; Conceptual replacement for PyMFEM's test_numba.py using Guile JIT
;; instead of Numba. Requires SWIG director support for Coefficient classes.
;;
;; Once directors are enabled, this test will:
;; 1. Define a custom Coefficient subclass in Scheme
;; 2. Call ProjectCoefficient with the Scheme-defined coefficient
;; 3. Compare results with ConstantCoefficient (C++ implementation)
;; 4. Guile's JIT (since 3.0) automatically optimizes the Scheme callback

;; Guard: skip until SWIG director support is enabled for coefficient classes
;; TODO: Replace this guard with a proper check once directors are available
(format (current-error-port) "SKIP: SWIG director support for coefficients not yet available~%")
(exit 77)

(use-modules (srfi srfi-64)
             (mfem coefficient) (mfem mesh) (mfem fe_coll)
             (mfem fespace) (mfem gridfunc))

(test-begin "mfem-custom-coefficient")

;; Placeholder: define custom coefficient and project it
(test-group "custom-scalar-coefficient"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-custom-coefficient")
(exit (zero? (test-runner-fail-count runner)))
