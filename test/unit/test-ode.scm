;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM ODE solvers (from MFEM test_ode.cpp).
;; Requires (mfem ode) module and SWIG directors for TimeDependentOperator.

;; Guard: skip if required module is not compiled
(unless (false-if-exception (resolve-interface '(mfem ode)))
  (format (current-error-port) "SKIP: module (mfem ode) not available~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem ode) (mfem vector))
(use-modules (ode-primitive))

(test-begin "mfem-ode")

;; ForwardEulerSolver construction
(test-group "forward-euler"
  (let ((solver (new-ForwardEulerSolver)))
    (test-assert "ForwardEulerSolver created" solver)))

;; RK2Solver construction
(test-group "rk2"
  (let ((solver (new-RK2Solver)))
    (test-assert "RK2Solver created" solver)))

;; RK4Solver construction
(test-group "rk4"
  (let ((solver (new-RK4Solver)))
    (test-assert "RK4Solver created" solver)))

(define runner (test-runner-current))
(test-end "mfem-ode")
(exit (zero? (test-runner-fail-count runner)))
