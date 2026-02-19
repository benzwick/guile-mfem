;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM ODE solvers (from MFEM test_ode.cpp).
;; Requires (mfem ode) module and SWIG directors for TimeDependentOperator.

(use-modules (test unit-harness))
(skip-unless '(mfem ode))

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem ode) (mfem vector))

(test-begin "mfem-ode")

;; ForwardEulerSolver construction
(test-group "forward-euler"
  (let ((solver (make <ForwardEulerSolver>)))
    (test-assert "ForwardEulerSolver created" solver)))

;; RK2Solver construction
(test-group "rk2"
  (let ((solver (make <RK2Solver>)))
    (test-assert "RK2Solver created" solver)))

;; RK4Solver construction
(test-group "rk4"
  (let ((solver (make <RK4Solver>)))
    (test-assert "RK4Solver created" solver)))

(define runner (test-runner-current))
(test-end "mfem-ode")
(exit (zero? (test-runner-fail-count runner)))
