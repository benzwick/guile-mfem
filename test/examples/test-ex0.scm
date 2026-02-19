;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex0.scm — Poisson problem (MFEM Example 0)

(use-modules (srfi srfi-64) (test example-harness))

(define-values (run run/xfail) (make-example-runner "ex0.scm" '("sol.gf" "mesh.mesh")))

(test-begin "mfem-ex0")

(run)

(when full?
  (run "-m" "fichera.mesh")
  (run "-m" "square-disc.mesh" "-o" "2"))

(test-end/exit "mfem-ex0")
