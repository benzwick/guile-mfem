;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex2-lisp.scm — Linear elasticity (MFEM Example 2)

(use-modules (srfi srfi-64) (test example-harness))

(define-values (run run/xfail) (make-example-runner "ex2-lisp.scm" '("sol.gf" "displaced.mesh")))

(test-begin "mfem-ex2-lisp")

(run "-m" "beam-tri.mesh")

(when full?
  (run "-m" "beam-quad.mesh")
  (run "-m" "beam-tet.mesh")
  (run "-m" "beam-hex.mesh")
  (run "-m" "beam-quad.mesh" "-o" "3" "-s"))

(test-end/exit "mfem-ex2-lisp")
