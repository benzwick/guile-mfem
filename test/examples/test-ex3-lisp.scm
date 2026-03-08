;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex3-lisp.scm — Definite Maxwell problem (MFEM Example 3)

(use-modules (srfi srfi-64) (test example-harness))

(define-values (run run/xfail) (make-example-runner "ex3-lisp.scm" '("sol.gf" "refined.mesh")))

(test-begin "mfem-ex3-lisp")

(run)

(when full?
  (run "-m" "fichera.mesh")
  (run "-m" "beam-tet.mesh")
  (run "-m" "escher.mesh")
  (run "-m" "fichera-q2.mesh" "-o" "2")
  (run "-m" "fichera-q3.mesh" "-o" "3")
  (run "-m" "star-surf.mesh" "-o" "1")
  (run "-m" "beam-tet.mesh" "-s"))

(test-end/exit "mfem-ex3-lisp")
