;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex4-lisp.scm — Grad-div problem (MFEM Example 4)

(use-modules (srfi srfi-64) (test example-harness))

(define-values (run run/xfail) (make-example-runner "ex4-lisp.scm" '("sol.gf" "refined.mesh")))

(test-begin "mfem-ex4-lisp")

(run)

(when full?
  (run "-m" "square-disc.mesh")
  (run "-m" "star.mesh")
  (run "-m" "beam-tet.mesh")
  (run "-m" "beam-hex.mesh")
  (run "-m" "escher.mesh")
  (run "-m" "fichera.mesh")
  (run "-m" "fichera-q2.mesh" "-o" "2")
  (run "-m" "fichera-q3.mesh" "-o" "3")
  (run "-m" "square-disc.mesh" "-o" "2" "-s")
  (run "-m" "beam-tet.mesh" "-s"))

(test-end/exit "mfem-ex4-lisp")
