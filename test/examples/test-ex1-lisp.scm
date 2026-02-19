;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex1-lisp.scm — Poisson problem (MFEM Example 1)

(use-modules (srfi srfi-64) (test example-harness))

(define-values (run run/xfail) (make-example-runner "ex1-lisp.scm" '("sol.gf" "refined.mesh")))

(test-begin "mfem-ex1-lisp")

(run "-m" "star.mesh")

(when full?
  (run "-m" "square-disc.mesh")
  (run "-m" "fichera.mesh")

  ;; NURBS meshes with order -1: xfail due to null pointer truthiness
  ;; bug in isoparametric path (see BUGS.md)
  (run/xfail "-m" "square-disc-nurbs.mesh" "-o" "-1")
  (run/xfail "-m" "mobius-strip.mesh" "-o" "-1" "-s"))

(test-end/exit "mfem-ex1-lisp")
