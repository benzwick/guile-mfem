;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex2.scm — Linear elasticity (MFEM Example 2)

(use-modules (srfi srfi-64) (test example-harness))

(define-values (run run/xfail) (make-example-runner "ex2.scm" '("sol.gf" "displaced.mesh")))

(test-begin "mfem-ex2")

(run)

(when full?
  (run "-m" "beam-tri.mesh")
  (run "-m" "beam-quad.mesh")
  (run "-m" "beam-tet.mesh")
  (run "-m" "beam-hex.mesh")
  (run "-m" "beam-wedge.mesh")
  (run "-m" "beam-quad.mesh" "-o" "3" "-s")

  ;; NURBS meshes: xfail due to NURBSext null pointer bug (see BUGS.md)
  (run/xfail "-m" "beam-quad-nurbs.mesh")
  (run/xfail "-m" "beam-hex-nurbs.mesh"))

(test-end/exit "mfem-ex2")
