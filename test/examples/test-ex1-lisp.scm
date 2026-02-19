;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex1-lisp.scm — Poisson problem (MFEM Example 1)

(use-modules (srfi srfi-64) (test example-harness))

(define-values (run run/xfail) (make-example-runner "ex1-lisp.scm" '("sol.gf" "refined.mesh")))

(test-begin "mfem-ex1-lisp")

(run)

(when full?
  (run "-m" "square-disc.mesh")
  (run "-m" "star.mesh")
  (run "-m" "star-mixed.mesh")
  (run "-m" "escher.mesh")
  (run "-m" "fichera.mesh")
  (run "-m" "fichera-mixed.mesh")
  (run "-m" "toroid-wedge.mesh")
  (run "-m" "octahedron.mesh" "-o" "1")
  (run "-m" "periodic-annulus-sector.msh")
  (run "-m" "periodic-torus-sector.msh")
  (run "-m" "square-disc-p2.vtk" "-o" "2")
  (run "-m" "square-disc-p3.mesh" "-o" "3")
  ;; NURBS meshes with order -1: xfail due to null pointer truthiness
  ;; bug in isoparametric path (see BUGS.md)
  (run/xfail "-m" "square-disc-nurbs.mesh" "-o" "-1")
  (run "-m" "star-mixed-p2.mesh" "-o" "2")
  (run/xfail "-m" "disc-nurbs.mesh" "-o" "-1")
  (run/xfail "-m" "pipe-nurbs.mesh" "-o" "-1")
  (run "-m" "fichera-mixed-p2.mesh" "-o" "2")
  (run "-m" "star-surf.mesh")
  (run "-m" "square-disc-surf.mesh")
  (run "-m" "inline-segment.mesh")
  (run "-m" "amr-quad.mesh")
  (run "-m" "amr-hex.mesh")
  (run "-m" "fichera-amr.mesh")
  (run "-m" "mobius-strip.mesh")
  (run/xfail "-m" "mobius-strip.mesh" "-o" "-1" "-s")
  (run/xfail "-m" "nc3-nurbs.mesh" "-o" "-1"))

(test-end/exit "mfem-ex1-lisp")
