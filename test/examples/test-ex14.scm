;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex14.scm — DG diffusion (MFEM Example 14)

(use-modules (srfi srfi-64) (test example-harness))

(define-values (run run/xfail) (make-example-runner "ex14.scm" '("sol.gf" "refined.mesh")))

(test-begin "mfem-ex14")

(run)

(when full?
  (run "-m" "star.mesh")
  (run "-m" "escher.mesh")
  (run "-m" "fichera.mesh")
  (run "-m" "inline-quad.mesh")
  (run "-m" "inline-hex.mesh")
  (run "-m" "star.mesh" "-o" "2")
  (run "-m" "inline-quad.mesh" "-o" "2"))

(test-end/exit "mfem-ex14")
