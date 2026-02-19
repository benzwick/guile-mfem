;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex1.scm — Poisson problem (MFEM Example 1)
;;
;; Output files: sol.gf, refined.mesh
;;
;; TEST_MODE=fast: default mesh only
;; TEST_MODE=full: all sample runs from ex1.scm header
;;
;; Sample runs from header:
;;   guile -L build ex1.scm
;;   guile -L build ex1.scm -m ../data/star.mesh
;;   guile -L build ex1.scm -m ../data/square-disc.mesh
;;   guile -L build ex1.scm -m ../data/escher.mesh
;;   guile -L build ex1.scm -m ../data/fichera.mesh
;;   guile -L build ex1.scm -m ../data/fichera-mixed.mesh
;;   guile -L build ex1.scm -m ../data/toroid-wedge.mesh
;;   guile -L build ex1.scm -m ../data/square-disc-p2.vtk -o 2
;;   guile -L build ex1.scm -m ../data/square-disc-p3.mesh -o 3
;;   guile -L build ex1.scm -m ../data/square-disc-nurbs.mesh -o -1
;;   guile -L build ex1.scm -m ../data/star-mixed-p2.mesh -o 2
;;   guile -L build ex1.scm -m ../data/disc-nurbs.mesh -o -1
;;   guile -L build ex1.scm -m ../data/pipe-nurbs.mesh -o -1
;;   guile -L build ex1.scm -m ../data/star-surf.mesh
;;   guile -L build ex1.scm -m ../data/square-disc-surf.mesh
;;   guile -L build ex1.scm -m ../data/inline-segment.mesh
;;   guile -L build ex1.scm -m ../data/amr-quad.mesh
;;   guile -L build ex1.scm -m ../data/amr-hex.mesh
;;   guile -L build ex1.scm -m ../data/fichera-amr.mesh
;;   guile -L build ex1.scm -m ../data/mobius-strip.mesh
;;   guile -L build ex1.scm -m ../data/mobius-strip.mesh -o -1 -s

(use-modules (srfi srfi-64) (ice-9 format))

(define mfem-data-dir (getenv "MFEM_DATA_DIR"))
(define guile-load-path (getenv "LTDL_LIBRARY_PATH"))
(define project-root (dirname (dirname (dirname (current-filename)))))
(define script (string-append project-root "/examples/ex1.scm"))

(define test-mode (or (getenv "TEST_MODE") "full"))
(define full? (string=? test-mode "full"))

;;; Timing and reporting helpers

(define (elapsed start end)
  (+ (- (car end) (car start))
     (/ (- (cdr end) (cdr start)) 1000000.0)))

(define (file-non-empty? path)
  (catch #t
    (lambda () (> (stat:size (stat path)) 100))
    (lambda _ #f)))

(define total-time 0.0)

(define (run-example label args xfail?)
  "Run ex1 with ARGS, check output files, print result with timing."
  (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
    '("sol.gf" "refined.mesh"))
  (let* ((start (gettimeofday))
         (rc (apply system* "guile" "-L" guile-load-path script args))
         (end (gettimeofday))
         (dt (elapsed start end))
         (ok? (and (zero? (status:exit-val rc))
                   (file-non-empty? "sol.gf")
                   (file-non-empty? "refined.mesh"))))
    (set! total-time (+ total-time dt))
    (format #t "  ~6a  ~a  (~,2fs)~%"
            (cond ((and ok? (not xfail?)) "PASS")
                  ((and (not ok?) xfail?) "XFAIL")
                  ((and ok? xfail?) "XPASS")
                  (else "FAIL"))
            label dt)
    (when xfail? (test-expect-fail 1))
    (test-assert label ok?)))

;;; Tests

(test-begin "mfem-ex1")
(format #t "~%=== ex1 (TEST_MODE=~a) ===~%" test-mode)

;; fast + full: default mesh (star.mesh) — covers header lines 1-2
(run-example "star.mesh (default)"
  (list "-m" (string-append mfem-data-dir "/star.mesh"))
  #f)

(when full?
  (run-example "square-disc.mesh"
    (list "-m" (string-append mfem-data-dir "/square-disc.mesh"))
    #f)
  (run-example "escher.mesh"
    (list "-m" (string-append mfem-data-dir "/escher.mesh"))
    #f)
  (run-example "fichera.mesh"
    (list "-m" (string-append mfem-data-dir "/fichera.mesh"))
    #f)
  (run-example "fichera-mixed.mesh"
    (list "-m" (string-append mfem-data-dir "/fichera-mixed.mesh"))
    #f)
  (run-example "toroid-wedge.mesh"
    (list "-m" (string-append mfem-data-dir "/toroid-wedge.mesh"))
    #f)
  (run-example "square-disc-p2.vtk -o 2"
    (list "-m" (string-append mfem-data-dir "/square-disc-p2.vtk") "-o" "2")
    #f)
  (run-example "square-disc-p3.mesh -o 3"
    (list "-m" (string-append mfem-data-dir "/square-disc-p3.mesh") "-o" "3")
    #f)
  (run-example "star-mixed-p2.mesh -o 2"
    (list "-m" (string-append mfem-data-dir "/star-mixed-p2.mesh") "-o" "2")
    #f)
  (run-example "star-surf.mesh"
    (list "-m" (string-append mfem-data-dir "/star-surf.mesh"))
    #f)
  (run-example "square-disc-surf.mesh"
    (list "-m" (string-append mfem-data-dir "/square-disc-surf.mesh"))
    #f)
  (run-example "inline-segment.mesh"
    (list "-m" (string-append mfem-data-dir "/inline-segment.mesh"))
    #f)
  (run-example "amr-quad.mesh"
    (list "-m" (string-append mfem-data-dir "/amr-quad.mesh"))
    #f)
  (run-example "amr-hex.mesh"
    (list "-m" (string-append mfem-data-dir "/amr-hex.mesh"))
    #f)
  (run-example "fichera-amr.mesh"
    (list "-m" (string-append mfem-data-dir "/fichera-amr.mesh"))
    #f)
  (run-example "mobius-strip.mesh"
    (list "-m" (string-append mfem-data-dir "/mobius-strip.mesh"))
    #f)

  ;; NURBS meshes with order -1: the example errors out because the
  ;; isoparametric path is broken (null pointer truthiness bug, see
  ;; BUGS.md). These are marked xfail — when the bug is fixed, they
  ;; will xpass, signaling that the error and xfail can be removed.
  (run-example "square-disc-nurbs.mesh -o -1 (xfail: null ptr bug)"
    (list "-m" (string-append mfem-data-dir "/square-disc-nurbs.mesh") "-o" "-1")
    #t)
  (run-example "disc-nurbs.mesh -o -1 (xfail: null ptr bug)"
    (list "-m" (string-append mfem-data-dir "/disc-nurbs.mesh") "-o" "-1")
    #t)
  (run-example "pipe-nurbs.mesh -o -1 (xfail: null ptr bug)"
    (list "-m" (string-append mfem-data-dir "/pipe-nurbs.mesh") "-o" "-1")
    #t)
  (run-example "mobius-strip.mesh -o -1 -s (xfail: null ptr bug)"
    (list "-m" (string-append mfem-data-dir "/mobius-strip.mesh") "-o" "-1" "-s")
    #t))

(format #t "  --- total: ~,2fs ---~%~%" total-time)

(define runner (test-runner-current))
(test-end "mfem-ex1")
(exit (zero? (test-runner-fail-count runner)))
