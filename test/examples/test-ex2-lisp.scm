;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex2-lisp.scm — Linear elasticity (MFEM Example 2)
;;
;; Output files: sol.gf, displaced.mesh
;;
;; TEST_MODE=fast: default mesh only
;; TEST_MODE=full: all sample runs from ex2-lisp.scm header
;;
;; Sample runs from header:
;;   guile -L build ex2-lisp.scm
;;   guile -L build ex2-lisp.scm -m ../data/beam-tri.mesh
;;   guile -L build ex2-lisp.scm -m ../data/beam-quad.mesh
;;   guile -L build ex2-lisp.scm -m ../data/beam-tet.mesh
;;   guile -L build ex2-lisp.scm -m ../data/beam-hex.mesh
;;   guile -L build ex2-lisp.scm -m ../data/beam-quad.mesh -o 3 -s

(use-modules (srfi srfi-64) (ice-9 format))

(define mfem-data-dir (getenv "MFEM_DATA_DIR"))
(define guile-load-path (getenv "LTDL_LIBRARY_PATH"))
(define project-root (dirname (dirname (dirname (current-filename)))))
(define script (string-append project-root "/examples/ex2-lisp.scm"))

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
  "Run ex2-lisp with ARGS, check output files, print result with timing."
  (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
    '("sol.gf" "displaced.mesh"))
  (let* ((start (gettimeofday))
         (rc (apply system* "guile" "-L" guile-load-path script args))
         (end (gettimeofday))
         (dt (elapsed start end))
         (ok? (and (zero? (status:exit-val rc))
                   (file-non-empty? "sol.gf")
                   (file-non-empty? "displaced.mesh"))))
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

(test-begin "mfem-ex2-lisp")
(format #t "~%=== ex2-lisp (TEST_MODE=~a) ===~%" test-mode)

;; fast + full: default mesh (beam-tri.mesh) — covers header lines 1-2
(run-example "beam-tri.mesh (default)"
  (list "-m" (string-append mfem-data-dir "/beam-tri.mesh"))
  #f)

(when full?
  (run-example "beam-quad.mesh"
    (list "-m" (string-append mfem-data-dir "/beam-quad.mesh"))
    #f)
  (run-example "beam-tet.mesh"
    (list "-m" (string-append mfem-data-dir "/beam-tet.mesh"))
    #f)
  (run-example "beam-hex.mesh"
    (list "-m" (string-append mfem-data-dir "/beam-hex.mesh"))
    #f)
  (run-example "beam-quad.mesh -o 3 -s"
    (list "-m" (string-append mfem-data-dir "/beam-quad.mesh") "-o" "3" "-s")
    #f))

(format #t "  --- total: ~,2fs ---~%~%" total-time)

(define runner (test-runner-current))
(test-end "mfem-ex2-lisp")
(exit (zero? (test-runner-fail-count runner)))
