;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Test for examples/ex0.scm — Poisson problem (MFEM Example 0)
;;
;; Output files: sol.gf, mesh.mesh
;;
;; TEST_MODE=fast: default mesh only
;; TEST_MODE=full: all sample runs from ex0.scm header
;;
;; Sample runs from header:
;;   guile -L build ex0.scm
;;   guile -L build ex0.scm -m ../data/fichera.mesh
;;   guile -L build ex0.scm -m ../data/square-disc.mesh -o 2

(use-modules (srfi srfi-64) (ice-9 format))

(define mfem-data-dir (getenv "MFEM_DATA_DIR"))
(define guile-load-path (getenv "LTDL_LIBRARY_PATH"))
(define project-root (dirname (dirname (dirname (current-filename)))))
(define script (string-append project-root "/examples/ex0.scm"))

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
  "Run ex0 with ARGS, check output files, print result with timing."
  (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
    '("sol.gf" "mesh.mesh"))
  (let* ((start (gettimeofday))
         (rc (apply system* "guile" "-L" guile-load-path script args))
         (end (gettimeofday))
         (dt (elapsed start end))
         (ok? (and (zero? (status:exit-val rc))
                   (file-non-empty? "sol.gf")
                   (file-non-empty? "mesh.mesh"))))
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

(test-begin "mfem-ex0")
(format #t "~%=== ex0 (TEST_MODE=~a) ===~%" test-mode)

;; fast + full: default mesh (star.mesh) — covers header lines 1-2
(run-example "star.mesh (default)"
  (list "-m" (string-append mfem-data-dir "/star.mesh"))
  #f)

(when full?
  (run-example "fichera.mesh"
    (list "-m" (string-append mfem-data-dir "/fichera.mesh"))
    #f)
  (run-example "square-disc.mesh -o 2"
    (list "-m" (string-append mfem-data-dir "/square-disc.mesh") "-o" "2")
    #f))

(format #t "  --- total: ~,2fs ---~%~%" total-time)

(define runner (test-runner-current))
(test-end "mfem-ex0")
(exit (zero? (test-runner-fail-count runner)))
