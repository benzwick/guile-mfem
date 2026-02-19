;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;; Shared test harness for MFEM example tests.
;;;
;;; Provides make-example-runner which returns run and run/xfail
;;; procedures, plus test-mode, full?, and test-end/exit.

(define-module (test example-harness)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:export (test-mode full?
            make-example-runner
            test-end/exit))

(define test-mode (or (getenv "TEST_MODE") "full"))
(define full? (string=? test-mode "full"))

(define mfem-data-dir (getenv "MFEM_DATA_DIR"))
(define guile-load-path (getenv "LTDL_LIBRARY_PATH"))
(define project-root
  (dirname (dirname (%search-load-path "test/example-harness"))))

(define (elapsed start end)
  (+ (- (car end) (car start))
     (/ (- (cdr end) (cdr start)) 1000000.0)))

(define (file-non-empty? path)
  (catch #t
    (lambda () (> (stat:size (stat path)) 100))
    (lambda _ #f)))

(define total-time 0.0)

(define (expand-args args)
  "Scan ARGS for -m and expand the following mesh filename to full path."
  (let loop ((rest args) (result '()))
    (cond
      ((null? rest) (reverse result))
      ((and (string=? (car rest) "-m") (pair? (cdr rest)))
       (loop (cddr rest)
             (cons (string-append mfem-data-dir "/" (cadr rest))
                   (cons "-m" result))))
      (else (loop (cdr rest) (cons (car rest) result))))))

(define (make-label args xfail?)
  "Generate test label from ARGS: mesh-file + other flags [+ (xfail)]."
  (let loop ((rest args) (mesh #f) (others '()))
    (cond
      ((null? rest)
       (let* ((parts (if mesh (cons mesh (reverse others)) (reverse others)))
              (base (if (null? parts) "(default)" (string-join parts " "))))
         (if xfail?
             (string-append base " (xfail)")
             base)))
      ((and (string=? (car rest) "-m") (pair? (cdr rest)))
       (loop (cddr rest) (cadr rest) others))
      (else (loop (cdr rest) mesh (cons (car rest) others))))))

(define (make-example-runner example-name output-files)
  "Return (values run run/xfail) for EXAMPLE-NAME checking OUTPUT-FILES."
  (let ((script (string-append project-root "/examples/" example-name)))

    (define (run-impl xfail? args)
      (let ((label (make-label args xfail?))
            (expanded (expand-args args)))
        (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
                  output-files)
        (let* ((start (gettimeofday))
               (rc (apply system* "guile" "-L" guile-load-path script expanded))
               (end (gettimeofday))
               (dt (elapsed start end))
               (ok? (and (zero? (status:exit-val rc))
                         (every file-non-empty? output-files))))
          (set! total-time (+ total-time dt))
          (format #t "  ~6a  ~a  (~,2fs)~%"
                  (cond ((and ok? (not xfail?)) "PASS")
                        ((and (not ok?) xfail?) "XFAIL")
                        ((and ok? xfail?) "XPASS")
                        (else "FAIL"))
                  label dt)
          (when xfail? (test-expect-fail 1))
          (test-assert label ok?))))

    (define (run . args) (run-impl #f args))
    (define (run/xfail . args) (run-impl #t args))

    (values run run/xfail)))

(define (test-end/exit test-name)
  "Print total time, end SRFI-64 test suite TEST-NAME, exit with status."
  (format #t "  --- total: ~,2fs ---~%~%" total-time)
  (set! total-time 0.0)
  (let ((runner (test-runner-current)))
    (test-end test-name)
    (exit (zero? (test-runner-fail-count runner)))))
