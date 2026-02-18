;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

(use-modules (srfi srfi-64))

;; MFEM data directory (set by CTest via MFEM_DATA_DIR environment variable)
(define mfem-data-dir (getenv "MFEM_DATA_DIR"))
(define guile-load-path (getenv "LTDL_LIBRARY_PATH"))
(define project-root (dirname (dirname (dirname (current-filename)))))
(define ex1-script (string-append project-root "/examples/ex1.scm"))

(test-begin "mfem-ex1")

(test-group "poisson"
  (let* ((mesh-file (string-append mfem-data-dir "/star.mesh"))
         (rc (system* "guile"
                      "-L" guile-load-path
                      ex1-script
                      "--" "-m" mesh-file)))
    (test-assert "ex1 exits successfully" (zero? (status:exit-val rc)))
    (test-assert "sol.gf exists" (file-exists? "sol.gf"))
    (test-assert "refined.mesh exists" (file-exists? "refined.mesh"))
    (test-assert "sol.gf non-empty"
      (> (stat:size (stat "sol.gf")) 100))))

(define runner (test-runner-current))
(test-end "mfem-ex1")
(exit (zero? (test-runner-fail-count runner)))
