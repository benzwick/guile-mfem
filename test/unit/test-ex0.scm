(use-modules (srfi srfi-64))

;; MFEM data directory (set by CTest via MFEM_DATA_DIR environment variable)
(define mfem-data-dir (getenv "MFEM_DATA_DIR"))
(define guile-load-path (getenv "LTDL_LIBRARY_PATH"))
(define project-root (dirname (dirname (dirname (current-filename)))))
(define ex0-script (string-append project-root "/examples/ex0.scm"))

(test-begin "mfem-ex0")

(test-group "poisson"
  (let* ((mesh-file (string-append mfem-data-dir "/star.mesh"))
         (rc (system* "guile"
                      "-L" guile-load-path
                      ex0-script
                      "--" "-m" mesh-file)))
    (test-assert "ex0 exits successfully" (zero? (status:exit-val rc)))
    (test-assert "sol.gf exists" (file-exists? "sol.gf"))
    (test-assert "mesh.mesh exists" (file-exists? "mesh.mesh"))
    (test-assert "sol.gf non-empty"
      (> (stat:size (stat "sol.gf")) 100))))

(define runner (test-runner-current))
(test-end "mfem-ex0")
(exit (zero? (test-runner-fail-count runner)))
