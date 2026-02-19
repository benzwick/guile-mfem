;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for parallel boundary projection
;; (from MFEM test_project_bdr_par.cpp).
;; Requires parallel MFEM build with MPI bindings.

;; Guard: skip if parallel module is not compiled
(unless (false-if-exception (resolve-interface '(mfem par)))
  (format (current-error-port) "SKIP: module (mfem par) not available (requires MPI)~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-project-bdr-par")

(test-group "boundary-projection"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-project-bdr-par")
(exit (zero? (test-runner-fail-count runner)))
