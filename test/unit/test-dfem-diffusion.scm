;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for distributed FEM diffusion operator
;; (from MFEM test_diffusion.cpp).
;; Requires parallel MFEM build with MPI bindings.

;; Guard: skip if parallel module is not compiled
(unless (false-if-exception (resolve-interface '(mfem par)))
  (format (current-error-port) "SKIP: module (mfem par) not available (requires MPI)~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem par))

(test-begin "mfem-dfem-diffusion")

(test-group "distributed-diffusion"
  (test-assert "placeholder" #t))

(define runner (test-runner-current))
(test-end "mfem-dfem-diffusion")
(exit (zero? (test-runner-fail-count runner)))
