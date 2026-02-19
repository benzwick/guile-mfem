;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM periodic mesh (from PyMFEM test_periodic_mesh.py).
;; Requires MakePeriodic and CreatePeriodicVertexMapping methods.

;; Guard: skip if MakePeriodic is not available
;; (Static methods MakeCartesian1D and MakePeriodic are not yet wrapped
;; in the GOOPS proxy — see BUGS.md)
(unless (false-if-exception
          (begin
            (resolve-interface '(mfem mesh))
            (module-ref (resolve-interface '(mfem mesh)) 'MakePeriodic)))
  (format (current-error-port) "SKIP: MakePeriodic not available~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (oop goops)
             (mfem mesh) (mfem vector))

(test-begin "mfem-periodic-mesh")

;; 1D periodic mesh
(test-group "1d-periodic"
  (let* ((n 3)
         (orig (MakeCartesian1D n))
         (trans (make <Vector> 1)))
    (set trans 0 1.0)
    ;; CreatePeriodicVertexMapping + MakePeriodic
    (let* ((mapping (CreatePeriodicVertexMapping orig trans))
           (mesh (MakePeriodic orig mapping)))
      (test-equal "1D periodic NV" n (GetNV mesh)))))

(define runner (test-runner-current))
(test-end "mfem-periodic-mesh")
(exit (zero? (test-runner-fail-count runner)))
