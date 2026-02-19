;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM periodic mesh (from PyMFEM test_periodic_mesh.py).
;; Requires MakePeriodic and CreatePeriodicVertexMapping methods.

;; Guard: skip if MakePeriodic is not available
;; (It may be in mesh module but the static method might not be wrapped)
(unless (false-if-exception
          (begin
            (resolve-interface '(mfem mesh))
            ;; Check if MakePeriodic is available as a procedure
            (module-ref (resolve-interface '(mfem mesh)) 'Mesh-MakePeriodic)))
  (format (current-error-port) "SKIP: Mesh-MakePeriodic not available~%")
  (exit 77))

(use-modules (srfi srfi-64)
             (mfem mesh) (mfem vector))
(use-modules (mesh-primitive))

(test-begin "mfem-periodic-mesh")

;; 1D periodic mesh
(test-group "1d-periodic"
  (let* ((n 3)
         (orig (Mesh-MakeCartesian1D n))
         (trans (new-Vector 1)))
    (Vector-set trans 0 1.0)
    ;; CreatePeriodicVertexMapping + MakePeriodic
    (let* ((mapping (Mesh-CreatePeriodicVertexMapping orig trans))
           (mesh (Mesh-MakePeriodic orig mapping)))
      (test-equal "1D periodic NV" n (Mesh-GetNV mesh)))))

(define runner (test-runner-current))
(test-end "mfem-periodic-mesh")
(exit (zero? (test-runner-fail-count runner)))
