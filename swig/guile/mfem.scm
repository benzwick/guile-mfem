;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;;; (mfem) — umbrella module re-exporting all MFEM bindings.
;;;
;;; Usage:
;;;   (use-modules (oop goops) (mfem))
;;;
;;; Individual modules can be cherry-picked for faster load times:
;;;   (use-modules (oop goops) (mfem vector) (mfem mesh))

(define-module (mfem)
  #:duplicates (merge-generics replace warn-override-core warn last))

(use-modules (mfem mem_manager)
             (mfem globals)
             (mfem array)
             (mfem vector)
             (mfem operators)
             (mfem matrix)
             (mfem densemat)
             (mfem sparsemat)
             (mfem intrules)
             (mfem eltrans)
             (mfem fe)
             (mfem symmat)
             (mfem integrator)
             (mfem nonlininteg)
             (mfem coefficient)
             (mfem mesh)
             (mfem fe_coll)
             (mfem fespace)
             (mfem gridfunc)
             (mfem lininteg)
             (mfem bilininteg)
             (mfem linearform)
             (mfem handle)
             (mfem bilinearform)
             (mfem solvers)
             (mfem sparsesmoothers)
             (mfem device))

;; Re-export all public bindings from the imported modules.
;; Look up each name in the current module (where merge-generics has
;; combined generic functions from all imports) rather than copying
;; variables directly from individual module interfaces.
(let ((public-i (module-public-interface (current-module)))
      (this-mod (current-module)))
  (for-each
    (lambda (mod)
      (module-for-each
        (lambda (name _var)
          (let ((merged-var (module-variable this-mod name)))
            (when merged-var
              (module-add! public-i name merged-var))))
        (resolve-interface mod)))
    '((mfem mem_manager)
      (mfem globals)
      (mfem array)
      (mfem vector)
      (mfem operators)
      (mfem matrix)
      (mfem densemat)
      (mfem sparsemat)
      (mfem intrules)
      (mfem eltrans)
      (mfem fe)
      (mfem symmat)
      (mfem integrator)
      (mfem nonlininteg)
      (mfem coefficient)
      (mfem mesh)
      (mfem fe_coll)
      (mfem fespace)
      (mfem gridfunc)
      (mfem lininteg)
      (mfem bilininteg)
      (mfem linearform)
      (mfem handle)
      (mfem bilinearform)
      (mfem solvers)
      (mfem sparsesmoothers)
      (mfem device))))
