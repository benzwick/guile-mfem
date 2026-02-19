;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

(define-module (test unit-harness)
  #:use-module (ice-9 format)
  #:export (skip-unless))

(define (skip-unless module-name)
  "Exit 77 (CTest SKIP) unless MODULE-NAME (a symbol list) is available."
  (unless (false-if-exception (resolve-interface module-name))
    (format (current-error-port) "SKIP: module ~a not available~%" module-name)
    (exit 77)))
