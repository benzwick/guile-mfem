;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

;; Unit tests for MFEM Device class.

(use-modules (srfi srfi-64)
             (mfem device))
(use-modules (device-primitive))

(test-begin "mfem-device")

;; Device construction with "cpu" backend
(test-group "device-cpu"
  (let ((dev (new-Device "cpu")))
    (test-assert "Device created" dev)))

(define runner (test-runner-current))
(test-end "mfem-device")
(exit (zero? (test-runner-fail-count runner)))
