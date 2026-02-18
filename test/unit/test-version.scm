;; SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
;; SPDX-License-Identifier: BSD-3-Clause

(use-modules (srfi srfi-64)
             (mfem version))

(test-begin "mfem-version")

(test-assert "GetVersionStr returns a string"
  (string? (GetVersionStr)))

(test-assert "GetVersionStr is not empty"
  (not (string-null? (GetVersionStr))))

(test-assert "GetVersion returns a positive integer"
  (> (GetVersion) 0))

(test-assert "GetVersionMajor returns a non-negative integer"
  (>= (GetVersionMajor) 0))

(test-assert "GetVersionMinor returns a non-negative integer"
  (>= (GetVersionMinor) 0))

(test-assert "GetVersionPatch returns a non-negative integer"
  (>= (GetVersionPatch) 0))

(define runner (test-runner-current))
(test-end "mfem-version")
(exit (zero? (test-runner-fail-count runner)))