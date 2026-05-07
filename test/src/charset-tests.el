;;; charset-tests.el --- Tests for charset.c -*- lexical-binding: t -*-

;; Copyright 2017-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)

(ert-deftest charset-decode-char ()
  "Test `decode-char'."
  (should-error (decode-char 'ascii 0.5)))

(ert-deftest charset-tests-define-charset ()
  (eval '(define-charset 'charset-tests-cs-1
           "Only used for testing"
           :short-name  "CTCS1"
           :long-name "Charset-Tests-Charset-1"
           :code-space [33 126 33 126]
           :code-offset #x28083A
           :unify-map "CNS-F"))
  (should (charsetp 'charset-tests-cs-1)))

(provide 'charset-tests)

;;; charset-tests.el ends here
