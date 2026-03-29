;;; doc-tests.el --- tests for doc.c functions  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

(ert-deftest doc-tests-documentation/c-primitive ()
  (should (stringp (documentation 'defalias))))

(ert-deftest doc-tests-documentation/preloaded ()
  (should (stringp (documentation 'defun))))

(ert-deftest doc-tests-documentation/autoloaded-macro ()
  (skip-unless noninteractive)
  (should (autoloadp (symbol-function 'benchmark-run)))
  (should (stringp (documentation 'benchmark-run))))     ; See Bug#52969.

(ert-deftest doc-tests-documentation/autoloaded-defun ()
  (skip-unless noninteractive)
  (should (autoloadp (symbol-function 'tetris)))
  (should (stringp (documentation 'tetris)))) ; See Bug#52969.

(ert-deftest doc-tests-quoting-style ()
  (should (memq (text-quoting-style) '(grave straight curve))))

;;; doc-tests.el ends here
