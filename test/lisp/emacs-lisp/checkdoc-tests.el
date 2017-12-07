;;; checkdoc-tests.el --- unit tests for checkdoc.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for lisp/emacs-lisp/checkdoc.el.

;;; Code:

(require 'checkdoc)

(require 'elisp-mode)
(require 'ert)

(ert-deftest checkdoc-tests--bug-24998 ()
  "Checks that Bug#24998 is fixed."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo())")
    (should-error (checkdoc-defun) :type 'user-error)))

(ert-deftest checkdoc-tests--next-docstring ()
  "Checks that the one-argument form of `defvar' works.
See the comments in Bug#24998."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defvar foo)
\(defvar foo bar \"baz\")
\(require 'foo)")
    (goto-char (point-min))
    (should (checkdoc-next-docstring))
    (should (looking-at-p "\"baz\")"))
    (should-not (checkdoc-next-docstring))))

;;; checkdoc-tests.el ends here
