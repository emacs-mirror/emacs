;;; checkdoc-tests.el --- unit tests for checkdoc.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

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

(ert-deftest checkdoc-cl-defmethod-ok ()
  "Checkdoc should be happy with a simple correct cl-defmethod."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defmethod foo (a) \"Return A.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defmethod-with-types-ok ()
  "Checkdoc should be happy with a cl-defmethod using types."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; this method matches if A is the symbol `smthg' and if b is a list:
    (insert "(cl-defmethod foo ((a (eql smthg)) (b list)) \"Return A+B.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-key-ok ()
  "Checkdoc should be happy with a cl-defun using &key."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defun foo (&key a (b 27)) \"Return :A+:B.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-allow-other-keys-ok ()
  "Checkdoc should be happy with a cl-defun using &allow-other-keys."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defun foo (&key a &allow-other-keys) \"Return :A.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-default-optional-value-ok ()
  "Checkdoc should be happy with a cl-defun using default values for optional args."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; B is optional and equals 1+a if not provided. HAS-BS is non-nil
    ;; if B was provided in the call:
    (insert "(cl-defun foo (a &optional (b (1+ a) has-bs)) \"Return A + B.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-destructuring-ok ()
  "Checkdoc should be happy with a cl-defun destructuring its arguments."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defun foo ((a b &optional c) d) \"Return A+B+C+D.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defmethod-ok ()
  "Checkdoc should be happy with a simple correct cl-defmethod."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defmethod foo (a) \"Return A.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defmethod-with-types-ok ()
  "Checkdoc should be happy with a cl-defmethod using types."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; this method matches if A is the symbol `smthg' and if b is a list:
    (insert "(cl-defmethod foo ((a (eql smthg)) (b list)) \"Return A+B.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-key-ok ()
  "Checkdoc should be happy with a cl-defun using &key."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defun foo (&key a (b 27)) \"Return :A+:B.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-allow-other-keys-ok ()
  "Checkdoc should be happy with a cl-defun using &allow-other-keys."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defun foo (&key a &allow-other-keys) \"Return :A.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-default-optional-value-ok ()
  "Checkdoc should be happy with a cl-defun using default values for optional args."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; B is optional and equals 1+a if not provided. HAS-BS is non-nil
    ;; if B was provided in the call:
    (insert "(cl-defun foo (a &optional (b (1+ a) has-bs)) \"Return A + B.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-destructuring-ok ()
  "Checkdoc should be happy with a cl-defun destructuring its arguments."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defun foo ((a b &optional c) d) \"Return A+B+C+D.\")")
    (checkdoc-defun)))

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
