;;; autoconf-tests.el --- Tests for autoconf.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

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

(require 'autoconf)
(require 'ert)
(require 'ert-font-lock)

(ert-deftest autoconf-tests-current-defun-function-define ()
  (with-temp-buffer
    (autoconf-mode)
    (insert "AC_DEFINE([HAVE_RSVG], [1], [Define to 1 if using librsvg.])")
    (let ((def "HAVE_RSVG"))
      (search-backward def)
      (should (equal (autoconf-current-defun-function) def)))
    (goto-char (point-min))
    (should-not (autoconf-current-defun-function))))

(ert-deftest autoconf-tests-current-defun-function-subst ()
  (with-temp-buffer
    (autoconf-mode)
    (insert "AC_SUBST([srcdir])")
    (let ((def "srcdir"))
      (search-backward def)
      (should (equal (autoconf-current-defun-function) "srcdir")))
    (goto-char (point-min))
    (should-not (autoconf-current-defun-function))))

(ert-deftest autoconf-tests-autoconf-mode-comment-syntax ()
  (with-temp-buffer
    (autoconf-mode)
    (dolist (start '("dnl" "#"))
      (insert start "  Autoconf script for GNU Emacs")
      (should (eq (syntax-ppss-context (syntax-ppss)) 'comment))
      (insert "\n")
      (should-not (syntax-ppss-context (syntax-ppss))))))

(ert-font-lock-deftest-file autoconf-tests-font-lock
  "Test `autoconf-mode' font lock."
  autoconf-mode "configure.ac")

(provide 'autoconf-tests)
;;; autoconf-tests.el ends here
