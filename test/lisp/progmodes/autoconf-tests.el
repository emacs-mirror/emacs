;;; autoconf-tests.el --- Tests for autoconf.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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

;;; Commentary:

;;

;;; Code:

(require 'autoconf)
(require 'ert)

(ert-deftest autoconf-tests-current-defun-function-define ()
  (with-temp-buffer
    (insert "AC_DEFINE(HAVE_RSVG, 1, [Define to 1 if using librsvg.])")
    (goto-char (point-min))
    (should-not (autoconf-current-defun-function))
    (forward-char 10)
    (should (equal (autoconf-current-defun-function) "HAVE_RSVG"))))

(ert-deftest autoconf-tests-current-defun-function-subst ()
  (with-temp-buffer
    (insert "AC_SUBST(srcdir)")
    (goto-char (point-min))
    (should-not (autoconf-current-defun-function))
    (forward-char 9)
    (should (equal (autoconf-current-defun-function) "srcdir"))))

(ert-deftest autoconf-tests-autoconf-mode-comment-syntax ()
  (with-temp-buffer
    (autoconf-mode)
    (insert "dnl  Autoconf script for GNU Emacs")
    (should (nth 4 (syntax-ppss)))))

(provide 'autoconf-tests)
;;; autoconf-tests.el ends here
