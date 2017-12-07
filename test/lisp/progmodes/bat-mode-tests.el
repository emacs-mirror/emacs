;;; bat-mode-tests.el --- Tests for bat-mode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Vladimir Panteleev <vladimir@thecybershadow.net>
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

(require 'ert)
(require 'bat-mode)
(require 'htmlfontify)

(defun bat-test-fontify (str)
  "Fontify STR in `bat-mode' to a HTML string using `htmlfontify' and return it."
  (with-temp-buffer
    (insert str)
    (bat-mode)
    (let ((hfy-optimizations '(body-text-only merge-adjacent-tags)))
      (with-current-buffer (htmlfontify-buffer) (buffer-string)))))

(ert-deftest bat-test-fontification-var-decl ()
  "Test fontification of variable declarations."
  (should
   (equal
    (bat-test-fontify "set a_b-c{d}e=f")
    "<span class=\"builtin\">set</span> <span class=\"variable-name\">a_b-c{d}e</span>=f")))

(ert-deftest bat-test-fontification-var-exp ()
  "Test fontification of variable expansions."
  (should
   (equal
    (bat-test-fontify "echo %a_b-c{d}e%")
    "<span class=\"builtin\">echo</span> %<span class=\"variable-name\">a_b-c{d}e</span>%")))

(ert-deftest bat-test-fontification-var-delayed-exp ()
  "Test fontification of delayed variable expansions."
  (should
   (equal
    (bat-test-fontify "echo !a_b-c{d}e!")
    "<span class=\"builtin\">echo</span> !<span class=\"variable-name\">a_b-c{d}e</span>!")))

(ert-deftest bat-test-fontification-iter-var-1 ()
  "Test fontification of iteration variables."
  (should
   (equal
    (bat-test-fontify "echo %%a\necho %%~dp1\necho %%~$PATH:I")
    "<span class=\"builtin\">echo</span> %%<span class=\"variable-name\">a</span>
<span class=\"builtin\">echo</span> %%~dp<span class=\"variable-name\">1</span>
<span class=\"builtin\">echo</span> %%~$<span class=\"variable-name\">PATH</span>:<span class=\"variable-name\">I</span>")))

(defun bat-test-fill-paragraph (str)
  "Return the result of invoking `fill-paragraph' on STR in a `bat-mode' buffer."
  (with-temp-buffer
    (bat-mode)
    (insert str)
    (goto-char 1)
    (font-lock-ensure)
    (fill-paragraph)
    (buffer-string)))

(ert-deftest bat-test-fill-paragraph-comment ()
  "Test `fill-paragraph' in a comment block."
  (should (equal (bat-test-fill-paragraph "rem foo\nrem bar\n") "rem foo bar\n")))

(provide 'bat-tests)
;;; bat-mode-tests.el ends here
