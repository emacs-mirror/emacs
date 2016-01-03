;;; help-fns.el --- tests for help-fns.el

;; Copyright (C) 2014-2015 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)

(autoload 'help-fns-test--macro "help-fns" nil nil t)

(ert-deftest help-fns-test-bug17410 ()
  "Test for http://debbugs.gnu.org/17410 ."
  (describe-function 'help-fns-test--macro)
  (with-current-buffer "*Help*"
    (goto-char (point-min))
    (should (search-forward "autoloaded Lisp macro" (line-end-position)))))

(defun abc\\\[universal-argument\]b\`c\'d\\e\"f (x)
  "A function with a funny name.

\(fn XYZZY)"
  x)

(defun defgh\\\[universal-argument\]b\`c\'d\\e\"f (x)
  "Another function with a funny name."
  x)

(ert-deftest help-fns-test-funny-names ()
  "Test for help with functions with funny names."
  (describe-function 'abc\\\[universal-argument\]b\`c\'d\\e\"f)
  (with-current-buffer "*Help*"
    (goto-char (point-min))
    (should (search-forward
             "(abc\\\\\\[universal-argument\\]b\\`c\\'d\\\\e\\\"f XYZZY)")))
  (describe-function 'defgh\\\[universal-argument\]b\`c\'d\\e\"f)
  (with-current-buffer "*Help*"
    (goto-char (point-min))
    (should (search-forward
             "(defgh\\\\\\[universal-argument\\]b\\`c\\'d\\\\e\\\"f X)"))))

(ert-deftest help-fns-test-describe-symbol ()
  "Test the `describe-symbol' function."
  ;; 'describe-symbol' would originally signal an error for
  ;; 'font-lock-comment-face'.
  (describe-symbol 'font-lock-comment-face)
  (with-current-buffer "*Help*"
    (should (> (point-max) 1))
    (goto-char (point-min))
    (should (looking-at "^font-lock-comment-face is "))))

;;; help-fns.el ends here
