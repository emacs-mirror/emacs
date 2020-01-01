;;; asm-mode-tests.el --- Tests for asm-mode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

(require 'asm-mode)
(require 'ert)

(defmacro asm-mode-tests--with-temp-buffer (&rest body)
  "Create a temporary Asm mode buffer and evaluate BODY there."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((asm-comment-char ?\;))
       (asm-mode)
       ,@body)))

(ert-deftest asm-mode-tests-colon ()
  (asm-mode-tests--with-temp-buffer
    (let ((indent-tabs-mode t))
      (insert "\t  label")
      (let ((last-command-event ?:))
        (asm-colon))
      (should (equal (buffer-string) "label:\t")))))

(ert-deftest asm-mode-tests-colon-inside-comment ()
  (asm-mode-tests--with-temp-buffer
    (insert ";comment")
    (let ((last-command-event ?:))
      (asm-colon))
    (should (equal (buffer-string) ";comment:"))))

(ert-deftest asm-mode-tests-comment ()
  (asm-mode-tests--with-temp-buffer
    (insert "label:")
    (goto-char (point-min))
    ;; First invocation
    (asm-comment)
    (should (string-match-p "label:[ \t]+;" (buffer-string)))
    (should (= (current-column) (+ comment-column 1)))
    ;; Second invocation
    (asm-comment)
    (should (string-match-p "[ \t]+;; \nlabel:" (buffer-string)))
    (should (= (current-column) (+ tab-width 3)))
    ;; Third invocation
    (asm-comment)
    (should (string-match-p ";;; \nlabel:" (buffer-string)))
    (should (= (current-column) 4))))

;;; asm-mode-tests.el ends here
