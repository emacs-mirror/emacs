;;; faceup-test-mode.el --- Dummy major mode for testing `faceup'.

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; Author: Anders Lindgren
;; Keywords: languages, faces

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

;; Dummy major-mode for testing `faceup', a regression test system for
;; font-lock keywords (syntax highlighting rules for Emacs).
;;
;; This mode use `syntax-propertize' to set the `syntax-table'
;; property on "<" and ">" in "<TEXT>" to make them act like
;; parentheses.
;;
;; This mode also sets the `help-echo' property on the text WARNING,
;; the effect is that Emacs displays a tooltip when you move your
;; mouse on to the text.

;;; Code:

(defvar faceup-test-mode-syntax-table
  (make-syntax-table)
  "Syntax table for `faceup-test-mode'.")

(defvar faceup-test-font-lock-keywords
  '(("\\_<WARNING\\_>"
     (0 (progn
          (add-text-properties (match-beginning 0)
                               (match-end 0)
                               '(help-echo "Baloon tip: Fly smoothly!"))
          font-lock-warning-face))))
  "Highlight rules for `faceup-test-mode'.")

(defun faceup-test-syntax-propertize (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("\\(<\\)\\([^<>\n]*\\)\\(>\\)"
     (1 "()  ")
     (3 ")(  ")))
   start end))

(defmacro faceup-test-define-prog-mode (mode name &rest args)
  "Define a major mode for a programming language.
If `prog-mode' is defined, inherit from it."
  (declare (indent defun))
  `(define-derived-mode
     ,mode ,(and (fboundp 'prog-mode) 'prog-mode)
     ,name ,@args))

(faceup-test-define-prog-mode faceup-test-mode "faceup-test"
  "Dummy major mode for testing `faceup', a test system for font-lock."
  (set (make-local-variable 'syntax-propertize-function)
       #'faceup-test-syntax-propertize)
  (setq font-lock-defaults '(faceup-test-font-lock-keywords nil)))

(provide 'faceup-test-mode)

;;; faceup-test-mode.el ends here
