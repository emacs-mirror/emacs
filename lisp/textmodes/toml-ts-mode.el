;;; toml-ts-mode.el --- tree-sitter support for TOML  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author     : Jostein Kjønigsen <jostein@kjonigsen.net>
;; Maintainer : Jostein Kjønigsen <jostein@kjonigsen.net>
;; Created    : December 2022
;; Keywords   : toml languages tree-sitter

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

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

(defcustom toml-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `toml-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'toml)

(defvar toml-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    (modify-syntax-entry ?#  "<"   table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    table)
  "Syntax table for `toml-ts-mode'.")

(defvar toml-ts-mode--indent-rules
  `((toml
     ((node-is "]") parent-bol 0)
     ((parent-is "string") parent-bol toml-ts-mode-indent-offset)
     ((parent-is "array") parent-bol toml-ts-mode-indent-offset))))

(defvar toml-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'toml
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'toml
   :feature 'constant
   '((boolean) @font-lock-constant-face)

   :language 'toml
   :feature 'delimiter
   '((["="]) @font-lock-delimiter-face)

   :language 'toml
   :feature 'number
   '([(integer) (float) (local_date) (local_date_time) (local_time)]
     @font-lock-number-face)

   :language 'toml
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'toml
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'toml
   :feature 'pair
   :override t            ; Needed for overriding string face on keys.
   '((bare_key) @font-lock-property-use-face
     (quoted_key) @font-lock-property-use-face
     (table ("[" @font-lock-bracket-face
             (_) @font-lock-type-face
             "]" @font-lock-bracket-face))
     (table_array_element ("[[" @font-lock-bracket-face
                           (_) @font-lock-type-face
                           "]]" @font-lock-bracket-face))
     (table (quoted_key) @font-lock-type-face)
     (table (dotted_key (quoted_key)) @font-lock-type-face))

   :language 'toml
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Font-lock settings for TOML.")

(defun toml-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "table" "table_array_element")
     (or (treesit-node-text (treesit-node-child node 1) t)
         "Root table"))))

;;;###autoload
(define-derived-mode toml-ts-mode text-mode "TOML"
  "Major mode for editing TOML, powered by tree-sitter."
  :group 'toml-mode
  :syntax-table toml-ts-mode--syntax-table

  (when (treesit-ready-p 'toml)
    (treesit-parser-create 'toml)

    ;; Comments
    (setq-local comment-start "# ")
    (setq-local comment-end "")

    ;; Indent.
    (setq-local treesit-simple-indent-rules toml-ts-mode--indent-rules)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (rx (or "table" "table_array_element")))
    (setq-local treesit-defun-name-function #'toml-ts-mode--defun-name)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings toml-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (constant number pair string)
                  (escape-sequence)
                  (delimiter error)))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                '(("Header" "\\`table\\'" nil nil)
                  ("Array" "\\`table_array_element\\'" nil nil)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'toml)
    (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode)))

(provide 'toml-ts-mode)

;;; toml-ts-mode.el ends here
