;;; yaml-ts-mode.el --- tree-sitter support for YAML  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author     : Randy Taylor <dev@rjt.dev>
;; Maintainer : Randy Taylor <dev@rjt.dev>
;; Created    : December 2022
;; Keywords   : yaml languages tree-sitter

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

(defvar yaml-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?\( "."  table)
    (modify-syntax-entry ?\) "."  table)
    (modify-syntax-entry ?\' "\"" table)
    table)
  "Syntax table for `yaml-ts-mode'.")

(defvar yaml-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'yaml
   :feature 'bracket
   '((["[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'yaml
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'yaml
   :feature 'constant
   '([(boolean_scalar)
      (null_scalar)
      (reserved_directive)
      (tag_directive)
      (yaml_directive)] @font-lock-constant-face)

   :language 'yaml
   :feature 'delimiter
   '((["," ":" "-" ">" "?" "|"]) @font-lock-delimiter-face)

   :language 'yaml
   :feature 'misc-punctuation
   '((["---" "..." "&" "*"]) @font-lock-misc-punctuation-face)

   :language 'yaml
   :feature 'number
   '([(float_scalar) (integer_scalar)] @font-lock-number-face)

   :language 'yaml
   :feature 'type
   '([(alias_name) (anchor_name) (tag)] @font-lock-type-face)

   :language 'yaml
   :feature 'string
   :override t
   '([(block_scalar)
      (double_quote_scalar)
      (single_quote_scalar)
      (string_scalar)] @font-lock-string-face)

   :language 'yaml
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'yaml
   :feature 'property
   :override t
   '((block_mapping_pair
      key: (flow_node (plain_scalar (string_scalar) @font-lock-property-use-face)))
     (block_mapping_pair
      key: (flow_node
            [(double_quote_scalar) (single_quote_scalar)] @font-lock-property-use-face))
     (flow_mapping
      (_ key: (flow_node (plain_scalar (string_scalar) @font-lock-property-use-face))))
     (flow_mapping
      (_ key:
         (flow_node
          [(double_quote_scalar) (single_quote_scalar)] @font-lock-property-use-face)))
     (flow_sequence
      (_ key: (flow_node (plain_scalar (string_scalar) @font-lock-property-use-face))))
     (flow_sequence
      (_ key:
         (flow_node
          [(double_quote_scalar) (single_quote_scalar)] @font-lock-property-use-face))))

   :language 'yaml
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `yaml-ts-mode'.")

;;;###autoload
(define-derived-mode yaml-ts-mode text-mode "YAML"
  "Major mode for editing YAML, powered by tree-sitter."
  :group 'yaml
  :syntax-table yaml-ts-mode--syntax-table

  (when (treesit-ready-p 'yaml)
    (treesit-parser-create 'yaml)

    ;; Comments.
    (setq-local comment-start "# ")
    (setq-local comment-end "")

    ;; Indentation.
    (setq-local indent-tabs-mode nil)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings yaml-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (string type)
                  (constant escape-sequence number property)
                  (bracket delimiter error misc-punctuation)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'yaml)
    (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))

(provide 'yaml-ts-mode)

;;; yaml-ts-mode.el ends here
