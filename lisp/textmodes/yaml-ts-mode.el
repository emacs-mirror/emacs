;;; yaml-ts-mode.el --- tree-sitter support for YAML  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Free Software Foundation, Inc.

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
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

(add-to-list
 'treesit-language-source-alist
 '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
        :commit "b733d3f5f5005890f324333dd57e1f0badec5c87")
 t)

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

(defvar yaml-ts-mode--font-lock-feature-list
  '((comment)
    (string type)
    (constant escape-sequence number property)
    (bracket delimiter error misc-punctuation))
  "Tree-sitter font-lock feature list for `yaml-ts-mode'.")

(defun yaml-ts-mode--fill-paragraph (&optional justify)
  "Fill paragraph.
Behaves like `fill-paragraph', but respects block node
boundaries.  JUSTIFY is passed to `fill-paragraph'."
  (interactive "*P")
  (save-restriction
    (widen)
    (let ((node (treesit-node-at (point))))
      (pcase (treesit-node-type node)
        ("block_scalar"
         (let* ((start (treesit-node-start node))
                (end (treesit-node-end node))
                (start-marker (point-marker))
                (fill-paragraph-function nil))
           (save-excursion
             (goto-char start)
             (forward-line)
             (move-marker start-marker (point))
             (narrow-to-region (point) end))
           (fill-region start-marker end justify)))
        ("comment"
         (fill-comment-paragraph justify))))
    t))

(defun yaml-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (when (equal (treesit-node-type node) "block_mapping_pair")
    (treesit-node-text (treesit-node-child-by-field-name
                        node "key")
                       t)))

(defvar yaml-ts-mode--outline-nodes
  (rx (or "block_mapping_pair" "block_sequence_item"))
  "Node names for outline headings.")

(defun yaml-ts-mode--outline-predicate (node)
  "Limit outlines to top-level mappings."
  (when (string-match-p yaml-ts-mode--outline-nodes (treesit-node-type node))
    (not (treesit-node-top-level node yaml-ts-mode--outline-nodes))))

;;;###autoload
(define-derived-mode yaml-ts-mode text-mode "YAML"
  "Major mode for editing YAML, powered by tree-sitter."
  :group 'yaml
  :syntax-table yaml-ts-mode--syntax-table

  (when (treesit-ensure-installed 'yaml)
    (setq treesit-primary-parser (treesit-parser-create 'yaml))

    ;; Comments.
    (setq-local comment-start "# ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "#+ *")

    ;; Indentation.
    (setq-local indent-tabs-mode nil)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings yaml-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list yaml-ts-mode--font-lock-feature-list)

    (setq-local fill-paragraph-function #'yaml-ts-mode--fill-paragraph)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp "block_mapping_pair")
    (setq-local treesit-defun-name-function #'yaml-ts-mode--defun-name)
    (setq-local treesit-defun-tactic 'top-level)

    (setq-local treesit-thing-settings
                `((yaml
                   (list ,(rx (or "block_mapping_pair" "flow_sequence")))
                   (sentence ,"block_mapping_pair"))))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                '((nil "\\`block_mapping_pair\\'" nil nil)))

    ;; Outline minor mode.
    (setq-local treesit-outline-predicate #'yaml-ts-mode--outline-predicate)

    (treesit-major-mode-setup)

    ;; Use the `list' thing defined above to navigate only lists
    ;; with `C-M-n', `C-M-p', `C-M-u', `C-M-d', but not sexps
    ;; with `C-M-f', `C-M-b' neither adapt to 'show-paren-mode'
    ;; that is problematic in languages without explicit
    ;; opening/closing nodes.
    (kill-local-variable 'forward-sexp-function)
    (kill-local-variable 'show-paren-data-function)))

(derived-mode-add-parents 'yaml-ts-mode '(yaml-mode))

;;;###autoload
(defun yaml-ts-mode-maybe ()
  "Enable `yaml-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'yaml)
          (eq treesit-enabled-modes t)
          (memq 'yaml-ts-mode treesit-enabled-modes))
      (yaml-ts-mode)
    (fundamental-mode)))

;;;###autoload
(when (treesit-available-p)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode-maybe))
  ;; To be able to toggle between an external package and core ts-mode:
  (defvar treesit-major-mode-remap-alist)
  (add-to-list 'treesit-major-mode-remap-alist
               '(yaml-mode . yaml-ts-mode)))

(provide 'yaml-ts-mode)

;;; yaml-ts-mode.el ends here
