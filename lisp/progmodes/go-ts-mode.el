;;; go-ts-mode.el --- tree-sitter support for Go  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author     : Randy Taylor <dev@rjt.dev>
;; Maintainer : Randy Taylor <dev@rjt.dev>
;; Created    : December 2022
;; Keywords   : go languages tree-sitter

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
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defcustom go-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `go-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'go)

(defvar go-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)
    (modify-syntax-entry ?^   "."      table)
    (modify-syntax-entry ?!   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?\\  "\\"     table)
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    (modify-syntax-entry ?\n  "> b"    table)
    table)
  "Syntax table for `go-ts-mode'.")

(defvar go-ts-mode--indent-rules
  `((go
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is "labeled_statement") no-indent)
     ((parent-is "argument_list") parent-bol go-ts-mode-indent-offset)
     ((parent-is "block") parent-bol go-ts-mode-indent-offset)
     ((parent-is "const_declaration") parent-bol go-ts-mode-indent-offset)
     ((parent-is "default_case") parent-bol go-ts-mode-indent-offset)
     ((parent-is "expression_case") parent-bol go-ts-mode-indent-offset)
     ((parent-is "expression_switch_statement") parent-bol 0)
     ((parent-is "field_declaration_list") parent-bol go-ts-mode-indent-offset)
     ((parent-is "import_spec_list") parent-bol go-ts-mode-indent-offset)
     ((parent-is "labeled_statement") parent-bol go-ts-mode-indent-offset)
     ((parent-is "literal_value") parent-bol go-ts-mode-indent-offset)
     ((parent-is "type_spec") parent-bol go-ts-mode-indent-offset)
     ((parent-is "var_declaration") parent-bol go-ts-mode-indent-offset)
     (no-node parent-bol 0)))
  "Tree-sitter indent rules for `go-ts-mode'.")

(defvar go-ts-mode--keywords
  '("break" "case" "chan" "const" "continue" "default" "defer" "else"
    "fallthrough" "for" "func" "go" "goto" "if" "import" "interface" "map"
    "package" "range" "return" "select" "struct" "switch" "type" "var")
  "Go keywords for tree-sitter font-locking.")

(defvar go-ts-mode--operators
  '("+" "&" "+=" "&=" "&&" "==" "!=" "-" "|" "-=" "|=" "||" "<" "<="
    "*" "^" "*=" "^=" "<-" ">" ">=" "/" "<<" "/=" "<<=" "++" "=" ":=" "%"
    ">>" "%=" ">>=" "--" "!"  "..."  "&^" "&^=" "~")
  "Go operators for tree-sitter font-locking.")

(defvar go-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'go
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'go
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'go
   :feature 'constant
   '([(false) (iota) (nil) (true)] @font-lock-constant-face
     (const_declaration
      (const_spec name: (identifier) @font-lock-constant-face)))

   :language 'go
   :feature 'delimiter
   '((["," "." ";" ":"]) @font-lock-delimiter-face)

   :language 'go
   :feature 'function
   '((call_expression
      function: (identifier) @font-lock-function-name-face)
     (call_expression
      function: (selector_expression
                 field: (field_identifier) @font-lock-function-name-face))
     (function_declaration
      name: (identifier) @font-lock-function-name-face)
     (method_declaration
      name: (field_identifier) @font-lock-function-name-face))

   :language 'go
   :feature 'keyword
   `([,@go-ts-mode--keywords] @font-lock-keyword-face)

   :language 'go
   :feature 'label
   '((label_name) @font-lock-constant-face)

   :language 'go
   :feature 'number
   '([(float_literal)
      (imaginary_literal)
      (int_literal)] @font-lock-number-face)

   :language 'go
   :feature 'string
   '([(interpreted_string_literal)
      (raw_string_literal)
      (rune_literal)] @font-lock-string-face)

   :language 'go
   :feature 'type
   '([(package_identifier) (type_identifier)] @font-lock-type-face)

   :language 'go
   :feature 'variable
   '((identifier) @font-lock-variable-name-face)

   :language 'go
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'go
   :feature 'property
   :override t
   '((field_identifier) @font-lock-property-face
     (keyed_element (_ (identifier) @font-lock-property-face)))

   :language 'go
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `go-ts-mode'.")

(defun go-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (func-tree (treesit-induce-sparse-tree
                     node "function_declaration" nil 1000))
         (type-tree (treesit-induce-sparse-tree
                     node "type_spec" nil 1000))
         (func-index (go-ts-mode--imenu-1 func-tree))
         (type-index (go-ts-mode--imenu-1 type-tree)))
    (append
     (when func-index `(("Function" . ,func-index)))
     (when type-index `(("Type" . ,type-index))))))

(defun go-ts-mode--imenu-1 (node)
  "Helper for `go-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'go-ts-mode--imenu-1
                           children))
         (name (when ts-node
                 (treesit-node-text
                  (pcase (treesit-node-type ts-node)
                    ("function_declaration"
                     (treesit-node-child-by-field-name ts-node "name"))
                    ("type_spec"
                     (treesit-node-child-by-field-name ts-node "name"))))))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((or (null ts-node) (null name)) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

;;;###autoload
(define-derived-mode go-ts-mode prog-mode "Go"
  "Major mode for editing Go, powered by tree-sitter."
  :group 'go
  :syntax-table go-ts-mode--syntax-table

  (when (treesit-ready-p 'go)
    (treesit-parser-create 'go)

    ;; Comments.
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))

    ;; Imenu.
    (setq-local imenu-create-index-function #'go-ts-mode--imenu)
    (setq-local which-func-functions nil)

    ;; Indent.
    (setq-local indent-tabs-mode t
                treesit-simple-indent-rules go-ts-mode--indent-rules)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings go-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment)
                  ( keyword string type)
                  ( constant escape-sequence function label number
                    property variable)
                  ( bracket delimiter error operator)))

    (treesit-major-mode-setup)))

;; go.mod support.

(defvar go-mod-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?\n  "> b"    table)
    table)
  "Syntax table for `go-mod-ts-mode'.")

(defvar go-mod-ts-mode--indent-rules
  `((gomod
     ((node-is ")") parent-bol 0)
     ((parent-is "exclude_directive") parent-bol go-ts-mode-indent-offset)
     ((parent-is "module_directive") parent-bol go-ts-mode-indent-offset)
     ((parent-is "replace_directive") parent-bol go-ts-mode-indent-offset)
     ((parent-is "require_directive") parent-bol go-ts-mode-indent-offset)
     ((parent-is "retract_directive") parent-bol go-ts-mode-indent-offset)
     ((go-mod-ts-mode--in-directive-p) no-indent go-ts-mode-indent-offset)
     (no-node no-indent 0)))
  "Tree-sitter indent rules for `go-mod-ts-mode'.")

(defun go-mod-ts-mode--in-directive-p ()
  "Return non-nil if inside a directive.
When entering an empty directive or adding a new entry to one, no node
will be present meaning none of the indentation rules will match,
because there is no parent to match against.  This function determines
what the parent of the node would be if it were a node."
  (lambda (node _ _ &rest _)
    (unless (treesit-node-type node)
      (save-excursion
        (backward-up-list)
        (back-to-indentation)
        (pcase (treesit-node-type (treesit-node-at (point)))
          ("exclude" t)
          ("module" t)
          ("replace" t)
          ("require" t)
          ("retract" t))))))

(defvar go-mod-ts-mode--keywords
  '("exclude" "go" "module" "replace" "require" "retract")
  "go.mod keywords for tree-sitter font-locking.")

(defvar go-mod-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'gomod
   :feature 'bracket
   '((["(" ")"]) @font-lock-bracket-face)

   :language 'gomod
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'gomod
   :feature 'keyword
   `([,@go-mod-ts-mode--keywords] @font-lock-keyword-face)

   :language 'gomod
   :feature 'number
   '([(go_version) (version)] @font-lock-number-face)

   :language 'gomod
   :feature 'operator
   '((["=>"]) @font-lock-operator-face)

   :language 'gomod
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `go-mod-ts-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))

;;;###autoload
(define-derived-mode go-mod-ts-mode prog-mode "Go Mod"
  "Major mode for editing go.mod files, powered by tree-sitter."
  :group 'go
  :syntax-table go-mod-ts-mode--syntax-table

  (when (treesit-ready-p 'gomod)
    (treesit-parser-create 'gomod)

    ;; Comments.
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))

    ;; Indent.
    (setq-local indent-tabs-mode t
                treesit-simple-indent-rules go-mod-ts-mode--indent-rules)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings go-mod-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword)
                  (number)
                  (bracket error operator)))

    (treesit-major-mode-setup)))

(provide 'go-ts-mode)

;;; go-ts-mode.el ends here
