;;; go-ts-mode.el --- tree-sitter support for Go  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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
(declare-function treesit-search-subtree "treesit.c")

(defcustom go-ts-mode-indent-offset 8
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
    (modify-syntax-entry ?\'  "\""     table)
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    (modify-syntax-entry ?\n  "> b"    table)
    table)
  "Syntax table for `go-ts-mode'.")

(defvar go-ts-mode--indent-rules
  `((go
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is "labeled_statement") no-indent 0)
     ((parent-is "raw_string_literal") no-indent 0)
     ((parent-is "argument_list") parent-bol go-ts-mode-indent-offset)
     ((parent-is "block") parent-bol go-ts-mode-indent-offset)
     ((parent-is "communication_case") parent-bol go-ts-mode-indent-offset)
     ((parent-is "const_declaration") parent-bol go-ts-mode-indent-offset)
     ((parent-is "default_case") parent-bol go-ts-mode-indent-offset)
     ((parent-is "expression_case") parent-bol go-ts-mode-indent-offset)
     ((parent-is "selector_expression") parent-bol go-ts-mode-indent-offset)
     ((parent-is "expression_switch_statement") parent-bol 0)
     ((parent-is "field_declaration_list") parent-bol go-ts-mode-indent-offset)
     ((parent-is "import_spec_list") parent-bol go-ts-mode-indent-offset)
     ((parent-is "interface_type") parent-bol go-ts-mode-indent-offset)
     ((parent-is "labeled_statement") parent-bol go-ts-mode-indent-offset)
     ((parent-is "literal_value") parent-bol go-ts-mode-indent-offset)
     ((parent-is "parameter_list") parent-bol go-ts-mode-indent-offset)
     ((parent-is "select_statement") parent-bol 0)
     ((parent-is "type_case") parent-bol go-ts-mode-indent-offset)
     ((parent-is "type_spec") parent-bol go-ts-mode-indent-offset)
     ((parent-is "type_switch_statement") parent-bol 0)
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

(defun go-ts-mode--iota-query-supported-p ()
  "Return t if the iota query is supported by the tree-sitter-go grammar."
  (ignore-errors
    (or (treesit-query-string "" '((iota) @font-lock-constant-face) 'go) t)))

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
   `([(false) (nil) (true)] @font-lock-constant-face
     ,@(when (go-ts-mode--iota-query-supported-p)
         '((iota) @font-lock-constant-face))
     (const_declaration
      (const_spec name: (identifier) @font-lock-constant-face)))

   :language 'go
   :feature 'delimiter
   '((["," "." ";" ":"]) @font-lock-delimiter-face)

   :language 'go
   :feature 'definition
   '((function_declaration
      name: (identifier) @font-lock-function-name-face)
     (method_declaration
      name: (field_identifier) @font-lock-function-name-face)
     (method_spec
      name: (field_identifier) @font-lock-function-name-face)
     (field_declaration
      name: (field_identifier) @font-lock-property-name-face)
     (parameter_declaration
      name: (identifier) @font-lock-variable-name-face)
     (short_var_declaration
      left: (expression_list
             (identifier) @font-lock-variable-name-face
             ("," (identifier) @font-lock-variable-name-face)*))
     (var_spec name: (identifier) @font-lock-variable-name-face
               ("," name: (identifier) @font-lock-variable-name-face)*))

   :language 'go
   :feature 'function
   '((call_expression
      function: (identifier) @font-lock-function-call-face)
     (call_expression
      function: (selector_expression
                 field: (field_identifier) @font-lock-function-call-face)))

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
   :feature 'property
   '((selector_expression field: (field_identifier) @font-lock-property-use-face)
     (keyed_element (_ (identifier) @font-lock-property-use-face)))

   :language 'go
   :feature 'variable
   '((identifier) @font-lock-variable-use-face)

   :language 'go
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'go
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `go-ts-mode'.")

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

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("method_declaration"
                              "function_declaration"
                              "type_declaration")))
    (setq-local treesit-defun-name-function #'go-ts-mode--defun-name)

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Function" "\\`function_declaration\\'" nil nil)
                  ("Method" "\\`method_declaration\\'" nil nil)
                  ("Struct" "\\`type_declaration\\'" go-ts-mode--struct-node-p nil)
                  ("Interface" "\\`type_declaration\\'" go-ts-mode--interface-node-p nil)
                  ("Type" "\\`type_declaration\\'" go-ts-mode--other-type-node-p nil)
                  ("Alias" "\\`type_declaration\\'" go-ts-mode--alias-node-p nil)))

    ;; Indent.
    (setq-local indent-tabs-mode t
                treesit-simple-indent-rules go-ts-mode--indent-rules)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}()" electric-indent-chars))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings go-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string type)
                  ( constant escape-sequence label number)
                  ( bracket delimiter error function operator property variable)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'go)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode)))

(defun go-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("function_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))
    ("method_declaration"
     (let* ((receiver-node (treesit-node-child-by-field-name node "receiver"))
            (type-node (treesit-search-subtree receiver-node "type_identifier"))
            (name-node (treesit-node-child-by-field-name node "name")))
       (concat
        "(" (treesit-node-text type-node) ")."
        (treesit-node-text name-node))))
    ("type_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name
       (treesit-node-child node 0 t) "name")
      t))))

(defun go-ts-mode--interface-node-p (node)
  "Return t if NODE is an interface."
  (and
   (string-equal "type_declaration" (treesit-node-type node))
   (treesit-search-subtree node "interface_type" nil nil 2)))

(defun go-ts-mode--struct-node-p (node)
  "Return t if NODE is a struct."
  (and
   (string-equal "type_declaration" (treesit-node-type node))
   (treesit-search-subtree node "struct_type" nil nil 2)))

(defun go-ts-mode--alias-node-p (node)
  "Return t if NODE is a type alias."
  (and
   (string-equal "type_declaration" (treesit-node-type node))
   (treesit-search-subtree node "type_alias" nil nil 1)))

(defun go-ts-mode--other-type-node-p (node)
  "Return t if NODE is a type other than interface, struct, or alias."
  (and
   (string-equal "type_declaration" (treesit-node-type node))
   (not (go-ts-mode--interface-node-p node))
   (not (go-ts-mode--struct-node-p node))
   (not (go-ts-mode--alias-node-p node))))

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
  "Return non-nil if point is inside a directive.
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

(if (treesit-ready-p 'gomod)
    (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode)))

(provide 'go-ts-mode)

;;; go-ts-mode.el ends here
