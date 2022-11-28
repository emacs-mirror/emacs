;;; java-ts-mode.el --- tree-sitter support for Java  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : November 2022
;; Keywords   : java languages tree-sitter

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

(defcustom java-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `java-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'java)

(defvar java-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the cc-langs version
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    (modify-syntax-entry ?\240 "."   table)
    table)
  "Syntax table for `java-ts-mode'.")

(defvar java-ts-mode--indent-rules
  `((java
     ((parent-is "program") parent-bol 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((and (parent-is "comment") comment-end) comment-start -1)
     ((parent-is "comment") comment-start-skip 0)
     ((parent-is "class_body") parent-bol java-ts-mode-indent-offset)
     ((parent-is "interface_body") parent-bol java-ts-mode-indent-offset)
     ((parent-is "constructor_body") parent-bol java-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol java-ts-mode-indent-offset)
     ((parent-is "switch_block") parent-bol java-ts-mode-indent-offset)
     ((parent-is "record_declaration_body") parent-bol java-ts-mode-indent-offset)
     ((query "(method_declaration (block _ @indent))") parent-bol java-ts-mode-indent-offset)
     ((query "(method_declaration (block (_) @indent))") parent-bol java-ts-mode-indent-offset)
     ((parent-is "variable_declarator") parent-bol java-ts-mode-indent-offset)
     ((parent-is "method_invocation") parent-bol java-ts-mode-indent-offset)
     ((parent-is "switch_rule") parent-bol java-ts-mode-indent-offset)
     ((parent-is "ternary_expression") parent-bol java-ts-mode-indent-offset)
     ((parent-is "element_value_array_initializer") parent-bol java-ts-mode-indent-offset)
     ((parent-is "function_definition") parent-bol 0)
     ((parent-is "conditional_expression") first-sibling 0)
     ((parent-is "assignment_expression") parent-bol 2)
     ((parent-is "binary_expression") parent 0)
     ((parent-is "parenthesized_expression") first-sibling 1)
     ((parent-is "argument_list") parent-bol java-ts-mode-indent-offset)
     ((parent-is "annotation_argument_list") parent-bol java-ts-mode-indent-offset)
     ((parent-is "modifiers") parent-bol 0)
     ((parent-is "formal_parameters") parent-bol java-ts-mode-indent-offset)
     ((parent-is "formal_parameter") parent-bol 0)
     ((parent-is "init_declarator") parent-bol java-ts-mode-indent-offset)
     ((parent-is "if_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "for_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "while_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "switch_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "case_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "labeled_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "do_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "block") (and parent parent-bol) java-ts-mode-indent-offset)))
  "Tree-sitter indent rules.")

(defvar java-ts-mode--keywords
  '("abstract" "assert" "break" "case" "catch"
    "class" "continue" "default" "do" "else"
    "enum" "exports" "extends" "final" "finally"
    "for" "if" "implements" "import" "instanceof"
    "interface" "module" "native" "new" "non-sealed"
    "open" "opens" "package" "private" "protected"
    "provides" "public" "requires" "return" "sealed"
    "static" "strictfp" "switch" "synchronized"
    "throw" "throws" "to" "transient" "transitive"
    "try" "uses" "volatile" "while" "with" "record")
  "C keywords for tree-sitter font-locking.")

(defvar java-ts-mode--operators
  '("+" ":" "++" "-" "--" "&" "&&" "|" "||" "="
    "!=" "==" "*" "/" "%" "<" "<=" ">" ">="
    "-=" "+=" "*=" "/=" "%=" "->" "^" "^="
    "|=" "~" ">>" ">>>" "<<" "::" "?" "&=")
  "C operators for tree-sitter font-locking.")

(defvar java-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'java
   :override t
   :feature 'comment
   `((line_comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)
   :language 'java
   :override t
   :feature 'constant
   `(((identifier) @font-lock-constant-face
      (:match "^[A-Z_][A-Z_\\d]*$" @font-lock-constant-face))
     [(true) (false)] @font-lock-constant-face)
   :language 'java
   :override t
   :feature 'keyword
   `([,@java-ts-mode--keywords] @font-lock-keyword-face
     (labeled_statement
      (identifier) @font-lock-keyword-face))
   :language 'java
   :override t
   :feature 'operator
   `([,@java-ts-mode--operators] @font-lock-operator-face
     "@" @font-lock-constant-face)
   :language 'java
   :override t
   :feature 'annotation
   `((annotation
      name: (identifier) @font-lock-constant-face)

     (marker_annotation
      name: (identifier) @font-lock-constant-face))
   :language 'java
   :override t
   :feature 'string
   `((string_literal) @font-lock-string-face)
   :language 'java
   :override t
   :feature 'literal
   `((null_literal) @font-lock-constant-face
     (binary_integer_literal)  @font-lock-number-face
     (decimal_integer_literal) @font-lock-number-face
     (hex_integer_literal) @font-lock-number-face
     (octal_integer_literal) @font-lock-number-face
     (decimal_floating_point_literal) @font-lock-number-face
     (hex_floating_point_literal) @font-lock-number-face)
   :language 'java
   :override t
   :feature 'type
   '((interface_declaration
      name: (identifier) @font-lock-type-face)

     (class_declaration
      name: (identifier) @font-lock-type-face)

     (record_declaration
      name: (identifier) @font-lock-type-face)

     (enum_declaration
      name: (identifier) @font-lock-type-face)

     (constructor_declaration
      name: (identifier) @font-lock-type-face)

     (field_access
      object: (identifier) @font-lock-type-face)

     (method_reference (identifier) @font-lock-type-face)

     (scoped_identifier (identifier) @font-lock-variable-name-face)

     ((scoped_identifier name: (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))

     (type_identifier) @font-lock-type-face

     [(boolean_type)
      (integral_type)
      (floating_point_type)
      (void_type)] @font-lock-type-face)
   :language 'java
   :override t
   :feature 'definition
   `((method_declaration
      name: (identifier) @font-lock-function-name-face)

     (variable_declarator
      name: (identifier) @font-lock-variable-name-face)

     (element_value_pair
      key: (identifier) @font-lock-property-face)

     (formal_parameter
      name: (identifier) @font-lock-variable-name-face)

     (catch_formal_parameter
      name: (identifier) @font-lock-variable-name-face))
   :language 'java
   :override t
   :feature 'expression
   '((method_invocation
      object: (identifier) @font-lock-variable-name-face)

     (method_invocation
      name: (identifier) @font-lock-function-name-face)

     (argument_list (identifier) @font-lock-variable-name-face))

   :language 'java
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'java
   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face))
  "Tree-sitter font-lock settings.")

(defun java-ts-mode--imenu-1 (node)
  "Helper for `java-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (subtrees (mapcan #'java-ts-mode--imenu-1 (cdr node)))
         (name (when ts-node
                 (or (treesit-node-text
                      (or (treesit-node-child-by-field-name
                           ts-node "name"))
                      t)
                     "Unnamed node")))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((null ts-node) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

(defun java-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (class-tree (treesit-induce-sparse-tree
                      node "^class_declaration$" nil 1000))
         (interface-tree (treesit-induce-sparse-tree
                          node "^interface_declaration$" nil 1000))
         (enum-tree (treesit-induce-sparse-tree
                     node "^enum_declaration$" nil 1000))
         (record-tree (treesit-induce-sparse-tree
                       node "^record_declaration$"  nil 1000))
         (method-tree (treesit-induce-sparse-tree
                       node "^method_declaration$" nil 1000))
         (class-index (java-ts-mode--imenu-1 class-tree))
         (interface-index (java-ts-mode--imenu-1 interface-tree))
         (enum-index (java-ts-mode--imenu-1 enum-tree))
         (record-index (java-ts-mode--imenu-1 record-tree))
         (method-index (java-ts-mode--imenu-1 method-tree)))
    (append
     (when class-index `(("Class" . ,class-index)))
     (when interface-index `(("Interface" . ,interface-index)))
     (when enum-index `(("Enum" . ,enum-index)))
     (when record-index `(("Record" . ,record-index)))
     (when method-index `(("Method" . ,method-index))))))

;;;###autoload
(define-derived-mode java-ts-mode prog-mode "Java"
  "Major mode for editing Java, powered by tree-sitter."
  :group 'java
  :syntax-table java-ts-mode--syntax-table

  (unless (treesit-ready-p 'java)
    (error "Tree-sitter for Java isn't available"))

  (treesit-parser-create 'java)

  ;; Comments.
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (group "/" (or (+ "/") (+ "*")))
                                     (* (syntax whitespace))))
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end)
                             (seq (+ "*") "/")))))

  ;; Indent.
  (setq-local treesit-simple-indent-rules java-ts-mode--indent-rules)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp "declaration")

  ;; Font-lock.
  (setq-local treesit-font-lock-settings java-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( comment definition )
                ( constant keyword string type)
                ( annotation expression literal)
                ( bracket delimiter operator)))

  ;; Imenu.
  (setq-local imenu-create-index-function #'java-ts-mode--imenu)
  (setq-local which-func-functions nil) ;; Piggyback on imenu
  (treesit-major-mode-setup))

(provide 'java-ts-mode)

;;; java-ts-mode.el ends here
