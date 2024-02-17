;;; java-ts-mode.el --- tree-sitter support for Java  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : November 2022
;; Keywords   : java languages tree-sitter

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
(require 'c-ts-common) ; For comment indent and filling.

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-query-capture "treesit.c")

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
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table for `java-ts-mode'.")

(defvar java-ts-mode--indent-rules
  `((java
     ((parent-is "program") column-0 0)
     ((match "}" "element_value_array_initializer")
      parent-bol 0)
     ((node-is
       ,(format "\\`%s\\'"
                (regexp-opt '("constructor_body" "class_body" "interface_body"
                              "block" "switch_block" "array_initializer"))))
      parent-bol 0)
     ((node-is "}") standalone-parent 0)
     ((node-is ")") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "text_block") no-indent)
     ((parent-is "class_body") column-0 c-ts-common-statement-offset)
     ((parent-is "array_initializer") parent-bol java-ts-mode-indent-offset)
     ((parent-is "annotation_type_body") column-0 c-ts-common-statement-offset)
     ((parent-is "interface_body") column-0 c-ts-common-statement-offset)
     ((parent-is "constructor_body") standalone-parent java-ts-mode-indent-offset)
     ((parent-is "enum_body_declarations") parent-bol 0)
     ((parent-is "enum_body") column-0 c-ts-common-statement-offset)
     ((parent-is "switch_block") standalone-parent java-ts-mode-indent-offset)
     ((parent-is "record_declaration_body") column-0 c-ts-common-statement-offset)
     ((query "(method_declaration (block _ @indent))") parent-bol java-ts-mode-indent-offset)
     ((query "(method_declaration (block (_) @indent))") parent-bol java-ts-mode-indent-offset)
     ((parent-is "local_variable_declaration") parent-bol java-ts-mode-indent-offset)
     ((parent-is "expression_statement") parent-bol java-ts-mode-indent-offset)
     ((match "type_identifier" "field_declaration") parent-bol 0)
     ((parent-is "field_declaration") parent-bol java-ts-mode-indent-offset)
     ((parent-is "return_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "variable_declarator") parent-bol java-ts-mode-indent-offset)
     ((match ">" "type_arguments") parent-bol 0)
     ((parent-is "type_arguments") parent-bol java-ts-mode-indent-offset)
     ((parent-is "method_invocation") parent-bol java-ts-mode-indent-offset)
     ((parent-is "switch_rule") parent-bol java-ts-mode-indent-offset)
     ((parent-is "switch_label") parent-bol java-ts-mode-indent-offset)
     ((parent-is "ternary_expression") parent-bol java-ts-mode-indent-offset)
     ((parent-is "lambda_expression") parent-bol java-ts-mode-indent-offset)
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
     ((parent-is "block") standalone-parent java-ts-mode-indent-offset)))
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
    "try" "uses" "volatile" "while" "with" "record"
    "@interface")
  "Java keywords for tree-sitter font-locking.")

(defvar java-ts-mode--operators
  '("+" ":" "++" "-" "--" "&" "&&" "|" "||" "="
    "!=" "==" "*" "/" "%" "<" "<=" ">" ">="
    "-=" "+=" "*=" "/=" "%=" "->" "^" "^="
    "|=" "~" ">>" ">>>" "<<" "::" "?" "&=")
  "Java operators for tree-sitter font-locking.")

(defun java-ts-mode--string-highlight-helper ()
"Returns, for strings, a query based on what is supported by
the available version of Tree-sitter for java."
  (condition-case nil
      (progn (treesit-query-capture 'java '((text_block) @font-lock-string-face))
	     `((string_literal) @font-lock-string-face
	       (text_block) @font-lock-string-face))
    (error
     `((string_literal) @font-lock-string-face))))

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
      (:match "\\`[A-Z_][0-9A-Z_]*\\'" @font-lock-constant-face))
     [(true) (false)] @font-lock-constant-face)
   :language 'java
   :override t
   :feature 'keyword
   `([,@java-ts-mode--keywords
      (this)
      (super)] @font-lock-keyword-face
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
   (java-ts-mode--string-highlight-helper)
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
   '((annotation_type_declaration
      name: (identifier) @font-lock-type-face)

     (interface_declaration
      name: (identifier) @font-lock-type-face)

     (class_declaration
      name: (identifier) @font-lock-type-face)

     (record_declaration
      name: (identifier) @font-lock-type-face)

     (enum_declaration
      name: (identifier) @font-lock-type-face)

     (constructor_declaration
      name: (identifier) @font-lock-type-face)

     (compact_constructor_declaration
      name: (identifier) @font-lock-type-face)

     (field_access
      object: (identifier) @font-lock-type-face)

     (method_reference (identifier) @font-lock-type-face)

     (scoped_identifier (identifier) @font-lock-constant-face)

     ((scoped_identifier name: (identifier) @font-lock-type-face)
      (:match "\\`[A-Z]" @font-lock-type-face))

     (type_identifier) @font-lock-type-face

     [(boolean_type)
      (integral_type)
      (floating_point_type)
      (void_type)] @font-lock-type-face)
   :language 'java
   :override t
   :feature 'definition
   `((annotation_type_element_declaration
      name: (identifier) @font-lock-function-name-face)

     (method_declaration
      name: (identifier) @font-lock-function-name-face)

     (variable_declarator
      name: (identifier) @font-lock-variable-name-face)

     (element_value_pair
      key: (identifier) @font-lock-property-use-face)

     (formal_parameter
      name: (identifier) @font-lock-variable-name-face)

     (catch_formal_parameter
      name: (identifier) @font-lock-variable-name-face))
   :language 'java
   :override t
   :feature 'expression
   '((method_invocation
      object: (identifier) @font-lock-variable-use-face)

     (method_invocation
      name: (identifier) @font-lock-function-call-face)

     (argument_list (identifier) @font-lock-variable-name-face)

     (expression_statement (identifier) @font-lock-variable-use-face))

   :language 'java
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'java
   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face))
  "Tree-sitter font-lock settings for `java-ts-mode'.")

(defun java-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "method_declaration"
         "class_declaration"
         "record_declaration"
         "interface_declaration"
         "enum_declaration"
         "import_declaration"
         "package_declaration"
         "module_declaration")
     (treesit-node-text
      (treesit-node-child-by-field-name node "name")
      t))))

;;;###autoload
(define-derived-mode java-ts-mode prog-mode "Java"
  "Major mode for editing Java, powered by tree-sitter."
  :group 'java
  :syntax-table java-ts-mode--syntax-table

  (unless (treesit-ready-p 'java)
    (error "Tree-sitter for Java isn't available"))

  (treesit-parser-create 'java)

  ;; Comments.
  (c-ts-common-comment-setup)

  ;; Indent.
  (setq-local c-ts-common-indent-type-regexp-alist
              `((block . ,(rx (or "class_body"
                                  "array_initializer"
                                  "constructor_body"
                                  "annotation_type_body"
                                  "interface_body"
                                  "lambda_expression"
                                  "enum_body"
                                  "switch_block"
                                  "record_declaration_body"
                                  "block")))
                (close-bracket . "}")
                (if . "if_statement")
                (else . ("if_statement" . "alternative"))
                (for . "for_statement")
                (while . "while_statement")
                (do . "do_statement")))
  (setq-local c-ts-common-indent-offset 'java-ts-mode-indent-offset)
  (setq-local treesit-simple-indent-rules java-ts-mode--indent-rules)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (regexp-opt '("method_declaration"
                            "class_declaration"
                            "record_declaration"
                            "interface_declaration"
                            "enum_declaration"
                            "import_declaration"
                            "package_declaration"
                            "module_declaration"
                            "constructor_declaration")))
  (setq-local treesit-defun-name-function #'java-ts-mode--defun-name)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings java-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( comment definition )
                ( constant keyword string type)
                ( annotation expression literal)
                ( bracket delimiter operator)))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              '(("Class" "\\`class_declaration\\'" nil nil)
                ("Interface" "\\`interface_declaration\\'" nil nil)
                ("Enum" "\\`record_declaration\\'" nil nil)
                ("Method" "\\`method_declaration\\'" nil nil)))
  (treesit-major-mode-setup))

(if (treesit-ready-p 'java)
    (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode)))

(provide 'java-ts-mode)

;;; java-ts-mode.el ends here
