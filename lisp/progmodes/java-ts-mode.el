;;; java-ts-mode.el --- tree-sitter support for Java  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;;; Tree-sitter language versions
;;
;; java-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-java: v0.23.5
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

;;; Commentary:
;;
;; If the tree-sitter doxygen grammar is available, then the comment
;; blocks can be highlighted according to this grammar.

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))
(require 'c-ts-common) ; For comment indent and filling.
(treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 '(java "https://github.com/tree-sitter/tree-sitter-java"
        :commit "94703d5a6bed02b98e438d7cad1136c01a60ba2c")
 t)
(add-to-list
 'treesit-language-source-alist
 '(doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen"
           :commit "1e28054cb5be80d5febac082706225e42eff14e6")
 t)

(defcustom java-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `java-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'java)

(defcustom java-ts-mode-method-chaining-indent-offset 8
  "Indent offset for method chaining in `java-ts-mode'."
  :version "31.1"
  :type 'integer
  :safe 'integerp
  :group 'java)

(defcustom java-ts-mode-enable-doxygen nil
  "Enable doxygen syntax highlighting.
If Non-nil, enable doxygen based font lock for comment blocks.
This needs to be set before enabling `java-ts-mode'; if you change
the value after enabling `java-ts-mode', toggle the mode off and on
again."
  :version "31.1"
  :type 'boolean
  :safe 'booleanp
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

(defun java-ts-mode--standalone-predicate (node)
  "Java's standalone predicate.
Return t if NODE is on the start of a line."
  (save-excursion
    (goto-char (treesit-node-start node))
    (if (looking-back (rx bol (* whitespace) (? ".")) (pos-bol))
        t
      (back-to-indentation)
      (when (eq (char-after) ?.)
        (point)))))

(defun java-ts-mode--first-line-on-multi-line-string (_node parent _bol &rest _)
  "Simple-indent matcher for the first line in a multi-line string block.
PARENT and BOL are the as in other matchers."
  (and (treesit-node-match-p parent "multiline_string_fragment")
       (save-excursion
         ;; Less than 2 newlines between point and string start.
         (not (search-backward "\n" (treesit-node-start parent) t 2)))))

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
     (java-ts-mode--first-line-on-multi-line-string parent-bol
                                                    java-ts-mode-indent-offset)
     ((parent-is "multiline_string_fragment") prev-adaptive-prefix 0)
     ((match "\"\"\"" "string_literal" nil 1) prev-adaptive-prefix 0)
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
     ((parent-is "method_invocation") parent-bol java-ts-mode-method-chaining-indent-offset)
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
     ;; ((parent-is "formal_parameters") parent-bol java-ts-mode-indent-offset)
     ;; ((parent-is "formal_parameter") parent-bol 0)
     ((parent-is "init_declarator") parent-bol java-ts-mode-indent-offset)
     ((parent-is "if_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "for_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "while_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "switch_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "case_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "labeled_statement") parent-bol java-ts-mode-indent-offset)
     ((parent-is "do_statement") parent-bol java-ts-mode-indent-offset)
     ;; ((parent-is "block") standalone-parent java-ts-mode-indent-offset)
     c-ts-common-baseline-indent-rule))
  "Tree-sitter indent rules.")

(defvar java-ts-mode--keywords
  '("abstract" "assert" "break"
    "case" "catch" "class" "continue" "default" "do"
    "else" "enum" "exports" "extends" "final" "finally"
    "for" "if" "implements" "import" "instanceof"
    "interface" "long" "module" "native" "new" "non-sealed"
    "open" "opens" "package" "permits" "private" "protected"
    "provides" "public" "record" "requires" "return" "sealed"
    "short" "static" "strictfp" "switch" "synchronized"
    "throw" "throws" "to" "transient" "transitive" "try"
    "uses" "volatile" "when" "while" "with" "yield")
  "Java keywords for tree-sitter font-locking.")

(defvar java-ts-mode--operators
  '("+" ":" "++" "-" "--" "&" "&&" "|" "||" "="
    "!=" "==" "*" "/" "%" "<" "<=" ">" ">="
    "-=" "+=" "*=" "/=" "%=" "->" "^" "^="
    "|=" "~" ">>" ">>>" "<<" "::" "?" "&=")
  "Java operators for tree-sitter font-locking.")

(defun java-ts-mode--string-highlight-helper ()
  "Return, for strings, a query based on what is supported by
the available version of Tree-sitter for Java."
  (condition-case nil
      (progn (treesit-query-capture 'java '((text_block) @font-lock-string-face))
	     `((string_literal) @font-lock-string-face
	       (text_block) @font-lock-string-face))
    (error
     `((string_literal) @font-lock-string-face))))

(defun java-ts-mode--fontify-constant (node override start end &rest _)
  "Fontify a Java constant.
In Java the names of variables declared class constants and of ANSI
constants should be all uppercase with words separated by underscores.
This function also prevents annotations from being highlighted as if
they were constants.
For NODE, OVERRIDE, START, and END, see `treesit-font-lock-rules'."
  (let ((node-start (treesit-node-start node))
	(case-fold-search nil))
    (when (and
	   (not (equal (char-before node-start) ?@)) ;; skip annotations
	   (string-match "\\`[A-Z_][0-9A-Z_]*\\'" (treesit-node-text node)))
      (treesit-fontify-with-override
       node-start (treesit-node-end node)
       'font-lock-constant-face override
       start end))))

(defun java-ts-mode--font-lock-settings ()
  "Return tree-sitter font-lock settings for `java-ts-mode'.

Tree-sitter font-lock settings are evaluated the first time this
function is called.  Subsequent calls return the first evaluated value."
  (treesit-font-lock-rules
   :language 'java
   :override t
   :feature 'comment
   `((line_comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)
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

     (method_reference (identifier) @font-lock-type-face)

     (scoped_identifier (identifier) @font-lock-constant-face)

     ((scoped_identifier name: (identifier) @font-lock-type-face)
      (:match "\\`[A-Z]" @font-lock-type-face))

     (type_identifier) @font-lock-type-face
     ;; In Java, var is not a keyword but rather a auto-determined type.
     ;; But we want to fontify it as a keyword.  (The override query is
     ;; below the general query because :override flag is set for this
     ;; rule.)
     ((type_identifier) @font-lock-keyword-face
      (:match "\\`var\\'" @font-lock-keyword-face))

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
   ;; Make sure the constant feature is after expression and definition,
   ;; because those two applies variable-name-face on some constants.
   :language 'java
   :override t
   :feature 'constant
   `((identifier) @java-ts-mode--fontify-constant
     [(true) (false)] @font-lock-constant-face)
   :language 'java
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'java
   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face)))

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


(defvar java-ts-mode--feature-list
  '(( comment document definition )
    ( constant keyword string type)
    ( annotation expression literal)
    ( bracket delimiter operator)))

;;;###autoload
(define-derived-mode java-ts-mode prog-mode "Java"
  "Major mode for editing Java, powered by tree-sitter."
  :group 'java
  :syntax-table java-ts-mode--syntax-table

  (unless (treesit-ensure-installed 'java)
    (error "Tree-sitter for Java isn't available"))

  (let ((primary-parser (treesit-parser-create 'java)))

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
    (setq-local treesit-simple-indent-standalone-predicate
                #'java-ts-mode--standalone-predicate)
    (setq-local c-ts-common-list-indent-style 'simple)

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

    (setq-local treesit-thing-settings
                `((java
                   (sexp (not (or (and named
                                       ,(rx bos (or "program"
                                                    "line_comment"
                                                    "block_comment")
                                            eos))
                                  (and anonymous
                                       ,(rx (or "{" "}" "[" "]"
                                                "(" ")" "<" ">"
                                                ","))))))
                   (list ,(rx bos (or "inferred_parameters"
                                      "parenthesized_expression"
                                      "argument_list"
                                      "type_arguments"
                                      "switch_block"
                                      "record_pattern_body"
                                      "block"
                                      "resource_specification"
                                      "annotation_argument_list"
                                      "element_value_array_initializer"
                                      "module_body"
                                      "enum_body"
                                      "type_parameters"
                                      "class_body"
                                      "constructor_body"
                                      "annotation_type_body"
                                      "interface_body"
                                      "array_initializer"
                                      "formal_parameters")
                              eos))
                   (sentence ,(rx (or "statement"
                                      "local_variable_declaration"
                                      "field_declaration"
                                      "module_declaration"
                                      "package_declaration"
                                      "import_declaration")))
                   (text ,(regexp-opt '("line_comment"
                                        "block_comment"
                                        "text_block"))))))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings
                (java-ts-mode--font-lock-settings))

    ;; Inject doxygen parser for comment.
    (when (and java-ts-mode-enable-doxygen
               (treesit-ensure-installed 'doxygen))
      (setq-local treesit-primary-parser primary-parser)
      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings
                          c-ts-mode-doxygen-comment-font-lock-settings))
      (setq-local treesit-range-settings
                  (treesit-range-rules
                   :embed 'doxygen
                   :host 'java
                   :local t
                   `(((block_comment) @cap (:match "/\\*\\*" @cap)))))))

  (setq-local treesit-font-lock-feature-list java-ts-mode--feature-list)

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              '(("Class" "\\`class_declaration\\'" nil nil)
                ("Interface" "\\`interface_declaration\\'" nil nil)
                ("Enum" "\\`record_declaration\\'" nil nil)
                ("Method" "\\`method_declaration\\'" nil nil)))
  ;; Outline minor mode
  (setq-local treesit-outline-predicate
              (rx bos (or "class_declaration"
                          "interface_declaration"
                          "method_declaration"
                          "constructor_declaration")
                  eos))

  (treesit-major-mode-setup))

(derived-mode-add-parents 'java-ts-mode '(java-mode))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'treesit-major-mode-remap-alist
               '(java-mode . java-ts-mode)))

(provide 'java-ts-mode)

;;; java-ts-mode.el ends here
