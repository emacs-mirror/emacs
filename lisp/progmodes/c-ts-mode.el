;;; c-ts-mode.el --- tree sitter support for C and C++  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : November 2022
;; Keywords   : c c++ cpp languages tree-sitter

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
(require 'rx)

(defcustom c-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `c-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'c)

(defcustom c-ts-mode-indent-style 'gnu
  "Style used for indentation.

The selected style could be one of GNU, K&R, LINUX or BSD.  If
one of the supplied styles doesn't suffice a function could be
set instead.  This function is expected return a list that
follows the form of `treesit-simple-indent-rules'."
  :type '(choice (symbol :tag "Gnu" 'gnu)
                 (symbol :tag "K&R" 'k&r)
                 (symbol :tag "Linux" 'linux)
                 (symbol :tag "BSD" 'bsd)
                 (function :tag "A function for user customized style" ignore))
  :group 'c)

(defvar c-ts-mode--syntax-table
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
    table)
  "Syntax table for `c-ts-mode'.")

(defun c-ts-mode--indent-styles (mode)
  "Indent rules supported by `c-ts-mode'.
MODE is either `c' or `cpp'."
  (let ((common
         `(((parent-is "translation_unit") parent-bol 0)
           ((node-is ")") parent 1)
           ((node-is "]") parent-bol 0)
           ((node-is "}") (and parent parent-bol) 0)
           ((node-is "else") parent-bol 0)
           ((node-is "case") parent-bol 0)
           ((node-is "preproc_arg") no-indent)
           ((node-is "comment") no-indent)
           ((parent-is "comment") no-indent)
           ((node-is "labeled_statement") parent-bol 0)
           ((parent-is "labeled_statement") parent-bol c-ts-mode-indent-offset)
           ((match "preproc_ifdef" "compound_statement") point-min 0)
           ((match "#endif" "preproc_ifdef") point-min 0)
           ((match "preproc_if" "compound_statement") point-min 0)
           ((match "#endif" "preproc_if") point-min 0)
           ((match "preproc_function_def" "compound_statement") point-min 0)
           ((match "preproc_call" "compound_statement") point-min 0)
           ((parent-is "compound_statement") (and parent parent-bol) c-ts-mode-indent-offset)
           ((parent-is "function_definition") parent-bol 0)
           ((parent-is "conditional_expression") first-sibling 0)
           ((parent-is "assignment_expression") parent-bol c-ts-mode-indent-offset)
           ((parent-is "comma_expression") first-sibling 0)
           ((parent-is "init_declarator") parent-bol c-ts-mode-indent-offset)
           ((parent-is "parenthesized_expression") first-sibling 1)
           ((parent-is "argument_list") first-sibling 1)
           ((parent-is "parameter_list") first-sibling 1)
           ((parent-is "binary_expression") parent 0)
           ((query "(for_statement initializer: (_) @indent)") parent-bol 5)
           ((query "(for_statement condition: (_) @indent)") parent-bol 5)
           ((query "(for_statement update: (_) @indent)") parent-bol 5)
           ((query "(call_expression arguments: (_) @indent)") parent c-ts-mode-indent-offset)
           ((parent-is "call_expression") parent 0)
           ((parent-is "enumerator_list") parent-bol c-ts-mode-indent-offset)
           ((parent-is "field_declaration_list") parent-bol c-ts-mode-indent-offset)
           ((parent-is "initializer_list") parent-bol c-ts-mode-indent-offset)
           ((parent-is "if_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "for_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "while_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "switch_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "case_statement") parent-bol c-ts-mode-indent-offset)
           ((parent-is "do_statement") parent-bol c-ts-mode-indent-offset)
           ,@(when (eq mode 'cpp)
               `(((node-is "field_initializer_list") parent-bol ,(* c-ts-mode-indent-offset 2)))))))
    `((gnu
       ;; Prepend rules to set highest priority
       ((match "while" "do_statement") parent 0)
       ,@common)
      (k&r ,@common)
      (linux ,@common)
      (bsd
       ((parent-is "if_statement") parent-bol 0)
       ((parent-is "for_statement") parent-bol 0)
       ((parent-is "while_statement") parent-bol 0)
       ((parent-is "switch_statement") parent-bol 0)
       ((parent-is "case_statement") parent-bol 0)
       ((parent-is "do_statement") parent-bol 0)
       ,@common))))

(defun c-ts-mode--set-indent-style (mode)
  "Helper function to set indentation style.
MODE is either `c' or `cpp'."
  (let ((style
         (if (functionp c-ts-mode-indent-style)
             (funcall c-ts-mode-indent-style)
           (pcase c-ts-mode-indent-style
             ('gnu   (alist-get 'gnu (c-ts-mode--indent-styles mode)))
             ('k&r   (alist-get 'k&r (c-ts-mode--indent-styles mode)))
             ('bsd   (alist-get 'bsd (c-ts-mode--indent-styles mode)))
             ('linux (alist-get 'linux (c-ts-mode--indent-styles mode)))))))
    `((,mode ,@style))))

(defvar c-ts-mode--preproc-keywords
  '("#define" "#if" "#ifdef" "#ifndef"
    "#else" "#elif" "#endif" "#include")
  "C/C++ keywords for tree-sitter font-locking.")

(defun c-ts-mode--keywords (mode)
  "C/C++ keywords for tree-sitter font-locking.
MODE is either `c' or `cpp'."
  (let ((c-keywords
         '("break" "case" "const" "continue"
           "default" "do" "else" "enum"
           "extern" "for" "goto" "if"
           "long" "register" "return" "short"
           "signed" "sizeof" "static" "struct"
           "switch" "typedef" "union" "unsigned"
           "volatile" "while")))
    (if (eq mode 'cpp)
        (append c-keywords
                '("and" "and_eq" "bitand" "bitor"
                  "catch" "class" "co_await" "co_return"
                  "co_yield" "compl" "concept" "consteval"
                  "constexpr" "constinit" "decltype" "delete"
                  "explicit" "final" "friend" "friend"
                  "mutable" "namespace" "new" "noexcept"
                  "not" "not_eq" "operator" "or"
                  "or_eq" "override" "private" "protected"
                  "public" "requires" "template" "throw"
                  "try" "typename" "using" "virtual"
                  "xor" "xor_eq"))
      (append '("auto") c-keywords))))

(defvar c-ts-mode--operators
  '("=" "-" "*" "/" "+" "%" "~" "|" "&" "^" "<<" ">>" "->"
    "." "<" "<=" ">=" ">" "==" "!=" "!" "&&" "||" "-="
    "+=" "*=" "/=" "%=" "|=" "&=" "^=" ">>=" "<<=" "--" "++")
  "C/C++ operators for tree-sitter font-locking.")

(defun c-ts-mode--font-lock-settings (mode)
  "Tree-sitter font-lock settings.
MODE is either `c' or `cpp'."
  (treesit-font-lock-rules
   :language mode
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face
     (comment) @contexual)
   :language mode
   :override t
   :feature 'preprocessor
   `((preproc_directive) @font-lock-preprocessor-face

     (preproc_def
      name: (identifier) @font-lock-variable-name-face)

     (preproc_ifdef
      name: (identifier) @font-lock-variable-name-face)

     (preproc_function_def
      name: (identifier) @font-lock-function-name-face)

     (preproc_params
      (identifier) @font-lock-variable-name-face)

     (preproc_defined) @font-lock-preprocessor-face
     (preproc_defined (identifier) @font-lock-variable-name-face)
     [,@c-ts-mode--preproc-keywords] @font-lock-preprocessor-face)
   :language mode
   :override t
   :feature 'constant
   `((true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (null) @font-lock-constant-face
     ,@(when (eq mode 'cpp)
         '((this) @font-lock-constant-face)))
   :language mode
   :override t
   :feature 'keyword
   `([,@(c-ts-mode--keywords mode)] @font-lock-keyword-face
     ,@(when (eq mode 'cpp)
         '((auto) @font-lock-keyword-face)))
   :language mode
   :override t
   :feature 'operator
   `([,@c-ts-mode--operators] @font-lock-builtin-face)
   :language mode
   :override t
   :feature 'string
   `((string_literal) @font-lock-string-face
     ((string_literal)) @contextual
     (system_lib_string) @font-lock-string-face
     (escape_sequence) @font-lock-string-face)
   :language mode
   :override t
   :feature 'literal
   `((number_literal) @font-lock-constant-face
     (char_literal) @font-lock-constant-face)
   :language mode
   :override t
   :feature 'type
   `((primitive_type) @font-lock-type-face
     ,@(when (eq mode 'cpp)
         '((type_qualifier) @font-lock-type-face

           (qualified_identifier
            scope: (namespace_identifier) @font-lock-type-face)

           (operator_cast) type: (type_identifier) @font-lock-type-face)))
   :language mode
   :override t
   :feature 'definition
   `((declaration
      declarator: (identifier) @font-lock-variable-name-face)

     (declaration
      type: (type_identifier) @font-lock-type-face)

     (field_declaration
      declarator: (field_identifier) @font-lock-variable-name-face)

     (field_declaration
      type: (type_identifier) @font-lock-type-face)

     (parameter_declaration
      type: (type_identifier) @font-lock-type-face)

     (function_definition
      type: (type_identifier) @font-lock-type-face)

     (function_declarator
      declarator: (identifier) @font-lock-function-name-face)

     (array_declarator
      declarator: (identifier) @font-lock-variable-name-face)

     (init_declarator
      declarator: (identifier) @font-lock-variable-name-face)

     (struct_specifier
      name: (type_identifier) @font-lock-type-face)

     (sized_type_specifier) @font-lock-type-face

     (enum_specifier
      name: (type_identifier) @font-lock-type-face)

     (enumerator
      name: (identifier) @font-lock-variable-name-face)

     (parameter_declaration
      type: (_) @font-lock-type-face
      declarator: (identifier) @font-lock-variable-name-face)

     (pointer_declarator
      declarator: (identifier) @font-lock-variable-name-face)

     (pointer_declarator
      declarator: (field_identifier) @font-lock-variable-name-face))
   :language mode
   :override t
   :feature 'expression
   '((assignment_expression
      left: (identifier) @font-lock-variable-name-face)

     (call_expression
      function: (identifier) @font-lock-function-name-face)

     (field_expression
      field: (field_identifier) @font-lock-variable-name-face)

     (field_expression
      argument: (identifier) @font-lock-variable-name-face
      field: (field_identifier) @font-lock-variable-name-face)

     (pointer_expression
      argument: (identifier) @font-lock-variable-name-face))
   :language mode
   :override t
   :feature 'statement
   '((expression_statement (identifier) @font-lock-variable-name-face)
     (labeled_statement
      label: (statement_identifier) @font-lock-type-face))
   :language mode
   :override t
   :feature 'error
   '((ERROR) @font-lock-warning-face)))

(defun c-ts-mode--imenu-1 (node)
  "Helper for `c-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (subtrees (mapcan #'c-ts-mode--imenu-1 (cdr node)))
         (name (when ts-node
                 (or (treesit-node-text
                      (or (treesit-node-child-by-field-name
                           ts-node "declarator")
                          (treesit-node-child-by-field-name
                           ts-node "name"))
                      t)
                     "Unnamed node")))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    ;; A struct_specifier could be inside a parameter list or another
    ;; struct definition.  In those cases we don't include it.
    (cond
     ((string-match-p
       (rx (or "parameter" "field") "_declaration")
       (or (treesit-node-type (treesit-node-parent ts-node))
           ""))
      nil)
     ((null ts-node) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

(defun c-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (tree (treesit-induce-sparse-tree
                node (rx (or "function_definition"
                             "struct_specifier")))))
    (c-ts-mode--imenu-1 tree)))

;;;###autoload
(define-derived-mode c-ts-mode--base-mode prog-mode "C"
  "Major mode for editing C, powered by Tree Sitter."
  :group 'c
  :syntax-table c-ts-mode--syntax-table

  ;; Comments.
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (rx (or "specifier"
                      "definition")))

  ;; Indent.
  (when (eq c-ts-mode-indent-style 'linux)
    (setq-local indent-tabs-mode t))

  ;; Electric
  (setq-local electric-indent-chars
	      (append "{}():;," electric-indent-chars))

  ;; Imenu.
  (setq-local imenu-create-index-function #'c-ts-mode--imenu)
  (setq-local which-func-functions nil)

  (setq-local treesit-font-lock-feature-list
              '((comment preprocessor operator constant string literal keyword)
                (type definition expression statement)
                (error))))

;;;###autoload
(define-derived-mode c-ts-mode c-ts-mode--base-mode "C"
  "Major mode for editing C, powered by Tree Sitter."
  :group 'c

  (unless (treesit-ready-p nil 'c)
    (error "Tree Sitter for C isn't available"))

  (treesit-parser-create 'c)

  (setq-local treesit-simple-indent-rules
              (c-ts-mode--set-indent-style 'c))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (c-ts-mode--font-lock-settings 'c))

  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode c++-ts-mode c-ts-mode--base-mode "C++"
  "Major mode for editing C, powered by Tree Sitter."
  :group 'c++

  (unless (treesit-ready-p nil 'cpp)
    (error "Tree Sitter for C++ isn't available"))

  (treesit-parser-create 'cpp)

  (setq-local treesit-simple-indent-rules
              (c-ts-mode--set-indent-style 'cpp))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (c-ts-mode--font-lock-settings 'cpp))

  (treesit-major-mode-setup))

(provide 'c-ts-mode)

;;; c-ts-mode.el ends here
