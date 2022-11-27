;;; c-ts-mode.el --- tree-sitter support for C and C++  -*- lexical-binding: t; -*-

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
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defcustom c-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `c-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'c)

(defcustom c-ts-mode-indent-style 'gnu
  "Style used for indentation.

The selected style could be one of GNU, K&R, LINUX or BSD.  If
one of the supplied styles doesn't suffice a function could be
set instead.  This function is expected return a list that
follows the form of `treesit-simple-indent-rules'."
  :version "29.1"
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
           ((and (parent-is "comment") comment-end) comment-start -1)
           ((parent-is "comment") comment-start-skip 0)
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
   :feature 'comment
   `((comment) @font-lock-comment-face
     (comment) @contextual)

   :language mode
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
   :feature 'constant
   `((true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (null) @font-lock-constant-face
     ,@(when (eq mode 'cpp)
         '((this) @font-lock-constant-face)))

   :language mode
   :feature 'keyword
   `([,@(c-ts-mode--keywords mode)] @font-lock-keyword-face
     ,@(when (eq mode 'cpp)
         '((auto) @font-lock-keyword-face)))

   :language mode
   :feature 'operator
   `([,@c-ts-mode--operators] @font-lock-operator-face
     "!" @font-lock-negation-char-face)

   :language mode
   :feature 'string
   `((string_literal) @font-lock-string-face
     (system_lib_string) @font-lock-string-face)

   :language mode
   :feature 'literal
   `((number_literal) @font-lock-number-face
     (char_literal) @font-lock-constant-face)

   :language mode
   :feature 'type
   `((primitive_type) @font-lock-type-face
     (type_identifier) @font-lock-type-face
     (sized_type_specifier) @font-lock-type-face
     ,@(when (eq mode 'cpp)
         '((type_qualifier) @font-lock-type-face

           (qualified_identifier
            scope: (namespace_identifier) @font-lock-type-face)

           (operator_cast) type: (type_identifier) @font-lock-type-face)))

   :language mode
   :feature 'definition
   ;; Highlights identifiers in declarations.
   `((declaration
      declarator: (_) @c-ts-mode--fontify-declarator)

     (field_declaration
      declarator: (_) @c-ts-mode--fontify-declarator)

     (function_definition
      declarator: (_) @c-ts-mode--fontify-declarator))

   ;; Should we highlight identifiers in the parameter list?
   ;; (parameter_declaration
   ;;  declarator: (_) @c-ts-mode--fontify-declarator))

   :language mode
   :feature 'assignment
   ;; TODO: Recursively highlight identifiers in parenthesized
   ;; expressions, see `c-ts-mode--fontify-struct-declarator' for
   ;; inspiration.
   '((assignment_expression
      left: (identifier) @font-lock-variable-name-face)
     (assignment_expression
      left: (field_expression field: (_) @font-lock-property-face))
     (assignment_expression
      left: (pointer_expression
             (identifier) @font-lock-variable-name-face))
     (assignment_expression
      left: (subscript_expression
             (identifier) @font-lock-variable-name-face))
     (init_declarator declarator: (_) @c-ts-mode--fontify-declarator))

   :language mode
   :feature 'function
   '((call_expression
      function: (identifier) @font-lock-function-name-face))

   :language mode
   :feature 'variable
   '((identifier) @c-ts-mode--fontify-variable)

   :language mode
   :feature 'label
   '((labeled_statement
      label: (statement_identifier) @font-lock-constant-face))

   :language mode
   :feature 'error
   '((ERROR) @c-ts-fontify-error)

   :feature 'escape-sequence
   :language mode
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language mode
   :feature 'property
   '((field_identifier) @font-lock-property-face
     (enumerator
      name: (identifier) @font-lock-property-face))

   :language mode
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language mode
   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face)

   :language mode
   :feature 'emacs-devel
   :override t
   '(((call_expression
       (call_expression function: (identifier) @fn)
       @c-ts-mode--fontify-defun)
      (:match "^DEFUN$" @fn)))))

(defun c-ts-mode--fontify-declarator (node override start end &rest args)
  "Fontify a declarator (whatever under the \"declarator\" field).
For NODE, OVERRIDE, START, END, and ARGS, see
`treesit-font-lock-rules'."
  (pcase (treesit-node-type node)
    ((or "attributed_declarator" "parenthesized_declarator")
     (apply #'c-ts-mode--fontify-declarator
            (treesit-node-child node 0 t) override start end args))
    ("pointer_declarator"
     (apply #'c-ts-mode--fontify-declarator
            (treesit-node-child node -1) override start end args))
    ((or "function_declarator" "array_declarator" "init_declarator")
     (apply #'c-ts-mode--fontify-declarator
            (treesit-node-child-by-field-name node "declarator")
            override start end args))
    ((or "identifier" "field_identifier")
     (treesit-fontify-with-override
      (max (treesit-node-start node) start)
      (min (treesit-node-end node) end)
      (pcase (treesit-node-type (treesit-node-parent node))
        ("function_declarator" 'font-lock-function-name-face)
        (_ 'font-lock-variable-name-face))
      override))))

(defun c-ts-mode--fontify-variable (node override start end &rest _)
  "Fontify an identifier node.
Fontify it if NODE is not a function identifier.  For NODE,
OVERRIDE, START, END, and ARGS, see `treesit-font-lock-rules'."
  (when (not (equal (treesit-node-type
                     (treesit-node-parent node))
                    "call_expression"))
    (treesit-fontify-with-override
     (max (treesit-node-start node) start)
     (min (treesit-node-end node) end)
     'font-lock-variable-name-face
     override)))

(defun c-ts-mode--fontify-defun (node override start end &rest _)
  "Correctly fontify the DEFUN macro.
For NODE, OVERRIDE, START, and END, see
`treesit-font-lock-rules'.  The captured NODE is a
call_expression where DEFUN is the function.

This function corrects the fontification on the colon in
\"doc:\", and the parameter list."
  (let* ((parent (treesit-node-parent node))
         ;; ARG-LIST-1 and 2 are like this:
         ;;
         ;; DEFUN (ARG-LIST-1)
         ;; (ARG-LIST-2)
         (arg-list-1 (treesit-node-children
                      (treesit-node-child-by-field-name
                       node "arguments")))
         ;; ARG-LIST-2 is the
         (arg-list-2 (treesit-node-children
                      (treesit-node-child-by-field-name
                       parent "arguments") t)))
    ;; Fix the colon.
    (dolist (node arg-list-1)
      (when (equal (treesit-node-text node t) ":")
        (treesit-fontify-with-override
         (treesit-node-start node) (treesit-node-end node)
         'default override)))
    ;; Fix the parameter list.
    (while arg-list-2
      (let ((type (and arg-list-2 (pop arg-list-2)))
            (arg (and arg-list-2 (pop arg-list-2))))
        (when type
          (treesit-fontify-with-override
           (max start (treesit-node-start type))
           (min end (treesit-node-end type))
           'font-lock-type-face override))
        (when arg
          (treesit-fontify-with-override
           (max start (treesit-node-start arg))
           (min end (treesit-node-end arg))
           'default override))))))

(defun c-ts-fontify-error (node override start end &rest _)
  "Fontify the error nodes.
For NODE, OVERRIDE, START, and END, see
`treesit-font-lock-rules'."
  (let ((parent (treesit-node-parent node))
        (child (treesit-node-child node 0)))
    (treesit-fontify-with-override
     (max start (treesit-node-start node))
     (min end (treesit-node-end node))
     (cond
      ;; This matches the case MACRO(struct a, b, c)
      ;; where struct is seen as error.
      ((and (equal (treesit-node-type child) "identifier")
            (equal (treesit-node-type parent) "argument_list")
            (member (treesit-node-text child)
                    '("struct" "long" "short" "enum" "union")))
       'font-lock-keyword-face)
      (t 'font-lock-warning-face))
     override)))

(defun c-ts-mode--imenu-1 (node)
  "Helper for `c-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (subtrees (mapcan #'c-ts-mode--imenu-1 (cdr node)))
         (name (when ts-node
                 (treesit-node-text
                  (pcase (treesit-node-type ts-node)
                    ("function_definition"
                     (treesit-node-child-by-field-name
                      (treesit-node-child-by-field-name
                       ts-node "declarator")
                      "declarator"))
                    ("declaration"
                     (let ((child (treesit-node-child ts-node -1 t)))
                       (pcase (treesit-node-type child)
                         ("identifier" child)
                         (_ (treesit-node-child-by-field-name
                             child "declarator")))))
                    ("struct_specifier"
                     (treesit-node-child-by-field-name
                      ts-node "name"))))))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ;; A struct_specifier could be inside a parameter list, another
     ;; struct definition, a variable declaration, a function
     ;; declaration.  In those cases we don't include it.
     ((string-match-p
       (rx (or "parameter_declaration" "field_declaration"
               "declaration" "function_definition"))
       (or (treesit-node-type (treesit-node-parent ts-node))
           ""))
      nil)
     ;; Ignore function local variable declarations.
     ((and (equal (treesit-node-type ts-node) "declaration")
           (not (equal (treesit-node-type (treesit-node-parent ts-node))
                       "translation_unit")))
      nil)
     ((or (null ts-node) (null name)) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

(defun c-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (func-tree (treesit-induce-sparse-tree
                     node "^function_definition$" nil 1000))
         (var-tree (treesit-induce-sparse-tree
                    node "^declaration$" nil 1000))
         (struct-tree (treesit-induce-sparse-tree
                       node "^struct_specifier$" nil 1000))
         (func-index (c-ts-mode--imenu-1 func-tree))
         (var-index (c-ts-mode--imenu-1 var-tree))
         (struct-index (c-ts-mode--imenu-1 struct-tree)))
    (append
     (when struct-index `(("Struct" . ,struct-index)))
     (when var-index `(("Variable" . ,var-index)))
     (when func-index `(("Function" . ,func-index))))))

;;;###autoload
(define-derived-mode c-ts-mode--base-mode prog-mode "C"
  "Major mode for editing C, powered by tree-sitter."
  :syntax-table c-ts-mode--syntax-table

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
              '(( comment definition)
                ( keyword preprocessor string type)
                ( assignment constant escape-sequence label literal property )
                ( bracket delimiter error function operator variable))))

;;;###autoload
(define-derived-mode c-ts-mode c-ts-mode--base-mode "C"
  "Major mode for editing C, powered by tree-sitter."
  :group 'c

  (unless (treesit-ready-p 'c)
    (error "Tree-sitter for C isn't available"))

  (treesit-parser-create 'c)

  ;; Comments.
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")
  (setq-local comment-start-skip (rx (group "/" (or (+ "/") (+ "*")))
                                     (* (syntax whitespace))))
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end)
                             (seq (+ "*") "/")))))

  (setq-local treesit-simple-indent-rules
              (c-ts-mode--set-indent-style 'c))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (c-ts-mode--font-lock-settings 'c))

  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode c++-ts-mode c-ts-mode--base-mode "C++"
  "Major mode for editing C++, powered by tree-sitter."
  :group 'c++

  (unless (treesit-ready-p 'cpp)
    (error "Tree-sitter for C++ isn't available"))

  ;; Comments.
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (group "/" (or (+ "/") (+ "*")))
                                     (* (syntax whitespace))))
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end)
                             (seq (+ "*") "/")))))

  (treesit-parser-create 'cpp)

  (setq-local treesit-simple-indent-rules
              (c-ts-mode--set-indent-style 'cpp))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (c-ts-mode--font-lock-settings 'cpp))

  (treesit-major-mode-setup))

(provide 'c-ts-mode)

;;; c-ts-mode.el ends here
