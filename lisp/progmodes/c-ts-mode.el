;;; c-ts-mode.el --- tree-sitter support for C and C++  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : November 2022
;; Keywords   : c c++ cpp languages tree-sitter

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
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")

;;; Custom variables

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

;;; Syntax table

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
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    table)
  "Syntax table for `c-ts-mode'.")

(defun c-ts-mode--syntax-propertize (beg end)
  "Apply syntax text property to template delimiters between BEG and END.

< and > are usually punctuation, e.g., in ->.  But when used for
templates, they should be considered pairs.

This function checks for < and > in the changed RANGES and apply
appropriate text property to alter the syntax of template
delimiters < and >'s."
  (goto-char beg)
  (while (re-search-forward (rx (or "<" ">")) end t)
    (pcase (treesit-node-type
            (treesit-node-parent
             (treesit-node-at (match-beginning 0))))
      ("template_argument_list"
       (put-text-property (match-beginning 0)
                          (match-end 0)
                          'syntax-table
                          (pcase (char-before)
                            (?< '(4 . ?>))
                            (?> '(5 . ?<))))))))

;;; Indent

(defun c-ts-mode--indent-styles (mode)
  "Indent rules supported by `c-ts-mode'.
MODE is either `c' or `cpp'."
  (let ((common
         `(((parent-is "translation_unit") parent-bol 0)
           ((node-is ")") parent 1)
           ((node-is "]") parent-bol 0)
           ((node-is "}") c-ts-mode--bracket-children-anchor 0)
           ((node-is "else") parent-bol 0)
           ((node-is "case") parent-bol 0)
           ((node-is "preproc_arg") no-indent)
           ((and (parent-is "comment") c-ts-mode--looking-at-star)
            c-ts-mode--comment-start-after-first-star -1)
           ((parent-is "comment") prev-adaptive-prefix 0)
           ((node-is "labeled_statement") parent-bol 0)
           ((parent-is "labeled_statement") parent-bol c-ts-mode-indent-offset)
           ((match "preproc_ifdef" "compound_statement") point-min 0)
           ((match "#endif" "preproc_ifdef") point-min 0)
           ((match "preproc_if" "compound_statement") point-min 0)
           ((match "#endif" "preproc_if") point-min 0)
           ((match "preproc_function_def" "compound_statement") point-min 0)
           ((match "preproc_call" "compound_statement") point-min 0)
           ((parent-is "compound_statement")
            c-ts-mode--bracket-children-anchor c-ts-mode-indent-offset)
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
           ,@(when (eq mode 'cpp)
               '(((node-is "access_specifier") parent-bol 0)))
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

(defun c-ts-mode--bracket-children-anchor (_n parent &rest _)
  "This anchor is used for children of a compound_statement.
So anything inside a {} block.  PARENT should be the
compound_statement.  This anchor looks at the {, if itson its own
line, anchor at it, if it has stuff before it, anchor at the
beginning of grandparent."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (let ((bol (line-beginning-position)))
      (skip-chars-backward " \t")
      (treesit-node-start
       (if (< bol (point))
           (treesit-node-parent parent)
         parent)))))

(defun c-ts-mode--looking-at-star (&rest _)
  "A tree-sitter simple indent matcher.
Matches if there is a \"*\" after point (ignoring whitespace in
between)."
  (looking-at (rx (* (syntax whitespace)) "*")))

(defun c-ts-mode--comment-start-after-first-star (_n parent &rest _)
  "A tree-sitter simple indent anchor.
Finds the \"/*\" and returns the point after the \"*\".
Assumes PARENT is a comment node."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (if (looking-at (rx "/*"))
        (match-end 0)
      (point))))

;;; Font-lock

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
           "extern" "for" "goto" "if" "inline"
           "register" "return"
           "sizeof" "static" "struct"
           "switch" "typedef" "union"
           "volatile" "while")))
    (if (eq mode 'cpp)
        (append c-keywords
                '("and" "and_eq" "bitand" "bitor"
                  "catch" "class" "co_await" "co_return"
                  "co_yield" "compl" "concept" "consteval"
                  "constexpr" "constinit" "decltype" "delete"
                  "explicit" "final" "friend"
                  "mutable" "namespace" "new" "noexcept"
                  "not" "not_eq" "operator" "or"
                  "or_eq" "override" "private" "protected"
                  "public" "requires" "template" "throw"
                  "try" "typename" "using" "virtual"
                  "xor" "xor_eq"))
      (append '("auto") c-keywords))))

(defvar c-ts-mode--type-keywords
  '("long" "short" "signed" "unsigned")
  "Keywords that should be considered as part of a type.")

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
         '((nullptr) @font-lock-constant-face)))

   :language mode
   :feature 'keyword
   `([,@(c-ts-mode--keywords mode)] @font-lock-keyword-face
     ,@(when (eq mode 'cpp)
         '((auto) @font-lock-keyword-face
           (this) @font-lock-keyword-face)))

   :language mode
   :feature 'operator
   `([,@c-ts-mode--operators] @font-lock-operator-face
     "!" @font-lock-negation-char-face)

   :language mode
   :feature 'string
   `((string_literal) @font-lock-string-face
     (system_lib_string) @font-lock-string-face
     ,@(when (eq mode 'cpp)
         '((raw_string_literal) @font-lock-string-face)))

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

           (operator_cast) type: (type_identifier) @font-lock-type-face))
     [,@c-ts-mode--type-keywords] @font-lock-type-face)

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
   ;; expressions, see `c-ts-mode--fontify-declarator' for
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
   '((ERROR) @c-ts-mode--fontify-error)

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

;;; Font-lock helpers

(defun c-ts-mode--declarator-identifier (node)
  "Return the identifier of the declarator node NODE."
  (pcase (treesit-node-type node)
    ;; Recurse.
    ((or "attributed_declarator" "parenthesized_declarator")
     (c-ts-mode--declarator-identifier (treesit-node-child node 0 t)))
    ((or "pointer_declarator" "reference_declarator")
     (c-ts-mode--declarator-identifier (treesit-node-child node -1)))
    ((or "function_declarator" "array_declarator" "init_declarator")
     (c-ts-mode--declarator-identifier
      (treesit-node-child-by-field-name node "declarator")))
    ("qualified_identifier"
     (c-ts-mode--declarator-identifier
      (treesit-node-child-by-field-name node "name")))
    ;; Terminal case.
    ((or "identifier" "field_identifier")
     node)))

(defun c-ts-mode--fontify-declarator (node override start end &rest _args)
  "Fontify a declarator (whatever under the \"declarator\" field).
For NODE, OVERRIDE, START, END, and ARGS, see
`treesit-font-lock-rules'."
  (let* ((identifier (c-ts-mode--declarator-identifier node))
         (qualified-root
          (treesit-parent-while (treesit-node-parent identifier)
                                (lambda (node)
                                  (equal (treesit-node-type node)
                                         "qualified_identifier"))))
         (face (pcase (treesit-node-type (treesit-node-parent
                                          (or qualified-root
                                              identifier)))
                 ("function_declarator" 'font-lock-function-name-face)
                 (_ 'font-lock-variable-name-face))))
    (treesit-fontify-with-override
     (treesit-node-start identifier) (treesit-node-end identifier)
     face override start end)))

(defun c-ts-mode--fontify-variable (node override start end &rest _)
  "Fontify an identifier node if it is a variable.
Don't fontify if it is a function identifier.  For NODE,
OVERRIDE, START, END, and ARGS, see `treesit-font-lock-rules'."
  (when (not (equal (treesit-node-type
                     (treesit-node-parent node))
                    "call_expression"))
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     'font-lock-variable-name-face override start end)))

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
         'default override start end)))
    ;; Fix the parameter list.
    (while arg-list-2
      (let ((type (and arg-list-2 (pop arg-list-2)))
            (arg (and arg-list-2 (pop arg-list-2))))
        (when type
          (treesit-fontify-with-override
           (treesit-node-start type) (treesit-node-end type)
           'font-lock-type-face override start end))
        (when arg
          (treesit-fontify-with-override
           (treesit-node-start arg) (treesit-node-end arg)
           'default override start end))))))

(defun c-ts-mode--fontify-error (node override start end &rest _)
  "Fontify the error nodes.
For NODE, OVERRIDE, START, and END, see
`treesit-font-lock-rules'."
  (let ((parent (treesit-node-parent node))
        (child (treesit-node-child node 0)))
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     (cond
      ;; This matches the case MACRO(struct a, b, c)
      ;; where struct is seen as error.
      ((and (equal (treesit-node-type child) "identifier")
            (equal (treesit-node-type parent) "argument_list")
            (member (treesit-node-text child)
                    '("struct" "long" "short" "enum" "union")))
       'font-lock-keyword-face)
      (t 'font-lock-warning-face))
     override start end)))

;;; Imenu

(defun c-ts-mode--defun-name (node)
  "Return the name of the defun NODE.
Return nil if NODE is not a defun node or doesn't have a name."
  (treesit-node-text
   (pcase (treesit-node-type node)
     ((or "function_definition" "declaration")
      (c-ts-mode--declarator-identifier
       (treesit-node-child-by-field-name node "declarator")))
     ((or "struct_specifier" "enum_specifier"
          "union_specifier" "class_specifier")
      (treesit-node-child-by-field-name node "name")))
   t))

;;; Defun navigation

(defun c-ts-mode--defun-valid-p (node)
  "Return non-nil if NODE is a valid defun node.
Ie, NODE is not nested."
  (not (or (and (member (treesit-node-type node)
                        '("struct_specifier"
                          "enum_specifier"
                          "union_specifier"
                          "declaration"))
                ;; If NODE's type is one of the above, make sure it is
                ;; top-level.
                (treesit-node-top-level
                 node (rx (or "function_definition"
                              "type_definition"
                              "struct_specifier"
                              "enum_specifier"
                              "union_specifier"
                              "declaration"))))

           (and (equal (treesit-node-type node) "declaration")
                ;; If NODE is a declaration, make sure it is not a
                ;; function declaration.
                (equal (treesit-node-type
                        (treesit-node-child-by-field-name
                         node "declarator"))
                       "function_declarator")))))

(defun c-ts-mode--defun-skipper ()
  "Custom defun skipper for `c-ts-mode' and friends.
Structs in C ends with a semicolon, but the semicolon is not
considered part of the struct node, so point would stop before
the semicolon.  This function skips the semicolon."
  (when (looking-at (rx (* (or " " "\t")) ";"))
    (goto-char (match-end 0)))
  (treesit-default-defun-skipper))

(defun c-ts-mode-indent-defun ()
  "Indent the current top-level declaration syntactically.

`treesit-defun-type-regexp' defines what constructs to indent."
  (interactive "*")
  (when-let ((orig-point (point-marker))
             (node (treesit-defun-at-point)))
    (indent-region (treesit-node-start node)
                   (treesit-node-end node))
    (goto-char orig-point)))

;;; Filling

(defun c-ts-mode--fill-paragraph (&optional arg)
  "Fillling function for `c-ts-mode'.
ARG is passed to `fill-paragraph'."
  (interactive "*P")
  (save-restriction
    (widen)
    (let* ((node (treesit-node-at (point)))
           (start (treesit-node-start node))
           (end (treesit-node-end node))
           ;; Bind to nil to avoid infinite recursion.
           (fill-paragraph-function nil)
           (orig-point (point-marker))
           (start-marker nil)
           (end-marker nil)
           (end-len 0))
      ;; These covers C/C++, Java, JavaScript, TypeScript, Rust, C#.
      (when (member (treesit-node-type node)
                    '("comment" "line_comment" "block_comment"))
        ;; We mask "/*" and the space before "*/" like
        ;; `c-fill-paragraph' does.
        (atomic-change-group
          ;; Mask "/*".
          (goto-char start)
          (when (looking-at (rx (* (syntax whitespace))
                                (group "/") "*"))
            (goto-char (match-beginning 1))
            (setq start-marker (point-marker))
            (replace-match " " nil nil nil 1))
          ;; Include whitespaces before /*.
          (goto-char start)
          (beginning-of-line)
          (setq start (point))
          ;; Mask spaces before "*/" if it is attached at the end
          ;; of a sentence rather than on its own line.
          (goto-char end)
          (when (looking-back (rx (not (syntax whitespace))
                                  (group (+ (syntax whitespace)))
                                  "*/")
                              (line-beginning-position))
            (goto-char (match-beginning 1))
            (setq end-marker (point-marker))
            (setq end-len (- (match-end 1) (match-beginning 1)))
            (replace-match (make-string end-len ?x)
                           nil nil nil 1))
          ;; If "*/" is on its own line, don't included it in the
          ;; filling region.
          (when (not end-marker)
            (goto-char end)
            (when (looking-back (rx "*/") 2)
              (backward-char 2)
              (skip-syntax-backward "-")
              (setq end (point))))
          ;; Let `fill-paragraph' do its thing.
          (goto-char orig-point)
          (narrow-to-region start end)
          (funcall #'fill-paragraph arg)
          ;; Unmask.
          (when start-marker
            (goto-char start-marker)
            (delete-char 1)
            (insert "/"))
          (when end-marker
            (goto-char end-marker)
            (delete-region (point) (+ end-len (point)))
            (insert (make-string end-len ?\s))))
        (goto-char orig-point))
      ;; Return t so `fill-paragraph' doesn't attempt to fill by
      ;; itself.
      t)))

(defun c-ts-mode-comment-setup ()
  "Set up local variables for C-like comment.

Set up:
 - `comment-start'
 - `comment-end'
 - `comment-start-skip'
 - `comment-end-skip'
 - `adaptive-fill-mode'
 - `adaptive-fill-first-line-regexp'
 - `paragraph-start'
 - `paragraph-separate'
 - `fill-paragraph-function'"
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (or (seq "/" (+ "/"))
                                         (seq "/" (+ "*")))
                                     (* (syntax whitespace))))
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end)
                             (seq (+ "*") "/")))))
  (setq-local adaptive-fill-mode t)
  ;; This matches (1) empty spaces (the default), (2) "//", (3) "*",
  ;; but do not match "/*", because we don't want to use "/*" as
  ;; prefix when filling.  (Actually, it doesn't matter, because
  ;; `comment-start-skip' matches "/*" which will cause
  ;; `fill-context-prefix' to use "/*" as a prefix for filling, that's
  ;; why we mask the "/*" in `c-ts-mode--fill-paragraph'.)
  (setq-local adaptive-fill-regexp
              (concat (rx (* (syntax whitespace))
                          (group (or (seq "/" (+ "/")) (* "*"))))
                      adaptive-fill-regexp))
  ;; Note the missing * comparing to `adaptive-fill-regexp'.  The
  ;; reason for its absence is a bit convoluted to explain.  Suffice
  ;; to say that without it, filling a single line paragraph that
  ;; starts with /* doesn't insert * at the beginning of each
  ;; following line, and filling a multi-line paragraph whose first
  ;; two lines start with * does insert * at the beginning of each
  ;; following line.  If you know how does adaptive filling works, you
  ;; know what I mean.
  (setq-local adaptive-fill-first-line-regexp
              (rx bos
                  (seq (* (syntax whitespace))
                       (group (seq "/" (+ "/")))
                       (* (syntax whitespace)))
                  eos))
  ;; Same as `adaptive-fill-regexp'.
  (setq-local paragraph-start
              (rx (or (seq (* (syntax whitespace))
                           (group (or (seq "/" (+ "/")) (* "*")))
                           (* (syntax whitespace))
                           ;; Add this eol so that in
                           ;; `fill-context-prefix', `paragraph-start'
                           ;; doesn't match the prefix.
                           eol)
                      "\f")))
  (setq-local paragraph-separate paragraph-start)
  (setq-local fill-paragraph-function #'c-ts-mode--fill-paragraph))

;;; Modes

(defvar-keymap c-ts-mode-map
  :doc "Keymap for the C language with tree-sitter"
  :parent prog-mode-map
  "C-c C-q" #'c-ts-mode-indent-defun)

;;;###autoload
(define-derived-mode c-ts-base-mode prog-mode "C"
  "Major mode for editing C, powered by tree-sitter.

\\{c-ts-mode-map}"
  :syntax-table c-ts-mode--syntax-table

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (cons (regexp-opt '("function_definition"
                                  "type_definition"
                                  "struct_specifier"
                                  "enum_specifier"
                                  "union_specifier"
                                  "class_specifier"))
                    #'c-ts-mode--defun-valid-p))
  (setq-local treesit-defun-skipper #'c-ts-mode--defun-skipper)
  (setq-local treesit-defun-name-function #'c-ts-mode--defun-name)

  ;; Nodes like struct/enum/union_specifier can appear in
  ;; function_definitions, so we need to find the top-level node.
  (setq-local treesit-defun-prefer-top-level t)

  ;; Indent.
  (when (eq c-ts-mode-indent-style 'linux)
    (setq-local indent-tabs-mode t))

  ;; Comment
  (c-ts-mode-comment-setup)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              (let ((pred #'c-ts-mode--defun-valid-p))
                `(("Struct" ,(rx bos (or "struct" "enum" "union")
                                 "_specifier" eos)
                   ,pred nil)
                  ("Variable" ,(rx bos "declaration" eos) ,pred nil)
                  ("Function" "\\`function_definition\\'" ,pred nil)
                  ("Class" ,(rx bos (or "class_specifier"
                                        "function_definition")
                                eos)
                   ,pred nil))))

  (setq-local treesit-font-lock-feature-list
              '(( comment definition)
                ( keyword preprocessor string type)
                ( assignment constant escape-sequence label literal property )
                ( bracket delimiter error function operator variable))))

;;;###autoload
(define-derived-mode c-ts-mode c-ts-base-mode "C"
  "Major mode for editing C, powered by tree-sitter."
  :group 'c

  (unless (treesit-ready-p 'c)
    (error "Tree-sitter for C isn't available"))

  (treesit-parser-create 'c)

  ;; Comments.
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")

  (setq-local treesit-simple-indent-rules
              (c-ts-mode--set-indent-style 'c))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (c-ts-mode--font-lock-settings 'c))

  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode c++-ts-mode c-ts-base-mode "C++"
  "Major mode for editing C++, powered by tree-sitter."
  :group 'c++

  (unless (treesit-ready-p 'cpp)
    (error "Tree-sitter for C++ isn't available"))

  (treesit-parser-create 'cpp)
  (setq-local syntax-propertize-function
              #'c-ts-mode--syntax-propertize)

  (setq-local treesit-simple-indent-rules
              (c-ts-mode--set-indent-style 'cpp))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (c-ts-mode--font-lock-settings 'cpp))

  (treesit-major-mode-setup))

(provide 'c-ts-mode)

;;; c-ts-mode.el ends here
