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
;; This package provides major modes for C and C++, plus some handy
;; functions that are useful generally to major modes for C-like
;; languages.
;;
;; This package provides `c-ts-mode' for C, `c++-ts-mode' for C++, and
;; `c-or-c++-ts-mode' which automatically chooses the right mode for
;; C/C++ header files.
;;
;; To use these modes by default, assuming you have the respective
;; tree-sitter grammars available, do one of the following:
;;
;; - If you have both C and C++ grammars installed, add
;;
;;    (require 'c-ts-mode)
;;
;;   to your init file.
;;
;; - Add one or mode of the following to your init file:
;;
;;    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;;    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;;    (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
;;
;;   If you have only C grammar available, use only the first one; if
;;   you have only the C++ grammar, use only the second one.
;;
;; - Customize 'auto-mode-alist' to turn one or more of the modes
;;   automatically.  For example:
;;
;;     (add-to-list 'auto-mode-alist
;;                  '("\\(\\.ii\\|\\.\\(CC?\\|HH?\\)\\|\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\|\\.\\(cc\\|hh\\)\\)\\'"
;;                    . c++-ts-mode))
;;
;;   will turn on the c++-ts-mode for C++ source files.
;;
;; You can also turn on these modes manually in a buffer.  Doing so
;; will set up Emacs to use the C/C++ modes defined here for other
;; files, provided that you have the corresponding parser grammar
;; libraries installed.

;;; Code:

(require 'treesit)
(require 'c-ts-common)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-first-child-for-pos "treesit.c")

;;; Custom variables

(defcustom c-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `c-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'c)

(defun c-ts-mode-toggle-comment-style (&optional arg)
  "Toggle the comment style between block and line comments.
Optional numeric ARG, if supplied, switches to block comment
style when positive, to line comment style when negative, and
just toggles it when zero or left out."
  (interactive "P")
  (let ((prevstate-line (string= comment-start "// ")))
    (when (or (not arg)
              (zerop (setq arg (prefix-numeric-value arg)))
              (xor (> 0 arg) prevstate-line))
      (pcase-let ((`(,starter . ,ender)
                   (if prevstate-line
                       (cons "/* " " */")
                     (cons "// " ""))))
        (setq-local comment-start starter
                    comment-end ender))
      (c-ts-mode-set-modeline))))

(defun c-ts-mode-set-modeline ()
  (setq mode-name
        (concat (if (eq major-mode 'c-ts-mode) "C" "C++")
                (string-trim-right comment-start)))
  (force-mode-line-update))

(defun c-ts-mode--indent-style-setter (sym val)
  "Custom setter for `c-ts-mode-set-style'.

Apart from setting the default value of SYM to VAL, also change
the value of SYM in `c-ts-mode' and `c++-ts-mode' buffers to VAL.

SYM should be `c-ts-mode-indent-style', and VAL should be a style
symbol."
  (set-default sym val)
  (named-let loop ((res nil)
                   (buffers (buffer-list)))
    (if (null buffers)
        (mapc (lambda (b)
                (with-current-buffer b
                  (c-ts-mode-set-style val)))
              res)
      (let ((buffer (car buffers)))
        (with-current-buffer buffer
          (if (derived-mode-p 'c-ts-mode 'c++-ts-mode)
              (loop (append res (list buffer)) (cdr buffers))
            (loop res (cdr buffers))))))))

(defcustom c-ts-mode-indent-style 'gnu
  "Style used for indentation.

The selected style could be one of GNU, K&R, LINUX or BSD.  If
one of the supplied styles doesn't suffice, a function could be
set instead.  This function is expected to return a list that
follows the form of `treesit-simple-indent-rules'."
  :version "29.1"
  :type '(choice (symbol :tag "Gnu" gnu)
                 (symbol :tag "K&R" k&r)
                 (symbol :tag "Linux" linux)
                 (symbol :tag "BSD" bsd)
                 (function :tag "A function for user customized style" ignore))
  :set #'c-ts-mode--indent-style-setter
  :group 'c)

(defun c-ts-mode--get-indent-style (mode)
  "Helper function to set indentation style.
MODE is either `c' or `cpp'."
  (let ((style
         (if (functionp c-ts-mode-indent-style)
             (funcall c-ts-mode-indent-style)
           (alist-get c-ts-mode-indent-style (c-ts-mode--indent-styles mode)))))
    `((,mode ,@style))))

(defun c-ts-mode--prompt-for-style ()
  "Prompt for an indent style and return the symbol for it."
  (let ((mode (if (derived-mode-p 'c-ts-mode) 'c 'c++)))
    (intern
     (completing-read
      "Style: "
      (mapcar #'car (c-ts-mode--indent-styles mode))
      nil t nil nil "gnu"))))

(defun c-ts-mode-set-global-style (style)
  "Set the indent style of C/C++ modes globally to STYLE.

This changes the current indent style of every C/C++ buffer and
the default C/C++ indent style for `c-ts-mode' and `c++-ts-mode'
in this Emacs session."
  (interactive (list (c-ts-mode--prompt-for-style)))
  (c-ts-mode--indent-style-setter 'c-ts-mode-indent-style style))

(defun c-ts-mode-set-style (style)
  "Set the C/C++ indent style of the current buffer to STYLE.

To set the default indent style globally, use
`c-ts-mode-set-global-style'."
  (interactive (list (c-ts-mode--prompt-for-style)))
  (if (not (derived-mode-p 'c-ts-mode 'c++-ts-mode))
      (user-error "The current buffer is not in `c-ts-mode' nor `c++-ts-mode'")
    (setq-local c-ts-mode-indent-style style)
    (setq treesit-simple-indent-rules
          (treesit--indent-rules-optimize
           (c-ts-mode--get-indent-style
            (if (derived-mode-p 'c-ts-mode) 'c 'cpp))))))

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

(defun c-ts-mode--preproc-offset (_n _p &rest _)
  "This anchor is used for preprocessor directives.

Because node is nil at the moment of indentation, we use
`treesit-node-on' to capture the anonymous node covering the
newline.  If the grand-parent of that node is the
translation_unit itself, we don't indent.  Otherwise, just indent
one step according to the great-grand-parent indent level.  The
reason there is a difference between grand-parent and
great-grand-parent here is that the node containing the newline
is actually the parent of point at the moment of indentation."
  (when-let ((node (treesit-node-on (point) (point))))
    (if (string-equal "translation_unit"
                      (treesit-node-type
                       (treesit-node-parent
                        (treesit-node-parent node))))
        0
      c-ts-mode-indent-offset)))

(defun c-ts-mode--anchor-prev-sibling (node parent bol &rest _)
  "Return the start of the previous named sibling of NODE.

This anchor handles the special case where the previous sibling
is a labeled_statement, in that case, return the child of the
labeled statement instead.  (Actually, recursively go down until
the node isn't a labeled_statement.)  Eg,

label:
  int x = 1;
  int y = 2;

The anchor of \"int y = 2;\" should be \"int x = 1;\" rather than
the labeled_statement.

Return nil if a) there is no prev-sibling, or 2) prev-sibling
doesn't have a child.

PARENT and BOL are like other anchor functions."
  (when-let ((prev-sibling
              (or (treesit-node-prev-sibling node t)
                  (treesit-node-prev-sibling
                   (treesit-node-first-child-for-pos parent bol) t)
                  (treesit-node-child parent -1 t)))
             (continue t))
    (while (and prev-sibling continue)
      (pcase (treesit-node-type prev-sibling)
        ;; Get the statement in the label.
        ("labeled_statement"
         (setq prev-sibling (treesit-node-child prev-sibling 2)))
        ;; Get the last statement in the preproc.  Tested by
        ;; "Prev-Sibling When Prev-Sibling is Preproc" test.
        ((or "preproc_if" "preproc_ifdef" "preproc_elif" "preproc_else")
         (setq prev-sibling (treesit-node-child prev-sibling -2)))
        ;; Don't do anything special.
        (_ (setq continue nil))))
    ;; This could be nil if a) there is no prev-sibling or b)
    ;; prev-sibling doesn't have a child.
    (treesit-node-start prev-sibling)))

(defun c-ts-mode--standalone-grandparent (_node parent bol &rest args)
  "Like the standalone-parent anchor but pass it the grandparent.
PARENT, BOL, ARGS are the same as other anchor functions."
  (apply (alist-get 'standalone-parent treesit-simple-indent-presets)
         parent (treesit-node-parent parent) bol args))

(defun c-ts-mode--indent-styles (mode)
  "Indent rules supported by `c-ts-mode'.
MODE is either `c' or `cpp'."
  (let ((common
         `(((parent-is "translation_unit") point-min 0)
           ((query "(ERROR (ERROR)) @indent") point-min 0)
           ((node-is ")") parent 1)
           ((node-is "]") parent-bol 0)
           ((node-is "else") parent-bol 0)
           ((node-is "case") parent-bol 0)
           ((node-is "preproc_arg") no-indent)
           ;; `c-ts-common-looking-at-star' has to come before
           ;; `c-ts-common-comment-2nd-line-matcher'.
           ((and (parent-is "comment") c-ts-common-looking-at-star)
            c-ts-common-comment-start-after-first-star -1)
           (c-ts-common-comment-2nd-line-matcher
            c-ts-common-comment-2nd-line-anchor
            1)
           ((parent-is "comment") prev-adaptive-prefix 0)

           ;; Labels.
           ((node-is "labeled_statement") standalone-parent 0)
           ((parent-is "labeled_statement")
            c-ts-mode--standalone-grandparent c-ts-mode-indent-offset)

           ((node-is "preproc") point-min 0)
           ((node-is "#endif") point-min 0)
           ((match "preproc_call" "compound_statement") point-min 0)

           ((n-p-gp nil "preproc" "translation_unit") point-min 0)
           ((n-p-gp nil "\n" "preproc") great-grand-parent c-ts-mode--preproc-offset)
           ((parent-is "preproc") grand-parent c-ts-mode-indent-offset)

           ((parent-is "function_definition") parent-bol 0)
           ((parent-is "conditional_expression") first-sibling 0)
           ((parent-is "assignment_expression") parent-bol c-ts-mode-indent-offset)
           ((parent-is "concatenated_string") parent-bol c-ts-mode-indent-offset)
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
           ;; Closing bracket.  This should be before initializer_list
           ;; (and probably others) rule because that rule (and other
           ;; similar rules) will match the closing bracket.  (Bug#61398)
           ((node-is "}") standalone-parent 0)
           ,@(when (eq mode 'cpp)
               '(((node-is "access_specifier") parent-bol 0)
                 ;; Indent the body of namespace definitions.
                 ((parent-is "declaration_list") parent-bol c-ts-mode-indent-offset)))


           ;; int[5] a = { 0, 0, 0, 0 };
           ((match nil "initializer_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
           ((parent-is "initializer_list") c-ts-mode--anchor-prev-sibling 0)
           ;; Statement in enum.
           ((match nil "enumerator_list" nil 1 1) standalone-parent c-ts-mode-indent-offset)
           ((parent-is "enumerator_list") c-ts-mode--anchor-prev-sibling 0)
           ;; Statement in struct and union.
           ((match nil "field_declaration_list" nil 1 1) standalone-parent c-ts-mode-indent-offset)
           ((parent-is "field_declaration_list") c-ts-mode--anchor-prev-sibling 0)

           ;; Statement in {} blocks.
           ((match nil "compound_statement" nil 1 1) standalone-parent c-ts-mode-indent-offset)
           ((parent-is "compound_statement") c-ts-mode--anchor-prev-sibling 0)
           ;; Opening bracket.
           ((node-is "compound_statement") standalone-parent c-ts-mode-indent-offset)
           ;; Bug#61291.
           ((match "expression_statement" nil "body") standalone-parent c-ts-mode-indent-offset)
           ;; These rules are for cases where the body is bracketless.
           ;; Tested by the "Bracketless Simple Statement" test.
           ((parent-is "if_statement") standalone-parent c-ts-mode-indent-offset)
           ((parent-is "for_statement") standalone-parent c-ts-mode-indent-offset)
           ((parent-is "while_statement") standalone-parent c-ts-mode-indent-offset)
           ((parent-is "do_statement") standalone-parent c-ts-mode-indent-offset)

           ,@(when (eq mode 'cpp)
               `(((node-is "field_initializer_list") parent-bol ,(* c-ts-mode-indent-offset 2)))))))
    `((gnu
       ;; Prepend rules to set highest priority
       ((match "while" "do_statement") parent 0)
       (c-ts-mode--top-level-label-matcher point-min 1)
       ,@common)
      (k&r ,@common)
      (linux
       ;; Reference:
       ;; https://www.kernel.org/doc/html/latest/process/coding-style.html,
       ;; and script/Lindent in Linux kernel repository.
       ((node-is "labeled_statement") point-min 0)
       ,@common)
      (bsd
       ((node-is "}") parent-bol 0)
       ((node-is "labeled_statement") parent-bol c-ts-mode-indent-offset)
       ((parent-is "labeled_statement") parent-bol c-ts-mode-indent-offset)
       ((parent-is "compound_statement") parent-bol c-ts-mode-indent-offset)
       ((parent-is "if_statement") parent-bol 0)
       ((parent-is "for_statement") parent-bol 0)
       ((parent-is "while_statement") parent-bol 0)
       ((parent-is "switch_statement") parent-bol 0)
       ((parent-is "case_statement") parent-bol 0)
       ((parent-is "do_statement") parent-bol 0)
       ,@common))))

(defun c-ts-mode--top-level-label-matcher (node parent &rest _)
  "A matcher that matches a top-level label.
NODE should be a labeled_statement.  PARENT is its parent."
  (and (equal (treesit-node-type node)
              "labeled_statement")
       (equal "function_definition"
              (treesit-node-type (treesit-node-parent parent)))))

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
      declarator: (_) @c-ts-mode--fontify-declarator)

     (parameter_declaration
      declarator: (_) @c-ts-mode--fontify-declarator)

     (enumerator
      name: (identifier) @font-lock-property-name-face))

   :language mode
   :feature 'assignment
   ;; TODO: Recursively highlight identifiers in parenthesized
   ;; expressions, see `c-ts-mode--fontify-declarator' for
   ;; inspiration.
   '((assignment_expression
      left: (identifier) @font-lock-variable-name-face)
     (assignment_expression
      left: (field_expression field: (_) @font-lock-property-use-face))
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
      function:
      [(identifier) @font-lock-function-call-face
       (field_expression field: (field_identifier) @font-lock-function-call-face)]))

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
   '((field_identifier) @font-lock-property-use-face)

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

(defun c-ts-mode--declarator-identifier (node &optional qualified)
  "Return the identifier of the declarator node NODE.

If QUALIFIED is non-nil, include the names space part of the
identifier and return a qualified_identifier."
  (pcase (treesit-node-type node)
    ;; Recurse.
    ((or "attributed_declarator" "parenthesized_declarator")
     (c-ts-mode--declarator-identifier (treesit-node-child node 0 t)
                                       qualified))
    ((or "pointer_declarator" "reference_declarator")
     (c-ts-mode--declarator-identifier (treesit-node-child node -1)
                                       qualified))
    ((or "function_declarator" "array_declarator" "init_declarator")
     (c-ts-mode--declarator-identifier
      (treesit-node-child-by-field-name node "declarator")
      qualified))
    ("qualified_identifier"
     (if qualified
         node
       (c-ts-mode--declarator-identifier
        (treesit-node-child-by-field-name node "name")
        qualified)))
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
                 ("field_declaration" 'font-lock-property-name-face)
                 ("function_declarator" 'font-lock-function-name-face)
                 (_ 'font-lock-variable-name-face))))
    (when identifier
      (treesit-fontify-with-override
       (treesit-node-start identifier) (treesit-node-end identifier)
       face override start end))))

(defun c-ts-mode--fontify-variable (node override start end &rest _)
  "Fontify an identifier node if it is a variable.
Don't fontify if it is a function identifier.  For NODE,
OVERRIDE, START, END, and ARGS, see `treesit-font-lock-rules'."
  (when (not (equal (treesit-node-type
                     (treesit-node-parent node))
                    "call_expression"))
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     'font-lock-variable-use-face override start end)))

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
       (treesit-node-child-by-field-name node "declarator")
       t))
     ((or "struct_specifier" "enum_specifier"
          "union_specifier" "class_specifier"
          "namespace_definition")
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

(defun c-ts-mode--defun-for-class-in-imenu-p (node)
  "Check if NODE is a valid entry for the Class subindex.

Basically, if NODE is a class, return non-nil; if NODE is a
function but is under a class, return non-nil; if NODE is a
top-level function, return nil.

This is for the Class subindex in
`treesit-simple-imenu-settings'."
  (pcase (treesit-node-type node)
    ;; The Class subindex only has class_specifier and
    ;; function_definition.
    ("class_specifier" t)
    ("function_definition"
     ;; Return t if this function is nested in a class.
     (treesit-node-top-level node "class_specifier"))))

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

;;; Modes

(defvar-keymap c-ts-base-mode-map
  :doc "Keymap for C and C-like languages with tree-sitter"
  :parent prog-mode-map
  "C-c C-q" #'c-ts-mode-indent-defun
  "C-c ." #'c-ts-mode-set-style
  "C-c C-c" #'comment-region
  "C-c C-k" #'c-ts-mode-toggle-comment-style)

;;;###autoload
(define-derived-mode c-ts-base-mode prog-mode "C"
  "Major mode for editing C, powered by tree-sitter.

\\{c-ts-base-mode-map}"
  :syntax-table c-ts-mode--syntax-table

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (cons (regexp-opt '("function_definition"
                                  "type_definition"
                                  "struct_specifier"
                                  "enum_specifier"
                                  "union_specifier"
                                  "class_specifier"
                                  "namespace_definition"))
                    #'c-ts-mode--defun-valid-p))
  (setq-local treesit-defun-skipper #'c-ts-mode--defun-skipper)
  (setq-local treesit-defun-name-function #'c-ts-mode--defun-name)

  (setq-local treesit-sentence-type-regexp
              ;; compound_statement makes us jump over too big units
              ;; of code, so skip that one, and include the other
              ;; statements.
              (regexp-opt '("preproc"
                            "declaration"
                            "specifier"
                            "attributed_statement"
                            "labeled_statement"
                            "expression_statement"
                            "if_statement"
                            "switch_statement"
                            "do_statement"
                            "while_statement"
                            "for_statement"
                            "return_statement"
                            "break_statement"
                            "continue_statement"
                            "goto_statement"
                            "case_statement")))

  (setq-local treesit-sexp-type-regexp
              (regexp-opt '("preproc"
                            "declarator"
                            "qualifier"
                            "type"
                            "parameter"
                            "expression"
                            "literal"
                            "string")))

  ;; Nodes like struct/enum/union_specifier can appear in
  ;; function_definitions, so we need to find the top-level node.
  (setq-local treesit-defun-prefer-top-level t)

  ;; Indent.
  (when (eq c-ts-mode-indent-style 'linux)
    (setq-local indent-tabs-mode t))
  (setq-local c-ts-common-indent-offset 'c-ts-mode-indent-offset)
  ;; This setup is not needed anymore, but we might find uses for it
  ;; later, so I'm keeping it.
  (setq-local c-ts-common-indent-type-regexp-alist
              `((block . ,(rx (or "compound_statement"
                                  "field_declaration_list"
                                  "enumerator_list"
                                  "initializer_list"
                                  "declaration_list")))
                (if . "if_statement")
                (else . ("if_statement" . "alternative"))
                (do . "do_statement")
                (while . "while_statement")
                (for . "for_statement")
                (close-bracket . "}")))
  ;; Comment
  (c-ts-common-comment-setup)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;,#" electric-indent-chars))

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
                   c-ts-mode--defun-for-class-in-imenu-p nil))))

  (setq-local treesit-font-lock-feature-list
              '(( comment definition)
                ( keyword preprocessor string type)
                ( assignment constant escape-sequence label literal)
                ( bracket delimiter error function operator property variable))))

;;;###autoload
(define-derived-mode c-ts-mode c-ts-base-mode "C"
  "Major mode for editing C, powered by tree-sitter.

This mode is independent from the classic cc-mode.el based
`c-mode', so configuration variables of that mode, like
`c-basic-offset', doesn't affect this mode.

To use tree-sitter C/C++ modes by default, evaluate

    (add-to-list \\='major-mode-remap-alist \\='(c-mode . c-ts-mode))
    (add-to-list \\='major-mode-remap-alist \\='(c++-mode . c++-ts-mode))
    (add-to-list \\='major-mode-remap-alist
                 \\='(c-or-c++-mode . c-or-c++-ts-mode))

in your configuration."
  :group 'c
  :after-hook (c-ts-mode-set-modeline)

  (when (treesit-ready-p 'c)
    (treesit-parser-create 'c)
    ;; Comments.
    (setq-local comment-start "/* ")
    (setq-local comment-end " */")
    ;; Indent.
    (setq-local treesit-simple-indent-rules
                (c-ts-mode--get-indent-style 'c))
    ;; Font-lock.
    (setq-local treesit-font-lock-settings (c-ts-mode--font-lock-settings 'c))
    ;; Navigation.
    (setq-local treesit-defun-tactic 'top-level)
    (treesit-major-mode-setup)))

;;;###autoload
(define-derived-mode c++-ts-mode c-ts-base-mode "C++"
  "Major mode for editing C++, powered by tree-sitter.

This mode is independent from the classic cc-mode.el based
`c++-mode', so configuration variables of that mode, like
`c-basic-offset', don't affect this mode.

To use tree-sitter C/C++ modes by default, evaluate

    (add-to-list \\='major-mode-remap-alist \\='(c-mode . c-ts-mode))
    (add-to-list \\='major-mode-remap-alist \\='(c++-mode . c++-ts-mode))
    (add-to-list \\='major-mode-remap-alist
                 \\='(c-or-c++-mode . c-or-c++-ts-mode))

in your configuration."
  :group 'c++
  :after-hook (c-ts-mode-set-modeline)

  (when (treesit-ready-p 'cpp)
    (setq-local treesit-text-type-regexp
                (regexp-opt '("comment"
                              "raw_string_literal")))

    (treesit-parser-create 'cpp)

    ;; Syntax.
    (setq-local syntax-propertize-function
                #'c-ts-mode--syntax-propertize)

    ;; Indent.
    (setq-local treesit-simple-indent-rules
                (c-ts-mode--get-indent-style 'cpp))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings (c-ts-mode--font-lock-settings 'cpp))

    (treesit-major-mode-setup)))

;; We could alternatively use parsers, but if this works well, I don't
;; see the need to change.  This is copied verbatim from cc-guess.el.
(defconst c-ts-mode--c-or-c++-regexp
  (eval-when-compile
    (let ((id "[a-zA-Z_][a-zA-Z0-9_]*") (ws "[ \t]+") (ws-maybe "[ \t]*")
          (headers '("string" "string_view" "iostream" "map" "unordered_map"
                     "set" "unordered_set" "vector" "tuple")))
      (concat "^" ws-maybe "\\(?:"
              "using"     ws "\\(?:namespace" ws
              "\\|" id "::"
              "\\|" id ws-maybe "=\\)"
              "\\|" "\\(?:inline" ws "\\)?namespace"
              "\\(:?" ws "\\(?:" id "::\\)*" id "\\)?" ws-maybe "{"
              "\\|" "class"     ws id
              "\\(?:" ws "final" "\\)?" ws-maybe "[:{;\n]"
              "\\|" "struct"     ws id "\\(?:" ws "final" ws-maybe "[:{\n]"
              "\\|" ws-maybe ":\\)"
              "\\|" "template"  ws-maybe "<.*?>"
              "\\|" "#include"  ws-maybe "<" (regexp-opt headers) ">"
              "\\)")))
  "A regexp applied to C header files to check if they are really C++.")

;;;###autoload
(defun c-or-c++-ts-mode ()
  "Analyze buffer and enable either C or C++ mode.

Some people and projects use .h extension for C++ header files
which is also the one used for C header files.  This makes
matching on file name insufficient for detecting major mode that
should be used.

This function attempts to use file contents to determine whether
the code is C or C++ and based on that chooses whether to enable
`c-ts-mode' or `c++-ts-mode'."
  (interactive)
  (if (save-excursion
        (save-restriction
          (save-match-data ; Why `save-match-data'?
            (widen)
            (goto-char (point-min))
            (re-search-forward c-ts-mode--c-or-c++-regexp nil t))))
      (c++-ts-mode)
    (c-ts-mode)))
;; The entries for C++ must come first to prevent *.c files be taken
;; as C++ on case-insensitive filesystems, since *.C files are C++,
;; not C.
(if (treesit-ready-p 'cpp)
    (add-to-list 'auto-mode-alist
                 '("\\(\\.ii\\|\\.\\(CC?\\|HH?\\)\\|\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\|\\.\\(cc\\|hh\\)\\)\\'"
                   . c++-ts-mode)))

(if (treesit-ready-p 'c)
    (add-to-list 'auto-mode-alist
                 '("\\(\\.[chi]\\|\\.lex\\|\\.y\\(acc\\)?\\|\\.x[bp]m\\)\\'"
                   . c-ts-mode)))

(if (and (treesit-ready-p 'cpp)
         (treesit-ready-p 'c))
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c-or-c++-ts-mode)))

(provide 'c-ts-mode)

;;; c-ts-mode.el ends here
