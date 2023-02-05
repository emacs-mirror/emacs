;;; rust-ts-mode.el --- tree-sitter support for Rust  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author     : Randy Taylor <dev@rjt.dev>
;; Maintainer : Randy Taylor <dev@rjt.dev>
;; Created    : December 2022
;; Keywords   : rust languages tree-sitter

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
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-query-compile "treesit.c")

(defcustom rust-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `rust-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'rust)

(defvar rust-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)
    (modify-syntax-entry ?^   "."      table)
    (modify-syntax-entry ?!   "."      table)
    (modify-syntax-entry ?@   "."      table)
    (modify-syntax-entry ?~   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    (modify-syntax-entry ?\n  "> b"    table)
    (modify-syntax-entry ?\^m "> b"    table)
    table)
  "Syntax table for `rust-ts-mode'.")

(defvar rust-ts-mode--indent-rules
  `((rust
     ((parent-is "source_file") point-min 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "arguments") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "await_expression") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "array_expression") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "binary_expression") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "block") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "declaration_list") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "enum_variant_list") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "field_declaration_list") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "field_expression") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "field_initializer_list") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "let_declaration") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "macro_definition") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "parameters") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "token_tree") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "use_list") parent-bol rust-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `rust-ts-mode'.")

(defvar rust-ts-mode--builtin-macros
  '("concat_bytes" "concat_idents" "const_format_args"
    "format_args_nl" "log_syntax" "trace_macros" "assert" "assert_eq"
    "assert_ne" "cfg" "column" "compile_error" "concat" "dbg"
    "debug_assert" "debug_assert_eq" "debug_assert_ne" "env" "eprint"
    "eprintln" "file" "format" "format_args" "include" "include_bytes"
    "include_str" "is_x86_feature_detected" "line" "matches"
    "module_path" "option_env" "panic" "print" "println" "stringify"
    "thread_local" "todo" "try" "unimplemented" "unreachable" "vec"
    "write" "writeln")
  "Rust built-in macros for tree-sitter font-locking.")

(defvar rust-ts-mode--keywords
  '("as" "async" "await" "break" "const" "continue" "dyn" "else"
    "enum" "extern" "fn" "for" "if" "impl" "in" "let" "loop" "match"
    "mod" "move" "pub" "ref" "return" "static" "struct" "trait" "type"
    "union" "unsafe" "use" "where" "while" (crate) (self) (super)
    (mutable_specifier))
  "Rust keywords for tree-sitter font-locking.")

(defvar rust-ts-mode--operators
  '("!"  "!=" "%" "%=" "&" "&=" "&&" "*" "*=" "+" "+=" "," "-" "-="
    "->" "."  ".."  "..=" "..."  "/" "/=" ":" ";" "<<" "<<=" "<" "<="
    "=" "==" "=>" ">" ">=" ">>" ">>=" "@" "^" "^=" "|" "|=" "||" "?")
  "Rust operators for tree-sitter font-locking.")

(defvar rust-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'rust
   :feature 'attribute
   '((attribute_item) @font-lock-constant-face
     (inner_attribute_item) @font-lock-constant-face)

   :language 'rust
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'rust
   :feature 'builtin
   `((macro_invocation
      macro: ((identifier) @font-lock-builtin-face
              (:match ,(rx-to-string
                        `(seq bol
                              (or ,@rust-ts-mode--builtin-macros)
                              eol))
                      @font-lock-builtin-face)))
     ((identifier) @font-lock-type-face
      (:match "^\\(:?Err\\|Ok\\|None\\|Some\\)$" @font-lock-type-face)))

   :language 'rust
   :feature 'comment
   '(([(block_comment) (line_comment)]) @font-lock-comment-face)

   :language 'rust
   :feature 'constant
   `((boolean_literal) @font-lock-constant-face
     ((identifier) @font-lock-constant-face
      (:match "^[A-Z][A-Z\\d_]*$" @font-lock-constant-face)))

   :language 'rust
   :feature 'delimiter
   '((["," "." ";" ":" "::"]) @font-lock-delimiter-face)

   :language 'rust
   :feature 'definition
   '((function_item name: (identifier) @font-lock-function-name-face)
     (macro_definition "macro_rules!" @font-lock-constant-face)
     (macro_definition (identifier) @font-lock-preprocessor-face)
     (field_declaration name: (field_identifier) @font-lock-property-face)
     (parameter) @rust-ts-mode--fontify-pattern
     (let_declaration) @rust-ts-mode--fontify-pattern
     (for_expression) @rust-ts-mode--fontify-pattern
     (let_condition) @rust-ts-mode--fontify-pattern
     (match_arm) @rust-ts-mode--fontify-pattern)

   :language 'rust
   :feature 'function
   '((call_expression
      function:
      [(identifier) @font-lock-function-name-face
       (field_expression
        field: (field_identifier) @font-lock-function-name-face)
       (scoped_identifier
        name: (identifier) @font-lock-function-name-face)])
     (generic_function
      function: [(identifier) @font-lock-function-name-face
                 (field_expression
                  field: (field_identifier) @font-lock-function-name-face)
                 (scoped_identifier
                  name: (identifier) @font-lock-function-name-face)])
     (macro_invocation macro: (identifier) @font-lock-preprocessor-face))

   :language 'rust
   :feature 'keyword
   `([,@rust-ts-mode--keywords] @font-lock-keyword-face)

   :language 'rust
   :feature 'number
   '([(float_literal) (integer_literal)] @font-lock-number-face)

   :language 'rust
   :feature 'operator
   `([,@rust-ts-mode--operators] @font-lock-operator-face)

   :language 'rust
   :feature 'string
   '([(char_literal)
      (raw_string_literal)
      (string_literal)] @font-lock-string-face)

   :language 'rust
   :feature 'type
   `((enum_variant name: (identifier) @font-lock-type-face)
     (match_arm
      pattern: (match_pattern (_ type: (identifier) @font-lock-type-face)))
     (match_arm
      pattern: (match_pattern
                (_ type: (scoped_identifier
                          path: (identifier) @font-lock-type-face))))
     (mod_item name: (identifier) @font-lock-constant-face)
     (primitive_type) @font-lock-type-face
     (type_identifier) @font-lock-type-face
     ((scoped_identifier name: (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     ((scoped_identifier path: (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     ((scoped_identifier
        (scoped_identifier
         path: (identifier) @font-lock-type-face))
      (:match "^[A-Z]" @font-lock-type-face))
     ((scoped_identifier
       path: [(identifier) @font-lock-type-face
              (scoped_identifier
               name: (identifier) @font-lock-type-face)])
      (:match "^[A-Z]" @font-lock-type-face))
     (scoped_type_identifier path: (identifier) @font-lock-type-face)
     (type_identifier) @font-lock-type-face
     (use_as_clause alias: (identifier) @font-lock-type-face)
     (use_list (identifier) @font-lock-type-face))

   :language 'rust
   :feature 'property
   '((field_identifier) @font-lock-property-face
     (shorthand_field_initializer (identifier) @font-lock-property-face))

   :language 'rust
   :feature 'variable
   '((identifier) @font-lock-variable-name-face
     ;; Everything in a token_tree is an identifier.
     (token_tree (identifier) @default))

   :language 'rust
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'rust
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `rust-ts-mode'.")

(defalias 'rust-ts-mode--fontify-pattern
  (and
   (treesit-available-p)
   `(lambda (node override start end &rest _)
      (let ((captures (treesit-query-capture
                       (treesit-node-child-by-field-name node "pattern")
                       ,(treesit-query-compile 'rust '((identifier) @id
                                                       (shorthand_field_identifier) @id)))))
        (pcase-dolist (`(_name . ,id) captures)
          (unless (string-match-p "\\`scoped_\\(?:type_\\)?identifier\\'"
                                  (treesit-node-type
                                   (treesit-node-parent id)))
            (treesit-fontify-with-override
             (treesit-node-start id) (treesit-node-end id)
             'font-lock-variable-name-face override start end)))))))

(defun rust-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("enum_item"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("function_item"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("impl_item"
     (let ((trait-node (treesit-node-child-by-field-name node "trait")))
       (concat
        (treesit-node-text trait-node t)
        (when trait-node " for ")
        (treesit-node-text
         (treesit-node-child-by-field-name node "type") t))))
    ("mod_item"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("struct_item"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("type_item"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))))

(defun rust-ts-mode--syntax-propertize (beg end)
  "Apply syntax text property to template delimiters between BEG and END.

< and > are usually punctuation, e.g., as greater/less-than.  But
when used for types, they should be considered pairs.

This function checks for < and > in the changed RANGES and apply
appropriate text property to alter the syntax of template
delimiters < and >'s."
  (goto-char beg)
  (while (re-search-forward (rx (or "<" ">")) end t)
    (pcase (treesit-node-type
            (treesit-node-parent
             (treesit-node-at (match-beginning 0))))
      ("type_arguments"
       (put-text-property (match-beginning 0)
                          (match-end 0)
                          'syntax-table
                          (pcase (char-before)
                            (?< '(4 . ?>))
                            (?> '(5 . ?<))))))))

;;;###autoload
(define-derived-mode rust-ts-mode prog-mode "Rust"
  "Major mode for editing Rust, powered by tree-sitter."
  :group 'rust
  :syntax-table rust-ts-mode--syntax-table

  (when (treesit-ready-p 'rust)
    (treesit-parser-create 'rust)

    ;; Syntax.
    (setq-local syntax-propertize-function
                #'rust-ts-mode--syntax-propertize)

    ;; Comments.
    (c-ts-common-comment-setup)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings rust-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string)
                  ( attribute builtin constant escape-sequence
                    number type)
                  ( bracket delimiter error function operator property variable)))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Module" "\\`mod_item\\'" nil nil)
                  ("Enum" "\\`enum_item\\'" nil nil)
                  ("Impl" "\\`impl_item\\'" nil nil)
                  ("Type" "\\`type_item\\'" nil nil)
                  ("Struct" "\\`struct_item\\'" nil nil)
                  ("Fn" "\\`function_item\\'" nil nil)))

    ;; Indent.
    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules rust-ts-mode--indent-rules)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("enum_item"
                              "function_item"
                              "impl_item"
                              "struct_item")))
    (setq-local treesit-defun-name-function #'rust-ts-mode--defun-name)

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'rust)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

(provide 'rust-ts-mode)

;;; rust-ts-mode.el ends here
