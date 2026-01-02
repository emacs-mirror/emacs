;;; rust-ts-mode.el --- tree-sitter support for Rust  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;;; Tree-sitter language versions
;;
;; rust-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-rust: v0.24.0
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))
(require 'c-ts-common) ; For comment indent and filling.
(treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 `(rust "https://github.com/tree-sitter/tree-sitter-rust"
        :commit ,(if (and (treesit-available-p)
                          (< (treesit-library-abi-version) 15))
                     "1f63b33efee17e833e0ea29266dd3d713e27e321"
                   "18b0515fca567f5a10aee9978c6d2640e878671a"))
 t)

(defcustom rust-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `rust-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'rust)

(defcustom rust-ts-flymake-command '("clippy-driver" "-")
  "The external tool that will be used to perform the check.
This is a non-empty list of strings: the checker tool possibly followed
by required arguments.  Once launched it will receive the Rust source
to be checked as its standard input."
  :version "30.1"
  :type '(choice (const :tag "Clippy standalone" ("clippy-driver" "-"))
                 ;; TODO: Maybe add diagnostics filtering by file name,
                 ;; to limit non-project list to the current buffer.
                 ;; Or annotate them with file names, at least.
                 (const :tag "Clippy cargo" ("cargo" "clippy"))
                 (repeat :tag "Custom command" string))
  :group 'rust)

(defcustom rust-ts-mode-fontify-number-suffix-as-type nil
  "If non-nil, suffixes of number literals are fontified as types.
In Rust, number literals can possess an optional type suffix.  When this
variable is non-nil, these suffixes are fontified using
`font-lock-type-face' instead of `font-lock-number-face'."
  :version "31.1"
  :type 'boolean
  :group 'rust)

(defvar rust-ts-mode-prettify-symbols-alist
  '(("&&" . ?∧) ("||" . ?∨)
    ("<=" . ?≤)  (">=" . ?≥) ("!=" . ?≠)
    ("INFINITY" . ?∞) ("->" . ?→) ("=>" . ?⇒))
  "Value for `prettify-symbols-alist' in `rust-ts-mode'.")

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
     ((parent-is "source_file") column-0 0)
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
     ((parent-is "struct_pattern") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "token_tree") parent-bol rust-ts-mode-indent-offset)
     ((parent-is "use_list") parent-bol rust-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `rust-ts-mode'.")

(defconst rust-ts-mode--number-types
  (regexp-opt '("u8" "i8" "u16" "i16" "u32" "i32" "u64"
                "i64" "u128" "i128" "usize" "isize" "f32" "f64"))
  "Regexp matching type suffixes of number literals.
See https://doc.rust-lang.org/reference/tokens.html#suffixes.")

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
  '("as" "async" "await" "break" "const" "continue" "default" "dyn" "else"
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
   '((attribute_item) @font-lock-preprocessor-face
     (inner_attribute_item) @font-lock-preprocessor-face)

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
      (:match "\\`\\(?:Err\\|Ok\\|None\\|Some\\)\\'" @font-lock-type-face)))

   :language 'rust
   :feature 'comment
   '(([(block_comment) (line_comment)]) @rust-ts-mode--comment-docstring)

   :language 'rust
   :feature 'delimiter
   '((["," "." ";" ":" "::"]) @font-lock-delimiter-face)

   :language 'rust
   :feature 'definition
   '((function_item name: (identifier) @font-lock-function-name-face)
     (function_signature_item name: (identifier) @font-lock-function-name-face)
     (macro_definition "macro_rules!" @font-lock-constant-face)
     (macro_definition (identifier) @font-lock-preprocessor-face)
     (token_binding_pattern
      name: (metavariable) @font-lock-variable-name-face)
     (field_declaration name: (field_identifier) @font-lock-property-name-face)
     (parameter pattern: (_) @rust-ts-mode--fontify-pattern)
     (closure_parameters (_) @rust-ts-mode--fontify-pattern)
     (let_declaration pattern: (_) @rust-ts-mode--fontify-pattern)
     (for_expression pattern: (_) @rust-ts-mode--fontify-pattern)
     (let_condition pattern: (_) @rust-ts-mode--fontify-pattern)
     (match_arm pattern: (_) @rust-ts-mode--fontify-pattern))

   :language 'rust
   :feature 'assignment
   '((assignment_expression left: (_) @rust-ts-mode--fontify-pattern)
     (compound_assignment_expr left: (_) @rust-ts-mode--fontify-pattern))

   :language 'rust
   :feature 'function
   '((call_expression
      function:
      [(identifier) @font-lock-function-call-face
       (field_expression
        field: (field_identifier) @font-lock-function-call-face)
       (scoped_identifier
        name: (identifier) @font-lock-function-call-face)])
     (generic_function
      function: [(identifier) @font-lock-function-call-face
                 (field_expression
                  field: (field_identifier) @font-lock-function-call-face)
                 (scoped_identifier
                  name: (identifier) @font-lock-function-call-face)])
     (macro_invocation macro: (identifier) @font-lock-preprocessor-face))

   :language 'rust
   :feature 'keyword
   `([,@rust-ts-mode--keywords] @font-lock-keyword-face
     ;; If these keyword are in a macro body, they're marked as
     ;; identifiers.
     ((identifier) @font-lock-keyword-face
      (:match ,(rx bos (or "else" "in" "move") eos) @font-lock-keyword-face)))

   :language 'rust
   :feature 'number
   '([(float_literal) (integer_literal)]
     @rust-ts-mode--fontify-number-literal)

   :language 'rust
   :feature 'operator
   `([,@rust-ts-mode--operators] @font-lock-operator-face
     (token_repetition_pattern ["$" "*" "+"] @font-lock-operator-face)
     (token_repetition ["$" "*" "+"] @font-lock-operator-face))

   :language 'rust
   :feature 'string
   '([(char_literal)
      (raw_string_literal)
      (string_literal)] @font-lock-string-face)

   :language 'rust
   :feature 'type
   `((scoped_use_list path: (identifier) @font-lock-constant-face)
     (scoped_use_list path: (scoped_identifier
                             name: (identifier) @font-lock-constant-face))
     ((use_as_clause alias: (identifier) @font-lock-type-face)
      (:match "\\`[A-Z]" @font-lock-type-face))
     ((use_as_clause path: (identifier) @font-lock-type-face)
      (:match "\\`[A-Z]" @font-lock-type-face))
     ((use_list (identifier) @font-lock-type-face)
      (:match "\\`[A-Z]" @font-lock-type-face))
     (use_wildcard [(identifier) @rust-ts-mode--fontify-scope
                    (scoped_identifier
                     name: (identifier) @rust-ts-mode--fontify-scope)])
     (enum_variant name: (identifier) @font-lock-type-face)
     (match_arm
      pattern: (match_pattern (_ type: (identifier) @font-lock-type-face)))
     (match_arm
      pattern: (match_pattern
                (_ type: (scoped_identifier
                          path: (identifier) @font-lock-type-face))))
     (mod_item name: (identifier) @font-lock-constant-face)
     [(fragment_specifier) (primitive_type) (type_identifier)] @font-lock-type-face
     ((scoped_identifier name: (identifier) @rust-ts-mode--fontify-tail))
     ((scoped_identifier path: (identifier) @font-lock-type-face)
      (:match ,(rx bos
                   (or "u8" "u16" "u32" "u64" "u128" "usize"
                       "i8" "i16" "i32" "i64" "i128" "isize"
                       "char" "str")
                   eos)
              @font-lock-type-face))
     ((scoped_identifier path: (identifier) @rust-ts-mode--fontify-scope))
     ((scoped_type_identifier path: (identifier) @rust-ts-mode--fontify-scope))
     ;; Sometimes the parser can't determine if an identifier is a type,
     ;; so we use this heuristic. See bug#69625 for the full discussion.
     ((identifier) @font-lock-type-face
      (:match ,(rx bos upper) @font-lock-type-face)))

   :language 'rust
   :feature 'property
   '((field_identifier) @font-lock-property-use-face
     (shorthand_field_initializer (identifier) @font-lock-property-use-face))

   ;; Must be under type, otherwise some imports can be highlighted as constants.
   :language 'rust
   :feature 'constant
   `((boolean_literal) @font-lock-constant-face
     ((identifier) @font-lock-constant-face
      (:match "\\`[A-Z][0-9A-Z_]*\\'" @font-lock-constant-face)))

   :language 'rust
   :feature 'variable
   '((arguments (identifier) @font-lock-variable-use-face)
     (array_expression (identifier) @font-lock-variable-use-face)
     (assignment_expression right: (identifier) @font-lock-variable-use-face)
     (binary_expression left: (identifier) @font-lock-variable-use-face)
     (binary_expression right: (identifier) @font-lock-variable-use-face)
     (block (identifier) @font-lock-variable-use-face)
     (compound_assignment_expr right: (identifier) @font-lock-variable-use-face)
     (field_expression value: (identifier) @font-lock-variable-use-face)
     (field_initializer value: (identifier) @font-lock-variable-use-face)
     (if_expression condition: (identifier) @font-lock-variable-use-face)
     (let_condition value: (identifier) @font-lock-variable-use-face)
     (let_declaration value: (identifier) @font-lock-variable-use-face)
     (match_arm value: (identifier) @font-lock-variable-use-face)
     (match_expression value: (identifier) @font-lock-variable-use-face)
     (reference_expression value: (identifier) @font-lock-variable-use-face)
     (return_expression (identifier) @font-lock-variable-use-face)
     (tuple_expression (identifier) @font-lock-variable-use-face)
     (unary_expression (identifier) @font-lock-variable-use-face)
     (while_expression condition: (identifier) @font-lock-variable-use-face)
     (metavariable) @font-lock-variable-use-face)

   :language 'rust
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'rust
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `rust-ts-mode'.")

(defun rust-ts-mode--comment-docstring (node override start end &rest _args)
  "Use the comment or documentation face appropriately for comments."
  (let* ((beg (treesit-node-start node))
         (face (save-excursion
                 (goto-char beg)
                 (if (looking-at-p
                      "/\\(?:/\\(?:/[^/]\\|!\\)\\|\\*\\(?:\\*[^*/]\\|!\\)\\)")
                     'font-lock-doc-face
                   'font-lock-comment-face))))
    (treesit-fontify-with-override beg (treesit-node-end node)
                                   face override start end)))

(defun rust-ts-mode--fontify-scope (node override start end &optional tail-p)
  (let* ((case-fold-search nil)
         (face
          (cond
           ((string-match-p "^[A-Z]" (treesit-node-text node))
            'font-lock-type-face)
           ((and
             tail-p
             (string-match-p
              "\\`\\(?:use_list\\|call_expression\\|use_as_clause\\|use_declaration\\)\\'"
              (or (treesit-node-type (treesit-node-parent (treesit-node-parent node)))
                  "no_parent")))
            nil)
           (t 'font-lock-constant-face))))
    (when face
      (treesit-fontify-with-override
       (treesit-node-start node) (treesit-node-end node)
       face
       override start end))))

(defun rust-ts-mode--fontify-tail (node override start end)
  (rust-ts-mode--fontify-scope node override start end t))

(defalias 'rust-ts-mode--fontify-pattern
  (and
   (treesit-available-p)
   `(lambda (node override start end &rest _)
      (let ((captures (treesit-query-capture
                       node
                       ,(treesit-query-compile 'rust '((identifier) @id
                                                       (shorthand_field_identifier) @id)))))
        (pcase-dolist (`(_name . ,id) captures)
          (unless (string-match-p
                   "\\`scoped_\\(?:type_\\)?identifier\\'"
                   (or (treesit-node-type (treesit-node-parent id)) "no_parent"))
            (treesit-fontify-with-override
             (treesit-node-start id) (treesit-node-end id)
             'font-lock-variable-name-face override start end)))))))

(defun rust-ts-mode--fontify-number-literal (node override start stop &rest _)
  "Fontify number literals, highlighting the optional type suffix.
If `rust-ts-mode-fontify-number-suffix-as-type' is non-nil, use
`font-lock-type-face' to highlight the suffix."
  (let* ((beg (treesit-node-start node))
         (end (treesit-node-end node)))
    (save-excursion
      (goto-char end)
      (if (and rust-ts-mode-fontify-number-suffix-as-type
               (looking-back rust-ts-mode--number-types beg))
          (let* ((ty (match-beginning 0))
                 (nb (if (eq (char-before ty) ?_) (1- ty) ty)))
            (treesit-fontify-with-override
             ty end 'font-lock-type-face override start stop)
            (treesit-fontify-with-override
             beg nb 'font-lock-number-face override start stop))
          (treesit-fontify-with-override
           beg end 'font-lock-number-face override start stop)))))

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
  "Apply syntax properties to special characters between BEG and END.

Apply syntax properties to various special characters with
contextual meaning between BEG and END.

The apostrophe \\=' should be treated as string when used for char literals.

< and > are usually punctuation, e.g., as greater/less-than.  But
when used for types, they should be considered pairs.

This function checks for < and > in the changed RANGES and apply
appropriate text property to alter the syntax of template
delimiters < and >'s."
  (goto-char beg)
  (while (search-forward "'" end t)
    (when (string-equal "char_literal"
                        (treesit-node-type
                         (treesit-node-at (match-beginning 0))))
      (put-text-property (match-beginning 0) (match-end 0)
                         'syntax-table (string-to-syntax "\""))))
  (goto-char beg)
  (while (re-search-forward (rx (or "<" ">")) end t)
    (pcase (treesit-node-type
            (treesit-node-parent
             (treesit-node-at (match-beginning 0))))
      ((or "type_arguments" "type_parameters")
       (put-text-property (match-beginning 0)
                          (match-end 0)
                          'syntax-table
                          (pcase (char-before)
                            (?< '(4 . ?>))
                            (?> '(5 . ?<))))))))

(defun rust-ts-mode--prettify-symbols-compose-p (start end match)
  "Return non-nil if the symbol MATCH should be composed.
See `prettify-symbols-compose-predicate'."
  (and (fboundp 'prettify-symbols-default-compose-p)
       (prettify-symbols-default-compose-p start end match)
       ;; Make sure || is not a closure with 0 arguments and && is not
       ;; a double reference.
       (pcase match
         ((or "||" "&&")
          (string= (treesit-node-field-name (treesit-node-at (point)))
                   "operator"))
         (_ t))))

(defvar rust-ts--flymake-proc nil)

(defun rust-ts-flymake--helper (process-name command parser-fn)
  (when (process-live-p rust-ts--flymake-proc)
    (kill-process rust-ts--flymake-proc))

  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq
       rust-ts--flymake-proc
       (make-process
        :name process-name :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer (format " *%s*" process-name))
        :command command
        :sentinel
        (lambda (proc _event)
          (when (and (eq 'exit (process-status proc)) (buffer-live-p source))
            (unwind-protect
                (if (with-current-buffer source (eq proc rust-ts--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (funcall parser-fn proc source))
                  (flymake-log :debug "Canceling obsolete check %s"
                               proc))
              (kill-buffer (process-buffer proc)))))))
      (process-send-region rust-ts--flymake-proc (point-min) (point-max))
      (process-send-eof rust-ts--flymake-proc))))

(defun rust-ts-flymake (report-fn &rest _args)
  "Rust backend for Flymake."
  (unless (executable-find (car rust-ts-flymake-command))
    (error "Cannot find the rust flymake program: %s" (car rust-ts-flymake-command)))

  (rust-ts-flymake--helper
   "rust-ts-flymake"
   rust-ts-flymake-command
   (lambda (_proc source)
     (goto-char (point-min))
     (cl-loop
      while (search-forward-regexp
             (concat
              "^\\(\\(?:warning\\|error\\|help\\).*\\)\n +--> [^:]+:"
              "\\([0-9]+\\):\\([0-9]+\\)\\(\\(?:\n[^\n]+\\)*\\)\n\n")
             nil t)
      for msg1 = (match-string 1)
      for msg2 = (match-string 4)
      for (beg . end) = (flymake-diag-region
                         source
                         (string-to-number (match-string 2))
                         (string-to-number (match-string 3)))
      for type = (if (string-match "^warning" msg1)
                     :warning
                   :error)
      collect (flymake-make-diagnostic source
                                       beg
                                       end
                                       type
                                       (concat msg1 msg2))
      into diags
      finally (funcall report-fn diags)))))

;;;###autoload
(define-derived-mode rust-ts-mode prog-mode "Rust"
  "Major mode for editing Rust, powered by tree-sitter."
  :group 'rust
  :syntax-table rust-ts-mode--syntax-table

  (when (treesit-ensure-installed 'rust)
    (setq treesit-primary-parser (treesit-parser-create 'rust))

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
                  ( assignment attribute builtin constant escape-sequence
                    number type)
                  ( bracket delimiter error function operator property variable)))

    ;; Prettify configuration
    (setq prettify-symbols-alist rust-ts-mode-prettify-symbols-alist)
    (setq prettify-symbols-compose-predicate
          #'rust-ts-mode--prettify-symbols-compose-p)

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Module" "\\`mod_item\\'" nil nil)
                  ("Enum" "\\`enum_item\\'" nil nil)
                  ("Impl" "\\`impl_item\\'" nil nil)
                  ("Type" "\\`type_item\\'" nil nil)
                  ("Struct" "\\`struct_item\\'" nil nil)
                  ("Fn" "\\`function_item\\'" nil nil)))

    ;; Outline.
    (setq-local treesit-outline-predicate
                (rx bos (or "mod_item"
                            "enum_item"
                            "impl_item"
                            "type_item"
                            "struct_item"
                            "function_item"
                            "trait_item")
                    eos))
    ;; Indent.
    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules rust-ts-mode--indent-rules)

    ;; Electric.
    (setq-local electric-indent-chars
                (append "{}():;,#" electric-indent-chars))

    ;; Flymake.
    (add-hook 'flymake-diagnostic-functions #'rust-ts-flymake nil 'local)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("enum_item"
                              "function_item"
                              "impl_item"
                              "struct_item")))
    (setq-local treesit-defun-name-function #'rust-ts-mode--defun-name)

    (setq-local treesit-thing-settings
                `((rust
                   (list
                    ,(rx bos (or "token_tree_pattern"
                                 "token_tree"
                                 "attribute_item"
                                 "inner_attribute_item"
                                 "declaration_list"
                                 "enum_variant_list"
                                 "field_declaration_list"
                                 "ordered_field_declaration_list"
                                 "type_parameters"
                                 "use_list"
                                 "parameters"
                                 "bracketed_type"
                                 "array_type"
                                 "for_lifetimes"
                                 "tuple_type"
                                 "unit_type"
                                 "use_bounds"
                                 "type_arguments"
                                 "delim_token_tree"
                                 "arguments"
                                 "array_expression"
                                 "parenthesized_expression"
                                 "tuple_expression"
                                 "unit_expression"
                                 "field_initializer_list"
                                 "match_block"
                                 "block"
                                 "tuple_pattern"
                                 "slice_pattern")
                         eos)))))

    (treesit-major-mode-setup)))

(derived-mode-add-parents 'rust-ts-mode '(rust-mode))

;;;###autoload
(defun rust-ts-mode-maybe ()
  "Enable `rust-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'rust)
          (eq treesit-enabled-modes t)
          (memq 'rust-ts-mode treesit-enabled-modes))
      (rust-ts-mode)
    (fundamental-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode-maybe))
  ;; To be able to toggle between an external package and core ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(rust-mode . rust-ts-mode)))

(provide 'rust-ts-mode)

;;; rust-ts-mode.el ends here
