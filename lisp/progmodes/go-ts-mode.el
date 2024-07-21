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
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-search-subtree "treesit.c")

(defcustom go-ts-mode-indent-offset 8
  "Number of spaces for each indentation step in `go-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'go)

(defcustom go-ts-mode-build-tags nil
  "List of Go build tags for the test commands."
  :version "31.1"
  :type '(repeat string)
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

(defvar go-ts-mode--builtin-functions
  '("append" "cap" "clear" "close" "complex" "copy" "delete" "imag" "len" "make"
    "max" "min" "new" "panic" "print" "println" "real" "recover")
  "Go built-in functions for tree-sitter font-locking.")

(defun go-ts-mode--iota-query-supported-p ()
  "Return t if the iota query is supported by the tree-sitter-go grammar."
  (ignore-errors
    (or (treesit-query-string "" '((iota) @font-lock-constant-face) 'go) t)))

;; tree-sitter-go changed method_spec to method_elem in
;; https://github.com/tree-sitter/tree-sitter-go/commit/b82ab803d887002a0af11f6ce63d72884580bf33
(defun go-ts-mode--method-elem-supported-p ()
  "Return t if Go grammar uses `method_elem' instead of `method_spec'."
  (ignore-errors
    (or (treesit-query-string "" '((method_elem) @cap) 'go) t)))

(defvar go-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'go
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'go
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'go
   :feature 'builtin
   `((call_expression
      function: ((identifier) @font-lock-builtin-face
                 (:match ,(rx-to-string
                           `(seq bol
                                 (or ,@go-ts-mode--builtin-functions)
                                 eol))
                         @font-lock-builtin-face))))

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
   :feature 'operator
   `([,@go-ts-mode--operators] @font-lock-operator-face)

   :language 'go
   :feature 'definition
   `((function_declaration
      name: (identifier) @font-lock-function-name-face)
     (method_declaration
      name: (field_identifier) @font-lock-function-name-face)
     (,(if (go-ts-mode--method-elem-supported-p)
           'method_elem
         'method_spec)
      name: (field_identifier) @font-lock-function-name-face)
     (field_declaration
      name: (field_identifier) @font-lock-property-name-face)
     (parameter_declaration
      name: (identifier) @font-lock-variable-name-face)
     (variadic_parameter_declaration
      name: (identifier) @font-lock-variable-name-face)
     (short_var_declaration
      left: (expression_list
             (identifier) @font-lock-variable-name-face
             ("," (identifier) @font-lock-variable-name-face)*))
     (var_spec name: (identifier) @font-lock-variable-name-face
               ("," name: (identifier) @font-lock-variable-name-face)*)
     (range_clause
      left: (expression_list
             (identifier) @font-lock-variable-name-face)))

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

(defvar-keymap go-ts-mode-map
  :doc "Keymap used in Go mode, powered by tree-sitter"
  :parent prog-mode-map
  "C-c C-d" #'go-ts-mode-docstring
  "C-c C-t t" #'go-ts-mode-test-function-at-point
  "C-c C-t f" #'go-ts-mode-test-this-file
  "C-c C-t p" #'go-ts-mode-test-this-package)

;;;###autoload
(define-derived-mode go-ts-mode prog-mode "Go"
  "Major mode for editing Go, powered by tree-sitter.

\\{go-ts-mode-map}"
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
                  ( builtin constant escape-sequence label number)
                  ( bracket delimiter error function operator property variable)))

    (treesit-major-mode-setup)))

(derived-mode-add-parents 'go-ts-mode '(go-mode))

(if (treesit-ready-p 'go)
    ;; FIXME: Should we instead put `go-mode' in `auto-mode-alist'
    ;; and then use `major-mode-remap-defaults' to map it to `go-ts-mode'?
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode)))

(defun go-ts-mode--defun-name (node &optional skip-prefix)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node.
Methods are prefixed with the receiver name, unless SKIP-PREFIX is t."
  (pcase (treesit-node-type node)
    ("function_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))
    ("method_declaration"
     (let* ((receiver-node (treesit-node-child-by-field-name node "receiver"))
            (receiver (treesit-node-text (treesit-search-subtree receiver-node "type_identifier")))
            (method (treesit-node-text (treesit-node-child-by-field-name node "name"))))
       (if skip-prefix method
         (concat "(" receiver ")." method))))
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

(defun go-ts-mode-docstring ()
  "Add a docstring comment for the current defun.
The added docstring is prefilled with the defun's name.  If the
comment already exists, jump to it."
  (interactive)
  (when-let ((defun-node (treesit-defun-at-point)))
    (goto-char (treesit-node-start defun-node))
    (if (go-ts-mode--comment-on-previous-line-p)
        ;; go to top comment line
        (while (go-ts-mode--comment-on-previous-line-p)
          (forward-line -1))
      (insert "// " (go-ts-mode--defun-name defun-node t))
      (newline)
      (backward-char))))

(defun go-ts-mode--comment-on-previous-line-p ()
  "Return t if the previous line is a comment."
  (when-let ((point (- (pos-bol) 1))
             ((> point 0))
             (node (treesit-node-at point)))
    (and
     ;; check point is actually inside the found node
     ;; treesit-node-at can return nodes after point
     (<= (treesit-node-start node) point (treesit-node-end node))
     (string-equal "comment" (treesit-node-type node)))))

(defun go-ts-mode--get-build-tags-flag ()
  "Return the compile flag for build tags.
This function respects the `go-ts-mode-build-tags' variable for
specifying build tags."
  (if go-ts-mode-build-tags
      (format "-tags %s" (string-join go-ts-mode-build-tags ","))
    ""))

(defun go-ts-mode--compile-test (regexp)
  "Compile the tests matching REGEXP.
This function respects the `go-ts-mode-build-tags' variable for
specifying build tags."
  (compile (format "go test -v %s -run '%s'"
                   (go-ts-mode--get-build-tags-flag)
                   regexp)))

(defun go-ts-mode--find-defun-at (start)
  "Return the first defun node from START."
  (let ((thing (or treesit-defun-type-regexp 'defun)))
    (or (treesit-thing-at start thing)
        (treesit-thing-next start thing))))

(defun go-ts-mode--get-function-regexp (name)
  (if name
      (format "^%s$" name)
    (error "No test function found")))

(defun go-ts-mode--get-functions-in-range (start end)
  "Return a list with the names of all defuns in the range START to END."
  (let* ((node (go-ts-mode--find-defun-at start))
         (name (treesit-defun-name node))
         (node-start (treesit-node-start node))
         (node-end (treesit-node-end node)))
    (cond ((or (not node)
               (> start node-end)
               (< end node-start))
           nil)
          ((or (not (equal (treesit-node-type node) "function_declaration"))
               (not (string-prefix-p "Test" name)))
           (go-ts-mode--get-functions-in-range (treesit-node-end node) end))
          (t
           (cons (go-ts-mode--get-function-regexp name)
                 (go-ts-mode--get-functions-in-range (treesit-node-end node) end))))))

(defun go-ts-mode--get-test-regexp-at-point ()
  "Return a regular expression for the tests at point.
If region is active, the regexp will include all the functions under the
region."
  (if-let ((range (if (region-active-p)
                      (list (region-beginning) (region-end))
                    (list (point) (point))))
           (funcs (apply #'go-ts-mode--get-functions-in-range range)))
      (string-join funcs "|")
    (error "No test function found")))

(defun go-ts-mode-test-function-at-point ()
  "Run the unit test at point.
If the point is anywhere in the test function, that function will be
run.  If the region is selected, all the functions under the region will
be run."
  (interactive)
  (go-ts-mode--compile-test (go-ts-mode--get-test-regexp-at-point)))

(defun go-ts-mode-test-this-file ()
  "Run all the unit tests in the current file."
  (interactive)
  (if-let ((defuns (go-ts-mode--get-functions-in-range (point-min) (point-max))))
      (go-ts-mode--compile-test (string-join defuns "|"))
    (error "No test functions found in the current file")))

(defun go-ts-mode-test-this-package ()
  "Run all the unit tests under the current package."
  (interactive)
  (compile (format "go test -v %s -run %s"
                   (go-ts-mode--get-build-tags-flag)
                   default-directory)))

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

(derived-mode-add-parents 'go-mod-ts-mode '(go-mod-mode))

(if (treesit-ready-p 'gomod)
    (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode)))

(provide 'go-ts-mode)

;;; go-ts-mode.el ends here
