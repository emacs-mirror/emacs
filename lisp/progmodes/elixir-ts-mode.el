;;; elixir-ts-mode.el --- Major mode for Elixir with tree-sitter support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author: Wilhelm H Kirschbaum <wkirschbaum@gmail.com>
;; Created: November 2022
;; Keywords: elixir languages tree-sitter

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
;; This package provides `elixir-ts-mode' which is a major mode for editing
;; Elixir files and embedded HEEx templates that uses Tree Sitter to parse
;; the language.
;;
;; This package is compatible with and was tested against the tree-sitter grammar
;; for Elixir found at https://github.com/elixir-lang/tree-sitter-elixir.
;;
;; Features
;;
;; * Indent
;;
;; `elixir-ts-mode' tries to replicate the indentation provided by
;; mix format, but will come with some minor differences.
;;
;; * IMenu
;; * Navigation
;; * Which-fun

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-parser-language "treesit.c")
(declare-function treesit-parser-included-ranges "treesit.c")
(declare-function treesit-parser-list "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-query-compile "treesit.c")
(declare-function treesit-query-capture "treesit.c")
(declare-function treesit-node-eq "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")

(defgroup elixir-ts nil
  "Major mode for editing Elixir code."
  :prefix "elixir-ts-"
  :group 'languages)

(defcustom elixir-ts-indent-offset 2
  "Indentation of Elixir statements."
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'elixir-ts)

(defface elixir-ts-font-comment-doc-identifier-face
  '((t (:inherit font-lock-doc-face)))
  "Face used for @comment.doc tags in Elixir files.")

(defface elixir-ts-font-comment-doc-attribute-face
  '((t (:inherit font-lock-doc-face)))
  "Face used for @comment.doc.__attribute__ tags in Elixir files.")

(defface elixir-ts-font-sigil-name-face
  '((t (:inherit font-lock-string-face)))
  "Face used for @__name__ tags in Elixir files.")

(defconst elixir-ts--sexp-regexp
  (rx bol
      (or "call" "stab_clause" "binary_operator" "list" "tuple" "map" "pair"
          "sigil" "string" "atom" "alias" "arguments" "identifier"
          "boolean" "quoted_content")
      eol))

(defconst elixir-ts--test-definition-keywords
  '("describe" "test"))

(defconst elixir-ts--definition-keywords
  '("def" "defdelegate" "defexception" "defguard" "defguardp"
    "defimpl" "defmacro" "defmacrop" "defmodule" "defn" "defnp"
    "defoverridable" "defp" "defprotocol" "defstruct"))

(defconst elixir-ts--definition-keywords-re
  (concat "^" (regexp-opt elixir-ts--definition-keywords) "$"))

(defconst elixir-ts--kernel-keywords
  '("alias" "case" "cond" "else" "for" "if" "import" "quote"
    "raise" "receive" "require" "reraise" "super" "throw" "try"
    "unless" "unquote" "unquote_splicing" "use" "with"))

(defconst elixir-ts--kernel-keywords-re
  (concat "^" (regexp-opt elixir-ts--kernel-keywords) "$"))

(defconst elixir-ts--builtin-keywords
  '("__MODULE__" "__DIR__" "__ENV__" "__CALLER__" "__STACKTRACE__"))

(defconst elixir-ts--builtin-keywords-re
  (concat "^" (regexp-opt elixir-ts--builtin-keywords) "$"))

(defconst elixir-ts--doc-keywords
  '("moduledoc" "typedoc" "doc"))

(defconst elixir-ts--doc-keywords-re
  (concat "^" (regexp-opt elixir-ts--doc-keywords) "$"))

(defconst elixir-ts--reserved-keywords
  '("when" "and" "or" "not" "in"
    "not in" "fn" "do" "end" "catch" "rescue" "after" "else"))

(defconst elixir-ts--reserved-keywords-re
  (concat "^" (regexp-opt elixir-ts--reserved-keywords) "$"))

(defconst elixir-ts--reserved-keywords-vector
  (apply #'vector elixir-ts--reserved-keywords))

(defvar elixir-ts--capture-anonymous-function-end
  (when (treesit-available-p)
    (treesit-query-compile 'elixir '((anonymous_function "end" @end)))))

(defvar elixir-ts--capture-operator-parent
  (when (treesit-available-p)
    (treesit-query-compile 'elixir '((binary_operator operator: _ @val)))))

(defvar elixir-ts--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?~ "w" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?: "'" table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table for `elixir-ts-mode'.")

(defun elixir-ts--argument-indent-offset (node _parent &rest _)
  "Return the argument offset position for NODE."
  (if (or (treesit-node-prev-sibling node t)
          ;; Don't indent if this is the first node or
          ;; if the line is empty.
          (save-excursion
            (beginning-of-line)
            (looking-at-p "[[:blank:]]*$")))
      0 elixir-ts-indent-offset))

(defun elixir-ts--argument-indent-anchor (node parent &rest _)
  "Return the argument anchor position for NODE and PARENT."
  (let ((first-sibling (treesit-node-child parent 0 t)))
    (if (and first-sibling (not (treesit-node-eq first-sibling node)))
        (treesit-node-start first-sibling)
      (elixir-ts--parent-expression-start node parent))))

(defun elixir-ts--parent-expression-start (_node parent &rest _)
  "Return the indentation expression start for NODE and PARENT."
  ;; If the parent is the first expression on the line return the
  ;; parent start of node position, otherwise use the parent call
  ;; start if available.
  (if (eq (treesit-node-start parent)
          (save-excursion
            (goto-char (treesit-node-start parent))
            (back-to-indentation)
            (point)))
      (treesit-node-start parent)
    (let ((expr-parent
           (treesit-parent-until
            parent
            (lambda (n)
              (member (treesit-node-type n)
                      '("call" "binary_operator" "keywords" "list"))))))
      (save-excursion
        (goto-char (treesit-node-start expr-parent))
        (back-to-indentation)
        (if (looking-at "|>")
            (point)
          (treesit-node-start expr-parent))))))

(defvar elixir-ts--indent-rules
  (let ((offset elixir-ts-indent-offset))
    `((elixir
       ((parent-is "^source$") column-0 0)
       ((parent-is "^string$") parent-bol 0)
       ((parent-is "^quoted_content$")
        (lambda (_n parent bol &rest _)
          (save-excursion
            (back-to-indentation)
            (if (bolp)
                (progn
                  (goto-char (treesit-node-start parent))
                  (back-to-indentation)
                  (point))
              (point))))
        0)
       ((node-is "^|>$") parent-bol 0)
       ((node-is "^|$") parent-bol 0)
       ((node-is "^]$") ,'elixir-ts--parent-expression-start 0)
       ((node-is "^}$") ,'elixir-ts--parent-expression-start 0)
       ((node-is "^)$") ,'elixir-ts--parent-expression-start 0)
       ((node-is "^else_block$") grand-parent 0)
       ((node-is "^catch_block$") grand-parent 0)
       ((node-is "^rescue_block$") grand-parent 0)
       ((node-is "^after_block$") grand-parent 0)
       ((parent-is "^else_block$") parent ,offset)
       ((parent-is "^catch_block$") parent ,offset)
       ((parent-is "^rescue_block$") parent ,offset)
       ((parent-is "^rescue_block$") parent ,offset)
       ((parent-is "^after_block$") parent ,offset)
       ((parent-is "^access_call$")
        ,'elixir-ts--argument-indent-anchor
        ,'elixir-ts--argument-indent-offset)
       ((parent-is "^tuple$")
        ,'elixir-ts--argument-indent-anchor
        ,'elixir-ts--argument-indent-offset)
       ((parent-is "^list$")
        ,'elixir-ts--argument-indent-anchor
        ,'elixir-ts--argument-indent-offset)
       ((parent-is "^pair$") parent ,offset)
       ((parent-is "^map_content$") parent-bol 0)
       ((parent-is "^map$") ,'elixir-ts--parent-expression-start ,offset)
       ((node-is "^stab_clause$") parent-bol ,offset)
       ((query ,elixir-ts--capture-operator-parent) grand-parent 0)
       ((node-is "^when$") parent 0)
       ((node-is "^keywords$") parent-bol ,offset)
       ((parent-is "^body$")
        (lambda (node parent _)
          (save-excursion
            ;; The grammar adds a comment outside of the body, so we have to indent
            ;; to the grand-parent if it is available.
            (goto-char (treesit-node-start
                        (or (treesit-node-parent parent) (parent))))
            (back-to-indentation)
            (point)))
        ,offset)
       ((parent-is "^arguments$")
        ,'elixir-ts--argument-indent-anchor
        ,'elixir-ts--argument-indent-offset)
       ;; Handle incomplete maps when parent is ERROR.
       ((n-p-gp "^binary_operator$" "ERROR" nil) parent-bol 0)
       ;; When there is an ERROR, just indent to prev-line.
       ((parent-is "ERROR") prev-line ,offset)
       ((node-is "^binary_operator$")
        (lambda (node parent &rest _)
          (let ((top-level
                 (treesit-parent-while
                  node
                  (lambda (node)
                    (equal (treesit-node-type node)
                           "binary_operator")))))
            (if (treesit-node-eq top-level node)
                (elixir-ts--parent-expression-start node parent)
              (treesit-node-start top-level))))
        (lambda (node parent _)
          (cond
           ((equal (treesit-node-type parent) "do_block")
            ,offset)
           ((equal (treesit-node-type parent) "binary_operator")
            ,offset)
           (t 0))))
       ((parent-is "^binary_operator$")
        (lambda (node parent bol &rest _)
          (treesit-node-start
           (treesit-parent-while
            parent
            (lambda (node)
              (equal (treesit-node-type node) "binary_operator")))))
        ,offset)
       ((node-is "^pair$") first-sibling 0)
       ((query ,elixir-ts--capture-anonymous-function-end) parent-bol 0)
       ((node-is "^end$") standalone-parent 0)
       ((parent-is "^do_block$") grand-parent ,offset)
       ((parent-is "^anonymous_function$")
        elixir-ts--treesit-anchor-grand-parent-bol ,offset)
       ((parent-is "^else_block$") parent ,offset)
       ((parent-is "^rescue_block$") parent ,offset)
       ((parent-is "^catch_block$") parent ,offset)
       ((parent-is "^keywords$") parent-bol 0)
       ((node-is "^call$") parent-bol ,offset)
       ((node-is "^comment$") parent-bol ,offset)))))

(defvar elixir-ts--font-lock-settings
  (treesit-font-lock-rules
   :language 'elixir
   :feature 'elixir-comment
   '((comment) @font-lock-comment-face)

   :language 'elixir
   :feature 'elixir-string
   :override t
   '([(string) (charlist)] @font-lock-string-face)

   :language 'elixir
   :feature 'elixir-string-interpolation
   :override t
   '((string
      [
       quoted_end: _ @font-lock-string-face
       quoted_start: _ @font-lock-string-face
       (quoted_content) @font-lock-string-face
       (interpolation
        "#{" @font-lock-regexp-grouping-backslash "}"
        @font-lock-regexp-grouping-backslash)
       ])
     (charlist
      [
       quoted_end: _ @font-lock-string-face
       quoted_start: _ @font-lock-string-face
       (quoted_content) @font-lock-string-face
       (interpolation
        "#{" @font-lock-regexp-grouping-backslash "}"
        @font-lock-regexp-grouping-backslash)
       ]))

   :language 'elixir
   :feature 'elixir-keyword
   `(,elixir-ts--reserved-keywords-vector
     @font-lock-keyword-face
     (binary_operator
      operator: _ @font-lock-keyword-face
      (:match ,elixir-ts--reserved-keywords-re @font-lock-keyword-face)))

   :language 'elixir
   :feature 'elixir-doc
   :override t
   `((unary_operator
      operator: "@" @elixir-ts-font-comment-doc-attribute-face
      operand: (call
                target: (identifier) @elixir-ts-font-comment-doc-identifier-face
                ;; Arguments can be optional, so adding another
                ;; entry without arguments.
                ;; If we don't handle then we don't apply font
                ;; and the non doc fortification query will take specify
                ;; a more specific font which takes precedence.
                (arguments
                 [
                  (string) @font-lock-doc-face
                  (charlist) @font-lock-doc-face
                  (sigil) @font-lock-doc-face
                  (boolean) @font-lock-doc-face
                  ]))
      (:match ,elixir-ts--doc-keywords-re
              @elixir-ts-font-comment-doc-identifier-face))
     (unary_operator
      operator: "@" @elixir-ts-font-comment-doc-attribute-face
      operand: (call
                target: (identifier) @elixir-ts-font-comment-doc-identifier-face)
      (:match ,elixir-ts--doc-keywords-re
              @elixir-ts-font-comment-doc-identifier-face)))

   :language 'elixir
   :feature 'elixir-unary-operator
   `((unary_operator operator: "@" @font-lock-preprocessor-face
                     operand: [
                               (identifier)  @font-lock-preprocessor-face
                               (call target: (identifier)
                                     @font-lock-preprocessor-face)
                               (boolean)  @font-lock-preprocessor-face
                               (nil)  @font-lock-preprocessor-face
                               ])

     (unary_operator operator: "&") @font-lock-function-name-face
     (operator_identifier) @font-lock-operator-face)

   :language 'elixir
   :feature 'elixir-operator
   '((binary_operator operator: _ @font-lock-operator-face)
     (dot operator: _ @font-lock-operator-face)
     (stab_clause operator: _ @font-lock-operator-face)

     [(boolean) (nil)] @font-lock-constant-face
     [(integer) (float)] @font-lock-number-face
     (alias) @font-lock-type-face
     (call target: (dot left: (atom) @font-lock-type-face))
     (char) @font-lock-constant-face
     [(atom) (quoted_atom)] @font-lock-type-face
     [(keyword) (quoted_keyword)] @font-lock-builtin-face)

   :language 'elixir
   :feature 'elixir-call
   `((call
      target: (identifier) @font-lock-keyword-face
      (:match ,elixir-ts--definition-keywords-re @font-lock-keyword-face))
     (call
      target: (identifier) @font-lock-keyword-face
      (:match ,elixir-ts--kernel-keywords-re @font-lock-keyword-face))
     (call
      target: [(identifier) @font-lock-function-name-face
               (dot right: (identifier) @font-lock-keyword-face)])
     (call
      target: (identifier) @font-lock-keyword-face
      (arguments
       [
        (identifier) @font-lock-keyword-face
        (binary_operator
         left: (identifier) @font-lock-keyword-face
         operator: "when")
        ])
      (:match ,elixir-ts--definition-keywords-re @font-lock-keyword-face))
     (call
      target: (identifier) @font-lock-keyword-face
      (arguments
       (binary_operator
        operator: "|>"
        right: (identifier)))
      (:match ,elixir-ts--definition-keywords-re @font-lock-keyword-face)))

   :language 'elixir
   :feature 'elixir-constant
   `((binary_operator operator: "|>" right: (identifier)
                      @font-lock-function-name-face)
     ((identifier) @font-lock-keyword-face
      (:match ,elixir-ts--builtin-keywords-re
              @font-lock-keyword-face))
     ((identifier) @font-lock-comment-face
      (:match "^_" @font-lock-comment-face))
     (identifier) @font-lock-function-name-face
     ["%"] @font-lock-keyward-face
     ["," ";"] @font-lock-keyword-face
     ["(" ")" "[" "]" "{" "}" "<<" ">>"] @font-lock-keyword-face)

   :language 'elixir
   :feature 'elixir-sigil
   :override t
   `((sigil
      (sigil_name) @elixir-ts-font-sigil-name-face
      (:match "^[sSwWpPUD]$" @elixir-ts-font-sigil-name-face))
     @font-lock-string-face
     (sigil
      "~" @font-lock-string-face
      (sigil_name) @elixir-ts-font-sigil-name-face
      (:match "^[rR]$" @elixir-ts-font-sigil-name-face))
     @font-lock-regexp-face
     (sigil
      "~" @font-lock-string-face
      (sigil_name) @elixir-ts-font-sigil-name-face
      quoted_start: _ @font-lock-string-face
      quoted_end: _ @font-lock-string-face
      (:match "^[HF]$" @elixir-ts-font-sigil-name-face)))

   :language 'elixir
   :feature 'elixir-string-escape
   :override t
   `((escape_sequence) @font-lock-regexp-grouping-backslash))
  "Tree-sitter font-lock settings.")

(defvar elixir-ts--treesit-range-rules
  (when (treesit-available-p)
    (treesit-range-rules
     :embed 'heex
     :host 'elixir
     '((sigil (sigil_name) @name (:match "^[HF]$" @name) (quoted_content) @heex)))))

(defvar heex-ts--sexp-regexp)
(defvar heex-ts--indent-rules)
(defvar heex-ts--font-lock-settings)

(defun elixir-ts--forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
With ARG, do it many times.  Negative ARG means move backward."
  (or arg (setq arg 1))
  (funcall
   (if (> arg 0) #'treesit-end-of-thing #'treesit-beginning-of-thing)
   (if (eq (treesit-language-at (point)) 'heex)
       heex-ts--sexp-regexp
     elixir-ts--sexp-regexp)
   (abs arg)))

(defun elixir-ts--treesit-anchor-grand-parent-bol (_n parent &rest _)
  "Return the beginning of non-space characters for the parent node of PARENT."
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-parent parent)))
    (back-to-indentation)
    (point)))

(defun elixir-ts--treesit-language-at-point (point)
  "Return the language at POINT."
  (let* ((range nil)
         (language-in-range
          (cl-loop
           for parser in (treesit-parser-list)
           do (setq range
                    (cl-loop
                     for range in (treesit-parser-included-ranges parser)
                     if (and (>= point (car range)) (<= point (cdr range)))
                     return parser))
           if range
           return (treesit-parser-language parser))))
    (if (null language-in-range)
        (when-let ((parser (car (treesit-parser-list))))
          (treesit-parser-language parser))
      language-in-range)))

(defun elixir-ts--defun-p (node)
  "Return non-nil when NODE is a defun."
  (member (treesit-node-text
           (treesit-node-child-by-field-name node "target"))
          (append
           elixir-ts--definition-keywords
           elixir-ts--test-definition-keywords)))

(defun elixir-ts--defun-name (node)
  "Return the name of the defun NODE.
Return nil if NODE is not a defun node or doesn't have a name."
  (pcase (treesit-node-type node)
    ("call" (let ((node-child
                   (treesit-node-child (treesit-node-child node 1) 0)))
              (pcase (treesit-node-type node-child)
                ("alias" (treesit-node-text node-child t))
                ("call" (treesit-node-text
                         (treesit-node-child-by-field-name node-child "target") t))
                ("binary_operator"
                 (treesit-node-text
                  (treesit-node-child-by-field-name
                   (treesit-node-child-by-field-name node-child "left") "target")
                  t))
                ("identifier"
                 (treesit-node-text node-child t))
                (_ nil))))
    (_ nil)))

(defvar elixir-ts--syntax-propertize-query
  (when (treesit-available-p)
    (treesit-query-compile
     'elixir
     '(((["\"\"\""] @quoted-text))))))

(defun elixir-ts--syntax-propertize (start end)
  "Apply syntax text properties between START and END for `elixir-ts-mode'."
  (let ((captures
         (treesit-query-capture 'elixir elixir-ts--syntax-propertize-query start end)))
    (pcase-dolist (`(,name . ,node) captures)
      (pcase-exhaustive name
        ('quoted-text
         (put-text-property (1- (treesit-node-end node)) (treesit-node-end node)
                            'syntax-table (string-to-syntax "$")))))))

;;;###autoload
(define-derived-mode elixir-ts-mode prog-mode "Elixir"
  "Major mode for editing Elixir, powered by tree-sitter."
  :group 'elixir-ts
  :syntax-table elixir-ts--syntax-table

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-start-skip
              (rx "#" (* (syntax whitespace))))

  (setq-local comment-end "")
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end) "\n"))))

  ;; Compile
  (setq-local compile-command "mix")

  (when (treesit-ready-p 'elixir)
    ;; The HEEx parser has to be created first for elixir to ensure elixir
    ;; is the first language when looking for treesit ranges.
    (when (treesit-ready-p 'heex)
      ;; Require heex-ts-mode only when we load elixir-ts-mode
      ;; so that we don't get a tree-sitter compilation warning for
      ;; elixir-ts-mode.
      (require 'heex-ts-mode)
      (treesit-parser-create 'heex))

    (treesit-parser-create 'elixir)

    (setq-local treesit-language-at-point-function
                'elixir-ts--treesit-language-at-point)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings elixir-ts--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( elixir-comment elixir-constant elixir-doc )
                  ( elixir-string elixir-keyword elixir-unary-operator
                    elixir-call elixir-operator )
                  ( elixir-sigil elixir-string-escape elixir-string-interpolation)))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                '((nil "\\`call\\'" elixir-ts--defun-p nil)))

    ;; Indent.
    (setq-local treesit-simple-indent-rules elixir-ts--indent-rules)

    ;; Navigation
    (setq-local forward-sexp-function #'elixir-ts--forward-sexp)
    (setq-local treesit-defun-type-regexp
                '("call" . elixir-ts--defun-p))

    (setq-local treesit-defun-name-function #'elixir-ts--defun-name)

    ;; Embedded Heex
    (when (treesit-ready-p 'heex)
      (setq-local treesit-range-settings elixir-ts--treesit-range-rules)

      (setq-local treesit-simple-indent-rules
                  (append treesit-simple-indent-rules heex-ts--indent-rules))

      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings
                          heex-ts--font-lock-settings))

      (setq-local treesit-simple-indent-rules
                  (append treesit-simple-indent-rules
                          heex-ts--indent-rules))

      (setq-local treesit-font-lock-feature-list
                  '(( elixir-comment elixir-constant elixir-doc
                      heex-comment heex-keyword heex-doctype )
                    ( elixir-string elixir-keyword elixir-unary-operator
                      elixir-call elixir-operator
                      heex-component heex-tag heex-attribute heex-string)
                    ( elixir-sigil elixir-string-escape
                      elixir-string-interpolation ))))

    (treesit-major-mode-setup)
    (setq-local syntax-propertize-function #'elixir-ts--syntax-propertize)))

(if (treesit-ready-p 'elixir)
    (progn
      (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
      (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-ts-mode))))

(provide 'elixir-ts-mode)

;;; elixir-ts-mode.el ends here
