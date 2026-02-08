;;; elixir-ts-mode.el --- Major mode for Elixir with tree-sitter support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;;; Tree-sitter language versions
;;
;; elixir-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-elixir: v0.3.3
;; - tree-sitter-heex: v0.7.0
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

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
(treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 '(elixir "https://github.com/elixir-lang/tree-sitter-elixir"
          :commit "02a6f7fd4be28dd94ee4dd2ca19cb777053ea74e")
 t)
(add-to-list
 'treesit-language-source-alist
 '(heex "https://github.com/phoenixframework/tree-sitter-heex"
        :commit "f6b83f305a755cd49cf5f6a66b2b789be93dc7b9")
 t)

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

;; 'define-derived-mode' doesn't expose the generated mode hook
;; variable to Custom, because we are not smart enough to provide the
;; ':options' for hook variables.  Also, some packages modify hook
;; variables.  The below is done because users of this mode explicitly
;; requested the hook to be customizable via Custom.
(defcustom elixir-ts-mode-hook nil
  "Hook run after entering `elixir-ts-mode'."
  :type 'hook
  :options '(eglot-ensure)
  :group 'elixir-ts
  :version "30.1")

(defface elixir-ts-comment-doc-identifier
  '((t (:inherit font-lock-doc-face)))
  "Face used for doc identifiers in Elixir files."
  :group 'elixir-ts)

(defface elixir-ts-comment-doc-attribute
  '((t (:inherit font-lock-doc-face)))
  "Face used for doc attributes in Elixir files."
  :group 'elixir-ts)

(defface elixir-ts-sigil-name
  '((t (:inherit font-lock-string-face)))
  "Face used for sigils in Elixir files."
  :group 'elixir-ts)

(defface elixir-ts-atom
  '((t (:inherit font-lock-constant-face)))
  "Face used for atoms in Elixir files."
  :group 'elixir-ts)

(defface elixir-ts-keyword-key
  '((t (:inherit elixir-ts-atom)))
  "Face used for keyword keys in Elixir files."
  :group 'elixir-ts)

(defface elixir-ts-attribute
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for attributes in Elixir files."
  :group 'elixir-ts)

(defconst elixir-ts--test-definition-keywords
  '("describe" "test"))

(defconst elixir-ts--definition-keywords
  '("def" "defdelegate" "defexception" "defguard" "defguardp"
    "defimpl" "defmacro" "defmacrop" "defmodule" "defn" "defnp"
    "defoverridable" "defp" "defprotocol" "defstruct"))

(defconst elixir-ts--definition-keywords-re
  (concat "^" (regexp-opt
               (append elixir-ts--definition-keywords
                       elixir-ts--test-definition-keywords))
          "$"))

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
       ((parent-is "^source$")
        ,(lambda (_node _parent bol &rest _)
           ;; If Elixir is embedded indent to parent
           ;; otherwise indent to the bol.
           (if (treesit-local-parsers-at (point))
               (save-excursion
                 (goto-char (treesit-node-start
                             (treesit-node-at bol 'heex)))
                 (back-to-indentation)
                 (point))
             (pos-bol)))
        ,(lambda (_node _parent _bol &rest _)
           (if (treesit-local-parsers-at (point))
               elixir-ts-indent-offset
             0)))
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
       ((node-is "^>>$") ,'elixir-ts--parent-expression-start 0)
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
       ((parent-is "^bitstring$") parent ,offset)
       ((parent-is "^map_content$") parent-bol 0)
       ((parent-is "^map$") ,'elixir-ts--parent-expression-start ,offset)
       ((node-is "^stab_clause$") parent-bol ,offset)
       ((query ,elixir-ts--capture-operator-parent) grand-parent 0)
       ((node-is "^when$") parent 0)
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
       ((node-is "^keywords$") parent-bol ,offset)
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
       ((node-is "^comment$") parent-bol ,offset)
       ((node-is "\"\"\"") parent-bol 0)
       ;; Handle quoted_content indentation on the last
       ;; line before the closing \"\"\", where it might
       ;; see it as no-node outside a HEEx tag.
       (no-node (lambda (_n _p _bol)
                  (treesit-node-start
                   (treesit-node-parent
                    (treesit-node-at (point) 'elixir))))
                0)))))

(defvar elixir-ts--font-lock-settings
  (when (treesit-available-p)
    (treesit-font-lock-rules
     :language 'elixir
     :feature 'elixir-definition
     `((call target: (identifier) @target-identifier
             (arguments
              (call target: (identifier) @font-lock-function-name-face
                    (arguments)))
             (:match ,elixir-ts--definition-keywords-re @target-identifier))
       (call target: (identifier) @target-identifier
             (arguments (identifier) @font-lock-function-name-face)
             (:match ,elixir-ts--definition-keywords-re @target-identifier))
       (call target: (identifier) @target-identifier
             (arguments
              (call target: (identifier) @font-lock-function-name-face
                    (arguments ((identifier)) @font-lock-variable-name-face)))
             (:match ,elixir-ts--definition-keywords-re @target-identifier))
       (call target: (identifier) @target-identifier
             (arguments
              (binary_operator
               left: (call target: (identifier) @font-lock-function-name-face)))
             (:match ,elixir-ts--definition-keywords-re @target-identifier))
       (call target: (identifier) @target-identifier
             (arguments (identifier) @font-lock-function-name-face)
             (do_block)
             (:match ,elixir-ts--definition-keywords-re @target-identifier))
       (call target: (identifier) @target-identifier
             (arguments
              (call target: (identifier) @font-lock-function-name-face
                    (arguments ((identifier)) @font-lock-variable-name-face)))
             (do_block)
             (:match ,elixir-ts--definition-keywords-re @target-identifier))
       (call target: (identifier) @target-identifier
             (arguments
              (binary_operator
               left: (call target: (identifier) @font-lock-function-name-face
                           (arguments ((identifier)) @font-lock-variable-name-face))))
             (do_block)
             (:match ,elixir-ts--definition-keywords-re @target-identifier))
       (unary_operator
        operator: "@"
        (call (arguments
               (binary_operator
                left: (call target: (identifier) @font-lock-function-name-face))))))

     :language 'elixir
     :feature 'elixir-comment
     :override t
     '((comment) @font-lock-comment-face
       ((identifier) @font-lock-comment-face
        (:match "^_[a-z]\\|^_$" @font-lock-comment-face)))

     :language 'elixir
     :feature 'elixir-variable
     `((call target: (identifier)
             (arguments
              (binary_operator
               (call target: (identifier)
                     (arguments ((identifier) @font-lock-variable-use-face))))))
       (call target: (identifier)
             (arguments
              (call target: (identifier)
                    (arguments ((identifier)) @font-lock-variable-use-face))))
       (dot left: (identifier) @font-lock-variable-use-face operator: "." ))

     :language 'elixir
     :feature 'elixir-doc
     `((unary_operator
        operator: "@" @elixir-ts-comment-doc-attribute
        operand: (call
                  target: (identifier) @elixir-ts-comment-doc-identifier
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
                    (keywords) @font-lock-doc-face
                    ]))
        (:match ,elixir-ts--doc-keywords-re
                @elixir-ts-comment-doc-identifier))
       (unary_operator
        operator: "@" @elixir-ts-comment-doc-attribute
        operand: (call
                  target: (identifier) @elixir-ts-comment-doc-identifier)
        (:match ,elixir-ts--doc-keywords-re
                @elixir-ts-comment-doc-identifier)))

     :language 'elixir
     :feature 'elixir-string
     '((interpolation
        "#{" @font-lock-escape-face
        "}" @font-lock-escape-face)
       (string (quoted_content) @font-lock-string-face)
       (quoted_keyword (quoted_content) @font-lock-string-face)
       (charlist (quoted_content) @font-lock-string-face)
       ["\"" "'" "\"\"\""] @font-lock-string-face)

     :language 'elixir
     :feature 'elixir-sigil
     `((sigil
        (sigil_name) @elixir-ts-sigil-name
        (quoted_content) @font-lock-string-face
        ;; HEEx and Surface templates will handled by
        ;; heex-ts-mode if its available.
        (:match "^[^HF]$" @elixir-ts-sigil-name))
       @font-lock-string-face
       (sigil
        (sigil_name) @font-lock-regexp-face
        (:match "^[rR]$" @font-lock-regexp-face))
       @font-lock-regexp-face
       (sigil
        "~" @font-lock-string-face
        (sigil_name) @font-lock-string-face
        quoted_start: _ @font-lock-string-face
        quoted_end: _ @font-lock-string-face))

     :language 'elixir
     :feature 'elixir-operator
     `(["!"] @font-lock-negation-char-face
       ["%"] @font-lock-bracket-face
       ["," ";"] @font-lock-operator-face
       ["(" ")" "[" "]" "{" "}" "<<" ">>"] @font-lock-bracket-face)

     :language 'elixir
     :feature 'elixir-data-type
     '((alias) @font-lock-type-face
       (atom) @elixir-ts-atom
       (keywords (pair key: (keyword) @elixir-ts-keyword-key))
       [(keyword) (quoted_keyword)] @elixir-ts-atom
       [(boolean) (nil)] @elixir-ts-atom
       (unary_operator operator: "@" @elixir-ts-attribute
                       operand: [
                                 (identifier) @elixir-ts-attribute
                                 (call target: (identifier)
                                       @elixir-ts-attribute)
                                 (boolean) @elixir-ts-attribute
                                 (nil) @elixir-ts-attribute
                                 ])
       (operator_identifier) @font-lock-operator-face)

     :language 'elixir
     :feature 'elixir-keyword
     `(,elixir-ts--reserved-keywords-vector
       @font-lock-keyword-face
       (binary_operator
        operator: _ @font-lock-keyword-face
        (:match ,elixir-ts--reserved-keywords-re @font-lock-keyword-face))
       (binary_operator operator: _ @font-lock-operator-face)
       (call
        target: (identifier) @font-lock-keyword-face
        (:match ,elixir-ts--definition-keywords-re @font-lock-keyword-face))
       (call
        target: (identifier) @font-lock-keyword-face
        (:match ,elixir-ts--kernel-keywords-re @font-lock-keyword-face)))

     :language 'elixir
     :feature 'elixir-function-call
     '((call target: (identifier) @font-lock-function-call-face)
       (unary_operator operator: "&" @font-lock-operator-face
                       operand: (binary_operator
                                 left: (identifier)
                                 @font-lock-function-call-face
                                 operator: "/" right: (integer)))
       (call
        target: (dot right: (identifier) @font-lock-function-call-face))
       (unary_operator operator: "&" @font-lock-variable-use-face
                       operand: (integer) @font-lock-variable-use-face)
       (unary_operator operator: "&" @font-lock-operator-face
                       operand: (list)))

     :language 'elixir
     :feature 'elixir-string-escape
     :override t
     `((escape_sequence) @font-lock-escape-face)

     :language 'elixir
     :feature 'elixir-number
     '([(integer) (float)] @font-lock-number-face)

     :language 'elixir
     :feature 'elixir-variable
     '((binary_operator left: (identifier) @font-lock-variable-use-face)
       (binary_operator right: (identifier) @font-lock-variable-use-face)
       (arguments ( (identifier) @font-lock-variable-use-face))
       (tuple (identifier) @font-lock-variable-use-face)
       (list (identifier) @font-lock-variable-use-face)
       (pair value: (identifier) @font-lock-variable-use-face)
       (body (identifier) @font-lock-variable-use-face)
       (unary_operator operand: (identifier) @font-lock-variable-use-face)
       (interpolation (identifier) @font-lock-variable-use-face)
       (do_block (identifier) @font-lock-variable-use-face)
       (rescue_block (identifier) @font-lock-variable-use-face)
       (catch_block (identifier) @font-lock-variable-use-face)
       (else_block (identifier) @font-lock-variable-use-face)
       (after_block (identifier) @font-lock-variable-use-face)
       (access_call target: (identifier) @font-lock-variable-use-face)
       (access_call "[" key: (identifier) @font-lock-variable-use-face "]"))

     :language 'elixir
     :feature 'elixir-builtin
     :override t
     `(((identifier) @font-lock-builtin-face
        (:match ,elixir-ts--builtin-keywords-re
                @font-lock-builtin-face)))))
  "Tree-sitter font-lock settings.")

(defvar elixir-ts--font-lock-feature-list
  '(( elixir-comment elixir-doc elixir-definition)
    ( elixir-string elixir-keyword elixir-data-type)
    ( elixir-sigil elixir-builtin elixir-string-escape)
    ( elixir-function-call elixir-variable elixir-operator elixir-number ))
  "Tree-sitter font-lock feature list.")

(defvar elixir-ts--thing-settings
  `((defun ,(rx bos "call" eos))
    (sexp (not (or (and named
                        ,(rx bos (or "source" "keywords" "comment")
                             eos))
                   (and anonymous
                        ,(rx bos (or "{" "}" "[" "]" "(" ")" ","
                                     "do" "end")
                             eos)))))
    (list
     (or (and "\\`arguments\\'" ,#'elixir-ts--with-parens-0-p)
         (and "\\`unary_operator\\'" ,#'elixir-ts--with-parens-1-p)
         ,(rx bos (or "block"
                      "quoted_atom"
                      "string"
                      "interpolation"
                      "sigil"
                      "quoted_keyword"
                      "list"
                      "tuple"
                      "bitstring"
                      "map"
                      "do_block"
                      "anonymous_function")
              eos)))
    (sexp-default
     ;; For `C-M-f' in "&|(a)"
     ("(" . ,(lambda (node)
               (equal (treesit-node-type (treesit-node-parent node))
                      "unary_operator"))))
    (sentence
     ,(rx bos (or "call") eos))
    (text
     ,(rx bos (or "string" "sigil" "comment") eos)))
  "`treesit-thing-settings' for Elixir.")

(defvar elixir-ts--range-rules
  (when (treesit-available-p)
    (treesit-range-rules
     :embed 'heex
     :host 'elixir
     '((sigil (sigil_name) @_name
              (:match "^[HF]$" @_name)
              (quoted_content) @heex)))))

(defvar heex-ts--range-rules)
(defvar heex-ts--thing-settings)
(defvar heex-ts--indent-rules)
(defvar heex-ts--font-lock-settings)
(defvar heex-ts--font-lock-feature-list)

(defun elixir-ts--treesit-anchor-grand-parent-bol (_n parent &rest _)
  "Return the beginning of non-space characters for the parent node of PARENT."
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-parent parent)))
    (back-to-indentation)
    (point)))

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

(defun elixir-ts--with-parens-0-p (node)
  (equal (treesit-node-type (treesit-node-child node 0))
         "("))

(defun elixir-ts--with-parens-1-p (node)
  (equal (treesit-node-type (treesit-node-child node 1))
         "("))

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

(defun elixir-ts--electric-pair-string-delimiter ()
  "Insert corresponding multi-line string for `electric-pair-mode'."
  (when (and electric-pair-mode
             (eq last-command-event ?\")
             (let ((count 0))
               (while (eq (char-before (- (point) count)) last-command-event)
                 (incf count))
               (= count 3))
             (eq (char-after) last-command-event))
    (save-excursion
      (insert (make-string 2 last-command-event)))
    (save-excursion
      (newline 1 t))))

;;;###autoload
(define-derived-mode elixir-ts-mode prog-mode "Elixir"
  "Major mode for editing Elixir, powered by tree-sitter."
  :group 'elixir-ts
  :syntax-table elixir-ts--syntax-table

  ;; Comments.
  (setq-local comment-start "# ")
  (setq-local comment-start-skip
              (rx "#" (* (syntax whitespace))))

  (setq-local comment-end "")
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end) "\n"))))

  ;; Compile.
  (setq-local compile-command "mix")

  ;; Electric pair.
  (add-hook 'post-self-insert-hook
            #'elixir-ts--electric-pair-string-delimiter 'append t)

  (when (treesit-ensure-installed 'elixir)
    (setq-local treesit-primary-parser
                (treesit-parser-create 'elixir))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings elixir-ts--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                elixir-ts--font-lock-feature-list)

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                '((nil "\\`call\\'" elixir-ts--defun-p nil)))

    ;; Indent.
    (setq-local treesit-simple-indent-rules elixir-ts--indent-rules)

    ;; Navigation.
    (setq-local treesit-thing-settings
                `((elixir ,@elixir-ts--thing-settings)))
    (setq-local treesit-defun-type-regexp
                '("call" . elixir-ts--defun-p))

    (setq-local treesit-defun-name-function #'elixir-ts--defun-name)

    ;; Embedded Heex.
    (when (treesit-ensure-installed 'heex)
      (require 'heex-ts-mode)
      (treesit-parser-create 'heex)

      (setq-local treesit-range-settings
                  (append elixir-ts--range-rules
                          ;; Leave only local parsers from heex
                          ;; for elixir->heex->elixir embedding.
                          (seq-filter (lambda (r) (nth 2 r))
                                      heex-ts--range-rules)))

      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings
                          heex-ts--font-lock-settings))

      (setq-local treesit-simple-indent-rules
                  (append treesit-simple-indent-rules
                          heex-ts--indent-rules))

      (setq-local treesit-font-lock-feature-list
                  (treesit-merge-font-lock-feature-list
                   treesit-font-lock-feature-list
                   heex-ts--font-lock-feature-list))

      (setq-local treesit-thing-settings
                  (append treesit-thing-settings
                          `((heex ,@heex-ts--thing-settings)))))

    (treesit-major-mode-setup)

    (setq-local syntax-propertize-function #'elixir-ts--syntax-propertize)

    ;; Enable the 'sexp' navigation by default
    (setq-local forward-sexp-function #'treesit-forward-sexp
                treesit-sexp-thing 'sexp
                ;; But still use 'list' for `down-list' and `up-list'
                treesit-sexp-thing-down-list 'list
                treesit-sexp-thing-up-list 'list
                hs-treesit-things '(or defun sexp))))

(derived-mode-add-parents 'elixir-ts-mode '(elixir-mode))

;;;###autoload
(defun elixir-ts-mode-maybe ()
  "Enable `elixir-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'elixir)
          (eq treesit-enabled-modes t)
          (memq 'elixir-ts-mode treesit-enabled-modes))
      (elixir-ts-mode)
    (fundamental-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-ts-mode-maybe))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode-maybe))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode-maybe))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-ts-mode-maybe))
  ;; To be able to toggle between an external package and core ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(elixir-mode . elixir-ts-mode)))

(provide 'elixir-ts-mode)

;;; elixir-ts-mode.el ends here
