;;; lua-ts-mode.el --- Major mode for editing Lua files -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: John Muhl <jm@pub.pink>
;; Created: June 27, 2023
;; Keywords: lua languages tree-sitter

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides `lua-ts-mode' which is a major mode for Lua
;; files that uses Tree Sitter to parse the language.
;;
;; This package is compatible with and tested against the grammar
;; for Lua found at https://github.com/MunifTanjim/tree-sitter-lua

;;; Code:

(require 'comint)
(require 'treesit)

(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-search-subtree "treesit.c")

(defgroup lua-ts nil
  "Major mode for editing Lua files."
  :prefix "lua-ts-"
  :group 'languages)

(defcustom lua-ts-indent-offset 4
  "Number of spaces for each indentation step in `lua-ts-mode'."
  :type 'natnum
  :safe 'natnump
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-luacheck-program "luacheck"
  "Location of the Luacheck program."
  :type '(choice (const nil) string)
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-inferior-buffer "*Lua*"
  "Name of the inferior Lua buffer."
  :type 'string
  :safe 'stringp
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-inferior-program "lua"
  "Program to run in the inferior Lua process."
  :type '(choice (const nil) string)
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-inferior-options '("-i")
  "Command line options for the inferior Lua process."
  :type '(repeat string)
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-inferior-startfile nil
  "File to load into the inferior Lua process at startup."
  :type '(choice (const nil) (file :must-match t))
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-inferior-prompt-regexp "^>>?[[:blank:]]"
  "Regular expression matching the prompt of the inferior Lua process."
  :type 'regexp
  :group 'lua-ts
  :version "30.1")

(defvar lua-ts--builtins
  '("assert" "bit32" "collectgarbage" "coroutine" "debug" "dofile"
    "error" "getmetatable" "io" "ipairs" "load" "loadfile"
    "math" "next" "os" "package" "pairs" "pcall" "print"
    "rawequal" "rawget" "rawlen" "rawset" "require" "select"
    "setmetatable" "string" "table" "tonumber" "tostring"
    "type" "utf8" "warn" "xpcall" "_G" "_VERSION"
    ;; methods for file handlers
    "close" "flush" "lines" "read" "seek" "setvbuf" "write")
  "Lua built-in functions for tree-sitter font-locking.")

(defvar lua-ts--font-lock-settings
  (treesit-font-lock-rules
   :language 'lua
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'lua
   :feature 'delimiter
   '(["," ";"] @font-lock-delimiter-face)

   :language 'lua
   :feature 'escape
   '((escape_sequence) @font-lock-escape-face)

   :language 'lua
   :feature 'constant
   '((variable_list
      attribute: (attribute (["<" ">"] (identifier))))
     @font-lock-constant-face)

   :language 'lua
   :feature 'operator
   '(["and" "not" "or" "+" "-" "*" "/" "%" "^"
      "#" "==" "~=" "<=" ">=" "<" ">" "=" "&"
      "~" "|" "<<" ">>" "//" ".."]
     @font-lock-operator-face
     (vararg_expression) @font-lock-operator-face)

   :language 'lua
   :feature 'property
   '((field name: (identifier) @font-lock-property-name-face)
     (dot_index_expression
      field: (identifier) @font-lock-property-use-face))

   :language 'lua
   :feature 'builtin
   `(((identifier) @font-lock-builtin-face
      (:match ,(regexp-opt lua-ts--builtins 'symbols)
              @font-lock-builtin-face)))

   :language 'lua
   :feature 'function
   '((function_call name: (identifier) @font-lock-function-call-face)
     (function_call
      name: (method_index_expression
             method: (identifier) @font-lock-function-call-face))
     (function_call
      name: (dot_index_expression
             table: (identifier) @font-lock-function-call-face)))

   :language 'lua
   :feature 'punctuation
   '(["." ":"] @font-lock-punctuation-face)

   :language 'lua
   :feature 'variable
   '((function_call
      arguments: (arguments (identifier))
      @font-lock-variable-use-face)
     (function_call
      name: (method_index_expression
             table: (identifier) @font-lock-variable-use-face))
     (goto_statement (identifier) @font-lock-variable-use-face))

   :language 'lua
   :feature 'assignment
   '((variable_list (identifier) @font-lock-variable-name-face))

   :language 'lua
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'lua
   :feature 'keyword
   '((break_statement) @font-lock-keyword-face
     (true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (nil) @font-lock-constant-face
     ["and" "do" "else" "elseif" "end" "for" "function"
      "goto" "if" "in" "local" "not" "or" "repeat"
      "return" "then" "until" "while"]
     @font-lock-keyword-face)

   :language 'lua
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'lua
   :feature 'comment
   '((comment) @font-lock-comment-face
     (hash_bang_line) @font-lock-comment-face)

   :language 'lua
   :feature 'definition
   '((function_declaration
      name: (identifier) @font-lock-function-name-face)
     (parameters
      name: (identifier) @font-lock-variable-name-face)
     (label_statement) @font-lock-variable-name-face)

   :language 'lua
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `lua-ts-mode'.")

(defvar lua-ts--simple-indent-rules
  `((lua
     ((parent-is "chunk") column-0 0)
     ((node-is "comment_end") column-0 0)
     ((parent-is "block") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "else_statement") parent-bol 0)
     ((node-is "elseif_statement") parent-bol 0)
     ((node-is "end") parent-bol 0)
     ((node-is "until") parent-bol 0)
     ((parent-is "for_statement") parent-bol lua-ts-indent-offset)
     ((parent-is "function_declaration") parent-bol lua-ts-indent-offset)
     ((parent-is "function_definition") parent-bol lua-ts-indent-offset)
     ((parent-is "if_statement") parent-bol lua-ts-indent-offset)
     ((parent-is "else_statement") parent-bol lua-ts-indent-offset)
     ((parent-is "repeat_statement") parent-bol lua-ts-indent-offset)
     ((parent-is "while_statement") parent-bol lua-ts-indent-offset)
     ((parent-is "table_constructor") parent-bol lua-ts-indent-offset)
     ((parent-is "arguments") parent-bol lua-ts-indent-offset)
     ((parent-is "parameters") parent-bol lua-ts-indent-offset)
     ((parent-is "ERROR") no-indent 0))))

(defvar lua-ts--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+  "."    table)
    (modify-syntax-entry ?-  ". 12" table)
    (modify-syntax-entry ?=  "."    table)
    (modify-syntax-entry ?%  "."    table)
    (modify-syntax-entry ?^  "."    table)
    (modify-syntax-entry ?~  "."    table)
    (modify-syntax-entry ?<  "."    table)
    (modify-syntax-entry ?>  "."    table)
    (modify-syntax-entry ?/  "."    table)
    (modify-syntax-entry ?*  "."    table)
    (modify-syntax-entry ?\n ">"    table)
    (modify-syntax-entry ?\' "\""   table)
    (modify-syntax-entry ?\" "\""   table)
    table)
  "Syntax table for `lua-ts-mode'.")

(defun lua-ts--defun-name-function (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (let ((child (treesit-node-child-by-field-name node "name")))
    (pcase (treesit-node-type node)
      ((or "function_declaration" "function_definition")
       (treesit-node-text child t))
      ("variable_declaration"
       (if child
           (treesit-node-text child t)
         (treesit-node-text
          (treesit-node-child-by-field-name
           (treesit-search-subtree node "assignment_statement" nil nil 1)
           "name"))))
      ("field"
       (and (treesit-search-subtree node "function_definition" nil nil 1)
            (treesit-node-text child t))))))

(defvar-local lua-ts--flymake-process nil)

(defun lua-ts-flymake-luacheck (report-fn &rest _args)
  "Luacheck backend for Flymake.
Calls REPORT-FN directly."
  (when (process-live-p lua-ts--flymake-process)
    (kill-process lua-ts--flymake-process))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq lua-ts--flymake-process
            (make-process
             :name "lua-ts-flymake-luacheck"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer " *lua-ts-flymake-luacheck*")
             :command `(,lua-ts-luacheck-program
                        "--codes" "--ranges" "--formatter" "plain" "-")
             :sentinel
             (lambda (proc _event)
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     (if (with-current-buffer source
                           (eq proc lua-ts--flymake-process))
                         (with-current-buffer (process-buffer proc)
                           (goto-char (point-min))
                           (cl-loop
                            while (search-forward-regexp
                                   (rx (seq bol
                                            (0+ alnum) ":"
                                            (group (1+ digit)) ":"
                                            (group (1+ digit)) "-"
                                            (group (1+ digit)) ": "
                                            (group (0+ nonl))
                                            eol))
                                   nil t)
                            for line = (string-to-number (match-string 1))
                            for beg = (string-to-number (match-string 2))
                            for end = (string-to-number (match-string 3))
                            for msg = (match-string 4)
                            for type = (if (string-match "^(W" msg)
                                           :warning
                                         :error)
                            when (and beg end)
                            collect (flymake-make-diagnostic source
                                                             (cons line beg)
                                                             (cons line (1+ end))
                                                             type
                                                             msg)
                            into diags
                            finally (funcall report-fn diags)))
                       (flymake-log :warning "Canceling obsolete check %s" proc))
                   (kill-buffer (process-buffer proc)))))))
      (process-send-region lua-ts--flymake-process (point-min) (point-max))
      (process-send-eof lua-ts--flymake-process))))

;;;###autoload
(defun lua-ts-inferior-lua ()
  "Run a Lua interpreter in an inferior process."
  (interactive)
  (let* ((buffer lua-ts-inferior-buffer)
         (name (string-replace "*" "" buffer))
         (program lua-ts-inferior-program)
         (prompt-regexp lua-ts-inferior-prompt-regexp)
         (switches lua-ts-inferior-options)
         (startfile lua-ts-inferior-startfile))
    (unless (comint-check-proc buffer)
      (set-buffer (apply (function make-comint) name program startfile switches))
      (setq-local comint-input-ignoredups t
                  comint-prompt-read-only t
                  comint-prompt-regexp prompt-regexp
                  comint-use-prompt-regexp t))
    (select-window (display-buffer buffer '((display-buffer-reuse-window
                                             display-buffer-pop-up-frame)
                                            (reusable-frames . t))))))

;;;###autoload
(define-derived-mode lua-ts-mode prog-mode "Lua"
  "Major mode for editing Lua files, powered by tree-sitter."
  :syntax-table lua-ts--syntax-table

  (when (treesit-ready-p 'lua)
    (treesit-parser-create 'lua)

    ;; Comments.
    (setq-local comment-start "--")
    (setq-local comment-start-skip "--\\s-*")
    (setq-local comment-end "")

    ;; Font-lock.
    (setq-local treesit-font-lock-settings lua-ts--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword property string)
                  (assignment builtin constant number)
                  (bracket
                   delimiter
                   escape
                   function
                   operator
                   punctuation
                   variable)))

    ;; Indent.
    (setq-local treesit-simple-indent-rules lua-ts--simple-indent-rules)

    ;; Navigation.
    (setq-local treesit-defun-name-function #'lua-ts--defun-name-function)
    (setq-local treesit-defun-type-regexp
                (rx (or "function_declaration" "function_definition")))
    (setq-local treesit-thing-settings
                `((lua
                   (sentence ,(rx (or "do_statement"
                                      "field"
                                      "for_statement"
                                      "function_call"
                                      "if_statement"
                                      "repeat_statement"
                                      "return_statement"
                                      "variable_declaration"
                                      "while_statement")))
                   (sexp ,(rx (or "arguments"
                                  "block"
                                  "parameters"
                                  "string"
                                  "table_constructor")))
                   (text "comment"))))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Variable" ,(rx bos "variable_declaration" eos) nil nil)
                  ("Function" ,(rx bos
                                   (or "function_declaration"
                                       "function_definition"
                                       "field")
                                   eos)
                   nil nil)))

    ;; Which-function.
    (setq-local which-func-functions (treesit-defun-at-point))

    ;; Outline.
    (setq-local outline-regexp
                (rx (seq (0+ space)
                         (or (seq "--[[" (0+ space) eol)
                             (seq symbol-start
                                  (or "do" "for" "if" "repeat" "while"
                                      (seq (? (seq "local" (1+ space)))
                                           "function"))
                                  symbol-end)))))

    (treesit-major-mode-setup))

  (add-hook 'flymake-diagnostic-functions #'lua-ts-flymake-luacheck nil 'local))

(when (treesit-ready-p 'lua)
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode)))

(provide 'lua-ts-mode)

;;; lua-ts-mode.el ends here
