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

(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-child-count "treesit.c")
(declare-function treesit-node-first-child-for-pos "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-search-subtree "treesit.c")

(defgroup lua-ts nil
  "Major mode for editing Lua files."
  :prefix "lua-ts-"
  :group 'languages)

(defcustom lua-ts-mode-hook nil
  "Hook run after entering `lua-ts-mode'."
  :type 'hook
  :options '(flymake-mode
             hs-minor-mode
             outline-minor-mode)
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-indent-offset 4
  "Number of spaces for each indentation step in `lua-ts-mode'."
  :type 'natnum
  :safe 'natnump
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-luacheck-program "luacheck"
  "Location of the Luacheck program."
  :type '(choice (const :tag "None" nil) string)
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
  :type '(choice (const :tag "None" nil) string)
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-inferior-options '("-i")
  "Command line options for the inferior Lua process."
  :type '(repeat string)
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-inferior-startfile nil
  "File to load into the inferior Lua process at startup."
  :type '(choice (const :tag "None" nil) (file :must-match t))
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-inferior-prompt ">"
  "Prompt used by the inferior Lua process."
  :type 'string
  :safe 'stringp
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-inferior-prompt-continue ">>"
  "Continuation prompt used by the inferior Lua process."
  :type 'string
  :safe 'stringp
  :group 'lua-ts
  :version "30.1")

(defcustom lua-ts-inferior-history nil
  "File used to save command history of the inferior Lua process."
  :type '(choice (const :tag "None" nil) file)
  :safe 'string-or-null-p
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

(defvar lua-ts--keywords
  '("and" "do" "else" "elseif" "end" "for" "function" "goto" "if"
    "in" "local" "not" "or" "repeat" "return" "then" "until" "while")
  "Lua keywords for tree-sitter font-locking and navigation.")

(defun lua-ts--comment-font-lock (node override start end &rest _)
  "Apply font lock to comment NODE within START and END.
Applies `font-lock-comment-delimiter-face' and
`font-lock-comment-face' See `treesit-fontify-with-override' for
values of OVERRIDE."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (node-text (treesit-node-text node t))
         (delimiter-end (+ 2 node-start)))
    (when (and (>= node-start start)
               (<= delimiter-end end)
               (string-match "\\`--" node-text))
      (treesit-fontify-with-override node-start
                                     delimiter-end
                                     font-lock-comment-delimiter-face
                                     override))
    (treesit-fontify-with-override (max delimiter-end start)
                                   (min node-end end)
                                   font-lock-comment-face
                                   override)))

(defvar lua-ts--font-lock-settings
  (treesit-font-lock-rules
   :default-language 'lua
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :feature 'delimiter
   '(["," ";"] @font-lock-delimiter-face)

   :feature 'constant
   '([(variable_list
       attribute: (attribute (["<" ">"] (identifier))))
      (label_statement)
      (true) (false) (nil)]
     @font-lock-constant-face)

   :feature 'operator
   '(["+" "-" "*" "/" "%" "^" "#" "==" "~=" "<=" ">="
      "<" ">" "=" "&" "~" "|" "<<" ">>" "//" ".."
      (vararg_expression)]
     @font-lock-operator-face)

   :feature 'builtin
   `(((identifier) @font-lock-builtin-face
      (:match ,(regexp-opt lua-ts--builtins 'symbols)
              @font-lock-builtin-face)))

   :feature 'function
   '((function_call name: (identifier) @font-lock-function-call-face)
     (function_call
      (method_index_expression
       method: (identifier) @font-lock-function-call-face))
     (function_call
      (dot_index_expression
       field: (identifier) @font-lock-function-call-face)))

   :feature 'punctuation
   '(["." ":"] @font-lock-punctuation-face)

   :feature 'variable
   '((function_call
      (arguments (identifier) @font-lock-variable-use-face))
     (function_call
      (arguments
       (binary_expression (identifier) @font-lock-variable-use-face)))
     (function_call
      (arguments
       (bracket_index_expression (identifier) @font-lock-variable-use-face)))
     (function_declaration
      (parameters name: (identifier) @font-lock-variable-name-face)))

   :feature 'number
   '((number) @font-lock-number-face)

   :feature 'keyword
   `([(break_statement)
      ,(vconcat lua-ts--keywords)]
     @font-lock-keyword-face
     (goto_statement ((identifier) @font-lock-constant-face)))

   :feature 'string
   '((string) @font-lock-string-face)

   :feature 'escape
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :feature 'comment
   '((comment) @lua-ts--comment-font-lock
     (hash_bang_line) @font-lock-comment-face)

   :feature 'definition
   '((function_declaration
      (identifier) @font-lock-function-name-face)
     (function_declaration
      (dot_index_expression
       field: (identifier) @font-lock-function-name-face))
     (function_declaration
      (method_index_expression
       method: (identifier) @font-lock-function-name-face))
     (assignment_statement
      (variable_list
       (identifier) @font-lock-function-name-face)
      (expression_list value: (function_definition)))
     (field
      name: (identifier) @font-lock-function-name-face
      value: (function_definition))
     (assignment_statement
      (variable_list
       (dot_index_expression
        field: (identifier) @font-lock-function-name-face))
      (expression_list
       value:
       (function_definition))))

   :feature 'assignment
   '((variable_list (identifier) @font-lock-variable-name-face)
     (variable_list
      (bracket_index_expression
       field: (identifier) @font-lock-variable-name-face))
     (variable_list
      (dot_index_expression
       field: (identifier) @font-lock-variable-name-face))
     (for_numeric_clause name: (identifier) @font-lock-variable-name-face))

   :feature 'property
   '((field name: (identifier) @font-lock-property-name-face)
     (dot_index_expression
      field: (identifier) @font-lock-property-use-face))

   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `lua-ts-mode'.")

(defvar lua-ts--simple-indent-rules
  `((lua
     ((or (node-is "comment")
          (parent-is "comment_content")
          (parent-is "string_content")
          (node-is "]]"))
      no-indent 0)
     ((and (n-p-gp "field" "table_constructor" "arguments")
           lua-ts--multi-arg-function-call-matcher)
      parent lua-ts-indent-offset)
     ((and (n-p-gp "}" "table_constructor" "arguments")
           lua-ts--multi-arg-function-call-matcher)
      parent 0)
     ((or (node-is "do")
          (node-is "then")
          (node-is "elseif_statement")
          (node-is "else_statement")
          (node-is "until")
          (node-is ")")
          (node-is "}"))
      standalone-parent 0)
     ((or (and (parent-is "arguments") lua-ts--first-child-matcher)
          (and (parent-is "parameters") lua-ts--first-child-matcher)
          (and (parent-is "table_constructor") lua-ts--first-child-matcher))
      standalone-parent lua-ts-indent-offset)
     ((or (parent-is "arguments")
          (parent-is "parameters")
          (parent-is "table_constructor"))
      (nth-sibling 1) 0)
     ((and (n-p-gp "block" "function_definition" "parenthesized_expression")
           lua-ts--nested-function-block-matcher
           lua-ts--nested-function-block-include-matcher)
      parent lua-ts-indent-offset)
     ((and (n-p-gp "block" "function_definition" "arguments")
           lua-ts--nested-function-argument-matcher)
      parent lua-ts-indent-offset)
     ((match "function_definition" "parenthesized_expression")
      standalone-parent lua-ts-indent-offset)
     ((node-is "block") standalone-parent lua-ts-indent-offset)
     ((parent-is "block") parent 0)
     ((and (node-is "end") lua-ts--end-line-matcher)
      standalone-parent lua-ts--end-indent-offset)
     ((match "end" "function_declaration") parent 0)
     ((and (n-p-gp "end" "function_definition" "parenthesized_expression")
           lua-ts--nested-function-end-argument-matcher)
      parent 0)
     ((and (n-p-gp "end" "function_definition" "parenthesized_expression")
           lua-ts--nested-function-block-matcher
           lua-ts--nested-function-end-matcher
           lua-ts--nested-function-last-function-matcher)
      parent 0)
     ((n-p-gp "end" "function_definition" "arguments") parent 0)
     ((or (match "end" "function_definition")
          (node-is "end"))
      standalone-parent 0)
     ((or (parent-is "function_declaration")
          (parent-is "function_definition")
          (parent-is "do_statement")
          (parent-is "for_statement")
          (parent-is "repeat_statement")
          (parent-is "while_statement")
          (parent-is "if_statement")
          (parent-is "else_statement")
          (parent-is "elseif_statement"))
      standalone-parent lua-ts-indent-offset)
     ((parent-is "chunk") column-0 0)
     ((parent-is "ERROR") no-indent 0))))

(defun lua-ts--end-line-matcher (&rest _)
  "Matches if there is more than one `end' on the current line."
  (> (lua-ts--end-count) 1))

(defun lua-ts--end-indent-offset (&rest _)
  "Calculate indent offset based on `end' count."
  (- (* (1- (lua-ts--end-count)) lua-ts-indent-offset)))

(defun lua-ts--end-count ()
  "Count the number of `end's on the current line."
  (count-matches "end" (line-beginning-position) (line-end-position)))

(defun lua-ts--first-child-matcher (node &rest _)
  "Matches if NODE is the first among its siblings."
  (= (treesit-node-index node) 1))

(defun lua-ts--function-definition-p (node)
  "Return t if NODE is a function_definition."
  (equal "function_definition" (treesit-node-type node)))

(defun lua-ts--g-g-g-parent (node)
  "Return the great-great-grand-parent of NODE."
  (let* ((parent (treesit-node-parent node))
         (g-parent (treesit-node-parent parent))
         (g-g-parent (treesit-node-parent g-parent)))
    (treesit-node-parent g-g-parent)))

(defun lua-ts--multi-arg-function-call-matcher (_n parent &rest _)
  "Matches if PARENT has multiple arguments."
  (> (treesit-node-child-count (treesit-node-parent parent)) 3))

(defun lua-ts--nested-function-argument-matcher (node &rest _)
  "Matches if NODE is in a nested function argument."
  (save-excursion
    (goto-char (treesit-node-start node))
    (treesit-beginning-of-defun)
    (backward-char 2)
    (not (looking-at ")("))))

(defun lua-ts--nested-function-block-matcher (node &rest _)
  "Matches if NODE is in a nested function block."
  (let* ((g-g-g-parent (lua-ts--g-g-g-parent node))
         (g-g-g-type (treesit-node-type g-g-g-parent)))
    (not (equal g-g-g-type "chunk"))))

(defun lua-ts--nested-function-block-include-matcher (node _p bol &rest _)
  "Matches if NODE's child at BOL is not another block."
  (let* ((child (treesit-node-first-child-for-pos node bol))
         (child-type (treesit-node-type child))
         (g-g-g-type (treesit-node-type (lua-ts--g-g-g-parent node))))
    (or (equal child-type "assignment_statement")
        (and (equal child-type "return_statement")
             (or (equal g-g-g-type "arguments")
                 (and (equal g-g-g-type "expression_list")
                      (not (treesit-search-subtree child "function_call"))))))))

(defun lua-ts--nested-function-end-matcher (node &rest _)
  "Matches if NODE is the `end' of a nested function."
  (save-excursion
    (goto-char (treesit-node-start node))
    (treesit-beginning-of-defun)
    (looking-at "function[[:space:]]*")))

(defun lua-ts--nested-function-end-argument-matcher (node &rest _)
  "Matches if great-great-grandparent of NODE is arguments."
  (equal "arguments" (treesit-node-type (lua-ts--g-g-g-parent node))))

(defun lua-ts--nested-function-last-function-matcher (_n parent &rest _)
  "Matches if PARENT is the last nested function."
  (let ((sparse-tree
         (treesit-induce-sparse-tree parent #'lua-ts--function-definition-p)))
    (= 1 (length (cadr sparse-tree)))))

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

(defun lua-ts--named-function-p (node)
  "Matches if NODE is a named function."
  (let ((type (treesit-node-type node)))
    (or (equal "function_declaration" type)
        (and (equal "field" type)
             (equal "function_definition"
                    (treesit-node-type
                     (treesit-node-child-by-field-name
                      node "value")))
             (treesit-node-child-by-field-name node "name")))))

(defun lua-ts--require-name-function (node)
  "Return name of NODE to use for requires in imenu."
  (when-let* (((lua-ts--require-p node))
              (parent (treesit-node-parent node))
              (parent-type (treesit-node-type parent)))
    (if (equal "expression_list" parent-type)
        (let* ((g-parent (treesit-node-parent parent))
               (name (treesit-node-child-by-field-name g-parent "name")))
          (treesit-node-text name t))
      (treesit-node-text (treesit-search-subtree node "string_content") t))))

(defun lua-ts--require-p (node)
  "Matches if NODE is a require statement."
  (let ((name (treesit-node-child-by-field-name node "name")))
    (equal "require" (treesit-node-text name t))))

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
                            for (beg . end) = (flymake-diag-region
                                               source
                                               (string-to-number (match-string 1))
                                               (string-to-number (match-string 2)))
                            for msg = (match-string 4)
                            for type = (if (string-match "^(W" msg)
                                           :warning
                                         :error)
                            when (and beg end)
                            collect (flymake-make-diagnostic source
                                                             beg
                                                             end
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
  (unless (comint-check-proc lua-ts-inferior-buffer)
    (apply #'make-comint-in-buffer
           (string-replace "*" "" lua-ts-inferior-buffer)
           lua-ts-inferior-buffer
           lua-ts-inferior-program
           lua-ts-inferior-startfile
           lua-ts-inferior-options)
    (when lua-ts-inferior-history
        (set-process-sentinel (get-buffer-process lua-ts-inferior-buffer)
                              'lua-ts-inferior--write-history))
    (with-current-buffer lua-ts-inferior-buffer
      (setq-local comint-input-ignoredups t
                  comint-input-ring-file-name lua-ts-inferior-history
                  comint-use-prompt-regexp t
                  comint-prompt-read-only t
                  comint-prompt-regexp (rx-to-string `(: bol
                                                         ,lua-ts-inferior-prompt
                                                         (1+ space))))
      (comint-read-input-ring t)
      (add-hook 'comint-preoutput-filter-functions
                (lambda (string)
                  (if (or (not (equal (buffer-name) lua-ts-inferior-buffer))
                          (equal string
                                 (concat lua-ts-inferior-prompt-continue " ")))
                      string
                    (concat
                     ;; Filter out the extra prompt characters that
                     ;; accumulate in the output when sending regions
                     ;; to the inferior process.
                     (replace-regexp-in-string (rx-to-string
                                                `(: bol
                                                    (* ,lua-ts-inferior-prompt
                                                       (? ,lua-ts-inferior-prompt)
                                                       (1+ space))
                                                    (group (* nonl))))
                                               "\\1" string)
                     ;; Re-add the prompt for the next line.
                     lua-ts-inferior-prompt " "))))))
  (select-window (display-buffer lua-ts-inferior-buffer
                                 '((display-buffer-reuse-window
                                    display-buffer-pop-up-frame)
                                   (reusable-frames . t))))
  (get-buffer-process (current-buffer)))

(defun lua-ts-send-buffer ()
  "Send current buffer to the inferior Lua process."
  (interactive)
  (lua-ts-send-region (point-min) (point-max)))

(defun lua-ts-send-file (file)
  "Send contents of FILE to the inferior Lua process."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents-literally file)
    (lua-ts-send-region (point-min) (point-max))))

(defun lua-ts-send-region (beg end)
  "Send region between BEG and END to the inferior Lua process."
  (interactive "r")
  (let ((string (buffer-substring-no-properties beg end))
        (proc-buffer (lua-ts-inferior-lua)))
    (comint-send-string proc-buffer "print()") ; Prevent output from
    (comint-send-string proc-buffer "\n")      ; appearing at prompt.
    (comint-send-string proc-buffer string)
    (comint-send-string proc-buffer "\n")))

(defun lua-ts-show-process-buffer ()
  "Show the inferior Lua process buffer."
  (interactive)
  (display-buffer lua-ts-inferior-buffer))

(defun lua-ts-hide-process-buffer ()
  "Hide the inferior Lua process buffer."
  (interactive)
  (delete-windows-on lua-ts-inferior-buffer))

(defun lua-ts-kill-process ()
  "Kill the inferior Lua process."
  (interactive)
  (with-current-buffer lua-ts-inferior-buffer
    (kill-buffer-and-window)))

(defun lua-ts-inferior--write-history (process _)
  "Write history file for inferior Lua PROCESS."
  ;; Depending on how the process is killed the buffer may not be
  ;; around anymore; e.g. `kill-buffer'.
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p (process-buffer process))))
    (with-current-buffer buffer (comint-write-input-ring))))

(defvar lua-ts-mode-map
  (let ((map (make-sparse-keymap "Lua")))
    (define-key map "\C-c\C-n" 'lua-ts-inferior-lua)
    (define-key map "\C-c\C-c" 'lua-ts-send-buffer)
    (define-key map "\C-c\C-l" 'lua-ts-send-file)
    (define-key map "\C-c\C-r" 'lua-ts-send-region)
    map)
  "Keymap for `lua-ts-mode' buffers.")

(easy-menu-define lua-ts-mode-menu lua-ts-mode-map
  "Menu bar entry for `lua-ts-mode'."
  `("Lua"
    ["Evaluate Buffer" lua-ts-send-buffer]
    ["Evaluate File" lua-ts-send-file]
    ["Evaluate Region" lua-ts-send-region]
    "--"
    ["Start Process" lua-ts-inferior-lua]
    ["Show Process Buffer" lua-ts-show-process-buffer]
    ["Hide Process Buffer" lua-ts-hide-process-buffer]
    ["Kill Process" lua-ts-kill-process]
    "--"
    ["Customize" (lambda () (interactive) (customize-group "lua-ts"))]))

;;;###autoload
(define-derived-mode lua-ts-mode prog-mode "Lua"
  "Major mode for editing Lua files, powered by tree-sitter.

\\{lua-ts-mode-map}"
  :syntax-table lua-ts--syntax-table
  (use-local-map lua-ts-mode-map)

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
                  (keyword string)
                  (assignment builtin constant number)
                  (bracket
                   delimiter
                   escape
                   function
                   operator
                   property
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
                   (function ,(rx (or "function_declaration"
                                      "function_definition")))
                   (keyword ,(regexp-opt lua-ts--keywords
                                         'symbols))
                   (loop-statement ,(rx (or "do_statement"
                                            "for_statement"
                                            "repeat_statement"
                                            "while_statement")))
                   (sentence (or function
                                 loop-statement
                                 ,(rx (or "assignment_statement"
                                          "comment"
                                          "field"
                                          "function_call"
                                          "if_statement"
                                          "return_statement"
                                          "variable_declaration"))))
                   (sexp (or function
                             keyword
                             loop-statement
                             ,(rx (or "arguments"
                                      "break_statement"
                                      "expression_list"
                                      "false"
                                      "identifier"
                                      "nil"
                                      "number"
                                      "parameters"
                                      "parenthesized_expression"
                                      "string"
                                      "table_constructor"
                                      "true"
                                      "vararg_expression"))))
                   (text "comment"))))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Requires"
                   "\\`function_call\\'"
                   lua-ts--require-p
                   lua-ts--require-name-function)
                  ("Variables" "\\`variable_declaration\\'" nil nil)
                  (nil
                   "\\`\\(?:f\\(?:ield\\|unction_declaration\\)\\)\\'"
                   lua-ts--named-function-p
                   nil)))

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

    ;; Align.
    (setq-local align-indent-before-aligning t)

    (treesit-major-mode-setup))

  (add-hook 'flymake-diagnostic-functions #'lua-ts-flymake-luacheck nil 'local))

(when (treesit-ready-p 'lua)
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode)))

(provide 'lua-ts-mode)

;;; lua-ts-mode.el ends here
