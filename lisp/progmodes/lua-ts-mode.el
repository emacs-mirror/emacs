;;; lua-ts-mode.el --- Major mode for editing Lua files -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

;;; Tree-sitter language versions
;;
;; lua-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-lua: v0.3.0-1-gdb16e76
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

;;; Commentary:

;; This package provides `lua-ts-mode' which is a major mode for Lua
;; files that uses Tree Sitter to parse the language.
;;
;; This package is compatible with and tested against the grammar for
;; Lua found at https://github.com/tree-sitter-grammars/tree-sitter-lua

;;; Code:

(require 'c-ts-common)
(require 'comint)
(require 'treesit)
(treesit-declare-unavailable-functions)

(eval-when-compile
  (require 'rx))

(add-to-list
 'treesit-language-source-alist
 '(lua "https://github.com/tree-sitter-grammars/tree-sitter-lua"
       :commit "db16e76558122e834ee214c8dc755b4a3edc82a9")
 t)

(defgroup lua-ts nil
  "Major mode for editing Lua files."
  :prefix "lua-ts-"
  :group 'languages)

(defcustom lua-ts-mode-hook nil
  "Hook run after entering `lua-ts-mode'."
  :type 'hook
  :options '(eglot-ensure
             flymake-mode
             hs-minor-mode
             outline-minor-mode)
  :version "30.1")

(defcustom lua-ts-indent-offset 4
  "Number of spaces for each indentation step in `lua-ts-mode'."
  :type 'natnum
  :safe 'natnump
  :version "30.1")

(defcustom lua-ts-auto-close-block-comments nil
  "If non-nil, inserting a block comment \"--[[\" will insert its respective \"]]\"."
  :type 'boolean
  :version "31.1")

(defcustom lua-ts-luacheck-program "luacheck"
  "Location of the Luacheck program."
  :type 'file
  :version "30.1")

(defcustom lua-ts-inferior-buffer "*Lua*"
  "Name of the inferior Lua buffer."
  :type 'string
  :safe 'stringp
  :version "30.1")

(defcustom lua-ts-inferior-program "lua"
  "Program to run in the inferior Lua process."
  :type 'file
  :version "30.1")

(defcustom lua-ts-inferior-options '("-i")
  "Command line options for the inferior Lua process."
  :type '(repeat string)
  :version "30.1")

(defcustom lua-ts-inferior-startfile nil
  "File to load into the inferior Lua process at startup."
  :type '(choice (const :tag "None" nil) (file :must-match t))
  :version "30.1")

(defcustom lua-ts-inferior-prompt ">"
  "Prompt used by the inferior Lua process."
  :type 'string
  :safe 'stringp
  :version "30.1")

(defcustom lua-ts-inferior-prompt-continue ">>"
  "Continuation prompt used by the inferior Lua process."
  :type 'string
  :safe 'stringp
  :version "30.1")

(defcustom lua-ts-inferior-history nil
  "File used to save command history of the inferior Lua process."
  :type '(choice (const :tag "None" nil) file)
  :safe 'string-or-null-p
  :version "30.1")

(defcustom lua-ts-indent-continuation-lines t
  "Controls how multi-line if/else statements are aligned.

If non-nil, then continuation lines are indented by `lua-ts-indent-offset':

  if a
      and b then
      print(1)
  end

If nil, then continuation lines are aligned with the beginning of
the statement:

  if a
  and b then
      print(1)
  end"
  :type 'boolean
  :safe 'booleanp
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
`font-lock-comment-face'.  See `treesit-fontify-with-override' for
values of OVERRIDE."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (node-text (treesit-node-text node t))
         (delimiter-end (progn
                          (goto-char node-start)
                          (while (looking-at-p "-") (forward-char))
                          (point))))
    (when (and (>= node-start start)
               (<= delimiter-end end)
               (string-match "\\`---*" node-text))
      (treesit-fontify-with-override node-start
                                     delimiter-end
                                     'font-lock-comment-delimiter-face
                                     override))
    (treesit-fontify-with-override (max delimiter-end start)
                                   (min node-end end)
                                   'font-lock-comment-face
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
     ;; Handle multi-line strings and comments.
     ((or (and (node-is "comment") (parent-is "chunk"))
          lua-ts--multi-line-comment-start
          (parent-is "comment_content")
          (parent-is "string_content")
          (or (node-is "]]") (node-is "comment_end")))
      no-indent 0)

     ;; Handle multiple "end" statements on a single line.
     ((and (node-is "end") lua-ts--end-line-matcher)
      standalone-parent lua-ts--end-indent-offset)

     ;; Handle tables in the arguments of a function call.
     ((and (n-p-gp "field" "table_constructor" "arguments")
           lua-ts--multi-arg-function-call-matcher
           lua-ts--last-arg-function-call-matcher)
      standalone-parent lua-ts-indent-offset)
     ((and (n-p-gp "}" "table_constructor" "arguments")
           lua-ts--multi-arg-function-call-matcher
           lua-ts--last-arg-function-call-matcher)
      standalone-parent 0)
     ((and (match "field" "table_constructor" nil 1 1)
           lua-ts--multi-arg-function-call-matcher)
      parent lua-ts-indent-offset)
     ((and (n-p-gp "}" "table_constructor" "arguments")
           lua-ts--multi-arg-function-call-matcher)
      parent 0)

     ;; Handle multi-line concatenation and continuation.
     ((or (n-p-gp "expression_list" "assignment_statement" "variable_declaration")
          (and (parent-is "binary_expression")
               lua-ts--variable-declaration-continuation))
      lua-ts--variable-declaration-continuation-anchor
      lua-ts-indent-offset)
     ;; `lua-ts-indent-continuation-lines' is non-nil.
     ((and (lambda (&rest _) lua-ts-indent-continuation-lines)
           (parent-is "binary_expression"))
      standalone-parent lua-ts-indent-offset)
     ;; `lua-ts-indent-continuation-lines' is nil.
     ((parent-is "binary_expression") standalone-parent 0)

     ;; Handle immediately invoked function expressions.
     ((or (n-p-gp "block" "function_definition" "parenthesized_expression")
          (n-p-gp "block" "function_definition" "arguments"))
      parent lua-ts-indent-offset)
     ((or (n-p-gp "end" "function_definition" "parenthesized_expression")
          (n-p-gp "end" "function_definition" "arguments"))
      parent 0)

     ;; Handle basic indentation.
     ((or (node-is "do")
          (node-is "then")
          (node-is "elseif_statement")
          (node-is "else_statement")
          (node-is "until")
          (node-is "end")
          (node-is ")"))
      standalone-parent 0)

     ((or (parent-is "function_declaration")
          (parent-is "function_definition")
          (parent-is "do_statement")
          (parent-is "for_statement")
          (parent-is "repeat_statement")
          (parent-is "while_statement")
          (parent-is "if_statement")
          (parent-is "else_statement")
          (parent-is "elseif_statement")
          ;; `c-ts-common-baseline-indent-rule' will handle further
          ;; siblings after the first one has been properly indented.
          ;; The opening bracket occupies index 0.
          (match nil "arguments" nil 1 1)
          (match nil "parameters" nil 1 1)
          (match "field" "table_constructor" nil 1 1))
      standalone-parent lua-ts-indent-offset)

     ((parent-is "block") parent 0)
     ((parent-is "chunk") column-0 0)
     ((parent-is "ERROR") no-indent 0)
     c-ts-common-baseline-indent-rule)))

(defun lua-ts--end-line-matcher (&rest _)
  "Matches if there is more than one `end' on the current line."
  (> (lua-ts--end-count) 1))

(defun lua-ts--end-indent-offset (&rest _)
  "Calculate indent offset based on `end' count."
  (- (* (1- (lua-ts--end-count)) lua-ts-indent-offset)))

(defun lua-ts--end-count ()
  "Count the number of `end's on the current line."
  (count-matches "end" (line-beginning-position) (line-end-position)))

(defun lua-ts--g-parent (node)
  "Return the grand-parent of NODE."
  (let ((parent (treesit-node-parent node)))
    (treesit-node-parent parent)))

(defun lua-ts--multi-arg-function-call-matcher (_n parent &rest _)
  "Matches if PARENT has multiple arguments."
  (> (treesit-node-child-count (treesit-node-parent parent)) 3))

(defun lua-ts--last-arg-function-call-matcher (node parent &rest _)
  "Matches if NODE's PARENT is the last argument in a function call."
  (let* ((g-parent (lua-ts--g-parent node))
         (last (1- (treesit-node-child-count g-parent t))))
    (treesit-node-eq parent (seq-elt (treesit-node-children g-parent t) last))))

(defun lua-ts--variable-declaration-continuation (node &rest _)
  "Matches if NODE is part of a multi-line variable declaration."
  (treesit-parent-until node (lambda (p)
                               (equal "variable_declaration"
                                      (treesit-node-type p)))))

(defun lua-ts--variable-declaration-continuation-anchor (node &rest _)
  "Return the start position of the variable declaration for NODE."
  (save-excursion
    (goto-char (treesit-node-start
                (lua-ts--variable-declaration-continuation node)))
    (when (looking-back (rx bol (* whitespace)) (line-beginning-position))
      (point))))

(defun lua-ts--multi-line-comment-start (node &rest _)
  "Matches if NODE is the beginning of a multi-line comment."
  (and node
       (equal "comment" (treesit-node-type node))
       (save-excursion
         (goto-char (treesit-node-start node))
         (forward-char 2)               ; Skip the -- part.
         (looking-at "\\[\\["))))

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
                           (let (diags)
                             (while (search-forward-regexp
                                     (rx bol (0+ alnum) ":"
                                         (group (1+ digit)) ":"
                                         (group (1+ digit)) "-"
                                         (group (1+ digit)) ": "
                                         (group (0+ nonl)) eol)
                                     nil t)
                               (let* ((beg
                                       (car (flymake-diag-region
                                             source
                                             (string-to-number (match-string 1))
                                             (string-to-number (match-string 2)))))
                                      (end
                                       (cdr (flymake-diag-region
                                             source
                                             (string-to-number (match-string 1))
                                             (string-to-number (match-string 3)))))
                                      (msg (match-string 4))
                                      (type (if (string-prefix-p "(W" msg)
                                                :warning
                                              :error)))
                                 (push (flymake-make-diagnostic
                                        source beg end type msg)
                                       diags)))
                             (funcall report-fn diags)))
                       (flymake-log :warning "Canceling obsolete check %s" proc))
                   (kill-buffer (process-buffer proc)))))))
      (process-send-region lua-ts--flymake-process (point-min) (point-max))
      (process-send-eof lua-ts--flymake-process))))

;;;###autoload
(defun lua-ts-inferior-lua ()
  "Run a Lua interpreter in an inferior process."
  (interactive)
  (if (not lua-ts-inferior-program)
      (user-error "You must set `lua-ts-inferior-program' to use this command")
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
                    comint-prompt-read-only t
                    comint-prompt-regexp (rx bol
                                             (literal lua-ts-inferior-prompt)
                                             (1+ space)))
        (comint-read-input-ring t)
        (add-hook 'comint-preoutput-filter-functions
                  (lambda (string)
                    (if (equal string (concat lua-ts-inferior-prompt-continue " "))
                        string
                      (concat
                       ;; Filter out the extra prompt characters that
                       ;; accumulate in the output when sending regions
                       ;; to the inferior process.
                       (replace-regexp-in-string
                        (rx bol
                            (* (literal lua-ts-inferior-prompt)
                               (? (literal lua-ts-inferior-prompt))
                               (1+ space))
                            (group (* nonl)))
                        "\\1" string)
                       ;; Re-add the prompt for the next line.
                       lua-ts-inferior-prompt " ")))
                  nil t)))
    (select-window (display-buffer lua-ts-inferior-buffer
                                   '((display-buffer-reuse-window
                                      display-buffer-pop-up-window)
                                     (reusable-frames . t))))
    (get-buffer-process (current-buffer))))

(defun lua-ts-send-buffer ()
  "Send current buffer to the inferior Lua process."
  (interactive nil lua-ts-mode)
  (lua-ts-send-region (point-min) (point-max)))

(defun lua-ts-send-file (file)
  "Send contents of FILE to the inferior Lua process."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents-literally file)
    (lua-ts-send-region (point-min) (point-max))))

(defun lua-ts-send-region (beg end)
  "Send region between BEG and END to the inferior Lua process."
  (interactive "r" lua-ts-mode)
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

(defvar-keymap lua-ts-mode-map
  :doc "Keymap for `lua-ts-mode' buffers."
  "C-c C-n" #'lua-ts-inferior-lua
  "C-c C-c" #'lua-ts-send-buffer
  "C-c C-l" #'lua-ts-send-file
  "C-c C-r" #'lua-ts-send-region)

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

  (when (treesit-ensure-installed 'lua)
    (setq treesit-primary-parser (treesit-parser-create 'lua))

    ;; Comments.
    (setq-local comment-start "--")
    (setq-local comment-start-skip "--\\s-*")
    (setq-local comment-end "")

    ;; Pairs.
    (when (and lua-ts-auto-close-block-comments
               (boundp 'electric-pair-pairs))
      (setq-local electric-pair-pairs
                  (cons
                   '("--\\[\\[" . "\n]")
                   electric-pair-pairs)))

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
    (setq-local c-ts-common-indent-offset 'lua-ts-indent-offset)
    (setq-local c-ts-common-list-indent-style 'simple)
    (setq-local treesit-simple-indent-rules lua-ts--simple-indent-rules)

    ;; Navigation.
    (setq-local treesit-defun-name-function #'lua-ts--defun-name-function)
    (setq-local treesit-defun-type-regexp
                (rx (or "function_declaration" "function_definition")))
    (setq-local treesit-thing-settings
                `((lua
                   (function (or "function_declaration"
                                 "function_definition"))
                   (keyword ,(regexp-opt lua-ts--keywords 'symbols))
                   (loop-statement (or "do_statement"
                                       "for_statement"
                                       "repeat_statement"
                                       "while_statement"))
                   (sentence (or function
                                 loop-statement
                                 comment
                                 "assignment_statement"
                                 "field"
                                 "function_call"
                                 "if_statement"
                                 "return_statement"
                                 "variable_declaration"))
                   (sexp (or function
                             keyword
                             loop-statement
                             "arguments"
                             "parameters"
                             "parenthesized_expression"
                             "string"
                             "table_constructor"))
                   (list (or function
                             loop-statement
                             "arguments"
                             "parameters"
                             "table_constructor"
                             "parenthesized_expression"
                             ,(rx bos "if_statement" eos)))
                   (text (or comment "string"))
                   (comment ,(rx bos "comment" eos)))))

    ;; Imenu/Outline/Which-function.
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

    ;; Align.
    (setq-local align-indent-before-aligning t)

    (treesit-major-mode-setup))

  (add-hook 'flymake-diagnostic-functions #'lua-ts-flymake-luacheck nil 'local))

(derived-mode-add-parents 'lua-ts-mode '(lua-mode))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'treesit-major-mode-remap-alist
               '(lua-mode . lua-ts-mode)))

(provide 'lua-ts-mode)

;;; lua-ts-mode.el ends here
