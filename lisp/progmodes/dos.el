;;; dos.el --- Major mode for editing Dos scripts

;; Copyright (C) 2003, 2008-2013 Free Software Foundation, Inc.

;; Author: Arni Magnusson <arnima@hafro.is>
;; Keywords: languages

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Major mode for editing Dos scripts (batch files). Provides syntax
;; highlighting, a basic template, access to Dos help pages, imenu/outline
;; navigation, and the ability to run scripts from within Emacs. The syntax
;; groups for highlighting are:
;;
;; Face                          Example
;; dos-label-face                :LABEL
;; font-lock-comment-face        rem
;; font-lock-builtin-face        copy
;; font-lock-keyword-face        goto
;; font-lock-warning-face        cp
;; font-lock-constant-face       [call] prog
;; font-lock-variable-name-face  %var%
;; font-lock-type-face           -option
;;
;; Usage:
;;
;; See documentation of function `dos-mode'.
;;
;; Separate package `dos-indent' (Matthew Fidler) provides rudimentary
;; indentation, see http://www.emacswiki.org/emacs/dos-indent.el.
;;
;; Acknowledgements:
;;
;; Inspired by `batch-mode' (Agnar Renolen) and `cmd-mode' (Tadamegu Furukawa).

;;; Code:

;; 1  Preamble

(defgroup dos nil
  "Major mode for editing Dos scripts."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

;; 2  User variables

(defface dos-label-face '((t :weight bold))
  "Font Lock mode face used to highlight Dos labels."
  :group 'dos)

;; 3  Internal variables

(defvar dos-font-lock-keywords
  (eval-when-compile
    (let ((COMMANDS
           '("assoc" "at" "attrib" "cd" "cls" "color" "copy" "date" "del" "dir"
             "doskey" "echo" "endlocal" "erase" "fc" "find" "findstr" "format"
             "ftype" "label" "md" "mkdir" "more" "move" "net" "path" "pause"
             "popd" "prompt" "pushd" "rd" "ren" "rename" "replace" "rmdir" "set"
             "setlocal" "shift" "sort" "subst" "time" "title" "tree" "type"
             "ver" "vol" "xcopy"))
          (CONTROLFLOW
           '("call" "cmd" "defined" "do" "else" "equ" "exist" "exit" "for" "geq"
             "goto" "gtr" "if" "in" "leq" "lss" "neq" "not" "start"))
          (LINUX
           '("cat" "cp" "ls" "mv" "rm")))
      (list
       '("\\<\\(call\\|goto\\)\\>[ \t]+%?\\([A-Za-z0-9-_\\:.]+\\)%?"
         (2 font-lock-constant-face t))
       '("^[ \t]*\\(@?rem\\>\\|::\\).*"
         (0 font-lock-comment-face t))
       '("^:[^:].*"
         . 'dos-label-face)
       '("\\<\\(defined\\|set\\)\\>[ \t]*\\(\\w+\\)"
         (2 font-lock-variable-name-face))
       '("%\\(\\w+\\)%?"
         (1 font-lock-variable-name-face))
       '("!\\(\\w+\\)!?" ; delayed-expansion !variable!
         (1 font-lock-variable-name-face))
       '("[ =][-/]+\\(\\w+\\)"
         (1 font-lock-type-face append))
       (cons (regexp-opt COMMANDS 'words) font-lock-builtin-face)
       (cons (regexp-opt CONTROLFLOW 'words) font-lock-keyword-face)
       (cons (regexp-opt LINUX 'words) font-lock-warning-face)))))

(defvar dos-menu
  '("Dos"
    ["Run" dos-run :help "Run script"]
    ["Run with Args" dos-run-args :help "Run script with args"]
    "--"
    ["Imenu" imenu :help "Navigate with imenu"]
    "--"
    ["Template" dos-template :help "Insert template"]
    "--"
    ["Help (Command)" dos-cmd-help :help "Show help page for Dos command"]
    ["Help (Mode)" dos-mode-help :help "Show help page for Emacs Dos Mode"]))

(defvar dos-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil dos-menu)
    (define-key map [?\C-c ?\C-.] 'dos-mode-help)
    (define-key map [?\C-c ?\C-/] 'dos-cmd-help)
    (define-key map [?\C-c ?\C-a] 'dos-run-args)
    (define-key map [?\C-c ?\C-c] 'dos-run)
    (define-key map [?\C-c ?\C-t] 'dos-template)
    (define-key map [?\C-c ?\C-v] 'dos-run)
    map))

(defvar dos-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?~ "w" table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?{ "w" table)
    (modify-syntax-entry ?} "w" table)
    (modify-syntax-entry ?\\ "." table)
    table))

;; 4  User functions

(defun dos-cmd-help (cmd)
  "Show help for Dos command."
  (interactive "sHelp: ")
  (if (string-equal cmd "net")
      (shell-command "net /?") (shell-command (concat "help " cmd))))

(defun dos-mode-help ()
  "Show help page for `dos-mode'."
  (interactive)
  (describe-function 'dos-mode)
  (switch-to-buffer "*Help*") (delete-other-windows) (message nil))

(defun dos-run ()
  "Run Dos script."
  (interactive)
  (save-buffer) (shell-command buffer-file-name))

(defun dos-run-args (args)
  "Run Dos script with ARGS."
  (interactive "sArgs: ")
  (shell-command (concat buffer-file-name " " args)))

(defun dos-template ()
  "Insert minimal Dos template."
  (interactive)
  (goto-char (point-min)) (insert "@echo off\nsetlocal\n\n"))

;; 5  Main function

;;;###autoload
(define-derived-mode dos-mode prog-mode "Dos"
  "Major mode for editing Dos scripts.\n
The `dos-mode-help' command shows this page.\n
Start a new script from `dos-template'. Read help pages for Dos commands with
`dos-cmd-help'. Navigate between sections using `imenu'. Run script using
`dos-run' and `dos-run-args'.\n
\\{dos-mode-map}"
  (set (make-local-variable 'comment-start) "rem")
  (set (make-local-variable 'font-lock-defaults)
       '(dos-font-lock-keywords nil t)) ; case-insensitive keywords
  (set (make-local-variable 'imenu-generic-expression) '((nil "^:[^:].*" 0)))
  (set (make-local-variable 'outline-regexp) ":[^:]")
  (set-syntax-table dos-mode-syntax-table))

(provide 'dos)

;;; dos.el ends here
