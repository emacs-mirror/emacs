;;; m4-mode.el --- m4 code editing commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1996-1997, 2001-2022 Free Software Foundation, Inc.

;; Author: Andrew Csillag <drew@thecsillags.com>
;; Keywords: languages, faces

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

;; A smart editing mode for m4 macro definitions.  It seems to have most of the
;; syntax right (sexp motion commands work, but function motion commands don't).
;; It also sets the font-lock syntax stuff for colorization

;; To Do's:

;; * want to make m4-m4-(buffer|region) look sorta like M-x compile look&feel ?
;; * sexp motion commands don't seem to work right

;;; Thanks:
;;;         to Akim Demaille and Terry Jones for the bug reports
;;;         to Simon Marshall for the regexp tip
;;;         to Martin Buchholz for some general fixes

;;; Code:

(defgroup m4 nil
  "m4 code editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "m4-"
  :group 'languages)

(defcustom m4-program "m4"
  "File name of the m4 executable.
If m4 is not in your PATH, set this to an absolute file name."
  :version "24.4"
  :type 'file)

;;options to m4
(defcustom m4-program-options nil
  "Options to pass to `m4-program'."
  :type '(repeat string))

;;to use --prefix-builtins, you can use
;;(defconst m4-program-options '("-P"))
;;or
;;(defconst m4-program-options '("--prefix-builtins"))

;; Needed at compile-time for `m4-font-lock-keywords' below.
(eval-and-compile
  (defconst m4--macro-list
    ;; From (info "(m4) Macro index")
    '("__file__" "__gnu__" "__line__" "__os2__" "__program__" "__unix__"
      "__windows__" "argn" "array" "array_set" "builtin" "capitalize"
      "changecom" "changequote" "changeword" "cleardivert" "cond" "copy"
      "curry" "debugfile" "debugmode" "decr" "define" "define_blind"
      "defn" "divert" "divnum" "dnl" "downcase" "dquote" "dquote_elt"
      "dumpdef" "errprint" "esyscmd" "eval" "example" "exch"
      "fatal_error" "file" "foreach" "foreachq" "forloop" "format" "gnu"
      "ifdef" "ifelse" "include" "incr" "index" "indir" "join" "joinall"
      "len" "line" "m4exit" "m4wrap" "maketemp" "mkstemp" "nargs" "os2"
      "patsubst" "popdef" "pushdef" "quote" "regexp" "rename" "reverse"
      "shift" "sinclude" "stack_foreach" "stack_foreach_lifo"
      "stack_foreach_sep" "stack_foreach_sep_lifo" "substr" "syscmd"
      "sysval" "traceoff" "traceon" "translit" "undefine" "undivert"
      "unix" "upcase" "windows")
    "List of valid m4 macros for M4 mode."))

(defvar m4-font-lock-keywords
  (eval-when-compile
    `(("\\(\\_<\\(m4_\\)?dnl\\_>\\).*$" (0 font-lock-comment-face t))
      ("\\$[*#@0-9]" . font-lock-variable-name-face)
      ("\\$@" . font-lock-variable-name-face)
      ("\\$\\*" . font-lock-variable-name-face)
      (,(concat "\\_<\\(m4_\\)?" (regexp-opt m4--macro-list) "\\_>")
       . font-lock-keyword-face)))
  "Default `font-lock-keywords' for M4 mode.")

(defcustom m4-mode-hook nil
  "Hook called by `m4-mode'."
  :type 'hook)

;;this may still need some work
(defvar m4-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?` "('" table)
    (modify-syntax-entry ?' ")`" table)
    (modify-syntax-entry ?# "<\n" table)
    (modify-syntax-entry ?\n ">#" table)
    (modify-syntax-entry ?{  "." table)
    (modify-syntax-entry ?}  "." table)
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?\"  "." table)
    table)
  "Syntax table used while in `m4-mode'.")

(defun m4--quoted-p (pos)
  "Return non-nil if POS is inside a quoted string."
  (let ((quoted nil))
    (dolist (o (nth 9 (save-excursion (syntax-ppss pos))))
      (if (eq (char-after o) ?\`) (setq quoted t)))
    quoted))

(defconst m4-syntax-propertize
  (syntax-propertize-rules
   ("#" (0 (when (m4--quoted-p (match-beginning 0))
             (string-to-syntax "."))))))

(defvar m4-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'm4-m4-buffer)
    (define-key map "\C-c\C-r" 'm4-m4-region)
    (define-key map "\C-c\C-c" 'comment-region)
    map)
  "Keymap for M4 Mode.")

(easy-menu-define m4-mode-menu m4-mode-map
  "Menu for M4 Mode."
  '("M4"
    ["M4 Region" m4-m4-region
     :help "Send contents of the current region to m4"]
    ["M4 Buffer" m4-m4-buffer
     :help "Send contents of the current buffer to m4"]
    ["Comment Region" comment-region
     :help "Comment Region"]))

(defun m4-m4-buffer ()
  "Send contents of the current buffer to m4."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (mapconcat #'identity (cons m4-program m4-program-options) "\s")
   "*m4-output*" nil)
  (switch-to-buffer-other-window "*m4-output*"))

(defun m4-m4-region ()
  "Send contents of the current region to m4."
  (interactive)
  (shell-command-on-region
   (point) (mark)
   (mapconcat #'identity (cons m4-program m4-program-options) "\s")
   "*m4-output*" nil)
  (switch-to-buffer-other-window "*m4-output*"))

(defun m4-current-defun-name ()
  "Return the name of the M4 function at point, or nil."
  (save-excursion
    (if (re-search-backward
	 "^\\(\\(m4_\\)?define\\|A._DEFUN\\)(\\[?\\([A-Za-z0-9_]+\\)" nil t)
	(match-string-no-properties 3))))

;;;###autoload
(define-derived-mode m4-mode prog-mode "m4"
  "A major mode to edit m4 macro files."
  (setq-local comment-start "#")
  (setq-local parse-sexp-ignore-comments t)
  (setq-local add-log-current-defun-function #'m4-current-defun-name)
  (setq-local syntax-propertize-function m4-syntax-propertize)
  (setq-local font-lock-defaults '(m4-font-lock-keywords nil)))

(provide 'm4-mode)
;;stuff to play with for debugging
;(char-to-string (char-syntax ?`))

;;; m4-mode.el ends here
