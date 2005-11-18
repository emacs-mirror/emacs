;;; prolog.el --- major mode for editing and running Prolog under Emacs

;; Copyright (C) 1986, 1987, 2001, 2002, 2003, 2004, 2005
;; Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Keywords: languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides a major mode for editing Prolog.  It knows
;; about Prolog syntax and comments, and can send regions to an inferior
;; Prolog interpreter process.  Font locking is tuned towards GNU Prolog.

;;; Code:

(defvar comint-prompt-regexp)


(defgroup prolog nil
  "Major mode for editing and running Prolog under Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)


(defcustom prolog-program-name
  (let ((names '("prolog" "gprolog")))
    (while (and names
		(not (executable-find (car names))))
      (setq names (cdr names)))
    (or (car names) "prolog"))
  "*Program name for invoking an inferior Prolog with `run-prolog'."
  :type 'string
  :group 'prolog)

(defcustom prolog-consult-string "reconsult(user).\n"
  "*(Re)Consult mode (for C-Prolog and Quintus Prolog). "
  :type 'string
  :group 'prolog)

(defcustom prolog-compile-string "compile(user).\n"
  "*Compile mode (for Quintus Prolog)."
  :type 'string
  :group 'prolog)

(defcustom prolog-eof-string "end_of_file.\n"
  "*String that represents end of file for Prolog.
When nil, send actual operating system end of file."
  :type 'string
  :group 'prolog)

(defcustom prolog-indent-width 4
  "Level of indentation in Prolog buffers."
  :type 'integer
  :group 'prolog)

(defvar prolog-font-lock-keywords
  '(("\\(#[<=]=>\\|:-\\)\\|\\(#=\\)\\|\\(#[#<>\\/][=\\/]*\\|!\\)"
     0 font-lock-keyword-face)
    ("\\<\\(is\\|write\\|nl\\|read_\\sw+\\)\\>"
     1 font-lock-keyword-face)
    ("^\\(\\sw+\\)\\s-*\\((\\(.+\\))\\)*"
     (1 font-lock-function-name-face)
     (3 font-lock-variable-name-face)))
  "Font-lock keywords for Prolog mode.")

(defvar prolog-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    table))

(defvar prolog-mode-abbrev-table nil)
(define-abbrev-table 'prolog-mode-abbrev-table ())

(defun prolog-mode-variables ()
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat "%%\\|$\\|" page-delimiter)) ;'%%..'
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression '((nil "^\\sw+" 0)))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'prolog-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(?:%+\\|/\\*+\\)[ \t]*")
  (make-local-variable 'comment-end-skip)
  (setq comment-end-skip "[ \t]*\\(\n\\|\\*+/\\)")
  (make-local-variable 'comment-column)
  (setq comment-column 48))

(defvar prolog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\C-x" 'prolog-consult-region)
    map))

;;;###autoload
(defun prolog-mode ()
  "Major mode for editing Prolog code for Prologs.
Blank lines and `%%...' separate paragraphs.  `%'s start comments.
Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of `prolog-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map prolog-mode-map)
  (set-syntax-table prolog-mode-syntax-table)
  (setq major-mode 'prolog-mode)
  (setq mode-name "Prolog")
  (prolog-mode-variables)
  ;; font lock
  (setq font-lock-defaults '(prolog-font-lock-keywords
                             nil nil nil
                             beginning-of-line))
  (run-mode-hooks 'prolog-mode-hook))

(defun prolog-indent-line (&optional whole-exp)
  "Indent current line as Prolog code.
With argument, indent any additional lines of the same clause
rigidly along with this one (not yet)."
  (interactive "p")
  (let ((indent (prolog-indent-level))
	(pos (- (point-max) (point))) beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (zerop (- indent (current-column)))
	nil
      (delete-region beg (point))
      (indent-to indent))
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    ))

(defun prolog-indent-level ()
  "Compute Prolog indentation level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond
     ((looking-at "%%%") 0)		;Large comment starts
     ((looking-at "%[^%]") comment-column) ;Small comment starts
     ((bobp) 0)				;Beginning of buffer
     (t
      (let ((empty t) ind more less)
	(if (looking-at ")")
	    (setq less t)		;Find close
	  (setq less nil))
	;; See previous indentation
	(while empty
	  (forward-line -1)
	  (beginning-of-line)
 	  (if (bobp)
 	      (setq empty nil)
 	    (skip-chars-forward " \t")
 	    (if (not (or (looking-at "%[^%]") (looking-at "\n")))
 		(setq empty nil))))
 	(if (bobp)
 	    (setq ind 0)		;Beginning of buffer
	  (setq ind (current-column)))	;Beginning of clause
	;; See its beginning
	(if (looking-at "%%[^%]")
	    ind
	  ;; Real prolog code
	  (if (looking-at "(")
	      (setq more t)		;Find open
	    (setq more nil))
	  ;; See its tail
	  (end-of-prolog-clause)
	  (or (bobp) (forward-char -1))
	  (cond ((looking-at "[,(;>]")
		 (if (and more (looking-at "[^,]"))
		     (+ ind prolog-indent-width) ;More indentation
		   (max tab-width ind))) ;Same indentation
		((looking-at "-") tab-width) ;TAB
		((or less (looking-at "[^.]"))
		 (max (- ind prolog-indent-width) 0)) ;Less indentation
		(t 0))			;No indentation
	  )))
     )))

(defun end-of-prolog-clause ()
  "Go to end of clause in this line."
  (beginning-of-line 1)
  (let* ((eolpos (save-excursion (end-of-line) (point))))
    (if (re-search-forward comment-start-skip eolpos 'move)
	(goto-char (match-beginning 0)))
    (skip-chars-backward " \t")))

;;;
;;; Inferior prolog mode
;;;
(defvar inferior-prolog-mode-map
  (let ((map (make-sparse-keymap)))
    ;; This map will inherit from `comint-mode-map' when entering
    ;; inferior-prolog-mode.
    map))

(defvar inferior-prolog-mode-syntax-table prolog-mode-syntax-table)
(defvar inferior-prolog-mode-abbrev-table prolog-mode-abbrev-table)

(define-derived-mode inferior-prolog-mode comint-mode "Inferior Prolog"
  "Major mode for interacting with an inferior Prolog process.

The following commands are available:
\\{inferior-prolog-mode-map}

Entry to this mode calls the value of `prolog-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`prolog-mode-hook' is called after `comint-mode-hook'.

You can send text to the inferior Prolog from other buffers using the commands
`process-send-region', `process-send-string' and \\[prolog-consult-region].

Commands:
Tab indents for Prolog; with argument, shifts rest
 of expression rigidly with the current line.
Paragraphs are separated only by blank lines and '%%'.
'%'s start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops. \\[comint-quit-subjob] sends quit signal."
  (setq comint-prompt-regexp "^| [ ?][- ] *")
  (prolog-mode-variables))

;;;###autoload
(defun run-prolog ()
  "Run an inferior Prolog process, input and output via buffer *prolog*."
  (interactive)
  (require 'comint)
  (pop-to-buffer (make-comint "prolog" prolog-program-name))
  (inferior-prolog-mode))

(defun prolog-consult-region (compile beg end)
  "Send the region to the Prolog process made by \"M-x run-prolog\".
If COMPILE (prefix arg) is not nil, use compile mode rather than consult mode."
  (interactive "P\nr")
  (save-excursion
    (if compile
	(process-send-string "prolog" prolog-compile-string)
      (process-send-string "prolog" prolog-consult-string))
    (process-send-region "prolog" beg end)
    (process-send-string "prolog" "\n")		;May be unnecessary
    (if prolog-eof-string
	(process-send-string "prolog" prolog-eof-string)
      (process-send-eof "prolog")))) ;Send eof to prolog process.

(defun prolog-consult-region-and-go (compile beg end)
  "Send the region to the inferior Prolog, and switch to *prolog* buffer.
If COMPILE (prefix arg) is not nil, use compile mode rather than consult mode."
  (interactive "P\nr")
  (prolog-consult-region compile beg end)
  (switch-to-buffer "*prolog*"))

(provide 'prolog)

;;; arch-tag: f3ec6748-1272-4ab6-8826-c50cb1607636
;;; prolog.el ends here
