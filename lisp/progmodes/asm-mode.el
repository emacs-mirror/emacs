;;; asm-mode.el --- mode for editing assembler code

;; Copyright (C) 1991 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: FSF
;; Keywords: tools, languages

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This mode was written by Eric S. Raymond <esr@snark.thyrsus.com>,
;; inspired by an earlier asm-mode by Martin Neitzel.

;; This minor mode is based on text mode.  It defines a private abbrev table
;; that can be used to save abbrevs for assembler mnemonics.  It binds just
;; five keys:
;;
;;	TAB		tab to next tab stop
;;	:		outdent preceding label, tab to tab stop
;;	comment char	place or move comment
;;			asm-comment-char specifies which character this is;
;;			you can use a different character in different
;;			Asm mode buffers.
;;	C-j, C-m	newline and tab to tab stop
;;
;; Code is indented to the first tab stop level.

;; This mode runs two hooks:
;;   1) An asm-mode-set-comment-hook before the part of the initialization
;; depending on asm-comment-char, and
;;   2) an asm-mode-hook at the end of initialization.

;;; Code:

(defvar asm-comment-char ?;
  "*The comment-start character assumed by Asm mode.")

(defvar asm-mode-syntax-table nil
  "Syntax table used while in Asm mode.")

(defvar asm-mode-abbrev-table nil
  "Abbrev table used while in Asm mode.")
(define-abbrev-table 'asm-mode-abbrev-table ())

(defvar asm-mode-map nil
  "Keymap for Asm mode.")

(if asm-mode-map
    nil
  (setq asm-mode-map (make-sparse-keymap))
  ;; Note that the comment character isn't set up until asm-mode is called.
  (define-key asm-mode-map ":"		'asm-colon)
  (define-key asm-mode-map "\C-i"	'tab-to-tab-stop)
  (define-key asm-mode-map "\C-j"	'asm-newline)
  (define-key asm-mode-map "\C-m"	'asm-newline)
  )

(defconst asm-font-lock-keywords
 '(("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\)?"
    (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t))
   ("^\\s +\\(\\(\\sw\\|\\s_\\)+\\)" 1 font-lock-keyword-face))
 "Additional expressions to highlight in Assembler mode.")

(defvar asm-code-level-empty-comment-pattern nil)
(defvar asm-flush-left-empty-comment-pattern nil)
(defvar asm-inline-empty-comment-pattern nil)

;;;###autoload
(defun asm-mode ()
  "Major mode for editing typical assembler code.
Features a private abbrev table and the following bindings:

\\[asm-colon]\toutdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]\ttab to next tab stop.
\\[asm-newline]\tnewline, then tab to next tab stop.
\\[asm-comment]\tsmart placement of assembler comments.

The character used for making comments is set by the variable
`asm-comment-char' (which defaults to `?;').

Alternatively, you may set this variable in `asm-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on Asm mode runs the hook `asm-mode-hook' at the end of initialization.

Special commands:
\\{asm-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Assembler")
  (setq major-mode 'asm-mode)
  (setq local-abbrev-table asm-mode-abbrev-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(asm-font-lock-keywords))
  (make-local-variable 'asm-mode-syntax-table)
  (setq asm-mode-syntax-table (make-syntax-table))
  (set-syntax-table asm-mode-syntax-table)

  (run-hooks 'asm-mode-set-comment-hook)
  ;; Make our own local child of asm-mode-map
  ;; so we can define our own comment character.
  (use-local-map (nconc (make-sparse-keymap) asm-mode-map))
  (local-set-key (vector asm-comment-char) 'asm-comment)

  (modify-syntax-entry	asm-comment-char
			"<" asm-mode-syntax-table)
  (modify-syntax-entry	?\n
			 ">" asm-mode-syntax-table)
  (let ((cs (regexp-quote (char-to-string asm-comment-char))))
    (make-local-variable 'comment-start)
    (setq comment-start (concat cs " "))
    (make-local-variable 'comment-start-skip)
    (setq comment-start-skip (concat cs "+[ \t]*"))
    (setq asm-inline-empty-comment-pattern (concat "^.+" cs "+ *$"))
    (setq asm-code-level-empty-comment-pattern (concat "^[\t ]+" cs cs " *$"))
    (setq asm-flush-left-empty-comment-pattern (concat "^" cs cs cs " *$"))
    )
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (setq fill-prefix "\t")
  (run-hooks 'asm-mode-hook))

(defun asm-colon ()
  "Insert a colon; if it follows a label, delete the label's indentation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]+\\(\\sw\\|\\s_\\)+$")
	(delete-horizontal-space)))
  (insert ":")
  (tab-to-tab-stop)
  )

(defun asm-newline ()
  "Insert LFD + fill-prefix, to bring us back to code-indent level."
  (interactive)
  (if (eolp) (delete-horizontal-space))
  (insert "\n")
  (tab-to-tab-stop)
  )

(defun asm-line-matches (pattern &optional withcomment)
  (save-excursion
    (beginning-of-line)
    (looking-at pattern)))

(defun asm-pop-comment-level ()
  ;; Delete an empty comment ending current line.  Then set up for a new one,
  ;; on the current line if it was all comment, otherwise above it
  (end-of-line)
  (delete-horizontal-space)
  (while (= (preceding-char) asm-comment-char)
    (delete-backward-char 1))
  (delete-horizontal-space)
  (if (bolp)
      nil
    (beginning-of-line)
    (open-line 1))
  )


(defun asm-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

   1 -- comment to the right of the code (at the comment-column)
   2 -- comment on its own line, indented like code
   3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger asm-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (cond

   ;; Blank line?  Then start comment at code indent level.
   ((asm-line-matches "^[ \t]*$")
    (delete-horizontal-space)
    (tab-to-tab-stop)
    (insert asm-comment-char comment-start))

   ;; Nonblank line with no comment chars in it?
   ;; Then start a comment at the current comment column
   ((asm-line-matches (format "^[^%c\n]+$" asm-comment-char))
    (indent-for-comment))

   ;; Flush-left comment present?  Just insert character.
   ((asm-line-matches asm-flush-left-empty-comment-pattern)
    (insert asm-comment-char))

   ;; Empty code-level comment already present?
   ;; Then start flush-left comment, on line above if this one is nonempty. 
   ((asm-line-matches asm-code-level-empty-comment-pattern)
    (asm-pop-comment-level)
    (insert asm-comment-char asm-comment-char comment-start))

   ;; Empty comment ends line?
   ;; Then make code-level comment, on line above if this one is nonempty. 
   ((asm-line-matches asm-inline-empty-comment-pattern)
    (asm-pop-comment-level)
    (tab-to-tab-stop)
    (insert asm-comment-char comment-start))

   ;; If all else fails, insert character
   (t
    (insert asm-comment-char))

   )
  (end-of-line))

;;; asm-mode.el ends here
