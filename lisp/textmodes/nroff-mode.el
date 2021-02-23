;;; nroff-mode.el --- GNU Emacs major mode for editing nroff source  -*- lexical-binding: t -*-

;; Copyright (C) 1985-1986, 1994-1995, 1997, 2001-2021 Free Software
;; Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: wp

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

;; This package is a major mode for editing nroff source code.  It knows
;; about various nroff constructs, ms, mm, and me macros, and will fill
;; and indent paragraphs properly in their presence.  It also includes
;; a command to count text lines (excluding nroff constructs), a command
;; to center a line, and movement commands that know how to skip macros.

;; Paragraph filling and line-counting currently don't respect comments,
;; as they should.

;;; Code:

(defgroup nroff nil
  "Nroff mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'text
  :prefix "nroff-")


(defcustom nroff-electric-mode nil
  "Non-nil means automatically closing requests when you insert an open."
  :type 'boolean)

(defvar nroff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t"  'tab-to-tab-stop)
    (define-key map "\e?" 'nroff-count-text-lines)
    (define-key map "\n"  'nroff-electric-newline)
    (define-key map "\en" 'nroff-forward-text-line)
    (define-key map "\ep" 'nroff-backward-text-line)
    (define-key map "\C-c\C-c" 'nroff-view)
    map)
  "Major mode keymap for `nroff-mode'.")

(easy-menu-define nroff-mode-menu nroff-mode-map
  "Menu for `nroff-mode'."
  '("Nroff"
    ["Preview as man page" nroff-view
     :help "Run man on this file."]
    ["Electric newline mode" nroff-electric-mode
     :help "Auto insert closing requests if necessary"
     :style toggle
     :selected nroff-electric-mode]
    ["Backward text line" nroff-backward-text-line
     :help "Go backward one nroff text line, skipping lines of nroff requests"]
    ["Forward text line" nroff-forward-text-line
     :help "Go forward one nroff text line, skipping lines of nroff requests"]
    ["Count text lines" nroff-count-text-lines
     :help "Count lines in region, except for nroff request lines."]
    ["Newline" nroff-electric-newline
     :help "Insert newline for nroff mode; special if nroff-electric mode"]))

(defvar nroff-mode-syntax-table
  (let ((st (copy-syntax-table text-mode-syntax-table)))
    ;; " isn't given string quote syntax in text-mode but it
    ;; (arguably) should be for use round nroff arguments (with ` and
    ;; ' used otherwise).
    (modify-syntax-entry ?\" "\"  2" st)
    ;; Comments are delimited by \" and newline.
    ;; And in groff also \# to newline.
    (modify-syntax-entry ?# ".  2"  st)
    (modify-syntax-entry ?\\ "\\  1" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table used while in `nroff-mode'.")

(defvar nroff-imenu-expression
  ;; man headers:
  '((nil "^\\.SH \"?\\([^\"\n]*\\)\"?$" 1)))

(defcustom nroff-font-lock-keywords
  (list
   ;; Directives are . or ' at start of line, followed by
   ;; optional whitespace, then command (which my be longer than
   ;; 2 characters in groff).  Perhaps the arguments should be
   ;; fontified as well.
   "^[.']\\s-*\\sw+"
   ;; There are numerous groff escapes; the following get things
   ;; like \-, \(em (standard troff) and \f[bar] (groff
   ;; variants).  This won't currently do groff's \A'foo' and
   ;; the like properly.  One might expect it to highlight an escape's
   ;; arguments in common cases, like \f.
   (concat "\\\\"		      ; backslash
	 "\\("			      ; followed by various possibilities
         (mapconcat #'identity
		    '("[f*n]*\\[.+?]" ; some groff extensions
		      "(.."	      ; two chars after (
		      "[^(\"#]"	      ; single char escape
		      ) "\\|")
	 "\\)")
   )
  "Font-lock highlighting control in `nroff-mode'."
  :type '(repeat regexp))

(defcustom nroff-mode-hook nil
  "Hook run by function `nroff-mode'."
  :type 'hook)

;;;###autoload
(define-derived-mode nroff-mode text-mode "Nroff"
  "Major mode for editing text intended for nroff to format.
\\{nroff-mode-map}
Turning on Nroff mode runs `text-mode-hook', then `nroff-mode-hook'.
Also, try `nroff-electric-mode', for automatically inserting
closing requests for requests that are used in matched pairs."
  (setq-local font-lock-defaults
              ;; SYNTAX-BEGIN is set to backward-paragraph to avoid slow-down
              ;; near the end of large buffers due to searching to buffer's
              ;; beginning.
              '(nroff-font-lock-keywords nil t nil backward-paragraph))
  (setq-local outline-regexp "\\.H[ ]+[1-7]+ ")
  (setq-local outline-level 'nroff-outline-level)
  ;; now define a bunch of variables for use by commands in this mode
  (setq-local page-delimiter "^\\.\\(bp\\|SK\\|OP\\)")
  (setq-local paragraph-start (concat "[.']\\|" paragraph-start))
  (setq-local paragraph-separate (concat "[.']\\|" paragraph-separate))
  ;; Don't auto-fill directive lines starting . or ' since they normally
  ;; have to be one line.  But do auto-fill comments .\" .\# and '''.
  ;; Comment directives (those starting . or ') are [.'][ \t]*\\[#"]
  ;; or ''', and this regexp is everything except those.  So [.']
  ;; followed by not backslash and not ' or followed by backslash but
  ;; then not # or "
  (setq-local auto-fill-inhibit-regexp
              "[.'][ \t]*\\([^ \t\\']\\|\\\\[^#\"]\\)")
  ;; comment syntax added by mit-erl!gildea 18 Apr 86
  (setq-local comment-start "\\\" ")
  (setq-local comment-start-skip "\\\\[\"#][ \t]*")
  (setq-local comment-column 24)
  (setq-local comment-indent-function #'nroff-comment-indent)
  (setq-local comment-insert-comment-function #'nroff-insert-comment-function)
  (setq-local imenu-generic-expression nroff-imenu-expression))

(defun nroff-outline-level ()
  (save-excursion
    (looking-at outline-regexp)
    (skip-chars-forward ".H ")
    (string-to-number (buffer-substring (point) (+ 1 (point))))))

;; Compute how much to indent a comment in nroff/troff source.
;; By mit-erl!gildea April 86
(defun nroff-comment-indent ()
  "Compute indent for an nroff/troff comment.
Puts a full-stop before comments on a line by themselves."
  (let ((pt (point)))
    (unwind-protect
	(progn
	  (skip-chars-backward " \t")
	  (if (bolp)
	      (progn
		;; FIXME delete-horizontal-space?
		(setq pt (1+ pt))
		(insert ?.)
		1)
	    (if (save-excursion
		  (backward-char 1)
		  (looking-at "^[.']"))
		1
	      (max comment-column
		   (* 8 (/ (+ (current-column)
			      9) 8)))))) ; add 9 to ensure at least two blanks
      (goto-char pt))))

;; https://lists.gnu.org/r/emacs-devel/2007-10/msg01869.html
(defun nroff-insert-comment-function ()
  "Function for `comment-insert-comment-function' in `nroff-mode'."
  (indent-to (nroff-comment-indent))
  (insert comment-start))

(defun nroff-count-text-lines (start end &optional print)
  "Count lines in region, except for nroff request lines.
All lines not starting with a period are counted up.
Interactively, print result in echo area.
Noninteractively, return number of non-request lines from START to END."
  (interactive "r\np")
  (if print
      (message "Region has %d text lines" (nroff-count-text-lines start end))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(- (buffer-size) (nroff-forward-text-line (buffer-size)))))))

(defun nroff-forward-text-line (&optional cnt)
  "Go forward one nroff text line, skipping lines of nroff requests.
An argument is a repeat count; if negative, move backward."
  (interactive "p")
  (if (not cnt) (setq cnt 1))
  (while (and (> cnt 0) (not (eobp)))
    (forward-line 1)
    (while (and (not (eobp)) (looking-at "[.']."))
      (forward-line 1))
    (setq cnt (- cnt 1)))
  (while (and (< cnt 0) (not (bobp)))
    (forward-line -1)
    (while (and (not (bobp))
		(looking-at "[.']."))
      (forward-line -1))
    (setq cnt (+ cnt 1)))
  cnt)

(defun nroff-backward-text-line (&optional cnt)
  "Go backward one nroff text line, skipping lines of nroff requests.
An argument is a repeat count; negative means move forward."
  (interactive "p")
  (nroff-forward-text-line (- cnt)))

(defconst nroff-brace-table
  '((".(b" . ".)b")
    (".(l" . ".)l")
    (".(q" . ".)q")
    (".(c" . ".)c")
    (".(x" . ".)x")
    (".(z" . ".)z")
    (".(d" . ".)d")
    (".(f" . ".)f")
    (".LG" . ".NL")
    (".SM" . ".NL")
    (".LD" . ".DE")
    (".CD" . ".DE")
    (".BD" . ".DE")
    (".DS" . ".DE")
    (".DF" . ".DE")
    (".FS" . ".FE")
    (".KS" . ".KE")
    (".KF" . ".KE")
    (".LB" . ".LE")
    (".AL" . ".LE")
    (".BL" . ".LE")
    (".DL" . ".LE")
    (".ML" . ".LE")
    (".RL" . ".LE")
    (".VL" . ".LE")
    (".RS" . ".RE")
    (".TS" . ".TE")
    (".EQ" . ".EN")
    (".PS" . ".PE")
    (".BS" . ".BE")
    (".G1" . ".G2")			; grap
    (".na" . ".ad b")
    (".nf" . ".fi")
    (".de" . "..")))

(defun nroff-electric-newline (arg)
  "Insert newline for nroff mode; special if nroff-electric mode.
In `nroff-electric-mode', if ending a line containing an nroff opening request,
automatically inserts the matching closing request after point."
  (interactive "P")
  (let ((completion (save-excursion
		      (beginning-of-line)
		      (and (null arg)
			   nroff-electric-mode
			   (<= (point) (- (point-max) 3))
			   (cdr (assoc (buffer-substring (point)
							 (+ 3 (point)))
				       nroff-brace-table)))))
	(needs-nl (not (looking-at "[ \t]*$"))))
    (if (null completion)
	(newline (prefix-numeric-value arg))
      (save-excursion
	(insert "\n\n" completion)
	(if needs-nl (insert "\n")))
      (forward-char 1))))

(define-minor-mode nroff-electric-mode
  "Toggle automatic nroff request pairing (Nroff Electric mode).

Nroff Electric mode is a buffer-local minor mode, for use with
`nroff-mode'.  When enabled, Emacs checks for an nroff request at
the beginning of the line, and inserts the matching closing
request if necessary.  This command toggles that mode (off->on,
on->off), with an argument, turns it on if arg is positive,
otherwise off."
  :lighter " Electric"
  (or (derived-mode-p 'nroff-mode) (error "Must be in nroff mode")))

(declare-function Man-getpage-in-background "man" (topic))

(defun nroff-view ()
  "Run man on this file."
  (interactive)
  (require 'man)
  (let* ((file (buffer-file-name))
	 (viewbuf (get-buffer (concat "*Man " file "*"))))
    (unless file
      (error "Buffer is not associated with any file"))
    (and (buffer-modified-p)
	 (y-or-n-p (format "Save buffer %s first? " (buffer-name)))
	 (save-buffer))
    (if viewbuf
	(kill-buffer viewbuf))
    (Man-getpage-in-background (shell-quote-argument file))))

(provide 'nroff-mode)

;;; nroff-mode.el ends here
