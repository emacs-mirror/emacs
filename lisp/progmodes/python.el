;;; python.el --- silly walks for Python

;; Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Maintainer: FSF
;; Created: Nov 2003
;; Keywords: languages

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Major mode for editing Python, with support for inferior processes.

;; There is another Python mode, python-mode.el, used by XEmacs and
;; maintained with Python.  That isn't covered by an FSF copyright
;; assignment, unlike this code, and seems not to be well-maintained
;; for Emacs (though I've submitted fixes).  This mode is rather
;; simpler and is, perhaps, better in other ways.  In particular,
;; using the syntax functions with text properties maintained by
;; font-lock should make it more correct with arbitrary string and
;; comment contents.

;; This doesn't implement all the facilities of python-mode.el.  Some
;; just need doing, e.g. catching exceptions in the inferior Python
;; buffer (but see M-x pdb for debugging).  [Actually, the use of
;; `compilation-minor-mode' now is probably enough for that.]  Others
;; don't seem appropriate.  For instance, `forward-into-nomenclature'
;; should be done separately, since it's not specific to Python, and
;; I've installed a minor mode to do the job properly in Emacs 22.
;; Other things seem more natural or canonical here, e.g. the
;; {beginning,end}-of-defun implementation dealing with nested
;; definitions, and the inferior mode following `cmuscheme'.  The
;; inferior mode can find the source of errors from
;; `python-send-region' & al via `compilation-minor-mode'.  Successive
;; TABs cycle between possible indentations for the line.  There is
;; symbol completion using lookup in Python.

;; Even where it has similar facilities, this is incompatible with
;; python-mode.el in various respects.  For instance, various key
;; bindings are changed to obey Emacs conventions, and things like
;; marking blocks and `beginning-of-defun' behave differently.

;; TODO: See various Fixmes below.

;;; Code:

;; It's messy to autoload the relevant comint functions so that comint
;; is only required when inferior Python is used.
(require 'comint)
(eval-when-compile
  (require 'compile)
  (autoload 'info-lookup-maybe-add-help "info-look"))

(defgroup python nil
  "Silly walks in the Python language."
  :group 'languages
  :version "22.1"
  :link '(emacs-commentary-link "python"))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("jython" . jython-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;;; Font lock

(defvar python-font-lock-keywords
  `(,(rx (and word-start
	      ;; From v 2.3 reference.
	      ;; def and class dealt with separately below
	      (or "and" "assert" "break" "continue" "del" "elif" "else"
		  "except" "exec" "finally" "for" "from" "global" "if"
		  "import" "in" "is" "lambda" "not" "or" "pass" "print"
		  "raise" "return" "try" "while" "yield"
		  ;; Future keywords
		  "as" "None")
	      word-end))
   (,(rx (and word-start (group "class") (1+ space) (group (1+ word))))
    (1 font-lock-keyword-face) (2 font-lock-type-face))
   (,(rx (and word-start (group "def") (1+ space) (group (1+ word))))
    (1 font-lock-keyword-face) (2 font-lock-function-name-face))))

(defconst python-font-lock-syntactic-keywords
  ;; Make outer chars of matching triple-quote sequences into generic
  ;; string delimiters.  Fixme: Is there a better way?
  `((,(rx (and (or line-start buffer-start (not (syntax escape))) ; avoid escaped
						       ; leading quote
	       (group (optional (any "uUrR"))) ; prefix gets syntax property
	       (optional (any "rR"))	; possible second prefix
	       (group (syntax string-quote))	; maybe gets property
	       (backref 2)			; per first quote
	       (group (backref 2))))		; maybe gets property
     (1 (python-quote-syntax 1))
     (2 (python-quote-syntax 2))
     (3 (python-quote-syntax 3)))
    ;; This doesn't really help.
;;;     (,(rx (and ?\\ (group ?\n))) (1 " "))
    ))

(defun python-quote-syntax (n)
  "Put `syntax-table' property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as `syntax-ppss' to be correct and it seems to be OK
  ;; to use it here despite initial worries.)  We also have to sort
  ;; out a possible prefix -- well, we don't _have_ to, but I think it
  ;; should be treated as part of the string.

  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (cond
     ;; Consider property for the last char if in a fenced string.
     ((= n 3)
      (let ((syntax (syntax-ppss)))
	(when (eq t (nth 3 syntax))	; after unclosed fence
	  (goto-char (nth 8 syntax))	; fence position
	  ;; Skip any prefix.
	  (if (memq (char-after) '(?u ?U ?R ?r))
	      (skip-chars-forward "uUrR"))
	  ;; Is it a matching sequence?
	  (if (eq (char-after) (char-after (match-beginning 2)))
	      (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2)			; not prefix
	       (= (match-beginning 1) (match-end 1))) ; prefix is null
	  (and (= n 1)			; prefix
	       (/= (match-beginning 1) (match-end 1)))) ; non-empty
      (unless (eq 'string (syntax-ppss-context (syntax-ppss)))
	(eval-when-compile (string-to-syntax "|"))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))

;; This isn't currently in `font-lock-defaults' as probably not worth
;; it -- we basically only mess with a few normally-symbol characters.

;; (defun python-font-lock-syntactic-face-function (state)
;;   "`font-lock-syntactic-face-function' for Python mode.
;; Returns the string or comment face as usual, with side effect of putting
;; a `syntax-table' property on the inside of the string or comment which is
;; the standard syntax table."
;;   (if (nth 3 state)
;;       (save-excursion
;; 	(goto-char (nth 8 state))
;; 	(condition-case nil
;; 	    (forward-sexp)
;; 	  (error nil))
;; 	(put-text-property (1+ (nth 8 state)) (1- (point))
;; 			   'syntax-table (standard-syntax-table))
;; 	'font-lock-string-face)
;;     (put-text-property (1+ (nth 8 state)) (line-end-position)
;; 			   'syntax-table (standard-syntax-table))
;;     'font-lock-comment-face))

;;;; Keymap and syntax

(defvar python-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Mostly taken from python-mode.el.
    (define-key map ":" 'python-electric-colon)
    (define-key map "\177" 'python-backspace)
    (define-key map "\C-c<" 'python-shift-left)
    (define-key map "\C-c>" 'python-shift-right)
    (define-key map "\C-c\C-k" 'python-mark-block)
    (define-key map "\C-c\C-n" 'python-next-statement)
    (define-key map "\C-c\C-p" 'python-previous-statement)
    (define-key map "\C-c\C-u" 'python-beginning-of-block)
    (define-key map "\C-c\C-f" 'python-describe-symbol)
    (define-key map "\C-c\C-w" 'python-check)
    (define-key map "\C-c\C-v" 'python-check) ; a la sgml-mode
    (define-key map "\C-c\C-s" 'python-send-string)
    (define-key map [?\C-\M-x] 'python-send-defun)
    (define-key map "\C-c\C-r" 'python-send-region)
    (define-key map "\C-c\M-r" 'python-send-region-and-go)
    (define-key map "\C-c\C-c" 'python-send-buffer)
    (define-key map "\C-c\C-z" 'python-switch-to-python)
    (define-key map "\C-c\C-m" 'python-load-file)
    (define-key map "\C-c\C-l" 'python-load-file) ; a la cmuscheme
    (substitute-key-definition 'complete-symbol 'python-complete-symbol
			       map global-map)
    ;; Fixme: Add :help to menu.
    (easy-menu-define python-menu map "Python Mode menu"
      '("Python"
	["Shift region left" python-shift-left :active mark-active]
	["Shift region right" python-shift-right :active mark-active]
	"-"
	["Mark block" python-mark-block]
	["Mark def/class" mark-defun
	 :help "Mark innermost definition around point"]
	"-"
	["Start of block" python-beginning-of-block]
	["End of block" python-end-of-block]
	["Start of def/class" beginning-of-defun
	 :help "Go to start of innermost definition around point"]
	["End of def/class" end-of-defun
	 :help "Go to end of innermost definition around point"]
	"-"
	["Start interpreter" run-python
	 :help "Run `inferior' Python in separate buffer"]
	["Import/reload file" python-load-file
	 :help "Load into inferior Python session"]
	["Eval buffer" python-send-buffer
	 :help "Evaluate buffer en bloc in inferior Python session"]
	["Eval region" python-send-region :active mark-active
	 :help "Evaluate region en bloc in inferior Python session"]
	["Eval def/class" python-send-defun
	 :help "Evaluate current definition in inferior Python session"]
	["Switch to interpreter" python-switch-to-python
	 :help "Switch to inferior Python buffer"]
	["Check file" python-check :help "Run pychecker"]
	["Debugger" pdb :help "Run pdb under GUD"]
	"-"
	["Help on symbol" python-describe-symbol
	 :help "Use pydoc on symbol at point"]))
    map))

(defvar python-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
	  (sst (standard-syntax-table)))
      (dotimes (i 128)
	(unless (= i ?_)
	  (if (equal symbol (aref sst i))
	      (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table))

;;;; Utility stuff

(defsubst python-in-string/comment ()
  "Return non-nil if point is in a Python literal (a comment or string)."
  (syntax-ppss-context (syntax-ppss)))

(defconst python-space-backslash-table
  (let ((table (copy-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?\\ " " table)
    table)
  "`python-mode-syntax-table' with backslash given whitespace syntax.")

(defun python-skip-comments/blanks (&optional backward)
  "Skip comments and blank lines.
BACKWARD non-nil means go backwards, otherwise go forwards.  Backslash is
treated as whitespace so that continued blank lines are skipped.
Doesn't move out of comments -- should be outside or at end of line."
  (with-syntax-table python-space-backslash-table
    (forward-comment (if backward
			 most-negative-fixnum
		       most-positive-fixnum))))

(defun python-backslash-continuation-line-p ()
  "Non-nil if preceding line ends with backslash that is not in a comment."
  (and (eq ?\\ (char-before (line-end-position 0)))
       (not (syntax-ppss-context (syntax-ppss)))))

(defun python-continuation-line-p ()
  "Return non-nil if current line continues a previous one.
The criteria are that the previous line ends in a backslash outside
comments and strings, or that the bracket/paren nesting depth is nonzero."
  (or (and (eq ?\\ (char-before (line-end-position 0)))
	   (not (syntax-ppss-context (syntax-ppss))))
      (/= 0 (syntax-ppss-depth
	     (save-excursion	  ; syntax-ppss with arg changes point
	       (syntax-ppss (line-beginning-position)))))))

(defun python-comment-line-p ()
  "Return non-nil iff current line has only a comment."
  (save-excursion
    (end-of-line)
    (when (eq 'comment (syntax-ppss-context (syntax-ppss)))
      (back-to-indentation)
      (looking-at (rx (or (syntax comment-start) line-end))))))

(defun python-beginning-of-string ()
  "Go to beginning of string around point.
Do nothing if not in string."
  (let ((state (syntax-ppss)))
    (when (eq 'string (syntax-ppss-context state))
      (goto-char (nth 8 state)))))

(defun python-open-block-statement-p (&optional bos)
  "Return non-nil if statement at point opens a block.
BOS non-nil means point is known to be at beginning of statement."
  (save-excursion
    (unless bos (python-beginning-of-statement))
    (and (not (python-comment-line-p))
	 (re-search-forward (rx (and ?: (0+ space)
				     (optional (and (syntax comment-start)
						    (0+ not-newline)))
				     line-end))
			    (save-excursion (python-end-of-statement))
			    t)
	 (not (progn (goto-char (match-beginning 0))
		     (python-in-string/comment))))))

(defun python-close-block-statement-p (&optional bos)
  "Return non-nil if current line is a statement closing a block.
BOS non-nil means point is at beginning of statement.
The criteria are that the line isn't a comment or in string and starts with
keyword `raise', `break', `continue' or `pass'."
  (save-excursion
    (unless bos (python-beginning-of-statement))
    (back-to-indentation)
    (looking-at (rx (and (or "return" "raise" "break" "continue" "pass")
			 symbol-end)))))

(defun python-outdent-p ()
  "Return non-nil if current line should outdent a level."
  (save-excursion
    (back-to-indentation)
    (and (looking-at (rx (and (or (and (or "else" "finally") symbol-end)
				  (and (or "except" "elif") symbol-end
				       (1+ (not (any ?:)))))
			      (optional space) ":" (optional space)
			      (or (syntax comment-start) line-end))))
	 (progn (end-of-line)
		(not (python-in-string/comment)))
	 ;; Ensure there's a previous statement and move to it.
	 (zerop (python-previous-statement))
	 (not (python-close-block-statement-p t))
	 ;; Fixme: check this
	 (not (looking-at (rx (and (or (and (or "if" "elif" "except"
						"for" "while")
					    symbol-end (1+ (not (any ?:))))
				       (and "try" symbol-end))
				   (optional space) ":" (optional space)
				   (or (syntax comment-start) line-end)))))
	 (progn (end-of-line)
		(not (python-in-string/comment))))))

;;;; Indentation.

(defcustom python-indent 4
  "*Number of columns for a unit of indentation in Python mode.
See also `\\[python-guess-indent]'"
  :group 'python
  :type 'integer)

(defcustom python-guess-indent t
  "*Non-nil means Python mode guesses `python-indent' for the buffer."
  :type 'boolean
  :group 'python)

(defcustom python-indent-string-contents t
  "*Non-nil means indent contents of multi-line strings together.
This means indent them the same as the preceding non-blank line.
Otherwise indent them to column zero."
  :type '(choice (const :tag "Align with preceding" t)
		 (const :tag "Indent to column 0" nil))
  :group 'python)

(defcustom python-honour-comment-indentation nil
  "Non-nil means indent relative to preceding comment line.
Only do this for comments where the leading comment character is followed
by space.  This doesn't apply to comment lines, which are always indented
in lines with preceding comments."
  :type 'boolean
  :group 'python)

(defcustom python-continuation-offset 4
  "*Number of columns of additional indentation for continuation lines.
Continuation lines follow a backslash-terminated line starting a statement."
  :group 'python
  :type 'integer)

(defun python-guess-indent ()
  "Guess step for indentation of current buffer.
Set `python-indent' locally to the value guessed."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (done indent)
	(while (and (not done) (not (eobp)))
	  (when (and (re-search-forward (rx (and ?: (0+ space)
						 (or (syntax comment-start)
						     line-end)))
					nil 'move)
		     (python-open-block-statement-p))
	    (save-excursion
	      (python-beginning-of-statement)
	      (let ((initial (current-indentation)))
		(if (zerop (python-next-statement))
		    (setq indent (- (current-indentation) initial)))
		(if (and (>= indent 2) (<= indent 8)) ; sanity check
		    (setq done t))))))
	(when done
	  (when (/= indent (default-value 'python-indent))
	    (set (make-local-variable 'python-indent) indent)
	    (unless (= tab-width python-indent)
	      (setq indent-tabs-mode nil)))
	  indent)))))

(defun python-calculate-indentation ()
  "Calculate Python indentation for line at point."
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss))
	  start)
      (cond
       ((eq 'string (syntax-ppss-context syntax)) ; multi-line string
	(if (not python-indent-string-contents)
	    0
	  (save-excursion
	    ;; Find indentation of preceding non-blank line within string.
	    (setq start (nth 8 syntax))
	    (forward-line -1)
	    (while (and (< start (point)) (looking-at "\\s-*$"))
	      (forward-line -1))
	    (current-indentation))))
       ((python-continuation-line-p)
	(let ((point (point))
	      (open-start (cadr syntax)))
	  (if open-start
	      ;; Inside bracketed expression.
	      (progn
		(goto-char (1+ open-start))
		;; Look for first item in list (preceding point) and
		;; align with it, if found.
		(if (with-syntax-table python-space-backslash-table
		      (let ((parse-sexp-ignore-comments t))
			(condition-case ()
			    (progn (forward-sexp)
				   (backward-sexp)
				   (< (point) point))
			  (error nil))))
		    (current-column)
		  ;; Otherwise indent relative to statement start, one
		  ;; level per bracketing level.
		  (goto-char (1+ open-start))
		  (python-beginning-of-statement)
		  (+ (current-indentation) (* (car syntax) python-indent))))
	    ;; Otherwise backslash-continued.
	    (forward-line -1)
	    (if (python-continuation-line-p)
		;; We're past first continuation line.  Align with
		;; previous line.
		(current-indentation)
	      ;; First continuation line.  Indent one step, with an
	      ;; extra one if statement opens a block.
	      (save-excursion
		(python-beginning-of-statement)
		(+ (current-indentation) python-continuation-offset
		   (if (python-open-block-statement-p t)
		       python-indent
		     0)))))))
       ((bobp) 0)
       ;; Fixme: Like python-mode.el; not convinced by this.
       ((looking-at (rx (and (0+ space) (syntax comment-start)
			     (not (any " \t\n"))))) ; non-indentable comment
	(current-indentation))
       (t (let ((point (point)))
	    (if python-honour-comment-indentation
		;; Back over whitespace, newlines, non-indentable comments.
		(catch 'done
		  (while t
		    (if (cond ((bobp))
			      ;; not at comment start
			      ((not (forward-comment -1))
			       (python-beginning-of-statement)
			       t)
			      ;; trailing comment
			      ((/= (current-column) (current-indentation))
			       (python-beginning-of-statement)
			       t)
			      ;; indentable comment like python-mode.el
			      ((and (looking-at (rx (and (syntax comment-start)
							 (or space line-end))))
				    (/= 0 (current-column)))))
			(throw 'done t))))
	      ;; Else back over all comments.
	      (python-skip-comments/blanks t)
	      (python-beginning-of-statement))
	    ;; don't lose on bogus outdent
	    (max 0 (+ (current-indentation)
		      (or (cond ((python-open-block-statement-p t)
				 python-indent)
				((python-close-block-statement-p t)
				 (- python-indent)))
			  (progn (goto-char point)
				 (if (python-outdent-p)
				     (- python-indent)))
			  0)))))))))

(defun python-comment-indent ()
  "`comment-indent-function' for Python."
  ;; If previous non-blank line was a comment, use its indentation.
  ;; FIXME: This seems unnecessary since the default code delegates to
  ;; indent-according-to-mode.  --Stef
  (unless (bobp)
    (save-excursion
      (forward-comment -1)
      (if (eq ?# (char-after)) (current-column)))))

;;;; Cycling through the possible indentations with successive TABs.

;; These don't need to be buffer-local since they're only relevant
;; during a cycle.

;; Alist of possible indentations and start of statement they would close.
(defvar python-indent-list nil
  "Internal use.")
;; Length of the above
(defvar python-indent-list-length nil
  "Internal use.")
;; Current index into the alist.
(defvar python-indent-index nil
  "Internal use.")

(defun python-initial-text ()
  "Text of line following indentation and ignoring any trailing comment."
  (buffer-substring (+ (line-beginning-position) (current-indentation))
		    (save-excursion
		      (end-of-line)
		      (forward-comment -1)
		      (point))))

(defun python-indentation-levels ()
  "Return a list of possible indentations for this line.
Includes the default indentation and those which would close all
enclosing blocks.  Assumes the line has already been indented per
`python-indent-line'.  Elements of the list are actually pairs:
\(INDENTATION . TEXT), where TEXT is the initial text of the
corresponding block opening (or nil)."
  (save-excursion
    (let ((levels (list (cons (current-indentation)
			      (save-excursion
				(if (python-beginning-of-block)
				    (python-initial-text)))))))
      ;; Only one possibility if we immediately follow a block open or
      ;; are in a continuation line.
      (unless (or (python-continuation-line-p)
		  (save-excursion (and (python-previous-statement)
				       (python-open-block-statement-p t))))
	(while (python-beginning-of-block)
	  (push (cons (current-indentation) (python-initial-text))
		levels)))
      levels)))

;; This is basically what `python-indent-line' would be if we didn't
;; do the cycling.
(defun python-indent-line-1 ()
  "Subroutine of `python-indent-line'."
  (let ((target (python-calculate-indentation))
	(pos (- (point-max) (point))))
    (if (= target (current-indentation))
	(if (< (current-column) (current-indentation))
	    (back-to-indentation))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to target)
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

(defun python-indent-line ()
  "Indent current line as Python code.
When invoked via `indent-for-tab-command', cycle through possible
indentations for current line.  The cycle is broken by a command different
from `indent-for-tab-command', i.e. successive TABs do the cycling."
  (interactive)
  ;; Don't do extra work if invoked via `indent-region', for instance.
  (if (not (eq this-command 'indent-for-tab-command))
      (python-indent-line-1)
    (if (eq last-command this-command)
	(if (= 1 python-indent-list-length)
	    (message "Sole indentation")
	  (progn (setq python-indent-index (% (1+ python-indent-index)
					      python-indent-list-length))
		 (beginning-of-line)
		 (delete-horizontal-space)
		 (indent-to (car (nth python-indent-index python-indent-list)))
		 (if (python-block-end-p)
		     (let ((text (cdr (nth python-indent-index
					   python-indent-list))))
		       (if text
			   (message "Closes: %s" text))))))
      (python-indent-line-1)
      (setq python-indent-list (python-indentation-levels)
	    python-indent-list-length (length python-indent-list)
	    python-indent-index (1- python-indent-list-length)))))

(defun python-block-end-p ()
  "Non-nil if this is a line in a statement closing a block,
or a blank line indented to where it would close a block."
  (and (not (python-comment-line-p))
       (or (python-close-block-statement-p t)
	   (< (current-indentation)
	      (save-excursion
		(python-previous-statement)
		(current-indentation))))))

;; Fixme: Define an indent-region-function.  It should probably leave
;; lines alone if the indentation is already at one of the allowed
;; levels.  Otherwise, M-C-\ typically keeps indenting more deeply
;; down a function.

;;;; Movement.

(defun python-beginning-of-defun ()
  "`beginning-of-defun-function' for Python.
Finds beginning of innermost nested class or method definition.
Returns the name of the definition found at the end, or nil if reached
start of buffer."
  (let ((ci (current-indentation))
	(def-re (rx (and line-start (0+ space) (or "def" "class")
			 (1+ space)
			 (group (1+ (or word (syntax symbol)))))))
	found lep def-line)
    (if (python-comment-line-p)
	(setq ci most-positive-fixnum))
    (while (and (not (bobp)) (not found))
      ;; Treat bol at beginning of function as outside function so
      ;; that successive C-M-a makes progress backwards.
      (setq def-line (looking-at def-re))
      (unless (bolp) (end-of-line))
      (setq lep (line-end-position))
      (if (and (re-search-backward def-re nil 'move)
	       ;; Must be less indented or matching top level, or
	       ;; equally indented if we started on a definition line.
	       (let ((in (current-indentation)))
	       (or (and (zerop ci) (zerop in))
		   (= lep (line-end-position)) ; on initial line
		   (and def-line (= in ci))
		   (< in ci)))
	       (not (python-in-string/comment)))
	  (setq found t)))))

(defun python-end-of-defun ()
  "`end-of-defun-function' for Python.
Finds end of innermost nested class or method definition."
  (let ((orig (point))
	(pattern (rx (and line-start (0+ space) (or "def" "class") space))))
    ;; Go to start of current block and check whether it's at top
    ;; level.  If it is, and not a block start, look forward for
    ;; definition statement.
    (when (python-comment-line-p)
      (end-of-line)
      (forward-comment most-positive-fixnum))
    (if (not (python-open-block-statement-p))
	(python-beginning-of-block))
    (if (zerop (current-indentation))
	(unless (python-open-block-statement-p)
	  (while (and (re-search-forward pattern nil 'move)
		      (python-in-string/comment))) ; just loop
	  (unless (eobp)
	    (beginning-of-line)))
      ;; Don't move before top-level statement that would end defun.
      (end-of-line)
      (python-beginning-of-defun))
    ;; If we got to the start of buffer, look forward for
    ;; definition statement.
    (if (and (bobp) (not (looking-at "def\\|class")))
	(while (and (not (eobp))
		    (re-search-forward pattern nil 'move)
		    (python-in-string/comment)))) ; just loop
    ;; We're at a definition statement (or end-of-buffer).
    (unless (eobp)
      (python-end-of-block)
      ;; Count trailing space in defun (but not trailing comments).
      (skip-syntax-forward " >")
      (beginning-of-line))
    ;; Catch pathological case like this, where the beginning-of-defun
    ;; skips to a definition we're not in:
    ;; if ...:
    ;;     ...
    ;; else:
    ;;     ...  # point here
    ;;     ...
    ;;     def ...
    (if (< (point) orig)
	(goto-char (point-max)))))

(defun python-beginning-of-statement ()
  "Go to start of current statement.
Accounts for continuation lines, multi-line strings, and multi-line bracketed
expressions."
  (beginning-of-line)
  (python-beginning-of-string)
  (catch 'foo
    (while (python-continuation-line-p)
      (beginning-of-line)
      (if (python-backslash-continuation-line-p)
	  (while (python-backslash-continuation-line-p)
	    (forward-line -1))
	(python-beginning-of-string)
	;; Skip forward out of nested brackets.
	(condition-case ()		; beware invalid syntax
	    (progn (backward-up-list (syntax-ppss-depth (syntax-ppss))) t)
	  (error (throw 'foo nil))))))
  (back-to-indentation))

(defun python-end-of-statement ()
  "Go to the end of the current statement and return point.
Usually this is the start of the next line, but if this is a
multi-line statement we need to skip over the continuation lines.
On a comment line, go to end of line."
  (end-of-line)
  (while (let (comment)
	   ;; Move past any enclosing strings and sexps, or stop if
	   ;; we're in a comment.
	   (while (let ((s (syntax-ppss)))
		    (cond ((eq 'comment (syntax-ppss-context s))
			   (setq comment t)
			   nil)
			  ((eq 'string (syntax-ppss-context s))
			   ;; Go to start of string and skip it.
			   (goto-char (nth 8 s))
			   (condition-case () ; beware invalid syntax
			       (progn (forward-sexp) t)
			     (error (end-of-line))))
			  ((> (syntax-ppss-depth s) 0)
			   ;; Skip forward out of nested brackets.
			   (condition-case () ; beware invalid syntax
			       (progn (backward-up-list
				       (- (syntax-ppss-depth s)))
				      t)
			     (error (end-of-line))))))
	     (end-of-line))
	   (unless comment
	     (eq ?\\ (char-before))))	; Line continued?
    (end-of-line 2))			; Try next line.
  (point))

(defun python-previous-statement (&optional count)
  "Go to start of previous statement.
With argument COUNT, do it COUNT times.  Stop at beginning of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (python-next-statement (- count))
    (python-beginning-of-statement)
    (while (and (> count 0) (not (bobp)))
      (python-skip-comments/blanks t)
      (python-beginning-of-statement)
      (unless (bobp) (setq count (1- count))))
    count))

(defun python-next-statement (&optional count)
  "Go to start of next statement.
With argument COUNT, do it COUNT times.  Stop at end of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (python-previous-statement (- count))
    (beginning-of-line)
    (while (and (> count 0) (not (eobp)))
      (python-end-of-statement)
      (python-skip-comments/blanks)
      (setq count (1- count)))
    count))

(defun python-beginning-of-block (&optional arg)
  "Go to start of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`python-end-of-block' instead.
If point is on the first line of a block, use its outer block.
If current statement is in column zero, don't move and return nil.
Otherwise return non-nil."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((zerop arg))
   ((< arg 0) (python-end-of-block (- arg)))
   (t
    (let ((point (point)))
      (if (python-comment-line-p)
	  (python-skip-comments/blanks t))
      (python-beginning-of-statement)
      (let ((ci (current-indentation)))
	(if (zerop ci)
	    (not (goto-char point))	; return nil
	  ;; Look upwards for less indented statement.
	  (if (catch 'done
;;; This is slower than the below.
;;; 	  (while (zerop (python-previous-statement))
;;; 	    (when (and (< (current-indentation) ci)
;;; 		       (python-open-block-statement-p t))
;;; 	      (beginning-of-line)
;;; 	      (throw 'done t)))
		(while (and (zerop (forward-line -1)))
		  (when (and (< (current-indentation) ci)
			     (not (python-comment-line-p))
			     ;; Move to beginning to save effort in case
			     ;; this is in string.
			     (progn (python-beginning-of-statement) t)
			     (python-open-block-statement-p t))
		    (beginning-of-line)
		    (throw 'done t)))
		(not (goto-char point))) ; Failed -- return nil
	      (python-beginning-of-block (1- arg)))))))))

(defun python-end-of-block (&optional arg)
  "Go to end of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`python-beginning-of-block' instead.
If current statement is in column zero and doesn't open a block, don't
move and return nil.  Otherwise return t."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (python-beginning-of-block (- arg)))
  (while (and (> arg 0)
	      (let* ((point (point))
		     (_ (if (python-comment-line-p)
			    (python-skip-comments/blanks t)))
		     (ci (current-indentation))
		     (open (python-open-block-statement-p)))
		(if (and (zerop ci) (not open))
		    (not (goto-char point))
		  (catch 'done
		    (while (zerop (python-next-statement))
		      (when (or (and open (<= (current-indentation) ci))
				(< (current-indentation) ci))
			(python-skip-comments/blanks t)
			(beginning-of-line 2)
			(throw 'done t)))
		    (not (goto-char point))))))
    (setq arg (1- arg)))
  (zerop arg))

;;;; Imenu.

(defvar python-recursing)
(defun python-imenu-create-index ()
  "`imenu-create-index-function' for Python.

Makes nested Imenu menus from nested `class' and `def' statements.
The nested menus are headed by an item referencing the outer
definition; it has a space prepended to the name so that it sorts
first with `imenu--sort-by-name' (though, unfortunately, sub-menus
precede it)."
  (unless (boundp 'python-recursing)		; dynamically bound below
    (goto-char (point-min)))		; normal call from Imenu
  (let (index-alist			; accumulated value to return
	name)
    (while (re-search-forward
	    (rx (and line-start (0+ space) ; leading space
		     (or (group "def") (group "class"))	    ; type
		     (1+ space) (group (1+ (or word ?_))))) ; name
	    nil t)
      (unless (python-in-string/comment)
	(let ((pos (match-beginning 0))
	      (name (match-string-no-properties 3)))
	  (if (match-beginning 2)	; def or class?
	      (setq name (concat "class " name)))
	  (save-restriction
	    (narrow-to-defun)
	    (let* ((python-recursing t)
		   (sublist (python-imenu-create-index)))
	      (if sublist
		  (progn (push (cons (concat " " name) pos) sublist)
			 (push (cons name sublist) index-alist))
		(push (cons name pos) index-alist)))))))
    (nreverse index-alist)))

;;;; `Electric' commands.

(defun python-electric-colon (arg)
  "Insert a colon and maybe outdent the line if it is a statement like `else'.
With numeric ARG, just insert that many colons.  With \\[universal-argument],
just insert a single colon."
  (interactive "*P")
  (self-insert-command (if (not (integerp arg)) 1 arg))
  (and (not arg)
       (eolp)
       (python-outdent-p)
       (not (python-in-string/comment))
       (> (current-indentation) (python-calculate-indentation))
       (python-indent-line)))		; OK, do it
(put 'python-electric-colon 'delete-selection t)

(defun python-backspace (arg)
  "Maybe delete a level of indentation on the current line.
If not at the end of line's indentation, or on a comment line, just call
`backward-delete-char-untabify'.  With ARG, repeat that many times."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
	  (bolp)
	  (python-continuation-line-p))
      (backward-delete-char-untabify arg)
    (let ((indent 0))
      (save-excursion
	(while (and (> arg 0) (python-beginning-of-block))
	  (setq arg (1- arg)))
	(when (zerop arg)
	  (setq indent (current-indentation))
	  (message "Closes %s" (python-initial-text))))
      (delete-horizontal-space)
      (indent-to indent))))
(put 'python-backspace 'delete-selection 'supersede)

;;;; pychecker

(defcustom python-check-command "pychecker --stdlib"
  "*Command used to check a Python file."
  :type 'string
  :group 'python)

(defvar python-saved-check-command nil
  "Internal use.")

;; After `sgml-validate-command'.
(defun python-check (command)
  "Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `python-check-command' for the default."
  (interactive
   (list (read-string "Checker command: "
		      (or python-saved-check-command
			  (concat python-check-command " "
				  (let ((name (buffer-file-name)))
				    (if name
					(file-name-nondirectory name))))))))
  (setq python-saved-check-command command)
  (require 'compile)                    ;To define compilation-* variables.
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((compilation-error-regexp-alist
	 (cons '("(\\([^,]+\\), line \\([0-9]+\\))" 1 2)
	       compilation-error-regexp-alist)))
    (compilation-start command)))

;;;; Inferior mode stuff (following cmuscheme).

;; Fixme: Make sure we can work with IPython.

(defcustom python-python-command "python"
  "*Shell command to run Python interpreter.
Any arguments can't contain whitespace.
Note that IPython may not work properly; it must at least be used with the
`-cl' flag, i.e. use `ipython -cl'."
  :group 'python
  :type 'string)

(defcustom python-jython-command "jython"
  "*Shell command to run Jython interpreter.
Any arguments can't contain whitespace."
  :group 'python
  :type 'string)

(defvar python-command python-python-command
  "Actual command used to run Python.
May be `python-python-command' or `python-jython-command'.
Additional arguments are added when the command is used by `run-python'
et al.")

(defvar python-buffer nil
  "The current python process buffer."
  ;; Fixme: a single process is currently assumed, so that this doc
  ;; is misleading.

;;   "*The current python process buffer.
;; To run multiple Python processes, start the first with \\[run-python].
;; It will be in a buffer named *Python*.  Rename that with
;; \\[rename-buffer].  Now start a new process with \\[run-python].  It
;; will be in a new buffer, named *Python*.  Switch between the different
;; process buffers with \\[switch-to-buffer].

;; Commands that send text from source buffers to Python processes have
;; to choose a process to send to.  This is determined by global variable
;; `python-buffer'.  Suppose you have three inferior Pythons running:
;;     Buffer	Process
;;     foo		python
;;     bar		python<2>
;;     *Python*    python<3>
;; If you do a \\[python-send-region-and-go] command on some Python source
;; code, what process does it go to?

;; - In a process buffer (foo, bar, or *Python*), send it to that process.
;; - In some other buffer (e.g. a source file), send it to the process
;;   attached to `python-buffer'.
;; Process selection is done by function `python-proc'.

;; Whenever \\[run-python] starts a new process, it resets `python-buffer'
;; to be the new process's buffer.  If you only run one process, this will
;; do the right thing.  If you run multiple processes, you can change
;; `python-buffer' to another process buffer with \\[set-variable]."
  )

(defconst python-compilation-regexp-alist
  ;; FIXME: maybe these should move to compilation-error-regexp-alist-alist.
  `((,(rx (and line-start (1+ (any " \t")) "File \""
	       (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
	       "\", line " (group (1+ digit))))
     1 2)
    (,(rx (and " in file " (group (1+ not-newline)) " on line "
	       (group (1+ digit))))
     1 2))
  "`compilation-error-regexp-alist' for inferior Python.")

(defvar inferior-python-mode-map
  (let ((map (make-sparse-keymap)))
    ;; This will inherit from comint-mode-map.
    (define-key map "\C-c\C-l" 'python-load-file)
    (define-key map "\C-c\C-v" 'python-check)
    ;; Note that we _can_ still use these commands which send to the
    ;; Python process even at the prompt iff we have a normal prompt,
    ;; i.e. '>>> ' and not '... '.  See the comment before
    ;; python-send-region.  Fixme: uncomment these if we address that.

    ;; (define-key map [(meta ?\t)] 'python-complete-symbol)
    ;; (define-key map "\C-c\C-f" 'python-describe-symbol)
    map))

;; Fixme: This should inherit some stuff from python-mode, but I'm not
;; sure how much: at least some keybindings, like C-c C-f; syntax?;
;; font-locking, e.g. for triple-quoted strings?
(define-derived-mode inferior-python-mode comint-mode "Inferior Python"
  "Major mode for interacting with an inferior Python process.
A Python process can be started with \\[run-python].

Hooks `comint-mode-hook' and `inferior-python-mode-hook' are run in
that order.

You can send text to the inferior Python process from other buffers containing
Python source.
 * `python-switch-to-python' switches the current buffer to the Python
    process buffer.
 * `python-send-region' sends the current region to the Python process.
 * `python-send-region-and-go' switches to the Python process buffer
    after sending the text.
For running multiple processes in multiple buffers, see `python-buffer'.

\\{inferior-python-mode-map}"
  :group 'python
  (set-syntax-table python-mode-syntax-table)
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'comint-input-filter) 'python-input-filter)
  (add-hook 'comint-preoutput-filter-functions #'python-preoutput-filter
	    nil t)
  ;; Still required by `comint-redirect-send-command', for instance
  ;; (and we need to match things like `>>> ... >>> '):
  (set (make-local-variable 'comint-prompt-regexp)
       (rx (and line-start (1+ (and (repeat 3 (any ">.")) ?\s)))))
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-compilation-regexp-alist)
  (compilation-shell-minor-mode 1))

(defcustom inferior-python-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'"
  "*Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters."
  :type 'regexp
  :group 'python)

(defun python-input-filter (str)
  "`comint-input-filter' function for inferior Python.
Don't save anything for STR matching `inferior-python-filter-regexp'."
  (not (string-match inferior-python-filter-regexp str)))

;; Fixme: Loses with quoted whitespace.
(defun python-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (python-args-to-list (substring string (+ 1 where)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if pos (python-args-to-list (substring string pos))))))))

(defvar python-preoutput-result nil
  "Data from last `_emacs_out' line seen by the preoutput filter.")

(defvar python-preoutput-continuation nil
  "If non-nil, funcall this when `python-preoutput-filter' sees `_emacs_ok'.")

(defvar python-preoutput-leftover nil)

;; Using this stops us getting lines in the buffer like
;; >>> ... ... >>>
;; Also look for (and delete) an `_emacs_ok' string and call
;; `python-preoutput-continuation' if we get it.
(defun python-preoutput-filter (s)
  "`comint-preoutput-filter-functions' function: ignore prompts not at bol."
  (when python-preoutput-leftover
    (setq s (concat python-preoutput-leftover s))
    (setq python-preoutput-leftover nil))
  (cond ((and (string-match (rx (and string-start (repeat 3 (any ".>"))
                                     " " string-end))
                            s)
              (/= (let ((inhibit-field-text-motion t))
                    (line-beginning-position))
                  (point)))
         "")
        ((string= s "_emacs_ok\n")
         (when python-preoutput-continuation
           (funcall python-preoutput-continuation)
           (setq python-preoutput-continuation nil))
         "")
        ((string-match "_emacs_out \\(.*\\)\n" s)
         (setq python-preoutput-result (match-string 1 s))
         "")
	((string-match ".*\n" s)
	 s)
	((or (eq t (compare-strings s nil nil "_emacs_ok\n" nil (length s)))
	     (let ((end (min (length "_emacs_out ") (length s))))
	       (eq t (compare-strings s nil end "_emacs_out " nil end))))
	 (setq python-preoutput-leftover s)
	 "")
        (t s)))

;;;###autoload
(defun run-python (&optional cmd noshow)
  "Run an inferior Python process, input and output via buffer *Python*.
CMD is the Python command to run.  NOSHOW non-nil means don't show the
buffer automatically.
If there is a process already running in `*Python*', switch to
that buffer.  Interactively, a prefix arg allows you to edit the initial
command line (default is `python-command'); `-i' etc.  args will be added
to this as appropriate.  Runs the hook `inferior-python-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run Python: " python-command)
		       python-command)))
  (unless cmd (setq cmd python-python-command))
  (setq python-command cmd)
  ;; Fixme: Consider making `python-buffer' buffer-local as a buffer
  ;; (not a name) in Python buffers from which `run-python' &c is
  ;; invoked.  Would support multiple processes better.
  (unless (comint-check-proc python-buffer)
    (let* ((cmdlist (append (python-args-to-list cmd) '("-i")))
	   (path (getenv "PYTHONPATH"))
	   (process-environment		; to import emacs.py
	    (cons (concat "PYTHONPATH=" data-directory
			  (if path (concat ":" path)))
		  process-environment)))
      (set-buffer (apply 'make-comint "Python" (car cmdlist) nil
			 (cdr cmdlist)))
      (setq python-buffer (buffer-name)))
    (inferior-python-mode)
    ;; Load function defintions we need.
    ;; Before the preoutput function was used, this was done via -c in
    ;; cmdlist, but that loses the banner and doesn't run the startup
    ;; file.  The code might be inline here, but there's enough that it
    ;; seems worth putting in a separate file, and it's probably cleaner
    ;; to put it in a module.
    (python-send-string "import emacs"))
  (unless noshow (pop-to-buffer python-buffer)))

;; Fixme: We typically lose if the inferior isn't in the normal REPL,
;; e.g. prompt is `help> '.  Probably raise an error if the form of
;; the prompt is unexpected; actually, it needs to be `>>> ', not
;; `... ', i.e. we're not inputting a block &c.  However, this may not
;; be the place to do it, e.g. we might actually want to send commands
;; having set up such a state.

(defun python-send-command (command)
  "Like `python-send-string' but resets `compilation-minor-mode'."
  (goto-char (point-max))
  (let ((end (marker-position (process-mark (python-proc)))))
    (compilation-forget-errors)
    (python-send-string command)
    (set-marker compilation-parsing-end end)
    (setq compilation-last-buffer (current-buffer))))

(defun python-send-region (start end)
  "Send the region to the inferior Python process."
  ;; The region is evaluated from a temporary file.  This avoids
  ;; problems with blank lines, which have different semantics
  ;; interactively and in files.  It also saves the inferior process
  ;; buffer filling up with interpreter prompts.  We need a Python
  ;; function to remove the temporary file when it has been evaluated
  ;; (though we could probably do it in Lisp with a Comint output
  ;; filter).  This function also catches exceptions and truncates
  ;; tracebacks not to mention the frame of the function itself.
  ;;
  ;; The compilation-minor-mode parsing takes care of relating the
  ;; reference to the temporary file to the source.
  ;;
  ;; Fixme: Write a `coding' header to the temp file if the region is
  ;; non-ASCII.
  (interactive "r")
  (let* ((f (make-temp-file "py"))
	 (command (format "emacs.eexecfile(%S)" f))
	 (orig-start (copy-marker start)))
    (when (save-excursion
	    (goto-char start)
	    (/= 0 (current-indentation))) ; need dummy block
      (save-excursion
	(goto-char orig-start)
	;; Wrong if we had indented code at buffer start.
	(set-marker orig-start (line-beginning-position 0)))
      (write-region "if True:\n" nil f nil 'nomsg))
    (write-region start end f t 'nomsg)
    (with-current-buffer (process-buffer (python-proc))	;Runs python if needed.
      (python-send-command command)
      ;; Tell compile.el to redirect error locations in file `f' to
      ;; positions past marker `orig-start'.  It has to be done *after*
      ;; python-send-command's call to compilation-forget-errors.
      (compilation-fake-loc orig-start f))))

(defun python-send-string (string)
  "Evaluate STRING in inferior Python process."
  (interactive "sPython command: ")
  (comint-send-string (python-proc) string)
  (comint-send-string (python-proc) "\n\n"))

(defun python-send-buffer ()
  "Send the current buffer to the inferior Python process."
  (interactive)
  (python-send-region (point-min) (point-max)))

;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun python-send-defun ()
  "Send the current defun (class or method) to the inferior Python process."
  (interactive)
  (save-excursion (python-send-region (progn (beginning-of-defun) (point))
				      (progn (end-of-defun) (point)))))

(defun python-switch-to-python (eob-p)
  "Switch to the Python process buffer.
With prefix arg, position cursor at end of buffer."
  (interactive "P")
  (pop-to-buffer (process-buffer (python-proc))) ;Runs python if needed.
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun python-send-region-and-go (start end)
  "Send the region to the inferior Python process.
Then switch to the process buffer."
  (interactive "r")
  (python-send-region start end)
  (python-switch-to-python t))

(defcustom python-source-modes '(python-mode jython-mode)
  "*Used to determine if a buffer contains Python source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Python source file by `python-load-file'.
Used by these commands to determine defaults."
  :type '(repeat function)
  :group 'python)

(defvar python-prev-dir/file nil
  "Caches (directory . file) pair used in the last `python-load-file' command.
Used for determining the default in the next one.")

(defun python-load-file (file-name)
  "Load a Python file FILE-NAME into the inferior Python process.
If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive (comint-get-source "Load Python file: " python-prev-dir/file
				  python-source-modes
				  t))	; because execfile needs exact name
  (comint-check-source file-name)     ; Check to see if buffer needs saving.
  (setq python-prev-dir/file (cons (file-name-directory file-name)
				   (file-name-nondirectory file-name)))
  (with-current-buffer (process-buffer (python-proc)) ;Runs python if needed.
    ;; Fixme: I'm not convinced by this logic from python-mode.el.
    (python-send-command
     (if (string-match "\\.py\\'" file-name)
	 (let ((module (file-name-sans-extension
			(file-name-nondirectory file-name))))
	   (format "emacs.eimport(%S,%S)"
		   module (file-name-directory file-name)))
       (format "execfile(%S)" file-name)))
    (message "%s loaded" file-name)))

;; Fixme: If we need to start the process, wait until we've got the OK
;; from the startup.
(defun python-proc ()
  "Return the current Python process.
See variable `python-buffer'.  Starts a new process if necessary."
  (or (if python-buffer
	  (get-buffer-process (if (eq major-mode 'inferior-python-mode)
				  (current-buffer)
				python-buffer)))
      (progn (run-python nil t)
	     (python-proc))))

;;;; Context-sensitive help.

(defconst python-dotty-syntax-table
  (let ((table (make-syntax-table)))
    (set-char-table-parent table python-mode-syntax-table)
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table giving `.' symbol syntax.
Otherwise inherits from `python-mode-syntax-table'.")

(defvar view-return-to-alist)
(eval-when-compile (autoload 'help-buffer "help-fns"))

;; Fixme: Should this actually be used instead of info-look, i.e. be
;; bound to C-h S?  Can we use other pydoc stuff before python 2.2?
(defun python-describe-symbol (symbol)
  "Get help on SYMBOL using `help'.
Interactively, prompt for symbol.

Symbol may be anything recognized by the interpreter's `help' command --
e.g. `CALLS' -- not just variables in scope.
This only works for Python version 2.2 or newer since earlier interpreters
don't support `help'."
  ;; Note that we do this in the inferior process, not a separate one, to
  ;; ensure the environment is appropriate.
  (interactive
   (let ((symbol (with-syntax-table python-dotty-syntax-table
		   (current-word)))
	 (enable-recursive-minibuffers t))
     (list (read-string (if symbol
			    (format "Describe symbol (default %s): " symbol)
			  "Describe symbol: ")
			nil nil symbol))))
  (if (equal symbol "") (error "No symbol"))
  (let* ((func `(lambda ()
		  (comint-redirect-send-command
		   (format "emacs.ehelp(%S, globals(), locals())\n" ,symbol)
		   "*Help*" nil))))
    ;; Ensure we have a suitable help buffer.
    ;; Fixme: Maybe process `Related help topics' a la help xrefs and
    ;; allow C-c C-f in help buffer.
    (let ((temp-buffer-show-hook	; avoid xref stuff
	   (lambda ()
	     (toggle-read-only 1)
	     (setq view-return-to-alist
		   (list (cons (selected-window) help-return-method))))))
      (help-setup-xref (list 'python-describe-symbol symbol) (interactive-p))
      (with-output-to-temp-buffer (help-buffer)
	(with-current-buffer standard-output
	  (set (make-local-variable 'comint-redirect-subvert-readonly) t)
	  (print-help-return-message))))
    (if (and python-buffer (get-buffer python-buffer))
	(with-current-buffer python-buffer
	  (funcall func))
      (setq python-preoutput-continuation func)
      (run-python nil t))))

(add-to-list 'debug-ignored-errors "^No symbol")

(defun python-send-receive (string)
  "Send STRING to inferior Python (if any) and return result.
The result is what follows `_emacs_out' in the output (or nil)."
  (let ((proc (python-proc)))
    (python-send-string string)
    (setq python-preoutput-result nil)
    (while (progn
	     (accept-process-output proc 5)
	     python-preoutput-leftover))
    python-preoutput-result))

;; Fixme: try to make it work with point in the arglist.  Also, is
;; there anything reasonable we can do with random methods?
;; (Currently only works with functions.)
(defun python-eldoc-function ()
  "`eldoc-print-current-symbol-info' for Python.
Only works when point is in a function name, not its arglist, for instance.
Assumes an inferior Python is running."
  (let ((symbol (with-syntax-table python-dotty-syntax-table
		  (current-word))))
    (when symbol
      (python-send-receive (format "emacs.eargs(%S)" symbol)))))

;;;; Info-look functionality.

(defun python-after-info-look ()
  "Set up info-look for Python.
Used with `eval-after-load'."
  (let* ((version (let ((s (shell-command-to-string (concat python-command
							    " -V"))))
		    (string-match "^Python \\([0-9]+\\.[0-9]+\\>\\)" s)
		    (match-string 1 s)))
	 ;; Whether info files have a Python version suffix, e.g. in Debian.
	 (versioned
	  (with-temp-buffer
	    (with-no-warnings (Info-mode))
	    (condition-case ()
		;; Don't use `info' because it would pop-up a *info* buffer.
		(with-no-warnings
		 (Info-goto-node (format "(python%s-lib)Miscellaneous Index"
					 version))
		 t)
	      (error nil)))))
    (info-lookup-maybe-add-help
     :mode 'python-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     ;; Fixme: Can this reasonably be made specific to indices with
     ;; different rules?  Is the order of indices optimal?
     ;; (Miscellaneous in -ref first prefers lookup of keywords, for
     ;; instance.)
     (if versioned
	 ;; The empty prefix just gets us highlighted terms.
	 `((,(concat "(python" version "-ref)Miscellaneous Index") nil "")
	   (,(concat "(python" version "-ref)Module Index" nil ""))
	   (,(concat "(python" version "-ref)Function-Method-Variable Index"
		     nil ""))
	   (,(concat "(python" version "-ref)Class-Exception-Object Index"
		     nil ""))
	   (,(concat "(python" version "-lib)Module Index" nil ""))
	   (,(concat "(python" version "-lib)Class-Exception-Object Index"
		     nil ""))
	   (,(concat "(python" version "-lib)Function-Method-Variable Index"
		     nil ""))
	   (,(concat "(python" version "-lib)Miscellaneous Index" nil "")))
       '(("(python-ref)Miscellaneous Index" nil "")
	 ("(python-ref)Module Index" nil "")
	 ("(python-ref)Function-Method-Variable Index" nil "")
	 ("(python-ref)Class-Exception-Object Index" nil "")
	 ("(python-lib)Module Index" nil "")
	 ("(python-lib)Class-Exception-Object Index" nil "")
	 ("(python-lib)Function-Method-Variable Index" nil "")
	 ("(python-lib)Miscellaneous Index" nil ""))))))
(eval-after-load "info-look" '(python-after-info-look))

;;;; Miscellancy.

(defcustom python-jython-packages '("java" "javax" "org" "com")
  "Packages implying `jython-mode'.
If these are imported near the beginning of the buffer, `python-mode'
actually punts to `jython-mode'."
  :type '(repeat string)
  :group 'python)

;; Called from `python-mode', this causes a recursive call of the
;; mode.  See logic there to break out of the recursion.
(defun python-maybe-jython ()
  "Invoke `jython-mode' if the buffer appears to contain Jython code.
The criterion is either a match for `jython-mode' via
`interpreter-mode-alist' or an import of a module from the list
`python-jython-packages'."
  ;; The logic is taken from python-mode.el.
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((interpreter (if (looking-at auto-mode-interpreter-regexp)
			     (match-string 2))))
	(if (and interpreter (eq 'jython-mode
				 (cdr (assoc (file-name-nondirectory
					      interpreter)
					     interpreter-mode-alist))))
	    (jython-mode)
	  (if (catch 'done
		(while (re-search-forward
			(rx (and line-start (or "import" "from") (1+ space)
				 (group (1+ (not (any " \t\n."))))))
			(+ (point-min) 10000) ; Probably not worth customizing.
			t)
		  (if (member (match-string 1) python-jython-packages)
		      (throw 'done t))))
	      (jython-mode)))))))

(defun python-fill-paragraph (&optional justify)
  "`fill-paragraph-function' handling comments and multi-line strings.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's
indentation and initial comment characters.  Similarly if the end
of the current line is in or at the end of a multi-line string.
Otherwise, do nothing."
  (interactive "P")
  (or (fill-comment-paragraph justify)
      ;; The `paragraph-start' and `paragraph-separate' variables
      ;; don't allow us to delimit the last paragraph in a multi-line
      ;; string properly, so narrow to the string and then fill around
      ;; (the end of) the current line.
      (save-excursion
	(end-of-line)
	(let* ((syntax (syntax-ppss))
	       (orig (point))
	       (start (nth 8 syntax))
	       end)
	  (cond ((eq t (nth 3 syntax))	    ; in fenced string
		 (goto-char (nth 8 syntax)) ; string start
		 (condition-case ()	    ; for unbalanced quotes
		     (progn (forward-sexp)
			    (setq end (point)))
		   (error (setq end (point-max)))))
		((re-search-backward "\\s|\\s-*\\=" nil t) ; end of fenced
							   ; string
		 (forward-char)
		 (setq end (point))
		 (condition-case ()
		     (progn (backward-sexp)
			    (setq start (point)))
		   (error nil))))
	  (when end
	    (save-restriction
	      (narrow-to-region start end)
	      (goto-char orig)
	      (fill-paragraph justify))))))
      t)

(defun python-shift-left (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the left.
COUNT defaults to `python-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie.  It is an error if any lines in the region are indented less than
COUNT columns."
  (interactive (if mark-active
		   (list (region-beginning) (region-end) current-prefix-arg)
		 (list (point) (point) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count python-indent))
  (when (> count 0)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(if (and (< (current-indentation) count)
		 (not (looking-at "[ \t]*$")))
	    (error "Can't shift all lines enough"))
	(forward-line))
      (indent-rigidly start end (- count)))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun python-shift-right (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the right.
COUNT defaults to `python-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie."
  (interactive (if mark-active
		   (list (region-beginning) (region-end) current-prefix-arg)
		 (list (point) (point) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count python-indent))
  (indent-rigidly start end count))

(defun python-outline-level ()
  "`outline-level' function for Python mode.
The level is the number of `python-indent' steps of indentation
of current line."
  (/ (current-indentation) python-indent))

;; Fixme: Consider top-level assignments, imports, &c.
(defun python-current-defun ()
  "`add-log-current-defun-function' for Python."
  (save-excursion
    ;; Move up the tree of nested `class' and `def' blocks until we
    ;; get to zero indentation, accumulating the defined names.
    (let ((start t)
	  accum)
      (while (or start (> (current-indentation) 0))
	(setq start nil)
	(python-beginning-of-block)
	(end-of-line)
	(beginning-of-defun)
	(if (looking-at (rx (and (0+ space) (or "def" "class") (1+ space)
				 (group (1+ (or word (syntax symbol))))
				 ;; Greediness makes this unnecessary?  --Stef
				 symbol-end)))
	    (push (match-string 1) accum)))
      (if accum (mapconcat 'identity accum ".")))))

(defun python-mark-block ()
  "Mark the block around point.
Uses `python-beginning-of-block', `python-end-of-block'."
  (interactive)
  (push-mark)
  (python-beginning-of-block)
  (push-mark (point) nil t)
  (python-end-of-block)
  (exchange-point-and-mark))

;;;; Completion.

(defun python-symbol-completions (symbol)
  "Return a list of completions of the string SYMBOL from Python process.
The list is sorted."
  (when symbol
    (let ((completions
	   (condition-case ()
	       (car (read-from-string (python-send-receive
				       (format "emacs.complete(%S)" symbol))))
	     (error nil))))
      (sort
       ;; We can get duplicates from the above -- don't know why.
       (delete-dups completions)
       #'string<))))

(defun python-partial-symbol ()
  "Return the partial symbol before point (for completion)."
  (let ((end (point))
	(start (save-excursion
		 (and (re-search-backward
		       (rx (and (or buffer-start (regexp "[^[:alnum:]._]"))
				(group (1+ (regexp "[[:alnum:]._]")))
				point))
		       nil t)
		      (match-beginning 1)))))
    (if start (buffer-substring-no-properties start end))))

;; Fixme: We should have an abstraction of this sort of thing in the
;; core.
(defun python-complete-symbol ()
  "Perform completion on the Python symbol preceding point.
Repeating the command scrolls the completion window."
  (interactive)
  (let ((window (get-buffer-window "*Completions*")))
    (if (and (eq last-command this-command)
	     window (window-live-p window) (window-buffer window)
	     (buffer-name (window-buffer window)))
	(with-current-buffer (window-buffer window)
	  (if (pos-visible-in-window-p (point-max) window)
	      (set-window-start window (point-min))
	    (save-selected-window
	      (select-window window)
	      (scroll-up))))
      ;; Do completion.
      (let* ((end (point))
	     (symbol (python-partial-symbol))
	     (completions (python-symbol-completions symbol))
	     (completion (if completions
			     (try-completion symbol completions))))
	(when symbol
	  (cond ((eq completion t))
		((null completion)
		 (message "Can't find completion for \"%s\"" symbol)
		 (ding))
		((not (string= symbol completion))
		 (delete-region (- end (length symbol)) end)
		 (insert completion))
		(t
		 (message "Making completion list...")
		 (with-output-to-temp-buffer "*Completions*"
		   (display-completion-list completions symbol))
		 (message "Making completion list...%s" "done"))))))))

(eval-when-compile (require 'hippie-exp))

(defun python-try-complete (old)
  "Completion function for Python for use with `hippie-expand'."
  (when (eq major-mode 'python-mode)	; though we only add it locally
    (unless old
      (let ((symbol (python-partial-symbol)))
	(he-init-string (- (point) (length symbol)) (point))
	(if (not (he-string-member he-search-string he-tried-table))
	    (push he-search-string he-tried-table))
	(setq he-expand-list
	      (and symbol (python-symbol-completions symbol)))))
    (while (and he-expand-list
		(he-string-member (car he-expand-list) he-tried-table))
      (pop he-expand-list))
    (if he-expand-list
	(progn
	  (he-substitute-string (pop he-expand-list))
	  t)
      (if old (he-reset-string))
      nil)))

;;;; Modes.

(defvar outline-heading-end-regexp)
(defvar eldoc-documentation-function)

;;;###autoload
(define-derived-mode python-mode fundamental-mode "Python"
  "Major mode for editing Python files.
Turns on Font Lock mode unconditionally since it is required for correct
parsing of the source.
See also `jython-mode', which is actually invoked if the buffer appears to
contain Jython code.  See also `run-python' and associated Python mode
commands for running Python under Emacs.

The Emacs commands which work with `defun's, e.g. \\[beginning-of-defun], deal
with nested `def' and `class' blocks.  They take the innermost one as
current without distinguishing method and class definitions.  Used multiple
times, they move over others at the same indentation level until they reach
the end of definitions at that level, when they move up a level.
\\<python-mode-map>
Colon is electric: it outdents the line if appropriate, e.g. for
an else statement.  \\[python-backspace] at the beginning of an indented statement
deletes a level of indentation to close the current block; otherwise it
deletes a charcter backward.  TAB indents the current line relative to
the preceding code.  Successive TABs, with no intervening command, cycle
through the possibilities for indentation on the basis of enclosing blocks.

\\[fill-paragraph] fills comments and multiline strings appropriately, but has no
effect outside them.

Supports Eldoc mode (only for functions, using a Python process),
Info-Look and Imenu.  In Outline minor mode, `class' and `def'
lines count as headers.

\\{python-mode-map}"
  :group 'python
  (set (make-local-variable 'font-lock-defaults)
       '(python-font-lock-keywords nil nil ((?_ . "w")) nil
				   (font-lock-syntactic-keywords
				    . python-font-lock-syntactic-keywords)
				   ;; This probably isn't worth it.
				   ;; (font-lock-syntactic-face-function
				   ;;  . python-font-lock-syntactic-face-function)
				   ))
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-indent-function) #'python-comment-indent)
  (set (make-local-variable 'indent-line-function) #'python-indent-line)
  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function) 'python-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'add-log-current-defun-function)
       #'python-current-defun)
  ;; Fixme: Generalize to do all blocks?
  (set (make-local-variable 'outline-regexp) "\\s-*\\(def\\|class\\)\\>")
  (set (make-local-variable 'outline-heading-end-regexp) ":\\s-*\n")
  (set (make-local-variable 'outline-level) #'python-outline-level)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (make-local-variable 'python-saved-check-command)
  (set (make-local-variable 'beginning-of-defun-function)
       'python-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'python-end-of-defun)
  (setq imenu-create-index-function #'python-imenu-create-index)
  (set (make-local-variable 'eldoc-documentation-function)
       #'python-eldoc-function)
  (add-hook 'eldoc-mode-hook
	    '(lambda () (run-python 0 t)) nil t) ; need it running
  (if (featurep 'hippie-exp)
      (set (make-local-variable 'hippie-expand-try-functions-list)
	   (cons 'python-try-complete hippie-expand-try-functions-list)))
  (unless font-lock-mode (font-lock-mode 1))
  (when python-guess-indent (python-guess-indent))
  (set (make-local-variable 'python-command) python-python-command)
  (unless (boundp 'python-mode-running)	; kill the recursion from jython-mode
    (let ((python-mode-running t))
      (python-maybe-jython))))

(custom-add-option 'python-mode-hook 'imenu-add-menubar-index)
(custom-add-option 'python-mode-hook
		   '(lambda ()
		      "Turn on Indent Tabs mode."
		      (set (make-local-variable 'indent-tabs-mode) t)))
(custom-add-option 'python-mode-hook 'turn-on-eldoc-mode)

;;;###autoload
(define-derived-mode jython-mode python-mode  "Jython"
  "Major mode for editing Jython files.
Like `python-mode', but sets up parameters for Jython subprocesses.
Runs `jython-mode-hook' after `python-mode-hook'."
  :group 'python
  (set (make-local-variable 'python-command) python-jython-command))

(provide 'python)
;; arch-tag: 6fce1d99-a704-4de9-ba19-c6e4912b0554
;;; python.el ends here
