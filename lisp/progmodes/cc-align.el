;;; cc-align.el --- custom indentation functions for CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97,98 Free Software Foundation, Inc.

;; Authors:    1998 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(eval-when-compile
  (require 'cc-defs)
  (require 'cc-vars)
  (require 'cc-engine)
  (require 'cc-langs))


;; Standard indentation line-ups
(defun c-lineup-arglist (langelem)
  ;; lineup the current arglist line with the arglist appearing just
  ;; after the containing paren which starts the arglist.
  (save-excursion
    (let* ((containing-sexp
	    (save-excursion
	      ;; arglist-cont-nonempty gives relpos ==
	      ;; to boi of containing-sexp paren. This
	      ;; is good when offset is +, but bad
	      ;; when it is c-lineup-arglist, so we
	      ;; have to special case a kludge here.
	      (if (memq (car langelem) '(arglist-intro arglist-cont-nonempty))
		  (progn
		    (beginning-of-line)
		    (backward-up-list 1)
		    (skip-chars-forward " \t" (c-point 'eol)))
		(goto-char (cdr langelem)))
	      (point)))
	   (langelem-col (c-langelem-col langelem t)))
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "[ \t]*)"))
	  (progn (goto-char (match-end 0))
		 (c-forward-sexp -1)
		 (forward-char 1)
		 (c-forward-syntactic-ws)
		 (- (current-column) langelem-col))
	(goto-char containing-sexp)
	(or (eolp)
	    (not (memq (char-after) '(?{ ?\( )))
	    (let ((eol (c-point 'eol))
		  (here (progn
			  (forward-char 1)
			  (skip-chars-forward " \t")
			  (point))))
	      (c-forward-syntactic-ws)
	      (if (< (point) eol)
		  (goto-char here))))
	(- (current-column) langelem-col)
	))))

(defun c-lineup-arglist-intro-after-paren (langelem)
  ;; lineup an arglist-intro line to just after the open paren
  (save-excursion
    (let ((langelem-col (c-langelem-col langelem t))
	  (ce-curcol (save-excursion
		       (beginning-of-line)
		       (backward-up-list 1)
		       (skip-chars-forward " \t" (c-point 'eol))
		       (current-column))))
      (- ce-curcol langelem-col -1))))

(defun c-lineup-arglist-close-under-paren (langelem)
  ;; lineup an arglist-close line under the corresponding open paren
  (save-excursion
    (let ((langelem-col (c-langelem-col langelem t))
	  (ce-curcol (save-excursion
		       (beginning-of-line)
		       (backward-up-list 1)
		       (current-column))))
      (- ce-curcol langelem-col))))

(defun c-lineup-close-paren (langelem)
  ;; Indents the closing paren under its corresponding open paren if
  ;; the open paren is followed by code.  If the open paren ends its
  ;; line, no indentation is added.  E.g:
  ;;
  ;; main (int,                main (
  ;;       char **               int, char **
  ;;      )            <->     )              <- c-lineup-close-paren
  ;;
  ;; Works with any type of paren.
  (save-excursion
    (condition-case nil
	(let (opencol spec)
	  (beginning-of-line)
	  (backward-up-list 1)
	  (setq spec (c-looking-at-special-brace-list))
	  (if spec (goto-char (car (car spec))))
	  (setq opencol (current-column))
	  (forward-char 1)
	  (if spec (progn
		     (c-forward-syntactic-ws)
		     (forward-char 1)))
	  (c-forward-syntactic-ws (c-point 'eol))
	  (if (eolp)
	      0
	    (- opencol (c-langelem-col langelem t))))
      (error 0))))

(defun c-lineup-streamop (langelem)
  ;; lineup stream operators
  (save-excursion
    (let ((langelem-col (c-langelem-col langelem)))
      (re-search-forward "<<\\|>>" (c-point 'eol) 'move)
      (goto-char (match-beginning 0))
      (- (current-column) langelem-col))))

(defun c-lineup-multi-inher (langelem)
  ;; line up multiple inheritance lines
  (save-excursion
    (let ((eol (c-point 'eol))
	  (here (point))
	  (langelem-col (c-langelem-col langelem)))
      (skip-chars-forward "^:" eol)
      (skip-chars-forward " \t:" eol)
      (if (or (eolp)
	      (looking-at c-comment-start-regexp))
	  (c-forward-syntactic-ws here))
      (- (current-column) langelem-col)
      )))

(defun c-lineup-java-inher (langelem)
  ;; line up Java implements and extends continuations
  (save-excursion
    (let ((langelem-col (c-langelem-col langelem)))
      (forward-word 1)
      (if (looking-at "[ \t]*$")
	  langelem-col
	(c-forward-syntactic-ws)
	(- (current-column) langelem-col)))))

(defun c-lineup-java-throws (langelem)
  ;; lineup func-decl-cont's in Java which are continuations of throws
  ;; declarations.  If `throws' starts the previous line, line up to
  ;; just after that keyword.  If not, lineup under the previous line.
  (save-excursion
    (let ((iopl (c-point 'iopl))
	  (langelem-col (c-langelem-col langelem t))
	  (extra 0))
      (back-to-indentation)
      (cond
       ((looking-at "throws[ \t\n]")
	(goto-char (cdr langelem))
	(setq extra c-basic-offset))
       ((and (goto-char iopl)
	     (looking-at "throws[ \t\n]"))
	(forward-word 1)
	(skip-chars-forward " \t")
	(if (eolp)
	    (progn
	      (back-to-indentation)
	      (setq extra c-basic-offset))))
       (t (goto-char iopl)))
      (+ (- (current-column) langelem-col) extra))))

(defun c-indent-one-line-block (langelem)
  ;; Adds c-basic-offset to the indentation if the line is a one line
  ;; block, otherwise 0.  E.g:
  ;;
  ;; if (n)                     if (n)
  ;;   {m+=n; n=0;}     <->     {            <- c-indent-one-line-block
  ;;                              m+=n; n=0;
  ;;                            }
  (save-excursion
    (let ((eol (progn (end-of-line) (point))))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (and (eq (following-char) ?{)
	       (c-safe (progn (c-forward-sexp) t))
	       (<= (point) eol)
	       (eq (preceding-char) ?}))
	  c-basic-offset
	0))))

(defun c-lineup-C-comments (langelem)
  ;; line up C block comment continuation lines
  (save-excursion
    (let ((here (point))
	  (stars (progn (back-to-indentation)
			(skip-chars-forward "*")))
	  (langelem-col (c-langelem-col langelem)))
      (back-to-indentation)
      (if (not (re-search-forward "/\\([*]+\\)" (c-point 'eol) t))
	  (progn
	    (if (not (looking-at "[*]+"))
		(progn
		  ;; we now have to figure out where this comment begins.
		  (goto-char here)
		  (back-to-indentation)
		  (if (looking-at "[*]+/")
		      (progn (goto-char (match-end 0))
			     (forward-comment -1))
		    (goto-char (cdr langelem))
		    (back-to-indentation))))
	    (- (current-column) langelem-col))
	(if (zerop stars)
	    (progn
	      (skip-chars-forward " \t")
	      (- (current-column) langelem-col))
	  ;; how many stars on comment opening line?  if greater than
	  ;; on current line, align left.  if less than or equal,
	  ;; align right.  this should also pick up Javadoc style
	  ;; comments.
	  (if (> (length (match-string 1)) stars)
	      (progn
		(back-to-indentation)
		(- (current-column) -1 langelem-col))
	    (- (current-column) stars langelem-col))
	  )))))

(defun c-lineup-comment (langelem)
  ;; support old behavior for comment indentation. we look at
  ;; c-comment-only-line-offset to decide how to indent comment
  ;; only-lines
  (save-excursion
    (back-to-indentation)
    ;; this highly kludgiforous flag prevents the mapcar over
    ;; c-syntactic-context from entering an infinite loop
    (let ((recurse-prevention-flag (boundp 'recurse-prevention-flag)))
      (cond
       ;; CASE 1: preserve comment-column
       (recurse-prevention-flag 0)
       ((= (current-column) comment-column)
	;; we have to subtract out all other indentation
	(- comment-column (apply '+ (mapcar 'c-get-offset
					    c-syntactic-context))))
       ;; indent as specified by c-comment-only-line-offset
       ((not (bolp))
	(or (car-safe c-comment-only-line-offset)
	    c-comment-only-line-offset))
       (t
	(or (cdr-safe c-comment-only-line-offset)
	    (car-safe c-comment-only-line-offset)
	    -1000))			;jam it against the left side
       ))))

(defun c-lineup-runin-statements (langelem)
  ;; line up statements in coding standards which place the first
  ;; statement on the same line as the block opening brace.
  (if (eq (char-after (cdr langelem)) ?{)
      (save-excursion
	(let ((langelem-col (c-langelem-col langelem)))
	  (forward-char 1)
	  (skip-chars-forward " \t")
	  (- (current-column) langelem-col)))
    0))

(defun c-lineup-math (langelem)
  ;; line up math statement-cont after the equals
  (save-excursion
    (let ((equalp (save-excursion
		    (goto-char (c-point 'boi))
		    (skip-chars-forward "^=" (c-point 'eol))
		    (and (eq (char-after) ?=)
			 (- (point) (c-point 'boi)))))
	  (langelem-col (c-langelem-col langelem))
	  donep)
      (while (and (not donep)
		  (< (point) (c-point 'eol)))
	(skip-chars-forward "^=" (c-point 'eol))
	(if (c-in-literal (cdr langelem))
	    (forward-char 1)
	  (setq donep t)))
      (if (or (not (eq (char-after) ?=))
	      (save-excursion
		(forward-char 1)
		(c-forward-syntactic-ws (c-point 'eol))
		(eolp)))
	  ;; there's no equal sign on the line
	  c-basic-offset
	;; calculate indentation column after equals and ws, unless
	;; our line contains an equals sign
	(if (not equalp)
	    (progn
	      (forward-char 1)
	      (skip-chars-forward " \t")
	      (setq equalp 0)))
	(- (current-column) equalp langelem-col))
      )))

(defun c-lineup-ObjC-method-call (langelem)
  ;; Line up methods args as elisp-mode does with function args: go to
  ;; the position right after the message receiver, and if you are at
  ;; (eolp) indent the current line by a constant offset from the
  ;; opening bracket; otherwise we are looking at the first character
  ;; of the first method call argument, so lineup the current line
  ;; with it.
  (save-excursion
    (let* ((extra (save-excursion
		    (back-to-indentation)
		    (c-backward-syntactic-ws (cdr langelem))
		    (if (eq (char-before) ?:)
			(- c-basic-offset)
		      0)))
	   (open-bracket-pos (cdr langelem))
           (open-bracket-col (progn
			       (goto-char open-bracket-pos)
			       (current-column)))
           (target-col (progn
			 (forward-char)
			 (c-forward-sexp)
			 (skip-chars-forward " \t")
			 (if (eolp)
			     (+ open-bracket-col c-basic-offset)
			   (current-column))))
	   )
      (- target-col open-bracket-col extra))))

(defun c-lineup-ObjC-method-args (langelem)
  ;; Line up the colons that separate args. This is done trying to
  ;; align colons vertically.
  (save-excursion
    (let* ((here (c-point 'boi))
	   (curcol (progn (goto-char here) (current-column)))
	   (eol (c-point 'eol))
	   (relpos (cdr langelem))
	   (first-col-column (progn
			       (goto-char relpos)
			       (skip-chars-forward "^:" eol)
			       (and (eq (char-after) ?:)
				    (current-column)))))
      (if (not first-col-column)
	  c-basic-offset
	(goto-char here)
	(skip-chars-forward "^:" eol)
	(if (eq (char-after) ?:)
	    (+ curcol (- first-col-column (current-column)))
	  c-basic-offset)))))

(defun c-lineup-ObjC-method-args-2 (langelem)
  ;; Line up the colons that separate args. This is done trying to
  ;; align the colon on the current line with the previous one.
  (save-excursion
    (let* ((here (c-point 'boi))
	   (curcol (progn (goto-char here) (current-column)))
	   (eol (c-point 'eol))
	   (relpos (cdr langelem))
	   (prev-col-column (progn
			      (skip-chars-backward "^:" relpos)
			      (and (eq (char-before) ?:)
				   (- (current-column) 1)))))
      (if (not prev-col-column)
	  c-basic-offset
	(goto-char here)
	(skip-chars-forward "^:" eol)
	(if (eq (char-after) ?:)
	    (+ curcol (- prev-col-column (current-column)))
	  c-basic-offset)))))

(defun c-lineup-inexpr-block (langelem)
  ;; This function lines up the block for the various constructs that
  ;; uses a block inside an expression.  For constructs matching
  ;; c-lambda-key and c-inexpr-block-key, indentation to the column of
  ;; the beginning of the match is added.  For standalone statement
  ;; blocks, indentation to the column of the opening brace is added.
  (save-excursion
    (back-to-indentation)
    (let ((res (or (c-looking-at-inexpr-block)
		   (if (c-safe (backward-up-list 1)
			       (eq (char-after) ?{))
		       (c-looking-at-inexpr-block)))))
      (if (not res)
	  0
	(goto-char (cdr res))
	(- (current-column)
	   (progn
	     (back-to-indentation)
	     (current-column)))))))

(defun c-lineup-dont-change (langelem)
  ;; Do not change the indentation of the current line
  (save-excursion
    (back-to-indentation)
    (current-column)))



(defun c-snug-do-while (syntax pos)
  "Dynamically calculate brace hanginess for do-while statements.
Using this function, `while' clauses that end a `do-while' block will
remain on the same line as the brace that closes that block.

See `c-hanging-braces-alist' for how to utilize this function as an
ACTION associated with `block-close' syntax."
  (save-excursion
    (let (langelem)
      (if (and (eq syntax 'block-close)
	       (setq langelem (assq 'block-close c-syntactic-context))
	       (progn (goto-char (cdr langelem))
		      (if (eq (char-after) ?{)
			  (c-safe (c-forward-sexp -1)))
		      (looking-at "\\<do\\>[^_]")))
	  '(before)
	'(before after)))))

(defun c-gnu-impose-minimum ()
  "Imposes a minimum indentation for lines inside a top-level construct.
The variable `c-label-minimum-indentation' specifies the minimum
indentation amount."
  (let ((non-top-levels '(defun-block-intro statement statement-cont
			   statement-block-intro statement-case-intro
			   statement-case-open substatement substatement-open
			   case-label label do-while-closure else-clause
			   ))
	(syntax c-syntactic-context)
	langelem)
    (while syntax
      (setq langelem (car (car syntax))
	    syntax (cdr syntax))
      ;; don't adjust comment-only lines
      (cond ((eq langelem 'comment-intro)
	     (setq syntax nil))
	    ((memq langelem non-top-levels)
	     (save-excursion
	       (setq syntax nil)
	       (back-to-indentation)
	       (if (zerop (current-column))
		   (insert (make-string c-label-minimum-indentation 32)))
	       ))
	    ))))


;; Useful for c-hanging-semi&comma-criteria
(defun c-semi&comma-inside-parenlist ()
  "Controls newline insertion after semicolons in parenthesis lists.
If a comma was inserted, no determination is made.  If a semicolon was
inserted inside a parenthesis list, no newline is added otherwise a
newline is added.  In either case, checking is stopped.  This supports
exactly the old newline insertion behavior."
  ;; newline only after semicolon, but only if that semicolon is not
  ;; inside a parenthesis list (e.g. a for loop statement)
  (if (not (eq last-command-char ?\;))
      nil				; continue checking
    (if (condition-case nil
	    (save-excursion
	      (up-list -1)
	      (not (eq (char-after) ?\()))
	  (error t))
	t
      'stop)))

;; Suppresses newlines before non-blank lines
(defun c-semi&comma-no-newlines-before-nonblanks ()
  "Controls newline insertion after semicolons.
If a comma was inserted, no determination is made.  If a semicolon was
inserted, and the following line is not blank, no newline is inserted.
Otherwise, no determination is made."
  (save-excursion
    (if (and (= last-command-char ?\;)
	     ;;(/= (point-max)
	     ;;    (save-excursion (skip-syntax-forward " ") (point))
	     (zerop (forward-line 1))
	     (not (looking-at "^[ \t]*$")))
	'stop
      nil)))

;; Suppresses new lines after semicolons in one-liners methods
(defun c-semi&comma-no-newlines-for-oneline-inliners ()
  "Controls newline insertion after semicolons for some one-line methods.
If a comma was inserted, no determination is made.  Newlines are
suppressed in one-liners, if the line is an in-class inline function.
For other semicolon contexts, no determination is made."
  (let ((syntax (c-guess-basic-syntax))
        (bol (save-excursion
               (if (c-safe (up-list -1) t)
                   (c-point 'bol)
                 -1))))
    (if (and (eq last-command-char ?\;)
             (eq (car (car syntax)) 'inclass)
             (eq (car (car (cdr syntax))) 'topmost-intro)
             (= (c-point 'bol) bol))
        'stop
      nil)))


(provide 'cc-align)
;;; cc-align.el ends here
