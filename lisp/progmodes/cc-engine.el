;;; cc-engine.el --- core syntax guessing engine for CC mode

;; Copyright (C) 1985,1987,1992-2001 Free Software Foundation, Inc.

;; Authors:    2000- Martin Stjernholm
;;	       1998-1999 Barry A. Warsaw and Martin Stjernholm
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The functions which have docstring documentation can be considered
;; part of an API which other packages can use in CC Mode buffers.
;; Otoh, undocumented functions and functions with the documentation
;; in comments are considered purely internal and can change semantics
;; or even disappear in the future.
;;
;; (This policy applies to CC Mode as a whole, not just this file.  It
;; probably also applies to many other Emacs packages, but here it's
;; clearly spelled out.)

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (require 'cc-bytecomp)))

(cc-require 'cc-defs)
(cc-require 'cc-vars)
(cc-require 'cc-langs)

;; Silence the compiler.
(cc-bytecomp-defun buffer-syntactic-context) ; XEmacs


(defun c-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))


(defvar c-in-literal-cache t)
(defvar c-parsing-error nil)

;; KLUDGE ALERT: c-maybe-labelp is used to pass information between
;; c-crosses-statement-barrier-p and c-beginning-of-statement-1.  A
;; better way should be implemented, but this will at least shut up
;; the byte compiler.
(defvar c-maybe-labelp nil)

;; Macros used internally in c-beginning-of-statement-1 for the
;; automaton actions.
(defmacro c-bos-push-state ()
  '(setq stack (cons (cons state saved-pos)
		     stack)))
(defmacro c-bos-pop-state (&optional do-if-done)
  `(if (setq state (car (car stack))
	     saved-pos (cdr (car stack))
	     stack (cdr stack))
       t
     ,do-if-done
     (throw 'loop nil)))
(defmacro c-bos-pop-state-and-retry ()
  '(throw 'loop (setq state (car (car stack))
		      saved-pos (cdr (car stack))
		      ;; Throw nil if stack is empty, else throw non-nil.
		      stack (cdr stack))))
(defmacro c-bos-save-pos ()
  '(setq saved-pos (vector pos tok ptok pptok)))
(defmacro c-bos-restore-pos ()
  '(unless (eq (elt saved-pos 0) start)
     (setq pos (elt saved-pos 0)
	   tok (elt saved-pos 1)
	   ptok (elt saved-pos 2)
	   pptok (elt saved-pos 3))
     (goto-char pos)
     (setq sym nil)))
(defmacro c-bos-save-error-info (missing got)
  `(setq saved-pos (vector pos ,missing ,got)))
(defmacro c-bos-report-error ()
  '(unless noerror
     (setq c-parsing-error
	   (format "No matching `%s' found for `%s' on line %d"
		   (elt saved-pos 1)
		   (elt saved-pos 2)
		   (1+ (count-lines (point-min)
				    (c-point 'bol (elt saved-pos 0))))))))

(defun c-beginning-of-statement-1 (&optional lim ignore-labels
					     noerror comma-delim)
  "Move to the start of the current statement or declaration, or to
the previous one if already at the beginning of one.  Only
statements/declarations on the same level are considered, i.e. don't
move into or out of sexps (not even normal expression parentheses).

Stop at statement continuations like \"else\", \"catch\", \"finally\"
and the \"while\" in \"do ... while\" if the start point is within
them.  If starting at such a continuation, move to the corresponding
statement start.  If at the beginning of a statement, move to the
closest containing statement if there is any.  This might also stop at
a continuation clause.

Labels are treated as separate statements if IGNORE-LABELS is non-nil.
The function is not overly intelligent in telling labels from other
uses of colons; if used outside a statement context it might trip up
on e.g. inherit colons, so IGNORE-LABELS should be used then.  There
should be no such mistakes in a statement context, however.

Macros are ignored unless point is within one, in which case the
content of the macro is treated as normal code.  Aside from any normal
statement starts found in it, stop at the first token of the content
in the macro, i.e. the expression of an \"#if\" or the start of the
definition in a \"#define\".  Also stop at start of macros before
leaving them.

Return 'label if stopped at a label, 'same if stopped at the beginning
of the current statement, 'up if stepped to a containing statement,
'previous if stepped to a preceding statement, 'beginning if stepped
from a statement continuation clause to its start clause, or 'macro if
stepped to a macro start.  Note that 'same and not 'label is returned
if stopped at the same label without crossing the colon character.

LIM may be given to limit the search.  If the search hits the limit,
point will be left at the closest following token, or at the start
position if that is less ('same is returned in this case).

NOERROR turns off error logging to `c-parsing-error'.

Normally only ';' is considered to delimit statements, but if
COMMA-DELIM is non-nil then ',' is treated likewise."

  ;; The bulk of this function is a pushdown automaton that looks at
  ;; statement boundaries and the tokens in c-opt-block-stmt-key.
  ;;
  ;; Note: The position of a boundary is the following token.
  ;;
  ;; Begin with current token, stop when stack is empty and the
  ;; position has been moved.
  ;;
  ;; Common state:
  ;;   "else": Push state, goto state `else':
  ;;     boundary: Goto state `else-boundary':
  ;;       "if": Pop state.
  ;;       boundary: Error, pop state.
  ;;       other: See common state.
  ;;     other: Error, pop state, retry token.
  ;;   "while": Push state, goto state `while':
  ;;     boundary: Save position, goto state `while-boundary':
  ;;       "do": Pop state.
  ;;       boundary: Restore position if it's not at start, pop state.
  ;;       other: See common state.
  ;;     other: Pop state, retry token.
  ;;   "catch" or "finally": Push state, goto state `catch':
  ;;     boundary: Goto state `catch-boundary':
  ;;       "try": Pop state.
  ;;       "catch": Goto state `catch'.
  ;;       boundary: Error, pop state.
  ;;       other: See common state.
  ;;     other: Error, pop state, retry token.
  ;;   other: Do nothing special.
  ;;
  ;; In addition to the above there is some special handling of labels
  ;; and macros.

  (let ((case-fold-search nil)
	(start (point))
	macro-start
	(delims (if comma-delim '(?\; ?,) '(?\;)))
	(c-stmt-delim-chars (if comma-delim
				c-stmt-delim-chars-with-comma
			      c-stmt-delim-chars))
	pos				; Current position.
	boundary-pos			; Position of last boundary.
	after-labels-pos		; Value of tok after first found colon.
	last-label-pos			; Value of tok after last found colon.
	sym				; Current symbol in the alphabet.
	state				; Current state in the automaton.
	saved-pos			; Current saved positions.
	stack				; Stack of conses (state . saved-pos).
	(cond-key (or c-opt-block-stmt-key
		      "\\<\\>"))	; Matches nothing.
	(ret 'same)
	tok ptok pptok			; Pos of last three sexps or bounds.
	c-in-literal-cache c-maybe-labelp saved)

    (save-restriction
      (if lim (narrow-to-region lim (point-max)))

      (if (save-excursion
	    (and (c-beginning-of-macro)
		 (/= (point) start)))
	  (setq macro-start (point)))

      ;; Try to skip over unary operator characters, to register
      ;; that we've moved.
      (while (progn
	       (setq pos (point))
	       (c-backward-syntactic-ws)
	       (/= (skip-chars-backward "-+!*&~@`#") 0)))

      ;; First check for bare semicolon.  Later on we ignore the
      ;; boundaries for statements that doesn't contain any sexp.
      ;; The only thing that is affected is that the error checking
      ;; is a little less strict, and we really don't bother.
      (if (and (memq (char-before) delims)
	       (progn (forward-char -1)
		      (setq saved (point))
		      (c-backward-syntactic-ws)
		      (or (memq (char-before) delims)
			  (memq (char-before) '(?: nil))
			  (eq (char-syntax (char-before)) ?\())))
	  (setq ret 'previous
		pos saved)

	;; Begin at start and not pos to detect macros if we stand
	;; directly after the #.
	(goto-char start)
	(if (looking-at "\\<\\|\\W")
	    ;; Record this as the first token if not starting inside it.
	    (setq tok start))

	(while
	    (catch 'loop ;; Throw nil to break, non-nil to continue.
	      (cond
	       ;; Check for macro start.
	       ((save-excursion
		  (and macro-start
		       (looking-at "[ \t]*[a-zA-Z0-9!]")
		       (progn (skip-chars-backward " \t")
			      (eq (char-before) ?#))
		       (progn (setq saved (1- (point)))
			      (beginning-of-line)
			      (not (eq (char-before (1- (point))) ?\\)))
		       (progn (skip-chars-forward " \t")
			      (eq (point) saved))))
		(goto-char saved)
		(if (and (c-forward-to-cpp-define-body)
			 (progn (c-forward-syntactic-ws start)
				(< (point) start)))
		    ;; Stop at the first token in the content of the macro.
		    (setq pos (point)
			  ignore-labels t) ; Avoid the label check on exit.
		  (setq pos saved
			ret 'macro
			ignore-labels t))
		(throw 'loop nil))

	       ;; Do a round through the automaton if we found a
	       ;; boundary or if looking at a statement keyword.
	       ((or sym
		    (and (looking-at cond-key)
			 (setq sym (intern (match-string 1)))))

		(when (and (< pos start) (null stack))
		  (throw 'loop nil))

		;; The state handling.  Continue in the common state for
		;; unhandled cases.
		(or (cond
		     ((eq state 'else)
		      (if (eq sym 'boundary)
			  (setq state 'else-boundary)
			(c-bos-report-error)
			(c-bos-pop-state-and-retry)))

		     ((eq state 'else-boundary)
		      (cond ((eq sym 'if)
			     (c-bos-pop-state (setq ret 'beginning)))
			    ((eq sym 'boundary)
			     (c-bos-report-error)
			     (c-bos-pop-state))))

		     ((eq state 'while)
		      (if (and (eq sym 'boundary)
			       ;; Since this can cause backtracking we do a
			       ;; little more careful analysis to avoid it:
			       ;; If there's a label in front of the while
			       ;; it can't be part of a do-while.
			       (not after-labels-pos))
			  (progn (c-bos-save-pos)
				 (setq state 'while-boundary))
			(c-bos-pop-state-and-retry)))

		     ((eq state 'while-boundary)
		      (cond ((eq sym 'do)
			     (c-bos-pop-state (setq ret 'beginning)))
			    ((eq sym 'boundary)
			     (c-bos-restore-pos)
			     (c-bos-pop-state))))

		     ((eq state 'catch)
		      (if (eq sym 'boundary)
			  (setq state 'catch-boundary)
			(c-bos-report-error)
			(c-bos-pop-state-and-retry)))

		     ((eq state 'catch-boundary)
		      (cond
		       ((eq sym 'try)
			(c-bos-pop-state (setq ret 'beginning)))
		       ((eq sym 'catch)
			(setq state 'catch))
		       ((eq sym 'boundary)
			(c-bos-report-error)
			(c-bos-pop-state)))))

		    ;; This is state common.
		    (cond ((eq sym 'boundary)
			   (if (< pos start)
			       (c-bos-pop-state)
			     (c-bos-push-state)))
			  ((eq sym 'else)
			   (c-bos-push-state)
			   (c-bos-save-error-info 'if 'else)
			   (setq state 'else))
			  ((eq sym 'while)
			   (when (or (not pptok)
				     (memq (char-after pptok) delims))
			     ;; Since this can cause backtracking we do a
			     ;; little more careful analysis to avoid it: If
			     ;; the while isn't followed by a semicolon it
			     ;; can't be a do-while.
			     (c-bos-push-state)
			     (setq state 'while)))
			  ((memq sym '(catch finally))
			   (c-bos-push-state)
			   (c-bos-save-error-info 'try sym)
			   (setq state 'catch))))

		(when c-maybe-labelp
		  ;; We're either past a statement boundary or at the
		  ;; start of a statement, so throw away any label data
		  ;; for the previous one.
		  (setq after-labels-pos nil
			last-label-pos nil
			c-maybe-labelp nil))))

	      ;; Step to next sexp, but not if we crossed a boundary, since
	      ;; that doesn't consume an sexp.
	      (if (eq sym 'boundary)
		  (setq ret 'previous)
		(while
		    (progn
		      (or (c-safe (goto-char (scan-sexps (point) -1)) t)
			  (throw 'loop nil))
		      (cond ((looking-at "\\\\$")
			     ;; Step again if we hit a line continuation.
			     t)
			    (macro-start
			     ;; If we started inside a macro then this
			     ;; sexp is always interesting.
			     nil)
			    (t
			     ;; Otherwise check that we didn't step
			     ;; into a macro from the end.
			     (let ((macro-start
				    (save-excursion
				      (and (c-beginning-of-macro)
					   (point)))))
			       (when macro-start
				 (goto-char macro-start)
				 t))))))

		;; Check for statement boundary.
		(when (save-excursion
			(if (if (eq (char-after) ?{)
				(c-looking-at-inexpr-block lim nil)
			      (eq (char-syntax (char-after)) ?\())
			    ;; Need to move over parens and
			    ;; in-expression blocks to get a good start
			    ;; position for the boundary check.
			    (c-forward-sexp 1))
			(setq boundary-pos (c-crosses-statement-barrier-p
					    (point) pos)))
		  (setq pptok ptok
			ptok tok
			tok boundary-pos
			sym 'boundary)
		  (throw 'loop t)))

	      (when (and (numberp c-maybe-labelp) (not ignore-labels))
		;; c-crosses-statement-barrier-p has found a colon, so
		;; we might be in a label now.
		(if (not after-labels-pos)
		    (setq after-labels-pos tok))
		(setq last-label-pos tok
		      c-maybe-labelp t))

	      ;; ObjC method def?
	      (when (and c-opt-method-key
			 (setq saved (c-in-method-def-p)))
		(setq pos saved
		      ignore-labels t)	; Avoid the label check on exit.
		(throw 'loop nil))

	      (setq sym nil
		    pptok ptok
		    ptok tok
		    tok (point)
		    pos tok)))		; Not nil.

	;; If the stack isn't empty there might be errors to report.
	(while stack
	  (if (and (vectorp saved-pos) (eq (length saved-pos) 3))
	      (c-bos-report-error))
	  (setq saved-pos (cdr (car stack))
		stack (cdr stack)))

	(when (and (eq ret 'same)
		   (not (memq sym '(boundary ignore nil))))
	  ;; Need to investigate closer whether we've crossed
	  ;; between a substatement and its containing statement.
	  (if (setq saved (if (looking-at c-block-stmt-1-key)
			      ptok
			    pptok))
	      (cond ((> start saved) (setq pos saved))
		    ((= start saved) (setq ret 'up)))))

	(when (and c-maybe-labelp (not ignore-labels) after-labels-pos)
	  ;; We're in a label.  Maybe we should step to the statement
	  ;; after it.
	  (if (< after-labels-pos start)
	      (setq pos after-labels-pos)
	    (setq ret 'label)
	    (if (< last-label-pos start)
		(setq pos last-label-pos)))))

      ;; Skip over the unary operators that can start the statement.
      (goto-char pos)
      (while (progn
	       (c-backward-syntactic-ws)
	       (/= (skip-chars-backward "-+!*&~@`#") 0))
	(setq pos (point)))
      (goto-char pos)
      ret)))

(defun c-crosses-statement-barrier-p (from to)
  "Return non-nil if buffer positions FROM to TO cross one or more
statement or declaration boundaries.  The returned value is actually
the position of the earliest boundary char.

The variable `c-maybe-labelp' is set to the position of the first `:' that
might start a label (i.e. not part of `::' and not preceded by `?').  If a
single `?' is found, then `c-maybe-labelp' is cleared."
  (let ((skip-chars c-stmt-delim-chars)
	lit-range)
    (save-excursion
      (catch 'done
	(goto-char from)
	(while (progn (skip-chars-forward skip-chars to)
		      (< (point) to))
	  (if (setq lit-range (c-literal-limits from))
	      (goto-char (setq from (cdr lit-range)))
	    (cond ((eq (char-after) ?:)
		   (forward-char)
		   (if (and (eq (char-after) ?:)
			    (< (point) to))
		       ;; Ignore scope operators.
		       (forward-char)
		     (setq c-maybe-labelp (1- (point)))))
		  ((eq (char-after) ??)
		   ;; A question mark.  Can't be a label, so stop
		   ;; looking for more : and ?.
		   (setq c-maybe-labelp nil
			 skip-chars (substring c-stmt-delim-chars 0 -2)))
		  (t (throw 'done (point))))))
	nil))))


;; This is a dynamically bound cache used together with
;; c-query-macro-start and c-query-and-set-macro-start.  It only works
;; as long as point doesn't cross a macro boundary.
(defvar c-macro-start 'unknown)

(defsubst c-query-and-set-macro-start ()
  (if (symbolp c-macro-start)
      (setq c-macro-start (save-excursion
			    (and (c-beginning-of-macro)
				 (point))))
    c-macro-start))

(defsubst c-query-macro-start ()
  (if (symbolp c-macro-start)
      (save-excursion
	(and (c-beginning-of-macro)
	     (point)))
    c-macro-start))

(defun c-beginning-of-macro (&optional lim)
  "Go to the beginning of a cpp macro definition.
Leave point at the beginning of the macro and return t if in a cpp
macro definition, otherwise return nil and leave point unchanged."
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
	(forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
	       (looking-at "#[ \t]*[a-zA-Z0-9!]"))
	  t
	(goto-char here)
	nil))))

(defun c-end-of-macro ()
  "Go to the end of a cpp macro definition.
More accurately, move point to the end of the closest following line
that doesn't end with a line continuation backslash."
  (while (progn
	   (end-of-line)
	   (when (and (eq (char-before) ?\\)
		      (not (eobp)))
	     (forward-char)
	     t))))

(defun c-forward-comment (count)
  ;; Insulation from various idiosyncrasies in implementations of
  ;; `forward-comment'.
  ;;
  ;; Note: Some emacsen considers incorrectly that any line comment
  ;; ending with a backslash continues to the next line.  I can't
  ;; think of any way to work around that in a reliable way without
  ;; changing the buffer, though.  Suggestions welcome. ;)  (No,
  ;; temporarily changing the syntax for backslash doesn't work since
  ;; we must treat escapes in string literals correctly.)
  ;;
  ;; Another note: When moving backwards over a block comment, there's
  ;; a bug in forward-comment that can make it stop at "/*" inside a
  ;; line comment.  Haven't yet found a reasonably cheap way to kludge
  ;; around that one either. :\
  (let ((here (point)))
    (if (>= count 0)
	(when (forward-comment count)
	  (if (eobp)
	      ;; Some emacsen (e.g. XEmacs 21) return t when moving
	      ;; forwards at eob.
	      nil
	    ;; Emacs includes the ending newline in a b-style (c++)
	    ;; comment, but XEmacs doesn't.  We depend on the Emacs
	    ;; behavior (which also is symmetric).
	    (if (and (eolp) (nth 7 (parse-partial-sexp here (point))))
		(condition-case nil (forward-char 1)))
	    t))
      ;; When we got newline terminated comments,
      ;; forward-comment in all supported emacsen so far will
      ;; stop at eol of each line not ending with a comment when
      ;; moving backwards.  The following corrects for it when
      ;; count is -1.  The other common case, when count is
      ;; large and negative, works regardless.  It's too much
      ;; work to correct for the rest of the cases.
      (skip-chars-backward " \t\n\r\f")
      (if (bobp)
	  ;; Some emacsen return t when moving backwards at bob.
	  nil
	(re-search-forward "[\n\r]" here t)
	(let* ((res (if (forward-comment count)
			(if (eolp) (forward-comment -1) t)))
	       (savepos (point)))
	  ;; XEmacs treats line continuations as whitespace (but only
	  ;; in the backward direction).
	  (while (and (progn (end-of-line) (< (point) here))
		      (eq (char-before) ?\\))
	    (setq res nil
		  savepos (point))
	    (forward-line))
	  (goto-char savepos)
	  res)))))

(defun c-forward-comment-lc (count)
  ;; Like `c-forward-comment', but treat line continuations as
  ;; whitespace.
  (catch 'done
    (if (> count 0)
	(while (if (c-forward-comment 1)
		   (progn
		     (setq count (1- count))
		     (> count 0))
		 (if (looking-at "\\\\$")
		     (progn
		       (forward-char)
		       t)
		   (throw 'done nil))))
      (while (if (c-forward-comment -1)
		 (progn
		   (setq count (1+ count))
		   (< count 0))
	       (if (and (eolp) (eq (char-before) ?\\))
		   (progn
		     (backward-char)
		     t)
		 (throw 'done nil)))))
    t))

(defun c-forward-syntactic-ws (&optional lim)
  "Forward skip of syntactic whitespace.
Syntactic whitespace is defined as whitespace characters, comments,
and preprocessor directives.  However if point starts inside a comment
or preprocessor directive, the content of it is not treated as
whitespace.  LIM sets an upper limit of the forward movement, if
specified."
  (let ((here (point-max)))
    (or lim (setq lim here))
    (while (/= here (point))
      ;; If forward-comment in at least XEmacs 21 is given a large
      ;; positive value, it'll loop all the way through if it hits eob.
      (while (c-forward-comment 5))
      (setq here (point))
      (cond
       ;; Skip line continuations.
       ((looking-at "\\\\$")
	(forward-char))
       ;; Skip preprocessor directives.
       ((and (looking-at "#[ \t]*[a-zA-Z0-9!]")
	     (progn (skip-chars-backward " \t")
		    (bolp)))
	(end-of-line)
	(while (and (<= (point) lim)
		    (eq (char-before) ?\\)
			 (= (forward-line 1) 0))
	  (end-of-line))
	(when (> (point) lim)
	  ;; Don't move past the macro if that'd take us past the limit.
	  (goto-char here)))
       ;; Skip in-comment line continuations (used for Pike refdoc).
       ((and c-opt-in-comment-lc (looking-at c-opt-in-comment-lc))
	(goto-char (match-end 0)))))
    (goto-char (min (point) lim))))

(defun c-backward-syntactic-ws (&optional lim)
  "Backward skip of syntactic whitespace.
Syntactic whitespace is defined as whitespace characters, comments,
and preprocessor directives.  However if point starts inside a comment
or preprocessor directive, the content of it is not treated as
whitespace.  LIM sets a lower limit of the backward movement, if
specified."
  (let ((start-line (c-point 'bol))
	(here (point-min))
	(line-cont 'maybe)
	prev-pos)
    (or lim (setq lim here))
    (while (/= here (point))
      (setq prev-pos (point))
      ;; If forward-comment in Emacs 19.34 is given a large negative
      ;; value, it'll loop all the way through if it hits bob.
      (while (c-forward-comment -5))
      (setq here (point))
      (cond
       ((and (eolp)
	     (eq (char-before) ?\\)
	     (if (<= prev-pos (c-point 'eonl))
		 t
	       ;; Passed a line continuation, but not from the line we
	       ;; started on.
	       (forward-char)
	       (setq line-cont nil)))
	(backward-char)
	(setq line-cont t))
       ((progn
	  (when (eq line-cont 'maybe)
	    (save-excursion
	      (end-of-line)
	      (setq line-cont (eq (char-before) ?\\))))
	  (or line-cont
	      (and (< (point) start-line)
		   (c-beginning-of-macro))))
	(if (< (point) lim)
	    ;; Don't move past the macro if we began inside it or at
	    ;; the end of the same line, or if the move would take us
	    ;; past the limit.
	    (goto-char here))
	(setq line-cont nil))
       ;; Skip in-comment line continuations (used for Pike refdoc).
       ((and c-opt-in-comment-lc
	     (save-excursion
	       (and (c-safe (beginning-of-line)
			    (backward-char 2)
			    t)
		    (looking-at c-opt-in-comment-lc)
		    (eq (match-end 0) here))))
	(goto-char (match-beginning 0)))))
    (goto-char (max (point) lim))))

(defun c-forward-token-1 (&optional count balanced lim)
  "Move forward by tokens.
A token is defined as all symbols and identifiers which aren't
syntactic whitespace \(note that e.g. \"->\" is considered to be two
tokens).  Point is always either left at the beginning of a token or
not moved at all.  COUNT specifies the number of tokens to move; a
negative COUNT moves in the opposite direction.  A COUNT of 0 moves to
the next token beginning only if not already at one.  If BALANCED is
true, move over balanced parens, otherwise move into them.  Also, if
BALANCED is true, never move out of an enclosing paren.  LIM sets the
limit for the movement and defaults to the point limit.

Return the number of tokens left to move \(positive or negative).  If
BALANCED is true, a move over a balanced paren counts as one.  Note
that if COUNT is 0 and no appropriate token beginning is found, 1 will
be returned.  Thus, a return value of 0 guarantees that point is at
the requested position and a return value less \(without signs) than
COUNT guarantees that point is at the beginning of some token."
  (or count (setq count 1))
  (if (< count 0)
      (- (c-backward-token-1 (- count) balanced lim))
    (let ((jump-syntax (if balanced
			   '(?w ?_ ?\( ?\) ?\" ?\\ ?/ ?$ ?')
			 '(?w ?_ ?\" ?\\ ?/ ?')))
	  (last (point))
	  (prev (point)))
      (save-restriction
	(if lim (narrow-to-region (point-min) lim))
	(if (/= (point)
		(progn (c-forward-syntactic-ws) (point)))
	    ;; Skip whitespace.  Count this as a move if we did in fact
	    ;; move and aren't out of bounds.
	    (or (eobp)
		(setq count (max (1- count) 0))))
	(if (and (= count 0)
		 (or (and (memq (char-syntax (or (char-after) ? )) '(?w ?_))
			  (memq (char-syntax (or (char-before) ? )) '(?w ?_)))
		     (eobp)))
	    ;; If count is zero we should jump if in the middle of a
	    ;; token or if there is whitespace between point and the
	    ;; following token beginning.
	    (setq count 1))
	(if (eobp)
	    (goto-char last)
	  ;; Avoid having the limit tests inside the loop.
	  (condition-case nil
	      (while (> count 0)
		(setq prev last
		      last (point))
		(if (memq (char-syntax (char-after)) jump-syntax)
		    (goto-char (scan-sexps (point) 1))
		  (forward-char))
		(c-forward-syntactic-ws)
		(setq count (1- count)))
	    (error (goto-char last)))
	  (when (eobp)
	    (goto-char prev)
	    (setq count (1+ count)))))
      count)))

(defun c-backward-token-1 (&optional count balanced lim)
  "Move backward by tokens.
See `c-forward-token-1' for details."
  (or count (setq count 1))
  (if (< count 0)
      (- (c-forward-token-1 (- count) balanced lim))
    (let ((jump-syntax (if balanced
			   '(?w ?_ ?\( ?\) ?\" ?\\ ?/ ?$ ?')
			 '(?w ?_ ?\" ?\\ ?/ ?')))
	  last)
      (if (and (= count 0)
	       (or (and (memq (char-syntax (or (char-after) ? )) '(?w ?_))
			(memq (char-syntax (or (char-before) ? )) '(?w ?_)))
		   (/= (point)
		       (save-excursion
			 (c-forward-syntactic-ws (1+ lim))
			 (point)))
		   (eobp)))
	  ;; If count is zero we should jump if in the middle of a
	  ;; token or if there is whitespace between point and the
	  ;; following token beginning.
	  (setq count 1))
      (save-restriction
	(if lim (narrow-to-region lim (point-max)))
	(or (bobp)
	    (progn
	      ;; Avoid having the limit tests inside the loop.
	      (condition-case nil
		  (while (progn
			   (setq last (point))
			   (> count 0))
		    (c-backward-syntactic-ws)
		    (if (memq (char-syntax (char-before)) jump-syntax)
			(goto-char (scan-sexps (point) -1))
		      (backward-char))
		    (setq count (1- count)))
		(error (goto-char last)))
	      (if (bobp) (goto-char last)))))
      count)))

(defun c-syntactic-re-search-forward (regexp &optional bound noerror count
					     paren-level)
  ;; Like `re-search-forward', but only report matches that are found
  ;; in syntactically significant text.  I.e. matches that begins in
  ;; comments, macros or string literals are ignored.  The start point
  ;; is assumed to be outside any comment, macro or string literal, or
  ;; else the content of that region is taken as syntactically
  ;; significant text.  If PAREN-LEVEL is non-nil, an additional
  ;; restriction is added to ignore matches in nested paren sexps, and
  ;; the search will also not go outside the current paren sexp.
  (or bound (setq bound (point-max)))
  (or count (setq count 1))
  (if paren-level (setq paren-level -1))
  (let ((start (point))
	(pos (point))
	match-pos state)
    (condition-case err
	(while (and (> count 0)
		    (re-search-forward regexp bound noerror))
	  (setq match-pos (point)
		state (parse-partial-sexp pos (match-beginning 0)
					  paren-level nil state)
		pos (point))
	  (cond ((nth 3 state)
		 ;; Match inside a string.  Skip to the end of it
		 ;; before continuing.
		 (let ((ender (make-string 1 (nth 3 state))))
		   (while (progn
			    (search-forward ender bound noerror)
			    (setq state (parse-partial-sexp pos (point)
							    nil nil state)
				  pos (point))
			    (nth 3 state)))))
		((nth 7 state)
		 ;; Match inside a line comment.  Skip to eol.  Use
		 ;; re-search-forward for it to get the right bound
		 ;; behavior.
		 (re-search-forward "[\n\r]" bound noerror))
		((nth 4 state)
		 ;; Match inside a block comment.  Skip to the '*/'.
		 (re-search-forward "\\*/" bound noerror))
		((save-excursion (c-beginning-of-macro start))
		 ;; Match inside a macro.  Skip to the end of it.
		 (c-end-of-macro))
		((and paren-level (/= (car state) 0))
		 (if (> (car state) 0)
		     ;; Match inside a nested paren sexp.  Skip out of it.
		     (setq state (parse-partial-sexp pos bound 0 nil state)
			   pos (point))
		   ;; Have exited the current paren sexp.  The
		   ;; parse-partial-sexp above has left us just after
		   ;; the closing paren in this case.  Just make
		   ;; re-search-forward above fail in the appropriate
		   ;; way; we'll adjust the leave off point below if
		   ;; necessary.
		   (setq bound (point))))
		(t
		 ;; A real match.
		 (setq count (1- count)))))
      (error
       (goto-char start)
       (signal (car err) (cdr err))))
    (if (= count 0)
	(progn
	  (goto-char match-pos)
	  match-pos)
      ;; Search failed.  Set point as appropriate.
      (cond ((eq noerror t)
	     (goto-char start))
	    (paren-level
	     (if (eq (car (parse-partial-sexp pos bound -1 nil state)) -1)
		 (backward-char)))
	    (t
	     (goto-char bound)))
      nil)))


(defun c-in-literal (&optional lim detect-cpp)
  "Return the type of literal point is in, if any.
The return value is `c' if in a C-style comment, `c++' if in a C++
style comment, `string' if in a string literal, `pound' if DETECT-CPP
is non-nil and on a preprocessor line, or nil if somewhere else.
Optional LIM is used as the backward limit of the search.  If omitted,
or nil, `c-beginning-of-defun' is used.

The last point calculated is cached if the cache is enabled, i.e. if
`c-in-literal-cache' is bound to a two element vector."
  (if (and (vectorp c-in-literal-cache)
	   (= (point) (aref c-in-literal-cache 0)))
      (aref c-in-literal-cache 1)
    (let ((rtn (save-excursion
		 (let* ((lim (or lim (c-point 'bod)))
			(state (parse-partial-sexp lim (point))))
		   (cond
		    ((nth 3 state) 'string)
		    ((nth 4 state) (if (nth 7 state) 'c++ 'c))
		    ((and detect-cpp (c-beginning-of-macro lim)) 'pound)
		    (t nil))))))
      ;; cache this result if the cache is enabled
      (if (not c-in-literal-cache)
	  (setq c-in-literal-cache (vector (point) rtn)))
      rtn)))

;; XEmacs has a built-in function that should make this much quicker.
;; I don't think we even need the cache, which makes our lives more
;; complicated anyway.  In this case, lim is only used to detect
;; cpp directives.
(defun c-fast-in-literal (&optional lim detect-cpp)
  (let ((context (buffer-syntactic-context)))
    (cond
     ((eq context 'string) 'string)
     ((eq context 'comment) 'c++)
     ((eq context 'block-comment) 'c)
     ((and detect-cpp (save-excursion (c-beginning-of-macro lim))) 'pound))))

(if (fboundp 'buffer-syntactic-context)
    (defalias 'c-in-literal 'c-fast-in-literal))

(defun c-literal-limits (&optional lim near not-in-delimiter)
  "Return a cons of the beginning and end positions of the comment or
string surrounding point (including both delimiters), or nil if point
isn't in one.  If LIM is non-nil, it's used as the \"safe\" position
to start parsing from.  If NEAR is non-nil, then the limits of any
literal next to point is returned.  \"Next to\" means there's only [
\t] between point and the literal.  The search for such a literal is
done first in forward direction.  If NOT-IN-DELIMITER is non-nil, the
case when point is inside a starting delimiter won't be recognized.
This only has effect for comments, which have starting delimiters with
more than one character."
  (save-excursion
    (let* ((pos (point))
	   (lim (or lim (c-point 'bod)))
	   (state (parse-partial-sexp lim (point))))
      (cond ((nth 3 state)
	     ;; String.  Search backward for the start.
	     (while (nth 3 state)
	       (search-backward (make-string 1 (nth 3 state)))
	       (setq state (parse-partial-sexp lim (point))))
	     (cons (point) (or (c-safe (c-forward-sexp 1) (point))
			       (point-max))))
	    ((nth 7 state)
	     ;; Line comment.  Search from bol for the comment starter.
	     (beginning-of-line)
	     (setq state (parse-partial-sexp lim (point))
		   lim (point))
	     (while (not (nth 7 state))
	       (search-forward "//")	; Should never fail.
	       (setq state (parse-partial-sexp
			    lim (point) nil nil state)
		     lim (point)))
	     (backward-char 2)
	     (cons (point) (progn (c-forward-comment 1) (point))))
	    ((nth 4 state)
	     ;; Block comment.  Search backward for the comment starter.
	     (while (nth 4 state)
	       (search-backward "/*")	; Should never fail.
	       (setq state (parse-partial-sexp lim (point))))
	     (cons (point) (progn (c-forward-comment 1) (point))))
	    ((and (not not-in-delimiter)
		  (not (nth 5 state))
		  (eq (char-before) ?/)
		  (looking-at "[/*]"))
	     ;; We're standing in a comment starter.
	     (backward-char 1)
	     (cons (point) (progn (c-forward-comment 1) (point))))
	    (near
	     (goto-char pos)
	     ;; Search forward for a literal.
	     (skip-chars-forward " \t")
	     (cond
	      ((eq (char-syntax (or (char-after) ?\ )) ?\") ; String.
	       (cons (point) (or (c-safe (c-forward-sexp 1) (point))
				 (point-max))))
	      ((looking-at "/[/*]")	; Line or block comment.
	       (cons (point) (progn (c-forward-comment 1) (point))))
	      (t
	       ;; Search backward.
	       (skip-chars-backward " \t")
	       (let ((end (point)) beg)
		 (cond
		  ((eq (char-syntax (or (char-before) ?\ )) ?\") ; String.
		   (setq beg (c-safe (c-backward-sexp 1) (point))))
		  ((and (c-safe (forward-char -2) t)
			(looking-at "*/"))
		   ;; Block comment.  Due to the nature of line
		   ;; comments, they will always be covered by the
		   ;; normal case above.
		   (goto-char end)
		   (c-forward-comment -1)
		   ;; If LIM is bogus, beg will be bogus.
		   (setq beg (point))))
		 (if beg (cons beg end))))))
	    ))))

(defun c-literal-limits-fast (&optional lim near not-in-delimiter)
  ;; Like c-literal-limits, but for emacsen whose `parse-partial-sexp'
  ;; returns the pos of the comment start.
  (save-excursion
    (let* ((pos (point))
	   (lim (or lim (c-point 'bod)))
	   (state (parse-partial-sexp lim (point))))
      (cond ((nth 3 state)		; String.
	     (goto-char (nth 8 state))
	     (cons (point) (or (c-safe (c-forward-sexp 1) (point))
			       (point-max))))
	    ((nth 4 state)		; Comment.
	     (goto-char (nth 8 state))
	     (cons (point) (progn (c-forward-comment 1) (point))))
	    ((and (not not-in-delimiter)
		  (not (nth 5 state))
		  (eq (char-before) ?/)
		  (looking-at "[/*]"))
	     ;; We're standing in a comment starter.
	     (backward-char 1)
	     (cons (point) (progn (c-forward-comment 1) (point))))
	    (near
	     (goto-char pos)
	     ;; Search forward for a literal.
	     (skip-chars-forward " \t")
	     (cond
	      ((eq (char-syntax (or (char-after) ?\ )) ?\") ; String.
	       (cons (point) (or (c-safe (c-forward-sexp 1) (point))
				 (point-max))))
	      ((looking-at "/[/*]")	; Line or block comment.
	       (cons (point) (progn (c-forward-comment 1) (point))))
	      (t
	       ;; Search backward.
	       (skip-chars-backward " \t")
	       (let ((end (point)) beg)
		 (cond
		  ((eq (char-syntax (or (char-before) ?\ )) ?\") ; String.
		   (setq beg (c-safe (c-backward-sexp 1) (point))))
		  ((and (c-safe (forward-char -2) t)
			(looking-at "*/"))
		   ;; Block comment.  Due to the nature of line
		   ;; comments, they will always be covered by the
		   ;; normal case above.
		   (goto-char end)
		   (c-forward-comment -1)
		   ;; If LIM is bogus, beg will be bogus.
		   (setq beg (point))))
		 (if beg (cons beg end))))))
	    ))))

(if (c-safe (> (length (save-excursion (parse-partial-sexp 1 1))) 8))
    (defalias 'c-literal-limits 'c-literal-limits-fast))

(defun c-collect-line-comments (range)
  "If the argument is a cons of two buffer positions (such as returned by
`c-literal-limits'), and that range contains a C++ style line comment,
then an extended range is returned that contains all adjacent line
comments (i.e. all comments that starts in the same column with no
empty lines or non-whitespace characters between them).  Otherwise the
argument is returned."
  (save-excursion
    (condition-case nil
	(if (and (consp range) (progn
				 (goto-char (car range))
				 (looking-at "//")))
	    (let ((col (current-column))
		  (beg (point))
		  (bopl (c-point 'bopl))
		  (end (cdr range)))
	      ;; Got to take care in the backward direction to handle
	      ;; comments which are preceded by code.
	      (while (and (c-forward-comment -1)
			  (>= (point) bopl)
			  (looking-at "//")
			  (= col (current-column)))
		(setq beg (point)
		      bopl (c-point 'bopl)))
	      (goto-char end)
	      (while (and (progn (skip-chars-forward " \t")
				 (looking-at "//"))
			  (= col (current-column))
			  (prog1 (zerop (forward-line 1))
			    (setq end (point)))))
	      (cons beg end))
	  range)
      (error range))))

(defun c-literal-type (range)
  "Convenience function that given the result of `c-literal-limits',
returns nil or the type of literal that the range surrounds.  It's
much faster than using `c-in-literal' and is intended to be used when
you need both the type of a literal and its limits."
  (if (consp range)
      (save-excursion
	(goto-char (car range))
	(cond ((eq (char-syntax (or (char-after) ?\ )) ?\") 'string)
	      ((looking-at "//") 'c++)
	      (t 'c)))			; Assuming the range is valid.
    range))



;; utilities for moving and querying around syntactic elements

(defvar c-state-cache nil)
(make-variable-buffer-local 'c-state-cache)
;; The state cache used by `c-parse-state' to cut down the amount of
;; searching.  It's the result from some earlier `c-parse-state' call.
;; The use of the cached info is more effective if the next
;; `c-parse-state' call is on a line close by the one the cached state
;; was made at; the cache can actually slow down a little if the
;; cached state was made very far back in the buffer.  The cache is
;; most effective if `c-parse-state' is used on each line while moving
;; forward.

(defvar c-state-cache-start nil)
;; This (point-min) when `c-state-cache' was calculated, to detect
;; that the start point hasn't changed due to narrowing.

(defun c-parse-state ()
  ;; Finds and records all noteworthy parens between some good point
  ;; earlier in the file and point.  That good point is at least the
  ;; beginning of the top-level construct we are in, or the beginning
  ;; of the preceding top-level construct if we aren't in one.
  ;;
  ;; The returned value is a list of the noteworthy parens with the
  ;; last one first.  If an element in the list is an integer, it's
  ;; the position of an open paren which has not been closed before
  ;; point.  If an element is a cons, it gives the position of a
  ;; closed brace paren pair; the car is the start paren position and
  ;; the cdr is the position following the closing paren.  Only the
  ;; last closed brace paren pair before each open paren is recorded,
  ;; and thus the state never contains two cons elements in
  ;; succession.
  (save-restriction
    (let* ((here (point))
	   (c-macro-start (c-query-macro-start))
	   (in-macro-start (or c-macro-start (point)))
	   old-state last-pos pairs pos)
      ;; Somewhat ugly use of c-check-state-cache to get rid of the
      ;; part of the state cache that is after point.  Can't use
      ;; c-whack-state-after for the same reasons as in that function.
      (c-check-state-cache (point) nil nil)
      ;; Get the latest position we know are directly inside the
      ;; closest containing paren of the cached state.
      (setq last-pos (and c-state-cache
			  (if (consp (car c-state-cache))
			      (cdr (car c-state-cache))
			    (1+ (car c-state-cache)))))
      ;; Check if the found last-pos is in a macro.  If it is, and
      ;; we're not in the same macro, we must discard everything on
      ;; c-state-cache that is inside the macro before using it.
      (when last-pos
	(save-excursion
	  (goto-char last-pos)
	  (when (and (c-beginning-of-macro)
		     (/= (point) in-macro-start))
	    (c-check-state-cache (point) nil nil)
	    ;; Set last-pos again, just like above.
	    (setq last-pos (and c-state-cache
				(if (consp (car c-state-cache))
				    (cdr (car c-state-cache))
				  (1+ (car c-state-cache))))))))
      (setq pos
	    ;; Find the start position for the forward search.  (Can't
	    ;; search in the backward direction since point might be
	    ;; in some kind of literal.)
	    (or (when last-pos
		  ;; There's a cached state with a containing paren.  Pop
		  ;; off the stale containing sexps from it by going
		  ;; forward out of parens as far as possible.
		  (narrow-to-region (point-min) here)
		  (let (placeholder pair-beg)
		    (while (and c-state-cache
				(setq placeholder
				      (c-up-list-forward last-pos)))
		      (setq last-pos placeholder)
		      (if (consp (car c-state-cache))
			  (setq pair-beg (car-safe (cdr c-state-cache))
				c-state-cache (cdr-safe (cdr c-state-cache)))
			(setq pair-beg (car c-state-cache)
			      c-state-cache (cdr c-state-cache))))
		    (when (and pair-beg (eq (char-after pair-beg) ?{))
		      ;; The last paren pair we moved out from was a brace
		      ;; pair.  Modify the state to record this as a closed
		      ;; pair now.
		      (if (consp (car-safe c-state-cache))
			  (setq c-state-cache (cdr c-state-cache)))
		      (setq c-state-cache (cons (cons pair-beg last-pos)
						c-state-cache))))
		  ;; Check if the preceding balanced paren is within a
		  ;; macro; it should be ignored if we're outside the
		  ;; macro.  There's no need to check any further upwards;
		  ;; if the macro contains an unbalanced opening paren then
		  ;; we're smoked anyway.
		  (when (and (<= (point) in-macro-start)
			     (consp (car c-state-cache)))
		    (save-excursion
		      (goto-char (car (car c-state-cache)))
		      (when (c-beginning-of-macro)
			(setq here (point)
			      c-state-cache (cdr c-state-cache)))))
		  (when c-state-cache
		    (setq old-state c-state-cache)
		    last-pos))
		(save-excursion
		  ;; go back 2 bods, but ignore any bogus positions
		  ;; returned by beginning-of-defun (i.e. open paren in
		  ;; column zero)
		  (goto-char here)
		  (let ((cnt 2))
		    (while (not (or (bobp) (zerop cnt)))
		      (c-beginning-of-defun-1)
		      (if (eq (char-after) ?\{)
			  (setq cnt (1- cnt)))))
		  (point))))
      (narrow-to-region (point-min) here)
      (while pos
	;; Find the balanced brace pairs.
	(setq pairs nil)
	(while (and (setq last-pos (c-down-list-forward pos))
		    (setq pos (c-up-list-forward last-pos)))
	  (if (eq (char-before last-pos) ?{)
	      (setq pairs (cons (cons last-pos pos) pairs))))
	;; Should ignore any pairs that are in a macro, providing
	;; we're not in the same one.
	(when (and pairs (< (car (car pairs)) in-macro-start))
	  (while (and (save-excursion
			(goto-char (car (car pairs)))
			(c-beginning-of-macro))
		      (setq pairs (cdr pairs)))))
	;; Record the last brace pair.
	(when pairs
	  (if (and (eq c-state-cache old-state)
		   (consp (car-safe c-state-cache)))
	      ;; There's a closed pair on the cached state but we've
	      ;; found a later one, so remove it.
	      (setq c-state-cache (cdr c-state-cache)))
	  (setq pairs (car pairs))
	  (setcar pairs (1- (car pairs)))
	  (setq c-state-cache (cons pairs c-state-cache)))
	(if last-pos
	    ;; Prepare to loop, but record the open paren only if it's
	    ;; outside a macro or within the same macro as point.
	    (progn
	      (setq pos last-pos)
	      (if (or (>= last-pos in-macro-start)
		      (save-excursion
			(goto-char last-pos)
			(not (c-beginning-of-macro))))
		  (setq c-state-cache (cons (1- pos) c-state-cache))))
	  (if (setq last-pos (c-up-list-forward pos))
	      ;; Found a close paren without a corresponding opening
	      ;; one.  Maybe we didn't go back far enough, so try to
	      ;; scan backward for the start paren and then start over.
	      (progn
		(setq pos (c-up-list-backward pos)
		      c-state-cache nil)
		(unless pos
		  (setq pos last-pos
			c-parsing-error
			(format "Unbalanced close paren at line %d"
				(1+ (count-lines (point-min)
						 (c-point 'bol last-pos)))))))
	    (setq pos nil))))
      c-state-cache)))

;; Debug tool to catch cache inconsistencies.
(defvar c-debug-parse-state nil)
(unless (fboundp 'c-real-parse-state)
  (fset 'c-real-parse-state (symbol-function 'c-parse-state)))
(cc-bytecomp-defun c-real-parse-state)
(defun c-debug-parse-state ()
  (let ((res1 (c-real-parse-state)) res2)
    (let ((c-state-cache nil))
      (setq res2 (c-real-parse-state)))
    (unless (equal res1 res2)
      (error "c-parse-state inconsistency: using cache: %s, from scratch: %s"
	     res1 res2))
    res1))
(defun c-toggle-parse-state-debug (&optional arg)
  (interactive "P")
  (setq c-debug-parse-state (c-calculate-state arg c-debug-parse-state))
  (fset 'c-parse-state (symbol-function (if c-debug-parse-state
					    'c-debug-parse-state
					  'c-real-parse-state)))
  (c-keep-region-active))

(defun c-check-state-cache (beg end old-length)
  ;; Used on `after-change-functions' to adjust `c-state-cache'.
  ;; Prefer speed to finesse here, since there will be many more calls
  ;; to this function than times `c-state-cache' is used.
  ;;
  ;; This is much like `c-whack-state-after', but it never changes a
  ;; paren pair element into an open paren element.  Doing that would
  ;; mean that the new open paren wouldn't have the required preceding
  ;; paren pair element.
  (if (not (eq c-state-cache-start (point-min)))
      (setq c-state-cache-start (point-min)
	    c-state-cache nil)
    (while (and c-state-cache
		(let ((elem (car c-state-cache)))
		  (if (consp elem)
		      (or (<= beg (car elem))
			  (< beg (cdr elem)))
		    (<= beg elem))))
      (setq c-state-cache (cdr c-state-cache)))))

(defun c-whack-state-before (bufpos paren-state)
  ;; Whack off any state information from PAREN-STATE which lies
  ;; before BUFPOS.  Not destructive on PAREN-STATE.
  (let* ((newstate (list nil))
	 (ptr newstate)
	 car)
    (while paren-state
      (setq car (car paren-state)
	    paren-state (cdr paren-state))
      (if (< (if (consp car) (car car) car) bufpos)
	  (setq paren-state nil)
	(setcdr ptr (list car))
	(setq ptr (cdr ptr))))
    (cdr newstate)))

(defun c-whack-state-after (bufpos paren-state)
  ;; Whack off any state information from PAREN-STATE which lies at or
  ;; after BUFPOS.  Not destructive on PAREN-STATE.
  (catch 'done
    (while paren-state
      (let ((car (car paren-state)))
	(if (consp car)
	    ;; just check the car, because in a balanced brace
	    ;; expression, it must be impossible for the corresponding
	    ;; close brace to be before point, but the open brace to
	    ;; be after.
	    (if (<= bufpos (car car))
		nil			; whack it off
	      (if (< bufpos (cdr car))
		  ;; its possible that the open brace is before
		  ;; bufpos, but the close brace is after.  In that
		  ;; case, convert this to a non-cons element.  The
		  ;; rest of the state is before bufpos, so we're
		  ;; done.
		  (throw 'done (cons (car car) (cdr paren-state)))
		;; we know that both the open and close braces are
		;; before bufpos, so we also know that everything else
		;; on state is before bufpos.
		(throw 'done paren-state)))
	  (if (<= bufpos car)
	      nil			; whack it off
	    ;; it's before bufpos, so everything else should too.
	    (throw 'done paren-state)))
	(setq paren-state (cdr paren-state)))
      nil)))


(defun c-beginning-of-inheritance-list (&optional lim)
  ;; Go to the first non-whitespace after the colon that starts a
  ;; multiple inheritance introduction.  Optional LIM is the farthest
  ;; back we should search.
  (let* ((lim (or lim (c-point 'bod))))
    (c-with-syntax-table c++-template-syntax-table
      (c-backward-token-1 0 t lim)
      (while (and (looking-at "[_a-zA-Z<,]")
		  (= (c-backward-token-1 1 t lim) 0)))
      (skip-chars-forward "^:"))))

(defun c-in-method-def-p ()
  ;; Return nil if we aren't in a method definition, otherwise the
  ;; position of the initial [+-].
  (save-excursion
    (beginning-of-line)
    (and c-opt-method-key
	 (looking-at c-opt-method-key)
	 (point))
    ))

;; Contributed by Kevin Ryde <user42@zip.com.au>.
(defun c-in-gcc-asm-p ()
  ;; Return non-nil if point is within a gcc \"asm\" block.
  ;;
  ;; This should be called with point inside an argument list.
  ;;
  ;; Only one level of enclosing parentheses is considered, so for
  ;; instance `nil' is returned when in a function call within an asm
  ;; operand.

  (and c-opt-asm-stmt-key
       (save-excursion
	 (beginning-of-line)
	 (backward-up-list 1)
	 (c-beginning-of-statement-1 (point-min) nil t)
	 (looking-at c-opt-asm-stmt-key))))

(defun c-at-toplevel-p ()
  "Return a determination as to whether point is at the `top-level'.
Being at the top-level means that point is either outside any
enclosing block (such function definition), or inside a class,
namespace or extern definition, but outside any method blocks.

If point is not at the top-level (e.g. it is inside a method
definition), then nil is returned.  Otherwise, if point is at a
top-level not enclosed within a class definition, t is returned.
Otherwise, a 2-vector is returned where the zeroth element is the
buffer position of the start of the class declaration, and the first
element is the buffer position of the enclosing class's opening
brace."
  (let ((paren-state (c-parse-state)))
    (or (not (c-most-enclosing-brace paren-state))
	(c-search-uplist-for-classkey paren-state))))

(defun c-forward-to-cpp-define-body ()
  ;; Assuming point is at the "#" that introduces a preprocessor
  ;; directive, it's moved forward to the start of the definition body
  ;; if it's a "#define".  Non-nil is returned in this case, in all
  ;; other cases nil is returned and point isn't moved.
  (when (and (looking-at
	      (concat "#[ \t]*"
		      "define[ \t]+\\(\\sw\\|_\\)+\\(\([^\)]*\)\\)?"
		      "\\([ \t]\\|\\\\\n\\)*"))
	     (not (= (match-end 0) (c-point 'eol))))
    (goto-char (match-end 0))))

(defun c-just-after-func-arglist-p (&optional containing lim)
  ;; Return t if we are between a function's argument list closing
  ;; paren and its opening brace.  Note that the list close brace
  ;; could be followed by a "const" specifier or a member init hanging
  ;; colon.  Optional CONTAINING is position of containing s-exp open
  ;; brace.  If not supplied, point is used as search start.  LIM is
  ;; used as bound for some backward buffer searches; the search might
  ;; continue past it.
  ;;
  ;; Note: This test is easily fooled.  It only works reasonably well
  ;; in the situations where `c-guess-basic-syntax' uses it.
  (save-excursion
    (c-backward-syntactic-ws lim)
    (let ((checkpoint (or containing (point))))
      (goto-char checkpoint)
      ;; could be looking at const specifier
      (if (and (eq (char-before) ?t)
	       (forward-word -1)
	       (looking-at "\\<const\\>[^_]"))
	  (c-backward-syntactic-ws lim)
	;; otherwise, we could be looking at a hanging member init
	;; colon
	(goto-char checkpoint)
	(while (eq (char-before) ?,)
	  ;; this will catch member inits with multiple
	  ;; line arglists
	  (forward-char -1)
	  (c-backward-syntactic-ws (c-point 'bol))
	  (if (eq (char-before) ?\))
	      (c-backward-sexp 2)
	    (c-backward-sexp 1))
	  (c-backward-syntactic-ws lim))
	(if (and (eq (char-before) ?:)
		 (progn
		   (forward-char -1)
		   (c-backward-syntactic-ws lim)
		   (looking-at "\\([ \t\n]\\|\\\\\n\\)*:\\([^:]+\\|$\\)")))
	    nil
	  (goto-char checkpoint))
	)
      (setq checkpoint (point))
      (and (eq (char-before) ?\))
	   ;; Check that it isn't a cpp expression, e.g. the
	   ;; expression of an #if directive or the "function header"
	   ;; of a #define.
	   (or (not (c-beginning-of-macro))
	       (and (c-forward-to-cpp-define-body)
		    (< (point) checkpoint)))
	   ;; check if we are looking at an ObjC method def
	   (or (not c-opt-method-key)
	       (progn
		 (goto-char checkpoint)
		 (c-forward-sexp -1)
		 (forward-char -1)
		 (c-backward-syntactic-ws lim)
		 (not (or (memq (char-before) '(?- ?+))
			  ;; or a class category
			  (progn
			    (c-forward-sexp -2)
			    (looking-at c-class-key))
			  )))))
      )))

(defun c-in-knr-argdecl (&optional lim)
  ;; Return the position of the first argument declaration if point is
  ;; inside a K&R style argument declaration list, nil otherwise.
  ;; `c-recognize-knr-p' is not checked.  If LIM is non-nil, it's a
  ;; position that bounds the backward search for the argument list.
  ;;
  ;; Note: A declaration level context is assumed; the test can return
  ;; false positives for statements and #define headers.  This test is
  ;; even more easily fooled than `c-just-after-func-arglist-p'.
  (save-excursion
    (save-restriction
      ;; Go back to the closest preceding normal parenthesis sexp.  We
      ;; take that as the argument list in the function header.  Then
      ;; check that it's followed by some symbol before the next ';'
      ;; or '{'.  If it does, it's the header of the K&R argdecl we're
      ;; in.
      (if lim (narrow-to-region lim (point)))
      (let (paren-end)
	(and (c-safe (setq paren-end (c-down-list-backward (point))))
	     (eq (char-after paren-end) ?\))
	     (progn
	       (goto-char (1+ paren-end))
	       (c-forward-syntactic-ws)
	       (looking-at "\\w\\|\\s_"))
	     (c-safe (c-up-list-backward paren-end))
	     (point))))))

(defun c-skip-conditional ()
  ;; skip forward over conditional at point, including any predicate
  ;; statements in parentheses. No error checking is performed.
  (c-forward-sexp (cond
		   ;; else if()
		   ((looking-at (concat "\\<else"
					"\\([ \t\n]\\|\\\\\n\\)+"
					"if\\>\\([^_]\\|$\\)"))
		    3)
		   ;; do, else, try, finally
		   ((looking-at (concat "\\<\\("
					"do\\|else\\|try\\|finally"
					"\\)\\>\\([^_]\\|$\\)"))
		    1)
		   ;; for, if, while, switch, catch, synchronized, foreach
		   (t 2))))

(defun c-after-conditional (&optional lim)
  ;; If looking at the token after a conditional then return the
  ;; position of its start, otherwise return nil.
  (save-excursion
    (and (= (c-backward-token-1 1 t lim) 0)
	 (or (looking-at c-block-stmt-1-key)
	     (and (eq (char-after) ?\()
		  (= (c-backward-token-1 1 t lim) 0)
		  (looking-at c-block-stmt-2-key)))
	 (point))))

(defsubst c-backward-to-block-anchor (&optional lim)
  ;; Assuming point is at a brace that opens a statement block of some
  ;; kind, move to the proper anchor point for that block.  It might
  ;; need to be adjusted further by c-add-stmt-syntax, but the
  ;; position at return is suitable as start position for that
  ;; function.
  (unless (= (point) (c-point 'boi))
    (let ((start (c-after-conditional lim)))
      (if start
	  (goto-char start)))))

(defun c-backward-to-decl-anchor (&optional lim)
  ;; Assuming point is at a brace that opens the block of a top level
  ;; declaration of some kind, move to the proper anchor point for
  ;; that block.
  (unless (= (point) (c-point 'boi))
    ;; What we have below is actually an extremely stripped variant of
    ;; c-beginning-of-statement-1.
    (let ((pos (point)))
      ;; Switch syntax table to avoid stopping at line continuations.
      (save-restriction
	(if lim (narrow-to-region lim (point-max)))
	(while (and (progn
		      (c-backward-syntactic-ws)
		      (c-safe (goto-char (scan-sexps (point) -1)) t))
		    (not (c-crosses-statement-barrier-p (point) pos)))
	  (setq pos (point)))
	(goto-char pos)))))

(defsubst c-search-decl-header-end ()
  ;; Search forward for the end of the "header" of the current
  ;; declaration.  That's the position where the definition body
  ;; starts, or the first variable initializer, or the ending
  ;; semicolon.  I.e. search forward for the closest following
  ;; (syntactically relevant) '{', '=' or ';' token.  Point is left
  ;; _after_ the first found token, or at point-max if none is found.
  (c-with-syntax-table (if (c-major-mode-is 'c++-mode)
			   c++-template-syntax-table
			 (syntax-table))
    (while (and (c-syntactic-re-search-forward "[;{=]" nil 'move 1 t)
		;; In Pike it can be an operator identifier containing
		;; '='.
		(c-major-mode-is 'pike-mode)
		(eq (char-before) ?=)
		(c-on-identifier)))))

(defun c-beginning-of-decl-1 (&optional lim)
  ;; Go to the beginning of the current declaration, or the beginning
  ;; of the previous one if already at the start of it.  Point won't
  ;; be moved out of any surrounding paren.  Return a cons cell on the
  ;; form (MOVE . KNR-POS).  MOVE is like the return value from
  ;; `c-beginning-of-statement-1'.  If point skipped over some K&R
  ;; style argument declarations (and they are to be recognized) then
  ;; KNR-POS is set to the start of the first such argument
  ;; declaration, otherwise KNR-POS is nil.  If LIM is non-nil, it's a
  ;; position that bounds the backward search.
  ;;
  ;; NB: Cases where the declaration continues after the block, as in
  ;; "struct foo { ... } bar;", are currently recognized as two
  ;; declarations, e.g. "struct foo { ... }" and "bar;" in this case.
  (catch 'return
    (let* ((start (point))
	 (last-stmt-start (point))
	 (move (c-beginning-of-statement-1 lim t t)))

    (while (and (/= last-stmt-start (point))
		(save-excursion
		  (c-backward-syntactic-ws lim)
		  (not (memq (char-before) '(?\; ?} ?: nil)))))
      ;; `c-beginning-of-statement-1' stops at a block start, but we
      ;; want to continue if the block doesn't begin a top level
      ;; construct, i.e. if it isn't preceded by ';', '}', ':', or bob.
      (setq last-stmt-start (point)
	    move (c-beginning-of-statement-1 lim t t)))

    (when c-recognize-knr-p
      (let ((fallback-pos (point)) knr-argdecl-start)
	;; Handle K&R argdecls.  Back up after the "statement" jumped
	;; over by `c-beginning-of-statement-1', unless it was the
	;; function body, in which case we're sitting on the opening
	;; brace now.  Then test if we're in a K&R argdecl region and
	;; that we started at the other side of the first argdecl in
	;; it.
	(unless (eq (char-after) ?{)
	  (goto-char last-stmt-start))
	(if (and (setq knr-argdecl-start (c-in-knr-argdecl lim))
		 (< knr-argdecl-start start)
		 (progn
		   (goto-char knr-argdecl-start)
		   (not (eq (c-beginning-of-statement-1 lim t t) 'macro))))
	    (throw 'return
		   (cons (if (eq (char-after fallback-pos) ?{)
			     'previous
			   'same)
			 knr-argdecl-start))
	  (goto-char fallback-pos))))

    (when c-opt-access-key
      ;; Might have ended up before a protection label.  This should
      ;; perhaps be checked before `c-recognize-knr-p' to be really
      ;; accurate, but we know that no language has both.
      (while (looking-at c-opt-access-key)
	(goto-char (match-end 0))
	(c-forward-syntactic-ws)
	(when (>= (point) start)
	  (goto-char start)
	  (throw 'return (cons 'same nil)))))

    ;; `c-beginning-of-statement-1' counts each brace block as a
    ;; separate statement, so the result will be 'previous if we've
    ;; moved over any.  If they were brace list initializers we might
    ;; not have moved over a declaration boundary though, so change it
    ;; to 'same if we've moved past a '=' before '{', but not ';'.
    ;; (This ought to be integrated into `c-beginning-of-statement-1',
    ;; so we avoid this extra pass which potentially can search over a
    ;; large amount of text.)
    (if (and (eq move 'previous)
	     (c-with-syntax-table (if (c-major-mode-is 'c++-mode)
				      c++-template-syntax-table
				    (syntax-table))
	       (save-excursion
		 (and (c-syntactic-re-search-forward "[;={]" start t 1 t)
		      (eq (char-before) ?=)
		      (c-syntactic-re-search-forward "[;{]" start t 1 t)
		      (eq (char-before) ?{)
		      (c-safe (goto-char (c-up-list-forward (point))) t)
		      (not (c-syntactic-re-search-forward ";" start t 1 t))))))
	(cons 'same nil)
      (cons move nil)))))

(defun c-end-of-decl-1 ()
  ;; Assuming point is at the start of a declaration (as detected by
  ;; e.g. `c-beginning-of-decl-1'), go to the end of it.  Unlike
  ;; `c-beginning-of-decl-1', this function handles the case when a
  ;; block is followed by identifiers in e.g. struct declarations in C
  ;; or C++.  If a proper end was found then t is returned, otherwise
  ;; point is moved as far as possible within the current sexp and nil
  ;; is returned.  This function doesn't handle macros; use
  ;; `c-end-of-macro' instead in those cases.
  (let ((start (point))
	(decl-syntax-table (if (c-major-mode-is 'c++-mode)
			       c++-template-syntax-table
			     (syntax-table))))
    (catch 'return
      (c-search-decl-header-end)

      (when (and c-recognize-knr-p
		 (eq (char-before) ?\;)
		 (c-in-knr-argdecl start))
	;; Stopped at the ';' in a K&R argdecl section which is
	;; detected using the same criteria as in
	;; `c-beginning-of-decl-1'.  Move to the following block
	;; start.
	(c-syntactic-re-search-forward "{" nil 'move 1 t))

      (when (eq (char-before) ?{)
	;; Encountered a block in the declaration.  Jump over it.
	(condition-case nil
	    (goto-char (c-up-list-forward (point)))
	  (goto-char (point-max))
	  (throw 'return nil))
	(if (or (not c-opt-block-decls-with-vars-key)
		(save-excursion
		  (c-with-syntax-table decl-syntax-table
		    (let ((lim (point)))
		      (goto-char start)
		      (not (and
			    ;; Check for `c-opt-block-decls-with-vars-key'
			    ;; before the first paren.
			    (c-syntactic-re-search-forward
			     (concat "[;=\(\[{]\\|\\<\\("
				     c-opt-block-decls-with-vars-key
				     "\\)")
			     lim t 1 t)
			    (match-beginning 1)
			    (not (eq (char-before) ?_))
			    ;; Check that the first following paren is the block.
			    (c-syntactic-re-search-forward "[;=\(\[{]" lim t 1 t)
			    (eq (char-before) ?{)))))))
	    ;; The declaration doesn't have any of the
	    ;; `c-opt-block-decls-with-vars' keywords in the
	    ;; beginning, so it ends here at the end of the block.
	    (throw 'return t)))

      (c-with-syntax-table decl-syntax-table
	(while (progn
		 (if (eq (char-before) ?\;)
		     (throw 'return t))
		 (c-syntactic-re-search-forward ";" nil 'move 1 t))))
      nil)))

(defun c-beginning-of-member-init-list (&optional limit)
  ;; Goes to the beginning of a member init list (i.e. just after the
  ;; ':') if inside one. Returns t in that case, nil otherwise.
  (or limit
      (setq limit (point-min)))
  (skip-chars-forward " \t")
  (if (eq (char-after) ?,)
      (forward-char 1)
    (c-backward-syntactic-ws limit))
  (while (and (< limit (point))
	      (eq (char-before) ?,))
    ;; this will catch member inits with multiple
    ;; line arglists
    (forward-char -1)
    (c-backward-syntactic-ws limit)
    (if (eq (char-before) ?\))
	(c-backward-sexp 1))
    (c-backward-syntactic-ws limit)
    ;; Skip over any template arg to the class.
    (if (eq (char-before) ?>)
	(c-with-syntax-table c++-template-syntax-table
	  (c-backward-sexp 1)))
    (c-backward-sexp 1)
    (c-backward-syntactic-ws limit)
    ;; Skip backwards over a fully::qualified::name.
    (while (and (eq (char-before) ?:)
		(save-excursion
		  (forward-char -1)
		  (eq (char-before) ?:)))
      (backward-char 2)
      (c-backward-sexp 1))
    ;; now continue checking
    (c-backward-syntactic-ws limit))
  (and (< limit (point))
       (eq (char-before) ?:)))

(defun c-search-uplist-for-classkey (paren-state)
  ;; search for the containing class, returning a 2 element vector if
  ;; found. aref 0 contains the bufpos of the boi of the class key
  ;; line, and aref 1 contains the bufpos of the open brace.
  (if (null paren-state)
      ;; no paren-state means we cannot be inside a class
      nil
    (let ((carcache (car paren-state))
	  search-start search-end)
      (if (consp carcache)
	  ;; a cons cell in the first element means that there is some
	  ;; balanced sexp before the current bufpos. this we can
	  ;; ignore. the nth 1 and nth 2 elements define for us the
	  ;; search boundaries
	  (setq search-start (nth 2 paren-state)
		search-end (nth 1 paren-state))
	;; if the car was not a cons cell then nth 0 and nth 1 define
	;; for us the search boundaries
	(setq search-start (nth 1 paren-state)
	      search-end (nth 0 paren-state)))
      ;; if search-end is nil, or if the search-end character isn't an
      ;; open brace, we are definitely not in a class
      (if (or (not search-end)
	      (< search-end (point-min))
	      (not (eq (char-after search-end) ?{)))
	  nil
	;; now, we need to look more closely at search-start.  if
	;; search-start is nil, then our start boundary is really
	;; point-min.
	(if (not search-start)
	    (setq search-start (point-min))
	  ;; if search-start is a cons cell, then we can start
	  ;; searching from the end of the balanced sexp just ahead of
	  ;; us
	  (if (consp search-start)
	      (setq search-start (cdr search-start))))
	;; now we can do a quick regexp search from search-start to
	;; search-end and see if we can find a class key.  watch for
	;; class like strings in literals
	(save-excursion
	  (save-restriction
	    (goto-char search-start)
	    (let (foundp class match-end)
	      (while (and (not foundp)
			  (progn
			    (c-forward-syntactic-ws search-end)
			    (> search-end (point)))
			  (re-search-forward c-decl-block-key search-end t))
		(setq class (match-beginning 0)
		      match-end (match-end 0))
		(goto-char class)
		(if (c-in-literal search-start)
		    (goto-char match-end) ; its in a comment or string, ignore
		  (c-skip-ws-forward)
		  (setq foundp (vector (c-point 'boi) search-end))
		  (cond
		   ;; check for embedded keywords
		   ((let ((char (char-after (1- class))))
		      (and char
			   (memq (char-syntax char) '(?w ?_))))
		    (goto-char match-end)
		    (setq foundp nil))
		   ;; make sure we're really looking at the start of a
		   ;; class definition, and not an ObjC method.
		   ((and c-opt-method-key
			 (re-search-forward c-opt-method-key search-end t)
			 (not (c-in-literal class)))
		    (setq foundp nil))
		   ;; Check if this is an anonymous inner class.
		   ((and c-opt-inexpr-class-key
			 (looking-at c-opt-inexpr-class-key))
		    (while (and (= (c-forward-token-1 1 t) 0)
				(looking-at "(\\|\\w\\|\\s_\\|\\.")))
		    (if (eq (point) search-end)
			;; We're done.  Just trap this case in the cond.
			nil
		      ;; False alarm; all conditions aren't satisfied.
		      (setq foundp nil)))
		   ;; Its impossible to define a regexp for this, and
		   ;; nearly so to do it programmatically.
		   ;;
		   ;; ; picks up forward decls
		   ;; = picks up init lists
		   ;; ) picks up return types
		   ;; > picks up templates, but remember that we can
		   ;;   inherit from templates!
		   ((let ((skipchars "^;=)"))
		      ;; try to see if we found the `class' keyword
		      ;; inside a template arg list
		      (save-excursion
			(skip-chars-backward "^<>" search-start)
			(if (eq (char-before) ?<)
			    (setq skipchars (concat skipchars ">"))))
		      (while (progn
			       (skip-chars-forward skipchars search-end)
			       (c-in-literal class))
			(forward-char))
		      (/= (point) search-end))
		    (setq foundp nil))
		   )))
	      foundp))
	  )))))

(defun c-inside-bracelist-p (containing-sexp paren-state)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren.  BRACE-STATE is the remainder of the state of enclosing
  ;; braces
  ;;
  ;; N.B.: This algorithm can potentially get confused by cpp macros
  ;; places in inconvenient locations.  Its a trade-off we make for
  ;; speed.
  (or
   ;; this will pick up enum lists
   (c-safe
    (save-excursion
      (goto-char containing-sexp)
      (c-forward-sexp -1)
      (let (bracepos)
	(if (and (or (looking-at "enum\\>[^_]")
		     (progn (c-forward-sexp -1)
			    (looking-at "enum\\>[^_]")))
		 (setq bracepos (c-down-list-forward (point)))
		 (not (c-crosses-statement-barrier-p (point)
						     (- bracepos 2))))
	    (point)))))
   ;; this will pick up array/aggregate init lists, even if they are nested.
   (save-excursion
     (let ((class-key
	    ;; Pike can have class definitions anywhere, so we must
	    ;; check for the class key here.
	    (and (c-major-mode-is 'pike-mode)
		 c-decl-block-key))
	   bufpos braceassignp lim next-containing)
       (while (and (not bufpos)
		   containing-sexp)
	   (when paren-state
	     (if (consp (car paren-state))
		 (setq lim (cdr (car paren-state))
		       paren-state (cdr paren-state))
	       (setq lim (car paren-state)))
	     (when paren-state
	       (setq next-containing (car paren-state)
		     paren-state (cdr paren-state))))
	   (goto-char containing-sexp)
	   (if (c-looking-at-inexpr-block next-containing next-containing)
	       ;; We're in an in-expression block of some kind.  Do not
	       ;; check nesting.  We deliberately set the limit to the
	       ;; containing sexp, so that c-looking-at-inexpr-block
	       ;; doesn't check for an identifier before it.
	       (setq containing-sexp nil)
	     ;; see if the open brace is preceded by = or [...] in
	     ;; this statement, but watch out for operator=
	     (setq braceassignp 'dontknow)
	     (c-backward-token-1 1 t lim)
	     ;; Checks to do only on the first sexp before the brace.
	     (when (and (c-major-mode-is 'java-mode)
			(eq (char-after) ?\[))
	       ;; In Java, an initialization brace list may follow
	       ;; directly after "new Foo[]", so check for a "new"
	       ;; earlier.
	       (while (eq braceassignp 'dontknow)
		 (setq braceassignp
		       (cond ((/= (c-backward-token-1 1 t lim) 0) nil)
			     ((looking-at "new\\>[^_]") t)
			     ((looking-at "\\sw\\|\\s_\\|[.[]")
			      ;; Carry on looking if this is an
			      ;; identifier (may contain "." in Java)
			      ;; or another "[]" sexp.
			      'dontknow)
			     (t nil)))))
	     ;; Checks to do on all sexps before the brace, up to the
	     ;; beginning of the statement.
	     (while (eq braceassignp 'dontknow)
	       (cond ((eq (char-after) ?\;)
		      (setq braceassignp nil))
		     ((and class-key
			   (looking-at class-key))
		      (setq braceassignp nil))
		     ((eq (char-after) ?=)
		      ;; We've seen a =, but must check earlier tokens so
		      ;; that it isn't something that should be ignored.
		      (setq braceassignp 'maybe)
		      (while (and (eq braceassignp 'maybe)
				  (zerop (c-backward-token-1 1 t lim)))
			(setq braceassignp
			      (cond
			       ;; Check for operator =
			       ((looking-at "operator\\>[^_]") nil)
			       ;; Check for `<opchar>= in Pike.
			       ((and (c-major-mode-is 'pike-mode)
				     (or (eq (char-after) ?`)
					 ;; Special case for Pikes
					 ;; `[]=, since '[' is not in
					 ;; the punctuation class.
					 (and (eq (char-after) ?\[)
					      (eq (char-before) ?`))))
				nil)
			       ((looking-at "\\s.") 'maybe)
			       ;; make sure we're not in a C++ template
			       ;; argument assignment
			       ((and
				 (c-major-mode-is 'c++-mode)
				 (save-excursion
				   (let ((here (point))
					 (pos< (progn
						 (skip-chars-backward "^<>")
						 (point))))
				     (and (eq (char-before) ?<)
					  (not (c-crosses-statement-barrier-p
						pos< here))
					  (not (c-in-literal))
					  ))))
				nil)
			       (t t))))))
	       (if (and (eq braceassignp 'dontknow)
			(/= (c-backward-token-1 1 t lim) 0))
		   (setq braceassignp nil)))
	     (if (not braceassignp)
		 (if (eq (char-after) ?\;)
		     ;; Brace lists can't contain a semicolon, so we're done.
		     (setq containing-sexp nil)
		   ;; Go up one level.
		   (setq containing-sexp next-containing
			 lim nil
			 next-containing nil))
	       ;; we've hit the beginning of the aggregate list
	       (c-beginning-of-statement-1
		(c-most-enclosing-brace paren-state))
	       (setq bufpos (point))))
	   )
       bufpos))
   ))

(defun c-looking-at-special-brace-list (&optional lim)
  ;; If we're looking at the start of a pike-style list, ie `({ })',
  ;; `([ ])', `(< >)' etc, a cons of a cons of its starting and ending
  ;; positions and its entry in c-special-brace-lists is returned, nil
  ;; otherwise.  The ending position is nil if the list is still open.
  ;; LIM is the limit for forward search.  The point may either be at
  ;; the `(' or at the following paren character.  Tries to check the
  ;; matching closer, but assumes it's correct if no balanced paren is
  ;; found (i.e. the case `({ ... } ... )' is detected as _not_ being
  ;; a special brace list).
  (if c-special-brace-lists
      (condition-case ()
	  (save-excursion
	    (let ((beg (point))
		  end type)
	      (c-forward-syntactic-ws)
	      (if (eq (char-after) ?\()
		  (progn
		    (forward-char 1)
		    (c-forward-syntactic-ws)
		    (setq type (assq (char-after) c-special-brace-lists)))
		(if (setq type (assq (char-after) c-special-brace-lists))
		    (progn
		      (c-backward-syntactic-ws)
		      (forward-char -1)
		      (setq beg (if (eq (char-after) ?\()
				    (point)
				  nil)))))
	      (if (and beg type)
		  (if (and (c-safe (goto-char beg)
				   (c-forward-sexp 1)
				   (setq end (point))
				   (= (char-before) ?\)))
			   (c-safe (goto-char beg)
				   (forward-char 1)
				   (c-forward-sexp 1)
				   ;; Kludges needed to handle inner
				   ;; chars both with and without
				   ;; paren syntax.
				   (or (/= (char-syntax (char-before)) ?\))
				       (= (char-before) (cdr type)))))
		      (if (or (/= (char-syntax (char-before)) ?\))
			      (= (progn
				   (c-forward-syntactic-ws)
				   (point))
				 (1- end)))
			  (cons (cons beg end) type))
		    (cons (list beg) type)))))
	(error nil))))

(defun c-looking-at-bos (&optional lim)
  ;; Return non-nil if between two statements or declarations, assuming
  ;; point is not inside a literal or comment.
  (save-excursion
    (c-backward-syntactic-ws lim)
    (or (bobp)
	;; Return t if at the start inside some parenthesis expression
	;; too, to catch macros that have statements as arguments.
	(memq (char-before) '(?\; ?} ?\())
	(and (eq (char-before) ?{)
	     (not (and c-special-brace-lists
		       (progn (backward-char)
			      (c-looking-at-special-brace-list))))))))

(defun c-looking-at-inexpr-block (lim containing-sexp)
  ;; Returns non-nil if we're looking at the beginning of a block
  ;; inside an expression.  The value returned is actually a cons of
  ;; either 'inlambda, 'inexpr-statement or 'inexpr-class and the
  ;; position of the beginning of the construct.  LIM limits the
  ;; backward search.  CONTAINING-SEXP is the start position of the
  ;; closest containing list.  If it's nil, the containing paren isn't
  ;; used to decide whether we're inside an expression or not.  If
  ;; both LIM and CONTAINING-SEXP is used, LIM needs to be farther
  ;; back.
  (save-excursion
    (let ((res 'maybe) passed-bracket
	  (closest-lim (or containing-sexp lim (point-min)))
	  ;; Look at the character after point only as a last resort
	  ;; when we can't disambiguate.
	  (block-follows (and (eq (char-after) ?{) (point))))
      (while (and (eq res 'maybe)
		  (progn (c-backward-syntactic-ws)
			 (> (point) closest-lim))
		  (not (bobp))
		  (progn (backward-char)
			 (looking-at "[\]\).]\\|\\w\\|\\s_"))
		  (progn (forward-char)
			 (goto-char (scan-sexps (point) -1))))
	(setq res
	      (cond
	       ((and block-follows
		     c-opt-inexpr-class-key
		     (looking-at c-opt-inexpr-class-key))
		(and (not passed-bracket)
		     (or (not (looking-at c-class-key))
			 ;; If the class definition is at the start of
			 ;; a statement, we don't consider it an
			 ;; in-expression class.
			 (let ((prev (point)))
			   (while (and
				   (= (c-backward-token-1 1 nil closest-lim) 0)
				   (eq (char-syntax (char-after)) ?w))
			     (setq prev (point)))
			   (goto-char prev)
			   (not (c-looking-at-bos)))
			 ;; Also, in Pike we treat it as an
			 ;; in-expression class if it's used in an
			 ;; object clone expression.
			 (save-excursion
			   (and (c-major-mode-is 'pike-mode)
				(progn (goto-char block-follows)
				       (= (c-forward-token-1 1 t) 0))
				(eq (char-after) ?\())))
		     (cons 'inexpr-class (point))))
	       ((and c-opt-inexpr-block-key
		     (looking-at c-opt-inexpr-block-key))
		(cons 'inexpr-statement (point)))
	       ((and c-opt-lambda-key
		     (looking-at c-opt-lambda-key))
		(cons 'inlambda (point)))
	       ((and c-opt-block-stmt-key
		     (looking-at c-opt-block-stmt-key))
		nil)
	       (t
		(if (eq (char-after) ?\[)
		    (setq passed-bracket t))
		'maybe))))
      (if (eq res 'maybe)
	  (when (and block-follows
		     containing-sexp
		     (eq (char-after containing-sexp) ?\())
	    (goto-char containing-sexp)
	    (if (or (save-excursion
		      (c-backward-syntactic-ws lim)
		      (and (> (point) (or lim (point-min)))
			   (c-on-identifier)))
		    (and c-special-brace-lists
			 (c-looking-at-special-brace-list)))
		nil
	      (cons 'inexpr-statement (point))))
	res))))

(defun c-looking-at-inexpr-block-backward (paren-state)
  ;; Returns non-nil if we're looking at the end of an in-expression
  ;; block, otherwise the same as `c-looking-at-inexpr-block'.
  ;; PAREN-STATE is the paren state relevant at the current position.
  (save-excursion
    ;; We currently only recognize a block.
    (let ((here (point))
	  (elem (car-safe paren-state))
	  containing-sexp)
      (when (and (consp elem)
		 (progn (goto-char (cdr elem))
			(c-forward-syntactic-ws here)
			(= (point) here)))
	(goto-char (car elem))
	(if (setq paren-state (cdr paren-state))
	    (setq containing-sexp (car-safe paren-state)))
	(c-looking-at-inexpr-block (c-safe-position containing-sexp
						    paren-state)
				   containing-sexp)))))

(defun c-on-identifier ()
  "Return non-nil if we're on or directly after an identifier.
Keywords are recognized and not considered identifiers."
  (if (or (memq (char-syntax (or (char-after) ? )) '(?w ?_))
	  (memq (char-syntax (or (char-before) ? )) '(?w ?_)))
      (save-excursion
	(skip-syntax-backward "w_")
	(not (looking-at c-keywords-regexp)))
    (if (c-major-mode-is 'pike-mode)
	;; Handle the `<operator> syntax in Pike.
	(save-excursion
	  (if (eq (char-after) ?\`) (forward-char))
	  (skip-chars-backward "!%&*+\\-/<=>^|~")
	  (let ((pos (point)))
	    (cond ((memq (char-before) '(?\) ?\]))
		   (c-safe (backward-char 2)))
		  ((memq (char-before) '(?\( ?\[))
		   (c-safe (backward-char 1))))
	    (if (not (looking-at "()\\|\\[]"))
		(goto-char pos)))
	  (and (eq (char-before) ?\`)
	       (looking-at "[-!%&*+/<=>^|~]\\|()\\|\\[]"))))))


(defun c-most-enclosing-brace (paren-state &optional bufpos)
  ;; Return the bufpos of the innermost enclosing brace before bufpos
  ;; that hasn't been narrowed out, or nil if none was found.
  (let (enclosingp)
    (or bufpos (setq bufpos 134217727))
    (while paren-state
      (setq enclosingp (car paren-state)
	    paren-state (cdr paren-state))
      (if (or (consp enclosingp)
	      (>= enclosingp bufpos))
	  (setq enclosingp nil)
	(if (< enclosingp (point-min))
	    (setq enclosingp nil))
	(setq paren-state nil)))
    enclosingp))

(defun c-least-enclosing-brace (paren-state &optional bufpos)
  ;; Return the bufpos of the outermost enclosing brace before bufpos
  ;; that hasn't been narrowed out, or nil if none was found.
  (let (pos elem)
    (or bufpos (setq bufpos 134217727))
    (while paren-state
      (setq elem (car paren-state)
	    paren-state (cdr paren-state))
      (unless (or (consp elem)
		  (>= elem bufpos))
	(if (>= elem (point-min))
	    (setq pos elem))))
    pos))

(defun c-safe-position (bufpos paren-state)
  ;; Return the closest known safe position higher up than BUFPOS, or
  ;; nil if PAREN-STATE doesn't contain one.  Return nil if BUFPOS is
  ;; nil, which is useful to find the closest limit before a given
  ;; limit that might be nil.
  (when bufpos
    (let ((c-macro-start (c-query-macro-start)) safepos)
      (if (and c-macro-start
	       (< c-macro-start bufpos))
	  ;; Make sure bufpos is outside the macro we might be in.
	  (setq bufpos c-macro-start))
      (catch 'done
	(while paren-state
	  (setq safepos
		(if (consp (car paren-state))
		    (cdr (car paren-state))
		  (car paren-state)))
	  (if (< safepos bufpos)
	      (throw 'done safepos)
	    (setq paren-state (cdr paren-state))))
	(if (eq c-macro-start bufpos)
	    ;; Backed up bufpos to the macro start and got outside the
	    ;; state.  We know the macro is at the top level in this case,
	    ;; so we can use the macro start as the safe position.
	    c-macro-start)))))

(defun c-narrow-out-enclosing-class (paren-state lim)
  ;; Narrow the buffer so that the enclosing class is hidden.  Uses
  ;; and returns the value from c-search-uplist-for-classkey.
  (setq paren-state (c-whack-state-after (point) paren-state))
  (let (inclass-p)
    (and paren-state
	 (setq inclass-p (c-search-uplist-for-classkey paren-state))
	 (narrow-to-region
	  (progn
	    (goto-char (1+ (aref inclass-p 1)))
	    (c-skip-ws-forward lim)
	    ;; if point is now left of the class opening brace, we're
	    ;; hosed, so try a different tact
	    (if (<= (point) (aref inclass-p 1))
		(progn
		  (goto-char (1+ (aref inclass-p 1)))
		  (c-forward-syntactic-ws lim)))
	    (point))
	  ;; end point is the end of the current line
	  (progn
	    (goto-char lim)
	    (c-point 'eol))))
    ;; return the class vector
    inclass-p))


;; c-guess-basic-syntax implements the main decision tree for
;; determining the syntactic analysis of the current line of code.
;; Yes, it's huge and bloated!

;; It's useful to break out some parts of the decision tree to
;; separate functions, which are all collected below.  Use dynamic
;; binding to propagate back the syntax results from them.
(defvar syntax)
(defvar syntactic-relpos)

(defun c-add-stmt-syntax (syntax-symbol
			  stop-at-boi-only
			  containing-sexp
			  paren-state
			  &optional at-block-start)
  ;; Do the generic processing to anchor the given syntax symbol on
  ;; the preceding statement: Skip over any labels and containing
  ;; statements on the same line, and then search backward until we
  ;; find a statement or block start that begins at boi without a
  ;; label or comment.
  ;;
  ;; Point is assumed to be at the prospective anchor point for the
  ;; given SYNTAX-SYMBOL.  More syntax entries are added if we need to
  ;; skip past block opens and containing statement.  All the added
  ;; syntax elements will get the same anchor point.
  ;;
  ;; If STOP-AT-BOI-ONLY is nil, we might stop in the middle of the
  ;; line if another statement precedes the current one on this line.
  ;;
  ;; If AT-BLOCK-START is non-nil, point is taken to be at the
  ;; beginning of a block or brace list, which then might be nested
  ;; inside an expression.  If AT-BLOCK-START is nil, this is found
  ;; out by checking whether the character at point is "{" or not.
  (if (= (point) (c-point 'boi))
      ;; This is by far the most common case, so let's give it special
      ;; treatment.
      (c-add-syntax syntax-symbol (point))

    (let* ((savepos (point))
	   (syms (list syntax-symbol))
	   (syms-tail syms)
	   (boi (c-point 'boi))
	   (prev-paren (if at-block-start ?{ (char-after)))
	   step-type step-tmp at-comment add-inexpr-stmt)

      ;; Begin by skipping any labels and containing statements that
      ;; are on the same line.
      (while (and (/= (point) boi)
		  (if (memq (setq step-tmp
				  (c-beginning-of-statement-1 boi nil t))
			    '(up label))
		      t
		    (goto-char savepos)
		    nil)
		  (/= (point) savepos))
	(setq savepos (point)
	      step-type step-tmp))

      ;; Skip over any comments that stands between the statement and
      ;; boi.  If stop-at-boi-only is nil and we're not at boi after
      ;; this, then we're done.
      (while (and (/= (setq savepos (point)) boi)
		  (c-forward-comment -1))
	(setq at-comment t
	      boi (c-point 'boi)))
      (goto-char savepos)

      (when (or stop-at-boi-only
		(= (point) boi))
	(catch 'done
	  ;; Loop if we have to back out of the containing block.
	  (while
	    (progn
	      ;; Loop if we have to back up another statement.
	      (while
		  (progn
		    ;; Always start by skipping over any comments that
		    ;; stands between the statement and boi.
		    (while (and (/= (setq savepos (point)) boi)
				(c-forward-comment -1))
		      (setq at-comment t
			    boi (c-point 'boi)))
		    (goto-char savepos)
		    (and (or at-comment
			     (eq step-type 'label)
			     (/= savepos boi))
			 (progn
			   ;; Current position not good enough; skip
			   ;; backward another statement.
			   (setq stop-at-boi-only t
				 step-type (c-beginning-of-statement-1
					    containing-sexp))
			   ;; Record this a substatement if we skipped
			   ;; up one level, but not if we're still on
			   ;; the same line.  This so e.g. a sequence
			   ;; of "else if" clauses won't indent deeper
			   ;; and deeper.
			   (when (and (eq step-type 'up)
				      (< (point) boi))
			     (setcdr syms-tail (list 'substatement))
			     (setq syms-tail (cdr syms-tail)))
			   (setq boi (c-point 'boi))
			   (/= (point) savepos))))
		(setq savepos (point)
		      at-comment nil))
	      (setq at-comment nil)

	      (when (and (eq step-type 'same)
			 containing-sexp)
		(goto-char containing-sexp)
		(setq paren-state (c-whack-state-after containing-sexp
						       paren-state)
		      containing-sexp (c-most-enclosing-brace paren-state))

		(when (and (prog1
			       (eq prev-paren ?{)
			     (setq prev-paren (char-after)))
			   (eq prev-paren ?\())
		  (c-backward-syntactic-ws containing-sexp)
		  (when (c-on-identifier)
		    ;; Arrived at a function arglist start.  Exit with
		    ;; the position of the first argument inside it.
		    (goto-char savepos)
		    (throw 'done t))
		  ;; We're in an in-expression statement.  Remember
		  ;; this.  We'll iterate below, but won't add any
		  ;; syntax element.
		  (setq add-inexpr-stmt t))

		(setq savepos (point)
		      boi (c-point 'boi)
		      step-type (c-beginning-of-statement-1 containing-sexp))

		(let ((at-bod (and (eq step-type 'same)
				   (/= savepos (point))
				   (eq prev-paren ?{))))
		  (when (= savepos boi)
		    ;; If the open brace was at boi, we're always
		    ;; done.  The c-beginning-of-statement-1 call
		    ;; above is necessary anyway, to decide the type
		    ;; of block-intro to add.
		    (goto-char savepos)
		    (setq savepos nil))

		  (when (eq prev-paren ?{)
		    (setcdr syms-tail (list (if at-bod
						'defun-block-intro
					      'statement-block-intro)))
		    (setq syms-tail (cdr syms-tail)))

		  (when (and (not at-bod) savepos)
		    ;; Loop if the brace wasn't at boi, and we didn't
		    ;; arrive at a defun block.
		    (if (eq step-type 'same)
			;; Avoid backing up another sexp if the point
			;; we're at now is found to be good enough in
			;; the loop above.
			(setq step-type nil))
		    (setq stop-at-boi-only t
			  boi (c-point 'boi)))))
	      ))))

      (while syms
	(c-add-syntax (car syms) (point))
	(setq syms (cdr syms)))
      (if add-inexpr-stmt
	  (c-add-syntax 'inexpr-statement))
      )))

(defun c-add-class-syntax (symbol classkey paren-state)
  ;; The inclass and class-close syntactic symbols are added in
  ;; several places and some work is needed to fix everything.
  ;; Therefore it's collected here.
  (save-restriction
    (widen)
    (let (inexpr anchor containing-sexp)
      (goto-char (aref classkey 1))
      (if (and (eq symbol 'inclass) (= (point) (c-point 'boi)))
	  (c-add-syntax symbol (setq anchor (point)))
	(c-add-syntax symbol (setq anchor (aref classkey 0)))
	(if (and c-opt-inexpr-class-key
		 (setq containing-sexp (c-most-enclosing-brace paren-state
							       (point))
		       inexpr (cdr (c-looking-at-inexpr-block
				    (c-safe-position containing-sexp
						     paren-state)
				    containing-sexp)))
		 (/= inexpr (c-point 'boi inexpr)))
	    (c-add-syntax 'inexpr-class)))
      anchor)))

(defun c-guess-continued-construct (indent-point
				    char-after-ip
				    beg-of-same-or-containing-stmt
				    containing-sexp
				    paren-state)
  ;; This function contains the decision tree reached through both
  ;; cases 18 and 10.  It's a continued statement or top level
  ;; construct of some kind.
  (let (special-brace-list)
    (goto-char indent-point)
    (skip-chars-forward " \t")
    (cond
     ;; (CASE A removed.)
     ;; CASE B: open braces for class or brace-lists
     ((setq special-brace-list
	    (or (and c-special-brace-lists
		     (c-looking-at-special-brace-list))
		(eq char-after-ip ?{)))
      (cond
       ;; CASE B.1: class-open
       ((save-excursion
	  (goto-char indent-point)
	  (skip-chars-forward " \t{")
	  (let ((decl (c-search-uplist-for-classkey (c-parse-state))))
	    (and decl
		 (setq beg-of-same-or-containing-stmt (aref decl 0)))
	    ))
	(c-add-syntax 'class-open beg-of-same-or-containing-stmt))
       ;; CASE B.2: brace-list-open
       ((or (consp special-brace-list)
	    (save-excursion
	      (goto-char beg-of-same-or-containing-stmt)
	      (looking-at "enum\\>[^_]"))
	    (save-excursion
	      (goto-char indent-point)
	      (while (and (> (point) beg-of-same-or-containing-stmt)
			  (= (c-backward-token-1 1 t) 0)
			  (/= (char-after) ?=)))
	      (eq (char-after) ?=)))
	;; The most semantically accurate symbol here is
	;; brace-list-open, but we report it simply as a statement-cont.
	;; The reason is that one normally adjusts brace-list-open for
	;; brace lists as top-level constructs, and brace lists inside
	;; statements is a completely different context.
	(c-beginning-of-statement-1 containing-sexp)
	(c-add-stmt-syntax 'statement-cont nil containing-sexp paren-state))
       ;; CASE B.3: The body of a function declared inside a normal
       ;; block.  Can occur e.g. in Pike and when using gcc
       ;; extensions.  Might also trigger it with some macros followed
       ;; by blocks, and this gives sane indentation then too.
       ;; C.f. cases 16F and 17G.
       ((progn
	  (goto-char indent-point)
	  (and (not (c-looking-at-bos))
	       (eq (c-beginning-of-statement-1 containing-sexp nil nil t)
		   'same)))
	(c-add-stmt-syntax 'defun-open t containing-sexp paren-state))
       ;; CASE B.4: Continued statement with block open.
       (t
	(goto-char beg-of-same-or-containing-stmt)
	(c-add-stmt-syntax 'statement-cont nil containing-sexp paren-state)
	(c-add-syntax 'block-open))
       ))
     ;; CASE C: iostream insertion or extraction operator
     ((and (looking-at "<<\\|>>")
	   (save-excursion
	     (goto-char beg-of-same-or-containing-stmt)
	     (while (and (re-search-forward "<<\\|>>" indent-point 'move)
			 (c-in-literal beg-of-same-or-containing-stmt)))
	     ;; if we ended up at indent-point, then the first streamop is on a
	     ;; separate line. Indent the line like a statement-cont instead
	     (when (/= (point) indent-point)
	       (c-add-syntax 'stream-op (c-point 'boi))
	       t))))
     ;; CASE D: continued statement.
     (t
      (c-beginning-of-statement-1 containing-sexp)
      (c-add-stmt-syntax 'statement-cont nil containing-sexp paren-state))
     )))

(defun c-guess-basic-syntax ()
  "Return the syntactic context of the current line."
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((indent-point (point))
	     (case-fold-search nil)
	     (paren-state (c-parse-state))
	     literal containing-sexp char-before-ip char-after-ip lim
	     syntax placeholder c-in-literal-cache step-type
	     tmpsymbol keyword injava-inher special-brace-list
	     ;; narrow out any enclosing class or extern "C" block
	     (inclass-p (c-narrow-out-enclosing-class paren-state
						      indent-point))
	     ;; c-state-cache is shadowed here.  That means we must
	     ;; not do any changes during the execution of this
	     ;; function, since c-check-state-cache then would change
	     ;; this local variable and leave a bogus value in the
	     ;; global one.
	     (c-state-cache (if inclass-p
				(c-whack-state-before (point-min) paren-state)
			      paren-state))
	     (c-state-cache-start (point-min))
	     inenclosing-p macro-start in-macro-expr
	     ;; There's always at most one syntactic element which got
	     ;; a relpos.  It's stored in syntactic-relpos.
	     syntactic-relpos
	     (c-stmt-delim-chars c-stmt-delim-chars))
	;; check for meta top-level enclosing constructs, possible
	;; extern language definitions, possibly (in C++) namespace
	;; definitions.
	(save-excursion
	  (save-restriction
	    (widen)
	    (if (and inclass-p
		     (progn
		       (goto-char (aref inclass-p 0))
		       (looking-at c-other-decl-block-key)))
		(let ((enclosing (match-string 1)))
		  (cond
		   ((string-equal enclosing "extern")
		    (setq inenclosing-p 'extern))
		   ((string-equal enclosing "namespace")
		    (setq inenclosing-p 'namespace))
		   )))))

	;; Init some position variables:
	;;
	;; containing-sexp is the open paren of the closest
	;; surrounding sexp or nil if there is none that hasn't been
	;; narrowed out.
	;;
	;; lim is the position after the closest preceding brace sexp
	;; (nested sexps are ignored), or the position after
	;; containing-sexp if there is none, or (point-min) if
	;; containing-sexp is nil.
	;;
	;; c-state-cache is the state from c-parse-state at
	;; indent-point, without any parens outside the region
	;; narrowed by c-narrow-out-enclosing-class.
	;;
	;; paren-state is the state from c-parse-state outside
	;; containing-sexp, or at indent-point if containing-sexp is
	;; nil.  paren-state is not limited to the narrowed region, as
	;; opposed to c-state-cache.
	(if c-state-cache
	    (progn
	      (setq containing-sexp (car paren-state)
		    paren-state (cdr paren-state))
	      (if (consp containing-sexp)
		  (progn
		    (setq lim (cdr containing-sexp))
		    (if (cdr c-state-cache)
			;; Ignore balanced paren.  The next entry
			;; can't be another one.
			(setq containing-sexp (car (cdr c-state-cache))
			      paren-state (cdr paren-state))
		      ;; If there is no surrounding open paren then
		      ;; put the last balanced pair back on paren-state.
		      (setq paren-state (cons containing-sexp paren-state)
			    containing-sexp nil)))
		(setq lim (1+ containing-sexp))))
	  (setq lim (point-min)))

	;; If we're in a parenthesis list then ',' delimits the
	;; "statements" rather than being an operator (with the
	;; exception of the "for" clause).  This difference is
	;; typically only noticeable when statements are used in macro
	;; arglists.
	(when (and containing-sexp
		   (eq (char-after containing-sexp) ?\())
	  (setq c-stmt-delim-chars c-stmt-delim-chars-with-comma))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(c-backward-syntactic-ws lim)
	(setq char-before-ip (char-before))
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (char-after))

	;; are we in a literal?
	(setq literal (c-in-literal lim))

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 ((eq literal 'string)
	  (c-add-syntax 'string (c-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
	 ((memq literal '(c c++))
	  (c-add-syntax literal (car (c-literal-limits lim))))
	 ;; CASE 3: in a cpp preprocessor macro continuation.
	 ((and (save-excursion
		 (when (c-beginning-of-macro)
		   (setq macro-start (point))))
	       (/= macro-start (c-point 'boi))
	       (progn
		 (setq tmpsymbol 'cpp-macro-cont)
		 (or (not c-syntactic-indentation-in-macros)
		     (save-excursion
		       (goto-char macro-start)
		       ;; If at the beginning of the body of a #define
		       ;; directive then analyze as cpp-define-intro
		       ;; only.  Go on with the syntactic analysis
		       ;; otherwise.  in-macro-expr is set if we're in a
		       ;; cpp expression, i.e. before the #define body
		       ;; or anywhere in a non-#define directive.
		       (if (c-forward-to-cpp-define-body)
			   (let ((indent-boi (c-point 'boi indent-point)))
			     (setq in-macro-expr (> (point) indent-boi)
				   tmpsymbol 'cpp-define-intro)
			     (= (point) indent-boi))
			 (setq in-macro-expr t)
			 nil)))))
	  (c-add-syntax tmpsymbol macro-start)
	  (setq macro-start nil))
	 ;; CASE 11: an else clause?
	 ((looking-at "else\\>[^_]")
	  (c-beginning-of-statement-1 containing-sexp)
	  (c-add-stmt-syntax 'else-clause t containing-sexp paren-state))
	 ;; CASE 12: while closure of a do/while construct?
	 ((and (looking-at "while\\>[^_]")
	       (save-excursion
		 (prog1 (eq (c-beginning-of-statement-1 containing-sexp)
			    'beginning)
		   (setq placeholder (point)))))
	  (goto-char placeholder)
	  (c-add-stmt-syntax 'do-while-closure t containing-sexp paren-state))
	 ;; CASE 13: A catch or finally clause?  This case is simpler
	 ;; than if-else and do-while, because a block is required
	 ;; after every try, catch and finally.
	 ((save-excursion
	    (and (cond ((c-major-mode-is 'c++-mode)
			(looking-at "catch\\>[^_]"))
		       ((c-major-mode-is 'java-mode)
			(looking-at "\\(catch\\|finally\\)\\>[^_]")))
		 (and (c-safe (c-backward-syntactic-ws)
			      (c-backward-sexp)
			      t)
		      (eq (char-after) ?{)
		      (c-safe (c-backward-syntactic-ws)
			      (c-backward-sexp)
			      t)
		      (if (eq (char-after) ?\()
			  (c-safe (c-backward-sexp) t)
			t))
		 (looking-at "\\(try\\|catch\\)\\>[^_]")
		 (setq placeholder (point))))
	  (goto-char placeholder)
	  (c-add-stmt-syntax 'catch-clause t containing-sexp paren-state))
	 ;; CASE 18: A substatement we can recognize by keyword.
	 ((save-excursion
	    (and c-opt-block-stmt-key
		 (not (eq char-before-ip ?\;))
		 (not (memq char-after-ip '(?\) ?\] ?,)))
		 (or (not (eq char-before-ip ?}))
		     (c-looking-at-inexpr-block-backward c-state-cache))
		 (> (point)
		    (progn
		      ;; Ought to cache the result from the
		      ;; c-beginning-of-statement-1 calls here.
		      (setq placeholder (point))
		      (while (eq (setq step-type
				       (c-beginning-of-statement-1 lim))
				 'label))
		      (if (eq step-type 'previous)
			  (goto-char placeholder)
			(setq placeholder (point))
			(if (and (eq step-type 'same)
				 (not (looking-at c-opt-block-stmt-key)))
			    ;; Step up to the containing statement if we
			    ;; stayed in the same one.
			    (let (step)
			      (while (eq
				      (setq step
					    (c-beginning-of-statement-1 lim))
				      'label))
			      (if (eq step 'up)
				  (setq placeholder (point))
				;; There was no containing statement afterall.
				(goto-char placeholder)))))
		      placeholder))
		 (if (looking-at c-block-stmt-2-key)
		     ;; Require a parenthesis after these keywords.
		     ;; Necessary to catch e.g. synchronized in Java,
		     ;; which can be used both as statement and
		     ;; modifier.
		     (and (= (c-forward-token-1 1 nil) 0)
			  (eq (char-after) ?\())
		   (looking-at c-opt-block-stmt-key))))
	  (if (eq step-type 'up)
	      ;; CASE 18A: Simple substatement.
	      (progn
		(goto-char placeholder)
		(cond
		 ((eq char-after-ip ?{)
		  (c-add-stmt-syntax 'substatement-open nil
				     containing-sexp paren-state))
		 ((save-excursion
		    (goto-char indent-point)
		    (back-to-indentation)
		    (looking-at c-label-key))
		  (c-add-stmt-syntax 'substatement-label nil
				     containing-sexp paren-state))
		 (t
		  (c-add-stmt-syntax 'substatement nil
				     containing-sexp paren-state))))
	    ;; CASE 18B: Some other substatement.  This is shared
	    ;; with case 10.
	    (c-guess-continued-construct indent-point
					 char-after-ip
					 placeholder
					 lim
					 paren-state)))
	 ;; CASE 4: In-expression statement.  C.f. cases 7B, 16A and
	 ;; 17E.
	 ((and (or c-opt-inexpr-class-key
		   c-opt-inexpr-block-key
		   c-opt-lambda-key)
	       (setq placeholder (c-looking-at-inexpr-block
				  (c-safe-position containing-sexp paren-state)
				  containing-sexp)))
	  (setq tmpsymbol (assq (car placeholder)
				'((inexpr-class . class-open)
				  (inexpr-statement . block-open))))
	  (if tmpsymbol
	      ;; It's a statement block or an anonymous class.
	      (setq tmpsymbol (cdr tmpsymbol))
	    ;; It's a Pike lambda.  Check whether we are between the
	    ;; lambda keyword and the argument list or at the defun
	    ;; opener.
	    (setq tmpsymbol (if (eq char-after-ip ?{)
				'inline-open
			      'lambda-intro-cont)))
	  (goto-char (cdr placeholder))
	  (back-to-indentation)
	  (c-add-stmt-syntax tmpsymbol t
			     (c-most-enclosing-brace c-state-cache (point))
			     (c-whack-state-after (point) paren-state))
	  (unless (eq (point) (cdr placeholder))
	    (c-add-syntax (car placeholder))))
	 ;; CASE 5: Line is at top level.
	 ((null containing-sexp)
	  (cond
	   ;; CASE 5A: we are looking at a defun, brace list, class,
	   ;; or inline-inclass method opening brace
	   ((setq special-brace-list
		  (or (and c-special-brace-lists
			   (c-looking-at-special-brace-list))
		      (eq char-after-ip ?{)))
	    (cond
	     ;; CASE 5A.1: extern language or namespace construct
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t")
		(and (c-safe (progn (c-backward-sexp 2) t))
		     (looking-at c-other-decl-block-key)
		     (setq keyword (match-string 1)
			   placeholder (point))
		     (or (and (string-equal keyword "namespace")
			      (setq tmpsymbol 'namespace-open))
			 (and (string-equal keyword "extern")
			      (progn
				(c-forward-sexp 1)
				(c-forward-syntactic-ws)
				(eq (char-after) ?\"))
			      (setq tmpsymbol 'extern-lang-open)))
		     ))
	      (goto-char placeholder)
	      (c-add-syntax tmpsymbol (c-point 'boi)))
	     ;; CASE 5A.2: we are looking at a class opening brace
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (and decl
		       (setq placeholder (aref decl 0)))
		  ))
	      (c-add-syntax 'class-open placeholder))
	     ;; CASE 5A.3: brace list open
	     ((save-excursion
		(c-beginning-of-statement-1 lim t)
		(if (looking-at "typedef\\>[^_]")
		    (progn (c-forward-sexp 1)
			   (c-forward-syntactic-ws indent-point)))
		(setq placeholder (c-point 'boi))
		(or (consp special-brace-list)
		    (and (or (save-excursion
			       (goto-char indent-point)
			       (setq tmpsymbol nil)
			       (while (and (> (point) placeholder)
					   (= (c-backward-token-1 1 t) 0)
					   (/= (char-after) ?=))
				 (if (and (not tmpsymbol)
					  (looking-at "new\\>[^_]"))
				     (setq tmpsymbol 'topmost-intro-cont)))
			       (eq (char-after) ?=))
			     (looking-at "enum\\>[^_]"))
			 (save-excursion
			   (while (and (< (point) indent-point)
				       (= (c-forward-token-1 1 t) 0)
				       (not (memq (char-after) '(?\; ?\()))))
			   (not (memq (char-after) '(?\; ?\()))
			   ))))
	      (if (and (c-major-mode-is 'java-mode)
		       (eq tmpsymbol 'topmost-intro-cont))
		  ;; We're in Java and have found that the open brace
		  ;; belongs to a "new Foo[]" initialization list,
		  ;; which means the brace list is part of an
		  ;; expression and not a top level definition.  We
		  ;; therefore treat it as any topmost continuation
		  ;; even though the semantically correct symbol still
		  ;; is brace-list-open, on the same grounds as in
		  ;; case 10B.2.
		  (progn
		    (c-beginning-of-statement-1 lim)
		    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
		(c-add-syntax 'brace-list-open placeholder)))
	     ;; CASE 5A.4: inline defun open
	     ((and inclass-p (not inenclosing-p))
	      (c-add-syntax 'inline-open)
	      (c-add-class-syntax 'inclass inclass-p paren-state))
	     ;; CASE 5A.5: ordinary defun open
	     (t
	      (goto-char placeholder)
	      (if (or inclass-p macro-start)
		  (c-add-syntax 'defun-open (c-point 'boi))
		;; Bogus to use bol here, but it's the legacy.
		(c-add-syntax 'defun-open (c-point 'bol)))
	      )))
	   ;; CASE 5B: first K&R arg decl or member init
	   ((c-just-after-func-arglist-p nil lim)
	    (cond
	     ;; CASE 5B.1: a member init
	     ((or (eq char-before-ip ?:)
		  (eq char-after-ip ?:))
	      ;; this line should be indented relative to the beginning
	      ;; of indentation for the topmost-intro line that contains
	      ;; the prototype's open paren
	      ;; TBD: is the following redundant?
	      (if (eq char-before-ip ?:)
		  (forward-char -1))
	      (c-backward-syntactic-ws lim)
	      ;; TBD: is the preceding redundant?
	      (if (eq (char-before) ?:)
		  (progn (forward-char -1)
			 (c-backward-syntactic-ws lim)))
	      (if (eq (char-before) ?\))
		  (c-backward-sexp 1))
	      (setq placeholder (point))
	      (save-excursion
		(and (c-safe (c-backward-sexp 1) t)
		     (looking-at "throw[^_]")
		     (c-safe (c-backward-sexp 1) t)
		     (setq placeholder (point))))
	      (goto-char placeholder)
	      (c-add-syntax 'member-init-intro (c-point 'boi))
	      ;; we don't need to add any class offset since this
	      ;; should be relative to the ctor's indentation
	      )
	     ;; CASE 5B.2: K&R arg decl intro
	     (c-recognize-knr-p
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
	      (if inclass-p
		  (c-add-class-syntax 'inclass inclass-p paren-state)))
	     ;; CASE 5B.3: Inside a member init list.
	     ((c-beginning-of-member-init-list lim)
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5B.4: Nether region after a C++ or Java func
	     ;; decl, which could include a `throws' declaration.
	     (t
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'func-decl-cont (c-point 'boi))
	      )))
	   ;; CASE 5C: inheritance line. could be first inheritance
	   ;; line, or continuation of a multiple inheritance
	   ((or (and (c-major-mode-is 'c++-mode)
		     (progn
		       (when (eq char-after-ip ?,)
			 (skip-chars-forward " \t")
			 (forward-char))
		       (looking-at c-opt-decl-spec-key)))
		(and (or (eq char-before-ip ?:)
			 ;; watch out for scope operator
			 (save-excursion
			   (and (eq char-after-ip ?:)
				(c-safe (progn (forward-char 1) t))
				(not (eq (char-after) ?:))
				)))
		     (save-excursion
		       (c-backward-syntactic-ws lim)
		       (if (eq char-before-ip ?:)
			   (progn
			     (forward-char -1)
			     (c-backward-syntactic-ws lim)))
		       (back-to-indentation)
		       (looking-at c-class-key)))
		;; for Java
		(and (c-major-mode-is 'java-mode)
		     (let ((fence (save-excursion
				    (c-beginning-of-statement-1 lim)
				    (point)))
			   cont done)
		       (save-excursion
			 (while (not done)
			   (cond ((looking-at c-opt-decl-spec-key)
				  (setq injava-inher (cons cont (point))
					done t))
				 ((or (not (c-safe (c-forward-sexp -1) t))
				      (<= (point) fence))
				  (setq done t))
				 )
			   (setq cont t)))
		       injava-inher)
		     (not (c-crosses-statement-barrier-p (cdr injava-inher)
							 (point)))
		     ))
	    (cond
	     ;; CASE 5C.1: non-hanging colon on an inher intro
	     ((eq char-after-ip ?:)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 5C.2: hanging colon on an inher intro
	     ((eq char-before-ip ?:)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      (if inclass-p
		  (c-add-class-syntax 'inclass inclass-p paren-state)))
	     ;; CASE 5C.3: in a Java implements/extends
	     (injava-inher
	      (let ((where (cdr injava-inher))
		    (cont (car injava-inher)))
		(goto-char where)
		(cond ((looking-at "throws\\>[^_]")
		       (c-add-syntax 'func-decl-cont
				     (progn (c-beginning-of-statement-1 lim)
					    (c-point 'boi))))
		      (cont (c-add-syntax 'inher-cont where))
		      (t (c-add-syntax 'inher-intro
				       (progn (goto-char (cdr injava-inher))
					      (c-beginning-of-statement-1 lim)
					      (point))))
		      )))
	     ;; CASE 5C.4: a continued inheritance line
	     (t
	      (c-beginning-of-inheritance-list lim)
	      (c-add-syntax 'inher-cont (point))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )))
	   ;; CASE 5D: this could be a top-level initialization, a
	   ;; member init list continuation, or a template argument
	   ;; list continuation.
	   ((c-with-syntax-table (if (c-major-mode-is 'c++-mode)
				     c++-template-syntax-table
				   (syntax-table))
	      (save-excursion
		;; Note: We use the fact that lim is always after any
		;; preceding brace sexp.
		(while (and (= (c-backward-token-1 1 t lim) 0)
			    (not (looking-at "[;<,=]"))))
		(or (memq (char-after) '(?, ?=))
		    (and (c-major-mode-is 'c++-mode)
			 (= (c-backward-token-1 1 nil lim) 0)
			 (eq (char-after) ?<)))))
	    (goto-char indent-point)
	    (c-beginning-of-member-init-list lim)
	    (cond
	     ;; CASE 5D.1: hanging member init colon, but watch out
	     ;; for bogus matches on access specifiers inside classes.
	     ((and (save-excursion
		     (setq placeholder (point))
		     (c-backward-token-1 1 t lim)
		     (and (eq (char-after) ?:)
			  (not (eq (char-before) ?:))))
		   (save-excursion
		     (goto-char placeholder)
		     (back-to-indentation)
		     (or
		      (/= (car (save-excursion
				 (parse-partial-sexp (point) placeholder)))
			  0)
		      (and
		       (if c-opt-access-key
			   (not (looking-at c-opt-access-key)) t)
		       (not (looking-at c-class-key))
		       (if c-opt-bitfield-key
			   (not (looking-at c-opt-bitfield-key)) t))
		      )))
	      (goto-char placeholder)
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point))
	      ;; we do not need to add class offset since relative
	      ;; point is the member init above us
	      )
	     ;; CASE 5D.2: non-hanging member init colon
	     ((progn
		(c-forward-syntactic-ws indent-point)
		(eq (char-after) ?:))
	      (skip-chars-forward " \t:")
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5D.3: perhaps a template list continuation?
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (save-restriction
		       (c-with-syntax-table c++-template-syntax-table
			 (goto-char indent-point)
			 (setq placeholder (c-up-list-backward (point)))
			 (and placeholder
			      (eq (char-after placeholder) ?<))))))
	      ;; we can probably indent it just like an arglist-cont
	      (goto-char placeholder)
	      (c-beginning-of-statement-1 lim t)
	      (c-add-syntax 'template-args-cont (c-point 'boi)))
	     ;; CASE 5D.4: perhaps a multiple inheritance line?
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (c-beginning-of-statement-1 lim)
		     (setq placeholder (point))
		     (if (looking-at "static\\>[^_]")
			 (c-forward-token-1 1 nil indent-point))
		     (and (looking-at c-class-key)
			  (= (c-forward-token-1 2 nil indent-point) 0)
			  (if (eq (char-after) ?<)
			      (c-with-syntax-table c++-template-syntax-table
				(= (c-forward-token-1 1 t indent-point) 0))
			    t)
			  (eq (char-after) ?:))))
	      (goto-char placeholder)
	      (c-add-syntax 'inher-cont (c-point 'boi)))
	     ;; CASE 5D.5: Continuation of the "expression part" of a
	     ;; top level construct.
	     (t
	      (while (and (eq (car (c-beginning-of-decl-1 containing-sexp))
			      'same)
			  (save-excursion
			    (c-backward-syntactic-ws)
			    (eq (char-before) ?}))))
	      (c-add-stmt-syntax
	       (if (eq char-before-ip ?,)
		   ;; A preceding comma at the top level means that a
		   ;; new variable declaration starts here.  Use
		   ;; topmost-intro-cont for it, for consistency with
		   ;; the first variable declaration.  C.f. case 5N.
		   'topmost-intro-cont
		 'statement-cont)
	       nil containing-sexp paren-state))
	     ))
	   ;; CASE 5E: we are looking at a access specifier
	   ((and inclass-p
		 c-opt-access-key
		 (looking-at c-opt-access-key))
	    (setq placeholder (c-add-class-syntax 'inclass inclass-p
						  paren-state))
	    ;; Append access-label with the same anchor point as inclass gets.
	    (nconc syntax (list (cons 'access-label placeholder))))
	   ;; CASE 5F: extern-lang-close or namespace-close?
	   ((and inenclosing-p
		 (eq char-after-ip ?}))
	    (setq tmpsymbol (if (eq inenclosing-p 'extern)
				'extern-lang-close
			      'namespace-close))
	    (c-add-syntax tmpsymbol (aref inclass-p 0)))
	   ;; CASE 5G: we are looking at the brace which closes the
	   ;; enclosing nested class decl
	   ((and inclass-p
		 (eq char-after-ip ?})
		 (save-excursion
		   (save-restriction
		     (widen)
		     (forward-char 1)
		     (and (c-safe (progn (c-backward-sexp 1) t))
			  (= (point) (aref inclass-p 1))
			  ))))
	    (c-add-class-syntax 'class-close inclass-p paren-state))
	   ;; CASE 5H: we could be looking at subsequent knr-argdecls
	   ((and c-recognize-knr-p
		 (not (eq char-before-ip ?}))
		 (save-excursion
		   (setq placeholder (cdr (c-beginning-of-decl-1 lim)))
		   (and placeholder
			;; Do an extra check to avoid tripping up on
			;; statements that occur in invalid contexts
			;; (e.g. in macro bodies where we don't really
			;; know the context of what we're looking at).
			(not (and c-opt-block-stmt-key
				  (looking-at c-opt-block-stmt-key)))))
		 (< placeholder indent-point))
	    (goto-char placeholder)
	    (c-add-syntax 'knr-argdecl (point)))
	   ;; CASE 5I: ObjC method definition.
	   ((and c-opt-method-key
		 (looking-at c-opt-method-key))
	    (c-beginning-of-statement-1 lim)
	    (c-add-syntax 'objc-method-intro (c-point 'boi)))
	   ;; CASE 5N: At a variable declaration that follows a class
	   ;; definition or some other block declaration that doesn't
	   ;; end at the closing '}'.  C.f. case 5D.5.
	   ((progn
	      (c-backward-syntactic-ws lim)
	      (and (eq (char-before) ?})
		   (save-excursion
		     (let ((start (point)))
		       (if paren-state
			   ;; Speed up the backward search a bit.
			   (goto-char (car (car paren-state))))
		       (c-beginning-of-decl-1 containing-sexp)
		       (setq placeholder (point))
		       (if (= start (point))
			   ;; The '}' is unbalanced.
			   nil
			 (c-end-of-decl-1)
			 (> (point) indent-point))))))
	    (goto-char placeholder)
	    (c-add-stmt-syntax 'topmost-intro-cont nil
			       containing-sexp paren-state))
	   ;; CASE 5J: we are at the topmost level, make
	   ;; sure we skip back past any access specifiers
	   ((progn
	      (while (and inclass-p
			  c-opt-access-key
			  (not (bobp))
			  (save-excursion
			    (c-safe (progn (c-backward-sexp 1) t))
			    (looking-at c-opt-access-key)))
		(c-backward-sexp 1)
		(c-backward-syntactic-ws lim))
	      (or (bobp)
		  (memq (char-before) '(?\; ?}))
		  (and (c-major-mode-is 'objc-mode)
		       (progn
			 (c-beginning-of-statement-1 lim)
			 (eq (char-after) ?@)))))
	    ;; real beginning-of-line could be narrowed out due to
	    ;; enclosure in a class block
	    (save-restriction
	      (widen)
	      (c-add-syntax 'topmost-intro (c-point 'bol))
	      ;; Using bol instead of boi above is highly bogus, and
	      ;; it makes our lives hard to remain compatible. :P
	      (if inclass-p
		  (progn
		    (goto-char (aref inclass-p 1))
		    (or (= (point) (c-point 'boi))
			(goto-char (aref inclass-p 0)))
		    (cond
		     ((eq inenclosing-p 'extern)
		      (c-add-syntax 'inextern-lang (c-point 'boi)))
		     ((eq inenclosing-p 'namespace)
		      (c-add-syntax 'innamespace (c-point 'boi)))
		     (t (c-add-class-syntax 'inclass inclass-p paren-state)))
		    ))
	      (when (and c-syntactic-indentation-in-macros
			 macro-start
			 (/= macro-start (c-point 'boi indent-point)))
		(c-add-syntax 'cpp-define-intro)
		(setq macro-start nil))
	      ))
	   ;; CASE 5K: we are at an ObjC method definition
	   ;; continuation line.
	   ((and c-opt-method-key
		 (progn
		   (c-beginning-of-statement-1 lim)
		   (beginning-of-line)
		   (looking-at c-opt-method-key)))
	    (c-add-syntax 'objc-method-args-cont (point)))
	   ;; CASE 5L: we are at the first argument of a template
	   ;; arglist that begins on the previous line.
	   ((eq (char-before) ?<)
	    (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
	    (c-add-syntax 'template-args-cont (c-point 'boi)))
	   ;; CASE 5M: we are at a topmost continuation line
	   (t
	    (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
	    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	   ))
	 ;; (CASE 6 has been removed.)
	 ;; CASE 7: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((not (or (and c-special-brace-lists
			(save-excursion
			  (goto-char containing-sexp)
			  (c-looking-at-special-brace-list)))
		   (eq (char-after containing-sexp) ?{)))
	  (cond
	   ;; CASE 7A: we are looking at the arglist closing paren
	   ((memq char-after-ip '(?\) ?\]))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (> (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-close placeholder))
	   ;; CASE 7B: Looking at the opening brace of an
	   ;; in-expression block or brace list.  C.f. cases 4, 16A
	   ;; and 17E.
	   ((and (eq char-after-ip ?{)
		 (progn
		   (setq placeholder (c-inside-bracelist-p (point)
							   c-state-cache))
		   (if placeholder
		       (setq tmpsymbol '(brace-list-open . inexpr-class))
		     (setq tmpsymbol '(block-open . inexpr-statement)
			   placeholder
			   (cdr-safe (c-looking-at-inexpr-block
				      (c-safe-position containing-sexp
						       paren-state)
				      containing-sexp)))
		     ;; placeholder is nil if it's a block directly in
		     ;; a function arglist.  That makes us skip out of
		     ;; this case.
		     )))
	    (goto-char placeholder)
	    (back-to-indentation)
	    (c-add-stmt-syntax (car tmpsymbol) t
			       (c-most-enclosing-brace paren-state (point))
			       (c-whack-state-after (point) paren-state))
	    (if (/= (point) placeholder)
		(c-add-syntax (cdr tmpsymbol))))
	   ;; CASE 7C: we are looking at the first argument in an empty
	   ;; argument list. Use arglist-close if we're actually
	   ;; looking at a close paren or bracket.
	   ((memq char-before-ip '(?\( ?\[))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (> (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-intro placeholder))
	   ;; CASE 7D: we are inside a conditional test clause. treat
	   ;; these things as statements
	   ((progn
	      (goto-char containing-sexp)
	      (and (c-safe (progn (c-forward-sexp -1) t))
		   (looking-at "\\<for\\>[^_]")))
	    (goto-char (1+ containing-sexp))
	    (c-forward-syntactic-ws indent-point)
	    (if (eq char-before-ip ?\;)
		(c-add-syntax 'statement (point))
	      (c-add-syntax 'statement-cont (point))
	      ))
	   ;; CASE 7E: maybe a continued ObjC method call. This is the
	   ;; case when we are inside a [] bracketed exp, and what
	   ;; precede the opening bracket is not an identifier.
	   ((and c-opt-method-key
		 (eq (char-after containing-sexp) ?\[)
		 (progn
		   (goto-char (1- containing-sexp))
		   (c-backward-syntactic-ws (c-point 'bod))
		   (if (not (looking-at c-symbol-key))
		       (c-add-syntax 'objc-method-call-cont containing-sexp))
		   )))
	   ;; CASE 7F: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; mathematical paren groupings, but we could be on a
	   ;; for-list continuation line
	   ((progn
	      (goto-char (1+ containing-sexp))
	      (skip-chars-forward " \t")
	      (and (not (eolp))
		   (not (looking-at "\\\\$"))))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (> (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-cont-nonempty placeholder))
	   ;; CASE 7G: we are looking at just a normal arglist
	   ;; continuation line
	   (t (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'arglist-cont (c-point 'boi)))
	   ))
	 ;; CASE 8: func-local multi-inheritance line
	 ((and (c-major-mode-is 'c++-mode)
	       (save-excursion
		 (goto-char indent-point)
		 (skip-chars-forward " \t")
		 (looking-at c-opt-decl-spec-key)))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 8A: non-hanging colon on an inher intro
	   ((eq char-after-ip ?:)
	    (c-backward-syntactic-ws lim)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 8B: hanging colon on an inher intro
	   ((eq char-before-ip ?:)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 8C: a continued inheritance line
	   (t
	    (c-beginning-of-inheritance-list lim)
	    (c-add-syntax 'inher-cont (point))
	    )))
	 ;; CASE 9: we are inside a brace-list
	 ((setq special-brace-list
		(or (and c-special-brace-lists
			 (save-excursion
			   (goto-char containing-sexp)
			   (c-looking-at-special-brace-list)))
		    (c-inside-bracelist-p containing-sexp paren-state)))
	  (cond
	   ;; CASE 9A: In the middle of a special brace list opener.
	   ((and (consp special-brace-list)
		 (save-excursion
		   (goto-char containing-sexp)
		   (eq (char-after) ?\())
		 (eq char-after-ip (car (cdr special-brace-list))))
	    (goto-char (car (car special-brace-list)))
	    (skip-chars-backward " \t")
	    (if (and (bolp)
		     (assoc 'statement-cont
			    (setq placeholder (c-guess-basic-syntax))))
		(setq syntax placeholder)
	      (c-beginning-of-statement-1
	       (c-safe-position (1- containing-sexp) paren-state))
	      (c-forward-token-1 0)
	      (if (looking-at "typedef\\>[^_]") (c-forward-token-1 1))
	      (c-add-syntax 'brace-list-open (c-point 'boi))))
	   ;; CASE 9B: brace-list-close brace
	   ((if (consp special-brace-list)
		;; Check special brace list closer.
		(progn
		  (goto-char (car (car special-brace-list)))
		  (save-excursion
		    (goto-char indent-point)
		    (back-to-indentation)
		    (or
		     ;; We were between the special close char and the `)'.
		     (and (eq (char-after) ?\))
			  (eq (1+ (point)) (cdr (car special-brace-list))))
		     ;; We were before the special close char.
		     (and (eq (char-after) (cdr (cdr special-brace-list)))
			  (= (c-forward-token-1) 0)
			  (eq (1+ (point)) (cdr (car special-brace-list)))))))
	      ;; Normal brace list check.
	      (and (eq char-after-ip ?})
		   (c-safe (progn (goto-char (c-up-list-backward (point)))
				  t))
		   (= (point) containing-sexp)))
	    (if (eq (point) (c-point 'boi))
		(c-add-syntax 'brace-list-close (point))
	      (setq lim (c-most-enclosing-brace c-state-cache (point)))
	      (c-beginning-of-statement-1 lim)
	      (c-add-stmt-syntax 'brace-list-close t lim
				 (c-whack-state-after (point) paren-state)
				 t)))
	   (t
	    ;; Prepare for the rest of the cases below by going to the
	    ;; token following the opening brace
	    (if (consp special-brace-list)
		(progn
		  (goto-char (car (car special-brace-list)))
		  (c-forward-token-1 1 nil indent-point))
	      (goto-char containing-sexp))
	    (forward-char)
	    (let ((start (point)))
	      (c-forward-syntactic-ws indent-point)
	      (goto-char (max start (c-point 'bol))))
	    (c-skip-ws-forward indent-point)
	    (cond
	     ;; CASE 9C: we're looking at the first line in a brace-list
	     ((= (point) indent-point)
	      (if (consp special-brace-list)
		  (goto-char (car (car special-brace-list)))
		(goto-char containing-sexp))
	      (if (eq (point) (c-point 'boi))
		  (c-add-syntax 'brace-list-intro (point))
		(setq lim (c-most-enclosing-brace c-state-cache (point)))
		(c-beginning-of-statement-1 lim)
		(c-add-stmt-syntax 'brace-list-intro t lim
				   (c-whack-state-after (point) paren-state)
				   t)))
	     ;; CASE 9D: this is just a later brace-list-entry or
	     ;; brace-entry-open
	     (t (if (or (eq char-after-ip ?{)
			(and c-special-brace-lists
			     (save-excursion
			       (goto-char indent-point)
			       (c-forward-syntactic-ws (c-point 'eol))
			       (c-looking-at-special-brace-list (point)))))
		    (c-add-syntax 'brace-entry-open (point))
		  (c-add-syntax 'brace-list-entry (point))
		  ))
	     ))))
	 ;; CASE 10: A continued statement or top level construct.
	 ((and (not (memq char-before-ip '(?\; ?:)))
	       (or (not (eq char-before-ip ?}))
		   (c-looking-at-inexpr-block-backward c-state-cache))
	       (> (point)
		  (save-excursion
		    (c-beginning-of-statement-1 containing-sexp)
		    (setq placeholder (point))))
	       (/= placeholder containing-sexp))
	  ;; This is shared with case 18.
	  (c-guess-continued-construct indent-point
				       char-after-ip
				       placeholder
				       containing-sexp
				       paren-state))
	 ;; CASE 14: A case or default label
	 ((looking-at c-label-kwds-regexp)
	  (goto-char containing-sexp)
	  (setq lim (c-most-enclosing-brace c-state-cache containing-sexp))
	  (c-backward-to-block-anchor lim)
	  (c-add-stmt-syntax 'case-label t lim paren-state))
	 ;; CASE 15: any other label
	 ((looking-at c-label-key)
	  (goto-char containing-sexp)
	  (setq lim (c-most-enclosing-brace c-state-cache containing-sexp))
	  (save-excursion
	    (setq tmpsymbol
		  (if (and (eq (c-beginning-of-statement-1 lim) 'up)
			   (looking-at "switch\\>[^_]"))
		      ;; If the surrounding statement is a switch then
		      ;; let's analyze all labels as switch labels, so
		      ;; that they get lined up consistently.
		      'case-label
		    'label)))
	  (c-backward-to-block-anchor lim)
	  (c-add-stmt-syntax tmpsymbol t lim paren-state))
	 ;; CASE 16: block close brace, possibly closing the defun or
	 ;; the class
	 ((eq char-after-ip ?})
	  ;; From here on we have the next containing sexp in lim.
	  (setq lim (c-most-enclosing-brace paren-state))
	  (goto-char containing-sexp)
	    (cond
	     ;; CASE 16E: Closing a statement block?  This catches
	     ;; cases where it's preceded by a statement keyword,
	     ;; which works even when used in an "invalid" context,
	     ;; e.g. a macro argument.
	     ((c-after-conditional)
	      (c-backward-to-block-anchor lim)
	      (c-add-stmt-syntax 'block-close t lim paren-state))
	     ;; CASE 16A: closing a lambda defun or an in-expression
	     ;; block?  C.f. cases 4, 7B and 17E.
	     ((setq placeholder (c-looking-at-inexpr-block
				 (c-safe-position containing-sexp paren-state)
				 nil))
	      (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				  'inline-close
				'block-close))
	      (goto-char containing-sexp)
	      (back-to-indentation)
	      (if (= containing-sexp (point))
		  (c-add-syntax tmpsymbol (point))
		(goto-char (cdr placeholder))
		(back-to-indentation)
		(c-add-stmt-syntax tmpsymbol t
				   (c-most-enclosing-brace paren-state (point))
				   (c-whack-state-after (point) paren-state))
		(if (/= (point) (cdr placeholder))
		    (c-add-syntax (car placeholder)))))
	     ;; CASE 16B: does this close an inline or a function in
	     ;; an extern block or namespace?
	     ((setq placeholder (c-search-uplist-for-classkey paren-state))
	      (c-backward-to-decl-anchor lim)
	      (back-to-indentation)
	      (if (save-excursion
		    (goto-char (aref placeholder 0))
		    (looking-at c-other-decl-block-key))
		  (c-add-syntax 'defun-close (point))
		(c-add-syntax 'inline-close (point))))
	     ;; CASE 16F: Can be a defun-close of a function declared
	     ;; in a statement block, e.g. in Pike or when using gcc
	     ;; extensions.  Might also trigger it with some macros
	     ;; followed by blocks, and this gives sane indentation
	     ;; then too.  Let it through to be handled below.
	     ;; C.f. cases B.3 and 17G.
	     ((and (not inenclosing-p)
		   lim
		   (save-excursion
		     (and (not (c-looking-at-bos))
			  (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
			  (setq placeholder (point)))))
	      (back-to-indentation)
	      (if (/= (point) containing-sexp)
		  (goto-char placeholder))
	      (c-add-stmt-syntax 'defun-close t lim paren-state))
	     ;; CASE 16C: if there an enclosing brace that hasn't
	     ;; been narrowed out by a class, then this is a
	     ;; block-close.  C.f. case 17H.
	     ((and (not inenclosing-p) lim)
	      ;; If the block is preceded by a case/switch label on
	      ;; the same line, we anchor at the first preceding label
	      ;; at boi.  The default handling in c-add-stmt-syntax is
	      ;; really fixes it better, but we do like this to keep
	      ;; the indentation compatible with version 5.28 and
	      ;; earlier.
	      (while (and (/= (setq placeholder (point)) (c-point 'boi))
			  (eq (c-beginning-of-statement-1 lim) 'label)))
	      (goto-char placeholder)
	      (if (looking-at c-label-kwds-regexp)
		  (c-add-syntax 'block-close (point))
		(goto-char containing-sexp)
		;; c-backward-to-block-anchor not necessary here; those
		;; situations are handled in case 16E above.
		(c-add-stmt-syntax 'block-close t lim paren-state)))
	     ;; CASE 16D: find out whether we're closing a top-level
	     ;; class or a defun
	     (t
	      (save-restriction
		(narrow-to-region (point-min) indent-point)
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (if decl
		      (c-add-class-syntax 'class-close decl paren-state)
		    (goto-char containing-sexp)
		    (c-backward-to-decl-anchor lim)
		    (back-to-indentation)
		    (c-add-syntax 'defun-close (point)))))
	      )))
	 ;; CASE 17: Statement or defun catchall.
	 (t
	  (goto-char indent-point)
	  ;; Back up statements until we find one that starts at boi.
	  (while (let* ((prev-point (point))
			(last-step-type (c-beginning-of-statement-1
					 containing-sexp)))
		   (if (= (point) prev-point)
		       (progn
			 (setq step-type (or step-type last-step-type))
			 nil)
		     (setq step-type last-step-type)
		     (/= (point) (c-point 'boi)))))
	  (cond
	   ;; CASE 17B: continued statement
	   ((and (eq step-type 'same)
		 (/= (point) indent-point))
	    (c-add-stmt-syntax 'statement-cont nil
			       containing-sexp paren-state))
	   ;; CASE 17A: After a case/default label?
	   ((progn
	      (while (and (eq step-type 'label)
			  (not (looking-at c-label-kwds-regexp)))
		(setq step-type
		      (c-beginning-of-statement-1 containing-sexp)))
	      (eq step-type 'label))
	    (c-add-stmt-syntax (if (eq char-after-ip ?{)
				   'statement-case-open
				 'statement-case-intro)
			       t containing-sexp paren-state))
	   ;; CASE 17D: any old statement
	   ((progn
	      (while (eq step-type 'label)
		(setq step-type
		      (c-beginning-of-statement-1 containing-sexp)))
	      (eq step-type 'previous))
	    (c-add-stmt-syntax 'statement t containing-sexp paren-state)
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17I: Inside a substatement block.
	   ((progn
	      ;; The following tests are all based on containing-sexp.
	      (goto-char containing-sexp)
	      ;; From here on we have the next containing sexp in lim.
	      (setq lim (c-most-enclosing-brace paren-state containing-sexp))
	      (c-after-conditional))
	    (c-backward-to-block-anchor lim)
	    (c-add-stmt-syntax 'statement-block-intro t lim paren-state)
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17E: first statement in an in-expression block.
	   ;; C.f. cases 4, 7B and 16A.
	   ((setq placeholder (c-looking-at-inexpr-block
			       (c-safe-position containing-sexp paren-state)
			       nil))
	    (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				'defun-block-intro
			      'statement-block-intro))
	    (back-to-indentation)
	    (if (= containing-sexp (point))
		(c-add-syntax tmpsymbol (point))
	      (goto-char (cdr placeholder))
	      (back-to-indentation)
	      (c-add-stmt-syntax tmpsymbol t
				 (c-most-enclosing-brace c-state-cache (point))
				 (c-whack-state-after (point) paren-state))
	      (if (/= (point) (cdr placeholder))
		  (c-add-syntax (car placeholder))))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17F: first statement in an inline, or first
	   ;; statement in a top-level defun. we can tell this is it
	   ;; if there are no enclosing braces that haven't been
	   ;; narrowed out by a class (i.e. don't use bod here).
	   ;; However, we first check for statements that we can
	   ;; recognize by keywords.  That increases the robustness in
	   ;; cases where statements are used on the top level,
	   ;; e.g. in macro definitions.
	   ((save-excursion
	      (save-restriction
		(widen)
		(c-narrow-out-enclosing-class paren-state containing-sexp)
		(not (c-most-enclosing-brace paren-state))))
	    (c-backward-to-decl-anchor lim)
	    (back-to-indentation)
	    (c-add-syntax 'defun-block-intro (point)))
	   ;; CASE 17G: First statement in a function declared inside
	   ;; a normal block.  This can occur in Pike and with
	   ;; e.g. the gcc extensions.  Might also trigger it with
	   ;; some macros followed by blocks, and this gives sane
	   ;; indentation then too.  C.f. cases B.3 and 16F.
	   ((save-excursion
	      (and (not (c-looking-at-bos))
		   (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
		   (setq placeholder (point))))
	    (back-to-indentation)
	    (if (/= (point) containing-sexp)
		(goto-char placeholder))
	    (c-add-stmt-syntax 'defun-block-intro t lim paren-state))
	   ;; CASE 17H: First statement in a block.  C.f. case 16C.
	   (t
	    ;; If the block is preceded by a case/switch label on the
	    ;; same line, we anchor at the first preceding label at
	    ;; boi.  The default handling in c-add-stmt-syntax is
	    ;; really fixes it better, but we do like this to keep the
	    ;; indentation compatible with version 5.28 and earlier.
	    (while (and (/= (setq placeholder (point)) (c-point 'boi))
			(eq (c-beginning-of-statement-1 lim) 'label)))
	    (goto-char placeholder)
	    (if (looking-at c-label-kwds-regexp)
		(c-add-syntax 'statement-block-intro (point))
	      (goto-char containing-sexp)
	      ;; c-backward-to-block-anchor not necessary here; those
	      ;; situations are handled in case 17I above.
	      (c-add-stmt-syntax 'statement-block-intro t lim paren-state))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ))
	 )
	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	;; are we looking at a comment only line?
	(when (and (looking-at c-comment-start-regexp)
		   (/= (c-forward-token-1 0 nil (c-point 'eol)) 0))
	  (c-add-syntax 'comment-intro))
	;; we might want to give additional offset to friends (in C++).
	(when (and c-opt-friend-key
		   (looking-at c-opt-friend-key))
	  (c-add-syntax 'friend))
	;; Start of or a continuation of a preprocessor directive?
	(if (and macro-start
		 (eq macro-start (c-point 'boi))
		 (not (and (c-major-mode-is 'pike-mode)
			   (eq (char-after (1+ macro-start)) ?\"))))
	    (c-add-syntax 'cpp-macro)
	  (when (and c-syntactic-indentation-in-macros macro-start)
	    (if in-macro-expr
		(when (or (< syntactic-relpos macro-start)
			  (not (or (assq 'arglist-intro syntax)
				   (assq 'arglist-cont syntax)
				   (assq 'arglist-cont-nonempty syntax)
				   (assq 'arglist-close syntax))))
		  ;; If inside a cpp expression, i.e. anywhere in a
		  ;; cpp directive except a #define body, we only let
		  ;; through the syntactic analysis that is internal
		  ;; in the expression.  That means the arglist
		  ;; elements, if they are anchored inside the cpp
		  ;; expression.
		  (setq syntax `((cpp-macro-cont . ,macro-start))))
	      (when (and (eq macro-start syntactic-relpos)
			 (not (assq 'cpp-define-intro syntax))
			 (save-excursion
			   (goto-char macro-start)
			   (or (not (c-forward-to-cpp-define-body))
			       (<= (point) (c-point 'boi indent-point)))))
		;; Inside a #define body and the syntactic analysis is
		;; anchored on the start of the #define.  In this case
		;; we add cpp-define-intro to get the extra
		;; indentation of the #define body.
		(c-add-syntax 'cpp-define-intro)))))
	;; return the syntax
	syntax))))


(defun c-echo-parsing-error (&optional quiet)
  (when (and c-report-syntactic-errors c-parsing-error (not quiet))
    (c-benign-error "%s" c-parsing-error))
  c-parsing-error)

(defun c-evaluate-offset (offset langelem symbol)
  ;; offset can be a number, a function, a variable, a list, or one of
  ;; the symbols + or -
  (cond
   ((eq offset '+)         c-basic-offset)
   ((eq offset '-)         (- c-basic-offset))
   ((eq offset '++)        (* 2 c-basic-offset))
   ((eq offset '--)        (* 2 (- c-basic-offset)))
   ((eq offset '*)         (/ c-basic-offset 2))
   ((eq offset '/)         (/ (- c-basic-offset) 2))
   ((numberp offset)       offset)
   ((functionp offset)     (c-evaluate-offset
			    (funcall offset langelem) langelem symbol))
   ((vectorp offset)       offset)
   ((null offset)          nil)
   ((listp offset)
    (let (done)
      (while (and (not done) offset)
	(setq done (c-evaluate-offset (car offset) langelem symbol)
	      offset (cdr offset)))
      (if (and c-strict-syntax-p (not done))
	  (c-benign-error "No offset found for syntactic symbol %s" symbol))
      done))
   (t (symbol-value offset))
   ))

(defun c-get-offset (langelem)
  "Get offset from LANGELEM which is a cons cell of the form:
\(SYMBOL . RELPOS).  The symbol is matched against `c-offsets-alist'
and the offset found there is returned."
  (let* ((symbol (car langelem))
	 (match  (assq symbol c-offsets-alist))
	 (offset (cdr-safe match)))
    (if match
	(setq offset (c-evaluate-offset offset langelem symbol))
      (if c-strict-syntax-p
	  (c-benign-error "No offset found for syntactic symbol %s" symbol))
      (setq offset 0))
    (if (vectorp offset)
	offset
      (or (and (numberp offset) offset)
	  (and (symbolp offset) (symbol-value offset))
	  0))
    ))

(defun c-get-syntactic-indentation (langelems)
  "Apply `c-get-offset' to a list of langelem cells to get the total
syntactic indentation.  The anchor position, whose column is used as a
base for all the collected offsets, is taken from the first element
with a relpos."
  ;; Note that topmost-intro always has a relpos at bol, for
  ;; historical reasons.  It's often used together with other symbols
  ;; that has more sane positions.  Since we always use the first
  ;; found relpos, we rely on that these other symbols always precede
  ;; topmost-intro in the LANGELEMS list.
  (let ((indent 0) anchor)
    (catch 'done
      (while langelems
	(let ((res (c-get-offset (car langelems))))
	  (if (vectorp res)
	      (throw 'done (elt res 0))
	    (unless anchor
	      (let ((relpos (cdr (car langelems))))
		(if relpos
		    (setq anchor relpos))))
	    (setq indent (+ indent res)
		  langelems (cdr langelems)))))
      (+ indent
	 (if anchor
	     (save-excursion
	       (goto-char anchor)
	       (current-column))
	   0)))))


(cc-provide 'cc-engine)

;;; cc-engine.el ends here
