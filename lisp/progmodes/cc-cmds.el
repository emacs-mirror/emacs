;;; cc-cmds.el --- user level commands for CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97,98 Free Softare Foundation, Inc.

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
  (require 'cc-defs))

(defun c-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

;; Auto-newline and hungry-delete
(defun c-toggle-auto-state (arg)
  "Toggle auto-newline feature.
Optional numeric ARG, if supplied turns on auto-newline when positive,
turns it off when negative, and just toggles it when zero.

When the auto-newline feature is enabled (as evidenced by the `/a' or
`/ah' on the modeline after the mode name) newlines are automatically
inserted after special characters such as brace, comma, semi-colon,
and colon."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-hungry-state (arg)
  "Toggle hungry-delete-key feature.
Optional numeric ARG, if supplied turns on hungry-delete when positive,
turns it off when negative, and just toggles it when zero.

When the hungry-delete-key feature is enabled (as evidenced by the
`/h' or `/ah' on the modeline after the mode name) the delete key
gobbles all preceding whitespace in one fell swoop."
  (interactive "P")
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))

(defun c-toggle-auto-hungry-state (arg)
  "Toggle auto-newline and hungry-delete-key features.
Optional numeric ARG, if supplied turns on auto-newline and
hungry-delete when positive, turns them off when negative, and just
toggles them when zero.

See `c-toggle-auto-state' and `c-toggle-hungry-state' for details."
  (interactive "P")
  (setq c-auto-newline (c-calculate-state arg c-auto-newline))
  (setq c-hungry-delete-key (c-calculate-state arg c-hungry-delete-key))
  (c-update-modeline)
  (c-keep-region-active))


;; Electric keys

;; Note: In XEmacs 20.3 the Delete and BackSpace keysyms have been
;; separated and "\177" is no longer an alias for both keys.  Also,
;; the variable delete-key-deletes-forward controls in which direction
;; the Delete keysym deletes characters.  The functions
;; c-electric-delete and c-electric-backspace attempt to deal with
;; this new functionality.  For Emacs 19 and XEmacs 19 backwards
;; compatibility, the old behavior has moved to c-electric-backspace
;; and c-backspace-function.

(defun c-electric-backspace (arg)
  "Deletes preceding character or whitespace.
If `c-hungry-delete-key' is non-nil, as evidenced by the \"/h\" or
\"/ah\" string on the mode line, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `c-hungry-delete-key' is
nil, or point is inside a literal then the function in the variable
`c-backspace-function' is called.

See also \\[c-electric-delete]."
  (interactive "*P")
  (if (or (not c-hungry-delete-key)
	  arg
	  (c-in-literal))
      (funcall c-backspace-function (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall c-backspace-function 1)
	))))

(defun c-electric-delete (arg)
  "Deletes preceding or following character or whitespace.

The behavior of this function depends on the variable
`delete-key-deletes-forward'.  If this variable is nil (or does not
exist, as in older Emacsen), then this function behaves identical to
\\[c-electric-backspace].

If `delete-key-deletes-forward' is non-nil and is supported in your
Emacs, then deletion occurs in the forward direction.  So if
`c-hungry-delete-key' is non-nil, as evidenced by the \"/h\" or
\"/ah\" string on the mode line, then all following whitespace is
consumed.  If however an ARG is supplied, or `c-hungry-delete-key' is
nil, or point is inside a literal then the function in the variable
`c-delete-function' is called."
  (interactive "*P")
  (if (or (and (fboundp 'delete-forward-p) ;XEmacs 21
	       (delete-forward-p))
	  (and (boundp 'delete-key-deletes-forward) ;XEmacs 20
	       delete-key-deletes-forward))
      (if (or (not c-hungry-delete-key)
	      arg
	      (c-in-literal))
	  (funcall c-delete-function (prefix-numeric-value arg))
	(let ((here (point)))
	  (skip-chars-forward " \t\n")
	  (if (/= (point) here)
	      (delete-region (point) here)
	    (funcall c-delete-function 1))))
    ;; act just like c-electric-backspace
    (c-electric-backspace arg)))

(defun c-electric-pound (arg)
  "Electric pound (`#') insertion.
Inserts a `#' character specially depending on the variable
`c-electric-pound-behavior'.  If a numeric ARG is supplied, or if
point is inside a literal, nothing special happens."
  (interactive "*P")
  (if (or arg
	  (not (memq 'alignleft c-electric-pound-behavior))
	  (save-excursion (skip-chars-backward " \t") (not (bolp)))
	  (c-in-literal))
      ;; do nothing special
      (self-insert-command (prefix-numeric-value arg))
    ;; place the pound character at the left edge
    (let ((pos (- (point-max) (point)))
	  (bolp (bolp)))
      (beginning-of-line)
      (delete-horizontal-space)
      (insert-char last-command-char 1)
      (and (not bolp)
	   (goto-char (- (point-max) pos)))
      )))

(defun c-electric-brace (arg)
  "Insert a brace.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, newlines are inserted before and
after braces based on the value of `c-hanging-braces-alist'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the brace, or
the brace is inserted inside a literal.

This function does various newline cleanups based on the value of
`c-cleanup-list'."
  (interactive "*P")
  (let* ((c-state-cache (c-parse-state))
	 (safepos (c-safe-position (point) c-state-cache))
	 (literal (c-in-literal safepos)))
    ;; if we're in a literal, or we're not at the end of the line, or
    ;; a numeric arg is provided, or auto-newlining is turned off,
    ;; then just insert the character.
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))
      (let* ((syms
	      ;; This is the list of brace syntactic symbols that can
	      ;; hang.  If any new ones are added to c-offsets-alist,
	      ;; they should be added here as well.
	      '(class-open class-close defun-open defun-close
		inline-open inline-close
		brace-list-open brace-list-close
		brace-list-intro brace-entry-open
		block-open block-close
		substatement-open statement-case-open
		extern-lang-open extern-lang-close
		namespace-open namespace-close
		inexpr-class-open inexpr-class-close
		))
	    ;; we want to inhibit blinking the paren since this will
	    ;; be most disruptive. we'll blink it ourselves later on
	    (old-blink-paren blink-paren-function)
	    blink-paren-function
	    (insertion-point (point))
	    delete-temp-newline
	    (preserve-p (and (not (bobp))
			     (eq ?\  (char-syntax (char-before)))))
	    ;; shut this up too
	    (c-echo-syntactic-information-p nil)
	    (syntax (progn
		      ;; only insert a newline if there is
		      ;; non-whitespace behind us
		      (if (save-excursion
			    (skip-chars-backward " \t")
			    (not (bolp)))
			  (progn (newline)
				 (setq delete-temp-newline t)))
		      (self-insert-command (prefix-numeric-value arg))
		      ;; state cache doesn't change
		      (c-guess-basic-syntax)))
	    (newlines (and
		       c-auto-newline
		       (or (c-lookup-lists
			    syms
			    ;; Substitute inexpr-class and class-open
			    ;; or class-close with inexpr-class-open
			    ;; or inexpr-class-close.
			    (if (assq 'inexpr-class syntax)
				(cond ((assq 'class-open syntax)
				       '((inexpr-class-open)))
				      ((assq 'class-close syntax)
				       '((inexpr-class-close)))
				      (t syntax))
			      syntax)
			    c-hanging-braces-alist)
			   '(ignore before after)))))
	;; Do not try to insert newlines around a special (Pike-style)
	;; brace list.
	(if (and c-special-brace-lists
		 (c-intersect-lists '(brace-list-open brace-list-close
				      brace-list-intro brace-entry-open)
				    syntax)
		 (save-excursion
		   (c-safe (if (= (char-before) ?{)
			       (forward-char -1)
			     (c-forward-sexp -1))
			   (c-looking-at-special-brace-list))))
	    (setq newlines nil))
	;; If syntax is a function symbol, then call it using the
	;; defined semantics.
	(if (and (not (consp (cdr newlines)))
		 (functionp (cdr newlines)))
	    (let ((c-syntactic-context syntax))
	      (setq newlines
		    (funcall (cdr newlines) (car newlines) insertion-point))))
	;; does a newline go before the open brace?
	(if (memq 'before newlines)
	    ;; we leave the newline we've put in there before,
	    ;; but we need to re-indent the line above
	    (let ((pos (- (point-max) (point)))
		  (here (point)))
	      (forward-line -1)
	      (let ((c-state-cache (c-whack-state (point) c-state-cache)))
		;; we may need to update the cache. this should
		;; still be faster than recalculating the state
		;; in many cases
		(save-excursion
		  (save-restriction
		    (narrow-to-region here (point))
		    (if (and (c-safe (progn (backward-up-list -1) t))
			     (memq (char-before) '(?\) ?}))
			     (progn (widen)
				    (c-safe (progn (c-forward-sexp -1)
						   t))))
			(setq c-state-cache
			      (c-hack-state (point) 'open c-state-cache)))))
		(c-indent-line))
	      (setq c-state-cache (c-adjust-state (c-point 'bol) here
						  (- (point) (c-point 'bol))
						  c-state-cache))
	      (goto-char (- (point-max) pos))
	      ;; if the buffer has changed due to the indentation, we
	      ;; need to recalculate syntax for the current line, but
	      ;; we won't need to update the state cache.
	      (if (/= (point) here)
		  (setq syntax (c-guess-basic-syntax))))
	  ;; must remove the newline we just stuck in (if we really did it)
	  (and delete-temp-newline
	       (save-excursion
		 ;; if there is whitespace before point, then preserve
		 ;; at least one space.
		 (delete-indentation)
		 (just-one-space)
		 (if (not preserve-p)
		     (delete-char -1))))
	  ;; since we're hanging the brace, we need to recalculate
	  ;; syntax.  Update the state to accurately reflect the
	  ;; beginning of the line.  We punt if we cross any open or
	  ;; closed parens because its just too hard to modify the
	  ;; known state.  This limitation will be fixed in v5.
	  (save-excursion
	    (let ((bol (c-point 'bol)))
	      (if (zerop (car (parse-partial-sexp bol (1- (point)))))
		  (setq c-state-cache (c-whack-state bol c-state-cache)
			syntax (c-guess-basic-syntax))
		;; gotta punt. this requires some horrible kludgery
		(beginning-of-line)
		(makunbound 'c-state-cache)
		(setq c-state-cache (c-parse-state)
		      syntax nil))))
	  )
	;; now adjust the line's indentation. don't update the state
	;; cache since c-guess-basic-syntax isn't called when the
	;; syntax is passed to c-indent-line
	(let* ((here (point)))
	  (c-indent-line syntax)
	  (setq c-state-cache (c-adjust-state (c-point 'bol) here
					      (- (c-point 'boi) (c-point 'bol))
					      c-state-cache)))
	;; Do all appropriate clean ups
	(let ((here (point))
	      (pos (- (point-max) (point)))
	      mbeg mend)
	  ;; clean up empty defun braces
	  (if (and c-auto-newline
		   (memq 'empty-defun-braces c-cleanup-list)
		   (eq last-command-char ?\})
		   (c-intersect-lists '(defun-close class-close inline-close)
				      syntax)
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n")
		     (eq (char-before) ?\{))
		   ;; make sure matching open brace isn't in a comment
		   (not (c-in-literal)))
	      (delete-region (point) (1- here)))
	  ;; clean up brace-else-brace
	  (if (and c-auto-newline
		   (memq 'brace-else-brace c-cleanup-list)
		   (eq last-command-char ?\{)
		   (re-search-backward "}[ \t\n]*else[ \t\n]*{" nil t)
		   (progn
		     (setq mbeg (match-beginning 0)
			   mend (match-end 0))
		     (= mend here))
		   (not (c-in-literal)))
	      (progn
		(delete-region mbeg mend)
		(insert "} else {")))
	  (goto-char (- (point-max) pos))
	  )
	;; does a newline go after the brace?
	(if (memq 'after newlines)
	    (progn
	      (newline)
	      ;; update on c-state-cache
	      (let* ((bufpos (- (point) 2))
		     (which (if (eq (char-after bufpos) ?{) 'open 'close))
		     (c-state-cache (c-hack-state bufpos which c-state-cache)))
		(c-indent-line))))
	;; blink the paren
	(and (eq last-command-char ?\})
	     old-blink-paren
	     (save-excursion
	       (c-backward-syntactic-ws safepos)
	       (funcall old-blink-paren)))
	))))

(defun c-electric-slash (arg)
  "Insert a slash character.

Indent the line as a comment, if:

  1. The slash is second of a `//' line oriented comment introducing
     token and we are on a comment-only-line, or

  2. The slash is part of a `*/' token that closes a block oriented
     comment.

If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "*P")
  (let* ((ch (char-before))
	 (indentp (and (not arg)
		       (eq last-command-char ?/)
		       (or (and (eq ch ?/)
				(not (c-in-literal)))
			   (and (eq ch ?*)
				(c-in-literal)))
		       ))
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-line))))

(defun c-electric-star (arg)
  "Insert a star character.
If the star is the second character of a C style comment introducing
construct, and we are on a comment-only-line, indent line as comment.
If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  ;; if we are in a literal, or if arg is given do not re-indent the
  ;; current line, unless this star introduces a comment-only line.
  (if (and (not arg)
	   (memq (c-in-literal) '(c))
	   (eq (char-before) ?*)
	   (save-excursion
	     (forward-char -1)
	     (skip-chars-backward "*")
	     (if (eq (char-before) ?/)
		 (forward-char -1))
	     (skip-chars-backward " \t")
	     (bolp)))
      ;; shut this up
      (let (c-echo-syntactic-information-p)
	(c-indent-line))
    ))

(defun c-electric-semi&comma (arg)
  "Insert a comma or semicolon.
When the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, a newline might be inserted.  See
the variable `c-hanging-semi&comma-criteria' for how newline insertion
is determined.

When semicolon is inserted, the line is re-indented unless a numeric
arg is supplied, point is inside a literal, or there are
non-whitespace characters on the line following the semicolon.

Based on the value of `c-cleanup-list', this function cleans up commas
following brace lists and semicolons following defuns."
  (interactive "*P")
  (let* ((lim (c-most-enclosing-brace (c-parse-state)))
	 (literal (c-in-literal lim))
	 (here (point))
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))
      ;; do some special stuff with the character
      (self-insert-command (prefix-numeric-value arg))
      ;; do all cleanups and newline insertions if c-auto-newline is
      ;; turned on
      (if (not c-auto-newline)
	  (c-indent-line)
	;; clean ups
	(let ((pos (- (point-max) (point))))
	  (if (and (or (and
			(eq last-command-char ?,)
			(memq 'list-close-comma c-cleanup-list))
		       (and
			(eq last-command-char ?\;)
			(memq 'defun-close-semi c-cleanup-list)))
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n")
		     (eq (char-before) ?}))
		   ;; make sure matching open brace isn't in a comment
		   (not (c-in-literal lim)))
	      (delete-region (point) here))
	  (goto-char (- (point-max) pos)))
	;; re-indent line
	(c-indent-line)
	;; check to see if a newline should be added
	(let ((criteria c-hanging-semi&comma-criteria)
	      answer add-newline-p)
	  (while criteria
	    (setq answer (funcall (car criteria)))
	    ;; only nil value means continue checking
	    (if (not answer)
		(setq criteria (cdr criteria))
	      (setq criteria nil)
	      ;; only 'stop specifically says do not add a newline
	      (setq add-newline-p (not (eq answer 'stop)))
	      ))
	  (if add-newline-p
	      (progn (newline)
		     (c-indent-line)))
	  )))))

(defun c-electric-colon (arg)
  "Insert a colon.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, newlines are inserted before and
after colons based on the value of `c-hanging-colons-alist'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the colon, or
the colon is inserted inside a literal.

This function cleans up double colon scope operators based on the
value of `c-cleanup-list'."
  (interactive "*P")
  (let* ((bod (c-point 'bod))
	 (literal (c-in-literal bod))
	 syntax newlines is-scope-op
	 ;; shut this up
	 (c-echo-syntactic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))
      ;; insert the colon, then do any specified cleanups
      (self-insert-command (prefix-numeric-value arg))
      (let ((pos (- (point-max) (point)))
	    (here (point)))
	(if (and c-auto-newline
		 (memq 'scope-operator c-cleanup-list)
		 (eq (char-before) ?:)
		 (progn
		   (forward-char -1)
		   (skip-chars-backward " \t\n")
		   (eq (char-before) ?:))
		 (not (c-in-literal))
		 (not (eq (char-after (- (point) 2)) ?:)))
	    (progn
	      (delete-region (point) (1- here))
	      (setq is-scope-op t)))
	(goto-char (- (point-max) pos)))
      ;; lets do some special stuff with the colon character
      (setq syntax (c-guess-basic-syntax)
	    ;; some language elements can only be determined by
	    ;; checking the following line.  Lets first look for ones
	    ;; that can be found when looking on the line with the
	    ;; colon
	    newlines
	    (and c-auto-newline
		 (or (c-lookup-lists '(case-label label access-label)
				     syntax c-hanging-colons-alist)
		     (c-lookup-lists '(member-init-intro inher-intro)
				     (let ((buffer-undo-list t))
				       (insert "\n")
				       (unwind-protect
					   (c-guess-basic-syntax)
					 (delete-char -1)))
				     c-hanging-colons-alist))))
      ;; indent the current line
      (c-indent-line syntax)
      ;; does a newline go before the colon?  Watch out for already
      ;; non-hung colons.  However, we don't unhang them because that
      ;; would be a cleanup (and anti-social).
      (if (and (memq 'before newlines)
	       (not is-scope-op)
	       (save-excursion
		 (skip-chars-backward ": \t")
		 (not (bolp))))
	  (let ((pos (- (point-max) (point))))
	    (forward-char -1)
	    (newline)
	    (c-indent-line)
	    (goto-char (- (point-max) pos))))
      ;; does a newline go after the colon?
      (if (and (memq 'after (cdr-safe newlines))
	       (not is-scope-op))
	  (progn
	    (newline)
	    (c-indent-line)))
      )))

(defun c-electric-lt-gt (arg)
  "Insert a less-than, or greater-than character.
The line will be re-indented if the character inserted is the second
of a C++ style stream operator and the buffer is in C++ mode.
Exceptions are when a numeric argument is supplied, or point is inside
a literal, in which case the line will not be re-indented."
  (interactive "*P")
  (let ((indentp (and (not arg)
		      (eq (char-before) last-command-char)
		      (not (c-in-literal))))
	;; shut this up
	(c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-line))))

(defun c-electric-paren (arg)
  "Insert a parenthesis.

If the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, some newline cleanups are done if
appropriate; see the variable `c-cleanup-list'.

Also, the line is re-indented unless a numeric ARG is supplied, there
are non-whitespace characters present on the line after the colon, or
the colon is inserted inside a literal."
  (interactive "*P")
  (let (;; shut this up
	(c-echo-syntactic-information-p nil))
    (if (or arg
	    (not (looking-at "[ \t]*$"))
	    (c-in-literal (c-point 'bod)))
	(self-insert-command (prefix-numeric-value arg))
      ;; do some special stuff with the character
      (let* (;; We want to inhibit blinking the paren since this will
	     ;; be most disruptive.  We'll blink it ourselves
	     ;; afterwards.
	     (old-blink-paren blink-paren-function)
	     blink-paren-function)
	(self-insert-command (prefix-numeric-value arg))
	(c-indent-line)
	(when c-auto-newline
	  ;; Do all appropriate clean ups
	  (let ((here (point))
		(pos (- (point-max) (point)))
		mbeg mend)
	    ;; clean up brace-elseif-brace
	    (if (and (memq 'brace-elseif-brace c-cleanup-list)
		     (eq last-command-char ?\()
		     (re-search-backward "}[ \t\n]*else[ \t\n]+if[ \t\n]*("
					 nil t)
		     (save-excursion
		       (setq mbeg (match-beginning 0)
			     mend (match-end 0))
		       (= mend here))
		     (not (c-in-literal)))
		(progn
		  (delete-region mbeg mend)
		  (insert "} else if (")))
	    ;; clean up brace-catch-brace
	    (if (and (memq 'brace-catch-brace c-cleanup-list)
		     (eq last-command-char ?\()
		     (re-search-backward "}[ \t\n]*catch[ \t\n]*(" nil t)
		     (save-excursion
		       (setq mbeg (match-beginning 0)
			     mend (match-end 0))
		       (= mend here))
		     (not (c-in-literal)))
		(progn
		  (delete-region mbeg mend)
		  (insert "} catch (")))
	    (goto-char (- (point-max) pos))
	    ))
	(funcall old-blink-paren)))))



;; better movement routines for ThisStyleOfVariablesCommonInCPlusPlus
;; originally contributed by Terry_Glanfield.Southern@rxuk.xerox.com
(defun c-forward-into-nomenclature (&optional arg)
  "Move forward to end of a nomenclature section or word.
With arg, to it arg times."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
	(re-search-forward "\\W*\\([A-Z]*[a-z0-9]*\\)" (point-max) t arg)
      (while (and (< arg 0)
		  (re-search-backward
		   "\\(\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\W\\w+\\)"
		   (point-min) 0))
	(forward-char 1)
	(setq arg (1+ arg)))))
  (c-keep-region-active))

(defun c-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.
With optional ARG, move that many times.  If ARG is negative, move
forward."
  (interactive "p")
  (c-forward-into-nomenclature (- arg))
  (c-keep-region-active))

(defun c-scope-operator ()
  "Insert a double colon scope operator at point.
No indentation or other \"electric\" behavior is performed."
  (interactive "*")
  (insert "::"))

(defun c-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of defun.
Returns t unless search stops due to beginning or end of buffer.

Unlike the built-in `beginning-of-defun' this tries to be smarter
about finding the char with open-parenthesis syntax that starts the
defun."
  (interactive "p")
  (if (< arg 0)
      (c-end-of-defun (- arg))
    (while (> arg 0)
      (let ((state (nreverse (c-parse-state)))
	    prevbod bod)
	(while (and state (not bod))
	  (setq bod (car state)
		state (cdr state))
	  (if (consp bod)
	      (setq prevbod (car bod)
		    bod nil)))
	(cond
	 (bod (goto-char bod))
	 (prevbod (goto-char prevbod))
	 (t (goto-char (c-point 'bod)))))
      (setq arg (1- arg))))
  (c-keep-region-active))

(defun c-end-of-defun (&optional arg)
  "Move forward to next end of defun.  With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

An end of a defun occurs right after the close-parenthesis that matches
the open-parenthesis that starts a defun; see `beginning-of-defun'."
  (interactive "p")
  (if (not arg)
      (setq arg 1))
  (if (< arg 0)
      (c-beginning-of-defun (- arg))
    (while (> arg 0)
      ;; skip down into the next defun-block
      (while (and (c-safe (down-list 1) t)
		  (not (eq (char-before) ?{)))
	(forward-char -1)
	(c-forward-sexp))
      (c-beginning-of-defun 1)
      (c-forward-sexp 1)
      (setq arg (1- arg)))
    (forward-line 1))
  (c-keep-region-active))


(defun c-beginning-of-statement (&optional count lim sentence-flag)
  "Go to the beginning of the innermost C statement.
With prefix arg, go back N - 1 statements.  If already at the
beginning of a statement then go to the beginning of the closest
preceding one, moving into nested blocks if necessary (use
\\[backward-sexp] to skip over a block).  If within a comment, or next
to a comment (only whitespace between), move by sentences instead of
statements.

When called from a program, this function takes 3 optional args: the
repetition count, a buffer position limit which is the farthest back
to search, and a flag saying whether to do sentence motion when in a
comment."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     nil t))
  (let* ((count (or count 1))
	 here
	 (range (c-collect-line-comments (c-literal-limits lim))))
    (while (and (/= count 0)
		(or (not lim) (> (point) lim)))
      (setq here (point))
      (if (and (not range) sentence-flag)
	  (save-excursion
	    ;; Find the comment next to point if we're not in one.
	    (if (> count 0)
		;; Finding a comment backwards is a bit cumbersome
		;; because `forward-comment' regards every newline as
		;; a comment when searching backwards (Emacs 19.34).
		(while (and (progn (skip-chars-backward " \t")
				   (setq range (point))
				   (setq range (if (forward-comment -1)
						   (cons (point) range)
						 nil)))
			    (= (char-after) ?\n)))
	      (skip-chars-forward " \t\n")
	      (setq range (point))
	      (setq range (if (forward-comment 1)
			      (cons range (point))
			    nil)))
	    (setq range (c-collect-line-comments range))))
      (if (and (< count 0) (= here (point-max)))
	  ;; Special case because eob might be in a literal.
	  (setq range nil))
      (if range
	  (if (and sentence-flag
		   (/= (char-syntax (char-after (car range))) ?\"))
	      (progn
		;; move by sentence, but not past the limit of the literal
		(save-restriction
		  (narrow-to-region (save-excursion
				      (goto-char (car range))
				      (looking-at comment-start-skip)
				      (goto-char (match-end 0))
				      (point))
				    (save-excursion
				      (goto-char (cdr range))
				      (if (save-excursion
					    (goto-char (car range))
					    (looking-at "/\\*"))
					  (backward-char 2))
				      (skip-chars-backward " \t\n")
				      (point)))
		  (c-safe (forward-sentence (if (> count 0) -1 1))))
		;; See if we should escape the literal.
		(if (> count 0)
		    (if (< (point) here)
			(setq count (1- count))
		      (goto-char (car range))
		      (setq range nil))
		  (if (> (point) here)
		      (setq count (1+ count))
		    (goto-char (cdr range))
		    (setq range nil))))
	    (goto-char (if (> count 0) (car range) (cdr range)))
	    (setq range nil))
	;; Below we do approximately the same as
	;; c-beginning-of-statement-1 and c-end-of-statement-1 and
	;; perhaps they should be changed, but that'd likely break a
	;; lot in cc-engine.
	(goto-char here)
	(if (> count 0)
	    (if (condition-case nil
		    ;; Stop before `{' and after `;', `{', `}' and
		    ;; `};' when not followed by `}' or `)', but on
		    ;; the other side of the syntactic ws.  Also stop
		    ;; before `}', but only to catch comments.  Move
		    ;; by sexps and move into parens.
		    (catch 'done
		      (let (last)
			(while t
			  (setq last (point))
			  (if (and (looking-at "[{}]")
				   (/= here last))
			      (throw 'done (= (char-after) ?{)))
			  (c-backward-syntactic-ws)
			  (cond ((bobp) ; Must handle bob specially.
				 (if (= here last)
				     (if (= last (point-min))
					 (throw 'done t)
				       (goto-char last)
				       (throw 'done nil))
				   (goto-char last)
				   (throw 'done t)))
				((progn (backward-char)
					(looking-at "[;{}]"))
				 (if (or (= here last)
					 (memq (char-after last) '(?\) ?})))
				     (if (and (= (char-before) ?})
					      (= (char-after) ?\;))
					 (backward-char))
				   (goto-char last)
				   (throw 'done t)))
				((= (char-syntax (char-after)) ?\")
				 (forward-char)
				 (c-backward-sexp))
				))))
		  (error
		   (goto-char (point-min))
		   t))
		(setq count (1- count)))
	  (if (condition-case nil
		  ;; Stop before `{' and `}', but on the other side of
		  ;; the syntactic ws, and after `;', `}' and `};'.
		  ;; Only stop before `{' if at top level or inside
		  ;; braces, though.  Also stop after `{', but only to
		  ;; catch comments.  Move by sexps and move into
		  ;; parens.
		  (catch 'done
		    (let (last)
		      (while t
			(setq last (point))
			(c-forward-syntactic-ws)
			(cond ((= (char-after) ?{)
			       (if (or (= here last)
				       (save-excursion
					 (and (c-safe (progn (up-list -1) t))
					      (/= (char-after) ?{))))
				   (progn (forward-char)
					  (throw 'done nil))
				 (goto-char last)
				 (throw 'done t)))
			      ((and (= (char-after) ?})
				    (/= here last))
			       (goto-char last)
			       (throw 'done t))
			      ((looking-at ";\\|};?")
			       (goto-char (match-end 0))
			       (throw 'done t))
			      ((= (char-syntax (char-after)) ?\")
			       (c-forward-sexp))
			      (t
			       (forward-char))
			      ))))
		(error
		 (goto-char (point-max))
		 t))
	      (setq count (1+ count)))))
      ;; If we haven't moved we're near a buffer limit.
      (when (= (point) here)
	(goto-char (if (> count 0) (point-min) (point-max)))
	(setq count 0)))
    ;; its possible we've been left up-buf of lim
    (if lim (goto-char (max (point) lim))))
  (c-keep-region-active))

(defun c-end-of-statement (&optional count lim sentence-flag)
  "Go to the end of the innermost C statement.
With prefix arg, go forward N - 1 statements.  Move forward to the end
of the next statement if already at end, and move into nested blocks
\(use \\[forward-sexp] to skip over a block).  If within a comment, or
next to a comment (only whitespace between), move by sentences instead
of statements.

When called from a program, this function takes 3 optional args: the
repetition count, a buffer position limit which is the farthest back
to search, and a flag saying whether to do sentence motion when in a
comment."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     nil t))
  (c-beginning-of-statement (- (or count 1)) lim sentence-flag)
  (c-keep-region-active))


;; set up electric character functions to work with pending-del,
;; (a.k.a. delsel) mode.  All symbols get the t value except
;; the functions which delete, which gets 'supersede.
(mapcar
 (function
  (lambda (sym)
    (put sym 'delete-selection t)	; for delsel (Emacs)
    (put sym 'pending-delete t)))	; for pending-del (XEmacs)
 '(c-electric-pound
   c-electric-brace
   c-electric-slash
   c-electric-star
   c-electric-semi&comma
   c-electric-lt-gt
   c-electric-colon
   c-electric-paren))
(put 'c-electric-delete    'delete-selection 'supersede) ; delsel
(put 'c-electric-delete    'pending-delete   'supersede) ; pending-del
(put 'c-electric-backspace 'delete-selection 'supersede) ; delsel
(put 'c-electric-backspace 'pending-delete   'supersede) ; pending-del


;; This is used by indent-for-comment to decide how much to indent a
;; comment in C code based on its context.
(defun c-comment-indent ()
  (if (looking-at (concat "^\\(" c-comment-start-regexp "\\)"))
      0				;Existing comment at bol stays there.
    (let ((opoint (point))
	  placeholder)
      (save-excursion
	(beginning-of-line)
	(cond
	 ;; CASE 1: A comment following a solitary close-brace should
	 ;; have only one space.
	 ((looking-at (concat "[ \t]*}[ \t]*\\($\\|"
			      c-comment-start-regexp
			      "\\)"))
	  (search-forward "}")
	  (1+ (current-column)))
	 ;; CASE 2: 2 spaces after #endif
	 ((or (looking-at "^#[ \t]*endif[ \t]*")
	      (looking-at "^#[ \t]*else[ \t]*"))
	  7)
	 ;; CASE 3: when c-indent-comments-syntactically-p is t,
	 ;; calculate the offset according to c-offsets-alist.
	 ;; E.g. identical to hitting TAB.
	 ((and c-indent-comments-syntactically-p
	       (save-excursion
		 (skip-chars-forward " \t")
		 (or (looking-at c-comment-start-regexp)
		     (eolp))))
	  (let ((syntax (c-guess-basic-syntax)))
	    ;; BOGOSITY ALERT: if we're looking at the eol, its
	    ;; because indent-for-comment hasn't put the comment-start
	    ;; in the buffer yet.  this will screw up the syntactic
	    ;; analysis so we kludge in the necessary info.  Another
	    ;; kludge is that if we're at the bol, then we really want
	    ;; to ignore any anchoring as specified by
	    ;; c-comment-only-line-offset since it doesn't apply here.
	    (if (save-excursion
		  (beginning-of-line)
		  (skip-chars-forward " \t")
		  (eolp))
		(c-add-syntax 'comment-intro))
	    (let ((c-comment-only-line-offset
		   (if (consp c-comment-only-line-offset)
		       c-comment-only-line-offset
		     (cons c-comment-only-line-offset
			   c-comment-only-line-offset))))
	      (apply '+ (mapcar 'c-get-offset syntax)))))
	 ;; CASE 4: use comment-column if previous line is a
	 ;; comment-only line indented to the left of comment-column
	 ((save-excursion
	    (beginning-of-line)
	    (and (not (bobp))
		 (forward-line -1))
	    (skip-chars-forward " \t")
	    (prog1
		(looking-at c-comment-start-regexp)
	      (setq placeholder (point))))
	  (goto-char placeholder)
	  (if (< (current-column) comment-column)
	      comment-column
	    (current-column)))
	 ;; CASE 5: If comment-column is 0, and nothing but space
	 ;; before the comment, align it at 0 rather than 1.
	 ((progn
	    (goto-char opoint)
	    (skip-chars-backward " \t")
	    (and (= comment-column 0) (bolp)))
	  0)
	 ;; CASE 6: indent at comment column except leave at least one
	 ;; space.
	 (t (max (1+ (current-column))
		 comment-column))
	 )))))


;; for proposed new variable comment-line-break-function
(defun c-comment-line-break-function (&optional soft)
  ;; we currently don't do anything with soft line breaks
  (let ((literal (c-in-literal))
	at-comment-col)
    (cond
     ((eq literal 'string)
      (insert ?\n))
     ((or (not c-comment-continuation-stars)
	  (not literal))
      (indent-new-comment-line soft))
     (t (let ((here (point))
	      (leader c-comment-continuation-stars))
	  (back-to-indentation)
	  ;; comment could be hanging
	  (if (not (c-in-literal))
	      (progn
		(forward-line 1)
		(forward-comment -1)
		(setq at-comment-col (= (current-column) comment-column))))
	  ;; are we looking at a block or lines style comment?
	  (if (and (looking-at (concat "\\(" c-comment-start-regexp
				       "\\)[ \t]+"))
		   (string-equal (match-string 1) "//"))
	      ;; line style
	      (setq leader (match-string 0)))
	  (goto-char here)
	  (delete-region (progn (skip-chars-backward " \t") (point))
			 (progn (skip-chars-forward " \t") (point)))
	  (newline)
	  ;; to avoid having an anchored comment that c-indent-line will
	  ;; trip up on
	  (insert " " leader)
	  (if at-comment-col
	      (indent-for-comment))
	  (c-indent-line))))))

;; advice for indent-new-comment-line for older Emacsen
(or (boundp 'comment-line-break-function)
    (defadvice indent-new-comment-line (around c-line-break-advice
					       activate preactivate)
      "Calls c-comment-line-break-function if in a comment in CC Mode."
      (if (or (not c-buffer-is-cc-mode)
	      (not (c-in-literal))
	      (not c-comment-continuation-stars))
	  ad-do-it
	(c-comment-line-break-function (ad-get-arg 0)))))

;; used by outline-minor-mode
(defun c-outline-level ()
  ;; This so that `current-column' DTRT in otherwise-hidden text.
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))


(defun c-up-conditional (count)
  "Move back to the containing preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward to the end of the containing preprocessor conditional.
When going backwards, `#elif' is treated like `#else' followed by
`#if'.  When going forwards, `#elif' is ignored."
  (interactive "p")
  (c-forward-conditional (- count) t)
  (c-keep-region-active))

(defun c-backward-conditional (count &optional up-flag)
  "Move back across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move forward across a preprocessor conditional."
  (interactive "p")
  (c-forward-conditional (- count) up-flag)
  (c-keep-region-active))

(defun c-forward-conditional (count &optional up-flag)
  "Move forward across a preprocessor conditional, leaving mark behind.
A prefix argument acts as a repeat count.  With a negative argument,
move backward across a preprocessor conditional."
  (interactive "p")
  (let* ((forward (> count 0))
	 (increment (if forward -1 1))
	 (search-function (if forward 're-search-forward 're-search-backward))
	 (new))
    (save-excursion
      (while (/= count 0)
	(let ((depth (if up-flag 0 -1)) found)
	  (save-excursion
	    ;; Find the "next" significant line in the proper direction.
	    (while (and (not found)
			;; Rather than searching for a # sign that
			;; comes at the beginning of a line aside from
			;; whitespace, search first for a string
			;; starting with # sign.  Then verify what
			;; precedes it.  This is faster on account of
			;; the fastmap feature of the regexp matcher.
			(funcall search-function
				 "#[ \t]*\\(if\\|elif\\|endif\\)"
				 nil t))
	      (beginning-of-line)
	      ;; Now verify it is really a preproc line.
	      (if (looking-at "^[ \t]*#[ \t]*\\(if\\|elif\\|endif\\)")
		  (let ((prev depth))
		    ;; Update depth according to what we found.
		    (beginning-of-line)
		    (cond ((looking-at "[ \t]*#[ \t]*endif")
			   (setq depth (+ depth increment)))
			  ((looking-at "[ \t]*#[ \t]*elif")
			   (if (and forward (= depth 0))
			       (setq found (point))))
			  (t (setq depth (- depth increment))))
		    ;; If we are trying to move across, and we find an
		    ;; end before we find a beginning, get an error.
		    (if (and (< prev 0) (< depth prev))
			(error (if forward
				   "No following conditional at this level"
				 "No previous conditional at this level")))
		    ;; When searching forward, start from next line so
		    ;; that we don't find the same line again.
		    (if forward (forward-line 1))
		    ;; If this line exits a level of conditional, exit
		    ;; inner loop.
		    (if (< depth 0)
			(setq found (point))))
		;; else
		(if forward (forward-line 1))
		)))
	  (or found
	      (error "No containing preprocessor conditional"))
	  (goto-char (setq new found)))
	(setq count (+ count increment))))
    (push-mark)
    (goto-char new))
  (c-keep-region-active))


;; commands to indent lines, regions, defuns, and expressions
(defun c-indent-command (&optional whole-exp)
  "Indent current line as C code, and/or insert some whitespace.

If `c-tab-always-indent' is t, always just indent the current line.
If nil, indent the current line only if point is at the left margin or
in the line's indentation; otherwise insert some whitespace[*].  If
other than nil or t, then some whitespace[*] is inserted only within
literals (comments and strings) and inside preprocessor directives,
but the line is always reindented.

A numeric argument, regardless of its value, means indent rigidly all
the lines of the expression starting after point so that this line
becomes properly indented.  The relative indentation among the lines
of the expression is preserved.

  [*] The amount and kind of whitespace inserted is controlled by the
  variable `c-insert-tab-function', which is called to do the actual
  insertion of whitespace.  Normally the function in this variable
  just inserts a tab character, or the equivalent number of spaces,
  depending on the variable `indent-tabs-mode'."

  (interactive "P")
  (let ((bod (c-point 'bod)))
    (if whole-exp
	;; If arg, always indent this line as C
	;; and shift remaining lines of expression the same amount.
	(let ((shift-amt (c-indent-line))
	      beg end)
	  (save-excursion
	    (if (eq c-tab-always-indent t)
		(beginning-of-line))
	    (setq beg (point))
	    (c-forward-sexp 1)
	    (setq end (point))
	    (goto-char beg)
	    (forward-line 1)
	    (setq beg (point)))
	  (if (> end beg)
	      (indent-code-rigidly beg end (- shift-amt) "#")))
      ;; No arg supplied, use c-tab-always-indent to determine
      ;; behavior
      (cond
       ;; CASE 1: indent when at column zero or in lines indentation,
       ;; otherwise insert a tab
       ((not c-tab-always-indent)
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (not (bolp)))
	    (funcall c-insert-tab-function)
	  (c-indent-line)))
       ;; CASE 2: just indent the line
       ((eq c-tab-always-indent t)
	(c-indent-line))
       ;; CASE 3: if in a literal, insert a tab, but always indent the
       ;; line
       (t
	(if (c-in-literal bod)
	    (funcall c-insert-tab-function))
	(c-indent-line)
	)))))

(defun c-indent-exp (&optional shutup-p)
  "Indent each line in balanced expression following point.
Optional SHUTUP-P if non-nil, inhibits message printing and error checking."
  (interactive "*P")
  (let ((here (point))
	end progress-p)
    (unwind-protect
	(let ((c-echo-syntactic-information-p nil) ;keep quiet for speed
	      (start (progn
		       ;; try to be smarter about finding the range of
		       ;; lines to indent. skip all following
		       ;; whitespace. failing that, try to find any
		       ;; opening brace on the current line
		       (skip-chars-forward " \t\n")
		       (if (memq (char-after) '(?\( ?\[ ?\{))
			   (point)
			 (let ((state (parse-partial-sexp (point)
							  (c-point 'eol))))
			   (and (nth 1 state)
				(goto-char (nth 1 state))
				(memq (char-after) '(?\( ?\[ ?\{))
				(point)))))))
	  ;; find balanced expression end
	  (setq end (and (c-safe (progn (c-forward-sexp 1) t))
			 (point-marker)))
	  ;; sanity check
	  (and (not start)
	       (not shutup-p)
	       (error "Cannot find start of balanced expression to indent."))
	  (and (not end)
	       (not shutup-p)
	       (error "Cannot find end of balanced expression to indent."))
	  (c-progress-init start end 'c-indent-exp)
	  (setq progress-p t)
	  (goto-char start)
	  (beginning-of-line)
	  (while (< (point) end)
	    (if (not (looking-at "[ \t]*$"))
		(c-indent-line))
	    (c-progress-update)
	    (forward-line 1)))
      ;; make sure marker is deleted
      (and end
	   (set-marker end nil))
      (and progress-p
	   (c-progress-fini 'c-indent-exp))
      (goto-char here))))

(defun c-indent-defun ()
  "Re-indents the current top-level function def, struct or class declaration."
  (interactive "*")
  (let ((here (point-marker))
	(c-echo-syntactic-information-p nil)
	(brace (c-least-enclosing-brace (c-parse-state))))
    (goto-char (or brace (c-point 'bod)))
    ;; if we're sitting at b-o-b, it might be because there was no
    ;; least enclosing brace and we were sitting on the defun's open
    ;; brace.
    (if (and (bobp) (not (eq (char-after) ?\{)))
	(goto-char here))
    ;; if defun-prompt-regexp is non-nil, b-o-d might not leave us at
    ;; the open brace. I consider this an Emacs bug.
    (and (boundp 'defun-prompt-regexp)
	 defun-prompt-regexp
	 (looking-at defun-prompt-regexp)
	 (goto-char (match-end 0)))
    ;; catch all errors in c-indent-exp so we can 1. give more
    ;; meaningful error message, and 2. restore point
    (unwind-protect
	(c-indent-exp)
      (goto-char here)
      (set-marker here nil))))

(defun c-indent-region (start end)
  ;; Indent every line whose first char is between START and END inclusive.
  (save-excursion
    (goto-char start)
    ;; Advance to first nonblank line.
    (skip-chars-forward " \t\n")
    (beginning-of-line)
    (let (endmark)
      (unwind-protect
	  (let ((c-tab-always-indent t)
		;; shut up any echo msgs on indiv lines
		(c-echo-syntactic-information-p nil)
		fence)
	    (c-progress-init start end 'c-indent-region)
	    (setq endmark (copy-marker end))
	    (while (and (bolp)
			(not (eobp))
			(< (point) endmark))
	      ;; update progress
	      (c-progress-update)
	      ;; Indent one line as with TAB.
	      (let (nextline sexpend sexpbeg)
		;; skip blank lines
		(skip-chars-forward " \t\n")
		(beginning-of-line)
		;; indent the current line
		(c-indent-line)
		(setq fence (point))
		(if (save-excursion
		      (beginning-of-line)
		      (looking-at "[ \t]*#"))
		    (forward-line 1)
		  (save-excursion
		    ;; Find beginning of following line.
		    (setq nextline (c-point 'bonl))
		    ;; Find first beginning-of-sexp for sexp extending past
		    ;; this line.
		    (beginning-of-line)
		    (while (< (point) nextline)
		      (condition-case nil
			  (progn
			    (c-forward-sexp 1)
			    (setq sexpend (point)))
			(error (setq sexpend nil)
			       (goto-char nextline)))
		      (c-forward-syntactic-ws))
		    (if sexpend
			(progn
			  ;; make sure the sexp we found really starts on the
			  ;; current line and extends past it
			  (goto-char sexpend)
			  (setq sexpend (point-marker))
			  (c-safe (c-backward-sexp 1))
			  (setq sexpbeg (point))))
		    (if (and sexpbeg (< sexpbeg fence))
			(setq sexpbeg fence)))
		  ;; check to see if the next line starts a
		  ;; comment-only line
		  (save-excursion
		    (forward-line 1)
		    (skip-chars-forward " \t")
		    (if (looking-at c-comment-start-regexp)
			(setq sexpbeg (c-point 'bol))))
		  ;; If that sexp ends within the region, indent it all at
		  ;; once, fast.
		  (condition-case nil
		      (if (and sexpend
			       (> sexpend nextline)
			       (<= sexpend endmark))
			  (progn
			    (goto-char sexpbeg)
			    (c-indent-exp 'shutup)
			    (c-progress-update)
			    (goto-char sexpend)))
		    (error
		     (goto-char sexpbeg)
		     (c-indent-line)))
		  ;; Move to following line and try again.
		  (and sexpend
		       (markerp sexpend)
		       (set-marker sexpend nil))
		  (forward-line 1)
		  (setq fence (point))))))
	(set-marker endmark nil)
	(c-progress-fini 'c-indent-region)
	(c-echo-parsing-error)
	))))

(defun c-mark-function ()
  "Put mark at end of current top-level defun, point at beginning."
  (interactive)
  (let ((here (point))
	(eod (c-point 'eod))
	(state (c-parse-state)))
    ;; Are we sitting at the top level, someplace between either the
    ;; beginning of buffer, or the nearest preceding defun?  If so,
    ;; try first to figure out whether we're sitting on the
    ;; introduction to a top-level defun, in which case we want to
    ;; mark the entire defun we're sitting on.
    ;;
    ;; If we're sitting on anything else at the top-level, we want to
    ;; just mark the statement that we're on
    (if (or (and (consp (car state))
		 (= (length state) 1))
	    (null state))
	;; Are we in the whitespace after the nearest preceding defun?
	(if (and state
		 (looking-at "[ \t]*$")
		 (= (save-excursion
		      (c-backward-syntactic-ws)
		      (skip-chars-backward ";")
		      (point))
		    (cdar state)))
	    (progn
	      (setq eod (point))
	      (goto-char (caar state))
	      (c-beginning-of-statement-1))
	  (if (= ?{ (save-excursion
		      (c-end-of-statement-1)
		      (char-before)))
	      ;; We must be in a defuns's introduction
	      (progn
		(c-end-of-statement-1)
		(skip-chars-backward "{")
		(c-beginning-of-statement-1)
		(c-forward-syntactic-ws))
	    ;; Just mark the statement
	    (c-end-of-statement-1)
	    (forward-line 1)
	    (setq eod (point))
	    (c-beginning-of-statement-1)))
      ;; We are inside some enclosing brace structure, so we first
      ;; need to find our way to the least enclosing brace.  Then, in
      ;; both cases, we to mark the region from the beginning of the
      ;; current statement, until the end of the next following defun
      (while (and state)
	(or (consp (car state))
	    (goto-char (car state)))
	(setq state (cdr state)))
      (c-beginning-of-statement-1))
    (push-mark here)
    (push-mark eod nil t)))

(defun c-indent-line-or-region ()
  "When the region is active, indent it.  Otherwise indent the current line."
  ;; Emacs has a variable called mark-active, XEmacs uses region-active-p
  (interactive)
  (if (c-region-is-active-p)
      (c-indent-region (region-beginning) (region-end))
    (c-indent-command)))


;; for progress reporting
(defvar c-progress-info nil)

(defun c-progress-init (start end context)
  (cond
   ;; Be silent
   ((not c-progress-interval))
   ;; Start the progress update messages.  If this Emacs doesn't have
   ;; a built-in timer, just be dumb about it.
   ((not (fboundp 'current-time))
    (message "indenting region... (this may take a while)"))
   ;; If progress has already been initialized, do nothing. otherwise
   ;; initialize the counter with a vector of:
   ;;     [start end lastsec context]
   (c-progress-info)
   (t (setq c-progress-info (vector start
				    (save-excursion
				      (goto-char end)
				      (point-marker))
				    (nth 1 (current-time))
				    context))
      (message "indenting region..."))
   ))

(defun c-progress-update ()
  ;; update progress
  (if (not (and c-progress-info c-progress-interval))
      nil
    (let ((now (nth 1 (current-time)))
	  (start (aref c-progress-info 0))
	  (end (aref c-progress-info 1))
	  (lastsecs (aref c-progress-info 2)))
      ;; should we update?  currently, update happens every 2 seconds,
      ;; what's the right value?
      (if (< c-progress-interval (- now lastsecs))
	  (progn
	    (message "indenting region... (%d%% complete)"
		     (/ (* 100 (- (point) start)) (- end start)))
	    (aset c-progress-info 2 now)))
      )))

(defun c-progress-fini (context)
  ;; finished
  (if (not c-progress-interval)
      nil
    (if (or (eq context (aref c-progress-info 3))
	    (eq context t))
	(progn
	  (set-marker (aref c-progress-info 1) nil)
	  (setq c-progress-info nil)
	  (message "indenting region...done")))))



;;; This page handles insertion and removal of backslashes for C macros.

(defun c-backslash-region (from to delete-flag)
  "Insert, align, or delete end-of-line backslashes on the lines in the region.
With no argument, inserts backslashes and aligns existing backslashes.
With an argument, deletes the backslashes.

This function does not modify blank lines at the start of the region.
If the region ends at the start of a line, it always deletes the
backslash (if any) at the end of the previous line.

You can put the region around an entire macro definition and use this
command to conveniently insert and align the necessary backslashes."
  (interactive "*r\nP")
  (save-excursion
    (goto-char from)
    (let ((column c-backslash-column)
          (endmark (make-marker)))
      (move-marker endmark to)
      ;; Compute the smallest column number past the ends of all the lines.
      (if (not delete-flag)
          (while (< (point) to)
            (end-of-line)
            (if (eq (char-before) ?\\)
                (progn (forward-char -1)
                       (skip-chars-backward " \t")))
            (setq column (max column (1+ (current-column))))
            (forward-line 1)))
      ;; Adjust upward to a tab column, if that doesn't push past the margin.
      (if (> (% column tab-width) 0)
          (let ((adjusted (* (/ (+ column tab-width -1) tab-width) tab-width)))
            (if (< adjusted (window-width))
                (setq column adjusted))))
      ;; Don't modify blank lines at start of region.
      (goto-char from)
      (while (and (< (point) endmark) (eolp))
        (forward-line 1))
      ;; Add or remove backslashes on all the lines.
      (while (< (point) endmark)
	(if (and (not delete-flag)
 		 ;; Un-backslashify the last line
 		 ;; if the region ends right at the start of the next line.
 		 (save-excursion
 		   (forward-line 1)
 		   (< (point) endmark)))
            (c-append-backslash column)
          (c-delete-backslash))
        (forward-line 1))
      (move-marker endmark nil))))

(defun c-append-backslash (column)
  (end-of-line)
  (if (eq (char-before) ?\\)
      (progn (forward-char -1)
             (delete-horizontal-space)
             (indent-to column))
    (indent-to column)
    (insert "\\")))

(defun c-delete-backslash ()
  (end-of-line)
  (or (bolp)
      (progn
 	(forward-char -1)
 	(if (looking-at "\\\\")
 	    (delete-region (1+ (point))
 			   (progn (skip-chars-backward " \t") (point)))))))


(defun c-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handles C and C++ style comments.
If any of the current line is a comment or within a comment,
fill the comment or the paragraph of it that point is in,
preserving the comment indentation or line-starting decorations.

If point is inside multiline string literal, fill it.  This currently
does not respect escaped newlines, except for the special case when it
is the very first thing in the string.  The intended use for this rule
is in situations like the following:

char description[] = \"\\
A very long description of something that you want to fill to make
nicely formatted output.\"\;

If point is in any other situation, i.e. in normal code, do nothing.

Optional prefix ARG means justify paragraph as well."
  (interactive "*P")
  (let* ((point-save (point-marker))
	 limits
	 comment-start-place
	 (first-line
	  ;; Check for obvious entry to comment.
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t")
	    (and (looking-at comment-start-skip)
		 (setq comment-start-place (point)))))
	 (re1 "\\|\\([ \t]*/\\*[ \t]*\\|[ \t]*\\*/[ \t]*\\|[ \t/*]*\\)"))
    (if (save-excursion
	  (beginning-of-line)
	  (looking-at ".*//"))
	(let ((fill-prefix fill-prefix)
	       ;; Lines containing just a comment start or just an end
	       ;; should not be filled into paragraphs they are next
	       ;; to.
	      (paragraph-start (concat paragraph-start re1 "$"))
	      (paragraph-separate (concat paragraph-separate re1 "$")))
	  (save-excursion
	    (beginning-of-line)
	    ;; Move up to first line of this comment.
	    (while (and (not (bobp))
 			(looking-at "[ \t]*//[ \t]*[^ \t\n]"))
	      (forward-line -1))
 	    (if (not (looking-at ".*//[ \t]*[^ \t\n]"))
		(forward-line 1))
	    ;; Find the comment start in this line.
	    (re-search-forward "[ \t]*//[ \t]*")
	    ;; Set the fill-prefix to be what all lines except the first
	    ;; should start with.  But do not alter a user set fill-prefix.
	    (if (null fill-prefix)
		(setq fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))
	    (save-restriction
	      ;; Narrow down to just the lines of this comment.
	      (narrow-to-region (c-point 'bol)
				(save-excursion
				  (forward-line 1)
				  (while
				      (looking-at (regexp-quote fill-prefix))
				    (forward-line 1))
				  (point)))
	      (or (c-safe
		   ;; fill-paragraph sometimes fails to detect when we
		   ;; are between paragraphs.
		   (beginning-of-line)
		   (search-forward fill-prefix (c-point 'eol))
		   (looking-at paragraph-separate))
		  ;; Avoids recursion
		  (let (fill-paragraph-function)
		    (fill-paragraph arg))))))
      ;; else C style comments
      (if (or first-line
	      ;; t if we enter a comment between start of function and
	      ;; this line.
	      (save-excursion
		(setq limits (c-literal-limits))
		(and (consp limits)
		     (save-excursion
		       (goto-char (car limits))
		       (looking-at c-comment-start-regexp))))
	      ;; t if this line contains a comment starter.
	      (setq first-line
		    (save-excursion
		      (beginning-of-line)
		      (prog1
			  (re-search-forward comment-start-skip
					     (save-excursion (end-of-line)
							     (point))
					     t)
			(setq comment-start-place (point)))))
	      ;; t if we're in the whitespace after a comment ender
	      ;; which ends its line.
	      (and (not limits)
		   (when (and (looking-at "[ \t]*$")
			      (save-excursion
				(beginning-of-line)
				(looking-at ".*\\*/[ \t]*$")))
		     (save-excursion
		       (forward-comment -1)
		       (setq comment-start-place (point)))
		     t)))
	  ;; Inside a comment: fill one comment paragraph.
	  (let ((fill-prefix
		 (or
		  ;; Keep user set fill prefix if any.
		  fill-prefix
		  ;; The prefix for each line of this paragraph
		  ;; is the appropriate part of the start of this line,
		  ;; up to the column at which text should be indented.
		  (save-excursion
		    (beginning-of-line)
		    (if (looking-at ".*/\\*.*\\*/")
			(progn (re-search-forward comment-start-skip)
			       (make-string (current-column) ?\ ))
		      (if first-line
			  (forward-line 1)
			(if (and (looking-at "[ \t]*\\*/")
				 (not (save-excursion
					(forward-line -1)
					(looking-at ".*/\\*"))))
			    (forward-line -1)))

		      (let ((line-width (progn (end-of-line)
					       (current-column))))
			(beginning-of-line)
			(prog1
			    (buffer-substring
			     (point)

			     ;; How shall we decide where the end of the
			     ;; fill-prefix is?
			     (progn
			       (skip-chars-forward " \t*" (c-point 'eol))
			       ;; kludge alert, watch out for */, in
			       ;; which case fill-prefix should *not*
			       ;; be "*"!
			       (if (and (eq (char-after) ?/)
					(eq (char-before) ?*))
				   (forward-char -1))
			       (point)))

			  ;; If the comment is only one line followed
			  ;; by a blank line, calling move-to-column
			  ;; above may have added some spaces and tabs
			  ;; to the end of the line; the fill-paragraph
			  ;; function will then delete it and the
			  ;; newline following it, so we'll lose a
			  ;; blank line when we shouldn't.  So delete
			  ;; anything move-to-column added to the end
			  ;; of the line.  We record the line width
			  ;; instead of the position of the old line
			  ;; end because move-to-column might break a
			  ;; tab into spaces, and the new characters
			  ;; introduced there shouldn't be deleted.

			  ;; If you can see a better way to do this,
			  ;; please make the change.  This seems very
			  ;; messy to me.
			  (delete-region (progn (move-to-column line-width)
						(point))
					 (progn (end-of-line) (point)))))))))

		;; Lines containing just a comment start or just an end
		;; should not be filled into paragraphs they are next
		;; to.
		(paragraph-start (if (c-major-mode-is 'java-mode)
				     (concat paragraph-start
					     re1 "\\("
					     c-Java-javadoc-paragraph-start
					     "\\|$\\)")
				   (concat paragraph-start re1 "$")))
		(paragraph-separate (concat paragraph-separate re1 "$"))
		(chars-to-delete 0)
		)
	    (save-restriction
	      ;; Don't fill the comment together with the code
	      ;; following it.  So temporarily exclude everything
	      ;; before the comment start, and everything after the
	      ;; line where the comment ends.  If comment-start-place
	      ;; is non-nil, the comment starter is there.  Otherwise,
	      ;; point is inside the comment.
	      (narrow-to-region (save-excursion
				  (if comment-start-place
				      (goto-char comment-start-place)
				    (search-backward "/*"))
				  (if (and (not c-hanging-comment-starter-p)
					   (looking-at
					    (concat c-comment-start-regexp
						    "[ \t]*$")))
				      (forward-line 1))
				  ;; Protect text before the comment
				  ;; start by excluding it.  Add
				  ;; spaces to bring back proper
				  ;; indentation of that point.
				  (let ((column (current-column)))
				    (prog1 (point)
				      (setq chars-to-delete column)
				      (insert-char ?\  column))))
				(save-excursion
				  (if comment-start-place
				      (goto-char (+ comment-start-place 2)))
				  (search-forward "*/" nil 'move)
				  (if (and (not c-hanging-comment-ender-p)
					   (save-excursion
					     (beginning-of-line)
					     (looking-at "[ \t]*\\*/")))
				      (beginning-of-line)
				    (forward-line 1))
				  (point)))
	      (or (c-safe
		   ;; fill-paragraph sometimes fails to detect when we
		   ;; are between paragraphs.
		   (beginning-of-line)
		   (search-forward fill-prefix (c-point 'eol))
		   (looking-at paragraph-separate))
		  ;; Avoids recursion
		  (let (fill-paragraph-function)
		    (fill-paragraph arg)))
	      (save-excursion
		;; Delete the chars we inserted to avoid clobbering
		;; the stuff before the comment start.
		(goto-char (point-min))
		(if (> chars-to-delete 0)
		    (delete-region (point) (+ (point) chars-to-delete)))
		;; Find the comment ender (should be on last line of
		;; buffer, given the narrowing) and don't leave it on
		;; its own line, unless that's the style that's desired.
		(goto-char (point-max))
		(forward-line -1)
		(search-forward "*/" nil 'move)
		(beginning-of-line)
		(if (and c-hanging-comment-ender-p
			 (looking-at "[ \t]*\\*/"))
		    ;(delete-indentation)))))
		    (let ((fill-column (+ fill-column 9999)))
		      (forward-line -1)
		      (fill-region-as-paragraph (point) (point-max))
		      ;; If fill-prefix ended with a `*', it may be
		      ;; taken away from the comment ender.  We got to
		      ;; check this and put it back if that is the
		      ;; case.
		      (goto-char (- (point-max) 2))
		      (if (not (= (char-before) ?*))
			  (insert ?*))
		      )))))
	;; Else maybe a string.  Fill it if it's a multiline string.
	;; FIXME: This currently doesn't handle escaped newlines.
	;; Doing that correctly is a bit tricky.
	(if (and limits
		 (eq (char-syntax (char-after (car limits))) ?\")
		 (save-excursion
		   (goto-char (car limits))
		   (end-of-line)
		   (< (point) (cdr limits))))
	    (let (fill-paragraph-function)
	      (save-restriction
		(narrow-to-region (save-excursion
				    (goto-char (1+ (car limits)))
				    (if (looking-at "\\\\$")
					;; Some DWIM: Leave the start
					;; line if it's nothing but an
					;; escaped newline.
					(1+ (match-end 0))
				      (point)))
				  (save-excursion
				    (goto-char (1- (cdr limits)))
				    ;; Inserting a newline and
				    ;; removing it again after
				    ;; fill-paragraph makes it more
				    ;; predictable.
				    (insert ?\n)
				    (point)))
		;; Do not compensate for the narrowed column.  This
		;; way the literal will always be filled at the same
		;; column internally.
		(fill-paragraph arg)
		(goto-char (1- (point-max)))
		(delete-char 1)))
	  )))
    (goto-char (marker-position point-save))
    (set-marker point-save nil)
    ;; Always return t.  This has the effect that if filling isn't
    ;; done above, it isn't done at all, and it's therefore
    ;; effectively disabled in normal code.
    t))


(provide 'cc-cmds)
;;; cc-cmds.el ends here
