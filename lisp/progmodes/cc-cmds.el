;;; cc-cmds.el --- user level commands for CC Mode 

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:    1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: cc-mode-help@python.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    5.12
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
  (interactive "P")
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

If `delete-key-deletes-forward' is non-nil, then deletion occurs in
the forward direction.  So if `c-hungry-delete-key' is non-nil, as
evidenced by the \"/h\" or \"/ah\" string on the mode line, then all
following whitespace is consumed.  If however an ARG is supplied, or
`c-hungry-delete-key' is nil, or point is inside a literal then the
function in the variable `c-delete-function' is called."
  (interactive "P")
  (if (and (boundp 'delete-key-deletes-forward)
	   delete-key-deletes-forward)
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
  (interactive "P")
  (if (or (c-in-literal)
	  arg
	  (not (memq 'alignleft c-electric-pound-behavior)))
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
the brace is inserted inside a literal."
  (interactive "P")
  (let* ((c-state-cache (c-parse-state))
	 (safepos (c-safe-position (point) c-state-cache))
	 (literal (c-in-literal safepos)))
    ;; if we're in a literal, or we're not at the end of the line, or
    ;; a numeric arg is provided, or auto-newlining is turned off,
    ;; then just insert the character.
    (if (or literal arg
;	    (not c-auto-newline)
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))
      (let* ((syms '(class-open class-close defun-open defun-close 
		     inline-open inline-close brace-list-open brace-list-close
		     brace-list-intro brace-list-entry block-open block-close
		     substatement-open statement-case-open
		     extern-lang-open extern-lang-close))
	    ;; we want to inhibit blinking the paren since this will
	    ;; be most disruptive. we'll blink it ourselves later on
	    (old-blink-paren blink-paren-function)
	    blink-paren-function
	    (insertion-point (point))
	    delete-temp-newline
	    (preserve-p (eq 32 (char-syntax (char-before))))
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
		       (or (c-lookup-lists syms syntax c-hanging-braces-alist)
			   '(ignore before after)))))
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
		  (here (point))
		  (c-state-cache c-state-cache))
	      (forward-line -1)
	      ;; we may need to update the cache. this should still be
	      ;; faster than recalculating the state in many cases
	      (save-excursion
		(save-restriction
		  (narrow-to-region here (point))
		  (if (and (c-safe (progn (backward-up-list -1) t))
			   (memq (char-before) '(?\) ?}))
			   (progn (widen)
				  (c-safe (progn (forward-sexp -1) t))))
		      (setq c-state-cache
			    (c-hack-state (point) 'open c-state-cache))
		    (if (and (car c-state-cache)
			     (not (consp (car c-state-cache)))
			     (<= (point) (car c-state-cache)))
			(setq c-state-cache (cdr c-state-cache))
		      ))))
	      (let ((here (point))
		    (shift (c-indent-line)))
		(setq c-state-cache (c-adjust-state (c-point 'bol) here
						    (- shift) c-state-cache)))
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
	(let ((here (point))
	      (shift (c-indent-line syntax)))
	  (setq c-state-cache (c-adjust-state (c-point 'bol) here
					      (- shift) c-state-cache)))
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
	  ;; clean up brace-elseif-brace
	  (if (and c-auto-newline
		   (memq 'brace-elseif-brace c-cleanup-list)
		   (eq last-command-char ?\{)
		   (re-search-backward "}[ \t\n]*else[ \t\n]+if[ \t\n]*" nil t)
		   (save-excursion
		     (goto-char (match-end 0))
		     (c-safe (forward-sexp 1))
		     (skip-chars-forward " \t\n")
		     (setq mbeg (match-beginning 0)
			   mend (match-end 0))
		     (= here (1+ (point))))
		   (not (c-in-literal)))
	      (progn
		(delete-region mbeg mend)
		(insert "} else if ")))
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
If slash is second of a double-slash C++ style comment introducing
construct, and we are on a comment-only-line, indent line as comment.
If numeric ARG is supplied or point is inside a literal, indentation
is inhibited."
  (interactive "P")
  (let ((indentp (and (not arg)
		      (eq (char-before) ?/)
		      (eq last-command-char ?/)
		      (not (c-in-literal))))
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
  (interactive "P")
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
non-whitespace characters on the line following the semicolon."
  (interactive "P")
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
      ;; do all cleanups, reindentations, and newline insertions, but
      ;; only if c-auto-newline is turned on
      (if (not c-auto-newline) nil
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
  (interactive "P")
  (let* ((bod (c-point 'bod))
	 (literal (c-in-literal bod))
	 syntax newlines
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
	    (delete-region (point) (1- here)))
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
				     (prog2
					 (insert "\n")
					 (c-guess-basic-syntax)
				       (delete-char -1))
				     c-hanging-colons-alist))))
      ;; indent the current line
      (c-indent-line syntax)
      ;; does a newline go before the colon?  Watch out for already
      ;; non-hung colons.  However, we don't unhang them because that
      ;; would be a cleanup (and anti-social).
      (if (and (memq 'before newlines)
	       (save-excursion
		 (skip-chars-backward ": \t")
		 (not (bolp))))
	  (let ((pos (- (point-max) (point))))
	    (forward-char -1)
	    (newline)
	    (c-indent-line)
	    (goto-char (- (point-max) pos))))
      ;; does a newline go after the colon?
      (if (memq 'after (cdr-safe newlines))
	  (progn
	    (newline)
	    (c-indent-line)))
      )))

(defun c-electric-lt-gt (arg)
  "Insert a less-than, or greater-than character.
When the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, the line will be re-indented if
the character inserted is the second of a C++ style stream operator
and the buffer is in C++ mode.

The line will also not be re-indented if a numeric argument is
supplied, or point is inside a literal."
  (interactive "P")
  (let ((indentp (and (not arg)
		      (eq (char-before) last-command-char)
		      (not (c-in-literal))))
	;; shut this up
	(c-echo-syntactic-information-p nil))
    (self-insert-command (prefix-numeric-value arg))
    (if indentp
	(c-indent-line))))



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
  (interactive)
  (insert "::"))


(defun c-beginning-of-statement (&optional count lim sentence-flag)
  "Go to the beginning of the innermost C statement.
With prefix arg, go back N - 1 statements.  If already at the
beginning of a statement then go to the beginning of the preceding
one.  If within a string or comment, or next to a comment (only
whitespace between), move by sentences instead of statements.

When called from a program, this function takes 3 optional args: the
repetition count, a buffer position limit which is the farthest back
to search, and a flag saying whether to do sentence motion when in a
comment."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     nil t))
  (let ((here (point))
	(count (or count 1))
	(lim (or lim (c-point 'bod)))
	state)
    (save-excursion
      (goto-char lim)
      (setq state (parse-partial-sexp (point) here nil nil)))
    (if (and sentence-flag
	     (or (nth 3 state)
		 (nth 4 state)
;		 (looking-at (concat "[ \t]*" comment-start-skip))
		 (save-excursion
		   (skip-chars-backward " \t")
		   (goto-char (- (point) 2))
		   (looking-at "\\*/"))))
	(forward-sentence (- count))
      (while (> count 0)
	(c-beginning-of-statement-1 lim)
	(setq count (1- count)))
      (while (< count 0)
	(c-end-of-statement-1)
	(setq count (1+ count))))
    ;; its possible we've been left up-buf of lim
    (goto-char (max (point) lim))
    )
  (c-keep-region-active))

(defun c-end-of-statement (&optional count lim sentence-flag)
  "Go to the end of the innermost C statement.

With prefix arg, go forward N - 1 statements.  Move forward to end of
the next statement if already at end.  If within a string or comment,
move by sentences instead of statements.

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
;; c-electric-delete which gets 'supersede.
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
   c-electric-colon))
(put 'c-electric-delete 'delete-selection 'supersede) ; delsel
(put 'c-electric-delete 'pending-delete   'supersede) ; pending-del


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
	 ;; CASE 3: when comment-column is nil, calculate the offset
	 ;; according to c-offsets-alist.  E.g. identical to hitting
	 ;; TAB.
	 ((and c-indent-comments-syntactically-p
	       (save-excursion
		 (skip-chars-forward " \t")
		 (or (looking-at comment-start)
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

;; used by outline-minor-mode
(defun c-outline-level ()
  (save-excursion
    (skip-chars-forward "\t ")
    (current-column)))


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
of the expression are preserved.

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
	    (forward-sexp 1)
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
  (interactive "P")
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
	  (setq end (and (c-safe (progn (forward-sexp 1) t))
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
  (interactive)
  (let ((here (point-marker))
	(c-echo-syntactic-information-p nil)
	(brace (c-least-enclosing-brace (c-parse-state))))
    (if brace
	(goto-char brace)
      (beginning-of-defun))
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
			    (forward-sexp 1)
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
			  (c-safe (backward-sexp 1))
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
  "Put mark at end of a C, C++, or Objective-C defun, point at beginning."
  (interactive)
  (let ((here (point))
	;; there should be a c-point position for 'eod
	(eod  (save-excursion (end-of-defun) (point)))
	(state (c-parse-state))
	brace)
    (while state
      (setq brace (car state))
      (if (consp brace)
	  (goto-char (cdr brace))
	(goto-char brace))
      (setq state (cdr state)))
    (if (eq (char-after) ?{)
	(progn
	  (forward-line -1)
	  (while (not (or (bobp)
			  (looking-at "[ \t]*$")))
	    (forward-line -1)))
      (forward-line 1)
      (skip-chars-forward " \t\n"))
    (push-mark here)
    (push-mark eod nil t)))


;; for progress reporting
(defvar c-progress-info nil)

(defun c-progress-init (start end context)
  ;; start the progress update messages.  if this emacs doesn't have a
  ;; built-in timer, just be dumb about it
  (if (not (fboundp 'current-time))
      (message "indenting region... (this may take a while)")
    ;; if progress has already been initialized, do nothing. otherwise
    ;; initialize the counter with a vector of:
    ;; [start end lastsec context]
    (if c-progress-info
	()
      (setq c-progress-info (vector start
				    (save-excursion
				      (goto-char end)
				      (point-marker))
				    (nth 1 (current-time))
				    context))
      (message "indenting region..."))))

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
  (if (or (eq context (aref c-progress-info 3))
	  (eq context t))
      (progn
	(set-marker (aref c-progress-info 1) nil)
	(setq c-progress-info nil)
	(message "indenting region...done"))))



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
  (interactive "r\nP")
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
      (move-marker endmark nil)))
  (c-keep-region-active))

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

Optional prefix ARG means justify paragraph as well."
  (interactive "P")
  (let* (comment-start-place
	 (first-line
	  ;; Check for obvious entry to comment.
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t\n")
	    (and (looking-at comment-start-skip)
		 (setq comment-start-place (point)))))
	 (re1 "\\|[ \t]*/\\*[ \t]*$\\|[ \t]*\\*/[ \t]*$\\|[ \t/*]*$"))
    (if (and c-double-slash-is-comments-p
	     (save-excursion
	       (beginning-of-line)
	       (looking-at ".*//")))
	(let ((fill-prefix fill-prefix)
	       ;; Lines containing just a comment start or just an end
	       ;; should not be filled into paragraphs they are next
	       ;; to.
	      (paragraph-start (concat paragraph-start re1))
	      (paragraph-separate (concat paragraph-separate re1)))
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
				  (while (looking-at fill-prefix)
				    (forward-line 1))
				  (point)))
	      (fill-paragraph arg)
	      t)))
      ;; else C style comments
      (if (or first-line
	      ;; t if we enter a comment between start of function and
	      ;; this line.
	      (eq (c-in-literal) 'c)
	      ;; t if this line contains a comment starter.
	      (setq first-line
		    (save-excursion
		      (beginning-of-line)
		      (prog1
			  (re-search-forward comment-start-skip
					     (save-excursion (end-of-line)
							     (point))
					     t)
			(setq comment-start-place (point))))))
	  ;; Inside a comment: fill one comment paragraph.
	  (let ((fill-prefix
		 ;; The prefix for each line of this paragraph
		 ;; is the appropriate part of the start of this line,
		 ;; up to the column at which text should be indented.
		 (save-excursion
		   (beginning-of-line)
		   (if (looking-at "[ \t]*/\\*.*\\*/")
		       (progn (re-search-forward comment-start-skip)
			      (make-string (current-column) ?\ ))
		     (if first-line (forward-line 1))

		     (let ((line-width (progn (end-of-line) (current-column))))
		       (beginning-of-line)
		       (prog1
			   (buffer-substring
			    (point)

			    ;; How shall we decide where the end of the
			    ;; fill-prefix is?
			    (progn
			      (beginning-of-line)
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
					(progn (end-of-line) (point))))))))

		;; Lines containing just a comment start or just an end
		;; should not be filled into paragraphs they are next
		;; to.
		(paragraph-start (concat paragraph-start re1))
		(paragraph-separate (concat paragraph-separate re1))
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
				  (forward-line 1)
				  (point)))
	      (fill-paragraph arg)
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
		      (fill-region-as-paragraph (point) (point-max))))))
	    t)))))


(provide 'cc-cmds)
;;; cc-cmds.el ends here
