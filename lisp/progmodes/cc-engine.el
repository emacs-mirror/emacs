;;; cc-engine.el --- core syntax guessing engine for CC mode

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:    1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: cc-mode-help@python.org
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


;; KLUDGE ALERT: c-maybe-labelp is used to pass information between
;; c-crosses-statement-barrier-p and c-beginning-of-statement-1.  A
;; better way should be implemented, but this will at least shut up
;; the byte compiler.
(defvar c-maybe-labelp nil)

;; WARNING WARNING WARNING
;;
;; Be *exceptionally* careful about modifications to this function!
;; Much of CC Mode depends on this Doing The Right Thing.  If you
;; break it you will be sorry.  If you think you know how this works,
;; you probably don't.  No human on Earth does! :-)
;;
;; WARNING WARNING WARNING

(defun c-beginning-of-statement-1 (&optional lim)
  ;; move to the start of the current statement, or the previous
  ;; statement if already at the beginning of one.
  (let ((firstp t)
	(substmt-p t)
	donep c-in-literal-cache saved
	(last-begin (point)))
    ;; first check for bare semicolon
    (if (and (progn (c-backward-syntactic-ws lim)
		    (eq (char-before) ?\;))
	     (c-safe (progn (forward-char -1)
			    (setq saved (point))
			    t))
	     (progn (c-backward-syntactic-ws lim)
		    (memq (char-before) '(?\; ?{ ?:)))
	     )
	(setq last-begin saved)
      (goto-char last-begin)
      (while (not donep)
	;; stop at beginning of buffer
	(if (bobp) (setq donep t)
	  ;; go backwards one balanced expression, but be careful of
	  ;; unbalanced paren being reached
	  (if (not (c-safe (progn (backward-sexp 1) t)))
	      (progn
		(if firstp
		    (backward-up-list 1)
		  (goto-char last-begin))
		;; skip over any unary operators, or other special
		;; characters appearing at front of identifier
		(save-excursion
		  (c-backward-syntactic-ws lim)
		  (skip-chars-backward "-+!*&:.~ \t\n")
		  (if (eq (char-before) ?\()
		      (setq last-begin (point))))
		(goto-char last-begin)
		(setq last-begin (point)
		      donep t)))

	  (setq c-maybe-labelp nil)
	  ;; see if we're in a literal. if not, then this bufpos may be
	  ;; a candidate for stopping
	  (cond
	   ;; CASE 0: did we hit the error condition above?
	   (donep)
	   ;; CASE 1: are we in a literal?
	   ((eq (c-in-literal lim) 'pound)
	    (beginning-of-line))
	   ;; CASE 2: some other kind of literal?
	   ((c-in-literal lim))
	   ;; CASE 3: are we looking at a conditional keyword?
	   ((or (looking-at c-conditional-key)
		(and (eq (char-after) ?\()
		     (save-excursion
		       (forward-sexp 1)
		       (c-forward-syntactic-ws)
		       (not (eq (char-after) ?\;)))
		     (let ((here (point))
			   (foundp (progn
				     (c-backward-syntactic-ws lim)
				     (forward-word -1)
				     (and lim
					  (<= lim (point))
					  (not (c-in-literal lim))
					  (not (eq (char-before) ?_))
					  (looking-at c-conditional-key)
					  ))))
		       ;; did we find a conditional?
		       (if (not foundp)
			   (goto-char here))
		       foundp)))
	    ;; are we in the middle of an else-if clause?
	    (if (save-excursion
		  (and (not substmt-p)
		       (c-safe (progn (forward-sexp -1) t))
		       (looking-at "\\<else\\>[ \t\n]+\\<if\\>")
		       (not (c-in-literal lim))))
		(progn
		  (forward-sexp -1)
		  (c-backward-to-start-of-if lim)))
	    ;; are we sitting at an else clause, that we are not a
	    ;; substatement of?
	    (if (and (not substmt-p)
		     (looking-at "\\<else\\>[^_]"))
		(c-backward-to-start-of-if lim))
	    ;; are we sitting at the while of a do-while?
	    (if (and (looking-at "\\<while\\>[^_]")
		     (c-backward-to-start-of-do lim))
		(setq substmt-p nil))
	    (setq last-begin (point)
		  donep substmt-p))
	   ;; CASE 4: are we looking at a label?
	   ((looking-at c-label-key))
	   ;; CASE 5: is this the first time we're checking?
	   (firstp (setq firstp nil
			 substmt-p (not (c-crosses-statement-barrier-p
					 (point) last-begin))
			 last-begin (point)))
	   ;; CASE 6: have we crossed a statement barrier?
	   ((c-crosses-statement-barrier-p (point) last-begin)
	    (setq donep t))
	   ;; CASE 7: ignore labels
	   ((and c-maybe-labelp
		 (or (and c-access-key (looking-at c-access-key))
		     ;; with switch labels, we have to go back further
		     ;; to try to pick up the case or default
		     ;; keyword. Potential bogosity alert: we assume
		     ;; `case' or `default' is first thing on line
		     (let ((here (point)))
		       (beginning-of-line)
		       (c-forward-syntactic-ws)
		       (if (looking-at c-switch-label-key)
			   t
			 (goto-char here)
			 nil))
		     (looking-at c-label-key))))
	   ;; CASE 8: ObjC or Java method def
	   ((and c-method-key
		 (setq last-begin (c-in-method-def-p)))
	    (setq donep t))
	   ;; CASE 9: nothing special
	   (t (setq last-begin (point)))
	   ))))
    (goto-char last-begin)
    ;; we always do want to skip over non-whitespace modifier
    ;; characters that didn't get skipped above
    (skip-chars-backward "-+!*&:.~" (c-point 'boi))))

(defun c-end-of-statement-1 ()
  (condition-case nil
      (let (beg end found)
	(while (and (not (eobp))
		    (progn
		      (setq beg (point))
		      (forward-sexp 1)
		      (setq end (point))
		      (goto-char beg)
		      (setq found nil)
		      (while (and (not found)
				  (re-search-forward "[;{}]" end t))
			(if (not (c-in-literal beg))
			    (setq found t)))
		      (not found)))
	  (goto-char end))
	(re-search-backward "[;{}]")
	(forward-char 1))
    (error
     (let ((beg (point)))
       (c-safe (backward-up-list -1))
       (let ((end (point)))
	 (goto-char beg)
	 (search-forward ";" end 'move)))
     )))


(defun c-crosses-statement-barrier-p (from to)
  ;; Does buffer positions FROM to TO cross a C statement boundary?
  (let ((here (point))
	(lim from)
	crossedp)
    (condition-case ()
	(progn
	  (goto-char from)
	  (while (and (not crossedp)
		      (< (point) to))
	    (skip-chars-forward "^;{}:" to)
	    (if (not (c-in-literal lim))
		(progn
		  (if (memq (char-after) '(?\; ?{ ?}))
		      (setq crossedp t)
		    (if (eq (char-after) ?:)
			(setq c-maybe-labelp t))
		    (forward-char 1))
		  (setq lim (point)))
	      (forward-char 1))))
      (error (setq crossedp nil)))
    (goto-char here)
    crossedp))


;; Skipping of "syntactic whitespace", defined as lexical whitespace,
;; C and C++ style comments, and preprocessor directives.  Search no
;; farther back or forward than optional LIM.  If LIM is omitted,
;; `beginning-of-defun' is used for backward skipping, point-max is
;; used for forward skipping.

(defun c-forward-syntactic-ws (&optional lim)
  ;; Forward skip of syntactic whitespace for Emacs 19.
  (save-restriction
    (let* ((lim (or lim (point-max)))
	   (here lim)
	   (hugenum (point-max)))
      (narrow-to-region lim (point))
      (while (/= here (point))
	(setq here (point))
	(forward-comment hugenum)
	;; skip preprocessor directives
	(if (and (eq (char-after) ?#)
		 (= (c-point 'boi) (point)))
	    (end-of-line)
	  )))))

(defun c-backward-syntactic-ws (&optional lim)
  ;; Backward skip over syntactic whitespace for Emacs 19.
  (save-restriction
    (let* ((lim (or lim (c-point 'bod)))
	   (here lim)
	   (hugenum (- (point-max))))
      (if (< lim (point))
	  (progn
	    (narrow-to-region lim (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment hugenum)
	      (if (eq (c-in-literal lim) 'pound)
		  (beginning-of-line))
	      )))
      )))


;; Return `c' if in a C-style comment, `c++' if in a C++ style
;; comment, `string' if in a string literal, `pound' if on a
;; preprocessor line, or nil if not in a comment at all.  Optional LIM
;; is used as the backward limit of the search.  If omitted, or nil,
;; `beginning-of-defun' is used."

(defun c-in-literal (&optional lim)
  ;; Determine if point is in a C++ literal. we cache the last point
  ;; calculated if the cache is enabled
  (if (and (boundp 'c-in-literal-cache)
	   c-in-literal-cache
	   (= (point) (aref c-in-literal-cache 0)))
      (aref c-in-literal-cache 1)
    (let ((rtn (save-excursion
		 (let* ((lim (or lim (c-point 'bod)))
			(here (point))
			(state (parse-partial-sexp lim (point))))
		   (cond
		    ((nth 3 state) 'string)
		    ((nth 4 state) (if (nth 7 state) 'c++ 'c))
		    ((progn
		       (goto-char here)
		       (beginning-of-line)
		       (looking-at "[ \t]*#"))
		     'pound)
		    (t nil))))))
      ;; cache this result if the cache is enabled
      (and (boundp 'c-in-literal-cache)
	   (setq c-in-literal-cache (vector (point) rtn)))
      rtn)))


;; utilities for moving and querying around syntactic elements
(defvar c-parsing-error nil)

(defun c-parse-state ()
  ;; Finds and records all open parens between some important point
  ;; earlier in the file and point.
  ;;
  ;; if there's a state cache, return it
  (setq c-parsing-error nil)
  (if (boundp 'c-state-cache) c-state-cache
    (let* (at-bob
	   (pos (save-excursion
		  ;; go back 2 bods, but ignore any bogus positions
		  ;; returned by beginning-of-defun (i.e. open paren
		  ;; in column zero)
		  (let ((cnt 2))
		    (while (not (or at-bob (zerop cnt)))
		      (beginning-of-defun)
		      (if (eq (char-after) ?\{)
			  (setq cnt (1- cnt)))
		      (if (bobp)
			  (setq at-bob t))))
		  (point)))
	   (here (save-excursion
		   ;;(skip-chars-forward " \t}")
		   (point)))
	   (last-bod pos) (last-pos pos)
	   placeholder state sexp-end)
      ;; cache last bod position
      (while (catch 'backup-bod
	       (setq state nil)
	       (while (and pos (< pos here))
		 (setq last-pos pos)
		 (if (and (setq pos (c-safe (scan-lists pos 1 -1)))
			  (<= pos here))
		     (progn
		       (setq sexp-end (c-safe (scan-sexps (1- pos) 1)))
		       (if (and sexp-end
				(<= sexp-end here))
			   ;; we want to record both the start and end
			   ;; of this sexp, but we only want to record
			   ;; the last-most of any of them before here
			   (progn
			     (if (eq (char-after (1- pos)) ?\{)
				 (setq state (cons (cons (1- pos) sexp-end)
						   (if (consp (car state))
						       (cdr state)
						     state))))
			     (setq pos sexp-end))
			 ;; we're contained in this sexp so put pos on
			 ;; front of list
			 (setq state (cons (1- pos) state))))
		   ;; something bad happened. check to see if we
		   ;; crossed an unbalanced close brace. if so, we
		   ;; didn't really find the right `important bufpos'
		   ;; so lets back up and try again
		   (if (and (not pos) (not at-bob)
			    (setq placeholder
				  (c-safe (scan-lists last-pos 1 1)))
			    ;;(char-after (1- placeholder))
			    (<= placeholder here)
			    (eq (char-after (1- placeholder)) ?\}))
		       (while t
			 (setq last-bod (c-safe (scan-lists last-bod -1 1)))
			 (if (not last-bod)
			     (progn
			       ;; bogus, but what can we do here?
			       (setq c-parsing-error (1- placeholder))
			       (throw 'backup-bod nil))
			   (setq at-bob (= last-bod (point-min))
				 pos last-bod)
			   (if (= (char-after last-bod) ?\{)
			       (throw 'backup-bod t)))
			 ))		;end-if
		   ))			;end-while
	       nil))
      state)))

(defun c-whack-state (bufpos state)
  ;; whack off any state information that appears on STATE which lies
  ;; after the bounds of BUFPOS.
  (let (newstate car)
    (while state
      (setq car (car state)
	    state (cdr state))
      (if (consp car)
	  ;; just check the car, because in a balanced brace
	  ;; expression, it must be impossible for the corresponding
	  ;; close brace to be before point, but the open brace to be
	  ;; after.
	  (if (<= bufpos (car car))
	      nil			; whack it off
	    ;; its possible that the open brace is before bufpos, but
	    ;; the close brace is after.  In that case, convert this
	    ;; to a non-cons element.
	    (if (<= bufpos (cdr car))
		(setq newstate (append newstate (list (car car))))
	      ;; we know that both the open and close braces are
	      ;; before bufpos, so we also know that everything else
	      ;; on state is before bufpos, so we can glom up the
	      ;; whole thing and exit.
	      (setq newstate (append newstate (list car) state)
		    state nil)))
	(if (<= bufpos car)
	    nil				; whack it off
	  ;; it's before bufpos, so everything else should too
	  (setq newstate (append newstate (list car) state)
		state nil))))
    newstate))

(defun c-hack-state (bufpos which state)
  ;; Using BUFPOS buffer position, and WHICH (must be 'open or
  ;; 'close), hack the c-parse-state STATE and return the results.
  (if (eq which 'open)
      (let ((car (car state)))
	(if (or (null car)
		(consp car)
		(/= bufpos car))
	    (cons bufpos state)
	  state))
    (if (not (eq which 'close))
	(error "c-hack-state, bad argument: %s" which))
    ;; 'close brace
    (let ((car (car state))
	  (cdr (cdr state)))
      (if (consp car)
	  (setq car (car cdr)
		cdr (cdr cdr)))
      ;; TBD: is this test relevant???
      (if (consp car)
	  state				;on error, don't change
	;; watch out for balanced expr already on cdr of list
	(cons (cons car bufpos)
	      (if (consp (car cdr))
		  (cdr cdr) cdr))
	))))

(defun c-adjust-state (from to shift state)
  ;; Adjust all points in state that lie in the region FROM..TO by
  ;; SHIFT amount (as would be returned by c-indent-line).
  (mapcar
   (function
    (lambda (e)
      (if (consp e)
	  (let ((car (car e))
		(cdr (cdr e)))
	    (if (and (<= from car) (< car to))
		(setcar e (+ shift car)))
	    (if (and (<= from cdr) (< cdr to))
		(setcdr e (+ shift cdr))))
	(if (and (<= from e) (< e to))
	    (setq e (+ shift e))))
      e))
   state))


(defun c-beginning-of-inheritance-list (&optional lim)
  ;; Go to the first non-whitespace after the colon that starts a
  ;; multiple inheritance introduction.  Optional LIM is the farthest
  ;; back we should search.
  (let ((lim (or lim (c-point 'bod)))
	(placeholder (progn
		       (back-to-indentation)
		       (point))))
    (c-backward-syntactic-ws lim)
    (while (and (> (point) lim)
		(memq (char-before) '(?, ?:))
		(progn
		  (beginning-of-line)
		  (setq placeholder (point))
		  (skip-chars-forward " \t")
		  (not (looking-at c-class-key))
		  ))
      (c-backward-syntactic-ws lim))
    (goto-char placeholder)
    (skip-chars-forward "^:" (c-point 'eol))))

(defun c-beginning-of-macro (&optional lim)
  ;; Go to the beginning of the macro. Right now we don't support
  ;; multi-line macros too well
  (back-to-indentation))

(defun c-in-method-def-p ()
  ;; Return nil if we aren't in a method definition, otherwise the
  ;; position of the initial [+-].
  (save-excursion
    (beginning-of-line)
    (and c-method-key
	 (looking-at c-method-key)
	 (point))
    ))

(defun c-just-after-func-arglist-p (&optional containing)
  ;; Return t if we are between a function's argument list closing
  ;; paren and its opening brace.  Note that the list close brace
  ;; could be followed by a "const" specifier or a member init hanging
  ;; colon.  Optional CONTAINING is position of containing s-exp open
  ;; brace.  If not supplied, point is used as search start.
  (save-excursion
    (c-backward-syntactic-ws)
    (let ((checkpoint (or containing (point))))
      (goto-char checkpoint)
      ;; could be looking at const specifier
      (if (and (eq (char-before) ?t)
	       (forward-word -1)
	       (looking-at "\\<const\\>"))
	  (c-backward-syntactic-ws)
	;; otherwise, we could be looking at a hanging member init
	;; colon
	(goto-char checkpoint)
	(if (and (eq (char-before) ?:)
		 (progn
		   (forward-char -1)
		   (c-backward-syntactic-ws)
		   (looking-at "[ \t\n]*:\\([^:]+\\|$\\)")))
	    nil
	  (goto-char checkpoint))
	)
      (and (eq (char-before) ?\))
	   ;; check if we are looking at a method def
	   (or (not c-method-key)
	       (progn
		 (forward-sexp -1)
		 (forward-char -1)
		 (c-backward-syntactic-ws)
		 (not (or (memq (char-before) '(?- ?+))
			  ;; or a class category
			  (progn
			    (forward-sexp -2)
			    (looking-at c-class-key))
			  )))))
      )))

;; defuns to look backwards for things
(defun c-backward-to-start-of-do (&optional lim)
  ;; Move to the start of the last "unbalanced" do expression.
  ;; Optional LIM is the farthest back to search.  If none is found,
  ;; nil is returned and point is left unchanged, otherwise t is returned.
  (let ((do-level 1)
	(case-fold-search nil)
	(lim (or lim (c-point 'bod)))
	(here (point))
	foundp)
    (while (not (zerop do-level))
      ;; we protect this call because trying to execute this when the
      ;; while is not associated with a do will throw an error
      (condition-case nil
	  (progn
	    (backward-sexp 1)
	    (cond
	     ((memq (c-in-literal lim) '(c c++)))
	     ((looking-at "while\\b[^_]")
	      (setq do-level (1+ do-level)))
	     ((looking-at "do\\b[^_]")
	      (if (zerop (setq do-level (1- do-level)))
		  (setq foundp t)))
	     ((<= (point) lim)
	      (setq do-level 0)
	      (goto-char lim))))
	(error
	 (goto-char lim)
	 (setq do-level 0))))
    (if (not foundp)
	(goto-char here))
    foundp))

(defun c-backward-to-start-of-if (&optional lim)
  ;; Move to the start of the last "unbalanced" if and return t.  If
  ;; none is found, and we are looking at an if clause, nil is
  ;; returned.  If none is found and we are looking at an else clause,
  ;; an error is thrown.
  (let ((if-level 1)
	(here (c-point 'bol))
	(case-fold-search nil)
	(lim (or lim (c-point 'bod)))
	(at-if (looking-at "if\\b[^_]")))
    (catch 'orphan-if
      (while (and (not (bobp))
		  (not (zerop if-level)))
	(c-backward-syntactic-ws)
	(condition-case nil
	    (backward-sexp 1)
	  (error
	   (if at-if
	       (throw 'orphan-if nil)
	     (error "No matching `if' found for `else' on line %d."
		    (1+ (count-lines 1 here))))))
	(cond
	 ((looking-at "else\\b[^_]")
	  (setq if-level (1+ if-level)))
	 ((looking-at "if\\b[^_]")
	  ;; check for else if... skip over
	  (let ((here (point)))
	    (c-safe (forward-sexp -1))
	    (if (looking-at "\\<else\\>[ \t]+\\<if\\>")
		nil
	      (setq if-level (1- if-level))
	      (goto-char here))))
	 ((< (point) lim)
	  (setq if-level 0)
	  (goto-char lim))
	 ))
      t)))

(defun c-skip-conditional ()
  ;; skip forward over conditional at point, including any predicate
  ;; statements in parentheses. No error checking is performed.
  (forward-sexp (cond
		 ;; else if()
		 ((looking-at "\\<else\\>[ \t]+\\<if\\>") 3)
		 ;; do, else, try, finally
		 ((looking-at "\\<\\(do\\|else\\|try\\|finally\\)\\>") 1)
		 ;; for, if, while, switch, catch, synchronized
		 (t 2))))

(defun c-skip-case-statement-forward (state &optional lim)
  ;; skip forward over case/default bodies, with optional maximal
  ;; limit. if no next case body is found, nil is returned and point
  ;; is not moved
  (let ((lim (or lim (point-max)))
	(here (point))
	donep foundp bufpos
	(safepos (point))
	(balanced (car state)))
    ;; search until we've passed the limit, or we've found our match
    (while (and (< (point) lim)
		(not donep))
      (setq safepos (point))
      ;; see if we can find a case statement, not in a literal
      (if (and (re-search-forward c-switch-label-key lim 'move)
	       (setq bufpos (match-beginning 0))
	       (not (c-in-literal safepos))
	       (/= bufpos here))
	  ;; if we crossed into a balanced sexp, we know the case is
	  ;; not part of our switch statement, so just bound over the
	  ;; sexp and keep looking.
	  (if (and (consp balanced)
		   (> bufpos (car balanced))
		   (< bufpos (cdr balanced)))
	      (goto-char (cdr balanced))
	    (goto-char bufpos)
	    (setq donep t
		  foundp t))))
    (if (not foundp)
	(goto-char here))
    foundp))

(defun c-search-uplist-for-classkey (brace-state)
  ;; search for the containing class, returning a 2 element vector if
  ;; found. aref 0 contains the bufpos of the class key, and aref 1
  ;; contains the bufpos of the open brace.
  (if (null brace-state)
      ;; no brace-state means we cannot be inside a class
      nil
    (let ((carcache (car brace-state))
	  search-start search-end)
      (if (consp carcache)
	  ;; a cons cell in the first element means that there is some
	  ;; balanced sexp before the current bufpos. this we can
	  ;; ignore. the nth 1 and nth 2 elements define for us the
	  ;; search boundaries
	  (setq search-start (nth 2 brace-state)
		search-end (nth 1 brace-state))
	;; if the car was not a cons cell then nth 0 and nth 1 define
	;; for us the search boundaries
	(setq search-start (nth 1 brace-state)
	      search-end (nth 0 brace-state)))
      ;; search-end cannot be a cons cell
      (and (consp search-end)
	   (error "consp search-end: %s" search-end))
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
	    (let ((search-key (concat c-class-key "\\|extern[^_]"))
		  foundp class match-end)
	      (while (and (not foundp)
			  (progn
			    (c-forward-syntactic-ws)
			    (> search-end (point)))
			  (re-search-forward search-key search-end t))
		(setq class (match-beginning 0)
		      match-end (match-end 0))
		(if (c-in-literal search-start)
		    nil			; its in a comment or string, ignore
		  (goto-char class)
		  (skip-chars-forward " \t\n")
		  (setq foundp (vector (c-point 'boi) search-end))
		  (cond
		   ;; check for embedded keywords
		   ((let ((char (char-after (1- class))))
		      (and char
			   (memq (char-syntax char) '(?w ?_))))
		    (goto-char match-end)
		    (setq foundp nil))
		   ;; make sure we're really looking at the start of a
		   ;; class definition, and not a forward decl, return
		   ;; arg, template arg list, or an ObjC or Java method.
		   ((and c-method-key
			 (re-search-forward c-method-key search-end t))
		    (setq foundp nil))
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
		      (skip-chars-forward skipchars search-end)
		      (/= (point) search-end))
		    (setq foundp nil))
		   )))
	      foundp))
	  )))))

(defun c-inside-bracelist-p (containing-sexp brace-state)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren. BRACE-STATE is the remainder of the state of enclosing braces
  ;;
  ;; N.B.: This algorithm can potentially get confused by cpp macros
  ;; places in inconvenient locations.  Its a trade-off we make for
  ;; speed.
  (or
   ;; this will pick up enum lists
   (condition-case ()
       (save-excursion
	 (goto-char containing-sexp)
	 (forward-sexp -1)
	 (if (and (or (looking-at "enum[\t\n ]+")
		      (progn (forward-sexp -1)
			     (looking-at "enum[\t\n ]+")))
		  (progn (c-end-of-statement-1)
			 (> (point) containing-sexp)))
	     (point)))
     (error nil))
   ;; this will pick up array/aggregate init lists, even if they are nested.
   (save-excursion
     (let (bufpos failedp)
       (while (and (not bufpos)
		   containing-sexp)
	 (if (consp containing-sexp)
	     (setq containing-sexp (car brace-state)
		   brace-state (cdr brace-state))
	   ;; see if significant character just before brace is an equal
	   (goto-char containing-sexp)
	   (setq failedp nil)
	   (condition-case ()
	       (progn
		 (forward-sexp -1)
		 (forward-sexp 1)
		 (c-forward-syntactic-ws containing-sexp))
	     (error (setq failedp t)))
	   (if (or failedp (not (eq (char-after) ?=)))
	       ;; lets see if we're nested. find the most nested
	       ;; containing brace
	       (setq containing-sexp (car brace-state)
		     brace-state (cdr brace-state))
	     ;; we've hit the beginning of the aggregate list
	     (c-beginning-of-statement-1 (c-most-enclosing-brace brace-state))
	     (setq bufpos (point)))
	   ))
       bufpos))
   ))


(defun c-most-enclosing-brace (state)
  ;; return the bufpos of the most enclosing brace that hasn't been
  ;; narrowed out by any enclosing class, or nil if none was found
  (let (enclosingp)
    (while (and state (not enclosingp))
      (setq enclosingp (car state)
	    state (cdr state))
      (if (consp enclosingp)
	  (setq enclosingp nil)
	(if (> (point-min) enclosingp)
	    (setq enclosingp nil))
	(setq state nil)))
    enclosingp))

(defun c-least-enclosing-brace (state)
  ;; return the bufpos of the least (highest) enclosing brace that
  ;; hasn't been narrowed out by any enclosing class, or nil if none
  ;; was found.
  (c-most-enclosing-brace (nreverse state)))

(defun c-safe-position (bufpos state)
  ;; return the closest known safe position higher up than point
  (let ((safepos nil))
    (while state
      (setq safepos
	    (if (consp (car state))
		(cdr (car state))
	      (car state)))
      (if (< safepos bufpos)
	  (setq state nil)
	(setq state (cdr state))))
    safepos))

(defun c-narrow-out-enclosing-class (state lim)
  ;; narrow the buffer so that the enclosing class is hidden
  (let (inclass-p)
    (and state
	 (setq inclass-p (c-search-uplist-for-classkey state))
	 (narrow-to-region
	  (progn
	    (goto-char (1+ (aref inclass-p 1)))
	    (skip-chars-forward " \t\n" lim)
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


;; This function implements the main decision tree for determining the
;; syntactic analysis of the current line of code.  Yes, it's huge and
;; bloated!

(defun c-guess-basic-syntax ()
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((indent-point (point))
	     (case-fold-search nil)
	     (fullstate (c-parse-state))
	     (state fullstate)
	     (in-method-intro-p (and (eq major-mode 'objc-mode)
				     c-method-key
				     (looking-at c-method-key)))
	     literal containing-sexp char-before-ip char-after-ip lim
	     syntax placeholder c-in-literal-cache inswitch-p
	     injava-inher
	     ;; narrow out any enclosing class or extern "C" block
	     (inclass-p (c-narrow-out-enclosing-class state indent-point))
	     (inextern-p (and inclass-p
			      (save-excursion
				(save-restriction
				  (widen)
				  (goto-char (aref inclass-p 0))
				  (looking-at "extern[^_]")))))
	     )

	;; get the buffer position of the most nested opening brace,
	;; if there is one, and it hasn't been narrowed out
	(save-excursion
	  (goto-char indent-point)
	  (skip-chars-forward " \t}")
	  (skip-chars-backward " \t")
	  (while (and state
		      (not in-method-intro-p)
		      (not containing-sexp))
	    (setq containing-sexp (car state)
		  state (cdr state))
	    (if (consp containing-sexp)
		;; if cdr == point, then containing sexp is the brace
		;; that opens the sexp we close
		(if (= (cdr containing-sexp) (point))
		    (setq containing-sexp (car containing-sexp))
		  ;; otherwise, ignore this element
		  (setq containing-sexp nil))
	      ;; ignore the bufpos if its been narrowed out by the
	      ;; containing class
	      (if (<= containing-sexp (point-min))
		  (setq containing-sexp nil)))))

	;; set the limit on the farthest back we need to search
	(setq lim (or containing-sexp
		      (if (consp (car fullstate))
			  (cdr (car fullstate))
			nil)
		      (point-min)))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (char-after))
	(c-backward-syntactic-ws lim)
	(setq char-before-ip (char-before))
	(goto-char indent-point)
	(skip-chars-forward " \t")

	;; are we in a literal?
	(setq literal (c-in-literal lim))

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 ((memq literal '(string))
	  (c-add-syntax 'string (c-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
	 ((memq literal '(c c++))
	  ;; we need to catch multi-paragraph C comments
	  (while (and (zerop (forward-line -1))
		      (looking-at "^[ \t]*$")))
	  (c-add-syntax literal (c-point 'boi)))
	 ;; CASE 3: in a cpp preprocessor
	 ((eq literal 'pound)
	  (c-beginning-of-macro lim)
	  (c-add-syntax 'cpp-macro (c-point 'boi)))
	 ;; CASE 4: in an objective-c method intro
	 (in-method-intro-p
	  (c-add-syntax 'objc-method-intro (c-point 'boi)))
	 ;; CASE 5: Line is at top level.
	 ((null containing-sexp)
	  (cond
	   ;; CASE 5A: we are looking at a defun, class, or
	   ;; inline-inclass method opening brace
	   ((eq char-after-ip ?{)
	    (cond
	     ;; CASE 5A.1: extern declaration
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t")
		(and (c-safe (progn (backward-sexp 2) t))
		     (looking-at "extern[^_]")
		     (progn
		       (setq placeholder (point))
		       (forward-sexp 1)
		       (c-forward-syntactic-ws)
		       (eq (char-after) ?\"))))
	      (goto-char placeholder)
	      (c-add-syntax 'extern-lang-open (c-point 'boi)))
	     ;; CASE 5A.2: we are looking at a class opening brace
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		;; TBD: watch out! there could be a bogus
		;; c-state-cache in place when we get here.  we have
		;; to go through much chicanery to ignore the cache.
		;; But of course, there may not be!  BLECH!  BOGUS!
		(let ((decl
		       (if (boundp 'c-state-cache)
			   (let ((old-cache c-state-cache))
			     (prog2
				 (makunbound 'c-state-cache)
				 (c-search-uplist-for-classkey (c-parse-state))
			       (setq c-state-cache old-cache)))
			 (c-search-uplist-for-classkey (c-parse-state))
			 )))
		  (and decl
		       (setq placeholder (aref decl 0)))
		  ))
	      (c-add-syntax 'class-open placeholder))
	     ;; CASE 5A.3: brace list open
	     ((save-excursion
		(c-beginning-of-statement-1 lim)
		;; c-b-o-s could have left us at point-min
		(and (bobp)
		     (c-forward-syntactic-ws indent-point))
		(if (looking-at "typedef[^_]")
		    (progn (forward-sexp 1)
			   (c-forward-syntactic-ws indent-point)))
		(setq placeholder (c-point 'boi))
		(and (or (looking-at "enum[ \t\n]+")
			 (eq char-before-ip ?=))
		     (save-excursion
		       (skip-chars-forward "^;(" indent-point)
		       (not (memq (char-after) '(?\; ?\()))
		       )))
	      (c-add-syntax 'brace-list-open placeholder))
	     ;; CASE 5A.4: inline defun open
	     ((and inclass-p (not inextern-p))
	      (c-add-syntax 'inline-open)
	      (c-add-syntax 'inclass (aref inclass-p 0)))
	     ;; CASE 5A.5: ordinary defun open
	     (t
	      (goto-char placeholder)
	      (c-add-syntax 'defun-open (c-point 'bol))
	      )))
	   ;; CASE 5B: first K&R arg decl or member init
	   ((c-just-after-func-arglist-p)
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
		  (backward-sexp 1))
	      (setq placeholder (point))
	      (save-excursion
		(and (c-safe (backward-sexp 1) t)
		     (looking-at "throw[^_]")
		     (c-safe (backward-sexp 1) t)
		     (setq placeholder (point))))
	      (goto-char placeholder)
	      (c-add-syntax 'member-init-intro (c-point 'boi))
	      ;; we don't need to add any class offset since this
	      ;; should be relative to the ctor's indentation
	      )
	     ;; CASE 5B.2: K&R arg decl intro
	     (c-recognize-knr-p
	      (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
	      (and inclass-p (c-add-syntax 'inclass (aref inclass-p 0))))
	     ;; CASE 5B.3: Nether region after a C++ or Java func
	     ;; decl, which could include a `throws' declaration.
	     (t
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'func-decl-cont (c-point 'boi))
	      )))
	   ;; CASE 5C: inheritance line. could be first inheritance
	   ;; line, or continuation of a multiple inheritance
	   ((or (and c-baseclass-key (looking-at c-baseclass-key))
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
		(and (eq major-mode 'java-mode)
		     (let ((fence (save-excursion
				    (c-beginning-of-statement-1 lim)
				    (point)))
			   cont done)
		       (save-excursion
			 (while (not done)
			   (cond ((looking-at c-Java-special-key)
				  (setq injava-inher (cons cont (point))
					done t))
				 ((or (not (c-safe (forward-sexp -1) t))
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
	      (c-backward-syntactic-ws lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 5C.2: hanging colon on an inher intro
	     ((eq char-before-ip ?:)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      (and inclass-p (c-add-syntax 'inclass (aref inclass-p 0))))
	     ;; CASE 5C.3: in a Java implements/extends
	     (injava-inher
	      (let ((where (cdr injava-inher))
		    (cont (car injava-inher)))
		(goto-char where)
		(cond ((looking-at "throws[ \t\n]")
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
	   ;; CASE 5D: this could be a top-level compound statement or a
	   ;; member init list continuation
	   ((eq char-before-ip ?,)
	    (goto-char indent-point)
	    (c-backward-syntactic-ws lim)
	    (while (and (< lim (point))
			(eq (char-before) ?,))
	      ;; this will catch member inits with multiple
	      ;; line arglists
	      (forward-char -1)
	      (c-backward-syntactic-ws (c-point 'bol))
	      (if (eq (char-before) ?\))
		  (backward-sexp 1))
	      ;; now continue checking
	      (beginning-of-line)
	      (c-backward-syntactic-ws lim))
	    (cond
	     ;; CASE 5D.1: hanging member init colon, but watch out
	     ;; for bogus matches on access specifiers inside classes.
	     ((and (eq (char-before) ?:)
		   (save-excursion
		     (forward-word -1)
		     (not (looking-at c-access-key))))
	      (goto-char indent-point)
	      (c-backward-syntactic-ws lim)
	      (c-safe (backward-sexp 1))
	      (c-add-syntax 'member-init-cont (c-point 'boi))
	      ;; we do not need to add class offset since relative
	      ;; point is the member init above us
	      )
	     ;; CASE 5D.2: non-hanging member init colon
	     ((progn
		(c-forward-syntactic-ws indent-point)
		(eq (char-after) ?:))
	      (skip-chars-forward " \t:")
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5D.3: perhaps a multiple inheritance line?
	     ((looking-at c-inher-key)
	      (c-add-syntax 'inher-cont (c-point 'boi)))
	     ;; CASE 5D.4: perhaps a template list continuation?
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-backward "^<" lim)
		;; not sure if this is the right test, but it should
		;; be fast and mostly accurate.
		(and (eq (char-before) ?<)
		     (not (c-in-literal lim))))
	      ;; we can probably indent it just like an arglist-cont
	      (c-add-syntax 'template-args-cont (point)))
	     ;; CASE 5D.5: perhaps a top-level statement-cont
	     (t
	      (c-beginning-of-statement-1 lim)
	      ;; skip over any access-specifiers
	      (and inclass-p c-access-key
		   (while (looking-at c-access-key)
		     (forward-line 1)))
	      ;; skip over comments, whitespace
	      (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'statement-cont (c-point 'boi)))
	     ))
	   ;; CASE 5E: we are looking at a access specifier
	   ((and inclass-p
		 c-access-key
		 (looking-at c-access-key))
	    (c-add-syntax 'access-label (c-point 'bonl))
	    (c-add-syntax 'inclass (aref inclass-p 0)))
	   ;; CASE 5F: extern-lang-close?
	   ((and inextern-p
		 (eq char-after-ip ?}))
	    (c-add-syntax 'extern-lang-close (aref inclass-p 0)))
	   ;; CASE 5G: we are looking at the brace which closes the
	   ;; enclosing nested class decl
	   ((and inclass-p
		 (eq char-after-ip ?})
		 (save-excursion
		   (save-restriction
		     (widen)
		     (forward-char 1)
		     (and
		      (condition-case nil
			  (progn (backward-sexp 1) t)
			(error nil))
		      (= (point) (aref inclass-p 1))
		      ))))
	    (save-restriction
	      (widen)
	      (goto-char (aref inclass-p 0))
	      (c-add-syntax 'class-close (c-point 'boi))))
	   ;; CASE 5H: we could be looking at subsequent knr-argdecls
	   ((and c-recognize-knr-p
		 ;; here we essentially use the hack that is used in
		 ;; Emacs' c-mode.el to limit how far back we should
		 ;; look.  The assumption is made that argdecls are
		 ;; indented at least one space and that function
		 ;; headers are not indented.
		 (let ((limit (save-excursion
				(re-search-backward "^[^ \^L\t\n#]" nil 'move)
				(point))))
		   (save-excursion
		     (c-backward-syntactic-ws limit)
		     (setq placeholder (point))
		     (while (and (memq (char-before) '(?\; ?,))
				 (> (point) limit))
		       (beginning-of-line)
		       (setq placeholder (point))
		       (c-backward-syntactic-ws limit))
		     (and (eq (char-before) ?\))
			  (or (not c-method-key)
			      (progn
				(forward-sexp -1)
				(forward-char -1)
				(c-backward-syntactic-ws)
				(not (or (memq (char-before) '(?- ?+))
					 ;; or a class category
					 (progn
					   (forward-sexp -2)
					   (looking-at c-class-key))
					 )))))
		     ))
		 (save-excursion
		   (c-beginning-of-statement-1)
		   (not (looking-at "typedef[ \t\n]+"))))
	    (goto-char placeholder)
	    (c-add-syntax 'knr-argdecl (c-point 'boi)))
	   ;; CASE 5I: we are at the topmost level, make sure we skip
	   ;; back past any access specifiers
	   ((progn
	      (c-backward-syntactic-ws lim)
	      (while (and inclass-p
			  c-access-key
			  (not (bobp))
			  (save-excursion
			    (c-safe (progn (backward-sexp 1) t))
			    (looking-at c-access-key)))
		(backward-sexp 1)
		(c-backward-syntactic-ws lim))
	      (or (bobp)
		  (memq (char-before) '(?\; ?\}))))
	    ;; real beginning-of-line could be narrowed out due to
	    ;; enclosure in a class block
	    (save-restriction
	      (widen)
	      (c-add-syntax 'topmost-intro (c-point 'bol))
	      (if inclass-p
		  (progn
		    (goto-char (aref inclass-p 1))
		    (or (= (point) (c-point 'boi))
			(goto-char (aref inclass-p 0)))
		    (if inextern-p
			(c-add-syntax 'inextern-lang)
		      (c-add-syntax 'inclass (c-point 'boi)))))
		))
	   ;; CASE 5J: we are at an ObjC or Java method definition
	   ;; continuation line.
	   ((and c-method-key
		 (progn
		   (c-beginning-of-statement-1 lim)
		   (beginning-of-line)
		   (looking-at c-method-key)))
	    (c-add-syntax 'objc-method-args-cont (point)))
	   ;; CASE 5K: we are at a topmost continuation line
	   (t
	    (c-beginning-of-statement-1 lim)
	    (c-forward-syntactic-ws)
	    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	   ))				; end CASE 5
	 ;; CASE 6: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((not (eq (char-after containing-sexp) ?{))
	  (c-backward-syntactic-ws containing-sexp)
	  (cond
	   ;; CASE 6A: we are looking at the arglist closing paren
	   ((and (not (eq char-before-ip ?,))
		 (memq char-after-ip '(?\) ?\])))
	    (goto-char containing-sexp)
	    (c-add-syntax 'arglist-close (c-point 'boi)))
	   ;; CASE 6B: we are looking at the first argument in an empty
	   ;; argument list. Use arglist-close if we're actually
	   ;; looking at a close paren or bracket.
	   ((memq char-before-ip '(?\( ?\[))
	    (goto-char containing-sexp)
	    (c-add-syntax 'arglist-intro (c-point 'boi)))
	   ;; CASE 6C: we are inside a conditional test clause. treat
	   ;; these things as statements
	   ((save-excursion
	     (goto-char containing-sexp)
	     (and (c-safe (progn (forward-sexp -1) t))
		  (looking-at "\\<for\\>[^_]")))
	    (goto-char (1+ containing-sexp))
	    (c-forward-syntactic-ws indent-point)
	    (c-beginning-of-statement-1 containing-sexp)
	    (if (eq char-before-ip ?\;)
		(c-add-syntax 'statement (point))
	      (c-add-syntax 'statement-cont (point))
	      ))
	   ;; CASE 6D: maybe a continued method call. This is the case
	   ;; when we are inside a [] bracketed exp, and what precede
	   ;; the opening bracket is not an identifier.
	   ((and c-method-key
		 (eq (char-after containing-sexp) ?\[)
		 (save-excursion
		   (goto-char (1- containing-sexp))
		   (c-backward-syntactic-ws (c-point 'bod))
		   (if (not (looking-at c-symbol-key))
		       (c-add-syntax 'objc-method-call-cont containing-sexp))
		   )))
	   ;; CASE 6E: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; mathematical paren groupings, but we could be on a
	   ;; for-list continuation line
	   ((and (save-excursion
		   (goto-char (1+ containing-sexp))
		   (skip-chars-forward " \t")
		   (not (eolp)))
		 (save-excursion
		   (c-beginning-of-statement-1 lim)
		   (skip-chars-backward " \t([")
		   (<= (point) containing-sexp)))
	    (goto-char containing-sexp)
	    (c-add-syntax 'arglist-cont-nonempty (c-point 'boi)))
	   ;; CASE 6F: we are looking at just a normal arglist
	   ;; continuation line
	   (t (c-beginning-of-statement-1 containing-sexp)
	      (forward-char 1)
	      (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'arglist-cont (c-point 'boi)))
	   ))
	 ;; CASE 7: func-local multi-inheritance line
	 ((and c-baseclass-key
	       (save-excursion
		 (goto-char indent-point)
		 (skip-chars-forward " \t")
		 (looking-at c-baseclass-key)))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 7A: non-hanging colon on an inher intro
	   ((eq char-after-ip ?:)
	    (c-backward-syntactic-ws lim)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 7B: hanging colon on an inher intro
	   ((eq char-before-ip ?:)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 7C: a continued inheritance line
	   (t
	    (c-beginning-of-inheritance-list lim)
	    (c-add-syntax 'inher-cont (point))
	    )))
	 ;; CASE 8: we are inside a brace-list
	 ((setq placeholder (c-inside-bracelist-p containing-sexp state))
	  (cond
	   ;; CASE 8A: brace-list-close brace
	   ((and (eq char-after-ip ?})
		 (c-safe (progn (forward-char 1)
				(backward-sexp 1)
				t))
		 (= (point) containing-sexp))
	    (c-add-syntax 'brace-list-close (c-point 'boi)))
	   ;; CASE 8B: we're looking at the first line in a brace-list
	   ((save-excursion
	      (goto-char indent-point)
	      (c-backward-syntactic-ws containing-sexp)
	      (= (point) (1+ containing-sexp)))
	    (goto-char containing-sexp)
	    (c-add-syntax 'brace-list-intro (c-point 'boi))
	    )
	    ;;))			; end CASE 8B
	   ;; CASE 8C: this is just a later brace-list-entry
	   (t (goto-char (1+ containing-sexp))
	      (c-forward-syntactic-ws indent-point)
	      (if (eq char-after-ip ?{)
		  (c-add-syntax 'brace-list-open (point))
		(c-add-syntax 'brace-list-entry (point))
		))			; end CASE 8C
	   ))				; end CASE 8
	 ;; CASE 9: A continued statement
	 ((and (not (memq char-before-ip '(?\; ?} ?:)))
	       (> (point)
		  (save-excursion
		    (c-beginning-of-statement-1 containing-sexp)
		    (setq placeholder (point))))
	       (/= placeholder containing-sexp))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (let ((after-cond-placeholder
		 (save-excursion
		   (goto-char placeholder)
		   (if (looking-at c-conditional-key)
		       (progn
			 (c-safe (c-skip-conditional))
			 (c-forward-syntactic-ws)
			 (if (eq (char-after) ?\;)
			     (progn
			       (forward-char 1)
			       (c-forward-syntactic-ws)))
			 (point))
		     nil))))
	    (cond
	     ;; CASE 9A: substatement
	     ((and after-cond-placeholder
		   (>= after-cond-placeholder indent-point))
	      (goto-char placeholder)
	      (if (eq char-after-ip ?{)
		  (c-add-syntax 'substatement-open (c-point 'boi))
		(c-add-syntax 'substatement (c-point 'boi))))
	     ;; CASE 9B: open braces for class or brace-lists
	     ((eq char-after-ip ?{)
	      (cond
	       ;; CASE 9B.1: class-open
	       ((save-excursion
		  (goto-char indent-point)
		  (skip-chars-forward " \t{")
		  (let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		    (and decl
			 (setq placeholder (aref decl 0)))
		    ))
		(c-add-syntax 'class-open placeholder))
	       ;; CASE 9B.2: brace-list-open
	       ((or (save-excursion
		      (goto-char placeholder)
		      (looking-at "\\<enum\\>"))
		    (eq char-before-ip ?=))
		(c-add-syntax 'brace-list-open placeholder))
	       ;; CASE 9B.3: catch-all for unknown construct.
	       (t
		;; Can and should I add an extensibility hook here?
		;; Something like c-recognize-hook so support for
		;; unknown constructs could be added.  It's probably a
		;; losing proposition, so I dunno.
		(goto-char placeholder)
		(c-add-syntax 'statement-cont (c-point 'boi))
		(c-add-syntax 'block-open))
	       ))
	     ;; CASE 9C: iostream insertion or extraction operator
	     ((looking-at "<<\\|>>")
	      (goto-char placeholder)
	      (and after-cond-placeholder
		   (goto-char after-cond-placeholder))
	      (while (and (re-search-forward "<<\\|>>" indent-point 'move)
			  (c-in-literal placeholder)))
	      ;; if we ended up at indent-point, then the first
	      ;; streamop is on a separate line. Indent the line like
	      ;; a statement-cont instead
	      (if (/= (point) indent-point)
		  (c-add-syntax 'stream-op (c-point 'boi))
		(c-backward-syntactic-ws lim)
		(c-add-syntax 'statement-cont (c-point 'boi))))
	     ;; CASE 9D: continued statement. find the accurate
	     ;; beginning of statement or substatement
	     (t
	      (c-beginning-of-statement-1 after-cond-placeholder)
	      ;; KLUDGE ALERT!  c-beginning-of-statement-1 can leave
	      ;; us before the lim we're passing in.  It should be
	      ;; fixed, but I'm worried about side-effects at this
	      ;; late date.  Fix for v5.
	      (goto-char (or (and after-cond-placeholder
				  (max after-cond-placeholder (point)))
			     (point)))
	      (c-add-syntax 'statement-cont (point)))
	     )))
	 ;; CASE 10: an else clause?
	 ((looking-at "\\<else\\>[^_]")
	  (c-backward-to-start-of-if containing-sexp)
	  (c-add-syntax 'else-clause (c-point 'boi)))
	 ;; CASE 11: Statement. But what kind?  Lets see if its a
	 ;; while closure of a do/while construct
	 ((progn
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (and (looking-at "while\\b[^_]")
		 (save-excursion
		   (c-backward-to-start-of-do containing-sexp)
		   (setq placeholder (point))
		   (looking-at "do\\b[^_]"))
		 ))
	  (c-add-syntax 'do-while-closure placeholder))
	 ;; CASE 12: A case or default label
	 ((looking-at c-switch-label-key)
	  (goto-char containing-sexp)
	  ;; check for hanging braces
	  (if (/= (point) (c-point 'boi))
	      (forward-sexp -1))
	  (c-add-syntax 'case-label (c-point 'boi)))
	 ;; CASE 13: any other label
	 ((looking-at c-label-key)
	  (goto-char containing-sexp)
	  (c-add-syntax 'label (c-point 'boi)))
	 ;; CASE 14: block close brace, possibly closing the defun or
	 ;; the class
	 ((eq char-after-ip ?})
	  (let* ((lim (c-safe-position containing-sexp fullstate))
		 (relpos (save-excursion
			   (goto-char containing-sexp)
			   (if (/= (point) (c-point 'boi))
			       (c-beginning-of-statement-1 lim))
			   (c-point 'boi))))
	    (cond
	     ;; CASE 14A: does this close an inline?
	     ((let ((inclass-p (progn
				 (goto-char containing-sexp)
				 (c-search-uplist-for-classkey state))))
		;; inextern-p in higher level let*
		(setq inextern-p (and inclass-p
				      (progn
					(goto-char (aref inclass-p 0))
					(looking-at "extern[^_]"))))
		(and inclass-p (not inextern-p)))
	      (c-add-syntax 'inline-close relpos))
	     ;; CASE 14B: if there an enclosing brace that hasn't
	     ;; been narrowed out by a class, then this is a
	     ;; block-close
	     ((and (not inextern-p)
		   (c-most-enclosing-brace state))
	      (c-add-syntax 'block-close relpos))
	     ;; CASE 14C: find out whether we're closing a top-level
	     ;; class or a defun
	     (t
	      (save-restriction
		(narrow-to-region (point-min) indent-point)
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (if decl
		      (c-add-syntax 'class-close (aref decl 0))
		    (c-add-syntax 'defun-close relpos)))))
	     )))
	 ;; CASE 15: statement catchall
	 (t
	  ;; we know its a statement, but we need to find out if it is
	  ;; the first statement in a block
	  (goto-char containing-sexp)
	  (forward-char 1)
	  (c-forward-syntactic-ws indent-point)
	  ;; now skip forward past any case/default clauses we might find.
	  (while (or (c-skip-case-statement-forward fullstate indent-point)
		     (and (looking-at c-switch-label-key)
			  (not inswitch-p)))
	    (setq inswitch-p t))
	  ;; we want to ignore non-case labels when skipping forward
	  (while (and (looking-at c-label-key)
		      (goto-char (match-end 0)))
	    (c-forward-syntactic-ws indent-point))
	  (cond
	   ;; CASE 15A: we are inside a case/default clause inside a
	   ;; switch statement.  find out if we are at the statement
	   ;; just after the case/default label.
	   ((and inswitch-p
		 (progn
		   (goto-char indent-point)
		   (c-backward-syntactic-ws containing-sexp)
		   (back-to-indentation)
		   (setq placeholder (point))
		   (looking-at c-switch-label-key)))
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (if (eq (char-after) ?{)
		(c-add-syntax 'statement-case-open placeholder)
	      (c-add-syntax 'statement-case-intro placeholder)))
	   ;; CASE 15B: continued statement
	   ((eq char-before-ip ?,)
	    (c-add-syntax 'statement-cont (c-point 'boi)))
	   ;; CASE 15C: a question/colon construct?  But make sure
	   ;; what came before was not a label, and what comes after
	   ;; is not a globally scoped function call!
	   ((or (and (memq char-before-ip '(?: ??))
		     (save-excursion
		       (goto-char indent-point)
		       (c-backward-syntactic-ws lim)
		       (back-to-indentation)
		       (not (looking-at c-label-key))))
		(and (memq char-after-ip '(?: ??))
		     (save-excursion
		       (goto-char indent-point)
		       (skip-chars-forward " \t")
		       ;; watch out for scope operator
		       (not (looking-at "::")))))
	    (c-add-syntax 'statement-cont (c-point 'boi)))
	   ;; CASE 15D: any old statement
	   ((< (point) indent-point)
	    (let ((safepos (c-most-enclosing-brace fullstate))
		  relpos done)
	      (goto-char indent-point)
	      (c-beginning-of-statement-1 safepos)
	      ;; It is possible we're on the brace that opens a nested
	      ;; function.
	      (if (and (eq (char-after) ?{)
		       (save-excursion
			 (c-backward-syntactic-ws safepos)
			 (not (eq (char-before) ?\;))))
		  (c-beginning-of-statement-1 safepos))
	      (if (and inswitch-p
		       (looking-at c-switch-label-key))
		  (progn
		    (goto-char placeholder)
		    (end-of-line)
		    (forward-sexp -1)))
	      (setq relpos (c-point 'boi))
	      (while (and (not done)
			  (<= safepos (point))
			  (/= relpos (point)))
		(c-beginning-of-statement-1 safepos)
		(if (= relpos (c-point 'boi))
		    (setq done t))
		(setq relpos (c-point 'boi)))
	      (c-add-syntax 'statement relpos)
	      (if (eq char-after-ip ?{)
		  (c-add-syntax 'block-open))))
	   ;; CASE 15E: first statement in an inline, or first
	   ;; statement in a top-level defun. we can tell this is it
	   ;; if there are no enclosing braces that haven't been
	   ;; narrowed out by a class (i.e. don't use bod here!)
	   ((save-excursion
	      (save-restriction
		(widen)
		(goto-char containing-sexp)
		(c-narrow-out-enclosing-class state containing-sexp)
		(not (c-most-enclosing-brace state))))
	    (goto-char containing-sexp)
	    ;; if not at boi, then defun-opening braces are hung on
	    ;; right side, so we need a different relpos
	    (if (/= (point) (c-point 'boi))
		(progn
		  (c-backward-syntactic-ws)
		  (c-safe (forward-sexp (if (eq (char-before) ?\))
					    -1 -2)))
		  ;; looking at a Java throws clause following a
		  ;; method's parameter list
		  (c-beginning-of-statement-1)
		  ))
	    (c-add-syntax 'defun-block-intro (c-point 'boi)))
	   ;; CASE 15F: first statement in a block
	   (t (goto-char containing-sexp)
	      (if (/= (point) (c-point 'boi))
		  (c-beginning-of-statement-1
		   (if (= (point) lim)
		       (c-safe-position (point) state) lim)))
	      (c-add-syntax 'statement-block-intro (c-point 'boi))
	      (if (eq char-after-ip ?{)
		  (c-add-syntax 'block-open)))
	   ))
	 )

	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	;; are we looking at a comment only line?
	(if (looking-at c-comment-start-regexp)
	    (c-add-syntax 'comment-intro))
	;; we might want to give additional offset to friends (in C++).
	(if (and (eq major-mode 'c++-mode)
		 (looking-at c-C++-friend-key))
	    (c-add-syntax 'friend))
	;; return the syntax
	syntax))))


(defun c-echo-parsing-error ()
  (if (not c-parsing-error)
      nil
    (message "unbalanced close brace at bufpos %d -- INDENTATION IS SUSPECT!"
	     c-parsing-error)
    (ding))
  c-parsing-error)

;; indent via syntactic language elements
(defun c-indent-line (&optional syntax)
  ;; indent the current line as C/C++/ObjC code. Optional SYNTAX is the
  ;; syntactic information for the current line. Returns the amount of
  ;; indentation change
  (let* ((c-syntactic-context (or syntax (c-guess-basic-syntax)))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'c-get-offset c-syntactic-context)))
	 (shift-amt  (- (current-indentation) indent)))
    (and c-echo-syntactic-information-p
	 (not (c-echo-parsing-error))
	 (message "syntax: %s, indent= %d" c-syntactic-context indent))
    (if (zerop shift-amt)
	nil
      (delete-region (c-point 'bol) (c-point 'boi))
      (beginning-of-line)
      (indent-to indent))
    (if (< (point) (c-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      )
    (run-hooks 'c-special-indent-hook)
    shift-amt))

(defun c-show-syntactic-information (arg)
  "Show syntactic information for current line.
With universal argument, inserts the analysis as a comment on that line."
  (interactive "P")
  (let ((syntax (c-guess-basic-syntax)))
    (if (not (consp arg))
	(if (not (c-echo-parsing-error))
	    (message "syntactic analysis: %s" syntax))
      (indent-for-comment)
      (insert (format "%s" syntax))
      ))
  (c-keep-region-active))


(provide 'cc-engine)
;;; cc-engine.el ends here
