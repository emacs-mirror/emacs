;;; lisp.el --- Lisp editing commands for Emacs

;; Copyright (C) 1985, 1986, 1994, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: lisp, languages

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Lisp editing commands to go with Lisp major mode.  More-or-less
;; applicable in other modes too.

;;; Code:

;; Note that this variable is used by non-lisp modes too.
(defcustom defun-prompt-regexp nil
  "*If non-nil, a regexp to ignore before a defun.
This is only necessary if the opening paren or brace is not in column 0.
See function `beginning-of-defun'."
  :type '(choice (const nil)
		 regexp)
  :group 'lisp)
(make-variable-buffer-local 'defun-prompt-regexp)

(defcustom parens-require-spaces t
  "If non-nil, add whitespace as needed when inserting parentheses.
This affects `insert-parentheses' and `insert-pair'."
  :type 'boolean
  :group 'lisp)

(defvar forward-sexp-function nil
  "If non-nil, `forward-sexp' delegates to this function.
Should take the same arguments and behave similarly to `forward-sexp'.")

(defun forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
With ARG, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  (interactive "p")
  (or arg (setq arg 1))
  (if forward-sexp-function
      (funcall forward-sexp-function arg)
    (goto-char (or (scan-sexps (point) arg) (buffer-end arg)))
    (if (< arg 0) (backward-prefix-chars))))

(defun backward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).
With ARG, do it that many times.  Negative arg -N means
move forward across N balanced expressions."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-sexp (- arg)))

(defun mark-sexp (&optional arg allow-extend)
  "Set mark ARG sexps from point.
The place mark goes is the same place \\[forward-sexp] would
move to with the same argument.
Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG sexps after the ones already marked."
  (interactive "P\np")
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (and transient-mark-mode mark-active)))
	 (setq arg (if arg (prefix-numeric-value arg)
		     (if (< (mark) (point)) -1 1)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (forward-sexp arg)
	    (point))))
	(t
	 (push-mark
	  (save-excursion
	    (forward-sexp (prefix-numeric-value arg))
	    (point))
	  nil t))))

(defun forward-list (&optional arg)
  "Move forward across one balanced group of parentheses.
With ARG, do it that many times.
Negative arg -N means move backward across N groups of parentheses."
  (interactive "p")
  (or arg (setq arg 1))
  (goto-char (or (scan-lists (point) arg 0) (buffer-end arg))))

(defun backward-list (&optional arg)
  "Move backward across one balanced group of parentheses.
With ARG, do it that many times.
Negative arg -N means move forward across N groups of parentheses."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-list (- arg)))

(defun down-list (&optional arg)
  "Move forward down one level of parentheses.
With ARG, do this that many times.
A negative argument means move backward but still go down a level."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (scan-lists (point) inc -1) (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun backward-up-list (&optional arg)
  "Move backward out of one level of parentheses.
With ARG, do this that many times.
A negative argument means move forward but still to a less deep spot."
  (interactive "p")
  (up-list (- (or arg 1))))

(defun up-list (&optional arg)
  "Move forward out of one level of parentheses.
With ARG, do this that many times.
A negative argument means move backward but still to a less deep spot."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (scan-lists (point) inc 1) (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun kill-sexp (&optional arg)
  "Kill the sexp (balanced expression) following point.
With ARG, kill that many sexps after point.
Negative arg -N means kill N sexps before point."
  (interactive "p")
  (let ((opoint (point)))
    (forward-sexp (or arg 1))
    (kill-region opoint (point))))

(defun backward-kill-sexp (&optional arg)
  "Kill the sexp (balanced expression) preceding point.
With ARG, kill that many sexps before point.
Negative arg -N means kill N sexps after point."
  (interactive "p")
  (kill-sexp (- (or arg 1))))

;; After Zmacs:
(defun kill-backward-up-list (&optional arg)
  "Kill the form containing the current sexp, leaving the sexp itself.
A prefix argument ARG causes the relevant number of surrounding
forms to be removed."
  (interactive "*p")
  (let ((current-sexp (thing-at-point 'sexp)))
    (if current-sexp
        (save-excursion
          (backward-up-list arg)
          (kill-sexp)
          (insert current-sexp))
      (error "Not at a sexp"))))

(defvar beginning-of-defun-function nil
  "If non-nil, function for `beginning-of-defun-raw' to call.
This is used to find the beginning of the defun instead of using the
normal recipe (see `beginning-of-defun').  Major modes can define this
if defining `defun-prompt-regexp' is not sufficient to handle the mode's
needs.

The function takes the same argument as `beginning-of-defun' and should
behave similarly, returning non-nil if it found the beginning of a defun.
Ideally it should move to a point right before an open-paren which encloses
the body of the defun.")

(defun beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
With ARG, do it that many times.  Negative arg -N
means move forward to Nth following beginning of defun.
Returns t unless search stops due to beginning or end of buffer.

If variable `beginning-of-defun-function' is non-nil, its value
is called as a function to find the defun's beginning.

Normally a defun is assumed to start where there is a char with
open-parenthesis syntax at the beginning of a line.  If
`defun-prompt-regexp' is non-nil, then a string which matches
that regexp may precede the open-parenthesis, and point ends up
at the beginning of the line.

If `defun-prompt-regexp' and `open-paren-in-column-0-is-defun-start'
are both nil, the function instead finds an open-paren at the
outermost level."
  (interactive "p")
  (or (not (eq this-command 'beginning-of-defun))
      (eq last-command 'beginning-of-defun)
      (and transient-mark-mode mark-active)
      (push-mark))
  (and (beginning-of-defun-raw arg)
       (progn (beginning-of-line) t)))

(defun beginning-of-defun-raw (&optional arg)
  "Move point to the character that starts a defun.
This is identical to function `beginning-of-defun', except that point
does not move to the beginning of the line when `defun-prompt-regexp'
is non-nil.

If variable `beginning-of-defun-function' is non-nil, its value
is called as a function to find the defun's beginning."
  (interactive "p")   ; change this to "P", maybe, if we ever come to pass ARG
                      ; to beginning-of-defun-function.
  (unless arg (setq arg 1))
  (cond
   (beginning-of-defun-function
    (condition-case nil
        (funcall beginning-of-defun-function arg)
      ;; We used to define beginning-of-defun-function as taking no argument
      ;; but that makes it impossible to implement correct forward motion:
      ;; we used to use end-of-defun for that, but it's not supposed to do
      ;; the same thing (it moves to the end of a defun not to the beginning
      ;; of the next).
      ;; In case the beginning-of-defun-function uses the old calling
      ;; convention, fallback on the old implementation.
      (wrong-number-of-arguments
       (if (> arg 0)
           (dotimes (i arg)
             (funcall beginning-of-defun-function))
         ;; Better not call end-of-defun-function directly, in case
         ;; it's not defined.
         (end-of-defun (- arg))))))

   ((or defun-prompt-regexp open-paren-in-column-0-is-defun-start)
    (and (< arg 0) (not (eobp)) (forward-char 1))
    (and (re-search-backward (if defun-prompt-regexp
				 (concat (if open-paren-in-column-0-is-defun-start
					     "^\\s(\\|" "")
					 "\\(?:" defun-prompt-regexp "\\)\\s(")
			       "^\\s(")
			     nil 'move arg)
	 (progn (goto-char (1- (match-end 0)))) t))

   ;; If open-paren-in-column-0-is-defun-start and defun-prompt-regexp
   ;; are both nil, column 0 has no significance - so scan forward
   ;; from BOB to see how nested point is, then carry on from there.
   ;;
   ;; It is generally not a good idea to land up here, because the
   ;; call to scan-lists below can be extremely slow.  This is because
   ;; back_comment in syntax.c may have to scan from bob to find the
   ;; beginning of each comment.  Fixing this is not trivial -- cyd.

   ((eq arg 0))
   (t
    (let ((floor (point-min))
	  (ceiling (point-max))
	  (arg-+ve (> arg 0)))
      (save-restriction
	(widen)
	(let ((ppss (let (syntax-begin-function
			  font-lock-beginning-of-syntax-function)
		      (syntax-ppss)))
	      ;; position of least enclosing paren, or nil.
	      encl-pos)
	  ;; Back out of any comment/string, so that encl-pos will always
	  ;; become nil if we're at top-level.
	  (when (nth 8 ppss)
	    (goto-char (nth 8 ppss))
	    (setq ppss (syntax-ppss)))	; should be fast, due to cache.
	  (setq encl-pos (syntax-ppss-toplevel-pos ppss))
	  (if encl-pos (goto-char encl-pos))

	  (and encl-pos arg-+ve (setq arg (1- arg)))
	  (and (not encl-pos) (not arg-+ve) (not (looking-at "\\s("))
	       (setq arg (1+ arg)))

	  (condition-case nil   ; to catch crazy parens.
	      (progn
		(goto-char (scan-lists (point) (- arg) 0))
		(if arg-+ve
		    (if (>= (point) floor)
			t
		      (goto-char floor)
		      nil)
		  ;; forward to next (, or trigger the c-c
		  (goto-char (1- (scan-lists (point) 1 -1)))
		  (if (<= (point) ceiling)
		      t
		    (goto-char ceiling)
		    nil)))
	    (error
	     (goto-char (if arg-+ve floor ceiling))
	     nil))))))))

(defvar end-of-defun-function #'forward-sexp
  "Function for `end-of-defun' to call.
This is used to find the end of the defun.
It is called with no argument, right after calling `beginning-of-defun-raw'.
So the function can assume that point is at the beginning of the defun body.")

(defun buffer-end (arg)
  "Return the \"far end\" position of the buffer, in direction ARG.
If ARG is positive, that's the end of the buffer.
Otherwise, that's the beginning of the buffer."
  (if (> arg 0) (point-max) (point-min)))

(defun end-of-defun (&optional arg)
  "Move forward to next end of defun.
With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

An end of a defun occurs right after the close-parenthesis that
matches the open-parenthesis that starts a defun; see function
`beginning-of-defun'.

If variable `end-of-defun-function' is non-nil, its value
is called as a function to find the defun's end."
  (interactive "p")
  (or (not (eq this-command 'end-of-defun))
      (eq last-command 'end-of-defun)
      (and transient-mark-mode mark-active)
      (push-mark))
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (while (> arg 0)
    (let ((pos (point)))
      (end-of-line 1)
      (beginning-of-defun-raw 1)
      (while (unless (eobp)
               (funcall end-of-defun-function)
               (skip-chars-forward " \t")
               (if (looking-at "\\s<\\|\n")
                   (forward-line 1))
               ;; If we started after the end of the previous function, then
               ;; try again with the next one.
               (when (<= (point) pos)
                 (or (bobp) (forward-char -1))
                 (beginning-of-defun-raw -1)
                 'try-again))))
    (setq arg (1- arg)))
  (while (< arg 0)
    (let ((pos (point)))
      (while (unless (bobp)
               (beginning-of-line 1)
               (beginning-of-defun-raw 1)
               (let ((beg (point)))
                 (funcall end-of-defun-function)
                 (skip-chars-forward " \t")
                 (if (looking-at "\\s<\\|\n")
                     (forward-line 1))
                 ;; If we started from within the function just found, then
                 ;; try again with the previous one.
                 (when (>= (point) pos)
                   (goto-char beg)
                   'try-again)))))
    (setq arg (1+ arg))))

(defun mark-defun (&optional allow-extend)
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.

Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next defun after the ones already marked."
  (interactive "p")
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (and transient-mark-mode mark-active)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (end-of-defun)
	    (point))))
	(t
	 (let ((opoint (point))
	       beg end)
	   (push-mark opoint)
	   ;; Try first in this order for the sake of languages with nested
	   ;; functions where several can end at the same place as with
	   ;; the offside rule, e.g. Python.
	   (beginning-of-defun)
	   (setq beg (point))
	   (end-of-defun)
	   (setq end (point))
	   (while (looking-at "^\n")
	     (forward-line 1))
	   (if (> (point) opoint)
	       (progn
		 ;; We got the right defun.
		 (push-mark beg nil t)
		 (goto-char end)
		 (exchange-point-and-mark))
	     ;; beginning-of-defun moved back one defun
	     ;; so we got the wrong one.
	     (goto-char opoint)
	     (end-of-defun)
	     (push-mark (point) nil t)
	     (beginning-of-defun))
	   (re-search-backward "^\n" (- (point) 1) t)))))

(defun narrow-to-defun (&optional arg)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional ARG is ignored."
  (interactive)
  (save-excursion
    (widen)
    (let ((opoint (point))
	  beg end)
      ;; Try first in this order for the sake of languages with nested
      ;; functions where several can end at the same place as with
      ;; the offside rule, e.g. Python.
      (beginning-of-defun)
      (setq beg (point))
      (end-of-defun)
      (setq end (point))
      (while (looking-at "^\n")
	(forward-line 1))
      (unless (> (point) opoint)
	;; beginning-of-defun moved back one defun
	;; so we got the wrong one.
	(goto-char opoint)
	(end-of-defun)
	(setq end (point))
	(beginning-of-defun)
	(setq beg (point)))
      (goto-char end)
      (re-search-backward "^\n" (- (point) 1) t)
      (narrow-to-region beg end))))

(defvar insert-pair-alist
  '((?\( ?\)) (?\[ ?\]) (?\{ ?\}) (?\< ?\>) (?\" ?\") (?\' ?\') (?\` ?\'))
  "Alist of paired characters inserted by `insert-pair'.
Each element looks like (OPEN-CHAR CLOSE-CHAR) or (COMMAND-CHAR
OPEN-CHAR CLOSE-CHAR).  The characters OPEN-CHAR and CLOSE-CHAR
of the pair whose key is equal to the last input character with
or without modifiers, are inserted by `insert-pair'.")

(defun insert-pair (&optional arg open close)
  "Enclose following ARG sexps in a pair of OPEN and CLOSE characters.
Leave point after the first character.
A negative ARG encloses the preceding ARG sexps instead.
No argument is equivalent to zero: just insert characters
and leave point between.
If `parens-require-spaces' is non-nil, this command also inserts a space
before and after, depending on the surrounding characters.
If region is active, insert enclosing characters at region boundaries.

If arguments OPEN and CLOSE are nil, the character pair is found
from the variable `insert-pair-alist' according to the last input
character with or without modifiers.  If no character pair is
found in the variable `insert-pair-alist', then the last input
character is inserted ARG times."
  (interactive "P")
  (if (not (and open close))
      (let ((pair (or (assq last-command-char insert-pair-alist)
                      (assq (event-basic-type last-command-event)
                            insert-pair-alist))))
        (if pair
            (if (nth 2 pair)
                (setq open (nth 1 pair) close (nth 2 pair))
              (setq open (nth 0 pair) close (nth 1 pair))))))
  (if (and open close)
      (if (and transient-mark-mode mark-active)
          (progn
            (save-excursion (goto-char (region-end))       (insert close))
            (save-excursion (goto-char (region-beginning)) (insert open)))
        (if arg (setq arg (prefix-numeric-value arg))
          (setq arg 0))
        (cond ((> arg 0) (skip-chars-forward " \t"))
              ((< arg 0) (forward-sexp arg) (setq arg (- arg))))
        (and parens-require-spaces
             (not (bobp))
             (memq (char-syntax (preceding-char)) (list ?w ?_ (char-syntax close)))
             (insert " "))
        (insert open)
        (save-excursion
          (or (eq arg 0) (forward-sexp arg))
          (insert close)
          (and parens-require-spaces
               (not (eobp))
               (memq (char-syntax (following-char)) (list ?w ?_ (char-syntax open)))
               (insert " "))))
    (insert-char (event-basic-type last-command-event)
                 (prefix-numeric-value arg))))

(defun insert-parentheses (&optional arg)
  "Enclose following ARG sexps in parentheses.
Leave point after open-paren.
A negative ARG encloses the preceding ARG sexps instead.
No argument is equivalent to zero: just insert `()' and leave point between.
If `parens-require-spaces' is non-nil, this command also inserts a space
before and after, depending on the surrounding characters.
If region is active, insert enclosing characters at region boundaries."
  (interactive "P")
  (insert-pair arg ?\( ?\)))

(defun delete-pair ()
  "Delete a pair of characters enclosing the sexp that follows point."
  (interactive)
  (save-excursion (forward-sexp 1) (delete-char -1))
  (delete-char 1))

(defun raise-sexp (&optional arg)
  "Raise ARG sexps higher up the tree."
  (interactive "p")
  (let ((s (if (and transient-mark-mode mark-active)
               (buffer-substring (region-beginning) (region-end))
             (buffer-substring
              (point)
              (save-excursion (forward-sexp arg) (point))))))
    (backward-up-list 1)
    (delete-region (point) (save-excursion (forward-sexp 1) (point)))
    (save-excursion (insert s))))

(defun move-past-close-and-reindent ()
  "Move past next `)', delete indentation before it, then indent after it."
  (interactive)
  (up-list 1)
  (forward-char -1)
  (while (save-excursion		; this is my contribution
	   (let ((before-paren (point)))
	     (back-to-indentation)
	     (and (= (point) before-paren)
		  (progn
		    ;; Move to end of previous line.
		    (beginning-of-line)
		    (forward-char -1)
		    ;; Verify it doesn't end within a string or comment.
		    (let ((end (point))
			  state)
		      (beginning-of-line)
		      ;; Get state at start of line.
		      (setq state  (list 0 nil nil
					 (null (calculate-lisp-indent))
					 nil nil nil nil
					 nil))
		      ;; Parse state across the line to get state at end.
		      (setq state (parse-partial-sexp (point) end nil nil
						      state))
		      ;; Check not in string or comment.
		      (and (not (elt state 3)) (not (elt state 4))))))))
    (delete-indentation))
  (forward-char 1)
  (newline-and-indent))

(defun check-parens ()			; lame name?
  "Check for unbalanced parentheses in the current buffer.
More accurately, check the narrowed part of the buffer for unbalanced
expressions (\"sexps\") in general.  This is done according to the
current syntax table and will find unbalanced brackets or quotes as
appropriate.  (See Info node `(emacs)Parentheses'.)  If imbalance is
found, an error is signaled and point is left at the first unbalanced
character."
  (interactive)
  (condition-case data
      ;; Buffer can't have more than (point-max) sexps.
      (scan-sexps (point-min) (point-max))
    (scan-error (goto-char (nth 2 data))
		;; Could print (nth 1 data), which is either
		;; "Containing expression ends prematurely" or
		;; "Unbalanced parentheses", but those may not be so
		;; accurate/helpful, e.g. quotes may actually be
		;; mismatched.
  		(error "Unmatched bracket or quote"))))

(defun field-complete (table &optional predicate)
  (let* ((pattern (field-string-no-properties))
         (completion (try-completion pattern table predicate)))
    (cond ((eq completion t))
          ((null completion)
           (message "Can't find completion for \"%s\"" pattern)
           (ding))
          ((not (string= pattern completion))
           (delete-region (field-beginning) (field-end))
           (insert completion)
           ;; Don't leave around a completions buffer that's out of date.
           (let ((win (get-buffer-window "*Completions*" 0)))
             (if win (with-selected-window win (bury-buffer)))))
          (t
           (let ((minibuf-is-in-use
                  (eq (minibuffer-window) (selected-window))))
             (unless minibuf-is-in-use
               (message "Making completion list..."))
             (let ((list (all-completions pattern table predicate)))
               (setq list (sort list 'string<))
               (or (eq predicate 'fboundp)
                   (let (new)
                     (while list
                       (setq new (cons (if (fboundp (intern (car list)))
                                           (list (car list) " <f>")
                                         (car list))
                                       new))
                       (setq list (cdr list)))
                     (setq list (nreverse new))))
               (if (> (length list) 1)
                   (with-output-to-temp-buffer "*Completions*"
                     (display-completion-list list pattern))
                 ;; Don't leave around a completions buffer that's
                 ;; out of date.
                 (let ((win (get-buffer-window "*Completions*" 0)))
                   (if win (with-selected-window win (bury-buffer))))))
             (unless minibuf-is-in-use
               (message "Making completion list...%s" "done")))))))

(defun lisp-complete-symbol (&optional predicate)
  "Perform completion on Lisp symbol preceding point.
Compare that symbol against the known Lisp symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered, e.g. `commandp'.
If PREDICATE is nil, the context determines which symbols are
considered.  If the symbol starts just after an open-parenthesis, only
symbols with function definitions are considered.  Otherwise, all
symbols with function definitions, values or properties are
considered."
  (interactive)
  (let ((window (get-buffer-window "*Completions*" 0)))
    (if (and (eq last-command this-command)
	     window (window-live-p window) (window-buffer window)
	     (buffer-name (window-buffer window)))
	;; If this command was repeated, and
	;; there's a fresh completion window with a live buffer,
	;; and this command is repeated, scroll that window.
	(with-current-buffer (window-buffer window)
	  (if (pos-visible-in-window-p (point-max) window)
	      (set-window-start window (point-min))
	    (save-selected-window
	      (select-window window)
	      (scroll-up))))

      ;; Do completion.
      (let* ((end (point))
	     (beg (with-syntax-table emacs-lisp-mode-syntax-table
		    (save-excursion
		      (backward-sexp 1)
		      (while (= (char-syntax (following-char)) ?\')
			(forward-char 1))
		      (point))))
	     (pattern (buffer-substring-no-properties beg end))
	     (predicate
	      (or predicate
		  (save-excursion
		    (goto-char beg)
		    (if (not (eq (char-before) ?\())
			(lambda (sym)	;why not just nil ?   -sm
			  (or (boundp sym) (fboundp sym)
			      (symbol-plist sym)))
		      ;; Looks like a funcall position.  Let's double check.
		      (if (condition-case nil
			      (progn (up-list -2) (forward-char 1)
				     (eq (char-after) ?\())
			    (error nil))
			  ;; If the first element of the parent list is an open
			  ;; parenthesis we are probably not in a funcall position.
			  ;; Maybe a `let' varlist or something.
			  nil
			;; Else, we assume that a function name is expected.
			'fboundp)))))
	     (completion (try-completion pattern obarray predicate)))
	(cond ((eq completion t))
	      ((null completion)
	       (if (window-minibuffer-p (selected-window))
		   (minibuffer-message (format " [No completions of \"%s\"]" pattern))
		 (message "Can't find completion for \"%s\"" pattern))
	       (ding))
	      ((not (string= pattern completion))
	       (delete-region beg end)
	       (insert completion)
	       ;; Don't leave around a completions buffer that's out of date.
	       (let ((win (get-buffer-window "*Completions*" 0)))
		 (if win (with-selected-window win (bury-buffer)))))
	      (t
	       (let ((minibuf-is-in-use
		      (eq (minibuffer-window) (selected-window))))
		 (unless minibuf-is-in-use
		   (message "Making completion list..."))
		 (let ((list (all-completions pattern obarray predicate)))
		   (setq list (sort list 'string<))
		   (or (eq predicate 'fboundp)
		       (let (new)
			 (while list
			   (setq new (cons (if (fboundp (intern (car list)))
					       (list (car list) " <f>")
					     (car list))
					   new))
			   (setq list (cdr list)))
			 (setq list (nreverse new))))
		   (if (> (length list) 1)
		       (with-output-to-temp-buffer "*Completions*"
			 (display-completion-list list pattern))
		     ;; Don't leave around a completions buffer that's
		     ;; out of date.
		     (let ((win (get-buffer-window "*Completions*" 0)))
		       (if win (with-selected-window win (bury-buffer))))))
		 (unless minibuf-is-in-use
		   (message "Making completion list...%s" "done")))))))))

;; arch-tag: aa7fa8a4-2e6f-4e9b-9cd9-fef06340e67e
;;; lisp.el ends here
