;;; lisp.el --- Lisp editing commands for Emacs

;; Copyright (C) 1985, 1986, 1994, 2000 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: lisp, languages

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

;;; Commentary:

;; Lisp editing commands to go with Lisp major mode.  More-or-less
;; applicable in other modes too.

;;; Code:

;; Note that this variable is used by non-lisp modes too.
(defcustom defun-prompt-regexp nil
  "*If non-nil, a regexp to ignore before the character that starts a defun.
This is only necessary if the opening paren or brace is not in column 0.
See function `beginning-of-defun'.

Setting this variable automatically makes it local to the current buffer."
  :type '(choice (const nil)
		 regexp)
  :group 'lisp)
(make-variable-buffer-local 'defun-prompt-regexp)

(defcustom parens-require-spaces t
  "Non-nil means `insert-parentheses' should insert whitespace as needed."
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

(defun mark-sexp (&optional arg)
  "Set mark ARG sexps from point.
The place mark goes is the same place \\[forward-sexp] would
move to with the same argument.
If this command is repeated, it marks the next ARG sexps after the ones
already marked."
  (interactive "p")
  (push-mark
    (save-excursion
      (if (and (eq last-command this-command) (mark t))
	  (goto-char (mark)))
      (forward-sexp (or arg 1))
      (point))
    nil t))

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
  "Kill the sexp (balanced expression) following the cursor.
With ARG, kill that many sexps after the cursor.
Negative arg -N means kill N sexps before the cursor."
  (interactive "p")
  (let ((opoint (point)))
    (forward-sexp (or arg 1))
    (kill-region opoint (point))))

(defun backward-kill-sexp (&optional arg)
  "Kill the sexp (balanced expression) preceding the cursor.
With ARG, kill that many sexps before the cursor.
Negative arg -N means kill N sexps after the cursor."
  (interactive "p")
  (kill-sexp (- (or arg 1))))

(defvar beginning-of-defun-function nil
  "If non-nil, function for `beginning-of-defun-raw' to call.
This is used to find the beginning of the defun instead of using the
normal recipe (see `beginning-of-defun').  Major modes can define this
if defining `defun-prompt-regexp' is not sufficient to handle the mode's
needs.

The function should go to the line on which the current defun starts,
and return non-nil, or should return nil if it can't find the beginning.")

(defun beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
With ARG, do it that many times.  Negative arg -N
means move forward to Nth following beginning of defun.
Returns t unless search stops due to beginning or end of buffer.

Normally a defun starts when there is an char with open-parenthesis
syntax at the beginning of a line.  If `defun-prompt-regexp' is
non-nil, then a string which matches that regexp may precede the
open-parenthesis, and point ends up at the beginning of the line.

If variable `beginning-of-defun-function' is non-nil, its value
is called as a function to find the defun's beginning."
  (interactive "p")
  (and (beginning-of-defun-raw arg)
       (progn (beginning-of-line) t)))

(defun beginning-of-defun-raw (&optional arg)
  "Move point to the character that starts a defun.
This is identical to function `beginning-of-defun', except that point
does not move to the beginning of the line when `defun-prompt-regexp'
is non-nil.

If variable `beginning-of-defun-function' is non-nil, its value
is called as a function to find the defun's beginning."
  (interactive "p")
  (if beginning-of-defun-function
      (funcall beginning-of-defun-function)
    (and arg (< arg 0) (not (eobp)) (forward-char 1))
    (and (re-search-backward (if defun-prompt-regexp
				 (concat (if open-paren-in-column-0-is-defun-start
					     "^\\s(\\|" "")
					 "\\(" defun-prompt-regexp "\\)\\s(")
			       "^\\s(")
			     nil 'move (or arg 1))
	 (progn (goto-char (1- (match-end 0)))) t)))

(defvar end-of-defun-function nil
  "If non-nil, function for function `end-of-defun' to call.
This is used to find the end of the defun instead of using the normal
recipe (see `end-of-defun').  Major modes can define this if the
normal method is not appropriate.")

(defun buffer-end (arg)
  (if (> arg 0) (point-max) (point-min)))

(defun end-of-defun (&optional arg)
  "Move forward to next end of defun.  With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

An end of a defun occurs right after the close-parenthesis that
matches the open-parenthesis that starts a defun; see function
`beginning-of-defun'.

If variable `end-of-defun-function' is non-nil, its value
is called as a function to find the defun's end."
  (interactive "p")
  (if end-of-defun-function
      (funcall end-of-defun-function)
    (if (or (null arg) (= arg 0)) (setq arg 1))
    (let ((first t))
      (while (and (> arg 0) (< (point) (point-max)))
	(let ((pos (point)) npos)
	  (while (progn
		   (if (and first
			    (progn
			      (end-of-line 1)
			      (beginning-of-defun-raw 1)))
		       nil
		     (or (bobp) (forward-char -1))
		     (beginning-of-defun-raw -1))
		   (setq first nil)
		   (forward-list 1)
		   (skip-chars-forward " \t")
		   (if (looking-at "\\s<\\|\n")
		       (forward-line 1))
		   (<= (point) pos))))
	(setq arg (1- arg)))
      (while (< arg 0)
	(let ((pos (point)))
	  (beginning-of-defun-raw 1)
	  (forward-sexp 1)
	  (forward-line 1)
	  (if (>= (point) pos)
	      (if (beginning-of-defun-raw 2)
		  (progn
		    (forward-list 1)
		    (skip-chars-forward " \t")
		    (if (looking-at "\\s<\\|\n")
			(forward-line 1)))
		(goto-char (point-min)))))
	(setq arg (1+ arg))))))

(defun mark-defun ()
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark (point) nil t)
  (beginning-of-defun)
  (re-search-backward "^\n" (- (point) 1) t))

(defun narrow-to-defun (&optional arg)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional ARG is ignored."
  (interactive)
  (save-excursion
    (widen)
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (narrow-to-region (point) end))))

(defun insert-parentheses (arg)
  "Enclose following ARG sexps in parentheses.  Leave point after open-paren.
A negative ARG encloses the preceding ARG sexps instead.
No argument is equivalent to zero: just insert `()' and leave point between.
If `parens-require-spaces' is non-nil, this command also inserts a space
before and after, depending on the surrounding characters."
  (interactive "P")
  (if arg (setq arg (prefix-numeric-value arg))
    (setq arg 0))
  (cond ((> arg 0) (skip-chars-forward " \t"))
	((< arg 0) (forward-sexp arg) (setq arg (- arg))))
  (and parens-require-spaces
       (not (bobp))
       (memq (char-syntax (preceding-char)) '(?w ?_ ?\) ))
       (insert " "))
  (insert ?\()
  (save-excursion
    (or (eq arg 0) (forward-sexp arg))
    (insert ?\))
    (and parens-require-spaces
	 (not (eobp))
	 (memq (char-syntax (following-char)) '(?w ?_ ?\( ))
	 (insert " "))))

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
appropriate.  (See Info node `(emacs)Lists and Sexps'.)  If imbalance
is found, an error is signalled and point is left at the first
unbalanced character."
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
  		(error "Unmatched bracket or quote"))
    (error (cond ((eq 'scan-error (car data))
		  (goto-char (nth 2 data))
		  (error "Unmatched bracket or quote"))
		 (t (signal (car data) (cdr data)))))))

(defun lisp-complete-symbol (&optional predicate)
  "Perform completion on Lisp symbol preceding point.
Compare that symbol against the known Lisp symbols.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered, e.g. `commandp'.
If PREDICATE is nil, the context determines which symbols are
considered.  If the symbol starts just after an open-parenthesis, only
symbols with function definitions are considered.  Otherwise, all
symbols with function definitions, values or properties are
considered."
  (interactive)
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
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
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
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))

;;; lisp.el ends here
