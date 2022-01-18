;;; lisp.el --- Lisp editing commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1985-1986, 1994, 2000-2022 Free Software Foundation,
;; Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: lisp, languages
;; Package: emacs

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

;; Lisp editing commands to go with Lisp major mode.  More-or-less
;; applicable in other modes too.

;;; Code:

;; Note that this variable is used by non-lisp modes too.
(defcustom defun-prompt-regexp nil
  "If non-nil, a regexp to ignore before a defun.
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
  ;; FIXME:
  ;; - for some uses, we may want a "sexp-only" version, which only
  ;;   jumps over a well-formed sexp, rather than some dwimish thing
  ;;   like jumping from an "else" back up to its "if".
  ;; - for up-list, we could use the "sexp-only" behavior as well
  ;;   to treat the dwimish halfsexp as a form of "up-list" step.
  "If non-nil, `forward-sexp' delegates to this function.
Should take the same arguments and behave similarly to `forward-sexp'.")

(defun forward-sexp (&optional arg interactive)
  "Move forward across one balanced expression (sexp).
With ARG, do it that many times.  Negative arg -N means move
backward across N balanced expressions.  This command assumes
point is not in a string or comment.  Calls
`forward-sexp-function' to do the work, if that is non-nil.
If unable to move over a sexp, signal `scan-error' with three
arguments: a message, the start of the obstacle (usually a
parenthesis or list marker of some kind), and end of the
obstacle.  If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "^p\nd")
  (if interactive
      (condition-case _
          (forward-sexp arg nil)
        (scan-error (user-error (if (> arg 0)
                                    "No next sexp"
                                  "No previous sexp"))))
    (or arg (setq arg 1))
    (if forward-sexp-function
        (funcall forward-sexp-function arg)
      (goto-char (or (scan-sexps (point) arg) (buffer-end arg)))
      (if (< arg 0) (backward-prefix-chars)))))

(defun backward-sexp (&optional arg interactive)
  "Move backward across one balanced expression (sexp).
With ARG, do it that many times.  Negative arg -N means
move forward across N balanced expressions.
This command assumes point is not in a string or comment.
Uses `forward-sexp' to do the work.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "^p\nd")
  (or arg (setq arg 1))
  (forward-sexp (- arg) interactive))

(defun mark-sexp (&optional arg allow-extend)
  "Set mark ARG sexps from point.
The place mark goes is the same place \\[forward-sexp] would
move to with the same argument.
Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG sexps after the ones already marked.
This command assumes point is not in a string or comment."
  (interactive "P\np")
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (and transient-mark-mode mark-active)))
	 (setq arg (if arg (prefix-numeric-value arg)
		     (if (< (mark) (point)) -1 1)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
            (condition-case error
	        (forward-sexp arg)
              (scan-error
               (user-error (if (equal (cadr error)
                                      "Containing expression ends prematurely")
                               "No more sexp to select"
                             (cadr error)))))
	    (point))))
	(t
	 (push-mark
	  (save-excursion
            (condition-case error
	        (forward-sexp (prefix-numeric-value arg))
              (scan-error
               (user-error (if (equal (cadr error)
                                      "Containing expression ends prematurely")
                               "No sexp to select"
                             (cadr error)))))
	    (point))
	  nil t))))

(defun forward-list (&optional arg interactive)
  "Move forward across one balanced group of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.
With ARG, do it that many times.
Negative arg -N means move backward across N groups of parentheses.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "^p\nd")
  (if interactive
      (condition-case _
          (forward-list arg nil)
        (scan-error (user-error (if (> arg 0)
                                    "No next group"
                                  "No previous group"))))
    (or arg (setq arg 1))
    (goto-char (or (scan-lists (point) arg 0) (buffer-end arg)))))

(defun backward-list (&optional arg interactive)
  "Move backward across one balanced group of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.
With ARG, do it that many times.
Negative arg -N means move forward across N groups of parentheses.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "^p\nd")
  (or arg (setq arg 1))
  (forward-list (- arg) interactive))

(defun down-list (&optional arg interactive)
  "Move forward down one level of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.
With ARG, do this that many times.
A negative argument means move backward but still go down a level.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "^p\nd")
  (if interactive
      (condition-case _
          (down-list arg nil)
        (scan-error (user-error "At bottom level")))
    (or arg (setq arg 1))
    (let ((inc (if (> arg 0) 1 -1)))
      (while (/= arg 0)
        (goto-char (or (scan-lists (point) inc -1) (buffer-end arg)))
        (setq arg (- arg inc))))))

(defun backward-up-list (&optional arg escape-strings no-syntax-crossing)
  "Move backward out of one level of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.  With ARG, do this that
many times.  A negative argument means move forward but still to
a less deep spot.

If ESCAPE-STRINGS is non-nil (as it is interactively), move out
of enclosing strings as well.

If NO-SYNTAX-CROSSING is non-nil (as it is interactively), prefer
to break out of any enclosing string instead of moving to the
start of a list broken across multiple strings.

On error, location of point is unspecified."
  (interactive "^p\nd\nd")
  (up-list (- (or arg 1)) escape-strings no-syntax-crossing))

(defun up-list (&optional arg escape-strings no-syntax-crossing)
  "Move forward out of one level of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.  With ARG, do this that
many times.  A negative argument means move backward but still to
a less deep spot.

If ESCAPE-STRINGS is non-nil (as it is interactively), move out
of enclosing strings as well.

If NO-SYNTAX-CROSSING is non-nil (as it is interactively), prefer
to break out of any enclosing string instead of moving to the
end of a list broken across multiple strings.

On error, location of point is unspecified."
  (interactive "^p\nd\nd")
  (or arg (setq arg 1))
  (let ((inc (if (> arg 0) 1 -1))
        (pos nil))
    (while (/= arg 0)
      (condition-case err
          (save-restriction
            ;; If we've been asked not to cross string boundaries
            ;; and we're inside a string, narrow to that string so
            ;; that scan-lists doesn't find a match in a different
            ;; string.
            (when no-syntax-crossing
              (let* ((syntax (syntax-ppss))
                     (string-comment-start (nth 8 syntax)))
                (when string-comment-start
                  (save-excursion
                    (goto-char string-comment-start)
                    (narrow-to-region
                     (point)
                     (if (nth 3 syntax) ; in string
                         (condition-case nil
                             (progn (forward-sexp) (point))
                           (scan-error (point-max)))
                       (forward-comment 1)
                       (point)))))))
            (if (null forward-sexp-function)
                (goto-char (or (scan-lists (point) inc 1)
                               (buffer-end arg)))
              (condition-case err
                  (while (progn (setq pos (point))
                                (forward-sexp inc)
                                (/= (point) pos)))
                (scan-error (goto-char (nth (if (> arg 0) 3 2) err))))
              (if (= (point) pos)
                  (signal 'scan-error
                          (list "Unbalanced parentheses" (point) (point))))))
        (scan-error
         (let ((syntax nil))
           (or
            ;; If we bumped up against the end of a list, see whether
            ;; we're inside a string: if so, just go to the beginning
            ;; or end of that string.
            (and escape-strings
                 (or syntax (setf syntax (syntax-ppss)))
                 (nth 3 syntax)
                 (goto-char (nth 8 syntax))
                 (progn (when (> inc 0)
                          (forward-sexp))
                        t))
            ;; If we narrowed to a comment above and failed to escape
            ;; it, the error might be our fault, not an indication
            ;; that we're out of syntax.  Try again from beginning or
            ;; end of the comment.
            (and no-syntax-crossing
                 (or syntax (setf syntax (syntax-ppss)))
                 (nth 4 syntax)
                 (goto-char (nth 8 syntax))
                 (or (< inc 0)
                     (forward-comment 1))
                 (setf arg (+ arg inc)))
            (if no-syntax-crossing
                ;; Assume called interactively; don't signal an error.
                (user-error "At top level")
              (signal (car err) (cdr err)))))))
      (setq arg (- arg inc)))))

(defun kill-sexp (&optional arg interactive)
  "Kill the sexp (balanced expression) following point.
With ARG, kill that many sexps after point.
Negative arg -N means kill N sexps before point.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (if interactive
      (condition-case _
          (kill-sexp arg nil)
        (scan-error (user-error (if (> arg 0)
                                    "No next sexp"
                                  "No previous sexp"))))
    (let ((opoint (point)))
      (forward-sexp (or arg 1))
      (kill-region opoint (point)))))

(defun backward-kill-sexp (&optional arg interactive)
  "Kill the sexp (balanced expression) preceding point.
With ARG, kill that many sexps before point.
Negative arg -N means kill N sexps after point.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (kill-sexp (- (or arg 1)) interactive))

;; After Zmacs:
(defun kill-backward-up-list (&optional arg)
  "Kill the form containing the current sexp, leaving the sexp itself.
A prefix argument ARG causes the relevant number of surrounding
forms to be removed.
This command assumes point is not in a string or comment."
  (interactive "*p")
  (let ((current-sexp (thing-at-point 'sexp)))
    (if current-sexp
        (save-excursion
          (backward-up-list arg)
          (kill-sexp)
          (insert current-sexp))
      (user-error "Not at a sexp"))))

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
With ARG, do it that many times.  Negative ARG means move forward
to the ARGth following beginning of defun.

If search is successful, return t; point ends up at the beginning
of the line where the search succeeded.  Otherwise, return nil.

When `open-paren-in-column-0-is-defun-start' is non-nil, a defun
is assumed to start where there is a char with open-parenthesis
syntax at the beginning of a line.  If `defun-prompt-regexp' is
non-nil, then a string which matches that regexp may also precede
the open-parenthesis.  If `defun-prompt-regexp' and
`open-paren-in-column-0-is-defun-start' are both nil, this
function instead finds an open-paren at the outermost level.

If the variable `beginning-of-defun-function' is non-nil, its
value is called as a function, with argument ARG, to find the
defun's beginning.

Regardless of the values of `defun-prompt-regexp' and
`beginning-of-defun-function', point always moves to the
beginning of the line whenever the search is successful."
  (interactive "^p")
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
  (interactive "^p")   ; change this to "P", maybe, if we ever come to pass ARG
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
           (dotimes (_ arg)
             (funcall beginning-of-defun-function))
	 (dotimes (_ (- arg))
	   (funcall end-of-defun-function))))))

   ((or defun-prompt-regexp open-paren-in-column-0-is-defun-start)
    (and (< arg 0) (not (eobp)) (forward-char 1))
    (and (let (found)
           (while
               (and (setq found
                          (re-search-backward
                           (if defun-prompt-regexp
			       (concat (if open-paren-in-column-0-is-defun-start
					   "^\\s(\\|" "")
				       "\\(?:" defun-prompt-regexp "\\)\\s(")
			     "^\\s(")
			                      nil 'move arg))
                    (nth 8 (syntax-ppss))))
           found)
	 (progn (goto-char (1- (match-end 0)))
                t)))

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
	(let ((ppss (with-suppressed-warnings ((obsolete syntax-begin-function))
                      (let (syntax-begin-function)
		        (syntax-ppss))))
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

(defun beginning-of-defun--in-emptyish-line-p ()
  "Return non-nil if the point is in an \"emptyish\" line.
This means a line that consists entirely of comments and/or
whitespace."
;; See https://lists.gnu.org/r/help-gnu-emacs/2016-08/msg00141.html
  (save-excursion
    (forward-line 0)
    (let ((ppss (syntax-ppss)))
      (and (null (nth 3 ppss))
           (< (line-end-position)
              (progn (when (nth 4 ppss)
                       (goto-char (nth 8 ppss)))
                     (forward-comment (point-max))
                     (point)))))))

(defun beginning-of-defun-comments (&optional arg)
  "Move to the beginning of ARGth defun, including comments."
  (interactive "^p")
  (unless arg (setq arg 1))
  (beginning-of-defun arg)
  (let (first-line-p)
    (while (let ((ppss (progn (setq first-line-p (= (forward-line -1) -1))
                              (syntax-ppss (line-end-position)))))
             (while (and (nth 4 ppss) ; If eol is in a line-spanning comment,
                         (< (nth 8 ppss) (line-beginning-position)))
               (goto-char (nth 8 ppss)) ; skip to comment start.
               (setq ppss (syntax-ppss (line-end-position))))
             (and (not first-line-p)
                  (progn (skip-syntax-backward
                          "-" (line-beginning-position))
                         (not (bolp))) ; Check for blank line.
                  (beginning-of-defun--in-emptyish-line-p)))) ; Check for non-comment text.
    (forward-line (if first-line-p 0 1))))

(defvar end-of-defun-function
  (lambda () (forward-sexp 1))
  "Function for `end-of-defun' to call.
This is used to find the end of the defun at point.
It is called with no argument, right after calling `beginning-of-defun-raw'.
So the function can assume that point is at the beginning of the defun body.
It should move point to the first position after the defun.")

(defun buffer-end (arg)
  "Return the \"far end\" position of the buffer, in direction ARG.
If ARG is positive, that's the end of the buffer.
Otherwise, that's the beginning of the buffer."
  (if (> arg 0) (point-max) (point-min)))

(defun end-of-defun (&optional arg interactive)
  "Move forward to next end of defun.
With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

An end of a defun occurs right after the close-parenthesis that
matches the open-parenthesis that starts a defun; see function
`beginning-of-defun'.

If variable `end-of-defun-function' is non-nil, its value
is called as a function to find the defun's end.

If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "^p\nd")
  (if interactive
      (condition-case e
          (end-of-defun arg nil)
        (scan-error (user-error (cadr e))))
    (or (not (eq this-command 'end-of-defun))
        (eq last-command 'end-of-defun)
        (and transient-mark-mode mark-active)
        (push-mark))
    (if (or (null arg) (= arg 0)) (setq arg 1))
    (let ((pos (point))
          (beg (progn (end-of-line 1) (beginning-of-defun-raw 1) (point)))
	  (skip (lambda ()
		  ;; When comparing point against pos, we want to consider that
		  ;; if point was right after the end of the function, it's
		  ;; still considered as "in that function".
		  ;; E.g. `eval-defun' from right after the last close-paren.
		  (unless (bolp)
		    (skip-chars-forward " \t")
		    (if (looking-at "\\s<\\|\n")
		        (forward-line 1))))))
      (funcall end-of-defun-function)
      (when (<= arg 1)
        (funcall skip))
      (cond
       ((> arg 0)
        ;; Moving forward.
        (if (> (point) pos)
            ;; We already moved forward by one because we started from
            ;; within a function.
            (setq arg (1- arg))
          ;; We started from after the end of the previous function.
          (goto-char pos))
        (unless (zerop arg)
          (beginning-of-defun-raw (- arg))
          (funcall end-of-defun-function)))
       ((< arg 0)
        ;; Moving backward.
        (if (< (point) pos)
            ;; We already moved backward because we started from between
            ;; two functions.
            (setq arg (1+ arg))
          ;; We started from inside a function.
          (goto-char beg))
        (unless (zerop arg)
          (beginning-of-defun-raw (- arg))
	  (setq beg (point))
          (funcall end-of-defun-function))))
      (funcall skip)
      (while (and (< arg 0) (>= (point) pos))
        ;; We intended to move backward, but this ended up not doing so:
        ;; Try harder!
        (goto-char beg)
        (beginning-of-defun-raw (- arg))
        (if (>= (point) beg)
	    (setq arg 0)
	  (setq beg (point))
          (funcall end-of-defun-function)
	  (funcall skip))))))

(defun mark-defun (&optional arg interactive)
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
With positive ARG, mark this and that many next defuns; with negative
ARG, change the direction of marking.

If the mark is active, it marks the next or previous defun(s) after
the one(s) already marked.

If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (if interactive
      (condition-case e
          (mark-defun arg nil)
        (scan-error (user-error (cadr e))))
    (setq arg (or arg 1))
    ;; There is no `mark-defun-back' function - see
    ;; https://lists.gnu.org/r/bug-gnu-emacs/2016-11/msg00079.html
    ;; for explanation
    (when (eq last-command 'mark-defun-back)
      (setq arg (- arg)))
    (when (< arg 0)
      (setq this-command 'mark-defun-back))
    (cond ((use-region-p)
           (if (>= arg 0)
               (set-mark
                (save-excursion
                  (goto-char (mark))
                  ;; change the dotimes below to (end-of-defun arg)
                  ;; once bug #24427 is fixed
                  (dotimes (_ignore arg)
                    (end-of-defun))
                  (point)))
             (beginning-of-defun-comments (- arg))))
          (t
           (let ((opoint (point))
                 beg end)
             (push-mark opoint)
             ;; Try first in this order for the sake of languages with nested
             ;; functions where several can end at the same place as with the
             ;; offside rule, e.g. Python.
             (beginning-of-defun-comments)
             (setq beg (point))
             (end-of-defun)
             (setq end (point))
             (when (or (and (<= (point) opoint)
                            (> arg 0))
                       (= beg (point-min))) ; we were before the first defun!
               ;; beginning-of-defun moved back one defun so we got the wrong
               ;; one.  If ARG < 0, however, we actually want to go back.
               (goto-char opoint)
               (end-of-defun)
               (setq end (point))
               (beginning-of-defun-comments)
               (setq beg (point)))
             (goto-char beg)
             (cond ((> arg 0)
                    ;; change the dotimes below to (end-of-defun arg)
                    ;; once bug #24427 is fixed
                    (dotimes (_ignore arg)
                      (end-of-defun))
                    (setq end (point))
                    (push-mark end nil t)
                    (goto-char beg))
                   (t
                    (goto-char beg)
                    (unless (= arg -1)
                      ;; beginning-of-defun behaves strange with zero arg - see
                      ;; lists.gnu.org/r/bug-gnu-emacs/2017-02/msg00196.html
                      (beginning-of-defun (1- (- arg))))
                    (push-mark end nil t))))))
    (skip-chars-backward "[:space:]\n")
    (unless (bobp)
      (forward-line 1))))

(defvar narrow-to-defun-include-comments nil
  "If non-nil, `narrow-to-defun' will also show comments preceding the defun.")

(defun narrow-to-defun (&optional include-comments)
  "Make text outside current defun invisible.
The current defun is the one that contains point or follows point.
Preceding comments are included if INCLUDE-COMMENTS is non-nil.
Interactively, the behavior depends on `narrow-to-defun-include-comments'."
  (interactive (list narrow-to-defun-include-comments))
  (save-excursion
    (widen)
    (let ((opoint (point))
	  beg end)
      ;; Try first in this order for the sake of languages with nested
      ;; functions where several can end at the same place as with
      ;; the offside rule, e.g. Python.

      ;; Finding the start of the function is a bit problematic since
      ;; `beginning-of-defun' when we are on the first character of
      ;; the function might go to the previous function.
      ;;
      ;; Therefore we first move one character forward and then call
      ;; `beginning-of-defun'.  However now we must check that we did
      ;; not move into the next function.
      (let ((here (point)))
        (unless (eolp)
	  (forward-char))
        (beginning-of-defun)
        (when (< (point) here)
          (goto-char here)
          (beginning-of-defun)))
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
      (when include-comments
	(goto-char beg)
	;; Move back past all preceding comments (and whitespace).
	(when (forward-comment -1)
	  (while (forward-comment -1))
	  ;; Move forwards past any page breaks within these comments.
	  (when (and page-delimiter (not (string= page-delimiter "")))
	    (while (re-search-forward page-delimiter beg t)))
	  ;; Lastly, move past any empty lines.
	  (skip-chars-forward "[:space:]\n")
	  (beginning-of-line)
	  (setq beg (point))))
      (goto-char end)
      (re-search-backward "^\n" (- (point) 1) t)
      (narrow-to-region beg end))))

(defcustom insert-pair-alist
  '((?\( ?\)) (?\[ ?\]) (?\{ ?\}) (?\< ?\>) (?\" ?\") (?\' ?\') (?\` ?\'))
  "Alist of paired characters inserted by `insert-pair'.
Each element looks like (OPEN-CHAR CLOSE-CHAR) or (COMMAND-CHAR
OPEN-CHAR CLOSE-CHAR).  The characters OPEN-CHAR and CLOSE-CHAR
of the pair whose key is equal to the last input character with
or without modifiers, are inserted by `insert-pair'.

If COMMAND-CHAR is specified, it is a character that triggers the
insertion of the open/close pair, and COMMAND-CHAR itself isn't
inserted."
  :type '(repeat (choice (list :tag "Pair"
                               (character :tag "Open")
                               (character :tag "Close"))
                         (list :tag "Triple"
                               (character :tag "Command")
                               (character :tag "Open")
                               (character :tag "Close"))))
  :group 'lisp
  :version "27.1")

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
character is inserted ARG times.

This command assumes point is not in a string or comment."
  (interactive "P")
  (if (not (and open close))
      (let ((pair (or (assq last-command-event insert-pair-alist)
                      (assq (event-basic-type last-command-event)
                            insert-pair-alist))))
        (if pair
            (if (nth 2 pair)
                (setq open (nth 1 pair) close (nth 2 pair))
              (setq open (nth 0 pair) close (nth 1 pair))))))
  (if (and open close)
      (if (and transient-mark-mode mark-active)
          (progn
            (save-excursion
              (goto-char (region-end))
              (insert close))
            (goto-char (region-beginning))
            (insert open))
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
If region is active, insert enclosing characters at region boundaries.

This command assumes point is not in a string or comment."
  (interactive "P")
  (insert-pair arg ?\( ?\)))

(defcustom delete-pair-blink-delay blink-matching-delay
  "Time in seconds to delay after showing a paired character to delete.
It's used by the command `delete-pair'.  The value 0 disables blinking."
  :type 'number
  :group 'lisp
  :version "28.1")

(defun delete-pair (&optional arg)
  "Delete a pair of characters enclosing ARG sexps that follow point.
A negative ARG deletes a pair around the preceding ARG sexps instead.
The option `delete-pair-blink-delay' can disable blinking."
  (interactive "P")
  (if arg
      (setq arg (prefix-numeric-value arg))
    (setq arg 1))
  (if (< arg 0)
      (save-excursion
	(skip-chars-backward " \t")
	(save-excursion
	  (let ((close-char (char-before)))
	    (forward-sexp arg)
	    (unless (member (list (char-after) close-char)
			    (mapcar (lambda (p)
				      (if (= (length p) 3) (cdr p) p))
				    insert-pair-alist))
	      (error "Not after matching pair"))
	    (when (and (numberp delete-pair-blink-delay)
		       (> delete-pair-blink-delay 0))
	      (sit-for delete-pair-blink-delay))
	    (delete-char 1)))
	(delete-char -1))
    (save-excursion
      (skip-chars-forward " \t")
      (save-excursion
	(let ((open-char (char-after)))
	  (forward-sexp arg)
	  (unless (member (list open-char (char-before))
			  (mapcar (lambda (p)
				    (if (= (length p) 3) (cdr p) p))
				  insert-pair-alist))
	    (error "Not before matching pair"))
	  (when (and (numberp delete-pair-blink-delay)
		     (> delete-pair-blink-delay 0))
	    (sit-for delete-pair-blink-delay))
	  (delete-char -1)))
      (delete-char 1))))

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
    (scan-error (push-mark)
		(goto-char (nth 2 data))
		;; Could print (nth 1 data), which is either
		;; "Containing expression ends prematurely" or
		;; "Unbalanced parentheses", but those may not be so
		;; accurate/helpful, e.g. quotes may actually be
		;; mismatched.
  		(user-error "Unmatched bracket or quote"))))

(defun field-complete (table &optional predicate)
  (declare (obsolete completion-in-region "24.4"))
  (let ((minibuffer-completion-table table)
        (minibuffer-completion-predicate predicate)
        ;; This made sense for lisp-complete-symbol, but for
        ;; field-complete, this is out of place.  --Stef
        ;; (completion-annotate-function
        ;;  (unless (eq predicate 'fboundp)
        ;;    (lambda (str)
        ;;      (if (fboundp (intern-soft str)) " <f>"))))
        )
    (call-interactively 'minibuffer-complete)))

(defun lisp-complete-symbol (&optional _predicate)
  "Perform completion on Lisp symbol preceding point.
Compare that symbol against the known Lisp symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

The context determines which symbols are considered.  If the
symbol starts just after an open-parenthesis, only symbols with
function definitions are considered.  Otherwise, all symbols with
function definitions, values or properties are considered."
  (declare (obsolete completion-at-point "24.4")
           (advertised-calling-convention () "25.1"))
  (interactive)
  (let* ((data (elisp-completion-at-point))
         (plist (nthcdr 3 data)))
    (if (null data)
        (minibuffer-message "Nothing to complete")
      (let ((completion-extra-properties plist))
        (completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)
                              (plist-get plist :predicate))))))

;;; lisp.el ends here
