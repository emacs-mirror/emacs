;;; skeleton.el --- Metalanguage for writing statement skeletons
;; Copyright (C) 1993 by Free Software Foundation, Inc.

;; Author: Daniel Pfeiffer, fax (+49 69) 75 88 529, c/o <bonhoure@cict.fr>
;; Maintainer: FSF
;; Keywords: shell programming

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
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; A very concise metalanguage for writing structured statement
;; skeleton insertion commands for programming language modes.  This
;; originated in shell-script mode and was applied to ada-mode's
;; commands which shrunk to one third.  And these commands are now
;; user configurable.

;;; Code:

;; page 1:	statement skeleton metalanguage definition & interpreter
;; page 2:	paired insertion
;; page 3:	mirror-mode, an example for setting up paired insertion


(defvar skeleton-transformation nil
  "*If non-nil, function applied to strings before they are inserted.
It should take strings and characters and return them transformed, or nil
which means no transformation.
Typical examples might be `upcase' or `capitalize'.")

; this should be a fourth argument to defvar
(put 'skeleton-transformation 'variable-interactive
     "aTransformation function: ")



(defvar skeleton-subprompt
  (substitute-command-keys
   "RET, \\<minibuffer-local-map>\\[abort-recursive-edit] or \\[help-command]")
  "*Replacement for %s in prompts of recursive skeleton definitions.")



(defvar skeleton-debug nil
  "*If non-nil `define-skeleton' will override previous definition.")



;;;###autoload
(defmacro define-skeleton (command documentation &rest definition)
  "Define a user-configurable COMMAND that enters a statement skeleton.
DOCUMENTATION is that of the command, while the variable of the same name,
which contains the definition, has a documentation to that effect.
PROMPT and ELEMENT ... are as defined under `skeleton-insert'."
  (if skeleton-debug
      (set command definition))
  (require 'backquote)
  (`(progn
      (defvar (, command) '(, definition)
	(, (concat "*Definition for the "
		   (symbol-name command)
		   " skeleton command.
See function `skeleton-insert' for meaning."))	)
      (defun (, command) ()
	(, documentation)
	(interactive)
	;; Don't use last-command to guarantee command does the same thing,
	;; whatever other name it is given.
	(skeleton-insert (, command))))))



;;;###autoload
(defun skeleton-insert (definition &optional no-newline)
  "Insert the complex statement skeleton DEFINITION describes very concisely.
If optional NO-NEWLINE is nil the skeleton will end on a line of its own.

DEFINITION is made up as (PROMPT ELEMENT ...).  PROMPT may be nil if not
needed, a prompt-string or an expression for complex read functions.

If ELEMENT is a string or a character it gets inserted (see also
`skeleton-transformation').  Other possibilities are:

	\\n	go to next line and align cursor
	>	indent according to major mode
	<	undent tab-width spaces but not beyond beginning of line
	_	cursor after termination
	&	skip next ELEMENT if previous didn't move point
	|	skip next ELEMENT if previous moved point
	-num	delete num preceding characters
	resume:	skipped, continue here if quit is signaled
	nil	skipped

ELEMENT may itself be DEFINITION with a PROMPT.  The user is prompted
repeatedly for different inputs.  The DEFINITION is processed as often
as the user enters a non-empty string.  \\[keyboard-quit] terminates
skeleton insertion, but continues after `resume:' and positions at `_'
if any.  If PROMPT in such a sub-definition contains a \".. %s ..\" it
is replaced by `skeleton-subprompt'.

Other lisp-expressions are evaluated and the value treated as above.
The following local variables are available:

	str	first time: read a string prompting with PROMPT and insert it
			    if PROMPT is not a string it is evaluated instead
		then: insert previously read string once more
	quit	non-nil when resume: section is entered by keyboard quit
	v1, v2	local variables for memorising anything you want"
  (let (modified opoint point resume: quit v1 v2)
    (skeleton-internal-list definition (car definition))
    (or no-newline
	(eolp)
	(newline)
	(indent-relative t))
    (if point
	(goto-char point))))



(defun skeleton-internal-read (str)
  (let ((minibuffer-help-form "\
As long as you provide input you will insert another subskeleton.

If you enter the empty string, the loop inserting subskeletons is
left, and the current one is removed as far as it has been entered.

If you quit, the current subskeleton is removed as far as it has been
entered.  No more of the skeleton will be inserted, except maybe for a
syntactically necessary termination."))
    (setq str (if (stringp str)
		  (read-string
		   (format str skeleton-subprompt))
		(eval str))))
  (if (string= str "")
      (signal 'quit t)
    str))


(defun skeleton-internal-list (definition &optional str recursive start line)
  (condition-case quit
      (progn
	(setq start (save-excursion (beginning-of-line) (point))
	      column (current-column)
	      line (buffer-substring start
				     (save-excursion (end-of-line) (point)))
	      str (list 'setq 'str
			(if recursive
			    (list 'skeleton-internal-read (list 'quote str))
			  (list (if (stringp str)
				    'read-string
				  'eval)
				str))))
	(while (setq modified (eq opoint (point))
		     opoint (point)
		     definition (cdr definition))
	  (skeleton-internal-1 (car definition)))
	;; maybe continue loop
	recursive)
    (quit ;; remove the subskeleton as far as it has been shown
	  (if (eq (cdr quit) 'recursive)
	      ()
	    ;; the subskeleton shouldn't have deleted outside current line
	    (end-of-line)
	    (delete-region start (point))
	    (insert line)
	    (move-to-column column))
	  (if (eq (cdr quit) t)
	      ;; empty string entered
	      nil
	    (while (if definition
		       (not (eq (car (setq definition (cdr definition)))
				'resume:))))
	    (if definition
		(skeleton-internal-list definition)
	      ;; propagate signal we can't handle
	      (if recursive (signal 'quit 'recursive)))))))



(defun skeleton-internal-1 (element)
  (cond ((and (integerp element)
	      (< element 0))
	 (delete-char element))
	((char-or-string-p element)
	 (insert (if skeleton-transformation
		     (funcall skeleton-transformation element)
		   element)) )
	((eq element '\n)		; actually (eq '\n 'n)
	 (newline)
	 (indent-relative t) )
	((eq element '>)
	 (indent-for-tab-command) )
	((eq element '<)
	 (backward-delete-char-untabify (min tab-width (current-column))) )
	((eq element '_)
	 (or point
	     (setq point (point))) )
	((eq element '&)
	 (if modified
	     (setq definition (cdr definition))) )
	((eq element '|)
	 (or modified
	     (setq definition (cdr definition))) )
	((if (consp element)
	     (or (stringp (car element))
		 (consp (car element))))
	 (while (skeleton-internal-list element (car element) t)) )
	((null element) )
	((skeleton-internal-1 (eval element)) )))


;; variables and command for automatically inserting pairs like () or ""

(defvar pair nil
  "*If this is nil pairing is turned off, no matter what else is set.
Otherwise modes with `pair-insert-maybe' on some keys will attempt this.")


(defvar pair-on-word nil
  "*If this is nil pairing is not attempted before or inside a word.")


(defvar pair-filter (lambda ())
  "Attempt pairing if this function returns nil, before inserting.
This allows for context-sensitive checking whether pairing is appropriate.")


(defvar pair-alist ()
  "An override alist of pairing partners matched against
`last-command-char'.  Each alist element, which looks like (ELEMENT
...), is passed to `skeleton-insert' with no prompt.  Variable `str'
does nothing.

Elements might be (?` ?` _ \"''\"), (?\\( ?  _ \" )\") or (?{ \\n > _ \\n < ?}).")



;;;###autoload
(defun pair-insert-maybe (arg)
  "Insert the character you type ARG times.

With no ARG, if `pair' is non-nil, and if
`pair-on-word' is non-nil or we are not before or inside a
word, and if `pair-filter' returns nil, pairing is performed.

If a match is found in `pair-alist', that is inserted, else
the defaults are used.  These are (), [], {}, <> and `' for the
symmetrical ones, and the same character twice for the others."
  (interactive "*P")
  (if (or arg
	  (not pair)
	  (if (not pair-on-word) (looking-at "\\w"))
	  (funcall pair-filter))
      (self-insert-command (prefix-numeric-value arg))
    (insert last-command-char)
    (if (setq arg (assq last-command-char pair-alist))
	;; typed char is inserted, and car means no prompt
	(skeleton-insert arg t)
      (save-excursion
	(insert (or (cdr (assq last-command-char
			       '((?( . ?))
				 (?[ . ?])
				 (?{ . ?})
				 (?< . ?>)
				 (?` . ?'))))
		    last-command-char))))))


;; a more serious example can be found in sh-script.el
;;;(defun mirror-mode ()
;;;  "This major mode is an amusing little example of paired insertion.
;;;All printable characters do a paired self insert, while the other commands
;;;work normally."
;;;  (interactive)
;;;  (kill-all-local-variables)
;;;  (make-local-variable 'pair)
;;;  (make-local-variable 'pair-on-word)
;;;  (make-local-variable 'pair-filter)
;;;  (make-local-variable 'pair-alist)
;;;  (setq major-mode 'mirror-mode
;;;	mode-name "Mirror"
;;;	pair-on-word t
;;;	;; in the middle column insert one or none if odd window-width
;;;	pair-filter (lambda ()
;;;		      (if (>= (current-column)
;;;			      (/ (window-width) 2))
;;;			  ;; insert both on next line
;;;			  (next-line 1)
;;;			;; insert one or both?
;;;			(= (* 2 (1+ (current-column)))
;;;			   (window-width))))
;;;	;; mirror these the other way round as well
;;;	pair-alist '((?) _ ?()
;;;			      (?] _ ?[)
;;;			      (?} _ ?{)
;;;			      (?> _ ?<)
;;;			      (?/ _ ?\\)
;;;			      (?\\ _ ?/)
;;;			      (?` ?` _ "''")
;;;			      (?' ?' _ "``"))
;;;	;; in this mode we exceptionally ignore the user, else it's no fun
;;;	pair t)
;;;  (let ((map (make-keymap))
;;;	(i ? ))
;;;    (use-local-map map)
;;;    (setq map (car (cdr map)))
;;;    (while (< i ?\^?)
;;;      (aset map i 'pair-insert-maybe)
;;;      (setq i (1+ i))))
;;;  (run-hooks 'mirror-mode-hook))

;; skeleton.el ends here
