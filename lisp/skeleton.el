;;; skeleton.el --- Lisp language extension for writing statement skeletons
;; Copyright (C) 1993, 1994, 1995 by Free Software Foundation, Inc.

;; Author: Daniel.Pfeiffer@Informatik.START.dbp.de, fax (+49 69) 7588-2389
;; Maintainer: FSF
;; Keywords: extensions, abbrev, languages, tools

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

;; A very concise language extension for writing structured statement
;; skeleton insertion commands for programming language modes.  This
;; originated in shell-script mode and was applied to ada-mode's
;; commands which shrunk to one third.  And these commands are now
;; user configurable.

;;; Code:

;; page 1:	statement skeleton language definition & interpreter
;; page 2:	paired insertion
;; page 3:	mirror-mode, an example for setting up paired insertion


(defvar skeleton-transformation nil
  "*If non-nil, function applied to literal strings before they are inserted.
It should take strings and characters and return them transformed, or nil
which means no transformation.
Typical examples might be `upcase' or `capitalize'.")

; this should be a fourth argument to defvar
(put 'skeleton-transformation 'variable-interactive
     "aTransformation function: ")



;;;###autoload
(defvar skeleton-filter 'identity
  "Function for transforming a skeleton-proxy's aliases' variable value.")


(defvar skeleton-untabify t
  "When non-`nil' untabifies when deleting backwards with element -ARG.")


(defvar skeleton-further-elements ()
  "A buffer-local varlist (see `let') of mode specific skeleton elements.
These variables are bound while interpreting a skeleton.  Their value may
in turn be any valid skeleton element if they are themselves to be used as
skeleton elements.")
(make-variable-buffer-local 'skeleton-further-elements)


(defvar skeleton-subprompt
  (substitute-command-keys
   "RET, \\<minibuffer-local-map>\\[abort-recursive-edit] or \\[help-command]")
  "*Replacement for %s in prompts of recursive subskeletons.")


(defvar skeleton-abbrev-cleanup nil)


(defvar skeleton-debug nil
  "*If non-nil `define-skeleton' will override previous definition.")


;;;###autoload
(defmacro define-skeleton (command documentation &rest skeleton)
  "Define a user-configurable COMMAND that enters a statement skeleton.
DOCUMENTATION is that of the command, while the variable of the same name,
which contains the skeleton, has a documentation to that effect.
INTERACTOR and ELEMENT ... are as defined under `skeleton-insert'."
  (if skeleton-debug
      (set command skeleton))
  (require 'backquote)
  (`(progn
      (defvar (, command) '(, skeleton)
	(, documentation))
      (defalias '(, command) 'skeleton-proxy))))



;; This command isn't meant to be called, only it's aliases with meaningful
;; names are.
;;;###autoload
(defun skeleton-proxy (&optional arg)
  "Insert a skeleton defined by variable of same name (see `skeleton-insert').
Prefix ARG allows wrapping around words or regions (see `skeleton-insert').
This command can also be an abbrev expansion (3rd and 4th columns in
\\[edit-abbrevs]  buffer: \"\"  command-name)."
  (interactive "*P")
  (let ((function (nth 1 (backtrace-frame 1))))
    (if (eq function 'nth)		; uncompiled lisp function
	(setq function (nth 1 (backtrace-frame 5)))
      (if (eq function 'byte-code)	; tracing byte-compiled function
	  (setq function (nth 1 (backtrace-frame 2)))))
    (if (not (setq function (funcall skeleton-filter (symbol-value function))))
	(if (or (eq this-command 'self-insert-command)
		(eq this-command 'pair-insert-maybe)
		(eq this-command 'expand-abbrev))
	    (setq buffer-undo-list
		  (primitive-undo 1 buffer-undo-list)))
      (skeleton-insert function
		       nil
		       (if (setq skeleton-abbrev-cleanup
				 (or (eq this-command 'self-insert-command)
				     (eq this-command 'pair-insert-maybe)))
			   ()
			 ;; Pretend  C-x a e  passed the prefix arg to us
			 (if (or arg current-prefix-arg)
			     (prefix-numeric-value (or arg
						       current-prefix-arg)))))
      (if skeleton-abbrev-cleanup
	  (setq deferred-action-list t
		deferred-action-function 'skeleton-abbrev-cleanup
		skeleton-abbrev-cleanup (point))))))


(defun skeleton-abbrev-cleanup (&rest list)
  "Value for `post-command-hook' to remove char that expanded abbrev."
  (if (integerp skeleton-abbrev-cleanup)
      (progn
	(delete-region skeleton-abbrev-cleanup (point))
	(setq deferred-action-list ()
	      deferred-action-function nil
	      skeleton-abbrev-cleanup nil))))


;;;###autoload
(defun skeleton-insert (skeleton &optional no-newline regions)
  "Insert the complex statement skeleton SKELETON describes very concisely.
If optional NO-NEWLINE is nil the skeleton will end on a line of its own.

With optional third REGIONS wrap first interesting point (`_') in skeleton
around next REGIONS words, if REGIONS is positive.  If REGIONS is negative,
wrap REGIONS preceding interregions into first REGIONS interesting positions
\(successive `_'s) in skeleton.  An interregion is the stretch of text between
two contiguous marked points.  If you marked A B C [] (where [] is the cursor)
in alphabetical order, the 3 interregions are simply the last 3 regions.  But
if you marked B A [] C, the interregions are B-A, A-[], []-C.

SKELETON is made up as (INTERACTOR ELEMENT ...).  INTERACTOR may be nil if
not needed, a prompt-string or an expression for complex read functions.

If ELEMENT is a string or a character it gets inserted (see also
`skeleton-transformation').  Other possibilities are:

	\\n	go to next line and align cursor
	_	interesting point, interregion here, point after termination
	>	indent line (or interregion if > _) according to major mode
	&	do next ELEMENT if previous moved point
	|	do next ELEMENT if previous didn't move point
	-num	delete num preceding characters (see `skeleton-untabify')
	resume:	skipped, continue here if quit is signaled
	nil	skipped

Further elements can be defined via `skeleton-further-elements'.  ELEMENT may
itself be a SKELETON with an INTERACTOR.  The user is prompted repeatedly for
different inputs.  The SKELETON is processed as often as the user enters a
non-empty string.  \\[keyboard-quit] terminates skeleton insertion, but
continues after `resume:' and positions at `_' if any.  If INTERACTOR in such
a subskeleton is a prompt-string which contains a \".. %s ..\" it is
formatted with `skeleton-subprompt'.

Quoted lisp-expressions are evaluated evaluated for their side-effect.
Other lisp-expressions are evaluated and the value treated as above.
Note that expressions may not return `t' since this impplies an
endless loop.  Modes can define other symbols by locally setting them
to any valid skeleton element.  The following local variables are
available:

	str	first time: read a string according to INTERACTOR
		then: insert previously read string once more
	help	help-form during interaction with the user or `nil'
	quit	non-nil after resume: section is entered by keyboard quit
	v1, v2	local variables for memorising anything you want"
  (and regions
       (setq regions
	     (if (> regions 0)
		 (list (point-marker)
		       (save-excursion (forward-word regions) (point-marker)))
	       (setq regions (- regions))
	       ;; copy regions - 1 elements from `mark-ring'
	       (let ((l1 (cons (mark-marker) mark-ring))
		     (l2 (list (point-marker))))
		 (while (and l1 (> regions 0))
		   (setq l2 (cons (car l1) l2)
			 regions (1- regions)
			 l1 (cdr l1)))
		 (sort l2 '<))))
       (goto-char (car regions))
       (setq regions (cdr regions)))
  (let (modified point resume: help quit v1 v2)
    (or no-newline
	(eolp)
	;;(save-excursion
	;;  (indent-to (prog1
	;;		 (current-indentation)
	;;	       (newline))))
	(goto-char (prog1 (point)
		     (indent-to (prog1
				    (current-indentation)
				  (newline))))))
    (unwind-protect
	(eval (list 'let skeleton-further-elements
		    '(skeleton-internal-list skeleton (car skeleton))))
      (if point
	  (goto-char point)))))



(defun skeleton-read (str &optional initial-input recursive)
  "Function for reading a string from the minibuffer in skeletons.
PROMPT may contain a `%s' which will be replaced by `skeleton-subprompt'.
If non-`nil' second arg INITIAL-INPUT is a string to insert before reading.
While reading, the value of `minibuffer-help-form' is variable `help' if that is
non-`nil' or a default string if optional ITERATIVE is non-`nil'."

  (or no-newline
      (eolp)
      (goto-char (prog1 (point)
		   (indent-to (prog1
				  (current-indentation)
				(newline))))))
  (let ((minibuffer-help-form (or help (if recursive "\
As long as you provide input you will insert another subskeleton.

If you enter the empty string, the loop inserting subskeletons is
left, and the current one is removed as far as it has been entered.

If you quit, the current subskeleton is removed as far as it has been
entered.  No more of the skeleton will be inserted, except maybe for a
syntactically necessary termination."
					 "
You are inserting a skeleton.  Standard text gets inserted into the buffer
automatically, and you are prompted to fill in the variable parts."))))
    (setq str (if (stringp str)
		  (read-string (format str skeleton-subprompt) initial-input)
		(eval str))))
  (if (or (null str) (string= str ""))
      (signal 'quit t)
    str))


(defun skeleton-internal-list (skeleton &optional str recursive)
  (let* ((start (save-excursion (beginning-of-line) (point)))
	 (column (current-column))
	 (line (buffer-substring start
				 (save-excursion (end-of-line) (point))))
	 opoint)
    (condition-case quit
	(progn
	  '(setq str (list 'setq 'str
			  (if recursive
			      (list 'skeleton-read (list 'quote str))
			    (list (if (stringp str)
				      'read-string
				    'eval)
				  str))))
	  (setq str (list 'setq 'str
			  (list 'skeleton-read
				(list 'quote str nil recursive))))
	  (while (setq modified (eq opoint (point))
		       opoint (point)
		       skeleton (cdr skeleton))
	    (skeleton-internal-1 (car skeleton)))
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
	 (while (if skeleton
		    (not (eq (car (setq skeleton (cdr skeleton)))
			     'resume:))))
	 (if skeleton
	     (skeleton-internal-list skeleton)
	   ;; propagate signal we can't handle
	   (if recursive (signal 'quit 'recursive)))
	 (signal 'quit nil))))))


(defun skeleton-internal-1 (element &optional literal)
  (cond ((and (integerp element)	; -num
	      (< element 0))
	 (if skeleton-untabify
	     (backward-delete-char-untabify (- element))
	   (delete-backward-char (- element))))
	((char-or-string-p element)
	 (insert-before-markers (if (and skeleton-transformation
					 (not literal))
				    (funcall skeleton-transformation element)
				  element)))
	((eq element '\n)		; actually (eq '\n 'n)
	 (newline)
	 (indent-relative t))
	((eq element '>)
	 (if (and regions
		  (eq (nth 1 skeleton) '_))
	     (indent-region (point) (car regions) nil)
	   (indent-for-tab-command)))
	((eq element '_)
	 (if regions
	     (progn
	       (goto-char (car regions))
	       (setq regions (cdr regions)))
	   (or point
	       (setq point (point)))))
	((eq element '&)
	 (if modified
	     (setq skeleton (cdr skeleton))))
	((eq element '|)
	 (or modified
	     (setq skeleton (cdr skeleton))))
	((if (consp element)
	     (or (stringp (car element))
		 (consp (car element))))
	 (while (skeleton-internal-list element (car element) t)))
	((if (consp element)
	     (eq 'quote (car element)))
	 (eval (nth 1 element)))
	((null element))
	((skeleton-internal-1 (eval element) t))))

;; Maybe belongs into simple.el or elsewhere

;;;###autoload
(define-skeleton local-variables-section
  "Insert a local variables section.  Use current comment syntax if any."
  ()
  '(save-excursion
     (if (re-search-forward page-delimiter nil t)
	 (error "Not on last page.")))
  comment-start "Local Variables:" comment-end \n
  comment-start "mode: "
  (completing-read "Mode: " obarray
		   (lambda (symbol)
		     (if (commandp symbol)
			 (string-match "-mode$" (symbol-name symbol))))
		   t)
  & -5 | '(kill-line 0) & -1 | comment-end \n
  ( (completing-read (format "Variable, %s: " skeleton-subprompt)
		     obarray
		     (lambda (symbol)
		       (or (eq symbol 'eval)
			   (user-variable-p symbol)))
		     t)
    comment-start str ": "
    (read-from-minibuffer "Expression: " nil read-expression-map nil
			  'read-expression-history) | _
    comment-end \n)
  resume:
  comment-start "End:" comment-end)

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
...), is passed to `skeleton-insert' with no interactor.  Variable `str'
does nothing.

Elements might be (?` ?` _ \"''\"), (?\\( ?  _ \" )\") or (?{ \\n > _ \\n ?} >).")



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
    (self-insert-command 1)
    (if skeleton-abbrev-cleanup
	()
      ;; (preceding-char) is stripped of any Meta-stuff in last-command-char
      (if (setq arg (assq (preceding-char) pair-alist))
	  ;; typed char is inserted, and car means no interactor
	  (skeleton-insert arg t)
	(save-excursion
	  (insert (or (cdr (assq (preceding-char)
				 '((?( . ?))
				   (?[ . ?])
				   (?{ . ?})
				   (?< . ?>)
				   (?` . ?'))))
		      last-command-char)))))))


;; A more serious example can be found in sh-script.el
;; The quote before (defun prevents this from being byte-compiled.
'(defun mirror-mode ()
  "This major mode is an amusing little example of paired insertion.
All printable characters do a paired self insert, while the other commands
work normally."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'pair)
  (make-local-variable 'pair-on-word)
  (make-local-variable 'pair-filter)
  (make-local-variable 'pair-alist)
  (setq major-mode 'mirror-mode
	mode-name "Mirror"
	pair-on-word t
	;; in the middle column insert one or none if odd window-width
	pair-filter (lambda ()
		      (if (>= (current-column)
			      (/ (window-width) 2))
			  ;; insert both on next line
			  (next-line 1)
			;; insert one or both?
			(= (* 2 (1+ (current-column)))
			   (window-width))))
	;; mirror these the other way round as well
	pair-alist '((?) _ ?()
		     (?] _ ?[)
		     (?} _ ?{)
		     (?> _ ?<)
		     (?/ _ ?\\)
		     (?\\ _ ?/)
		     (?` ?` _ "''")
		     (?' ?' _ "``"))
	;; in this mode we exceptionally ignore the user, else it's no fun
	pair t)
  (let ((map (make-keymap))
	(i ? ))
    (use-local-map map)
    (setq map (car (cdr map)))
    (while (< i ?\^?)
      (aset map i 'pair-insert-maybe)
      (setq i (1+ i))))
  (run-hooks 'mirror-mode-hook))

;; skeleton.el ends here
