;;; skeleton.el --- Lisp language extension for writing statement skeletons

;; Copyright (C) 1993, 1994, 1995, 1996 by Free Software Foundation, Inc.

;; Author: Daniel Pfeiffer <occitan@esperanto.org>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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


(defvar skeleton-transformation 'identity
  "*If non-nil, function applied to literal strings before they are inserted.
It should take strings and characters and return them transformed, or nil
which means no transformation.
Typical examples might be `upcase' or `capitalize'.")

; this should be a fourth argument to defvar
(put 'skeleton-transformation 'variable-interactive
     "aTransformation function: ")


(defvar skeleton-autowrap t
  "Controls wrapping behaviour of functions created with `define-skeleton'.
When the region is visible (due to `transient-mark-mode' or marking a region
with the mouse) and this is non-`nil' and the function was called without an
explicit ARG, then the ARG defaults to -1, i.e. wrapping around the visible
region.

We will probably delete this variable in a future Emacs version
unless we get a substantial number of complaints about the auto-wrap
feature.")

(defvar skeleton-end-newline t
  "If non-nil, make sure that the skeleton inserted ends with a newline.
This just influences the way the default `skeleton-end-hook' behaves.")

(defvar skeleton-end-hook
  (lambda ()
    (or (eolp) (not skeleton-end-newline) (newline-and-indent)))
  "Hook called at end of skeleton but before going to point of interest.
By default this moves out anything following to next line,
  unless `skeleton-end-newline' is set to nil.
The variables `v1' and `v2' are still set when calling this.")


;;;###autoload
(defvar skeleton-filter 'identity
  "Function for transforming a skeleton proxy's aliases' variable value.")

(defvar skeleton-untabify t
  "When non-`nil' untabifies when deleting backwards with element -ARG.")

(defvar skeleton-newline-indent-rigidly nil
  "When non-`nil', indent rigidly under current line for element `\\n'.
Else use mode's `indent-line-function'.")

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


(defvar skeleton-abbrev-cleanup nil
  "Variable used to delete the character that led to abbrev expansion.")


(defvar skeleton-debug nil
  "*If non-nil `define-skeleton' will override previous definition.")

(defvar skeleton-positions nil
  "List of positions marked with @, after skeleton insertion.
The list describes the most recent skeleton insertion, and its elements
are integer buffer positions in the reverse order of the insertion order.")

;; reduce the number of compiler warnings
(defvar skeleton)
(defvar skeleton-modified)
(defvar skeleton-point)
(defvar skeleton-regions)

;;;###autoload
(defmacro define-skeleton (command documentation &rest skeleton)
  "Define a user-configurable COMMAND that enters a statement skeleton.
DOCUMENTATION is that of the command, while the variable of the same name,
which contains the skeleton, has a documentation to that effect.
INTERACTOR and ELEMENT ... are as defined under `skeleton-insert'."
  (if skeleton-debug
      (set command skeleton))
  `(progn
     (defun ,command (&optional str arg)
       ,(concat documentation
		(if (string-match "\n\\>" documentation)
		    "" "\n")
		"\n"
  "This is a skeleton command (see `skeleton-insert').
Normally the skeleton text is inserted at point, with nothing \"inside\".
If there is a highlighted region, the skeleton text is wrapped
around the region text.

A prefix argument ARG says to wrap the skeleton around the next ARG words.
A prefix argument of -1 says to wrap around region, even if not highlighted.
A prefix argument of zero says to wrap around zero words---that is, nothing.
This is a way of overriding the use of a highlighted region.")
       (interactive "*P\nP")
       (skeleton-proxy-new ',skeleton str arg))))

;;;###autoload
(defun skeleton-proxy-new (skeleton &optional str arg)
  "Insert skeleton defined by variable of same name (see `skeleton-insert').
Prefix ARG allows wrapping around words or regions (see `skeleton-insert').
If no ARG was given, but the region is visible, ARG defaults to -1 depending
on `skeleton-autowrap'.  An ARG of  M-0  will prevent this just for once.
This command can also be an abbrev expansion (3rd and 4th columns in
\\[edit-abbrevs]  buffer: \"\"  command-name).

When called as a function, optional first argument STR may also be a string
which will be the value of `str' whereas the skeleton's interactor is then
ignored."
  (interactive "*P\nP")
  (setq skeleton (funcall skeleton-filter skeleton))
  (if (not skeleton)
      (if (memq this-command '(self-insert-command
			       skeleton-pair-insert-maybe
			       expand-abbrev))
	  (setq buffer-undo-list (primitive-undo 1 buffer-undo-list)))
    (skeleton-insert skeleton
		     (if (setq skeleton-abbrev-cleanup
			       (or (eq this-command 'self-insert-command)
				   (eq this-command
				       'skeleton-pair-insert-maybe)))
			 ()
		       ;; Pretend  C-x a e  passed its prefix arg to us
		       (if (or arg current-prefix-arg)
			   (prefix-numeric-value (or arg
						     current-prefix-arg))
			 (and skeleton-autowrap
			      (or (eq last-command 'mouse-drag-region)
				  (and transient-mark-mode mark-active))
			      -1)))
		     (if (stringp str)
			 str))
    (and skeleton-abbrev-cleanup
	 (setq skeleton-abbrev-cleanup (point))
	 (add-hook 'post-command-hook 'skeleton-abbrev-cleanup nil t))))

;; This command isn't meant to be called, only its aliases with meaningful
;; names are.
;;;###autoload
(defun skeleton-proxy (&optional str arg)
  "Insert skeleton defined by variable of same name (see `skeleton-insert').
Prefix ARG allows wrapping around words or regions (see `skeleton-insert').
If no ARG was given, but the region is visible, ARG defaults to -1 depending
on `skeleton-autowrap'.  An ARG of  M-0  will prevent this just for once.
This command can also be an abbrev expansion (3rd and 4th columns in
\\[edit-abbrevs]  buffer: \"\"  command-name).

When called as a function, optional first argument STR may also be a string
which will be the value of `str' whereas the skeleton's interactor is then
ignored."
  (interactive "*P\nP")
  (let ((function (nth 1 (backtrace-frame 1))))
    (if (eq function 'nth)		; uncompiled Lisp function
	(setq function (nth 1 (backtrace-frame 5)))
      (if (eq function 'byte-code)	; tracing byte-compiled function
	  (setq function (nth 1 (backtrace-frame 2)))))
    (if (not (setq function (funcall skeleton-filter (symbol-value function))))
	(if (memq this-command '(self-insert-command
				 skeleton-pair-insert-maybe
				 expand-abbrev))
	    (setq buffer-undo-list (primitive-undo 1 buffer-undo-list)))
      (skeleton-insert function
		       (if (setq skeleton-abbrev-cleanup
				 (or (eq this-command 'self-insert-command)
				     (eq this-command
					 'skeleton-pair-insert-maybe)))
			   ()
			 ;; Pretend  C-x a e  passed its prefix arg to us
			 (if (or arg current-prefix-arg)
			     (prefix-numeric-value (or arg
						       current-prefix-arg))
			   (and skeleton-autowrap
				(or (eq last-command 'mouse-drag-region)
				    (and transient-mark-mode mark-active))
				-1)))
		       (if (stringp str)
			   str))
      (and skeleton-abbrev-cleanup
	   (setq skeleton-abbrev-cleanup (point))
	   (add-hook 'post-command-hook 'skeleton-abbrev-cleanup nil t)))))


(defun skeleton-abbrev-cleanup (&rest list)
  "Value for `post-command-hook' to remove char that expanded abbrev."
  (if (integerp skeleton-abbrev-cleanup)
      (progn
	(delete-region skeleton-abbrev-cleanup (point))
	(setq skeleton-abbrev-cleanup)
	(remove-hook 'post-command-hook 'skeleton-abbrev-cleanup t))))


;;;###autoload
(defun skeleton-insert (skeleton &optional regions str)
  "Insert the complex statement skeleton SKELETON describes very concisely.

With optional second argument REGIONS, wrap first interesting point
\(`_') in skeleton around next REGIONS words, if REGIONS is positive.
If REGIONS is negative, wrap REGIONS preceding interregions into first
REGIONS interesting positions \(successive `_'s) in skeleton.

An interregion is the stretch of text between two contiguous marked
points.  If you marked A B C [] (where [] is the cursor) in
alphabetical order, the 3 interregions are simply the last 3 regions.
But if you marked B A [] C, the interregions are B-A, A-[], []-C.

The optional third argument STR, if specified, is the value for the
variable `str' within the skeleton.  When this is non-nil, the
interactor gets ignored, and this should be a valid skeleton element.

SKELETON is made up as (INTERACTOR ELEMENT ...).  INTERACTOR may be nil if
not needed, a prompt-string or an expression for complex read functions.

If ELEMENT is a string or a character it gets inserted (see also
`skeleton-transformation').  Other possibilities are:

	\\n	go to next line and indent according to mode
	_	interesting point, interregion here
	>	indent line (or interregion if > _) according to major mode
	@	add position to `skeleton-positions'
	&	do next ELEMENT iff previous moved point
	|	do next ELEMENT iff previous didn't move point
	-num	delete num preceding characters (see `skeleton-untabify')
	resume:	skipped, continue here if quit is signaled
	nil	skipped

After termination, point will be positioned at the first occurrence
of _ or @ or at the end of the inserted text.

Further elements can be defined via `skeleton-further-elements'.  ELEMENT may
itself be a SKELETON with an INTERACTOR.  The user is prompted repeatedly for
different inputs.  The SKELETON is processed as often as the user enters a
non-empty string.  \\[keyboard-quit] terminates skeleton insertion, but
continues after `resume:' and positions at `_' if any.  If INTERACTOR in such
a subskeleton is a prompt-string which contains a \".. %s ..\" it is
formatted with `skeleton-subprompt'.  Such an INTERACTOR may also be a list of
strings with the subskeleton being repeated once for each string.

Quoted Lisp expressions are evaluated for their side-effects.
Other Lisp expressions are evaluated and the value treated as above.
Note that expressions may not return `t' since this implies an
endless loop.  Modes can define other symbols by locally setting them
to any valid skeleton element.  The following local variables are
available:

	str	first time: read a string according to INTERACTOR
		then: insert previously read string once more
	help	help-form during interaction with the user or `nil'
	input	initial input (string or cons with index) while reading str
	v1, v2	local variables for memorizing anything you want

When done with skeleton, but before going back to `_'-point call
`skeleton-end-hook' if that is non-`nil'."
  (let ((skeleton-regions regions))
    (and skeleton-regions
	 (setq skeleton-regions
	       (if (> skeleton-regions 0)
		   (list (copy-marker (point) t)
			 (save-excursion (forward-word skeleton-regions)
					 (point-marker)))
		 (setq skeleton-regions (- skeleton-regions))
		 ;; copy skeleton-regions - 1 elements from `mark-ring'
		 (let ((l1 (cons (mark-marker) mark-ring))
		       (l2 (list (copy-marker (point) t))))
		   (while (and l1 (> skeleton-regions 0))
		     (push (copy-marker (pop l1) t) l2)
		     (setq skeleton-regions (1- skeleton-regions)))
		   (sort l2 '<))))
	 (goto-char (car skeleton-regions))
	 (setq skeleton-regions (cdr skeleton-regions)))
    (let ((beg (point))
	  skeleton-modified skeleton-point resume: help input v1 v2)
      (setq skeleton-positions nil)
      (unwind-protect
	  (eval `(let ,skeleton-further-elements
		   (skeleton-internal-list skeleton str)))
	(run-hooks 'skeleton-end-hook)
	(sit-for 0)
	(or (pos-visible-in-window-p beg)
	    (progn
	      (goto-char beg)
	      (recenter 0)))
	(if skeleton-point
	    (goto-char skeleton-point))))))

(defun skeleton-read (prompt &optional initial-input recursive)
  "Function for reading a string from the minibuffer within skeletons.

PROMPT must be a string or a form that evaluates to a string.
It may contain a `%s' which will be replaced by `skeleton-subprompt'.
If non-`nil' second arg INITIAL-INPUT or variable `input' is a string or
cons with index to insert before reading.  If third arg RECURSIVE is non-`nil'
i.e. we are handling the iterator of a subskeleton, returns empty string if
user didn't modify input.
While reading, the value of `minibuffer-help-form' is variable `help' if that
is non-nil or a default string."
  (let ((minibuffer-help-form (or (if (boundp 'help) (symbol-value 'help))
				  (if recursive "\
As long as you provide input you will insert another subskeleton.

If you enter the empty string, the loop inserting subskeletons is
left, and the current one is removed as far as it has been entered.

If you quit, the current subskeleton is removed as far as it has been
entered.  No more of the skeleton will be inserted, except maybe for a
syntactically necessary termination."
				    "\
You are inserting a skeleton.  Standard text gets inserted into the buffer
automatically, and you are prompted to fill in the variable parts.")))
	(eolp (eolp)))
    ;; since Emacs doesn't show main window's cursor, do something noticeable
    (or eolp
	(open-line 1))
    (unwind-protect
	(setq prompt (if (stringp prompt)
			 (read-string (format prompt skeleton-subprompt)
				      (setq initial-input
					    (or initial-input
						(symbol-value 'input))))
		       (eval prompt)))
      (or eolp
	  (delete-char 1))))
  (if (and recursive
	   (or (null prompt)
	       (string= prompt "")
	       (equal prompt initial-input)
	       (equal prompt (car-safe initial-input))))
      (signal 'quit t)
    prompt))

(defun skeleton-internal-list (skeleton &optional str recursive)
  (let* ((start (save-excursion (beginning-of-line) (point)))
	 (column (current-column))
	 (line (buffer-substring start (line-end-position)))
	 opoint)
    (or str
	(setq str `(setq str (skeleton-read ',(car skeleton) nil ,recursive))))
    (when (and (eq (cadr skeleton) '\n)
	       (save-excursion (skip-chars-backward " \t") (bolp)))
      (setq skeleton (cons nil (cons '> (cddr skeleton)))))
    (while (setq skeleton-modified (eq opoint (point))
		 opoint (point)
		 skeleton (cdr skeleton))
      (condition-case quit
	  (skeleton-internal-1 (car skeleton))
	(quit
	 (if (eq (cdr quit) 'recursive)
	     (setq recursive 'quit
		   skeleton (memq 'resume: skeleton))
	   ;; Remove the subskeleton as far as it has been shown
	   ;; the subskeleton shouldn't have deleted outside current line.
	   (end-of-line)
	   (delete-region start (point))
	   (insert line)
	   (move-to-column column)
	   (if (cdr quit)
	       (setq skeleton ()
		     recursive nil)
	     (signal 'quit 'recursive)))))))
  ;; maybe continue loop or go on to next outer resume: section
  (if (eq recursive 'quit)
      (signal 'quit 'recursive)
    recursive))

(defun skeleton-internal-1 (element &optional literal)
  (cond
   ((char-or-string-p element)
    (if (and (integerp element)		; -num
	     (< element 0))
	(if skeleton-untabify
	    (backward-delete-char-untabify (- element))
	  (delete-backward-char (- element)))
      (insert (if (and skeleton-transformation
		       (not literal))
		  (funcall skeleton-transformation element)
		element))))
   ((or (eq element '\n)			; actually (eq '\n 'n)
	;; The sequence `> \n' is handled specially so as to indent the first
	;; line after inserting the newline (to get the proper indentation).
	(and (eq element '>) (eq (nth 1 skeleton) '\n) (pop skeleton)))
    (let ((pos (if (eq element '>) (point))))
      (cond
       ((and skeleton-regions (eq (nth 1 skeleton) '_))
	(or (eolp) (newline))
	(if pos (save-excursion (goto-char pos) (indent-according-to-mode)))
	(indent-region (line-beginning-position)
		       (car skeleton-regions) nil))
       ;; \n as last element only inserts \n if not at eol.
       ((and (null (cdr skeleton)) (eolp))
	(if pos (indent-according-to-mode)))
       (skeleton-newline-indent-rigidly
	(let ((pt (point)))
	  (newline)
	  (indent-to (save-excursion
		       (goto-char pt)
		       (if pos (indent-according-to-mode))
		       (current-indentation)))))
       (t (if pos (reindent-then-newline-and-indent)
	    (newline)
	    (indent-according-to-mode))))))
   ((eq element '>)
    (if (and skeleton-regions (eq (nth 1 skeleton) '_))
	(indent-region (line-beginning-position)
		       (car skeleton-regions) nil)
      (indent-according-to-mode)))
   ((eq element '_)
    (if skeleton-regions
	(progn
	  (goto-char (pop skeleton-regions))
	  (and (<= (current-column) (current-indentation))
	       (eq (nth 1 skeleton) '\n)
	       (end-of-line 0)))
      (or skeleton-point
	  (setq skeleton-point (point)))))
	((eq element '&)
    (when skeleton-modified (pop skeleton)))
   ((eq element '|)
    (unless skeleton-modified (pop skeleton)))
   ((eq element '@)
    (push (point) skeleton-positions)
    (unless skeleton-point (setq skeleton-point (point))))
   ((eq 'quote (car-safe element))
    (eval (nth 1 element)))
   ((or (stringp (car-safe element))
	(consp (car-safe element)))
    (if (symbolp (car-safe (car element)))
	(while (skeleton-internal-list element nil t))
      (setq literal (car element))
      (while literal
	(skeleton-internal-list element (car literal))
	(setq literal (cdr literal)))))
   ((null element))
   (t (skeleton-internal-1 (eval element) t))))

;; Maybe belongs into simple.el or elsewhere
;; ;;;###autoload
;; (define-skeleton local-variables-section
;;  "Insert a local variables section.  Use current comment syntax if any."
;;  (completing-read "Mode: " obarray
;;		   (lambda (symbol)
;;		     (if (commandp symbol)
;;			 (string-match "-mode$" (symbol-name symbol))))
;;		   t)
;;  '(save-excursion
;;     (if (re-search-forward page-delimiter nil t)
;;	 (error "Not on last page")))
;;  comment-start "Local Variables:" comment-end \n
;;  comment-start "mode: " str
;;  & -5 | '(kill-line 0) & -1 | comment-end \n
;;  ( (completing-read (format "Variable, %s: " skeleton-subprompt)
;;		     obarray
;;		     (lambda (symbol)
;;		       (or (eq symbol 'eval)
;;			   (user-variable-p symbol)))
;;		     t)
;;    comment-start str ": "
;;    (read-from-minibuffer "Expression: " nil read-expression-map nil
;;			  'read-expression-history) | _
;;    comment-end \n)
;;  resume:
;;  comment-start "End:" comment-end \n)

;; Variables and command for automatically inserting pairs like () or "".

(defvar skeleton-pair nil
  "*If this is nil pairing is turned off, no matter what else is set.
Otherwise modes with `skeleton-pair-insert-maybe' on some keys
will attempt to insert pairs of matching characters.")


(defvar skeleton-pair-on-word nil
  "*If this is nil, paired insertion is inhibited before or inside a word.")


(defvar skeleton-pair-filter (lambda () nil)
  "Attempt paired insertion if this function returns nil, before inserting.
This allows for context-sensitive checking whether pairing is appropriate.")


(defvar skeleton-pair-alist ()
  "An override alist of pairing partners matched against `last-command-char'.
Each alist element, which looks like (ELEMENT ...), is passed to
`skeleton-insert' with no interactor.  Variable `str' does nothing.

Elements might be (?` ?` _ \"''\"), (?\\( ?  _ \" )\") or (?{ \\n > _ \\n ?} >).")


;;;###autoload
(defun skeleton-pair-insert-maybe (arg)
  "Insert the character you type ARG times.

With no ARG, if `skeleton-pair' is non-nil, pairing can occur.  If the region
is visible the pair is wrapped around it depending on `skeleton-autowrap'.
Else, if `skeleton-pair-on-word' is non-nil or we are not before or inside a
word, and if `skeleton-pair-filter' returns nil, pairing is performed.
Pairing is also prohibited if we are right after a quoting character
such as backslash.

If a match is found in `skeleton-pair-alist', that is inserted, else
the defaults are used.  These are (), [], {}, <> and `' for the
symmetrical ones, and the same character twice for the others."
  (interactive "*P")
  (let ((mark (and skeleton-autowrap
		   (or (eq last-command 'mouse-drag-region)
		       (and transient-mark-mode mark-active))))
	(skeleton-end-hook))
    (if (or arg
	    (not skeleton-pair)
	    (memq (char-syntax (preceding-char)) '(?\\ ?/))
	    (and (not mark)
		 (or overwrite-mode
		     (if (not skeleton-pair-on-word) (looking-at "\\w"))
		     (funcall skeleton-pair-filter))))
	(self-insert-command (prefix-numeric-value arg))
      (setq last-command-char (logand last-command-char 255))
      (or skeleton-abbrev-cleanup
	  (skeleton-insert
	   (cons nil (or (assq last-command-char skeleton-pair-alist)
			 (assq last-command-char '((?( _ ?))
						   (?[ _ ?])
						   (?{ _ ?})
						   (?< _ ?>)
						   (?` _ ?')))
			 `(,last-command-char _ ,last-command-char)))
	   (if mark -1))))))


;; A more serious example can be found in sh-script.el
;;; (defun mirror-mode ()
;;  "This major mode is an amusing little example of paired insertion.
;;All printable characters do a paired self insert, while the other commands
;;work normally."
;;  (interactive)
;;  (kill-all-local-variables)
;;  (make-local-variable 'skeleton-pair)
;;  (make-local-variable 'skeleton-pair-on-word)
;;  (make-local-variable 'skeleton-pair-filter)
;;  (make-local-variable 'skeleton-pair-alist)
;;  (setq major-mode 'mirror-mode
;;	mode-name "Mirror"
;;	skeleton-pair-on-word t
;;	;; in the middle column insert one or none if odd window-width
;;	skeleton-pair-filter (lambda ()
;;			       (if (>= (current-column)
;;				       (/ (window-width) 2))
;;				   ;; insert both on next line
;;				   (next-line 1)
;;				 ;; insert one or both?
;;				 (= (* 2 (1+ (current-column)))
;;				    (window-width))))
;;	;; mirror these the other way round as well
;;	skeleton-pair-alist '((?) _ ?()
;;			      (?] _ ?[)
;;			      (?} _ ?{)
;;			      (?> _ ?<)
;;			      (?/ _ ?\\)
;;			      (?\\ _ ?/)
;;			      (?` ?` _ "''")
;;			      (?' ?' _ "``"))
;;	;; in this mode we exceptionally ignore the user, else it's no fun
;;	skeleton-pair t)
;;  (let ((map (make-vector 256 'skeleton-pair-insert-maybe))
;;	(i 0))
;;    (use-local-map `(keymap ,map))
;;    (while (< i ? )
;;      (aset map i nil)
;;      (aset map (+ i 128) nil)
;;      (setq i (1+ i))))
;;  (run-hooks 'mirror-mode-hook))

(provide 'skeleton)

;;; skeleton.el ends here
