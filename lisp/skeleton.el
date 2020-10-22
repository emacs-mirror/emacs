;;; skeleton.el --- Lisp language extension for writing statement skeletons  -*- lexical-binding: t; -*-

;; Copyright (C) 1993-1996, 2001-2020 Free Software Foundation, Inc.

;; Author: Daniel Pfeiffer <occitan@esperanto.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: extensions, abbrev, languages, tools

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

;; A very concise language extension for writing structured statement
;; skeleton insertion commands for programming language modes.  This
;; originated in shell-script mode and was applied to ada-mode's
;; commands which shrunk to one third.  And these commands are now
;; user configurable.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; page 1:	statement skeleton language definition & interpreter
;; page 2:	paired insertion
;; page 3:	mirror-mode, an example for setting up paired insertion

(defvaralias 'skeleton-transformation 'skeleton-transformation-function)

(defvar skeleton-transformation-function 'identity
  "If non-nil, function applied to literal strings before they are inserted.
It should take strings and characters and return them transformed, or nil
which means no transformation.
Typical examples might be `upcase' or `capitalize'.")

; this should be a fourth argument to defvar
(put 'skeleton-transformation-function 'variable-interactive
     "aTransformation function: ")


(defvar skeleton-autowrap t
  "Controls wrapping behavior of functions created with `define-skeleton'.
When the region is visible (due to `transient-mark-mode' or marking a region
with the mouse) and this is non-nil and the function was called without an
explicit ARG, then the ARG defaults to -1, i.e. wrapping around the visible
region.")
(make-obsolete-variable 'skeleton-autowrap nil "24.5")

(defvar skeleton-end-newline t
  "If non-nil, make sure that the skeleton inserted ends with a newline.")

(defvar skeleton-end-hook nil
  "Hook called at end of skeleton but before going to point of interest.
The variables `v1' and `v2' are still set when calling this.")

(defvaralias 'skeleton-filter 'skeleton-filter-function)

;;;###autoload
(defvar skeleton-filter-function 'identity
  "Function for transforming a skeleton proxy's aliases' variable value.")

(defvar skeleton-untabify nil		; bug#12223
  "When non-nil untabifies when deleting backwards with element -ARG.")

(defvar skeleton-newline-indent-rigidly nil
  "When non-nil, indent rigidly under current line for element `\\n'.
Else use mode's `indent-line-function'.")

(defvar-local skeleton-further-elements ()
  "A buffer-local varlist (see `let') of mode specific skeleton elements.
These variables are bound while interpreting a skeleton.  Their value may
in turn be any valid skeleton element if they are themselves to be used as
skeleton elements.")

(defvar skeleton-subprompt
  (substitute-command-keys
   "RET, \\<minibuffer-local-map>\\[abort-recursive-edit] or \\[help-command]")
  "Replacement for %s in prompts of recursive subskeletons.")


(defvar skeleton-debug nil
  "If non-nil `define-skeleton' will override previous definition.")

(defvar skeleton-positions nil
  "List of positions marked with @, after skeleton insertion.
The list describes the most recent skeleton insertion, and its elements
are integer buffer positions in the reverse order of the insertion order.")

;; reduce the number of compiler warnings
(defvar skeleton-il)
(defvar skeleton-modified)
(defvar skeleton-point)
(defvar skeleton-regions)

(def-edebug-spec skeleton-edebug-spec
  ([&or null stringp (stringp &rest stringp) [[&not atom] sexp]]
   &rest &or "n" "_" "-" ">" "@" "&" "!" "|" "resume:"
   ("quote" def-form) skeleton-edebug-spec def-form))
;;;###autoload
(defmacro define-skeleton (command documentation &rest skeleton)
  "Define a user-configurable COMMAND that enters a statement skeleton.
DOCUMENTATION is that of the command.
SKELETON is as defined under `skeleton-insert'."
  (declare (doc-string 2) (debug (&define name stringp skeleton-edebug-spec)))
  (if skeleton-debug
      (set command skeleton))
  `(progn
     ;; Tell self-insert-command that this function, if called by an
     ;; abbrev, should cause the self-insert to be skipped.
     (put ',command 'no-self-insert t)
     (defun ,command (&optional str arg)
       ,(concat documentation
		(if (string-match "\n\\'" documentation)
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
       (atomic-change-group
         (skeleton-proxy-new ',skeleton str arg)))))

;;;###autoload
(defun skeleton-proxy-new (skeleton &optional str arg)
  "Insert SKELETON.
Prefix ARG allows wrapping around words or regions (see `skeleton-insert').
If no ARG was given, but the region is visible, ARG defaults to -1 depending
on `skeleton-autowrap'.  An ARG of  M-0  will prevent this just for once.
This command can also be an abbrev expansion (3rd and 4th columns in
\\[edit-abbrevs]  buffer: \"\"  command-name).

Optional second argument STR may also be a string which will be the value
of `str' whereas the skeleton's interactor is then ignored."
  (skeleton-insert (funcall skeleton-filter-function skeleton)
		   ;; Pretend  C-x a e  passed its prefix arg to us
		   (if (or arg current-prefix-arg)
		       (prefix-numeric-value (or arg
						 current-prefix-arg))
		     (and skeleton-autowrap
			  (use-region-p)
			  ;; Deactivate the mark, in case one of the
			  ;; elements of the skeleton is sensitive
			  ;; to such situations (e.g. it is itself a
			  ;; skeleton).
			  (progn (deactivate-mark)
				 -1)))
		   (if (stringp str)
		       str))
  ;; Return non-nil to tell expand-abbrev that expansion has happened.
  ;; Otherwise the no-self-insert is ignored.
  t)

;;;###autoload
(defun skeleton-insert (skeleton &optional regions str)
  "Insert the complex statement skeleton SKELETON describes very concisely.

With optional second argument REGIONS, wrap first interesting point
\(`_') in skeleton around next REGIONS words, if REGIONS is positive.
If REGIONS is negative, wrap REGIONS preceding interregions into first
REGIONS interesting positions (successive `_'s) in skeleton.

An interregion is the stretch of text between two contiguous marked
points.  If you marked A B C [] (where [] is the cursor) in
alphabetical order, the 3 interregions are simply the last 3 regions.
But if you marked B A [] C, the interregions are B-A, A-[], []-C.

The optional third argument STR, if specified, is the value for the
variable `str' within the skeleton.  When this is non-nil, the
interactor gets ignored, and this should be a valid skeleton element.

When done with skeleton, but before going back to `_'-point, add
a newline (unless `skeleton-end-newline' is nil) and run the hook
`skeleton-end-hook'.

SKELETON is made up as (INTERACTOR ELEMENT ...).  INTERACTOR may be nil if
not needed, a prompt-string or an expression for complex read functions.

If ELEMENT is a string or a character it gets inserted (see also
`skeleton-transformation-function').  Other possibilities are:

	\\n	go to next line and indent according to mode, unless
                this is the first/last element of a skeleton and point
                is at bol/eol
	_	interesting point, interregion here
	-	interesting point, no interregion interaction, overrides
		interesting point set by _
	>	indent line (or interregion if > _) according to major mode
	@	add position to `skeleton-positions'
	&	do next ELEMENT if previous moved point
	|	do next ELEMENT if previous didn't move point
	-NUM	delete NUM preceding characters (see `skeleton-untabify')
	resume:	skipped, continue here if quit is signaled
	nil	skipped

After termination, point will be positioned at the last occurrence of -
or at the first occurrence of _ or at the end of the inserted text.

Note that \\n as the last element of the skeleton only inserts a
newline if not at eol.  If you want to unconditionally insert a newline
at the end of the skeleton, use \"\\n\" instead.  Likewise with \\n
as the first element when at bol.

Further elements can be defined via `skeleton-further-elements'.
ELEMENT may itself be a SKELETON with an INTERACTOR.  The user is prompted
repeatedly for different inputs.  The SKELETON is processed as often as
the user enters a non-empty string.  \\[keyboard-quit] terminates skeleton insertion, but
continues after `resume:' and positions at `_' if any.  If INTERACTOR in
such a subskeleton is a prompt-string which contains a \".. %s ..\" it is
formatted with `skeleton-subprompt'.  Such an INTERACTOR may also be a list
of strings with the subskeleton being repeated once for each string.

Quoted Lisp expressions are evaluated for their side-effects.
Other Lisp expressions are evaluated and the value treated as above.
Note that expressions may not return t since this implies an
endless loop.  Modes can define other symbols by locally setting them
to any valid skeleton element.  The following local variables are
available:

	str	first time: read a string according to INTERACTOR
		then: insert previously read string once more
	help	help-form during interaction with the user or nil
	input	initial input (string or cons with index) while reading str
	v1, v2	local variables for memorizing anything you want"
  (let ((skeleton-regions regions))
    (and skeleton-regions
	 (setq skeleton-regions
	       (if (> skeleton-regions 0)
		   (list (copy-marker (point) t)
			 (save-excursion (forward-word-strictly
                                          skeleton-regions)
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
	  skeleton-modified skeleton-point) ;; resume:
      (with-suppressed-warnings ((lexical help input v1 v2))
        (dlet (help input v1 v2)
          (setq skeleton-positions nil)
          (unwind-protect
	      (cl-progv
	          (mapcar #'car skeleton-further-elements)
	          (mapcar (lambda (x) (eval (cadr x) t)) skeleton-further-elements)
	        (skeleton-internal-list skeleton str))
	    (or (eolp) (not skeleton-end-newline) (newline-and-indent))
	    (run-hooks 'skeleton-end-hook)
	    (sit-for 0)
	    (or (not (eq (window-buffer) (current-buffer)))
	        (pos-visible-in-window-p beg)
	        (progn
	          (goto-char beg)
	          (recenter 0)))
	    (if skeleton-point
	        (goto-char skeleton-point))))))))

(defun skeleton-read (prompt &optional initial-input recursive)
  "Function for reading a string from the minibuffer within skeletons.

PROMPT must be a string or a function that evaluates to a string.
It may also be a form that evaluates to a string (deprecated).
It may contain a `%s' which will be replaced by `skeleton-subprompt'.
If non-nil second arg INITIAL-INPUT or variable `input' is a string or
cons with index to insert before reading.  If third arg RECURSIVE is non-nil
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
        ;; We used open-line before, but that can do a lot more than we want,
	;; since it runs self-insert-command.  E.g. it may remove spaces
	;; before point.
        (save-excursion (insert "\n")))
    (unwind-protect
	(setq prompt (cond ((stringp prompt)
                            (read-string (format prompt skeleton-subprompt)
                                         (setq initial-input
                                               (or initial-input
                                                   (symbol-value 'input)))))
                           ((functionp prompt)
                            (funcall prompt))
                           (t (eval prompt))))
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
  (let* ((start (line-beginning-position))
	 (column (current-column))
	 (line (buffer-substring start (line-end-position)))
         (skeleton-il skeleton)
         opoint)
    (with-suppressed-warnings ((lexical str))
      (dlet ((str (or str
                      `(setq str
			     (skeleton-read ',(car skeleton-il)
			                    nil ,recursive)))))
	(when (and (eq (cadr skeleton-il) '\n) (not recursive)
	           (save-excursion (skip-chars-backward " \t") (bolp)))
	  (setq skeleton-il (cons nil (cons '> (cddr skeleton-il)))))
	(while (setq skeleton-modified (eq opoint (point))
		     opoint (point)
		     skeleton-il (cdr skeleton-il))
	  (condition-case quit
	      (skeleton-internal-1 (car skeleton-il) nil recursive)
	    (quit
	     (if (eq (cdr quit) 'recursive)
		 (setq recursive 'quit
		       skeleton-il (memq 'resume: skeleton-il))
	       ;; Remove the subskeleton as far as it has been shown
	       ;; the subskeleton shouldn't have deleted outside current line.
	       (end-of-line)
	       (delete-region start (point))
	       (insert line)
	       (move-to-column column)
	       (if (cdr quit)
		   (setq skeleton-il ()
		         recursive nil)
		 (signal 'quit 'recursive)))))))))
  ;; maybe continue loop or go on to next outer resume: section
  (if (eq recursive 'quit)
      (signal 'quit 'recursive)
    recursive))


(defun skeleton-internal-1 (element &optional literal recursive)
  (cond
   ((or (integerp element) (stringp element))
    (if (and (integerp element)		; -num
	     (< element 0))
	(if skeleton-untabify
	    (backward-delete-char-untabify (- element))
	  (delete-char element))
      (insert (if (not literal)
		  (funcall skeleton-transformation-function element)
		element))))
   ((or (eq element '\n)			; actually (eq '\n 'n)
	;; The sequence `> \n' is handled specially so as to indent the first
	;; line after inserting the newline (to get the proper indentation).
	(and (eq element '>) (eq (nth 1 skeleton-il) '\n) (pop skeleton-il)))
    (let ((pos (if (eq element '>) (point))))
      (cond
       ((and skeleton-regions (eq (nth 1 skeleton-il) '_))
	(or (eolp) (insert "\n"))
	(if pos (save-excursion (goto-char pos) (indent-according-to-mode)))
	(indent-region (line-beginning-position)
		       (car skeleton-regions) nil))
       ;; \n as last element only inserts \n if not at eol.
       ((and (null (cdr skeleton-il)) (not recursive) (eolp))
	(if pos (indent-according-to-mode)))
       (skeleton-newline-indent-rigidly
	(let ((pt (point)))
          (insert "\n")
	  (indent-to (save-excursion
		       (goto-char pt)
		       (if pos (indent-according-to-mode))
		       (current-indentation)))))
       (t (if pos (reindent-then-newline-and-indent)
	    (insert "\n")
	    (indent-according-to-mode))))))
   ((eq element '>)
    (if (and skeleton-regions (eq (nth 1 skeleton-il) '_))
	(indent-region (line-beginning-position)
		       (car skeleton-regions) nil)
      (indent-according-to-mode)))
   ((eq element '_)
    (if skeleton-regions
	(progn
	  (goto-char (pop skeleton-regions))
	  (and (<= (current-column) (current-indentation))
	       (eq (nth 1 skeleton-il) '\n)
	       (end-of-line 0)))
      (or skeleton-point
	  (setq skeleton-point (point)))))
   ((eq element '-)
    (setq skeleton-point (point)))
   ((eq element '&)
    (when skeleton-modified (pop skeleton-il)))
   ((eq element '|)
    (unless skeleton-modified (pop skeleton-il)))
   ((eq element '@)
    (push (point) skeleton-positions))
   ((eq 'quote (car-safe element))
    (eval (nth 1 element)))
   ((and (consp element)
	 (or (stringp (car element)) (listp (car element))))
    ;; Don't forget: `symbolp' is also true for nil.
    (if (symbolp (car-safe (car element)))
	(while (and (skeleton-internal-list element nil t)
		    ;; If the interactor is nil, don't infinite loop.
		    (car element)))
      (setq literal (car element))
      (while literal
	(skeleton-internal-list element (car literal))
	(setq literal (cdr literal)))))
   ((null element))
   (t (skeleton-internal-1 (eval element) t recursive))))

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
;;			   (custom-variable-p symbol)))
;;		     t)
;;    comment-start str ": "
;;    (read-from-minibuffer "Expression: " nil read-expression-map nil
;;			  'read-expression-history) | _
;;    comment-end \n)
;;  resume:
;;  comment-start "End:" comment-end \n)

;; Variables and command for automatically inserting pairs like () or "".

(defvar skeleton-pair nil
  "If this is nil pairing is turned off, no matter what else is set.
Otherwise modes with `skeleton-pair-insert-maybe' on some keys
will attempt to insert pairs of matching characters.")


(defvar skeleton-pair-on-word nil
  "If this is nil, paired insertion is inhibited before or inside a word.")


(defvar skeleton-pair-filter-function (lambda () nil)
  "Attempt paired insertion if this function returns nil, before inserting.
This allows for context-sensitive checking whether pairing is appropriate.")


(defvar skeleton-pair-alist ()
  "An override alist of pairing partners matched against `last-command-event'.
Each alist element, which looks like (ELEMENT ...), is passed to
`skeleton-insert' with no interactor.  Variable `str' does nothing.

Elements might be (?\\=` ?\\=` _ \"\\='\\='\"), (?\\( ?  _ \" )\") or (?{ \\n > _ \\n ?} >).")

(defvar skeleton-pair-default-alist '((?\( _ ?\)) (?\))
				      (?\[ _ ?\]) (?\])
				      (?{ _ ?}) (?\})
				      (?< _ ?>) (?\>)
				      (?« _ ?») (?\»)
				      (?` _ ?')))

;;;###autoload
(defun skeleton-pair-insert-maybe (arg)
  "Insert the character you type ARG times.

With no ARG, if `skeleton-pair' is non-nil, pairing can occur.  If the region
is visible the pair is wrapped around it depending on `skeleton-autowrap'.
Else, if `skeleton-pair-on-word' is non-nil or we are not before or inside a
word, and if `skeleton-pair-filter-function' returns nil, pairing is performed.
Pairing is also prohibited if we are right after a quoting character
such as backslash.

If a match is found in `skeleton-pair-alist', that is inserted, else
the defaults are used.  These are (), [], {}, <> and (grave
accent, apostrophe) for the paired ones, and the same character
twice for the others."
  (interactive "*P")
  (if (or arg (not skeleton-pair))
      (self-insert-command (prefix-numeric-value arg))
    (let* ((mark (and skeleton-autowrap
		      (or (eq last-command 'mouse-drag-region)
			  (and transient-mark-mode mark-active))))
	   (char last-command-event)
	   (skeleton (or (assq char skeleton-pair-alist)
			 (assq char skeleton-pair-default-alist)
			 `(,char _ ,char))))
      (if (or (memq (char-syntax (preceding-char)) '(?\\ ?/))
	      (and (not mark)
		   (or overwrite-mode
		       (if (not skeleton-pair-on-word) (looking-at "\\w"))
		       (funcall skeleton-pair-filter-function))))
	  (self-insert-command (prefix-numeric-value arg))
	;; Newlines not desirable for inserting pairs.  See bug#16138.
	(let ((skeleton-end-newline nil))
	  (skeleton-insert (cons nil skeleton) (if mark -1)))))))


;; A more serious example can be found in sh-script.el
;; (defun mirror-mode ()
;;  "This major mode is an amusing little example of paired insertion.
;;All printable characters do a paired self insert, while the other commands
;;work normally."
;;  (interactive)
;;  (kill-all-local-variables)
;;  (make-local-variable 'skeleton-pair)
;;  (make-local-variable 'skeleton-pair-on-word)
;;  (make-local-variable 'skeleton-pair-filter-function)
;;  (make-local-variable 'skeleton-pair-alist)
;;  (setq major-mode 'mirror-mode
;;	mode-name "Mirror"
;;	skeleton-pair-on-word t
;;	;; in the middle column insert one or none if odd window-width
;;	skeleton-pair-filter-function (lambda ()
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
;;  (run-mode-hooks 'mirror-mode-hook))

(provide 'skeleton)

;;; skeleton.el ends here
