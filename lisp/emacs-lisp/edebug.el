;;; edebug.el --- a source-level debugger for emacs lisp

;; Copyright (C) 1988, 1989, 1990, 1991 Free Software Foundation, Inc

;; Author: Daniel LaLiberte <liberte@cs.uiuc.edu>
;; Keywords: lisp, tools, maint

;; This is Dan's 2.5 version with some header comments rearranged to separate
;; the Change Log from the Commentary (so the package-finder code can browse
;; the Commentary).

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;;; Commentary:

;;; This minor mode allows programmers to step through Emacs Lisp source
;;; code while executing, set breakpoints, etc.  See the texinfo
;;; document (being constructed...) for more detailed instructions
;;; than contained here.  Send me your enhancement, ideas, bugs, or
;;; fixes.

;;; Daniel LaLiberte   217-244-0785
;;; University of Illinois, Urbana-Champaign
;;; Department of Computer Science
;;; 1304 W Springfield
;;; Urbana, IL  61801

;;; uiucdcs!liberte
;;; liberte@cs.uiuc.edu

;;; Contents:
;;; =========
;;; Installation
;;; Change list
;;; Utilities
;;; Parser
;;; Debugger


;;; Installation
;;; ------------
;; Put edebug.el in some directory in your load-path and byte-compile it.

;; Put the following forms in your .emacs file.
;; (define-key emacs-lisp-mode-map "\^Xx" 'edebug-defun)
;; (autoload 'edebug-defun "edebug")
;; (autoload 'edebug-debug "edebug")
;; (setq debugger 'edebug-debug)
;; ... other options, described in the next section.

;; Evaluate a defun for edebug with edebug-defun.  
;; Evaluate your function normally.
;; Use the "?" command in edebug to describe other commands.
;; See edebug.texinfo for more instructions.

;;; Change Log:

;;; Revision 2.5  91/07/25  13:32:53  liberte
;;; Doc string cleanup.
;;; If edebug-form-hook is t, evaluate all arguments.
;;; If edebug-form-hook is 0, evaluate no arguments.
;;; If edebug-form-hook is nil, evaluate macro args according
;;; 	to edebug-eval-macro-args.
;;; Save the outside value of executing macro.
;;; Save and restore the outside restriction.
;;; Dont force update for go and Go-nonstop.
;;; Save and restore last-command-char, last-command,
;;; 	this-command, last-input-char.
;;; For epoch, do epoch::dispatch-events before sit-for
;;; 	and input-pending-p since X events could interfere.
;;; Warn about unsetting non-existent breakpoint.
;;; Fix edebug-forward-sexp with prefix arg.
;;; Add edebug-step-out to exit from current sexp.
;;; 
;;; Revision 2.4  91/03/18  12:35:44  liberte
;;; Force update after go or Go-nonstop modes, so overlay arrow is correct.
;;; Support debug-on-quit.  Remove edebug-on-error.
;;; Fix edebug-anonymous.  Bug found by jackr@wpd.sgi.com (Jack Repenning).
;;; Don't discard-input anymore.  Easier to change modes this way.
;;; Fix max-lisp-eval-depth and max-specpdl-size incrementing.
;;; Save and restore points in all buffers, if
;;;         edebug-save-buffer-points is non-nil.  Expensive!
;;;         Bug caught by wolfgang@wsrcc.com (Wolfgang S. Rupprecht)
;;; Save standard-output and standard-input in edebug-recursive-edit
;;;         so that edebug-outside-excursion can restore them.
;;; Call set-buffer in edebug-pop-to-buffer since
;;;         select-window does not do that.
;;; Fix edebug's eval-defun to remember current buffer inside evaluations
;;;         and to evaluate top-level forms.  Found by Jamie Zawinski.
;;; Add edebug-interactive-entry to support interactive forms with
;;;         non-string arg. Bug found by Jack Repenning.
;;; Simplify edebug-restore-match-data to just store-match-data.
;;;         Motivated by linus@lysator.liu.se.
;;; Move the match-data call to before the outside
;;;         buffer is changed, since it assumes that.
;;; 
;;; Revision 2.3  91/01/17  20:55:14  liberte
;;; Fix bug found by hollen@megatek.uucp.
;;; 	Current buffer was not being restored.
;;; Call edebug with (edebug begin end 'exp)
;;; 	and add additional wrapper around body of functions:
;;; 	(edebug-enter function body).
;;; Make &optional only apply to immediate next arg
;;; 	in edebug-form-parser (was edebug-macro-parser).
;;; Catch debug errors with edebug.  Yeah!
;;; Reset edebug-mode on first function entry.  Yeah!
;;; 	Motivated by Dion Hollenbeck.
;;; Add the missing bindings to the global-edebug-map.
;;; eval-current-buffer now uses eval-region.
;;; eval-region now does not narrow region.
;;; 	Narrowing was the cause of the window-start being set wrong.
;;; Reset edebug-mode only on
;;; 	first entry of any function at each recursive-edit level.
;;; Add edebug-backtrace, to generate cleaned up
;;; 	backtrace.  It doesn't "work" like the debug backtrace, however.
;;; Require reselecting outside window even if
;;; 	quit occurs, otherwise save-excursions may restore
;;; 	buffer to the wrong window.
;;; 
;;; Revision 2.2  90/11/26  21:14:22  liberte
;;; Shadow eval-defun and eval-region.  Toggle
;;; 	edebugging with edebug-all-defuns.
;;; Call edebug with (edebug 'function begin end 'exp)
;;; 	Suggested by Jamie Zawinski <jwz@lucid.com>.
;;; Add edebug-form-parser to process macro args.
;;; 	Motivated by Darryl Okahata darrylo@hpnmxx.hp.com.
;;; Fix by Roland McGrath <roland@ai.mit.edu>
;;; 	to wrap body of edebug-save-restriction in progn.
;;; Fix by Darryl Okahata <darrylo%hpnmd@hpcea.hp.com>
;;; 	to add (set-window-hscroll (selected-window) 0) to
;;; 	edebug-pop-to-buffer.
;;; 
;;; Revision 2.1  90/11/16  21:55:35  liberte
;;; Clean up.
;;; Add edebug-form-hook to edebug macro calls. Thanks to Joe Wells.
;;; edebug-forward-sexp uses step mode if no forward-sexp.
;;; 
;;; Revision 2.0  90/11/14  22:30:54  liberte
;;; Handle lambda forms, function, interactive evals, defmacro.
;;; Clean up display for Epoch - save and restore screen configurations.
;;;   Note: epoch 3.2 broke set-window-configuration.
;;;   Also, sit-for pauses do not always work in epoch.
;;; Display evaluations window.
;;; Display result after expression evaluation.
;;;   Thanks to discussions with Shinichirou Sugou.
;;; Conditional and temporary breakpoints.
;;; Change "continue" to "go" mode and add different "continue" mode.
;;; Option to stop before symbols.
;;;
;;; Fix by: Glen Ditchfield  gjditchfield@violet.uwaterloo.ca
;;; to handle ?# type chars.
;;;
;;; Revision 1.5  89/05/10  02:39:27  liberte
;;; Fix condition-case expression lists.
;;; Reorganize edebug.
;;; 
;;; Revision 1.4  89/02/14  22:58:34  liberte
;;; Fix broken breakpointing.
;;; Temporarily widen Emacs Lisp buffer during edebug.
;;; 
;;; Revision 1.3  89/01/30  00:26:09  liberte
;;; More bug fixes for cond and let.
;;; Another parsing fix backquote.
;;; Fix for lambda forms inside defuns.
;;; Leave point at syntax error, mark at starting position.
;;; 
;;; Revision 1.2  88/11/28  12:14:15  liberte
;;; Bug fixes: cond construct didn't execute.
;;;   () in sexp list didn't parse
;;;   () as variable in condition-case didn't parse.
;;; 
;;; Revision 1.1  88/11/28  12:11:27  liberte
;;; Initial revision
;;; 

;;; Code:


;;; Options
;;; -------

(defvar edebug-all-defuns nil
  "*If non-nil, all defuns and defmacros evaluated will use edebug.
eval-defun without prefix arg and eval-region will use edebug-defun.

If nil, eval-region evaluates normally, but eval-defun with prefix arg
uses edebug-defun.  eval-region is called by eval-defun, eval-last-sexp,
and eval-print-last-sexp.

You may wish to make this variable local to each Emacs Lisp buffer by calling
(make-local-variable 'edebug-all-defuns) in your emacs-lisp-mode-hook.
You can use the function edebug-all-defuns to toggle its value.")


(defvar edebug-eval-macro-args nil
  "*If non-nil, edebug will assume that all macro call arguments for
macros that have no edebug-form-hook may be evaluated, otherwise it
will not.  To specify exceptions for macros that have some arguments
evaluated and some not, you should specify an edebug-form-hook")

(defvar edebug-stop-before-symbols nil
  "*Non-nil causes edebug to stop before symbols as well as after.
In any case, it is possible to stop before a symbol with a breakpoint or
interrupt.")

(defvar edebug-save-windows t
  "*If non-nil, save and restore window configuration on edebug calls.
It takes some time to save and restore, so if your program does not care
what happens to the window configurations, it is better to set this
variable to nil.")

(defvar edebug-save-point t
  "*If non-nil, save and restore the point and mark in source code buffers.")

(defvar edebug-save-buffer-points nil
  "*If non-nil, save and restore the points of all buffers, displayed or not.

Saving and restoring buffer points is necessary if you are debugging
code that changes the point of a buffer which is displayed in a
non-selected window.  If edebug or the user then selects the
window, the buffer's point will be changed to the window's point.

Saving and restoring all the points is an expensive operation since it
visits each buffer twice for each edebug call, so it is best to avoid
it if you can.")

(defvar edebug-initial-mode 'step
  "*Global initial mode for edebug, if non-nil.
This is used when edebug is first entered for each recursive-edit level.
Possible values are nil (meaning keep using edebug-mode), step, go,
Go-nonstop, trace, Trace-fast, continue, and Continue-fast.")

(defvar edebug-trace nil
  "*Non-nil if edebug should show a trace of function entry and exit.
Tracing output is displayed in a buffer named *edebug-trace*, one
function entry or exit per line, indented by the recursion level.  You
can customize by replacing functions edebug-print-trace-entry and
edebug-print-trace-exit.")



;;;========================================================================
;;; Utilities
;;; ---------

(defun edebug-which-function ()
  "Return the symbol of the function we are in"
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (down-list 1)
    (if (not (memq (read (current-buffer)) '(defun defmacro)))
	(error "Not in defun or defmacro"))
    (read (current-buffer))))

(defun edebug-last-sexp ()
  "Return the last sexp before point in current buffer.
Assumes Emacs Lisp syntax is active."
  (car
   (read-from-string
    (buffer-substring
     (save-excursion
       (forward-sexp -1)
       (point))
     (point)))))

(defun edebug-window-list ()
  "Return a list of windows, in order of next-window."
  ;; This doesn't work for epoch.
  (let* ((first-window (selected-window))
	 (window-list (list first-window))
	 (next (next-window first-window)))
    (while (not (eq next first-window))
      (setq window-list (cons next window-list))
      (setq next (next-window next)))
    (nreverse window-list)))

(defun edebug-get-buffer-points ()
  "Return a list of buffer point pairs, for all buffers."
  (save-excursion
    (mapcar (function (lambda (buf)
			(set-buffer buf)
			(cons buf (point))))
	    (buffer-list))))

(defun edebug-set-buffer-points ()
  "Restore the buffer-points given by edebug-get-buffer-points."
  (mapcar (function (lambda (buf-point)
		      (if (buffer-name (car buf-point)) ; still exists
			  (progn
			    (set-buffer (car buf-point))
			    (goto-char (cdr buf-point))))))
	  edebug-buffer-points))

(defun edebug-two-window-p ()
  "Return t if there are two windows."
  (and (not (one-window-p))
       (eq (selected-window)
	   (next-window (next-window (selected-window))))))

(defun edebug-macrop (object)
  "Return the macro named by OBJECT, or nil if it is not a macro."
  (while (and (symbolp object) (fboundp object))
    (setq object (symbol-function object)))
  (if (and (listp object)
	   (eq 'macro (car object))
	   (edebug-functionp (cdr object)))
      object))

(defun edebug-functionp (object)
  "Returns the function named by OBJECT, or nil if it is not a function."
  (while (and (symbolp object) (fboundp object))
    (setq object (symbol-function object)))
  (if (or (subrp object)
	  (byte-code-function-p object)
	  (and (listp object)
	       (eq (car object) 'lambda)
	       (listp (car (cdr object)))))
      object))

(defun edebug-sort-alist (alist function)
  "Return the ALIST sorted with comparison function FUNCTION.
This uses 'sort so the sorting is destructive."
  (sort alist (function
	       (lambda (e1 e2)
		 (funcall function (car e1) (car e2))))))

(put 'edebug-save-restriction 'edebug-form-hook
     '(&rest form))

(defmacro edebug-save-restriction (&rest body)
  "Evaluate BODY while saving the current buffers restriction.
BODY may change buffer outside of current restriction, unlike
save-restriction.  BODY may change the current buffer,
and the restriction will be restored to the original buffer,
and the current buffer remains current.
Return the result of the last expression in BODY."
  (` (let ((edebug:s-r-beg (point-min-marker))
	   (edebug:s-r-end (point-max-marker)))
       (unwind-protect
	   (progn (,@ body))
	 (save-excursion
	   (set-buffer (marker-buffer edebug:s-r-beg))
	   (narrow-to-region edebug:s-r-beg edebug:s-r-end))))))


;;;=============================================================
;;; Redefine eval-defun, eval-region, and eval-current-buffer.
;;; -----------------------------------------------------------

(defun edebug-all-defuns ()
  "Toggle edebugging of all defuns and defmacros,
not including those evaluated in the minibuffer, or during load."
  (interactive)
  (setq edebug-all-defuns (not edebug-all-defuns))
  (message "Edebugging is %s." (if edebug-all-defuns "on" "off")))


(if (not (fboundp 'edebug-emacs-eval-defun))
    (fset 'edebug-emacs-eval-defun (symbol-function 'eval-defun)))
;;(fset 'eval-defun (symbol-function 'edebug-emacs-eval-defun))

(defun eval-defun (edebug-debug)
  "Edebug replacement for eval-defun.  Print value in the minibuffer.
Evaluate the top-level form that point is in or before.  Note:
eval-defun normally evaluates any top-level form, not just defuns.  

Here are the differences from the standard eval-defun.  If the prefix
argument is the same as edebug-all-defuns (nil or non-nil), evaluate
normally; otherwise edebug-defun is called to wrap edebug calls around
evaluatable expressions in the defun or defmacro body.  Also, the
value printed by edebug-defun is not just the function name."
  (interactive "P")
  (let ((edebug-all-defuns
	 (not (eq (not edebug-debug) (not edebug-all-defuns)))))
    (edebug-emacs-eval-defun nil)
    ))


(if (not (fboundp 'edebug-emacs-eval-region))
    (fset 'edebug-emacs-eval-region (symbol-function 'eval-region)))
;; (fset 'eval-region (symbol-function 'edebug-emacs-eval-region))

(defun eval-region (edebug-e-r-start edebug-e-r-end
				      &optional edebug-e-r-output)
  "Edebug replacement for eval-defun.
Like eval-region, but call edebug-defun for defuns or defmacros.
Also, this eval-region does not narrow to the region and
if an error occurs, point is left at the error."
  ;; One other piddling difference concerns whitespace after the expression.
  (interactive "r")
  (let ((standard-output (or edebug-e-r-output 'symbolp))
	(edebug-e-r-pnt (point))
	(edebug-e-r-buf (current-buffer))
	(edebug-e-r-inside-buf (current-buffer))
	;; Mark the end because it may move.
	(edebug-e-r-end-marker (set-marker (make-marker) edebug-e-r-end))
	edebug-e-r-val
	)
    (goto-char edebug-e-r-start)
    (edebug-skip-whitespace)
    (while (< (point) edebug-e-r-end-marker)
      (if (and edebug-all-defuns
	       (eq 'lparen (edebug-next-token-class))
	       (save-excursion
		 (forward-char 1)	; skip \(
		 (memq (edebug-read-sexp) '(defun defmacro))))
	  (progn
	    (edebug-defun)
	    ;; Potential problem: edebug-defun always prints name.
	    (forward-sexp 1)		; skip the defun
	    )
	(if (and (eq 'lparen (edebug-next-token-class))
		 (save-excursion
		   (forward-char 1)	; skip \(
		   (memq (edebug-read-sexp) '(defun defmacro))))
	    ;; If it's a defun or defmacro, but not edebug-all-defuns
	    ;; reset the symbols edebug property to be just a marker at
	    ;; the definitions source code.
	    (put (edebug-which-function) 'edebug (point-marker)))

	;; Evaluate normally - after restoring the current-buffer.
	(setq edebug-e-r-val (edebug-read-sexp))
	(save-excursion
	  (set-buffer edebug-e-r-inside-buf)
	  (setq edebug-e-r-val (eval edebug-e-r-val))
	  ;; Remember current buffer for next time.
	  (setq edebug-e-r-inside-buf (current-buffer)))

	(if edebug-e-r-output
	    (progn
	      (setq values (cons edebug-e-r-val values))
	      (if (eq standard-output t)
		  (prin1 edebug-e-r-val)
		(print edebug-e-r-val))))
	)
      (goto-char
       (min (max edebug-e-r-end-marker (point))
	    (progn (edebug-skip-whitespace) (point))))
      )					; while
    (if (null edebug-e-r-output)
	;; do the save-excursion recovery
	(progn
	  ;; but mark is not restored
	  (set-buffer edebug-e-r-buf)
	  (goto-char edebug-e-r-pnt)))
    nil
    ))


(defun edebug-eval-current-buffer (&optional edebug-e-c-b-output)
  "Call eval-region on the whole buffer."
  (interactive)
  (eval-region (point-min) (point-max) edebug-e-c-b-output))

(defun edebug-eval-buffer (&optional buffer edebug-e-c-b-output)
  "Call eval-region on the whole buffer."
  (interactive "bEval buffer: ")
  (save-excursion
    (set-buffer buffer)
    (eval-region (point-min) (point-max) edebug-e-c-b-output)))

;; The standard eval-current-buffer doesn't use eval-region.
(if (and (fboundp 'eval-current-buffer)
	 (not (fboundp 'edebug-emacs-eval-current-buffer)))
    (progn
      (fset 'edebug-emacs-eval-current-buffer
	    (symbol-function 'eval-current-buffer))
      (fset 'eval-current-buffer 'edebug-eval-current-buffer)))
(if (and (fboundp 'eval-buffer)
	 (not (fboundp 'edebug-emacs-eval-buffer)))
    (progn
      (fset 'edebug-emacs-eval-buffer
	    (symbol-function 'eval-buffer))
      (fset 'eval-buffer 'edebug-eval-buffer)))



;;;======================================================================
;;; The Parser
;;; ----------

;;; The top level function for parsing defuns is edebug-defun; it
;;; calls all the rest.  It checks the syntax a bit and leaves point
;;; at any error it finds, but otherwise should appear to work like
;;; eval-defun.

;;; The basic plan is to surround each expression with a call to the
;;; function edebug together with indexes into a table of positions of
;;; all expressions.  Thus an expression "exp" in function foo
;;; becomes:

;;; (edebug 1 2 'exp)

;;; First point moved to to the beginning of exp (offset 1 of the
;;; current function).  Then the expression is evaluated and point is
;;; moved to offset 2, at the end of exp.

;;; The top level expressions of the function are wrapped in a call to
;;; edebug-enter, which supplies the function name and the actual
;;; arguments to the function.  See functions edebug and edebug-enter
;;; for more details.


;;;###autoload
(defun edebug-defun ()
  "Evaluate defun or defmacro, like eval-defun, but with edebug calls.
Print its name in the minibuffer and leave point after any error it finds,
with mark at the original point."
  (interactive)
  (let (def-kind  ; whether defmacro or defun
	 def-name
	 def-args
	 def-docstring
	 defun-interactive
	 (edebug-offset-index 0)
	 edebug-offset-list
	 edebug-func-mark
	 (starting-point (point))
	 tmp-point
	 (parse-sexp-ignore-comments t))
    
    (condition-case err
	(progn
	  (end-of-defun)
	  (beginning-of-defun)
	  (down-list 1)
	  
	  (setq edebug-func-mark (point-marker))
	  (if (not (eq 'defun (setq def-kind (edebug-read-sexp))))
	      (if (not (eq 'defmacro def-kind))
		  (edebug-syntax-error "%s is not a defun or defmacro."
				       def-kind)))
	  (setq def-name (edebug-read-sexp))
	  (if (not (symbolp def-name))
	      (edebug-syntax-error "Bad defun name: %s" def-name))
	  (setq def-args (edebug-read-sexp))
	  (if (not (listp def-args))
	      (edebug-syntax-error "Bad defun arg list: %s" def-args))
	  
	  ;; look for doc string
	  (setq tmp-point (point))
	  (if (eq 'string (edebug-next-token-class))
	      (progn
		(setq def-docstring (edebug-read-sexp))
		(setq tmp-point (point))))
	  
	  ;; look for interactive form
	  (if (eq 'lparen (edebug-next-token-class))
	      (progn
		(forward-char 1)	; skip \(
		(if (eq 'interactive (edebug-read-sexp))
		    (progn
		      (setq defun-interactive
			    (cons 'interactive (edebug-interactive)))
		      (forward-char 1)	; skip \)
		      (setq tmp-point (point))
		      ))))
	  
	  (goto-char tmp-point)
	  
	  ;; build the new definition
	  (fset def-name (` (lambda
			      (, def-args)
			      (, def-docstring)
			      (, defun-interactive)
			      ;; the remainder is a list of sexps
			      (edebug-enter
			       (quote (, def-name))
			       (quote (, def-args))
			       (quote (progn
					(,@ (edebug-sexp-list t)))))
			      )))
	  ;; if it is a defmacro, prepend 'macro
	  (if (eq 'defmacro def-kind)
	      (fset def-name (cons 'macro (symbol-function def-name))))
	  
	  ;; recover point, like save-excursion but only if no error occurs
	  (goto-char starting-point)   

	  ;; store the offset list in functions property list
	  (put def-name 'edebug
	       (list edebug-func-mark
		     nil  ; clear breakpoints
		     (vconcat (nreverse edebug-offset-list))))
	  (message "edebug: %s" def-name)
	  )  ; progn
      
      (invalid-read-syntax
       ;; Set mark at starting-point so user can return.
       ;; Leave point at error.
       (save-excursion  
	 (goto-char starting-point)
	 (set-mark-command nil))
       (message "Syntax error: %s" (cdr err))
;;       (signal 'invalid-read-syntax (cdr err))  ; pass it on, to who?
       )
      ) ; condition-case
    def-name
    ))


(defun edebug-sexp-list (debuggable)
  "Return an edebug form built from the sexp list following point in the
current buffer. If DEBUGGABLE then wrap edebug calls around each sexp.
The sexp list does not start with a left paren; we are already in the list.
Leave point at (before) the trailing right paren."
  (let (sexp-list)
    (while (not (eq 'rparen (edebug-next-token-class)))
      (setq sexp-list (cons (if debuggable
				(edebug-form)
			      (edebug-read-sexp))
			    sexp-list)))
    (nreverse sexp-list)))


(defun edebug-increment-offset ()
  ;; accesses edebug-offset-index and edebug-offset-list
  (setq edebug-offset-index (1+ edebug-offset-index))
  (setq edebug-offset-list (cons (- (point) edebug-func-mark)
				 edebug-offset-list)))


(defun edebug-make-edebug-form (index form)
  "Return the edebug form for the current function at offset INDEX given FORM.
Looks like: (edebug def-name INDEX edebug-offset-index 'FORM).
Also increment the offset index."
  (prog1
      (list 'edebug
	    index
	    edebug-offset-index
	    (list 'quote form))
    (edebug-increment-offset)
    ))


(defun edebug-form  ()
  "Return the debug form for the following form.  Add the point offset
to the edebug-offset-list for the function and move point to
immediately after the form."
  (let* ((index edebug-offset-index)
	 form class)
    ;; The point must be added to the offset list now
    ;; because edebug-list will add more offsets indirectly.
    (edebug-skip-whitespace)
    (edebug-increment-offset)
    (setq class (edebug-next-token-class))
    (cond
     ((eq 'lparen class)
      (edebug-make-edebug-form index (edebug-list)))

     ((eq 'symbol class)
      (if (and (not (memq (setq form (edebug-read-sexp)) '(nil t)))
	       ;; note: symbol includes numbers, see parsing utilities
	       (not (numberp form)))
	  (edebug-make-edebug-form index form)
	form))
     (t (edebug-read-sexp)))))


(defun edebug-list ()
  "Return an edebug form built from the list form that follows point.
Insert debug calls as appropriate to the form.  Start with point at
the left paren.  Leave point after the right paren."
  (let ((beginning (point))
	class
	head)
    
    (forward-char 1)    ; skip \(
    (setq class (edebug-next-token-class))
    (cond
     ((eq 'symbol class)
      (setq head (edebug-read-sexp)))
     ((eq 'lparen class)
      (setq head (edebug-anonymous)))
     ((eq 'rparen class)
      (setq head nil))
     (t (edebug-syntax-error
	 "Head of list must be a symbol or lambda expression.")))
    
    (prog1
	(if head
	    (cons head
		  (cond

;; None of the edebug-form-hooks defined below are used, for speed.
;; They are included for documentation, though the hook would not
;; necessarily behave the same as the function it is replacing.

;;; Using the edebug-form-hooks should work, but would take more time.
;;;		   ((symbolp head)
;;;		    (let ((form (get head 'edebug-form-hook)))
;;;		      (if form
;;;			  (edebug-form-parser form)
;;;			(if (edebug-macrop head)
;;;			    (if edebug-eval-macro-args
;;;				(edebug-sexp-list t)
;;;			      (edebug-sexp-list nil))
;;;			  ;; assume it is a function
;;;			  (edebug-sexp-list t)))))

		   ;; handle all special-forms with unevaluated arguments
		   ((memq head '(let let*)) (edebug-let))
		   ((memq head '(setq setq-default)) (edebug-setq))
		   ((eq head 'cond) (edebug-cond))
		   ((eq head 'condition-case) (edebug-condition-case))

		   ((memq head '(quote ; permits more than one arg
				 defun defvar defconst defmacro))
		    (edebug-sexp-list nil))
		   ((eq head 'function) 
		    (list
		     (if (eq 'lparen (edebug-next-token-class))
			 (edebug-anonymous)
		       (edebug-read-sexp)  ; should be just a symbol
		       )))
		   
		   ;; is it a lisp macro?
		   ((edebug-macrop head)
		    (or (and (symbolp head)
			     (let ((form (get head 'edebug-form-hook)))
                               (if form
				   (if (eq form t)
				       (edebug-sexp-list t)
				     (if (eq form 0)
					 (edebug-sexp-list nil)
				       (edebug-form-parser form))))))
                        (edebug-sexp-list edebug-eval-macro-args)))
   
		   ((eq head 'interactive) 
		    (edebug-syntax-error "interactive not expected here."))
		   
		   ;; otherwise it is a function call
		   (t (edebug-sexp-list t))
		   )))
      
      (if (eq 'rparen (edebug-next-token-class))
	  (forward-char 1) ; skip \)
	(edebug-syntax-error "Too many arguments."))
      )))


(defun edebug-form-parser (args)
  "Parse the macro arguments that follow based on ARGS.  
ARGS describes the types of the arguments of a list form.  Each of the ARGS
is processed left to right, in the same order as the arguments of the
list form.  See the edebug documentation for more details.  The ARGS
may be one of the following:

 symbolp - an unevaluated symbol
 integerp - an unevaluated number
 stringp - an unevaluated string
 vectorp - an unevaluated vector
 atom - an unevaluated number, string, symbol, or vector

 sexp - an unevaluated sexp (atom or list); may not be empty
 form - an evaluated sexp; may not be empty

 foo - any other symbol should be the name of a function; this
	function is called on the argument as a predicate and an error
	is signaled if the predicate fails.

 &optional - one following arg in the list may or may not appear. 
 &rest - all following args are repeated zero or more times as a group.
	This is an extension of the normal meaning of &rest.
 &or - each of the following args are alternatives, processed left to
	right until one succeeds.  There is no way to group
        more than one list element as one alternative.

 (...) - a sublist, of the same format as the top level, processed recursively.
	Special case: if the car of the list is quote, the argument must match
	the quoted sexp (see example below of 'for macro).
"

  (let ((arglist args)
	arg form form-list class
	&optional &rest &or)
    (while (and arglist
		(not (eq 'rparen (setq class (edebug-next-token-class)))))
      (catch 'no-match
	(setq arg (car arglist))
	(setq arglist (cdr arglist))
	(if (and &rest (null arglist))
	    (setq arglist &rest))

	(cond
	 ((memq arg '(&optional &rest &or))
	  ;; remember arglist at this point
	  (set arg arglist)
	  (throw 'no-match nil))

	 ((eq arg 'form)
	  (setq form (edebug-form)))

	 ((eq arg 'sexp)
	  (setq form (edebug-read-sexp)))

	 ((listp arg)
	  (if (eq 'quote (car arg))
	      ;; special case, match the quoted symbol
	      (let ((pnt (point)))
		(setq arg (car (cdr arg)))
		(if (not (eq arg (setq form (edebug-read-sexp))))
		    (edebug-form-parser-error)
		  ))
	    (if (eq class 'lparen)
		(progn
		  (forward-char 1)	; skip \(
		  (setq form (edebug-form-parser arg))
		  (forward-char 1)	; skip \)
		  ))))
	 ((symbolp arg)
	  (let ((pnt (point))
		(pred (if (fboundp arg) (symbol-function arg))))
	    (and pred
		 (not (funcall pred (setq form (edebug-read-sexp))))
		 (edebug-form-parser-error)
		 )))
	 (t (throw 'no-match nil))
	 ) ; cond
	(setq &optional nil)  ; only lasts for one match
	(setq form-list (cons form form-list)) ; skipped by no-match throw
	)) ; while

    (if (and arglist (not (or &optional &rest
			      (memq (car arglist) '(&optional &rest)))))
	(edebug-syntax-error "Not enough arguments."))
    (if (not (eq 'rparen (edebug-next-token-class)))
	(if &or
	    (edebug-syntax-error "Unrecognized argument.")
	  (edebug-syntax-error "Too many arguments.")))
    (nreverse form-list)))


(defun edebug-form-parser-error ()
  (goto-char pnt)
  (if &or
      (throw 'no-match nil)
    (if &optional
	(progn
	  (setq &optional nil)  ; only lasts for one failed match not in &or
	  (throw 'no-match nil))
      (edebug-syntax-error "%s is not %s" form arg))))

;; for loop defined in Emacs Lisp manual
(put 'for 'edebug-form-hook
     '(symbolp 'from form 'to form 'do &rest form))

;; case and do defined in cl.el
(put 'case 'edebug-form-hook
     '(form &rest (sexp form)))

(put 'do 'edebug-form-hook
     '((&rest
        &or symbolp
            (symbolp &optional form
                     &optional form))
        (form &rest form)
        &rest body))

(put 'defvar 'edebug-form-hook 
     (put 'defconst 'edebug-form-hook
	  '(symbolp &optional form &optional stringp)))

(put 'defun 'edebug-form-hook 
     (put 'defmacro 'edebug-form-hook 
	  '(symbolp (&rest symbolp)
		    &optional stringp
		    &optional ('interactive &or stringp form)
		    &rest form)))

(put 'anonymous 'edebug-form-hook 
     '(&optional 'macro 'lambda (&rest symbolp) &rest form))

(defun edebug-anonymous ()
  "Return the edebug form for an anonymous lambda or macro.
Point starts before the left paren and ends after it."
  (forward-char 1)	; skip \(
  (prog1
      (let ((head (edebug-read-sexp)))
	(cond 
	 ((eq head 'lambda)
	  (edebug-lambda))
	 ((eq head 'macro)
	  (if (not (eq 'lambda (edebug-read-sexp)))
	      (edebug-syntax-error "lambda expected."))
	  (cons 'macro (edebug-lambda)))
	 (t (edebug-syntax-error "Anonymous lambda or macro expected."))))
    (forward-char 1)	; skip \)
    ))


(defun edebug-lambda ()
  "Return the edebug form for the lambda form that follows.
Point starts after the lambda symbol and is moved to before the right paren."
  (append
   (list 'lambda (edebug-read-sexp)) ; the args
   (edebug-sexp-list t))) ; the body



(put 'let 'edebug-form-hook
     (put 'let* 'edebug-form-hook
	  '((&rest
	     &or (symbolp &optional form)
	     symbolp)
	    &rest form)))

(defun edebug-let ()
  "Return the edebug form of the let or let* form.
Leave point before the right paren."
  (let (var-value-list
	token
	class)
    (cons
     ;; first process the var/value list
     (if (not (eq 'lparen (edebug-next-token-class)))
	 (if (setq token (edebug-read-sexp))
	     (edebug-syntax-error "Bad var list in let.") ; should be nil
	   token  ; == nil
	   )
       
       (forward-char 1)			; lparen
       (while (not (eq 'rparen (setq class (edebug-next-token-class))))
	 (setq var-value-list
	       (cons
		(if (not (eq 'lparen class))
		    (edebug-read-sexp)
		  (forward-char 1)		; lparen
		  (prog1
		      (edebug-var-value)
		    (if (not (eq 'rparen (edebug-next-token-class)))
			(edebug-syntax-error "Right paren expected in let.")
		      (forward-char 1)		; rparen
		      )))
		var-value-list)))
       (forward-char 1)			; rparen
       (nreverse var-value-list))
     
     ;; now process the expression list
     (edebug-sexp-list t))))


(defun edebug-var-value ()
  "Return the edebug form of the var and optional value that follow point.  
Leave point after the value, if there is one."
  (list
   (edebug-read-sexp) ; the variable
   (and (not (eq 'rparen (edebug-next-token-class)))
	(edebug-form))))


(put 'setq 'edebug-form-hook
     (put 'setq-default 'edebug-form-hook
	  '(&rest symbolp form)))

(defun edebug-setq ()
  "Return the edebug form of the setq or setq-default var-value list."
  (let (var-value-list)
    (while (not (eq 'rparen (edebug-next-token-class)))
      (setq var-value-list
	    (append var-value-list
		    (edebug-var-value))))
    var-value-list))


(put 'interactive 'edebug-form-hook
     '(&optional &or stringp form))

(defun edebug-interactive ()
  "Return the edebug form of the interactive form."
  (list
   (if (not (eq 'rparen (edebug-next-token-class)))
       (if (eq 'string (edebug-next-token-class))
	   (edebug-read-sexp)
	 (prog1
	     (` (edebug-interactive-entry
		 (quote (, def-name))
		 (quote ((,@ (edebug-form))))))
	   (if (not (eq 'rparen (edebug-next-token-class)))
	       (edebug-syntax-error 
		"Only first expression used in interactive form.")))))))


(put 'cond 'edebug-form-hook
     '(&rest (form &rest form)))

(defun edebug-cond ()
  "Return the edebug form of the cond form."
  (let (value-value-list
	class)
    (while (not (eq 'rparen (setq class (edebug-next-token-class))))
      (setq value-value-list
	    (cons
	     (if (not (eq 'lparen class))
		 (let ((thing (edebug-read-sexp)))
		   (if thing
		       (edebug-syntax-error "Condition expected in cond")
		     nil))
	       (forward-char 1) ; \(
	       (prog1
		   (cons
		    (edebug-form)
		    (if (eq 'rparen (edebug-next-token-class))
			nil
		      (edebug-sexp-list t)))
		 (if (not (eq 'rparen (edebug-next-token-class)))
		     (edebug-syntax-error "Right paren expected in cond"))
		 (forward-char 1) ; \)
		 ))
	     value-value-list)))
    (nreverse value-value-list)))


;; Bug: this doesn't support condition name lists
(put 'condition-case 'edebug-form-hook
     '(symbolp
       form
       &rest (symbolp &optional form)))

(defun edebug-condition-case ()
  "Return the edebug form of the condition-case form."
  (cons
   (let (token)
     ;; read the variable or nil
     (setq token (edebug-read-sexp))
     (if (not (symbolp token))
	 (edebug-syntax-error
	  "Variable or nil required for condition-case; found: %s" token))
     token)
   
   (cons
    (edebug-form)			; the form
    
    ;; process handlers
    (let (symb-sexp-list
	  class)
      (while (not (eq 'rparen (setq class (edebug-next-token-class))))
	(setq symb-sexp-list
	      (cons
	       (if (not (eq 'lparen class))
		   (edebug-syntax-error "Bad handler in condition-case.")
		 (forward-char 1)	; \(
		 (prog1
		     (cons 
		      (edebug-read-sexp) ; the error-condition
		      (and (not (eq 'rparen (edebug-next-token-class)))
			   (edebug-sexp-list t)))
		   (forward-char 1)	; \)
		   ))
	       symb-sexp-list)))
      (nreverse symb-sexp-list)))))



;;------------------------------------------------
;; Parser utilities

(defun edebug-syntax-error (msg &rest args)
  "Signal an invalid-read-syntax with MSG and ARGS.  
   This is caught by edebug-defun."
  (signal 'invalid-read-syntax (apply 'format msg args)))


(defun edebug-skip-whitespace ()
  "Leave point before the next token, skipping white space and comments."
  (skip-chars-forward " \t\r\n\f")
  (while (= (following-char) ?\;)
    (skip-chars-forward "^\n\r")  ; skip the comment
    (skip-chars-forward " \t\r\n\f")))

(defun edebug-read-sexp ()
  "Read one sexp from the current buffer starting at point.
Leave point immediately after it.  A sexp can be a list or atom.
An atom is a symbol (or number), character, string, or vector."
  ;; This is gummed up by parser inconsistencies (bugs?)
  (let (token)
    (edebug-skip-whitespace)
    (if (or (= (following-char) ?\[) (= (following-char) ??))
	;; scan-sexps doesn't read vectors or character literals correctly,
	;; but read does.
	(setq token (read (current-buffer)))
      (goto-char
       (min  ; use the lesser of the read and scan-sexps motion
	;; read goes one too far if (quoted) string or symbol
	;; is immediately followed by non-whitespace
	(save-excursion
	  (setq token (read (current-buffer)))
	  (point))
	;; scan-sexps reads too far if a quoting character is read
	(scan-sexps (point) 1))))
    token))

(defconst edebug-syntax-table
  (let ((table (make-vector 256 'symbol)))
    ;; Treat numbers as symbols, because of confusion with -, -1, and 1-.
    (aset table ?\( 'lparen)
    (aset table ?\) 'rparen)
    (aset table ?\' 'quote)
    (aset table ?\" 'string)
    (aset table ?\? 'char)
    (aset table ?\[ 'vector)
    (aset table ?\. 'dot)
    ;; We dont care about any other chars since they wont be seen.
    table)
  "Lookup table for the token class of each character.")

(defun edebug-next-token-class ()
  "Move to the next token and return its class.  We only care about
lparen, rparen, dot, quote, string, char, vector, or symbol."
  (edebug-skip-whitespace)
  (aref edebug-syntax-table (following-char)))


;;;=================================================================
;;; The debugger itself
;;; -------------------


(defvar edebug-active nil
  "Non-nil when edebug is active")


;;; add minor-mode-alist entry
(or (assq 'edebug-active minor-mode-alist)
    (setq minor-mode-alist (cons (list 'edebug-active " *Debugging*")
				 minor-mode-alist)))

(defvar edebug-backtrace nil
  "Stack of active functions evaluated via edebug.
Should be nil at the top level.")

(defvar edebug-offset-indices nil  ; not used yet.
  "Stack of offset indices of visited edebug sexps.
Should be nil at the top level.")

(defvar edebug-entered nil
  "Non-nil if edebug has already been entered at this recursive edit level.")


(defun edebug-enter (edebug-func edebug-args edebug-body)
  "Entering FUNC.  The arguments are ARGS, and the body is BODY.
Setup edebug variables and evaluate BODY.  This function is called
when a function evaluated with edebug-defun is entered.  Return the
result of BODY."

  ;; Is this the first time we are entering edebug since
  ;; lower-level recursive-edit command?
  (if (and (not edebug-entered)
	   edebug-initial-mode)
      ;; Reset edebug-mode to the initial mode.
      (setq edebug-mode edebug-initial-mode))
  (let* ((edebug-entered t)
	 (pre-command-hook (if (memq edebug-func pre-command-hook)
			       nil pre-command-hook))
	 (post-command-hook (if (memq edebug-func post-command-hook)
				nil post-command-hook))
	 (edebug-data  (get edebug-func 'edebug))
	 ;; pull out parts of the edebug-data
	 (edebug-func-mark (car edebug-data))	; mark at function start

	 (edebug-buffer  (marker-buffer edebug-func-mark))
	 (edebug-backtrace (cons edebug-func edebug-backtrace))
	 (max-lisp-eval-depth (+ 6 max-lisp-eval-depth))  ; too much??
	 (max-specpdl-size (+ 10 max-specpdl-size)) ; the args and these vars
	 )
    (if edebug-trace
	(let ((edebug-stack-depth (1- (length edebug-backtrace)))
	      edebug-result)
	  (edebug-print-trace-entry
	   "*edebug-trace*" edebug-func edebug-args edebug-stack-depth)
	  (setq edebug-result (eval edebug-body))
      	  (edebug-print-trace-exit
	   "*edebug-trace*" edebug-func edebug-result edebug-stack-depth)
	  edebug-result)
      (eval edebug-body)
      )))

(defun edebug-interactive-entry (edebug-func edebug-args)
  "Evaluating FUNCs non-string argument of interactive form ARGS."
  (if (and (not edebug-entered)
	   edebug-initial-mode)
      ;; Reset edebug-mode to the initial mode.
      (setq edebug-mode edebug-initial-mode))
  (let* ((edebug-entered t)
	 (edebug-data  (get edebug-func 'edebug))
	 ;; pull out parts of the edebug-data
	 (edebug-func-mark (car edebug-data))	; mark at function start

	 (edebug-buffer  (marker-buffer edebug-func-mark))
;;	 (edebug-backtrace (cons edebug-func edebug-backtrace))
	 )
    (eval edebug-args)))


(defun edebug-print-trace-entry
  (edebug-stream edebug-function edebug-args edebug-stack-depth)
  (edebug-trace-display
   edebug-stream
   "%sEnter: %s\n" (make-string edebug-stack-depth ?\ ) edebug-function)
   )

(defun edebug-print-trace-exit
  (edebug-stream edebug-function edebug-result edebug-stack-depth)
  (edebug-trace-display
   edebug-stream
   "%sExit: %s\n" (make-string edebug-stack-depth ?\ ) edebug-function)
   )


(defun edebug (edebug-before-index edebug-after-index edebug-exp)
  "Debug current function given BEFORE and AFTER positions around EXP.
BEFORE and AFTER are indexes into the position offset vector in the
functions 'edebug property.  edebug is called from functions compiled
with edebug-defun."
  (let ((max-lisp-eval-depth (+ 5 max-lisp-eval-depth)) ; enough??
	(max-specpdl-size (+ 7 max-specpdl-size)) ; the args and these vars
	(edebug-offset-indices
	 (cons edebug-before-index edebug-offset-indices))
	;; Save the outside value of executing macro.
	(edebug-outside-executing-macro executing-macro)
	;; Don't keep reading from an executing kbd macro within edebug!
	(executing-macro nil)
	)
    (if (and (eq edebug-mode 'Go-nonstop)
	     (not (edebug-input-pending-p)))
	;; Just return evalled expression.
	(eval edebug-exp)
      (edebug-debugger edebug-before-index 'enter edebug-exp)
      (edebug-debugger edebug-after-index 'exit (eval edebug-exp))
      )))


(defun edebug-debugger (edebug-offset-index edebug-arg-mode edebug-exp)
  "Determine if edebug display should be updated."
  (let* (
	 ;; This needs to be here since breakpoints may be changed.
	 (edebug-breakpoints (car (cdr edebug-data))) ; list of breakpoints
	 (edebug-break-data (assq edebug-offset-index edebug-breakpoints))
	 (edebug-break
	  (if edebug-break-data
	      (let ((edebug-break-condition
		     (car (cdr edebug-break-data))))
		(or (not edebug-break-condition)
		    (eval edebug-break-condition)))))
	 )
    (if (and edebug-break
	     (car (cdr (cdr edebug-break-data)))) ; is it temporary?
	;; Delete the breakpoint.
	(setcdr edebug-data
		(cons (delq edebug-break-data edebug-breakpoints)
		      (cdr (cdr edebug-data)))))
      
    ;; Dont do anything if mode is go, continue, or Continue-fast
    ;; and no break, and no input.
    (if (or (and (not (memq edebug-mode '(go continue Continue-fast)))
		 (or edebug-stop-before-symbols
		     (not (and (eq edebug-arg-mode 'enter)
			       (symbolp edebug-exp)))))
	    (edebug-input-pending-p)
	    edebug-break)
	(edebug-display))
    
    edebug-exp
    ))


(defvar edebug-window-start 0
  "Remember where each buffers' window starts between edebug calls.
This is to avoid spurious recentering.")

(setq-default edebug-window-start 0)
(make-variable-buffer-local 'edebug-window-start)

(defun edebug-display ()
  "Setup windows for edebug, determine mode, maybe enter recursive-edit."
  ;; uses local variables of edebug-enter, edebug, and edebug-debugger.
  (let ((edebug-active t)		; for minor mode alist
	edebug-stop			; should we enter recursive-edit
	(edebug-point (+ edebug-func-mark
			 (aref (car (cdr (cdr edebug-data)))
			       edebug-offset-index)))
	(edebug-buffer-points
	 (if edebug-save-buffer-points (edebug-get-buffer-points)))
	edebug-window			; window displaying edebug-buffer
	edebug-inside-window		; window displayed after recursive edit
	(edebug-outside-window (selected-window))
	(edebug-outside-buffer (current-buffer))
	(edebug-outside-point (point))
	(edebug-outside-mark (mark t))
	edebug-outside-windows		; window or screen configuration
	edebug-outside-edebug-point	; old point in edebug buffer
	edebug-outside-edebug-mark
	
	edebug-eval-buffer		; declared here so we can kill it below
	(edebug-eval-result-list (and edebug-eval-list
				      (edebug-eval-result-list)))
	(edebug-outside-o-a-p overlay-arrow-position)
	(edebug-outside-o-a-s overlay-arrow-string)
	(edebug-outside-c-i-e-a cursor-in-echo-area)

	edebug-outside-point-min
	edebug-outside-point-max

	overlay-arrow-position
	overlay-arrow-string
	(cursor-in-echo-area nil)
	;; any others??
	)
    (if (not (buffer-name edebug-buffer))
	(let ((debug-on-error nil))
	  (error "Buffer defining %s not found" edebug-func)))
    
      ;; Save windows now before we modify them.
      (if edebug-save-windows
	  (setq edebug-outside-windows
		(edebug-current-window-configuration)))
    
      ;; If edebug-buffer is not currently displayed,
      ;; first find a window for it.
      (edebug-pop-to-buffer edebug-buffer)
      (setq edebug-window (selected-window))

      ;; Now display eval list, if any.
      ;; This is done after the pop to edebug-buffer 
      ;; so that buffer-window correspondence is correct after quit.
      (edebug-eval-display edebug-eval-result-list)
      (select-window edebug-window)
	    
      (if edebug-save-point
	  (progn
	    (setq edebug-outside-edebug-point (point))
	    (setq edebug-outside-edebug-mark (mark t))))

      (edebug-save-restriction
       (setq edebug-outside-point-min (point-min))
       (setq edebug-outside-point-max (point-max))
       (widen)
       (goto-char edebug-point)
	    
       (setq edebug-window-start
	     (edebug-adjust-window edebug-window-start))
	    
       (if (edebug-input-pending-p)	; not including keyboard macros
	   (progn
	     (setq edebug-mode 'step)
	     (setq edebug-stop t)
	     (edebug-stop)
	     ;;	    (discard-input)		; is this unfriendly??
	     ))
       (edebug-overlay-arrow)
	    
       (cond
	((eq 'exit edebug-arg-mode)
	 ;; Display result of previous evaluation.
	 (setq edebug-previous-result edebug-exp)
	 (edebug-previous-result))

	((eq 'error edebug-arg-mode)
	 ;; Display error message
	 (beep)
	 (if (eq 'quit (car edebug-exp))
	     (message "Quit")
	   (message "%s: %s"
		    (get (car edebug-exp) 'error-message)
		    (car (cdr edebug-exp)))))

	(edebug-break
	 (message "Break"))
	(t (message "")))
	    
       (if edebug-break
	   (if (not (memq edebug-mode '(continue Continue-fast)))
	       (setq edebug-stop t)
	     (if (eq edebug-mode 'continue)
		 (edebug-sit-for 1)
	       (edebug-sit-for 0)))
	 ;; not edebug-break
	 (if (eq edebug-mode 'trace)
	     (edebug-sit-for 1)		; Force update and pause.
	   (if (eq edebug-mode 'Trace-fast)
	       (edebug-sit-for 0)	; Force update and continue.
	     )))

       (unwind-protect
	   (if (or edebug-stop
		   (eq edebug-mode 'step)
		   (eq edebug-arg-mode 'error)) 
	       (progn
		 (setq edebug-mode 'step)
		 (edebug-overlay-arrow)	; this doesn't always show up.
		 (edebug-recursive-edit));;   <<<<<< Recursive edit
	     )

	 (if edebug-save-buffer-points
	     (edebug-set-buffer-points))
	 ;; Since we may be in a save-excursion, in case of quit
	 ;; restore the outside window only.
	 (select-window edebug-outside-window)
	 )				; unwind-protect

       ;; None of the following is done if quit or signal occurs.
       (if edebug-save-point
	   ;; Restore point and mark in edebug-buffer.
	   ;; This does the save-excursion recovery only if no quit.
	   ;; If edebug-buffer == edebug-outside-buffer,
	   ;; then this is redundant with outside save-excursion.
	   (progn
	     (set-buffer edebug-buffer)
	     (goto-char edebug-outside-edebug-point)
	     (if (mark-marker)
		 (set-marker (mark-marker) edebug-outside-edebug-mark))
	     ))
       )				; edebug-save-restriction

      ;; Restore windows, buffer, point, and mark.
      (if edebug-save-windows
	  ;; Restore windows before continuing.
	  (edebug-set-window-configuration edebug-outside-windows))
      (set-buffer edebug-outside-buffer)
      (goto-char edebug-outside-point)
      (if (mark-marker)
	  (set-marker (mark-marker) edebug-outside-mark))
      ;; The following is not sufficient, and sometimes annoying.
      ;; (if (memq edebug-mode '(go Go-nonstop))
      ;;    (edebug-sit-for 0))
      ))


(defvar edebug-depth 0
  "Number of recursive edits started by edebug.
Should be 0 at the top level.")

(defvar edebug-recursion-depth 0
  "Value of recursion-depth when edebug was called.")


(defun edebug-recursive-edit ()
  "Start up a recursive edit inside of edebug."
  ;; The current buffer is the edebug-buffer, which is put into edebug-mode.
  (let ((edebug-buffer-read-only buffer-read-only)
	;; match-data must be done in the outside buffer
	(edebug-outside-match-data
	 (save-excursion
	   (set-buffer edebug-outside-buffer)
	   (match-data)))

	(edebug-depth (1+ edebug-depth))
	(edebug-recursion-depth (recursion-depth))
	edebug-entered			; bind locally to nil
	edebug-backtrace-buffer		; each recursive edit gets its own
	;; The window configuration may be saved and restored
	;; during a recursive-edit
	edebug-inside-windows

	(edebug-outside-map (current-local-map))
	(edebug-outside-standard-output standard-output)
	(edebug-outside-standard-input standard-input)

	(edebug-outside-last-command-char last-command-char)
	(edebug-outside-last-command last-command)
	(edebug-outside-this-command this-command)
	(edebug-outside-last-input-char last-input-char)
;;	(edebug-outside-unread-command-char unread-command-char)

	;; Declare the following local variables to protect global values.
	;; We could set these to the values for previous edebug call.
	;; But instead make it local, but use global value.
	(last-command-char last-command-char)
	(last-command last-command) 
	(this-command this-command)
	(last-input-char last-input-char)
	;; Assume no edebug command sets unread-command-events.
;;	(unread-command-char -1)

	(debug-on-error debug-on-error)
	
	;; others??
	)

    (if (and (eq edebug-mode 'go)
	     (not (memq edebug-arg-mode '(exit error))))
	(message "Break"))
    (edebug-mode)
    (if (boundp 'edebug-outside-debug-on-error)
	(setq debug-on-error edebug-outside-debug-on-error))

    (setq buffer-read-only t)
    (unwind-protect
	(recursive-edit)     ;  <<<<<<<<<< Recursive edit

      ;; Do the following, even if quit occurs.
      (if edebug-backtrace-buffer
	  (kill-buffer edebug-backtrace-buffer))
      ;; Could be an option to keep eval display up.
      (if edebug-eval-buffer (kill-buffer edebug-eval-buffer))

      ;; Remember selected-window after recursive-edit.
      (setq edebug-inside-window (selected-window))

      (store-match-data edebug-outside-match-data)

      ;; Recursive edit may have changed buffers,
      ;; so set it back before exiting let.
      (if (buffer-name edebug-buffer)	; if it still exists
	  (progn
	    (set-buffer edebug-buffer)
	    (if (memq edebug-mode '(go Go-nonstop))
		(edebug-overlay-arrow))
	    (setq buffer-read-only edebug-buffer-read-only)
	    (use-local-map edebug-outside-map)
	    ;; Remember current window-start for next visit.
	    (select-window edebug-window)
	    (if (eq edebug-buffer (window-buffer edebug-window))
		(setq edebug-window-start (window-start)))
	    (select-window edebug-inside-window)
	    ))
      )))


;;--------------------------
;; Display related functions

(defun edebug-adjust-window (old-start)
  "Adjust window to fit as much as possible following point.
The display should prefer to start at OLD-START if point is not visible.
Return the new window-start."
  (if (not (pos-visible-in-window-p))
      (progn
	(set-window-start (selected-window) old-start)
	(if (not (pos-visible-in-window-p))
	    (let ((start (window-start))
		  (pnt (point)))
	      (set-window-start
	       (selected-window)
	       (save-excursion
		 (forward-line
		  (if (< pnt start) -1	; one line before
		    (- (/ (window-height) 2)) ; center the line
		    ))
		 (beginning-of-line)
		 (point)))))))
  (window-start))


(defconst edebug-arrow-alist
  '((Continue-fast . ">")
    (Trace-fast . ">")
    (continue . ">")
    (trace . "->")
    (step . "=>")
    (go . "<>")
    (Go-nonstop . "..")  ; not used
    )
  "Association list of arrows for each edebug mode.
If you come up with arrows that make more sense, let me know.")

(defun edebug-overlay-arrow ()
  "Set up the overlay arrow at beginning-of-line in current buffer.
The arrow string is derived from edebug-arrow-alist and edebug-mode."
  (let* ((pos))
    (save-excursion
      (beginning-of-line)
      (setq pos (point)))
    (setq overlay-arrow-string
	  (cdr (assq edebug-mode edebug-arrow-alist)))
    (setq overlay-arrow-position (make-marker))
    (set-marker overlay-arrow-position pos (current-buffer))))


(put 'edebug-outside-excursion 'edebug-form-hook
     '(&rest form))

(defmacro edebug-outside-excursion (&rest body)
  "Evaluate an expression list in the outside context.
Return the result of the last expression."
  (` (save-excursion			; of current-buffer
       (if edebug-save-windows
	   (progn
	     ;; After excursion, we will 
	     ;; restore to current window configuration.
	     (setq edebug-inside-windows
		   (edebug-current-window-configuration))
	     ;; Restore outside windows.
	     (edebug-set-window-configuration edebug-outside-windows)))

       (set-buffer edebug-buffer)
       ;; Restore outside context.
       (let ((edebug-inside-map (current-local-map))
	     (last-command-char edebug-outside-last-command-char)
	     (last-command edebug-outside-last-command)
	     (this-command edebug-outside-this-command)
;;	     (unread-command-char edebug-outside-unread-command-char)
	     (last-input-char edebug-outside-last-input-char)
	     (overlay-arrow-position edebug-outside-o-a-p)
	     (overlay-arrow-string edebug-outside-o-a-s)
	     (cursor-in-echo-area edebug-outside-c-i-e-a)
	     (standard-output edebug-outside-standard-output)
	     (standard-input edebug-outside-standard-input)
	     (executing-macro edebug-outside-executing-macro)
	     )
	 (unwind-protect
	     (save-restriction
	       (narrow-to-region edebug-outside-point-min
				 edebug-outside-point-max)
	       (save-excursion		; of edebug-buffer
		 (if edebug-save-point
		     (progn
		       (goto-char edebug-outside-edebug-point)
		       (if (mark-marker)
			   (set-marker (mark-marker)
				       edebug-outside-edebug-mark))
		       ))
		 (use-local-map edebug-outside-map)
		 (store-match-data edebug-outside-match-data)
		 (select-window edebug-outside-window)
		 (set-buffer edebug-outside-buffer)
		 (goto-char edebug-outside-point)
		 (,@ body)
		 )			; save-excursion
	       )			; save-restriction
	   ;; Back to edebug-buffer.  Restore rest of inside context.
	   (use-local-map edebug-inside-map)
	   (if edebug-save-windows
	       ;; Restore inside windows.
	       (edebug-set-window-configuration edebug-inside-windows))
	   ))				; let
       )))


(defun edebug-toggle-save-windows ()
  "Toggle the edebug-save-windows variable.
Each time you toggle it, the inside and outside window configurations
become the same as the current configuration."
  (interactive)
  (if (setq edebug-save-windows (not edebug-save-windows))
      (setq edebug-inside-windows
	    (setq edebug-outside-windows
		  (edebug-current-window-configuration))))
  (message "Window saving is %s."
	   (if edebug-save-windows "on" "off")))


(defun edebug-where ()
  "Show the debug windows and where we stopped in the program."
  (interactive)
  (if (not edebug-active)
      (error "edebug is not active"))
  (edebug-pop-to-buffer edebug-buffer)
  (goto-char edebug-point)  ; from edebug
  )

(defun edebug-view-outside ()
  "Change to the outside window configuration."
  (interactive)
  (if (not edebug-active)
      (error "edebug is not active"))
  (setq edebug-inside-windows (edebug-current-window-configuration))
  (edebug-set-window-configuration edebug-outside-windows)
  (goto-char edebug-outside-point)
  (message "Window configuration outside of edebug.  Return with %s"
	   (substitute-command-keys "\\<global-map>\\[edebug-where]")))


(defun edebug-bounce-point ()
  "Bounce the point in the outside current buffer."
  (interactive)
  (if (not edebug-active)
      (error "edebug is not active"))
  (save-excursion
    ;; If the buffer's currently displayed, avoid the set-window-configuration.
    (save-window-excursion
      (edebug-pop-to-buffer edebug-outside-buffer)
      ;; (edebug-sit-for 1)			; this shouldnt be necessary
      (goto-char edebug-outside-point)
      ;;	(message "current buffer: %s" (current-buffer))
      (edebug-sit-for 1)
      (edebug-pop-to-buffer edebug-buffer))))



;;--------------------------
;; epoch related things

(defvar edebug-epoch-running (and (boundp 'epoch::version) epoch::version)
  "non-nil if epoch is running.
Windows are handled a little differently under epoch.")


(defun edebug-current-window-configuration ()
  "Return the current window or frame configuration."
  (if edebug-epoch-running
      (edebug-current-screen-configuration)
    (current-window-configuration)))


(defun edebug-set-window-configuration (conf)
  "Set the window or frame configuration to CONF."
  (if edebug-epoch-running
      (edebug-set-screen-configuration conf)
    (set-window-configuration conf)))


(defun edebug-get-buffer-window (buffer)
  (if edebug-epoch-running
      (epoch::get-buffer-window buffer)
    (get-buffer-window buffer)))


(defun edebug-pop-to-buffer (buffer)
  "Like pop-to-buffer, but select a frame that buffer was shown in."
  (let ((edebug-window (edebug-get-buffer-window buffer)))
    (if edebug-window
	(select-window edebug-window)
      ;; It is not currently displayed, so find some place to display it.
      (if edebug-epoch-running
	  ;; Select a screen that the buffer has been displayed in before
	  ;; or the current screen otherwise.
	  (select-screen
	   ;; allowed-screens in epoch 3.2, was called screens before that
	   (or (car (symbol-buffer-value 'allowed-screens buffer))
	       (epoch::current-screen))))
      (if (one-window-p)
	  (split-window))
      (select-window (next-window))
      (set-window-buffer (selected-window) buffer)
      (set-window-hscroll (selected-window) 0)
      ))
  ;; Selecting the window does not set the buffer.
  (set-buffer buffer)
  )


(defun edebug-current-screen-configuration ()
  "Return an object recording the current configuration of Epoch screen-list.  
The object is a list of pairs of the form (SCREEN .  CONFIGURATION)
where SCREEN has window-configuration CONFIGURATION.  The current
screen is the head of the list."
  (let ((screen-list (epoch::screen-list 'unmapped))
	(current-screen (epoch::get-screen))
	(current-buffer (current-buffer))
	)
    ;; put current screen first
    (setq screen-list (cons current-screen (delq current-screen screen-list)))
    (prog1
	(mapcar (function
		 (lambda (screen)
		   (cons screen
			 (progn
			   (epoch::select-screen screen)
			   (current-window-configuration)))))
		screen-list)
      (epoch::select-screen current-screen)
      (set-buffer current-buffer)
      )))

(defun edebug-set-screen-configuration (sc)
  "Set the window-configuration for all the screens in SC.
Set the current screen to be the head of SC."
    (mapcar (function
	     (lambda (screen-conf)
	       (if (epoch::screen-p (car screen-conf))  ; still exist?
		   (progn
		     (epoch::select-screen (car screen-conf))
		     (set-window-configuration (cdr screen-conf))))))
	    sc)
    (if (epoch::screen-p (car (car sc)))
	(epoch::select-screen (car (car sc))))
    )


(defun edebug-sit-for (arg)
  (if edebug-epoch-running
      (epoch::dispatch-events))
  (sit-for arg)
)

(defun edebug-input-pending-p ()
  (if edebug-epoch-running
      (epoch::dispatch-events))
  (input-pending-p)
)



;;--------------------------
;; breakpoint related functions

(defun edebug-find-stop-point ()
  "Return (function . index) of the nearest edebug stop point."
  (let* ((def-name (edebug-which-function))
	 (edebug-data
	  (or (get def-name 'edebug)
	      (error
	       "%s must first be evaluated with edebug-defun" def-name)))
	 ;; pull out parts of edebug-data.
	 (edebug-func-mark (car edebug-data))
	 (edebug-breakpoints (car (cdr edebug-data)))

	 (offset-vector (car (cdr (cdr edebug-data))))
	 (offset (- (save-excursion
		      (if (looking-at "[ \t]")
			  ;; skip backwards until non-whitespace, or bol
			  (skip-chars-backward " \t"))
		      (point))
		    edebug-func-mark))
	 len i)
    ;; the offsets are in order so we can do a linear search
    (setq len (length offset-vector))
    (setq i 0)
    (while (and (< i len) (> offset (aref offset-vector i)))
      (setq i (1+ i)))
    (if (and (< i len)
	     (<= offset (aref offset-vector i)))
	;; return the relevant info
	(cons def-name i)
      (message "Point is not on an expression in %s."
	       def-name)
      )))


(defun edebug-next-breakpoint ()
  "Move point to the next breakpoint, or first if none past point."
  (interactive)
  (let ((edebug-stop-point (edebug-find-stop-point)))
    (if edebug-stop-point
	(let* ((def-name (car edebug-stop-point))
	       (index (cdr edebug-stop-point))
	       (edebug-data (get def-name 'edebug))
	       
	       ;; pull out parts of edebug-data
	       (edebug-func-mark (car edebug-data))
	       (edebug-breakpoints (car (cdr edebug-data)))
	       (offset-vector (car (cdr (cdr edebug-data))))
	       breakpoint)
	  (if (not edebug-breakpoints)
	      (message "No breakpoints in this function.")
	    (let ((breaks edebug-breakpoints))
	      (while (and breaks
			  (<= (car (car breaks)) index))
		(setq breaks (cdr breaks)))
	      (setq breakpoint
		    (if breaks
			(car breaks)
		      ;; goto the first breakpoint
		      (car edebug-breakpoints)))
	      (goto-char (+ edebug-func-mark
			    (aref offset-vector (car breakpoint))))
	      
	      (message (concat (if (car (cdr (cdr breakpoint)))
				   "Temporary " "")
			       (if (car (cdr breakpoint))
				   (format "Condition: %s"
					   (prin1-to-string
					    (car (cdr breakpoint))))
				 "")))
	      ))))))


(defun edebug-modify-breakpoint (flag &optional condition temporary)
  "Modify the breakpoint for the form at point or after it according
to FLAG: set if t, clear if nil.  Then move to that point.
If CONDITION or TEMPORARY are non-nil, add those attributes to
the breakpoint.  "  
  (let ((edebug-stop-point (edebug-find-stop-point)))
    (if edebug-stop-point
	(let* ((def-name (car edebug-stop-point))
	       (index (cdr edebug-stop-point))
	       (edebug-data (get def-name 'edebug))
	       
	       ;; pull out parts of edebug-data
	       (edebug-func-mark (car edebug-data))
	       (edebug-breakpoints (car (cdr edebug-data)))
	       (offset-vector (car (cdr (cdr edebug-data))))
	       present)
	  ;; delete it either way
	  (setq present (assq index edebug-breakpoints))
	  (setq edebug-breakpoints (delq present edebug-breakpoints))
	  (if flag
	      (progn
		;; add it to the list and resort
		(setq edebug-breakpoints
		      (edebug-sort-alist
		       (cons
			(list index condition temporary)
			edebug-breakpoints) '<))
		(message "Breakpoint set in %s." def-name))
	    (if present
		(message "Breakpoint unset in %s." def-name)
	      (message "No breakpoint here.")))
	  
	  (setcdr edebug-data
		  (cons edebug-breakpoints (cdr (cdr edebug-data))))
	  (goto-char (+ edebug-func-mark (aref offset-vector index)))
	  ))))

(defun edebug-set-breakpoint (arg)
  "Set the breakpoint of nearest sexp.
With prefix argument, make it a temporary breakpoint."
  (interactive "P")
  (edebug-modify-breakpoint t nil arg))

(defun edebug-unset-breakpoint ()
  "Clear the breakpoint of nearest sexp."
  (interactive)
  (edebug-modify-breakpoint nil))

(defun edebug-set-conditional-breakpoint (arg condition)
  "Set a conditional breakpoint at nearest sexp.
The condition is evaluated in the outside context.
With prefix argument, make it a temporary breakpoint."
  (interactive "P\nxCondition: ")
  (edebug-modify-breakpoint t condition arg))


;;--------------------------
;; Mode switching functions

(defun edebug-set-mode (mode shortmsg msg)
  "Set the edebug mode to MODE.
Display SHORTMSG, or MSG if not within edebug."
  (interactive)
  (setq edebug-mode mode)
  (if (< 0 edebug-depth)
      (if (eq (current-buffer) edebug-buffer)
	  (progn
	    (message shortmsg)
	    (exit-recursive-edit)))
    (message msg)))


(defun edebug-step-through ()
  "Proceed to next debug step."
  (interactive)
  (edebug-set-mode 'step "" "edebug will stop before next eval."))

(defun edebug-go (arg)
  "Go, evaluating until break.
With ARG set temporary break at stop point and go."
  (interactive "P")
  (if arg
      (edebug-set-breakpoint t))
  (edebug-set-mode 'go "Go..." "edebug will go until break."))

(defun edebug-Go-nonstop ()
  "Go, evaluating without debugging."
  (interactive)
  (edebug-set-mode 'Go-nonstop "Go-Nonstop..."
		   "edebug will not stop at breaks."))

(defun edebug-forward-sexp (arg)
  "Proceed from the current point to the end of the ARGth sexp ahead.
If there are not ARG sexps ahead, then do edebug-step-out."
  (interactive "p")
  (condition-case err
      (let ((parse-sexp-ignore-comments t))
	;; Call forward-sexp repeatedly until done or failure.
	(forward-sexp arg)
	(edebug-go t))
    (error
     (edebug-step-out)
     )))

(defun edebug-step-out ()
  "Proceed from the current point to the end of the containing sexp.
If there is no containing sexp that is not the top level defun,
go to the end of the last sexp, or if that is the same point, then step."
  (interactive)
  (condition-case err
      (let ((parse-sexp-ignore-comments t))
	(up-list 1)
	(save-excursion
	  ;; Is there still a containing expression?
	  (up-list 1))
	(edebug-go t))
    (error
     ;; At top level - 1, so first check if there are more sexps at this level.
     (let ((start-point (point)))
;;       (up-list 1)
       (down-list -1)
       (if (= (point) start-point)
	   (edebug-step-through)	; No more at this level, so step.
	 (edebug-go t)
	 )))))


(defun edebug-goto-here ()
  "Proceed to this stop point."
  (interactive)
  (edebug-go t)
  )

(defun edebug-trace ()
  "Begin trace mode."
  (interactive)
  (edebug-set-mode 'trace "Tracing..." "edebug will trace with pause."))

(defun edebug-Trace-fast ()
  "Trace with no wait at each step."
  (interactive)
  (edebug-set-mode 'Trace-fast
		   "Trace fast..." "edebug will trace without pause."))

(defun edebug-continue ()
  "Begin continue mode."
  (interactive)
  (edebug-set-mode 'continue "Continue..."
		   "edebug will pause at breakpoints."))

(defun edebug-Continue-fast ()
  "Trace with no wait at each step."
  (interactive)
  (edebug-set-mode 'Continue-fast "Continue fast..."
		   "edebug will stop and go at breakpoints."))


(defun edebug-step-in ()
  "Step into the function about to be called.
Do this before the arguments are evaluated since otherwise it will be
too late.  One side effect of using edebug-step-in is that the next
time the function is called, edebug will be called there as well."
  (interactive)
  (if (not (eq 'enter edebug-arg-mode))
      (error "You must be in front of a function or macro call"))
  (let* ((func (car edebug-exp))
	 (func-marker (get func 'edebug)))
    (cond
     ((markerp func-marker)
      (save-excursion
	(set-buffer (marker-buffer func-marker))
	(goto-char func-marker)
	(edebug-defun)))
     ((listp func-marker)
      ;; its already been evaluated for edebug
      nil)
     (t (error "You must first evaluate %s in a buffer" func))))
  (exit-recursive-edit))


;;(defun edebug-exit-out ()
;;  "Go until the current function exits."
;;  (interactive)
;;  (edebug-set-mode 'exiting "Exit..."))


(defun edebug-stop ()
  "Useful for exiting from trace loop."
  (interactive)
  (message "Stop"))


;;; The following initial mode setting definitions are not used yet.

(defconst edebug-initial-mode-alist
  '((edebug-Continue-fast . Continue-fast)
    (edebug-Trace-fast . Trace-fast)
    (edebug-continue . continue)
    (edebug-trace . trace)
    (edebug-go . go)
    (edebug-step-through . step)
    (edebug-Go-nonstop . Go-nonstop)
    )
  "Association list between commands and the modes they set.")


(defun edebug-set-initial-mode ()
  "Ask for the initial mode of the enclosing function.
The mode is requested via the key that would be used to set the mode in
edebug-mode."
  (interactive)
  (let* ((this-function (edebug-which-function))
	 (keymap (if (eq edebug-mode-map (current-local-map))
		     edebug-mode-map))
	 (old-mode (or (get this-function 'edebug-initial-mode)
		       edebug-initial-mode))
	 (key (read-key-sequence
	       (format
		"Change initial edebug mode for %s from %s (%s) to (enter key): "
		       this-function
		       old-mode
		       (where-is-internal
			(car (rassq old-mode edebug-initial-mode-alist))
			keymap 'firstonly
			))))
	 (mode (cdr (assq (key-binding key) edebug-initial-mode-alist)))
	 )
    (if (and mode
	     (or (get this-function 'edebug-initial-mode)
		 (not (eq mode edebug-initial-mode))))
	(progn
	  (put this-function 'edebug-initial-mode mode)
	  (message "Initial mode for %s is now: %s"
		   this-function mode))
      (error "Key must map to one of the mode changing commands")
      )))



;;--------------------------
;; Evaluation of expressions

(defvar edebug-previous-result nil
  "Last result returned from an expression.")

(defun edebug-previous-result ()
  "Return the previous result."
  (interactive)
  (let ((print-escape-newlines t)
	(print-length 20))
    (message "Result: %s" (prin1-to-string edebug-previous-result))))


(defun edebug-eval (expr)
  "Evaluate EXPR in the outside environment."
  (if (not edebug-active)
      (error "edebug is not active"))
  (edebug-outside-excursion
   (eval expr)))

(defun edebug-eval-expression (expr)
  "Prompt and evaluate an expression in the outside environment.  
Print result in minibuffer."
  (interactive "xEval: ")
  (prin1 (edebug-eval expr)))

(defun edebug-eval-last-sexp ()
  "Evaluate sexp before point in the outside environment;
print value in minibuffer."
  (interactive)
  (prin1 (edebug-eval (edebug-last-sexp))))

(defun edebug-eval-print-last-sexp ()
  "Evaluate sexp before point in the outside environment; 
print value into current buffer."
  (interactive)
  (let ((standard-output (current-buffer)))
    (print 
     (condition-case err
	 (edebug-eval (edebug-last-sexp))
       (error (format "%s: %s"
		      (get (car err) 'error-message)
		      (car (cdr err))))))))

;;;---------------------------------
;;; edebug minor mode initialization

(defvar edebug-mode 'step
  "Current edebug mode set by user.")

(defvar edebug-mode-map nil)
(if edebug-mode-map
    nil
  (progn
    (setq edebug-mode-map (copy-keymap emacs-lisp-mode-map))
    ;; control
    (define-key edebug-mode-map " " 'edebug-step-through)
    (define-key edebug-mode-map "g" 'edebug-go)
    (define-key edebug-mode-map "G" 'edebug-Go-nonstop)
    (define-key edebug-mode-map "t" 'edebug-trace)
    (define-key edebug-mode-map "T" 'edebug-Trace-fast)
    (define-key edebug-mode-map "c" 'edebug-continue)
    (define-key edebug-mode-map "C" 'edebug-Continue-fast)

    (define-key edebug-mode-map "f" 'edebug-forward-sexp)
    (define-key edebug-mode-map "h" 'edebug-goto-here)

    (define-key edebug-mode-map "r" 'edebug-previous-result)

    (define-key edebug-mode-map "i" 'edebug-step-in)
    (define-key edebug-mode-map "o" 'edebug-step-out)
    
;;    (define-key edebug-mode-map "m" 'edebug-set-initial-mode)

    (define-key edebug-mode-map "q" 'top-level)
    (define-key edebug-mode-map "a" 'abort-recursive-edit)
    (define-key edebug-mode-map "S" 'edebug-stop)

    ;; breakpoints
    (define-key edebug-mode-map "b" 'edebug-set-breakpoint)
    (define-key edebug-mode-map "u" 'edebug-unset-breakpoint)
    (define-key edebug-mode-map "B" 'edebug-next-breakpoint)
    (define-key edebug-mode-map "x" 'edebug-set-conditional-breakpoint)
    
    ;; evaluation
    (define-key edebug-mode-map "e" 'edebug-eval-expression)
    (define-key edebug-mode-map "\C-x\C-e" 'edebug-eval-last-sexp)
    (define-key edebug-mode-map "E" 'edebug-visit-eval-list)
    
    ;; views
    (define-key edebug-mode-map "w" 'edebug-where)
    (define-key edebug-mode-map "v" 'edebug-view-outside)
    (define-key edebug-mode-map "p" 'edebug-bounce-point)
    (define-key edebug-mode-map "W" 'edebug-toggle-save-windows)
    
    ;; misc
    (define-key edebug-mode-map "?" 'edebug-help)
    (define-key edebug-mode-map "d" 'edebug-backtrace)
    
    (define-key edebug-mode-map "-" 'negative-argument)
    ))

;;;###autoload
(defvar global-edebug-prefix "\^XX"
  "Prefix key for global edebug commands, available from any buffer.")

;;;###autoload
(defvar global-edebug-map nil
  "Global map of edebug commands, available from any buffer.")

;;;###autoload
(if global-edebug-map
    nil
  (setq global-edebug-map (make-sparse-keymap))

  (global-unset-key global-edebug-prefix)
  (global-set-key global-edebug-prefix global-edebug-map)

;;  (define-key global-edebug-map "X" 'edebug-step-through)
  (define-key global-edebug-map "d" 'edebug-defun)
  (define-key global-edebug-map " " 'edebug-step-through)
  (define-key global-edebug-map "g" 'edebug-go)
  (define-key global-edebug-map "G" 'edebug-Go-nonstop)
  (define-key global-edebug-map "t" 'edebug-trace)
  (define-key global-edebug-map "T" 'edebug-Trace-fast)
  (define-key global-edebug-map "c" 'edebug-continue)
  (define-key global-edebug-map "C" 'edebug-Continue-fast)

;;  (define-key global-edebug-map "m" 'edebug-set-initial-mode)
  (define-key global-edebug-map "b" 'edebug-set-breakpoint)
  (define-key global-edebug-map "x" 'edebug-set-conditional-breakpoint)
  (define-key global-edebug-map "u" 'edebug-unset-breakpoint)
  (define-key global-edebug-map "w" 'edebug-where)
  (define-key global-edebug-map "q" 'top-level)
  )


(defun edebug-help ()
  (interactive)
  (describe-function 'edebug-mode))


(defun edebug-mode ()
  "Mode for Emacs Lisp buffers while in edebug.  Under construction.

There are both buffer local and global key bindings to several
functions.  E.g. edebug-step-through is bound to
\\[edebug-step-through] in the debug buffer and
\\<global-map>\\[edebug-step-through] in any buffer.

edebug buffer commands:
\\{edebug-mode-map}

Global commands prefixed by global-edbug-prefix:
\\{global-edebug-map}

Options:
edebug-all-defuns
edebug-eval-macro-args
edebug-stop-before-symbols
edebug-save-windows
edebug-save-point
edebug-save-buffer-points
edebug-initial-mode
edebug-trace
"
  (use-local-map edebug-mode-map))



;;===============================================
;; edebug eval list mode
;; A list of expressions and their evaluations is displayed
;; in edebug-eval-buffer

(defvar edebug-eval-list nil
  "List of expressions to evaluate.")

;;(defvar edebug-eval-buffer "*edebug*"
;;  "*Declared globally so edebug-eval-display can be called independent
;;of edebug (not implemented yet).")


(defun edebug-eval-result-list ()
  "Return a list of evaluations of edebug-eval-list"
  ;; Assumes in outside environment.
  (mapcar (function
	   (lambda (expr)
	     (condition-case err
		 (eval expr)
	       (error (format "%s: %s"
			      (get (car err) 'error-message)
			      (car (cdr err))))
	       )))
	  edebug-eval-list))

(defun edebug-eval-display-list (edebug-eval-result-list)
  ;; Assumes edebug-eval-buffer exists.
  (let ((edebug-eval-list-temp edebug-eval-list)
	(standard-output edebug-eval-buffer)
	(edebug-display-line
	 (format ";%s\n" (make-string (- (window-width) 2) ?-))))
    (edebug-pop-to-buffer edebug-eval-buffer)
    (erase-buffer)
    (while edebug-eval-list-temp
      (prin1 (car edebug-eval-list-temp)) (terpri)
      (prin1 (car edebug-eval-result-list)) (terpri)
      (princ edebug-display-line)
      (setq edebug-eval-list-temp (cdr edebug-eval-list-temp))
      (setq edebug-eval-result-list (cdr edebug-eval-result-list)))
    ))

(defun edebug-create-eval-buffer ()
  (if (not (and edebug-eval-buffer (buffer-name edebug-eval-buffer)))
      (progn
	(set-buffer (setq edebug-eval-buffer (get-buffer-create "*edebug*")))
	(edebug-eval-mode))))

;; Should generalize this to be callable outside of edebug
;; with calls in user functions, e.g. (edebug-eval-display)

(defun edebug-eval-display (edebug-eval-result-list)
  "Display expressions and evaluations in EVAL-LIST.
It modifies the context by popping up the eval display."
  (if edebug-eval-result-list
      (progn
	(edebug-create-eval-buffer)
	(edebug-pop-to-buffer edebug-eval-buffer)
	(edebug-eval-display-list edebug-eval-result-list)
	)))

(defun edebug-eval-redisplay ()
  "Redisplay eval list in outside environment.
May only be called from within edebug-recursive-edit."
  (edebug-create-eval-buffer)
  (edebug-pop-to-buffer edebug-eval-buffer)
  (edebug-outside-excursion
   (edebug-eval-display-list (edebug-eval-result-list))
   ))

(defun edebug-visit-eval-list ()
  (interactive)
  (edebug-eval-redisplay)
  (edebug-pop-to-buffer edebug-eval-buffer))


(defun edebug-update-eval-list ()
  "Replace the evaluation list with the sexps now in the eval buffer."
  (interactive)
  (let ((starting-point (point))
	new-list)
    (goto-char (point-min))
    ;; get the first expression
    (edebug-skip-whitespace)
    (if (not (eobp))
	(progn
	  (forward-sexp 1)
	  (setq new-list (cons (edebug-last-sexp) new-list))))
    
    (while (re-search-forward "^;" nil t)
      (forward-line 1)
      (skip-chars-forward " \t\n\r")
      (if (and (/= ?\; (following-char))
	       (not (eobp)))
	  (progn
	    (forward-sexp 1)
	    (setq new-list (cons (edebug-last-sexp) new-list)))))
    
    (setq edebug-eval-list (nreverse new-list))
    (edebug-eval-redisplay)
    (goto-char starting-point)))


(defun edebug-delete-eval-item ()
  "Delete the item under point and redisplay."
  ;; could add arg to do repeatedly
  (interactive)
  (if (re-search-backward "^;" nil 'nofail)
      (forward-line 1))
  (delete-region
   (point) (progn (re-search-forward "^;" nil 'nofail)
		  (beginning-of-line)
		  (point)))
  (edebug-update-eval-list))



(defvar edebug-eval-mode-map nil
  "Keymap for edebug-eval-mode.  Superset of lisp-interaction-mode.")

(if edebug-eval-mode-map
    nil
  (setq edebug-eval-mode-map (copy-keymap lisp-interaction-mode-map))
  
  (define-key edebug-eval-mode-map "\C-c\C-w" 'edebug-where)
  (define-key edebug-eval-mode-map "\C-c\C-d" 'edebug-delete-eval-item)
  (define-key edebug-eval-mode-map "\C-c\C-u" 'edebug-update-eval-list)
  (define-key edebug-eval-mode-map "\C-x\C-e" 'edebug-eval-last-sexp)
  (define-key edebug-eval-mode-map "\C-j" 'edebug-eval-print-last-sexp)
  )


(defun edebug-eval-mode ()
  "Mode for data display buffer while in edebug.  Under construction.
... ignore the following...
There are both buffer local and global key bindings to several
functions.  E.g. edebug-step-through is bound to
\\[edebug-step-through] in the debug buffer and
\\<global-map>\\[edebug-step-through] in any buffer.

Eval list buffer commands:
\\{edebug-eval-mode-map}

Global commands prefixed by global-edbug-prefix:
\\{global-edebug-map}
"
  (lisp-interaction-mode)
  (setq major-mode 'edebug-eval-mode)
  (setq mode-name "Edebug-Eval")
  (use-local-map edebug-eval-mode-map))


;;========================================
;; Interface with standard debugger.

(setq debugger 'edebug-debug)
;; (setq debugger 'debug)  ; use the default

;; Note that debug and its utilities must be byte-compiled to work, since
;; they depend on the backtrace looking a certain way.

;;;###autoload
(defun edebug-debug (&rest debugger-args)
  "Replacement for debug.  
If an error or quit occurred and we are running an edebugged function,
show where we last were.  Otherwise call debug normally."
  (if (and edebug-backtrace  ; anything active?
	   (eq (recursion-depth) edebug-recursion-depth)
	   )

      ;; Where were we before the error occurred?
      (let ((edebug-offset-index (car edebug-offset-indices))
	    (edebug-arg-mode (car debugger-args))
	    (edebug-exp (car (cdr debugger-args)))
	    edebug-break-data 
	    edebug-break
	    (edebug-outside-debug-on-eror debug-on-error)
	    (debug-on-error nil))
	(edebug-display)
	)

    ;; Otherwise call debug normally.
    ;; Still need to remove extraneous edebug calls from stack.
    (apply 'debug debugger-args)
    ))


(defun edebug-backtrace ()
  "Display a non-working backtrace.  Better than nothing..."
  (interactive)
  (let ((old-buf (current-buffer)))
    (if (not edebug-backtrace-buffer)
	(setq edebug-backtrace-buffer
	      (let ((default-major-mode 'fundamental-mode))
		(generate-new-buffer "*Backtrace*"))))
    (edebug-pop-to-buffer edebug-backtrace-buffer)
    (erase-buffer)
    (let ((standard-output (current-buffer))
	  (print-escape-newlines t)
	  (print-length 50)
	  last-ok-point
	  )
      (setq truncate-lines t)
      (backtrace)

      ;; Clean up the backtrace.
      (goto-char (point-min))
      (delete-region
       (point)
       (progn
	 ;; Everything up to the first edebug is internal.
	 (re-search-forward "^  edebug(")
	 (forward-line 1)
	 (point)))
      (forward-line 1)
      (setq last-ok-point (point))

      ;; Delete interspersed edebug internals.
      (while (re-search-forward "^  edebug" nil t)
	(if (looking-at "-enter")
	    ;; delete extraneous progn at top level of function body
	    (save-excursion
	      (goto-char last-ok-point)
	      (forward-line -1)
	      (setq last-ok-point (point))))
	(forward-line 1)
	(delete-region last-ok-point (point))
	(forward-line 1) ; skip past the good line
	(setq last-ok-point (point))
	)
      )
    (edebug-pop-to-buffer old-buf)
    ))


;;========================================================================
;; Trace display - append text to a buffer, and update display.
;;; e.g.
;;;	 (edebug-trace-display
;;;	  "*trace-point*"
;;;	  "saving: point = %s  window-start = %s\n"
;;;	  (point) (window-start))

(defun edebug-trace-display (buf-name fmt &rest args)
  "In buffer BUF-NAME, display FMT and ARGS at the end and make it visible.
The buffer is created if it does not exist.
You must include newlines in FMT to break lines."
  (let* ((selected-window (selected-window))
	 (buffer (get-buffer-create buf-name))
	 (buf-window))
    (edebug-pop-to-buffer buffer)
    (save-excursion
      (setq buf-window (selected-window))
      (set-buffer buffer)
      (goto-char (point-max))
      (insert (apply 'format fmt args))
      (set-window-point buf-window (point))
      (forward-line (- 1 (window-height buf-window)))
      (set-window-start buf-window (point))
;;      (edebug-sit-for 1)
      (bury-buffer buffer)
      )
    (select-window selected-window)))

(provide 'edebug)

;;; edebug.el ends here
