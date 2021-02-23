;;; edebug.el --- a source-level debugger for Emacs Lisp  -*- lexical-binding: t -*-

;; Copyright (C) 1988-1995, 1997, 1999-2021 Free Software Foundation,
;; Inc.

;; Author: Daniel LaLiberte <liberte@holonexus.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: lisp, tools, maint

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

;; This minor mode allows programmers to step through Emacs Lisp
;; source code while executing functions.  You can also set
;; breakpoints, trace (stopping at each expression), evaluate
;; expressions as if outside Edebug, reevaluate and display a list of
;; expressions, trap errors normally caught by debug, and display a
;; debug style backtrace.

;;; Minimal Instructions
;; =====================

;; First evaluate a defun with C-M-x, then run the function.  Step
;; through the code with SPC, mark breakpoints with b, go until a
;; breakpoint is reached with g, and quit execution with q.  Use the
;; "?" command in edebug to describe other commands.
;; See the Emacs Lisp Reference Manual for more details.

;; If you wish to change the default edebug global command prefix, change:
;; (setq global-edebug-prefix "\C-xX")

;; Edebug was written by
;; Daniel LaLiberte
;; GTE Labs
;; 40 Sylvan Rd
;; Waltham, MA  02254
;; liberte@holonexus.org

;;; Code:

(require 'backtrace)
(require 'macroexp)
(require 'cl-lib)
(require 'seq)
(eval-when-compile (require 'pcase))

;;; Options

(defgroup edebug nil
  "A source-level debugger for Emacs Lisp."
  :group 'lisp)

(defface edebug-enabled-breakpoint '((t :inherit highlight))
  "Face used to mark enabled breakpoints."
  :version "27.1")

(defface edebug-disabled-breakpoint
  '((((class color) (min-colors 88) (background light))
     :background "#ddffdd" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#335533" :extend t))
  "Face used to mark disabled breakpoints."
  :version "27.1")

(defcustom edebug-setup-hook nil
  "Functions to call before edebug is used.
Each time it is set to a new value, Edebug will call those functions
once and then reset `edebug-setup-hook' to nil.  You could use this
to load up Edebug specifications associated with a package you are
using, but only when you also use Edebug."
  :type 'hook)

;; edebug-all-defs and edebug-all-forms need to be autoloaded
;; because the byte compiler binds them; as a result, if edebug
;; is first loaded for a require in a compilation, they will be left unbound.

(defcustom edebug-all-defs nil
  "If non-nil, evaluating defining forms instruments for Edebug.
This applies to `eval-defun', `eval-region', `eval-buffer', and
`eval-current-buffer'.  `eval-region' is also called by
`eval-last-sexp', and `eval-print-last-sexp'.

You can use the command `edebug-all-defs' to toggle the value of this
variable.  You may wish to make it local to each buffer with
\(make-local-variable \\='edebug-all-defs) in your
`emacs-lisp-mode-hook'."
  :type 'boolean)

(defcustom edebug-all-forms nil
  "Non-nil means evaluation of all forms will instrument for Edebug.
This doesn't apply to loading or evaluations in the minibuffer.
Use the command `edebug-all-forms' to toggle the value of this option."
  :type 'boolean)

(defcustom edebug-eval-macro-args nil
  "Non-nil means all macro call arguments may be evaluated.
If this variable is nil, the default, Edebug will *not* wrap
macro call arguments as if they will be evaluated.
For each macro, an `edebug-form-spec' overrides this option.
So to specify exceptions for macros that have some arguments evaluated
and some not, use `def-edebug-spec' to specify an `edebug-form-spec'."
  :type 'boolean)

(defcustom edebug-max-depth 150
  "Maximum recursion depth when instrumenting code.
This limit is intended to stop recursion if an Edebug specification
contains an infinite loop.  When Edebug is instrumenting code
containing very large quoted lists, it may reach this limit and give
the error message \"Too deep - perhaps infinite loop in spec?\".
Make this limit larger to countermand that, but you may also need to
increase `max-lisp-eval-depth' and `max-specpdl-size'."
  :type 'integer
  :version "26.1")

(defcustom edebug-save-windows t
  "If non-nil, Edebug saves and restores the window configuration.
That takes some time, so if your program does not care what happens to
the window configurations, it is better to set this variable to nil.

If the value is a list, only the listed windows are saved and
restored.

`edebug-toggle-save-windows' may be used to change this variable."
  :type '(choice boolean (repeat string)))

(defcustom edebug-save-displayed-buffer-points nil
  "If non-nil, save and restore point in all displayed buffers.

Saving and restoring point in other buffers is necessary if you are
debugging code that changes the point of a buffer that is displayed
in a non-selected window.  If Edebug or the user then selects the
window, the buffer's point will be changed to the window's point.

Saving and restoring point in all buffers is expensive, since it
requires selecting each window twice, so enable this only if you
need it."
  :type 'boolean)

(defcustom edebug-initial-mode 'step
  "Initial execution mode for Edebug, if non-nil.
If this variable is non-nil, it specifies the initial execution mode
for Edebug when it is first activated.  Possible values are step, next,
go, Go-nonstop, trace, Trace-fast, continue, and Continue-fast."
  :type '(choice (const step) (const next) (const go)
		 (const Go-nonstop) (const trace)
		 (const Trace-fast) (const continue)
		 (const Continue-fast)))

(defcustom edebug-trace nil
  "Non-nil means display a trace of function entry and exit.
Tracing output is displayed in a buffer named `*edebug-trace*', one
function entry or exit per line, indented by the recursion level.

You can customize by replacing functions `edebug-print-trace-before'
and `edebug-print-trace-after'."
  :type 'boolean)

(defcustom edebug-test-coverage nil
  "If non-nil, Edebug tests coverage of all expressions debugged.
This is done by comparing the result of each expression with the
previous result.  Coverage is considered OK if two different
results are found.

Use `edebug-display-freq-count' to display the frequency count and
coverage information for a definition."
  :type 'boolean)

(defcustom edebug-continue-kbd-macro nil
  "If non-nil, continue defining or executing any keyboard macro.
Use this with caution since it is not debugged."
  :type 'boolean)


(defcustom edebug-print-length 50
  "If non-nil, default value of `print-length' for printing results in Edebug."
  :type '(choice integer (const nil)))
(defcustom edebug-print-level 50
  "If non-nil, default value of `print-level' for printing results in Edebug."
  :type '(choice integer (const nil)))
(defcustom edebug-print-circle t
  "If non-nil, default value of `print-circle' for printing results in Edebug."
  :type 'boolean)

(defcustom edebug-unwrap-results nil
  "Non-nil if Edebug should unwrap results of expressions.
That is, Edebug will try to remove its own instrumentation from the result.
This is useful when debugging macros where the results of expressions
are instrumented expressions."
  :type 'boolean)

(defcustom edebug-on-error t
  "Value bound to `debug-on-error' while Edebug is active.

If `debug-on-error' is non-nil, that value is still used.

If the value is a list of signal names, Edebug will stop when any of
these errors are signaled from Lisp code whether or not the signal is
handled by a `condition-case'.  This option is useful for debugging
signals that *are* handled since they would otherwise be missed.
After execution is resumed, the error is signaled again."
  :type '(choice (const :tag "off")
		 (repeat :menu-tag "When"
			 :value (nil)
			 (symbol :format "%v"))
		 (const :tag "always" t)))

(defcustom edebug-on-quit t
  "Value bound to `debug-on-quit' while Edebug is active."
  :type 'boolean)

(defcustom edebug-global-break-condition nil
  "If non-nil, an expression to test for at every stop point.
If the result is non-nil, then break.  Errors are ignored."
  :type 'sexp
  :risky t)

(defcustom edebug-sit-for-seconds 1
  "Number of seconds to pause when execution mode is `trace' or `continue'."
  :type 'number)

(defcustom edebug-sit-on-break t
  "Whether or not to pause for `edebug-sit-for-seconds' on reaching a break."
  :type 'boolean
  :version "26.1")

;;; Form spec utilities.

(defun edebug-get-spec (symbol)
  "Return the Edebug spec of a given Lisp expression's head SYMBOL.
The argument is usually a symbol, but it doesn't have to be."
  ;; Get the spec of symbol resolving all indirection.
  (let ((spec nil)
	(indirect symbol))
    (while
        (and (symbolp indirect)
             (setq indirect
                   (function-get indirect 'edebug-form-spec 'macro)))
      ;; (edebug-trace "indirection: %s" edebug-form-spec)
      (setq spec indirect))
    spec))

(define-obsolete-function-alias 'get-edebug-spec #'edebug-get-spec "28.1")

(defun edebug--get-elem-spec (elem)
  "Return the specs of the Edebug element ELEM, if any.
ELEM has to be a symbol."
  (or (get elem 'edebug-elem-spec)
      ;; For backward compatibility, we also allow the use of
      ;; a form's name as a shorthand to refer to its spec.
      (edebug-get-spec elem)))

;;;###autoload
(defun edebug-basic-spec (spec)
  "Return t if SPEC uses only extant spec symbols.
An extant spec symbol is a symbol that is not a function and has a
`edebug-form-spec' property."
  (cond ((listp spec)
	 (catch 'basic
	   (while spec
	     (unless (edebug-basic-spec (car spec)) (throw 'basic nil))
	     (setq spec (cdr spec)))
	   t))
	((symbolp spec)
	 (unless (functionp spec)
           (and (function-get spec 'edebug-form-spec) t)))))

;;; Utilities

(defun edebug-lambda-list-keywordp (object)
  "Return t if OBJECT is a lambda list keyword.
A lambda list keyword is a symbol that starts with `&'."
  (and (symbolp object)
       (= ?& (aref (symbol-name object) 0))))


(defun edebug-last-sexp ()
  ;; Return the last sexp before point in current buffer.
  ;; Assumes Emacs Lisp syntax is active.
  (car
   (read-from-string
    (buffer-substring
     (save-excursion
       (forward-sexp -1)
       (point))
     (point)))))

(defun edebug-window-list ()
  "Return a list of windows, in order of `next-window'."
  ;; This doesn't work for epoch.
  (let (window-list)
    (walk-windows (lambda (w) (push w window-list)))
    (nreverse window-list)))

;; Not used.
'(defun edebug-two-window-p ()
  "Return t if there are two windows."
  (and (not (one-window-p))
       (eq (selected-window)
	   (next-window (next-window)))))

(defun edebug-sort-alist (alist function)
  ;; Return the ALIST sorted with comparison function FUNCTION.
  ;; This uses 'sort so the sorting is destructive.
  (sort alist (lambda (e1 e2)
                (funcall function (car e1) (car e2)))))

;; Not used.
'(defmacro edebug-save-restriction (&rest body)
  "Evaluate BODY while saving the current buffers restriction.
BODY may change buffer outside of current restriction, unlike
save-restriction.  BODY may change the current buffer,
and the restriction will be restored to the original buffer,
and the current buffer remains current.
Return the result of the last expression in BODY."
  (declare (debug t))
  `(let ((edebug:s-r-beg (point-min-marker))
	 (edebug:s-r-end (point-max-marker)))
     (unwind-protect
	 (progn ,@body)
       (with-current-buffer (marker-buffer edebug:s-r-beg)
	 (narrow-to-region edebug:s-r-beg edebug:s-r-end)))))

;;; Display

(defconst edebug-trace-buffer "*edebug-trace*"
  "Name of the buffer to put trace info in.")

(defun edebug-pop-to-buffer (buffer &optional window)
  ;; Like pop-to-buffer, but select window where BUFFER was last shown.
  ;; Select WINDOW if it is provided and still exists.  Otherwise,
  ;; if buffer is currently shown in several windows, choose one.
  ;; Otherwise, find a new window, possibly splitting one.
  ;; FIXME: We should probably just be using `pop-to-buffer'.
  (setq window
	(cond
         ((and (window-live-p window)
	       (eq (window-buffer window) buffer))
	  window)
	 ((eq (window-buffer) buffer)
	  ;; Selected window already displays BUFFER.
	  (selected-window))
	 ((get-buffer-window buffer 0))
	 ((one-window-p 'nomini)
	  ;; When there's one window only, split it.
	  (split-window (minibuffer-selected-window)))
	 ((let ((trace-window (get-buffer-window edebug-trace-buffer)))
	    (catch 'found
	      (dolist (elt (window-list nil 'nomini))
		(unless (or (eq elt (selected-window)) (eq elt trace-window)
			    (window-dedicated-p elt))
		  ;; Found a non-dedicated window not showing
		  ;; `edebug-trace-buffer', use it.
		  (throw 'found elt))))))
	 ;; All windows are dedicated or show `edebug-trace-buffer', split
	 ;; selected one.
	 (t (split-window (minibuffer-selected-window)))))
  (set-window-buffer window buffer)
  (select-window window)
  (unless (memq (framep (selected-frame)) '(nil t pc))
    (x-focus-frame (selected-frame)))
  (set-window-hscroll window 0)) ;; should this be??

(defun edebug-get-displayed-buffer-points ()
  ;; Return a list of buffer point pairs, for all displayed buffers.
  (let (list)
    (walk-windows (lambda (w)
		    (unless (eq w (selected-window))
		      (push (cons (window-buffer w)
				  (window-point w))
			    list))))
    list))


(defun edebug-set-buffer-points (buffer-points)
  ;; Restore the buffer-points created by edebug-get-displayed-buffer-points.
  (save-current-buffer
    (mapcar (lambda (buf-point)
	      (when (buffer-live-p (car buf-point))
		(set-buffer (car buf-point))
		(goto-char (cdr buf-point))))
	    buffer-points)))

(defun edebug-current-windows (which-windows)
  ;; Get either a full window configuration or some window information.
  (if (listp which-windows)
      (mapcar (lambda (window)
                (if (window-live-p window)
                    (list window
                          (window-buffer window)
                          (window-point window)
                          (window-start window)
                          (window-hscroll window))))
	      which-windows)
    (current-window-configuration)))

(defun edebug-set-windows (window-info)
  ;; Set either a full window configuration or some window information.
  (if (listp window-info)
      (mapcar (lambda (one-window-info)
                (if one-window-info
                    (apply (lambda (window buffer point start hscroll)
                             (if (window-live-p window)
                                 (progn
                                   (set-window-buffer window buffer)
                                   (set-window-point window point)
                                   (set-window-start window start)
                                   (set-window-hscroll window hscroll))))
                           one-window-info)))
	      window-info)
    (set-window-configuration window-info)))

;;; Redefine read and eval functions
;; read is redefined to maybe instrument forms.
;; eval-defun is redefined to check edebug-all-forms and edebug-all-defs.

(defun edebug--read (orig &optional stream)
  "Read one Lisp expression as text from STREAM, return as Lisp object.
If STREAM is nil, use the value of `standard-input' (which see).
STREAM or the value of `standard-input' may be:
 a buffer (read from point and advance it)
 a marker (read from where it points and advance it)
 a function (call it with no arguments for each character,
     call it with a char as argument to push a char back)
 a string (takes text from string, starting at the beginning)
 t (read text line using minibuffer and use it).

This version, from Edebug, maybe instruments the expression.  But the
STREAM must be the current buffer to do so.  Whether it instruments is
also dependent on the values of the option `edebug-all-defs' and
the option `edebug-all-forms'."
  (or stream (setq stream standard-input))
  (if (eq stream (current-buffer))
      (edebug-read-and-maybe-wrap-form)
    (funcall (or orig #'read) stream)))

(defvar edebug-result) ; The result of the function call returned by body.

;; We should somehow arrange to be able to do this
;; without actually replacing the eval-defun command.
(defun edebug--eval-defun (orig-fun edebug-it)
  "Setting option `edebug-all-defs' to a non-nil value reverses the meaning
of the prefix argument.  Code is then instrumented when this function is
invoked without a prefix argument.

If acting on a `defun' for FUNCTION, and the function was instrumented,
`Edebug: FUNCTION' is printed in the minibuffer.  If not instrumented,
just FUNCTION is printed."
  (let* ((edebug-all-forms (not (eq (not edebug-it) (not edebug-all-defs))))
	 (edebug-all-defs  edebug-all-forms))
    (funcall orig-fun nil)))

(defun edebug-eval-defun (edebug-it)
  (declare (obsolete "use eval-defun or edebug--eval-defun instead" "28.1"))
  (interactive "P")
  (if (advice-member-p #'edebug--eval-defun 'eval-defun)
      (eval-defun edebug-it)
    (edebug--eval-defun #'eval-defun edebug-it)))

;;;###autoload
(defalias 'edebug-defun 'edebug-eval-top-level-form)

;;;###autoload
(defun edebug-eval-top-level-form ()
  "Evaluate the top level form point is in, stepping through with Edebug.
This is like `eval-defun' except that it steps the code for Edebug
before evaluating it.  It displays the value in the echo area
using `eval-expression' (which see).

If you do this on a function definition such as a defun or defmacro,
it defines the function and instruments its definition for Edebug,
so it will do Edebug stepping when called later.  It displays
`Edebug: FUNCTION' in the echo area to indicate that FUNCTION is now
instrumented for Edebug.

If the current defun is actually a call to `defvar' or `defcustom',
evaluating it this way resets the variable using its initial value
expression even if the variable already has some other value.
\(Normally `defvar' and `defcustom' do not alter the value if there
already is one.)"
  (interactive)
  (eval-expression
   ;; Bind edebug-all-forms only while reading, not while evalling
   ;; but this causes problems while edebugging edebug.
   (let ((edebug-all-forms t)
	 (edebug-all-defs t))
     (eval-sexp-add-defvars
      (edebug-read-top-level-form)))))


(defvar edebug-active nil)  ;; Non-nil when edebug is active

(defun edebug-read-top-level-form ()
  (let ((starting-point (point))
        ;; Don't enter Edebug while doing that, in case we're trying to
        ;; instrument things like end-of-defun.
        (edebug-active t))
    (end-of-defun)
    (beginning-of-defun)
    (prog1
	(edebug-read-and-maybe-wrap-form)
      ;; Recover point, but only if no error occurred.
      (goto-char starting-point))))


;; Compatibility with old versions.
(define-obsolete-function-alias 'edebug-all-defuns #'edebug-all-defs "28.1")

;;;###autoload
(defun edebug-all-defs ()
  "Toggle edebugging of all definitions."
  (interactive)
  (setq edebug-all-defs (not edebug-all-defs))
  (message "Edebugging all definitions is %s."
	   (if edebug-all-defs "on" "off")))


;;;###autoload
(defun edebug-all-forms ()
  "Toggle edebugging of all forms."
  (interactive)
  (setq edebug-all-forms (not edebug-all-forms))
  (message "Edebugging all forms is %s."
	   (if edebug-all-forms "on" "off")))


(defun edebug-install-read-eval-functions ()
  (interactive)
  (add-function :around load-read-function #'edebug--read)
  (advice-add 'eval-defun :around #'edebug--eval-defun))

(defun edebug-uninstall-read-eval-functions ()
  (interactive)
  (remove-function load-read-function #'edebug--read)
  (advice-remove 'eval-defun #'edebug--eval-defun))

;;; Edebug internal data

;; The internal data that is needed for edebugging is kept in the
;; buffer-local variable `edebug-form-data'.

(defvar-local edebug-form-data nil
  "A list of entries associating symbols with buffer regions.
Each entry is an `edebug--form-data' struct with fields:
SYMBOL, BEGIN-MARKER, and END-MARKER.  The markers
are at the beginning and end of an instrumented form and SYMBOL is
a symbol that holds all edebug related information for the form on its
property list.

In the future (haha!), the symbol will be irrelevant and edebug data will
be stored in the definitions themselves rather than in the property
list of a symbol.")

(cl-defstruct (edebug--form-data
               ;; Some callers expect accessors to return nil when passed nil.
               (:type list)
               (:constructor edebug--make-form-data-entry (name begin end))
               (:predicate nil) (:constructor nil) (:copier nil))
  name begin end)

(defsubst edebug-set-form-data-entry (entry name begin end)
  (setf (edebug--form-data-name entry) name) ;; In case name is changed.
  (set-marker (edebug--form-data-begin entry) begin)
  (set-marker (edebug--form-data-end entry) end))

(defun edebug-get-form-data-entry (pnt &optional end-point)
  ;; Find the edebug form data entry which is closest to PNT.
  ;; If END-POINT is supplied, match must be exact.
  ;; Return nil if none found.
  (let ((rest edebug-form-data)
	closest-entry
	(closest-dist 999999))  ;; Need maxint here.
    (while (and rest (< 0 closest-dist))
      (let* ((entry (car rest))
	     (begin (edebug--form-data-begin entry))
	     (dist (- pnt begin)))
	(setq rest (cdr rest))
	(if (and (<= 0 dist)
		 (< dist closest-dist)
		 (or (not end-point)
		     (= end-point (edebug--form-data-end entry)))
		 (<= pnt (edebug--form-data-end entry)))
	    (setq closest-dist dist
		  closest-entry entry))))
    closest-entry))

;; Also need to find all contained entries,
;; and find an entry given a symbol, which should be just assq.

(defun edebug-form-data-symbol ()
  "Return the edebug data symbol of the form where point is in.
If point is not inside an edebuggable form, signal an error."
  (or (edebug--form-data-name (edebug-get-form-data-entry (point)))
      (error "Not inside instrumented form")))

(defun edebug-make-top-form-data-entry (new-entry)
  ;; Make NEW-ENTRY the first element in the `edebug-form-data' list.
  (edebug-clear-form-data-entry new-entry)
  (push new-entry edebug-form-data))

(defun edebug-clear-form-data-entry (entry)
  "If non-nil, clear ENTRY out of the form data.
Maybe clear the markers and delete the symbol's edebug property?"
  (if entry
      (progn
	;; Instead of this, we could just find all contained forms.
	;; (put (car entry) 'edebug nil)   ;
	;; (mapcar #'edebug-clear-form-data-entry   ; dangerous
	;;   (get (car entry) 'edebug-dependents))
	;; (set-marker (nth 1 entry) nil)
	;; (set-marker (nth 2 entry) nil)
	(setq edebug-form-data (delq entry edebug-form-data)))))

;;; Parser utilities

(defun edebug-syntax-error (&rest args)
  ;; Signal an invalid-read-syntax with ARGS.
  (signal 'invalid-read-syntax args))


(defconst edebug-read-syntax-table
  ;; Lookup table for significant characters indicating the class of the
  ;; token that follows.  This is not a \"real\" syntax table.
  (let ((table (make-char-table 'syntax-table 'symbol))
	(i 0))
    (while (< i ?!)
      (aset table i 'space)
      (setq i (1+ i)))
    (aset table ?\( 'lparen)
    (aset table ?\) 'rparen)
    (aset table ?\' 'quote)
    (aset table ?\` 'backquote)
    (aset table ?\, 'comma)
    (aset table ?\" 'string)
    (aset table ?\? 'char)
    (aset table ?\[ 'lbracket)
    (aset table ?\] 'rbracket)
    (aset table ?\. 'dot)
    (aset table ?\# 'hash)
    ;; We treat numbers as symbols, because of confusion with -, -1, and 1-.
    ;; We don't care about any other chars since they won't be seen.
    table))

(defun edebug-next-token-class ()
  ;; Move to the next token and return its class.  We only care about
  ;; lparen, rparen, dot, quote, backquote, comma, string, char, vector,
  ;; or symbol.
  (edebug-skip-whitespace)
  (if (and (eq (following-char) ?.)
	   (save-excursion
	     (forward-char 1)
	     (or (and (eq (aref edebug-read-syntax-table (following-char))
			  'symbol)
		      (not (= (following-char) ?\;)))
		 (memq (following-char) '(?\, ?\.)))))
      'symbol
    (aref edebug-read-syntax-table (following-char))))


(defun edebug-skip-whitespace ()
  ;; Leave point before the next token, skipping white space and comments.
  (skip-chars-forward " \t\r\n\f")
  (while (= (following-char) ?\;)
    (skip-chars-forward "^\n")  ; skip the comment
    (skip-chars-forward " \t\r\n\f")))


;; Mostly obsolete reader; still used in one case.

(defun edebug-read-sexp ()
  ;; Read one sexp from the current buffer starting at point.
  ;; Leave point immediately after it.  A sexp can be a list or atom.
  ;; An atom is a symbol (or number), character, string, or vector.
  ;; This works for reading anything legitimate, but it
  ;; is gummed up by parser inconsistencies (bugs?)
  (let ((class (edebug-next-token-class)))
    (cond
     ;; read goes one too far if a (possibly quoted) string or symbol
     ;; is immediately followed by non-whitespace.
     ((eq class 'symbol) (read (current-buffer)))
     ((eq class 'string) (read (current-buffer)))
     ((eq class 'quote) (forward-char 1)
      (list 'quote (edebug-read-sexp)))
     ((eq class 'backquote) (forward-char 1)
      (list '\` (edebug-read-sexp)))
     ((eq class 'comma) (forward-char 1)
      (list '\, (edebug-read-sexp)))
     (t ; anything else, just read it.
      (read (current-buffer))))))

;;; Offsets for reader

(defun edebug-get-edebug-or-ghost (name)
  "Get NAME's value of property `edebug' or property `ghost-edebug'.

The idea is that should function NAME be recompiled whilst
debugging is in progress, property `edebug' will get set to a
marker.  The needed data will then come from property
`ghost-edebug'."
  (let ((e (get name 'edebug)))
    (if (consp e)
        e
      (let ((g (get name 'ghost-edebug)))
        (if (consp g)
            g
          e)))))

;; Define a structure to represent offset positions of expressions.
;; Each offset structure looks like: (before . after) for constituents,
;; or for structures that have elements: (before <subexpressions> . after)
;; where the <subexpressions> are the offset structures for subexpressions
;; including the head of a list.
(defvar edebug-offsets nil)

;; Stack of offset structures in reverse order of the nesting.
;; This is used to get back to previous levels.
(defvar edebug-offsets-stack nil)
(defvar edebug-current-offset nil) ; Top of the stack, for convenience.

;; The association list of objects read with the #n=object form.
;; Each member of the list has the form (n . object), and is used to
;; look up the object for the corresponding #n# construct.
(defvar edebug-read-objects nil)

;; We must store whether we just read a list with a dotted form that
;; is itself a list.  This structure will be condensed, so the offsets
;; must also be condensed.
(defvar edebug-read-dotted-list nil)

(defsubst edebug-initialize-offsets ()
  ;; Reinitialize offset recording.
  (setq edebug-current-offset nil))

(defun edebug-store-before-offset (point)
  ;; Add a new offset pair with POINT as the before offset.
  (let ((new-offset (list point)))
    (if edebug-current-offset
	(setcdr edebug-current-offset
		(cons new-offset (cdr edebug-current-offset)))
      ;; Otherwise, we are at the top level, so initialize.
      (setq edebug-offsets new-offset
	    edebug-offsets-stack nil
	    edebug-read-dotted-list nil))
    ;; Cons the new offset to the front of the stack.
    (setq edebug-offsets-stack (cons new-offset edebug-offsets-stack)
	  edebug-current-offset new-offset)
    ))

(defun edebug-store-after-offset (point)
  ;; Finalize the current offset struct by reversing it and
  ;; store POINT as the after offset.
  (if (not edebug-read-dotted-list)
      ;; Just reverse the offsets of all subexpressions.
      (setcdr edebug-current-offset (nreverse (cdr edebug-current-offset)))

    ;; We just read a list after a dot, which will be abbreviated out.
    (setq edebug-read-dotted-list nil)
    ;; Drop the corresponding offset pair.
    ;; That is, nconc the reverse of the rest of the offsets
    ;; with the cdr of last offset.
    (setcdr edebug-current-offset
	    (nconc (nreverse (cdr (cdr edebug-current-offset)))
		   (cdr (car (cdr edebug-current-offset))))))

  ;; Now append the point using nconc.
  (setq edebug-current-offset (nconc edebug-current-offset point))
  ;; Pop the stack.
  (setq edebug-offsets-stack (cdr edebug-offsets-stack)
	edebug-current-offset (car edebug-offsets-stack)))

(defun edebug-ignore-offset ()
  ;; Ignore the last created offset pair.
  (setcdr edebug-current-offset (cdr (cdr edebug-current-offset))))

(defmacro edebug-storing-offsets (point &rest body)
  (declare (debug (form body)) (indent 1))
  `(unwind-protect
       (progn
	 (edebug-store-before-offset ,point)
	 ,@body)
     (edebug-store-after-offset (point))))


;;; Reader for Emacs Lisp.

;; Uses edebug-next-token-class (and edebug-skip-whitespace) above.

(defconst edebug-read-alist
  '((symbol . edebug-read-symbol)
    (lparen . edebug-read-list)
    (string . edebug-read-string)
    (quote . edebug-read-quote)
    (backquote . edebug-read-backquote)
    (comma . edebug-read-comma)
    (lbracket . edebug-read-vector)
    (hash . edebug-read-special)
    ))

(defun edebug-read-storing-offsets (stream)
  (let (edebug-read-dotted-list) ; see edebug-store-after-offset
    (edebug-storing-offsets (point)
      (funcall
       (or (cdr (assq (edebug-next-token-class) edebug-read-alist))
	   ;; anything else, just read it.
	   #'read)
       stream))))

(defalias 'edebug-read-symbol #'read)
(defalias 'edebug-read-string #'read)

(defun edebug-read-quote (stream)
  ;; Turn 'thing into (quote thing)
  (forward-char 1)
  (list
   (edebug-storing-offsets (1- (point)) 'quote)
   (edebug-read-storing-offsets stream)))

(defun edebug-read-backquote (stream)
  ;; Turn `thing into (\` thing)
  (forward-char 1)
  (list
   (edebug-storing-offsets (1- (point)) '\`)
   (edebug-read-storing-offsets stream)))

(defun edebug-read-comma (stream)
  ;; Turn ,thing into (\, thing).  Handle ,@ and ,. also.
  (let ((opoint (point)))
    (forward-char 1)
    (let ((symbol '\,))
      (cond ((eq (following-char) ?\.)
	     (setq symbol '\,\.)
	     (forward-char 1))
	    ((eq (following-char) ?\@)
	     (setq symbol '\,@)
	     (forward-char 1)))
      ;; Generate the same structure of offsets we would have
      ;; if the resulting list appeared verbatim in the input text.
      (list
       (edebug-storing-offsets opoint symbol)
       (edebug-read-storing-offsets stream)))))

(defun edebug-read-special (stream)
  "Read from STREAM a Lisp object beginning with #.
Turn #'thing into (function thing) and handle the read syntax for
circular objects.  Let `read' read everything else."
  (catch 'return
    (forward-char 1)
    (let ((start (point)))
      (cond
       ((eq ?\' (following-char))
        (forward-char 1)
        (throw 'return
               (list
                (edebug-storing-offsets (- (point) 2) 'function)
                (edebug-read-storing-offsets stream))))
       ((and (>= (following-char) ?0) (<= (following-char) ?9))
        (while (and (>= (following-char) ?0) (<= (following-char) ?9))
          (forward-char 1))
        (let ((n (string-to-number (buffer-substring start (point)))))
          (when read-circle
            (cond
             ((eq ?= (following-char))
              ;; Make a placeholder for #n# to use temporarily.
              (let* ((placeholder (cons nil nil))
                     (elem (cons n placeholder)))
                (push elem edebug-read-objects)
                ;; Read the object and then replace the placeholder
                ;; with the object itself, wherever it occurs.
                (forward-char 1)
                (let ((obj (edebug-read-storing-offsets stream)))
                  (lread--substitute-object-in-subtree obj placeholder t)
                  (throw 'return (setf (cdr elem) obj)))))
             ((eq ?# (following-char))
              ;; #n# returns a previously read object.
              (let ((elem (assoc n edebug-read-objects)))
                (when (consp elem)
                  (forward-char 1)
                  (throw 'return (cdr elem))))))))))
      ;; Let read handle errors, radix notation, and anything else.
      (goto-char (1- start))
      (read stream))))

(defun edebug-read-list (stream)
  (forward-char 1)			; skip \(
  (prog1
      (let ((elements))
	(while (not (memq (edebug-next-token-class) '(rparen dot)))
          (push (edebug-read-storing-offsets stream) elements))
	(setq elements (nreverse elements))
	(if (eq 'dot (edebug-next-token-class))
	    (let (dotted-form)
	      (forward-char 1)		; skip \.
	      (setq dotted-form (edebug-read-storing-offsets stream))
		    elements (nconc elements dotted-form)
	      (if (not (eq (edebug-next-token-class) 'rparen))
		  (edebug-syntax-error "Expected `)'"))
	      (setq edebug-read-dotted-list (listp dotted-form))
	      ))
	elements)
    (forward-char 1)			; skip \)
    ))

(defun edebug-read-vector (stream)
  (forward-char 1)			; skip \[
  (prog1
      (let ((elements))
	(while (not (eq 'rbracket (edebug-next-token-class)))
	  (push (edebug-read-storing-offsets stream) elements))
	(apply #'vector (nreverse elements)))
    (forward-char 1)			; skip \]
    ))

;;; Cursors for traversal of list and vector elements with offsets.

;; Edebug's instrumentation is based on parsing the sexps, which come with
;; auxiliary position information.  Instead of keeping the position
;; information together with the sexps, it is kept in a "parallel
;; tree" of offsets.
;;
;; An "edebug cursor" is a pair of a *list of sexps* (called the
;; "expressions") together with a matching list of offsets.
;; When we're parsing the content of a list, the
;; `edebug-cursor-expressions' is simply the list but when parsing
;; a vector, the `edebug-cursor-expressions' is a list formed of the
;; elements of the vector.

(defvar edebug-dotted-spec nil
  "Set to t when matching after the dot in a dotted spec list.")

(defun edebug-new-cursor (expressions offsets)
  ;; Return a new cursor for EXPRESSIONS with OFFSETS.
  (if (vectorp expressions)
      (setq expressions (append expressions nil)))
  (cons expressions offsets))

(defsubst edebug-set-cursor (cursor expressions offsets)
  ;; Set the CURSOR's EXPRESSIONS and OFFSETS to the given.
  ;; Return the cursor.
  (setcar cursor expressions)
  (setcdr cursor offsets)
  cursor)

(defun edebug-copy-cursor (cursor)
  ;; Copy the cursor using the same object and offsets.
  (cons (car cursor) (cdr cursor)))

(defsubst edebug-cursor-expressions (cursor)
  (car cursor))
(defsubst edebug-cursor-offsets (cursor)
  (cdr cursor))

(defsubst edebug-empty-cursor (cursor)
  ;; Return non-nil if CURSOR is empty - meaning no more elements.
  (null (car cursor)))

(defsubst edebug-top-element (cursor)
  ;; Return the top element at the cursor.
  ;; Assumes not empty.
  (car (car cursor)))

(defun edebug-top-element-required (cursor &rest error)
  ;; Check if a dotted form is required.
  (if edebug-dotted-spec (edebug-no-match cursor "Dot expected."))
  ;; Check if there is at least one more argument.
  (if (edebug-empty-cursor cursor) (apply #'edebug-no-match cursor error))
  ;; Return that top element.
  (edebug-top-element cursor))

(defsubst edebug-top-offset (cursor)
  ;; Return the top offset pair corresponding to the top element.
  (car (cdr cursor)))

(defun edebug-move-cursor (cursor)
  ;; Advance and return the cursor to the next element and offset.
  ;; throw no-match if empty before moving.
  ;; This is a violation of the cursor encapsulation, but
  ;; there is plenty of that going on while matching.
  ;; The following test should always fail.
  (if (edebug-empty-cursor cursor)
      (edebug-no-match cursor "Not enough arguments."))
  (cl-callf cdr (car cursor))
  (cl-callf cdr (cdr cursor))
  cursor)


(defun edebug-before-offset (cursor)
  ;; Return the before offset of the cursor.
  ;; If there is nothing left in the offsets,
  ;; return one less than the offset itself,
  ;; which is the after offset for a list.
  (let ((offset (edebug-cursor-offsets cursor)))
    (if (consp offset)
	(car (car offset))
      (1- offset))))

(defun edebug-after-offset (cursor)
  ;; Return the after offset of the cursor object.
  (let ((offset (edebug-top-offset cursor)))
    (while (consp offset)
      (setq offset (cdr offset)))
    offset))

;;; The Parser

;; The top level function for parsing forms is
;; edebug-read-and-maybe-wrap-form; it calls all the rest.  It checks the
;; syntax a bit and leaves point at any error it finds, but otherwise
;; should appear to work like eval-defun.

;; The basic plan is to surround each expression with a call to
;; the edebug debugger together with indexes into a table of positions of
;; all expressions.  Thus an expression "exp" becomes:

;; (edebug-after (edebug-before 1) 2 exp)

;; When this is evaluated, first point is moved to the beginning of
;; exp at offset 1 of the current function.  The expression is
;; evaluated, which may cause more edebug calls, and then point is
;; moved to offset 2 after the end of exp.

;; The highest level expressions of the function are wrapped in a call to
;; edebug-enter, which supplies the function name and the actual
;; arguments to the function.  See functions edebug-enter, edebug-before,
;; and edebug-after for more details.

;; Dynamically bound vars, left unbound, but globally declared.
;; This is to quiet the byte compiler.

;; Window data of the highest definition being wrapped.
;; This data is shared by all embedded definitions.
(defvar edebug-top-window-data)

(defvar edebug-gate nil) ;; whether no-match forces an error.

(defvar edebug-def-name nil) ; name of definition, used by interactive-form
(defvar edebug-old-def-name nil) ; previous name of containing definition.

(defvar edebug-error-point nil)
(defvar edebug-best-error nil)

;; Functions which may be used to extend Edebug's functionality.  See
;; Testcover for an example.
(defvar edebug-after-instrumentation-function #'identity
  "Function to run on code after instrumentation for debugging.
The function is called with one argument, a FORM which has just
been instrumented for Edebugging, and it should return either FORM
or a replacement form to use in its place.")

(defvar edebug-new-definition-function #'edebug-new-definition
  "Function to call after Edebug wraps a new definition.
After Edebug has initialized its own data, this function is
called with one argument, the symbol associated with the
definition, which may be the actual symbol defined or one
generated by Edebug.")

(defvar edebug-behavior-alist
  '((edebug edebug-default-enter edebug-slow-before edebug-slow-after))
  "Alist describing the runtime behavior of Edebug's instrumented code.
Each definition instrumented by Edebug will have a
`edebug-behavior' property which is a key to this alist.  When
the instrumented code is running, Edebug will look here for the
implementations of `edebug-enter', `edebug-before', and
`edebug-after'.  Edebug's instrumentation may be used for a new
purpose by adding an entry to this alist, and setting
`edebug-new-definition-function' to a function which sets
`edebug-behavior' for the definition.")

(defun edebug-read-and-maybe-wrap-form ()
  ;; Read a form and wrap it with edebug calls, if the conditions are right.
  ;; Here we just catch any no-match not caught below and signal an error.

  ;; Run the setup hook.
  ;; If it gets an error, make it nil.
  (let ((temp-hook edebug-setup-hook))
    (setq edebug-setup-hook nil)
    (if (functionp temp-hook) (funcall temp-hook)
      (mapc #'funcall temp-hook)))

  (let (result
	edebug-top-window-data
	edebug-def-name;; make sure it is locally nil
	;; I don't like these here!!
	edebug-gate
	edebug-best-error
	edebug-error-point
	;; Do this once here instead of several times.
	(max-lisp-eval-depth (+ 800 max-lisp-eval-depth))
	(max-specpdl-size (+ 2000 max-specpdl-size)))
    (let ((no-match
           (catch 'no-match
             (setq result (edebug-read-and-maybe-wrap-form1))
             nil)))
      (if no-match
          (apply #'edebug-syntax-error no-match)))
    result))


(defun edebug-read-and-maybe-wrap-form1 ()
  (let (spec
	def-kind
	defining-form-p
	def-name
	;; These offset things don't belong here, but to support recursive
	;; calls to edebug-read, they need to be here.
	edebug-offsets
	edebug-offsets-stack
	edebug-current-offset ; reset to nil
        edebug-read-objects
	)
    (save-excursion
      (if (and (eq 'lparen (edebug-next-token-class))
	       (eq 'symbol (progn (forward-char 1) (edebug-next-token-class))))
	  ;; Find out if this is a defining form from first symbol
	  (setq def-kind (read (current-buffer))
		spec (and (symbolp def-kind) (edebug-get-spec def-kind))
		defining-form-p (and (listp spec)
				     (eq '&define (car spec)))
		;; This is incorrect in general!! But OK most of the time.
		def-name (if (and defining-form-p
				  (eq 'name (car (cdr spec)))
				  (eq 'symbol (edebug-next-token-class)))
			     (read (current-buffer))))))
;;;(message "all defs: %s   all forms: %s"  edebug-all-defs edebug-all-forms)
    (let ((result
           (cond
            ;; IIUC, `&define' is treated specially here so as to avoid
            ;; entering Edebug during the actual function's definition:
            ;; we only want to enter Edebug later when the thing is called.
            (defining-form-p
              (if (or edebug-all-defs edebug-all-forms)
                  ;; If it is a defining form and we are edebugging defs,
                  ;; then let edebug-list-form start it.
                  (let ((cursor (edebug-new-cursor
                                 (list (edebug-read-storing-offsets (current-buffer)))
                                 (list edebug-offsets))))
                    (car
                     (edebug-make-form-wrapper
                      cursor
                      (edebug-before-offset cursor)
                      (1- (edebug-after-offset cursor))
                      (list (cons (symbol-name def-kind) (cdr spec))))))

                ;; Not edebugging this form, so reset the symbol's edebug
                ;; property to be just a marker at the definition's source code.
                ;; This only works for defs with simple names.

                ;; Preserve the `edebug' property in case there's
                ;; debugging still under way.
                (let ((ghost (get def-name 'edebug)))
                  (if (consp ghost)
                      (put def-name 'ghost-edebug ghost)))
                (put def-name 'edebug (point-marker))
                ;; Also nil out dependent defs.
                '(mapcar (function
                          (lambda (def)
                            (put def-name 'edebug nil)))
                         (get def-name 'edebug-dependents))
                (edebug-read-sexp)))

            ;; If all forms are being edebugged, explicitly wrap it.
            (edebug-all-forms
             (let ((cursor (edebug-new-cursor
                            (list (edebug-read-storing-offsets (current-buffer)))
                            (list edebug-offsets))))
               (edebug-make-form-wrapper
                cursor
                (edebug-before-offset cursor)
                (edebug-after-offset cursor)
                nil)))

            ;; Not a defining form, and not edebugging.
            (t (edebug-read-sexp)))))
      (funcall edebug-after-instrumentation-function result))))

(defvar edebug-def-args) ; args of defining form.
(defvar edebug-inside-func)  ;; whether code is inside function context.
;; Currently def-form sets this to nil; def-body sets it to t.


(defvar edebug-lexical-macro-ctx nil
  "Alist mapping lexically scoped macro names to their debug spec.")

(defun edebug-make-enter-wrapper (forms)
  ;; Generate the enter wrapper for some forms of a definition.
  ;; This is not to be used for the body of other forms, e.g. `while',
  ;; since it wraps the list of forms with a call to `edebug-enter'.
  ;; Uses the dynamically bound vars edebug-def-name and edebug-def-args.
  ;; Do this after parsing since that may find a name.
  (when (string-match-p (rx bos "edebug-anon" (+ digit) eos)
                        (symbol-name edebug-old-def-name))
    ;; FIXME: Due to Bug#42701, we reset an anonymous name so that
    ;; backtracking doesn't generate duplicate definitions.  It would
    ;; be better to not define wrappers in the case of a non-matching
    ;; specification branch to begin with.
    (setq edebug-old-def-name nil))
  (setq edebug-def-name
	(or edebug-def-name edebug-old-def-name (gensym "edebug-anon")))
  `(edebug-enter
    (quote ,edebug-def-name)
    ,(if edebug-inside-func
	 `(list
	   ;; Doesn't work with more than one def-body!!
	   ;; But the list will just be reversed.
	   ,@(nreverse edebug-def-args))
       'nil)
    (function (lambda () ,@forms))
    ))


(defvar edebug-form-begin-marker) ; the mark for def being instrumented

(defvar edebug-offset-index) ; the next available offset index.
(defvar edebug-offset-list) ; the list of offset positions.

(defun edebug-inc-offset (offset)
  ;; Modifies edebug-offset-index and edebug-offset-list
  ;; accesses edebug-func-marc and buffer point.
  (prog1
      edebug-offset-index
    (setq edebug-offset-list (cons (- offset edebug-form-begin-marker)
				   edebug-offset-list)
	  edebug-offset-index (1+ edebug-offset-index))))


(defun edebug-make-before-and-after-form (before-index form after-index)
  ;; Return the edebug form for the current function at offset BEFORE-INDEX
  ;; given FORM.  Looks like:
  ;; (edebug-after (edebug-before BEFORE-INDEX) AFTER-INDEX FORM)
  ;; Also increment the offset index for subsequent use.
  `(edebug-after (edebug-before ,before-index) ,after-index ,form))

(defun edebug-make-after-form (form after-index)
  ;; Like edebug-make-before-and-after-form, but only after.
  `(edebug-after 0 ,after-index ,form))


(defun edebug-unwrap (sexp)
  "Return the unwrapped SEXP or return it as is if it is not wrapped.
The SEXP might be the result of wrapping a body, which is a list of
expressions; a `progn' form will be returned enclosing these forms.
Does not unwrap inside vectors, records, structures, or hash tables."
  (pcase sexp
    (`(edebug-after ,_before-form ,_after-index ,form)
     form)
    (`(lambda ,args (edebug-enter ',_sym ,_arglist
				   (function (lambda nil . ,body))))
     `(lambda ,args ,@body))
    (`(closure ,env ,args (edebug-enter ',_sym ,_arglist
					 (function (lambda nil . ,body))))
     `(closure ,env ,args ,@body))
    (`(edebug-enter ',_sym ,_args (function (lambda nil . ,body)))
     (macroexp-progn body))
    (_ sexp)))

(defun edebug-unwrap* (sexp)
  "Return the SEXP recursively unwrapped."
  (let ((ht (make-hash-table :test 'eq)))
    (edebug--unwrap1 sexp ht)))

(defun edebug--unwrap1 (sexp hash-table)
  "Unwrap SEXP using HASH-TABLE of things already unwrapped.
HASH-TABLE contains the results of unwrapping cons cells within
SEXP, which are reused to avoid infinite loops when SEXP is or
contains a circular object."
  (let ((new-sexp (edebug-unwrap sexp)))
    (while (not (eq sexp new-sexp))
      (setq sexp new-sexp
	    new-sexp (edebug-unwrap sexp)))
    (if (consp new-sexp)
	(let ((result (gethash new-sexp hash-table nil)))
	  (unless result
	    (let ((remainder new-sexp)
		  current)
	      (setq result (cons nil nil)
		    current result)
	      (while
		  (progn
		    (puthash remainder current hash-table)
		    (setf (car current)
			  (edebug--unwrap1 (car remainder) hash-table))
		    (setq remainder (cdr remainder))
		    (cond
		     ((atom remainder)
		      (setf (cdr current)
			    (edebug--unwrap1 remainder hash-table))
		      nil)
		     ((gethash remainder hash-table nil)
		      (setf (cdr current) (gethash remainder hash-table nil))
		      nil)
		     (t (setq current
			      (setf (cdr current) (cons nil nil)))))))))
	  result)
      new-sexp)))


(defun edebug-defining-form (cursor form-begin form-end speclist)
  ;; Process the defining form, starting outside the form.
  ;; The speclist is a generated list spec that looks like:
  ;;   (("def-symbol" defining-form-spec-sans-&define))
  ;; Skip the first offset.
  (edebug-set-cursor cursor (edebug-cursor-expressions cursor)
		     (cdr (edebug-cursor-offsets cursor)))
  (edebug-make-form-wrapper
   cursor
   form-begin (1- form-end)
   speclist))

(defun edebug-make-form-wrapper (cursor form-begin form-end
					&optional speclist)
  ;; Wrap a form, usually a defining form, but any evaluated one.
  ;; If speclist is non-nil, this is being called by edebug-defining-form.
  ;; Otherwise it is being called from edebug-read-and-maybe-wrap-form1.
  ;; This is a hack, but I haven't figured out a simpler way yet.
  (let* ((form-data-entry (edebug-get-form-data-entry form-begin form-end))
	 ;; Set this marker before parsing.
	 (edebug-form-begin-marker
	  (if form-data-entry
	      (edebug--form-data-begin form-data-entry)
	    ;; Buffer must be current-buffer for this to work:
	    (set-marker (make-marker) form-begin))))

    (let (edebug-offset-list
	  (edebug-offset-index 0)
	  result
	  ;; For definitions.
	  ;; (edebug-containing-def-name edebug-def-name)
	  ;; Get name from form-data, if any.
	  (edebug-old-def-name (edebug--form-data-name form-data-entry))
	  edebug-def-name
	  edebug-def-args
	  edebug-inside-func;; whether wrapped code executes inside a function.
	  )

      (setq result
	    (if speclist
		(edebug-match cursor speclist)

	      ;; else wrap as an enter-form.
	      (edebug-make-enter-wrapper (list (edebug-form cursor)))))

      ;; Set the name here if it was not set by edebug-make-enter-wrapper.
      (setq edebug-def-name
	    (or edebug-def-name edebug-old-def-name (cl-gensym "edebug-anon")))

      ;; Add this def as a dependent of containing def.  Buggy.
      '(if (and edebug-containing-def-name
		(not (get edebug-containing-def-name 'edebug-dependents)))
	   (put edebug-containing-def-name 'edebug-dependents
		(cons edebug-def-name
		      (get edebug-containing-def-name
			   'edebug-dependents))))

      ;; Create a form-data-entry or modify existing entry's markers.
      ;; In the latter case, pointers to the entry remain eq.
      (if (not form-data-entry)
	  (setq form-data-entry
		(edebug--make-form-data-entry
		 edebug-def-name
		 edebug-form-begin-marker
		 ;; Buffer must be current-buffer.
		 (set-marker (make-marker) form-end)
		 ))
	(edebug-set-form-data-entry
	 form-data-entry edebug-def-name ;; in case name is changed
	 form-begin form-end))

      ;;    (message "defining: %s" edebug-def-name) (sit-for 2)
      (edebug-make-top-form-data-entry form-data-entry)
      ;;(debug edebug-def-name)

      ;; Destructively reverse edebug-offset-list and make vector from it.
      (setq edebug-offset-list (vconcat (nreverse edebug-offset-list)))

      ;; Side effects on the property list of edebug-def-name.
      (edebug-clear-frequency-count edebug-def-name)
      (edebug-clear-coverage edebug-def-name)

      ;; Set up the initial window data.
      (if (not edebug-top-window-data) ;; if not already set, do it now.
	  (let ((window ;; Find the best window for this buffer.
		 (or (get-buffer-window (current-buffer))
		     (selected-window))))
	    (setq edebug-top-window-data
		  (cons window (window-start window)))))

      ;; Store the edebug data in symbol's property list.
      ;; We actually want to remove this property entirely, but can't.
      (put edebug-def-name 'ghost-edebug nil)
      (put edebug-def-name 'edebug
	   ;; A struct or vector would be better here!!
	   (list edebug-form-begin-marker
		 (edebug--restore-breakpoints edebug-old-def-name)
		 edebug-offset-list
		 edebug-top-window-data))

      (funcall edebug-new-definition-function edebug-def-name)
      result
      )))

(defun edebug--restore-breakpoints (name)
  (let ((data (edebug-get-edebug-or-ghost name)))
    (when (consp data)
      (let ((offsets (nth 2 data))
            (breakpoints (nth 1 data))
            (start (nth 0 data))
            index)
        ;; Breakpoints refer to offsets from the start of the function.
        ;; The start position is a marker, so it'll move around in a
        ;; similar fashion as the breakpoint markers.  If we find a
        ;; breakpoint marker that refers to an offset (which is a place
        ;; where breakpoints can be made), then we restore it.
        (cl-loop for breakpoint in breakpoints
                 for marker = (nth 3 breakpoint)
                 when (and (marker-position marker)
                           (setq index (seq-position
                                        offsets
                                        (- (marker-position marker) start))))
                 collect (cons index (cdr breakpoint)))))))

(defun edebug-new-definition (def-name)
  "Set up DEF-NAME to use Edebug's instrumentation functions."
  (put def-name 'edebug-behavior 'edebug)
  (message "Edebug: %s" def-name))


(defun edebug-clear-frequency-count (name)
  ;; Create initial frequency count vector.
  ;; For each stop point, the counter is incremented each time it is visited.
  (put name 'edebug-freq-count
       (make-vector (length edebug-offset-list) 0)))


(defun edebug-clear-coverage (name)
  ;; Create initial coverage vector.
  ;; Only need one per expression, but it is simpler to use stop points.
  (put name 'edebug-coverage
       (make-vector (length edebug-offset-list) 'edebug-unknown)))


(defun edebug-form (cursor)
  ;; Return the instrumented form for the following form.
  ;; Add the point offsets to the edebug-offset-list for the form.
  (let* ((form (edebug-top-element-required cursor "Expected form"))
	 (offset (edebug-top-offset cursor)))
    (prog1
	(cond
	 ((consp form)
	  ;; The first offset for a list form is for the list form itself.
	  (if (eq 'quote (car form))
	      ;; This makes sure we don't instrument 'foo
              ;; which would cause the debugger to single-step
	      ;; the trivial evaluation of a constant.
	      form
	    (let* ((head (car form))
		   (spec (and (symbolp head) (edebug-get-spec head)))
		   (new-cursor (edebug-new-cursor form offset)))
	      ;; Find out if this is a defining form from first symbol.
	      ;; An indirect spec would not work here, yet.
	      (if (and (consp spec) (eq '&define (car spec)))
		  (edebug-defining-form
		   new-cursor
		   (car offset);; before the form
		   (edebug-after-offset cursor)
		   (cons (symbol-name head) (cdr spec)))
		;; Wrap a regular form.
		(edebug-make-before-and-after-form
		 (edebug-inc-offset (car offset))
		 (edebug-list-form new-cursor)
		 ;; After processing the list form, the new-cursor is left
		 ;; with the offset after the form.
		 (edebug-inc-offset (edebug-cursor-offsets new-cursor))))
	      )))

	 ((symbolp form)
	  (cond
	   ;; Check for constant symbols that don't get wrapped.
	   ((or (memq form '(t nil))
		(keywordp form))
	    form)

	   (t ;; just a variable
	    (edebug-make-after-form form (edebug-inc-offset (cdr offset))))))

	 ;; Anything else is self-evaluating.
	 (t form))
    (edebug-move-cursor cursor))))


(defsubst edebug-forms (cursor)  (edebug-match cursor '(&rest form)))
(defsubst edebug-sexps (cursor)  (edebug-match cursor '(&rest sexp)))

(defsubst edebug-list-form-args (head cursor)
  ;; Process the arguments of a list form given that head of form is a symbol.
  ;; Helper for edebug-list-form
  (let* ((lex-spec (assq head edebug-lexical-macro-ctx))
         (spec (if lex-spec (cdr lex-spec)
                 (edebug-get-spec head))))
    (cond
     (spec
      (cond
       ((consp spec)
	;; It is a speclist.
	(let (edebug-best-error
	      edebug-error-point);; This may not be needed.
	  (edebug-match-sublist cursor spec)))
       ((eq t spec) (edebug-forms cursor))
       ((eq 0 spec) (edebug-sexps cursor))
       ((symbolp spec) (funcall spec cursor));; Not used by edebug,
					; but leave it in for compatibility.
       ))
     ;; No edebug-form-spec provided.
     ((or lex-spec (macrop head))
      (if edebug-eval-macro-args
	  (edebug-forms cursor)
	(edebug-sexps cursor)))
     (t ;; Otherwise it is a function call.
      (edebug-forms cursor)))))


(defun edebug-list-form (cursor)
  ;; Return an instrumented form built from the list form.
  ;; The after offset will be left in the cursor after processing the form.
  (let ((head (edebug-top-element-required cursor "Expected elements"))
	;; Prevent backtracking whenever instrumenting.
	(edebug-gate t))
    ;; Skip the first offset.
    (edebug-set-cursor cursor (edebug-cursor-expressions cursor)
		       (cdr (edebug-cursor-offsets cursor)))
    (cond
     ((symbolp head)
      (cond
       ((null head) nil) ; () is valid.
       (t
	(cons head (edebug-list-form-args
		    head (edebug-move-cursor cursor))))))

     ((consp head)
      (if (eq (car head) '\,)
	  ;; The head of a form should normally be a symbol or a lambda
	  ;; expression but it can also be an unquote form to be filled
	  ;; before evaluation.  We evaluate the arguments anyway, on the
	  ;; assumption that the unquote form will place a proper function
	  ;; name (rather than a macro name).
	  (edebug-match cursor '(("," def-form) body))
	;; Process anonymous function and args.
	;; This assumes no anonymous macros.
	(edebug-match-specs cursor '(lambda-expr body) 'edebug-match-specs)))

     (t (edebug-syntax-error
	 "Head of list form must be a symbol or lambda expression")))
      ))

;;; Matching of specs.

(defvar edebug-matching-depth 0)  ;; initial value


;;; Failure to match

;; This throws to no-match, if there are higher alternatives.
;; Otherwise it signals an error.  The place of the error is found
;; with the two before- and after-offset functions.

(defun edebug-no-match (cursor &rest args)
  ;; Throw a no-match, or signal an error immediately if gate is active.
  ;; Remember this point in case we need to report this error.
  (setq edebug-error-point (or edebug-error-point
			       (edebug-before-offset cursor))
	edebug-best-error (or edebug-best-error args))
  (if edebug-gate
      (progn
	(if edebug-error-point
	    (goto-char edebug-error-point))
	(apply #'edebug-syntax-error args))
    (throw 'no-match args)))


(defun edebug-match (cursor specs)
  ;; Top level spec matching function.
  ;; Used also at each lower level of specs.
  (let (edebug-best-error
	edebug-error-point
	(edebug-gate edebug-gate)  ;; locally bound to limit effect
	)
    (edebug-match-specs cursor specs #'edebug-match-specs)))


(defun edebug-match-one-spec (cursor spec)
  ;; Match one spec, which is not a keyword &-spec.
  (cond
   ((symbolp spec) (edebug-match-symbol cursor spec))
   ((vectorp spec) (edebug-match cursor (append spec nil)))
   ((stringp spec) (edebug-match-string cursor spec))
   ((listp spec) (edebug-match-list cursor spec))
   ))


(defun edebug-match-specs (cursor specs remainder-handler)
  ;; Append results of matching the list of specs.
  ;; The first spec is handled and the remainder-handler handles the rest.
  (let ((edebug-matching-depth
	 (if (> edebug-matching-depth edebug-max-depth)
	     (error "Too deep - perhaps infinite loop in spec?")
	   (1+ edebug-matching-depth))))
    (cond
     ((null specs) nil)

     ;; Is the spec dotted?
     ((atom specs)
      (let ((edebug-dotted-spec t));; Containing spec list was dotted.
	(edebug-match-specs cursor (list specs) remainder-handler)))

     ;; The reason for processing here &optional, &rest, and vectors
     ;; which might contain them even when the form is dotted is to
     ;; allow them to match nothing, so we can advance to the dotted
     ;; part of the spec.
     ((or (listp (edebug-cursor-expressions cursor))
          (vectorp (car specs))
          (memq (car specs) '(&optional &rest))) ; Process normally.
      ;; (message "%scursor=%s specs=%s"
      ;;          (make-string edebug-matching-depth ?|) cursor (car specs))
      (let* ((spec (car specs))
	     (rest)
	     (first-char (and (symbolp spec) (aref (symbol-name spec) 0)))
	     (match (cond
		     ((eq ?& first-char);; "&" symbols take all following specs.
		      (edebug--match-&-spec-op spec cursor (cdr specs)))
		     ((eq ?: first-char);; ":" symbols take one following spec.
		      (setq rest (cdr (cdr specs)))
		      (edebug--handle-:-spec-op spec cursor (car (cdr specs))))
		     (t;; Any other normal spec.
		      (setq rest (cdr specs))
		      (edebug-match-one-spec cursor spec)))))
        ;; The first match result may not be a list, which can happen
        ;; when matching the tail of a dotted list.  In that case
        ;; there is no remainder.
	(if (listp match)
	    (nconc match
		   (funcall remainder-handler cursor rest remainder-handler))
	  match)))

     ;; Must be a dotted form, with no remaining &rest or &optional specs to
     ;; match.
     (t
      (if (not edebug-dotted-spec)
	  (edebug-no-match cursor "Dotted spec required."))
      ;; Cancel dotted spec and dotted form.
      (let ((edebug-dotted-spec)
            (this-form (edebug-cursor-expressions cursor))
            (this-offset (edebug-cursor-offsets cursor)))
	;; Wrap the form in a list, by changing the cursor.
	(edebug-set-cursor cursor (list this-form) this-offset)
	;; Process normally, then unwrap the result.
	(car (edebug-match-specs cursor specs remainder-handler)))))))

;; Define specs for all the symbol specs with functions used to process them.
;; Perhaps we shouldn't be doing this with edebug-form-specs since the
;; user may want to define macros or functions with the same names.
;; We could use an internal obarray for these primitive specs.

(dolist (pair '((form . edebug-match-form)
		(sexp . edebug-match-sexp)
		(body . edebug-match-body)
		(arg . edebug-match-arg)
		(def-body . edebug-match-def-body)
		(def-form . edebug-match-def-form)
		;; Less frequently used:
		;; (function . edebug-match-function)
		(place . edebug-match-place)
		(gate . edebug-match-gate)
		;;   (nil . edebug-match-nil)  not this one - special case it.
		))
  (put (car pair) 'edebug-elem-spec (cdr pair)))

(defun edebug-match-symbol (cursor symbol)
  ;; Match a symbol spec.
  (let* ((spec (edebug--get-elem-spec symbol)))
    (cond
     (spec
      (if (consp spec)
	  ;; It is an indirect spec.
	  (edebug-match cursor spec)
	;; Otherwise it should be the symbol name of a function.
	;; There could be a bug here - maybe need to do edebug-match bindings.
	(funcall spec cursor)))

     ((null symbol)  ;; special case this.
      (edebug-match-nil cursor))

     ((fboundp symbol)			; is it a predicate?
      (let ((sexp (edebug-top-element-required cursor "Expected" symbol)))
	;; Special case for edebug-`.
	(if (and (listp sexp) (eq (car sexp) '\,))
	    (edebug-match cursor '(("," def-form)))
	  (if (not (funcall symbol sexp))
	      (edebug-no-match cursor symbol "failed"))
	  (edebug-move-cursor cursor)
	  (list sexp))))
     (t (error "%s is not a form-spec or function" symbol))
     )))


(defun edebug-match-sexp (cursor)
  (list (prog1 (edebug-top-element-required cursor "Expected sexp")
	  (edebug-move-cursor cursor))))

(defun edebug-match-form (cursor)
  (list (edebug-form cursor)))

(defalias 'edebug-match-place 'edebug-match-form)
  ;; Currently identical to edebug-match-form.
  ;; This is for common lisp setf-style place arguments.

(defsubst edebug-match-body (cursor) (edebug-forms cursor))

(cl-defmethod edebug--match-&-spec-op ((_ (eql &optional)) cursor specs)
  ;; Keep matching until one spec fails.
  (edebug-&optional-wrapper cursor specs #'edebug-&optional-wrapper))

(defun edebug-&optional-wrapper (cursor specs remainder-handler)
  (let (result
	(edebug-gate nil)
	(this-form (edebug-cursor-expressions cursor))
	(this-offset (edebug-cursor-offsets cursor)))
    (if (null (catch 'no-match
		(setq result
		      (edebug-match-specs cursor specs remainder-handler))
		;; Returning nil means no no-match was thrown.
		nil))
	result
      ;; no-match, but don't fail; just reset cursor and return nil.
      (edebug-set-cursor cursor this-form this-offset)
      nil)))


(cl-defgeneric edebug--match-&-spec-op (op cursor specs)
  "Handle &foo spec operators.
&foo spec operators operate on all the subsequent SPECS.")

(cl-defmethod edebug--match-&-spec-op ((_ (eql &rest)) cursor specs)
  ;; Repeatedly use specs until failure.
  (let (edebug-best-error
	edebug-error-point)
    ;; Reuse the &optional handler with this as the remainder handler.
    (edebug-&optional-wrapper
     cursor specs
     (lambda (c s rh)
       ;; `s' is the remaining spec to match.
       ;; When it's nil, start over matching `specs'.
       (edebug-&optional-wrapper c (or s specs) rh)))))


(cl-defmethod edebug--match-&-spec-op ((_ (eql &or)) cursor specs)
  ;; Keep matching until one spec succeeds, and return its results.
  ;; If none match, fail.
  ;; This needs to be optimized since most specs spend time here.
  (let ((original-specs specs)
	(this-form (edebug-cursor-expressions cursor))
	(this-offset (edebug-cursor-offsets cursor)))
    (catch 'matched
      (while specs
	(catch 'no-match
	  (throw 'matched
		 (let (edebug-gate ;; only while matching each spec
		       edebug-best-error
		       edebug-error-point)
		   ;; Doesn't support e.g. &or symbolp &rest form
		   (edebug-match-one-spec cursor (car specs)))))
	;; Match failed, so reset and try again.
	(setq specs (cdr specs))
	;; Reset the cursor for the next match.
	(edebug-set-cursor cursor this-form this-offset))
      ;; All failed.
      (apply #'edebug-no-match cursor "Expected one of" original-specs))
    ))

(cl-defmethod edebug--match-&-spec-op ((_ (eql &interpose)) cursor specs)
  "Compute the specs for `&interpose SPEC FUN ARGS...'.
Extracts the head of the data by matching it against SPEC,
and then matches the rest by calling (FUN HEAD PF ARGS...)
where PF is the parsing function which FUN can call exactly once,
passing it the specs that it needs to match.
Note that HEAD will always be a list, since specs are defined to match
a sequence of elements."
  (pcase-let*
      ((`(,spec ,fun . ,args) specs)
       (exps (edebug-cursor-expressions cursor))
       (instrumented-head (edebug-match-one-spec cursor spec))
       (consumed (- (length exps)
                    (length (edebug-cursor-expressions cursor))))
       (head (seq-subseq exps 0 consumed)))
    (cl-assert (eq (edebug-cursor-expressions cursor) (nthcdr consumed exps)))
    (apply fun `(,head
                 ,(lambda (newspecs)
                    ;; FIXME: What'd be the difference if we used
                    ;; `edebug-match-sublist', which is what
                    ;; `edebug-list-form-args' uses for the similar purpose
                    ;; when matching "normal" forms?
                    (append instrumented-head (edebug-match cursor newspecs)))
                 ,@args))))

(cl-defmethod edebug--match-&-spec-op ((_ (eql &not)) cursor specs)
  ;; If any specs match, then fail
  (if (null (catch 'no-match
	      (let ((edebug-gate nil))
		(save-excursion
		  (edebug--match-&-spec-op '&or cursor specs)))
	      nil))
      ;; This means something matched, so it is a no match.
      (edebug-no-match cursor "Unexpected"))
  ;; This means nothing matched, so it is OK.
  nil) ;; So, return nothing

(cl-defmethod edebug--match-&-spec-op ((_ (eql &key)) cursor specs)
  ;; Following specs must look like (<name> <spec>) ...
  ;; where <name> is the name of a keyword, and spec is its spec.
  ;; This really doesn't save much over the expanded form and takes time.
  (edebug--match-&-spec-op
   '&rest
   cursor
   (cons '&or
	 (mapcar (lambda (pair)
                   (vector (format ":%s" (car pair))
                           (car (cdr pair))))
		 specs))))

(cl-defmethod edebug--match-&-spec-op ((_ (eql &error)) cursor specs)
  ;; Signal an error, using the following string in the spec as argument.
  (let ((error-string (car specs))
        (edebug-error-point (edebug-before-offset cursor)))
    (goto-char edebug-error-point)
    (error "%s"
           (if (stringp error-string)
               error-string
             "String expected after &error in edebug-spec"))))

(defun edebug-match-gate (_cursor)
  ;; Simply set the gate to prevent backtracking at this level.
  (setq edebug-gate t)
  nil)


(defun edebug-match-list (cursor specs)
  ;; The spec is a list, but what kind of list, and what context?
  (if edebug-dotted-spec
      ;; After dotted spec but form did not contain dot,
      ;; so match list spec elements as if spliced in.
      (prog1
	  (let ((edebug-dotted-spec))
	    (edebug-match-specs cursor specs 'edebug-match-specs))
	;; If it matched, really clear the dotted-spec flag.
	(setq edebug-dotted-spec nil))
    (let ((spec (car specs))
	  (form (edebug-top-element-required cursor "Expected" specs)))
      (cond
       ((eq 'quote spec)
	(let ((spec (car (cdr specs))))
	  (cond
	   ((symbolp spec)
	    ;; Special case: spec quotes a symbol to match.
	    ;; Change in future.  Use "..." instead.
	    (if (not (eq spec form))
		(edebug-no-match cursor "Expected" spec))
	    (edebug-move-cursor cursor)
	    (setq edebug-gate t)
	    form)
	   (t
	    (error "Bad spec: %s" specs)))))

       ((eq 'vector spec)
	(if (vectorp form)
	    ;; Special case: match a vector with the specs.
	    (let ((result (edebug-match-sublist
			   (edebug-new-cursor
			    form (cdr (edebug-top-offset cursor)))
			   (cdr specs))))
	      (edebug-move-cursor cursor)
	      (list (apply #'vector result)))
	  (edebug-no-match cursor "Expected" specs)))

       ((listp form)
	(prog1
	    (list (edebug-match-sublist
		   ;; First offset is for the list form itself.
		   ;; Treat nil as empty list.
		   (edebug-new-cursor form (cdr (edebug-top-offset cursor)))
		   specs))
	  (edebug-move-cursor cursor)))

       (t (edebug-no-match cursor "Expected" specs)))
      )))


(defun edebug-match-sublist (cursor specs)
  ;; Match a sublist of specs.
  (prog1
      ;; match with edebug-match-specs so edebug-best-error is not bound.
      (edebug-match-specs cursor specs 'edebug-match-specs)
    (if (not (edebug-empty-cursor cursor))
	(if edebug-best-error
	    (apply #'edebug-no-match cursor edebug-best-error)
	  ;; A failed &rest or &optional spec may leave some args.
	  (edebug-no-match cursor "Failed matching" specs)
	  ))))


(defun edebug-match-string (cursor spec)
  (let ((sexp (edebug-top-element-required cursor "Expected" spec)))
    (if (not (eq (intern spec) sexp))
	(edebug-no-match cursor "Expected" spec)
      ;; Since it matched, failure means immediate error, unless &optional.
      (setq edebug-gate t)
      (edebug-move-cursor cursor)
      (list sexp)
      )))

(defun edebug-match-nil (cursor)
  ;; There must be nothing left to match a nil.
  (if (not (edebug-empty-cursor cursor))
      (edebug-no-match cursor "Unmatched argument(s)")
    nil))


(defun edebug-match-function (_cursor)
  (error "Use function-form instead of function in edebug spec"))

(cl-defmethod edebug--match-&-spec-op ((_ (eql &define)) cursor specs)
  ;; Match a defining form.
  ;; Normally, &define is interpreted specially other places.
  ;; This should only be called inside of a spec list to match the remainder
  ;; of the current list.  e.g. ("lambda" &define args def-body)
   (edebug-make-form-wrapper
    cursor
    (edebug-before-offset cursor)
    ;; Find the last offset in the list.
    (let ((offsets (edebug-cursor-offsets cursor)))
      (while (consp offsets) (setq offsets (cdr offsets)))
      offsets)
    specs))

(cl-defmethod edebug--match-&-spec-op ((_ (eql &name)) cursor specs)
  "Compute the name for `&name SPEC FUN` spec operator.

The full syntax of that operator is:
    &name [PRESTRING] SPEC [POSTSTRING] FUN ARGS...

Extracts the head of the data by matching it against SPEC,
and then get the new name to use by calling
  (FUN ARGS... OLDNAME [PRESTRING] HEAD [POSTSTRING])
FUN should return either a string or a symbol.
FUN can be missing in which case it defaults to concatenating
the new name to the end of the old with an \"@\" char between the two.
PRESTRING and POSTSTRING are optional strings that get prepended
or appended to the actual name."
  (pcase-let*
      ((`(,spec ,fun . ,args) specs)
       (prestrings (when (stringp spec)
                     (prog1 (list spec) (setq spec fun fun (pop args)))))
       (poststrings (when (stringp fun)
                      (prog1 (list fun) (setq fun (pop args)))))
       (exps (edebug-cursor-expressions cursor))
       (instrumented (edebug-match-one-spec cursor spec))
       (consumed (- (length exps)
                    (length (edebug-cursor-expressions cursor))))
       (newname (apply (or fun #'edebug--concat-name)
                       `(,@args ,edebug-def-name
                                ,@prestrings
                                ,@(seq-subseq exps 0 consumed)
                                ,@poststrings))))
    (cl-assert (eq (edebug-cursor-expressions cursor) (nthcdr consumed exps)))
    (setq edebug-def-name (if (stringp newname) (intern newname) newname))
    instrumented))

(defun edebug--concat-name (oldname &rest newnames)
  (let ((newname (if (null (cdr newnames))
                     (car newnames)
                   ;; Put spaces between each name, but not for the
                   ;; leading and trailing strings, if any.
                   (let (beg mid end)
                     (dolist (name newnames)
                       (if (stringp name)
                           (push name (if mid end beg))
                         (when end (setq mid (nconc end mid) end nil))
                         (push name mid)))
                     (apply #'concat `(,@(nreverse beg)
                                       ,(mapconcat (lambda (x) (format "%s" x))
                                                   (nreverse mid) " ")
                                       ,@(nreverse end)))))))
    (if (null oldname)
        (if (or (stringp newname) (symbolp newname))
            newname
          (format "%s" newname))
      (format "%s@%s" edebug-def-name newname))))

(def-edebug-elem-spec 'name '(&name symbolp))

(cl-defgeneric edebug--handle-:-spec-op (op cursor spec)
  "Handle :foo spec operators.
:foo spec operators operate on just the one subsequent SPEC element.")

(cl-defmethod edebug--handle-:-spec-op ((_ (eql :name)) _cursor spec)
  ;; Set the edebug-def-name to the spec.
  (setq edebug-def-name
	(if edebug-def-name
	    ;; Construct a new name by appending to previous name.
	    (intern (format "%s@%s" edebug-def-name spec))
	  spec))
  nil)

(cl-defmethod edebug--handle-:-spec-op ((_ (eql :unique)) _cursor spec)
  "Match a `:unique PREFIX' specifier.
SPEC is the symbol name prefix for `gensym'."
  (let ((suffix (gensym spec)))
    (setq edebug-def-name
	  (if edebug-def-name
	      ;; Construct a new name by appending to previous name.
	      (intern (format "%s@%s" edebug-def-name suffix))
	    suffix)))
  nil)

(defun edebug-match-arg (cursor)
  ;; set the def-args bound in edebug-defining-form
  (let ((edebug-arg (edebug-top-element-required cursor "Expected arg")))
    (if (or (not (symbolp edebug-arg))
	    (edebug-lambda-list-keywordp edebug-arg))
      (edebug-no-match cursor "Bad argument:" edebug-arg))
    (edebug-move-cursor cursor)
    (setq edebug-def-args (cons edebug-arg edebug-def-args))
    (list edebug-arg)))

(defun edebug-match-def-form (cursor)
  ;; Like form but the form is wrapped in edebug-enter form.
  ;; The form is assumed to be executing outside of the function context.
  ;; This is a hack for now, since a def-form might execute inside as well.
  ;; Not to be used otherwise.
  (let ((edebug-inside-func nil))
    (list (edebug-make-enter-wrapper (list (edebug-form cursor))))))

(defun edebug-match-def-body (cursor)
  ;; Like body but body is wrapped in edebug-enter form.
  ;; The body is assumed to be executing inside of the function context.
  ;; Not to be used otherwise.
  (let* ((edebug-inside-func t)
         (forms (edebug-forms cursor)))
    ;; If there's no form, there's nothing to wrap!
    ;; This happens to handle bug#20281, tho maybe a better fix would be to
    ;; improve the `defun' spec.
    (when forms
      (list (edebug-make-enter-wrapper forms)))))


;;;; Edebug Form Specs
;;; ==========================================================

;;;* Emacs special forms and some functions.

(pcase-dolist
    (`(,name ,spec)

     '((quote (sexp)) ;quote expects only one arg, tho it allows any number.

       ;; The standard defining forms.
       (defvar (symbolp &optional form stringp))
       (defconst defvar)

       ;; Contrary to macros, special forms default to assuming that all args
       ;; are normal forms, so we don't need to do anything about those
       ;; special forms:
       ;;(save-current-buffer t)
       ;;(save-excursion t)
       ;;...
       ;;(progn t)

       ;; `defun' and `defmacro' are not special forms (any more), but it's
       ;; more convenient to define their Edebug spec here.
       (defun ( &define name lambda-list lambda-doc
	        [&optional ("declare" def-declarations)]
	        [&optional ("interactive" &optional [&or stringp def-form]
                            &rest symbolp)]
	        def-body))

       (defmacro ( &define name lambda-list lambda-doc
                   [&optional ("declare" def-declarations)]
                   def-body))

       ;; function expects a symbol or a lambda or macro expression
       ;; A macro is allowed by Emacs.
       (function (&or symbolp lambda-expr))

       ;; FIXME?  The manual uses this form (maybe that's just
       ;; for illustration purposes?):
       ;; (let ((&rest &or symbolp (gate symbolp &optional form)) body))
       (let ((&rest &or (symbolp &optional form) symbolp) body))
       (let* let)

       (setq (&rest symbolp form))
       (cond (&rest (&rest form)))

       (condition-case ( symbolp form
                         &rest ([&or symbolp (&rest symbolp)] body)))

       (\` (backquote-form))

       ;; Assume immediate quote in unquotes mean backquote at next
       ;;  higher level.
       (\, (&or ("quote" edebug-\`) def-form))
       (\,@ (&define  ;; so (,@ form) is never wrapped.
	     &or ("quote" edebug-\`) def-form))
       ))
    (put name 'edebug-form-spec spec))

(defun edebug--match-declare-arg (head pf)
  (funcall pf (get (car head) 'edebug-declaration-spec)))

(def-edebug-elem-spec 'def-declarations
  '(&rest &or (&interpose symbolp edebug--match-declare-arg) sexp))

(def-edebug-elem-spec 'lambda-list
  '(([&rest arg]
     [&optional ["&optional" arg &rest arg]]
     &optional ["&rest" arg]
     )))

(def-edebug-elem-spec 'lambda-expr
  '(("lambda" &define lambda-list lambda-doc
     [&optional ("interactive" interactive)]
     def-body)))

(def-edebug-elem-spec 'arglist '(lambda-list))  ;; deprecated - use lambda-list.

(def-edebug-elem-spec 'lambda-doc
  '(&optional [&or stringp
                   (&define ":documentation" def-form)]))

(def-edebug-elem-spec 'interactive '(&optional [&or stringp def-form]
                                               &rest symbolp))

;; A function-form is for an argument that may be a function or a form.
;; This specially recognizes anonymous functions quoted with quote.
(def-edebug-elem-spec 'function-form          ;Deprecated, use `form'!
  ;; form at the end could also handle "function",
  ;; but recognize it specially to avoid wrapping function forms.
  '(&or ([&or "quote" "function"] &or symbolp lambda-expr) form))

;; Supports quotes inside backquotes,
;; but only at the top level inside unquotes.
(def-edebug-elem-spec 'backquote-form
  '(&or
    ;; Disallow instrumentation of , and ,@ inside a nested backquote, since
    ;; these are likely to be forms generated by a macro being debugged.
    ("`" nested-backquote-form)
    ([&or "," ",@"] &or ("quote" backquote-form) form)
    ;; The simple version:
    ;;   (backquote-form &rest backquote-form)
    ;; doesn't handle (a . ,b).  The straightforward fix:
    ;;   (backquote-form . [&or nil backquote-form])
    ;; uses up too much stack space.
    ;; Note that `(foo . ,@bar) is not valid, so we don't need to handle it.
    (backquote-form [&rest [&not ","] backquote-form]
		    . [&or nil backquote-form])
    ;; If you use dotted forms in backquotes, replace the previous line
    ;; with the following.  This takes quite a bit more stack space, however.
    ;; (backquote-form . [&or nil backquote-form])
    (vector &rest backquote-form)
    sexp))

(def-edebug-elem-spec 'nested-backquote-form
  '(&or
    ("`" &error "Triply nested backquotes (without commas \"between\" them) \
are too difficult to instrument")
    ;; Allow instrumentation of any , or ,@ contained within the (\, ...) or
    ;; (\,@ ...) matched on the next line.
    ([&or "," ",@"] backquote-form)
    (nested-backquote-form [&rest [&not "," ",@"] nested-backquote-form]
                           . [&or nil nested-backquote-form])
    (vector &rest nested-backquote-form)
    sexp))

;; Special version of backquote that instruments backquoted forms
;; destined to be evaluated, usually as the result of a
;; macroexpansion.  Backquoted code can only have unquotes (, and ,@)
;; in places where list forms are allowed, and predicates. If the
;; backquote is used in a macro, unquoted code that come from
;; arguments must be instrumented, if at all, with def-form not def-body.

;; We could assume that all forms (not nested in other forms)
;; in arguments of macros should be def-forms, whether or not the macros
;; are defined with edebug-` but this would be expensive.

;; ,@ might have some problems.

(defmacro edebug-\` (exp)
  (declare (debug (def-form)))
  (list '\` exp))

;;; The debugger itself

(defvar edebug-stack nil)
;; Stack of active functions evaluated via edebug.
;; Should be nil at the top level.

(defvar edebug-stack-depth -1)
;; Index of last edebug-stack item.

(defvar edebug-offset-indices nil)
;; Stack of offset indices of visited edebug sexps.
;; Should be nil at the top level.
;; Each function adds one cons.  Top is modified with setcar.


(defvar edebug-entered nil
  ;; Non-nil if edebug has already been entered at this recursive edit level.
  ;; This should stay nil at the top level.
  )

;; Should these be options?
(defconst edebug-debugger 'edebug
  ;; Name of function to use for debugging when error or quit occurs.
  ;; Set this to 'debug if you want to debug edebug.
  )


;; Dynamically bound variables, declared globally but left unbound.
(defvar edebug-function) ; the function being executed. change name!!
(defvar edebug-data) ; the edebug data for the function
(defvar edebug-def-mark) ; the mark for the definition
(defvar edebug-freq-count) ; the count of expression visits.
(defvar edebug-coverage) ; the coverage results of each expression of function.

(defvar edebug-buffer) ; which buffer the function is in.

(defvar edebug-execution-mode 'step) ; Current edebug mode set by user.
(defvar edebug-next-execution-mode nil) ; Use once instead of initial mode.

(defvar edebug-outside-debug-on-error) ; the value of debug-on-error outside
(defvar edebug-outside-debug-on-quit) ; the value of debug-on-quit outside

;;; Handling signals

(defun edebug-signal (signal-name signal-data)
  "Signal an error.  Args are SIGNAL-NAME, and associated DATA.
A signal name is a symbol with an `error-conditions' property
that is a list of condition names.
A handler for any of those names will get to handle this signal.
The symbol `error' should always be one of them.

DATA should be a list.  Its elements are printed as part of the error message.
If the signal is handled, DATA is made available to the handler.
See `condition-case'.

This is the Edebug replacement for the standard `signal'.  It should
only be active while Edebug is.  It checks `debug-on-error' to see
whether it should call the debugger.  When execution is resumed, the
error is signaled again."
  (if (and (listp debug-on-error) (memq signal-name debug-on-error))
      (edebug 'error (cons signal-name signal-data)))
  ;; If we reach here without another non-local exit, then send signal again.
  ;; i.e. the signal is not continuable, yet.
  ;; Avoid infinite recursion.
  (let ((signal-hook-function nil))
    (signal signal-name signal-data)))

;;; Entering Edebug

(defun edebug-enter (func args body)
  "Enter Edebug for a function.
FUNC should be the symbol with the Edebug information, ARGS is
the list of arguments and BODY is the code.

Look up the `edebug-behavior' for FUNC in `edebug-behavior-alist'
and run its entry function, and set up `edebug-before' and
`edebug-after'."
  (cl-letf* ((behavior (get func 'edebug-behavior))
             (functions (cdr (assoc behavior edebug-behavior-alist)))
             ((symbol-function 'edebug-before) (nth 1 functions))
             ((symbol-function 'edebug-after) (nth 2 functions)))
    (funcall (nth 0 functions) func args body)))

(defun edebug-default-enter (function args body)
  ;; Entering FUNC.  The arguments are ARGS, and the body is BODY.
  ;; Setup edebug variables and evaluate BODY.  This function is called
  ;; when a function evaluated with edebug-eval-top-level-form is entered.
  ;; Return the result of BODY.

  ;; Is this the first time we are entering edebug since
  ;; lower-level recursive-edit command?
  ;; More precisely, this tests whether Edebug is currently active.
  (let ((edebug-function function))
    (if (not edebug-entered)
        (let ((edebug-entered t)
              ;; Binding max-lisp-eval-depth here is OK,
              ;; but not inside an unwind-protect.
              ;; Doing it here also keeps it from growing too large.
              (max-lisp-eval-depth (+ 100 max-lisp-eval-depth)) ; too much??
              (max-specpdl-size (+ 200 max-specpdl-size))

              (debugger edebug-debugger) ; only while edebug is active.
              (edebug-outside-debug-on-error debug-on-error)
              (edebug-outside-debug-on-quit debug-on-quit)
              (outside-frame (selected-frame))
              ;; Binding these may not be the right thing to do.
              ;; We want to allow the global values to be changed.
              (debug-on-error (or debug-on-error edebug-on-error))
              (debug-on-quit edebug-on-quit))
          (unwind-protect
              (let ((signal-hook-function #'edebug-signal))
                (setq edebug-execution-mode (or edebug-next-execution-mode
                                                edebug-initial-mode
                                                edebug-execution-mode)
                      edebug-next-execution-mode nil)
                (edebug-default-enter function args body))
            (if (and (frame-live-p outside-frame)
                     (not (memq (framep outside-frame) '(nil t pc))))
                (x-focus-frame outside-frame))))

      (let* ((edebug-data (get function 'edebug))
             (edebug-def-mark (car edebug-data)) ; mark at def start
             (edebug-freq-count (get function 'edebug-freq-count))
             (edebug-coverage (get function 'edebug-coverage))
             (edebug-buffer (marker-buffer edebug-def-mark))

             (edebug-stack (cons function edebug-stack))
             (edebug-offset-indices (cons 0 edebug-offset-indices))
             )
        (if (get function 'edebug-on-entry)
            (progn
              (setq edebug-execution-mode 'step)
              (if (eq (get function 'edebug-on-entry) 'temp)
                  (put function 'edebug-on-entry nil))))
        (if edebug-trace
            (edebug--enter-trace function args body)
          (funcall body))
        ))))

(defun edebug-var-status (var)
  "Return a cons cell describing the status of VAR's current binding.
The purpose of this function is so you can properly undo
subsequent changes to the same binding, by passing the status
cons cell to `edebug-restore-status'.  The status cons cell
has the form (LOCUS . VALUE), where LOCUS can be a buffer
\(for a buffer-local binding), or nil (if the default binding is current)."
  (cons (variable-binding-locus var)
	(symbol-value var)))

(defun edebug-restore-status (var status)
  "Reset VAR based on STATUS.
STATUS should be a list returned by `edebug-var-status'."
  (let ((locus (car status))
	(value (cdr status)))
    (cond ((bufferp locus)
	   (if (buffer-live-p locus)
	       (with-current-buffer locus
		 (set var value))))
	  ((framep locus)
	   (modify-frame-parameters locus (list (cons var value))))
	  (t
	   (set var value)))))

(defun edebug--enter-trace (function args body)
  (let ((edebug-stack-depth (1+ edebug-stack-depth))
	edebug-result)
    (edebug-print-trace-before
     (format "%s args: %s" function args))
    (prog1 (setq edebug-result (funcall body))
      (edebug-print-trace-after
       (format "%s result: %s" function edebug-result)))))

(defmacro edebug-tracing (msg &rest body)
  "Print MSG in *edebug-trace* before and after evaluating BODY.
The result of BODY is also printed."
  (declare (debug (form body)))
  `(let ((edebug-stack-depth (1+ edebug-stack-depth))
	 edebug-result)
     (edebug-print-trace-before ,msg)
     (prog1 (setq edebug-result (progn ,@body))
       (edebug-print-trace-after
	(format "%s result: %s" ,msg edebug-result)))))

(defun edebug-print-trace-before (msg)
  "Function called to print trace info before expression evaluation.
MSG is printed after `::::{ '."
  (edebug-trace-display
   edebug-trace-buffer "%s{ %s" (make-string edebug-stack-depth ?\:) msg))

(defun edebug-print-trace-after (msg)
  "Function called to print trace info after expression evaluation.
MSG is printed after `::::} '."
  (edebug-trace-display
   edebug-trace-buffer "%s} %s" (make-string edebug-stack-depth ?\:) msg))



(defun edebug-slow-before (before-index)
  (unless edebug-active
    ;; Debug current function given BEFORE position.
    ;; Called from functions compiled with edebug-eval-top-level-form.
    ;; Return the before index.
    (setcar edebug-offset-indices before-index)

    ;; Increment frequency count
    (aset edebug-freq-count before-index
	  (1+ (aref edebug-freq-count before-index)))

    (if (or (not (memq edebug-execution-mode '(Go-nonstop next)))
	    (input-pending-p))
	(edebug-debugger before-index 'before nil)))
  before-index)

(defun edebug-fast-before (_before-index)
  ;; Do nothing.
  )

(defun edebug-slow-after (_before-index after-index value)
  (if edebug-active
      value
    ;; Debug current function given AFTER position and VALUE.
    ;; Called from functions compiled with edebug-eval-top-level-form.
    ;; Return VALUE.
    (setcar edebug-offset-indices after-index)

    ;; Increment frequency count
    (aset edebug-freq-count after-index
	  (1+ (aref edebug-freq-count after-index)))
    (if edebug-test-coverage (edebug--update-coverage after-index value))

    (if (and (eq edebug-execution-mode 'Go-nonstop)
	     (not (input-pending-p)))
	;; Just return result.
	value
      (edebug-debugger after-index 'after value)
      )))
(defun edebug-fast-after (_before-index _after-index value)
  ;; Do nothing but return the value.
  value)

(defun edebug-run-slow ()
  "Set up Edebug's normal behavior."
  (setf (cdr (assq 'edebug edebug-behavior-alist))
        '(edebug-default-enter edebug-slow-before edebug-slow-after)))

;; This is not used, yet.
(defun edebug-run-fast ()
  "Disable Edebug without de-instrumenting code."
  (setf (cdr (assq 'edebug edebug-behavior-alist))
        '(edebug-default-enter edebug-fast-before edebug-fast-after)))

(defalias 'edebug-before nil
  "Function called by Edebug before a form is evaluated.
See `edebug-behavior-alist' for implementations.")
(defalias 'edebug-after nil
  "Function called by Edebug after a form is evaluated.
See `edebug-behavior-alist' for implementations.")

(defun edebug--update-coverage (after-index value)
  (let ((old-result (aref edebug-coverage after-index)))
    (cond
     ((eq 'edebug-ok-coverage old-result))
     ((eq 'edebug-unknown old-result)
      (aset edebug-coverage after-index value))
     ;; Test if a different result.
     ((not (eq value old-result))
      (aset edebug-coverage after-index 'edebug-ok-coverage)))))


;; Dynamically declared unbound variables.
(defvar edebug-breakpoints)
(defvar edebug-break-data) ; break data for current function.
(defvar edebug-break) ; whether a break occurred.
(defvar edebug-global-break) ; whether a global break occurred.
(defvar edebug-break-condition) ; whether the breakpoint is conditional.

(defvar edebug-break-result nil)
(defvar edebug-global-break-result nil)


(defun edebug-debugger (offset-index arg-mode value)
  (if inhibit-redisplay
      ;; Don't really try to enter edebug within an eval from redisplay.
      value
    ;; Check breakpoints and pending input.
    ;; If edebug display should be updated, call edebug--display.
    ;; Return value.
    (let* ( ;; This needs to be here since breakpoints may be changed.
	   (edebug-breakpoints (car (cdr edebug-data)))	; list of breakpoints
	   (edebug-break-data (assq offset-index edebug-breakpoints))
	   (edebug-break-condition (car (cdr edebug-break-data)))
           (breakpoint-disabled (nth 4 edebug-break-data))
	   (edebug-global-break
	    (if edebug-global-break-condition
		(condition-case nil
		    (setq edebug-global-break-result
			  (edebug-eval edebug-global-break-condition))
		  (error nil))))
	   (edebug-break))

      ;;(edebug-trace "exp: %s" value)
      ;; Test whether we should break.
      (setq edebug-break
	    (or edebug-global-break
		(and edebug-break-data
                     (not breakpoint-disabled)
		     (or (not edebug-break-condition)
			 (setq edebug-break-result
			       (edebug-eval edebug-break-condition))))))
      (if (and edebug-break
	       (nth 2 edebug-break-data)) ; is it temporary?
	  ;; Delete the breakpoint.
	  (setcdr edebug-data
		  (cons (delq edebug-break-data edebug-breakpoints)
			(cdr (cdr edebug-data)))))

      ;; Display if mode is not go, continue, or Continue-fast
      ;; or break, or input is pending,
      (if (or (not (memq edebug-execution-mode '(go continue Continue-fast)))
	      edebug-break
	      (input-pending-p))
	  (edebug--display value offset-index arg-mode)) ; <---------- display

      value)))


;; window-start now stored with each function.
;;(defvar-local edebug-window-start nil)
;; Remember where each buffers' window starts between edebug calls.
;; This is to avoid spurious recentering.
;; Does this still need to be buffer-local??
;;(setq-default edebug-window-start nil)


;; Dynamically declared unbound vars
(defvar edebug-point) ; the point in edebug buffer
(defvar edebug-outside-buffer) ; the current-buffer outside of edebug
(defvar edebug-outside-point) ; the point outside of edebug
(defvar edebug-outside-mark) ; the mark outside of edebug
(defvar edebug-window-data)  ; window and window-start for current function
(defvar edebug-outside-windows) ; outside window configuration
(defvar edebug-eval-buffer) ; for the evaluation list.
(defvar edebug-outside-d-c-i-n-s-w) ; outside default cursor-in-non-selected-windows

(defvar edebug-eval-list nil) ;; List of expressions to evaluate.

(defvar edebug-previous-result nil) ;; Last result returned.

(defun edebug--display (value offset-index arg-mode)
  ;; edebug--display-1 is too big, we should split it.  This function
  ;; here was just introduced to avoid making edebug--display-1
  ;; yet a bit deeper.
  (save-excursion (edebug--display-1 value offset-index arg-mode)))

(defun edebug--display-1 (value offset-index arg-mode)
  (unless (marker-position edebug-def-mark)
    ;; The buffer holding the source has been killed.
    ;; Let's at least show a backtrace so the user can figure out
    ;; which function we're talking about.
    (debug))
  ;; Setup windows for edebug, determine mode, maybe enter recursive-edit.
  ;; Uses local variables of edebug-enter, edebug-before, edebug-after
  ;; and edebug-debugger.
  (let ((edebug-active t)		; For minor mode alist.
	(edebug-with-timeout-suspend (with-timeout-suspend))
	edebug-stop			; Should we enter recursive-edit?
	(edebug-point (+ edebug-def-mark
			 (aref (nth 2 edebug-data) offset-index)))
	edebug-buffer-outside-point     ; current point in edebug-buffer
	;; window displaying edebug-buffer
	(edebug-window-data (nth 3 edebug-data))
	(edebug-outside-window (selected-window))
	(edebug-outside-buffer (current-buffer))
	(edebug-outside-point (point))
        (edebug-outside-mark (mark t))
	edebug-outside-windows		; Window or screen configuration.
	edebug-buffer-points

	edebug-eval-buffer             ; Declared here so we can kill it below.
	(eval-result-list (and edebug-eval-list
                               (edebug-eval-result-list)))
	edebug-trace-window
	edebug-trace-window-start

	(edebug-outside-d-c-i-n-s-w
         (default-value 'cursor-in-non-selected-windows)))
    (unwind-protect
	(let ((cursor-in-echo-area nil)
	      (unread-command-events nil)
	      ;; any others??
	      )
          (setq-default cursor-in-non-selected-windows t)
	  (if (not (buffer-name edebug-buffer))
              (user-error "Buffer defining %s not found" edebug-function))

	  (if (eq 'after arg-mode)
	      ;; Compute result string now before windows are modified.
	      (edebug-compute-previous-result value))

	  (if edebug-save-windows
	      ;; Save windows now before we modify them.
	      (setq edebug-outside-windows
		    (edebug-current-windows edebug-save-windows)))

	  (if edebug-save-displayed-buffer-points
	      (setq edebug-buffer-points (edebug-get-displayed-buffer-points)))

	  ;; First move the edebug buffer point to edebug-point
	  ;; so that window start doesn't get changed when we display it.
	  ;; I don't know if this is going to help.
	  ;;(set-buffer edebug-buffer)
	  ;;(goto-char edebug-point)

	  ;; If edebug-buffer is not currently displayed,
	  ;; first find a window for it.
	  (edebug-pop-to-buffer edebug-buffer (car edebug-window-data))
	  (setcar edebug-window-data (selected-window))

	  ;; Now display eval list, if any.
	  ;; This is done after the pop to edebug-buffer
	  ;; so that buffer-window correspondence is correct after quitting.
	  (edebug-eval-display eval-result-list)
	  ;; The evaluation list better not have deleted edebug-window-data.
	  (select-window (car edebug-window-data))
          (if (not (memq (framep (selected-frame)) '(nil t pc)))
              (x-focus-frame (selected-frame)))
	  (set-buffer edebug-buffer)

	  (setq edebug-buffer-outside-point (point))
	  (goto-char edebug-point)

	  (if (eq 'before arg-mode)
	      ;; Check whether positions are up-to-date.
	      ;; This assumes point is never before symbol.
	      (if (not (memq (following-char) '(?\( ?\# ?\` )))
                  (user-error "Source has changed - reevaluate definition of %s"
                              edebug-function)
                ))

          ;; Make sure we bind those in the right buffer (bug#16410).
          (let ((overlay-arrow-position overlay-arrow-position)
                (overlay-arrow-string overlay-arrow-string))
            ;; Now display arrow based on mode.
            (edebug-overlay-arrow)

            (cond
             ((eq 'error arg-mode)
              ;; Display error message
              (setq edebug-execution-mode 'step)
              (edebug-overlay-arrow)
              (beep)
              (if (eq 'quit (car value))
                  (message "Quit")
                (edebug-report-error value)))
             (edebug-break
              (cond
               (edebug-global-break
                (message "Global Break: %s => %s"
                         edebug-global-break-condition
                         edebug-global-break-result))
               (edebug-break-condition
                (message "Break: %s => %s"
                         edebug-break-condition
                         edebug-break-result))
               ((not (eq edebug-execution-mode 'Continue-fast))
                (message "Break"))
               (t)))

             (t (message "")))

            (if (eq 'after arg-mode)
                (progn
                  ;; Display result of previous evaluation.
                  (if (and edebug-break
                           edebug-sit-on-break
                           (not (eq edebug-execution-mode 'Continue-fast)))
                      (sit-for edebug-sit-for-seconds)) ; Show message.
                  (edebug-previous-result)))

            (cond
             (edebug-break
              (cond
               ((eq edebug-execution-mode 'continue)
                (sit-for edebug-sit-for-seconds))
               ((eq edebug-execution-mode 'Continue-fast) (sit-for 0))
               (t (setq edebug-stop t))))
             ;; not edebug-break
             ((eq edebug-execution-mode 'trace)
              (sit-for edebug-sit-for-seconds)) ; Force update and pause.
             ((eq edebug-execution-mode 'Trace-fast)
              (sit-for 0)))		; Force update and continue.

            (when (input-pending-p)
	      (setq edebug-stop t)
	      (setq edebug-execution-mode 'step) ; for `edebug-overlay-arrow'
	      (edebug-stop))

	    (edebug-overlay-arrow)
            (edebug--overlay-breakpoints edebug-function)

            (unwind-protect
                (if (or edebug-stop
                        (memq edebug-execution-mode '(step next))
                        (eq arg-mode 'error))
		    (edebug--recursive-edit arg-mode)) ; <--- Recursive edit

              ;; Reset the edebug-window-data to whatever it is now.
              (let ((window (if (eq (window-buffer) edebug-buffer)
                                (selected-window)
                              (get-buffer-window edebug-buffer))))
                ;; Remember window-start for edebug-buffer, if still displayed.
                (if window
                    (progn
                      (setcar edebug-window-data window)
                      (setcdr edebug-window-data (window-start window)))))

              ;; Save trace window point before restoring outside windows.
              ;; Could generalize this for other buffers.
              (setq edebug-trace-window
                    (get-buffer-window edebug-trace-buffer))
              (if edebug-trace-window
                  (setq edebug-trace-window-start
                        (and edebug-trace-window
                             (window-start edebug-trace-window))))

              ;; Restore windows before continuing.
              (if edebug-save-windows
                  (progn
                    (edebug-set-windows edebug-outside-windows)

                    ;; Restore displayed buffer points.
                    ;; Needed even if restoring windows because
                    ;; window-points are not restored. (should they be??)
                    (if edebug-save-displayed-buffer-points
                        (edebug-set-buffer-points edebug-buffer-points))

                    ;; Unrestore trace window's window-point.
                    (if edebug-trace-window
                        (set-window-start edebug-trace-window
                                          edebug-trace-window-start))

                    ;; Unrestore edebug-buffer's window-start, if displayed.
                    (let ((window (car edebug-window-data)))
                      (if (and (window-live-p window)
                               (eq (window-buffer) edebug-buffer))
                          (progn
                            (set-window-start window (cdr edebug-window-data)
                                              'no-force)
                            ;; Unrestore edebug-buffer's window-point.
                            ;; Needed in addition to setting the buffer point
                            ;; - otherwise quitting doesn't leave point as is.
                            ;; But can this causes point to not be restored.
                            ;; Also, it may not be a visible window.
                            ;; (set-window-point window edebug-point)
                            )))

                    ;; Unrestore edebug-buffer's point.   Rerestored below.
                    ;;  (goto-char edebug-point) ;; in edebug-buffer
                    )
                ;; Since we may be in a save-excursion, in case of quit,
                ;; reselect the outside window only.
                ;; Only needed if we are not recovering windows??
                (if (window-live-p edebug-outside-window)
                    (select-window edebug-outside-window))
                )                       ; if edebug-save-windows

              ;; Restore current buffer always, in case application needs it.
              (if (buffer-name edebug-outside-buffer)
                  (set-buffer edebug-outside-buffer))
              ;; Restore point, and mark.
              ;; Needed even if restoring windows because
              ;; that doesn't restore point and mark in the current buffer.
              ;; But don't restore point if edebug-buffer is current buffer.
              (if (not (eq edebug-buffer edebug-outside-buffer))
                  (goto-char edebug-outside-point))
              (if (marker-buffer (mark-marker))
                  (set-marker (mark-marker) edebug-outside-mark))
              ))     ; unwind-protect
	  ;; None of the following is done if quit or signal occurs.

	  ;; Restore edebug-buffer's outside point.
	  ;;    (edebug-trace "restore edebug-buffer point: %s"
	  ;;		  edebug-buffer-outside-point)
	  (with-current-buffer edebug-buffer
	    (goto-char edebug-buffer-outside-point))
	  ;; ... nothing more.
	  )
      (edebug--overlay-breakpoints-remove (point-min) (point-max))
      ;; Could be an option to keep eval display up.
      (if edebug-eval-buffer (kill-buffer edebug-eval-buffer))
      (with-timeout-unsuspend edebug-with-timeout-suspend)
      ;; Reset global variables to outside values in case they were changed.
      (setq-default cursor-in-non-selected-windows edebug-outside-d-c-i-n-s-w)
      )))


(defvar edebug-number-of-recursions 0)
;; Number of recursive edits started by edebug.
;; Should be 0 at the top level.

(defvar edebug-recursion-depth 0)
;; Value of recursion-depth when edebug was called.

;; Dynamically declared unbound vars
(defvar edebug-outside-match-data) ; match data outside of edebug
(defvar edebug-backtrace-buffer) ; each recursive edit gets its own
(defvar edebug-inside-windows)

(defvar edebug-mode-map)		; will be defined fully later.

(defun edebug--recursive-edit (arg-mode)
  ;; Start up a recursive edit inside of edebug.
  ;; The current buffer is the edebug-buffer, which is put into edebug-mode.
  ;; Assume that none of the variables below are buffer-local.
  (let (;; match-data must be done in the outside buffer
	(edebug-outside-match-data
	 (with-current-buffer edebug-outside-buffer ; in case match buffer different
	   (match-data)))

	;;(edebug-number-of-recursions (1+ edebug-number-of-recursions))
	(edebug-recursion-depth (recursion-depth))
	edebug-entered			; bind locally to nil
	edebug-backtrace-buffer		; each recursive edit gets its own
	;; The window configuration may be saved and restored
	;; during a recursive-edit
	edebug-inside-windows
	)

    (unwind-protect
	(let (
	      ;; Declare global values local but using the same global value.
	      ;; We could set these to the values for previous edebug call.
	      (last-command last-command)
	      (this-command this-command)
	      (current-prefix-arg nil)

	      ;; More for Emacs 19
	      (last-input-event nil)
	      (last-command-event nil)
	      (last-event-frame nil)
	      (last-nonmenu-event nil)
	      (track-mouse nil)

              (standard-output t)
              (standard-input t)

	      ;; Don't keep reading from an executing kbd macro
              ;; within edebug unless edebug-continue-kbd-macro is
              ;; non-nil.  Again, local binding may not be best.
              (executing-kbd-macro
               (if edebug-continue-kbd-macro executing-kbd-macro))

              ;; Don't get confused by the user's keymap changes.
              (overriding-local-map nil)
              (overriding-terminal-local-map nil)
              ;; Override other minor modes that may bind the keys
              ;; edebug uses.
              (minor-mode-overriding-map-alist
               (list (cons 'edebug-mode edebug-mode-map)))

              ;; Bind again to outside values.
	      (debug-on-error edebug-outside-debug-on-error)
	      (debug-on-quit edebug-outside-debug-on-quit)

	      ;; Don't keep defining a kbd macro.
	      (defining-kbd-macro
		(if edebug-continue-kbd-macro defining-kbd-macro))

	      ;; others??
	      )

	  (if (and (eq edebug-execution-mode 'go)
		   (not (memq arg-mode '(after error))))
	      (message "Break"))

	  (setq signal-hook-function nil)

	  (edebug-mode 1)
	  (unwind-protect
	      (recursive-edit)		;  <<<<<<<<<< Recursive edit

	    ;; Do the following, even if quit occurs.
	    (setq signal-hook-function #'edebug-signal)
	    (if edebug-backtrace-buffer
		(kill-buffer edebug-backtrace-buffer))

	    ;; Remember selected-window after recursive-edit.
	    ;;      (setq edebug-inside-window (selected-window))

	    (set-match-data edebug-outside-match-data)

	    ;; Recursive edit may have changed buffers,
	    ;; so set it back before exiting let.
	    (if (buffer-name edebug-buffer) ; if it still exists
		(progn
		  (set-buffer edebug-buffer)
		  (when (memq edebug-execution-mode '(go Go-nonstop))
		    (edebug-overlay-arrow)
		    (sit-for 0))
                  (edebug-mode -1))
	      ;; gotta have a buffer to let its buffer local variables be set
	      (get-buffer-create " bogus edebug buffer"))
	    ));; inner let
      )))


;;; Display related functions

(defconst edebug-arrow-alist
  '((Continue-fast . "=")
    (Trace-fast . "-")
    (continue . ">")
    (trace . "->")
    (step . "=>")
    (next . "=>")
    (go . "<>")
    (Go-nonstop . "..")
    )
  "Association list of arrows for each edebug mode.")

(defun edebug-overlay-arrow ()
  ;; Set up the overlay arrow at beginning-of-line in current buffer.
  ;; The arrow string is derived from edebug-arrow-alist and
  ;; edebug-execution-mode.
  (let ((pos (line-beginning-position)))
    (setq overlay-arrow-string
	  (cdr (assq edebug-execution-mode edebug-arrow-alist)))
    (setq overlay-arrow-position (make-marker))
    (set-marker overlay-arrow-position pos (current-buffer))))


(defun edebug-toggle-save-all-windows ()
  "Toggle the saving and restoring of all windows.
Also, each time you toggle it on, the inside and outside window
configurations become the same as the current configuration."
  (interactive)
  (setq edebug-save-windows (not edebug-save-windows))
  (if edebug-save-windows
      (setq edebug-inside-windows
	    (setq edebug-outside-windows
		  (edebug-current-windows
		   edebug-save-windows))))
  (message "Window saving is %s for all windows."
	   (if edebug-save-windows "on" "off")))

(defmacro edebug-changing-windows (&rest body)
  `(let ((window (selected-window)))
     (setq edebug-inside-windows (edebug-current-windows t))
     (edebug-set-windows edebug-outside-windows)
     ,@body;; Code to change edebug-save-windows
     (setq edebug-outside-windows (edebug-current-windows
				   edebug-save-windows))
     ;; Problem: what about outside windows that are deleted inside?
     (edebug-set-windows edebug-inside-windows)))

(defun edebug-toggle-save-selected-window ()
  "Toggle the saving and restoring of the selected window.
Also, each time you toggle it on, the inside and outside window
configurations become the same as the current configuration."
  (interactive)
  (cond
   ((eq t edebug-save-windows)
    ;; Save all outside windows except the selected one.
    ;; Remove (selected-window) from outside-windows.
    (edebug-changing-windows
     (setq edebug-save-windows (delq window (edebug-window-list)))))

   ((memq (selected-window) edebug-save-windows)
    (setq edebug-outside-windows
	  (delq (assq (selected-window) edebug-outside-windows)
		edebug-outside-windows))
    (setq edebug-save-windows
	  (delq (selected-window) edebug-save-windows)))
   (t					; Save a new window.
    (edebug-changing-windows
     (setq edebug-save-windows (cons window edebug-save-windows)))))

  (message "Window saving is %s for %s."
	   (if (memq (selected-window) edebug-save-windows)
	       "on" "off")
	   (selected-window)))

(defun edebug-toggle-save-windows (arg)
  "Toggle the saving and restoring of windows.
With prefix, toggle for just the selected window.
Otherwise, toggle for all windows."
  (interactive "P")
  (if arg
      (edebug-toggle-save-selected-window)
    (edebug-toggle-save-all-windows)))

(defun edebug-where ()
  "Show the debug windows and where we stopped in the program."
  (interactive)
  (if (not edebug-active)
      (error "Edebug is not active"))
  ;; Restore the window configuration to what it last was inside.
  ;; But it is not always set.   - experiment
  ;;(if edebug-inside-windows
  ;;  (edebug-set-windows edebug-inside-windows))
  (edebug-pop-to-buffer edebug-buffer)
  (goto-char edebug-point))

(defun edebug-view-outside ()
  "Change to the outside window configuration.
Use `edebug-where' to return."
  (interactive)
  (if (not edebug-active)
      (error "Edebug is not active"))
  (setq edebug-inside-windows
	(edebug-current-windows edebug-save-windows))
  (edebug-set-windows edebug-outside-windows)
  (goto-char edebug-outside-point)
  (message "Window configuration outside of Edebug.  Return with %s"
	   (substitute-command-keys "\\<global-map>\\[edebug-where]")))


(defun edebug-bounce-point (arg)
  "Bounce the point in the outside current buffer.
If prefix argument ARG is supplied, sit for that many seconds
before returning.  The default is one second."
  (interactive "p")
  (if (not edebug-active)
      (error "Edebug is not active"))
  (save-excursion
    ;; If the buffer's currently displayed, avoid set-window-configuration.
    (save-window-excursion
      (edebug-pop-to-buffer edebug-outside-buffer)
      (goto-char edebug-outside-point)
      (message "Current buffer: %s Point: %s Mark: %s"
	       (current-buffer) (point)
               (if (marker-buffer (mark-marker))
                   (marker-position (mark-marker)) "<not set>"))
      (sit-for arg)
      (edebug-pop-to-buffer edebug-buffer (car edebug-window-data)))))


;; Joe Wells, here is a start at your idea of adding a buffer to the internal
;; display list.  Still need to use this list in edebug--display.

'(defvar edebug-display-buffer-list nil
  "List of buffers that edebug will display when it is active.")

'(defun edebug-display-buffer (buffer)
  "Toggle display of a buffer inside of edebug."
  (interactive "bBuffer: ")
  (let ((already-displaying (memq buffer edebug-display-buffer-list)))
    (setq edebug-display-buffer-list
	  (if already-displaying
	      (delq buffer edebug-display-buffer-list)
	    (cons buffer edebug-display-buffer-list)))
    (message "Displaying %s %s" buffer
	     (if already-displaying "off" "on"))))

;;; Breakpoint related functions

(defun edebug-find-stop-point ()
  ;; Return (function . index) of the nearest edebug stop point.
  (let* ((edebug-def-name (edebug-form-data-symbol))
	 (edebug-data
	   (let ((data (edebug-get-edebug-or-ghost edebug-def-name)))
	     (if (or (null data) (markerp data))
		 (error "%s is not instrumented for Edebug" edebug-def-name))
	     data))  ; we could do it automatically, if data is a marker.
	 ;; pull out parts of edebug-data.
	 (edebug-def-mark (car edebug-data))
	 ;; (edebug-breakpoints (car (cdr edebug-data)))

	 (offset-vector (nth 2 edebug-data))
	 (offset (- (save-excursion
		      (if (looking-at "[ \t]")
			  ;; skip backwards until non-whitespace, or bol
			  (skip-chars-backward " \t"))
		      (point))
		    edebug-def-mark))
	 len i)
    ;; the offsets are in order so we can do a linear search
    (setq len (length offset-vector))
    (setq i 0)
    (while (and (< i len) (> offset (aref offset-vector i)))
      (setq i (1+ i)))
    (if (and (< i len)
	     (<= offset (aref offset-vector i)))
	;; return the relevant info
	(cons edebug-def-name i)
      (message "Point is not on an expression in %s."
	       edebug-def-name)
      )))


(defun edebug-next-breakpoint ()
  "Move point to the next breakpoint, or first if none past point."
  (interactive)
  (let ((edebug-stop-point (edebug-find-stop-point)))
    (if edebug-stop-point
	(let* ((edebug-def-name (car edebug-stop-point))
	       (index (cdr edebug-stop-point))
	       (edebug-data (edebug-get-edebug-or-ghost edebug-def-name))

	       ;; pull out parts of edebug-data
	       (edebug-def-mark (car edebug-data))
	       (edebug-breakpoints (car (cdr edebug-data)))
	       (offset-vector (nth 2 edebug-data))
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
	      (goto-char (+ edebug-def-mark
			    (aref offset-vector (car breakpoint))))

	      (message "%s"
		       (concat (if (nth 2 breakpoint)
				   "Temporary " "")
			       (if (car (cdr breakpoint))
				   (format "Condition: %s"
					   (edebug-safe-prin1-to-string
					    (car (cdr breakpoint))))
				 "")))
	      ))))))


(defun edebug-modify-breakpoint (flag &optional condition temporary)
  "Modify the breakpoint for the form at point or after it.
Set it if FLAG is non-nil, clear it otherwise.  Then move to that point.
If CONDITION or TEMPORARY are non-nil, add those attributes to
the breakpoint."
  (let ((edebug-stop-point (edebug-find-stop-point)))
    (if edebug-stop-point
	(let* ((edebug-def-name (car edebug-stop-point))
	       (index (cdr edebug-stop-point))
	       (edebug-data (edebug-get-edebug-or-ghost edebug-def-name))

	       ;; pull out parts of edebug-data
	       (edebug-def-mark (car edebug-data))
	       (edebug-breakpoints (car (cdr edebug-data)))
	       (offset-vector (nth 2 edebug-data))
               (position (+ edebug-def-mark (aref offset-vector index)))
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
			(list index condition temporary
                              (set-marker (make-marker) position)
                              nil)
			edebug-breakpoints)
                       '<))
		(if condition
		    (message "Breakpoint set in %s with condition: %s"
			     edebug-def-name condition)
		  (message "Breakpoint set in %s" edebug-def-name)))
	    (if present
		(message "Breakpoint unset in %s" edebug-def-name)
	      (message "No breakpoint here")))

	  (setcar (cdr edebug-data) edebug-breakpoints)
	  (goto-char position)
          (edebug--overlay-breakpoints edebug-def-name)))))

(define-fringe-bitmap 'edebug-breakpoint
  "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")

(defun edebug--overlay-breakpoints (function)
  (let* ((data (edebug-get-edebug-or-ghost function))
         (start (nth 0 data))
         (breakpoints (nth 1 data))
         (offsets (nth 2 data)))
    ;; First remove all old breakpoint overlays.
    (edebug--overlay-breakpoints-remove
     start (+ start (aref offsets (1- (length offsets)))))
    ;; Then make overlays for the breakpoints (but only when we are in
    ;; edebug mode).
    (when edebug-active
      (dolist (breakpoint breakpoints)
        (let* ((pos (+ start (aref offsets (car breakpoint))))
               (overlay (make-overlay pos (1+ pos)))
               (face (if (nth 4 breakpoint)
                         (progn
                           (overlay-put overlay
                                        'help-echo "Disabled breakpoint")
                           (overlay-put overlay
                                        'face 'edebug-disabled-breakpoint))
                       (overlay-put overlay 'help-echo "Breakpoint")
                       (overlay-put overlay 'face 'edebug-enabled-breakpoint))))
          (overlay-put overlay 'edebug t)
          (let ((fringe (make-overlay pos pos)))
            (overlay-put fringe 'edebug t)
            (overlay-put fringe 'before-string
                         (propertize
                          "x" 'display
                          `(left-fringe edebug-breakpoint ,face)))))))))

(defun edebug--overlay-breakpoints-remove (start end)
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'edebug)
      (delete-overlay overlay))))

(defun edebug-set-breakpoint (arg)
  "Set the breakpoint of nearest sexp.
With prefix argument, make it a temporary breakpoint."
  (interactive "P")
  ;; If the form hasn't been instrumented yet, do it now.
  (when (and (not edebug-active)
	     (let ((data (edebug-get-edebug-or-ghost
                          (edebug--form-data-name
                           (edebug-get-form-data-entry (point))))))
	       (or (null data) (markerp data))))
    (edebug-defun))
  (edebug-modify-breakpoint t nil arg))

(defun edebug-unset-breakpoint ()
  "Clear the breakpoint of nearest sexp."
  (interactive)
  (edebug-modify-breakpoint nil))

(defun edebug-unset-breakpoints ()
  "Unset all the breakpoints in the current form."
  (interactive)
  (let* ((name (edebug-form-data-symbol))
         (breakpoints (nth 1 (edebug-get-edebug-or-ghost name))))
    (unless breakpoints
      (user-error "There are no breakpoints in %s" name))
    (save-excursion
      (dolist (breakpoint breakpoints)
        (goto-char (nth 3 breakpoint))
        (edebug-modify-breakpoint nil)))))

(defun edebug-toggle-disable-breakpoint ()
  "Toggle whether the breakpoint near point is disabled."
  (interactive)
  (let ((stop-point (edebug-find-stop-point)))
    (unless stop-point
      (user-error "No stop point near point"))
    (let* ((name (car stop-point))
           (index (cdr stop-point))
           (data (edebug-get-edebug-or-ghost name))
           (breakpoint (assq index (nth 1 data))))
      (unless breakpoint
        (user-error "No breakpoint near point"))
      (setf (nth 4 breakpoint)
            (not (nth 4 breakpoint)))
      (edebug--overlay-breakpoints name))))

(defun edebug-set-global-break-condition (expression)
  "Set `edebug-global-break-condition' to EXPRESSION."
  (interactive
   (list
    (let ((initial (and edebug-global-break-condition
			(format "%s" edebug-global-break-condition))))
      (read-from-minibuffer
       "Global Condition: " initial read-expression-map t
       (if (equal (car read-expression-history) initial)
	   '(read-expression-history . 1)
	 'read-expression-history)))))
  (setq edebug-global-break-condition expression))


;;; Mode switching functions

(defun edebug-set-mode (mode shortmsg msg)
  ;; Set the edebug mode to MODE.
  ;; Display SHORTMSG, or MSG if not within edebug.
  (if (eq (1+ edebug-recursion-depth) (recursion-depth))
      (progn
	(setq edebug-execution-mode mode)
	(message "%s" shortmsg)
	;; Continue execution
	(exit-recursive-edit))
    ;; This is not terribly useful!!
    (setq edebug-next-execution-mode mode)
    (message "%s" msg)))


(defalias 'edebug-step-through-mode 'edebug-step-mode)

(defun edebug-step-mode ()
  "Proceed to next stop point."
  (interactive)
  (edebug-set-mode 'step "" "Edebug will stop at next stop point."))

(defun edebug-next-mode ()
  "Proceed to next `after' stop point."
  (interactive)
  (edebug-set-mode 'next "" "Edebug will stop after next eval."))

(defun edebug-go-mode (arg)
  "Go, evaluating until break.
With prefix ARG, set temporary break at current point and go."
  (interactive "P")
  (if arg
      (edebug-set-breakpoint t))
  (edebug-set-mode 'go "Go..." "Edebug will go until break."))

(defun edebug-Go-nonstop-mode ()
  "Go, evaluating without debugging.
You can use `edebug-stop', or any editing command, to stop."
  (interactive)
  (edebug-set-mode 'Go-nonstop "Go-Nonstop..."
		   "Edebug will not stop at breaks."))


(defun edebug-trace-mode ()
  "Begin trace mode.
Pauses for `edebug-sit-for-seconds' at each stop point."
  (interactive)
  (edebug-set-mode 'trace "Tracing..." "Edebug will trace with pause."))

(defun edebug-Trace-fast-mode ()
  "Trace with no wait at each step.
Updates the display at each stop point, but does not pause."
  (interactive)
  (edebug-set-mode 'Trace-fast
		   "Trace fast..." "Edebug will trace without pause."))

(defun edebug-continue-mode ()
  "Begin continue mode.
Pauses for `edebug-sit-for-seconds' at each break point."
  (interactive)
  (edebug-set-mode 'continue "Continue..."
		   "Edebug will pause at breakpoints."))

(defun edebug-Continue-fast-mode ()
  "Trace with no wait at each step.
Updates the display at each break point, but does not pause."
  (interactive)
  (edebug-set-mode 'Continue-fast "Continue fast..."
		   "Edebug will stop and go at breakpoints."))

;; ------------------------------------------------------------
;; The following use the mode changing commands and breakpoints.


(defun edebug-goto-here ()
  "Proceed to first stop-point at or after current position of point."
  (interactive)
  (edebug-go-mode t))


(defun edebug-stop ()
  "Stop execution and do not continue.
Useful for exiting from trace or continue loop."
  (interactive)
  (message "Stop"))


'(defun edebug-forward ()
  "Proceed to the exit of the next expression to be evaluated."
  (interactive)
  (edebug-set-mode
   'forward "Forward"
   "Edebug will stop after exiting the next expression."))


(defun edebug-forward-sexp (arg)
  "Proceed from the current point to the end of the ARGth sexp ahead.
If there are not ARG sexps ahead, then do `edebug-step-out'."
  (interactive "p")
  (condition-case nil
      (let ((parse-sexp-ignore-comments t))
	;; Call forward-sexp repeatedly until done or failure.
	(forward-sexp arg)
	(edebug-go-mode t))
    (error
     (edebug-step-out)
     )))

(defun edebug-step-out ()
  "Proceed from the current point to the end of the containing sexp.
If there is no containing sexp that is not the top level defun,
go to the end of the last sexp, or if that is the same point, then step."
  (interactive)
  (condition-case nil
      (let ((parse-sexp-ignore-comments t))
	(up-list 1)
	(save-excursion
	  ;; Is there still a containing expression?
	  (up-list 1))
	(edebug-go-mode t))
    (error
     ;; At top level - 1, so first check if there are more sexps at this level.
     (let ((start-point (point)))
;;       (up-list 1)
       (down-list -1)
       (if (= (point) start-point)
	   (edebug-step-mode)	; No more at this level, so step.
	 (edebug-go-mode t)
	 )))))

(defun edebug-instrument-function (func)
  "Instrument the function or generic method FUNC.
Return the list of function symbols which were instrumented.
This may be simply (FUNC) for a normal function, or a list of
generated symbols for methods.  If a function or method to
instrument cannot be found, signal an error."
  (let ((func-marker (get func 'edebug)))
    (cond
     ((cl-generic-p func)
      (let ((method-defs (cl--generic-method-files func))
            symbols)
        (unless method-defs
          (error "Could not find any method definitions for %s" func))
        (pcase-dolist (`(,file . ,spec) method-defs)
          (let* ((loc (find-function-search-for-symbol spec 'cl-defmethod file)))
            (unless (cdr loc)
              (error "Could not find the definition for %s in its file" spec))
            (with-current-buffer (car loc)
              (goto-char (cdr loc))
              (edebug-eval-top-level-form)
              (push (edebug-form-data-symbol) symbols))))
        symbols))
     ((and (markerp func-marker) (marker-buffer func-marker))
      ;; It is uninstrumented, so instrument it.
      (with-current-buffer (marker-buffer func-marker)
	(goto-char func-marker)
	(edebug-eval-top-level-form)
        (list func)))
     ((and (consp func-marker) (consp (symbol-function func)))
      (message "%s is already instrumented." func)
      (list func))
     (t
      (let ((loc (find-function-noselect func t)))
	(unless (cdr loc)
	  (error "Could not find the definition in its file"))
	(with-current-buffer (car loc)
	  (goto-char (cdr loc))
	  (edebug-eval-top-level-form)
          (list func)))))))

(defun edebug-instrument-callee ()
  "Instrument the definition of the function or macro about to be called.
Do this when stopped before the form or it will be too late.
One side effect of using this command is that the next time the
function or macro is called, Edebug will be called there as well.
If the callee is a generic function, Edebug will instrument all
the methods, not just the one which is about to be called.  Return
the list of symbols which were instrumented."
  (interactive)
  (if (not (looking-at "("))
      (error "You must be before a list form")
    (let ((func
	   (save-excursion
	     (down-list 1)
	     (if (looking-at "(")
		 (edebug--form-data-name
		  (edebug-get-form-data-entry (point)))
	       (read (current-buffer))))))
      (edebug-instrument-function func))))


(defun edebug-step-in ()
  "Step into the definition of the function, macro or method about to be called.
This first does `edebug-instrument-callee' to ensure that it is
instrumented.  Then it does `edebug-on-entry' and switches to `go' mode."
  (interactive)
  (let ((funcs (edebug-instrument-callee)))
    (if funcs
	(progn
          (mapc (lambda (func) (edebug-on-entry func 'temp)) funcs)
          (edebug-go-mode nil)))))

(defun edebug-on-entry (function &optional flag)
  "Cause Edebug to stop when FUNCTION is called.

FUNCTION needs to be edebug-instrumented for this to work; if
FUNCTION isn't, this function has no effect.

With prefix argument, make this temporary so it is automatically
canceled the first time the function is entered."
  (interactive "aEdebug on entry to: \nP")
  ;; Could store this in the edebug data instead.
  (put function 'edebug-on-entry (if flag 'temp t)))

(define-obsolete-function-alias 'edebug-cancel-edebug-on-entry
  #'edebug-cancel-on-entry "28.1")
(define-obsolete-function-alias 'cancel-edebug-on-entry
  #'edebug-cancel-on-entry "28.1")

(defun edebug--edebug-on-entry-functions ()
  (let ((functions nil))
    (mapatoms
     (lambda (symbol)
       (when (and (fboundp symbol)
                  (get symbol 'edebug-on-entry))
         (push symbol functions)))
     obarray)
    functions))

(defun edebug-cancel-on-entry (function)
  "Cause Edebug to not stop when FUNCTION is called.
The removes the effect of `edebug-on-entry'.  If FUNCTION is is
nil, remove `edebug-on-entry' on all functions."
  (interactive
   (list (let ((name (completing-read
                      "Cancel edebug on entry to (default all functions): "
                      (let ((functions (edebug--edebug-on-entry-functions)))
                        (unless functions
                          (user-error "No functions have `edebug-on-entry'"))
                        functions))))
           (when (and name
                      (not (equal name "")))
             (intern name)))))
  (unless function
    (message "Removing `edebug-on-entry' from all functions."))
  (dolist (function (if function
                        (list function)
                      (edebug--edebug-on-entry-functions)))
    (put function 'edebug-on-entry nil)))

'(advice-add 'debug-on-entry :around 'edebug--debug-on-entry)  ;; Should we do this?
;; Also need edebug-cancel-debug-on-entry

'(defun edebug--debug-on-entry (orig function)
  "If the function is instrumented for Edebug, call `edebug-on-entry'."
  (let ((func-data (get function 'edebug)))
    (if (or (null func-data) (markerp func-data))
	(funcall orig function)
      (edebug-on-entry function))))


(defun edebug-top-level-nonstop ()
  "Set mode to Go-nonstop, and exit to top-level.
This is useful for exiting even if `unwind-protect' code may be executed."
  (interactive)
  (setq edebug-execution-mode 'Go-nonstop)
  (top-level))

;;(defun edebug-exit-out ()
;;  "Go until the current function exits."
;;  (interactive)
;;  (edebug-set-mode 'exiting "Exit..."))

(defconst edebug-initial-mode-alist
  '((edebug-step-mode . step)
    (edebug-next-mode . next)
    (edebug-trace-mode . trace)
    (edebug-Trace-fast-mode . Trace-fast)
    (edebug-go-mode . go)
    (edebug-continue-mode . continue)
    (edebug-Continue-fast-mode . Continue-fast)
    (edebug-Go-nonstop-mode . Go-nonstop))
  "Association list between commands and the modes they set.")

(defun edebug-set-initial-mode ()
  "Set the initial execution mode of Edebug.
The mode is requested via the key that would be used to set the mode in
edebug-mode."
  (interactive)
  (let* ((old-mode edebug-initial-mode)
	 (key (read-key-sequence
	       (format
		"Change initial edebug mode from %s (%c) to (enter key): "
		old-mode
		(aref (where-is-internal
		       (car (rassq old-mode edebug-initial-mode-alist))
		       edebug-mode-map 'firstonly)
		      0))))
	 (mode (cdr (assq (lookup-key edebug-mode-map key)
			  edebug-initial-mode-alist))))
    (if mode
	(progn
	  (setq edebug-initial-mode mode)
	  (message "Edebug's initial mode is now: %s" mode))
      (error "Key must map to one of the mode changing commands"))))

;;; Evaluation of expressions

(defmacro edebug-outside-excursion (&rest body)
  "Evaluate an expression list in the outside context.
Return the result of the last expression."
  ;; Only restores the non-variables context since all the variables let-bound
  ;; by Edebug will be properly reset to the appropriate context's value by
  ;; backtrace-eval.
  (declare (debug t))
  `(save-excursion			; of current-buffer
     (if edebug-save-windows
	 (progn
	   ;; After excursion, we will
	   ;; restore to current window configuration.
	   (setq edebug-inside-windows
		 (edebug-current-windows edebug-save-windows))
	   ;; Restore outside windows.
	   (edebug-set-windows edebug-outside-windows)))

     (set-buffer edebug-buffer)		; why?
     (set-match-data edebug-outside-match-data)
     ;; Restore outside context.
     (setq-default cursor-in-non-selected-windows edebug-outside-d-c-i-n-s-w)
     (unwind-protect
         ;; FIXME: This restoring of edebug-outside-buffer and
         ;; edebug-outside-point is redundant now that backtrace-eval does it
         ;; for us.
         (with-current-buffer edebug-outside-buffer ; of edebug-buffer
           (goto-char edebug-outside-point)
           (if (marker-buffer (mark-marker))
               (set-marker (mark-marker) edebug-outside-mark))
           ,@body)

       ;; Back to edebug-buffer.  Restore rest of inside context.
       ;; (use-local-map edebug-inside-map)
       (if edebug-save-windows
           ;; Restore inside windows.
           (edebug-set-windows edebug-inside-windows))

       ;; Save values that may have been changed.
       (setq edebug-outside-d-c-i-n-s-w
             (default-value 'cursor-in-non-selected-windows))

       ;; Restore the outside saved values; don't alter
       ;; the outside binding loci.
       (setq-default cursor-in-non-selected-windows t))))

(defun edebug-eval (expr)
  (backtrace-eval expr 0 'edebug-after))

(defun edebug-safe-eval (expr)
  ;; Evaluate EXPR safely.
  ;; If there is an error, a string is returned describing the error.
  (condition-case edebug-err
      (edebug-eval expr)
    (error (edebug-format "%s: %s"  ;; could
			  (get (car edebug-err) 'error-message)
			  (car (cdr edebug-err))))))

;;; Printing


(defun edebug-report-error (value)
  ;; Print an error message like command level does.
  ;; This also prints the error name if it has no error-message.
  (message "%s: %s"
	   (or (get (car value) 'error-message)
	       (format "peculiar error (%s)" (car value)))
	   (mapconcat (lambda (edebug-arg)
                        ;; continuing after an error may
                        ;; complain about edebug-arg. why??
                        (prin1-to-string edebug-arg))
		      (cdr value) ", ")))

;; Alternatively, we could change the definition of
;; edebug-safe-prin1-to-string to only use these if defined.

(defun edebug-safe-prin1-to-string (value)
  (let ((print-escape-newlines t)
	(print-length (or edebug-print-length print-length))
	(print-level (or edebug-print-level print-level))
	(print-circle (or edebug-print-circle print-circle)))
    (edebug-prin1-to-string value)))

(defun edebug-compute-previous-result (previous-value)
  (if edebug-unwrap-results
      (setq previous-value
	    (edebug-unwrap* previous-value)))
  (setq edebug-previous-result
	(concat "Result: "
		(edebug-safe-prin1-to-string previous-value)
		(eval-expression-print-format previous-value))))

(defun edebug-previous-result ()
  "Print the previous result."
  (interactive)
  (message "%s" edebug-previous-result))

;;; Read, Eval and Print

(defalias 'edebug-prin1-to-string #'cl-prin1-to-string)
(defalias 'edebug-format #'format-message)
(defalias 'edebug-message #'message)

(defun edebug-eval-expression (expr)
  "Evaluate an expression in the outside environment.
If interactive, prompt for the expression.
Print result in minibuffer."
  (interactive (list (read--expression "Eval: ")))
  (princ
   (edebug-outside-excursion
    (let ((result (edebug-eval expr)))
      (values--store-value result)
      (concat (edebug-safe-prin1-to-string result)
              (eval-expression-print-format result))))))

(defun edebug-eval-last-sexp (&optional no-truncate)
  "Evaluate sexp before point in the outside environment.
Print value in minibuffer.

If NO-TRUNCATE is non-nil (or interactively with a prefix
argument of zero), show the full length of the expression, not
limited by `edebug-print-length' or `edebug-print-level'."
  (interactive
   (list (and current-prefix-arg
              (zerop (prefix-numeric-value current-prefix-arg)))))
  (if no-truncate
      (let ((edebug-print-length nil)
            (edebug-print-level nil))
        (edebug-eval-expression (edebug-last-sexp)))
    (edebug-eval-expression (edebug-last-sexp))))

(defun edebug-eval-print-last-sexp (&optional no-truncate)
  "Evaluate sexp before point in outside environment; insert value.
This prints the value into current buffer.

If NO-TRUNCATE is non-nil (or interactively with a prefix
argument of zero), show the full length of the expression, not
limited by `edebug-print-length' or `edebug-print-level'."
  (interactive
   (list (and current-prefix-arg
              (zerop (prefix-numeric-value current-prefix-arg)))))
  (let* ((form (edebug-last-sexp))
	 (result-string
	  (edebug-outside-excursion
	   (if no-truncate
               (let ((edebug-print-length nil)
                     (edebug-print-level nil))
                 (edebug-safe-prin1-to-string (edebug-safe-eval form)))
             (edebug-safe-prin1-to-string (edebug-safe-eval form)))))
	 (standard-output (current-buffer)))
    (princ "\n")
    ;; princ the string to get rid of quotes.
    (princ result-string)
    (princ "\n")))

;;; Edebug Minor Mode

(define-obsolete-variable-alias 'gud-inhibit-global-bindings
  'edebug-inhibit-emacs-lisp-mode-bindings "24.3")

(defvar edebug-inhibit-emacs-lisp-mode-bindings nil
  "If non-nil, inhibit Edebug bindings on the C-x C-a key.
By default, loading the `edebug' library causes these bindings to
be installed in `emacs-lisp-mode-map'.")

;; Global GUD bindings for all emacs-lisp-mode buffers.
(unless edebug-inhibit-emacs-lisp-mode-bindings
  (define-key emacs-lisp-mode-map "\C-x\C-a\C-s" 'edebug-step-mode)
  (define-key emacs-lisp-mode-map "\C-x\C-a\C-n" 'edebug-next-mode)
  (define-key emacs-lisp-mode-map "\C-x\C-a\C-c" 'edebug-go-mode)
  (define-key emacs-lisp-mode-map "\C-x\C-a\C-l" 'edebug-where)
  ;; The following isn't a GUD binding.
  (define-key emacs-lisp-mode-map "\C-x\C-a\C-m" 'edebug-set-initial-mode))

(defvar edebug-mode-map
  (let ((map (copy-keymap emacs-lisp-mode-map)))
    ;; control
    (define-key map " " 'edebug-step-mode)
    (define-key map "n" 'edebug-next-mode)
    (define-key map "g" 'edebug-go-mode)
    (define-key map "G" 'edebug-Go-nonstop-mode)
    (define-key map "t" 'edebug-trace-mode)
    (define-key map "T" 'edebug-Trace-fast-mode)
    (define-key map "c" 'edebug-continue-mode)
    (define-key map "C" 'edebug-Continue-fast-mode)

    ;;(define-key map "f" 'edebug-forward) not implemented
    (define-key map "f" 'edebug-forward-sexp)
    (define-key map "h" 'edebug-goto-here)

    (define-key map "I" 'edebug-instrument-callee)
    (define-key map "i" 'edebug-step-in)
    (define-key map "o" 'edebug-step-out)

    ;; quitting and stopping
    (define-key map "q" 'top-level)
    (define-key map "Q" 'edebug-top-level-nonstop)
    (define-key map "a" 'abort-recursive-edit)
    (define-key map "S" 'edebug-stop)

    ;; breakpoints
    (define-key map "b" 'edebug-set-breakpoint)
    (define-key map "u" 'edebug-unset-breakpoint)
    (define-key map "U" 'edebug-unset-breakpoints)
    (define-key map "B" 'edebug-next-breakpoint)
    (define-key map "x" 'edebug-set-conditional-breakpoint)
    (define-key map "X" 'edebug-set-global-break-condition)
    (define-key map "D" 'edebug-toggle-disable-breakpoint)

    ;; evaluation
    (define-key map "r" 'edebug-previous-result)
    (define-key map "e" 'edebug-eval-expression)
    (define-key map "\C-x\C-e" 'edebug-eval-last-sexp)
    (define-key map "E" 'edebug-visit-eval-list)

    ;; views
    (define-key map "w" 'edebug-where)
    (define-key map "v" 'edebug-view-outside) ;; maybe obsolete??
    (define-key map "p" 'edebug-bounce-point)
    (define-key map "P" 'edebug-view-outside) ;; same as v
    (define-key map "W" 'edebug-toggle-save-windows)

    ;; misc
    (define-key map "?" 'edebug-help)
    (define-key map "d" 'edebug-pop-to-backtrace)

    (define-key map "-" 'negative-argument)

    ;; statistics
    (define-key map "=" 'edebug-temp-display-freq-count)

    ;; GUD bindings
    (define-key map "\C-c\C-s" 'edebug-step-mode)
    (define-key map "\C-c\C-n" 'edebug-next-mode)
    (define-key map "\C-c\C-c" 'edebug-go-mode)

    (define-key map "\C-x " 'edebug-set-breakpoint)
    (define-key map "\C-c\C-d" 'edebug-unset-breakpoint)
    (define-key map "\C-c\C-t"
      (lambda () (interactive) (edebug-set-breakpoint t)))
    (define-key map "\C-c\C-l" 'edebug-where)
    map))

;; Autoloading these global bindings doesn't make sense because
;; they cannot be used anyway unless Edebug is already loaded and active.

(define-obsolete-variable-alias 'global-edebug-prefix
  'edebug-global-prefix "28.1")
(defvar edebug-global-prefix "\^XX"
  "Prefix key for global edebug commands, available from any buffer.")

(define-obsolete-variable-alias 'global-edebug-map
  'edebug-global-map "28.1")
(defvar edebug-global-map
  (let ((map (make-sparse-keymap)))

    (define-key map " " 'edebug-step-mode)
    (define-key map "g" 'edebug-go-mode)
    (define-key map "G" 'edebug-Go-nonstop-mode)
    (define-key map "t" 'edebug-trace-mode)
    (define-key map "T" 'edebug-Trace-fast-mode)
    (define-key map "c" 'edebug-continue-mode)
    (define-key map "C" 'edebug-Continue-fast-mode)

    ;; breakpoints
    (define-key map "b" 'edebug-set-breakpoint)
    (define-key map "u" 'edebug-unset-breakpoint)
    (define-key map "U" 'edebug-unset-breakpoints)
    (define-key map "x" 'edebug-set-conditional-breakpoint)
    (define-key map "X" 'edebug-set-global-break-condition)
    (define-key map "D" 'edebug-toggle-disable-breakpoint)

    ;; views
    (define-key map "w" 'edebug-where)
    (define-key map "W" 'edebug-toggle-save-windows)

    ;; quitting
    (define-key map "q" 'top-level)
    (define-key map "Q" 'edebug-top-level-nonstop)
    (define-key map "a" 'abort-recursive-edit)

    ;; statistics
    (define-key map "=" 'edebug-display-freq-count)
    map)
  "Global map of edebug commands, available from any buffer.")

(when edebug-global-prefix
  (global-unset-key edebug-global-prefix)
  (global-set-key edebug-global-prefix edebug-global-map))


(defun edebug-help ()
  "Describe `edebug-mode'."
  (interactive)
  (describe-function 'edebug-mode))

(defvar edebug--mode-saved-vars nil)

(define-minor-mode edebug-mode
  "Mode for Emacs Lisp buffers while in Edebug.

In addition to all Emacs Lisp commands (except those that modify the
buffer) there are local and global key bindings to several Edebug
specific commands.  E.g. `edebug-step-mode' is bound to \\[edebug-step-mode]
in the Edebug buffer and \\<global-map>\\[edebug-step-mode] in any buffer.

Also see bindings for the eval list buffer *edebug* in `edebug-eval-mode'.

The edebug buffer commands:
\\{edebug-mode-map}

Global commands prefixed by `global-edebug-prefix':
\\{global-edebug-map}

Options:
`edebug-setup-hook'
`edebug-all-defs'
`edebug-all-forms'
`edebug-save-windows'
`edebug-save-displayed-buffer-points'
`edebug-initial-mode'
`edebug-trace'
`edebug-test-coverage'
`edebug-continue-kbd-macro'
`edebug-print-length'
`edebug-print-level'
`edebug-print-circle'
`edebug-on-error'
`edebug-on-quit'
`edebug-unwrap-results'
`edebug-global-break-condition'"
  :lighter " *Debugging*"
  :keymap edebug-mode-map
  ;; If the user kills the buffer in which edebug is currently active,
  ;; exit to top level, because the edebug command loop can't usefully
  ;; continue running in such a case.
  ;;
  (if (not edebug-mode)
      (progn
        (while edebug--mode-saved-vars
          (let ((setting (pop edebug--mode-saved-vars)))
            (if (consp setting)
                (set (car setting) (cdr setting))
              (kill-local-variable setting))))
        (remove-hook 'kill-buffer-hook #'edebug-kill-buffer t))
    (pcase-dolist (`(,var . ,val) '((buffer-read-only . t)))
      (push
       (if (local-variable-p var) (cons var (symbol-value var)) var)
       edebug--mode-saved-vars)
      (set (make-local-variable var) val))
    ;; Append `edebug-kill-buffer' to the hook to avoid interfering with
    ;; other entries that are unguarded against deleted buffer.
    (add-hook 'kill-buffer-hook #'edebug-kill-buffer t t)))

(defun edebug-kill-buffer ()
  "Used on `kill-buffer-hook' when Edebug is operating in a buffer of Lisp code."
  (run-with-timer 0 nil #'top-level))

;;; edebug eval list mode

;; A list of expressions and their evaluations is displayed in *edebug*.

(defun edebug-eval-result-list ()
  "Return a list of evaluations of `edebug-eval-list'."
  ;; Assumes in outside environment.
  ;; Don't do any edebug things now.
  (let ((edebug-execution-mode 'Go-nonstop)
	(edebug-trace nil))
    (mapcar #'edebug-safe-eval edebug-eval-list)))

(defun edebug-eval-display-list (eval-result-list)
  ;; Assumes edebug-eval-buffer exists.
  (let ((standard-output edebug-eval-buffer)
	(edebug-comment-line
	 (format ";%s\n" (make-string (- (window-width) 2) ?-))))
    (set-buffer edebug-eval-buffer)
    (erase-buffer)
    (dolist (exp edebug-eval-list)
      (prin1 exp) (terpri)
      (prin1 (pop eval-result-list)) (terpri)
      (princ edebug-comment-line))
    (edebug-pop-to-buffer edebug-eval-buffer)
    ))

(defun edebug-create-eval-buffer ()
  (unless (and edebug-eval-buffer (buffer-name edebug-eval-buffer))
    (set-buffer (setq edebug-eval-buffer (get-buffer-create "*edebug*")))
    (edebug-eval-mode)))

;; Should generalize this to be callable outside of edebug
;; with calls in user functions, e.g. (edebug-eval-display)

(defun edebug-eval-display (eval-result-list)
  "Display expressions and evaluations in EVAL-RESULT-LIST.
It modifies the context by popping up the eval display."
  (when eval-result-list
    (edebug-create-eval-buffer)
    (edebug-eval-display-list eval-result-list)))

(defun edebug-eval-redisplay ()
  "Redisplay eval list in outside environment.
May only be called from within `edebug--recursive-edit'."
  (edebug-create-eval-buffer)
  (edebug-outside-excursion
   (edebug-eval-display-list (edebug-eval-result-list))
   ))

(defun edebug-visit-eval-list ()
  "Switch to the evaluation list buffer \"*edebug*\"."
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
	  (push (edebug-last-sexp) new-list)))

    (while (re-search-forward "^;" nil t)
      (forward-line 1)
      (skip-chars-forward " \t\n\r")
      (if (and (/= ?\; (following-char))
	       (not (eobp)))
	  (progn
	    (forward-sexp 1)
	    (push (edebug-last-sexp) new-list))))

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



(defvar edebug-eval-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-interaction-mode-map)
    (define-key map "\C-c\C-w" 'edebug-where)
    (define-key map "\C-c\C-d" 'edebug-delete-eval-item)
    (define-key map "\C-c\C-u" 'edebug-update-eval-list)
    (define-key map "\C-x\C-e" 'edebug-eval-last-sexp)
    (define-key map "\C-j" 'edebug-eval-print-last-sexp)
    map)
  "Keymap for Edebug Eval mode.  Superset of Lisp Interaction mode.")

(put 'edebug-eval-mode 'mode-class 'special)

(define-derived-mode edebug-eval-mode lisp-interaction-mode "Edebug Eval"
  "Mode for evaluation list buffer while in Edebug.

In addition to all Interactive Emacs Lisp commands there are local and
global key bindings to several Edebug specific commands.  E.g.
`edebug-step-mode' is bound to \\[edebug-step-mode] in the Edebug
buffer and \\<global-map>\\[edebug-step-mode] in any buffer.

Eval list buffer commands:
\\{edebug-eval-mode-map}

Global commands prefixed by `global-edebug-prefix':
\\{global-edebug-map}")

;;; Interface with standard debugger.

;; (setq debugger 'edebug) ; to use the edebug debugger
;; (setq debugger 'debug)  ; use the standard debugger

;; Note that debug and its utilities must be byte-compiled to work,
;; since they depend on the backtrace looking a certain way.  Edebug
;; will work if not byte-compiled, but it will not be able correctly
;; remove its instrumentation from backtraces unless it is
;; byte-compiled.

(defun edebug (&optional arg-mode &rest args)
  "Replacement for `debug'.
If we are running an edebugged function, show where we last were.
Otherwise call `debug' normally."
  ;;(message "entered: %s  depth: %s  edebug-recursion-depth: %s"
  ;;  edebug-entered (recursion-depth) edebug-recursion-depth) (sit-for 1)
  (if (and edebug-entered  ; anything active?
	   (eq (recursion-depth) edebug-recursion-depth))
      (let (;; Where were we before the error occurred?
	    (offset-index (car edebug-offset-indices))
	    (value (car args))
	    ;; Bind variables required by edebug--display.
	    edebug-breakpoints
	    edebug-break-data
	    edebug-break-condition
	    edebug-global-break
	    (edebug-break (null arg-mode)) ;; If called explicitly.
	    )
	(edebug--display value offset-index arg-mode)
	(if (eq arg-mode 'error)
	    nil
	  value))

    ;; Otherwise call debug normally.
    ;; Still need to remove extraneous edebug calls from stack.
    (apply #'debug arg-mode args)
    ))

;;; Backtrace buffer

(defvar-local edebug-backtrace-frames nil
  "Stack frames of the current Edebug Backtrace buffer without instrumentation.
This should be a list of `edebug---frame' objects.")
(defvar-local edebug-instrumented-backtrace-frames nil
  "Stack frames of the current Edebug Backtrace buffer with instrumentation.
This should be a list of `edebug---frame' objects.")

;; Data structure for backtrace frames with information
;; from Edebug instrumentation found in the backtrace.
(cl-defstruct
    (edebug--frame
     (:constructor edebug--make-frame)
     (:include backtrace-frame))
  def-name before-index after-index)

(defun edebug-pop-to-backtrace ()
  "Display the current backtrace in a `backtrace-mode' window."
  (interactive)
  (if (or (not edebug-backtrace-buffer)
	  (null (buffer-name edebug-backtrace-buffer)))
      (setq edebug-backtrace-buffer
	    (generate-new-buffer "*Edebug Backtrace*"))
    ;; Else, could just display edebug-backtrace-buffer.
    )
  (pop-to-buffer edebug-backtrace-buffer)
  (unless (derived-mode-p 'backtrace-mode)
    (backtrace-mode)
    (add-hook 'backtrace-goto-source-functions
              #'edebug--backtrace-goto-source nil t))
  (setq edebug-instrumented-backtrace-frames
        (backtrace-get-frames 'edebug-debugger
                              :constructor #'edebug--make-frame)
        edebug-backtrace-frames (edebug--strip-instrumentation
                                 edebug-instrumented-backtrace-frames)
        backtrace-frames edebug-backtrace-frames)
  (backtrace-print)
  (goto-char (point-min)))

(defun edebug--strip-instrumentation (frames)
  "Return a new list of backtrace frames with instrumentation removed.
Remove frames for Edebug's functions and the lambdas in
`edebug-enter' wrappers.  Fill in the def-name, before-index
and after-index fields in both FRAMES and the returned list
of deinstrumented frames, for those frames where the source
code location is known."
  (let (skip-next-lambda def-name before-index after-index results
        (index (length frames)))
    (dolist (frame (reverse frames))
      (let ((new-frame (copy-edebug--frame frame))
            (fun (edebug--frame-fun frame))
            (args (edebug--frame-args frame)))
        (cl-decf index)
        (pcase fun
          ('edebug-enter
	   (setq skip-next-lambda t
                 def-name (nth 0 args)))
          ('edebug-after
           (setq before-index (if (consp (nth 0 args))
                                  (nth 1 (nth 0 args))
                                (nth 0 args))
                 after-index (nth 1 args)))
          ((pred edebug--symbol-not-prefixed-p)
           (edebug--unwrap-frame new-frame)
           (edebug--add-source-info new-frame def-name before-index after-index)
           (edebug--add-source-info frame def-name before-index after-index)
           (push new-frame results)
           (setq before-index nil
                 after-index nil))
          (`(,(or 'lambda 'closure) . ,_)
	   (unless skip-next-lambda
             (edebug--unwrap-frame new-frame)
             (edebug--add-source-info frame def-name before-index after-index)
             (edebug--add-source-info new-frame def-name before-index after-index)
             (push new-frame results))
	   (setq before-index nil
                 after-index nil
                 skip-next-lambda nil)))))
    results))

(defun edebug--symbol-not-prefixed-p (sym)
  "Return non-nil if SYM is a symbol not prefixed by \"edebug-\"."
  (and (symbolp sym)
       (not (string-prefix-p "edebug-" (symbol-name sym)))))

(defun edebug--unwrap-frame (frame)
  "Remove Edebug's instrumentation from FRAME.
Strip it from the function and any unevaluated arguments."
  (setf (edebug--frame-fun frame) (edebug-unwrap* (edebug--frame-fun frame)))
  (unless (edebug--frame-evald frame)
    (let (results)
      (dolist (arg (edebug--frame-args frame))
        (push (edebug-unwrap* arg) results))
      (setf (edebug--frame-args frame) (nreverse results)))))

(defun edebug--add-source-info (frame def-name before-index after-index)
  "Update FRAME with the additional info needed by an edebug--frame.
Save DEF-NAME, BEFORE-INDEX and AFTER-INDEX in FRAME."
  (when (and before-index def-name)
    (setf (edebug--frame-flags frame)
          (plist-put (copy-sequence (edebug--frame-flags frame))
                     :source-available t)))
  (setf (edebug--frame-def-name frame) (and before-index def-name))
  (setf (edebug--frame-before-index frame) before-index)
  (setf (edebug--frame-after-index frame) after-index))

(defun edebug--backtrace-goto-source ()
  (let* ((index (backtrace-get-index))
         (frame (nth index backtrace-frames)))
    (when (edebug--frame-def-name frame)
      (let* ((data (edebug-get-edebug-or-ghost (edebug--frame-def-name frame)))
             (marker (nth 0 data))
             (offsets (nth 2 data)))
        (pop-to-buffer (marker-buffer marker))
        (goto-char (+ (marker-position marker)
                      (aref offsets (edebug--frame-before-index frame))))))))

(defun edebug-backtrace-show-instrumentation ()
  "Show Edebug's instrumentation in an Edebug Backtrace buffer."
  (interactive)
  (unless (eq backtrace-frames edebug-instrumented-backtrace-frames)
    (setq backtrace-frames edebug-instrumented-backtrace-frames)
    (revert-buffer)))

(defun edebug-backtrace-hide-instrumentation ()
  "Hide Edebug's instrumentation in an Edebug Backtrace buffer."
  (interactive)
  (unless (eq backtrace-frames edebug-backtrace-frames)
    (setq backtrace-frames edebug-backtrace-frames)
    (revert-buffer)))

;;; Trace display

(defun edebug-trace-display (buf-name fmt &rest args)
  "In buffer BUF-NAME, display FMT and ARGS at the end and make it visible.
The buffer is created if it does not exist.
You must include newlines in FMT to break lines, but one newline is appended."
  ;; e.g.
  ;; (edebug-trace-display "*trace-point*"
  ;;                       "saving: point = %s  window-start = %s"
  ;;                       (point) (window-start))
  (let* ((oldbuf (current-buffer))
	 (selected-window (selected-window))
	 (buffer (get-buffer-create buf-name))
	 buf-window)
    ;; (message "before pop-to-buffer") (sit-for 1)
    (edebug-pop-to-buffer buffer)
    (setq truncate-lines t)
    (setq buf-window (selected-window))
    (goto-char (point-max))
    (insert (apply #'edebug-format fmt args) "\n")
    ;; Make it visible.
    (vertical-motion (- 1 (window-height)))
    (set-window-start buf-window (point))
    (goto-char (point-max))
    ;; (set-window-point buf-window (point))
    ;; (sit-for 0)
    (bury-buffer buffer)
    (select-window selected-window)
    (set-buffer oldbuf))
  buf-name)


(defun edebug-trace (fmt &rest args)
  "Convenience call to `edebug-trace-display' using `edebug-trace-buffer'."
  (apply #'edebug-trace-display edebug-trace-buffer fmt args))


;;; Frequency count and coverage

;; FIXME should this use overlays instead?
;; Definitely, IMO.  The current business with undo in
;; edebug-temp-display-freq-count is horrid.
(defun edebug-display-freq-count ()
  "Display the frequency count data for each line of the current definition.
The frequency counts are inserted as comment lines after each line,
and you can undo all insertions with one `undo' command.

The counts are inserted starting under the `(' before an expression
or the `)' after an expression, or on the last char of a symbol.
The counts are only displayed when they differ from previous counts on
the same line.

If coverage is being tested, whenever all known results of an expression
are `eq', the char `=' will be appended after the count
for that expression.  Note that this is always the case for an
expression only evaluated once.

To clear the frequency count and coverage data for a definition,
reinstrument it."
  (interactive)
  (let* ((function (edebug-form-data-symbol))
	 (counts (get function 'edebug-freq-count))
	 (coverages (get function 'edebug-coverage))
	 (data (edebug-get-edebug-or-ghost function))
	 (def-mark (car data))	; mark at def start
	 (edebug-points (nth 2 data))
	 (i (1- (length edebug-points)))
	 (last-index)
	 (first-index)
	 (start-of-line)
	 (start-of-count-line)
	 (last-count)
	 )
    (save-excursion
      ;; Traverse in reverse order so offsets are correct.
      (while (<= 0 i)
	;; Start at last expression in line.
	(goto-char (+ def-mark (aref edebug-points i)))
	(beginning-of-line)
	(setq start-of-line (- (point) def-mark)
	      last-index i)

	;; Find all indexes on same line.
	(while (and (<= 0 (setq i (1- i)))
		    (<= start-of-line (aref edebug-points i))))
	;; Insert all the indices for this line.
	(forward-line 1)
	(setq start-of-count-line (point)
	      first-index i   ; Really, last index for line above this one.
	      last-count -1)  ; Cause first count to always appear.
	(insert ";#")
	;; i == first-index still
	(while (<= (setq i (1+ i)) last-index)
	  (let ((count (aref counts i))
		(coverage (aref coverages i))
		(col (save-excursion
		       (goto-char (+ (aref edebug-points i) def-mark))
		       (- (current-column)
			  (if (= ?\( (following-char)) 0 1)))))
	    (insert (make-string
		     (max 0 (- col (- (point) start-of-count-line))) ?\s)
		    (if (and (< 0 count)
			     (not (memq coverage
					'(edebug-unknown edebug-ok-coverage))))
			"=" "")
		    (if (= count last-count) "" (int-to-string count))
		    " ")
	    (setq last-count count)))
	(insert "\n")
	(setq i first-index)))))

;; FIXME this does not work very well.  Eg if you press an arrow key,
;; or make a mouse-click, it fails with "Non-character input-event".
(defun edebug-temp-display-freq-count ()
  "Temporarily display the frequency count data for the current definition.
It is removed when you hit any char."
  (interactive)
  (let ((inhibit-read-only t))
    (undo-boundary)
    (edebug-display-freq-count)
    (setq unread-command-events
          (append unread-command-events (list (read-event))))
    ;; Yuck!  This doesn't seem to work at all for me.
    (undo)))


;;; Menus

(defun edebug-toggle (variable)
  (set variable (not (symbol-value variable)))
  (message "%s: %s" variable (symbol-value variable)))

;; We have to require easymenu (even for Emacs 18) just so
;; the easy-menu-define macro call is compiled correctly.
(require 'easymenu)

(defconst edebug-mode-menus
  '("Edebug"
     ["Stop" edebug-stop t]
     ["Step" edebug-step-mode t]
     ["Next" edebug-next-mode t]
     ["Trace" edebug-trace-mode t]
     ["Trace Fast" edebug-Trace-fast-mode t]
     ["Continue" edebug-continue-mode t]
     ["Continue Fast" edebug-Continue-fast-mode t]
     ["Go" edebug-go-mode t]
     ["Go Nonstop" edebug-Go-nonstop-mode t]
     "----"
     ["Help" edebug-help t]
     ["Abort" abort-recursive-edit t]
     ["Quit to Top Level"  top-level t]
     ["Quit Nonstop" edebug-top-level-nonstop t]
     "----"
    ("Jumps"
     ["Forward Sexp" edebug-forward-sexp t]
     ["Step In" edebug-step-in t]
     ["Step Out" edebug-step-out t]
     ["Goto Here" edebug-goto-here t])

    ("Breaks"
     ["Set Breakpoint" edebug-set-breakpoint t]
     ["Unset Breakpoint" edebug-unset-breakpoint t]
     ["Unset Breakpoints In Form" edebug-unset-breakpoints t]
     ["Toggle Disable Breakpoint" edebug-toggle-disable-breakpoint t]
     ["Set Conditional Breakpoint" edebug-set-conditional-breakpoint t]
     ["Set Global Break Condition" edebug-set-global-break-condition t]
     ["Show Next Breakpoint" edebug-next-breakpoint t])

    ("Views"
     ["Where am I?" edebug-where t]
     ["Bounce to Current Point" edebug-bounce-point t]
     ["View Outside Windows" edebug-view-outside t]
     ["Previous Result" edebug-previous-result t]
     ["Show Backtrace" edebug-pop-to-backtrace t]
     ["Display Freq Count" edebug-display-freq-count t])

    ("Eval"
     ["Expression" edebug-eval-expression t]
     ["Last Sexp" edebug-eval-last-sexp t]
     ["Visit Eval List" edebug-visit-eval-list t])

    ("Options"
     ["Edebug All Defs" edebug-all-defs
      :style toggle :selected edebug-all-defs]
     ["Edebug All Forms" edebug-all-forms
      :style toggle :selected edebug-all-forms]
     "----"
     ["Tracing" (edebug-toggle 'edebug-trace)
      :style toggle :selected edebug-trace]
     ["Test Coverage" (edebug-toggle 'edebug-test-coverage)
      :style toggle :selected edebug-test-coverage]
     ["Save Windows" edebug-toggle-save-windows
      :style toggle :selected edebug-save-windows]
     ["Save Point"
      (edebug-toggle 'edebug-save-displayed-buffer-points)
      :style toggle :selected edebug-save-displayed-buffer-points]
     ))
  "Menus for Edebug.")


;;; Emacs version specific code

(defun edebug-set-conditional-breakpoint (arg condition)
  "Set a conditional breakpoint at nearest sexp.
The condition is evaluated in the outside context.
With prefix argument, make it a temporary breakpoint."
  ;; (interactive "P\nxCondition: ")
  (interactive
   (list
    current-prefix-arg
    ;; Read condition as follows; getting previous condition is cumbersome:
    (let ((edebug-stop-point (edebug-find-stop-point)))
      (if edebug-stop-point
	  (let* ((edebug-def-name (car edebug-stop-point))
		 (index (cdr edebug-stop-point))
		 (edebug-data (edebug-get-edebug-or-ghost edebug-def-name))
		 (edebug-breakpoints (car (cdr edebug-data)))
		 (edebug-break-data (assq index edebug-breakpoints))
		 (edebug-break-condition (car (cdr edebug-break-data)))
		 (initial (and edebug-break-condition
			       (format "%s" edebug-break-condition))))
	    (read-from-minibuffer
	     "Condition: " initial read-expression-map t
	     (if (equal (car read-expression-history) initial)
		 '(read-expression-history . 1)
	       'read-expression-history)))))))
  (edebug-modify-breakpoint t condition arg))

(easy-menu-define edebug-menu edebug-mode-map "Edebug menus" edebug-mode-menus)


;;; Finalize Loading

;; When edebugging a function, some of the sub-expressions are
;; wrapped in (edebug-enter (lambda () ..)), so we need to teach
;; called-interactively-p that calls within the inner lambda should refer to
;; the outside function.
(add-hook 'called-interactively-p-functions
          #'edebug--called-interactively-skip)
(defun edebug--called-interactively-skip (i frame1 frame2)
  (when (and (memq (car-safe (nth 1 frame1)) '(lambda closure))
             ;; Lambda value with no arguments.
             (null (nth (if (eq (car-safe (nth 1 frame1)) 'lambda) 1 2)
                        (nth 1 frame1)))
             (memq (nth 1 frame2) '(edebug-enter edebug-default-enter)))
    ;; `edebug-enter' calls itself on its first invocation.
    (let ((s 1))
      (while (memq (nth 1 (backtrace-frame i 'called-interactively-p))
                   '(edebug-enter edebug-default-enter))
        (cl-incf s)
        (cl-incf i))
      s)))

;; Finally, hook edebug into the rest of Emacs.
;; There are probably some other things that could go here.

;; Install edebug read and eval functions.
(edebug-install-read-eval-functions)

(defun edebug-unload-function ()
  "Unload the Edebug source level debugger."
  (when edebug-active
    (setq edebug-active nil)
    (unwind-protect
        (abort-recursive-edit)
      ;; We still want to run unload-feature to completion
      (run-with-idle-timer 0 nil #'(lambda () (unload-feature 'edebug)))))
  (remove-hook 'called-interactively-p-functions
               #'edebug--called-interactively-skip)
  (edebug-uninstall-read-eval-functions)
  ;; Continue standard unloading.
  nil)

(defun edebug--unwrap*-symbol-function (symbol)
  ;; Try to unwrap SYMBOL's `symbol-function'.  The result is suitable
  ;; to be fbound back to SYMBOL with `defalias'.  When no unwrapping
  ;; could be done return nil.
  (pcase (symbol-function symbol)
    ((or (and `(macro . ,f) (let was-macro t))
         (and  f            (let was-macro nil)))
     ;; `defalias' takes care of advises so we must strip them
     (let* ((orig-f (advice--cd*r f))
            (unwrapped (edebug-unwrap* orig-f)))
       (cond
        ((equal unwrapped orig-f) nil)
        (was-macro               `(macro . ,unwrapped))
        (t                       unwrapped))))))

(defun edebug-remove-instrumentation (functions)
  "Remove Edebug instrumentation from FUNCTIONS.
Interactively, the user is prompted for the function to remove
instrumentation for, defaulting to all functions."
  (interactive
   (list
    (let ((functions nil))
      (mapatoms
       (lambda (symbol)
         (when (and (get symbol 'edebug)
                    (or (functionp symbol)
                        (macrop symbol))
                    (edebug--unwrap*-symbol-function
                     symbol))
           (push symbol functions)))
       obarray)
      (unless functions
        (user-error "Found no functions to remove instrumentation from"))
      (let ((name
             (completing-read
              "Remove instrumentation from (default all functions): "
              functions)))
        (if (and name
                 (not (equal name "")))
            (list (intern name))
          functions)))))
  ;; Remove instrumentation.
  (dolist (symbol functions)
    (when-let ((unwrapped
                (edebug--unwrap*-symbol-function symbol)))
      (defalias symbol unwrapped)))
  (message "Removed edebug instrumentation from %s"
           (mapconcat #'symbol-name functions ", ")))


;;; Obsolete.

(defun edebug-mark ()
  (declare (obsolete mark "28.1"))
  (mark t))

(define-obsolete-function-alias 'edebug-mark-marker #'mark-marker "28.1")
(define-obsolete-function-alias 'edebug-window-live-p #'window-live-p "28.1")

(provide 'edebug)
;;; edebug.el ends here
