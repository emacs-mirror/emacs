;;;; testcover.el -- Visual code-coverage tool

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Jonathan Yavner <jyavner@member.fsf.org>
;; Maintainer: Jonathan Yavner <jyavner@member.fsf.org>
;; Keywords: lisp utility

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

;; * Use `testcover-start' to instrument a Lisp file for coverage testing.
;; * Use `testcover-mark-all' to add overlay "splotches" to the Lisp file's
;;   buffer to show where coverage is lacking.  Normally, a red splotch
;;   indicates the form was never evaluated; a brown splotch means it always
;;   evaluted to the same value.
;; * Use `testcover-next-mark' (bind it to a key!) to jump to the next spot
;;   that has a splotch.

;; * Basic algorithm: use `edebug' to mark up the function text with
;;   instrumentation callbacks, then replace edebug's callbacks with ours.
;; * To show good coverage, we want to see two values for every form, except
;;   functions that always return the same value and `defconst' variables
;;   need show only value for good coverage.  To avoid the brown splotch, the
;;   definitions for constants and 1-valued functions must precede the
;;   references.
;; * Use the macro `1value' in your Lisp code to mark spots where the local
;;   code environment causes a function or variable to always have the same
;;   value, but the function or variable is not intrinsically 1-valued.
;; * Use the macro `noreturn' in your Lisp code to mark function calls that
;;   never return, because of the local code environment, even though the
;;   function being called is capable of returning in other cases.

;; Problems:
;; * To detect different values, we store the form's result in a vector and
;;   compare the next result using `equal'.  We don't copy the form's
;;   result, so if caller alters it (`setcar', etc.) we'll think the next
;;   call has the same value!  Also, equal thinks two strings are the same
;;   if they differ only in properties.
;; * Because we have only a "1value" class and no "always nil" class, we have
;;   to treat as 1-valued any `and' whose last term is 1-valued, in case the
;;   last term is always nil.  Example:
;;     (and (< (point) 1000) (forward-char 10))
;;   This form always returns nil.  Similarly, `if' and `cond' are
;;   treated as 1-valued if all clauses are, in case those values are
;;   always nil.

(require 'edebug)
(provide 'testcover)


;;;==========================================================================
;;; User options
;;;==========================================================================

(defgroup testcover nil
  "Code-coverage tester"
  :group 'lisp
  :prefix "testcover-"
  :version "21.1")

(defcustom testcover-constants
  '(nil t emacs-build-time emacs-version emacs-major-version
    emacs-minor-version)
  "Variables whose values never change.  No brown splotch is shown for
these.  This list is quite incomplete!"
  :group 'testcover
  :type '(repeat variable))

(defcustom testcover-1value-functions
  '(backward-char barf-if-buffer-read-only beginning-of-line
    buffer-disable-undo buffer-enable-undo current-global-map deactivate-mark
    delete-char delete-region ding error forward-char function* insert
    insert-and-inherit kill-all-local-variables lambda mapc narrow-to-region
    noreturn push-mark put-text-property run-hooks set-text-properties signal
    substitute-key-definition suppress-keymap throw undo use-local-map while
    widen yank)
  "Functions that always return the same value.  No brown splotch is shown
for these.  This list is quite incomplete!  Notes: Nobody ever changes the
current global map.  The macro `lambda' is self-evaluating, hence always
returns the same value (the function it defines may return varying values
when called)."
  :group 'testcover
  :type 'hook)

(defcustom testcover-noreturn-functions
  '(error noreturn throw signal)
  "Subset of `testcover-1value-functions' -- these never return.  We mark
them as having returned nil just before calling them."
  :group 'testcover
  :type 'hook)

(defcustom testcover-compose-functions
  '(+ - * / length list make-keymap make-sparse-keymap message propertize
    replace-regexp-in-string run-with-idle-timer
    set-buffer-modified-p)
  "Functions that are 1-valued if all their args are either constants or
calls to one of the `testcover-1value-functions', so if that's true then no
brown splotch is shown for these.  This list is quite incomplete!  Most
side-effect-free functions should be here."
  :group 'testcover
  :type 'hook)

(defcustom testcover-progn-functions
  '(define-key fset function goto-char or overlay-put progn save-current-buffer
    save-excursion save-match-data save-restriction save-selected-window
    save-window-excursion set set-default setq setq-default
    with-output-to-temp-buffer with-syntax-table with-temp-buffer
    with-temp-file with-temp-message with-timeout)
  "Functions whose return value is the same as their last argument.  No
brown splotch is shown for these if the last argument is a constant or a
call to one of the `testcover-1value-functions'.  This list is probably
incomplete!  Note: `or' is here in case the last argument is a function that
always returns nil."
  :group 'testcover
  :type 'hook)

(defcustom testcover-prog1-functions
  '(prog1 unwind-protect)
  "Functions whose return value is the same as their first argument.  No
brown splotch is shown for these if the first argument is a constant or a
call to one of the `testcover-1value-functions'."
  :group 'testcover
  :type 'hook)

(defface testcover-nohits-face
  '((t (:background "DeepPink2")))
  "Face for forms that had no hits during coverage test"
  :group 'testcover)

(defface testcover-1value-face
  '((t (:background "Wheat2")))
  "Face for forms that always produced the same value during coverage test"
  :group 'testcover)


;;;=========================================================================
;;; Other variables
;;;=========================================================================

(defvar testcover-module-constants nil
  "Symbols declared with defconst in the last file processed by
`testcover-start'.")

(defvar testcover-module-1value-functions nil
  "Symbols declared with defun in the last file processed by
`testcover-start', whose functions always return the same value.")

(defvar testcover-vector nil
  "Locally bound to coverage vector for function in progress.")


;;;=========================================================================
;;; Add instrumentation to your module
;;;=========================================================================

(defun testcover-start (filename &optional byte-compile)
  "Uses edebug to instrument all macros and functions in FILENAME, then
changes the instrumentation from edebug to testcover--much faster, no
problems with type-ahead or post-command-hook, etc.  If BYTE-COMPILE is
non-nil, byte-compiles each function after instrumenting."
  (interactive "f")
  (let ((buf                (find-file filename))
	(load-read-function 'testcover-read)
	(edebug-all-defs t))
    (setq edebug-form-data                       nil
	  testcover-module-constants             nil
	  testcover-module-1value-functions nil)
    (eval-buffer buf))
  (when byte-compile
    (dolist (x (reverse edebug-form-data))
      (when (fboundp (car x))
	(message "Compiling %s..." (car x))
	(byte-compile (car x))))))

;;;###autoload
(defun testcover-this-defun ()
  "Start coverage on function under point."
  (interactive)
  (let* ((edebug-all-defs t)
	 (x (symbol-function (eval-defun nil))))
    (testcover-reinstrument x)
    x))

(defun testcover-read (&optional stream)
  "Read a form using edebug, changing edebug callbacks to testcover callbacks."
  (let ((x (edebug-read stream)))
    (testcover-reinstrument x)
    x))

(defun testcover-reinstrument (form)
  "Reinstruments FORM to use testcover instead of edebug.  This function
modifies the list that FORM points to.  Result is non-nil if FORM will
always return the same value."
  (let ((fun (car-safe form))
	id)
    (cond
     ((not fun) ;Atom
      (or (not (symbolp form))
	  (memq form testcover-constants)
	  (memq form testcover-module-constants)))
     ((consp fun) ;Embedded list
      (testcover-reinstrument fun)
      (testcover-reinstrument-list (cdr form))
      nil)
     ((or (memq fun testcover-1value-functions)
	  (memq fun testcover-module-1value-functions))
      ;;Always return same value
      (testcover-reinstrument-list (cdr form))
      t)
     ((memq fun testcover-progn-functions)
      ;;1-valued if last argument is
      (testcover-reinstrument-list (cdr form)))
     ((memq fun testcover-prog1-functions)
      ;;1-valued if first argument is
      (testcover-reinstrument-list (cddr form))
      (testcover-reinstrument (cadr form)))
     ((memq fun testcover-compose-functions)
      ;;1-valued if all arguments are
      (setq id t)
      (mapc #'(lambda (x) (setq id (or (testcover-reinstrument x) id)))
	    (cdr form))
      id)
     ((eq fun 'edebug-enter)
      ;;(edebug-enter 'SYM ARGS #'(lambda nil FORMS))
      ;;  => (testcover-enter 'SYM #'(lambda nil FORMS))
      (setcar form 'testcover-enter)
      (setcdr (nthcdr 1 form) (nthcdr 3 form))
      (let ((testcover-vector (get (cadr (cadr form)) 'edebug-coverage)))
	(testcover-reinstrument-list (nthcdr 2 (cadr (nth 2 form))))))
     ((eq fun 'edebug-after)
      ;;(edebug-after (edebug-before XXX) YYY FORM)
      ;; => (testcover-after YYY FORM), mark XXX as ok-coverage
      (unless (eq (cadr form) 0)
	(aset testcover-vector (cadr (cadr form)) 'ok-coverage))
      (setq id (nth 2 form))
      (setcdr form (nthcdr 2 form))
      (cond
       ((memq (car-safe (nth 2 form)) testcover-noreturn-functions)
	;;This function won't return, so set the value in advance
	;;(edebug-after (edebug-before XXX) YYY FORM)
	;;  => (progn (edebug-after YYY nil) FORM)
	(setcar form 'progn)
	(setcar (cdr form) `(testcover-after ,id nil)))
       ((eq (car-safe (nth 2 form)) '1value)
	;;This function is always supposed to return the same value
	(setcar form 'testcover-1value))
       (t
	(setcar form 'testcover-after)))
      (when (testcover-reinstrument (nth 2 form))
	(aset testcover-vector id '1value)))
     ((eq fun 'defun)
      (if (testcover-reinstrument-list (nthcdr 3 form))
	  (push (cadr form) testcover-module-1value-functions)))
     ((eq fun 'defconst)
      ;;Define this symbol as 1-valued
      (push (cadr form) testcover-module-constants)
      (testcover-reinstrument-list (cddr form)))
     ((memq fun '(dotimes dolist))
      ;;Always returns third value from SPEC
      (testcover-reinstrument-list (cddr form))
      (setq fun (testcover-reinstrument-list (cadr form)))
      (if (nth 2 (cadr form))
	  fun
	;;No third value, always returns nil
	t))
     ((memq fun '(let let*))
      ;;Special parsing for second argument
      (mapc 'testcover-reinstrument-list (cadr form))
      (testcover-reinstrument-list (cddr form)))
     ((eq fun 'if)
      ;;1-valued if both THEN and ELSE clauses are
      (testcover-reinstrument (cadr form))
      (let ((then (testcover-reinstrument (nth 2 form)))
	    (else (testcover-reinstrument-list (nthcdr 3 form))))
	(and then else)))
     ((memq fun '(when unless and))
      ;;1-valued if last clause of BODY is
      (testcover-reinstrument-list (cdr form)))
     ((eq fun 'cond)
      ;;1-valued if all clauses are
      (testcover-reinstrument-clauses (cdr form)))
     ((eq fun 'condition-case)
      ;;1-valued if BODYFORM is and all HANDLERS are
      (let ((body (testcover-reinstrument (nth 2 form)))
	    (errs (testcover-reinstrument-clauses (mapcar #'cdr
							  (nthcdr 3 form)))))
	(and body errs)))
     ((eq fun 'quote)
      ;;Don't reinstrument what's inside!
      ;;This doesn't apply within a backquote
      t)
     ((eq fun '\`)
      ;;Quotes are not special within backquotes
      (let ((testcover-1value-functions
	     (cons 'quote testcover-1value-functions)))
	(testcover-reinstrument (cadr form))))
     ((eq fun '\,)
      ;;In commas inside backquotes, quotes are special again
      (let ((testcover-1value-functions
	     (remq 'quote testcover-1value-functions)))
	(testcover-reinstrument (cadr form))))
     ((memq fun '(1value noreturn))
      ;;Hack - pretend the arg is 1-valued here
      (if (symbolp (cadr form)) ;A pseudoconstant variable
	  t
	(if (eq (car (cadr form)) 'edebug-after)
	    (setq id (car (nth 3 (cadr form))))
	  (setq id (car (cadr form))))
	(let ((testcover-1value-functions
	       (cons id testcover-1value-functions)))
	  (testcover-reinstrument (cadr form)))))
     (t ;Some other function or weird thing
      (testcover-reinstrument-list (cdr form))
      nil))))

(defun testcover-reinstrument-list (list)
  "Reinstruments each form in LIST to use testcover instead of edebug.
This function modifies the forms in LIST.  Result is `testcover-reinstrument's
value for the last form in LIST.  If the LIST is empty, its evaluation will
always be nil, so we return t for 1-valued."
  (let ((result t))
    (while (consp list)
      (setq result (testcover-reinstrument (pop list))))
    result))

(defun testcover-reinstrument-clauses (clauselist)
  "Reinstrument each list in CLAUSELIST.
Result is t if every clause is 1-valued."
  (let ((result t))
    (mapc #'(lambda (x)
	      (setq result (and (testcover-reinstrument-list x) result)))
	  clauselist)
    result))

(defun testcover-end (buffer)
  "Turn off instrumentation of all macros and functions in FILENAME."
  (interactive "b")
  (let ((buf (find-file-noselect buffer)))
    (eval-buffer buf t)))


;;;=========================================================================
;;; Accumulate coverage data
;;;=========================================================================

(defun testcover-enter (testcover-sym testcover-fun)
  "Internal function for coverage testing.  Invokes TESTCOVER-FUN while
binding `testcover-vector' to the code-coverage vector for TESTCOVER-SYM
\(the name of the current function)."
  (let ((testcover-vector (get testcover-sym 'edebug-coverage)))
    (funcall testcover-fun)))

(defun testcover-after (idx val)
  "Internal function for coverage testing.  Returns VAL after installing it in
`testcover-vector' at offset IDX."
  (cond
   ((eq (aref testcover-vector idx) 'unknown)
    (aset testcover-vector idx val))
   ((not (equal (aref testcover-vector idx) val))
    (aset testcover-vector idx 'ok-coverage)))
  val)

(defun testcover-1value (idx val)
  "Internal function for coverage testing.  Returns VAL after installing it in
`testcover-vector' at offset IDX.  Error if FORM does not always return the
same value during coverage testing."
  (cond
   ((eq (aref testcover-vector idx) '1value)
    (aset testcover-vector idx (cons '1value val)))
   ((not (and (eq (car-safe (aref testcover-vector idx)) '1value)
	      (equal (cdr (aref testcover-vector idx)) val)))
    (error "Value of form marked with `1value' does vary.")))
  val)



;;;=========================================================================
;;; Display the coverage data as color splotches on your code.
;;;=========================================================================

(defun testcover-mark (def)
  "Marks one DEF (a function or macro symbol) to highlight its contained forms
that did not get completely tested during coverage tests.
  A marking of testcover-nohits-face (default = red) indicates that the
form was never evaluated.  A marking of testcover-1value-face
\(default = tan) indicates that the form always evaluated to the same value.
  The forms throw, error, and signal are not marked.  They do not return and
would always get a red mark.  Some forms that always return the same
value (e.g., setq of a constant), always get a tan mark that can't be
eliminated by adding more test cases."
  (let* ((data     (get def 'edebug))
	 (def-mark (car data))
	 (points   (nth 2 data))
	 (len      (length points))
	 (changed (buffer-modified-p))
	 (coverage (get def 'edebug-coverage))
	 ov j item)
    (or (and def-mark points coverage)
	(error "Missing edebug data for function %s" def))
    (when len
      (set-buffer (marker-buffer def-mark))
      (mapc 'delete-overlay
	    (overlays-in def-mark (+ def-mark (aref points (1- len)) 1)))
      (while (> len 0)
	(setq len  (1- len)
	      data (aref coverage len))
	(when (and (not (eq data 'ok-coverage))
		   (not (eq (car-safe data) '1value))
		   (setq j (+ def-mark (aref points len))))
	  (setq ov (make-overlay (1- j) j))
	  (overlay-put ov 'face
		       (if (memq data '(unknown 1value))
			   'testcover-nohits-face
			 'testcover-1value-face))))
      (set-buffer-modified-p changed))))

(defun testcover-mark-all (&optional buffer)
  "Mark all forms in BUFFER that did not get completley tested during
coverage tests.  This function creates many overlays."
  (interactive "b")
  (if buffer
      (switch-to-buffer buffer))
  (goto-char 1)
  (dolist (x edebug-form-data)
    (if (get (car x) 'edebug)
	(testcover-mark (car x)))))

(defun testcover-unmark-all (buffer)
  "Remove all overlays from FILENAME."
  (interactive "b")
  (condition-case nil
      (progn
	(set-buffer buffer)
	(mapc 'delete-overlay (overlays-in 1 (buffer-size))))
    (error nil)))  ;Ignore "No such buffer" errors

(defun testcover-next-mark ()
  "Moves point to next line in current buffer that has a splotch."
  (interactive)
  (goto-char (next-overlay-change (point)))
  (end-of-line))

;;; arch-tag: 72324a4a-4a2e-4142-9249-cc56d6757588
;; testcover.el ends here.
