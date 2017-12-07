;;;; testcover.el -- Visual code-coverage tool  -*- lexical-binding:t -*-

;; Copyright (C) 2002-2017 Free Software Foundation, Inc.

;; Author: Jonathan Yavner <jyavner@member.fsf.org>
;; Maintainer: Jonathan Yavner <jyavner@member.fsf.org>
;; Keywords: lisp utility

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

;; * Use `testcover-start' to instrument a Lisp file for coverage testing.
;; * Use `testcover-mark-all' to add overlay "splotches" to the Lisp file's
;;   buffer to show where coverage is lacking.  Normally, a red splotch
;;   indicates the form was never evaluated; a brown splotch means it always
;;   evaluated to the same value.
;; * Use `testcover-next-mark' (bind it to a key!) to jump to the next spot
;;   that has a splotch.

;; * Basic algorithm: use `edebug' to mark up the function text with
;;   instrumentation callbacks, walk the instrumented code looking for
;;   forms which don't return or always return the same value, then use
;;   Edebug's before and after hooks to replace its code coverage with ours.
;; * To show good coverage, we want to see two values for every form, except
;;   functions that always return the same value and `defconst' variables
;;   need show only one value for good coverage.  To avoid the brown
;;   splotch, the definitions for constants and 1-valued functions must
;;   precede the references.
;; * Use the macro `1value' in your Lisp code to mark spots where the local
;;   code environment causes a function or variable to always have the same
;;   value, but the function or variable is not intrinsically 1-valued.
;; * Use the macro `noreturn' in your Lisp code to mark function calls that
;;   never return, because of the local code environment, even though the
;;   function being called is capable of returning in other cases.

;; Problems:
;; * `equal', which is used to compare the results of repeatedly executing
;;   a form, has a couple of shortcomings.  It considers strings to be the same
;;   if they only differ in properties, and it raises an error when asked to
;;   compare circular lists.
;; * Because we have only a "1value" class and no "always nil" class, we have
;;   to treat as potentially 1-valued any `and' whose last term is 1-valued,
;;   in case the last term is always nil.  Example:
;;     (and (< (point) 1000) (forward-char 10))
;;   This form always returns nil.  Similarly, `or', `if', and `cond' are
;;   treated as potentially 1-valued if all clauses are, in case those
;;   values are always nil.  Unlike truly 1-valued functions, it is not an
;;   error if these "potentially" 1-valued forms actually return differing
;;   values.

(require 'edebug)
(provide 'testcover)


;;;==========================================================================
;;; User options
;;;==========================================================================

(defgroup testcover nil
  "Code-coverage tester."
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
    buffer-disable-undo buffer-enable-undo current-global-map
    deactivate-mark delete-backward-char delete-char delete-region ding
    forward-char function* insert insert-and-inherit kill-all-local-variables
    kill-line kill-paragraph kill-region kill-sexp
    minibuffer-complete-and-exit narrow-to-region next-line push-mark
    put-text-property run-hooks set-match-data signal
    substitute-key-definition suppress-keymap undo use-local-map while widen
    yank)
  "Functions that always return the same value, according to `equal'.
No brown splotch is shown for these.  This list is quite
incomplete!  Notes: Nobody ever changes the current global map."
  :group 'testcover
  :type '(repeat symbol))

(defcustom testcover-noreturn-functions
  '(error noreturn throw signal)
  "Subset of `testcover-1value-functions' -- these never return.  We mark
them as having returned nil just before calling them."
  :group 'testcover
  :type '(repeat symbol))

(defcustom testcover-compose-functions
  '(+ - * / = append length list make-keymap make-sparse-keymap
    message propertize replace-regexp-in-string
    run-with-idle-timer set-buffer-modified-p)
  "Functions that are 1-valued if all their args are either constants or
calls to one of the `testcover-1value-functions', so if that's true then no
brown splotch is shown for these.  This list is quite incomplete!  Most
side-effect-free functions should be here."
  :group 'testcover
  :type '(repeat symbol))

(defcustom testcover-progn-functions
  '(define-key fset function goto-char mapc overlay-put progn
    save-current-buffer save-excursion save-match-data
    save-restriction save-selected-window save-window-excursion
    set set-default set-marker-insertion-type setq setq-default
    with-current-buffer with-output-to-temp-buffer with-syntax-table
    with-temp-buffer with-temp-file with-temp-message with-timeout)
  "Functions whose return value is the same as their last argument.  No
brown splotch is shown for these if the last argument is a constant or a
call to one of the `testcover-1value-functions'.  This list is probably
incomplete!"
  :group 'testcover
  :type '(repeat symbol))

(defcustom testcover-prog1-functions
  '(prog1 unwind-protect)
  "Functions whose return value is the same as their first argument.  No
brown splotch is shown for these if the first argument is a constant or a
call to one of the `testcover-1value-functions'."
  :group 'testcover
  :type '(repeat symbol))

(defcustom testcover-potentially-1value-functions
  '(add-hook and beep or remove-hook unless when)
  "Functions that are potentially 1-valued.  No brown splotch if actually
1-valued, no error if actually multi-valued."
  :group 'testcover
  :type '(repeat symbol))

(defface testcover-nohits
  '((t (:background "DeepPink2")))
  "Face for forms that had no hits during coverage test"
  :group 'testcover)

(defface testcover-1value
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
`testcover-start', whose functions should always return the same value.")

(defvar testcover-module-potentially-1value-functions nil
  "Symbols declared with defun in the last file processed by
`testcover-start', whose functions might always return the same value.")

(defvar testcover-vector nil
  "Locally bound to coverage vector for function in progress.")


;;;=========================================================================
;;; Add instrumentation to your module
;;;=========================================================================

;;;###autoload
(defun testcover-start (filename &optional byte-compile)
  "Use Edebug to instrument for coverage all macros and functions in FILENAME.
If BYTE-COMPILE is non-nil, byte compile each function after instrumenting."
  (interactive "fStart covering file: ")
  (let ((buf (find-file filename)))
    (setq edebug-form-data nil
          testcover-module-constants nil
          testcover-module-1value-functions nil
          testcover-module-potentially-1value-functions nil)
    (let ((edebug-all-defs t)
          (edebug-after-instrumentation-function #'testcover-after-instrumentation)
          (edebug-new-definition-function #'testcover-init-definition))
      (eval-buffer buf)))
  (when byte-compile
    (dolist (x (reverse edebug-form-data))
      (when (fboundp (car x))
	(message "Compiling %s..." (car x))
	(byte-compile (car x))))))

;;;###autoload
(defun testcover-this-defun ()
  "Start coverage on function under point."
  (interactive)
  (let ((edebug-all-defs t)
        (edebug-after-instrumentation-function #'testcover-after-instrumentation)
        (edebug-new-definition-function #'testcover-init-definition))
    (eval-defun nil)))

(defun testcover-end (filename)
  "Turn off instrumentation of all macros and functions in FILENAME."
  (interactive "fStop covering file: ")
  (let ((buf (find-file-noselect filename)))
    (eval-buffer buf)))


;;;=========================================================================
;;; Accumulate coverage data
;;;=========================================================================

(defun testcover-after-instrumentation (form)
  "Analyze FORM for code coverage."
  (testcover-analyze-coverage form)
  form)

(defun testcover-init-definition (sym)
  "Mark SYM as under test coverage."
  (message "Testcover: %s" sym)
  (put sym 'edebug-behavior 'testcover))

(defun testcover-enter (func _args body)
  "Begin execution of a function under coverage testing.
Bind `testcover-vector' to the code-coverage vector for FUNC and
return the result of evaluating BODY."
  (let ((testcover-vector (get func 'edebug-coverage)))
    (funcall body)))

(defun testcover-before (before-index)
  "Update code coverage before a form is evaluated.
BEFORE-INDEX is the form's index into the code-coverage vector."
  (let ((before-entry (aref testcover-vector before-index)))
    (when (eq (car-safe before-entry) 'noreturn)
      (let* ((after-index (cdr before-entry)))
        (aset testcover-vector after-index 'ok-coverage)))))

(defun testcover-after (_before-index after-index value)
  "Update code coverage with the result of a form's evaluation.
AFTER-INDEX is the form's index into the code-coverage
vector.  Return VALUE."
  (let ((old-result (aref testcover-vector after-index)))
    (cond
     ((eq 'unknown old-result)
      (aset testcover-vector after-index (testcover--copy-object value)))
     ((eq 'maybe old-result)
      (aset testcover-vector after-index 'ok-coverage))
     ((eq '1value old-result)
      (aset testcover-vector after-index
            (cons old-result (testcover--copy-object value))))
     ((and (eq (car-safe old-result) '1value)
           (not (condition-case ()
                    (equal (cdr old-result) value)
                  (circular-list t))))
      (error "Value of form expected to be constant does vary, from %s to %s"
             old-result value))
     ;; Test if a different result.
     ((not (condition-case ()
               (equal value old-result)
             (circular-list nil)))
      (aset testcover-vector after-index 'ok-coverage))))
  value)

;; Add these behaviors to Edebug.
(unless (assoc 'testcover edebug-behavior-alist)
  (push '(testcover testcover-enter testcover-before testcover-after)
        edebug-behavior-alist))

(defun testcover--copy-object (obj)
  "Make a copy of OBJ.
If OBJ is a cons cell, copy both its car and its cdr.
Contrast to `copy-tree' which does the same but fails on circular
structures, and `copy-sequence', which copies only along the
cdrs.  Copy vectors as well as conses."
  (let ((ht (make-hash-table :test 'eq)))
    (testcover--copy-object1 obj t ht)))

(defun testcover--copy-object1 (obj vecp hash-table)
  "Make a copy of OBJ, using a HASH-TABLE of objects already copied.
If OBJ is a cons cell, this recursively copies its car and
iteratively copies its cdr.  When VECP is non-nil, copy
vectors as well as conses."
  (if (and (atom obj) (or (not vecp) (not (vectorp obj))))
      obj
    (let ((copy (gethash obj hash-table nil)))
      (unless copy
        (cond
         ((consp obj)
          (let* ((rest obj) current)
	    (setq copy (cons nil nil)
                  current copy)
            (while
                (progn
                  (puthash rest current hash-table)
                  (setf (car current)
                        (testcover--copy-object1 (car rest) vecp hash-table))
                  (setq rest (cdr rest))
                  (cond
                   ((atom rest)
                    (setf (cdr current)
                          (testcover--copy-object1 rest vecp hash-table))
                    nil)
                   ((gethash rest hash-table nil)
                    (setf (cdr current) (gethash rest hash-table nil))
                    nil)
                   (t (setq current
                            (setf (cdr current) (cons nil nil)))))))))
         (t ; (and vecp (vectorp obj)) is true due to test in if above.
          (setq copy (copy-sequence obj))
          (puthash obj copy hash-table)
          (dotimes (i (length copy))
            (aset copy i
                  (testcover--copy-object1 (aref copy i) vecp hash-table))))))
      copy)))

;;;=========================================================================
;;; Display the coverage data as color splotches on your code.
;;;=========================================================================

(defun testcover-mark (def)
  "Marks one DEF (a function or macro symbol) to highlight its contained forms
that did not get completely tested during coverage tests.
  A marking with the face `testcover-nohits' (default = red) indicates that the
form was never evaluated.  A marking using the `testcover-1value' face
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
	 ov j)
    (or (and def-mark points coverage)
	(error "Missing edebug data for function %s" def))
    (when (> len 0)
      (set-buffer (marker-buffer def-mark))
      (mapc 'delete-overlay
	    (overlays-in def-mark (+ def-mark (aref points (1- len)) 1)))
      (while (> len 0)
	(setq len  (1- len)
	      data (aref coverage len))
        (when (and (not (eq data 'ok-coverage))
                   (not (memq (car-safe data)
                              '(1value maybe noreturn)))
                   (setq j (+ def-mark (aref points len))))
	  (setq ov (make-overlay (1- j) j))
	  (overlay-put ov 'face
                       (if (memq data '(unknown maybe 1value))
			   'testcover-nohits
			 'testcover-1value))))
      (set-buffer-modified-p changed))))

(defun testcover-mark-all (&optional buffer)
  "Mark all forms in BUFFER that did not get completely tested during
coverage tests.  This function creates many overlays."
  (interactive "bMark forms in buffer: ")
  (if buffer
      (switch-to-buffer buffer))
  (goto-char 1)
  (dolist (x edebug-form-data)
    (if (get (car x) 'edebug)
	(testcover-mark (car x)))))

(defun testcover-unmark-all (buffer)
  "Remove all overlays from FILENAME."
  (interactive "bUnmark forms in buffer: ")
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


;;; Coverage Analysis

;; The top level function for initializing code coverage is
;; `testcover-analyze-coverage', which recursively walks the form it is
;; passed, which should have already been instrumented by
;; edebug-read-and-maybe-wrap-form, and initializes the associated
;; code coverage vectors, which should have already been created by
;; `edebug-clear-coverage'.
;;
;; The purpose of the analysis is to identify forms which can only
;; ever return a single value.  These forms can be considered to have
;; adequate code coverage even if only executed once.  In addition,
;; forms which will never return, such as error signals, can be
;; identified and treated correctly.
;;
;; The code coverage vector entries for the beginnings of forms will
;; be changed to `ok-coverage.', except for the beginnings of forms
;; which should never return, which will be changed to
;; (noreturn . AFTER-INDEX) so that testcover-before can set the entry
;; for the end of the form just before it is executed.
;;
;; Entries for the ends of forms may be changed to `1value' if
;; analysis determines the form will only ever return a single value,
;; or `maybe' if the form could potentially only ever return a single
;; value.
;;
;; An example of a potentially 1-valued form is an `and' whose last
;; term is 1-valued, in case the last term is always nil.  Example:
;;
;; (and (< (point) 1000) (forward-char 10))
;;
;; This form always returns nil.  Similarly, `or', `if', and `cond'
;; are treated as potentially 1-valued if all clauses are, in case
;; those values are always nil.  Unlike truly 1-valued functions, it
;; is not an error if these "potentially" 1-valued forms actually
;; return differing values.

(defun testcover-analyze-coverage (form)
  "Analyze FORM and initialize coverage vectors for definitions found within.
Return 1value, maybe or nil depending on if the form is determined
to return only a single value, potentially return only a single value,
or return multiple values."
  (pcase form
    (`(edebug-enter ',sym ,_ (function (lambda nil . ,body)))
     (let ((testcover-vector (get sym 'edebug-coverage)))
       (testcover-analyze-coverage-progn body)))

    (`(edebug-after ,(and before-form
                          (or `(edebug-before ,before-id) before-id))
                    ,after-id ,wrapped-form)
     (testcover-analyze-coverage-edebug-after
      form before-form before-id after-id wrapped-form))

    (`(defconst ,sym . ,args)
     (push sym testcover-module-constants)
     (testcover-analyze-coverage-progn args)
     '1value)

    (`(defun ,name ,_ . ,doc-and-body)
     (let ((val (testcover-analyze-coverage-progn doc-and-body)))
       (cl-case val
         ((1value) (push name testcover-module-1value-functions))
         ((maybe) (push name testcover-module-potentially-1value-functions)))
       nil))

    (`(quote . ,_)
     ;; A quoted form is 1value. Edebug could have instrumented
     ;; something inside the form if an Edebug spec contained a quote.
     ;; It's also possible that the quoted form is a circular object.
     ;; To avoid infinite recursion, don't examine quoted objects.
     ;; This will cause the coverage marks on an instrumented quoted
     ;; form to look odd. See bug#25316.
     '1value)

    (`(\` ,bq-form)
     (testcover-analyze-coverage-backquote-form bq-form))

    ((or 't 'nil (pred keywordp))
     '1value)

    ((pred vectorp)
     (testcover-analyze-coverage-compose (append form nil)
                                         #'testcover-analyze-coverage))

    ((pred symbolp)
     nil)

    ((pred atom)
     '1value)

    (_
     ;; Whatever we have here, it's not wrapped, so treat it as a list of forms.
     (testcover-analyze-coverage-compose form #'testcover-analyze-coverage))))

(defun testcover-analyze-coverage-progn (forms)
  "Analyze FORMS, which should be a list of forms, for code coverage.
Analyze all the forms in FORMS and return 1value, maybe or nil
depending on the analysis of the last one.  Find the coverage
vectors referenced by `edebug-enter' forms nested within FORMS and
update them with the results of the analysis."
  (let ((result '1value))
    (while (consp forms)
      (setq result (testcover-analyze-coverage (pop forms))))
    result))

(defun testcover-analyze-coverage-edebug-after (_form before-form before-id
                                               after-id wrapped-form
                                               &optional wrapper)
  "Analyze a _FORM wrapped by `edebug-after' for code coverage.
_FORM should be either:
    (edebug-after (edebug-before BEFORE-ID) AFTER-ID WRAPPED-FORM)
or:
    (edebug-after 0 AFTER-ID WRAPPED-FORM)

where BEFORE-FORM is bound to either (edebug-before BEFORE-ID) or
0.  WRAPPER may be 1value or noreturn, and if so it forces the
form to be treated accordingly."
  (let (val)
    (unless (eql before-form 0)
      (aset testcover-vector before-id 'ok-coverage))

    (setq val (testcover-analyze-coverage-wrapped-form wrapped-form))
    (when (or (eq wrapper '1value) val)
      ;; The form is 1-valued or potentially 1-valued.
      (aset testcover-vector after-id (or val '1value)))

    (cond
     ((or (eq wrapper 'noreturn)
          (memq (car-safe wrapped-form) testcover-noreturn-functions))
      ;; This function won't return, so indicate to testcover-before that
      ;; it should record coverage.
      (aset testcover-vector before-id (cons 'noreturn after-id))
      (aset testcover-vector after-id '1value)
      (setq val '1value))

     ((eq (car-safe wrapped-form) '1value)
      ;; This function is always supposed to return the same value.
      (setq val '1value)
      (aset testcover-vector after-id '1value)))
    val))

(defun testcover-analyze-coverage-wrapped-form (form)
  "Analyze a FORM for code coverage which was wrapped by `edebug-after'.
FORM is treated as if it will be evaluated."
  (pcase form
    ((pred keywordp)
     '1value)
    ((pred symbolp)
     (when (or (memq form testcover-constants)
               (memq form testcover-module-constants))
       '1value))
    ((pred atom)
     '1value)
    (`(\` ,bq-form)
     (testcover-analyze-coverage-backquote-form bq-form))
    (`(defconst ,sym ,val . ,_)
     (push sym testcover-module-constants)
     (testcover-analyze-coverage val)
     '1value)
    (`(,(or 'dotimes 'dolist) (,_ ,expr . ,result) . ,body)
     ;; These always return RESULT if provided.
     (testcover-analyze-coverage expr)
     (testcover-analyze-coverage-progn body)
     (let ((val (testcover-analyze-coverage-progn result)))
       ;; If the third value is not present, the loop always returns nil.
       (if result val '1value)))
    (`(,(or 'let 'let*) ,bindings . ,body)
     (testcover-analyze-coverage-progn bindings)
     (testcover-analyze-coverage-progn body))
    (`(if ,test ,then-form . ,else-body)
     ;; `if' is potentially 1-valued if both THEN and ELSE clauses are.
     (testcover-analyze-coverage test)
     (let ((then (testcover-analyze-coverage then-form))
           (else (testcover-analyze-coverage else-body)))
       (and then else 'maybe)))
    (`(cond . ,clauses)
     ;; `cond' is potentially 1-valued if all clauses are.
     (when (testcover-analyze-coverage-compose clauses #'testcover-analyze-coverage-progn)
       'maybe))
    (`(condition-case ,_ ,body-form . ,handlers)
     ;; `condition-case' is potentially 1-valued if BODY-FORM is and all
     ;; HANDLERS are.
     (let ((body (testcover-analyze-coverage body-form))
           (errs (testcover-analyze-coverage-compose
                  (mapcar #'cdr handlers)
                  #'testcover-analyze-coverage-progn)))
       (and body errs 'maybe)))
    (`(apply (quote ,(and func (pred symbolp))) . ,args)
     ;; Process application of a constant symbol as 1value or noreturn
     ;; depending on the symbol.
     (let ((temp-form (cons func args)))
       (testcover-analyze-coverage-wrapped-form temp-form)))
    (`(,(and func (or '1value 'noreturn)) ,inner-form)
     ;; 1value and noreturn change how the edebug-after they wrap is handled.
     (let ((val (if (eq func '1value) '1value 'maybe)))
       (pcase inner-form
         (`(edebug-after ,(and before-form
                               (or `(edebug-before ,before-id) before-id))
                         ,after-id ,wrapped-form)
          (testcover-analyze-coverage-edebug-after inner-form before-form
                                             before-id after-id
                                             wrapped-form func))
         (_ (testcover-analyze-coverage inner-form)))
       val))
    (`(,func . ,args)
     (testcover-analyze-coverage-wrapped-application func args))))

(defun testcover-analyze-coverage-wrapped-application (func args)
  "Analyze the application of FUNC to ARGS for code coverage."
  (cond
   ((eq func 'quote) '1value)
   ((or (memq func testcover-1value-functions)
        (memq func testcover-module-1value-functions))
    ;; The function should always return the same value.
    (testcover-analyze-coverage-progn args)
    '1value)
   ((or (memq func testcover-potentially-1value-functions)
        (memq func testcover-module-potentially-1value-functions))
    ;; The function might always return the same value.
    (testcover-analyze-coverage-progn args)
    'maybe)
   ((memq func testcover-progn-functions)
    ;; The function is 1-valued if the last argument is.
    (testcover-analyze-coverage-progn args))
   ((memq func testcover-prog1-functions)
    ;; The function is 1-valued if first argument is.
    (testcover-analyze-coverage-progn (cdr args))
    (testcover-analyze-coverage (car args)))
   ((memq func testcover-compose-functions)
    ;; The function is 1-valued if all arguments are, and potentially
    ;; 1-valued if all arguments are either definitely or potentially.
    (testcover-analyze-coverage-compose args #'testcover-analyze-coverage))
   (t (testcover-analyze-coverage-progn args)
      nil)))

(defun testcover-coverage-combine (result val)
  "Combine RESULT with VAL and return the new result.
If either argument is nil, return nil, otherwise if either
argument is maybe, return maybe.  Return 1value only if both arguments
are 1value."
  (cl-case val
    (1value result)
    (maybe (and result 'maybe))
    (nil nil)))

(defun testcover-analyze-coverage-compose (forms func)
  "Analyze a list of FORMS for code coverage using FUNC.
The list is 1valued if all of its constituent elements are also 1valued."
  (let ((result '1value))
    (dolist (form forms)
      (let ((val (funcall func form)))
        (setq result (testcover-coverage-combine result val))))
    result))

(defun testcover-analyze-coverage-backquote (bq-list)
  "Analyze BQ-LIST, the body of a backquoted list, for code coverage."
  (let ((result '1value))
    (while (consp bq-list)
      (let ((form (car bq-list))
            val)
        (if (memq form (list '\, '\,@))
            ;; Correctly handle `(foo bar . ,(baz).
            (progn
              (setq val (testcover-analyze-coverage (cdr bq-list)))
              (setq bq-list nil))
          (setq val (testcover-analyze-coverage-backquote-form form))
          (setq bq-list (cdr bq-list)))
        (setq result (testcover-coverage-combine result val))))
    result))

(defun testcover-analyze-coverage-backquote-form (form)
  "Analyze a single FORM from a backquoted list for code coverage."
  (cond
   ((vectorp form) (testcover-analyze-coverage-backquote (append form nil)))
   ((atom form) '1value)
   ((memq (car form) (list '\, '\,@))
    (testcover-analyze-coverage (cadr form)))
   (t (testcover-analyze-coverage-backquote form))))

;; testcover.el ends here.
