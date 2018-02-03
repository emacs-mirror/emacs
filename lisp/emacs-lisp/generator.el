;;; generator.el --- generators  -*- lexical-binding: t -*-

;;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

;; Author: Daniel Colascione <dancol@dancol.org>
;; Keywords: extensions, elisp
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

;; This package implements generators for Emacs Lisp through a
;; continuation-passing transformation.  It provides essentially the
;; same generator API and iterator facilities that Python and
;; JavaScript ES6 provide.
;;
;; `iter-lambda' and `iter-defun' work like `lambda' and `defun',
;; except that they evaluate to or define, respectively, generator
;; functions.  These functions, when called, return an iterator.
;; An iterator is an opaque object that generates a sequence of
;; values.  Callers use `iter-next' to retrieve the next value from
;; the sequence; when the sequence is exhausted, `iter-next' will
;; raise the `iter-end-of-sequence' condition.
;;
;; Generator functions are written like normal functions, except that
;; they can invoke `iter-yield' to suspend themselves and return a
;; value to callers; this value becomes the return value of
;; `iter-next'.  On the next call to `iter-next', execution of the
;; generator function resumes where it left off.  When a generator
;; function returns normally, the `iter-next' raises
;; `iter-end-of-sequence' with the value the function returned.
;;
;; `iter-yield-from' yields all the values from another iterator; it
;; then evaluates to the value the sub-iterator returned normally.
;; This facility is useful for functional composition of generators
;; and for implementing coroutines.
;;
;; `iter-yield' is illegal inside the UNWINDFORMS of an
;; `unwind-protect' for various sordid internal reasons documented in
;; the code.
;;
;; N.B. Each call to a generator function generates a *new* iterator,
;; and each iterator maintains its own internal state.
;;
;; This raw form of iteration is general, but a bit awkward to use, so
;; this library also provides some convenience functions:
;;
;; `iter-do' is like `cl-do', except that instead of walking a list,
;; it walks an iterator.  `cl-loop' is also extended with a new
;; keyword, `iter-by', that iterates over an iterator.
;;

;;; Implementation:

;;
;; The internal cps transformation code uses the cps- namespace.
;; Iteration functions use the `iter-' namespace.  Generator functions
;; are somewhat less efficient than conventional elisp routines,
;; although we try to avoid CPS transformation on forms that do not
;; invoke `iter-yield'.
;;

;;; Code:

(require 'cl-lib)

(defvar cps--bindings nil)
(defvar cps--states nil)
(defvar cps--value-symbol nil)
(defvar cps--state-symbol nil)
(defvar cps--cleanup-table-symbol nil)
(defvar cps--cleanup-function nil)

(defmacro cps--gensym (fmt &rest args)
  `(gensym (format ,fmt ,@args)))

(defvar cps--dynamic-wrappers '(identity)
  "List of transformer functions to apply to atomic forms we
evaluate in CPS context.")

(defconst cps-standard-special-forms
  '(setq setq-default throw interactive)
  "List of special forms that we treat just like ordinary
  function applications." )

(defun cps--trace-funcall (func &rest args)
  (message "%S: args=%S" func args)
  (let ((result (apply func args)))
    (message "%S: result=%S" func result)
    result))

(defun cps--trace (fmt &rest args)
  (princ (apply #'format (concat fmt "\n") args)))

(defun cps--special-form-p (definition)
  "Non-nil if and only if DEFINITION is a special form."
  ;; Copied from ad-special-form-p
  (if (and (symbolp definition) (fboundp definition))
      (setf definition (indirect-function definition)))
  (and (subrp definition) (eq (cdr (subr-arity definition)) 'unevalled)))

(defmacro cps--define-unsupported (function)
  `(defun ,(intern (format "cps--transform-%s" function))
       (error "%s not supported in generators" ,function)))

(defmacro cps--with-value-wrapper (wrapper &rest body)
  "Continue generating CPS code with an atomic-form wrapper
to the current stack of such wrappers.  WRAPPER is a function that
takes a form and returns a wrapped form.

Whenever we generate an atomic form (i.e., a form that can't
iter-yield), we first (before actually inserting that form in our
generated code) pass that form through all the transformer
functions.  We use this facility to wrap forms that can transfer
control flow non-locally in goo that diverts this control flow to
the CPS state machinery.
"
  (declare (indent 1))
  `(let ((cps--dynamic-wrappers
          (cons
           ,wrapper
           cps--dynamic-wrappers)))
     ,@body))

(defun cps--make-dynamic-binding-wrapper (dynamic-var static-var)
  (cl-assert lexical-binding)
  (lambda (form)
    `(let ((,dynamic-var ,static-var))
       (unwind-protect ; Update the static shadow after evaluation is done
            ,form
         (setf ,static-var ,dynamic-var)))))

(defmacro cps--with-dynamic-binding (dynamic-var static-var &rest body)
  "Evaluate BODY such that generated atomic evaluations run with
DYNAMIC-VAR bound to STATIC-VAR."
  (declare (indent 2))
  `(cps--with-value-wrapper
       (cps--make-dynamic-binding-wrapper ,dynamic-var ,static-var)
     ,@body))

(defun cps--add-state (kind body)
  "Create a new CPS state with body BODY and return the state's name."
  (declare (indent 1))
  (let* ((state (cps--gensym "cps-state-%s-" kind)))
    (push (list state body cps--cleanup-function) cps--states)
    (push state cps--bindings)
    state))

(defun cps--add-binding (original-name)
  (car (push (cps--gensym (format "cps-binding-%s-" original-name))
             cps--bindings)))

(defun cps--find-special-form-handler (form)
  (let* ((handler-name (format "cps--transform-%s" (car-safe form)))
         (handler (intern-soft handler-name)))
    (and (fboundp handler) handler)))

(defvar cps-inhibit-atomic-optimization nil
  "When t, always rewrite forms into cps even when they
don't yield.")

(defvar cps--yield-seen)

(defun cps--atomic-p (form)
  "Return whether the given form never yields."

  (and (not cps-inhibit-atomic-optimization)
       (let* ((cps--yield-seen))
         (ignore (macroexpand-all
                  `(cl-macrolet ((cps-internal-yield
                                     (_val)
                                   (setf cps--yield-seen t)))
                     ,form)
                  macroexpand-all-environment))
         (not cps--yield-seen))))

(defun cps--make-atomic-state (form next-state)
  (let ((tform `(prog1 ,form (setf ,cps--state-symbol ,next-state))))
    (cl-loop for wrapper in cps--dynamic-wrappers
       do (setf tform (funcall wrapper tform)))
    ;; Bind cps--cleanup-function to nil here because the wrapper
    ;; function mechanism is responsible for cleanup here, not the
    ;; generic cleanup mechanism.  If we didn't make this binding,
    ;; we'd run cleanup handlers twice on anything that made it out
    ;; to toplevel.
    (let ((cps--cleanup-function nil))
      (cps--add-state "atom"
        `(setf ,cps--value-symbol ,tform)))))

(defun cps--transform-1 (form next-state)
  (pcase form

    ;; If we're looking at an "atomic" form (i.e., one that does not
    ;; iter-yield), just evaluate the form as a whole instead of rewriting
    ;; it into CPS.

    ((guard (cps--atomic-p form))
     (cps--make-atomic-state form next-state))

    ;; Process `and'.

    (`(and)                             ; (and) -> t
      (cps--transform-1 t next-state))
    (`(and ,condition)                  ; (and CONDITION) -> CONDITION
      (cps--transform-1 condition next-state))
    (`(and ,condition . ,rest)
      ;; Evaluate CONDITION; if it's true, go on to evaluate the rest
      ;; of the `and'.
      (cps--transform-1
       condition
       (cps--add-state "and"
         `(setf ,cps--state-symbol
                (if ,cps--value-symbol
                    ,(cps--transform-1 `(and ,@rest)
                                       next-state)
                  ,next-state)))))

    ;; Process `catch'.

    (`(catch ,tag . ,body)
      (let ((tag-binding (cps--add-binding "catch-tag")))
        (cps--transform-1 tag
                          (cps--add-state "cps-update-tag"
                            `(setf ,tag-binding ,cps--value-symbol
                                   ,cps--state-symbol
                                   ,(cps--with-value-wrapper
                                     (cps--make-catch-wrapper
                                      tag-binding next-state)
                                     (cps--transform-1 `(progn ,@body)
                                                       next-state)))))))

    ;; Process `cond': transform into `if' or `or' depending on the
    ;; precise kind of the condition we're looking at.

    (`(cond)                            ; (cond) -> nil
      (cps--transform-1 nil next-state))
    (`(cond (,condition) . ,rest)
      (cps--transform-1 `(or ,condition (cond ,@rest))
                        next-state))
    (`(cond (,condition . ,body) . ,rest)
      (cps--transform-1 `(if ,condition
                             (progn ,@body)
                           (cond ,@rest))
                        next-state))

    ;; Process `condition-case': do the heavy lifting in a helper
    ;; function.

    (`(condition-case ,var ,bodyform . ,handlers)
      (cps--with-value-wrapper
          (cps--make-condition-wrapper var next-state handlers)
        (cps--transform-1 bodyform
                          next-state)))

    ;; Process `if'.

    (`(if ,cond ,then . ,else)
      (cps--transform-1 cond
                        (cps--add-state "if"
                          `(setf ,cps--state-symbol
                                 (if ,cps--value-symbol
                                     ,(cps--transform-1 then
                                                        next-state)
                                   ,(cps--transform-1 `(progn ,@else)
                                                      next-state))))))

    ;; Process `progn' and `inline': they are identical except for the
    ;; name, which has some significance to the byte compiler.

    (`(inline) (cps--transform-1 nil next-state))
    (`(inline ,form) (cps--transform-1 form next-state))
    (`(inline ,form . ,rest)
      (cps--transform-1 form
                        (cps--transform-1 `(inline ,@rest)
                                          next-state)))

    (`(progn) (cps--transform-1 nil next-state))
    (`(progn ,form) (cps--transform-1 form next-state))
    (`(progn ,form . ,rest)
      (cps--transform-1 form
                        (cps--transform-1 `(progn ,@rest)
                                          next-state)))

    ;; Process `let' in a helper function that transforms it into a
    ;; let* with temporaries.

    (`(let ,bindings . ,body)
      (let* ((bindings (cl-loop for binding in bindings
                          collect (if (symbolp binding)
                                      (list binding nil)
                                    binding)))
             (temps (cl-loop for (var _value-form) in bindings
                       collect (cps--add-binding var))))
        (cps--transform-1
         `(let* ,(append
                  (cl-loop for (_var value-form) in bindings
                     for temp in temps
                     collect (list temp value-form))
                  (cl-loop for (var _binding) in bindings
                     for temp in temps
                     collect (list var temp)))
            ,@body)
         next-state)))

    ;; Process `let*' binding: process one binding at a time.  Flatten
    ;; lexical bindings.

    (`(let* () . ,body)
      (cps--transform-1 `(progn ,@body) next-state))

    (`(let* (,binding . ,more-bindings) . ,body)
      (let* ((var (if (symbolp binding) binding (car binding)))
             (value-form (car (cdr-safe binding)))
             (new-var (cps--add-binding var)))

        (cps--transform-1
         value-form
         (cps--add-state "let*"
           `(setf ,new-var ,cps--value-symbol
                  ,cps--state-symbol
                  ,(if (or (not lexical-binding) (special-variable-p var))
                       (cps--with-dynamic-binding var new-var
                         (cps--transform-1
                          `(let* ,more-bindings ,@body)
                          next-state))
                       (cps--transform-1
                        (cps--replace-variable-references
                         var new-var
                         `(let* ,more-bindings ,@body))
                        next-state)))))))

    ;; Process `or'.

    (`(or) (cps--transform-1 nil next-state))
    (`(or ,condition) (cps--transform-1 condition next-state))
    (`(or ,condition . ,rest)
      (cps--transform-1
       condition
       (cps--add-state "or"
         `(setf ,cps--state-symbol
                (if ,cps--value-symbol
                    ,next-state
                  ,(cps--transform-1
                    `(or ,@rest) next-state))))))

    ;; Process `prog1'.

    (`(prog1 ,first) (cps--transform-1 first next-state))
    (`(prog1 ,first . ,body)
      (cps--transform-1
       first
       (let ((temp-var-symbol (cps--add-binding "prog1-temp")))
         (cps--add-state "prog1"
           `(setf ,temp-var-symbol
                  ,cps--value-symbol
                  ,cps--state-symbol
                  ,(cps--transform-1
                    `(progn ,@body)
                    (cps--add-state "prog1inner"
                      `(setf ,cps--value-symbol ,temp-var-symbol
                             ,cps--state-symbol ,next-state))))))))

    ;; Process `prog2'.

    (`(prog2 ,form1 ,form2 . ,body)
      (cps--transform-1
       `(progn ,form1 (prog1 ,form2 ,@body))
       next-state))

    ;; Process `unwind-protect': If we're inside an unwind-protect, we
    ;; have a block of code UNWINDFORMS which we would like to run
    ;; whenever control flows away from the main piece of code,
    ;; BODYFORM.  We deal with the local control flow case by
    ;; generating BODYFORM such that it yields to a continuation that
    ;; executes UNWINDFORMS, which then yields to NEXT-STATE.
    ;;
    ;; Non-local control flow is trickier: we need to ensure that we
    ;; execute UNWINDFORMS even when control bypasses our normal
    ;; continuation.  To make this guarantee, we wrap every external
    ;; application (i.e., every piece of elisp that can transfer
    ;; control non-locally) in an unwind-protect that runs UNWINDFORMS
    ;; before allowing the non-local control transfer to proceed.
    ;;
    ;; Unfortunately, because elisp lacks a mechanism for generically
    ;; capturing the reason for an arbitrary non-local control
    ;; transfer and restarting the transfer at a later point, we
    ;; cannot reify non-local transfers and cannot allow
    ;; continuation-passing code inside UNWINDFORMS.

    (`(unwind-protect ,bodyform . ,unwindforms)
      ;; Signal the evaluator-generator that it needs to generate code
      ;; to handle cleanup forms.
      (unless cps--cleanup-table-symbol
        (setf cps--cleanup-table-symbol (cps--gensym "cps-cleanup-table-")))
      (let* ((unwind-state
              (cps--add-state
                  "unwind"
                ;; N.B. It's safe to just substitute unwindforms by
                ;; sexp-splicing: we've already replaced all variable
                ;; references inside it with lifted equivalents.
                `(progn
                   ,@unwindforms
                   (setf ,cps--state-symbol ,next-state))))
             (old-cleanup cps--cleanup-function)
             (cps--cleanup-function
              (let ((cps--cleanup-function nil))
                (cps--add-state "cleanup"
                  `(progn
                     ,(when old-cleanup `(funcall ,old-cleanup))
                     ,@unwindforms)))))
        (cps--with-value-wrapper
            (cps--make-unwind-wrapper unwindforms)
          (cps--transform-1 bodyform unwind-state))))

    ;; Process `while'.

    (`(while ,test . ,body)
      ;; Open-code state addition instead of using cps--add-state: we
      ;; need our states to be self-referential. (That's what makes the
      ;; state a loop.)
      (let* ((loop-state
                (cps--gensym "cps-state-while-"))
             (eval-loop-condition-state
              (cps--transform-1 test loop-state))
             (loop-state-body
                `(progn
                   (setf ,cps--state-symbol
                         (if ,cps--value-symbol
                             ,(cps--transform-1
                               `(progn ,@body)
                               eval-loop-condition-state)
                           ,next-state)))))
        (push (list loop-state loop-state-body cps--cleanup-function)
              cps--states)
        (push loop-state cps--bindings)
        eval-loop-condition-state))

    ;; Process various kinds of `quote'.

    (`(quote ,arg) (cps--add-state "quote"
                     `(setf ,cps--value-symbol (quote ,arg)
                            ,cps--state-symbol ,next-state)))
    (`(function ,arg) (cps--add-state "function"
                        `(setf ,cps--value-symbol (function ,arg)
                               ,cps--state-symbol ,next-state)))

    ;; Deal with `iter-yield'.

    (`(cps-internal-yield ,value)
      (cps--transform-1
       value
       (cps--add-state "iter-yield"
         `(progn
            (setf ,cps--state-symbol
                  ,(if cps--cleanup-function
                       (cps--add-state "after-yield"
                         `(setf ,cps--state-symbol ,next-state))
                       next-state))
            (throw 'cps--yield ,cps--value-symbol)))))

    ;; Catch any unhandled special forms.

    ((and `(,name . ,_)
          (guard (cps--special-form-p name))
          (guard (not (memq name cps-standard-special-forms))))
     name                               ; Shut up byte compiler
     (error "special form %S incorrect or not supported" form))

    ;; Process regular function applications with nontrivial
    ;; parameters, converting them to applications of trivial
    ;; let-bound parameters.

    ((and `(,function . ,arguments)
          (guard (not (cl-loop for argument in arguments
                         always (atom argument)))))
     (let ((argument-symbols
            (cl-loop for argument in arguments
               collect (if (atom argument)
                           argument
                         (cps--gensym "cps-argument-")))))

       (cps--transform-1
        `(let* ,(cl-loop for argument in arguments
                   for argument-symbol in argument-symbols
                   unless (eq argument argument-symbol)
                   collect (list argument-symbol argument))
           ,(cons function argument-symbols))
        next-state)))

    ;; Process everything else by just evaluating the form normally.
    (_ (cps--make-atomic-state form next-state))))

(defun cps--make-catch-wrapper (tag-binding next-state)
  (lambda (form)
    (let ((normal-exit-symbol
           (cps--gensym "cps-normal-exit-from-catch-")))
      `(let (,normal-exit-symbol)
         (prog1
             (catch ,tag-binding
               (prog1
                   ,form
                 (setf ,normal-exit-symbol t)))
           (unless ,normal-exit-symbol
             (setf ,cps--state-symbol ,next-state)))))))

(defun cps--make-condition-wrapper (var next-state handlers)
  ;; Each handler is both one of the transformers with which we wrap
  ;; evaluated atomic forms and a state to which we jump when we
  ;; encounter the given error.

  (let* ((error-symbol (cps--add-binding "condition-case-error"))
         (lexical-error-symbol (cps--gensym "cps-lexical-error-"))
         (processed-handlers
          (cl-loop for (condition . body) in handlers
             collect (cons condition
                           (cps--transform-1
                            (cps--replace-variable-references
                             var error-symbol
                             `(progn ,@body))
                            next-state)))))

    (lambda (form)
      `(condition-case
           ,lexical-error-symbol
           ,form
         ,@(cl-loop
              for (condition . error-state) in processed-handlers
              collect
                `(,condition
                  (setf ,error-symbol
                        ,lexical-error-symbol
                        ,cps--state-symbol
                        ,error-state)))))))

(defun cps--replace-variable-references (var new-var form)
  "Replace all non-shadowed references to VAR with NEW-VAR in FORM.
This routine does not modify FORM. Instead, it returns a
modified copy."
  (macroexpand-all
   `(cl-symbol-macrolet ((,var ,new-var)) ,form)
   macroexpand-all-environment))

(defun cps--make-unwind-wrapper (unwind-forms)
  (cl-assert lexical-binding)
  (lambda (form)
    (let ((normal-exit-symbol
           (cps--gensym "cps-normal-exit-from-unwind-")))
      `(let (,normal-exit-symbol)
         (unwind-protect
              (prog1
                  ,form
                (setf ,normal-exit-symbol t))
           (unless ,normal-exit-symbol
             ,@unwind-forms))))))

(put 'iter-end-of-sequence 'error-conditions '(iter-end-of-sequence))
(put 'iter-end-of-sequence 'error-message "iteration terminated")

(defun cps--make-close-iterator-form (terminal-state)
  (if cps--cleanup-table-symbol
      `(let ((cleanup (cdr (assq ,cps--state-symbol ,cps--cleanup-table-symbol))))
         (setf ,cps--state-symbol ,terminal-state
               ,cps--value-symbol nil)
         (when cleanup (funcall cleanup)))
    `(setf ,cps--state-symbol ,terminal-state
           ,cps--value-symbol nil)))

(defun cps-generate-evaluator (body)
  (let* (cps--states
         cps--bindings
         cps--cleanup-function
         (cps--value-symbol (cps--gensym "cps-current-value-"))
         (cps--state-symbol (cps--gensym "cps-current-state-"))
         ;; We make *cps-cleanup-table-symbol** non-nil when we notice
         ;; that we have cleanup processing to perform.
         (cps--cleanup-table-symbol nil)
         (terminal-state (cps--add-state "terminal"
                           `(signal 'iter-end-of-sequence
                                    ,cps--value-symbol)))
         (initial-state (cps--transform-1
                         (macroexpand-all
                          `(cl-macrolet
                               ((iter-yield (value)
                                  `(cps-internal-yield ,value)))
                             ,@body)
                          macroexpand-all-environment)
                         terminal-state))
         (finalizer-symbol
          (when cps--cleanup-table-symbol
            (when cps--cleanup-table-symbol
              (cps--gensym "cps-iterator-finalizer-")))))
    `(let ,(append (list cps--state-symbol cps--value-symbol)
                   (when cps--cleanup-table-symbol
                     (list cps--cleanup-table-symbol))
                   (when finalizer-symbol
                     (list finalizer-symbol))
                   (nreverse cps--bindings))
       ;; Order state list so that cleanup states are always defined
       ;; before they're referenced.
       ,@(cl-loop for (state body cleanup) in (nreverse cps--states)
            collect `(setf ,state (lambda () ,body))
            when cleanup
            do (cl-assert cps--cleanup-table-symbol)
            and collect `(push (cons ,state ,cleanup) ,cps--cleanup-table-symbol))
       (setf ,cps--state-symbol ,initial-state)

       (let ((iterator
              (lambda (op value)
                (cond
                  ,@(when finalizer-symbol
                          `(((eq op :stash-finalizer)
                             (setf ,finalizer-symbol value))
                            ((eq op :get-finalizer)
                             ,finalizer-symbol)))
                  ((eq op :close)
                   ,(cps--make-close-iterator-form terminal-state))
                  ((eq op :next)
                   (setf ,cps--value-symbol value)
                   (let ((yielded nil))
                     (unwind-protect
                          (prog1
                              (catch 'cps--yield
                                (while t
                                  (funcall ,cps--state-symbol)))
                            (setf yielded t))
                       (unless yielded
                         ;; If we're exiting non-locally (error, quit,
                         ;; etc.)  close the iterator.
                         ,(cps--make-close-iterator-form terminal-state)))))
                  (t (error "unknown iterator operation %S" op))))))
         ,(when finalizer-symbol
                `(funcall iterator
                          :stash-finalizer
                          (make-finalizer
                           (lambda ()
                             (iter-close iterator)))))
         iterator))))

(defun iter-yield (value)
  "When used inside a generator, yield control to caller.
The caller of `iter-next' receives VALUE, and the next call to
`iter-next' resumes execution at the previous
`iter-yield' point."
  (identity value)
  (error "`iter-yield' used outside a generator"))

(defmacro iter-yield-from (value)
  "When used inside a generator function, delegate to a sub-iterator.
The values that the sub-iterator yields are passed directly to
the caller, and values supplied to `iter-next' are sent to the
sub-iterator.  `iter-yield-from' evaluates to the value that the
sub-iterator function returns via `iter-end-of-sequence'."
  (let ((errsym (cps--gensym "yield-from-result"))
        (valsym (cps--gensym "yield-from-value")))
    `(let ((,valsym ,value))
       (unwind-protect
            (condition-case ,errsym
                (let ((vs nil))
                  (while t
                    (setf vs (iter-yield (iter-next ,valsym vs)))))
              (iter-end-of-sequence (cdr ,errsym)))
         (iter-close ,valsym)))))

(defmacro iter-defun (name arglist &rest body)
  "Creates a generator NAME.
When called as a function, NAME returns an iterator value that
encapsulates the state of a computation that produces a sequence
of values.  Callers can retrieve each value using `iter-next'."
  (declare (indent defun)
           (debug (&define name lambda-list lambda-doc def-body))
           (doc-string 3))
  (cl-assert lexical-binding)
  (let* ((parsed-body (macroexp-parse-body body))
         (declarations (car parsed-body))
         (exps (cdr parsed-body)))
    `(defun ,name ,arglist
       ,@declarations
       ,(cps-generate-evaluator exps))))

(defmacro iter-lambda (arglist &rest body)
  "Return a lambda generator.
`iter-lambda' is to `iter-defun' as `lambda' is to `defun'."
  (declare (indent defun)
           (debug (&define lambda-list lambda-doc def-body)))
  (cl-assert lexical-binding)
  `(lambda ,arglist
     ,(cps-generate-evaluator body)))

(defun iter-next (iterator &optional yield-result)
  "Extract a value from an iterator.
YIELD-RESULT becomes the return value of `iter-yield' in the
context of the generator.

This routine raises the `iter-end-of-sequence' condition if the
iterator cannot supply more values."
  (funcall iterator :next yield-result))

(defun iter-close (iterator)
  "Terminate an iterator early.
Run any unwind-protect handlers in scope at the point  ITERATOR
is blocked."
  (funcall iterator :close nil))

(cl-defmacro iter-do ((var iterator) &rest body)
  "Loop over values from an iterator.
Evaluate BODY with VAR bound to each value from ITERATOR.
Return the value with which ITERATOR finished iteration."
  (declare (indent 1)
           (debug ((symbolp form) body)))
  (let ((done-symbol (cps--gensym "iter-do-iterator-done"))
        (condition-symbol (cps--gensym "iter-do-condition"))
        (it-symbol (cps--gensym "iter-do-iterator"))
        (result-symbol (cps--gensym "iter-do-result")))
    `(let (,var
           ,result-symbol
           (,done-symbol nil)
           (,it-symbol ,iterator))
       (while (not ,done-symbol)
         (condition-case ,condition-symbol
             (setf ,var (iter-next ,it-symbol))
           (iter-end-of-sequence
            (setf ,result-symbol (cdr ,condition-symbol))
            (setf ,done-symbol t)))
         (unless ,done-symbol ,@body))
       ,result-symbol)))

(defvar cl--loop-args)

(defmacro cps--advance-for (conscell)
  ;; See cps--handle-loop-for
  `(condition-case nil
       (progn
         (setcar ,conscell (iter-next (cdr ,conscell)))
         ,conscell)
     (iter-end-of-sequence
      nil)))

(defmacro cps--initialize-for (iterator)
  ;; See cps--handle-loop-for
  (let ((cs (cps--gensym "cps--loop-temp")))
    `(let ((,cs (cons nil ,iterator)))
       (cps--advance-for ,cs))))

(defun cps--handle-loop-for (var)
  "Support `iter-by' in `loop'.  "
  ;; N.B. While the cl-loop-for-handler is a documented interface,
  ;; there's no documented way for cl-loop-for-handler callbacks to do
  ;; anything useful!  Additionally, cl-loop currently lexbinds useful
  ;; internal variables, so our only option is to modify
  ;; cl--loop-args.  If we substitute a general-purpose for-clause for
  ;; our iterating clause, however, we can't preserve the
  ;; parallel-versus-sequential `loop' semantics for for clauses ---
  ;; we need a terminating condition as well, which requires us to use
  ;; while, and inserting a while would break and-sequencing.
  ;;
  ;; To work around this problem, we actually use the "for var in LIST
  ;; by FUNCTION" syntax, creating a new fake list each time through
  ;; the loop, this "list" being a cons cell (val . it).
  (let ((it-form (pop cl--loop-args)))
    (setf cl--loop-args
          (append
           `(for ,var
                 in (cps--initialize-for ,it-form)
                 by 'cps--advance-for)
           cl--loop-args))))

(put 'iter-by 'cl-loop-for-handler 'cps--handle-loop-for)

(eval-after-load 'elisp-mode
  (lambda ()
    (font-lock-add-keywords
     'emacs-lisp-mode
     '(("(\\(iter-defun\\)\\_>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
        (1 font-lock-keyword-face nil t)
        (2 font-lock-function-name-face nil t))
       ("(\\(iter-\\(?:next\\|lambda\\|yield\\|yield-from\\)\\)\\_>"
        (1 font-lock-keyword-face nil t))))))

(provide 'generator)

;;; generator.el ends here
