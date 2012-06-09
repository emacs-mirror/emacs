;;; macroexp.el --- Additional macro-expansion support -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright (C) 2004-2012 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: lisp, compiler, macros

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file contains macro-expansions functions that are not defined in
;; the Lisp core, namely `macroexpand-all', which expands all macros in
;; a form, not just a top-level one.
;;

;;; Code:

;; Bound by the top-level `macroexpand-all', and modified to include any
;; macros defined by `defmacro'.
(defvar macroexpand-all-environment nil)

(defun macroexp--cons (car cdr original-cons)
  "Return (CAR . CDR), using ORIGINAL-CONS if possible."
  (if (and (eq car (car original-cons)) (eq cdr (cdr original-cons)))
      original-cons
    (cons car cdr)))

;; We use this special macro to iteratively process forms and share list
;; structure of the result with the input.  Doing so recursively using
;; `macroexp--cons' results in excessively deep recursion for very long
;; input forms.
(defmacro macroexp--accumulate (var+list &rest body)
  "Return a list of the results of evaluating BODY for each element of LIST.
Evaluate BODY with VAR bound to each `car' from LIST, in turn.
Return a list of the values of the final form in BODY.
The list structure of the result will share as much with LIST as
possible (for instance, when BODY just returns VAR unchanged, the
result will be eq to LIST).

\(fn (VAR LIST) BODY...)"
  (declare (indent 1))
  (let ((var (car var+list))
	(list (cadr var+list))
	(shared (make-symbol "shared"))
	(unshared (make-symbol "unshared"))
	(tail (make-symbol "tail"))
	(new-el (make-symbol "new-el")))
    `(let* ((,shared ,list)
	    (,unshared nil)
	    (,tail ,shared)
	    ,var ,new-el)
       (while (consp ,tail)
	 (setq ,var (car ,tail)
	       ,new-el (progn ,@body))
	 (unless (eq ,var ,new-el)
	   (while (not (eq ,shared ,tail))
	     (push (pop ,shared) ,unshared))
	   (setq ,shared (cdr ,shared))
	   (push ,new-el ,unshared))
	 (setq ,tail (cdr ,tail)))
       (nconc (nreverse ,unshared) ,shared))))

(defun macroexp--all-forms (forms &optional skip)
  "Return FORMS with macros expanded.  FORMS is a list of forms.
If SKIP is non-nil, then don't expand that many elements at the start of
FORMS."
  (macroexp--accumulate (form forms)
    (if (or (null skip) (zerop skip))
	(macroexp--expand-all form)
      (setq skip (1- skip))
      form)))

(defun macroexp--all-clauses (clauses &optional skip)
  "Return CLAUSES with macros expanded.
CLAUSES is a list of lists of forms; any clause that's not a list is ignored.
If SKIP is non-nil, then don't expand that many elements at the start of
each clause."
  (macroexp--accumulate (clause clauses)
    (if (listp clause)
	(macroexp--all-forms clause skip)
      clause)))

(defun macroexp--expand-all (form)
  "Expand all macros in FORM.
This is an internal version of `macroexpand-all'.
Assumes the caller has bound `macroexpand-all-environment'."
  (if (and (listp form) (eq (car form) 'backquote-list*))
      ;; Special-case `backquote-list*', as it is normally a macro that
      ;; generates exceedingly deep expansions from relatively shallow input
      ;; forms.  We just process it `in reverse' -- first we expand all the
      ;; arguments, _then_ we expand the top-level definition.
      (macroexpand (macroexp--all-forms form 1)
		   macroexpand-all-environment)
    ;; Normal form; get its expansion, and then expand arguments.
    (let ((new-form (macroexpand form macroexpand-all-environment)))
      (when (and (not (eq form new-form)) ;It was a macro call.
                 (car-safe form)
                 (symbolp (car form))
                 (get (car form) 'byte-obsolete-info)
                 (fboundp 'byte-compile-warn-obsolete))
        (byte-compile-warn-obsolete (car form)))
      (setq form new-form))
    (pcase form
      (`(cond . ,clauses)
       (macroexp--cons 'cond (macroexp--all-clauses clauses) form))
      (`(condition-case . ,(or `(,err ,body . ,handlers) dontcare))
       (macroexp--cons
        'condition-case
        (macroexp--cons err
                    (macroexp--cons (macroexp--expand-all body)
                                (macroexp--all-clauses handlers 1)
                                (cddr form))
                    (cdr form))
        form))
      (`(,(or `defvar `defconst) . ,_) (macroexp--all-forms form 2))
      (`(function ,(and f `(lambda . ,_)))
       (macroexp--cons 'function
                   (macroexp--cons (macroexp--all-forms f 2)
                               nil
                               (cdr form))
                   form))
      (`(,(or `function `quote) . ,_) form)
      (`(,(and fun (or `let `let*)) . ,(or `(,bindings . ,body) dontcare))
       (macroexp--cons fun
                   (macroexp--cons (macroexp--all-clauses bindings 1)
                               (macroexp--all-forms body)
                               (cdr form))
                   form))
      (`(,(and fun `(lambda . ,_)) . ,args)
       ;; Embedded lambda in function position.
       (macroexp--cons (macroexp--all-forms fun 2)
                   (macroexp--all-forms args)
                   form))
      ;; The following few cases are for normal function calls that
      ;; are known to funcall one of their arguments.  The byte
      ;; compiler has traditionally handled these functions specially
      ;; by treating a lambda expression quoted by `quote' as if it
      ;; were quoted by `function'.  We make the same transformation
      ;; here, so that any code that cares about the difference will
      ;; see the same transformation.
      ;; First arg is a function:
      (`(,(and fun (or `funcall `apply `mapcar `mapatoms `mapconcat `mapc))
         ',(and f `(lambda . ,_)) . ,args)
       (byte-compile-log-warning
        (format "%s quoted with ' rather than with #'"
                (list 'lambda (nth 1 f) '...))
        t)
       ;; We don't use `macroexp--cons' since there's clearly a change.
       (cons fun
             (cons (macroexp--expand-all (list 'function f))
                   (macroexp--all-forms args))))
      ;; Second arg is a function:
      (`(,(and fun (or `sort)) ,arg1 ',(and f `(lambda . ,_)) . ,args)
       (byte-compile-log-warning
        (format "%s quoted with ' rather than with #'"
                (list 'lambda (nth 1 f) '...))
        t)
       ;; We don't use `macroexp--cons' since there's clearly a change.
       (cons fun
             (cons (macroexp--expand-all arg1)
                   (cons (macroexp--expand-all
                          (list 'function f))
                         (macroexp--all-forms args)))))
      (`(,func . ,_)
       ;; Macro expand compiler macros.  This cannot be delayed to
       ;; byte-optimize-form because the output of the compiler-macro can
       ;; use macros.
       (let ((handler nil))
         (while (and (symbolp func)
                     (not (setq handler (get func 'compiler-macro)))
                     (fboundp func))
           ;; Follow the sequence of aliases.
           (setq func (symbol-function func)))
         (if (null handler)
             ;; No compiler macro.  We just expand each argument (for
             ;; setq/setq-default this works alright because the variable names
             ;; are symbols).
             (macroexp--all-forms form 1)
           ;; If the handler is not loaded yet, try (auto)loading the
           ;; function itself, which may in turn load the handler.
           (when (and (not (functionp handler))
                      (fboundp func) (eq (car-safe (symbol-function func))
                                         'autoload))
             (ignore-errors
               (load (nth 1 (symbol-function func))
                     'noerror 'nomsg)))
           (let ((newform (condition-case err
                              (apply handler form (cdr form))
                            (error (message "Compiler-macro error: %S" err)
                                   form))))
             (if (eq form newform)
                 ;; The compiler macro did not find anything to do.
                 (if (equal form (setq newform (macroexp--all-forms form 1)))
                     form
                   ;; Maybe after processing the args, some new opportunities
                   ;; appeared, so let's try the compiler macro again.
                   (setq form (condition-case err
                                  (apply handler newform (cdr newform))
                                (error (message "Compiler-macro error: %S" err)
                                       newform)))
                   (if (eq newform form)
                       newform
                     (macroexp--expand-all newform)))
               (macroexp--expand-all newform))))))

      (t form))))

;;;###autoload
(defun macroexpand-all (form &optional environment)
  "Return result of expanding macros at all levels in FORM.
If no macros are expanded, FORM is returned unchanged.
The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation."
  (let ((macroexpand-all-environment environment))
    (macroexp--expand-all form)))

;;; Handy functions to use in macros.

(defun macroexp-progn (exps)
  "Return an expression equivalent to `(progn ,@EXPS)."
  (if (cdr exps) `(progn ,@exps) (car exps)))

(defun macroexp-unprogn (exp)
  "Turn EXP into a list of expressions to execute in sequence."
  (if (eq (car-safe exp) 'progn) (cdr exp) (list exp)))

(defun macroexp-let* (bindings exp)
  "Return an expression equivalent to `(let* ,bindings ,exp)."
  (cond
   ((null bindings) exp)
   ((eq 'let* (car-safe exp)) `(let* (,@bindings ,@(cadr exp)) ,@(cddr exp)))
   (t `(let* ,bindings ,exp))))

(defun macroexp-if (test then else)
  "Return an expression equivalent to `(if ,test ,then ,else)."
  (cond
   ((eq (car-safe else) 'if)
    (if (equal test (nth 1 else))
        ;; Doing a test a second time: get rid of the redundancy.
        `(if ,test ,then ,@(nthcdr 3 else))
      `(cond (,test ,then)
             (,(nth 1 else) ,(nth 2 else))
             (t ,@(nthcdr 3 else)))))
   ((eq (car-safe else) 'cond)
    `(cond (,test ,then)
           ;; Doing a test a second time: get rid of the redundancy, as above.
           ,@(remove (assoc test else) (cdr else))))
   ;; Invert the test if that lets us reduce the depth of the tree.
   ((memq (car-safe then) '(if cond)) (macroexp-if `(not ,test) else then))
   (t `(if ,test ,then ,else))))

(defmacro macroexp-let² (test var exp &rest exps)
  "Bind VAR to a copyable expression that returns the value of EXP.
This is like `(let ((v ,EXP)) ,EXPS) except that `v' is a new generated
symbol which EXPS can find in VAR.
TEST should be the name of a predicate on EXP checking whether the `let' can
be skipped; if nil, as is usual, `macroexp-const-p' is used."
  (declare (indent 3) (debug (sexp form sexp body)))
  (let ((bodysym (make-symbol "body"))
        (expsym (make-symbol "exp")))
    `(let* ((,expsym ,exp)
            (,var (if (,(or test #'macroexp-const-p) ,expsym)
                      ,expsym (make-symbol "x")))
            (,bodysym ,(macroexp-progn exps)))
       (if (eq ,var ,expsym) ,bodysym
         (macroexp-let* (list (list ,var ,expsym))
                        ,bodysym)))))

(defsubst macroexp--const-symbol-p (symbol &optional any-value)
  "Non-nil if SYMBOL is constant.
If ANY-VALUE is nil, only return non-nil if the value of the symbol is the
symbol itself."
  (or (memq symbol '(nil t))
      (keywordp symbol)
      (if any-value
	  (or (memq symbol byte-compile-const-variables)
	      ;; FIXME: We should provide a less intrusive way to find out
	      ;; if a variable is "constant".
	      (and (boundp symbol)
		   (condition-case nil
		       (progn (set symbol (symbol-value symbol)) nil)
		     (setting-constant t)))))))

(defun macroexp-const-p (exp)
  "Return non-nil if EXP will always evaluate to the same value."
  (cond ((consp exp) (or (eq (car exp) 'quote)
                         (and (eq (car exp) 'function)
                              (symbolp (cadr exp)))))
        ;; It would sometimes make sense to pass `any-value', but it's not
        ;; always safe since a "constant" variable may not actually always have
        ;; the same value.
        ((symbolp exp) (macroexp--const-symbol-p exp))
        (t t)))

(defun macroexp-copyable-p (exp)
  "Return non-nil if EXP can be copied without extra cost."
  (or (symbolp exp) (macroexp-const-p exp)))

(provide 'macroexp)

;;; macroexp.el ends here
