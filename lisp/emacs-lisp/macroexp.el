;;; macroexp.el --- Additional macro-expansion support
;;
;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
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

(defun maybe-cons (car cdr original-cons)
  "Return (CAR . CDR), using ORIGINAL-CONS if possible."
  (if (and (eq car (car original-cons)) (eq cdr (cdr original-cons)))
      original-cons
    (cons car cdr)))

;; We use this special macro to iteratively process forms and share list
;; structure of the result with the input.  Doing so recursively using
;; `maybe-cons' results in excessively deep recursion for very long
;; input forms.
(defmacro macroexp-accumulate (var+list &rest body)
  "Return a list of the results of evaluating BODY for each element of LIST.
Evaluate BODY with VAR bound to each `car' from LIST, in turn.
Return a list of the values of the final form in BODY.
The list structure of the result will share as much with LIST as
possible (for instance, when BODY just returns VAR unchanged, the
result will be eq to LIST).

\(fn (VAR LIST) BODY...)"
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
       (while ,tail
	 (setq ,var (car ,tail)
	       ,new-el (progn ,@body))
	 (unless (eq ,var ,new-el)
	   (while (not (eq ,shared ,tail))
	     (push (pop ,shared) ,unshared))
	   (setq ,shared (cdr ,shared))
	   (push ,new-el ,unshared))
	 (setq ,tail (cdr ,tail)))
       (nconc (nreverse ,unshared) ,shared))))
(put 'macroexp-accumulate 'lisp-indent-function 1)

(defun macroexpand-all-forms (forms &optional skip)
  "Return FORMS with macros expanded.  FORMS is a list of forms.
If SKIP is non-nil, then don't expand that many elements at the start of
FORMS."
  (macroexp-accumulate (form forms)
    (if (or (null skip) (zerop skip))
	(macroexpand-all-1 form)
      (setq skip (1- skip))
      form)))

(defun macroexpand-all-clauses (clauses &optional skip)
  "Return CLAUSES with macros expanded.
CLAUSES is a list of lists of forms; any clause that's not a list is ignored.
If SKIP is non-nil, then don't expand that many elements at the start of
each clause."
  (macroexp-accumulate (clause clauses)
    (if (listp clause)
	(macroexpand-all-forms clause skip)
      clause)))

(defun macroexpand-all-1 (form)
  "Expand all macros in FORM.
This is an internal version of `macroexpand-all'.
Assumes the caller has bound `macroexpand-all-environment'."
  (if (and (listp form) (eq (car form) 'backquote-list*))
      ;; Special-case `backquote-list*', as it is normally a macro that
      ;; generates exceedingly deep expansions from relatively shallow input
      ;; forms.  We just process it `in reverse' -- first we expand all the
      ;; arguments, _then_ we expand the top-level definition.
      (macroexpand (macroexpand-all-forms form 1)
		   macroexpand-all-environment)
    ;; Normal form; get its expansion, and then expand arguments.
    (setq form (macroexpand form macroexpand-all-environment))
    (if (consp form)
	(let ((fun (car form)))
	  (cond
	   ((eq fun 'cond)
	    (maybe-cons fun (macroexpand-all-clauses (cdr form)) form))
	   ((eq fun 'condition-case)
	    (maybe-cons
	     fun
	     (maybe-cons (cadr form)
			 (maybe-cons (macroexpand-all-1 (nth 2 form))
				     (macroexpand-all-clauses (nthcdr 3 form) 1)
				     (cddr form))
			 (cdr form))
	     form))
	   ((eq fun 'defmacro)
	    (push (cons (cadr form) (cons 'lambda (cddr form)))
		  macroexpand-all-environment)
	    (macroexpand-all-forms form 3))
	   ((eq fun 'defun)
	    (macroexpand-all-forms form 3))
	   ((memq fun '(defvar defconst))
	    (macroexpand-all-forms form 2))
	   ((eq fun 'function)
	    (if (and (consp (cadr form)) (eq (car (cadr form)) 'lambda))
		(maybe-cons fun
			    (maybe-cons (macroexpand-all-forms (cadr form) 2)
					nil
					(cadr form))
			    form)
	      form))
	   ((memq fun '(let let*))
	    (maybe-cons fun
			(maybe-cons (macroexpand-all-clauses (cadr form) 1)
				    (macroexpand-all-forms (cddr form))
				    (cdr form))
			form))
	   ((eq fun 'quote)
	    form)
	   ((and (consp fun) (eq (car fun) 'lambda))
	    ;; embedded lambda
	    (maybe-cons (macroexpand-all-forms fun 2)
			(macroexpand-all-forms (cdr form))
			form))
	   ;; The following few cases are for normal function calls that
	   ;; are known to funcall one of their arguments.  The byte
	   ;; compiler has traditionally handled these functions specially
	   ;; by treating a lambda expression quoted by `quote' as if it
	   ;; were quoted by `function'.  We make the same transformation
	   ;; here, so that any code that cares about the difference will
	   ;; see the same transformation.
	   ;; First arg is a function:
	   ((and (memq fun '(apply mapcar mapatoms mapconcat mapc))
		 (consp (cadr form))
		 (eq (car (cadr form)) 'quote))
	    ;; We don't use `maybe-cons' since there's clearly a change.
	    (cons fun
		  (cons (macroexpand-all-1 (cons 'function (cdr (cadr form))))
			(macroexpand-all-forms (cddr form)))))
	   ;; Second arg is a function:
	   ((and (eq fun 'sort)
		 (consp (nth 2 form))
		 (eq (car (nth 2 form)) 'quote))
	    ;; We don't use `maybe-cons' since there's clearly a change.
	    (cons fun
		  (cons (macroexpand-all-1 (cadr form))
			(cons (macroexpand-all-1
			       (cons 'function (cdr (nth 2 form))))
			      (macroexpand-all-forms (nthcdr 3 form))))))
	   (t
	    ;; For everything else, we just expand each argument (for
	    ;; setq/setq-default this works alright because the variable names
	    ;; are symbols).
	    (macroexpand-all-forms form 1))))
      form)))

;;;###autoload
(defun macroexpand-all (form &optional environment)
  "Return result of expanding macros at all levels in FORM.
If no macros are expanded, FORM is returned unchanged.
The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation."
  (let ((macroexpand-all-environment environment))
    (macroexpand-all-1 form)))

(provide 'macroexp)

;; arch-tag: af9b8c24-c196-43bc-91e1-a3570790fa5a
;;; macroexp.el ends here
