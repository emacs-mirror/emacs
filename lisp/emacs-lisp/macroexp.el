;;; macroexp.el --- Additional macro-expansion support
;;
;; Copyright (C) 2001 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: lisp

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

(defun macroexpand-all-forms (forms &optional skip)
  "Return FORMS with macros expanded.  FORMS is a list of forms.
If SKIP is non-nil, then don't expand that many elements at the start of
FORMS."
  (and forms
       (maybe-cons
	(if (and (integerp skip) (> skip 0))
	    (car forms)
	  (macroexpand-all-1 (car forms)))
	(macroexpand-all-forms (cdr forms)
			       (and (integerp skip) (> skip 0) (1- skip)))
	forms)))

(defun macroexpand-all-clauses (clauses &optional skip)
  "Return CLAUSES with macros expanded.
CLAUSES is a list of lists of forms; any clause that's not a list is ignored.
If SKIP is non-nil, then don't expand that many elements at the start of
each clause."
  (and clauses
       (maybe-cons
	(if (listp (car clauses))
	    (macroexpand-all-forms (car clauses) skip)
	  (car clauses))
	(macroexpand-all-clauses (cdr clauses) skip)
	clauses)))

(defun macroexpand-all-1 (form)
  "Expand all macros in FORM.
This is an internal version of `macroexpand-all'.
Assumes the caller has bound `macroexpand-all-environment'."
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
	 ((eq fun 'defconst)
	  form)
	 ((eq fun 'defmacro)
	  (push (cons (cadr form) (cons 'lambda (cddr form)))
		macroexpand-all-environment))
	 ((eq fun 'defun)
	  (macroexpand-all-forms form 3))
	 ((eq fun 'defvar)
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
    form))

(defun macroexpand-all (form &optional environment)
  "Return result of expanding macros at all levels in FORM.
If no macros are expanded, FORM is returned unchanged.
The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation."
  (let ((macroexpand-all-environment environment))
    (macroexpand-all-1 form)))

(provide 'macroexp)

;;; macroexp.el ends here


