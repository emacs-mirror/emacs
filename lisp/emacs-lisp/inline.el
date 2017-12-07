;;; inline.el --- Define functions by their inliner  -*- lexical-binding:t; -*-

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;; This package provides the macro `define-inline' which lets you define
;; functions by defining their (exhaustive) compiler macro.
;;
;; The idea is that instead of doing like defsubst and cl-defsubst (i.e. from
;; the function's definition, guess the best way to inline the function),
;; we go the other way around: the programmer provides the code that does the
;; inlining (as a compiler-macro) and from that we derive the definition of the
;; function itself.  The idea originated in an attempt to clean up `cl-typep',
;; whose function definition amounted to (eval (cl--make-type-test EXP TYPE)).
;;
;; The simplest use is for plain and simple inlinable functions.  Rather than:
;;
;;     (defmacro myaccessor (obj)
;;       (macroexp-let2 macroexp-copyable-p obj obj
;;         `(if (foop ,obj) (aref (cdr ,obj) 3) (aref ,obj 2))))
;; Or
;;     (defsubst myaccessor (obj)
;;       (if (foop obj) (aref (cdr obj) 3) (aref obj 2)))
;; Or
;;     (cl-defsubst myaccessor (obj)
;;       (if (foop obj) (aref (cdr obj) 3) (aref obj 2)))
;;
;; You'd do
;;
;;     (define-inline myaccessor (obj)
;;       (inline-letevals (obj)
;;         (inline-quote (if (foop ,obj) (aref (cdr ,obj) 3) (aref ,obj 2)))))
;;
;; Other than verbosity, you get the best of all 3 above without their
;; respective downsides:
;; - defmacro: can't be passed to `mapcar' since it's not a function.
;; - defsubst: not as efficient, and doesn't work as a `gv' place.
;; - cl-defsubst: only works by accident, since it has latent bugs in its
;;   handling of variables and scopes which could bite you at any time.
;;   (e.g. try (cl-defsubst my-test1 (x) (let ((y 5)) (+ x y)))
;;         and then M-: (macroexpand-all '(my-test1 y)) RET)
;; There is still one downside shared with the defmacro and cl-defsubst
;; approach: when the function is inlined, the scoping rules (dynamic or
;; lexical) will be inherited from the call site.

;; Of course, since define-inline defines a compiler macro, you can also do
;; call-site optimizations, just like you can with `defmacro', but not with
;; defsubst nor cl-defsubst.

;;; Code:

(require 'macroexp)

(defmacro inline-quote (_exp)
  "Similar to backquote, but quotes code and only accepts , and not ,@."
  (declare (debug t))
  (error "inline-quote can only be used within define-inline"))

(defmacro inline-const-p (_exp)
  "Return non-nil if the value of EXP is already known."
  (declare (debug t))
  (error "inline-const-p can only be used within define-inline"))

(defmacro inline-const-val (_exp)
  "Return the value of EXP."
  (declare (debug t))
  (error "inline-const-val can only be used within define-inline"))

(defmacro inline-error (_format &rest _args)
  "Signal an error."
  (declare (debug t))
  (error "inline-error can only be used within define-inline"))

(defmacro inline--leteval (_var-exp &rest _body)
  (declare (indent 1) (debug (sexp &rest body)))
  (error "inline-letevals can only be used within define-inline"))
(defmacro inline--letlisteval (_list &rest _body)
  (declare (indent 1) (debug (sexp &rest body)))
  (error "inline-letevals can only be used within define-inline"))

(defmacro inline-letevals (vars &rest body)
  "Make sure the expressions in VARS are evaluated.
VARS should be a list of elements of the form (VAR EXP) or just VAR, in case
EXP is equal to VAR.  The result is to evaluate EXP and bind the result to VAR.

The tail of VARS can be either nil or a symbol VAR which should hold a list
of arguments, in which case each argument is evaluated and the resulting
new list is re-bound to VAR.

After VARS is handled, BODY is evaluated in the new environment."
  (declare (indent 1) (debug (sexp &rest form)))
  (cond
   ((consp vars)
    `(inline--leteval ,(pop vars) (inline-letevals ,vars ,@body)))
   (vars
    `(inline--letlisteval ,vars ,@body))
   (t (macroexp-progn body))))

;; (defmacro inline-if (testfun testexp then else)
;;   (declare (indent 2) (debug (sexp symbolp form form)))
;;   (macroexp-let2 macroexp-copyable-p testsym testexp
;;     `(if (inline-const-p ,testexp)
;;          (if (,testfun (inline-const-val ,testexp)) ,then ,else)
;;        (inline-quote (if (,testfun ,testexp) ,(list '\, then)
;;                         ,(list '\, else))))))

;;;###autoload
(defmacro define-inline (name args &rest body)
  ;; FIXME: How can this work with CL arglists?
  (declare (indent defun) (debug defun) (doc-string 3))
  (let ((doc (if (stringp (car-safe body)) (list (pop body))))
        (declares (if (eq (car-safe (car-safe body)) 'declare) (pop body)))
        (cm-name (intern (format "%s--inliner" name)))
        (bodyexp (macroexp-progn body)))
    ;; If the function is autoloaded then when we load the .el file, the
    ;; `compiler-macro' property is already set (from loaddefs.el) and might
    ;; hence be called during the macroexpand-all calls below (if the function
    ;; is recursive).
    ;; So we disable any pre-loaded compiler-macro setting to avoid this.
    (function-put name 'compiler-macro nil)
    `(progn
       (defun ,name ,args
         ,@doc
         (declare (compiler-macro ,cm-name) ,@(cdr declares))
         ,(macroexpand-all bodyexp
                           `((inline-quote . inline--dont-quote)
                             ;; (inline-\` . inline--dont-quote)
                             (inline--leteval . inline--dont-leteval)
                             (inline--letlisteval . inline--dont-letlisteval)
                             (inline-const-p . inline--alwaysconst-p)
                             (inline-const-val . inline--alwaysconst-val)
                             (inline-error . inline--error)
                             ,@macroexpand-all-environment)))
       :autoload-end
       (eval-and-compile
         (defun ,cm-name ,(cons 'inline--form args)
           (ignore inline--form)     ;In case it's not used!
           (catch 'inline--just-use
             ,(macroexpand-all
               bodyexp
               `((inline-quote . inline--do-quote)
                 ;; (inline-\` . inline--do-quote)
                 (inline--leteval . inline--do-leteval)
                 (inline--letlisteval
                  . inline--do-letlisteval)
                 (inline-const-p . inline--testconst-p)
                 (inline-const-val . inline--getconst-val)
                 (inline-error . inline--warning)
                 ,@macroexpand-all-environment))))))))

(defun inline--do-quote (exp)
  (pcase exp
    (`(,'\, ,e) e)                      ;Eval `e' now *and* later.
    (`'(,'\, ,e) `(list 'quote ,e))     ;Only eval `e' now, not later.
    (`#'(,'\, ,e) `(list 'function ,e)) ;Only eval `e' now, not later.
    ((pred consp)
     (let ((args ()))
       (while (and (consp exp) (not (eq '\, (car exp))))
         (push (inline--do-quote (pop exp)) args))
       (setq args (nreverse args))
       (if exp
           `(backquote-list* ,@args ,(inline--do-quote exp))
         `(list ,@args))))
    (_ (macroexp-quote exp))))

(defun inline--dont-quote (exp)
  (pcase exp
    (`(,'\, ,e) e)
    (`'(,'\, ,e) e)
    (`#'(,'\, ,e) e)
    ((pred consp)
     (let ((args ()))
       (while (and (consp exp) (not (eq '\, (car exp))))
         (push (inline--dont-quote (pop exp)) args))
       (setq args (nreverse args))
       (if (null exp)
           args
         `(apply #',(car args) ,@(cdr args) ,(inline--dont-quote exp)))))
    (_ exp)))

(defun inline--do-leteval (var-exp &rest body)
  `(macroexp-let2 ,(if (symbolp var-exp) #'macroexp-copyable-p #'ignore)
       ,(or (car-safe var-exp) var-exp)
       ,(or (car (cdr-safe var-exp)) var-exp)
     ,@body))

(defun inline--dont-leteval (var-exp &rest body)
  (if (symbolp var-exp)
      (macroexp-progn body)
    `(let (,var-exp) ,@body)))

(defun inline--do-letlisteval (listvar &rest body)
  ;; Here's a sample situation:
  ;; (define-inline foo (arg &rest keys)
  ;;   (inline-letevals (arg . keys)
  ;;      <check-keys>))
  ;; I.e. in <check-keys> we need `keys' to contain a list of
  ;; macroexp-copyable-p expressions.
  (let ((bsym (make-symbol "bindings")))
    `(let* ((,bsym ())
            (,listvar (mapcar (lambda (e)
                                (if (macroexp-copyable-p e) e
                                  (let ((v (gensym "v")))
                                    (push (list v e) ,bsym)
                                    v)))
                              ,listvar)))
       (macroexp-let* (nreverse ,bsym)
                      ,(macroexp-progn body)))))

(defun inline--dont-letlisteval (_listvar &rest body)
  (macroexp-progn body))

(defun inline--testconst-p (exp)
  (macroexp-let2 macroexp-copyable-p exp exp
    `(or (macroexp-const-p ,exp)
         (eq (car-safe ,exp) 'function))))

(defun inline--alwaysconst-p (_exp)
  t)

(defun inline--getconst-val (exp)
  (macroexp-let2 macroexp-copyable-p exp exp
    `(cond
      ((not ,(inline--testconst-p exp))
       (throw 'inline--just-use inline--form))
      ((consp ,exp) (cadr ,exp))
      (t ,exp))))

(defun inline--alwaysconst-val (exp)
  exp)

(defun inline--error (&rest args)
  `(error ,@args))

(defun inline--warning (&rest _args)
  `(throw 'inline--just-use
          ;; FIXME: This would inf-loop by calling us right back when
          ;; macroexpand-all recurses to expand inline--form.
          ;; (macroexp--warn-and-return (format ,@args)
          ;;                            inline--form)
          inline--form))

(provide 'inline)
;;; inline.el ends here
