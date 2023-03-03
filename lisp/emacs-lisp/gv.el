;;; gv.el --- generalized variables  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2023 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: extensions
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

;; This is a re-implementation of the setf machinery using a different
;; underlying approach from the one used earlier in CL, which was based on
;; define-setf-expander.
;; `define-setf-expander' makes every "place-expander" return a 5-tuple
;;   (VARS VALUES STORES GETTER SETTER)
;; where STORES is a list with a single variable (Common-Lisp allows multiple
;; variables for use with multiple-return-values, but this is rarely used and
;; not applicable to Elisp).
;; It basically says that GETTER is an expression that returns the place's
;; value, and (lambda STORES SETTER) is an expression that assigns the value(s)
;; passed to that function to the place, and that you need to wrap the whole
;; thing within a `(let* ,(zip VARS VALUES) ...).
;;
;; Instead, we use here a higher-order approach: instead
;; of a 5-tuple, a place-expander returns a function.
;; If you think about types, the old approach returns things of type
;;    {vars: List Var, values: List Exp,
;;     stores: List Var, getter: Exp, setter: Exp}
;; whereas the new approach returns a function of type
;;    (do: ((getter: Exp, setter: ((store: Exp) -> Exp)) -> Exp)) -> Exp.
;; You can get the new function from the old 5-tuple with something like:
;;    (lambda (do)
;;       `(let* ,(zip VARS VALUES)
;;          (funcall do GETTER (lambda ,STORES ,SETTER))))
;; You can't easily do the reverse, because this new approach is more
;; expressive than the old one, so we can't provide a backward-compatible
;; get-setf-method.
;;
;; While it may seem intimidating for people not used to higher-order
;; functions, you will quickly see that its use (especially with the
;; `gv-letplace' macro) is actually much easier and more elegant than the old
;; approach which is clunky and often leads to unreadable code.

;; Food for thought: the syntax of places does not actually conflict with the
;; pcase patterns.  The `cons' gv works just like a `(,a . ,b) pcase
;; pattern, and actually the `logand' gv is even closer since it should
;; arguably fail when trying to set a value outside of the mask.
;; Generally, places are used for destructors (gethash, aref, car, ...)
;; whereas pcase patterns are used for constructors (backquote, constants,
;; vectors, ...).

;;; Code:

(require 'macroexp)

;; What we call a "gvar" is basically a function of type "(getter * setter ->
;; code) -> code", where "getter" is code and setter is "code -> code".

;; (defvar gv--macro-environment nil
;;   "Macro expanders for generalized variables.")

(define-error 'gv-invalid-place "Invalid place expression")

;;;###autoload
(defun gv-get (place do)
  "Build the code that applies DO to PLACE.
PLACE must be a valid generalized variable.
DO must be a function; it will be called with 2 arguments: GETTER and SETTER,
where GETTER is a (copyable) Elisp expression that returns the value of PLACE,
and SETTER is a function which returns the code to set PLACE when called
with a (not necessarily copyable) Elisp expression that returns the value to
set it to.
DO must return an Elisp expression."
  (cond
   ((symbolp place)
    (let ((me (macroexpand-1 place macroexpand-all-environment)))
      (if (eq me place)
          (funcall do place (lambda (v) `(setq ,place ,v)))
        (gv-get me do))))
   ((not (consp place)) (signal 'gv-invalid-place (list place)))
   (t
    (let* ((head (car place))
           (gf (function-get head 'gv-expander 'autoload)))
      (when (and (symbolp head)
                 (get head 'byte-obsolete-generalized-variable))
        (byte-compile-warn-obsolete head "generalized variable"))
      (if gf (apply gf do (cdr place))
        (let ((me (macroexpand-1 place
                                 ;; (append macroexpand-all-environment
                                 ;;         gv--macro-environment)
                                 macroexpand-all-environment)))
          (if (and (eq me place) (get head 'compiler-macro))
              ;; Expand compiler macros: this takes care of all the accessors
              ;; defined via cl-defsubst, such as cXXXr and defstruct slots.
              (setq me (apply (get head 'compiler-macro) place (cdr place))))
          (if (and (eq me place) (fboundp head)
                   (symbolp (symbol-function head)))
              ;; Follow aliases.
              (setq me (cons (symbol-function head) (cdr place))))
          (if (eq me place)
              (if (and (symbolp head) (get head 'setf-method))
                  (error "Incompatible place needs recompilation: %S" head)
                (let* ((setter (gv-setter head)))
                  (gv--defsetter head (lambda (&rest args) `(,setter ,@args))
                                 do (cdr place))))
            (gv-get me do))))))))

(defun gv-setter (name)
  ;; The name taken from Scheme's SRFI-17.  Actually, for SRFI-17, the argument
  ;; could/should be a function value rather than a symbol.
  "Return the symbol where the (setf NAME) function should be placed."
  (if (get name 'gv-expander)
      (error "gv-expander conflicts with (setf %S)" name))
  ;; FIXME: This is wrong if `name' is uninterned (or interned elsewhere).
  (intern (format "(setf %s)" name)))

;;;###autoload
(defmacro gv-letplace (vars place &rest body)
  "Build the code manipulating the generalized variable PLACE.
GETTER will be bound to a copyable expression that returns the value
of PLACE.
SETTER will be bound to a function that takes an expression V and returns
a new expression that sets PLACE to V.
BODY should return some Elisp expression E manipulating PLACE via GETTER
and SETTER.
The returned value will then be an Elisp expression that first evaluates
all the parts of PLACE that can be evaluated and then runs E.

\(fn (GETTER SETTER) PLACE &rest BODY)"
  (declare (indent 2) (debug (sexp form def-body)))
  `(gv-get ,place (lambda ,vars ,@body)))

;; Different ways to declare a generalized variable.
;;;###autoload
(defmacro gv-define-expander (name handler)
  "Use HANDLER to handle NAME as a generalized var.
NAME is a symbol: the name of a function, macro, or special form.
HANDLER is a function which takes an argument DO followed by the same
arguments as NAME.  DO is a function as defined in `gv-get'."
  (declare (indent 1) (debug (sexp form)))
  `(function-put ',name 'gv-expander ,handler))

;;;###autoload
(defun gv--defun-declaration (symbol name args handler &optional fix)
  `(progn
     ;; No need to autoload this part, since gv-get will auto-load the
     ;; function's definition before checking the `gv-expander' property.
     :autoload-end
     ,(pcase (cons symbol handler)
        (`(gv-expander . (lambda (,do) . ,body))
         `(gv-define-expander ,name (lambda (,do ,@args) ,@body)))
        (`(gv-expander . ,(pred symbolp))
         `(gv-define-expander ,name #',handler))
        (`(gv-setter . (lambda (,store) . ,body))
         `(gv-define-setter ,name (,store ,@args) ,@body))
        (`(gv-setter . ,(pred symbolp))
         `(gv-define-simple-setter ,name ,handler ,fix))
        ;; (`(expand ,expander) `(gv-define-expand ,name ,expander))
        (_ (message "Unknown %s declaration %S" symbol handler) nil))))

(defun make-obsolete-generalized-variable (obsolete-name current-name when)
  "Make byte-compiler warn that generalized variable OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.

If CURRENT-NAME is a string, that is the `use instead' message.

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number."
  (put obsolete-name 'byte-obsolete-generalized-variable
       (purecopy (list current-name when)))
  obsolete-name)

;; Additions for `declare'.  We specify the values as named aliases so
;; that `describe-variable' prints something useful; cf. Bug#40491.

;;;###autoload
(defsubst gv--expander-defun-declaration (&rest args)
  (apply #'gv--defun-declaration 'gv-expander args))

;;;###autoload
(defsubst gv--setter-defun-declaration (&rest args)
  (apply #'gv--defun-declaration 'gv-setter args))

;;;###autoload
(or (assq 'gv-expander defun-declarations-alist)
    (let ((x (list 'gv-expander #'gv--expander-defun-declaration)))
      (push x macro-declarations-alist)
      (push x defun-declarations-alist)))
;;;###autoload
(or (assq 'gv-setter defun-declarations-alist)
    (push (list 'gv-setter #'gv--setter-defun-declaration)
	  defun-declarations-alist))

;;;###autoload
(let ((spec (get 'compiler-macro 'edebug-declaration-spec)))
  ;; It so happens that it's the same spec for gv-* as for compiler-macros.
  ;; '(&or symbolp ("lambda" &define lambda-list lambda-doc def-body))
  (put 'gv-expander 'edebug-declaration-spec spec)
  (put 'gv-setter 'edebug-declaration-spec spec))

;; (defmacro gv-define-expand (name expander)
;;   "Use EXPANDER to handle NAME as a generalized var.
;; NAME is a symbol: the name of a function, macro, or special form.
;; EXPANDER is a function that will be called as a macro-expander to reduce
;; uses of NAME to some other generalized variable."
;;   (declare (debug (sexp form)))
;;   `(eval-and-compile
;;      (if (not (boundp 'gv--macro-environment))
;;          (setq gv--macro-environment nil))
;;      (push (cons ',name ,expander) gv--macro-environment)))

(defun gv--defsetter (name setter do args &optional vars)
  "Helper function used by code generated by `gv-define-setter'.
NAME is the name of the getter function.
SETTER is a function that generates the code for the setter.
NAME accept ARGS as arguments and SETTER accepts (NEWVAL . ARGS).
VARS is used internally for recursive calls."
  (if (null args)
      (let ((vars (nreverse vars)))
        (funcall do `(,name ,@vars) (lambda (v) (apply setter v vars))))
    ;; FIXME: Often it would be OK to skip this `let', but in general,
    ;; `do' may have all kinds of side-effects.
    (macroexp-let2 nil v (car args)
      (gv--defsetter name setter do (cdr args) (cons v vars)))))

;;;###autoload
(defmacro gv-define-setter (name arglist &rest body)
  "Define a setter method for generalized variable NAME.
This macro is an easy-to-use substitute for `gv-define-expander' that works
well for simple place forms.
Assignments of VAL to (NAME ARGS...) are expanded by binding the argument
forms (VAL ARGS...) according to ARGLIST, then executing BODY, which must
return a Lisp form that does the assignment.
The first arg in ARGLIST (the one that receives VAL) receives an expression
which can do arbitrary things, whereas the other arguments are all guaranteed
to be pure and copyable.  Example use:
  (gv-define-setter aref (v a i) \\=`(aset ,a ,i ,v))"
  (declare (indent 2)
           (debug (&define [&name symbolp "@gv-setter"] sexp def-body)))
  `(gv-define-expander ,name
     (lambda (do &rest args)
       (declare-function
        gv--defsetter "gv" (name setter do args &optional vars))
       (gv--defsetter ',name (lambda ,arglist ,@body) do args))))

;;;###autoload
(defmacro gv-define-simple-setter (name setter &optional fix-return)
  "Define a simple setter method for generalized variable NAME.
This macro is an easy-to-use substitute for `gv-define-expander' that works
well for simple place forms.  Assignments of VAL to (NAME ARGS...) are
turned into calls of the form (SETTER ARGS... VAL).

If FIX-RETURN is non-nil, then SETTER is not assumed to return VAL and
instead the assignment is turned into something equivalent to
  (let ((temp VAL))
    (SETTER ARGS... temp)
    temp)
so as to preserve the semantics of `setf'."
  (declare (debug (sexp (&or symbolp lambda-expr) &optional sexp)))
  (when (eq 'lambda (car-safe setter))
    (message "Use `gv-define-setter' or name %s's setter function" name))
  `(gv-define-setter ,name (val &rest args)
     ,(if fix-return
          `(macroexp-let2 nil v val
             `(progn
                (,',setter ,@args ,v)
                ,v))
        ``(,',setter ,@args ,val))))

;;; Typical operations on generalized variables.

;;;###autoload
(defmacro setf (&rest args)
  "Set each PLACE to the value of its VAL.
This is a generalized version of `setq'; the PLACEs may be symbolic
references such as (car x) or (aref x i), as well as plain symbols.
For example, (setf (cadr x) y) is equivalent to (setcar (cdr x) y).
The return value is the last VAL in the list.

\(fn PLACE VAL PLACE VAL ...)"
  (declare (debug (&rest [gv-place form])))
  (if (/= (logand (length args) 1) 0)
      (signal 'wrong-number-of-arguments (list 'setf (length args))))
  (if (and args (null (cddr args)))
      (let ((place (pop args))
            (val (car args)))
        (gv-letplace (_getter setter) place
          (funcall setter val)))
    (let ((sets nil))
      (while args (push `(setf ,(pop args) ,(pop args)) sets))
      (cons 'progn (nreverse sets)))))

;; (defmacro gv-pushnew! (val place)
;;   "Like `gv-push!' but only adds VAL if it's not yet in PLACE.
;; Presence is checked with `member'.
;; The return value is unspecified."
;;   (declare (debug (form gv-place)))
;;   (macroexp-let2 macroexp-copyable-p v val
;;     (gv-letplace (getter setter) place
;;       `(if (member ,v ,getter) nil
;;          ,(funcall setter `(cons ,v ,getter))))))

;; (defmacro gv-inc! (place &optional val)
;;   "Increment PLACE by VAL (default to 1)."
;;   (declare (debug (gv-place &optional form)))
;;   (gv-letplace (getter setter) place
;;     (funcall setter `(+ ,getter ,(or val 1)))))

;; (defmacro gv-dec! (place &optional val)
;;   "Decrement PLACE by VAL (default to 1)."
;;   (declare (debug (gv-place &optional form)))
;;   (gv-letplace (getter setter) place
;;     (funcall setter `(- ,getter ,(or val 1)))))

;; For Edebug, the idea is to let Edebug instrument gv-places just like it does
;; for normal expressions, and then give it a gv-expander to DTRT.
;; Maybe this should really be in edebug.el rather than here.

;; Autoload this `put' since a user might use C-u C-M-x on an expression
;; containing a non-trivial `push' even before gv.el was loaded.
;;;###autoload
(def-edebug-elem-spec 'gv-place '(form))

;; CL did the equivalent of:
;;(gv-define-macroexpand edebug-after (lambda (before index place) place))
(put 'edebug-after 'gv-expander
     (lambda (do before index place)
       (gv-letplace (getter setter) place
         (funcall do `(edebug-after ,before ,index ,getter)
                  (lambda (store)
                    `(edebug-after ,before ,index ,(funcall setter store)))))))

;;; The common generalized variables.

(gv-define-simple-setter aref aset)
(gv-define-simple-setter char-table-range set-char-table-range)
(gv-define-simple-setter car setcar)
(gv-define-simple-setter cdr setcdr)
;; FIXME: add compiler-macros for `cXXr' instead!
(gv-define-setter caar (val x) `(setcar (car ,x) ,val))
(gv-define-setter cadr (val x) `(setcar (cdr ,x) ,val))
(gv-define-setter cdar (val x) `(setcdr (car ,x) ,val))
(gv-define-setter cddr (val x) `(setcdr (cdr ,x) ,val))
(gv-define-setter elt (store seq n)
  `(if (listp ,seq) (setcar (nthcdr ,n ,seq) ,store)
     (aset ,seq ,n ,store)))
(gv-define-simple-setter get put)
(gv-define-setter gethash (val k h &optional _d) `(puthash ,k ,val ,h))

;; (gv-define-expand nth (lambda (idx list) `(car (nthcdr ,idx ,list))))
(put 'nth 'gv-expander
     (lambda (do idx list)
       (macroexp-let2 nil c `(nthcdr ,idx ,list)
         (funcall do `(car ,c) (lambda (v) `(setcar ,c ,v))))))
(gv-define-simple-setter symbol-function fset)
(gv-define-simple-setter symbol-plist setplist)
(gv-define-simple-setter symbol-value set)

(put 'nthcdr 'gv-expander
     (lambda (do n place)
       (macroexp-let2 nil idx n
         (gv-letplace (getter setter) place
           (funcall do `(nthcdr ,idx ,getter)
                    (lambda (v) `(if (<= ,idx 0) ,(funcall setter v)
                              (setcdr (nthcdr (1- ,idx) ,getter) ,v))))))))

;;; Elisp-specific generalized variables.

(gv-define-simple-setter default-value set-default)
(gv-define-simple-setter frame-parameter set-frame-parameter 'fix)
(gv-define-simple-setter terminal-parameter set-terminal-parameter)
(gv-define-simple-setter keymap-parent set-keymap-parent)
(gv-define-simple-setter match-data set-match-data 'fix)
(gv-define-simple-setter overlay-get overlay-put)
(gv-define-setter overlay-start (store ov)
  `(progn (move-overlay ,ov ,store (overlay-end ,ov)) ,store))
(gv-define-setter overlay-end (store ov)
  `(progn (move-overlay ,ov (overlay-start ,ov) ,store) ,store))
(gv-define-simple-setter process-buffer set-process-buffer)
(gv-define-simple-setter process-filter set-process-filter)
(gv-define-simple-setter process-sentinel set-process-sentinel)
(gv-define-simple-setter process-get process-put)
(gv-define-simple-setter window-parameter set-window-parameter)
(gv-define-setter window-buffer (v &optional w)
  (macroexp-let2 nil v v
    `(progn (set-window-buffer ,w ,v) ,v)))
(gv-define-setter window-display-table (v &optional w)
  (macroexp-let2 nil v v
    `(progn (set-window-display-table ,w ,v) ,v)))
(gv-define-setter window-dedicated-p (v &optional w)
  `(set-window-dedicated-p ,w ,v))
(gv-define-setter window-hscroll (v &optional w) `(set-window-hscroll ,w ,v))
(gv-define-setter window-point (v &optional w) `(set-window-point ,w ,v))
(gv-define-setter window-start (v &optional w) `(set-window-start ,w ,v))

(gv-define-setter buffer-local-value (val var buf)
  (macroexp-let2 nil v val
    `(with-current-buffer ,buf (set (make-local-variable ,var) ,v))))
(make-obsolete-generalized-variable 'buffer-local-value nil "29.1")

(gv-define-expander alist-get
  (lambda (do key alist &optional default remove testfn)
    (macroexp-let2 macroexp-copyable-p k key
      (gv-letplace (getter setter) alist
        (macroexp-let2 nil p (if (member testfn '(nil 'eq #'eq))
                                 `(assq ,k ,getter)
                               `(assoc ,k ,getter ,testfn))
          (funcall do (if (null default) `(cdr ,p)
                        `(if ,p (cdr ,p) ,default))
                   (lambda (v)
                     (macroexp-let2 nil v v
                       (let ((set-exp
                              `(if ,p (setcdr ,p ,v)
                                 ,(funcall setter
                                           `(cons (setq ,p (cons ,k ,v))
                                                  ,getter)))))
                         `(progn
                            ,(cond
                             ((null remove) set-exp)
                             ((or (eql v default)
                                  (and (eq (car-safe v) 'quote)
                                       (eq (car-safe default) 'quote)
                                       (eql (cadr v) (cadr default))))
                              `(if ,p ,(funcall setter `(delq ,p ,getter))))
                             (t
                              `(cond
                                ((not (eql ,default ,v)) ,set-exp)
                                (,p ,(funcall setter
                                              `(delq ,p ,getter))))))
                            ,v))))))))))

(gv-define-expander plist-get
  (lambda (do plist prop &optional predicate)
    (macroexp-let2 macroexp-copyable-p key prop
      (gv-letplace (getter setter) plist
        (macroexp-let2 nil p `(cdr (plist-member ,getter ,key ,predicate))
          (funcall do
                   `(car ,p)
                   (lambda (val)
                     `(if ,p
                          (setcar ,p ,val)
                        ,(funcall setter
                                  `(cons ,key (cons ,val ,getter)))))))))))

;;; Some occasionally handy extensions.

;; While several of the "places" below are not terribly useful for direct use,
;; they can show up as the output of the macro expansion of reasonable places,
;; such as struct-accessors.

(put 'progn 'gv-expander
     (lambda (do &rest exps)
       (let ((start (butlast exps))
             (end (car (last exps))))
         (if (null start) (gv-get end do)
           `(progn ,@start ,(gv-get end do))))))

(let ((let-expander
       (lambda (letsym)
         (lambda (do bindings &rest body)
           `(,letsym ,bindings
                     ,@(macroexp-unprogn
                        (gv-get (macroexp-progn body) do)))))))
  (put 'let 'gv-expander (funcall let-expander 'let))
  (put 'let* 'gv-expander (funcall let-expander 'let*)))

(put 'if 'gv-expander
     (lambda (do test then &rest else)
       (if (or (not lexical-binding)  ;The other code requires lexical-binding.
               (macroexp-small-p (funcall do 'dummy (lambda (_) 'dummy))))
           ;; This duplicates the `do' code, which is a problem if that
           ;; code is large, but otherwise results in more efficient code.
           `(if ,test ,(gv-get then do)
              ,@(macroexp-unprogn (gv-get (macroexp-progn else) do)))
         (let ((v (gensym "v")))
           (macroexp-let2 nil
               gv `(if ,test ,(gv-letplace (getter setter) then
                                `(cons (lambda () ,getter)
                                       (lambda (,v) ,(funcall setter v))))
                     ,(gv-letplace (getter setter) (macroexp-progn else)
                        `(cons (lambda () ,getter)
                               (lambda (,v) ,(funcall setter v)))))
             (funcall do `(funcall (car ,gv))
                      (lambda (v) `(funcall (cdr ,gv) ,v))))))))

(put 'cond 'gv-expander
     (lambda (do &rest branches)
       (if (or (not lexical-binding)  ;The other code requires lexical-binding.
               (macroexp-small-p (funcall do 'dummy (lambda (_) 'dummy))))
           ;; This duplicates the `do' code, which is a problem if that
           ;; code is large, but otherwise results in more efficient code.
           `(cond
             ,@(mapcar (lambda (branch)
                         (if (cdr branch)
                             (cons (car branch)
                                   (macroexp-unprogn
                                    (gv-get (macroexp-progn (cdr branch)) do)))
                           (gv-get (car branch) do)))
                       branches))
         (let ((v (gensym "v")))
           (macroexp-let2 nil
               gv `(cond
                    ,@(mapcar
                       (lambda (branch)
                         (if (cdr branch)
                             `(,(car branch)
                               ,@(macroexp-unprogn
                                  (gv-letplace (getter setter)
                                      (macroexp-progn (cdr branch))
                                    `(cons (lambda () ,getter)
                                           (lambda (,v) ,(funcall setter v))))))
                           (gv-letplace (getter setter)
                               (car branch)
                             `(cons (lambda () ,getter)
                                    (lambda (,v) ,(funcall setter v))))))
                       branches))
             (funcall do `(funcall (car ,gv))
                      (lambda (v) `(funcall (cdr ,gv) ,v))))))))

(put 'error 'gv-expander
     (lambda (do &rest args)
       (funcall do `(error . ,args)
                (lambda (v) `(progn ,v (error . ,args))))))

(defun gv-synthetic-place (getter setter)
  "Special place described by its setter and getter.
GETTER and SETTER (typically obtained via `gv-letplace') get and
set that place.  I.e. this function allows you to do the
\"reverse\" of what `gv-letplace' does.

This function is only useful when used in conjunction with
generalized variables in place forms."
  (declare (gv-expander funcall) (compiler-macro (lambda (_) getter)))
  (ignore setter)
  getter)

(defmacro gv-delay-error (place)
  "Special place which delays the `gv-invalid-place' error to run-time.
It behaves just like PLACE except that in case PLACE is not a valid place,
the `gv-invalid-place' error will only be signaled at run-time when (and if)
we try to use the setter.
This macro only makes sense when used in a place."
  (declare
   (gv-expander
    (lambda (do)
      (condition-case err
          (gv-get place do)
        (gv-invalid-place
         ;; Delay the error until we try to use the setter.
         (funcall do place (lambda (_) `(signal ',(car err) ',(cdr err)))))))))
  place)

;;; Even more debatable extensions.

(put 'cons 'gv-expander
     (lambda (do a d)
       (gv-letplace (agetter asetter) a
         (gv-letplace (dgetter dsetter) d
           (funcall do
                    `(cons ,agetter ,dgetter)
                    (lambda (v)
                      (macroexp-let2 nil v v
                        `(progn
                           ,(funcall asetter `(car ,v))
                           ,(funcall dsetter `(cdr ,v))
                           ,v))))))))

(put 'logand 'gv-expander
     (lambda (do place &rest masks)
       (gv-letplace (getter setter) place
         (macroexp-let2 macroexp-copyable-p
             mask (if (cdr masks) `(logand ,@masks) (car masks))
           (funcall
            do `(logand ,getter ,mask)
            (lambda (v)
              (macroexp-let2 nil v v
                `(progn
                   ,(funcall setter
                             `(logior (logand ,v ,mask)
                                      (logand ,getter (lognot ,mask))))
                   ,v))))))))

;;; References

;;;###autoload
(defmacro gv-ref (place)
  "Return a reference to PLACE.
This is like the `&' operator of the C language.
Note: this only works reliably with lexical binding mode, except for very
simple PLACEs such as (symbol-function \\='foo) which will also work in dynamic
binding mode."
  (let ((code
         (gv-letplace (getter setter) place
           `(cons (lambda () ,getter)
                  (lambda (gv--val) ,(funcall setter 'gv--val))))))
    (if (or lexical-binding
            ;; If `code' still starts with `cons' then presumably gv-letplace
            ;; did not add any new let-bindings, so the `lambda's don't capture
            ;; any new variables.  As a consequence, the code probably works in
            ;; dynamic binding mode as well.
            (eq (car-safe code) 'cons))
        code
      (macroexp-warn-and-return
       "Use of gv-ref probably requires lexical-binding"
       code nil nil place))))

(defsubst gv-deref (ref)
  "Dereference REF, returning the referenced value.
This is like the `*' operator of the C language.
REF must have been previously obtained with `gv-ref'."
  (funcall (car ref)))
;; Don't use `declare' because it seems to introduce circularity problems:
;; Eager macro-expansion skipped due to cycle:
;;  … => (load "gv.el") => (macroexpand-all (defsubst gv-deref …)) => (macroexpand (defun …)) => (load "gv.el")
(gv-define-setter gv-deref (v ref) `(funcall (cdr ,ref) ,v))

;; (defmacro gv-letref (vars place &rest body)
;;   (declare (indent 2) (debug (sexp form body)))
;;   (require 'cl-lib) ;Can't require cl-lib at top-level for bootstrap reasons!
;;   (gv-letplace (getter setter) place
;;     `(cl-macrolet ((,(nth 0 vars) () ',getter)
;;                    (,(nth 1 vars) (v) (funcall ',setter v)))
;;        ,@body)))

;;; Generalized variables.

;; Some Emacs-related place types.
(gv-define-simple-setter buffer-file-name set-visited-file-name t)
(make-obsolete-generalized-variable
 'buffer-file-name 'set-visited-file-name "29.1")

(gv-define-setter buffer-modified-p (flag &optional buf)
  (macroexp-let2 nil buffer `(or ,buf (current-buffer))
    `(with-current-buffer ,buffer
       (set-buffer-modified-p ,flag))))
(make-obsolete-generalized-variable
 'buffer-modified-p 'set-buffer-modified-p "29.1")

(gv-define-simple-setter buffer-name rename-buffer t)
(make-obsolete-generalized-variable 'buffer-name 'rename-buffer "29.1")

(gv-define-setter buffer-string (store)
  `(insert (prog1 ,store (erase-buffer))))
(make-obsolete-generalized-variable 'buffer-string nil "29.1")

(gv-define-simple-setter buffer-substring cl--set-buffer-substring)
(make-obsolete-generalized-variable 'buffer-substring nil "29.1")

(gv-define-simple-setter current-buffer set-buffer)
(make-obsolete-generalized-variable 'current-buffer 'set-buffer "29.1")

(gv-define-simple-setter current-column move-to-column t)
(make-obsolete-generalized-variable 'current-column 'move-to-column "29.1")

(gv-define-simple-setter current-global-map use-global-map t)
(make-obsolete-generalized-variable 'current-global-map 'use-global-map "29.1")

(gv-define-setter current-input-mode (store)
  `(progn (apply #'set-input-mode ,store) ,store))
(make-obsolete-generalized-variable 'current-input-mode nil "29.1")

(gv-define-simple-setter current-local-map use-local-map t)
(make-obsolete-generalized-variable 'current-local-map 'use-local-map "29.1")

(gv-define-simple-setter current-window-configuration
                         set-window-configuration t)
(make-obsolete-generalized-variable
 'current-window-configuration 'set-window-configuration "29.1")

(gv-define-simple-setter default-file-modes set-default-file-modes t)
(make-obsolete-generalized-variable
 'default-file-modes 'set-default-file-modes "29.1")

(gv-define-simple-setter documentation-property put)
(make-obsolete-generalized-variable 'documentation-property 'put "29.1")

(gv-define-setter face-background (x f &optional s)
  `(set-face-background ,f ,x ,s))
(gv-define-setter face-background-pixmap (x f &optional s)
  `(set-face-stipple ,f ,x ,s))
(make-obsolete-generalized-variable 'face-background-pixmap 'face-stipple "29.1")
(gv-define-setter face-stipple (x f &optional s)
  `(set-face-stipple ,f ,x ,s))
(gv-define-setter face-font (x f &optional s) `(set-face-font ,f ,x ,s))
(gv-define-setter face-foreground (x f &optional s)
  `(set-face-foreground ,f ,x ,s))
(gv-define-setter face-underline-p (x f &optional s)
  `(set-face-underline ,f ,x ,s))
(gv-define-simple-setter file-modes set-file-modes t)

(gv-define-setter frame-height (x &optional frame)
  `(set-frame-height (or ,frame (selected-frame)) ,x))
(make-obsolete-generalized-variable 'frame-height 'set-frame-height "29.1")

(gv-define-simple-setter frame-parameters modify-frame-parameters t)
(gv-define-simple-setter frame-visible-p cl--set-frame-visible-p)
(make-obsolete-generalized-variable 'frame-visible-p nil "29.1")

(gv-define-setter frame-width (x &optional frame)
  `(set-frame-width (or ,frame (selected-frame)) ,x))
(make-obsolete-generalized-variable 'frame-width 'set-frame-width "29.1")

(gv-define-simple-setter getenv setenv t)
(gv-define-simple-setter get-register set-register)

(gv-define-simple-setter global-key-binding global-set-key)
(make-obsolete-generalized-variable 'global-key-binding 'global-set-key "29.1")

(gv-define-simple-setter local-key-binding local-set-key)
(make-obsolete-generalized-variable 'local-key-binding 'local-set-key "29.1")

(gv-define-simple-setter mark set-mark t)
(make-obsolete-generalized-variable 'mark 'set-mark "29.1")

(gv-define-simple-setter mark-marker set-mark t)
(make-obsolete-generalized-variable 'mark-marker 'set-mark "29.1")

(gv-define-simple-setter marker-position set-marker t)
(make-obsolete-generalized-variable 'marker-position 'set-marker "29.1")

(gv-define-setter mouse-position (store scr)
  `(set-mouse-position ,scr (car ,store) (cadr ,store)
                       (cddr ,store)))
(make-obsolete-generalized-variable 'mouse-position 'set-mouse-position "29.1")

(gv-define-simple-setter point goto-char)
(make-obsolete-generalized-variable 'point 'goto-char "29.1")

(gv-define-simple-setter point-marker goto-char t)
(make-obsolete-generalized-variable 'point-marker 'goto-char "29.1")

(gv-define-setter point-max (store)
  `(progn (narrow-to-region (point-min) ,store) ,store))
(make-obsolete-generalized-variable 'point-max 'narrow-to-region "29.1")

(gv-define-setter point-min (store)
  `(progn (narrow-to-region ,store (point-max)) ,store))
(make-obsolete-generalized-variable 'point-min 'narrow-to-region "29.1")

(gv-define-setter read-mouse-position (store scr)
  `(set-mouse-position ,scr (car ,store) (cdr ,store)))
(make-obsolete-generalized-variable
 'read-mouse-position 'set-mouse-position "29.1")

(gv-define-simple-setter screen-height set-screen-height t)
(make-obsolete-generalized-variable 'screen-height 'set-screen-height "29.1")

(gv-define-simple-setter screen-width set-screen-width t)
(make-obsolete-generalized-variable 'screen-width 'set-screen-width "29.1")

(gv-define-simple-setter selected-window select-window)
(make-obsolete-generalized-variable 'selected-window 'select-window "29.1")

(gv-define-simple-setter selected-screen select-screen)
(make-obsolete-generalized-variable 'selected-screen 'select-screen "29.1")

(gv-define-simple-setter selected-frame select-frame)
(make-obsolete-generalized-variable 'selected-frame 'select-frame "29.1")

(gv-define-simple-setter standard-case-table set-standard-case-table)
(make-obsolete-generalized-variable
 'standard-case-table 'set-standard-case-table "29.1")

(gv-define-simple-setter syntax-table set-syntax-table)
(make-obsolete-generalized-variable 'syntax-table 'set-syntax-table "29.1")

(gv-define-simple-setter visited-file-modtime set-visited-file-modtime t)
(make-obsolete-generalized-variable
 'visited-file-modtime 'set-visited-file-modtime "29.1")

(gv-define-setter window-height (store)
  `(progn (enlarge-window (- ,store (window-height))) ,store))
(make-obsolete-generalized-variable 'window-height 'enlarge-window "29.1")

(gv-define-setter window-width (store)
  `(progn (enlarge-window (- ,store (window-width)) t) ,store))
(make-obsolete-generalized-variable 'window-width 'enlarge-window "29.1")

(gv-define-simple-setter x-get-secondary-selection x-own-secondary-selection t)
(make-obsolete-generalized-variable
 'x-get-secondary-selection 'x-own-secondary-selection "29.1")


;; More complex setf-methods.

;; This is a hack that allows (setf (eq a 7) B) to mean either
;; (setq a 7) or (setq a nil) depending on whether B is nil or not.
;; This is useful when you have control over the PLACE but not over
;; the VALUE, as is the case in define-minor-mode's :variable.
;; It turned out that :variable needed more flexibility anyway, so
;; this doesn't seem too useful now.
(gv-define-expander eq
  (lambda (do place val)
    (gv-letplace (getter setter) place
      (macroexp-let2 nil val val
        (funcall do `(eq ,getter ,val)
                 (lambda (v)
                   `(cond
                     (,v ,(funcall setter val))
                     ((eq ,getter ,val) ,(funcall setter `(not ,val))))))))))
(make-obsolete-generalized-variable 'eq nil "29.1")

(gv-define-expander substring
  (lambda (do place from &optional to)
    (gv-letplace (getter setter) place
      (macroexp-let2* nil ((start from) (end to))
        (funcall do `(substring ,getter ,start ,end)
                 (lambda (v)
                   (macroexp-let2 nil v v
                     `(progn
                        ,(funcall setter `(cl--set-substring
                                           ,getter ,start ,end ,v))
                        ,v))))))))

(provide 'gv)
;;; gv.el ends here
