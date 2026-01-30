;;; macroexp.el --- Additional macro-expansion support -*- lexical-binding: t -*-
;;
;; Copyright (C) 2004-2026 Free Software Foundation, Inc.
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file contains macro-expansions functions that are not defined in
;; the Lisp core, namely `macroexpand-all', which expands all macros in
;; a form, not just a top-level one.

;;; Code:

(defvar byte-compile-form-stack nil
  "Dynamic list of successive enclosing forms.
This is used by the warning message routines to determine a
source code position.  The most accessible element is the current
most deeply nested form.

Normally a form is manually pushed onto the list at the beginning
of `byte-compile-form', etc., and manually popped off at its end.
This is to preserve the data in it in the event of a
`condition-case' handling a signaled error.")

(defmacro macroexp--with-extended-form-stack (expr &rest body)
  "Evaluate BODY with EXPR pushed onto `byte-compile-form-stack'."
  (declare (indent 1)
           (debug (sexp body)))
  `(let ((byte-compile-form-stack (cons ,expr byte-compile-form-stack)))
     ,@body))

;; Bound by the top-level `macroexpand-all', and modified to include any
;; macros defined by `defmacro'.
(defvar macroexpand-all-environment nil)

(defun macroexp--cons (car cdr original-cons)
  "Return ORIGINAL-CONS if the car/cdr of it is `eq' to CAR and CDR, respectively.
If not, return (CAR . CDR)."
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

(defvar macroexp-inhibit-compiler-macros nil
  "Inhibit application of compiler macros if non-nil.")

(defun macroexp--compiler-macro (handler form)
  "Apply compiler macro HANDLER to FORM and return the result.
Unless `macroexp-inhibit-compiler-macros' is non-nil, in which
case return FORM unchanged."
  (if macroexp-inhibit-compiler-macros
      form
    (condition-case-unless-debug err
        (macroexp-preserve-posification
             form
          (apply handler form (cdr form)))
      (error
       (message "Warning: Optimization failure for %S: Handler: %S\n%S"
                (car form) handler err)
       form))))

(defun macroexp--funcall-if-compiled (_form)
  "Pseudo function used internally by macroexp to delay warnings.
The purpose is to delay warnings to bytecomp.el, so they can use things
like `byte-compile-warn' to get better file-and-line-number data
and also to avoid outputting the warning during normal execution."
  nil)
(put 'macroexp--funcall-if-compiled 'byte-compile
     (lambda (form)
       (funcall (eval (cadr form)))
       (byte-compile-constant nil)))

(defun macroexp-compiling-p ()
  "Return non-nil if we're macroexpanding for the compiler."
  ;; FIXME: ¡¡Major Ugly Hack!! To determine whether the output of this
  ;; macro-expansion will be processed by the byte-compiler, we check
  ;; circumstantial evidence.
  (member '(declare-function . byte-compile-macroexpand-declare-function)
          macroexpand-all-environment))

(defun macroexp-file-name ()
  "Return the name of the file from which the code comes.
Returns nil when we do not know.
A non-nil result is expected to be reliable when called from a macro in order
to find the file in which the macro's call was found, and it should be
reliable as well when used at the top-level of a file.
Other uses risk returning non-nil value that point to the wrong file."
  ;; `eval-buffer' binds `current-load-list' but not `load-file-name',
  ;; so prefer using it over using `load-file-name'.
  (let ((file (car (last current-load-list))))
    (or (if (stringp file) file)
        (bound-and-true-p byte-compile-current-file))))

(defvar macroexp--warned (make-hash-table :test #'equal :weakness 'key))

(defun macroexp--warn-wrap (arg msg form category)
  (let ((when-compiled
	 (lambda ()
           (when (if (consp category)
                     (apply #'byte-compile-warning-enabled-p category)
                   (byte-compile-warning-enabled-p category))
             (byte-compile-warn-x arg "%s" msg)))))
    `(progn
       (macroexp--funcall-if-compiled ',when-compiled)
       ,form)))

(define-obsolete-function-alias 'macroexp--warn-and-return
  #'macroexp-warn-and-return "28.1")
(defun macroexp-warn-and-return (msg form &optional category compile-only arg)
  "Return code equivalent to FORM labeled with warning MSG.
CATEGORY is the category of the warning, like the categories that
can appear in `byte-compile-warnings'.
COMPILE-ONLY non-nil means no warning should be emitted if the code
is executed without being compiled first.
ARG is a symbol (or a form) giving the source code position for the message.
It should normally be a symbol with position and it defaults to FORM."
  (cond
   ((null msg) form)
   ((macroexp-compiling-p)
    (if (and (consp form) (gethash form macroexp--warned))
        ;; Already wrapped this exp with a warning: avoid inf-looping
        ;; where we keep adding the same warning onto `form' because
        ;; macroexpand-all gets right back to macroexpanding `form'.
        form
      (puthash form form macroexp--warned)
      (macroexp--warn-wrap (or arg form) msg form category)))
   (t
    (unless compile-only
      (message "%sWarning: %s"
               (if (stringp load-file-name)
                   (concat (file-relative-name load-file-name) ": ")
                 "")
               msg))
    form)))

(defun macroexp--obsolete-warning (fun obsolescence-data type &optional key)
  (let ((instead (car obsolescence-data))
        (asof (nth 2 obsolescence-data)))
    (format-message
     "`%s' is an obsolete %s%s%s" fun type
     (if asof (concat " (as of " asof ")") "")
     (cond ((stringp instead) (concat "; " (substitute-command-keys instead)))
           ((and instead key)
            (format-message "; use `%S' (%s) instead." instead key))
           (instead (format-message "; use `%S' instead." instead))
           (t ".")))))

(defun macroexpand-1 (form &optional environment)
  "Perform (at most) one step of macroexpansion."
  (cond
   ((consp form)
    (let* ((head (car form))
           (env-expander (assq head environment)))
      (if env-expander
          (if (cdr env-expander)
              (apply (cdr env-expander) (cdr form))
            form)
        (if (not (and (symbolp head) (fboundp head)))
            form
          (let ((def (autoload-do-load (symbol-function head) head 'macro)))
            (cond
             ;; Follow alias, but only for macros, otherwise we may end up
             ;; skipping an important compiler-macro (e.g. cl--block-wrapper).
             ((and (symbolp def) (macrop def)) (cons def (cdr form)))
             ((not (consp def)) form)
             (t
              (if (eq 'macro (car def))
                  (apply (cdr def) (cdr form))
                form))))))))
   (t form)))

(defun macroexp--posify-form-1 (form call-pos depth)
  "The recursive part of `macroexp--posify-form'.
It modifies a single symbol to a symbol with position, or does nothing.
FORM and CALL-POS are as in that function.  DEPTH is a small integer,
decremented at each recursive call, to prevent infinite recursion.

Return the form with a symbol with position in the canonical position
for that form, either the one that was already there or CALL-POS; return
nil if this isn't possible.
"
  (let (new-form)
    (cond
     ((zerop depth) nil)
     ((and (consp form)
           (symbolp (car form))
           (car form))
      (unless (symbol-with-pos-p (car form))
        (setcar form (position-symbol (car form) call-pos)))
      form)
     ((consp form)
      (or (when (setq new-form (macroexp--posify-form-1
                                (car form) call-pos (1- depth)))
            (setcar form new-form)
            form)
          (when (setq new-form (macroexp--posify-form-1
                                (cdr form) call-pos (1- depth)))
            (setcdr form new-form)
            form)))
     ((symbolp form)
      (if form                          ; Don't position nil!
          (if (symbol-with-pos-p form)
              form
            (position-symbol form call-pos))))
     ((and (or (vectorp form) (recordp form)))
      (let ((len (length form))
            (i 0)
            )
        (while (and (< i len)
                    (not (setq new-form (macroexp--posify-form-1
                                         (aref form i) call-pos (1- depth)))))
          (setq i (1+ i)))
        (when (< i len)
          (aset form i new-form)
          form))))))

(defun macroexp--posify-form (form call-pos)
  "Try to apply the position CALL-POS to the form FORM, if needed.
CALL-POS is a buffer position, a number.  FORM may be any lisp form,
and is typically the output form returned by a macro expansion.

Apply CALL-POS to FORM as a symbol with position, such that
`byte-compile--first-symbol-with-pos' can later return it.  If there is
already a symbol with position in a \"canonical\" position for that
function, leave it unchanged and do nothing.  Return the possibly
modified FORM."
  (let ((new-form (macroexp--posify-form-1 form call-pos 10)))
    (or new-form form)))

(defmacro macroexp-preserve-posification (pos-form &rest body)
  "Evaluate BODY..., posifying the result with POS-FORM's position, if any.
If the result of body happens to have a position already, we do not
change this."
  (declare (debug (sexp body)) (indent 1))
  `(let ((call-pos (cond
                    ((consp ,pos-form)
                     (and (symbol-with-pos-p (car ,pos-form))
                          (symbol-with-pos-pos (car ,pos-form))))
                    ((symbol-with-pos-p ,pos-form)
                     (symbol-with-pos-pos ,pos-form))))
         (new-value (progn ,@body)))
     (if (and call-pos
              (not (or (and (consp new-value)
                            (symbol-with-pos-p (car new-value)))
                       (and (symbol-with-pos-p new-value)))))
         (macroexp--posify-form new-value call-pos)
       new-value)))

(defun macroexp-macroexpand (form env)
  "Like `macroexpand' but checking obsolescence."
  (let* ((macroexpand-all-environment env)
         new-form)
    (macroexp-preserve-posification
        form
      (while (not (eq form (setq new-form (macroexpand-1 form env))))
        (let ((fun (car-safe form)))
          (setq form
                (if (and fun (symbolp fun)
                         (get fun 'byte-obsolete-info))
                    (macroexp-warn-and-return
                     (macroexp--obsolete-warning
                      fun (get fun 'byte-obsolete-info)
                      (if (symbolp (symbol-function fun)) "alias" "macro"))
                     new-form (list 'obsolete fun) nil fun)
                  new-form))))
      form)))

(defun macroexp--unfold-lambda (form &optional name)
  (or name (setq name "anonymous lambda"))
  (pcase form
    ((or `(funcall (function ,lambda) . ,actuals) `(,lambda . ,actuals))
     (let* ((formals (nth 1 lambda))
            (body (cdr (macroexp-parse-body (cddr lambda))))
            optionalp restp
            (dynboundarg nil)
            bindings)
       ;; FIXME: The checks below do not belong in an optimization phase.
       (while formals
         (if (macroexp--dynamic-variable-p (car formals))
             (setq dynboundarg t))
         (cond ((eq (car formals) '&optional)
                ;; ok, I'll let this slide because funcall_lambda() does...
                ;; (if optionalp (error "Multiple &optional keywords in %s" name))
                (if restp (error "&optional found after &rest in %s" name))
                (if (null (cdr formals))
                    (error "Nothing after &optional in %s" name))
                (setq optionalp t))
               ((eq (car formals) '&rest)
                ;; ...but it is by no stretch of the imagination a reasonable
                ;; thing that funcall_lambda() allows (&rest x y) and
                ;; (&rest x &optional y) in formalss.
                (if (null (cdr formals))
                    (error "Nothing after &rest in %s" name))
                (if (cdr (cdr formals))
                    (error "Multiple vars after &rest in %s" name))
                (setq restp t))
               (restp
                (setq bindings (cons (list (car formals)
                                           (and actuals (cons 'list actuals)))
                                     bindings)
                      actuals nil))
               ((and (not optionalp) (null actuals))
                (setq formals nil actuals 'too-few))
               (t
                (setq bindings (cons (list (car formals) (car actuals))
                                     bindings)
                      actuals (cdr actuals))))
         (setq formals (cdr formals)))
       (cond
        (actuals
         (macroexp-warn-and-return
          (format-message
           (if (eq actuals 'too-few)
               "attempt to open-code `%s' with too few arguments"
             "attempt to open-code `%s' with too many arguments")
           name)
          form nil nil formals))
        ;; In lexical-binding mode, let and functions don't bind vars in
        ;; the same way (let obey special-variable-p, but functions
        ;; don't).  So if one of the vars is declared as dynamically scoped, we
        ;; can't just convert the call to `let'.
        ;; FIXME: We should α-rename the affected args and then use `let'.
        (dynboundarg form)
        (bindings `(let ,(nreverse bindings) . ,body))
        (t (macroexp-progn body)))))
    (_ (error "Not an unfoldable form: %S" form))))

(defun macroexp--dynamic-variable-p (var)
  "Whether the variable VAR is dynamically scoped.
Only valid during macro-expansion."
  (or (not lexical-binding)
      (special-variable-p var)
      (memq var macroexp--dynvars)
      (and (boundp 'byte-compile-bound-variables)
           (memq var byte-compile-bound-variables))))

(defun macroexp--expand-all (form)
  "Expand all macros in FORM.
This is an internal version of `macroexpand-all'.
Assumes the caller has bound `macroexpand-all-environment'."
  ;; Note that this function must preserve any position on FORM in the
  ;; function's return value.  See the page "Symbols with Position" in
  ;; the elisp manual.
  (macroexp--with-extended-form-stack form
      (if (eq (car-safe form) 'backquote-list*)
          ;; Special-case `backquote-list*', as it is normally a macro that
          ;; generates exceedingly deep expansions from relatively shallow input
          ;; forms.  We just process it `in reverse' -- first we expand all the
          ;; arguments, _then_ we expand the top-level definition.
          (macroexpand (macroexp--all-forms form 1)
		       macroexpand-all-environment)
        ;; Normal form; get its expansion, and then expand arguments.
        (setq form (macroexp-macroexpand form macroexpand-all-environment))
        ;; FIXME: It'd be nice to use `byte-optimize--pcase' here, but when
        ;; I tried it, it broke the bootstrap :-(
        (let ((fn (car-safe form)))
          (pcase form
            (`(cond . ,clauses)
             ;; Check for rubbish clauses at the end before macro-expansion,
             ;; to avoid nuisance warnings from clauses that become
             ;; unconditional through that process.
             ;; FIXME: this strategy is defeated by forced `macroexpand-all',
             ;; such as in `cl-flet'.  Haven't seen that in the wild, though.
             (let ((default-tail nil)
                   (n 0)
                   (rest clauses))
               (while (cdr rest)
                 (let ((c (car-safe (car rest))))
                   (when (cond ((consp c) (and (memq (car c) '(quote function))
                                               (cadr c)))
                               ((symbolp c) (or (eq c t) (keywordp c)))
                               (t t))
                     ;; This is unquestionably a default clause.
                     (setq default-tail (cdr rest))
                     (setq clauses (take (1+ n) clauses))  ; trim the tail
                     (setq rest nil)))
                 (setq n (1+ n))
                 (setq rest (cdr rest)))
               (let ((expanded-form
                      (macroexp--cons fn (macroexp--all-clauses clauses) form)))
                 (if default-tail
                     (macroexp-warn-and-return
                      (format-message
                       "Useless clause following default `cond' clause")
                      expanded-form '(suspicious cond) t default-tail)
                   expanded-form))))
            (`(condition-case . ,(or `(,err ,body . ,handlers) pcase--dontcare))
             (let ((exp-body (macroexp--expand-all body)))
               (if handlers
                   (macroexp--cons fn
                                   (macroexp--cons
                                    err (macroexp--cons
                                         exp-body
                                         (macroexp--all-clauses handlers 1)
                                         (cddr form))
                                    (cdr form))
                                   form)
                 (macroexp-warn-and-return
                  (format-message "`condition-case' without handlers")
                  exp-body (list 'suspicious 'condition-case) t form))))
            (`(,(or 'defvar 'defconst) . ,args)
             (if (and (car-safe args) (symbolp (car-safe args)))
                 (progn
                   (push (car args) macroexp--dynvars)
                   (macroexp--all-forms form 2))
               form))
            (`(function . ,rest)
             (if (and (eq (car-safe (car-safe rest)) 'lambda)
                      (null (cdr rest)))
                 (let ((f (car rest)))
                   (let ((macroexp--dynvars macroexp--dynvars))
                     (macroexp--cons fn
                                     (macroexp--cons (macroexp--all-forms f 2)
                                                     nil
                                                     (cdr form))
                                     form)))
               form))
            (`(,(or 'function 'quote) . ,_) form)
            (`(,(and fun (or 'let 'let*)) . ,(or `(,bindings . ,body)
                                                 pcase--dontcare))
             (let ((macroexp--dynvars macroexp--dynvars))
               (macroexp--cons
                fun
                (macroexp--cons
                 (macroexp--all-clauses bindings 1)
                 (if (null body)
                     (macroexp-unprogn
                      (macroexp-warn-and-return
                       (format-message "`%s' with empty body" fun)
                       nil (list 'empty-body fun) 'compile-only fun))
                   (macroexp--all-forms body))
                 (cdr form))
                form)))
            (`(while . ,args)
             (if args
                 (macroexp--all-forms form 1)
               (macroexp-warn-and-return
                (format-message "missing `while' condition")
                `(signal 'wrong-number-of-arguments '(while 0))
                nil 'compile-only form)))
            (`(unwind-protect . ,args)
             (if (cdr-safe args)
                 (macroexp--all-forms form 1)
               (macroexp-warn-and-return
                (format-message "`unwind-protect' without unwind forms")
                (macroexp--expand-all (car-safe args))
                (list 'suspicious 'unwind-protect) t form)))
            (`(setq . ,args)
             (let ((nargs (length args))
                   (var (car-safe args)))
               (if (and (= nargs 2)
                        (symbolp var)
                        (not (booleanp var)) (not (keywordp var)))
                   ;; Fast path for the common case.
                   (let* ((expr (nth 1 args))
                          (new-expr (macroexp--expand-all expr)))
                     (if (eq new-expr expr)
                         form
                       `(,fn ,var ,new-expr)))
                 ;; Normalize to a sequence of (setq SYM EXPR).
                 ;; Malformed code is translated to code that signals an error
                 ;; at run time.
                 (if (oddp nargs)
                     (macroexp-warn-and-return
                      (format-message "odd number of arguments in `setq' form")
                      `(signal 'wrong-number-of-arguments '(setq ,nargs))
                      nil 'compile-only fn)
                   (let ((assignments nil))
                     (while (consp (cdr-safe args))
                       (let* ((var (car args))
                              (expr (cadr args))
                              (new-expr (macroexp--expand-all expr))
                              (assignment
                               (if (and (symbolp var)
                                        (not (booleanp var))
                                        (not (keywordp var)))
                                   `(,fn ,var ,new-expr)
                                 (macroexp-warn-and-return
                                  (format-message "attempt to set %s `%s'"
                                                  (if (symbolp var)
                                                      "constant"
                                                    "non-variable")
                                                  var)
                                  (cond
                                   ((keywordp var)
                                    ;; Accept `(setq :a :a)' for compatibility.
                                    ;; FIXME: Why, exactly? It's useless.
                                    `(if (eq ,var ,new-expr)
                                         ,var
                                       (signal 'setting-constant (list ',var))))
                                   ((symbolp var)
                                    `(signal 'setting-constant (list ',var)))
                                   (t
                                    `(signal 'wrong-type-argument
                                             (list 'symbolp ',var))))
                                  nil 'compile-only var))))
                         (push assignment assignments))
                       (setq args (cddr args)))
                     (cons 'progn (nreverse assignments)))))))
            (`(funcall ,exp . ,args)
             (let ((eexp (macroexp--expand-all exp))
                   (eargs (macroexp--all-forms args)))
               (if (eq (car-safe eexp) 'function)
                   (let ((f (cadr eexp)))
                     (cond
                      ;; Rewrite (funcall #'foo bar) to (foo bar), in case `foo'
                      ;; has a compiler-macro, or to unfold it.
                      ((and (symbolp f)
                            ;; bug#46636
                            (not (or (special-form-p f) (macrop f))))
                       (macroexp--expand-all `(,f . ,eargs)))
                      ((eq (car-safe f) 'lambda)
                       (macroexp--unfold-lambda `(,fn ,eexp . ,eargs)))
                      (t `(,fn ,eexp . ,eargs))))
                 `(,fn ,eexp . ,eargs))))
            (`(funcall . ,_) form)      ;bug#53227
            (`(,(and func (pred symbolp)) . ,_)
             (let ((handler (function-get func 'compiler-macro)))
               ;; Macro expand compiler macros.  This cannot be delayed to
               ;; byte-optimize-form because the output of the compiler-macro can
               ;; use macros.
               (if (null handler)
                   ;; No compiler macro.  We just expand each argument.
                   (macroexp--all-forms form 1)
                 ;; If the handler is not loaded yet, try (auto)loading the
                 ;; function itself, which may in turn load the handler.
                 (unless (functionp handler)
                   (with-demoted-errors "macroexp--expand-all: %S"
                     (autoload-do-load (indirect-function func) func)))
                 (let ((newform (macroexp--compiler-macro handler form)))
                   (if (eq form newform)
                       ;; The compiler macro did not find anything to do.
                       (if (equal form (setq newform (macroexp--all-forms form 1)))
                           form
                         ;; Maybe after processing the args, some new opportunities
                         ;; appeared, so let's try the compiler macro again.
                         (setq form (macroexp--compiler-macro handler newform))
                         (if (eq newform form)
                             newform
                           (macroexp--expand-all form)))
                     (macroexp--expand-all newform))))))
            (`(,(and fun `(lambda . ,_)) . ,args)
	     (macroexp--cons (macroexp--all-forms fun 2)
                             (macroexp--all-forms args)
                             form))
            (_ form))))))

;;;###autoload
(defun macroexpand-all (form &optional environment)
  "Return result of expanding macros at all levels in FORM.
If no macros are expanded, FORM is returned unchanged.
The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation."
  (let ((macroexpand-all-environment environment)
        (macroexp--dynvars macroexp--dynvars))
    (macroexp--expand-all form)))

;; This function is like `macroexpand-all' but for use with top-level
;; forms.  It does not dynbind `macroexp--dynvars' because we want
;; top-level `defvar' declarations to be recorded in that variable.
(defun macroexpand--all-toplevel (form &optional environment)
  (let ((macroexpand-all-environment environment))
    (macroexp--expand-all form)))

;;; Handy functions to use in macros.

(defun macroexp-parse-body (body)
  "Parse a function BODY into (DECLARATIONS . EXPS)."
  (let ((decls ()))
    (while
        (and body
             (let ((e (car body)))
               (or (and (stringp e)
                        ;; If there is only a string literal with
                        ;; nothing following, we consider this to be
                        ;; part of the body (the return value) rather
                        ;; than a declaration at this point.
                        (cdr body))
                   (memq (car-safe e)
                         '(:documentation declare interactive cl-declare)))))
      (push (pop body) decls))
    (cons (nreverse decls) body)))

(defun macroexp-progn (exps)
  "Return EXPS (a list of expressions) with `progn' prepended.
If EXPS is a list with a single expression, `progn' is not
prepended, but that expression is returned instead."
  (if (cdr exps) `(progn ,@exps) (car exps)))

(defun macroexp-unprogn (exp)
  "Turn EXP into a list of expressions to execute in sequence.
Never returns an empty list."
  (if (eq (car-safe exp) 'progn) (or (cdr exp) '(nil)) (list exp)))

(defun macroexp-let* (bindings exp)
  "Return an expression equivalent to \\=`(let* ,BINDINGS ,EXP)."
  (cond
   ((null bindings) exp)
   ((eq 'let* (car-safe exp)) `(let* (,@bindings ,@(cadr exp)) ,@(cddr exp)))
   (t `(let* ,bindings ,exp))))

(defun macroexp-if (test then else)
  "Return an expression equivalent to \\=`(if ,TEST ,THEN ,ELSE)."
  (cond
   ((eq (car-safe else) 'if)
    (cond
     ;; Drop this optimization: It's unsafe (it assumes that `test' is
     ;; pure, or at least idempotent), and it's not used even a single
     ;; time while compiling Emacs's sources.
     ;;((equal test (nth 1 else))
     ;; ;; Doing a test a second time: get rid of the redundancy.
     ;; (message "macroexp-if: sharing 'test' %S" test)
     ;; `(if ,test ,then ,@(nthcdr 3 else)))
     ((equal then (nth 2 else))
      ;; (message "macroexp-if: sharing 'then' %S" then)
      `(if (or ,test ,(nth 1 else)) ,then ,@(nthcdr 3 else)))
     ((equal (macroexp-unprogn then) (nthcdr 3 else))
      ;; (message "macroexp-if: sharing 'then' with not %S" then)
      `(if (or ,test (not ,(nth 1 else)))
           ,then ,@(macroexp-unprogn (nth 2 else))))
     (t
      `(cond (,test ,@(macroexp-unprogn then))
             (,(nth 1 else) ,@(macroexp-unprogn (nth 2 else)))
             ,@(let ((def (nthcdr 3 else))) (if def `((t ,@def))))))))
   ((eq (car-safe else) 'cond)
    `(cond (,test ,@(macroexp-unprogn then)) ,@(cdr else)))
   ;; Invert the test if that lets us reduce the depth of the tree.
   ((memq (car-safe then) '(if cond)) (macroexp-if `(not ,test) else then))
   (t `(if ,test ,then ,@(if else (macroexp-unprogn else))))))

(defmacro macroexp-let2 (test sym exp &rest body)
  "Evaluate BODY with SYM bound to an expression for EXP's value.
The intended usage is that BODY generates an expression that
will refer to EXP's value multiple times, but will evaluate
EXP only once.  As BODY generates that expression, it should
use SYM to stand for the value of EXP.

If EXP is a simple, safe expression, then SYM's value is EXP itself.
Otherwise, SYM's value is a symbol which holds the value produced by
evaluating EXP.  The return value incorporates the value of BODY, plus
additional code to evaluate EXP once and save the result so SYM can
refer to it.

If BODY consists of multiple forms, they are all evaluated
but only the last one's value matters.

TEST is a predicate to determine whether EXP qualifies as simple and
safe; if TEST is nil, only constant expressions qualify.

Example:
 (macroexp-let2 nil foo EXP
   \\=`(* ,foo ,foo))
generates an expression that evaluates EXP once,
then returns the square of that value.
You could do this with
  (let ((foovar EXP))
    (* foovar foovar))
but using `macroexp-let2' produces more efficient code in
cases where EXP is a constant."
  (declare (indent 3) (debug (sexp sexp form body)))
  (let ((bodysym (make-symbol "body"))
        (expsym (make-symbol "exp")))
    `(let* ((,expsym ,exp)
            (,sym (if (funcall #',(or test #'macroexp-const-p) ,expsym)
                      ,expsym (make-symbol ,(symbol-name sym))))
            (,bodysym ,(macroexp-progn body)))
       (if (eq ,sym ,expsym) ,bodysym
         (macroexp-let* (list (list ,sym ,expsym))
                        ,bodysym)))))

(defmacro macroexp-let2* (test bindings &rest body)
  "Multiple binding version of `macroexp-let2'.

BINDINGS is a list of elements of the form (SYM EXP) or just SYM,
which then stands for (SYM SYM).
Each EXP can refer to symbols specified earlier in the binding list.

TEST has to be a symbol, and if it is nil it can be omitted."
  (declare (indent 2) (debug (sexp (&rest (sexp form)) body)))
  (when (consp test) ;; `test' was omitted.
    (push bindings body)
    (setq bindings test)
    (setq test nil))
  (pcase-exhaustive bindings
    ('nil (macroexp-progn body))
    (`(,(or `(,var ,exp) (and (pred symbolp) var (let exp var)))
       . ,tl)
     `(macroexp-let2 ,test ,var ,exp
        (macroexp-let2* ,test ,tl ,@body)))))

(defun macroexp--maxsize (exp size)
  (cond ((< size 0) size)
        ((symbolp exp) (1- size))
        ((stringp exp) (- size (/ (length exp) 16)))
        ((vectorp exp)
         (dotimes (i (length exp))
           (setq size (macroexp--maxsize (aref exp i) size)))
         (1- size))
        ((consp exp)
         ;; We could try to be more clever with quote&function,
         ;; but it is difficult to do so correctly, and it's not obvious that
         ;; it would be worth the effort.
         (dolist (e exp)
           (setq size (macroexp--maxsize e size)))
         (1- size))
        (t -1)))

(defun macroexp-small-p (exp)
  "Return non-nil if EXP can be considered small."
  (> (macroexp--maxsize exp 10) 0))

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

(defun macroexp-quote (v)
  "Return an expression E such that `(eval E)' is V.

E is either V or (quote V) depending on whether V evaluates to
itself or not."
  (if (and (not (consp v))
	   (or (keywordp v)
	       (not (symbolp v))
	       (memq v '(nil t))))
      v
    (list 'quote v)))

(defun macroexp--fgrep (bindings sexp)
  "Return those of the BINDINGS which might be used in SEXP.
It is used as a poor-man's \"free variables\" test.  It differs from a true
test of free variables in the following ways:
- It does not distinguish variables from functions, so it can be used
  both to detect whether a given variable is used by SEXP and to
  detect whether a given function is used by SEXP.
- It does not actually know Elisp syntax, so it only looks for the presence
  of symbols in SEXP and can't distinguish if those symbols are truly
  references to the given variable (or function).  That can make the result
  include bindings which actually aren't used.
- For the same reason it may cause the result to fail to include bindings
  which will be used if SEXP is not yet fully macro-expanded and the
  use of the binding will only be revealed by macro expansion."
  (let ((res '())
        ;; Cyclic code should not happen, but code can contain cyclic data :-(
        (seen (make-hash-table :test #'eq))
        (sexpss (list (list sexp))))
    ;; Use a nested while loop to reduce the amount of heap allocations for
    ;; pushes to `sexpss' and the `gethash' overhead.
    (while (and sexpss bindings)
      (let ((sexps (pop sexpss)))
        (unless (gethash sexps seen)
          (puthash sexps t seen) ;; Using `setf' here causes bootstrap problems.
          (if (vectorp sexps) (setq sexps (mapcar #'identity sexps)))
          (let ((tortoise sexps) (skip t))
            (while sexps
              (let ((sexp (if (consp sexps) (pop sexps)
                            (prog1 sexps (setq sexps nil)))))
                (if skip
                    (setq skip nil)
                  (setq tortoise (cdr tortoise))
                  (if (eq tortoise sexps)
                      (setq sexps nil) ;; Found a cycle: we're done!
                    (setq skip t)))
                (cond
                 ((or (consp sexp) (vectorp sexp)) (push sexp sexpss))
                 (t
                  (let ((tmp (assq sexp bindings)))
                    (when tmp
                      (push tmp res)
                      (setq bindings (remove tmp bindings))))))))))))
    res))

;;; Load-time macro-expansion.

;; Because macro-expansion used to be more lazy, eager macro-expansion
;; tends to bump into previously harmless/unnoticeable cyclic-dependencies.
;; So, we have to delay macro-expansion like we used to when we detect
;; such a cycle, and we also want to help coders resolve those cycles (since
;; they can be non-obvious) by providing a usefully trimmed backtrace
;; (hopefully) highlighting the problem.

(defun macroexp--backtrace ()
  "Return the Elisp backtrace, more recent frames first."
  (let ((bt ())
        (i 0))
    (while
        (let ((frame (backtrace-frame i)))
          (when frame
            (push frame bt)
            (setq i (1+ i)))))
    (nreverse bt)))

(defun macroexp--trim-backtrace-frame (frame)
  (pcase frame
    (`(,_ macroexpand (,head . ,_) . ,_) `(macroexpand (,head …)))
    (`(,_ internal-macroexpand-for-load (,head ,second . ,_) . ,_)
     (if (or (symbolp second)
             (and (eq 'quote (car-safe second))
                  (symbolp (cadr second))))
         `(macroexpand-all (,head ,second …))
       '(macroexpand-all …)))
    (`(,_ load-with-code-conversion ,name . ,_)
     `(load ,(file-name-nondirectory name)))))

(defvar macroexp--pending-eager-loads nil
  "Stack of files currently undergoing eager macro-expansion.")

(defvar macroexp--debug-eager nil)

(defun internal-macroexpand-for-load (form full-p)
  ;; Called from the eager-macroexpansion in readevalloop.
  (cond
   ;; Don't repeat the same warning for every top-level element.
   ((eq 'skip (car macroexp--pending-eager-loads)) form)
   ;; If we detect a cycle, skip macro-expansion for now, and output a warning
   ;; with a trimmed backtrace.
   ((and load-file-name (member load-file-name macroexp--pending-eager-loads))
    (let* ((bt (delq nil
                     (mapcar #'macroexp--trim-backtrace-frame
                             (macroexp--backtrace))))
           (elem `(load ,(file-name-nondirectory load-file-name)))
           (tail (member elem (cdr (member elem bt)))))
      (if tail (setcdr tail (list '…)))
      (if (eq (car-safe (car bt)) 'macroexpand-all) (setq bt (cdr bt)))
      (if macroexp--debug-eager
          (debug 'eager-macroexp-cycle)
        (error "Eager macro-expansion skipped due to cycle:\n  %s"
               (mapconcat #'prin1-to-string (nreverse bt) " => ")))
      (push 'skip macroexp--pending-eager-loads)
      form))
   (t
    (condition-case err
        (let ((macroexp--pending-eager-loads
               (cons load-file-name macroexp--pending-eager-loads)))
          (if full-p
              (macroexpand--all-toplevel form)
            (macroexpand form)))
      ((debug error)
       ;; Hopefully this shouldn't happen thanks to the cycle detection,
       ;; but in case it does happen, let's catch the error and give the
       ;; code a chance to macro-expand later.
       (error "Eager macro-expansion failure: %S" err)
       form)))))

;; ¡¡¡ Big Ugly Hack !!!
;; src/bootstrap-emacs is mostly used to compile .el files, so it needs
;; macroexp, bytecomp, cconv, and byte-opt to be fast.  Generally this is done
;; by compiling those files first, but this only makes a difference if those
;; files are not preloaded.  But macroexp.el is preloaded so we reload it if
;; the current version is interpreted and there's a compiled version available.
(eval-when-compile
  (add-hook 'emacs-startup-hook
            (lambda ()
              (and (not (compiled-function-p
                         (symbol-function 'macroexpand-all)))
                   (locate-library "macroexp.elc")
                   (load "macroexp.elc")))))

(provide 'macroexp)

;;; macroexp.el ends here
