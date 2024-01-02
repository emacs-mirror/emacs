;;; cconv.el --- Closure conversion for statically scoped Emacs Lisp. -*- lexical-binding: t -*-

;; Copyright (C) 2011-2024 Free Software Foundation, Inc.

;; Author: Igor Kuzmin <kzuminig@iro.umontreal.ca>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: lisp
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

;; This takes a piece of Elisp code, and eliminates all free variables from
;; lambda expressions.  The user entry point is `cconv-closure-convert'.
;; All macros should be expanded beforehand.
;;
;; Here is a brief explanation how this code works.
;; Firstly, we analyze the tree by calling `cconv-analyze-form'.
;; This function finds all mutated variables, all functions that are suitable
;; for lambda lifting and all variables captured by closure.  It passes the tree
;; once, returning a list of three lists.
;;
;; Then we calculate the intersection of the first and third lists returned by
;; `cconv-analyze-form' to find all mutated variables that are captured by
;; closure.

;; Armed with this data, we call `cconv-convert', that rewrites the
;; tree recursively, lifting lambdas where possible, building closures where it
;; is needed and eliminating mutable variables used in closure.
;;
;; We do following replacements :
;; (lambda (v1 ...) ... fv1 fv2 ...) => (lambda (v1 ... fv1 fv2 ) ... fv1 fv2 .)
;; if the function is suitable for lambda lifting (if all calls are known)
;;
;; (lambda (v0 ...) ... fv0 .. fv1 ...)  =>
;; (internal-make-closure (v0 ...) (fv0 ...) <doc>
;;   ... (internal-get-closed-var 0) ...  (internal-get-closed-var 1) ...)
;;
;; If the function has no free variables, we don't do anything.
;;
;; If a variable is mutated (updated by setq), and it is used in a closure
;; we wrap its definition with list: (list val) and we also replace
;; var => (car-safe var) wherever this variable is used, and also
;; (setq var value) => (setcar var value) where it is updated.
;;
;; If defun argument is closure mutable, we letbind it and wrap it's
;; definition with list.
;; (defun foo (... mutable-arg ...) ...) =>
;; (defun foo (... m-arg ...) (let ((m-arg (list m-arg))) ...))
;;
;;; Code:

;; TODO: (not just for cconv but also for the lexbind changes in general)
;; - let (e)debug find the value of lexical variables from the stack.
;; - make eval-region do the eval-sexp-add-defvars dance.
;; - byte-optimize-form should be applied before cconv.
;;   OTOH, the warnings emitted by cconv-analyze need to come before optimize
;;   since afterwards they can become obnoxious (warnings about an "unused
;;   variable" should not be emitted when the variable use has simply been
;;   optimized away).
;; - let macros specify that some let-bindings come from the same source,
;;   so the unused warning takes all uses into account.
;; - let interactive specs return a function to build the args (to stash into
;;   command-history).
;; - canonize code in macro-expand so we don't have to handle (let (var) body)
;;   and other oddities.
;; - new byte codes for unwind-protect so that closures aren't needed at all.
;; - Since we know here when a variable is not mutated, we could pass that
;;   info to the byte-compiler, e.g. by using a new `immutable-let'.
;; - call known non-escaping functions with `goto' rather than `call'.

(eval-when-compile (require 'cl-lib))

(defconst cconv-liftwhen 6
  "Try to do lambda lifting if the number of arguments + free variables
is less than this number.")
(defvar cconv-var-classification
  ;; Alist mapping variables to a given class.
  ;; The keys are of the form (BINDER . PARENTFORM) where BINDER
  ;; is the (VAR VAL) that introduces it (or is just (VAR) for variables
  ;; not introduced by let).
  ;; The class can be one of:
  ;; - :unused
  ;; - :lambda-candidate
  ;; - :captured+mutated
  ;; - nil for "normal" variables, which would then just not appear
  ;;   in the alist at all.
  )

(defvar cconv-freevars-alist
  ;; Alist associating to each function body the list of its free variables.
  )

(defvar cconv--interactive-form-funs
  ;; Table used to hold the functions we create internally for
  ;; interactive forms.
  (make-hash-table :test #'eq :weakness 'key))

(defvar cconv--dynbound-variables nil
  "List of variables known to be dynamically bound.")

;;;###autoload
(defun cconv-closure-convert (form &optional dynbound-vars)
  "Main entry point for closure conversion.
FORM is a piece of Elisp code after macroexpansion.
DYNBOUND-VARS is a list of symbols that should be considered as
using dynamic scoping.

Returns a form where all lambdas don't have any free variables."
  (let ((cconv--dynbound-variables dynbound-vars)
	(cconv-freevars-alist '())
	(cconv-var-classification '()))
    ;; Analyze form - fill these variables with new information.
    (cconv-analyze-form form '())
    (setq cconv-freevars-alist (nreverse cconv-freevars-alist))
    (prog1 (cconv-convert form nil nil) ; Env initially empty.
      (cl-assert (null cconv-freevars-alist)))))

(defun cconv--set-diff (s1 s2)
  "Return elements of set S1 that are not in set S2."
  (let ((res '()))
    (dolist (x s1)
      (unless (memq x s2) (push x res)))
    (nreverse res)))

(defun cconv--set-diff-map (s m)
  "Return elements of set S that are not in Dom(M)."
  (let ((res '()))
    (dolist (x s)
      (unless (assq x m) (push x res)))
    (nreverse res)))

(defun cconv--map-diff (m1 m2)
  "Return the submap of map M1 that has Dom(M2) removed."
  (let ((res '()))
    (dolist (x m1)
      (unless (assq (car x) m2) (push x res)))
    (nreverse res)))

(defun cconv--map-diff-elem (m x)
  "Return the map M minus any mapping for X."
  ;; Here we assume that X appears at most once in M.
  (let* ((b (assq x m))
         (res (if b (remq b m) m)))
    (cl-assert (null (assq x res))) ;; Check the assumption was warranted.
    res))

(defun cconv--map-diff-set (m s)
  "Return the map M minus any mapping for elements of S."
  ;; Here we assume that X appears at most once in M.
  (let ((res '()))
    (dolist (b m)
      (unless (memq (car b) s) (push b res)))
    (nreverse res)))

(defun cconv--convert-function (args body env parentform &optional docstring)
  (cl-assert (equal body (caar cconv-freevars-alist)))
  (let* ((fvs (cdr (pop cconv-freevars-alist)))
         (body-new '())
         (envector ())
         (i 0)
         (new-env ()))
    ;; Build the "formal and actual envs" for the closure-converted function.
    ;; Hack for OClosure: `nreverse' here intends to put the captured vars
    ;; in the closure such that the first one is the one that is bound
    ;; most closely.
    (dolist (fv (nreverse fvs))
      (let ((exp (or (cdr (assq fv env)) fv)))
        (pcase exp
          ;; If `fv' is a variable that's wrapped in a cons-cell,
          ;; we want to put the cons-cell itself in the closure,
          ;; rather than just a copy of its current content.
          (`(car-safe ,iexp . ,_)
           (push iexp envector)
           (push `(,fv . (car-safe (internal-get-closed-var ,i))) new-env))
          (_
           (push exp envector)
           (push `(,fv . (internal-get-closed-var ,i)) new-env))))
      (setq i (1+ i)))
    (setq envector (nreverse envector))
    (setq new-env (nreverse new-env))

    (setq body-new (cconv--convert-funcbody
                     args body new-env parentform))
    (cond
     ((not (or envector docstring))     ;If no freevars - do nothing.
      `(function (lambda ,args . ,body-new)))
     (t
      `(internal-make-closure
        ,args ,envector ,docstring . ,body-new)))))

(defun cconv--remap-llv (new-env var closedsym)
  ;; In a case such as:
  ;;   (let* ((fun (lambda (x) (+ x y))) (y 1)) (funcall fun 1))
  ;; A naive lambda-lifting would return
  ;;   (let* ((fun (lambda (y x) (+ x y))) (y 1)) (funcall fun y 1))
  ;; Where the external `y' is mistakenly captured by the inner one.
  ;; So when we detect that case, we rewrite it to:
  ;;   (let* ((closed-y y) (fun (lambda (y x) (+ x y))) (y 1))
  ;;     (funcall fun closed-y 1))
  ;; We do that even if there's no `funcall' that uses `fun' in the scope
  ;; where `y' is shadowed by another variable because, to treat
  ;; this case better, we'd need to traverse the tree one more time to
  ;; collect this data, and I think that it's not worth it.
  (mapcar (lambda (mapping)
            (if (not (eq (cadr mapping) #'apply-partially))
                mapping
              (cl-assert (eq (car mapping) (nth 2 mapping)))
              `(,(car mapping)
                apply-partially
                ,(car mapping)
                ,@(mapcar (lambda (arg)
                            (if (eq var arg)
                                closedsym arg))
                          (nthcdr 3 mapping)))))
          new-env))

(defun cconv--warn-unused-msg (var varkind)
  (unless (or ;; Uninterned symbols typically come from macro-expansion, so
              ;; it is often non-trivial for the programmer to avoid such
              ;; unused vars.
              (not (intern-soft var))
              (eq ?_ (aref (symbol-name var) 0)))
       (let ((suggestions (help-uni-confusable-suggestions (symbol-name var))))
         (format "Unused lexical %s `%S'%s"
                 varkind (bare-symbol var)
                 (if suggestions (concat "\n  " suggestions) "")))))

(define-inline cconv--var-classification (binder form)
  (inline-quote
   (cdr (assoc (cons ,binder ,form) cconv-var-classification))))

(defun cconv--convert-funcbody (funargs funcbody env parentform)
  "Run `cconv-convert' on FUNCBODY, the forms of a lambda expression.
PARENTFORM is the form containing the lambda expression.  ENV is a
lexical environment (same format as for `cconv-convert'), not
including FUNARGS, the function's argument list.  Return a list
of converted forms."
  (let ((wrappers ()))
    (dolist (arg funargs)
      (pcase (cconv--var-classification (list arg) parentform)
        (:captured+mutated
         (push `(,arg . (car-safe ,arg)) env)
         (push (lambda (body) `(let ((,arg (list ,arg))) ,body)) wrappers))
        ((and :unused
              (let (and (pred stringp) msg)
                (cconv--warn-unused-msg arg "argument")))
         (if (assq arg env) (push `(,arg . nil) env)) ;FIXME: Is it needed?
         (push (lambda (body) (macroexp--warn-wrap arg msg body 'lexical)) wrappers))
        (_
         (if (assq arg env) (push `(,arg . nil) env)))))
    (setq funcbody (mapcar (lambda (form)
                             (cconv-convert form env nil))
                           funcbody))
    (if wrappers
        (pcase-let ((`(,decls . ,body) (macroexp-parse-body funcbody)))
          (let ((body (macroexp-progn body)))
            (dolist (wrapper wrappers) (setq body (funcall wrapper body)))
            `(,@decls ,@(macroexp-unprogn body))))
      funcbody)))

(defun cconv--lifted-arg (var env)
  "The argument to use for VAR in λ-lifted calls according to ENV.
This is used when VAR is being shadowed; we may still need its value for
such calls."
  (let ((mapping (cdr (assq var env))))
    (pcase-exhaustive mapping
      (`(internal-get-closed-var . ,_)
       ;; The variable is captured.
       mapping)
      (`(car-safe ,exp)
       ;; The variable is mutably captured; skip
       ;; the indirection step because the variable is
       ;; passed "by reference" to the λ-lifted function.
       exp)
      (_
       ;; The variable is not captured; use the (shadowed) variable value.
       ;; (If the mapping is `(car-safe SYMBOL)', SYMBOL is always VAR.
       var))))

(defun cconv-convert (form env extend)
  ;; This function actually rewrites the tree.
  "Return FORM with all its lambdas changed so they are closed.
ENV is a lexical environment mapping variables to the expression
used to get its value.  This is used for variables that are copied into
closures, moved into cons cells, ...
ENV is a list where each entry takes the shape either:
 (VAR . (car-safe EXP)): VAR has been moved into the car of a cons-cell, and EXP
    is an expression that evaluates to this cons-cell.
 (VAR . (internal-get-closed-var N)): VAR has been copied into the closure
    environment's Nth slot.
 (VAR . (apply-partially F ARG1 ARG2 ..)): VAR has been λ-lifted and takes
    additional arguments ARGs.
 (VAR . nil): VAR is accessed normally.  This is the same as VAR
    being absent from ENV, but an explicit nil entry is useful
    for shadowing VAR for a specific scope.
EXTEND is a list of variables which might need to be accessed even from places
where they are shadowed, because some part of ENV causes them to be used at
places where they originally did not directly appear."
  (cl-assert (not (delq nil (mapcar (lambda (mapping)
                                      (if (eq (cadr mapping) #'apply-partially)
                                          (cconv--set-diff (cdr (cddr mapping))
                                                           extend)))
                                    env))))

  ;; What's the difference between fvrs and envs?
  ;; Suppose that we have the code
  ;; (lambda (..) fvr (let ((fvr 1)) (+ fvr 1)))
  ;; only the first occurrence of fvr should be replaced by
  ;; (aref env ...).
  ;; So initially envs and fvrs are the same thing, but when we descend to
  ;; the 'let, we delete fvr from fvrs. Why we don't delete fvr from envs?
  ;; Because in envs the order of variables is important. We use this list
  ;; to find the number of a specific variable in the environment vector,
  ;; so we never touch it(unless we enter to the other closure).
  ;;(if (listp form) (print (car form)) form)
  (pcase form
    (`(,(and letsym (or 'let* 'let)) ,binders . ,body)

					; let and let* special forms
     (let ((binders-new '())
           (new-env env)
           (new-extend extend))

       (dolist (binder binders)
         (let* ((value nil)
		(var (if (not (consp binder))
			 (prog1 binder (setq binder (list binder)))
                       (when (cddr binder)
                         (byte-compile-warn-x
                          binder
                          "Malformed `%S' binding: %S"
                          letsym binder))
		       (setq value (cadr binder))
		       (car binder))))
           (cond
            ;; Ignore bindings without a valid name.
            ((not (symbolp var))
             (byte-compile-warn-x var "attempt to let-bind nonvariable `%S'" var))
            ((or (booleanp var) (keywordp var))
             (byte-compile-warn-x var "attempt to let-bind constant `%S'" var))
            (t
             (let ((new-val
		    (pcase (cconv--var-classification binder form)
                      ;; Check if var is a candidate for lambda lifting.
                      ((and :lambda-candidate
                            (guard
                             (progn
                               (cl-assert (and (eq (car value) 'function)
                                               (eq (car (cadr value)) 'lambda)))
                               (cl-assert (equal (cddr (cadr value))
                                                 (caar cconv-freevars-alist)))
                               ;; Peek at the freevars to decide whether
                               ;; to λ-lift.
                               (let* ((fvs (cdr (car cconv-freevars-alist)))
                                      (fun (cadr value))
                                      (funargs (cadr fun))
                                      (funcvars (append fvs funargs)))
					; lambda lifting condition
                                 (and fvs (>= cconv-liftwhen
					      (length funcvars)))))))
					; Lift.
                       (let* ((fvs (cdr (pop cconv-freevars-alist)))
                              (fun (cadr value))
                              (funargs (cadr fun))
                              (funcvars (append fvs funargs))
                              (funcbody (cddr fun))
                              (funcbody-env ()))
                         (push `(,var . (apply-partially ,var . ,fvs)) new-env)
                         (dolist (fv fvs)
                           (cl-pushnew fv new-extend)
                           (if (and (eq 'car-safe (car-safe
                                                   (cdr (assq fv env))))
                                    (not (memq fv funargs)))
                               (push `(,fv . (car-safe ,fv)) funcbody-env)))
                         `(function (lambda ,funcvars .
                                      ,(cconv--convert-funcbody
                                        funargs funcbody funcbody-env value)))))

                      ;; Check if it needs to be turned into a "ref-cell".
                      (:captured+mutated
                       ;; Declared variable is mutated and captured.
                       (push `(,var . (car-safe ,var)) new-env)
                       `(list ,(cconv-convert value env extend)))

                      ;; Check if it needs to be turned into a "ref-cell".
                      (:unused
                       ;; Declared variable is unused.
                       (if (assq var new-env)
                           (push `(,var) new-env)) ;FIXME:Needed?
                       (let* ((Ignore (if (symbol-with-pos-p var)
                                          (position-symbol 'ignore var)
                                        'ignore))
                              (newval `(,Ignore
                                        ,(cconv-convert value env extend)))
                              (msg (cconv--warn-unused-msg var "variable")))
                         (if (null msg) newval
                           (macroexp--warn-wrap var msg newval 'lexical))))

                      ;; Normal default case.
                      (_
                       (if (assq var new-env) (push `(,var) new-env))
                       (cconv-convert value env extend)))))

               (when (and (eq letsym 'let*) (memq var new-extend))
                 ;; One of the lambda-lifted vars is shadowed, so add
                 ;; a reference to the outside binding and arrange to use
                 ;; that reference.
                 (let ((var-def (cconv--lifted-arg var env))
                       (closedsym (make-symbol (format "closed-%s" var))))
                   (setq new-env (cconv--remap-llv new-env var closedsym))
                   ;; FIXME: `closedsym' doesn't need to be added to `extend'
                   ;; but adding it makes it easier to write the assertion at
                   ;; the beginning of this function.
                   (setq new-extend (cons closedsym (remq var new-extend)))
                   (push `(,closedsym ,var-def) binders-new)))

               ;; We push the element after redefined free variables are
               ;; processed.  This is important to avoid the bug when free
               ;; variable and the function have the same name.
               (push (list var new-val) binders-new)

               (when (eq letsym 'let*)
                 (setq env new-env)
                 (setq extend new-extend))))))
         )                           ; end of dolist over binders

       (when (not (eq letsym 'let*))
         ;; We can't do the cconv--remap-llv at the same place for let and
         ;; let* because in the case of `let', the shadowing may occur
         ;; before we know that the var will be in `new-extend' (bug#24171).
         (dolist (binder binders-new)
           (when (memq (car-safe binder) new-extend)
             ;; One of the lambda-lifted vars is shadowed.
             (let* ((var (car-safe binder))
                    (var-def (cconv--lifted-arg var env))
                    (closedsym (make-symbol (format "closed-%s" var))))
               (setq new-env (cconv--remap-llv new-env var closedsym))
               (setq new-extend (cons closedsym (remq var new-extend)))
               (push `(,closedsym ,var-def) binders-new)))))

       `(,letsym ,(nreverse binders-new)
                 . ,(mapcar (lambda (form)
                              (cconv-convert
                               form new-env new-extend))
                            body))))
					;end of let let* forms

                                  ; first element is lambda expression
    (`(,(and `(lambda . ,_) fun) . ,args)
     ;; FIXME: it's silly to create a closure just to call it.
     ;; Running byte-optimize-form earlier will resolve this.
     `(funcall
       ,(cconv-convert `(function ,fun) env extend)
       ,@(mapcar (lambda (form)
                   (cconv-convert form env extend))
                 args)))

    (`(cond . ,cond-forms)              ; cond special form
     `(,(car form) . ,(mapcar (lambda (branch)
                                (mapcar (lambda (form)
                                          (cconv-convert form env extend))
                                        branch))
                              cond-forms)))

    (`(function (lambda ,args . ,body) . ,_)
     (let* ((docstring (if (eq :documentation (car-safe (car body)))
                           (cconv-convert (cadr (pop body)) env extend)))
            (bf (if (stringp (car body)) (cdr body) body))
            (if (when (eq 'interactive (car-safe (car bf)))
                  (gethash form cconv--interactive-form-funs)))
            (cif (when if (cconv-convert if env extend)))
            (_ (pcase cif
                 (`#'(lambda () ,form) (setf (cadr (car bf)) form) (setq cif nil))
                 ('nil nil)
                 ;; The interactive form needs special treatment, so the form
                 ;; inside the `interactive' won't be used any further.
                 (_ (setf (cadr (car bf)) nil))))
            (cf (cconv--convert-function args body env form docstring)))
       (if (not cif)
           ;; Normal case, the interactive form needs no special treatment.
           cf
         `(cconv--interactive-helper ,cf ,cif))))

    (`(internal-make-closure . ,_)
     (byte-compile-report-error
      "Internal error in compiler: cconv called twice?"))

    (`(quote . ,_) form)
    (`(function . ,_) form)

					;defconst, defvar
    (`(,(and sym (or 'defconst 'defvar)) ,definedsymbol . ,forms)
     `(,sym ,definedsymbol
            . ,(when (consp forms)
                 (cons (cconv-convert (car forms) env extend)
                       ;; The rest (i.e. docstring, of any) is not evaluated,
                       ;; and may be an invalid expression (e.g. ($# . 678)).
                       (cdr forms)))))

                                        ; condition-case
    (`(condition-case ,var ,protected-form . ,handlers)
     (let* ((class (and var (cconv--var-classification (list var) form)))
            (newenv
             (cond ((eq class :captured+mutated)
                    (cons `(,var . (car-safe ,var)) env))
                   ((assq var env) (cons `(,var) env))
                   (t env)))
            (msg (when (eq class :unused)
                   (cconv--warn-unused-msg var "variable")))
            (newprotform (cconv-convert protected-form env extend)))
       `(,(car form) ,var
            ,(if msg
                 (macroexp--warn-wrap var msg newprotform 'lexical)
               newprotform)
          ,@(mapcar
             (lambda (handler)
               `(,(car handler)
                 ,@(let ((body
                          (mapcar (lambda (form)
                                    (cconv-convert form newenv extend))
                                  (cdr handler))))
                     (if (not (eq class :captured+mutated))
                         body
                       `((let ((,var (list ,var))) ,@body))))))
             handlers))))

    (`(unwind-protect ,form1 . ,body)
     `(,(car form) ,(cconv-convert form1 env extend)
        :fun-body ,(cconv--convert-function () body env form1)))

    (`(setq ,var ,expr)
     (let ((var-new (or (cdr (assq var env)) var))
           (value (cconv-convert expr env extend)))
       (pcase var-new
         ((pred symbolp) `(,(car form) ,var-new ,value))
         (`(car-safe ,iexp) `(setcar ,iexp ,value))
         ;; This "should never happen", but for variables which are
         ;; mutated+captured+unused, we may end up trying to `setq'
         ;; on a closed-over variable, so just drop the setq.
         (_ ;; (byte-compile-report-error
          ;;  (format "Internal error in cconv of (setq %s ..)"
          ;;          sym-new))
          value))))

    (`(,(and (or 'funcall 'apply) callsym) ,fun . ,args)
     ;; These are not special forms but we treat them separately for the needs
     ;; of lambda lifting.
     (let ((mapping (cdr (assq fun env))))
       (pcase mapping
         (`(apply-partially ,_ . ,(and fvs `(,_ . ,_)))
          (cl-assert (eq (cadr mapping) fun))
          `(,callsym ,fun
                     ,@(mapcar (lambda (fv)
                                 (let ((exp (or (cdr (assq fv env)) fv)))
                                   (pcase exp
                                     (`(car-safe ,iexp . ,_) iexp)
                                     (_ exp))))
                               fvs)
                     ,@(mapcar (lambda (arg)
                                 (cconv-convert arg env extend))
                               args)))
         (_ `(,callsym ,@(mapcar (lambda (arg)
                                   (cconv-convert arg env extend))
                                 (cons fun args)))))))

    ;; The form (if any) is converted beforehand as part of the `lambda' case.
    (`(interactive . ,_) form)

    ;; `declare' should now be macro-expanded away (and if they're not, we're
    ;; in trouble because they *can* contain code nowadays).
    ;; (`(declare . ,_) form)              ;The args don't contain code.

    (`(oclosure--fix-type (ignore . ,vars) ,exp)
     (dolist (var vars)
       (let ((x (assq var env)))
         (pcase (cdr x)
           (`(car-safe . ,_) (error "Slot %S should not be mutated" var))
           (_ (cl-assert (null (cdr x)))))))
     (cconv-convert exp env extend))

    (`(,func . ,forms)
     ;; First element is function or whatever function-like forms are: or, and,
     ;; if, catch, progn, prog1, while, until
     `(,func . ,(mapcar (lambda (form)
                          (cconv-convert form env extend))
                        forms)))

    (_ (or (cdr (assq form env)) form))))

(defvar byte-compile-lexical-variables)

(defun cconv--not-lexical-var-p (var dynbounds)
  (or (not lexical-binding)
      (not (symbolp var))
      (special-variable-p var)
      (memq var dynbounds)))

(defun cconv--analyze-use (vardata form varkind)
  "Analyze the use of a variable.
VARDATA should be (BINDER READ MUTATED CAPTURED CALLED).
VARKIND is the name of the kind of variable.
FORM is the parent form that binds this var."
  ;; use = `(,binder ,read ,mutated ,captured ,called)
  (pcase vardata
    (`(,_ nil nil nil nil) nil)
    (`((,(and var (guard (eq ?_ (aref (symbol-name var) 0)))) . ,_)
       ,_ ,_ ,_ ,_)
     ;; FIXME: Convert this warning to use `macroexp--warn-wrap'
     ;; so as to give better position information.
     (when (byte-compile-warning-enabled-p 'not-unused var)
       (byte-compile-warn-x
        var "%s `%S' not left unused" varkind var)))
    ((and (let (or 'let* 'let) (car form))
          `((,var) ;; (or `(,var nil) : Too many false positives: bug#47080
            t nil ,_ ,_))
     ;; FIXME: Convert this warning to use `macroexp--warn-wrap'
     ;; so as to give better position information and obey
     ;; `byte-compile-warnings'.
     (unless (not (intern-soft var))
       (byte-compile-warn-x var "Variable `%S' left uninitialized" var))))
  (pcase vardata
    (`(,binder nil ,_ ,_ nil)
     (push (cons (cons binder form) :unused) cconv-var-classification))
    ;; If it's unused, there's no point converting it into a cons-cell, even if
    ;; it's captured and mutated.
    (`(,binder ,_ t t ,_)
     (push (cons (cons binder form) :captured+mutated)
           cconv-var-classification))
    (`(,(and binder `(,_ (function (lambda . ,_)))) nil nil nil t)
     (push (cons (cons binder form) :lambda-candidate)
           cconv-var-classification))))

(defun cconv--analyze-function (args body env parentform)
  (let* ((newvars nil)
         (freevars (list body))
         ;; We analyze the body within a new environment where all uses are
         ;; nil, so we can distinguish uses within that function from uses
         ;; outside of it.
         (envcopy
          (mapcar (lambda (vdata) (list (car vdata) nil nil nil nil)) env))
         (cconv--dynbound-variables cconv--dynbound-variables)
         (newenv envcopy))
    ;; Push it before recursing, so cconv-freevars-alist contains entries in
    ;; the order they'll be used by closure-convert-rec.
    (push freevars cconv-freevars-alist)
    (when lexical-binding
      (dolist (arg args)
        (cond
         ((cconv--not-lexical-var-p arg cconv--dynbound-variables)
          (byte-compile-warn-x
           arg
           "Lexical argument shadows the dynamic variable %S"
           arg))
         ((eq ?& (aref (symbol-name arg) 0)) nil) ;Ignore &rest, &optional, ...
         (t (let ((varstruct (list arg nil nil nil nil)))
              (cl-pushnew arg byte-compile-lexical-variables)
              (push (cons (list arg) (cdr varstruct)) newvars)
              (push varstruct newenv))))))
    (dolist (form body)                   ;Analyze body forms.
      (cconv-analyze-form form newenv))
    ;; Summarize resulting data about arguments.
    (dolist (vardata newvars)
      (cconv--analyze-use vardata parentform "argument"))
    ;; Transfer uses collected in `envcopy' (via `newenv') back to `env';
    ;; and compute free variables.
    (while env
      (cl-assert (and envcopy (eq (caar env) (caar envcopy))))
      (let ((free nil)
            (x (cdr (car env)))
            (y (cdr (car envcopy))))
        (while x
          (when (car y) (setcar x t) (setq free t))
          (setq x (cdr x) y (cdr y)))
        (when free
          (push (caar env) (cdr freevars))
          (setf (nth 3 (car env)) t))
        (setq env (cdr env) envcopy (cdr envcopy))))))

(defvar cconv--dynbindings)

(defun cconv-analyze-form (form env)
  "Find mutated variables and variables captured by closure.
Analyze lambdas if they are suitable for lambda lifting.
- FORM is a piece of Elisp code after macroexpansion.
- ENV is an alist mapping each enclosing lexical variable to its info.
   I.e. each element has the form (VAR . (READ MUTATED CAPTURED CALLED)).
This function does not return anything but instead fills the
`cconv-var-classification' variable and updates the data stored in ENV."
  (pcase form
					; let special form
    (`(,(and (or 'let* 'let) letsym) ,binders . ,body-forms)

     (let ((orig-env env)
           (newvars nil)
           (var nil)
           (cconv--dynbound-variables cconv--dynbound-variables)
           (value nil))
       (dolist (binder binders)
         (if (not (consp binder))
             (progn
               (setq var binder)      ; treat the form (let (x) ...) well
               (setq binder (list binder))
               (setq value nil))
           (setq var (car binder))
           (setq value (cadr binder))

           (cconv-analyze-form value (if (eq letsym 'let*) env orig-env)))

         (if (cconv--not-lexical-var-p var cconv--dynbound-variables)
             (when (boundp 'cconv--dynbindings)
               (push var cconv--dynbindings))
           (cl-pushnew var byte-compile-lexical-variables)
           (let ((varstruct (list var nil nil nil nil)))
             (push (cons binder (cdr varstruct)) newvars)
             (push varstruct env))))

       (dolist (form body-forms)          ; Analyze body forms.
         (cconv-analyze-form form env))

       (dolist (vardata newvars)
         (cconv--analyze-use vardata form "variable"))))

    (`(function (lambda ,vrs . ,body-forms))
     (when (eq :documentation (car-safe (car body-forms)))
       (cconv-analyze-form (cadr (pop body-forms)) env))
     (let ((bf (if (stringp (car body-forms)) (cdr body-forms) body-forms)))
       (when (eq 'interactive (car-safe (car bf)))
         (let ((if (cadr (car bf))))
           (unless (macroexp-const-p if) ;Optimize this common case.
             (let ((f `#'(lambda () ,if)))
               (setf (gethash form cconv--interactive-form-funs) f)
               (cconv-analyze-form f env))))))
     (cconv--analyze-function vrs body-forms env form))

    (`(setq ,var ,expr)
     ;; If a local variable (member of env) is modified by setq then
     ;; it is a mutated variable.
     (let ((v (assq var env))) ; v = non nil if visible
       (when v
         (setf (nth 2 v) t)))
     (cconv-analyze-form expr env))

    (`((lambda . ,_) . ,_)             ; First element is lambda expression.
     (byte-compile-warn-x
      (nth 1 (car form))
      "Use of deprecated ((lambda %s ...) ...) form" (nth 1 (car form)))
     (dolist (exp `((function ,(car form)) . ,(cdr form)))
       (cconv-analyze-form exp env)))

    (`(cond . ,cond-forms)              ; cond special form
     (dolist (forms cond-forms)
       (dolist (form forms) (cconv-analyze-form form env))))

    ;; ((and `(quote ,v . ,_) (guard (assq v env)))
    ;;  (byte-compile-warn
    ;;   "Possible confusion variable/symbol for `%S'" v))

    (`(quote . ,_) nil)                 ; quote form
    (`(function . ,_) nil)              ; same as quote

    (`(condition-case ,var ,protected-form . ,handlers)
     (cconv-analyze-form protected-form env)
     (unless lexical-binding
       (setq var nil))
     (when (and var (symbolp var)
                (cconv--not-lexical-var-p var cconv--dynbound-variables))
       (byte-compile-warn-x
        var "Lexical variable shadows the dynamic variable %S" var))
     (let* ((varstruct (list var nil nil nil nil)))
       (if var (push varstruct env))
       (dolist (handler handlers)
         (dolist (form (cdr handler))
           (cconv-analyze-form form env)))
       (if var (cconv--analyze-use (cons (list var) (cdr varstruct))
                                   form "variable"))))

    ;; FIXME: The bytecode for unwind-protect forces us to wrap the unwind.
    (`(unwind-protect ,form . ,body)
     (cconv-analyze-form form env)
     (cconv--analyze-function () body env form))

    (`(defvar ,var) (push var cconv--dynbound-variables))
    (`(,(or 'defconst 'defvar) ,var ,value . ,_)
     (push var cconv--dynbound-variables)
     (cconv-analyze-form value env))

    (`(,(or 'funcall 'apply) ,fun . ,args)
     ;; Here we ignore fun because funcall and apply are the only two
     ;; functions where we can pass a candidate for lambda lifting as
     ;; argument.  So, if we see fun elsewhere, we'll delete it from
     ;; lambda candidate list.
     (let ((fdata (and (symbolp fun) (assq fun env))))
       (if fdata
           (setf (nth 4 fdata) t)
         (cconv-analyze-form fun env)))
     (dolist (form args) (cconv-analyze-form form env)))

    ;; The form (if any) is converted beforehand as part of the `lambda' case.
    (`(interactive . ,_) nil)

    ;; `declare' should now be macro-expanded away (and if they're not, we're
    ;; in trouble because they *can* contain code nowadays).
    ;; (`(declare . ,_) nil)               ;The args don't contain code.

    (`(,_ . ,body-forms)    ; First element is a function or whatever.
     (unless (listp body-forms)
       (signal 'wrong-type-argument (list 'proper-list-p form)))
     (dolist (form body-forms) (cconv-analyze-form form env)))

    ((pred symbolp)
     (let ((dv (assq form env)))        ; dv = declared and visible
       (when dv
         (setf (nth 1 dv) t))))))
(define-obsolete-function-alias 'cconv-analyse-form #'cconv-analyze-form "25.1")

(defun cconv-fv (form lexvars dynvars)
  "Return the list of free variables in FORM.
LEXVARS is the list of statically scoped vars in the context
and DYNVARS is the list of dynamically scoped vars in the context.
Returns a pair (LEXV . DYNV) of those vars actually used by FORM."
  (let* ((fun
          ;; Wrap FORM into a function because the analysis code we
          ;; have only computes freevars for functions.
          ;; In practice FORM is always already of the form
          ;; #'(lambda ...), so optimize for this case.
          (if (and (eq 'function (car-safe form))
                   (eq 'lambda (car-safe (cadr form)))
                   ;; To get correct results, FUN needs to be a "simple lambda"
                   ;; without nested forms that aren't part of the body.  :-(
                   (not (assq 'interactive (cadr form)))
                   (not (assq ':documentation (cadr form))))
              form
            `#'(lambda () ,form)))
         (analysis-env (mapcar (lambda (v) (list v nil nil nil nil)) lexvars))
         (cconv--dynbound-variables dynvars)
         (byte-compile-lexical-variables nil)
         (cconv--dynbindings nil)
         (cconv-freevars-alist '())
	 (cconv-var-classification '()))
    (let* ((body (cddr (cadr fun))))
      ;; Analyze form - fill these variables with new information.
      (cconv-analyze-form fun analysis-env)
      (setq cconv-freevars-alist (nreverse cconv-freevars-alist))
      (unless (equal (if (eq :documentation (car-safe (car body)))
                            (cdr body) body)
                     (caar cconv-freevars-alist))
        (message "BOOH!\n%S\n%S"
                 body (caar cconv-freevars-alist)))
      (cl-assert (equal (if (eq :documentation (car-safe (car body)))
                            (cdr body) body)
                        (caar cconv-freevars-alist)))
      (let ((fvs (nreverse (cdar cconv-freevars-alist)))
            (dyns (delq nil (mapcar (lambda (var) (car (memq var dynvars)))
                                    (delete-dups cconv--dynbindings)))))
        (cons fvs dyns)))))

(defun cconv-make-interpreted-closure (fun env)
  (cl-assert (eq (car-safe fun) 'lambda))
  (let ((lexvars (delq nil (mapcar #'car-safe env))))
    (if (null lexvars)
        ;; The lexical environment is empty, so there's no need to
        ;; look for free variables.
        `(closure ,env . ,(cdr fun))
      ;; We could try and cache the result of the macroexpansion and
      ;; `cconv-fv' analysis.  Not sure it's worth the trouble.
      (let* ((form `#',fun)
             (expanded-form
              (let ((lexical-binding t) ;; Tell macros which dialect is in use.
	            ;; Make the macro aware of any defvar declarations in scope.
                    (macroexp--dynvars
                     (if macroexp--dynvars
                         (append env macroexp--dynvars) env)))
                (macroexpand-all form macroexpand-all-environment)))
             ;; Since we macroexpanded the body, we may as well use that.
             (expanded-fun-cdr
              (pcase expanded-form
                (`#'(lambda . ,cdr) cdr)
                (_ (cdr fun))))

             (dynvars (delq nil (mapcar (lambda (b) (if (symbolp b) b)) env)))
             (fvs (cconv-fv expanded-form lexvars dynvars))
             (newenv (nconc (mapcar (lambda (fv) (assq fv env)) (car fvs))
                            (cdr fvs))))
        ;; Never return a nil env, since nil means to use the dynbind
        ;; dialect of ELisp.
        `(closure ,(or newenv '(t)) . ,expanded-fun-cdr)))))


(provide 'cconv)
;;; cconv.el ends here
