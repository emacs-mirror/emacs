;;; cconv.el --- Closure conversion for statically scoped Emacs lisp. -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Igor Kuzmin <kzuminig@iro.umontreal.ca>
;; Maintainer: FSF
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This takes a piece of Elisp code, and eliminates all free variables from
;; lambda expressions.  The user entry points are cconv-closure-convert and
;; cconv-closure-convert-toplevel(for toplevel forms).
;; All macros should be expanded beforehand.
;;
;; Here is a brief explanation how this code works.
;; Firstly, we analyse the tree by calling cconv-analyse-form.
;; This function finds all mutated variables, all functions that are suitable
;; for lambda lifting and all variables captured by closure. It passes the tree
;; once, returning a list of three lists.
;;
;; Then we calculate the intersection of first and third lists returned by
;; cconv-analyse form to find all mutated variables that are captured by
;; closure.

;; Armed with this data, we call cconv-closure-convert-rec, that rewrites the
;; tree recursivly, lifting lambdas where possible, building closures where it
;; is needed and eliminating mutable variables used in closure.
;;
;; We do following replacements :
;; (lambda (v1 ...) ... fv1 fv2 ...) => (lambda (v1 ... fv1 fv2 ) ... fv1 fv2 .)
;; if the function is suitable for lambda lifting (if all calls are known)
;;
;; (lambda (v0 ...) ... fv0 .. fv1 ...)  =>
;; (internal-make-closure (v0 ...) (fv1 ...)
;;   ... (internal-get-closed-var 0) ...  (internal-get-closed-var 1) ...)
;;
;; If the function has no free variables, we don't do anything.
;;
;; If a variable is mutated (updated by setq), and it is used in a closure
;; we wrap its definition with list: (list val) and we also replace
;; var => (car var) wherever this variable is used, and also
;; (setq var value) => (setcar var value) where it is updated.
;;
;; If defun argument is closure mutable, we letbind it and wrap it's
;; definition with list.
;; (defun foo (... mutable-arg ...) ...) =>
;; (defun foo (... m-arg ...) (let ((m-arg (list m-arg))) ...))
;;
;;; Code:

;; TODO:
;; - byte-optimize-form should be applied before cconv.
;; - maybe unify byte-optimize and compiler-macros.
;; - canonize code in macro-expand so we don't have to handle (let (var) body)
;;   and other oddities.
;; - clean up cconv-closure-convert-rec, especially the `let' binding part.
;; - new byte codes for unwind-protect, catch, and condition-case so that
;;   closures aren't needed at all.
;; - a reference to a var that is known statically to always hold a constant
;;   should be turned into a byte-constant rather than a byte-stack-ref.
;;   Hmm... right, that's called constant propagation and could be done here,
;;   but when that constant is a function, we have to be careful to make sure
;;   the bytecomp only compiles it once.
;; - Since we know here when a variable is not mutated, we could pass that
;;   info to the byte-compiler, e.g. by using a new `immutable-let'.
;; - add tail-calls to bytecode.c and the byte compiler.

;; (defmacro dlet (binders &rest body)
;;   ;; Works in both lexical and non-lexical mode.
;;   `(progn
;;      ,@(mapcar (lambda (binder)
;;                  `(defvar ,(if (consp binder) (car binder) binder)))
;;                binders)
;;      (let ,binders ,@body)))

;; (defmacro llet (binders &rest body)
;;   ;; Only works in lexical-binding mode.
;;   `(funcall
;;     (lambda ,(mapcar (lambda (binder) (if (consp binder) (car binder) binder))
;;                 binders)
;;       ,@body)
;;     ,@(mapcar (lambda (binder) (if (consp binder) (cadr binder)))
;;               binders)))

;; (defmacro letrec (binders &rest body)
;;   ;; Only useful in lexical-binding mode.
;;   ;; As a special-form, we could implement it more efficiently (and cleanly,
;;   ;; making the vars actually unbound during evaluation of the binders).
;;   `(let ,(mapcar (lambda (binder) (if (consp binder) (car binder) binder))
;;                  binders)
;;      ,@(delq nil (mapcar (lambda (binder) (if (consp binder) `(setq ,@binder)))
;;                          binders))
;;      ,@body))

(eval-when-compile (require 'cl))

(defconst cconv-liftwhen 6
  "Try to do lambda lifting if the number of arguments + free variables
is less than this number.")
;; List of all the variables that are both captured by a closure
;; and mutated.  Each entry in the list takes the form
;; (BINDER . PARENTFORM) where BINDER is the (VAR VAL) that introduces the
;; variable (or is just (VAR) for variables not introduced by let).
(defvar cconv-captured+mutated)

;; List of candidates for lambda lifting.
;; Each candidate has the form (BINDER . PARENTFORM).  A candidate
;; is a variable that is only passed to `funcall' or `apply'.
(defvar cconv-lambda-candidates)

;; Alist associating to each function body the list of its free variables.
(defvar cconv-freevars-alist)

;;;###autoload
(defun cconv-closure-convert (form)
  "Main entry point for closure conversion.
-- FORM is a piece of Elisp code after macroexpansion.
-- TOPLEVEL(optional) is a boolean variable, true if we are at the root of AST

Returns a form where all lambdas don't have any free variables."
  ;; (message "Entering cconv-closure-convert...")
  (let ((cconv-freevars-alist '())
	(cconv-lambda-candidates '())
	(cconv-captured+mutated '()))
    ;; Analyse form - fill these variables with new information.
    (cconv-analyse-form form '())
    (setq cconv-freevars-alist (nreverse cconv-freevars-alist))
    (cconv-closure-convert-rec
     form                               ; the tree
     '()                                ;
     '()                                ; fvrs initially empty
     '()                                ; envs initially empty
     '()
     )))

(defconst cconv--dummy-var (make-symbol "ignored"))

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
    (assert (null (assq x res))) ;; Check the assumption was warranted.
    res))

(defun cconv--map-diff-set (m s)
  "Return the map M minus any mapping for elements of S."
  ;; Here we assume that X appears at most once in M.
  (let ((res '()))
    (dolist (b m)
      (unless (memq (car b) s) (push b res)))
    (nreverse res)))

(defun cconv-closure-convert-function (fvrs vars emvrs envs lmenvs body-forms
                                            parentform)
  (assert (equal body-forms (caar cconv-freevars-alist)))
  (let* ((fvrs-new (cconv--set-diff fvrs vars)) ; Remove vars from fvrs.
         (fv (cdr (pop cconv-freevars-alist)))
         (body-forms-new '())
         (letbind '())
         (envector nil))
    (when fv
      ;; Here we form our environment vector.

      (dolist (elm fv)
        (push
         (cconv-closure-convert-rec
          ;; Remove `elm' from `emvrs' for this call because in case
          ;; `elm' is a variable that's wrapped in a cons-cell, we
          ;; want to put the cons-cell itself in the closure, rather
          ;; than just a copy of its current content.
          elm (remq elm emvrs) fvrs envs lmenvs)
         envector))                     ; Process vars for closure vector.
      (setq envector (reverse envector))
      (setq envs fv)
      (setq fvrs-new fv))               ; Update substitution list.

    (setq emvrs (cconv--set-diff emvrs vars))
    (setq lmenvs (cconv--map-diff-set lmenvs vars))
       
    ;; The difference between envs and fvrs is explained
    ;; in comment in the beginning of the function.
    (dolist (var vars)
      (when (member (cons (list var) parentform) cconv-captured+mutated)
        (push var emvrs)
        (push `(,var (list ,var)) letbind)))
    (dolist (elm body-forms)            ; convert function body
      (push (cconv-closure-convert-rec
             elm emvrs fvrs-new envs lmenvs)
            body-forms-new))

    (setq body-forms-new
          (if letbind `((let ,letbind . ,(reverse body-forms-new)))
            (reverse body-forms-new)))

    (cond
					;if no freevars - do nothing
     ((null envector)
      `(function (lambda ,vars . ,body-forms-new)))
                                        ; 1 free variable - do not build vector
     (t
      `(internal-make-closure
        ,vars ,envector . ,body-forms-new)))))

(defun cconv-closure-convert-rec (form emvrs fvrs envs lmenvs)
  ;; This function actually rewrites the tree.
  "Eliminates all free variables of all lambdas in given forms.
Arguments:
- FORM is a piece of Elisp code after macroexpansion.
- LMENVS is a list of environments used for lambda-lifting.  Initially empty.
- EMVRS is a list that contains mutated variables that are visible
within current environment.
- ENVS is an environment(list of free variables) of current closure.
Initially empty.
- FVRS is a list of variables to substitute in each context.
Initially empty.

Returns a form where all lambdas don't have any free variables."
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
    (`(,(and letsym (or `let* `let)) ,binders . ,body-forms)

					; let and let* special forms
     (let ((body-forms-new '())
           (binders-new '())
           ;; next for variables needed for delayed push
           ;; because we should process <value(s)>
           ;; before we change any arguments
           (lmenvs-new '())             ;needed only in case of let
           (emvrs-new '())              ;needed only in case of let
           (emvr-push)                  ;needed only in case of let*
           (lmenv-push))                ;needed only in case of let*

       (dolist (binder binders)
         (let* ((value nil)
                (var (if (not (consp binder))
                         (prog1 binder (setq binder (list binder)))
                       (setq value (cadr binder))
                       (car binder)))
                (new-val
                 (cond
                  ;; Check if var is a candidate for lambda lifting.
                  ((member (cons binder form) cconv-lambda-candidates)
                   (assert (and (eq (car value) 'function)
                                (eq (car (cadr value)) 'lambda)))
                   (assert (equal (cddr (cadr value))
                                  (caar cconv-freevars-alist)))
		   ;; Peek at the freevars to decide whether to λ-lift.
                   (let* ((fv (cdr (car cconv-freevars-alist)))
                          (funargs (cadr (cadr value)))
                          (funcvars (append fv funargs))
                          (funcbodies (cddadr value)) ; function bodies
                          (funcbodies-new '()))
					; lambda lifting condition
                     (if (or (not fv) (< cconv-liftwhen (length funcvars)))
					; do not lift
                         (progn
                           ;; (byte-compile-log-warning
                           ;;  (format "Not λ-lifting `%S': %d > %d"
                           ;;          var (length funcvars) cconv-liftwhen))

                           (cconv-closure-convert-rec
                            value emvrs fvrs envs lmenvs))
					; lift
                       (progn
                         ;; (byte-compile-log-warning
                         ;;  (format "λ-lifting `%S'" var))
			 (setq cconv-freevars-alist
			       ;; Now that we know we'll λ-lift, consume the
			       ;; freevar data.
			       (cdr cconv-freevars-alist))
                         (dolist (elm2 funcbodies)
                           (push        ; convert function bodies
                            (cconv-closure-convert-rec
                             elm2 emvrs nil envs lmenvs)
                            funcbodies-new))
                         (if (eq letsym 'let*)
                             (setq lmenv-push (cons var fv))
                           (push (cons var fv) lmenvs-new))
					; push lifted function

                         `(function .
                                    ((lambda ,funcvars .
                                       ,(reverse funcbodies-new))))))))

                  ;; Check if it needs to be turned into a "ref-cell".
                  ((member (cons binder form) cconv-captured+mutated)
                   ;; Declared variable is mutated and captured.
                   (prog1
                       `(list ,(cconv-closure-convert-rec
                                value emvrs
                                fvrs envs lmenvs))
                     (if (eq letsym 'let*)
                         (setq emvr-push var)
                       (push var emvrs-new))))

                  ;; Normal default case.
                  (t
                   (cconv-closure-convert-rec
                    value emvrs fvrs envs lmenvs)))))

           ;; this piece of code below letbinds free
           ;; variables  of a lambda lifted function
           ;; if they are redefined in this let
           ;; example:
           ;; (let* ((fun (lambda (x) (+ x y))) (y 1)) (funcall fun 1))
           ;; Here we can not pass y as parameter because it is
           ;; redefined. We add a (closed-y y) declaration.
           ;; We do that even if the function is not used inside
           ;; this let(*). The reason why we ignore this case is
           ;; that we can't "look forward" to see if the function
           ;; is called there or not. To treat well this case we
           ;; need to traverse the tree one more time to collect this
           ;; data, and I think that it's not worth it.

           (when (eq letsym 'let*)
             (let ((closedsym '())
                   (new-lmenv '())
                   (old-lmenv '()))
               (dolist (lmenv lmenvs)
                 (when (memq var (cdr lmenv))
                   (setq closedsym
                         (make-symbol
                          (concat "closed-" (symbol-name var))))
                   (setq new-lmenv (list (car lmenv)))
                   (dolist (frv (cdr lmenv)) (if (eq frv var)
                                                 (push closedsym new-lmenv)
                                               (push frv new-lmenv)))
                   (setq new-lmenv (reverse new-lmenv))
                   (setq old-lmenv lmenv)))
               (when new-lmenv
                 (setq lmenvs (remq old-lmenv lmenvs))
                 (push new-lmenv lmenvs)
                 (push `(,closedsym ,var) binders-new))))
           ;; We push the element after redefined free variables are
           ;; processed.  This is important to avoid the bug when free
           ;; variable and the function have the same name.
           (push (list var new-val) binders-new)

           (when (eq letsym 'let*)      ; update fvrs
             (setq fvrs (remq var fvrs))
             (setq emvrs (remq var emvrs)) ; remove if redefined
             (when emvr-push
               (push emvr-push emvrs)
               (setq emvr-push nil))
             (setq lmenvs (cconv--map-diff-elem lmenvs var))
             (when lmenv-push
               (push lmenv-push lmenvs)
               (setq lmenv-push nil)))
           ))                          ; end of dolist over binders
       (when (eq letsym 'let)

         ;; Here we update emvrs, fvrs and lmenvs lists
         (setq fvrs (cconv--set-diff-map fvrs binders-new))
         (setq emvrs (cconv--set-diff-map emvrs binders-new))
         (setq emvrs (append emvrs emvrs-new))
         (setq lmenvs (cconv--set-diff-map lmenvs binders-new))
         (setq lmenvs (append lmenvs lmenvs-new))

         ;; Here we do the same letbinding as for let* above
         ;; to avoid situation when a free variable of a lambda lifted
         ;; function got redefined.

         (let ((new-lmenv)
               (var nil)
               (closedsym nil)
               (letbinds '()))
           (dolist (binder binders)
             (setq var (if (consp binder) (car binder) binder))

             (let ((lmenvs-1 lmenvs))   ; just to avoid manipulating
               (dolist (lmenv lmenvs-1) ; the counter inside the loop
                 (when (memq var (cdr lmenv))
                   (setq closedsym (make-symbol
                                    (concat "closed-"
                                            (symbol-name var))))

                   (setq new-lmenv (list (car lmenv)))
                   (dolist (frv (cdr lmenv))
                     (push (if (eq frv var) closedsym frv)
                           new-lmenv))
                   (setq new-lmenv (reverse new-lmenv))
                   (setq lmenvs (remq lmenv lmenvs))
                   (push new-lmenv lmenvs)
                   (push `(,closedsym ,var) letbinds)
                   ))))
           (setq binders-new (append binders-new letbinds))))

       (dolist (elm body-forms)         ; convert body forms
         (push (cconv-closure-convert-rec
                elm emvrs fvrs envs lmenvs)
               body-forms-new))
       `(,letsym ,(reverse binders-new) . ,(reverse body-forms-new))))
					;end of let let* forms

                                  ; first element is lambda expression
    (`(,(and `(lambda . ,_) fun) . ,other-body-forms)

     (let ((other-body-forms-new '()))
       (dolist (elm other-body-forms)
         (push (cconv-closure-convert-rec
                elm emvrs fvrs envs lmenvs)
               other-body-forms-new))
       `(funcall
         ,(cconv-closure-convert-rec
           (list 'function fun) emvrs fvrs envs lmenvs)
         ,@(nreverse other-body-forms-new))))

    (`(cond . ,cond-forms)              ; cond special form
     (let ((cond-forms-new '()))
       (dolist (elm cond-forms)
         (push (let ((elm-new '()))
                 (dolist (elm-2 elm)
                   (push
                    (cconv-closure-convert-rec
                     elm-2 emvrs fvrs envs lmenvs)
                    elm-new))
                 (reverse elm-new))
               cond-forms-new))
       (cons 'cond
             (reverse cond-forms-new))))

    (`(quote . ,_) form)

    (`(function (lambda ,vars . ,body-forms)) ; function form
     (cconv-closure-convert-function
      fvrs vars emvrs envs lmenvs body-forms form))

    (`(internal-make-closure . ,_)
     (error "Internal byte-compiler error: cconv called twice"))

    (`(function . ,_) form)             ; Same as quote.

					;defconst, defvar
    (`(,(and sym (or `defconst `defvar)) ,definedsymbol . ,body-forms)

     (let ((body-forms-new '()))
       (dolist (elm body-forms)
         (push (cconv-closure-convert-rec
                elm emvrs fvrs envs lmenvs)
               body-forms-new))
       (setq body-forms-new (reverse body-forms-new))
       `(,sym ,definedsymbol . ,body-forms-new)))

					;defun, defmacro
    (`(,(and sym (or `defun `defmacro))
       ,func ,vars . ,body-forms)

     ;; The freevar data was pushed onto cconv-freevars-alist
     ;; but we don't need it.
     (assert (equal body-forms (caar cconv-freevars-alist)))
     (assert (null (cdar cconv-freevars-alist)))
     (setq cconv-freevars-alist (cdr cconv-freevars-alist))

     (let ((body-new '())	  ; The whole body.
           (body-forms-new '())   ; Body w\o docstring and interactive.
           (letbind '()))
					; Find mutable arguments.
       (dolist (elm vars)
         (when (member (cons (list elm) form) cconv-captured+mutated)
           (push elm letbind)
           (push elm emvrs)))
                                            ;Transform body-forms.
       (when (stringp (car body-forms))     ; Treat docstring well.
         (push (car body-forms) body-new)
         (setq body-forms (cdr body-forms)))
       (when (eq (car-safe (car body-forms)) 'interactive)
         (push (cconv-closure-convert-rec
                (car body-forms)
                emvrs fvrs envs lmenvs)
               body-new)
         (setq body-forms (cdr body-forms)))

       (dolist (elm body-forms)
         (push (cconv-closure-convert-rec
                elm emvrs fvrs envs lmenvs)
               body-forms-new))
       (setq body-forms-new (reverse body-forms-new))

       (if letbind
					; Letbind mutable arguments.
           (let ((binders-new '()))
             (dolist (elm letbind) (push `(,elm (list ,elm))
                                         binders-new))
             (push `(let ,(reverse binders-new) .
                         ,body-forms-new) body-new)
             (setq body-new (reverse body-new)))
         (setq body-new (append (reverse body-new) body-forms-new)))

       `(,sym ,func ,vars . ,body-new)))

					;condition-case
    (`(condition-case ,var ,protected-form . ,handlers)
     (let ((newform (cconv-closure-convert-rec
                     `(function (lambda () ,protected-form))
                     emvrs fvrs envs lmenvs)))
       (setq fvrs (remq var fvrs))
       `(condition-case :fun-body ,newform
	  ,@(mapcar (lambda (handler)
                      (list (car handler)
                            (cconv-closure-convert-rec
                             (let ((arg (or var cconv--dummy-var)))
                               `(function (lambda (,arg) ,@(cdr handler))))
                             emvrs fvrs envs lmenvs)))
                    handlers))))

    (`(,(and head (or `catch `unwind-protect)) ,form . ,body)
     `(,head ,(cconv-closure-convert-rec form emvrs fvrs envs lmenvs)
        :fun-body
        ,(cconv-closure-convert-rec `(function (lambda () ,@body))
                                    emvrs fvrs envs lmenvs)))

    (`(track-mouse . ,body)
     `(track-mouse
        :fun-body
        ,(cconv-closure-convert-rec `(function (lambda () ,@body))
                                    emvrs fvrs envs lmenvs)))

    (`(setq . ,forms)                   ; setq special form
     (let (prognlist sym sym-new value)
       (while forms
         (setq sym (car forms))
         (setq sym-new (cconv-closure-convert-rec
                        sym
                        (remq sym emvrs) fvrs envs lmenvs))
         (setq value
               (cconv-closure-convert-rec
                (cadr forms) emvrs fvrs envs lmenvs))
         (cond
          ((memq sym emvrs) (push `(setcar ,sym-new ,value) prognlist))
          ((symbolp sym-new) (push `(setq ,sym-new ,value) prognlist))
          ;; This should never happen, but for variables which are
          ;; mutated+captured+unused, we may end up trying to `setq'
          ;; on a closed-over variable, so just drop the setq.
          (t (push value prognlist)))
         (setq forms (cddr forms)))
       (if (cdr prognlist)
           `(progn . ,(reverse prognlist))
         (car prognlist))))

    (`(,(and (or `funcall `apply) callsym) ,fun . ,args)
                                     ; funcall is not a special form
                                     ; but we treat it separately
                                     ; for the needs of lambda lifting
     (let ((fv (cdr (assq fun lmenvs))))
       (if fv
           (let ((args-new '())
                 (processed-fv '()))
             ;; All args (free variables and actual arguments)
             ;; should be processed, because they can be fvrs
             ;; (free variables of another closure)
             (dolist (fvr fv)
               (push (cconv-closure-convert-rec
                      fvr (remq fvr emvrs)
                      fvrs envs lmenvs)
                     processed-fv))
             (setq processed-fv (reverse processed-fv))
             (dolist (elm args)
               (push (cconv-closure-convert-rec
                      elm emvrs fvrs envs lmenvs)
                     args-new))
             (setq args-new (append processed-fv (reverse args-new)))
             (setq fun (cconv-closure-convert-rec
                        fun emvrs fvrs envs lmenvs))
             `(,callsym ,fun . ,args-new))
         (let ((cdr-new '()))
           (dolist (elm (cdr form))
             (push (cconv-closure-convert-rec
                    elm emvrs fvrs envs lmenvs)
                   cdr-new))
           `(,callsym . ,(reverse cdr-new))))))

    (`(interactive . ,forms)
     `(interactive
       ,@(mapcar (lambda (form)
                   (cconv-closure-convert-rec form nil nil nil nil))
                 forms)))
    
    (`(,func . ,body-forms)    ; first element is function or whatever
                               ; function-like forms are:
                               ; or, and, if, progn, prog1, prog2,
                               ; while, until
     (let ((body-forms-new '()))
       (dolist (elm body-forms)
         (push (cconv-closure-convert-rec
                elm emvrs fvrs envs lmenvs)
               body-forms-new))
       (setq body-forms-new (reverse body-forms-new))
       `(,func . ,body-forms-new)))

    (_
     (let ((free (memq form fvrs)))
       (if free                         ;form is a free variable
           (let* ((numero (- (length fvrs) (length free)))
                  ;; Replace form => (aref env #)
                  (var `(internal-get-closed-var ,numero)))
             (if (memq form emvrs) ; form => (car (aref env #)) if mutable
                 `(car ,var)
               var))
         (if (memq form emvrs)         ; if form is a mutable variable
             `(car ,form)              ; replace form => (car form)
           form))))))

(unless (fboundp 'byte-compile-not-lexical-var-p)
  ;; Only used to test the code in non-lexbind Emacs.
  (defalias 'byte-compile-not-lexical-var-p 'boundp))

(defun cconv-analyse-use (vardata form varkind)
  "Analyse the use of a variable.
VARDATA should be (BINDER READ MUTATED CAPTURED CALLED).
VARKIND is the name of the kind of variable.
FORM is the parent form that binds this var."
  ;; use = `(,binder ,read ,mutated ,captured ,called)
  (pcase vardata
    (`(,_ nil nil nil nil) nil)
    (`((,(and (pred (lambda (var) (eq ?_ (aref (symbol-name var) 0)))) var) . ,_)
       ,_ ,_ ,_ ,_)
     (byte-compile-log-warning (format "%s `%S' not left unused" varkind var)))
    ((or `(,_ ,_ ,_ ,_ ,_) dontcare) nil))
  (pcase vardata
    (`((,var . ,_) nil ,_ ,_ nil)
     ;; FIXME: This gives warnings in the wrong order, with imprecise line
     ;; numbers and without function name info.
     (unless (or ;; Uninterned symbols typically come from macro-expansion, so
              ;; it is often non-trivial for the programmer to avoid such
              ;; unused vars.
              (not (intern-soft var))
              (eq ?_ (aref (symbol-name var) 0)))
       (byte-compile-log-warning (format "Unused lexical %s `%S'"
                                         varkind var))))
    ;; If it's unused, there's no point converting it into a cons-cell, even if
    ;; it's captured and mutated.
    (`(,binder ,_ t t ,_)
     (push (cons binder form) cconv-captured+mutated))
    (`(,(and binder `(,_ (function (lambda . ,_)))) nil nil nil t)
     (push (cons binder form) cconv-lambda-candidates))
    (`(,_ ,_ ,_ ,_ ,_) nil)
    (dontcare)))

(defun cconv-analyse-function (args body env parentform)
  (let* ((newvars nil)
         (freevars (list body))
         ;; We analyze the body within a new environment where all uses are
         ;; nil, so we can distinguish uses within that function from uses
         ;; outside of it.
         (envcopy
          (mapcar (lambda (vdata) (list (car vdata) nil nil nil nil)) env))
         (newenv envcopy))
    ;; Push it before recursing, so cconv-freevars-alist contains entries in
    ;; the order they'll be used by closure-convert-rec.
    (push freevars cconv-freevars-alist)
    (dolist (arg args)
      (cond
       ((byte-compile-not-lexical-var-p arg)
        (byte-compile-report-error
         (format "Argument %S is not a lexical variable" arg)))
       ((eq ?& (aref (symbol-name arg) 0)) nil) ;Ignore &rest, &optional, ...
       (t (let ((varstruct (list arg nil nil nil nil)))
            (push (cons (list arg) (cdr varstruct)) newvars)
            (push varstruct newenv)))))
    (dolist (form body)                   ;Analyse body forms.
      (cconv-analyse-form form newenv))
    ;; Summarize resulting data about arguments.
    (dolist (vardata newvars)
      (cconv-analyse-use vardata parentform "argument"))
    ;; Transfer uses collected in `envcopy' (via `newenv') back to `env';
    ;; and compute free variables.
    (while env
      (assert (and envcopy (eq (caar env) (caar envcopy))))
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

(defun cconv-analyse-form (form env)
  "Find mutated variables and variables captured by closure.
Analyse lambdas if they are suitable for lambda lifting.
- FORM is a piece of Elisp code after macroexpansion.
- ENV is an alist mapping each enclosing lexical variable to its info.
   I.e. each element has the form (VAR . (READ MUTATED CAPTURED CALLED)).
This function does not return anything but instead fills the
`cconv-captured+mutated' and `cconv-lambda-candidates' variables
and updates the data stored in ENV."
  (pcase form
					; let special form
    (`(,(and (or `let* `let) letsym) ,binders . ,body-forms)

     (let ((orig-env env)
           (newvars nil)
           (var nil)
           (value nil))
       (dolist (binder binders)
         (if (not (consp binder))
             (progn
               (setq var binder)      ; treat the form (let (x) ...) well
               (setq binder (list binder))
               (setq value nil))
           (setq var (car binder))
           (setq value (cadr binder))

           (cconv-analyse-form value (if (eq letsym 'let*) env orig-env)))

         (unless (byte-compile-not-lexical-var-p var)
           (let ((varstruct (list var nil nil nil nil)))
             (push (cons binder (cdr varstruct)) newvars)
             (push varstruct env))))

       (dolist (form body-forms)          ; Analyse body forms.
         (cconv-analyse-form form env))

       (dolist (vardata newvars)
         (cconv-analyse-use vardata form "variable"))))

					; defun special form
    (`(,(or `defun `defmacro) ,func ,vrs . ,body-forms)
     (when env
       (byte-compile-log-warning
        (format "Function %S will ignore its context %S"
                func (mapcar #'car env))
        t :warning))
     (cconv-analyse-function vrs body-forms nil form))

    (`(function (lambda ,vrs . ,body-forms))
     (cconv-analyse-function vrs body-forms env form))
     
    (`(setq . ,forms)
     ;; If a local variable (member of env) is modified by setq then
     ;; it is a mutated variable.
     (while forms
       (let ((v (assq (car forms) env))) ; v = non nil if visible
         (when v (setf (nth 2 v) t)))
       (cconv-analyse-form (cadr forms) env)
       (setq forms (cddr forms))))

    (`((lambda . ,_) . ,_)             ; first element is lambda expression
     (dolist (exp `((function ,(car form)) . ,(cdr form)))
       (cconv-analyse-form exp env)))

    (`(cond . ,cond-forms)              ; cond special form
     (dolist (forms cond-forms)
       (dolist (form forms) (cconv-analyse-form form env))))

    (`(quote . ,_) nil)                 ; quote form
    (`(function . ,_) nil)              ; same as quote

    (`(condition-case ,var ,protected-form . ,handlers)
     ;; FIXME: The bytecode for condition-case forces us to wrap the
     ;; form and handlers in closures (for handlers, it's probably
     ;; unavoidable, but not for the protected form).
     (cconv-analyse-function () (list protected-form) env form)
     (dolist (handler handlers)
       (cconv-analyse-function (if var (list var)) (cdr handler) env form)))

    ;; FIXME: The bytecode for catch forces us to wrap the body.
    (`(,(or `catch `unwind-protect) ,form . ,body)
     (cconv-analyse-form form env)
     (cconv-analyse-function () body env form))

    ;; FIXME: The bytecode for save-window-excursion and the lack of
    ;; bytecode for track-mouse forces us to wrap the body.
    (`(track-mouse . ,body)
     (cconv-analyse-function () body env form))

    (`(,(or `defconst `defvar) ,var ,value . ,_)
     (push var byte-compile-bound-variables)
     (cconv-analyse-form value env))

    (`(,(or `funcall `apply) ,fun . ,args)
     ;; Here we ignore fun because funcall and apply are the only two
     ;; functions where we can pass a candidate for lambda lifting as
     ;; argument.  So, if we see fun elsewhere, we'll delete it from
     ;; lambda candidate list.
     (let ((fdata (and (symbolp fun) (assq fun env))))
       (if fdata
           (setf (nth 4 fdata) t)
         (cconv-analyse-form fun env)))
     (dolist (form args) (cconv-analyse-form form env)))

    (`(interactive . ,forms)
     ;; These appear within the function body but they don't have access
     ;; to the function's arguments.
     ;; We could extend this to allow interactive specs to refer to
     ;; variables in the function's enclosing environment, but it doesn't
     ;; seem worth the trouble.
     (dolist (form forms) (cconv-analyse-form form nil)))
    
    (`(,_ . ,body-forms)    ; First element is a function or whatever.
     (dolist (form body-forms) (cconv-analyse-form form env)))

    ((pred symbolp)
     (let ((dv (assq form env)))        ; dv = declared and visible
       (when dv
         (setf (nth 1 dv) t))))))

(provide 'cconv)
;;; cconv.el ends here
