;;; cconv.el --- Closure conversion for statically scoped Emacs lisp. -*- lexical-binding: nil -*-

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
;; (lambda (v1 ...) ... fv ...)  =>
;; (curry (lambda (env v1 ...) ... env ...) env)
;; if the function has only 1 free variable
;;
;; and finally
;; (lambda (v1 ...) ... fv1 fv2 ...)  =>
;; (curry (lambda (env v1 ..) .. (aref env 0) (aref env 1) ..) (vector fv1 fv2))
;; if the function has 2 or more free variables.
;;
;; If the function has no free variables, we don't do anything.
;;
;; If a variable is mutated (updated by setq), and it is used in a closure
;; we wrap it's definition with list: (list val) and we also replace
;; var => (car var) wherever this variable is used, and also
;; (setq var value) => (setcar var value) where it is updated.
;;
;; If defun argument is closure mutable, we letbind it and wrap it's
;; definition with list.
;; (defun foo (... mutable-arg ...) ...) =>
;; (defun foo (... m-arg ...) (let ((m-arg (list m-arg))) ...))
;;
;;; Code:

(eval-when-compile (require 'cl))

(defconst cconv-liftwhen 3
  "Try to do lambda lifting if the number of arguments + free variables
is less than this number.")
(defvar cconv-mutated nil
  "List of mutated variables in current form")
(defvar cconv-captured nil
  "List of closure captured variables in current form")
(defvar cconv-captured+mutated nil
  "An intersection between cconv-mutated and cconv-captured lists.")
(defvar cconv-lambda-candidates nil
  "List of candidates for lambda lifting.
Each candidate has the form (VAR INCLOSURE BINDER PARENTFORM).")

(defun cconv-not-lexical-var-p (var)
  (or (not (symbolp var))               ; form is not a list
      (special-variable-p var)
      ;; byte-compile-bound-variables normally holds both the
      ;; dynamic and lexical vars, but the bytecomp.el should
      ;; only call us at the top-level so there shouldn't be
      ;; any lexical vars in it here.
      (memq var byte-compile-bound-variables)
      (memq var '(nil t))
      (keywordp var)))

(defun cconv-freevars (form &optional fvrs)
  "Find all free variables of given form.
Arguments:
-- FORM is a piece of Elisp code after macroexpansion.
-- FVRS(optional) is a list of variables already found.  Used for recursive tree
traversal

Returns a list of free variables."
  ;; If a leaf in the tree is a symbol, but it is not a global variable, not a
  ;; keyword, not 'nil or 't we consider this leaf as a variable.
  ;; Free variables are the variables that are not declared above in this tree.
  ;; For example free variables of (lambda (a1 a2 ..) body-forms) are
  ;; free variables of body-forms excluding a1, a2 ..
  ;; Free variables of (let ((v1 ..) (v2) ..)) body-forms) are
  ;; free variables of body-forms excluding v1, v2 ...
  ;; and so on.

  ;; A list of free variables already found(FVRS) is passed in parameter
  ;; to try to use cons or push where possible, and to minimize the usage
  ;; of append.

  ;; This function can return duplicates (because we use 'append instead
  ;; of union of two sets - for performance reasons).
  (pcase form
    (`(let ,varsvalues . ,body-forms)   ; let special form
     (let ((fvrs-1 '()))
       (dolist (exp body-forms)
         (setq fvrs-1 (cconv-freevars exp fvrs-1)))
       (dolist (elm varsvalues)
         (setq fvrs-1 (delq (if (consp elm) (car elm) elm) fvrs-1)))
       (setq fvrs (nconc fvrs-1 fvrs))
       (dolist (exp varsvalues)
         (when (consp exp) (setq fvrs (cconv-freevars (cadr exp) fvrs))))
       fvrs))

    (`(let* ,varsvalues . ,body-forms)  ; let* special form
     (let ((vrs '())
           (fvrs-1 '()))
       (dolist (exp varsvalues)
         (if (consp exp)
             (progn
               (setq fvrs-1 (cconv-freevars (cadr exp) fvrs-1))
               (dolist (elm vrs) (setq fvrs-1 (delq elm fvrs-1)))
               (push (car exp) vrs))
           (progn
             (dolist (elm vrs) (setq fvrs-1 (delq elm fvrs-1)))
             (push exp vrs))))
       (dolist (exp body-forms)
         (setq fvrs-1 (cconv-freevars exp fvrs-1)))
       (dolist (elm vrs) (setq fvrs-1 (delq elm fvrs-1)))
       (append fvrs fvrs-1)))

    (`((lambda . ,_) . ,_)             ; first element is lambda expression
     (dolist (exp `((function ,(car form)) . ,(cdr form)))
       (setq fvrs (cconv-freevars exp fvrs))) fvrs)

    (`(cond . ,cond-forms)              ; cond special form
     (dolist (exp1 cond-forms)
       (dolist (exp2 exp1)
         (setq fvrs (cconv-freevars exp2 fvrs)))) fvrs)

    (`(quote . ,_) fvrs)                ; quote form

    (`(function . ((lambda ,vars . ,body-forms)))
     (let ((functionform (cadr form)) (fvrs-1 '()))
       (dolist (exp body-forms)
         (setq fvrs-1 (cconv-freevars exp fvrs-1)))
       (dolist (elm vars) (setq fvrs-1 (delq elm fvrs-1)))
       (append fvrs fvrs-1)))           ; function form

    (`(function . ,_) fvrs)             ; same as quote
					;condition-case
    (`(condition-case ,var ,protected-form . ,conditions-bodies)
     (let ((fvrs-1 '()))
       (dolist (exp conditions-bodies)
         (setq fvrs-1 (cconv-freevars (cadr exp) fvrs-1)))
       (setq fvrs-1 (delq var fvrs-1))
       (setq fvrs-1 (cconv-freevars protected-form fvrs-1))
       (append fvrs fvrs-1)))

    (`(,(and sym (or `defun `defconst `defvar)) . ,_)
     ;; We call cconv-freevars only for functions(lambdas)
     ;; defun, defconst, defvar are not allowed to be inside
     ;; a function (lambda).
     ;; FIXME: should be a byte-compile-report-error!
     (error "Invalid form: %s inside a function" sym))

    (`(,_ . ,body-forms)    ; First element is (like) a function.
     (dolist (exp body-forms)
       (setq fvrs (cconv-freevars exp fvrs))) fvrs)

    (_ (if (cconv-not-lexical-var-p form)
           fvrs
         (cons form fvrs)))))

;;;###autoload
(defun cconv-closure-convert (form &optional toplevel)
  ;; cconv-closure-convert-rec has a lot of parameters that are
  ;; whether useless for user, whether they should contain
  ;; specific data like a list of closure mutables or the list
  ;; of lambdas suitable for lifting.
  ;;
  ;; That's why this function exists.
  "Main entry point for non-toplevel forms.
-- FORM is a piece of Elisp code after macroexpansion.
-- TOPLEVEL(optional) is a boolean variable, true if we are at the root of AST

Returns a form where all lambdas don't have any free variables."
  (message "Entering cconv-closure-convert...")
  (let ((cconv-mutated '())
	(cconv-lambda-candidates '())
	(cconv-captured '())
	(cconv-captured+mutated '()))
    ;; Analyse form - fill these variables with new information
    (cconv-analyse-form form '() 0)
    ;; Calculate an intersection of cconv-mutated and cconv-captured
    (dolist (mvr cconv-mutated)
      (when (memq mvr cconv-captured)   ;
        (push mvr cconv-captured+mutated)))
    (cconv-closure-convert-rec
     form                               ; the tree
     '()                                ;
     '()                                ; fvrs initially empty
     '()                                ; envs initially empty
     '()
     toplevel)))                 ; true if the tree is a toplevel form

;;;###autoload
(defun cconv-closure-convert-toplevel (form)
  "Entry point for toplevel forms.
-- FORM is a piece of Elisp code after macroexpansion.

Returns a form where all lambdas don't have any free variables."
  ;; we distinguish toplevel forms to treat def(un|var|const) correctly.
  (cconv-closure-convert form t))

(defun cconv-closure-convert-rec
  (form emvrs fvrs envs lmenvs defs-are-legal)
  ;; This function actually rewrites the tree.
  "Eliminates all free variables of all lambdas in given forms.
Arguments:
-- FORM is a piece of Elisp code after macroexpansion.
-- LMENVS is a list of environments used for lambda-lifting. Initially empty.
-- EMVRS is a list that contains mutated variables that are visible
within current environment.
-- ENVS is an environment(list of free variables) of current closure.
Initially empty.
-- FVRS is a list of variables to substitute in each context.
Initially empty.
-- DEFS-ARE-LEGAL is a boolean variable, true if def(un|var|const)
can be used in this form(e.g. toplevel form)

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
    (`(,(and letsym (or `let* `let)) ,varsvalues . ,body-forms)

					; let and let* special forms
     (let ((body-forms-new '())
           (varsvalues-new '())
           ;; next for variables needed for delayed push
           ;; because we should process <value(s)>
           ;; before we change any arguments
           (lmenvs-new '())             ;needed only in case of let
           (emvrs-new '())              ;needed only in case of let
           (emvr-push)                  ;needed only in case of let*
           (lmenv-push))                ;needed only in case of let*

       (dolist (elm varsvalues)       ;begin of dolist over varsvalues
         (let (var value elm-new iscandidate ismutated)
           (if (consp elm)    ; (let (v1) ...) => (let ((v1 nil)) ...)
               (progn
                 (setq var (car elm))
                 (setq value (cadr elm)))
             (setq var elm))

           ;; Check if var is a candidate for lambda lifting
           (let ((lcandid cconv-lambda-candidates))
             (while (and lcandid (not iscandidate))
               (when (and (eq (caar lcandid) var)
                          (eq (caddar lcandid) elm)
                          (eq (cadr (cddar lcandid)) form))
                 (setq iscandidate t))
               (setq lcandid (cdr lcandid))))

                                    ; declared variable is a candidate
                                    ; for lambda lifting
           (if iscandidate
               (let* ((func (cadr elm)) ; function(lambda) itself
					; free variables
                      (fv (delete-dups (cconv-freevars func '())))
                      (funcvars (append fv (cadadr func))) ;function args
                      (funcbodies (cddadr func)) ; function bodies
                      (funcbodies-new '()))
					; lambda lifting condition
                 (if (or (not fv) (< cconv-liftwhen (length funcvars)))
					; do not lift
                     (setq
                      elm-new
                      `(,var
                        ,(cconv-closure-convert-rec
                          func emvrs fvrs envs lmenvs nil)))
					; lift
                   (progn
                     (dolist (elm2 funcbodies)
                       (push            ; convert function bodies
                        (cconv-closure-convert-rec
                         elm2 emvrs nil envs lmenvs nil)
                        funcbodies-new))
                     (if (eq letsym 'let*)
                         (setq lmenv-push (cons var fv))
                       (push (cons var fv) lmenvs-new))
					; push lifted function

                     (setq elm-new
                           `(,var
                             (function .
                                       ((lambda ,funcvars .
                                          ,(reverse funcbodies-new)))))))))

                                  ;declared variable is not a function
             (progn
               ;; Check if var is mutated
               (let ((lmutated cconv-captured+mutated))
                 (while (and lmutated (not ismutated))
                   (when (and (eq (caar lmutated) var)
                              (eq (caddar lmutated) elm)
                              (eq (cadr (cddar lmutated)) form))
                     (setq ismutated t))
                   (setq lmutated (cdr lmutated))))
               (if ismutated
                   (progn               ; declared variable is mutated
                     (setq elm-new
                           `(,var (list ,(cconv-closure-convert-rec
                                          value emvrs
                                          fvrs envs lmenvs nil))))
                     (if (eq letsym 'let*)
                         (setq emvr-push var)
                       (push var emvrs-new)))
                 (progn
                   (setq
                    elm-new
                    `(,var              ; else
                      ,(cconv-closure-convert-rec
                        value emvrs fvrs envs lmenvs nil)))))))

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
                 (push `(,closedsym ,var) varsvalues-new))))
           ;; we push the element after redefined free variables
           ;; are processes. this is important to avoid the bug
           ;; when free variable and the function have the same
           ;; name
           (push elm-new varsvalues-new)

           (when (eq letsym 'let*)      ; update fvrs
             (setq fvrs (remq var fvrs))
             (setq emvrs (remq var emvrs)) ; remove if redefined
             (when emvr-push
               (push emvr-push emvrs)
               (setq emvr-push nil))
             (let (lmenvs-1)     ; remove var from lmenvs if redefined
               (dolist (iter lmenvs)
                 (when (not (assq var lmenvs))
                   (push iter lmenvs-1)))
               (setq lmenvs lmenvs-1))
             (when lmenv-push
               (push lmenv-push lmenvs)
               (setq lmenv-push nil)))
           ))                          ; end of dolist over varsvalues
       (when (eq letsym 'let)

         (let (var fvrs-1 emvrs-1 lmenvs-1)
           ;; Here we update emvrs, fvrs and lmenvs lists
           (dolist (vr fvrs)
					; safely remove
             (when (not (assq vr varsvalues-new)) (push vr fvrs-1)))
           (setq fvrs fvrs-1)
           (dolist (vr emvrs)
					; safely remove
             (when (not (assq vr varsvalues-new)) (push vr emvrs-1)))
           (setq emvrs emvrs-1)
					; push new
           (setq emvrs (append emvrs emvrs-new))
           (dolist (vr lmenvs)
             (when (not (assq (car vr) varsvalues-new))
               (push vr lmenvs-1)))
           (setq lmenvs (append lmenvs lmenvs-new)))

         ;; Here we do the same letbinding as for let* above
         ;; to avoid situation when a free variable of a lambda lifted
         ;; function got redefined.

         (let ((new-lmenv)
               (var nil)
               (closedsym nil)
               (letbinds '())
               (fvrs-new))              ; list of (closed-var var)
           (dolist (elm varsvalues)
             (setq var (if (consp elm) (car elm) elm))

             (let ((lmenvs-1 lmenvs))   ; just to avoid manipulating
               (dolist (lmenv lmenvs-1) ; the counter inside the loop
                 (when (memq var (cdr lmenv))
                   (setq closedsym (make-symbol
                                    (concat "closed-"
                                            (symbol-name var))))

                   (setq new-lmenv (list (car lmenv)))
                   (dolist (frv (cdr lmenv)) (if (eq frv var)
                                                 (push closedsym new-lmenv)
                                               (push frv new-lmenv)))
                   (setq new-lmenv (reverse new-lmenv))
                   (setq lmenvs (remq lmenv lmenvs))
                   (push new-lmenv lmenvs)
                   (push `(,closedsym ,var) letbinds)
                   ))))
           (setq varsvalues-new (append varsvalues-new letbinds))))

       (dolist (elm body-forms)         ; convert body forms
         (push (cconv-closure-convert-rec
                elm emvrs fvrs envs lmenvs nil)
               body-forms-new))
       `(,letsym ,(reverse varsvalues-new) . ,(reverse body-forms-new))))
					;end of let let* forms

                                  ; first element is lambda expression
    (`(,(and `(lambda . ,_) fun) . ,other-body-forms)

     (let ((other-body-forms-new '()))
       (dolist (elm other-body-forms)
         (push (cconv-closure-convert-rec
                elm emvrs fvrs envs lmenvs nil)
               other-body-forms-new))
       (cons
        (cadr
         (cconv-closure-convert-rec
          (list 'function fun) emvrs fvrs envs lmenvs nil))
        (reverse other-body-forms-new))))

    (`(cond . ,cond-forms)              ; cond special form
     (let ((cond-forms-new '()))
       (dolist (elm cond-forms)
         (push (let ((elm-new '()))
                 (dolist (elm-2 elm)
                   (push
                    (cconv-closure-convert-rec
                     elm-2 emvrs fvrs envs lmenvs nil)
                    elm-new))
                 (reverse elm-new))
               cond-forms-new))
       (cons 'cond
             (reverse cond-forms-new))))

    (`(quote . ,_) form)                ; quote form

    (`(function . ((lambda ,vars . ,body-forms))) ; function form
     (let (fvrs-new)      ; we remove vars from fvrs
       (dolist (elm fvrs) ;i use such a tricky way to avoid side effects
         (when (not (memq elm vars))
           (push elm fvrs-new)))
       (setq fvrs fvrs-new))
     (let* ((fv (delete-dups (cconv-freevars form '())))
            (leave fvrs) ; leave = non nil if we should leave env unchanged
            (body-forms-new '())
            (letbind '())
            (mv nil)
            (envector nil))
       (when fv
         ;; Here we form our environment vector.
         ;; If outer closure contains all
         ;; free variables of this function(and nothing else)
         ;; then we use the same environment vector as for outer closure,
         ;; i.e. we leave the environment vector unchanged
         ;; otherwise we build a new environmet vector
         (if (eq (length envs) (length fv))
             (let ((fv-temp fv))
               (while (and fv-temp leave)
                 (when (not (memq (car fv-temp) fvrs)) (setq leave nil))
                 (setq fv-temp (cdr fv-temp))))
           (setq leave nil))

         (if (not leave)
             (progn
               (dolist (elm fv)
                 (push
                  (cconv-closure-convert-rec
                   elm (remq elm emvrs) fvrs envs lmenvs nil)
                  envector))         ; process vars for closure vector
               (setq envector (reverse envector))
               (setq envs fv))
           (setq envector `(env)))	; leave unchanged
         (setq fvrs fv))                ; update substitution list

       ;; the difference between envs and fvrs is explained
       ;; in comment in the beginning of the function
       (dolist (elm cconv-captured+mutated) ; find mutated arguments
         (setq mv (car elm))                ; used in inner closures
         (when (and (memq mv vars) (eq form (caddr elm)))
           (progn (push mv emvrs)
                  (push `(,mv (list ,mv)) letbind))))
       (dolist (elm body-forms)         ; convert function body
         (push (cconv-closure-convert-rec
                elm emvrs fvrs envs lmenvs nil)
               body-forms-new))

       (setq body-forms-new
             (if letbind `((let ,letbind . ,(reverse body-forms-new)))
               (reverse body-forms-new)))

       (cond
					;if no freevars - do nothing
        ((null envector)
         `(function (lambda ,vars . ,body-forms-new)))
                               ; 1 free variable - do not build vector
        ((null (cdr envector))
         `(curry
           (function (lambda (env . ,vars) . ,body-forms-new))
           ,(car envector)))
                                   ; >=2 free variables - build vector
        (t
         `(curry
           (function (lambda (env . ,vars) . ,body-forms-new))
           (vector . ,envector))))))

    (`(function . ,_) form)             ; same as quote

					;defconst, defvar
    (`(,(and sym (or `defconst `defvar)) ,definedsymbol . ,body-forms)

     (if defs-are-legal
         (let ((body-forms-new '()))
           (dolist (elm body-forms)
             (push (cconv-closure-convert-rec
                    elm emvrs fvrs envs lmenvs nil)
                   body-forms-new))
           (setq body-forms-new (reverse body-forms-new))
           `(,sym ,definedsymbol . ,body-forms-new))
       (error "Invalid form: %s inside a function" sym)))

					;defun, defmacro
    (`(,(and sym (or `defun `defmacro))
       ,func ,vars . ,body-forms)
     (if defs-are-legal
         (let ((body-new '())       ; the whole body
               (body-forms-new '()) ; body w\o docstring and interactive
               (letbind '()))
					; find mutable arguments
           (let ((lmutated cconv-captured+mutated) ismutated)
             (dolist (elm vars)
               (setq ismutated nil)
               (while (and lmutated (not ismutated))
                 (when (and (eq (caar lmutated) elm)
                            (eq (cadar lmutated) form))
                   (setq ismutated t))
                 (setq lmutated (cdr lmutated)))
               (when ismutated
                 (push elm letbind)
                 (push elm emvrs))))
                                            ;transform body-forms
           (when (stringp (car body-forms)) ; treat docstring well
             (push (car body-forms) body-new)
             (setq body-forms (cdr body-forms)))
           (when (eq (car-safe (car body-forms)) 'interactive)
             (push
              (cconv-closure-convert-rec
               (car body-forms)
               emvrs fvrs envs lmenvs nil) body-new)
             (setq body-forms (cdr body-forms)))

           (dolist (elm body-forms)
             (push (cconv-closure-convert-rec
                    elm emvrs fvrs envs lmenvs nil)
                   body-forms-new))
           (setq body-forms-new (reverse body-forms-new))

           (if letbind
					; letbind mutable arguments
               (let ((varsvalues-new '()))
                 (dolist (elm letbind) (push `(,elm (list ,elm))
                                             varsvalues-new))
                 (push `(let ,(reverse varsvalues-new) .
                             ,body-forms-new) body-new)
                 (setq body-new (reverse body-new)))
             (setq body-new (append (reverse body-new) body-forms-new)))

           `(,sym ,func ,vars . ,body-new))

       (error "Invalid form: defun inside a function")))
					;condition-case
    (`(condition-case ,var ,protected-form . ,conditions-bodies)
     (let ((conditions-bodies-new '()))
       (setq fvrs (remq var fvrs))
       (dolist (elm conditions-bodies)
         (push (let ((elm-new '()))
                 (dolist (elm-2 (cdr elm))
                   (push
                    (cconv-closure-convert-rec
                     elm-2 emvrs fvrs envs lmenvs nil)
                    elm-new))
                 (cons (car elm) (reverse elm-new)))
               conditions-bodies-new))
       `(condition-case
            ,var
            ,(cconv-closure-convert-rec
              protected-form emvrs fvrs envs lmenvs nil)
          . ,(reverse conditions-bodies-new))))

    (`(setq . ,forms)                   ; setq special form
     (let (prognlist sym sym-new value)
       (while forms
         (setq sym (car forms))
         (setq sym-new (cconv-closure-convert-rec
                        sym
                        (remq sym emvrs) fvrs envs lmenvs nil))
         (setq value
               (cconv-closure-convert-rec
                (cadr forms) emvrs fvrs envs lmenvs nil))
         (if (memq sym emvrs)
             (push `(setcar ,sym-new ,value) prognlist)
           (if (symbolp sym-new)
               (push `(setq ,sym-new ,value) prognlist)
             (push `(set ,sym-new ,value) prognlist)))
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
                      fvrs envs lmenvs nil)
                     processed-fv))
             (setq processed-fv (reverse processed-fv))
             (dolist (elm args)
               (push (cconv-closure-convert-rec
                      elm emvrs fvrs envs lmenvs nil)
                     args-new))
             (setq args-new (append processed-fv (reverse args-new)))
             (setq fun (cconv-closure-convert-rec
                        fun emvrs fvrs envs lmenvs nil))
             `(,callsym ,fun . ,args-new))
         (let ((cdr-new '()))
           (dolist (elm (cdr form))
             (push (cconv-closure-convert-rec
                    elm emvrs fvrs envs lmenvs nil)
                   cdr-new))
           `(,callsym . ,(reverse cdr-new))))))

    (`(,func . ,body-forms)    ; first element is function or whatever
                               ; function-like forms are:
                               ; or, and, if, progn, prog1, prog2,
                               ; while, until
     (let ((body-forms-new '()))
       (dolist (elm body-forms)
         (push (cconv-closure-convert-rec
                elm emvrs fvrs envs lmenvs defs-are-legal)
               body-forms-new))
       (setq body-forms-new (reverse body-forms-new))
       `(,func . ,body-forms-new)))

    (_
     (let ((free (memq form fvrs)))
       (if free                         ;form is a free variable
           (let* ((numero (- (length fvrs) (length free)))
                  (var '()))
             (assert numero)
             (if (null (cdr envs))
                 (setq var 'env)
					;replace form =>
					;(aref env #)
               (setq var `(aref env ,numero)))
             (if (memq form emvrs) ; form => (car (aref env #)) if mutable
                 `(car ,var)
               var))
         (if (memq form emvrs)         ; if form is a mutable variable
             `(car ,form)              ; replace form => (car form)
           form))))))

(defun cconv-analyse-function (args body env parentform inclosure)
  (dolist (arg args)
    (cond
     ((cconv-not-lexical-var-p arg)
      (byte-compile-report-error
       (format "Argument %S is not a lexical variable" arg)))
     ((eq ?& (aref (symbol-name arg) 0)) nil) ;Ignore &rest, &optional, ...
     (t (push (list arg inclosure parentform) env)))) ;Push vrs to vars.
  (dolist (form body)                   ;Analyse body forms.
    (cconv-analyse-form form env inclosure)))

(defun cconv-analyse-form (form env inclosure)
  "Find mutated variables and variables captured by closure.  Analyse
lambdas if they are suitable for lambda lifting.
-- FORM is a piece of Elisp code after macroexpansion.
-- ENV is a list of variables visible in current lexical environment.
  Each entry has the form (VAR INCLOSURE BINDER PARENTFORM)
  for let-bound vars and (VAR INCLOSURE PARENTFORM) for function arguments.
-- INCLOSURE is the nesting level within lambdas."
  (pcase form
					; let special form
    (`(,(and (or `let* `let) letsym) ,binders . ,body-forms)

     (let ((orig-env env)
           (var nil)
           (value nil))
       (dolist (binder binders)
         (if (not (consp binder))
             (progn
               (setq var binder)      ; treat the form (let (x) ...) well
               (setq value nil))
           (setq var (car binder))
           (setq value (cadr binder))

           (cconv-analyse-form value (if (eq letsym 'let*) env orig-env)
                               inclosure))

         (unless (cconv-not-lexical-var-p var)
           (let ((varstruct (list var inclosure binder form)))
             (push varstruct env)       ; Push a new one.

             (pcase value
               (`(function (lambda . ,_))
                ;; If var is a function push it to lambda list.
                (push varstruct cconv-lambda-candidates)))))))

     (dolist (form body-forms)          ; Analyse body forms.
       (cconv-analyse-form form env inclosure)))

					; defun special form
    (`(,(or `defun `defmacro) ,func ,vrs . ,body-forms)
     (when env
       (byte-compile-log-warning
        (format "Function %S will ignore its context %S"
                func (mapcar #'car env))
        t :warning))
     (cconv-analyse-function vrs body-forms nil form 0))

    (`(function (lambda ,vrs . ,body-forms))
     (cconv-analyse-function vrs body-forms env form (1+ inclosure)))
     
    (`(setq . ,forms)
     ;; If a local variable (member of env) is modified by setq then
     ;; it is a mutated variable.
     (while forms
       (let ((v (assq (car forms) env))) ; v = non nil if visible
         (when v
           (push v cconv-mutated)
           ;; Delete from candidate list for lambda lifting.
           (setq cconv-lambda-candidates (delq v cconv-lambda-candidates))
           (unless (eq inclosure (cadr v)) ;Bound in a different closure level.
             (push v cconv-captured))))
       (cconv-analyse-form (cadr forms) env inclosure)
       (setq forms (cddr forms))))

    (`((lambda . ,_) . ,_)             ; first element is lambda expression
     (dolist (exp `((function ,(car form)) . ,(cdr form)))
       (cconv-analyse-form exp env inclosure)))

    (`(cond . ,cond-forms)              ; cond special form
     (dolist (forms cond-forms)
       (dolist (form forms)
         (cconv-analyse-form form env inclosure))))

    (`(quote . ,_) nil)                 ; quote form
    (`(function . ,_) nil)              ; same as quote

    (`(condition-case ,var ,protected-form . ,handlers)
     ;; FIXME: The bytecode for condition-case forces us to wrap the
     ;; form and handlers in closures (for handlers, it's probably
     ;; unavoidable, but not for the protected form).
     (setq inclosure (1+ inclosure))
     (cconv-analyse-form protected-form env inclosure)
     (push (list var inclosure form) env)
     (dolist (handler handlers)
       (dolist (form (cdr handler))
         (cconv-analyse-form form env inclosure))))

    ;; FIXME: The bytecode for catch forces us to wrap the body.
    (`(,(or `catch `unwind-protect) ,form . ,body)
     (cconv-analyse-form form env inclosure)
     (setq inclosure (1+ inclosure))
     (dolist (form body)
       (cconv-analyse-form form env inclosure)))

    ;; FIXME: The bytecode for save-window-excursion and the lack of
    ;; bytecode for track-mouse forces us to wrap the body.
    (`(,(or `save-window-excursion `track-mouse) . ,body)
     (setq inclosure (1+ inclosure))
     (dolist (form body)
       (cconv-analyse-form form env inclosure)))

    (`(,(or `defconst `defvar) ,var ,value . ,_)
     (push var byte-compile-bound-variables)
     (cconv-analyse-form value env inclosure))

    (`(,(or `funcall `apply) ,fun . ,args)
     ;; Here we ignore fun because funcall and apply are the only two
     ;; functions where we can pass a candidate for lambda lifting as
     ;; argument.  So, if we see fun elsewhere, we'll delete it from
     ;; lambda candidate list.
     (if (symbolp fun)
         (let ((lv (assq fun cconv-lambda-candidates)))
           (when lv
             (unless (eq (cadr lv) inclosure)
               (push lv cconv-captured)
               ;; If this funcall and the definition of fun are in
               ;; different closures - we delete fun from candidate
               ;; list, because it is too complicated to manage free
               ;; variables in this case.
               (setq cconv-lambda-candidates
                     (delq lv cconv-lambda-candidates)))))
       (cconv-analyse-form fun env inclosure))
     (dolist (form args)
       (cconv-analyse-form form env inclosure)))

    (`(,_ . ,body-forms)    ; First element is a function or whatever.
     (dolist (form body-forms)
       (cconv-analyse-form form env inclosure)))

    ((pred symbolp)
     (let ((dv (assq form env)))        ; dv = declared and visible
       (when dv
         (unless (eq inclosure (cadr dv)) ; capturing condition
           (push dv cconv-captured))
         ;; Delete lambda if it is found here, since it escapes.
         (setq cconv-lambda-candidates
               (delq dv cconv-lambda-candidates)))))))

(provide 'cconv)
;;; cconv.el ends here
