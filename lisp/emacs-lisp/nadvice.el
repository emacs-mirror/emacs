;;; nadvice.el --- Light-weight advice primitives for Elisp functions  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2026 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: extensions, lisp, tools
;; Version: 1.0

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

;; This package lets you add behavior (which we call "piece of advice") to
;; existing functions, like the old `advice.el' package, but with much fewer
;; bells and whistles.  It comes in 2 parts:
;;
;; - The first part lets you add/remove functions, similarly to
;;   add/remove-hook, from any "place" (i.e. as accepted by `setf') that
;;   holds a function.
;;   This part provides mainly 2 macros: `add-function' and `remove-function'.
;;
;; - The second part provides `advice-add' and `advice-remove' which are
;;   refined version of the previous macros specially tailored for the case
;;   where the place that we want to modify is a `symbol-function'.

;;; Code:

(oclosure-define (advice
                  (:predicate advice--p)
                  (:copier advice--cons (cdr))
                  (:copier advice--copy (car cdr how props)))
  car cdr how props)

(eval-when-compile
  (defmacro advice--make-how-alist (&rest args)
    `(list
      ,@(mapcar
         (lambda (arg)
           (pcase-let ((`(,how . ,body) arg))
             `(list ,how
                    (oclosure-lambda (advice (how ,how)) (&rest r)
                      ,@body)
                    ,(replace-regexp-in-string
                      "\\<car\\>" "FUNCTION"
                      (replace-regexp-in-string
                       "\\<cdr\\>" "OLDFUN"
                       (format "%S" `(lambda (&rest r) ,@body))
                       t t)
                      t t))))
         args))))

;;;; Lightweight advice/hook
(defvar advice--how-alist
  (advice--make-how-alist
   (:around (apply car cdr r))
   (:before (apply car r) (apply cdr r))
   (:after (prog1 (apply cdr r) (apply car r)))
   (:override (apply car r))
   (:after-until (or (apply cdr r) (apply car r)))
   (:after-while (and (apply cdr r) (apply car r)))
   (:before-until (or (apply car r) (apply cdr r)))
   (:before-while (and (apply car r) (apply cdr r)))
   (:filter-args (apply cdr (funcall car r)))
   (:filter-return (funcall car (apply cdr r))))
  "List of descriptions of how to add a function.
Each element has the form (HOW OCL DOC) where HOW is a keyword,
OCL is a \"prototype\" function of type `advice', and
DOC is a string where \"FUNCTION\" and \"OLDFUN\" are expected.")

(defun advice--cd*r (f)
  (while (advice--p f)
    (setq f (advice--cdr f)))
  f)

(define-obsolete-function-alias 'advice--where #'advice--how "29.1")

(defun advice--make-single-doc (flist function macrop)
  (let ((how (advice--how flist)))
    (concat
     (format "This %s has %s advice: "
             (if macrop "macro" "function")
             how)
     (let ((fun (advice--car flist)))
       (if (symbolp fun) (format-message "`%S'." fun)
         (let* ((name (cdr (assq 'name (advice--props flist))))
                (doc (documentation fun t))
                (usage (help-split-fundoc doc function)))
           (if usage (setq doc (cdr usage)))
           (if name
               (if doc
                   (format "%s\n%s" name doc)
                 (format "%s" name))
             (or doc "No documentation")))))
     "\n"
     (and
      (eq how :override)
      (concat
       (format-message
        "\nThis is an :override advice, which means that `%s' isn't\n" function)
       "run at all, and the documentation below may be irrelevant.\n")))))

(defun advice--make-docstring (function)
  "Build the raw docstring for FUNCTION, presumably advised."
  (let* ((flist (indirect-function function))
         (docfun nil)
         (macrop (eq 'macro (car-safe flist)))
         (before nil)
         (after nil))
    (when macrop
      (setq flist (cdr flist)))
    (if (and (autoloadp flist)
             (get function 'advice--pending))
        (setq after
              (advice--make-single-doc (get function 'advice--pending)
                                       function macrop))
      (while (advice--p flist)
        ;; Hack attack!  For advices installed before calling
        ;; Snarf-documentation, the integer offset into the DOC file will not
        ;; be installed in the "core unadvised function" but in the advice
        ;; object instead!  So here we try to undo the damage.
        (when (integerp (aref flist 4))
          (setq docfun flist))
        (let ((doc-bit (advice--make-single-doc flist function macrop)))
          ;; We want :overrides to go to the front, because they mean
          ;; that the doc string may be irrelevant.
          (if (eq (advice--how flist) :override)
              (setq before (concat before doc-bit))
            (setq after (concat after doc-bit))))
        (setq flist (advice--cdr flist))))
    (unless docfun
      (setq docfun flist))
    (let* ((origdoc (unless (eq function docfun) ;Avoid inf-loops.
                      (documentation docfun t)))
           (usage (help-split-fundoc origdoc function)))
      (setq usage (if (null usage)
                      (let ((arglist (help-function-arglist flist)))
                        ;; "[Arg list not available until function
                        ;; definition is loaded]", bug#21299
                        (if (stringp arglist) t
                          (help--make-usage-docstring function arglist)))
                    (setq origdoc (cdr usage))
                    (car usage)))
      (help-add-fundoc-usage
       (with-temp-buffer
         (when before
           (insert before)
           (ensure-empty-lines 1))
         (when origdoc
           (insert origdoc))
         (when after
           (ensure-empty-lines 1)
           (insert after))
         (buffer-string))
       usage))))

;; FIXME: How about renaming this to just `eval-interactive-spec'?
;; It's not specific to the advice system.
(defun advice-eval-interactive-spec (spec)
  "Evaluate the interactive spec SPEC."
  (cond
   ((stringp spec)
    ;; There's no direct access to the C code (in call-interactively) that
    ;; processes those specs, but that shouldn't stop us, should it?
    ;; FIXME: Despite appearances, this is not faithful: SPEC and
    ;; (advice-eval-interactive-spec SPEC) will behave subtly differently w.r.t
    ;; command-history (and maybe a few other details).
    (call-interactively
     ;; Sadly (lambda (&rest args) (interactive spec) args) doesn't work :-(
     (cconv--interactive-helper (lambda (&rest args) args) spec)))
   ;; ((functionp spec) (funcall spec))
   (t (eval spec))))

(defun advice--interactive-form-1 (function)
  "Like `interactive-form' but preserves the static context if needed."
  (let ((if (interactive-form function)))
    (if (not (and if (interpreted-function-p function)))
        if
      (cl-assert (eq 'interactive (car if)))
      (let ((form (cadr if)))
        (if (macroexp-const-p form)     ;Common case: a string.
            if
          ;; The interactive is expected to be run in the static context
          ;; that the function captured.
          (let ((ctx (aref function 2)))
            `(interactive
              ,(let* ((f (if (eq 'function (car-safe form)) (cadr form) form)))
                 ;; If the form jut returns a function, preserve the fact that
                 ;; it just returns a function, which is an info we use in
                 ;; `advice--make-interactive-form'.
                 (if (eq 'lambda (car-safe f))
                     (eval form ctx)
                   `(eval ',form ',ctx))))))))))

(defun advice--interactive-form (function)
  "Like `interactive-form' but tries to avoid autoloading functions."
  (if (not (and (symbolp function) (autoloadp (indirect-function function))))
      (advice--interactive-form-1 function)
    (when (commandp function)
      `(interactive (advice-eval-interactive-spec
                     (cadr (advice--interactive-form-1 ',function)))))))

(defun advice--make-interactive-form (iff ifm)
  (let* ((fspec (cadr iff)))
    (when (memq (car-safe fspec) '(function quote)) ;; Macroexpanded lambda?
      (setq fspec (eval fspec t)))
    (if (functionp fspec)
        `(funcall ',fspec ',(cadr ifm))
      (cadr (or iff ifm)))))


(cl-defmethod oclosure-interactive-form ((ad advice) &optional _)
  (let* ((car (advice--car ad))
         (cdr (advice--cdr ad))
         (ifa (advice--interactive-form car))
         (ifd (advice--interactive-form cdr)))
    (when (or ifa ifd)
      `(interactive ,(advice--make-interactive-form ifa ifd)))))

(cl-defmethod cl-print-object ((object advice) stream)
  (cl-assert (advice--p object))
  (princ "#f(advice " stream)
  (cl-print-object (advice--car object) stream)
  (princ " " stream)
  (princ (advice--how object) stream)
  (princ " " stream)
  (cl-print-object (advice--cdr object) stream)
  (let ((props (advice--props object)))
    (when props
      (princ " " stream)
      (cl-print-object props stream)))
  (princ ")" stream))

(defun advice--make (how function main props)
  "Build a function value that adds FUNCTION to MAIN at HOW.
HOW is a symbol to select an entry in `advice--how-alist'."
  (let ((fd (or (cdr (assq 'depth props)) 0))
        (md (if (advice--p main)
                (or (cdr (assq 'depth (advice--props main))) 0))))
    (if (and md (> fd md))
        ;; `function' should go deeper.
        (let ((rest (advice--make how function (advice--cdr main) props)))
          (advice--cons main rest))
      (let ((proto (assq how advice--how-alist)))
        (unless proto (error "Unknown add-function location `%S'" how))
        (advice--copy (cadr proto)
                      function main how props)))))

(defun advice--member-p (function use-name definition)
  (let ((found nil))
    (while (and (not found) (advice--p definition))
      (if (if (eq use-name :use-both)
	      (or (equal function
			 (cdr (assq 'name (advice--props definition))))
		  (equal function (advice--car definition)))
	    (equal function (if use-name
				(cdr (assq 'name (advice--props definition)))
			      (advice--car definition))))
          (setq found definition)
        (setq definition (advice--cdr definition))))
    found))

(defun advice--tweak (flist tweaker)
  (if (not (advice--p flist))
      (funcall tweaker nil flist nil)
    (let ((first (advice--car flist))
          (rest (advice--cdr flist))
          (props (advice--props flist)))
      (let ((val (funcall tweaker first rest props)))
        (if val (car val)
          (let ((nrest (advice--tweak rest tweaker)))
            (if (eq rest nrest) flist
              (advice--cons flist nrest))))))))

;;;###autoload
(defun advice--remove-function (flist function)
  (advice--tweak flist
                 (lambda (first rest props)
                   (cond ((not first) rest)
                         ((or (equal function first)
                              (equal function (cdr (assq 'name props))))
                          (list (advice--remove-function rest function)))))))

(oclosure-define (advice--forward
                  (:predicate advice--forward-p))
  "Redirect to the global value of a var.
These functions act like the t special value in buffer-local hooks.")

(defun advice--set-buffer-local (var val)
  (if (advice--forward-p val)
      (kill-local-variable var)
    (set (make-local-variable var) val)))

;;;###autoload
(defun advice--buffer-local (var)
  "Buffer-local value of VAR, presumed to contain a function."
  (declare (gv-setter advice--set-buffer-local))
  (if (local-variable-p var) (symbol-value var)
    ;; FIXME: Provide an `advice-bottom' function that's like
    ;; `advice--cd*r' but also follows through this proxy.
    (oclosure-lambda (advice--forward) (&rest args)
      (apply (default-value var) args))))

(eval-and-compile
  (defun advice--normalize-place (place)
    (cond ((eq 'local (car-safe place)) `(advice--buffer-local ,@(cdr place)))
          ((eq 'var (car-safe place))   (nth 1 place))
          ((symbolp place)              `(default-value ',place))
          (t place))))

(defun advice--make-nadvice-docstring (sym)
  "Make docstring for a nadvice function.
Modifies the function's docstring by replacing \"<<>>\" with the
description of the possible HOWs."
  (let* ((main (documentation (symbol-function sym) 'raw))
         (ud (help-split-fundoc main 'pcase))
         (doc (or (cdr ud) main))
         (col1width (apply #'max (mapcar (lambda (x)
                                           (string-width (symbol-name (car x))))
                                         advice--how-alist)))
         (table (mapconcat (lambda (x)
                             (format (format " %%-%ds %%s" col1width)
                                     (car x) (nth 2 x)))
                           advice--how-alist "\n"))
         (table (if global-prettify-symbols-mode
                    (replace-regexp-in-string "(lambda\\>" "(λ" table t t)
                  table))
         (combined-doc
          (if (not (string-match "<<>>" doc))
              doc
            (replace-match table t t doc))))
    (if ud (help-add-fundoc-usage combined-doc (car ud)) combined-doc)))

(put 'add-function 'function-documentation
     '(advice--make-nadvice-docstring 'add-function))

;;;###autoload
(defmacro add-function (how place function &optional props)
  ;; TODO:
  ;; - maybe let `how' specify some kind of predicate and use it
  ;;   to implement things like mode-local or cl-defmethod.
  ;;   Of course, that only makes sense if the predicates of all advices can
  ;;   be combined and made more efficient.
  ;; :before is like a normal add-hook on a normal hook.
  ;; :before-while is like add-hook on run-hook-with-args-until-failure.
  ;; :before-until is like add-hook on run-hook-with-args-until-success.
  ;; Same with :after-* but for (add-hook ... 'append).
  "Add a piece of advice on the function stored at PLACE.
FUNCTION describes the code to add.  HOW describes how to add it.
HOW can be explained by showing the resulting new function, as the
result of combining FUNCTION and the previous value of PLACE, which we
call OLDFUN here:
<<>>
If FUNCTION was already added, do nothing.
PROPS is an alist of additional properties, among which the following have
a special meaning:
- `name': a string or symbol.  It can be used to refer to this piece of advice.
- `depth': a number indicating a preference w.r.t ordering.
  The default depth is 0.  By convention, a depth of 100 means that
  the advice  should be innermost (i.e. at the end of the list),
  whereas a depth of -100 means that the advice should be outermost.

If PLACE is a symbol, its `default-value' will be affected.
Use (local \\='SYMBOL) if you want to apply FUNCTION to SYMBOL buffer-locally.
Use (var VAR) if you want to apply FUNCTION to the (lexical) VAR.
If you are trying to modify an existing named function rather
than a function value, you probably want to use `advice-add'
instead (see Info node `(elisp) Advising Named Functions').

If one of FUNCTION or OLDFUN is interactive, then the resulting function
is also interactive.  There are 3 cases:
- FUNCTION is not interactive: the interactive spec of OLDFUN is used.
- The interactive spec of FUNCTION is itself a function: it should take one
  argument (the interactive spec of OLDFUN, which it can pass to
  `advice-eval-interactive-spec') and return the list of arguments to use.
- Else, use the interactive spec of FUNCTION and ignore the one of OLDFUN."
  (declare
   ;;(indent 2)
   (debug (form [&or symbolp ("local" form) ("var" sexp) gv-place]
                form &optional form)))
  `(advice--add-function ,how (gv-ref ,(advice--normalize-place place))
                         ,function ,props))

;;;###autoload
(defun advice--add-function (how ref function props)
  (let* ((name (cdr (assq 'name props)))
         (a (advice--member-p (or name function) (if name t) (gv-deref ref))))
    (when a
      ;; The advice is already present.  Remove the old one, first.
      (setf (gv-deref ref)
            (advice--remove-function (gv-deref ref)
                                     (or name (advice--car a)))))
    (setf (gv-deref ref)
          (advice--make how function (gv-deref ref) props))))

;;;###autoload
(defmacro remove-function (place function)
  "Remove the FUNCTION piece of advice from PLACE.
If FUNCTION was not added to PLACE, do nothing.
Instead of FUNCTION being the actual function, it can also be the `name'
of the piece of advice."
  (declare (debug ([&or symbolp ("local" form) ("var" sexp) gv-place]
                   form)))
  (gv-letplace (getter setter) (advice--normalize-place place)
    (macroexp-let2 nil new `(advice--remove-function ,getter ,function)
      `(unless (eq ,new ,getter) ,(funcall setter new)))))

(defun advice-function-mapc (f function-def)
  "Apply F to every advice function in FUNCTION-DEF.
F is called with two arguments: the function that was added, and the
properties alist that was specified when it was added."
  (while (advice--p function-def)
    (funcall f (advice--car function-def) (advice--props function-def))
    (setq function-def (advice--cdr function-def))))

(defun advice-function-member-p (advice function-def)
  "Return non-nil if ADVICE is already in FUNCTION-DEF.
Instead of ADVICE being the actual function, it can also be the `name'
of the piece of advice."
  (advice--member-p advice :use-both function-def))

;;;; Specific application of add-function to `symbol-function' for advice.

(defun advice--subst-main (old new)
  (advice--tweak old
                 (lambda (first _rest _props) (if (not first) new))))

(defun advice--normalize (symbol def)
  (cond
   ((special-form-p def)
    ;; Not worth the trouble trying to handle this, I think.
    (error "Advice impossible: %S is a special form" symbol))
   ((and (symbolp def) (macrop def))
    (let ((newval `(macro . ,(lambda (&rest r) (macroexpand `(,def . ,r))))))
      (put symbol 'advice--saved-rewrite (cons def (cdr newval)))
      newval))
   ;; `f' might be a pure (hence read-only) cons!
   ((and (eq 'macro (car-safe def))
	 (not (ignore-errors (setcdr def (cdr def)) t)))
    (cons 'macro (cdr def)))
   (t def)))

(defsubst advice--strip-macro (x)
  (if (eq 'macro (car-safe x)) (cdr x) x))

(defun advice--symbol-function (symbol)
  ;; The value conceptually stored in `symbol-function' is split into two
  ;; parts:
  ;; - the normal function definition.
  ;; - the list of advice applied to it.
  ;; `advice--symbol-function' is intended to return the second part (i.e. the
  ;; list of advice, which includes a hole at the end which typically holds the
  ;; first part, but this function doesn't care much which value is found
  ;; there).
  ;; In the "normal" state both parts are combined into a single value stored
  ;; in the "function slot" of the symbol.  But the way they are combined is
  ;; different depending on whether the definition is a function or a macro.
  ;; Also if the function definition is nil (i.e. unbound) or is an autoload,
  ;; the second part is stashed away temporarily in the `advice--pending'
  ;; symbol property.
  (or (get symbol 'advice--pending)
      (advice--strip-macro (symbol-function symbol))))

(defun advice--defalias-fset (fsetfun symbol newdef)
  (unless fsetfun (setq fsetfun #'fset))
  ;; `newdef' shouldn't include advice wrappers, since that's what *we* manage!
  ;; So if `newdef' includes advice wrappers, it's usually because someone
  ;; naively took (symbol-function F) and then passed that back to `defalias':
  ;; let's strip them away.
  (cond
   ((advice--p newdef) (setq newdef (advice--cd*r newdef)))
   ((and (eq 'macro (car-safe newdef))
         (advice--p (cdr newdef)))
    (setq newdef `(macro . ,(advice--cd*r (cdr newdef))))))
  ;; The saved-rewrite is specific to the current value, so since we are about
  ;; to overwrite that current value with new value, the old saved-rewrite is
  ;; not relevant any more.
  (when (get symbol 'advice--saved-rewrite)
    (put symbol 'advice--saved-rewrite nil))
  (setq newdef (advice--normalize symbol newdef))
  (let ((oldadv (advice--symbol-function symbol)))
    (if (and newdef (not (autoloadp newdef)))
        (let* ((snewdef (advice--strip-macro newdef))
               (snewadv (advice--subst-main oldadv snewdef)))
          (put symbol 'advice--pending nil)
          (funcall fsetfun symbol
                   (if (eq snewdef newdef) snewadv (cons 'macro snewadv))))
      (unless (eq oldadv (get symbol 'advice--pending))
        (put symbol 'advice--pending (advice--subst-main oldadv nil)))
      (funcall fsetfun symbol newdef))))

(put 'advice-add 'function-documentation
     '(advice--make-nadvice-docstring 'advice-add))

;;;###autoload
(defun advice-add (symbol how function &optional props)
  "Like `add-function' but for the function named SYMBOL.
Contrary to `add-function', this will properly handle the cases where SYMBOL
is defined as a macro, alias, command, ...
HOW can be one of:
<<>>"
  ;; TODO:
  ;; - record the advice location, to display in describe-function.
  (let* ((f (symbol-function symbol))
	 (nf (advice--normalize symbol f)))
    (unless (eq f nf) (fset symbol nf))
    (add-function how (cond
                       ((eq (car-safe nf) 'macro) (cdr nf))
                       ;; Reasons to delay installation of the advice:
                       ;; - If the function is not yet defined, installing
                       ;;   the advice would affect `fboundp'ness.
                       ;; - the symbol-function slot of an autoloaded
                       ;;   function is not itself a function value.
                       ;; - `autoload' does nothing if the function is
                       ;;   not an autoload or undefined.
                       ((or (not nf) (autoloadp nf))
                        (get symbol 'advice--pending))
                       (t (symbol-function symbol)))
                  function props)
    ;; FIXME: We could use a defmethod on `function-documentation' instead,
    ;; except when (autoloadp nf)!
    (put symbol 'function-documentation `(advice--make-docstring ',symbol))
    (add-function :around (get symbol 'defalias-fset-function)
                  #'advice--defalias-fset))
  nil)

;;;###autoload
(defun advice-remove (symbol function)
  "Like `remove-function' but for the function named SYMBOL.
Contrary to `remove-function', this also works when SYMBOL is a macro
or an autoload and it preserves `fboundp'.
Instead of the actual function to remove, FUNCTION can also be the `name'
of the piece of advice."
  (interactive
   (let* ((pred (lambda (sym) (advice--p (advice--symbol-function sym))))
          (default (when-let* ((f (function-called-at-point))
                               ((funcall pred f)))
                     (symbol-name f)))
          (prompt (format-prompt "Remove advice from function" default))
          (symbol (intern (completing-read prompt obarray pred t nil nil default)))
          advices)
     (advice-mapc (lambda (f p)
                    (let ((k (or (alist-get 'name p) f)))
                      (push (cons
                             ;; "name" (string) and 'name (symbol) are
                             ;; considered different names so we use
                             ;; `prin1-to-string' even if the name is
                             ;; a string to distinguish between these
                             ;; two cases.
                             (prin1-to-string k)
                             ;; We use `k' here instead of `f' because
                             ;; the same advice can have multiple
                             ;; names.
                             k)
                            advices)))
                  symbol)
     (list symbol (cdr (assoc-string
                        (completing-read "Advice to remove: " advices nil t)
                        advices)))))
  (let ((f (symbol-function symbol)))
    (remove-function (cond ;This is `advice--symbol-function' but as a "place".
                      ((get symbol 'advice--pending)
                       (get symbol 'advice--pending))
                      ((eq (car-safe f) 'macro) (cdr f))
                      (t (symbol-function symbol)))
                     function)
    (unless (advice--p (advice--symbol-function symbol))
      (remove-function (get symbol 'defalias-fset-function)
                       #'advice--defalias-fset)
      (let ((asr (get symbol 'advice--saved-rewrite)))
        (and asr (eq (cdr-safe (symbol-function symbol))
                     (cdr asr))
             (fset symbol (car (get symbol 'advice--saved-rewrite)))))))
  nil)

;;;###autoload
(defmacro define-advice (symbol args &rest body)
  "Define an advice and add it to function named SYMBOL.
See `advice-add' and `add-function' for explanation of the
arguments.  If NAME is non-nil, the advice is named `SYMBOL@NAME'
and installed with the name NAME; otherwise, the advice is anonymous.

\(fn SYMBOL (HOW LAMBDA-LIST &optional NAME DEPTH) &rest BODY)"
  (declare (indent 2) (doc-string 3) (debug (sexp sexp def-body)))
  (or (listp args) (signal 'wrong-type-argument (list 'listp args)))
  (or (<= 2 (length args) 4)
      (signal 'wrong-number-of-arguments (list 2 4 (length args))))
  (let* ((how           (nth 0 args))
         (lambda-list   (nth 1 args))
         (name          (nth 2 args))
         (depth         (nth 3 args))
         (props         (append
                         (and depth `((depth . ,depth)))
                         (and name `((name . ,name)))))
         (advice (cond ((null name) `(lambda ,lambda-list ,@body))
                       ((or (stringp name) (symbolp name))
                        (intern (format "%s@%s" symbol name)))
                       (t (error "Unrecognized name spec `%S'" name)))))
    `(prog1 ,@(and (symbolp advice) `((defun ,advice ,lambda-list ,@body)))
       (advice-add ',symbol ,how #',advice ,@(and props `(',props))))))

(defun advice-mapc (fun symbol)
  "Apply FUN to every advice function in SYMBOL.
FUN is called with a two arguments: the function that was added, and the
properties alist that was specified when it was added."
  (advice-function-mapc fun (advice--symbol-function symbol)))

;;;###autoload
(defun advice-member-p (advice symbol)
  "Return non-nil if ADVICE has been added to SYMBOL.
Instead of ADVICE being the actual function, it can also be the `name'
of the piece of advice."
  (advice-function-member-p advice (advice--symbol-function symbol)))

;; When code is advised, called-interactively-p needs to be taught to skip
;; the advising frames.
;; FIXME: This Major Ugly Hack won't handle calls to called-interactively-p
;; done from the advised function if the deepest advice is an around advice!
;; In other cases (calls from an advice or calls from the advised function when
;; the deepest advice is not an around advice), it should hopefully get
;; it right.
(add-hook 'called-interactively-p-functions
          #'advice--called-interactively-skip)
(defun advice--called-interactively-skip (origi frame1 frame2)
  (let* ((i origi)
         (get-next-frame
          (lambda ()
            (setq frame1 frame2)
            (setq frame2 (backtrace-frame i #'called-interactively-p))
            ;; (message "Advice Frame %d = %S" i frame2)
            (setq i (1+ i)))))
    ;; FIXME: Adjust this for the new :filter advices, since they use `funcall'
    ;; rather than `apply'.
    ;; FIXME: Somehow this doesn't work on (advice-add :before
    ;; 'call-interactively #'ignore), see bug#3984.
    (when (and (eq (nth 1 frame2) 'apply)
               (progn
                 (funcall get-next-frame)
                 (advice--p (indirect-function (nth 1 frame2)))))
      (funcall get-next-frame)
      ;; If we now have the symbol, this was the head advice and
      ;; we're done.
      (while (advice--p (nth 1 frame1))
        ;; This was an inner advice called from some earlier advice.
        ;; The stack frames look different depending on the particular
        ;; kind of the earlier advice.
        (let ((inneradvice (nth 1 frame1)))
          (if (and (eq (nth 1 frame2) 'apply)
                   (progn
                     (funcall get-next-frame)
                     (advice--p (indirect-function
                                 (nth 1 frame2)))))
              ;; The earlier advice was something like a before/after
              ;; advice where the "next" code is called directly by the
              ;; advice--p object.
              (funcall get-next-frame)
            ;; It's apparently an around advice, where the "next" is
            ;; called by the body of the advice in any way it sees fit,
            ;; so we need to skip the frames of that body.
            (while
                (progn
                  (funcall get-next-frame)
                  (and frame2
                       (not (and (eq (nth 1 frame2) 'apply)
                                 (eq (nth 3 frame2) inneradvice))))))
            (funcall get-next-frame)
            (funcall get-next-frame))))
      (- i origi 1))))


(provide 'nadvice)
;;; nadvice.el ends here
