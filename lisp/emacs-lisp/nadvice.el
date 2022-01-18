;;; nadvice.el --- Light-weight advice primitives for Elisp functions  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2022 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: extensions, lisp, tools

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

;; The autoloads.el mechanism which adds package--builtin-versions
;; maintenance to loaddefs.el doesn't work for preloaded packages (such
;; as this one), so we have to do it by hand!
(push (purecopy '(nadvice 1 0)) package--builtin-versions)

;;;; Lightweight advice/hook
(defvar advice--where-alist
  '((:around "\300\301\302\003#\207" 5)
    (:before "\300\301\002\"\210\300\302\002\"\207" 4)
    (:after "\300\302\002\"\300\301\003\"\210\207" 5)
    (:override "\300\301\002\"\207" 4)
    (:after-until "\300\302\002\"\206\013\000\300\301\002\"\207" 4)
    (:after-while "\300\302\002\"\205\013\000\300\301\002\"\207" 4)
    (:before-until "\300\301\002\"\206\013\000\300\302\002\"\207" 4)
    (:before-while "\300\301\002\"\205\013\000\300\302\002\"\207" 4)
    (:filter-args "\300\302\301\003!\"\207" 5)
    (:filter-return "\301\300\302\003\"!\207" 5))
  "List of descriptions of how to add a function.
Each element has the form (WHERE BYTECODE STACK) where:
  WHERE is a keyword indicating where the function is added.
  BYTECODE is the corresponding byte-code that will be used.
  STACK is the amount of stack space needed by the byte-code.")

(defvar advice--bytecodes (mapcar #'cadr advice--where-alist))

(defun advice--p (object)
  (and (byte-code-function-p object)
       (eq 128 (aref object 0))
       (memq (length object) '(5 6))
       (memq (aref object 1) advice--bytecodes)
       (eq #'apply (aref (aref object 2) 0))))

(defsubst advice--car   (f) (aref (aref f 2) 1))
(defsubst advice--cdr   (f) (aref (aref f 2) 2))
(defsubst advice--props (f) (aref (aref f 2) 3))

(defun advice--cd*r (f)
  (while (advice--p f)
    (setq f (advice--cdr f)))
  f)

(defun advice--where (f)
  (let ((bytecode (aref f 1))
        (where nil))
    (dolist (elem advice--where-alist)
      (if (eq bytecode (cadr elem)) (setq where (car elem))))
    where))

(defun advice--make-single-doc (flist function macrop)
  (let ((where (advice--where flist)))
    (concat
     (format "This %s has %s advice: "
             (if macrop "macro" "function")
             where)
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
     "\n")))

(defun advice--make-docstring (function)
  "Build the raw docstring for FUNCTION, presumably advised."
  (let* ((flist (indirect-function function))
         (docfun nil)
         (macrop (eq 'macro (car-safe flist)))
         (docstring nil))
    (when macrop
      (setq flist (cdr flist)))
    (if (and (autoloadp flist)
             (get function 'advice--pending))
        (setq docstring
              (advice--make-single-doc (get function 'advice--pending)
                                       function macrop))
      (while (advice--p flist)
        ;; Hack attack!  For advices installed before calling
        ;; Snarf-documentation, the integer offset into the DOC file will not
        ;; be installed in the "core unadvised function" but in the advice
        ;; object instead!  So here we try to undo the damage.
        (when (integerp (aref flist 4))
          (setq docfun flist))
        (setq docstring (concat docstring (advice--make-single-doc
                                           flist function macrop))
              flist (advice--cdr flist))))
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
                    (setq origdoc (cdr usage)) (car usage)))
      (help-add-fundoc-usage (concat origdoc
                                     (if (string-suffix-p "\n" origdoc)
                                         "\n"
                                       "\n\n")
                                     docstring)
                             usage))))

(defun advice-eval-interactive-spec (spec)
  "Evaluate the interactive spec SPEC."
  (cond
   ((stringp spec)
    ;; There's no direct access to the C code (in call-interactively) that
    ;; processes those specs, but that shouldn't stop us, should it?
    ;; FIXME: Despite appearances, this is not faithful: SPEC and
    ;; (advice-eval-interactive-spec SPEC) will behave subtly differently w.r.t
    ;; command-history (and maybe a few other details).
    (call-interactively `(lambda (&rest args) (interactive ,spec) args)))
   ;; ((functionp spec) (funcall spec))
   (t (eval spec))))

(defun advice--interactive-form (function)
  "Like `interactive-form' but tries to avoid autoloading functions."
  (when (commandp function)
    (if (not (and (symbolp function) (autoloadp (indirect-function function))))
        (interactive-form function)
      `(interactive (advice-eval-interactive-spec
                     (cadr (interactive-form ',function)))))))

(defun advice--make-interactive-form (function main)
  ;; TODO: make it so that interactive spec can be a constant which
  ;; dynamically checks the advice--car/cdr to do its job.
  ;; For that, advice-eval-interactive-spec needs to be more faithful.
  (let* ((iff (advice--interactive-form function))
         (ifm (advice--interactive-form main))
         (fspec (cadr iff)))
    (when (eq 'function (car-safe fspec)) ;; Macroexpanded lambda?
      (setq fspec (nth 1 fspec)))
    (if (functionp fspec)
        `(funcall ',fspec ',(cadr ifm))
      (cadr (or iff ifm)))))

(defun advice--make-1 (byte-code stack-depth function main props)
  "Build a function value that adds FUNCTION to MAIN."
  (let ((adv-sig (gethash main advertised-signature-table))
        (advice
         (apply #'make-byte-code 128 byte-code
                (vector #'apply function main props) stack-depth nil
                (and (or (commandp function) (commandp main))
                     (list (advice--make-interactive-form
                            function main))))))
    (when adv-sig (puthash advice adv-sig advertised-signature-table))
    advice))

(defun advice--make (where function main props)
  "Build a function value that adds FUNCTION to MAIN at WHERE.
WHERE is a symbol to select an entry in `advice--where-alist'."
  (let ((fd (or (cdr (assq 'depth props)) 0))
        (md (if (advice--p main)
                (or (cdr (assq 'depth (advice--props main))) 0))))
    (if (and md (> fd md))
        ;; `function' should go deeper.
        (let ((rest (advice--make where function (advice--cdr main) props)))
          (advice--make-1 (aref main 1) (aref main 3)
                          (advice--car main) rest (advice--props main)))
      (let ((desc (assq where advice--where-alist)))
        (unless desc (error "Unknown add-function location `%S'" where))
        (advice--make-1 (nth 1 desc) (nth 2 desc)
                        function main props)))))

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
              (advice--make-1 (aref flist 1) (aref flist 3)
                              first nrest props))))))))

;;;###autoload
(defun advice--remove-function (flist function)
  (advice--tweak flist
                 (lambda (first rest props)
                   (cond ((not first) rest)
                         ((or (equal function first)
                              (equal function (cdr (assq 'name props))))
                          (list (advice--remove-function rest function)))))))

(defvar advice--buffer-local-function-sample nil
  "Keeps an example of the special \"run the default value\" functions.
These functions play the same role as t in buffer-local hooks, and to recognize
them, we keep a sample here against which to compare.  Each instance is
different, but `function-equal' will hopefully ignore those differences.")

(defun advice--set-buffer-local (var val)
  (if (function-equal val advice--buffer-local-function-sample)
      (kill-local-variable var)
    (set (make-local-variable var) val)))

;;;###autoload
(defun advice--buffer-local (var)
  "Buffer-local value of VAR, presumed to contain a function."
  (declare (gv-setter advice--set-buffer-local))
  (if (local-variable-p var) (symbol-value var)
    (setq advice--buffer-local-function-sample
          ;; This function acts like the t special value in buffer-local hooks.
          ;; FIXME: Provide an `advice-bottom' function that's like
          ;; `advice-cd*r' but also follows through this proxy.
          (lambda (&rest args) (apply (default-value var) args)))))

(eval-and-compile
  (defun advice--normalize-place (place)
    (cond ((eq 'local (car-safe place)) `(advice--buffer-local ,@(cdr place)))
          ((eq 'var (car-safe place))   (nth 1 place))
          ((symbolp place)              `(default-value ',place))
          (t place))))

;;;###autoload
(defmacro add-function (where place function &optional props)
  ;; TODO:
  ;; - maybe let `where' specify some kind of predicate and use it
  ;;   to implement things like mode-local or eieio-defmethod.
  ;;   Of course, that only makes sense if the predicates of all advices can
  ;;   be combined and made more efficient.
  ;; :before is like a normal add-hook on a normal hook.
  ;; :before-while is like add-hook on run-hook-with-args-until-failure.
  ;; :before-until is like add-hook on run-hook-with-args-until-success.
  ;; Same with :after-* but for (add-hook ... 'append).
  "Add a piece of advice on the function stored at PLACE.
FUNCTION describes the code to add.  WHERE describes where to add it.
WHERE can be explained by showing the resulting new function, as the
result of combining FUNCTION and the previous value of PLACE, which we
call OLDFUN here:
`:before'	(lambda (&rest r) (apply FUNCTION r) (apply OLDFUN r))
`:after'	(lambda (&rest r) (prog1 (apply OLDFUN r) (apply FUNCTION r)))
`:around'	(lambda (&rest r) (apply FUNCTION OLDFUN r))
`:override'	(lambda (&rest r) (apply FUNCTION r))
`:before-while'	(lambda (&rest r) (and (apply FUNCTION r) (apply OLDFUN r)))
`:before-until'	(lambda (&rest r) (or  (apply FUNCTION r) (apply OLDFUN r)))
`:after-while'	(lambda (&rest r) (and (apply OLDFUN r) (apply FUNCTION r)))
`:after-until'	(lambda (&rest r) (or  (apply OLDFUN r) (apply FUNCTION r)))
`:filter-args'	(lambda (&rest r) (apply OLDFUN (funcall FUNCTION r)))
`:filter-return'(lambda (&rest r) (funcall FUNCTION (apply OLDFUN r)))
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
  `(advice--add-function ,where (gv-ref ,(advice--normalize-place place))
                         ,function ,props))

(declare-function comp-subr-trampoline-install "comp")

;;;###autoload
(defun advice--add-function (where ref function props)
  (when (and (featurep 'native-compile)
             (subr-primitive-p (gv-deref ref)))
    (let ((subr-name (intern (subr-name (gv-deref ref)))))
      ;; Requiring the native compiler to advice `macroexpand' cause a
      ;; circular dependency in eager macro expansion.  uniquify is
      ;; advising `rename-buffer' while being loaded in loadup.el.
      ;; This would require the whole native compiler machinery but we
      ;; don't want to include it in the dump.  Because these two
      ;; functions are already handled in
      ;; `native-comp-never-optimize-functions' we hack the problem
      ;; this way for now :/
      (unless (memq subr-name '(macroexpand rename-buffer))
        ;; Must require explicitly as during bootstrap we have no
        ;; autoloads.
        (require 'comp)
        (comp-subr-trampoline-install subr-name))))
  (let* ((name (cdr (assq 'name props)))
         (a (advice--member-p (or name function) (if name t) (gv-deref ref))))
    (when a
      ;; The advice is already present.  Remove the old one, first.
      (setf (gv-deref ref)
            (advice--remove-function (gv-deref ref)
                                     (or name (advice--car a)))))
    (setf (gv-deref ref)
          (advice--make where function (gv-deref ref) props))))

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

;;;###autoload
(defun advice-add (symbol where function &optional props)
  "Like `add-function' but for the function named SYMBOL.
Contrary to `add-function', this will properly handle the cases where SYMBOL
is defined as a macro, alias, command, ..."
  ;; TODO:
  ;; - record the advice location, to display in describe-function.
  ;; - change all defadvice in lisp/**/*.el.
  ;; - obsolete advice.el.
  (let* ((f (symbol-function symbol))
	 (nf (advice--normalize symbol f)))
    (unless (eq f nf) (fset symbol nf))
    (add-function where (cond
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
See `advice-add' and `add-function' for explanation on the
arguments.  Note if NAME is nil the advice is anonymous;
otherwise it is named `SYMBOL@NAME'.

\(fn SYMBOL (WHERE LAMBDA-LIST &optional NAME DEPTH) &rest BODY)"
  (declare (indent 2) (doc-string 3) (debug (sexp sexp def-body)))
  (or (listp args) (signal 'wrong-type-argument (list 'listp args)))
  (or (<= 2 (length args) 4)
      (signal 'wrong-number-of-arguments (list 2 4 (length args))))
  (let* ((where         (nth 0 args))
         (lambda-list   (nth 1 args))
         (name          (nth 2 args))
         (depth         (nth 3 args))
         (props         (and depth `((depth . ,depth))))
         (advice (cond ((null name) `(lambda ,lambda-list ,@body))
                       ((or (stringp name) (symbolp name))
                        (intern (format "%s@%s" symbol name)))
                       (t (error "Unrecognized name spec `%S'" name)))))
    `(prog1 ,@(and (symbolp advice) `((defun ,advice ,lambda-list ,@body)))
       (advice-add ',symbol ,where #',advice ,@(and props `(',props))))))

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
