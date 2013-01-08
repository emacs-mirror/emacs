;;; nadvice.el --- Light-weight advice primitives for Elisp functions  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2013 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: extensions, lisp, tools
;; Package: emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package lets you add behavior (which we call "piece of advice") to
;; existing functions, like the old `advice.el' package, but with much fewer
;; bells ans whistles.  It comes in 2 parts:
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

;;;; Lightweight advice/hook
(defvar advice--where-alist
  '((:around "\300\301\302\003#\207" 5)
    (:before "\300\301\002\"\210\300\302\002\"\207" 4)
    (:after "\300\302\002\"\300\301\003\"\210\207" 5)
    (:after-until "\300\302\002\"\206\013\000\300\301\002\"\207" 4)
    (:after-while "\300\302\002\"\205\013\000\300\301\002\"\207" 4)
    (:before-until "\300\301\002\"\206\013\000\300\302\002\"\207" 4)
    (:before-while "\300\301\002\"\205\013\000\300\302\002\"\207" 4))
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

(defun advice--make-docstring (_string function)
  "Build the raw doc-string of SYMBOL, presumably advised."
  (let ((flist (indirect-function function))
        (docstring nil))
    (if (eq 'macro (car-safe flist)) (setq flist (cdr flist)))
    (while (advice--p flist)
      (let ((bytecode (aref flist 1))
            (where nil))
        (dolist (elem advice--where-alist)
          (if (eq bytecode (cadr elem)) (setq where (car elem))))
        (setq docstring
              (concat
               docstring
               (propertize (format "%s advice: " where)
                           'face 'warning)
               (let ((fun (advice--car flist)))
                 (if (symbolp fun) (format "`%S'" fun)
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
      (setq flist (advice--cdr flist)))
    (if docstring (setq docstring (concat docstring "\n")))
    (let* ((origdoc (unless (eq function flist) ;Avoid inf-loops.
                      (documentation flist t)))
           (usage (help-split-fundoc origdoc function)))
      (setq usage (if (null usage)
                      (let ((arglist (help-function-arglist flist)))
                        (format "%S" (help-make-usage function arglist)))
                    (setq origdoc (cdr usage)) (car usage)))
      (help-add-fundoc-usage (concat docstring origdoc) usage))))

(defvar advice--docstring
  ;; Can't eval-when-compile nor use defconst because it then gets pure-copied,
  ;; which drops the text-properties.
  ;;(eval-when-compile
  (propertize "Advised function"
              'dynamic-docstring-function #'advice--make-docstring)) ;; )

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

(defun advice--make-interactive-form (function main)
  ;; TODO: make it so that interactive spec can be a constant which
  ;; dynamically checks the advice--car/cdr to do its job.
  ;; For that, advice-eval-interactive-spec needs to be more faithful.
  ;; FIXME: The calls to interactive-form below load autoloaded functions
  ;; too eagerly.
  (let ((fspec (cadr (interactive-form function))))
    (when (eq 'function (car-safe fspec)) ;; Macroexpanded lambda?
      (setq fspec (nth 1 fspec)))
    (if (functionp fspec)
        `(funcall ',fspec
                  ',(cadr (interactive-form main)))
  (cadr (or (interactive-form function)
                (interactive-form main))))))

(defsubst advice--make-1 (byte-code stack-depth function main props)
  "Build a function value that adds FUNCTION to MAIN."
  (let ((adv-sig (gethash main advertised-signature-table))
        (advice
         (apply #'make-byte-code 128 byte-code
                (vector #'apply function main props) stack-depth
                advice--docstring
                (when (or (commandp function) (commandp main))
                  (list (advice--make-interactive-form
                         function main))))))
    (when adv-sig (puthash advice adv-sig advertised-signature-table))
    advice))

(defun advice--make (where function main props)
  "Build a function value that adds FUNCTION to MAIN at WHERE.
WHERE is a symbol to select an entry in `advice--where-alist'."
  (let ((desc (assq where advice--where-alist)))
    (unless desc (error "Unknown add-function location `%S'" where))
    (advice--make-1 (nth 1 desc) (nth 2 desc)
                    function main props)))

(defun advice--member-p (function definition)
  (let ((found nil))
    (while (and (not found) (advice--p definition))
      (if (or (equal function (advice--car definition))
              (equal function (cdr (assq 'name (advice--props definition)))))
          (setq found t)
        (setq definition (advice--cdr definition))))
    found))

(defun advice--tweak (flist tweaker)
  (if (not (advice--p flist))
      (funcall tweaker nil flist nil)
    (let ((first (advice--car flist))
          (rest (advice--cdr flist))
          (props (advice--props flist)))
      (or (funcall tweaker first rest props)
          (let ((nrest (advice--tweak rest tweaker)))
            (if (eq rest nrest) flist
              (advice--make-1 (aref flist 1) (aref flist 3)
                              first nrest props)))))))

;;;###autoload
(defun advice--remove-function (flist function)
  (advice--tweak flist
                 (lambda (first rest props)
                   (if (or (not first)
                           (equal function first)
                           (equal function (cdr (assq 'name props))))
                       rest))))

(defvar advice--buffer-local-function-sample nil)

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
          (lambda (&rest args) (apply (default-value var) args)))))

;;;###autoload
(defmacro add-function (where place function &optional props)
  ;; TODO:
  ;; - obsolete with-wrapper-hook (mostly requires buffer-local support).
  ;; - provide some kind of control over ordering.  E.g. debug-on-entry, ELP
  ;;   and tracing want to stay first.
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
`:before-while'	(lambda (&rest r) (and (apply FUNCTION r) (apply OLDFUN r)))
`:before-until'	(lambda (&rest r) (or  (apply FUNCTION r) (apply OLDFUN r)))
`:after-while'	(lambda (&rest r) (and (apply OLDFUN r) (apply FUNCTION r)))
`:after-until'	(lambda (&rest r) (or  (apply OLDFUN r) (apply FUNCTION r)))
If FUNCTION was already added, do nothing.
PROPS is an alist of additional properties, among which the following have
a special meaning:
- `name': a string or symbol.  It can be used to refer to this piece of advice.

PLACE cannot be a simple variable.  Instead it should either be
\(default-value 'VAR) or (local 'VAR) depending on whether FUNCTION
should be applied to VAR buffer-locally or globally.

If one of FUNCTION or OLDFUN is interactive, then the resulting function
is also interactive.  There are 3 cases:
- FUNCTION is not interactive: the interactive spec of OLDFUN is used.
- The interactive spec of FUNCTION is itself a function: it should take one
  argument (the interactive spec of OLDFUN, which it can pass to
  `advice-eval-interactive-spec') and return the list of arguments to use.
- Else, use the interactive spec of FUNCTION and ignore the one of OLDFUN."
  (declare (debug t)) ;;(indent 2)
  (cond ((eq 'local (car-safe place))
         (setq place `(advice--buffer-local ,@(cdr place))))
        ((symbolp place)
         (error "Use (default-value '%S) or (local '%S)" place place)))
  `(advice--add-function ,where (gv-ref ,place) ,function ,props))

;;;###autoload
(defun advice--add-function (where ref function props)
  (unless (advice--member-p function (gv-deref ref))
    (setf (gv-deref ref)
          (advice--make where function (gv-deref ref) props))))

(defmacro remove-function (place function)
  "Remove the FUNCTION piece of advice from PLACE.
If FUNCTION was not added to PLACE, do nothing.
Instead of FUNCTION being the actual function, it can also be the `name'
of the piece of advice."
  (declare (debug t))
  (cond ((eq 'local (car-safe place))
         (setq place `(advice--buffer-local ,@(cdr place))))
        ((symbolp place)
         (error "Use (default-value '%S) or (local '%S)" place place)))
  (gv-letplace (getter setter) place
    (macroexp-let2 nil new `(advice--remove-function ,getter ,function)
      `(unless (eq ,new ,getter) ,(funcall setter new)))))

;;;; Specific application of add-function to `symbol-function' for advice.

(defun advice--subst-main (old new)
  (advice--tweak old
                 (lambda (first _rest _props) (if (not first) new))))

(defun advice--normalize (symbol def)
  (cond
   ((special-form-p def)
    ;; Not worth the trouble trying to handle this, I think.
    (error "advice-add failure: %S is a special form" symbol))
   ((and (symbolp def)
	 (eq 'macro (car-safe (ignore-errors (indirect-function def)))))
    (let ((newval (cons 'macro (cdr (indirect-function def)))))
      (put symbol 'advice--saved-rewrite (cons def newval))
      newval))
   ;; `f' might be a pure (hence read-only) cons!
   ((and (eq 'macro (car-safe def))
	 (not (ignore-errors (setcdr def (cdr def)) t)))
    (cons 'macro (cdr def)))
   (t def)))

(defsubst advice--strip-macro (x)
  (if (eq 'macro (car-safe x)) (cdr x) x))

(defun advice--defalias-fset (fsetfun symbol newdef)
  (when (get symbol 'advice--saved-rewrite)
    (put symbol 'advice--saved-rewrite nil))
  (setq newdef (advice--normalize symbol newdef))
  (let* ((olddef (advice--strip-macro
		  (if (fboundp symbol) (symbol-function symbol))))
         (oldadv
          (cond
	   ((null (get symbol 'advice--pending))
	    (or olddef
		(progn
		  (message "Delayed advice activation failed for %s: no data"
			   symbol)
		  nil)))
	   ((or (not olddef) (autoloadp olddef))
	    (prog1 (get symbol 'advice--pending)
	      (put symbol 'advice--pending nil)))
           (t (message "Dropping left-over advice--pending for %s" symbol)
              (put symbol 'advice--pending nil)
              olddef))))
    (let* ((snewdef (advice--strip-macro newdef))
	   (snewadv (advice--subst-main oldadv snewdef)))
      (funcall (or fsetfun #'fset) symbol
	       (if (eq snewdef newdef) snewadv (cons 'macro snewadv))))))
    

;;;###autoload
(defun advice-add (symbol where function &optional props)
  "Like `add-function' but for the function named SYMBOL.
Contrary to `add-function', this will properly handle the cases where SYMBOL
is defined as a macro, alias, command, ..."
  ;; TODO:
  ;; - record the advice location, to display in describe-function.
  ;; - change all defadvice in lisp/**/*.el.
  ;; - rewrite advice.el on top of this.
  ;; - obsolete advice.el.
  (let* ((f (and (fboundp symbol) (symbol-function symbol)))
	 (nf (advice--normalize symbol f)))
    (unless (eq f nf) ;; Most importantly, if nf == nil!
      (fset symbol nf))
    (add-function where (cond
                         ((eq (car-safe nf) 'macro) (cdr nf))
                         ;; Reasons to delay installation of the advice:
                         ;; - If the function is not yet defined, installing
                         ;;   the advice would affect `fboundp'ness.
                         ;; - If it's an autoloaded command,
                         ;;   advice--make-interactive-form would end up
                         ;;   loading the command eagerly.
                         ;; - `autoload' does nothing if the function is
                         ;;   not an autoload or undefined.
                         ((or (not nf) (autoloadp nf))
                          (get symbol 'advice--pending))
                         (t (symbol-function symbol)))
                  function props)
    (add-function :around (get symbol 'defalias-fset-function)
                  #'advice--defalias-fset))
  nil)

;;;###autoload
(defun advice-remove (symbol function)
  "Like `remove-function' but for the function named SYMBOL.
Contrary to `remove-function', this will work also when SYMBOL is a macro
and it will not signal an error if SYMBOL is not `fboundp'.
Instead of the actual function to remove, FUNCTION can also be the `name'
of the piece of advice."
  (when (fboundp symbol)
    (let ((f (symbol-function symbol)))
      ;; Can't use the `if' place here, because the body is too large,
      ;; resulting in use of code that only works with lexical-scoping.
      (remove-function (if (eq (car-safe f) 'macro)
                           (cdr f)
                         (symbol-function symbol))
                       function)
      (unless (advice--p
               (if (eq (car-safe f) 'macro) (cdr f) (symbol-function symbol)))
        ;; Not advised any more.
        (remove-function (get symbol 'defalias-fset-function)
                         #'advice--defalias-fset)
        (if (eq (symbol-function symbol)
                (cdr (get symbol 'advice--saved-rewrite)))
            (fset symbol (car (get symbol 'advice--saved-rewrite))))))
    nil))

;; (defun advice-mapc (fun symbol)
;;   "Apply FUN to every function added as advice to SYMBOL.
;; FUN is called with a two arguments: the function that was added, and the
;; properties alist that was specified when it was added."
;;   (let ((def (or (get symbol 'advice--pending)
;;                  (if (fboundp symbol) (symbol-function symbol)))))
;;     (while (advice--p def)
;;       (funcall fun (advice--car def) (advice--props def))
;;       (setq def (advice--cdr def)))))

;;;###autoload
(defun advice-member-p (advice function-name)
  "Return non-nil if ADVICE has been added to FUNCTION-NAME.
Instead of ADVICE being the actual function, it can also be the `name'
of the piece of advice."
  (advice--member-p advice
                    (or (get function-name 'advice--pending)
			(advice--strip-macro
			 (if (fboundp function-name)
			     (symbol-function function-name))))))

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
            (setq frame2 (internal--called-interactively-p--get-frame i))
            ;; (message "Advice Frame %d = %S" i frame2)
            (setq i (1+ i)))))
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
                  (not (and (eq (nth 1 frame2) 'apply)
                            (eq (nth 3 frame2) inneradvice)))))
            (funcall get-next-frame)
            (funcall get-next-frame))))
      (- i origi 1))))


(provide 'nadvice)
;;; nadvice.el ends here
