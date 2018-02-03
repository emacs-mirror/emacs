;;; macroexp.el --- Additional macro-expansion support -*- lexical-binding: t -*-
;;
;; Copyright (C) 2004-2018 Free Software Foundation, Inc.
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

;; Bound by the top-level `macroexpand-all', and modified to include any
;; macros defined by `defmacro'.
(defvar macroexpand-all-environment nil)

(defun macroexp--cons (car cdr original-cons)
  "Return (CAR . CDR), using ORIGINAL-CONS if possible."
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

(defun macroexp--compiler-macro (handler form)
  (condition-case err
      (apply handler form (cdr form))
    (error
     (message "Compiler-macro error for %S: %S" (car form) err)
           form)))

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

(defun macroexp--compiling-p ()
  "Return non-nil if we're macroexpanding for the compiler."
  ;; FIXME: ¡¡Major Ugly Hack!! To determine whether the output of this
  ;; macro-expansion will be processed by the byte-compiler, we check
  ;; circumstantial evidence.
  (member '(declare-function . byte-compile-macroexpand-declare-function)
          macroexpand-all-environment))

(defvar macroexp--warned (make-hash-table :test #'equal :weakness 'key))

(defun macroexp--warn-and-return (msg form &optional compile-only)
  (let ((when-compiled (lambda () (byte-compile-warn "%s" msg))))
    (cond
     ((null msg) form)
     ((macroexp--compiling-p)
      (if (gethash form macroexp--warned)
          ;; Already wrapped this exp with a warning: avoid inf-looping
          ;; where we keep adding the same warning onto `form' because
          ;; macroexpand-all gets right back to macroexpanding `form'.
          form
        (puthash form form macroexp--warned)
        `(progn
           (macroexp--funcall-if-compiled ',when-compiled)
           ,form)))
     (t
      (unless compile-only
        (message "%s%s" (if (stringp load-file-name)
                            (concat (file-relative-name load-file-name) ": ")
                          "")
                 msg))
      form))))

(defun macroexp--obsolete-warning (fun obsolescence-data type)
  (let ((instead (car obsolescence-data))
        (asof (nth 2 obsolescence-data)))
    (format-message
     "`%s' is an obsolete %s%s%s" fun type
     (if asof (concat " (as of " asof ")") "")
     (cond ((stringp instead) (concat "; " (substitute-command-keys instead)))
           (instead (format-message "; use `%s' instead." instead))
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

(defun macroexp-macroexpand (form env)
  "Like `macroexpand' but checking obsolescence."
  (let ((new-form
         (macroexpand form env)))
    (if (and (not (eq form new-form))   ;It was a macro call.
             (car-safe form)
             (symbolp (car form))
             (get (car form) 'byte-obsolete-info)
             (or (not (fboundp 'byte-compile-warning-enabled-p))
                 (byte-compile-warning-enabled-p 'obsolete)))
        (let* ((fun (car form))
               (obsolete (get fun 'byte-obsolete-info)))
          (macroexp--warn-and-return
           (macroexp--obsolete-warning
            fun obsolete
            (if (symbolp (symbol-function fun))
                "alias" "macro"))
           new-form))
      new-form)))

(defun macroexp--expand-all (form)
  "Expand all macros in FORM.
This is an internal version of `macroexpand-all'.
Assumes the caller has bound `macroexpand-all-environment'."
  (if (eq (car-safe form) 'backquote-list*)
      ;; Special-case `backquote-list*', as it is normally a macro that
      ;; generates exceedingly deep expansions from relatively shallow input
      ;; forms.  We just process it `in reverse' -- first we expand all the
      ;; arguments, _then_ we expand the top-level definition.
      (macroexpand (macroexp--all-forms form 1)
		   macroexpand-all-environment)
    ;; Normal form; get its expansion, and then expand arguments.
    (setq form (macroexp-macroexpand form macroexpand-all-environment))
    (pcase form
      (`(cond . ,clauses)
       (macroexp--cons 'cond (macroexp--all-clauses clauses) form))
      (`(condition-case . ,(or `(,err ,body . ,handlers) dontcare))
       (macroexp--cons
        'condition-case
        (macroexp--cons err
                        (macroexp--cons (macroexp--expand-all body)
                                        (macroexp--all-clauses handlers 1)
                                        (cddr form))
                        (cdr form))
        form))
      (`(,(or `defvar `defconst) . ,_) (macroexp--all-forms form 2))
      (`(function ,(and f `(lambda . ,_)))
       (macroexp--cons 'function
                       (macroexp--cons (macroexp--all-forms f 2)
                                       nil
                                       (cdr form))
                       form))
      (`(,(or `function `quote) . ,_) form)
      (`(,(and fun (or `let `let*)) . ,(or `(,bindings . ,body) dontcare))
       (macroexp--cons fun
                       (macroexp--cons (macroexp--all-clauses bindings 1)
                                       (macroexp--all-forms body)
                                       (cdr form))
                       form))
      (`(,(and fun `(lambda . ,_)) . ,args)
       ;; Embedded lambda in function position.
       (macroexp--cons (macroexp--all-forms fun 2)
                       (macroexp--all-forms args)
                       form))
      ;; The following few cases are for normal function calls that
      ;; are known to funcall one of their arguments.  The byte
      ;; compiler has traditionally handled these functions specially
      ;; by treating a lambda expression quoted by `quote' as if it
      ;; were quoted by `function'.  We make the same transformation
      ;; here, so that any code that cares about the difference will
      ;; see the same transformation.
      ;; First arg is a function:
      (`(,(and fun (or `funcall `apply `mapcar `mapatoms `mapconcat `mapc))
         ',(and f `(lambda . ,_)) . ,args)
       (macroexp--warn-and-return
        (format "%s quoted with ' rather than with #'"
                (list 'lambda (nth 1 f) '...))
        (macroexp--expand-all `(,fun ,f . ,args))))
      ;; Second arg is a function:
      (`(,(and fun (or `sort)) ,arg1 ',(and f `(lambda . ,_)) . ,args)
       (macroexp--warn-and-return
        (format "%s quoted with ' rather than with #'"
                (list 'lambda (nth 1 f) '...))
        (macroexp--expand-all `(,fun ,arg1 ,f . ,args))))
      (`(funcall #',(and f (pred symbolp)) . ,args)
       ;; Rewrite (funcall #'foo bar) to (foo bar), in case `foo'
       ;; has a compiler-macro.
       (macroexp--expand-all `(,f . ,args)))
      (`(,func . ,_)
       ;; Macro expand compiler macros.  This cannot be delayed to
       ;; byte-optimize-form because the output of the compiler-macro can
       ;; use macros.
       (let ((handler (function-get func 'compiler-macro)))
         (if (null handler)
             ;; No compiler macro.  We just expand each argument (for
             ;; setq/setq-default this works alright because the variable names
             ;; are symbols).
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
                     (macroexp--expand-all newform)))
               (macroexp--expand-all newform))))))

      (_ form))))

;;;###autoload
(defun macroexpand-all (form &optional environment)
  "Return result of expanding macros at all levels in FORM.
If no macros are expanded, FORM is returned unchanged.
The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation."
  (let ((macroexpand-all-environment environment))
    (macroexp--expand-all form)))

;;; Handy functions to use in macros.

(defun macroexp-parse-body (body)
  "Parse a function BODY into (DECLARATIONS . EXPS)."
  (let ((decls ()))
    (while (and (cdr body)
                (let ((e (car body)))
                  (or (stringp e)
                      (memq (car-safe e)
                            '(:documentation declare interactive cl-declare)))))
      (push (pop body) decls))
    (cons (nreverse decls) body)))

(defun macroexp-progn (exps)
  "Return an expression equivalent to `(progn ,@EXPS)."
  (if (cdr exps) `(progn ,@exps) (car exps)))

(defun macroexp-unprogn (exp)
  "Turn EXP into a list of expressions to execute in sequence.
Never returns an empty list."
  (if (eq (car-safe exp) 'progn) (or (cdr exp) '(nil)) (list exp)))

(defun macroexp-let* (bindings exp)
  "Return an expression equivalent to `(let* ,bindings ,exp)."
  (cond
   ((null bindings) exp)
   ((eq 'let* (car-safe exp)) `(let* (,@bindings ,@(cadr exp)) ,@(cddr exp)))
   (t `(let* ,bindings ,exp))))

(defun macroexp-if (test then else)
  "Return an expression equivalent to `(if ,TEST ,THEN ,ELSE)."
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
             (t ,@(nthcdr 3 else))))))
   ((eq (car-safe else) 'cond)
    `(cond (,test ,@(macroexp-unprogn then)) ,@(cdr else)))
   ;; Invert the test if that lets us reduce the depth of the tree.
   ((memq (car-safe then) '(if cond)) (macroexp-if `(not ,test) else then))
   (t `(if ,test ,then ,@(macroexp-unprogn else)))))

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
  "Bind each binding in BINDINGS as `macroexp-let2' does."
  (declare (indent 2) (debug (sexp (&rest (sexp form)) body)))
  (pcase-exhaustive bindings
    (`nil (macroexp-progn body))
    (`((,var ,exp) . ,tl)
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
        (message "Warning: Eager macro-expansion skipped due to cycle:\n  %s"
                 (mapconcat #'prin1-to-string (nreverse bt) " => ")))
      (push 'skip macroexp--pending-eager-loads)
      form))
   (t
    (condition-case err
        (let ((macroexp--pending-eager-loads
               (cons load-file-name macroexp--pending-eager-loads)))
          (if full-p
              (macroexpand-all form)
            (macroexpand form)))
      (error
       ;; Hopefully this shouldn't happen thanks to the cycle detection,
       ;; but in case it does happen, let's catch the error and give the
       ;; code a chance to macro-expand later.
       (message "Eager macro-expansion failure: %S" err)
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
              (and (not (byte-code-function-p
                         (symbol-function 'macroexpand-all)))
                   (locate-library "macroexp.elc")
                   (load "macroexp.elc")))))

(provide 'macroexp)

;;; macroexp.el ends here
