;; cl-macs.el --- Common Lisp extensions for GNU Emacs Lisp (part four)

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Version: 2.02
;; Keywords: extensions

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; This package works with Emacs 18, Emacs 19, and Lucid Emacs 19.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains the portions of the Common Lisp extensions
;; package which should be autoloaded, but need only be present
;; if the compiler or interpreter is used---this file is not
;; necessary for executing compiled code.

;; See cl.el for Change Log.


;;; Code:

(or (memq 'cl-19 features)
    (error "Tried to load `cl-macs' before `cl'!"))


;;; We define these here so that this file can compile without having
;;; loaded the cl.el file already.

(defmacro cl-push (x place) (list 'setq place (list 'cons x place)))
(defmacro cl-pop (place)
  (list 'car (list 'prog1 place (list 'setq place (list 'cdr place)))))
(defmacro cl-pop2 (place)
  (list 'prog1 (list 'car (list 'cdr place))
	(list 'setq place (list 'cdr (list 'cdr place)))))
(put 'cl-push 'edebug-form-spec 'edebug-sexps)
(put 'cl-pop 'edebug-form-spec 'edebug-sexps)
(put 'cl-pop2 'edebug-form-spec 'edebug-sexps)

(defvar cl-emacs-type)
(defvar cl-optimize-safety)
(defvar cl-optimize-speed)


;;; This kludge allows macros which use cl-transform-function-property
;;; to be called at compile-time.

(require
 (progn
   (or (fboundp 'defalias) (fset 'defalias 'fset))
   (or (fboundp 'cl-transform-function-property)
       (defalias 'cl-transform-function-property
	 (function (lambda (n p f)
		     (list 'put (list 'quote n) (list 'quote p)
			   (list 'function (cons 'lambda f)))))))
   (car (or features (setq features (list 'cl-kludge))))))


;;; Initialization.

(defvar cl-old-bc-file-form nil)

;; Patch broken Emacs 18 compiler (re top-level macros).
;; Emacs 19 compiler doesn't need this patch.
;; Also, undo broken definition of `eql' that uses same bytecode as `eq'.
(defun cl-compile-time-init ()
  (setq cl-old-bc-file-form (symbol-function 'byte-compile-file-form))
  (or (fboundp 'byte-compile-flush-pending)   ; Emacs 19 compiler?
      (defalias 'byte-compile-file-form
	(function
	 (lambda (form)
	   (setq form (macroexpand form byte-compile-macro-environment))
	   (if (eq (car-safe form) 'progn)
	       (cons 'progn (mapcar 'byte-compile-file-form (cdr form)))
	     (funcall cl-old-bc-file-form form))))))
  (put 'eql 'byte-compile 'cl-byte-compile-compiler-macro)
  (run-hooks 'cl-hack-bytecomp-hook))


;;; Symbols.

(defvar *gensym-counter*)
(defun gensym (&optional arg)
  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\"."
  (let ((prefix (if (stringp arg) arg "G"))
	(num (if (integerp arg) arg
	       (prog1 *gensym-counter*
		 (setq *gensym-counter* (1+ *gensym-counter*))))))
    (make-symbol (format "%s%d" prefix num))))

(defun gentemp (&optional arg)
  "Generate a new interned symbol with a unique name.
The name is made by appending a number to PREFIX, default \"G\"."
  (let ((prefix (if (stringp arg) arg "G"))
	name)
    (while (intern-soft (setq name (format "%s%d" prefix *gensym-counter*)))
      (setq *gensym-counter* (1+ *gensym-counter*)))
    (intern name)))


;;; Program structure.

(defmacro defun* (name args &rest body)
  "(defun* NAME ARGLIST [DOCSTRING] BODY...): define NAME as a function.
Like normal `defun', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (block NAME ...)."
  (let* ((res (cl-transform-lambda (cons args body) name))
	 (form (list* 'defun name (cdr res))))
    (if (car res) (list 'progn (car res) form) form)))

(defmacro defmacro* (name args &rest body)
  "(defmacro* NAME ARGLIST [DOCSTRING] BODY...): define NAME as a macro.
Like normal `defmacro', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (block NAME ...)."
  (let* ((res (cl-transform-lambda (cons args body) name))
	 (form (list* 'defmacro name (cdr res))))
    (if (car res) (list 'progn (car res) form) form)))

(defmacro function* (func)
  "(function* SYMBOL-OR-LAMBDA): introduce a function.
Like normal `function', except that if argument is a lambda form, its
ARGLIST allows full Common Lisp conventions."
  (if (eq (car-safe func) 'lambda)
      (let* ((res (cl-transform-lambda (cdr func) 'cl-none))
	     (form (list 'function (cons 'lambda (cdr res)))))
	(if (car res) (list 'progn (car res) form) form))
    (list 'function func)))

(defun cl-transform-function-property (func prop form)
  (let ((res (cl-transform-lambda form func)))
    (append '(progn) (cdr (cdr (car res)))
	    (list (list 'put (list 'quote func) (list 'quote prop)
			(list 'function (cons 'lambda (cdr res))))))))

(defconst lambda-list-keywords
  '(&optional &rest &key &allow-other-keys &aux &whole &body &environment))

(defvar cl-macro-environment nil)
(defvar bind-block) (defvar bind-defs) (defvar bind-enquote)
(defvar bind-inits) (defvar bind-lets) (defvar bind-forms)

(defun cl-transform-lambda (form bind-block)
  (let* ((args (car form)) (body (cdr form))
	 (bind-defs nil) (bind-enquote nil)
	 (bind-inits nil) (bind-lets nil) (bind-forms nil)
	 (header nil) (simple-args nil))
    (while (or (stringp (car body)) (eq (car-safe (car body)) 'interactive))
      (cl-push (cl-pop body) header))
    (setq args (if (listp args) (copy-list args) (list '&rest args)))
    (let ((p (last args))) (if (cdr p) (setcdr p (list '&rest (cdr p)))))
    (if (setq bind-defs (cadr (memq '&cl-defs args)))
	(setq args (delq '&cl-defs (delq bind-defs args))
	      bind-defs (cadr bind-defs)))
    (if (setq bind-enquote (memq '&cl-quote args))
	(setq args (delq '&cl-quote args)))
    (if (memq '&whole args) (error "&whole not currently implemented"))
    (let* ((p (memq '&environment args)) (v (cadr p)))
      (if p (setq args (nconc (delq (car p) (delq v args))
			      (list '&aux (list v 'cl-macro-environment))))))
    (while (and args (symbolp (car args))
		(not (memq (car args) '(nil &rest &body &key &aux)))
		(not (and (eq (car args) '&optional)
			  (or bind-defs (consp (cadr args))))))
      (cl-push (cl-pop args) simple-args))
    (or (eq bind-block 'cl-none)
	(setq body (list (list* 'block bind-block body))))
    (if (null args)
	(list* nil (nreverse simple-args) (nconc (nreverse header) body))
      (if (memq '&optional simple-args) (cl-push '&optional args))
      (cl-do-arglist args nil (- (length simple-args)
				 (if (memq '&optional simple-args) 1 0)))
      (setq bind-lets (nreverse bind-lets))
      (list* (and bind-inits (list* 'eval-when '(compile load eval)
				    (nreverse bind-inits)))
	     (nconc (nreverse simple-args)
		    (list '&rest (car (cl-pop bind-lets))))
	     (nconc (nreverse header)
		    (list (nconc (list 'let* bind-lets)
				 (nreverse bind-forms) body)))))))

(defun cl-do-arglist (args expr &optional num)   ; uses bind-*
  (if (nlistp args)
      (if (or (memq args lambda-list-keywords) (not (symbolp args)))
	  (error "Invalid argument name: %s" args)
	(cl-push (list args expr) bind-lets))
    (setq args (copy-list args))
    (let ((p (last args))) (if (cdr p) (setcdr p (list '&rest (cdr p)))))
    (let ((p (memq '&body args))) (if p (setcar p '&rest)))
    (if (memq '&environment args) (error "&environment used incorrectly"))
    (let ((save-args args)
	  (restarg (memq '&rest args))
	  (safety (if (cl-compiling-file) cl-optimize-safety 3))
	  (keys nil)
	  (laterarg nil) (exactarg nil) minarg)
      (or num (setq num 0))
      (if (listp (cadr restarg))
	  (setq restarg (gensym "--rest--"))
	(setq restarg (cadr restarg)))
      (cl-push (list restarg expr) bind-lets)
      (if (eq (car args) '&whole)
	  (cl-push (list (cl-pop2 args) restarg) bind-lets))
      (let ((p args))
	(setq minarg restarg)
	(while (and p (not (memq (car p) lambda-list-keywords)))
	  (or (eq p args) (setq minarg (list 'cdr minarg)))
	  (setq p (cdr p)))
	(if (memq (car p) '(nil &aux))
	    (setq minarg (list '= (list 'length restarg)
			       (length (ldiff args p)))
		  exactarg (not (eq args p)))))
      (while (and args (not (memq (car args) lambda-list-keywords)))
	(let ((poparg (list (if (or (cdr args) (not exactarg)) 'pop 'car)
			    restarg)))
	  (cl-do-arglist
	   (cl-pop args)
	   (if (or laterarg (= safety 0)) poparg
	     (list 'if minarg poparg
		   (list 'signal '(quote wrong-number-of-arguments)
			 (list 'list (and (not (eq bind-block 'cl-none))
					  (list 'quote bind-block))
			       (list 'length restarg)))))))
	(setq num (1+ num) laterarg t))
      (while (and (eq (car args) '&optional) (cl-pop args))
	(while (and args (not (memq (car args) lambda-list-keywords)))
	  (let ((arg (cl-pop args)))
	    (or (consp arg) (setq arg (list arg)))
	    (if (cddr arg) (cl-do-arglist (nth 2 arg) (list 'and restarg t)))
	    (let ((def (if (cdr arg) (nth 1 arg)
			 (or (car bind-defs)
			     (nth 1 (assq (car arg) bind-defs)))))
		  (poparg (list 'pop restarg)))
	      (and def bind-enquote (setq def (list 'quote def)))
	      (cl-do-arglist (car arg)
			     (if def (list 'if restarg poparg def) poparg))
	      (setq num (1+ num))))))
      (if (eq (car args) '&rest)
	  (let ((arg (cl-pop2 args)))
	    (if (consp arg) (cl-do-arglist arg restarg)))
	(or (eq (car args) '&key) (= safety 0) exactarg
	    (cl-push (list 'if restarg
			   (list 'signal '(quote wrong-number-of-arguments)
				 (list 'list
				       (and (not (eq bind-block 'cl-none))
					    (list 'quote bind-block))
				       (list '+ num (list 'length restarg)))))
		     bind-forms)))
      (while (and (eq (car args) '&key) (cl-pop args))
	(while (and args (not (memq (car args) lambda-list-keywords)))
	  (let ((arg (cl-pop args)))
	    (or (consp arg) (setq arg (list arg)))
	    (let* ((karg (if (consp (car arg)) (caar arg)
			   (intern (format ":%s" (car arg)))))
		   (varg (if (consp (car arg)) (cadar arg) (car arg)))
		   (def (if (cdr arg) (cadr arg)
			  (or (car bind-defs) (cadr (assq varg bind-defs)))))
		   (look (list 'memq (list 'quote karg) restarg)))
	      (and def bind-enquote (setq def (list 'quote def)))
	      (if (cddr arg)
		  (let* ((temp (or (nth 2 arg) (gensym)))
			 (val (list 'car (list 'cdr temp))))
		    (cl-do-arglist temp look)
		    (cl-do-arglist varg
				   (list 'if temp
					 (list 'prog1 val (list 'setq temp t))
					 def)))
		(cl-do-arglist
		 varg
		 (list 'car
		       (list 'cdr
			     (if (null def)
				 look
			       (list 'or look
				     (if (eq (cl-const-expr-p def) t)
					 (list
					  'quote
					  (list nil (cl-const-expr-val def)))
				       (list 'list nil def))))))))
	      (cl-push karg keys)
	      (if (= (aref (symbol-name karg) 0) ?:)
		  (progn (set karg karg)
			 (cl-push (list 'setq karg (list 'quote karg))
				  bind-inits)))))))
      (setq keys (nreverse keys))
      (or (and (eq (car args) '&allow-other-keys) (cl-pop args))
	  (null keys) (= safety 0)
	  (let* ((var (gensym "--keys--"))
		 (allow '(:allow-other-keys))
		 (check (list
			 'while var
			 (list
			  'cond
			  (list (list 'memq (list 'car var)
				      (list 'quote (append keys allow)))
				(list 'setq var (list 'cdr (list 'cdr var))))
			  (list (list 'car
				      (list 'cdr
					    (list 'memq (cons 'quote allow)
						  restarg)))
				(list 'setq var nil))
			  (list t
				(list
				 'error
				 (format "Keyword argument %%s not one of %s"
					 keys)
				 (list 'car var)))))))
	    (cl-push (list 'let (list (list var restarg)) check) bind-forms)))
      (while (and (eq (car args) '&aux) (cl-pop args))
	(while (and args (not (memq (car args) lambda-list-keywords)))
	  (if (consp (car args))
	      (if (and bind-enquote (cadar args))
		  (cl-do-arglist (caar args)
				 (list 'quote (cadr (cl-pop args))))
		(cl-do-arglist (caar args) (cadr (cl-pop args))))
	    (cl-do-arglist (cl-pop args) nil))))
      (if args (error "Malformed argument list %s" save-args)))))

(defun cl-arglist-args (args)
  (if (nlistp args) (list args)
    (let ((res nil) (kind nil) arg)
      (while (consp args)
	(setq arg (cl-pop args))
	(if (memq arg lambda-list-keywords) (setq kind arg)
	  (if (eq arg '&cl-defs) (cl-pop args)
	    (and (consp arg) kind (setq arg (car arg)))
	    (and (consp arg) (cdr arg) (eq kind '&key) (setq arg (cadr arg)))
	    (setq res (nconc res (cl-arglist-args arg))))))
      (nconc res (and args (list args))))))

(defmacro destructuring-bind (args expr &rest body)
  (let* ((bind-lets nil) (bind-forms nil) (bind-inits nil)
	 (bind-defs nil) (bind-block 'cl-none))
    (cl-do-arglist (or args '(&aux)) expr)
    (append '(progn) bind-inits
	    (list (nconc (list 'let* (nreverse bind-lets))
			 (nreverse bind-forms) body)))))


;;; The `eval-when' form.

(defvar cl-not-toplevel nil)

(defmacro eval-when (when &rest body)
  "(eval-when (WHEN...) BODY...): control when BODY is evaluated.
If `compile' is in WHEN, BODY is evaluated when compiled at top-level.
If `load' is in WHEN, BODY is evaluated when loaded after top-level compile.
If `eval' is in WHEN, BODY is evaluated when interpreted or at non-top-level."
  (if (and (fboundp 'cl-compiling-file) (cl-compiling-file)
	   (not cl-not-toplevel) (not (boundp 'for-effect)))  ; horrible kludge
      (let ((comp (or (memq 'compile when) (memq ':compile-toplevel when)))
	    (cl-not-toplevel t))
	(if (or (memq 'load when) (memq ':load-toplevel when))
	    (if comp (cons 'progn (mapcar 'cl-compile-time-too body))
	      (list* 'if nil nil body))
	  (progn (if comp (eval (cons 'progn body))) nil)))
    (and (or (memq 'eval when) (memq ':execute when))
	 (cons 'progn body))))

(defun cl-compile-time-too (form)
  (or (and (symbolp (car-safe form)) (get (car-safe form) 'byte-hunk-handler))
      (setq form (macroexpand
		  form (cons '(eval-when) byte-compile-macro-environment))))
  (cond ((eq (car-safe form) 'progn)
	 (cons 'progn (mapcar 'cl-compile-time-too (cdr form))))
	((eq (car-safe form) 'eval-when)
	 (let ((when (nth 1 form)))
	   (if (or (memq 'eval when) (memq ':execute when))
	       (list* 'eval-when (cons 'compile when) (cddr form))
	     form)))
	(t (eval form) form)))

(or (and (fboundp 'eval-when-compile)
	 (not (eq (car-safe (symbol-function 'eval-when-compile)) 'autoload)))
    (eval '(defmacro eval-when-compile (&rest body)
	     "Like `progn', but evaluates the body at compile time.
The result of the body appears to the compiler as a quoted constant."
	     (list 'quote (eval (cons 'progn body))))))

(defmacro load-time-value (form &optional read-only)
  "Like `progn', but evaluates the body at load time.
The result of the body appears to the compiler as a quoted constant."
  (if (cl-compiling-file)
      (let* ((temp (gentemp "--cl-load-time--"))
	     (set (list 'set (list 'quote temp) form)))
	(if (and (fboundp 'byte-compile-file-form-defmumble)
		 (boundp 'this-kind) (boundp 'that-one))
	    (fset 'byte-compile-file-form
		  (list 'lambda '(form)
			(list 'fset '(quote byte-compile-file-form)
			      (list 'quote
				    (symbol-function 'byte-compile-file-form)))
			(list 'byte-compile-file-form (list 'quote set))
			'(byte-compile-file-form form)))
	  (print set (symbol-value 'outbuffer)))
	(list 'symbol-value (list 'quote temp)))
    (list 'quote (eval form))))


;;; Conditional control structures.

(defmacro case (expr &rest clauses)
  "(case EXPR CLAUSES...): evals EXPR, chooses from CLAUSES on that value.
Each clause looks like (KEYLIST BODY...).  EXPR is evaluated and compared
against each key in each KEYLIST; the corresponding BODY is evaluated.
If no clause succeeds, case returns nil.  A single atom may be used in
place of a KEYLIST of one atom.  A KEYLIST of `t' or `otherwise' is
allowed only in the final clause, and matches if no other keys match.
Key values are compared by `eql'."
  (let* ((temp (if (cl-simple-expr-p expr 3) expr (gensym)))
	 (head-list nil)
	 (body (cons
		'cond
		(mapcar
		 (function
		  (lambda (c)
		    (cons (cond ((memq (car c) '(t otherwise)) t)
				((eq (car c) 'ecase-error-flag)
				 (list 'error "ecase failed: %s, %s"
				       temp (list 'quote (reverse head-list))))
				((listp (car c))
				 (setq head-list (append (car c) head-list))
				 (list 'member* temp (list 'quote (car c))))
				(t
				 (if (memq (car c) head-list)
				     (error "Duplicate key in case: %s"
					    (car c)))
				 (cl-push (car c) head-list)
				 (list 'eql temp (list 'quote (car c)))))
			  (or (cdr c) '(nil)))))
		 clauses))))
    (if (eq temp expr) body
      (list 'let (list (list temp expr)) body))))

(defmacro ecase (expr &rest clauses)
  "(ecase EXPR CLAUSES...): like `case', but error if no case fits.
`otherwise'-clauses are not allowed."
  (list* 'case expr (append clauses '((ecase-error-flag)))))

(defmacro typecase (expr &rest clauses)
  "(typecase EXPR CLAUSES...): evals EXPR, chooses from CLAUSES on that value.
Each clause looks like (TYPE BODY...).  EXPR is evaluated and, if it
satisfies TYPE, the corresponding BODY is evaluated.  If no clause succeeds,
typecase returns nil.  A TYPE of `t' or `otherwise' is allowed only in the
final clause, and matches if no other keys match."
  (let* ((temp (if (cl-simple-expr-p expr 3) expr (gensym)))
	 (type-list nil)
	 (body (cons
		'cond
		(mapcar
		 (function
		  (lambda (c)
		    (cons (cond ((eq (car c) 'otherwise) t)
				((eq (car c) 'ecase-error-flag)
				 (list 'error "etypecase failed: %s, %s"
				       temp (list 'quote (reverse type-list))))
				(t
				 (cl-push (car c) type-list)
				 (cl-make-type-test temp (car c))))
			  (or (cdr c) '(nil)))))
		 clauses))))
    (if (eq temp expr) body
      (list 'let (list (list temp expr)) body))))

(defmacro etypecase (expr &rest clauses)
  "(etypecase EXPR CLAUSES...): like `typecase', but error if no case fits.
`otherwise'-clauses are not allowed."
  (list* 'typecase expr (append clauses '((ecase-error-flag)))))


;;; Blocks and exits.

(defmacro block (name &rest body)
  "(block NAME BODY...): define a lexically-scoped block named NAME.
NAME may be any symbol.  Code inside the BODY forms can call `return-from'
to jump prematurely out of the block.  This differs from `catch' and `throw'
in two respects:  First, the NAME is an unevaluated symbol rather than a
quoted symbol or other form; and second, NAME is lexically rather than
dynamically scoped:  Only references to it within BODY will work.  These
references may appear inside macro expansions, but not inside functions
called from BODY."
  (if (cl-safe-expr-p (cons 'progn body)) (cons 'progn body)
    (list 'cl-block-wrapper
	  (list* 'catch (list 'quote (intern (format "--cl-block-%s--" name)))
		 body))))

(defvar cl-active-block-names nil)

(put 'cl-block-wrapper 'byte-compile 'cl-byte-compile-block)
(defun cl-byte-compile-block (cl-form)
  (if (fboundp 'byte-compile-form-do-effect)  ; Check for optimizing compiler
      (progn
	(let* ((cl-entry (cons (nth 1 (nth 1 (nth 1 cl-form))) nil))
	       (cl-active-block-names (cons cl-entry cl-active-block-names))
	       (cl-body (byte-compile-top-level
			 (cons 'progn (cddr (nth 1 cl-form))))))
	  (if (cdr cl-entry)
	      (byte-compile-form (list 'catch (nth 1 (nth 1 cl-form)) cl-body))
	    (byte-compile-form cl-body))))
    (byte-compile-form (nth 1 cl-form))))

(put 'cl-block-throw 'byte-compile 'cl-byte-compile-throw)
(defun cl-byte-compile-throw (cl-form)
  (let ((cl-found (assq (nth 1 (nth 1 cl-form)) cl-active-block-names)))
    (if cl-found (setcdr cl-found t)))
  (byte-compile-normal-call (cons 'throw (cdr cl-form))))

(defmacro return (&optional res)
  "(return [RESULT]): return from the block named nil.
This is equivalent to `(return-from nil RESULT)'."
  (list 'return-from nil res))

(defmacro return-from (name &optional res)
  "(return-from NAME [RESULT]): return from the block named NAME.
This jump out to the innermost enclosing `(block NAME ...)' form,
returning RESULT from that form (or nil if RESULT is omitted).
This is compatible with Common Lisp, but note that `defun' and
`defmacro' do not create implicit blocks as they do in Common Lisp."
  (let ((name2 (intern (format "--cl-block-%s--" name))))
    (list 'cl-block-throw (list 'quote name2) res)))


;;; The "loop" macro.

(defvar args) (defvar loop-accum-var) (defvar loop-accum-vars)
(defvar loop-bindings) (defvar loop-body) (defvar loop-destr-temps)
(defvar loop-finally) (defvar loop-finish-flag) (defvar loop-first-flag)
(defvar loop-initially) (defvar loop-map-form) (defvar loop-name)
(defvar loop-result) (defvar loop-result-explicit)
(defvar loop-result-var) (defvar loop-steps) (defvar loop-symbol-macs)

(defmacro loop (&rest args)
  "(loop CLAUSE...): The Common Lisp `loop' macro.
Valid clauses are:
  for VAR from/upfrom/downfrom NUM to/upto/downto/above/below NUM by NUM,
  for VAR in LIST by FUNC, for VAR on LIST by FUNC, for VAR = INIT then EXPR,
  for VAR across ARRAY, repeat NUM, with VAR = INIT, while COND, until COND,
  always COND, never COND, thereis COND, collect EXPR into VAR,
  append EXPR into VAR, nconc EXPR into VAR, sum EXPR into VAR,
  count EXPR into VAR, maximize EXPR into VAR, minimize EXPR into VAR,
  if COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...],
  unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...],
  do EXPRS..., initially EXPRS..., finally EXPRS..., return EXPR,
  finally return EXPR, named NAME."
  (if (not (memq t (mapcar 'symbolp (delq nil (delq t (copy-list args))))))
      (list 'block nil (list* 'while t args))
    (let ((loop-name nil)	(loop-bindings nil)
	  (loop-body nil)	(loop-steps nil)
	  (loop-result nil)	(loop-result-explicit nil)
	  (loop-result-var nil) (loop-finish-flag nil)
	  (loop-accum-var nil)	(loop-accum-vars nil)
	  (loop-initially nil)	(loop-finally nil)
	  (loop-map-form nil)   (loop-first-flag nil)
	  (loop-destr-temps nil) (loop-symbol-macs nil))
      (setq args (append args '(cl-end-loop)))
      (while (not (eq (car args) 'cl-end-loop)) (cl-parse-loop-clause))
      (if loop-finish-flag
	  (cl-push (list (list loop-finish-flag t)) loop-bindings))
      (if loop-first-flag
	  (progn (cl-push (list (list loop-first-flag t)) loop-bindings)
		 (cl-push (list 'setq loop-first-flag nil) loop-steps)))
      (let* ((epilogue (nconc (nreverse loop-finally)
			      (list (or loop-result-explicit loop-result))))
	     (ands (cl-loop-build-ands (nreverse loop-body)))
	     (while-body (nconc (cadr ands) (nreverse loop-steps)))
	     (body (append
		    (nreverse loop-initially)
		    (list (if loop-map-form
			      (list 'block '--cl-finish--
				    (subst
				     (if (eq (car ands) t) while-body
				       (cons (list 'or (car ands)
						   '(return-from --cl-finish--
						      nil))
					     while-body))
				     '--cl-map loop-map-form))
			    (list* 'while (car ands) while-body)))
		    (if loop-finish-flag
			(if (equal epilogue '(nil)) (list loop-result-var)
			  (list (list 'if loop-finish-flag
				      (cons 'progn epilogue) loop-result-var)))
		      epilogue))))
	(if loop-result-var (cl-push (list loop-result-var) loop-bindings))
	(while loop-bindings
	  (if (cdar loop-bindings)
	      (setq body (list (cl-loop-let (cl-pop loop-bindings) body t)))
	    (let ((lets nil))
	      (while (and loop-bindings
			  (not (cdar loop-bindings)))
		(cl-push (car (cl-pop loop-bindings)) lets))
	      (setq body (list (cl-loop-let lets body nil))))))
	(if loop-symbol-macs
	    (setq body (list (list* 'symbol-macrolet loop-symbol-macs body))))
	(list* 'block loop-name body)))))

(defun cl-parse-loop-clause ()   ; uses args, loop-*
  (let ((word (cl-pop args))
	(hash-types '(hash-key hash-keys hash-value hash-values))
	(key-types '(key-code key-codes key-seq key-seqs
		     key-binding key-bindings)))
    (cond

     ((null args)
      (error "Malformed `loop' macro"))

     ((eq word 'named)
      (setq loop-name (cl-pop args)))

     ((eq word 'initially)
      (if (memq (car args) '(do doing)) (cl-pop args))
      (or (consp (car args)) (error "Syntax error on `initially' clause"))
      (while (consp (car args))
	(cl-push (cl-pop args) loop-initially)))

     ((eq word 'finally)
      (if (eq (car args) 'return)
	  (setq loop-result-explicit (or (cl-pop2 args) '(quote nil)))
	(if (memq (car args) '(do doing)) (cl-pop args))
	(or (consp (car args)) (error "Syntax error on `finally' clause"))
	(if (and (eq (caar args) 'return) (null loop-name))
	    (setq loop-result-explicit (or (nth 1 (cl-pop args)) '(quote nil)))
	  (while (consp (car args))
	    (cl-push (cl-pop args) loop-finally)))))

     ((memq word '(for as))
      (let ((loop-for-bindings nil) (loop-for-sets nil) (loop-for-steps nil)
	    (ands nil))
	(while
	    (let ((var (or (cl-pop args) (gensym))))
	      (setq word (cl-pop args))
	      (if (eq word 'being) (setq word (cl-pop args)))
	      (if (memq word '(the each)) (setq word (cl-pop args)))
	      (if (memq word '(buffer buffers))
		  (setq word 'in args (cons '(buffer-list) args)))
	      (cond

	       ((memq word '(from downfrom upfrom to downto upto
			     above below by))
		(cl-push word args)
		(if (memq (car args) '(downto above))
		    (error "Must specify `from' value for downward loop"))
		(let* ((down (or (eq (car args) 'downfrom)
				 (memq (caddr args) '(downto above))))
		       (excl (or (memq (car args) '(above below))
				 (memq (caddr args) '(above below))))
		       (start (and (memq (car args) '(from upfrom downfrom))
				   (cl-pop2 args)))
		       (end (and (memq (car args)
				       '(to upto downto above below))
				 (cl-pop2 args)))
		       (step (and (eq (car args) 'by) (cl-pop2 args)))
		       (end-var (and (not (cl-const-expr-p end)) (gensym)))
		       (step-var (and (not (cl-const-expr-p step))
				      (gensym))))
		  (and step (numberp step) (<= step 0)
		       (error "Loop `by' value is not positive: %s" step))
		  (cl-push (list var (or start 0)) loop-for-bindings)
		  (if end-var (cl-push (list end-var end) loop-for-bindings))
		  (if step-var (cl-push (list step-var step)
					loop-for-bindings))
		  (if end
		      (cl-push (list
				(if down (if excl '> '>=) (if excl '< '<=))
				var (or end-var end)) loop-body))
		  (cl-push (list var (list (if down '- '+) var
					   (or step-var step 1)))
			   loop-for-steps)))

	       ((memq word '(in in-ref on))
		(let* ((on (eq word 'on))
		       (temp (if (and on (symbolp var)) var (gensym))))
		  (cl-push (list temp (cl-pop args)) loop-for-bindings)
		  (cl-push (list 'consp temp) loop-body)
		  (if (eq word 'in-ref)
		      (cl-push (list var (list 'car temp)) loop-symbol-macs)
		    (or (eq temp var)
			(progn
			  (cl-push (list var nil) loop-for-bindings)
			  (cl-push (list var (if on temp (list 'car temp)))
				   loop-for-sets))))
		  (cl-push (list temp
				 (if (eq (car args) 'by)
				     (let ((step (cl-pop2 args)))
				       (if (and (memq (car-safe step)
						      '(quote function
							      function*))
						(symbolp (nth 1 step)))
					   (list (nth 1 step) temp)
					 (list 'funcall step temp)))
				   (list 'cdr temp)))
			   loop-for-steps)))

	       ((eq word '=)
		(let* ((start (cl-pop args))
		       (then (if (eq (car args) 'then) (cl-pop2 args) start)))
		  (cl-push (list var nil) loop-for-bindings)
		  (if (or ands (eq (car args) 'and))
		      (progn
			(cl-push (list var
				       (list 'if
					     (or loop-first-flag
						 (setq loop-first-flag
						       (gensym)))
					     start var))
				 loop-for-sets)
			(cl-push (list var then) loop-for-steps))
		    (cl-push (list var
				   (if (eq start then) start
				     (list 'if
					   (or loop-first-flag
					       (setq loop-first-flag (gensym)))
					   start then)))
			     loop-for-sets))))

	       ((memq word '(across across-ref))
		(let ((temp-vec (gensym)) (temp-idx (gensym)))
		  (cl-push (list temp-vec (cl-pop args)) loop-for-bindings)
		  (cl-push (list temp-idx -1) loop-for-bindings)
		  (cl-push (list '< (list 'setq temp-idx (list '1+ temp-idx))
				 (list 'length temp-vec)) loop-body)
		  (if (eq word 'across-ref)
		      (cl-push (list var (list 'aref temp-vec temp-idx))
			       loop-symbol-macs)
		    (cl-push (list var nil) loop-for-bindings)
		    (cl-push (list var (list 'aref temp-vec temp-idx))
			     loop-for-sets))))

	       ((memq word '(element elements))
		(let ((ref (or (memq (car args) '(in-ref of-ref))
			       (and (not (memq (car args) '(in of)))
				    (error "Expected `of'"))))
		      (seq (cl-pop2 args))
		      (temp-seq (gensym))
		      (temp-idx (if (eq (car args) 'using)
				    (if (and (= (length (cadr args)) 2)
					     (eq (caadr args) 'index))
					(cadr (cl-pop2 args))
				      (error "Bad `using' clause"))
				  (gensym))))
		  (cl-push (list temp-seq seq) loop-for-bindings)
		  (cl-push (list temp-idx 0) loop-for-bindings)
		  (if ref
		      (let ((temp-len (gensym)))
			(cl-push (list temp-len (list 'length temp-seq))
				 loop-for-bindings)
			(cl-push (list var (list 'elt temp-seq temp-idx))
				 loop-symbol-macs)
			(cl-push (list '< temp-idx temp-len) loop-body))
		    (cl-push (list var nil) loop-for-bindings)
		    (cl-push (list 'and temp-seq
				   (list 'or (list 'consp temp-seq)
					 (list '< temp-idx
					       (list 'length temp-seq))))
			     loop-body)
		    (cl-push (list var (list 'if (list 'consp temp-seq)
					     (list 'pop temp-seq)
					     (list 'aref temp-seq temp-idx)))
			     loop-for-sets))
		  (cl-push (list temp-idx (list '1+ temp-idx))
			   loop-for-steps)))

	       ((memq word hash-types)
		(or (memq (car args) '(in of)) (error "Expected `of'"))
		(let* ((table (cl-pop2 args))
		       (other (if (eq (car args) 'using)
				  (if (and (= (length (cadr args)) 2)
					   (memq (caadr args) hash-types)
					   (not (eq (caadr args) word)))
				      (cadr (cl-pop2 args))
				    (error "Bad `using' clause"))
				(gensym))))
		  (if (memq word '(hash-value hash-values))
		      (setq var (prog1 other (setq other var))))
		  (setq loop-map-form
			(list 'maphash (list 'function
					     (list* 'lambda (list var other)
						    '--cl-map)) table))))

	       ((memq word '(symbol present-symbol external-symbol
			     symbols present-symbols external-symbols))
		(let ((ob (and (memq (car args) '(in of)) (cl-pop2 args))))
		  (setq loop-map-form
			(list 'mapatoms (list 'function
					      (list* 'lambda (list var)
						     '--cl-map)) ob))))

	       ((memq word '(overlay overlays extent extents))
		(let ((buf nil) (from nil) (to nil))
		  (while (memq (car args) '(in of from to))
		    (cond ((eq (car args) 'from) (setq from (cl-pop2 args)))
			  ((eq (car args) 'to) (setq to (cl-pop2 args)))
			  (t (setq buf (cl-pop2 args)))))
		  (setq loop-map-form
			(list 'cl-map-extents
			      (list 'function (list 'lambda (list var (gensym))
						    '(progn . --cl-map) nil))
			      buf from to))))

	       ((memq word '(interval intervals))
		(let ((buf nil) (prop nil) (from nil) (to nil)
		      (var1 (gensym)) (var2 (gensym)))
		  (while (memq (car args) '(in of property from to))
		    (cond ((eq (car args) 'from) (setq from (cl-pop2 args)))
			  ((eq (car args) 'to) (setq to (cl-pop2 args)))
			  ((eq (car args) 'property)
			   (setq prop (cl-pop2 args)))
			  (t (setq buf (cl-pop2 args)))))
		  (if (and (consp var) (symbolp (car var)) (symbolp (cdr var)))
		      (setq var1 (car var) var2 (cdr var))
		    (cl-push (list var (list 'cons var1 var2)) loop-for-sets))
		  (setq loop-map-form
			(list 'cl-map-intervals
			      (list 'function (list 'lambda (list var1 var2)
						    '(progn . --cl-map)))
			      buf prop from to))))

	       ((memq word key-types)
		(or (memq (car args) '(in of)) (error "Expected `of'"))
		(let ((map (cl-pop2 args))
		      (other (if (eq (car args) 'using)
				 (if (and (= (length (cadr args)) 2)
					  (memq (caadr args) key-types)
					  (not (eq (caadr args) word)))
				     (cadr (cl-pop2 args))
				   (error "Bad `using' clause"))
			       (gensym))))
		  (if (memq word '(key-binding key-bindings))
		      (setq var (prog1 other (setq other var))))
		  (setq loop-map-form
			(list (if (memq word '(key-seq key-seqs))
				  'cl-map-keymap-recursively 'cl-map-keymap)
			      (list 'function (list* 'lambda (list var other)
						     '--cl-map)) map))))

	       ((memq word '(frame frames screen screens))
		(let ((temp (gensym)))
		  (cl-push (list var (if (eq cl-emacs-type 'lucid)
					 '(selected-screen) '(selected-frame)))
			   loop-for-bindings)
		  (cl-push (list temp nil) loop-for-bindings)
		  (cl-push (list 'prog1 (list 'not (list 'eq var temp))
				 (list 'or temp (list 'setq temp var)))
			   loop-body)
		  (cl-push (list var (list (if (eq cl-emacs-type 'lucid)
					       'next-screen 'next-frame) var))
			   loop-for-steps)))

	       ((memq word '(window windows))
		(let ((scr (and (memq (car args) '(in of)) (cl-pop2 args)))
		      (temp (gensym)))
		  (cl-push (list var (if scr
					 (list (if (eq cl-emacs-type 'lucid)
						   'screen-selected-window
						 'frame-selected-window) scr)
				       '(selected-window)))
			   loop-for-bindings)
		  (cl-push (list temp nil) loop-for-bindings)
		  (cl-push (list 'prog1 (list 'not (list 'eq var temp))
				 (list 'or temp (list 'setq temp var)))
			   loop-body)
		  (cl-push (list var (list 'next-window var)) loop-for-steps)))

	       (t
		(let ((handler (and (symbolp word)
				    (get word 'cl-loop-for-handler))))
		  (if handler
		      (funcall handler var)
		    (error "Expected a `for' preposition, found %s" word)))))
	      (eq (car args) 'and))
	  (setq ands t)
	  (cl-pop args))
	(if (and ands loop-for-bindings)
	    (cl-push (nreverse loop-for-bindings) loop-bindings)
	  (setq loop-bindings (nconc (mapcar 'list loop-for-bindings)
				     loop-bindings)))
	(if loop-for-sets
	    (cl-push (list 'progn
			   (cl-loop-let (nreverse loop-for-sets) 'setq ands)
			   t) loop-body))
	(if loop-for-steps
	    (cl-push (cons (if ands 'psetq 'setq)
			   (apply 'append (nreverse loop-for-steps)))
		     loop-steps))))

     ((eq word 'repeat)
      (let ((temp (gensym)))
	(cl-push (list (list temp (cl-pop args))) loop-bindings)
	(cl-push (list '>= (list 'setq temp (list '1- temp)) 0) loop-body)))

     ((eq word 'collect)
      (let ((what (cl-pop args))
	    (var (cl-loop-handle-accum nil 'nreverse)))
	(if (eq var loop-accum-var)
	    (cl-push (list 'progn (list 'push what var) t) loop-body)
	  (cl-push (list 'progn
			 (list 'setq var (list 'nconc var (list 'list what)))
			 t) loop-body))))

     ((memq word '(nconc nconcing append appending))
      (let ((what (cl-pop args))
	    (var (cl-loop-handle-accum nil 'nreverse)))
	(cl-push (list 'progn
		       (list 'setq var
			     (if (eq var loop-accum-var)
				 (list 'nconc
				       (list (if (memq word '(nconc nconcing))
						 'nreverse 'reverse)
					     what)
				       var)
			       (list (if (memq word '(nconc nconcing))
					 'nconc 'append)
				     var what))) t) loop-body)))

     ((memq word '(concat concating))
      (let ((what (cl-pop args))
	    (var (cl-loop-handle-accum "")))
	(cl-push (list 'progn (list 'callf 'concat var what) t) loop-body)))

     ((memq word '(vconcat vconcating))
      (let ((what (cl-pop args))
	    (var (cl-loop-handle-accum [])))
	(cl-push (list 'progn (list 'callf 'vconcat var what) t) loop-body)))

     ((memq word '(sum summing))
      (let ((what (cl-pop args))
	    (var (cl-loop-handle-accum 0)))
	(cl-push (list 'progn (list 'incf var what) t) loop-body)))

     ((memq word '(count counting))
      (let ((what (cl-pop args))
	    (var (cl-loop-handle-accum 0)))
	(cl-push (list 'progn (list 'if what (list 'incf var)) t) loop-body)))

     ((memq word '(minimize minimizing maximize maximizing))
      (let* ((what (cl-pop args))
	     (temp (if (cl-simple-expr-p what) what (gensym)))
	     (var (cl-loop-handle-accum nil))
	     (func (intern (substring (symbol-name word) 0 3)))
	     (set (list 'setq var (list 'if var (list func var temp) temp))))
	(cl-push (list 'progn (if (eq temp what) set
				(list 'let (list (list temp what)) set))
		       t) loop-body)))

     ((eq word 'with)
      (let ((bindings nil))
	(while (progn (cl-push (list (cl-pop args)
				     (and (eq (car args) '=) (cl-pop2 args)))
			       bindings)
		      (eq (car args) 'and))
	  (cl-pop args))
	(cl-push (nreverse bindings) loop-bindings)))

     ((eq word 'while)
      (cl-push (cl-pop args) loop-body))

     ((eq word 'until)
      (cl-push (list 'not (cl-pop args)) loop-body))

     ((eq word 'always)
      (or loop-finish-flag (setq loop-finish-flag (gensym)))
      (cl-push (list 'setq loop-finish-flag (cl-pop args)) loop-body)
      (setq loop-result t))

     ((eq word 'never)
      (or loop-finish-flag (setq loop-finish-flag (gensym)))
      (cl-push (list 'setq loop-finish-flag (list 'not (cl-pop args)))
	       loop-body)
      (setq loop-result t))

     ((eq word 'thereis)
      (or loop-finish-flag (setq loop-finish-flag (gensym)))
      (or loop-result-var (setq loop-result-var (gensym)))
      (cl-push (list 'setq loop-finish-flag
		     (list 'not (list 'setq loop-result-var (cl-pop args))))
	       loop-body))

     ((memq word '(if when unless))
      (let* ((cond (cl-pop args))
	     (then (let ((loop-body nil))
		     (cl-parse-loop-clause)
		     (cl-loop-build-ands (nreverse loop-body))))
	     (else (let ((loop-body nil))
		     (if (eq (car args) 'else)
			 (progn (cl-pop args) (cl-parse-loop-clause)))
		     (cl-loop-build-ands (nreverse loop-body))))
	     (simple (and (eq (car then) t) (eq (car else) t))))
	(if (eq (car args) 'end) (cl-pop args))
	(if (eq word 'unless) (setq then (prog1 else (setq else then))))
	(let ((form (cons (if simple (cons 'progn (nth 1 then)) (nth 2 then))
			  (if simple (nth 1 else) (list (nth 2 else))))))
	  (if (cl-expr-contains form 'it)
	      (let ((temp (gensym)))
		(cl-push (list temp) loop-bindings)
		(setq form (list* 'if (list 'setq temp cond)
				  (subst temp 'it form))))
	    (setq form (list* 'if cond form)))
	  (cl-push (if simple (list 'progn form t) form) loop-body))))

     ((memq word '(do doing))
      (let ((body nil))
	(or (consp (car args)) (error "Syntax error on `do' clause"))
	(while (consp (car args)) (cl-push (cl-pop args) body))
	(cl-push (cons 'progn (nreverse (cons t body))) loop-body)))

     ((eq word 'return)
      (or loop-finish-flag (setq loop-finish-flag (gensym)))
      (or loop-result-var (setq loop-result-var (gensym)))
      (cl-push (list 'setq loop-result-var (cl-pop args)
		     loop-finish-flag nil) loop-body))

     (t
      (let ((handler (and (symbolp word) (get word 'cl-loop-handler))))
	(or handler (error "Expected a loop keyword, found %s" word))
	(funcall handler))))
    (if (eq (car args) 'and)
	(progn (cl-pop args) (cl-parse-loop-clause)))))

(defun cl-loop-let (specs body par)   ; uses loop-*
  (let ((p specs) (temps nil) (new nil))
    (while (and p (or (symbolp (car-safe (car p))) (null (cadar p))))
      (setq p (cdr p)))
    (and par p
	 (progn
	   (setq par nil p specs)
	   (while p
	     (or (cl-const-expr-p (cadar p))
		 (let ((temp (gensym)))
		   (cl-push (list temp (cadar p)) temps)
		   (setcar (cdar p) temp)))
	     (setq p (cdr p)))))
    (while specs
      (if (and (consp (car specs)) (listp (caar specs)))
	  (let* ((spec (caar specs)) (nspecs nil)
		 (expr (cadr (cl-pop specs)))
		 (temp (cdr (or (assq spec loop-destr-temps)
				(car (cl-push (cons spec (or (last spec 0)
							     (gensym)))
					      loop-destr-temps))))))
	    (cl-push (list temp expr) new)
	    (while (consp spec)
	      (cl-push (list (cl-pop spec)
			     (and expr (list (if spec 'pop 'car) temp)))
		       nspecs))
	    (setq specs (nconc (nreverse nspecs) specs)))
	(cl-push (cl-pop specs) new)))
    (if (eq body 'setq)
	(let ((set (cons (if par 'psetq 'setq) (apply 'nconc (nreverse new)))))
	  (if temps (list 'let* (nreverse temps) set) set))
      (list* (if par 'let 'let*)
	     (nconc (nreverse temps) (nreverse new)) body))))

(defun cl-loop-handle-accum (def &optional func)   ; uses args, loop-*
  (if (eq (car args) 'into)
      (let ((var (cl-pop2 args)))
	(or (memq var loop-accum-vars)
	    (progn (cl-push (list (list var def)) loop-bindings)
		   (cl-push var loop-accum-vars)))
	var)
    (or loop-accum-var
	(progn
	  (cl-push (list (list (setq loop-accum-var (gensym)) def))
		   loop-bindings)
	  (setq loop-result (if func (list func loop-accum-var)
			      loop-accum-var))
	  loop-accum-var))))

(defun cl-loop-build-ands (clauses)
  (let ((ands nil)
	(body nil))
    (while clauses
      (if (and (eq (car-safe (car clauses)) 'progn)
	       (eq (car (last (car clauses))) t))
	  (if (cdr clauses)
	      (setq clauses (cons (nconc (butlast (car clauses))
					 (if (eq (car-safe (cadr clauses))
						 'progn)
					     (cdadr clauses)
					   (list (cadr clauses))))
				  (cddr clauses)))
	    (setq body (cdr (butlast (cl-pop clauses)))))
	(cl-push (cl-pop clauses) ands)))
    (setq ands (or (nreverse ands) (list t)))
    (list (if (cdr ands) (cons 'and ands) (car ands))
	  body
	  (let ((full (if body
			  (append ands (list (cons 'progn (append body '(t)))))
			ands)))
	    (if (cdr full) (cons 'and full) (car full))))))


;;; Other iteration control structures.

(defmacro do (steps endtest &rest body)
  "The Common Lisp `do' loop.
Format is: (do ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)"
  (cl-expand-do-loop steps endtest body nil))

(defmacro do* (steps endtest &rest body)
  "The Common Lisp `do*' loop.
Format is: (do* ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)"
  (cl-expand-do-loop steps endtest body t))

(defun cl-expand-do-loop (steps endtest body star)
  (list 'block nil
	(list* (if star 'let* 'let)
	       (mapcar (function (lambda (c)
				   (if (consp c) (list (car c) (nth 1 c)) c)))
		       steps)
	       (list* 'while (list 'not (car endtest))
		      (append body
			      (let ((sets (mapcar
					   (function
					    (lambda (c)
					      (and (consp c) (cdr (cdr c))
						   (list (car c) (nth 2 c)))))
					   steps)))
				(setq sets (delq nil sets))
				(and sets
				     (list (cons (if (or star (not (cdr sets)))
						     'setq 'psetq)
						 (apply 'append sets)))))))
	       (or (cdr endtest) '(nil)))))

(defmacro dolist (spec &rest body)
  "(dolist (VAR LIST [RESULT]) BODY...): loop over a list.
Evaluate BODY with VAR bound to each `car' from LIST, in turn.
Then evaluate RESULT to get return value, default nil."
  (let ((temp (gensym "--dolist-temp--")))
    (list 'block nil
	  (list* 'let (list (list temp (nth 1 spec)) (car spec))
		 (list* 'while temp (list 'setq (car spec) (list 'car temp))
			(append body (list (list 'setq temp
						 (list 'cdr temp)))))
		 (if (cdr (cdr spec))
		     (cons (list 'setq (car spec) nil) (cdr (cdr spec)))
		   '(nil))))))

(defmacro dotimes (spec &rest body)
  "(dotimes (VAR COUNT [RESULT]) BODY...): loop a certain number of times.
Evaluate BODY with VAR bound to successive integers from 0, inclusive,
to COUNT, exclusive.  Then evaluate RESULT to get return value, default
nil."
  (let ((temp (gensym "--dotimes-temp--")))
    (list 'block nil
	  (list* 'let (list (list temp (nth 1 spec)) (list (car spec) 0))
		 (list* 'while (list '< (car spec) temp)
			(append body (list (list 'incf (car spec)))))
		 (or (cdr (cdr spec)) '(nil))))))

(defmacro do-symbols (spec &rest body)
  "(dosymbols (VAR [OBARRAY [RESULT]]) BODY...): loop over all symbols.
Evaluate BODY with VAR bound to each interned symbol, or to each symbol
from OBARRAY."
  ;; Apparently this doesn't have an implicit block.
  (list 'block nil
	(list 'let (list (car spec))
	      (list* 'mapatoms
		     (list 'function (list* 'lambda (list (car spec)) body))
		     (and (cadr spec) (list (cadr spec))))
	      (caddr spec))))

(defmacro do-all-symbols (spec &rest body)
  (list* 'do-symbols (list (car spec) nil (cadr spec)) body))


;;; Assignments.

(defmacro psetq (&rest args)
  "(psetq SYM VAL SYM VAL ...): set SYMs to the values VALs in parallel.
This is like `setq', except that all VAL forms are evaluated (in order)
before assigning any symbols SYM to the corresponding values."
  (cons 'psetf args))


;;; Binding control structures.

(defmacro progv (symbols values &rest body)
  "(progv SYMBOLS VALUES BODY...): bind SYMBOLS to VALUES dynamically in BODY.
The forms SYMBOLS and VALUES are evaluated, and must evaluate to lists.
Each SYMBOL in the first list is bound to the corresponding VALUE in the
second list (or made unbound if VALUES is shorter than SYMBOLS); then the
BODY forms are executed and their result is returned.  This is much like
a `let' form, except that the list of symbols can be computed at run-time."
  (list 'let '((cl-progv-save nil))
	(list 'unwind-protect
	      (list* 'progn (list 'cl-progv-before symbols values) body)
	      '(cl-progv-after))))

;;; This should really have some way to shadow 'byte-compile properties, etc.
(defmacro flet (bindings &rest body)
  "(flet ((FUNC ARGLIST BODY...) ...) FORM...): make temporary function defns.
This is an analogue of `let' that operates on the function cell of FUNC
rather than its value cell.  The FORMs are evaluated with the specified
function definitions in place, then the definitions are undone (the FUNCs
go back to their previous definitions, or lack thereof)."
  (list* 'letf*
	 (mapcar
	  (function
	   (lambda (x)
	     (let ((func (list 'function*
			       (list 'lambda (cadr x)
				     (list* 'block (car x) (cddr x))))))
	       (if (and (cl-compiling-file)
			(boundp 'byte-compile-function-environment))
		   (cl-push (cons (car x) (eval func))
			    byte-compile-function-environment))
	       (list (list 'symbol-function (list 'quote (car x))) func))))
	  bindings)
	 body))

(defmacro labels (&rest args) (cons 'flet args))

;; The following ought to have a better definition for use with newer
;; byte compilers.
(defmacro macrolet (bindings &rest body)
  "(macrolet ((NAME ARGLIST BODY...) ...) FORM...): make temporary macro defns.
This is like `flet', but for macros instead of functions."
  (if (cdr bindings)
      (list 'macrolet
	    (list (car bindings)) (list* 'macrolet (cdr bindings) body))
    (if (null bindings) (cons 'progn body)
      (let* ((name (caar bindings))
	     (res (cl-transform-lambda (cdar bindings) name)))
	(eval (car res))
	(cl-macroexpand-all (cons 'progn body)
			    (cons (list* name 'lambda (cdr res))
				  cl-macro-environment))))))

(defmacro symbol-macrolet (bindings &rest body)
  "(symbol-macrolet ((NAME EXPANSION) ...) FORM...): make symbol macro defns.
Within the body FORMs, references to the variable NAME will be replaced
by EXPANSION, and (setq NAME ...) will act like (setf EXPANSION ...)."
  (if (cdr bindings)
      (list 'symbol-macrolet
	    (list (car bindings)) (list* 'symbol-macrolet (cdr bindings) body))
    (if (null bindings) (cons 'progn body)
      (cl-macroexpand-all (cons 'progn body)
			  (cons (list (symbol-name (caar bindings))
				      (cadar bindings))
				cl-macro-environment)))))

(defvar cl-closure-vars nil)
(defmacro lexical-let (bindings &rest body)
  "(lexical-let BINDINGS BODY...): like `let', but lexically scoped.
The main visible difference is that lambdas inside BODY will create
lexical closures as in Common Lisp."
  (let* ((cl-closure-vars cl-closure-vars)
	 (vars (mapcar (function
			(lambda (x)
			  (or (consp x) (setq x (list x)))
			  (cl-push (gensym (format "--%s--" (car x)))
				   cl-closure-vars)
			  (list (car x) (cadr x) (car cl-closure-vars))))
		       bindings))
	 (ebody 
	  (cl-macroexpand-all
	   (cons 'progn body)
	   (nconc (mapcar (function (lambda (x)
				      (list (symbol-name (car x))
					    (list 'symbol-value (caddr x))
					    t))) vars)
		  (list '(defun . cl-defun-expander))
		  cl-macro-environment))))
    (if (not (get (car (last cl-closure-vars)) 'used))
	(list 'let (mapcar (function (lambda (x)
				       (list (caddr x) (cadr x)))) vars)
	      (sublis (mapcar (function (lambda (x)
					  (cons (caddr x)
						(list 'quote (caddr x)))))
			      vars)
		      ebody))
      (list 'let (mapcar (function (lambda (x)
				     (list (caddr x)
					   (list 'make-symbol
						 (format "--%s--" (car x))))))
			 vars)
	    (apply 'append '(setf)
		   (mapcar (function
			    (lambda (x)
			      (list (list 'symbol-value (caddr x)) (cadr x))))
			   vars))
	    ebody))))

(defmacro lexical-let* (bindings &rest body)
  "(lexical-let* BINDINGS BODY...): like `let*', but lexically scoped.
The main visible difference is that lambdas inside BODY will create
lexical closures as in Common Lisp."
  (if (null bindings) (cons 'progn body)
    (setq bindings (reverse bindings))
    (while bindings
      (setq body (list (list* 'lexical-let (list (cl-pop bindings)) body))))
    (car body)))

(defun cl-defun-expander (func &rest rest)
  (list 'progn
	(list 'defalias (list 'quote func)
	      (list 'function (cons 'lambda rest)))
	(list 'quote func)))


;;; Multiple values.

(defmacro multiple-value-bind (vars form &rest body)
  "(multiple-value-bind (SYM SYM...) FORM BODY): collect multiple return values.
FORM must return a list; the BODY is then executed with the first N elements
of this list bound (`let'-style) to each of the symbols SYM in turn.  This
is analogous to the Common Lisp `multiple-value-bind' macro, using lists to
simulate true multiple return values.  For compatibility, (values A B C) is
a synonym for (list A B C)."
  (let ((temp (gensym)) (n -1))
    (list* 'let* (cons (list temp form)
		       (mapcar (function
				(lambda (v)
				  (list v (list 'nth (setq n (1+ n)) temp))))
			       vars))
	   body)))

(defmacro multiple-value-setq (vars form)
  "(multiple-value-setq (SYM SYM...) FORM): collect multiple return values.
FORM must return a list; the first N elements of this list are stored in
each of the symbols SYM in turn.  This is analogous to the Common Lisp
`multiple-value-setq' macro, using lists to simulate true multiple return
values.  For compatibility, (values A B C) is a synonym for (list A B C)."
  (cond ((null vars) (list 'progn form nil))
	((null (cdr vars)) (list 'setq (car vars) (list 'car form)))
	(t
	 (let* ((temp (gensym)) (n 0))
	   (list 'let (list (list temp form))
		 (list 'prog1 (list 'setq (cl-pop vars) (list 'car temp))
		       (cons 'setq (apply 'nconc
					  (mapcar (function
						   (lambda (v)
						     (list v (list
							      'nth
							      (setq n (1+ n))
							      temp))))
						  vars)))))))))


;;; Declarations.

(defmacro locally (&rest body) (cons 'progn body))
(defmacro the (type form) form)

(defvar cl-proclaim-history t)    ; for future compilers
(defvar cl-declare-stack t)       ; for future compilers

(defun cl-do-proclaim (spec hist)
  (and hist (listp cl-proclaim-history) (cl-push spec cl-proclaim-history))
  (cond ((eq (car-safe spec) 'special)
	 (if (boundp 'byte-compile-bound-variables)
	     (setq byte-compile-bound-variables
		   (append (cdr spec) byte-compile-bound-variables))))

	((eq (car-safe spec) 'inline)
	 (while (setq spec (cdr spec))
	   (or (memq (get (car spec) 'byte-optimizer)
		     '(nil byte-compile-inline-expand))
	       (error "%s already has a byte-optimizer, can't make it inline"
		      (car spec)))
	   (put (car spec) 'byte-optimizer 'byte-compile-inline-expand)))

	((eq (car-safe spec) 'notinline)
	 (while (setq spec (cdr spec))
	   (if (eq (get (car spec) 'byte-optimizer)
		   'byte-compile-inline-expand)
	       (put (car spec) 'byte-optimizer nil))))

	((eq (car-safe spec) 'optimize)
	 (let ((speed (assq (nth 1 (assq 'speed (cdr spec)))
			    '((0 nil) (1 t) (2 t) (3 t))))
	       (safety (assq (nth 1 (assq 'safety (cdr spec)))
			     '((0 t) (1 t) (2 t) (3 nil)))))
	   (if speed (setq cl-optimize-speed (car speed)
			   byte-optimize (nth 1 speed)))
	   (if safety (setq cl-optimize-safety (car safety)
			    byte-compile-delete-errors (nth 1 safety)))))

	((and (eq (car-safe spec) 'warn) (boundp 'byte-compile-warnings))
	 (if (eq byte-compile-warnings t)
	     (setq byte-compile-warnings byte-compile-warning-types))
	 (while (setq spec (cdr spec))
	   (if (consp (car spec))
	       (if (eq (cadar spec) 0)
		   (setq byte-compile-warnings
			 (delq (caar spec) byte-compile-warnings))
		 (setq byte-compile-warnings
		       (adjoin (caar spec) byte-compile-warnings)))))))
  nil)

;;; Process any proclamations made before cl-macs was loaded.
(defvar cl-proclaims-deferred)
(let ((p (reverse cl-proclaims-deferred)))
  (while p (cl-do-proclaim (cl-pop p) t))
  (setq cl-proclaims-deferred nil))

(defmacro declare (&rest specs)
  (if (cl-compiling-file)
      (while specs
	(if (listp cl-declare-stack) (cl-push (car specs) cl-declare-stack))
	(cl-do-proclaim (cl-pop specs) nil)))
  nil)



;;; Generalized variables.

(defmacro define-setf-method (func args &rest body)
  "(define-setf-method NAME ARGLIST BODY...): define a `setf' method.
This method shows how to handle `setf's to places of the form (NAME ARGS...).
The argument forms ARGS are bound according to ARGLIST, as if NAME were
going to be expanded as a macro, then the BODY forms are executed and must
return a list of five elements: a temporary-variables list, a value-forms
list, a store-variables list (of length one), a store-form, and an access-
form.  See `defsetf' for a simpler way to define most setf-methods."
  (append '(eval-when (compile load eval))
	  (if (stringp (car body))
	      (list (list 'put (list 'quote func) '(quote setf-documentation)
			  (cl-pop body))))
	  (list (cl-transform-function-property
		 func 'setf-method (cons args body)))))

(defmacro defsetf (func arg1 &rest args)
  "(defsetf NAME FUNC): define a `setf' method.
This macro is an easy-to-use substitute for `define-setf-method' that works
well for simple place forms.  In the simple `defsetf' form, `setf's of
the form (setf (NAME ARGS...) VAL) are transformed to function or macro
calls of the form (FUNC ARGS... VAL).  Example: (defsetf aref aset).
Alternate form: (defsetf NAME ARGLIST (STORE) BODY...).
Here, the above `setf' call is expanded by binding the argument forms ARGS
according to ARGLIST, binding the value form VAL to STORE, then executing
BODY, which must return a Lisp form that does the necessary `setf' operation.
Actually, ARGLIST and STORE may be bound to temporary variables which are
introduced automatically to preserve proper execution order of the arguments.
Example: (defsetf nth (n x) (v) (list 'setcar (list 'nthcdr n x) v))."
  (if (listp arg1)
      (let* ((largs nil) (largsr nil)
	     (temps nil) (tempsr nil)
	     (restarg nil) (rest-temps nil)
	     (store-var (car (prog1 (car args) (setq args (cdr args)))))
	     (store-temp (intern (format "--%s--temp--" store-var)))
	     (lets1 nil) (lets2 nil)
	     (docstr nil) (p arg1))
	(if (stringp (car args))
	    (setq docstr (prog1 (car args) (setq args (cdr args)))))
	(while (and p (not (eq (car p) '&aux)))
	  (if (eq (car p) '&rest)
	      (setq p (cdr p) restarg (car p))
	    (or (memq (car p) '(&optional &key &allow-other-keys))
		(setq largs (cons (if (consp (car p)) (car (car p)) (car p))
				  largs)
		      temps (cons (intern (format "--%s--temp--" (car largs)))
				  temps))))
	  (setq p (cdr p)))
	(setq largs (nreverse largs) temps (nreverse temps))
	(if restarg
	    (setq largsr (append largs (list restarg))
		  rest-temps (intern (format "--%s--temp--" restarg))
		  tempsr (append temps (list rest-temps)))
	  (setq largsr largs tempsr temps))
	(let ((p1 largs) (p2 temps))
	  (while p1
	    (setq lets1 (cons (list (car p2)
				    (list 'gensym (format "--%s--" (car p1))))
			      lets1)
		  lets2 (cons (list (car p1) (car p2)) lets2)
		  p1 (cdr p1) p2 (cdr p2))))
	(if restarg (setq lets2 (cons (list restarg rest-temps) lets2)))
	(append (list 'define-setf-method func arg1)
		(and docstr (list docstr))
		(list
		 (list 'let*
		       (nreverse
			(cons (list store-temp
				    (list 'gensym (format "--%s--" store-var)))
			      (if restarg
				  (append
				   (list
				    (list rest-temps
					  (list 'mapcar '(quote gensym)
						restarg)))
				   lets1)
				lets1)))
		       (list 'list  ; 'values
			     (cons (if restarg 'list* 'list) tempsr)
			     (cons (if restarg 'list* 'list) largsr)
			     (list 'list store-temp)
			     (cons 'let*
				   (cons (nreverse
					  (cons (list store-var store-temp)
						lets2))
					 args))
			     (cons (if restarg 'list* 'list)
				   (cons (list 'quote func) tempsr)))))))
    (list 'defsetf func '(&rest args) '(store)
	  (let ((call (list 'cons (list 'quote arg1)
			    '(append args (list store)))))
	    (if (car args)
		(list 'list '(quote progn) call 'store)
	      call)))))

;;; Some standard place types from Common Lisp.
(defsetf aref aset)
(defsetf car setcar)
(defsetf cdr setcdr)
(defsetf elt (seq n) (store)
  (list 'if (list 'listp seq) (list 'setcar (list 'nthcdr n seq) store)
	(list 'aset seq n store)))
(defsetf get put)
(defsetf get* (x y &optional d) (store) (list 'put x y store))
(defsetf gethash (x h &optional d) (store) (list 'cl-puthash x store h))
(defsetf nth (n x) (store) (list 'setcar (list 'nthcdr n x) store))
(defsetf subseq (seq start &optional end) (new)
  (list 'progn (list 'replace seq new ':start1 start ':end1 end) new))
(defsetf symbol-function fset)
(defsetf symbol-plist setplist)
(defsetf symbol-value set)

;;; Various car/cdr aliases.  Note that `cadr' is handled specially.
(defsetf first setcar)
(defsetf second (x) (store) (list 'setcar (list 'cdr x) store))
(defsetf third (x) (store) (list 'setcar (list 'cddr x) store))
(defsetf fourth (x) (store) (list 'setcar (list 'cdddr x) store))
(defsetf fifth (x) (store) (list 'setcar (list 'nthcdr 4 x) store))
(defsetf sixth (x) (store) (list 'setcar (list 'nthcdr 5 x) store))
(defsetf seventh (x) (store) (list 'setcar (list 'nthcdr 6 x) store))
(defsetf eighth (x) (store) (list 'setcar (list 'nthcdr 7 x) store))
(defsetf ninth (x) (store) (list 'setcar (list 'nthcdr 8 x) store))
(defsetf tenth (x) (store) (list 'setcar (list 'nthcdr 9 x) store))
(defsetf rest setcdr)

;;; Some more Emacs-related place types.
(defsetf buffer-file-name set-visited-file-name t)
(defsetf buffer-modified-p set-buffer-modified-p t)
(defsetf buffer-name rename-buffer t)
(defsetf buffer-string () (store)
  (list 'progn '(erase-buffer) (list 'insert store)))
(defsetf buffer-substring cl-set-buffer-substring)
(defsetf current-buffer set-buffer)
(defsetf current-case-table set-case-table)
(defsetf current-column move-to-column t)
(defsetf current-global-map use-global-map t)
(defsetf current-input-mode () (store)
  (list 'progn (list 'apply 'set-input-mode store) store))
(defsetf current-local-map use-local-map t)
(defsetf current-window-configuration set-window-configuration t)
(defsetf default-file-modes set-default-file-modes t)
(defsetf default-value set-default)
(defsetf documentation-property put)
(defsetf extent-data set-extent-data)
(defsetf extent-face set-extent-face)
(defsetf extent-priority set-extent-priority)
(defsetf extent-end-position (ext) (store)
  (list 'progn (list 'set-extent-endpoints (list 'extent-start-position ext)
		     store) store))
(defsetf extent-start-position (ext) (store)
  (list 'progn (list 'set-extent-endpoints store
		     (list 'extent-end-position ext)) store))
(defsetf face-background (f &optional s) (x) (list 'set-face-background f x s))
(defsetf face-background-pixmap (f &optional s) (x)
  (list 'set-face-background-pixmap f x s))
(defsetf face-font (f &optional s) (x) (list 'set-face-font f x s))
(defsetf face-foreground (f &optional s) (x) (list 'set-face-foreground f x s))
(defsetf face-underline-p (f &optional s) (x)
  (list 'set-face-underline-p f x s))
(defsetf file-modes set-file-modes t)
(defsetf frame-height set-screen-height t)
(defsetf frame-parameters modify-frame-parameters t)
(defsetf frame-visible-p cl-set-frame-visible-p)
(defsetf frame-width set-screen-width t)
(defsetf getenv setenv t)
(defsetf get-register set-register)
(defsetf global-key-binding global-set-key)
(defsetf keymap-parent set-keymap-parent)
(defsetf local-key-binding local-set-key)
(defsetf mark set-mark t)
(defsetf mark-marker set-mark t)
(defsetf marker-position set-marker t)
(defsetf match-data store-match-data t)
(defsetf mouse-position (scr) (store)
  (list 'set-mouse-position scr (list 'car store) (list 'cadr store)
	(list 'cddr store)))
(defsetf overlay-get overlay-put)
(defsetf overlay-start (ov) (store)
  (list 'progn (list 'move-overlay ov store (list 'overlay-end ov)) store))
(defsetf overlay-end (ov) (store)
  (list 'progn (list 'move-overlay ov (list 'overlay-start ov) store) store))
(defsetf point goto-char)
(defsetf point-marker goto-char t)
(defsetf point-max () (store)
  (list 'progn (list 'narrow-to-region '(point-min) store) store))
(defsetf point-min () (store)
  (list 'progn (list 'narrow-to-region store '(point-max)) store))
(defsetf process-buffer set-process-buffer)
(defsetf process-filter set-process-filter)
(defsetf process-sentinel set-process-sentinel)
(defsetf read-mouse-position (scr) (store)
  (list 'set-mouse-position scr (list 'car store) (list 'cdr store)))
(defsetf screen-height set-screen-height t)
(defsetf screen-width set-screen-width t)
(defsetf selected-window select-window)
(defsetf selected-screen select-screen)
(defsetf selected-frame select-frame)
(defsetf standard-case-table set-standard-case-table)
(defsetf syntax-table set-syntax-table)
(defsetf visited-file-modtime set-visited-file-modtime t)
(defsetf window-buffer set-window-buffer t)
(defsetf window-display-table set-window-display-table t)
(defsetf window-dedicated-p set-window-dedicated-p t)
(defsetf window-height () (store)
  (list 'progn (list 'enlarge-window (list '- store '(window-height))) store))
(defsetf window-hscroll set-window-hscroll)
(defsetf window-point set-window-point)
(defsetf window-start set-window-start)
(defsetf window-width () (store)
  (list 'progn (list 'enlarge-window (list '- store '(window-width)) t) store))
(defsetf x-get-cutbuffer x-store-cutbuffer t)
(defsetf x-get-cut-buffer x-store-cut-buffer t)   ; groan.
(defsetf x-get-secondary-selection x-own-secondary-selection t)
(defsetf x-get-selection x-own-selection t)

;;; More complex setf-methods.
;;; These should take &environment arguments, but since full arglists aren't
;;; available while compiling cl-macs, we fake it by referring to the global
;;; variable cl-macro-environment directly.

(define-setf-method apply (func arg1 &rest rest)
  (or (and (memq (car-safe func) '(quote function function*))
	   (symbolp (car-safe (cdr-safe func))))
      (error "First arg to apply in setf is not (function SYM): %s" func))
  (let* ((form (cons (nth 1 func) (cons arg1 rest)))
	 (method (get-setf-method form cl-macro-environment)))
    (list (car method) (nth 1 method) (nth 2 method)
	  (cl-setf-make-apply (nth 3 method) (cadr func) (car method))
	  (cl-setf-make-apply (nth 4 method) (cadr func) (car method)))))

(defun cl-setf-make-apply (form func temps)
  (if (eq (car form) 'progn)
      (list* 'progn (cl-setf-make-apply (cadr form) func temps) (cddr form))
    (or (equal (last form) (last temps))
	(error "%s is not suitable for use with setf-of-apply" func))
    (list* 'apply (list 'quote (car form)) (cdr form))))

(define-setf-method nthcdr (n place)
  (let ((method (get-setf-method place cl-macro-environment))
	(n-temp (gensym "--nthcdr-n--"))
	(store-temp (gensym "--nthcdr-store--")))
    (list (cons n-temp (car method))
	  (cons n (nth 1 method))
	  (list store-temp)
	  (list 'let (list (list (car (nth 2 method))
				 (list 'cl-set-nthcdr n-temp (nth 4 method)
				       store-temp)))
		(nth 3 method) store-temp)
	  (list 'nthcdr n-temp (nth 4 method)))))

(define-setf-method getf (place tag &optional def)
  (let ((method (get-setf-method place cl-macro-environment))
	(tag-temp (gensym "--getf-tag--"))
	(def-temp (gensym "--getf-def--"))
	(store-temp (gensym "--getf-store--")))
    (list (append (car method) (list tag-temp def-temp))
	  (append (nth 1 method) (list tag def))
	  (list store-temp)
	  (list 'let (list (list (car (nth 2 method))
				 (list 'cl-set-getf (nth 4 method)
				       tag-temp store-temp)))
		(nth 3 method) store-temp)
	  (list 'getf (nth 4 method) tag-temp def-temp))))

(define-setf-method substring (place from &optional to)
  (let ((method (get-setf-method place cl-macro-environment))
	(from-temp (gensym "--substring-from--"))
	(to-temp (gensym "--substring-to--"))
	(store-temp (gensym "--substring-store--")))
    (list (append (car method) (list from-temp to-temp))
	  (append (nth 1 method) (list from to))
	  (list store-temp)
	  (list 'let (list (list (car (nth 2 method))
				 (list 'cl-set-substring (nth 4 method)
				       from-temp to-temp store-temp)))
		(nth 3 method) store-temp)
	  (list 'substring (nth 4 method) from-temp to-temp))))

;;; Getting and optimizing setf-methods.
(defun get-setf-method (place &optional env)
  "Return a list of five values describing the setf-method for PLACE.
PLACE may be any Lisp form which can appear as the PLACE argument to
a macro like `setf' or `incf'."
  (if (symbolp place)
      (let ((temp (gensym "--setf--")))
	(list nil nil (list temp) (list 'setq place temp) place))
    (or (and (symbolp (car place))
	     (let* ((func (car place))
		    (name (symbol-name func))
		    (method (get func 'setf-method))
		    (case-fold-search nil))
	       (or (and method
			(let ((cl-macro-environment env))
			  (setq method (apply method (cdr place))))
			(if (and (consp method) (= (length method) 5))
			    method
			  (error "Setf-method for %s returns malformed method"
				 func)))
		   (and (string-match "\\`c[ad][ad][ad]?[ad]?r\\'" name)
			(get-setf-method (compiler-macroexpand place)))
		   (and (eq func 'edebug-after)
			(get-setf-method (nth (1- (length place)) place)
					 env)))))
	(if (eq place (setq place (macroexpand place env)))
	    (if (and (symbolp (car place)) (fboundp (car place))
		     (symbolp (symbol-function (car place))))
		(get-setf-method (cons (symbol-function (car place))
				       (cdr place)) env)
	      (error "No setf-method known for %s" (car place)))
	  (get-setf-method place env)))))

(defun cl-setf-do-modify (place opt-expr)
  (let* ((method (get-setf-method place cl-macro-environment))
	 (temps (car method)) (values (nth 1 method))
	 (lets nil) (subs nil)
	 (optimize (and (not (eq opt-expr 'no-opt))
			(or (and (not (eq opt-expr 'unsafe))
				 (cl-safe-expr-p opt-expr))
			    (cl-setf-simple-store-p (car (nth 2 method))
						    (nth 3 method)))))
	 (simple (and optimize (consp place) (cl-simple-exprs-p (cdr place)))))
    (while values
      (if (or simple (cl-const-expr-p (car values)))
	  (cl-push (cons (cl-pop temps) (cl-pop values)) subs)
	(cl-push (list (cl-pop temps) (cl-pop values)) lets)))
    (list (nreverse lets)
	  (cons (car (nth 2 method)) (sublis subs (nth 3 method)))
	  (sublis subs (nth 4 method)))))

(defun cl-setf-do-store (spec val)
  (let ((sym (car spec))
	(form (cdr spec)))
    (if (or (cl-const-expr-p val)
	    (and (cl-simple-expr-p val) (eq (cl-expr-contains form sym) 1))
	    (cl-setf-simple-store-p sym form))
	(subst val sym form)
      (list 'let (list (list sym val)) form))))

(defun cl-setf-simple-store-p (sym form)
  (and (consp form) (eq (cl-expr-contains form sym) 1)
       (eq (nth (1- (length form)) form) sym)
       (symbolp (car form)) (fboundp (car form))
       (not (eq (car-safe (symbol-function (car form))) 'macro))))

;;; The standard modify macros.
(defmacro setf (&rest args)
  "(setf PLACE VAL PLACE VAL ...): set each PLACE to the value of its VAL.
This is a generalized version of `setq'; the PLACEs may be symbolic
references such as (car x) or (aref x i), as well as plain symbols.
For example, (setf (cadar x) y) is equivalent to (setcar (cdar x) y).
The return value is the last VAL in the list."
  (if (cdr (cdr args))
      (let ((sets nil))
	(while args (cl-push (list 'setf (cl-pop args) (cl-pop args)) sets))
	(cons 'progn (nreverse sets)))
    (if (symbolp (car args))
	(and args (cons 'setq args))
      (let* ((method (cl-setf-do-modify (car args) (nth 1 args)))
	     (store (cl-setf-do-store (nth 1 method) (nth 1 args))))
	(if (car method) (list 'let* (car method) store) store)))))

(defmacro psetf (&rest args)
  "(psetf PLACE VAL PLACE VAL ...): set PLACEs to the values VALs in parallel.
This is like `setf', except that all VAL forms are evaluated (in order)
before assigning any PLACEs to the corresponding values."
  (let ((p args) (simple t) (vars nil))
    (while p
      (if (or (not (symbolp (car p))) (cl-expr-depends-p (nth 1 p) vars))
	  (setq simple nil))
      (if (memq (car p) vars)
	  (error "Destination duplicated in psetf: %s" (car p)))
      (cl-push (cl-pop p) vars)
      (or p (error "Odd number of arguments to psetf"))
      (cl-pop p))
    (if simple
	(list 'progn (cons 'setf args) nil)
      (setq args (reverse args))
      (let ((expr (list 'setf (cadr args) (car args))))
	(while (setq args (cddr args))
	  (setq expr (list 'setf (cadr args) (list 'prog1 (car args) expr))))
	(list 'progn expr nil)))))

(defun cl-do-pop (place)
  (if (cl-simple-expr-p place)
      (list 'prog1 (list 'car place) (list 'setf place (list 'cdr place)))
    (let* ((method (cl-setf-do-modify place t))
	   (temp (gensym "--pop--")))
      (list 'let*
	    (append (car method)
		    (list (list temp (nth 2 method))))
	    (list 'prog1
		  (list 'car temp)
		  (cl-setf-do-store (nth 1 method) (list 'cdr temp)))))))

(defmacro remf (place tag)
  "(remf PLACE TAG): remove TAG from property list PLACE.
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The form returns true if TAG was found and removed, nil otherwise."
  (let* ((method (cl-setf-do-modify place t))
	 (tag-temp (and (not (cl-const-expr-p tag)) (gensym "--remf-tag--")))
	 (val-temp (and (not (cl-simple-expr-p place))
			(gensym "--remf-place--")))
	 (ttag (or tag-temp tag))
	 (tval (or val-temp (nth 2 method))))
    (list 'let*
	  (append (car method)
		  (and val-temp (list (list val-temp (nth 2 method))))
		  (and tag-temp (list (list tag-temp tag))))
	  (list 'if (list 'eq ttag (list 'car tval))
		(list 'progn
		      (cl-setf-do-store (nth 1 method) (list 'cddr tval))
		      t)
		(list 'cl-do-remf tval ttag)))))

(defmacro shiftf (place &rest args)
  "(shiftf PLACE PLACE... VAL): shift left among PLACEs.
Example: (shiftf A B C) sets A to B, B to C, and returns the old A.
Each PLACE may be a symbol, or any generalized variable allowed by `setf'."
  (if (not (memq nil (mapcar 'symbolp (butlast (cons place args)))))
      (list* 'prog1 place
	     (let ((sets nil))
	       (while args
		 (cl-push (list 'setq place (car args)) sets)
		 (setq place (cl-pop args)))
	       (nreverse sets)))
    (let* ((places (reverse (cons place args)))
	   (form (cl-pop places)))
      (while places
	(let ((method (cl-setf-do-modify (cl-pop places) 'unsafe)))
	  (setq form (list 'let* (car method)
			   (list 'prog1 (nth 2 method)
				 (cl-setf-do-store (nth 1 method) form))))))
      form)))

(defmacro rotatef (&rest args)
  "(rotatef PLACE...): rotate left among PLACEs.
Example: (rotatef A B C) sets A to B, B to C, and C to A.  It returns nil.
Each PLACE may be a symbol, or any generalized variable allowed by `setf'."
  (if (not (memq nil (mapcar 'symbolp args)))
      (and (cdr args)
	   (let ((sets nil)
		 (first (car args)))
	     (while (cdr args)
	       (setq sets (nconc sets (list (cl-pop args) (car args)))))
	     (nconc (list 'psetf) sets (list (car args) first))))
    (let* ((places (reverse args))
	   (temp (gensym "--rotatef--"))
	   (form temp))
      (while (cdr places)
	(let ((method (cl-setf-do-modify (cl-pop places) 'unsafe)))
	  (setq form (list 'let* (car method)
			   (list 'prog1 (nth 2 method)
				 (cl-setf-do-store (nth 1 method) form))))))
      (let ((method (cl-setf-do-modify (car places) 'unsafe)))
	(list 'let* (append (car method) (list (list temp (nth 2 method))))
	      (cl-setf-do-store (nth 1 method) form) nil)))))

(defmacro letf (bindings &rest body)
  "(letf ((PLACE VALUE) ...) BODY...): temporarily bind to PLACEs.
This is the analogue of `let', but with generalized variables (in the
sense of `setf') for the PLACEs.  Each PLACE is set to the corresponding
VALUE, then the BODY forms are executed.  On exit, either normally or
because of a `throw' or error, the PLACEs are set back to their original
values.  Note that this macro is *not* available in Common Lisp.
As a special case, if `(PLACE)' is used instead of `(PLACE VALUE)',
the PLACE is not modified before executing BODY."
  (if (and (not (cdr bindings)) (cdar bindings) (symbolp (caar bindings)))
      (list* 'let bindings body)
    (let ((lets nil) (sets nil)
	  (unsets nil) (rev (reverse bindings)))
      (while rev
	(let* ((place (if (symbolp (caar rev))
			  (list 'symbol-value (list 'quote (caar rev)))
			(caar rev)))
	       (value (cadar rev))
	       (method (cl-setf-do-modify place 'no-opt))
	       (save (gensym "--letf-save--"))
	       (bound (and (memq (car place) '(symbol-value symbol-function))
			   (gensym "--letf-bound--")))
	       (temp (and (not (cl-const-expr-p value)) (cdr bindings)
			  (gensym "--letf-val--"))))
	  (setq lets (nconc (car method)
			    (if bound
				(list (list bound
					    (list (if (eq (car place)
							  'symbol-value)
						      'boundp 'fboundp)
						  (nth 1 (nth 2 method))))
				      (list save (list 'and bound
						       (nth 2 method))))
			      (list (list save (nth 2 method))))
			    (and temp (list (list temp value)))
			    lets)
		body (list
		      (list 'unwind-protect
			    (cons 'progn
				  (if (cdr (car rev))
				      (cons (cl-setf-do-store (nth 1 method)
							      (or temp value))
					    body)
				    body))
			    (if bound
				(list 'if bound
				      (cl-setf-do-store (nth 1 method) save)
				      (list (if (eq (car place) 'symbol-value)
						'makunbound 'fmakunbound)
					    (nth 1 (nth 2 method))))
			      (cl-setf-do-store (nth 1 method) save))))
		rev (cdr rev))))
      (list* 'let* lets body))))

(defmacro letf* (bindings &rest body)
  "(letf* ((PLACE VALUE) ...) BODY...): temporarily bind to PLACEs.
This is the analogue of `let*', but with generalized variables (in the
sense of `setf') for the PLACEs.  Each PLACE is set to the corresponding
VALUE, then the BODY forms are executed.  On exit, either normally or
because of a `throw' or error, the PLACEs are set back to their original
values.  Note that this macro is *not* available in Common Lisp.
As a special case, if `(PLACE)' is used instead of `(PLACE VALUE)',
the PLACE is not modified before executing BODY."
  (if (null bindings)
      (cons 'progn body)
    (setq bindings (reverse bindings))
    (while bindings
      (setq body (list (list* 'letf (list (cl-pop bindings)) body))))
    (car body)))

(defmacro callf (func place &rest args)
  "(callf FUNC PLACE ARGS...): set PLACE to (FUNC PLACE ARGS...).
FUNC should be an unquoted function name.  PLACE may be a symbol,
or any generalized variable allowed by `setf'."
  (let* ((method (cl-setf-do-modify place (cons 'list args)))
	 (rargs (cons (nth 2 method) args)))
    (list 'let* (car method)
	  (cl-setf-do-store (nth 1 method)
			    (if (symbolp func) (cons func rargs)
			      (list* 'funcall (list 'function func)
				     rargs))))))

(defmacro callf2 (func arg1 place &rest args)
  "(callf2 FUNC ARG1 PLACE ARGS...): set PLACE to (FUNC ARG1 PLACE ARGS...).
Like `callf', but PLACE is the second argument of FUNC, not the first."
  (if (and (cl-safe-expr-p arg1) (cl-simple-expr-p place) (symbolp func))
      (list 'setf place (list* func arg1 place args))
    (let* ((method (cl-setf-do-modify place (cons 'list args)))
	   (temp (and (not (cl-const-expr-p arg1)) (gensym "--arg1--")))
	   (rargs (list* (or temp arg1) (nth 2 method) args)))
      (list 'let* (append (and temp (list (list temp arg1))) (car method))
	    (cl-setf-do-store (nth 1 method)
			      (if (symbolp func) (cons func rargs)
				(list* 'funcall (list 'function func)
				       rargs)))))))

(defmacro define-modify-macro (name arglist func &optional doc)
  "(define-modify-macro NAME ARGLIST FUNC): define a `setf'-like modify macro.
If NAME is called, it combines its PLACE argument with the other arguments
from ARGLIST using FUNC: (define-modify-macro incf (&optional (n 1)) +)"
  (if (memq '&key arglist) (error "&key not allowed in define-modify-macro"))
  (let ((place (gensym "--place--")))
    (list 'defmacro* name (cons place arglist) doc
	  (list* (if (memq '&rest arglist) 'list* 'list)
		 '(quote callf) (list 'quote func) place
		 (cl-arglist-args arglist)))))


;;; Structures.

(defmacro defstruct (struct &rest descs)
  "(defstruct (NAME OPTIONS...) (SLOT SLOT-OPTS...)...): define a struct type.
This macro defines a new Lisp data type called NAME, which contains data
stored in SLOTs.  This defines a `make-NAME' constructor, a `copy-NAME'
copier, a `NAME-p' predicate, and setf-able `NAME-SLOT' accessors."
  (let* ((name (if (consp struct) (car struct) struct))
	 (opts (cdr-safe struct))
	 (slots nil)
	 (defaults nil)
	 (conc-name (concat (symbol-name name) "-"))
	 (constructor (intern (format "make-%s" name)))
	 (constrs nil)
	 (copier (intern (format "copy-%s" name)))
	 (predicate (intern (format "%s-p" name)))
	 (print-func nil) (print-auto nil)
	 (safety (if (cl-compiling-file) cl-optimize-safety 3))
	 (include nil)
	 (tag (intern (format "cl-struct-%s" name)))
	 (tag-symbol (intern (format "cl-struct-%s-tags" name)))
	 (include-descs nil)
	 (include-tag-symbol nil)
	 (side-eff nil)
	 (type nil)
	 (named nil)
	 (forms nil)
	 pred-form pred-check)
    (if (stringp (car descs))
	(cl-push (list 'put (list 'quote name) '(quote structure-documentation)
		       (cl-pop descs)) forms))
    (setq descs (cons '(cl-tag-slot)
		      (mapcar (function (lambda (x) (if (consp x) x (list x))))
			      descs)))
    (while opts
      (let ((opt (if (consp (car opts)) (caar opts) (car opts)))
	    (args (cdr-safe (cl-pop opts))))
	(cond ((eq opt ':conc-name)
	       (if args
		   (setq conc-name (if (car args)
				       (symbol-name (car args)) ""))))
	      ((eq opt ':constructor)
	       (if (cdr args)
		   (cl-push args constrs)
		 (if args (setq constructor (car args)))))
	      ((eq opt ':copier)
	       (if args (setq copier (car args))))
	      ((eq opt ':predicate)
	       (if args (setq predicate (car args))))
	      ((eq opt ':include)
	       (setq include (car args)
		     include-descs (mapcar (function
					    (lambda (x)
					      (if (consp x) x (list x))))
					   (cdr args))
		     include-tag-symbol (intern (format "cl-struct-%s-tags"
							include))))
	      ((eq opt ':print-function)
	       (setq print-func (car args)))
	      ((eq opt ':type)
	       (setq type (car args)))
	      ((eq opt ':named)
	       (setq named t))
	      ((eq opt ':initial-offset)
	       (setq descs (nconc (make-list (car args) '(cl-skip-slot))
				  descs)))
	      (t
	       (error "Slot option %s unrecognized" opt)))))
    (if print-func
	(setq print-func (list 'progn
			       (list 'funcall (list 'function print-func)
				     'cl-x 'cl-s 'cl-n) t))
      (or type (and include (not (get include 'cl-struct-print)))
	  (setq print-auto t
		print-func (and (or (not (or include type)) (null print-func))
				(list 'progn
				      (list 'princ (format "#S(%s" name)
					    'cl-s))))))
    (if include
	(let ((inc-type (get include 'cl-struct-type))
	      (old-descs (get include 'cl-struct-slots)))
	  (or inc-type (error "%s is not a struct name" include))
	  (and type (not (eq (car inc-type) type))
	       (error ":type disagrees with :include for %s" name))
	  (while include-descs
	    (setcar (memq (or (assq (caar include-descs) old-descs)
			      (error "No slot %s in included struct %s"
				     (caar include-descs) include))
			  old-descs)
		    (cl-pop include-descs)))
	  (setq descs (append old-descs (delq (assq 'cl-tag-slot descs) descs))
		type (car inc-type)
		named (assq 'cl-tag-slot descs))
	  (if (cadr inc-type) (setq tag name named t))
	  (cl-push (list 'pushnew (list 'quote tag) include-tag-symbol)
		   forms))
      (if type
	  (progn
	    (or (memq type '(vector list))
		(error "Illegal :type specifier: %s" type))
	    (if named (setq tag name)))
	(setq type 'vector named 'true)))
    (or named (setq descs (delq (assq 'cl-tag-slot descs) descs)))
    (cl-push (list 'defvar tag-symbol) forms)
    (setq pred-form (and named
			 (let ((pos (- (length descs)
				       (length (memq (assq 'cl-tag-slot descs)
						     descs)))))
			   (if (eq type 'vector)
			       (list 'and '(vectorp cl-x)
				     (list '>= '(length cl-x) (length descs))
				     (list 'memq (list 'aref 'cl-x pos)
					   tag-symbol))
			     (if (= pos 0)
				 (list 'memq '(car-safe cl-x) tag-symbol)
			       (list 'and '(consp cl-x)
				     (list 'memq (list 'nth pos 'cl-x)
					   tag-symbol))))))
	  pred-check (and pred-form (> safety 0)
			  (if (and (eq (caadr pred-form) 'vectorp)
				   (= safety 1))
			      (cons 'and (cdddr pred-form)) pred-form)))
    (let ((pos 0) (descp descs))
      (while descp
	(let* ((desc (cl-pop descp))
	       (slot (car desc)))
	  (if (memq slot '(cl-tag-slot cl-skip-slot))
	      (progn
		(cl-push nil slots)
		(cl-push (and (eq slot 'cl-tag-slot) (list 'quote tag))
			 defaults))
	    (if (assq slot descp)
		(error "Duplicate slots named %s in %s" slot name))
	    (let ((accessor (intern (format "%s%s" conc-name slot))))
	      (cl-push slot slots)
	      (cl-push (nth 1 desc) defaults)
	      (cl-push (list*
			'defsubst* accessor '(cl-x)
			(append
			 (and pred-check
			      (list (list 'or pred-check
					  (list 'error
						(format "%s accessing a non-%s"
							accessor name)
						'cl-x))))
			 (list (if (eq type 'vector) (list 'aref 'cl-x pos)
				 (if (= pos 0) '(car cl-x)
				   (list 'nth pos 'cl-x)))))) forms)
	      (cl-push (cons accessor t) side-eff)
	      (cl-push (list 'define-setf-method accessor '(cl-x)
			     (if (cadr (memq ':read-only (cddr desc)))
				 (list 'error (format "%s is a read-only slot"
						      accessor))
			       (list 'cl-struct-setf-expander 'cl-x
				     (list 'quote name) (list 'quote accessor)
				     (and pred-check (list 'quote pred-check))
				     pos)))
		       forms)
	      (if print-auto
		  (nconc print-func
			 (list (list 'princ (format " %s" slot) 'cl-s)
			       (list 'prin1 (list accessor 'cl-x) 'cl-s)))))))
	(setq pos (1+ pos))))
    (setq slots (nreverse slots)
	  defaults (nreverse defaults))
    (and predicate pred-form
	 (progn (cl-push (list 'defsubst* predicate '(cl-x)
			       (if (eq (car pred-form) 'and)
				   (append pred-form '(t))
				 (list 'and pred-form t))) forms)
		(cl-push (cons predicate 'error-free) side-eff)))
    (and copier
	 (progn (cl-push (list 'defun copier '(x) '(copy-sequence x)) forms)
		(cl-push (cons copier t) side-eff)))
    (if constructor
	(cl-push (list constructor
		       (cons '&key (delq nil (copy-sequence slots))))
		 constrs))
    (while constrs
      (let* ((name (caar constrs))
	     (args (cadr (cl-pop constrs)))
	     (anames (cl-arglist-args args))
	     (make (mapcar* (function (lambda (s d) (if (memq s anames) s d)))
			    slots defaults)))
	(cl-push (list 'defsubst* name
		       (list* '&cl-defs (list 'quote (cons nil descs)) args)
		       (cons type make)) forms)
	(if (cl-safe-expr-p (cons 'progn (mapcar 'second descs)))
	    (cl-push (cons name t) side-eff))))
    (if print-auto (nconc print-func (list '(princ ")" cl-s) t)))
    (if print-func
	(cl-push (list 'push
		       (list 'function
			     (list 'lambda '(cl-x cl-s cl-n)
				   (list 'and pred-form print-func)))
		       'custom-print-functions) forms))
    (cl-push (list 'setq tag-symbol (list 'list (list 'quote tag))) forms)
    (cl-push (list* 'eval-when '(compile load eval)
		    (list 'put (list 'quote name) '(quote cl-struct-slots)
			  (list 'quote descs))
		    (list 'put (list 'quote name) '(quote cl-struct-type)
			  (list 'quote (list type (eq named t))))
		    (list 'put (list 'quote name) '(quote cl-struct-print)
			  print-auto)
		    (mapcar (function (lambda (x)
					(list 'put (list 'quote (car x))
					      '(quote side-effect-free)
					      (list 'quote (cdr x)))))
			    side-eff))
	     forms)
    (cons 'progn (nreverse (cons (list 'quote name) forms)))))

(defun cl-struct-setf-expander (x name accessor pred-form pos)
  (let* ((temp (gensym "--x--")) (store (gensym "--store--")))
    (list (list temp) (list x) (list store)
	  (append '(progn)
		  (and pred-form
		       (list (list 'or (subst temp 'cl-x pred-form)
				   (list 'error
					 (format
					  "%s storing a non-%s" accessor name)
					 temp))))
		  (list (if (eq (car (get name 'cl-struct-type)) 'vector)
			    (list 'aset temp pos store)
			  (list 'setcar
				(if (<= pos 5)
				    (let ((xx temp))
				      (while (>= (setq pos (1- pos)) 0)
					(setq xx (list 'cdr xx)))
				      xx)
				  (list 'nthcdr pos temp))
				store))))
	  (list accessor temp))))


;;; Types and assertions.

(defmacro deftype (name args &rest body)
  "(deftype NAME ARGLIST BODY...): define NAME as a new data type.
The type name can then be used in `typecase', `check-type', etc."
  (list 'eval-when '(compile load eval)
	(cl-transform-function-property
	 name 'cl-deftype-handler (cons (list* '&cl-defs ''('*) args) body))))

(defun cl-make-type-test (val type)
  (if (memq type '(character string-char)) (setq type '(integer 0 255)))
  (if (symbolp type)
      (cond ((get type 'cl-deftype-handler)
	     (cl-make-type-test val (funcall (get type 'cl-deftype-handler))))
	    ((memq type '(nil t)) type)
	    ((eq type 'null) (list 'null val))
	    ((eq type 'float) (list 'floatp-safe val))
	    ((eq type 'real) (list 'numberp val))
	    ((eq type 'fixnum) (list 'integerp val))
	    (t
	     (let* ((name (symbol-name type))
		    (namep (intern (concat name "p"))))
	       (if (fboundp namep) (list namep val)
		 (list (intern (concat name "-p")) val)))))
    (cond ((get (car type) 'cl-deftype-handler)
	   (cl-make-type-test val (apply (get (car type) 'cl-deftype-handler)
					 (cdr type))))
	  ((memq (car-safe type) '(integer float real number))
	   (delq t (list 'and (cl-make-type-test val (car type))
			 (if (memq (cadr type) '(* nil)) t
			   (if (consp (cadr type)) (list '> val (caadr type))
			     (list '>= val (cadr type))))
			 (if (memq (caddr type) '(* nil)) t
			   (if (consp (caddr type)) (list '< val (caaddr type))
			     (list '<= val (caddr type)))))))
	  ((memq (car-safe type) '(and or not))
	   (cons (car type)
		 (mapcar (function (lambda (x) (cl-make-type-test val x)))
			 (cdr type))))
	  ((memq (car-safe type) '(member member*))
	   (list 'and (list 'member* val (list 'quote (cdr type))) t))
	  ((eq (car-safe type) 'satisfies) (list (cadr type) val))
	  (t (error "Bad type spec: %s" type)))))

(defun typep (val type)   ; See compiler macro below.
  "Check that OBJECT is of type TYPE.
TYPE is a Common Lisp-style type specifier."
  (eval (cl-make-type-test 'val type)))

(defmacro check-type (form type &optional string)
  "Verify that FORM is of type TYPE; signal an error if not.
STRING is an optional description of the desired type."
  (and (or (not (cl-compiling-file))
	   (< cl-optimize-speed 3) (= cl-optimize-safety 3))
       (let* ((temp (if (cl-simple-expr-p form 3) form (gensym)))
	      (body (list 'or (cl-make-type-test temp type)
			  (list 'signal '(quote wrong-type-argument)
				(list 'list (or string (list 'quote type))
				      temp (list 'quote form))))))
	 (if (eq temp form) (list 'progn body nil)
	   (list 'let (list (list temp form)) body nil)))))

(defmacro assert (form &optional show-args string &rest args)
  "Verify that FORM returns non-nil; signal an error if not.
Second arg SHOW-ARGS means to include arguments of FORM in message.
Other args STRING and ARGS... are arguments to be passed to `error'.
They are not evaluated unless the assertion fails.  If STRING is
omitted, a default message listing FORM itself is used."
  (and (or (not (cl-compiling-file))
	   (< cl-optimize-speed 3) (= cl-optimize-safety 3))
       (let ((sargs (and show-args (delq nil (mapcar
					      (function
					       (lambda (x)
						 (and (not (cl-const-expr-p x))
						      x))) (cdr form))))))
	 (list 'progn
	       (list 'or form
		     (if string
			 (list* 'error string (append sargs args))
		       (list 'signal '(quote cl-assertion-failed)
			     (list* 'list (list 'quote form) sargs))))
	       nil))))

(defmacro ignore-errors (&rest body)
  "Execute FORMS; if an error occurs, return nil.
Otherwise, return result of last FORM."
  (let ((err (gensym)))
    (list 'condition-case err (cons 'progn body) '(error nil))))


;;; Some predicates for analyzing Lisp forms.  These are used by various
;;; macro expanders to optimize the results in certain common cases.

(defconst cl-simple-funcs '(car cdr nth aref elt if and or + - 1+ 1- min max
			    car-safe cdr-safe progn prog1 prog2))
(defconst cl-safe-funcs '(* / % length memq list vector vectorp
			  < > <= >= = error))

;;; Check if no side effects, and executes quickly.
(defun cl-simple-expr-p (x &optional size)
  (or size (setq size 10))
  (if (and (consp x) (not (memq (car x) '(quote function function*))))
      (and (symbolp (car x))
	   (or (memq (car x) cl-simple-funcs)
	       (get (car x) 'side-effect-free))
	   (progn
	     (setq size (1- size))
	     (while (and (setq x (cdr x))
			 (setq size (cl-simple-expr-p (car x) size))))
	     (and (null x) (>= size 0) size)))
    (and (> size 0) (1- size))))

(defun cl-simple-exprs-p (xs)
  (while (and xs (cl-simple-expr-p (car xs)))
    (setq xs (cdr xs)))
  (not xs))

;;; Check if no side effects.
(defun cl-safe-expr-p (x)
  (or (not (and (consp x) (not (memq (car x) '(quote function function*)))))
      (and (symbolp (car x))
	   (or (memq (car x) cl-simple-funcs)
	       (memq (car x) cl-safe-funcs)
	       (get (car x) 'side-effect-free))
	   (progn
	     (while (and (setq x (cdr x)) (cl-safe-expr-p (car x))))
	     (null x)))))

;;; Check if constant (i.e., no side effects or dependencies).
(defun cl-const-expr-p (x)
  (cond ((consp x)
	 (or (eq (car x) 'quote)
	     (and (memq (car x) '(function function*))
		  (or (symbolp (nth 1 x))
		      (and (eq (car-safe (nth 1 x)) 'lambda) 'func)))))
	((symbolp x) (and (memq x '(nil t)) t))
	(t t)))

(defun cl-const-exprs-p (xs)
  (while (and xs (cl-const-expr-p (car xs)))
    (setq xs (cdr xs)))
  (not xs))

(defun cl-const-expr-val (x)
  (and (eq (cl-const-expr-p x) t) (if (consp x) (nth 1 x) x)))

(defun cl-expr-access-order (x v)
  (if (cl-const-expr-p x) v
    (if (consp x)
	(progn
	  (while (setq x (cdr x)) (setq v (cl-expr-access-order (car x) v)))
	  v)
      (if (eq x (car v)) (cdr v) '(t)))))

;;; Count number of times X refers to Y.  Return NIL for 0 times.
(defun cl-expr-contains (x y)
  (cond ((equal y x) 1)
	((and (consp x) (not (memq (car-safe x) '(quote function function*))))
	 (let ((sum 0))
	   (while x
	     (setq sum (+ sum (or (cl-expr-contains (cl-pop x) y) 0))))
	   (and (> sum 0) sum)))
	(t nil)))

(defun cl-expr-contains-any (x y)
  (while (and y (not (cl-expr-contains x (car y)))) (cl-pop y))
  y)

;;; Check whether X may depend on any of the symbols in Y.
(defun cl-expr-depends-p (x y)
  (and (not (cl-const-expr-p x))
       (or (not (cl-safe-expr-p x)) (cl-expr-contains-any x y))))


;;; Compiler macros.

(defmacro define-compiler-macro (func args &rest body)
  "(define-compiler-macro FUNC ARGLIST BODY...): Define a compiler-only macro.
This is like `defmacro', but macro expansion occurs only if the call to
FUNC is compiled (i.e., not interpreted).  Compiler macros should be used
for optimizing the way calls to FUNC are compiled; the form returned by
BODY should do the same thing as a call to the normal function called
FUNC, though possibly more efficiently.  Note that, like regular macros,
compiler macros are expanded repeatedly until no further expansions are
possible.  Unlike regular macros, BODY can decide to \"punt\" and leave the
original function call alone by declaring an initial `&whole foo' parameter
and then returning foo."
  (let ((p (if (listp args) args (list '&rest args))) (res nil))
    (while (consp p) (cl-push (cl-pop p) res))
    (setq args (nreverse res)) (setcdr res (and p (list '&rest p))))
  (list 'eval-when '(compile load eval)
	(cl-transform-function-property
	 func 'cl-compiler-macro
	 (cons (if (memq '&whole args) (delq '&whole args)
		 (cons '--cl-whole-arg-- args)) body))
	(list 'or (list 'get (list 'quote func) '(quote byte-compile))
	      (list 'put (list 'quote func) '(quote byte-compile)
		    '(quote cl-byte-compile-compiler-macro)))))

(defun compiler-macroexpand (form)
  (while
      (let ((func (car-safe form)) (handler nil))
	(while (and (symbolp func)
		    (not (setq handler (get func 'cl-compiler-macro)))
		    (fboundp func)
		    (or (not (eq (car-safe (symbol-function func)) 'autoload))
			(load (nth 1 (symbol-function func)))))
	  (setq func (symbol-function func)))
	(and handler
	     (not (eq form (setq form (apply handler form (cdr form))))))))
  form)

(defun cl-byte-compile-compiler-macro (form)
  (if (eq form (setq form (compiler-macroexpand form)))
      (byte-compile-normal-call form)
    (byte-compile-form form)))

(defmacro defsubst* (name args &rest body)
  "(defsubst* NAME ARGLIST [DOCSTRING] BODY...): define NAME as a function.
Like `defun', except the function is automatically declared `inline',
ARGLIST allows full Common Lisp conventions, and BODY is implicitly
surrounded by (block NAME ...)."
  (let* ((argns (cl-arglist-args args)) (p argns)
	 (pbody (cons 'progn body))
	 (unsafe (not (cl-safe-expr-p pbody))))
    (while (and p (eq (cl-expr-contains args (car p)) 1)) (cl-pop p))
    (list 'progn
	  (if p nil   ; give up if defaults refer to earlier args
	    (list 'define-compiler-macro name
		  (list* '&whole 'cl-whole '&cl-quote args)
		  (list* 'cl-defsubst-expand (list 'quote argns)
			 (list 'quote (list* 'block name body))
			 (not (or unsafe (cl-expr-access-order pbody argns)))
			 (and (memq '&key args) 'cl-whole) unsafe argns)))
	  (list* 'defun* name args body))))

(defun cl-defsubst-expand (argns body simple whole unsafe &rest argvs)
  (if (and whole (not (cl-safe-expr-p (cons 'progn argvs)))) whole
    (if (cl-simple-exprs-p argvs) (setq simple t))
    (let ((lets (delq nil
		      (mapcar* (function
				(lambda (argn argv)
				  (if (or simple (cl-const-expr-p argv))
				      (progn (setq body (subst argv argn body))
					     (and unsafe (list argn argv)))
				    (list argn argv))))
			       argns argvs))))
      (if lets (list 'let lets body) body))))


;;; Compile-time optimizations for some functions defined in this package.
;;; Note that cl.el arranges to force cl-macs to be loaded at compile-time,
;;; mainly to make sure these macros will be present.

(put 'eql 'byte-compile nil)
(define-compiler-macro eql (&whole form a b)
  (cond ((eq (cl-const-expr-p a) t)
	 (let ((val (cl-const-expr-val a)))
	   (if (and (numberp val) (not (integerp val)))
	       (list 'equal a b)
	     (list 'eq a b))))
	((eq (cl-const-expr-p b) t)
	 (let ((val (cl-const-expr-val b)))
	   (if (and (numberp val) (not (integerp val)))
	       (list 'equal a b)
	     (list 'eq a b))))
	((cl-simple-expr-p a 5)
	 (list 'if (list 'numberp a)
	       (list 'equal a b)
	       (list 'eq a b)))
	((and (cl-safe-expr-p a)
	      (cl-simple-expr-p b 5))
	 (list 'if (list 'numberp b)
	       (list 'equal a b)
	       (list 'eq a b)))
	(t form)))

(define-compiler-macro member* (&whole form a list &rest keys)
  (let ((test (and (= (length keys) 2) (eq (car keys) ':test)
		   (cl-const-expr-val (nth 1 keys)))))
    (cond ((eq test 'eq) (list 'memq a list))
	  ((eq test 'equal) (list 'member a list))
	  ((or (null keys) (eq test 'eql))
	   (if (eq (cl-const-expr-p a) t)
	       (list (if (floatp-safe (cl-const-expr-val a)) 'member 'memq)
		     a list)
	     (if (eq (cl-const-expr-p list) t)
		 (let ((p (cl-const-expr-val list)) (mb nil) (mq nil))
		   (if (not (cdr p))
		       (and p (list 'eql a (list 'quote (car p))))
		     (while p
		       (if (floatp-safe (car p)) (setq mb t)
			 (or (integerp (car p)) (symbolp (car p)) (setq mq t)))
		       (setq p (cdr p)))
		     (if (not mb) (list 'memq a list)
		       (if (not mq) (list 'member a list) form))))
	       form)))
	  (t form))))

(define-compiler-macro assoc* (&whole form a list &rest keys)
  (let ((test (and (= (length keys) 2) (eq (car keys) ':test)
		   (cl-const-expr-val (nth 1 keys)))))
    (cond ((eq test 'eq) (list 'assq a list))
	  ((eq test 'equal) (list 'assoc a list))
	  ((and (eq (cl-const-expr-p a) t) (or (null keys) (eq test 'eql)))
	   (if (floatp-safe (cl-const-expr-val a))
	       (list 'assoc a list) (list 'assq a list)))
	  (t form))))

(define-compiler-macro adjoin (&whole form a list &rest keys)
  (if (and (cl-simple-expr-p a) (cl-simple-expr-p list)
	   (not (memq ':key keys)))
      (list 'if (list* 'member* a list keys) list (list 'cons a list))
    form))

(define-compiler-macro list* (arg &rest others)
  (let* ((args (reverse (cons arg others)))
	 (form (car args)))
    (while (setq args (cdr args))
      (setq form (list 'cons (car args) form)))
    form))

(define-compiler-macro get* (sym prop &optional def)
  (if def
      (list 'getf (list 'symbol-plist sym) prop def)
    (list 'get sym prop)))

(define-compiler-macro typep (&whole form val type)
  (if (cl-const-expr-p type)
      (let ((res (cl-make-type-test val (cl-const-expr-val type))))
	(if (or (memq (cl-expr-contains res val) '(nil 1))
		(cl-simple-expr-p val)) res
	  (let ((temp (gensym)))
	    (list 'let (list (list temp val)) (subst temp val res)))))
    form))


(mapcar (function
	 (lambda (y)
	   (put (car y) 'side-effect-free t)
	   (put (car y) 'byte-compile 'cl-byte-compile-compiler-macro)
	   (put (car y) 'cl-compiler-macro
		(list 'lambda '(w x)
		      (if (symbolp (cadr y))
			  (list 'list (list 'quote (cadr y))
				(list 'list (list 'quote (caddr y)) 'x))
			(cons 'list (cdr y)))))))
	'((first 'car x) (second 'cadr x) (third 'caddr x) (fourth 'cadddr x)
	  (fifth 'nth 4 x) (sixth 'nth 5 x) (seventh 'nth 6 x)
	  (eighth 'nth 7 x) (ninth 'nth 8 x) (tenth 'nth 9 x)
	  (rest 'cdr x) (endp 'null x) (plusp '> x 0) (minusp '< x 0)
	  (caar car car) (cadr car cdr) (cdar cdr car) (cddr cdr cdr)
	  (caaar car caar) (caadr car cadr) (cadar car cdar)
	  (caddr car cddr) (cdaar cdr caar) (cdadr cdr cadr)
	  (cddar cdr cdar) (cdddr cdr cddr) (caaaar car caaar)
	  (caaadr car caadr) (caadar car cadar) (caaddr car caddr)
	  (cadaar car cdaar) (cadadr car cdadr) (caddar car cddar)
	  (cadddr car cdddr) (cdaaar cdr caaar) (cdaadr cdr caadr)
	  (cdadar cdr cadar) (cdaddr cdr caddr) (cddaar cdr cdaar)
	  (cddadr cdr cdadr) (cdddar cdr cddar) (cddddr cdr cdddr) ))

;;; Things that are inline.
(proclaim '(inline floatp-safe acons map concatenate notany notevery
		   cl-set-elt revappend nreconc gethash))

;;; Things that are side-effect-free.
(mapcar (function (lambda (x) (put x 'side-effect-free t)))
	'(oddp evenp abs expt signum last butlast ldiff pairlis gcd lcm
	  isqrt floor* ceiling* truncate* round* mod* rem* subseq
	  list-length get* getf gethash hash-table-count))

;;; Things that are side-effect-and-error-free.
(mapcar (function (lambda (x) (put x 'side-effect-free 'error-free)))
	'(eql floatp-safe list* subst acons equalp random-state-p
	  copy-tree sublis hash-table-p))


(run-hooks 'cl-macs-load-hook)

;;; cl-macs.el ends here
