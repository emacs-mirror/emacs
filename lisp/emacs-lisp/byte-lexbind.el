;;; byte-lexbind.el --- Lexical binding support for byte-compiler
;;
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: lisp

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;; Code:

(require 'bytecomp)

;; Downward closures aren't implemented yet, so this should always be nil
(defconst byte-compile-use-downward-closures nil
  "If true, use `downward closures', which are closures that don't cons.")

(defconst byte-compile-save-window-excursion-uses-eval t
  "If true, the bytecode for `save-window-excursion' uses eval.
This means that the body of the form must be put into a closure.")

(defun byte-compile-arglist-vars (arglist)
  "Return a list of the variables in the lambda argument list ARGLIST."
  (remq '&rest (remq '&optional arglist)))


;; Variable extent analysis

;; A `lforminfo' holds information about lexical bindings in a form, and
;; some other info:
;; LFORMINFO : ((LVARINFO...) . NUM-CLOSURES)
;;   NUM-CLOSURES is the number of closures found
;;
;; A `lvarinfo' holds information about a single lexical variable:
;; LVARINFO : ((VAR NUMREFS NUMSETS CLOSED-OVER) ...)
;;   NUMREFS is the number of uses
;;   NUMSETS is the number of sets
;;   CLOSED-OVER is set to non-nil if the variable is referenced
;;     anywhere but in its original function-level

;; lvarinfo:
;; constructor
(defsubst byte-compile-make-lvarinfo (var &optional already-set)
  `(,var 0 ,(if already-set 1 0) 0 nil))
;; accessors
(defsubst byte-compile-lvarinfo-var (vinfo) (car vinfo))
(defsubst byte-compile-lvarinfo-numrefs (vinfo) (cadr vinfo))
(defsubst byte-compile-lvarinfo-numsets (vinfo) (nth 3 vinfo))
(defsubst byte-compile-lvarinfo-closed-over-p (vinfo) (nth 4 vinfo))
;; setters
(defsubst byte-compile-lvarinfo-note-ref (vinfo)
  (setcar (cdr vinfo) (1+ (cadr vinfo))))
(defsubst byte-compile-lvarinfo-note-set (vinfo)
  (setcar (cddr vinfo) (1+ (nth 3 vinfo))))
(defsubst byte-compile-lvarinfo-note-closure (vinfo)
  (setcar (nthcdr 4 vinfo) t))

;; lforminfo:
;; constructor
(defsubst byte-compile-make-lforminfo ()
  (cons nil 0))
;; accessors
(defalias 'byte-compile-lforminfo-vars 'car)
(defalias 'byte-compile-lforminfo-num-closures 'cdr)
;; setters
(defsubst byte-compile-lforminfo-add-var (finfo var &optional already-set)
  (setcar finfo (cons (byte-compile-make-lvarinfo var already-set)
		      (car finfo))))

(defun byte-compile-lforminfo-make-closure-flag ()
  "Return a new `closure-flag'."
  (cons nil nil))

(defsubst byte-compile-lforminfo-note-closure (lforminfo lvarinfo closure-flag)
  "If a variable reference or definition is inside a closure, record that fact.
LFORMINFO describes the form currently being analyzed, and LVARINFO
describes the variable.  CLOSURE-FLAG is either nil, if currently _not_
inside a closure, and otherwise a `closure flag' returned by
`byte-compile-lforminfo-make-closure-flag'."
  (when closure-flag
    (byte-compile-lvarinfo-note-closure lvarinfo)
    (unless (car closure-flag)
      (setcdr lforminfo (1+ (cdr lforminfo)))
      (setcar closure-flag t))))

(defun byte-compile-compute-lforminfo (form &optional special)
  "Return information about variables lexically bound by FORM.
SPECIAL is a list of variables that are special, and so shouldn't be
bound lexically (in addition to variable that are considered special
because they are declared with `defvar', et al).

The result is an alist of variables, with each element being of the form
  (VAR NUMREFS NUMSETS CLOSED-OVER)
where:
  NUMREFS is the number of times VAR is used
  NUMSETS is the number of times it is set
  CLOSED-OVER is non-nil if VAR is referenced by an embedded closure."
  (and
   (consp form)
   (let ((lforminfo (byte-compile-make-lforminfo)))
     (cond ((eq (car form) 'let)
	    ;; Find the bound variables
	    (dolist (clause (cadr form))
	      (let ((var (if (consp clause) (car clause) clause)))
		(unless (memq var special)
		  (byte-compile-lforminfo-add-var lforminfo var t))))
	    ;; Analyze the body
	    (unless (null (byte-compile-lforminfo-vars lforminfo))
	      (byte-compile-lforminfo-analyze-forms lforminfo form 2
						    special nil)))
	   ((eq (car form) 'let*)
	    (dolist (clause (cadr form))
	      (let ((var (if (consp clause) (car clause) clause)))
		;; Analyze each initializer based on the previously
		;; bound variables.
		(when (and (consp clause) lforminfo)
		  (byte-compile-lforminfo-analyze lforminfo (cadr clause)
						  special nil))
		(unless (memq var special)
		  (byte-compile-lforminfo-add-var lforminfo var t))))
	    ;; Analyze the body
	    (unless (null (byte-compile-lforminfo-vars lforminfo))
	      (byte-compile-lforminfo-analyze-forms lforminfo form 2
						    special nil)))
	   ((eq (car form) 'condition-case)
	    ;; `condition-case' currently must dynamically bind the
	    ;; error variable, so do nothing.
	    )
	   ((memq (car form) '(defun defmacro))
	    (byte-compile-lforminfo-from-lambda lforminfo (cdr form) special))
	   ((eq (car form) 'lambda)
	    (byte-compile-lforminfo-from-lambda lforminfo form special))
	   ((and (consp (car form)) (eq (caar form) 'lambda))
	    ;; An embedded lambda, which is basically just a `let'
	    (byte-compile-lforminfo-from-lambda lforminfo (cdr form) special)))
     (if (byte-compile-lforminfo-vars lforminfo)
	 lforminfo
       nil))))

(defun byte-compile-lforminfo-from-lambda (lforminfo lambda special)
  "Initialize LFORMINFO from the lambda expression LAMBDA.
SPECIAL is a list of variables to ignore.
The first element of LAMBDA is ignored; it need not actually be `lambda'."
  ;; Add the arguments
  (dolist (arg (byte-compile-arglist-vars (cadr lambda)))
    (byte-compile-lforminfo-add-var lforminfo arg t))
  ;; Analyze the body
  (unless (null (byte-compile-lforminfo-vars lforminfo))
    (byte-compile-lforminfo-analyze-forms lforminfo lambda 2 special nil)))

(defun byte-compile-lforminfo-analyze (lforminfo form &optional ignore closure-flag)
  "Update variable information in LFORMINFO by analyzing FORM.
IGNORE is a list of variables that shouldn't be analyzed (usually because
they're special, or because some inner binding shadows the version in
LFORMINFO).  CLOSURE-FLAG should be either nil or a `closure flag' created
with `byte-compile-lforminfo-make-closure-flag'; the latter indicates that
FORM is inside a lambda expression that may close over some variable in
LFORMINFO."
  (cond ((symbolp form)
	 ;; variable reference
	 (unless (member form ignore)
	   (let ((vinfo (assq form (byte-compile-lforminfo-vars lforminfo))))
	     (when vinfo
	       (byte-compile-lvarinfo-note-ref vinfo)
	       (byte-compile-lforminfo-note-closure lforminfo vinfo
						    closure-flag)))))
	;; function call/special form
	((consp form)
	 (let ((fun (car form)))
	   (cond
	    ((eq fun 'setq)
	     (pop form)
	     (while form
	       (let ((var (pop form)))
		 (byte-compile-lforminfo-analyze lforminfo (pop form)
						 ignore closure-flag)
		 (unless (member var ignore)
		   (let ((vinfo
			  (assq var (byte-compile-lforminfo-vars lforminfo))))
		     (byte-compile-lvarinfo-note-set vinfo)
		     (byte-compile-lforminfo-note-closure lforminfo vinfo
							  closure-flag))))))
	    ((eq fun 'catch)
	     ;; tag
	     (byte-compile-lforminfo-analyze lforminfo (cadr form)
					     ignore closure-flag)
	     ;; `catch' uses a closure for the body
	     (byte-compile-lforminfo-analyze-forms
	      lforminfo form 2
	      ignore
	      (or closure-flag
		  (and (not byte-compile-use-downward-closures)
		       (byte-compile-lforminfo-make-closure-flag)))))
	    ((eq fun 'cond)
	     (byte-compile-lforminfo-analyze-clauses lforminfo (cdr form) 0
						     ignore closure-flag))
	    ((eq fun 'condition-case)
	     ;; `condition-case' separates its body/handlers into
	     ;; separate closures.
	     (unless (or closure-flag byte-compile-use-downward-closures)
	       ;; condition case is implemented by calling a function
	       (setq closure-flag (byte-compile-lforminfo-make-closure-flag)))
	     ;; value form
	     (byte-compile-lforminfo-analyze lforminfo (nth 2 form)
					     ignore closure-flag)
	     ;; the error variable is always bound dynamically (because
	     ;; of the implementation)
	     (when (cadr form)
	       (push (cadr form) ignore))
	     ;; handlers
	     (byte-compile-lforminfo-analyze-clauses lforminfo
						     (nthcdr 2 form) 1
						     ignore closure-flag))
	    ((eq fun '(defvar defconst))
	     (byte-compile-lforminfo-analyze lforminfo (nth 2 form)
					     ignore closure-flag))
	    ((memq fun '(defun defmacro))
	     (byte-compile-lforminfo-analyze-forms lforminfo form 3
						   ignore closure-flag))
	    ((eq fun 'function)
	     (when (and (consp (cadr form)) (eq (car (cadr form)) 'lambda))
	       (setq ignore
		     (nconc (byte-compile-arglist-vars (cadr (cadr form)))
			    ignore))
	       (byte-compile-lforminfo-analyze-forms
		lforminfo (cadr form) 2
		ignore
		(or closure-flag
		    (byte-compile-lforminfo-make-closure-flag)))))
	    ((eq fun 'let)
	     ;; analyze variable inits
	     (byte-compile-lforminfo-analyze-clauses lforminfo (cadr form) 1
						     ignore closure-flag)
	     ;; shadow bound variables
	     (dolist (clause (cadr form))
	       (push (if (symbolp clause) clause (car clause))
		     ignore))
	     ;; analyze body
	     (byte-compile-lforminfo-analyze-forms lforminfo form 2
						   ignore closure-flag))
	    ((eq fun 'let*)
	     (dolist (clause (cadr form))
	       (if (symbolp clause)
		   (push clause ignore)
		 (byte-compile-lforminfo-analyze lforminfo (cadr clause)
						 ignore closure-flag)
		 (push (car clause) ignore)))
	     ;; analyze body
	     (byte-compile-lforminfo-analyze-forms lforminfo form 2
						   ignore closure-flag))
	    ((eq fun 'quote)
	     ;; do nothing
	     )
	    ((eq fun 'save-window-excursion)
	     ;; `save-window-excursion' currently uses a funny implementation
	     ;; that requires its body forms be put into a closure (it should
	     ;; be fixed to work more like `save-excursion' etc., do).
	     (byte-compile-lforminfo-analyze-forms
	      lforminfo form 2
	      ignore
	      (or closure-flag
		  (and byte-compile-save-window-excursion-uses-eval
		       (not byte-compile-use-downward-closures)
		       (byte-compile-lforminfo-make-closure-flag)))))
	    ((and (consp fun) (eq (car fun) 'lambda))
	     ;; embedded lambda.  These are inlined by the compiler, so
	     ;; we don't treat them like a real closure, more like `let'.
	     (byte-compile-lforminfo-analyze-forms lforminfo form 2
						   ignore closure-flag)
	     (setq ignore (nconc (byte-compile-arglist-vars (cadr fun))
				 ignore))
	     (byte-compile-lforminfo-analyze-forms lforminfo fun 2
						   ignore closure-flag))
	    (t
	     ;; For everything else, we just expand each argument (for
	     ;; setq/setq-default this works alright because the
	     ;; variable names are symbols).
	     (byte-compile-lforminfo-analyze-forms lforminfo form 1
						   ignore closure-flag)))))))

(defun byte-compile-lforminfo-analyze-forms
  (lforminfo forms skip ignore closure-flag)
  "Update variable information in LFORMINFO by analyzing each form in FORMS.
The first SKIP elements of FORMS are skipped without analysis.  IGNORE
is a list of variables that shouldn't be analyzed (usually because
they're special, or because some inner binding shadows the version in
LFORMINFO).  CLOSURE-FLAG should be either nil or a `closure flag' created with
`byte-compile-lforminfo-make-closure-flag'; the latter indicates that FORM is
inside a lambda expression that may close over some variable in LFORMINFO."
  (when skip
    (setq forms (nthcdr skip forms)))
  (while forms
    (byte-compile-lforminfo-analyze lforminfo (pop forms)
				    ignore closure-flag)))

(defun byte-compile-lforminfo-analyze-clauses
  (lforminfo clauses skip ignore closure-flag)
  "Update variable information in LFORMINFO by analyzing each clause in CLAUSES.
Each clause is a list of forms; any clause that's not a list is ignored.  The
first SKIP elements of each clause are skipped without analysis.  IGNORE is a
list of variables that shouldn't be analyzed (usually because they're special,
or because some inner binding shadows the version in LFORMINFO).
CLOSURE-FLAG should be either nil or a `closure flag' created with
`byte-compile-lforminfo-make-closure-flag'; the latter indicates that FORM is
inside a lambda expression that may close over some variable in LFORMINFO."
  (while clauses
    (let ((clause (pop clauses)))
      (when (consp clause)
	(byte-compile-lforminfo-analyze-forms lforminfo clause skip
					      ignore closure-flag)))))


;; Lexical environments

;; A lexical environment is an alist, where each element is of the form
;; (VAR . (OFFSET . ENV)) where VAR is either a symbol, for normal
;; variables, or an `heapenv' descriptor for references to heap environment
;; vectors.  ENV is either an atom, meaning a `stack allocated' variable
;; (the particular atom serves to indicate the particular function context
;; on whose stack it's allocated), or an `heapenv' descriptor (see above),
;; meaning a variable allocated in a heap environment vector.  For the
;; later case, an anonymous `variable' holding a pointer to the environment
;; vector may be located by recursively looking up ENV in the environment
;; as if it were a variable (so the entry for that `variable' will have a
;; non-symbol VAR).

;; We call a lexical environment a `lexenv', and an entry in it a `lexvar'.

;; constructor
(defsubst byte-compile-make-lexvar (name offset &optional env)
  (cons name (cons offset env)))
;; accessors
(defsubst byte-compile-lexvar-name (lexvar) (car lexvar))
(defsubst byte-compile-lexvar-offset (lexvar) (cadr lexvar))
(defsubst byte-compile-lexvar-environment (lexvar) (cddr lexvar))
(defsubst byte-compile-lexvar-variable-p (lexvar) (symbolp (car lexvar)))
(defsubst byte-compile-lexvar-environment-p (lexvar)
  (not (symbolp (car lexvar))))
(defsubst byte-compile-lexvar-on-stack-p (lexvar)
  (atom (byte-compile-lexvar-environment lexvar)))
(defsubst byte-compile-lexvar-in-heap-p (lexvar)
  (not (byte-compile-lexvar-on-stack-p lexvar)))

(defun byte-compile-make-lambda-lexenv (form closed-over-lexenv)
  "Make a new lexical environment for a lambda expression FORM.
CLOSED-OVER-LEXENV is the lexical environment in which FORM occurs."
  ;; See if this is a closure or not
  (let ((closure nil)
	(lforminfo (byte-compile-make-lforminfo))
	(args (byte-compile-arglist-vars (cadr form))))
    (dolist (lexvar closed-over-lexenv)
      (when (and (byte-compile-lexvar-in-heap-p lexvar)
		 (not (memq (car lexvar) args)))
	;; The variable is located in a heap-allocated environment
	;; vector, so FORM may use it.  Add it to the set of variables
	;; that we'll search for in FORM.
	(byte-compile-lforminfo-add-var lforminfo (car lexvar))))
    ;; See how FORM uses these potentially closed-over variables.
    (byte-compile-lforminfo-analyze lforminfo form args)
    (let ((lexenv nil))
      (dolist (vinfo (byte-compile-lforminfo-vars lforminfo))
	(when (> (byte-compile-lvarinfo-numrefs vinfo) 0)
	  ;; FORM uses VINFO's variable, so it must be a closure.
	  (setq closure t)
	  ;; Make sure that the environment in which the variable is
	  ;; located is accessible (since we only ever pass the
	  ;; innermost environment to closures, if it's in some other
	  ;; envionment, there must be path to it from the innermost
	  ;; one).
	  (byte-compile-heapenv-ensure-access
	   byte-compile-current-heap-environment
	   (byte-compile-lexvar-environment
	    (assq (car vinfo) closed-over-lexenv)))
	  ;; Put this variable in the new lexical environment
	  (push vinfo lexenv)))
      ;; Fill in the initial stack contents
      (let ((stackpos 0))
	(when closure
	  ;; Add the magic first argument that holds the environment pointer
	  (push (byte-compile-make-lexvar byte-compile-current-heap-environment
					  0)
		lexenv)
	  (setq stackpos (1+ stackpos)))
	;; Add entries for each argument
	(dolist (arg args)
	  (push (byte-compile-make-lexvar arg stackpos) lexenv)
	  (setq stackpos (1+ stackpos)))
	;; Return the new lexical environment
	lexenv))))

(defun byte-compile-closure-lexenv-p (lexenv)
  "Return non-nil if LEXENV is the initial lexical environment for a closure.
This only works correctly when passed a new lexical environment as returned
by `byte-compile-make-lambda-lexenv'."
  (let ((closure nil))
    (while (and lexenv (not closure))
      (when (byte-compile-lexvar-environment-p (pop lexenv))
	(setq closure t)))
    closure))


;; Heap environment vectors

;; A `heap environment vector' is heap-allocated vector used to store
;; variable that can't be put onto the stack.
;;
;; They are represented in the compiler by a list of the form
;;    (SIZE SIZE-CONST-ID INIT-POSITION . ENVS)
;; SIZE is the current size of the vector (which may be
;; incremented if another variable or environment-reference is added to
;; the end).  SIZE-CONST-ID is an `unknown constant id' (as returned by
;; `byte-compile-push-unknown-constant') representing the constant used
;; in the vector initialization code, and INIT-POSITION is a position
;; in the byte-code output (as returned by `byte-compile-delay-out')
;; at which more initialization code can be added.
;; ENVS is a list of other environment vectors accessible form this one,
;; where each element is of the form (ENV . OFFSET).

;; constructor
(defsubst byte-compile-make-heapenv (size-const-id init-position)
  (list 0 size-const-id init-position))
;; accessors
(defsubst byte-compile-heapenv-size (heapenv) (car heapenv))
(defsubst byte-compile-heapenv-size-const-id (heapenv) (cadr heapenv))
(defsubst byte-compile-heapenv-init-position (heapenv) (nth 2 heapenv))
(defsubst byte-compile-heapenv-accessible-envs (heapenv) (nthcdr 3 heapenv))

(defun byte-compile-heapenv-add-slot (heapenv)
  "Add a slot to the heap environment HEAPENV and return its offset."
  (prog1 (car heapenv) (setcar heapenv (1+ (car heapenv)))))

(defun byte-compile-heapenv-add-accessible-env (heapenv env offset)
  "Add to HEAPENV's list of accessible environments, ENV at OFFSET."
  (setcdr (nthcdr 2 heapenv)
	  (cons (cons env offset)
		(byte-compile-heapenv-accessible-envs heapenv))))

(defun byte-compile-push-heapenv ()
  "Generate byte-code to push a new heap environment vector.
Sets `byte-compile-current-heap-environment' to the compiler descriptor
for the new heap environment.
Return a `lexvar' descriptor for the new heap environment."
  (let ((env-stack-pos byte-compile-depth)
	size-const-id init-position)
    ;; Generate code to push the vector
    (byte-compile-push-constant 'make-vector)
    (setq size-const-id (byte-compile-push-unknown-constant))
    (byte-compile-push-constant nil)
    (byte-compile-out 'byte-call 2)
    (setq init-position (byte-compile-delay-out 3))
    ;; Now make a heap-environment for the compiler to use
    (setq byte-compile-current-heap-environment
	  (byte-compile-make-heapenv size-const-id init-position))
    (byte-compile-make-lexvar byte-compile-current-heap-environment
			      env-stack-pos)))

(defun byte-compile-heapenv-ensure-access (heapenv other-heapenv)
  "Make sure that HEAPENV can be used to access OTHER-HEAPENV.
If not, then add a new slot to HEAPENV pointing to OTHER-HEAPENV."
  (unless (memq heapenv (byte-compile-heapenv-accessible-envs heapenv))
    (let ((offset (byte-compile-heapenv-add-slot heapenv)))
      (byte-compile-heapenv-add-accessible-env heapenv other-heapenv offset))))


;; Variable binding/unbinding

(defun byte-compile-non-stack-bindings-p (clauses lforminfo)
  "Return non-nil if any lexical bindings in CLAUSES are not stack-allocated.
LFORMINFO should be information about lexical variables being bound."
  (or (not (= (length clauses) (length lforminfo)))
      (progn
	(while (and lforminfo clauses)
	  (when (byte-compile-lvarinfo-closed-over-p (pop lforminfo))
	    (setq clauses nil)))
	(not clauses))))

(defun byte-compile-let-clauses-trivial-init-p (clauses)
  "Return true if let binding CLAUSES all have a `trivial' init value.
Trivial means either a constant value, or a simple variable initialization."
  (or (null clauses)
      (and (or (atom (car clauses))
	       (atom (cadr (car clauses)))
	       (eq (car (cadr (car clauses))) 'quote))
	   (byte-compile-let-clauses-trivial-init-p (cdr clauses)))))

(defun byte-compile-rearrange-let-clauses (clauses lforminfo)
  "Return CLAUSES rearranged so non-stack variables come last if possible.
Care is taken to only do so when it's clear that the meaning is the same.
LFORMINFO should be information about lexical variables being bound."
  ;; We currently do a very simple job by only exchanging clauses when
  ;; one has a constant init, or one has a variable init and the other
  ;; doesn't have a function call init (because that could change the
  ;; value of the variable).  This could be more clever and actually
  ;; attempt to analyze which variables could possible be changed, etc.
  (let ((unchanged nil)
	(lex-non-stack nil)
	(dynamic nil))
    (while clauses
      (let* ((clause (pop clauses))
	     (var (if (consp clause) (car clause) clause))
	     (init (and (consp clause) (cadr clause)))
	     (vinfo (assq var (byte-compile-lforminfo-vars lforminfo))))
	(cond
	 ((or (and vinfo
		   (not (byte-compile-lvarinfo-closed-over-p vinfo)))
	      (not
	       (or (eq init nil) (eq init t)
		   (and (atom init) (not (symbolp init)))
		   (and (consp init) (eq (car init) 'quote))
		   (byte-compile-let-clauses-trivial-init-p clauses))))
	  (push clause unchanged))
	 (vinfo
	  (push clause lex-non-stack))
	 (t
	  (push clause dynamic)))))
    (nconc (nreverse unchanged) (nreverse lex-non-stack) (nreverse dynamic))))

(defun byte-compile-maybe-push-heap-environment (&optional lforminfo)
  "Push a new heap environment if necessary.
LFORMINFO should be information about lexical variables being bound.
Return a lexical environment containing only the heap vector.
Also, `byte-compile-current-heap-environment' and
`byte-compile-current-num-closures' are updated to reflect any change (so they
should probably be bound by the caller to ensure that the new values have the
proper scope)."
  ;; We decide whether a new heap environment is required by seeing if
  ;; the number of closures inside the form described by LFORMINFO is
  ;; the same as the number inside the binding form that created the
  ;; currently active heap environment.
  (let ((nclosures
	 (if lforminfo (byte-compile-lforminfo-num-closures lforminfo) -1)))
    (if (and byte-compile-current-heap-environment
	     (or (zerop nclosures)
		 (= nclosures byte-compile-current-num-closures)))
	;; No need to push a heap environment.
	nil
      ;; Have to push one.  A heap environment is really just a vector, so
      ;; we emit bytecodes to create a vector.  However, the size is not
      ;; fixed yet (the vector can grow if subforms use it to store
      ;; values, and if `access points' to parent heap environments are
      ;; added), so we use `byte-compile-push-unknown-constant' to push the
      ;; vector size.
      (setq byte-compile-current-num-closures nclosures)
      (list (byte-compile-push-heap-environment)))))

(defun byte-compile-bind (var init-lexenv &optional lforminfo)
  "Emit byte-codes to bind VAR.
INIT-LEXENV should be a lexical-environment alist describing the
positions of the init value that have been pushed on the stack, and
LFORMINFO should be information about lexical variables being bound.
Return non-nil if the TOS value was popped."
  ;; The presence of lexical bindings mean that we may have to
  ;; juggle things on the stack, either to move them to TOS for
  ;; dynamic binding, or to put them in a non-stack environment
  ;; vector.
  (let ((vinfo (assq var (byte-compile-lforminfo-vars lforminfo))))
    (unless (and vinfo (zerop (byte-compile-lvarinfo-numrefs vinfo)))
      (cond ((and (null vinfo) (eq var (caar init-lexenv)))
	     ;; VAR is dynamic and is on the top of the
	     ;; stack, so we can just bind it like usual
	     (byte-compile-variable-ref 'byte-varbind var)
	     t)
	    ((null vinfo)
	     ;; VAR is dynamic, but we have to get its
	     ;; value out of the middle of the stack
	     (let ((stack-pos (cdr (assq var init-lexenv))))
	       (byte-compile-out 'byte-stack-ref stack-pos)
	       (byte-compile-variable-ref 'byte-varbind var)
	       ;; Now we have to store nil into its temporary
	       ;; stack position to avoid problems with GC
	       (byte-compile-push-constant nil)
	       (byte-compile-out 'byte-stack-set stack-pos))
	     nil)
	    ((byte-compile-lvarinfo-closed-over-p vinfo)
	     ;; VAR is lexical, but needs to be in a
	     ;; heap-allocated environment.
	     (unless byte-compile-current-heap-environment
	       (error "No current heap-environment to allocate `%s' in!" var))
	     (let ((init-stack-pos
		    ;; nil if the init value is on the top of the stack,
		    ;; otherwise the position of the init value on the stack.
		    (and (not (eq var (caar init-lexenv)))
			 (byte-compile-lexvar-offset (assq var init-lexenv))))
		   (env-vec-pos
		    ;; Position of VAR in the environment vector
		    (byte-compile-lexvar-offset
		     (assq var byte-compile-lexical-environment)))
		   (env-vec-stack-pos
		    ;; Position of the the environment vector on the stack
		    ;; (the heap-environment must _always_ be available on
		    ;; the stack!)
		    (byte-compile-lexvar-offset
		     (assq byte-compile-current-heap-environment
			   byte-compile-lexical-environment))))
	       (unless env-vec-stack-pos
		 (error "Couldn't find location of current heap environment!"))
	       (if init-stack-pos
		   ;; VAR is not on the top of the stack, so get it
		   (byte-compile-out 'byte-stack-ref init-stack-pos)
		 ;; Record that we've popped VAR's init value
		 (pop init-lexenv))
	       (byte-compile-out 'byte-stack-ref env-vec-stack-pos)
	       ;; Store the variable into the vector
	       (byte-compile-out 'byte-vec-set env-vec-pos)
	       (when init-stack-pos
		 ;; Store nil into VAR's temporary stack
		 ;; position to avoid problems with GC
		 (byte-compile-push-constant nil)
		 (byte-compile-out 'byte-stack-set init-stack-pos))
	       ;; Push a record of VAR's new lexical binding
	       (push (byte-compile-make-lexvar
		      var env-vec-pos byte-compile-current-heap-environment)
		     byte-compile-lexical-environment)
	       (not init-stack-pos)))
	    (t
	     ;; VAR is a simple stack-allocated lexical variable
	     (push (assq var init-lexenv)
		   byte-compile-lexical-environment)
	     nil)))))

(defun byte-compile-unbind (clauses init-lexenv
				    &optional lforminfo preserve-body-value)
  "Emit byte-codes to unbind the variables bound by CLAUSES.
CLAUSES is a `let'-style variable binding list.  INIT-LEXENV should be a
lexical-environment alist describing the positions of the init value that
have been pushed on the stack, and LFORMINFO should be information about
the lexical variables that were bound.  If PRESERVE-BODY-VALUE is true,
then an additional value on the top of the stack, above any lexical binding
slots, is preserved, so it will be on the top of the stack after all
binding slots have been popped."
  ;; Unbind dynamic variables
  (let ((num-dynamic-bindings 0))
    (dolist (clause clauses)
      (unless (assq (if (consp clause) (car clause) clause)
		    (byte-compile-lforminfo-vars lforminfo))
	(setq num-dynamic-bindings (1+ num-dynamic-bindings))))
    (unless (zerop num-dynamic-bindings)
      (byte-compile-out 'byte-unbind num-dynamic-bindings)))
  ;; Pop lexical variables off the stack, possibly preserving the
  ;; return value of the body.
  (when init-lexenv
    ;; INIT-LEXENV contains all init values left on the stack
    (when preserve-body-value
      ;; Preserve the return value of the body, which is now on the
      ;; top of the stack, by storing it directly into the stack
      ;; position which will be at TOS after we pop.
      (byte-compile-out 'byte-stack-set (cdr (last init-lexenv)))
      (pop init-lexenv))
    (while init-lexenv
      (byte-compile-discard)
      (pop init-lexenv))))


(provide 'byte-lexbind)

;;; byte-lexbind.el ends here
