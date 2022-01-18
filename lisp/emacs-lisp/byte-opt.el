;;; byte-opt.el --- the optimization passes of the emacs-lisp byte compiler -*- lexical-binding: t -*-

;; Copyright (C) 1991, 1994, 2000-2022 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;;	Hallvard Furuseth <hbf@ulrik.uio.no>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
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

;; ========================================================================
;; "No matter how hard you try, you can't make a racehorse out of a pig.
;; You can, however, make a faster pig."
;;
;; Or, to put it another way, the Emacs byte compiler is a VW Bug.  This code
;; makes it be a VW Bug with fuel injection and a turbocharger...  You're
;; still not going to make it go faster than 70 mph, but it might be easier
;; to get it there.
;;

;; TO DO:
;;
;; (apply (lambda (x &rest y) ...) 1 (foo))
;;
;; maintain a list of functions known not to access any global variables
;; (actually, give them a 'dynamically-safe property) and then
;;   (let ( v1 v2 ... vM vN ) <...dynamically-safe...> )  ==>
;;   (let ( v1 v2 ... vM ) vN <...dynamically-safe...> )
;; by recursing on this, we might be able to eliminate the entire let.
;; However certain variables should never have their bindings optimized
;; away, because they affect everything.
;;   (put 'debug-on-error 'binding-is-magic t)
;;   (put 'debug-on-abort 'binding-is-magic t)
;;   (put 'debug-on-next-call 'binding-is-magic t)
;;   (put 'inhibit-quit 'binding-is-magic t)
;;   (put 'quit-flag 'binding-is-magic t)
;;   (put 't 'binding-is-magic t)
;;   (put 'nil 'binding-is-magic t)
;; possibly also
;;   (put 'gc-cons-threshold 'binding-is-magic t)
;;   (put 'track-mouse 'binding-is-magic t)
;; others?
;;
;; Simple defsubsts often produce forms like
;;    (let ((v1 (f1)) (v2 (f2)) ...)
;;       (FN v1 v2 ...))
;; It would be nice if we could optimize this to
;;    (FN (f1) (f2) ...)
;; but we can't unless FN is dynamically-safe (it might be dynamically
;; referring to the bindings that the lambda arglist established.)
;; One of the uncountable lossages introduced by dynamic scope...
;;
;; Maybe there should be a control-structure that says "turn on
;; fast-and-loose type-assumptive optimizations here."  Then when
;; we see a form like (car foo) we can from then on assume that
;; the variable foo is of type cons, and optimize based on that.
;; But, this won't win much because of (you guessed it) dynamic
;; scope.  Anything down the stack could change the value.
;; (Another reason it doesn't work is that it is perfectly valid
;; to call car with a null argument.)  A better approach might
;; be to allow type-specification of the form
;;   (put 'foo 'arg-types '(float (list integer) dynamic))
;;   (put 'foo 'result-type 'bool)
;; It should be possible to have these types checked to a certain
;; degree.
;;
;; collapse common subexpressions
;;
;; It would be nice if redundant sequences could be factored out as well,
;; when they are known to have no side-effects:
;;   (list (+ a b c) (+ a b c))   -->  a b add c add dup list-2
;; but beware of traps like
;;   (cons (list x y) (list x y))
;;
;; Tail-recursion elimination is not really possible in Emacs Lisp.
;; Tail-recursion elimination is almost always impossible when all variables
;; have dynamic scope, but given that the "return" byteop requires the
;; binding stack to be empty (rather than emptying it itself), there can be
;; no truly tail-recursive Emacs Lisp functions that take any arguments or
;; make any bindings.
;;
;; Here is an example of an Emacs Lisp function which could safely be
;; byte-compiled tail-recursively:
;;
;;  (defun tail-map (fn list)
;;    (cond (list
;;           (funcall fn (car list))
;;           (tail-map fn (cdr list)))))
;;
;; However, if there was even a single let-binding around the COND,
;; it could not be byte-compiled, because there would be an "unbind"
;; byte-op between the final "call" and "return."  Adding a
;; Bunbind_all byteop would fix this.
;;
;;   (defun foo (x y z) ... (foo a b c))
;;   ... (const foo) (varref a) (varref b) (varref c) (call 3) END: (return)
;;   ... (varref a) (varbind x) (varref b) (varbind y) (varref c) (varbind z) (goto 0) END: (unbind-all) (return)
;;   ... (varref a) (varset x) (varref b) (varset y) (varref c) (varset z) (goto 0) END: (return)
;;
;; this also can be considered tail recursion:
;;
;;   ... (const foo) (varref a) (call 1) (goto X) ... X: (return)
;; could generalize this by doing the optimization
;;   (goto X) ... X: (return)  -->  (return)
;;
;; But this doesn't solve all of the problems: although by doing tail-
;; recursion elimination in this way, the call-stack does not grow, the
;; binding-stack would grow with each recursive step, and would eventually
;; overflow.  I don't believe there is any way around this without lexical
;; scope.
;;
;; Wouldn't it be nice if Emacs Lisp had lexical scope.
;;
;; Idea: the form (lexical-scope) in a file means that the file may be
;; compiled lexically.  This proclamation is file-local.  Then, within
;; that file, "let" would establish lexical bindings, and "let-dynamic"
;; would do things the old way.  (Or we could use CL "declare" forms.)
;; We'd have to notice defvars and defconsts, since those variables should
;; always be dynamic, and attempting to do a lexical binding of them
;; should simply do a dynamic binding instead.
;; But!  We need to know about variables that were not necessarily defvared
;; in the file being compiled (doing a boundp check isn't good enough.)
;; Fdefvar() would have to be modified to add something to the plist.
;;
;; A major disadvantage of this scheme is that the interpreter and compiler
;; would have different semantics for files compiled with (dynamic-scope).
;; Since this would be a file-local optimization, there would be no way to
;; modify the interpreter to obey this (unless the loader was hacked
;; in some grody way, but that's a really bad idea.)

;; Other things to consider:

;; ;; Associative math should recognize subcalls to identical function:
;; (disassemble (lambda (x) (+ (+ (foo) 1) (+ (bar) 2))))
;; ;; This should generate the same as (1+ x) and (1- x)

;; (disassemble (lambda (x) (cons (+ x 1) (- x 1))))
;; ;; An awful lot of functions always return a non-nil value.  If they're
;; ;; error free also they may act as true-constants.

;; (disassemble (lambda (x) (and (point) (foo))))
;; ;; When
;; ;;   - all but one arguments to a function are constant
;; ;;   - the non-constant argument is an if-expression (cond-expression?)
;; ;; then the outer function can be distributed.  If the guarding
;; ;; condition is side-effect-free [assignment-free] then the other
;; ;; arguments may be any expressions.  Since, however, the code size
;; ;; can increase this way they should be "simple".  Compare:

;; (disassemble (lambda (x) (eq (if (point) 'a 'b) 'c)))
;; (disassemble (lambda (x) (if (point) (eq 'a 'c) (eq 'b 'c))))

;; ;; (car (cons A B)) -> (prog1 A B)
;; (disassemble (lambda (x) (car (cons (foo) 42))))

;; ;; (cdr (cons A B)) -> (progn A B)
;; (disassemble (lambda (x) (cdr (cons 42 (foo)))))

;; ;; (car (list A B ...)) -> (prog1 A B ...)
;; (disassemble (lambda (x) (car (list (foo) 42 (bar)))))

;; ;; (cdr (list A B ...)) -> (progn A (list B ...))
;; (disassemble (lambda (x) (cdr (list 42 (foo) (bar)))))


;;; Code:

(require 'bytecomp)
(eval-when-compile (require 'cl-lib))
(require 'macroexp)
(eval-when-compile (require 'subr-x))

(defun byte-compile-log-lap-1 (format &rest args)
  ;; Newer byte codes for stack-ref make the slot 0 non-nil again.
  ;; But the "old disassembler" is *really* ancient by now.
  ;; (if (aref byte-code-vector 0)
  ;;     (error "The old version of the disassembler is loaded.  Reload new-bytecomp as well"))
  (byte-compile-log-1
   (apply #'format-message format
     (let (c a)
       (mapcar (lambda (arg)
		  (if (not (consp arg))
		      (if (and (symbolp arg)
			       (string-match "^byte-" (symbol-name arg)))
			  (intern (substring (symbol-name arg) 5))
			arg)
		    (if (integerp (setq c (car arg)))
                        (error "Non-symbolic byte-op %s" c))
		    (if (eq c 'TAG)
			(setq c arg)
		      (setq a (cond ((memq c byte-goto-ops)
				     (car (cdr (cdr arg))))
				    ((memq c byte-constref-ops)
				     (car (cdr arg)))
				    (t (cdr arg))))
		      (setq c (symbol-name c))
		      (if (string-match "^byte-." c)
			  (setq c (intern (substring c 5)))))
		    (if (eq c 'constant) (setq c 'const))
		    (if (and (eq (cdr arg) 0)
			     (not (memq c '(unbind call const))))
			c
		      (format "(%s %s)" c a))))
	       args)))))

(defmacro byte-compile-log-lap (format-string &rest args)
  `(and (memq byte-optimize-log '(t byte))
	(byte-compile-log-lap-1 ,format-string ,@args)))


(defvar byte-optimize--lexvars nil
  "Lexical variables in scope, in reverse order of declaration.
Each element is on the form (NAME KEEP [VALUE]), where:
  NAME is the variable name,
  KEEP is a boolean indicating whether the binding must be retained,
  VALUE, if present, is a substitutable expression.
Earlier variables shadow later ones with the same name.")

;;; byte-compile optimizers to support inlining

(put 'inline 'byte-optimizer #'byte-optimize-inline-handler)

(defun byte-optimize-inline-handler (form)
  "byte-optimize-handler for the `inline' special-form."
  (cons 'progn
	(mapcar
	 (lambda (sexp)
	   (let ((f (car-safe sexp)))
	     (if (and (symbolp f)
		      (or (cdr (assq f byte-compile-function-environment))
			  (not (or (not (fboundp f))
				   (cdr (assq f byte-compile-macro-environment))
				   (and (consp (setq f (symbol-function f)))
					(eq (car f) 'macro))
				   (subrp f)))))
		 (byte-compile-inline-expand sexp)
	       sexp)))
	 (cdr form))))

(defun byte-compile-inline-expand (form)
  (let* ((name (car form))
         (localfn (cdr (assq name byte-compile-function-environment)))
	 (fn (or localfn (symbol-function name))))
    (when (autoloadp fn)
      (autoload-do-load fn)
      (setq fn (or (symbol-function name)
                   (cdr (assq name byte-compile-function-environment)))))
    (pcase fn
      ('nil
       (byte-compile-warn "attempt to inline `%s' before it was defined"
                          name)
       form)
      (`(autoload . ,_)
       (error "File `%s' didn't define `%s'" (nth 1 fn) name))
      ((and (pred symbolp) (guard (not (eq fn t)))) ;A function alias.
       (byte-compile-inline-expand (cons fn (cdr form))))
      ((pred byte-code-function-p)
       ;; (message "Inlining byte-code for %S!" name)
       ;; The byte-code will be really inlined in byte-compile-unfold-bcf.
       (byte-compile--check-arity-bytecode form fn)
       `(,fn ,@(cdr form)))
      ((or `(lambda . ,_) `(closure . ,_))
       ;; While byte-compile-unfold-bcf can inline dynbind byte-code into
       ;; letbind byte-code (or any other combination for that matter), we
       ;; can only inline dynbind source into dynbind source or letbind
       ;; source into letbind source.
       ;; When the function comes from another file, we byte-compile
       ;; the inlined function first, and then inline its byte-code.
       ;; This also has the advantage that the final code does not
       ;; depend on the order of compilation of ELisp files, making
       ;; the build more reproducible.
       (if (eq fn localfn)
           ;; From the same file => same mode.
           (macroexp--unfold-lambda `(,fn ,@(cdr form)))
         ;; Since we are called from inside the optimiser, we need to make
         ;; sure not to propagate lexvar values.
         (let ((byte-optimize--lexvars nil)
               ;; Silence all compilation warnings: the useful ones should
               ;; be displayed when the function's source file will be
               ;; compiled anyway, but more importantly we would otherwise
               ;; emit spurious warnings here because we don't have the full
               ;; context, such as `declare-functions' placed earlier in the
               ;; source file's code or `with-suppressed-warnings' that
               ;; surrounded the `defsubst'.
               (byte-compile-warnings nil))
           (byte-compile name))
         (let ((bc (symbol-function name)))
           (byte-compile--check-arity-bytecode form bc)
           `(,bc ,@(cdr form)))))

      (_ ;; Give up on inlining.
       form))))

;;; implementing source-level optimizers

(defvar byte-optimize--vars-outside-loop nil
  "Alist of variables lexically bound outside the innermost `while' loop.
Variables here are sensitive to mutation inside the loop, since this can
occur an indeterminate number of times and thus have effect on code
sequentially preceding the mutation itself.
Same format as `byte-optimize--lexvars', with shared structure and contents.")

(defvar byte-optimize--inhibit-outside-loop-constprop nil
  "If t, don't propagate values for variables declared outside the inner loop.
This indicates the loop discovery phase.")

(defvar byte-optimize--dynamic-vars nil
  "List of variables declared as dynamic during optimisation.")

(defvar byte-optimize--aliased-vars nil
  "List of variables which may be aliased by other lexical variables.
If an entry in `byte-optimize--lexvars' has another variable as its VALUE,
then that other variable must be in this list.
This variable thus carries no essential information but is maintained
for speeding up processing.")

(defun byte-optimize--substitutable-p (expr)
  "Whether EXPR is a constant that can be propagated."
  ;; Only consider numbers, symbols and strings to be values for substitution
  ;; purposes.  Numbers and symbols are immutable, and mutating string
  ;; literals (or results from constant-evaluated string-returning functions)
  ;; can be considered undefined.
  ;; (What about other quoted values, like conses?)
  (or (booleanp expr)
      (numberp expr)
      (stringp expr)
      (and (consp expr)
           (memq (car expr) '(quote function))
           (symbolp (cadr expr)))
      (keywordp expr)))

(defmacro byte-optimize--pcase (exp &rest cases)
  ;; When we do
  ;;
  ;;     (pcase EXP
  ;;       (`(if ,exp ,then ,else) (DO-TEST))
  ;;       (`(plus ,e2 ,e2)        (DO-ADD))
  ;;       (`(times ,e2 ,e2)       (DO-MULT))
  ;;       ...)
  ;;
  ;; we usually don't want to fall back to the default case if
  ;; the value of EXP is of a form like `(if E1 E2)' or `(plus E1)'
  ;; or `(times E1 E2 E3)', instead we either want to signal an error
  ;; that EXP has an unexpected shape, or we want to carry on as if
  ;; it had the right shape (ignore the extra data and pretend the missing
  ;; data is nil) because it should simply never happen.
  ;;
  ;; The macro below implements the second option by rewriting patterns
  ;; like `(if ,exp ,then ,else)'
  ;; to   `(if . (or `(,exp ,then ,else) pcase--dontcare))'.
  ;;
  ;; The resulting macroexpansion is also significantly cleaner/smaller/faster.
  (declare (indent 1) (debug pcase))
  `(pcase ,exp
     . ,(mapcar (lambda (case)
                  `(,(pcase (car case)
                       ((and `(,'\` (,_ . (,'\, ,_))) pat) pat)
                       (`(,'\` (,head . ,tail))
                        (list '\`
                              (cons head
                                    (list '\, `(or ,(list '\` tail) pcase--dontcare)))))
                       (pat pat))
                    . ,(cdr case)))
                cases)))

(defun byte-optimize-form-code-walker (form for-effect)
  ;;
  ;; For normal function calls, We can just mapcar the optimizer the cdr.  But
  ;; we need to have special knowledge of the syntax of the special forms
  ;; like let and defun (that's why they're special forms :-).  (Actually,
  ;; the important aspect is that they are subrs that don't evaluate all of
  ;; their args.)
  ;;
  ;; FIXME: There are a bunch of `byte-compile-warn' here which arguably
  ;; have no place in an optimizer: the corresponding tests should be
  ;; performed in `macroexpand-all', or in `cconv', or in `bytecomp'.
  (let ((fn (car-safe form)))
    (byte-optimize--pcase form
      ((pred (not consp))
       (cond
        ((and for-effect
	      (or byte-compile-delete-errors
		  (not (symbolp form))
		  (eq form t)
                  (keywordp form)))
         nil)
        ((symbolp form)
         (let ((lexvar (assq form byte-optimize--lexvars)))
           (cond
            ((not lexvar) form)
            (for-effect nil)
            ((and (cddr lexvar)         ; substitution available
                  ;; Perform substitution, except during the loop mutation
                  ;; discovery phase if the variable was bound outside the
                  ;; innermost loop.
                  (not (and byte-optimize--inhibit-outside-loop-constprop
                            (assq form byte-optimize--vars-outside-loop))))
             (caddr lexvar))
            (t form))))
        (t form)))
      (`(quote . ,v)
       (if (or (not v) (cdr v))
	   (byte-compile-warn "malformed quote form: `%s'"
			      (prin1-to-string form)))
       ;; Map (quote nil) to nil to simplify optimizer logic.
       ;; Map quoted constants to nil if for-effect (just because).
       (and (car v)
	    (not for-effect)
	    form))
      (`(,(or 'let 'let*) . ,rest)
       (cons fn (byte-optimize-let-form fn rest for-effect)))
      (`(cond . ,clauses)
       ;; FIXME: The condition in the first clause is always executed, and
       ;; clause bodies are mutually exclusive -- use this for improved
       ;; optimisation (see comment about `if' below).
       (cons fn
             (mapcar (lambda (clause)
                       (if (consp clause)
                           (cons
                            (byte-optimize-form (car clause) nil)
                            (byte-optimize-body (cdr clause) for-effect))
                         (byte-compile-warn "malformed cond form: `%s'"
                                            (prin1-to-string clause))
                         clause))
                     clauses)))
      (`(progn . ,exps)
       ;; As an extra added bonus, this simplifies (progn <x>) --> <x>.
       (if (cdr exps)
           (macroexp-progn (byte-optimize-body exps for-effect))
	 (byte-optimize-form (car exps) for-effect)))
      (`(prog1 ,exp . ,exps)
       (let ((exp-opt (byte-optimize-form exp for-effect)))
         (if exps
             (let ((exps-opt (byte-optimize-body exps t)))
               (if (macroexp-const-p exp-opt)
                   `(progn ,@exps-opt ,exp-opt)
	         `(prog1 ,exp-opt ,@exps-opt)))
	   exp-opt)))

      (`(,(or `save-excursion `save-restriction `save-current-buffer) . ,exps)
       ;; Those subrs which have an implicit progn; it's not quite good
       ;; enough to treat these like normal function calls.
       ;; This can turn (save-excursion ...) into (save-excursion) which
       ;; will be optimized away in the lap-optimize pass.
       (cons fn (byte-optimize-body exps for-effect)))

      (`(if ,test ,then . ,else)
       ;; FIXME: We are conservative here: any variable changed in the
       ;; THEN branch will be barred from substitution in the ELSE
       ;; branch, despite the branches being mutually exclusive.
       (let* ((test-opt (byte-optimize-form test nil))
              (const (macroexp-const-p test-opt))
              ;; Avoid traversing dead branches.
              (then-opt (and test-opt (byte-optimize-form then for-effect)))
              (else-opt (and (not (and test-opt const))
                             (byte-optimize-body else for-effect))))
         `(if ,test-opt ,then-opt . ,else-opt)))

      (`(,(or 'and 'or) . ,exps)
       ;; FIXME: We have to traverse the expressions in left-to-right
       ;; order (because that is the order of evaluation and variable
       ;; mutations must be found prior to their use), but doing so we miss
       ;; some optimisation opportunities:
       ;; consider (and A B) in a for-effect context, where B => nil.
       ;; Then A could be optimised in a for-effect context too.
       (let ((tail exps)
             (args nil))
         (while tail
           (push (byte-optimize-form
                  (car tail) (and for-effect (null (cdr tail))))
                 args)
           (setq tail (cdr tail)))
         (cons fn (nreverse args))))

      (`(while ,exp . ,exps)
       ;; FIXME: If the loop condition is statically nil after substitution
       ;; of surrounding variables then we can eliminate the whole loop,
       ;; even if those variables are mutated inside the loop.
       ;; We currently don't perform this important optimisation.
       (let* ((byte-optimize--vars-outside-loop byte-optimize--lexvars)
              (condition-body
               (if byte-optimize--inhibit-outside-loop-constprop
                   ;; We are already inside the discovery phase of an outer
                   ;; loop so there is no need for traversing this loop twice.
                   (cons exp exps)
                 ;; Discovery phase: run optimisation without substitution
                 ;; of variables bound outside this loop.
                 (let ((byte-optimize--inhibit-outside-loop-constprop t))
                   (cons (byte-optimize-form exp nil)
                         (byte-optimize-body exps t)))))
              ;; Optimise again, this time with constprop enabled (unless
              ;; we are in discovery of an outer loop),
              ;; as mutated variables have been marked as non-substitutable.
              (condition (byte-optimize-form (car condition-body) nil))
              (body (byte-optimize-body (cdr condition-body) t)))
         `(while ,condition . ,body)))

      (`(interactive . ,_)
       (byte-compile-warn "misplaced interactive spec: `%s'"
			  (prin1-to-string form))
       nil)

      (`(function . ,_)
       ;; This forms is compiled as constant or by breaking out
       ;; all the subexpressions and compiling them separately.
       form)

      (`(condition-case ,var ,exp . ,clauses)
       `(condition-case ,var          ;Not evaluated.
            ,(byte-optimize-form exp for-effect)
          ,@(mapcar (lambda (clause)
                      (let ((byte-optimize--lexvars
                             (and lexical-binding
                                  (if var
                                      (cons (list var t)
                                            byte-optimize--lexvars)
                                    byte-optimize--lexvars))))
                        (cons (car clause)
                              (byte-optimize-body (cdr clause) for-effect))))
                    clauses)))

      (`(unwind-protect ,exp . ,exps)
       ;; The unwinding part of an unwind-protect is compiled (and thus
       ;; optimized) as a top-level form, but run the optimizer for it here
       ;; anyway for lexical variable usage and substitution.  But the
       ;; protected part has the same for-effect status as the
       ;; unwind-protect itself.  (The unwinding part is always for effect,
       ;; but that isn't handled properly yet.)
       (let ((bodyform (byte-optimize-form exp for-effect)))
         (pcase exps
           (`(:fun-body ,f)
            `(unwind-protect ,bodyform
               :fun-body ,(byte-optimize-form f nil)))
           (_
            `(unwind-protect ,bodyform
               . ,(byte-optimize-body exps t))))))

      (`(catch ,tag . ,exps)
       `(catch ,(byte-optimize-form tag nil)
          . ,(byte-optimize-body exps for-effect)))

      ;; Needed as long as we run byte-optimize-form after cconv.
      (`(internal-make-closure . ,_)
       ;; Look up free vars and mark them to be kept, so that they
       ;; won't be optimised away.
       (dolist (var (caddr form))
         (let ((lexvar (assq var byte-optimize--lexvars)))
           (when lexvar
             (setcar (cdr lexvar) t))))
       form)

      (`((lambda . ,_) . ,_)
       (let ((newform (macroexp--unfold-lambda form)))
	 (if (eq newform form)
	     ;; Some error occurred, avoid infinite recursion.
	     form
	   (byte-optimize-form newform for-effect))))

      ;; FIXME: Strictly speaking, I think this is a bug: (closure...)
      ;; is a *value* and shouldn't appear in the car.
      (`((closure . ,_) . ,_) form)

      (`(setq . ,args)
       (let ((var-expr-list nil))
         (while args
           (unless (and (consp args)
                        (symbolp (car args)) (consp (cdr args)))
             (byte-compile-warn "malformed setq form: %S" form))
           (let* ((var (car args))
                  (expr (cadr args))
                  (lexvar (assq var byte-optimize--lexvars))
                  (value (byte-optimize-form expr nil)))
             (when lexvar
               (setcar (cdr lexvar) t)    ; Mark variable to be kept.
               (setcdr (cdr lexvar) nil)  ; Inhibit further substitution.

               (when (memq var byte-optimize--aliased-vars)
                 ;; Cancel aliasing of variables aliased to this one.
                 (dolist (v byte-optimize--lexvars)
                   (when (eq (nth 2 v) var)
                     ;; V is bound to VAR but VAR is now mutated:
                     ;; cancel aliasing.
                     (setcdr (cdr v) nil)))))

             (push var var-expr-list)
             (push value var-expr-list))
           (setq args (cddr args)))
         (cons fn (nreverse var-expr-list))))

      (`(defvar ,(and (pred symbolp) name) . ,rest)
       (let ((optimized-rest (and rest
                                  (cons (byte-optimize-form (car rest) nil)
                                        (cdr rest)))))
         (push name byte-optimize--dynamic-vars)
         `(defvar ,name . ,optimized-rest)))

      (`(,(pred byte-code-function-p) . ,exps)
       (cons fn (mapcar #'byte-optimize-form exps)))

      (`(,(pred (not symbolp)) . ,_)
       (byte-compile-warn "`%s' is a malformed function"
			  (prin1-to-string fn))
       form)

      ((guard (when for-effect
		(if-let ((tmp (get fn 'side-effect-free)))
		    (or byte-compile-delete-errors
		        (eq tmp 'error-free)
		        (progn
			  (byte-compile-warn "value returned from %s is unused"
					     (prin1-to-string form))
			  nil)))))
       (byte-compile-log "  %s called for effect; deleted" fn)
       ;; appending a nil here might not be necessary, but it can't hurt.
       (byte-optimize-form
	(cons 'progn (append (cdr form) '(nil))) t))

      (_
       ;; Otherwise, no args can be considered to be for-effect,
       ;; even if the called function is for-effect, because we
       ;; don't know anything about that function.
       (let ((form (cons fn (mapcar #'byte-optimize-form (cdr form)))))
	 (if (get fn 'pure)
	     (byte-optimize-constant-args form)
	   form))))))

(defun byte-optimize-one-form (form &optional for-effect)
  "The source-level pass of the optimizer."
  ;; Make optimiser aware of lexical arguments.
  (let ((byte-optimize--lexvars
         (mapcar (lambda (v) (list (car v) t))
                 byte-compile--lexical-environment)))
    (byte-optimize-form form for-effect)))

(defun byte-optimize-form (form &optional for-effect)
  (while
      (progn
        ;; First, optimize all sub-forms of this one.
        (setq form (byte-optimize-form-code-walker form for-effect))

        ;; If a form-specific optimiser is available, run it and start over
        ;; until a fixpoint has been reached.
        (and (consp form)
             (symbolp (car form))
             (let ((opt (function-get (car form) 'byte-optimizer)))
               (and opt
                    (let ((old form)
                          (new (funcall opt form)))
	              (byte-compile-log "  %s\t==>\t%s" old new)
                      (setq form new)
                      (not (eq new old))))))))
  form)

(defun byte-optimize--rename-var-body (var new-var body)
  "Replace VAR with NEW-VAR in BODY."
  (mapcar (lambda (form) (byte-optimize--rename-var var new-var form)) body))

(defun byte-optimize--rename-var (var new-var form)
  "Replace VAR with NEW-VAR in FORM."
  (pcase form
    ((pred symbolp) (if (eq form var) new-var form))
    (`(setq . ,args)
     (let ((new-args nil))
       (while args
         (push (byte-optimize--rename-var var new-var (car args)) new-args)
         (push (byte-optimize--rename-var var new-var (cadr args)) new-args)
         (setq args (cddr args)))
       `(setq . ,(nreverse new-args))))
    ;; In binding constructs like `let', `let*' and `condition-case' we
    ;; rename everything for simplicity, even new bindings named VAR.
    (`(,(and head (or 'let 'let*)) ,bindings . ,body)
     `(,head
       ,(mapcar (lambda (b) (byte-optimize--rename-var-body var new-var b))
                bindings)
       ,@(byte-optimize--rename-var-body var new-var body)))
    (`(condition-case ,res-var ,protected-form . ,handlers)
     `(condition-case ,(byte-optimize--rename-var var new-var res-var)
          ,(byte-optimize--rename-var var new-var protected-form)
        ,@(mapcar (lambda (h)
                    (cons (car h)
                          (byte-optimize--rename-var-body var new-var (cdr h))))
                  handlers)))
    (`(internal-make-closure ,vars ,env . ,rest)
     `(internal-make-closure
       ,vars ,(byte-optimize--rename-var-body var new-var env) . ,rest))
    (`(defvar ,name . ,rest)
     ;; NAME is not renamed here; we only care about lexical variables.
     `(defvar ,name . ,(byte-optimize--rename-var-body var new-var rest)))

    (`(cond . ,clauses)
     `(cond ,@(mapcar (lambda (c)
                        (byte-optimize--rename-var-body var new-var c))
                      clauses)))

    (`(function . ,_) form)
    (`(quote . ,_) form)
    (`(lambda . ,_) form)

    ;; Function calls and special forms not handled above.
    (`(,head . ,args)
     `(,head . ,(byte-optimize--rename-var-body var new-var args)))
    (_ form)))

(defun byte-optimize-let-form (head form for-effect)
  ;; Recursively enter the optimizer for the bindings and body
  ;; of a let or let*.  This for depth-firstness: forms that
  ;; are more deeply nested are optimized first.
  (if lexical-binding
      (let* ((byte-optimize--lexvars byte-optimize--lexvars)
             (byte-optimize--aliased-vars byte-optimize--aliased-vars)
             (new-lexvars nil)
             (new-aliased-vars nil)
             (let-vars nil)
             (body (cdr form))
             (bindings (car form)))
        (while bindings
          (let* ((binding (car bindings))
                 (name (car binding))
                 (expr (byte-optimize-form (cadr binding) nil)))
            (setq bindings (cdr bindings))
            (when (and (eq head 'let*)
                       (memq name byte-optimize--aliased-vars))
              ;; New variable shadows an aliased variable -- α-rename
              ;; it in this and all subsequent bindings.
              (let ((new-name (make-symbol (symbol-name name))))
                (setq bindings
                      (mapcar (lambda (b)
                                (list (byte-optimize--rename-var
                                       name new-name (car b))
                                      (byte-optimize--rename-var
                                       name new-name (cadr b))))
                              bindings))
                (setq body (byte-optimize--rename-var-body name new-name body))
                (setq name new-name)))
            (let* ((aliased nil)
                   (value (and
                           (or (byte-optimize--substitutable-p expr)
                               ;; Aliasing another lexvar.
                               (setq aliased
                                     (and (symbolp expr)
                                          (assq expr byte-optimize--lexvars))))
                           (list expr)))
                   (lexical (not (or (special-variable-p name)
                                     (memq name byte-compile-bound-variables)
                                     (memq name byte-optimize--dynamic-vars))))
                   (lexinfo (and lexical (cons name (cons nil value)))))
              (push (cons name (cons expr (cdr lexinfo))) let-vars)
              (when lexinfo
                (push lexinfo (if (eq head 'let*)
                                  byte-optimize--lexvars
                                new-lexvars)))
              (when aliased
                (push expr (if (eq head 'let*)
                               byte-optimize--aliased-vars
                             new-aliased-vars))))))

        (setq byte-optimize--aliased-vars
              (append new-aliased-vars byte-optimize--aliased-vars))
        (when (and (eq head 'let) byte-optimize--aliased-vars)
          ;; Find new variables that shadow aliased variables.
          (let ((shadowing-vars nil))
            (dolist (lexvar new-lexvars)
              (let ((name (car lexvar)))
                (when (and (memq name byte-optimize--aliased-vars)
                           (not (memq name shadowing-vars)))
                  (push name shadowing-vars))))
            ;; α-rename them
            (dolist (name shadowing-vars)
              (let ((new-name (make-symbol (symbol-name name))))
                (setq new-lexvars
                      (mapcar (lambda (lexvar)
                                (if (eq (car lexvar) name)
                                    (cons new-name (cdr lexvar))
                                  lexvar))
                              new-lexvars))
                (setq let-vars
                      (mapcar (lambda (v)
                                (if (eq (car v) name)
                                    (cons new-name (cdr v))
                                  v))
                              let-vars))
                (setq body (byte-optimize--rename-var-body
                            name new-name body))))))
        (setq byte-optimize--lexvars
              (append new-lexvars byte-optimize--lexvars))
        ;; Walk the body expressions, which may mutate some of the records,
        ;; and generate new bindings that exclude unused variables.
        (let* ((byte-optimize--dynamic-vars byte-optimize--dynamic-vars)
               (opt-body (byte-optimize-body body for-effect))
               (bindings nil))
          (dolist (var let-vars)
            ;; VAR is (NAME EXPR [KEEP [VALUE]])
            (when (or (not (nthcdr 3 var)) (nth 2 var)
                      byte-optimize--inhibit-outside-loop-constprop)
              ;; Value not present, or variable marked to be kept,
              ;; or we are in the loop discovery phase: keep the binding.
              (push (list (nth 0 var) (nth 1 var)) bindings)))
          (cons bindings opt-body)))

    ;; With dynamic binding, no substitutions are in effect.
    (let ((byte-optimize--lexvars nil))
      (cons
       (mapcar (lambda (binding)
	         (if (symbolp binding)
		     binding
	           (when (or (atom binding) (cddr binding))
		     (byte-compile-warn "malformed let binding: `%S'" binding))
	           (list (car binding)
		         (byte-optimize-form (nth 1 binding) nil))))
	       (car form))
       (byte-optimize-body (cdr form) for-effect)))))


(defun byte-optimize-body (forms all-for-effect)
  ;; Optimize the cdr of a progn or implicit progn; all forms is a list of
  ;; forms, all but the last of which are optimized with the assumption that
  ;; they are being called for effect.  the last is for-effect as well if
  ;; all-for-effect is true.  returns a new list of forms.
  (let ((rest forms)
	(result nil)
	fe new)
    (while rest
      (setq fe (or all-for-effect (cdr rest)))
      (setq new (and (car rest) (byte-optimize-form (car rest) fe)))
      (when (and (consp new) (eq (car new) 'progn))
        ;; Flatten `progn' form into the body.
        (setq result (append (reverse (cdr new)) result))
        (setq new (pop result)))
      (when (or new (not fe))
	(setq result (cons new result)))
      (setq rest (cdr rest)))
    (nreverse result)))


;; some source-level optimizers
;;
;; when writing optimizers, be VERY careful that the optimizer returns
;; something not EQ to its argument if and ONLY if it has made a change.
;; This implies that you cannot simply destructively modify the list;
;; you must return something not EQ to it if you make an optimization.
;;
;; It is now safe to optimize code such that it introduces new bindings.

(defsubst byte-compile-trueconstp (form)
  "Return non-nil if FORM always evaluates to a non-nil value."
  (while (eq (car-safe form) 'progn)
    (setq form (car (last (cdr form)))))
  (cond ((consp form)
         (pcase (car form)
           ('quote (cadr form))
           ;; Can't use recursion in a defsubst.
           ;; (`progn (byte-compile-trueconstp (car (last (cdr form)))))
           ))
        ((not (symbolp form)))
        ((eq form t))
        ((keywordp form))))

(defsubst byte-compile-nilconstp (form)
  "Return non-nil if FORM always evaluates to a nil value."
  (while (eq (car-safe form) 'progn)
    (setq form (car (last (cdr form)))))
  (cond ((consp form)
         (pcase (car form)
           ('quote (null (cadr form)))
           ;; Can't use recursion in a defsubst.
           ;; (`progn (byte-compile-nilconstp (car (last (cdr form)))))
           ))
        ((not (symbolp form)) nil)
        ((null form))))

;; If the function is being called with constant integer args,
;; evaluate as much as possible at compile-time.  This optimizer
;; assumes that the function is associative, like min or max.
(defun byte-optimize-associative-math (form)
  (let ((args nil)
	(constants nil)
	(rest (cdr form)))
    (while rest
      (if (integerp (car rest))
	  (setq constants (cons (car rest) constants))
	  (setq args (cons (car rest) args)))
      (setq rest (cdr rest)))
    (if (cdr constants)
        (let ((const (apply (car form) (nreverse constants))))
	  (if args
	      (append (list (car form) const)
                      (nreverse args))
	    const))
      form)))

(defun byte-optimize-min-max (form)
  "Optimize `min' and `max'."
  (let ((opt (byte-optimize-associative-math form)))
    (if (and (consp opt) (memq (car opt) '(min max))
             (= (length opt) 4))
        ;; (OP x y z) -> (OP (OP x y) z), in order to use binary byte ops.
        (list (car opt)
              (list (car opt) (nth 1 opt) (nth 2 opt))
              (nth 3 opt))
      opt)))

;; Use OP to reduce any leading prefix of constant numbers in the list
;; (cons ACCUM ARGS) down to a single number, and return the
;; resulting list A of arguments.  The idea is that applying OP to A
;; is equivalent to (but likely more efficient than) applying OP to
;; (cons ACCUM ARGS), on any Emacs platform.  Do not make any special
;; provision for (- X) or (/ X); for example, it is the caller’s
;; responsibility that (- 1 0) should not be "optimized" to (- 1).
(defun byte-opt--arith-reduce (op accum args)
  (when (numberp accum)
    (let (accum1)
      (while (and (numberp (car args))
                  (numberp
                   (setq accum1 (condition-case ()
                                    (funcall op accum (car args))
                                  (error))))
                  (= accum1 (funcall op (float accum) (car args))))
        (setq accum accum1)
        (setq args (cdr args)))))
  (cons accum args))

(defun byte-optimize-plus (form)
  (let ((args (remq 0 (byte-opt--arith-reduce #'+ 0 (cdr form)))))
    (cond
     ;; (+) -> 0
     ((null args) 0)
     ;; (+ n) -> n, where n is a number
     ((and (null (cdr args)) (numberp (car args))) (car args))
     ;; (+ x 1) --> (1+ x) and (+ x -1) --> (1- x).
     ((and (null (cddr args)) (or (memq 1 args) (memq -1 args)))
      (let* ((arg1 (car args)) (arg2 (cadr args))
             (integer-is-first (memq arg1 '(1 -1)))
             (integer (if integer-is-first arg1 arg2))
             (other (if integer-is-first arg2 arg1)))
        (list (if (eq integer 1) '1+ '1-) other)))
     ;; (+ x y z) -> (+ (+ x y) z)
     ((= (length args) 3)
      `(+ ,(byte-optimize-plus `(+ ,(car args) ,(cadr args))) ,@(cddr args)))
     ;; not further optimized
     ((equal args (cdr form)) form)
     (t (cons '+ args)))))

(defun byte-optimize-minus (form)
  (let ((args (cdr form)))
    (if (and (cdr args)
             (null (cdr (setq args (byte-opt--arith-reduce
                                    #'- (car args) (cdr args)))))
             (numberp (car args)))
        ;; The entire argument list reduced to a constant; return it.
        (car args)
      ;; Remove non-leading zeros, except for (- x 0).
      (when (memq 0 (cdr args))
        (setq args (cons (car args) (or (remq 0 (cdr args)) (list 0)))))
      (cond
       ;; (- x 1) --> (1- x)
       ((equal (cdr args) '(1))
        (list '1- (car args)))
       ;; (- x -1) --> (1+ x)
       ((equal (cdr args) '(-1))
        (list '1+ (car args)))
       ;; (- n) -> -n, where n and -n are constant numbers.
       ;; This must be done separately since byte-opt--arith-reduce
       ;; is not applied to (- n).
       ((and (null (cdr args))
             (numberp (car args)))
        (- (car args)))
       ;; (- x y z) -> (- (- x y) z)
       ((= (length args) 3)
        `(- ,(byte-optimize-minus `(- ,(car args) ,(cadr args))) ,@(cddr args)))
       ;; not further optimized
       ((equal args (cdr form)) form)
       (t (cons '- args))))))

(defun byte-optimize-multiply (form)
  (let* ((args (remq 1 (byte-opt--arith-reduce #'* 1 (cdr form)))))
    (cond
     ;; (*) -> 1
     ((null args) 1)
     ;; (* n) -> n, where n is a number
     ((and (null (cdr args)) (numberp (car args))) (car args))
     ;; (* x y z) -> (* (* x y) z)
     ((= (length args) 3)
      `(* ,(byte-optimize-multiply `(* ,(car args) ,(cadr args)))
          ,@(cddr args)))
     ;; not further optimized
     ((equal args (cdr form)) form)
     (t (cons '* args)))))

(defun byte-optimize-divide (form)
  (let ((args (cdr form)))
    (if (and (cdr args)
             (null (cdr (setq args (byte-opt--arith-reduce
                                    #'/ (car args) (cdr args)))))
             (numberp (car args)))
        ;; The entire argument list reduced to a constant; return it.
        (car args)
      ;; Remove non-leading 1s, except for (/ x 1).
      (when (memq 1 (cdr args))
        (setq args (cons (car args) (or (remq 1 (cdr args)) (list 1)))))
      (if (equal args (cdr form))
          form
        (cons '/ args)))))

(defun byte-optimize-binary-predicate (form)
  (cond
   ((or (not (macroexp-const-p (nth 1 form)))
        (nthcdr 3 form)) ;; In case there are more than 2 args.
    form)
   ((macroexp-const-p (nth 2 form))
    (condition-case ()
        (list 'quote (eval form))
      (error form)))
   (t ;; Moving the constant to the end can enable some lapcode optimizations.
    (list (car form) (nth 2 form) (nth 1 form)))))

(defun byte-optimize-constant-args (form)
  (let ((ok t)
	(rest (cdr form)))
    (while (and rest ok)
      (setq ok (macroexp-const-p (car rest))
	    rest (cdr rest)))
    (if ok
	(condition-case ()
	    (list 'quote (eval form))
	  (error form))
	form)))

(defun byte-optimize-identity (form)
  (if (and (cdr form) (null (cdr (cdr form))))
      (nth 1 form)
    form))

(defun byte-optimize--constant-symbol-p (expr)
  "Whether EXPR is a constant symbol."
  (and (macroexp-const-p expr) (symbolp (eval expr))))

(defun byte-optimize--fixnump (o)
  "Return whether O is guaranteed to be a fixnum in all Emacsen.
See Info node `(elisp) Integer Basics'."
  (and (fixnump o) (<= -536870912 o 536870911)))

(defun byte-optimize-equal (form)
  ;; Replace `equal' or `eql' with `eq' if at least one arg is a
  ;; symbol or fixnum.
  (byte-optimize-binary-predicate
   (if (= (length (cdr form)) 2)
       (if (or (byte-optimize--constant-symbol-p (nth 1 form))
               (byte-optimize--constant-symbol-p (nth 2 form))
               (byte-optimize--fixnump (nth 1 form))
               (byte-optimize--fixnump (nth 2 form)))
           (cons 'eq (cdr form))
         form)
     ;; Arity errors reported elsewhere.
     form)))

(defun byte-optimize-eq (form)
  (pcase (cdr form)
    ((or `(,x nil) `(nil ,x)) `(not ,x))
    (_ (byte-optimize-binary-predicate form))))

(defun byte-optimize-member (form)
  (cond
   ((/= (length (cdr form)) 2) form)    ; arity error
   ((null (nth 2 form))                 ; empty list
    `(progn ,(nth 1 form) nil))
   ;; Replace `member' or `memql' with `memq' if the first arg is a symbol
   ;; or fixnum, or the second arg is a list of symbols or fixnums.
   ((or (byte-optimize--constant-symbol-p (nth 1 form))
        (byte-optimize--fixnump (nth 1 form))
        (let ((arg2 (nth 2 form)))
          (and (macroexp-const-p arg2)
               (let ((listval (eval arg2)))
                 (and (listp listval)
                      (not (memq nil (mapcar
                                      (lambda (o)
                                        (or (symbolp o)
                                            (byte-optimize--fixnump o)))
                                      listval))))))))
    (cons 'memq (cdr form)))
   (t form)))

(defun byte-optimize-assoc (form)
  ;; Replace 2-argument `assoc' with `assq', `rassoc' with `rassq',
  ;; if the first arg is a symbol or fixnum.
  (cond
   ((/= (length form) 3)
    form)
   ((null (nth 2 form))                 ; empty list
    `(progn ,(nth 1 form) nil))
   ((or (byte-optimize--constant-symbol-p (nth 1 form))
        (byte-optimize--fixnump (nth 1 form)))
    (cons (if (eq (car form) 'assoc) 'assq 'rassq)
          (cdr form)))
   (t (byte-optimize-constant-args form))))

(defun byte-optimize-assq (form)
  (cond
   ((/= (length form) 3)
    form)
   ((null (nth 2 form))                 ; empty list
    `(progn ,(nth 1 form) nil))
   (t (byte-optimize-constant-args form))))

(defun byte-optimize-memq (form)
  (if (= (length (cdr form)) 2)
      (let ((list (nth 2 form)))
        (cond
         ((null list)                   ; empty list
          `(progn ,(nth 1 form) nil))
         ;; (memq foo '(bar)) => (and (eq foo 'bar) '(bar))
         ((and (eq (car-safe list) 'quote)
               (listp (setq list (cadr list)))
               (= (length list) 1))
          `(and (eq ,(nth 1 form) ',(nth 0 list))
                ',list))
         (t form)))
    ;; Arity errors reported elsewhere.
    form))

(defun byte-optimize-concat (form)
  "Merge adjacent constant arguments to `concat'."
  (let ((args (cdr form))
        (newargs nil))
    (while args
      (let ((strings nil)
            val)
        (while (and args (macroexp-const-p (car args))
                    (progn
                      (setq val (eval (car args)))
                      (and (or (stringp val)
                               (and (or (listp val) (vectorp val))
                                    (not (memq nil
                                               (mapcar #'characterp val))))))))
          (push val strings)
          (setq args (cdr args)))
        (when strings
          (let ((s (apply #'concat (nreverse strings))))
            (when (not (zerop (length s)))
              (push s newargs)))))
      (when args
        (push (car args) newargs)
        (setq args (cdr args))))
    (if (= (length newargs) (length (cdr form)))
        form          ; No improvement.
      (cons 'concat (nreverse newargs)))))

(put 'identity 'byte-optimizer #'byte-optimize-identity)
(put 'memq 'byte-optimizer #'byte-optimize-memq)
(put 'memql  'byte-optimizer #'byte-optimize-member)
(put 'member 'byte-optimizer #'byte-optimize-member)
(put 'assoc 'byte-optimizer #'byte-optimize-assoc)
(put 'rassoc 'byte-optimizer #'byte-optimize-assoc)
(put 'assq 'byte-optimizer #'byte-optimize-assq)
(put 'rassq 'byte-optimizer #'byte-optimize-assq)

(put '+   'byte-optimizer #'byte-optimize-plus)
(put '*   'byte-optimizer #'byte-optimize-multiply)
(put '-   'byte-optimizer #'byte-optimize-minus)
(put '/   'byte-optimizer #'byte-optimize-divide)
(put 'max 'byte-optimizer #'byte-optimize-min-max)
(put 'min 'byte-optimizer #'byte-optimize-min-max)

(put '=   'byte-optimizer #'byte-optimize-binary-predicate)
(put 'eq  'byte-optimizer #'byte-optimize-eq)
(put 'eql   'byte-optimizer #'byte-optimize-equal)
(put 'equal 'byte-optimizer #'byte-optimize-equal)
(put 'string= 'byte-optimizer #'byte-optimize-binary-predicate)
(put 'string-equal 'byte-optimizer #'byte-optimize-binary-predicate)

(put 'concat 'byte-optimizer #'byte-optimize-concat)

;; I'm not convinced that this is necessary.  Doesn't the optimizer loop
;; take care of this? - Jamie
;; I think this may some times be necessary to reduce ie (quote 5) to 5,
;; so arithmetic optimizers recognize the numeric constant.  - Hallvard
(put 'quote 'byte-optimizer #'byte-optimize-quote)
(defun byte-optimize-quote (form)
  (if (or (consp (nth 1 form))
	  (and (symbolp (nth 1 form))
	       (not (macroexp--const-symbol-p (nth 1 form)))))
      form
    (nth 1 form)))

(defun byte-optimize-and (form)
  ;; Simplify if less than 2 args.
  ;; if there is a literal nil in the args to `and', throw it and following
  ;; forms away, and surround the `and' with (progn ... nil).
  (cond ((null (cdr form)))
	((memq nil form)
	 (list 'progn
	       (byte-optimize-and
		(prog1 (setq form (copy-sequence form))
		  (while (nth 1 form)
		    (setq form (cdr form)))
		  (setcdr form nil)))
	       nil))
	((null (cdr (cdr form)))
	 (nth 1 form))
	((byte-optimize-constant-args form))))

(defun byte-optimize-or (form)
  ;; Throw away nil's, and simplify if less than 2 args.
  ;; If there is a literal non-nil constant in the args to `or', throw away all
  ;; following forms.
  (setq form (remq nil form))
  (let ((rest form))
    (while (cdr (setq rest (cdr rest)))
      (if (byte-compile-trueconstp (car rest))
	  (setq form (copy-sequence form)
		rest (setcdr (memq (car rest) form) nil))))
    (if (cdr (cdr form))
	(byte-optimize-constant-args form)
      (nth 1 form))))

(defun byte-optimize-cond (form)
  ;; if any clauses have a literal nil as their test, throw them away.
  ;; if any clause has a literal non-nil constant as its test, throw
  ;; away all following clauses.
  (let (rest)
    ;; This must be first, to reduce (cond (t ...) (nil)) to (progn t ...)
    (while (setq rest (assq nil (cdr form)))
      (setq form (remq rest form)))
    (setq form (remq nil form))
    (setq rest form)
    (while (setq rest (cdr rest))
      (cond ((byte-compile-trueconstp (car-safe (car rest)))
             ;; This branch will always be taken: kill the subsequent ones.
	     (cond ((eq rest (cdr form)) ;First branch of `cond'.
		    (setq form `(progn ,@(car rest))))
		   ((cdr rest)
		    (setq form (copy-sequence form))
		    (setcdr (memq (car rest) form) nil)))
	     (setq rest nil))
            ((and (consp (car rest))
                  (byte-compile-nilconstp (caar rest)))
             ;; This branch will never be taken: kill its body.
             (setcdr (car rest) nil)))))
  ;;
  ;; Turn (cond (( <x> )) ... ) into (or <x> (cond ... ))
  (if (eq 'cond (car-safe form))
      (let ((clauses (cdr form)))
	(if (and (consp (car clauses))
		 (null (cdr (car clauses))))
	    (list 'or (car (car clauses))
		  (byte-optimize-cond
		   (cons (car form) (cdr (cdr form)))))
	  form))
    form))

(defun byte-optimize-if (form)
  ;; (if (progn <insts> <test>) <rest>) ==> (progn <insts> (if <test> <rest>))
  ;; (if <true-constant> <then> <else...>) ==> <then>
  ;; (if <false-constant> <then> <else...>) ==> (progn <else...>)
  ;; (if <test> nil <else...>) ==> (if (not <test>) (progn <else...>))
  ;; (if <test> <then> nil) ==> (if <test> <then>)
  (let ((clause (nth 1 form)))
    (cond ((and (eq (car-safe clause) 'progn)
                (proper-list-p clause))
           (if (null (cddr clause))
               ;; A trivial `progn'.
               (byte-optimize-if `(if ,(cadr clause) ,@(nthcdr 2 form)))
             (nconc (butlast clause)
                    (list
                     (byte-optimize-if
                      `(if ,(car (last clause)) ,@(nthcdr 2 form)))))))
          ((byte-compile-trueconstp clause)
	   `(progn ,clause ,(nth 2 form)))
	  ((byte-compile-nilconstp clause)
           `(progn ,clause ,@(nthcdr 3 form)))
	  ((nth 2 form)
	   (if (equal '(nil) (nthcdr 3 form))
	       (list 'if clause (nth 2 form))
	     form))
	  ((or (nth 3 form) (nthcdr 4 form))
	   (list 'if
		 ;; Don't make a double negative;
		 ;; instead, take away the one that is there.
		 (if (and (consp clause) (memq (car clause) '(not null))
			  (= (length clause) 2)) ; (not xxxx) or (not (xxxx))
		     (nth 1 clause)
		   (list 'not clause))
		 (if (nthcdr 4 form)
		     (cons 'progn (nthcdr 3 form))
		   (nth 3 form))))
	  (t
	   (list 'progn clause nil)))))

(defun byte-optimize-while (form)
  (when (< (length form) 2)
    (byte-compile-warn "too few arguments for `while'"))
  (if (nth 1 form)
      form))

(put 'and   'byte-optimizer #'byte-optimize-and)
(put 'or    'byte-optimizer #'byte-optimize-or)
(put 'cond  'byte-optimizer #'byte-optimize-cond)
(put 'if    'byte-optimizer #'byte-optimize-if)
(put 'while 'byte-optimizer #'byte-optimize-while)

;; byte-compile-negation-optimizer lives in bytecomp.el
(put '/= 'byte-optimizer #'byte-compile-negation-optimizer)
(put 'atom 'byte-optimizer #'byte-compile-negation-optimizer)
(put 'nlistp 'byte-optimizer #'byte-compile-negation-optimizer)


(defun byte-optimize-funcall (form)
  ;; (funcall (lambda ...) ...) ==> ((lambda ...) ...)
  ;; (funcall foo ...) ==> (foo ...)
  (let ((fn (nth 1 form)))
    (if (memq (car-safe fn) '(quote function))
	(cons (nth 1 fn) (cdr (cdr form)))
      form)))

(defun byte-optimize-apply (form)
  ;; If the last arg is a literal constant, turn this into a funcall.
  ;; The funcall optimizer can then transform (funcall 'foo ...) -> (foo ...).
  (if (= (length form) 2)
      ;; single-argument `apply' is not worth optimizing (bug#40968)
      form
    (let ((fn (nth 1 form))
	  (last (nth (1- (length form)) form))) ; I think this really is fastest
      (or (if (or (null last)
		  (eq (car-safe last) 'quote))
	      (if (listp (nth 1 last))
		  (let ((butlast (nreverse (cdr (reverse (cdr (cdr form)))))))
		    (nconc (list 'funcall fn) butlast
			   (mapcar (lambda (x) (list 'quote x)) (nth 1 last))))
	        (byte-compile-warn
	         "last arg to apply can't be a literal atom: `%s'"
	         (prin1-to-string last))
	        nil))
	  form))))

(put 'funcall 'byte-optimizer #'byte-optimize-funcall)
(put 'apply   'byte-optimizer #'byte-optimize-apply)


(put 'let 'byte-optimizer #'byte-optimize-letX)
(put 'let* 'byte-optimizer #'byte-optimize-letX)
(defun byte-optimize-letX (form)
  (pcase form
    ;; No bindings.
    (`(,_ () . ,body)
     `(progn . ,body))

    ;; Body is empty or just contains a constant.
    (`(,head ,bindings . ,(or '() `(,(and const (pred macroexp-const-p)))))
     (if (eq head 'let)
         `(progn ,@(mapcar (lambda (binding)
                             (and (consp binding) (cadr binding)))
                           bindings)
                 ,const)
       `(let* ,(butlast bindings)
          ,@(and (consp (car (last bindings)))
                 (cdar (last bindings)))
          ,const)))

    ;; Body is last variable.
    (`(,head ,(and bindings
                   (let last-var (let ((last (car (last bindings))))
                                   (if (consp last) (car last) last))))
             ,(and last-var             ; non-linear pattern
                   (pred symbolp) (pred (not keywordp)) (pred (not booleanp))))
     (if (eq head 'let)
         `(progn ,@(mapcar (lambda (binding)
                             (and (consp binding) (cadr binding)))
                           bindings))
       `(let* ,(butlast bindings)
          ,@(and (consp (car (last bindings)))
                 (cdar (last bindings))))))

    (_ form)))


(put 'nth 'byte-optimizer #'byte-optimize-nth)
(defun byte-optimize-nth (form)
  (if (= (safe-length form) 3)
      (if (memq (nth 1 form) '(0 1))
	  (list 'car (if (zerop (nth 1 form))
			 (nth 2 form)
		       (list 'cdr (nth 2 form))))
	form)
    form))

(put 'nthcdr 'byte-optimizer #'byte-optimize-nthcdr)
(defun byte-optimize-nthcdr (form)
  (if (= (safe-length form) 3)
      (if (memq (nth 1 form) '(0 1 2))
	  (let ((count (nth 1 form)))
	    (setq form (nth 2 form))
	    (while (>= (setq count (1- count)) 0)
	      (setq form (list 'cdr form)))
	    form)
	form)
    form))

(put 'cons 'byte-optimizer #'byte-optimize-cons)
(defun byte-optimize-cons (form)
  ;; (cons X nil) => (list X)
  (if (and (= (safe-length form) 3)
           (null (nth 2 form)))
      `(list ,(nth 1 form))
    form))

;; Fixme: delete-char -> delete-region (byte-coded)
;; optimize string-as-unibyte, string-as-multibyte, string-make-unibyte,
;; string-make-multibyte for constant args.

(put 'set 'byte-optimizer #'byte-optimize-set)
(defun byte-optimize-set (form)
  (let ((var (car-safe (cdr-safe form))))
    (cond
     ((and (eq (car-safe var) 'quote) (consp (cdr var)))
      `(setq ,(cadr var) ,@(cddr form)))
     ((and (eq (car-safe var) 'make-local-variable)
	   (eq (car-safe (setq var (car-safe (cdr var)))) 'quote)
	   (consp (cdr var)))
      `(progn ,(cadr form) (setq ,(cadr var) ,@(cddr form))))
     (t form))))

;; enumerating those functions which need not be called if the returned
;; value is not used.  That is, something like
;;    (progn (list (something-with-side-effects) (yow))
;;           (foo))
;; may safely be turned into
;;    (progn (progn (something-with-side-effects) (yow))
;;           (foo))
;; Further optimizations will turn (progn (list 1 2 3) 'foo) into 'foo.

;; Some of these functions have the side effect of allocating memory
;; and it would be incorrect to replace two calls with one.
;; But we don't try to do those kinds of optimizations,
;; so it is safe to list such functions here.
;; Some of these functions return values that depend on environment
;; state, so that constant folding them would be wrong,
;; but we don't do constant folding based on this list.

;; However, at present the only optimization we normally do
;; is delete calls that need not occur, and we only do that
;; with the error-free functions.

;; I wonder if I missed any :-\)
(let ((side-effect-free-fns
       '(% * + - / /= 1+ 1- < <= = > >= abs acos append aref ash asin atan
	 assq
         bool-vector-count-consecutive bool-vector-count-population
         bool-vector-subsetp
	 boundp buffer-file-name buffer-local-variables buffer-modified-p
	 buffer-substring byte-code-function-p
	 capitalize car-less-than-car car cdr ceiling char-after char-before
	 char-equal char-to-string char-width compare-strings
	 compare-window-configurations concat coordinates-in-window-p
	 copy-alist copy-sequence copy-marker copysign cos count-lines
	 current-time-string current-time-zone
	 decode-char
	 decode-time default-boundp default-value documentation downcase
	 elt encode-char exp expt encode-time error-message-string
	 fboundp fceiling featurep ffloor
	 file-directory-p file-exists-p file-locked-p file-name-absolute-p
         file-name-concat
	 file-newer-than-file-p file-readable-p file-symlink-p file-writable-p
	 float float-time floor format format-time-string frame-first-window
	 frame-root-window frame-selected-window
	 frame-visible-p fround ftruncate
	 get gethash get-buffer get-buffer-window getenv get-file-buffer
	 hash-table-count
	 int-to-string intern-soft isnan
	 keymap-parent
         lax-plist-get ldexp
         length length< length> length=
         line-beginning-position line-end-position
	 local-variable-if-set-p local-variable-p locale-info
	 log log10 logand logb logcount logior lognot logxor lsh
	 make-byte-code make-list make-string make-symbol mark marker-buffer max
         match-beginning match-end
	 member memq memql min minibuffer-selected-window minibuffer-window
	 mod multibyte-char-to-unibyte next-window nth nthcdr number-to-string
	 parse-colon-path plist-get plist-member
	 prefix-numeric-value previous-window prin1-to-string propertize
	 degrees-to-radians
	 radians-to-degrees rassq rassoc read-from-string regexp-opt
         regexp-quote region-beginning region-end reverse round
	 sin sqrt string string< string= string-equal string-lessp
         string> string-greaterp string-empty-p
         string-prefix-p string-suffix-p string-blank-p
         string-search string-to-char
	 string-to-number string-to-syntax substring
	 sxhash sxhash-equal sxhash-eq sxhash-eql
	 symbol-function symbol-name symbol-plist symbol-value string-make-unibyte
	 string-make-multibyte string-as-multibyte string-as-unibyte
	 string-to-multibyte
	 tan time-convert truncate
	 unibyte-char-to-multibyte upcase user-full-name
	 user-login-name user-original-login-name custom-variable-p
	 vconcat
	 window-absolute-pixel-edges window-at window-body-height
	 window-body-width window-buffer window-dedicated-p window-display-table
	 window-combination-limit window-edges window-frame window-fringes
	 window-height window-hscroll window-inside-edges
	 window-inside-absolute-pixel-edges window-inside-pixel-edges
	 window-left-child window-left-column window-margins window-minibuffer-p
	 window-next-buffers window-next-sibling window-new-normal
	 window-new-total window-normal-size window-parameter window-parameters
	 window-parent window-pixel-edges window-point window-prev-buffers
	 window-prev-sibling window-redisplay-end-trigger window-scroll-bars
	 window-start window-text-height window-top-child window-top-line
	 window-total-height window-total-width window-use-time window-vscroll
	 window-width zerop))
      (side-effect-and-error-free-fns
       '(always arrayp atom
	 bignump bobp bolp bool-vector-p
	 buffer-end buffer-list buffer-size buffer-string bufferp
	 car-safe case-table-p cdr-safe char-or-string-p characterp
	 charsetp commandp cons consp
	 current-buffer current-global-map current-indentation
	 current-local-map current-minor-mode-maps current-time
	 eobp eolp eq equal eventp
	 fixnump floatp following-char framep
	 get-largest-window get-lru-window
	 hash-table-p
         ;; `ignore' isn't here because we don't want calls to it elided;
         ;; see `byte-compile-ignore'.
	 identity integerp integer-or-marker-p interactive-p
	 invocation-directory invocation-name
	 keymapp keywordp
	 list listp
	 make-marker mark-marker markerp max-char
	 memory-limit
	 mouse-movement-p
	 natnump nlistp not null number-or-marker-p numberp
	 one-window-p overlayp
	 point point-marker point-min point-max preceding-char primary-charset
	 processp
	 recent-keys recursion-depth
	 safe-length selected-frame selected-window sequencep
	 standard-case-table standard-syntax-table stringp subrp symbolp
	 syntax-table syntax-table-p
	 this-command-keys this-command-keys-vector this-single-command-keys
	 this-single-command-raw-keys type-of
	 user-real-login-name user-real-uid user-uid
	 vector vectorp visible-frame-list
	 wholenump window-configuration-p window-live-p
	 window-valid-p windowp)))
  (while side-effect-free-fns
    (put (car side-effect-free-fns) 'side-effect-free t)
    (setq side-effect-free-fns (cdr side-effect-free-fns)))
  (while side-effect-and-error-free-fns
    (put (car side-effect-and-error-free-fns) 'side-effect-free 'error-free)
    (setq side-effect-and-error-free-fns (cdr side-effect-and-error-free-fns)))
  nil)


;; Pure functions are side-effect free functions whose values depend
;; only on their arguments, not on the platform.  For these functions,
;; calls with constant arguments can be evaluated at compile time.
;; For example, ash is pure since its results are machine-independent,
;; whereas lsh is not pure because (lsh -1 -1)'s value depends on the
;; fixnum range.
;;
;; When deciding whether a function is pure, do not worry about
;; mutable strings or markers, as they are so unlikely in real code
;; that they are not worth worrying about.  Thus string-to-char is
;; pure even though it might return different values if a string is
;; changed, and logand is pure even though it might return different
;; values if a marker is moved.

(let ((pure-fns
       '(concat regexp-opt regexp-quote
	 string-to-char string-to-syntax symbol-name
         eq eql
         = /= < <= >= > min max
         + - * / % mod abs ash 1+ 1- sqrt
         logand logior lognot logxor logcount
         copysign isnan ldexp float logb
         floor ceiling round truncate
         ffloor fceiling fround ftruncate
         string= string-equal string< string-lessp string> string-greaterp
         string-empty-p string-blank-p string-prefix-p string-suffix-p
         string-search
         consp atom listp nlistp proper-list-p
         sequencep arrayp vectorp stringp bool-vector-p hash-table-p
         null not
         numberp integerp floatp natnump characterp
         integer-or-marker-p number-or-marker-p char-or-string-p
         symbolp keywordp
         type-of
         identity ignore

         ;; The following functions are pure up to mutation of their
         ;; arguments.  This is pure enough for the purposes of
         ;; constant folding, but not necessarily for all kinds of
         ;; code motion.
         car cdr car-safe cdr-safe nth nthcdr last
         equal
         length safe-length
         memq memql member
         ;; `assoc' and `assoc-default' are excluded since they are
         ;; impure if the test function is (consider `string-match').
         assq rassq rassoc
         plist-get lax-plist-get plist-member
         aref elt
         bool-vector-subsetp
         bool-vector-count-population bool-vector-count-consecutive
         )))
  (while pure-fns
    (put (car pure-fns) 'pure t)
    (setq pure-fns (cdr pure-fns)))
  nil)

(defconst byte-constref-ops
  '(byte-constant byte-constant2 byte-varref byte-varset byte-varbind))

;; Used and set dynamically in byte-decompile-bytecode-1.
(defvar bytedecomp-op)
(defvar bytedecomp-ptr)

;; This function extracts the bitfields from variable-length opcodes.
;; Originally defined in disass.el (which no longer uses it.)
(defun disassemble-offset (bytes)
  "Don't call this!"
  ;; Fetch and return the offset for the current opcode.
  ;; Return nil if this opcode has no offset.
  (cond ((< bytedecomp-op byte-pophandler)
	 (let ((tem (logand bytedecomp-op 7)))
	   (setq bytedecomp-op (logand bytedecomp-op 248))
	   (cond ((eq tem 6)
		  ;; Offset in next byte.
		  (setq bytedecomp-ptr (1+ bytedecomp-ptr))
		  (aref bytes bytedecomp-ptr))
		 ((eq tem 7)
		  ;; Offset in next 2 bytes.
		  (setq bytedecomp-ptr (1+ bytedecomp-ptr))
		  (+ (aref bytes bytedecomp-ptr)
		     (progn (setq bytedecomp-ptr (1+ bytedecomp-ptr))
			    (ash (aref bytes bytedecomp-ptr) 8))))
		 (t tem))))		;Offset was in opcode.
	((>= bytedecomp-op byte-constant)
	 (prog1 (- bytedecomp-op byte-constant)	;Offset in opcode.
	   (setq bytedecomp-op byte-constant)))
	((or (and (>= bytedecomp-op byte-constant2)
                  (<= bytedecomp-op byte-goto-if-not-nil-else-pop))
             (memq bytedecomp-op (eval-when-compile
                                   (list byte-stack-set2 byte-pushcatch
                                         byte-pushconditioncase))))
	 ;; Offset in next 2 bytes.
	 (setq bytedecomp-ptr (1+ bytedecomp-ptr))
	 (+ (aref bytes bytedecomp-ptr)
	    (progn (setq bytedecomp-ptr (1+ bytedecomp-ptr))
		   (ash (aref bytes bytedecomp-ptr) 8))))
	((and (>= bytedecomp-op byte-listN)
	      (<= bytedecomp-op byte-discardN))
	 (setq bytedecomp-ptr (1+ bytedecomp-ptr)) ;Offset in next byte.
	 (aref bytes bytedecomp-ptr))))

(defvar byte-compile-tag-number)

;; This de-compiler is used for inline expansion of compiled functions,
;; and by the disassembler.
;;
;; This list contains numbers, which are pc values,
;; before each instruction.
(defun byte-decompile-bytecode (bytes constvec)
  "Turn BYTECODE into lapcode, referring to CONSTVEC."
  (let ((byte-compile-constants nil)
	(byte-compile-variables nil)
	(byte-compile-tag-number 0))
    (byte-decompile-bytecode-1 bytes constvec)))

;; As byte-decompile-bytecode, but updates
;; byte-compile-{constants, variables, tag-number}.
;; If MAKE-SPLICEABLE is true, then `return' opcodes are replaced
;; with `goto's destined for the end of the code.
;; That is for use by the compiler.
;; If MAKE-SPLICEABLE is nil, we are being called for the disassembler.
;; In that case, we put a pc value into the list
;; before each insn (or its label).
(defun byte-decompile-bytecode-1 (bytes constvec &optional make-spliceable)
  (let ((length (length bytes))
        (bytedecomp-ptr 0) optr tags bytedecomp-op offset
	lap tmp last-constant)
    (while (not (= bytedecomp-ptr length))
      (or make-spliceable
	  (push bytedecomp-ptr lap))
      (setq bytedecomp-op (aref bytes bytedecomp-ptr)
	    optr bytedecomp-ptr
            ;; This uses dynamic-scope magic.
            offset (disassemble-offset bytes))
      (let ((opcode (aref byte-code-vector bytedecomp-op)))
	(cl-assert opcode)
	(setq bytedecomp-op opcode))
      (cond ((memq bytedecomp-op byte-goto-ops)
	     ;; It's a pc.
	     (setq offset
		   (cdr (or (assq offset tags)
                            (let ((new (cons offset (byte-compile-make-tag))))
                              (push new tags)
                              new)))))
	    ((cond ((eq bytedecomp-op 'byte-constant2)
		    (setq bytedecomp-op 'byte-constant) t)
		   ((memq bytedecomp-op byte-constref-ops)))
	     (setq tmp (if (>= offset (length constvec))
			   (list 'out-of-range offset)
			 (aref constvec offset))
		   offset (if (eq bytedecomp-op 'byte-constant)
			      (byte-compile-get-constant tmp)
			    (or (assq tmp byte-compile-variables)
                                (let ((new (list tmp)))
                                  (push new byte-compile-variables)
                                  new)))
                   last-constant tmp))
	    ((eq bytedecomp-op 'byte-stack-set2)
	     (setq bytedecomp-op 'byte-stack-set))
	    ((and (eq bytedecomp-op 'byte-discardN) (>= offset #x80))
	     ;; The top bit of the operand for byte-discardN is a flag,
	     ;; saying whether the top-of-stack is preserved.  In
	     ;; lapcode, we represent this by using a different opcode
	     ;; (with the flag removed from the operand).
	     (setq bytedecomp-op 'byte-discardN-preserve-tos)
	     (setq offset (- offset #x80)))
            ((eq bytedecomp-op 'byte-switch)
             (cl-assert (hash-table-p last-constant) nil
                        "byte-switch used without preceding hash table")
             ;; We cannot use the original hash table referenced in the op,
             ;; so we create a copy of it, and replace the addresses with
             ;; TAGs.
             (let ((orig-table last-constant))
               (setq last-constant (copy-hash-table last-constant))
               ;; Replace all addresses with TAGs.
               (maphash #'(lambda (value offset)
                            (let ((match (assq offset tags)))
                              (puthash value
                                       (if match
                                           (cdr match)
                                         (let ((tag (byte-compile-make-tag)))
                                           (push (cons offset tag) tags)
                                           tag))
                                       last-constant)))
                        last-constant)
               ;; Replace the hash table referenced in the lapcode with our
               ;; modified one.
               (cl-loop for el in-ref lap
                        when (and (listp el) ;; make sure we're at the correct op
                                  (eq (nth 1 el) 'byte-constant)
                                  (eq (nth 2 el) orig-table))
                        ;; Jump tables are never reused, so do this exactly
                        ;; once.
                        do (setf (nth 2 el) last-constant) and return nil))))
      ;; lap = ( [ (pc . (op . arg)) ]* )
      (push (cons optr (cons bytedecomp-op (or offset 0)))
            lap)
      (setq bytedecomp-ptr (1+ bytedecomp-ptr)))
    (let ((rest lap))
      (while rest
	(cond ((numberp (car rest)))
	      ((setq tmp (assq (car (car rest)) tags))
	       ;; This addr is jumped to.
	       (setcdr rest (cons (cons nil (cdr tmp))
				  (cdr rest)))
	       (setq tags (delq tmp tags))
	       (setq rest (cdr rest))))
	(setq rest (cdr rest))))
    (if tags (error "Optimizer error: missed tags %s" tags))
    ;; Remove addrs, lap = ( [ (op . arg) | (TAG tagno) ]* )
    (mapcar (lambda (elt)
              (if (numberp elt)
                  elt
                (cdr elt)))
	    (nreverse lap))))


;;; peephole optimizer

(defconst byte-tagref-ops (cons 'TAG byte-goto-ops))

(defconst byte-conditional-ops
  '(byte-goto-if-nil byte-goto-if-not-nil byte-goto-if-nil-else-pop
    byte-goto-if-not-nil-else-pop))

(defconst byte-after-unbind-ops
   '(byte-constant byte-dup
     byte-symbolp byte-consp byte-stringp byte-listp byte-numberp byte-integerp
     byte-eq byte-not
     byte-cons byte-list1 byte-list2	; byte-list3 byte-list4
     byte-interactive-p)
   ;; How about other side-effect-free-ops?  Is it safe to move an
   ;; error invocation (such as from nth) out of an unwind-protect?
   ;; No, it is not, because the unwind-protect forms can alter
   ;; the inside of the object to which nth would apply.
   ;; For the same reason, byte-equal was deleted from this list.
   "Byte-codes that can be moved past an unbind.")

(defconst byte-compile-side-effect-and-error-free-ops
  '(byte-constant byte-dup byte-symbolp byte-consp byte-stringp byte-listp
    byte-integerp byte-numberp byte-eq byte-equal byte-not byte-car-safe
    byte-cdr-safe byte-cons byte-list1 byte-list2 byte-point byte-point-max
    byte-point-min byte-following-char byte-preceding-char
    byte-current-column byte-eolp byte-eobp byte-bolp byte-bobp
    byte-current-buffer byte-stack-ref))

(defconst byte-compile-side-effect-free-ops
  (append
   '(byte-varref byte-nth byte-memq byte-car byte-cdr byte-length byte-aref
     byte-symbol-value byte-get byte-concat2 byte-concat3 byte-sub1 byte-add1
     byte-eqlsign byte-gtr byte-lss byte-leq byte-geq byte-diff byte-negate
     byte-plus byte-max byte-min byte-mult byte-char-after byte-char-syntax
     byte-buffer-substring byte-string= byte-string< byte-nthcdr byte-elt
     byte-member byte-assq byte-quo byte-rem byte-substring)
   byte-compile-side-effect-and-error-free-ops))

;; This crock is because of the way DEFVAR_BOOL variables work.
;; Consider the code
;;
;;	(defun foo (flag)
;;	  (let ((old-pop-ups pop-up-windows)
;;		(pop-up-windows flag))
;;	    (cond ((not (eq pop-up-windows old-pop-ups))
;;		   (setq old-pop-ups pop-up-windows)
;;		   ...))))
;;
;; Uncompiled, old-pop-ups will always be set to nil or t, even if FLAG is
;; something else.  But if we optimize
;;
;;	varref flag
;;	varbind pop-up-windows
;;	varref pop-up-windows
;;	not
;; to
;;	varref flag
;;	dup
;;	varbind pop-up-windows
;;	not
;;
;; we break the program, because it will appear that pop-up-windows and
;; old-pop-ups are not EQ when really they are.  So we have to know what
;; the BOOL variables are, and not perform this optimization on them.

;; The variable `byte-boolean-vars' is now primitive and updated
;; automatically by DEFVAR_BOOL.

(defun byte-optimize-lapcode (lap &optional _for-effect)
  "Simple peephole optimizer.  LAP is both modified and returned.
If FOR-EFFECT is non-nil, the return value is assumed to be of no importance."
  (let (lap0
	lap1
	lap2
	(keep-going 'first-time)
	(add-depth 0)
	rest tmp tmp2 tmp3
	(side-effect-free (if byte-compile-delete-errors
			      byte-compile-side-effect-free-ops
			    byte-compile-side-effect-and-error-free-ops)))
    (while keep-going
      (or (eq keep-going 'first-time)
	  (byte-compile-log-lap "  ---- next pass"))
      (setq rest lap
	    keep-going nil)
      (while rest
	(setq lap0 (car rest)
	      lap1 (nth 1 rest)
	      lap2 (nth 2 rest))

	;; You may notice that sequences like "dup varset discard" are
	;; optimized but sequences like "dup varset TAG1: discard" are not.
	;; You may be tempted to change this; resist that temptation.
	(cond
	 ;; <side-effect-free> pop -->  <deleted>
	 ;;  ...including:
	 ;; const-X pop   -->  <deleted>
	 ;; varref-X pop  -->  <deleted>
	 ;; dup pop       -->  <deleted>
	 ;;
	 ((and (eq 'byte-discard (car lap1))
	       (memq (car lap0) side-effect-free))
	  (setq keep-going t)
	  (setq tmp (aref byte-stack+-info (symbol-value (car lap0))))
	  (setq rest (cdr rest))
	  (cond ((= tmp 1)
		 (byte-compile-log-lap
		  "  %s discard\t-->\t<deleted>" lap0)
		 (setq lap (delq lap0 (delq lap1 lap))))
		((= tmp 0)
		 (byte-compile-log-lap
		  "  %s discard\t-->\t<deleted> discard" lap0)
		 (setq lap (delq lap0 lap)))
		((= tmp -1)
		 (byte-compile-log-lap
		  "  %s discard\t-->\tdiscard discard" lap0)
		 (setcar lap0 'byte-discard)
		 (setcdr lap0 0))
		((error "Optimizer error: too much on the stack"))))
	 ;;
	 ;; goto*-X X:  -->  X:
	 ;;
	 ((and (memq (car lap0) byte-goto-ops)
	       (eq (cdr lap0) lap1))
	  (cond ((eq (car lap0) 'byte-goto)
		 (setq lap (delq lap0 lap))
		 (setq tmp "<deleted>"))
		((memq (car lap0) byte-goto-always-pop-ops)
		 (setcar lap0 (setq tmp 'byte-discard))
		 (setcdr lap0 0))
		((error "Depth conflict at tag %d" (nth 2 lap0))))
	  (and (memq byte-optimize-log '(t byte))
	       (byte-compile-log "  (goto %s) %s:\t-->\t%s %s:"
				 (nth 1 lap1) (nth 1 lap1)
				 tmp (nth 1 lap1)))
	  (setq keep-going t))
	 ;;
	 ;; varset-X varref-X  -->  dup varset-X
	 ;; varbind-X varref-X  -->  dup varbind-X
	 ;; const/dup varset-X varref-X --> const/dup varset-X const/dup
	 ;; const/dup varbind-X varref-X --> const/dup varbind-X const/dup
	 ;; The latter two can enable other optimizations.
	 ;;
         ;; For lexical variables, we could do the same
         ;;   stack-set-X+1 stack-ref-X  -->  dup stack-set-X+2
         ;; but this is a very minor gain, since dup is stack-ref-0,
         ;; i.e. it's only better if X>5, and even then it comes
         ;; at the cost of an extra stack slot.  Let's not bother.
	 ((and (eq 'byte-varref (car lap2))
               (eq (cdr lap1) (cdr lap2))
               (memq (car lap1) '(byte-varset byte-varbind)))
	  (if (and (setq tmp (memq (car (cdr lap2)) byte-boolean-vars))
		   (not (eq (car lap0) 'byte-constant)))
	      nil
	    (setq keep-going t)
            (if (memq (car lap0) '(byte-constant byte-dup))
                (progn
                  (setq tmp (if (or (not tmp)
                                    (macroexp--const-symbol-p
                                     (car (cdr lap0))))
                                (cdr lap0)
                              (byte-compile-get-constant t)))
		  (byte-compile-log-lap "  %s %s %s\t-->\t%s %s %s"
					lap0 lap1 lap2 lap0 lap1
					(cons (car lap0) tmp))
		  (setcar lap2 (car lap0))
		  (setcdr lap2 tmp))
	      (byte-compile-log-lap "  %s %s\t-->\tdup %s" lap1 lap2 lap1)
	      (setcar lap2 (car lap1))
	      (setcar lap1 'byte-dup)
	      (setcdr lap1 0)
	      ;; The stack depth gets locally increased, so we will
	      ;; increase maxdepth in case depth = maxdepth here.
	      ;; This can cause the third argument to byte-code to
	      ;; be larger than necessary.
	      (setq add-depth 1))))
	 ;;
	 ;; dup varset-X discard  -->  varset-X
	 ;; dup varbind-X discard  -->  varbind-X
         ;; dup stack-set-X discard  -->  stack-set-X-1
	 ;; (the varbind variant can emerge from other optimizations)
	 ;;
	 ((and (eq 'byte-dup (car lap0))
	       (eq 'byte-discard (car lap2))
	       (memq (car lap1) '(byte-varset byte-varbind
                                  byte-stack-set)))
	  (byte-compile-log-lap "  dup %s discard\t-->\t%s" lap1 lap1)
	  (setq keep-going t
		rest (cdr rest))
          (if (eq 'byte-stack-set (car lap1)) (cl-decf (cdr lap1)))
	  (setq lap (delq lap0 (delq lap2 lap))))
	 ;;
	 ;; not goto-X-if-nil              -->  goto-X-if-non-nil
	 ;; not goto-X-if-non-nil          -->  goto-X-if-nil
	 ;;
	 ;; it is wrong to do the same thing for the -else-pop variants.
	 ;;
	 ((and (eq 'byte-not (car lap0))
	       (memq (car lap1) '(byte-goto-if-nil byte-goto-if-not-nil)))
	  (byte-compile-log-lap "  not %s\t-->\t%s"
				lap1
				(cons
				 (if (eq (car lap1) 'byte-goto-if-nil)
				     'byte-goto-if-not-nil
				   'byte-goto-if-nil)
				 (cdr lap1)))
	  (setcar lap1 (if (eq (car lap1) 'byte-goto-if-nil)
			   'byte-goto-if-not-nil
			 'byte-goto-if-nil))
	  (setq lap (delq lap0 lap))
	  (setq keep-going t))
	 ;;
	 ;; goto-X-if-nil     goto-Y X:  -->  goto-Y-if-non-nil X:
	 ;; goto-X-if-non-nil goto-Y X:  -->  goto-Y-if-nil     X:
	 ;;
	 ;; it is wrong to do the same thing for the -else-pop variants.
	 ;;
	 ((and (memq (car lap0)
                     '(byte-goto-if-nil byte-goto-if-not-nil)) ; gotoX
	       (eq 'byte-goto (car lap1))                      ; gotoY
	       (eq (cdr lap0) lap2))                           ; TAG X
	  (let ((inverse (if (eq 'byte-goto-if-nil (car lap0))
			     'byte-goto-if-not-nil 'byte-goto-if-nil)))
	    (byte-compile-log-lap "  %s %s %s:\t-->\t%s %s:"
				  lap0 lap1 lap2
				  (cons inverse (cdr lap1)) lap2)
	    (setq lap (delq lap0 lap))
	    (setcar lap1 inverse)
	    (setq keep-going t)))
	 ;;
	 ;; const goto-if-* --> whatever
	 ;;
	 ((and (eq 'byte-constant (car lap0))
	       (memq (car lap1) byte-conditional-ops)
               ;; If the `byte-constant's cdr is not a cons cell, it has
               ;; to be an index into the constant pool); even though
               ;; it'll be a constant, that constant is not known yet
               ;; (it's typically a free variable of a closure, so will
               ;; only be known when the closure will be built at
               ;; run-time).
               (consp (cdr lap0)))
	  (cond ((if (memq (car lap1) '(byte-goto-if-nil
                                        byte-goto-if-nil-else-pop))
                     (car (cdr lap0))
                   (not (car (cdr lap0))))
		 (byte-compile-log-lap "  %s %s\t-->\t<deleted>"
				       lap0 lap1)
		 (setq rest (cdr rest)
		       lap (delq lap0 (delq lap1 lap))))
		(t
		 (byte-compile-log-lap "  %s %s\t-->\t%s"
				       lap0 lap1
				       (cons 'byte-goto (cdr lap1)))
		 (when (memq (car lap1) byte-goto-always-pop-ops)
		   (setq lap (delq lap0 lap)))
		 (setcar lap1 'byte-goto)))
          (setq keep-going t))
	 ;;
	 ;; varref-X varref-X  -->  varref-X dup
	 ;; varref-X [dup ...] varref-X  -->  varref-X [dup ...] dup
	 ;; stackref-X [dup ...] stackref-X+N --> stackref-X [dup ...] dup
	 ;; We don't optimize the const-X variations on this here,
	 ;; because that would inhibit some goto optimizations; we
	 ;; optimize the const-X case after all other optimizations.
	 ;;
	 ((and (memq (car lap0) '(byte-varref byte-stack-ref))
	       (progn
		 (setq tmp (cdr rest))
                 (setq tmp2 0)
		 (while (eq (car (car tmp)) 'byte-dup)
		   (setq tmp2 (1+ tmp2))
                   (setq tmp (cdr tmp)))
		 t)
	       (eq (if (eq 'byte-stack-ref (car lap0))
                       (+ tmp2 1 (cdr lap0))
                     (cdr lap0))
                   (cdr (car tmp)))
	       (eq (car lap0) (car (car tmp))))
	  (if (memq byte-optimize-log '(t byte))
	      (let ((str ""))
		(setq tmp2 (cdr rest))
		(while (not (eq tmp tmp2))
		  (setq tmp2 (cdr tmp2)
			str (concat str " dup")))
		(byte-compile-log-lap "  %s%s %s\t-->\t%s%s dup"
				      lap0 str lap0 lap0 str)))
	  (setq keep-going t)
	  (setcar (car tmp) 'byte-dup)
	  (setcdr (car tmp) 0)
	  (setq rest tmp))
	 ;;
	 ;; TAG1: TAG2: --> TAG1: <deleted>
	 ;; (and other references to TAG2 are replaced with TAG1)
	 ;;
	 ((and (eq (car lap0) 'TAG)
	       (eq (car lap1) 'TAG))
	  (and (memq byte-optimize-log '(t byte))
	       (byte-compile-log "  adjacent tags %d and %d merged"
				 (nth 1 lap1) (nth 1 lap0)))
	  (setq tmp3 lap)
	  (while (setq tmp2 (rassq lap0 tmp3))
	    (setcdr tmp2 lap1)
	    (setq tmp3 (cdr (memq tmp2 tmp3))))
	  (setq lap (delq lap0 lap)
		keep-going t)
          ;; replace references to tag in jump tables, if any
          (dolist (table byte-compile-jump-tables)
            (maphash #'(lambda (value tag)
                         (when (equal tag lap0)
                           (puthash value lap1 table)))
                     table)))
	 ;;
	 ;; unused-TAG: --> <deleted>
	 ;;
	 ((and (eq 'TAG (car lap0))
	       (not (rassq lap0 lap))
               ;; make sure this tag isn't used in a jump-table
               (cl-loop for table in byte-compile-jump-tables
                        when (member lap0 (hash-table-values table))
                        return nil finally return t))
	  (and (memq byte-optimize-log '(t byte))
	       (byte-compile-log "  unused tag %d removed" (nth 1 lap0)))
	  (setq lap (delq lap0 lap)
		keep-going t))
	 ;;
	 ;; goto   ... --> goto   <delete until TAG or end>
	 ;; return ... --> return <delete until TAG or end>
	 ;; (unless a jump-table is being used, where deleting may affect
         ;; other valid case bodies)
         ;;
	 ((and (memq (car lap0) '(byte-goto byte-return))
	       (not (memq (car lap1) '(TAG nil)))
               ;; FIXME: Instead of deferring simply when jump-tables are
               ;; being used, keep a list of tags used for switch tags and
               ;; use them instead (see `byte-compile-inline-lapcode').
               (not byte-compile-jump-tables))
	  (setq tmp rest)
	  (let ((i 0)
		(opt-p (memq byte-optimize-log '(t lap)))
		str deleted)
	    (while (and (setq tmp (cdr tmp))
			(not (eq 'TAG (car (car tmp)))))
	      (if opt-p (setq deleted (cons (car tmp) deleted)
			      str (concat str " %s")
			      i (1+ i))))
	    (if opt-p
		(let ((tagstr
		       (if (eq 'TAG (car (car tmp)))
			   (format "%d:" (car (cdr (car tmp))))
			 (or (car tmp) ""))))
		  (if (< i 6)
		      (apply 'byte-compile-log-lap-1
			     (concat "  %s" str
				     " %s\t-->\t%s <deleted> %s")
			     lap0
			     (nconc (nreverse deleted)
				    (list tagstr lap0 tagstr)))
		    (byte-compile-log-lap
		     "  %s <%d unreachable op%s> %s\t-->\t%s <deleted> %s"
		     lap0 i (if (= i 1) "" "s")
		     tagstr lap0 tagstr))))
	    (rplacd rest tmp))
	  (setq keep-going t))
	 ;;
	 ;; <safe-op> unbind --> unbind <safe-op>
	 ;; (this may enable other optimizations.)
	 ;;
	 ((and (eq 'byte-unbind (car lap1))
	       (memq (car lap0) byte-after-unbind-ops))
	  (byte-compile-log-lap "  %s %s\t-->\t%s %s" lap0 lap1 lap1 lap0)
	  (setcar rest lap1)
	  (setcar (cdr rest) lap0)
	  (setq keep-going t))
	 ;;
	 ;; varbind-X unbind-N         -->  discard unbind-(N-1)
	 ;; save-excursion unbind-N    -->  unbind-(N-1)
	 ;; save-restriction unbind-N  -->  unbind-(N-1)
	 ;;
	 ((and (eq 'byte-unbind (car lap1))
	       (memq (car lap0) '(byte-varbind byte-save-excursion
				  byte-save-restriction))
	       (< 0 (cdr lap1)))
	  (if (zerop (setcdr lap1 (1- (cdr lap1))))
	      (delq lap1 rest))
	  (if (eq (car lap0) 'byte-varbind)
	      (setcar rest (cons 'byte-discard 0))
	    (setq lap (delq lap0 lap)))
	  (byte-compile-log-lap "  %s %s\t-->\t%s %s"
		                lap0 (cons (car lap1) (1+ (cdr lap1)))
		                (if (eq (car lap0) 'byte-varbind)
		                    (car rest)
		                  (car (cdr rest)))
		                (if (and (/= 0 (cdr lap1))
			                 (eq (car lap0) 'byte-varbind))
			            (car (cdr rest))
			          ""))
	  (setq keep-going t))
	 ;;
	 ;; goto*-X ... X: goto-Y  --> goto*-Y
	 ;; goto-X ...  X: return  --> return
	 ;;
	 ((and (memq (car lap0) byte-goto-ops)
	       (memq (car (setq tmp (nth 1 (memq (cdr lap0) lap))))
		     '(byte-goto byte-return)))
	  (cond ((and (not (eq tmp lap0))
		      (or (eq (car lap0) 'byte-goto)
			  (eq (car tmp) 'byte-goto)))
		 (byte-compile-log-lap "  %s [%s]\t-->\t%s"
				       (car lap0) tmp tmp)
		 (if (eq (car tmp) 'byte-return)
		     (setcar lap0 'byte-return))
		 (setcdr lap0 (cdr tmp))
		 (setq keep-going t))))
	 ;;
	 ;; goto-*-else-pop X ... X: goto-if-* --> whatever
	 ;; goto-*-else-pop X ... X: discard --> whatever
	 ;;
	 ((and (memq (car lap0) '(byte-goto-if-nil-else-pop
				  byte-goto-if-not-nil-else-pop))
	       (memq (car (car (setq tmp (cdr (memq (cdr lap0) lap)))))
		     (eval-when-compile
		       (cons 'byte-discard byte-conditional-ops)))
	       (not (eq lap0 (car tmp))))
	  (setq tmp2 (car tmp))
	  (setq tmp3 (assq (car lap0) '((byte-goto-if-nil-else-pop
					 byte-goto-if-nil)
					(byte-goto-if-not-nil-else-pop
					 byte-goto-if-not-nil))))
	  (if (memq (car tmp2) tmp3)
	      (progn (setcar lap0 (car tmp2))
		     (setcdr lap0 (cdr tmp2))
		     (byte-compile-log-lap "  %s-else-pop [%s]\t-->\t%s"
					   (car lap0) tmp2 lap0))
	    ;; Get rid of the -else-pop's and jump one step further.
	    (or (eq 'TAG (car (nth 1 tmp)))
		(setcdr tmp (cons (byte-compile-make-tag)
				  (cdr tmp))))
	    (byte-compile-log-lap "  %s [%s]\t-->\t%s <skip>"
				  (car lap0) tmp2 (nth 1 tmp3))
	    (setcar lap0 (nth 1 tmp3))
	    (setcdr lap0 (nth 1 tmp)))
	  (setq keep-going t))
	 ;;
	 ;; const goto-X ... X: goto-if-* --> whatever
	 ;; const goto-X ... X: discard   --> whatever
	 ;;
	 ((and (eq (car lap0) 'byte-constant)
	       (eq (car lap1) 'byte-goto)
	       (memq (car (car (setq tmp (cdr (memq (cdr lap1) lap)))))
		     (eval-when-compile
		       (cons 'byte-discard byte-conditional-ops)))
	       (not (eq lap1 (car tmp))))
	  (setq tmp2 (car tmp))
	  (cond ((when (consp (cdr lap0))
		   (memq (car tmp2)
			 (if (null (car (cdr lap0)))
			     '(byte-goto-if-nil byte-goto-if-nil-else-pop)
			   '(byte-goto-if-not-nil
			     byte-goto-if-not-nil-else-pop))))
		 (byte-compile-log-lap "  %s goto [%s]\t-->\t%s %s"
				       lap0 tmp2 lap0 tmp2)
		 (setcar lap1 (car tmp2))
		 (setcdr lap1 (cdr tmp2))
		 ;; Let next step fix the (const,goto-if*) sequence.
		 (setq rest (cons nil rest))
		 (setq keep-going t))
		((or (consp (cdr lap0))
		     (eq (car tmp2) 'byte-discard))
		 ;; Jump one step further
		 (byte-compile-log-lap
		  "  %s goto [%s]\t-->\t<deleted> goto <skip>"
		  lap0 tmp2)
		 (or (eq 'TAG (car (nth 1 tmp)))
		     (setcdr tmp (cons (byte-compile-make-tag)
				       (cdr tmp))))
		 (setcdr lap1 (car (cdr tmp)))
		 (setq lap (delq lap0 lap))
		 (setq keep-going t))))
	 ;;
	 ;; X: varref-Y    ...     varset-Y goto-X  -->
	 ;; X: varref-Y Z: ... dup varset-Y goto-Z
	 ;; (varset-X goto-BACK, BACK: varref-X --> copy the varref down.)
	 ;; (This is so usual for while loops that it is worth handling).
         ;;
         ;; Here again, we could do it for stack-ref/stack-set, but
	 ;; that's replacing a stack-ref-Y with a stack-ref-0, which
         ;; is a very minor improvement (if any), at the cost of
	 ;; more stack use and more byte-code.  Let's not do it.
	 ;;
	 ((and (eq (car lap1) 'byte-varset)
	       (eq (car lap2) 'byte-goto)
	       (not (memq (cdr lap2) rest)) ;Backwards jump
	       (eq (car (car (setq tmp (cdr (memq (cdr lap2) lap)))))
		   'byte-varref)
	       (eq (cdr (car tmp)) (cdr lap1))
	       (not (memq (car (cdr lap1)) byte-boolean-vars)))
	  ;;(byte-compile-log-lap "  Pulled %s to end of loop" (car tmp))
	  (let ((newtag (byte-compile-make-tag)))
	    (byte-compile-log-lap
	     "  %s: %s ... %s %s\t-->\t%s: %s %s: ... %s %s %s"
	     (nth 1 (cdr lap2)) (car tmp)
             lap1 lap2
	     (nth 1 (cdr lap2)) (car tmp)
	     (nth 1 newtag) 'byte-dup lap1
	     (cons 'byte-goto newtag)
	     )
	    (setcdr rest (cons (cons 'byte-dup 0) (cdr rest)))
	    (setcdr tmp (cons (setcdr lap2 newtag) (cdr tmp))))
	  (setq add-depth 1)
	  (setq keep-going t))
	 ;;
	 ;; goto-X Y: ... X: goto-if*-Y  -->  goto-if-not-*-X+1 Y:
	 ;; (This can pull the loop test to the end of the loop)
	 ;;
	 ((and (eq (car lap0) 'byte-goto)
	       (eq (car lap1) 'TAG)
	       (eq lap1
		   (cdr (car (setq tmp (cdr (memq (cdr lap0) lap))))))
	       (memq (car (car tmp))
		     '(byte-goto byte-goto-if-nil byte-goto-if-not-nil
		       byte-goto-if-nil-else-pop)))
	  ;;	       (byte-compile-log-lap "  %s %s, %s %s  --> moved conditional"
	  ;;				     lap0 lap1 (cdr lap0) (car tmp))
	  (let ((newtag (byte-compile-make-tag)))
	    (byte-compile-log-lap
	     "%s %s: ... %s: %s\t-->\t%s ... %s:"
	     lap0 (nth 1 lap1) (nth 1 (cdr lap0)) (car tmp)
	     (cons (cdr (assq (car (car tmp))
			      '((byte-goto-if-nil . byte-goto-if-not-nil)
				(byte-goto-if-not-nil . byte-goto-if-nil)
				(byte-goto-if-nil-else-pop .
				                           byte-goto-if-not-nil-else-pop)
				(byte-goto-if-not-nil-else-pop .
				                               byte-goto-if-nil-else-pop))))
		   newtag)

	     (nth 1 newtag)
	     )
	    (setcdr tmp (cons (setcdr lap0 newtag) (cdr tmp)))
	    (if (eq (car (car tmp)) 'byte-goto-if-nil-else-pop)
		;; We can handle this case but not the -if-not-nil case,
		;; because we won't know which non-nil constant to push.
		(setcdr rest (cons (cons 'byte-constant
					 (byte-compile-get-constant nil))
				   (cdr rest))))
	    (setcar lap0 (nth 1 (memq (car (car tmp))
				      '(byte-goto-if-nil-else-pop
					byte-goto-if-not-nil
					byte-goto-if-nil
					byte-goto-if-not-nil
					byte-goto byte-goto))))
	    )
	  (setq keep-going t))

	 ;;
	 ;; stack-set-M [discard/discardN ...]  -->  discardN-preserve-tos
	 ;; stack-set-M [discard/discardN ...]  -->  discardN
	 ;;
	 ((and (eq (car lap0) 'byte-stack-set)
	       (memq (car lap1) '(byte-discard byte-discardN))
	       (progn
	         ;; See if enough discard operations follow to expose or
	         ;; destroy the value stored by the stack-set.
	         (setq tmp (cdr rest))
	         (setq tmp2 (1- (cdr lap0)))
	         (setq tmp3 0)
	         (while (memq (car (car tmp)) '(byte-discard byte-discardN))
	           (setq tmp3
                         (+ tmp3 (if (eq (car (car tmp)) 'byte-discard)
                                     1
                                   (cdr (car tmp)))))
	           (setq tmp (cdr tmp)))
	         (>= tmp3 tmp2)))
	  ;; Do the optimization.
	  (setq lap (delq lap0 lap))
          (setcar lap1
                  (if (= tmp2 tmp3)
                      ;; The value stored is the new TOS, so pop one more
                      ;; value (to get rid of the old value) using the
                      ;; TOS-preserving discard operator.
                      'byte-discardN-preserve-tos
                    ;; Otherwise, the value stored is lost, so just use a
                    ;; normal discard.
                    'byte-discardN))
          (setcdr lap1 (1+ tmp3))
	  (setcdr (cdr rest) tmp)
	  (byte-compile-log-lap "  %s [discard/discardN]...\t-->\t%s"
	                        lap0 lap1))

	 ;;
	 ;; discardN-preserve-tos return  -->  return
	 ;; dup return  -->  return
	 ;; stack-set-N return  -->  return     ; where N is TOS-1
	 ;;
	 ((and (eq (car lap1) 'byte-return)
	       (or (memq (car lap0) '(byte-discardN-preserve-tos byte-dup))
	           (and (eq (car lap0) 'byte-stack-set)
	                (= (cdr lap0) 1))))
	  (setq keep-going t)
	  ;; The byte-code interpreter will pop the stack for us, so
	  ;; we can just leave stuff on it.
	  (setq lap (delq lap0 lap))
	  (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1 lap1))

	 ;;
	 ;; goto-X ... X: discard  ==>  discard goto-Y ... X: discard Y:
	 ;;
	 ((and (eq (car lap0) 'byte-goto)
	       (setq tmp (cdr (memq (cdr lap0) lap)))
	       (memq (caar tmp) '(byte-discard byte-discardN
	                          byte-discardN-preserve-tos)))
	  (byte-compile-log-lap
	   "  goto-X .. X: \t-->\t%s goto-X.. X: %s Y:"
	   (car tmp) (car tmp))
	  (setq keep-going t)
	  (let* ((newtag (byte-compile-make-tag))
	         ;; Make a copy, since we sometimes modify insts in-place!
	         (newdiscard (cons (caar tmp) (cdar tmp)))
	         (newjmp (cons (car lap0) newtag)))
	    (push newtag (cdr tmp))     ;Push new tag after the discard.
	    (setcar rest newdiscard)
	    (push newjmp (cdr rest))))

	 ;;
	 ;; const discardN-preserve-tos ==> discardN const
	 ;;
	 ((and (eq (car lap0) 'byte-constant)
	       (eq (car lap1) 'byte-discardN-preserve-tos))
	  (setq keep-going t)
	  (let ((newdiscard (cons 'byte-discardN (cdr lap1))))
	    (byte-compile-log-lap
	     "  %s %s\t-->\t%s %s" lap0 lap1 newdiscard lap0)
	    (setf (car rest) newdiscard)
	    (setf (cadr rest) lap0)))
	 )
	(setq rest (cdr rest)))
      )
    ;; Cleanup stage:
    ;; Rebuild byte-compile-constants / byte-compile-variables.
    ;; Simple optimizations that would inhibit other optimizations if they
    ;; were done in the optimizing loop, and optimizations which there is no
    ;; need to do more than once.
    (setq byte-compile-constants nil
	  byte-compile-variables nil)
    (setq rest lap)
    (byte-compile-log-lap "  ---- final pass")
    (while rest
      (setq lap0 (car rest)
	    lap1 (nth 1 rest))
      (if (memq (car lap0) byte-constref-ops)
	  (if (memq (car lap0) '(byte-constant byte-constant2))
	      (unless (memq (cdr lap0) byte-compile-constants)
		(setq byte-compile-constants (cons (cdr lap0)
						   byte-compile-constants)))
	    (unless (memq (cdr lap0) byte-compile-variables)
	      (setq byte-compile-variables (cons (cdr lap0)
						 byte-compile-variables)))))
      (cond (;;
	     ;; const-C varset-X const-C  -->  const-C dup varset-X
	     ;; const-C varbind-X const-C  -->  const-C dup varbind-X
	     ;;
	     (and (eq (car lap0) 'byte-constant)
		  (eq (car (nth 2 rest)) 'byte-constant)
		  (eq (cdr lap0) (cdr (nth 2 rest)))
		  (memq (car lap1) '(byte-varbind byte-varset)))
	     (byte-compile-log-lap "  %s %s %s\t-->\t%s dup %s"
				   lap0 lap1 lap0 lap0 lap1)
	     (setcar (cdr (cdr rest)) (cons (car lap1) (cdr lap1)))
	     (setcar (cdr rest) (cons 'byte-dup 0))
	     (setq add-depth 1))
	    ;;
	    ;; const-X  [dup/const-X ...]   -->  const-X  [dup ...] dup
	    ;; varref-X [dup/varref-X ...]  -->  varref-X [dup ...] dup
	    ;;
	    ((memq (car lap0) '(byte-constant byte-varref))
	     (setq tmp rest
		   tmp2 nil)
	     (while (progn
		      (while (eq 'byte-dup (car (car (setq tmp (cdr tmp))))))
		      (and (eq (cdr lap0) (cdr (car tmp)))
			   (eq (car lap0) (car (car tmp)))))
	       (setcar tmp (cons 'byte-dup 0))
	       (setq tmp2 t))
	     (if tmp2
		 (byte-compile-log-lap
		  "  %s [dup/%s]...\t-->\t%s dup..." lap0 lap0 lap0)))
	    ;;
	    ;; unbind-N unbind-M  -->  unbind-(N+M)
	    ;;
	    ((and (eq 'byte-unbind (car lap0))
		  (eq 'byte-unbind (car lap1)))
	     (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1
				   (cons 'byte-unbind
					 (+ (cdr lap0) (cdr lap1))))
	     (setq lap (delq lap0 lap))
	     (setcdr lap1 (+ (cdr lap1) (cdr lap0))))

	    ;;
	    ;; discard/discardN/discardN-preserve-tos-X discard/discardN-Y  -->
	    ;; discardN-(X+Y)
	    ;;
	    ((and (memq (car lap0)
			'(byte-discard byte-discardN
			  byte-discardN-preserve-tos))
		  (memq (car lap1) '(byte-discard byte-discardN)))
	     (setq lap (delq lap0 lap))
	     (byte-compile-log-lap
	      "  %s %s\t-->\t(discardN %s)"
	      lap0 lap1
	      (+ (if (eq (car lap0) 'byte-discard) 1 (cdr lap0))
		 (if (eq (car lap1) 'byte-discard) 1 (cdr lap1))))
	     (setcdr lap1 (+ (if (eq (car lap0) 'byte-discard) 1 (cdr lap0))
			     (if (eq (car lap1) 'byte-discard) 1 (cdr lap1))))
	     (setcar lap1 'byte-discardN))

	    ;;
	    ;; discardN-preserve-tos-X discardN-preserve-tos-Y  -->
	    ;; discardN-preserve-tos-(X+Y)
	    ;;
	    ((and (eq (car lap0) 'byte-discardN-preserve-tos)
		  (eq (car lap1) 'byte-discardN-preserve-tos))
	     (setq lap (delq lap0 lap))
	     (setcdr lap1 (+ (cdr lap0) (cdr lap1)))
	     (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1 (car rest)))
            )
      (setq rest (cdr rest)))
    (setq byte-compile-maxdepth (+ byte-compile-maxdepth add-depth)))
  lap)

(provide 'byte-opt)


;; To avoid "lisp nesting exceeds max-lisp-eval-depth" when this file compiles
;; itself, compile some of its most used recursive functions (at load time).
;;
(eval-when-compile
 (or (byte-code-function-p (symbol-function 'byte-optimize-form))
     (subr-native-elisp-p (symbol-function 'byte-optimize-form))
     (assq 'byte-code (symbol-function 'byte-optimize-form))
     (let ((byte-optimize nil)
	   (byte-compile-warnings nil))
       (mapc (lambda (x)
	       (or noninteractive (message "compiling %s..." x))
	       (byte-compile x)
	       (or noninteractive (message "compiling %s...done" x)))
	     '(byte-optimize-form
	       byte-optimize-body
	       byte-optimize-constant-args
	       byte-optimize-binary-predicate
	       ;; Inserted some more than necessary, to speed it up.
	       byte-optimize-form-code-walker
	       byte-optimize-lapcode))))
 nil)

;;; byte-opt.el ends here
