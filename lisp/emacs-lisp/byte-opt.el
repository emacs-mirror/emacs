;;; byte-opt.el --- the optimization passes of the emacs-lisp byte compiler -*- lexical-binding: t -*-

;; Copyright (C) 1991, 1994, 2000-2026 Free Software Foundation, Inc.

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
;; ;; An awful lot of functions always return a non-nil value.  If they're
;; ;; error free also they may act as true-constants.
;;
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

(defun bytecomp--log-lap-arg (arg)
  ;; Convert an argument that may be a LAP operation to something printable.
  (cond
   ;; Symbols are just stripped of their -byte prefix if any.
   ((symbolp arg)
    (intern (string-remove-prefix "byte-" (symbol-name arg))))
   ;; Conses are assumed to be LAP ops or tags.
   ((and (consp arg) (symbolp (car arg)))
    (let* ((head (car arg))
           (tail (cdr arg))
           (op (intern (string-remove-prefix "byte-" (symbol-name head)))))
      (cond
       ((eq head 'TAG)
        (format "%d:" (car tail)))
       ((memq head byte-goto-ops)
        (format "(%s %d)" op (cadr tail)))
       ((memq head byte-constref-ops)
        (format "(%s %s)"
                (if (eq op 'constant) 'const op)
                (if (numberp tail)
                    (format "<V%d>" tail)     ; closure var reference
                  (format "%S" (car tail))))) ; actual constant
       ;; Ops with an immediate argument.
       ((memq op '( stack-ref stack-set call unbind
                    listN concatN insertN discardN discardN-preserve-tos))
        (format "(%s %S)" op tail))
       ;; Without immediate, print just the symbol.
       (t op))))
   ;; Anything else is printed as-is.
   (t arg)))

(defun byte-compile-log-lap-1 (format &rest args)
  (byte-compile-log-1
   (apply #'format-message format (mapcar #'bytecomp--log-lap-arg args))))

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
       (byte-compile-warn-x name
                            "attempt to inline `%s' before it was defined"
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
      ((pred interpreted-function-p)
       ;; While byte-compile-unfold-bcf can inline dynbind byte-code into
       ;; letbind byte-code (or any other combination for that matter), we
       ;; can only inline dynbind source into dynbind source or lexbind
       ;; source into lexbind source.
       ;; We assume that the function comes from another file (it would
       ;; have already been compiled otherwise) and byte-compile
       ;; the inlined function first, and then inline its byte-code.
       ;; This also has the advantage that the final code does not
       ;; depend on the order of compilation of Elisp files, making
       ;; the build more reproducible.

       ;; Since we are called from inside the optimizer, we need to make
       ;; sure not to propagate lexvar values.
       (let ((byte-optimize--lexvars nil)
             ;; Silence all compilation warnings: the useful ones should
             ;; be displayed when the function's source file will be
             ;; compiled anyway, but more importantly we would otherwise
             ;; emit spurious warnings here because we don't have the full
             ;; context, such as `declare-function's placed earlier in the
             ;; source file's code or `with-suppressed-warnings' that
             ;; surrounded the `defsubst'.
             (byte-compile-warnings nil))
         (byte-compile name))
       (let ((bc (symbol-function name)))
         (byte-compile--check-arity-bytecode form bc)
         `(,bc ,@(cdr form))))

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
  "List of variables declared as dynamic during optimization.")

(defvar byte-optimize--aliased-vars nil
  "List of variables which may be aliased by other lexical variables.
Each element is (NAME . ALIAS) where NAME is the aliased variable
and ALIAS the variable record (in the format described for
`byte-optimize--lexvars') for an alias, which may have NAME as its VALUE.
There can be multiple entries for the same NAME if it has several aliases.")

(defun byte-optimize--substitutable-p (expr)
  "Whether EXPR is a constant that can be propagated."
  (or (booleanp expr)
      (numberp expr)
      (arrayp expr)
      (let ((head (car-safe expr)))
        (cond ((eq head 'quote) t)
              ;; Don't substitute #'(lambda ...) since that would enable
              ;; uncontrolled inlining.
              ((eq head 'function) (symbolp (cadr expr)))
              ;; (internal-get-closed-var N) can be considered constant for
              ;; const-prop purposes.
              ((eq head 'internal-get-closed-var) (integerp (cadr expr)))))
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

(defsubst byte-opt--fget (f prop)
  "Simpler and faster version of `function-get'."
  (let ((val nil))
    (while (and (symbolp f) f
                (null (setq val (get f prop))))
      (setq f (symbol-function f)))
    val))

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
	   (byte-compile-warn-x form "malformed quote form: `%s'"
			        form))
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
       ;; optimization (see comment about `if' below).
       (cons fn
             (mapcar (lambda (clause)
                       (if (consp clause)
                           (cons
                            (byte-optimize-form (car clause) nil)
                            (byte-optimize-body (cdr clause) for-effect))
                         (byte-compile-warn-x
                          clause "malformed cond form: `%s'"
                          clause)
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
	         `(,fn ,exp-opt ,@exps-opt)))
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
         `(,fn ,test-opt ,then-opt . ,else-opt)))

      (`(,(or 'and 'or) . ,exps)
       ;; FIXME: We have to traverse the expressions in left-to-right
       ;; order (because that is the order of evaluation and variable
       ;; mutations must be found prior to their use), but doing so we miss
       ;; some optimization opportunities:
       ;; consider (and A B) in a for-effect context, where B => nil.
       ;; Then A could be optimized in a for-effect context too.
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
       ;; We currently don't perform this important optimization.
       (let* ((byte-optimize--vars-outside-loop byte-optimize--lexvars)
              (condition-body
               (if byte-optimize--inhibit-outside-loop-constprop
                   ;; We are already inside the discovery phase of an outer
                   ;; loop so there is no need for traversing this loop twice.
                   (cons exp exps)
                 ;; Discovery phase: run optimization without substitution
                 ;; of variables bound outside this loop.
                 (let ((byte-optimize--inhibit-outside-loop-constprop t))
                   (cons (byte-optimize-form exp nil)
                         (byte-optimize-body exps t)))))
              ;; Optimize again, this time with constprop enabled (unless
              ;; we are in discovery of an outer loop),
              ;; as mutated variables have been marked as non-substitutable.
              (condition (byte-optimize-form (car condition-body) nil))
              (body (byte-optimize-body (cdr condition-body) t)))
         `(,fn ,condition . ,body)))

      (`(interactive . ,_)
       (byte-compile-warn-x form "misplaced interactive spec: `%s'" form)
       nil)

      (`(function . ,_)
       ;; This forms is compiled as constant or by breaking out
       ;; all the subexpressions and compiling them separately.
       (and (not for-effect) form))

      (`(condition-case ,var ,exp . ,clauses)
       `(,fn ,var          ;Not evaluated.
             ,(byte-optimize-form exp
                                  (if (assq :success clauses)
                                      (null var)
                                    for-effect))
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

      (`(unwind-protect ,protected-expr :fun-body ,unwind-fun)
       ;; FIXME: The return value of UNWIND-FUN is never used so we
       ;; could potentially optimize it for-effect, but we don't do
       ;; that right no.
       `(,fn ,(byte-optimize-form protected-expr for-effect)
             :fun-body ,(byte-optimize-form unwind-fun)))

      (`(catch ,tag . ,exps)
       `(,fn ,(byte-optimize-form tag nil)
          . ,(byte-optimize-body exps for-effect)))

      ;; Needed as long as we run byte-optimize-form after cconv.
      (`(internal-make-closure ,vars ,env . ,rest)
       (if for-effect
           `(progn ,@(byte-optimize-body env t))
         `(,fn ,vars ,(mapcar #'byte-optimize-form env) . ,rest)))

      (`(setq ,var ,expr)
       (let ((lexvar (assq var byte-optimize--lexvars))
             (value (byte-optimize-form expr nil)))
         (when lexvar
           (setcar (cdr lexvar) t)    ; Mark variable to be kept.
           (setcdr (cdr lexvar) nil)  ; Inhibit further substitution.

           ;; Cancel substitution of variables aliasing this one.
           (let ((aliased-vars byte-optimize--aliased-vars))
             (while
                 (let ((alias (assq var aliased-vars)))
                   (and alias
                        (progn
                          ;; Found a variable bound to VAR but VAR is
                          ;; now mutated; cancel aliasing.
                          (setcdr (cddr alias) nil)
                          (setq aliased-vars (cdr (memq alias aliased-vars)))
                          t))))))
         `(,fn ,var ,value)))

      (`(defvar ,(and (pred symbolp) name) . ,rest)
       (let ((optimized-rest (and rest
                                  (cons (byte-optimize-form (car rest) nil)
                                        (cdr rest)))))
         (push name byte-optimize--dynamic-vars)
         `(,fn ,name . ,optimized-rest)))

      ((guard (when for-effect
		(if-let* ((tmp (byte-opt--fget fn 'side-effect-free)))
		    (or byte-compile-delete-errors
		        (eq tmp 'error-free)))))
       (byte-compile-log "  %s called for effect; deleted" fn)
       (byte-optimize-form (cons 'progn (cdr form)) t))

      (_
       ;; Otherwise, no args can be considered to be for-effect,
       ;; even if the called function is for-effect, because we
       ;; don't know anything about that function.
       (let ((form (cons fn (mapcar #'byte-optimize-form (cdr form)))))
	 (if (byte-opt--fget fn 'pure)
	     (byte-optimize-constant-args form)
	   form))))))

(defun byte-optimize-one-form (form &optional for-effect)
  "The source-level pass of the optimizer."
  ;; Make optimizer aware of lexical arguments.
  (let ((byte-optimize--lexvars
         (mapcar (lambda (v) (list (car v) t))
                 byte-compile--lexical-environment)))
    (byte-optimize-form form for-effect)))

(defun byte-optimize-form (form &optional for-effect)
  (while
      (progn
        ;; First, optimize all sub-forms of this one.
        ;; `byte-optimize-form-code-walker' fails to preserve any
        ;; position on `form' in enough separate places that we invoke
        ;; `macroexp-preserve-posification' here for source code economy.
        (setq form
              (macroexp-preserve-posification
                  form (byte-optimize-form-code-walker form for-effect)))

        ;; If a form-specific optimizer is available, run it and start over
        ;; until a fixpoint has been reached.
        (and (consp form)
             (symbolp (car form))
             (let ((opt (byte-opt--fget (car form) 'byte-optimizer)))
               (and opt
                    (let ((old form)
                          (new (macroexp-preserve-posification
                                   form (funcall opt form))))
	              (byte-compile-log "  %s\t==>\t%s" old new)
                      (setq form new)
                      (not (eq new old))))))))
  form)

(defun byte-optimize--rename-var-body (var new-var body)
  "Replace VAR with NEW-VAR in BODY."
  (mapcar (lambda (form) (byte-optimize--rename-var var new-var form)) body))

(defun byte-optimize--rename-var (var new-var form)
  "Replace VAR with NEW-VAR in FORM."
  (let ((fn (car-safe form)))
    (pcase form
      ((pred symbolp) (if (eq form var) new-var form))
      (`(setq . ,args)
       (let ((new-args nil))
         (while args
           (push (byte-optimize--rename-var var new-var (car args)) new-args)
           (push (byte-optimize--rename-var var new-var (cadr args)) new-args)
           (setq args (cddr args)))
         `(,fn . ,(nreverse new-args))))
      ;; In binding constructs like `let', `let*' and `condition-case' we
      ;; rename everything for simplicity, even new bindings named VAR.
      (`(,(and head (or 'let 'let*)) ,bindings . ,body)
       `(,head
         ,(mapcar (lambda (b) (byte-optimize--rename-var-body var new-var b))
                  bindings)
         ,@(byte-optimize--rename-var-body var new-var body)))
      (`(condition-case ,res-var ,protected-form . ,handlers)
       `(,fn ,(byte-optimize--rename-var var new-var res-var)
             ,(byte-optimize--rename-var var new-var protected-form)
             ,@(mapcar (lambda (h)
                         (cons (car h)
                               (byte-optimize--rename-var-body var new-var (cdr h))))
                       handlers)))
      (`(internal-make-closure ,vars ,env . ,rest)
       `(,fn
         ,vars ,(byte-optimize--rename-var-body var new-var env) . ,rest))
      (`(defvar ,name . ,rest)
       ;; NAME is not renamed here; we only care about lexical variables.
       `(,fn ,name . ,(byte-optimize--rename-var-body var new-var rest)))

      (`(cond . ,clauses)
       `(,fn ,@(mapcar (lambda (c)
                         (byte-optimize--rename-var-body var new-var c))
                       clauses)))

      (`(function . ,_) form)
      (`(quote . ,_) form)
      (`(lambda . ,_) form)

      ;; Function calls and special forms not handled above.
      (`(,head . ,args)
       `(,head . ,(byte-optimize--rename-var-body var new-var args)))
      (_ form))))

(defun byte-optimize-let-form (head form for-effect)
  ;; Recursively enter the optimizer for the bindings and body
  ;; of a let or let*.  This for depth-firstness: forms that
  ;; are more deeply nested are optimized first.
  (if lexical-binding
      (let* ((byte-optimize--lexvars byte-optimize--lexvars)
             (byte-optimize--aliased-vars byte-optimize--aliased-vars)
             (new-lexvars nil)
             (let-vars nil)
             (body (cdr form))
             (bindings (car form)))
        (while bindings
          (let* ((binding (car bindings))
                 (name (car binding))
                 (expr (byte-optimize-form (cadr binding) nil)))
            (setq bindings (cdr bindings))
            (when (and (eq head 'let*)
                       (assq name byte-optimize--aliased-vars))
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
            (let* ((aliased
                    ;; Aliasing another lexvar.
                    (and (symbolp expr) (assq expr byte-optimize--lexvars)))
                   (value (and (or aliased
                                   (byte-optimize--substitutable-p expr))
                               (list expr)))
                   (lexical (not (or (special-variable-p name)
                                     (memq name byte-compile-bound-variables)
                                     (memq name byte-optimize--dynamic-vars))))
                   (lexinfo (and lexical (cons name (cons nil value)))))
              (push (cons name (cons expr (cdr lexinfo))) let-vars)
              (when lexinfo
                (push lexinfo (if (eq head 'let*)
                                  byte-optimize--lexvars
                                new-lexvars))
                (when aliased
                  (push (cons expr lexinfo) byte-optimize--aliased-vars))))))

        (when (and (eq head 'let) byte-optimize--aliased-vars)
          ;; Find new variables that shadow aliased variables.
          (let ((shadowing-vars nil))
            (dolist (lexvar new-lexvars)
              (let ((name (car lexvar)))
                (when (and (assq name byte-optimize--aliased-vars)
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
	         (list (car binding)
		       (byte-optimize-form (nth 1 binding) nil)))
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

(defsubst byte-opt--bool-value-form (form)
  "The form in FORM that yields its boolean value, possibly FORM itself."
  (while (let ((head (car-safe form)))
           (cond ((memq head '( progn inline save-excursion save-restriction
                                save-current-buffer))
                  (setq form (car (last (cdr form))))
                  t)
                 ((memq head '(let let*))
                  (setq form (car (last (cddr form))))
                  t)
                 ((memq head '( prog1 unwind-protect copy-sequence identity
                                reverse nreverse sort))
                  (setq form (nth 1 form))
                  t)
                 ((memq head '(mapc setq setcar setcdr puthash set))
                  (setq form (nth 2 form))
                  t)
                 ((memq head '(aset put function-put))
                  (setq form (nth 3 form))
                  t))))
  form)

(defun byte-compile-trueconstp (form)
  "Return non-nil if FORM always evaluates to a non-nil value."
  (setq form (byte-opt--bool-value-form form))
  (cond ((consp form)
         (let ((head (car form)))
           ;; FIXME: Lots of other expressions are statically non-nil.
           (cond ((memq head '(quote function)) (cadr form))
                 ((eq head 'list) (cdr form))
                 ((memq head
                        ;; FIXME: Replace this list with a function property?
                        '( lambda internal-make-closure
                           length safe-length cons
                           string unibyte-string make-string concat
                           format format-message
                           substring substring-no-properties string-replace
                           replace-regexp-in-string symbol-name make-symbol
                           compare-strings string-distance
                           mapconcat
                           vector make-vector vconcat make-record record
                           regexp-quote regexp-opt
                           buffer-string buffer-substring
                           buffer-substring-no-properties
                           current-buffer buffer-size get-buffer-create
                           point point-min point-max buffer-end count-lines
                           following-char preceding-char get-byte max-char
                           region-beginning region-end
                           line-beginning-position line-end-position
                           pos-bol pos-eol
                           + - * / % 1+ 1- min max abs mod expt logb
                           logand logior logxor lognot ash logcount
                           floor ceiling round truncate
                           sqrt sin cos tan asin acos atan exp log copysign
                           ffloor fceiling fround ftruncate float
                           ldexp frexp
                           number-to-string string-to-number
                           int-to-string char-to-string
                           prin1-to-string read-from-string
                           byte-to-string string-to-vector string-to-char
                           capitalize upcase downcase
                           propertize
                           string-as-multibyte string-as-unibyte
                           string-to-multibyte string-to-unibyte
                           string-make-multibyte string-make-unibyte
                           string-width char-width
                           make-hash-table hash-table-count
                           unibyte-char-to-multibyte multibyte-char-to-unibyte
                           sxhash sxhash-equal sxhash-eq sxhash-eql
                           sxhash-equal-including-properties
                           make-marker copy-marker point-marker mark-marker
                           set-marker
                           kbd key-description
                           skip-chars-forward skip-chars-backward
                           skip-syntax-forward skip-syntax-backward
                           current-column current-indentation
                           char-syntax syntax-class-to-char
                           parse-partial-sexp goto-char forward-line
                           next-window previous-window minibuffer-window
                           selected-frame selected-window
                           standard-case-table standard-syntax-table
                           syntax-table
                           frame-first-window frame-root-window
                           frame-selected-window
                           always))
                  t)
                 ((eq head 'if)
                  (and (byte-compile-trueconstp (nth 2 form))
                       (byte-compile-trueconstp (car (last (cdddr form))))))
                 ((memq head '(not null))
                  (byte-compile-nilconstp (cadr form)))
                 ((eq head 'or)
                  (and (cdr form)
                       (byte-compile-trueconstp (car (last (cdr form)))))))))
        ((not (symbolp form)))
        ((eq form t))
        ((keywordp form))))

(defun byte-compile-nilconstp (form)
  "Return non-nil if FORM always evaluates to a nil value."
  (setq form (byte-opt--bool-value-form form))
  (or (not form)   ; assume (quote nil) always being normalized to nil
      (and (consp form)
           (let ((head (car form)))
             (cond ((memq head
                          ;; Some forms that are statically nil.
                          ;; FIXME: Replace with a function property?
                          '( while ignore
                             insert insert-and-inherit insert-before-markers
                             insert-before-markers-and-inherit
                             insert-char insert-byte insert-buffer-substring
                             delete-region delete-char
                             widen narrow-to-region transpose-regions
                             forward-char backward-char
                             beginning-of-line end-of-line
                             erase-buffer buffer-swap-text
                             delete-overlay delete-all-overlays
                             remhash
                             maphash
                             map-charset-chars map-char-table
                             mapbacktrace
                             mapatoms
                             ding beep sleep-for
                             json-insert
                             set-match-data
                             ))
                    t)
                   ((eq head 'if)
                    (and (byte-compile-nilconstp (nth 2 form))
                         (byte-compile-nilconstp (car (last (cdddr form))))))
                   ((memq head '(not null))
                    (byte-compile-trueconstp (cadr form)))
                   ((eq head 'and)
                    (and (cdr form)
                         (byte-compile-nilconstp (car (last (cdr form)))))))))))

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
  (let* ((not-0 (remq 0 (byte-opt--arith-reduce #'+ 0 (cdr form))))
         (args (if (and (= (length not-0) 1)
                        (> (length form) 2))
                   ;; We removed numbers and only one arg remains: add a 0
                   ;; so that it isn't turned into (* X 1) later on.
                   (append not-0 '(0))
                 not-0)))
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

(defun byte-opt--nary-comparison (form)
  "Optimize n-ary comparisons such as `=', `<' etc."
  (let ((nargs (length (cdr form))))
    (cond
     ((= nargs 1)
      `(progn ,(cadr form) t))
     ((>= nargs 3)
      ;; At least 3 arguments: transform to N-1 binary comparisons,
      ;; since those have their own byte-ops which are particularly
      ;; fast for fixnums.
      (let* ((op (car form))
             (bindings nil)
             (rev-args nil))
        (if (all #'macroexp-copyable-p (cddr form))
            ;; All args beyond the first are copyable: no temporary variables
            ;; required.
            (setq rev-args (reverse (cdr form)))
          ;; At least one arg beyond the first is non-constant non-variable:
          ;; create temporaries for all args to guard against side-effects.
          ;; The optimizer will eliminate trivial bindings later.
          (let ((i 1))
            (dolist (arg (cdr form))
              (let ((var (make-symbol (format "arg%d" i))))
                (push var rev-args)
                (push (list var arg) bindings)
                (setq i (1+ i))))))
        (let ((prev (car rev-args))
              (exprs nil))
          (dolist (arg (cdr rev-args))
            (push (list op arg prev) exprs)
            (setq prev arg))
          (let ((and-expr (cons 'and exprs)))
            (if bindings
                (list 'let (nreverse bindings) and-expr)
              and-expr)))))
     (t form))))

(defun byte-optimize-constant-args (form)
  (if (all #'macroexp-const-p (cdr form))
      (condition-case ()
	  (list 'quote (eval form t))
	(error form))
    form))

(defun byte-optimize-identity (form)
  (if (and (cdr form) (null (cdr (cdr form))))
      (nth 1 form)
    form))

(defun byte-optimize--constant-symbol-p (expr)
  "Whether EXPR is a constant symbol, like (quote hello), nil, t, or :keyword."
  (if (consp expr)
      (and (memq (car expr) '(quote function))
           (symbolp (cadr expr)))
    (or (memq expr '(nil t))
        (keywordp expr))))

(defsubst byteopt--eval-const (expr)
  "Evaluate EXPR which must be a constant (quoted or self-evaluating).
Ie, (macroexp-const-p EXPR) must be true."
  (if (consp expr)
      (cadr expr)                   ; assumed to be 'VALUE or #'SYMBOL
    expr))

(defun byte-optimize--fixnump (o)
  "Return whether O is guaranteed to be a fixnum in all Emacsen.
See Info node `(elisp) Integer Basics'."
  (and (integerp o) (<= -536870912 o 536870911)))

(defun byte-optimize-equal (form)
  (cond ((/= (length (cdr form)) 2) form)  ; Arity errors reported elsewhere.
        ;; Anything is identical to itself.
        ((and (eq (nth 1 form) (nth 2 form)) (symbolp (nth 1 form))) t)
        ;; Replace `equal' or `eql' with `eq' if at least one arg is a
        ;; symbol or fixnum.
        ((or (byte-optimize--constant-symbol-p (nth 1 form))
             (byte-optimize--constant-symbol-p (nth 2 form))
             (byte-optimize--fixnump (nth 1 form))
             (byte-optimize--fixnump (nth 2 form)))
         (byte-optimize-binary-predicate (cons 'eq (cdr form))))
        (t (byte-optimize-binary-predicate form))))

(defun byte-optimize-eq (form)
  (cond ((/= (length (cdr form)) 2) form)  ; arity error
        ;; Anything is identical to itself.
        ((and (eq (nth 1 form) (nth 2 form)) (symbolp (nth 1 form))) t)
        ;; Strength-reduce comparison with `nil'.
        ((null (nth 1 form)) `(not ,(nth 2 form)))
        ((null (nth 2 form)) `(not ,(nth 1 form)))
        (t (byte-optimize-binary-predicate form))))

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
               (let ((listval (byteopt--eval-const arg2)))
                 (and (listp listval)
                      (all (lambda (o)
                             (or (symbolp o) (byte-optimize--fixnump o)))
                           listval))))))
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
               (null (cdr list)))
          `(and (eq ,(nth 1 form) ',(nth 0 list))
                ',list))
         (t form)))
    ;; Arity errors reported elsewhere.
    form))

(defun byte-optimize-concat (form)
  "Merge adjacent constant arguments to `concat' and flatten nested forms."
  (let ((args (cdr form))
        (newargs nil))
    (while args
      (let ((strings nil))
        (while
            (and args
                 (let ((arg (car args)))
                   (pcase arg
                     ;; Merge consecutive constant arguments.
                     ((pred macroexp-const-p)
                      (let ((val (byteopt--eval-const arg)))
                        (and (or (stringp val)
                                 (and (or (listp val) (vectorp val))
                                      (not (memq nil
                                                 (mapcar #'characterp val)))))
                             (progn
                               (push val strings)
                               (setq args (cdr args))
                               t))))
                     ;; Flatten nested `concat' form.
                     (`(concat . ,nested-args)
                      (setq args (append nested-args (cdr args)))
                      t)))))

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

(defun byte-optimize-string-greaterp (form)
  ;; Rewrite in terms of `string-lessp' which has its own bytecode.
  (pcase (cdr form)
    (`(,a ,b) (let ((arg1 (make-symbol "arg1")))
                `(let ((,arg1 ,a))
                   (string-lessp ,b ,arg1))))
    (_ form)))

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

(put 'eq  'byte-optimizer #'byte-optimize-eq)
(put 'eql   'byte-optimizer #'byte-optimize-equal)
(put 'equal 'byte-optimizer #'byte-optimize-equal)
(put 'string= 'byte-optimizer #'byte-optimize-binary-predicate)
(put 'string-equal 'byte-optimizer #'byte-optimize-binary-predicate)

(put '=  'byte-optimizer #'byte-opt--nary-comparison)
(put '<  'byte-optimizer #'byte-opt--nary-comparison)
(put '<= 'byte-optimizer #'byte-opt--nary-comparison)
(put '>  'byte-optimizer #'byte-opt--nary-comparison)
(put '>= 'byte-optimizer #'byte-opt--nary-comparison)

(put 'string-greaterp 'byte-optimizer #'byte-optimize-string-greaterp)
(put 'string> 'byte-optimizer #'byte-optimize-string-greaterp)

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
  (let ((seq nil)
        (new-args nil)
        (nil-result nil)
        (args (cdr form)))
    (while
        (and args
             (let ((arg (car args)))
               (cond
                (seq                    ; previous arg was always-true
                 (push arg seq)
                 (unless (and (cdr args) (byte-compile-trueconstp arg))
                   (push `(progn . ,(nreverse seq)) new-args)
                   (setq seq nil))
                 t)
                ((and (cdr args) (byte-compile-trueconstp arg))
                 ;; Always-true arg: evaluate unconditionally.
                 (push arg seq)
                 t)
                ((and arg (not (byte-compile-nilconstp arg)))
                 (push arg new-args)
                 t)
                (t
                 ;; Throw away the remaining args; this one is always false.
                 (setq nil-result t)
                 (when arg
                   (push arg new-args))  ; keep possible side-effects
                 nil))))
      (setq args (cdr args)))

    (setq new-args (nreverse new-args))
    (if (equal new-args (cdr form))
        ;; Input is unchanged: keep original form, and don't represent
        ;; a nil result explicitly because that would lead to infinite
        ;; growth when the optimizer is iterated.
        (setq nil-result nil)
      (setq form (cons (car form) new-args)))

    (let ((new-form
           (pcase form
             ;; (and (progn ... X) ...) -> (progn ... (and X ...))
             (`(,head (progn . ,forms) . ,rest)
              `(progn ,@(butlast forms) (,head ,(car (last forms)) . ,rest)))
             (`(,_) t)                   ; (and) -> t
             (`(,_ ,arg) arg)            ; (and X) -> X
             (_ (byte-optimize-constant-args form)))))
      (if nil-result
          `(progn ,new-form nil)
        new-form))))

(defun byte-optimize-or (form)
  (let ((seq nil)
        (new-args nil)
        (args (remq nil (cdr form))))   ; Discard nil arguments.
    (while
        (and args
             (let ((arg (car args)))
               (cond
                (seq                    ; previous arg was always-false
                 (push arg seq)
                 (unless (and (cdr args) (byte-compile-nilconstp arg))
                   (push `(progn . ,(nreverse seq)) new-args)
                   (setq seq nil))
                 t)
                ((and (cdr args) (byte-compile-nilconstp arg))
                 ;; Always-false arg: evaluate unconditionally.
                 (push arg seq)
                 t)
                (t
                 (push arg new-args)
                 ;; If this arg is always true, throw away the remaining args.
                 (not (byte-compile-trueconstp arg))))))
      (setq args (cdr args)))

    (setq new-args (nreverse new-args))
    ;; Keep original form unless the arguments changed.
    (unless (equal new-args (cdr form))
      (setq form (cons (car form) new-args)))

    (pcase form
      ;; (or (progn ... X) ...) -> (progn ... (or X ...))
      (`(,head (progn . ,forms) . ,rest)
       `(progn ,@(butlast forms) (,head ,(car (last forms)) . ,rest)))
      (`(,_) nil)                       ; (or) -> nil
      (`(,_ ,arg) arg)                  ; (or X) -> X
      (_ (byte-optimize-constant-args form)))))

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
	  (and clauses form)))
    form))

(defsubst byte-opt--negate (form)
  "Negate FORM, avoiding double negation if already negated."
  (if (and (consp form) (memq (car form) '(not null)))
      (cadr form)
    `(not ,form)))

(defun byte-optimize-if (form)
  (let ((condition (nth 1 form))
        (then (nth 2 form))
        (else (nthcdr 3 form)))
    (cond
     ;; (if (progn ... X) ...) -> (progn ... (if X ...))
     ((eq (car-safe condition) 'progn)
      (nconc (butlast condition)
             (list
              (byte-optimize-if
               `(,(car form) ,(car (last condition)) ,@(nthcdr 2 form))))))
     ;; (if TRUE THEN ...) -> (progn TRUE THEN)
     ((byte-compile-trueconstp condition)
      `(progn ,condition ,then))
     ;; (if FALSE THEN ELSE...) -> (progn FALSE ELSE...)
     ((byte-compile-nilconstp condition)
      (if else
          `(progn ,condition ,@else)
        condition))
     ;; (if X t) -> (not (not X))
     ((and (eq then t) (null else))
      `(not ,(byte-opt--negate condition)))
     ;; (if VAR VAR X...) -> (or VAR (progn X...))
     ((and (symbolp condition) (eq condition then))
      `(or ,then ,(if (cdr else)
                      `(progn . ,else)
                    (car else))))
     ;; (if X THEN nil) -> (if X THEN)
     (then
      (if (equal else '(nil))
	  (list (car form) condition then)
	form))
     ;; (if X nil ELSE...) -> (if (not X) (progn ELSE...))
     ((or (car else) (cdr else))
      (list (car form) (byte-opt--negate condition)
	    (if (cdr else)
		`(progn . ,else)
	      (car else))))
     ;; (if X nil nil) -> (progn X nil)
     (t
      (list 'progn condition nil)))))

(defun byte-optimize-while (form)
  (let ((condition (nth 1 form)))
    (if (byte-compile-nilconstp condition)
        condition
      form)))

(defun byte-optimize-not (form)
  (if (= (length form) 2)
      (let ((arg (nth 1 form)))
        (cond ((null arg) t)
              ((macroexp-const-p arg) nil)
              ((byte-compile-nilconstp arg) `(progn ,arg t))
              ((byte-compile-trueconstp arg) `(progn ,arg nil))
              (t form)))
    form))

(put 'and   'byte-optimizer #'byte-optimize-and)
(put 'or    'byte-optimizer #'byte-optimize-or)
(put 'cond  'byte-optimizer #'byte-optimize-cond)
(put 'if    'byte-optimizer #'byte-optimize-if)
(put 'while 'byte-optimizer #'byte-optimize-while)
(put 'not   'byte-optimizer #'byte-optimize-not)
(put 'null  'byte-optimizer #'byte-optimize-not)

(defun byte-optimize-funcall (form)
  ;; (funcall #'(lambda ...) ...) -> (let ...)
  ;; (funcall #'SYM ...) -> (SYM ...)
  ;; (funcall 'SYM ...)  -> (SYM ...)
  (pcase form
    (`(,_ #'(lambda . ,_) . ,_)
     (macroexp--unfold-lambda form))
    (`(,_ ,(or `#',f `',(and f (pred symbolp))) . ,actuals)
     `(,f ,@actuals))
    (_ form)))

(defun byte-optimize-apply (form)
  (let ((len (length form)))
    ;; Single-arg `apply' is an abomination that we don't bother optimizing.
    (if (> len 2)
        (let ((fn (nth 1 form))
	      (last (nth (1- len) form)))
          (cond
           ;; (apply F ... '(X Y ...)) -> (funcall F ... 'X 'Y ...)
           ((or (null last)
                (eq (car-safe last) 'quote))
            (let ((last-value (nth 1 last)))
	      (if (listp last-value)
                  `(funcall ,fn ,@(butlast (cddr form))
                            ,@(mapcar (lambda (x) (list 'quote x)) last-value))
	        (byte-compile-warn-x
                 last "last arg to apply can't be a literal atom: `%s'" last)
	        nil)))
           ;; (apply F ... (list X Y ...)) -> (funcall F ... X Y ...)
           ((eq (car-safe last) 'list)
            `(funcall ,fn ,@(butlast (cddr form)) ,@(cdr last)))
           ;; (apply F ... (cons X Y)) -> (apply F ... X Y)
           ((eq (car-safe last) 'cons)
            (append (butlast form) (cdr last)))
           (t form)))
      form)))

(put 'funcall 'byte-optimizer #'byte-optimize-funcall)
(put 'apply   'byte-optimizer #'byte-optimize-apply)


(put 'let 'byte-optimizer #'byte-optimize-letX)
(put 'let* 'byte-optimizer #'byte-optimize-letX)
(defun byte-optimize-letX (form)
  (pcase form
    ;; Bindings list is empty.
    (`(,_ () . ,body)
     `(progn . ,body))

    ;; Body is empty or just contains a constant.
    (`(,head ,bindings . ,(or '() `(,(and const (pred macroexp-const-p)))))
     (if (eq head 'let)
         `(progn ,@(mapcar #'cadr bindings) ,const)
       `(,head ,(butlast bindings) ,(cadar (last bindings)) ,const)))

    ;; Body does nothing but return the last variable in bindings.
    (`(,head ,(and bindings
                   (let last-var (caar (last bindings))))
             ,(and last-var             ; non-linear pattern
                   (pred symbolp) (pred (not keywordp)) (pred (not booleanp))))
     (if (eq head 'let)
         `(progn ,@(mapcar #'cadr bindings))
       `(,head ,(butlast bindings) ,(cadar (last bindings)))))

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
      (let ((count (nth 1 form)))
        (cond ((and (integerp count) (<= count 3))
	       (setq form (nth 2 form))
	       (while (>= (setq count (1- count)) 0)
	         (setq form (list 'cdr form)))
	       form)
              ((not (eq (car form) 'nthcdr))
               (cons 'nthcdr (cdr form)))  ; use the nthcdr byte-op
              (t form)))
    form))

(put 'cons 'byte-optimizer #'byte-optimize-cons)
(defun byte-optimize-cons (form)
  (let ((tail (nth 2 form)))
    (cond
     ;; (cons X nil) => (list X)
     ((null tail) `(list ,(nth 1 form)))
     ;; (cons X (list YS...)) -> (list X YS...)
     ((and (consp tail) (eq (car tail) 'list))
      `(,(car tail) ,(nth 1 form) . ,(cdr tail)))
     (t form))))

(put 'list 'byte-optimizer #'byte-optimize-list)
(defun byte-optimize-list (form)
  ;; (list) -> nil
  (and (cdr form) form))

(put 'nconc 'byte-optimizer #'byte-optimize-nconc)
(defun byte-optimize-nconc (form)
  (pcase (cdr form)
    ('nil nil)                          ; (nconc) -> nil
    (`(,x) x)                           ; (nconc X) -> X
    (_ (named-let loop ((args (cdr form)) (newargs nil))
         (if args
             (let ((arg (car args))
                   (prev (car newargs)))
               (cond
                ;; Elide null args.
                ((and (null arg)
                      ;; Don't elide a terminal nil unless preceded by
                      ;; a nonempty proper list, since that will have
                      ;; its last cdr forced to nil.
                      (or (cdr args)
                          ;; FIXME: prove the 'nonempty proper list' property
                          ;; for more forms than just `list', such as
                          ;; `append', `mapcar' etc.
                          (eq 'list (car-safe (car newargs)))))
                 (loop (cdr args) newargs))
                ;; Merge consecutive `list' args.
                ((and (eq (car-safe arg) 'list)
                      (eq (car-safe prev) 'list))
                 (loop (cons (cons (car prev) (append (cdr prev) (cdr arg)))
                             (cdr args))
                       (cdr newargs)))
                ;; (nconc ... (list A) B ...) -> (nconc ... (cons A B) ...)
                ((and (eq (car-safe prev) 'list) (cdr prev) (null (cddr prev)))
                 (loop (cdr args)
                       (cons (list 'cons (cadr prev) arg)
                             (cdr newargs))))
                (t (loop (cdr args) (cons arg newargs)))))
           (let ((new-form (cons (car form) (nreverse newargs))))
             (if (equal new-form form)
                 form
               new-form)))))))

(put 'append 'byte-optimizer #'byte-optimize-append)
(defun byte-optimize-append (form)
  ;; There is (probably) too much code relying on `append' to return a
  ;; new list for us to do full constant-folding; these transformations
  ;; preserve the allocation semantics.
  (and (cdr form)                       ; (append) -> nil
       (named-let loop ((args (cdr form)) (newargs nil))
         (let ((arg (car args))
               (prev (car newargs)))
           (cond
            ;; Flatten nested `append' forms.
            ((and (consp arg) (eq (car arg) 'append))
             (loop (append (cdr arg) (cdr args)) newargs))

            ;; Merge consecutive `list' forms.
            ((and (consp arg) (eq (car arg) 'list)
                  newargs (consp prev) (eq (car prev) 'list))
             (loop (cons (cons (car prev) (append (cdr prev) (cdr arg)))
                         (cdr args))
                   (cdr newargs)))

            ;; non-terminal arg
            ((cdr args)
             (cond
              ((macroexp-const-p arg)
               ;; constant arg
               (let ((val (byteopt--eval-const arg)))
                 (cond
                  ;; Elide empty arguments (nil, empty string, etc).
                  ((zerop (length val))
                   (loop (cdr args) newargs))
                  ;; Merge consecutive constants.
                  ((and newargs (macroexp-const-p prev))
                   (loop (cdr args)
                         (cons
                          (list 'quote
                                (append (byteopt--eval-const prev) val nil))
                          (cdr newargs))))
                  (t (loop (cdr args) (cons arg newargs))))))

              ;; (list CONSTANTS...) -> '(CONSTANTS...)
              ((and (consp arg) (eq (car arg) 'list)
                    (all #'macroexp-const-p (cdr arg)))
               (loop (cons (list 'quote (eval arg)) (cdr args)) newargs))

              (t (loop (cdr args) (cons arg newargs)))))

            ;; At this point, `arg' is the last (tail) argument.

            ;; (append X) -> X
            ((null newargs) arg)

            ;; (append ... (list Xs...) nil) -> (append ... (list Xs...))
            ((and (null arg) (eq (car-safe prev) 'list))
             (cons (car form) (nreverse newargs)))

            ;; (append '(X) Y)     -> (cons 'X Y)
            ;; (append (list X) Y) -> (cons X Y)
            ((and newargs (null (cdr newargs))
                  (consp prev)
                  (cond ((eq (car prev) 'quote)
                         (and (consp (cadr prev))
                              (= (length (cadr prev)) 1)))
                        ((eq (car prev) 'list)
                         (= (length (cdr prev)) 1))))
             `(cons ,(if (eq (car prev) 'quote)
                         (macroexp-quote (caadr prev))
                       (cadr prev))
                    ,arg))

            (t
             (let ((new-form (cons (car form) (nreverse (cons arg newargs)))))
               (if (equal new-form form)
                   form
                 new-form))))))))

;; Fixme: delete-char -> delete-region (byte-coded)

(put 'set 'byte-optimizer #'byte-optimize-set)
(defun byte-optimize-set (form)
  (pcase (cdr form)
    ;; Make sure we only turn `set' into `setq' for dynamic variables.
    (`((quote ,(and var (guard (and (symbolp var)
                                    (not (macroexp--const-symbol-p var))
                                    (not (assq var byte-optimize--lexvars))))))
       ,newval)
     `(setq ,var ,newval))
    (`(,(and ml `(make-local-variable ,(and v `(quote ,_)))) ,newval)
     `(progn ,ml (,(car form) ,v ,newval)))
    (_ form)))

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
       '(
         ;; alloc.c
         make-bool-vector make-byte-code make-list make-record make-string
         make-symbol make-vector
         ;; buffer.c
         buffer-base-buffer buffer-chars-modified-tick buffer-file-name
         buffer-local-value buffer-local-variables buffer-modified-p
         buffer-modified-tick buffer-name get-buffer next-overlay-change
         overlay-buffer overlay-end overlay-get overlay-properties
         overlay-start overlays-at overlays-in previous-overlay-change
         ;; callint.c
         prefix-numeric-value
         ;; casefiddle.c
         capitalize downcase upcase upcase-initials
         ;; category.c
         category-docstring category-set-mnemonics char-category-set
         copy-category-table get-unused-category make-category-set
         ;; character.c
         char-width get-byte multibyte-char-to-unibyte string string-width
         unibyte-char-to-multibyte unibyte-string
         ;; charset.c
         decode-char encode-char
         ;; chartab.c
         make-char-table
         ;; data.c
         % * + - / /= 1+ 1- < <= = > >=
         aref ash bare-symbol
         bool-vector-count-consecutive bool-vector-count-population
         bool-vector-subsetp
         boundp car cdr default-boundp default-value fboundp
         get-variable-watchers indirect-variable
         local-variable-if-set-p local-variable-p
         logand logcount logior lognot logxor max min mod
         number-to-string position-symbol string-to-number
         subr-arity subr-name subr-native-lambda-list subr-type
         symbol-function symbol-name symbol-plist symbol-value
         symbol-with-pos-pos variable-binding-locus
         ;; doc.c
         documentation
         ;; editfns.c
         buffer-substring buffer-substring-no-properties
         byte-to-position byte-to-string
         char-after char-before char-equal char-to-string
         compare-buffer-substrings
         format format-message
         group-name
         line-beginning-position line-end-position ngettext pos-bol pos-eol
         propertize region-beginning region-end string-to-char
         user-full-name user-login-name
         ;; eval.c
         special-variable-p
         ;; fileio.c
         car-less-than-car directory-name-p file-directory-p file-exists-p
         file-name-absolute-p file-name-concat file-newer-than-file-p
         file-readable-p file-symlink-p file-writable-p
         ;; filelock.c
         file-locked-p
         ;; floatfns.c
         abs acos asin atan ceiling copysign cos exp expt fceiling ffloor
         float floor frexp fround ftruncate isnan ldexp log logb round
         sin sqrt tan
         truncate
         ;; fns.c
         append assq
         base64-decode-string base64-encode-string base64url-encode-string
         buffer-hash buffer-line-statistics
         compare-strings concat copy-alist copy-hash-table copy-sequence elt
         equal equal-including-properties
         featurep get
         gethash hash-table-count hash-table-rehash-size
         hash-table-rehash-threshold hash-table-size hash-table-test
         hash-table-weakness
         length length< length= length>
         line-number-at-pos load-average locale-info make-hash-table md5
         member memq memql nth nthcdr
         object-intervals rassoc rassq reverse secure-hash
         string-as-multibyte string-as-unibyte string-bytes
         string-collate-equalp string-collate-lessp string-distance
         string-equal string-lessp string-make-multibyte string-make-unibyte
         string-search string-to-multibyte string-to-unibyte
         string-version-lessp
         substring substring-no-properties
         sxhash-eq sxhash-eql sxhash-equal sxhash-equal-including-properties
         take value< vconcat
         ;; frame.c
         frame-ancestor-p frame-bottom-divider-width frame-char-height
         frame-char-width frame-child-frame-border-width frame-focus
         frame-fringe-width frame-internal-border-width frame-native-height
         frame-native-width frame-parameter frame-parameters frame-parent
         frame-pointer-visible-p frame-position frame-right-divider-width
         frame-scale-factor frame-scroll-bar-height frame-scroll-bar-width
         frame-text-cols frame-text-height frame-text-lines frame-text-width
         frame-total-cols frame-total-lines frame-visible-p
         frame-window-state-change next-frame previous-frame
         tool-bar-pixel-width window-system
         ;; fringe.c
         fringe-bitmaps-at-pos
         ;; json.c
         json-serialize json-parse-string
         ;; keyboard.c
         posn-at-point posn-at-x-y
         ;; keymap.c
         copy-keymap keymap-parent keymap-prompt make-keymap make-sparse-keymap
         ;; lread.c
         intern-soft read-from-string
         ;; marker.c
         copy-marker marker-buffer marker-insertion-type marker-position
         ;; minibuf.c
         active-minibuffer-window assoc-string innermost-minibuffer-p
         minibuffer-innermost-command-loop-p minibufferp
         ;; print.c
         error-message-string prin1-to-string
         ;; process.c
         format-network-address get-buffer-process get-process
         process-buffer process-coding-system process-command process-filter
         process-id process-inherit-coding-system-flag process-mark
         process-name process-plist process-query-on-exit-flag
         process-running-child-p process-sentinel process-thread
         process-tty-name process-type
         ;; search.c
         match-beginning match-end regexp-quote
         ;; sqlite.c
         sqlite-columns sqlite-more-p sqlite-version
         ;; syntax.c
         char-syntax copy-syntax-table matching-paren string-to-syntax
         syntax-class-to-char
         ;; term.c
         controlling-tty-p tty-display-color-cells tty-display-color-p
         tty-top-frame tty-type
         ;; terminal.c
         frame-terminal terminal-list terminal-live-p terminal-name
         terminal-parameter terminal-parameters
         ;; textprop.c
         get-char-property get-char-property-and-overlay get-text-property
         next-char-property-change next-property-change
         next-single-char-property-change next-single-property-change
         previous-char-property-change previous-property-change
         previous-single-char-property-change previous-single-property-change
         text-properties-at text-property-any text-property-not-all
         ;; thread.c
         all-threads condition-mutex condition-name mutex-name thread-live-p
         thread-name
         ;; timefns.c
         current-cpu-time
         current-time-string current-time-zone decode-time encode-time
         float-time format-time-string time-add time-convert time-equal-p
         time-less-p time-subtract
         ;; window.c
         coordinates-in-window-p frame-first-window frame-root-window
         frame-selected-window get-buffer-window minibuffer-selected-window
         minibuffer-window next-window previous-window window-at
         window-body-height window-body-width window-buffer
         window-combination-limit window-configuration-equal-p
         window-dedicated-p window-display-table window-frame window-fringes
         window-hscroll window-left-child window-left-column window-margins
         window-minibuffer-p window-new-normal window-new-total
         window-next-buffers window-next-sibling window-normal-size
         window-parameter window-parameters window-parent window-point
         window-prev-buffers window-prev-sibling window-scroll-bars
         window-start window-text-height window-top-child window-top-line
         window-total-height window-total-width window-use-time window-vscroll
         ;; xdisp.c
         buffer-text-pixel-size current-bidi-paragraph-direction
         get-display-property invisible-p line-pixel-height lookup-image-map
         tab-bar-height tool-bar-height window-text-pixel-size
         ))
      (side-effect-and-error-free-fns
       '(
         ;; alloc.c
         bool-vector cons list make-marker record vector
         ;; buffer.c
         buffer-list buffer-live-p current-buffer overlay-lists overlayp
         ;; casetab.c
         case-table-p current-case-table standard-case-table
         ;; category.c
         category-table category-table-p make-category-table
         standard-category-table
         ;; character.c
         characterp max-char
         ;; charset.c
         charsetp
         ;; data.c
         arrayp atom bare-symbol-p bool-vector-p bufferp byte-code-function-p
         interpreted-function-p closurep
         byteorder car-safe cdr-safe char-or-string-p char-table-p
         condition-variable-p consp eq floatp indirect-function
         integer-or-marker-p integerp keywordp listp markerp
         module-function-p multibyte-string-p mutexp native-comp-function-p
         natnump nlistp null
         number-or-marker-p numberp recordp remove-pos-from-symbol
         sequencep stringp subrp symbol-with-pos-p symbolp
         threadp type-of user-ptrp vector-or-char-table-p vectorp wholenump
         ;; editfns.c
         bobp bolp buffer-size buffer-string current-message emacs-pid
         eobp eolp following-char gap-position gap-size group-gid
         group-real-gid mark-marker point point-marker point-max point-min
         position-bytes preceding-char system-name
         user-real-login-name user-real-uid user-uid
         ;; emacs.c
         invocation-directory invocation-name
         ;; eval.c
         commandp functionp
         ;; fileio.c
         default-file-modes
         ;; fns.c
         eql
         hash-table-p identity proper-list-p safe-length
         secure-hash-algorithms
         ;; frame.c
         frame-list frame-live-p framep last-nonminibuffer-frame
         old-selected-frame selected-frame visible-frame-list
         ;; image.c
         imagep
         ;; indent.c
         current-column current-indentation
         ;; keyboard.c
         current-idle-time current-input-mode recent-keys recursion-depth
         this-command-keys this-command-keys-vector this-single-command-keys
         this-single-command-raw-keys
         ;; keymap.c
         current-global-map current-local-map current-minor-mode-maps keymapp
         ;; minibuf.c
         minibuffer-contents minibuffer-contents-no-properties minibuffer-depth
         minibuffer-prompt minibuffer-prompt-end
         ;; process.c
         process-list processp signal-names waiting-for-user-input-p
         ;; sqlite.c
         sqlite-available-p sqlitep
         ;; syntax.c
         standard-syntax-table syntax-table syntax-table-p
         ;; thread.c
         current-thread
         ;; timefns.c
         current-time
         ;; window.c
         selected-window window-configuration-p window-live-p window-valid-p
         windowp
         ;; xdisp.c
         long-line-optimizations-p
         )))
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
       '(
         ;; character.c
         characterp max-char
         ;; data.c
         % * + - / /= 1+ 1- < <= = > >= aref arrayp ash atom bare-symbol
         bool-vector-count-consecutive bool-vector-count-population
         bool-vector-p bool-vector-subsetp
         bufferp car car-safe cdr cdr-safe char-or-string-p char-table-p
         condition-variable-p consp eq floatp integer-or-marker-p integerp
         keywordp listp logand logcount logior lognot logxor markerp max min
         mod multibyte-string-p mutexp natnump nlistp null number-or-marker-p
         numberp recordp remove-pos-from-symbol sequencep stringp symbol-name
         symbolp threadp type-of vector-or-char-table-p vectorp
         ;; editfns.c
         string-to-char
         ;; floatfns.c
         abs ceiling copysign fceiling ffloor float floor fround ftruncate
         isnan ldexp logb round sqrt truncate
         ;; fns.c
         assq base64-decode-string base64-encode-string base64url-encode-string
         concat elt eql equal equal-including-properties
         hash-table-p identity length length< length=
         length> member memq memql nth nthcdr proper-list-p rassoc rassq
         safe-length string-bytes string-distance string-equal string-lessp
         string-search string-version-lessp take value<
         ;; json.c
         json-serialize json-parse-string
         ;; search.c
         regexp-quote
         ;; syntax.c
         string-to-syntax
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

(eval-when-compile
  (defconst byte-opt--side-effect-and-error-free-ops
    '( byte-stack-ref byte-constant byte-dup byte-symbolp byte-consp
       byte-stringp byte-listp byte-integerp byte-numberp byte-eq byte-not
       byte-car-safe byte-cdr-safe
       byte-cons byte-list1 byte-list2 byte-list3 byte-list4 byte-listN
       byte-point byte-point-max byte-point-min
       byte-following-char byte-preceding-char
       byte-current-column byte-eolp byte-eobp byte-bolp byte-bobp
       byte-current-buffer))

  (defconst byte-opt--side-effect-free-ops
    (append
     '( byte-varref byte-nth byte-memq byte-car byte-cdr byte-length byte-aref
        byte-symbol-value byte-get byte-concat2 byte-concat3 byte-sub1 byte-add1
        byte-eqlsign byte-equal byte-gtr byte-lss byte-leq byte-geq byte-diff
        byte-negate byte-plus byte-max byte-min byte-mult byte-char-after
        byte-char-syntax byte-buffer-substring byte-string= byte-string<
        byte-nthcdr byte-elt byte-member byte-assq byte-quo byte-rem
        byte-substring)
     byte-opt--side-effect-and-error-free-ops))
  )

(defun byte-optimize-lapcode (lap &optional _for-effect)
  "Simple peephole optimizer.  LAP is both modified and returned.
If FOR-EFFECT is non-nil, the return value is assumed to be of no importance."
  (let* ((side-effect-free
          (if byte-compile-delete-errors
	      (eval-when-compile byte-opt--side-effect-free-ops)
	    (eval-when-compile byte-opt--side-effect-and-error-free-ops)))

         (conditional-ops
          '( byte-goto-if-nil byte-goto-if-not-nil byte-goto-if-nil-else-pop
             byte-goto-if-not-nil-else-pop))
         (conditional-or-discard-ops (cons 'byte-discard conditional-ops))

         ;; Ops that can be sunk past an unbind.
         ;; This means they have to commute with anything else, which rules
         ;; out ones like `byte-car-safe' and `byte-equal'.
         ;; In particular, `byte-eq' and `byte-symbolp' aren't here despite
         ;; being nominally pure because they are currently affected by
         ;; `symbols-with-pos-enabled'.  Yes, this is unsatisfactory.
         (after-unbind-ops
          '( byte-constant byte-dup byte-stack-ref byte-stack-set byte-discard
             byte-discardN byte-discardN-preserve-tos
             byte-consp byte-stringp byte-listp byte-numberp
             byte-integerp byte-not
             byte-cons byte-list1 byte-list2 byte-list3 byte-list4 byte-listN))

         ;; Ops taking and produce a single value on the stack.
         (unary-ops '( byte-not byte-length byte-list1 byte-nreverse
                       byte-car byte-cdr byte-car-safe byte-cdr-safe
                       byte-symbolp byte-consp byte-stringp
                       byte-listp byte-integerp byte-numberp
                       byte-add1 byte-sub1 byte-negate
                       ;; There are more of these but the list is
                       ;; getting long and the gain is typically small.
                       ))
         ;; Ops producing a single result without looking at the stack.
         (producer-ops '( byte-constant byte-varref
                          byte-point byte-point-max byte-point-min
                          byte-following-char byte-preceding-char
                          byte-current-column
                          byte-eolp byte-eobp byte-bolp byte-bobp
                          byte-current-buffer byte-widen))
	 (add-depth 0)
	 (keep-going 'first-time)
         ;; Create a cons cell as head of the list so that removing the first
         ;; element does not need special-casing: `setcdr' always works.
         (lap-head (cons nil lap)))
    (while keep-going
      (byte-compile-log-lap "  ---- %s pass"
                            (if (eq keep-going 'first-time) "first" "next"))
      (setq keep-going nil)
      (let ((prev lap-head))
        (while (cdr prev)
          (let* ((rest (cdr prev))
	         (lap0 (car rest))
	         (lap1 (nth 1 rest))
	         (lap2 (nth 2 rest)))

	    ;; You may notice that sequences like "dup varset discard" are
	    ;; optimized but sequences like "dup varset TAG1: discard" are not.
	    ;; You may be tempted to change this; resist that temptation.

            ;; Each clause in this `cond' statement must keep `prev' the
            ;; predecessor of the remainder of the list for inspection.
	    (cond
             ;;
             ;; PUSH(K) discard(N) -->  <deleted> discard(N-K), N>K
             ;; PUSH(K) discard(N) -->  <deleted>,              N=K
             ;;  where PUSH(K) is a side-effect-free op such as
             ;;  const, varref, dup
             ;;
             ((and (memq (car lap1) '(byte-discard byte-discardN))
	           (memq (car lap0) side-effect-free))
	      (setq keep-going t)
              (let* ((pushes (aref byte-stack+-info (symbol-value (car lap0))))
                     (pops (if (eq (car lap1) 'byte-discardN) (cdr lap1) 1))
                     (net-pops (- pops pushes)))
                (cond ((= net-pops 0)
                       (byte-compile-log-lap "  %s %s\t-->\t<deleted>"
                                             lap0 lap1)
                       (setcdr prev (cddr rest)))
                      ((> net-pops 0)
                       (byte-compile-log-lap
                        "  %s %s\t-->\t<deleted> discard(%d)"
                        lap0 lap1 net-pops)
                       (setcar rest (if (eql net-pops 1)
                                        (cons 'byte-discard nil)
                                      (cons 'byte-discardN net-pops)))
                       (setcdr rest (cddr rest)))
                      (t (error "Optimizer error: too much on the stack")))))
	     ;;
	     ;; goto(X)              X:  -->          X:
             ;; goto-if-[not-]nil(X) X:  -->  discard X:
	     ;;
	     ((and (memq (car lap0) byte-goto-ops)
	           (eq (cdr lap0) lap1))
	      (cond ((eq (car lap0) 'byte-goto)
	             (byte-compile-log-lap "  %s %s\t-->\t<deleted> %s"
                                           lap0 lap1 lap1)
                     (setcdr prev (cdr rest)))
		    ((memq (car lap0) byte-goto-always-pop-ops)
	             (byte-compile-log-lap "  %s %s\t-->\tdiscard %s"
                                           lap0 lap1 lap1)
		     (setcar lap0 'byte-discard)
		     (setcdr lap0 0))
                    ;; goto-*-else-pop(X) cannot occur here because it would
                    ;; be a depth conflict.
		    (t (error "Depth conflict at tag %d" (nth 2 lap0))))
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
                   (memq (car lap1) '(byte-varset byte-varbind))
                   ;; Can't optimize away varref for DEFVAR_BOOL vars
                   ;; because what we put in might not be what we get out.
                   (let ((tmp (memq (car (cdr lap2)) byte-boolean-vars)))
                     (and
                      (not (and tmp (not (eq (car lap0) 'byte-constant))))
                      (progn
	                (setq keep-going t)
                        (if (memq (car lap0) '(byte-constant byte-dup))
                            (let ((tmp (if (or (not tmp)
                                               (macroexp--const-symbol-p
                                                (car (cdr lap0))))
                                           (cdr lap0)
                                         (byte-compile-get-constant t))))
		              (byte-compile-log-lap "  %s %s %s\t-->\t%s %s %s"
				                    lap0 lap1 lap2 lap0 lap1
				                    (cons (car lap0) tmp))
		              (setcar lap2 (car lap0))
		              (setcdr lap2 tmp))
	                  (byte-compile-log-lap "  %s %s\t-->\tdup %s"
                                                lap1 lap2 lap1)
	                  (setcar lap2 (car lap1))
	                  (setcar lap1 'byte-dup)
	                  (setcdr lap1 0)
	                  ;; The stack depth gets locally increased, so we will
	                  ;; increase maxdepth in case depth = maxdepth here.
	                  ;; This can cause the third argument to byte-code to
	                  ;; be larger than necessary.
	                  (setq add-depth 1))
                        t)))))
             ;;
             ;; dup varset discard(N)       --> varset discard(N-1)
             ;; dup varbind discard(N)      --> varbind discard(N-1)
             ;; dup stack-set(M) discard(N) --> stack-set(M-1) discard(N-1), M>1
             ;; (the varbind variant can emerge from other optimizations)
             ;;
             ((and (eq 'byte-dup (car lap0))
                   (memq (car lap2) '(byte-discard byte-discardN))
                   (or (memq (car lap1) '(byte-varset byte-varbind))
                       (and (eq (car lap1) 'byte-stack-set)
                            (> (cdr lap1) 1))))
              (setcdr prev (cdr rest))          ; remove dup
              (let ((new1 (if (eq (car lap1) 'byte-stack-set)
                              (cons 'byte-stack-set (1- (cdr lap1)))
                            lap1))
                    (n (if (eq (car lap2) 'byte-discard) 1 (cdr lap2))))
                (setcar (cdr rest) new1)
                (cl-assert (> n 0))
                (cond
                 ((> n 1)
                  (let ((new2 (if (> n 2)
                                  (cons 'byte-discardN (1- n))
                                (cons 'byte-discard nil))))
                    (byte-compile-log-lap "  %s %s %s\t-->\t%s %s"
                                          lap0 lap1 lap2 new1 new2)
                    (setcar (cddr rest) new2)))
                 (t
                  (byte-compile-log-lap "  %s %s %s\t-->\t%s"
                                        lap0 lap1 lap2 new1)
                  ;; discard(0) = nop, remove
                  (setcdr (cdr rest) (cdddr rest)))))
              (setq keep-going t))

	     ;;
	     ;; not goto-X-if-nil              -->  goto-X-if-non-nil
	     ;; not goto-X-if-non-nil          -->  goto-X-if-nil
	     ;;
	     ;; it is wrong to do the same thing for the -else-pop variants.
	     ;;
	     ((and (eq 'byte-not (car lap0))
	           (memq (car lap1) '(byte-goto-if-nil byte-goto-if-not-nil)))
              (let ((not-goto (if (eq (car lap1) 'byte-goto-if-nil)
			          'byte-goto-if-not-nil
			        'byte-goto-if-nil)))
	        (byte-compile-log-lap "  not %s\t-->\t%s"
                                      lap1 (cons not-goto (cdr lap1)))
	        (setcar lap1 not-goto)
                (setcdr prev (cdr rest))    ; delete not
	        (setq keep-going t)))
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
	        (byte-compile-log-lap "  %s %s %s\t-->\t%s %s"
				      lap0 lap1 lap2
				      (cons inverse (cdr lap1)) lap2)
                (setcdr prev (cdr rest))
	        (setcar lap1 inverse)
	        (setq keep-going t)))
	     ;;
	     ;; const goto-if-* --> whatever
	     ;;
	     ((and (eq 'byte-constant (car lap0))
	           (memq (car lap1) conditional-ops)
                   ;; Must be an actual constant, not a closure variable.
                   (consp (cdr lap0)))
	      (cond ((if (memq (car lap1) '(byte-goto-if-nil
                                            byte-goto-if-nil-else-pop))
                         (car (cdr lap0))
                       (not (car (cdr lap0))))
                     ;; Branch not taken.
		     (byte-compile-log-lap "  %s %s\t-->\t<deleted>"
				           lap0 lap1)
                     (setcdr prev (cddr rest))) ; delete both
		    ((memq (car lap1) byte-goto-always-pop-ops)
                     ;; Always-pop branch taken.
		     (byte-compile-log-lap "  %s %s\t-->\t%s"
				           lap0 lap1
				           (cons 'byte-goto (cdr lap1)))
                     (setcdr prev (cdr rest)) ; delete const
		     (setcar lap1 'byte-goto))
                    (t  ; -else-pop branch taken: keep const
		     (byte-compile-log-lap "  %s %s\t-->\t%s %s"
                                           lap0 lap1
                                           lap0 (cons 'byte-goto (cdr lap1)))
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
                   (let ((tmp (cdr rest))
                         (tmp2 0))
		     (while (eq (car (car tmp)) 'byte-dup)
		       (setq tmp2 (1+ tmp2))
                       (setq tmp (cdr tmp)))
		     (and (eq (if (eq 'byte-stack-ref (car lap0))
                                  (+ tmp2 1 (cdr lap0))
                                (cdr lap0))
                              (cdr (car tmp)))
	                  (eq (car lap0) (car (car tmp)))
                          (progn
	                    (when (memq byte-optimize-log '(t byte))
	                      (let ((str "")
		                    (tmp2 (cdr rest)))
	                        (while (not (eq tmp tmp2))
		                  (setq tmp2 (cdr tmp2))
                                  (setq str (concat str " dup")))
	                        (byte-compile-log-lap "  %s%s %s\t-->\t%s%s dup"
				                      lap0 str lap0 lap0 str)))
	                    (setq keep-going t)
	                    (setcar (car tmp) 'byte-dup)
	                    (setcdr (car tmp) 0)
                            t)))))
	     ;;
	     ;; TAG1: TAG2: --> <deleted> TAG2:
	     ;; (and other references to TAG1 are replaced with TAG2)
	     ;;
	     ((and (eq (car lap0) 'TAG)
	           (eq (car lap1) 'TAG))
	      (byte-compile-log-lap "  adjacent tags %d and %d merged"
				    (nth 1 lap1) (nth 1 lap0))
              (let ((tmp3 (cdr lap-head)))
	        (while (let ((tmp2 (rassq lap0 tmp3)))
                         (and tmp2
	                      (progn
                                (setcdr tmp2 lap1)
	                        (setq tmp3 (cdr (memq tmp2 tmp3)))
                                t))))
                (setcdr prev (cdr rest))
	        (setq keep-going t)
                ;; replace references to tag in jump tables, if any
                (dolist (table byte-compile-jump-tables)
                  (maphash #'(lambda (value tag)
                               (when (equal tag lap0)
                                 (puthash value lap1 table)))
                           table))))
	     ;;
	     ;; unused-TAG: --> <deleted>
	     ;;
	     ((and (eq 'TAG (car lap0))
	           (not (rassq lap0 (cdr lap-head)))
                   ;; make sure this tag isn't used in a jump-table
                   (cl-loop for table in byte-compile-jump-tables
                            when (member lap0 (hash-table-values table))
                            return nil finally return t))
	      (byte-compile-log-lap "  unused tag %d removed" (nth 1 lap0))
              (setcdr prev (cdr rest))
              (setq keep-going t))
	     ;;
	     ;; goto   ... --> goto   <delete until TAG or end>
	     ;; return ... --> return <delete until TAG or end>
             ;;
	     ((and (memq (car lap0) '(byte-goto byte-return))
	           (not (memq (car lap1) '(TAG nil))))
	      (let ((i 0)
                    (tmp rest)
		    (opt-p (memq byte-optimize-log '(t byte)))
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
	        (setcdr rest tmp)
	        (setq keep-going t)))
	     ;;
	     ;; <safe-op> unbind --> unbind <safe-op>
	     ;; (this may enable other optimizations.)
	     ;;
	     ((and (eq 'byte-unbind (car lap1))
	           (memq (car lap0) after-unbind-ops))
	      (byte-compile-log-lap "  %s %s\t-->\t%s %s" lap0 lap1 lap1 lap0)
	      (setcar rest lap1)
	      (setcar (cdr rest) lap0)
	      (setq keep-going t))
	     ;;
	     ;; varbind-X unbind-N            -->  discard unbind-(N-1)
	     ;; save-excursion unbind-N       -->  unbind-(N-1)
	     ;; save-restriction unbind-N     -->  unbind-(N-1)
	     ;; save-current-buffer unbind-N  -->  unbind-(N-1)
	     ;;
	     ((and (eq 'byte-unbind (car lap1))
	           (memq (car lap0) '(byte-varbind byte-save-excursion
				                   byte-save-restriction
                                                   byte-save-current-buffer))
	           (< 0 (cdr lap1)))
              (setcdr lap1 (1- (cdr lap1)))
	      (when (zerop (cdr lap1))
                (setcdr rest (cddr rest)))
	      (if (eq (car lap0) 'byte-varbind)
	          (setcar rest (cons 'byte-discard 0))
                (setcdr prev (cddr prev)))
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
                   (let ((tmp (nth 1 (memq (cdr lap0) (cdr lap-head)))))
                     (and
	              (memq (car tmp) '(byte-goto byte-return))
                      (or (eq (car lap0) 'byte-goto)
		          (eq (car tmp) 'byte-goto))
                      (not (eq (cdr tmp) (cdr lap0)))
                      (progn
	                (byte-compile-log-lap "  %s [%s]\t-->\t%s"
                                              (car lap0) tmp
                                              (if (eq (car tmp) 'byte-return)
                                                  tmp
                                                (cons (car lap0) (cdr tmp))))
	                (when (eq (car tmp) 'byte-return)
	                  (setcar lap0 'byte-return))
	                (setcdr lap0 (cdr tmp))
	                (setq keep-going t)
                        t)))))

             ;;
             ;; OP goto(X) Y: OP X: -> Y: OP X:
             ;;
             ((and (eq (car lap1) 'byte-goto)
                   (eq (car lap2) 'TAG)
                   (let ((lap3 (nth 3 rest)))
                     (and (eq (car lap0) (car lap3))
                          (eq (cdr lap0) (cdr lap3))
                          (eq (cdr lap1) (nth 4 rest)))))
              (byte-compile-log-lap "  %s %s %s %s %s\t-->\t%s %s %s"
                                    lap0 lap1 lap2
                                    (nth 3 rest)  (nth 4 rest)
                                    lap2 (nth 3 rest) (nth 4 rest))
              (setcdr prev (cddr rest))
              (setq keep-going t))

             ;;
             ;; NOEFFECT PRODUCER return  -->  PRODUCER return
             ;;  where NOEFFECT lacks effects beyond stack change,
             ;;        PRODUCER pushes a result without looking at the stack:
             ;;                 const, varref, point etc.
             ;;
             ((and (eq (car (nth 2 rest)) 'byte-return)
                   (memq (car lap1) producer-ops)
                   (or (memq (car lap0) '( byte-discard byte-discardN
                                           byte-discardN-preserve-tos
                                           byte-stack-set))
                       (memq (car lap0) side-effect-free)))
              (setq keep-going t)
              (setq add-depth 1)
              (setcdr prev (cdr rest))
              (byte-compile-log-lap "  %s %s %s\t-->\t%s %s"
                                    lap0 lap1 (nth 2 rest) lap1 (nth 2 rest)))

             ;;
             ;; (discardN-preserve-tos|dup) UNARY return  -->  UNARY return
             ;;  where UNARY takes and produces a single value on the stack
             ;;
             ;; FIXME: ideally we should run this backwards, so that we could do
             ;;   discardN-preserve-tos OP1...OPn return -> OP1..OPn return
             ;; but that would require a different approach.
             ;;
             ((and (eq (car (nth 2 rest)) 'byte-return)
                   (memq (car lap1) unary-ops)
                   (or (memq (car lap0) '(byte-discardN-preserve-tos byte-dup))
                       (and (eq (car lap0) 'byte-stack-set)
                            (eql (cdr lap0) 1))))
              (setq keep-going t)
              (setcdr prev (cdr rest))  ; eat lap0
              (byte-compile-log-lap "  %s %s %s\t-->\t%s %s"
                                    lap0 lap1 (nth 2 rest) lap1 (nth 2 rest)))

	     ;;
	     ;; goto-*-else-pop X ... X: goto-if-* --> whatever
	     ;; goto-*-else-pop X ... X: discard --> whatever
	     ;;
	     ((and (memq (car lap0) '(byte-goto-if-nil-else-pop
				      byte-goto-if-not-nil-else-pop))
                   (let ((tmp (cdr (memq (cdr lap0) (cdr lap-head)))))
                     (and
	              (memq (caar tmp) conditional-or-discard-ops)
	              (not (eq lap0 (car tmp)))
                      (let ((tmp2 (car tmp))
                            (tmp3 (assq (car lap0)
                                        '((byte-goto-if-nil-else-pop
					   byte-goto-if-nil)
					  (byte-goto-if-not-nil-else-pop
					   byte-goto-if-not-nil)))))
	                (if (memq (car tmp2) tmp3)
	                    (progn (setcar lap0 (car tmp2))
		                   (setcdr lap0 (cdr tmp2))
		                   (byte-compile-log-lap
                                    "  %s-else-pop [%s]\t-->\t%s"
				    (car lap0) tmp2 lap0))
	                  ;; Get rid of the -else-pop's and jump one
	                  ;; step further.
	                  (or (eq 'TAG (car (nth 1 tmp)))
		              (setcdr tmp (cons (byte-compile-make-tag)
				                (cdr tmp))))
	                  (byte-compile-log-lap "  %s [%s]\t-->\t%s <skip>"
				                (car lap0) tmp2 (nth 1 tmp3))
	                  (setcar lap0 (nth 1 tmp3))
	                  (setcdr lap0 (nth 1 tmp)))
	                (setq keep-going t)
                        t)))))
	     ;;
	     ;; const goto-X ... X: goto-if-* --> whatever
	     ;; const goto-X ... X: discard   --> whatever
	     ;;
	     ((and (eq (car lap0) 'byte-constant)
	           (eq (car lap1) 'byte-goto)
                   (let ((tmp (cdr (memq (cdr lap1) (cdr lap-head)))))
                     (and
	              (memq (caar tmp) conditional-or-discard-ops)
	              (not (eq lap1 (car tmp)))
	              (let ((tmp2 (car tmp)))
	                (cond ((and (consp (cdr lap0))
		                    (memq (car tmp2)
			                  (if (null (car (cdr lap0)))
			                      '(byte-goto-if-nil
                                                byte-goto-if-nil-else-pop)
			                    '(byte-goto-if-not-nil
			                      byte-goto-if-not-nil-else-pop))))
		               (byte-compile-log-lap
                                "  %s goto [%s]\t-->\t%s %s"
                                lap0 tmp2 lap0 tmp2)
		               (setcar lap1 (car tmp2))
		               (setcdr lap1 (cdr tmp2))
		               ;; Let next step fix the (const,goto-if*) seq.
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
                               (setcdr prev (cdr rest))
		               (setq keep-going t))
                              (t
                               (setq prev (cdr prev))))
                        t)))))
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
                   (let ((tmp (cdr (memq (cdr lap2) (cdr lap-head)))))
                     (and
	              (eq (car (car tmp)) 'byte-varref)
	              (eq (cdr (car tmp)) (cdr lap1))
                      ;; Can't optimize away varref for DEFVAR_BOOL vars
                      ;; because what we put in might not be what we get out.
	              (not (memq (car (cdr lap1)) byte-boolean-vars))
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
	                (setcdr tmp (cons (setcdr lap2 newtag) (cdr tmp)))
	                (setq add-depth 1)
	                (setq keep-going t)
                        t)))))
             ;;
             ;;    goto(X)          Y: ... X: goto-if*(Y)
             ;; -> goto-if-not-*(Z) Y: ... X: goto-if*(Y) Z:
             ;;
             ;;    goto(X)                Y: ... X: goto-if-nil-else-pop(Y)
             ;; -> goto-if-not-nil(Z) nil Y: ... X: goto-if-nil-else-pop(Y) Z:
             ;;
             ;; where in both cases the first jump may go either
             ;; forwards or backwards.  The purpose is to move a conditional
             ;; branch from the top to the botton of a loop, but it only works
             ;; when other transforms have prepared the ground first.
	     ;;
	     ((and (eq (car lap0) 'byte-goto)
	           (eq (car lap1) 'TAG)
                   (let* ((tail (cdr (memq (cdr lap0) (cdr lap-head))))
                          (branch (car tail)))
                     (and
	              (eq lap1 (cdr branch))
	              (memq (car branch)
		            '( byte-goto-if-nil byte-goto-if-not-nil
		               byte-goto-if-nil-else-pop))
	              (let ((newtag (byte-compile-make-tag))
                            (new-jmp (cdr (assq (car branch)
			                        '((byte-goto-if-nil
                                                   . byte-goto-if-not-nil)
				                  (byte-goto-if-not-nil
                                                   . byte-goto-if-nil)
				                  (byte-goto-if-nil-else-pop
                                                   . byte-goto-if-not-nil)))))
                            ;; Rematerialise nil value if needed.
		            ;; (We can't handle goto-if-not-nil-else-pop
		            ;; because we wouldn't know which non-nil
                            ;; constant to push.)
                            (new-const
                             (and (eq (car branch) 'byte-goto-if-nil-else-pop)
                                  (cons 'byte-constant
					(byte-compile-get-constant nil))))
                            )
	                (byte-compile-log-lap
	                 "  %s %s ... %s %s\t-->\t%s %s %s ... %s %s %s"
	                 lap0 lap1 (cdr lap0) branch
	                 (cons new-jmp newtag) (or new-const "") lap1
                         (cdr lap0) branch newtag)
	                (setcdr tail (cons (setcdr lap0 newtag) (cdr tail)))
	                (when new-const
		          (setcdr rest (cons new-const (cdr rest))))
	                (setcar lap0 new-jmp)
	                (setq keep-going t)
                        t)))))

             ;;
             ;; discardN-preserve-tos(X) discardN-preserve-tos(Y)
             ;; --> discardN-preserve-tos(X+Y)
             ;;  where stack-set(1) is accepted as discardN-preserve-tos(1)
             ;;
             ((and (or (eq (car lap0) 'byte-discardN-preserve-tos)
                       (and (eq (car lap0) 'byte-stack-set)
                            (eql (cdr lap0) 1)))
                   (or (eq (car lap1) 'byte-discardN-preserve-tos)
                       (and (eq (car lap1) 'byte-stack-set)
                            (eql (cdr lap1) 1))))
              (setq keep-going t)
              (let ((new-op (cons 'byte-discardN-preserve-tos
                                  ;; This happens to work even when either
                                  ;; op is stack-set(1).
                                  (+ (cdr lap0) (cdr lap1)))))
                (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1 new-op)
                (setcar rest new-op)
                (setcdr rest (cddr rest))))

	     ;;
	     ;; stack-set-M [discard/discardN ...]  -->  discardN-preserve-tos
	     ;; stack-set-M [discard/discardN ...]  -->  discardN
	     ;;
	     ((and (eq (car lap0) 'byte-stack-set)
	           (memq (car lap1) '(byte-discard byte-discardN))
                   (let ((tmp2 (1- (cdr lap0)))
                         (tmp3 0)
                         (tmp (cdr rest)))
	             ;; See if enough discard operations follow to expose or
	             ;; destroy the value stored by the stack-set.
	             (while (memq (car (car tmp)) '(byte-discard byte-discardN))
	               (setq tmp3
                             (+ tmp3 (if (eq (car (car tmp)) 'byte-discard)
                                         1
                                       (cdr (car tmp)))))
	               (setq tmp (cdr tmp)))
                     (and
	              (>= tmp3 tmp2)
                      (progn
	                ;; Do the optimization.
                        (setcdr prev (cdr rest))
                        (setcar lap1
                                (if (= tmp2 tmp3)
                                    ;; The value stored is the new TOS, so pop
                                    ;; one more value (to get rid of the old
                                    ;; value) using TOS-preserving discard.
                                    'byte-discardN-preserve-tos
                                  ;; Otherwise, the value stored is lost,
                                  ;; so just use a normal discard.
                                  'byte-discardN))
                        (setcdr lap1 (1+ tmp3))
	                (setcdr (cdr rest) tmp)
	                (byte-compile-log-lap
                         "  %s [discard/discardN]...\t-->\t%s" lap0 lap1)
                        (setq keep-going t)
                        t
                        )))))

	     ;;
	     ;; discardN-preserve-tos return  -->  return
	     ;; dup return  -->  return
	     ;; stack-set(1) return  -->  return
	     ;;
	     ((and (eq (car lap1) 'byte-return)
	           (or (memq (car lap0) '(byte-discardN-preserve-tos byte-dup))
	               (and (eq (car lap0) 'byte-stack-set)
	                    (= (cdr lap0) 1))))
	      (setq keep-going t)
	      ;; The byte-code interpreter will pop the stack for us, so
	      ;; we can just leave stuff on it.
	      (setcdr prev (cdr rest))
	      (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1 lap1))

             ;;
             ;;     stack-ref(X) discardN-preserve-tos(Y)
             ;; --> discard(Y) stack-ref(X-Y),                X≥Y
             ;;     discard(X) discardN-preserve-tos(Y-X-1),  X<Y
             ;; where: stack-ref(0) = dup  (works both ways)
             ;;        discard(0) = no-op
             ;;        discardN-preserve-tos(0) = no-op
             ;;
	     ((and (memq (car lap0) '(byte-stack-ref byte-dup))
	           (or (eq (car lap1) 'byte-discardN-preserve-tos)
	               (and (eq (car lap1) 'byte-stack-set)
	                    (eql (cdr lap1) 1)))
                   ;; Don't apply if immediately preceding a `return',
                   ;; since there are more effective rules for that case.
                   (not (eq (car lap2) 'byte-return)))
              (let ((x (if (eq (car lap0) 'byte-dup) 0 (cdr lap0)))
                    (y (cdr lap1)))
                (cl-assert (> y 0))
                (cond
                 ((>= x y)              ; --> discard(Y) stack-ref(X-Y)
                  (let ((new0 (if (= y 1)
                                  (cons 'byte-discard nil)
                                (cons 'byte-discardN y)))
                        (new1 (if (= x y)
                                  (cons 'byte-dup nil)
                                (cons 'byte-stack-ref (- x y)))))
	            (byte-compile-log-lap "  %s %s\t-->\t%s %s"
                                          lap0 lap1 new0 new1)
                    (setcar rest new0)
                    (setcar (cdr rest) new1)))
                 ((= x 0)               ; --> discardN-preserve-tos(Y-1)
                  (setcdr prev (cdr rest))  ; eat lap0
                  (if (> y 1)
                      (let ((new (cons 'byte-discardN-preserve-tos (- y 1))))
                        (byte-compile-log-lap "  %s %s\t-->\t%s"
                                              lap0 lap1 new)
                        (setcar (cdr prev) new))
                    (byte-compile-log-lap "  %s %s\t-->\t<deleted>" lap0 lap1)
                    (setcdr prev (cddr prev))))  ; eat lap1
                 ((= y (+ x 1))         ; --> discard(X)
                  (setcdr prev (cdr rest))  ; eat lap0
                  (let ((new (if (= x 1)
                                 (cons 'byte-discard nil)
                               (cons 'byte-discardN x))))
                    (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1 new)
                    (setcar (cdr prev) new)))
                 (t               ; --> discard(X) discardN-preserve-tos(Y-X-1)
                  (let ((new0 (if (= x 1)
                                  (cons 'byte-discard nil)
                                (cons 'byte-discardN x)))
                        (new1 (cons 'byte-discardN-preserve-tos (- y x 1))))
	            (byte-compile-log-lap "  %s %s\t-->\t%s %s"
                                          lap0 lap1 new0 new1)
                    (setcar rest new0)
                    (setcar (cdr rest) new1)))))
              (setq keep-going t))

	     ;;
	     ;; goto-X ... X: discard  ==>  discard goto-Y ... X: discard Y:
	     ;;
	     ((and (eq (car lap0) 'byte-goto)
                   (let ((tmp (cdr (memq (cdr lap0) (cdr lap-head)))))
                     (and
                      tmp
                      (or (memq (caar tmp) '(byte-discard byte-discardN))
                          ;; Make sure we don't hoist a discardN-preserve-tos
                          ;; that really should be merged or deleted instead.
                          (and (or (eq (caar tmp) 'byte-discardN-preserve-tos)
                                   (and (eq (caar tmp) 'byte-stack-set)
                                        (eql (cdar tmp) 1)))
                               (let ((next (cadr tmp)))
                                 (not (or (memq (car next)
                                                '(byte-discardN-preserve-tos
                                                  byte-return))
                                          (and (eq (car next) 'byte-stack-set)
                                               (eql (cdr next) 1)))))))
                      (progn
	                (byte-compile-log-lap
	                 "  goto-X .. X: \t-->\t%s goto-X.. X: %s Y:"
	                 (car tmp) (car tmp))
	                (setq keep-going t)
	                (let* ((newtag (byte-compile-make-tag))
	                       ;; Make a copy, since we sometimes modify
	                       ;; insts in-place!
	                       (newdiscard (cons (caar tmp) (cdar tmp)))
	                       (newjmp (cons (car lap0) newtag)))
                          ;; Push new tag after the discard.
	                  (push newtag (cdr tmp))
	                  (setcar rest newdiscard)
	                  (push newjmp (cdr rest)))
                        t)))))

             ;;
             ;; UNARY discardN-preserve-tos --> discardN-preserve-tos UNARY
             ;;  where UNARY takes and produces a single value on the stack
             ;;
             ((and (memq (car lap0) unary-ops)
	           (or (eq (car lap1) 'byte-discardN-preserve-tos)
                       (and (eq (car lap1) 'byte-stack-set)
                            (eql (cdr lap1) 1)))
                   ;; unless followed by return (which will eat the discard)
                   (not (eq (car lap2) 'byte-return)))
	      (setq keep-going t)
	      (byte-compile-log-lap "  %s %s\t-->\t%s %s" lap0 lap1 lap1 lap0)
	      (setcar rest lap1)
	      (setcar (cdr rest) lap0))

	     ;;
	     ;; PRODUCER discardN-preserve-tos(X) --> discard(X) PRODUCER
             ;;  where PRODUCER pushes a result without looking at the stack:
             ;;                 const, varref, point etc.
	     ;;
	     ((and (memq (car lap0) producer-ops)
	           (or (eq (car lap1) 'byte-discardN-preserve-tos)
                       (and (eq (car lap1) 'byte-stack-set)
                            (eql (cdr lap1) 1)))
                   ;; unless followed by return (which will eat the discard)
                   (not (eq (car lap2) 'byte-return)))
	      (setq keep-going t)
              (let ((newdiscard (if (eql (cdr lap1) 1)
                                    (cons 'byte-discard nil)
                                  (cons 'byte-discardN (cdr lap1)))))
	        (byte-compile-log-lap
	         "  %s %s\t-->\t%s %s" lap0 lap1 newdiscard lap0)
	        (setf (car rest) newdiscard)
	        (setf (cadr rest) lap0)))

             (t
              ;; If no rule matched, advance and try again.
              (setq prev (cdr prev))))))))
    ;; Cleanup stage:
    ;; Rebuild byte-compile-constants / byte-compile-variables.
    ;; Simple optimizations that would inhibit other optimizations if they
    ;; were done in the optimizing loop, and optimizations which there is no
    ;; need to do more than once.
    (setq byte-compile-constants nil
	  byte-compile-variables nil)
    (byte-compile-log-lap "  ---- final pass")
    (let ((prev lap-head))
      (while (cdr prev)
        (let* ((rest (cdr prev))
               (lap0 (car rest))
	       (lap1 (nth 1 rest)))
          ;; FIXME: Would there ever be a `byte-constant2' op here?
          (if (memq (car lap0) byte-constref-ops)
	      (if (memq (car lap0) '(byte-constant byte-constant2))
	          (unless (memq (cdr lap0) byte-compile-constants)
		    (setq byte-compile-constants (cons (cdr lap0)
						       byte-compile-constants)))
	        (unless (memq (cdr lap0) byte-compile-variables)
	          (setq byte-compile-variables (cons (cdr lap0)
						     byte-compile-variables)))))
          (cond
           ;;
	   ;; const-C varset-X const-C  -->  const-C dup varset-X
	   ;; const-C varbind-X const-C  -->  const-C dup varbind-X
	   ;;
	   ((and (eq (car lap0) 'byte-constant)
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
	    (let ((tmp rest)
		  (tmp2 nil))
	      (while (progn
		       (while (eq 'byte-dup (car (car (setq tmp (cdr tmp))))))
		       (and (eq (cdr lap0) (cdr (car tmp)))
			    (eq (car lap0) (car (car tmp)))))
	        (setcar tmp (cons 'byte-dup 0))
	        (setq tmp2 t))
	      (if tmp2
		  (byte-compile-log-lap
		   "  %s [dup/%s]...\t-->\t%s dup..." lap0 lap0 lap0)
                (setq prev (cdr prev)))))
	   ;;
	   ;; unbind-N unbind-M  -->  unbind-(N+M)
	   ;;
	   ((and (eq 'byte-unbind (car lap0))
		 (eq 'byte-unbind (car lap1)))
	    (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1
				  (cons 'byte-unbind
					(+ (cdr lap0) (cdr lap1))))
	    (setcdr prev (cdr rest))
	    (setcdr lap1 (+ (cdr lap1) (cdr lap0))))

	   ;;
	   ;; discard/discardN/discardN-preserve-tos-X discard/discardN-Y  -->
	   ;; discardN-(X+Y)
	   ;;
	   ((and (memq (car lap0)
		       '(byte-discard byte-discardN
			              byte-discardN-preserve-tos))
		 (memq (car lap1) '(byte-discard byte-discardN)))
	    (setcdr prev (cdr rest))
	    (byte-compile-log-lap
	     "  %s %s\t-->\t(discardN %s)"
	     lap0 lap1
	     (+ (if (eq (car lap0) 'byte-discard) 1 (cdr lap0))
		(if (eq (car lap1) 'byte-discard) 1 (cdr lap1))))
	    (setcdr lap1 (+ (if (eq (car lap0) 'byte-discard) 1 (cdr lap0))
			    (if (eq (car lap1) 'byte-discard) 1 (cdr lap1))))
	    (setcar lap1 'byte-discardN))
           (t
            (setq prev (cdr prev)))))))
    (setq byte-compile-maxdepth (+ byte-compile-maxdepth add-depth))
    (cdr lap-head)))

(provide 'byte-opt)


;; To avoid "lisp nesting exceeds max-lisp-eval-depth" when this file compiles
;; itself, compile some of its most used recursive functions (at load time).
;;
(eval-when-compile
 (or (compiled-function-p (symbol-function 'byte-optimize-form))
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
