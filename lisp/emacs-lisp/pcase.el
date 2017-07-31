;;; pcase.el --- ML-style pattern-matching macro for Elisp -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

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

;; ML-style pattern matching.
;; The entry points are autoloaded.

;; Todo:

;; - (pcase e (`(,x . ,x) foo)) signals an "x unused" warning if `foo' doesn't
;;   use x, because x is bound separately for the equality constraint
;;   (as well as any pred/guard) and for the body, so uses at one place don't
;;   count for the other.
;; - provide ways to extend the set of primitives, with some kind of
;;   define-pcase-matcher.  We could easily make it so that (guard BOOLEXP)
;;   could be defined this way, as a shorthand for (pred (lambda (_) BOOLEXP)).
;;   But better would be if we could define new ways to match by having the
;;   extension provide its own `pcase--split-<foo>' thingy.
;; - along these lines, provide patterns to match CL structs.
;; - provide something like (setq VAR) so a var can be set rather than
;;   let-bound.
;; - provide a way to fallthrough to subsequent cases (not sure what I meant by
;;   this :-()
;; - try and be more clever to reduce the size of the decision tree, and
;;   to reduce the number of leaves that need to be turned into function:
;;   - first, do the tests shared by all remaining branches (it will have
;;     to be performed anyway, so better do it first so it's shared).
;;   - then choose the test that discriminates more (?).
;; - provide Agda's `with' (along with its `...' companion).
;; - implement (not PAT).  This might require a significant redesign.
;; - ideally we'd want (pcase s ((re RE1) E1) ((re RE2) E2)) to be able to
;;   generate a lex-style DFA to decide whether to run E1 or E2.

;;; Code:

(require 'macroexp)

;; Macro-expansion of pcase is reasonably fast, so it's not a problem
;; when byte-compiling a file, but when interpreting the code, if the pcase
;; is in a loop, the repeated macro-expansion becomes terribly costly, so we
;; memoize previous macro expansions to try and avoid recomputing them
;; over and over again.
;; FIXME: Now that macroexpansion is also performed when loading an interpreted
;; file, this is not a real problem any more.
(defconst pcase--memoize (make-hash-table :weakness 'key :test 'eq))
;; (defconst pcase--memoize-1 (make-hash-table :test 'eq))
;; (defconst pcase--memoize-2 (make-hash-table :weakness 'key :test 'equal))

(defconst pcase--dontcare-upats '(t _ pcase--dontcare))

(defvar pcase--dontwarn-upats '(pcase--dontcare))

(def-edebug-spec
  pcase-PAT
  (&or symbolp
       ("or" &rest pcase-PAT)
       ("and" &rest pcase-PAT)
       ("guard" form)
       ("let" pcase-PAT form)
       ("pred" pcase-FUN)
       ("app" pcase-FUN pcase-PAT)
       pcase-MACRO
       sexp))

(def-edebug-spec
  pcase-FUN
  (&or lambda-expr
       ;; Punt on macros/special forms.
       (functionp &rest form)
       sexp))

;; See bug#24717
(put 'pcase-MACRO 'edebug-form-spec 'pcase--edebug-match-macro)

;; Only called from edebug.
(declare-function get-edebug-spec "edebug" (symbol))
(declare-function edebug-match "edebug" (cursor specs))

(defun pcase--edebug-match-macro (cursor)
  (let (specs)
    (mapatoms
     (lambda (s)
       (let ((m (get s 'pcase-macroexpander)))
	 (when (and m (get-edebug-spec m))
	   (push (cons (symbol-name s) (get-edebug-spec m))
		 specs)))))
    (edebug-match cursor (cons '&or specs))))

;;;###autoload
(defmacro pcase (exp &rest cases)
  "Evaluate EXP and attempt to match it against structural patterns.
CASES is a list of elements of the form (PATTERN CODE...).

A structural PATTERN describes a template that identifies a class
of values.  For example, the pattern \\=`(,foo ,bar) matches any
two element list, binding its elements to symbols named `foo' and
`bar' -- in much the same way that `cl-destructuring-bind' would.

A significant difference from `cl-destructuring-bind' is that, if
a pattern match fails, the next case is tried until either a
successful match is found or there are no more cases.

Another difference is that pattern elements may be quoted,
meaning they must match exactly: The pattern \\='(foo bar)
matches only against two element lists containing the symbols
`foo' and `bar' in that order.  (As a short-hand, atoms always
match themselves, such as numbers or strings, and need not be
quoted.)

Lastly, a pattern can be logical, such as (pred numberp), that
matches any number-like element; or the symbol `_', that matches
anything.  Also, when patterns are backquoted, a comma may be
used to introduce logical patterns inside backquoted patterns.

The complete list of standard patterns is as follows:

  _		matches anything.
  SYMBOL	matches anything and binds it to SYMBOL.
                If a SYMBOL is used twice in the same pattern
                the second occurrence becomes an `eq'uality test.
  (or PAT...)	matches if any of the patterns matches.
  (and PAT...)	matches if all the patterns match.
  \\='VAL		matches if the object is `equal' to VAL.
  ATOM		is a shorthand for \\='ATOM.
		   ATOM can be a keyword, an integer, or a string.
  (pred FUN)	matches if FUN applied to the object returns non-nil.
  (guard BOOLEXP)	matches if BOOLEXP evaluates to non-nil.
  (let PAT EXP)	matches if EXP matches PAT.
  (app FUN PAT)	matches if FUN applied to the object matches PAT.

Additional patterns can be defined using `pcase-defmacro'.

The FUN argument in the `app' pattern may have the following forms:
  SYMBOL or (lambda ARGS BODY)  in which case it's called with one argument.
  (F ARG1 .. ARGn) in which case F gets called with an n+1'th argument
                        which is the value being matched.
So a FUN of the form SYMBOL is equivalent to (FUN).
FUN can refer to variables bound earlier in the pattern.

See Info node `(elisp) Pattern matching case statement' in the
Emacs Lisp manual for more information and examples."
  (declare (indent 1) (debug (form &rest (pcase-PAT body))))
  ;; We want to use a weak hash table as a cache, but the key will unavoidably
  ;; be based on `exp' and `cases', yet `cases' is a fresh new list each time
  ;; we're called so it'll be immediately GC'd.  So we use (car cases) as key
  ;; which does come straight from the source code and should hence not be GC'd
  ;; so easily.
  (let ((data (gethash (car cases) pcase--memoize)))
    ;; data = (EXP CASES . EXPANSION)
    (if (and (equal exp (car data)) (equal cases (cadr data)))
        ;; We have the right expansion.
        (cddr data)
      ;; (when (gethash (car cases) pcase--memoize-1)
      ;;   (message "pcase-memoize failed because of weak key!!"))
      ;; (when (gethash (car cases) pcase--memoize-2)
      ;;   (message "pcase-memoize failed because of eq test on %S"
      ;;            (car cases)))
      ;; (when data
      ;;   (message "pcase-memoize: equal first branch, yet different"))
      (let ((expansion (pcase--expand exp cases)))
        (puthash (car cases) `(,exp ,cases ,@expansion) pcase--memoize)
        ;; (puthash (car cases) `(,exp ,cases ,@expansion) pcase--memoize-1)
        ;; (puthash (car cases) `(,exp ,cases ,@expansion) pcase--memoize-2)
        expansion))))

(declare-function help-fns--signature "help-fns"
                  (function doc real-def real-function buffer))

;; FIXME: Obviously, this will collide with nadvice's use of
;; function-documentation if we happen to advise `pcase'.
(put 'pcase 'function-documentation '(pcase--make-docstring))
(defun pcase--make-docstring ()
  (let* ((main (documentation (symbol-function 'pcase) 'raw))
         (ud (help-split-fundoc main 'pcase)))
    ;; So that eg emacs -Q -l cl-lib --eval "(documentation 'pcase)" works,
    ;; where cl-lib is anything using pcase-defmacro.
    (require 'help-fns)
    (with-temp-buffer
      (insert (or (cdr ud) main))
      (mapatoms
       (lambda (symbol)
         (let ((me (get symbol 'pcase-macroexpander)))
           (when me
             (insert "\n\n-- ")
             (let* ((doc (documentation me 'raw)))
               (setq doc (help-fns--signature symbol doc me
                                              (indirect-function me) nil))
               (insert "\n" (or doc "Not documented.")))))))
      (let ((combined-doc (buffer-string)))
        (if ud (help-add-fundoc-usage combined-doc (car ud)) combined-doc)))))

;;;###autoload
(defmacro pcase-exhaustive (exp &rest cases)
  "The exhaustive version of `pcase' (which see)."
  (declare (indent 1) (debug pcase))
  (let* ((x (make-symbol "x"))
         (pcase--dontwarn-upats (cons x pcase--dontwarn-upats)))
    (pcase--expand
     ;; FIXME: Could we add the FILE:LINE data in the error message?
     exp (append cases `((,x (error "No clause matching `%S'" ,x)))))))

;;;###autoload
(defmacro pcase-lambda (lambda-list &rest body)
  "Like `lambda' but allow each argument to be a pattern.
I.e. accepts the usual &optional and &rest keywords, but every
formal argument can be any pattern accepted by `pcase' (a mere
variable name being but a special case of it)."
  (declare (doc-string 2) (indent defun)
           (debug ((&rest pcase-PAT) body)))
  (let* ((bindings ())
         (parsed-body (macroexp-parse-body body))
         (args (mapcar (lambda (pat)
                         (if (symbolp pat)
                             ;; Simple vars and &rest/&optional are just passed
                             ;; through unchanged.
                             pat
                           (let ((arg (make-symbol
                                       (format "arg%s" (length bindings)))))
                             (push `(,pat ,arg) bindings)
                             arg)))
                       lambda-list)))
    `(lambda ,args ,@(car parsed-body)
       (pcase-let* ,(nreverse bindings) ,@(cdr parsed-body)))))

(defun pcase--let* (bindings body)
  (cond
   ((null bindings) (macroexp-progn body))
   ((pcase--trivial-upat-p (caar bindings))
    (macroexp-let* `(,(car bindings)) (pcase--let* (cdr bindings) body)))
   (t
    (let ((binding (pop bindings)))
      (pcase--expand
       (cadr binding)
       `((,(car binding) ,(pcase--let* bindings body))
         ;; We can either signal an error here, or just use `pcase--dontcare'
         ;; which generates more efficient code.  In practice, if we use
         ;; `pcase--dontcare' we will still often get an error and the few
         ;; cases where we don't do not matter that much, so
         ;; it's a better choice.
         (pcase--dontcare nil)))))))

;;;###autoload
(defmacro pcase-let* (bindings &rest body)
  "Like `let*' but where you can use `pcase' patterns for bindings.
BODY should be an expression, and BINDINGS should be a list of bindings
of the form (PAT EXP)."
  (declare (indent 1)
           (debug ((&rest (pcase-PAT &optional form)) body)))
  (let ((cached (gethash bindings pcase--memoize)))
    ;; cached = (BODY . EXPANSION)
    (if (equal (car cached) body)
        (cdr cached)
      (let ((expansion (pcase--let* bindings body)))
        (puthash bindings (cons body expansion) pcase--memoize)
        expansion))))

;;;###autoload
(defmacro pcase-let (bindings &rest body)
  "Like `let' but where you can use `pcase' patterns for bindings.
BODY should be a list of expressions, and BINDINGS should be a list of bindings
of the form (PAT EXP).
The macro is expanded and optimized under the assumption that those
patterns *will* match, so a mismatch may go undetected or may cause
any kind of error."
  (declare (indent 1) (debug pcase-let*))
  (if (null (cdr bindings))
      `(pcase-let* ,bindings ,@body)
    (let ((matches '()))
      (dolist (binding (prog1 bindings (setq bindings nil)))
        (cond
         ((memq (car binding) pcase--dontcare-upats)
          (push (cons (make-symbol "_") (cdr binding)) bindings))
         ((pcase--trivial-upat-p (car binding)) (push binding bindings))
         (t
          (let ((tmpvar (make-symbol (format "x%d" (length bindings)))))
            (push (cons tmpvar (cdr binding)) bindings)
            (push (list (car binding) tmpvar) matches)))))
      `(let ,(nreverse bindings) (pcase-let* ,matches ,@body)))))

;;;###autoload
(defmacro pcase-dolist (spec &rest body)
  "Like `dolist' but where the binding can be a `pcase' pattern.
\n(fn (PATTERN LIST) BODY...)"
  (declare (indent 1) (debug ((pcase-PAT form) body)))
  (if (pcase--trivial-upat-p (car spec))
      `(dolist ,spec ,@body)
    (let ((tmpvar (make-symbol "x")))
      `(dolist (,tmpvar ,@(cdr spec))
         (pcase-let* ((,(car spec) ,tmpvar))
           ,@body)))))


(defun pcase--trivial-upat-p (upat)
  (and (symbolp upat) (not (memq upat pcase--dontcare-upats))))

(defun pcase--expand (exp cases)
  ;; (message "pid=%S (pcase--expand %S ...hash=%S)"
  ;;          (emacs-pid) exp (sxhash cases))
  (macroexp-let2 macroexp-copyable-p val exp
    (let* ((defs ())
           (seen '())
           (codegen
            (lambda (code vars)
              (let ((prev (assq code seen)))
                (if (not prev)
                    (let ((res (pcase-codegen code vars)))
                      (push (list code vars res) seen)
                      res)
                  ;; Since we use a tree-based pattern matching
                  ;; technique, the leaves (the places that contain the
                  ;; code to run once a pattern is matched) can get
                  ;; copied a very large number of times, so to avoid
                  ;; code explosion, we need to keep track of how many
                  ;; times we've used each leaf and move it
                  ;; to a separate function if that number is too high.
                  ;;
                  ;; We've already used this branch.  So it is shared.
                  (let* ((code (car prev))         (cdrprev (cdr prev))
                         (prevvars (car cdrprev))  (cddrprev (cdr cdrprev))
                         (res (car cddrprev)))
                    (unless (symbolp res)
                      ;; This is the first repeat, so we have to move
                      ;; the branch to a separate function.
                      (let ((bsym
                             (make-symbol (format "pcase-%d" (length defs)))))
                        (push `(,bsym (lambda ,(mapcar #'car prevvars) ,@code))
                              defs)
                        (setcar res 'funcall)
                        (setcdr res (cons bsym (mapcar #'cdr prevvars)))
                        (setcar (cddr prev) bsym)
                        (setq res bsym)))
                    (setq vars (copy-sequence vars))
                    (let ((args (mapcar (lambda (pa)
                                          (let ((v (assq (car pa) vars)))
                                            (setq vars (delq v vars))
                                            (cdr v)))
                                        prevvars)))
                      ;; If some of `vars' were not found in `prevvars', that's
                      ;; OK it just means those vars aren't present in all
                      ;; branches, so they can be used within the pattern
                      ;; (e.g. by a `guard/let/pred') but not in the branch.
                      ;; FIXME: But if some of `prevvars' are not in `vars' we
                      ;; should remove them from `prevvars'!
                      `(funcall ,res ,@args)))))))
           (used-cases ())
           (main
            (pcase--u
             (mapcar (lambda (case)
                       `(,(pcase--match val (pcase--macroexpand (car case)))
                         ,(lambda (vars)
                            (unless (memq case used-cases)
                              ;; Keep track of the cases that are used.
                              (push case used-cases))
                            (funcall
                             (if (pcase--small-branch-p (cdr case))
                                 ;; Don't bother sharing multiple
                                 ;; occurrences of this leaf since it's small.
                                 #'pcase-codegen codegen)
                             (cdr case)
                             vars))))
                     cases))))
      (dolist (case cases)
        (unless (or (memq case used-cases)
                    (memq (car case) pcase--dontwarn-upats))
          (message "Redundant pcase pattern: %S" (car case))))
      (macroexp-let* defs main))))

(defun pcase--macroexpand (pat)
  "Expands all macro-patterns in PAT."
  (let ((head (car-safe pat)))
    (cond
     ((null head)
      (if (pcase--self-quoting-p pat) `',pat pat))
     ((memq head '(pred guard quote)) pat)
     ((memq head '(or and)) `(,head ,@(mapcar #'pcase--macroexpand (cdr pat))))
     ((eq head 'let) `(let ,(pcase--macroexpand (cadr pat)) ,@(cddr pat)))
     ((eq head 'app) `(app ,(nth 1 pat) ,(pcase--macroexpand (nth 2 pat))))
     (t
      (let* ((expander (get head 'pcase-macroexpander))
             (npat (if expander (apply expander (cdr pat)))))
        (if (null npat)
            (error (if expander
                       "Unexpandable %s pattern: %S"
                     "Unknown %s pattern: %S")
                   head pat)
          (pcase--macroexpand npat)))))))

;;;###autoload
(defmacro pcase-defmacro (name args &rest body)
  "Define a new kind of pcase PATTERN, by macro expansion.
Patterns of the form (NAME ...) will be expanded according
to this macro."
  (declare (indent 2) (debug defun) (doc-string 3))
  ;; Add the function via `fsym', so that an autoload cookie placed
  ;; on a pcase-defmacro will cause the macro to be loaded on demand.
  (let ((fsym (intern (format "%s--pcase-macroexpander" name)))
	(decl (assq 'declare body)))
    (when decl (setq body (remove decl body)))
    `(progn
       (defun ,fsym ,args ,@body)
       (define-symbol-prop ',fsym 'edebug-form-spec ',(cadr (assq 'debug decl)))
       (define-symbol-prop ',name 'pcase-macroexpander #',fsym))))

(defun pcase--match (val upat)
  "Build a MATCH structure, hoisting all `or's and `and's outside."
  (cond
   ;; Hoist or/and patterns into or/and matches.
   ((memq (car-safe upat) '(or and))
    `(,(car upat)
      ,@(mapcar (lambda (upat)
                  (pcase--match val upat))
                (cdr upat))))
   (t
    `(match ,val . ,upat))))

(defun pcase-codegen (code vars)
  ;; Don't use let*, otherwise macroexp-let* may merge it with some surrounding
  ;; let* which might prevent the setcar/setcdr in pcase--expand's fancy
  ;; codegen from later metamorphosing this let into a funcall.
  (if vars
      `(let ,(mapcar (lambda (b) (list (car b) (cdr b))) vars)
         ,@code)
    `(progn ,@code)))

(defun pcase--small-branch-p (code)
  (and (= 1 (length code))
       (or (not (consp (car code)))
           (let ((small t))
             (dolist (e (car code))
               (if (consp e) (setq small nil)))
             small))))

;; Try to use `cond' rather than a sequence of `if's, so as to reduce
;; the depth of the generated tree.
(defun pcase--if (test then else)
  (cond
   ((eq else :pcase--dontcare) then)
   ((eq then :pcase--dontcare) (debug) else) ;Can/should this ever happen?
   (t (macroexp-if test then else))))

;; Note about MATCH:
;; When we have patterns like `(PAT1 . PAT2), after performing the `consp'
;; check, we want to turn all the similar patterns into ones of the form
;; (and (match car PAT1) (match cdr PAT2)), so you naturally need conjunction.
;; Earlier code hence used branches of the form (MATCHES . CODE) where
;; MATCHES was a list (implicitly a conjunction) of (SYM . PAT).
;; But if we have a pattern of the form (or `(PAT1 . PAT2) PAT3), there is
;; no easy way to eliminate the `consp' check in such a representation.
;; So we replaced the MATCHES by the MATCH below which can be made up
;; of conjunctions and disjunctions, so if we know `foo' is a cons, we can
;; turn (match foo . (or `(PAT1 . PAT2) PAT3)) into
;; (or (and (match car . `PAT1) (match cdr . `PAT2)) (match foo . PAT3)).
;; The downside is that we now have `or' and `and' both in MATCH and
;; in PAT, so there are different equivalent representations and we
;; need to handle them all.  We do not try to systematically
;; canonicalize them to one form over another, but we do occasionally
;; turn one into the other.

(defun pcase--u (branches)
  "Expand matcher for rules BRANCHES.
Each BRANCH has the form (MATCH CODE . VARS) where
CODE is the code generator for that branch.
VARS is the set of vars already bound by earlier matches.
MATCH is the pattern that needs to be matched, of the form:
  (match VAR . PAT)
  (and MATCH ...)
  (or MATCH ...)"
  (when (setq branches (delq nil branches))
    (let* ((carbranch (car branches))
           (match (car carbranch)) (cdarbranch (cdr carbranch))
           (code (car cdarbranch))
           (vars (cdr cdarbranch)))
      (pcase--u1 (list match) code vars (cdr branches)))))

(defun pcase--and (match matches)
  (if matches `(and ,match ,@matches) match))

(defconst pcase-mutually-exclusive-predicates
  '((symbolp . integerp)
    (symbolp . numberp)
    (symbolp . consp)
    (symbolp . arrayp)
    (symbolp . vectorp)
    (symbolp . stringp)
    (symbolp . byte-code-function-p)
    (symbolp . recordp)
    (integerp . consp)
    (integerp . arrayp)
    (integerp . vectorp)
    (integerp . stringp)
    (integerp . byte-code-function-p)
    (integerp . recordp)
    (numberp . consp)
    (numberp . arrayp)
    (numberp . vectorp)
    (numberp . stringp)
    (numberp . byte-code-function-p)
    (numberp . recordp)
    (consp . arrayp)
    (consp . atom)
    (consp . vectorp)
    (consp . stringp)
    (consp . byte-code-function-p)
    (consp . recordp)
    (arrayp . byte-code-function-p)
    (vectorp . byte-code-function-p)
    (vectorp . recordp)
    (stringp . vectorp)
    (stringp . recordp)
    (stringp . byte-code-function-p)))

(defun pcase--mutually-exclusive-p (pred1 pred2)
  (or (member (cons pred1 pred2)
              pcase-mutually-exclusive-predicates)
      (member (cons pred2 pred1)
              pcase-mutually-exclusive-predicates)))

(defun pcase--split-match (sym splitter match)
  (cond
    ((eq (car-safe match) 'match)
     (if (not (eq sym (cadr match)))
         (cons match match)
       (let ((res (funcall splitter (cddr match))))
         (cons (or (car res) match) (or (cdr res) match)))))
    ((memq (car-safe match) '(or and))
     (let ((then-alts '())
           (else-alts '())
           (neutral-elem (if (eq 'or (car match))
                             :pcase--fail :pcase--succeed))
           (zero-elem (if (eq 'or (car match)) :pcase--succeed :pcase--fail)))
       (dolist (alt (cdr match))
         (let ((split (pcase--split-match sym splitter alt)))
           (unless (eq (car split) neutral-elem)
             (push (car split) then-alts))
           (unless (eq (cdr split) neutral-elem)
             (push (cdr split) else-alts))))
       (cons (cond ((memq zero-elem then-alts) zero-elem)
                   ((null then-alts) neutral-elem)
                   ((null (cdr then-alts)) (car then-alts))
                   (t (cons (car match) (nreverse then-alts))))
             (cond ((memq zero-elem else-alts) zero-elem)
                   ((null else-alts) neutral-elem)
                   ((null (cdr else-alts)) (car else-alts))
                   (t (cons (car match) (nreverse else-alts)))))))
    ((memq match '(:pcase--succeed :pcase--fail)) (cons match match))
    (t (error "Uknown MATCH %s" match))))

(defun pcase--split-rest (sym splitter rest)
  (let ((then-rest '())
        (else-rest '()))
    (dolist (branch rest)
      (let* ((match (car branch))
             (code&vars (cdr branch))
             (split
              (pcase--split-match sym splitter match)))
        (unless (eq (car split) :pcase--fail)
          (push (cons (car split) code&vars) then-rest))
        (unless (eq (cdr split) :pcase--fail)
          (push (cons (cdr split) code&vars) else-rest))))
    (cons (nreverse then-rest) (nreverse else-rest))))

(defun pcase--split-equal (elem pat)
  (cond
   ;; The same match will give the same result.
   ((and (eq (car-safe pat) 'quote) (equal (cadr pat) elem))
    '(:pcase--succeed . :pcase--fail))
   ;; A different match will fail if this one succeeds.
   ((and (eq (car-safe pat) 'quote)
         ;; (or (integerp (cadr pat)) (symbolp (cadr pat))
         ;;     (consp (cadr pat)))
         )
    '(:pcase--fail . nil))
   ((and (eq (car-safe pat) 'pred)
         (symbolp (cadr pat))
         (get (cadr pat) 'side-effect-free))
    (ignore-errors
      (if (funcall (cadr pat) elem)
	  '(:pcase--succeed . nil)
	'(:pcase--fail . nil))))))

(defun pcase--split-member (elems pat)
  ;; FIXME: The new pred-based member code doesn't do these optimizations!
  ;; Based on pcase--split-equal.
  (cond
   ;; The same match (or a match of membership in a superset) will
   ;; give the same result, but we don't know how to check it.
   ;; (???
   ;;  '(:pcase--succeed . nil))
   ;; A match for one of the elements may succeed or fail.
   ((and (eq (car-safe pat) 'quote) (member (cadr pat) elems))
    nil)
   ;; A different match will fail if this one succeeds.
   ((and (eq (car-safe pat) 'quote)
         ;; (or (integerp (cadr pat)) (symbolp (cadr pat))
         ;;     (consp (cadr pat)))
         )
    '(:pcase--fail . nil))
   ((and (eq (car-safe pat) 'pred)
         (symbolp (cadr pat))
         (get (cadr pat) 'side-effect-free)
	 (ignore-errors
	   (let ((p (cadr pat)) (all t))
	     (dolist (elem elems)
	       (unless (funcall p elem) (setq all nil)))
	     all)))
    '(:pcase--succeed . nil))))

(defun pcase--split-pred (vars upat pat)
  (let (test)
    (cond
     ((and (equal upat pat)
           ;; For predicates like (pred (> a)), two such predicates may
           ;; actually refer to different variables `a'.
           (or (and (eq 'pred (car upat)) (symbolp (cadr upat)))
               ;; FIXME: `vars' gives us the environment in which `upat' will
               ;; run, but we don't have the environment in which `pat' will
               ;; run, so we can't do a reliable verification.  But let's try
               ;; and catch at least the easy cases such as (bug#14773).
               (not (pcase--fgrep (mapcar #'car vars) (cadr upat)))))
      '(:pcase--succeed . :pcase--fail))
     ((and (eq 'pred (car upat))
           (let ((otherpred
                  (cond ((eq 'pred (car-safe pat)) (cadr pat))
                        ((not (eq 'quote (car-safe pat))) nil)
                        ((consp (cadr pat)) #'consp)
                        ((stringp (cadr pat)) #'stringp)
                        ((vectorp (cadr pat)) #'vectorp)
                        ((byte-code-function-p (cadr pat))
                         #'byte-code-function-p))))
             (pcase--mutually-exclusive-p (cadr upat) otherpred)))
      '(:pcase--fail . nil))
     ((and (eq 'pred (car upat))
           (eq 'quote (car-safe pat))
           (symbolp (cadr upat))
           (or (symbolp (cadr pat)) (stringp (cadr pat)) (numberp (cadr pat)))
           (get (cadr upat) 'side-effect-free)
           (ignore-errors
             (setq test (list (funcall (cadr upat) (cadr pat))))))
      (if (car test)
          '(nil . :pcase--fail)
        '(:pcase--fail . nil))))))

(defun pcase--fgrep (vars sexp)
  "Check which of the symbols VARS appear in SEXP."
  (let ((res '()))
    (while (consp sexp)
      (dolist (var (pcase--fgrep vars (pop sexp)))
        (unless (memq var res) (push var res))))
    (and (memq sexp vars) (not (memq sexp res)) (push sexp res))
    res))

(defun pcase--self-quoting-p (upat)
  (or (keywordp upat) (integerp upat) (stringp upat)))

(defun pcase--app-subst-match (match sym fun nsym)
  (cond
   ((eq (car-safe match) 'match)
    (if (and (eq sym (cadr match))
             (eq 'app (car-safe (cddr match)))
             (equal fun (nth 1 (cddr match))))
        (pcase--match nsym (nth 2 (cddr match)))
      match))
   ((memq (car-safe match) '(or and))
    `(,(car match)
      ,@(mapcar (lambda (match)
                  (pcase--app-subst-match match sym fun nsym))
                (cdr match))))
   ((memq match '(:pcase--succeed :pcase--fail)) match)
   (t (error "Uknown MATCH %s" match))))

(defun pcase--app-subst-rest (rest sym fun nsym)
  (mapcar (lambda (branch)
            `(,(pcase--app-subst-match (car branch) sym fun nsym)
              ,@(cdr branch)))
          rest))

(defsubst pcase--mark-used (sym)
  ;; Exceptionally, `sym' may be a constant expression rather than a symbol.
  (if (symbolp sym) (put sym 'pcase-used t)))

(defmacro pcase--flip (fun arg1 arg2)
  "Helper function, used internally to avoid (funcall (lambda ...) ...)."
  (declare (debug (sexp body)))
  `(,fun ,arg2 ,arg1))

(defun pcase--funcall (fun arg vars)
  "Build a function call to FUN with arg ARG."
  (if (symbolp fun)
      `(,fun ,arg)
    (let* (;; `vs' is an upper bound on the vars we need.
           (vs (pcase--fgrep (mapcar #'car vars) fun))
           (env (mapcar (lambda (var)
                          (list var (cdr (assq var vars))))
                        vs))
           (call (progn
                   (when (memq arg vs)
                     ;; `arg' is shadowed by `env'.
                     (let ((newsym (make-symbol "x")))
                       (push (list newsym arg) env)
                       (setq arg newsym)))
                   (if (functionp fun)
                       `(funcall #',fun ,arg)
                     `(,@fun ,arg)))))
      (if (null vs)
          call
        ;; Let's not replace `vars' in `fun' since it's
        ;; too difficult to do it right, instead just
        ;; let-bind `vars' around `fun'.
        `(let* ,env ,call)))))

(defun pcase--eval (exp vars)
  "Build an expression that will evaluate EXP."
  (let* ((found (assq exp vars)))
    (if found (cdr found)
      (let* ((vs (pcase--fgrep (mapcar #'car vars) exp))
             (env (mapcar (lambda (v) (list v (cdr (assq v vars))))
                          vs)))
        (if env (macroexp-let* env exp) exp)))))

;; It's very tempting to use `pcase' below, tho obviously, it'd create
;; bootstrapping problems.
(defun pcase--u1 (matches code vars rest)
  "Return code that runs CODE (with VARS) if MATCHES match.
Otherwise, it defers to REST which is a list of branches of the form
\(ELSE-MATCH ELSE-CODE . ELSE-VARS)."
  ;; Depending on the order in which we choose to check each of the MATCHES,
  ;; the resulting tree may be smaller or bigger.  So in general, we'd want
  ;; to be careful to chose the "optimal" order.  But predicate
  ;; patterns make this harder because they create dependencies
  ;; between matches.  So we don't bother trying to reorder anything.
  (cond
   ((null matches) (funcall code vars))
   ((eq :pcase--fail (car matches)) (pcase--u rest))
   ((eq :pcase--succeed (car matches))
    (pcase--u1 (cdr matches) code vars rest))
   ((eq 'and (caar matches))
    (pcase--u1 (append (cdar matches) (cdr matches)) code vars rest))
   ((eq 'or (caar matches))
    (let* ((alts (cdar matches))
           (var (if (eq (caar alts) 'match) (cadr (car alts))))
           (simples '()) (others '()) (memq-ok t))
      (when var
        (dolist (alt alts)
          (if (and (eq (car alt) 'match) (eq var (cadr alt))
                   (let ((upat (cddr alt)))
                     (eq (car-safe upat) 'quote)))
              (let ((val (cadr (cddr alt))))
                (unless (or (integerp val) (symbolp val))
                  (setq memq-ok nil))
                (push (cadr (cddr alt)) simples))
            (push alt others))))
      (cond
       ((null alts) (error "Please avoid it") (pcase--u rest))
       ;; Yes, we can use `memq' (or `member')!
       ((> (length simples) 1)
        (pcase--u1 (cons `(match ,var
                                 . (pred (pcase--flip
                                          ,(if memq-ok #'memq #'member)
                                          ',simples)))
                         (cdr matches))
                   code vars
                   (if (null others) rest
                     (cons (cons
                            (pcase--and (if (cdr others)
                                            (cons 'or (nreverse others))
                                          (car others))
                                        (cdr matches))
                            (cons code vars))
                           rest))))
       (t
        (pcase--u1 (cons (pop alts) (cdr matches)) code vars
                   (if (null alts) (progn (error "Please avoid it") rest)
                     (cons (cons
                            (pcase--and (if (cdr alts)
                                            (cons 'or alts) (car alts))
                                        (cdr matches))
                            (cons code vars))
                           rest)))))))
   ((eq 'match (caar matches))
    (let* ((popmatches (pop matches))
           (_op (car popmatches))      (cdrpopmatches (cdr popmatches))
           (sym (car cdrpopmatches))
           (upat (cdr cdrpopmatches)))
      (cond
       ((memq upat '(t _))
        (let ((code (pcase--u1 matches code vars rest)))
          (if (eq upat '_) code
            (macroexp--warn-and-return
             "Pattern t is deprecated.  Use `_' instead"
             code))))
       ((eq upat 'pcase--dontcare) :pcase--dontcare)
       ((memq (car-safe upat) '(guard pred))
        (if (eq (car upat) 'pred) (pcase--mark-used sym))
        (let* ((splitrest
                (pcase--split-rest
                 sym (lambda (pat) (pcase--split-pred vars upat pat)) rest))
               (then-rest (car splitrest))
               (else-rest (cdr splitrest)))
          (pcase--if (if (eq (car upat) 'pred)
                         (pcase--funcall (cadr upat) sym vars)
                       (pcase--eval (cadr upat) vars))
                     (pcase--u1 matches code vars then-rest)
                     (pcase--u else-rest))))
       ((and (symbolp upat) upat)
        (pcase--mark-used sym)
        (if (not (assq upat vars))
            (pcase--u1 matches code (cons (cons upat sym) vars) rest)
          ;; Non-linear pattern.  Turn it into an `eq' test.
          (pcase--u1 (cons `(match ,sym . (pred (eq ,(cdr (assq upat vars)))))
                           matches)
                     code vars rest)))
       ((eq (car-safe upat) 'let)
        ;; A upat of the form (let VAR EXP).
        ;; (pcase--u1 matches code
        ;;            (cons (cons (nth 1 upat) (nth 2 upat)) vars) rest)
        (macroexp-let2
            macroexp-copyable-p sym
            (pcase--eval (nth 2 upat) vars)
          (pcase--u1 (cons (pcase--match sym (nth 1 upat)) matches)
                     code vars rest)))
       ((eq (car-safe upat) 'app)
        ;; A upat of the form (app FUN PAT)
        (pcase--mark-used sym)
        (let* ((fun (nth 1 upat))
               (nsym (make-symbol "x"))
               (body
                ;; We don't change `matches' to reuse the newly computed value,
                ;; because we assume there shouldn't be such redundancy in there.
                (pcase--u1 (cons (pcase--match nsym (nth 2 upat)) matches)
                           code vars
                           (pcase--app-subst-rest rest sym fun nsym))))
          (if (not (get nsym 'pcase-used))
              body
            (macroexp-let*
             `((,nsym ,(pcase--funcall fun sym vars)))
             body))))
       ((eq (car-safe upat) 'quote)
        (pcase--mark-used sym)
        (let* ((val (cadr upat))
               (splitrest (pcase--split-rest
                           sym (lambda (pat) (pcase--split-equal val pat)) rest))
               (then-rest (car splitrest))
               (else-rest (cdr splitrest)))
          (pcase--if (cond
                      ((null val) `(null ,sym))
                      ((or (integerp val) (symbolp val))
                       (if (pcase--self-quoting-p val)
                           `(eq ,sym ,val)
                         `(eq ,sym ',val)))
                      (t `(equal ,sym ',val)))
                     (pcase--u1 matches code vars then-rest)
                     (pcase--u else-rest))))
       ((eq (car-safe upat) 'not)
        ;; FIXME: The implementation below is naive and results in
        ;; inefficient code.
        ;; To make it work right, we would need to turn pcase--u1's
        ;; `code' and `vars' into a single argument of the same form as
        ;; `rest'.  We would also need to split this new `then-rest' argument
        ;; for every test (currently we don't bother to do it since
        ;; it's only useful for odd patterns like (and `(PAT1 . PAT2)
        ;; `(PAT3 . PAT4)) which the programmer can easily rewrite
        ;; to the more efficient `(,(and PAT1 PAT3) . ,(and PAT2 PAT4))).
        (pcase--u1 `((match ,sym . ,(cadr upat)))
                   ;; FIXME: This codegen is not careful to share its
                   ;; code if used several times: code blow up is likely.
                   (lambda (_vars)
                     ;; `vars' will likely contain bindings which are
                     ;; not always available in other paths to
                     ;; `rest', so there' no point trying to pass
                     ;; them down.
                     (pcase--u rest))
                   vars
                   (list `((and . ,matches) ,code . ,vars))))
       (t (error "Unknown pattern `%S'" upat)))))
   (t (error "Incorrect MATCH %S" (car matches)))))

(def-edebug-spec
  pcase-QPAT
  ;; Cf. edebug spec for `backquote-form' in edebug.el.
  (&or ("," pcase-PAT)
       (pcase-QPAT [&rest [&not ","] pcase-QPAT]
		   . [&or nil pcase-QPAT])
       (vector &rest pcase-QPAT)
       sexp))

(pcase-defmacro \` (qpat)
  "Backquote-style pcase patterns.
QPAT can take the following forms:
  (QPAT1 . QPAT2)       matches if QPAT1 matches the car and QPAT2 the cdr.
  [QPAT1 QPAT2..QPATn]  matches a vector of length n and QPAT1..QPATn match
                           its 0..(n-1)th elements, respectively.
  ,PAT                  matches if the pcase pattern PAT matches.
  ATOM                  matches if the object is `equal' to ATOM.
			   ATOM can be a symbol, an integer, or a string."
  (declare (debug (pcase-QPAT)))
  (cond
   ((eq (car-safe qpat) '\,) (cadr qpat))
   ((vectorp qpat)
    `(and (pred vectorp)
          (app length ,(length qpat))
          ,@(let ((upats nil))
              (dotimes (i (length qpat))
                (push `(app (pcase--flip aref ,i) ,(list '\` (aref qpat i)))
                      upats))
              (nreverse upats))))
   ((consp qpat)
    `(and (pred consp)
          (app car ,(list '\` (car qpat)))
          (app cdr ,(list '\` (cdr qpat)))))
   ((or (stringp qpat) (integerp qpat) (symbolp qpat)) `',qpat)
   (t (error "Unknown QPAT: %S" qpat))))

(provide 'pcase)
;;; pcase.el ends here
