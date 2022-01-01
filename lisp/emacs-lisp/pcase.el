;;; pcase.el --- ML-style pattern-matching macro for Elisp -*- lexical-binding: t -*-

;; Copyright (C) 2010-2022 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: extensions

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

;; ML-style pattern matching.
;; The entry points are autoloaded.

;; Todo:

;; - Allow to provide new `pcase--split-<foo>' thingy.
;; - provide something like (setq VAR) so a var can be set rather than
;;   let-bound.
;; - provide a way to continue matching to subsequent cases
;;   (e.g. Like Racket's (=> ID).
;; - try and be more clever to reduce the size of the decision tree, and
;;   to reduce the number of leaves that need to be turned into functions:
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
;; (defconst pcase--memoize (make-hash-table :test 'eq))
;; (defconst pcase--memoize-1 (make-hash-table :test 'eq))
;; (defconst pcase--memoize-2 (make-hash-table :weakness 'key :test 'equal))

(defconst pcase--dontcare-upats '(t _ pcase--dontcare))

(defvar pcase--dontwarn-upats '(pcase--dontcare))

(def-edebug-elem-spec 'pcase-PAT
  '(&or (&interpose symbolp pcase--edebug-match-pat-args) sexp))

(def-edebug-elem-spec 'pcase-FUN
  '(&or lambda-expr
        ;; Punt on macros/special forms.
        (functionp &rest form)
        sexp))

;; Only called from edebug.
(declare-function edebug-get-spec "edebug" (symbol))
(defun pcase--edebug-match-pat-args (head pf)
  ;; (cl-assert (null (cdr head)))
  (setq head (car head))
  (or (alist-get head '((quote sexp)
                        (or    &rest pcase-PAT)
                        (and   &rest pcase-PAT)
                        (guard form)
                        (pred  &or ("not" pcase-FUN) pcase-FUN)
                        (app   pcase-FUN pcase-PAT)))
      (let ((me (pcase--get-macroexpander head)))
        (funcall pf (and me (symbolp me) (edebug-get-spec me))))))

(defun pcase--get-macroexpander (s)
  "Return the macroexpander for pcase pattern head S, or nil."
  (get s 'pcase-macroexpander))

;;;###autoload
(defmacro pcase (exp &rest cases)
  ;; FIXME: Add some "global pattern" to wrap every case?
  ;; Could be used to wrap all cases in a `
  "Evaluate EXP to get EXPVAL; try passing control to one of CASES.
CASES is a list of elements of the form (PATTERN CODE...).
For the first CASE whose PATTERN \"matches\" EXPVAL,
evaluate its CODE..., and return the value of the last form.
If no CASE has a PATTERN that matches, return nil.

Each PATTERN expands, in essence, to a predicate to call
on EXPVAL.  When the return value of that call is non-nil,
PATTERN matches.  PATTERN can take one of the forms:

  _                matches anything.
  \\='VAL             matches if EXPVAL is `equal' to VAL.
  KEYWORD          shorthand for \\='KEYWORD
  INTEGER          shorthand for \\='INTEGER
  STRING           shorthand for \\='STRING
  SYMBOL           matches anything and binds it to SYMBOL.
                   If a SYMBOL is used twice in the same pattern
                   the second occurrence becomes an `eq'uality test.
  (pred FUN)       matches if FUN called on EXPVAL returns non-nil.
  (pred (not FUN)) matches if FUN called on EXPVAL returns nil.
  (app FUN PAT)    matches if FUN called on EXPVAL matches PAT.
  (guard BOOLEXP)  matches if BOOLEXP evaluates to non-nil.
  (and PAT...)     matches if all the patterns match.
  (or PAT...)      matches if any of the patterns matches.

FUN in `pred' and `app' can take one of the forms:
  SYMBOL  or  (lambda ARGS BODY)
     call it with one argument
  (F ARG1 .. ARGn)
     call F with ARG1..ARGn and EXPVAL as n+1'th argument

FUN, BOOLEXP, and subsequent PAT can refer to variables
bound earlier in the pattern by a SYMBOL pattern.

Additional patterns can be defined using `pcase-defmacro'.

See Info node `(elisp) Pattern-Matching Conditional' in the
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
;;;###autoload
(put 'pcase 'function-documentation '(pcase--make-docstring))
;;;###autoload
(defun pcase--make-docstring ()
  (let* ((main (documentation (symbol-function 'pcase) 'raw))
         (ud (help-split-fundoc main 'pcase)))
    ;; So that eg emacs -Q -l cl-lib --eval "(documentation 'pcase)" works,
    ;; where cl-lib is anything using pcase-defmacro.
    (require 'help-fns)
    (with-temp-buffer
      (insert (or (cdr ud) main))
      ;; Presentation Note: For conceptual continuity, we guarantee
      ;; that backquote doc immediately follows main pcase doc.
      ;; (The order of the other extensions is unimportant.)
      (let (more)
        ;; Collect all the extensions.
        (mapatoms (lambda (symbol)
                    (let ((me (pcase--get-macroexpander symbol)))
                      (when me
                        (push (cons symbol me)
                              more)))))
        ;; Ensure backquote is first.
        (let ((x (assq '\` more)))
          (setq more (cons x (delq x more))))
        ;; Do the output.
        (while more
          (let* ((pair (pop more))
                 (symbol (car pair))
                 (me (cdr pair))
                 (doc (documentation me 'raw)))
            (insert "\n\n-- ")
            (setq doc (help-fns--signature symbol doc me
                                           (indirect-function me)
                                           nil))
            (insert "\n" (or doc "Not documented.")))))
      (let ((combined-doc (buffer-string)))
        (if ud (help-add-fundoc-usage combined-doc (car ud)) combined-doc)))))

;;;###autoload
(defmacro pcase-exhaustive (exp &rest cases)
  "The exhaustive version of `pcase' (which see).
If EXP fails to match any of the patterns in CASES, an error is
signaled.

In contrast, `pcase' will return nil if there is no match, but
not signal an error."
  (declare (indent 1) (debug pcase))
  (let* ((x (gensym "x"))
         (pcase--dontwarn-upats (cons x pcase--dontwarn-upats)))
    (pcase--expand
     ;; FIXME: Could we add the FILE:LINE data in the error message?
     ;; FILE is available from `macroexp-file-name'.
     exp (append cases `((,x (error "No clause matching `%S'" ,x)))))))

;;;###autoload
(defmacro pcase-lambda (lambda-list &rest body)
  "Like `lambda' but allow each argument to be a pattern.
I.e. accepts the usual &optional and &rest keywords, but every
formal argument can be any pattern accepted by `pcase' (a mere
variable name being but a special case of it)."
  (declare (doc-string 2) (indent defun)
           (debug (&define (&rest pcase-PAT) lambda-doc def-body)))
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
  "Like `let*', but supports destructuring BINDINGS using `pcase' patterns.
As with `pcase-let', BINDINGS are of the form (PATTERN EXP), but the
EXP in each binding in BINDINGS can use the results of the destructuring
bindings that precede it in BINDINGS' order.

Each EXP should match (i.e. be of compatible structure) to its
respective PATTERN; a mismatch may signal an error or may go
undetected, binding variables to arbitrary values, such as nil."
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
  "Like `let', but supports destructuring BINDINGS using `pcase' patterns.
BODY should be a list of expressions, and BINDINGS should be a list of
bindings of the form (PATTERN EXP).
All EXPs are evaluated first, and then used to perform destructuring
bindings by matching each EXP against its respective PATTERN.  Then
BODY is evaluated with those bindings in effect.

Each EXP should match (i.e. be of compatible structure) to its
respective PATTERN; a mismatch may signal an error or may go
undetected, binding variables to arbitrary values, such as nil."
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
  "Eval BODY once for each set of bindings defined by PATTERN and LIST elements.
PATTERN should be a `pcase' pattern describing the structure of
LIST elements, and LIST is a list of objects that match PATTERN,
i.e. have a structure that is compatible with PATTERN.
For each element of LIST, this macro binds the variables in
PATTERN to the corresponding subfields of the LIST element, and
then evaluates BODY with these bindings in effect.  The
destructuring bindings of variables in PATTERN to the subfields
of the elements of LIST is performed as if by `pcase-let'.
\n(fn (PATTERN LIST) BODY...)"
  (declare (indent 1) (debug ((pcase-PAT form) body)))
  (if (pcase--trivial-upat-p (car spec))
      `(dolist ,spec ,@body)
    (let ((tmpvar (gensym "x")))
      `(dolist (,tmpvar ,@(cdr spec))
         (pcase-let* ((,(car spec) ,tmpvar))
           ,@body)))))

;;;###autoload
(defmacro pcase-setq (pat val &rest args)
  "Assign values to variables by destructuring with `pcase'.
PATTERNS are normal `pcase' patterns, and VALUES are expression.

Evaluation happens sequentially as in `setq' (not in parallel).

An example: (pcase-setq `((,a) [(,b)]) '((1) [(2)]))

VAL is presumed to match PAT.  Failure to match may signal an error or go
undetected, binding variables to arbitrary values, such as nil.

\(fn PATTERNS VALUE PATTERN VALUES ...)"
  (declare (debug (&rest [pcase-PAT form])))
  (cond
   (args
    (let ((arg-length (length args)))
      (unless (= 0 (mod arg-length 2))
        (signal 'wrong-number-of-arguments
                (list 'pcase-setq (+ 2 arg-length)))))
    (let ((result))
      (while args
        (push `(pcase-setq ,(pop args) ,(pop args))
              result))
      `(progn
         (pcase-setq ,pat ,val)
         ,@(nreverse result))))
   ((pcase--trivial-upat-p pat)
    `(setq ,pat ,val))
   (t
    (pcase-compile-patterns
     val
     `((,pat
        . ,(lambda (varvals &rest _)
             `(setq ,@(mapcan (lambda (varval)
                                (let ((var (car varval))
                                      (val (cadr varval)))
                                  (list var val)))
                              varvals))))
       (pcase--dontcare . ignore))))))

(defun pcase--trivial-upat-p (upat)
  (and (symbolp upat) (not (memq upat pcase--dontcare-upats))))

(defun pcase-compile-patterns (exp cases)
  "Compile the set of patterns in CASES.
EXP is the expression that will be matched against the patterns.
CASES is a list of elements (PAT . CODEGEN)
where CODEGEN is a function that returns the code to use when
PAT matches.  That code has to be in the form of a cons cell.

CODEGEN will be called with at least 2 arguments, VARVALS and COUNT.
VARVALS is a list of elements of the form (VAR VAL . RESERVED) where VAR
is a variable bound by the pattern and VAL is a duplicable expression
that returns the value this variable should be bound to.
If the pattern PAT uses `or', CODEGEN may be called multiple times,
in which case it may want to generate the code differently to avoid
a potential code explosion.  For this reason the COUNT argument indicates
how many time this CODEGEN is called."
  (macroexp-let2 macroexp-copyable-p val exp
    (let* ((seen '())
           (phcounter 0)
           (main
            (pcase--u
             (mapcar
              (lambda (case)
                `(,(pcase--match val (pcase--macroexpand (car case)))
                  ,(lambda (vars)
                     (let ((prev (assq case seen)))
                       (unless prev
                         ;; Keep track of the cases that are used.
                         (push (setq prev (list case)) seen))
                       ;; Put a counter in the cdr just so that not
                       ;; all branches look identical (to avoid things
                       ;; like `macroexp--if' optimizing them too
                       ;; optimistically).
                       (let ((ph (cons 'pcase--placeholder
                                       (setq phcounter (1+ phcounter)))))
                         (setcdr prev (cons (cons vars ph) (cdr prev)))
                         ph)))))
              cases))))
      ;; Take care of the place holders now.
      (dolist (branch seen)
        (let ((codegen (cdar branch))
              (uses (cdr branch)))
          ;; Find all the vars that are in scope (the union of the
          ;; vars provided in each use case).
          (let* ((allvarinfo '())
                 (_ (dolist (use uses)
                      (dolist (v (car use))
                        (let ((vi (assq (car v) allvarinfo)))
                          (if vi
                              (if (cddr v) (setcdr vi 'used))
                            (push (cons (car v) (cddr v)) allvarinfo))))))
                 (allvars (mapcar #'car allvarinfo)))
            (dolist (use uses)
              (let* ((vars (car use))
                     (varvals
                      (mapcar (lambda (v)
                                `(,v ,(cadr (assq v vars))
                                     ,(cdr (assq v allvarinfo))))
                              allvars))
                     (placeholder (cdr use))
                     (code (funcall codegen varvals (length uses))))
                ;; (cl-assert (eq (car placeholder) 'pcase--placeholder))
                (setcar placeholder (car code))
                (setcdr placeholder (cdr code)))))))
      (dolist (case cases)
        (unless (or (assq case seen)
                    (memq (car case) pcase--dontwarn-upats))
          (setq main
                (macroexp-warn-and-return
                 (format "pcase pattern %S shadowed by previous pcase pattern"
                         (car case))
                 main))))
      main)))

(defun pcase--expand (exp cases)
  ;; (message "pid=%S (pcase--expand %S ...hash=%S)"
  ;;          (emacs-pid) exp (sxhash cases))
  (let* ((defs ())
         (codegen
          (lambda (code)
            (if (member code '(nil (nil) ('nil)))
                (lambda (&rest _) ''nil)
              (let ((bsym ()))
                (lambda (varvals count &rest _)
                  (let* ((ignored-vars
                          (delq nil (mapcar (lambda (vv) (if (nth 2 vv) (car vv)))
                                            varvals)))
                         (ignores (if ignored-vars
                                      `((ignore . ,ignored-vars)))))
                    ;; Since we use a tree-based pattern matching
                    ;; technique, the leaves (the places that contain the
                    ;; code to run once a pattern is matched) can get
                    ;; copied a very large number of times, so to avoid
                    ;; code explosion, we need to keep track of how many
                    ;; times we've used each leaf and move it
                    ;; to a separate function if that number is too high.
                    (if (or (< count 2) (pcase--small-branch-p code))
                        `(let ,(mapcar (lambda (vv) (list (car vv) (cadr vv)))
                                       varvals)
                           ;; Try and silence some of the most common
                           ;; spurious "unused var" warnings.
                           ,@ignores
                           ,@code)
                    ;; Several occurrence of this non-small branch in
                    ;; the output.
                    (unless bsym
                      (setq bsym (make-symbol
                                  (format "pcase-%d" (length defs))))
                      (push `(,bsym (lambda ,(mapcar #'car varvals)
                                      ,@ignores ,@code))
                            defs))
                    `(funcall ,bsym ,@(mapcar #'cadr varvals)))))))))
         (main
          (pcase-compile-patterns
           exp
           (mapcar (lambda (case)
                     (cons (car case) (funcall codegen (cdr case))))
                   cases))))
    (macroexp-let* defs main)))

(defun pcase--macroexpand (pat)
  "Expands all macro-patterns in PAT."
  (let ((head (car-safe pat)))
    (cond
     ((null head)
      (if (pcase--self-quoting-p pat) `',pat pat))
     ((memq head '(pred guard quote)) pat)
     ((memq head '(or and)) `(,head ,@(mapcar #'pcase--macroexpand (cdr pat))))
     ((eq head 'app) `(app ,(nth 1 pat) ,(pcase--macroexpand (nth 2 pat))))
     (t
      (let* ((expander (pcase--get-macroexpander head))
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
to this macro.

By convention, DOC should use \"EXPVAL\" to stand
for the result of evaluating EXP (first arg to `pcase').
\n(fn NAME ARGS [DOC] &rest BODY...)"
  (declare (indent 2) (debug defun) (doc-string 3))
  ;; Add the function via `fsym', so that an autoload cookie placed
  ;; on a pcase-defmacro will cause the macro to be loaded on demand.
  (let ((fsym (intern (format "%s--pcase-macroexpander" name)))
	(decl (assq 'declare body)))
    (when decl (setq body (remove decl body)))
    `(progn
       ;; FIXME: We use `eval-and-compile' here so that the pcase macro can be
       ;; used in the same file where it's defined, but ideally, we should
       ;; handle this using something similar to `overriding-plist-environment'
       ;; but for `symbol-function' slots so compiling a file doesn't have the
       ;; side-effect of defining the function.
       (eval-and-compile
         (defun ,fsym ,args ,@body))
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
   ((eq else :pcase--dontcare) `(progn (ignore ,test) ,then))
   ;; This happens very rarely.  Known case:
   ;;     (pcase EXP ((and 1 pcase--dontcare) FOO))
   ((eq then :pcase--dontcare) `(progn (ignore ,test) ,else))
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
MATCH is the pattern that needs to be matched, of the form:
  (match VAR . PAT)
  (and MATCH ...)
  (or MATCH ...)
VARS is the set of vars already bound by earlier matches.
It is a list of (NAME VAL . USED) where NAME is the variable's symbol,
VAL is the expression to which it should be bound and USED is a boolean
recording whether the var has been referenced by earlier parts of the match."
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
    (t (error "Unknown MATCH %s" match))))

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
  "Indicate the overlap or mutual-exclusion between UPAT and PAT.
More specifically returns a pair (A . B) where A indicates whether PAT
can match when UPAT has matched, and B does the same for the case
where UPAT failed to match.
A and B can be one of:
- nil if we don't know
- `:pcase--fail' if UPAT match's result implies that PAT can't match
- `:pcase--succeed' if UPAT match's result implies that PAT matches"
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
               (not (macroexp--fgrep vars (cadr upat)))))
      '(:pcase--succeed . :pcase--fail))
     ;; In case PAT is of the form (pred (not PRED))
     ((and (eq 'pred (car-safe pat)) (eq 'not (car-safe (cadr pat))))
      (let* ((test (cadr (cadr pat)))
             (res (pcase--split-pred vars upat `(pred ,test)))
             (reverse (lambda (x) (cond ((eq x :pcase--succeed) :pcase--fail)
                                   ((eq x :pcase--fail) :pcase--succeed)))))
        (cons (funcall reverse (car res))
              (funcall reverse (cdr res)))))
     ;; All the rest below presumes UPAT is of the form (pred ...).
     ((not (eq 'pred (car upat))) nil)
     ;; In case UPAT is of the form (pred (not PRED))
     ((eq 'not (car-safe (cadr upat)))
      (let* ((test (cadr (cadr upat)))
             (res (pcase--split-pred vars `(pred ,test) pat)))
        (cons (cdr res) (car res))))
     ((let ((otherpred
             (cond ((eq 'pred (car-safe pat)) (cadr pat))
                   ((not (eq 'quote (car-safe pat))) nil)
                   ((consp (cadr pat)) #'consp)
                   ((stringp (cadr pat)) #'stringp)
                   ((vectorp (cadr pat)) #'vectorp)
                   ((byte-code-function-p (cadr pat))
                    #'byte-code-function-p))))
        (pcase--mutually-exclusive-p (cadr upat) otherpred))
      '(:pcase--fail . nil))
     ;; Since we turn (or 'a 'b 'c) into (pred (pcase--flip (memq '(a b c))))
     ;; try and preserve the info we get from that memq test.
     ((and (eq 'pcase--flip (car-safe (cadr upat)))
           (memq (cadr (cadr upat)) '(memq member memql))
           (eq 'quote (car-safe (nth 2 (cadr upat))))
           (eq 'quote (car-safe pat)))
      (let ((set (cadr (nth 2 (cadr upat)))))
        (if (member (cadr pat) set)
            '(nil . :pcase--fail)
          '(:pcase--fail . nil))))
     ((and (eq 'quote (car-safe pat))
           (symbolp (cadr upat))
           (or (symbolp (cadr pat)) (stringp (cadr pat)) (numberp (cadr pat)))
           (get (cadr upat) 'side-effect-free)
           (ignore-errors
             (setq test (list (funcall (cadr upat) (cadr pat))))))
      (if (car test)
          '(nil . :pcase--fail)
        '(:pcase--fail . nil))))))

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
   (t (error "Unknown MATCH %s" match))))

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
  (cond
   ((symbolp fun) `(,fun ,arg))
   ((eq 'not (car-safe fun)) `(not ,(pcase--funcall (cadr fun) arg vars)))
   (t
    (let* (;; `env' is hopefully an upper bound on the bindings we need,
           ;; FIXME: See bug#46786 for a counter example :-(
           (env (mapcar (lambda (x)
                          (setcdr (cdr x) 'used)
                          (list (car x) (cadr x)))
                        (macroexp--fgrep vars fun)))
           (call (progn
                   (when (assq arg env)
                     ;; `arg' is shadowed by `env'.
                     (let ((newsym (gensym "x")))
                       (push (list newsym arg) env)
                       (setq arg newsym)))
                   (if (or (functionp fun) (not (consp fun)))
                       `(funcall #',fun ,arg)
                     `(,@fun ,arg)))))
      (if (null env)
          call
        ;; Let's not replace `vars' in `fun' since it's
        ;; too difficult to do it right, instead just
        ;; let-bind `vars' around `fun'.
        `(let* ,env ,call))))))

(defun pcase--eval (exp vars)
  "Build an expression that will evaluate EXP."
  (let* ((found (assq exp vars)))
    (if found (progn (setcdr (cdr found) 'used) (cadr found))
      (let* ((env (macroexp--fgrep vars exp)))
        (if env
            (macroexp-let* (mapcar (lambda (x)
                                     (setcdr (cdr x) 'used)
                                     (list (car x) (cadr x)))
                                   env)
                           exp)
          exp)))))

;; It's very tempting to use `pcase' below, tho obviously, it'd create
;; bootstrapping problems.
(defun pcase--u1 (matches code vars rest)
  "Return code that runs CODE (with VARS) if MATCHES match.
Otherwise, it defers to REST which is a list of branches of the form
\(ELSE-MATCH ELSE-CODE . ELSE-VARS)."
  ;; Depending on the order in which we choose to check each of the MATCHES,
  ;; the resulting tree may be smaller or bigger.  So in general, we'd want
  ;; to be careful to choose the "optimal" order.  But predicate
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
           (simples '()) (others '()) (mem-fun 'memq))
      (when var
        (dolist (alt alts)
          (if (and (eq (car alt) 'match) (eq var (cadr alt))
                   (let ((upat (cddr alt)))
                     (eq (car-safe upat) 'quote)))
              (let ((val (cadr (cddr alt))))
                (cond ((integerp val)
                       (when (eq mem-fun 'memq)
                         (setq mem-fun 'memql)))
                      ((not (symbolp val))
                       (setq mem-fun 'member)))
                (push val simples))
            (push alt others))))
      (cond
       ((null alts) (error "Please avoid it") (pcase--u rest))
       ;; Yes, we can use `memql' (or `member')!
       ((> (length simples) 1)
        (pcase--u1 (cons `(match ,var
                                 . (pred (pcase--flip ,mem-fun ',simples)))
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
            (macroexp-warn-and-return
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
        (let ((v (assq upat vars)))
          (if (not v)
              (pcase--u1 matches code (cons (list upat sym) vars) rest)
            ;; Non-linear pattern.  Turn it into an `eq' test.
            (setcdr (cdr v) 'used)
            (pcase--u1 (cons `(match ,sym . (pred (eql ,(cadr v))))
                             matches)
                       code vars rest))))
       ((eq (car-safe upat) 'app)
        ;; A upat of the form (app FUN PAT)
        (pcase--mark-used sym)
        (let* ((fun (nth 1 upat))
               (nsym (gensym "x"))
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
                      ((integerp val) `(eql ,sym ,val))
                      ((symbolp val)
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

(def-edebug-elem-spec 'pcase-QPAT
  ;; Cf. edebug spec for `backquote-form' in edebug.el.
  '(&or ("," pcase-PAT)
        (pcase-QPAT [&rest [&not ","] pcase-QPAT]
		    . [&or nil pcase-QPAT])
	(vector &rest pcase-QPAT)
	sexp))

(pcase-defmacro \` (qpat)
  "Backquote-style pcase patterns: \\=`QPAT
QPAT can take the following forms:
  (QPAT1 . QPAT2)       matches if QPAT1 matches the car and QPAT2 the cdr.
  [QPAT1 QPAT2..QPATn]  matches a vector of length n and QPAT1..QPATn match
                           its 0..(n-1)th elements, respectively.
  ,PAT                  matches if the `pcase' pattern PAT matches.
  SYMBOL                matches if EXPVAL is `equal' to SYMBOL.
  KEYWORD               likewise for KEYWORD.
  NUMBER                likewise for NUMBER.
  STRING                likewise for STRING.

The list or vector QPAT is a template.  The predicate formed
by a backquote-style pattern is a combination of those
formed by any sub-patterns, wrapped in a top-level condition:
EXPVAL must be \"congruent\" with the template.  For example:

  \\=`(technical ,forum)

The predicate is the logical-AND of:
 - Is EXPVAL a list of two elements?
 - Is the first element the symbol `technical'?
 - True!  (The second element can be anything, and for the sake
   of the body forms, its value is bound to the symbol `forum'.)"
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
          (app car-safe ,(list '\` (car qpat)))
          (app cdr-safe ,(list '\` (cdr qpat)))))
   ((or (stringp qpat) (numberp qpat) (symbolp qpat)) `',qpat)
   ;; In all other cases just raise an error so we can't break
   ;; backward compatibility when adding \` support for other
   ;; compounded values that are not `consp'
   (t (error "Unknown QPAT: %S" qpat))))

(pcase-defmacro let (pat expr)
  "Matches if EXPR matches PAT."
  (declare (debug (pcase-PAT form)))
  `(app (lambda (_) ,expr) ,pat))

;; (pcase-defmacro guard (expr)
;;   "Matches if EXPR is non-nil."
;;   (declare (debug (form)))
;;   `(pred (lambda (_) ,expr)))

(provide 'pcase)
;;; pcase.el ends here
