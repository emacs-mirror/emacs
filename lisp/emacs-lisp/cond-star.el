;;; cond-star.el --- Extended form of `cond' construct  -*-lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Maintainer: Richard Stallman <rms@gnu.org>
;; Package: cond-star
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

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

;; This library implements the `cond*' macro, that extends `cond' with
;; pattern-matching capabilities.  It provides an alternative to
;; `pcase'.  Consult the Info note `(elisp) cond* Macro' for details on
;; how to use it.

;;; Implementation Notice:

;; Here is the list of functions the generated code is known to call:
;; car, cdr, car-safe, cdr-safe, nth, nthcdr, null, eq, equal, eql, =,
;; vectorp, length.
;; It also uses these control and binding primitives:
;; and, or, if, progn, let, let*, setq.
;; For regexp matching only, it can call string-match and match-string.

;; ??? If a clause starts with a keyword,
;; should the element after the keyword be treated in the usual way
;; as a pattern?  Currently `cond*-non-exit-clause-substance' explicitly
;; prevents that by adding t at the front of its value.

;;; Code:

(require 'cl-lib) ; for cl-assert

;;;###autoload
(defmacro cond* (&rest clauses)
  "Extended form of traditional Lisp `cond' construct.
A `cond*' construct is a series of clauses, and a clause
normally has the form (CONDITION BODY...).

CONDITION can be a Lisp expression, as in `cond'.
Or it can be one of `(bind* BINDINGS...)', `(match* PATTERN DATUM)',
or `(pcase* PATTERN DATUM)',

`(bind* BINDINGS...)' means to bind BINDINGS (as if they were in `let*')
for the body of the clause, and all subsequent clauses, since the `bind*'
clause is always a non-exit clause.  As a condition, it counts as true
and runs the body of the clause if the first binding's value is non-nil.

`(match* PATTERN DATUM)' means to match DATUM against the pattern PATTERN
For its patterns, see `match*'.
The condition counts as true if PATTERN matches DATUM.

`(bind-and* BINDINGS...)' means to bind BINDINGS (as if they were in
`if-let*') for only the the body of the clause.  If any expression
evaluates to nil, the condition counts as false.

`(pcase* PATTERN DATUM)' means to match DATUM against the
pattern PATTERN, using the same pattern syntax as `pcase'.
The condition counts as true if PATTERN matches DATUM.

When a clause's condition is true, and it exits the `cond*'
or is the last clause, the value of the last expression
in its body becomes the return value of the `cond*' construct.

Non-exit clause:

If a clause has only one element, or if its first element is
t or a `bind*' clause, this clause never exits the `cond*' construct.
Instead, control always falls through to the next clause (if any).
All bindings made in CONDITION for the BODY of the non-exit clause
are passed along to the rest of the clauses in this `cond*' construct.

\\[match*] for documentation of the patterns for use in `match*'."
  ;; FIXME: Want an Edebug declaration.
  (cond*-convert clauses))

;; The following four macros are autoloaded for the sake of syntax
;; highlighting.

;;;###autoload
(defmacro match* (_pattern _datum)
  "This specifies matching DATUM against PATTERN.
This is not really a Lisp operator; it is meaningful only in the
CONDITION of a `cond*' clause.

`_' matches any value.
KEYWORD matches that keyword.
nil  matches nil.
t    matches t.
SYMBOL matches any value and binds SYMBOL to that value.
  If SYMBOL has been matched and bound earlier in this pattern,
  it matches here the same value that it matched before.
REGEXP matches a string if REGEXP matches it.
  The match must cover the entire string from its first char to its last.
ATOM (meaning any other kind of non-list not described above)
  matches anything `equal' to it.
\(rx REGEXP) uses a regexp specified in s-expression form,
  as in the function `rx', and matches the data that way.
\(rx REGEXP SYM0 SYM1...) uses a regexp specified in s-expression form,
  and binds the symbols SYM0, SYM1, and so on
  to (match-string 0 DATUM), (match-string 1 DATUM), and so on.
  You can use as many SYMs as regexp matching supports.

\\=`OBJECT  matches any value `equal' to OBJECT.
\(cons CARPAT CDRPAT)
  matches a cons cell if CARPAT matches its car and CDRPAT matches its cdr.
\(list ELTPATS...)
  matches a list if the ELTPATS match its elements.
  The first ELTPAT should match the list's first element.
  The second ELTPAT should match the list's second element.  And so on.
\(vector ELTPATS...)
  matches a vector if the ELTPATS match its elements.
  The first ELTPAT should match the vector's first element.
  The second ELTPAT should match the vector's second element.  And so on.
\(cdr PATTERN)  matches PATTERN with strict checking of cdrs.
  That means that `list' patterns verify that the final cdr is nil.
  Strict checking is the default.
\(cdr-ignore PATTERN)  matches PATTERN with lax checking of cdrs.
  That means that `list' patterns do not examine the final cdr.
\(and CONJUNCTS...)  matches each of the CONJUNCTS against the same data.
  If all of them match, this pattern succeeds.
  If one CONJUNCT fails, this pattern fails and does not try more CONJUNCTS.
\(or DISJUNCTS...)  matches each of the DISJUNCTS against the same data.
  If one DISJUNCT succeeds, this pattern succeeds
  and does not try more DISJUNCTs.
  If all of them fail, this pattern fails.
\(COND*-EXPANDER ...)
  Here the car is a symbol that has a `cond*-expander' property
  which defines how to handle it in a pattern.  The property value
  is a function.  Trying to match such a pattern calls that
  function with one argument, the pattern in question (including its car).
  The function should return an equivalent pattern
  to be matched instead.
\(PREDICATE SYMBOL)
  matches datum if (PREDICATE DATUM) is true,
  then binds SYMBOL to DATUM.
\(PREDICATE SYMBOL MORE-ARGS...)
  matches datum if (PREDICATE DATUM MORE-ARGS...) is true,
  then binds SYMBOL to DATUM.
  MORE-ARGS... can refer to symbols bound earlier in the pattern.
\(constrain SYMBOL EXP)
  matches datum if the form EXP is true.
  EXP can refer to symbols bound earlier in the pattern."
  (macroexp-warn-and-return "`match*' used other than as a `cond*' condition"
                            nil 'suspicious))

;;;###autoload
(defmacro bind* (&rest _bindings)
  "Evaluate BINDINGS like `let*'.
This is not really a Lisp operator; it is meaningful only in the
CONDITION of a `cond*' clause.  See `cond*' for details."
  (macroexp-warn-and-return "`bind*' used other than as a `cond*' condition"
                            nil 'suspicious))

;;;###autoload
(defmacro bind-and* (&rest _bindings)
  "Evaluate BINDINGS like `if-let*'.
This is not really a Lisp operator; it is meaningful only in the
CONDITION of a `cond*' clause.  See `cond*' for details."
  (macroexp-warn-and-return "`bind-and*' used other than as a `cond*' condition"
                            nil 'suspicious))

;;;###autoload
(defmacro pcase* (_pattern _datum)
  "Evaluate PATTERN and DATUM like an element of BINDINGS in `pcase-let'.
This is not really a Lisp operator; it is meaningful only in the
CONDITION of a `cond*' clause.  See `cond*' for details."
  (macroexp-warn-and-return "`pcase*' used other than as a `cond*' condition"
                            nil 'suspicious))

(defun cond*-non-exit-clause-p (clause)
  "If CLAUSE, a cond* clause, is a non-exit clause, return t."
  (or (null (cdr-safe clause))   ;; clause has only one element.
      (and (cdr-safe clause)
           ;; Starts with t.
           (or (eq (car clause) t)
               ;; Starts with a `bind*' pseudo-form.
               (and (consp (car clause))
                    (eq (caar clause) 'bind*))))))

(defun cond*-non-exit-clause-substance (clause)
  "For a non-exit cond* clause CLAUSE, return its substance.
This removes a final keyword if that's what makes CLAUSE non-exit."
  (cond ((or (null (cdr-safe clause))   ;; either clause has only one element
             (and (consp (car clause))  ;; or it starts with `bind*'
                  (eq (caar clause) 'bind*)))
         clause)
        ;; Starts with t or a keyword.
        ;; Include t as the first element of the substance
        ;; so that the following element is not treated as a pattern.
        ((and (cdr-safe clause)
              (or (eq (car clause) t)
                  (keywordp (car clause))))
         ;; Standardize on t as the first element.
         (cons t (cdr clause)))

        ;; Ends with keyword.
        ((keywordp (car (last clause)))
         ;; Do NOT include the final keyword.
         (butlast clause))))

(defun cond*-convert (clauses)
  "Process a list of cond* clauses, CLAUSES.
Returns the equivalent Lisp expression."
  (if clauses
      (cond*-convert-clause (car-safe clauses) (cdr-safe clauses))))

(defun cond*-convert-clause (clause rest)
  "Process one `cond*' clause, CLAUSE.
REST is the rest of the clauses of this cond* expression."
  (if (cond*-non-exit-clause-p clause)
      ;; Handle a non-exit clause.  Make its bindings active
      ;; around the whole rest of this cond*, treating it as
      ;; a condition whose value is always t, around the rest
      ;; of this cond*.
      (let ((substance (cond*-non-exit-clause-substance clause)))
        (cond*-convert-condition
         ;; Handle the first substantial element in the non-exit clause
         ;; as a matching condition.
         (car substance)
         ;; Any following elements in the
         ;; non-exit clause are just expressions.
         (cdr substance)
         ;; Remaining clauses will be UNCONDIT-CLAUSES:
         ;; run unconditionally and handled as a cond* body.
         rest
         nil nil))
    ;; Handle a normal (conditional exit) clause.
    (cond*-convert-condition (car-safe clause) (cdr-safe clause) nil
                             rest (cond*-convert rest))))

(defun cond*-convert-condition (condition true-exps uncondit-clauses rest iffalse)
  "Process the condition part of one cond* clause.
TRUE-EXPS is a list of Lisp expressions to be executed if this
condition is true, and inside its bindings.
UNCONDIT-CLAUSES is a list of cond*-clauses to be executed if this
condition is true, and inside its bindings.
This is used for non-exit clauses; it is nil for conditional-exit clauses.

REST and IFFALSE are non-nil for conditional-exit clauses that are not final.
REST is a list of clauses to process after this one if
this one could have exited but does not exit.
This is used for conditional exit clauses.
IFFALSE is the value to compute after this one if
this one could have exited but does not exit.
This is used for conditional exit clauses."
  (if (and uncondit-clauses rest)
      (error "Clause is both exiting and non-exit"))
  (let ((pat-type (car-safe condition)))
    (cond ((eq pat-type 'bind*)
           (let* ((bindings (cdr condition))
                  (first-binding (car bindings))
                  (first-variable (if (symbolp first-binding) first-binding
                                    (car first-binding)))
                  (first-value (if (symbolp first-binding) nil
                                 (cadr first-binding)))
                  (init-gensym (gensym "init"))
                  ;; BINDINGS with the initial value of the first binding
                  ;; replaced by INIT-GENSYM.
                  (mod-bindings
                   (cons (list first-variable init-gensym) (cdr bindings))))
             ;;; ??? Here pull out all nontrivial initial values
             ;;; ??? to compute them earlier.
             (if rest
                 ;; bind* starts an exiting clause which is not final.
                 ;; Therefore, must run IFFALSE.
                 `(let ((,init-gensym ,first-value))
                    (if ,init-gensym
                        (let* ,mod-bindings
                          . ,true-exps)
                      ;; Always calculate all bindings' initial values,
                      ;; but the bindings must not cover IFFALSE.
                      (let* ,mod-bindings nil)
                      ,iffalse))
               (if uncondit-clauses
                   ;; bind* starts a non-exit clause which is not final.
                   ;; Run the TRUE-EXPS if condition value is true.
                   ;; Then always go on to run the UNCONDIT-CLAUSES.
                   (if true-exps
                       `(let ((,init-gensym ,first-value))
;;;  ??? Should we make the bindings a second time for the UNCONDIT-CLAUSES.
;;;  as the doc string says, for uniformity with match*?
                          (let* ,mod-bindings
                            (when ,init-gensym
                              . ,true-exps)
                            ,(cond*-convert uncondit-clauses)))
                     `(let* ,bindings
                        ,(cond*-convert uncondit-clauses)))
                 ;; bind* starts a final clause.
                 ;; If there are TRUE-EXPS, run them if condition succeeded.
                 ;; Always make the bindings, in case the
                 ;; initial values have side effects.
                 `(let ((,init-gensym ,first-value))
                    ;; Calculate all binding values unconditionally.
                    (let* ,mod-bindings
                      (when ,init-gensym
                        . ,true-exps)))))))
          ((eq pat-type 'bind-and*)
           (let ((checks '()) (last t))
             (dolist (bind (cdr condition))
               (push (list (car bind) (list 'and last (cadr bind)))
                     checks)
               (when (eq (caar checks) '_)
                 (setcar (car checks) (make-symbol "s")))
               (setq last (caar checks)))
             (cond
              ;; For explanations on these cases, see "Ordinary
              ;; Lisp expression is the condition." below.
              (rest
               (let ((quit (gensym "quit")))
                 `(catch ',quit
                    (let* (,@(nreverse checks))
                      (if ,last (throw ',quit ,(macroexp-progn true-exps))))
                    ,iffalse)))
              (uncondit-clauses
               `(progn
                  (let* (,@(nreverse checks))
                    (if ,last ,(macroexp-progn true-exps)))
                  ,(cond*-convert uncondit-clauses)))
              (true-exps
               `(let* (,@(nreverse checks))
                  (if ,last ,(macroexp-progn true-exps))))
              (t last))))
          ((eq pat-type 'pcase*)
           (if true-exps
               (progn
                 (when uncondit-clauses
                   ;; FIXME: This happens in cases like
                   ;;     (cond* ((match* `(,x . ,y) EXP) THEN :non-exit)
                   ;;            (t ELSE))
                   ;; where ELSE is supposed to run after THEN also (and
                   ;; with access to `x' and `y').
                   (error ":non-exit not supported with `pcase*'"))
                 (cl-assert (or (null iffalse) rest))
                 `(pcase ,(nth 2 condition)
                    (,(nth 1 condition) ,@true-exps)
                    (_ ,iffalse)))
             (cl-assert (null iffalse))
             (cl-assert (null rest))
             `(pcase-let ((,(nth 1 condition) ,(nth 2 condition)))
                (cond* . ,uncondit-clauses))))
          ((eq pat-type 'match*)
           (cond*-match condition true-exps uncondit-clauses iffalse))
          (t
           ;; Ordinary Lisp expression is the condition.
           (if rest
               ;; A nonfinal exiting clause.
               ;; If condition succeeds, run the TRUE-EXPS.
               ;; There are following clauses, so run IFFALSE
               ;; if the condition fails.
               `(if ,condition
                    (progn . ,true-exps)
                  ,iffalse)
             (if uncondit-clauses
                 ;; A non-exit clause.
                 ;; If condition succeeds, run the TRUE-EXPS.
                 ;; Then always go on to run the UNCONDIT-CLAUSES.
                 `(progn (if ,condition
                             (progn . ,true-exps))
                         ,(cond*-convert uncondit-clauses))
               ;; An exiting clause which is also final.
               ;; If there are TRUE-EXPS, run them if CONDITION succeeds.
               (if true-exps
                   `(if ,condition (progn . ,true-exps))
                 ;; Run and return CONDITION.
                 condition)))))))

(defun cond*-match (matchexp true-exps uncondit-clauses iffalse)
  "Generate code to match a match* pattern PATTERN.
Match it against data represented by the expression DATA.
TRUE-EXPS, UNCONDIT-CLAUSES and IFFALSE have the same meanings
as in `cond*-condition'."
  (when (or (null matchexp) (null (cdr-safe matchexp))
            (null (cdr-safe (cdr matchexp)))
            (cdr-safe (cdr (cdr matchexp))))
    (byte-compile-warn-x matchexp "Malformed (match* ...) expression"))
  (let* (raw-result
         (pattern (nth 1 matchexp))
         (data (nth 2 matchexp))
         expression
         (inner-data data)
         ;; Add backtrack aliases for or-subpatterns to cdr of this.
         (backtrack-aliases (list nil))
         run-true-exps
         store-value-swap-outs retrieve-value-swap-outs
         gensym)
    ;; For now, always bind a gensym to the data to be matched.
    (setq gensym (gensym "d") inner-data gensym)
    ;; Process the whole pattern as a subpattern.
    (setq raw-result (cond*-subpat pattern nil nil nil backtrack-aliases inner-data))
    (setq expression (cdr raw-result))
    ;; If there are conditional expressions and some
    ;; unconditional clauses to follow,
    ;; and the pattern bound some variables,
    ;; copy their values into special aliases
    ;; to be copied back at the start of the unconditional clauses.
    (when (and uncondit-clauses true-exps
               (car raw-result))
      (dolist (bound-var (car raw-result))
        (push `(setq ,(gensym "ua") ,(car bound-var)) store-value-swap-outs)
        (push `(,(car bound-var) ,(gensym "ua")) retrieve-value-swap-outs)))

    ;; Make an expression to run the TRUE-EXPS inside our bindings.
    (if store-value-swap-outs
        ;; If we have to store those bindings' values in aliases
        ;; for the UNCONDIT-CLAUSES, do so inside these bindings.
        (setq run-true-exps
              (cond*-bind-pattern-syms
               (car raw-result)
               `(prog1 (progn . ,true-exps) . ,store-value-swap-outs)))
      (setq run-true-exps
            (cond*-bind-pattern-syms
             (car raw-result)
             `(progn . ,true-exps))))
    ;; Run TRUE-EXPS if match succeeded.  Bind our bindings around it.
    (setq expression
          (if (and (null run-true-exps) (null iffalse))
              ;; We MUST compute the expression, even when no decision
              ;; depends on its value, because it may call functions with
              ;; side effects.
              expression
            `(if ,expression
                 ,run-true-exps
               ;; For a non-final exiting clause, run IFFALSE if match failed.
               ;; Don't bind the bindings around it, since
               ;; an exiting clause's bindings don't affect later clauses.
               ,iffalse)))
    ;; For a non-final non-exiting clause,
    ;; always run the UNCONDIT-CLAUSES.
    (if uncondit-clauses
        (setq expression
              `(progn ,expression
                      ,(cond*-bind-pattern-syms
                        (if retrieve-value-swap-outs
                            ;; If we saved the bindings' values after the
                            ;; true-clauses, bind the same variables
                            ;; here to the values we saved then.
                            retrieve-value-swap-outs
                          ;; Otherwise bind them to the values
                          ;; they matched in the pattern.
                          (car raw-result))
                        (cond*-convert uncondit-clauses)))))
    ;; Bind the backtrack-aliases if any.
    ;; We need them bound for the TRUE-EXPS.
    ;; It is harmless to bind them around IFFALSE
    ;; because they are all gensyms anyway.
    (if (cdr backtrack-aliases)
        (setq expression
              `(let ,(mapcar #'cdr (cdr backtrack-aliases))
                 ,expression)))
    (if retrieve-value-swap-outs
        (setq expression
              `(let ,(mapcar #'cadr retrieve-value-swap-outs)
                 ,expression)))
    ;; If we used a gensym, wrap on code to bind it.
    (if gensym
        (if (and (listp expression) (eq (car expression) 'progn))
            `(let ((,gensym ,data)) . ,(cdr expression))
          `(let ((,gensym ,data)) ,expression))
      expression)))

(defun cond*-bind-pattern-syms (bindings expr)
  "Wrap EXPR in code to bind the BINDINGS.
This is used for the bindings specified explicitly in match* patterns."
  ;; They can't have side effects.  Skip them
  ;; if we don't actually need them.
  (if (equal expr '(progn))
      nil
    (if bindings
        (if (eq (car expr) 'progn)
            `(let* ,bindings . ,(cdr expr))
          `(let* ,bindings ,expr))
      expr)))

(defvar cond*-debug-pattern nil)

;; ??? Structure type patterns not implemented yet.
;; ??? Probably should optimize the `nth' calls in handling `list'.

(defun cond*-subpat (subpat cdr-ignore bindings inside-or backtrack-aliases data)
  "Generate code to match the subpattern within `match*'.
SUBPAT is the subpattern to handle.
CDR-IGNORE if true means don't verify there are no extra elts in a list.
BINDINGS is the list of bindings made by
the containing and previous subpatterns of this pattern.
Each element of BINDINGS must have the form (VAR VALUE).
BACKTRACK-ALIASES is used to pass data upward.  Initial call should
pass (list).  The cdr of this collects backtracking aliases made for
variables bound within (or...) patterns so that the caller
can bind them etc.  Each of them has the form (USER-SYMBOL . GENSYM).
DATA is the expression for the data that this subpattern is
supposed to match against.

Return Value has the form (BINDINGS . CONDITION), where
BINDINGS is the list of bindings to be made for SUBPAT
plus the subpatterns that contain/precede it.
Each element of BINDINGS has the form (VAR VALUE).
CONDITION is the condition to be tested to decide
whether SUBPAT (as well as the subpatterns that contain/precede it) matches,"
  (if (equal cond*-debug-pattern subpat)
      (debug))
;;;  (push subpat subpat-log)
  (cond ((eq subpat '_)
         ;; _ as pattern makes no bindings and matches any data.
         (cons bindings t))
        ((memq subpat '(nil t))
         (cons bindings `(eq ,subpat ,data)))
        ((keywordp subpat)
         (cons bindings `(eq ,subpat ,data)))
        ((symbolp subpat)
         (let ((this-binding (assq subpat bindings))
               (this-alias (assq subpat (cdr backtrack-aliases))))
           (if this-binding
               ;; Variable already bound.
               ;; Compare what this variable should be bound to
               ;; to the data it is supposed to match.
               ;; That is because we don't actually bind these bindings
               ;; around the condition-testing expression.
               (cons bindings `(equal ,(cadr this-binding) ,data))
             (if inside-or
                 (let (alias-gensym)
                   (if this-alias
                       ;; Inside `or' subpattern, if this symbol already
                       ;; has an alias for backtracking, just use that.
                       ;; This means the symbol was matched
                       ;; in a previous arm of the `or'.
                       (setq alias-gensym (cdr this-alias))
                     ;; Inside `or' subpattern, but this symbol has no alias,
                     ;; make an alias for it.
                     (setq alias-gensym (gensym "ba"))
                     (push (cons subpat alias-gensym) (cdr backtrack-aliases)))
                   ;; Make a binding for the symbol, to its backtrack-alias,
                   ;; and set the alias (a gensym) to nil.
                   (cons `((,subpat ,alias-gensym) . ,bindings)
                         `(setq ,alias-gensym ,data)))
               ;; Not inside `or' subpattern: ask for a binding for this symbol
               ;; and say it does match whatever datum.
               (cons `((,subpat ,data) . ,bindings)
                     t)))))
        ;; Various constants.
        ((numberp subpat)
         (cons bindings `(eql ,subpat ,data)))
        ;; Regular expressions as strings.
        ((stringp subpat)
         (cons bindings `(string-match ,(concat subpat "\\'") ,data)))
        ;; All other atoms match with `equal'.
        ((not (consp subpat))
         (cons bindings `(equal ,subpat ,data)))
        ((not (consp (cdr subpat)))
         (byte-compile-warn-x subpat "%s subpattern with malformed or missing arguments" (car subpat)))
        ;; Regular expressions specified as list structure.
        ;; (rx REGEXP VARS...)
        ((eq (car subpat) 'rx)
         (let* ((rxpat (concat (rx-to-string (cadr subpat) t) "\\'"))
                (vars (cddr subpat)) setqs (varnum 0)
                (match-exp `(string-match ,rxpat ,data)))
           (if (null vars)
               (cons bindings match-exp)
             ;; There are variables to bind to the matched substrings.
             (if (> (length vars) 10)
                 (byte-compile-warn-x vars "Too many variables specified for matched substrings"))
             (dolist (elt vars)
               (unless (symbolp elt)
                 (byte-compile-warn-x vars "Non-symbol %s given as name for matched substring" elt)))
             ;; Bind these variables to nil, before the pattern.
             (setq bindings (nconc (mapcar #'list vars) bindings))
             ;; Make the expressions to set the variables.
             (setq setqs (mapcar
                          (lambda (var)
                            (prog1 `(setq ,var (match-string ,varnum ,data))
                              (setq varnum (1+ varnum))))
                          vars))
             (cons bindings `(if ,match-exp
                                 (progn ,@setqs t))))))
        ;; Quoted object as constant to match with `eq' or `equal'.
        ((eq (car subpat) 'quote)
         (if (symbolp (car-safe (cdr-safe subpat)))
             (cons bindings `(eq ,subpat ,data))
           (cons bindings `(equal ,subpat ,data))))
        ;; Match a call to `cons' by destructuring.
        ((eq (car subpat) 'cons)
         (let (car-result cdr-result car-exp cdr-exp)
           (setq car-result
                 (cond*-subpat (nth 1 subpat) cdr-ignore bindings inside-or backtrack-aliases `(car ,data)))
           (setq bindings (car car-result)
                 car-exp (cdr car-result))
           (setq cdr-result
                 (cond*-subpat (nth 2 subpat) cdr-ignore bindings inside-or backtrack-aliases `(cdr ,data)))
           (setq bindings (car cdr-result)
                 cdr-exp (cdr cdr-result))
           (cons bindings
                 (cond*-and `((consp ,data) ,car-exp ,cdr-exp)))))
        ;; Match a call to `list' by destructuring.
        ((eq (car subpat) 'list)
         (let ((i 0) expressions)
           ;; Check for bad structure of SUBPAT here?
           (dolist (this-elt (cdr subpat))
             (let ((result
                    (cond*-subpat this-elt cdr-ignore bindings inside-or
                                  backtrack-aliases `(nth ,i ,data))))
               (setq bindings (car result))
               (push `(consp ,(if (zerop i) data `(nthcdr ,i ,data)))
                     expressions)
               (setq i (1+ i))
               (push (cdr result) expressions)))
           ;; Verify that list ends here, if we are supposed to check that.
           (unless cdr-ignore
             (push `(null (nthcdr ,i ,data)) expressions))
           (cons bindings (cond*-and (nreverse expressions)))))
        ;; Match (apply 'vector (backquote-list* LIST...)), destructuring.
        ((eq (car subpat) 'apply)
         ;; We only try to handle the case generated by backquote.
         ;; Convert it to a call to `vector' and handle that.
         (let ((cleaned-up
                `(vector . ,(cond*-un-backquote-list* (cdr (nth 2 subpat))))))
           ;; (cdr (nth 2 subpat)) gets LIST as above.
           (cond*-subpat cleaned-up
                         cdr-ignore bindings inside-or backtrack-aliases data)))
        ;; Match a call to `vector' by destructuring.
        ((eq (car subpat) 'vector)
         (let* ((elts (cdr subpat))
                (length (length elts))
                expressions (i 0))
           (dolist (elt elts)
             (let* ((result
                     (cond*-subpat elt cdr-ignore bindings inside-or
                                   backtrack-aliases `(aref ,i ,data))))
               (setq i (1+ i))
               (setq bindings (car result))
               (push (cdr result) expressions)))
           (cons bindings
                 (cond*-and `((vectorp ,data) (= (length ,data) ,length)
                              . ,(nreverse expressions))))))
        ;; Subpattern to set the cdr-ignore flag.
        ((eq (car subpat) 'cdr-ignore)
         (cond*-subpat (cadr subpat) t bindings inside-or backtrack-aliases data))
        ;; Subpattern to clear the cdr-ignore flag.
        ((eq (car subpat) 'cdr)
         (cond*-subpat (cadr subpat) nil bindings inside-or backtrack-aliases data))
        ;; Handle conjunction subpatterns.
        ((eq (car subpat) 'and)
         (let (expressions)
           ;; Check for bad structure of SUBPAT here?
           (dolist (this-elt (cdr subpat))
             (let ((result
                    (cond*-subpat this-elt cdr-ignore bindings inside-or
                                  backtrack-aliases data)))
               (setq bindings (car result))
               (push (cdr result) expressions)))
           (cons bindings (cond*-and (nreverse expressions)))))
        ;; Handle disjunction subpatterns.
        ((eq (car subpat) 'or)
         ;; The main complexity is unsetting the pattern variables
         ;; that tentatively match in an or-branch that later failed.
         (let (expressions
               (bindings-before-or bindings)
               (aliases-before-or (cdr backtrack-aliases)))
           ;; Check for bad structure of SUBPAT here?
           (dolist (this-elt (cdr subpat))
             (let* ((bindings bindings-before-or)
                    bindings-to-clear expression
                    result)
               (setq result
                     (cond*-subpat this-elt cdr-ignore bindings t
                                   backtrack-aliases data))
               (setq bindings (car result))
               (setq expression (cdr result))
               ;; Were any bindings made by this arm of the disjunction?
               (when (not (eq bindings bindings-before-or))
                 ;; OK, arrange to clear their backtrack aliases
                 ;; if this arm does not match.
                 (setq bindings-to-clear bindings)
                 (let (clearing)
                   ;; For each of those bindings, ...
                   (while (not (eq bindings-to-clear bindings-before-or))
                     ;; ... make an expression to set it to nil, in CLEARING.
                     (let* ((this-variable (caar bindings-to-clear))
                            (this-backtrack (assq this-variable
                                                  (cdr backtrack-aliases))))
                       (push `(setq ,(cdr this-backtrack) nil) clearing))
                     (setq bindings-to-clear (cdr bindings-to-clear)))
                   ;; Wrap EXPRESSION to clear those backtrack aliases
                   ;; if EXPRESSION is false.
                   (setq expression
                         (if (null clearing)
                             expression
                           (if (null (cdr clearing))
                               `(or ,expression
                                    ,(car clearing))
                             `(progn ,@clearing))))))
               (push expression expressions)))
           ;; At end of (or...), EACH variable bound by any arm
           ;; has a backtrack alias gensym.  At run time, that gensym's value
           ;; will be what was bound in the successful arm, or nil.
           ;; Now make a binding for each variable from its alias gensym.
           (let ((aliases (cdr backtrack-aliases)))
             (while (not (eq aliases aliases-before-or))
               (push `(,(caar aliases) ,(cdar aliases)) bindings)
               (pop aliases)))
           (cons bindings `(or . ,(nreverse expressions)))))
        ;; Expand cond*-macro call, treat result as a subpattern.
        ((get (car subpat) 'cond*-expander)
         ;; Treat result as a subpattern.
         (cond*-subpat (funcall (get (car subpat) 'cond*-expander) subpat)
                       cdr-ignore bindings inside-or backtrack-aliases data))
        ((macrop (car subpat))
         (cond*-subpat (macroexpand subpat) cdr-ignore bindings inside-or
                       backtrack-aliases data))
        ;; Simple constrained variable, as in (symbolp x).
        ((functionp (car subpat))
         ;; Without this, nested constrained variables just work.
         (unless (symbolp (cadr subpat))
           (byte-compile-warn-x subpat "Complex pattern nested in constrained variable pattern"))
         (let* ((rest-args (cddr subpat))
                ;; Process VAR to get a binding for it.
                (result (cond*-subpat (cadr subpat) cdr-ignore bindings inside-or backtrack-aliases data))
                (new-bindings (car result))
                (expression (cdr result))
                (combined-exp
                 (cond*-and (list `(,(car subpat) ,data . ,rest-args) expression))))

           (cons new-bindings
                 (cond*-bind-around new-bindings combined-exp))))
        ;; Generalized constrained variable: (constrain VAR EXP)
        ((eq (car subpat) 'constrain)
         ;; Without this, nested constrained variables just work.
         (unless (symbolp (cadr subpat))
           (byte-compile-warn-x subpat "Complex pattern nested in constrained variable pattern"))
         ;; Process VAR to get a binding for it.
         (let ((result
                (cond*-subpat (cadr subpat) cdr-ignore bindings inside-or
                              backtrack-aliases data)))
           (cons (car result)
                 ;; This is the test condition.
                 (cond*-bind-around (car result) (nth 2 subpat)))))
        (t
         (byte-compile-warn-x subpat "Undefined pattern type `%s' in `cond*'" (car subpat)))))

;;; Subroutines of cond*-subpat.

(defun cond*-bind-around (bindings exp)
  "Wrap a `let*' around EXP, to bind those of BINDINGS used in EXP."
  (let ((what-to-bind (cond*-used-within bindings exp)))
    (if what-to-bind
        `(let* ,(nreverse what-to-bind) ,exp)
      exp)))

(defun cond*-used-within (bindings exp)
  "Return the list of those bindings in BINDINGS which EXP refers to.
This operates naively and errs on the side of overinclusion,
and does not distinguish function names from variable names.
That is safe for the purpose this is used for."
  (cond ((symbolp exp)
         (let ((which (assq exp bindings)))
           (if which (list which))))
        ((listp exp)
         (let (combined (rest exp))
           ;; Find the bindings used in each element of EXP
           ;; and merge them together in COMBINED.
           ;; It would be simpler to use dolist at each level,
           ;; but this avoids errors from improper lists.
           (while rest
             (let ((in-this-elt (cond*-used-within bindings (car rest))))
               (while in-this-elt
                 ;; Don't insert the same binding twice.
                 (unless (memq (car-safe in-this-elt) combined)
                   (push (car-safe in-this-elt) combined))
                 (pop in-this-elt)))
             (pop rest))
           combined))))

;; Construct a simplified equivalent to `(and . ,CONJUNCTS),
;; assuming that it will be used only as a truth value.
;; We don't bother checking for nil in CONJUNCTS
;; because that would not normally happen.
(defun cond*-and (conjuncts)
  (setq conjuncts (remq t conjuncts))
  (if (null conjuncts)
      t
    (if (null (cdr conjuncts))
        (car conjuncts)
      `(and . ,conjuncts))))

;; Convert the arguments in a form that calls `backquote-list*'
;; into equivalent args to pass to `list'.
;; We assume the last argument has the form 'LIST.
;; That means quotify each of that list's elements,
;; and preserve the other arguments in front of them.
(defun cond*-un-backquote-list* (args)
  (if (cdr args)
      (cons (car args)
            (cond*-un-backquote-list* (cdr args)))
    (mapcar (lambda (x) (list 'quote x)) (cadr (car args)))))

(provide 'cond-star)

;;; cond-star.el ends here
