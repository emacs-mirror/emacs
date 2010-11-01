;;; pcase.el --- ML-style pattern-matching macro for Elisp

;; Copyright (C) 2010  Free Software Foundation, Inc.

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

;; - provide ways to extend the set of primitives, with some kind of
;;   define-pcase-matcher.  We could easily make it so that (guard BOOLEXP)
;;   could be defined this way, as a shorthand for (pred (lambda (_) BOOLEXP)).
;;   But better would be if we could define new ways to match by having the
;;   extension provide its own `pcase-split-<foo>' thingy.
;; - ideally we'd want (pcase s ((re RE1) E1) ((re RE2) E2)) to be able to
;;   generate a lex-style DFA to decide whether to run E1 or E2.

;;; Code:

(eval-when-compile (require 'cl))

;; Macro-expansion of pcase is reasonably fast, so it's not a problem
;; when byte-compiling a file, but when interpreting the code, if the pcase
;; is in a loop, the repeated macro-expansion becomes terribly costly, so we
;; memoize previous macro expansions to try and avoid recomputing them
;; over and over again.
(defconst pcase-memoize (make-hash-table :weakness t :test 'equal))

;;;###autoload
(defmacro pcase (exp &rest cases)
  "Perform ML-style pattern matching on EXP.
CASES is a list of elements of the form (UPATTERN CODE...).

UPatterns can take the following forms:
  _		matches anything.
  SYMBOL	matches anything and binds it to SYMBOL.
  (or UPAT...)	matches if any of the patterns matches.
  (and UPAT...)	matches if all the patterns match.
  `QPAT		matches if the QPattern QPAT matches.
  (pred PRED)	matches if PRED applied to the object returns non-nil.
  (guard BOOLEXP)	matches if BOOLEXP evaluates to non-nil.

QPatterns can take the following forms:
  (QPAT1 . QPAT2)	matches if QPAT1 matches the car and QPAT2 the cdr.
  ,UPAT			matches if the UPattern UPAT matches.
  STRING			matches if the object is `equal' to STRING.
  ATOM			matches if the object is `eq' to ATOM.
QPatterns for vectors are not implemented yet.

PRED can take the form
  FUNCTION	in which case it gets called with one argument.
  (FUN ARG1 .. ARGN) in which case it gets called with N+1 arguments.
A PRED of the form FUNCTION is equivalent to one of the form (FUNCTION).
PRED patterns can refer to variables bound earlier in the pattern.
E.g. you can match pairs where the cdr is larger than the car with a pattern
like `(,a . ,(pred (< a))) or, with more checks:
`(,(and a (pred numberp)) . ,(and (pred numberp) (pred (< a))))"
  (declare (indent 1) (debug case))
  (or (gethash (cons exp cases) pcase-memoize)
      (puthash (cons exp cases)
               (pcase-expand exp cases)
               pcase-memoize)))

;;;###autoload
(defmacro pcase-let* (bindings body)
  "Like `let*' but where you can use `pcase' patterns for bindings.
BODY should be an expression, and BINDINGS should be a list of bindings
of the form (UPAT EXP)."
  (if (null bindings) body
    `(pcase ,(cadr (car bindings))
       (,(caar bindings) (pcase-let* ,(cdr bindings) ,body))
       ;; FIXME: In many cases `dontcare' would be preferable, so maybe we
       ;; should have `let' and `elet', like we have `case' and `ecase'.
       (t (error "Pattern match failure in `pcase-let'")))))

;;;###autoload
(defmacro pcase-let (bindings body)
  "Like `let' but where you can use `pcase' patterns for bindings.
BODY should be an expression, and BINDINGS should be a list of bindings
of the form (UPAT EXP)."
  (if (null (cdr bindings))
      `(pcase-let* ,bindings ,body)
    (setq bindings (mapcar (lambda (x) (cons (make-symbol "x") x)) bindings))
    `(let ,(mapcar (lambda (binding) (list (nth 0 binding) (nth 2 binding)))
                   bindings)
       (pcase-let*
        ,(mapcar (lambda (binding) (list (nth 1 binding) (nth 0 binding)))
                 bindings)
        ,body))))

(defun pcase-expand (exp cases)
  (let* ((defs (if (symbolp exp) '()
                 (let ((sym (make-symbol "x")))
                   (prog1 `((,sym ,exp)) (setq exp sym)))))
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
                (destructuring-bind (code prevvars res) prev
                  (unless (symbolp res)
                    ;; This is the first repeat, so we have to move
                    ;; the branch to a separate function.
                    (let ((bsym
                           (make-symbol (format "pcase-%d" (length defs)))))
                      (push `(,bsym (lambda ,(mapcar #'car prevvars) ,@code)) defs)
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
                    (when vars          ;New additional vars.
                      (error "The vars %s are only bound in some paths"
                             (mapcar #'car vars)))
                    `(funcall ,res ,@args)))))))
         (main
          (pcase-u
           (mapcar (lambda (case)
                     `((match ,exp . ,(car case))
                       ,(apply-partially
                         (if (pcase-small-branch-p (cdr case))
                             ;; Don't bother sharing multiple
                             ;; occurrences of this leaf since it's small.
                             #'pcase-codegen codegen)
                         (cdr case))))
                   cases))))
    `(let ,defs ,main)))

(defun pcase-codegen (code vars)
  `(let ,(mapcar (lambda (b) (list (car b) (cdr b))) vars)
     ,@code))

(defun pcase-small-branch-p (code)
  (and (= 1 (length code))
       (or (not (consp (car code)))
           (let ((small t))
             (dolist (e (car code))
               (if (consp e) (setq small nil)))
             small))))

;; Try to use `cond' rather than a sequence of `if's, so as to reduce
;; the depth of the generated tree.
(defun pcase-if (test then else)
  (cond
   ((eq else :pcase-dontcare) then)
   ((eq (car-safe else) 'if)
    (if (equal test (nth 1 else))
        ;; Doing a test a second time: get rid of the redundancy.
        ;; FIXME: ideally, this should never happen because the pcase-split-*
        ;; functions should have eliminated such things, but pcase-split-member
        ;; is imprecise, so in practice it does happen occasionally.
        `(if ,test ,then ,@(nthcdr 3 else))
      `(cond (,test ,then)
             (,(nth 1 else) ,(nth 2 else))
             (t ,@(nthcdr 3 else)))))
   ((eq (car-safe else) 'cond)
    `(cond (,test ,then)
           ;; Doing a test a second time: get rid of the redundancy, as above.
           ,@(remove (assoc test else) (cdr else))))
   (t `(if ,test ,then ,else))))

(defun pcase-upat (qpattern)
  (cond
   ((eq (car-safe qpattern) '\,) (cadr qpattern))
   (t (list '\` qpattern))))

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

(defun pcase-u (branches)
  "Expand matcher for rules BRANCHES.
Each BRANCH has the form (MATCH CODE . VARS) where
CODE is the code generator for that branch.
VARS is the set of vars already bound by earlier matches.
MATCH is the pattern that needs to be matched, of the form:
  (match VAR . UPAT)
  (and MATCH ...)
  (or MATCH ...)"
  (when (setq branches (delq nil branches))
    (destructuring-bind (match code &rest vars) (car branches)
      (pcase-u1 (list match) code vars (cdr branches)))))

(defun pcase-and (match matches)
  (if matches `(and ,match ,@matches) match))

(defun pcase-split-match (sym splitter match)
  (case (car match)
    ((match)
     (if (not (eq sym (cadr match)))
         (cons match match)
       (let ((pat (cddr match)))
         (cond
          ;; Hoist `or' and `and' patterns to `or' and `and' matches.
          ((memq (car-safe pat) '(or and))
           (pcase-split-match sym splitter
                              (cons (car pat)
                                    (mapcar (lambda (alt)
                                              `(match ,sym . ,alt))
                                            (cdr pat)))))
          (t (let ((res (funcall splitter (cddr match))))
               (cons (or (car res) match) (or (cdr res) match))))))))
    ((or and)
     (let ((then-alts '())
           (else-alts '())
           (neutral-elem (if (eq 'or (car match)) :pcase-fail :pcase-succeed))
           (zero-elem (if (eq 'or (car match)) :pcase-succeed :pcase-fail)))
       (dolist (alt (cdr match))
         (let ((split (pcase-split-match sym splitter alt)))
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
    (t (error "Uknown MATCH %s" match))))

(defun pcase-split-rest (sym splitter rest)
  (let ((then-rest '())
        (else-rest '()))
    (dolist (branch rest)
      (let* ((match (car branch))
             (code&vars (cdr branch))
             (splitted
              (pcase-split-match sym splitter match)))
        (unless (eq (car splitted) :pcase-fail)
          (push (cons (car splitted) code&vars) then-rest))
        (unless (eq (cdr splitted) :pcase-fail)
          (push (cons (cdr splitted) code&vars) else-rest))))
    (cons (nreverse then-rest) (nreverse else-rest))))

(defun pcase-split-consp (syma symd pat)
  (cond
   ;; A QPattern for a cons, can only go the `then' side.
   ((and (eq (car-safe pat) '\`) (consp (cadr pat)))
    (let ((qpat (cadr pat)))
      (cons `(and (match ,syma . ,(pcase-upat (car qpat)))
                  (match ,symd . ,(pcase-upat (cdr qpat))))
            :pcase-fail)))
   ;; A QPattern but not for a cons, can only go the `else' side.
   ((eq (car-safe pat) '\`) (cons :pcase-fail nil))))

(defun pcase-split-equal (elem pat)
  (cond
   ;; The same match will give the same result.
   ((and (eq (car-safe pat) '\`) (equal (cadr pat) elem))
    (cons :pcase-succeed :pcase-fail))
   ;; A different match will fail if this one succeeds.
   ((and (eq (car-safe pat) '\`)
         ;; (or (integerp (cadr pat)) (symbolp (cadr pat))
         ;;     (consp (cadr pat)))
         )
    (cons :pcase-fail nil))))

(defun pcase-split-member (elems pat)
  ;; Based on pcase-split-equal.
  (cond
   ;; The same match (or a match of membership in a superset) will
   ;; give the same result, but we don't know how to check it.
   ;; (???
   ;;  (cons :pcase-succeed nil))
   ;; A match for one of the elements may succeed or fail.
   ((and (eq (car-safe pat) '\`) (member (cadr pat) elems))
    nil)
   ;; A different match will fail if this one succeeds.
   ((and (eq (car-safe pat) '\`)
         ;; (or (integerp (cadr pat)) (symbolp (cadr pat))
         ;;     (consp (cadr pat)))
         )
    (cons :pcase-fail nil))))

(defun pcase-split-pred (upat pat)
  ;; FIXME: For predicates like (pred (> a)), two such predicates may
  ;; actually refer to different variables `a'.
  (if (equal upat pat)
      (cons :pcase-succeed :pcase-fail)))

(defun pcase-fgrep (vars sexp)
  "Check which of the symbols VARS appear in SEXP."
  (let ((res '()))
    (while (consp sexp)
      (dolist (var (pcase-fgrep vars (pop sexp)))
        (unless (memq var res) (push var res))))
    (and (memq sexp vars) (not (memq sexp res)) (push sexp res))
    res))

;; It's very tempting to use `pcase' below, tho obviously, it'd create
;; bootstrapping problems.
(defun pcase-u1 (matches code vars rest)
  "Return code that runs CODE (with VARS) if MATCHES match.
and otherwise defers to REST which is a list of branches of the form
\(ELSE-MATCH ELSE-CODE . ELSE-VARS)."
  ;; Depending on the order in which we choose to check each of the MATCHES,
  ;; the resulting tree may be smaller or bigger.  So in general, we'd want
  ;; to be careful to chose the "optimal" order.  But predicate
  ;; patterns make this harder because they create dependencies
  ;; between matches.  So we don't bother trying to reorder anything.
  (cond
   ((null matches) (funcall code vars))
   ((eq :pcase-fail (car matches)) (pcase-u rest))
   ((eq :pcase-succeed (car matches))
    (pcase-u1 (cdr matches) code vars rest))
   ((eq 'and (caar matches))
    (pcase-u1 (append (cdar matches) (cdr matches)) code vars rest))
   ((eq 'or (caar matches))
    (let* ((alts (cdar matches))
           (var (if (eq (caar alts) 'match) (cadr (car alts))))
           (simples '()) (others '()))
      (when var
        (dolist (alt alts)
          (if (and (eq (car alt) 'match) (eq var (cadr alt))
                   (let ((upat (cddr alt)))
                     (and (eq (car-safe upat) '\`)
                          (or (integerp (cadr upat)) (symbolp (cadr upat))
                              (stringp (cadr upat))))))
              (push (cddr alt) simples)
            (push alt others))))
      (cond
       ((null alts) (error "Please avoid it") (pcase-u rest))
       ((> (length simples) 1)
        ;; De-hoist the `or' MATCH into an `or' pattern that will be
        ;; turned into a `memq' below.
        (pcase-u1 (cons `(match ,var or . ,(nreverse simples)) (cdr matches))
                  code vars
                  (if (null others) rest
                    (cons (list*
                           (pcase-and (if (cdr others)
                                          (cons 'or (nreverse others))
                                        (car others))
                                      (cdr matches))
                           code vars)
                          rest))))
       (t
        (pcase-u1 (cons (pop alts) (cdr matches)) code vars
                  (if (null alts) (progn (error "Please avoid it") rest)
                    (cons (list*
                           (pcase-and (if (cdr alts)
                                          (cons 'or alts) (car alts))
                                      (cdr matches))
                           code vars)
                          rest)))))))
   ((eq 'match (caar matches))
    (destructuring-bind (op sym &rest upat) (pop matches)
      (cond
       ((memq upat '(t _)) (pcase-u1 matches code vars rest))
       ((eq upat 'dontcare) :pcase-dontcare)
       ((functionp upat)  (error "Feature removed, use (pred %s)" upat))
       ((memq (car-safe upat) '(guard pred))
        (destructuring-bind (then-rest &rest else-rest)
            (pcase-split-rest
             sym (apply-partially 'pcase-split-pred upat) rest)
          (pcase-if (if (and (eq (car upat) 'pred) (symbolp (cadr upat)))
                        `(,(cadr upat) ,sym)
                      (let* ((exp (cadr upat))
                             ;; `vs' is an upper bound on the vars we need.
                             (vs (pcase-fgrep (mapcar #'car vars) exp))
                             (call (cond
                                    ((eq 'guard (car upat)) exp)
                                    ((functionp exp) `(,exp ,sym))
                                    (t `(,@exp ,sym)))))
                        (if (null vs)
                            call
                          ;; Let's not replace `vars' in `exp' since it's
                          ;; too difficult to do it right, instead just
                          ;; let-bind `vars' around `exp'.
                          `(let ,(mapcar (lambda (var)
                                           (list var (cdr (assq var vars))))
                                         vs)
                             ;; FIXME: `vars' can capture `sym'.  E.g.
                             ;; (pcase x ((and `(,x . ,y) (pred (fun x)))))
                             ,call))))
                    (pcase-u1 matches code vars then-rest)
                    (pcase-u else-rest))))
       ((symbolp upat)
        (pcase-u1 matches code (cons (cons upat sym) vars) rest))
       ((eq (car-safe upat) '\`)
        (pcase-q1 sym (cadr upat) matches code vars rest))
       ((eq (car-safe upat) 'or)
        (let ((all (> (length (cdr upat)) 1))
              (memq-fine t))
          (when all
            (dolist (alt (cdr upat))
              (unless (and (eq (car-safe alt) '\`)
                           (or (symbolp (cadr alt)) (integerp (cadr alt))
                               (setq memq-fine nil)
                               (stringp (cadr alt))))
                (setq all nil))))
          (if all
              ;; Use memq for (or `a `b `c `d) rather than a big tree.
              (let ((elems (mapcar 'cadr (cdr upat))))
                (destructuring-bind (then-rest &rest else-rest)
                    (pcase-split-rest
                     sym (apply-partially 'pcase-split-member elems) rest)
                  (pcase-if `(,(if memq-fine #'memq #'member) ,sym ',elems)
                            (pcase-u1 matches code vars then-rest)
                            (pcase-u else-rest))))
            (pcase-u1 (cons `(match ,sym ,@(cadr upat)) matches) code vars
                      (append (mapcar (lambda (upat)
                                        `((and (match ,sym . ,upat) ,@matches)
                                          ,code ,@vars))
                                      (cddr upat))
                              rest)))))
       ((eq (car-safe upat) 'and)
        (pcase-u1 (append (mapcar (lambda (upat) `(match ,sym ,@upat)) (cdr upat))
                          matches)
                  code vars rest))
       ((eq (car-safe upat) 'not)
        ;; FIXME: The implementation below is naive and results in
        ;; inefficient code.
        ;; To make it work right, we would need to turn pcase-u1's
        ;; `code' and `vars' into a single argument of the same form as
        ;; `rest'.  We would also need to split this new `then-rest' argument
        ;; for every test (currently we don't bother to do it since
        ;; it's only useful for odd patterns like (and `(PAT1 . PAT2)
        ;; `(PAT3 . PAT4)) which the programmer can easily rewrite
        ;; to the more efficient `(,(and PAT1 PAT3) . ,(and PAT2 PAT4))).
        (pcase-u1 `((match ,sym . ,(cadr upat)))
                  (lexical-let ((rest rest))
                    ;; FIXME: This codegen is not careful to share its
                    ;; code if used several times: code blow up is likely.
                    (lambda (vars)
                      ;; `vars' will likely contain bindings which are
                      ;; not always available in other paths to
                      ;; `rest', so there' no point trying to pass
                      ;; them down.
                      (pcase-u rest)))
                  vars
                  (list `((and . ,matches) ,code . ,vars))))
       (t (error "Unknown upattern `%s'" upat)))))
   (t (error "Incorrect MATCH %s" (car matches)))))

(defun pcase-q1 (sym qpat matches code vars rest)
  "Return code that runs CODE if SYM matches QPAT and if MATCHES match.
and if not, defers to REST which is a list of branches of the form
\(OTHER_MATCH OTHER-CODE . OTHER-VARS)."
  (cond
   ((eq (car-safe qpat) '\,) (error "Can't use `,UPATTERN"))
   ((floatp qpat) (error "Floating point patterns not supported"))
   ((vectorp qpat)
    ;; FIXME.
    (error "Vector QPatterns not implemented yet"))
   ((consp qpat)
    (let ((syma (make-symbol "xcar"))
          (symd (make-symbol "xcdr")))
      (destructuring-bind (then-rest &rest else-rest)
          (pcase-split-rest sym (apply-partially 'pcase-split-consp syma symd)
                            rest)
        (pcase-if `(consp ,sym)
                  `(let ((,syma (car ,sym))
                         (,symd (cdr ,sym)))
                     ,(pcase-u1 `((match ,syma . ,(pcase-upat (car qpat)))
                                  (match ,symd . ,(pcase-upat (cdr qpat)))
                                  ,@matches)
                                code vars then-rest))
                  (pcase-u else-rest)))))
   ((or (integerp qpat) (symbolp qpat) (stringp qpat))
    (destructuring-bind (then-rest &rest else-rest)
        (pcase-split-rest sym (apply-partially 'pcase-split-equal qpat) rest)
      (pcase-if `(,(if (stringp qpat) #'equal #'eq) ,sym ',qpat)
                (pcase-u1 matches code vars then-rest)
                (pcase-u else-rest))))
   (t (error "Unkown QPattern %s" qpat))))


(provide 'pcase)
;;; pcase.el ends here
