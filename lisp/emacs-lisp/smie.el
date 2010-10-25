;;; smie.el --- Simple Minded Indentation Engine

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: languages, lisp, internal, parsing, indentation

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; While working on the SML indentation code, the idea grew that maybe
;; I could write something generic to do the same thing, and at the
;; end of working on the SML code, I had a pretty good idea of what it
;; could look like.  That idea grew stronger after working on
;; LaTeX indentation.
;;
;; So at some point I decided to try it out, by writing a new
;; indentation code for Coq while trying to keep most of the code
;; "table driven", where only the tables are Coq-specific.  The result
;; (which was used for Beluga-mode as well) turned out to be based on
;; something pretty close to an operator precedence parser.

;; So here is another rewrite, this time following the actual principles of
;; operator precedence grammars.  Why OPG?  Even though they're among the
;; weakest kinds of parsers, these parsers have some very desirable properties
;; for Emacs:
;; - most importantly for indentation, they work equally well in either
;;   direction, so you can use them to parse backward from the indentation
;;   point to learn the syntactic context;
;; - they work locally, so there's no need to keep a cache of
;;   the parser's state;
;; - because of that locality, indentation also works just fine when earlier
;;   parts of the buffer are syntactically incorrect since the indentation
;;   looks at "as little as possible" of the buffer to make an indentation
;;   decision.
;; - they typically have no error handling and can't even detect a parsing
;;   error, so we don't have to worry about what to do in case of a syntax
;;   error because the parser just automatically does something.  Better yet,
;;   we can afford to use a sloppy grammar.

;; The development (especially the parts building the 2D precedence
;; tables and then computing the precedence levels from it) is largely
;; inspired from page 187-194 of "Parsing techniques" by Dick Grune
;; and Ceriel Jacobs (BookBody.pdf available at
;; http://www.cs.vu.nl/~dick/PTAPG.html).
;;
;; OTOH we had to kill many chickens, read many coffee grounds, and practice
;; untold numbers of black magic spells, to come up with the indentation code.
;; Since then, some of that code has been beaten into submission, but the
;; smie-indent-keyword is still pretty obscure.

;;; Code:

;; FIXME: I think the behavior on empty lines is wrong.  It shouldn't
;; look at the next token on subsequent lines.

(eval-when-compile (require 'cl))

(defvar comment-continue)
(declare-function comment-string-strip "newcomment" (str beforep afterp))

;;; Building precedence level tables from BNF specs.

;; We have 4 different representations of a "grammar":
;; - a BNF table, which is a list of BNF rules of the form
;;   (NONTERM RHS1 ... RHSn) where each RHS is a list of terminals (tokens)
;;   or nonterminals.  Any element in these lists which does not appear as
;;   the `car' of a BNF rule is taken to be a terminal.
;; - A list of precedences (key word "precs"), is a list, sorted
;;   from lowest to highest precedence, of precedence classes that
;;   have the form (ASSOCIATIVITY TERMINAL1 .. TERMINALn), where
;;   ASSOCIATIVITY can be `assoc', `left', `right' or `nonassoc'.
;; - a 2 dimensional precedence table (key word "prec2"), is a 2D
;;   table recording the precedence relation (can be `<', `=', `>', or
;;   nil) between each pair of tokens.
;; - a precedence-level table (key word "levels"), while is a alist
;;   giving for each token its left and right precedence level (a
;;   number or nil).  This is used in `smie-op-levels'.
;; The prec2 tables are only intermediate data structures: the source
;; code normally provides a mix of BNF and precs tables, and then
;; turns them into a levels table, which is what's used by the rest of
;; the SMIE code.

(defun smie-set-prec2tab (table x y val &optional override)
  (assert (and x y))
  (let* ((key (cons x y))
         (old (gethash key table)))
    (if (and old (not (eq old val)))
        (if (and override (gethash key override))
            ;; FIXME: The override is meant to resolve ambiguities,
            ;; but it also hides real conflicts.  It would be great to
            ;; be able to distinguish the two cases so that overrides
            ;; don't hide real conflicts.
            (puthash key (gethash key override) table)
          (display-warning 'smie (format "Conflict: %s %s/%s %s" x old val y)))
      (puthash key val table))))

(defun smie-precs-precedence-table (precs)
  "Compute a 2D precedence table from a list of precedences.
PRECS should be a list, sorted by precedence (e.g. \"+\" will
come before \"*\"), of elements of the form \(left OP ...)
or (right OP ...) or (nonassoc OP ...) or (assoc OP ...).  All operators in
one of those elements share the same precedence level and associativity."
  (let ((prec2-table (make-hash-table :test 'equal)))
    (dolist (prec precs)
      (dolist (op (cdr prec))
        (let ((selfrule (cdr (assq (car prec)
                                   '((left . >) (right . <) (assoc . =))))))
          (when selfrule
            (dolist (other-op (cdr prec))
              (smie-set-prec2tab prec2-table op other-op selfrule))))
        (let ((op1 '<) (op2 '>))
          (dolist (other-prec precs)
            (if (eq prec other-prec)
                (setq op1 '> op2 '<)
              (dolist (other-op (cdr other-prec))
                (smie-set-prec2tab prec2-table op other-op op2)
                (smie-set-prec2tab prec2-table other-op op op1)))))))
    prec2-table))

(defun smie-merge-prec2s (&rest tables)
  (if (null (cdr tables))
      (car tables)
    (let ((prec2 (make-hash-table :test 'equal)))
      (dolist (table tables)
        (maphash (lambda (k v)
                   (if (consp k)
                       (smie-set-prec2tab prec2 (car k) (cdr k) v)
                     (if (and (gethash k prec2)
                              (not (equal (gethash k prec2) v)))
                         (error "Conflicting values for %s property" k)
                       (puthash k v prec2))))
                 table))
      prec2)))

(defun smie-bnf-precedence-table (bnf &rest precs)
  (let ((nts (mapcar 'car bnf))         ;Non-terminals
        (first-ops-table ())
        (last-ops-table ())
        (first-nts-table ())
        (last-nts-table ())
        (prec2 (make-hash-table :test 'equal))
        (override (apply 'smie-merge-prec2s
                         (mapcar 'smie-precs-precedence-table precs)))
        again)
    (dolist (rules bnf)
      (let ((nt (car rules))
            (last-ops ())
            (first-ops ())
            (last-nts ())
            (first-nts ()))
        (dolist (rhs (cdr rules))
          (unless (consp rhs)
            (signal 'wrong-type-argument `(consp ,rhs)))
          (if (not (member (car rhs) nts))
              (pushnew (car rhs) first-ops)
            (pushnew (car rhs) first-nts)
            (when (consp (cdr rhs))
              ;; If the first is not an OP we add the second (which
              ;; should be an OP if BNF is an "operator grammar").
              ;; Strictly speaking, this should only be done if the
              ;; first is a non-terminal which can expand to a phrase
              ;; without any OP in it, but checking doesn't seem worth
              ;; the trouble, and it lets the writer of the BNF
              ;; be a bit more sloppy by skipping uninteresting base
              ;; cases which are terminals but not OPs.
              (assert (not (member (cadr rhs) nts)))
              (pushnew (cadr rhs) first-ops)))
          (let ((shr (reverse rhs)))
            (if (not (member (car shr) nts))
                (pushnew (car shr) last-ops)
              (pushnew (car shr) last-nts)
              (when (consp (cdr shr))
                (assert (not (member (cadr shr) nts)))
                (pushnew (cadr shr) last-ops)))))
        (push (cons nt first-ops) first-ops-table)
        (push (cons nt last-ops) last-ops-table)
        (push (cons nt first-nts) first-nts-table)
        (push (cons nt last-nts) last-nts-table)))
    ;; Compute all first-ops by propagating the initial ones we have
    ;; now, according to first-nts.
    (setq again t)
    (while (prog1 again (setq again nil))
      (dolist (first-nts first-nts-table)
        (let* ((nt (pop first-nts))
               (first-ops (assoc nt first-ops-table)))
          (dolist (first-nt first-nts)
            (dolist (op (cdr (assoc first-nt first-ops-table)))
              (unless (member op first-ops)
                (setq again t)
                (push op (cdr first-ops))))))))
    ;; Same thing for last-ops.
    (setq again t)
    (while (prog1 again (setq again nil))
      (dolist (last-nts last-nts-table)
        (let* ((nt (pop last-nts))
               (last-ops (assoc nt last-ops-table)))
          (dolist (last-nt last-nts)
            (dolist (op (cdr (assoc last-nt last-ops-table)))
              (unless (member op last-ops)
                (setq again t)
                (push op (cdr last-ops))))))))
    ;; Now generate the 2D precedence table.
    (dolist (rules bnf)
      (dolist (rhs (cdr rules))
        (while (cdr rhs)
          (cond
           ((member (car rhs) nts)
            (dolist (last (cdr (assoc (car rhs) last-ops-table)))
              (smie-set-prec2tab prec2 last (cadr rhs) '> override)))
           ((member (cadr rhs) nts)
            (dolist (first (cdr (assoc (cadr rhs) first-ops-table)))
              (smie-set-prec2tab prec2 (car rhs) first '< override))
            (if (and (cddr rhs) (not (member (car (cddr rhs)) nts)))
                (smie-set-prec2tab prec2 (car rhs) (car (cddr rhs))
                                   '= override)))
           (t (smie-set-prec2tab prec2 (car rhs) (cadr rhs) '= override)))
          (setq rhs (cdr rhs)))))
    ;; Keep track of which tokens are openers/closer, so they can get a nil
    ;; precedence in smie-prec2-levels.
    (puthash :smie-open/close-alist (smie-bnf-classify bnf) prec2)
    prec2))

;; (defun smie-prec2-closer-alist (prec2 include-inners)
;;   "Build a closer-alist from a PREC2 table.
;; The return value is in the same form as `smie-closer-alist'.
;; INCLUDE-INNERS if non-nil means that inner keywords will be included
;; in the table, e.g. the table will include things like (\"if\" . \"else\")."
;;   (let* ((non-openers '())
;;          (non-closers '())
;;          ;; For each keyword, this gives the matching openers, if any.
;;          (openers (make-hash-table :test 'equal))
;;          (closers '())
;;          (done nil))
;;     ;; First, find the non-openers and non-closers.
;;     (maphash (lambda (k v)
;;                (unless (or (eq v '<) (member (cdr k) non-openers))
;;                  (push (cdr k) non-openers))
;;                (unless (or (eq v '>) (member (car k) non-closers))
;;                  (push (car k) non-closers)))
;;              prec2)
;;     ;; Then find the openers and closers.
;;     (maphash (lambda (k _)
;;                (unless (member (car k) non-openers)
;;                  (puthash (car k) (list (car k)) openers))
;;                (unless (or (member (cdr k) non-closers)
;;                            (member (cdr k) closers))
;;                  (push (cdr k) closers)))
;;              prec2)
;;     ;; Then collect the matching elements.
;;     (while (not done)
;;       (setq done t)
;;       (maphash (lambda (k v)
;;                  (when (eq v '=)
;;                    (let ((aopeners (gethash (car k) openers))
;;                          (dopeners (gethash (cdr k) openers))
;;                          (new nil))
;;                      (dolist (o aopeners)
;;                        (unless (member o dopeners)
;;                          (setq new t)
;;                          (push o dopeners)))
;;                      (when new
;;                        (setq done nil)
;;                        (puthash (cdr k) dopeners openers)))))
;;                prec2))
;;     ;; Finally, dump the resulting table.
;;     (let ((alist '()))
;;       (maphash (lambda (k v)
;;                  (when (or include-inners (member k closers))
;;                    (dolist (opener v)
;;                      (unless (equal opener k)
;;                        (push (cons opener k) alist)))))
;;                openers)
;;       alist)))

(defun smie-bnf-closer-alist (bnf &optional no-inners)
  ;; We can also build this closer-alist table from a prec2 table,
  ;; but it takes more work, and the order is unpredictable, which
  ;; is a problem for smie-close-block.
  ;; More convenient would be to build it from a levels table since we
  ;; always have this table (contrary to the BNF), but it has all the
  ;; disadvantages of the prec2 case plus the disadvantage that the levels
  ;; table has lost some info which would result in extra invalid pairs.
  "Build a closer-alist from a BNF table.
The return value is in the same form as `smie-closer-alist'.
NO-INNERS if non-nil means that inner keywords will be excluded
from the table, e.g. the table will not include things like (\"if\" . \"else\")."
  (let ((nts (mapcar #'car bnf))        ;non terminals.
        (alist '()))
    (dolist (nt bnf)
      (dolist (rhs (cdr nt))
        (unless (or (< (length rhs) 2) (member (car rhs) nts))
          (if no-inners
              (let ((last (car (last rhs))))
                (unless (member last nts)
                  (pushnew (cons (car rhs) last) alist :test #'equal)))
            ;; Reverse so that the "real" closer gets there first,
            ;; which is important for smie-close-block.
            (dolist (term (reverse (cdr rhs)))
              (unless (member term nts)
                (pushnew (cons (car rhs) term) alist :test #'equal)))))))
    (nreverse alist)))
    
(defun smie-bnf-classify (bnf)
  "Return a table classifying terminals.
Each terminal can either be an `opener', a `closer', or neither."
  (let ((table (make-hash-table :test #'equal))
        (alist '()))
    (dolist (category bnf)
      (puthash (car category) 'neither table) ;Remove non-terminals.
      (dolist (rhs (cdr category))
        (if (null (cdr rhs))
            (puthash (pop rhs) 'neither table)
          (let ((first (pop rhs)))
            (puthash first
                     (if (memq (gethash first table) '(nil opener))
                         'opener 'neither)
                     table))
          (while (cdr rhs)
            (puthash (pop rhs) 'neither table)) ;Remove internals.
          (let ((last (pop rhs)))
            (puthash last
                     (if (memq (gethash last table) '(nil closer))
                         'closer 'neither)
                     table)))))
    (maphash (lambda (tok v)
               (when (memq v '(closer opener))
                 (push (cons tok v) alist)))
             table)
    alist))

(defun smie-debug--prec2-cycle (csts)
  "Return a cycle in CSTS, assuming there's one.
CSTS is a list of pairs representing arcs in a graph."
  ;; A PATH is of the form (START . REST) where REST is a reverse
  ;; list of nodes through which the path goes.
  (let ((paths (mapcar (lambda (pair) (list (car pair) (cdr pair))) csts))
        (cycle nil))
    (while (null cycle)
      (dolist (path (prog1 paths (setq paths nil)))
        (dolist (cst csts)
          (when (eq (car cst) (nth 1 path))
            (if (eq (cdr cst) (car path))
                (setq cycle path)
              (push (cons (car path) (cons (cdr cst) (cdr path)))
                    paths))))))
    (cons (car cycle) (nreverse (cdr cycle)))))
            
(defun smie-debug--describe-cycle (table cycle)
  (let ((names
         (mapcar (lambda (val)
                   (let ((res nil))
                     (dolist (elem table)
                       (if (eq (cdr elem) val)
                           (push (concat "." (car elem)) res))
                       (if (eq (cddr elem) val)
                           (push (concat (car elem) ".") res)))
                     (assert res)
                     res))
                 cycle)))
    (mapconcat
     (lambda (elems) (mapconcat 'identity elems "="))
     (append names (list (car names)))
     " < ")))

(defun smie-prec2-levels (prec2)
  ;; FIXME: Rather than only return an alist of precedence levels, we should
  ;; also extract other useful data from it:
  ;; - better default indentation rules (i.e. non-zero indentation after inner
  ;;   keywords like the "in" of "let..in..end") for smie-indent-after-keyword.
  ;; Of course, maybe those things would be even better handled in the
  ;; bnf->prec function.
  "Take a 2D precedence table and turn it into an alist of precedence levels.
PREC2 is a table as returned by `smie-precs-precedence-table' or
`smie-bnf-precedence-table'."
  ;; For each operator, we create two "variables" (corresponding to
  ;; the left and right precedence level), which are represented by
  ;; cons cells.  Those are the very cons cells that appear in the
  ;; final `table'.  The value of each "variable" is kept in the `car'.
  (let ((table ())
        (csts ())
        (eqs ())
        tmp x y)
    ;; From `prec2' we construct a list of constraints between
    ;; variables (aka "precedence levels").  These can be either
    ;; equality constraints (in `eqs') or `<' constraints (in `csts').
    (maphash (lambda (k v)
               (when (consp k)
                 (if (setq tmp (assoc (car k) table))
                     (setq x (cddr tmp))
                   (setq x (cons nil nil))
                   (push (cons (car k) (cons nil x)) table))
                 (if (setq tmp (assoc (cdr k) table))
                     (setq y (cdr tmp))
                   (setq y (cons nil (cons nil nil)))
                   (push (cons (cdr k) y) table))
                 (ecase v
                   (= (push (cons x y) eqs))
                   (< (push (cons x y) csts))
                   (> (push (cons y x) csts)))))
             prec2)
    ;; First process the equality constraints.
    (let ((eqs eqs))
      (while eqs
        (let ((from (caar eqs))
              (to (cdar eqs)))
          (setq eqs (cdr eqs))
          (if (eq to from)
              nil                   ;Nothing to do.
            (dolist (other-eq eqs)
              (if (eq from (cdr other-eq)) (setcdr other-eq to))
              (when (eq from (car other-eq))
                ;; This can happen because of `assoc' settings in precs
                ;; or because of a rhs like ("op" foo "op").
                (setcar other-eq to)))
            (dolist (cst csts)
              (if (eq from (cdr cst)) (setcdr cst to))
              (if (eq from (car cst)) (setcar cst to)))))))
    ;; Then eliminate trivial constraints iteratively.
    (let ((i 0))
      (while csts
        (let ((rhvs (mapcar 'cdr csts))
              (progress nil))
          (dolist (cst csts)
            (unless (memq (car cst) rhvs)
              (setq progress t)
              ;; We could give each var in a given iteration the same value,
              ;; but we can also give them arbitrarily different values.
              ;; Basically, these are vars between which there is no
              ;; constraint (neither equality nor inequality), so
              ;; anything will do.
              ;; We give them arbitrary values, which means that we
              ;; replace the "no constraint" case with either > or <
              ;; but not =.  The reason we do that is so as to try and
              ;; distinguish associative operators (which will have
              ;; left = right).
              (unless (caar cst)
                (setcar (car cst) i)
                (incf i))
              (setq csts (delq cst csts))))
          (unless progress
            (error "Can't resolve the precedence cycle: %s"
                   (smie-debug--describe-cycle
                    table (smie-debug--prec2-cycle csts)))))
        (incf i 10))
      ;; Propagate equalities back to their source.
      (dolist (eq (nreverse eqs))
        (assert (or (null (caar eq)) (eq (car eq) (cdr eq))))
        (setcar (car eq) (cadr eq)))
      ;; Finally, fill in the remaining vars (which only appeared on the
      ;; right side of the < constraints).
      (let ((classification-table (gethash :smie-open/close-alist prec2)))
        (dolist (x table)
          ;; When both sides are nil, it means this operator binds very
          ;; very tight, but it's still just an operator, so we give it
          ;; the highest precedence.
          ;; OTOH if only one side is nil, it usually means it's like an
          ;; open-paren, which is very important for indentation purposes,
          ;; so we keep it nil if so, to make it easier to recognize.
          (unless (or (nth 1 x)
                      (eq 'opener (cdr (assoc (car x) classification-table))))
            (setf (nth 1 x) i)
            (incf i))                   ;See other (incf i) above.
          (unless (or (nth 2 x)
                      (eq 'closer (cdr (assoc (car x) classification-table))))
            (setf (nth 2 x) i)
            (incf i)))))                ;See other (incf i) above.
    table))

;;; Parsing using a precedence level table.

(defvar smie-op-levels 'unset
  "List of token parsing info.
Each element is of the form (TOKEN LEFT-LEVEL RIGHT-LEVEL).
Parsing is done using an operator precedence parser.
LEFT-LEVEL and RIGHT-LEVEL can be either numbers or nil, where nil
means that this operator does not bind on the corresponding side,
i.e. a LEFT-LEVEL of nil means this is a token that behaves somewhat like
an open-paren, whereas a RIGHT-LEVEL of nil would correspond to something
like a close-paren.")

(defvar smie-forward-token-function 'smie-default-forward-token
  "Function to scan forward for the next token.
Called with no argument should return a token and move to its end.
If no token is found, return nil or the empty string.
It can return nil when bumping into a parenthesis, which lets SMIE
use syntax-tables to handle them in efficient C code.")

(defvar smie-backward-token-function 'smie-default-backward-token
  "Function to scan backward the previous token.
Same calling convention as `smie-forward-token-function' except
it should move backward to the beginning of the previous token.")

(defalias 'smie-op-left 'car)
(defalias 'smie-op-right 'cadr)

(defun smie-default-backward-token ()
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (progn (if (zerop (skip-syntax-backward "."))
              (skip-syntax-backward "w_'"))
          (point))))

(defun smie-default-forward-token ()
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn (if (zerop (skip-syntax-forward "."))
              (skip-syntax-forward "w_'"))
          (point))))

(defun smie--associative-p (toklevels)
  ;; in "a + b + c" we want to stop at each +, but in
  ;; "if a then b elsif c then d else c" we don't want to stop at each keyword.
  ;; To distinguish the two cases, we made smie-prec2-levels choose
  ;; different levels for each part of "if a then b else c", so that
  ;; by checking if the left-level is equal to the right level, we can
  ;; figure out that it's an associative operator.
  ;; This is not 100% foolproof, tho, since the "elsif" will have to have
  ;; equal left and right levels (since it's optional), so smie-next-sexp
  ;; has to be careful to distinguish those different cases.
  (eq (smie-op-left toklevels) (smie-op-right toklevels)))

(defun smie-next-sexp (next-token next-sexp op-forw op-back halfsexp)
  "Skip over one sexp.
NEXT-TOKEN is a function of no argument that moves forward by one
token (after skipping comments if needed) and returns it.
NEXT-SEXP is a lower-level function to skip one sexp.
OP-FORW is the accessor to the forward level of the level data.
OP-BACK is the accessor to the backward level of the level data.
HALFSEXP if non-nil, means skip over a partial sexp if needed.  I.e. if the
first token we see is an operator, skip over its left-hand-side argument.
Possible return values:
  (FORW-LEVEL POS TOKEN): we couldn't skip TOKEN because its back-level
    is too high.  FORW-LEVEL is the forw-level of TOKEN,
    POS is its start position in the buffer.
  (t POS TOKEN): same thing when we bump on the wrong side of a paren.
  (nil POS TOKEN): we skipped over a paren-like pair.
  nil: we skipped over an identifier, matched parentheses, ..."
  (catch 'return
    (let ((levels ()))
      (while
          (let* ((pos (point))
                 (token (funcall next-token))
                 (toklevels (cdr (assoc token smie-op-levels))))
            (cond
             ((null toklevels)
              (when (zerop (length token))
                (condition-case err
                    (progn (goto-char pos) (funcall next-sexp 1) nil)
                  (scan-error (throw 'return
                                     (list t (caddr err)
                                           (buffer-substring-no-properties
                                            (caddr err)
                                            (+ (caddr err)
                                               (if (< (point) (caddr err))
                                                   -1 1)))))))
                (if (eq pos (point))
                    ;; We did not move, so let's abort the loop.
                    (throw 'return (list t (point))))))
             ((null (funcall op-back toklevels))
              ;; A token like a paren-close.
              (assert (funcall op-forw toklevels)) ;Otherwise, why mention it?
              (push toklevels levels))
             (t
              (while (and levels (< (funcall op-back toklevels)
                                    (funcall op-forw (car levels))))
                (setq levels (cdr levels)))
              (cond
               ((null levels)
                (if (and halfsexp (funcall op-forw toklevels))
                    (push toklevels levels)
                  (throw 'return
                         (prog1 (list (or (car toklevels) t) (point) token)
                           (goto-char pos)))))
               (t
                (let ((lastlevels levels))
                  (if (and levels (= (funcall op-back toklevels)
                                     (funcall op-forw (car levels))))
                      (setq levels (cdr levels)))
                  ;; We may have found a match for the previously pending
                  ;; operator.  Is this the end?
                  (cond
                   ;; Keep looking as long as we haven't matched the
                   ;; topmost operator.
                   (levels
                    (if (funcall op-forw toklevels)
                        (push toklevels levels)))
                   ;; We matched the topmost operator.  If the new operator
                   ;; is the last in the corresponding BNF rule, we're done.
                   ((null (funcall op-forw toklevels))
                    ;; It is the last element, let's stop here.
                    (throw 'return (list nil (point) token)))
                   ;; If the new operator is not the last in the BNF rule,
                   ;; ans is not associative, it's one of the inner operators
                   ;; (like the "in" in "let .. in .. end"), so keep looking.
                   ((not (smie--associative-p toklevels))
                    (push toklevels levels))
                   ;; The new operator is associative.  Two cases:
                   ;; - it's really just an associative operator (like + or ;)
                   ;;   in which case we should have stopped right before.
                   ((and lastlevels
                         (smie--associative-p (car lastlevels)))
                    (throw 'return
                           (prog1 (list (or (car toklevels) t) (point) token)
                             (goto-char pos))))
                   ;; - it's an associative operator within a larger construct
                   ;;   (e.g. an "elsif"), so we should just ignore it and keep
                   ;;   looking for the closing element.
                   (t (setq levels lastlevels))))))))
            levels)
        (setq halfsexp nil)))))

(defun smie-backward-sexp (&optional halfsexp)
  "Skip over one sexp.
HALFSEXP if non-nil, means skip over a partial sexp if needed.  I.e. if the
first token we see is an operator, skip over its left-hand-side argument.
Possible return values:
  (LEFT-LEVEL POS TOKEN): we couldn't skip TOKEN because its right-level
    is too high.  LEFT-LEVEL is the left-level of TOKEN,
    POS is its start position in the buffer.
  (t POS TOKEN): same thing but for an open-paren or the beginning of buffer.
  (nil POS TOKEN): we skipped over a paren-like pair.
  nil: we skipped over an identifier, matched parentheses, ..."
  (smie-next-sexp
   (indirect-function smie-backward-token-function)
   (indirect-function 'backward-sexp)
   (indirect-function 'smie-op-left)
   (indirect-function 'smie-op-right)
   halfsexp))

(defun smie-forward-sexp (&optional halfsexp)
  "Skip over one sexp.
HALFSEXP if non-nil, means skip over a partial sexp if needed.  I.e. if the
first token we see is an operator, skip over its left-hand-side argument.
Possible return values:
  (RIGHT-LEVEL POS TOKEN): we couldn't skip TOKEN because its left-level
    is too high.  RIGHT-LEVEL is the right-level of TOKEN,
    POS is its end position in the buffer.
  (t POS TOKEN): same thing but for an open-paren or the beginning of buffer.
  (nil POS TOKEN): we skipped over a paren-like pair.
  nil: we skipped over an identifier, matched parentheses, ..."
  (smie-next-sexp
   (indirect-function smie-forward-token-function)
   (indirect-function 'forward-sexp)
   (indirect-function 'smie-op-right)
   (indirect-function 'smie-op-left)
   halfsexp))

;;; Miscellanous commands using the precedence parser.

(defun smie-backward-sexp-command (&optional n)
  "Move backward through N logical elements."
  (interactive "^p")
  (smie-forward-sexp-command (- n)))

(defun smie-forward-sexp-command (&optional n)
  "Move forward through N logical elements."
  (interactive "^p")
  (let ((forw (> n 0))
        (forward-sexp-function nil))
    (while (/= n 0)
      (setq n (- n (if forw 1 -1)))
      (let ((pos (point))
            (res (if forw
                     (smie-forward-sexp 'halfsexp)
                   (smie-backward-sexp 'halfsexp))))
        (if (and (car res) (= pos (point)) (not (if forw (eobp) (bobp))))
            (signal 'scan-error
                    (list "Containing expression ends prematurely"
                          (cadr res) (cadr res)))
          nil)))))

(defvar smie-closer-alist nil
  "Alist giving the closer corresponding to an opener.")

(defun smie-close-block ()
  "Close the closest surrounding block."
  (interactive)
  (let ((closer
         (save-excursion
           (backward-up-list 1)
           (if (looking-at "\\s(")
               (string (cdr (syntax-after (point))))
             (let* ((open (funcall smie-forward-token-function))
                    (closer (cdr (assoc open smie-closer-alist)))
                    (levels (list (assoc open smie-op-levels)))
                    (seen '())
                    (found '()))
               (cond
                ;; Even if we improve the auto-computation of closers,
                ;; there are still cases where we need manual
                ;; intervention, e.g. for Octave's use of `until'
                ;; as a pseudo-closer of `do'.
                (closer)
                ((or (equal levels '(nil)) (nth 1 (car levels)))
                 (error "Doesn't look like a block"))
                (t
                 ;; FIXME: With grammars like Octave's, every closer ("end",
                 ;; "endif", "endwhile", ...) has the same level, so we'd need
                 ;; to look at the BNF or at least at the 2D prec-table, in
                 ;; order to find the right closer for a given opener.
                 (while levels
                   (let ((level (pop levels)))
                     (dolist (other smie-op-levels)
                       (when (and (eq (nth 2 level) (nth 1 other))
                                  (not (memq other seen)))
                         (push other seen)
                         (if (nth 2 other)
                             (push other levels)
                           (push (car other) found))))))
                 (cond
                  ((null found) (error "No known closer for opener %s" open))
                  ;; FIXME: what should we do if there are various closers?
                  (t (car found))))))))))
    (unless (save-excursion (skip-chars-backward " \t") (bolp))
      (newline))
    (insert closer)
    (if (save-excursion (skip-chars-forward " \t") (eolp))
        (indent-according-to-mode)
      (reindent-then-newline-and-indent))))

(defun smie-down-list (&optional arg)
  "Move forward down one level paren-like blocks.  Like `down-list'.
With argument ARG, do this that many times.
A negative argument means move backward but still go down a level.
This command assumes point is not in a string or comment."
  (interactive "p")
  (let ((start (point))
        (inc (if (< arg 0) -1 1))
        (offset (if (< arg 0) 1 0))
        (next-token (if (< arg 0)
                        smie-backward-token-function
                      smie-forward-token-function)))
    (while (/= arg 0)
      (setq arg (- arg inc))
      (while
          (let* ((pos (point))
                 (token (funcall next-token))
                 (levels (assoc token smie-op-levels)))
            (cond
             ((zerop (length token))
              (if (if (< inc 0) (looking-back "\\s(\\|\\s)" (1- (point)))
                    (looking-at "\\s(\\|\\s)"))
                  ;; Go back to `start' in case of an error.  This presumes
                  ;; none of the token we've found until now include a ( or ).
                  (progn (goto-char start) (down-list inc) nil)
                (forward-sexp inc)
                (/= (point) pos)))
             ((and levels (null (nth (+ 1 offset) levels))) nil)
             ((and levels (null (nth (- 2 offset) levels)))
              (let ((end (point)))
                (goto-char start)
                (signal 'scan-error
                        (list "Containing expression ends prematurely"
                              pos end))))
             (t)))))))

(defvar smie-blink-matching-triggers '(?\s ?\n)
  "Chars which might trigger `blink-matching-open'.
These can include the final chars of end-tokens, or chars that are
typically inserted right after an end token.
I.e. a good choice can be:
    (delete-dups
     (mapcar (lambda (kw) (aref (cdr kw) (1- (length (cdr kw)))))
             smie-closer-alist))")

(defcustom smie-blink-matching-inners t
  "Whether SMIE should blink to matching opener for inner keywords.
If non-nil, it will blink not only for \"begin..end\" but also for \"if...else\"."
  :type 'boolean)

(defun smie-blink-matching-check (start end)
  (save-excursion
    (goto-char end)
    (let ((ender (funcall smie-backward-token-function)))
      (cond
       ((not (and ender (rassoc ender smie-closer-alist)))
        ;; This not is one of the begin..end we know how to check.
        (blink-matching-check-mismatch start end))
       ((not start) t)
       ((eq t (car (rassoc ender smie-closer-alist))) nil)
       (t
        (goto-char start)
        (let ((starter (funcall smie-forward-token-function)))
          (not (member (cons starter ender) smie-closer-alist))))))))

(defun smie-blink-matching-open ()
  "Blink the matching opener when applicable.
This uses SMIE's tables and is expected to be placed on `post-self-insert-hook'."
  (when (and blink-matching-paren
             smie-closer-alist                     ; Optimization.
             (eq (char-before) last-command-event) ; Sanity check.
             (memq last-command-event smie-blink-matching-triggers)
             (not (nth 8 (syntax-ppss))))
    (save-excursion
      (let ((pos (point))
            (token (funcall smie-backward-token-function)))
        (when (and (eq (point) (1- pos))
                   (= 1 (length token))
                   (not (rassoc token smie-closer-alist)))
          ;; The trigger char is itself a token but is not one of the
          ;; closers (e.g. ?\; in Octave mode), so go back to the
          ;; previous token.
          (setq pos (point))
          (setq token (save-excursion
                        (funcall smie-backward-token-function))))
        (when (rassoc token smie-closer-alist)
          ;; We're after a close token.  Let's still make sure we
          ;; didn't skip a comment to find that token.
          (funcall smie-forward-token-function)
          (when (and (save-excursion
                       ;; Trigger can be SPC, or reindent.
                       (skip-chars-forward " \n\t")
                       (>= (point) pos))
                     ;; If token ends with a trigger char, so don't blink for
                     ;; anything else than this trigger char, lest we'd blink
                     ;; both when inserting the trigger char and when
                     ;; inserting a subsequent trigger char like SPC.
                     (or (eq (point) pos)
                         (not (memq (char-before)
                                    smie-blink-matching-triggers)))
                     (or smie-blink-matching-inners
                         (null (nth 2 (assoc token smie-op-levels)))))
            ;; The major mode might set blink-matching-check-function
            ;; buffer-locally so that interactive calls to
            ;; blink-matching-open work right, but let's not presume
            ;; that's the case.
            (let ((blink-matching-check-function #'smie-blink-matching-check))
              (blink-matching-open))))))))

;;; The indentation engine.

(defcustom smie-indent-basic 4
  "Basic amount of indentation."
  :type 'integer)

(defvar smie-indent-rules 'unset
  ;; TODO: For SML, we need more rule formats, so as to handle
  ;;   structure Foo =
  ;;      Bar (toto)
  ;; and
  ;;   structure Foo =
  ;;   struct ... end
  ;; I.e. the indentation after "=" depends on the parent ("structure")
  ;; as well as on the following token ("struct").
  "Rules of the following form.
\((:before . TOK) . OFFSET-RULES)	how to indent TOK itself.
\(TOK . OFFSET-RULES)	how to indent right after TOK.
\(list-intro . TOKENS)	declare TOKENS as being followed by what may look like
			  a funcall but is just a sequence of expressions.
\(t . OFFSET)		basic indentation step.
\(args . OFFSET)		indentation of arguments.
\((T1 . T2) OFFSET)	like ((:before . T2) (:parent T1 OFFSET)).

OFFSET-RULES is a list of elements which can each either be:

\(:hanging . OFFSET-RULES)	if TOK is hanging, use OFFSET-RULES.
\(:parent PARENT . OFFSET-RULES) if TOK's parent is PARENT, use OFFSET-RULES.
\(:next TOKEN . OFFSET-RULES)	if TOK is followed by TOKEN, use OFFSET-RULES.
\(:prev TOKEN . OFFSET-RULES)	if TOK is preceded by TOKEN, use
\(:bolp . OFFSET-RULES)		If TOK is first on a line, use OFFSET-RULES.
OFFSET				the offset to use.

PARENT can be either the name of the parent or a list of such names.

OFFSET can be of the form:
`point'				align with the token.
`parent'				align with the parent.
NUMBER				offset by NUMBER.
\(+ OFFSETS...)			use the sum of OFFSETS.
VARIABLE			use the value of VARIABLE as offset.

The precise meaning of `point' depends on various details: it can
either mean the position of the token we're indenting, or the
position of its parent, or the position right after its parent.

A nil offset for indentation after an opening token defaults
to `smie-indent-basic'.")

(defun smie-indent--hanging-p ()
  ;; A hanging keyword is one that's at the end of a line except it's not at
  ;; the beginning of a line.
  (and (save-excursion
         (when (zerop (length (funcall smie-forward-token-function)))
           ;; Could be an open-paren.
           (forward-char 1))
         (skip-chars-forward " \t")
         (eolp))
       (not (smie-indent--bolp))))

(defun smie-indent--bolp ()
  (save-excursion (skip-chars-backward " \t") (bolp)))

(defun smie-indent--offset (elem)
  (or (cdr (assq elem smie-indent-rules))
      (cdr (assq t smie-indent-rules))
      smie-indent-basic))

(defvar smie-indent-debug-log)

(defun smie-indent--offset-rule (tokinfo &optional after parent)
  "Apply the OFFSET-RULES in TOKINFO.
Point is expected to be right in front of the token corresponding to TOKINFO.
If computing the indentation after the token, then AFTER is the position
after the token, otherwise it should be nil.
PARENT if non-nil should be the parent info returned by `smie-backward-sexp'."
  (let ((rules (cdr tokinfo))
        next prev
        offset)
    (while (consp rules)
      (let ((rule (pop rules)))
        (cond
         ((not (consp rule)) (setq offset rule))
         ((eq (car rule) '+) (setq offset rule))
         ((eq (car rule) :hanging)
          (when (smie-indent--hanging-p)
            (setq rules (cdr rule))))
         ((eq (car rule) :bolp)
          (when (smie-indent--bolp)
            (setq rules (cdr rule))))
         ((eq (car rule) :eolp)
          (unless after
            (error "Can't use :eolp in :before indentation rules"))
          (when (> after (line-end-position))
            (setq rules (cdr rule))))
         ((eq (car rule) :prev)
          (unless prev
            (save-excursion
              (setq prev (smie-indent-backward-token))))
          (when (equal (car prev) (cadr rule))
            (setq rules (cddr rule))))
         ((eq (car rule) :next)
          (unless next
            (unless after
              (error "Can't use :next in :before indentation rules"))
            (save-excursion
              (goto-char after)
              (setq next (smie-indent-forward-token))))
          (when (equal (car next) (cadr rule))
            (setq rules (cddr rule))))
         ((eq (car rule) :parent)
          (unless parent
            (save-excursion
              (if after (goto-char after))
              (setq parent (smie-backward-sexp 'halfsexp))))
          (when (if (listp (cadr rule))
                    (member (nth 2 parent) (cadr rule))
                  (equal (nth 2 parent) (cadr rule)))
            (setq rules (cddr rule))))
         (t (error "Unknown rule %s for indentation of %s"
                   rule (car tokinfo))))))
    ;; If `offset' is not set yet, use `rules' to handle the case where
    ;; the tokinfo uses the old-style ((PARENT . TOK). OFFSET).
    (unless offset (setq offset rules))
    (when (boundp 'smie-indent-debug-log)
      (push (list (point) offset tokinfo) smie-indent-debug-log))
    offset))

(defun smie-indent--column (offset &optional base parent virtual-point)
  "Compute the actual column to use for a given OFFSET.
BASE is the base position to use, and PARENT is the parent info, if any.
If VIRTUAL-POINT is non-nil, then `point' is virtual."
  (cond
   ((eq (car-safe offset) '+)
    (apply '+ (mapcar (lambda (offset) (smie-indent--column offset nil parent))
                      (cdr offset))))
   ((integerp offset)
    (+ offset
       (case base
         ((nil) 0)
         (parent (goto-char (cadr parent))
                 (smie-indent-virtual))
         (t
          (goto-char base)
          ;; For indentation after "(let" in SML-mode, we end up accumulating
          ;; the offset of "(" and the offset of "let", so we use `min' to try
          ;; and get it right either way.
          (min (smie-indent-virtual) (current-column))))))
   ((eq offset 'point)
    ;; In indent-keyword, if we're indenting `then' wrt `if', we want to use
    ;; indent-virtual rather than use just current-column, so that we can
    ;; apply the (:before . "if") rule which does the "else if" dance in SML.
    ;; But in other cases, we do not want to use indent-virtual
    ;; (e.g. indentation of "*" w.r.t "+", or ";" wrt "(").  We could just
    ;; always use indent-virtual and then have indent-rules say explicitly
    ;; to use `point' after things like "(" or "+" when they're not at EOL,
    ;; but you'd end up with lots of those rules.
    ;; So we use a heuristic here, which is that we only use virtual if
    ;; the parent is tightly linked to the child token (they're part of
    ;; the same BNF rule).
    (if (and virtual-point (null (car parent))) ;Black magic :-(
        (smie-indent-virtual) (current-column)))
   ((eq offset 'parent)
    (unless parent
      (setq parent (or (smie-backward-sexp 'halfsexp) :notfound)))
    (if (consp parent) (goto-char (cadr parent)))
    (smie-indent-virtual))
   ((eq offset nil) nil)
   ((and (symbolp offset) (boundp 'offset))
    (smie-indent--column (symbol-value offset) base parent virtual-point))
   (t (error "Unknown indentation offset %s" offset))))

(defun smie-indent-forward-token ()
  "Skip token forward and return it, along with its levels."
  (let ((tok (funcall smie-forward-token-function)))
    (cond
     ((< 0 (length tok)) (assoc tok smie-op-levels))
     ((looking-at "\\s(")
      (forward-char 1)
      (list (buffer-substring (1- (point)) (point)) nil 0)))))

(defun smie-indent-backward-token ()
  "Skip token backward and return it, along with its levels."
  (let ((tok (funcall smie-backward-token-function)))
    (cond
     ((< 0 (length tok)) (assoc tok smie-op-levels))
     ;; 4 == Open paren syntax.
     ((eq 4 (syntax-class (syntax-after (1- (point)))))
      (forward-char -1)
      (list (buffer-substring (point) (1+ (point))) nil 0)))))

(defun smie-indent-virtual ()
  ;; We used to take an optional arg (with value :not-hanging) to specify that
  ;; we should only use (smie-indent-calculate) if we're looking at a hanging
  ;; keyword.  This was a bad idea, because the virtual indent of a position
  ;; should not depend on the caller, since it leads to situations where two
  ;; dependent indentations get indented differently.
  "Compute the virtual indentation to use for point.
This is used when we're not trying to indent point but just
need to compute the column at which point should be indented
in order to figure out the indentation of some other (further down) point."
  ;; Trust pre-existing indentation on other lines.
  (if (smie-indent--bolp) (current-column) (smie-indent-calculate)))

(defun smie-indent-fixindent ()
  ;; Obey the `fixindent' special comment.
  (and (smie-indent--bolp)
       (save-excursion
         (comment-normalize-vars)
         (re-search-forward (concat comment-start-skip
                                    "fixindent"
                                    comment-end-skip)
                            ;; 1+ to account for the \n comment termination.
                            (1+ (line-end-position)) t))
       (current-column)))

(defun smie-indent-bob ()
  ;; Start the file at column 0.
  (save-excursion
    (forward-comment (- (point)))
    (if (bobp) 0)))

(defun smie-indent-close ()
  ;; Align close paren with opening paren.
  (save-excursion
    ;; (forward-comment (point-max))
    (when (looking-at "\\s)")
      (while (not (zerop (skip-syntax-forward ")")))
        (skip-chars-forward " \t"))
      (condition-case nil
          (progn
            (backward-sexp 1)
            (smie-indent-virtual))      ;:not-hanging
        (scan-error nil)))))

(defun smie-indent-keyword ()
  ;; Align closing token with the corresponding opening one.
  ;; (e.g. "of" with "case", or "in" with "let").
  (save-excursion
    (let* ((pos (point))
           (toklevels (smie-indent-forward-token))
           (token (pop toklevels)))
      (if (null (car toklevels))
          (save-excursion
            (goto-char pos)
            ;; Different cases:
            ;; - smie-indent--bolp: "indent according to others".
            ;; - common hanging: "indent according to others".
            ;; - SML-let hanging: "indent like parent".
            ;; - if-after-else: "indent-like parent".
            ;; - middle-of-line: "trust current position".
            (cond
             ((null (cdr toklevels)) nil) ;Not a keyword.
             ((smie-indent--bolp)
              ;; For an open-paren-like thingy at BOL, always indent only
              ;; based on other rules (typically smie-indent-after-keyword).
              nil)
             (t
              ;; We're only ever here for virtual-indent, which is why
              ;; we can use (current-column) as answer for `point'.
              (let* ((tokinfo (or (assoc (cons :before token)
                                         smie-indent-rules)
                                  ;; By default use point unless we're hanging.
                                  `((:before . ,token) (:hanging nil) point)))
                     ;; (after (prog1 (point) (goto-char pos)))
                     (offset (smie-indent--offset-rule tokinfo)))
                (smie-indent--column offset)))))

        ;; FIXME: This still looks too much like black magic!!
        ;; FIXME: Rather than a bunch of rules like (PARENT . TOKEN), we
        ;; want a single rule for TOKEN with different cases for each PARENT.
        (let* ((parent (smie-backward-sexp 'halfsexp))
               (tokinfo
                (or (assoc (cons (caddr parent) token)
                           smie-indent-rules)
                    (assoc (cons :before token) smie-indent-rules)
                    ;; Default rule.
                    `((:before . ,token)
                      ;; (:parent open 0)
                      point)))
               (offset (save-excursion
                         (goto-char pos)
                         (smie-indent--offset-rule tokinfo nil parent))))
          ;; Different behaviors:
          ;; - align with parent.
          ;; - parent + offset.
          ;; - after parent's column + offset (actually, after or before
          ;;   depending on where backward-sexp stopped).
          ;; ? let it drop to some other indentation function (almost never).
          ;; ? parent + offset + parent's own offset.
          ;; Different cases:
          ;; - bump into a same-level operator.
          ;; - bump into a specific known parent.
          ;; - find a matching open-paren thingy.
          ;; - bump into some random parent.
          ;; ? borderline case (almost never).
          ;; ? bump immediately into a parent.
          (cond
           ((not (or (< (point) pos)
                     (and (cadr parent) (< (cadr parent) pos))))
            ;; If we didn't move at all, that means we didn't really skip
            ;; what we wanted.  Should almost never happen, other than
            ;; maybe when an infix or close-paren is at the beginning
            ;; of a buffer.
            nil)
           ((eq (car parent) (car toklevels))
            ;; We bumped into a same-level operator. align with it.
            (if (and (smie-indent--bolp) (/= (point) pos)
                     (save-excursion
                       (goto-char (goto-char (cadr parent)))
                       (not (smie-indent--bolp)))
                     ;; Check the offset of `token' rather then its parent
                     ;; because its parent may have used a special rule.  E.g.
                     ;;    function foo;
                     ;;      line2;
                     ;;      line3;
                     ;; The ; on the first line had a special rule, but when
                     ;; indenting line3, we don't care about it and want to
                     ;; align with line2.
                     (memq offset '(point nil)))
                ;; If the parent is at EOL and its children are indented like
                ;; itself, then we can just obey the indentation chosen for the
                ;; child.
                ;; This is important for operators like ";" which
                ;; are usually at EOL (and have an offset of 0): otherwise we'd
                ;; always go back over all the statements, which is
                ;; a performance problem and would also mean that fixindents
                ;; in the middle of such a sequence would be ignored.
                ;;
                ;; This is a delicate point!
                ;; Even if the offset is not 0, we could follow the same logic
                ;; and subtract the offset from the child's indentation.
                ;; But that would more often be a bad idea: OT1H we generally
                ;; want to reuse the closest similar indentation point, so that
                ;; the user's choice (or the fixindents) are obeyed.  But OTOH
                ;; we don't want this to affect "unrelated" parts of the code.
                ;; E.g. a fixindent in the body of a "begin..end" should not
                ;; affect the indentation of the "end".
                (current-column)
              (goto-char (cadr parent))
              ;; Don't use (smie-indent-virtual :not-hanging) here, because we
              ;; want to jump back over a sequence of same-level ops such as
              ;;    a -> b -> c
              ;;    -> d
              ;; So as to align with the earliest appropriate place.
              (smie-indent-virtual)))
           (tokinfo
            (if (and (= (point) pos) (smie-indent--bolp)
                     (or (eq offset 'point)
                         (and (consp offset) (memq 'point offset))))
                ;; Since we started at BOL, we're not computing a virtual
                ;; indentation, and we're still at the starting point, so
                ;; we can't use `current-column' which would cause
                ;; indentation to depend on itself.
                nil
              (smie-indent--column offset 'parent parent
                                  ;; If we're still at pos, indent-virtual
                                  ;; will inf-loop.
                                  (unless (= (point) pos) 'virtual))))))))))

(defun smie-indent-comment ()
  "Compute indentation of a comment."
  ;; Don't do it for virtual indentations.  We should normally never be "in
  ;; front of a comment" when doing virtual-indentation anyway.  And if we are
  ;; (as can happen in octave-mode), moving forward can lead to inf-loops.
  (and (smie-indent--bolp)
       (let ((pos (point)))
         (save-excursion
           (beginning-of-line)
           (and (re-search-forward comment-start-skip (line-end-position) t)
                (eq pos (or (match-end 1) (match-beginning 0))))))
       (save-excursion
         (forward-comment (point-max))
         (skip-chars-forward " \t\r\n")
         (smie-indent-calculate))))

(defun smie-indent-comment-continue ()
  ;; indentation of comment-continue lines.
  (let ((continue (and comment-continue
                       (comment-string-strip comment-continue t t))))
    (and (< 0 (length continue))
         (looking-at (regexp-quote continue)) (nth 4 (syntax-ppss))
         (let ((ppss (syntax-ppss)))
           (save-excursion
             (forward-line -1)
             (if (<= (point) (nth 8 ppss))
                 (progn (goto-char (1+ (nth 8 ppss))) (current-column))
               (skip-chars-forward " \t")
               (if (looking-at (regexp-quote continue))
                   (current-column))))))))

(defun smie-indent-comment-close ()
  (and (boundp 'comment-end-skip)
       comment-end-skip
       (not (looking-at " \t*$"))       ;Not just a \n comment-closer.
       (looking-at comment-end-skip)
       (nth 4 (syntax-ppss))
       (save-excursion
         (goto-char (nth 8 (syntax-ppss)))
         (current-column))))

(defun smie-indent-comment-inside ()
  (and (nth 4 (syntax-ppss))
       'noindent))

(defun smie-indent-after-keyword ()
  ;; Indentation right after a special keyword.
  (save-excursion
    (let* ((pos (point))
           (toklevel (smie-indent-backward-token))
           (tok (car toklevel))
           (tokinfo (assoc tok smie-indent-rules)))
      ;; Set some default indent rules.
      (if (and toklevel (null (cadr toklevel)) (null tokinfo))
          (setq tokinfo (list (car toklevel))))
      ;; (if (and tokinfo (null toklevel))
      ;;     (error "Token %S has indent rule but has no parsing info" tok))
      (when toklevel
        (unless tokinfo
          ;; The default indentation after a keyword/operator is 0 for
          ;; infix and t for prefix.
          ;; Using the BNF syntax, we could come up with better
          ;; defaults, but we only have the precedence levels here.
          (setq tokinfo (list tok 'default-rule
                              (if (cadr toklevel) 0 (smie-indent--offset t)))))
        (let ((offset
               (or (smie-indent--offset-rule tokinfo pos)
                   (smie-indent--offset t))))
          (let ((before (point)))
            (goto-char pos)
            (smie-indent--column offset before)))))))

(defun smie-indent-exps ()
  ;; Indentation of sequences of simple expressions without
  ;; intervening keywords or operators.  E.g. "a b c" or "g (balbla) f".
  ;; Can be a list of expressions or a function call.
  ;; If it's a function call, the first element is special (it's the
  ;; function).  We distinguish function calls from mere lists of
  ;; expressions based on whether the preceding token is listed in
  ;; the `list-intro' entry of smie-indent-rules.
  ;;
  ;; TODO: to indent Lisp code, we should add a way to specify
  ;; particular indentation for particular args depending on the
  ;; function (which would require always skipping back until the
  ;; function).
  ;; TODO: to indent C code, such as "if (...) {...}" we might need
  ;; to add similar indentation hooks for particular positions, but
  ;; based on the preceding token rather than based on the first exp.
  (save-excursion
    (let ((positions nil)
          arg)
      (while (and (null (car (smie-backward-sexp)))
                  (push (point) positions)
                  (not (smie-indent--bolp))))
      (save-excursion
        ;; Figure out if the atom we just skipped is an argument rather
        ;; than a function.
        (setq arg (or (null (car (smie-backward-sexp)))
                      (member (funcall smie-backward-token-function)
                              (cdr (assoc 'list-intro smie-indent-rules))))))
      (cond
       ((null positions)
        ;; We're the first expression of the list.  In that case, the
        ;; indentation should be (have been) determined by its context.
        nil)
       (arg
        ;; There's a previous element, and it's not special (it's not
        ;; the function), so let's just align with that one.
        (goto-char (car positions))
        (current-column))
       ((cdr positions)
        ;; We skipped some args plus the function and bumped into something.
        ;; Align with the first arg.
        (goto-char (cadr positions))
        (current-column))
       (positions
        ;; We're the first arg.
        (goto-char (car positions))
        ;; FIXME: Use smie-indent--column.
        (+ (smie-indent--offset 'args)
           ;; We used to use (smie-indent-virtual), but that
           ;; doesn't seem right since it might then indent args less than
           ;; the function itself.
           (current-column)))))))

(defvar smie-indent-functions
  '(smie-indent-fixindent smie-indent-bob smie-indent-close
    smie-indent-comment smie-indent-comment-continue smie-indent-comment-close
    smie-indent-comment-inside smie-indent-keyword smie-indent-after-keyword
    smie-indent-exps)
  "Functions to compute the indentation.
Each function is called with no argument, shouldn't move point, and should
return either nil if it has no opinion, or an integer representing the column
to which that point should be aligned, if we were to reindent it.")

(defun smie-indent-calculate ()
  "Compute the indentation to use for point."
  (run-hook-with-args-until-success 'smie-indent-functions))

(defun smie-indent-line ()
  "Indent current line using the SMIE indentation engine."
  (interactive)
  (let* ((savep (point))
	 (indent (condition-case-no-debug nil
		     (save-excursion
                       (forward-line 0)
                       (skip-chars-forward " \t")
                       (if (>= (point) savep) (setq savep nil))
                       (or (smie-indent-calculate) 0))
                   (error 0))))
    (if (not (numberp indent))
        ;; If something funny is used (e.g. `noindent'), return it.
        indent
      (if (< indent 0) (setq indent 0)) ;Just in case.
      (if savep
          (save-excursion (indent-line-to indent))
        (indent-line-to indent)))))

(defun smie-indent-debug ()
  "Show the rules used to compute indentation of current line."
  (interactive)
  (let ((smie-indent-debug-log '()))
    (smie-indent-calculate)
    ;; FIXME: please improve!
    (message "%S" smie-indent-debug-log)))

(defun smie-setup (op-levels indent-rules)
  (set (make-local-variable 'smie-indent-rules) indent-rules)
  (set (make-local-variable 'smie-op-levels) op-levels)
  (set (make-local-variable 'indent-line-function) 'smie-indent-line))


(provide 'smie)
;;; smie.el ends here
