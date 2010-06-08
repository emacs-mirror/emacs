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
;;   looks at "as little as possible" of the buffer make an indentation
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
;; OTOH we had to kill many chickens, read many coffee grounds, and practiced
;; untold numbers of black magic spells.

;;; Code:

(eval-when-compile (require 'cl))

(defvar comment-continue)

;;; Building precedence level tables from BNF specs.

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
or (right OP ...) or (nonassoc OP ...)  or (assoc OP ...).  All operators in
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
                   (smie-set-prec2tab prec2 (car k) (cdr k) v))
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
          (assert (consp rhs))
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
    prec2))

(defun smie-prec2-levels (prec2)
  "Take a 2D precedence table and turn it into an alist of precedence levels.
PREC2 is a table as returned by `smie-precs-precedence-table' or
`smie-bnf-precedence-table'."
  ;; For each operator, we create two "variables" (corresponding to
  ;; the left and right precedence level), which are represented by
  ;; cons cells.  Those are the vary cons cells that appear in the
  ;; final `table'.  The value of each "variable" is kept in the `car'.
  (let ((table ())
        (csts ())
        (eqs ())
        tmp x y)
    ;; From `prec2' we construct a list of constraints between
    ;; variables (aka "precedence levels").  These can be either
    ;; equality constraints (in `eqs') or `<' constraints (in `csts').
    (maphash (lambda (k v)
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
                 (> (push (cons y x) csts))))
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
            (error "Can't resolve the precedence table to precedence levels")))
        (incf i 10))
      ;; Propagate equalities back to their source.
      (dolist (eq (nreverse eqs))
        (assert (or (null (caar eq)) (eq (car eq) (cdr eq))))
        (setcar (car eq) (cadr eq)))
      ;; Finally, fill in the remaining vars (which only appeared on the
      ;; right side of the < constraints).
      (dolist (x table)
        ;; When both sides are nil, it means this operator binds very
        ;; very tight, but it's still just an operator, so we give it
        ;; the highest precedence.
        ;; OTOH if only one side is nil, it usually means it's like an
        ;; open-paren, which is very important for indentation purposes,
        ;; so we keep it nil, to make it easier to recognize.
        (unless (or (nth 1 x) (nth 2 x))
          (setf (nth 1 x) i)
          (setf (nth 2 x) i))))
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
  (buffer-substring (point)
                    (progn (if (zerop (skip-syntax-backward "."))
                               (skip-syntax-backward "w_'"))
                           (point))))

(defun smie-default-forward-token ()
  (forward-comment (point-max))
  (buffer-substring (point)
                    (progn (if (zerop (skip-syntax-forward "."))
                               (skip-syntax-forward "w_'"))
                           (point))))

(defun smie-associative-p (toklevels)
  ;; in "a + b + c" we want to stop at each +, but in
  ;; "if a then b else c" we don't want to stop at each keyword.
  ;; To distinguish the two cases, we made smie-prec2-levels choose
  ;; different levels for each part of "if a then b else c", so that
  ;; by checking if the left-level is equal to the right level, we can
  ;; figure out that it's an associative operator.
  ;; This is not 100% foolproof, tho, since a grammar like
  ;;   (exp ("A" exp "C") ("A" exp "B" exp "C"))
  ;; will cause "B" to have equal left and right levels, even though
  ;; it is not an associative operator.
  ;; A better check would be the check the actual previous operator
  ;; against this one to see if it's the same, but we'd have to change
  ;; `levels' to keep a stack of operators rather than only levels.
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
                  (scan-error (throw 'return (list t (caddr err)))))
                (if (eq pos (point))
                    ;; We did not move, so let's abort the loop.
                    (throw 'return (list t (point))))))
             ((null (funcall op-back toklevels))
              ;; A token like a paren-close.
              (assert (funcall op-forw toklevels)) ;Otherwise, why mention it?
              (push (funcall op-forw toklevels) levels))
             (t
              (while (and levels (< (funcall op-back toklevels) (car levels)))
                (setq levels (cdr levels)))
              (cond
               ((null levels)
                (if (and halfsexp (funcall op-forw toklevels))
                    (push (funcall op-forw toklevels) levels)
                  (throw 'return
                         (prog1 (list (or (car toklevels) t) (point) token)
                           (goto-char pos)))))
               (t
                (if (and levels (= (funcall op-back toklevels) (car levels)))
                    (setq levels (cdr levels)))
                (cond
                 ((null levels)
                  (cond
                   ((null (funcall op-forw toklevels))
                    (throw 'return (list nil (point) token)))
                   ((smie-associative-p toklevels)
                    (throw 'return
                           (prog1 (list (or (car toklevels) t) (point) token)
                             (goto-char pos))))
                   ;; We just found a match to the previously pending operator
                   ;; but this new operator is still part of a larger RHS.
                   ;; E.g. we're now looking at the "then" in
                   ;; "if a then b else c".  So we have to keep parsing the
                   ;; rest of the construct.
                   (t (push (funcall op-forw toklevels) levels))))
                 (t
                  (if (funcall op-forw toklevels)
                      (push (funcall op-forw toklevels) levels))))))))
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

(defun smie-backward-sexp-command (&optional n)
  "Move backward through N logical elements."
  (interactive "p")
  (if (< n 0)
      (smie-forward-sexp-command (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        (decf n)
        (let ((pos (point))
              (res (smie-backward-sexp 'halfsexp)))
          (if (and (car res) (= pos (point)) (not (bolp)))
              (signal 'scan-error
                      (list "Containing expression ends prematurely"
                            (cadr res) (cadr res)))
            nil))))))

(defun smie-forward-sexp-command (&optional n)
  "Move forward through N logical elements."
  (interactive "p")
  (if (< n 0)
      (smie-backward-sexp-command (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        (decf n)
        (let ((pos (point))
              (res (smie-forward-sexp 'halfsexp)))
          (if (and (car res) (= pos (point)) (not (bolp)))
              (signal 'scan-error
                      (list "Containing expression ends prematurely"
                            (cadr res) (cadr res)))
            nil))))))

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
\((T1 . T2) . OFFSET)	how to indent token T2 w.r.t T1.
\((t . TOK) . OFFSET)	how to indent TOK with respect to its parent.
\(list-intro . TOKENS)	declare TOKENS as being followed by what may look like
			  a funcall but is just a sequence of expressions.
\(t . OFFSET)		basic indentation step.
\(args . OFFSET)		indentation of arguments.

OFFSET-RULES is a list of elements which can each either be:

\(:hanging . OFFSET-RULES)	if TOK is hanging, use OFFSET-RULES.
\(:parent PARENT . OFFSET-RULES) if TOK's parent is PARENT, use OFFSET-RULES.
\(:next TOKEN . OFFSET-RULES)	if TOK is followed by TOKEN, use OFFSET-RULES.
\(:prev TOKEN . OFFSET-RULES)	if TOK is preceded by TOKEN, use OFFSET-RULES.
a number				the offset to use.
`point'				align with the token.
`parent'				align with the parent.

A nil offset for indentation after a token defaults to `smie-indent-basic'.")

(defun smie-indent-hanging-p ()
  ;; A hanging keyword is one that's at the end of a line except it's not at
  ;; the beginning of a line.
  (and (save-excursion
         (when (zerop (length (funcall smie-forward-token-function)))
           ;; Could be an open-paren.
           (forward-char 1))
         (skip-chars-forward " \t")
         (eolp))
       (not (smie-bolp))))

(defun smie-bolp ()
  (save-excursion (skip-chars-backward " \t") (bolp)))

(defun smie-indent-offset (elem)
  (or (cdr (assq elem smie-indent-rules))
      (cdr (assq t smie-indent-rules))
      smie-indent-basic))

(defun smie-indent-offset-rule (tokinfo &optional after)
  "Apply the OFFSET-RULES in TOKINFO.
Point is expected to be right in front of the token corresponding to TOKINFO.
If computing the indentation after the token, then AFTER is the position
after the token."
  (let ((rules (cdr tokinfo))
        parent next prev
        offset)
    (while (consp rules)
      (let ((rule (pop rules)))
        (cond
         ((not (consp rule)) (setq offset rule))
         ((eq (car rule) :hanging)
          (when (smie-indent-hanging-p)
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
          (when (equal (nth 2 parent) (cadr rule))
            (setq rules (cddr rule))))
         (t (error "Unknown rule %s for indentation of %s"
                   rule (car tokinfo))))))
    offset))

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
  (if (smie-bolp) (current-column) (smie-indent-calculate)))

(defun smie-indent-fixindent ()
  ;; Obey the `fixindent' special comment.
  (and (smie-bolp)
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
          ;; Different case:
          ;; - smie-bolp: "indent according to others".
          ;; - common hanging: "indent according to others".
          ;; - SML-let hanging: "indent like parent".
          ;; - if-after-else: "indent-like parent".
          ;; - middle-of-line: "trust current position".
          (cond
           ((null (cdr toklevels)) nil) ;Not a keyword.
           ((smie-bolp)
            ;; For an open-paren-like thingy at BOL, always indent only
            ;; based on other rules (typically smie-indent-after-keyword).
            nil)
           (t
            (let* ((tokinfo (or (assoc (cons :before token) smie-indent-rules)
                                ;; By default use point unless we're hanging.
                                (cons (cons :before token)
                                      '((:hanging nil) point))))
                   (after (prog1 (point) (goto-char pos)))
                   (offset (smie-indent-offset-rule tokinfo)))
              (cond
               ((eq offset 'point) (current-column))
               ((eq offset 'parent)
                (let ((parent (smie-backward-sexp 'halfsexp)))
                  (if parent (goto-char (cadr parent))))
                (smie-indent-virtual))
               ((eq offset nil) nil)
               (t (error "Unhandled offset %s in %s"
                         offset (cons :before token)))))))

        ;; FIXME: This still looks too much like black magic!!
        ;; FIXME: Rather than a bunch of rules like (PARENT . TOKEN), we
        ;; want a single rule for TOKEN with different cases for each PARENT.
        (let ((res (smie-backward-sexp 'halfsexp)) tmp)
          (cond
           ((not (or (< (point) pos)
                     (and (cadr res) (< (cadr res) pos))))
            ;; If we didn't move at all, that means we didn't really skip
            ;; what we wanted.
            nil)
           ((eq (car res) (car toklevels))
            ;; We bumped into a same-level operator. align with it.
            (goto-char (cadr res))
            ;; Don't use (smie-indent-virtual :not-hanging) here, because we
            ;; want to jump back over a sequence of same-level ops such as
            ;;    a -> b -> c
            ;;    -> d
            ;; So as to align with the earliest appropriate place.
            (smie-indent-virtual))
           ((equal token (save-excursion
                           (funcall smie-backward-token-function)))
            ;; in cases such as "fn x => fn y => fn z =>",
            ;; jump back to the very first fn.
            ;; FIXME: should we only do that for special tokens like "=>"?
            (smie-indent-virtual))
           ((setq tmp (assoc (cons (caddr res) token)
                             smie-indent-rules))
            (goto-char (cadr res))
            (+ (cdr tmp) (smie-indent-virtual))) ;:not-hanging
           ;; FIXME: The rules ((t . TOK) . OFFSET) either indent
           ;; relative to "before the parent" or "after the parent",
           ;; depending on details of the grammar.
           ((null (car res))
            (assert (eq (point) (cadr res)))
            (goto-char (cadr res))
            (+ (or (cdr (assoc (cons t token) smie-indent-rules)) 0)
               (smie-indent-virtual)))  ;:not-hanging
           ((and (= (point) pos) (smie-bolp))
            ;; Since we started at BOL, we're not computing a virtual
            ;; indentation, and we're still at the starting point, so the
            ;; next (default) rule can't be used since it uses `current-column'
            ;; which would cause. indentation to depend on itself.
            ;; We could just return nil, but OTOH that's not good enough in
            ;; some cases.  Instead, we want to combine the offset-rules for
            ;; the current token with the offset-rules of the previous one.
            (+ (or (cdr (assoc (cons t token) smie-indent-rules)) 0)
               ;; FIXME: This is odd.  Can't we make it use
               ;; smie-indent-(calculate|virtual) somehow?
               (smie-indent-after-keyword)))
           (t
            (+ (or (cdr (assoc (cons t token) smie-indent-rules)) 0)
               (current-column)))))))))

(defun smie-indent-comment ()
  ;; Indentation of a comment.
  (and (looking-at comment-start-skip)
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

(defun smie-indent-after-keyword ()
  ;; Indentation right after a special keyword.
  (save-excursion
    (let* ((pos (point))
           (toklevel (smie-indent-backward-token))
           (tok (car toklevel))
           (tokinfo (assoc tok smie-indent-rules)))
      (if (and toklevel (null (cadr toklevel)) (null tokinfo))
          (setq tokinfo (list (car toklevel))))
      ;; (if (and tokinfo (null toklevel))
      ;;     (error "Token %S has indent rule but has no parsing info" tok))
      (when toklevel
        (let ((offset
               (cond
                (tokinfo (or (smie-indent-offset-rule tokinfo pos)
                             (smie-indent-offset t)))
                ;; The default indentation after a keyword/operator
                ;; is 0 for infix and t for prefix.
                ;; Using the BNF syntax, we could come up with
                ;; better defaults, but we only have the
                ;; precedence levels here.
                ((null (cadr toklevel)) (smie-indent-offset t))
                (t 0))))
          ;; For indentation after "(let" in SML-mode, we end up accumulating
          ;; the offset of "(" and the offset of "let", so we use `min' to try
          ;; and get it right either way.
          (+ (min (smie-indent-virtual) (current-column)) offset))))))

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
                  (not (smie-bolp))))
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
        (+ (smie-indent-offset 'args)
           ;; We used to use (smie-indent-virtual), but that
           ;; doesn't seem right since it might then indent args less than
           ;; the function itself.
           (current-column)))))))

(defvar smie-indent-functions
  '(smie-indent-fixindent smie-indent-bob smie-indent-close smie-indent-comment
    smie-indent-comment-continue smie-indent-keyword smie-indent-after-keyword
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
	 (indent (condition-case nil
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

;;;###autoload
(defun smie-setup (op-levels indent-rules)
  (set (make-local-variable 'smie-indent-rules) indent-rules)
  (set (make-local-variable 'smie-op-levels) op-levels)
  (set (make-local-variable 'indent-line-function) 'smie-indent-line))


(provide 'smie)
;;; smie.el ends here
