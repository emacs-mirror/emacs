;;; peg.el --- Parsing Expression Grammars in Emacs Lisp  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2026 Free Software Foundation, Inc.
;;
;; Author: Helmut Eller <eller.helmut@gmail.com>
;; Maintainer: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 1.0.1
;; Package-Requires: ((emacs "25"))
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package implements Parsing Expression Grammars for Emacs Lisp.

;; Parsing Expression Grammars (PEG) are a formalism in the spirit of
;; Context Free Grammars (CFG) with some simplifications which makes
;; the implementation of PEGs as recursive descent parsers particularly
;; simple and easy to understand [Ford, Baker].
;; PEGs are more expressive than regexps and potentially easier to use.
;;
;; This file implements the macros `define-peg-rule', `with-peg-rules', and
;; `peg-parse' which parses the current buffer according to a PEG.
;; E.g. we can match integers with:
;;
;;     (with-peg-rules
;;         ((number sign digit (* digit))
;;          (sign   (or "+" "-" ""))
;;          (digit  [0-9]))
;;       (peg-run (peg number)))
;; or
;;     (define-peg-rule digit ()
;;       [0-9])
;;     (peg-parse (number sign digit (* digit))
;;                (sign   (or "+" "-" "")))
;;
;; In contrast to regexps, PEGs allow us to define recursive "rules".
;; A "grammar" is a set of rules.  A rule is written as (NAME PEX...)
;; E.g. (sign (or "+" "-" "")) is a rule with the name "sign".
;; The syntax for PEX (Parsing Expression) is a follows:
;;
;;     Description		Lisp		Traditional, as in Ford's paper
;;     ===========		====		===========
;;     Sequence			(and E1 E2)	e1 e2
;;     Prioritized Choice	(or E1 E2)	e1 / e2
;;     Not-predicate		(not E)		!e
;;     And-predicate		(if E)		&e
;;     Any character		(any)		.
;;     Literal string		"abc"		"abc"
;;     Character C		(char C)	'c'
;;     Zero-or-more		(* E)		e*
;;     One-or-more		(+ E)		e+
;;     Optional			(opt E)		e?
;;     Non-terminal             SYMBOL		A
;;     Character range		(range A B)	[a-b]
;;     Character set		[a-b "+*" ?x]	[a-b+*x]   ;Note: it's a vector
;;     Character classes	[ascii cntrl]
;;     Boolean-guard		(guard EXP)
;;     Syntax-Class		(syntax-class NAME)
;;     Local definitions	(with RULES PEX...)
;;     Indirect call            (funcall EXP ARGS...)
;; and
;;     Empty-string		(null)		ε
;;     Beginning-of-Buffer	(bob)
;;     End-of-Buffer		(eob)
;;     Beginning-of-Line	(bol)
;;     End-of-Line		(eol)
;;     Beginning-of-Word	(bow)
;;     End-of-Word		(eow)
;;     Beginning-of-Symbol	(bos)
;;     End-of-Symbol		(eos)
;;
;; Rules can refer to other rules, and a grammar is often structured
;; as a tree, with a root rule referring to one or more "branch
;; rules", all the way down to the "leaf rules" that deal with actual
;; buffer text.  Rules can be recursive or mutually referential,
;; though care must be taken not to create infinite loops.
;;
;;;; Named rulesets:
;;
;; You can define a set of rules for later use with:
;;
;;     (define-peg-ruleset myrules
;;       (sign  () (or "+" "-" ""))
;;       (digit () [0-9])
;;       (nat   () digit (* digit))
;;       (int   () sign digit (* digit))
;;       (float () int "." nat))
;;
;; and later refer to it:
;;
;;     (with-peg-rules
;;         (myrules
;;          (complex float "+i" float))
;;       ... (peg-parse nat "," nat "," complex) ...)
;;
;;;; Parsing actions:
;;
;; PEXs also support parsing actions, i.e. Lisp snippets which are
;; executed when a pex matches.  This can be used to construct syntax
;; trees or for similar tasks.  The most basic form of action is
;; written as:
;;
;;     (action FORM)          ; evaluate FORM for its side-effects
;;
;; Actions don't consume input, but are executed at the point of
;; match.  Another kind of action is called a "stack action", and
;; looks like this:
;;
;;     `(VAR... -- FORM...)   ; stack action
;;
;; A stack action takes VARs from the "value stack" and pushes the
;; results of evaluating FORMs to that stack.

;; The value stack is created during the course of parsing.  Certain
;; operators (see below) that match buffer text can push values onto
;; this stack.  "Upstream" rules can then draw values from the stack,
;; and optionally push new ones back.  For instance, consider this
;; very simple grammar:
;;
;; (with-peg-rules
;;     ((query (+ term) (eol))
;;      (term key ":" value (opt (+ [space]))
;; 	   `(k v -- (cons (intern k) v)))
;;      (key (substring (and (not ":") (+ [word]))))
;;      (value (or string-value number-value))
;;      (string-value (substring (+ [alpha])))
;;      (number-value (substring (+ [digit]))
;; 		   `(val -- (string-to-number val))))
;;   (peg-run (peg query)))
;;
;; This invocation of `peg-run' would parse this buffer text:
;;
;; name:Jane age:30
;;
;; And return this Elisp sexp:
;;
;; ((age . 30) (name . "Jane"))
;;
;; Note that, in complex grammars, some care must be taken to make
;; sure that the number and type of values drawn from the stack always
;; match those pushed.  In the example above, both `string-value' and
;; `number-value' push a single value to the stack.  Since the `value'
;; rule only includes these two sub-rules, any upstream rule that
;; makes use of `value' can be confident it will always and only push
;; a single value to the stack.
;;
;; Stack action forms are in a sense analogous to lambda forms: the
;; symbols before the "--" are the equivalent of lambda arguments,
;; while the forms after the "--" are return values.  The difference
;; being that a lambda form can only return a single value, while a
;; stack action can push multiple values onto the stack.  It's also
;; perfectly valid to use `(-- FORM...)' or `(VAR... --)': the former
;; pushes values to the stack without consuming any, and the latter
;; pops values from the stack and discards them.
;;
;;;; Derived Operators:
;;
;; The following operators are implemented as combinations of
;; primitive expressions:
;;
;;     (substring E)  ; Match E and push the substring for the matched region.
;;     (region E)     ; Match E and push the start and end positions.
;;     (replace E RPL); Match E and replace the matched region with RPL.
;;     (list E)       ; Match E and push a list of the items that E produced.
;;
;; See `peg-ex-parse-int' in `peg-tests.el' for further examples.
;;
;; Regexp equivalents:
;;
;; Here a some examples for regexps and how those could be written as pex.
;; [Most are taken from rx.el]
;;
;;     "^[a-z]*"
;;     (and (bol) (* [a-z]))
;;
;;     "\n[^ \t]"
;;     (and "\n" (not [" \t"]) (any))
;;
;;     "\\*\\*\\* EOOH \\*\\*\\*\n"
;;     "*** EOOH ***\n"
;;
;;     "\\<\\(catch\\|finally\\)\\>[^_]"
;;     (and (bow) (or "catch" "finally") (eow) (not "_") (any))
;;
;;     "[ \t\n]*:\\([^:]+\\|$\\)"
;;     (and (* [" \t\n"]) ":" (or (+ (not ":") (any)) (eol)))
;;
;;     "^content-transfer-encoding:\\(\n?[\t ]\\)*quoted-printable\\(\n?[\t ]\\)*"
;;     (and (bol)
;;          "content-transfer-encoding:"
;;          (* (opt "\n") ["\t "])
;;          "quoted-printable"
;;          (* (opt "\n") ["\t "]))
;;
;;     "\\$[I]d: [^ ]+ \\([^ ]+\\) "
;;     (and "$Id: " (+ (not " ") (any)) " " (+ (not " ") (any)) " ")
;;
;;     "^;;\\s-*\n\\|^\n"
;;     (or (and (bol) ";;" (* (syntax-class whitespace)) "\n")
;;         (and (bol) "\n"))
;;
;;     "\\\\\\\\\\[\\w+"
;;     (and "\\\\[" (+ (syntax-class word)))
;;
;; See ";;; Examples" in `peg-tests.el' for other examples.
;;
;;;; Rule argument and indirect calls:
;;
;; Rules can take arguments and those arguments can themselves be PEGs.
;; For example:
;;
;;     (define-peg-rule 2-or-more (peg)
;;       (funcall peg)
;;       (funcall peg)
;;       (* (funcall peg)))
;;
;;     ... (peg-parse
;;          ...
;;          (2-or-more (peg foo))
;;          ...
;;          (2-or-more (peg bar))
;;          ...)
;;
;;;; References:
;;
;; [Ford] Bryan Ford. Parsing Expression Grammars: a Recognition-Based
;; Syntactic Foundation. In POPL'04: Proceedings of the 31st ACM
;; SIGPLAN-SIGACT symposium on Principles of Programming Languages,
;; pages 111-122, New York, NY, USA, 2004. ACM Press.
;; http://pdos.csail.mit.edu/~baford/packrat/
;;
;; [Baker] Baker, Henry G. "Pragmatic Parsing in Common Lisp".  ACM Lisp
;; Pointers 4(2), April--June 1991, pp. 3--15.
;; http://home.pipeline.com/~hbaker1/Prag-Parse.html
;;
;; Roman Redziejowski does good PEG related research
;; http://www.romanredz.se/pubs.htm

;;;; Todo:

;; - Fix the exponential blowup in `peg-translate-exp'.
;; - Add a proper debug-spec for PEXs.

;;; News:

;; Since 1.0.1:
;; - Use OClosures to represent PEG rules when available, and let cl-print
;;   display their source code.
;; - New PEX form (with RULES PEX...).
;; - Named rulesets.
;; - You can pass arguments to rules.
;; - New `funcall' rule to call rules indirectly (e.g. a peg you received
;;   as argument).

;; Version 1.0:
;; - New official entry points `peg` and `peg-run`.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defvar peg--actions nil
  "Actions collected along the current parse.
Used at runtime for backtracking.  It's a list ((POS . THUNK)...).
Each THUNK is executed at the corresponding POS.  Thunks are
executed in a post-processing step, not during parsing.")

(defvar peg--errors nil
  "Data keeping track of the rightmost parse failure location.
It's a pair (POSITION . EXPS ...).  POSITION is the buffer position and
EXPS is a list of rules/expressions that failed.")

;;;; Main entry points

(defmacro peg--when-fboundp (f &rest body)
  (declare (indent 1) (debug (sexp body)))
  (when (fboundp f)
    (macroexp-progn body)))

(peg--when-fboundp oclosure-define
  (oclosure-define peg-function
    "Parsing function built from PEG rule."
    pexs)

  (cl-defmethod cl-print-object ((peg peg-function) stream)
    (princ "#f<peg " stream)
    (let ((args (help-function-arglist peg 'preserve-names)))
      (if args
          (prin1 args stream)
        (princ "()" stream)))
    (princ " " stream)
    (prin1 (peg-function--pexs peg) stream)
    (princ ">" stream)))

(defmacro peg--lambda (pexs args &rest body)
  (declare (indent 2)
           (debug (&define form lambda-list def-body)))
  (if (fboundp 'oclosure-lambda)
      `(oclosure-lambda (peg-function (pexs ,pexs)) ,args . ,body)
    `(lambda ,args . ,body)))

;; Sometimes (with-peg-rules ... (peg-run (peg ...))) is too
;; long-winded for the task at hand, so `peg-parse' comes in handy.
(defmacro peg-parse (&rest pexs)
  "Match PEXS at point.
PEXS is a sequence of PEG expressions, implicitly combined with `and'.
Returns STACK if the match succeed and signals an error on failure,
moving point along the way.
For backward compatibility (and convenience) PEXS can also be a list of
RULES in which case we run the first such rule.  In case of ambiguity,
prefix PEXS with \"\" so it doesn't look like a list of rules."
  (if (and (consp (car pexs))
           (symbolp (caar pexs))
           (not (or (get (peg--rule-id (caar pexs)) 'peg--rule-definition)
                    (ignore-errors
                      (not (eq 'call (car (peg-normalize (car pexs)))))))))
      ;; The first of `pexs' has not been defined as a rule, so assume
      ;; that none of them have been and they should be fed to
      ;; `with-peg-rules'
      `(with-peg-rules ,pexs (peg-run (peg ,(caar pexs)) #'peg-signal-failure))
    `(peg-run (peg ,@pexs) #'peg-signal-failure)))

(defmacro peg (&rest pexs)
  "Return a PEG-matcher that matches PEXS."
  (pcase (peg-normalize `(and . ,pexs))
    (`(call ,name) `#',(peg--rule-id name)) ;Optimize this case by η-reduction!
    (exp `(peg--lambda ',pexs () ,(peg-translate-exp exp)))))

;; There are several "infos we want to return" when parsing a given PEX:
;; 1- We want to return the success/failure of the parse.
;; 2- We want to return the data of the successful parse (the stack).
;; 3- We want to return the diagnostic of the failures.
;; 4- We want to perform the actions (upon parse success)!
;; `peg-parse' used an error signal to encode the (1) boolean, which
;; lets it return all the info conveniently but the error signal was sometimes
;; inconvenient.  Other times one wants to just know (1) maybe without even
;; performing (4).
;; `peg-run' lets you choose all that, and by default gives you
;; (1) as a simple boolean, while also doing (2), and (4).

(defun peg-run (peg-matcher &optional failure-function success-function)
  "Parse with PEG-MATCHER at point and run the success/failure function.
If a match was found, move to the end of the match and call SUCCESS-FUNCTION
with one argument: a function which will perform all the actions collected
during the parse and then return the resulting stack (or t if empty).
If no match was found, move to the (rightmost) point of parse failure and call
FAILURE-FUNCTION with one argument, which is a list of PEG expressions that
failed at this point.
SUCCESS-FUNCTION defaults to `funcall' and FAILURE-FUNCTION
defaults to `ignore'."
  (let ((peg--actions '()) (peg--errors '(-1)))
    (if (funcall peg-matcher)
        ;; Found a parse: run the actions collected along the way.
        (funcall (or success-function #'funcall)
                 (lambda ()
                   (save-excursion (peg-postprocess peg--actions))))
      (goto-char (car peg--errors))
      (when failure-function
        (funcall failure-function (peg-merge-errors (cdr peg--errors)))))))

(defmacro define-peg-rule (name args &rest pexs)
  "Define PEG rule NAME as equivalent to PEXS.
The PEG expressions in PEXS are implicitly combined with the
sequencing `and' operator of PEG grammars."
  (declare (indent 2))
  (let ((inline nil))
    (while (keywordp (car pexs))
      (pcase (pop pexs)
        (:inline (setq inline (car pexs))))
      (setq pexs (cdr pexs)))
    (let ((id (peg--rule-id name))
          (exp (peg-normalize `(and . ,pexs))))
      `(progn
         (defalias ',id
           (peg--lambda ',pexs ,args
             ,(if inline
                  ;; Short-circuit to peg--translate in order to skip
                  ;; the extra failure-recording of `peg-translate-exp'.
                  ;; It also skips the cycle detection of
                  ;; `peg--translate-rule-body', which is not the main
                  ;; purpose but we can live with it.
                  (apply #'peg--translate exp)
                (peg--translate-rule-body name exp))))
         (eval-and-compile
           ;; FIXME: We shouldn't need this any more since the info is now
           ;; stored in the function, but sadly we need to find a name's EXP
           ;; during compilation (i.e. before the `defalias' is executed)
           ;; as part of cycle-detection!
           (put ',id 'peg--rule-definition ',exp)
           ,@(when inline
               ;; FIXME: Copied from `defsubst'.
               `(;; Never native-compile defsubsts as we need the byte
                 ;; definition in `byte-compile-unfold-bcf' to perform the
                 ;; inlining (Bug#42664, Bug#43280, Bug#44209).
                 ,(byte-run--set-speed id nil -1)
                 (put ',id 'byte-optimizer #'byte-compile-inline-expand))))))))

(defmacro define-peg-ruleset (name &rest rules)
  "Define a set of PEG rules for later use, e.g., in `with-peg-rules'."
  (declare (indent 1))
  (let ((defs ())
        (aliases ()))
    (dolist (rule rules)
      (let* ((rname (car rule))
             (full-rname (format "%s %s" name rname)))
        (push `(define-peg-rule ,full-rname . ,(cdr rule)) defs)
        (push `(,(peg--rule-id rname) #',(peg--rule-id full-rname)) aliases)))
    (require 'cl-lib)
    `(cl-flet ,aliases
       ,@defs
       (eval-and-compile (put ',name 'peg--rules ',aliases)))))

(defmacro with-peg-rules (rules &rest body)
  "Make PEG rules RULES available within the scope of BODY.
RULES is a list of rules of the form (NAME . PEXS), where PEXS is a sequence
of PEG expressions, implicitly combined with `and'.
RULES can also contain symbols in which case these must name
rulesets defined previously with `define-peg-ruleset'."
  (declare (indent 1) (debug (sexp form))) ;FIXME: `sexp' is not good enough!
  (let* ((rulesets nil)
         (rules
          ;; First, macroexpand the rules.
          (delq nil
                (mapcar (lambda (rule)
                          (if (symbolp rule)
                              (progn (push rule rulesets) nil)
                            (cons (car rule) (peg-normalize `(and . ,(cdr rule))))))
                        rules)))
         (ctx (assq :peg-rules macroexpand-all-environment))
         (body
    (macroexpand-all
     `(cl-labels
          ,(mapcar (lambda (rule)
		     `(,(peg--rule-id (car rule))
		       (peg--lambda ',(cdr rule) ()
		         ,(peg--translate-rule-body (car rule) (cdr rule)))))
		   rules)
        ,@body)
     `((:peg-rules ,@(append rules (cdr ctx)))
       ,@macroexpand-all-environment))))
    (if (null rulesets)
        body
      `(cl-flet ,(mapcan (lambda (ruleset)
                           (let ((aliases (get ruleset 'peg--rules)))
                             (unless aliases
                               (message "Unknown PEG ruleset: %S" ruleset))
                             (copy-sequence aliases)))
                         rulesets)
         ,body))))

;;;;; Old entry points

(defmacro peg-parse-exp (exp)
  "Match the parsing expression EXP at point."
  (declare (obsolete peg-parse "peg-0.9"))
  `(peg-run (peg ,exp)))

;;;; The actual implementation

(defun peg--lookup-rule (name)
  (or (cdr (assq name (cdr (assq :peg-rules macroexpand-all-environment))))
      ;; With `peg-function' objects, we can recover the PEG from which it was
      ;; defined, but this info is not yet available at compile-time.  :-(
      ;;(let ((id (peg--rule-id name)))
      ;;  (peg-function--pexs (symbol-function id)))
      (get (peg--rule-id name) 'peg--rule-definition)))

(defun peg--rule-id (name)
  (intern (format "peg-rule %s" name)))

(define-error 'peg-search-failed "Parse error at %d (expecting %S)")

(defun peg-signal-failure (failures)
  (signal 'peg-search-failed (list (point) failures)))

(defun peg-parse-at-point (peg-matcher)
  "Parse text at point according to the PEG rule PEG-MATCHER."
  (declare (obsolete peg-run "peg-1.0"))
  (peg-run peg-matcher
           #'peg-signal-failure
           (lambda (f) (let ((r (funcall f))) (if (listp r) r)))))

;; Internally we use a regularized syntax, e.g. we only have binary OR
;; nodes.  Regularized nodes are lists of the form (OP ARGS...).
(cl-defgeneric peg-normalize (exp)
  "Return a \"normalized\" form of EXP."
  (error "Invalid parsing expression: %S" exp))

(cl-defmethod peg-normalize ((exp string))
  (let ((len (length exp)))
    (cond ((zerop len) '(guard t))
	  ((= len 1) `(char ,(aref exp 0)))
	  (t `(str ,exp)))))

(cl-defmethod peg-normalize ((exp symbol))
  ;; (peg--lookup-rule exp)
  `(call ,exp))

(cl-defmethod peg-normalize ((exp vector))
  (peg-normalize `(set . ,(append exp '()))))

(cl-defmethod peg-normalize ((exp cons))
  (apply #'peg--macroexpand exp))

(defconst peg-leaf-types '(any call action char range str set
			   guard syntax-class = funcall))

(cl-defgeneric peg--macroexpand (head &rest args)
  (cond
   ((memq head peg-leaf-types) (cons head args))
   (t `(call ,head ,@args))))

(cl-defmethod peg--macroexpand ((_ (eql or)) &rest args)
  (cond ((null args) '(guard nil))
	((null (cdr args)) (peg-normalize (car args)))
	(t `(or ,(peg-normalize (car args))
		,(peg-normalize `(or . ,(cdr args)))))))

(cl-defmethod peg--macroexpand ((_ (eql and)) &rest args)
  (cond ((null args) '(guard t))
	((null (cdr args)) (peg-normalize (car args)))
	(t `(and ,(peg-normalize (car args))
		 ,(peg-normalize `(and . ,(cdr args)))))))

(cl-defmethod peg--macroexpand ((_ (eql *)) &rest args)
  `(* ,(peg-normalize `(and . ,args))))

;; FIXME: this duplicates code; could use some loop to avoid that
(cl-defmethod peg--macroexpand ((_ (eql +)) &rest args)
  (let ((e (peg-normalize `(and . ,args))))
    `(and ,e (* ,e))))

(cl-defmethod peg--macroexpand ((_ (eql opt)) &rest args)
  (let ((e (peg-normalize `(and . ,args))))
    `(or ,e (guard t))))

(cl-defmethod peg--macroexpand ((_ (eql if)) &rest args)
  `(if ,(peg-normalize `(and . ,args))))

(cl-defmethod peg--macroexpand ((_ (eql not)) &rest args)
  `(not ,(peg-normalize `(and . ,args))))

(cl-defmethod peg--macroexpand ((_ (eql \`)) form)
  (peg-normalize `(stack-action ,form)))

(cl-defmethod peg--macroexpand ((_ (eql stack-action)) form)
  (unless (member '-- form)
    (error "Malformed stack action: %S" form))
  (let ((args (cdr (member '-- (reverse form))))
	(values (cdr (member '-- form))))
    (let ((form `(let ,(mapcar (lambda (var) `(,var (pop peg--stack))) args)
		   ,@(or (mapcar (lambda (val) `(push ,val peg--stack)) values)
		         '(nil)))))
      `(action ,form))))

(defvar peg-char-classes
  '(ascii alnum alpha blank cntrl digit graph lower multibyte nonascii print
	  punct space unibyte upper word xdigit))

(cl-defmethod peg--macroexpand ((_ (eql set)) &rest specs)
  (cond ((null specs) '(guard nil))
	((and (null (cdr specs))
	      (let ((range (peg-range-designator (car specs))))
		(and range `(range ,(car range) ,(cdr range))))))
	(t
	 (let ((chars '()) (ranges '()) (classes '()))
	   (while specs
	     (let* ((spec (pop specs))
		    (range (peg-range-designator spec)))
	       (cond (range
		      (push range ranges))
		     ((peg-characterp spec)
		      (push spec chars))
		     ((stringp spec)
		      (setq chars (append (reverse (append spec ())) chars)))
		     ((memq spec peg-char-classes)
		      (push spec classes))
		     (t (error "Invalid set specifier: %S" spec)))))
	   (setq ranges (reverse ranges))
	   (setq chars (delete-dups (reverse chars)))
	   (setq classes (reverse classes))
	   (cond ((and (null ranges)
		       (null classes)
		       (cond ((null chars) '(guard nil))
			     ((null (cdr chars)) `(char ,(car chars))))))
		 (t `(set ,ranges ,chars ,classes)))))))

(defun peg-range-designator (x)
  (and (symbolp x)
       (let ((str (symbol-name x)))
	 (and (= (length str) 3)
	      (eq (aref str 1) ?-)
	      (< (aref str 0) (aref str 2))
	      (cons (aref str 0) (aref str 2))))))

;; characterp is new in Emacs 23.
(defun peg-characterp (x)
  (if (fboundp 'characterp)
      (characterp x)
    (integerp x)))

(cl-defmethod peg--macroexpand ((_ (eql list)) &rest args)
  (peg-normalize
   (let ((marker (make-symbol "magic-marker")))
     `(and (stack-action (-- ',marker))
	   ,@args
	   (stack-action (--
			  (let ((l '()))
			    (while
				(let ((e (pop peg--stack)))
				  (cond ((eq e ',marker) nil)
					((null peg--stack)
					 (error "No marker on stack"))
					(t (push e l) t))))
			    l)))))))

(cl-defmethod peg--macroexpand ((_ (eql substring)) &rest args)
  (peg-normalize
   `(and `(-- (point))
	 ,@args
	 `(start -- (buffer-substring-no-properties start (point))))))

(cl-defmethod peg--macroexpand ((_ (eql region)) &rest args)
  (peg-normalize
   `(and `(-- (point))
	 ,@args
	 `(-- (point)))))

(cl-defmethod peg--macroexpand ((_ (eql replace)) pe replacement)
  (peg-normalize
   `(and (stack-action (-- (point)))
	 ,pe
	 (stack-action (start -- (progn
				   (delete-region start (point))
				   (insert-before-markers ,replacement))))
	 (stack-action (_ --)))))

(cl-defmethod peg--macroexpand ((_ (eql quote)) _form)
  (error "quote is reserved for future use"))

(cl-defgeneric peg--translate (head &rest args)
  (error "No translator for: %S" (cons head args)))

(defun peg--translate-rule-body (name exp)
  (let ((msg (condition-case err
                 (progn (peg-detect-cycles exp (list name)) nil)
               (error (error-message-string err))))
        (code (peg-translate-exp exp)))
    (cond
     ((null msg) code)
     (t (macroexp-warn-and-return msg code 'peg nil exp)))))

;; This is the main translation function.
(defun peg-translate-exp (exp)
  "Return the Emacs Lisp code to match the PE EXP."
  ;; FIXME: This expansion basically duplicates `exp' in the output, which is
  ;; a serious problem because it's done recursively, so it makes the output
  ;; code's size exponentially larger than the input!
  `(or ,(apply #'peg--translate exp)
       (peg--record-failure ',exp))) ; for error reporting

(define-obsolete-function-alias 'peg-record-failure
  #'peg--record-failure "peg-1.0")
(defun peg--record-failure (exp)
  (cond ((= (point) (car peg--errors))
	 (setcdr peg--errors (cons exp (cdr peg--errors))))
	((> (point) (car peg--errors))
	 (setq peg--errors (list (point) exp))))
  nil)

(cl-defmethod peg--translate ((_ (eql and)) e1 e2)
  `(and ,(peg-translate-exp e1)
	,(peg-translate-exp e2)))

;; Choicepoints are used for backtracking.  At a choicepoint we save
;; enough state, so that we can continue from there if needed.
(defun peg--choicepoint-moved-p (choicepoint)
  `(/= ,(car choicepoint) (point)))

(defun peg--choicepoint-restore (choicepoint)
  `(progn
     (goto-char ,(car choicepoint))
     (setq peg--actions ,(cdr choicepoint))))

(defmacro peg--with-choicepoint (var &rest body)
  (declare (indent 1) (debug (symbolp form)))
  `(let ((,var (cons (make-symbol "point") (make-symbol "actions"))))
     `(let ((,(car ,var) (point))
	    (,(cdr ,var) peg--actions))
        ,@(list ,@body))))

(cl-defmethod peg--translate ((_ (eql or)) e1 e2)
  (peg--with-choicepoint cp
    `(or ,(peg-translate-exp e1)
	 (,@(peg--choicepoint-restore cp)
	  ,(peg-translate-exp e2)))))

(cl-defmethod peg--translate ((_ (eql with)) rules &rest exps)
  `(with-peg-rules ,rules ,(peg--translate `(and . ,exps))))

(cl-defmethod peg--translate ((_ (eql guard)) exp) exp)

(defvar peg-syntax-classes
  '((whitespace ?-) (word ?w) (symbol ?_) (punctuation ?.)
    (open ?\() (close ?\)) (string ?\") (escape ?\\) (charquote ?/)
    (math ?$) (prefix ?') (comment ?<) (endcomment ?>)
    (comment-fence ?!) (string-fence ?|)))

(cl-defmethod peg--translate ((_ (eql syntax-class)) class)
  (let ((probe (assoc class peg-syntax-classes)))
    (cond (probe `(when (looking-at ,(format "\\s%c" (cadr probe)))
                    (forward-char)
                    t))
	  (t (error "Invalid syntax class: %S\nMust be one of: %s" class
		    (mapcar #'car peg-syntax-classes))))))

(cl-defmethod peg--translate ((_ (eql =)) string)
  `(let ((str ,string))
     (when (zerop (length str))
       (error "Empty strings not allowed for ="))
     (search-forward str (+ (point) (length str)) t)))

(cl-defmethod peg--translate ((_ (eql *)) e)
  `(progn (while ,(peg--with-choicepoint cp
		    `(if ,(peg-translate-exp e)
                         ;; Just as regexps do for the `*' operator,
                         ;; we allow the body of `*' loops to match
                         ;; the empty string, but we don't repeat the loop if
                         ;; we haven't moved, to avoid inf-loops.
                         ,(peg--choicepoint-moved-p cp)
                       ,(peg--choicepoint-restore cp)
		       nil)))
	  t))

(cl-defmethod peg--translate ((_ (eql if)) e)
  (peg--with-choicepoint cp
    `(when ,(peg-translate-exp e)
       ,(peg--choicepoint-restore cp)
       t)))

(cl-defmethod peg--translate ((_ (eql not)) e)
  (peg--with-choicepoint cp
    `(unless ,(peg-translate-exp e)
       ,(peg--choicepoint-restore cp)
       t)))

(cl-defmethod peg--translate ((_ (eql any)) )
  '(when (not (eobp))
     (forward-char)
     t))

(cl-defmethod peg--translate ((_ (eql char)) c)
  `(when (eq (char-after) ',c)
     (forward-char)
     t))

(cl-defmethod peg--translate ((_ (eql set)) ranges chars classes)
  `(when (looking-at ',(peg-make-charset-regexp ranges chars classes))
     (forward-char)
     t))

(defun peg-make-charset-regexp (ranges chars classes)
  (when (and (not ranges) (not classes) (<= (length chars) 1))
    (error "Bug"))
  (let ((rbracket (member ?\] chars))
	(minus (member ?- chars))
	(hat (member ?^ chars)))
    (dolist (c '(?\] ?- ?^))
      (setq chars (remove c chars)))
    (format "[%s%s%s%s%s%s]"
	    (if rbracket "]" "")
	    (if minus "-" "")
	    (mapconcat (lambda (x) (format "%c-%c" (car x) (cdr x))) ranges "")
	    (mapconcat (lambda (c) (format "[:%s:]" c)) classes "")
	    (mapconcat (lambda (c) (format "%c" c)) chars "")
	    (if hat "^" ""))))

(cl-defmethod peg--translate ((_ (eql range)) from to)
  `(when (and (char-after)
	      (<= ',from (char-after))
	      (<= (char-after) ',to))
     (forward-char)
     t))

(cl-defmethod peg--translate ((_ (eql str)) str)
  `(when (looking-at ',(regexp-quote str))
     (goto-char (match-end 0))
     t))

(cl-defmethod peg--translate ((_ (eql call)) name &rest args)
  `(,(peg--rule-id name) ,@args))

(cl-defmethod peg--translate ((_ (eql funcall)) exp &rest args)
  `(funcall ,exp ,@args))

(cl-defmethod peg--translate ((_ (eql action)) form)
  `(progn
     (push (cons (point) (lambda () ,form)) peg--actions)
     t))

(defvar peg--stack nil)
(defun peg-postprocess (actions)
  "Execute \"actions\"."
  (let  ((peg--stack '())
         (forw-actions ()))
    (pcase-dolist (`(,pos . ,thunk) actions)
      (push (cons (copy-marker pos) thunk) forw-actions))
    (pcase-dolist (`(,pos . ,thunk) forw-actions)
      (goto-char pos)
      (funcall thunk))
    (or peg--stack t)))

;; Left recursion is presumably a common mistake when using PEGs.
;; Here we try to detect such mistakes.  Essentially we traverse the
;; graph as long as we can without consuming input.  When we find a
;; recursive call we signal an error.

(defun peg-detect-cycles (exp path)
  "Signal an error on a cycle.
Otherwise traverse EXP recursively and return T if EXP can match
without consuming input.  Return nil if EXP definitely consumes
input.  PATH is the list of rules that we have visited so far."
  (apply #'peg--detect-cycles path exp))

(cl-defgeneric peg--detect-cycles (head _path &rest args)
  (error "No detect-cycle method for: %S" (cons head args)))

(cl-defmethod peg--detect-cycles (path (_ (eql call)) name &rest _args)
  (if (member name path)
      (error "Possible left recursion: %s"
	     (mapconcat (lambda (x) (format "%s" x))
			(reverse (cons name path)) " -> "))
    (let ((exp (peg--lookup-rule name)))
      (if (null exp)
          ;; If there's no rule by that name, either we'll fail at
          ;; run-time or it will be defined later.  In any case, at this
          ;; point there's no evidence of a cycle, and if a cycle appears
          ;; later we'll hopefully catch it when the rule gets defined.
          ;; FIXME: In practice, if `name' is part of the cycle, we will
          ;; indeed detect it when it gets defined, but OTOH if `name'
          ;; is not part of a cycle but it *enables* a cycle because
          ;; it matches the empty string (i.e. we should have returned t
          ;; here), then we may not catch the problem at all :-(
          nil
	(peg-detect-cycles exp (cons name path))))))

(cl-defmethod peg--detect-cycles (path (_ (eql and)) e1 e2)
  (and (peg-detect-cycles e1 path)
       (peg-detect-cycles e2 path)))

(cl-defmethod peg--detect-cycles (path (_ (eql or)) e1 e2)
  (or (peg-detect-cycles e1 path)
      (peg-detect-cycles e2 path)))

(cl-defmethod peg--detect-cycles (path (_ (eql *)) e)
  (peg-detect-cycles e path)
  t)

(cl-defmethod peg--detect-cycles (path (_ (eql if)) e)
  (peg-unary-nullable e path))
(cl-defmethod peg--detect-cycles (path (_ (eql not)) e)
  (peg-unary-nullable e path))

(defun peg-unary-nullable (exp path)
  (peg-detect-cycles exp path)
  t)

(cl-defmethod peg--detect-cycles (_path (_ (eql any)))           nil)
(cl-defmethod peg--detect-cycles (_path (_ (eql char)) _c)       nil)
(cl-defmethod peg--detect-cycles (_path (_ (eql set)) _r _c _k)  nil)
(cl-defmethod peg--detect-cycles (_path (_ (eql range)) _c1 _c2) nil)
(cl-defmethod peg--detect-cycles (_path (_ (eql str)) s)         (equal s ""))
(cl-defmethod peg--detect-cycles (_path (_ (eql guard)) _e)      t)
(cl-defmethod peg--detect-cycles (_path (_ (eql =)) _s)          nil)
(cl-defmethod peg--detect-cycles (_path (_ (eql syntax-class)) _n) nil)

(cl-defmethod peg--detect-cycles (_path (_ (eql funcall)) &rest _args)
  ;; There might very well be a cycle here, and we may very well match
  ;; the empty string, but it's much too hard (and in general
  ;; impossible) to try and figure out.
  nil)

(cl-defmethod peg--detect-cycles (_path (_ (eql action)) _form)  t)

(defun peg-merge-errors (exps)
  "Build a more readable error message out of failed expression."
  (let ((merged '()))
    (dolist (exp exps)
      (setq merged (peg-merge-error exp merged)))
    merged))

(defun peg-merge-error (exp merged)
  (apply #'peg--merge-error merged exp))

(cl-defgeneric peg--merge-error (merged head &rest args)
  (cl-adjoin (cons head args) merged :test #'equal))

(cl-defmethod peg--merge-error (merged (_ (eql or)) e1 e2)
  (peg-merge-error e2 (peg-merge-error e1 merged)))

(cl-defmethod peg--merge-error (merged (_ (eql and)) e1 _e2)
  ;; FIXME: Why is `e2' not used?
  (peg-merge-error e1 merged))

(cl-defmethod peg--merge-error (merged (_ (eql str)) str)
  ;;(add-to-list 'merged str)
  (cl-adjoin str merged :test #'equal))

(cl-defmethod peg--merge-error (merged (_ (eql call)) rule &rest args)
  (cl-adjoin (if args (cons rule args) rule) merged :test #'equal))

(cl-defmethod peg--merge-error (merged (_ (eql char)) char)
  (cl-adjoin (string char) merged :test #'equal))

(cl-defmethod peg--merge-error (merged (_ (eql set)) r c k)
  (cl-adjoin (peg-make-charset-regexp r c k) merged :test #'equal))

(cl-defmethod peg--merge-error (merged (_ (eql range)) from to)
  (cl-adjoin (format "[%c-%c]" from to) merged :test #'equal))

(cl-defmethod peg--merge-error (merged (_ (eql *)) exp)
  (peg-merge-error exp merged))

(cl-defmethod peg--merge-error (merged (_ (eql action)) _action) merged)
(cl-defmethod peg--merge-error (merged (_ (eql guard)) e)
  (if (eq e t) merged (cl-call-next-method)))

(provide 'peg)
(require 'peg)

(define-peg-rule null () :inline t (guard t))
(define-peg-rule fail () :inline t (guard nil))
(define-peg-rule bob  () :inline t (guard (bobp)))
(define-peg-rule eob  () :inline t (guard (eobp)))
(define-peg-rule bol  () :inline t (guard (bolp)))
(define-peg-rule eol  () :inline t (guard (eolp)))
(define-peg-rule bow  () :inline t (guard (looking-at "\\<")))
(define-peg-rule eow  () :inline t (guard (looking-at "\\>")))
(define-peg-rule bos  () :inline t (guard (looking-at "\\_<")))
(define-peg-rule eos  () :inline t (guard (looking-at "\\_>")))

;;; peg.el ends here
