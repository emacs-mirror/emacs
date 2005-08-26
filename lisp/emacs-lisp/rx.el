;;; rx.el --- sexp notation for regular expressions

;; Copyright (C) 2001, 2003, 2004, 2005  Free Software Foundation, Inc.

;; Author: Gerd Moellmann <gerd@gnu.org>
;; Maintainer: FSF
;; Keywords: strings, regexps, extensions

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is another implementation of sexp-form regular expressions.
;; It was unfortunately written without being aware of the Sregex
;; package coming with Emacs, but as things stand, Rx completely
;; covers all regexp features, which Sregex doesn't, doesn't suffer
;; from the bugs mentioned in the commentary section of Sregex, and
;; uses a nicer syntax (IMHO, of course :-).

;; This significantly extended version of the original, is almost
;; compatible with Sregex.  The only incompatibility I (fx) know of is
;; that the `repeat' form can't have multiple regexp args.

;; Now alternative forms are provided for a degree of compatibility
;; with Shivers' attempted definitive SRE notation
;; <URL:http://www.ai.mit.edu/~/shivers/sre.txt>.  SRE forms not
;; catered for include: dsm, uncase, w/case, w/nocase, ,@<exp>,
;; ,<exp>, (word ...), word+, posix-string, and character class forms.
;; Some forms are inconsistent with SRE, either for historical reasons
;; or because of the implementation -- simple translation into Emacs
;; regexp strings.  These include: any, word.  Also, case-sensitivity
;; and greediness are controlled by variables external to the regexp,
;; and you need to feed the forms to the `posix-' functions to get
;; SRE's POSIX semantics.  There are probably more difficulties.

;; Rx translates a sexp notation for regular expressions into the
;; usual string notation.  The translation can be done at compile-time
;; by using the `rx' macro.  It can be done at run-time by calling
;; function `rx-to-string'.  See the documentation of `rx' for a
;; complete description of the sexp notation.
;;
;; Some examples of string regexps and their sexp counterparts:
;;
;; "^[a-z]*"
;; (rx (and line-start (0+ (in "a-z"))))
;;
;; "\n[^ \t]"
;; (rx (and "\n" (not blank))), or
;; (rx (and "\n" (not (any " \t"))))
;;
;; "\\*\\*\\* EOOH \\*\\*\\*\n"
;; (rx "*** EOOH ***\n")
;;
;; "\\<\\(catch\\|finally\\)\\>[^_]"
;; (rx (and word-start (submatch (or "catch" "finally")) word-end
;;          (not (any ?_))))
;;
;; "[ \t\n]*:\\([^:]+\\|$\\)"
;; (rx (and (zero-or-more (in " \t\n")) ":"
;;          (submatch (or line-end (one-or-more (not (any ?:)))))))
;;
;; "^content-transfer-encoding:\\(\n?[\t ]\\)*quoted-printable\\(\n?[\t ]\\)*"
;; (rx (and line-start
;;          "content-transfer-encoding:"
;;          (+ (? ?\n)) blank
;;	    "quoted-printable"
;;	    (+ (? ?\n)) blank))
;;
;; (concat "^\\(?:" something-else "\\)")
;; (rx (and line-start (eval something-else))), statically or
;; (rx-to-string '(and line-start ,something-else)), dynamically.
;;
;; (regexp-opt '(STRING1 STRING2 ...))
;; (rx (or STRING1 STRING2 ...)), or in other words, `or' automatically
;; calls `regexp-opt' as needed.
;;
;; "^;;\\s-*\n\\|^\n"
;; (rx (or (and line-start ";;" (0+ space) ?\n)
;;         (and line-start ?\n)))
;;
;; "\\$[I]d: [^ ]+ \\([^ ]+\\) "
;; (rx (and "$Id: "
;;          (1+ (not (in " ")))
;;          " "
;;          (submatch (1+ (not (in " "))))
;;          " "))
;;
;; "\\\\\\\\\\[\\w+"
;; (rx (and ?\\ ?\\ ?\[ (1+ word)))
;;
;; etc.

;;; History:
;;

;;; Code:

(defconst rx-constituents
  '((and		. (rx-and 1 nil))
    (seq		. and)		; SRE
    (:			. and)		; SRE
    (sequence		. and)		; sregex
    (or			. (rx-or 1 nil))
    (|			. or)		; SRE
    (not-newline	. ".")
    (nonl		. not-newline)	; SRE
    (anything		. ".\\|\n")
    (any		. (rx-any 1 nil rx-check-any)) ; inconsistent with SRE
    (in			. any)
    (char		. any)		; sregex
    (not-char		. (rx-not-char 1 nil rx-check-any)) ; sregex
    (not		. (rx-not 1 1 rx-check-not))
    ;; Partially consistent with sregex, whose `repeat' is like our
    ;; `**'.  (`repeat' with optional max arg and multiple sexp forms
    ;; is ambiguous.)
    (repeat		. (rx-repeat 2 3))
    (=			. (rx-= 2 nil))	   ; SRE
    (>=			. (rx->= 2 nil))   ; SRE
    (**			. (rx-** 2 nil))   ; SRE
    (submatch		. (rx-submatch 1 nil)) ; SRE
    (group		. submatch)
    (zero-or-more	. (rx-kleene 1 nil))
    (one-or-more	. (rx-kleene 1 nil))
    (zero-or-one	. (rx-kleene 1 nil))
    (\?			. zero-or-one)	; SRE
    (\??		. zero-or-one)
    (*			. zero-or-more)	; SRE
    (*?			. zero-or-more)
    (0+			. zero-or-more)
    (+			. one-or-more)	; SRE
    (+?			. one-or-more)
    (1+			. one-or-more)
    (optional		. zero-or-one)
    (opt		. zero-or-one)	; sregex
    (minimal-match	. (rx-greedy 1 1))
    (maximal-match	. (rx-greedy 1 1))
    (backref		. (rx-backref 1 1 rx-check-backref))
    (line-start		. "^")
    (bol		. line-start)	; SRE
    (line-end		. "$")
    (eol		. line-end)	; SRE
    (string-start	. "\\`")
    (bos		. string-start)	; SRE
    (bot		. string-start)	; sregex
    (string-end		. "\\'")
    (eos		. string-end)	; SRE
    (eot		. string-end)	; sregex
    (buffer-start	. "\\`")
    (buffer-end		. "\\'")
    (point		. "\\=")
    (word-start		. "\\<")
    (bow		. word-start)	; SRE
    (word-end		. "\\>")
    (eow		. word-end)	; SRE
    (word-boundary	. "\\b")
    (not-word-boundary	. "\\B")	; sregex
    (symbol-start       . "\\_<")
    (symbol-end         . "\\_>")
    (syntax		. (rx-syntax 1 1))
    (not-syntax		. (rx-not-syntax 1 1)) ; sregex
    (category		. (rx-category 1 1 rx-check-category))
    (eval		. (rx-eval 1 1))
    (regexp		. (rx-regexp 1 1 stringp))
    (digit		. "[[:digit:]]")
    (numeric		. digit)	; SRE
    (num		. digit)	; SRE
    (control		. "[[:cntrl:]]") ; SRE
    (cntrl		. control)	 ; SRE
    (hex-digit		. "[[:xdigit:]]") ; SRE
    (hex		. hex-digit)	  ; SRE
    (xdigit		. hex-digit)	  ; SRE
    (blank		. "[[:blank:]]")  ; SRE
    (graphic		. "[[:graph:]]")  ; SRE
    (graph		. graphic)	  ; SRE
    (printing		. "[[:print:]]")  ; SRE
    (print		. printing)	  ; SRE
    (alphanumeric	. "[[:alnum:]]")  ; SRE
    (alnum		. alphanumeric)	  ; SRE
    (letter		. "[[:alpha:]]")
    (alphabetic		. letter)	; SRE
    (alpha		. letter)	; SRE
    (ascii		. "[[:ascii:]]") ; SRE
    (nonascii		. "[[:nonascii:]]")
    (lower		. "[[:lower:]]") ; SRE
    (lower-case		. lower)	 ; SRE
    (punctuation	. "[[:punct:]]") ; SRE
    (punct		. punctuation)	 ; SRE
    (space		. "[[:space:]]") ; SRE
    (whitespace		. space)	 ; SRE
    (white		. space)	 ; SRE
    (upper		. "[[:upper:]]") ; SRE
    (upper-case		. upper)	 ; SRE
    (word		. "[[:word:]]")	 ; inconsistent with SRE
    (wordchar		. word)		 ; sregex
    (not-wordchar	. "[^[:word:]]") ; sregex (use \\W?)
    )
  "Alist of sexp form regexp constituents.
Each element of the alist has the form (SYMBOL . DEFN).
SYMBOL is a valid constituent of sexp regular expressions.
If DEFN is a string, SYMBOL is translated into DEFN.
If DEFN is a symbol, use the definition of DEFN, recursively.
Otherwise, DEFN must be a list (FUNCTION MIN-ARGS MAX-ARGS PREDICATE).
FUNCTION is used to produce code for SYMBOL.  MIN-ARGS and MAX-ARGS
are the minimum and maximum number of arguments the function-form
sexp constituent SYMBOL may have in sexp regular expressions.
MAX-ARGS nil means no limit.  PREDICATE, if specified, means that
all arguments must satisfy PREDICATE.")


(defconst rx-syntax
  '((whitespace		. ?-)
    (punctuation	. ?.)
    (word		. ?w)
    (symbol		. ?_)
    (open-parenthesis	. ?\()
    (close-parenthesis	. ?\))
    (expression-prefix	. ?\')
    (string-quote	. ?\")
    (paired-delimiter	. ?$)
    (escape		. ?\\)
    (character-quote	. ?/)
    (comment-start	. ?<)
    (comment-end	. ?>)
    (string-delimiter	. ?|)
    (comment-delimiter	. ?!))
  "Alist mapping Rx syntax symbols to syntax characters.
Each entry has the form (SYMBOL . CHAR), where SYMBOL is a valid
symbol in `(syntax SYMBOL)', and CHAR is the syntax character
corresponding to SYMBOL, as it would be used with \\s or \\S in
regular expressions.")


(defconst rx-categories
  '((consonant			. ?0)
    (base-vowel			. ?1)
    (upper-diacritical-mark	. ?2)
    (lower-diacritical-mark	. ?3)
    (tone-mark			. ?4)
    (symbol			. ?5)
    (digit			. ?6)
    (vowel-modifying-diacritical-mark . ?7)
    (vowel-sign			. ?8)
    (semivowel-lower		. ?9)
    (not-at-end-of-line		. ?<)
    (not-at-beginning-of-line	. ?>)
    (alpha-numeric-two-byte	. ?A)
    (chinse-two-byte		. ?C)
    (greek-two-byte		. ?G)
    (japanese-hiragana-two-byte . ?H)
    (indian-two-byte		. ?I)
    (japanese-katakana-two-byte . ?K)
    (korean-hangul-two-byte	. ?N)
    (cyrillic-two-byte		. ?Y)
    (combining-diacritic	. ?^)
    (ascii			. ?a)
    (arabic			. ?b)
    (chinese			. ?c)
    (ethiopic			. ?e)
    (greek			. ?g)
    (korean			. ?h)
    (indian			. ?i)
    (japanese			. ?j)
    (japanese-katakana		. ?k)
    (latin			. ?l)
    (lao			. ?o)
    (tibetan			. ?q)
    (japanese-roman		. ?r)
    (thai			. ?t)
    (vietnamese			. ?v)
    (hebrew			. ?w)
    (cyrillic			. ?y)
    (can-break			. ?|))
  "Alist mapping symbols to category characters.
Each entry has the form (SYMBOL . CHAR), where SYMBOL is a valid
symbol in `(category SYMBOL)', and CHAR is the category character
corresponding to SYMBOL, as it would be used with `\\c' or `\\C' in
regular expression strings.")


(defvar rx-greedy-flag t
  "Non-nil means produce greedy regular expressions for `zero-or-one',
`zero-or-more', and `one-or-more'.  Dynamically bound.")


(defun rx-info (op)
  "Return parsing/code generation info for OP.
If OP is the space character ASCII 32, return info for the symbol `?'.
If OP is the character `?', return info for the symbol `??'.
See also `rx-constituents'."
  (cond ((eq op ? ) (setq op '\?))
	((eq op ??) (setq op '\??)))
  (while (and (not (null op)) (symbolp op))
    (setq op (cdr (assq op rx-constituents))))
  op)


(defun rx-check (form)
  "Check FORM according to its car's parsing info."
  (unless (listp form)
    (error "rx `%s' needs argument(s)" form))
  (let* ((rx (rx-info (car form)))
	 (nargs (1- (length form)))
	 (min-args (nth 1 rx))
	 (max-args (nth 2 rx))
	 (type-pred (nth 3 rx)))
    (when (and (not (null min-args))
	       (< nargs min-args))
      (error "rx form `%s' requires at least %d args"
	     (car form) min-args))
    (when (and (not (null max-args))
	       (> nargs max-args))
      (error "rx form `%s' accepts at most %d args"
	     (car form) max-args))
    (when (not (null type-pred))
      (dolist (sub-form (cdr form))
	(unless (funcall type-pred sub-form)
	  (error "rx form `%s' requires args satisfying `%s'"
		 (car form) type-pred))))))


(defun rx-and (form)
  "Parse and produce code from FORM.
FORM is of the form `(and FORM1 ...)'."
  (rx-check form)
  (concat "\\(?:"
	  (mapconcat
	   (function (lambda (x) (rx-to-string x 'no-group)))
	   (cdr form) nil)
	  "\\)"))


(defun rx-or (form)
  "Parse and produce code from FORM, which is `(or FORM1 ...)'."
  (rx-check form)
  (let ((all-args-strings t))
    (dolist (arg (cdr form))
      (unless (stringp arg)
	(setq all-args-strings nil)))
    (concat "\\(?:"
	    (if all-args-strings
		(regexp-opt (cdr form))
	      (mapconcat #'rx-to-string (cdr form) "\\|"))
	    "\\)")))


(defvar rx-bracket)		       ; dynamically bound in `rx-any'

(defun rx-check-any (arg)
   "Check arg ARG for Rx `any'."
   (if (integerp arg)
       (setq arg (string arg)))
   (when (stringp arg)
     (if (zerop (length arg))
	 (error "String arg for Rx `any' must not be empty"))
     ;; Quote ^ at start; don't bother to check whether this is first arg.
     (if (eq ?^ (aref arg 0))
	 (setq arg (concat "\\" arg)))
     ;; Remove ] and set flag for adding it to start of overall result.
     (when (string-match "]" arg)
       (setq arg (replace-regexp-in-string "]" "" arg)
	     rx-bracket "]")))
   (when (symbolp arg)
     (let ((translation (condition-case nil
			    (rx-to-string arg 'no-group)
			  (error nil))))
       (unless translation (error "Invalid char class `%s' in Rx `any'" arg))
       (setq arg (substring translation 1 -1)))) ; strip outer brackets
   ;; sregex compatibility
   (when (and (integerp (car-safe arg))
	      (integerp (cdr-safe arg)))
     (setq arg (string (car arg) ?- (cdr arg))))
   (unless (stringp arg)
     (error "rx `any' requires string, character, char pair or char class args"))
   arg)

(defun rx-any (form)
  "Parse and produce code from FORM, which is `(any ARG ...)'.
ARG is optional."
  (rx-check form)
  (let* ((rx-bracket nil)
	 (args (mapcar #'rx-check-any (cdr form)))) ; side-effects `rx-bracket'
    ;; If there was a ?- in the form, move it to the front to avoid
    ;; accidental range.
    (if (member "-" args)
	(setq args (cons "-" (delete "-" args))))
    (apply #'concat "[" rx-bracket (append args '("]")))))


(defun rx-check-not (arg)
  "Check arg ARG for Rx `not'."
  (unless (or (and (symbolp arg)
		   (string-match "\\`\\[\\[:[-a-z]:]]\\'"
				 (condition-case nil
				     (rx-to-string arg 'no-group)
				   (error ""))))
	      (eq arg 'word-boundary)
	      (and (consp arg)
		   (memq (car arg) '(not any in syntax category))))
    (error "rx `not' syntax error: %s" arg))
  t)


(defun rx-not (form)
  "Parse and produce code from FORM.  FORM is `(not ...)'."
  (rx-check form)
  (let ((result (rx-to-string (cadr form) 'no-group))
	case-fold-search)
    (cond ((string-match "\\`\\[^" result)
	   (if (= (length result) 4)
	       (substring result 2 3)
	     (concat "[" (substring result 2))))
	  ((eq ?\[ (aref result 0))
	   (concat "[^" (substring result 1)))
	  ((string-match "\\`\\\\[scb]" result)
	   (concat (capitalize (substring result 0 2)) (substring result 2)))
	  (t
	   (concat "[^" result "]")))))


(defun rx-not-char (form)
  "Parse and produce code from FORM.  FORM is `(not-char ...)'."
  (rx-check form)
  (rx-not `(not (in ,@(cdr form)))))


(defun rx-not-syntax (form)
  "Parse and produce code from FORM.  FORM is `(not-syntax SYNTAX)'."
  (rx-check form)
  (rx-not `(not (syntax ,@(cdr form)))))


(defun rx-trans-forms (form &optional skip)
  "If FORM's length is greater than two, transform it to length two.
A form (HEAD REST ...) becomes (HEAD (and REST ...)).
If SKIP is non-nil, allow that number of items after the head, i.e.
`(= N REST ...)' becomes `(= N (and REST ...))' if SKIP is 1."
  (unless skip (setq skip 0))
  (let ((tail (nthcdr (1+ skip) form)))
    (if (= (length tail) 1)
	form
      (let ((form (copy-sequence form)))
	(setcdr (nthcdr skip form) (list (cons 'and tail)))
	form))))


(defun rx-= (form)
  "Parse and produce code from FORM `(= N ...)'."
  (rx-check form)
  (setq form (rx-trans-forms form 1))
  (unless (and (integerp (nth 1 form))
	       (> (nth 1 form) 0))
    (error "rx `=' requires positive integer first arg"))
  (format "%s\\{%d\\}" (rx-to-string (nth 2 form)) (nth 1 form)))


(defun rx->= (form)
  "Parse and produce code from FORM `(>= N ...)'."
  (rx-check form)
  (setq form (rx-trans-forms form 1))
  (unless (and (integerp (nth 1 form))
	       (> (nth 1 form) 0))
    (error "rx `>=' requires positive integer first arg"))
  (format "%s\\{%d,\\}" (rx-to-string (nth 2 form)) (nth 1 form)))


(defun rx-** (form)
  "Parse and produce code from FORM `(** N M ...)'."
  (rx-check form)
  (setq form (cons 'repeat (cdr (rx-trans-forms form 2))))
  (rx-to-string form))


(defun rx-repeat (form)
  "Parse and produce code from FORM.
FORM is either `(repeat N FORM1)' or `(repeat N M FORM1)'."
  (rx-check form)
  (cond ((= (length form) 3)
	 (unless (and (integerp (nth 1 form))
		      (> (nth 1 form) 0))
	   (error "rx `repeat' requires positive integer first arg"))
	 (format "%s\\{%d\\}" (rx-to-string (nth 2 form)) (nth 1 form)))
	((or (not (integerp (nth 2 form)))
	     (< (nth 2 form) 0)
	     (not (integerp (nth 1 form)))
	     (< (nth 1 form) 0)
	     (< (nth 2 form) (nth 1 form)))
	 (error "rx `repeat' range error"))
	(t
	 (format "%s\\{%d,%d\\}" (rx-to-string (nth 3 form))
		 (nth 1 form) (nth 2 form)))))


(defun rx-submatch (form)
  "Parse and produce code from FORM, which is `(submatch ...)'."
  (concat "\\("
	  (mapconcat (function (lambda (x) (rx-to-string x 'no-group)))
		     (cdr form) nil)
	  "\\)"))

(defun rx-backref (form)
  "Parse and produce code from FORM, which is `(backref N)'."
  (rx-check form)
  (format "\\%d" (nth 1 form)))

(defun rx-check-backref (arg)
  "Check arg ARG for Rx `backref'."
  (or (and (integerp arg) (>= arg 1) (<= arg 9))
      (error "rx `backref' requires numeric 1<=arg<=9: %s" arg)))

(defun rx-kleene (form)
  "Parse and produce code from FORM.
FORM is `(OP FORM1)', where OP is one of the `zero-or-one',
`zero-or-more' etc.  operators.
If OP is one of `*', `+', `?', produce a greedy regexp.
If OP is one of `*?', `+?', `??', produce a non-greedy regexp.
If OP is anything else, produce a greedy regexp if `rx-greedy-flag'
is non-nil."
  (rx-check form)
  (setq form (rx-trans-forms form))
  (let ((suffix (cond ((memq (car form) '(* + ? )) "")
		      ((memq (car form) '(*? +? ??)) "?")
		      (rx-greedy-flag "")
		      (t "?")))
	(op (cond ((memq (car form) '(* *? 0+ zero-or-more)) "*")
		  ((memq (car form) '(+ +? 1+ one-or-more))  "+")
		  (t "?")))
	(result (rx-to-string (cadr form) 'no-group)))
    (if (not (rx-atomic-p result))
	(setq result (concat "\\(?:" result "\\)")))
    (concat result op suffix)))

(defun rx-atomic-p (r)
  "Return non-nil if regexp string R is atomic.
An atomic regexp R is one such that a suffix operator
appended to R will apply to all of R.  For example, \"a\"
\"[abc]\" and \"\\(ab\\|ab*c\\)\" are atomic and \"ab\",
\"[ab]c\", and \"ab\\|ab*c\" are not atomic.

This function may return false negatives, but it will not
return false positives.  It is nevertheless useful in
situations where an efficiency shortcut can be taken iff a
regexp is atomic.  The function can be improved to detect
more cases of atomic regexps.  Presently, this function
detects the following categories of atomic regexp;

  a group or shy group:  \\(...\\)
  a character class:     [...]
  a single character:    a

On the other hand, false negatives will be returned for
regexps that are atomic but end in operators, such as
\"a+\".  I think these are rare.  Probably such cases could
be detected without much effort.  A guarantee of no false
negatives would require a theoretic specification of the set
of all atomic regexps."
  (let ((l (length r)))
    (or (equal l 1)
	(and (>= l 6)
	     (equal (substring r 0 2) "\\(")
	     (equal (substring r -2) "\\)"))
	(and (>= l 2)
	     (equal (substring r 0 1) "[")
	     (equal (substring r -1) "]")))))


(defun rx-syntax (form)
  "Parse and produce code from FORM, which is `(syntax SYMBOL)'."
  (rx-check form)
  (let* ((sym (cadr form))
	 (syntax (assq sym rx-syntax)))
    (unless syntax
      ;; Try sregex compatibility.
      (let ((name (symbol-name sym)))
	(if (= 1 (length name))
	    (setq syntax (rassq (aref name 0) rx-syntax))))
      (unless syntax
	(error "Unknown rx syntax `%s'" (cadr form))))
    (format "\\s%c" (cdr syntax))))


(defun rx-check-category (form)
  "Check the argument FORM of a `(category FORM)'."
  (unless (or (integerp form)
	      (cdr (assq form rx-categories)))
    (error "Unknown category `%s'" form))
  t)


(defun rx-category (form)
  "Parse and produce code from FORM, which is `(category SYMBOL)'."
  (rx-check form)
  (let ((char (if (integerp (cadr form))
		  (cadr form)
		(cdr (assq (cadr form) rx-categories)))))
    (format "\\c%c" char)))


(defun rx-eval (form)
  "Parse and produce code from FORM, which is `(eval FORM)'."
  (rx-check form)
  (rx-to-string (eval (cadr form))))


(defun rx-greedy (form)
  "Parse and produce code from FORM.
If FORM is '(minimal-match FORM1)', non-greedy versions of `*',
`+', and `?' operators will be used in FORM1.  If FORM is
'(maximal-match FORM1)', greedy operators will be used."
  (rx-check form)
  (let ((rx-greedy-flag (eq (car form) 'maximal-match)))
    (rx-to-string (cadr form))))


(defun rx-regexp (form)
  "Parse and produce code from FORM, which is `(regexp STRING)'."
  (rx-check form)
  (concat "\\(?:" (cadr form) "\\)"))


;;;###autoload
(defun rx-to-string (form &optional no-group)
  "Parse and produce code for regular expression FORM.
FORM is a regular expression in sexp form.
NO-GROUP non-nil means don't put shy groups around the result."
  (cond ((stringp form)
	 (regexp-quote form))
	((integerp form)
	 (regexp-quote (char-to-string form)))
	((symbolp form)
	 (let ((info (rx-info form)))
	   (cond ((stringp info)
		  info)
		 ((null info)
		  (error "Unknown rx form `%s'" form))
		 (t
		  (funcall (nth 0 info) form)))))
	((consp form)
	 (let ((info (rx-info (car form))))
	   (unless (consp info)
	     (error "Unknown rx form `%s'" (car form)))
	   (let ((result (funcall (nth 0 info) form)))
	     (if (or no-group (string-match "\\`\\\\[(]" result))
		 result
	       (concat "\\(?:" result "\\)")))))
	(t
	 (error "rx syntax error at `%s'" form))))


;;;###autoload
(defmacro rx (&rest regexps)
  "Translate regular expressions REGEXPS in sexp form to a regexp string.
REGEXPS is a non-empty sequence of forms of the sort listed below.
See also `rx-to-string' for how to do such a translation at run-time.

The following are valid subforms of regular expressions in sexp
notation.

STRING
     matches string STRING literally.

CHAR
     matches character CHAR literally.

`not-newline', `nonl'
     matches any character except a newline.
			.
`anything'
     matches any character

`(any SET ...)'
`(in SET ...)'
`(char SET ...)'
     matches any character in SET ....  SET may be a character or string.
     Ranges of characters can be specified as `A-Z' in strings.
     Ranges may also be specified as conses like `(?A . ?Z)'.

     SET may also be the name of a character class: `digit',
     `control', `hex-digit', `blank', `graph', `print', `alnum',
     `alpha', `ascii', `nonascii', `lower', `punct', `space', `upper',
     `word', or one of their synonyms.

`(not (any SET ...))'
     matches any character not in SET ...

`line-start', `bol'
     matches the empty string, but only at the beginning of a line
     in the text being matched

`line-end', `eol'
     is similar to `line-start' but matches only at the end of a line

`string-start', `bos', `bot'
     matches the empty string, but only at the beginning of the
     string being matched against.

`string-end', `eos', `eot'
     matches the empty string, but only at the end of the
     string being matched against.

`buffer-start'
     matches the empty string, but only at the beginning of the
     buffer being matched against.  Actually equivalent to `string-start'.

`buffer-end'
     matches the empty string, but only at the end of the
     buffer being matched against.  Actually equivalent to `string-end'.

`point'
     matches the empty string, but only at point.

`word-start', `bow'
     matches the empty string, but only at the beginning or end of a
     word.

`word-end', `eow'
     matches the empty string, but only at the end of a word.

`word-boundary'
     matches the empty string, but only at the beginning or end of a
     word.

`(not word-boundary)'
`not-word-boundary'
     matches the empty string, but not at the beginning or end of a
     word.

`digit', `numeric', `num'
     matches 0 through 9.

`control', `cntrl'
     matches ASCII control characters.

`hex-digit', `hex', `xdigit'
     matches 0 through 9, a through f and A through F.

`blank'
     matches space and tab only.

`graphic', `graph'
     matches graphic characters--everything except ASCII control chars,
     space, and DEL.

`printing', `print'
     matches printing characters--everything except ASCII control chars
     and DEL.

`alphanumeric', `alnum'
     matches letters and digits.  (But at present, for multibyte characters,
     it matches anything that has word syntax.)

`letter', `alphabetic', `alpha'
     matches letters.  (But at present, for multibyte characters,
     it matches anything that has word syntax.)

`ascii'
     matches ASCII (unibyte) characters.

`nonascii'
     matches non-ASCII (multibyte) characters.

`lower', `lower-case'
     matches anything lower-case.

`upper', `upper-case'
     matches anything upper-case.

`punctuation', `punct'
     matches punctuation.  (But at present, for multibyte characters,
     it matches anything that has non-word syntax.)

`space', `whitespace', `white'
     matches anything that has whitespace syntax.

`word', `wordchar'
     matches anything that has word syntax.

`not-wordchar'
     matches anything that has non-word syntax.

`(syntax SYNTAX)'
     matches a character with syntax SYNTAX.  SYNTAX must be one
     of the following symbols, or a symbol corresponding to the syntax
     character, e.g. `\\.' for `\\s.'.

     `whitespace'		(\\s- in string notation)
     `punctuation'		(\\s.)
     `word'			(\\sw)
     `symbol'			(\\s_)
     `open-parenthesis'		(\\s()
     `close-parenthesis'	(\\s))
     `expression-prefix'	(\\s')
     `string-quote'		(\\s\")
     `paired-delimiter'		(\\s$)
     `escape'			(\\s\\)
     `character-quote'		(\\s/)
     `comment-start'		(\\s<)
     `comment-end'		(\\s>)
     `string-delimiter'		(\\s|)
     `comment-delimiter'	(\\s!)

`(not (syntax SYNTAX))'
     matches a character that doesn't have syntax SYNTAX.

`(category CATEGORY)'
     matches a character with category CATEGORY.  CATEGORY must be
     either a character to use for C, or one of the following symbols.

     `consonant'			(\\c0 in string notation)
     `base-vowel'			(\\c1)
     `upper-diacritical-mark'		(\\c2)
     `lower-diacritical-mark'		(\\c3)
     `tone-mark'		        (\\c4)
     `symbol'			        (\\c5)
     `digit'			        (\\c6)
     `vowel-modifying-diacritical-mark'	(\\c7)
     `vowel-sign'			(\\c8)
     `semivowel-lower'			(\\c9)
     `not-at-end-of-line'		(\\c<)
     `not-at-beginning-of-line'		(\\c>)
     `alpha-numeric-two-byte'		(\\cA)
     `chinse-two-byte'			(\\cC)
     `greek-two-byte'			(\\cG)
     `japanese-hiragana-two-byte'	(\\cH)
     `indian-tow-byte'			(\\cI)
     `japanese-katakana-two-byte'	(\\cK)
     `korean-hangul-two-byte'		(\\cN)
     `cyrillic-two-byte'		(\\cY)
     `combining-diacritic'		(\\c^)
     `ascii'				(\\ca)
     `arabic'				(\\cb)
     `chinese'				(\\cc)
     `ethiopic'				(\\ce)
     `greek'				(\\cg)
     `korean'				(\\ch)
     `indian'				(\\ci)
     `japanese'				(\\cj)
     `japanese-katakana'		(\\ck)
     `latin'				(\\cl)
     `lao'				(\\co)
     `tibetan'				(\\cq)
     `japanese-roman'			(\\cr)
     `thai'				(\\ct)
     `vietnamese'			(\\cv)
     `hebrew'				(\\cw)
     `cyrillic'				(\\cy)
     `can-break'			(\\c|)

`(not (category CATEGORY))'
     matches a character that doesn't have category CATEGORY.

`(and SEXP1 SEXP2 ...)'
`(: SEXP1 SEXP2 ...)'
`(seq SEXP1 SEXP2 ...)'
`(sequence SEXP1 SEXP2 ...)'
     matches what SEXP1 matches, followed by what SEXP2 matches, etc.

`(submatch SEXP1 SEXP2 ...)'
`(group SEXP1 SEXP2 ...)'
     like `and', but makes the match accessible with `match-end',
     `match-beginning', and `match-string'.

`(group SEXP1 SEXP2 ...)'
     another name for `submatch'.

`(or SEXP1 SEXP2 ...)'
`(| SEXP1 SEXP2 ...)'
     matches anything that matches SEXP1 or SEXP2, etc.  If all
     args are strings, use `regexp-opt' to optimize the resulting
     regular expression.

`(minimal-match SEXP)'
     produce a non-greedy regexp for SEXP.  Normally, regexps matching
     zero or more occurrences of something are \"greedy\" in that they
     match as much as they can, as long as the overall regexp can
     still match.  A non-greedy regexp matches as little as possible.

`(maximal-match SEXP)'
     produce a greedy regexp for SEXP.  This is the default.

Below, `SEXP ...' represents a sequence of regexp forms, treated as if
enclosed in `(and ...)'.

`(zero-or-more SEXP ...)'
`(0+ SEXP ...)'
     matches zero or more occurrences of what SEXP ... matches.

`(* SEXP ...)'
     like `zero-or-more', but always produces a greedy regexp, independent
     of `rx-greedy-flag'.

`(*? SEXP ...)'
     like `zero-or-more', but always produces a non-greedy regexp,
     independent of `rx-greedy-flag'.

`(one-or-more SEXP ...)'
`(1+ SEXP ...)'
     matches one or more occurrences of SEXP ...

`(+ SEXP ...)'
     like `one-or-more', but always produces a greedy regexp.

`(+? SEXP ...)'
     like `one-or-more', but always produces a non-greedy regexp.

`(zero-or-one SEXP ...)'
`(optional SEXP ...)'
`(opt SEXP ...)'
     matches zero or one occurrences of A.

`(? SEXP ...)'
     like `zero-or-one', but always produces a greedy regexp.

`(?? SEXP ...)'
     like `zero-or-one', but always produces a non-greedy regexp.

`(repeat N SEXP)'
`(= N SEXP ...)'
     matches N occurrences.

`(>= N SEXP ...)'
     matches N or more occurrences.

`(repeat N M SEXP)'
`(** N M SEXP ...)'
     matches N to M occurrences.

`(backref N)'
    matches what was matched previously by submatch N.

`(backref N)'
     matches what was matched previously by submatch N.

`(backref N)'
    matches what was matched previously by submatch N.

`(eval FORM)'
     evaluate FORM and insert result.  If result is a string,
     `regexp-quote' it.

`(regexp REGEXP)'
     include REGEXP in string notation in the result."
  (cond ((null regexps)
	 (error "No regexp"))
	((cdr regexps)
	 (rx-to-string `(and ,@regexps) t))
	(t
	 (rx-to-string (car regexps) t))))

;; ;; sregex.el replacement

;; ;;;###autoload (provide 'sregex)
;; ;;;###autoload (autoload 'sregex "rx")
;; (defalias 'sregex 'rx-to-string)
;; ;;;###autoload (autoload 'sregexq "rx" nil nil 'macro)
;; (defalias 'sregexq 'rx)

(provide 'rx)

;; arch-tag: 12d01a63-0008-42bb-ab8c-1c7d63be370b
;;; rx.el ends here
