;;; rx.el --- sexp notation for regular expressions

;; Copyright (C) 2001 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is another implementation of sexp-form regular expressions.
;; It was unfortunately written without being aware of the Sregex
;; package coming with Emacs, but as things stand, Rx completely
;; covers all regexp features, which Sregex doesn't, doesn't suffer
;; from the bugs mentioned in the commentary section of Sregex, and
;; uses a nicer syntax (IMHO, of course :-).

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
    (or			. (rx-or 1 nil))
    (not-newline	. ".")
    (anything		. ".\\|\n")
    (any		. (rx-any 1 1 rx-check-any))
    (in			. any)
    (not		. (rx-not 1 1 rx-check-not))
    (repeat		. (rx-repeat 2 3))
    (submatch		. (rx-submatch 1 nil))
    (group		. submatch)
    (zero-or-more	. (rx-kleene 1 1))
    (one-or-more	. (rx-kleene 1 1))
    (zero-or-one	. (rx-kleene 1 1))
    (\?			. zero-or-one)
    (\??		. zero-or-one)
    (*			. zero-or-more)
    (*?			. zero-or-more)
    (0+			. zero-or-more)
    (+			. one-or-more)
    (+?			. one-or-more)
    (1+			. one-or-more)
    (optional		. zero-or-one)
    (minimal-match	. (rx-greedy 1 1))
    (maximal-match	. (rx-greedy 1 1))
    (backref		. (rx-backref 1 1 rx-check-backref))
    (line-start		. "^")
    (line-end		. "$")
    (string-start	. "\\`")
    (string-end		. "\\'")
    (buffer-start	. "\\`")
    (buffer-end		. "\\'")
    (point		. "\\=")
    (word-start		. "\\<")
    (word-end		. "\\>")
    (word-boundary	. "\\b")
    (syntax		. (rx-syntax 1 1))
    (category		. (rx-category 1 1 rx-check-category))
    (eval		. (rx-eval 1 1))
    (regexp		. (rx-regexp 1 1 stringp))
    (digit		. "[[:digit:]]")
    (control		. "[[:cntrl:]]")
    (hex-digit		. "[[:xdigit:]]")
    (blank		. "[[:blank:]]")
    (graphic		. "[[:graph:]]")
    (printing		. "[[:print:]]")
    (alphanumeric	. "[[:alnum:]]")
    (letter		. "[[:alpha:]]")
    (ascii		. "[[:ascii:]]")
    (nonascii		. "[[:nonascii:]]")
    (lower		. "[[:lower:]]")
    (punctuation	. "[[:punct:]]")
    (space		. "[[:space:]]")
    (upper		. "[[:upper:]]")
    (word		. "[[:word:]]"))
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


(defun rx-quote-for-set (string)
  "Transform STRING for use in a character set.
If STRING contains a `]', move it to the front.
If STRING starts with a '^', move it to the end."
  (when (string-match "\\`\\(\\(?:.\\|\n\\)+\\)\\]\\(\\(?:.\\|\n\\)\\)*\\'"
		      string)
    (setq string (concat "]" (match-string 1 string)
			 (match-string 2 string))))
  (when (string-match "\\`^\\(\\(?:.\\|\n\\)+\\)\\'" string)
    (setq string (concat (substring string 1) "^")))
  string)


(defun rx-check-any (arg)
   "Check arg ARG for Rx `any'."
   (cond ((integerp arg) t)
	 ((and (stringp arg) (zerop (length arg)))
	  (error "String arg for rx `any' must not be empty"))
	 ((stringp arg) t)
	 (t
	  (error "rx `any' requires string or character arg"))))


(defun rx-any (form)
  "Parse and produce code from FORM, which is `(any STRING)'.
STRING is optional.  If it is omitted, build a regexp that
matches anything."
  (rx-check form)
  (let ((arg (cadr form)))
    (cond ((integerp arg)
	   (char-to-string arg))
	  ((= (length arg) 1)
	   arg)
	  (t
	   (concat "[" (rx-quote-for-set (cadr form)) "]")))))


(defun rx-check-not (arg)
  "Check arg ARG for Rx `not'."
  (unless (or (memq form
		    '(digit control hex-digit blank graphic printing
			    alphanumeric letter ascii nonascii lower
			    punctuation space upper word))
	      (and (consp form)
		   (memq (car form) '(not any in syntax category:))))
    (error "rx `not' syntax error: %s" form))
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
	  ((string-match "\\`\\[" result)
	   (concat "[^" (substring result 1)))
	  ((string-match "\\`\\\\s." result)
	   (concat "\\S" (substring result 2)))
	  ((string-match "\\`\\\\S." result)
	   (concat "\\s" (substring result 2)))
	  ((string-match "\\`\\\\c." result)
	   (concat "\\C" (substring result 2)))
	  ((string-match "\\`\\\\C." result)
	   (concat "\\c" (substring result 2)))
	  ((string-match "\\`\\\\B" result)
	   (concat "\\b" (substring result 2)))
	  ((string-match "\\`\\\\b" result)
	   (concat "\\B" (substring result 2)))
	  (t
	   (concat "[^" result "]")))))


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
  (let ((syntax (assq (cadr form) rx-syntax)))
    (unless syntax
      (error "Unknown rx syntax `%s'" (cadr form)))
    (format "\\s%c" (cdr syntax))))


(defun rx-check-category (form)
  "Check the argument FORM of a `(category FORM)'."
  (unless (or (integerp form)
	      (cdr (assq form rx-categories)))
    (error "Unknown category `%s'" form))
  t)


(defun rx-category (form)
  "Parse and produce code from FORM, which is `(category SYMBOL ...)'."
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
(defmacro rx (regexp)
  "Translate a regular expression REGEXP in sexp form to a regexp string.
See also `rx-to-string' for how to do such a translation at run-time.

The following are valid subforms of regular expressions in sexp
notation.

STRING
     matches string STRING literally.

CHAR
     matches character CHAR literally.

`not-newline'
     matches any character except a newline.
			.
`anything'
     matches any character

`(any SET)'
     matches any character in SET.  SET may be a character or string.
     Ranges of characters can be specified as `A-Z' in strings.

'(in SET)'
     like `any'.

`(not (any SET))'
     matches any character not in SET

`line-start'
     matches the empty string, but only at the beginning of a line
     in the text being matched

`line-end'
     is similar to `line-start' but matches only at the end of a line

`string-start'
     matches the empty string, but only at the beginning of the
     string being matched against.

`string-end'
     matches the empty string, but only at the end of the
     string being matched against.

`buffer-start'
     matches the empty string, but only at the beginning of the
     buffer being matched against.

`buffer-end'
     matches the empty string, but only at the end of the
     buffer being matched against.

`point'
     matches the empty string, but only at point.

`word-start'
     matches the empty string, but only at the beginning or end of a
     word.

`word-end'
     matches the empty string, but only at the end of a word.

`word-boundary'
     matches the empty string, but only at the beginning or end of a
     word.

`(not word-boundary)'
     matches the empty string, but not at the beginning or end of a
     word.

`digit'
     matches 0 through 9.

`control'
     matches ASCII control characters.

`hex-digit'
     matches 0 through 9, a through f and A through F.

`blank'
     matches space and tab only.

`graphic'
     matches graphic characters--everything except ASCII control chars,
     space, and DEL.

`printing'
     matches printing characters--everything except ASCII control chars
     and DEL.

`alphanumeric'
     matches letters and digits.  (But at present, for multibyte characters,
     it matches anything that has word syntax.)

`letter'
     matches letters.  (But at present, for multibyte characters,
     it matches anything that has word syntax.)

`ascii'
     matches ASCII (unibyte) characters.

`nonascii'
     matches non-ASCII (multibyte) characters.

`lower'
     matches anything lower-case.

`upper'
     matches anything upper-case.

`punctuation'
     matches punctuation.  (But at present, for multibyte characters,
     it matches anything that has non-word syntax.)

`space'
     matches anything that has whitespace syntax.

`word'
     matches anything that has word syntax.

`(syntax SYNTAX)'
     matches a character with syntax SYNTAX.  SYNTAX must be one
     of the following symbols.

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
     matches a character that has not syntax SYNTAX.

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
     `combining-diacritic'              (\\c^)
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
     matches a character that has not category CATEGORY.

`(and SEXP1 SEXP2 ...)'
     matches what SEXP1 matches, followed by what SEXP2 matches, etc.

`(submatch SEXP1 SEXP2 ...)'
     like `and', but makes the match accessible with `match-end',
     `match-beginning', and `match-string'.

`(group SEXP1 SEXP2 ...)'
     another name for `submatch'.

`(or SEXP1 SEXP2 ...)'
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

`(zero-or-more SEXP)'
     matches zero or more occurrences of what SEXP matches.

`(0+ SEXP)'
     like `zero-or-more'.

`(* SEXP)'
     like `zero-or-more', but always produces a greedy regexp.

`(*? SEXP)'
     like `zero-or-more', but always produces a non-greedy regexp.

`(one-or-more SEXP)'
     matches one or more occurrences of A.

`(1+ SEXP)'
     like `one-or-more'.

`(+ SEXP)'
     like `one-or-more', but always produces a greedy regexp.

`(+? SEXP)'
     like `one-or-more', but always produces a non-greedy regexp.

`(zero-or-one SEXP)'
     matches zero or one occurrences of A.

`(optional SEXP)'
     like `zero-or-one'.

`(? SEXP)'
     like `zero-or-one', but always produces a greedy regexp.

`(?? SEXP)'
     like `zero-or-one', but always produces a non-greedy regexp.

`(repeat N SEXP)'
     matches N occurrences of what SEXP matches.

`(repeat N M SEXP)'
     matches N to M occurrences of what SEXP matches.

`(eval FORM)'
      evaluate FORM and insert result.  If result is a string,
      `regexp-quote' it.

`(regexp REGEXP)'
      include REGEXP in string notation in the result."

  `(rx-to-string ',regexp))


(provide 'rx)

;;; arch-tag: 12d01a63-0008-42bb-ab8c-1c7d63be370b
;;; rx.el ends here
