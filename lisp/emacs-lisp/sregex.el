;;; sregex.el --- symbolic regular expressions

;; Copyright (C) 1997, 1998 Free Software Foundation, Inc.

;; Author: Bob Glickstein <bobg+sregex@zanshin.com>
;; Maintainer: Bob Glickstein <bobg+sregex@zanshin.com>

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

;; This package allows you to write regular expressions using a
;; totally new, Lisp-like syntax.

;; A "symbolic regular expression" (sregex for short) is a Lisp form
;; that, when evaluated, produces the string form of the specified
;; regular expression.  Here's a simple example:

;;   (sregexq (or "Bob" "Robert"))  =>  "Bob\\|Robert"

;; As you can see, an sregex is specified by placing one or more
;; special clauses in a call to `sregexq'.  The clause in this case is
;; the `or' of two strings (not to be confused with the Lisp function
;; `or').  The list of allowable clauses appears below.

;; With sregex, it is never necessary to "escape" magic characters
;; that are meant to be taken literally; that happens automatically.
;; For example:

;;   (sregexq "M*A*S*H")  =>  "M\\*A\\*S\\*H"

;; It is also unnecessary to "group" parts of the expression together
;; to overcome operator precedence; that also happens automatically.
;; For example:

;;   (sregexq (opt (or "Bob" "Robert")))  =>  "\\(Bob\\|Robert\\)?"

;; It *is* possible to group parts of the expression in order to refer
;; to them with numbered backreferences:

;;   (sregexq (group (or "Go" "Run"))
;;            ", Spot, "
;;            (backref 1))             =>  "\\(Go\\|Run\\), Spot, \\1"

;; If `sregexq' needs to introduce its own grouping parentheses, it
;; will automatically renumber your backreferences:

;;   (sregexq (opt "resent-")
;;            (group (or "to" "cc" "bcc"))
;;            ": "
;;            (backref 1))  =>  "\\(resent-\\)?\\(to\\|cc\\|bcc\\): \\2"

;; `sregexq' is a macro.  Each time it is used, it constructs a simple
;; Lisp expression that then invokes a moderately complex engine to
;; interpret the sregex and render the string form.  Because of this,
;; I don't recommend sprinkling calls to `sregexq' throughout your
;; code, the way one normally does with string regexes (which are
;; cheap to evaluate).  Instead, it's wiser to precompute the regexes
;; you need wherever possible instead of repeatedly constructing the
;; same ones over and over.  Example:

;;    (let ((field-regex (sregexq (opt "resent-")
;;                                (or "to" "cc" "bcc"))))
;;      ...
;;      (while ...
;;        ...
;;        (re-search-forward field-regex ...)
;;        ...))

;; The arguments to `sregexq' are automatically quoted, but the
;; flipside of this is that it is not straightforward to include
;; computed (i.e., non-constant) values in `sregexq' expressions.  So
;; `sregex' is a function that is like `sregexq' but which does not
;; automatically quote its values.  Literal sregex clauses must be
;; explicitly quoted like so:

;;   (sregex '(or "Bob" "Robert"))  =>  "Bob\\|Robert"

;; but computed clauses can be included easily, allowing for the reuse
;; of common clauses:

;;  (let ((dotstar '(0+ any))
;;        (whitespace '(1+ (syntax ?-)))
;;        (digits '(1+ (char (?0 . ?9)))))
;;    (sregex 'bol dotstar ":" whitespace digits))  =>  "^.*:\\s-+[0-9]+"

;; This package also provides sregex-specific versions of the Emacs
;; functions `replace-match', `match-string',
;; `match-string-no-properties', `match-beginning', `match-end', and
;; `match-data'.  In each case, the sregex version's name begins with
;; `sregex-' and takes one additional optional parameter, an sregex
;; "info" object.  Each of these functions is concerned with numbered
;; submatches.  Since sregex may renumber submatches, alternate
;; versions of these functions are needed that know how to adjust the
;; supplied number.

;; The sregex info object for the most recently evaluated sregex can
;; be obtained with `sregex-info'; so if you precompute your sregexes
;; and you plan to use `replace-match' or one of the others with it,
;; you need to record the info object for later use:

;;   (let* ((regex (sregexq (opt "resent-")
;;                          (group (or "to" "cc" "bcc"))
;;                          ":"))
;;          (regex-info (sregex-info)))
;;     ...
;;     (if (re-search-forward regex ...)
;;         (let ((which (sregex-match-string 1 nil regex-info)))
;;           ...)))

;; In this example, `regex' is "\\(resent-\\)?\\(to\\|cc\\|bcc\\):",
;; so the call to (sregex-match-string 1 ...) is automatically turned
;; into a call to (match-string 2 ...).

;; If the sregex info argument to `sregex-replace-match',
;; `sregex-match-string', `sregex-match-string-no-properties',
;; `sregex-match-beginning', `sregex-match-end', or
;; `sregex-match-data' is omitted, the current value of (sregex-info)
;; is used.

;; You can do your own sregex submatch renumbering with
;; `sregex-backref-num'.

;; Finally, `sregex-save-match-data' is like `save-match-data' but
;; also saves and restores the information maintained by
;; `sregex-info'.

;; To use this package in a Lisp program, simply (require 'sregex).

;; Here are the clauses allowed in an `sregex' or `sregexq'
;; expression:

;; - a string
;;   This stands for the literal string.  If it contains
;;   metacharacters, they will be escaped in the resulting regex
;;   (using `regexp-quote').

;; - the symbol `any'
;;   This stands for ".", a regex matching any character except
;;   newline.

;; - the symbol `bol'
;;   Stands for "^", matching the empty string at the beginning of a line

;; - the symbol `eol'
;;   Stands for "$", matching the empty string at the end of a line

;; - (group CLAUSE ...)
;;   Groups the given CLAUSEs using "\\(" and "\\)".

;; - (sequence CLAUSE ...)

;;   Groups the given CLAUSEs; may or may not use "\\(" and "\\)".
;;   Clauses groups by `sequence' do not count for purposes of
;;   numbering backreferences.  Use `sequence' in situations like
;;   this:

;;     (sregexq (or "dog" "cat"
;;                  (sequence (opt "sea ") "monkey")))
;;                                  =>  "dog\\|cat\\|\\(sea \\)?monkey"

;;   where a single `or' alternate needs to contain multiple
;;   subclauses.

;; - (backref N)
;;   Matches the same string previously matched by the Nth "group" in
;;   the same sregex.  N is a positive integer.  In the resulting
;;   regex, N may be adjusted to account for automatically introduced
;;   groups.

;; - (or CLAUSE ...)
;;   Matches any one of the CLAUSEs by separating them with "\\|".

;; - (0+ CLAUSE ...)
;;   Concatenates the given CLAUSEs and matches zero or more
;;   occurrences by appending "*".

;; - (1+ CLAUSE ...)
;;   Concatenates the given CLAUSEs and matches one or more
;;   occurrences by appending "+".

;; - (opt CLAUSE ...)
;;   Concatenates the given CLAUSEs and matches zero or one occurrence
;;   by appending "?".

;; - (repeat MIN MAX CLAUSE ...)
;;   Concatenates the given CLAUSEs and constructs a regex matching at
;;   least MIN occurrences and at most MAX occurrences.  MIN must be a
;;   non-negative integer.  MAX must be a non-negative integer greater
;;   than or equal to MIN; or MAX can be nil to mean "infinity."

;; - (char CHAR-CLAUSE ...)
;;   Creates a "character class" matching one character from the given
;;   set.  See below for how to construct a CHAR-CLAUSE.

;; - (not-char CHAR-CLAUSE ...)
;;   Creates a "character class" matching any one character not in the
;;   given set.  See below for how to construct a CHAR-CLAUSE.

;; - the symbol `bot'
;;   Stands for "\\`", matching the empty string at the beginning of
;;   text (beginning of a string or of a buffer).

;; - the symbol `eot'
;;   Stands for "\\'", matching the empty string at the end of text.

;; - the symbol `point'
;;   Stands for "\\=", matching the empty string at point.

;; - the symbol `word-boundary'
;;   Stands for "\\b", matching the empty string at the beginning or
;;   end of a word.

;; - the symbol `not-word-boundary'
;;   Stands for "\\B", matching the empty string not at the beginning
;;   or end of a word.

;; - the symbol `bow'
;;   Stands for "\\<", matching the empty string at the beginning of a
;;   word.

;; - the symbol `eow'
;;   Stands for "\\>", matching the empty string at the end of a word.

;; - the symbol `wordchar'
;;   Stands for the regex "\\w", matching a word-constituent character
;;   (as determined by the current syntax table)

;; - the symbol `not-wordchar'
;;   Stands for the regex "\\W", matching a non-word-constituent
;;   character.

;; - (syntax CODE)
;;   Stands for the regex "\\sCODE", where CODE is a syntax table code
;;   (a single character).  Matches any character with the requested
;;   syntax.

;; - (not-syntax CODE)
;;   Stands for the regex "\\SCODE", where CODE is a syntax table code
;;   (a single character).  Matches any character without the
;;   requested syntax.

;; - (regex REGEX)
;;   This is a "trapdoor" for including ordinary regular expression
;;   strings in the result.  Some regular expressions are clearer when
;;   written the old way: "[a-z]" vs. (sregexq (char (?a . ?z))), for
;;   instance.  However, see the note under "Bugs," below.

;; Each CHAR-CLAUSE that is passed to (char ...) and (not-char ...)
;; has one of the following forms:

;; - a character
;;   Adds that character to the set.

;; - a string
;;   Adds all the characters in the string to the set.

;; - A pair (MIN . MAX)
;;   Where MIN and MAX are characters, adds the range of characters
;;   from MIN through MAX to the set.

;;; To do:

;; Make (sregexq (or "a" (sequence "b" "c"))) return "a\\|bc" instead
;; of "a\\|\\(bc\\)"

;; An earlier version of this package could optionally translate the
;; symbolic regex into other languages' syntaxes, e.g. Perl.  For
;; instance, with Perl syntax selected, (sregexq (or "ab" "cd")) would
;; yield "ab|cd" instead of "ab\\|cd".  It might be useful to restore
;; such a facility.

;;; Bugs:

;; The (regex REGEX) form can confuse the code that distinguishes
;; introduced groups from user-specified groups.  Try to avoid using
;; grouping within a `regex' form.  Failing that, try to avoid using
;; backrefs if you're using `regex'.

;;; Code:

(defsubst sregex--value-unitp  (val) (nth 0 val))
(defsubst sregex--value-groups (val) (nth 1 val))
(defsubst sregex--value-tree   (val) (nth 2 val))

(defun sregex--make-value (unitp groups tree)
  (list unitp groups tree))

(defvar sregex--current-sregex nil
  "Global state for `sregex-info'.")

(defun sregex-info ()
  "Return extra information about the latest call to `sregex'.
This extra information is needed in order to adjust user-requested
backreference numbers to numbers suitable for the generated regexp.
See e.g. `sregex-match-string' and `sregex-backref-num'."
  sregex--current-sregex)

; (require 'advice)
; (defadvice save-match-data (around sregex-save-match-data protect)
;   (let ((sregex--saved-sregex sregex--current-sregex))
;     (unwind-protect
;         ad-do-it
;       (setq sregex--current-sregex sregex--saved-sregex))))
(defmacro sregex-save-match-data (&rest forms)
  "Like `save-match-data', but also saves and restores `sregex-info' data."
  `(let ((sregex--saved-sregex sregex--current-sregex))
     (unwind-protect
         (save-match-data ,@forms)
       (setq sregex--current-sregex sregex--saved-sregex))))

(defun sregex-replace-match (replacement
                             &optional fixedcase literal string subexp sregex)
  "Like `replace-match', for a regexp made with `sregex'.
This takes one additional optional argument, the `sregex' info, which
can be obtained with `sregex-info'.  The SUBEXP argument is adjusted
to allow for \"introduced groups\".  If the extra argument is omitted
or nil, it defaults to the current value of (sregex-info)."
  (replace-match replacement fixedcase literal string
                 (and subexp
                      (sregex-backref-num subexp sregex))))

(defun sregex-match-string (count &optional in-string sregex)
  "Like `match-string', for a regexp made with `sregex'.
This takes one additional optional argument, the `sregex' info, which
can be obtained with `sregex-info'.  The COUNT argument is adjusted to
allow for \"introduced groups\".  If the extra argument is omitted or
nil, it defaults to the current value of (sregex-info)."
  (match-string (and count
                     (sregex-backref-num count sregex))
                in-string))

(defun sregex-match-string-no-properties (count &optional in-string sregex)
  "Like `match-string-no-properties', for a regexp made with `sregex'.
This takes one additional optional argument, the `sregex' info, which
can be obtained with `sregex-info'.  The COUNT argument is adjusted to
allow for \"introduced groups\".  If the extra argument is omitted or
nil, it defaults to the current value of (sregex-info)."
  (match-string-no-properties
   (and count
        (sregex-backref-num count sregex))
   in-string))

(defun sregex-match-beginning (count &optional sregex)
  "Like `match-beginning', for a regexp made with `sregex'.
This takes one additional optional argument, the `sregex' info, which
can be obtained with `sregex-info'.  The COUNT argument is adjusted to
allow for \"introduced groups\".  If the extra argument is omitted or
nil, it defaults to the current value of (sregex-info)."
  (match-beginning (sregex-backref-num count sregex)))

(defun sregex-match-end (count &optional sregex)
  "Like `match-end', for a regexp made with `sregex'.
This takes one additional optional argument, the `sregex' info, which
can be obtained with `sregex-info'.  The COUNT argument is adjusted to
allow for \"introduced groups\".  If the extra argument is omitted or
nil, it defaults to the current value of (sregex-info)."
  (match-end (sregex-backref-num count sregex)))

(defun sregex-match-data (&optional sregex)
  "Like `match-data', for a regexp made with `sregex'.
This takes one additional optional argument, the `sregex' info, which
can be obtained with `sregex-info'.  \"Introduced groups\" are removed
from the result.  If the extra argument is omitted or nil, it defaults
to the current value of (sregex-info)."
  (let* ((data (match-data))
         (groups (sregex--value-groups (or sregex
                                           sregex--current-sregex)))
         (result (list (car (cdr data))
                       (car data))))
    (setq data (cdr (cdr data)))
    (while data
      (if (car groups)
          (setq result (append (list (car (cdr data))
                                     (car data))
                               result)))
      (setq groups (cdr groups)
            data (cdr (cdr data))))
    (reverse result)))

(defun sregex--render-tree (tree sregex)
  (let ((key (car tree)))
    (cond ((eq key 'str)
           (cdr tree))
          ((eq key 'or)
           (mapconcat '(lambda (x)
                         (sregex--render-tree x sregex))
                      (cdr tree)
                      "\\|"))
          ((eq key 'sequence)
           (apply 'concat
                  (mapcar '(lambda (x)
                             (sregex--render-tree x sregex))
                          (cdr tree))))
          ((eq key 'group)
           (concat "\\("
                   (sregex--render-tree (cdr tree) sregex)
                   "\\)"))
          ((eq key 'opt)
           (concat (sregex--render-tree (cdr tree) sregex)
                   "?"))
          ((eq key '0+)
           (concat (sregex--render-tree (cdr tree) sregex)
                   "*"))
          ((eq key '1+)
           (concat (sregex--render-tree (cdr tree) sregex)
                   "+"))
          ((eq key 'backref)
           (let ((num (sregex-backref-num (cdr tree) sregex)))
             (if (> num 9)
                 (error "sregex: backref number %d too high after adjustment"
                        num)
               (concat "\\" (int-to-string num)))))
          (t (error "sregex internal error: unknown tree type %S"
                    key)))))

(defun sregex (&rest exps)
  "Symbolic regular expression interpreter.
This is exactly like `sregexq' (q.v.) except that it evaluates all its
arguments, so literal sregex clauses must be quoted.  For example:

  (sregex '(or \"Bob\" \"Robert\"))  =>  \"Bob\\\\|Robert\"

An argument-evaluating sregex interpreter lets you reuse sregex
subexpressions:

  (let ((dotstar '(0+ any))
        (whitespace '(1+ (syntax ?-)))
        (digits '(1+ (char (?0 . ?9)))))
    (sregex 'bol dotstar \":\" whitespace digits))  =>  \"^.*:\\\\s-+[0-9]+\""
  (progn
    (setq sregex--current-sregex (sregex--sequence exps nil))
    (sregex--render-tree (sregex--value-tree sregex--current-sregex)
                         sregex--current-sregex)))

(defmacro sregexq (&rest exps)
  "Symbolic regular expression interpreter.
This macro allows you to specify a regular expression (regexp) in
symbolic form, and converts it into the string form required by Emacs's
regex functions such as `re-search-forward' and `looking-at'.  Here is
a simple example:

  (sregexq (or \"Bob\" \"Robert\"))  =>  \"Bob\\\\|Robert\"

As you can see, an sregex is specified by placing one or more special
clauses in a call to `sregexq'.  The clause in this case is the `or'
of two strings (not to be confused with the Lisp function `or').  The
list of allowable clauses appears below.

With `sregex', it is never necessary to \"escape\" magic characters
that are meant to be taken literally; that happens automatically.
For example:

  (sregexq \"M*A*S*H\")  =>  \"M\\\\*A\\\\*S\\\\*H\"

It is also unnecessary to \"group\" parts of the expression together
to overcome operator precedence; that also happens automatically.
For example:

  (sregexq (opt (or \"Bob\" \"Robert\")))  =>  \"\\\\(Bob\\\\|Robert\\\\)?\"

It *is* possible to group parts of the expression in order to refer
to them with numbered backreferences:

  (sregexq (group (or \"Go\" \"Run\"))
           \", Spot, \"
           (backref 1))             =>  \"\\\\(Go\\\\|Run\\\\), Spot, \\\\1\"

If `sregexq' needs to introduce its own grouping parentheses, it will
automatically renumber your backreferences:

  (sregexq (opt \"resent-\")
           (group (or \"to\" \"cc\" \"bcc\"))
           \": \"
           (backref 1))  =>  \"\\\\(resent-\\\\)?\\\\(to\\\\|cc\\\\|bcc\\\\): \\\\2\"

`sregexq' is a macro.  Each time it is used, it constructs a simple
Lisp expression that then invokes a moderately complex engine to
interpret the sregex and render the string form.  Because of this, I
don't recommend sprinkling calls to `sregexq' throughout your code,
the way one normally does with string regexes (which are cheap to
evaluate).  Instead, it's wiser to precompute the regexes you need
wherever possible instead of repeatedly constructing the same ones
over and over.  Example:

   (let ((field-regex (sregexq (opt \"resent-\")
                               (or \"to\" \"cc\" \"bcc\"))))
     ...
     (while ...
       ...
       (re-search-forward field-regex ...)
       ...))

The arguments to `sregexq' are automatically quoted, but the
flipside of this is that it is not straightforward to include
computed (i.e., non-constant) values in `sregexq' expressions.  So
`sregex' is a function that is like `sregexq' but which does not
automatically quote its values.  Literal sregex clauses must be
explicitly quoted like so:

  (sregex '(or \"Bob\" \"Robert\"))  =>  \"Bob\\\\|Robert\"

but computed clauses can be included easily, allowing for the reuse
of common clauses:

  (let ((dotstar '(0+ any))
        (whitespace '(1+ (syntax ?-)))
        (digits '(1+ (char (?0 . ?9)))))
    (sregex 'bol dotstar \":\" whitespace digits))  =>  \"^.*:\\\\s-+[0-9]+\"

Here are the clauses allowed in an `sregex' or `sregexq' expression:

- a string
  This stands for the literal string.  If it contains
  metacharacters, they will be escaped in the resulting regex
  (using `regexp-quote').

- the symbol `any'
  This stands for \".\", a regex matching any character except
  newline.

- the symbol `bol'
  Stands for \"^\", matching the empty string at the beginning of a line

- the symbol `eol'
  Stands for \"$\", matching the empty string at the end of a line

- (group CLAUSE ...)
  Groups the given CLAUSEs using \"\\\\(\" and \"\\\\)\".

- (sequence CLAUSE ...)

  Groups the given CLAUSEs; may or may not use \"\\\\(\" and \"\\\\)\".
  Clauses groups by `sequence' do not count for purposes of
  numbering backreferences.  Use `sequence' in situations like
  this:

    (sregexq (or \"dog\" \"cat\"
                 (sequence (opt \"sea \") \"monkey\")))
                                 =>  \"dog\\\\|cat\\\\|\\\\(sea \\\\)?monkey\"

  where a single `or' alternate needs to contain multiple
  subclauses.

- (backref N)
  Matches the same string previously matched by the Nth \"group\" in
  the same sregex.  N is a positive integer.  In the resulting
  regex, N may be adjusted to account for automatically introduced
  groups.

- (or CLAUSE ...)
  Matches any one of the CLAUSEs by separating them with \"\\\\|\".

- (0+ CLAUSE ...)
  Concatenates the given CLAUSEs and matches zero or more
  occurrences by appending \"*\".

- (1+ CLAUSE ...)
  Concatenates the given CLAUSEs and matches one or more
  occurrences by appending \"+\".

- (opt CLAUSE ...)
  Concatenates the given CLAUSEs and matches zero or one occurrence
  by appending \"?\".

- (repeat MIN MAX CLAUSE ...)
  Concatenates the given CLAUSEs and constructs a regex matching at
  least MIN occurrences and at most MAX occurrences.  MIN must be a
  non-negative integer.  MAX must be a non-negative integer greater
  than or equal to MIN; or MAX can be nil to mean \"infinity.\"

- (char CHAR-CLAUSE ...)
  Creates a \"character class\" matching one character from the given
  set.  See below for how to construct a CHAR-CLAUSE.

- (not-char CHAR-CLAUSE ...)
  Creates a \"character class\" matching any one character not in the
  given set.  See below for how to construct a CHAR-CLAUSE.

- the symbol `bot'
  Stands for \"\\\\`\", matching the empty string at the beginning of
  text (beginning of a string or of a buffer).

- the symbol `eot'
  Stands for \"\\\\'\", matching the empty string at the end of text.

- the symbol `point'
  Stands for \"\\\\=\", matching the empty string at point.

- the symbol `word-boundary'
  Stands for \"\\\\b\", matching the empty string at the beginning or
  end of a word.

- the symbol `not-word-boundary'
  Stands for \"\\\\B\", matching the empty string not at the beginning
  or end of a word.

- the symbol `bow'
  Stands for \"\\\\\\=<\", matching the empty string at the beginning of a
  word.

- the symbol `eow'
  Stands for \"\\\\\\=>\", matching the empty string at the end of a word.

- the symbol `wordchar'
  Stands for the regex \"\\\\w\", matching a word-constituent character
  (as determined by the current syntax table)

- the symbol `not-wordchar'
  Stands for the regex \"\\\\W\", matching a non-word-constituent
  character.

- (syntax CODE)
  Stands for the regex \"\\\\sCODE\", where CODE is a syntax table code
  (a single character).  Matches any character with the requested
  syntax.

- (not-syntax CODE)
  Stands for the regex \"\\\\SCODE\", where CODE is a syntax table code
  (a single character).  Matches any character without the
  requested syntax.

- (regex REGEX)
  This is a \"trapdoor\" for including ordinary regular expression
  strings in the result.  Some regular expressions are clearer when
  written the old way: \"[a-z]\" vs. (sregexq (char (?a . ?z))), for
  instance.  However, using this can confuse the code that
  distinguishes introduced groups from user-specified groups.  Avoid
  using grouping within a `regex' form.  Failing that, avoid using
  backrefs if you're using `regex'.

Each CHAR-CLAUSE that is passed to (char ...) and (not-char ...)
has one of the following forms:

- a character
  Adds that character to the set.

- a string
  Adds all the characters in the string to the set.

- A pair (MIN . MAX)
  Where MIN and MAX are characters, adds the range of characters
  from MIN through MAX to the set."
  `(apply 'sregex ',exps))

(defun sregex--engine (exp combine)
  (let* ((val (cond ((stringp exp)
                     (sregex--make-value (or (not (eq combine 'suffix))
                                             (= (length exp) 1))
                                         nil
                                         (cons 'str
                                               (regexp-quote exp))))
                    ((symbolp exp)
                     (funcall (intern (concat "sregex--"
                                              (symbol-name exp)))
                              combine))
                    ((consp exp)
                     (funcall (intern (concat "sregex--"
                                              (symbol-name (car exp))))
                              (cdr exp)
                              combine))
                    (t (error "Invalid expression: %s" exp))))
         (unitp (sregex--value-unitp val))
         (groups (sregex--value-groups val))
         (tree (sregex--value-tree val)))
    (if (and combine (not unitp))
        (sregex--make-value t
                            (cons nil groups)
                            (cons 'group tree))
      (sregex--make-value unitp groups tree))))

(defun sregex--sequence (exps combine)
  (if (= (length exps) 1)
      (sregex--engine (car exps) combine)
    (let ((groups nil)
          (trees nil))                  ;grows in reverse
      (while exps
        (let ((val (sregex--engine (car exps) 'concat)))
          (setq groups (append groups
                               (sregex--value-groups val))
                trees (cons (sregex--value-tree val) trees)
                exps (cdr exps))))
      (setq trees (nreverse trees))
      (if (eq combine 'suffix)
          (sregex--make-value t
                              (cons nil groups)
                              (cons 'group
                                    (cons 'sequence trees)))
        (sregex--make-value (not (eq combine 'suffix))
                            groups
                            (cons 'sequence trees))))))

(defun sregex--group (exps combine)
  (let ((val (sregex--sequence exps nil)))
    (sregex--make-value t
                        (cons t (sregex--value-groups val))
                        (cons 'group (sregex--value-tree val)))))

(defun sregex-backref-num (n &optional sregex)
  "Adjust backreference number N according to SREGEX.
When `sregex' introduces parenthesized groups that the user didn't ask
for, the numbering of the groups that the user *did* ask for gets all
out of whack.  This function accounts for introduced groups.  Example:

  (sregexq (opt \"ab\")
           (group (or \"c\" \"d\")))  =>  \"\\\\(ab\\\\)?\\\\(c\\\\|d\\\\)\"
  (setq info (sregex-info))
  (sregex-backref-num 1 info)  =>  2

The SREGEX parameter is optional and defaults to the current value of
`sregex-info'."
  (let ((groups (sregex--value-groups (or sregex
                                          sregex--current-sregex)))
        (result 0))
    (while (and groups (> n 0))
      (if (car groups)
          (setq n (1- n)))
      (setq result (1+ result)
            groups (cdr groups)))
    result))

(defun sregex--backref (exps combine)
  (sregex--make-value t nil (cons 'backref (car exps))))

(defun sregex--any (combine)
  (sregex--make-value t nil '(str . ".")))

(defun sregex--opt (exps combine)
  (let ((val (sregex--sequence exps 'suffix)))
    (sregex--make-value t
                        (sregex--value-groups val)
                        (cons 'opt (sregex--value-tree val)))))

(defun sregex--0+ (exps combine)
  (let ((val (sregex--sequence exps 'suffix)))
    (sregex--make-value t
                        (sregex--value-groups val)
                        (cons '0+ (sregex--value-tree val)))))
(defun sregex--1+ (exps combine)
  (let ((val (sregex--sequence exps 'suffix)))
    (sregex--make-value t
                        (sregex--value-groups val)
                        (cons '1+ (sregex--value-tree val)))))

(defun sregex--repeat (exps combine)
  (let ((min (or (car exps) 0))
        (max (car (cdr exps))))
    (setq exps (cdr (cdr exps)))
    (cond ((zerop min)
           (cond ((equal max 0)         ;degenerate
                  (sregex--make-value t nil nil))
                 ((equal max 1)
                  (sregex--opt exps combine))
                 ((not max)
                  (sregex--0+ exps combine))
                 (t (sregex--sequence (make-list max
                                               (cons 'opt exps))
                                    combine))))
          ((= min 1)
           (cond ((equal max 1)
                  (sregex--sequence exps combine))
                 ((not max)
                  (sregex--1+ exps combine))
                 (t (sregex--sequence (append exps
                                             (make-list (1- max)
                                                        (cons 'opt exps)))
                                      combine))))
          (t (sregex--sequence (append exps
                                     (list (append (list 'repeat
                                                         (1- min)
                                                         (and max
                                                              (1- max)))
                                                   exps)))
                               combine)))))

(defun sregex--or (exps combine)
  (if (= (length exps) 1)
      (sregex--engine (car exps) combine)
    (let ((groups nil)
          (trees nil))
      (while exps
        (let ((val (sregex--engine (car exps) 'or)))
          (setq groups (append groups
                               (sregex--value-groups val))
                trees (cons (sregex--value-tree val) trees)
                exps (cdr exps))))
      (sregex--make-value (eq combine 'or)
                          groups
                          (cons 'or (nreverse trees))))))

(defmacro sregex--char-range-aux ()
  '(if start
       (let (startc endc)
         (if (and (<= 32 start)
                  (<= start 127))
             (setq startc (char-to-string start)
                   endc (char-to-string end))
           (setq startc (format "\\%03o" start)
                 endc (format "\\%03o" end)))
         (if (> end start)
             (if (> end (+ start 1))
                 (setq class (concat class startc "-" endc))
               (setq class (concat class startc endc)))
           (setq class (concat class startc))))))

(defmacro sregex--char-range (rstart rend)
  `(let ((i ,rstart)
         start end)
     (while (<= i ,rend)
       (if (aref chars i)
           (progn
             (if start
                 (setq end i)
               (setq start i
                     end i))
             (aset chars i nil))
         (sregex--char-range-aux)
         (setq start nil
               end nil))
       (setq i (1+ i)))
     (sregex--char-range-aux)))

(defun sregex--char-aux (complement args)
  (let ((chars (make-vector 256 nil)))
    (while args
      (let ((arg (car args)))
        (cond ((integerp arg)
               (aset chars arg t))
              ((stringp arg)
               (mapcar (function
                        (lambda (c)
                          (aset chars c t)))
                       arg))
              ((consp arg)
               (let ((start (car arg))
                     (end (cdr arg)))
                 (if (> start end)
                     (let ((tmp start))
                       (setq start end
                             end tmp)))
                 ;; now start <= end
                 (let ((i start))
                   (while (<= i end)
                     (aset chars i t)
                     (setq i (1+ i))))))))
      (setq args (cdr args)))
    ;; now chars is a map of the characters in the class
    (let ((class "")
          (caret (aref chars ?^)))
      (aset chars ?^ nil)
      (if (aref chars ?\])
          (progn
            (setq class (concat class "]"))
            (aset chars ?\] nil)))
      (if (aref chars ?-)
          (progn
            (setq class (concat class "-"))
            (aset chars ?- nil)))
      (if (aref chars ?\\)
          (progn
            (setq class (concat class "\\\\"))
            (aset chars ?\\ nil)))

      (sregex--char-range ?A ?Z)
      (sregex--char-range ?a ?z)
      (sregex--char-range ?0 ?9)

      (let ((i 32))
        (while (< i 128)
          (if (aref chars i)
              (progn
                (setq class (concat class (char-to-string i)))
                (aset chars i nil)))
          (setq i (1+ i))))

      (sregex--char-range 0 31)
      (sregex--char-range 128 255)
        
      (let ((i 0))
        (while (< i 256)
          (if (aref chars i)
              (setq class (concat class (format "\\%03o" i))))
          (setq i (1+ i))))

      (if caret
          (setq class (concat class "^")))
      (concat "[" (if complement "^") class "]"))))

(defun sregex--char (exps combine)
  (sregex--make-value t nil (cons 'str (sregex--char-aux nil exps))))
(defun sregex--not-char (exps combine)
  (sregex--make-value t nil (cons 'str (sregex--char-aux t exps))))

(defun sregex--bol (combine)
  (sregex--make-value t nil '(str . "^")))
(defun sregex--eol (combine)
  (sregex--make-value t nil '(str . "$")))

(defun sregex--wordchar (combine)
  (sregex--make-value t nil '(str . "\\w")))
(defun sregex--not-wordchar (combine)
  (sregex--make-value t nil '(str . "\\W")))

(defun sregex--syntax (exps combine)
  (sregex--make-value t nil (cons 'str (format "\\s%c" (car exps)))))
(defun sregex--not-syntax (exps combine)
  (sregex--make-value t nil (cons 'str (format "\\S%c" (car exps)))))

(defun sregex--bot (combine)
  (sregex--make-value t nil (cons 'str "\\`")))
(defun sregex--eot (combine)
  (sregex--make-value t nil (cons 'str "\\'")))

(defun sregex--point (combine)
  (sregex--make-value t nil '(str . "\\=")))

(defun sregex--word-boundary (combine)
  (sregex--make-value t nil '(str . "\\b")))
(defun sregex--not-word-boundary (combine)
  (sregex--make-value t nil '(str . "\\B")))

(defun sregex--bow (combine)
  (sregex--make-value t nil '(str . "\\<")))
(defun sregex--eow (combine)
  (sregex--make-value t nil '(str . "\\>")))


;; trapdoor - usage discouraged
(defun sregex--regex (exps combine)
  (sregex--make-value nil nil (car exps)))

(provide 'sregex)

;;; sregex.el ends here

