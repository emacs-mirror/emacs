;;; esh-arg.el --- argument processing  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; Parsing of arguments can be extended by adding functions to the
;; hook `eshell-parse-argument-hook'.  For a good example of this, see
;; `eshell-parse-drive-letter', defined in eshell-dirs.el.

;;; Code:

(require 'esh-util)
(require 'esh-module)

(require 'pcomplete)

(eval-when-compile
  (require 'cl-lib))

(declare-function eshell-term-as-value "esh-cmd" (term))

(defgroup eshell-arg nil
  "Argument parsing involves transforming the arguments passed on the
command line into equivalent Lisp forms that, when evaluated, will
yield the values intended."
  :tag "Argument parsing"
  :group 'eshell)

;;; Internal Variables:

(defvar eshell-current-argument nil)
(defvar eshell-current-modifiers nil)
(defvar eshell-arg-listified nil)
(defvar eshell-nested-argument nil)
(defvar eshell-current-quoted nil)
(defvar eshell-current-argument-plain nil
  "If non-nil, the current argument is \"plain\", and not part of a command.")

;;; User Variables:

(defcustom eshell-arg-load-hook nil
  "A hook that gets run when `eshell-arg' is loaded."
  :version "24.1"		       ; removed eshell-arg-initialize
  :type 'hook
  :group 'eshell-arg)

(defcustom eshell-delimiter-argument-list '(?\; ?& ?\| ?\> ?\s ?\t ?\n)
  "List of characters to recognize as argument separators."
  :type '(repeat character)
  :group 'eshell-arg)

(defcustom eshell-special-chars-inside-quoting '(?\\ ?\")
  "Characters which are still special inside double quotes."
  :type '(repeat character)
  :group 'eshell-arg)

(defcustom eshell-special-chars-outside-quoting
  (append eshell-delimiter-argument-list '(?# ?! ?\\ ?\" ?\'))
  "Characters that require escaping outside of double quotes.
Without escaping them, they will introduce a change in the argument."
  :type '(repeat character)
  :group 'eshell-arg)

(defsubst eshell-arg-delimiter (&optional pos)
  "Return non-nil if POS is an argument delimiter.
If POS is nil, the location of point is checked."
  (let ((pos (or pos (point))))
    (or (= pos (point-max))
	(memq (char-after pos) eshell-delimiter-argument-list))))

(defcustom eshell-parse-argument-hook
  '(;; A term such as #<buffer NAME>, or #<process NAME> is a buffer
    ;; or process reference.
    eshell-parse-special-reference
    ;; Numbers convert to numbers if they stand alone.
    eshell-parse-number
    ;; Integers convert to numbers if they stand alone or are part of a
    ;; range expression.
    eshell-parse-integer
    ;; Range tokens go between integers and denote a half-open range.
    eshell-parse-range-token
    ;; Parse any non-special characters, based on the current context.
    eshell-parse-non-special
    ;; Whitespace is an argument delimiter.
    eshell-parse-whitespace
    ;; ... so is a comment.
    eshell-parse-comment
    ;; Parse backslash and the character after.
    eshell-parse-backslash
    ;; Text beginning with ' is a literally quoted.
    eshell-parse-literal-quote
    ;; Text beginning with " is interpolably quoted.
    eshell-parse-double-quote
    ;; Delimiters that separate individual commands.
    eshell-parse-delimiter)
  "Define how to process Eshell command line arguments.
When each function on this hook is called, point will be at the
current position within the argument list.  The function should either
return nil, meaning that it did no argument parsing, or it should
return the result of the parse as a sexp.  If the function did do
argument parsing, but the result was nothing at all, it should return
`eshell-empty-token'.  The function is also responsible for moving the
point forward to reflect the amount of input text that was parsed.

If the hook determines that it has reached the end of an argument, it
should call `eshell-finish-arg' to complete processing of the current
argument and proceed to the next.

If no function handles the current character at point, it will be
treated as a literal character."
  :type 'hook
  :group 'eshell-arg)

(defvar eshell-special-ref-alist
  '(("buffer"
     (creation-function   eshell-get-buffer)
     (insertion-function  eshell-insert-buffer-name)
     (completion-function eshell-complete-buffer-ref))
    ("marker"
     (creation-function   eshell-get-marker)
     (insertion-function  eshell-insert-marker)
     (completion-function eshell-complete-marker-ref)))
  "Alist of special reference types for Eshell.
Each entry is a list of the form (TYPE (KEY VALUE)...).  TYPE is
the name of the special reference type, and each KEY/VALUE pair
represents a parameter for the type.  Eshell defines the
following KEYs:

* `creation-function'
  A function taking any number of arguments that returns the Lisp
  object for this special ref type.

* `insertion-function'
  An interactive function that returns the special reference in
  string form.  This string should look like \"#<TYPE ARG...>\";
  Eshell will pass the ARGs to `creation-function'.

* `completion-function'
  A function using Pcomplete to perform completion on any
  arguments necessary for creating this special reference type.")

(defcustom eshell-special-ref-default "buffer"
  "The default type for special references when the type keyword is omitted.
This should be a key in `eshell-special-ref-alist' (which see).
Eshell will expand special refs like \"#<ARG...>\" into
\"#<`eshell-special-ref-default' ARG...>\"."
  :version "30.1"
  :type 'string
  :group 'eshell-arg)

(defvar-keymap eshell-arg-mode-map
  "C-c M-b" #'eshell-insert-buffer-name)

;;; Functions:

(define-minor-mode eshell-arg-mode
  "Minor mode for the arg eshell module.

\\{eshell-arg-mode-map}"
  :keymap eshell-arg-mode-map)

(defun eshell-arg-initialize ()     ;Called from `eshell-mode' via intern-soft!
  "Initialize the argument parsing code."
  (eshell-arg-mode)
  (when (eshell-using-module 'eshell-cmpl)
    (add-hook 'pcomplete-try-first-hook
              #'eshell-complete-special-reference nil t)))

(defvar eshell--non-special-inside-quote-regexp nil)
(defsubst eshell--non-special-inside-quote-regexp ()
  (or eshell--non-special-inside-quote-regexp
      (setq-local eshell--non-special-inside-quote-regexp
                  (rx-to-string
                   `(+ (not (any ,@eshell-special-chars-inside-quoting))) t))))

(defvar eshell--non-special-outside-quote-regexp nil)
(defsubst eshell--non-special-outside-quote-regexp ()
  (or eshell--non-special-outside-quote-regexp
      (setq-local eshell--non-special-outside-quote-regexp
                  (rx-to-string
                   `(+ (not (any ,@eshell-special-chars-outside-quoting))) t))))

(defvar eshell--after-range-token-regexp nil)
(defsubst eshell--after-range-token-regexp ()
  (or eshell--after-range-token-regexp
      (setq-local eshell--after-range-token-regexp
                  (rx-to-string
                   `(or (any ,@eshell-special-chars-outside-quoting)
                        (regexp ,eshell-integer-regexp))
                   t))))

(defsubst eshell-escape-arg (string)
  "Return STRING with the `escaped' property on it."
  (declare (obsolete nil "31.1"))
  (if (stringp string)
      (add-text-properties 0 (length string) '(escaped t) string))
  string)

(defun eshell-concat (quoted &rest rest)
  "Concatenate all the arguments in REST and return the result.
If QUOTED is nil, the resulting value(s) may be converted to
numbers (see `eshell-concat-1').

If each argument in REST is a non-list value, the result will be
a single value, as if (mapconcat #\\='eshell-stringify REST) had been
called, possibly converted to a number.

If there is at least one (non-nil) list argument, the result will
be a list, with \"adjacent\" elements of consecutive arguments
concatenated as strings (again, possibly converted to numbers).
For example, concatenating \"a\", (\"b\"), and (\"c\" \"d\")
would produce (\"abc\" \"d\")."
  (let (result)
    (dolist (i rest result)
      (when i
        (cond
         ((null result)
          (setq result i))
         ((listp result)
          (let (curr-head curr-tail)
            (if (listp i)
                (setq curr-head (car i)
                      curr-tail (cdr i))
              (setq curr-head i
                    curr-tail nil))
            (setq result
                  (append
                   (butlast result 1)
                   (list (eshell-concat-1 quoted (car (last result))
                                          curr-head))
                   curr-tail))))
         ((listp i)
          (setq result
                (cons (eshell-concat-1 quoted result (car i))
                      (cdr i))))
         (t
          (setq result (eshell-concat-1 quoted result i))))))))

(defun eshell-concat-1 (quoted first second)
  "Concatenate FIRST and SECOND.
If QUOTED is nil and either FIRST or SECOND are numberlike, try to mark
the result as a number as well."
  (let ((result (concat (eshell-stringify first quoted)
                        (eshell-stringify second quoted))))
    (when (and (not quoted)
               (or (numberp first)  (eshell--numeric-string-p first)
                   (numberp second) (eshell--numeric-string-p second)))
      (eshell-mark-numeric-string result))
    result))

(defun eshell-concat-groups (quoted &rest args)
  "Concatenate groups of arguments in ARGS and return the result.
QUOTED is passed to `eshell-concat' (which see) and, if non-nil,
allows values to be converted to numbers where appropriate.

ARGS should be a list of lists of arguments, such as that
produced by `eshell-prepare-splice'.  \"Adjacent\" values of
consecutive arguments will be passed to `eshell-concat'.  For
example, if ARGS is

  ((list a) (list b) (list c d e) (list f g)),

then the result will be:

  ((eshell-concat QUOTED a b c)
   d
   (eshell-concat QUOTED e f)
   g)."
  (let (result current-arg)
    (dolist (arg args)
      (when arg
        (push (car arg) current-arg)
        (when (length> arg 1)
          (push (apply #'eshell-concat quoted (nreverse current-arg))
                result)
          (dolist (inner (butlast (cdr arg)))
            (push inner result))
          (setq current-arg (list (car (last arg)))))))
    (when current-arg
      (push (apply #'eshell-concat quoted (nreverse current-arg))
            result))
    (nreverse result)))

(defun eshell-resolve-current-argument ()
  "If there are pending modifications to be made, make them now."
  (when eshell-current-argument
    (when eshell-arg-listified
      (if-let* ((grouped-terms (eshell-prepare-splice
                                eshell-current-argument)))
          (setq eshell-current-argument
                `(eshell-splice-args
                  (eshell-concat-groups ,eshell-current-quoted
                                        ,@grouped-terms)))
        ;; If no terms are spliced, use a simpler command form.
        (setq eshell-current-argument
              (append (list 'eshell-concat eshell-current-quoted)
                      eshell-current-argument)))
      (setq eshell-arg-listified nil))
    (when eshell-current-modifiers
      (eshell-debug-command 'form
        "applying modifiers %S\n\n%s" eshell-current-modifiers
        (eshell-stringify eshell-current-argument)))
    (dolist (modifier eshell-current-modifiers)
      (setq eshell-current-argument
            (list modifier eshell-current-argument))))
  (setq eshell-current-modifiers nil))

(defun eshell-finish-arg (&rest arguments)
  "Finish the current argument being processed.
If any ARGUMENTS are specified, they will be added to the final
argument list in place of the value of the current argument."
  (when arguments
    (if (= (length arguments) 1)
        (setq eshell-current-argument (car arguments))
      (cl-assert (and (not eshell-arg-listified)
                      (not eshell-current-modifiers)))
      (setq eshell-current-argument
            (cons 'eshell-splice-immediately arguments))))
  (throw 'eshell-arg-done t))

(defun eshell-quote-argument (string)
  "Return STRING with magic characters quoted.
Magic characters are those in `eshell-special-chars-outside-quoting'.
For consistent results, only call this function within an Eshell buffer."
  (let ((index 0))
    (mapconcat (lambda (c)
		 (prog1
		     (or (eshell-quote-backslash string index)
			 (char-to-string c))
		   (setq index (1+ index))))
	       string
	       "")))

;; Argument parsing

(defun eshell-parse-arguments (beg end)
  "Parse all of the arguments at point from BEG to END.
Returns the list of arguments in their raw form.
Point is left at the end of the arguments."
  (save-excursion
    (save-restriction
      (goto-char beg)
      (narrow-to-region beg end)
      (let ((args (list t))
	    delim)
        (with-silent-modifications
          (remove-text-properties (point-min) (point-max)
                                  '(arg-begin nil arg-end nil))
          (if (setq
               delim
               (catch 'eshell-incomplete
                 (while (not (eobp))
                   (let* ((here (point))
                          (arg (eshell-parse-argument)))
                     (if (= (point) here)
                         (error "Failed to parse argument `%s'"
                                (buffer-substring here (point-max))))
                     (when arg
                       (nconc args
                              (if (eq (car-safe arg)
                                      'eshell-splice-immediately)
                                  (cdr arg)
                                (list arg))))))))
              (throw 'eshell-incomplete (if (listp delim)
                                            delim
                                          (list delim (point) (cdr args)))))
          (cdr args))))))

(defun eshell-parse-argument ()
  "Get the next argument.  Leave point after it."
  (let* ((outer (null eshell-nested-argument))
	 (arg-begin (and outer (point)))
	 (eshell-nested-argument t)
	 eshell-current-argument
	 eshell-current-modifiers
	 eshell-arg-listified)
    (catch 'eshell-arg-done
      (while (not (eobp))
	(let ((result
	       (or (run-hook-with-args-until-success
		    'eshell-parse-argument-hook)
		   (prog1
		       (char-to-string (char-after))
		     (forward-char)))))
          (unless (eq result 'eshell-empty-token)
            (if (not eshell-current-argument)
                (setq eshell-current-argument result)
              (unless eshell-arg-listified
                (setq eshell-current-argument
                      (list eshell-current-argument)
                      eshell-arg-listified t))
              (nconc eshell-current-argument (list result)))))))
    (when (and outer eshell-current-argument)
      (add-text-properties arg-begin (1+ arg-begin)
			   '(arg-begin t rear-nonsticky
				       (arg-begin arg-end)))
      (add-text-properties (1- (point)) (point)
			   '(arg-end t rear-nonsticky
				     (arg-end arg-begin))))
    (eshell-resolve-current-argument)
    eshell-current-argument))

(defsubst eshell-operator (&rest _args)
  "A stub function that generates an error if a floating operator is found."
  (error "Unhandled operator in input text"))

(defsubst eshell-splice-args (&rest _args)
  "A stub function that generates an error if a floating splice is found."
  (error "Splice operator is not permitted in this context"))

(defconst eshell--range-token (propertize ".." 'eshell-range t))

(defun eshell-parse-number ()
  "Parse a numeric argument.
Eshell can treat unquoted arguments matching `eshell-number-regexp' as
their numeric values."
  (when (and (not eshell-current-argument)
             (not eshell-current-quoted)
             (looking-at eshell-number-regexp)
             (eshell-arg-delimiter (match-end 0)))
    (goto-char (match-end 0))
    (let ((str (match-string 0)))
      (add-text-properties 0 (length str) '(number t) str)
      str)))

(defun eshell-parse-integer ()
  "Parse an integer argument."
  (unless eshell-current-quoted
    (let ((prev-token (if eshell-arg-listified
                          (car (last eshell-current-argument))
                        eshell-current-argument)))
      (when (and (memq prev-token `(nil ,eshell--range-token))
                 (looking-at eshell-integer-regexp)
                 (or (eshell-arg-delimiter (match-end 0))
                     (save-excursion
                       (goto-char (match-end 0))
                       (looking-at-p (rx "..")))))
        (goto-char (match-end 0))
        (let ((str (match-string 0)))
          (add-text-properties 0 (length str) '(number t) str)
          str)))))

(defun eshell-unmark-range-token (string)
  (remove-text-properties 0 (length string) '(eshell-range) string))

(defun eshell-parse-range-token ()
  "Parse a range token.
This separates two integers (possibly as dollar expansions) and denotes
a half-open range."
  (when (and (not eshell-current-quoted)
             (looking-at (rx ".."))
             (or (eshell-arg-delimiter (match-end 0))
                 (save-excursion
                   (goto-char (match-end 0))
                   (looking-at (eshell--after-range-token-regexp)))))
    ;; If we parse multiple range tokens for a single argument, then
    ;; they can't actually be range tokens.  Unmark the result to
    ;; indicate this.
    (when (memq eshell--range-token
                (if eshell-arg-listified
                    eshell-current-argument
                  (list eshell-current-argument)))
      (add-hook 'eshell-current-modifiers #'eshell-unmark-range-token))
    (forward-char 2)
    eshell--range-token))

(defun eshell-parse-non-special ()
  "Parse any non-special characters, depending on the current context."
  (when (looking-at (if eshell-current-quoted
                        (eshell--non-special-inside-quote-regexp)
                      (eshell--non-special-outside-quote-regexp)))
    (goto-char (match-end 0))
    (let ((str (match-string 0)))
      (when str
        (set-text-properties 0 (length str) nil str))
      str)))

(defun eshell-parse-whitespace ()
  "Parse any whitespace, finishing the current argument.
These are treated as argument delimiters and so finish the current argument."
  (when (looking-at "[ \t]+")
    (goto-char (match-end 0))
    (eshell-finish-arg)))

(defun eshell-parse-comment ()
  "Parse a comment, finishing the current argument."
  (when (and (not eshell-current-argument)
             (looking-at "#\\([^<'].*\\|$\\)"))
    (add-text-properties (match-beginning 0) (match-end 0) '(comment t))
    (goto-char (match-end 0))
    (eshell-finish-arg)))

(defsubst eshell-looking-at-backslash-return (pos)
  "Test whether a backslash-return sequence occurs at POS."
  (declare (obsolete nil "30.1"))
  (and (eq (char-after pos) ?\\)
       (or (= (1+ pos) (point-max))
	   (and (eq (char-after (1+ pos)) ?\n)
		(= (+ pos 2) (point-max))))))

(defun eshell-quote-backslash (string &optional index)
  "Intelligently backslash the character occurring in STRING at INDEX.
If the character is itself a backslash, it needs no escaping.  If the
character is a newline, quote it using single-quotes."
  (let ((char (aref string index)))
    (cond ((eq char ?\\)
	   (char-to-string char))
          ((eq char ?\n)
           "'\n'")
          ((memq char eshell-special-chars-outside-quoting)
	   (string ?\\ char)))))

(defun eshell-parse-backslash ()
  "Parse a single backslash (\\) character and the character after.
If the character after the backslash is special, always ignore
the backslash and return the escaped character.

Otherwise, if the backslash is not in quoted string, the
backslash is ignored and the character after is returned.  If the
backslash is in a quoted string, the backslash and the character
after are both returned."
  (when (eq (char-after) ?\\)
    (when (= (1+ (point)) (point-max))
      (throw 'eshell-incomplete "\\"))
    (forward-char 2) ; Move one char past the backslash.
    (cond
     ;; Escaped newlines are extra-special: they expand to an empty
     ;; token to allow for continuing Eshell commands across
     ;; multiple lines.
     ((eq (char-before) ?\n)
      'eshell-empty-token)
     ;; If the char is in a quote, backslash only has special
     ;; meaning if it is escaping a special char.  Otherwise, the
     ;; result is the literal string "\c".
     ((and eshell-current-quoted
           (not (memq (char-before) eshell-special-chars-inside-quoting)))
      (concat "\\" (char-to-string (char-before))))
     (t
      (char-to-string (char-before))))))

(defun eshell-parse-literal-quote ()
  "Parse a literally quoted string.  Nothing has special meaning!"
  (when (eq (char-after) ?\')
    (let ((end (eshell-find-delimiter ?\' ?\')))
      (unless end
        (throw 'eshell-incomplete "'"))
      (let ((string (buffer-substring-no-properties (1+ (point)) end)))
        (goto-char (1+ end))
        (while (string-match "''" string)
          (setq string (replace-match "'" t t string)))
        string))))

(defun eshell-parse-double-quote ()
  "Parse a double quoted string, which allows for variable interpolation."
  (when (eq (char-after) ?\")
    (let* ((end (eshell-find-delimiter ?\" ?\" nil nil t))
           (eshell-current-quoted t))
      (unless end
        (throw 'eshell-incomplete "\""))
      (prog1
          (save-restriction
            (forward-char)
            (narrow-to-region (point) end)
            (or (eshell-parse-argument) ""))
        (goto-char (1+ end))))))

(defun eshell-unescape-inner-double-quote (bound)
  "Unescape escaped characters inside a double-quoted string.
The string to parse starts at point and ends at BOUND.

If Eshell is currently parsing a quoted string and there are any
backslash-escaped characters, this will return the unescaped
string, updating point to BOUND.  Otherwise, this returns nil and
leaves point where it was."
  (when eshell-current-quoted
    (let (strings
          (start (point))
          (special-char
           (rx-to-string
            `(seq "\\" (group (any ,@eshell-special-chars-inside-quoting))))))
      (while (re-search-forward special-char bound t)
        (push (concat (buffer-substring start (match-beginning 0))
                      (match-string 1))
              strings)
        (setq start (match-end 0)))
      (when strings
        (push (buffer-substring start bound) strings)
        (goto-char bound)
        (apply #'concat (nreverse strings))))))

(defun eshell-parse-delimiter ()
  "Parse a command delimiter, which is essentially a command operator."
  ;; this `eshell-operator' keyword gets parsed out by
  ;; `eshell-split-commands'.  Right now the only possibility for
  ;; error is an incorrect output redirection specifier.
  (when (looking-at (rx (group (or "&" "|" ";" "\n" "&&" "||"))
                        (* (syntax whitespace))))
    (if eshell-current-argument
	(eshell-finish-arg)
      (let ((operator (match-string 1)))
        (when (string= operator "\n")
          (setq operator ";"))
        (eshell-finish-arg
         (prog1
	     `(eshell-operator ,operator)
	   (goto-char (match-end 0))))))))

(defun eshell-prepare-splice (args)
  "Prepare a list of ARGS for splicing, if any arg requested a splice.
This looks for `eshell-splice-args' as the CAR of each argument,
and if found, returns a grouped list like:

  ((list arg-1) (list arg-2) spliced-arg-3 ...)

This allows callers of this function to build the final spliced
list by concatenating each element together, e.g. with

   (apply #\\='append grouped-list)

If no argument requested a splice, return nil."
  (let* ((splicep nil)
         ;; Group each arg like ((list arg-1) (list arg-2) ...),
         ;; splicing in `eshell-splice-args' args.  This lets us
         ;; apply spliced args correctly elsewhere.
         (grouped-args
          (mapcar (lambda (i)
                    (if (eq (car-safe i) 'eshell-splice-args)
                        (progn
                          (setq splicep t)
                          (cadr i))
                      `(list ,i)))
                  args)))
    (when splicep
      grouped-args)))

;;; Special references

(defsubst eshell--special-ref-function (type function)
  "Get the specified FUNCTION for a particular special ref TYPE.
If TYPE is nil, get the FUNCTION for the `eshell-special-ref-default'."
  (cadr (assq function (assoc (or type eshell-special-ref-default)
                              eshell-special-ref-alist))))

(defun eshell-parse-special-reference ()
  "Parse a special syntax reference, of the form `#<args>'.

args           := `type' `whitespace' `arbitrary-args' | `arbitrary-args'
type           := one of the keys in `eshell-special-ref-alist'
arbitrary-args := any number of Eshell arguments

If the form has no `type', the syntax is parsed as if `type' were
`eshell-special-ref-default'."
  (let ((here (point))
        (special-ref-types (mapcar #'car eshell-special-ref-alist)))
    (when (and (not eshell-current-argument)
               (not eshell-current-quoted)
               (looking-at (rx-to-string
                            `(seq "#<" (? (group (or ,@special-ref-types))
                                          (+ space)))
                            t)))
      (goto-char (match-end 0))         ; Go to the end of the match.
      (let ((end (eshell-find-delimiter ?\< ?\>))
            (creation-fun (eshell--special-ref-function
                           (match-string 1) 'creation-function)))
        (unless end
          (when (match-beginning 1)
            (goto-char (match-beginning 1)))
          (throw 'eshell-incomplete "#<"))
        (if (eshell-arg-delimiter (1+ end))
            (prog1
                (cons creation-fun
                      (let ((eshell-current-argument-plain t))
                        (mapcar #'eshell-term-as-value
                                (eshell-parse-arguments (point) end))))
              (goto-char (1+ end)))
          (ignore (goto-char here)))))))

(defun eshell-insert-special-reference (type &rest args)
  "Insert a special reference of the specified TYPE.
ARGS is a list of arguments to pass to the insertion function for
TYPE (see `eshell-special-ref-alist')."
  (interactive
   (let* ((type (completing-read
                 (format-prompt "Type" eshell-special-ref-default)
                 (mapcar #'car eshell-special-ref-alist)
                 nil 'require-match nil nil eshell-special-ref-default))
          (insertion-fun (eshell--special-ref-function
                          type 'insertion-function)))
     (list :interactive (call-interactively insertion-fun))))
  (if (eq type :interactive)
      (car args)
    (apply (eshell--special-ref-function type 'insertion-function) args)))

(defun eshell-complete-special-reference ()
  "If there is a special reference, complete it."
  (when (string-prefix-p "#<" (pcomplete-actual-arg))
    (let ((special-ref-types (mapcar #'car eshell-special-ref-alist))
          num-args explicit-type)
      ;; When finished with completion, add a trailing ">" when
      ;; appropriate.
      (add-function
       :around (var pcomplete-exit-function)
       (lambda (oldfun value status)
         (when (eq status 'finished)
           ;; Don't count the special reference type (e.g. "buffer").
           (when (or explicit-type
                     (and (= num-args 1)
                          (member value special-ref-types)))
             (setq num-args (1- num-args)))
           (let ((creation-fun (eshell--special-ref-function
                                explicit-type 'creation-function)))
             ;; Check if we already have the maximum number of
             ;; arguments for this special ref type.  If so, finish
             ;; the ref with ">".  Otherwise, insert a space and set
             ;; the completion status to `sole'.
             (if (eq (cdr (func-arity creation-fun)) num-args)
                 (if (looking-at ">")
                     (goto-char (match-end 0))
                   (insert ">"))
               (pcomplete-default-exit-function value status)
               (setq status 'sole))
             (funcall oldfun value status)))))
      ;; Parse the arguments to this special reference and call the
      ;; appropriate completion function.
      (save-excursion
        (eshell-with-temp-command (cons (+ 2 (pcomplete-begin)) (point))
          (goto-char (point-max))
          (let (pcomplete-args pcomplete-last pcomplete-index pcomplete-begins)
            (when (let ((eshell-current-argument-plain t))
                    (pcomplete-parse-arguments
                     pcomplete-expand-before-complete))
              (setq num-args (length pcomplete-args))
              (if (= pcomplete-index pcomplete-last)
                  ;; Call the default special ref completion function,
                  ;; and also add the known special ref types as
                  ;; possible completions.
                  (throw 'pcomplete-completions
                         (nconc
                          (mapcar #'car eshell-special-ref-alist)
                          (catch 'pcomplete-completions
                            (funcall (eshell--special-ref-function
                                      nil 'completion-function)))))
                ;; Get the special ref type and call its completion
                ;; function.
                (let ((first (pcomplete-arg 'first)))
                  (when (member first special-ref-types)
                    ;; "Complete" the ref type (which we already
                    ;; completed above).
                    (pcomplete-here)
                    (setq explicit-type first)))
                (funcall (eshell--special-ref-function
                          explicit-type 'completion-function))))))))))

(defun eshell-get-buffer (buffer-or-name)
  "Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.
This is equivalent to `get-buffer-create', but only accepts a
single argument."
  (get-buffer-create buffer-or-name))

(defun eshell-insert-buffer-name (buffer-name)
  "Insert BUFFER-NAME into the current buffer at point."
  (interactive "BName of buffer: ")
  (insert-and-inherit "#<buffer " (eshell-quote-argument buffer-name) ">"))

(defun eshell-complete-buffer-ref ()
  "Perform completion for buffer references."
  (pcomplete-here (mapcar #'buffer-name (buffer-list))))

(defun eshell-get-marker (position buffer-or-name)
  "Return the marker for character number POSITION in BUFFER-OR-NAME.
BUFFER-OR-NAME can be a buffer or a string.  If a string and a
live buffer with that name exists, use that buffer.  If no such
buffer exists, create a new buffer with that name and use it."
  (let ((marker (make-marker)))
    (set-marker marker (string-to-number position)
                (get-buffer-create buffer-or-name))))

(defun eshell-insert-marker (position buffer-name)
  "Insert a marker into the current buffer at point.
This marker will point to POSITION in BUFFER-NAME."
  (interactive "nPosition: \nBName of buffer: ")
  (insert-and-inherit "#<marker " (number-to-string position) " "
                      (eshell-quote-argument buffer-name) ">"))

(defun eshell-complete-marker-ref ()
  "Perform completion for marker references."
  (pcomplete-here)
  (pcomplete-here (mapcar #'buffer-name (buffer-list))))

(provide 'esh-arg)
;;; esh-arg.el ends here
