;;; esh-arg.el --- argument processing  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2023 Free Software Foundation, Inc.

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

(eval-when-compile
  (require 'cl-lib))

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
(defvar eshell-inside-quote-regexp nil)
(defvar eshell-outside-quote-regexp nil)

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
  (list
   ;; a term such as #<buffer NAME>, or #<process NAME> is a buffer
   ;; or process reference
   'eshell-parse-special-reference

   ;; numbers convert to numbers if they stand alone
   (lambda ()
     (when (and (not eshell-current-argument)
                (not eshell-current-quoted)
                (looking-at eshell-number-regexp)
                (eshell-arg-delimiter (match-end 0)))
       (goto-char (match-end 0))
       (let ((str (match-string 0)))
         (if (> (length str) 0)
             (add-text-properties 0 (length str) '(number t) str))
         str)))

   ;; parse any non-special characters, based on the current context
   (lambda ()
     (unless eshell-inside-quote-regexp
       (setq eshell-inside-quote-regexp
             (format "[^%s]+"
                     (apply 'string eshell-special-chars-inside-quoting))))
     (unless eshell-outside-quote-regexp
       (setq eshell-outside-quote-regexp
             (format "[^%s]+"
                     (apply 'string eshell-special-chars-outside-quoting))))
     (when (looking-at (if eshell-current-quoted
                           eshell-inside-quote-regexp
                         eshell-outside-quote-regexp))
       (goto-char (match-end 0))
       (let ((str (match-string 0)))
         (if str
             (set-text-properties 0 (length str) nil str))
         str)))

   ;; whitespace or a comment is an argument delimiter
   (lambda ()
     (let (comment-p)
       (when (or (looking-at "[ \t]+")
                 (and (not eshell-current-argument)
                      (looking-at "#\\([^<'].*\\|$\\)")
                      (setq comment-p t)))
         (if comment-p
             (add-text-properties (match-beginning 0) (match-end 0)
                                  '(comment t)))
         (goto-char (match-end 0))
         (eshell-finish-arg))))

   ;; parse backslash and the character after
   'eshell-parse-backslash

   ;; text beginning with ' is a literally quoted
   'eshell-parse-literal-quote

   ;; text beginning with " is interpolably quoted
   'eshell-parse-double-quote

   ;; argument delimiter
   'eshell-parse-delimiter)
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
  (setq-local eshell-inside-quote-regexp nil)
  (setq-local eshell-outside-quote-regexp nil))

(defun eshell-insert-buffer-name (buffer-name)
  "Insert BUFFER-NAME into the current buffer at point."
  (interactive "BName of buffer: ")
  (insert-and-inherit "#<buffer " buffer-name ">"))

(defsubst eshell-escape-arg (string)
  "Return STRING with the `escaped' property on it."
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
If QUOTED is nil and either FIRST or SECOND are numbers, try to
convert the result to a number as well."
  (let ((result (concat (eshell-stringify first) (eshell-stringify second))))
    (if (and (not quoted)
             (or (numberp first) (numberp second)))
        (eshell-convert-to-number result)
      result)))

(defun eshell-concat-groups (quoted &rest args)
  "Concatenate groups of arguments in ARGS and return the result.
QUOTED is passed to `eshell-concat' (which see) and, if non-nil,
allows values to be converted to numbers where appropriate.

ARGS should be a list of lists of arguments, such as that
produced by `eshell-prepare-slice'.  \"Adjacent\" values of
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
      (if-let ((grouped-terms (eshell-prepare-splice
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
    (while eshell-current-modifiers
      (setq eshell-current-argument
	    (list (car eshell-current-modifiers) eshell-current-argument)
	    eshell-current-modifiers (cdr eshell-current-modifiers))))
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
Magic characters are those in `eshell-special-chars-outside-quoting'."
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

(defsubst eshell-looking-at-backslash-return (pos)
  "Test whether a backslash-return sequence occurs at POS."
  (and (eq (char-after pos) ?\\)
       (or (= (1+ pos) (point-max))
	   (and (eq (char-after (1+ pos)) ?\n)
		(= (+ pos 2) (point-max))))))

(defun eshell-quote-backslash (string &optional index)
  "Intelligently backslash the character occurring in STRING at INDEX.
If the character is itself a backslash, it needs no escaping."
  (let ((char (aref string index)))
    (if (eq char ?\\)
	(char-to-string char)
      (if (memq char eshell-special-chars-outside-quoting)
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
    (when (eshell-looking-at-backslash-return (point))
        (throw 'eshell-incomplete "\\"))
    (forward-char 2) ; Move one char past the backslash.
    (let ((special-chars (if eshell-current-quoted
                             eshell-special-chars-inside-quoting
                           eshell-special-chars-outside-quoting)))
      (cond
       ;; Escaped newlines are extra-special: they expand to an empty
       ;; token to allow for continuing Eshell commands across
       ;; multiple lines.
       ((eq (char-before) ?\n)
        'eshell-empty-token)
       ((memq (char-before) special-chars)
        (list 'eshell-escape-arg (char-to-string (char-before))))
       ;; If the char is in a quote, backslash only has special
       ;; meaning if it is escaping a special char.  Otherwise, the
       ;; result is the literal string "\c".
       (eshell-current-quoted
        (concat "\\" (char-to-string (char-before))))
       (t
        (char-to-string (char-before)))))))

(defun eshell-parse-literal-quote ()
  "Parse a literally quoted string.  Nothing has special meaning!"
  (if (eq (char-after) ?\')
      (let ((end (eshell-find-delimiter ?\' ?\')))
	(if (not end)
            (throw 'eshell-incomplete "'")
	  (let ((string (buffer-substring-no-properties (1+ (point)) end)))
	    (goto-char (1+ end))
	    (while (string-match "''" string)
	      (setq string (replace-match "'" t t string)))
	    (list 'eshell-escape-arg string))))))

(defun eshell-parse-double-quote ()
  "Parse a double quoted string, which allows for variable interpolation."
  (when (eq (char-after) ?\")
    (let* ((end (eshell-find-delimiter ?\" ?\" nil nil t))
	   (eshell-current-quoted t))
      (if (not end)
          (throw 'eshell-incomplete "\"")
	(prog1
	    (save-restriction
	      (forward-char)
	      (narrow-to-region (point) end)
	      (let ((arg (eshell-parse-argument)))
		(if (eq arg nil)
		    ""
		  (list 'eshell-escape-arg arg))))
	  (goto-char (1+ end)))))))

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

(defun eshell-parse-special-reference ()
  "Parse a special syntax reference, of the form `#<args>'.

args           := `type' `whitespace' `arbitrary-args' | `arbitrary-args'
type           := \"buffer\" or \"process\"
arbitrary-args := any string of characters.

If the form has no `type', the syntax is parsed as if `type' were
\"buffer\"."
  (when (and (not eshell-current-argument)
             (not eshell-current-quoted)
             (looking-at "#<\\(\\(buffer\\|process\\)\\s-\\)?"))
    (let ((here (point)))
      (goto-char (match-end 0)) ;; Go to the end of the match.
      (let ((buffer-p (if (match-string 1)
                          (string= (match-string 2) "buffer")
                        t)) ;; buffer-p is non-nil by default.
            (end (eshell-find-delimiter ?\< ?\>)))
        (when (not end)
          (throw 'eshell-incomplete "#<"))
        (if (eshell-arg-delimiter (1+ end))
            (prog1
                (list (if buffer-p 'get-buffer-create 'get-process)
                      (replace-regexp-in-string
                       (rx "\\" (group (or "\\" "<" ">"))) "\\1"
                       (buffer-substring-no-properties (point) end)))
              (goto-char (1+ end)))
          (ignore (goto-char here)))))))

(defun eshell-parse-delimiter ()
  "Parse an argument delimiter, which is essentially a command operator."
  ;; this `eshell-operator' keyword gets parsed out by
  ;; `eshell-separate-commands'.  Right now the only possibility for
  ;; error is an incorrect output redirection specifier.
  (when (looking-at "[&|;\n]\\s-*")
    (let ((end (match-end 0)))
    (if eshell-current-argument
	(eshell-finish-arg)
      (eshell-finish-arg
       (prog1
	   (list 'eshell-operator
		 (cond
		  ((eq (char-after end) ?\&)
		   (setq end (1+ end)) "&&")
		  ((eq (char-after end) ?\|)
		   (setq end (1+ end)) "||")
		  ((eq (char-after) ?\n) ";")
		  (t
		   (char-to-string (char-after)))))
	 (goto-char end)))))))

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

(provide 'esh-arg)
;;; esh-arg.el ends here
