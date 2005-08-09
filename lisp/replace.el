;;; replace.el --- replace commands for Emacs

;; Copyright (C) 1985, 1986, 1987, 1992, 1994, 1996, 1997, 2000, 2001,
;;   2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Maintainer: FSF

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

;; This package supplies the string and regular-expression replace functions
;; documented in the Emacs user's manual.

;;; Code:

(defcustom case-replace t
  "*Non-nil means `query-replace' should preserve case in replacements."
  :type 'boolean
  :group 'matching)

(defvar query-replace-history nil)

(defvar query-replace-interactive nil
  "Non-nil means `query-replace' uses the last search string.
That becomes the \"string to replace\".")

(defcustom query-replace-from-history-variable 'query-replace-history
  "History list to use for the FROM argument of `query-replace' commands.
The value of this variable should be a symbol; that symbol
is used as a variable to hold a history list for the strings
or patterns to be replaced."
  :group 'matching
  :type 'symbol
  :version "20.3")

(defcustom query-replace-to-history-variable 'query-replace-history
  "History list to use for the TO argument of `query-replace' commands.
The value of this variable should be a symbol; that symbol
is used as a variable to hold a history list for replacement
strings or patterns."
  :group 'matching
  :type 'symbol
  :version "20.3")

(defcustom query-replace-skip-read-only nil
  "*Non-nil means `query-replace' and friends ignore read-only matches."
  :type 'boolean
  :group 'matching
  :version "22.1")

(defcustom query-replace-highlight t
  "*Non-nil means to highlight matches during query replacement."
  :type 'boolean
  :group 'matching)

(defcustom query-replace-lazy-highlight t
  "*Controls the lazy-highlighting during query replacements.
When non-nil, all text in the buffer matching the current match
is highlighted lazily using isearch lazy highlighting (see
`lazy-highlight-initial-delay' and `lazy-highlight-interval')."
  :type 'boolean
  :group 'lazy-highlight
  :group 'matching
  :version "22.1")

(defface query-replace
  '((t (:inherit isearch)))
  "Face for highlighting query replacement matches."
  :group 'matching
  :version "22.1")

(defun query-replace-descr (string)
  (mapconcat 'isearch-text-char-description string ""))

(defun query-replace-read-from (string regexp-flag)
  "Query and return the `from' argument of a query-replace operation.
The return value can also be a pair (FROM . TO) indicating that the user
wants to replace FROM with TO."
  (if query-replace-interactive
      (car (if regexp-flag regexp-search-ring search-ring))
    (let* ((lastfrom (car (symbol-value query-replace-from-history-variable)))
	   (lastto (car (symbol-value query-replace-to-history-variable)))
	   (from
	    ;; The save-excursion here is in case the user marks and copies
	    ;; a region in order to specify the minibuffer input.
	    ;; That should not clobber the region for the query-replace itself.
	    (save-excursion
	      (when (equal lastfrom lastto)
		;; Typically, this is because the two histlists are shared.
		(setq lastfrom (cadr (symbol-value
				      query-replace-from-history-variable))))
	      (read-from-minibuffer
	       (if (and lastto lastfrom)
		   (format "%s (default %s -> %s): " string
			   (query-replace-descr lastfrom)
			   (query-replace-descr lastto))
		 (format "%s: " string))
	       nil nil nil
	       query-replace-from-history-variable
	       nil t t))))
      (if (and (zerop (length from)) lastto lastfrom)
	  (progn
	    (set query-replace-from-history-variable
		 (cdr (symbol-value query-replace-from-history-variable)))
	    (cons lastfrom
		  (query-replace-compile-replacement lastto regexp-flag)))
	;; Warn if user types \n or \t, but don't reject the input.
	(and regexp-flag
	     (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
	     (let ((match (match-string 3 from)))
	       (cond
		((string= match "\\n")
		 (message "Note: `\\n' here doesn't match a newline; to do that, type C-q C-j instead"))
		((string= match "\\t")
		 (message "Note: `\\t' here doesn't match a tab; to do that, just type TAB")))
	       (sit-for 2)))
	from))))

(defun query-replace-compile-replacement (to regexp-flag)
  "Maybe convert a regexp replacement TO to Lisp.
Returns a list suitable for `perform-replace' if necessary,
the original string if not."
  (if (and regexp-flag
	   (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to))
      (let (pos list char)
	(while
	    (progn
	      (setq pos (match-end 0))
	      (push (substring to 0 (- pos 2)) list)
	      (setq char (aref to (1- pos))
		    to (substring to pos))
	      (cond ((eq char ?\#)
		     (push '(number-to-string replace-count) list))
		    ((eq char ?\,)
		     (setq pos (read-from-string to))
		     (push `(replace-quote ,(car pos)) list)
		     (let ((end
			    ;; Swallow a space after a symbol
			    ;; if there is a space.
			    (if (and (or (symbolp (car pos))
					 ;; Swallow a space after 'foo
					 ;; but not after (quote foo).
					 (and (eq (car-safe (car pos)) 'quote)
					      (not (= ?\( (aref to 0)))))
				     (eq (string-match " " to (cdr pos))
					 (cdr pos)))
				(1+ (cdr pos))
			      (cdr pos))))
		       (setq to (substring to end)))))
	      (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to)))
	(setq to (nreverse (delete "" (cons to list))))
	(replace-match-string-symbols to)
	(cons 'replace-eval-replacement
	      (if (cdr to)
		  (cons 'concat to)
		(car to))))
    to))


(defun query-replace-read-to (from string regexp-flag)
  "Query and return the `to' argument of a query-replace operation."
  (query-replace-compile-replacement
   (save-excursion
     (read-from-minibuffer
      (format "%s %s with: " string (query-replace-descr from))
      nil nil nil
      query-replace-to-history-variable from t t))
   regexp-flag))

(defun query-replace-read-args (string regexp-flag &optional noerror)
  (unless noerror
    (barf-if-buffer-read-only))
  (let* ((from (query-replace-read-from string regexp-flag))
	 (to (if (consp from) (prog1 (cdr from) (setq from (car from)))
	       (query-replace-read-to from string regexp-flag))))
    (list from to current-prefix-arg)))

(defun query-replace (from-string to-string &optional delimited start end)
  "Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
string is used as FROM-STRING--you don't have to specify it with the
minibuffer.

Matching is independent of case if `case-fold-search' is non-nil and
FROM-STRING has no uppercase letters.  Replacement transfers the case
pattern of the old text to the new text, if `case-replace' and
`case-fold-search' are non-nil and FROM-STRING has no uppercase
letters.  \(Transferring the case pattern means that if the old text
matched is all caps, or capitalized, then its replacement is upcased
or capitalized.)

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

To customize possible responses, change the \"bindings\" in `query-replace-map'."
  (interactive (let ((common
		      (query-replace-read-args
		       (if (and transient-mark-mode mark-active)
			 "Query replace in region"
			 "Query replace")
			 nil)))
		 (list (nth 0 common) (nth 1 common) (nth 2 common)
		       ;; These are done separately here
		       ;; so that command-history will record these expressions
		       ;; rather than the values they had this time.
		       (if (and transient-mark-mode mark-active)
			   (region-beginning))
		       (if (and transient-mark-mode mark-active)
			   (region-end)))))
  (perform-replace from-string to-string t nil delimited nil nil start end))

(define-key esc-map "%" 'query-replace)

(defun query-replace-regexp (regexp to-string &optional delimited start end)
  "Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the
minibuffer.

Matching is independent of case if `case-fold-search' is non-nil and
REGEXP has no uppercase letters.  Replacement transfers the case
pattern of the old text to the new text, if `case-replace' and
`case-fold-search' are non-nil and REGEXP has no uppercase letters.
\(Transferring the case pattern means that if the old text matched is
all caps, or capitalized, then its replacement is upcased or
capitalized.)

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for
whatever what matched the Nth `\\(...\\)' in REGEXP.
`\\?' lets you edit the replacement text in the minibuffer
at the given position for each replacement.

In interactive calls, the replacement text can contain `\\,'
followed by a Lisp expression.  Each
replacement evaluates that expression to compute the replacement
string.  Inside of that expression, `\\&' is a string denoting the
whole match as a string, `\\N' for a partial match, `\\#&' and `\\#N'
for the whole or a partial match converted to a number with
`string-to-number', and `\\#' itself for the number of replacements
done so far (starting with zero).

If the replacement expression is a symbol, write a space after it
to terminate it.  One space there, if any, will be discarded.

When using those Lisp features interactively in the replacement
text, TO-STRING is actually made a list instead of a string.
Use \\[repeat-complex-command] after this command for details."
  (interactive
   (let ((common
	  (query-replace-read-args
	   (if (and transient-mark-mode mark-active)
	       "Query replace regexp in region"
	     "Query replace regexp")
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   ;; These are done separately here
	   ;; so that command-history will record these expressions
	   ;; rather than the values they had this time.
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (perform-replace regexp to-string t t delimited nil nil start end))

(define-key esc-map [?\C-%] 'query-replace-regexp)

(defun query-replace-regexp-eval (regexp to-expr &optional delimited start end)
  "Replace some things after point matching REGEXP with the result of TO-EXPR.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

TO-EXPR is a Lisp expression evaluated to compute each replacement.  It may
reference `replace-count' to get the number of replacements already made.
If the result of TO-EXPR is not a string, it is converted to one using
`prin1-to-string' with the NOESCAPE argument (which see).

For convenience, when entering TO-EXPR interactively, you can use `\\&' or
`\\0' to stand for whatever matched the whole of REGEXP, and `\\N' (where
N is a digit) to stand for whatever matched the Nth `\\(...\\)' in REGEXP.
Use `\\#&' or `\\#N' if you want a number instead of a string.
In interactive use, `\\#' in itself stands for `replace-count'.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the
minibuffer.

Preserves case in each replacement if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches that are surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on."
  (interactive
   (progn
   (barf-if-buffer-read-only)
   (let* ((from
	   ;; Let-bind the history var to disable the "foo -> bar" default.
	   ;; Maybe we shouldn't disable this default, but for now I'll
	   ;; leave it off.  --Stef
	   (let ((query-replace-to-history-variable nil))
	     (query-replace-read-from "Query replace regexp" t)))
	  (to (list (read-from-minibuffer
		     (format "Query replace regexp %s with eval: "
			     (query-replace-descr from))
		     nil nil t query-replace-to-history-variable from t))))
     ;; We make TO a list because replace-match-string-symbols requires one,
     ;; and the user might enter a single token.
     (replace-match-string-symbols to)
     (list from (car to) current-prefix-arg
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end))))))
  (perform-replace regexp (cons 'replace-eval-replacement to-expr)
		   t 'literal delimited nil nil start end))

(defun map-query-replace-regexp (regexp to-strings &optional n start end)
  "Replace some matches for REGEXP with various strings, in rotation.
The second argument TO-STRINGS contains the replacement strings,
separated by spaces.  Third arg DELIMITED (prefix arg if interactive),
if non-nil, means replace only matches surrounded by word boundaries.
This command works like `query-replace-regexp' except that each
successive replacement uses the next successive replacement string,
wrapping around from the last such string to the first.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Non-interactively, TO-STRINGS may be a list of replacement strings.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the minibuffer.

A prefix argument N says to use each replacement string N times
before rotating to the next.
Fourth and fifth arg START and END specify the region to operate on."
  (interactive
   (let* ((from (if query-replace-interactive
		    (car regexp-search-ring)
		  (read-from-minibuffer "Map query replace (regexp): "
					nil nil nil
					'query-replace-history nil t)))
	  (to (read-from-minibuffer
	       (format "Query replace %s with (space-separated strings): "
		       (query-replace-descr from))
	       nil nil nil
	       'query-replace-history from t)))
     (list from to
	   (and current-prefix-arg
		(prefix-numeric-value current-prefix-arg))
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (let (replacements)
    (if (listp to-strings)
	(setq replacements to-strings)
      (while (/= (length to-strings) 0)
	(if (string-match " " to-strings)
	    (setq replacements
		  (append replacements
			  (list (substring to-strings 0
					   (string-match " " to-strings))))
		  to-strings (substring to-strings
				       (1+ (string-match " " to-strings))))
	  (setq replacements (append replacements (list to-strings))
		to-strings ""))))
    (perform-replace regexp replacements t t nil n nil start end)))

(defun replace-string (from-string to-string &optional delimited start end)
  "Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and FROM-STRING has no uppercase letters.
\(Preserving case means that if the string matched is all caps, or capitalized,
then its replacement is upcased or capitalized.)

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

If `query-replace-interactive' is non-nil, the last incremental search
string is used as FROM-STRING--you don't have to specify it with the
minibuffer.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (search-forward FROM-STRING nil t)
    (replace-match TO-STRING nil t))
which will run faster and will not set the mark or print anything.
\(You may need a more complex loop if FROM-STRING can match the null string
and TO-STRING is also null.)"
  (interactive
   (let ((common
	  (query-replace-read-args
	   (if (and transient-mark-mode mark-active)
	       "Replace string in region"
	     "Replace string")
	   nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (perform-replace from-string to-string nil nil delimited nil nil start end))

(defun replace-regexp (regexp to-string &optional delimited start end)
  "Replace things after point matching REGEXP with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for
whatever what matched the Nth `\\(...\\)' in REGEXP.
`\\?' lets you edit the replacement text in the minibuffer
at the given position for each replacement.

In interactive calls, the replacement text may contain `\\,'
followed by a Lisp expression used as part of the replacement
text.  Inside of that expression, `\\&' is a string denoting the
whole match, `\\N' a partial matches, `\\#&' and `\\#N' the
respective numeric values from `string-to-number', and `\\#'
itself for `replace-count', the number of replacements occured so
far.

If your Lisp expression is an identifier and the next letter in
the replacement string would be interpreted as part of it, you
can wrap it with an expression like `\\,(or \\#)'.  Incidentally,
for this particular case you may also enter `\\#' in the
replacement text directly.

When using those Lisp features interactively in the replacement
text, TO-STRING is actually made a list instead of a string.
Use \\[repeat-complex-command] after this command for details.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the minibuffer.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (re-search-forward REGEXP nil t)
    (replace-match TO-STRING nil nil))
which will run faster and will not set the mark or print anything."
  (interactive
   (let ((common
	  (query-replace-read-args
	   (if (and transient-mark-mode mark-active)
	       "Replace regexp in region"
	     "Replace regexp")
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (perform-replace regexp to-string nil t delimited nil nil start end))


(defvar regexp-history nil
  "History list for some commands that read regular expressions.")


(defalias 'delete-non-matching-lines 'keep-lines)
(defalias 'delete-matching-lines 'flush-lines)
(defalias 'count-matches 'how-many)


(defun keep-lines-read-args (prompt)
  "Read arguments for `keep-lines' and friends.
Prompt for a regexp with PROMPT.
Value is a list, (REGEXP)."
  (list (read-from-minibuffer prompt nil nil nil
			      'regexp-history nil t)
	nil nil t))

(defun keep-lines (regexp &optional rstart rend interactive)
  "Delete all lines except those containing matches for REGEXP.
A match split across lines preserves all the lines it lies in.
When called from Lisp (and usually interactively as well, see below)
applies to all lines starting after point.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
This command operates on (the accessible part of) all lines whose
accessible part is entirely contained in the region determined by RSTART
and REND.  (A newline ending a line counts as part of that line.)

Interactively, in Transient Mark mode when the mark is active, operate
on all lines whose accessible part is entirely contained in the region.
Otherwise, the command applies to all lines starting after point.
When calling this function from Lisp, you can pretend that it was
called interactively by passing a non-nil INTERACTIVE argument.

This function starts looking for the next match from the end of
the previous match.  Hence, it ignores matches that overlap
a previously found match."

  (interactive
   (progn
     (barf-if-buffer-read-only)
     (keep-lines-read-args "Keep lines (containing match for regexp): ")))
  (if rstart
      (progn
	(goto-char (min rstart rend))
	(setq rend
	      (progn
		(save-excursion
		  (goto-char (max rstart rend))
		  (unless (or (bolp) (eobp))
		    (forward-line 0))
		  (point-marker)))))
    (if (and interactive transient-mark-mode mark-active)
	(setq rstart (region-beginning)
	      rend (progn
		     (goto-char (region-end))
		     (unless (or (bolp) (eobp))
		       (forward-line 0))
		     (point-marker)))
      (setq rstart (point)
	    rend (point-max-marker)))
    (goto-char rstart))
  (save-excursion
    (or (bolp) (forward-line 1))
    (let ((start (point))
	  (case-fold-search  (and case-fold-search
				  (isearch-no-upper-case-p regexp t))))
      (while (< (point) rend)
	;; Start is first char not preserved by previous match.
	(if (not (re-search-forward regexp rend 'move))
	    (delete-region start rend)
	  (let ((end (save-excursion (goto-char (match-beginning 0))
				     (forward-line 0)
				     (point))))
	    ;; Now end is first char preserved by the new match.
	    (if (< start end)
		(delete-region start end))))

	(setq start (save-excursion (forward-line 1) (point)))
	;; If the match was empty, avoid matching again at same place.
	(and (< (point) rend)
	     (= (match-beginning 0) (match-end 0))
	     (forward-char 1)))))
  (set-marker rend nil)
  nil)


(defun flush-lines (regexp &optional rstart rend interactive)
 "Delete lines containing matches for REGEXP.
When called from Lisp (and usually when called interactively as
well, see below), applies to the part of the buffer after point.
The line point is in is deleted if and only if it contains a
match for regexp starting after point.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
Lines partially contained in this region are deleted if and only if
they contain a match entirely contained in it.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of (the accessible portion of) the buffer.  When calling this function
from Lisp, you can pretend that it was called interactively by passing
a non-nil INTERACTIVE argument.

If a match is split across lines, all the lines it lies in are deleted.
They are deleted _before_ looking for the next match.  Hence, a match
starting on the same line at which another match ended is ignored."

  (interactive
   (progn
     (barf-if-buffer-read-only)
     (keep-lines-read-args "Flush lines (containing match for regexp): ")))
  (if rstart
      (progn
	(goto-char (min rstart rend))
	(setq rend (copy-marker (max rstart rend))))
    (if (and interactive transient-mark-mode mark-active)
	(setq rstart (region-beginning)
	      rend (copy-marker (region-end)))
      (setq rstart (point)
	    rend (point-max-marker)))
    (goto-char rstart))
  (let ((case-fold-search (and case-fold-search
			       (isearch-no-upper-case-p regexp t))))
    (save-excursion
      (while (and (< (point) rend)
		  (re-search-forward regexp rend t))
	(delete-region (save-excursion (goto-char (match-beginning 0))
				       (forward-line 0)
				       (point))
		       (progn (forward-line 1) (point))))))
  (set-marker rend nil)
  nil)


(defun how-many (regexp &optional rstart rend interactive)
  "Print and return number of matches for REGEXP following point.
When called from Lisp and INTERACTIVE is omitted or nil, just return
the number, do not print it; if INTERACTIVE is t, the function behaves
in all respects has if it had been called interactively.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of (the accessible portion of) the buffer.

This function starts looking for the next match from the end of
the previous match.  Hence, it ignores matches that overlap
a previously found match."

  (interactive
   (keep-lines-read-args "How many matches for (regexp): "))
  (save-excursion
    (if rstart
	(progn
	  (goto-char (min rstart rend))
	  (setq rend (max rstart rend)))
      (if (and interactive transient-mark-mode mark-active)
	  (setq rstart (region-beginning)
		rend (region-end))
	(setq rstart (point)
	      rend (point-max)))
      (goto-char rstart))
    (let ((count 0)
	  opoint
	  (case-fold-search (and case-fold-search
				 (isearch-no-upper-case-p regexp t))))
      (while (and (< (point) rend)
		  (progn (setq opoint (point))
			 (re-search-forward regexp rend t)))
	(if (= opoint (point))
	    (forward-char 1)
	  (setq count (1+ count))))
      (when interactive (message "%d occurrence%s"
				 count
				 (if (= count 1) "" "s")))
      count)))


(defvar occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'occur-mode-mouse-goto)
    (define-key map "\C-c\C-c" 'occur-mode-goto-occurrence)
    (define-key map "\C-m" 'occur-mode-goto-occurrence)
    (define-key map "o" 'occur-mode-goto-occurrence-other-window)
    (define-key map "\C-o" 'occur-mode-display-occurrence)
    (define-key map "\M-n" 'occur-next)
    (define-key map "\M-p" 'occur-prev)
    (define-key map "r" 'occur-rename-buffer)
    (define-key map "c" 'clone-buffer)
    (define-key map "g" 'revert-buffer)
    (define-key map "q" 'quit-window)
    (define-key map "z" 'kill-this-buffer)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)
    map)
  "Keymap for `occur-mode'.")

(defvar occur-revert-arguments nil
  "Arguments to pass to `occur-1' to revert an Occur mode buffer.
See `occur-revert-function'.")

(defcustom occur-mode-hook '(turn-on-font-lock)
  "Hook run when entering Occur mode."
  :type 'hook
  :group 'matching)

(defcustom occur-hook nil
  "Hook run by Occur when there are any matches."
  :type 'hook
  :group 'matching)

(put 'occur-mode 'mode-class 'special)
(defun occur-mode ()
  "Major mode for output from \\[occur].
\\<occur-mode-map>Move point to one of the items in this buffer, then use
\\[occur-mode-goto-occurrence] to go to the occurrence that the item refers to.
Alternatively, click \\[occur-mode-mouse-goto] on an item to go to it.

\\{occur-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map occur-mode-map)
  (setq major-mode 'occur-mode)
  (setq mode-name "Occur")
  (set (make-local-variable 'revert-buffer-function) 'occur-revert-function)
  (make-local-variable 'occur-revert-arguments)
  (add-hook 'change-major-mode-hook 'font-lock-defontify nil t)
  (setq next-error-function 'occur-next-error)
  (run-mode-hooks 'occur-mode-hook))

(defun occur-revert-function (ignore1 ignore2)
  "Handle `revert-buffer' for Occur mode buffers."
  (apply 'occur-1 (append occur-revert-arguments (list (buffer-name)))))

(defun occur-mode-mouse-goto (event)
  "In Occur mode, go to the occurrence whose line you click on."
  (interactive "e")
  (let (pos)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq pos (occur-mode-find-occurrence))))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)))

(defun occur-mode-find-occurrence ()
  (let ((pos (get-text-property (point) 'occur-target)))
    (unless pos
      (error "No occurrence on this line"))
    (unless (buffer-live-p (marker-buffer pos))
      (error "Buffer for this occurrence was killed"))
    pos))

(defun occur-mode-goto-occurrence ()
  "Go to the occurrence the current line describes."
  (interactive)
  (let ((pos (occur-mode-find-occurrence)))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)))

(defun occur-mode-goto-occurrence-other-window ()
  "Go to the occurrence the current line describes, in another window."
  (interactive)
  (let ((pos (occur-mode-find-occurrence)))
    (switch-to-buffer-other-window (marker-buffer pos))
    (goto-char pos)))

(defun occur-mode-display-occurrence ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (let ((pos (occur-mode-find-occurrence))
	window
	;; Bind these to ensure `display-buffer' puts it in another window.
	same-window-buffer-names
	same-window-regexps)
    (setq window (display-buffer (marker-buffer pos)))
    ;; This is the way to set point in the proper window.
    (save-selected-window
      (select-window window)
      (goto-char pos))))

(defun occur-find-match (n search message)
  (if (not n) (setq n 1))
  (let ((r))
    (while (> n 0)
      (setq r (funcall search (point) 'occur-match))
      (and r
           (get-text-property r 'occur-match)
           (setq r (funcall search r 'occur-match)))
      (if r
          (goto-char r)
        (error message))
      (setq n (1- n)))))

(defun occur-next (&optional n)
  "Move to the Nth (default 1) next match in an Occur mode buffer."
  (interactive "p")
  (occur-find-match n #'next-single-property-change "No more matches"))

(defun occur-prev (&optional n)
  "Move to the Nth (default 1) previous match in an Occur mode buffer."
  (interactive "p")
  (occur-find-match n #'previous-single-property-change "No earlier matches"))

(defun occur-next-error (&optional argp reset)
  "Move to the Nth (default 1) next match in an Occur mode buffer.
Compatibility function for \\[next-error] invocations."
  (interactive "p")
  ;; we need to run occur-find-match from within the Occur buffer
  (with-current-buffer
      ;; Choose the buffer and make it current.
      (if (next-error-buffer-p (current-buffer))
	  (current-buffer)
	(next-error-find-buffer nil nil
				(lambda ()
				  (eq major-mode 'occur-mode))))

    (goto-char (cond (reset (point-min))
		     ((< argp 0) (line-beginning-position))
		     ((line-end-position))))
    (occur-find-match
     (abs argp)
     (if (> 0 argp)
	 #'previous-single-property-change
       #'next-single-property-change)
     "No more matches")
    ;; In case the *Occur* buffer is visible in a nonselected window.
    (set-window-point (get-buffer-window (current-buffer)) (point))
    (occur-mode-goto-occurrence)))

(defface match
  '((((class color) (min-colors 88) (background light))
     :background "Tan")
    (((class color) (min-colors 88) (background dark))
     :background "RoyalBlue3")
    (((class color) (min-colors 8))
     :background "blue" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face used to highlight matches permanently."
  :group 'matching
  :version "22.1")

(defcustom list-matching-lines-default-context-lines 0
  "*Default number of context lines included around `list-matching-lines' matches.
A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after."
  :type 'integer
  :group 'matching)

(defalias 'list-matching-lines 'occur)

(defcustom list-matching-lines-face 'match
  "*Face used by \\[list-matching-lines] to show the text that matches.
If the value is nil, don't highlight the matching portions specially."
  :type 'face
  :group 'matching)

(defcustom list-matching-lines-buffer-name-face 'underline
  "*Face used by \\[list-matching-lines] to show the names of buffers.
If the value is nil, don't highlight the buffer names specially."
  :type 'face
  :group 'matching)

(defun occur-accumulate-lines (count &optional keep-props)
  (save-excursion
    (let ((forwardp (> count 0))
	  result beg end)
      (while (not (or (zerop count)
		      (if forwardp
			  (eobp)
			(bobp))))
	(setq count (+ count (if forwardp -1 1)))
	(setq beg (line-beginning-position)
	      end (line-end-position))
	(if (and keep-props (if (boundp 'jit-lock-mode) jit-lock-mode)
		 (text-property-not-all beg end 'fontified t))
	    (if (fboundp 'jit-lock-fontify-now)
		(jit-lock-fontify-now beg end)))
	(push
	 (funcall (if keep-props
		      #'buffer-substring
		    #'buffer-substring-no-properties)
		  beg end)
	 result)
	(forward-line (if forwardp 1 -1)))
      (nreverse result))))

(defun occur-read-primary-args ()
  (list (let* ((default (car regexp-history))
	       (input
		(read-from-minibuffer
		 (if default
		     (format "List lines matching regexp (default `%s'): "
			     (query-replace-descr default))
		   "List lines matching regexp: ")
		 nil
		 nil
		 nil
		 'regexp-history
		 default)))
	  (if (equal input "")
	      default
	    input))
	(when current-prefix-arg
	  (prefix-numeric-value current-prefix-arg))))

(defun occur-rename-buffer (&optional unique-p interactive-p)
  "Rename the current *Occur* buffer to *Occur: original-buffer-name*.
Here `original-buffer-name' is the buffer name were Occur was originally run.
When given the prefix argument, or called non-interactively, the renaming
will not clobber the existing buffer(s) of that name, but use
`generate-new-buffer-name' instead.  You can add this to `occur-hook'
if you always want a separate *Occur* buffer for each buffer where you
invoke `occur'."
  (interactive "P\np")
  (with-current-buffer
      (if (eq major-mode 'occur-mode) (current-buffer) (get-buffer "*Occur*"))
    (rename-buffer (concat "*Occur: "
                           (mapconcat #'buffer-name
                                      (car (cddr occur-revert-arguments)) "/")
                           "*")
                   (or unique-p (not interactive-p)))))

(defun occur (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.
This function can not handle matches that span more than one line.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\<occur-mode-map>\\[describe-mode] in that buffer will explain how.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive."
  (interactive (occur-read-primary-args))
  (occur-1 regexp nlines (list (current-buffer))))

(defun multi-occur (bufs regexp &optional nlines)
  "Show all lines in buffers BUFS containing a match for REGEXP.
This function acts on multiple buffers; otherwise, it is exactly like
`occur'."
  (interactive
   (cons
    (let* ((bufs (list (read-buffer "First buffer to search: "
				    (current-buffer) t)))
	   (buf nil)
	   (ido-ignore-item-temp-list bufs))
      (while (not (string-equal
		   (setq buf (read-buffer
			      (if (eq read-buffer-function 'ido-read-buffer)
				  "Next buffer to search (C-j to end): "
				"Next buffer to search (RET to end): ")
			      nil t))
		   ""))
	(add-to-list 'bufs buf)
	(setq ido-ignore-item-temp-list bufs))
      (nreverse (mapcar #'get-buffer bufs)))
    (occur-read-primary-args)))
  (occur-1 regexp nlines bufs))

(defun multi-occur-by-filename-regexp (bufregexp regexp &optional nlines)
  "Show all lines matching REGEXP in buffers named by BUFREGEXP.
See also `multi-occur'."
  (interactive
   (cons
    (let* ((default (car regexp-history))
	   (input
	    (read-from-minibuffer
	     "List lines in buffers whose filename matches regexp: "
	     nil
	     nil
	     nil
	     'regexp-history)))
      (if (equal input "")
	  default
	input))
    (occur-read-primary-args)))
  (when bufregexp
    (occur-1 regexp nlines
	     (delq nil
		   (mapcar (lambda (buf)
			     (when (and (buffer-file-name buf)
					(string-match bufregexp
						      (buffer-file-name buf)))
			       buf))
			   (buffer-list))))))

(defun occur-1 (regexp nlines bufs &optional buf-name)
  (unless buf-name
    (setq buf-name "*Occur*"))
  (let (occur-buf
	(active-bufs (delq nil (mapcar #'(lambda (buf)
					   (when (buffer-live-p buf) buf))
				       bufs))))
    ;; Handle the case where one of the buffers we're searching is the
    ;; output buffer.  Just rename it.
    (when (member buf-name (mapcar 'buffer-name active-bufs))
      (with-current-buffer (get-buffer buf-name)
	(rename-uniquely)))

    ;; Now find or create the output buffer.
    ;; If we just renamed that buffer, we will make a new one here.
    (setq occur-buf (get-buffer-create buf-name))

    (with-current-buffer occur-buf
      (occur-mode)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(let ((count (occur-engine
		      regexp active-bufs occur-buf
		      (or nlines list-matching-lines-default-context-lines)
		      (and case-fold-search
			   (isearch-no-upper-case-p regexp t))
		      list-matching-lines-buffer-name-face
		      nil list-matching-lines-face t)))
	  (let* ((bufcount (length active-bufs))
		 (diff (- (length bufs) bufcount)))
	    (message "Searched %d buffer%s%s; %s match%s for `%s'"
		     bufcount (if (= bufcount 1) "" "s")
		     (if (zerop diff) "" (format " (%d killed)" diff))
		     (if (zerop count) "no" (format "%d" count))
		     (if (= count 1) "" "es")
		     regexp))
	  (setq occur-revert-arguments (list regexp nlines bufs))
          (if (= count 0)
              (kill-buffer occur-buf)
            (display-buffer occur-buf)
            (setq next-error-last-buffer occur-buf)
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (run-hooks 'occur-hook)))))))

(defun occur-engine-add-prefix (lines)
  (mapcar
   #'(lambda (line)
       (concat "       :" line "\n"))
   lines))

(defun occur-engine (regexp buffers out-buf nlines case-fold-search
			    title-face prefix-face match-face keep-props)
  (with-current-buffer out-buf
    (let ((globalcount 0)
	  ;; Don't generate undo entries for creation of the initial contents.
	  (buffer-undo-list t)
	  (coding nil))
      ;; Map over all the buffers
      (dolist (buf buffers)
	(when (buffer-live-p buf)
	  (let ((matches 0)	;; count of matched lines
		(lines 1)	;; line count
		(matchbeg 0)
		(origpt nil)
		(begpt nil)
		(endpt nil)
		(marker nil)
		(curstring "")
		(headerpt (with-current-buffer out-buf (point))))
	    (save-excursion
	      (set-buffer buf)
	      (or coding
		  ;; Set CODING only if the current buffer locally
		  ;; binds buffer-file-coding-system.
		  (not (local-variable-p 'buffer-file-coding-system))
		  (setq coding buffer-file-coding-system))
	      (save-excursion
		(goto-char (point-min)) ;; begin searching in the buffer
		(while (not (eobp))
		  (setq origpt (point))
		  (when (setq endpt (re-search-forward regexp nil t))
		    (setq matches (1+ matches)) ;; increment match count
		    (setq matchbeg (match-beginning 0))
		    (setq lines (+ lines (1- (count-lines origpt endpt))))
		    (save-excursion
		      (goto-char matchbeg)
		      (setq begpt (line-beginning-position)
			    endpt (line-end-position)))
		    (setq marker (make-marker))
		    (set-marker marker matchbeg)
		    (if (and keep-props
			     (if (boundp 'jit-lock-mode) jit-lock-mode)
			     (text-property-not-all begpt endpt 'fontified t))
			(if (fboundp 'jit-lock-fontify-now)
			    (jit-lock-fontify-now begpt endpt)))
		    (setq curstring (buffer-substring begpt endpt))
		    ;; Depropertize the string, and maybe
		    ;; highlight the matches
		    (let ((len (length curstring))
			  (start 0))
		      (unless keep-props
			(set-text-properties 0 len nil curstring))
		      (while (and (< start len)
				  (string-match regexp curstring start))
			(add-text-properties
			 (match-beginning 0) (match-end 0)
			 (append
			  `(occur-match t)
			  (when match-face
			    ;; Use `face' rather than `font-lock-face' here
			    ;; so as to override faces copied from the buffer.
			    `(face ,match-face)))
			 curstring)
			(setq start (match-end 0))))
		    ;; Generate the string to insert for this match
		    (let* ((out-line
			    (concat
			     ;; Using 7 digits aligns tabs properly.
			     (apply #'propertize (format "%7d:" lines)
				    (append
				     (when prefix-face
				       `(font-lock-face prefix-face))
				     '(occur-prefix t)))
			     ;; We don't put `mouse-face' on the newline,
			     ;; because that loses.  And don't put it
			     ;; on context lines to reduce flicker.
			     (propertize curstring 'mouse-face 'highlight)
			     "\n"))
			   (data
			    (if (= nlines 0)
				;; The simple display style
				out-line
			      ;; The complex multi-line display
			      ;; style.  Generate a list of lines,
			      ;; concatenate them all together.
			      (apply #'concat
				     (nconc
				      (occur-engine-add-prefix (nreverse (cdr (occur-accumulate-lines (- (1+ (abs nlines))) keep-props))))
				      (list out-line)
				      (if (> nlines 0)
					  (occur-engine-add-prefix
					   (cdr (occur-accumulate-lines (1+ nlines) keep-props)))))))))
		      ;; Actually insert the match display data
		      (with-current-buffer out-buf
			(let ((beg (point))
			      (end (progn (insert data) (point))))
			  (unless (= nlines 0)
			    (insert "-------\n"))
			  (add-text-properties
			   beg end
			   `(occur-target ,marker help-echo "mouse-2: go to this occurrence")))))
		    (goto-char endpt))
		  (if endpt
		      (progn
			(setq lines (1+ lines))
			;; On to the next match...
			(forward-line 1))
		    (goto-char (point-max))))))
	    (when (not (zerop matches)) ;; is the count zero?
	      (setq globalcount (+ globalcount matches))
	      (with-current-buffer out-buf
		(goto-char headerpt)
		(let ((beg (point))
		      end)
		  (insert (format "%d match%s for \"%s\" in buffer: %s\n"
				  matches (if (= matches 1) "" "es")
				  regexp (buffer-name buf)))
		  (setq end (point))
		  (add-text-properties beg end
				       (append
					(when title-face
					  `(font-lock-face ,title-face))
					`(occur-title ,buf))))
		(goto-char (point-min)))))))
      (if coding
	  ;; CODING is buffer-file-coding-system of the first buffer
	  ;; that locally binds it.  Let's use it also for the output
	  ;; buffer.
	  (set-buffer-file-coding-system coding))
      ;; Return the number of matches
      globalcount)))


;; It would be nice to use \\[...], but there is no reasonable way
;; to make that display both SPC and Y.
(defconst query-replace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
RET or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r to enter recursive edit (\\[exit-recursive-edit] to get out again),
C-w to delete match and recursive edit,
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ to move point back to previous match,
E to edit the replacement string"
  "Help message while in `query-replace'.")

(defvar query-replace-map (make-sparse-keymap)
  "Keymap that defines the responses to questions in `query-replace'.
The \"bindings\" in this map are not commands; they are answers.
The valid answers include `act', `skip', `act-and-show',
`exit', `act-and-exit', `edit', `delete-and-edit', `recenter',
`automatic', `backup', `exit-prefix', and `help'.")

(define-key query-replace-map " " 'act)
(define-key query-replace-map "\d" 'skip)
(define-key query-replace-map [delete] 'skip)
(define-key query-replace-map [backspace] 'skip)
(define-key query-replace-map "y" 'act)
(define-key query-replace-map "n" 'skip)
(define-key query-replace-map "Y" 'act)
(define-key query-replace-map "N" 'skip)
(define-key query-replace-map "e" 'edit-replacement)
(define-key query-replace-map "E" 'edit-replacement)
(define-key query-replace-map "," 'act-and-show)
(define-key query-replace-map "q" 'exit)
(define-key query-replace-map "\r" 'exit)
(define-key query-replace-map [return] 'exit)
(define-key query-replace-map "." 'act-and-exit)
(define-key query-replace-map "\C-r" 'edit)
(define-key query-replace-map "\C-w" 'delete-and-edit)
(define-key query-replace-map "\C-l" 'recenter)
(define-key query-replace-map "!" 'automatic)
(define-key query-replace-map "^" 'backup)
(define-key query-replace-map "\C-h" 'help)
(define-key query-replace-map [f1] 'help)
(define-key query-replace-map [help] 'help)
(define-key query-replace-map "?" 'help)
(define-key query-replace-map "\C-g" 'quit)
(define-key query-replace-map "\C-]" 'quit)
(define-key query-replace-map "\e" 'exit-prefix)
(define-key query-replace-map [escape] 'exit-prefix)

(defun replace-match-string-symbols (n)
  "Process a list (and any sub-lists), expanding certain symbols.
Symbol  Expands To
N     (match-string N)           (where N is a string of digits)
#N    (string-to-number (match-string N))
&     (match-string 0)
#&    (string-to-number (match-string 0))
#     replace-count

Note that these symbols must be preceeded by a backslash in order to
type them."
  (while n
    (cond
     ((consp (car n))
      (replace-match-string-symbols (car n))) ;Process sub-list
     ((symbolp (car n))
      (let ((name (symbol-name (car n))))
        (cond
         ((string-match "^[0-9]+$" name)
          (setcar n (list 'match-string (string-to-number name))))
         ((string-match "^#[0-9]+$" name)
          (setcar n (list 'string-to-number
                          (list 'match-string
                                (string-to-number (substring name 1))))))
         ((string= "&" name)
          (setcar n '(match-string 0)))
         ((string= "#&" name)
          (setcar n '(string-to-number (match-string 0))))
	 ((string= "#" name)
	  (setcar n 'replace-count))))))
    (setq n (cdr n))))

(defun replace-eval-replacement (expression replace-count)
  (let ((replacement (eval expression)))
    (if (stringp replacement)
        replacement
      (prin1-to-string replacement t))))

(defun replace-quote (replacement)
  "Quote a replacement string.
This just doubles all backslashes in REPLACEMENT and
returns the resulting string.  If REPLACEMENT is not
a string, it is first passed through `prin1-to-string'
with the `noescape' argument set.

`match-data' is preserved across the call."
  (save-match-data
    (replace-regexp-in-string "\\\\" "\\\\"
			      (if (stringp replacement)
				  replacement
				(prin1-to-string replacement t))
			      t t)))

(defun replace-loop-through-replacements (data replace-count)
  ;; DATA is a vector contaning the following values:
  ;;   0 next-rotate-count
  ;;   1 repeat-count
  ;;   2 next-replacement
  ;;   3 replacements
  (if (= (aref data 0) replace-count)
      (progn
        (aset data 0 (+ replace-count (aref data 1)))
        (let ((next (cdr (aref data 2))))
          (aset data 2 (if (consp next) next (aref data 3))))))
  (car (aref data 2)))

(defun replace-match-data (integers reuse &optional new)
  "Like `match-data', but markers in REUSE get invalidated.
If NEW is non-NIL, it is set and returned instead of fresh data,
but coerced to the correct value of INTEGERS."
  (or (and new
	   (progn
	     (set-match-data new)
	     (and (eq new reuse)
		  (eq (null integers) (markerp (car reuse)))
		  new)))
      (match-data integers reuse t)))

(defun replace-match-maybe-edit (newtext fixedcase literal noedit match-data)
  "Make a replacement with `replace-match', editing `\\?'.
NEWTEXT, FIXEDCASE, LITERAL are just passed on.  If NOEDIT is true, no
check for `\\?' is made to save time.  MATCH-DATA is used for the
replacement.  In case editing is done, it is changed to use markers.

The return value is non-NIL if there has been no `\\?' or NOEDIT was
passed in.  If LITERAL is set, no checking is done, anyway."
  (unless (or literal noedit)
    (setq noedit t)
    (while (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\\\?\\)"
			 newtext)
      (setq newtext
	    (read-string "Edit replacement string: "
                         (prog1
                             (cons
                              (replace-match "" t t newtext 3)
                              (1+ (match-beginning 3)))
                           (setq match-data
                                 (replace-match-data
                                  nil match-data match-data))))
	    noedit nil)))
  (set-match-data match-data)
  (replace-match newtext fixedcase literal)
  noedit)

(defun perform-replace (from-string replacements
		        query-flag regexp-flag delimited-flag
			&optional repeat-count map start end)
  "Subroutine of `query-replace'.  Its complexity handles interactive queries.
Don't use this in your own program unless you want to query and set the mark
just as `query-replace' does.  Instead, write a simple loop like this:

  (while (re-search-forward \"foo[ \\t]+bar\" nil t)
    (replace-match \"foobar\" nil nil))

which will run faster and probably do exactly what you want.  Please
see the documentation of `replace-match' to find out how to simulate
`case-replace'.

This function returns nil if and only if there were no matches to
make, or the user didn't cancel the call."
  (or map (setq map query-replace-map))
  (and query-flag minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let ((nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	(case-fold-search (and case-fold-search
			       (string-equal from-string
					     (downcase from-string))))
	(literal (or (not regexp-flag) (eq regexp-flag 'literal)))
	(search-function (if regexp-flag 're-search-forward 'search-forward))
	(search-string from-string)
	(real-match-data nil)		; the match data for the current match
	(next-replacement nil)
	(noedit nil)
	(keep-going t)
	(stack nil)
	(replace-count 0)
	(nonempty-match nil)

	;; If non-nil, it is marker saying where in the buffer to stop.
	(limit nil)

	;; Data for the next match.  If a cons, it has the same format as
	;; (match-data); otherwise it is t if a match is possible at point.
	(match-again t)

	(message
	 (if query-flag
	     (substitute-command-keys
	      "Query replacing %s with %s: (\\<query-replace-map>\\[help] for help) "))))

    ;; If region is active, in Transient Mark mode, operate on region.
    (when start
      (setq limit (copy-marker (max start end)))
      (goto-char (min start end))
      (deactivate-mark))

    ;; REPLACEMENTS is either a string, a list of strings, or a cons cell
    ;; containing a function and its first argument.  The function is
    ;; called to generate each replacement like this:
    ;;   (funcall (car replacements) (cdr replacements) replace-count)
    ;; It must return a string.
    (cond
     ((stringp replacements)
      (setq next-replacement replacements
            replacements     nil))
     ((stringp (car replacements)) ; If it isn't a string, it must be a cons
      (or repeat-count (setq repeat-count 1))
      (setq replacements (cons 'replace-loop-through-replacements
                               (vector repeat-count repeat-count
                                       replacements replacements)))))

    (if delimited-flag
	(setq search-function 're-search-forward
	      search-string (concat "\\b"
				    (if regexp-flag from-string
				      (regexp-quote from-string))
				    "\\b")))
    (when query-replace-lazy-highlight
      (setq isearch-lazy-highlight-last-string nil))

    (push-mark)
    (undo-boundary)
    (unwind-protect
	;; Loop finding occurrences that perhaps should be replaced.
	(while (and keep-going
		    (not (or (eobp) (and limit (>= (point) limit))))
		    ;; Use the next match if it is already known;
		    ;; otherwise, search for a match after moving forward
		    ;; one char if progress is required.
		    (setq real-match-data
			  (if (consp match-again)
			      (progn (goto-char (nth 1 match-again))
				     (replace-match-data t
				      real-match-data
				      match-again))
			    (and (or match-again
				     ;; MATCH-AGAIN non-nil means we
				     ;; accept an adjacent match.  If
				     ;; we don't, move one char to the
				     ;; right.  This takes us a
				     ;; character too far at the end,
				     ;; but this is undone after the
				     ;; while-loop.
				     (progn
				       (forward-char 1)
				       (not (or (eobp)
						(and limit (>= (point) limit))))))
				 (funcall search-function search-string limit t)
				 ;; For speed, use only integers and
				 ;; reuse the list used last time.
				 (replace-match-data t real-match-data)))))
	  ;; Optionally ignore matches that have a read-only property.
	  (unless (and query-replace-skip-read-only
		       (text-property-not-all
			(match-beginning 0) (match-end 0)
			'read-only nil))

	    ;; Record whether the match is nonempty, to avoid an infinite loop
	    ;; repeatedly matching the same empty string.
	    (setq nonempty-match
		  (/= (nth 0 real-match-data) (nth 1 real-match-data)))

	    ;; If the match is empty, record that the next one can't be
	    ;; adjacent.

	    ;; Otherwise, if matching a regular expression, do the next
	    ;; match now, since the replacement for this match may
	    ;; affect whether the next match is adjacent to this one.
	    ;; If that match is empty, don't use it.
	    (setq match-again
		  (and nonempty-match
		       (or (not regexp-flag)
			   (and (looking-at search-string)
				(let ((match (match-data)))
				  (and (/= (nth 0 match) (nth 1 match))
				       match))))))

	    ;; Calculate the replacement string, if necessary.
	    (when replacements
	      (set-match-data real-match-data)
	      (setq next-replacement
		    (funcall (car replacements) (cdr replacements)
			     replace-count)
		    noedit nil))
	    (if (not query-flag)
		(let ((inhibit-read-only
		       query-replace-skip-read-only))
		  (unless (or literal noedit)
		    (replace-highlight
		     (nth 0 real-match-data) (nth 1 real-match-data)
		     start end search-string
		     (or delimited-flag regexp-flag) case-fold-search))
		  (setq noedit
			(replace-match-maybe-edit
			 next-replacement nocasify literal
			 noedit real-match-data)
			replace-count (1+ replace-count)))
	      (undo-boundary)
	      (let (done replaced key def)
		;; Loop reading commands until one of them sets done,
		;; which means it has finished handling this
		;; occurrence.  Any command that sets `done' should
		;; leave behind proper match data for the stack.
		;; Commands not setting `done' need to adjust
		;; `real-match-data'.
		(while (not done)
		  (set-match-data real-match-data)
		  (replace-highlight
		   (match-beginning 0) (match-end 0)
		   start end search-string
		   (or delimited-flag regexp-flag) case-fold-search)
		  ;; Bind message-log-max so we don't fill up the message log
		  ;; with a bunch of identical messages.
		  (let ((message-log-max nil))
		    (message message
                             (query-replace-descr from-string)
                             (query-replace-descr next-replacement)))
		  (setq key (read-event))
		  ;; Necessary in case something happens during read-event
		  ;; that clobbers the match data.
		  (set-match-data real-match-data)
		  (setq key (vector key))
		  (setq def (lookup-key map key))
		  ;; Restore the match data while we process the command.
		  (cond ((eq def 'help)
			 (with-output-to-temp-buffer "*Help*"
			   (princ
			    (concat "Query replacing "
				    (if regexp-flag "regexp " "")
				    from-string " with "
				    next-replacement ".\n\n"
				    (substitute-command-keys
				     query-replace-help)))
			   (with-current-buffer standard-output
			     (help-mode))))
			((eq def 'exit)
			 (setq keep-going nil)
			 (setq done t))
			((eq def 'backup)
			 (if stack
			     (let ((elt (pop stack)))
			       (goto-char (nth 0 elt))
			       (setq replaced (nth 1 elt)
				     real-match-data
				     (replace-match-data
				      t real-match-data
				      (nth 2 elt))))
			   (message "No previous match")
			   (ding 'no-terminate)
			   (sit-for 1)))
			((eq def 'act)
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data)
				   replace-count (1+ replace-count)))
			 (setq done t replaced t))
			((eq def 'act-and-exit)
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data)
				   replace-count (1+ replace-count)))
			 (setq keep-going nil)
			 (setq done t replaced t))
			((eq def 'act-and-show)
			 (if (not replaced)
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data)
				   replace-count (1+ replace-count)
				   real-match-data (replace-match-data
						    t real-match-data)
				   replaced t)))
			((eq def 'automatic)
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data)
				   replace-count (1+ replace-count)))
			 (setq done t query-flag nil replaced t))
			((eq def 'skip)
			 (setq done t))
			((eq def 'recenter)
			 (recenter nil))
			((eq def 'edit)
			 (let ((opos (point-marker)))
			   (setq real-match-data (replace-match-data
						  nil real-match-data
						  real-match-data))
			   (goto-char (match-beginning 0))
			   (save-excursion
			     (save-window-excursion
			       (recursive-edit)))
			   (goto-char opos)
			   (set-marker opos nil))
			 ;; Before we make the replacement,
			 ;; decide whether the search string
			 ;; can match again just after this match.
			 (if (and regexp-flag nonempty-match)
			     (setq match-again (and (looking-at search-string)
						    (match-data)))))
			;; Edit replacement.
			((eq def 'edit-replacement)
			 (setq real-match-data (replace-match-data
						nil real-match-data
						real-match-data)
			       next-replacement
			       (read-string "Edit replacement string: "
                                            next-replacement)
			       noedit nil)
			 (if replaced
			     (set-match-data real-match-data)
			   (setq noedit
				 (replace-match-maybe-edit
				  next-replacement nocasify literal noedit
				  real-match-data)
				 replaced t))
			 (setq done t))

			((eq def 'delete-and-edit)
			 (replace-match "" t t)
			 (setq real-match-data (replace-match-data
						nil real-match-data))
			 (replace-dehighlight)
			 (save-excursion (recursive-edit))
			 (setq replaced t))
			;; Note: we do not need to treat `exit-prefix'
			;; specially here, since we reread
			;; any unrecognized character.
			(t
			 (setq this-command 'mode-exited)
			 (setq keep-going nil)
			 (setq unread-command-events
			       (append (listify-key-sequence key)
				       unread-command-events))
			 (setq done t)))
		  (when query-replace-lazy-highlight
		    ;; Force lazy rehighlighting only after replacements
		    (if (not (memq def '(skip backup)))
			(setq isearch-lazy-highlight-last-string nil))))
		;; Record previous position for ^ when we move on.
		;; Change markers to numbers in the match data
		;; since lots of markers slow down editing.
		(push (list (point) replaced
;;;  If the replacement has already happened, all we need is the
;;;  current match start and end.  We could get this with a trivial
;;;  match like
;;;  (save-excursion (goto-char (match-beginning 0))
;;;		     (search-forward (match-string 0))
;;;                  (match-data t))
;;;  if we really wanted to avoid manually constructing match data.
;;;  Adding current-buffer is necessary so that match-data calls can
;;;  return markers which are appropriate for editing.
			    (if replaced
				(list
				 (match-beginning 0)
				 (match-end 0)
				 (current-buffer))
			      (match-data t)))
		      stack)))))

      ;; The code preventing adjacent regexp matches in the condition
      ;; of the while-loop above will haven taken us one character
      ;; beyond the last replacement.  Undo that.
      (when (and regexp-flag (not match-again) (> replace-count 0))
	(backward-char 1))

      (replace-dehighlight))
    (or unread-command-events
	(message "Replaced %d occurrence%s"
		 replace-count
		 (if (= replace-count 1) "" "s")))
    (and keep-going stack)))

(defvar replace-overlay nil)

(defun replace-highlight (match-beg match-end range-beg range-end
			  string regexp case-fold)
  (if query-replace-highlight
      (if replace-overlay
	  (move-overlay replace-overlay match-beg match-end (current-buffer))
	(setq replace-overlay (make-overlay match-beg match-end))
	(overlay-put replace-overlay 'priority 1) ;higher than lazy overlays
	(overlay-put replace-overlay 'face 'query-replace)))
  (if query-replace-lazy-highlight
      (let ((isearch-string string)
	    (isearch-regexp regexp)
	    (isearch-case-fold-search case-fold))
	(isearch-lazy-highlight-new-loop range-beg range-end))))

(defun replace-dehighlight ()
  (when replace-overlay
    (delete-overlay replace-overlay))
  (when query-replace-lazy-highlight
    (lazy-highlight-cleanup lazy-highlight-cleanup)
    (setq isearch-lazy-highlight-last-string nil)))

;; arch-tag: 16b4cd61-fd40-497b-b86f-b667c4cf88e4
;;; replace.el ends here
