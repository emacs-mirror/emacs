;;; newcomment.el --- (un)comment regions of buffers

;; Copyright (C) 1999, 2000  Free Software Foundation Inc.

;; Author: code extracted from Emacs-20's simple.el
;; Maintainer: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: comment uncomment
;; Revision: $Id: newcomment.el,v 1.44 2002/01/13 09:59:39 pj Exp $

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

;; A replacement for simple.el's comment-related functions.

;;; Bugs:

;; - nested comments in sgml-mode are not properly quoted.
;; - single-char nestable comment-start can only do the "\\s<+" stuff
;;   if the corresponding closing marker happens to be right.
;; - comment-box in TeXinfo generate bogus comments @ccccc@
;; - uncomment-region with a numeric argument can render multichar
;;   comment markers invalid.
;; - comment-indent or comment-region when called inside a comment
;;   will happily break the surrounding comment.
;; - comment-quote-nested will not (un)quote properly all nested comment
;;   markers if there are more than just comment-start and comment-end.
;;   For example, in Pascal where {...*) and (*...} are possible.

;;; Todo:

;; - quantized steps in comment-alignment
;; - try to align tail comments
;; - check what c-comment-line-break-function has to say
;; - spill auto-fill of comments onto the end of the next line
;; - uncomment-region with a consp (for blocks) or somehow make the
;;   deletion of continuation markers less dangerous
;; - drop block-comment-<foo> unless it's really used
;; - uncomment-region on a subpart of a comment
;; - support gnu-style "multi-line with space in continue"
;; - somehow allow comment-dwim to use the region even if transient-mark-mode
;;   is not turned on.

;; - when auto-filling a comment, try to move the comment to the left
;;   rather than break it (if possible).
;; - sometimes default the comment-column to the same
;;   one used on the preceding line(s).

;;; Code:

;;;###autoload
(defalias 'indent-for-comment 'comment-indent)
;;;###autoload
(defalias 'set-comment-column 'comment-set-column)
;;;###autoload
(defalias 'kill-comment 'comment-kill)
;;;###autoload
(defalias 'indent-new-comment-line 'comment-indent-new-line)

;;;###autoload
(defgroup comment nil
  "Indenting and filling of comments."
  :prefix "comment-"
  :version "21.1"
  :group 'fill)

(defvar comment-use-syntax 'undecided
  "Non-nil if syntax-tables can be used instead of regexps.
Can also be `undecided' which means that a somewhat expensive test will
be used to try to determine whether syntax-tables should be trusted
to understand comments or not in the given buffer.
Major modes should set this variable.")

;;;###autoload
(defcustom comment-column 32
  "*Column to indent right-margin comments to.
Setting this variable automatically makes it local to the current buffer.
Each mode establishes a different default value for this variable; you
can set the value for a particular mode using that mode's hook."
  :type 'integer)
(make-variable-buffer-local 'comment-column)

;;;###autoload
(defvar comment-start nil
  "*String to insert to start a new comment, or nil if no comment syntax.")

;;;###autoload
(defvar comment-start-skip nil
  "*Regexp to match the start of a comment plus everything up to its body.
If there are any \\(...\\) pairs, the comment delimiter text is held to begin
at the place matched by the close of the first pair.")

;;;###autoload
(defvar comment-end-skip nil
  "Regexp to match the end of a comment plus everything up to its body.")

;;;###autoload
(defvar comment-end ""
  "*String to insert to end a new comment.
Should be an empty string if comments are terminated by end-of-line.")

;;;###autoload
(defvar comment-indent-function 'comment-indent-default
  "Function to compute desired indentation for a comment.
This function is called with no args with point at the beginning of
the comment's starting delimiter and should return either the desired
column indentation or nil.
If nil is returned, indentation is delegated to `indent-according-to-mode'.")

(defvar block-comment-start nil)
(defvar block-comment-end nil)

(defvar comment-quote-nested t
  "Non-nil if nested comments should be quoted.
This should be locally set by each major mode if needed.")

(defvar comment-continue nil
  "Continuation string to insert for multiline comments.
This string will be added at the beginning of each line except the very
first one when commenting a region with a commenting style that allows
comments to span several lines.
It should generally have the same length as `comment-start' in order to
preserve indentation.
If it is nil a value will be automatically derived from `comment-start'
by replacing its first character with a space.")

(defvar comment-add 0
  "How many more comment chars should be inserted by `comment-region'.
This determines the default value of the numeric argument of `comment-region'.
This should generally stay 0, except for a few modes like Lisp where
it can be convenient to set it to 1 so that regions are commented with
two semi-colons.")

(defconst comment-styles
  '((plain	. (nil nil nil nil))
    (indent	. (nil nil nil t))
    (aligned	. (nil t nil t))
    (multi-line	. (t nil nil t))
    (extra-line	. (t nil t t))
    (box	. (nil t t t))
    (box-multi	. (t t t t)))
  "Possible comment styles of the form (STYLE . (MULTI ALIGN EXTRA INDENT)).
STYLE should be a mnemonic symbol.
MULTI specifies that comments are allowed to span multiple lines.
ALIGN specifies that the `comment-end' markers should be aligned.
EXTRA specifies that an extra line should be used before and after the
  region to comment (to put the `comment-end' and `comment-start').
INDENT specifies that the `comment-start' markers should not be put at the
  left margin but at the current indentation of the region to comment.")

;;;###autoload
(defcustom comment-style 'plain
  "*Style to be used for `comment-region'.
See `comment-styles' for a list of available styles."
  :type (if (boundp 'comment-styles)
	    `(choice ,@(mapcar (lambda (s) `(const ,(car s))) comment-styles))
	  'symbol))

;;;###autoload
(defcustom comment-padding " "
  "Padding string that `comment-region' puts between comment chars and text.
Can also be an integer which will be automatically turned into a string
of the corresponding number of spaces.

Extra spacing between the comment characters and the comment text
makes the comment easier to read.  Default is 1.  nil means 0."
  :type '(choice string integer (const nil)))

;;;###autoload
(defcustom comment-multi-line nil
  "*Non-nil means \\[comment-indent-new-line] continues comments, with no new terminator or starter.
This is obsolete because you might as well use \\[newline-and-indent]."
  :type 'boolean)

;;;;
;;;; Helpers
;;;;

(defun comment-string-strip (str beforep afterp)
  "Strip STR of any leading (if BEFOREP) and/or trailing (if AFTERP) space."
  (string-match (concat "\\`" (if beforep "\\s-*")
			"\\(.*?\\)" (if afterp "\\s-*\n?")
			"\\'") str)
  (match-string 1 str))

(defun comment-string-reverse (s)
  "Return the mirror image of string S, without any trailing space."
  (comment-string-strip (concat (nreverse (string-to-list s))) nil t))

;;;###autoload
(defun comment-normalize-vars (&optional noerror)
  (if (not comment-start) (or noerror (error "No comment syntax is defined"))
    ;; comment-use-syntax
    (when (eq comment-use-syntax 'undecided)
      (set (make-local-variable 'comment-use-syntax)
	   (let ((st (syntax-table))
		 (cs comment-start)
		 (ce (if (string= "" comment-end) "\n" comment-end)))
	     ;; Try to skip over a comment using forward-comment
	     ;; to see if the syntax tables properly recognize it.
	     (with-temp-buffer
	       (set-syntax-table st)
	       (insert cs " hello " ce)
	       (goto-char (point-min))
	       (and (forward-comment 1) (eobp))))))
    ;; comment-padding
    (unless comment-padding (setq comment-padding 0))
    (when (integerp comment-padding)
      (setq comment-padding (make-string comment-padding ? )))
    ;; comment markers
    ;;(setq comment-start (comment-string-strip comment-start t nil))
    ;;(setq comment-end (comment-string-strip comment-end nil t))
    ;; comment-continue
    (unless (or comment-continue (string= comment-end ""))
      (set (make-local-variable 'comment-continue)
	   (concat (if (string-match "\\S-\\S-" comment-start) " " "|")
		   (substring comment-start 1)))
      ;; Hasn't been necessary yet.
      ;; (unless (string-match comment-start-skip comment-continue)
      ;; 	(kill-local-variable 'comment-continue))
      )
    ;; comment-skip regexps
    (unless comment-start-skip
      (set (make-local-variable 'comment-start-skip)
	   (concat "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(\\s<+\\|"
		   (regexp-quote (comment-string-strip comment-start t t))
		   ;; Let's not allow any \s- but only [ \t] since \n
		   ;; might be both a comment-end marker and \s-.
		   "+\\)[ \t]*")))
    (unless comment-end-skip
      (let ((ce (if (string= "" comment-end) "\n"
		  (comment-string-strip comment-end t t))))
	(set (make-local-variable 'comment-end-skip)
	     ;; We use [ \t] rather than \s- because we don't want to
	     ;; remove ^L in C mode when uncommenting.
	     (concat "[ \t]*\\(\\s>" (if comment-quote-nested "" "+")
		     "\\|" (regexp-quote (substring ce 0 1))
		     (if (and comment-quote-nested (<= (length ce) 1)) "" "+")
		     (regexp-quote (substring ce 1))
		     "\\)"))))))
 
(defun comment-quote-re (str unp)
  (concat (regexp-quote (substring str 0 1))
	  "\\\\" (if unp "+" "*")
	  (regexp-quote (substring str 1))))

(defun comment-quote-nested (cs ce unp)
  "Quote or unquote nested comments.
If UNP is non-nil, unquote nested comment markers."
  (setq cs (comment-string-strip cs t t))
  (setq ce (comment-string-strip ce t t))
  (when (and comment-quote-nested (> (length ce) 0))
    (let ((re (concat (comment-quote-re ce unp)
		      "\\|" (comment-quote-re cs unp))))
      (goto-char (point-min))
      (while (re-search-forward re nil t)
	(goto-char (match-beginning 0))
	(forward-char 1)
	(if unp (delete-char 1) (insert "\\"))
	(when (= (length ce) 1)
	  ;; If the comment-end is a single char, adding a \ after that
	  ;; "first" char won't deactivate it, so we turn such a CE
	  ;; into !CS.  I.e. for pascal, we turn } into !{
	  (if (not unp)
	      (when (string= (match-string 0) ce)
		(replace-match (concat "!" cs) t t))
	    (when (and (< (point-min) (match-beginning 0))
		       (string= (buffer-substring (1- (match-beginning 0))
						  (1- (match-end 0)))
				(concat "!" cs)))
	      (backward-char 2)
	      (delete-char (- (match-end 0) (match-beginning 0)))
	      (insert ce))))))))

;;;;
;;;; Navigation
;;;;

(defun comment-search-forward (limit &optional noerror)
  "Find a comment start between point and LIMIT.
Moves point to inside the comment and returns the position of the
comment-starter.  If no comment is found, moves point to LIMIT
and raises an error or returns nil of NOERROR is non-nil."
  (if (not comment-use-syntax)
      (if (re-search-forward comment-start-skip limit noerror)
	  (or (match-end 1) (match-beginning 0))
	(goto-char limit)
	(unless noerror (error "No comment")))
    (let* ((pt (point))
	   ;; Assume (at first) that pt is outside of any string.
	   (s (parse-partial-sexp pt (or limit (point-max)) nil nil nil t)))
      (when (and (nth 8 s) (nth 3 s))
	  ;; The search ended inside a string.  Try to see if it
	  ;; works better when we assume that pt is inside a string.
	  (setq s (parse-partial-sexp
		   pt (or limit (point-max)) nil nil
		   (list nil nil nil (nth 3 s) nil nil nil nil)
		   t)))
      (if (not (and (nth 8 s) (not (nth 3 s))))
	  (unless noerror (error "No comment"))
	;; We found the comment.
	(let ((pos (point))
	      (start (nth 8 s))
	      (bol (line-beginning-position))
	      (end nil))
	  (while (and (null end) (>= (point) bol))
	    (if (looking-at comment-start-skip)
		(setq end (min (or limit (point-max)) (match-end 0)))
	      (backward-char)))
	  (goto-char (or end pos))
	  start)))))

(defun comment-search-backward (&optional limit noerror)
  "Find a comment start between LIMIT and point.
Moves point to inside the comment and returns the position of the
comment-starter.  If no comment is found, moves point to LIMIT
and raises an error or returns nil of NOERROR is non-nil."
  ;; FIXME: If a comment-start appears inside a comment, we may erroneously
  ;; stop there.  This can be rather bad in general, but since
  ;; comment-search-backward is only used to find the comment-column (in
  ;; comment-set-column) and to find the comment-start string (via
  ;; comment-beginning) in indent-new-comment-line, it should be harmless.
  (if (not (re-search-backward comment-start-skip limit t))
      (unless noerror (error "No comment"))
    (beginning-of-line)
    (let* ((end (match-end 0))
	   (cs (comment-search-forward end t))
	   (pt (point)))
      (if (not cs)
	  (progn (beginning-of-line)
		 (comment-search-backward limit noerror))
	(while (progn (goto-char cs)
		      (comment-forward)
		      (and (< (point) end)
			   (setq cs (comment-search-forward end t))))
	  (setq pt (point)))
	(goto-char pt)
	cs))))

(defun comment-beginning ()
  "Find the beginning of the enclosing comment.
Returns nil if not inside a comment, else moves point and returns
the same as `comment-search-forward'."
  ;; HACK ATTACK!
  ;; We should really test `in-string-p' but that can be expensive.
  (unless (eq (get-text-property (point) 'face) 'font-lock-string-face)
    (let ((pt (point))
	  (cs (comment-search-backward nil t)))
      (when cs
	(if (save-excursion
	      (goto-char cs)
	      (and
	       ;; For modes where comment-start and comment-end are the same,
	       ;; the search above may have found a `ce' rather than a `cs'.
	       (or (not (looking-at comment-end-skip))
		   ;; Maybe font-lock knows that it's a `cs'?
		   (eq (get-text-property (match-end 0) 'face)
		       'font-lock-comment-face)
		   (unless (eq (get-text-property (point) 'face)
			       'font-lock-comment-face)
		     ;; Let's assume it's a `cs' if we're on the same line.
		     (>= (line-end-position) pt)))
	       ;; Make sure that PT is not past the end of the comment.
	       (if (comment-forward 1) (> (point) pt) (eobp))))
	    cs
	  (goto-char pt)
	  nil)))))

(defun comment-forward (&optional n)
  "Skip forward over N comments.
Just like `forward-comment' but only for positive N
and can use regexps instead of syntax."
  (setq n (or n 1))
  (if (< n 0) (error "No comment-backward")
    (if comment-use-syntax (forward-comment n)
      (while (> n 0)
	(setq n
	      (if (or (forward-comment 1)
		      (and (looking-at comment-start-skip)
			   (goto-char (match-end 0))
			   (re-search-forward comment-end-skip nil 'move)))
		  (1- n) -1)))
      (= n 0))))

(defun comment-enter-backward ()
  "Move from the end of a comment to the end of its content.
Point is assumed to be just at the end of a comment."
  (if (bolp)
      ;; comment-end = ""
      (progn (backward-char) (skip-syntax-backward " "))
    (let ((end (point)))
      (beginning-of-line)
      (save-restriction
	(narrow-to-region (point) end)
	(if (re-search-forward (concat comment-end-skip "\\'") nil t)
	    (goto-char (match-beginning 0))
	  ;; comment-end-skip not found probably because it was not set right.
	  ;; Since \\s> should catch the single-char case, we'll blindly
	  ;; assume we're at the end of a two-char comment-end.
	  (goto-char (point-max))
	  (backward-char 2)
	  (skip-chars-backward (string (char-after)))
	  (skip-syntax-backward " "))))))

;;;;
;;;; Commands
;;;;

;;;###autoload
(defun comment-indent-default ()
  "Default for `comment-indent-function'."
  (if (and (looking-at "\\s<\\s<\\(\\s<\\)?")
	   (or (match-end 1) (/= (current-column) (current-indentation))))
      0
    (when (or (/= (current-column) (current-indentation))
	      (and (> comment-add 0) (looking-at "\\s<\\S<")))
      comment-column)))

;;;###autoload
(defun comment-indent (&optional continue)
  "Indent this line's comment to comment column, or insert an empty comment.
If CONTINUE is non-nil, use the `comment-continue' markers if any."
  (interactive "*")
  (comment-normalize-vars)
  (let* ((empty (save-excursion (beginning-of-line)
				(looking-at "[ \t]*$")))
	 (starter (or (and continue comment-continue)
		      (and empty block-comment-start) comment-start))
	 (ender (or (and continue comment-continue "")
		    (and empty block-comment-end) comment-end)))
    (unless starter (error "No comment syntax defined"))
    (beginning-of-line)
    (let* ((eolpos (line-end-position))
	   (begpos (comment-search-forward eolpos t))
	   cpos indent)
      ;; An existing comment?
      (if begpos
	  (progn
	    (if (and (not (looking-at "[\t\n ]"))
		     (looking-at comment-end-skip))
		;; The comment is empty and we have skipped all its space
		;; and landed right before the comment-ender:
		;; Go back to the middle of the space.
		(forward-char (/ (skip-chars-backward " \t") -2)))
	    (setq cpos (point-marker)))
	  ;; If none, insert one.
	  (save-excursion
	    ;; Some comment-indent-function insist on not moving comments that
	    ;; are in column 0, so we first go to the likely target column.
	    (indent-to comment-column)
	    (setq begpos (point))
	    (insert starter)
	    (setq cpos (point-marker))
	    (insert ender)))
      (goto-char begpos)
      ;; Compute desired indent.
      (setq indent (save-excursion (funcall comment-indent-function)))
      (if (not indent)
	  ;; comment-indent-function refuses: delegate to indent.
	  (indent-according-to-mode)
	;; Avoid moving comments past the fill-column.
	(unless (save-excursion (skip-chars-backward " \t") (bolp))
	  (setq indent
		(min indent
		     (+ (current-column)
			(- fill-column
			   (save-excursion (end-of-line) (current-column)))))))
	(unless (= (current-column) indent)
	  ;; If that's different from current, change it.
	  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
	  (indent-to (if (bolp) indent
		       (max indent (1+ (current-column)))))))
      (goto-char cpos)
      (set-marker cpos nil))))

;;;###autoload
(defun comment-set-column (arg)
  "Set the comment column based on point.
With no ARG, set the comment column to the current column.
With just minus as arg, kill any comment on this line.
With any other arg, set comment column to indentation of the previous comment
 and then align or create a comment on this line at that column."
  (interactive "P")
  (cond
   ((eq arg '-) (comment-kill nil))
   (arg
    (save-excursion
      (beginning-of-line)
      (comment-search-backward)
      (beginning-of-line)
      (goto-char (comment-search-forward (line-end-position)))
      (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))
    (comment-indent))
   (t (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))))

;;;###autoload
(defun comment-kill (arg)
  "Kill the comment on this line, if any.
With prefix ARG, kill comments on that many lines starting with this one."
  (interactive "P")
  (dotimes (_ (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
	(when cs
	  (goto-char cs)
	  (skip-syntax-backward " ")
	  (setq cs (point))
	  (comment-forward)
	  (kill-region cs (if (bolp) (1- (point)) (point)))
	  (indent-according-to-mode))))
    (if arg (forward-line 1))))

(defun comment-padright (str &optional n)
  "Construct a string composed of STR plus `comment-padding'.
It also adds N copies of the last non-whitespace chars of STR.
If STR already contains padding, the corresponding amount is
ignored from `comment-padding'.
N defaults to 0.
If N is `re', a regexp is returned instead, that would match
the string for any N."
  (setq n (or n 0))
  (when (and (stringp str) (not (string= "" str)))
    ;; Separate the actual string from any leading/trailing padding
    (string-match "\\`\\s-*\\(.*?\\)\\s-*\\'" str)
    (let ((s (match-string 1 str))	;actual string
	  (lpad (substring str 0 (match-beginning 1))) ;left padding
	  (rpad (concat (substring str (match-end 1)) ;original right padding
			(substring comment-padding ;additional right padding
				   (min (- (match-end 0) (match-end 1))
					(length comment-padding)))))
	  ;; We can only duplicate C if the comment-end has multiple chars
	  ;; or if comments can be nested, else the comment-end `}' would
	  ;; be turned into `}}}' where only the first ends the comment
	  ;; and the rest becomes bogus junk.
	  (multi (not (and comment-quote-nested
			   ;; comment-end is a single char
			   (string-match "\\`\\s-*\\S-\\s-*\\'" comment-end)))))
      (if (not (symbolp n))
	  (concat lpad s (when multi (make-string n (aref str (1- (match-end 1))))) rpad)
	;; construct a regexp that would match anything from just S
	;; to any possible output of this function for any N.
	(concat (mapconcat (lambda (c) (concat (regexp-quote (string c)) "?"))
			   lpad "")	;padding is not required
		(regexp-quote s)
		(when multi "+")	;the last char of S might be repeated
		(mapconcat (lambda (c) (concat (regexp-quote (string c)) "?"))
			   rpad "")))))) ;padding is not required

(defun comment-padleft (str &optional n)
  "Construct a string composed of `comment-padding' plus STR.
It also adds N copies of the first non-whitespace chars of STR.
If STR already contains padding, the corresponding amount is
ignored from `comment-padding'.
N defaults to 0.
If N is `re', a regexp is returned instead, that would match
  the string for any N."
  (setq n (or n 0))
  (when (and (stringp str) (not (string= "" str)))
    ;; Only separate the left pad because we assume there is no right pad.
    (string-match "\\`\\s-*" str)
    (let ((s (substring str (match-end 0)))
	  (pad (concat (substring comment-padding
				  (min (- (match-end 0) (match-beginning 0))
				       (length comment-padding)))
		       (match-string 0 str)))
	  (c (aref str (match-end 0)))	;the first non-space char of STR
	  ;; We can only duplicate C if the comment-end has multiple chars
	  ;; or if comments can be nested, else the comment-end `}' would
	  ;; be turned into `}}}' where only the first ends the comment
	  ;; and the rest becomes bogus junk.
	  (multi (not (and comment-quote-nested
			   ;; comment-end is a single char
			   (string-match "\\`\\s-*\\S-\\s-*\\'" comment-end)))))
      (if (not (symbolp n))
	  (concat pad (when multi (make-string n c)) s)
	;; Construct a regexp that would match anything from just S
	;; to any possible output of this function for any N.
	;; We match any number of leading spaces because this regexp will
	;; be used for uncommenting where we might want to remove
	;; uncomment markers with arbitrary leading space (because
	;; they were aligned).
	(concat "\\s-*"
		(if multi (concat (regexp-quote (string c)) "*"))
		(regexp-quote s))))))

;;;###autoload
(defun uncomment-region (beg end &optional arg)
  "Uncomment each line in the BEG..END region.
The numeric prefix ARG can specify a number of chars to remove from the
comment markers."
  (interactive "*r\nP")
  (comment-normalize-vars)
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (goto-char beg)
    (setq end (copy-marker end))
    (let ((numarg (prefix-numeric-value arg))
	  spt)
      (while (and (< (point) end)
		  (setq spt (comment-search-forward end t)))
	(let* ((ipt (point))
	       ;; Find the end of the comment.
	       (ept (progn
		      (goto-char spt)
		      (unless (comment-forward)
			(error "Can't find the comment end"))
		      (point)))
	       (box nil)
	       (ccs comment-continue)
	       (srei (comment-padright ccs 're))
	       (sre (and srei (concat "^\\s-*?\\(" srei "\\)"))))
	  (save-restriction
	    (narrow-to-region spt ept)
	    ;; Remove the comment-start.
	    (goto-char ipt)
	    (skip-syntax-backward " ")
	    ;; Check for special `=' used sometimes in comment-box.
	    (when (and (= (- (point) (point-min)) 1) (looking-at "=\\{7\\}"))
	      (skip-chars-forward "="))
	    ;; A box-comment starts with a looong comment-start marker.
	    (when (> (- (point) (point-min) (length comment-start)) 7)
	      (setq box t))
	    (when (looking-at (regexp-quote comment-padding))
	      (goto-char (match-end 0)))
	    (when (and sre (looking-at (concat "\\s-*\n\\s-*" srei)))
	      (goto-char (match-end 0)))
	    (if (null arg) (delete-region (point-min) (point))
	      (skip-syntax-backward " ")
	      (delete-char (- numarg)))

	    ;; Remove the end-comment (and leading padding and such).
	    (goto-char (point-max)) (comment-enter-backward)
	    ;; Check for special `=' used sometimes in comment-box.
	    (when (= (- (point-max) (point)) 1)
	      (let ((pos (point)))
		;; skip `=' but only if there are at least 7.
		(when (> (skip-chars-backward "=") -7) (goto-char pos))))
	    (unless (looking-at "\\(\n\\|\\s-\\)*\\'")
	      (when (and (bolp) (not (bobp))) (backward-char))
	      (if (null arg) (delete-region (point) (point-max))
		(skip-syntax-forward " ")
		(delete-char numarg)))

	    ;; Unquote any nested end-comment.
	    (comment-quote-nested comment-start comment-end t)

	    ;; Eliminate continuation markers as well.
	    (when sre
	      (let* ((cce (comment-string-reverse (or comment-continue
						      comment-start)))
		     (erei (and box (comment-padleft cce 're)))
		     (ere (and erei (concat "\\(" erei "\\)\\s-*$"))))
		(goto-char (point-min))
		(while (progn
			 (if (and ere (re-search-forward
				       ere (line-end-position) t))
			     (replace-match "" t t nil (if (match-end 2) 2 1))
			   (setq ere nil))
			 (forward-line 1)
			 (re-search-forward sre (line-end-position) t))
		  (replace-match "" t t nil (if (match-end 2) 2 1)))))
	    ;; Go to the end for the next comment.
	    (goto-char (point-max)))))
      (set-marker end nil))))

(defun comment-make-extra-lines (cs ce ccs cce min-indent max-indent &optional block)
  "Make the leading and trailing extra lines.
This is used for `extra-line' style (or `box' style if BLOCK is specified)."
  (let ((eindent 0))
    (if (not block)
	;; Try to match CS and CE's content so they align aesthetically.
	(progn
	  (setq ce (comment-string-strip ce t t))
	  (when (string-match "\\(.+\\).*\n\\(.*?\\)\\1" (concat ce "\n" cs))
	    (setq eindent
		  (max (- (match-end 2) (match-beginning 2) (match-beginning 0))
		       0))))
      ;; box comment
      (let* ((width (- max-indent min-indent))
	     (s (concat cs "a=m" cce))
	     (e (concat ccs "a=m" ce))
	     (c (if (string-match ".*\\S-\\S-" cs)
		    (aref cs (1- (match-end 0))) ?=))
	     (_ (string-match "\\s-*a=m\\s-*" s))
	     (fill
	      (make-string (+ width (- (match-end 0)
				       (match-beginning 0) (length cs) 3)) c)))
	(setq cs (replace-match fill t t s))
	(string-match "\\s-*a=m\\s-*" e)
	(setq ce (replace-match fill t t e))))
    (cons (concat cs "\n" (make-string min-indent ? ) ccs)
	  (concat cce "\n" (make-string (+ min-indent eindent) ? ) ce))))

(def-edebug-spec comment-with-narrowing t)
(put 'comment-with-narrowing 'lisp-indent-function 2)
(defmacro comment-with-narrowing (beg end &rest body)
  "Execute BODY with BEG..END narrowing.
Space is added (and then removed) at the beginning for the text's
indentation to be kept as it was before narrowing."
  (let ((bindent (make-symbol "bindent")))
    `(let ((,bindent (save-excursion (goto-char beg) (current-column))))
       (save-restriction
	 (narrow-to-region beg end)
	 (goto-char (point-min))
	 (insert (make-string ,bindent ? ))
	 (prog1
	     (progn ,@body)
	   ;; remove the bindent
	   (save-excursion
	     (goto-char (point-min))
	     (when (looking-at " *")
	       (let ((n (min (- (match-end 0) (match-beginning 0)) ,bindent)))
		 (delete-char n)
		 (setq ,bindent (- ,bindent n))))
	     (end-of-line)
	     (let ((e (point)))
	       (beginning-of-line)
	       (while (and (> ,bindent 0) (re-search-forward "   *" e t))
		 (let ((n (min ,bindent (- (match-end 0) (match-beginning 0) 1))))
		   (goto-char (match-beginning 0))
		   (delete-char n)
		   (setq ,bindent (- ,bindent n)))))))))))

(defun comment-region-internal (beg end cs ce
				    &optional ccs cce block lines indent)
  "Comment region BEG..END.
CS and CE are the comment start resp end string.
CCS and CCE are the comment continuation strings for the start resp end
of lines (default to CS and CE).
BLOCK indicates that end of lines should be marked with either CCE, CE or CS
\(if CE is empty) and that those markers should be aligned.
LINES indicates that an extra lines will be used at the beginning and end
of the region for CE and CS.
INDENT indicates to put CS and CCS at the current indentation of the region
rather than at left margin."
  ;;(assert (< beg end))
  (let ((no-empty t))
    ;; Sanitize CE and CCE.
    (if (and (stringp ce) (string= "" ce)) (setq ce nil))
    (if (and (stringp cce) (string= "" cce)) (setq cce nil))
    ;; If CE is empty, multiline cannot be used.
    (unless ce (setq ccs nil cce nil))
    ;; Should we mark empty lines as well ?
    (if (or ccs block lines) (setq no-empty nil))
    ;; Make sure we have end-markers for BLOCK mode.
    (when block (unless ce (setq ce (comment-string-reverse cs))))
    ;; If BLOCK is not requested, we don't need CCE.
    (unless block (setq cce nil))
    ;; Continuation defaults to the same as CS and CE.
    (unless ccs (setq ccs cs cce ce))
    
    (save-excursion
      (goto-char end)
      ;; If the end is not at the end of a line and the comment-end
      ;; is implicit (i.e. a newline), explicitly insert a newline.
      (unless (or ce (eolp)) (insert "\n") (indent-according-to-mode))
      (comment-with-narrowing beg end
	(let ((min-indent (point-max))
	      (max-indent 0))
	  (goto-char (point-min))
	  ;; Quote any nested comment marker
	  (comment-quote-nested comment-start comment-end nil)

	  ;; Loop over all lines to find the needed indentations.
	  (goto-char (point-min))
	  (while
	      (progn
		(unless (looking-at "[ \t]*$")
		  (setq min-indent (min min-indent (current-indentation))))
		(end-of-line)
		(setq max-indent (max max-indent (current-column)))
		(not (or (eobp) (progn (forward-line) nil)))))
	  
	  ;; Inserting ccs can change max-indent by (1- tab-width).
	  (setq max-indent
		(+ max-indent (max (length cs) (length ccs)) tab-width -1))
	  (unless indent (setq min-indent 0))

	  ;; make the leading and trailing lines if requested
	  (when lines
	    (let ((csce
		   (comment-make-extra-lines
		    cs ce ccs cce min-indent max-indent block)))
	      (setq cs (car csce))
	      (setq ce (cdr csce))))
	  
	  (goto-char (point-min))
	  ;; Loop over all lines from BEG to END.
	  (while
	      (progn
		(unless (and no-empty (looking-at "[ \t]*$"))
		  (move-to-column min-indent t)
		  (insert cs) (setq cs ccs) ;switch to CCS after the first line
		  (end-of-line)
		  (if (eobp) (setq cce ce))
		  (when cce
		    (when block (move-to-column max-indent t))
		    (insert cce)))
		(end-of-line)
		(not (or (eobp) (progn (forward-line) nil))))))))))

;;;###autoload
(defun comment-region (beg end &optional arg)
  "Comment or uncomment each line in the region.
With just \\[universal-argument] prefix arg, uncomment each line in region BEG..END.
Numeric prefix arg ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.
By default, comments start at the left margin, are terminated on each line,
even for syntax in which newline does not end the comment and blank lines
do not get comments.  This can be changed with `comment-style'.

The strings used as comment starts are built from
`comment-start' without trailing spaces and `comment-padding'."
  (interactive "*r\nP")
  (comment-normalize-vars)
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (let* ((numarg (prefix-numeric-value arg))
	 (add comment-add)
	 (style (cdr (assoc comment-style comment-styles)))
	 (lines (nth 2 style))
	 (block (nth 1 style))
	 (multi (nth 0 style)))
    (save-excursion
      ;; we use `chars' instead of `syntax' because `\n' might be
      ;; of end-comment syntax rather than of whitespace syntax.
      ;; sanitize BEG and END
      (goto-char beg) (skip-chars-forward " \t\n\r") (beginning-of-line)
      (setq beg (max beg (point)))
      (goto-char end) (skip-chars-backward " \t\n\r") (end-of-line)
      (setq end (min end (point)))
      (if (>= beg end) (error "Nothing to comment"))

      ;; sanitize LINES
      (setq lines
	    (and
	     lines ;; multi
	     (progn (goto-char beg) (beginning-of-line)
		    (skip-syntax-forward " ")
		    (>= (point) beg))
	     (progn (goto-char end) (end-of-line) (skip-syntax-backward " ")
		    (<= (point) end))
	     (or (not (string= "" comment-end)) block)
	     (progn (goto-char beg) (search-forward "\n" end t)))))

    ;; don't add end-markers just because the user asked for `block'
    (unless (or lines (string= "" comment-end)) (setq block nil))

    (cond
     ((consp arg) (uncomment-region beg end))
     ((< numarg 0) (uncomment-region beg end (- numarg)))
     (t
      (setq numarg (if (and (null arg) (= (length comment-start) 1))
		       add (1- numarg)))
      (comment-region-internal
       beg end
       (let ((s (comment-padright comment-start numarg)))
	 (if (string-match comment-start-skip s) s
	   (comment-padright comment-start)))
       (let ((s (comment-padleft comment-end numarg)))
	 (and s (if (string-match comment-end-skip s) s
		  (comment-padright comment-end))))
       (if multi (comment-padright comment-continue numarg))
       (if multi (comment-padleft (comment-string-reverse comment-continue) numarg))
       block
       lines
       (nth 3 style))))))

(defun comment-box (beg end &optional arg)
  "Comment out the BEG..END region, putting it inside a box.
The numeric prefix ARG specifies how many characters to add to begin- and
end- comment markers additionally to what `comment-add' already specifies."
  (interactive "*r\np")
  (let ((comment-style (if (cadr (assoc comment-style comment-styles))
			   'box-multi 'box)))
    (comment-region beg end (+ comment-add arg))))

;;;###autoload
(defun comment-dwim (arg)
  "Call the comment command you want (Do What I Mean).
If the region is active and `transient-mark-mode' is on, call
  `comment-region' (unless it only consists of comments, in which
  case it calls `uncomment-region').
Else, if the current line is empty, insert a comment and indent it.
Else if a prefix ARG is specified, call `comment-kill'.
Else, call `comment-indent'."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and mark-active transient-mark-mode)
      (let ((beg (min (point) (mark)))
	    (end (max (point) (mark))))
	(if (save-excursion ;; check for already commented region
	      (goto-char beg)
	      (comment-forward (point-max))
	      (<= end (point)))
	    (uncomment-region beg end arg)
	  (comment-region beg end arg)))
    (if (save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
	;; FIXME: If there's no comment to kill on this line and ARG is
	;; specified, calling comment-kill is not very clever.
	(if arg (comment-kill (and (integerp arg) arg)) (comment-indent))
      (let ((add (if arg (prefix-numeric-value arg)
		   (if (= (length comment-start) 1) comment-add 0))))
	;; Some modes insist on keeping column 0 comment in column 0
	;; so we need to move away from it before inserting the comment.
	(indent-according-to-mode)
	(insert (comment-padright comment-start add))
	(save-excursion
	  (unless (string= "" comment-end)
	    (insert (comment-padleft comment-end add)))
	  (indent-according-to-mode))))))

(defcustom comment-auto-fill-only-comments nil
  "Non-nil means to only auto-fill inside comments.
This has no effect in modes that do not define a comment syntax."
  :type 'boolean)

(defun comment-valid-prefix (prefix compos)
  (or
   ;; Accept any prefix if the current comment is not EOL-terminated.
   (save-excursion (goto-char compos) (comment-forward) (not (bolp)))
   ;; Accept any prefix that starts with a comment-start marker.
   (string-match (concat "\\`[ \t]*\\(?:" comment-start-skip "\\)")
		 fill-prefix)))

;;;###autoload
(defun comment-indent-new-line (&optional soft)
  "Break line at point and indent, continuing comment if within one.
This indents the body of the continued comment
under the previous comment line.

This command is intended for styles where you write a comment per line,
starting a new comment (and terminating it if necessary) on each line.
If you want to continue one comment across several lines, use \\[newline-and-indent].

If a fill column is specified, it overrides the use of the comment column
or comment indentation.

The inserted newline is marked hard if variable `use-hard-newlines' is true,
unless optional argument SOFT is non-nil."
  (interactive)
  (comment-normalize-vars t)
  (let (compos comin)
    ;; If we are not inside a comment and we only auto-fill comments,
    ;; don't do anything (unless no comment syntax is defined).
    (unless (and comment-start
		 comment-auto-fill-only-comments
		 (not (interactive-p))
		 (not (save-excursion
			(prog1 (setq compos (comment-beginning))
			  (setq comin (point))))))

      ;; Now we know we should auto-fill.
      (delete-horizontal-space)
      (if soft (insert-and-inherit ?\n) (newline 1))
      (if (and fill-prefix (not adaptive-fill-mode))
	  ;; Blindly trust a non-adaptive fill-prefix.
	  (progn
	    (indent-to-left-margin)
	    (insert-and-inherit fill-prefix))

	;; If necessary check whether we're inside a comment.
	(unless (or compos (null comment-start))
	  (save-excursion
	    (backward-char)
	    (setq compos (comment-beginning))
	    (setq comin (point))))

	(cond
	 ;; If there's an adaptive prefix, use it unless we're inside
	 ;; a comment and the prefix is not a comment starter.
	 ((and fill-prefix
	       (or (not compos)
		   (comment-valid-prefix fill-prefix compos)))
	  (indent-to-left-margin)
	  (insert-and-inherit fill-prefix))
	 ;; If we're not inside a comment, just try to indent.
	 ((not compos) (indent-according-to-mode))
	 (t
	  (let* ((comment-column
		  ;; The continuation indentation should be somewhere between
		  ;; the current line's indentation (plus 2 for good measure)
		  ;; and the current comment's indentation, with a preference
		  ;; for comment-column.
		  (save-excursion
		    ;; FIXME: use prev line's info rather than first line's.
		    (goto-char compos)
		    (min (current-column) (max comment-column
					       (+ 2 (current-indentation))))))
		 (comstart (buffer-substring compos comin))
		 (normalp
		  (string-match (regexp-quote (comment-string-strip
					       comment-start t t))
				comstart))
		 (comment-end
		  (if normalp comment-end
		    ;; The comment starter is not the normal comment-start
		    ;; so we can't just use comment-end.
		    (save-excursion
		      (goto-char compos)
		      (if (not (comment-forward)) comment-end
			(comment-string-strip
			 (buffer-substring
			  (save-excursion (comment-enter-backward) (point))
			  (point))
			 nil t)))))
		 (comment-start comstart)
		 (continuep (or comment-multi-line
				(cadr (assoc comment-style comment-styles))))
		 ;; Force comment-continue to be recreated from comment-start.
		 ;; FIXME: wrong if comment-continue was set explicitly!
		 ;; FIXME: use prev line's continuation if available.
		 (comment-continue nil))
	    (if (and comment-multi-line (> (length comment-end) 0))
		(indent-according-to-mode)
	      (insert-and-inherit ?\n)
	      (forward-char -1)
	      (comment-indent continuep)
	      (save-excursion
		(let ((pt (point)))
		  (end-of-line)
		  (let ((comend (buffer-substring pt (point))))
		    ;; The 1+ is to make sure we delete the \n inserted above.
		    (delete-region pt (1+ (point)))
		    (end-of-line 0)
		    (insert comend))))))))))))

(provide 'newcomment)

;;; newcomment.el ends here
