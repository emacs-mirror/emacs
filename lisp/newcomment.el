;;; newcomment.el --- (un)comment regions of buffers

;; Copyright (C) 1999  Stefan Monnier <monnier@cs.yale.edu>

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: comment uncomment
;; Version: $Name:  $
;; Revision: $Id: diff-mode.el,v 1.11 1999/10/09 23:38:29 monnier Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; History:

;;; Bugs:

;; - most of the code is not written (just copied from simple.el)
;; - too many other bugs to mention

;;; Todo:

;; - extract comment data from the syntax-table
;; - maybe do the opposite as well (set the syntax-table from other data)
;; - customizable auto-fill of comments

;;; Code:

(eval-when-compile (require 'cl))

(defcustom comment-column 32
  "*Column to indent right-margin comments to.
Setting this variable automatically makes it local to the current buffer.
Each mode establishes a different default value for this variable; you
can set the value for a particular mode using that mode's hook."
  :type 'integer
  :group 'fill-comments)
(make-variable-buffer-local 'comment-column)

(defcustom comment-start nil
  "*String to insert to start a new comment, or nil if no comment syntax."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'fill-comments)

(defcustom comment-start-skip nil
  "*Regexp to match the start of a comment plus everything up to its body.
If there are any \\(...\\) pairs, the comment delimiter text is held to begin
at the place matched by the close of the first pair."
  :type '(choice (const :tag "None" nil)
		 regexp)
  :group 'fill-comments)

(defcustom comment-end ""
  "*String to insert to end a new comment.
Should be an empty string if comments are terminated by end-of-line."
  :type 'string
  :group 'fill-comments)

(defvar comment-indent-hook nil
  "Obsolete variable for function to compute desired indentation for a comment.
This function is called with no args with point at the beginning of
the comment's starting delimiter.")

(defvar comment-indent-function
  '(lambda () comment-column)
  "Function to compute desired indentation for a comment.
This function is called with no args with point at the beginning of
the comment's starting delimiter.")

(defcustom block-comment-start nil
  "*String to insert to start a new comment on a line by itself.
If nil, use `comment-start' instead.
Note that the regular expression `comment-start-skip' should skip this string
as well as the `comment-start' string."
  :type '(choice (const :tag "Use comment-start" nil)
		 string)
  :group 'fill-comments)

(defcustom block-comment-end nil
  "*String to insert to end a new comment on a line by itself.
Should be an empty string if comments are terminated by end-of-line.
If nil, use `comment-end' instead."
  :type '(choice (const :tag "Use comment-end" nil)
		 string)
  :group 'fill-comments)

(defun indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment."
  (interactive "*")
  (let* ((empty (save-excursion (beginning-of-line)
				(looking-at "[ \t]*$")))
	 (starter (or (and empty block-comment-start) comment-start))
	 (ender (or (and empty block-comment-end) comment-end)))
    (cond
     ((null starter)
      (error "No comment syntax defined"))
     ((null comment-start-skip)
      (error "This mode doesn't define `comment-start-skip'"))
     (t (let* ((eolpos (save-excursion (end-of-line) (point)))
               cpos indent begpos)
          (beginning-of-line)
          (if (re-search-forward comment-start-skip eolpos 'move)
              (progn (setq cpos (point-marker))
                     ;; Find the start of the comment delimiter.
                     ;; If there were paren-pairs in comment-start-skip,
                     ;; position at the end of the first pair.
                     (if (match-end 1)
                         (goto-char (match-end 1))
                       ;; If comment-start-skip matched a string with
                       ;; internal whitespace (not final whitespace) then
                       ;; the delimiter start at the end of that
                       ;; whitespace.  Otherwise, it starts at the
                       ;; beginning of what was matched.
                       (skip-syntax-backward " " (match-beginning 0))
                       (skip-syntax-backward "^ " (match-beginning 0)))))
          (setq begpos (point))
          ;; Compute desired indent.
          (if (= (current-column)
                 (setq indent (if comment-indent-hook
                                  (funcall comment-indent-hook)
                                (funcall comment-indent-function))))
              (goto-char begpos)
            ;; If that's different from current, change it.
            (skip-chars-backward " \t")
            (delete-region (point) begpos)
            (indent-to indent))
          ;; An existing comment?
          (if cpos
              (progn (goto-char cpos)
                     (set-marker cpos nil))
            ;; No, insert one.
            (insert starter)
            (save-excursion
              (insert ender))))))))

(defun set-comment-column (arg)
  "Set the comment column based on point.
With no arg, set the comment column to the current column.
With just minus as arg, kill any comment on this line.
With any other arg, set comment column to indentation of the previous comment
 and then align or create a comment on this line at that column."
  (interactive "P")
  (if (eq arg '-)
      (kill-comment nil)
    (if arg
	(progn
	  (save-excursion
	    (beginning-of-line)
	    (re-search-backward comment-start-skip)
	    (beginning-of-line)
	    (re-search-forward comment-start-skip)
	    (goto-char (match-beginning 0))
	    (setq comment-column (current-column))
	    (message "Comment column set to %d" comment-column))
	  (indent-for-comment))
      (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))))

(defun kill-comment (arg)
  "Kill the comment on this line, if any.
With argument, kill comments on that many lines starting with this one."
  ;; this function loses in a lot of situations.  it incorrectly recognises
  ;; comment delimiters sometimes (ergo, inside a string), doesn't work
  ;; with multi-line comments, can kill extra whitespace if comment wasn't
  ;; through end-of-line, et cetera.
  (interactive "P")
  (or comment-start-skip (error "No comment syntax defined"))
  (let ((count (prefix-numeric-value arg)) endc)
    (while (> count 0)
      (save-excursion
	(end-of-line)
	(setq endc (point))
	(beginning-of-line)
	(and (string< "" comment-end)
	     (setq endc
		   (progn
		     (re-search-forward (regexp-quote comment-end) endc 'move)
		     (skip-chars-forward " \t")
		     (point))))
	(beginning-of-line)
	(if (re-search-forward comment-start-skip endc t)
	    (progn
	      (goto-char (match-beginning 0))
	      (skip-chars-backward " \t")
	      (kill-region (point) endc)
	      ;; to catch comments a line beginnings
	      (indent-according-to-mode))))
      (if arg (forward-line 1))
      (setq count (1- count)))))

(defvar comment-padding 1
  "Number of spaces `comment-region' puts between comment chars and text.
Can also be a string instead.

Extra spacing between the comment characters and the comment text
makes the comment easier to read.  Default is 1.  Nil means 0.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom comment-nested nil
  "Whether the comments can be nested.")
(defcustom comment-continue nil
  "Pair of strings to insert for multiline comments.")
(defcustom comment-add '(0 . 2)
  "How many more chars should be inserted by default.")
(defcustom comment-extra-lines nil
  "When comments should have an extra line before and after.
If nil, never add them.
If t, always add them,
If 'multiline, only add them for truly multiline comments.")
;; (defcustom comment-multiline t
;;   "non-nil if `comment-region' should use multi-line comments.")

(defun comment-normalize-vars ()
  (or comment-start (error "No comment syntax is defined"))
  (when (integerp comment-padding)
    (setq comment-padding (make-string comment-padding ? )))
  ;; 
  (when (string-match "\\`\\s-*\\(.*\\S-\\)\\s-*\\'" comment-start)
    (setq comment-start (match-string 1 comment-start)))
  (when (string-match "\\`\\s-*\\(.*\\S-\\)\\s-*\\'" comment-end)
    (setq comment-end (match-string 1 comment-end)))
  ;;
  (let ((csl (length comment-start)))
    (if (not (or comment-continue (string= comment-end "")))
	(set (make-local-variable 'comment-continue)
	     (cons (concat " " (substring comment-start 1))
		   "")))))

(defmacro until (&rest body)
  (let ((retsym (make-symbol "ret")))
    `(let (,retsym)
       (while (not (setq ,retsym (progn ,@body))))
       ,retsym)))

(defun string-reverse (s) (concat (reverse (string-to-list s))))

(defun uncomment-region (beg end &optional arg)
  "Comment or uncomment each line in the region.
With just C-u prefix arg, uncomment each line in region.
Numeric prefix arg ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.
Comments are terminated on each line, even for syntax in which newline does
not end the comment.  Blank lines do not get comments.

The strings used as comment starts are build from
`comment-start' without trailing spaces and `comment-padding'."
  (interactive "*r\nP")
  (comment-normalize-vars)
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (save-restriction
      (let* ((cs comment-start) (ce comment-end)
	     numarg)
	(if (consp arg) (setq numarg t)
	  (setq numarg (prefix-numeric-value arg))
	  ;; For positive arg > 1, replicate the comment delims now,
	  ;; then insert the replicated strings just once.
	  (while (> numarg 1)
	    (setq cs (concat cs comment-start)
		  ce (concat ce comment-end))
	    (setq numarg (1- numarg))))
	;; Loop over all lines from BEG to END.
	(narrow-to-region beg end)
	(goto-char beg)
	(cond
	 ((consp arg) (comment-region beg end))
	 ((< numarg 0) (comment-region beg end (- numarg)))
	 (t
	    (while (not (eobp))
	      (let (found-comment)
		;; Delete comment start from beginning of line.
		(if (eq numarg t)
		    (while (looking-at (regexp-quote cs))
		      (setq found-comment t)
		      (delete-char (length cs)))
		  (let ((count numarg))
		    (while (and (> 1 (setq count (1+ count)))
				(looking-at (regexp-quote cs)))
		      (setq found-comment t)
		      (delete-char (length cs)))))
		;; Delete comment padding from beginning of line
		(when (and found-comment comment-padding
			   (looking-at (regexp-quote comment-padding)))
		  (delete-char (length comment-padding)))
		;; Delete comment end from end of line.
		(if (string= "" ce)
		    nil
		  (if (eq numarg t)
		      (progn
			(end-of-line)
			;; This is questionable if comment-end ends in
			;; whitespace.  That is pretty brain-damaged,
			;; though.
			(while (progn (skip-chars-backward " \t")
				      (and (>= (- (point) (point-min)) (length ce))
					   (save-excursion
					     (backward-char (length ce))
					     (looking-at (regexp-quote ce)))))
			    (delete-char (- (length ce)))))
		    (let ((count numarg))
		      (while (> 1 (setq count (1+ count)))
			(end-of-line)
			;; this is questionable if comment-end ends in whitespace
			;; that is pretty brain-damaged though
			(skip-chars-backward " \t")
			(if (>= (- (point) (point-min)) (length ce))
			    (save-excursion
			      (backward-char (length ce))
			      (if (looking-at (regexp-quote ce))
				  (delete-char (length ce)))))))))
		(forward-line 1)))))))))

(defun comment-region-internal (beg end cs ce &optional ccs cce block lines)
  (assert (< beg end))
  (let ((no-empty t))
    ;; sanitize ce and cce
    (if (and (stringp ce) (string= "" ce)) (setq ce nil))
    (if (and (stringp cce) (string= "" cce)) (setq cce nil))
    ;; should we mark empty lines as well ?
    (if (or ccs block lines) (setq no-empty nil))
    ;; continuation defaults to the same
    (if ccs (unless block (setq cce nil))
      (setq ccs cs cce ce))
    ;; make sure we have end-markers for BLOCK mode
    (when block
      (if (null ce) (setq ce (string-reverse cs)))
      (if (null cce) (setq cce (string-reverse ccs))))

    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(let ((ce-quote-re
	       (when (and (not comment-nested) (> (length comment-end) 1))
		 (concat (regexp-quote (substring comment-end 0 1))
			 "\\\\*\\(\\)"
			 (regexp-quote (substring comment-end 1)))))
	      (min-indent (point-max))
	      (max-indent 0))
	  (goto-char (point-min))
	  ;; loop over all lines to find the needed indentations
	  (until
	   (unless (looking-at "[ \t]*$")
	     (setq min-indent (min min-indent (current-indentation))))
	   (when ce-quote-re
	     (let ((eol (save-excursion (end-of-line) (point))))
	       (while (re-search-forward ce-quote-re eol 'move)
		 (incf eol)
		 (replace-match "\\" t t nil 1))))
	   (end-of-line)
	   (setq max-indent (max max-indent (current-column)))
	   (or (eobp) (progn (forward-line) nil)))

	  ;; inserting ccs can change max-indent by (1- tab-width)
	  (incf max-indent (+ (max (length cs) (length ccs)) -1 tab-width))

	  (when lines
	    (if block
		(let* ((s (concat cs "a=m" cce "\n"
				  (make-string min-indent ? ) ccs))
		       (e (concat cce "\n" (make-string min-indent ? )
				  ccs "a=m" ce))
		       (_ (assert (string-match "\\s-*\\(a=m\\)\\s-*" s)))
		       (fill (make-string (+ (- max-indent
						min-indent
						(match-beginning 0))
					     (- (match-end 0)
						(match-end 1)))
					  (aref s (match-end 0)))))
		  (setq cs (replace-match fill t t s))
		  (assert (string-match "\\s-*\\(a=m\\)\\s-*" e))
		  (setq ce (replace-match fill t t e)))
	      (when (and ce (string-match "\\`\\s-*\\(.*\\S-\\)\\s-*\\'" ce))
		(setq ce (match-string 1 ce)))
	      (let* ((c (concat ce "a=m" cs))
		     (indent (if (string-match "\\(.+\\).*a=m\\(.*\\)\\1" c)
				 (max (+ min-indent
					 (- (match-end 2) (match-beginning 2))
					 (- (match-beginning 0)))
				      0)
			       min-indent)))
		(setq ce (concat cce "\n" (make-string indent ? ) (or ce cs)))
		(setq cs (concat cs "\n" (make-string min-indent ? ) ccs)))))
	  
	  (goto-char (point-min))
	  ;; Loop over all lines from BEG to END.
	  (until
	   (unless (and no-empty (looking-at "[ \t]*$"))
	     (move-to-column min-indent t)
	     (insert cs) (setq cs ccs)
	     (end-of-line)
	     (if (eobp) (setq cce ce))
	     (when cce
	       (when block (move-to-column max-indent t))
	       (insert cce)))
	   (end-of-line)
	   (or (eobp) (progn (forward-line) nil))))))))

(defun comment-addright (str n)
  (when (and (stringp str) (not (string= "" str)))
    (concat str (make-string n (aref str (1- (length str)))) comment-padding)))
(defun comment-addleft (str n)
  (when (and (stringp str) (not (string= "" str)))
    (concat comment-padding
	    (when (or comment-nested (> (length comment-end) 1))
	      (make-string n (aref str 0)))
	    str)))

(defun comment-region (beg end &optional arg)
  "Comment or uncomment each line in the region.
With just \\[universal-prefix] prefix arg, uncomment each line in region BEG..END.
Numeric prefix arg ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.
Comments are terminated on each line, even for syntax in which newline does
not end the comment.  Blank lines do not get comments.

The strings used as comment starts are built from
`comment-start' without trailing spaces and `comment-padding'."
  (interactive "*r\nP")
  (comment-normalize-vars)
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (let ((numarg (prefix-numeric-value arg))
	(add (car comment-add))
	(lines comment-extra-lines)
	(block nil))
    (save-excursion
      ;; we use `chars' instead of `syntax' because `\n' might be
      ;; of end-comment syntax rather than of whitespace syntax.
      ;; sanitize BEG and END
      (goto-char beg) (skip-chars-forward " \t\n\r") (beginning-of-line)
      (setq beg (max beg (point)))
      (goto-char end) (skip-chars-backward " \t\n\r") (end-of-line)
      (setq end (min end (point)))
      (if (>= beg end) (error "Nothing to comment"))

      ;; check for already commented region
      (goto-char beg)
      (forward-comment (point-max))
      (if (< end (point)) (setq arg '(4) numarg 4))

      ;; sanitize LINES
      (setq lines
	    (and
	     comment-multi-line
	     (progn (goto-char beg) (beginning-of-line)
		    (skip-syntax-forward " ")
		    (>= (point) beg))
	     (progn (goto-char end) (end-of-line) (skip-syntax-backward " ")
		    (<= (point) end))
	     (if (eq comment-extra-lines 'multiline)
		 (and (not (string= "" comment-end))
		      (progn (goto-char beg)
			     (search-forward "\n" end t)))
	       lines))))

    (when (and (consp arg) (>= numarg 16))
      (setq lines (>= numarg 64))
      (setq arg nil numarg 1 block t add (or (cdr comment-add) 2)))
    (cond
     ((consp arg) (uncomment-region beg end))
     ((< numarg 0) (uncomment-region beg end (- numarg)))
     (t
      (if (and (null arg) (= (length comment-start) 1))
	  (setq numarg add) (decf numarg))
      (comment-region-internal
       beg end
       (comment-addright comment-start numarg)
       (comment-addleft comment-end numarg)
       (if comment-multi-line
	   (comment-addright (car comment-continue) numarg))
       (if comment-multi-line
	   (comment-addleft (cdr comment-continue) numarg))
       block
       lines)))))

(provide 'newcomment)

;;; Change Log:
;; $Log$

;;; newcomment.el ends here
