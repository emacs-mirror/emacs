;;; newcomment.el --- (un)comment regions of buffers

;; Copyright (C) 1999  Stefan Monnier <monnier@cs.yale.edu>

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: comment uncomment
;; Version: $Name:  $
;; Revision: $Id: newcomment.el,v 1.4 1999/11/29 01:31:47 monnier Exp $

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

;; - single-char nestable comment-start can only do the "\\s<+" stuff
;;   if the corresponding closing marker happens to be right.
;; - C-u C-u comment-region in TeXinfo generates bogus comments @ccccc@
;; - removal of comment-continue does not necesarily work because the
;;   continuation marker could have a leading space that turned into a tab

;;; Todo:

;; - extract comment data from the syntax-table
;; - maybe do the opposite as well (set the syntax-table from other data)
;; - customizable auto-fill of comments
;; - uncomment-region with a numeric argument
;; - uncomment-region with a consp (for blocks) or somehow make the
;;   deletion of continuation markers less dangerous
;; - drop block-comment-<foo> unless it's really used
;; - uncomment-region un a part of a comment

;;; Problems:

;; - comment padding:  (= comment-start "[- ") can either mean that
;;   the syntax of a comment-start is "[-" plus " " of padding
;;   (as is the case for C) or that the space is strictly required
;;   as is the case for TeXinfo.

;;; Code:

(eval-when-compile (require 'cl))

(defcustom comment-use-syntax 'maybe
  "Non-nil if syntax-tables can be used instead of regexps.")

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
(defcustom comment-end-skip nil
  "*Regexp to match the end of a comment plus everything up to its body."
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

(defcustom comment-nested 'maybe
  "Whether the comments can be nested.")
(defcustom comment-continue nil
  "Pair of strings to insert for multiline comments.")
(defcustom comment-add '(0 . 2)
  "How many more chars should be inserted by default.")

(defcustom comment-style 'multi-line
  "*Style to be used for inserting comments."
  :group 'comment
  :type '(choice (const plain)
		 (const aligned)
		 (const multi-line)
		 (const extra-line)))
(defconst comment-styles
  '((plain	. (nil nil nil))
    (aligned	. (nil t nil))
    (multi-line	. (t nil nil))
    (extra-line	. (t nil t)))
  "Possible styles.")

(defvar comment-padding 1
  "Number of spaces `comment-region' puts between comment chars and text.
Can also be a string instead.

Extra spacing between the comment characters and the comment text
makes the comment easier to read.  Default is 1.  Nil means 0.")

;;;;
;;;; Helpers
;;;;

(defun comment-string-strip (str before after)
  (string-match (concat "\\`" (if before "\\s-*")
			"\\(.*?\\)" (if after "\\s-*")
			"\\'") str)
  (match-string 1 str))

(defun comment-string-reverse (s)
  (comment-string-strip (concat (reverse (string-to-list s))) nil t))

(defun comment-normalize-vars (&optional noerror)
  (if (not comment-start) (or noerror (error "No comment syntax is defined"))
    ;; comment-use-syntax
    (when (eq comment-use-syntax 'maybe)
      (set (make-local-variable 'comment-use-syntax)
	   (let ((st (syntax-table))
		 (cs comment-start)
		 (ce (if (string= "" comment-end) "\n" comment-end)))
	     (with-temp-buffer
	       (set-syntax-table st)
	       (insert cs " hello " ce)
	       (goto-char (point-min))
	       (and (forward-comment 1) (eobp))))))
    (when (eq comment-nested 'maybe)
      (set (make-local-variable 'comment-nested)
	   (let ((st (syntax-table))
		 (cs comment-start)
		 (ce (if (string= "" comment-end) "\n" comment-end)))
	     (with-temp-buffer
	       (set-syntax-table st)
	       (insert cs " he " cs " hello " ce " ho " ce)
	       (goto-char (point-min))
	       (and (forward-comment 1) (eobp))))))
    ;; comment-padding
    (when (integerp comment-padding)
      (setq comment-padding (make-string comment-padding ? )))
    ;; comment markers
    ;;(setq comment-start (comment-string-strip comment-start t nil))
    ;;(setq comment-end (comment-string-strip comment-end nil t))
    ;; comment-continue
    (unless (or (car comment-continue) (string= comment-end ""))
      (set (make-local-variable 'comment-continue)
	   (cons (concat " " (substring comment-start 1))
		 nil)))
    (when (and (car comment-continue) (null (cdr comment-continue)))
      (setcdr comment-continue (comment-string-reverse (car comment-continue))))
    ;; comment-skip regexps
    (unless comment-start-skip
      (set (make-local-variable 'comment-start-skip)
	   (concat "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(\\s<+\\|"
		   (regexp-quote (comment-string-strip comment-start t t))
		   "+\\)\\s-*")))
    (unless comment-end-skip
      (let ((ce (if (string= "" comment-end) "\n"
		  (comment-string-strip comment-end t t))))
	(set (make-local-variable 'comment-end-skip)
	     (concat "\\s-*\\(\\s>" (if comment-nested "+" "")
		     "\\|" (regexp-quote (substring ce 0 1))
		     (if (or comment-nested (> (length ce) 1)) "+" "")
		     (regexp-quote (substring ce 1))
		     "\\)"))))))
 
(defmacro until (&rest body)
  (let ((retsym (make-symbol "ret")))
    `(let (,retsym)
       (while (not (setq ,retsym (progn ,@body))))
       ,retsym)))
(def-edebug-spec until t)

(defun comment-end-quote-re (str &optional re)
  "Make a regexp that matches the (potentially quoted) STR comment-end.
The regexp has one group in it which matches RE right after the
potential quoting."
  (setq str (comment-string-strip str t t))
  (when (and (not comment-nested) (> (length str) 1))
    (concat (regexp-quote (substring str 0 1))
	    "\\\\*\\(" re "\\)"
	    (regexp-quote (substring str 1)))))

;;;;
;;;; Navigation
;;;;

(defun comment-search-forward (&optional limit noerror)
  "Find a comment start between the point and LIMIT.
Moves the point to inside the comment and returns the position of the
comment-starter.  If no comment is found, moves the point to LIMIT
and raises an error or returns nil of NOERROR is non-nil."
  (if (not comment-use-syntax)
      (when (re-search-forward comment-start-skip limit noerror)
	(or (match-end 1) (match-beginning 0)))
    (let ((s (parse-partial-sexp (point) (or limit (point-max)) nil nil nil t)))
      (if (and (nth 8 s) (not (nth 3 s)))
	  (let ((pt (point))
		(start (nth 8 s))
		(bol (save-excursion (beginning-of-line) (point)))
		(end nil))
	    (while (and (null end) (>= (point) bol))
	      (if (looking-at comment-start-skip)
		  (setq end (match-end 0))
		(backward-char)))
	    (goto-char end)
	    start)
	(unless noerror (error "No comment"))))))

(defun comment-search-backward (&optional limit noerror)
  "Find a comment start between LIMIT and point.
Moves the point to inside the comment and returns the position of the
comment-starter.  If no comment is found, moves the point to LIMIT
and raises an error or returns nil of NOERROR is non-nil."
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
  "Find the beginning of the inclosing comment.
Returns nil if not inside a comment, else moves the point and returns
the same as `comment-search-forward'."
  (let ((pt (point))
	(cs (comment-search-backward nil t)))
    (save-excursion
      (and cs
	   (progn (goto-char cs) (forward-comment 1) (> (point) pt))
	   cs))))

(defun comment-forward (&optional n)
  "Skip forward over N comments.
Just like `forward-comment' but only for positive N
and can use regexps instead of syntax."
  (setq n (or n 1))
  (if (< n 0) (error "No comment-backward")
    (if comment-use-syntax (forward-comment n)
      (while (> n 0)
	(skip-syntax-forward " ")
	(if (and (looking-at comment-start-skip)
		 (re-search-forward comment-end-skip nil 'move))
	    (decf n)
	  (setq n -1)))
      (= n 0))))

(defun comment-enter-backward ()
  "Move from the end of a comment to the end of its content.
The point is assumed to be right at the end of a comment."
  (if (bolp)
      ;; comment-end = ""
      (progn (backward-char) (skip-syntax-backward " "))
    (let ((end (point)))
      (beginning-of-line)
      (save-restriction
	(narrow-to-region (point) end)
	(re-search-forward (concat comment-end-skip "\\'"))
	(goto-char (match-beginning 0))))))

;;;;
;;;; Commands
;;;;

(defun indent-for-comment (&optional continue)
  "Indent this line's comment to comment column, or insert an empty comment.
If CONTINUE is non-nil, use the `comment-continuation' markers if any."
  (interactive "*")
  (let* ((empty (save-excursion (beginning-of-line)
				(looking-at "[ \t]*$")))
	 (starter (or (and continue (car comment-continue))
		      (and empty block-comment-start) comment-start))
	 (ender (or (and continue (car comment-continue) "")
		    (and empty block-comment-end) comment-end)))
    (cond
     ((null starter)
      (error "No comment syntax defined"))
     (t (let* ((eolpos (save-excursion (end-of-line) (point)))
               cpos indent begpos)
          (beginning-of-line)
          (when (setq begpos (comment-search-forward eolpos t))
	    (skip-chars-forward
	     (concat (buffer-substring (1- (point)) (point)) " \t"))
	    (setq cpos (point-marker))
	    (goto-char begpos))
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
With no ARG, set the comment column to the current column.
With just minus as arg, kill any comment on this line.
With any other arg, set comment column to indentation of the previous comment
 and then align or create a comment on this line at that column."
  (interactive "P")
  (cond
   ((eq arg '-) (kill-comment nil))
   (arg
    (save-excursion
      (beginning-of-line)
      (comment-search-backward)
      (beginning-of-line)
      (goto-char (comment-search-forward))
      (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))
    (indent-for-comment))
   (t (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))))

(defun kill-comment (arg)
  "Kill the comment on this line, if any.
With prefix ARG, kill comments on that many lines starting with this one."
  (interactive "P")
  (let (endc)
    (dotimes (_ (prefix-numeric-value arg))
      (save-excursion
	(end-of-line)
	(setq endc (point))
	(beginning-of-line)
	(let ((cs (comment-search-forward endc t)))
	  (when cs
	    (goto-char cs)
	    (skip-syntax-backward " ")
	    (setq cs (point))
	    (comment-forward)
	    (kill-region cs (if (bolp) (1- (point)) (point)))
	    (indent-according-to-mode))))
      (if arg (forward-line 1)))))

(defun comment-padright (str &optional n)
  "Construct a string composed of STR plus `comment-padding'.
It contains N copies of the last non-whitespace chars of STR.
If STR already contains padding, the corresponding amount is
  ignored from `comment-padding'.
N defaults to 1.
It N is 're, a regexp is returned instead, that would match
  the string for any N."
  (setq n (or n 0))
  (when (and (stringp str) (not (string= "" str)))
    (string-match "\\s-*\\'" str)
    (let ((s (substring str 0 (match-beginning 0)))
	  (pad (concat (match-string 0 str)
		       (substring comment-padding
				  (min (- (match-end 0) (match-beginning 0))
				       (length comment-padding))))))
      (if (symbolp n)
	  (concat (regexp-quote s) "+"
		  (mapconcat (lambda (c) (concat (regexp-quote (string c)) "?"))
			     pad ""))
	(concat s (make-string n (aref str (1- (match-beginning 0)))) pad)))))

(defun comment-padleft (str &optional n)
  "Construct a string composed of `comment-padding' plus STR.
It contains N copies of the last non-whitespace chars of STR.
If STR already contains padding, the corresponding amount is
  ignored from `comment-padding'.
N defaults to 1.
It N is 're, a regexp is returned instead, that would match
  the string for any N."
  (setq n (or n 0))
  (when (and (stringp str) (not (string= "" str)))
    (string-match "\\`\\s-*" str)
    (let ((s (substring str (match-end 0)))
	  (pad (concat (substring comment-padding
				  (min (- (match-end 0) (match-beginning 0))
				       (length comment-padding)))
		       (match-string 0 str)))
	  (c (aref str (match-end 0)))
	  (multi (or comment-nested (string= comment-end "")
		     (> (length str) (1+ (match-end 0))))))
      (if (symbolp n)
	  (concat "\\s-*"
		  (if multi (concat (regexp-quote (string c)) "*"))
		  (regexp-quote s))
	(concat pad (when multi (make-string n c)) s)))))

(defun uncomment-region (beg end &optional arg)
  "Uncomment each line in the BEG..END region.
ARG is currently ignored."
  (interactive "*r\nP")
  (comment-normalize-vars)
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (goto-char beg)
    (unless (markerp end) (setq end (copy-marker end)))
    (let ((numarg (prefix-numeric-value arg))
	  spt)
      (while (and (< (point) end)
		  (setq spt (comment-search-forward end t)))
	(let* ((ipt (point))
	       ;; find the end of the comment
	       (ept (progn
		      (goto-char spt)
		      (unless (comment-forward)
			(error "Can't find the comment end"))
		      (point-marker)))
	       (block nil)
	       (end-quote-re (comment-end-quote-re comment-end "\\\\"))
	       (ccs (car comment-continue))
	       (srei (comment-padright ccs 're))
	       (sre (and srei (concat "^\\s-*\\(" srei "\\)"))))
	  (save-restriction
	    (narrow-to-region spt ept)
	    ;; remove the comment-start
	    (goto-char ipt)
	    (skip-syntax-backward " ")
	    (when (> (- (point) (point-min) (length comment-start)) 7)
	      (setq block t))
	    (when (looking-at (regexp-quote comment-padding))
	      (goto-char (match-end 0)))
	    (when (and sre (looking-at (concat "\\s-*\n\\s-*" srei)))
	      (goto-char (match-end 0)))
	    (delete-region (point-min) (point))

	    ;; remove the end-comment (and leading padding and such)
	    (goto-char (point-max)) (comment-enter-backward)
	    (unless (string-match "\\`\\(\n\\|\\s-\\)*\\'"
				  (buffer-substring (point) ept))
	      (when (and (bolp) (not (bobp))) (backward-char))
	      (delete-region (point) ept))

	    ;; unquote any nested end-comment
	    (when end-quote-re
	      (goto-char (point-min))
	      (while (re-search-forward end-quote-re nil t)
		(delete-region (match-beginning 1) (match-end 1))))

	    ;; eliminate continuation markers as well
	    (let* ((cce (or (cdr comment-continue)
			(comment-string-reverse comment-start)))
		   (erei (and block (comment-padleft cce 're)))
		   (ere (and erei (concat "\\(" erei "\\)\\s-*$")))
		   (re (if (and sre ere) (concat sre "\\|" ere) (or sre ere))))
	      (when re
		(goto-char (point-min))
		;; there can't be a real SRE on the first line.
		(when (and sre (looking-at sre)) (goto-char (match-end 0)))
		(while (re-search-forward re nil t)
		  (replace-match "" t t nil (if (match-end 2) 2 1)))))
	      ;; go the the end for the next comment
	    (goto-char (point-max))))))))

(defun comment-make-extra-lines (cs ce ccs cce min-indent max-indent &optional block)
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
      (setq cs (concat cs "\n" (make-string min-indent ? ) ccs))))
  (values cs ce))

(def-edebug-spec comment-with-narrowing t)
(put 'comment-with-narrowing 'lisp-indent-function 2)
(defmacro comment-with-narrowing (beg end &rest body)
  "Execute BODY with BEG..END narrowing.
Space is added (and then removed) at the beginning for the text's
indentation to be kept as it was before narrowing."
  `(let ((-bindent (save-excursion (goto-char beg) (current-column))))
     (save-restriction
       (narrow-to-region beg end)
       (goto-char (point-min))
       (insert (make-string -bindent ? ))
       (prog1
	   (progn ,@body)
	 ;; remove the -bindent
	 (save-excursion
	   (goto-char (point-min))
	   (when (looking-at " *")
	     (let ((n (min (- (match-end 0) (match-beginning 0)) -bindent)))
	       (delete-char n)
	       (decf -bindent n)))
	   (end-of-line)
	   (let ((e (point)))
	     (beginning-of-line)
	     (while (and (> -bindent 0) (re-search-forward "  +" e t))
	       (let ((n (min -bindent (- (match-end 0) (match-beginning 0) 1))))
		 (goto-char (match-beginning 0))
		 (delete-char n)
		 (decf -bindent n)))))))))

(defun comment-region-internal (beg end cs ce &optional ccs cce block lines)
  (assert (< beg end))
  (let ((no-empty t))
    ;; sanitize ce and cce
    (if (and (stringp ce) (string= "" ce)) (setq ce nil))
    (if (and (stringp cce) (string= "" cce)) (setq cce nil))
    ;; should we mark empty lines as well ?
    (if (or ccs block lines) (setq no-empty nil))
    ;; make sure we have end-markers for BLOCK mode
    (when block (unless ce (setq ce (comment-string-reverse cs))))
    ;; continuation defaults to the same
    (if ccs (unless block (setq cce nil))
      (setq ccs cs cce ce))
    
    (save-excursion
      (goto-char end)
      (unless (or ce (eolp)) (insert "\n") (indent-according-to-mode))
      (comment-with-narrowing beg end
	(let ((ce-quote-re (comment-end-quote-re comment-end))
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

	  ;; make the leading and trailing lines if requested
	  (when lines
	    (multiple-value-setq (cs ce)
	      (comment-make-extra-lines
	       cs ce ccs cce min-indent max-indent block)))
	  
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
  (let* ((numarg (prefix-numeric-value arg))
	 (add (car comment-add))
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
	     lines multi
	     (progn (goto-char beg) (beginning-of-line)
		    (skip-syntax-forward " ")
		    (>= (point) beg))
	     (progn (goto-char end) (end-of-line) (skip-syntax-backward " ")
		    (<= (point) end))
	     (not (string= "" comment-end))
	     (progn (goto-char beg) (search-forward "\n" end t)))))

    ;; C-u C-u makes a full block
    (when (and (consp arg) (>= numarg 16))
      (setq lines t block t add (or (cdr comment-add) 2))
      (setq arg nil numarg 1))

    ;; don't add end-markers just because the user asked for `block'
    (unless (or lines (string= "" comment-end)) (setq block nil))

    (cond
     ((consp arg) (uncomment-region beg end))
     ((< numarg 0) (uncomment-region beg end (- numarg)))
     (t
      (if (and (null arg) (= (length comment-start) 1))
	  (setq numarg add) (decf numarg))
      (comment-region-internal
       beg end
       (comment-padright comment-start numarg)
       (comment-padleft comment-end numarg)
       (if multi (comment-padright (car comment-continue) numarg))
       (if multi (comment-padleft (cdr comment-continue) numarg))
       block
       lines)))))

(defun comment-dwim (arg)
  "Call the comment command you want.
If the region is active, calls `comment-region' (unless it only consists
in comments, in which case it calls `uncomment-region').
Else, if the current line is empty, insert a comment and indent it.
Else call `indent-for-comment' or `kill-comment' if a prefix ARG is specified."
  (interactive "*P")
  (comment-normalize-vars)
  (if mark-active
      (let ((beg (min (point) (mark)))
	    (end (max (point) (mark))))
	(if (save-excursion ;; check for already commented region
	      (goto-char beg)
	      (comment-forward (point-max))
	      (<= end (point)))
	    (uncomment-region beg end arg)
	  (comment-region beg end arg)))
    (if (save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
	(if arg (kill-comment (and (integerp arg) arg)) (indent-for-comment))
      (let ((add (if arg (prefix-numeric-value arg)
		   (if (= (length comment-start) 1) (car comment-add) 0))))
	(insert (comment-padright comment-start add))
	(save-excursion
	  (unless (string= "" comment-end)
	    (insert (comment-padleft comment-end add)))
	  (indent-according-to-mode))))))

(defcustom comment-multi-line nil
  "*Non-nil means \\[indent-new-comment-line] should continue same comment
on new line, with no new terminator or starter.
This is obsolete because you might as well use \\[newline-and-indent]."
  :type 'boolean
  :group 'fill-comments)

(defun indent-new-comment-line (&optional soft)
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
  (let (comcol comstart)
    (skip-chars-backward " \t")
    (delete-region (point)
		   (progn (skip-chars-forward " \t")
			  (point)))
    (if soft (insert-and-inherit ?\n) (newline 1))
    (if fill-prefix
	(progn
	  (indent-to-left-margin)
	  (insert-and-inherit fill-prefix))
      (unless comment-multi-line
	(save-excursion
	  (backward-char)
	  (if (and comment-start
		   (setq comcol (comment-beginning)))
	      ;; The old line has a comment and point was inside the comment.
	      ;; Set WIN to the pos of the comment-start.

	      ;; If comment-start-skip contains a \(...\) pair,
	      ;; the real comment delimiter starts at the end of that pair.
	      (let ((win comcol))
		;; But if the comment is empty, look at preceding lines
		;; to find one that has a nonempty comment.
		;; (while (and (eolp) (not (bobp))
		;; 	    (let (opoint)
		;; 	      (beginning-of-line)
		;; 	      (setq opoint (point))
		;; 	      (forward-line -1)
		;; 	      (setq win (comment-search-forward opoint t)))))
		;; Why do we do that ?  -sm

		;; Indent this line like what we found.
		(setq comstart (buffer-substring win (point)))
		(goto-char win)
		(setq comcol (current-column))
		))))
      (if comcol
	  (let ((comment-column comcol)
		(comment-start comstart))
	    ;;(if (not (eolp)) (setq comment-end ""))
	    (insert-and-inherit ?\n)
	    (forward-char -1)
	    (indent-for-comment (cadr (assoc comment-style comment-styles)))
	    (save-excursion
	      (let ((pt (point)))
		(end-of-line)
		(let ((comend (buffer-substring pt (point))))
		  ;; The 1+ is to make sure we delete the \n inserted above.
		  (delete-region pt (1+ (point)))
		  (beginning-of-line)
		  (backward-char)
		  (insert comend)
		  (forward-char)))))
	(indent-according-to-mode)))))

(provide 'newcomment)

;;; Change Log:
;; $Log: newcomment.el,v $
;; Revision 1.4  1999/11/29 01:31:47  monnier
;; (comment-find): New function.
;; (indent-for-comment, set-comment-column, kill-comment): use it.
;;
;; Revision 1.3  1999/11/29 00:49:18  monnier
;; (kill-comment): Fixed by rewriting it with syntax-tables rather than regexps
;; (comment-normalize-vars): Set default (cdr comment-continue)
;; (comment-end-quote-re): new function taken out of `comment-region-internal'
;; (uncomment-region): Rewritten using syntax-tables.  Also unquotes
;;   nested comment-ends and eliminates continuation markers.
;; (comment-region-internal): Don't create a default for cce.
;;   Use `comment-end-quote-re'.
;;
;; Revision 1.2  1999/11/28 21:33:55  monnier
;; (comment-make-extra-lines): Moved out of comment-region-internal.
;; (comment-with-narrowing): New macro.  Provides a way to preserve
;;   indentation inside narrowing.
;; (comment-region-internal): Add "\n" to close the comment if necessary.
;;   Correctly handle commenting-out when BEG is not bolp.
;;
;; Revision 1.1  1999/11/28 18:51:06  monnier
;; First "working" version:
;; - uncomment-region doesn't work for some unknown reason
;; - comment-multi-line allows the use of multi line comments
;; - comment-extra-lines allows yet another style choice
;; - comment-add allows to default to `;;'
;; - comment-region on a comment calls uncomment-region
;; - C-u C-u comment-region aligns comment end markers
;; - C-u C-u C-u comment-region puts the comment inside a rectangle
;; - comment-region will try to quote coment-end markers inside the region
;; - comment-start markers are placed at the indentation level
;;

;;; newcomment.el ends here
