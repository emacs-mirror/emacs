;;; newcomment.el --- (un)comment regions of buffers

;; Copyright (C) 1999  Stefan Monnier <monnier@cs.yale.edu>

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: comment uncomment
;; Version: $Name:  $
;; Revision: $Id: newcomment.el,v 1.3 1999/11/29 00:49:18 monnier Exp $

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

;; - comment-multi-line already exists with a different meaning
;;   and is not orthogonal to comment-extra-lines

;;; Todo:

;; - extract comment data from the syntax-table
;; - maybe do the opposite as well (set the syntax-table from other data)
;; - customizable auto-fill of comments
;; - uncomment-region with a numeric argument
;; - uncomment-region with a consp (for blocks) or somehow make the
;;   deletion of continuation markers less dangerous
;; - fix set-comment-column to not use comment-start-skip

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

(defun comment-find (&optional limit noerror)
  "Find a comment start between the point and LIMIT.
Moves the point to inside the comment and returns the position of the
comment-starter.  If no comment is found, moves the point to LIMIT
and raises an error or returns nil of NOERROR is non-nil."
  (let ((s (parse-partial-sexp (point) (or limit (point-max)) nil nil nil t)))
    (if (and (nth 8 s) (not (nth 3 s)))
	(nth 8 s)
      (unless noerror (error "No comment")))))

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
     (t (let* ((eolpos (save-excursion (end-of-line) (point)))
               cpos indent begpos)
          (beginning-of-line)
          (when (setq begpos (comment-find eolpos t))
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
With no arg, set the comment column to the current column.
With just minus as arg, kill any comment on this line.
With any other arg, set comment column to indentation of the previous comment
 and then align or create a comment on this line at that column."
  (interactive "P")
  (cond
   ((eq arg '-) (kill-comment nil))
   (arg
    (save-excursion
      (beginning-of-line)
      (re-search-backward comment-start-skip)
      (beginning-of-line)
      (goto-char (comment-find))
      (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))
    (indent-for-comment))
   (t (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))))

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

(defvar comment-padding 1
  "Number of spaces `comment-region' puts between comment chars and text.
Can also be a string instead.

Extra spacing between the comment characters and the comment text
makes the comment easier to read.  Default is 1.  Nil means 0.")

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
	(let ((cs (comment-find endc t)))
	  (when cs
	    (goto-char cs)
	    (skip-syntax-backward " ")
	    (setq cs (point))
	    (forward-comment 1)
	    (skip-syntax-backward " ")
	    (kill-region cs (if (bolp) (1- (point)) (point)))
	    (indent-according-to-mode))))
      (if arg (forward-line 1)))))

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
  (unless (or (car comment-continue) (string= comment-end ""))
    (set (make-local-variable 'comment-continue)
	 (cons (concat " " (substring comment-start 1))
	       nil)))
  (when (and (car comment-continue) (null (cdr comment-continue)))
    (setf (cdr comment-continue) (string-reverse (car comment-continue)))))

(defmacro until (&rest body)
  (let ((retsym (make-symbol "ret")))
    `(let (,retsym)
       (while (not (setq ,retsym (progn ,@body))))
       ,retsym)))
(def-edebug-spec until t)

(defun string-reverse (s) (concat (reverse (string-to-list s))))

(defun comment-end-quote-re (str &optional re)
  "Make a regexp that matches the (potentially quoted) STR comment-end.
The regexp has one group in it which matches RE right after the
potential quoting."
  (when (and (not comment-nested) (> (length str) 1))
    (concat (regexp-quote (substring str 0 1))
	    "\\\\*\\(" re "\\)"
	    (regexp-quote (substring str 1)))))

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
	  state spt)
      (while (and (< (point) end)
		  (setq state (parse-partial-sexp
			       (point) end
			       nil nil nil t))
		  (setq spt (nth 8 state))
		  (not (nth 3 state)))
	(let* ((stxt (buffer-substring spt (point)))
	       ;; find the end of the comment
	       (ept (progn
		      (when (nth 8 (parse-partial-sexp
				    (point) (point-max)
				    nil nil state 'syntax-table))
			(error "Can't find the comment end"))
		      (point-marker)))
	       ;; find the start of the end-comment
	       (_ (while (save-excursion
			   (save-restriction
			     (narrow-to-region (point) ept)
			     (nth 8
				  (parse-partial-sexp (point) ept
						      nil nil state))))
		    (backward-char)))
	       (etxt (buffer-substring (point) ept))
	       (end-quote-re (comment-end-quote-re etxt "\\\\")))
	  (save-restriction
	    (narrow-to-region spt ept)
	    ;; remove the end-comment (and leading padding and such)
	    (unless (string= "\n" etxt)
	      (beginning-of-line)
	      (re-search-forward (concat "\\(^\\s-*\\|\\("
					 (regexp-quote comment-padding)
					 "\\)?\\)"
					 (regexp-quote (substring etxt 0 1))
					 "+"
					 (regexp-quote (substring etxt 1))
					 "\\'"))
	      (delete-region (match-beginning 0) (match-end 0)))

	    ;; remove the comment-start
	    (goto-char (point-min))
	    (looking-at (concat (regexp-quote stxt)
				"+\\(\\s-*$\\|"
				(regexp-quote comment-padding)
				"\\)"))
	    (delete-region (match-beginning 0) (match-end 0))

	    ;; unquote any nested end-comment
	    (when end-quote-re
	      (goto-char (point-min))
	      (while (re-search-forward end-quote-re nil t)
		(delete-region (match-beginning 1) (match-end 1))))

	    ;; eliminate continuation markers as well
	    (let* ((ccs (car comment-continue))
		   (cce (cdr comment-continue))
		   (sre (when (and (stringp ccs) (not (string= "" ccs)))
			  (concat
			   "^\\s-*\\(" (regexp-quote ccs)
			   "+\\(" (regexp-quote comment-padding)
			   "\\)?\\)")))
		   (ere (when (and (stringp cce) (not (string= "" cce)))
			  (concat
			   "\\(\\(" (regexp-quote comment-padding)
			   "\\)?" (regexp-quote cce) "\\)\\s-*$")))
		   (re (if (and sre ere) (concat sre "\\|" ere)
			 (or sre ere))))
	      (when re
		(goto-char (point-min))
		(while (re-search-forward re nil t)
		  (replace-match "" t t nil (if (match-end 1) 1 3)))))
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
    (when block (unless ce (setq ce (string-reverse cs))))
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
;; $Log: newcomment.el,v $
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
