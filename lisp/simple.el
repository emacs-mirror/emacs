;;; simple.el --- basic editing commands for Emacs

;; Copyright (C) 1985, 86, 87, 93, 94, 95, 96, 97, 98, 99, 2000, 2001, 2002
;;        Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

;; A grab-bag of basic Emacs commands not specifically related to some
;; major mode or to file-handling.

;;; Code:

(eval-when-compile
  (autoload 'widget-convert "wid-edit")
  (autoload 'shell-mode "shell")
  (require 'cl))


(defgroup killing nil
  "Killing and yanking commands"
  :group 'editing)

(defgroup paren-matching nil
  "Highlight (un)matching of parens and expressions."
  :group 'matching)

(define-key global-map [?\C-x right] 'next-buffer)
(define-key global-map [?\C-x left] 'prev-buffer)
(defun next-buffer ()
  "Switch to the next buffer in cyclic order."
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-buffer (other-buffer buffer))
    (bury-buffer buffer)))

(defun prev-buffer ()
  "Switch to the previous buffer in cyclic order."
  (interactive)
  (let ((list (nreverse (buffer-list)))
	found)
    (while (and (not found) list)
      (let ((buffer (car list)))
	(if (and (not (get-buffer-window buffer))
		 (not (string-match "\\` " (buffer-name buffer))))
	    (setq found buffer)))
      (setq list (cdr list)))
    (switch-to-buffer found)))

(defun fundamental-mode ()
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (interactive)
  (kill-all-local-variables))

;; Making and deleting lines.

(defun newline (&optional arg)
  "Insert a newline, and move to left margin of the new line if it's blank.
If `use-hard-newlines' is non-nil, the newline is marked with the
text-property `hard'.
With ARG, insert that many newlines.
Call `auto-fill-function' if the current column number is greater
than the value of `fill-column' and ARG is `nil'."
  (interactive "*P")
  (barf-if-buffer-read-only)
  ;; Inserting a newline at the end of a line produces better redisplay in
  ;; try_window_id than inserting at the beginning of a line, and the textual
  ;; result is the same.  So, if we're at beginning of line, pretend to be at
  ;; the end of the previous line.
  (let ((flag (and (not (bobp))
		   (bolp)
		   ;; Make sure no functions want to be told about
		   ;; the range of the changes.
		   (not after-change-functions)
		   (not before-change-functions)
		   ;; Make sure there are no markers here.
		   (not (buffer-has-markers-at (1- (point))))
		   (not (buffer-has-markers-at (point)))
		   ;; Make sure no text properties want to know
		   ;; where the change was.
		   (not (get-char-property (1- (point)) 'modification-hooks))
		   (not (get-char-property (1- (point)) 'insert-behind-hooks))
		   (or (eobp)
		       (not (get-char-property (point) 'insert-in-front-hooks)))
		   ;; Make sure the newline before point isn't intangible.
		   (not (get-char-property (1- (point)) 'intangible))
		   ;; Make sure the newline before point isn't read-only.
		   (not (get-char-property (1- (point)) 'read-only))
		   ;; Make sure the newline before point isn't invisible.
		   (not (get-char-property (1- (point)) 'invisible))
		   ;; Make sure the newline before point has the same
		   ;; properties as the char before it (if any).
		   (< (or (previous-property-change (point)) -2)
		      (- (point) 2))))
	(was-page-start (and (bolp)
			     (looking-at page-delimiter)))
	(beforepos (point)))
    (if flag (backward-char 1))
    ;; Call self-insert so that auto-fill, abbrev expansion etc. happens.
    ;; Set last-command-char to tell self-insert what to insert.
    (let ((last-command-char ?\n)
	  ;; Don't auto-fill if we have a numeric argument.
	  ;; Also not if flag is true (it would fill wrong line);
	  ;; there is no need to since we're at BOL.
	  (auto-fill-function (if (or arg flag) nil auto-fill-function)))
      (unwind-protect
	  (self-insert-command (prefix-numeric-value arg))
	;; If we get an error in self-insert-command, put point at right place.
	(if flag (forward-char 1))))
    ;; Even if we did *not* get an error, keep that forward-char;
    ;; all further processing should apply to the newline that the user
    ;; thinks he inserted.

    ;; Mark the newline(s) `hard'.
    (if use-hard-newlines
	(set-hard-newline-properties
	 (- (point) (if arg (prefix-numeric-value arg) 1)) (point)))
    ;; If the newline leaves the previous line blank,
    ;; and we have a left margin, delete that from the blank line.
    (or flag
	(save-excursion
	  (goto-char beforepos)
	  (beginning-of-line)
	  (and (looking-at "[ \t]$")
	       (> (current-left-margin) 0)
	       (delete-region (point) (progn (end-of-line) (point))))))
    ;; Indent the line after the newline, except in one case:
    ;; when we added the newline at the beginning of a line
    ;; which starts a page.
    (or was-page-start
	(move-to-left-margin nil t)))
  nil)

(defun set-hard-newline-properties (from to)
  (let ((sticky (get-text-property from 'rear-nonsticky)))
    (put-text-property from to 'hard 't)
    ;; If rear-nonsticky is not "t", add 'hard to rear-nonsticky list
    (if (and (listp sticky) (not (memq 'hard sticky)))
	(put-text-property from (point) 'rear-nonsticky
			   (cons 'hard sticky)))))

(defun open-line (arg)
  "Insert a newline and leave point before it.
If there is a fill prefix and/or a left-margin, insert them on the new line
if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (newline arg)
    (goto-char loc)
    (while (> arg 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq arg (1- arg)))
    (goto-char loc)
    (end-of-line)))

(defun split-line ()
  "Split current line, moving portion beyond point vertically down."
  (interactive "*")
  (skip-chars-forward " \t")
  (let ((col (current-column))
	(pos (point)))
    (newline 1)
    (indent-to col 0)
    (goto-char pos)))

(defun delete-indentation (&optional arg)
  "Join this line to previous and fix up whitespace at join.
If there is a fill prefix, delete it from the beginning of this line.
With argument, join this line to following line."
  (interactive "*P")
  (beginning-of-line)
  (if arg (forward-line 1))
  (if (eq (preceding-char) ?\n)
      (progn
	(delete-region (point) (1- (point)))
	;; If the second line started with the fill prefix,
	;; delete the prefix.
	(if (and fill-prefix
		 (<= (+ (point) (length fill-prefix)) (point-max))
		 (string= fill-prefix
			  (buffer-substring (point)
					    (+ (point) (length fill-prefix)))))
	    (delete-region (point) (+ (point) (length fill-prefix))))
	(fixup-whitespace))))

(defalias 'join-line #'delete-indentation) ; easier to find

(defun delete-blank-lines ()
  "On blank line, delete all surrounding blank lines, leaving just one.
On isolated blank line, delete that one.
On nonblank line, delete any immediately following blank lines."
  (interactive "*")
  (let (thisblank singleblank)
    (save-excursion
      (beginning-of-line)
      (setq thisblank (looking-at "[ \t]*$"))
      ;; Set singleblank if there is just one blank line here.
      (setq singleblank
	    (and thisblank
		 (not (looking-at "[ \t]*\n[ \t]*$"))
		 (or (bobp)
		     (progn (forward-line -1)
			    (not (looking-at "[ \t]*$")))))))
    ;; Delete preceding blank lines, and this one too if it's the only one.
    (if thisblank
	(progn
	  (beginning-of-line)
	  (if singleblank (forward-line 1))
	  (delete-region (point)
			 (if (re-search-backward "[^ \t\n]" nil t)
			     (progn (forward-line 1) (point))
			   (point-min)))))
    ;; Delete following blank lines, unless the current line is blank
    ;; and there are no following blank lines.
    (if (not (and thisblank singleblank))
	(save-excursion
	  (end-of-line)
	  (forward-line 1)
	  (delete-region (point)
			 (if (re-search-forward "[^ \t\n]" nil t)
			     (progn (beginning-of-line) (point))
			   (point-max)))))
    ;; Handle the special case where point is followed by newline and eob.
    ;; Delete the line, leaving point at eob.
    (if (looking-at "^[ \t]*\n\\'")
	(delete-region (point) (point-max)))))

(defun delete-trailing-whitespace ()
  "Delete all the trailing whitespace across the current buffer.
All whitespace after the last non-whitespace character in a line is deleted.
This respects narrowing, created by \\[narrow-to-region] and friends.
A formfeed is not considered whitespace by this function."
  (interactive "*")
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\s-$" nil t)
	(skip-syntax-backward "-" (save-excursion (forward-line 0) (point)))
	;; Don't delete formfeeds, even if they are considered whitespace.
	(save-match-data
	  (if (looking-at ".*\f")
	      (goto-char (match-end 0))))
	(delete-region (point) (match-end 0))))))

(defun newline-and-indent ()
  "Insert a newline, then indent according to major mode.
Indentation is done using the value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this command indents to the
column specified by the function `current-left-margin'."
  (interactive "*")
  (delete-horizontal-space t)
  (newline)
  (indent-according-to-mode))

(defun reindent-then-newline-and-indent ()
  "Reindent current line, insert newline, then indent the new line.
Indentation of both lines is done according to the current major mode,
which means calling the current value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents to the
column specified by the function `current-left-margin'."
  (interactive "*")
  (delete-horizontal-space t)
  (let ((pos (point)))
    ;; Be careful to insert the newline before indenting the line.
    ;; Otherwise, the indentation might be wrong.
    (newline)
    (save-excursion
      (goto-char pos)
      (indent-according-to-mode))
    (indent-according-to-mode)))

(defun quoted-insert (arg)
  "Read next input character and insert it.
This is useful for inserting control characters.

If the first character you type after this command is an octal digit,
you should type a sequence of octal digits which specify a character code.
Any nondigit terminates the sequence.  If the terminator is a RET,
it is discarded; any other terminator is used itself as input.
The variable `read-quoted-char-radix' specifies the radix for this feature;
set it to 10 or 16 to use decimal or hex instead of octal.

In overwrite mode, this function inserts the character anyway, and
does not handle octal digits specially.  This means that if you use
overwrite as your normal editing mode, you can use this function to
insert characters when necessary.

In binary overwrite mode, this function does overwrite, and octal
digits are interpreted as a character code.  This is intended to be
useful for editing binary files."
  (interactive "*p")
  (let ((char (if (or (not overwrite-mode)
		      (eq overwrite-mode 'overwrite-mode-binary))
		  (read-quoted-char)
		(read-char))))
    ;; Assume character codes 0240 - 0377 stand for characters in some
    ;; single-byte character set, and convert them to Emacs
    ;; characters.
    (if (and enable-multibyte-characters
	     (>= char ?\240)
	     (<= char ?\377))
	(setq char (unibyte-char-to-multibyte char)))
    (if (> arg 0)
	(if (eq overwrite-mode 'overwrite-mode-binary)
	    (delete-char arg)))
    (while (> arg 0)
      (insert-and-inherit char)
      (setq arg (1- arg)))))

(defun forward-to-indentation (arg)
  "Move forward ARG lines and position at first nonblank character."
  (interactive "p")
  (forward-line arg)
  (skip-chars-forward " \t"))

(defun backward-to-indentation (arg)
  "Move backward ARG lines and position at first nonblank character."
  (interactive "p")
  (forward-line (- arg))
  (skip-chars-forward " \t"))

(defun back-to-indentation ()
  "Move point to the first non-whitespace character on this line."
  (interactive)
  (beginning-of-line 1)
  (skip-chars-forward " \t"))

(defun fixup-whitespace ()
  "Fixup white space between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|\\s)")
	    (save-excursion (forward-char -1)
			    (looking-at "$\\|\\s(\\|\\s'")))
	nil
      (insert ?\ ))))

(defun delete-horizontal-space (&optional backward-only)
  "Delete all spaces and tabs around point.
If BACKWARD-ONLY is non-nil, only delete spaces before point."
  (interactive "*")
  (let ((orig-pos (point)))
    (delete-region
     (if backward-only
	 orig-pos
       (progn
	 (skip-chars-forward " \t")
	 (constrain-to-field nil orig-pos t)))
     (progn
       (skip-chars-backward " \t")
       (constrain-to-field nil orig-pos)))))

(defun just-one-space ()
  "Delete all spaces and tabs around point, leaving one space."
  (interactive "*")
  (let ((orig-pos (point)))
    (skip-chars-backward " \t")
    (constrain-to-field nil orig-pos)
    (if (= (following-char) ? )
	(forward-char 1)
      (insert ? ))
    (delete-region
     (point)
     (progn
       (skip-chars-forward " \t")
       (constrain-to-field nil orig-pos t)))))

(defun beginning-of-buffer (&optional arg)
  "Move point to the beginning of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the beginning.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.

Don't use this command in Lisp programs!
\(goto-char (point-min)) is faster and avoids clobbering the mark."
  (interactive "P")
  (push-mark)
  (let ((size (- (point-max) (point-min))))
    (goto-char (if arg
		   (+ (point-min)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (+ 10 (* size (prefix-numeric-value arg))) 10)))
		 (point-min))))
  (if arg (forward-line 1)))

(defun end-of-buffer (&optional arg)
  "Move point to the end of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the end.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer.

Don't use this command in Lisp programs!
\(goto-char (point-max)) is faster and avoids clobbering the mark."
  (interactive "P")
  (push-mark)
  (let ((size (- (point-max) (point-min))))
    (goto-char (if arg
		   (- (point-max)
		      (if (> size 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			(/ (* size (prefix-numeric-value arg)) 10)))
		 (point-max))))
  ;; If we went to a place in the middle of the buffer,
  ;; adjust it to the beginning of a line.
  (cond (arg (forward-line 1))
	((> (point) (window-end nil t))
	 ;; If the end of the buffer is not already on the screen,
	 ;; then scroll specially to put it near, but not at, the bottom.
	 (overlay-recenter (point))
	 (recenter -3))))

(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer.
You probably should not use this function in Lisp programs;
it is usually a mistake for a Lisp function to use any subroutine
that uses or sets the mark."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min)))


;; Counting lines, one way or another.

(defun goto-line (arg)
  "Goto line ARG, counting from line 1 at beginning of buffer."
  (interactive "NGoto line: ")
  (setq arg (prefix-numeric-value arg))
  (save-restriction
    (widen)
    (goto-char 1)
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- arg))
      (forward-line (1- arg)))))

(defun count-lines-region (start end)
  "Print number of lines and characters in the region."
  (interactive "r")
  (message "Region has %d lines, %d characters"
	   (count-lines start end) (- end start)))

(defun what-line ()
  "Print the current buffer line number and narrowed line number of point."
  (interactive)
  (let ((opoint (point)) start)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(widen)
	(forward-line 0)
	(setq start (point))
	(goto-char opoint)
	(forward-line 0)
	(if (/= start 1)
	    (message "line %d (narrowed line %d)"
		     (1+ (count-lines 1 (point)))
		     (1+ (count-lines start (point))))
	  (message "Line %d" (1+ (count-lines 1 (point)))))))))

(defun count-lines (start end)
  "Return number of lines between START and END.
This is usually the number of newlines between them,
but can be one more if START is not equal to END
and the greater of them is not at the start of a line."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (eq selective-display t)
	  (save-match-data
	    (let ((done 0))
	      (while (re-search-forward "[\n\C-m]" nil t 40)
		(setq done (+ 40 done)))
	      (while (re-search-forward "[\n\C-m]" nil t 1)
		(setq done (+ 1 done)))
	      (goto-char (point-max))
	      (if (and (/= start end)
		       (not (bolp)))
		  (1+ done)
		done)))
	(- (buffer-size) (forward-line (buffer-size)))))))

(defun what-cursor-position (&optional detail)
  "Print info on cursor position (on screen and within buffer).
Also describe the character after point, and give its character code
in octal, decimal and hex.

For a non-ASCII multibyte character, also give its encoding in the
buffer's selected coding system if the coding system encodes the
character safely.  If the character is encoded into one byte, that
code is shown in hex.  If the character is encoded into more than one
byte, just \"...\" is shown.

In addition, with prefix argument, show details about that character
in *Help* buffer.  See also the command `describe-char-after'."
  (interactive "P")
  (let* ((char (following-char))
	 (beg (point-min))
	 (end (point-max))
         (pos (point))
	 (total (buffer-size))
	 (percent (if (> total 50000)
		      ;; Avoid overflow from multiplying by 100!
		      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
		    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
	 (hscroll (if (= (window-hscroll) 0)
		      ""
		    (format " Hscroll=%d" (window-hscroll))))
	 (col (current-column)))
    (if (= pos end)
	(if (or (/= beg 1) (/= end (1+ total)))
	    (message "point=%d of %d (%d%%) <%d - %d> column %d %s"
		     pos total percent beg end col hscroll)
	  (message "point=%d of %d (%d%%) column %d %s"
		   pos total percent col hscroll))
      (let ((coding buffer-file-coding-system)
	    encoded encoding-msg)
	(if (or (not coding)
		(eq (coding-system-type coding) t))
	    (setq coding default-buffer-file-coding-system))
	(if (not (char-valid-p char))
	    (setq encoding-msg
		  (format "(0%o, %d, 0x%x, invalid)" char char char))
	  (setq encoded (and (>= char 128) (encode-coding-char char coding)))
	  (setq encoding-msg
		(if encoded
		    (format "(0%o, %d, 0x%x, file %s)"
			    char char char
			    (if (> (length encoded) 1)
				"..."
			      (encoded-string-description encoded coding)))
		  (format "(0%o, %d, 0x%x)" char char char))))
	(if detail
	    ;; We show the detailed information about CHAR.
	    (describe-char-after (point)))
	(if (or (/= beg 1) (/= end (1+ total)))
	    (message "Char: %s %s point=%d of %d (%d%%) <%d - %d> column %d %s"
		     (if (< char 256)
			 (single-key-description char)
		       (buffer-substring-no-properties (point) (1+ (point))))
		     encoding-msg pos total percent beg end col hscroll)
	  (message "Char: %s %s point=%d of %d (%d%%) column %d %s"
		   (if (< char 256)
		       (single-key-description char)
		     (buffer-substring-no-properties (point) (1+ (point))))
		   encoding-msg pos total percent col hscroll))))))

(defvar read-expression-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\t" 'lisp-complete-symbol)
    (set-keymap-parent m minibuffer-local-map)
    m)
  "Minibuffer keymap used for reading Lisp expressions.")

(defvar read-expression-history nil)

(defcustom eval-expression-print-level 4
  "*Value to use for `print-level' when printing value in `eval-expression'.
A value of nil means no limit."
  :group 'lisp
  :type '(choice (const :tag "No Limit" nil) integer)
  :version "21.1")

(defcustom eval-expression-print-length 12
  "*Value to use for `print-length' when printing value in `eval-expression'.
A value of nil means no limit."
  :group 'lisp
  :type '(choice (const :tag "No Limit" nil) integer)
  :version "21.1")

(defcustom eval-expression-debug-on-error t
  "*Non-nil means set `debug-on-error' when evaluating in `eval-expression'.
If nil, don't change the value of `debug-on-error'."
  :group 'lisp
  :type 'boolean
  :version "21.1")

;; We define this, rather than making `eval' interactive,
;; for the sake of completion of names like eval-region, eval-current-buffer.
(defun eval-expression (eval-expression-arg
			&optional eval-expression-insert-value)
  "Evaluate EVAL-EXPRESSION-ARG and print value in the echo area.
Value is also consed on to front of the variable `values'.
Optional argument EVAL-EXPRESSION-INSERT-VALUE, if non-nil, means
insert the result into the current buffer instead of printing it in
the echo area."
  (interactive
   (list (read-from-minibuffer "Eval: "
			       nil read-expression-map t
			       'read-expression-history)
	 current-prefix-arg))

  (if (null eval-expression-debug-on-error)
      (setq values (cons (eval eval-expression-arg) values))
    (let ((old-value (make-symbol "t")) new-value)
      ;; Bind debug-on-error to something unique so that we can
      ;; detect when evaled code changes it.
      (let ((debug-on-error old-value))
	(setq values (cons (eval eval-expression-arg) values))
	(setq new-value debug-on-error))
      ;; If evaled code has changed the value of debug-on-error,
      ;; propagate that change to the global binding.
      (unless (eq old-value new-value)
	(setq debug-on-error new-value))))

  (let ((print-length eval-expression-print-length)
	(print-level eval-expression-print-level))
    (prin1 (car values)
	   (if eval-expression-insert-value (current-buffer) t))))

(defun edit-and-eval-command (prompt command)
  "Prompting with PROMPT, let user edit COMMAND and eval result.
COMMAND is a Lisp expression.  Let user edit that expression in
the minibuffer, then read and evaluate the result."
  (let ((command (read-from-minibuffer prompt
				       (prin1-to-string command)
				       read-expression-map t
				       '(command-history . 1))))
    ;; If command was added to command-history as a string,
    ;; get rid of that.  We want only evaluable expressions there.
    (if (stringp (car command-history))
	(setq command-history (cdr command-history)))

    ;; If command to be redone does not match front of history,
    ;; add it to the history.
    (or (equal command (car command-history))
	(setq command-history (cons command command-history)))
    (eval command)))

(defun repeat-complex-command (arg)
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history.
You can use the minibuffer history commands \\<minibuffer-local-map>\\[next-history-element] and \\[previous-history-element]
to get different commands to edit and resubmit."
  (interactive "p")
  (let ((elt (nth (1- arg) command-history))
	newcmd)
    (if elt
	(progn
	  (setq newcmd
		(let ((print-level nil)
		      (minibuffer-history-position arg)
		      (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
		  (read-from-minibuffer
		   "Redo: " (prin1-to-string elt) read-expression-map t
		   (cons 'command-history arg))))

	  ;; If command was added to command-history as a string,
	  ;; get rid of that.  We want only evaluable expressions there.
	  (if (stringp (car command-history))
	      (setq command-history (cdr command-history)))

	  ;; If command to be redone does not match front of history,
	  ;; add it to the history.
	  (or (equal newcmd (car command-history))
	      (setq command-history (cons newcmd command-history)))
	  (eval newcmd))
      (ding))))

(defvar minibuffer-history nil
  "Default minibuffer history list.
This is used for all minibuffer input
except when an alternate history list is specified.")
(defvar minibuffer-history-sexp-flag nil
  "Non-nil when doing history operations on the variable `command-history'.
More generally, indicates that the history list being acted on
contains expressions rather than strings.
It is only valid if its value equals the current minibuffer depth,
to handle recursive uses of the minibuffer.")
(setq minibuffer-history-variable 'minibuffer-history)
(setq minibuffer-history-position nil)
(defvar minibuffer-history-search-history nil)

(defvar minibuffer-text-before-history nil
  "Text that was in this minibuffer before any history commands.
This is nil if there have not yet been any history commands
in this use of the minibuffer.")

(add-hook 'minibuffer-setup-hook 'minibuffer-history-initialize)

(defun minibuffer-history-initialize ()
  (setq minibuffer-text-before-history nil))

(defun minibuffer-avoid-prompt (new old)
  "A point-motion hook for the minibuffer, that moves point out of the prompt."
  (constrain-to-field nil (point-max)))

(defcustom minibuffer-history-case-insensitive-variables nil
  "*Minibuffer history variables for which matching should ignore case.
If a history variable is a member of this list, then the
\\[previous-matching-history-element] and \\[next-matching-history-element]\
 commands ignore case when searching it, regardless of `case-fold-search'."
  :type '(repeat variable)
  :group 'minibuffer)

(defun previous-matching-history-element (regexp n)
  "Find the previous history element that matches REGEXP.
\(Previous history elements refer to earlier actions.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match.
Normally, history elements are matched case-insensitively if
`case-fold-search' is non-nil, but an uppercase letter in REGEXP
makes the search case-sensitive.
See also `minibuffer-history-case-insensitive-variables'."
  (interactive
   (let* ((enable-recursive-minibuffers t)
	  (regexp (read-from-minibuffer "Previous element matching (regexp): "
					nil
					minibuffer-local-map
					nil
					'minibuffer-history-search-history)))
     ;; Use the last regexp specified, by default, if input is empty.
     (list (if (string= regexp "")
	       (if minibuffer-history-search-history
		   (car minibuffer-history-search-history)
		 (error "No previous history search regexp"))
	     regexp)
	   (prefix-numeric-value current-prefix-arg))))
  (unless (zerop n)
    (if (and (zerop minibuffer-history-position)
	     (null minibuffer-text-before-history))
	(setq minibuffer-text-before-history
	      (minibuffer-contents-no-properties)))
    (let ((history (symbol-value minibuffer-history-variable))
	  (case-fold-search
	   (if (isearch-no-upper-case-p regexp t) ; assume isearch.el is dumped
	       ;; On some systems, ignore case for file names.
	       (if (memq minibuffer-history-variable
			 minibuffer-history-case-insensitive-variables)
		   t
		 ;; Respect the user's setting for case-fold-search:
		 case-fold-search)
	     nil))
	  prevpos
	  match-string
	  match-offset
	  (pos minibuffer-history-position))
      (while (/= n 0)
	(setq prevpos pos)
	(setq pos (min (max 1 (+ pos (if (< n 0) -1 1))) (length history)))
	(when (= pos prevpos)
	  (error (if (= pos 1)
		     "No later matching history item"
		   "No earlier matching history item")))
	(setq match-string
	      (if (eq minibuffer-history-sexp-flag (minibuffer-depth))
		  (let ((print-level nil))
		    (prin1-to-string (nth (1- pos) history)))
		(nth (1- pos) history)))
	(setq match-offset
	      (if (< n 0)
		  (and (string-match regexp match-string)
		       (match-end 0))
		(and (string-match (concat ".*\\(" regexp "\\)") match-string)
		     (match-beginning 1))))
	(when match-offset
	  (setq n (+ n (if (< n 0) 1 -1)))))
      (setq minibuffer-history-position pos)
      (goto-char (point-max))
      (delete-minibuffer-contents)
      (insert match-string)
      (goto-char (+ (minibuffer-prompt-end) match-offset))))
  (if (memq (car (car command-history)) '(previous-matching-history-element
					  next-matching-history-element))
      (setq command-history (cdr command-history))))

(defun next-matching-history-element (regexp n)
  "Find the next history element that matches REGEXP.
\(The next history element refers to a more recent action.)
With prefix argument N, search for Nth next match.
If N is negative, find the previous or Nth previous match.
Normally, history elements are matched case-insensitively if
`case-fold-search' is non-nil, but an uppercase letter in REGEXP
makes the search case-sensitive."
  (interactive
   (let* ((enable-recursive-minibuffers t)
	  (regexp (read-from-minibuffer "Next element matching (regexp): "
					nil
					minibuffer-local-map
					nil
					'minibuffer-history-search-history)))
     ;; Use the last regexp specified, by default, if input is empty.
     (list (if (string= regexp "")
	       (setcar minibuffer-history-search-history
		       (nth 1 minibuffer-history-search-history))
	     regexp)
	   (prefix-numeric-value current-prefix-arg))))
  (previous-matching-history-element regexp (- n)))

(defvar minibuffer-temporary-goal-position nil)

(defun next-history-element (n)
  "Insert the next element of the minibuffer history into the minibuffer."
  (interactive "p")
  (or (zerop n)
      (let ((narg (- minibuffer-history-position n))
	    (minimum (if minibuffer-default -1 0))
	    elt minibuffer-returned-to-present)
	(if (and (zerop minibuffer-history-position)
		 (null minibuffer-text-before-history))
	    (setq minibuffer-text-before-history
		  (minibuffer-contents-no-properties)))
	(if (< narg minimum)
	    (if minibuffer-default
		(error "End of history; no next item")
	      (error "End of history; no default available")))
	(if (> narg (length (symbol-value minibuffer-history-variable)))
	    (error "Beginning of history; no preceding item"))
	(unless (memq last-command '(next-history-element
				     previous-history-element))
	  (let ((prompt-end (minibuffer-prompt-end)))
	    (set (make-local-variable 'minibuffer-temporary-goal-position)
		 (cond ((<= (point) prompt-end) prompt-end)
		       ((eobp) nil)
		       (t (point))))))
	(goto-char (point-max))
	(delete-minibuffer-contents)
	(setq minibuffer-history-position narg)
	(cond ((= narg -1)
	       (setq elt minibuffer-default))
	      ((= narg 0)
	       (setq elt (or minibuffer-text-before-history ""))
	       (setq minibuffer-returned-to-present t)
	       (setq minibuffer-text-before-history nil))
	      (t (setq elt (nth (1- minibuffer-history-position)
				(symbol-value minibuffer-history-variable)))))
	(insert
	 (if (and (eq minibuffer-history-sexp-flag (minibuffer-depth))
		  (not minibuffer-returned-to-present))
	     (let ((print-level nil))
	       (prin1-to-string elt))
	   elt))
	(goto-char (or minibuffer-temporary-goal-position (point-max))))))

(defun previous-history-element (n)
  "Inserts the previous element of the minibuffer history into the minibuffer."
  (interactive "p")
  (next-history-element (- n)))

(defun next-complete-history-element (n)
  "Get next history element which completes the minibuffer before the point.
The contents of the minibuffer after the point are deleted, and replaced
by the new completion."
  (interactive "p")
  (let ((point-at-start (point)))
    (next-matching-history-element
     (concat
      "^" (regexp-quote (buffer-substring (minibuffer-prompt-end) (point))))
     n)
    ;; next-matching-history-element always puts us at (point-min).
    ;; Move to the position we were at before changing the buffer contents.
    ;; This is still sensical, because the text before point has not changed.
    (goto-char point-at-start)))

(defun previous-complete-history-element (n)
  "\
Get previous history element which completes the minibuffer before the point.
The contents of the minibuffer after the point are deleted, and replaced
by the new completion."
  (interactive "p")
  (next-complete-history-element (- n)))

;; For compatibility with the old subr of the same name.
(defun minibuffer-prompt-width ()
  "Return the display width of the minibuffer prompt.
Return 0 if current buffer is not a mini-buffer."
  ;; Return the width of everything before the field at the end of
  ;; the buffer; this should be 0 for normal buffers.
  (1- (minibuffer-prompt-end)))

;Put this on C-x u, so we can force that rather than C-_ into startup msg
(defalias 'advertised-undo 'undo)

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count.

In Transient Mark mode when the mark is active, only undo changes within
the current region.  Similarly, when not in Transient Mark mode, just C-u
as an argument limits undo to changes within the current region."
  (interactive "*P")
  ;; Make last-command indicate for the next command that this was an undo.
  ;; That way, another undo will undo more.
  ;; If we get to the end of the undo history and get an error,
  ;; another undo command will find the undo history empty
  ;; and will get another error.  To begin undoing the undos,
  ;; you must type some other command.
  (setq this-command 'undo)
  (let ((modified (buffer-modified-p))
	(recent-save (recent-auto-save-p)))
    (or (eq (selected-window) (minibuffer-window))
	(message "Undo!"))
    (unless (eq last-command 'undo)
      (if (if transient-mark-mode mark-active (and arg (not (numberp arg))))
	  (undo-start (region-beginning) (region-end))
	(undo-start))
      ;; get rid of initial undo boundary
      (undo-more 1))
    (undo-more
     (if (or transient-mark-mode (numberp arg))
	 (prefix-numeric-value arg)
       1))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    (let ((tail buffer-undo-list)
	  (prev nil))
      (while (car tail)
	(when (integerp (car tail))
	  (let ((pos (car tail)))
	    (if (null prev)
		(setq buffer-undo-list (cdr tail))
	      (setcdr prev (cdr tail)))
	    (setq tail (cdr tail))
	    (while (car tail)
	      (if (eq pos (car tail))
		  (if prev
		      (setcdr prev (cdr tail))
		    (setq buffer-undo-list (cdr tail)))
		(setq prev tail))
	      (setq tail (cdr tail)))
	    (setq tail nil)))
	(setq prev tail tail (cdr tail))))

    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary recent-save))))

(defvar pending-undo-list nil
  "Within a run of consecutive undo commands, list remaining to be undone.")

(defvar undo-in-progress nil
  "Non-nil while performing an undo.
Some change-hooks test this variable to do something different.")

(defun undo-more (count)
  "Undo back N undo-boundaries beyond what was already undone recently.
Call `undo-start' to get ready to undo recent changes,
then call `undo-more' one or more times to undo them."
  (or pending-undo-list
      (error "No further undo information"))
  (let ((undo-in-progress t))
    (setq pending-undo-list (primitive-undo count pending-undo-list))))

;; Deep copy of a list
(defun undo-copy-list (list)
  "Make a copy of undo list LIST."
  (mapcar 'undo-copy-list-1 list))

(defun undo-copy-list-1 (elt)
  (if (consp elt)
      (cons (car elt) (undo-copy-list-1 (cdr elt)))
    elt))

(defun undo-start (&optional beg end)
  "Set `pending-undo-list' to the front of the undo list.
The next call to `undo-more' will undo the most recently made change.
If BEG and END are specified, then only undo elements
that apply to text between BEG and END are used; other undo elements
are ignored.  If BEG and END are nil, all undo elements are used."
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (setq pending-undo-list
	(if (and beg end (not (= beg end)))
	    (undo-make-selective-list (min beg end) (max beg end))
	  buffer-undo-list)))

(defvar undo-adjusted-markers)

(defun undo-make-selective-list (start end)
  "Return a list of undo elements for the region START to END.
The elements come from `buffer-undo-list', but we keep only
the elements inside this region, and discard those outside this region.
If we find an element that crosses an edge of this region,
we stop and ignore all further elements."
  (let ((undo-list-copy (undo-copy-list buffer-undo-list))
	(undo-list (list nil))
	undo-adjusted-markers
	some-rejected
	undo-elt undo-elt temp-undo-list delta)
    (while undo-list-copy
      (setq undo-elt (car undo-list-copy))
      (let ((keep-this
	     (cond ((and (consp undo-elt) (eq (car undo-elt) t))
		    ;; This is a "was unmodified" element.
		    ;; Keep it if we have kept everything thus far.
		    (not some-rejected))
		   (t
		    (undo-elt-in-region undo-elt start end)))))
	(if keep-this
	    (progn
	      (setq end (+ end (cdr (undo-delta undo-elt))))
	      ;; Don't put two nils together in the list
	      (if (not (and (eq (car undo-list) nil)
			    (eq undo-elt nil)))
		  (setq undo-list (cons undo-elt undo-list))))
	  (if (undo-elt-crosses-region undo-elt start end)
	      (setq undo-list-copy nil)
	    (setq some-rejected t)
	    (setq temp-undo-list (cdr undo-list-copy))
	    (setq delta (undo-delta undo-elt))

	    (when (/= (cdr delta) 0)
	      (let ((position (car delta))
		    (offset (cdr delta)))

		;; Loop down the earlier events adjusting their buffer
		;; positions to reflect the fact that a change to the buffer
		;; isn't being undone. We only need to process those element
		;; types which undo-elt-in-region will return as being in
		;; the region since only those types can ever get into the
		;; output

		(while temp-undo-list
		  (setq undo-elt (car temp-undo-list))
		  (cond ((integerp undo-elt)
			 (if (>= undo-elt position)
			     (setcar temp-undo-list (- undo-elt offset))))
			((atom undo-elt) nil)
			((stringp (car undo-elt))
			 ;; (TEXT . POSITION)
			 (let ((text-pos (abs (cdr undo-elt)))
			       (point-at-end (< (cdr undo-elt) 0 )))
			   (if (>= text-pos position)
			       (setcdr undo-elt (* (if point-at-end -1 1)
						   (- text-pos offset))))))
			((integerp (car undo-elt))
			 ;; (BEGIN . END)
			 (when (>= (car undo-elt) position)
			   (setcar undo-elt (- (car undo-elt) offset))
			   (setcdr undo-elt (- (cdr undo-elt) offset))))
			((null (car undo-elt))
			 ;; (nil PROPERTY VALUE BEG . END)
			 (let ((tail (nthcdr 3 undo-elt)))
			   (when (>= (car tail) position)
			     (setcar tail (- (car tail) offset))
			     (setcdr tail (- (cdr tail) offset))))))
		  (setq temp-undo-list (cdr temp-undo-list))))))))
      (setq undo-list-copy (cdr undo-list-copy)))
    (nreverse undo-list)))

(defun undo-elt-in-region (undo-elt start end)
  "Determine whether UNDO-ELT falls inside the region START ... END.
If it crosses the edge, we return nil."
  (cond ((integerp undo-elt)
	 (and (>= undo-elt start)
	      (<  undo-elt end)))
	((eq undo-elt nil)
	 t)
	((atom undo-elt)
	 nil)
	((stringp (car undo-elt))
	 ;; (TEXT . POSITION)
	 (and (>= (abs (cdr undo-elt)) start)
	      (< (abs (cdr undo-elt)) end)))
	((and (consp undo-elt) (markerp (car undo-elt)))
	 ;; This is a marker-adjustment element (MARKER . ADJUSTMENT).
	 ;; See if MARKER is inside the region.
	 (let ((alist-elt (assq (car undo-elt) undo-adjusted-markers)))
	   (unless alist-elt
	     (setq alist-elt (cons (car undo-elt)
				   (marker-position (car undo-elt))))
	     (setq undo-adjusted-markers
		   (cons alist-elt undo-adjusted-markers)))
	   (and (cdr alist-elt)
		(>= (cdr alist-elt) start)
		(< (cdr alist-elt) end))))
	((null (car undo-elt))
	 ;; (nil PROPERTY VALUE BEG . END)
	 (let ((tail (nthcdr 3 undo-elt)))
	   (and (>= (car tail) start)
		(< (cdr tail) end))))
	((integerp (car undo-elt))
	 ;; (BEGIN . END)
	 (and (>= (car undo-elt) start)
	      (< (cdr undo-elt) end)))))

(defun undo-elt-crosses-region (undo-elt start end)
  "Test whether UNDO-ELT crosses one edge of that region START ... END.
This assumes we have already decided that UNDO-ELT
is not *inside* the region START...END."
  (cond ((atom undo-elt) nil)
	((null (car undo-elt))
	 ;; (nil PROPERTY VALUE BEG . END)
	 (let ((tail (nthcdr 3 undo-elt)))
	   (not (or (< (car tail) end)
		    (> (cdr tail) start)))))
	((integerp (car undo-elt))
	 ;; (BEGIN . END)
	 (not (or (< (car undo-elt) end)
		  (> (cdr undo-elt) start))))))

;; Return the first affected buffer position and the delta for an undo element
;; delta is defined as the change in subsequent buffer positions if we *did*
;; the undo.
(defun undo-delta (undo-elt)
  (if (consp undo-elt)
      (cond ((stringp (car undo-elt))
	     ;; (TEXT . POSITION)
	     (cons (abs (cdr undo-elt)) (length (car undo-elt))))
	    ((integerp (car undo-elt))
	     ;; (BEGIN . END)
	     (cons (car undo-elt) (- (car undo-elt) (cdr undo-elt))))
	    (t
	     '(0 . 0)))
    '(0 . 0)))

(defvar shell-command-history nil
  "History list for some commands that read shell commands.")

(defvar shell-command-switch "-c"
  "Switch used to have the shell execute its command line argument.")

(defvar shell-command-default-error-buffer nil
  "*Buffer name for `shell-command' and `shell-command-on-region' error output.
This buffer is used when `shell-command' or 'shell-command-on-region'
is run interactively.  A value of nil means that output to stderr and
stdout will be intermixed in the output stream.")

(defun shell-command (command &optional output-buffer error-buffer)
  "Execute string COMMAND in inferior shell; display output, if any.
With prefix argument, insert the COMMAND's output at point.

If COMMAND ends in ampersand, execute it asynchronously.
The output appears in the buffer `*Async Shell Command*'.
That buffer is in shell mode.

Otherwise, COMMAND is executed synchronously.  The output appears in
the buffer `*Shell Command Output*'.  If the output is short enough to
display in the echo area (which is determined by the variables
`resize-mini-windows' and `max-mini-window-height'), it is shown
there, but it is nonetheless available in buffer `*Shell Command
Output*' even though that buffer is not automatically displayed.

To specify a coding system for converting non-ASCII characters
in the shell command output, use \\[universal-coding-system-argument]
before this command.

Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

The optional second argument OUTPUT-BUFFER, if non-nil,
says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in current buffer.  (This cannot be done asynchronously.)
In either case, the output is inserted after point (leaving mark after it).

If the command terminates without error, but generates output,
and you did not specify \"insert it in the current buffer\",
the output can be displayed in the echo area or in its buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.  Otherwise,
the buffer containing the output is displayed.

If there is output and an error, and you did not specify \"insert it
in the current buffer\", a message about the error goes at the end
of the output.

If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional third argument ERROR-BUFFER is non-nil, it is a buffer
or buffer name to which to direct the command's standard error output.
If it is nil, error output is mingled with regular output.
In an interactive call, the variable `shell-command-default-error-buffer'
specifies the value of ERROR-BUFFER."

  (interactive (list (read-from-minibuffer "Shell command: "
					   nil nil nil 'shell-command-history)
		     current-prefix-arg
		     shell-command-default-error-buffer))
  ;; Look for a handler in case default-directory is a remote file name.
  (let ((handler
	 (find-file-name-handler (directory-file-name default-directory)
				 'shell-command)))
    (if handler
	(funcall handler 'shell-command command output-buffer error-buffer)
      (if (and output-buffer
	       (not (or (bufferp output-buffer)  (stringp output-buffer))))
	  ;; Output goes in current buffer.
	  (let ((error-file
		 (if error-buffer
		     (make-temp-file
		      (expand-file-name "scor"
					(or small-temporary-file-directory
					    temporary-file-directory)))
		   nil)))
	    (barf-if-buffer-read-only)
	    (push-mark nil t)
	    ;; We do not use -f for csh; we will not support broken use of
	    ;; .cshrcs.  Even the BSD csh manual says to use
	    ;; "if ($?prompt) exit" before things which are not useful
	    ;; non-interactively.  Besides, if someone wants their other
	    ;; aliases for shell commands then they can still have them.
	    (call-process shell-file-name nil
			  (if error-file
			      (list t error-file)
			    t)
			  nil shell-command-switch command)
	    (when (and error-file (file-exists-p error-file))
	      (if (< 0 (nth 7 (file-attributes error-file)))
		  (with-current-buffer (get-buffer-create error-buffer)
		    (let ((pos-from-end (- (point-max) (point))))
		      (or (bobp)
			  (insert "\f\n"))
		      ;; Do no formatting while reading error file,
		      ;; because that can run a shell command, and we
		      ;; don't want that to cause an infinite recursion.
		      (format-insert-file error-file nil)
		      ;; Put point after the inserted errors.
		      (goto-char (- (point-max) pos-from-end)))
		    (display-buffer (current-buffer))))
	      (delete-file error-file))
	    ;; This is like exchange-point-and-mark, but doesn't
	    ;; activate the mark.  It is cleaner to avoid activation,
	    ;; even though the command loop would deactivate the mark
	    ;; because we inserted text.
	    (goto-char (prog1 (mark t)
			 (set-marker (mark-marker) (point)
				     (current-buffer)))))
	;; Output goes in a separate buffer.
	;; Preserve the match data in case called from a program.
	(save-match-data
	  (if (string-match "[ \t]*&[ \t]*$" command)
	      ;; Command ending with ampersand means asynchronous.
	      (let ((buffer (get-buffer-create
			     (or output-buffer "*Async Shell Command*")))
		    (directory default-directory)
		    proc)
		;; Remove the ampersand.
		(setq command (substring command 0 (match-beginning 0)))
		;; If will kill a process, query first.
		(setq proc (get-buffer-process buffer))
		(if proc
		    (if (yes-or-no-p "A command is running.  Kill it? ")
			(kill-process proc)
		      (error "Shell command in progress")))
		(save-excursion
		  (set-buffer buffer)
		  (setq buffer-read-only nil)
		  (erase-buffer)
		  (display-buffer buffer)
		  (setq default-directory directory)
		  (setq proc (start-process "Shell" buffer shell-file-name
					    shell-command-switch command))
		  (setq mode-line-process '(":%s"))
		  (require 'shell) (shell-mode)
		  (set-process-sentinel proc 'shell-command-sentinel)
		  ))
	    (shell-command-on-region (point) (point) command
				     output-buffer nil error-buffer)))))))

(defun display-message-or-buffer (message
				  &optional buffer-name not-this-window frame)
  "Display MESSAGE in the echo area if possible, otherwise in a pop-up buffer.
MESSAGE may be either a string or a buffer.

A buffer is displayed using `display-buffer' if MESSAGE is too long for
the maximum height of the echo area, as defined by `max-mini-window-height'
if `resize-mini-windows' is non-nil.

Returns either the string shown in the echo area, or when a pop-up
buffer is used, the window used to display it.

If MESSAGE is a string, then the optional argument BUFFER-NAME is the
name of the buffer used to display it in the case where a pop-up buffer
is used, defaulting to `*Message*'.  In the case where MESSAGE is a
string and it is displayed in the echo area, it is not specified whether
the contents are inserted into the buffer anyway.

Optional arguments NOT-THIS-WINDOW and FRAME are as for `display-buffer',
and only used if a buffer is displayed."
  (cond ((and (stringp message) (not (string-match "\n" message)))
	 ;; Trivial case where we can use the echo area
	 (message "%s" message))
	((and (stringp message)
	      (= (string-match "\n" message) (1- (length message))))
	 ;; Trivial case where we can just remove single trailing newline
	 (message "%s" (substring message 0 (1- (length message)))))
	(t
	 ;; General case
	 (with-current-buffer
	     (if (bufferp message)
		 message
	       (get-buffer-create (or buffer-name "*Message*")))

	   (unless (bufferp message)
	     (erase-buffer)
	     (insert message))

	   (let ((lines
		  (if (= (buffer-size) 0)
		      0
		    (count-lines (point-min) (point-max)))))
	     (cond ((or (<= lines 1)
			(<= lines
			    (if resize-mini-windows
				(cond ((floatp max-mini-window-height)
				       (* (frame-height)
					  max-mini-window-height))
				      ((integerp max-mini-window-height)
				       max-mini-window-height)
				      (t
				       1))
			      1)))
		    ;; Echo area
		    (goto-char (point-max))
		    (when (bolp)
		      (backward-char 1))
		    (message "%s" (buffer-substring (point-min) (point))))
		   (t
		    ;; Buffer
		    (goto-char (point-min))
		    (display-buffer (current-buffer)
				    not-this-window frame))))))))


;; We have a sentinel to prevent insertion of a termination message
;; in the buffer itself.
(defun shell-command-sentinel (process signal)
  (if (memq (process-status process) '(exit signal))
      (message "%s: %s."
	       (car (cdr (cdr (process-command process))))
	       (substring signal 0 -1))))

(defun shell-command-on-region (start end command
				      &optional output-buffer replace
				      error-buffer)
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.  Return the exit code of
COMMAND.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell command, use \\[universal-coding-system-argument]
before this command.  By default, the input (from the current buffer)
is encoded in the same coding system that will be used to save the file,
`buffer-file-coding-system'.  If the output is going to replace the region,
then it is decoded from that same coding system.

The noninteractive arguments are START, END, COMMAND, OUTPUT-BUFFER,
REPLACE, ERROR-BUFFER.  Noninteractive callers can specify coding
systems by binding `coding-system-for-read' and
`coding-system-for-write'.

If the command generates output, the output may be displayed
in the echo area or in a buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.  Otherwise
it is displayed in the buffer `*Shell Command Output*'.  The output
is available in that buffer in both cases.

If there is output and an error, a message about the error
appears at the end of the output.

If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional fourth argument OUTPUT-BUFFER is non-nil,
that says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in the current buffer.
In either case, the output is inserted after point (leaving mark after it).

If REPLACE, the optional fifth argument, is non-nil, that means insert
the output in place of text from START to END, putting point and mark
around it.

If optional sixth argument ERROR-BUFFER is non-nil, it is a buffer
or buffer name to which to direct the command's standard error output.
If it is nil, error output is mingled with regular output.
In an interactive call, the variable `shell-command-default-error-buffer'
specifies the value of ERROR-BUFFER."
  (interactive (let (string)
		 (unless (mark)
		   (error "The mark is not set now, so there is no region"))
		 ;; Do this before calling region-beginning
		 ;; and region-end, in case subprocess output
		 ;; relocates them while we are in the minibuffer.
		 (setq string (read-from-minibuffer "Shell command on region: "
						    nil nil nil
						    'shell-command-history))
		 ;; call-interactively recognizes region-beginning and
		 ;; region-end specially, leaving them in the history.
		 (list (region-beginning) (region-end)
		       string
		       current-prefix-arg
		       current-prefix-arg
		       shell-command-default-error-buffer)))
  (let ((error-file
	 (if error-buffer
	     (make-temp-file
	      (expand-file-name "scor"
				(or small-temporary-file-directory
				    temporary-file-directory)))
	   nil))
	exit-status)
    (if (or replace
	    (and output-buffer
		 (not (or (bufferp output-buffer) (stringp output-buffer)))))
	;; Replace specified region with output from command.
	(let ((swap (and replace (< start end))))
	  ;; Don't muck with mark unless REPLACE says we should.
	  (goto-char start)
	  (and replace (push-mark (point) 'nomsg))
	  (setq exit-status
		(call-process-region start end shell-file-name t
				     (if error-file
					 (list t error-file)
				       t)
				     nil shell-command-switch command))
	  ;; It is rude to delete a buffer which the command is not using.
	  ;; (let ((shell-buffer (get-buffer "*Shell Command Output*")))
	  ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
	  ;; 	 (kill-buffer shell-buffer)))
	  ;; Don't muck with mark unless REPLACE says we should.
	  (and replace swap (exchange-point-and-mark)))
      ;; No prefix argument: put the output in a temp buffer,
      ;; replacing its entire contents.
      (let ((buffer (get-buffer-create
		     (or output-buffer "*Shell Command Output*")))
	    (success nil))
	(unwind-protect
	    (if (eq buffer (current-buffer))
		;; If the input is the same buffer as the output,
		;; delete everything but the specified region,
		;; then replace that region with the output.
		(progn (setq buffer-read-only nil)
		       (delete-region (max start end) (point-max))
		       (delete-region (point-min) (min start end))
		       (setq exit-status
			     (call-process-region (point-min) (point-max)
						  shell-file-name t
						  (if error-file
						      (list t error-file)
						    t)
						  nil shell-command-switch
						  command)))
	      ;; Clear the output buffer, then run the command with
	      ;; output there.
	      (let ((directory default-directory))
		(save-excursion
		  (set-buffer buffer)
		  (setq buffer-read-only nil)
		  (if (not output-buffer)
		      (setq default-directory directory))
		  (erase-buffer)))
	      (setq exit-status
		    (call-process-region start end shell-file-name nil
					 (if error-file
					     (list buffer error-file)
					   buffer)
					 nil shell-command-switch command)))
	  (setq success (and exit-status (equal 0 exit-status)))
	  ;; Report the output.
	  (with-current-buffer buffer
	    (setq mode-line-process 
		  (if (not success)
		    (concat (format " - Exit [%d]" exit-status)))))
	  (if (with-current-buffer buffer (> (point-max) (point-min)))
	      ;; There's some output, display it
	      (display-message-or-buffer buffer)
	    ;; No output; error?
	    (let ((output
		   (if (and error-file
			    (< 0 (nth 7 (file-attributes error-file))))
		       "some error output"
		     "no output")))
	      (if (equal 0 exit-status)
		  (message "(Shell command succeeded with %s)"
			   output)
		(message "(Shell command failed with code %d and %s)"
			 exit-status output)))
	    ;; Don't kill: there might be useful info in the undo-log.
	    ;; (kill-buffer buffer)
	    ))))

    (when (and error-file (file-exists-p error-file))
      (if (< 0 (nth 7 (file-attributes error-file)))
	  (with-current-buffer (get-buffer-create error-buffer)
	    (let ((pos-from-end (- (point-max) (point))))
	      (or (bobp)
		  (insert "\f\n"))
	      ;; Do no formatting while reading error file,
	      ;; because that can run a shell command, and we
	      ;; don't want that to cause an infinite recursion.
	      (format-insert-file error-file nil)
	      ;; Put point after the inserted errors.
	      (goto-char (- (point-max) pos-from-end)))
	    (display-buffer (current-buffer))))
      (delete-file error-file))
    exit-status))

(defun shell-command-to-string (command)
  "Execute shell command COMMAND and return its output as a string."
  (with-output-to-string
    (with-current-buffer
      standard-output
      (call-process shell-file-name nil t nil shell-command-switch command))))

(defvar universal-argument-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'universal-argument-other-key)
    (define-key map (vector meta-prefix-char t) 'universal-argument-other-key)
    (define-key map [switch-frame] nil)
    (define-key map [?\C-u] 'universal-argument-more)
    (define-key map [?-] 'universal-argument-minus)
    (define-key map [?0] 'digit-argument)
    (define-key map [?1] 'digit-argument)
    (define-key map [?2] 'digit-argument)
    (define-key map [?3] 'digit-argument)
    (define-key map [?4] 'digit-argument)
    (define-key map [?5] 'digit-argument)
    (define-key map [?6] 'digit-argument)
    (define-key map [?7] 'digit-argument)
    (define-key map [?8] 'digit-argument)
    (define-key map [?9] 'digit-argument)
    (define-key map [kp-0] 'digit-argument)
    (define-key map [kp-1] 'digit-argument)
    (define-key map [kp-2] 'digit-argument)
    (define-key map [kp-3] 'digit-argument)
    (define-key map [kp-4] 'digit-argument)
    (define-key map [kp-5] 'digit-argument)
    (define-key map [kp-6] 'digit-argument)
    (define-key map [kp-7] 'digit-argument)
    (define-key map [kp-8] 'digit-argument)
    (define-key map [kp-9] 'digit-argument)
    (define-key map [kp-subtract] 'universal-argument-minus)
    map)
  "Keymap used while processing \\[universal-argument].")

(defvar universal-argument-num-events nil
  "Number of argument-specifying events read by `universal-argument'.
`universal-argument-other-key' uses this to discard those events
from (this-command-keys), and reread only the final command.")

(defun universal-argument ()
  "Begin a numeric argument for the following command.
Digits or minus sign following \\[universal-argument] make up the numeric argument.
\\[universal-argument] following the digits or minus sign ends the argument.
\\[universal-argument] without digits or minus sign provides 4 as argument.
Repeating \\[universal-argument] without digits or minus sign
 multiplies the argument by 4 each time.
For some commands, just \\[universal-argument] by itself serves as a flag
which is different in effect from any particular numeric argument.
These commands include \\[set-mark-command] and \\[start-kbd-macro]."
  (interactive)
  (setq prefix-arg (list 4))
  (setq universal-argument-num-events (length (this-command-keys)))
  (setq overriding-terminal-local-map universal-argument-map))

;; A subsequent C-u means to multiply the factor by 4 if we've typed
;; nothing but C-u's; otherwise it means to terminate the prefix arg.
(defun universal-argument-more (arg)
  (interactive "P")
  (if (consp arg)
      (setq prefix-arg (list (* 4 (car arg))))
    (if (eq arg '-)
	(setq prefix-arg (list -4))
      (setq prefix-arg arg)
      (setq overriding-terminal-local-map nil)))
  (setq universal-argument-num-events (length (this-command-keys))))

(defun negative-argument (arg)
  "Begin a negative numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "P")
  (cond ((integerp arg)
	 (setq prefix-arg (- arg)))
	((eq arg '-)
	 (setq prefix-arg nil))
	(t
	 (setq prefix-arg '-)))
  (setq universal-argument-num-events (length (this-command-keys)))
  (setq overriding-terminal-local-map universal-argument-map))

(defun digit-argument (arg)
  "Part of the numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "P")
  (let* ((char (if (integerp last-command-char)
		   last-command-char
		 (get last-command-char 'ascii-character)))
	 (digit (- (logand char ?\177) ?0)))
    (cond ((integerp arg)
	   (setq prefix-arg (+ (* arg 10)
			       (if (< arg 0) (- digit) digit))))
	  ((eq arg '-)
	   ;; Treat -0 as just -, so that -01 will work.
	   (setq prefix-arg (if (zerop digit) '- (- digit))))
	  (t
	   (setq prefix-arg digit))))
  (setq universal-argument-num-events (length (this-command-keys)))
  (setq overriding-terminal-local-map universal-argument-map))

;; For backward compatibility, minus with no modifiers is an ordinary
;; command if digits have already been entered.
(defun universal-argument-minus (arg)
  (interactive "P")
  (if (integerp arg)
      (universal-argument-other-key arg)
    (negative-argument arg)))

;; Anything else terminates the argument and is left in the queue to be
;; executed as a command.
(defun universal-argument-other-key (arg)
  (interactive "P")
  (setq prefix-arg arg)
  (let* ((key (this-command-keys))
	 (keylist (listify-key-sequence key)))
    (setq unread-command-events
	  (append (nthcdr universal-argument-num-events keylist)
		  unread-command-events)))
  (reset-this-command-lengths)
  (setq overriding-terminal-local-map nil))

;;;; Window system cut and paste hooks.

(defvar interprogram-cut-function nil
  "Function to call to make a killed region available to other programs.

Most window systems provide some sort of facility for cutting and
pasting text between the windows of different programs.
This variable holds a function that Emacs calls whenever text
is put in the kill ring, to make the new kill available to other
programs.

The function takes one or two arguments.
The first argument, TEXT, is a string containing
the text which should be made available.
The second, PUSH, if non-nil means this is a \"new\" kill;
nil means appending to an \"old\" kill.")

(defvar interprogram-paste-function nil
  "Function to call to get text cut from other programs.

Most window systems provide some sort of facility for cutting and
pasting text between the windows of different programs.
This variable holds a function that Emacs calls to obtain
text that other programs have provided for pasting.

The function should be called with no arguments.  If the function
returns nil, then no other program has provided such text, and the top
of the Emacs kill ring should be used.  If the function returns a
string, that string should be put in the kill ring as the latest kill.

Note that the function should return a string only if a program other
than Emacs has provided a string for pasting; if Emacs provided the
most recent string, the function should return nil.  If it is
difficult to tell whether Emacs or some other program provided the
current string, it is probably good enough to return nil if the string
is equal (according to `string=') to the last text Emacs provided.")



;;;; The kill ring data structure.

(defvar kill-ring nil
  "List of killed text sequences.
Since the kill ring is supposed to interact nicely with cut-and-paste
facilities offered by window systems, use of this variable should
interact nicely with `interprogram-cut-function' and
`interprogram-paste-function'.  The functions `kill-new',
`kill-append', and `current-kill' are supposed to implement this
interaction; you may want to use them instead of manipulating the kill
ring directly.")

(defcustom kill-ring-max 60
  "*Maximum length of kill ring before oldest elements are thrown away."
  :type 'integer
  :group 'killing)

(defvar kill-ring-yank-pointer nil
  "The tail of the kill ring whose car is the last thing yanked.")

(defun kill-new (string &optional replace)
  "Make STRING the latest kill in the kill ring.
Set `kill-ring-yank-pointer' to point to it.
If `interprogram-cut-function' is non-nil, apply it to STRING.
Optional second argument REPLACE non-nil means that STRING will replace
the front of the kill ring, rather than being added to the list."
  (and (fboundp 'menu-bar-update-yank-menu)
       (menu-bar-update-yank-menu string (and replace (car kill-ring))))
  (if (and replace kill-ring)
      (setcar kill-ring string)
    (setq kill-ring (cons string kill-ring))
    (if (> (length kill-ring) kill-ring-max)
	(setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
  (setq kill-ring-yank-pointer kill-ring)
  (if interprogram-cut-function
      (funcall interprogram-cut-function string (not replace))))

(defun kill-append (string before-p)
  "Append STRING to the end of the latest kill in the kill ring.
If BEFORE-P is non-nil, prepend STRING to the kill.
If `interprogram-cut-function' is set, pass the resulting kill to
it."
  (kill-new (if before-p
		(concat string (car kill-ring))
	      (concat (car kill-ring) string))
	    t))

(defun current-kill (n &optional do-not-move)
  "Rotate the yanking point by N places, and then return that kill.
If N is zero, `interprogram-paste-function' is set, and calling it
returns a string, then that string is added to the front of the
kill ring and returned as the latest kill.
If optional arg DO-NOT-MOVE is non-nil, then don't actually move the
yanking point; just return the Nth kill forward."
  (let ((interprogram-paste (and (= n 0)
				 interprogram-paste-function
				 (funcall interprogram-paste-function))))
    (if interprogram-paste
	(progn
	  ;; Disable the interprogram cut function when we add the new
	  ;; text to the kill ring, so Emacs doesn't try to own the
	  ;; selection, with identical text.
	  (let ((interprogram-cut-function nil))
	    (kill-new interprogram-paste))
	  interprogram-paste)
      (or kill-ring (error "Kill ring is empty"))
      (let ((ARGth-kill-element
	     (nthcdr (mod (- n (length kill-ring-yank-pointer))
			  (length kill-ring))
		     kill-ring)))
	(or do-not-move
	    (setq kill-ring-yank-pointer ARGth-kill-element))
	(car ARGth-kill-element)))))



;;;; Commands for manipulating the kill ring.

(defcustom kill-read-only-ok nil
  "*Non-nil means don't signal an error for killing read-only text."
  :type 'boolean
  :group 'killing)

(put 'text-read-only 'error-conditions
     '(text-read-only buffer-read-only error))
(put 'text-read-only 'error-message "Text is read-only")

(defun kill-region (beg end)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to kill and then yank immediately, use \\[kill-ring-save].)

If you want to append the killed region to the last killed text,
use \\[append-next-kill] before \\[kill-region].

If the buffer is read-only, Emacs will beep and refrain from deleting
the text, but put the text in the kill ring anyway.  This means that
you can use the killing commands to copy text from a read-only buffer.

This is the primitive for programs to kill text (as opposed to deleting it).
Supply two arguments, character numbers indicating the stretch of text
 to be killed.
Any command that calls this function is a \"kill command\".
If the previous command was also a kill command,
the text killed this time appends to the text killed last time
to make one entry in the kill ring."
  (interactive "r")
  (condition-case nil
      (let ((string (delete-and-extract-region beg end)))
	(when string			;STRING is nil if BEG = END
	  ;; Add that string to the kill ring, one way or another.
	  (if (eq last-command 'kill-region)
	      (kill-append string (< end beg))
	    (kill-new string)))
	(setq this-command 'kill-region))
    ((buffer-read-only text-read-only)
     ;; The code above failed because the buffer, or some of the characters
     ;; in the region, are read-only.
     ;; We should beep, in case the user just isn't aware of this.
     ;; However, there's no harm in putting
     ;; the region's text in the kill ring, anyway.
     (copy-region-as-kill beg end)
     ;; Set this-command now, so it will be set even if we get an error.
     (setq this-command 'kill-region)
     ;; This should barf, if appropriate, and give us the correct error.
     (if kill-read-only-ok
	 (message "Read only text copied to kill ring")
       ;; Signal an error if the buffer is read-only.
       (barf-if-buffer-read-only)
       ;; If the buffer isn't read-only, the text is.
       (signal 'text-read-only (list (current-buffer)))))))

;; copy-region-as-kill no longer sets this-command, because it's confusing
;; to get two copies of the text when the user accidentally types M-w and
;; then corrects it with the intended C-w.
(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (buffer-substring beg end) (< end beg))
    (kill-new (buffer-substring beg end)))
  (if transient-mark-mode
      (setq deactivate-mark t))
  nil)

(defun kill-ring-save (beg end)
  "Save the region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-ring-save].

This command is similar to `copy-region-as-kill', except that it gives
visual feedback indicating the extent of the region being copied."
  (interactive "r")
  (copy-region-as-kill beg end)
  (if (interactive-p)
      (let ((other-end (if (= (point) beg) end beg))
	    (opoint (point))
	    ;; Inhibit quitting so we can make a quit here
	    ;; look like a C-g typed as a command.
	    (inhibit-quit t))
	(if (pos-visible-in-window-p other-end (selected-window))
	    (unless transient-mark-mode
	      ;; Swap point and mark.
	      (set-marker (mark-marker) (point) (current-buffer))
	      (goto-char other-end)
	      (sit-for 1)
	      ;; Swap back.
	      (set-marker (mark-marker) other-end (current-buffer))
	      (goto-char opoint)
	      ;; If user quit, deactivate the mark
	      ;; as C-g would as a command.
	      (and quit-flag mark-active
		   (deactivate-mark)))
	  (let* ((killed-text (current-kill 0))
		 (message-len (min (length killed-text) 40)))
	    (if (= (point) beg)
		;; Don't say "killed"; that is misleading.
		(message "Saved text until \"%s\""
			(substring killed-text (- message-len)))
	      (message "Saved text from \"%s\""
		      (substring killed-text 0 message-len))))))))

(defun append-next-kill (&optional interactive)
  "Cause following command, if it kills, to append to previous kill.
The argument is used for internal purposes; do not supply one."
  (interactive "p")
  ;; We don't use (interactive-p), since that breaks kbd macros.
  (if interactive
      (progn
	(setq this-command 'kill-region)
	(message "If the next command is a kill, it will append"))
    (setq last-command 'kill-region)))

;; Yanking.

;; This is actually used in subr.el but defcustom does not work there.
(defcustom yank-excluded-properties
  '(read-only invisible intangible field mouse-face help-echo local-map keymap)
  "*Text properties to discard when yanking."
  :type '(choice (const :tag "All" t) (repeat symbol))
  :group 'editing
  :version 21.4)

(defun yank-pop (arg)
  "Replace just-yanked stretch of killed text with a different stretch.
This command is allowed only immediately after a `yank' or a `yank-pop'.
At such a time, the region contains a stretch of reinserted
previously-killed text.  `yank-pop' deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument N, insert the Nth previous kill.
If N is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (if (not (eq last-command 'yank))
      (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (let ((inhibit-read-only t)
	(before (< (point) (mark t))))
    (delete-region (point) (mark t))
    (set-marker (mark-marker) (point) (current-buffer))
    (insert-for-yank (current-kill arg))
    (if before
	;; This is like exchange-point-and-mark, but doesn't activate the mark.
	;; It is cleaner to avoid activation, even though the command
	;; loop would deactivate the mark because we inserted text.
	(goto-char (prog1 (mark t)
		     (set-marker (mark-marker) (point) (current-buffer))))))
  nil)

(defun yank (&optional arg)
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.  Put point at end, and set mark at beginning.
With just C-u as argument, same but put point at beginning (and mark at end).
With argument N, reinsert the Nth most recently killed stretch of killed
text.
See also the command \\[yank-pop]."
  (interactive "*P")
  ;; If we don't get all the way thru, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (push-mark (point))
  (insert-for-yank (current-kill (cond
				  ((listp arg) 0)
				  ((eq arg '-) -1)
				  (t (1- arg)))))
  (if (consp arg)
      ;; This is like exchange-point-and-mark, but doesn't activate the mark.
      ;; It is cleaner to avoid activation, even though the command
      ;; loop would deactivate the mark because we inserted text.
      (goto-char (prog1 (mark t)
		   (set-marker (mark-marker) (point) (current-buffer)))))
  ;; If we do get all the way thru, make this-command indicate that.
  (setq this-command 'yank)
  nil)

(defun rotate-yank-pointer (arg)
  "Rotate the yanking point in the kill ring.
With argument, rotate that many kills forward (or backward, if negative)."
  (interactive "p")
  (current-kill arg))

;; Some kill commands.

;; Internal subroutine of delete-char
(defun kill-forward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (forward-point arg)))

;; Internal subroutine of backward-delete-char
(defun kill-backward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (forward-point (- arg))))

(defcustom backward-delete-char-untabify-method 'untabify
  "*The method for untabifying when deleting backward.
Can be `untabify' -- turn a tab to many spaces, then delete one space;
       `hungry' -- delete all whitespace, both tabs and spaces;
       `all' -- delete all whitespace, including tabs, spaces and newlines;
       nil -- just delete one character."
  :type '(choice (const untabify) (const hungry) (const all) (const nil))
  :version "20.3"
  :group 'killing)

(defun backward-delete-char-untabify (arg &optional killp)
  "Delete characters backward, changing tabs into spaces.
The exact behavior depends on `backward-delete-char-untabify-method'.
Delete ARG chars, and kill (save in kill ring) if KILLP is non-nil.
Interactively, ARG is the prefix arg (default 1)
and KILLP is t if a prefix arg was specified."
  (interactive "*p\nP")
  (when (eq backward-delete-char-untabify-method 'untabify)
    (let ((count arg))
      (save-excursion
	(while (and (> count 0) (not (bobp)))
	  (if (= (preceding-char) ?\t)
	      (let ((col (current-column)))
		(forward-char -1)
		(setq col (- col (current-column)))
		(insert-char ?\ col)
		(delete-char 1)))
	  (forward-char -1)
	  (setq count (1- count))))))
  (delete-backward-char
   (let ((skip (cond ((eq backward-delete-char-untabify-method 'hungry) " \t")
                     ((eq backward-delete-char-untabify-method 'all)
                      " \t\n\r"))))
     (if skip
         (let ((wh (- (point) (save-excursion (skip-chars-backward skip)
					    (point)))))
	 (+ arg (if (zerop wh) 0 (1- wh))))
         arg))
   killp))

(defun zap-to-char (arg char)
  "Kill up to and including ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to char: ")
  (kill-region (point) (progn
			 (search-forward (char-to-string char) nil nil arg)
;			 (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
			 (point))))

;; kill-line and its subroutines.

(defcustom kill-whole-line nil
  "*If non-nil, `kill-line' with no arg at beg of line kills the whole line."
  :type 'boolean
  :group 'killing)

(defun kill-line (&optional arg)
  "Kill the rest of the current line; if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.
With zero argument, kills the text before point on the current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

To kill a whole line, when point is not at the beginning, type \
\\[beginning-of-line] \\[kill-line] \\[kill-line].

If `kill-whole-line' is non-nil, then this command kills the whole line
including its terminating newline, when used at the beginning of a line
with no argument.  As a consequence, you can always kill a whole line
by typing \\[beginning-of-line] \\[kill-line].

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-line].

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer."
  (interactive "P")
  (kill-region (point)
	       ;; It is better to move point to the other end of the kill
	       ;; before killing.  That way, in a read-only buffer, point
	       ;; moves across the text that is copied to the kill ring.
	       ;; The choice has no effect on undo now that undo records
	       ;; the value of point from before the command was run.
	       (progn
		 (if arg
		     (forward-visible-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
		       (forward-visible-line 1)
		     (end-of-visible-line)))
		 (point))))

(defun forward-visible-line (arg)
  "Move forward by ARG lines, ignoring currently invisible newlines only.
If ARG is negative, move backward -ARG lines.
If ARG is zero, move to the beginning of the current line."
  (condition-case nil
      (if (> arg 0)
	  (while (> arg 0)
	    (or (zerop (forward-line 1))
		(signal 'end-of-buffer nil))
	    ;; If the following character is currently invisible,
	    ;; skip all characters with that same `invisible' property value,
	    ;; then find the next newline.
	    (while (and (not (eobp))
			(let ((prop
			       (get-char-property (point) 'invisible)))
			  (if (eq buffer-invisibility-spec t)
			      prop
			    (or (memq prop buffer-invisibility-spec)
				(assq prop buffer-invisibility-spec)))))
	      (goto-char
	       (if (get-text-property (point) 'invisible)
		   (or (next-single-property-change (point) 'invisible)
		       (point-max))
		 (next-overlay-change (point))))
	      (or (zerop (forward-line 1))
		  (signal 'end-of-buffer nil)))
	    (setq arg (1- arg)))
	(let ((first t))
	  (while (or first (< arg 0))
	    (if (zerop arg)
		(beginning-of-line)
	      (or (zerop (forward-line -1))
		  (signal 'beginning-of-buffer nil)))
	    (while (and (not (bobp))
			(let ((prop
			       (get-char-property (1- (point)) 'invisible)))
			  (if (eq buffer-invisibility-spec t)
			      prop
			    (or (memq prop buffer-invisibility-spec)
				(assq prop buffer-invisibility-spec)))))
	      (goto-char
	       (if (get-text-property (1- (point)) 'invisible)
		   (or (previous-single-property-change (point) 'invisible)
		       (point-min))
		 (previous-overlay-change (point))))
	      (or (zerop (forward-line -1))
		  (signal 'beginning-of-buffer nil)))
	    (setq first nil)
	    (setq arg (1+ arg)))))
    ((beginning-of-buffer end-of-buffer)
     nil)))

(defun end-of-visible-line ()
  "Move to end of current visible line."
  (end-of-line)
  ;; If the following character is currently invisible,
  ;; skip all characters with that same `invisible' property value,
  ;; then find the next newline.
  (while (and (not (eobp))
	      (let ((prop
		     (get-char-property (point) 'invisible)))
		(if (eq buffer-invisibility-spec t)
		    prop
		  (or (memq prop buffer-invisibility-spec)
		      (assq prop buffer-invisibility-spec)))))
    (if (get-text-property (point) 'invisible)
	(goto-char (next-single-property-change (point) 'invisible))
      (goto-char (next-overlay-change (point))))
    (end-of-line)))

(defun insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name.

This function is meant for the user to run interactively.
Don't call it from programs!"
  (interactive
   (list
    (progn
      (barf-if-buffer-read-only)
      (read-buffer "Insert buffer: "
		   (if (eq (selected-window) (next-window (selected-window)))
		       (other-buffer (current-buffer))
		     (window-buffer (next-window (selected-window))))
		   t))))
  (or (bufferp buffer)
      (setq buffer (get-buffer buffer)))
  (let (start end newmark)
    (save-excursion
      (save-excursion
	(set-buffer buffer)
	(setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark))
  nil)

(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive
   (list (read-buffer "Append to buffer: " (other-buffer (current-buffer) t))
	 (region-beginning) (region-end)))
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (let* ((append-to (get-buffer-create buffer))
	     (windows (get-buffer-window-list append-to t t))
	     point)
	(set-buffer append-to)
	(setq point (point))
	(barf-if-buffer-read-only)
	(insert-buffer-substring oldbuf start end)
	(dolist (window windows)
	  (when (= (window-point window) point)
	    (set-window-point window (point))))))))

(defun prepend-to-buffer (buffer start end)
  "Prepend to specified buffer the text of the region.
It is inserted into that buffer after its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive "BPrepend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (barf-if-buffer-read-only)
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

(defun copy-to-buffer (buffer start end)
  "Copy to specified buffer the text of the region.
It is inserted into that buffer, replacing existing text there.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive "BCopy to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (barf-if-buffer-read-only)
      (erase-buffer)
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

(put 'mark-inactive 'error-conditions '(mark-inactive error))
(put 'mark-inactive 'error-message "The mark is not active now")

(defun mark (&optional force)
  "Return this buffer's mark value as integer; error if mark inactive.
If optional argument FORCE is non-nil, access the mark value
even if the mark is not currently active, and return nil
if there is no mark at all.

If you are using this in an editing command, you are most likely making
a mistake; see the documentation of `set-mark'."
  (if (or force (not transient-mark-mode) mark-active mark-even-if-inactive)
      (marker-position (mark-marker))
    (signal 'mark-inactive nil)))

;; Many places set mark-active directly, and several of them failed to also
;; run deactivate-mark-hook.  This shorthand should simplify.
(defsubst deactivate-mark ()
  "Deactivate the mark by setting `mark-active' to nil.
\(That makes a difference only in Transient Mark mode.)
Also runs the hook `deactivate-mark-hook'."
  (cond
   ((eq transient-mark-mode 'lambda)
    (setq transient-mark-mode nil))
   (transient-mark-mode
    (setq mark-active nil)
    (run-hooks 'deactivate-mark-hook))))

(defun set-mark (pos)
  "Set this buffer's mark to POS.  Don't use this function!
That is to say, don't use this function unless you want
the user to see that the mark has moved, and you want the previous
mark position to be lost.

Normally, when a new mark is set, the old one should go on the stack.
This is why most applications should use push-mark, not set-mark.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  The mark saves a location for the user's convenience.
Most editing commands should not alter the mark.
To remember a location for internal use in the Lisp program,
store it in a Lisp variable.  Example:

   (let ((beg (point))) (forward-line 1) (delete-region beg (point)))."

  (if pos
      (progn
	(setq mark-active t)
	(run-hooks 'activate-mark-hook)
	(set-marker (mark-marker) pos (current-buffer)))
    ;; Normally we never clear mark-active except in Transient Mark mode.
    ;; But when we actually clear out the mark value too,
    ;; we must clear mark-active in any mode.
    (setq mark-active nil)
    (run-hooks 'deactivate-mark-hook)
    (set-marker (mark-marker) nil)))

(defvar mark-ring nil
  "The list of former marks of the current buffer, most recent first.")
(make-variable-buffer-local 'mark-ring)
(put 'mark-ring 'permanent-local t)

(defcustom mark-ring-max 16
  "*Maximum size of mark ring.  Start discarding off end if gets this big."
  :type 'integer
  :group 'editing-basics)

(defvar global-mark-ring nil
  "The list of saved global marks, most recent first.")

(defcustom global-mark-ring-max 16
  "*Maximum size of global mark ring.  \
Start discarding off end if gets this big."
  :type 'integer
  :group 'editing-basics)

(defun pop-to-mark-command ()
  "Jump to mark, and pop a new position for mark off the ring
\(does not affect global mark ring\)."
  (interactive)
  (if (null (mark t))
      (error "No mark set in this buffer")
    (goto-char (mark t))
    (pop-mark)))

(defun push-mark-command (arg)
  "Set mark at where point is.
If no prefix arg and mark is already set there, just activate it."
  (interactive "P")
  (let ((mark (marker-position (mark-marker))))
    (if (or arg (null mark) (/= mark (point)))
	(push-mark nil nil t)
      (setq mark-active t)
      (message "Mark activated"))))

(defun set-mark-command (arg)
  "Set mark at where point is, or jump to mark.
With no prefix argument, set mark, push old mark position on local mark
ring, and push mark on global mark ring.  Immediately repeating the
command activates `transient-mark-mode' temporarily.

With argument, jump to mark, and pop a new position for mark off the ring
\(does not affect global mark ring\).  Repeating the command without
an argument jumps to the next position off the mark ring.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (interactive "P")
  (if (eq transient-mark-mode 'lambda)
      (setq transient-mark-mode nil))
  (cond
   ((not (eq this-command 'set-mark-command))
    (if arg
	(pop-to-mark-command)
      (push-mark-command t)))
   ((eq last-command 'pop-to-mark-command)
    (if (and (consp arg) (> (prefix-numeric-value arg) 4))
	(push-mark-command nil)
      (setq this-command 'pop-to-mark-command)
      (pop-to-mark-command)))
   (arg
    (setq this-command 'pop-to-mark-command)
    (pop-to-mark-command))
   ((and (eq last-command 'set-mark-command)
	 mark-active (null transient-mark-mode))
    (setq transient-mark-mode 'lambda)
    (message "Transient-mark-mode temporarily enabled"))
   (t
    (push-mark-command nil))))

(defun push-mark (&optional location nomsg activate)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.
In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information.

In Transient Mark mode, this does not activate the mark."
  (if (null (mark t))
      nil
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (if (> (length mark-ring) mark-ring-max)
	(progn
	  (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
	  (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil))))
  (set-marker (mark-marker) (or location (point)) (current-buffer))
  ;; Now push the mark on the global mark ring.
  (if (and global-mark-ring
	   (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
      ;; The last global mark pushed was in this same buffer.
      ;; Don't push another one.
      nil
    (setq global-mark-ring (cons (copy-marker (mark-marker)) global-mark-ring))
    (if (> (length global-mark-ring) global-mark-ring-max)
	(progn
	  (move-marker (car (nthcdr global-mark-ring-max global-mark-ring))
		       nil)
	  (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil))))
  (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
      (message "Mark set"))
  (if (or activate (not transient-mark-mode))
      (set-mark (mark t)))
  nil)

(defun pop-mark ()
  "Pop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (if mark-ring
      (progn
	(setq mark-ring (nconc mark-ring (list (copy-marker (mark-marker)))))
	(set-marker (mark-marker) (+ 0 (car mark-ring)) (current-buffer))
	(deactivate-mark)
	(move-marker (car mark-ring) nil)
	(if (null (mark t)) (ding))
	(setq mark-ring (cdr mark-ring)))))

(defalias 'exchange-dot-and-mark 'exchange-point-and-mark)
(defun exchange-point-and-mark (&optional arg)
  "Put the mark where point is now, and point where the mark is now.
This command works even when the mark is not active,
and it reactivates the mark.
With prefix arg, `transient-mark-mode' is enabled temporarily."
  (interactive "P")
  (if arg
      (if mark-active 
	  (if (null transient-mark-mode)
	      (setq transient-mark-mode 'lambda))
	(setq arg nil)))
  (unless arg
    (let ((omark (mark t)))
      (if (null omark)
	  (error "No mark set in this buffer"))
      (set-mark (point))
      (goto-char omark)
      nil)))

(defun transient-mark-mode (arg)
  "Toggle Transient Mark mode.
With arg, turn Transient Mark mode on if arg is positive, off otherwise.

In Transient Mark mode, when the mark is active, the region is highlighted.
Changing the buffer \"deactivates\" the mark.
So do certain other operations that set the mark
but whose main purpose is something else--for example,
incremental search, \\[beginning-of-buffer], and \\[end-of-buffer].

You can also deactivate the mark by typing \\[keyboard-quit] or
\\[keyboard-escape-quit].

Many commands change their behavior when Transient Mark mode is in effect
and the mark is active, by acting on the region instead of their usual
default part of the buffer's text.  Examples of such commands include
\\[comment-dwim], \\[flush-lines], \\[ispell], \\[keep-lines],
\\[query-replace], \\[query-replace-regexp], and \\[undo].  Invoke
\\[apropos-documentation] and type \"transient\" or \"mark.*active\" at
the prompt, to see the documentation of commands which are sensitive to
the Transient Mark mode."
  (interactive "P")
  (setq transient-mark-mode
	(if (null arg)
	    (not transient-mark-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (interactive-p)
      (if transient-mark-mode
	  (message "Transient Mark mode enabled")
	(message "Transient Mark mode disabled"))))

(defun pop-global-mark ()
  "Pop off global mark ring and jump to the top location."
  (interactive)
  ;; Pop entries which refer to non-existent buffers.
  (while (and global-mark-ring (not (marker-buffer (car global-mark-ring))))
    (setq global-mark-ring (cdr global-mark-ring)))
  (or global-mark-ring
      (error "No global mark set"))
  (let* ((marker (car global-mark-ring))
	 (buffer (marker-buffer marker))
	 (position (marker-position marker)))
    (setq global-mark-ring (nconc (cdr global-mark-ring)
				  (list (car global-mark-ring))))
    (set-buffer buffer)
    (or (and (>= position (point-min))
	     (<= position (point-max)))
	(widen))
    (goto-char position)
    (switch-to-buffer buffer)))

(defcustom next-line-add-newlines nil
  "*If non-nil, `next-line' inserts newline to avoid `end of buffer' error."
  :type 'boolean
  :version "21.1"
  :group 'editing-basics)

(defun next-line (&optional arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one, behavior depends on the
value of `next-line-add-newlines'.  If non-nil, it inserts a newline character
to create a line, and moves the cursor to that line.  Otherwise it moves the
cursor to the end of the buffer.

The command \\[set-goal-column] can be used to create
a semipermanent goal column for this command.
Then instead of trying to move exactly vertically (or as close as possible),
this command moves to the specified goal column (or as close as possible).
The goal column is stored in the variable `goal-column', which is nil
when there is no goal column.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (and next-line-add-newlines (= arg 1))
      (if (save-excursion (end-of-line) (eobp))
	  ;; When adding a newline, don't expand an abbrev.
	  (let ((abbrev-mode nil))
	    (end-of-line)
	    (insert "\n"))
	(line-move arg))
    (if (interactive-p)
	(condition-case nil
	    (line-move arg)
	  ((beginning-of-buffer end-of-buffer) (ding)))
      (line-move arg)))
  nil)

(defun previous-line (&optional arg)
  "Move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

The command \\[set-goal-column] can be used to create
a semipermanent goal column for this command.
Then instead of trying to move exactly vertically (or as close as possible),
this command moves to the specified goal column (or as close as possible).
The goal column is stored in the variable `goal-column', which is nil
when there is no goal column.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with a negative argument instead.  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (interactive-p)
      (condition-case nil
	  (line-move (- arg))
	((beginning-of-buffer end-of-buffer) (ding)))
    (line-move (- arg)))
  nil)

(defcustom track-eol nil
  "*Non-nil means vertical motion starting at end of line keeps to ends of lines.
This means moving to the end of each line moved onto.
The beginning of a blank line does not count as the end of a line."
  :type 'boolean
  :group 'editing-basics)

(defcustom goal-column nil
  "*Semipermanent goal column for vertical motion, as set by \\[set-goal-column], or nil."
  :type '(choice integer
		 (const :tag "None" nil))
  :group 'editing-basics)
(make-variable-buffer-local 'goal-column)

(defvar temporary-goal-column 0
  "Current goal column for vertical motion.
It is the column where point was
at the start of current run of vertical motion commands.
When the `track-eol' feature is doing its job, the value is 9999.")

(defcustom line-move-ignore-invisible nil
  "*Non-nil means \\[next-line] and \\[previous-line] ignore invisible lines.
Outline mode sets this."
  :type 'boolean
  :group 'editing-basics)

(defun line-move-invisible (pos)
  "Return non-nil if the character after POS is currently invisible."
  (let ((prop
	 (get-char-property pos 'invisible)))
    (if (eq buffer-invisibility-spec t)
	prop
      (or (memq prop buffer-invisibility-spec)
	  (assq prop buffer-invisibility-spec)))))

;; This is the guts of next-line and previous-line.
;; Arg says how many lines to move.
(defun line-move (arg)
  ;; Don't run any point-motion hooks, and disregard intangibility,
  ;; for intermediate positions.
  (let ((inhibit-point-motion-hooks t)
	(opoint (point))
	new line-end line-beg)
    (unwind-protect
	(progn
	  (if (not (memq last-command '(next-line previous-line)))
	      (setq temporary-goal-column
		    (if (and track-eol (eolp)
			     ;; Don't count beg of empty line as end of line
			     ;; unless we just did explicit end-of-line.
			     (or (not (bolp)) (eq last-command 'end-of-line)))
			9999
		      (current-column))))
	  (if (and (not (integerp selective-display))
		   (not line-move-ignore-invisible))
	      ;; Use just newline characters.
	      ;; Set ARG to 0 if we move as many lines as requested.
	      (or (if (> arg 0)
		      (progn (if (> arg 1) (forward-line (1- arg)))
			     ;; This way of moving forward ARG lines
			     ;; verifies that we have a newline after the last one.
			     ;; It doesn't get confused by intangible text.
			     (end-of-line)
			     (if (zerop (forward-line 1))
				 (setq arg 0)))
		    (and (zerop (forward-line arg))
			 (bolp)
			 (setq arg 0)))
		  (signal (if (< arg 0)
			      'beginning-of-buffer
			    'end-of-buffer)
			  nil))
	    ;; Move by arg lines, but ignore invisible ones.
	    (while (> arg 0)
	      ;; If the following character is currently invisible,
	      ;; skip all characters with that same `invisible' property value.
	      (while (and (not (eobp)) (line-move-invisible (point)))
		(goto-char (next-char-property-change (point))))
	      ;; Now move a line.
	      (end-of-line)
	      (and (zerop (vertical-motion 1))
		   (signal 'end-of-buffer nil))
	      (setq arg (1- arg)))
	    (while (< arg 0)
	      (beginning-of-line)
	      (and (zerop (vertical-motion -1))
		   (signal 'beginning-of-buffer nil))
	      (setq arg (1+ arg))
	      (while (and (not (bobp)) (line-move-invisible (1- (point))))
		(goto-char (previous-char-property-change (point)))))))

      (cond ((> arg 0)
	     ;; If we did not move down as far as desired,
	     ;; at least go to end of line.
	     (end-of-line))
	    ((< arg 0)
	     ;; If we did not move down as far as desired,
	     ;; at least go to end of line.
	     (beginning-of-line))
	    (t
	     (line-move-finish (or goal-column temporary-goal-column) opoint)))))
  nil)

(defun line-move-finish (column opoint)
  (let ((repeat t))
    (while repeat
      ;; Set REPEAT to t to repeat the whole thing.
      (setq repeat nil)

      (let (new
	    (line-beg (save-excursion (beginning-of-line) (point)))
	    (line-end
	     ;; Compute the end of the line
	     ;; ignoring effectively intangible newlines.
	     (let ((inhibit-point-motion-hooks nil))
	       (save-excursion (end-of-line) (point)))))

	;; Move to the desired column.
	(line-move-to-column column)
	(setq new (point))

	;; Process intangibility within a line.
	;; Move to the chosen destination position from above,
	;; with intangibility processing enabled.

	(goto-char (point-min))
	(let ((inhibit-point-motion-hooks nil))
	  (goto-char new)

	  ;; If intangibility moves us to a different (later) place
	  ;; in the same line, use that as the destination.
	  (if (<= (point) line-end)
	      (setq new (point))
	    ;; If that position is "too late",
	    ;; try the previous allowable position.
	    ;; See if it is ok.
	    (backward-char)
	    (if (<= (point) line-end)
		(setq new (point))
	      ;; As a last resort, use the end of the line.
	      (setq new line-end))))

	;; Now move to the updated destination, processing fields
	;; as well as intangibility.
	(goto-char opoint)
	(let ((inhibit-point-motion-hooks nil))
	  (goto-char
	   (constrain-to-field new opoint nil t
			       'inhibit-line-move-field-capture)))

	;; If all this moved us to a different line,
	;; retry everything within that new line.
	(when (or (< (point) line-beg) (> (point) line-end))
	  ;; Repeat the intangibility and field processing.
	  (setq repeat t))))))

(defun line-move-to-column (col)
  "Try to find column COL, considering invisibility.
This function works only in certain cases,
because what we really need is for `move-to-column'
and `current-column' to be able to ignore invisible text."
  (if (zerop col)
      (beginning-of-line)
    (move-to-column col))

  (when (and line-move-ignore-invisible
	     (not (bolp)) (line-move-invisible (1- (point))))
    (let ((normal-location (point))
	  (normal-column (current-column)))
      ;; If the following character is currently invisible,
      ;; skip all characters with that same `invisible' property value.
      (while (and (not (eobp))
		  (line-move-invisible (point)))
	(goto-char (next-char-property-change (point))))
      ;; Have we advanced to a larger column position?
      (if (> (current-column) normal-column)
	  ;; We have made some progress towards the desired column.
	  ;; See if we can make any further progress.
	  (line-move-to-column (+ (current-column) (- col normal-column)))
	;; Otherwise, go to the place we originally found
	;; and move back over invisible text.
	;; that will get us to the same place on the screen
	;; but with a more reasonable buffer position.
	(goto-char normal-location)
	(let ((line-beg (save-excursion (beginning-of-line) (point))))
	  (while (and (not (bolp)) (line-move-invisible (1- (point))))
	    (goto-char (previous-char-property-change (point) line-beg))))))))

;;; Many people have said they rarely use this feature, and often type
;;; it by accident.  Maybe it shouldn't even be on a key.
(put 'set-goal-column 'disabled t)

(defun set-goal-column (arg)
  "Set the current horizontal position as a goal for \\[next-line] and \\[previous-line].
Those commands will move to this position in the line moved to
rather than trying to keep the same horizontal position.
With a non-nil argument, clears out the goal column
so that \\[next-line] and \\[previous-line] resume vertical motion.
The goal column is stored in the variable `goal-column'."
  (interactive "P")
  (if arg
      (progn
        (setq goal-column nil)
        (message "No goal column"))
    (setq goal-column (current-column))
    (message (substitute-command-keys
	      "Goal column %d (use \\[set-goal-column] with an arg to unset it)")
	     goal-column))
  nil)


(defun scroll-other-window-down (lines)
  "Scroll the \"other window\" down.
For more details, see the documentation for `scroll-other-window'."
  (interactive "P")
  (scroll-other-window
   ;; Just invert the argument's meaning.
   ;; We can do that without knowing which window it will be.
   (if (eq lines '-) nil
     (if (null lines) '-
       (- (prefix-numeric-value lines))))))
(define-key esc-map [?\C-\S-v] 'scroll-other-window-down)

(defun beginning-of-buffer-other-window (arg)
  "Move point to the beginning of the buffer in the other window.
Leave mark at previous position.
With arg N, put point N/10 of the way from the true beginning."
  (interactive "P")
  (let ((orig-window (selected-window))
	(window (other-window-for-scrolling)))
    ;; We use unwind-protect rather than save-window-excursion
    ;; because the latter would preserve the things we want to change.
    (unwind-protect
	(progn
	  (select-window window)
	  ;; Set point and mark in that window's buffer.
	  (beginning-of-buffer arg)
	  ;; Set point accordingly.
	  (recenter '(t)))
      (select-window orig-window))))

(defun end-of-buffer-other-window (arg)
  "Move point to the end of the buffer in the other window.
Leave mark at previous position.
With arg N, put point N/10 of the way from the true end."
  (interactive "P")
  ;; See beginning-of-buffer-other-window for comments.
  (let ((orig-window (selected-window))
	(window (other-window-for-scrolling)))
    (unwind-protect
	(progn
	  (select-window window)
	  (end-of-buffer arg)
	  (recenter '(t)))
      (select-window orig-window))))

(defun transpose-chars (arg)
  "Interchange characters around point, moving forward one character.
With prefix arg ARG, effect is to take character before point
and drag it forward past ARG other characters (backward if ARG negative).
If no argument and at end of line, the previous two chars are exchanged."
  (interactive "*P")
  (and (null arg) (eolp) (forward-char -1))
  (transpose-subr 'forward-char (prefix-numeric-value arg)))

(defun transpose-words (arg)
  "Interchange words around point, leaving point at end of them.
With prefix arg ARG, effect is to take word before or around point
and drag it forward past ARG other words (backward if ARG negative).
If ARG is zero, the words around or after point and around or after mark
are interchanged."
  ;; FIXME: `foo a!nd bar' should transpose into `bar and foo'.
  (interactive "*p")
  (transpose-subr 'forward-word arg))

(defun transpose-sexps (arg)
  "Like \\[transpose-words] but applies to sexps.
Does not work on a sexp that point is in the middle of
if it is a list or string."
  (interactive "*p")
  (transpose-subr
   (lambda (arg)
     ;; Here we should try to simulate the behavior of
     ;; (cons (progn (forward-sexp x) (point))
     ;;       (progn (forward-sexp (- x)) (point)))
     ;; Except that we don't want to rely on the second forward-sexp
     ;; putting us back to where we want to be, since forward-sexp-function
     ;; might do funny things like infix-precedence.
     (if (if (> arg 0)
	     (looking-at "\\sw\\|\\s_")
	   (and (not (bobp))
		(save-excursion (forward-char -1) (looking-at "\\sw\\|\\s_"))))
	 ;; Jumping over a symbol.  We might be inside it, mind you.
	 (progn (funcall (if (> arg 0)
			     'skip-syntax-backward 'skip-syntax-forward)
			 "w_")
		(cons (save-excursion (forward-sexp arg) (point)) (point)))
       ;; Otherwise, we're between sexps.  Take a step back before jumping
       ;; to make sure we'll obey the same precedence no matter which direction
       ;; we're going.
       (funcall (if (> arg 0) 'skip-syntax-backward 'skip-syntax-forward) " .")
       (cons (save-excursion (forward-sexp arg) (point))
	     (progn (while (or (forward-comment (if (> arg 0) 1 -1))
			       (not (zerop (funcall (if (> arg 0)
							'skip-syntax-forward
						      'skip-syntax-backward)
						    ".")))))
		    (point)))))
   arg 'special))

(defun transpose-lines (arg)
  "Exchange current line and previous line, leaving point after both.
With argument ARG, takes previous line and moves it past ARG lines.
With argument 0, interchanges line point is in with line mark is in."
  (interactive "*p")
  (transpose-subr (function
		   (lambda (arg)
		     (if (> arg 0)
			 (progn
			   ;; Move forward over ARG lines,
			   ;; but create newlines if necessary.
			   (setq arg (forward-line arg))
			   (if (/= (preceding-char) ?\n)
			       (setq arg (1+ arg)))
			   (if (> arg 0)
			       (newline arg)))
		       (forward-line arg))))
		  arg))

(defun transpose-subr (mover arg &optional special)
  (let ((aux (if special mover
	       (lambda (x)
		 (cons (progn (funcall mover x) (point))
		       (progn (funcall mover (- x)) (point))))))
	pos1 pos2)
    (cond
     ((= arg 0)
      (save-excursion
	(setq pos1 (funcall aux 1))
	(goto-char (mark))
	(setq pos2 (funcall aux 1))
	(transpose-subr-1 pos1 pos2))
      (exchange-point-and-mark))
     ((> arg 0)
      (setq pos1 (funcall aux -1))
      (setq pos2 (funcall aux arg))
      (transpose-subr-1 pos1 pos2)
      (goto-char (car pos2)))
     (t
      (setq pos1 (funcall aux -1))
      (goto-char (car pos1))
      (setq pos2 (funcall aux arg))
      (transpose-subr-1 pos1 pos2)))))

(defun transpose-subr-1 (pos1 pos2)
  (when (> (car pos1) (cdr pos1)) (setq pos1 (cons (cdr pos1) (car pos1))))
  (when (> (car pos2) (cdr pos2)) (setq pos2 (cons (cdr pos2) (car pos2))))
  (when (> (car pos1) (car pos2))
    (let ((swap pos1))
      (setq pos1 pos2 pos2 swap)))
  (if (> (cdr pos1) (car pos2)) (error "Don't have two things to transpose"))
  (atomic-change-group
   (let (word2)
     (setq word2 (delete-and-extract-region (car pos2) (cdr pos2)))
     (goto-char (car pos2))
     (insert (delete-and-extract-region (car pos1) (cdr pos1)))
     (goto-char (car pos1))
     (insert word2))))

(defun backward-word (arg)
  "Move backward until encountering the beginning of a word.
With argument, do this that many times."
  (interactive "p")
  (forward-word (- arg)))

(defun mark-word (arg)
  "Set mark arg words away from point.
If this command is repeated, it marks the next ARG words after the ones
already marked."
  (interactive "p")
  (cond ((and (eq last-command this-command) (mark t))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (forward-word arg)
	    (point))))
	(t
	 (push-mark
	  (save-excursion
	    (forward-word arg)
	    (point))
	  nil t))))

(defun kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (forward-word arg) (point))))

(defun backward-kill-word (arg)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (kill-word (- arg)))

(defun current-word (&optional strict)
  "Return the word point is on (or a nearby word) as a string.
If optional arg STRICT is non-nil, return nil unless point is within
or adjacent to a word."
  (save-excursion
    (let ((oldpoint (point)) (start (point)) (end (point)))
      (skip-syntax-backward "w_") (setq start (point))
      (goto-char oldpoint)
      (skip-syntax-forward "w_") (setq end (point))
      (if (and (eq start oldpoint) (eq end oldpoint))
	  ;; Point is neither within nor adjacent to a word.
	  (and (not strict)
	       (progn
		 ;; Look for preceding word in same line.
		 (skip-syntax-backward "^w_"
				       (save-excursion (beginning-of-line)
						       (point)))
		 (if (bolp)
		     ;; No preceding word in same line.
		     ;; Look for following word in same line.
		     (progn
		       (skip-syntax-forward "^w_"
					    (save-excursion (end-of-line)
							    (point)))
		       (setq start (point))
		       (skip-syntax-forward "w_")
		       (setq end (point)))
		   (setq end (point))
		   (skip-syntax-backward "w_")
		   (setq start (point)))
		 (buffer-substring-no-properties start end)))
	(buffer-substring-no-properties start end)))))

(defcustom fill-prefix nil
  "*String for filling to insert at front of new line, or nil for none."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'fill)
(make-variable-buffer-local 'fill-prefix)

(defcustom auto-fill-inhibit-regexp nil
  "*Regexp to match lines which should not be auto-filled."
  :type '(choice (const :tag "None" nil)
		 regexp)
  :group 'fill)

(defvar comment-line-break-function 'comment-indent-new-line
  "*Mode-specific function which line breaks and continues a comment.

This function is only called during auto-filling of a comment section.
The function should take a single optional argument, which is a flag
indicating whether it should use soft newlines.

Setting this variable automatically makes it local to the current buffer.")

;; This function is used as the auto-fill-function of a buffer
;; when Auto-Fill mode is enabled.
;; It returns t if it really did any work.
;; (Actually some major modes use a different auto-fill function,
;; but this one is the default one.)
(defun do-auto-fill ()
  (let (fc justify bol give-up
	   (fill-prefix fill-prefix))
    (if (or (not (setq justify (current-justification)))
	    (null (setq fc (current-fill-column)))
	    (and (eq justify 'left)
		 (<= (current-column) fc))
	    (save-excursion (beginning-of-line)
			    (setq bol (point))
			    (and auto-fill-inhibit-regexp
				 (looking-at auto-fill-inhibit-regexp))))
	nil ;; Auto-filling not required
      (if (memq justify '(full center right))
	  (save-excursion (unjustify-current-line)))

      ;; Choose a fill-prefix automatically.
      (when (and adaptive-fill-mode
		 (or (null fill-prefix) (string= fill-prefix "")))
	(let ((prefix
	       (fill-context-prefix
		(save-excursion (backward-paragraph 1) (point))
		(save-excursion (forward-paragraph 1) (point)))))
	  (and prefix (not (equal prefix ""))
	       ;; Use auto-indentation rather than a guessed empty prefix.
	       (not (and fill-indent-according-to-mode
			 (string-match "[ \t]*" prefix)))
	       (setq fill-prefix prefix))))
      
      (while (and (not give-up) (> (current-column) fc))
	;; Determine where to split the line.
	(let* (after-prefix
	       (fill-point
		(let ((opoint (point)))
		  (save-excursion
		    (beginning-of-line)
		    (setq after-prefix (point))
		    (and fill-prefix
			 (looking-at (regexp-quote fill-prefix))
			 (setq after-prefix (match-end 0)))
		    (move-to-column (1+ fc))
		    (fill-move-to-break-point after-prefix)
		    (point)))))

	  ;; See whether the place we found is any good.
	  (if (save-excursion
		(goto-char fill-point)
		(or (bolp)
		    ;; There is no use breaking at end of line.
		    (save-excursion (skip-chars-forward " ") (eolp))
		    ;; It is futile to split at the end of the prefix
		    ;; since we would just insert the prefix again.
		    (and after-prefix (<= (point) after-prefix))
		    ;; Don't split right after a comment starter
		    ;; since we would just make another comment starter.
		    (and comment-start-skip
			 (let ((limit (point)))
			   (beginning-of-line)
			   (and (re-search-forward comment-start-skip
						   limit t)
				(eq (point) limit))))))
	      ;; No good place to break => stop trying.
	      (setq give-up t)
	    ;; Ok, we have a useful place to break the line.  Do it.
	    (let ((prev-column (current-column)))
	      ;; If point is at the fill-point, do not `save-excursion'.
	      ;; Otherwise, if a comment prefix or fill-prefix is inserted,
	      ;; point will end up before it rather than after it.
	      (if (save-excursion
		    (skip-chars-backward " \t")
		    (= (point) fill-point))
		  (funcall comment-line-break-function t)
		(save-excursion
		  (goto-char fill-point)
		  (funcall comment-line-break-function t)))
	      ;; Now do justification, if required
	      (if (not (eq justify 'left))
		  (save-excursion
		    (end-of-line 0)
		    (justify-current-line justify nil t)))
	      ;; If making the new line didn't reduce the hpos of
	      ;; the end of the line, then give up now;
	      ;; trying again will not help.
	      (if (>= (current-column) prev-column)
		  (setq give-up t))))))
      ;; Justify last line.
      (justify-current-line justify t t)
      t)))

(defvar normal-auto-fill-function 'do-auto-fill
  "The function to use for `auto-fill-function' if Auto Fill mode is turned on.
Some major modes set this.")

(defun auto-fill-mode (&optional arg)
  "Toggle Auto Fill mode.
With arg, turn Auto Fill mode on if and only if arg is positive.
In Auto Fill mode, inserting a space at a column beyond `current-fill-column'
automatically breaks the line at a previous space.

The value of `normal-auto-fill-function' specifies the function to use
for `auto-fill-function' when turning Auto Fill mode on."
  (interactive "P")
  (prog1 (setq auto-fill-function
	       (if (if (null arg)
		       (not auto-fill-function)
		       (> (prefix-numeric-value arg) 0))
		   normal-auto-fill-function
		   nil))
    (force-mode-line-update)))

;; This holds a document string used to document auto-fill-mode.
(defun auto-fill-function ()
  "Automatically break line at a previous space, in insertion of text."
  nil)

(defun turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  (auto-fill-mode 1))

(defun turn-off-auto-fill ()
  "Unconditionally turn off Auto Fill mode."
  (auto-fill-mode -1))

(custom-add-option 'text-mode-hook 'turn-on-auto-fill)

(defun set-fill-column (arg)
  "Set `fill-column' to specified argument.
Use \\[universal-argument] followed by a number to specify a column.
Just \\[universal-argument] as argument means to use the current column."
  (interactive "P")
  (if (consp arg)
      (setq arg (current-column)))
  (if (not (integerp arg))
      ;; Disallow missing argument; it's probably a typo for C-x C-f.
      (error "set-fill-column requires an explicit argument")
    (message "Fill column set to %d (was %d)" arg fill-column)
    (setq fill-column arg)))

(defun set-selective-display (arg)
  "Set `selective-display' to ARG; clear it if no arg.
When the value of `selective-display' is a number > 0,
lines whose indentation is >= that value are not displayed.
The variable `selective-display' has a separate value for each buffer."
  (interactive "P")
  (if (eq selective-display t)
      (error "selective-display already in use for marked lines"))
  (let ((current-vpos
	 (save-restriction
	   (narrow-to-region (point-min) (point))
	   (goto-char (window-start))
	   (vertical-motion (window-height)))))
    (setq selective-display
	  (and arg (prefix-numeric-value arg)))
    (recenter current-vpos))
  (set-window-start (selected-window) (window-start (selected-window)))
  (princ "selective-display set to " t)
  (prin1 selective-display t)
  (princ "." t))

(defvar overwrite-mode-textual " Ovwrt"
  "The string displayed in the mode line when in overwrite mode.")
(defvar overwrite-mode-binary " Bin Ovwrt"
  "The string displayed in the mode line when in binary overwrite mode.")

(defun overwrite-mode (arg)
  "Toggle overwrite mode.
With arg, turn overwrite mode on iff arg is positive.
In overwrite mode, printing characters typed in replace existing text
on a one-for-one basis, rather than pushing it to the right.  At the
end of a line, such characters extend the line.  Before a tab,
such characters insert until the tab is filled in.
\\[quoted-insert] still inserts characters in overwrite mode; this
is supposed to make it easier to insert characters when necessary."
  (interactive "P")
  (setq overwrite-mode
	(if (if (null arg) (not overwrite-mode)
	      (> (prefix-numeric-value arg) 0))
	    'overwrite-mode-textual))
  (force-mode-line-update))

(defun binary-overwrite-mode (arg)
  "Toggle binary overwrite mode.
With arg, turn binary overwrite mode on iff arg is positive.
In binary overwrite mode, printing characters typed in replace
existing text.  Newlines are not treated specially, so typing at the
end of a line joins the line to the next, with the typed character
between them.  Typing before a tab character simply replaces the tab
with the character typed.
\\[quoted-insert] replaces the text at the cursor, just as ordinary
typing characters do.

Note that binary overwrite mode is not its own minor mode; it is a
specialization of overwrite-mode, entered by setting the
`overwrite-mode' variable to `overwrite-mode-binary'."
  (interactive "P")
  (setq overwrite-mode
	(if (if (null arg)
		(not (eq overwrite-mode 'overwrite-mode-binary))
	      (> (prefix-numeric-value arg) 0))
	    'overwrite-mode-binary))
  (force-mode-line-update))

(defcustom line-number-mode t
  "*Non-nil means display line number in mode line."
  :type 'boolean
  :group 'editing-basics)

(defun line-number-mode (arg)
  "Toggle Line Number mode.
With arg, turn Line Number mode on iff arg is positive.
When Line Number mode is enabled, the line number appears
in the mode line.

Line numbers do not appear for very large buffers and buffers
with very long lines; see variables `line-number-display-limit'
and `line-number-display-limit-width'."
  (interactive "P")
  (setq line-number-mode
	(if (null arg) (not line-number-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defcustom column-number-mode nil
  "*Non-nil means display column number in mode line."
  :type 'boolean
  :group 'editing-basics)

(defun column-number-mode (arg)
  "Toggle Column Number mode.
With arg, turn Column Number mode on iff arg is positive.
When Column Number mode is enabled, the column number appears
in the mode line."
  (interactive "P")
  (setq column-number-mode
	(if (null arg) (not column-number-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defgroup paren-blinking nil
  "Blinking matching of parens and expressions."
  :prefix "blink-matching-"
  :group 'paren-matching)

(defcustom blink-matching-paren t
  "*Non-nil means show matching open-paren when close-paren is inserted."
  :type 'boolean
  :group 'paren-blinking)

(defcustom blink-matching-paren-on-screen t
  "*Non-nil means show matching open-paren when it is on screen.
If nil, means don't show it (but the open-paren can still be shown
when it is off screen)."
  :type 'boolean
  :group 'paren-blinking)

(defcustom blink-matching-paren-distance (* 25 1024)
  "*If non-nil, is maximum distance to search for matching open-paren."
  :type 'integer
  :group 'paren-blinking)

(defcustom blink-matching-delay 1
  "*Time in seconds to delay after showing a matching paren."
  :type 'number
  :group 'paren-blinking)

(defcustom blink-matching-paren-dont-ignore-comments nil
  "*Non-nil means `blink-matching-paren' will not ignore comments."
  :type 'boolean
  :group 'paren-blinking)

(defun blink-matching-open ()
  "Move cursor momentarily to the beginning of the sexp before point."
  (interactive)
  (and (> (point) (1+ (point-min)))
       blink-matching-paren
       ;; Verify an even number of quoting characters precede the close.
       (= 1 (logand 1 (- (point)
			 (save-excursion
			   (forward-char -1)
			   (skip-syntax-backward "/\\")
			   (point)))))
       (let* ((oldpos (point))
	      (blinkpos)
	      (mismatch))
	 (save-excursion
	   (save-restriction
	     (if blink-matching-paren-distance
		 (narrow-to-region (max (point-min)
					(- (point) blink-matching-paren-distance))
				   oldpos))
	     (condition-case ()
		 (let ((parse-sexp-ignore-comments
			(and parse-sexp-ignore-comments
			     (not blink-matching-paren-dont-ignore-comments))))
		   (setq blinkpos (scan-sexps oldpos -1)))
	       (error nil)))
	   (and blinkpos
		(/= (char-syntax (char-after blinkpos))
		    ?\$)
		(setq mismatch
		      (or (null (matching-paren (char-after blinkpos)))
			  (/= (char-after (1- oldpos))
			      (matching-paren (char-after blinkpos))))))
	   (if mismatch (setq blinkpos nil))
	   (if blinkpos
	       ;; Don't log messages about paren matching.
	       (let (message-log-max)
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (and blink-matching-paren-on-screen
			 (sit-for blink-matching-delay))
		  (goto-char blinkpos)
		  (message
		   "Matches %s"
		   ;; Show what precedes the open in its line, if anything.
		   (if (save-excursion
			 (skip-chars-backward " \t")
			 (not (bolp)))
		       (buffer-substring (progn (beginning-of-line) (point))
					 (1+ blinkpos))
		     ;; Show what follows the open in its line, if anything.
		     (if (save-excursion
			   (forward-char 1)
			   (skip-chars-forward " \t")
			   (not (eolp)))
			 (buffer-substring blinkpos
					   (progn (end-of-line) (point)))
		       ;; Otherwise show the previous nonblank line,
		       ;; if there is one.
		       (if (save-excursion
			     (skip-chars-backward "\n \t")
			     (not (bobp)))
			   (concat
			    (buffer-substring (progn
					       (skip-chars-backward "\n \t")
					       (beginning-of-line)
					       (point))
					      (progn (end-of-line)
						     (skip-chars-backward " \t")
						     (point)))
			    ;; Replace the newline and other whitespace with `...'.
			    "..."
			    (buffer-substring blinkpos (1+ blinkpos)))
			 ;; There is nothing to show except the char itself.
			 (buffer-substring blinkpos (1+ blinkpos))))))))
	     (cond (mismatch
		    (message "Mismatched parentheses"))
		   ((not blink-matching-paren-distance)
		    (message "Unmatched parenthesis"))))))))

;Turned off because it makes dbx bomb out.
(setq blink-paren-function 'blink-matching-open)

;; This executes C-g typed while Emacs is waiting for a command.
;; Quitting out of a program does not go through here;
;; that happens in the QUIT macro at the C code level.
(defun keyboard-quit ()
  "Signal a `quit' condition.
During execution of Lisp code, this character causes a quit directly.
At top-level, as an editor command, this simply beeps."
  (interactive)
  (deactivate-mark)
  (signal 'quit nil))

(define-key global-map "\C-g" 'keyboard-quit)

(defvar buffer-quit-function nil
  "Function to call to \"quit\" the current buffer, or nil if none.
\\[keyboard-escape-quit] calls this function when its more local actions
\(such as cancelling a prefix argument, minibuffer or region) do not apply.")

(defun keyboard-escape-quit ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers),
or go back to just one window (by deleting all but the selected window)."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((and transient-mark-mode
	      mark-active)
	 (deactivate-mark))
	((> (recursion-depth) 0)
	 (exit-recursive-edit))
	(buffer-quit-function
	 (funcall buffer-quit-function))
	((not (one-window-p t))
	 (delete-other-windows))
	((string-match "^ \\*" (buffer-name (current-buffer)))
	 (bury-buffer))))

(defun play-sound-file (file &optional volume device)
  "Play sound stored in FILE.
VOLUME and DEVICE correspond to the keywords of the sound
specification for `play-sound'."
  (interactive "fPlay sound file: ")
  (let ((sound (list :file file)))
    (if volume
	(plist-put sound :volume volume))
    (if device
	(plist-put sound :device device))
    (push 'sound sound)
    (play-sound sound)))

(define-key global-map "\e\e\e" 'keyboard-escape-quit)

(defcustom read-mail-command 'rmail
  "*Your preference for a mail reading package.
This is used by some keybindings which support reading mail.
See also `mail-user-agent' concerning sending mail."
  :type '(choice (function-item rmail)
		 (function-item gnus)
		 (function-item mh-rmail)
		 (function :tag "Other"))
  :version "21.1"
  :group 'mail)

(defcustom mail-user-agent 'sendmail-user-agent
  "*Your preference for a mail composition package.
Various Emacs Lisp packages (e.g. Reporter) require you to compose an
outgoing email message.  This variable lets you specify which
mail-sending package you prefer.

Valid values include:

  `sendmail-user-agent' -- use the default Emacs Mail package.
                           See Info node `(emacs)Sending Mail'.
  `mh-e-user-agent'     -- use the Emacs interface to the MH mail system.
                           See Info node `(mh-e)'.
  `message-user-agent'  -- use the Gnus Message package.
                           See Info node `(message)'.
  `gnus-user-agent'     -- like `message-user-agent', but with Gnus
                           paraphernalia, particularly the Gcc: header for
                           archiving.

Additional valid symbols may be available; check with the author of
your package for details.  The function should return non-nil if it
succeeds.

See also `read-mail-command' concerning reading mail."
  :type '(radio (function-item :tag "Default Emacs mail"
			       :format "%t\n"
			       sendmail-user-agent)
		(function-item :tag "Emacs interface to MH"
			       :format "%t\n"
			       mh-e-user-agent)
		(function-item :tag "Gnus Message package"
			       :format "%t\n"
			       message-user-agent)
		(function-item :tag "Gnus Message with full Gnus features"
			       :format "%t\n"
			       gnus-user-agent)
		(function :tag "Other"))
  :group 'mail)

(defun define-mail-user-agent (symbol composefunc sendfunc
				      &optional abortfunc hookvar)
  "Define a symbol to identify a mail-sending package for `mail-user-agent'.

SYMBOL can be any Lisp symbol.  Its function definition and/or
value as a variable do not matter for this usage; we use only certain
properties on its property list, to encode the rest of the arguments.

COMPOSEFUNC is program callable function that composes an outgoing
mail message buffer.  This function should set up the basics of the
buffer without requiring user interaction.  It should populate the
standard mail headers, leaving the `to:' and `subject:' headers blank
by default.

COMPOSEFUNC should accept several optional arguments--the same
arguments that `compose-mail' takes.  See that function's documentation.

SENDFUNC is the command a user would run to send the message.

Optional ABORTFUNC is the command a user would run to abort the
message.  For mail packages that don't have a separate abort function,
this can be `kill-buffer' (the equivalent of omitting this argument).

Optional HOOKVAR is a hook variable that gets run before the message
is actually sent.  Callers that use the `mail-user-agent' may
install a hook function temporarily on this hook variable.
If HOOKVAR is nil, `mail-send-hook' is used.

The properties used on SYMBOL are `composefunc', `sendfunc',
`abortfunc', and `hookvar'."
  (put symbol 'composefunc composefunc)
  (put symbol 'sendfunc sendfunc)
  (put symbol 'abortfunc (or abortfunc 'kill-buffer))
  (put symbol 'hookvar (or hookvar 'mail-send-hook)))

(define-mail-user-agent 'sendmail-user-agent
  'sendmail-user-agent-compose
  'mail-send-and-exit)

(defun rfc822-goto-eoh ()
  ;; Go to header delimiter line in a mail message, following RFC822 rules
  (goto-char (point-min))
  (when (re-search-forward
	 "^\\([:\n]\\|[^: \t\n]+[ \t\n]\\)" nil 'move)
    (goto-char (match-beginning 0))))

(defun sendmail-user-agent-compose (&optional to subject other-headers continue
					      switch-function yank-action
					      send-actions)
  (if switch-function
      (let ((special-display-buffer-names nil)
	    (special-display-regexps nil)
	    (same-window-buffer-names nil)
	    (same-window-regexps nil))
	(funcall switch-function "*mail*")))
  (let ((cc (cdr (assoc-ignore-case "cc" other-headers)))
	(in-reply-to (cdr (assoc-ignore-case "in-reply-to" other-headers)))
	(body (cdr (assoc-ignore-case "body" other-headers))))
    (or (mail continue to subject in-reply-to cc yank-action send-actions)
	continue
	(error "Message aborted"))
    (save-excursion
      (rfc822-goto-eoh)
      (while other-headers
	(unless (member-ignore-case (car (car other-headers))
				    '("in-reply-to" "cc" "body"))
	    (insert (car (car other-headers)) ": "
		    (cdr (car other-headers)) "\n"))
	(setq other-headers (cdr other-headers)))
      (when body
	(forward-line 1)
	(insert body))
      t)))

(define-mail-user-agent 'mh-e-user-agent
  'mh-smail-batch 'mh-send-letter 'mh-fully-kill-draft
  'mh-before-send-letter-hook)

(defun compose-mail (&optional to subject other-headers continue
			       switch-function yank-action send-actions)
  "Start composing a mail message to send.
This uses the user's chosen mail composition package
as selected with the variable `mail-user-agent'.
The optional arguments TO and SUBJECT specify recipients
and the initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

CONTINUE, if non-nil, says to continue editing a message already
being composed.

SWITCH-FUNCTION, if non-nil, is a function to use to
switch to and display the buffer used for mail composition.

YANK-ACTION, if non-nil, is an action to perform, if and when necessary,
to insert the raw text of the message being replied to.
It has the form (FUNCTION . ARGS).  The user agent will apply
FUNCTION to ARGS, to insert the raw text of the original message.
\(The user agent will also run `mail-citation-hook', *after* the
original text has been inserted in this way.)

SEND-ACTIONS is a list of actions to call when the message is sent.
Each action has the form (FUNCTION . ARGS)."
  (interactive
   (list nil nil nil current-prefix-arg))
  (let ((function (get mail-user-agent 'composefunc)))
    (funcall function to subject other-headers continue
	     switch-function yank-action send-actions)))

(defun compose-mail-other-window (&optional to subject other-headers continue
					    yank-action send-actions)
  "Like \\[compose-mail], but edit the outgoing message in another window."
  (interactive
   (list nil nil nil current-prefix-arg))
  (compose-mail to subject other-headers continue
		'switch-to-buffer-other-window yank-action send-actions))


(defun compose-mail-other-frame (&optional to subject other-headers continue
					    yank-action send-actions)
  "Like \\[compose-mail], but edit the outgoing message in another frame."
  (interactive
   (list nil nil nil current-prefix-arg))
  (compose-mail to subject other-headers continue
		'switch-to-buffer-other-frame yank-action send-actions))

(defvar set-variable-value-history nil
  "History of values entered with `set-variable'.")

(defun set-variable (var val)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
When using this interactively, enter a Lisp object for VALUE.
If you want VALUE to be a string, you must surround it with doublequotes.
VALUE is used literally, not evaluated.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read VALUE.

If VARIABLE has been defined with `defcustom', then the type information
in the definition is used to check that VALUE is valid."
  (interactive
   (let* ((default-var (variable-at-point))
          (var (if (symbolp default-var)
                   (read-variable (format "Set variable (default %s): " default-var)
                                  default-var)
                 (read-variable "Set variable: ")))
		      (minibuffer-help-form '(describe-variable var))
		      (prop (get var 'variable-interactive))
		      (prompt (format "Set %s to value: " var))
		      (val (if prop
			       ;; Use VAR's `variable-interactive' property
			       ;; as an interactive spec for prompting.
			       (call-interactively `(lambda (arg)
						      (interactive ,prop)
						      arg))
			     (read
			      (read-string prompt nil
					   'set-variable-value-history)))))
		 (list var val)))

  (let ((type (get var 'custom-type)))
    (when type
      ;; Match with custom type.
      (require 'cus-edit)
      (setq type (widget-convert type))
      (unless (widget-apply type :match val)
	(error "Value `%S' does not match type %S of %S"
	       val (car type) var))))
  (set var val)

  ;; Force a thorough redisplay for the case that the variable
  ;; has an effect on the display, like `tab-width' has.
  (force-mode-line-update))

;; Define the major mode for lists of completions.

(defvar completion-list-mode-map nil
  "Local map for completion list buffers.")
(or completion-list-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-2] 'mouse-choose-completion)
      (define-key map [down-mouse-2] nil)
      (define-key map "\C-m" 'choose-completion)
      (define-key map "\e\e\e" 'delete-completion-window)
      (define-key map [left] 'previous-completion)
      (define-key map [right] 'next-completion)
      (setq completion-list-mode-map map)))

;; Completion mode is suitable only for specially formatted data.
(put 'completion-list-mode 'mode-class 'special)

(defvar completion-reference-buffer nil
  "Record the buffer that was current when the completion list was requested.
This is a local variable in the completion list buffer.
Initial value is nil to avoid some compiler warnings.")

(defvar completion-no-auto-exit nil
  "Non-nil means `choose-completion-string' should never exit the minibuffer.
This also applies to other functions such as `choose-completion'
and `mouse-choose-completion'.")

(defvar completion-base-size nil
  "Number of chars at beginning of minibuffer not involved in completion.
This is a local variable in the completion list buffer
but it talks about the buffer in `completion-reference-buffer'.
If this is nil, it means to compare text to determine which part
of the tail end of the buffer's text is involved in completion.")

(defun delete-completion-window ()
  "Delete the completion list window.
Go to the window from which completion was requested."
  (interactive)
  (let ((buf completion-reference-buffer))
    (if (one-window-p t)
	(if (window-dedicated-p (selected-window))
	    (delete-frame (selected-frame)))
      (delete-window (selected-window))
      (if (get-buffer-window buf)
	  (select-window (get-buffer-window buf))))))

(defun previous-completion (n)
  "Move to the previous item in the completion list."
  (interactive "p")
  (next-completion (- n)))

(defun next-completion (n)
  "Move to the next item in the completion list.
With prefix argument N, move N items (negative N means move backward)."
  (interactive "p")
  (let ((beg (point-min)) (end (point-max)))
    (while (and (> n 0) (not (eobp)))
      ;; If in a completion, move to the end of it.
      (when (get-text-property (point) 'mouse-face)
	(goto-char (next-single-property-change (point) 'mouse-face nil end)))
      ;; Move to start of next one.
      (unless (get-text-property (point) 'mouse-face)
	(goto-char (next-single-property-change (point) 'mouse-face nil end)))
      (setq n (1- n)))
    (while (and (< n 0) (not (bobp)))
      (let ((prop (get-text-property (1- (point)) 'mouse-face)))
	;; If in a completion, move to the start of it.
	(when (and prop (eq prop (get-text-property (point) 'mouse-face)))
	  (goto-char (previous-single-property-change
		      (point) 'mouse-face nil beg)))
	;; Move to end of the previous completion.
	(unless (or (bobp) (get-text-property (1- (point)) 'mouse-face))
	  (goto-char (previous-single-property-change
		      (point) 'mouse-face nil beg)))
	;; Move to the start of that one.
	(goto-char (previous-single-property-change
		    (point) 'mouse-face nil beg))
	(setq n (1+ n))))))

(defun choose-completion ()
  "Choose the completion that point is in or next to."
  (interactive)
  (let (beg end completion (buffer completion-reference-buffer)
	(base-size completion-base-size))
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	(setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	(setq end (1- (point)) beg (point)))
    (if (null beg)
	(error "No completion here"))
    (setq beg (previous-single-property-change beg 'mouse-face))
    (setq end (or (next-single-property-change end 'mouse-face) (point-max)))
    (setq completion (buffer-substring beg end))
    (let ((owindow (selected-window)))
      (if (and (one-window-p t 'selected-frame)
	       (window-dedicated-p (selected-window)))
	  ;; This is a special buffer's frame
	  (iconify-frame (selected-frame))
	(or (window-dedicated-p (selected-window))
	    (bury-buffer)))
      (select-window owindow))
    (choose-completion-string completion buffer base-size)))

;; Delete the longest partial match for STRING
;; that can be found before POINT.
(defun choose-completion-delete-max-match (string)
  (let ((opoint (point))
	(len (min (length string)
		  (- (point) (point-min)))))
    (goto-char (- (point) (length string)))
    (if completion-ignore-case
	(setq string (downcase string)))
    (while (and (> len 0)
		(let ((tail (buffer-substring (point)
					      (+ (point) len))))
		  (if completion-ignore-case
		      (setq tail (downcase tail)))
		  (not (string= tail (substring string 0 len)))))
      (setq len (1- len))
      (forward-char 1))
    (delete-char len)))

;; Switch to BUFFER and insert the completion choice CHOICE.
;; BASE-SIZE, if non-nil, says how many characters of BUFFER's text
;; to keep.  If it is nil, use choose-completion-delete-max-match instead.

;; If BUFFER is the minibuffer, exit the minibuffer
;; unless it is reading a file name and CHOICE is a directory,
;; or completion-no-auto-exit is non-nil.
(defun choose-completion-string (choice &optional buffer base-size)
  (let ((buffer (or buffer completion-reference-buffer))
	(mini-p (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))))
    ;; If BUFFER is a minibuffer, barf unless it's the currently
    ;; active minibuffer.
    (if (and mini-p
	     (or (not (active-minibuffer-window))
		 (not (equal buffer
			     (window-buffer (active-minibuffer-window))))))
	(error "Minibuffer is not active for completion")
      ;; Insert the completion into the buffer where completion was requested.
      (set-buffer buffer)
      (if base-size
	  (delete-region (+ base-size (if mini-p
					  (minibuffer-prompt-end)
					(point-min)))
			 (point))
	(choose-completion-delete-max-match choice))
      (insert choice)
      (remove-text-properties (- (point) (length choice)) (point)
			      '(mouse-face nil))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
	(set-window-point window (point)))
      ;; If completing for the minibuffer, exit it with this choice.
      (and (not completion-no-auto-exit)
	   (equal buffer (window-buffer (minibuffer-window)))
	   minibuffer-completion-table
	   ;; If this is reading a file name, and the file name chosen
	   ;; is a directory, don't exit the minibuffer.
	   (if (and (eq minibuffer-completion-table 'read-file-name-internal)
		    (file-directory-p (field-string (point-max))))
	       (let ((mini (active-minibuffer-window)))
		 (select-window mini)
		 (when minibuffer-auto-raise
		   (raise-frame (window-frame mini))))
	     (exit-minibuffer))))))

(defun completion-list-mode ()
  "Major mode for buffers showing lists of possible completions.
Type \\<completion-list-mode-map>\\[choose-completion] in the completion list\
 to select the completion near point.
Use \\<completion-list-mode-map>\\[mouse-choose-completion] to select one\
 with the mouse."
  (interactive)
  (kill-all-local-variables)
  (use-local-map completion-list-mode-map)
  (setq mode-name "Completion List")
  (setq major-mode 'completion-list-mode)
  (make-local-variable 'completion-base-size)
  (setq completion-base-size nil)
  (run-hooks 'completion-list-mode-hook))

(defun completion-list-mode-finish ()
  "Finish setup of the completions buffer.
Called from `temp-buffer-show-hook'."
  (when (eq major-mode 'completion-list-mode)
    (toggle-read-only 1)))

(add-hook 'temp-buffer-show-hook 'completion-list-mode-finish)

(defvar completion-setup-hook nil
  "Normal hook run at the end of setting up a completion list buffer.
When this hook is run, the current buffer is the one in which the
command to display the completion list buffer was run.
The completion list buffer is available as the value of `standard-output'.")

;; This function goes in completion-setup-hook, so that it is called
;; after the text of the completion list buffer is written.

(defun completion-setup-function ()
  (save-excursion
    (let ((mainbuf (current-buffer)))
      (set-buffer standard-output)
      (completion-list-mode)
      (make-local-variable 'completion-reference-buffer)
      (setq completion-reference-buffer mainbuf)
      (if (eq minibuffer-completion-table 'read-file-name-internal)
	  ;; For file name completion,
	  ;; use the number of chars before the start of the
	  ;; last file name component.
	  (setq completion-base-size
		(save-excursion
		  (set-buffer mainbuf)
		  (goto-char (point-max))
		  (skip-chars-backward (format "^%c" directory-sep-char))
		  (- (point) (minibuffer-prompt-end))))
	;; Otherwise, in minibuffer, the whole input is being completed.
	(save-match-data
	  (if (string-match "\\` \\*Minibuf-[0-9]+\\*\\'"
			    (buffer-name mainbuf))
	      (setq completion-base-size 0))))
      (goto-char (point-min))
      (if (display-mouse-p)
	  (insert (substitute-command-keys
		   "Click \\[mouse-choose-completion] on a completion to select it.\n")))
      (insert (substitute-command-keys
	       "In this buffer, type \\[choose-completion] to \
select the completion near point.\n\n")))))

(add-hook 'completion-setup-hook 'completion-setup-function)

(define-key minibuffer-local-completion-map [prior]
  'switch-to-completions)
(define-key minibuffer-local-must-match-map [prior]
  'switch-to-completions)
(define-key minibuffer-local-completion-map "\M-v"
  'switch-to-completions)
(define-key minibuffer-local-must-match-map "\M-v"
  'switch-to-completions)

(defun switch-to-completions ()
  "Select the completion list window."
  (interactive)
  ;; Make sure we have a completions window.
  (or (get-buffer-window "*Completions*")
      (minibuffer-completion-help))
  (let ((window (get-buffer-window "*Completions*")))
    (when window
      (select-window window)
      (goto-char (point-min))
      (search-forward "\n\n")
      (forward-line 1))))

;; Support keyboard commands to turn on various modifiers.

;; These functions -- which are not commands -- each add one modifier
;; to the following event.

(defun event-apply-alt-modifier (ignore-prompt)
  "Add the Alt modifier to the following event.
For example, type \\[event-apply-alt-modifier] & to enter Alt-&."
  (vector (event-apply-modifier (read-event) 'alt 22 "A-")))
(defun event-apply-super-modifier (ignore-prompt)
  "Add the Super modifier to the following event.
For example, type \\[event-apply-super-modifier] & to enter Super-&."
  (vector (event-apply-modifier (read-event) 'super 23 "s-")))
(defun event-apply-hyper-modifier (ignore-prompt)
  "Add the Hyper modifier to the following event.
For example, type \\[event-apply-hyper-modifier] & to enter Hyper-&."
  (vector (event-apply-modifier (read-event) 'hyper 24 "H-")))
(defun event-apply-shift-modifier (ignore-prompt)
  "Add the Shift modifier to the following event.
For example, type \\[event-apply-shift-modifier] & to enter Shift-&."
  (vector (event-apply-modifier (read-event) 'shift 25 "S-")))
(defun event-apply-control-modifier (ignore-prompt)
  "Add the Ctrl modifier to the following event.
For example, type \\[event-apply-control-modifier] & to enter Ctrl-&."
  (vector (event-apply-modifier (read-event) 'control 26 "C-")))
(defun event-apply-meta-modifier (ignore-prompt)
  "Add the Meta modifier to the following event.
For example, type \\[event-apply-meta-modifier] & to enter Meta-&."
  (vector (event-apply-modifier (read-event) 'meta 27 "M-")))

(defun event-apply-modifier (event symbol lshiftby prefix)
  "Apply a modifier flag to event EVENT.
SYMBOL is the name of this modifier, as a symbol.
LSHIFTBY is the numeric value of this modifier, in keyboard events.
PREFIX is the string that represents this modifier in an event type symbol."
  (if (numberp event)
      (cond ((eq symbol 'control)
	     (if (and (<= (downcase event) ?z)
		      (>= (downcase event) ?a))
		 (- (downcase event) ?a -1)
	       (if (and (<= (downcase event) ?Z)
			(>= (downcase event) ?A))
		   (- (downcase event) ?A -1)
		 (logior (lsh 1 lshiftby) event))))
	    ((eq symbol 'shift)
	     (if (and (<= (downcase event) ?z)
		      (>= (downcase event) ?a))
		 (upcase event)
	       (logior (lsh 1 lshiftby) event)))
	    (t
	     (logior (lsh 1 lshiftby) event)))
    (if (memq symbol (event-modifiers event))
	event
      (let ((event-type (if (symbolp event) event (car event))))
	(setq event-type (intern (concat prefix (symbol-name event-type))))
	(if (symbolp event)
	    event-type
	  (cons event-type (cdr event)))))))

(define-key function-key-map [?\C-x ?@ ?h] 'event-apply-hyper-modifier)
(define-key function-key-map [?\C-x ?@ ?s] 'event-apply-super-modifier)
(define-key function-key-map [?\C-x ?@ ?m] 'event-apply-meta-modifier)
(define-key function-key-map [?\C-x ?@ ?a] 'event-apply-alt-modifier)
(define-key function-key-map [?\C-x ?@ ?S] 'event-apply-shift-modifier)
(define-key function-key-map [?\C-x ?@ ?c] 'event-apply-control-modifier)

;;;; Keypad support.

;;; Make the keypad keys act like ordinary typing keys.  If people add
;;; bindings for the function key symbols, then those bindings will
;;; override these, so this shouldn't interfere with any existing
;;; bindings.

;; Also tell read-char how to handle these keys.
(mapc
 (lambda (keypad-normal)
   (let ((keypad (nth 0 keypad-normal))
	 (normal (nth 1 keypad-normal)))
     (put keypad 'ascii-character normal)
     (define-key function-key-map (vector keypad) (vector normal))))
 '((kp-0 ?0) (kp-1 ?1) (kp-2 ?2) (kp-3 ?3) (kp-4 ?4)
   (kp-5 ?5) (kp-6 ?6) (kp-7 ?7) (kp-8 ?8) (kp-9 ?9)
   (kp-space ?\ )
   (kp-tab ?\t)
   (kp-enter ?\r)
   (kp-multiply ?*)
   (kp-add ?+)
   (kp-separator ?,)
   (kp-subtract ?-)
   (kp-decimal ?.)
   (kp-divide ?/)
   (kp-equal ?=)))

;;;;
;;;; forking a twin copy of a buffer.
;;;;

(defvar clone-buffer-hook nil
  "Normal hook to run in the new buffer at the end of `clone-buffer'.")

(defun clone-process (process &optional newname)
  "Create a twin copy of PROCESS.
If NEWNAME is nil, it defaults to PROCESS' name;
NEWNAME is modified by adding or incrementing <N> at the end as necessary.
If PROCESS is associated with a buffer, the new process will be associated
  with the current buffer instead.
Returns nil if PROCESS has already terminated."
  (setq newname (or newname (process-name process)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (when (memq (process-status process) '(run stop open))
    (let* ((process-connection-type (process-tty-name process))
	   (new-process
	    (if (memq (process-status process) '(open))
		(let ((args (process-contact process t)))
		  (setq args (plist-put args :name newname))
		  (setq args (plist-put args :buffer
					(if (process-buffer process) (current-buffer))))
		  (apply 'make-network-process args))
	      (apply 'start-process newname
		     (if (process-buffer process) (current-buffer))
		     (process-command process)))))
      (set-process-query-on-exit-flag
       new-process (process-query-on-exit-flag process))
      (set-process-inherit-coding-system-flag
       new-process (process-inherit-coding-system-flag process))
      (set-process-filter new-process (process-filter process))
      (set-process-sentinel new-process (process-sentinel process))
      new-process)))

;; things to maybe add (currently partly covered by `funcall mode'):
;; - syntax-table
;; - overlays
(defun clone-buffer (&optional newname display-flag)
  "Create a twin copy of the current buffer.
If NEWNAME is nil, it defaults to the current buffer's name;
NEWNAME is modified by adding or incrementing <N> at the end as necessary.

If DISPLAY-FLAG is non-nil, the new buffer is shown with `pop-to-buffer'.
This runs the normal hook `clone-buffer-hook' in the new buffer
after it has been set up properly in other respects."
  (interactive
   (progn
     (if buffer-file-name
	 (error "Cannot clone a file-visiting buffer"))
     (if (get major-mode 'no-clone)
	 (error "Cannot clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg (read-string "Name: "))
	   t)))
  (if buffer-file-name
      (error "Cannot clone a file-visiting buffer"))
  (if (get major-mode 'no-clone)
      (error "Cannot clone a buffer in %s mode" mode-name))
  (setq newname (or newname (buffer-name)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (let ((buf (current-buffer))
	(ptmin (point-min))
	(ptmax (point-max))
	(pt (point))
	(mk (if mark-active (mark t)))
	(modified (buffer-modified-p))
	(mode major-mode)
	(lvars (buffer-local-variables))
	(process (get-buffer-process (current-buffer)))
	(new (generate-new-buffer (or newname (buffer-name)))))
    (save-restriction
      (widen)
      (with-current-buffer new
	(insert-buffer-substring buf)))
    (with-current-buffer new
      (narrow-to-region ptmin ptmax)
      (goto-char pt)
      (if mk (set-mark mk))
      (set-buffer-modified-p modified)

      ;; Clone the old buffer's process, if any.
      (when process (clone-process process))

      ;; Now set up the major mode.
      (funcall mode)

      ;; Set up other local variables.
      (mapcar (lambda (v)
		(condition-case ()	;in case var is read-only
		    (if (symbolp v)
			(makunbound v)
		      (set (make-local-variable (car v)) (cdr v)))
		  (error nil)))
	      lvars)

      ;; Run any hooks (typically set up by the major mode
      ;; for cloning to work properly).
      (run-hooks 'clone-buffer-hook))
    (if display-flag (pop-to-buffer new))
    new))


(defun clone-indirect-buffer (newname display-flag &optional norecord)
  "Create an indirect buffer that is a twin copy of the current buffer.

Give the indirect buffer name NEWNAME.  Interactively, read NEW-NAME
from the minibuffer when invoked with a prefix arg.  If NEWNAME is nil
or if not called with a prefix arg, NEWNAME defaults to the current
buffer's name.  The name is modified by adding a `<N>' suffix to it
or by incrementing the N in an existing suffix.

DISPLAY-FLAG non-nil means show the new buffer with `pop-to-buffer'.
This is always done when called interactively.

Optional last arg NORECORD non-nil means do not put this buffer at the
front of the list of recently selected ones."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
	 (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
	       (read-string "BName of indirect buffer: "))
	   t)))
  (if (get major-mode 'no-clone-indirect)
      (error "Cannot indirectly clone a buffer in %s mode" mode-name))
  (setq newname (or newname (buffer-name)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (let* ((name (generate-new-buffer-name newname))
	 (buffer (make-indirect-buffer (current-buffer) name t)))
    (when display-flag
      (pop-to-buffer buffer norecord))
    buffer))


(defun clone-indirect-buffer-other-window (buffer &optional norecord)
  "Create an indirect buffer that is a twin copy of BUFFER.
Select the new buffer in another window.
Optional second arg NORECORD non-nil means do not put this buffer at
the front of the list of recently selected ones."
  (interactive "bClone buffer in other window: ")
  (let ((pop-up-windows t))
    (set-buffer buffer)
    (clone-indirect-buffer nil t norecord)))

(define-key ctl-x-4-map "c" 'clone-indirect-buffer-other-window)

;;; Handling of Backspace and Delete keys.

(defcustom normal-erase-is-backspace nil
  "If non-nil, Delete key deletes forward and Backspace key deletes backward.

On window systems, the default value of this option is chosen
according to the keyboard used.  If the keyboard has both a Backspace
key and a Delete key, and both are mapped to their usual meanings, the
option's default value is set to t, so that Backspace can be used to
delete backward, and Delete can be used to delete forward.

If not running under a window system, customizing this option accomplishes
a similar effect by mapping C-h, which is usually generated by the
Backspace key, to DEL, and by mapping DEL to C-d via
`keyboard-translate'.  The former functionality of C-h is available on
the F1 key.  You should probably not use this setting if you don't
have both Backspace, Delete and F1 keys.

Setting this variable with setq doesn't take effect.  Programmatically,
call `normal-erase-is-backspace-mode' (which see) instead."
  :type 'boolean
  :group 'editing-basics
  :version "21.1"
  :set (lambda (symbol value)
	 ;; The fboundp is because of a problem with :set when
	 ;; dumping Emacs.  It doesn't really matter.
	 (if (fboundp 'normal-erase-is-backspace-mode)
	     (normal-erase-is-backspace-mode (or value 0))
	   (set-default symbol value))))


(defun normal-erase-is-backspace-mode (&optional arg)
  "Toggle the Erase and Delete mode of the Backspace and Delete keys.

With numeric arg, turn the mode on if and only if ARG is positive.

On window systems, when this mode is on, Delete is mapped to C-d and
Backspace is mapped to DEL; when this mode is off, both Delete and
Backspace are mapped to DEL.  (The remapping goes via
`function-key-map', so binding Delete or Backspace in the global or
local keymap will override that.)

In addition, on window systems, the bindings of C-Delete, M-Delete,
C-M-Delete, C-Backspace, M-Backspace, and C-M-Backspace are changed in
the global keymap in accordance with the functionality of Delete and
Backspace.  For example, if Delete is remapped to C-d, which deletes
forward, C-Delete is bound to `kill-word', but if Delete is remapped
to DEL, which deletes backward, C-Delete is bound to
`backward-kill-word'.

If not running on a window system, a similar effect is accomplished by
remapping C-h (normally produced by the Backspace key) and DEL via
`keyboard-translate': if this mode is on, C-h is mapped to DEL and DEL
to C-d; if it's off, the keys are not remapped.

When not running on a window system, and this mode is turned on, the
former functionality of C-h is available on the F1 key.  You should
probably not turn on this mode on a text-only terminal if you don't
have both Backspace, Delete and F1 keys.

See also `normal-erase-is-backspace'."
  (interactive "P")
  (setq normal-erase-is-backspace
	(if arg
	    (> (prefix-numeric-value arg) 0)
	  (not normal-erase-is-backspace)))

  (cond ((or (memq window-system '(x w32 mac pc))
	     (memq system-type '(ms-dos windows-nt)))
	 (let ((bindings
		`(([C-delete] [C-backspace])
		  ([M-delete] [M-backspace])
		  ([C-M-delete] [C-M-backspace])
		  (,esc-map
		   [C-delete] [C-backspace])))
	       (old-state (lookup-key function-key-map [delete])))

	   (if normal-erase-is-backspace
	       (progn
		 (define-key function-key-map [delete] [?\C-d])
		 (define-key function-key-map [kp-delete] [?\C-d])
		 (define-key function-key-map [backspace] [?\C-?]))
	     (define-key function-key-map [delete] [?\C-?])
	     (define-key function-key-map [kp-delete] [?\C-?])
	     (define-key function-key-map [backspace] [?\C-?]))

	   ;; Maybe swap bindings of C-delete and C-backspace, etc.
	   (unless (equal old-state (lookup-key function-key-map [delete]))
	     (dolist (binding bindings)
	       (let ((map global-map))
		 (when (keymapp (car binding))
		   (setq map (car binding) binding (cdr binding)))
		 (let* ((key1 (nth 0 binding))
			(key2 (nth 1 binding))
			(binding1 (lookup-key map key1))
			(binding2 (lookup-key map key2)))
		   (define-key map key1 binding2)
		   (define-key map key2 binding1)))))))
	 (t
	  (if normal-erase-is-backspace
	      (progn
		(keyboard-translate ?\C-h ?\C-?)
		(keyboard-translate ?\C-? ?\C-d))
	    (keyboard-translate ?\C-h ?\C-h)
	    (keyboard-translate ?\C-? ?\C-?))))

  (run-hooks 'normal-erase-is-backspace-hook)
  (if (interactive-p)
      (message "Delete key deletes %s"
	       (if normal-erase-is-backspace "forward" "backward"))))


;;; make-network-process wrappers

(if (featurep 'make-network-process)
    (progn

(defun open-network-stream (name buffer host service)
  "Open a TCP connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
specifying a port number to connect to."
  (make-network-process :name name :buffer buffer
			:host host :service service))

(defun open-network-stream-nowait (name buffer host service &optional sentinel filter)
  "Initiate connection to a TCP connection for a service to a host.
It returns nil if non-blocking connects are not supported; otherwise,
it returns a subprocess-object to represent the connection.

This function is similar to `open-network-stream', except that this
function returns before the connection is established.  When the
connection is completed, the sentinel function will be called with
second arg matching `open' (if successful) or `failed' (on error).

Args are NAME BUFFER HOST SERVICE SENTINEL FILTER.
NAME, BUFFER, HOST, and SERVICE are as for `open-network-stream'.
Optional args, SENTINEL and FILTER specifies the sentinel and filter
functions to be used for this network stream."
  (if (featurep 'make-network-process  '(:nowait t))
      (make-network-process :name name :buffer buffer :nowait t
			    :host host :service service
			    :filter filter :sentinel sentinel)))

(defun open-network-stream-server (name buffer service &optional sentinel filter)
  "Create a network server process for a TCP service.
It returns nil if server processes are not supported; otherwise,
it returns a subprocess-object to represent the server.

When a client connects to the specified service, a new subprocess
is created to handle the new connection, and the sentinel function
is called for the new process.

Args are NAME BUFFER SERVICE SENTINEL FILTER.
NAME is name for the server process.  Client processes are named by
appending the ip-address and port number of the client to NAME.
BUFFER is the buffer (or buffer-name) to associate with the server
process.  Client processes will not get a buffer if a process filter
is specified or BUFFER is nil; otherwise, a new buffer is created for
the client process.  The name is similar to the process name.
Third arg SERVICE is name of the service desired, or an integer
specifying a port number to connect to.  It may also be t to selected
an unused port number for the server.
Optional args, SENTINEL and FILTER specifies the sentinel and filter
functions to be used for the client processes; the server process
does not use these function."
  (if (featurep 'make-network-process '(:server t))
      (make-network-process :name name :buffer buffer
			    :service service :server t :noquery t)))

))  ;; (featurep 'make-network-process)


;; compatibility

(defun process-kill-without-query (process &optional flag)
  "Say no query needed if PROCESS is running when Emacs is exited.
Optional second argument if non-nil says to require a query.
Value is t if a query was formerly required.  
New code should not use this function; use `process-query-on-exit-flag'
or `set-process-query-on-exit-flag' instead."
  (let ((old (process-query-on-exit-flag process)))
    (set-process-query-on-exit-flag process nil)
    old))

;;; Misc

(defun byte-compiling-files-p ()
  "Return t if currently byte-compiling files."
  (and (boundp 'byte-compile-current-file)
       (stringp byte-compile-current-file)))


;; Minibuffer prompt stuff.

;(defun minibuffer-prompt-modification (start end)
;  (error "You cannot modify the prompt"))
;
;
;(defun minibuffer-prompt-insertion (start end)
;  (let ((inhibit-modification-hooks t))
;    (delete-region start end)
;    ;; Discard undo information for the text insertion itself
;    ;; and for the text deletion.above.
;    (when (consp buffer-undo-list)
;      (setq buffer-undo-list (cddr buffer-undo-list)))
;    (message "You cannot modify the prompt")))
;
;
;(setq minibuffer-prompt-properties 
;  (list 'modification-hooks '(minibuffer-prompt-modification)
;	'insert-in-front-hooks '(minibuffer-prompt-insertion)))
;  

;;; simple.el ends here
