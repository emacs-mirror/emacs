;;; simple.el --- basic editing commands for Emacs

;; Copyright (C) 1985, 86, 87, 93, 94, 95 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; A grab-bag of basic Emacs commands not specifically related to some
;; major mode or to file-handling.

;;; Code:

(defun newline (&optional arg)
  "Insert a newline, and move to left margin of the new line if it's blank.
The newline is marked with the text-property `hard'.
With arg, insert that many newlines.
In Auto Fill mode, if no numeric arg, break the preceding line if it's long."
  (interactive "*P")
  ;; Inserting a newline at the end of a line produces better redisplay in
  ;; try_window_id than inserting at the beginning of a line, and the textual
  ;; result is the same.  So, if we're at beginning of line, pretend to be at
  ;; the end of the previous line.
  (let ((flag (and (not (bobp)) 
		   (bolp)
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
	  (auto-fill-function (if arg nil auto-fill-function)))
      (self-insert-command (prefix-numeric-value arg)))
    ;; Mark the newline(s) `hard'.
    (if use-hard-newlines
	(let* ((from (- (point) (if arg (prefix-numeric-value arg) 1)))
	       (sticky (get-text-property from 'rear-nonsticky)))
	  (put-text-property from (point) 'hard 't)
	  ;; If rear-nonsticky is not "t", add 'hard to rear-nonsticky list
	  (if (and (listp sticky) (not (memq 'hard sticky)))
	      (put-text-property from (point) 'rear-nonsticky
				 (cons 'hard sticky)))))
    ;; If the newline leaves the previous line blank,
    ;; and we have a left margin, delete that from the blank line.
    (or flag
	(save-excursion
	  (goto-char beforepos)
	  (beginning-of-line)
	  (and (looking-at "[ \t]$")
	       (> (current-left-margin) 0)
	       (delete-region (point) (progn (end-of-line) (point))))))
    (if flag (forward-char 1))
    ;; Indent the line after the newline, except in one case:
    ;; when we added the newline at the beginning of a line
    ;; which starts a page.
    (or was-page-start
	(move-to-left-margin nil t)))
  nil)

(defun open-line (arg)
  "Insert a newline and leave point before it.
If there is a fill prefix and/or a left-margin, insert them on the new line
if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point)))
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

(defun quoted-insert (arg)
  "Read next input character and insert it.
This is useful for inserting control characters.
You may also type up to 3 octal digits, to insert a character with that code.

In overwrite mode, this function inserts the character anyway, and
does not handle octal digits specially.  This means that if you use
overwrite as your normal editing mode, you can use this function to
insert characters when necessary.

In binary overwrite mode, this function does overwrite, and octal
digits are interpreted as a character code.  This is supposed to make
this function useful in editing binary files."
  (interactive "*p")
  (let ((char (if (or (not overwrite-mode)
		      (eq overwrite-mode 'overwrite-mode-binary))
		  (read-quoted-char)
		(read-char))))
    (if (> arg 0)
	(if (eq overwrite-mode 'overwrite-mode-binary)
	    (delete-char arg)))
    (while (> arg 0)
      (insert-and-inherit char)
      (setq arg (1- arg)))))

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

(defun delete-horizontal-space ()
  "Delete all spaces and tabs around point."
  (interactive "*")
  (skip-chars-backward " \t")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun just-one-space ()
  "Delete all spaces and tabs around point, leaving one space."
  (interactive "*")
  (skip-chars-backward " \t")
  (if (= (following-char) ? )
      (forward-char 1)
    (insert ? ))
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

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

(defun back-to-indentation ()
  "Move point to the first non-whitespace character on this line."
  (interactive)
  (beginning-of-line 1)
  (skip-chars-forward " \t"))

(defun newline-and-indent ()
  "Insert a newline, then indent according to major mode.
Indentation is done using the value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this command indents to the
column specified by the function `current-left-margin'."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
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
  (save-excursion
    (delete-region (point) (progn (skip-chars-backward " \t") (point)))
    (indent-according-to-mode))
  (newline)
  (indent-according-to-mode))

;; Internal subroutine of delete-char
(defun kill-forward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (+ (point) arg)))

;; Internal subroutine of backward-delete-char
(defun kill-backward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (- (point) arg)))

(defun backward-delete-char-untabify (arg &optional killp)
  "Delete characters backward, changing tabs into spaces.
Delete ARG chars, and kill (save in kill ring) if KILLP is non-nil.
Interactively, ARG is the prefix arg (default 1)
and KILLP is t if a prefix arg was specified."
  (interactive "*p\nP")
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
	(setq count (1- count)))))
  (delete-backward-char arg killp)
  ;; In overwrite mode, back over columns while clearing them out,
  ;; unless at end of line.
  (and overwrite-mode (not (eolp))
       (save-excursion (insert-char ?\  arg))))

(defun zap-to-char (arg char)
  "Kill up to and including ARG'th occurrence of CHAR.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to char: ")
  (kill-region (point) (progn
			 (search-forward (char-to-string char) nil nil arg)
;			 (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
			 (point))))

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
  (if arg (forward-line 1)
    ;; If the end of the buffer is not already on the screen,
    ;; then scroll specially to put it near, but not at, the bottom.
    (if (let ((old-point (point)))
	  (save-excursion
		    (goto-char (window-start))
		    (vertical-motion (window-height))
		    (< (point) old-point)))
	(progn
	  (overlay-recenter (point))
	  (recenter -3)))))

(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer.
You probably should not use this function in Lisp programs;
it is usually a mistake for a Lisp function to use any subroutine
that uses or sets the mark."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min)))

(defun count-lines-region (start end)
  "Print number of lines and characters in the region."
  (interactive "r")
  (message "Region has %d lines, %d characters"
	   (count-lines start end) (- end start)))

(defun what-line ()
  "Print the current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (message "Line %d"
	       (1+ (count-lines 1 (point)))))))

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

(defun what-cursor-position ()
  "Print info on cursor position (on screen and within buffer)."
  (interactive)
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
	    (message "point=%d of %d(%d%%) <%d - %d>  column %d %s"
		     pos total percent beg end col hscroll)
	  (message "point=%d of %d(%d%%)  column %d %s"
		   pos total percent col hscroll))
      (if (or (/= beg 1) (/= end (1+ total)))
	  (message "Char: %s (0%o, %d, 0x%x)  point=%d of %d(%d%%) <%d - %d>  column %d %s"
		   (single-key-description char) char char char pos total percent beg end col hscroll)
	(message "Char: %s (0%o, %d, 0x%x)  point=%d of %d(%d%%)  column %d %s"
		 (single-key-description char) char char char pos total percent col hscroll)))))

(defun fundamental-mode ()
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (interactive)
  (kill-all-local-variables))

(defvar read-expression-map (cons 'keymap minibuffer-local-map)
  "Minibuffer keymap used for reading Lisp expressions.")
(define-key read-expression-map "\M-\t" 'lisp-complete-symbol)

(put 'eval-expression 'disabled t)

(defvar read-expression-history nil)

;; We define this, rather than making `eval' interactive,
;; for the sake of completion of names like eval-region, eval-current-buffer.
(defun eval-expression (expression)
  "Evaluate EXPRESSION and print value in minibuffer.
Value is also consed on to front of the variable `values'."
  (interactive
   (list (read-from-minibuffer "Eval: "
			       nil read-expression-map t
			       'read-expression-history)))
  (setq values (cons (eval expression) values))
  (prin1 (car values) t))

(defun edit-and-eval-command (prompt command)
  "Prompting with PROMPT, let user edit COMMAND and eval result.
COMMAND is a Lisp expression.  Let user edit that expression in
the minibuffer, then read and evaluate the result."
  (let ((command (read-from-minibuffer prompt
				       (prin1-to-string command)
				       read-expression-map t
				       '(command-history . 1))))
    ;; If command was added to command-history as a string,
    ;; get rid of that.  We want only evallable expressions there.
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
	(minibuffer-history-position arg)
	(minibuffer-history-sexp-flag t)
	newcmd)
    (if elt
	(progn
	  (setq newcmd
		(let ((print-level nil))
		  (read-from-minibuffer
		   "Redo: " (prin1-to-string elt) read-expression-map t
		   (cons 'command-history arg))))

	  ;; If command was added to command-history as a string,
	  ;; get rid of that.  We want only evallable expressions there.
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
  "Non-nil when doing history operations on `command-history'.
More generally, indicates that the history list being acted on
contains expressions rather than strings.")
(setq minibuffer-history-variable 'minibuffer-history)
(setq minibuffer-history-position nil)
(defvar minibuffer-history-search-history nil)

(mapcar
 (lambda (key-and-command)
   (mapcar
    (lambda (keymap-and-completionp)
      ;; Arg is (KEYMAP-SYMBOL . COMPLETION-MAP-P).
      ;; If the cdr of KEY-AND-COMMAND (the command) is a cons,
      ;; its car is used if COMPLETION-MAP-P is nil, its cdr if it is t.
      (define-key (symbol-value (car keymap-and-completionp))
	(car key-and-command)
	(let ((command (cdr key-and-command)))
	  (if (consp command)
	      ;; (and ... nil) => ... turns back on the completion-oriented
	      ;; history commands which rms turned off since they seem to
	      ;; do things he doesn't like.
	      (if (and (cdr keymap-and-completionp) nil) ;XXX turned off
		  (progn (error "EMACS BUG!") (cdr command))
		(car command))
	    command))))
    '((minibuffer-local-map . nil)
      (minibuffer-local-ns-map . nil)
      (minibuffer-local-completion-map . t)
      (minibuffer-local-must-match-map . t)
      (read-expression-map . nil))))
 '(("\en" . (next-history-element . next-complete-history-element))
   ([next] . (next-history-element . next-complete-history-element))
   ("\ep" . (previous-history-element . previous-complete-history-element))
   ([prior] . (previous-history-element . previous-complete-history-element))
   ("\er" . previous-matching-history-element)
   ("\es" . next-matching-history-element)))

(defun previous-matching-history-element (regexp n)
  "Find the previous history element that matches REGEXP.
\(Previous history elements refer to earlier actions.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive
   (let* ((enable-recursive-minibuffers t)
	  (minibuffer-history-sexp-flag nil)
	  (regexp (read-from-minibuffer "Previous element matching (regexp): "
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
  (let ((history (symbol-value minibuffer-history-variable))
	prevpos
	(pos minibuffer-history-position))
    (while (/= n 0)
      (setq prevpos pos)
      (setq pos (min (max 1 (+ pos (if (< n 0) -1 1))) (length history)))
      (if (= pos prevpos)
	  (error (if (= pos 1)
		     "No later matching history item"
		   "No earlier matching history item")))
      (if (string-match regexp
			(if minibuffer-history-sexp-flag
			    (let ((print-level nil))
			      (prin1-to-string (nth (1- pos) history)))
			  (nth (1- pos) history)))
	  (setq n (+ n (if (< n 0) 1 -1)))))
    (setq minibuffer-history-position pos)
    (erase-buffer)
    (let ((elt (nth (1- pos) history)))
      (insert (if minibuffer-history-sexp-flag
		  (let ((print-level nil))
		    (prin1-to-string elt))
		elt)))
      (goto-char (point-min)))
  (if (or (eq (car (car command-history)) 'previous-matching-history-element)
	  (eq (car (car command-history)) 'next-matching-history-element))
      (setq command-history (cdr command-history))))

(defun next-matching-history-element (regexp n)
  "Find the next history element that matches REGEXP.
\(The next history element refers to a more recent action.)
With prefix argument N, search for Nth next match.
If N is negative, find the previous or Nth previous match."
  (interactive
   (let* ((enable-recursive-minibuffers t)
	  (minibuffer-history-sexp-flag nil)
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

(defun next-history-element (n)
  "Insert the next element of the minibuffer history into the minibuffer."
  (interactive "p")
  (or (zerop n)
      (let ((narg (min (max 1 (- minibuffer-history-position n))
		       (length (symbol-value minibuffer-history-variable)))))
	    (if (or (zerop narg)
		    (= minibuffer-history-position narg))
		(error (if (if (zerop narg)
			       (> n 0)
			     (= minibuffer-history-position 1))
		       "End of history; no next item"
		     "Beginning of history; no preceding item"))
	  (erase-buffer)
	  (setq minibuffer-history-position narg)
	  (let ((elt (nth (1- minibuffer-history-position)
			  (symbol-value minibuffer-history-variable))))
	    (insert
	     (if minibuffer-history-sexp-flag
		 (let ((print-level nil))
		   (prin1-to-string elt))
	       elt)))
	  (goto-char (point-min))))))

(defun previous-history-element (n)
  "Inserts the previous element of the minibuffer history into the minibuffer."
  (interactive "p")
  (next-history-element (- n)))

(defun next-complete-history-element (n)
  "Get next element of history which is a completion of minibuffer contents."
  (interactive "p")
  (let ((point-at-start (point)))
    (next-matching-history-element
     (concat "^" (regexp-quote (buffer-substring (point-min) (point)))) n)
    ;; next-matching-history-element always puts us at (point-min).
    ;; Move to the position we were at before changing the buffer contents.
    ;; This is still sensical, because the text before point has not changed.
    (goto-char point-at-start)))

(defun previous-complete-history-element (n)
  "\
Get previous element of history which is a completion of minibuffer contents."
  (interactive "p")
  (next-complete-history-element (- n)))

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

;Put this on C-x u, so we can force that rather than C-_ into startup msg
(define-function 'advertised-undo 'undo)

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  ;; If we don't get all the way thru, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (let ((modified (buffer-modified-p))
	(recent-save (recent-auto-save-p)))
    (or (eq (selected-window) (minibuffer-window))
	(message "Undo!"))
    (or (eq last-command 'undo)
	(progn (undo-start)
	       (undo-more 1)))
    (undo-more (or arg 1))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    (let ((tail buffer-undo-list)
	  done)
      (while (and tail (not done) (not (null (car tail))))
	(if (integerp (car tail))
	    (progn
	      (setq done t)
	      (setq buffer-undo-list (delq (car tail) buffer-undo-list))))
	(setq tail (cdr tail))))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary recent-save)))
  ;; If we do get all the way thru, make this-command indicate that.
  (setq this-command 'undo))

(defvar pending-undo-list nil
  "Within a run of consecutive undo commands, list remaining to be undone.")

(defun undo-start ()
  "Set `pending-undo-list' to the front of the undo list.
The next call to `undo-more' will undo the most recently made change."
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (setq pending-undo-list buffer-undo-list))

(defun undo-more (count)
  "Undo back N undo-boundaries beyond what was already undone recently.
Call `undo-start' to get ready to undo recent changes,
then call `undo-more' one or more times to undo them."
  (or pending-undo-list
      (error "No further undo information"))
  (setq pending-undo-list (primitive-undo count pending-undo-list)))

(defvar shell-command-history nil
  "History list for some commands that read shell commands.")

(defvar shell-command-switch "-c"
  "Switch used to have the shell execute its command line argument.")

(defun shell-command (command &optional output-buffer)
  "Execute string COMMAND in inferior shell; display output, if any.

If COMMAND ends in ampersand, execute it asynchronously.
The output appears in the buffer `*Async Shell Command*'.

Otherwise, COMMAND is executed synchronously.  The output appears
in the buffer `*Shell Command Output*'.
If the output is one line, it is displayed in the echo area *as well*,
but it is nonetheless available in buffer `*Shell Command Output*',
even though that buffer is not automatically displayed.
If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

The optional second argument OUTPUT-BUFFER, if non-nil,
says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in current buffer.  (This cannot be done asynchronously.)
In either case, the output is inserted after point (leaving mark after it)."
  (interactive (list (read-from-minibuffer "Shell command: "
					   nil nil nil 'shell-command-history)
		     current-prefix-arg))
  (if (and output-buffer
	   (not (or (bufferp output-buffer)  (stringp output-buffer))))
      (progn (barf-if-buffer-read-only)
	     (push-mark)
	     ;; We do not use -f for csh; we will not support broken use of
	     ;; .cshrcs.  Even the BSD csh manual says to use
	     ;; "if ($?prompt) exit" before things which are not useful
	     ;; non-interactively.  Besides, if someone wants their other
	     ;; aliases for shell commands then they can still have them.
	     (call-process shell-file-name nil t nil
			   shell-command-switch command)
	     ;; This is like exchange-point-and-mark, but doesn't activate the mark.
	     ;; It is cleaner to avoid activation, even though the command
	     ;; loop would deactivate the mark because we inserted text.
	     (goto-char (prog1 (mark t)
			  (set-marker (mark-marker) (point)
				      (current-buffer)))))
    ;; Preserve the match data in case called from a program.
    (let ((data (match-data)))
      (unwind-protect
	  (if (string-match "[ \t]*&[ \t]*$" command)
	      ;; Command ending with ampersand means asynchronous.
	      (let ((buffer (get-buffer-create
			     (or output-buffer "*Asynch Shell Command*")))
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
		  (setq proc (start-process "Shell" buffer 
					    shell-file-name 
					    shell-command-switch command))
		  (setq mode-line-process '(":%s"))
		  (set-process-sentinel proc 'shell-command-sentinel)
		  (set-process-filter proc 'shell-command-filter)
		  ))
	    (shell-command-on-region (point) (point) command nil))
	(store-match-data data)))))

;; We have a sentinel to prevent insertion of a termination message
;; in the buffer itself.
(defun shell-command-sentinel (process signal)
  (if (and (memq (process-status process) '(exit signal))
	   (buffer-name (process-buffer process)))
      (progn
	(message "%s: %s." 
		 (car (cdr (cdr (process-command process))))
		 (substring signal 0 -1))
	(save-excursion
	  (set-buffer (process-buffer process))
	  (setq mode-line-process nil))
	(delete-process process))))

(defun shell-command-filter (proc string)
  ;; Do save-excursion by hand so that we can leave point numerically unchanged
  ;; despite an insertion immediately after it.
  (let* ((obuf (current-buffer))
	 (buffer (process-buffer proc))
	 opoint
	 (window (get-buffer-window buffer))
	 (pos (window-start window)))
    (unwind-protect
	(progn
	  (set-buffer buffer)
	  (or (= (point) (point-max))
	      (setq opoint (point)))
	  (goto-char (point-max))
	  (insert-before-markers string))
      ;; insert-before-markers moved this marker: set it back.
      (set-window-start window pos)
      ;; Finish our save-excursion.
      (if opoint
	  (goto-char opoint))
      (set-buffer obuf))))

(defun shell-command-on-region (start end command
				      &optional output-buffer replace)
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.

The noninteractive arguments are START, END, COMMAND, OUTPUT-BUFFER, REPLACE.
If REPLACE is non-nil, that means insert the output
in place of text from START to END, putting point and mark around it.

If the output is one line, it is displayed in the echo area,
but it is nonetheless available in buffer `*Shell Command Output*'
even though that buffer is not automatically displayed.
If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional fourth argument OUTPUT-BUFFER is non-nil,
that says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in the current buffer.
In either case, the output is inserted after point (leaving mark after it)."
  (interactive (let ((string
		      ;; Do this before calling region-beginning
		      ;; and region-end, in case subprocess output
		      ;; relocates them while we are in the minibuffer.
		      (read-from-minibuffer "Shell command on region: "
					    nil nil nil
					    'shell-command-history)))
		 (list (region-beginning) (region-end)
		       string
		       current-prefix-arg
		       current-prefix-arg)))
  (if (or replace
	  (and output-buffer
	       (not (or (bufferp output-buffer) (stringp output-buffer)))))
      ;; Replace specified region with output from command.
      (let ((swap (and replace (< (point) (mark)))))
	;; Don't muck with mark unless REPLACE says we should.
	(goto-char start)
	(and replace (push-mark))
	(call-process-region start end shell-file-name t t nil
			     shell-command-switch command)
	(let ((shell-buffer (get-buffer "*Shell Command Output*")))
	  (and shell-buffer (not (eq shell-buffer (current-buffer)))
	       (kill-buffer shell-buffer)))
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
		     (delete-region end (point-max))
		     (delete-region (point-min) start)
		     (call-process-region (point-min) (point-max)
					  shell-file-name t t nil
					  shell-command-switch command)
		     (setq success t))
	    ;; Clear the output buffer, then run the command with output there.
	    (save-excursion
	      (set-buffer buffer)
	      (setq buffer-read-only nil)
	      (erase-buffer))
	    (call-process-region start end shell-file-name
				 nil buffer nil
				 shell-command-switch command)
	    (setq success t))
	;; Report the amount of output.
	(let ((lines (save-excursion
		       (set-buffer buffer)
		       (if (= (buffer-size) 0)
			   0
			 (count-lines (point-min) (point-max))))))
	  (cond ((= lines 0)
		 (if success
		     (message "(Shell command completed with no output)"))
		 (kill-buffer buffer))
		((and success (= lines 1))
		 (message "%s"
			  (save-excursion
			    (set-buffer buffer)
			    (goto-char (point-min))
			    (buffer-substring (point)
					      (progn (end-of-line) (point))))))
		(t 
		 (set-window-start (display-buffer buffer) 1))))))))

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

(defvar kill-whole-line nil
  "*If non-nil, `kill-line' with no arg at beg of line kills the whole line.")

(defun kill-line (&optional arg)
  "Kill the rest of the current line; if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

If `kill-whole-line' is non-nil, then kill the whole line
when given no argument at the beginning of a line."
  (interactive "P")
  (kill-region (point)
	       ;; It is better to move point to the other end of the kill
	       ;; before killing.  That way, in a read-only buffer, point
	       ;; moves across the text that is copied to the kill ring.
	       ;; The choice has no effect on undo now that undo records
	       ;; the value of point from before the command was run.
	       (progn
		 (if arg
		     (forward-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
		       (forward-line 1)
		     (end-of-line)))
		 (point))))

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

(defconst kill-ring-max 30
  "*Maximum length of kill ring before oldest elements are thrown away.")

(defvar kill-ring-yank-pointer nil
  "The tail of the kill ring whose car is the last thing yanked.")

(defun kill-new (string &optional replace)
  "Make STRING the latest kill in the kill ring.
Set the kill-ring-yank pointer to point to it.
If `interprogram-cut-function' is non-nil, apply it to STRING.
Optional second argument REPLACE non-nil means that STRING will replace
the front of the kill ring, rather than being added to the list."
  (and (fboundp 'menu-bar-update-yank-menu)
       (menu-bar-update-yank-menu string (and replace (car kill-ring))))
  (if replace
      (setcar kill-ring string)
    (setq kill-ring (cons string kill-ring))
    (if (> (length kill-ring) kill-ring-max)
	(setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
  (setq kill-ring-yank-pointer kill-ring)
  (if interprogram-cut-function
      (funcall interprogram-cut-function string t)))

(defun kill-append (string before-p)
  "Append STRING to the end of the latest kill in the kill ring.
If BEFORE-P is non-nil, prepend STRING to the kill.
If `interprogram-cut-function' is set, pass the resulting kill to
it."
  (kill-new (if before-p
		(concat string (car kill-ring))
	      (concat (car kill-ring) string)) t))

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

(defvar kill-read-only-ok nil
  "*Non-nil means don't signal an error for killing read-only text.")

(defun kill-region (beg end)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to kill and then yank immediately, use \\[copy-region-as-kill].)
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
  (cond

   ;; If the buffer is read-only, we should beep, in case the person
   ;; just isn't aware of this.  However, there's no harm in putting
   ;; the region's text in the kill ring, anyway.
   ((or (and buffer-read-only (not inhibit-read-only))
	(text-property-not-all beg end 'read-only nil))
    (copy-region-as-kill beg end)
    ;; This should always barf, and give us the correct error.
    (if kill-read-only-ok
	(message "Read only text copied to kill ring")
      (setq this-command 'kill-region)
      (barf-if-buffer-read-only)))

   ;; In certain cases, we can arrange for the undo list and the kill
   ;; ring to share the same string object.  This code does that.
   ((not (or (eq buffer-undo-list t)
	     (eq last-command 'kill-region)
	     ;; Use = since positions may be numbers or markers.
	     (= beg end)))
    ;; Don't let the undo list be truncated before we can even access it.
    (let ((undo-strong-limit (+ (- (max beg end) (min beg end)) 100))
	  (old-list buffer-undo-list)
	  tail)
      (delete-region beg end)
      ;; Search back in buffer-undo-list for this string,
      ;; in case a change hook made property changes.
      (setq tail buffer-undo-list)
      (while (not (stringp (car (car tail))))
	(setq tail (cdr tail)))
      ;; Take the same string recorded for undo
      ;; and put it in the kill-ring.
      (kill-new (car (car tail)))))

   (t
    (copy-region-as-kill beg end)
    (delete-region beg end)))
  (setq this-command 'kill-region))

;; copy-region-as-kill no longer sets this-command, because it's confusing
;; to get two copies of the text when the user accidentally types M-w and
;; then corrects it with the intended C-w.
(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (buffer-substring beg end) (< end beg))
    (kill-new (buffer-substring beg end)))
  nil)

(defun kill-ring-save (beg end)
  "Save the region as if killed, but don't kill it.
This command is similar to `copy-region-as-kill', except that it gives
visual feedback indicating the extent of the region being copied.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste."
  (interactive "r")
  (copy-region-as-kill beg end)
  (if (interactive-p)
      (let ((other-end (if (= (point) beg) end beg))
	    (opoint (point))
	    ;; Inhibit quitting so we can make a quit here
	    ;; look like a C-g typed as a command.
	    (inhibit-quit t))
	(if (pos-visible-in-window-p other-end (selected-window))
	    (progn
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

(defun append-next-kill ()
  "Cause following command, if it kills, to append to previous kill."
  (interactive)
  (if (interactive-p)
      (progn
	(setq this-command 'kill-region)
	(message "If the next command is a kill, it will append"))
    (setq last-command 'kill-region)))

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
  (let ((before (< (point) (mark t))))
    (delete-region (point) (mark t))
    (set-marker (mark-marker) (point) (current-buffer))
    (insert (current-kill arg))
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
  (insert (current-kill (cond
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


(defun insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  (interactive (list (progn (barf-if-buffer-read-only)
			    (read-buffer "Insert buffer: " 
					 (other-buffer (current-buffer) t)
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
      (set-buffer (get-buffer-create buffer))
      (insert-buffer-substring oldbuf start end))))

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
      (erase-buffer)
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

(defvar mark-even-if-inactive nil
  "*Non-nil means you can use the mark even when inactive.
This option makes a difference in Transient Mark mode.
When the option is non-nil, deactivation of the mark
turns off region highlighting, but commands that use the mark
behave as if the mark were still active.")

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
  (if transient-mark-mode
      (progn
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

(defconst mark-ring-max 16
  "*Maximum size of mark ring.  Start discarding off end if gets this big.")

(defvar global-mark-ring nil
  "The list of saved global marks, most recent first.")

(defconst global-mark-ring-max 16
  "*Maximum size of global mark ring.  \
Start discarding off end if gets this big.")

(defun set-mark-command (arg)
  "Set mark at where point is, or jump to mark.
With no prefix argument, set mark, push old mark position on local mark
ring, and push mark on global mark ring.
With argument, jump to mark, and pop a new position for mark off the ring
\(does not affect global mark ring\).

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (interactive "P")
  (if (null arg)
      (progn
	(push-mark nil nil t))
    (if (null (mark t))
	(error "No mark set in this buffer")
      (goto-char (mark t))
      (pop-mark))))

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
  (or nomsg executing-macro (> (minibuffer-depth) 0)
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

(define-function 'exchange-dot-and-mark 'exchange-point-and-mark)
(defun exchange-point-and-mark ()
  "Put the mark where point is now, and point where the mark is now.
This command works even when the mark is not active,
and it reactivates the mark."
  (interactive nil)
  (let ((omark (mark t)))
    (if (null omark)
	(error "No mark set in this buffer"))
    (set-mark (point))
    (goto-char omark)
    nil))

(defun transient-mark-mode (arg)
  "Toggle Transient Mark mode.
With arg, turn Transient Mark mode on if arg is positive, off otherwise.

In Transient Mark mode, when the mark is active, the region is highlighted.
Changing the buffer \"deactivates\" the mark.
So do certain other operations that set the mark
but whose main purpose is something else--for example,
incremental search, \\[beginning-of-buffer], and \\[end-of-buffer]."
  (interactive "P")
  (setq transient-mark-mode
	(if (null arg)
	    (not transient-mark-mode)
	  (> (prefix-numeric-value arg) 0))))

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

(defvar next-line-add-newlines t
  "*If non-nil, `next-line' inserts newline to avoid `end of buffer' error.")

(defun next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one, behavior depends on the
value of `next-line-add-newlines'.  If non-nil, it inserts a newline character
to create a line, and moves the cursor to that line.  Otherwise it moves the
cursor to the end of the buffer (if already at the end of the buffer, an error
is signaled).

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.  This goal column is stored
in `goal-column', which is nil when there is none.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (and next-line-add-newlines (= arg 1))
      (let ((opoint (point)))
	(end-of-line)
	(if (eobp)
	    (newline 1)
	  (goto-char opoint)
	  (line-move arg)))
    (if (interactive-p)
	(condition-case nil
	    (line-move arg)
	  ((beginning-of-buffer end-of-buffer) (ding)))
      (line-move arg)))
  nil)

(defun previous-line (arg)
  "Move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with a negative argument instead.  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
	  (line-move (- arg))
	((beginning-of-buffer end-of-buffer) (ding)))
    (line-move (- arg)))
  nil)

(defconst track-eol nil
  "*Non-nil means vertical motion starting at end of line keeps to ends of lines.
This means moving to the end of each line moved onto.
The beginning of a blank line does not count as the end of a line.")

(defvar goal-column nil
  "*Semipermanent goal column for vertical motion, as set by \\[set-goal-column], or nil.")
(make-variable-buffer-local 'goal-column)

(defvar temporary-goal-column 0
  "Current goal column for vertical motion.
It is the column where point was
at the start of current run of vertical motion commands.
When the `track-eol' feature is doing its job, the value is 9999.")

(defvar line-move-ignore-invisible nil
  "*Non-nil means \\[next-line] and \\[previous-line] ignore invisible lines.
Outline mode sets this.")

(defun line-move (arg)
  (if (not (or (eq last-command 'next-line)
	       (eq last-command 'previous-line)))
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
      (or (if (> arg 0)
	      (progn (if (> arg 1) (forward-line (1- arg)))
		     ;; This way of moving forward ARG lines
		     ;; verifies that we have a newline after the last one.
		     ;; It doesn't get confused by intangible text.
		     (end-of-line)
		     (zerop (forward-line 1)))
	    (and (zerop (forward-line arg))
		 (bolp)))
	  (signal (if (< arg 0)
		      'beginning-of-buffer
		    'end-of-buffer)
		  nil))
    ;; Move by arg lines, but ignore invisible ones.
    (while (> arg 0)
      (end-of-line)
      (and (zerop (vertical-motion 1))
	   (signal 'end-of-buffer nil))
      ;; If the following character is currently invisible,
      ;; skip all characters with that same `invisible' property value.
      (while (and (not (eobp))
		  (let ((prop
			 (get-char-property (point) 'invisible)))
		    (if (eq buffer-invisibility-spec t)
			prop
		      (or (memq prop buffer-invisibility-spec)
			  (assq prop buffer-invisibility-spec)))))
	(if (get-text-property (point) 'invisible)
	    (goto-char (next-single-property-change (point) 'invisible))
	  (goto-char (next-overlay-change (point)))))
      (setq arg (1- arg)))
    (while (< arg 0)
      (beginning-of-line)
      (and (zerop (vertical-motion -1))
	   (signal 'beginning-of-buffer nil))
      (while (and (not (bobp))
		  (let ((prop
			 (get-char-property (1- (point)) 'invisible)))
		    (if (eq buffer-invisibility-spec t)
			prop
		      (or (memq prop buffer-invisibility-spec)
			  (assq prop buffer-invisibility-spec)))))
	(if (get-text-property (1- (point)) 'invisible)
	    (goto-char (previous-single-property-change (point) 'invisible))
	  (goto-char (previous-overlay-change (point)))))
      (setq arg (1+ arg))))
  (move-to-column (or goal-column temporary-goal-column))
  nil)

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

;;; Partial support for horizontal autoscrolling.  Someday, this feature
;;; will be built into the C level and all the (hscroll-point-visible) calls
;;; will go away.

(defvar hscroll-step 0
   "*The number of columns to try scrolling a window by when point moves out.
If that fails to bring point back on frame, point is centered instead.
If this is zero, point is always centered after it moves off frame.")

(defun hscroll-point-visible ()
  "Scrolls the selected window horizontally to make point visible."
  (save-excursion
    (set-buffer (window-buffer))
    (if (not (or truncate-lines
		 (> (window-hscroll) 0)
		 (and truncate-partial-width-windows
		      (< (window-width) (frame-width)))))
	;; Point is always visible when lines are wrapped.
	()
      ;; If point is on the invisible part of the line before window-start,
      ;; then hscrolling can't bring it back, so reset window-start first.
      (and (< (point) (window-start))
	   (let ((ws-bol (save-excursion
			   (goto-char (window-start))
			   (beginning-of-line)
			   (point))))
	     (and (>= (point) ws-bol)
		  (set-window-start nil ws-bol))))
      (let* ((here (hscroll-window-column))
	     (left (min (window-hscroll) 1))
	     (right (1- (window-width))))
	;; Allow for the truncation glyph, if we're not exactly at eol.
	(if (not (and (= here right)
		      (= (following-char) ?\n)))
	    (setq right (1- right)))
	(cond
	 ;; If too far away, just recenter.  But don't show too much
	 ;; white space off the end of the line.
	 ((or (< here (- left  hscroll-step))
	      (> here (+ right hscroll-step)))
	  (let ((eol (save-excursion (end-of-line) (hscroll-window-column))))
	    (scroll-left (min (- here (/ (window-width) 2))
			      (- eol (window-width) -5)))))
	 ;; Within range.  Scroll by one step (or maybe not at all).
	 ((< here left)
	  (scroll-right hscroll-step))
	 ((> here right)
	  (scroll-left hscroll-step)))))))

;; This function returns the window's idea of the display column of point,
;; assuming that the window is already known to be truncated rather than
;; wrapped, and that we've already handled the case where point is on the
;; part of the line before window-start.  We ignore window-width; if point
;; is beyond the right margin, we want to know how far.  The return value
;; includes the effects of window-hscroll, window-start, and the prompt
;; string in the minibuffer.  It may be negative due to hscroll.
(defun hscroll-window-column ()
  (let* ((hscroll (window-hscroll))
	 (startpos (save-excursion
		     (beginning-of-line)
		     (if (= (point) (save-excursion
				      (goto-char (window-start))
				      (beginning-of-line)
				      (point)))
			 (goto-char (window-start)))
		     (point)))
	 (hpos (+ (if (and (eq (selected-window) (minibuffer-window))
			   (= 1 (window-start))
			   (= startpos (point-min)))
		      (minibuffer-prompt-width)
		    0)
		  (min 0 (- 1 hscroll))))
	 val)
    (car (cdr (compute-motion startpos (cons hpos 0)
			      (point) (cons 0 1)
			      1000000 (cons hscroll 0) nil)))))

  
;; rms: (1) The definitions of arrow keys should not simply restate
;; what keys they are.  The arrow keys should run the ordinary commands.
;; (2) The arrow keys are just one of many common ways of moving point
;; within a line.  Real horizontal autoscrolling would be a good feature,
;; but supporting it only for arrow keys is too incomplete to be desirable.

;;;;; Make arrow keys do the right thing for improved terminal support
;;;;; When we implement true horizontal autoscrolling, right-arrow and
;;;;; left-arrow can lose the (if truncate-lines ...) clause and become
;;;;; aliases.  These functions are bound to the corresponding keyboard
;;;;; events in loaddefs.el.

;;(defun right-arrow (arg)
;;  "Move right one character on the screen (with prefix ARG, that many chars).
;;Scroll right if needed to keep point horizontally onscreen."
;;  (interactive "P")
;;  (forward-char arg)
;;  (hscroll-point-visible))

;;(defun left-arrow (arg)
;;  "Move left one character on the screen (with prefix ARG, that many chars).
;;Scroll left if needed to keep point horizontally onscreen."
;;  (interactive "P")
;;  (backward-char arg)
;;  (hscroll-point-visible))

(defun scroll-other-window-down (lines)
  "Scroll the \"other window\" down."
  (interactive "P")
  (scroll-other-window
   ;; Just invert the argument's meaning.
   ;; We can do that without knowing which window it will be.
   (if (eq lines '-) nil
     (if (null lines) '-
       (- (prefix-numeric-value lines))))))

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
  (interactive "*p")
  (transpose-subr 'forward-word arg))

(defun transpose-sexps (arg)
  "Like \\[transpose-words] but applies to sexps.
Does not work on a sexp that point is in the middle of
if it is a list or string."
  (interactive "*p")
  (transpose-subr 'forward-sexp arg))

(defun transpose-lines (arg)
  "Exchange current line and previous line, leaving point after both.
With argument ARG, takes previous line and moves it past ARG lines.
With argument 0, interchanges line point is in with line mark is in."
  (interactive "*p")
  (transpose-subr (function
		   (lambda (arg)
		     (if (= arg 1)
			 (progn
			   ;; Move forward over a line,
			   ;; but create a newline if none exists yet.
			   (end-of-line)
			   (if (eobp)
			       (newline)
			     (forward-char 1)))
		       (forward-line arg))))
		  arg))

(defun transpose-subr (mover arg)
  (let (start1 end1 start2 end2)
    (if (= arg 0)
	(progn
	  (save-excursion
	    (funcall mover 1)
	    (setq end2 (point))
	    (funcall mover -1)
	    (setq start2 (point))
	    (goto-char (mark))
	    (funcall mover 1)
	    (setq end1 (point))
	    (funcall mover -1)
	    (setq start1 (point))
	    (transpose-subr-1))
	  (exchange-point-and-mark)))
    (while (> arg 0)
      (funcall mover -1)
      (setq start1 (point))
      (funcall mover 1)
      (setq end1 (point))
      (funcall mover 1)
      (setq end2 (point))
      (funcall mover -1)
      (setq start2 (point))
      (transpose-subr-1)
      (goto-char end2)
      (setq arg (1- arg)))
    (while (< arg 0)
      (funcall mover -1)
      (setq start2 (point))
      (funcall mover -1)
      (setq start1 (point))
      (funcall mover 1)
      (setq end1 (point))
      (funcall mover 1)
      (setq end2 (point))
      (transpose-subr-1)
      (setq arg (1+ arg)))))

(defun transpose-subr-1 ()
  (if (> (min end1 end2) (max start1 start2))
      (error "Don't have two things to transpose"))
  (let ((word1 (buffer-substring start1 end1))
	(word2 (buffer-substring start2 end2)))
    (delete-region start2 end2)
    (goto-char start2)
    (insert word1)
    (goto-char (if (< start1 start2) start1
		 (+ start1 (- (length word1) (length word2)))))
    (delete-char (length word1))
    (insert word2)))

(defconst comment-column 32
  "*Column to indent right-margin comments to.
Setting this variable automatically makes it local to the current buffer.
Each mode establishes a different default value for this variable; you
can set the value for a particular mode using that mode's hook.")
(make-variable-buffer-local 'comment-column)

(defconst comment-start nil
  "*String to insert to start a new comment, or nil if no comment syntax.")

(defconst comment-start-skip nil
  "*Regexp to match the start of a comment plus everything up to its body.
If there are any \\(...\\) pairs, the comment delimiter text is held to begin
at the place matched by the close of the first pair.")

(defconst comment-end ""
  "*String to insert to end a new comment.
Should be an empty string if comments are terminated by end-of-line.")

(defconst comment-indent-hook nil
  "Obsolete variable for function to compute desired indentation for a comment.
This function is called with no args with point at the beginning of
the comment's starting delimiter.")

(defconst comment-indent-function
  '(lambda () comment-column)
  "Function to compute desired indentation for a comment.
This function is called with no args with point at the beginning of
the comment's starting delimiter.")

(defconst block-comment-start nil
  "*String to insert to start a new comment on a line by itself.
If nil, use `comment-start' instead.
Note that the regular expression `comment-start-skip' should skip this string
as well as the `comment-start' string.")

(defconst block-comment-end nil
  "*String to insert to end a new comment on a line by itself.
Should be an empty string if comments are terminated by end-of-line.
If nil, use `comment-end' instead.")

(defun indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment."
  (interactive "*")
  (beginning-of-line 1)
  (let* ((empty (save-excursion (beginning-of-line)
				(looking-at "[ \t]*$")))
	 (starter (or (and empty block-comment-start) comment-start))
	 (ender (or (and empty block-comment-end) comment-end)))
    (if (null starter)
	(error "No comment syntax defined")
      (let* ((eolpos (save-excursion (end-of-line) (point)))
	     cpos indent begpos)
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
	    (insert ender)))))))

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

(defun comment-region (beg end &optional arg)
  "Comment or uncomment each line in the region.
With just C-u prefix arg, uncomment each line in region.
Numeric prefix arg ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.
Comments are terminated on each line, even for syntax in which newline does
not end the comment.  Blank lines do not get comments."
  ;; if someone wants it to only put a comment-start at the beginning and
  ;; comment-end at the end then typing it, C-x C-x, closing it, C-x C-x
  ;; is easy enough.  No option is made here for other than commenting
  ;; every line.
  (interactive "r\nP")
  (or comment-start (error "No comment syntax is defined"))
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (save-restriction
      (let ((cs comment-start) (ce comment-end)
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
        (while (not (eobp))
          (if (or (eq numarg t) (< numarg 0))
	      (progn
		;; Delete comment start from beginning of line.
		(if (eq numarg t)
		    (while (looking-at (regexp-quote cs))
		      (delete-char (length cs)))
		  (let ((count numarg))
		    (while (and (> 1 (setq count (1+ count)))
				(looking-at (regexp-quote cs)))
		      (delete-char (length cs)))))
		;; Delete comment end from end of line.
                (if (string= "" ce)
		    nil
		  (if (eq numarg t)
		      (progn
			(end-of-line)
			;; This is questionable if comment-end ends in
			;; whitespace.  That is pretty brain-damaged,
			;; though.
			(skip-chars-backward " \t")
			(if (and (>= (- (point) (point-min)) (length ce))
				 (save-excursion
				   (backward-char (length ce))
				   (looking-at (regexp-quote ce))))
			    (delete-char (- (length ce)))))
		    (let ((count numarg))
		      (while (> 1 (setq count (1+ count)))
			(end-of-line)
			;; this is questionable if comment-end ends in whitespace
			;; that is pretty brain-damaged though
			(skip-chars-backward " \t")
			(save-excursion
			  (backward-char (length ce))
			  (if (looking-at (regexp-quote ce))
			      (delete-char (length ce))))))))
		(forward-line 1))
	    ;; Insert at beginning and at end.
            (if (looking-at "[ \t]*$") ()
              (insert cs)
              (if (string= "" ce) ()
                (end-of-line)
                (insert ce)))
            (search-forward "\n" nil 'move)))))))

(defun backward-word (arg)
  "Move backward until encountering the end of a word.
With argument, do this that many times.
In programs, it is faster to call `forward-word' with negative arg."
  (interactive "p")
  (forward-word (- arg)))

(defun mark-word (arg)
  "Set mark arg words away from point."
  (interactive "p")
  (push-mark
    (save-excursion
      (forward-word arg)
      (point))
    nil t))

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
		 (buffer-substring start end)))
	(buffer-substring start end)))))

(defconst fill-prefix nil
  "*String for filling to insert at front of new line, or nil for none.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'fill-prefix)

(defconst auto-fill-inhibit-regexp nil
  "*Regexp to match lines which should not be auto-filled.")

(defun do-auto-fill ()
  (let (fc justify bol give-up)
    (if (or (not (setq justify (current-justification)))
	    (and (setq fc (current-fill-column)) ; make sure this gets set
		 (eq justify 'left)
		 (<= (current-column) (setq fc (current-fill-column))))
	    (save-excursion (beginning-of-line) 
			    (setq bol (point))
			    (and auto-fill-inhibit-regexp
				 (looking-at auto-fill-inhibit-regexp))))
	nil ;; Auto-filling not required
      (if (memq justify '(full center right))
	  (save-excursion (unjustify-current-line)))
      (while (and (not give-up) (> (current-column) fc))
	  ;; Determine where to split the line.
	  (let ((fill-point
		 (let ((opoint (point))
		       bounce
		       (first t))
		   (save-excursion
		     (move-to-column (1+ fc))
		     ;; Move back to a word boundary.
		     (while (or first
				;; If this is after period and a single space,
				;; move back once more--we don't want to break
				;; the line there and make it look like a
				;; sentence end.
				(and (not (bobp))
				     (not bounce)
				     sentence-end-double-space
				     (save-excursion (forward-char -1)
						     (and (looking-at "\\. ")
							  (not (looking-at "\\.  "))))))
		       (setq first nil)
		       (skip-chars-backward "^ \t\n")
		       ;; If we find nowhere on the line to break it,
		       ;; break after one word.  Set bounce to t
		       ;; so we will not keep going in this while loop.
		       (if (bolp)
			   (progn
			     (re-search-forward "[ \t]" opoint t)
			     (setq bounce t)))
		       (skip-chars-backward " \t"))
		     ;; Let fill-point be set to the place where we end up.
		     (point)))))
	    ;; If that place is not the beginning of the line,
	    ;; break the line there.
	    (if (save-excursion
		  (goto-char fill-point)
		  (not (bolp)))
		(let ((prev-column (current-column)))
		  ;; If point is at the fill-point, do not `save-excursion'.
		  ;; Otherwise, if a comment prefix or fill-prefix is inserted,
		  ;; point will end up before it rather than after it.
		  (if (save-excursion
			(skip-chars-backward " \t")
			(= (point) fill-point))
		      (indent-new-comment-line t)
		    (save-excursion
		      (goto-char fill-point)
		      (indent-new-comment-line t)))
		  ;; Now do justification, if required
		  (if (not (eq justify 'left))
		      (save-excursion 
			(end-of-line 0)
			(justify-current-line justify nil t)))
		  ;; If making the new line didn't reduce the hpos of
		  ;; the end of the line, then give up now;
		  ;; trying again will not help.
		  (if (>= (current-column) prev-column)
		      (setq give-up t)))
	      ;; No place to break => stop trying.
	      (setq give-up t))))
      ;; justify last line
      (justify-current-line justify t t)))) 

(defun auto-fill-mode (&optional arg)
  "Toggle auto-fill mode.
With arg, turn Auto-Fill mode on if and only if arg is positive.
In Auto-Fill mode, inserting a space at a column beyond `current-fill-column'
automatically breaks the line at a previous space."
  (interactive "P")
  (prog1 (setq auto-fill-function
	       (if (if (null arg)
		       (not auto-fill-function)
		       (> (prefix-numeric-value arg) 0))
		   'do-auto-fill
		   nil))
    ;; update mode-line
    (set-buffer-modified-p (buffer-modified-p))))

;; This holds a document string used to document auto-fill-mode.
(defun auto-fill-function ()
  "Automatically break line at a previous space, in insertion of text."
  nil)

(defun turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  (auto-fill-mode 1))

(defun set-fill-column (arg)
  "Set `fill-column' to current column, or to argument if given.
The variable `fill-column' has a separate value for each buffer."
  (interactive "P")
  (setq fill-column (if (integerp arg) arg (current-column)))
  (message "fill-column set to %d" fill-column))

(defconst comment-multi-line nil
  "*Non-nil means \\[indent-new-comment-line] should continue same comment
on new line, with no new terminator or starter.
This is obsolete because you might as well use \\[newline-and-indent].")

(defun indent-new-comment-line (&optional soft)
  "Break line at point and indent, continuing comment if within one.
This indents the body of the continued comment
under the previous comment line.

This command is intended for styles where you write a comment per line,
starting a new comment (and terminating it if necessary) on each line.
If you want to continue one comment across several lines, use \\[newline-and-indent].

The inserted newline is marked hard if `use-hard-newlines' is true, 
unless optional argument SOFT is non-nil."
  (interactive)
  (let (comcol comstart)
    (skip-chars-backward " \t")
    (delete-region (point)
		   (progn (skip-chars-forward " \t")
			  (point)))
    (if soft (insert-and-inherit ?\n) (newline 1))
    (if (not comment-multi-line)
	(save-excursion
	  (if (and comment-start-skip
		   (let ((opoint (point)))
		     (forward-line -1)
		     (re-search-forward comment-start-skip opoint t)))
	      ;; The old line is a comment.
	      ;; Set WIN to the pos of the comment-start.
	      ;; But if the comment is empty, look at preceding lines
	      ;; to find one that has a nonempty comment.

	      ;; If comment-start-skip contains a \(...\) pair,
	      ;; the real comment delimiter starts at the end of that pair.
	      (let ((win (or (match-end 1) (match-beginning 0))))
		(while (and (eolp) (not (bobp))
			    (let (opoint)
			      (beginning-of-line)
			      (setq opoint (point))
			      (forward-line -1)
			      (re-search-forward comment-start-skip opoint t)))
		  (setq win (or (match-end 1) (match-beginning 0))))
		;; Indent this line like what we found.
		(goto-char win)
		(setq comcol (current-column))
		(setq comstart
		      (buffer-substring (point) (match-end 0)))))))
    (if comcol
	(let ((comment-column comcol)
	      (comment-start comstart)
	      (comment-end comment-end))
	  (and comment-end (not (equal comment-end ""))
;	       (if (not comment-multi-line)
		   (progn
		     (forward-char -1)
		     (insert comment-end)
		     (forward-char 1))
;		 (setq comment-column (+ comment-column (length comment-start))
;		       comment-start "")
;		   )
	       )
	  (if (not (eolp))
	      (setq comment-end ""))
	  (insert-and-inherit ?\n)
	  (forward-char -1)
	  (indent-for-comment)
	  (save-excursion
	    ;; Make sure we delete the newline inserted above.
	    (end-of-line)
	    (delete-char 1)))
      (if (null fill-prefix)
	  (indent-according-to-mode)
	(indent-to-left-margin)
	(insert-and-inherit fill-prefix)))))

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

(defconst overwrite-mode-textual " Ovwrt"
  "The string displayed in the mode line when in overwrite mode.")
(defconst overwrite-mode-binary " Bin Ovwrt"
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

(defvar line-number-mode nil
  "*Non-nil means display line number in mode line.")

(defun line-number-mode (arg)
  "Toggle Line Number mode.
With arg, turn Line Number mode on iff arg is positive.
When Line Number mode is enabled, the line number appears
in the mode line."
  (interactive "P")
  (setq line-number-mode
	(if (null arg) (not line-number-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defvar blink-matching-paren t
  "*Non-nil means show matching open-paren when close-paren is inserted.")

(defconst blink-matching-paren-distance 12000
  "*If non-nil, is maximum distance to search for matching open-paren.")

(defconst blink-matching-delay 1
  "*The number of seconds that `blink-matching-open' will delay at a match.")

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
		 (setq blinkpos (scan-sexps oldpos -1))
	       (error nil)))
	   (and blinkpos (/= (char-syntax (char-after blinkpos))
			     ?\$)
		(setq mismatch
		      (/= (char-after (1- oldpos))
			  (matching-paren (char-after blinkpos)))))
	   (if mismatch (setq blinkpos nil))
	   (if blinkpos
	       (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for blink-matching-delay)
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
  "Signal a  quit  condition.
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
	(buffer-quit-function
	 (funcall buffer-quit-function))
	((not (one-window-p t))
	 (delete-other-windows))))

(define-key global-map "\e\e\e" 'keyboard-escape-quit)

(defun set-variable (var val)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
When using this interactively, supply a Lisp expression for VALUE.
If you want VALUE to be a string, you must surround it with doublequotes.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value."
  (interactive
   (let* ((var (read-variable "Set variable: "))
	  (minibuffer-help-form
	   '(funcall myhelp))
	  (myhelp
	   (function
	    (lambda ()
	      (with-output-to-temp-buffer "*Help*"
		(prin1 var)
		(princ "\nDocumentation:\n")
		(princ (substring (documentation-property var 'variable-documentation)
				  1))
		(if (boundp var)
		    (let ((print-length 20))
		      (princ "\n\nCurrent value: ")
		      (prin1 (symbol-value var))))
		(save-excursion
		  (set-buffer standard-output)
		  (help-mode))
		nil)))))
     (list var
	   (let ((prop (get var 'variable-interactive)))
	     (if prop
		 ;; Use VAR's `variable-interactive' property
		 ;; as an interactive spec for prompting.
		 (call-interactively (list 'lambda '(arg)
					   (list 'interactive prop)
					   'arg))
	       (eval-minibuffer (format "Set %s to value: " var)))))))
  (set var val))

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
    (delete-window (selected-window))
    (if (get-buffer-window buf)
	(select-window (get-buffer-window buf)))))

(defun previous-completion (n)
  "Move to the previous item in the completion list."
  (interactive "p")
  (next-completion (- n)))

(defun next-completion (n)
  "Move to the next item in the completion list.
WIth prefix argument N, move N items (negative N means move backward)."
  (interactive "p")
  (while (and (> n 0) (not (eobp)))
    (let ((prop (get-text-property (point) 'mouse-face)))
      ;; If in a completion, move to the end of it.
      (if prop
	  (goto-char (next-single-property-change (point) 'mouse-face)))
      ;; Move to start of next one.
      (goto-char (next-single-property-change (point) 'mouse-face)))
    (setq n (1- n)))
  (while (and (< n 0) (not (bobp)))
    (let ((prop (get-text-property (1- (point)) 'mouse-face)))
      ;; If in a completion, move to the start of it.
      (if prop
	  (goto-char (previous-single-property-change (point) 'mouse-face)))
      ;; Move to end of the previous completion.
      (goto-char (previous-single-property-change (point) 'mouse-face))
      ;; Move to the start of that one.
      (goto-char (previous-single-property-change (point) 'mouse-face)))
    (setq n (1+ n))))

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
(defun choose-completion-string (choice &optional buffer base-size)
  (let ((buffer (or buffer completion-reference-buffer)))
    ;; If BUFFER is a minibuffer, barf unless it's the currently
    ;; active minibuffer.
    (if (and (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
	     (or (not (active-minibuffer-window))
		 (not (equal buffer
			     (window-buffer (active-minibuffer-window))))))
	(error "Minibuffer is not active for completion")
      ;; Insert the completion into the buffer where completion was requested.
      (set-buffer buffer)
      (if base-size
	  (delete-region (+ base-size (point-min)) (point))
	(choose-completion-delete-max-match choice))
      (insert choice)
      (remove-text-properties (- (point) (length choice)) (point)
			      '(mouse-face nil))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
	(set-window-point window (point)))
      ;; If completing for the minibuffer, exit it with this choice.
      (and (equal buffer (window-buffer (minibuffer-window)))
	   minibuffer-completion-table
	   (exit-minibuffer)))))

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

(defvar completion-fixup-function nil
  "A function to customize how completions are identified in completion lists.
`completion-setup-function' calls this function with no arguments
each time it has found what it thinks is one completion.
Point is at the end of the completion in the completion list buffer.
If this function moves point, it can alter the end of that completion.")

;; This function goes in completion-setup-hook, so that it is called
;; after the text of the completion list buffer is written.

(defun completion-setup-function ()
  (save-excursion
    (let ((mainbuf (current-buffer)))
      (set-buffer standard-output)
      (completion-list-mode)
      (make-local-variable 'completion-reference-buffer)
      (setq completion-reference-buffer mainbuf)
;;; The value 0 is right in most cases, but not for file name completion.
;;; so this has to be turned off.
;;;      (setq completion-base-size 0)
      (goto-char (point-min))
      (if window-system
	  (insert (substitute-command-keys
		   "Click \\[mouse-choose-completion] on a completion to select it.\n")))
      (insert (substitute-command-keys
	       "In this buffer, type \\[choose-completion] to \
select the completion near point.\n\n"))
      (forward-line 1)
      (while (re-search-forward "[^ \t\n]+\\( [^ \t\n]+\\)*" nil t)
	(let ((beg (match-beginning 0))
	      (end (point)))
	  (if completion-fixup-function
	      (funcall completion-fixup-function))
	  (put-text-property beg (point) 'mouse-face 'highlight)
	  (goto-char end))))))

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
  (select-window (get-buffer-window "*Completions*"))
  (goto-char (point-min))
  (search-forward "\n\n")
  (forward-line 1))

;; Support keyboard commands to turn on various modifiers.

;; These functions -- which are not commands -- each add one modifier
;; to the following event.

(defun event-apply-alt-modifier (ignore-prompt)
  (vector (event-apply-modifier (read-event) 'alt 22 "A-")))
(defun event-apply-super-modifier (ignore-prompt)
  (vector (event-apply-modifier (read-event) 'super 23 "s-")))
(defun event-apply-hyper-modifier (ignore-prompt)
  (vector (event-apply-modifier (read-event) 'hyper 24 "H-")))
(defun event-apply-shift-modifier (ignore-prompt)
  (vector (event-apply-modifier (read-event) 'shift 25 "S-")))
(defun event-apply-control-modifier (ignore-prompt)
  (vector (event-apply-modifier (read-event) 'control 26 "C-")))
(defun event-apply-meta-modifier (ignore-prompt)
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
(mapcar
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

;;; simple.el ends here
