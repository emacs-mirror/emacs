;;; simple.el --- basic editing commands for Emacs

;; Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.

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

;;; Code:

(defun open-line (arg)
  "Insert a newline and leave point before it.
If there is a fill prefix, insert the fill prefix on the new line
if the line would have been empty.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (flag (and (null do-fill-prefix) (bolp) (not (bobp)))))
    ;; If this is a simple case, and we are at the beginning of a line,
    ;; actually insert the newline *before* the preceding newline
    ;; instead of after.  That makes better display behavior.
    (if flag
	(progn
	  ;; If undo is enabled, don't let this hack be visible:
	  ;; record the real value of point as the place to move back to
	  ;; if we undo this insert.
	  (if (and buffer-undo-list (not (eq buffer-undo-list t)))
	      (setq buffer-undo-list (cons (point) buffer-undo-list)))
	  (forward-char -1)))
    (while (> arg 0)
      (save-excursion
        (insert ?\n))
      (if do-fill-prefix (insert fill-prefix))
      (setq arg (1- arg)))
    (if flag (forward-char 1))))

(defun split-line ()
  "Split current line, moving portion beyond point vertically down."
  (interactive "*")
  (skip-chars-forward " \t")
  (let ((col (current-column))
	(pos (point)))
    (insert ?\n)
    (indent-to col 0)
    (goto-char pos)))

(defun quoted-insert (arg)
  "Read next input character and insert it.
This is useful for inserting control characters.
You may also type up to 3 octal digits, to insert a character with that code"
  (interactive "*p")
  (let ((char (read-quoted-char)))
    (while (> arg 0)
      (insert char)
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
On nonblank line, delete all blank lines that follow it."
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
column specified by the variable `left-margin'."
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
column specified by the variable `left-margin'."
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
and KILLP is t if prefix arg is was specified."
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
With arg N, put point N/10 of the way from the true beginning.

Don't use this command in Lisp programs!
\(goto-char (point-min)) is faster and avoids clobbering the mark."
  (interactive "P")
  (push-mark)
  (goto-char (if arg
		 (if (> (buffer-size) 10000)
		     ;; Avoid overflow for large buffer sizes!
		     (* (prefix-numeric-value arg)
			(/ (buffer-size) 10))
		   (/ (+ 10 (* (buffer-size) (prefix-numeric-value arg))) 10))
	       (point-min)))
  (if arg (forward-line 1)))

(defun end-of-buffer (&optional arg)
  "Move point to the end of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the true end.

Don't use this command in Lisp programs!
\(goto-char (point-max)) is faster and avoids clobbering the mark."
  (interactive "P")
  (push-mark)
  (goto-char (if arg
		 (- (1+ (buffer-size))
		    (if (> (buffer-size) 10000)
			;; Avoid overflow for large buffer sizes!
			(* (prefix-numeric-value arg)
			   (/ (buffer-size) 10))
		      (/ (* (buffer-size) (prefix-numeric-value arg)) 10)))
	       (point-max)))
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
	(recenter -3))))

(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer.
You probably should not use this function in Lisp programs;
it is usually a mistake for a Lisp function to use any subroutine
that uses or sets the mark."
  (interactive)
  (push-mark (point))
  (push-mark (point-max))
  (goto-char (point-min)))

(defun count-lines-region (start end)
  "Print number of lines and charcters in the region."
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
	  (let ((done 0))
	    (while (re-search-forward "[\n\C-m]" nil t 40)
	      (setq done (+ 40 done)))
	    (while (re-search-forward "[\n\C-m]" nil t 1)
	      (setq done (+ 1 done)))
	    done)
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
	  (message "Char: %s (0%o)  point=%d of %d(%d%%) <%d - %d>  column %d %s"
		   (single-key-description char) char pos total percent beg end col hscroll)
	(message "Char: %s (0%o)  point=%d of %d(%d%%)  column %d %s"
		 (single-key-description char) char pos total percent col hscroll)))))

(defun fundamental-mode ()
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (interactive)
  (kill-all-local-variables))

(defvar read-expression-map (copy-keymap minibuffer-local-map)
  "Minibuffer keymap used for reading Lisp expressions.")
(define-key read-expression-map "\M-\t" 'lisp-complete-symbol)

(put 'eval-expression 'disabled t)

;; We define this, rather than making  eval  interactive,
;; for the sake of completion of names like eval-region, eval-current-buffer.
(defun eval-expression (expression)
  "Evaluate EXPRESSION and print value in minibuffer.
Value is also consed on to front of the variable `values'."
  (interactive (list (read-from-minibuffer "Eval: "
					   nil read-expression-map t)))
  (setq values (cons (eval expression) values))
  (prin1 (car values) t))

(defun edit-and-eval-command (prompt command)
  "Prompting with PROMPT, let user edit COMMAND and eval result.
COMMAND is a Lisp expression.  Let user edit that expression in
the minibuffer, then read and evaluate the result."
  (let ((command (read-from-minibuffer prompt
				       (prin1-to-string command)
				       read-expression-map t)))
    ;; Add edited command to command history, unless redundant.
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
	  (setq newcmd (read-from-minibuffer "Redo: "
					     (prin1-to-string elt)
					     read-expression-map
					     t
					     (cons 'command-history
						   arg)))
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
  "Nonzero when doing history operations on `command-history'.
More generally, indicates that the history list being acted on
contains expressions rather than strings.")
(setq minibuffer-history-variable 'minibuffer-history)
(setq minibuffer-history-position nil)
(defvar minibuffer-history-search-history nil)

(mapcar
 (function (lambda (key-and-command)
	     (mapcar
	      (function (lambda (keymap)
			  (define-key (symbol-value keymap)
			    (car key-and-command)
			    (cdr key-and-command))))
	      '(minibuffer-local-map
		minibuffer-local-ns-map
		minibuffer-local-completion-map
		minibuffer-local-must-match-map
		read-expression-map))))
 '(("\en" . next-history-element) ([next] . next-history-element)
   ("\ep" . previous-history-element) ([prior] . previous-history-element)
   ("\er" . previous-matching-history-element)
   ("\es" . next-matching-history-element)))

(defun previous-matching-history-element (regexp n)
  "Find the previous history element that matches REGEXP.
\(Previous history elements refer to earlier actions.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive
   (let ((enable-recursive-minibuffers t)
	 (minibuffer-history-sexp-flag nil))
     (list (read-from-minibuffer "Previous element matching (regexp): "
				 nil
				 minibuffer-local-map
				 nil
				 'minibuffer-history-search-history)
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
			    (prin1-to-string (nth (1- pos) history))
			  (nth (1- pos) history)))
	  (setq n (+ n (if (< n 0) 1 -1)))))
    (setq minibuffer-history-position pos)
    (erase-buffer)
    (let ((elt (nth (1- pos) history)))
      (insert (if minibuffer-history-sexp-flag
		  (prin1-to-string elt)
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
   (let ((enable-recursive-minibuffers t)
	 (minibuffer-history-sexp-flag nil))
     (list (read-from-minibuffer "Next element matching (regexp): "
				 nil
				 minibuffer-local-map
				 nil
				 'minibuffer-history-search-history)
	   (prefix-numeric-value current-prefix-arg))))
  (previous-matching-history-element regexp (- n)))

(defun next-history-element (n)
  "Insert the next element of the minibuffer history into the minibuffer."
  (interactive "p")
  (let ((narg (min (max 1 (- minibuffer-history-position n))
		   (length (symbol-value minibuffer-history-variable)))))
    (if (= minibuffer-history-position narg)
	(error (if (= minibuffer-history-position 1)
		   "End of history; no next item"
		 "Beginning of history; no preceding item"))
      (erase-buffer)
      (setq minibuffer-history-position narg)
      (let ((elt (nth (1- minibuffer-history-position)
		      (symbol-value minibuffer-history-variable))))
	(insert
	 (if minibuffer-history-sexp-flag
	     (prin1-to-string elt)
	   elt)))
      (goto-char (point-min)))))

(defun previous-history-element (n)
  "Inserts the previous element of the minibuffer history into the minibuffer."
  (interactive "p")
  (next-history-element (- n)))

(defun goto-line (arg)
  "Goto line ARG, counting from line 1 at beginning of buffer."
  (interactive "NGoto line: ")
  (save-restriction
    (widen)
    (goto-char 1)
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- arg))
      (forward-line (1- arg)))))

;Put this on C-x u, so we can force that rather than C-_ into startup msg
(fset 'advertised-undo 'undo)

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  (let ((modified (buffer-modified-p)))
    (or (eq (selected-window) (minibuffer-window))
	(message "Undo!"))
    (or (eq last-command 'undo)
	(progn (undo-start)
	       (undo-more 1)))
    (setq this-command 'undo)
    (undo-more (or arg 1))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary))))

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

(defvar last-shell-command "")
(defvar last-shell-command-on-region "")

(defun shell-command (command &optional flag)
  "Execute string COMMAND in inferior shell; display output, if any.
If COMMAND ends in ampersand, execute it asynchronously.
 
Optional second arg non-nil (prefix arg, if interactive)
means insert output in current buffer after point (leave mark after it).
This cannot be done asynchronously."
  (interactive (list (read-string "Shell command: " last-shell-command)
		     current-prefix-arg))
  (if flag
      (progn (barf-if-buffer-read-only)
	     (push-mark)
	     ;; We do not use -f for csh; we will not support broken use of
	     ;; .cshrcs.  Even the BSD csh manual says to use
	     ;; "if ($?prompt) exit" before things which are not useful
	     ;; non-interactively.  Besides, if someone wants their other
	     ;; aliases for shell commands then they can still have them.
	     (call-process shell-file-name nil t nil
			   "-c" command)
	     (exchange-point-and-mark))
    ;; Preserve the match data in case called from a program.
    (let ((data (match-data)))
      (unwind-protect
	  (if (string-match "[ \t]*&[ \t]*$" command)
	      ;; Command ending with ampersand means asynchronous.
	      (let ((buffer (get-buffer-create "*shell-command*")) 
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
		  (erase-buffer)
		  (display-buffer buffer)
		  (setq default-directory directory)
		  (setq proc (start-process "Shell" buffer 
					    shell-file-name "-c" command))
		  (setq mode-line-process '(": %s"))
		  (set-process-sentinel proc 'shell-command-sentinel)
		  (set-process-filter proc 'shell-command-filter)
		  ))
	    (shell-command-on-region (point) (point) command nil))
	(store-match-data data)))))

;; We have a sentinel to prevent insertion of a termination message
;; in the buffer itself.
(defun shell-command-sentinel (process signal)
  (if (memq (process-status process) '(exit signal))
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
	  (setq opoint (point))
	  (goto-char (point-max))
	  (insert-before-markers string))
      ;; insert-before-markers moved this marker: set it back.
      (set-window-start window pos)
      ;; Finish our save-excursion.
      (goto-char opoint)
      (set-buffer obuf))))

(defun shell-command-on-region (start end command &optional flag interactive)
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.
Noninteractive args are START, END, COMMAND, FLAG.
Noninteractively FLAG means insert output in place of text from START to END,
and put point at the end, but don't alter the mark.

If the output is one line, it is displayed in the echo area,
but it is nonetheless available in buffer `*Shell Command Output*'
even though that buffer is not automatically displayed.  If there is no output
or output is inserted in the current buffer then `*Shell Command Output*' is
deleted." 
  (interactive (list (region-beginning) (region-end)
		     (read-string "Shell command on region: "
				  last-shell-command-on-region)
		     current-prefix-arg
		     (prefix-numeric-value current-prefix-arg)))
  (if flag
      ;; Replace specified region with output from command.
      (let ((swap (and interactive (< (point) (mark)))))
	;; Don't muck with mark
	;; unless called interactively.
	(and interactive (push-mark))
	(call-process-region start end shell-file-name t t nil
			     "-c" command)
	(if (get-buffer "*Shell Command Output*")
	    (kill-buffer "*Shell Command Output*"))
	(and interactive swap (exchange-point-and-mark)))
    ;; No prefix argument: put the output in a temp buffer,
    ;; replacing its entire contents.
    (let ((buffer (get-buffer-create "*Shell Command Output*")))
      (if (eq buffer (current-buffer))
	  ;; If the input is the same buffer as the output,
	  ;; delete everything but the specified region,
	  ;; then replace that region with the output.
	  (progn (delete-region end (point-max))
		 (delete-region (point-min) start)
		 (call-process-region (point-min) (point-max)
				      shell-file-name t t nil
				      "-c" command))
	;; Clear the output buffer, then run the command with output there.
	(save-excursion
	  (set-buffer buffer)
	  (erase-buffer))
	(call-process-region start end shell-file-name
			     nil buffer nil
			     "-c" command))
      ;; Report the amount of output.
      (let ((lines (save-excursion
		     (set-buffer buffer)
		     (if (= (buffer-size) 0)
			 0
		       (count-lines (point-min) (point-max))))))
	(cond ((= lines 0)
	       (message "(Shell command completed with no output)")
	       (kill-buffer "*Shell Command Output*"))
	      ((= lines 1)
	       (message "%s"
			(save-excursion
			  (set-buffer buffer)
			  (goto-char (point-min))
			  (buffer-substring (point)
					    (progn (end-of-line) (point))))))
	      (t 
	       (set-window-start (display-buffer buffer) 1)))))))

(defun universal-argument ()
  "Begin a numeric argument for the following command.
Digits or minus sign following \\[universal-argument] make up the numeric argument.
\\[universal-argument] following the digits or minus sign ends the argument.
\\[universal-argument] without digits or minus sign provides 4 as argument.
Repeating \\[universal-argument] without digits or minus sign
 multiplies the argument by 4 each time."
  (interactive nil)
  (let ((factor 4)
	key)
;;    (describe-arg (list factor) 1)
    (setq key (read-key-sequence nil t))
    (while (equal (key-binding key) 'universal-argument)
      (setq factor (* 4 factor))
;;      (describe-arg (list factor) 1)
      (setq key (read-key-sequence nil t)))
    (prefix-arg-internal key factor nil)))

(defun prefix-arg-internal (key factor value)
  (let ((sign 1))
    (if (and (numberp value) (< value 0))
	(setq sign -1 value (- value)))
    (if (eq value '-)
	(setq sign -1 value nil))
;;    (describe-arg value sign)
    (while (equal key "-")
      (setq sign (- sign) factor nil)
;;      (describe-arg value sign)
      (setq key (read-key-sequence nil t)))
    (while (and (stringp key)
		(= (length key) 1)
		(not (string< key "0"))
		(not (string< "9" key)))
      (setq value (+ (* (if (numberp value) value 0) 10)
		     (- (aref key 0) ?0))
	    factor nil)
;;      (describe-arg value sign)
      (setq key (read-key-sequence nil t)))
    (setq prefix-arg
	  (cond (factor (list factor))
		((numberp value) (* value sign))
		((= sign -1) '-)))
    ;; Calling universal-argument after digits
    ;; terminates the argument but is ignored.
    (if (eq (key-binding key) 'universal-argument)
	(progn
	  (describe-arg value sign)
	  (setq key (read-key-sequence nil t))))
    (if (= (length key) 1)
	;; Make sure self-insert-command finds the proper character;
	;; unread the character and let the command loop process it.
	(setq unread-command-event (aref key 0))
      ;; We can't push back a longer string, so we'll emulate the
      ;; command loop ourselves.
      (command-execute (key-binding key)))))

(defun describe-arg (value sign)
  (cond ((numberp value)
	 (message "Arg: %d" (* value sign)))
	((consp value)
	 (message "Arg: [%d]" (car value)))
	((< sign 0)
	 (message "Arg: -"))))

(defun digit-argument (arg)
  "Part of the numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "P")
  (prefix-arg-internal (char-to-string (logand last-command-char ?\177))
		       nil arg))

(defun negative-argument (arg)
  "Begin a negative numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "P")
  (prefix-arg-internal "-" nil arg))

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

(defun kill-line (&optional arg)
  "Kill the rest of the current line; if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg."
  (interactive "P")
  (kill-region (point)
	       ;; Don't shift point before doing the delete; that way,
	       ;; undo will record the right position of point.
	       (save-excursion
		 (if arg
		     (forward-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (looking-at "[ \t]*$")
		       (forward-line 1)
		     (end-of-line)))
		 (point))))

;;;; Window system cut and paste hooks.

(defvar interprogram-cut-function nil
  "Function to call to make a killed region available to other programs.

Most window systems provide some sort of facility for cutting and
pasting text between the windows of different programs.  On startup,
this variable is set to a function which emacs will call whenever text
is put in the kill ring to make the new kill available to other
programs.

The function takes one argument, TEXT, which is a string containing
the text which should be made available.")

(defvar interprogram-paste-function nil
  "Function to call to get text cut from other programs.

Most window systems provide some sort of facility for cutting and
pasting text between the windows of different programs.  On startup,
this variable is set to a function which emacs will call to obtain
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

(defun kill-new (string)
  "Make STRING the latest kill in the kill ring.
Set the kill-ring-yank pointer to point to it.
If `interprogram-cut-function' is non-nil, apply it to STRING."
  (setq kill-ring (cons string kill-ring))
  (if (> (length kill-ring) kill-ring-max)
      (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
  (setq kill-ring-yank-pointer kill-ring)
  (if interprogram-cut-function
      (funcall interprogram-cut-function string)))

(defun kill-append (string before-p)
  "Append STRING to the end of the latest kill in the kill ring.
If BEFORE-P is non-nil, prepend STRING to the kill.
If `interprogram-cut-function' is set, pass the resulting kill to
it."
  (setcar kill-ring
	  (if before-p
	      (concat string (car kill-ring))
	    (concat (car kill-ring) string)))
  (if interprogram-cut-function
      (funcall interprogram-cut-function (car kill-ring))))

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
      (let* ((length (length kill-ring))
	     (ARGth-kill-element
	      (nthcdr (% (+ n (- length (length kill-ring-yank-pointer)))
			 length)
		      kill-ring)))
	(or do-not-move
	    (setq kill-ring-yank-pointer ARGth-kill-element))
	(car ARGth-kill-element)))))



;;;; Commands for manipulating the kill ring.

(defun kill-region (beg end)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to kill and then yank immediately, use \\[copy-region-as-kill].)

This is the primitive for programs to kill text (as opposed to deleting it).
Supply two arguments, character numbers indicating the stretch of text
 to be killed.
Any command that calls this function is a \"kill command\".
If the previous command was also a kill command,
the text killed this time appends to the text killed last time
to make one entry in the kill ring."
  (interactive "*r")
  (cond
   ;; If the buffer was read-only, we used to just do a
   ;; copy-region-as-kill.  This was never what I wanted - usually I
   ;; was making a mistake and trying to edit a file checked into RCS -
   ;; so I've taken the code out.
   ((not (or (eq buffer-undo-list t)
	     (eq last-command 'kill-region)
	     (eq beg end)))
    ;; Don't let the undo list be truncated before we can even access it.
    (let ((undo-strong-limit (+ (- (max beg end) (min beg end)) 100)))
      (delete-region beg end)
      ;; Take the same string recorded for undo
      ;; and put it in the kill-ring.
      (kill-new (car (car buffer-undo-list)))
      (setq this-command 'kill-region)))
   (t
    (copy-region-as-kill beg end)
    (delete-region beg end))))

(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (buffer-substring beg end) (< end beg))
    (kill-new (buffer-substring beg end)))
  (setq this-command 'kill-region)
  nil)

(defun kill-ring-save (beg end)
  "Save the region as if killed, but don't kill it."
  (interactive "r")
  (copy-region-as-kill beg end)
  (if (interactive-p)
      (save-excursion
	(let ((other-end (if (= (point) beg) end beg)))
	  (if (pos-visible-in-window-p other-end (selected-window))
	      (progn
		(goto-char other-end)
		(sit-for 1))
	    (let* ((killed-text (current-kill 0))
		   (message-len (min (length killed-text) 40)))
	      (if (= (point) beg)
		  ;; Don't say "killed"; that is misleading.
		  (message "Saved text until \"%s\""
			  (substring killed-text (- message-len)))
		(message "Saved text from \"%s\""
			(substring killed-text 0 message-len)))))))))

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
  (let ((before (< (point) (mark))))
    (delete-region (point) (mark))
    (set-mark (point))
    (insert (current-kill arg))
    (if before (exchange-point-and-mark))))

(defun yank (&optional arg)
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.  Put point at end, and set mark at beginning.
With just C-u as argument, same but put point at beginning (and mark at end).
With argument N, reinsert the Nth most recently killed stretch of killed
text.
See also the command \\[yank-pop]."
  (interactive "*P")
  (push-mark (point))
  (insert (current-kill (cond
			 ((listp arg) 0)
			 ((eq arg '-) -1)
			 (t (1- arg)))))
  (if (consp arg)
      (exchange-point-and-mark)))

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
			    (read-buffer "Insert buffer: " (other-buffer) t))))
  (or (bufferp buffer)
      (setq buffer (get-buffer buffer)))
  (let (start end newmark)
    (save-excursion
      (save-excursion
	(set-buffer buffer)
	(setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark)))

(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive
   (list (read-buffer "Append to buffer: " (other-buffer nil t) t)))
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

(defun mark ()
  "Return this buffer's mark value as integer, or nil if no mark.
If you are using this in an editing command, you are most likely making
a mistake; see the documentation of `set-mark'."
  (marker-position (mark-marker)))

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

  (set-marker (mark-marker) pos (current-buffer)))

(defvar mark-ring nil
  "The list of saved former marks of the current buffer,
most recent first.")
(make-variable-buffer-local 'mark-ring)

(defconst mark-ring-max 16
  "*Maximum size of mark ring.  Start discarding off end if gets this big.")

(defun set-mark-command (arg)
  "Set mark at where point is, or jump to mark.
With no prefix argument, set mark, and push old mark position on mark ring.
With argument, jump to mark, and pop a new position for mark off the ring.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (interactive "P")
  (if (null arg)
      (push-mark)
    (if (null (mark))
	(error "No mark set in this buffer")
      (goto-char (mark))
      (pop-mark))))

(defun push-mark (&optional location nomsg)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
Displays \"Mark set\" unless the optional second arg NOMSG is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (if (null (mark))
      nil
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (if (> (length mark-ring) mark-ring-max)
	(progn
	  (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
	  (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil))))
  (set-mark (or location (point)))
  (or nomsg executing-macro (> (minibuffer-depth) 0)
      (message "Mark set"))
  nil)

(defun pop-mark ()
  "Pop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (if mark-ring
      (progn
	(setq mark-ring (nconc mark-ring (list (copy-marker (mark-marker)))))
	(set-mark (+ 0 (car mark-ring)))
	(move-marker (car mark-ring) nil)
	(if (null (mark)) (ding))
	(setq mark-ring (cdr mark-ring)))))

(fset 'exchange-dot-and-mark 'exchange-point-and-mark)
(defun exchange-point-and-mark ()
  "Put the mark where point is now, and point where the mark is now."
  (interactive nil)
  (let ((omark (mark)))
    (if (null omark)
	(error "No mark set in this buffer"))
    (set-mark (point))
    (goto-char omark)
    nil))

(defun next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one,
a newline character is inserted to create a line
and the cursor moves to that line.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.  This goal column is stored
in `goal-column', which is nil when there is none.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (= arg 1)
      (let ((opoint (point)))
	(forward-line 1)
	(if (or (= opoint (point))
		(not (eq (preceding-char) ?\n)))
	    (insert ?\n)
	  (goto-char opoint)
	  (line-move arg)))
    (line-move arg))
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
  (line-move (- arg))
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
  (if (not (integerp selective-display))
      (forward-line arg)
    ;; Move by arg lines, but ignore invisible ones.
    (while (> arg 0)
      (vertical-motion 1)
      (forward-char -1)
      (forward-line 1)
      (setq arg (1- arg)))
    (while (< arg 0)
      (vertical-motion -1)
      (beginning-of-line)
      (setq arg (1+ arg))))
  (move-to-column (or goal-column temporary-goal-column))
  nil)


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
can the value for a particular mode using that mode's hook.")
(make-variable-buffer-local 'comment-column)

(defconst comment-start nil
  "*String to insert to start a new comment, or nil if no comment syntax defined.")

(defconst comment-start-skip nil
  "*Regexp to match the start of a comment plus everything up to its body.
If there are any \\(...\\) pairs, the comment delimiter text is held to begin
at the place matched by the close of the first pair.")

(defconst comment-end ""
  "*String to insert to end a new comment.
Should be an empty string if comments are terminated by end-of-line.")

(defconst comment-indent-hook
  '(lambda () comment-column)
  "Function to compute desired indentation for a comment.
This function is called with no args with point at the beginning of
the comment's starting delimiter.")

(defun indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment."
  (interactive "*")
  (beginning-of-line 1)
  (if (null comment-start)
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
		   ;; If comment-start-skip matched a string with internal
		   ;; whitespace (not final whitespace) then the delimiter
		   ;; start at the end of that whitespace.
		   ;; Otherwise, it starts at the beginning of what was matched.
		   (skip-chars-backward " \t" (match-beginning 0))
		   (skip-chars-backward "^ \t" (match-beginning 0)))))
      (setq begpos (point))
      ;; Compute desired indent.
      (if (= (current-column)
	     (setq indent (funcall comment-indent-hook)))
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
	(insert comment-start)
	(save-excursion
	  (insert comment-end))))))

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
  "Comment the region; third arg numeric means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.
Comments are terminated on each line, even for syntax in which newline does
not end the comment.  Blank lines do not get comments."
  ;; if someone wants it to only put a comment-start at the beginning and
  ;; comment-end at the end then typing it, C-x C-x, closing it, C-x C-x
  ;; is easy enough.  No option is made here for other than commenting
  ;; every line.
  (interactive "r\np")
  (or comment-start (error "No comment syntax is defined"))
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (save-restriction
      (let ((cs comment-start) (ce comment-end))
        (cond ((not arg) (setq arg 1))
              ((> arg 1)
               (while (> (setq arg (1- arg)) 0)
                 (setq cs (concat cs comment-start)
                       ce (concat ce comment-end)))))
        (narrow-to-region beg end)
        (goto-char beg)
        (while (not (eobp))
          (if (< arg 0)
              (let ((count arg))
                (while (and (> 1 (setq count (1+ count)))
                            (looking-at (regexp-quote cs)))
                  (delete-char (length cs)))
                (if (string= "" ce) ()
                  (setq count arg)
                  (while (> 1 (setq count (1+ count)))
                    (end-of-line)
                    ;; this is questionable if comment-end ends in whitespace
                    ;; that is pretty brain-damaged though
                    (skip-chars-backward " \t")
                    (backward-char (length ce))
                    (if (looking-at (regexp-quote ce))
                        (delete-char (length ce)))))
		(forward-line 1))
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
      (point))))

(defun kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (kill-region (point) (save-excursion (forward-word arg) (point))))

(defun backward-kill-word (arg)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (kill-word (- arg)))

(defconst fill-prefix nil
  "*String for filling to insert at front of new line, or nil for none.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'fill-prefix)

(defconst auto-fill-inhibit-regexp nil
  "*Regexp to match lines which should not be auto-filled.")

(defun do-auto-fill ()
  (let (give-up)
    (or (and auto-fill-inhibit-regexp
	     (save-excursion (beginning-of-line)
			     (looking-at auto-fill-inhibit-regexp)))
	(while (and (not give-up) (> (current-column) fill-column))
	  (let ((fill-point
		 (let ((opoint (point)))
		   (save-excursion
		     (move-to-column (1+ fill-column))
		     (skip-chars-backward "^ \t\n")
		     (if (bolp)
			 (re-search-forward "[ \t]" opoint t))
		     (skip-chars-backward " \t")
		     (point)))))
	    ;; If there is a space on the line before fill-point,
	    ;; and nonspaces precede it, break the line there.
	    (if (save-excursion
		  (goto-char fill-point)
		  (not (bolp)))
		;; If point is at the fill-point, do not `save-excursion'.
		;; Otherwise, if a comment prefix or fill-prefix is inserted,
		;; point will end up before it rather than after it.
		(if (save-excursion
		      (skip-chars-backward " \t")
		      (= (point) fill-point))
		    (indent-new-comment-line)
		  (save-excursion
		    (goto-char fill-point)
		    (indent-new-comment-line)))
	      ;; No place to break => stop trying.
	      (setq give-up t)))))))

(defconst comment-multi-line nil
  "*Non-nil means \\[indent-new-comment-line] should continue same comment
on new line, with no new terminator or starter.
This is obsolete because you might as well use \\[newline-and-indent].")

(defun indent-new-comment-line ()
  "Break line at point and indent, continuing comment if presently within one.
The body of the continued comment is indented under the previous comment line.

This command is intended for styles where you write a comment per line,
starting a new comment (and terminating it if necessary) on each line.
If you want to continue one comment across several lines, use \\[newline-and-indent]."
  (interactive "*")
  (let (comcol comstart)
    (skip-chars-backward " \t")
    (delete-region (point)
		   (progn (skip-chars-forward " \t")
			  (point)))
    (insert ?\n)
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
	      (let ((win (match-beginning 0)))
		(while (and (eolp) (not (bobp))
			    (let (opoint)
			      (beginning-of-line)
			      (setq opoint (point))
			      (forward-line -1)
			      (re-search-forward comment-start-skip opoint t)))
		  (setq win (match-beginning 0)))
		;; Indent this line like what we found.
		(goto-char win)
		(setq comcol (current-column))
		(setq comstart (buffer-substring (point) (match-end 0)))))))
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
	  (insert ?\n)
	  (forward-char -1)
	  (indent-for-comment)
	  (save-excursion
	    ;; Make sure we delete the newline inserted above.
	    (end-of-line)
	    (delete-char 1)))
      (if fill-prefix
	  (insert fill-prefix)
	(indent-according-to-mode)))))

(defun auto-fill-mode (&optional arg)
  "Toggle auto-fill mode.
With arg, turn auto-fill mode on if and only if arg is positive.
In auto-fill mode, inserting a space at a column beyond  fill-column
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

(defun turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  (auto-fill-mode 1))

(defun set-fill-column (arg)
  "Set `fill-column' to current column, or to argument if given.
The variable `fill-column' has a separate value for each buffer."
  (interactive "P")
  (setq fill-column (if (integerp arg) arg (current-column)))
  (message "fill-column set to %d" fill-column))

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

(defun overwrite-mode (arg)
  "Toggle overwrite mode.
With arg, turn overwrite mode on iff arg is positive.
In overwrite mode, printing characters typed in replace existing text
on a one-for-one basis, rather than pushing it to the right."
  (interactive "P")
  (setq overwrite-mode
	(if (null arg) (not overwrite-mode)
	  (> (prefix-numeric-value arg) 0)))
  (set-buffer-modified-p (buffer-modified-p))) ;No-op, but updates mode line.

(defvar blink-matching-paren t
  "*Non-nil means show matching open-paren when close-paren is inserted.")

(defconst blink-matching-paren-distance 4000
  "*If non-nil, is maximum distance to search for matching open-paren
when close-paren is inserted.")

(defun blink-matching-open ()
  "Move cursor momentarily to the beginning of the sexp before point."
  (interactive)
  (and (> (point) (1+ (point-min)))
       (/= (char-syntax (char-after (- (point) 2))) ?\\ )
       blink-matching-paren
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
			  (logand (lsh (aref (syntax-table)
					     (char-after blinkpos))
				       -8)
				  255))))
	   (if mismatch (setq blinkpos nil))
	   (if blinkpos
	       (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (goto-char blinkpos)
		  (message
		   "Matches %s"
		   (if (save-excursion
			 (skip-chars-backward " \t")
			 (not (bolp)))
		       (buffer-substring (progn (beginning-of-line) (point))
					 (1+ blinkpos))
		     (buffer-substring blinkpos
				       (progn
					(forward-char 1)
					(skip-chars-forward "\n \t")
					(end-of-line)
					(point)))))))
	     (cond (mismatch
		    (message "Mismatched parentheses"))
		   ((not blink-matching-paren-distance)
		    (message "Unmatched parenthesis"))))))))

;Turned off because it makes dbx bomb out.
(setq blink-paren-function 'blink-matching-open)

; this is just something for the luser to see in a keymap -- this is not
;  how quitting works normally!
(defun keyboard-quit ()
  "Signal a  quit  condition."
  (interactive)
  (signal 'quit nil))

(define-key global-map "\C-g" 'keyboard-quit)

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

;;; simple.el ends here
