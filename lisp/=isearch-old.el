;; Incremental search
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;###autoload
(defvar search-last-string "" "\
Last string search for by a non-regexp search command.
This does not include direct calls to the primitive search functions,
and does not include searches that are aborted.")

;;;###autoload
(defvar search-last-regexp "" "\
Last string searched for by a regexp search command.
This does not include direct calls to the primitive search functions,
and does not include searches that are aborted.")


;;;###autoload
(defconst search-repeat-char ?\C-s "\
*Character to repeat incremental search forwards.")
;;;###autoload
(defconst search-reverse-char ?\C-r "\
*Character to repeat incremental search backwards.")
;;;###autoload
(defconst search-exit-char ?\C-m "\
*Character to exit incremental search.")
;;;###autoload
(defconst search-delete-char ?\177 "\
*Character to delete from incremental search string.")
;;;###autoload
(defconst search-quote-char ?\C-q "\
*Character to quote special characters for incremental search.")
;;;###autoload
(defconst search-yank-word-char ?\C-w "\
*Character to pull next word from buffer into search string.")
;;;###autoload
(defconst search-yank-line-char ?\C-y "\
*Character to pull rest of line from buffer into search string.")
;;;###autoload
(defconst search-ring-advance-char ?\M-n "\
*Character to pull next (more recent) search string from the ring of same.")
;;;###autoload
(defconst search-ring-retreat-char ?\M-p "\
*Character to pull previous (older) search string from the ring of same.")

;;;###autoload
(defconst search-exit-option t "\
*Non-nil means random control characters terminate incremental search.")

;;;###autoload
(defvar search-slow-window-lines 1 "\
*Number of lines in slow search display windows.
These are the short windows used during incremental search on slow terminals.
Negative means put the slow search window at the top (normally it's at bottom)
and the value is minus the number of lines.")

;;;###autoload
(defvar search-slow-speed 1200 "\
*Highest terminal speed at which to use \"slow\" style incremental search.
This is the style where a one-line window is created to show the line
that the search has reached.")

(defconst search-upper-case t
  "*Non-nil means an upper-case letter as search input means case-sensitive.
Any upper-case letter given explicitly as input to the incremental search
has the effect of turning off `case-fold-search' for the rest of this search.
Deleting the letter from the search string cancels the effect.")

(fset 'search-forward-regexp 're-search-forward)
(fset 'search-backward-regexp 're-search-backward)

(defvar search-ring nil
  "List of recent non-regexp incremental searches.
Each element is a cons cell of the form (STRING . UPPERCASE-FLAG).")

(defvar regexp-search-ring nil
  "List of recent regexp incremental searches.
Each element is a cons cell of the form (STRING . UPPERCASE-FLAG).")

(defconst search-ring-max 16
  "*Maximum length of search ring before oldest elements are thrown away.")

(defvar search-ring-yank-pointer nil
  "The tail of the search ring whose car is the last thing searched for.")

(defvar regexp-search-ring-yank-pointer nil
  "The tail of the regular expression search ring whose car is the last
thing searched for.")


;;;###autoload
(defun isearch-forward ()
  "Do incremental search forward.
As you type characters, they add to the search string and are found.
Type Delete to cancel characters from end of search string.
Type RET to exit, leaving point at location found.
Type C-s to search again forward, C-r to search again backward.
Type C-w to yank word from buffer onto end of search string and search for it.
Type C-y to yank rest of line onto end of search string, etc.
Type C-q to quote control character to search for it.
Other control and meta characters terminate the search
 and are then executed normally.
The above special characters are mostly controlled by parameters;
 do M-x apropos on search-.*-char to find them.
C-g while searching or when search has failed
 cancels input back to what has been found successfully.
C-g when search is successful aborts and moves point to starting point."
  (interactive)
  (isearch t))
;;;###autoload
(define-key global-map "\C-s" 'isearch-forward)

;;;###autoload
(defun isearch-forward-regexp ()
  "Do incremental search forward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  (interactive)
  (isearch t t))
;;;###autoload
(define-key esc-map "\C-s" 'isearch-forward-regexp)

;;;###autoload
(defun isearch-backward ()
  "Do incremental search backward.
See \\[isearch-forward] for more information."
  (interactive)
  (isearch nil))
;;;###autoload
(define-key global-map "\C-r" 'isearch-backward)

;;;###autoload
(defun isearch-backward-regexp ()
  "Do incremental search backward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  (interactive)
  (isearch nil t))
;;;###autoload
(define-key esc-map "\C-r" 'isearch-backward-regexp)


;; This function does all the work of incremental search.
;; The functions attached to ^R and ^S are trivial,
;; merely calling this one, but they are always loaded by default
;; whereas this file can optionally be autoloadable.
;; This is the only entry point in this file.

;; OP-FUN is a function to be called after each input character is processed.
;; (It is not called after characters that exit the search.)

(defun isearch (forward &optional regexp op-fun)
  (let ((search-string "")
	(search-message "")
	;; List of previous states during this search.
	(history nil)
	;; t means search is currently successful.
	(success t)
	;; Set once the search has wrapped around the end of the buffer.
	(wrapped nil)
	;; Nominal starting point for searching
	;; Usually this is the same as the opoint,
	;; but it is changed by wrapping
	;; and also by repeating the search.
	(barrier (point))
	;; Set temporarily when adding a character to a regexp
	;; enables it to match more rather than fewer places in the buffer.
	liberalized
	;; Set temporarily by yanking text into the search string.
	yank-flag
	(invalid-regexp nil)
	;; non-nil means an explicit uppercase letter seen in the input
	(uppercase-flag nil)
	;; Non-nil means start using a small window
	;; if the search moves outside what is currently on the screen.
	(slow-terminal-mode (and (<= baud-rate search-slow-speed)
				 (> (window-height)
				    (* 4 search-slow-window-lines))))
	;; t means a small window is currently in use.
	(small-window nil)		;if t, using a small window
	;; These variables preserve information from the small window
	;; through exit from the save-window-excursion.
	(found-point nil)
	(found-start nil)
	;; Point is at one end of the last match.
	;; This variable records the other end of that match.
	(other-end nil)
	;; Value of point at start of search,
	;; for moving the cursor back on quitting.
	(opoint (point))
	(inhibit-quit t)  ;Prevent ^G from quitting, so we can read it.
	;; The screen we're working on; if this changes, we exit isearch.
	(screen (if (fboundp 'selected-screen) (selected-screen))))
	   
    (isearch-push-state)
    (save-window-excursion
     (catch 'search-done
       (while t
	 (or (and (numberp unread-command-char) (>= unread-command-char 0))
	     (progn
	       (or (input-pending-p)
		   (isearch-message))
	       (if (and slow-terminal-mode
			(not (or small-window (pos-visible-in-window-p))))
		   (progn
		     (setq small-window t)
		     (setq found-point (point))
		     (move-to-window-line 0)
		     (let ((window-min-height 1))
		       (split-window nil (if (< search-slow-window-lines 0)
					     (1+ (- search-slow-window-lines))
					   (- (window-height)
					      (1+ search-slow-window-lines)))))
		     (if (< search-slow-window-lines 0)
			 (progn (vertical-motion (- 1 search-slow-window-lines))
				(set-window-start (next-window) (point))
				(set-window-hscroll (next-window)
						    (window-hscroll))
				(set-window-hscroll (selected-window) 0))
		       (other-window 1))
		     (goto-char found-point)))))
	 (let ((char (if quit-flag
			 ?\C-g
		       (read-event))))
	   (setq quit-flag nil liberalized nil yank-flag nil)
	   (cond ((and (or (not (integerp char))
			   (and (>= char 128)
				(not (= char search-ring-advance-char))
				(not (= char search-ring-retreat-char))))
		       search-exit-option)
		  (setq unread-command-char char)
		  (throw 'search-done t))

		 ;; If the user switches to a different screen, exit.
		 ((not (eq screen last-event-screen))
		  (setq unread-command-char char)
		  (throw 'search-done t))

		 ((eq char search-exit-char)
		  ;; RET means exit search normally.
		  ;; Except, if first thing typed, it means do nonincremental
		  (if (= 0 (length search-string))
		      (nonincremental-search forward regexp))
		  (throw 'search-done t))
		 ((= char ?\C-g)
		  ;; ^G means the user tried to quit.
		  (ding)
		  (discard-input)
		  (if success
		      ;; If search is successful, move back to starting point
		      ;; and really do quit.
		      (progn (goto-char opoint)
			     (signal 'quit nil))
		    ;; If search is failing, rub out until it is once more
		    ;;  successful.
		    (while (not success) (isearch-pop))))
		 ((or (eq char search-repeat-char)
		      (eq char search-reverse-char))
		  (if (eq forward (eq char search-repeat-char))
		      ;; C-s in forward or C-r in reverse.
		      (if (equal search-string "")
			  ;; If search string is empty, use last one.
			  (isearch-get-string-from-ring)
			;; If already have what to search for, repeat it.
			(or success
			    (progn (goto-char (if forward (point-min) (point-max)))
				   (setq wrapped t))))
		    ;; C-s in reverse or C-r in forward, change direction.
		    (setq forward (not forward)))
		  (setq barrier (point)) ; For subsequent \| if regexp.
		  (setq success t)
		  (or (equal search-string "")
		      (progn
			;; If repeating a search that found an empty string,
			;; ensure we advance.  Test history to make sure we
			;; actually have done a search already; otherwise,
			;; the match data will be random.
			(if (and (cdr history)
				 (= (match-end 0) (match-beginning 0)))
			    (forward-char (if forward 1 -1)))
			(isearch-search)))
		  (isearch-push-state))
		 ((= char search-delete-char)
		  ;; Rubout means discard last input item and move point
		  ;; back.  If buffer is empty, just beep.
		  (if (null (cdr history))
		      (ding)
		    (isearch-pop)))
		 ((= char search-ring-advance-char)
		  (isearch-pop)
		  (if regexp
		      (let ((length (length regexp-search-ring)))
			(if (zerop length)
			    ()
			  (setq regexp-search-ring-yank-pointer
				(nthcdr (% (+ 1 (- length (length regexp-search-ring-yank-pointer)))
					   length)
					regexp-search-ring))
			  (isearch-get-string-from-ring)))
		    (let ((length (length search-ring)))
			(if (zerop length)
			    ()
			  (setq search-ring-yank-pointer
				(nthcdr (% (+ 1 (- length (length search-ring-yank-pointer)))
					   length)
					search-ring))
			  (isearch-get-string-from-ring))))
		  (isearch-push-state)
		  (isearch-search))
		 ((= char search-ring-retreat-char)
		  (isearch-pop)
		  (if regexp
		      (let ((length (length regexp-search-ring)))
			(if (zerop length)
			    ()
			  (setq regexp-search-ring-yank-pointer
				(nthcdr (% (+ (- length (length regexp-search-ring-yank-pointer))
					      (1- length))
					   length)
					regexp-search-ring))
			  (isearch-get-string-from-ring)))
		    (let ((length (length search-ring)))
			(if (zerop length)
			    ()
			  (setq search-ring-yank-pointer
				(nthcdr (% (+ (- length (length search-ring-yank-pointer))
					      (1- length))
					   length)
					search-ring))
			  (isearch-get-string-from-ring))))
		  (isearch-push-state)
		  (isearch-search))
		 (t
		  (cond ((or (eq char search-yank-word-char)
			     (eq char search-yank-line-char))
			 ;; ^W means gobble next word from buffer.
			 ;; ^Y means gobble rest of line from buffer.
			 (let ((word (save-excursion
				       (and (not forward) other-end
					    (goto-char other-end))
				       (buffer-substring
					(point)
					(save-excursion
					  (if (eq char search-yank-line-char)
					      (end-of-line)
					    (forward-word 1))
					  (point))))))
			   (if regexp
			       (setq word (regexp-quote word)))
			   (setq search-string (concat search-string word)
				 search-message
				 (concat search-message
					 (mapconcat 'text-char-description
						    word ""))
				 ;; Don't move cursor in reverse search.
				 yank-flag t)))
			 ;; Any other control char =>
			 ;;  unread it and exit the search normally.
			 ((and search-exit-option
			       (/= char search-quote-char)
			       (or (>= char ?\177)
				   (and (< char ? )
					(/= char ?\t)
					(/= char ?\n))))
			  (setq unread-command-char char)
			  (throw 'search-done t))
			 (t
			  ;; Any other character => add it to the
			  ;;  search string and search.
			  (cond ((= char search-quote-char)
				 (setq char (read-quoted-char
					     (isearch-message t))))
				((= char ?\r)
				 ;; RET translates to newline.
				 (setq char ?\n)))
			  (setq search-string (concat search-string
						      (char-to-string char))
				search-message (concat search-message
						       (text-char-description char))
				uppercase-flag (or uppercase-flag
						   (not (= char (downcase char)))))))
		  (if (and (not success)
			   ;; unsuccessful regexp search may become
			   ;;  successful by addition of characters which
			   ;;  make search-string valid
			   (not regexp))
		      nil
		    ;; Check for chars that can make a regexp more liberal.
		    ;; They can make a regexp match sooner
		    ;; or make it succeed instead of failing.
		    ;; So go back to place last successful search started
		    ;; or to the last ^S/^R (barrier), whichever is nearer.
		    (and regexp history
			 (cond ((and (memq char '(?* ??))
				     ;; Don't treat *, ? as special
				     ;; within [] or after \.
				     (not (nth 6 (car history))))
				(setq liberalized t)
				;; This used to use element 2
				;; in a reverse search, but it seems that 5
				;; (which is the end of the old match)
				;; is better in that case too.
				(let ((cs (nth 5 ; old other-end.
					       (car (cdr history)))))
				  ;; (car history) is after last search;
				  ;; (car (cdr history)) is from before it.
				  (setq cs (or cs barrier))
				  (goto-char
				   (if forward
				       (max cs barrier)
				     (min cs barrier)))))
			       ((eq char ?\|)
				(setq liberalized t)
				(goto-char barrier))))
		    ;; Turn off case-sensitivity if string requests it.
		    (let ((case-fold-search
			   (and case-fold-search
				(not (and uppercase-flag
					  search-upper-case)))))
		      ;; In reverse search, adding stuff at
		      ;; the end may cause zero or many more chars to be
		      ;; matched, in the string following point.
		      ;; Allow all those possibilities without moving point as
		      ;; long as the match does not extend past search origin.
		      (if (and (not forward) (not liberalized)
			       (condition-case ()
				   (looking-at (if regexp search-string
						 (regexp-quote search-string)))
				 (error nil))
			       (or yank-flag
				   ;; Used to have (min opoint barrier)
				   ;; instead of barrier.
				   ;; This lost when wrapping.
				   (<= (match-end 0) barrier)))
			  (setq success t invalid-regexp nil
				other-end (match-end 0))
			;; Not regexp, not reverse, or no match at point.
			(if (and other-end (not liberalized))
			    (goto-char (if forward other-end
					 ;; Used to have opoint inside the min.
					 ;; This lost when wrapping.
					 (min barrier (1+ other-end)))))
			(isearch-search))))
		  (isearch-push-state))))
	 (if op-fun (funcall op-fun))))
     (setq found-start (window-start (selected-window)))
     (setq found-point (point)))
    (if (> (length search-string) 0)
	(if (and regexp (not (member search-string regexp-search-ring)))
	    (progn
	      (setq regexp-search-ring (cons (cons search-string uppercase-flag)
					    regexp-search-ring)
		    regexp-search-ring-yank-pointer regexp-search-ring)
	      (if (> (length regexp-search-ring) regexp-search-ring-max)
		  (setcdr (nthcdr (1- search-ring-max) regexp-search-ring) nil)))
	  (if (not (member search-string search-ring))
	      (progn
		(setq search-ring (cons (cons search-string uppercase-flag)
					search-ring)
		      search-ring-yank-pointer search-ring)
		(if (> (length search-ring) search-ring-max)
		    (setcdr (nthcdr (1- search-ring-max) search-ring) nil))))))
    ;; If we displayed a single-line window, set point in this window. 
    (if small-window
	(goto-char found-point))
    ;; If there was movement, mark the starting position.
    ;; Maybe should test difference between and set mark iff > threshold.
    (if (/= (point) opoint)
	(push-mark opoint)
      (message ""))
    (or small-window
	;; Exiting the save-window-excursion clobbers this; restore it.
	(set-window-start (selected-window) found-start t))))

(defun isearch-message (&optional c-q-hack ellipsis)
  ;; If about to search, and previous search regexp was invalid,
  ;; check that it still is.  If it is valid now,
  ;; let the message we display while searching say that it is valid.
  (and invalid-regexp ellipsis
       (condition-case ()
	   (progn (re-search-forward search-string (point) t)
		  (setq invalid-regexp nil))
	 (error nil)))
  ;; If currently failing, display no ellipsis.
  (or success (setq ellipsis nil))
  (let ((m (concat (if success "" "failing ")
		   (if wrapped "wrapped ")
		   (if (or (not case-fold-search)
			   (and uppercase-flag search-upper-case))
		       "case-sensitive ")
		   (if regexp "regexp " "")
		   "I-search"
		   (if forward ": " " backward: ")
		   search-message
		   (if c-q-hack "^Q" "")
		   (if invalid-regexp
		       (concat " [" invalid-regexp "]")
		     ""))))
    (aset m 0 (upcase (aref m 0)))
    (let ((cursor-in-echo-area ellipsis))
      (if c-q-hack m (message "%s" m)))))

;; Get the search string from the "front" of the ring of previous searches.
(defun isearch-get-string-from-ring ()
  (let ((elt (car (if regexp
		      (or regexp-search-ring-yank-pointer regexp-search-ring)
		    (or search-ring-yank-pointer search-ring)))))
    ;; ELT describes the most recent search or where we have rotated the ring.
    (if elt
	(setq search-string (car elt)
	      uppercase-flag (cdr elt))
      (setq search-string "" uppercase-flag nil)))
  ;; Let's give this one the benefit of the doubt.
  (setq invalid-regexp nil)
  (setq search-message (mapconcat 'text-char-description search-string "")))

(defun isearch-pop ()
  (setq history (cdr history))
  (let ((cmd (car history)))
    (setq search-string (car cmd)
	  search-message (car (cdr cmd))
	  success (nth 3 cmd)
	  forward (nth 4 cmd)
	  other-end (nth 5 cmd)
	  invalid-regexp (nth 6 cmd)
	  wrapped (nth 7 cmd)
	  barrier (nth 8 cmd)
	  uppercase-flag (nth 9 cmd))
    (goto-char (car (cdr (cdr cmd))))))

(defun isearch-push-state ()
  (setq history (cons (list search-string search-message (point)
			    success forward other-end invalid-regexp
			    wrapped barrier uppercase-flag)
		      history)))

(defun isearch-search ()
  (let ((case-fold-search
	 (and case-fold-search
	      (not (and uppercase-flag
			search-upper-case)))))
    (isearch-message nil t)
    (condition-case lossage
	(let ((inhibit-quit nil))
	  (if regexp (setq invalid-regexp nil))
	  (setq success
		(funcall
		 (if regexp
		     (if forward 're-search-forward 're-search-backward)
		   (if forward 'search-forward 'search-backward))
		 search-string nil t))
	  (if success
	      (setq other-end
		    (if forward (match-beginning 0) (match-end 0)))))
      (quit (setq unread-command-char ?\C-g)
	    (setq success nil))
      (invalid-regexp (setq invalid-regexp (car (cdr lossage)))
		      (if (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
					invalid-regexp)
			  (setq invalid-regexp "incomplete input"))))
    (if success
	nil
      ;; Ding if failed this time after succeeding last time.
      (and (nth 3 (car history))
	   (ding))
      (goto-char (nth 2 (car history))))))

;; This is called from incremental-search
;; if the first input character is the exit character.
;; The interactive-arg-reader uses free variables `forward' and `regexp'
;; which are bound by `incremental-search'.

;; We store the search string in `search-string'
;; which has been bound already by `incremental-search'
;; so that, when we exit, it is copied into `search-last-string'.

(defun nonincremental-search (forward regexp)
  (let (message char function string inhibit-quit)
    (let ((cursor-in-echo-area t))
      ;; Prompt assuming not word search,
      (setq message (if regexp 
			(if forward "Regexp search: "
			  "Regexp search backward: ")
		      (if forward "Search: " "Search backward: ")))
      (message "%s" message)
      ;; Read 1 char and switch to word search if it is ^W.
      (setq char (read-event)))
    (if (and (numberp char) (eq char search-yank-word-char))
	(setq message (if forward "Word search: " "Word search backward: "))
      ;; Otherwise let that 1 char be part of the search string.
      (setq unread-command-char char))
    (setq function
	  (if (eq char search-yank-word-char)
	      (if forward 'word-search-forward 'word-search-backward)
	    (if regexp
		(if forward 're-search-forward 're-search-backward)
	      (if forward 'search-forward 'search-backward))))
    ;; Read the search string with corrected prompt.
    (setq string (read-string message))
    ;; Empty means use default.
    (if (= 0 (length string))
	(setq string search-last-string)
      ;; Set last search string now so it is set even if we fail.
      (setq search-last-string string))
    ;; Since we used the minibuffer, we should be available for redo.
    (setq command-history (cons (list function string) command-history))
    ;; Go ahead and search.
    (funcall function string)))
