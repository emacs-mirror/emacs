;;; viper-mous.el -- Mouse support for Viper

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


(require 'viper-util)


;;; Variables
  
;; Variable used for catching the switch-frame event.
;; If non-nil, indicates that previous-frame should be the selected
;; one. Used by vip-mouse-click-get-word. Not a user option.
(defvar vip-frame-of-focus nil)
    
;; Frame that was selected before the switch-frame event.
(defconst vip-pre-click-frame (vip-selected-frame))
  
(defvar vip-surrounding-word-function 'vip-surrounding-word
  "*Function that determines what constitutes a word for clicking events.
Takes two parameters: a COUNT, indicating how many words to return, 
and CLICK-COUNT, telling whether this is the first click, a double-click,
or a tripple-click.")
       
;; time interval in millisecond within which successive clicks are
;; considered related
(defconst vip-multiclick-timeout (if vip-xemacs-p
				   500
				 double-click-time)
  "*Time interval in millisecond within which successive clicks are
considered related.")

;; current event click count; XEmacs only
(defvar vip-current-click-count 0)
;; time stamp of the last click event; XEmacs only
(defvar vip-last-click-event-timestamp 0)

;; Local variable used to toggle wraparound search on click.
(vip-deflocalvar  vip-mouse-click-search-noerror t)
	
;; Local variable used to delimit search after wraparound.
(vip-deflocalvar  vip-mouse-click-search-limit nil)
	
;; remembers prefix argument to pass along to commands invoked by second
;; click.
;; This is needed because in Emacs (not XEmacs), assigning to preix-arg
;; causes Emacs to count the second click as if it was a single click
(defvar vip-global-prefix-argument nil)



;;; Code

(defun vip-multiclick-p ()
  (not (vip-sit-for-short vip-multiclick-timeout t)))
	     
(defun vip-surrounding-word (count click-count)
   "Returns word surrounding point according to a heuristic.
COUNT indicates how many regions to return.
If CLICK-COUNT is 1, `word' is a word in Vi sense. If it is > 1,
then `word' is a Word in Vi sense.
If the character clicked on is a non-separator and is non-alphanumeric but
is adjacent to an alphanumeric symbol, then it is considered alphanumeric
for the purpose of this command. If this character has a matching
character, such as `\(' is a match for `\)', then the matching character is
also considered alphanumeric.
For convenience, in Lisp modes, `-' is considered alphanumeric."
   (let* ((basic-alpha "_a-zA-Z0-9") ;; it is important for `_' to come first
	  (basic-alpha-B "[_a-zA-Z0-9]")
	  (basic-nonalphasep-B vip-NONALPHASEP-B)
	  (end-modifiers "")
	  (start-modifiers "")
	  vip-ALPHA vip-ALPHA-B
	  vip-NONALPHA vip-NONALPHA-B
	  vip-ALPHASEP vip-ALPHASEP-B
	  vip-NONALPHASEP vip-NONALPHASEP-B
	  skip-flag
	  one-char-word-func word-function-forw word-function-back word-beg)
	  
     (if (and (looking-at basic-nonalphasep-B)
	      (or (save-excursion (vip-backward-char-carefully)
				  (looking-at basic-alpha-B))
		  (save-excursion (vip-forward-char-carefully)
				  (looking-at basic-alpha-B))))
	 (setq start-modifiers
	       (cond ((looking-at "\\\\") "\\\\")
		     ((looking-at "-") "")
		     ((looking-at "[][]") "][")
		     ((looking-at "[()]") ")(")
		     ((looking-at "[{}]") "{}")
		     ((looking-at "[<>]") "<>")
		     ((looking-at "[`']") "`'")
		     ((looking-at "\\^") "")
		     ((looking-at vip-SEP-B) "")
		     (t (char-to-string (following-char))))
	       end-modifiers
	       (cond ((looking-at "-") "C-C-") ;; note the C-C trick
		     ((looking-at "\\^") "^")
		     (t ""))))
		     
     ;; Add `-' to alphanum, if it wasn't added and in we are in Lisp
     (or (looking-at "-")
	 (not (string-match "lisp" (symbol-name major-mode)))
	 (setq end-modifiers (concat end-modifiers "C-C-")))
		 
     (setq vip-ALPHA
	   (format "%s%s%s" start-modifiers basic-alpha end-modifiers)
	   vip-ALPHA-B
	   (format "[%s%s%s]" start-modifiers basic-alpha end-modifiers)
	   vip-NONALPHA (concat "^" vip-ALPHA)
	   vip-NONALPHA-B (concat "[" vip-NONALPHA "]")
	   vip-ALPHASEP (concat vip-ALPHA vip-SEP)
	   vip-ALPHASEP-B
	   (format "[%s%s%s%s]"
		   start-modifiers basic-alpha vip-SEP end-modifiers)
	   vip-NONALPHASEP (format "^%s%s" vip-SEP vip-ALPHA)
	   vip-NONALPHASEP-B (format "[^%s%s]" vip-SEP vip-ALPHA)
	   )
	  
     (if (> click-count 1)
	 (setq one-char-word-func 'vip-one-char-Word-p
	       word-function-forw 'vip-end-of-Word
	       word-function-back 'vip-backward-Word)
       (setq one-char-word-func 'vip-one-char-word-p
	     word-function-forw 'vip-end-of-word
	     word-function-back 'vip-backward-word))
     
     (save-excursion
       (cond ((> click-count 1) (skip-chars-backward vip-NONSEP))
	     ((looking-at vip-ALPHA-B) (skip-chars-backward vip-ALPHA))
	     ((looking-at vip-NONALPHASEP-B)
	      (skip-chars-backward vip-NONALPHASEP))
	     (t (funcall word-function-back 1)))
     
       (setq word-beg (point))
       
       (setq skip-flag t)
       (while (> count 0)
	 ;; skip-flag and the test for 1-char word takes care of the
	 ;; special treatment that vip-end-of-word gives to 1-character
	 ;; words. Otherwise, clicking once on such a word will insert two
	 ;; words.
	 (if (and skip-flag (funcall one-char-word-func))
	     (setq skip-flag (not skip-flag))
	   (funcall word-function-forw 1))
	 (setq count (1- count)))
	 
       (vip-forward-char-carefully)
       (buffer-substring word-beg (point)))
       ))


(defun vip-mouse-click-get-word (click &optional count click-count)
  "Returns word surrounding the position of a mouse click.
Click may be in another window. Current window and buffer isn't changed."
     
  (let ((click-word "")
	(click-pos (vip-mouse-click-posn click))
	(click-buf (vip-mouse-click-window-buffer click)))
    (or (numberp count) (setq count 1))
    (or (numberp click-count) (setq click-count 1))
     
    (save-excursion
      (save-window-excursion
	(if click-pos
	    (progn
	      (set-buffer click-buf)
	
	      (goto-char click-pos)
	      (setq click-word
		    (funcall vip-surrounding-word-function count click-count)))
	  (error "Click must be over a window."))
	click-word))))

(defun vip-mouse-click-frame (click)
  "Returns window where click occurs."
  (vip-window-frame (vip-mouse-click-window click)))

(defun vip-mouse-click-window (click)
  "Returns window where click occurs."
  (if vip-xemacs-p
      (event-window click)
    (posn-window (event-start click))))

(defun vip-mouse-click-window-buffer (click)
  "Returns the buffer of the window where click occurs."
  (window-buffer (vip-mouse-click-window click)))

(defun vip-mouse-click-window-buffer-name (click)
  "Returns the name of the buffer in the window where click occurs."
  (buffer-name (vip-mouse-click-window-buffer click)))

(defun vip-mouse-click-posn (click)
  "Returns position of a click."
  (interactive "e")
  (if vip-xemacs-p
      (event-point click)
    (posn-point (event-start click))))

(defun vip-mouse-click-insert-word (click arg)
  "Insert word clicked or double-clicked on.
With prefix argument, N, insert that many words.
This command must be bound to a mouse click.
The double-click action of the same mouse button must not be bound
\(or it must be bound to the same function\).
See `vip-surrounding-word' for the definition of a word in this case."
  (interactive "e\nP")
  (if vip-frame-of-focus	;; to handle clicks in another frame
      (vip-select-frame vip-frame-of-focus))
      
  ;; turn arg into a number
  (cond ((numberp arg) nil)
	;; prefix arg is a list when one hits C-u then command
	((and (listp arg) (numberp (car arg)))
	 (setq arg (car arg)))
	(t (setq arg 1)))
      
  (let (click-count interrupting-event)
    (if (and
	 (vip-multiclick-p)
	 ;; This trick checks if there is a pending mouse event
	 ;; if so, we use this latter event and discard the current mouse click
	 ;; If the next panding event is not a mouse event, we execute
	 ;; the current mouse event
	 (progn
	   (setq interrupting-event (vip-read-event))
	   (vip-mouse-event-p last-input-event)))
	(progn ;; interrupted wait
	  (setq vip-global-prefix-argument arg)
	  ;; count this click for XEmacs
	  (vip-event-click-count click))
      ;; uninterrupted wait or the interrupting event wasn't a mouse event
      (setq click-count (vip-event-click-count click))
      (if (> click-count 1)
	  (setq arg vip-global-prefix-argument
		vip-global-prefix-argument nil))
      (insert (vip-mouse-click-get-word click arg click-count))
      (if (and interrupting-event
	       (eventp interrupting-event)
	       (not (vip-mouse-event-p interrupting-event)))
	  (vip-set-unread-command-events interrupting-event))
      )))
  
;; arg is an event. accepts symbols and numbers, too
(defun vip-mouse-event-p (event)
  (if (eventp event)
      (string-match "\\(mouse-\\|frame\\|screen\\|track\\)"
		    (prin1-to-string (vip-event-key event)))))
  
;; XEmacs has no double-click events. So, we must simulate.
;; So, we have to simulate event-click-count.
(defun vip-event-click-count (click)
  (if vip-xemacs-p
      (progn
	;; if more than 1 second
	(if (> (- (event-timestamp click) vip-last-click-event-timestamp)
	       vip-multiclick-timeout)
	    (setq vip-current-click-count 0))
	(setq vip-last-click-event-timestamp (event-timestamp click)
	      vip-current-click-count (1+ vip-current-click-count)))
    (event-click-count click)))
    


(defun vip-mouse-click-search-word (click arg)
   "Find the word clicked or double-clicked on. Word may be in another window.
With prefix argument, N, search for N-th occurrence.
This command must be bound to a mouse click. The double-click action of the
same button must not be bound \(or it must be bound to the same function\).
See `vip-surrounding-word' for the details on what constitutes a word for
this command."
  (interactive "e\nP")
  (if vip-frame-of-focus	;; to handle clicks in another frame
      (vip-select-frame vip-frame-of-focus))
  (let (click-word click-count
	(previous-search-string vip-s-string))
    
    (if (and
	 (vip-multiclick-p)
	 ;; This trick checks if there is a pending mouse event
	 ;; if so, we use this latter event and discard the current mouse click
	 ;; If the next panding event is not a mouse event, we execute
	 ;; the current mouse event
	 (progn
	   (vip-read-event)
	   (vip-mouse-event-p last-input-event)))
	(progn ;; interrupted wait
	  (setq vip-global-prefix-argument arg)
	  ;; remember command that was before the multiclick
	  (setq this-command last-command)
	  ;; make sure we counted this event---needed for XEmacs only
	  (vip-event-click-count click))
      ;; uninterrupted wait
      (setq click-count (vip-event-click-count click))
      (setq click-word (vip-mouse-click-get-word click nil click-count))
    
      (if (> click-count 1)
	  (setq arg vip-global-prefix-argument
		vip-global-prefix-argument nil))
      (setq arg (or arg 1))
    
      (vip-deactivate-mark)
      (if (or (not (string= click-word vip-s-string))
	      (not (markerp vip-search-start-marker))
	      (not (equal (marker-buffer vip-search-start-marker)
			  (current-buffer)))
	      (not (eq last-command 'vip-mouse-click-search-word)))
	  (progn
	    (setq  vip-search-start-marker (point-marker)
		   vip-local-search-start-marker vip-search-start-marker
		   vip-mouse-click-search-noerror t
		   vip-mouse-click-search-limit nil)
	    
	    ;; make search string known to Viper
	    (setq vip-s-string (if vip-re-search
				   (regexp-quote click-word)
				 click-word))
	    (if (not (string= vip-s-string (car vip-search-history)))
		(setq vip-search-history
		      (cons vip-s-string vip-search-history)))
	    ))
      
      (push-mark nil t)
      (while (> arg 0)
	(vip-forward-word 1)
	(condition-case nil
	    (progn
	      (if (not (search-forward click-word vip-mouse-click-search-limit
				       vip-mouse-click-search-noerror))
		  (progn
		    (setq vip-mouse-click-search-noerror nil)
		    (setq vip-mouse-click-search-limit
			  (save-excursion
			    (if (and
				 (markerp vip-local-search-start-marker)
				 (marker-buffer vip-local-search-start-marker))
				(goto-char vip-local-search-start-marker))
			    (vip-line-pos 'end)))
			    
		    (goto-char (point-min))
		    (search-forward click-word
				    vip-mouse-click-search-limit nil)))
	      (goto-char (match-beginning 0))
	      (message "Searching for: %s" vip-s-string)
	      (if (<= arg 1)
		  (vip-flash-search-pattern))
	      )
	  (error (beep 1)
		 (if (or (not (string= click-word previous-search-string))
			 (not (eq  last-command 'vip-mouse-click-search-word)))
		     (message "`%s': String not found in %s"
			      vip-s-string (buffer-name (current-buffer)))
		   (message
		    "`%s': Last occurrence in %s. Back to beginning of search"
		    click-word (buffer-name (current-buffer)))
		   (setq arg 1) ;; to terminate the loop
		   (sit-for 2))
		 (setq  vip-mouse-click-search-noerror t) 
		 (setq  vip-mouse-click-search-limit nil)
		 (if (and (markerp vip-local-search-start-marker)
			  (marker-buffer vip-local-search-start-marker))
		     (goto-char vip-local-search-start-marker))))
	(setq arg (1- arg)))
      )))
  
(defun vip-mouse-catch-frame-switch (event arg)
  "Catch the event of switching frame.
Usually is bound to a 'down-mouse' event to work properly. See sample
bindings in viper.el and in the Viper manual."
  (interactive "e\nP")
  (setq vip-frame-of-focus nil)
  ;; pass prefix arg along to vip-mouse-click-search/insert-word
  (setq prefix-arg arg)
  (if (eq last-command 'handle-switch-frame)
      (setq vip-frame-of-focus vip-pre-click-frame))
  ;; make Emacs forget that it executed vip-mouse-catch-frame-switch
  (setq this-command last-command))
      
;; Called just before switching frames. Saves the old selected frame.
;; Sets last-command to handle-switch-frame (this is done automatically in
;; Emacs. 
;; The semantics of switching frames is different in Emacs and XEmacs.
;; In Emacs, if you select-frame A while mouse is over frame B and then
;; start typing, input goes to frame B, which becomes selected.
;; In XEmacs, input will go to frame A. This may be a bug in one of the
;; Emacsen, but also may be a design decision.
;; Also, in Emacs sending input to frame B generates handle-switch-frame
;; event, while in XEmacs it doesn't.
;; All this accounts for the difference in the behavior of
;; vip-mouse-click-* commands when you click in a frame other than the one
;; that was the last to receive input. In Emacs, focus will be in frame A
;; until you do something other than vip-mouse-click-* command.
;; In XEmacs, you have to manually select frame B (with the mouse click) in
;; order to shift focus to frame B.
(defun vip-save-pre-click-frame (frame)
  (setq last-command 'handle-switch-frame)
  (setq vip-pre-click-frame (vip-selected-frame)))


(cond (window-system
       (let* ((search-key (if vip-xemacs-p [(meta button1up)] [S-mouse-1]))
	      (search-key-catch (if vip-xemacs-p
				    [(meta button1)] [S-down-mouse-1]))
	      (insert-key (if vip-xemacs-p [(meta button2up)] [S-mouse-2]))
	      (insert-key-catch (if vip-xemacs-p
				    [(meta button2)] [S-down-mouse-2]))
	      (search-key-unbound (and (not (key-binding search-key))
				       (not (key-binding search-key-catch))))
	      (insert-key-unbound (and (not (key-binding insert-key))
				       (not (key-binding insert-key-catch))))
	      )
	     
	 (if search-key-unbound
	     (global-set-key search-key 'vip-mouse-click-search-word))
	 (if insert-key-unbound
	     (global-set-key insert-key 'vip-mouse-click-insert-word))
    
	 ;; The following would be needed if you want to use the above two
	 ;; while clicking in another frame. If you only want to use them
	 ;; by clicking in another window, not frame, the bindings below
	 ;; aren't necessary.
	 
	 ;; These must be bound to mouse-down event for the same mouse
	 ;; buttons as 'vip-mouse-click-search-word and
	 ;; 'vip-mouse-click-insert-word
	 (if search-key-unbound
	     (global-set-key search-key-catch   'vip-mouse-catch-frame-switch))
	 (if insert-key-unbound
	     (global-set-key insert-key-catch   'vip-mouse-catch-frame-switch))
	 
	 (if vip-xemacs-p
	     (add-hook 'mouse-leave-screen-hook
		       'vip-save-pre-click-frame)
	   (defadvice handle-switch-frame (before vip-frame-advice activate)
	     "Remember the selected frame before the switch-frame event." 
	     (vip-save-pre-click-frame (vip-selected-frame))))
       )))



(provide 'viper-mous)

;;;  viper-mous.el ends here
