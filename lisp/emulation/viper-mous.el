;;; viper-mous.el --- mouse support for Viper

;; Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

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

;; Code

(require 'viper-util)

;; compiler pacifier
(defvar double-click-time)
(defvar mouse-track-multi-click-time)
;; end compiler pacifier


;;; Variables
  
;; Variable used for catching the switch-frame event.
;; If non-nil, indicates that previous-frame should be the selected
;; one. Used by vip-mouse-click-get-word. Not a user option.
(defvar vip-frame-of-focus nil)
    
;; Frame that was selected before the switch-frame event.
(defconst vip-current-frame-saved (selected-frame))
  
(defvar vip-surrounding-word-function 'vip-surrounding-word
  "*Function that determines what constitutes a word for clicking events.
Takes two parameters: a COUNT, indicating how many words to return, 
and CLICK-COUNT, telling whether this is the first click, a double-click,
or a tripple-click.")
       
;; time interval in millisecond within which successive clicks are
;; considered related
(defconst vip-multiclick-timeout (if vip-xemacs-p
				     mouse-track-multi-click-time
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

(defsubst vip-multiclick-p ()
  (not (vip-sit-for-short vip-multiclick-timeout t)))

;; Returns window where click occurs
(defsubst vip-mouse-click-window (click)
  (if vip-xemacs-p
      (event-window click)
    (posn-window (event-start click))))

;; Returns window where click occurs
(defsubst vip-mouse-click-frame (click)
  (window-frame (vip-mouse-click-window click)))

;; Returns the buffer of the window where click occurs
(defsubst vip-mouse-click-window-buffer (click)
  (window-buffer (vip-mouse-click-window click)))

;; Returns the name of the buffer in the window where click occurs
(defsubst vip-mouse-click-window-buffer-name (click)
  (buffer-name (vip-mouse-click-window-buffer click)))

;; Returns position of a click
(defsubst vip-mouse-click-posn (click)
  (if vip-xemacs-p
      (event-point click)
    (posn-point (event-start click))))
	     

(defun vip-surrounding-word (count click-count)
   "Returns word surrounding point according to a heuristic.
COUNT indicates how many regions to return.
If CLICK-COUNT is 1, `word' is a word in Vi sense.
If CLICK-COUNT is 2,then `word' is a Word in Vi sense.
If the character clicked on is a non-separator and is non-alphanumeric but
is adjacent to an alphanumeric symbol, then it is considered alphanumeric
for the purpose of this command. If this character has a matching
character, such as `\(' is a match for `\)', then the matching character is
also considered alphanumeric.
For convenience, in Lisp modes, `-' is considered alphanumeric.

If CLICK-COUNT is 3 or more, returns the line clicked on with leading and
trailing space and tabs removed. In that case, the first argument, COUNT,
is ignored."
   (let ((modifiers "")
	 beg skip-flag result
	 word-beg)
     (if (> click-count 2)
	 (save-excursion
	   (beginning-of-line)
	   (vip-skip-all-separators-forward 'within-line)
	   (setq beg (point))
	   (end-of-line)
	   (setq result (buffer-substring beg (point))))
       
       (if (and (not (vip-looking-at-alphasep))
		(or (save-excursion (vip-backward-char-carefully)
				    (vip-looking-at-alpha))
		    (save-excursion (vip-forward-char-carefully)
				    (vip-looking-at-alpha))))
	   (setq modifiers
		 (cond ((looking-at "\\\\") "\\\\")
		       ((looking-at "-") "C-C-")
		       ((looking-at "[][]") "][")
		       ((looking-at "[()]") ")(")
		       ((looking-at "[{}]") "{}")
		       ((looking-at "[<>]") "<>")
		       ((looking-at "[`']") "`'")
		       ((looking-at "\\^") "\\^")
		       ((vip-looking-at-separator) "")
		       (t (char-to-string (following-char))))
		 ))
       
       ;; Add `-' to alphanum, if it wasn't added and if we are in Lisp
       (or (looking-at "-")
	   (not (string-match "lisp" (symbol-name major-mode)))
	   (setq modifiers (concat modifiers "C-C-")))
       
       
       (save-excursion
	 (cond ((> click-count 1) (vip-skip-nonseparators 'backward))
	       ((vip-looking-at-alpha modifiers)
		(vip-skip-alpha-backward modifiers))
	       ((not (vip-looking-at-alphasep modifiers))
		(vip-skip-nonalphasep-backward))
	       (t (if (> click-count 1)
		      (vip-skip-nonseparators 'backward)
		    (vip-skip-alpha-backward modifiers))))

	 (setq word-beg (point))
	 
	 (setq skip-flag nil) ; don't move 1 char forw the first time
	 (while (> count 0)
	   (if skip-flag (vip-forward-char-carefully 1))
	   (setq skip-flag t) ; now always move 1 char forward
	   (if (> click-count 1)
	       (vip-skip-nonseparators 'forward)
	     (vip-skip-alpha-forward modifiers))
	   (setq count (1- count)))

	 (setq result (buffer-substring word-beg (point))))
       ) ; if
     ;; XEmacs doesn't have set-text-properties, but there buffer-substring
     ;; doesn't return properties together with the string, so it's not needed.
     (if vip-emacs-p
	 (set-text-properties 0 (length result) nil result))
     result
     ))


(defun vip-mouse-click-get-word (click count click-count)
  "Returns word surrounding the position of a mouse click.
Click may be in another window. Current window and buffer isn't changed.
On single or double click, returns the word as determined by
`vip-surrounding-word-function'."
     
  (let ((click-word "")
	(click-pos (vip-mouse-click-posn click))
	(click-buf (vip-mouse-click-window-buffer click)))
    (or (natnump count) (setq count 1))
    (or (natnump click-count) (setq click-count 1))
     
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


(defun vip-mouse-click-insert-word (click arg)
  "Insert word clicked or double-clicked on.
With prefix argument, N, insert that many words.
This command must be bound to a mouse click.
The double-click action of the same mouse button must not be bound
\(or it must be bound to the same function\).
See `vip-surrounding-word' for the definition of a word in this case."
  (interactive "e\nP")
  (if vip-frame-of-focus	;; to handle clicks in another frame
      (select-frame vip-frame-of-focus))
      
  ;; turn arg into a number
  (cond ((integerp arg) nil)
	;; prefix arg is a list when one hits C-u then command
	((and (listp arg) (integerp (car arg)))
	 (setq arg (car arg)))
	(t (setq arg 1)))
      
  (let (click-count interrupting-event)
    (if (and
	 (vip-multiclick-p)
	 ;; This trick checks if there is a pending mouse event
	 ;; if so, we use this latter event and discard the current mouse click
	 ;; If the next pending event is not a mouse event, we execute
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
      (select-frame vip-frame-of-focus))
  (let (click-word click-count
	(previous-search-string vip-s-string))
    
    (if (and
	 (vip-multiclick-p)
	 ;; This trick checks if there is a pending mouse event
	 ;; if so, we use this latter event and discard the current mouse click
	 ;; If the next pending event is not a mouse event, we execute
	 ;; the current mouse event
	 (progn
	   (vip-read-event)
	   (vip-mouse-event-p last-input-event)))
	(progn ;; interrupted wait
	  (setq vip-global-prefix-argument 
		(or vip-global-prefix-argument arg))
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
      (setq vip-frame-of-focus vip-current-frame-saved))
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
(defsubst vip-remember-current-frame (frame)
  (setq last-command 'handle-switch-frame
	vip-current-frame-saved (selected-frame)))


(cond ((vip-window-display-p)
       (let* ((search-key (if vip-xemacs-p
			      [(meta shift button1up)] [M-S-mouse-1]))
	      (search-key-catch (if vip-xemacs-p
				    [(meta shift button1)] [M-S-down-mouse-1]))
	      (insert-key (if vip-xemacs-p
			      [(meta shift button2up)] [M-S-mouse-2]))
	      (insert-key-catch (if vip-xemacs-p
				    [(meta shift button2)] [M-S-down-mouse-2]))
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
	     (add-hook 'mouse-leave-frame-hook
		       'vip-remember-current-frame)
	   (defadvice handle-switch-frame (before vip-frame-advice activate)
	     "Remember the selected frame before the switch-frame event." 
	     (vip-remember-current-frame (selected-frame))))
       )))



(provide 'viper-mous)

;;;  viper-mous.el ends here
