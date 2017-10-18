;; Subroutines of Mouse handling for Sun windows
;; Copyright (C) 1987, 1991, 1992 Free Software Foundation, Inc.

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

;;; Upgrade   Apr, 1992, Jeff Peck
;;; modeline-menu
;;; modeline resize
;;; mouse-fill-paragraph(s)
;;; mouse in Buffer-menu
;;;
;;; Fix       Aug, 1989, Jeff Peck
;;; minibuf-prompt-length 
;;;
;;; Submitted Mar. 1987, Jeff Peck
;;;		 	 Sun Microsystems Inc. <peck@sun.com>
;;; Conceived Nov. 1986, Stan Jefferson,
;;;                      Computer Science Lab, SRI International.
;;; GoodIdeas Feb. 1987, Steve Greenbaum
;;; & UpClicks           Reasoning Systems, Inc.
;;;
(provide 'sun-fns)
(require 'sun-mouse)
;;;
;;; Functions for manipulating via the mouse and mouse-map definitions
;;; for accessing them.  Also definitons of mouse menus.
;;; This file you should freely modify to reflect you personal tastes.
;;;
;;; First half of file defines functions to implement mouse commands,
;;; Don't delete any of those, just add what ever else you need.
;;; Second half of file defines mouse bindings, do whatever you want there.

;;;
;;;         Mouse Functions.
;;;
;;; These functions follow the sun-mouse-handler convention of being called
;;; with three arguements: (window x-pos y-pos)
;;; This makes it easy for a mouse executed command to know where the mouse is.
;;; Use the macro "eval-in-window" to execute a function 
;;; in a temporarily selected window.
;;;
;;; If you have a function that must be called with other arguments
;;; bind the mouse button to an s-exp that contains the necessary parameters.
;;; See "minibuffer" bindings for examples.
;;;
(defconst cursor-pause-milliseconds 300
  "*Number of milliseconds to display alternate cursor (usually the mark)")

(defun indicate-region (&optional pause)
  "Bounce cursor to mark for cursor-pause-milliseconds and back again"
  (or pause (setq pause cursor-pause-milliseconds))
  (let ((point (point)))
    (goto-char (mark))
    (sit-for-millisecs pause)
    ;(update-display)
    ;(sleep-for-millisecs pause)
    (goto-char point)))


;;;
;;; Text buffer operations
;;;
(defun mouse-move-point (window x y)
  "Move point to mouse cursor."
  (select-window window)
  (move-to-loc x y)
  (if (memq last-command	; support the mouse-copy/delete/yank
	    '(mouse-copy mouse-delete mouse-yank-move))
      (setq this-command 'mouse-yank-move))
  )

(defun mouse-set-mark (window x y)
  "Set mark at mouse cursor."
  (eval-in-window window	;; use this to get the unwind protect
    (let ((point (point)))
      (move-to-loc x y)
      (set-mark (point))
      (goto-char point)
      (indicate-region)))
  )

(defun mouse-set-mark-and-select (window x y)
  "Set mark at mouse cursor, and select that window."
  (select-window window)
  (mouse-set-mark window x y)
  )

(defun mouse-set-mark-and-stuff (w x y)
  "Set mark at mouse cursor, and put region in stuff buffer."
  (mouse-set-mark-and-select w x y)
  (sun-select-region (region-beginning) (region-end)))

;;;
;;; Simple mouse dragging stuff: marking with button up
;;;

(defvar *mouse-drag-window* nil)
(defvar *mouse-drag-x* -1)
(defvar *mouse-drag-y* -1)

(defun mouse-drag-move-point (window x y)
  "Move point to mouse cursor, and allow dragging."
  (mouse-move-point window x y)
  (setq *mouse-drag-window* window
	*mouse-drag-x* x
	*mouse-drag-y* y))

(defun mouse-drag-set-mark-stuff (window x y)
  "The up click handler that goes with mouse-drag-move-point.
If mouse is in same WINDOW but at different X or Y than when
mouse-drag-move-point was last executed, set the mark at mouse
and put the region in the stuff buffer."
  (if (and (eq *mouse-drag-window* window)
	   (not (and (equal *mouse-drag-x* x)
		     (equal *mouse-drag-y* y))))
      (mouse-set-mark-and-stuff window x y)
    (setq this-command last-command))	; this was just an upclick no-op.
  )

(defun mouse-select-or-drag-move-point (window x y)
  "Select window if not selected, otherwise do mouse-drag-move-point."
  (if (eq (selected-window) window)
      (mouse-drag-move-point window x y)
    (mouse-select-window window x y)))

;;;
;;; esoteria:
;;;
(defun mouse-exch-pt-and-mark (window x y)
  "Exchange point and mark."
  (select-window window)
  (exchange-point-and-mark)
  )

(defun mouse-call-kbd-macro (window x y)
  "Invokes last keyboard macro at mouse cursor."
  (mouse-move-point window x y)
  (call-last-kbd-macro)
  )

(defun mouse-mark-thing (window x y)
  "Set point and mark to text object using syntax table.
The resulting region is put in the sun-window stuff buffer.
Left or right Paren syntax marks an s-expression.  
Clicking at the end of a line marks the line including a trailing newline.  
If it doesn't recognize one of these it marks the character at point."
  (mouse-move-point window x y)
  (if (eobp) (open-line 1))
  (let* ((char (char-after (point)))
         (syntax (char-syntax char)))
    (cond
     ((eq syntax ?w)			; word.
      (forward-word 1)
      (set-mark (point))
      (forward-word -1))
     ;; try to include a single following whitespace (is this a good idea?)
     ;; No, not a good idea since inconsistent.
     ;;(if (eq (char-syntax (char-after (mark))) ?\ )
     ;;    (set-mark (1+ (mark))))
     ((eq syntax ?\( )			; open paren.
      (mark-sexp 1))
     ((eq syntax ?\) )			; close paren.
      (forward-char 1)
      (mark-sexp -1)
      (exchange-point-and-mark))
     ((eolp)				; mark line if at end.
      (set-mark (1+ (point)))
      (beginning-of-line 1))
     (t					; mark character
      (set-mark (1+ (point)))))
    (indicate-region))			; display region boundary.
  (sun-select-region (region-beginning) (region-end))
  )

(defun mouse-kill-thing (window x y)
  "Kill thing at mouse, and put point there."
  (mouse-mark-thing window x y)
  (kill-region-and-unmark (region-beginning) (region-end))
  )

(defun mouse-kill-thing-there (window x y)
  "Kill thing at mouse, leave point where it was.
See mouse-mark-thing for a description of the objects recognized."
  (eval-in-window window 
    (save-excursion
      (mouse-mark-thing window x y)
      (kill-region (region-beginning) (region-end))))
  )

(defun mouse-save-thing (window x y &optional quiet)
  "Put thing at mouse in kill ring.
See mouse-mark-thing for a description of the objects recognized."
  (mouse-mark-thing window x y)
  (copy-region-as-kill (region-beginning) (region-end))
  (if (not quiet) (message "Thing saved"))
  )

(defun mouse-save-thing-there (window x y &optional quiet)
  "Put thing at mouse in kill ring, leave point as is.
See mouse-mark-thing for a description of the objects recognized."
  (eval-in-window window
    (save-excursion
      (mouse-save-thing window x y quiet))))

;;;
;;; Mouse yanking...
;;;
(defun mouse-copy-thing (window x y)
  "Put thing at mouse in kill ring, yank to point.
See mouse-mark-thing for a description of the objects recognized."
  (setq last-command 'not-kill)	 ;Avoids appending to previous kills.
  (mouse-save-thing-there window x y t)
  (yank)
  (setq this-command 'yank))

(defun mouse-move-thing (window x y)
  "Kill thing at mouse, yank it to point.
See mouse-mark-thing for a description of the objects recognized."
  (setq last-command 'not-kill)	 ;Avoids appending to previous kills.
  (mouse-kill-thing-there window x y)
  (yank)
  (setq this-command 'yank))

(defun mouse-yank-at-point (&optional window x y)
  "Yank from kill-ring at point; then cycle thru kill ring."
  (if (eq last-command 'yank)
      (let ((before (< (point) (mark))))
	(delete-region (point) (mark))
	(rotate-yank-pointer 1)
	(insert (car kill-ring-yank-pointer))
	(if before (exchange-point-and-mark)))
    (yank))
  (setq this-command 'yank))

(defun mouse-yank-at-mouse (window x y)
  "Yank from kill-ring at mouse; then cycle thru kill ring."
  (mouse-move-point window x y)
  (mouse-yank-at-point window x y))

(defun mouse-save/delete/yank (&optional window x y)
  "Context sensitive save/delete/yank.
Consecutive clicks perform as follows:
    * first click saves region to kill ring,
    * second click kills region,
    * third click yanks from kill ring,
    * subsequent clicks cycle thru kill ring.
If mouse-move-point is performed after the first or second click,
the next click will do a yank, etc.  Except for a possible mouse-move-point,
this command is insensitive to mouse location."
  (cond
   ((memq last-command '(mouse-delete yank mouse-yank-move))	; third+ click
    (mouse-yank-at-point))
   ((eq last-command 'mouse-copy)	; second click
    (kill-region (region-beginning) (region-end))
    (setq this-command 'mouse-delete))
   (t					; first click
    (copy-region-as-kill (region-beginning) (region-end))
    (message "Region saved")
    (setq this-command 'mouse-copy))
   ))


(defun mouse-split-horizontally (window x y)
  "Splits the window horizontally at mouse cursor."
  (eval-in-window window (split-window-horizontally (1+ x))))

(defun mouse-split-vertically (window x y)
  "Split the window vertically at the mouse cursor."
  (eval-in-window window (split-window-vertically (1+ y))))

(defun mouse-select-window (window x y)
  "Selects the window, restoring point."
  (select-window window))

(defun mouse-delete-other-windows (window x y)
  "Deletes all windows except the one mouse is in."
  (delete-other-windows window))

(defun mouse-delete-window (window x y)
  "Deletes the window mouse is in."
  (delete-window window))

(defun mouse-undo (window x y)
  "Invokes undo in the window mouse is in."
  (eval-in-window window (undo)))

;;;
;;; Scroll operations
;;;

;;; The move-to-window-line is used below because otherwise
;;; scrolling a non-selected process window with the mouse, after
;;; the process has written text past the bottom of the window,
;;; gives an "End of buffer" error, and then scrolls.  The
;;; move-to-window-line seems to force recomputing where things are.
(defun mouse-scroll-up (window x y)
  "Scrolls the window upward."
  (eval-in-window window (move-to-window-line 1) (scroll-up nil)))

(defun mouse-scroll-down (window x y)
  "Scrolls the window downward."
  (eval-in-window window (scroll-down nil)))

(defun mouse-scroll-proportional (window x y)
  "Scrolls the window proportionally corresponding to window
relative X divided by window width."
  (eval-in-window window 
    (if (>= x (1- (window-width)))
	;; When x is maximun (equal to or 1 less than window width),
	;; goto end of buffer.  We check for this special case
	;; becuase the calculated goto-char often goes short of the
	;; end due to roundoff error, and we often really want to go
	;; to the end.
	(goto-char (point-max))
      (progn
	(goto-char (+ (point-min)	; For narrowed regions.
		      (* x (/ (- (point-max) (point-min))
			      (1- (window-width))))))
	(beginning-of-line))
      )
    (what-cursor-position)		; Report position.
    ))

(defun mouse-line-to-top (window x y)
  "Scrolls the line at the mouse cursor up to the top."
  (eval-in-window window (scroll-up y)))

(defun mouse-top-to-line (window x y)
  "Scrolls the top line down to the mouse cursor."
  (eval-in-window window (scroll-down y)))

(defun mouse-line-to-bottom (window x y)
  "Scrolls the line at the mouse cursor to the bottom."
  (eval-in-window window (scroll-up (+ y (- 2 (window-height))))))

(defun mouse-bottom-to-line (window x y)
  "Scrolls the bottom line up to the mouse cursor."
  (eval-in-window window (scroll-down (+ y (- 2 (window-height))))))

(defun mouse-line-to-middle (window x y)
  "Scrolls the line at the mouse cursor to the middle."
  (eval-in-window window (scroll-up (- y -1 (/ (window-height) 2)))))

(defun mouse-middle-to-line (window x y)
  "Scrolls the line at the middle to the mouse cursor."
  (eval-in-window window (scroll-up (- (/ (window-height) 2) y 1))))


;;;
;;; main emacs menu.
;;;
(defmenu expand-menu
  ("Vertically" mouse-expand-vertically *menu-window*)
  ("Horizontally" mouse-expand-horizontally *menu-window*))

(defmenu delete-window-menu
  ("This One" delete-window *menu-window*)
  ("All Others" delete-other-windows *menu-window*))

(defmenu mouse-help-menu
  ("Text Region"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'text)
  ("Scrollbar"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'scrollbar)
  ("Modeline"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'modeline)
  ("Minibuffer"
   mouse-help-region *menu-window* *menu-x* *menu-y* 'minibuffer)
  )
  
(defmenu emacs-quit-menu
  ("Suspend" suspend-emacstool)
  ("Quit" save-buffers-kill-emacs))

(defmenu emacs-menu
  ("Emacs Menu")
  ("Stuff Selection" sun-yank-selection)
  ("Expand" . expand-menu)
  ("Delete Window" . delete-window-menu)
  ("Previous Buffer" mouse-select-previous-buffer *menu-window*)
  ("Save Buffers" save-some-buffers)
  ("List Directory" list-directory nil)
  ("Dired" dired nil)
  ("Mouse Help" . mouse-help-menu)
  ("Quit" . emacs-quit-menu))

(defun emacs-menu-eval (window x y)
  "Pop-up menu of editor commands."
  (sun-menu-evaluate window (1+ x) (1- y) 'emacs-menu))

(defun mouse-expand-horizontally (window)
  (eval-in-window window
    (enlarge-window 4 t)
    (update-display)		; Try to redisplay, since can get confused.
    ))

(defun mouse-expand-vertically (window)
  (eval-in-window window (enlarge-window 4)))

(defun mouse-select-previous-buffer (window)
  "Switch buffer in mouse window to most recently selected buffer."
  (eval-in-window window (switch-to-buffer (other-buffer))))

;;;
;;; minibuffer menu
;;;
(defmenu minibuffer-menu 
  ("Minibuffer" message "Just some miscellanous minibuffer commands")
  ("Stuff" sun-yank-selection)
  ("Do-It" exit-minibuffer)
  ("Abort" abort-recursive-edit)
  ("Suspend" suspend-emacs))

(defun minibuffer-menu-eval (window x y)
  "Pop-up menu of commands."
  (sun-menu-evaluate window x (1- y) 'minibuffer-menu))

;;; Thanks to Joe Wells for this hack.
;;; GNU Emacs should supply something better...  Oh well.
(defun minibuf-prompt-length ()
  "Returns the length of the current minibuffer prompt."
  (save-window-excursion
    (select-window (minibuffer-window))
    (save-excursion
      (let ((screen-width (screen-width))
	    (point-min (point-min))
	    length)
	(goto-char point-min)
	(insert-char ?a screen-width)
	(goto-char point-min)
	(vertical-motion 1)
	(setq length (- screen-width (point)))
	(goto-char point-min)
	(delete-char screen-width)
	length))))

(defun mini-move-point (window x y)
  (mouse-move-point window (- x (minibuf-prompt-length)) 0))

(defun mini-set-mark-and-stuff (window x y)
  (mouse-set-mark-and-stuff window (- x (minibuf-prompt-length)) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; resize from modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defvar *modeline-hit* nil "store original modline-hit data")

(defun modeline-hit (w x y) (interactive)
  (setq *modeline-hit* (cons w (caddr hit))))

(defun mouse-drag-modeline (w x y) (interactive)
  (if *modeline-hit*
      (let ((delta (- (cdr *modeline-hit*) (caddr hit)))
	    (win (car *modeline-hit*)))
	(setq *modeline-hit* nil)
	(eval-in-window win (shrink-window delta)))))

;; Modeline drag to resize:
;; Watch out for interference if you use "up" for something else
;; For example: '(text up left) is used...
(global-set-mouse '(modeline      middle)	'modeline-hit)
(global-set-mouse '(modeline   up middle)	'mouse-drag-modeline)
(global-set-mouse '(text       up middle)	'mouse-drag-modeline)
(global-set-mouse '(scrollbar  up middle)	'mouse-drag-modeline)
(global-set-mouse '(minibuffer up middle)	'mouse-drag-modeline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; modeline-menu functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;; parse thru a modeline-menu, finding item under nth character
(defun nth-menu-elt (n menu)
  (let ((n (- n (length (caar menu)))))
    (if (< n 0)
	(cdar menu)
	(if (consp (cdr menu))
	    (nth-menu-elt n (cdr menu))
	    (cdar menu)))))

(defun modeline-menu-command (x menu)
  "*Evaluate the command associated with the character N of the MENU.
Each element of MENU is of the form (STRING . ACTION). The STRING is 
displayed in the modeline and ACTION to invoked when that string is moused. 
If (commandp ACTION) is true,the ACTION is called interactively; 
otherwise, ACTION is evaled."
  (let ((command (nth-menu-elt x menu)))
    (if (commandp command)
	(call-interactively command)
	(eval command))))

(defun modeline-menu-string (menu)
  "*Extract the strings in (cdr MENU) and concatenate them into a string.
The string in (car MENU) is not included in the returned string.
    For best results, (length (caar menu)) should equal 
    the prefix in the actual modeline format string."
  (apply 'concat (mapcar 'car (cdr menu))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Buffer-mode Mouse commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun Buffer-at-mouse (w x y)
  (save-window-excursion 
    (mouse-move-point w x y)
    (Buffer-menu-buffer t)))

(defun mouse-buffer-bury (w x y)
  "Bury the indicated buffer."
  (bury-buffer (Buffer-at-mouse w x y))
  (list-buffers)
  )

(defun mouse-buffer-select (w x y)
  "Select the indicated buffer in other-window."
  (switch-to-buffer (Buffer-at-mouse w x y))
  (list-buffers)
  )

(defun mouse-buffer-delete (w x y)
  "mark indicated buffer for delete"
  (save-window-excursion
    (mouse-move-point w x y)
    (Buffer-menu-delete)
    ))

(defun mouse-buffer-mark (w x y)
  "mark indicated buffer for delete"
  (save-window-excursion
    (mouse-move-point w x y)
    (Buffer-menu-mark)
    ))

(defun mouse-buffer-execute (w x y)
  "execute buffer-menu selections"
  (save-window-excursion
    (mouse-move-point w x y)
    (Buffer-menu-execute)
    ))
  
(defun buffer-modeline-menu-cmd (w x y)
  (select-window w)
  ;; goto a line with a buffer, skip first two lines
  (let ((line-no (count-lines 1 (point))))
    (if (< line-no 2) (forward-line (- 2 line-no))))
  (modeline-menu-command x buffer-modeline-menu))

(defvar buffer-modeline-menu '(("--%%-" . (forward-line -1))
			       (" [ " . (forward-line -1))
			       ("Mark " . Buffer-menu-mark)
			       ("Del " . Buffer-menu-delete)
			       ("Save " . Buffer-menu-save)
			       ("Undo " . Buffer-menu-unmark)
			       ("Prev " . (forward-line -1))
			       ("Next " . (forward-line 1))
			       ("Edit " . Buffer-menu-select)
			       ("eXec " . Buffer-menu-execute)
			       ("] " . (forward-line 1))
			       )
  "*Each element of this list is a character STRING
\(that is displayed in the modeline\) consed to an ACTION to invoke
when that string is moused. If (commandp ACTION) is true,
the ACTION is called interactively; otherwise, ACTION is evaled."
  )

(defun enable-mouse-in-buffer-list ()
  "Call this to enable mouse selections in *Buffer List*
    LEFT puts the indicated buffer in the selected window.
    MIDDLE buries the indicated buffer.
    RIGHT marks the indicated buffer for deletion.
    MIDDLE-RIGHT deletes the marked buffers.
To unmark a buffer marked for deletion, select it with LEFT."
  
  (local-set-mouse '(text left)   'mouse-buffer-select)	    
  (local-set-mouse '(text middle) 'mouse-buffer-bury)
  (local-set-mouse '(text right)  'mouse-buffer-delete)
  (local-set-mouse '(text middle left) 'mouse-buffer-mark)
  (local-set-mouse '(text middle right) 'mouse-buffer-execute)
  (setq mode-line-buffer-identification
	(list (modeline-menu-string buffer-modeline-menu) "%b"))
  (local-set-mouse '(modeline left) 'buffer-modeline-menu-cmd)
  (local-set-mouse '(modeline left double) 'buffer-modeline-menu-cmd)
  )

(defvar buffer-menu-mode-hook nil "run-hooks when entering Buffer Menu mode.")

(if (memq 'enable-mouse-in-buffer-list buffer-menu-mode-hook)
    nil
    (setq buffer-menu-mode-hook
	  (cons 'enable-mouse-in-buffer-list buffer-menu-mode-hook)))

;; make sure a new buffer is created using buffer-menu-mode-hook
(if (get-buffer "*Buffer List*") (kill-buffer "*Buffer List*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; mouse fill (useful to re-format mail messages with long lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun mouse-fill-paragraph (w x y)
  "Utility function to fill paragraphs from mouse click, 
useful in Mail to read things that have long lines."
  (eval-in-window w
    (mouse-move-point w x y)
    (let (fill-prefix)
      (fill-paragraph nil))))


(defun fill-some-paragraphs ()
  "*Fill the succeeding paragraphs that have the same prefix."
  (interactive)
  (let (fill-prefix fpr eop beg end)
    (set-fill-prefix)
    ;; if no fill-prefix, then match lines beginning with an alpha char.
    (setq fpr (or fill-prefix "[a-zA-Z]"))
    (setq fpr (if (let ((sm (string-match "[ \t]*" fpr)))
		    (and sm (= (length fpr) (match-end 0))))
		  ;; if fill-prefix is just TAB-SPACE, then also accept
		  ;; empty lines in the region.
		  (concat "\\(" fpr "\\)\\|\\(^$\\)")
		  (regexp-quote fpr)
		))
    ;; now that we have the prefix, find a region of lines that match:
    (save-excursion 
      (beginning-of-line 1)
      (setq beg (point))
      ;; find lines with similar prefixes:
      (while (progn (forward-line 1)
		    (setq end (point))
		    (and (not (eobp)) (looking-at fpr))))
      (fill-region beg end nil))))

;; fill all succeeding paragraphs with this fill prefix
(defun mouse-fill-paragraphs (w x y)
  "Utility function to fill paragraphs from mouse click, 
useful in Mail to read things that have long lines."
  (eval-in-window w
    (mouse-move-point w x y)
    (fill-some-paragraphs)))

;;;*******************************************************************
;;;
;;;           Global Mouse Bindings.
;;;
;;; There is some sense to this mouse binding madness:
;;; LEFT and RIGHT scrolls are inverses.
;;; SHIFT makes an opposite meaning in the scroll bar.
;;; SHIFT is an alternative to DOUBLE (but double chords do not exist).
;;; META makes the scrollbar functions work in the text region.
;;; MIDDLE operates the mark
;;; LEFT operates at point

;;; META commands are generally non-destructive,
;;; SHIFT is a little more dangerous.
;;; CONTROL is for the really complicated ones.

;;; CONTROL-META-SHIFT-RIGHT gives help on that region.

;;;
;;; Text Region mousemap
;;;
;; The basics: Point, Mark, Menu, Sun-Select:
(global-set-mouse '(text        left)	'mouse-drag-move-point)
(global-set-mouse '(text     up left)	'mouse-drag-set-mark-stuff)
(global-set-mouse '(text shift  left)	'mouse-exch-pt-and-mark)
(global-set-mouse '(text double left)	'mouse-exch-pt-and-mark)

(global-set-mouse '(text	middle)	'mouse-set-mark-and-stuff)

(global-set-mouse '(text	right)	'emacs-menu-eval)
(global-set-mouse '(text shift	right)	'(sun-yank-selection))
(global-set-mouse '(text double	right)	'(sun-yank-selection))

;; The Slymoblics multi-command for Save, Kill, Copy, Move:
(global-set-mouse '(text shift	middle)	'mouse-save/delete/yank)
(global-set-mouse '(text double	middle)	'mouse-save/delete/yank)

;; Save, Kill, Copy, Move Things:
;; control-left composes with control middle/right to produce copy/move
(global-set-mouse '(text control middle	    )	'mouse-save-thing-there)
(global-set-mouse '(text control right      )	'mouse-kill-thing-there)
(global-set-mouse '(text control 	left)	'mouse-yank-at-point)
(global-set-mouse '(text control middle	left)	'mouse-copy-thing)
(global-set-mouse '(text control right	left)	'mouse-move-thing)
(global-set-mouse '(text control right middle)	'mouse-mark-thing)

;; The Universal mouse help command (press all buttons):
(global-set-mouse '(text shift  control meta right)	'mouse-help-region)
(global-set-mouse '(text double control meta right)	'mouse-help-region)

;;; Meta in Text Region is like meta version in scrollbar:
(global-set-mouse '(text meta        left)	'mouse-line-to-top)
(global-set-mouse '(text meta shift  left)	'mouse-line-to-bottom)
(global-set-mouse '(text meta double left)	'mouse-line-to-bottom)
(global-set-mouse '(text meta         middle)	'mouse-line-to-middle)
(global-set-mouse '(text meta shift   middle)	'mouse-middle-to-line)
(global-set-mouse '(text meta double  middle)	'mouse-middle-to-line)
(global-set-mouse '(text meta control middle)	'mouse-split-vertically)
(global-set-mouse '(text meta        right)	'mouse-top-to-line)
(global-set-mouse '(text meta shift  right)	'mouse-bottom-to-line)
(global-set-mouse '(text meta double right)	'mouse-bottom-to-line)

;; Miscellaneous:
(global-set-mouse '(text meta control left)	'mouse-call-kbd-macro)
(global-set-mouse '(text meta control right)	'mouse-undo)

;;;
;;; Scrollbar mousemap.
;;; Are available in the Scrollbar Region, or with Meta Text (or Meta Scrollbar)
;;;
(global-set-mouse '(scrollbar        left)	'mouse-line-to-top)
(global-set-mouse '(scrollbar shift  left)	'mouse-line-to-bottom)
(global-set-mouse '(scrollbar double left)	'mouse-line-to-bottom)

(global-set-mouse '(scrollbar         middle)	'mouse-line-to-middle)
(global-set-mouse '(scrollbar shift   middle)	'mouse-middle-to-line)
(global-set-mouse '(scrollbar double  middle)	'mouse-middle-to-line)
(global-set-mouse '(scrollbar control middle)	'mouse-split-vertically)

(global-set-mouse '(scrollbar        right)	'mouse-top-to-line)
(global-set-mouse '(scrollbar shift  right)	'mouse-bottom-to-line)
(global-set-mouse '(scrollbar double right)	'mouse-bottom-to-line)

(global-set-mouse '(scrollbar meta        left)		'mouse-line-to-top)
(global-set-mouse '(scrollbar meta shift  left)		'mouse-line-to-bottom)
(global-set-mouse '(scrollbar meta double left)		'mouse-line-to-bottom)
(global-set-mouse '(scrollbar meta         middle)	'mouse-line-to-middle)
(global-set-mouse '(scrollbar meta shift   middle)	'mouse-middle-to-line)
(global-set-mouse '(scrollbar meta double  middle)	'mouse-middle-to-line)
(global-set-mouse '(scrollbar meta control middle)	'mouse-split-vertically)
(global-set-mouse '(scrollbar meta        right)	'mouse-top-to-line)
(global-set-mouse '(scrollbar meta shift  right)	'mouse-bottom-to-line)
(global-set-mouse '(scrollbar meta double right)	'mouse-bottom-to-line)

;; And the help menu:
(global-set-mouse '(scrollbar shift  control meta right) 'mouse-help-region)
(global-set-mouse '(scrollbar double control meta right) 'mouse-help-region)

;;;
;;; Modeline mousemap.
;;;
;;; Note: meta of any single button selects window.

(global-set-mouse '(modeline double left)	'mouse-scroll-up)
(global-set-mouse '(modeline shift  left)	'mouse-scroll-up)
(global-set-mouse '(modeline double  middle)	'mouse-scroll-proportional)
(global-set-mouse '(modeline shift   middle)	'mouse-scroll-proportional)
(global-set-mouse '(modeline double right)	'mouse-scroll-down)
(global-set-mouse '(modeline shift  right)	'mouse-scroll-down)

(global-set-mouse '(modeline meta   left)	'mouse-select-window)
(global-set-mouse '(modeline meta    middle)	'mouse-select-window)
(global-set-mouse '(modeline meta   right)	'mouse-select-window)

;;; control-left selects this window, control-right deletes it.
(global-set-mouse '(modeline control left)	'mouse-delete-other-windows)
(global-set-mouse '(modeline control middle)	'mouse-split-horizontally)
(global-set-mouse '(modeline control right)	'mouse-delete-window)

;; in case of confusion, just select it:
(global-set-mouse '(modeline control left right)'mouse-select-window)

;; even without confusion (and without the keyboard) select it:
(global-set-mouse '(modeline left right)	'mouse-select-window)

;; And the help menu:
(global-set-mouse '(modeline shift  control meta right)	'mouse-help-region)
(global-set-mouse '(modeline double control meta right)	'mouse-help-region)


;;;
;;; Minibuffer Mousemap
;;; Demonstrating some variety:
;;;
(global-set-mouse '(minibuffer left)		'mini-move-point)

(global-set-mouse '(minibuffer         middle)	'mini-set-mark-and-stuff)

(global-set-mouse '(minibuffer shift   middle) '(prev-complex-command))
(global-set-mouse '(minibuffer double  middle) '(prev-complex-command))
(global-set-mouse '(minibuffer control middle) '(next-complex-command 1))
(global-set-mouse '(minibuffer meta    middle) '(previous-complex-command 1))

(global-set-mouse '(minibuffer right)	'minibuffer-menu-eval)

(global-set-mouse '(minibuffer shift  control meta right)  'mouse-help-region)
(global-set-mouse '(minibuffer double control meta right)  'mouse-help-region)

