;;; mouse-sel.el --- Multi-click selection support for Emacs 19

;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: Mike Williams <mikew@gopher.dosli.govt.nz>
;; Keywords: mouse
;; Version: 2.1

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;;
;; This module provides multi-click mouse support for GNU Emacs versions
;; 19.18 and later.  I've tried to make it behave more like standard X
;; clients (eg. xterm) than the default Emacs 19 mouse selection handlers.
;; Basically:
;;
;;   * Clicking mouse-1 starts (cancels) selection, dragging extends it.
;;
;;   * Clicking or dragging mouse-3 extends the selection as well.
;;
;;   * Double-clicking on word constituents selects words.
;;     Double-clicking on symbol constituents selects symbols.
;;     Double-clicking on quotes or parentheses selects sexps.
;;     Double-clicking on whitespace selects whitespace.
;;     Triple-clicking selects lines.
;;
;;   * Selecting sets the region & X primary selection, but does NOT affect
;;     the kill-ring.  Because the mouse handlers set the primary selection
;;     directly, mouse-sel sets the variables interprogram-cut-function
;;     and interprogram-paste-function to nil.
;;
;;   * Clicking mouse-2 pastes contents of primary selection at the mouse
;;     position.
;;
;;   * Pressing mouse-2 while selecting or extending copies selection
;;     to the kill ring.  Pressing mouse-1 or mouse-3 kills it.
;;     
;;   * Double-clicking mouse-3 also kills selection.
;;
;; This module requires my thingatpt.el module, which it uses to find the
;; bounds of words, lines, sexps, etc.
;;
;; Thanks to KevinB@bartley.demon.co.uk for his useful input.
;;
;;--- Customisation -------------------------------------------------------
;;
;; * You may want to use none or more of following:
;;
;;      ;; Enable region highlight
;;      (transient-mark-mode 1)
;;
;;      ;; But only in the selected window
;;      (setq highlight-nonselected-windows nil)
;;      
;;      ;; Enable pending-delete
;;      (delete-selection-mode 1)
;;
;; * You can control the way mouse-sel binds it's keys by setting the value
;;   of mouse-sel-default-bindings before loading mouse-sel.
;;
;;   (a) If mouse-sel-default-bindings = t (the default)
;;   
;;       Mouse sets and pastes selection
;;	   mouse-1		mouse-select
;;	   mouse-2		mouse-insert-selection
;;         mouse-3		mouse-extend
;;
;;       Selection/kill-ring interaction is disabled
;;         interprogram-cut-function   = nil
;;         interprogram-paste-function = nil
;;
;;   (b) If mouse-sel-default-bindings = 'interprogram-cut-paste
;;   
;;       Mouse sets selection, and pastes from kill-ring
;;	   mouse-1		mouse-select
;;	   mouse-2		mouse-yank-at-click
;;	   mouse-3		mouse-extend
;;  
;;       Selection/kill-ring interaction is retained
;;         interprogram-cut-function   = x-select-text
;;         interprogram-paste-function = x-cut-buffer-or-selection-value
;;         
;;       What you lose is the ability to select some text in
;;       delete-selection-mode and yank over the top of it.
;;       
;;   (c) If mouse-sel-default-bindings = nil, no bindings are made.
;;
;; * By default, mouse-insert-selection (mouse-2) inserts the selection at
;;   the mouse position.  You can tell it to insert at point instead with:
;;
;;     (setq mouse-yank-at-point t)
;;
;; * I like to leave point at the end of the region nearest to where the
;;   mouse was, even though this makes region highlighting mis-leading (the
;;   cursor makes it look like one extra character is selected).  You can
;;   disable this behaviour with:
;;
;;     (setq mouse-sel-leave-point-near-mouse nil)
;;
;; * Normally, the selection highlight will be removed when the mouse is
;;   lifted.  You can tell mouse-sel to retain the selection highlight
;;   (useful if you don't use transient-mark-mode) with:
;;
;;     (setq mouse-sel-retain-highlight t)
;;
;; * By default, mouse-select cycles the click count after 3 clicks.  That
;;   is, clicking mouse-1 four times has the same effect as clicking it
;;   once, clicking five times has the same effect as clicking twice, etc.
;;   Disable this behaviour with:
;;
;;     (setq mouse-sel-cycle-clicks nil)
;;
;; * The variables mouse-sel-{set,get,check}-selection-function control how
;;   the selection is handled.  Under X Windows, these variables default so
;;   that the X primary selection is used.  Under other windowing systems,
;;   alternate functions are used, which simply store the selection value
;;   in a variable.
;;
;;--- Hints ---------------------------------------------------------------
;;
;; * You can change the selection highlight face by altering the properties
;;   of mouse-drag-overlay, eg.
;;
;;     (overlay-put mouse-drag-overlay 'face 'bold)
;;
;; * Pasting from the primary selection under emacs 19.19 is SLOW (there's
;;   a two second delay).  The following code will cause mouse-sel to use
;;   the cut buffer rather than the primary selection.  However, be aware
;;   that cut buffers are OBSOLETE, and some X applications may not support
;;   them.
;;   
;;     (setq mouse-sel-set-selection-function 'x-select-text
;;           mouse-sel-get-selection-function 'x-get-cut-buffer)
;;           
;;--- Warnings ------------------------------------------------------------
;;
;; * When selecting sexps, the selection extends by sexps at the same
;;   nesting level.  This also means the selection cannot be extended out
;;   of the enclosing nesting level.  This is INTENTIONAL.

;;; Code: =================================================================

(provide 'mouse-sel)

(require 'mouse)
(require 'thingatpt)

;;=== Version =============================================================

(defconst mouse-sel-version "2.1" 
  "The version number of mouse-sel (as string).")

;;=== User Variables ======================================================

(defvar mouse-sel-leave-point-near-mouse t
  "*Leave point near last mouse position.
If non-nil, \\[mouse-select] and \\[mouse-extend] leave point at the end
of the region nearest to where the mouse last was.
If nil, point is always placed at the beginning of the region.")

(defvar mouse-sel-retain-highlight nil
  "*Retain highlight after dragging is finished.
If non-nil, regions selected using \\[mouse-select] and \\[mouse-extend] will
remain highlighted.
If nil, highlighting turns off when you release the mouse button.")

(defvar mouse-sel-cycle-clicks t
  "*If non-nil, \\[mouse-select] cycles the click-counts after 3 clicks.
Ie. 4 clicks = 1 click, 5 clicks = 2 clicks, etc.")

(defvar mouse-sel-default-bindings t
  "Set to nil before loading `mouse-sel' to prevent default mouse bindings.")

;;=== Selection ===========================================================

(defvar mouse-sel-selection-type nil "Type of current selection")
(make-variable-buffer-local 'mouse-sel-selection-type)

(defvar mouse-sel-selection "" 
  "Store the selection value when using a window systems other than X.")

(defvar mouse-sel-set-selection-function 
  (if (fboundp 'x-set-selection)
      (function (lambda (s) (x-set-selection 'PRIMARY s)))
    (function (lambda (s) (setq mouse-sel-selection s))))
  "Function to call to set selection.
Called with one argument, the text to select.")

(defvar mouse-sel-get-selection-function
  (if (fboundp 'x-get-selection)
      'x-get-selection 
    (function (lambda () mouse-sel-selection)))
  "Function to call to get the selection.
Called with no argument.")

(defvar mouse-sel-check-selection-function
  (if (fboundp 'x-selection-owner-p)
      'x-selection-owner-p 
    nil)
  "Function to check whether Emacs still owns the selection.
Called with no arguments.")

(defun mouse-sel-determine-selection-type (NCLICKS)
  "Determine what \"thing\" `mouse-sel' should operate on.
The first argument, NCLICKS, is the number of consecutive
mouse clicks at the same position."
  (let* ((next-char (char-after (point)))
	 (char-syntax (if next-char (char-syntax next-char)))
	 (nclicks (if mouse-sel-cycle-clicks (1+ (% (1- NCLICKS) 3)) NCLICKS)))
    (cond
     ((= nclicks 1) nil)
     ((>= nclicks 3) 'line)
     ((memq char-syntax '(?\( ?\) ?\" ?')) 'sexp)
     ((memq next-char '(? ?\t ?\n)) 'whitespace)
     ((eq char-syntax ?_) 'symbol)
     ((eq char-syntax ?w) 'word))))

(defun mouse-select (EVENT)
  "Set region/selection using the mouse.

Clicking sets point to click position, and deactivates the mark
if you are in Transient Mark mode.
Dragging extends region/selection.

Double-clicking on word constituents selects words.
Double-clicking on symbol constituents selects symbols.
Double-clicking on quotes or parentheses selects sexps.
Double-clicking on whitespace selects whitespace.
Triple-clicking selects lines.

Clicking mouse-2 while selecting copies the region to the kill-ring.
Clicking mouse-1 or mouse-3 kills the region.

This should be bound to a down-mouse event."
  (interactive "e")
  (mouse-set-point EVENT)
  (setq mouse-sel-selection-type
	(mouse-sel-determine-selection-type (event-click-count EVENT)))
  (let ((object-bounds (bounds-of-thing-at-point mouse-sel-selection-type)))
    (if object-bounds
	(progn
	  (setq mark-active t)
	  (goto-char (car object-bounds))
	  (set-mark (cdr object-bounds)))
      (deactivate-mark)))
  (mouse-extend))

(defun mouse-extend (&optional EVENT)
  "Extend region/selection using the mouse.

See documentation for mouse-select for more details.

This should be bound to a down-mouse event."
  (interactive "e")
  (if EVENT (select-window (posn-window (event-end EVENT))))
  (let* ((use-region (and (or EVENT transient-mark-mode) mark-active))
	 (min (if use-region (region-beginning) (point)))
	 (max (if use-region (region-end) (point)))
	 (orig-window (selected-window))
	 (orig-window-frame (window-frame orig-window))
	 (top (nth 1 (window-edges orig-window)))
	 (bottom (nth 3 (window-edges orig-window)))
	 (orig-cursor-type 
	  (cdr (assoc 'cursor-type (frame-parameters (selected-frame)))))
	 direction
	 event)

    ;; Inhibit normal region highlight
    (setq mark-active nil)

    ;; Highlight region (forcing re-highlight)
    (move-overlay mouse-drag-overlay min max (current-buffer))
    (overlay-put mouse-drag-overlay 'face
		 (overlay-get mouse-drag-overlay 'face))

    ;; Bar cursor
    (if (fboundp 'modify-frame-parameters)
	(modify-frame-parameters (selected-frame) '((cursor-type . bar))))

    ;; Handle dragging
    (unwind-protect
	(progn 
	  (track-mouse
	    
	    (while (if EVENT		; Use initial event
		       (prog1
			   (setq event EVENT)
			 (setq EVENT nil))
		     (setq event (read-event))
		     (and (consp event)
			  (memq (car event) '(mouse-movement switch-frame))))
		  
	      (let ((end (event-end event)))
		    
		(cond
		     
		 ;; Ignore any movement outside the frame
		 ((eq (car-safe event) 'switch-frame) nil)
		 ((and (posn-window end)
		       (not (eq (let ((posn-w (posn-window end)))
				  (if (windowp posn-w)
				      (window-frame posn-w)
				    posn-w))
				(window-frame orig-window)))) nil)
		     
		 ;; Different window, same frame
		 ((not (eq (posn-window end) orig-window))
		  (let ((end-row (cdr (cdr (mouse-position)))))
		    (cond
		     ((and end-row (not (bobp)) (< end-row top))
		      (mouse-scroll-subr orig-window (- end-row top)
					 mouse-drag-overlay max))
		     ((and end-row (not (eobp)) (>= end-row bottom))
		      (mouse-scroll-subr orig-window (1+ (- end-row bottom))
					 mouse-drag-overlay min))
		     )))

		 ;; On the mode line
		 ((eq (posn-point end) 'mode-line)
		  (mouse-scroll-subr orig-window 1 mouse-drag-overlay min))

		 ;; In original window
		 (t (goto-char (posn-point end)))

		 )
		;; Determine direction of drag
		(cond
		 ((and (not direction) (not (eq min max)))
		  (setq direction (if (< (point) (/ (+ min max) 2)) -1 1)))
		 ((and (not (eq direction -1)) (<= (point) min))
		  (setq direction -1))
		 ((and (not (eq direction 1)) (>= (point) max))
		  (setq direction 1)))
		
		(if (not mouse-sel-selection-type) nil
		  
		  ;; If dragging forward, goal is next character
		  (if (and (eq direction 1) (not (eobp))) (forward-char 1))
		  
		  ;; Move to start/end of selected thing
		  (let ((goal (point))
			last)
		    (goto-char (if (eq 1 direction) min max))
		    (condition-case nil
			(progn
			  (while (> (* direction (- goal (point))) 0)
			    (setq last (point))
			    (forward-thing mouse-sel-selection-type 
					   direction))
			  (let ((end (point)))
			    (forward-thing mouse-sel-selection-type
					   (- direction))
			    (goto-char
			     (if (> (* direction (- goal (point))) 0)
				 end last))))
		      (error))))
		
		;; Move overlay
		(move-overlay mouse-drag-overlay
			      (if (eq 1 direction) min (point))
			      (if (eq -1 direction) max (point))
			      (current-buffer))
	      
		)))			; end track-mouse

	  (let ((overlay-start (overlay-start mouse-drag-overlay))
		(overlay-end (overlay-end mouse-drag-overlay)))

	    ;; Set region
	    (if (eq overlay-start overlay-end)
		(deactivate-mark)
	      (if (and mouse-sel-leave-point-near-mouse (eq direction 1))
		  (progn
		    (set-mark overlay-start)
		    (goto-char overlay-end))
		(set-mark overlay-end)
		(goto-char overlay-start)))
	    
	    ;; Set selection
	    (if (and mark-active mouse-sel-set-selection-function)
		(funcall mouse-sel-set-selection-function 
			 (buffer-substring overlay-start overlay-end)))
	      
	    ;; Handle copy/kill
	    (cond
	     ((eq (car-safe last-input-event) 'down-mouse-2)
	      (copy-region-as-kill overlay-start overlay-end)
	      (read-event) (read-event))
	     ((memq (car-safe last-input-event) '(down-mouse-1 down-mouse-3))
	      (kill-region overlay-start overlay-end)
	      (deactivate-mark)
	      (read-event) (read-event))
	     ((eq (car-safe last-input-event) 'double-mouse-3)
	      (kill-region overlay-start overlay-end)
	      (deactivate-mark)))))

      ;; Restore cursor
      (if (fboundp 'modify-frame-parameters)
	  (modify-frame-parameters 
	   (selected-frame) (list (cons 'cursor-type orig-cursor-type))))
      
      ;; Remove overlay
      (or mouse-sel-retain-highlight
	  (delete-overlay mouse-drag-overlay)))))

(defun mouse-insert-selection (click)
  "Insert the contents of the selection at mouse click.
If `mouse-yank-at-point' is non-nil, insert at point instead."
  (interactive "e")
  (or mouse-yank-at-point 
      (mouse-set-point click))
  (deactivate-mark)
  (if mouse-sel-get-selection-function
      (insert (or (funcall mouse-sel-get-selection-function) ""))))

(defun mouse-sel-validate-selection ()
  "Remove selection highlight if emacs no longer owns the primary selection."
  (or (not mouse-sel-check-selection-function)
      (funcall mouse-sel-check-selection-function)
      (delete-overlay mouse-drag-overlay)))

(add-hook 'pre-command-hook 'mouse-sel-validate-selection)

;;=== Key bindings ========================================================

(if (not mouse-sel-default-bindings) nil
  
  (global-unset-key [mouse-1])
  (global-unset-key [drag-mouse-1])
  (global-unset-key [mouse-3])
  
  (global-set-key [down-mouse-1]	'mouse-select)
  (global-set-key [down-mouse-3] 	'mouse-extend)
  
  (if (eq mouse-sel-default-bindings 'interprogram-cut-paste) nil
    
    (global-set-key [mouse-2] 	'mouse-insert-selection)
    (setq interprogram-cut-function nil
	  interprogram-paste-function nil))
  
  )

;; mouse-sel.el ends here.
