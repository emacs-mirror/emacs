;;; mouse.el --- window system-independent mouse support.

;;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: hardware

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package provides various useful commands (including help
;; system access) through the mouse.  All this code assumes that mouse
;; interpretation has been abstracted into Emacs input events.
;;
;; The code is rather X-dependent.

;;; Code:

;;; Utility functions.

;;; Indent track-mouse like progn.
(put 'track-mouse 'lisp-indent-function 0)


(defun mouse-delete-window (click)
  "Delete the window you click on.
This must be bound to a mouse click."
  (interactive "e")
  (delete-window (posn-window (event-start click))))

(defun mouse-tear-off-window (click)
  "Delete the window clicked on, and create a new frame displaying its buffer."
  (interactive "e")
  (let* ((window (posn-window (event-start click)))
	 (buf (window-buffer window))
	 (frame (new-frame)))
    (select-frame frame)
    (switch-to-buffer buf)
    (delete-window window)))

(defun mouse-delete-other-windows ()
  "Delete all window except the one you click on."
  (interactive "@")
  (delete-other-windows))

(defun mouse-split-window-vertically (click)
  "Select Emacs window mouse is on, then split it vertically in half.
The window is split at the line clicked on.
This command must be bound to a mouse click."
  (interactive "@e")
  (let ((start (event-start click)))
    (select-window (posn-window start))
    (let ((new-height (1+ (cdr (posn-col-row (event-end click)))))
	  (first-line window-min-height)
	  (last-line (- (window-height) window-min-height)))
      (if (< last-line first-line)
	  (error "window too short to split")
	(split-window-vertically
	 (min (max new-height first-line) last-line))))))

(defun mouse-split-window-horizontally (click)
  "Select Emacs window mouse is on, then split it horizontally in half.
The window is split at the column clicked on.
This command must be bound to a mouse click."
  (interactive "@e")
  (let ((start (event-start click)))
    (select-window (posn-window start))
    (let ((new-width (1+ (car (posn-col-row (event-end click)))))
	  (first-col window-min-width)
	  (last-col (- (window-width) window-min-width)))
      (if (< last-col first-col)
	  (error "window too narrow to split")
	(split-window-horizontally
	 (min (max new-width first-col) last-col))))))

(defun mouse-set-point (click)
  "Move point to the position clicked on with the mouse.
This must be bound to a mouse click."
  (interactive "e")
  (let ((posn (event-start click)))
    (select-window (posn-window posn))
    (if (numberp (posn-point posn))
	(goto-char (posn-point posn)))))

(defun mouse-set-region (click)
  "Set the region to the text that the mouse is dragged over.
This must be bound to a mouse drag event."
  (interactive "e")
  (let ((posn (event-start click))
	(end (event-end click)))
    (select-window (posn-window posn))
    (if (numberp (posn-point posn))
	(goto-char (posn-point posn)))
    ;; If mark is highlighted, no need to bounce the cursor.
    (or (and transient-mark-mode
	     (eq (framep (selected-frame)) 'x))
	(sit-for 1))
    (push-mark)
    (if (numberp (posn-point end))
	(goto-char (posn-point end)))))

(defun mouse-drag-region (click)
  "Set the region to the text that the mouse is dragged over.
This must be bound to a button-down mouse event."
  (interactive "e")
  (let ((posn (event-start click))
	done event (mark-active nil))
    (select-window (posn-window posn))
    ;; Set point temporarily, so user sees where it is.
    (if (numberp (posn-point posn))
	(goto-char (posn-point posn)))
    ;; Turn off the old mark when we set up an empty region.
    (setq deactivate-mark t)))

;;;Nice hack, but too slow.
;;;(defun mouse-drag-region-1 (click)
;;;  "Set the region to the text that the mouse is dragged over.
;;;This must be bound to a button-down mouse event."
;;;  (interactive "e")
;;;  (let (newmark)
;;;    (let ((posn (event-start click))
;;;	  done event omark (mark-active t))
;;;      (select-window (posn-window posn))
;;;      (setq omark (and mark-active (mark)))
;;;      (if (numberp (posn-point posn))
;;;	  (goto-char (posn-point posn)))
;;;      ;; Set mark temporarily, so highlighting does what we want.
;;;      (set-marker (mark-marker) (point))
;;;      (track-mouse
;;;	(while (not done)
;;;	  (setq event (read-event))
;;;	  (if (eq (car-safe event) 'mouse-movement)
;;;	      (goto-char (posn-point (event-start event)))
;;;	    ;; Exit when we get the drag event; ignore that event.
;;;	    (setq done t))))
;;;      (if (/= (mark) (point))
;;;	  (setq newmark (mark)))
;;;      ;; Restore previous mark status.
;;;      (if omark (set-marker (mark-marker) omark)))
;;;    ;; Now, if we dragged, set the mark at the proper place.
;;;    (if newmark
;;;	(push-mark newmark t)
;;;      ;; Turn off the old mark when we set up an empty region.
;;;      (setq deactivate-mark t))))

(defun mouse-set-mark (click)
  "Set mark at the position clicked on with the mouse.
Display cursor at that position for a second.
This must be bound to a mouse click."
  (interactive "e")
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point click)
	       (push-mark nil t)
	       (sit-for 1))
      (goto-char point-save))))

(defun mouse-kill (click)
  "Kill the region between point and the mouse click.
The text is saved in the kill ring, as with \\[kill-region]."
  (interactive "e")
  (let ((click-posn (posn-point (event-start click))))
    (if (numberp click-posn)
	(kill-region (min (point) click-posn)
		     (max (point) click-posn)))))

(defun mouse-yank-at-click (click arg)
  "Insert the last stretch of killed text at the position clicked on.
Prefix arguments are interpreted as with \\[yank]."
  (interactive "e\nP")
  (mouse-set-point click)
  (yank arg))

(defun mouse-kill-ring-save (click)
  "Copy the region between point and the mouse click in the kill ring.
This does not delete the region; it acts like \\[kill-ring-save]."
  (interactive "e")
  (mouse-set-mark click)
  (call-interactively 'kill-ring-save))

;;; This function used to delete the text between point and the mouse
;;; whenever it was equal to the front of the kill ring, but some
;;; people found that confusing.

;;; A list (TEXT START END), describing the text and position of the last
;;; invocation of mouse-save-then-kill.
(defvar mouse-save-then-kill-posn nil)

(defun mouse-save-then-kill (click)
  "Save text to point in kill ring; the second time, kill the text.
If the text between point and the mouse is the same as what's
at the front of the kill ring, this deletes the text.
Otherwise, it adds the text to the kill ring, like \\[kill-ring-save],
which prepares for a second click to delete the text."
  (interactive "e")
  (let ((click-posn (posn-point (event-start click))))
    (if (and (eq last-command 'kill-region)
	     mouse-save-then-kill-posn
	     (eq (car mouse-save-then-kill-posn) (car kill-ring))
	     (equal (cdr mouse-save-then-kill-posn) (list (point) click-posn)))
	;; If this is the second time we've called
	;; mouse-save-then-kill, delete the text from the buffer.
	(progn
	  (let ((buffer-undo-list t))
	    (delete-region (point) (mark)))
	  ;; Make the undo list by hand so it is shared.
	  (if (not (eq buffer-undo-list t))
	      (setq buffer-undo-list
		    (cons (cons (car kill-ring) (point)) buffer-undo-list))))
      ;; Otherwise, save this region.
      (mouse-set-mark click)
      (call-interactively 'kill-ring-save)
      (setq mouse-save-then-kill-posn
	    (list (car kill-ring) (point) click-posn)))))

(defun mouse-buffer-menu (event)
  "Pop up a menu of buffers for selection with the mouse.
This switches buffers in the window that you clicked on,
and selects that window."
  (interactive "e")
  (let ((menu
	 (list "Buffer Menu"
	       (cons "Select Buffer"
		     (let ((tail (buffer-list))
			   head)
		       (while tail
			 (let ((elt (car tail)))
			   (if (not (string-match "^ "
						  (buffer-name elt)))
			       (setq head (cons
					   (cons
					    (format
					     "%-14s   %s"
					     (buffer-name elt)
					     (or (buffer-file-name elt) ""))
					    elt)
					   head))))
			 (setq tail (cdr tail)))
		       (reverse head))))))
    (let ((buf (x-popup-menu event menu))
	  (window (posn-window (event-start event))))
      (if buf
	  (progn
	    (select-window window)
	    (switch-to-buffer buf))))))

;;; These need to be rewritten for the new scroll bar implementation.

;;;!! ;; Commands for the scroll bar.
;;;!! 
;;;!! (defun mouse-scroll-down (click)
;;;!!   (interactive "@e")
;;;!!   (scroll-down (1+ (cdr (mouse-coords click)))))
;;;!! 
;;;!! (defun mouse-scroll-up (click)
;;;!!   (interactive "@e")
;;;!!   (scroll-up (1+ (cdr (mouse-coords click)))))
;;;!! 
;;;!! (defun mouse-scroll-down-full ()
;;;!!   (interactive "@")
;;;!!   (scroll-down nil))
;;;!! 
;;;!! (defun mouse-scroll-up-full ()
;;;!!   (interactive "@")
;;;!!   (scroll-up nil))
;;;!! 
;;;!! (defun mouse-scroll-move-cursor (click)
;;;!!   (interactive "@e")
;;;!!   (move-to-window-line (1+ (cdr (mouse-coords click)))))
;;;!! 
;;;!! (defun mouse-scroll-absolute (event)
;;;!!   (interactive "@e")
;;;!!   (let* ((pos (car event))
;;;!! 	 (position (car pos))
;;;!! 	 (length (car (cdr pos))))
;;;!!     (if (<= length 0) (setq length 1))
;;;!!     (let* ((scale-factor (max 1 (/ length (/ 8000000 (buffer-size)))))
;;;!! 	   (newpos (* (/ (* (/ (buffer-size) scale-factor)
;;;!! 			    position)
;;;!! 			 length)
;;;!! 		      scale-factor)))
;;;!!       (goto-char newpos)
;;;!!       (recenter '(4)))))
;;;!! 
;;;!! (defun mouse-scroll-left (click)
;;;!!   (interactive "@e")
;;;!!   (scroll-left (1+ (car (mouse-coords click)))))
;;;!! 
;;;!! (defun mouse-scroll-right (click)
;;;!!   (interactive "@e")
;;;!!   (scroll-right (1+ (car (mouse-coords click)))))
;;;!! 
;;;!! (defun mouse-scroll-left-full ()
;;;!!   (interactive "@")
;;;!!   (scroll-left nil))
;;;!! 
;;;!! (defun mouse-scroll-right-full ()
;;;!!   (interactive "@")
;;;!!   (scroll-right nil))
;;;!! 
;;;!! (defun mouse-scroll-move-cursor-horizontally (click)
;;;!!   (interactive "@e")
;;;!!   (move-to-column (1+ (car (mouse-coords click)))))
;;;!! 
;;;!! (defun mouse-scroll-absolute-horizontally (event)
;;;!!   (interactive "@e")
;;;!!   (let* ((pos (car event))
;;;!! 	 (position (car pos))
;;;!! 	 (length (car (cdr pos))))
;;;!!   (set-window-hscroll (selected-window) 33)))
;;;!! 
;;;!! (global-set-key [scroll-bar mouse-1] 'mouse-scroll-up)
;;;!! (global-set-key [scroll-bar mouse-2] 'mouse-scroll-absolute)
;;;!! (global-set-key [scroll-bar mouse-3] 'mouse-scroll-down)
;;;!! 
;;;!! (global-set-key [vertical-slider mouse-1] 'mouse-scroll-move-cursor)
;;;!! (global-set-key [vertical-slider mouse-2] 'mouse-scroll-move-cursor)
;;;!! (global-set-key [vertical-slider mouse-3] 'mouse-scroll-move-cursor)
;;;!! 
;;;!! (global-set-key [thumbup mouse-1] 'mouse-scroll-up-full)
;;;!! (global-set-key [thumbup mouse-2] 'mouse-scroll-up-full)
;;;!! (global-set-key [thumbup mouse-3] 'mouse-scroll-up-full)
;;;!! 
;;;!! (global-set-key [thumbdown mouse-1] 'mouse-scroll-down-full)
;;;!! (global-set-key [thumbdown mouse-2] 'mouse-scroll-down-full)
;;;!! (global-set-key [thumbdown mouse-3] 'mouse-scroll-down-full)
;;;!! 
;;;!! (global-set-key [horizontal-scroll-bar mouse-1] 'mouse-scroll-left)
;;;!! (global-set-key [horizontal-scroll-bar mouse-2]
;;;!! 		'mouse-scroll-absolute-horizontally)
;;;!! (global-set-key [horizontal-scroll-bar mouse-3] 'mouse-scroll-right)
;;;!! 
;;;!! (global-set-key [horizontal-slider mouse-1]
;;;!! 		'mouse-scroll-move-cursor-horizontally)
;;;!! (global-set-key [horizontal-slider mouse-2]
;;;!! 		'mouse-scroll-move-cursor-horizontally)
;;;!! (global-set-key [horizontal-slider mouse-3]
;;;!! 		'mouse-scroll-move-cursor-horizontally)
;;;!! 
;;;!! (global-set-key [thumbleft mouse-1] 'mouse-scroll-left-full)
;;;!! (global-set-key [thumbleft mouse-2] 'mouse-scroll-left-full)
;;;!! (global-set-key [thumbleft mouse-3] 'mouse-scroll-left-full)
;;;!! 
;;;!! (global-set-key [thumbright mouse-1] 'mouse-scroll-right-full)
;;;!! (global-set-key [thumbright mouse-2] 'mouse-scroll-right-full)
;;;!! (global-set-key [thumbright mouse-3] 'mouse-scroll-right-full)
;;;!! 
;;;!! (global-set-key [horizontal-scroll-bar S-mouse-2]
;;;!! 		'mouse-split-window-horizontally)
;;;!! (global-set-key [mode-line S-mouse-2]
;;;!! 		'mouse-split-window-horizontally)
;;;!! (global-set-key [vertical-scroll-bar S-mouse-2]
;;;!! 		'mouse-split-window)

;;;!! ;;;;
;;;!! ;;;; Here are experimental things being tested.  Mouse events
;;;!! ;;;; are of the form:
;;;!! ;;;;	((x y) window screen-part key-sequence timestamp)
;;;!! ;;
;;;!! ;;;;
;;;!! ;;;; Dynamically track mouse coordinates
;;;!! ;;;;
;;;!! ;;
;;;!! ;;(defun track-mouse (event)
;;;!! ;;  "Track the coordinates, absolute and relative, of the mouse."
;;;!! ;;  (interactive "@e")
;;;!! ;;  (while mouse-grabbed
;;;!! ;;    (let* ((pos (read-mouse-position (selected-screen)))
;;;!! ;;	   (abs-x (car pos))
;;;!! ;;	   (abs-y (cdr pos))
;;;!! ;;	   (relative-coordinate (coordinates-in-window-p
;;;!! ;;				 (list (car pos) (cdr pos))
;;;!! ;;				 (selected-window))))
;;;!! ;;      (if (consp relative-coordinate)
;;;!! ;;	  (message "mouse: [%d %d], (%d %d)" abs-x abs-y
;;;!! ;;		   (car relative-coordinate)
;;;!! ;;		   (car (cdr relative-coordinate)))
;;;!! ;;	(message "mouse: [%d %d]" abs-x abs-y)))))
;;;!! 
;;;!! ;;
;;;!! ;; Dynamically put a box around the line indicated by point
;;;!! ;;
;;;!! ;;
;;;!! ;;(require 'backquote)
;;;!! ;;
;;;!! ;;(defun mouse-select-buffer-line (event)
;;;!! ;;  (interactive "@e")
;;;!! ;;  (let ((relative-coordinate
;;;!! ;;	 (coordinates-in-window-p (car event) (selected-window)))
;;;!! ;;	(abs-y (car (cdr (car event)))))
;;;!! ;;    (if (consp relative-coordinate)
;;;!! ;;	(progn
;;;!! ;;	  (save-excursion
;;;!! ;;	    (move-to-window-line (car (cdr relative-coordinate)))
;;;!! ;;	    (x-draw-rectangle
;;;!! ;;	     (selected-screen)
;;;!! ;;	     abs-y 0
;;;!! ;;	     (save-excursion
;;;!! ;;		 (move-to-window-line (car (cdr relative-coordinate)))
;;;!! ;;		 (end-of-line)
;;;!! ;;		 (push-mark nil t)
;;;!! ;;		 (beginning-of-line)
;;;!! ;;		 (- (region-end) (region-beginning))) 1))
;;;!! ;;	  (sit-for 1)
;;;!! ;;	  (x-erase-rectangle (selected-screen))))))
;;;!! ;;
;;;!! ;;(defvar last-line-drawn nil)
;;;!! ;;(defvar begin-delim "[^ \t]")
;;;!! ;;(defvar end-delim   "[^ \t]")
;;;!! ;;
;;;!! ;;(defun mouse-boxing (event)
;;;!! ;;  (interactive "@e")
;;;!! ;;  (save-excursion
;;;!! ;;    (let ((screen (selected-screen)))
;;;!! ;;      (while (= (x-mouse-events) 0)
;;;!! ;;	(let* ((pos (read-mouse-position screen))
;;;!! ;;	       (abs-x (car pos))
;;;!! ;;	       (abs-y (cdr pos))
;;;!! ;;	       (relative-coordinate
;;;!! ;;		(coordinates-in-window-p (` ((, abs-x) (, abs-y)))
;;;!! ;;					 (selected-window)))
;;;!! ;;	       (begin-reg nil)
;;;!! ;;	       (end-reg nil)
;;;!! ;;	       (end-column nil)
;;;!! ;;	       (begin-column nil))
;;;!! ;;	  (if (and (consp relative-coordinate)
;;;!! ;;		   (or (not last-line-drawn)
;;;!! ;;		       (not (= last-line-drawn abs-y))))
;;;!! ;;	      (progn
;;;!! ;;		(move-to-window-line (car (cdr relative-coordinate)))
;;;!! ;;		(if (= (following-char) 10)
;;;!! ;;		    ()
;;;!! ;;		  (progn
;;;!! ;;		    (setq begin-reg (1- (re-search-forward end-delim)))
;;;!! ;;		    (setq begin-column (1- (current-column)))
;;;!! ;;		    (end-of-line)
;;;!! ;;		    (setq end-reg (1+ (re-search-backward begin-delim)))
;;;!! ;;		    (setq end-column (1+ (current-column)))
;;;!! ;;		    (message "%s" (buffer-substring begin-reg end-reg))
;;;!! ;;		    (x-draw-rectangle screen
;;;!! ;;				      (setq last-line-drawn abs-y)
;;;!! ;;				      begin-column
;;;!! ;;				      (- end-column begin-column) 1))))))))))
;;;!! ;;
;;;!! ;;(defun mouse-erase-box ()
;;;!! ;;  (interactive)
;;;!! ;;  (if last-line-drawn
;;;!! ;;      (progn
;;;!! ;;	(x-erase-rectangle (selected-screen))
;;;!! ;;	(setq last-line-drawn nil))))
;;;!! 
;;;!! ;;; (defun test-x-rectangle ()
;;;!! ;;;   (use-local-mouse-map (setq rectangle-test-map (make-sparse-keymap)))
;;;!! ;;;   (define-key rectangle-test-map mouse-motion-button-left 'mouse-boxing)
;;;!! ;;;   (define-key rectangle-test-map mouse-button-left-up 'mouse-erase-box))
;;;!! 
;;;!! ;;
;;;!! ;; Here is how to do double clicking in lisp.  About to change.
;;;!! ;;
;;;!! 
;;;!! (defvar double-start nil)
;;;!! (defconst double-click-interval 300
;;;!!   "Max ticks between clicks")
;;;!! 
;;;!! (defun double-down (event)
;;;!!   (interactive "@e")
;;;!!   (if double-start
;;;!!       (let ((interval (- (nth 4 event) double-start)))
;;;!! 	(if (< interval double-click-interval)
;;;!! 	    (progn
;;;!! 	      (backward-up-list 1)
;;;!! 	      ;;      (message "Interval %d" interval)
;;;!! 	      (sleep-for 1)))
;;;!! 	(setq double-start nil))
;;;!!     (setq double-start (nth 4 event))))
;;;!!     
;;;!! (defun double-up (event)
;;;!!   (interactive "@e")
;;;!!   (and double-start
;;;!!        (> (- (nth 4 event ) double-start) double-click-interval)
;;;!!        (setq double-start nil)))
;;;!! 
;;;!! ;;; (defun x-test-doubleclick ()
;;;!! ;;;   (use-local-mouse-map (setq doubleclick-test-map (make-sparse-keymap)))
;;;!! ;;;   (define-key doubleclick-test-map mouse-button-left 'double-down)
;;;!! ;;;   (define-key doubleclick-test-map mouse-button-left-up 'double-up))
;;;!! 
;;;!! ;;
;;;!! ;; This scrolls while button is depressed.  Use preferable in scroll bar.
;;;!! ;;
;;;!! 
;;;!! (defvar scrolled-lines 0)
;;;!! (defconst scroll-speed 1)
;;;!! 
;;;!! (defun incr-scroll-down (event)
;;;!!   (interactive "@e")
;;;!!   (setq scrolled-lines 0)
;;;!!   (incremental-scroll scroll-speed))
;;;!! 
;;;!! (defun incr-scroll-up (event)
;;;!!   (interactive "@e")
;;;!!   (setq scrolled-lines 0)
;;;!!   (incremental-scroll (- scroll-speed)))
;;;!! 
;;;!! (defun incremental-scroll (n)
;;;!!   (while (= (x-mouse-events) 0)
;;;!!     (setq scrolled-lines (1+ (* scroll-speed scrolled-lines)))
;;;!!     (scroll-down n)
;;;!!     (sit-for 300 t)))
;;;!! 
;;;!! (defun incr-scroll-stop (event)
;;;!!   (interactive "@e")
;;;!!   (message "Scrolled %d lines" scrolled-lines)
;;;!!   (setq scrolled-lines 0)
;;;!!   (sleep-for 1))
;;;!! 
;;;!! ;;; (defun x-testing-scroll ()
;;;!! ;;;   (let ((scrolling-map (function mouse-vertical-scroll-bar-prefix)))
;;;!! ;;;     (define-key scrolling-map mouse-button-left 'incr-scroll-down)
;;;!! ;;;     (define-key scrolling-map mouse-button-right 'incr-scroll-up)
;;;!! ;;;     (define-key scrolling-map mouse-button-left-up 'incr-scroll-stop)
;;;!! ;;;     (define-key scrolling-map mouse-button-right-up 'incr-scroll-stop)))
;;;!! 
;;;!! ;;
;;;!! ;; Some playthings suitable for picture mode?  They need work.
;;;!! ;;
;;;!! 
;;;!! (defun mouse-kill-rectangle (event)
;;;!!   "Kill the rectangle between point and the mouse cursor."
;;;!!   (interactive "@e")
;;;!!   (let ((point-save (point)))
;;;!!     (save-excursion
;;;!!       (mouse-set-point event)
;;;!!       (push-mark nil t)
;;;!!       (if (> point-save (point))
;;;!! 	  (kill-rectangle (point) point-save)
;;;!! 	(kill-rectangle point-save (point))))))
;;;!! 
;;;!! (defun mouse-open-rectangle (event)
;;;!!   "Kill the rectangle between point and the mouse cursor."
;;;!!   (interactive "@e")
;;;!!   (let ((point-save (point)))
;;;!!     (save-excursion
;;;!!       (mouse-set-point event)
;;;!!       (push-mark nil t)
;;;!!       (if (> point-save (point))
;;;!! 	  (open-rectangle (point) point-save)
;;;!! 	(open-rectangle point-save (point))))))
;;;!! 
;;;!! ;; Must be a better way to do this.
;;;!! 
;;;!! (defun mouse-multiple-insert (n char)
;;;!!   (while (> n 0)
;;;!!     (insert char)
;;;!!     (setq n (1- n))))
;;;!! 
;;;!! ;; What this could do is not finalize until button was released.
;;;!! 
;;;!! (defun mouse-move-text (event)
;;;!!   "Move text from point to cursor position, inserting spaces."
;;;!!   (interactive "@e")
;;;!!   (let* ((relative-coordinate
;;;!! 	  (coordinates-in-window-p (car event) (selected-window))))
;;;!!     (if (consp relative-coordinate)
;;;!! 	(cond ((> (current-column) (car relative-coordinate))
;;;!! 	       (delete-char
;;;!! 		(- (car relative-coordinate) (current-column))))
;;;!! 	      ((< (current-column) (car relative-coordinate))
;;;!! 	       (mouse-multiple-insert
;;;!! 		(- (car relative-coordinate) (current-column)) " "))
;;;!! 	      ((= (current-column) (car relative-coordinate)) (ding))))))

;; Font selection.

(defvar x-fixed-font-alist
  '("Font menu"
    ("Misc"
     ("fixed" "fixed")
     ("6x10" "6x10")
     ("6x12" "6x12")
     ("6x13" "6x13")
     ("7x13" "7x13")
     ("7x14" "7x14")
     ("8x13" "8x13")
     ("8x13 bold" "8x13bold")
     ("8x16" "8x16")
     ("9x15" "9x15")
     ("9x15 bold" "9x15bold")
     ("10x20" "10x20")
     ("11x18" "11x18")
     ("12x24" "12x24"))
;;; We don't seem to have these; who knows what they are.
;;;    ("fg-18" "fg-18")
;;;    ("fg-25" "fg-25")
;;;    ("lucidasanstypewriter-12" "lucidasanstypewriter-12")
;;;    ("lucidasanstypewriter-bold-14" "lucidasanstypewriter-bold-14")
;;;    ("lucidasanstypewriter-bold-24" "lucidasanstypewriter-bold-24")
;;;    ("lucidatypewriter-bold-r-24" "-b&h-lucidatypewriter-bold-r-normal-sans-24-240-75-75-m-140-iso8859-1")
;;;    ("fixed-medium-20" "-misc-fixed-medium-*-*-*-20-*-*-*-*-*-*-*")
    ("Courier"
     ("8" "-adobe-courier-medium-r-normal--8-*-*-*-m-*-iso8859-1")
     ("10" "-adobe-courier-medium-r-normal--10-*-*-*-m-*-iso8859-1")
     ("12" "-adobe-courier-medium-r-normal--12-*-*-*-m-*-iso8859-1")
     ("14" "-adobe-courier-medium-r-normal--14-*-*-*-m-*-iso8859-1")
     ("18" "-adobe-courier-medium-r-normal--18-*-*-*-m-*-iso8859-1")
     ("24" "-adobe-courier-medium-r-normal--24-*-*-*-m-*-iso8859-1")
     ("8 bold" "-adobe-courier-bold-r-normal--8-*-*-*-m-*-iso8859-1")
     ("10 bold" "-adobe-courier-bold-r-normal--10-*-*-*-m-*-iso8859-1")
     ("12 bold" "-adobe-courier-bold-r-normal--12-*-*-*-m-*-iso8859-1")
     ("14 bold" "-adobe-courier-bold-r-normal--14-*-*-*-m-*-iso8859-1")
     ("18 bold" "-adobe-courier-bold-r-normal--18-*-*-*-m-*-iso8859-1")
     ("24 bold" "-adobe-courier-bold-r-normal--24-*-*-*-m-*-iso8859-1")
     ("8 slant" "-adobe-courier-medium-o-normal--8-*-*-*-m-*-iso8859-1")
     ("10 slant" "-adobe-courier-medium-o-normal--10-*-*-*-m-*-iso8859-1")
     ("12 slant" "-adobe-courier-medium-o-normal--12-*-*-*-m-*-iso8859-1")
     ("14 slant" "-adobe-courier-medium-o-normal--14-*-*-*-m-*-iso8859-1")
     ("18 slant" "-adobe-courier-medium-o-normal--18-*-*-*-m-*-iso8859-1")
     ("24 slant" "-adobe-courier-medium-o-normal--24-*-*-*-m-*-iso8859-1")
     ("8 bold slant" "-adobe-courier-bold-o-normal--8-*-*-*-m-*-iso8859-1")
     ("10 bold slant" "-adobe-courier-bold-o-normal--10-*-*-*-m-*-iso8859-1")
     ("12 bold slant" "-adobe-courier-bold-o-normal--12-*-*-*-m-*-iso8859-1")
     ("14 bold slant" "-adobe-courier-bold-o-normal--14-*-*-*-m-*-iso8859-1")
     ("18 bold slant" "-adobe-courier-bold-o-normal--18-*-*-*-m-*-iso8859-1")
     ("24 bold slant" "-adobe-courier-bold-o-normal--24-*-*-*-m-*-iso8859-1"))
    )
  "X fonts suitable for use in Emacs.")

(defun mouse-set-font (&optional font)
  "Select an emacs font from a list of known good fonts"
  (interactive
   (x-popup-menu last-nonmenu-event x-fixed-font-alist))
  (if font
      (modify-frame-parameters (selected-frame)
			       (list (cons 'font font)))))

;;; Bindings for mouse commands.

(define-key global-map [down-mouse-1] 'mouse-drag-region)
(global-set-key [mouse-1]	'mouse-set-point)
(global-set-key [drag-mouse-1]	'mouse-set-region)

(global-set-key [mouse-2]	'mouse-yank-at-click)
(global-set-key [mouse-3]	'mouse-save-then-kill)

;; By binding these to down-going events, we let the user use the up-going
;; event to make the selection, saving a click.
(global-set-key [C-down-mouse-1]	'mouse-buffer-menu)
(global-set-key [C-down-mouse-3]	'mouse-set-font)

;; Replaced with dragging mouse-1
;; (global-set-key [S-mouse-1]	'mouse-set-mark)

(global-set-key [mode-line mouse-1] 'mouse-delete-other-windows)
(global-set-key [mode-line mouse-3] 'mouse-delete-window)
(global-set-key [mode-line S-mouse-2] 'mouse-split-window-horizontally)

;; Define the mouse help menu tree.

(defvar help-menu-map '(keymap "Help"))
(global-set-key [C-down-mouse-2] help-menu-map)

(defvar help-apropos-map (make-sparse-keymap "Is there a command that..."))
(defvar help-keys-map (make-sparse-keymap "Key Commands <==> Functions"))
(defvar help-manual-map (make-sparse-keymap "Manual and tutorial"))
(defvar help-misc-map (make-sparse-keymap "Odds and ends"))
(defvar help-modes-map (make-sparse-keymap "Modes"))
(defvar help-admin-map (make-sparse-keymap "Administrivia"))

(define-key help-menu-map [apropos]
  (cons "@Is there a command that..." help-apropos-map))
(define-key help-menu-map [keys]
  (cons "@Key Commands <==> Functions" help-keys-map))
(define-key help-menu-map [manuals]
  (cons "@Manual and tutorial" help-manual-map))
(define-key help-menu-map [misc]
  (cons "@Odds and ends" help-misc-map))
(define-key help-menu-map [modes]
  (cons "@Modes" help-modes-map))
(define-key help-menu-map [admin]
  (cons "@Administrivia" help-admin-map))

(define-key help-apropos-map "c" '("Command Apropos" . command-apropos))
(define-key help-apropos-map "a" '("Apropos" . apropos))

(define-key help-keys-map "b"
  '("List all keystroke commands" . describe-bindings))
(define-key help-keys-map "c"
  '("Describe key briefly" . describe-key-briefly))
(define-key help-keys-map "k"
  '("Describe key verbose" . describe-key))
(define-key help-keys-map "f"
  '("Describe Lisp function" . describe-function))
(define-key help-keys-map "w"
  '("Where is this command" . where-is))

(define-key help-manual-map "i" '("Info system" . info))
(define-key help-manual-map "t"
  '("Invoke Emacs tutorial" . help-with-tutorial))

(define-key help-misc-map "l" '("Last 100 Keystrokes" . view-lossage))
(define-key help-misc-map "s" '("Describe syntax table" . describe-syntax))

(define-key help-modes-map "m"
  '("Describe current major mode" . describe-mode))
(define-key help-modes-map "b"
  '("List all keystroke commands" . describe-bindings))

(define-key help-admin-map "n"
  '("view Emacs news" . view-emacs-news))
(define-key help-admin-map "l"
  '("View the GNU Emacs license" . describe-copying))
(define-key help-admin-map "d"
  '("Describe distribution" . describe-distribution))
(define-key help-admin-map "w"
  '("Describe (non)warranty" . describe-no-warranty))

(provide 'mouse)

;;; mouse.el ends here
