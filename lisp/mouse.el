;; Mouse support that is independent of window systems.
;; Copyright (C) 1988 Free Software Foundation, Inc.

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

(provide 'mouse)


(defun mouse-select ()
  "Select Emacs window the mouse is on."
  (interactive "@"))

(defun mouse-delete-window ()
  "Delete the Emacs window the mouse is on."
  (interactive "@")
  (delete-window))

(defun mouse-keep-one-window ()
  "Select Emacs window mouse is on, then kill all other Emacs windows."
  (interactive "@")
  (delete-other-windows))

(defun mouse-select-and-split ()
  "Select Emacs window mouse is on, then split it vertically in half."
  (interactive "@")
  (split-window-vertically nil))

(defun mouse-set-point (event)
  "Select Emacs window mouse is on, and move point to mouse position."
  (interactive "@e")
  (let ((relative-coordinate
	 (coordinates-in-window-p (car event) (selected-window))))
    (if (consp relative-coordinate)
	(progn
	  (move-to-window-line (car (cdr relative-coordinate)))
	  (move-to-column (+ (car relative-coordinate) (current-column)
			     (window-hscroll (selected-window))))
	  (what-line)))))

(defun mouse-eval-last-sexpr (event)
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (eval-last-sexp nil)))

(defun mouse-line-length (event)
  "Print the length of the line indicated by the pointer."
  (interactive "@e")
  (let ((relative-coordinate
	  (coordinates-in-window-p (car event) (selected-window))))
    (if (consp relative-coordinate)
	(save-excursion
	  (move-to-window-line (car (cdr relative-coordinate)))
	  (end-of-line)
	  (push-mark nil t)
	  (beginning-of-line)
	  (message "Line length: %d"
			   (- (region-end) (region-beginning)))
	  (sleep-for 1)))))

(defun mouse-set-mark (event)
  "Select Emacs window mouse is on, and set mark at mouse position.
Display cursor at that position for a second."
  (interactive "@e")
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point event)
	       (push-mark nil t)
	       (sleep-for 1))
      (goto-char point-save))))

(defun mouse-scroll (event)
  "Scroll point to the mouse position."
  (interactive "@e")
  (let ((relative-coordinate
	 (coordinates-in-window-p (car event) (selected-window))))
    (if (consp relative-coordinate)
	(progn
	  (recenter (car (cdr relative-coordinate)))
	  (scroll-right (+ (car relative-coordinate) (current-column)))))))

(defun mouse-del-char (event)
  "Delete the char pointed to by the mouse."
  (interactive "@e")
  (let ((relative-coordinate
	  (coordinates-in-window-p (car event) (selected-window))))
    (if (consp relative-coordinate)
	(progn
	  (move-to-window-line (car (cdr relative-coordinate)))
	  (move-to-column (+ (car relative-coordinate) (current-column)))
	  (delete-char 1 nil)))))

(defun mouse-kill-line (event)
  "Kill the line pointed to by the mouse."
  (interactive "@e")
  (let ((relative-coordinate
	 (coordinates-in-window-p (car event) (selected-window))))
    (if (consp relative-coordinate)
	(progn
	  (move-to-window-line (car (cdr relative-coordinate)))
	  (move-to-column (+ (car relative-coordinate) (current-column)))
	  (kill-line nil)))))

(defun narrow-window-to-region (m n)
  "Narrow window to region between point and last mark"
  (interactive "r")
  (save-excursion
    (save-restriction
      (if (eq (selected-window) (next-window))
	  (split-window))
      (goto-char m)
      (recenter 0)
      (if (eq (selected-window)
	      (if (zerop (minibuffer-depth))
		  (next-window)))
	  ()
	(shrink-window (- (- (window-height) (count-lines m n)) 1))))))

(defun mouse-window-to-region (event)
  "Narrow window to region between cursor and mouse pointer."
  (interactive "@e")
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point event)
	       (push-mark nil t)
	       (sit-for 1))
      (goto-char point-save)
      (narrow-window-to-region (region-beginning) (region-end)))))

(defun mouse-ignore ()
  "Don't do anything."
  (interactive))

;; Commands for the scroll bar.

(defun mouse-scroll-down (nlines)
  (interactive "@p")
  (scroll-down nlines))

(defun mouse-scroll-up (nlines)
  (interactive "@p")
  (scroll-up nlines))

(defun mouse-scroll-down-full ()
  (interactive "@")
  (scroll-down nil))

(defun mouse-scroll-up-full ()
  (interactive "@")
  (scroll-up nil))

(defun mouse-scroll-move-cursor (nlines)
  (interactive "@p")
  (move-to-window-line nlines))

(defun mouse-scroll-absolute (event)
  (interactive "@e")
  (let* ((pos (car event))
	 (position (car pos))
	 (length (car (cdr pos))))
    (if (<= length 0) (setq length 1))
    (let* ((scale-factor (max 1 (/ length (/ 8000000 (buffer-size)))))
	   (newpos (* (/ (* (/ (buffer-size) scale-factor)
			    position)
			 length)
		      scale-factor)))
      (goto-char newpos)
      (recenter '(4)))))

(defun mouse-scroll-left (ncolumns)
  (interactive "@p")
  (scroll-left ncolumns))

(defun mouse-scroll-right (ncolumns)
  (interactive "@p")
  (scroll-right ncolumns))

(defun mouse-scroll-left-full ()
  (interactive "@")
  (scroll-left nil))

(defun mouse-scroll-right-full ()
  (interactive "@")
  (scroll-right nil))

(defun mouse-scroll-move-cursor-horizontally (ncolumns)
  (interactive "@p")
  (move-to-column ncolumns))

(defun mouse-scroll-absolute-horizontally (event)
  (interactive "@e")
  (let* ((pos (car event))
	 (position (car pos))
	 (length (car (cdr pos))))
  (set-window-hscroll (selected-window) 33)))

;; Set up these commands, including the prefix keys for the scroll bar.

(fset 'mouse-vertical-scroll-bar-prefix (make-sparse-keymap))
(define-key global-mouse-map mouse-vertical-scroll-bar-prefix
  'mouse-vertical-scroll-bar-prefix)

(defun mouse-scroll-motion (event)
  (interactive "e")
  (let ((pos (car (car event)))
	(length (car (cdr (car event)))))
    (message "[%d %d]" pos length)))

(let ((map (function mouse-vertical-scroll-bar-prefix)))
  (define-key map mouse-button-right 'mouse-scroll-down)
  (define-key map mouse-button-left 'mouse-scroll-up)
  (define-key map mouse-button-middle 'mouse-scroll-absolute)
  (define-key map mouse-motion 'x-horizontal-line))

;(fset 'mouse-vertical-slider-prefix (make-sparse-keymap))
;(define-key global-mouse-map mouse-vertical-slider-prefix
;  'mouse-vertical-slider-prefix)

;(let ((map (function mouse-vertical-slider-prefix)))
;  (define-key map mouse-button-right 'mouse-scroll-move-cursor)
;  (define-key map mouse-button-left 'mouse-scroll-move-cursor)
;  (define-key map mouse-button-middle 'mouse-scroll-move-cursor))

(fset 'mouse-vertical-thumbup-prefix (make-sparse-keymap))
(define-key global-mouse-map mouse-vertical-thumbup-prefix
  'mouse-vertical-thumbup-prefix)

(let ((map (function mouse-vertical-thumbup-prefix)))
  (define-key map mouse-button-right 'mouse-scroll-down-full)
  (define-key map mouse-button-left 'mouse-scroll-down-full)
  (define-key map mouse-button-middle 'mouse-scroll-down-full))

(fset 'mouse-vertical-thumbdown-prefix (make-sparse-keymap))
(define-key global-mouse-map mouse-vertical-thumbdown-prefix
  'mouse-vertical-thumbdown-prefix)

(let ((map (function mouse-vertical-thumbdown-prefix)))
  (define-key map mouse-button-right 'mouse-scroll-up-full)
  (define-key map mouse-button-left 'mouse-scroll-up-full)
  (define-key map mouse-button-middle 'mouse-scroll-up-full))

;; Horizontal bar

(fset 'mouse-horizontal-scroll-bar-prefix (make-sparse-keymap))
(define-key global-mouse-map mouse-horizontal-scroll-bar-prefix
  'mouse-horizontal-scroll-bar-prefix)

(let ((map (function mouse-horizontal-scroll-bar-prefix)))
  (define-key map mouse-button-right 'mouse-scroll-right)
  (define-key map mouse-button-left 'mouse-scroll-left)
  (define-key map mouse-button-middle 'mouse-scroll-absolute-horizontally))

(fset 'mouse-horizontal-thumbleft-prefix (make-sparse-keymap))
(define-key global-mouse-map mouse-horizontal-thumbleft-prefix
  'mouse-horizontal-thumbleft-prefix)

(let ((map (function mouse-horizontal-thumbleft-prefix)))
  (define-key map mouse-button-right 'mouse-scroll-left-full)
  (define-key map mouse-button-left 'mouse-scroll-left-full)
  (define-key map mouse-button-middle 'mouse-scroll-left-full))

(fset 'mouse-horizontal-thumbright-prefix (make-sparse-keymap))
(define-key global-mouse-map mouse-horizontal-thumbright-prefix
  'mouse-horizontal-thumbright-prefix)

(let ((map (function mouse-horizontal-thumbright-prefix)))
  (define-key map mouse-button-right 'mouse-scroll-right-full)
  (define-key map mouse-button-left 'mouse-scroll-right-full)
  (define-key map mouse-button-middle 'mouse-scroll-right-full))


;;
;; Here are experimental things being tested.  Mouse events
;; are of the form:
;;	((x y) window screen-part key-sequence timestamp)

;;
;; Dynamically track mouse coordinates
;;

(defun track-mouse (event)
  "Track the coordinates, absolute and relative, of the mouse."
  (interactive "@e")
  (while mouse-grabbed
    (let* ((pos (read-mouse-position (selected-screen)))
	   (abs-x (car pos))
	   (abs-y (cdr pos))
	   (relative-coordinate (coordinates-in-window-p
				 (list (car pos) (cdr pos))
				 (selected-window))))
      (if (consp relative-coordinate)
	  (message "mouse: [%d %d], (%d %d)" abs-x abs-y
		   (car relative-coordinate)
		   (car (cdr relative-coordinate)))
	(message "mouse: [%d %d]" abs-x abs-y)))))

;;
;; Dynamically put a box around the line indicated by point
;;

(require 'backquote)

(defun mouse-select-buffer-line (event)
  (interactive "@e")
  (let ((relative-coordinate
	 (coordinates-in-window-p (car event) (selected-window)))
	(abs-y (car (cdr (car event)))))
    (if (consp relative-coordinate)
	(progn
	  (save-excursion
	    (move-to-window-line (car (cdr relative-coordinate)))
	    (x-draw-rectangle
	     (selected-screen)
	     abs-y 0
	     (save-excursion
		 (move-to-window-line (car (cdr relative-coordinate)))
		 (end-of-line)
		 (push-mark nil t)
		 (beginning-of-line)
		 (- (region-end) (region-beginning))) 1)
	    (setq the-buffer (Buffer-menu-buffer t)))
	  (sit-for 1)
	  (x-erase-rectangle (selected-screen))))))

(defvar last-line-drawn nil)
(defvar begin-delim "[^ \t]")
(defvar end-delim   "[^ \t]")

(defun mouse-boxing (event)
  (interactive "@e")
  (save-excursion
    (let ((screen (selected-screen)))
      (while (= (x-mouse-events) 0)
	(let* ((pos (read-mouse-position screen))
	       (abs-x (car pos))
	       (abs-y (cdr pos))
	       (relative-coordinate
		(coordinates-in-window-p (` ((, abs-x) (, abs-y)))
					 (selected-window)))
	       (begin-reg nil)
	       (end-reg nil)
	       (end-column nil)
	       (begin-column nil))
	  (if (and (consp relative-coordinate)
		   (or (not last-line-drawn)
		       (not (= last-line-drawn abs-y))))
	      (progn
		(move-to-window-line (car (cdr relative-coordinate)))
		(if (= (following-char) 10)
		    ()
		  (progn
		    (setq begin-reg (1- (re-search-forward end-delim)))
		    (setq begin-column (1- (current-column)))
		    (end-of-line)
		    (setq end-reg (1+ (re-search-backward begin-delim)))
		    (setq end-column (1+ (current-column)))
		    (message "%s" (buffer-substring begin-reg end-reg))
		    (x-draw-rectangle screen
				      (setq last-line-drawn abs-y)
				      begin-column
				      (- end-column begin-column) 1))))))))))

(defun mouse-erase-box ()
  (interactive)
  (if last-line-drawn
      (progn
	(x-erase-rectangle (selected-screen))
	(setq last-line-drawn nil))))

(defun test-x-rectangle ()
  (use-local-mouse-map (setq rectangle-test-map (make-sparse-keymap)))
  (define-key rectangle-test-map mouse-motion-button-left 'mouse-boxing)
  (define-key rectangle-test-map mouse-button-left-up 'mouse-erase-box))

;;
;; Here is how to do double clicking in lisp.  About to change.
;;

(defvar double-start nil)
(defconst double-click-interval 300
  "Max ticks between clicks")

(defun double-down (event)
  (interactive "@e")
  (if double-start
      (let ((interval (- (nth 4 event) double-start)))
	(if (< interval double-click-interval)
	    (progn
	      (backward-up-list 1)
	      ;;      (message "Interval %d" interval)
	      (sleep-for 1)))
	(setq double-start nil))
    (setq double-start (nth 4 event))))
    
(defun double-up (event)
  (interactive "@e")
  (and double-start
       (> (- (nth 4 event ) double-start) double-click-interval)
       (setq double-start nil)))

(defun x-test-doubleclick ()
  (use-local-mouse-map (setq doubleclick-test-map (make-sparse-keymap)))
  (define-key doubleclick-test-map mouse-button-left 'double-down)
  (define-key doubleclick-test-map mouse-button-left-up 'double-up))

;;
;; This scrolls while button is depressed.  Use preferable in scrollbar.
;;

(defvar scrolled-lines 0)
(defconst scroll-speed 1)

(defun incr-scroll-down (event)
  (interactive "@e")
  (setq scrolled-lines 0)
  (incremental-scroll scroll-speed))

(defun incr-scroll-up (event)
  (interactive "@e")
  (setq scrolled-lines 0)
  (incremental-scroll (- scroll-speed)))

(defun incremental-scroll (n)
  (while (= (x-mouse-events) 0)
    (setq scrolled-lines (1+ (* scroll-speed scrolled-lines)))
    (scroll-down n)
    (sit-for 300 t)))

(defun incr-scroll-stop (event)
  (interactive "@e")
  (message "Scrolled %d lines" scrolled-lines)
  (setq scrolled-lines 0)
  (sleep-for 1))

(defun x-testing-scroll ()
  (let ((scrolling-map (function mouse-vertical-scroll-bar-prefix)))
    (define-key scrolling-map mouse-button-left 'incr-scroll-down)
    (define-key scrolling-map mouse-button-right 'incr-scroll-up)
    (define-key scrolling-map mouse-button-left-up 'incr-scroll-stop)
    (define-key scrolling-map mouse-button-right-up 'incr-scroll-stop)))

;;
;; Some playthings suitable for picture mode?  They need work.
;;

(defun mouse-kill-rectangle (event)
  "Kill the rectangle between point and the mouse cursor."
  (interactive "@e")
  (let ((point-save (point)))
    (save-excursion
      (mouse-set-point event)
      (push-mark nil t)
      (if (> point-save (point))
	  (kill-rectangle (point) point-save)
	(kill-rectangle point-save (point))))))

(defun mouse-open-rectangle (event)
  "Kill the rectangle between point and the mouse cursor."
  (interactive "@e")
  (let ((point-save (point)))
    (save-excursion
      (mouse-set-point event)
      (push-mark nil t)
      (if (> point-save (point))
	  (open-rectangle (point) point-save)
	(open-rectangle point-save (point))))))

;; Must be a better way to do this.

(defun mouse-multiple-insert (n char)
  (while (> n 0)
    (insert char)
    (setq n (1- n))))

;; What this could do is not finalize until button was released.

(defun mouse-move-text (event)
  "Move text from point to cursor position, inserting spaces."
  (interactive "@e")
  (let* ((relative-coordinate
	  (coordinates-in-window-p (car event) (selected-window))))
    (if (consp relative-coordinate)
	(cond ((> (current-column) (car relative-coordinate))
	       (delete-char
		(- (car relative-coordinate) (current-column))))
	      ((< (current-column) (car relative-coordinate))
	       (mouse-multiple-insert
		(- (car relative-coordinate) (current-column)) " "))
	      ((= (current-column) (car relative-coordinate)) (ding))))))
