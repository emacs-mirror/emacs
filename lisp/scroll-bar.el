;;; scroll-bar.el --- window system-independent scroll bar support

;; Copyright (C) 1993-1995, 1999-2020 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: hardware
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Window-system-independent bindings of mouse clicks on the scroll bar.
;; Presently emulates the scroll-bar behavior of xterm.

;;; Code:

(require 'mouse)
(eval-when-compile (require 'cl-lib))


;;;; Utilities.

(defun scroll-bar-event-ratio (event)
  "Given a scroll bar event EVENT, return the scroll bar position as a ratio.
The value is a cons cell (PORTION . WHOLE) containing two integers
whose ratio gives the event's vertical position in the scroll bar, with 0
referring to the top and 1 to the bottom."
  (nth 2 event))

(defun scroll-bar-scale (num-denom whole)
  "Given a pair (NUM . DENOM) and WHOLE, return (/ (* NUM WHOLE) DENOM).
This is handy for scaling a position on a scroll bar into real units,
like buffer positions.  If SCROLL-BAR-POS is the (PORTION . WHOLE) pair
from a scroll bar event, then (scroll-bar-scale SCROLL-BAR-POS
\(buffer-size)) is the position in the current buffer corresponding to
that scroll bar position."
  ;; We multiply before we divide to maintain precision.
  (truncate (* (car num-denom) whole) (cdr num-denom)))

(defun scroll-bar-columns (side)
  "Return the width, measured in columns, of the vertical scrollbar on SIDE.
SIDE must be the symbol `left' or `right'."
  (let* ((wsb   (window-scroll-bars))
         (vtype (nth 2 wsb))
         (cols  (nth 1 wsb)))
    (cond
     ((not (memq side '(left right)))
      (error "`left' or `right' expected instead of %S" side))
     ((and (eq vtype side) cols))
     ((eq (frame-parameter nil 'vertical-scroll-bars) side)
      ;; nil means it's a non-toolkit scroll bar, and its width in
      ;; columns is 14 pixels rounded up.
      (ceiling (or (frame-parameter nil 'scroll-bar-width) 14)
               (frame-char-width)))
     (0))))

(defun scroll-bar-lines ()
  "Return the height, measured in lines, of the horizontal scrollbar."
  (let* ((wsb   (window-scroll-bars))
         (htype (nth 5 wsb))
         (lines  (nth 4 wsb)))
    (cond
     (htype lines)
     ((frame-parameter nil 'horizontal-scroll-bars)
      ;; nil means it's a non-toolkit scroll bar (which is currently
      ;; impossible), and its height in lines is 14 pixels rounded up.
      (ceiling (or (frame-parameter nil 'scroll-bar-height) 14)
               (frame-char-width)))
     (0))))


;;;; Helpful functions for enabling and disabling scroll bars.

(defvar scroll-bar-mode)
(defvar horizontal-scroll-bar-mode)
(defvar previous-scroll-bar-mode nil)

(defvar scroll-bar-mode-explicit nil
  "Non-nil means `set-scroll-bar-mode' should really do something.
This is nil while loading `scroll-bar.el', and t afterward.")

(defun set-scroll-bar-mode (value)
  "Set the scroll bar mode to VALUE and put the new value into effect.
See the `scroll-bar-mode' variable for possible values to use."
  (if scroll-bar-mode
      (setq previous-scroll-bar-mode scroll-bar-mode))

  (setq scroll-bar-mode value)

  (when scroll-bar-mode-explicit
    (modify-all-frames-parameters (list (cons 'vertical-scroll-bars
					      scroll-bar-mode)))))

(defcustom scroll-bar-mode default-frame-scroll-bars
  "Specify whether to have vertical scroll bars, and on which side.
Possible values are nil (no scroll bars), `left' (scroll bars on left)
and `right' (scroll bars on right).
To set this variable in a Lisp program, use `set-scroll-bar-mode'
to make it take real effect.
Setting the variable with a customization buffer also takes effect."
  :type '(choice (const :tag "none (nil)" nil)
		 (const left)
		 (const right))
  :group 'frames
  ;; The default value for :initialize would try to use :set
  ;; when processing the file in cus-dep.el.
  :initialize 'custom-initialize-default
  :set (lambda (_sym val) (set-scroll-bar-mode val)))

;; We just set scroll-bar-mode, but that was the default.
;; If it is set again, that is for real.
(setq scroll-bar-mode-explicit t)

(defun get-scroll-bar-mode ()
  (declare (gv-setter set-scroll-bar-mode))
  scroll-bar-mode)

(define-minor-mode scroll-bar-mode
  "Toggle vertical scroll bars on all frames (Scroll Bar mode).

This command applies to all frames that exist and frames to be
created in the future."
  :variable ((get-scroll-bar-mode)
             . (lambda (v) (set-scroll-bar-mode
			    (if v (or previous-scroll-bar-mode
				      default-frame-scroll-bars))))))

(defun horizontal-scroll-bars-available-p ()
  "Return non-nil when horizontal scroll bars are available on this system."
  (and (display-graphic-p)
       (boundp 'x-toolkit-scroll-bars)
       x-toolkit-scroll-bars))

(define-minor-mode horizontal-scroll-bar-mode
  "Toggle horizontal scroll bars on all frames (Horizontal Scroll Bar mode).

This command applies to all frames that exist and frames to be
created in the future."
  :init-value nil
  :global t
  :group 'frames
  (if (and horizontal-scroll-bar-mode
	   (not (horizontal-scroll-bars-available-p)))
      (progn
	(setq horizontal-scroll-bar-mode nil)
	(message "Horizontal scroll bars are not implemented on this system"))
    (dolist (frame (frame-list))
      (set-frame-parameter
       frame 'horizontal-scroll-bars horizontal-scroll-bar-mode))
    ;; Handle `default-frame-alist' entry.
    (setq default-frame-alist
	  (cons (cons 'horizontal-scroll-bars horizontal-scroll-bar-mode)
		(assq-delete-all 'horizontal-scroll-bars
				 default-frame-alist)))))

(defun toggle-scroll-bar (arg)
  "Toggle whether or not the selected frame has vertical scroll bars.
With ARG, turn vertical scroll bars on if and only if ARG is positive.
The variable `scroll-bar-mode' controls which side the scroll bars are on
when they are turned on; if it is nil, they go on the left."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (frame-parameter nil 'vertical-scroll-bars) -1 1))
    (setq arg (prefix-numeric-value arg)))
  (modify-frame-parameters
   (selected-frame)
   (list (cons 'vertical-scroll-bars
	       (if (> arg 0)
		   (or scroll-bar-mode default-frame-scroll-bars))))))

(defun toggle-horizontal-scroll-bar (arg)
  "Toggle whether or not the selected frame has horizontal scroll bars.
With ARG, turn vertical scroll bars on if and only if ARG is positive."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (frame-parameter nil 'horizontal-scroll-bars) -1 1))
    (setq arg (prefix-numeric-value arg)))
  (modify-frame-parameters
   (selected-frame)
   (list (cons 'horizontal-scroll-bars
	       (when (> arg 0) 'bottom)))))

;;;; Buffer navigation using the scroll bar.

;; This was used for up-events on button 2, but no longer.
(defun scroll-bar-set-window-start (event)
  "Set the window start according to where the scroll bar is dragged.
EVENT should be a scroll bar click or drag event."
  (interactive "e")
  (let* ((end-position (event-end event))
	 (window (nth 0 end-position))
	 (portion-whole (nth 2 end-position)))
    (with-current-buffer (window-buffer window)
      (save-excursion
	(goto-char (+ (point-min)
		      (scroll-bar-scale portion-whole
					(- (point-max) (point-min)))))
	(beginning-of-line)
	(set-window-start window (point))))))

(defun scroll-bar-drag-position (portion-whole)
  "Calculate new window start for drag event."
  (save-excursion
    (goto-char (+ (point-min)
		  (scroll-bar-scale portion-whole
				    (- (point-max) (point-min)))))
    (beginning-of-line)
    (point)))

(defun scroll-bar-maybe-set-window-start (event)
  "Set the window start according to where the scroll bar is dragged.
Only change window start if the new start is substantially different.
EVENT should be a scroll bar click or drag event."
  (interactive "e")
  (let* ((end-position (event-end event))
	 (window (nth 0 end-position))
	 (portion-whole (nth 2 end-position))
	 (next-portion-whole (cons (1+ (car portion-whole))
				   (cdr portion-whole)))
	 portion-start
	 next-portion-start
	 (current-start (window-start window)))
    (with-current-buffer (window-buffer window)
      (setq portion-start (scroll-bar-drag-position portion-whole))
      (setq next-portion-start (max
				(scroll-bar-drag-position next-portion-whole)
				(1+ portion-start)))
      (if (or (>= current-start next-portion-start)
	      (< current-start portion-start))
	  (set-window-start window portion-start)
	;; Always set window start, to ensure scroll bar position is updated.
	(set-window-start window current-start)))))

;; Scroll the window to the proper position for EVENT.
(defun scroll-bar-drag-1 (event)
  (let* ((start-position (event-start event))
	 (window (nth 0 start-position))
	 (portion-whole (nth 2 start-position)))
    ;; With 'scroll-bar-adjust-thumb-portion' nil and 'portion-whole'
    ;; indicating that the buffer is fully visible, do not scroll the
    ;; window since that might make it impossible to scroll it back
    ;; with GTK's thumb (Bug#32002).
    (when (or scroll-bar-adjust-thumb-portion
              (not (numberp (car portion-whole)))
              (not (numberp (cdr portion-whole)))
              (/= (car portion-whole) (cdr portion-whole)))
      (save-excursion
        (with-current-buffer (window-buffer window)
	  ;; Calculate position relative to the accessible part of the buffer.
	  (goto-char (+ (point-min)
		        (scroll-bar-scale portion-whole
					  (- (point-max) (point-min)))))
	  (vertical-motion 0 window)
	  (set-window-start window (point)))))))

(defun scroll-bar-drag (event)
  "Scroll the window by dragging the scroll bar slider.
If you click outside the slider, the window scrolls to bring the slider there."
  (interactive "e")
  (let* (done
	 (echo-keystrokes 0)
	 (end-position (event-end event))
	 (window (nth 0 end-position))
	 (before-scroll))
    (with-current-buffer (window-buffer window)
      (setq before-scroll point-before-scroll))
    (save-selected-window
      (select-window window 'mark-for-redisplay)
      (setq before-scroll
	    (or before-scroll (point))))
    (scroll-bar-drag-1 event)
    (track-mouse
      (while (not done)
	(setq event (read-event))
	(if (eq (car-safe event) 'mouse-movement)
	    (setq event (read-event)))
	(cond ((eq (car-safe event) 'scroll-bar-movement)
	       (scroll-bar-drag-1 event))
	      (t
	       ;; Exit when we get the drag event; ignore that event.
	       (setq done t)))))
    (sit-for 0)
    (with-current-buffer (window-buffer window)
      (setq point-before-scroll before-scroll))))

;; Scroll the window to the proper position for EVENT.
(defun scroll-bar-horizontal-drag-1 (event)
  (let* ((start-position (event-start event))
	 (window (nth 0 start-position))
	 (portion-whole (nth 2 start-position))
	 (unit (frame-char-width (window-frame window))))
    (if (eq (current-bidi-paragraph-direction (window-buffer window))
	    'left-to-right)
	(set-window-hscroll
	 window (/ (+ (car portion-whole) (1- unit)) unit))
      (set-window-hscroll
       window (/ (+ (- (cdr portion-whole) (car portion-whole))
		    (1- unit))
		 unit)))))

(defun scroll-bar-horizontal-drag (event)
  "Scroll the window horizontally by dragging the scroll bar slider.
If you click outside the slider, the window scrolls to bring the slider there."
  (interactive "e")
  (let* (done
	 (echo-keystrokes 0)
	 (end-position (event-end event))
	 (window (nth 0 end-position))
	 (before-scroll))
    (with-current-buffer (window-buffer window)
      (setq before-scroll point-before-scroll))
    (save-selected-window
      (select-window window 'mark-for-redisplay)
      (setq before-scroll
	    (or before-scroll (point))))
    (scroll-bar-horizontal-drag-1 event)
    (track-mouse
      (while (not done)
	(setq event (read-event))
	(if (eq (car-safe event) 'mouse-movement)
	    (setq event (read-event)))
	(cond ((eq (car-safe event) 'scroll-bar-movement)
	       (scroll-bar-horizontal-drag-1 event))
	      (t
	       ;; Exit when we get the drag event; ignore that event.
	       (setq done t)))))
    (sit-for 0)
    (with-current-buffer (window-buffer window)
      (setq point-before-scroll before-scroll))))

(defun scroll-bar-scroll-down (event)
  "Scroll the window's top line down to the location of the scroll bar click.
EVENT should be a scroll bar click."
  (interactive "e")
  (let* ((end-position (event-end event))
	 (window (nth 0 end-position))
	 (before-scroll))
    (with-current-buffer (window-buffer window)
      (setq before-scroll point-before-scroll))
    (unwind-protect
	(save-selected-window
	  (let ((portion-whole (nth 2 end-position)))
	    (select-window window 'mark-for-redisplay)
	    (setq before-scroll
		  (or before-scroll (point)))
	    (scroll-down
	     (scroll-bar-scale portion-whole (1- (window-height)))))
	  (sit-for 0))
      (with-current-buffer (window-buffer window)
	(setq point-before-scroll before-scroll)))))

(defun scroll-bar-scroll-up (event)
  "Scroll the line next to the scroll bar click to the top of the window.
EVENT should be a scroll bar click."
  (interactive "e")
  (let* ((end-position (event-end event))
	 (window (nth 0 end-position))
	 (before-scroll))
    (with-current-buffer (window-buffer window)
      (setq before-scroll point-before-scroll))
    (unwind-protect
	(save-selected-window
	  (let ((portion-whole (nth 2 end-position)))
	    (select-window window 'mark-for-redisplay)
	    (setq before-scroll
		  (or before-scroll (point)))
	    (scroll-up
	     (scroll-bar-scale portion-whole (1- (window-height)))))
	  (sit-for 0))
      (with-current-buffer (window-buffer window)
	(setq point-before-scroll before-scroll)))))


;;; Tookit scroll bars.

(defun scroll-bar-toolkit-scroll (event)
  "Handle event EVENT on vertical scroll bar."
  (interactive "e")
  (let* ((end-position (event-end event))
	 (window (nth 0 end-position))
	 (part (nth 4 end-position))
	 before-scroll)
    (cond
     ((eq part 'end-scroll))
     (t
      (with-current-buffer (window-buffer window)
	(setq before-scroll point-before-scroll))
      (save-selected-window
	(select-window window 'mark-for-redisplay)
	(setq before-scroll (or before-scroll (point)))
	(cond
	 ((eq part 'above-handle)
	  (scroll-up '-))
	 ((eq part 'below-handle)
	  (scroll-up nil))
	 ((eq part 'ratio)
	  (let* ((portion-whole (nth 2 end-position))
		 (lines (scroll-bar-scale portion-whole
					  (1- (window-height)))))
	    (scroll-up (cond ((not (zerop lines)) lines)
			     ((< (car portion-whole) 0) -1)
			     (t 1)))))
	 ((eq part 'up)
	  (scroll-up -1))
	 ((eq part 'down)
	  (scroll-up 1))
	 ((eq part 'top)
	  (set-window-start window (point-min)))
	 ((eq part 'bottom)
	  (goto-char (point-max))
	  (recenter))
	 ((eq part 'handle)
	  (scroll-bar-drag-1 event))))
      (sit-for 0)
      (with-current-buffer (window-buffer window)
	(setq point-before-scroll before-scroll))))))

(defun scroll-bar-toolkit-horizontal-scroll (event)
  "Handle event EVENT on horizontal scroll bar."
  (interactive "e")
  (let* ((end-position (event-end event))
	 (window (nth 0 end-position))
	 (part (nth 4 end-position))
	 (bidi-factor
	  (if (eq (current-bidi-paragraph-direction (window-buffer window))
		  'left-to-right)
	      1
	    -1))
	 before-scroll)
    (cond
     ((eq part 'end-scroll))
     (t
      (with-current-buffer (window-buffer window)
	(setq before-scroll point-before-scroll))
      (save-selected-window
	(select-window window 'mark-for-redisplay)
	(setq before-scroll (or before-scroll (point)))
	(cond
	 ((eq part 'before-handle)
	  (scroll-right (* bidi-factor 4)))
	 ((eq part 'after-handle)
	  (scroll-left (* bidi-factor 4)))
	 ((eq part 'ratio)
	  (let* ((portion-whole (nth 2 end-position))
		 (columns (scroll-bar-scale portion-whole
					    (1- (window-width)))))
	    (scroll-right
	     (* (cond
		 ((not (zerop columns))
		  columns)
		 ((< (car portion-whole) 0) -1)
		 (t 1))
		bidi-factor))))
	 ((eq part 'left)
	  (scroll-right (* bidi-factor 1)))
	 ((eq part 'right)
	  (scroll-left (* bidi-factor 1)))
	 ((eq part 'leftmost)
	  (goto-char (if (eq bidi-factor 1)
			 (line-beginning-position)
		       (line-end-position))))
	 ((eq part 'rightmost)
	  (goto-char (if (eq bidi-factor 1)
			 (line-end-position)
		       (line-beginning-position))))
	 ((eq part 'horizontal-handle)
	  (scroll-bar-horizontal-drag-1 event))))
      (sit-for 0)
      (with-current-buffer (window-buffer window)
	(setq point-before-scroll before-scroll))))))

;;;; Bindings.

;; For now, we'll set things up to work like xterm.
(cond ((and (boundp 'x-toolkit-scroll-bars) x-toolkit-scroll-bars)
       (global-set-key [vertical-scroll-bar mouse-1]
		       'scroll-bar-toolkit-scroll)
       (global-set-key [horizontal-scroll-bar mouse-1]
		       'scroll-bar-toolkit-horizontal-scroll))
      (t
       (global-set-key [vertical-scroll-bar mouse-1]
		       'scroll-bar-scroll-up)
       (global-set-key [vertical-scroll-bar drag-mouse-1]
		       'scroll-bar-scroll-up)
       (global-set-key [vertical-scroll-bar down-mouse-2]
		       'scroll-bar-drag)
       (global-set-key [vertical-scroll-bar mouse-3]
		       'scroll-bar-scroll-down)
       (global-set-key [vertical-scroll-bar drag-mouse-3]
		       'scroll-bar-scroll-down)))


(provide 'scroll-bar)

;;; scroll-bar.el ends here
