;;; scroll-bar.el --- window system-independent scroll bar support.

;;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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

;;; Code:

;;; Commentary:

;; Window-system-independent bindings of mouse clicks on the scroll bar.
;; Presently emulates the scroll-bar behavior of xterm.
;;; Code:

(require 'mouse)


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
  ;; We use floating point because the product of a large buffer size
  ;; with a large scroll bar portion can easily overflow a lisp int.
  (truncate (/ (* (float (car num-denom)) whole) (cdr num-denom))))


;;;; Helpful functions for enabling and disabling scroll bars.

(defun scroll-bar-mode (flag)
  "Toggle display of vertical scroll bars on each frame.
This command applies to all frames that exist and frames to be
created in the future.
With a numeric argument, if the argument is negative,
turn off scroll bars; otherwise, turn on scroll bars."
  (interactive "P")
  (if flag (setq flag (prefix-numeric-value flag)))

  ;; Obtain the current setting by looking at default-frame-alist.
  (let ((scroll-bar-mode
	 (let ((assq (assq 'vertical-scroll-bars default-frame-alist)))
	   (if assq (cdr assq) t))))

    ;; Tweedle it according to the argument.
    (setq scroll-bar-mode (if (null flag) (not scroll-bar-mode)
			    (or (not (numberp flag)) (>= flag 0))))

    ;; Apply it to default-frame-alist.
    (mapcar
     (function
      (lambda (param-name)
	(let ((parameter (assq param-name default-frame-alist)))
	  (if (consp parameter)
	      (setcdr parameter scroll-bar-mode)
	    (setq default-frame-alist
		  (cons (cons param-name scroll-bar-mode)
			default-frame-alist))))))
     '(vertical-scroll-bars horizontal-scroll-bars))

    ;; Apply it to existing frames.
    (let ((frames (frame-list)))
      (while frames
	(modify-frame-parameters
	 (car frames)
	 (list (cons 'vertical-scroll-bars scroll-bar-mode)
	       (cons 'horizontal-scroll-bars scroll-bar-mode)))
	(setq frames (cdr frames))))))

;;;; Buffer navigation using the scroll bar.

;;; This was used for up-events on button 2, but no longer.
(defun scroll-bar-set-window-start (event)
  "Set the window start according to where the scroll bar is dragged.
EVENT should be a scroll bar click or drag event."
  (interactive "e")
  (let* ((end-position (event-end event))
	 (window (nth 0 end-position))
	 (portion-whole (nth 2 end-position)))
    (save-excursion
      (set-buffer (window-buffer window))
      (save-excursion
	(goto-char (+ (point-min)
		      (scroll-bar-scale portion-whole
					(- (point-max) (point-min)))))
	(beginning-of-line)
	(set-window-start window (point))))))

;; Scroll the window to the proper position for EVENT.
(defun scroll-bar-drag-1 (event)
  (let* ((start-position (event-start event))
	 (window (nth 0 start-position))
	 (portion-whole (nth 2 start-position)))
    (save-excursion
      (set-buffer (window-buffer window))
      ;; Calculate position relative to the accessible part of the buffer.
      (goto-char (+ (point-min)
		    (scroll-bar-scale portion-whole
				      (- (point-max) (point-min)))))
      (beginning-of-line)
      (set-window-start window (point)))))

(defun scroll-bar-drag (event)
  "Scroll the window by dragging the scroll bar slider.
If you click outside the slider, the window scrolls to bring the slider there."
  (interactive "e")
  (let* (done
	 (echo-keystrokes 0))
    (or point-before-scroll
	(setq point-before-scroll (point)))
    (scroll-bar-drag-1 event)
    (let (point-before-scroll)
      (track-mouse
	(while (not done)
	  (setq event (read-event))
	  (if (eq (car-safe event) 'mouse-movement)
	      (setq event (read-event)))
	  (cond ((eq (car-safe event) 'scroll-bar-movement)
		 (scroll-bar-drag-1 event))
		(t
		 ;; Exit when we get the drag event; ignore that event.
		 (setq done t))))))))

(defun scroll-bar-scroll-down (event)
  "Scroll the window's top line down to the location of the scroll bar click.
EVENT should be a scroll bar click."
  (interactive "e")
  (let ((old-selected-window (selected-window)))
    (unwind-protect
	(progn
	  (let* ((end-position (event-end event))
		 (window (nth 0 end-position))
		 (portion-whole (nth 2 end-position)))
	    (let (point-before-scroll)
	      (select-window window))
	    (or point-before-scroll
		(setq point-before-scroll (point)))
	    (let (point-before-scroll)
	      (scroll-down
	       (scroll-bar-scale portion-whole (1- (window-height)))))))
      (select-window old-selected-window))))

(defun scroll-bar-scroll-up (event)
  "Scroll the line next to the scroll bar click to the top of the window.
EVENT should be a scroll bar click."
  (interactive "e")
  (let ((old-selected-window (selected-window)))
    (unwind-protect
	(progn
	  (let* ((end-position (event-end event))
		 (window (nth 0 end-position))
		 (portion-whole (nth 2 end-position)))
	    (let (point-before-scroll)
	      (select-window window))
	    (or point-before-scroll
		(setq point-before-scroll (point)))
	    (let (point-before-scroll)
	      (scroll-up
	       (scroll-bar-scale portion-whole (1- (window-height)))))))
      (select-window old-selected-window))))


;;;; Bindings.

;;; For now, we'll set things up to work like xterm.
(global-set-key [vertical-scroll-bar mouse-1] 'scroll-bar-scroll-up)
(global-set-key [vertical-scroll-bar drag-mouse-1] 'scroll-bar-scroll-up)

(global-set-key [vertical-scroll-bar down-mouse-2] 'scroll-bar-drag)

(global-set-key [vertical-scroll-bar mouse-3] 'scroll-bar-scroll-down)
(global-set-key [vertical-scroll-bar drag-mouse-3] 'scroll-bar-scroll-down)


(provide 'scroll-bar)

;;; scroll-bar.el ends here
