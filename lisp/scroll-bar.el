;;; scrollbar.el -- window system-independent scrollbar support.

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

(require 'mouse)


;;;; Utilities.

(defun scrollbar-scale (num-denom whole)
  "Given a pair (NUM . DENOM) and WHOLE, return (/ (* NUM WHOLE) DENOM).
This is handy for scaling a position on a scrollbar into real units,
like buffer positions.  If SCROLLBAR-POS is the (PORTION . WHOLE) pair
from a scrollbar event, then (scrollbar-scale SCROLLBAR-POS
\(buffer-size)) is the position in the current buffer corresponding to
that scrollbar position."
  ;; We multiply before we divide to maintain precision.
  ;; We use floating point because the product of a large buffer size
  ;; with a large scrollbar portion can easily overflow a lisp int.
  (truncate (/ (* (float (car num-denom)) whole) (cdr num-denom))))


;;;; Helpful functions for enabling and disabling scroll bars.
(defvar scroll-bar-mode nil)

(defun scroll-bar-mode (flag)
  "Toggle display of vertical scroll bars on each frame.
This command applies to all frames that exist and frames to be
created in the future.
With a numeric argument, if the argument is negative,
turn off scroll bars; otherwise, turn on scroll bars."
  (interactive "P")
  (setq scroll-bar-mode (if (null flag) (not scroll-bar-mode)
			  (or (not (numberp flag)) (>= flag 0))))
  (mapcar
   (function
    (lambda (param-name)
      (let ((parameter (assq param-name default-frame-alist)))
	(if (consp parameter)
	    (setcdr parameter scroll-bar-mode)
	  (setq default-frame-alist
		(cons (cons param-name scroll-bar-mode)
		      default-frame-alist))))))
   '(vertical-scrollbars horizontal-scrollbars))
  (let ((frames (frame-list)))
    (while frames
      (modify-frame-parameters
       (car frames)
       (list (cons 'vertical-scrollbars scroll-bar-mode)
	     (cons 'horizontal-scrollbars scroll-bar-mode)))
      (setq frames (cdr frames)))))

;;;; Buffer navigation using the scrollbar.

(defun scrollbar-set-window-start (event)
  "Set the window start according to where the scrollbar is dragged.
EVENT should be a scrollbar click or drag event."
  (interactive "e")
  (let* ((end-position (event-end event))
	 (window (nth 0 end-position))
	 (portion-whole (nth 2 end-position)))
    (save-excursion
      (set-buffer (window-buffer window))
      (save-excursion
	(goto-char (scrollbar-scale portion-whole (buffer-size)))
	(beginning-of-line)
	(set-window-start window (point))))))

(defun scrollbar-scroll-down (event)
  "Scroll the window's top line down to the location of the scrollbar click.
EVENT should be a scrollbar click."
  (interactive "e")
  (let ((old-selected-window (selected-window)))
    (unwind-protect
	(progn
	  (let* ((end-position (event-end event))
		 (window (nth 0 end-position))
		 (portion-whole (nth 2 end-position)))
	    (select-window window)
	    (scroll-down
	     (scrollbar-scale portion-whole (1- (window-height))))))
      (select-window old-selected-window))))

(defun scrollbar-scroll-up (event)
  "Scroll the line next to the scrollbar click to the top of the window.
EVENT should be a scrollbar click."
  (interactive "e")
  (let ((old-selected-window (selected-window)))
    (unwind-protect
	(progn
	  (let* ((end-position (event-end event))
		 (window (nth 0 end-position))
		 (portion-whole (nth 2 end-position)))
	    (select-window window)
	    (scroll-up
	     (scrollbar-scale portion-whole (1- (window-height))))))
      (select-window old-selected-window))))


;;;; Bindings.

;;; For now, we'll set things up to work like xterm.
(global-set-key [vertical-scrollbar mouse-1] 'scrollbar-scroll-up)
(global-set-key [vertical-scrollbar drag-mouse-1] 'scrollbar-scroll-up)

(global-set-key [vertical-scrollbar mouse-2] 'scrollbar-set-window-start)
(global-set-key [vertical-scrollbar drag-mouse-2] 'scrollbar-set-window-start)
      
(global-set-key [vertical-scrollbar mouse-3] 'scrollbar-scroll-down)
(global-set-key [vertical-scrollbar drag-mouse-3] 'scrollbar-scroll-down)


(provide 'scroll-bar)

;;; scrollbar.el ends here
