;;; mldrag.el -- Mode line and vertical line dragging to resize windows.
;;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Kyle E. Jones <kyle@wonderworks.com>
;; Keywords: mouse

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

;;; Commentary:

;; This package lets you drag the modeline, vertical bar and
;; scrollbar to resize windows.  Suggested bindings are:
;;
;;   (global-set-key [mode-line down-mouse-1] 'mldrag-drag-mode-line)
;;   (global-set-key [vertical-line down-mouse-1] 'mldrag-drag-vertical-line)
;;   (global-set-key [vertical-scroll-bar S-down-mouse-1]
;;                   'mldrag-drag-vertical-line)
;;
;; Put the bindings and (require 'mldrag) in your .emacs file.

;;; Code:

(provide 'mldrag)

(defun mldrag-drag-mode-line (start-event)
  "Change the height of the current window with the mouse.
This command should be bound to a down-mouse- event, and is most
usefully bound with the `mode-line' prefix.  Holding down a mouse
button and moving the mouse up and down will make the clicked-on
window taller or shorter."
  (interactive "e")
  (let ((done nil)
	(echo-keystrokes 0)
	(start-event-frame (window-frame (car (car (cdr start-event)))))
	(start-event-window (car (car (cdr start-event))))
	(start-nwindows (count-windows t))
	(old-selected-window (selected-window))
	should-enlarge-minibuffer
	event mouse minibuffer y top bot edges wconfig params growth)
    (setq params (frame-parameters))
    (if (and (not (setq minibuffer (cdr (assq 'minibuffer params))))
	     (one-window-p t))
	(error "Attempt to resize sole window"))
    (unwind-protect
	(track-mouse
	  (progn
	    ;; enlarge-window only works on the selected window, so
	    ;; we must select the window where the start event originated.
	    ;; unwind-protect will restore the old selected window later.
	    (select-window start-event-window)
	    ;; if this is the bottommost ordinary window, then to
	    ;; move its modeline the minibuffer must be enlarged.
	    (setq should-enlarge-minibuffer
		  (and minibuffer
		       (not (one-window-p t))
		       (= (nth 1 (window-edges minibuffer))
			  (nth 3 (window-edges)))))
	    ;; loop reading events and sampling the position of
	    ;; the mouse.
	    (while (not done)
	      (setq event (read-event)
		    mouse (mouse-position))
	      ;; do nothing if
	      ;;   - there is a switch-frame event.
	      ;;   - the mouse isn't in the frame that we started in
	      ;;   - the mouse isn't in any Emacs frame
	      ;; drag if
	      ;;   - there is a mouse-movement event
	      ;;   - there is a scroll-bar-movement event
	      ;;     (same as mouse movement for our purposes)
	      ;; quit if
	      ;;   - there is a keyboard event or some other unknown event
	      ;;     unknown event.
	      (cond ((integerp event)
		     (setq done t))
		    ((eq (car event) 'switch-frame)
		     nil)
		    ((not (memq (car event)
				'(mouse-movement scroll-bar-movement)))
		     (setq done t))
		    ((not (eq (car mouse) start-event-frame))
		     nil)
		    ((null (car (cdr mouse)))
		     nil)
		    (t
		     (setq y (cdr (cdr mouse))
			   edges (window-edges)
			   top (nth 1 edges)
			   bot (nth 3 edges))
		     ;; scale back a move that would make the
		     ;; window too short.
		     (cond ((< (- y top -1) window-min-height)
			    (setq y (+ top window-min-height -1))))
		     ;; compute size change needed
		     (setq growth (- y bot -1)
			   wconfig (current-window-configuration))
		     ;; grow/shrink minibuffer?
		     (if should-enlarge-minibuffer
			 (progn
			   ;; yes.  briefly select minibuffer so
			   ;; enlarge-window will affect the
			   ;; correct window.
			   (select-window minibuffer)
			   ;; scale back shrinkage if it would
			   ;; make the minibuffer less than 1
			   ;; line tall.
			   (if (and (> growth 0)
				    (< (- (window-height minibuffer)
					  growth)
				       1))
			       (setq growth (1- (window-height minibuffer))))
			   (enlarge-window (- growth))
			   (select-window start-event-window))
		       ;; no.  grow/shrink the selected window
		       (enlarge-window growth))
		     ;; if this window's growth caused another
		     ;; window to be deleted because it was too
		     ;; short, rescind the change.
		     ;;
		     ;; if size change caused space to be stolen
		     ;; from a window above this one, rescind the
		     ;; change, but only if we didn't grow/srhink
		     ;; the minibuffer.  minibuffer size changes
		     ;; can cause all windows to shrink... no way
		     ;; around it.
		     (if (or (/= start-nwindows (count-windows t))
			     (and (not should-enlarge-minibuffer)
				  (/= top (nth 1 (window-edges)))))
			 (set-window-configuration wconfig)))))))
      ;; restore the old selected window
      (select-window old-selected-window))))

(defun mldrag-drag-vertical-line (start-event)
  "Change the width of the current window with the mouse.
This command should be bound to a down-mouse- event, and is most
usefully bound with the `vertical-line' or the `vertical-scroll-bar'
prefix.  Holding down a mouse button and moving the mouse left and
right will make the clicked-on window thinner or wider."
  (interactive "e")
  (let ((done nil)
	(echo-keystrokes 0)
	(start-event-frame (window-frame (car (car (cdr start-event)))))
	(start-event-window (car (car (cdr start-event))))
	(start-nwindows (count-windows t))
	(old-selected-window (selected-window))
	event mouse x left right edges wconfig growth)
    (if (one-window-p t)
	(error "Attempt to resize sole ordinary window"))
    (if (= (nth 2 (window-edges start-event-window))
	   (frame-width start-event-frame))
	(error "Attempt to drag rightmost scrollbar"))
    (unwind-protect
	(track-mouse
	  (progn
	    ;; enlarge-window only works on the selected window, so
	    ;; we must select the window where the start event originated.
	    ;; unwind-protect will restore the old selected window later.
	    (select-window start-event-window)
	    ;; loop reading events and sampling the position of
	    ;; the mouse.
	    (while (not done)
	      (setq event (read-event)
		    mouse (mouse-position))
	      ;; do nothing if
	      ;;   - there is a switch-frame event.
	      ;;   - the mouse isn't in the frame that we started in
	      ;;   - the mouse isn't in any Emacs frame
	      ;; drag if
	      ;;   - there is a mouse-movement event
	      ;;   - there is a scroll-bar-movement event
	      ;;     (same as mouse movement for our purposes)
	      ;; quit if
	      ;;   - there is a keyboard event or some other unknown event
	      ;;     unknown event.
	      (cond ((integerp event)
		     (setq done t))
		    ((eq (car event) 'switch-frame)
		     nil)
		    ((not (memq (car event)
				'(mouse-movement scroll-bar-movement)))
		     (setq done t))
		    ((not (eq (car mouse) start-event-frame))
		     nil)
		    ((null (car (cdr mouse)))
		     nil)
		    (t
		     (setq x (car (cdr mouse))
			   edges (window-edges)
			   left (nth 0 edges)
			   right (nth 2 edges))
		     ;; scale back a move that would make the
		     ;; window too thin.
		     (cond ((< (- x left -1) window-min-width)
			    (setq x (+ left window-min-width -1))))
		     ;; compute size change needed
		     (setq growth (- x right -1)
			   wconfig (current-window-configuration))
		     (enlarge-window growth t)
		     ;; if this window's growth caused another
		     ;; window to be deleted because it was too
		     ;; thin, rescind the change.
		     ;;
		     ;; if size change caused space to be stolen
		     ;; from a window to the left of this one,
		     ;; rescind the change.
		     (if (or (/= start-nwindows (count-windows t))
			     (/= left (nth 0 (window-edges))))
			 (set-window-configuration wconfig)))))))
      ;; restore the old selected window
      (select-window old-selected-window))))

;; mldrag.el ends here
