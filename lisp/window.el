;;; windows.el --- GNU Emacs window commands aside from those written in C.

;;; Copyright (C) 1985, 1989, 1992 Free Software Foundation, Inc.

;; Maintainer: FSF

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

;;; Code:

(defun count-windows (&optional minibuf)
   "Returns the number of visible windows.
Optional arg NO-MINI non-nil means don't count the minibuffer
even if it is active."
   (let ((count 0))
     (walk-windows (function (lambda ()
			       (setq count (+ count 1))))
		   minibuf)
     count))

(defun balance-windows ()
  "Makes all visible windows the same size (approximately)."
  (interactive)
  (let ((count 0))
    (walk-windows (function (lambda (w)
			      (setq count (+ count 1))))
		  'nomini)
    (let ((size (/ (frame-height) count)))
      (walk-windows (function (lambda (w)
				(select-window w)
				(enlarge-window (- size (window-height)))))
		    'nomini))))

;;; I think this should be the default; I think people will prefer it--rms.

(defvar split-window-keep-point t
  "*If non-nil, split windows so that both windows keep the original
value of point.  This is often more convenient for editing.
If nil, split windows to minimize redisplay.  This is convenient on
slow terminals, but point may be moved strangely to accommodate the
redisplay.")

(defun split-window-vertically (&optional arg)
  "Split current window into two windows, one above the other.
The uppermost window gets ARG lines and the other gets the rest.
With no argument, split equally or close to it.
Both windows display the same buffer now current.

If the variable split-window-keep-point is non-nil, both new windows
will get the same value of point as the current window.  This is often
more convenient for editing.

Otherwise, we chose window starts so as to minimize the amount of
redisplay; this is convenient on slow terminals.  The new selected
window is the one that the current value of point appears in.  The
value of point can change if the text around point is hidden by the
new mode line."
  (interactive "P")
  (let ((old-w (selected-window))
	(old-point (point))
	new-w bottom switch)
    (setq new-w (split-window nil (and arg (prefix-numeric-value arg))))
    (or split-window-keep-point
	(progn
	  (save-excursion
	    (set-buffer (window-buffer))
	    (goto-char (window-start))
	    (vertical-motion (window-height))
	    (set-window-start new-w (point))
	    (if (> (point) (window-point new-w))
		(set-window-point new-w (point)))
	    (vertical-motion -1)
	    (setq bottom (point)))
	  (if (<= bottom (point))
	      (set-window-point old-w (1- bottom)))
	  (if (< (window-start new-w) old-point)
	      (progn
		(set-window-point new-w old-point)
		(select-window new-w)))))))

(defun split-window-horizontally (&optional arg)
  "Split current window into two windows side by side.
This window becomes the leftmost of the two, and gets
ARG columns.  No arg means split equally."
  (interactive "P")
  (split-window nil (and arg (prefix-numeric-value arg)) t))

(defun enlarge-window-horizontally (arg)
  "Make current window ARG columns wider."
  (interactive "p")
  (enlarge-window arg t))

(defun shrink-window-horizontally (arg)
  "Make current window ARG columns narrower."
  (interactive "p")
  (shrink-window arg t))

(defun shrink-window-if-larger-than-buffer (&optional window)
  "Shrink the WINDOW to be as small as possible to display its contents.  Do
nothing if only one window is displayed or if the buffer contains more lines
than the present window height."
  (save-excursion
    (set-buffer (window-buffer window))
    (let ((w (selected-window)) ;save-window-excursion can't win
	  (buffer-file-name buffer-file-name)
	  (p (point))
	  (n 0)
	  (window-min-height 0)
	  (buffer-read-only nil)
	  (modified (buffer-modified-p))
	  (buffer (current-buffer)))
      (unwind-protect
	  (progn
	    (select-window (or window w))
	    (goto-char (point-min))
	    (while (pos-visible-in-window-p (point-max))
	      ;; defeat file locking... don't try this at home, kids!
	      (setq buffer-file-name nil)
	      (insert ?\n) (setq n (1+ n)))
	    (if (> n 0) (shrink-window (1- n))))
	(delete-region (point-min) (point))
	(set-buffer-modified-p modified)
	(goto-char p)
	(select-window w)
	;; Make sure we unbind buffer-read-only
	;; with the proper current buffer.
	(set-buffer buffer)))))
      
(define-key ctl-x-map "2" 'split-window-vertically)
(define-key ctl-x-map "3" 'split-window-horizontally)
(define-key ctl-x-map "}" 'enlarge-window-horizontally)
(define-key ctl-x-map "{" 'shrink-window-horizontally)
(define-key ctl-x-map "-" 'shrink-window-if-larger-than-buffer)
(define-key ctl-x-map "+" 'balance-windows)

;;; windows.el ends here
