;;; window.el --- GNU Emacs window commands aside from those written in C.

;;; Copyright (C) 1985, 1989, 1992, 1993, 1994 Free Software Foundation, Inc.

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
     (walk-windows (function (lambda (w)
			       (setq count (+ count 1))))
		   minibuf)
     count))

(defun balance-windows ()
  "Makes all visible windows the same height (approximately)."
  (interactive)
  (let ((count -1) levels newsizes size)
    ;; Find all the different vpos's at which windows start,
    ;; then count them.  But ignore levels that differ by only 1.
    (save-window-excursion
      (let (tops (prev-top -2))
	(walk-windows (function (lambda (w)
				  (setq tops (cons (nth 1 (window-edges w))
						   tops))))
		      'nomini)
	(setq tops (sort tops '<))
	(while tops
	  (if (> (car tops) (1+ prev-top))
	      (setq prev-top (car tops)
		    count (1+ count)))
	  (setq levels (cons (cons (car tops) count) levels))
	  (setq tops (cdr tops)))
	(setq count (1+ count))))
    ;; Subdivide the frame into that many vertical levels.
    (setq size (/ (frame-height) count))
    (walk-windows (function
		   (lambda (w)
		     (select-window w)
		     (let ((newtop (cdr (assq (nth 1 (window-edges))
					      levels)))
			   (newbot (or (cdr (assq (+ (window-height)
						     (nth 1 (window-edges)))
						  levels))
				       count)))
		       (setq newsizes
			     (cons (cons w (* size (- newbot newtop)))
				   newsizes)))))
		  'nomini)
    (walk-windows (function (lambda (w)
			      (select-window w)
			      (let ((newsize (cdr (assq w newsizes))))
				(enlarge-window (- newsize
						   (window-height))))))
		  'nomini)))

;;; I think this should be the default; I think people will prefer it--rms.
(defvar split-window-keep-point t
  "*If non-nil, split windows keeps the original point in both children.
This is often more convenient for editing.
If nil, adjust point in each of the two windows to minimize redisplay.
This is convenient on slow terminals, but point can move strangely.")

(defun split-window-vertically (&optional arg)
  "Split current window into two windows, one above the other.
The uppermost window gets ARG lines and the other gets the rest.
Negative arg means select the size of the lowermost window instead.
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
	(size (and arg (prefix-numeric-value arg)))
	new-w bottom switch)
    (and size (< size 0) (setq size (+ (window-height) size)))
    (setq new-w (split-window nil size))
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
		(select-window new-w)))))
    new-w))

(defun split-window-horizontally (&optional arg)
  "Split current window into two windows side by side.
This window becomes the leftmost of the two, and gets ARG columns.
Negative arg means select the size of the rightmost window instead.
No arg means split equally."
  (interactive "P")
  (let ((size (and arg (prefix-numeric-value arg))))
    (and size (< size 0)
	 (setq size (+ (window-width) size)))
    (split-window nil size t)))

(defun enlarge-window-horizontally (arg)
  "Make current window ARG columns wider."
  (interactive "p")
  (enlarge-window arg t))

(defun shrink-window-horizontally (arg)
  "Make current window ARG columns narrower."
  (interactive "p")
  (shrink-window arg t))

(defun shrink-window-if-larger-than-buffer (&optional window)
  "Shrink the WINDOW to be as small as possible to display its contents.
Do not shrink to less than `window-min-height' lines.
Do nothing if the buffer contains more lines than the present window height,
or if some of the window's contents are scrolled out of view,
or if the window is not the full width of the frame,
or if the window is the only window of its frame."
  (interactive)
  (or window (setq window (selected-window)))
  (save-excursion
    (set-buffer (window-buffer window))
    (let* ((w (selected-window))	;save-window-excursion can't win
	   (buffer-file-name buffer-file-name)
	   (p (point))
	   (n 0)
	   (ignore-final-newline
	    ;; If buffer ends with a newline, ignore it when counting height
	    ;; unless point is after it.
	    (and (not (eobp))
		 (eq ?\n (char-after (1- (point-max))))))
	   (buffer-read-only nil)
	   (modified (buffer-modified-p))
	   (buffer (current-buffer))
	   (params (frame-parameters (window-frame window)))
	   (mini (cdr (assq 'minibuffer params)))
	   (edges (window-edges (selected-window))))
      (if (and (< 1 (let ((frame (selected-frame)))
		      (select-frame (window-frame window))
		      (unwind-protect
			  (count-windows)
			(select-frame frame))))
	       (= (window-width window) (frame-width (window-frame window)))
	       (pos-visible-in-window-p (point-min) window)
	       (not (eq mini 'only))
	       (or (not mini)
		   (< (nth 3 edges)
		      (nth 1 (window-edges mini)))
		   (> (nth 1 edges)
		      (cdr (assq 'menu-bar-lines params)))))
	  (unwind-protect
	      (progn
		(select-window (or window w))
		(goto-char (point-min))
		(while (pos-visible-in-window-p
			(- (point-max)
			   (if ignore-final-newline 1 0)))
		  ;; defeat file locking... don't try this at home, kids!
		  (setq buffer-file-name nil)
		  (insert ?\n) (setq n (1+ n)))
		(if (> n 0)
		    (shrink-window (min (1- n)
					(- (window-height)
					   window-min-height)))))
	    (delete-region (point-min) (point))
	    (set-buffer-modified-p modified)
	    (goto-char p)
	    (select-window w)
	    ;; Make sure we unbind buffer-read-only
	    ;; with the proper current buffer.
	    (set-buffer buffer))))))
      
(define-key ctl-x-map "2" 'split-window-vertically)
(define-key ctl-x-map "3" 'split-window-horizontally)
(define-key ctl-x-map "}" 'enlarge-window-horizontally)
(define-key ctl-x-map "{" 'shrink-window-horizontally)
(define-key ctl-x-map "-" 'shrink-window-if-larger-than-buffer)
(define-key ctl-x-map "+" 'balance-windows)

;;; windows.el ends here
