;;; hscroll.el: Minor mode to automatically scroll truncated lines horizontally
;;; Copyright (C) 1992, 1993, 1995, 1996 Free Software Foundation, Inc.

;; Author: Wayne Mesard <wmesard@esd.sgi.com>
;; Keywords: display

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:a
;;
;;    Automatically scroll horizontally when the point moves off the
;;    left or right edge of the window.  
;;
;;    - Type "M-x hscroll-mode" to enable it in the current buffer.
;;    - Type "M-x hscroll-global-mode" to enable it in every buffer.
;;    - "turn-on-hscroll" is useful in mode hooks as in:
;;          (add-hook 'text-mode-hook 'turn-on-hscroll)
;;
;;    - hscroll-margin controls how close the cursor can get to the edge 
;;      of the window.
;;    - hscroll-step-percent controls how far to jump once we decide to do so.
;;
;;    Most users won't want to mess with the other variables defined
;;    here.  But they're all documented, and they all start with
;;    "hscroll-" if you're curious.
;;
;;    Oh, you should also know that if you set the hscroll-margin and
;;    hscroll-step-percent large enough, you can get an interesting, but
;;    undesired ping-pong effect as the point bounces from one edge to
;;    the other.
;;
;;    wmesard@sgi.com

;;; Code:

;;; 
;;; PUBLIC VARIABLES
;;; 

(defvar hscroll-version "2.2")

(defvar hscroll-margin 5 
  "*How many columns away from the edge of the window point is allowed to get
before HScroll will horizontally scroll the window.")

(defvar hscroll-snap-threshold 30
  "*When point is this many columns (or less) from the left edge of the document, 
don't do any horizontal scrolling.  In other words, be biased towards the left
edge of the document.
  Set this variable to zero to disable this bias.")

(defvar hscroll-step-percent 25
  "*How far away to place the point from the window's edge when scrolling.
Expressed as a percentage of the window's width.")

(defvar hscroll-mode-name " Hscr"
  "*Horizontal scrolling mode line indicator.
Set this to nil to conserve valuable mode line space.")

(or (assq 'hscroll-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(hscroll-mode hscroll-mode-name) minor-mode-alist)))


;;; 
;;; PRIVATE VARIABLES
;;; 

(defvar hscroll-mode nil 
  "Non-nil if HScroll mode is enabled.")
(make-variable-buffer-local 'hscroll-mode)


(defvar hscroll-old-truncate-local nil)
(defvar hscroll-old-truncate-was-global nil)
(make-variable-buffer-local 'hscroll-old-truncate)
(make-variable-buffer-local 'hscroll-old-truncate-was-global)

(defvar hscroll-old-truncate-default nil)

;;; 
;;; PUBLIC COMMANDS
;;; 

;;;###autoload
(defun turn-on-hscroll ()
  "Unconditionally turn on Hscroll mode in the current buffer."
  (hscroll-mode 1))

;;;###autoload
(defun hscroll-mode (&optional arg)
  "Toggle HScroll mode in the current buffer.
With ARG, turn HScroll mode on if ARG is positive, off otherwise.
In HScroll mode, truncated lines will automatically scroll left or
right when point gets near either edge of the window.
  See also \\[hscroll-global-mode]."
  (interactive "P")
  (make-local-hook 'post-command-hook)
  (let ((newmode (if (null arg)
		      (not hscroll-mode)
		    (> (prefix-numeric-value arg) 0))))

    (if newmode
	;; turn it on
	(if (not hscroll-mode)
	    ;; it was off
	    (let ((localp (local-variable-p 'truncate-lines)))
	      (if localp
		  (setq hscroll-old-truncate-local truncate-lines))
	      (setq hscroll-old-truncate-was-global (not localp))
	      (setq truncate-lines t)
	      (add-hook 'post-command-hook 
			(function hscroll-window-maybe) nil t)
	      ))
      ;; turn it off
      (if hscroll-mode
	  ;; it was on
	  (progn
	    (if hscroll-old-truncate-was-global
		(kill-local-variable 'truncate-lines)
	      (setq truncate-lines hscroll-old-truncate-local))
	    (if (not truncate-lines)
		(set-window-hscroll (selected-window) 0))
	    (remove-hook 'post-command-hook
			 (function hscroll-window-maybe) t)
	    ))
      )

    (setq hscroll-mode newmode)
    (force-mode-line-update nil)
    ))


;;;###autoload
(defun hscroll-global-mode  (&optional arg)
  "Toggle HScroll mode in all buffers.
With ARG, turn HScroll mode on if ARG is positive, off otherwise.
If a buffer ever has HScroll mode set locally (via \\[hscroll-mode]), 
it will forever use the local value (i.e., \\[hscroll-global-mode] 
will have no effect on it).
  See also \\[hscroll-mode]."
  (interactive "P")
  (let* ((oldmode (default-value 'hscroll-mode))
	 (newmode (if (null arg)
		      (not oldmode)
		    (> (prefix-numeric-value arg) 0))))

    (if newmode
	;; turn it on
	(if (not hscroll-mode)
	    ;; it was off
	    (progn
	      (setq hscroll-old-truncate-default (default-value truncate-lines))
	      (setq hscroll-old-truncate-was-global t)
	      (setq-default truncate-lines t)
	      (add-hook 'post-command-hook (function hscroll-window-maybe))
	      ))
      ;; turn it off
      (if hscroll-mode
	  ;; it was on
	  (progn
	    (setq-default truncate-lines hscroll-old-truncate-default)
	    (remove-hook 'post-command-hook (function hscroll-window-maybe))
	    ))
      )

    (setq-default hscroll-mode newmode)
    (force-mode-line-update t)
    ))

(defun hscroll-window-maybe ()
  "Scroll horizontally if point is off or nearly off the edge of the window.
This is called automatically when in HScroll mode, but it can be explicitly
invoked as well (i.e., it can be bound to a key)."
  (interactive)
  ;; Only consider scrolling if truncate-lines is true, 
  ;; the window is already scrolled or partial-widths is true and this is
  ;; a partial width window.  See display_text_line() in xdisp.c.
  (if (and hscroll-mode
	   (or truncate-lines
	       (not (zerop (window-hscroll)))
	       (and truncate-partial-width-windows
		    (< (window-width) (frame-width)))))
      (let ((linelen (save-excursion (end-of-line) (current-column)))
	    (rightmost-char (+ (window-width) (window-hscroll)))
	    )
 	(if (< (current-column) hscroll-snap-threshold)
 	    (set-window-hscroll 
 	     (selected-window) 
 	     (- (window-hscroll)))
 	  (if (>= (current-column)
		(- rightmost-char hscroll-margin
		   ;; Off-by-one if the left edge is scrolled
		   (if (not (zerop (window-hscroll))) 1 0)
		   ;; Off by one if the right edge is scrolled
		   (if (> linelen rightmost-char) 1 0)
		   ))
	    ;; Scroll to the left a proportion of the window's width.
	    (set-window-hscroll 
	     (selected-window) 
	     (- (+ (current-column) 
		   (/ (* (window-width) hscroll-step-percent) 100))
		(window-width)))
	  (if (< (current-column) (+ (window-hscroll) hscroll-margin))
	      ;; Scroll to the right a proportion of the window's width.
	      (set-window-hscroll
	       (selected-window)
	       (- (current-column) (/ (* (window-width) hscroll-step-percent) 100)))
	    )))
	)))

;;; 
;;; It's not a bug, it's a *feature*
;;; 

(provide 'hscroll)

;;; hscroll.el ends here
