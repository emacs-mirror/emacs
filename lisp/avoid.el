;;; avoid.el -- make mouse pointer stay out of the way of editing.

;;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@cs.rochester.edu>
;; Keywords: mouse
;; Version: 1.10

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
;;;
;;; For those who are annoyed by the mouse pointer obscuring text,
;;; this mode moves the mouse pointer - either just a little out of
;;; the way, or all the way to the corner of the frame. 
;;; To use, load or evaluate this file and type M-x mouse-avoidance-mode .
;;; To set up permanently, put this file on your load-path and put the
;;; following in your .emacs: 
;;;
;;; (cond (window-system
;;;        (require 'avoid)
;;;        (mouse-avoidance-mode 'cat-and-mouse)))
;;;
;;; The 'animate can be 'jump or 'banish or 'protean if you prefer.
;;;
;;; For added silliness, make the animatee animate...
;;; put something similar to the following into your .emacs:
;;;
;;; (cond (window-system
;;;       (setq x-pointer-shape 
;;;	     (eval (nth (random 4)
;;;			'(x-pointer-man x-pointer-spider
;;;			  x-pointer-gobbler x-pointer-gumby))))
;;;       (set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))))
;;;
;;; For completely random pointer shape, replace the setq above with:
;;; (setq x-pointer-shape (mouse-avoidance-random-shape))
;;; 
;;; Bugs & Warnings:
;;;
;;; - THIS CODE IS FOR USE WITH FSF EMACS 19.21 or later.
;;;   It can cause earlier versions of emacs to crash, due to a bug in the
;;;   mouse code. 
;;;
;;; - Using this code does slow emacs down.  "banish" mode shouldn't
;;;   ever be too bad though, and on my workstation even "animate" doesn't
;;;   seem to have a noticable effect.
;;;
;;; - There are some situations where it doesn't do what you expect,
;;;   notably when there are long wrapped lines in the buffer.  Since
;;;   there is no low-level command for finding point's position
;;;   on the screen, it can fail to move the pointer when on such a line.

;;; Credits:
;;; This code was helped by all those who contributed suggestions, fixes, and 
;;; additions:
;;; Joe Harrington (and his advisor), for the original inspiration
;;; Ken Manheimer, for dreaming up the Protean mode
;;; Richard Stallman, for the awful cat-and-mouse pun, among other things
;;; Mike Williams, Denis Howe, Bill Benedetto, Chris Moore, Don Morris,
;;; Simon Marshall, and M.S. Ashton, for their feedback.
;;;
;;; Code:

(provide 'avoid)

(defvar mouse-avoidance-mode nil
  "Value is t or a symbol if the mouse pointer should avoid the cursor.
See function mouse-avoidance-mode for possible values.  Changing this
variable is NOT the recommended way to change modes; use the function 
instead.")

(defvar mouse-avoidance-nudge-dist 4
  "*Average distance that mouse will be moved when approached by cursor.
Only applies in mode-avoidance-modes `animate' and `jump'.")

(defvar mouse-avoidance-nudge-var 3
  "*Variability of mouse-avoidance-nudge-dist (which see).")

(defvar mouse-avoidance-animation-delay .01
  "Delay between animation steps, in seconds.")

(defvar mouse-avoidance-threshhold 5
  "*Mouse-pointer's flight distance.
If the cursor gets closer than this, the mouse pointer will move away.
Only applies in mouse-avoidance-modes `animate' and `jump'.")

;; Internal variables for mouse-avoidance-random-shape
(defvar mouse-avoidance-pointer-shapes nil)
(defvar mouse-avoidance-n-pointer-shapes 0)

;;; Functions:

(defun mouse-avoidance-too-close-p ()
  ;;  Return t if mouse pointer and point cursor are too close.
  ;; Acceptable distance is defined by mouse-avoidance-threshhold.
  (let ((mouse (mouse-position)))
    (and (car (cdr mouse))
	 (< (abs (- (car (cdr mouse)) (current-column)))
	    mouse-avoidance-threshhold)
	 (< (abs (- (cdr (cdr mouse)) 
		    (+ (car (cdr (window-edges)))
		       (count-lines (window-start) (point)))))
	    mouse-avoidance-threshhold))))

(defun mouse-avoidance-banish-mouse ()
  ;; Put the mouse pointer in the upper-right corner of the current frame.
  (set-mouse-position (selected-frame) (1- (frame-width)) 0))

(defun mouse-avoidance-nudge-mouse () 
  ;; Push the mouse a little way away, possibly animating the move
  (let* ((cur (mouse-position))
	 (deltax (* (+ mouse-avoidance-nudge-dist 
		       (random mouse-avoidance-nudge-var))
		    (if (zerop (random 2)) 1 -1)))
	 (deltay (* (+ mouse-avoidance-nudge-dist
		       (random mouse-avoidance-nudge-var))
		    (if (zerop (random 2)) 1 -1))))
    (if (or (eq mouse-avoidance-mode 'animate) 
	    (eq mouse-avoidance-mode 'proteus))
	(let ((i 0.0)
	      (color (cdr (assoc 'mouse-color (frame-parameters)))))
	  (while (<= i 1)
	    (set-mouse-position 
	     (car cur) 
	     (mod (+ (car (cdr cur)) (round (* i deltax))) (window-width))
	     (mod (+ (cdr (cdr cur)) (round (* i deltay))) (window-height)))
	    (setq i (+ i (/ 1.0 mouse-avoidance-nudge-dist)))
	    (if (eq mouse-avoidance-mode 'proteus)
		(progn
		  (setq x-pointer-shape (mouse-avoidance-random-shape))
		  (set-mouse-color color)))
	    (sit-for mouse-avoidance-animation-delay)))
      (set-mouse-position 
       (car cur)
       (mod (+ (car (cdr cur)) deltax) (window-width))
       (mod (+ (cdr (cdr cur)) deltay) (window-height))))))

(defun mouse-avoidance-random-shape ()
  "Return a random cursor shape.
This assumes that any variable whose name begins with x-pointer- and
has an integer value is a valid cursor shape.  You might want to
redefine this function to suit your own tastes."
  (if (null mouse-avoidance-pointer-shapes)
      (progn
	(setq mouse-avoidance-pointer-shapes
	      (mapcar '(lambda (x) (symbol-value (intern x)))
		      (all-completions "x-pointer-" obarray
				       '(lambda (x) 
					  (and (boundp x)
					       (integerp (symbol-value x)))))))
	(setq mouse-avoidance-n-pointer-shapes 
	      (length mouse-avoidance-pointer-shapes))))
  (nth (random mouse-avoidance-n-pointer-shapes)
       mouse-avoidance-pointer-shapes))

(defun mouse-avoidance-simple-hook ()
  (if (and (mouse-avoidance-keyboard-command (this-command-keys)))
      (mouse-avoidance-banish-mouse)))

(defun mouse-avoidance-fancy-hook ()
  (if (and (mouse-avoidance-keyboard-command (this-command-keys))
	   (mouse-avoidance-too-close-p))
      (mouse-avoidance-nudge-mouse)))

(defun mouse-avoidance-keyboard-command (key)
  "Return t if the KEYSEQENCE is composed of keyboard events only.
Returns nil if there are any lists in the key sequence."
  (cond ((null key) nil)		; Null event seems to be
					; returned occasionally.
	((not (vectorp key)) t)		; Strings are keyboard events.
	((catch 'done
	   (let ((i 0)
		 (l (length key)))
	     (while (< i l)
	       (if (listp (aref key i))
		   (throw 'done nil))
	       (setq i (1+ i))))
	   t))))

(defun mouse-avoidance-mode (&optional mode)
  "Set cursor avoidance mode to MODE.
MODE should be one of the symbols `banish', `jump', `animate',
`cat-and-mouse', or `none'.  `Animate' is the same as `cat-and-mouse'.
If MODE is nil, toggle mouse avoidance.  Positive numbers and
symbols other than the above are treated as equivalent to `banish';
negative numbers and `-' are equivalent to `none'."
  (interactive
   (list (intern (completing-read
		  "Select cursor avoidance technique (SPACE for list): "
		  '(("banish") ("jump") ("animate") ("cat-and-mouse") 
		    ("proteus") ("none"))
		  nil t))))
  (if (eq mode 'cat-and-mouse)
      (setq mode 'animate))
  (setq post-command-hook
	(delete 'mouse-avoidance-simple-hook (append post-command-hook nil)))
  (setq post-command-hook
	(delete 'mouse-avoidance-fancy-hook (append post-command-hook nil)))
  (cond	((eq mode 'none)
	 (setq mouse-avoidance-mode nil))
	((or (eq mode 'jump)
	     (eq mode 'animate)
	     (eq mode 'proteus))
	 (add-hook 'post-command-hook 'mouse-avoidance-fancy-hook)
	 (setq mouse-avoidance-mode mode))
	((or (eq mode 'banish) 
	     (eq mode t)
	     (and (null mode) (null mouse-avoidance-mode))
	     (and mode (> (prefix-numeric-value mode) 0)))
	 (add-hook 'post-command-hook 'mouse-avoidance-simple-hook)
	 (setq mouse-avoidance-mode 'banish))
	(t (setq mouse-avoidance-mode nil))))

(or (assq 'mouse-avoidance-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(mouse-avoidance-mode " Avoid")
				 minor-mode-alist)))

;;; End of avoid.el

