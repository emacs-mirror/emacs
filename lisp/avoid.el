;;; avoid.el -- make mouse pointer stay out of the way of editing.

;;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@cs.rochester.edu>
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
;;;
;;; For those who are annoyed by the mouse pointer obscuring text,
;;; this mode moves the mouse pointer - either just a little out of
;;; the way, or all the way to the corner of the frame. 
;;; To use, load or evaluate this file and type M-x mouse-avoidance-mode .
;;; To set up permanently, put the following in your .emacs: 
;;;
;;; (if window-system (mouse-avoidance-mode 'animate))
;;;
;;; The 'animate can be 'jump or 'banish or 'exile or 'protean if you prefer.
;;; See the documentation for function `mouse-avoidance-mode' for
;;; details of the different modes.
;;;
;;; For added silliness, make the animatee animate...
;;; put something similar to the following into your .emacs:
;;;
;;; (if window-system
;;;     (mouse-avoidance-set-pointer-shape
;;;	     (eval (nth (random 4)
;;;			'(x-pointer-man x-pointer-spider
;;;			  x-pointer-gobbler x-pointer-gumby)))))
;;;
;;; For completely random pointer shape, replace the setq above with:
;;; (setq x-pointer-shape (mouse-avoidance-random-shape))
;;; 
;;; Bugs / Warnings / To-Do:
;;;
;;; - Using this code does slow emacs down.  "banish" mode shouldn't
;;;   be too bad, and on my workstation even "animate" is reasonable.
;;;
;;; - It ought to find out where any overlapping frames are and avoid them,
;;;   rather than always raising the frame.

;;; Credits:
;;; This code was helped by all those who contributed suggestions, 
;;;   fixes, and additions
;;; Joe Harrington (and his advisor), for the original inspiration.
;;; Ken Manheimer, for dreaming up the Protean mode.
;;; Richard Stallman, for the awful cat-and-mouse pun, among other things.
;;; Mike Williams, Denis Howe, Bill Benedetto, Chris Moore, Don Morris,
;;; Simon Marshall, and M.S. Ashton, for their feedback.
;;;
;;; Code:

(provide 'avoid)

(defvar mouse-avoidance-mode nil
  "Value is t or a symbol if the mouse pointer should avoid the cursor.
See function `mouse-avoidance-mode' for possible values.  Changing this
variable is NOT the recommended way to change modes; use that function 
instead.")

(defvar mouse-avoidance-nudge-dist 15
  "*Average distance that mouse will be moved when approached by cursor.
Only applies in mouse-avoidance-mode `jump' and its derivatives.
For best results make this larger than `mouse-avoidance-threshold'.")

(defvar mouse-avoidance-nudge-var 10
  "*Variability of `mouse-avoidance-nudge-dist' (which see).")

(defvar mouse-avoidance-animation-delay .01
  "Delay between animation steps, in seconds.")

(defvar mouse-avoidance-threshold 5
  "*Mouse-pointer's flight distance.
If the cursor gets closer than this, the mouse pointer will move away.
Only applies in mouse-avoidance-modes `animate' and `jump'.")

;; Internal variables
(defvar mouse-avoidance-state nil)
(defvar mouse-avoidance-pointer-shapes nil)
(defvar mouse-avoidance-n-pointer-shapes 0)

;;; Functions:

(defsubst mouse-avoidance-set-pointer-shape (shape)
  "Set the shape of the mouse pointer to SHAPE."
  (setq x-pointer-shape shape)
  (set-mouse-color nil))

(defun mouse-avoidance-point-position ()
  "Return the position of point as (FRAME X . Y).
Analogous to mouse-position."
  (let* ((w (selected-window))
	 (edges (window-edges w))
	 (list 
	  (compute-motion (max (window-start w) (point-min))   ; start pos
			  ;; window-start can be < point-min if the
			  ;; latter has changed since the last redisplay 
			  '(0 . 0)	                       ; start XY
			  (point)	                       ; stop pos
			  (cons (window-width) (window-height)); stop XY: none
			  (1- (window-width))                  ; width
			  (cons (window-hscroll w) 0)          ; 0 may not be right?
			  (selected-window))))
    ;; compute-motion returns (pos HPOS VPOS prevhpos contin)
    ;; we want:               (frame hpos . vpos)
    (cons (selected-frame)
	  (cons (+ (car edges)       (car (cdr list)))
		(+ (car (cdr edges)) (car (cdr (cdr list))))))))

;(defun mouse-avoidance-point-position-test ()
;  (interactive)
;  (message (format "point=%s mouse=%s" 
;		   (cdr (mouse-avoidance-point-position))
;		   (cdr (mouse-position)))))

(defun mouse-avoidance-set-mouse-position (pos)
  ;; Carefully set mouse position to given position (X . Y)
  ;; Ideally, should check if X,Y is in the current frame, and if not,
  ;; leave the mouse where it was.  However, this is currently
  ;; difficult to do, so we just raise the frame to avoid frame switches.
  ;; Returns t if it moved the mouse.
  (let ((f (selected-frame)))
    (raise-frame f)
    (set-mouse-position f (car pos) (cdr pos))
    t))
      
(defun mouse-avoidance-too-close-p (mouse)
  ;;  Return t if mouse pointer and point cursor are too close.
  ;; Acceptable distance is defined by mouse-avoidance-threshold.
  (let ((point (mouse-avoidance-point-position)))
    (and (eq (car mouse) (car point))
	 (car (cdr mouse))
	 (< (abs (- (car (cdr mouse)) (car (cdr point))))
	    mouse-avoidance-threshold)
	 (< (abs (- (cdr (cdr mouse)) (cdr (cdr point))))
	    mouse-avoidance-threshold))))

(defun mouse-avoidance-banish-destination ()
  "The position to which mouse-avoidance-mode `banish' moves the mouse.
You can redefine this if you want the mouse banished to a different corner."
 (cons (1- (frame-width))
       0))

(defun mouse-avoidance-banish-mouse ()
  ;; Put the mouse pointer in the upper-right corner of the current frame.
  (mouse-avoidance-set-mouse-position (mouse-avoidance-banish-destination)))

(defsubst mouse-avoidance-delta (cur delta dist var min max)
  ;; Decide how far to move in either dimension.
  ;; Args are the CURRENT location, the desired DELTA for
  ;; warp-conservation, the DISTANCE we like to move, the VARIABILITY
  ;; in distance allowed, and the MIN and MAX possible window positions.
  ;; Returns something as close to DELTA as possible withing the constraints.
  (let ((L1 (max (- min cur) (+ (- dist) (- var))))
	(R1                  (+ (- dist)    var ))
	(L2                  (+    dist  (- var)))
	(R2 (min (- max cur) (+    dist     var))))
    (if (< R1 (- min cur)) (setq L1 nil R1 nil))
    (if (> L2 (- max cur)) (setq L2 nil R2 nil))
    (cond ((and L1 (< delta L1)) L1)
	  ((and R1 (< delta R1)) delta)
	  ((and R1 (< delta 0)) R1)
	  ((and L2 (< delta L2)) L2)
	  ((and R2 (< delta R2)) delta)
	  (R2)
	  ((or R1 L2))
	  (t 0))))

(defun mouse-avoidance-nudge-mouse () 
  ;; Push the mouse a little way away, possibly animating the move
  ;; For these modes, state keeps track of the total offset that we've
  ;; accumulated, and tries to keep it close to zero.
  (let* ((cur (mouse-position))
	 (cur-frame (car cur))
	 (cur-pos (cdr cur))
	 (deltax (mouse-avoidance-delta 
		  (car cur-pos) (- (random mouse-avoidance-nudge-var)
				   (car mouse-avoidance-state))
		  mouse-avoidance-nudge-dist mouse-avoidance-nudge-var
		  0 (frame-width)))
	 (deltay (mouse-avoidance-delta 
		  (cdr cur-pos) (- (random mouse-avoidance-nudge-var)
				   (cdr mouse-avoidance-state))
		  mouse-avoidance-nudge-dist mouse-avoidance-nudge-var
		  0 (frame-height))))
    (setq mouse-avoidance-state
	  (cons (+ (car mouse-avoidance-state) deltax)
		(+ (cdr mouse-avoidance-state) deltay)))
    (if (or (eq mouse-avoidance-mode 'animate) 
	    (eq mouse-avoidance-mode 'proteus))
	(let ((i 0.0))
	  (while (<= i 1)
	    (mouse-avoidance-set-mouse-position 
	     (cons (+ (car cur-pos) (round (* i deltax)))
		   (+ (cdr cur-pos) (round (* i deltay)))))
    	    (setq i (+ i (max .1 (/ 1.0 mouse-avoidance-nudge-dist))))
	    (if (eq mouse-avoidance-mode 'proteus)
		(mouse-avoidance-set-pointer-shape 
		 (mouse-avoidance-random-shape)))
	    (sit-for mouse-avoidance-animation-delay)))
      (mouse-avoidance-set-mouse-position (cons (+ (car (cdr cur)) deltax)
						(+ (cdr (cdr cur)) deltay))))))

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

(defun mouse-avoidance-banish-hook ()
  (if (and (not executing-kbd-macro)	; don't check inside macro
	   (mouse-avoidance-kbd-command (this-command-keys)))
      (mouse-avoidance-banish-mouse)))

(defun mouse-avoidance-exile-hook ()
  ;; For exile mode, the state is nil when the mouse is in its normal
  ;; position, and set to the old mouse-position when the mouse is in exile.
  (if (and (not executing-kbd-macro)
	   (mouse-avoidance-kbd-command (this-command-keys)))
      (let ((mp (mouse-position)))
	(cond ((and (not mouse-avoidance-state)
		    (mouse-avoidance-too-close-p mp))
	       (setq mouse-avoidance-state mp)
	       (mouse-avoidance-banish-mouse))
	      ((and mouse-avoidance-state
		    (not (mouse-avoidance-too-close-p mouse-avoidance-state)))
	       (if (and (eq (car mp) (selected-frame))
			(equal (cdr mp) (mouse-avoidance-banish-destination)))
		   (mouse-avoidance-set-mouse-position
		    ;; move back only if user has not moved mouse
		    (cdr mouse-avoidance-state)))
	       ;; but clear state anyway, to be ready for another move
	       (setq mouse-avoidance-state nil))))))

(defun mouse-avoidance-fancy-hook ()
  ;; Used for the "fancy" modes, ie jump et al.
  (if (and (not executing-kbd-macro)	; don't check inside macro
	   (mouse-avoidance-kbd-command (this-command-keys))
	   (mouse-avoidance-too-close-p (mouse-position)))
      (let ((old-pos (mouse-position)))
	(mouse-avoidance-nudge-mouse)
	(if (not (eq (selected-frame) (car old-pos)))
	    ;; This should never happen.
	    (apply 'set-mouse-position old-pos)))))

(defun mouse-avoidance-kbd-command (key)
  "Return t if the KEYSEQENCE is composed of keyboard events only.
Return nil if there are any lists in the key sequence."
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

;;;###autoload
(defun mouse-avoidance-mode (&optional mode)
  "Set cursor avoidance mode to MODE.
MODE should be one of the symbols `banish', `exile', `jump', `animate',
`cat-and-mouse', `proteus', or `none'.

If MODE is nil, toggle mouse avoidance between `none` and `banish'
modes.  Positive numbers and symbols other than the above are treated
as equivalent to `banish'; negative numbers and `-' are equivalent to `none'.

Effects of the different modes: 
 * banish: Move the mouse to the upper-right corner on any keypress.
 * exile: Move the mouse to the corner only if the cursor gets too close,
     and allow it to return once the cursor is out of the way.
 * jump: If the cursor gets too close to the mouse, displace the mouse
     a random distance & direction.
 * animate: As `jump', but shows steps along the way for illusion of motion.
 * cat-and-mouse: Same as `animate'.
 * proteus: As `animate', but changes the shape of the mouse pointer too.

Whenever the mouse is moved, the frame is also raised.

\(see `mouse-avoidance-threshold' for definition of \"too close\",
and `mouse-avoidance-nudge-dist' and `mouse-avoidance-nudge-var' for
definition of \"random distance\".)"
  (interactive
   (list (intern (completing-read
		  "Select cursor avoidance technique (SPACE for list): "
		  '(("banish") ("exile") ("jump") ("animate")
		    ("cat-and-mouse") ("proteus") ("none"))
		  nil t))))
  (if (eq mode 'cat-and-mouse)
      (setq mode 'animate))
  (setq post-command-hook
	(delete 'mouse-avoidance-banish-hook (append post-command-hook nil)))
  (setq post-command-hook
	(delete 'mouse-avoidance-exile-hook (append post-command-hook nil)))
  (setq post-command-hook
	(delete 'mouse-avoidance-fancy-hook (append post-command-hook nil)))
  (cond	((eq mode 'none)
	 (setq mouse-avoidance-mode nil))
	((or (eq mode 'jump)
	     (eq mode 'animate)
	     (eq mode 'proteus))
	 (add-hook 'post-command-hook 'mouse-avoidance-fancy-hook)
	 (setq mouse-avoidance-mode mode
	       mouse-avoidance-state (cons 0 0)))
	((eq mode 'exile)
	 (add-hook 'post-command-hook 'mouse-avoidance-exile-hook)
	 (setq mouse-avoidance-mode mode
	       mouse-avoidance-state nil))
	((or (eq mode 'banish) 
	     (eq mode t)
	     (and (null mode) (null mouse-avoidance-mode))
	     (and mode (> (prefix-numeric-value mode) 0)))
	 (add-hook 'post-command-hook 'mouse-avoidance-banish-hook)
	 (setq mouse-avoidance-mode 'banish))
	(t (setq mouse-avoidance-mode nil)))
  (force-mode-line-update))

(or (assq 'mouse-avoidance-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(mouse-avoidance-mode " Avoid")
				 minor-mode-alist)))

;;; End of avoid.el
