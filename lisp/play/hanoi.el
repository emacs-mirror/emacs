;;; hanoi.el --- towers of hanoi in GNUmacs

;; Author: Damon Anton Permezel
;; Maintainer: FSF
;; Keywords: games

; Author (a) 1985, Damon Anton Permezel
; This is in the public domain
; since he distributed it without copyright notice in 1985.

;;; Commentary:

;; Solves the Towers of Hanoi puzzle while-U-wait.
;;
;; The puzzle: Start with N rings, decreasing in sizes from bottom to
;; top, stacked around a post.  There are two other posts.  Your mission,
;; should you choose to accept it, is to shift the pile, stacked in its
;; original order, to another post.
;;
;; The challenge is to do it in the fewest possible moves.  Each move
;; shifts one ring to a different post.  But there's a rule; you can
;; only stack a ring on top of a larger one.
;;
;; The simplest nontrivial version of this puzzle is N = 3.  Solution
;; time rises as 2**N, and programs to solve it have long been considered
;; classic introductory exercises in the use of recursion.
;;
;; The puzzle is called `Towers of Hanoi' because an early popular
;; presentation wove a fanciful legend around it.  According to this
;; myth (uttered long before the Vietnam War), there is a Buddhist
;; monastery at Hanoi which contains a large room with three time-worn
;; posts in it surrounded by 21 golden discs.  Monks, acting out the
;; command of an ancient prophecy, have been moving these disks, in
;; accordance with the rules of the puzzle, once every day since the
;; monastery was founded over a thousand years ago.  They are said
;; believe that when the last move of the puzzle is completed, the
;; world will end in a clap of thunder.  Fortunately, they are nowhere
;; even close to being done...

;;; Code:

;;;
;;; hanoi-topos - direct cursor addressing
;;;
(defun hanoi-topos (row col)
  (goto-line row)
  (beginning-of-line)
  (forward-char col))

;;;
;;; hanoi - user callable Towers of Hanoi
;;;
;;;###autoload
(defun hanoi (nrings)
  "Towers of Hanoi diversion.  Argument is number of rings."
  (interactive "p")
  (if (<= nrings 1) (setq nrings 7))
  (let* (floor-row
	 fly-row
	 (window-height (1- (window-height (selected-window))))
	 (window-width (window-width (selected-window)))

	 ;; This is half the spacing to use between poles.
	 (pole-spacing (/ window-width 6)))
    (if (not (and (> window-height (1+ nrings))
		  (> pole-spacing nrings)))
	(progn
	  (delete-other-windows)
	  (if (not (and (> (setq window-height
				 (1- (window-height (selected-window))))
			   (1+ nrings))
			(> (setq pole-spacing (/ window-width 6))
			   nrings)))
	      (error "Window is too small (need at least %dx%d)"
		     (* 6 (1+ nrings)) (+ 2 nrings)))))
    (setq floor-row (if (> (- window-height 3) (1+ nrings))
			(- window-height 3) window-height))
    (let ((fly-row (- floor-row nrings 1))
	  ;; pole: column . fill height
	  (pole-1 (cons (1- pole-spacing) floor-row))
	  (pole-2 (cons (1- (* 3 pole-spacing)) floor-row))
	  (pole-3 (cons (1- (* 5 pole-spacing)) floor-row))
	  (rings (make-vector nrings nil)))
      ;; construct the ring list
      (let ((i 0))
	(while (< i nrings)
	  ;; ring: [pole-number string empty-string]
	  (aset rings i (vector nil
				(make-string (+ i i 3) (+ ?0 (% i 10)))
				(make-string (+ i i 3) ?\  )))
	  (setq i (1+ i))))
      ;;
      ;; init the screen
      ;;
      (switch-to-buffer "*Hanoi*")
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (let ((i 0))
	(while (< i floor-row)
	  (setq i (1+ i))
	  (insert-char ?\  (1- window-width))
	  (insert ?\n)))
      (insert-char ?= (1- window-width))

      (let ((n 1))
	(while (< n 6)
	  (hanoi-topos fly-row (1- (* n pole-spacing)))
	  (setq n (+ n 2))
	  (let ((i fly-row))
	    (while (< i floor-row)
	      (setq i (1+ i))
	      (next-line 1)
	      (insert ?\|)
	      (delete-char 1)
	      (backward-char 1)))))
      ;(sit-for 0)
      ;;
      ;; now draw the rings in their initial positions
      ;;
      (let ((i 0)
	    ring)
	(while (< i nrings)
	  (setq ring (aref rings (- nrings 1 i)))
	  (aset ring 0 (- floor-row i))
	  (hanoi-topos (cdr pole-1)
		       (- (car pole-1) (- nrings i)))
	  (hanoi-draw-ring ring t nil)
	  (setcdr pole-1 (1- (cdr pole-1)))
	  (setq i (1+ i))))
      (setq buffer-read-only t)
      (sit-for 0)
      ;; Disable display of line and column numbers, for speed.
      (let ((line-number-mode nil)
	    (column-number-mode nil))
	;; do it!
	(hanoi0 (1- nrings) pole-1 pole-2 pole-3))
      (goto-char (point-min))
      (message "Done")
      (setq buffer-read-only t)
      (force-mode-line-update)
      (sit-for 0))))

;;;
;;; hanoi0 - work horse of hanoi
;;;
(defun hanoi0 (n from to work)
  (cond ((input-pending-p)
	 (signal 'quit (list "I can tell you've had enough")))
	((< n 0))
	(t
	 (hanoi0 (1- n) from work to)
	 (hanoi-move-ring n from to)
	 (hanoi0 (1- n) work to from))))

;;;
;;; hanoi-move-ring - move ring 'n' from 'from' to 'to'
;;;
;;;
(defun hanoi-move-ring (n from to)
  (let ((ring (aref rings n))		; ring <- ring: (ring# . row)
	(buffer-read-only nil))
    (let ((row (aref ring 0))		; row <- row ring is on
	  (col (- (car from) n 1))	; col <- left edge of ring
	  (dst-col (- (car to) n 1))	; dst-col <- dest col for left edge
	  (dst-row (cdr to)))		; dst-row <- dest row for ring
      (hanoi-topos row col)
      (while (> row fly-row)		; move up to the fly row
	(hanoi-draw-ring ring nil t)	; blank out ring
	(previous-line 1)		; move up a line
	(hanoi-draw-ring ring t nil)	; redraw
	(sit-for 0)
	(setq row (1- row)))
      (setcdr from (1+ (cdr from)))	; adjust top row
      ;;
      ;; fly the ring over to the right pole
      ;;
      (while (not (equal dst-col col))
	(cond ((> dst-col col)		; dst-col > col: right shift
	       (end-of-line 1)
	       (delete-backward-char 2)
	       (beginning-of-line 1)
	       (insert ?\  ?\  )
	       (sit-for 0)
	       (setq col (1+ (1+ col))))
	      ((< dst-col col)		; dst-col < col: left shift
	       (beginning-of-line 1)
	       (delete-char 2)
	       (end-of-line 1)
	       (insert ?\  ?\  )
	       (sit-for 0)
	       (setq col (1- (1- col))))))
      ;;
      ;; let the ring float down
      ;;
      (hanoi-topos fly-row dst-col)
      (while (< row dst-row)		; move down to the dest row
	(hanoi-draw-ring ring nil (> row fly-row)) ; blank out ring
	(next-line 1)			; move down a line
	(hanoi-draw-ring ring t nil)	; redraw ring
	(sit-for 0)
	(setq row (1+ row)))
      (aset ring 0 dst-row)
      (setcdr to (1- (cdr to))))))	; adjust top row

;;;
;;; draw-ring -	draw the ring at point, leave point unchanged
;;;
;;; Input:
;;;	ring
;;;	f1	-	flag: t -> draw, nil -> erase
;;;	f2	-	flag: t -> erasing and need to draw ?\|
;;;
(defun hanoi-draw-ring (ring f1 f2)
  (save-excursion
    (let* ((string (if f1 (aref ring 1) (aref ring 2)))
	   (len (length string)))
      (delete-char len)
      (insert string)
      (if f2
	  (progn
	    (backward-char (/ (+ len 1) 2))
	    (delete-char 1) (insert ?\|))))))

(provide 'hanoi)

;;; hanoi.el ends here
