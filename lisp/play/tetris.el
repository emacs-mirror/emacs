;;; tetris.el --- implementation of Tetris for Emacs

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Version: 2.01
;; Created: 1997-08-13
;; Keywords: games

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

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'gamegrid)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup tetris nil
  "Play a game of tetris."
  :prefix "tetris-"
  :group 'games)

(defcustom tetris-use-glyphs t
  "*Non-nil means use glyphs when available."
  :group 'tetris
  :type 'boolean)

(defcustom tetris-use-color t
  "*Non-nil means use color when available."
  :group 'tetris
  :type 'boolean)

(defcustom tetris-draw-border-with-glyphs t
  "*Non-nil means draw a border even when using glyphs."
  :group 'tetris
  :type 'boolean)

(defcustom tetris-default-tick-period 0.3
  "*The default time taken for a shape to drop one row."
  :group 'tetris
  :type 'number)

(defcustom tetris-update-speed-function
  'tetris-default-update-speed-function
  "Function run whenever the Tetris score changes
Called with two arguments: (SHAPES ROWS)
SHAPES is the number of shapes which have been dropped
ROWS is the number of rows which have been completed

If the return value is a number, it is used as the timer period."
  :group 'tetris
  :type 'function)

(defcustom tetris-mode-hook nil
  "Hook run upon starting Tetris."
  :group 'tetris
  :type 'hook)

(defcustom tetris-tty-colors
  [nil "blue" "white" "yellow" "magenta" "cyan" "green" "red"]
  "Vector of colors of the various shapes in text mode
Element 0 is ignored."
  :group 'tetris
  :type (let ((names `("Shape 1" "Shape 2" "Shape 3"
		       "Shape 4" "Shape 5" "Shape 6" "Shape 7"))
	      (result `(vector (const nil))))
	  (while names
	    (add-to-list 'result 
			 (cons 'choice 
			       (cons :tag 
				     (cons (car names) 
					   (mapcar (lambda (color)
						     (list 'const color))
						   (defined-colors)))))
			 t)
	    (setq names (cdr names)))
	  result))

(defcustom tetris-x-colors
  [nil [0 0 1] [0.7 0 1] [1 1 0] [1 0 1] [0 1 1] [0 1 0] [1 0 0]]
  "Vector of colors of the various shapes
Element 0 is ignored."
  :group 'tetris
  :type 'sexp)

(defcustom tetris-buffer-name "*Tetris*"
  "Name used for Tetris buffer."
  :group 'tetris
  :type 'string)

(defcustom tetris-buffer-width 30
  "Width of used portion of buffer."
  :group 'tetris
  :type 'number)

(defcustom tetris-buffer-height 22
  "Height of used portion of buffer."
  :group 'tetris
  :type 'number)

(defcustom tetris-width 10
  "Width of playing area."
  :group 'tetris
  :type 'number)

(defcustom tetris-height 20
  "Height of playing area."
  :group 'tetris
  :type 'number)

(defcustom tetris-top-left-x 3
  "X position of top left of playing area."
  :group 'tetris
  :type 'number)

(defcustom tetris-top-left-y 1
  "Y position of top left of playing area."
  :group 'tetris
  :type 'number)

(defvar tetris-next-x (+ (* 2 tetris-top-left-x) tetris-width)
  "X position of next shape.")

(defvar tetris-next-y tetris-top-left-y
  "Y position of next shape.")

(defvar tetris-score-x tetris-next-x
  "X position of score.")

(defvar tetris-score-y (+ tetris-next-y 6)
  "Y position of score.")

;; It is not safe to put this in /tmp.
;; Someone could make a symlink in /tmp 
;; pointing to a file you don't want to clobber.
(defvar tetris-score-file "~/.tetris-scores")
;; anybody with a well-connected server want to host this?
;(defvar tetris-score-file "/anonymous@ftp.pgt.com:/pub/cgw/tetris-scores"
  "File for holding high scores.")

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (t nil))
    (((glyph color-x) [0.5 0.5 0.5])
     (t nil))))

(defvar tetris-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty)
     (t nil))
    (((glyph color-x) [0 0 0])
     (color-tty "black")
     (t nil))))

(defvar tetris-cell-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty)
     (t nil))
    ;; color information is taken from tetris-x-colors and tetris-tty-colors
    ))

(defvar tetris-space-options
  '(((t ?\040))
    nil
    nil))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tetris-shapes
  [[[[1 1 0 0] [1 1 0 0] [1 1 0 0] [1 1 0 0]]
    [[1 1 0 0] [1 1 0 0] [1 1 0 0] [1 1 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[2 2 2 0] [0 2 0 0] [2 0 0 0] [2 2 0 0]]
    [[0 0 2 0] [0 2 0 0] [2 2 2 0] [2 0 0 0]]
    [[0 0 0 0] [2 2 0 0] [0 0 0 0] [2 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[3 3 3 0] [3 3 0 0] [0 0 3 0] [3 0 0 0]]
    [[3 0 0 0] [0 3 0 0] [3 3 3 0] [3 0 0 0]]
    [[0 0 0 0] [0 3 0 0] [0 0 0 0] [3 3 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[4 4 0 0] [0 4 0 0] [4 4 0 0] [0 4 0 0]]
    [[0 4 4 0] [4 4 0 0] [0 4 4 0] [4 4 0 0]]
    [[0 0 0 0] [4 0 0 0] [0 0 0 0] [4 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[0 5 5 0] [5 0 0 0] [0 5 5 0] [5 0 0 0]]
    [[5 5 0 0] [5 5 0 0] [5 5 0 0] [5 5 0 0]]
    [[0 0 0 0] [0 5 0 0] [0 0 0 0] [0 5 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[0 6 0 0] [6 0 0 0] [6 6 6 0] [0 6 0 0]]
    [[6 6 6 0] [6 6 0 0] [0 6 0 0] [6 6 0 0]]
    [[0 0 0 0] [6 0 0 0] [0 0 0 0] [0 6 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[7 7 7 7] [7 0 0 0] [7 7 7 7] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]]])

;;the scoring rules were taken from "xtetris".  Blocks score differently 
;;depending on their rotation

(defconst tetris-shape-scores 
  [ [6 6 6 6] [6 7 6 7] [6 7 6 7] [6 7 6 7] [6 7 6 7] [5 5 6 5] [5 8 5 8]] )

(defconst tetris-shape-dimensions
  [[2 2] [3 2] [3 2] [3 2] [3 2] [3 2] [4 1]])

(defconst tetris-blank 0)

(defconst tetris-border 8)

(defconst tetris-space 9)

(defun tetris-default-update-speed-function (shapes rows)
  (/ 20.0 (+ 50.0 rows)))

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-shape 0)
(defvar tetris-rot 0)
(defvar tetris-next-shape 0)
(defvar tetris-n-shapes 0)
(defvar tetris-n-rows 0)
(defvar tetris-score 0)
(defvar tetris-pos-x 0)
(defvar tetris-pos-y 0)
(defvar tetris-paused nil)

(make-variable-buffer-local 'tetris-shape)
(make-variable-buffer-local 'tetris-rot)
(make-variable-buffer-local 'tetris-next-shape)
(make-variable-buffer-local 'tetris-n-shapes)
(make-variable-buffer-local 'tetris-n-rows)
(make-variable-buffer-local 'tetris-score)
(make-variable-buffer-local 'tetris-pos-x)
(make-variable-buffer-local 'tetris-pos-y)
(make-variable-buffer-local 'tetris-paused)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-mode-map
  (make-sparse-keymap 'tetris-mode-map))

(define-key tetris-mode-map "n"		'tetris-start-game)
(define-key tetris-mode-map "q"		'tetris-end-game)
(define-key tetris-mode-map "p"		'tetris-pause-game)

(define-key tetris-mode-map " "		'tetris-move-bottom)
(define-key tetris-mode-map [left]	'tetris-move-left)
(define-key tetris-mode-map [right]	'tetris-move-right)
(define-key tetris-mode-map [up]	'tetris-rotate-prev)
(define-key tetris-mode-map [down]	'tetris-rotate-next)

(defvar tetris-null-map
  (make-sparse-keymap 'tetris-null-map))

(define-key tetris-null-map "n"		'tetris-start-game)

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tetris-display-options ()
  (let ((options (make-vector 256 nil)))
    (loop for c from 0 to 255 do
      (aset options c
	    (cond ((= c tetris-blank)
		    tetris-blank-options)
                  ((and (>= c 1) (<= c 7))
		   (append
		    tetris-cell-options
		    `((((glyph color-x) ,(aref tetris-x-colors c))
		       (color-tty ,(aref tetris-tty-colors c))
		       (t nil)))))
		   ((= c tetris-border)
		    tetris-border-options)
		   ((= c tetris-space)
		    tetris-space-options)
                  (t
                   '(nil nil nil)))))
    options))

(defun tetris-get-tick-period ()
  (if (boundp 'tetris-update-speed-function)
      (let ((period (apply tetris-update-speed-function
			   tetris-n-shapes
			   tetris-n-rows nil)))
	(and (numberp period) period))))

(defun tetris-get-shape-cell (x y)
  (aref (aref (aref (aref tetris-shapes
			  tetris-shape)
		    y)
	      tetris-rot)
	x))

(defun tetris-shape-width ()
  (aref (aref tetris-shape-dimensions tetris-shape)
	(% tetris-rot 2)))

(defun tetris-shape-height ()
  (aref (aref tetris-shape-dimensions tetris-shape)
	(- 1 (% tetris-rot 2))))

(defun tetris-draw-score ()
  (let ((strings (vector (format "Shapes: %05d" tetris-n-shapes)
			 (format "Rows:   %05d" tetris-n-rows)
			 (format "Score:  %05d" tetris-score))))
    (loop for y from 0 to 2 do
	  (let* ((string (aref strings y))
		 (len (length string)))
	    (loop for x from 0 to (1- len) do
		  (gamegrid-set-cell (+ tetris-score-x x)
				     (+ tetris-score-y y)
				     (aref string x)))))))

(defun tetris-update-score ()
  (tetris-draw-score)
  (let ((period (tetris-get-tick-period)))
    (if period (gamegrid-set-timer period))))

(defun tetris-new-shape ()
  (setq tetris-shape tetris-next-shape)
  (setq tetris-rot 0)
  (setq tetris-next-shape (random 7))
  (setq tetris-pos-x (/ (- tetris-width (tetris-shape-width)) 2))
  (setq tetris-pos-y 0)
  (if (tetris-test-shape)
      (tetris-end-game)
    (tetris-draw-shape))
  (tetris-draw-next-shape)
  (tetris-update-score))

(defun tetris-draw-next-shape ()
  (loop for y from 0 to 3 do
	(loop for x from 0 to 3 do
	      (gamegrid-set-cell (+ tetris-next-x x)
				 (+ tetris-next-y y)
				 (let ((tetris-shape tetris-next-shape)
				       (tetris-rot 0))
				   (tetris-get-shape-cell x y))))))

(defun tetris-draw-shape ()
  (loop for y from 0 to (1- (tetris-shape-height)) do
	(loop for x from 0 to (1- (tetris-shape-width)) do
	      (let ((c (tetris-get-shape-cell x y)))
		(if (/= c tetris-blank)
		    (gamegrid-set-cell (+ tetris-top-left-x
					  tetris-pos-x
					  x)
				       (+ tetris-top-left-y
					  tetris-pos-y
					  y)
				       c))))))

(defun tetris-erase-shape ()
  (loop for y from 0 to (1- (tetris-shape-height)) do
	(loop for x from 0 to (1- (tetris-shape-width)) do
	      (let ((c (tetris-get-shape-cell x y))
		    (px (+ tetris-top-left-x tetris-pos-x x))
		    (py (+ tetris-top-left-y tetris-pos-y y)))
		(if (/= c tetris-blank)
		    (gamegrid-set-cell px py tetris-blank))))))

(defun tetris-test-shape ()
  (let ((hit nil))
    (loop for y from 0 to (1- (tetris-shape-height)) do
	  (loop for x from 0 to (1- (tetris-shape-width)) do
		(unless hit
		  (setq hit
			(let* ((c (tetris-get-shape-cell x y))
			      (xx (+ tetris-pos-x x))
			      (yy (+ tetris-pos-y y))
			      (px (+ tetris-top-left-x xx))
			      (py (+ tetris-top-left-y yy)))
			  (and (/= c tetris-blank)
			       (or (>= xx tetris-width)
				   (>= yy tetris-height)
				   (/= (gamegrid-get-cell px py)
				       tetris-blank))))))))
    hit))

(defun tetris-full-row (y)
  (let ((full t))
    (loop for x from 0 to (1- tetris-width) do
	  (if (= (gamegrid-get-cell (+ tetris-top-left-x x)
				    (+ tetris-top-left-y y))
		 tetris-blank)
	      (setq full nil)))
    full))

(defun tetris-shift-row (y)
  (if (= y 0)
      (loop for x from 0 to (1- tetris-width) do
	(gamegrid-set-cell (+ tetris-top-left-x x)
			   (+ tetris-top-left-y y)
			   tetris-blank))
  (loop for x from 0 to (1- tetris-width) do
	(let ((c (gamegrid-get-cell (+ tetris-top-left-x x)
				    (+ tetris-top-left-y y -1))))
	  (gamegrid-set-cell (+ tetris-top-left-x x)
			     (+ tetris-top-left-y y)
			   c)))))

(defun tetris-shift-down ()
  (loop for y0 from 0 to (1- tetris-height) do
	(if (tetris-full-row y0)
	    (progn (setq tetris-n-rows (1+ tetris-n-rows))
		   (loop for y from y0 downto 0 do
			 (tetris-shift-row y))))))

(defun tetris-draw-border-p ()
  (or (not (eq gamegrid-display-mode 'glyph))
      tetris-draw-border-with-glyphs))

(defun tetris-init-buffer ()
  (gamegrid-init-buffer tetris-buffer-width
			tetris-buffer-height
			tetris-space)
  (let ((buffer-read-only nil))
    (if (tetris-draw-border-p)
	(loop for y from -1 to tetris-height do
	      (loop for x from -1 to tetris-width do
		    (gamegrid-set-cell (+ tetris-top-left-x x)
				       (+ tetris-top-left-y y)
				       tetris-border))))
    (loop for y from 0 to (1- tetris-height) do
	  (loop for x from 0 to (1- tetris-width) do
		(gamegrid-set-cell (+ tetris-top-left-x x)
				   (+ tetris-top-left-y y)
				   tetris-blank)))
    (if (tetris-draw-border-p)
	(loop for y from -1 to 4 do
	      (loop for x from -1 to 4 do
		    (gamegrid-set-cell (+ tetris-next-x x)
				       (+ tetris-next-y y)
				       tetris-border))))))

(defun tetris-reset-game ()
  (gamegrid-kill-timer)
  (tetris-init-buffer)
  (setq tetris-next-shape (random 7))
  (setq tetris-shape	0
	tetris-rot	0
	tetris-pos-x	0
	tetris-pos-y	0
	tetris-n-shapes	0
	tetris-n-rows	0
	tetris-score	0
	tetris-paused	nil)
  (tetris-new-shape))

(defun tetris-shape-done ()
  (tetris-shift-down)
  (setq tetris-n-shapes (1+ tetris-n-shapes))
  (setq tetris-score
	(+ tetris-score 
	   (aref (aref tetris-shape-scores tetris-shape) tetris-rot)))
  (tetris-update-score)
  (tetris-new-shape))

(defun tetris-update-game (tetris-buffer)
  "Called on each clock tick.
Drops the shape one square, testing for collision."
  (if (and (not tetris-paused)
	   (eq (current-buffer) tetris-buffer))
      (let (hit)
	(tetris-erase-shape)
	(setq tetris-pos-y (1+ tetris-pos-y))
	(setq hit (tetris-test-shape))
	(if hit
	    (setq tetris-pos-y (1- tetris-pos-y)))
	(tetris-draw-shape)
	(if hit
	    (tetris-shape-done)))))

(defun tetris-move-bottom ()
  "Drops the shape to the bottom of the playing area"
  (interactive)
  (let ((hit nil))
    (tetris-erase-shape)
    (while (not hit)
      (setq tetris-pos-y (1+ tetris-pos-y))
      (setq hit (tetris-test-shape)))
    (setq tetris-pos-y (1- tetris-pos-y))
    (tetris-draw-shape)
    (tetris-shape-done)))

(defun tetris-move-left ()
  "Moves the shape one square to the left"
  (interactive)
  (unless (= tetris-pos-x 0)
    (tetris-erase-shape)
    (setq tetris-pos-x (1- tetris-pos-x))
    (if (tetris-test-shape)
	(setq tetris-pos-x (1+ tetris-pos-x)))
    (tetris-draw-shape)))

(defun tetris-move-right ()
  "Moves the shape one square to the right"
  (interactive)
  (unless (= (+ tetris-pos-x (tetris-shape-width))
	     tetris-width)
    (tetris-erase-shape)
    (setq tetris-pos-x (1+ tetris-pos-x))
    (if (tetris-test-shape)
	(setq tetris-pos-x (1- tetris-pos-x)))
    (tetris-draw-shape)))

(defun tetris-rotate-prev ()
  "Rotates the shape clockwise"
  (interactive)
  (tetris-erase-shape)
  (setq tetris-rot (% (+ 1 tetris-rot) 4))
  (if (tetris-test-shape)
      (setq tetris-rot (% (+ 3 tetris-rot) 4)))
  (tetris-draw-shape))

(defun tetris-rotate-next ()
  "Rotates the shape anticlockwise"
  (interactive)
  (tetris-erase-shape)
  (setq tetris-rot (% (+ 3 tetris-rot) 4))
  (if (tetris-test-shape)
      (setq tetris-rot (% (+ 1 tetris-rot) 4)))
  (tetris-draw-shape))

(defun tetris-end-game ()
  "Terminates the current game"
  (interactive)
  (gamegrid-kill-timer)
  (use-local-map tetris-null-map)
  (gamegrid-add-score tetris-score-file tetris-score))

(defun tetris-start-game ()
  "Starts a new game of Tetris"
  (interactive)
  (tetris-reset-game)
  (use-local-map tetris-mode-map)
  (let ((period (or (tetris-get-tick-period)
		    tetris-default-tick-period)))
    (gamegrid-start-timer period 'tetris-update-game)))

(defun tetris-pause-game ()
  "Pauses (or resumes) the current game"
  (interactive)
  (setq tetris-paused (not tetris-paused))
  (message (and tetris-paused "Game paused (press p to resume)")))

(defun tetris-active-p ()
  (eq (current-local-map) tetris-mode-map))

(put 'tetris-mode 'mode-class 'special)

(defun tetris-mode ()
  "A mode for playing Tetris.

tetris-mode keybindings:
   \\{tetris-mode-map}
"
  (kill-all-local-variables)

  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)

  (use-local-map tetris-null-map)

  (setq major-mode 'tetris-mode)
  (setq mode-name "Tetris")

  (setq mode-popup-menu
	'("Tetris Commands"
	  ["Start new game"	tetris-start-game]
	  ["End game"		tetris-end-game
	   (tetris-active-p)]
	  ["Pause"		tetris-pause-game
	   (and (tetris-active-p) (not tetris-paused))]
	  ["Resume"		tetris-pause-game
	   (and (tetris-active-p) tetris-paused)]))

  (setq gamegrid-use-glyphs tetris-use-glyphs)
  (setq gamegrid-use-color tetris-use-color)

  (gamegrid-init (tetris-display-options))

  (run-hooks 'tetris-mode-hook))

;;;###autoload
(defun tetris ()
  "Play the Tetris game.
Shapes drop from the top of the screen, and the user has to move and
rotate the shape to fit in with those at the bottom of the screen so
as to form complete rows.

tetris-mode keybindings:
   \\<tetris-mode-map>
\\[tetris-start-game]	Starts a new game of Tetris
\\[tetris-end-game]	Terminates the current game
\\[tetris-pause-game]	Pauses (or resumes) the current game
\\[tetris-move-left]	Moves the shape one square to the left
\\[tetris-move-right]	Moves the shape one square to the right
\\[tetris-rotate-prev]	Rotates the shape clockwise
\\[tetris-rotate-next]	Rotates the shape anticlockwise
\\[tetris-move-bottom]	Drops the shape to the bottom of the playing area

"
  (interactive)

  (switch-to-buffer tetris-buffer-name)
  (gamegrid-kill-timer)
  (tetris-mode)
  (tetris-start-game))

(provide 'tetris)

;;; tetris.el ends here
