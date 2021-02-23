;;; tetris.el --- implementation of Tetris for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1997, 2001-2021 Free Software Foundation, Inc.

;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Old-Version: 2.01
;; Created: 1997-08-13
;; Keywords: games

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'gamegrid)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup tetris nil
  "Play a game of Tetris."
  :prefix "tetris-"
  :group 'games)

(defcustom tetris-use-glyphs t
  "Non-nil means use glyphs when available."
  :type 'boolean)

(defcustom tetris-use-color t
  "Non-nil means use color when available."
  :type 'boolean)

(defcustom tetris-draw-border-with-glyphs t
  "Non-nil means draw a border even when using glyphs."
  :type 'boolean)

(defcustom tetris-default-tick-period 0.3
  "The default time taken for a shape to drop one row."
  :type 'number)

(defcustom tetris-update-speed-function
  'tetris-default-update-speed-function
  "Function run whenever the Tetris score changes.
Called with two arguments: (SHAPES ROWS)
SHAPES is the number of shapes which have been dropped.
ROWS is the number of rows which have been completed.

If the return value is a number, it is used as the timer period."
  :type 'function)

(defcustom tetris-mode-hook nil
  "Hook run upon starting Tetris."
  :type 'hook)

(defcustom tetris-tty-colors
  ["blue" "white" "yellow" "magenta" "cyan" "green" "red"]
  "Vector of colors of the various shapes in text mode."
  :type '(vector (color :tag "Shape 1")
		 (color :tag "Shape 2")
		 (color :tag "Shape 3")
		 (color :tag "Shape 4")
		 (color :tag "Shape 5")
		 (color :tag "Shape 6")
		 (color :tag "Shape 7")))

(defcustom tetris-x-colors
  [[0 0 1] [0.7 0 1] [1 1 0] [1 0 1] [0 1 1] [0 1 0] [1 0 0]]
  "Vector of RGB colors of the various shapes."
  :type '(vector (vector :tag "Shape 1" number number number)
                 (vector :tag "Shape 2" number number number)
                 (vector :tag "Shape 3" number number number)
                 (vector :tag "Shape 4" number number number)
                 (vector :tag "Shape 5" number number number)
                 (vector :tag "Shape 6" number number number)
                 (vector :tag "Shape 7" number number number)))

(defcustom tetris-buffer-name "*Tetris*"
  "Name used for Tetris buffer."
  :type 'string)

(defcustom tetris-buffer-width 30
  "Width of used portion of buffer."
  :type 'number)

(defcustom tetris-buffer-height 22
  "Height of used portion of buffer."
  :type 'number)

(defcustom tetris-width 10
  "Width of playing area."
  :type 'number)

(defcustom tetris-height 20
  "Height of playing area."
  :type 'number)

(defcustom tetris-top-left-x 3
  "X position of top left of playing area."
  :type 'number)

(defcustom tetris-top-left-y 1
  "Y position of top left of playing area."
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
(defvar tetris-score-file "tetris-scores"
;; anybody with a well-connected server want to host this?
;(defvar tetris-score-file "/anonymous@ftp.pgt.com:/pub/cgw/tetris-scores"
  "File for holding high scores.")

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar tetris-cell-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    ;; color information is taken from tetris-x-colors and tetris-tty-colors
    ))

(defvar tetris-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))))

(defvar tetris-space-options
  '(((t ?\040))
    nil
    nil))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tetris-shapes
  [[[[0  0] [1  0] [0  1] [1  1]]]

   [[[0  0] [1  0] [2  0] [2  1]]
    [[1 -1] [1  0] [1  1] [0  1]]
    [[0 -1] [0  0] [1  0] [2  0]]
    [[1 -1] [2 -1] [1  0] [1  1]]]

   [[[0  0] [1  0] [2  0] [0  1]]
    [[0 -1] [1 -1] [1  0] [1  1]]
    [[2 -1] [0  0] [1  0] [2  0]]
    [[1 -1] [1  0] [1  1] [2  1]]]

   [[[0  0] [1  0] [1  1] [2  1]]
    [[1  0] [0  1] [1  1] [0  2]]]

   [[[1  0] [2  0] [0  1] [1  1]]
    [[0  0] [0  1] [1  1] [1  2]]]

   [[[1  0] [0  1] [1  1] [2  1]]
    [[1  0] [1  1] [2  1] [1  2]]
    [[0  1] [1  1] [2  1] [1  2]]
    [[1  0] [0  1] [1  1] [1  2]]]

   [[[0  0] [1  0] [2  0] [3  0]]
    [[1 -1] [1  0] [1  1] [1  2]]]]
  "Each shape is described by a vector that contains the coordinates of
each one of its four blocks.")

;;the scoring rules were taken from "xtetris".  Blocks score differently
;;depending on their rotation

(defconst tetris-shape-scores
  [[6] [6 7 6 7] [6 7 6 7] [6 7] [6 7] [5 5 6 5] [5 8]] )

(defconst tetris-shape-dimensions
  [[2 2] [3 2] [3 2] [3 2] [3 2] [3 2] [4 1]])

(defconst tetris-blank 7)

(defconst tetris-border 8)

(defconst tetris-space 9)

(defun tetris-default-update-speed-function (_shapes rows)
  (/ 20.0 (+ 50.0 rows)))

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local tetris-shape 0)
(defvar-local tetris-rot 0)
(defvar-local tetris-next-shape 0)
(defvar-local tetris-n-shapes 0)
(defvar-local tetris-n-rows 0)
(defvar-local tetris-score 0)
(defvar-local tetris-pos-x 0)
(defvar-local tetris-pos-y 0)
(defvar-local tetris-paused nil)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-mode-map
  (let ((map (make-sparse-keymap 'tetris-mode-map)))
    (define-key map "n"		'tetris-start-game)
    (define-key map "q"		'tetris-end-game)
    (define-key map "p"		'tetris-pause-game)

    (define-key map " "		'tetris-move-bottom)
    (define-key map [left]	'tetris-move-left)
    (define-key map [right]	'tetris-move-right)
    (define-key map [up]	'tetris-rotate-prev)
    (define-key map [down]	'tetris-move-down)
    map)
  "Keymap for Tetris games.")

(defvar tetris-null-map
  (let ((map (make-sparse-keymap 'tetris-null-map)))
    (define-key map "n"		'tetris-start-game)
    (define-key map "q"         'quit-window)
    map)
  "Keymap for finished Tetris games.")

(defconst tetris--menu-def
  '("Tetris"
    ["Start new game"    tetris-start-game
     :help "Start a new Tetris game"]
    ["End game"          tetris-end-game
     :active (tetris-active-p)
     :help "End the current Tetris game"]
    ;; FIXME: Pause and resume from the menu currently doesn't work
    ;;        very well and is therefore disabled.  The game continues
    ;;        running while navigating the menu.  See also
    ;;        `snake--menu-def' which has the same problem.
    ;; ["Pause"             tetris-pause-game
    ;;  :active (and (tetris-active-p) (not tetris-paused))
    ;;  :help "Pause running Tetris game"]
    ;; ["Resume"            tetris-pause-game
    ;;  :active (and (tetris-active-p) tetris-paused)
    ;;  :help "Resume paused Tetris game"]
    )
  "Menu for `tetris'.  Used to initialize menus.")

(easy-menu-define
  tetris-mode-menu tetris-mode-map
  "Menu for running Tetris games."
  tetris--menu-def)

(easy-menu-define
  tetris-null-menu tetris-null-map
  "Menu for finished Tetris games."
  tetris--menu-def)

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tetris-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
	    (cond ((= c tetris-blank)
                   tetris-blank-options)
                  ((and (>= c 0) (<= c 6))
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
  (let ((period (apply tetris-update-speed-function
                       tetris-n-shapes
                       tetris-n-rows nil)))
    (and (numberp period) period)))

(defun tetris-get-shape-cell (block)
  (aref (aref  (aref tetris-shapes
                     tetris-shape) tetris-rot)
        block))

(defun tetris-shape-width ()
  (aref (aref tetris-shape-dimensions tetris-shape) 0))

(defun tetris-shape-rotations ()
  (length (aref tetris-shapes tetris-shape)))

(defun tetris-draw-score ()
  (let ((strings (vector (format "Shapes: %05d" tetris-n-shapes)
			 (format "Rows:   %05d" tetris-n-rows)
			 (format "Score:  %05d" tetris-score))))
    (dotimes (y 3)
      (let* ((string (aref strings y))
             (len (length string)))
        (dotimes (x len)
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
    (tetris-draw-shape)
    (tetris-draw-next-shape)
    (tetris-update-score)))

(defun tetris-draw-next-shape ()
  (dotimes (x 4)
    (dotimes (y 4)
      (gamegrid-set-cell (+ tetris-next-x x)
                         (+ tetris-next-y y)
                         tetris-blank)))
  (dotimes (i 4)
    (let ((tetris-shape tetris-next-shape)
          (tetris-rot 0))
      (gamegrid-set-cell (+ tetris-next-x
                            (aref (tetris-get-shape-cell i) 0))
                         (+ tetris-next-y
                            (aref (tetris-get-shape-cell i) 1))
                         tetris-shape))))

(defun tetris-draw-shape ()
  (dotimes (i 4)
    (let ((c (tetris-get-shape-cell i)))
      (gamegrid-set-cell (+ tetris-top-left-x
                            tetris-pos-x
                            (aref c 0))
                         (+ tetris-top-left-y
                            tetris-pos-y
                            (aref c 1))
                         tetris-shape))))

(defun tetris-erase-shape ()
  (dotimes (i 4)
    (let ((c (tetris-get-shape-cell i)))
      (gamegrid-set-cell (+ tetris-top-left-x
                            tetris-pos-x
                            (aref c 0))
                         (+ tetris-top-left-y
                            tetris-pos-y
                            (aref c 1))
                         tetris-blank))))

(defun tetris-test-shape ()
  (let ((hit nil))
    (dotimes (i 4)
      (unless hit
        (setq hit
              (let* ((c (tetris-get-shape-cell i))
                     (xx (+ tetris-pos-x
                            (aref c 0)))
                     (yy (+ tetris-pos-y
                            (aref c 1))))
                (or (>= xx tetris-width)
                    (>= yy tetris-height)
                    (/= (gamegrid-get-cell
                         (+ xx tetris-top-left-x)
                         (+ yy tetris-top-left-y))
                        tetris-blank))))))
    hit))

(defun tetris-full-row (y)
  (let ((full t))
    (dotimes (x tetris-width)
      (if (= (gamegrid-get-cell (+ tetris-top-left-x x)
                                (+ tetris-top-left-y y))
             tetris-blank)
          (setq full nil)))
    full))

(defun tetris-shift-row (y)
  (if (= y 0)
      (dotimes (x tetris-width)
	(gamegrid-set-cell (+ tetris-top-left-x x)
			   (+ tetris-top-left-y y)
			   tetris-blank))
    (dotimes (x tetris-width)
      (let ((c (gamegrid-get-cell (+ tetris-top-left-x x)
                                  (+ tetris-top-left-y y -1))))
        (gamegrid-set-cell (+ tetris-top-left-x x)
                           (+ tetris-top-left-y y)
			   c)))))

(defun tetris-shift-down ()
  (dotimes (y0 tetris-height)
    (when (tetris-full-row y0)
      (setq tetris-n-rows (1+ tetris-n-rows))
      (cl-loop for y from y0 downto 0 do
               (tetris-shift-row y)))))

(defun tetris-draw-border-p ()
  (or (not (eq gamegrid-display-mode 'glyph))
      tetris-draw-border-with-glyphs))

(defun tetris-init-buffer ()
  (gamegrid-init-buffer tetris-buffer-width
			tetris-buffer-height
			tetris-space)
  (let ((buffer-read-only nil))
    (if (tetris-draw-border-p)
	(cl-loop for y from -1 to tetris-height do
                 (cl-loop for x from -1 to tetris-width do
                          (gamegrid-set-cell (+ tetris-top-left-x x)
                                             (+ tetris-top-left-y y)
                                             tetris-border))))
    (dotimes (y tetris-height)
      (dotimes (x tetris-width)
        (gamegrid-set-cell (+ tetris-top-left-x x)
                           (+ tetris-top-left-y y)
                           tetris-blank)))
    (if (tetris-draw-border-p)
	(cl-loop for y from -1 to 4 do
                 (cl-loop for x from -1 to 4 do
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
  "Drop the shape to the bottom of the playing area."
  (interactive nil tetris-mode)
  (unless tetris-paused
    (let ((hit nil))
      (tetris-erase-shape)
      (while (not hit)
        (setq tetris-pos-y (1+ tetris-pos-y))
        (setq hit (tetris-test-shape)))
      (setq tetris-pos-y (1- tetris-pos-y))
      (tetris-draw-shape)
      (tetris-shape-done))))

(defun tetris-move-left ()
  "Move the shape one square to the left."
  (interactive nil tetris-mode)
  (unless tetris-paused
    (tetris-erase-shape)
    (setq tetris-pos-x (1- tetris-pos-x))
    (if (tetris-test-shape)
        (setq tetris-pos-x (1+ tetris-pos-x)))
    (tetris-draw-shape)))

(defun tetris-move-right ()
  "Move the shape one square to the right."
  (interactive nil tetris-mode)
  (unless tetris-paused
    (tetris-erase-shape)
    (setq tetris-pos-x (1+ tetris-pos-x))
    (if (tetris-test-shape)
	(setq tetris-pos-x (1- tetris-pos-x)))
    (tetris-draw-shape)))

(defun tetris-move-down ()
  "Move the shape one square to the bottom."
  (interactive nil tetris-mode)
  (unless tetris-paused
    (tetris-erase-shape)
    (setq tetris-pos-y (1+ tetris-pos-y))
    (if (tetris-test-shape)
	(setq tetris-pos-y (1- tetris-pos-y)))
    (tetris-draw-shape)))

(defun tetris-rotate-prev ()
  "Rotate the shape clockwise."
  (interactive nil tetris-mode)
  (unless tetris-paused
      (tetris-erase-shape)
      (setq tetris-rot (% (+ 1 tetris-rot)
                          (tetris-shape-rotations)))
      (if (tetris-test-shape)
          (setq tetris-rot (% (+ 3 tetris-rot)
                              (tetris-shape-rotations))))
      (tetris-draw-shape)))

(defun tetris-rotate-next ()
  "Rotate the shape anticlockwise."
  (interactive nil tetris-mode)
  (unless tetris-paused
        (tetris-erase-shape)
        (setq tetris-rot (% (+ 3 tetris-rot)
                            (tetris-shape-rotations)))
        (if (tetris-test-shape)
            (setq tetris-rot (% (+ 1 tetris-rot)
                                (tetris-shape-rotations))))
        (tetris-draw-shape)))

(defun tetris-end-game ()
  "Terminate the current game."
  (interactive nil tetris-mode)
  (gamegrid-kill-timer)
  (use-local-map tetris-null-map)
  (gamegrid-add-score tetris-score-file tetris-score))

(defun tetris-start-game ()
  "Start a new game of Tetris."
  (interactive nil tetris-mode)
  (tetris-reset-game)
  (use-local-map tetris-mode-map)
  (let ((period (or (tetris-get-tick-period)
		    tetris-default-tick-period)))
    (gamegrid-start-timer period 'tetris-update-game)))

(defun tetris-pause-game ()
  "Pause (or resume) the current game."
  (interactive nil tetris-mode)
  (setq tetris-paused (not tetris-paused))
  (message (and tetris-paused "Game paused (press p to resume)")))

(defun tetris-active-p ()
  (eq (current-local-map) tetris-mode-map))

(put 'tetris-mode 'mode-class 'special)

(define-derived-mode tetris-mode nil "Tetris"
  "A mode for playing Tetris."
  :interactive nil

  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)

  (use-local-map tetris-null-map)

  (setq show-trailing-whitespace nil)

  (setq gamegrid-use-glyphs tetris-use-glyphs)
  (setq gamegrid-use-color tetris-use-color)

  (gamegrid-init (tetris-display-options)))

;;;###autoload
(defun tetris ()
  "Play the Tetris game.
Shapes drop from the top of the screen, and the user has to move and
rotate the shape to fit in with those at the bottom of the screen so
as to form complete rows.

tetris-mode keybindings:
\\<tetris-mode-map>
\\[tetris-start-game]	Start a new game of Tetris
\\[tetris-end-game]	Terminate the current game
\\[tetris-pause-game]	Pause (or resume) the current game
\\[tetris-move-left]	Move the shape one square to the left
\\[tetris-move-right]	Move the shape one square to the right
\\[tetris-rotate-prev]	Rotate the shape clockwise
\\[tetris-rotate-next]	Rotate the shape anticlockwise
\\[tetris-move-bottom]	Drop the shape to the bottom of the playing area"
  (interactive)

  (select-window (or (get-buffer-window tetris-buffer-name)
		     (selected-window)))
  (switch-to-buffer tetris-buffer-name)
  (gamegrid-kill-timer)
  (tetris-mode)
  (tetris-start-game))

(provide 'tetris)

;;; tetris.el ends here
