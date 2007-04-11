;;; 5x5.el --- simple little puzzle game

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Dave Pearson <davep@davep.org>
;; Maintainer: Dave Pearson <davep@davep.org>
;; Created: 1998-10-03
;; Keywords: games puzzles

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The aim of 5x5 is to fill in all the squares. If you need any more of an
;; explanation you probably shouldn't play the game.

;;; TODO:

;; o The code for updating the grid needs to be re-done. At the moment it
;;   simply re-draws the grid every time a move is made.
;;
;; o Look into tarting up the display with colour. gamegrid.el looks
;;   interesting, perhaps that is the way to go?

;;; Thanks:

;; Ralf Fassel <ralf@akutech.de> for his help and introduction to writing an
;; emacs mode.
;;
;; Pascal Q. Porcupine <joshagam@cs.nmsu.edu> for inspiring the animated
;; solver.

;;; Code:

;; Things we need.

(eval-when-compile
  (require 'cl))

;; If customize isn't available just use defvar instead.
(eval-and-compile
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

;; Customize options.

(defgroup 5x5 nil
  "5x5 - Silly little puzzle game."
  :group  'games
  :prefix "5x5-")

(defcustom 5x5-grid-size 5
  "*Size of the playing area."
  :type  'integer
  :group '5x5)

(defcustom 5x5-x-scale 4
  "*X scaling factor for drawing the grid."
  :type  'integer
  :group '5x5)

(defcustom 5x5-y-scale 3
  "*Y scaling factor for drawing the grid."
  :type  'integer
  :group '5x5)

(defcustom 5x5-animate-delay .01
  "*Delay in seconds when animating a solution crack."
  :type  'number
  :group '5x5)

(defcustom 5x5-hassle-me t
  "*Should 5x5 ask you when you want to do a destructive operation?"
  :type  'boolean
  :group '5x5)

(defcustom 5x5-mode-hook nil
  "*Hook run on starting 5x5."
  :type  'hook
  :group '5x5)

;; Non-customize variables.

(defvar 5x5-grid nil
  "5x5 grid contents.")

(defvar 5x5-x-pos 2
  "X position of cursor.")

(defvar 5x5-y-pos 2
  "Y position of cursor.")

(defvar 5x5-moves 0
  "Moves made.")

(defvar 5x5-cracking nil
  "Are we in cracking mode?")

(defvar 5x5-buffer-name "*5x5*"
  "Name of the 5x5 play buffer.")

(defvar 5x5-mode-map nil
  "Local keymap for the 5x5 game.")

;; Keymap.

(unless 5x5-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "?"                       #'describe-mode)
    (define-key map "\r"                      #'5x5-flip-current)
    (define-key map " "                       #'5x5-flip-current)
    (define-key map [up]                      #'5x5-up)
    (define-key map [down]                    #'5x5-down)
    (define-key map [left]                    #'5x5-left)
    (define-key map [tab]                     #'5x5-right)
    (define-key map [right]                   #'5x5-right)
    (define-key map [(control a)]             #'5x5-bol)
    (define-key map [(control e)]             #'5x5-eol)
    (define-key map [(control p)]             #'5x5-up)
    (define-key map [(control n)]             #'5x5-down)
    (define-key map [(control b)]             #'5x5-left)
    (define-key map [(control f)]             #'5x5-right)
    (define-key map [home]                    #'5x5-bol)
    (define-key map [end]                     #'5x5-eol)
    (define-key map [prior]                   #'5x5-first)
    (define-key map [next]                    #'5x5-last)
    (define-key map "r"                       #'5x5-randomize)
    (define-key map [(control c) (control r)] #'5x5-crack-randomly)
    (define-key map [(control c) (control c)] #'5x5-crack-mutating-current)
    (define-key map [(control c) (control b)] #'5x5-crack-mutating-best)
    (define-key map [(control c) (control x)] #'5x5-crack-xor-mutate)
    (define-key map "n"                       #'5x5-new-game)
    (define-key map "q"                       #'5x5-quit-game)
    (setq 5x5-mode-map map)))

;; Menu definition.

(easy-menu-define 5x5-mode-menu 5x5-mode-map "5x5 menu."
  '("5x5"
    ["New game"               5x5-new-game  t]
    ["Random game"            5x5-randomize t]
    ["Quit game"              5x5-quit-game t]
    "---"
    ["Crack randomly"         5x5-crack-randomly         t]
    ["Crack mutating current" 5x5-crack-mutating-current t]
    ["Crack mutating best"    5x5-crack-mutating-best    t]
    ["Crack with xor mutate"  5x5-crack-xor-mutate       t]))

;; Gameplay functions.

(put '5x5-mode 'mode-class 'special)

(defun 5x5-mode ()
  "A mode for playing `5x5'.

The key bindings for 5x5-mode are:

\\{5x5-mode-map}"
  (kill-all-local-variables)
  (use-local-map 5x5-mode-map)
  (setq major-mode '5x5-mode
        mode-name  "5x5")
  (run-mode-hooks '5x5-mode-hook)
  (setq buffer-read-only t
        truncate-lines   t)
  (buffer-disable-undo))

;;;###autoload
(defun 5x5 (&optional size)
  "Play 5x5.

The object of 5x5 is very simple, by moving around the grid and flipping
squares you must fill the grid.

5x5 keyboard bindings are:
\\<5x5-mode-map>
Flip                      \\[5x5-flip-current]
Move up                   \\[5x5-up]
Move down                 \\[5x5-down]
Move left                 \\[5x5-left]
Move right                \\[5x5-right]
Start new game            \\[5x5-new-game]
New game with random grid \\[5x5-randomize]
Random cracker            \\[5x5-crack-randomly]
Mutate current cracker    \\[5x5-crack-mutating-current]
Mutate best cracker       \\[5x5-crack-mutating-best]
Mutate xor cracker        \\[5x5-crack-xor-mutate]
Quit current game         \\[5x5-quit-game]"

  (interactive "P")
  (setq 5x5-cracking nil)
  (when size
    (setq 5x5-grid-size size))
  (switch-to-buffer 5x5-buffer-name)
  (if (or (not 5x5-grid) (not (= 5x5-grid-size (length (aref 5x5-grid 0)))))
      (5x5-new-game))
  (5x5-draw-grid (list 5x5-grid))
  (5x5-position-cursor)
  (5x5-mode))

(defun 5x5-new-game ()
  "Start a new game of `5x5'."
  (interactive)
  (when (if (interactive-p) (5x5-y-or-n-p "Start a new game? ") t)
    (setq 5x5-x-pos (/ 5x5-grid-size 2)
          5x5-y-pos (/ 5x5-grid-size 2)
          5x5-moves 0
          5x5-grid  (5x5-make-move (5x5-make-new-grid) 5x5-y-pos 5x5-x-pos))
    (5x5-draw-grid (list 5x5-grid))
    (5x5-position-cursor)))

(defun 5x5-quit-game ()
  "Quit the current game of `5x5'."
  (interactive)
  (kill-buffer 5x5-buffer-name))

(defun 5x5-make-new-grid ()
  "Create and return a new `5x5' grid structure."
  (let ((grid (make-vector 5x5-grid-size nil)))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (aset grid y (make-vector 5x5-grid-size nil)))
    grid))

(defun 5x5-cell (grid y x)
  "Return the value of the cell in GRID at location X,Y."
  (aref (aref grid y) x))

(defun 5x5-set-cell (grid y x value)
  "Set the value of cell X,Y in GRID to VALUE."
  (aset (aref grid y) x value))

(defun 5x5-flip-cell (grid y x)
  "Flip the value of cell X,Y in GRID."
  (5x5-set-cell grid y x (not (5x5-cell grid y x))))

(defun 5x5-copy-grid (grid)
  "Make a new copy of GRID."
  (let ((copy (5x5-make-new-grid)))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (loop for x from 0 to (1- 5x5-grid-size) do
                (5x5-set-cell copy y x (5x5-cell grid y x))))
    copy))

(defun 5x5-make-move (grid row col)
  "Make a move on GRID at row ROW and column COL."
  (5x5-flip-cell grid row col)
  (if (> row 0)
      (5x5-flip-cell grid (1- row) col))
  (if (< row (- 5x5-grid-size 1))
      (5x5-flip-cell grid (1+ row) col))
  (if (> col 0)
      (5x5-flip-cell grid row (1- col)))
  (if (< col (- 5x5-grid-size 1))
      (5x5-flip-cell grid row (1+ col)))
  grid)

(defun 5x5-row-value (row)
  "Get the \"on-value\" for grid row ROW."
  (loop for y from 0 to (1- 5x5-grid-size) sum (if (aref row y) 1 0)))

(defun 5x5-grid-value (grid)
  "Get the \"on-value\" for grid GRID."
  (loop for y from 0 to (1- 5x5-grid-size) sum (5x5-row-value (aref grid y))))

(defun 5x5-draw-grid-end ()
  "Draw the top/bottom of the grid."
  (insert "+")
  (loop for x from 0 to (1- 5x5-grid-size) do
        (insert "-" (make-string 5x5-x-scale ?-)))
  (insert "-+ "))

(defun 5x5-draw-grid (grids)
  "Draw the grids GRIDS into the current buffer."
  (let ((buffer-read-only nil))
    (erase-buffer)
    (loop for grid in grids do (5x5-draw-grid-end))
    (insert "\n")
    (loop for y from 0 to (1- 5x5-grid-size) do
          (loop for lines from 0 to (1- 5x5-y-scale) do
                (loop for grid in grids do
                      (loop for x from 0 to (1- 5x5-grid-size) do
                            (insert (if (zerop x) "| " " ")
                                    (make-string 5x5-x-scale
                                                 (if (5x5-cell grid y x) ?# ?.))))
                      (insert " | "))
                (insert "\n")))
    (loop for grid in grids do (5x5-draw-grid-end))
    (insert "\n")
    (insert (format "On: %d  Moves: %d" (5x5-grid-value (car grids)) 5x5-moves))))

(defun 5x5-position-cursor ()
  "Position the cursor on the grid."
  (goto-line (+ (* 5x5-y-pos 5x5-y-scale) 2))
  (goto-char (+ (point) (* 5x5-x-pos 5x5-x-scale) (+ 5x5-x-pos 1) 1)))

(defun 5x5-made-move ()
  "Keep track of how many moves have been made."
  (incf 5x5-moves))

(defun 5x5-make-random-grid ()
  "Make a random grid."
  (let ((grid (5x5-make-new-grid)))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (loop for x from 0 to (1- 5x5-grid-size) do
                (if (zerop (random 2))
                    (5x5-flip-cell grid y x))))
    grid))

;; Cracker functions.

;;;###autoload
(defun 5x5-crack-randomly ()
  "Attempt to crack 5x5 using random solutions."
  (interactive)
  (5x5-crack #'5x5-make-random-solution))

;;;###autoload
(defun 5x5-crack-mutating-current ()
  "Attempt to crack 5x5 by mutating the current solution."
  (interactive)
  (5x5-crack #'5x5-make-mutate-current))

;;;###autoload
(defun 5x5-crack-mutating-best ()
  "Attempt to crack 5x5 by mutating the best solution."
  (interactive)
  (5x5-crack #'5x5-make-mutate-best))

;;;###autoload
(defun 5x5-crack-xor-mutate ()
  "Attempt to crack 5x5 by xoring the current and best solution.
Mutate the result."
  (interactive)
  (5x5-crack #'5x5-make-xor-with-mutation))

;;;###autoload
(defun 5x5-crack (breeder)
  "Attempt to find a solution for 5x5.

5x5-crack takes the argument BREEDER which should be a function that takes
two parameters, the first will be a grid vector array that is the current
solution and the second will be the best solution so far.  The function
should return a grid vector array that is the new solution."

  (interactive "aBreeder function: ")
  (5x5)
  (setq 5x5-cracking t)
  (let* ((best-solution    (5x5-make-random-grid))
         (current-solution best-solution)
         (best-result      (5x5-make-new-grid))
         (current-result   (5x5-make-new-grid))
         (target           (* 5x5-grid-size 5x5-grid-size)))
    (while (and (< (5x5-grid-value best-result) target)
                (not (input-pending-p)))
      (setq current-result (5x5-play-solution current-solution best-solution))
      (if (> (5x5-grid-value current-result) (5x5-grid-value best-result))
          (setq best-solution current-solution
                best-result   current-result))
      (setq current-solution (funcall breeder
                                      (5x5-copy-grid current-solution)
                                      (5x5-copy-grid best-solution)))))
  (setq 5x5-cracking nil))

(defun 5x5-make-random-solution (&rest ignore)
  "Make a random solution."
  (5x5-make-random-grid))

(defun 5x5-make-mutate-current (current best)
  "Mutate the current solution."
  (5x5-mutate-solution current))

(defun 5x5-make-mutate-best (current best)
  "Mutate the best solution."
  (5x5-mutate-solution best))

(defun 5x5-make-xor-with-mutation (current best)
  "Xor current and best solution then mutate the result."
  (let ((xored (5x5-make-new-grid)))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (loop for x from 0 to (1- 5x5-grid-size) do
                (5x5-set-cell xored y x
                              (5x5-xor (5x5-cell current y x)
                                       (5x5-cell best    y x)))))
    (5x5-mutate-solution xored)))

(defun 5x5-mutate-solution (solution)
  "Randomly flip bits in the solution."
  (loop for y from 0 to (1- 5x5-grid-size) do
        (loop for x from 0 to (1- 5x5-grid-size) do
              (if (= (random (/ (* 5x5-grid-size 5x5-grid-size) 2))
                     (/ (/ (* 5x5-grid-size 5x5-grid-size) 2) 2))
                  (5x5-flip-cell solution y x))))
  solution)

(defun 5x5-play-solution (solution best)
  "Play a solution on an empty grid.  This destroys the current game
in progress because it is an animated attempt."
  (5x5-new-game)
  (let ((inhibit-quit t))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (loop for x from 0 to (1- 5x5-grid-size) do
                (setq 5x5-y-pos y
                      5x5-x-pos x)
                (if (5x5-cell solution y x)
                    (5x5-flip-current))
                (5x5-draw-grid (list 5x5-grid solution best))
                (5x5-position-cursor)
                (sit-for 5x5-animate-delay))))
  5x5-grid)

;; Keyboard response functions.

(defun 5x5-flip-current ()
  "Make a move on the current cursor location."
  (interactive)
  (setq 5x5-grid (5x5-make-move 5x5-grid 5x5-y-pos 5x5-x-pos))
  (5x5-made-move)
  (unless 5x5-cracking
    (5x5-draw-grid (list 5x5-grid)))
  (5x5-position-cursor)
  (when (= (5x5-grid-value 5x5-grid) (* 5x5-grid-size 5x5-grid-size))
    (beep)
    (message "You win!")))

(defun 5x5-up ()
  "Move up."
  (interactive)
  (unless (zerop 5x5-y-pos)
    (decf 5x5-y-pos)
    (5x5-position-cursor)))

(defun 5x5-down ()
  "Move down."
  (interactive)
  (unless (= 5x5-y-pos (1- 5x5-grid-size))
    (incf 5x5-y-pos)
    (5x5-position-cursor)))

(defun 5x5-left ()
  "Move left."
  (interactive)
  (unless (zerop 5x5-x-pos)
    (decf 5x5-x-pos)
    (5x5-position-cursor)))

(defun 5x5-right ()
  "Move right."
  (interactive)
  (unless (= 5x5-x-pos (1- 5x5-grid-size))
    (incf 5x5-x-pos)
    (5x5-position-cursor)))

(defun 5x5-bol ()
  "Move to beginning of line."
  (interactive)
  (setq 5x5-x-pos 0)
  (5x5-position-cursor))

(defun 5x5-eol ()
  "Move to end of line."
  (interactive)
  (setq 5x5-x-pos (1- 5x5-grid-size))
  (5x5-position-cursor))

(defun 5x5-first ()
  "Move to the first cell."
  (interactive)
  (setq 5x5-x-pos 0
        5x5-y-pos 0)
  (5x5-position-cursor))

(defun 5x5-last ()
  "Move to the last cell."
  (interactive)
  (setq 5x5-x-pos (1- 5x5-grid-size)
        5x5-y-pos (1- 5x5-grid-size))
  (5x5-position-cursor))

(defun 5x5-randomize ()
  "Randomize the grid."
  (interactive)
  (when (5x5-y-or-n-p "Start a new game with a random grid? ")
    (setq 5x5-x-pos (/ 5x5-grid-size 2)
          5x5-y-pos (/ 5x5-grid-size 2)
          5x5-moves 0
          5x5-grid  (5x5-make-random-grid))
    (unless 5x5-cracking
      (5x5-draw-grid (list 5x5-grid)))
    (5x5-position-cursor)))

;; Support functions

(defun 5x5-xor (x y)
  "Boolean exclusive-or of X and Y."
  (and (or x y) (not (and x y))))

(defun 5x5-y-or-n-p (prompt)
  "5x5 wrapper for `y-or-n-p' which respects the `5x5-hassle-me' setting."
  (if 5x5-hassle-me
      (y-or-n-p prompt)
    t))

(random t)

(provide '5x5)

;;; arch-tag: ec4dabd5-572d-41ea-b48c-ec5ce0d68fa9
;;; 5x5.el ends here
