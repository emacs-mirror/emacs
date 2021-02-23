;;; snake.el --- implementation of Snake for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1997, 2001-2021 Free Software Foundation, Inc.

;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Created: 1997-09-10
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

(defvar snake-use-glyphs-flag t
  "Non-nil means use glyphs when available.")

(defvar snake-use-color-flag t
  "Non-nil means use color when available.")

(defvar snake-buffer-name "*Snake*"
  "Name used for Snake buffer.")

(defvar snake-buffer-width 30
  "Width of used portion of buffer.")

(defvar snake-buffer-height 22
  "Height of used portion of buffer.")

(defvar snake-width 30
  "Width of playing area.")

(defvar snake-height 20
  "Height of playing area.")

(defvar snake-initial-length 5
  "Initial length of snake.")

(defvar snake-initial-x 10
  "Initial X position of snake.")

(defvar snake-initial-y 10
  "Initial Y position of snake.")

(defvar snake-initial-velocity-x 1
  "Initial X velocity of snake.")

(defvar snake-initial-velocity-y 0
  "Initial Y velocity of snake.")

(defvar snake-tick-period 0.2
  "The default time taken for the snake to advance one square.")

(defvar snake-mode-hook nil
  "Hook run upon starting Snake.")

(defvar snake-score-x 0
  "X position of score.")

(defvar snake-score-y snake-height
  "Y position of score.")

;; It is not safe to put this in /tmp.
;; Someone could make a symlink in /tmp
;; pointing to a file you don't want to clobber.
(defvar snake-score-file "snake-scores"
  "File for holding high scores.")

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar snake-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar snake-snake-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [1 1 0])
     (color-tty "yellow"))))

(defvar snake-dot-options
  '(((glyph colorize)
     (t ?\*))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 0 0])
     (color-tty "red"))))

(defvar snake-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))))

(defvar snake-space-options
  '(((t ?\040))
    nil
    nil))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst snake-blank	0)
(defconst snake-snake	1)
(defconst snake-dot	2)
(defconst snake-border	3)
(defconst snake-space	4)

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local snake-length 0)
(defvar-local snake-velocity-x 1)
(defvar-local snake-velocity-y 0)
(defvar-local snake-positions nil)
(defvar-local snake-score 0)
(defvar-local snake-paused nil)
(defvar-local snake-moved-p nil)
(defvar-local snake-velocity-queue nil
  "This queue stores the velocities requested too quickly by user.
They will take effect one at a time at each clock-interval.
This is necessary for proper behavior.

For instance, if you are moving right, you press up and then left, you
want the snake to move up just once before starting to move left.  If
we implemented all your keystrokes immediately, the snake would
effectively never move up.  Thus, we need to move it up for one turn
and then start moving it leftwards.")

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar snake-mode-map
  (let ((map (make-sparse-keymap 'snake-mode-map)))

    (define-key map "n"		'snake-start-game)
    (define-key map "q"		'snake-end-game)
    (define-key map "p"		'snake-pause-game)

    (define-key map [left]	'snake-move-left)
    (define-key map [right]	'snake-move-right)
    (define-key map [up]	'snake-move-up)
    (define-key map [down]	'snake-move-down)

    (define-key map "\C-b"	'snake-move-left)
    (define-key map "\C-f"	'snake-move-right)
    (define-key map "\C-p"	'snake-move-up)
    (define-key map "\C-n"	'snake-move-down)
    map)
  "Keymap for Snake games.")

(defvar snake-null-map
  (let ((map (make-sparse-keymap 'snake-null-map)))
    (define-key map "n"		'snake-start-game)
    (define-key map "q"         'quit-window)
    map)
  "Keymap for finished Snake games.")

(defconst snake--menu-def
  '("Snake"
    ["Start new game" snake-start-game
     :help "Start a new Snake game"]
    ["End game"       snake-end-game
     :active (snake-active-p)
     :help "End the current Snake game"]
    ;; FIXME: Pause and resume from the menu currently doesn't work
    ;;        very well and is therefore disabled.  The game continues
    ;;        running while navigating the menu.  See also
    ;;        `tetris--menu-def' which has the same problem.
    ;; ["Pause"          snake-pause-game
    ;;  :active (and (snake-active-p) (not snake-paused))
    ;;  :help "Pause running Snake game"]
    ;; ["Resume"         snake-pause-game
    ;;  :active (and (snake-active-p) snake-paused)
    ;;  :help "Resume paused Snake game"]
    )
  "Menu for `snake'.  Used to initialize menus.")

(easy-menu-define
  snake-mode-menu snake-mode-map
  "Menu for running Snake games."
  snake--menu-def)

(easy-menu-define
  snake-null-menu snake-null-map
  "Menu for finished Snake games."
  snake--menu-def)

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun snake-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
	    (cond ((= c snake-blank)
		   snake-blank-options)
                  ((= c snake-snake)
		   snake-snake-options)
                  ((= c snake-dot)
		   snake-dot-options)
                  ((= c snake-border)
		   snake-border-options)
                  ((= c snake-space)
		   snake-space-options)
                  (t
		   '(nil nil nil)))))
    options))

(defun snake-update-score ()
  (let* ((string (format "Score:  %05d" snake-score))
	 (len (length string)))
    (dotimes (x len)
      (gamegrid-set-cell (+ snake-score-x x)
			 snake-score-y
			 (aref string x)))))

(defun snake-init-buffer ()
  (gamegrid-init-buffer snake-buffer-width
			snake-buffer-height
			snake-space)
  (let ((buffer-read-only nil))
    (dotimes (y snake-height)
      (dotimes (x snake-width)
        (gamegrid-set-cell x y snake-border)))
    (cl-loop for y from 1 to (- snake-height 2) do
             (cl-loop for x from 1 to (- snake-width 2) do
                      (gamegrid-set-cell x y snake-blank)))))

(defun snake-reset-game ()
  (gamegrid-kill-timer)
  (snake-init-buffer)
  (setq snake-length		snake-initial-length
	snake-velocity-x	snake-initial-velocity-x
	snake-velocity-y	snake-initial-velocity-y
	snake-positions		nil
	snake-score		0
	snake-paused		nil
	snake-moved-p           nil
	snake-velocity-queue    nil)
  (let ((x snake-initial-x)
	(y snake-initial-y))
    (dotimes (_ snake-length)
      (gamegrid-set-cell x y snake-snake)
      (setq snake-positions (cons (vector x y) snake-positions))
      (cl-incf x snake-velocity-x)
      (cl-incf y snake-velocity-y)))
  (snake-update-score))

(defun snake-set-dot ()
  (let ((x (random snake-width))
	(y (random snake-height)))
    (while (not (= (gamegrid-get-cell x y) snake-blank))
      (setq x (random snake-width))
      (setq y (random snake-height)))
    (gamegrid-set-cell x y snake-dot)))

(defun snake-update-game (snake-buffer)
  "Called on each clock tick.
Advances the snake one square, testing for collision.
Argument SNAKE-BUFFER is the name of the buffer."
  (when (and (not snake-paused)
	     (eq (current-buffer) snake-buffer))
    (snake-update-velocity)
    (let* ((pos (car snake-positions))
	   (x (+ (aref pos 0) snake-velocity-x))
	   (y (+ (aref pos 1) snake-velocity-y))
	   (c (gamegrid-get-cell x y)))
      (if (or (= c snake-border)
	      (= c snake-snake))
	  (snake-end-game)
	(cond ((= c snake-dot)
	       (cl-incf snake-length)
	       (cl-incf snake-score)
	       (snake-update-score)
	       (snake-set-dot))
	      (t
	       (let* ((last-cons (nthcdr (- snake-length 2)
					 snake-positions))
		      (tail-pos (cadr last-cons))
		      (x0 (aref tail-pos 0))
		      (y0 (aref tail-pos 1)))
		 (gamegrid-set-cell x0 y0 snake-blank)
		 (setcdr last-cons nil))))
	(gamegrid-set-cell x y snake-snake)
	(setq snake-positions
	      (cons (vector x y) snake-positions))
	(setq snake-moved-p nil)))))

(defun snake-update-velocity ()
  (unless snake-moved-p
    (if snake-velocity-queue
	(let ((new-vel (car (last snake-velocity-queue))))
	  (setq snake-velocity-x (car new-vel)
		snake-velocity-y (cadr new-vel))
	  (setq snake-velocity-queue
		(nreverse (cdr (nreverse snake-velocity-queue))))))
    (setq snake-moved-p t)))

(defun snake-final-x-velocity ()
  (or (caar snake-velocity-queue)
      snake-velocity-x))

(defun snake-final-y-velocity ()
  (or (cadr (car snake-velocity-queue))
      snake-velocity-y))

(defun snake-move-left ()
  "Make the snake move left."
  (interactive nil snake-mode)
  (when (zerop (snake-final-x-velocity))
    (push '(-1 0) snake-velocity-queue)))

(defun snake-move-right ()
  "Make the snake move right."
  (interactive nil snake-mode)
  (when (zerop (snake-final-x-velocity))
    (push '(1 0) snake-velocity-queue)))

(defun snake-move-up ()
  "Make the snake move up."
  (interactive nil snake-mode)
  (when (zerop (snake-final-y-velocity))
    (push '(0 -1) snake-velocity-queue)))

(defun snake-move-down ()
  "Make the snake move down."
  (interactive nil snake-mode)
  (when (zerop (snake-final-y-velocity))
    (push '(0 1) snake-velocity-queue)))

(defun snake-end-game ()
  "Terminate the current game."
  (interactive nil snake-mode)
  (gamegrid-kill-timer)
  (use-local-map snake-null-map)
  (gamegrid-add-score snake-score-file snake-score))

(defun snake-start-game ()
  "Start a new game of Snake."
  (interactive nil snake-mode)
  (snake-reset-game)
  (snake-set-dot)
  (use-local-map snake-mode-map)
  (gamegrid-start-timer snake-tick-period 'snake-update-game))

(defun snake-pause-game ()
  "Pause (or resume) the current game."
  (interactive nil snake-mode)
  (setq snake-paused (not snake-paused))
  (message (and snake-paused "Game paused (press p to resume)")))

(defun snake-active-p ()
  (eq (current-local-map) snake-mode-map))

(put 'snake-mode 'mode-class 'special)

(define-derived-mode snake-mode special-mode "Snake"
  "A mode for playing Snake."
  :interactive nil

  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)

  (use-local-map snake-null-map)

  (setq gamegrid-use-glyphs snake-use-glyphs-flag)
  (setq gamegrid-use-color snake-use-color-flag)

  (gamegrid-init (snake-display-options)))

;;;###autoload
(defun snake ()
  "Play the Snake game.
Move the snake around without colliding with its tail or with the border.

Eating dots causes the snake to get longer.

Snake mode keybindings:
   \\<snake-mode-map>
\\[snake-start-game]	Starts a new game of Snake
\\[snake-end-game]	Terminates the current game
\\[snake-pause-game]	Pauses (or resumes) the current game
\\[snake-move-left]	Makes the snake move left
\\[snake-move-right]	Makes the snake move right
\\[snake-move-up]	Makes the snake move up
\\[snake-move-down]	Makes the snake move down"
  (interactive)

  (switch-to-buffer snake-buffer-name)
  (gamegrid-kill-timer)
  (snake-mode)
  (snake-start-game))

(provide 'snake)

;;; snake.el ends here
