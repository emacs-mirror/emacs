;; pc-win.el -- setup support for `PC windows' (whatever that is).

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@diku.dk>
;; Version: 1,00

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
;; ---------------------------------------------------------------------------
(load "term/internal" nil t)

;; Color translation -- doesn't really need to be fast

(defvar msdos-color-aliases
  '(("purple"         . "magenta")
    ("firebrick"      . "red")		; ?
    ("pink"           . "lightred")
    ("royalblue"      . "blue")
    ("cadetblue"      . "blue")
    ("forestgreen"    . "green")
    ("darkolivegreen" . "green")
    ("darkgoldenrod"  . "brown")
    ("goldenrod"      . "yellow")
    ("grey40"         . "darkgray")
    ("rosybrown"      . "brown"))
  "List of alternate names for colors.")

(defun msdos-color-translate (name)
  (setq name (downcase name))
  (let* ((len (length name))
	 (val (cdr (assoc name
			 '(("black" . 0)
			   ("blue" . 1)
			   ("green" . 2)
			   ("cyan" . 3)
			   ("red" . 4)
			   ("magenta" . 5)
			   ("brown" . 6)
			   ("lightgray" . 7) ("light gray" . 7)
			   ("darkgray" . 8) ("dark gray" . 8)
			   ("lightblue" . 9)
			   ("lightgreen" . 10)
			   ("lightcyan" . 11)
			   ("lightred" . 12)
			   ("lightmagenta" . 13)
			   ("yellow" . 14)
			   ("white" . 15)))))
	 (try))
    (or val
	(and (setq try (cdr (assoc name msdos-color-aliases)))
	     (msdos-color-translate try))
	(and (> len 5)
	     (string= "light" (substring name 0 4))
	     (setq try (msdos-color-translate (substring name 5)))
	     (logior try 8))
	(and (> len 6)
	     (string= "light " (substring name 0 5))
	     (setq try (msdos-color-translate (substring name 6)))
	     (logior try 8))
	(and (> len 4)
	     (string= "dark" (substring name 0 3))
	     (msdos-color-translate (substring name 4)))
	(and (> len 5)
	     (string= "dark " (substring name 0 4))
	     (msdos-color-translate (substring name 5))))))
;; ---------------------------------------------------------------------------
;; We want to delay setting frame parameters until the faces are setup
(defvar default-frame-alist nil)

(defun msdos-face-setup ()
  (modify-frame-parameters (selected-frame) default-frame-alist)

  (set-face-foreground 'bold "white")
  (set-face-foreground 'italic "red")
  (set-face-foreground 'bold-italic "yellow")
  (set-face-foreground 'underline "blue")
  (set-face-background 'region "green")

  (make-face 'msdos-menu-active-face)
  (make-face 'msdos-menu-passive-face)
  (make-face 'msdos-menu-select-face)
  (set-face-foreground 'msdos-menu-active-face "white")
  (set-face-foreground 'msdos-menu-passive-face "lightgray")
  (set-face-background 'msdos-menu-active-face "blue")
  (set-face-background 'msdos-menu-passive-face "blue")
  (set-face-background 'msdos-menu-select-face "red"))

;; We have only one font, so...
(add-hook 'before-init-hook 'msdos-face-setup)
;; ---------------------------------------------------------------------------
;; More or less useful immitations of certain X-functions.  A lot of the
;; values returned are questionable, but usually only the form of the
;; returned value matters.  Also, by the way, recall that `ignore' is
;; a useful function for returning 'nil regardless of argument.

;; From src/xfns.c
(defun x-display-color-p (&optional display) 't)
(fset 'focus-frame 'ignore)
(fset 'unfocus-frame 'ignore)
(defun x-list-fonts (pattern &optional face frame) (list "default"))
(defun x-color-defined-p (color) (numberp (msdos-color-translate color)))
(defun x-display-pixel-width (&optional frame) (* 8 (frame-width frame)))
(defun x-display-pixel-height (&optional frame) (* 8 (frame-height frame)))
(defun x-display-planes (&optional frame) 4) ; 3 for background, actually
(defun x-display-color-cells (&optional frame) 16) ; ???
(defun x-server-max-request-size (&optional frame) 1000000) ; ???
(defun x-server-vendor (&optional frame) t "GNU")
(defun x-server-version (&optional frame) '(1 0 0))
(defun x-display-screens (&optional frame) 1)
(defun x-display-mm-height (&optional frame) 200) ; Guess the size of my
(defun x-display-mm-width (&optional frame) 253)  ; monitor, MW...
(defun x-display-backing-store (&optional frame) 'not-useful)
(defun x-display-visual-class (&optional frame) 'static-color)
(fset 'x-display-save-under 'ignore)
(fset 'x-get-resource 'ignore)

;; From lisp/term/x-win.el
(setq x-display-name "pc")
(setq split-window-keep-point t)

;; From lisp/select.el
(defun x-get-selection (&rest rest) "")
(fset 'x-set-selection 'ignore)

;; From lisp/frame.el
(fset 'set-default-font 'ignore)
(fset 'set-mouse-color 'ignore)		; We cannot, I think.
(fset 'set-cursor-color 'ignore)	; Hardware determined by char under.
(fset 'set-border-color 'ignore)	; Not useful.
(fset 'auto-raise-mode 'ignore)
(fset 'auto-lower-mode 'ignore)
(defun set-background-color (color-name)
  "Set the background color of the selected frame to COLOR.
When called interactively, prompt for the name of the color to use."
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'background-color color-name))))
(defun set-foreground-color (color-name)
  "Set the foreground color of the selected frame to COLOR.
When called interactively, prompt for the name of the color to use."
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'foreground-color color-name))))
;; ---------------------------------------------------------------------------
;; Handle the X-like command line parameters "-fg" and "-bg"
(defun msdos-handle-args (args)
  (let ((rest nil))
    (while args
      (let ((this (car args)))
	(setq args (cdr args))
	(cond ((or (string= this "-fg") (string= this "-foreground"))
	       (if args
		   (setq default-frame-alist
			 (cons (cons 'foreground-color (car args))
			       default-frame-alist)
			 args (cdr args))))
	      ((or (string= this "-bg") (string= this "-background"))
	       (if args
		   (setq default-frame-alist
			 (cons (cons 'background-color (car args))
			       default-frame-alist)
			 args (cdr args))))
	      (t (setq rest (cons this rest))))))
    (nreverse rest)))

(setq command-line-args (msdos-handle-args command-line-args))
;; ---------------------------------------------------------------------------
(require 'faces)
(if (msdos-mouse-p)
    (progn
      (require 'menu-bar)
      (menu-bar-mode t)))
