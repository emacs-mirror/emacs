;; pc-win.el -- setup support for `PC windows' (whatever that is).

;; Copyright (C) 1994, 1996 Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@diku.dk>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(load "term/internal" nil t)

;; Color translation -- doesn't really need to be fast.
;; Colors listed here do not include the "light-",
;; "medium-" and "dark-" prefixes that are accounted for
;; by `msdos-color-translate', which see below).

(defvar msdos-color-aliases
  '(("snow"		. "white")
    ("ghost white"	. "white")
    ("ghostwhite"	. "white")
    ("white smoke"	. "white")
    ("whitesmoke"	. "white")
    ("gainsboro"	. "white")
    ("floral white"	. "white")
    ("floralwhite"	. "white")
    ("old lace"		. "white")
    ("oldlace"		. "white")
    ("linen"		. "white")
    ("antique white"	. "white")
    ("antiquewhite"	. "white")
    ("papaya whip"	. "white")
    ("papayawhip"	. "white")
    ("blanched almond"	. "white")
    ("blanchedalmond"	. "white")
    ("bisque"		. "white")
    ("peach puff"	. "lightred")
    ("peachpuff"	. "lightred")
    ("navajo white"	. "lightred")
    ("navajowhite"	. "lightred")
    ("moccasin"		. "lightred")
    ("cornsilk"		. "white")
    ("ivory"		. "white")
    ("lemon chiffon"	. "yellow")
    ("lemonchiffon"	. "yellow")
    ("seashell"		. "white")
    ("honeydew"		. "white")
    ("mint cream"	. "white")
    ("mintcream"	. "white")
    ("azure"		. "lightcyan")
    ("alice blue"	. "lightcyan")
    ("aliceblue"	. "lightcyan")
    ("lavender"		. "lightcyan")
    ("lavender blush"	. "lightcyan")
    ("lavenderblush"	. "lightcyan")
    ("misty rose"	. "lightred")
    ("mistyrose"	. "lightred")
    ("aquamarine" 	. "blue")
    ("cadet blue"	. "blue")
    ("cadetblue"	. "blue")
    ("cornflower blue"	. "lightblue")
    ("cornflowerblue"	. "lightblue")
    ("midnight blue"	. "blue")
    ("midnightblue"	. "blue")
    ("navy blue"	. "cyan")
    ("navyblue"		. "cyan")
    ("navy"		. "cyan")
    ("sky blue"		. "lightblue")
    ("skyblue"		. "lightblue")
    ("dodger blue"	. "blue")
    ("dodgerblue"	. "blue")
    ("powder blue"	. "lightblue")
    ("powderblue"	. "lightblue")
    ("slate blue"	. "cyan")
    ("slateblue"	. "cyan")
    ("steel blue"	. "blue")
    ("steelblue"	. "blue")
    ("coral"		. "lightred")
    ("firebrick"	. "red")
    ("gold"		. "yellow")
    ("goldenrod"	. "yellow")
    ("pale goldenrod"	. "yellow")
    ("palegoldenrod"	. "yellow")
    ("olive green"	. "lightgreen")
    ("olivegreen"	. "lightgreen")
    ("olive drab"	. "green")
    ("olivedrab"	. "green")
    ("forest green"	. "green")
    ("forestgreen"	. "green")
    ("lime green"	. "lightgreen")
    ("limegreen"	. "lightgreen")
    ("sea green"	. "lightcyan")
    ("seagreen"		. "lightcyan")
    ("spring green"	. "green")
    ("springgreen"	. "green")
    ("pale green"	. "lightgreen")
    ("palegreen"	. "lightgreen")
    ("lawn green"	. "lightgreen")
    ("lawngreen"	. "lightgreen")
    ("chartreuse"	. "yellow")
    ("yellow green"	. "lightgreen")
    ("yellowgreen"	. "lightgreen")
    ("green yellow"	. "lightgreen")
    ("greenyellow"	. "lightgreen")
    ("slate grey"	. "lightgray")
    ("slategrey"	. "lightgray")
    ("slate gray"	. "lightgray")
    ("slategray"	. "lightgray")
    ("dim grey"		. "darkgray")
    ("dimgrey"		. "darkgray")
    ("dim gray"		. "darkgray")
    ("dimgray"		. "darkgray")
    ("light grey"	. "lightgray")
    ("lightgrey"	. "lightgray")
    ("light gray"	. "lightgray")
    ("gray"		. "darkgray")
    ("grey"		. "darkgray")
    ("gray80"		. "darkgray")
    ("gray50"		. "black")
    ("gray90"		. "darkgray")
    ("khaki"		. "green")
    ("maroon"		. "red")
    ("orange"		. "brown")
    ("orchid"		. "brown")
    ("saddle brown"	. "red")
    ("saddlebrown"	. "red")
    ("sienna"		. "red")
    ("peru"		. "red")
    ("pink"		. "lightred")
    ("plum"		. "magenta")
    ("indian red"	. "red")
    ("indianred"	. "red")
    ("violet red"	. "magenta")
    ("violetred"	. "magenta")
    ("orange red"	. "red")
    ("orangered"	. "red")
    ("salmon"		.  "lightred")
    ("sienna"		. "lightred")
    ("tan"		. "lightred")
    ("thistle"		. "magenta")
    ("turquoise"	. "lightgreen")
    ("pale turquoise"	. "cyan")
    ("paleturquoise"	. "cyan")
    ("violet"		. "magenta")
    ("blue violet"	. "lightmagenta")
    ("blueviolet"	. "lightmagenta")
    ("wheat"		. "white")
    ("green yellow"	. "yellow")
    ("greenyellow"	. "yellow")
    ("purple"		. "magenta")
    ("royalblue"	. "blue")
    ("grey40"		. "darkgray")
    ("rosybrown"	. "brown")
    ("rosy brown"	. "brown")
    ("beige"		. "brown"))
  "List of alternate names for colors.")

(defun msdos-color-translate (name)
  (setq name (downcase name))
  (let* ((len (length name))
	 (val (- (length x-colors)
		 (length (member name x-colors))))
	 (try))
    (if (or (< val 0) (>= val (length x-colors))) (setq val nil))
    (or val
	(and (setq try (cdr (assoc name msdos-color-aliases)))
	     (msdos-color-translate try))
	(and (> len 5)
	     (string= "light" (substring name 0 5))
	     (setq try (msdos-color-translate (substring name 5)))
	     (logior try 8))
	(and (> len 6)
	     (string= "light " (substring name 0 6))
	     (setq try (msdos-color-translate (substring name 6)))
	     (logior try 8))
	(and (> len 6)
	     (string= "medium" (substring name 0 6))
	     (msdos-color-translate (substring name 6)))
	(and (> len 7)
	     (string= "medium " (substring name 0 7))
	     (msdos-color-translate (substring name 7)))
	(and (> len 4)
	     (string= "dark" (substring name 0 4))
	     (msdos-color-translate (substring name 4)))
	(and (> len 5)
	     (string= "dark " (substring name 0 5))
	     (msdos-color-translate (substring name 5))))))
;; ---------------------------------------------------------------------------
;; We want to delay setting frame parameters until the faces are setup
(defvar default-frame-alist nil)
(modify-frame-parameters terminal-frame default-frame-alist)

(defun msdos-face-setup ()
  (modify-frame-parameters terminal-frame default-frame-alist)

  (set-face-foreground 'bold "yellow" terminal-frame)
  (set-face-foreground 'italic "red" terminal-frame)
  (set-face-foreground 'bold-italic "lightred" terminal-frame)
  (set-face-foreground 'underline "white" terminal-frame)
  (set-face-background 'region "green" terminal-frame)

  (make-face 'msdos-menu-active-face)
  (make-face 'msdos-menu-passive-face)
  (make-face 'msdos-menu-select-face)
  (set-face-foreground 'msdos-menu-active-face "white" terminal-frame)
  (set-face-foreground 'msdos-menu-passive-face "lightgray" terminal-frame)
  (set-face-background 'msdos-menu-active-face "blue" terminal-frame)
  (set-face-background 'msdos-menu-passive-face "blue" terminal-frame)
  (set-face-background 'msdos-menu-select-face "red" terminal-frame))

;; We have only one font, so...
(add-hook 'before-init-hook 'msdos-face-setup)

;; We create frames as if we were a terminal, but with a twist.
(defun make-msdos-frame (&optional parameters)
  (let ((parms
	 (append initial-frame-alist default-frame-alist parameters nil)))
    (make-terminal-frame parms)))

(setq frame-creation-function 'make-msdos-frame)

;; ---------------------------------------------------------------------------
;; More or less useful imitations of certain X-functions.  A lot of the
;; values returned are questionable, but usually only the form of the
;; returned value matters.  Also, by the way, recall that `ignore' is
;; a useful function for returning 'nil regardless of argument.

;; From src/xfns.c
(defun x-display-color-p (&optional display) 't)
(defun x-list-fonts (pattern &optional face frame) (list "default"))
(defun x-color-defined-p (color) (numberp (msdos-color-translate color)))
(defun x-display-pixel-width (&optional frame) (frame-width frame))
(defun x-display-pixel-height (&optional frame) (frame-height frame))
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
(defvar x-colors '("black"
		   "blue"
		   "green"
		   "cyan"
		   "red"
		   "magenta"
		   "brown"
		   "lightgray"
		   "darkgray"
		   "lightblue"
		   "lightgreen"
		   "lightcyan"
		   "lightred"
		   "lightmagenta"
		   "yellow"
		   "white")
  "The list of colors available on a PC display under MS-DOS.")
(defun x-defined-colors (&optional frame)
  "Return a list of colors supported for a particular frame.
The argument FRAME specifies which frame to try.
The value may be different for frames on different X displays."
  x-colors)
;
;; From lisp/select.el
(defun x-get-selection (&rest rest) "")
(fset 'x-set-selection 'ignore)

;; From lisp/faces.el: we only have one font, so always return
;; it, no matter which variety they've asked for.
(defun x-frob-font-slant (font which)
  font)

;; From lisp/frame.el
(fset 'set-default-font 'ignore)
(fset 'set-mouse-color 'ignore)		; We cannot, I think.
(fset 'set-cursor-color 'ignore)	; Hardware determined by char under.
(fset 'set-border-color 'ignore)	; Not useful.
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
