;;; xterm.el --- define function key sequences and standard colors for xterm

;; Copyright (C) 1995, 2002 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals

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

(let ((map (make-sparse-keymap)))
  (define-key map "\e[A" [up])
  (define-key map "\e[B" [down])
  (define-key map "\e[C" [right])
  (define-key map "\e[D" [left])
  (define-key map "\e[1~" [home])
  (define-key map "\e[2~" [insert])
  (define-key map "\e[3~" [delete])
  (define-key map "\e[4~" [select])
  (define-key map "\e[5~" [prior])
  (define-key map "\e[6~" [next])
  (define-key map "\e[11~" [f1])
  (define-key map "\e[12~" [f2])
  (define-key map "\e[13~" [f3])
  (define-key map "\e[14~" [f4])
  (define-key map "\e[15~" [f5])
  (define-key map "\e[17~" [f6])
  (define-key map "\e[18~" [f7])
  (define-key map "\e[19~" [f8])
  (define-key map "\e[20~" [f9])
  (define-key map "\e[21~" [f10])
  (define-key map "\e[23~" [f11])
  (define-key map "\e[24~" [f12])
  (define-key map "\e[29~" [print])

  (define-key map "\e[2;2~" [S-insert])
  (define-key map "\e[3;2~" [S-delete])
  (define-key map "\e[5;2~" [S-prior])
  (define-key map "\e[6;2~" [S-next])

  (define-key map "\e[2;5~" [C-insert])
  (define-key map "\e[3;5~" [C-delete])
  (define-key map "\e[5;5~" [C-prior])
  (define-key map "\e[6;5~" [C-next])

  (define-key map "\eOA" [up])
  (define-key map "\eOB" [down])
  (define-key map "\eOC" [right])
  (define-key map "\eOD" [left])
  (define-key map "\eOF" [end])
  (define-key map "\eOH" [home])

  (define-key map "\eO2A" [S-up])
  (define-key map "\eO2B" [S-down])
  (define-key map "\eO2C" [S-right])
  (define-key map "\eO2D" [S-left])
  (define-key map "\eO2F" [S-end])
  (define-key map "\eO2H" [S-home])

  (define-key map "\eO5A" [C-up])
  (define-key map "\eO5B" [C-down])
  (define-key map "\eO5C" [C-right])
  (define-key map "\eO5D" [C-left])
  (define-key map "\eO5F" [C-end])
  (define-key map "\eO5H" [C-home])

  ;; Use inheritance to let the main keymap override those defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the .emacs file.
  (set-keymap-parent map (keymap-parent function-key-map))
  (set-keymap-parent function-key-map map))

;; Set up colors, for those versions of xterm that support it.
(defvar xterm-standard-colors
  ;; The names in the comments taken from XTerm-col.ad in the xterm
  ;; distribution, see ftp://dickey.his.com/xterm/.  RGB values are
  ;; from rgb.txt.
  '(("black"          0 (  0   0   0))	; black
    ("red"            1 (205   0   0))	; red3
    ("green"          2 (  0 205   0))	; green3
    ("yellow"         3 (205 205   0))	; yellow3
    ("blue"           4 (  0   0 205))	; blue3
    ("magenta"        5 (205   0 205))	; magenta3
    ("cyan"           6 (  0 205 205))	; cyan3
    ("white"          7 (229 229 229))	; gray90
    ("brightblack"    8 ( 77  77  77))	; gray30
    ("brightred"      9 (255   0   0))	; red
    ("brightgreen"   10 (  0 255   0))	; green
    ("brightyellow"  11 (255 255   0))	; yellow
    ("brightblue"    12 (  0   0 255))	; blue
    ("brightmagenta" 13 (255   0 255))	; magenta
    ("brightcyan"    14 (  0 255 255))	; cyan
    ("brightwhite"   15 (255 255 255)))	; white
  "Names of 16 standard xterm/aixterm colors, their numbers, and RGB values.")

(defun xterm-rgb-convert-to-16bit (prim)
  "Convert an 8-bit primary color value PRIM to a corresponding 16-bit value."
  (min 65535 (round (* (/ prim 255.0) 65535.0))))

(defun xterm-register-default-colors ()
  "Register the default set of colors for xterm or compatible emulator.

This function registers the number of colors returned by `display-color-cells'
for the currently selected frame.  The first 16 colors are taken from
`xterm-standard-colors', which see, while the rest are computed assuming
either the 88- or 256-color standard color scheme supported by latest
versions of xterm."
  (let* ((ncolors (display-color-cells))
	 (colors xterm-standard-colors)
	 (color (car colors)))
    (if (> ncolors 0)
	;; Clear the 8 default tty colors registered by startup.el
	(tty-color-clear))
    ;; Only register as many colors as are supported by the display.
    (while (and (> ncolors 0) colors)
      (tty-color-define (car color) (cadr color)
			(mapcar 'xterm-rgb-convert-to-16bit
				(car (cddr color))))
      (setq colors (cdr colors)
	    color (car colors)
	    ncolors (1- ncolors)))
    ;; We've exhausted the colors from `xterm-standard-colors'.  If there
    ;; are more colors to support, compute them now.
    (when (> ncolors 0)
      (cond
       ((= ncolors 240)	; 256-color xterm
	;; 216 non-gray colors first
	(let ((r 0) (g 0) (b 0))
	  (while (> ncolors 24)
	    ;; This and other formulae taken from 256colres.pl and
	    ;; 88colres.pl in the xterm distribution.
	    (tty-color-define (format "color-%d" (- 256 ncolors))
			      (- 256 ncolors)
			      (mapcar 'xterm-rgb-convert-to-16bit
				      (list (round (* r 42.5))
					    (round (* g 42.5))
					    (round (* b 42.5)))))
	    (setq b (1+ b))
	    (if (> b 5)
		(setq g (1+ g)
		      b 0))
	    (if (> g 5)
		(setq r (1+ r)
		      g 0))
	    (setq ncolors (1- ncolors))))
	;; Now the 24 gray colors
	(while (> ncolors 0)
	  (setq color (xterm-rgb-convert-to-16bit (+ 8 (* (- 24 ncolors) 10))))
	  (tty-color-define (format "color-%d" (- 256 ncolors))
			    (- 256 ncolors)
			    (list color color color))
	  (setq ncolors (1- ncolors))))
       ((= ncolors 72)  ; 88-color xterm
	;; 64 non-gray colors
	(let ((levels '(0 139 205 255))
	      (r 0) (g 0) (b 0))
	  (while (> ncolors 8)
	    (tty-color-define (format "color-%d" (- 88 ncolors))
			      (- 88 ncolors)
			      (mapcar 'xterm-rgb-convert-to-16bit
				      (list (nth r levels)
					    (nth g levels)
					    (nth b levels))))
	    (setq b (1+ b))
	    (if (> b 3)
		(setq g (1+ g)
		      b 0))
	    (if (> g 3)
		(setq r (1+ r)
		      g 0))
	    (setq ncolors (1- ncolors))))
	;; Now the 8 gray colors
	(while (> ncolors 0)
	  (setq color (xterm-rgb-convert-to-16bit
		       (round
			(if (= ncolors 8)
			    46.36363636
			  (+ (* (- 8 ncolors) 23.18181818) 69.54545454)))))
	  (tty-color-define (format "color-%d" (- 88 ncolors))
			    (- 88 ncolors)
			    (list color color color))
	  (setq ncolors (1- ncolors))))
       (t (error "Unsupported number of xterm colors (%d)" (+ 16 ncolors)))))
    ;; Modifying color mappings means realized faces don't use the
    ;; right colors, so clear them.
    (clear-face-cache)))

;; rxvt puts the default colors into an environment variable
;; COLORFGBG.  We use this to set the background mode in a more
;; intelligent way than the default guesswork in startup.el.
(defun xterm-rxvt-set-background-mode ()
  "Set background mode as appropriate for the default rxvt colors."
  (let ((fgbg (getenv "COLORFGBG"))
	bg rgb)
    (setq frame-background-mode 'light)	; default
    (when (and fgbg
	       (string-match ".*;\\([0-9][0-9]?\\)\\'" fgbg))
      (setq bg (string-to-number (substring fgbg (match-beginning 1))))
      ;; The next line assumes that xterm-standard-colors are ordered
      ;; by the color index in the ascending order!
      (setq rgb (car (cddr (nth bg xterm-standard-colors))))
      ;; See the commentary in frame-set-background-mode about the
      ;; computation below.
      (if (< (apply '+ rgb)
	     ;; The following line assumes that white is the 15th
	     ;; color in xterm-standard-colors.
	     (* (apply '+ (car (cddr (nth 15 xterm-standard-colors)))) 0.6))
	  (setq frame-background-mode 'dark)))
    (frame-set-background-mode (selected-frame))))

;; Do it!
(xterm-register-default-colors)
;; If this xterm is actually a disguised rxvt, be more intelligent about
;; determining the background mode.
(and (getenv "COLORTERM")
     (string-match "\\`rxvt" (getenv "COLORTERM"))
     (xterm-rxvt-set-background-mode))
;; This recomputes all the default faces given the colors we've just set up.
(tty-set-up-initial-frame-faces)

;;; arch-tag: 12e7ebdd-1e6c-4b25-b0f9-35ace25e855a
;;; xterm.el ends here
