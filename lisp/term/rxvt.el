;;; rxvt.el --- define function key sequences and standard colors for rxvt

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Eli Zaretskii
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

;; Set up function-key-map entries that termcap and terminfo don't know.
(let ((map (make-sparse-keymap)))
  (define-key map "\e[A" [up])
  (define-key map "\e[B" [down])
  (define-key map "\e[C" [right])
  (define-key map "\e[D" [left])
  (define-key map "\e[7~" [home])
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

  (define-key map "\e[11^" [C-f1])
  (define-key map "\e[12^" [C-f2])
  (define-key map "\e[13^" [C-f3])
  (define-key map "\e[14^" [C-f4])
  (define-key map "\e[15^" [C-f5])
  (define-key map "\e[17^" [C-f6])
  (define-key map "\e[18^" [C-f7])
  (define-key map "\e[19^" [C-f8])
  (define-key map "\e[20^" [C-f9])
  (define-key map "\e[21^" [C-f10])
  (define-key map "\e[23^" [C-f11])
  (define-key map "\e[24^" [C-f12])

  (define-key map "\e[29~" [print])

  (define-key map "\e[2;2~" [S-insert])
  (define-key map "\e[3$" [S-delete])

  (define-key map "\e[2^" [C-insert])
  (define-key map "\e[3^" [C-delete])
  (define-key map "\e[5^" [C-prior])
  (define-key map "\e[6^" [C-next])
  (define-key map "\eOd" [C-left])
  (define-key map "\eOc" [C-right])
  (define-key map "\eOa" [C-up])
  (define-key map "\eOb" [C-down])

  (define-key map "\e[5$" [S-prior])
  (define-key map "\e[6$" [S-next])
  (define-key map "\e[8$" [S-end])
  (define-key map "\e[7$" [S-home])
  (define-key map "\e[d" [S-left])
  (define-key map "\e[c" [S-right])
  (define-key map "\e[a" [S-up])
  (define-key map "\e[b" [S-down])

  ;; Use inheritance to let the main keymap override those defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the .emacs file.
  (set-keymap-parent map (keymap-parent function-key-map))
  (set-keymap-parent function-key-map map))

;; Set up colors, for those versions of rxvt that support it.
(defvar rxvt-standard-colors
  ;; The names of the colors in the comments taken from the rxvt.1 man
  ;; page; the corresponding RGB values--from rgb.txt.
  '(("black"          0 (  0   0   0))	; black
    ("red"            1 (205   0   0))	; red3
    ("green"          2 (  0 205   0))	; green3
    ("yellow"         3 (205 205   0))	; yellow3
    ("blue"           4 (  0   0 205))	; blue3
    ("magenta"        5 (205   0 205))	; magenta3
    ("cyan"           6 (  0 205 205))	; cyan3
    ("white"          7 (250 235 215))	; AntiqueWhite
    ("brightblack"    8 ( 64  64  64))	; gray25
    ("brightred"      9 (255   0   0))	; red
    ("brightgreen"   10 (  0 255   0))	; green
    ("brightyellow"  11 (255 255   0))	; yellow
    ("brightblue"    12 (  0   0 255))	; blue
    ("brightmagenta" 13 (255   0 255))	; magenta
    ("brightcyan"    14 (  0 255 255))	; cyan
    ("brightwhite"   15 (255 255 255)))	; white
  "Names of 16 standard rxvt colors, their numbers, and RGB values.")

(defun rxvt-rgb-convert-to-16bit (prim)
  "Convert an 8-bit primary color value PRIM to a corresponding 16-bit value."
  (min 65535 (round (* (/ prim 255.0) 65535.0))))

(defun rxvt-register-default-colors ()
  "Register the default set of colors for rxvt or compatible emulator.

This function registers the number of colors returned by `display-color-cells'
for the currently selected frame."
  (let* ((ncolors (display-color-cells))
	 (colors rxvt-standard-colors)
	 (color (car colors)))
    (if (> ncolors 0)
	;; Clear the 8 default tty colors registered by startup.el
	(tty-color-clear))
    ;; Only register as many colors as are supported by the display.
    (while (and (> ncolors 0) colors)
      (tty-color-define (car color) (cadr color)
			(mapcar 'rxvt-rgb-convert-to-16bit
				(car (cddr color))))
      (setq colors (cdr colors)
	    color (car colors)
	    ncolors (1- ncolors)))
    ;; Modifying color mappings means realized faces don't use the
    ;; right colors, so clear them.
    (clear-face-cache)))

;; rxvt puts the default colors into an environment variable
;; COLORFGBG.  We use this to set the background mode in a more
;; intelligent way than the default guesswork in startup.el.
(defun rxvt-set-background-mode ()
  "Set background mode as appropriate for the default rxvt colors."
  (let ((fgbg (getenv "COLORFGBG"))
	bg rgb)
    (setq default-frame-background-mode 'light)
    (when (and fgbg
	       (string-match ".*;\\([0-9][0-9]?\\)\\'" fgbg))
      (setq bg (string-to-number (substring fgbg (match-beginning 1))))
      ;; The next line assumes that rxvt-standard-colors are ordered
      ;; by the color index in the ascending order!
      (setq rgb (car (cddr (nth bg rxvt-standard-colors))))
      ;; See the commentary in frame-set-background-mode about the
      ;; computation below.
      (if (< (apply '+ rgb)
	     ;; The following line assumes that white is the 15th
	     ;; color in rxvt-standard-colors.
	     (* (apply '+ (car (cddr (nth 15 rxvt-standard-colors)))) 0.6))
	  (setq default-frame-background-mode 'dark)))
    (frame-set-background-mode (selected-frame))))

;; Do it!
(rxvt-register-default-colors)
(rxvt-set-background-mode)
;; This recomputes all the default faces given the colors we've just set up.
(tty-set-up-initial-frame-faces)

;;; arch-tag: 20cf2fb6-6318-4bab-9dbf-1d15048f2257
;;; rxvt.el ends here
