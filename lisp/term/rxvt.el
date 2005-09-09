;;; rxvt.el --- define function key sequences and standard colors for rxvt

;; Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'server)

(defvar rxvt-function-map nil
  "Function key overrides for rxvt.")

;; Protect against reloads.
(unless rxvt-function-map
  (setq rxvt-function-map (make-sparse-keymap))

  ;; Set up function-key-map entries that termcap and terminfo don't know.
  (define-key rxvt-function-map "\e[A" [up])
  (define-key rxvt-function-map "\e[B" [down])
  (define-key rxvt-function-map "\e[C" [right])
  (define-key rxvt-function-map "\e[D" [left])
  (define-key rxvt-function-map "\e[7~" [home])
  (define-key rxvt-function-map "\e[2~" [insert])
  (define-key rxvt-function-map "\e[3~" [delete])
  (define-key rxvt-function-map "\e[4~" [select])
  (define-key rxvt-function-map "\e[5~" [prior])
  (define-key rxvt-function-map "\e[6~" [next])
  (define-key rxvt-function-map "\e[11~" [f1])
  (define-key rxvt-function-map "\e[12~" [f2])
  (define-key rxvt-function-map "\e[13~" [f3])
  (define-key rxvt-function-map "\e[14~" [f4])
  (define-key rxvt-function-map "\e[15~" [f5])
  (define-key rxvt-function-map "\e[17~" [f6])
  (define-key rxvt-function-map "\e[18~" [f7])
  (define-key rxvt-function-map "\e[19~" [f8])
  (define-key rxvt-function-map "\e[20~" [f9])
  (define-key rxvt-function-map "\e[21~" [f10])
  ;; The strings emitted by f11 and f12 are the same as the strings
  ;; emitted by S-f1 and S-f2, so don't define f11 and f12.
  ;; (define-key rxvt-function-map "\e[23~" [f11])
  ;; (define-key rxvt-function-map "\e[24~" [f12])
  (define-key rxvt-function-map "\e[29~" [print])

  (define-key rxvt-function-map "\e[11^" [C-f1])
  (define-key rxvt-function-map "\e[12^" [C-f2])
  (define-key rxvt-function-map "\e[13^" [C-f3])
  (define-key rxvt-function-map "\e[14^" [C-f4])
  (define-key rxvt-function-map "\e[15^" [C-f5])
  (define-key rxvt-function-map "\e[17^" [C-f6])
  (define-key rxvt-function-map "\e[18^" [C-f7])
  (define-key rxvt-function-map "\e[19^" [C-f8])
  (define-key rxvt-function-map "\e[20^" [C-f9])
  (define-key rxvt-function-map "\e[21^" [C-f10])

  (define-key rxvt-function-map "\e[23~" [S-f1])
  (define-key rxvt-function-map "\e[24~" [S-f2])
  (define-key rxvt-function-map "\e[25~" [S-f3])
  (define-key rxvt-function-map "\e[26~" [S-f4])
  (define-key rxvt-function-map "\e[28~" [S-f5])
  (define-key rxvt-function-map "\e[29~" [S-f6])
  (define-key rxvt-function-map "\e[31~" [S-f7])
  (define-key rxvt-function-map "\e[32~" [S-f8])
  (define-key rxvt-function-map "\e[33~" [S-f9])
  (define-key rxvt-function-map "\e[34~" [S-f10])

  (define-key rxvt-function-map "\e[23^" [C-S-f1])
  (define-key rxvt-function-map "\e[24^" [C-S-f2])
  (define-key rxvt-function-map "\e[25^" [C-S-f3])
  (define-key rxvt-function-map "\e[26^" [C-S-f4])
  (define-key rxvt-function-map "\e[28^" [C-S-f5])
  (define-key rxvt-function-map "\e[29^" [C-S-f6])
  (define-key rxvt-function-map "\e[31^" [C-S-f7])
  (define-key rxvt-function-map "\e[32^" [C-S-f8])
  (define-key rxvt-function-map "\e[33^" [C-S-f9])
  (define-key rxvt-function-map "\e[34^" [C-S-f10])

  (define-key rxvt-function-map "\e[2^" [C-insert])
  (define-key rxvt-function-map "\e[3^" [C-delete])
  (define-key rxvt-function-map "\e[5^" [C-prior])
  (define-key rxvt-function-map "\e[6^" [C-next])
  (define-key rxvt-function-map "\e[7^" [C-home])
  (define-key rxvt-function-map "\e[8^" [C-end])
  (define-key rxvt-function-map "\eOd" [C-left])
  (define-key rxvt-function-map "\eOc" [C-right])
  (define-key rxvt-function-map "\eOa" [C-up])
  (define-key rxvt-function-map "\eOb" [C-down])

  (define-key rxvt-function-map "\e[2;2~" [S-insert])
  (define-key rxvt-function-map "\e[3$" [S-delete])
  (define-key rxvt-function-map "\e[5$" [S-prior])
  (define-key rxvt-function-map "\e[6$" [S-next])
  (define-key rxvt-function-map "\e[8$" [S-end])
  (define-key rxvt-function-map "\e[7$" [S-home])
  (define-key rxvt-function-map "\e[d" [S-left])
  (define-key rxvt-function-map "\e[c" [S-right])
  (define-key rxvt-function-map "\e[a" [S-up])
  (define-key rxvt-function-map "\e[b" [S-down]))

(defun terminal-init-rxvt ()
  "Terminal initialization function for rxvt."

  ;; The terminal-local stuff only need to be set up on the first
  ;; frame on that device.
  (when (eq 1 (length (frames-on-display-list)))
    ;; The terminal intialization C code file might have initialized
    ;; function keys F11->F42 from the termcap/terminfo information.  On
    ;; a PC-style keyboard these keys correspond to
    ;; MODIFIER-FUNCTION_KEY, where modifier is S-, C-, C-S-.  The
    ;; code here subsitutes the corresponding defintions in
    ;; function-key-map. This substitution is needed because if a key
    ;; definition if found in function-key-map, there are no further
    ;; lookups in other keymaps.
    (let ((m (terminal-local-value 'local-function-key-map nil)))
      (substitute-key-definition [f11] [S-f1] m)
      (substitute-key-definition [f12] [S-f2] m)
      (substitute-key-definition [f13] [S-f3] m)
      (substitute-key-definition [f14] [S-f4] m)
      (substitute-key-definition [f15] [S-f5] m)
      (substitute-key-definition [f16] [S-f6] m)
      (substitute-key-definition [f17] [S-f7] m)
      (substitute-key-definition [f18] [S-f8] m)
      (substitute-key-definition [f19] [S-f9] m)
      (substitute-key-definition [f20] [S-f10] m)
      
      (substitute-key-definition [f23] [C-f1] m)
      (substitute-key-definition [f24] [C-f2] m)
      (substitute-key-definition [f25] [C-f3] m)
      (substitute-key-definition [f26] [C-f4] m)
      (substitute-key-definition [f27] [C-f5] m)
      (substitute-key-definition [f28] [C-f6] m)
      (substitute-key-definition [f29] [C-f7] m)
      (substitute-key-definition [f30] [C-f8] m)
      (substitute-key-definition [f31] [C-f9] m)
      (substitute-key-definition [f32] [C-f10] m)

      (substitute-key-definition [f33] [C-S-f1] m)
      (substitute-key-definition [f34] [C-S-f2] m)
      (substitute-key-definition [f35] [C-S-f3] m)
      (substitute-key-definition [f36] [C-S-f4] m)
      (substitute-key-definition [f37] [C-S-f5] m)
      (substitute-key-definition [f38] [C-S-f6] m)
      (substitute-key-definition [f39] [C-S-f7] m)
      (substitute-key-definition [f40] [C-S-f8] m)
      (substitute-key-definition [f41] [C-S-f9] m)
      (substitute-key-definition [f42] [C-S-f10] m))

    ;; Use inheritance to let the main keymap override those defaults.
    ;; This way we don't override terminfo-derived settings or settings
    ;; made in the .emacs file.
    (let ((m (copy-keymap rxvt-function-map)))
      (set-keymap-parent m (keymap-parent (terminal-local-value 'local-function-key-map nil)))
      (set-keymap-parent (terminal-local-value 'local-function-key-map nil) m)))

  ;; Initialize colors and background mode.
  (rxvt-register-default-colors)
  (rxvt-set-background-mode)
  ;; This recomputes all the default faces given the colors we've just set up.
  (tty-set-up-initial-frame-faces))

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
  (let ((fgbg (server-getenv "COLORFGBG"))
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

;;; arch-tag: 20cf2fb6-6318-4bab-9dbf-1d15048f2257
;;; rxvt.el ends here
