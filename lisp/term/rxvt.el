;;; rxvt.el --- define function key sequences and standard colors for rxvt

;; Copyright (C) 2002-2018 Free Software Foundation, Inc.

;; Author: Eli Zaretskii
;; Keywords: terminals

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

(require 'term/xterm)

(defvar rxvt-function-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map xterm-rxvt-function-map)

    ;; Set up input-decode-map entries that termcap and terminfo don't know.
    (define-key map "\e[7~" [home])
    (define-key map "\e[8~" [end])
    ;; The strings emitted by f11 and f12 are the same as the strings
    ;; emitted by S-f1 and S-f2, so don't define f11 and f12.
    ;; (define-key rxvt-function-map "\e[23~" [f11])
    ;; (define-key rxvt-function-map "\e[24~" [f12])
    (define-key map "\e[23~" [S-f1])
    (define-key map "\e[24~" [S-f2])

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

    (define-key map "\e[25~" [S-f3])
    (define-key map "\e[26~" [S-f4])
    (define-key map "\e[28~" [S-f5])
    (define-key map "\e[29~" [S-f6])
    (define-key map "\e[31~" [S-f7])
    (define-key map "\e[32~" [S-f8])
    (define-key map "\e[33~" [S-f9])
    (define-key map "\e[34~" [S-f10])

    (define-key map "\e[23^" [C-S-f1])
    (define-key map "\e[24^" [C-S-f2])
    (define-key map "\e[25^" [C-S-f3])
    (define-key map "\e[26^" [C-S-f4])
    (define-key map "\e[28^" [C-S-f5])
    (define-key map "\e[29^" [C-S-f6])
    (define-key map "\e[31^" [C-S-f7])
    (define-key map "\e[32^" [C-S-f8])
    (define-key map "\e[33^" [C-S-f9])
    (define-key map "\e[34^" [C-S-f10])

    (define-key map "\e[2^" [C-insert])
    (define-key map "\e[3^" [C-delete])
    (define-key map "\e[5^" [C-prior])
    (define-key map "\e[6^" [C-next])
    (define-key map "\e[7^" [C-home])
    (define-key map "\e[8^" [C-end])
    (define-key map "\eOd" [C-left])
    (define-key map "\eOc" [C-right])
    (define-key map "\eOa" [C-up])
    (define-key map "\eOb" [C-down])

    (define-key map "\e[3$" [S-delete])
    (define-key map "\e[5$" [S-prior])
    (define-key map "\e[6$" [S-next])
    (define-key map "\e[7$" [S-home])
    (define-key map "\e[8$" [S-end])
    (define-key map "\e[d" [S-left])
    (define-key map "\e[c" [S-right])
    (define-key map "\e[a" [S-up])
    (define-key map "\e[b" [S-down])
    map)
  "Function key overrides for rxvt.")

(defvar rxvt-alternatives-map
  (let ((map (make-sparse-keymap)))
    ;; The terminal initialization C code file might have initialized
    ;; function keys F11->F42 from the termcap/terminfo information.  On
    ;; a PC-style keyboard these keys correspond to
    ;; MODIFIER-FUNCTION_KEY, where modifier is S-, C-, C-S-.  The
    ;; code here substitutes the corresponding definitions in
    ;; function-key-map.  This substitution is needed because if a key
    ;; definition if found in function-key-map, there are no further
    ;; lookups in other keymaps.
    (define-key map [f11] [S-f1])
    (define-key map [f12] [S-f2])
    (define-key map [f13] [S-f3])
    (define-key map [f14] [S-f4])
    (define-key map [f15] [S-f5])
    (define-key map [f16] [S-f6])
    (define-key map [f17] [S-f7])
    (define-key map [f18] [S-f8])
    (define-key map [f19] [S-f9])
    (define-key map [f20] [S-f10])

    (define-key map [f23] [C-f1])
    (define-key map [f24] [C-f2])
    (define-key map [f25] [C-f3])
    (define-key map [f26] [C-f4])
    (define-key map [f27] [C-f5])
    (define-key map [f28] [C-f6])
    (define-key map [f29] [C-f7])
    (define-key map [f30] [C-f8])
    (define-key map [f31] [C-f9])
    (define-key map [f32] [C-f10])

    (define-key map [f33] [C-S-f1])
    (define-key map [f34] [C-S-f2])
    (define-key map [f35] [C-S-f3])
    (define-key map [f36] [C-S-f4])
    (define-key map [f37] [C-S-f5])
    (define-key map [f38] [C-S-f6])
    (define-key map [f39] [C-S-f7])
    (define-key map [f40] [C-S-f8])
    (define-key map [f41] [C-S-f9])
    (define-key map [f42] [C-S-f10])
    map)
  "Keymap of possible alternative meanings for some keys.")

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
    ("white"          7 (229 229 229))	; gray90
    ("brightblack"    8 ( 77  77  77))	; gray30
    ("brightred"      9 (255   0   0))	; red
    ("brightgreen"   10 (  0 255   0))	; green
    ("brightyellow"  11 (255 255   0))	; yellow
    ("brightblue"    12 (  0   0 255))	; blue
    ("brightmagenta" 13 (255   0 255))	; magenta
    ("brightcyan"    14 (  0 255 255))	; cyan
    ("brightwhite"   15 (255 255 255)))	; white
  "Names of 16 standard rxvt colors, their numbers, and RGB values.")

(defun terminal-init-rxvt ()
  "Terminal initialization function for rxvt."

  (xterm--push-map rxvt-alternatives-map local-function-key-map)
  (xterm--push-map rxvt-function-map input-decode-map)

  ;; Initialize colors and background mode.
  (xterm-register-default-colors rxvt-standard-colors)
  (rxvt-set-background-mode)
  ;; This recomputes all the default faces given the colors we've just set up.
  (tty-set-up-initial-frame-faces))

;; rxvt puts the default colors into an environment variable
;; COLORFGBG.  We use this to set the background mode in a more
;; intelligent way than the default guesswork in startup.el.
(defun rxvt-set-background-mode ()
  "Set background mode as appropriate for the default rxvt colors."
  (let ((fgbg (getenv "COLORFGBG"))
	bg rgb)
    (set-terminal-parameter nil 'background-mode 'light)
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
	  (set-terminal-parameter nil 'background-mode 'dark)))))

(provide 'term/rxvt)

;;; rxvt.el ends here
