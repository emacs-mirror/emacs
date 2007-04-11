;;; xterm.el --- define function key sequences and standard colors for xterm

;; Copyright (C) 1995, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defun terminal-init-xterm ()
  "Terminal initialization function for xterm."
  ;; rxvt terminals sometimes set the TERM variable to "xterm", but
  ;; rxvt's keybindings that are incompatible with xterm's. It is
  ;; better in that case to use rxvt's initializion function.
  (if (and (getenv "COLORTERM")
	   (string-match "\\`rxvt" (getenv "COLORTERM")))
      (progn 
	(eval-and-compile (load "term/rxvt"))
	(terminal-init-rxvt))

    ;; The terminal intialization C code file might have initialized
    ;; function keys F13->F60 from the termcap/terminfo information.  On
    ;; a PC-style keyboard these keys correspond to
    ;; MODIFIER-FUNCTION_KEY, where modifier is S-, C, A-, C-S-.  The
    ;; code here subsitutes the corresponding defintions in
    ;; function-key-map. This substitution is needed because if a key
    ;; definition is found in function-key-map, there are no further
    ;; lookups in other keymaps.
    (substitute-key-definition [f13] [S-f1] function-key-map)
    (substitute-key-definition [f14] [S-f2] function-key-map)
    (substitute-key-definition [f15] [S-f3] function-key-map)
    (substitute-key-definition [f16] [S-f4] function-key-map)
    (substitute-key-definition [f17] [S-f5] function-key-map)
    (substitute-key-definition [f18] [S-f6] function-key-map)
    (substitute-key-definition [f19] [S-f7] function-key-map)
    (substitute-key-definition [f20] [S-f8] function-key-map)
    (substitute-key-definition [f21] [S-f9] function-key-map)
    (substitute-key-definition [f22] [S-f10] function-key-map)
    (substitute-key-definition [f23] [S-f11] function-key-map)
    (substitute-key-definition [f24] [S-f12] function-key-map)

    (substitute-key-definition [f25] [C-f1] function-key-map)
    (substitute-key-definition [f26] [C-f2] function-key-map)
    (substitute-key-definition [f27] [C-f3] function-key-map)
    (substitute-key-definition [f28] [C-f4] function-key-map)
    (substitute-key-definition [f29] [C-f5] function-key-map)
    (substitute-key-definition [f30] [C-f6] function-key-map)
    (substitute-key-definition [f31] [C-f7] function-key-map)
    (substitute-key-definition [f32] [C-f8] function-key-map)
    (substitute-key-definition [f33] [C-f9] function-key-map)
    (substitute-key-definition [f34] [C-f10] function-key-map)
    (substitute-key-definition [f35] [C-f11] function-key-map)
    (substitute-key-definition [f36] [C-f12] function-key-map)

    (substitute-key-definition [f37] [C-S-f1] function-key-map)
    (substitute-key-definition [f38] [C-S-f2] function-key-map)
    (substitute-key-definition [f39] [C-S-f3] function-key-map)
    (substitute-key-definition [f40] [C-S-f4] function-key-map)
    (substitute-key-definition [f41] [C-S-f5] function-key-map)
    (substitute-key-definition [f42] [C-S-f6] function-key-map)
    (substitute-key-definition [f43] [C-S-f7] function-key-map)
    (substitute-key-definition [f44] [C-S-f8] function-key-map)
    (substitute-key-definition [f45] [C-S-f9] function-key-map)
    (substitute-key-definition [f46] [C-S-f10] function-key-map)
    (substitute-key-definition [f47] [C-S-f11] function-key-map)
    (substitute-key-definition [f48] [C-S-f12] function-key-map)

    (substitute-key-definition [f49] [A-f1] function-key-map)
    (substitute-key-definition [f50] [A-f2] function-key-map)
    (substitute-key-definition [f51] [A-f3] function-key-map)
    (substitute-key-definition [f52] [A-f4] function-key-map)
    (substitute-key-definition [f53] [A-f5] function-key-map)
    (substitute-key-definition [f54] [A-f6] function-key-map)
    (substitute-key-definition [f55] [A-f7] function-key-map)
    (substitute-key-definition [f56] [A-f8] function-key-map)
    (substitute-key-definition [f57] [A-f9] function-key-map)
    (substitute-key-definition [f58] [A-f10] function-key-map)
    (substitute-key-definition [f59] [A-f11] function-key-map)
    (substitute-key-definition [f60] [A-f12] function-key-map)

    (let ((map (make-sparse-keymap)))
      ;; xterm from X.org 6.8.2 uses these key definitions.
      (define-key map "\eOP" [f1])
      (define-key map "\eOQ" [f2])
      (define-key map "\eOR" [f3])
      (define-key map "\eOS" [f4])
      (define-key map "\e[15~" [f5])
      (define-key map "\e[17~" [f6])
      (define-key map "\e[18~" [f7])
      (define-key map "\e[19~" [f8])
      (define-key map "\e[20~" [f9])
      (define-key map "\e[21~" [f10])
      (define-key map "\e[23~" [f11])
      (define-key map "\e[24~" [f12])

      (define-key map "\eO2P" [S-f1])
      (define-key map "\eO2Q" [S-f2])
      (define-key map "\eO2R" [S-f3])
      (define-key map "\eO2S" [S-f4])
      (define-key map "\e[1;2P" [S-f1])
      (define-key map "\e[1;2Q" [S-f2])
      (define-key map "\e[1;2R" [S-f3])
      (define-key map "\e[1;2S" [S-f4])
      (define-key map "\e[15;2~" [S-f5])
      (define-key map "\e[17;2~" [S-f6])
      (define-key map "\e[18;2~" [S-f7])
      (define-key map "\e[19;2~" [S-f8])
      (define-key map "\e[20;2~" [S-f9])
      (define-key map "\e[21;2~" [S-f10])
      (define-key map "\e[23;2~" [S-f11])
      (define-key map "\e[24;2~" [S-f12])

      (define-key map "\eO5P" [C-f1])
      (define-key map "\eO5Q" [C-f2])
      (define-key map "\eO5R" [C-f3])
      (define-key map "\eO5S" [C-f4])
      (define-key map "\e[15;5~" [C-f5])
      (define-key map "\e[17;5~" [C-f6])
      (define-key map "\e[18;5~" [C-f7])
      (define-key map "\e[19;5~" [C-f8])
      (define-key map "\e[20;5~" [C-f9])
      (define-key map "\e[21;5~" [C-f10])
      (define-key map "\e[23;5~" [C-f11])
      (define-key map "\e[24;5~" [C-f12])

      (define-key map "\eO6P" [C-S-f1])
      (define-key map "\eO6Q" [C-S-f2])
      (define-key map "\eO6R" [C-S-f3])
      (define-key map "\eO6S" [C-S-f4])
      (define-key map "\e[15;6~" [C-S-f5])
      (define-key map "\e[17;6~" [C-S-f6])
      (define-key map "\e[18;6~" [C-S-f7])
      (define-key map "\e[19;6~" [C-S-f8])
      (define-key map "\e[20;6~" [C-S-f9])
      (define-key map "\e[21;6~" [C-S-f10])
      (define-key map "\e[23;6~" [C-S-f11])
      (define-key map "\e[24;6~" [C-S-f12])

      (define-key map "\eO3P" [A-f1])
      (define-key map "\eO3Q" [A-f2])
      (define-key map "\eO3R" [A-f3])
      (define-key map "\eO3S" [A-f4])
      (define-key map "\e[15;3~" [A-f5])
      (define-key map "\e[17;3~" [A-f6])
      (define-key map "\e[18;3~" [A-f7])
      (define-key map "\e[19;3~" [A-f8])
      (define-key map "\e[20;3~" [A-f9])
      (define-key map "\e[21;3~" [A-f10])
      (define-key map "\e[23;3~" [A-f11])
      (define-key map "\e[24;3~" [A-f12])

      (define-key map "\eOA" [up])
      (define-key map "\eOB" [down])
      (define-key map "\eOC" [right])
      (define-key map "\eOD" [left])
      (define-key map "\eOF" [end])
      (define-key map "\eOH" [home])

      (define-key map "\e[1;2A" [S-up])
      (define-key map "\e[1;2B" [S-down])
      (define-key map "\e[1;2C" [S-right])
      (define-key map "\e[1;2D" [S-left])
      (define-key map "\e[1;2F" [S-end])
      (define-key map "\e[1;2H" [S-home])

      (define-key map "\e[1;5A" [C-up])
      (define-key map "\e[1;5B" [C-down])
      (define-key map "\e[1;5C" [C-right])
      (define-key map "\e[1;5D" [C-left])
      (define-key map "\e[1;5F" [C-end])
      (define-key map "\e[1;5H" [C-home])

      (define-key map "\e[1;6A" [C-S-up])
      (define-key map "\e[1;6B" [C-S-down])
      (define-key map "\e[1;6C" [C-S-right])
      (define-key map "\e[1;6D" [C-S-left])
      (define-key map "\e[1;6F" [C-S-end])
      (define-key map "\e[1;6H" [C-S-home])

      (define-key map "\e[1;3A" [A-up])
      (define-key map "\e[1;3B" [A-down])
      (define-key map "\e[1;3C" [A-right])
      (define-key map "\e[1;3D" [A-left])
      (define-key map "\e[1;3F" [A-end])
      (define-key map "\e[1;3H" [A-home])

      (define-key map "\e[2~" [insert])
      (define-key map "\e[3~" [delete])
      (define-key map "\e[5~" [prior])
      (define-key map "\e[6~" [next])

      (define-key map "\e[2;2~" [S-insert])
      (define-key map "\e[3;2~" [S-delete])
      (define-key map "\e[5;2~" [S-prior])
      (define-key map "\e[6;2~" [S-next])

      (define-key map "\e[2;5~" [C-insert])
      (define-key map "\e[3;5~" [C-delete])
      (define-key map "\e[5;5~" [C-prior])
      (define-key map "\e[6;5~" [C-next])

      (define-key map "\e[2;6~" [C-S-insert])
      (define-key map "\e[3;6~" [C-S-delete])
      (define-key map "\e[5;6~" [C-S-prior])
      (define-key map "\e[6;6~" [C-S-next])

      (define-key map "\e[2;3~" [A-insert])
      (define-key map "\e[3;3~" [A-delete])
      (define-key map "\e[5;3~" [A-prior])
      (define-key map "\e[6;3~" [A-next])

      (define-key map "\e[4~" [select])
      (define-key map "\e[29~" [print])

      (define-key map "\eOj" [kp-multiply])
      (define-key map "\eOk" [kp-add])
      (define-key map "\eOl" [kp-separator])
      (define-key map "\eOm" [kp-subtract])
      (define-key map "\eOo" [kp-divide])
      (define-key map "\eOp" [kp-0])
      (define-key map "\eOq" [kp-1])
      (define-key map "\eOr" [kp-2])
      (define-key map "\eOs" [kp-3])
      (define-key map "\eOt" [kp-4])
      (define-key map "\eOu" [kp-5])
      (define-key map "\eOv" [kp-6])
      (define-key map "\eOw" [kp-7])
      (define-key map "\eOx" [kp-8])
      (define-key map "\eOy" [kp-9])

      ;; These keys are available in xterm starting from version 216
      ;; if the modifyOtherKeys resource is set to 1.

      (define-key map "\e[27;5;9~"   [C-tab])
      (define-key map "\e[27;5;13~"  [C-return])
      (define-key map "\e[27;5;39~"  [?\C-\'])
      (define-key map "\e[27;5;44~"  [?\C-,])
      (define-key map "\e[27;5;45~"  [?\C--])
      (define-key map "\e[27;5;46~"  [?\C-.])
      (define-key map "\e[27;5;47~"  [?\C-/])
      (define-key map "\e[27;5;48~"  [?\C-0])
      (define-key map "\e[27;5;49~"  [?\C-1])
      ;; Not all C-DIGIT keys have a distinct binding.
      (define-key map "\e[27;5;57~"  [?\C-9])
      (define-key map "\e[27;5;59~"  [?\C-\;])
      (define-key map "\e[27;5;61~"  [?\C-=])
      (define-key map "\e[27;5;92~"  [?\C-\\])

      (define-key map "\e[27;6;33~"  [?\C-!])
      (define-key map "\e[27;6;34~"  [?\C-\"])
      (define-key map "\e[27;6;35~"  [?\C-#])
      (define-key map "\e[27;6;36~"  [?\C-$])
      (define-key map "\e[27;6;37~"  [?\C-%])
      (define-key map "\e[27;6;38~"  [?\C-&])
      (define-key map "\e[27;6;40~"  [?\C-(])
      (define-key map "\e[27;6;41~"  [?\C-)])
      (define-key map "\e[27;6;42~"  [?\C-*])
      (define-key map "\e[27;6;43~"  [?\C-+])
      (define-key map "\e[27;6;58~"  [?\C-:])
      (define-key map "\e[27;6;60~"  [?\C-<])
      (define-key map "\e[27;6;62~"  [?\C->])
      (define-key map "\e[27;6;63~"  [(control ??)])

      ;; These are the strings emitted for various C-M- combinations
      ;; for keyboards that the Meta and Alt modifiers are on the same
      ;; key (usually labeled "Alt").
      (define-key map "\e[27;13;9~"  [C-M-tab])
      (define-key map "\e[27;13;13~" [C-M-return])

      (define-key map "\e[27;13;39~" [?\C-\M-\'])
      (define-key map "\e[27;13;44~" [?\C-\M-,])
      (define-key map "\e[27;13;45~" [?\C-\M--])
      (define-key map "\e[27;13;46~" [?\C-\M-.])
      (define-key map "\e[27;13;47~" [?\C-\M-/])
      (define-key map "\e[27;13;48~" [?\C-\M-0])
      (define-key map "\e[27;13;49~" [?\C-\M-1])
      (define-key map "\e[27;13;50~" [?\C-\M-2])
      (define-key map "\e[27;13;51~" [?\C-\M-3])
      (define-key map "\e[27;13;52~" [?\C-\M-4])
      (define-key map "\e[27;13;53~" [?\C-\M-5])
      (define-key map "\e[27;13;54~" [?\C-\M-6])
      (define-key map "\e[27;13;55~" [?\C-\M-7])
      (define-key map "\e[27;13;56~" [?\C-\M-8])
      (define-key map "\e[27;13;57~" [?\C-\M-9])
      (define-key map "\e[27;13;59~" [?\C-\M-\;])
      (define-key map "\e[27;13;61~" [?\C-\M-=])
      (define-key map "\e[27;13;92~" [?\C-\M-\\])

      (define-key map "\e[27;14;33~"  [?\C-\M-!])
      (define-key map "\e[27;14;34~"  [?\C-\M-\"])
      (define-key map "\e[27;14;35~"  [?\C-\M-#])
      (define-key map "\e[27;14;36~"  [?\C-\M-$])
      (define-key map "\e[27;14;37~"  [?\C-\M-%])
      (define-key map "\e[27;14;38~"  [?\C-\M-&])
      (define-key map "\e[27;14;40~"  [?\C-\M-(])
      (define-key map "\e[27;14;41~"  [?\C-\M-)])
      (define-key map "\e[27;14;42~"  [?\C-\M-*])
      (define-key map "\e[27;14;43~"  [?\C-\M-+])
      (define-key map "\e[27;14;58~"  [?\C-\M-:])
      (define-key map "\e[27;14;60~"  [?\C-\M-<])
      (define-key map "\e[27;14;62~"  [?\C-\M->])
      (define-key map "\e[27;14;63~"  [(control meta ??)])

      (define-key map "\e[27;7;9~"  [C-M-tab])
      (define-key map "\e[27;7;13~" [C-M-return])

      (define-key map "\e[27;7;32~" [?\C-\M-\s])
      (define-key map "\e[27;7;39~" [?\C-\M-\'])
      (define-key map "\e[27;7;44~" [?\C-\M-,])
      (define-key map "\e[27;7;45~" [?\C-\M--])
      (define-key map "\e[27;7;46~" [?\C-\M-.])
      (define-key map "\e[27;7;47~" [?\C-\M-/])
      (define-key map "\e[27;7;48~" [?\C-\M-0])
      (define-key map "\e[27;7;49~" [?\C-\M-1])
      (define-key map "\e[27;7;50~" [?\C-\M-2])
      (define-key map "\e[27;7;51~" [?\C-\M-3])
      (define-key map "\e[27;7;52~" [?\C-\M-4])
      (define-key map "\e[27;7;53~" [?\C-\M-5])
      (define-key map "\e[27;7;54~" [?\C-\M-6])
      (define-key map "\e[27;7;55~" [?\C-\M-7])
      (define-key map "\e[27;7;56~" [?\C-\M-8])
      (define-key map "\e[27;7;57~" [?\C-\M-9])
      (define-key map "\e[27;7;59~" [?\C-\M-\;])
      (define-key map "\e[27;7;61~" [?\C-\M-=])
      (define-key map "\e[27;7;92~" [?\C-\M-\\])

      (define-key map "\e[27;8;33~"  [?\C-\M-!])
      (define-key map "\e[27;8;34~"  [?\C-\M-\"])
      (define-key map "\e[27;8;35~"  [?\C-\M-#])
      (define-key map "\e[27;8;36~"  [?\C-\M-$])
      (define-key map "\e[27;8;37~"  [?\C-\M-%])
      (define-key map "\e[27;8;38~"  [?\C-\M-&])
      (define-key map "\e[27;8;40~"  [?\C-\M-(])
      (define-key map "\e[27;8;41~"  [?\C-\M-)])
      (define-key map "\e[27;8;42~"  [?\C-\M-*])
      (define-key map "\e[27;8;43~"  [?\C-\M-+])
      (define-key map "\e[27;8;58~"  [?\C-\M-:])
      (define-key map "\e[27;8;60~"  [?\C-\M-<])
      (define-key map "\e[27;8;62~"  [?\C-\M->])
      (define-key map "\e[27;8;63~"  [(control meta ??)])

      (define-key map "\e[27;2;9~"   [S-tab])
      (define-key map "\e[27;2;13~"  [S-return])

      (define-key map "\e[27;6;9~"   [C-S-tab])
      (define-key map "\e[27;6;13~"  [C-S-return])

      ;; Other versions of xterm might emit these.
      (define-key map "\e[A" [up])
      (define-key map "\e[B" [down])
      (define-key map "\e[C" [right])
      (define-key map "\e[D" [left])
      (define-key map "\e[1~" [home])

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

      (define-key map "\e[11~" [f1])
      (define-key map "\e[12~" [f2])
      (define-key map "\e[13~" [f3])
      (define-key map "\e[14~" [f4])

      ;; Use inheritance to let the main keymap override those defaults.
      ;; This way we don't override terminfo-derived settings or settings
      ;; made in the .emacs file.
      (set-keymap-parent map (keymap-parent function-key-map))
      (set-keymap-parent function-key-map map))

    ;; Do it!
    (xterm-register-default-colors)
    ;; This recomputes all the default faces given the colors we've just set up.
    (tty-set-up-initial-frame-faces)))

;; Set up colors, for those versions of xterm that support it.
(defvar xterm-standard-colors
  ;; The names in the comments taken from XTerm-col.ad in the xterm
  ;; distribution, see ftp://dickey.his.com/xterm/.  RGB values are
  ;; from rgb.txt.
  '(("black"          0 (  0   0   0))	; black
    ("red"            1 (205   0   0))	; red3
    ("green"          2 (  0 205   0))	; green3
    ("yellow"         3 (205 205   0))	; yellow3
    ("blue"           4 (  0   0 238))	; blue2
    ("magenta"        5 (205   0 205))	; magenta3
    ("cyan"           6 (  0 205 205))	; cyan3
    ("white"          7 (229 229 229))	; gray90
    ("brightblack"    8 (127 127 127))	; gray50
    ("brightred"      9 (255   0   0))	; red
    ("brightgreen"   10 (  0 255   0))	; green
    ("brightyellow"  11 (255 255   0))	; yellow
    ("brightblue"    12 (92   92 255))	; rgb:5c/5c/ff
    ("brightmagenta" 13 (255   0 255))	; magenta
    ("brightcyan"    14 (  0 255 255))	; cyan
    ("brightwhite"   15 (255 255 255)))	; white
  "Names of 16 standard xterm/aixterm colors, their numbers, and RGB values.")

(defun xterm-rgb-convert-to-16bit (prim)
  "Convert an 8-bit primary color value PRIM to a corresponding 16-bit value."
  (logior prim (lsh prim 8)))

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
				      (list (if (zerop r) 0 (+ (* r 40) 55))
					    (if (zerop g) 0 (+ (* g 40) 55))
					    (if (zerop b) 0 (+ (* b 40) 55)))))

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
		       (floor
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

;; arch-tag: 12e7ebdd-1e6c-4b25-b0f9-35ace25e855a
;;; xterm.el ends here
