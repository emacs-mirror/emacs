;;; xterm.el --- define function key sequences for xterm

;; Copyright (C) 1995 Free Software Foundation, Inc.

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

  (define-key map "\eO5A" [C-up])
  (define-key map "\eO5B" [C-down])
  (define-key map "\eO5C" [C-right])
  (define-key map "\eO5D" [C-left])

  ;; Use inheritance to let the main keymap override those defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the .emacs file.
  (set-keymap-parent map (keymap-parent function-key-map))
  (set-keymap-parent function-key-map map))

;;; xterm.el ends here
