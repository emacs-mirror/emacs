;;; vt100-led.el --- functions for LED control on VT-100 terminals & clones

;; Copyright (C) 1988, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
;; Keywords: hardware

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

(defvar led-state (make-vector 5 nil)
   "The internal state of the LEDs.  Choices are nil, t, `flash'.
Element 0 is not used.")

(defun led-flash (l)
  "Flash LED l."
  (aset led-state l 'flash)
  (led-update))

(defun led-off (&optional l)
  "Turn off vt100 led number L.  With no argument, turn them all off."
  (interactive "P")
  (if l
      (aset led-state (prefix-numeric-value l) nil)
    (fillarray led-state nil))
  (led-update))

(defun led-on (l)
  "Turn on LED L."
  (aset led-state l t)
  (led-update))

(defun led-update ()
  "Update the terminal's LEDs to reflect the internal state."
  (let ((f "\e[?0")			; String to flash.
	(o "\e[0")			; String for steady on.
	(l 1))				; Current LED number.
    (while (/= l 5)
      (let ((s (aref led-state l)))
	(cond
	 ((eq s 'flash)
	  (setq f (concat f ";" (int-to-string l))))
	 (s
	  (setq o (concat o ";" (int-to-string l))))))
      (setq l (1+ l)))
    (setq o (concat o "q" f "t"))
    (send-string-to-terminal o)))

(provide 'vt100-led)

;;; arch-tag: 346e6480-5e31-4234-aafe-257cea4a36d1
;;; vt100-led.el ends here
