;;; File:  edt-vt100.el  ---  Enhanced EDT Keypad Mode Emulation
;;;                             for VT Series Terminals
;;;
;;;                          For GNU Emacs 19
;;;
;;
;; Copyright (C) 1986, 1992, 1993, 1995 Free Software Foundation, Inc.

;; Author: Kevin Gallagher <kgallagh@spd.dsccc.com>
;; Maintainer: Kevin Gallagher <kgallagh@spd.dsccc.com>
;; Keywords: emulations

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

;;; Usage:

;;  See edt-user.doc in the Emacs etc directory.

;; ====================================================================

;; Get keyboard function key mapping to EDT keys.
(load "edt-lk201" nil t)

(defun edt-set-screen-width-80 ()
  "Set screen width to 80 columns."
  (interactive)
  (vt100-wide-mode -1)
  (message "Screen width 80"))

(defun edt-set-screen-width-132 ()
  "Set screen width to 132 columns."
  (interactive)
  (vt100-wide-mode 1)
  (message "Screen width 132"))
