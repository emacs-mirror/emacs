;;; edt-vt100.el --- enhanced EDT keypad mode emulation for VT series terminals

;; Copyright (C) 1986, 1992, 1993, 1995, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Kevin Gallagher <Kevin.Gallagher@boeing.com>
;; Maintainer: Kevin Gallagher <Kevin.Gallagher@boeing.com>
;; Keywords: emulations

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;  See edt-user.doc in the Emacs etc directory.

;; ====================================================================


;;; Code:

;; Get keyboard function key mapping to EDT keys.
(load "edt-lk201" nil t)

;; The following functions are called by the EDT screen width commands defined
;; in edt.el.

(declare-function vt100-wide-mode "../term/vt100" (&optional arg))

(defun edt-set-term-width-80 ()
  "Set terminal width to 80 columns."
  (vt100-wide-mode -1))

(defun edt-set-term-width-132 ()
  "Set terminal width to 132 columns."
  (vt100-wide-mode 1))

;; arch-tag: c9f10c95-915f-44b5-93ff-4654abca4dd4
;;; edt-vt100.el ends here
