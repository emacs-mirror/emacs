;;; File:  edt-lk201.el  ---  Enhanced EDT Keypad Mode Emulation
;;;                             for LK-201 Keyboards
;;;
;;;                          For GNU Emacs 19
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

;;  See edt-user.doc

;; ====================================================================

;;;
;;;  Revision Information
;;;
(defconst edt-lk201-revision "$Revision: 1.2 $"
    "Revision Number of EDT LK-201 Keyboard Support.")

;;;;
;;;; KEY TRANSLATIONS
;;;;

;; Associate EDT keynames with Emacs terminal function vector names.
;; (Function key vector names for LK-201 are found in lisp/term/lk201.el.)
;;
;; F1 - F5 are not available on many DEC VT series terminals.
;; However, this is not always the case.  So support for F1 - F5 is
;; provided here and in lisp/term/lk201.el.
(defconst *EDT-keys*
  '(("KP0" . [kp-0]) ("KP1" . [kp-1]) ("KP2" . [kp-2]) ("KP3" . [kp-3]) 
    ("KP4" . [kp-4]) ("KP5" . [kp-5]) ("KP6" . [kp-6]) ("KP7" . [kp-7])
    ("KP8" . [kp-8]) ("KP9" . [kp-9]) ("KP," . [kp-separator])
    ("KP-" . [kp-subtract]) ("KPP" . [kp-decimal]) ("KPE" . [kp-enter])
    ("PF1" . [kp-f1]) ("PF2" . [kp-f2]) ("PF3" . [kp-f3]) ("PF4" . [kp-f4])
    ("UP" . [up]) ("DOWN" . [down]) ("RIGHT" . [right]) ("LEFT" . [left])
    ("FIND" . [find]) ("INSERT" . [insert]) ("REMOVE" . [delete])
    ("SELECT" . [select]) ("PREVIOUS" . [prior]) ("NEXT" . [next])
    ("F1" . [f1]) ("F2" . [f2]) ("F3" . [f3]) ("F4" . [f4]) ("F5" . [f5])
    ("F6" . [f6]) ("F7" . [f7]) ("F8" . [f8]) ("F9" . [f9]) ("F10" . [f10])
    ("F11" . [f11]) ("F12" . [f12]) ("F13" . [f13]) ("F14" . [f14])
    ("HELP" . [help]) ("DO" . [menu]) ("F17" . [f17]) ("F18" . [f18])
    ("F19" . [f19]) ("F20" . [f20])))
