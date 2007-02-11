;; leim-ext.el -- extra leim configuration	-*- coding:iso-2022-7bit; -*-

;; Copyright (C) 2004, 2005, 2006, 2007
;;   Free Software Foundation, Inc.
;; Copyright (C) 2004, 2005, 2006, 2007
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

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

;; Makefile in this directory appends the contents of this file (only
;; such non-empty lines that don't begin with ';') to the generated
;; file leim-list.el.

;;; Code:

(eval-after-load "quail/PY-b5"
  '(quail-defrule "ling2" ?$(0!r(B nil t))

;; Enable inputting full-width space (U+3000).
(eval-after-load "quail/Punct"
  '(quail-defrule " " ?$A!!(B nil t))
(eval-after-load "quail/Punct-b5"
  '(quail-defrule " " ?$(0!!(B nil t))

(autoload 'ucs-input-activate "quail/uni-input"
  "Activate UCS input method.
With arg, activate UCS input method if and only if arg is positive.

While this input method is active, the variable
`input-method-function' is bound to the function `ucs-input-method'.")

(register-input-method "ucs" "UTF-8" 'ucs-input-activate "U+"
		       "Unicode input as hex in the form Uxxxx.")

;; arch-tag: 75cfdfc7-de85-44f9-b408-ff67d1ec664e
