;;; float-sup.el --- define some constants useful for floating point numbers.

;; Copyright (C) 1985, 1986, 1987, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

;;; Code:

;; Provide a meaningful error message if we are running on
;; bare (non-float) emacs.

(if (fboundp 'atan)
    nil
  (error "Floating point was disabled at compile time"))

;; provide an easy hook to tell if we are running with floats or not.
;; define pi and e via math-lib calls. (much less prone to killer typos.)
(defconst pi (* 4 (atan 1)) "The value of Pi (3.1415926...).")

;; It's too inconvenient to make `e' a constant because it's used as
;; a temporary variable all the time.
(defvar e (exp 1) "The value of e (2.7182818...).")

(defconst degrees-to-radians (/ pi 180.0)
  "Degrees to radian conversion constant.")
(defconst radians-to-degrees (/ 180.0 pi)
  "Radian to degree conversion constant.")

;; these expand to a single multiply by a float when byte compiled

(defmacro degrees-to-radians (x)
  "Convert ARG from degrees to radians."
  (list '* (/ pi 180.0) x))
(defmacro radians-to-degrees (x)
  "Convert ARG from radians to degrees."
  (list '* (/ 180.0 pi) x))

(provide 'lisp-float-type)

;; arch-tag: e7837072-a4af-4d08-9953-8a3e755abf9d
;;; float-sup.el ends here
