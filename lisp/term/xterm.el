;;; xterm.el --- define function key sequences for xterm

;; Author: FSF
;; Keywords: terminals

;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Code:

(define-key function-key-map "\e[A" [up])
(define-key function-key-map "\e[B" [down])
(define-key function-key-map "\e[C" [right])
(define-key function-key-map "\e[D" [left])
(define-key function-key-map "\e[2~" [insert])
(define-key function-key-map "\e[4~" [select])
(define-key function-key-map "\e[5~" [prior])
(define-key function-key-map "\e[6~" [next])
(define-key function-key-map "\e[11~" [f1])
(define-key function-key-map "\e[12~" [f2])
(define-key function-key-map "\e[13~" [f3])
(define-key function-key-map "\e[14~" [f4])
(define-key function-key-map "\e[15~" [f5])
(define-key function-key-map "\e[17~" [f6])
(define-key function-key-map "\e[18~" [f7])
(define-key function-key-map "\e[19~" [f8])
(define-key function-key-map "\e[20~" [f9])
(define-key function-key-map "\e[21~" [f10])
(define-key function-key-map "\e[23~" [f11])
(define-key function-key-map "\e[24~" [f12])
(define-key function-key-map "\e[29~" [print])
