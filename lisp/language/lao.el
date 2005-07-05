;;; lao.el --- support for Lao -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 2001  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2002
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, Lao

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

(make-coding-system
 'lao 2 ?L
 "8-bit encoding for ASCII (MSB=0) and LAO (MSB=1)."
 '(ascii lao nil nil
   nil nil nil nil nil nil nil nil nil nil nil t)
 '((safe-charsets ascii lao)
   (post-read-conversion . lao-post-read-conversion)))

(set-language-info-alist
 "Lao" '((charset lao)
	 (coding-system lao)
	 (coding-priority lao)
	 (input-method . "lao")
	 (nonascii-translation . lao)
	 (unibyte-display . lao)
	 (features lao-util)
	 (documentation . t)))

(aset use-default-ascent ?(1;(B t)
(aset use-default-ascent ?$,1D;(B t)
(aset use-default-ascent ?(1=(B t)
(aset use-default-ascent ?$,1D=(B t)
(aset use-default-ascent ?(1?(B t)
(aset use-default-ascent ?$,1D?(B t)
(aset use-default-ascent ?(1B(B t)
(aset use-default-ascent ?$,1DB(B t)
(aset ignore-relative-composition ?(1\(B t)
(aset ignore-relative-composition ?$,1D\(B t)

;; Register a function to compose Lao characters.
(let ((patterns '(("\\c0\\c9?\\(\\(\\c2\\|\\c3\\)\\c4?\\|\\c4\\)?"
	 . lao-composition-function))))
  (aset composition-function-table (make-char 'lao) patterns)
  (dotimes (i (1+ (- #xeff #xe80)))
    (aset composition-function-table (decode-char 'ucs (+ i #xe80)) patterns)))

(provide 'lao)

;;; arch-tag: ba540fd9-6352-4449-a9cd-669afd21fa57
;;; lao.el ends here
