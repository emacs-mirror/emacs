;;; arabic.el --- Quail package for inputting Arabic	-*- coding: utf-8;-*-

;; Copyright (C) 2007 Free Software Foundation, Inc.
;; Written by James Cloos <cloos@jhcloos.com>
;; Keywords: mule, input method, Arabic

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

(require 'quail)

(quail-define-package
 "arabic" "Arabic" "ع" nil "Arabic input method.

Based on Arabic table in X Keyboard Configuration DB.
" nil t t t t nil nil nil nil nil t)

;;  ذّ 1! 2@ 3# 4$ 5% 6^ 7& 8* 9) 0( -_ =+
;;      ضَ صً ثُ قٌ فﻹ غإ ع` ه÷ خ× ح؛ ج< د> <>
;;       شِ سٍ ي] ب[ لﻷ اأ تـ ن، م/ ك: ط"
;;        ئ~ ءْ ؤ} ر{ ﻻﻵ ىآ ة' و, ز. ظ؟
;;

(quail-define-rules
 ("`" ?ذ)

 ("Q" ?َ)
 ("W" ?ً)
 ("E" ?ُ)
 ("R" ?ٌ)
 ("T" ?ﻹ)
 ("Y" ?إ)
 ("U" ?`)
 ("I" ?÷)
 ("O" ?×)
 ("P" ?؛)

 ("A" ?ِ)
 ("S" ?ٍ)
 ("D" ?])
 ("F" ?[)
 ("G" ?ﻷ)
 ("H" ?أ)
 ("J" ?ـ)
 ("K" ?،)
 ("L" ?/)
 (";" ?:)

 ("Z" ?~)
 ("X" ?ْ)
 ("C" ?})
 ("V" ?{)
 ("B" ?ﻵ)
 ("N" ?آ)
 ("M" ?')
 ("<" ?,)
 (">" ?.)
 ("?" ?؟)

 ("`" ?ذ)

 ("q" ?ض)
 ("w" ?ص)
 ("e" ?ث)
 ("r" ?ق)
 ("t" ?ف)
 ("y" ?غ)
 ("u" ?ع)
 ("i" ?ه)
 ("o" ?خ)
 ("p" ?ح)

 ("a" ?ش)
 ("s" ?س)
 ("d" ?ي)
 ("f" ?ب)
 ("g" ?ل)
 ("h" ?ا)
 ("j" ?ت)
 ("k" ?ن)
 ("l" ?م)
 (";" ?ك)

 ("z" ?ئ)
 ("x" ?ء)
 ("c" ?ؤ)
 ("v" ?ر)
 ("b" ?ﻻ)
 ("n" ?ى)
 ("m" ?ة)
 ("," ?و)
 ("." ?ز)
 ("/" ?ظ)

 ("'" ?ط))

;; arch-tag: 7e81e2af-5ea5-417f-bfe7-dfa6fd955cb5
;;; arabic.el ends here
