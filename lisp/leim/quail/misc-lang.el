;;; misc-lang.el --- Quail package for inputting Miscellaneous characters  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: समीर सिंह Sameer Singh <lumarzeli30@gmail.com>
;; Keywords: multilingual, input method, i18n, Miscellaneous

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Input methods for Miscellaneous languages.

;;; Code:

(require 'quail)

(quail-define-package
 "hanifi-rohingya" "Hanifi Rohingya" "𐴌𐴟" t "Hanifi Rohingya phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1"  ?𐴱)
 ("`1" ?1)
 ("2"  ?𐴲)
 ("`2" ?2)
 ("3"  ?𐴳)
 ("`3" ?3)
 ("4"  ?𐴴)
 ("`4" ?4)
 ("5"  ?𐴵)
 ("`5" ?5)
 ("6"  ?𐴶)
 ("`6" ?6)
 ("7"  ?𐴷)
 ("`7" ?7)
 ("8"  ?𐴸)
 ("`8" ?8)
 ("9"  ?𐴹)
 ("`9" ?9)
 ("0"  ?𐴰)
 ("`0" ?0)
 ("q"  ?𐴄)
 ("w"  ?𐴋)
 ("W"  ?𐴍)
 ("e"  ?𐴠)
 ("E"  ?𐴤)
 ("r"  ?𐴌)
 ("R"  ?𐴥)
 ("t"  ?𐴃)
 ("T"  ?𐴦)
 ("y"  ?𐴘)
 ("Y"  ?𐴙)
 ("u"  ?𐴟)
 ("U"  ?𐴧)
 ("i"  ?𐴞)
 ("o"  ?𐴡)
 ("p"  ?𐴂)
 ("a"  ?𐴀)
 ("A"  ?𐴝)
 ("s"  ?𐴏)
 ("S"  ?𐴐)
 ("d"  ?𐴊)
 ("f"  ?𐴉)
 ("F"  ?𐴢)
 ("g"  ?𐴒)
 ("h"  ?𐴇)
 ("j"  ?𐴅)
 ("k"  ?𐴑)
 ("K"  ?𐴈)
 ("l"  ?𐴓)
 ("z"  ?𐴎)
 ("c"  ?𐴆)
 ("C"  #x200C) ; ZWNJ
 ("v"  ?𐴖)
 ("V"  ?𐴗)
 ("`v" ?𐴜)
 ("b"  ?𐴁)
 ("n"  ?𐴕)
 ("N"  ?𐴚)
 ("`n" ?𐴛)
 ("`N" ?𐴣)
 ("m"  ?𐴔))

(provide 'misc-lang)
;;; misc-lang.el ends here
