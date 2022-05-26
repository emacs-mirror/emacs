;;; indonesian.el --- Quail package for inputting Indonesian characters  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: समीर सिंह Sameer Singh <lumarzeli30@gmail.com>
;; Keywords: multilingual, input method, i18n, Indonesia

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

;; Input methods for Indonesian languages.

;;; Code:

(require 'quail)

;; This input method supports languages like Buginese, Balinese, Sundanese and
;; Javanese.

(quail-define-package
 "balinese" "Balinese" "ᬅ" t "Balinese phonetic input method.

 `\\=`' is used to switch levels instead of Alt-Gr.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1"  ?᭑)
 ("`1" ?1)
 ("`!" ?᭫)
 ("2"  ?᭒)
 ("`2" ?2)
 ("`@" ?᭬)
 ("3"  ?᭓)
 ("`3" ?3)
 ("`#" ?᭭)
 ("4"  ?᭔)
 ("`4" ?4)
 ("`$" ?᭮)
 ("5"  ?᭕)
 ("`5" ?5)
 ("`%" ?᭯)
 ("6"  ?᭖)
 ("`6" ?6)
 ("`^" ?᭰)
 ("7"  ?᭗)
 ("`7" ?7)
 ("`&" ?᭱)
 ("8"  ?᭘)
 ("`8" ?8)
 ("`*" ?᭲)
 ("9"  ?᭙)
 ("`9" ?9)
 ("`\(" ?᭳)
 ("0"  ?᭐)
 ("`0" ?0)
 ("`\)" ?᭼)
 ("`\\" ?᭞)
 ("`|" ?᭟)
 ("`"  ?ᬝ)
 ("q"  ?ᬝ)
 ("Q"  ?ᬞ)
 ("`q" ?᭚)
 ("`Q" ?᭽)
 ("w"  ?ᬟ)
 ("W"  ?ᬠ)
 ("`w" ?᭛)
 ("`W" ?᭾)
 ("e"  ?ᬾ)
 ("E"  ?ᬿ)
 ("`e" ?ᬏ)
 ("`E" ?ᬐ)
 ("r"  ?ᬭ)
 ("R"  ?ᬃ)
 ("`r" ?ᬺ)
 ("`R" ?ᬋ)
 ("t"  ?ᬢ)
 ("T"  ?ᬣ)
 ("`t" ?᭜)
 ("`T" ?᭝)
 ("y"  ?ᬬ)
 ("Y"  ?ᭂ)
 ("`y" ?ᭃ)
 ("`Y" ?᭴)
 ("u"  ?ᬸ)
 ("U"  ?ᬹ)
 ("`u" ?ᬉ)
 ("`U" ?ᬊ)
 ("i"  ?ᬶ)
 ("I"  ?ᬷ)
 ("`i" ?ᬇ)
 ("`I" ?ᬈ)
 ("o"  ?ᭀ)
 ("O"  ?ᭁ)
 ("`o" ?ᬑ)
 ("`O" ?ᬒ)
 ("p"  ?ᬧ)
 ("P"  ?ᬨ)
 ("`p" ?ᭈ)
 ("`P" ?᭠)
 ("a"  ?ᬵ)
 ("A"  ?ᬆ)
 ("`a" ?ᬅ)
 ("`A" ?᭵)
 ("s"  ?ᬲ)
 ("S"  ?ᬰ)
 ("`s" ?᭡)
 ("`S" ?᭢)
 ("d"  ?ᬤ)
 ("D"  ?ᬥ)
 ("`d" ?᭣)
 ("`D" ?᭤)
 ("f"  ?᭄)
 ("F"  ?ᬻ)
 ("`f" ?ᬌ)
 ("`F" ?᭶)
 ("g"  ?ᬕ)
 ("G"  ?ᬖ)
 ("`g" ?᭥)
 ("`G" ?᭦)
 ("h"  ?ᬳ)
 ("H"  ?ᬄ)
 ("`h" ?᭧)
 ("`H" ?᭨)
 ("j"  ?ᬚ)
 ("J"  ?ᬛ)
 ("`j" ?ᭌ)
 ("`J" ?᭩)
 ("k"  ?ᬓ)
 ("K"  ?ᬔ)
 ("`k" ?ᭅ)
 ("`K" ?ᭆ)
 ("l"  ?ᬮ)
 ("L"  ?ᬼ)
 ("`l" ?ᬍ)
 ("`L" ?᭪)
 ("z"  ?ᭊ)
 ("Z"  ?ᬽ)
 ("`z" ?ᬎ)
 ("`Z" ?᭷)
 ("x"  ?ᬱ)
 ("X"  ?᬴)
 ("`x" ?᭸)
 ("c"  ?ᬘ)
 ("C"  ?ᬙ)
 ("`c" #x200C)  ; ZWNJ
 ("v"  ?ᬯ)
 ("V"  ?ᭉ)
 ("`v" ?᭹)
 ("`V" ?᭺)
 ("b"  ?ᬩ)
 ("B"  ?ᬪ)
 ("`b" ?᭻)
 ("n"  ?ᬦ)
 ("N"  ?ᬡ)
 ("`n" ?ᬗ)
 ("`N" ?ᬜ)
 ("m"  ?ᬫ)
 ("M"  ?ᬂ)
 ("`m" ?ᬁ)
 ("`M" ?ᬀ))

(provide 'indonesian)
;;; indonesian.el ends here
