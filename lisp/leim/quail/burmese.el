;;; burmese.el --- Quail package for inputting Burmese	-*- coding: utf-8; lexical-binding:t -*-

;; Copyright (C) 2007-2026 Free Software Foundation, Inc.

;; Author: Billy Lei <LeiYiXia29@outlook.com>
;; Keywords:  multilingual, input method, Burmese

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

;;; Code:

(require 'quail)


(quail-define-package
 "burmese" "Burmese" "MY" nil "Burmese input method (in phonetic order)." nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?၁)
 ("2" ?၂)
 ("3" ?၃)
 ("4" ?၄)
 ("5" ?၅)
 ("6" ?၆)
 ("7" ?၇)
 ("8" ?၈)
 ("9" ?၉)
 ("0" ?၀)

 ("!" ?ဍ)
 ("@" ?ၒ)
 ("#" ?ဋ)
 ("$" ?ၓ)
 ("%" ?ၔ)
 ("^" ?ၕ)
 ("&" ?ရ)

 ("`" ?ၐ)
 ("~" ?ဎ)

 ("Q" ?ဈ)
 ("W" ?ဝ)
 ("E" ?ဣ)
 ("R" ?၎)
 ("T" ?ဤ)
 ("Y" ?၌)
 ("U" ?ဥ)
 ("I" ?၍)
 ("O" ?ဿ)
 ("P" ?ဏ)
 ("{" ?ဧ)
 ("}" ?ဪ)

 ("A" ?ဗ)
 ("S" ?ှ)
 ("D" ?ီ)
 ("F" ?္)
 ("G" ?ွ)
 ("H" ?ံ)
 ("J" ?ဲ)
 ("K" ?ဒ)
 ("L" ?ဓ)

 ("Z" ?ဇ)
 ("X" ?ဌ)
 ("C" ?ဃ)
 ("V" ?ဠ)
 ("B" ?ယ)
 ("N" ?ဉ)
 ("M" ["ဦ"])
 ("<" ?၊)
 (">" ?။)
 ("?" ??)

 ("q" ?ဆ)
 ("w" ?တ)
 ("e" ?န)
 ("r" ?မ)
 ("t" ?အ)
 ("y" ?ပ)
 ("u" ?က)
 ("i" ?င)
 ("o" ?သ)
 ("p" ?စ)
 ("[" ?ဟ)
 ("]" ?ဩ)

 ("a" ?ေ)
 ("s" ?ျ)
 ("d" ?ိ)
 ("f" ?်)
 ("g" ?ါ)
 ("h" ?့)
 ("j" ?ြ)
 ("k" ?ု)
 ("l" ?ူ)
 (";" ?း)
 (":" ?ဂ)

 ("z" ?ဖ)
 ("x" ?ထ)
 ("c" ?ခ)
 ("v" ?လ)
 ("b" ?ဘ)
 ("n" ?ည)
 ("m" ?ာ)
 )

(quail-define-package
 "burmese-visual" "Burmese" "MYV" nil "Burmese input method (in visual order).

A more common way to input Burmese, which allows inputting the vowel ေ in visual order.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?၁)
 ("2" ?၂)
 ("3" ?၃)
 ("4" ?၄)
 ("5" ?၅)
 ("6" ?၆)
 ("7" ?၇)
 ("8" ?၈)
 ("9" ?၉)
 ("0" ?၀)

 ("!" ?ဍ)
 ("@" ?ၒ)
 ("#" ?ဋ)
 ("$" ?ၓ)
 ("%" ?ၔ)
 ("^" ?ၕ)
 ("&" ?ရ)

 ("`" ?ၐ)
 ("~" ?ဎ)

 ("Q" ?ဈ)
 ("W" ?ဝ)
 ("E" ?ဣ)
 ("R" ?၎)
 ("T" ?ဤ)
 ("Y" ?၌)
 ("U" ?ဥ)
 ("I" ?၍)
 ("O" ?ဿ)
 ("P" ?ဏ)
 ("{" ?ဧ)
 ("}" ?ဪ)

 ("A" ?ဗ)
 ("S" ?ှ)
 ("D" ?ီ)
 ("F" ?္)
 ("G" ?ွ)
 ("H" ?ံ)
 ("J" ?ဲ)
 ("K" ?ဒ)
 ("L" ?ဓ)

 ("Z" ?ဇ)
 ("X" ?ဌ)
 ("C" ?ဃ)
 ("V" ?ဠ)
 ("B" ?ယ)
 ("N" ?ဉ)
 ("M" ["ဦ"])
 ("<" ?၊)
 (">" ?။)

 ("q" ?ဆ)
 ("w" ?တ)
 ("e" ?န)
 ("r" ?မ)
 ("t" ?အ)
 ("y" ?ပ)
 ("u" ?က)
 ("i" ?င)
 ("o" ?သ)
 ("p" ?စ)
 ("[" ?ဟ)
 ("]" ["ဩ"])

 ("a" ?ေ)
 ("s" ?ျ)
 ("d" ?ိ)
 ("f" ?်)
 ("g" ?ါ)
 ("h" ?့)
 ("j" ?ြ)
 ("k" ?ု)
 ("l" ?ူ)
 (";" ?း)
 (":" ?ဂ)

 ("z" ?ဖ)
 ("x" ?ထ)
 ("c" ?ခ)
 ("v" ?လ)
 ("b" ?ဘ)
 ("n" ?ည)
 ("m" ?ာ)

 ("au" ["ကေ"]) ("ac" ["ခေ"]) ("a:" ["ဂေ"]) ("aC" ["ဃေ"]) ("ai" ["ငေ"])
 ("ap" ["စေ"]) ("aq" ["ဆေ"]) ("aZ" ["ဇေ"]) ("aQ" ["ဈေ"]) ("an" ["ညေ"])
 ("a#" ["ဋေ"]) ("aX" ["ဌေ"]) ("a!" ["ဍေ"]) ("a~" ["ဎေ"]) ("aP" ["ဏေ"])
 ("aw" ["တေ"]) ("ax" ["ထေ"]) ("aK" ["ဒေ"]) ("aL" ["ဓေ"]) ("ae" ["နေ"])
 ("ay" ["ပေ"]) ("az" ["ဖေ"]) ("aA" ["ဗေ"]) ("ab" ["ဘေ"]) ("ar" ["မေ"])
 ("aB" ["ယေ"]) ("a&" ["ရေ"]) ("av" ["လေ"]) ("aW" ["ဝေ"]) ("ao" ["သေ"])
 ("a[" ["ဟေ"]) ("aV" ["ဠေ"]) ("at" ["အေ"])
 )

(quail-define-package
 "shan" "Burmese" "SHN" nil "Shan input method." nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("Q" ?ꩡ)
 ("W" ?တ)
 ("E" ?ꧣ)
 ("R" ?႞)
 ("T" ?ြ)
 ("Y" ?ၿ)
 ("U" ?ၷ)
 ("I" ?ရ)
 ("O" ?သ)
 ("P" ?ႀ)

 ("A" ?ဵ)
 ("S" ?ႅ)
 ("D" ?ီ)
 ("F" ?ႂ)
 ("G" ?ႂ)
 ("H" ?့)
 ("J" ?ႆ)
 ("K" ?ဒ)
 ("L" ?ႊ)

 ("Z" ?ၾ)
 ("X" ?ꩪ)
 ("C" ?ꧠ)
 ("V" ?ꩮ)
 ("B" ?ျ)
 ("N" ?႟)
 ("M" ?ႃ)
 ("<" ?၊)
 (">" ?။)

 ("q" ?ၸ)
 ("w" ?တ)
 ("e" ?ၼ)
 ("r" ?မ)
 ("t" ?ဢ)
 ("y" ?ပ)
 ("u" ?ၵ)
 ("i" ?င)
 ("o" ?ဝ)
 ("p" ?ႁ)

 ("a" ?ေ)
 ("s" ?ႄ)
 ("d" ?ိ)
 ("f" ?်)
 ("g" ?ွ)
 ("h" ?ႉ)
 ("j" ?ႇ)
 ("k" ?ု)
 ("l" ?ူ)
 (";" ?ႈ)
 (":" ?း)

 ("z" ?ၽ)
 ("x" ?ထ)
 ("c" ?ၶ)
 ("v" ?လ)
 ("b" ?ယ)
 ("n" ?ၺ)
 ("m" ?ၢ)
 )


(quail-define-package
 "mon" "Burmese" "MON" nil "Mon input method." nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?၁)
 ("2" ?၂)
 ("3" ?၃)
 ("4" ?၄)
 ("5" ?၅)
 ("6" ?၆)
 ("7" ?၇)
 ("8" ?၈)
 ("9" ?၉)
 ("0" ?၀)

 ("!" ?ဍ)
 ("@" ?ၒ)
 ("#" ?ဋ)
 ("$" ?ၓ)
 ("^" ?ဵ)
 ("&" ?ရ)

 ("`" ?ၝ)
 ("~" ?ဎ)

 ("Q" ?ၛ)
 ("W" ?ဝ)
 ("E" ?ဣ)
 ("R" ?ၟ)
 ("T" ?ဳ)
 ("Y" ?ၠ)
 ("U" ?ဥ)
 ("I" ?၎)
 ("O" ?ဿ)
 ("P" ?ဏ)
 ("{" ?ဨ)
 ("}" ?/)

 ("A" ?ဗ)
 ("S" ?ှ)
 ("D" ?ီ)
 ("F" ?္)
 ("G" ?ွ)
 ("H" ?ံ)
 ("J" ?ဲ)
 ("K" ?ဒ)
 ("L" ?ဓ)

 ("Z" ?ဇ)
 ("X" ?ဌ)
 ("C" ?ဃ)
 ("V" ?ဠ)
 ("B" ?ၐ)
 ("N" ?ဉ)
 ("M" ?ၔ)
 ("<" ?ၞ)
 (">" ?ၕ)
 ("?" ?၊)
 ("/" ?။)

 ("q" ?ဆ)
 ("w" ?တ)
 ("e" ?န)
 ("r" ?မ)
 ("t" ?အ)
 ("y" ?ပ)
 ("u" ?က)
 ("i" ?ၚ)
 ("o" ?သ)
 ("p" ?စ)
 ("[" ?ဟ)
 ("]" ?ဩ)

 ("a" ?ေ)
 ("s" ?ျ)
 ("d" ?ိ)
 ("f" ?်)
 ("g" ?ါ)
 ("h" ?ဴ)
 ("j" ?ြ)
 ("k" ?ု)
 ("l" ?ူ)
 (";" ?း)

 ("z" ?ဖ)
 ("x" ?ထ)
 ("c" ?ခ)
 ("v" ?လ)
 ("b" ?ဘ)
 ("n" ?ည)
 ("m" ?ာ)
 )
;;; burmese.el ends here
