;;; persian.el  --- Quail package for inputting Persian/Farsi keyboard	-*- coding: utf-8;-*-

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

;; Author: Mohsen BANAN  <libre@mohsen.banan.1.byname.net>
;; http://mohsen.banan.1.byname.net/contact

;; Keywords: multilingual, input method, Farsi, Persian, keyboard

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; This is a Halaal Poly-Existential intended to remain perpetually Halaal.

;;; Commentary:
;;
;; This file contains a collection of input methods for
;; Persian languages - Farsi, Urdu, Pashto (Afghani), ...
;;
;; At this time, the following input methods are specified:
;;
;;  - (farsi) Persian Keyboard based on Islamic Republic of Iran's ISIR-9147
;;  - (farsi-translit) Intuitive transliteration keyboard layout for Persian
;;

;;; Code:

(require 'quail)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; farsi
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The keyboard mapping defined here is based on:
;;
;;   Institute of Standards and Industrial Research of Iran
;;   Information Technology – Layout of Persian Letters and Symbols on Computer Keyboards
;;   ISIRI 9147 -- 1st edition
;;   http://www.isiri.org/UserStd/DownloadStd.aspx?id=9147
;;
;;  Only layers 1 and 2 of ISIRI-9147 are applicable to emacs.
;;
;;  This input method was built using the Farsi table in X Keyboard Configuration Data Base.
;;
;;  0) Selected gnome keyboard "USA"
;;  1) Created a list of all keys
;;  2) Selected gnome keyboard "Iran"
;;  3) For each key just press it and get the mapped persian character
;;


(quail-define-package
 "farsi" "Farsi" " ف" nil "Farsi input method.

Based on ISIRI-9149 Layout of Persian Letters and Symbols on Computer Keyboards.
" nil t t t t nil nil nil nil nil t)

;; +----------------------------------------------------------------+
;; | ۱! | ۲٬ | ۳٫ | ۴﷼ | ۵٪ | ۶× | ۷، | ۸* | ۹) | ۰( | -ـ | =+ | `÷ |
;; +----------------------------------------------------------------+
;;   | ضْ| صٌ| ثٍ| قً| فُ| غِ| عَ| هّ| خ] | ح[ | ج} | چ{ |
;;   +------------------------------------------------------------+
;;    | شؤ | سئ | یي | بإ | لأ | اآ | تة | ن» | م« | ک: | گ؛ | \| |
;;    +-----------------------------------------------------------+
;;      | ظك | طٓ| زژ | رٰ| ذB | دٔ| پء | و> | .< | /؟ |
;;      +-------------------------------------------+

(quail-define-rules
 ("1" ?۱)
 ("2" ?۲)
 ("3" ?۳)
 ("4" ?۴)
 ("5" ?۵)
 ("6" ?۶)
 ("7" ?۷)
 ("8" ?۸)
 ("9" ?۹)
 ("0" ?۰)
 ("-" ?-)
 ("=" ?=)
 ;;("`" ?‍\))  ;; اتصال مجازى
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
 ("[" ?ج)
 ("]" ?چ)
 ("a" ?ش)
 ("s" ?س)
 ("d" ?ی)
 ("f" ?ب)
 ("g" ?ل)
 ("h" ?ا)
 ("j" ?ت)
 ("k" ?ن)
 ("l" ?م)
 (";" ?ک)
 ("'" ?گ)
 ("\\" ?\\)  ;; خط اريب وارو
 ("z" ?ظ)
 ("x" ?ط)
 ("c" ?ز)
 ("v" ?ر)
 ("b" ?ذ)
 ("n" ?د)
 ("m" ?پ)
 ("," ?و)
 ("." ?.)
 ("/" ?/)

 ("!" ?!)
 ("@" ?٬)
 ("#" ?٫)
 ("$" ?﷼)
 ("%" ?٪)
 ("^" ?×)
 ("&" ?،)
 ("*" ?*)
 ("(" ?\))
 (")" ?\()
 ("_" ?ـ)
 ("+" ?+)
 ("~" ?÷)
 ("Q" ?ْ)  ;; ساکن فارسى
 ("W" ?ٌ)  ;; دو پيش فارسى -- تنوين رفع
 ("E" ?ٍ)  ;; دو زير فارسى -- تنوين جر
 ("R" ?ً)  ;; دو زبر فارسى -- تنوين نصب
 ("T" ?ُ)  ;; پيش فارسى -- ضمه
 ("Y" ?ِ)  ;; زير فارسى -- کسره
 ("U" ?َ)  ;; زبر فارسى -- فتحه
 ("I" ?ّ)  ;; تشديد فارسى
 ("O" ?\])
 ("P" ?\[)
 ("{" ?})
 ("}" ?{)
 ("A" ?ؤ)
 ("S" ?ئ)
 ("D" ?ي)
 ("F" ?إ)
 ("G" ?أ)
 ("H" ?آ)
 ("J" ?ة)
 ("K" ?»)
 ("L" ?«)
 (":" ?:)
 ("\"" ?؛)
 ("|" ?|)
 ("Z" ?ك)
 ("X" ?ٓ)
 ("C" ?ژ)
 ("V" ?ٰ)
 ;; ("B" ?‌‌) ;; فاصلهً مجازى
 ("N" ?ٔ)  ;; همزه فارسى بالا
 ("M" ?ء)   ;;  harf farsi hamzeh
 ("<" ?>)
 (">" ?<)
 ("?" ?؟)
  )

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; farsi-translit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  This is a persian/farsi transliteration keyboard designed
;;;  for people who:
;;;   - Know how to write in farsi
;;;   - Are comfortable with the qwerty keyboard
;;;   - Are familiar with two letter phonetic mapping to persian characters
;;;     (e.g.: gh, kh, ch, sh, zh, hh, lh)
;;;
;;;  This translit keyboard is designed to be intuitive such that
;;;  mapping are easy and natural to remember for a persian writer.
;;;  For some persian characters there are multiple ways of inputting
;;;  the same character.
;;;
;;;  The letter 'h' is used for a number of two character mappings,
;;;  this means that some character sequence inputs need to be repeated
;;;  followed by a backspace followed by the 'h'.
;;;    For example:  سحر = 's' 's' '<bs>' 'h' 'h' 'r'
;;;  In practice such sequences are rare.
;;;



(quail-define-package
 "farsi-translit" "Farsi" "پ" t
 "Intuitive transliteration keyboard layout for persian/farsi.
" nil t t t t nil nil nil nil nil t)


(quail-define-rules
 ("a"  ?ا)
 ("A"  ?آ)  ;; alef madde
 ("b"  ?ب)
 ("p"  ?پ)
 ("t"  ?ت)
 ("c"  ?ث)
 ("j"  ?ج)
 ("ch" ?چ)
 ("hh" ?ح)
 ("kh" ?خ)
 ("d"  ?د)
 ("Z"  ?ذ)
 ("r"  ?ر)
 ("z"  ?ز)
 ("zh" ?ژ)
 ("s"  ?س)
 ("sh" ?ش)
 ("S"  ?ص)
 ("x"  ?ض)
 ("T"  ?ط)
 ("X"  ?ظ)
 ("w"  ?ع)
 ("Q"  ?غ)
 ("f"  ?ف)
 ("q"  ?ق)
 ("gh" ?ق)
 ("k"  ?ک)
 ("K"  ?ك)  ;;  Arabic kaf
 ("g"  ?گ)
 ("l"  ?ل)
 ("lh" ?ﻻ)
 ("m"  ?م)
 ("n"  ?ن)
 ("v"  ?و)
 ("V" ?ؤ)
 ("u"  ?و)
 ("H"  ?ه)
 ("h"  ?ه)
 ("th" ?ة)  ;; ta marbuteh
 ("yh" ?ۀ)  ;; he ye
 ("y"  ?ى)
 ("i"  ?ي)
 ("I" ?ئ)

 ("1"  ?۱)
 ("2"  ?۲)
 ("3"  ?۳)
 ("4"  ?۴)
 ("5"  ?۵)
 ("6"  ?۶)
 ("7"  ?۷)
 ("8"  ?۸)
 ("9"  ?۹)
 ("0"  ?۰)

 ("F" ?إ)
 ("G" ?أ)

 ("~"  ?ّ)  ;; tashdid ;;  تشديد فارسى
 ("`" ?ٓ)
 ("e"  ?ِ)  ;; zir   زير فارسى -- فتحه
 ("E"  ?ٍ)  ;; eizan ;; دو زير فارسى -- تنوين جر
 ("#"  ?ً)  ;; ً tanvin nasb ;; دو زبر فارسى -- تنوين نصب
 ("@" ?ْ)  ;; ساکن فارسى
 ("^"  ?َ)  ;; zbar ;; زبر فارسى -- فتحه
 ("o"  ?ُ)  ;; peesh ;; پيش فارسى -- ضمه
 ("O" ?ٌ)  ;; دو پيش فارسى -- تنوين رفع
 ("?"  ?؟)  ;; alamat soal
 ("&" ?ٔ)  ;; همزه فارسى بالا
 ("$"  ?ء)  ;; hamzeh
 ("%"  ?÷)  ;;
 ("*"  ?×)  ;;
 (";"  ?؛)  ;;
 (",h" ?،)  ;; farsi
 (",h" ?,)  ;; latin
 ("."  ?.)  ;;
 ("_"  ?ـ)  ;;
)


;;; persian.el ends here
