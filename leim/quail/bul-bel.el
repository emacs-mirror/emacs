;;; bul-bel.el --- Bulgarian/Belarusian Quail input methods  -*-coding: utf-8;-*-

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: i18n

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To support the Bulgarian and Belarusian language environments.
;; Improvements welcome from natives.

;;; Code:

;; From Yudit's `Belarusian input table according to STB955-94
;; belarusian standard'.  Alexander Mikhailian <mikhailian@altern.org>
(quail-define-package
 "belarusian" "Cyrillic" "і" ; fixme
 nil
 "Belarusian STB955-94 input, producing Unicode"
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("<<" ?«)
 (">>" ?»)
 (",," ?”)
 ("``" ?“)
 ("C)" ?©)
 ("x)" ?•)
 (":)" ?☺)
 (":(" ?☹)
 ("C-" ?¤)
 ("E-" ?€)
 ("L-" ?£)

 ("~" ?Ё)
 ("!" ?!)
 ("@" ?\")
 ("#" ?№)
 ("$" ?\;)
 ("%" ?%)
 ("^" ?:)
 ("&" ??)
 ("*" ?*)
 ("(" ?\()
 (")" ?\))
 ("_" ?_)
 ("+" ?+)
 ("Q" ?Й)
 ("W" ?Ц)
 ("E" ?У)
 ("R" ?К)
 ("T" ?Е)
 ("Y" ?Н)
 ("U" ?Г)
 ("I" ?Ш)
 ("O" ?Ў)
 ("P" ?З)
 ("{" ?Х)
 ("}" ?')
 ("A" ?Ф)
 ("S" ?Ы)
 ("D" ?В)
 ("F" ?А)
 ("G" ?П)
 ("H" ?Р)
 ("J" ?О)
 ("K" ?Л)
 ("L" ?Д)
 (":" ?Ж)
 ("\"" ?Э)
 ("|" ?/)
 ("Z" ?Я)
 ("X" ?Ч)
 ("C" ?С)
 ("V" ?М)
 ("B" ?І)
 ("N" ?Т)
 ("M" ?Ь)
 ("<" ?Б)
 (">" ?Ю)
 ("?" ?,)

 ("`" ?ё)
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?-)
 ("=" ?=)
 ("q" ?й)
 ("w" ?ц)
 ("e" ?у)
 ("r" ?к)
 ("t" ?е)
 ("y" ?н)
 ("u" ?г)
 ("i" ?ш)
 ("o" ?ў)
 ("p" ?з)
 ("[" ?х)
 ("]" ?')
 ("a" ?ф)
 ("s" ?ы)
 ("d" ?в)
 ("f" ?а)
 ("g" ?п)
 ("h" ?р)
 ("j" ?о)
 ("k" ?л)
 ("l" ?д)
 (";" ?ж)
 ("'" ?э)
 ("\\" ?\\)
 ("z" ?я)
 ("x" ?ч)
 ("c" ?с)
 ("v" ?м)
 ("b" ?і)
 ("n" ?т)
 ("m" ?ь)
 ("," ?б)
 ("." ?ю)
 ("/" ?.)

 ("D-" ?Ђ)
 ("G'" ?Ѓ)
 ("E>" ?Є)
 ("Z>" ?Ѕ)
 ("I/ " ?И)
 ("I:" ?Ї)
 ("J<" ?Ј)
 ("L>" ?Љ)
 ("N>" ?Њ)
 ("C'" ?Ћ)
 ("K'" ?Ќ)
 ("D>" ?Џ)

 ("d-" ?ђ)
 ("g'" ?ѓ)
 ("e>" ?є)
 ("z>" ?ѕ)
 ("i/ " ?и)
 ("i:" ?ї)
 ("j<" ?ј)
 ("l>" ?љ)
 ("c'" ?ћ)
 ("k'" ?ќ)
 ("d>" ?џ)

 ("G`" ?Ґ)
 ("g`" ?ґ)
 ("E<" ?Ѣ)
 ("e<" ?ѣ)
 ("A<" ?Ѫ)
 ("a<" ?ѫ)
 ("F`" ?Ѳ)
 ("f`" ?ѳ)
 ("Y`" ?Ѵ)
 ("y`" ?ѵ))

;; From `Bulgarian-PHO.kmap for Yudit', Alexander Shopov <al_shopov@web.bg>
(quail-define-package
 "bulgarian-pho" "Cyrillic" "Б" ; fixme
 nil
 "Bulragian PHO keyboard layout, producing Unicode"
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("A" ?А)
 ("B" ?Б)
 ("W" ?В)
 ("G" ?Г)
 ("D" ?Д)
 ("E" ?Е)
 ("V" ?Ж)
 ("Z" ?З)
 ("I" ?И)
 ("J" ?Й)
 ("K" ?К)
 ("L" ?Л)
 ("M" ?М)
 ("N" ?Н)
 ("O" ?О)
 ("P" ?П)
 ("R" ?Р)
 ("S" ?С)
 ("T" ?Т)
 ("U" ?У)
 ("F" ?Ф)
 ("H" ?Х)
 ("C" ?Ц)
 ("~" ?Ч)
 ("{" ?Ш)
 ("}" ?Щ)
 ("Y" ?Ъ)
 ("X" ?Ь)
 ("|" ?Ю)
 ("Q" ?Я)
 ("a" ?а)
 ("b" ?б)
 ("w" ?в)
 ("g" ?г)
 ("d" ?д)
 ("e" ?е)
 ("v" ?ж)
 ("z" ?з)
 ("i" ?и)
 ("j" ?й)
 ("k" ?к)
 ("l" ?л)
 ("m" ?м)
 ("n" ?н)
 ("o" ?о)
 ("p" ?п)
 ("r" ?р)
 ("s" ?с)
 ("t" ?т)
 ("u" ?у)
 ("f" ?ф)
 ("h" ?х)
 ("c" ?ц)
 ("`" ?ч)
 ("[" ?ш)
 ("]" ?щ)
 ("y" ?ъ)
 ("x" ?ь)
 ("\\" ?ю)
 ("q" ?я))

(provide 'bul-bel)
;;; bul-bel.el ends here
