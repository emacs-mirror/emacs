;;; tamil-dvorak.el --- Quail package for Tamil input with Dvorak keyboard

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

;; Author: Shakthi Kannan <author@shakthimaan.com>

;; Keywords: multilingual, input method, Indian, Tamil, Dvorak

;; This file is released under the terms of GNU Emacs.

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

;;; After loading this file in GNU Emacs, you can select this input
;;; layout using "C-x Return C-\" followed by "tamil-dvorak" (without
;;; the quotes). Available keys: Z

;;; TODO: Add Tamil numbers, calendar from Unicode

;;; Code:

(require 'quail)

(quail-define-package
 "tamil-dvorak" "Tamil" "யளனக" nil
 "யளனக Tamil keyboard layout for use with Unicode (UTF-8 encoding)
  and Dvorak keyboard layout."
 nil t t t t nil nil nil nil nil t)


;; US Dvorak
;; 1! 2@ 3# 4$ 5% 6^ 7& 8* 9( 0) [{ ]}
;;  '" ,< .> pP yY fF gG cC rR lL /? =+ \|
;;   aA oO eE uU iI dD hH tT nN sS -_
;;     ;: qQ jJ kK xX bB mM wW vV zZ

;; தமிழ்
;; 1! 2@ 3# 4௹ 5% 6^ 7& 8* 9( 0) -_ {}
;;  ஞஶ றஷ நஸ சஹ வஜ லல ரர ைஐ   ொ ோ ிீ ுூ =+ \|
;;    ய' ள, ன. க" ப? ாழ த[ ம] ட< ்ஃ ங>
;;      ண$ ஒஓ உஊ எஏ ெே ஔ ௌ அஆ இஈ ;: zZ

(quail-define-rules
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
 ("[" ?-)
 ("]" ?{)
 ("`" ?`)
 ("'" ?ஞ)
 ("," ?ற)
 ("." ?ந)
 ("p" ?ச)
 ("y" ?வ)
 ("f" ?ல)
 ("g" ?ர)
 ("c" ?ை)
 ("r" ?ொ)
 ("l" ?ி)
 ("/" ?ு)
 ("=" ?=)
 ("a" ?ய)
 ("o" ?ள)
 ("e" ?ன)
 ("u" ?க)
 ("i" ?ப)
 ("d" ?ா)
 ("h" ?த)
 ("t" ?ம)
 ("n" ?ட)
 ("s" ?்)
 ("-" ?ங)
 ("\\" ?\\)
 (";" ?ண)
 ("q" ?ஒ)
 ("j" ?உ)
 ("k" ?எ)
 ("x" ?ெ)
 ("b" ?ஔ)
 ("m" ?அ)
 ("w" ?இ)
 ("v" ?\;)
 ("z" ?/)
 ("!" ?!)
 ("@" ?@)
 ("#" ?#)
 ("$" ?௹)
 ("%" ?%)
 ("^" ?^)
 ("&" ?&)
 ("*" ?*)
 ("(" ?\()
 (")" ?\))
 ("{" ?_)
 ("}" ?})
 ("~" ?~)
 ("\"" ?ஶ)
 ("<" ?ஷ)
 (">" ?ஸ)
 ("P" ?ஹ)
 ("Y" ?ஜ)
 ("F" ?ல)
 ("G" ?ர)
 ("C" ?ஐ)
 ("R" ?ோ)
 ("L" ?ீ)
 ("?" ?ூ)
 ("+" ?+)
 ("A" ?')
 ("O" ?,)
 ("E" ?.)
 ("U" ?\")
 ("I" ??)
 ("D" ?ழ)
 ("H" ?\[)
 ("T" ?\])
 ("N" ?<)
 ("S" ?ஃ)
 ("_" ?>)
 ("|" ?|)
 (":" ?$)
 ("Q" ?ஓ)
 ("J" ?ஊ)
 ("K" ?ஏ)
 ("X" ?ே)
 ("B" ?ௌ)
 ("M" ?ஆ)
 ("W" ?ஈ)
 ("V" ?:)
 ("Z" ?Z))

;;; tamil-dvorak.el ends here
