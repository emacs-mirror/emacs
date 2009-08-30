;;; ipa.el --- Quail package for inputting IPA characters  -*-coding: utf-8;-*-

;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, IPA

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

;;; Commentary:

;;; Code:

(require 'quail)

(quail-define-package
 "ipa" "IPA" "IPA" t
 "International Phonetic Alphabet for English, French, German and Italian

Upside-down characters are obtained by a preceding slash (/)."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("i" ?i)
 ("I" ?ɪ)
 ("e" ?e)
 ("/3" ?ɛ)
 ("E" ?ɛ)
 ("ae" ?æ)
 ("a" ?a)
 ("i-" ?ɨ)
 ("/e" ?ə)
 ("/a" ?ɐ)
 ("/m" ?ɯ)
 ("&" ?ɤ)
 ("/v" ?ʌ)
 ("A" ?ɑ)
 ("o|" ?ɑ)
 ("y" ?y)
 ("Y" ?ʏ)
 ("o/" ?ø)
 ("oe" ?œ)
 ("OE" ?ɶ)
 ("u-" ?ʉ)
 ("o-" ?ɵ)
 ("u" ?u)
 ("U" ?ʊ)
 ("o" ?o)
 ("/c" ?ɔ)
 ("/A" ?ɒ)
 ("|o" ?ɒ)
 ("e-" ?ɚ)
 ("e|" ?ɚ)
 ("/3~" ["ɛ̃"])
 ("E~" ["ɛ̃"])
 ("A~" ["ɑ̃"])
 ("oe~" ["œ̃"])
 ("/c~" ["ɔ̃"])
 ("p" ?p)
 ("b" ?b)
 ("t" ?t)
 ("d" ?d)
 ("k" ?k)
 ("g" ?g)
 ("f" ?f)
 ("v" ?v)
 ("th" ?θ)
 ("dh" ?ð)
 ("s" ?s)
 ("z" ?z)
 ("sh" ?ʃ)
 ("zh" ?ʒ)
 ("3" ?ʒ)
 ("c," ?ç)
 ("x" ?x)
 ("/R" ?ʁ)
 ("h" ?h)
 ("m" ?m)
 ("n" ?n)
 ("gn" ?ɲ)
 ("ng" ?ŋ)
 ("r" ?r)
 ("R" ?ʀ)
 ("/r" ?ɹ)
 ("j" ?j)
 ("l" ?l)
 ("/y" ?ʎ)
 ("L" ?ʟ)
 ("/h" ?ɥ)
 ("w" ?w)
 ("M" ?ʍ)
 ("'" ?ˈ)
 ("`" ?ˌ)
 (":" ?ː))

;; arch-tag: cf2614cc-ecce-4ef5-ba51-37faeed41691
;;; ipa.el ends here
