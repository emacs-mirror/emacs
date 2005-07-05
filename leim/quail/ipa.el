;;; ipa.el --- Quail package for inputting IPA characters  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, input method, IPA

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
 "ipa" "IPA" "IPA" t
 "International Phonetic Alphabet for English, French, German and Italian

Upside-down characters are obtained by a preceding slash (/)."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("i" ?,0 (B)
 ("I" ?,0!(B)
 ("e" ?,0"(B)
 ("/3" ?,0#(B)
 ("E" ?,0#(B)
 ("ae" ?,0$(B)
 ("a" ?,0%(B)
 ("i-" ?,0&(B)
 ("/e" ?,0'(B)
 ("/a" ?,0((B)
 ("/m" ?,0)(B)
 ("&" ?,0*(B)
 ("/v" ?,0+(B)
 ("A" ?,0,(B)
 ("o|" ?,0,(B)
 ("y" ?,0-(B)
 ("Y" ?,0.(B)
 ("o/" ?,0/(B)
 ("oe" ?,00(B)
 ("OE" ?,01(B)
 ("u-" ?,02(B)
 ("o-" ?,03(B)
 ("u" ?,04(B)
 ("U" ?,05(B)
 ("o" ?,06(B)
 ("/c" ?,07(B)
 ("/A" ?,08(B)
 ("|o" ?,08(B)
 ("e-" ?,0:(B)
 ("e|" ?,0:(B)
 ("/3~" ?,0;(B)
 ("E~" ?,0;(B)
 ("A~" ?,0<(B)
 ("oe~" ?,0=(B)
 ("/c~" ?,0>(B)
 ("p" ?,0@(B)
 ("b" ?,0A(B)
 ("t" ?,0B(B)
 ("d" ?,0C(B)
 ("k" ?,0D(B)
 ("g" ?,0E(B)
 ("f" ?,0F(B)
 ("v" ?,0G(B)
 ("th" ?,0H(B)
 ("dh" ?,0I(B)
 ("s" ?,0J(B)
 ("z" ?,0K(B)
 ("sh" ?,0L(B)
 ("zh" ?,0M(B)
 ("3" ?,0M(B)
 ("c," ?,0N(B)
 ("x" ?,0O(B)
 ("/R" ?,0P(B)
 ("h" ?,0Q(B)
 ("m" ?,0R(B)
 ("n" ?,0S(B)
 ("gn" ?,0T(B)
 ("ng" ?,0U(B)
 ("r" ?,0V(B)
 ("R" ?,0W(B)
 ("/r" ?,0X(B)
 ("j" ?,0Y(B)
 ("l" ?,0Z(B)
 ("/y" ?,0[(B)
 ("L" ?,0\(B)
 ("/h" ?,0](B)
 ("w" ?,0^(B)
 ("M" ?,0_(B)
 ("'" ?,0p(B)
 ("`" ?,0q(B)
 (":" ?,0r(B))

;;; arch-tag: cf2614cc-ecce-4ef5-ba51-37faeed41691
;;; ipa.el ends here
