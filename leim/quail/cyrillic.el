;;; cyrillic.el --- Quail package for inputting Cyrillic characters

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002  Free Software Foundation, Inc.

;; Author: TAKAHASHI Naoto <ntakahas@m17n.org>
;; Keywords: multilingual, input method, Cyrillic, i18n

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'quail)

;; This was `cyrillic-jcuken'.  Alexander Mikhailian
;; <mikhailian@altern.org> says:  "cyrillic-jcuken" is actually
;; russian.  It is ok but a bit outdated.  This layout has been used
;; in typewriters for ages but it has been superceeded on desktops by
;; a variation of this layout, implemented in M$ Windows software.
;; The Windows layout is greatly preferred because of the comma and
;; period being placed more conviniently and, of course, because of
;; the popularity of Windows software. This layout is a common option
;; in X Windows and console layouts for GNU/Linux.  [See
;; `russian-computer' below.]
(quail-define-package
 "cyrillic-typewriter" "Russian" ",L69(B" nil
 ",L9FC:5=(B Russian typewriter layout."
 nil t t t t nil nil nil nil nil t)

;;  1! 2" 3' 4* 5: 6, 7. 8; 9( 0) -_ =+ ,L!(B
;;   ,L9(B  ,LF(B  ,LC(B  ,L:(B  ,L5(B  ,L=(B  ,L3(B  ,LH(B  ,LI(B  ,L7(B  ,LE(B  ,Lj(B
;;    ,LD(B  ,LK(B  ,L2(B  ,L0(B  ,L?(B  ,L@(B  ,L>(B  ,L;(B  ,L4(B ,L6(B  ,LM(B
;;     ,LO(B  ,LG(B  ,LA(B  ,L<(B  ,L8(B  ,LB(B  ,LL(B  ,L1(B  ,LN(B  /?

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
 ("-" ?-)
 ("=" ?=)
 ("`" ?,Lq(B)
 ("q" ?,LY(B)
 ("w" ?,Lf(B)
 ("e" ?,Lc(B)
 ("r" ?,LZ(B)
 ("t" ?,LU(B)
 ("y" ?,L](B)
 ("u" ?,LS(B)
 ("i" ?,Lh(B)
 ("o" ?,Li(B)
 ("p" ?,LW(B)
 ("[" ?,Le(B)
 ("]" ?,Lj(B)
 ("a" ?,Ld(B)
 ("s" ?,Lk(B)
 ("d" ?,LR(B)
 ("f" ?,LP(B)
 ("g" ?,L_(B)
 ("h" ?,L`(B)
 ("j" ?,L^(B)
 ("k" ?,L[(B)
 ("l" ?,LT(B)
 (";" ?,LV(B)
 ("'" ?,Lm(B)
 ("\\" ?\\)
 ("z" ?,Lo(B)
 ("x" ?,Lg(B)
 ("c" ?,La(B)
 ("v" ?,L\(B)
 ("b" ?,LX(B)
 ("n" ?,Lb(B)
 ("m" ?,Ll(B)
 ("," ?,LQ(B)
 ("." ?,Ln(B)
 ("/" ?/)
 
 ("!" ?!)
 ("@" ?\")
 ("#" ?')
 ("$" ?*)
 ("%" ?:)
 ("^" ?,)
 ("&" ?.)
 ("*" ?\;)
 ("(" ?\()
 (")" ?\))
 ("_" ?_)
 ("+" ?+)
 ("~" ?,L!(B)
 ("Q" ?,L9(B)
 ("W" ?,LF(B)
 ("E" ?,LC(B)
 ("R" ?,L:(B)
 ("T" ?,L5(B)
 ("Y" ?,L=(B)
 ("U" ?,L3(B)
 ("I" ?,LH(B)
 ("O" ?,LI(B)
 ("P" ?,L7(B)
 ("{" ?,LE(B)
 ("}" ?,LJ(B)
 ("A" ?,LD(B)
 ("S" ?,LK(B)
 ("D" ?,L2(B)
 ("F" ?,L0(B)
 ("G" ?,L?(B)
 ("H" ?,L@(B)
 ("J" ?,L>(B)
 ("K" ?,L;(B)
 ("L" ?,L4(B)
 (":" ?,L6(B)
 ("\"" ?,LM(B)
 ("|" ?|)
 ("Z" ?,LO(B)
 ("X" ?,LG(B)
 ("C" ?,LA(B)
 ("V" ?,L<(B)
 ("B" ?,L8(B)
 ("N" ?,LB(B)
 ("M" ?,LL(B)
 ("<" ?,L1(B)
 (">" ?,LN(B)
 ("?" ?,))

;; Mikhailian couldn't check the next two.  

;; This seems to have the same layout for letters as mk in XKB, but at
;; least the top row is different.
(quail-define-package
 "cyrillic-macedonian" "Cyrillic" ",L6(BM" nil
 ",L)*5@B7(B-,L#,(B keyboard layout based on JUS.I.K1.004"
 nil t t t t nil nil nil nil nil t)

;;  1! 2" 3# 4$ 5% 6& 7' 8( 9) 0= /? +* <>
;;   ,L)(B  ,L*(B  ,L5(B  ,L@(B  ,LB(B  ,L7(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,L#(B
;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L((B  ,L:(B  ,L;(B  ,LG(B  ,L,(B  ,L6(B
;;     ,L%(B  ,L/(B  ,LF(B  ,L2(B  ,L1(B  ,L=(B  ,L<(B  ,; .: -_

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
 ("-" ?/)
 ("=" ?+)
 ("`" ?<)
 ("q" ?,Ly(B)
 ("w" ?,Lz(B)
 ("e" ?,LU(B)
 ("r" ?,L`(B)
 ("t" ?,Lb(B)
 ("y" ?,LW(B)
 ("u" ?,Lc(B)
 ("i" ?,LX(B)
 ("o" ?,L^(B)
 ("p" ?,L_(B)
 ("[" ?,Lh(B)
 ("]" ?,Ls(B)
 ("a" ?,LP(B)
 ("s" ?,La(B)
 ("d" ?,LT(B)
 ("f" ?,Ld(B)
 ("g" ?,LS(B)
 ("h" ?,Le(B)
 ("j" ?,Lx(B)
 ("k" ?,LZ(B)
 ("l" ?,L[(B)
 (";" ?,Lg(B)
 ("'" ?,L|(B)
 ("\\" ?,LV(B)
 ("z" ?,Lu(B)
 ("x" ?,L(B)
 ("c" ?,Lf(B)
 ("v" ?,LR(B)
 ("b" ?,LQ(B)
 ("n" ?,L](B)
 ("m" ?,L\(B)
 ("," ?,)
 ("." ?.)
 ("/" ?-)
 
 ("!" ?!)
 ("@" ?\")
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?&)
 ("&" ?')
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?*)
 ("~" ?>)
 ("Q" ?,L)(B)
 ("W" ?,L*(B)
 ("E" ?,L5(B)
 ("R" ?,L@(B)
 ("T" ?,LB(B)
 ("Y" ?,L7(B)
 ("U" ?,LC(B)
 ("I" ?,L8(B)
 ("O" ?,L>(B)
 ("P" ?,L?(B)
 ("{" ?,LH(B)
 ("}" ?,L#(B)
 ("A" ?,L0(B)
 ("S" ?,LA(B)
 ("D" ?,L4(B)
 ("F" ?,LD(B)
 ("G" ?,L3(B)
 ("H" ?,LE(B)
 ("J" ?,L((B)
 ("K" ?,L:(B)
 ("L" ?,L;(B)
 (":" ?,LG(B)
 ("\"" ?,L,(B)
 ("|" ?,L6(B)
 ("Z" ?,L%(B)
 ("X" ?,L/(B)
 ("C" ?,LF(B)
 ("V" ?,L2(B)
 ("B" ?,L1(B)
 ("N" ?,L=(B)
 ("M" ?,L<(B)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_))

;;

(quail-define-package
 "cyrillic-serbian" "Cyrillic" ",L6(BS" nil
 ",L)*5@B7(B-,L"+(B keyboard layout based on JUS.I.K1.005"
 nil t t t t nil nil nil nil nil t)

;;  1! 2" 3# 4$ 5% 6& 7' 8( 9) 0= /? +* <>
;;   ,L)(B  ,L*(B  ,L5(B  ,L@(B  ,LB(B  ,L7(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,L"(B
;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L((B  ,L:(B  ,L;(B  ,LG(B  ,L+(B  ,L6(B
;;     ,L%(B  ,L/(B  ,LF(B  ,L2(B  ,L1(B  ,L=(B  ,L<(B  ,; .: -_

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
 ("-" ?/)
 ("=" ?+)
 ("`" ?<)
 ("q" ?,Ly(B)
 ("w" ?,Lz(B)
 ("e" ?,LU(B)
 ("r" ?,L`(B)
 ("t" ?,Lb(B)
 ("y" ?,LW(B)
 ("u" ?,Lc(B)
 ("i" ?,LX(B)
 ("o" ?,L^(B)
 ("p" ?,L_(B)
 ("[" ?,Lh(B)
 ("]" ?,Lr(B)
 ("a" ?,LP(B)
 ("s" ?,La(B)
 ("d" ?,LT(B)
 ("f" ?,Ld(B)
 ("g" ?,LS(B)
 ("h" ?,Le(B)
 ("j" ?,Lx(B)
 ("k" ?,LZ(B)
 ("l" ?,L[(B)
 (";" ?,Lg(B)
 ("'" ?,L{(B)
 ("\\" ?,LV(B)
 ("z" ?,Lu(B)
 ("x" ?,L(B)
 ("c" ?,Lf(B)
 ("v" ?,LR(B)
 ("b" ?,LQ(B)
 ("n" ?,L](B)
 ("m" ?,L\(B)
 ("," ?,)
 ("." ?.)
 ("/" ?-)
 
 ("!" ?!)
 ("@" ?\")
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?&)
 ("&" ?')
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?*)
 ("~" ?>)
 ("Q" ?,L)(B)
 ("W" ?,L*(B)
 ("E" ?,L5(B)
 ("R" ?,L@(B)
 ("T" ?,LB(B)
 ("Y" ?,L7(B)
 ("U" ?,LC(B)
 ("I" ?,L8(B)
 ("O" ?,L>(B)
 ("P" ?,L?(B)
 ("{" ?,LH(B)
 ("}" ?,L"(B)
 ("A" ?,L0(B)
 ("S" ?,LA(B)
 ("D" ?,L4(B)
 ("F" ?,LD(B)
 ("G" ?,L3(B)
 ("H" ?,LE(B)
 ("J" ?,L((B)
 ("K" ?,L:(B)
 ("L" ?,L;(B)
 (":" ?,LG(B)
 ("\"" ?,L+(B)
 ("|" ?,L6(B)
 ("Z" ?,L%(B)
 ("X" ?,L/(B)
 ("C" ?,LF(B)
 ("V" ?,L2(B)
 ("B" ?,L1(B)
 ("N" ?,L=(B)
 ("M" ?,L<(B)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_))

;;

;; Alexander Mikhailian comments:
;; Having worked for several years as a Belarusian linguist, I still
;; can not find the origin of this layout which BTW does include
;; several characters that are not present in Belarusian and does not
;; include a few ones that do exist in Belarusian.  Besides, the typo
;; in the name of this layout speaks for itself since Belarusian has
;; an outdated version of spelling which is "Byelorussian" and not
;; "beylorussian".  I suggest that you just remove this layout.

;; [`derived from JUS.I.K1' according to an old Mule note -- fx]

;; (quail-define-package
;;  "cyrillic-beylorussian" "Belarussian" ",L6(BB" nil
;;  ",L)*5@B7(B-,L&.(B BEYLORUSSIAN (ISO 8859-5 encoding)"
;;  nil t t t t nil nil nil nil nil t)

;; ;;  1! 2" 3# 4$ 5% 6& 7' 8( 9) 0= /? +* <>
;; ;;   ,L)(B  ,L*(B  ,L5(B  ,L@(B  ,LB(B  ,L7(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,L&(B
;; ;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L((B  ,L:(B  ,L;(B  ,LG(B  ,L.(B  ,L6(B
;; ;;     ,L%(B  ,L/(B  ,LF(B  ,L2(B  ,L1(B  ,L=(B  ,L<(B  ,; .: -_

;; (quail-define-rules
;;  ("-" ?/)
;;  ("=" ?+)
;;  ("`" ?<)
;;  ("q" ?,Ly(B)
;;  ("w" ?,Lz(B)
;;  ("e" ?,LU(B)
;;  ("r" ?,L`(B)
;;  ("t" ?,Lb(B)
;;  ("y" ?,LW(B)
;;  ("u" ?,Lc(B)
;;  ("i" ?,LX(B)
;;  ("o" ?,L^(B)
;;  ("p" ?,L_(B)
;;  ("[" ?,Lh(B)
;;  ("]" ?,Lv(B)
;;  ("a" ?,LP(B)
;;  ("s" ?,La(B)
;;  ("d" ?,LT(B)
;;  ("f" ?,Ld(B)
;;  ("g" ?,LS(B)
;;  ("h" ?,Le(B)
;;  ("j" ?,Lx(B)
;;  ("k" ?,LZ(B)
;;  ("l" ?,L[(B)
;;  (";" ?,Lg(B)
;;  ("'" ?,L~(B)
;;  ("\\" ?,LV(B)
;;  ("z" ?,Lu(B)
;;  ("x" ?,L(B)
;;  ("c" ?,Lf(B)
;;  ("v" ?,LR(B)
;;  ("b" ?,LQ(B)
;;  ("n" ?,L](B)
;;  ("m" ?,L\(B)
;;  ("/" ?-)
 
;;  ("@" ?\")
;;  ("^" ?&)
;;  ("&" ?')
;;  ("*" ?\()
;;  ("(" ?\))
;;  (")" ?=)
;;  ("_" ??)
;;  ("+" ?*)
;;  ("~" ?>)
;;  ("Q" ?,L)(B)
;;  ("W" ?,L*(B)
;;  ("E" ?,L5(B)
;;  ("R" ?,L@(B)
;;  ("T" ?,LB(B)
;;  ("Y" ?,L7(B)
;;  ("U" ?,LC(B)
;;  ("I" ?,L8(B)
;;  ("O" ?,L>(B)
;;  ("P" ?,L?(B)
;;  ("{" ?,LH(B)
;;  ("}" ?,L&(B)
;;  ("A" ?,L0(B)
;;  ("S" ?,LA(B)
;;  ("D" ?,L4(B)
;;  ("F" ?,LD(B)
;;  ("G" ?,L3(B)
;;  ("H" ?,LE(B)
;;  ("J" ?,L((B)
;;  ("K" ?,L:(B)
;;  ("L" ?,L;(B)
;;  (":" ?,LG(B)
;;  ("\"" ?,L.(B)
;;  ("|" ?,L6(B)
;;  ("Z" ?,L%(B)
;;  ("X" ?,L/(B)
;;  ("C" ?,LF(B)
;;  ("V" ?,L2(B)
;;  ("B" ?,L1(B)
;;  ("N" ?,L=(B)
;;  ("M" ?,L<(B)
;;  ("<" ?\;)
;;  (">" ?:)
;;  ("?" ?_))

;;

;; Alexander Mikhailian reports the opinion of fellow Ukrainian
;; linguist Bogdan Babych <babych@altern.org>:
;; He had seen this layout on some oldish systems but that the vast
;; majority of the population uses a modified version of the M$ Windows
;; layout.  In fact, Microsoft shipped for a while a layout that was lacking
;; two characters, precisely the "GHE_WITH_UPTURN" and the apostrophe.  The
;; latest versions of Windows software do have the "GHE_WITH_UPTURN" in the
;; ukrainian keyborad layout but the apostrophe is still not there, whereas
;; there is one letter, "Cyrillic_YO", not used in ukrainian.  Ukrainians
;; normally replace the "Cyrillic_YO" by the apostrophe sign and live
;; happily with this little change.  [See "ukrainian-computer" below.]

;; Fixme: add GHE_WITH_UPTURN.
(quail-define-package 
 "cyrillic-ukrainian" "Ukrainian" ",L6(BU" nil
 ",L$'5@B7(B-,L&.(B UKRAINIAN"
 nil t t t t nil nil nil nil nil t)

;;  1! 2" 3# 4$ 5% 6& 7' 8( 9) 0= /? +* <>
;;   ,L$(B  ,L'(B  ,L5(B  ,L@(B  ,LB(B  ,L7(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,L&(B
;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L((B  ,L:(B  ,L;(B  ,LG(B  ,L.(B  ,L6(B
;;     ,L%(B  ,L/(B  ,LF(B  ,L2(B  ,L1(B  ,L=(B  ,L<(B  ,; .: -_

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
 ("-" ?/)
 ("=" ?+)
 ("`" ?<)
 ("q" ?,Lt(B)
 ("w" ?,Lw(B)
 ("e" ?,LU(B)
 ("r" ?,L`(B)
 ("t" ?,Lb(B)
 ("y" ?,LW(B)
 ("u" ?,Lc(B)
 ("i" ?,LX(B)
 ("o" ?,L^(B)
 ("p" ?,L_(B)
 ("[" ?,Lh(B)
 ("]" ?,Lv(B)
 ("a" ?,LP(B)
 ("s" ?,La(B)
 ("d" ?,LT(B)
 ("f" ?,Ld(B)
 ("g" ?,LS(B)
 ("h" ?,Le(B)
 ("j" ?,Lx(B)
 ("k" ?,LZ(B)
 ("l" ?,L[(B)
 (";" ?,Lg(B)
 ("'" ?,L~(B)
 ("\\" ?,LV(B)
 ("z" ?,Lu(B)
 ("x" ?,L(B)
 ("c" ?,Lf(B)
 ("v" ?,LR(B)
 ("b" ?,LQ(B)
 ("n" ?,L](B)
 ("m" ?,L\(B)
 ("," ?,)
 ("." ?.)
 ("/" ?-)
 
 ("!" ?!)
 ("@" ?\")
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?&)
 ("&" ?')
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?*)
 ("~" ?>)
 ("Q" ?,L$(B)
 ("W" ?,L'(B)
 ("E" ?,L5(B)
 ("R" ?,L@(B)
 ("T" ?,LB(B)
 ("Y" ?,L7(B)
 ("U" ?,LC(B)
 ("I" ?,L8(B)
 ("O" ?,L>(B)
 ("P" ?,L?(B)
 ("{" ?,LH(B)
 ("}" ?,L&(B)
 ("A" ?,L0(B)
 ("S" ?,LA(B)
 ("D" ?,L4(B)
 ("F" ?,LD(B)
 ("G" ?,L3(B)
 ("H" ?,LE(B)
 ("J" ?,L((B)
 ("K" ?,L:(B)
 ("L" ?,L;(B)
 (":" ?,LG(B)
 ("\"" ?,L.(B)
 ("|" ?,L6(B)
 ("Z" ?,L%(B)
 ("X" ?,L/(B)
 ("C" ?,LF(B)
 ("V" ?,L2(B)
 ("B" ?,L1(B)
 ("N" ?,L=(B)
 ("M" ?,L<(B)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_))


(quail-define-package
 "ukrainian-computer" "Ukrainian" "UK" nil
 ",L9FC:5=(B Ukrainian."
 nil t t t t nil nil nil nil nil t)

;;  ' 1! 2" 3,Lp(B 4; 5% 6: 7? 8* 9( 0) -_ =+
;;   ,L9(B  ,LF(B  ,LC(B  ,L:(B  ,L5(B  ,L=(B  ,L3(B  ,LH(B  ,LI(B  ,L7(B  ,LE(B  ,L'(B
;;    ,LD(B  ,L&(B  ,L2(B  ,L0(B  ,L?(B  ,L@(B  ,L>(B  ,L;(B  ,L4(B  ,L6(B  ,L$(B  $,1)P(B
;;      ,LO(B  ,LG(B  ,LA(B  ,L<(B  ,L8(B  ,LB(B  ,LL(B  ,L1(B  ,LN(B  .,

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
 ("-" ?-)
 ("=" ?=)
 ("`" ?')
 ("q" ?,LY(B)
 ("w" ?,Lf(B)
 ("e" ?,Lc(B)
 ("r" ?,LZ(B)
 ("t" ?,LU(B)
 ("y" ?,L](B)
 ("u" ?,LS(B)
 ("i" ?,Lh(B)
 ("o" ?,Li(B)
 ("p" ?,LW(B)
 ("[" ?,Le(B)
 ("]" ?,Lw(B)
 ("a" ?,Ld(B)
 ("s" ?,Lv(B)
 ("d" ?,LR(B)
 ("f" ?,LP(B)
 ("g" ?,L_(B)
 ("h" ?,L`(B)
 ("j" ?,L^(B)
 ("k" ?,L[(B)
 ("l" ?,LT(B)
 (";" ?,LV(B)
 ("'" ?,Lt(B)
;;  ("\\" ?\\)
 ("z" ?,Lo(B)
 ("x" ?,Lg(B)
 ("c" ?,La(B)
 ("v" ?,L\(B)
 ("b" ?,LX(B)
 ("n" ?,Lb(B)
 ("m" ?,Ll(B)
 ("," ?,LQ(B)
 ("." ?,Ln(B)
 ("/" ?.)
;;  ("!" ?!)
 ("@" ?\")
 ("#" ?,Lp(B)
 ("$" ?\;)
;;  ("%" ?%)
 ("^" ?:)
 ("&" ??)
;;  ("*" ?*)
;;  ("(" ?()
;;  (")" ?))
;;  ("_" ?_)
;;  ("+" ?+)
 ("~" ?')
 ("Q" ?,L9(B)
 ("W" ?,LF(B)
 ("E" ?,LC(B)
 ("R" ?,L:(B)
 ("T" ?,L5(B)
 ("Y" ?,L=(B)
 ("U" ?,L3(B)
 ("I" ?,LH(B)
 ("O" ?,LI(B)
 ("P" ?,L7(B)
 ("{" ?,LE(B)
 ("}" ?,L'(B)
 ("A" ?,LD(B)
 ("S" ?,L&(B)
 ("D" ?,L2(B)
 ("F" ?,L0(B)
 ("G" ?,L?(B)
 ("H" ?,L@(B)
 ("J" ?,L>(B)
 ("K" ?,L;(B)
 ("L" ?,L4(B)
 (":" ?,L6(B)
 ("\"" ?,L$(B)
 ("Z" ?,LO(B)
 ("X" ?,LG(B)
 ("C" ?,LA(B)
 ("V" ?,L<(B)
 ("B" ?,L8(B)
 ("N" ?,LB(B)
 ("M" ?,LL(B)
 ("<" ?,L1(B)
 (">" ?,LN(B)
 ("?" ?,)
 ("\\" ?$,1)Q(B)
 ("|" ?$,1)P(B))
;;

;; Alexander Mikhailian says this is of limited use.  It has been
;; popular among emigrants or foreigners who have to type in Cyrillic
;; (mostly Russian) from time to time.
(quail-define-package 
 "cyrillic-yawerty" "Cyrillic" ",L6O(B" nil
 ",LO25@BK(B Roman transcription.

This layout is based on Roman transcription by phonemic resemblance.
When preceded by a '/', the second and the third rows (number key row) change
as follows.

  keytop | Q  W  E  R  T  Y  U  I  O  P  A  S  D
 --------+---------------------------------------
  input  | ,L"(B  ,L#(B  ,L$(B  ,L%(B  ,L&(B  ,L'(B  ,L((B  ,L)(B  ,L*(B  ,L+(B  ,L,(B  ,L.(B  ,L/(B"
 nil t t t t nil nil nil nil nil t)

;;  1! 2,Lq(B 3,Lj(B 4,L!(B 5% 6^ 7& 8* 9( 0) -_ ,LG(B  ,LN(B
;;   ,LO(B  ,L2(B  ,L5(B  ,L@(B  ,LB(B  ,LK(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,LI(B
;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L9(B  ,L:(B  ,L;(B  ;: '" ,LM(B
;;     ,L7(B  ,LL(B  ,LF(B  ,L6(B  ,L1(B  ,L=(B  ,L<(B  ,< .> /?

;;  1! 2,Lq(B 3,Lj(B 4,L!(B 5% 6^ 7& 8* 9( 0) -_ ,LG(B  ,LN(B
;;   ,L"(B  ,L#(B  ,L$(B  ,L%(B  ,L&(B  ,L'(B  ,L((B  ,L)(B  ,L*(B  ,L+(B  ,LH(B  ,LI(B
;;    ,L,(B  ,L.(B  ,L/(B  ,LD(B  ,L3(B  ,LE(B  ,L9(B  ,L:(B  ,L;(B  ;: '" ,LM(B
;;     ,L7(B  ,LL(B  ,LF(B  ,L6(B  ,L1(B  ,L=(B  ,L<(B  ,< .> /?

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
 ("-" ?-)
 ("=" ?,Lg(B)
 ("`" ?,Ln(B)
 ("q" ?,Lo(B)
 ("w" ?,LR(B)
 ("e" ?,LU(B)
 ("r" ?,L`(B)
 ("t" ?,Lb(B)
 ("y" ?,Lk(B)
 ("u" ?,Lc(B)
 ("i" ?,LX(B)
 ("o" ?,L^(B)
 ("p" ?,L_(B)
 ("[" ?,Lh(B)
 ("]" ?,Li(B)
 ("a" ?,LP(B)
 ("s" ?,La(B)
 ("d" ?,LT(B)
 ("f" ?,Ld(B)
 ("g" ?,LS(B)
 ("h" ?,Le(B)
 ("j" ?,LY(B)
 ("k" ?,LZ(B)
 ("l" ?,L[(B)
 (";" ?\;)
 ("'" ?')
 ("\\" ?,Lm(B)
 ("z" ?,LW(B)
 ("x" ?,Ll(B)
 ("c" ?,Lf(B)
 ("v" ?,LV(B)
 ("b" ?,LQ(B)
 ("n" ?,L](B)
 ("m" ?,L\(B)
 ("," ?,)
 ("." ?.)
 ("/" ?/)
 
 ("!" ?!)
 ("@" ?,Lq(B)
 ("#" ?,Lj(B)
 ("$" ?,L!(B)
 ("%" ?%)
 ("^" ?^)
 ("&" ?&)
 ("*" ?*)
 ("(" ?\()
 (")" ?\))
 ("_" ?_)
 ("+" ?,LG(B)
 ("~" ?,LN(B)
 ("Q" ?,LO(B)
 ("W" ?,L2(B)
 ("E" ?,L5(B)
 ("R" ?,L@(B)
 ("T" ?,LB(B)
 ("Y" ?,LK(B)
 ("U" ?,LC(B)
 ("I" ?,L8(B)
 ("O" ?,L>(B)
 ("P" ?,L?(B)
 ("{" ?,LH(B)
 ("}" ?,LI(B)
 ("A" ?,L0(B)
 ("S" ?,LA(B)
 ("D" ?,L4(B)
 ("F" ?,LD(B)
 ("G" ?,L3(B)
 ("H" ?,LE(B)
 ("J" ?,L9(B)
 ("K" ?,L:(B)
 ("L" ?,L;(B)
 (":" ?:)
 ("\"" ?\")
 ("|" ?,LM(B)
 ("Z" ?,L7(B)
 ("X" ?,LL(B)
 ("C" ?,LF(B)
 ("V" ?,L6(B)
 ("B" ?,L1(B)
 ("N" ?,L=(B)
 ("M" ?,L<(B)
 ("<" ?<)
 (">" ?>)
 ("?" ??)
 
 ("/q" ?,Lr(B)
 ("/w" ?,Ls(B)
 ("/e" ?,Lt(B)
 ("/r" ?,Lu(B)
 ("/t" ?,Lv(B)
 ("/y" ?,Lw(B)
 ("/u" ?,Lx(B)
 ("/i" ?,Ly(B)
 ("/o" ?,Lz(B)
 ("/p" ?,L{(B)
 ("/a" ?,L|(B)
 ("/s" ?,L~(B)
 ("/d" ?,L(B)
 
 ("/Q" ?,L"(B)
 ("/W" ?,L#(B)
 ("/E" ?,L$(B)
 ("/R" ?,L%(B)
 ("/T" ?,L&(B)
 ("/Y" ?,L'(B)
 ("/U" ?,L((B)
 ("/I" ?,L)(B)
 ("/O" ?,L*(B)
 ("/P" ?,L+(B)
 ("/A" ?,L,(B)
 ("/S" ?,L.(B)
 ("/D" ?,L/(B))

;; This was provided by Valery Alexeev <valery@domovoy.math.uga.edu>.

;; Ognyan Kulev <ogi@fmi.uni-sofia.bg> wrote:

;; I would suggest future `cyrillic-translit' to be with the
;; modification of `cyrillic-translit-bulgarian' applied and the
;; latter to disappear.  It could be used by people who write
;; bulgarian e-mails with latin letters for kick start (phonetic input
;; method is not so obvious as translit input method but each letter
;; is one keypress and a *lot* of people know it).

(quail-define-package
 "cyrillic-translit" "Cyrillic" ",L6(Bt" nil
 "Intuitively transliterated keyboard layout.
Most convenient for entering Russian but all Cyrillic characters are included.
Should handle most cases. However:
  for ,Lf(B (TSE) use \"c\", never \"ts\"
  ,Li(B (SHCHA = Bulgarian SHT) = \"shch\", \"sj\", \"/sht\" or \"/t\",
  ,Lm(B (REVERSE ROUNDED E) = \"e'\" or \"e`\"
  ,Le(B (KHA) when after ,La(B (S) = \"x\" or \"kh\"
  ,Lj(B (HARD SIGN) = \"~\", ,LJ(B (CAPITAL HARD SIGN) = \"~~\",
  ,Ll(B (SOFT SIGN) = \"'\", ,LL(B (CAPITAL SOFT SIGN) = \"''\",
  ,Lo(B (YA) = \"ya\", \"ja\" or \"q\".

Russian alphabet: a b v=w g d e yo=jo zh z i j=j' k l m n o p r s t
u f h=kh=x c ch sh shch=sj=/s=/sht ~ y ' e' yu=ju ya=ja=q

Also included are Ukrainian ,Lt(B (YE) = \"/e\" and ,Lw(B (YI) = \"yi\", 
Belarusian ,L~(B (SHORT U) = \"u'\",
Serbo-Croatian ,Lr(B (DJE) = \"/d\", ,L{(B (CHJE)= \"/ch\", 
Macedonian ,Ls(B (GJE) = \"/g\", ,Lu(B (DZE) = \"/s\", ,L|(B (KJE) = \"/k\",
cyrillic ,Lv(B (I DECIMAL) = \"/i\", ,Lx(B (JE) = \"/j\", 
,Ly(B (LJE) = \"/l\", ,Lz(B (NJE) = \"/n\" and ,L(B (DZE) =\"/z\"."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("a" ?,LP(B)("b" ?,LQ(B) ("v" ?,LR(B) ("w" ?,LR(B) ("g" ?,LS(B) ("d" ?,LT(B) 
 ("e" ?,LU(B) ("je" ?,LU(B) 
 ("yo" ?,Lq(B) ("jo" ?,Lq(B)
 ("zh" ?,LV(B) ("z" ?,LW(B) ("i" ?,LX(B) 
 ("j" ?,LY(B) ("j'" ?,LY(B) ("j`" ?,LY(B) ("k" ?,LZ(B) ("l" ?,L[(B)
 ("m" ?,L\(B) ("n" ?,L](B) ("o" ?,L^(B) ("p" ?,L_(B) ("r" ?,L`(B) ("s" ?,La(B) ("t" ?,Lb(B) ("u" ?,Lc(B)
 ("f" ?,Ld(B) ("x" ?,Le(B) ("h" ?,Le(B) ("kh" ?,Le(B)
 ("c" ?,Lf(B) ("ch" ?,Lg(B)
 ("sh" ?,Lh(B) 
 ("shch" ?,Li(B) ("sj" ?,Li(B) 
 ("/sht" ?,Li(B) ("/t" ?,Li(B) 
 ("~" ?,Lj(B) ("y" ?,Lk(B) ("'" ?,Ll(B) ("`" ?,Ll(B) 
 ("e'" ?,Lm(B) ("e`" ?,Lm(B) ("@" ?,Lm(B) 
 ("yu" ?,Ln(B) ("ju" ?,Ln(B) 
 ("ya" ?,Lo(B) ("ja" ?,Lo(B) ("q" ?,Lo(B)

 ("A" ?,L0(B) ("B" ?,L1(B) ("V" ?,L2(B) ("W" ?,L2(B) ("G" ?,L3(B) ("D" ?,L4(B) 
 ("E" ?,L5(B) ("Je" ?,L5(B) ("JE" ?,L5(B)
 ("Yo" ?,L!(B) ("YO" ?,L!(B) ("Jo" ?,L!(B) ("JO" ?,L!(B) 
 ("Zh" ?,L6(B) ("ZH" ?,L6(B) ("Z" ?,L7(B) ("I" ?,L8(B) 
 ("J" ?,L9(B) ("J'" ?,L9(B) ("J`" ?,L9(B) ("K" ?,L:(B) ("L" ?,L;(B)
 ("M" ?,L<(B) ("N" ?,L=(B) ("O" ?,L>(B) ("P" ?,L?(B) ("R" ?,L@(B) ("S" ?,LA(B) ("T" ?,LB(B) ("U" ?,LC(B)
 ("F" ?,LD(B) ("X" ?,LE(B) ("H" ?,LE(B) ("Kh" ?,LE(B) ("KH" ?,LE(B)
 ("C" ?,LF(B) ("Ch" ?,LG(B) ("CH" ?,LG(B) 
 ("Sh" ?,LH(B) ("SH" ?,LH(B) 
 ("Shch" ?,LI(B) ("SHCH" ?,LI(B) ("Sj" ?,LI(B) ("SJ" ?,LI(B) 
 ("/Sht" ?,LI(B) ("/SHT" ?,LI(B) ("/T" ?,LI(B) 
 ("~~" ",LJ(B") ("Y" ?,LK(B) ("''" ",LL(B") ("E'" ?,LM(B) ("E`" ?,LM(B) 
 ("Yu" ?,LN(B) ("YU" ?,LN(B) ("Ju" ?,LN(B) ("JU" ?,LN(B) 
 ("Ya" ?,LO(B) ("YA" ?,LO(B) ("Ja" ?,LO(B) ("JA" ?,LO(B) ("Q" ?,LO(B)

 ("/e" ?,Lt(B) ("yi" ?,Lw(B) ("u'" ?,L~(B)
 ("/d" ?,Lr(B) ("/ch" ?,L{(B)
 ("/g" ?,Ls(B) ("/s" ?,Lu(B) ("/k" ?,L|(B)
 ("/i" ?,Lv(B) ("/j" ?,Lx(B) ("/l" ?,Ly(B) ("/n" ?,Lz(B) ("/z" ?,L(B)
 ("/E" ?,L$(B) ("YE" ?,L$(B) ("Yi" ?,L'(B) ("YI" ?,L'(B) ("U'" ?,L.(B) 
 ("/D" ?,L"(B) ("/Ch" ?,L+(B) ("/CH" ?,L+(B)
 ("/G" ?,L#(B) ("/S" ?,L%(B) ("/K" ?,L,(B) 
 ("/I" ?,L&(B) ("/J" ?,L((B) ("/L" ?,L)(B) ("/N" ?,L*(B) ("/Z" ?,L/(B)
)

;; Originally from Yudit's `Belarusian input table according to
;; STB955-94 belarusian standard' by Alexander Mikhailian
;; <mikhailian@altern.org>, subsequently amended by AM.
(quail-define-package
 "belarusian" "Belarusian" "BE" nil
 ",L9FC:5=(B keyboard layout registered as STB955-94 Belarusian standard."
 nil t t t t nil nil nil nil nil t)

;; ,Lq!(B 1! 2" 3N 4; 5% 6: 7? 8* 9( 0) -_ =+
;;     ,L9(B  ,LF(B  ,LC(B  ,L:(B  ,L5(B  ,L=(B  ,L3(B  ,LH(B  ,L.(B ,L7(B  ,LE(B  '
;;      ,LD(B  ,LK(B  ,L2(B  ,L0(B  ,L?(B  ,L@(B  ,L>(B  ,L;(B  ,L4(B ,L6(B  ,LM(B
;;       ,LO(B  ,LG(B  ,LA(B  ,L<(B  ,L&(B  ,LB(B  ,LL(B  ,L1(B  ,LN(B  .,

(quail-define-rules
 ("~" ?,L!(B)
 ("@" ?\")
 ("#" ?,Lp(B)
 ("$" ?\;)
 ("%" ?%)
 ("^" ?:)
 ("&" ??)
 ("Q" ?,L9(B)
 ("W" ?,LF(B)
 ("E" ?,LC(B)
 ("R" ?,L:(B)
 ("T" ?,L5(B)
 ("Y" ?,L=(B)
 ("U" ?,L3(B)
 ("I" ?,LH(B)
 ("O" ?,L.(B)
 ("P" ?,L7(B)
 ("{" ?,LE(B)
 ("}" ?')
 ("A" ?,LD(B)
 ("S" ?,LK(B)
 ("D" ?,L2(B)
 ("F" ?,L0(B)
 ("G" ?,L?(B)
 ("H" ?,L@(B)
 ("J" ?,L>(B)
 ("K" ?,L;(B)
 ("L" ?,L4(B)
 (":" ?,L6(B)
 ("\"" ?,LM(B)
 ("|" ?|)
 ("Z" ?,LO(B)
 ("X" ?,LG(B)
 ("C" ?,LA(B)
 ("V" ?,L<(B)
 ("B" ?,L&(B)
 ("N" ?,LB(B)
 ("M" ?,LL(B)
 ("<" ?,L1(B)
 (">" ?,LN(B)
 ("?" ?,)

 ("`" ?,Lq(B)
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
 ("q" ?,LY(B)
 ("w" ?,Lf(B)
 ("e" ?,Lc(B)
 ("r" ?,LZ(B)
 ("t" ?,LU(B)
 ("y" ?,L](B)
 ("u" ?,LS(B)
 ("i" ?,Lh(B)
 ("o" ?,L~(B)
 ("p" ?,LW(B)
 ("[" ?,Le(B)
 ("]" ?')
 ("a" ?,Ld(B)
 ("s" ?,Lk(B)
 ("d" ?,LR(B)
 ("f" ?,LP(B)
 ("g" ?,L_(B)
 ("h" ?,L`(B)
 ("j" ?,L^(B)
 ("k" ?,L[(B)
 ("l" ?,LT(B)
 (";" ?,LV(B)
 ("'" ?,Lm(B)
 ("\\" ?\\)
 ("z" ?,Lo(B)
 ("x" ?,Lg(B)
 ("c" ?,La(B)
 ("v" ?,L\(B)
 ("b" ?,Lv(B)
 ("n" ?,Lb(B)
 ("m" ?,Ll(B)
 ("," ?,LQ(B)
 ("." ?,Ln(B)
 ("/" ?.))

;; From `Bulgarian-PHO.kmap for Yudit', Alexander Shopov
;; <al_shopov@web.bg>.

;; Extra commentary and the indicator from an independent
;; (cyrillic-iso8859-5) implementation by Ognyan Kulev
;; <ogi@fmi.uni-sofia.bg>.
(quail-define-package
 "bulgarian-pho" "Bulgarian" ",L61D(B"
 nil
 "Bulgarian Phonetic keyboard layout.

The layout is similar to `cyrillic-translit', but all Bulgarian
characters are typed with a single key.

Use /& for ,A'(B (Cyrillic paragraph) and /# for ,Lp(B."
 nil t t t t nil nil nil nil nil t)

;;   ,LO(B  ,L2(B  ,L5(B  ,L@(B  ,LB(B  ,LJ(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,LI(B
;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L9(B  ,L:(B  ,L;(B        ,LG(B
;;  ,LN(B  ,L7(B  ,LL(B  ,LF(B  ,L6(B  ,L1(B  ,L=(B  ,L<(B

(quail-define-rules
 ("/&" ?,A'(B)
 ("/#" ?,Lp(B)
 ("A" ?,L0(B)
 ("B" ?,L1(B)
 ("W" ?,L2(B)
 ("G" ?,L3(B)
 ("D" ?,L4(B)
 ("E" ?,L5(B)
 ("V" ?,L6(B)
 ("Z" ?,L7(B)
 ("I" ?,L8(B)
 ("J" ?,L9(B)
 ("K" ?,L:(B)
 ("L" ?,L;(B)
 ("M" ?,L<(B)
 ("N" ?,L=(B)
 ("O" ?,L>(B)
 ("P" ?,L?(B)
 ("R" ?,L@(B)
 ("S" ?,LA(B)
 ("T" ?,LB(B)
 ("U" ?,LC(B)
 ("F" ?,LD(B)
 ("H" ?,LE(B)
 ("C" ?,LF(B)
 ("~" ?,LG(B)
 ("{" ?,LH(B)
 ("}" ?,LI(B)
 ("Y" ?,LJ(B)
 ("X" ?,LL(B)
 ("|" ?,LN(B)
 ("Q" ?,LO(B)
 ("a" ?,LP(B)
 ("b" ?,LQ(B)
 ("w" ?,LR(B)
 ("g" ?,LS(B)
 ("d" ?,LT(B)
 ("e" ?,LU(B)
 ("v" ?,LV(B)
 ("z" ?,LW(B)
 ("i" ?,LX(B)
 ("j" ?,LY(B)
 ("k" ?,LZ(B)
 ("l" ?,L[(B)
 ("m" ?,L\(B)
 ("n" ?,L](B)
 ("o" ?,L^(B)
 ("p" ?,L_(B)
 ("r" ?,L`(B)
 ("s" ?,La(B)
 ("t" ?,Lb(B)
 ("u" ?,Lc(B)
 ("f" ?,Ld(B)
 ("h" ?,Le(B)
 ("c" ?,Lf(B)
 ("`" ?,Lg(B)
 ("[" ?,Lh(B)
 ("]" ?,Li(B)
 ("y" ?,Lj(B)
 ("x" ?,Ll(B)
 ("\\" ?,Ln(B)
 ("|" ?,LN(B)
 ("q" ?,Lo(B))

;; Based on an implementation by Ognyan Kulev <ogi@fmi.uni-sofia.bg>.
;; This follows XKB bg.

(quail-define-package
 "bulgarian-standard" "Bulgarian" ",L14A(B" nil
 "Bulgarian standard keyboard layout (BDS)

This keyboard layout is standard for Bulgarian typewriters."
 nil t t t t nil nil nil nil nil t)

;;  1! 2? 3+ 4" 5% 6= 7: 8/ 9_ 0,Lp(B -I .V
;;   ,,Lk(B ,LC(B  ,L5(B  ,L8(B  ,LH(B  ,LI(B  ,L:(B  ,LA(B  ,L4(B  ,L7(B  ,LF(B  ;,A'(B
;;    ,Ll(B  ,LO(B  ,L0(B  ,L>(B  ,L6(B  ,L3(B  ,LB(B  ,L=(B  ,L2(B  ,L<(B  ,LG(B  ()
;;     ,LN(B  ,L9(B  ,LJ(B  ,LM(B  ,LD(B  ,LE(B  ,L?(B  ,L@(B  ,L;(B  ,L1(B

(quail-define-rules

 ("1" ?1) ("!" ?!)
 ("2" ?2)
 ("@" ??)
 ("3" ?3)
 ("#" ?+)
 ("4" ?4)
 ("$" ?\")
 ("5" ?5) ("%" ?%)
 ("6" ?6)
 ("^" ?=)
 ("7" ?7)
 ("&" ?:)
 ("8" ?8)
 ("*" ?/)
 ("9" ?9)
 ("(" ?_)
 ("0" ?0)
 (")" ?,Lp(B)
 ("-" ?-)
 ("_" ?I)
 ("=" ?.) ("+" ?V)

 ("q" ?,) ("Q" ?,Lk(B)
 ("w" ?,Lc(B) ("W" ?,LC(B)
 ("e" ?,LU(B) ("E" ?,L5(B)
 ("r" ?,LX(B) ("R" ?,L8(B)
 ("t" ?,Lh(B) ("T" ?,LH(B)
 ("y" ?,Li(B) ("Y" ?,LI(B)
 ("u" ?,LZ(B) ("U" ?,L:(B)
 ("i" ?,La(B) ("I" ?,LA(B)
 ("o" ?,LT(B) ("O" ?,L4(B)
 ("p" ?,LW(B) ("P" ?,L7(B)
 ("[" ?,Lf(B) ("{" ?,LF(B)
 ("]" ?\;)
 ("}" ?,A'(B) ;; not in XKB's bg

 ("a" ?,Ll(B) ("A" ?,LL(B)
 ("s" ?,Lo(B) ("S" ?,LO(B)
 ("d" ?,LP(B) ("D" ?,L0(B)
 ("f" ?,L^(B) ("F" ?,L>(B)
 ("g" ?,LV(B) ("G" ?,L6(B)
 ("h" ?,LS(B) ("H" ?,L3(B)
 ("j" ?,Lb(B) ("J" ?,LB(B)
 ("k" ?,L](B) ("K" ?,L=(B)
 ("l" ?,LR(B) ("L" ?,L2(B)
 (";" ?,L\(B) (":" ?,L<(B)
 ("'" ?,Lg(B) ("\"" ?,LG(B)
 ("`" ?\() ("~" ?\))

 ("z" ?,Ln(B) ("Z" ?,LN(B)
 ("x" ?,LY(B) ("X" ?,L9(B)
 ("c" ?,Lj(B) ("C" ?,LJ(B)
 ("v" ?,Lm(B) ("V" ?,LM(B)
 ("b" ?,Ld(B) ("B" ?,LD(B)
 ("n" ?,Le(B) ("N" ?,LE(B)
 ("m" ?,L_(B) ("M" ?,L?(B)
 ("," ?,L`(B) ("<" ?,L@(B)
 ("." ?,L[(B) (">" ?,L;(B)
 ("/" ?,LQ(B) ("?" ?,L1(B)
 ("\\" ?') ("|" ?,LK(B))

;; Local Variables:
;; coding: iso-2022-7bit
;; End:

;;; cyrillic.el ends here
