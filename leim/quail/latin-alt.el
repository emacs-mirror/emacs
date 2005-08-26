;;; latin-alt.el --- Quail package for inputting various European characters -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.

;; Keywords: multilingual, input method, latin

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

;; Author: TAKAHASHI Naoto <ntakahas@etl.go.jp>

;;; Commentary:

;;; Code:

(require 'quail)

(quail-define-package
 "latin-1-alt-postfix" "Latin-1" "1<" t
 "Latin-1 character input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Aa(B
  grave      |    `    | a` -> ,A`(B
  circumflex |    ^    | a^ -> ,Ab(B
  diaeresis  |    \"    | a\" -> ,Ad(B
  tilde      |    ~    | a~ -> ,Ac(B
  cedilla    |    /    | c/ -> ,Ag(B
  nordic     |    /    | d/ -> ,Ap(B   t/ -> ,A~(B   a/ -> ,Ae(B   e/ -> ,Af(B   o/ -> ,Ax(B
  others     |   /<>   | s/ -> ,A_(B   ?/ -> ,A?(B   !/ -> ,A!(B
             | various | << -> ,A+(B   >> -> ,A;(B   o_ -> ,A:(B   a_ -> ,A*(B

It would be natural to use comma for cedillas, but that would be
inconvenient in practice because commas are needed very often after a
letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?,A@(B)
 ("A'" ?,AA(B)
 ("A^" ?,AB(B)
 ("A~" ?,AC(B)
 ("A\"" ?,AD(B)
 ("A/" ?,AE(B)
 ("a`" ?,A`(B)
 ("a'" ?,Aa(B)
 ("a^" ?,Ab(B)
 ("a~" ?,Ac(B)
 ("a\"" ?,Ad(B)
 ("a/" ?,Ae(B)
 ("E`" ?,AH(B)
 ("E'" ?,AI(B)
 ("E^" ?,AJ(B)
 ("E\"" ?,AK(B)
 ("E/" ?,AF(B)
 ("e`" ?,Ah(B)
 ("e'" ?,Ai(B)
 ("e^" ?,Aj(B)
 ("e\"" ?,Ak(B)
 ("e/" ?,Af(B)
 ("I`" ?,AL(B)
 ("i`" ?,Al(B)
 ("I'" ?,AM(B)
 ("i'" ?,Am(B)
 ("I^" ?,AN(B)
 ("i^" ?,An(B)
 ("I\"" ?,AO(B)
 ("i\"" ?,Ao(B)
 ("O`" ?,AR(B)
 ("o`" ?,Ar(B)
 ("O'" ?,AS(B)
 ("o'" ?,As(B)
 ("O^" ?,AT(B)
 ("o^" ?,At(B)
 ("O~" ?,AU(B)
 ("o~" ?,Au(B)
 ("O\"" ?,AV(B)
 ("o\"" ?,Av(B)
 ("O/" ?,AX(B)
 ("o/" ?,Ax(B)
 ("U`" ?,AY(B)
 ("u`" ?,Ay(B)
 ("U'" ?,AZ(B)
 ("u'" ?,Az(B)
 ("U^" ?,A[(B)
 ("u^" ?,A{(B)
 ("U\"" ?,A\(B)
 ("u\"" ?,A|(B)
 ("Y'" ?,A](B)
 ("y'" ?,A}(B)
 ("y\"" ?,A(B)
 ("D/" ?,AP(B)
 ("d/" ?,Ap(B)
 ("T/" ?,A^(B)
 ("t/" ?,A~(B)
 ("s/" ?,A_(B)
 ("C/" ?,AG(B)
 ("c/" ?,Ag(B)
 ("N~" ?,AQ(B)
 ("n~" ?,Aq(B)
 ("?/" ?,A?(B)
 ("!/" ?,A!(B)
 ("<<" ?,A+(B)
 (">>" ?,A;(B)
 ("o_" ?,A:(B)
 ("a_" ?,A*(B)

 ("A``" ["A`"])
 ("A''" ["A'"])
 ("A^^" ["A^"])
 ("A~~" ["A~"])
 ("A\"\"" ["A\""])
 ("A//" ["A/"])
 ("a``" ["a`"])
 ("a''" ["a'"])
 ("a^^" ["a^"])
 ("a~~" ["a~"])
 ("a\"\"" ["a\""])
 ("a//" ["a/"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("E^^" ["E^"])
 ("E\"\"" ["E\""])
 ("E//" ["E/"])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("e^^" ["e^"])
 ("e\"\"" ["e\""])
 ("e//" ["e/"])
 ("I``" ["I`"])
 ("i``" ["i`"])
 ("I''" ["I'"])
 ("i''" ["i'"])
 ("I^^" ["I^"])
 ("i^^" ["i^"])
 ("I\"\"" ["I\""])
 ("i\"\"" ["i\""])
 ("O``" ["O`"])
 ("o``" ["o`"])
 ("O''" ["O'"])
 ("o''" ["o'"])
 ("O^^" ["O^"])
 ("o^^" ["o^"])
 ("O~~" ["O~"])
 ("o~~" ["o~"])
 ("O\"\"" ["O\""])
 ("o\"\"" ["o\""])
 ("O//" ["O/"])
 ("o//" ["o/"])
 ("U``" ["U`"])
 ("u``" ["u`"])
 ("U''" ["U'"])
 ("u''" ["u'"])
 ("U^^" ["U^"])
 ("u^^" ["u^"])
 ("U\"\"" ["U\""])
 ("u\"\"" ["u\""])
 ("Y''" ["Y'"])
 ("y''" ["y'"])
 ("y\"\"" ["y\""])
 ("D//" ["D/"])
 ("d//" ["d/"])
 ("T//" ["T/"])
 ("t//" ["t/"])
 ("s//" ["s/"])
 ("C//" ["C/"])
 ("c//" ["c/"])
 ("N~~" ["N~"])
 ("n~~" ["n~"])
 ("?//" ["?/"])
 ("!//" ["!/"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("o__" ["o_"])
 ("a__" ["a_"])
 )

(quail-define-package
 "latin-2-alt-postfix" "Latin-2" "2<" t
 "Latin-2 character input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Ba(B
  ogonek     |    `    | a` -> ,B1(B
  diaeresis  |    \"    | a\" -> ,Bd(B
  circumflex |    ^    | a^ -> ,Bb(B
  breve      |    ~    | a~ -> ,Bc(B
  cedilla    |    `    | c` -> ,Bg(B
  caron      |    ~    | c~ -> ,Bh(B
  dbl. acute |    :    | o: -> ,Bu(B
  ring       |    `    | u` -> ,By(B
  dot        |    `    | z` -> ,B?(B
  stroke     |    /    | d/ -> ,Bp(B
  others     |    /    | s/ -> ,B_(B

It would be natural to use period and comma for dots/rings and
cedillas/ogoneks, but that would inconvenient in practice, because
periods and commas are needed very often after a letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?,BA(B)
 ("A`" ?,B!(B)
 ("A\"" ?,BD(B)
 ("A^" ?,BB(B)
 ("A~" ?,BC(B)
 ("C'" ?,BF(B)
 ("C`" ?,BG(B)
 ("C~" ?,BH(B)
 ("D/" ?,BP(B)
 ("D~" ?,BO(B)
 ("E'" ?,BI(B)
 ("E`" ?,BJ(B)
 ("E\"" ?,BK(B)
 ("E~" ?,BL(B)
 ("I'" ?,BM(B)
 ("I^" ?,BN(B)
 ("L'" ?,BE(B)
 ("L/" ?,B#(B)
 ("L~" ?,B%(B)
 ("N'" ?,BQ(B)
 ("N~" ?,BR(B)
 ("O'" ?,BS(B)
 ("O:" ?,BU(B)
 ("O\"" ?,BV(B)
 ("O^" ?,BT(B)
 ("R'" ?,B@(B)
 ("R~" ?,BX(B)
 ("S'" ?,B&(B)
 ("S`" ?,B*(B)
 ("S~" ?,B)(B)
 ("T`" ?,B^(B)
 ("T~" ?,B+(B)
 ("U'" ?,BZ(B)
 ("U:" ?,B[(B)
 ("U\"" ?,B\(B)
 ("U`" ?,BY(B)
 ("Y'" ?,B](B)
 ("Z'" ?,B,(B)
 ("Z`" ?,B/(B)
 ("Z~" ?,B.(B)
 ("a'" ?,Ba(B)
 ("a`" ?,B1(B)
 ("a\"" ?,Bd(B)
 ("a^" ?,Bb(B)
 ("a~" ?,Bc(B)
 ("c'" ?,Bf(B)
 ("c`" ?,Bg(B)
 ("c~" ?,Bh(B)
 ("d/" ?,Bp(B)
 ("d~" ?,Bo(B)
 ("e'" ?,Bi(B)
 ("e`" ?,Bj(B)
 ("e\"" ?,Bk(B)
 ("e~" ?,Bl(B)
 ("i'" ?,Bm(B)
 ("i^" ?,Bn(B)
 ("l'" ?,Be(B)
 ("l/" ?,B3(B)
 ("l~" ?,B5(B)
 ("n'" ?,Bq(B)
 ("n~" ?,Br(B)
 ("o'" ?,Bs(B)
 ("o:" ?,Bu(B)
 ("o\"" ?,Bv(B)
 ("o^" ?,Bt(B)
 ("r'" ?,B`(B)
 ("r~" ?,Bx(B)
 ("s'" ?,B6(B)
 ("s`" ?,B:(B)
 ("s/" ?,B_(B)
 ("s~" ?,B9(B)
 ("t`" ?,B~(B)
 ("t~" ?,B;(B)
 ("u'" ?,Bz(B)
 ("u:" ?,B{(B)
 ("u\"" ?,B|(B)
 ("u`" ?,By(B)
 ("y'" ?,B}(B)
 ("z'" ?,B<(B)
 ("z`" ?,B?(B)
 ("z~" ?,B>(B)

 ("A''" ["A'"])
 ("A``" ["A`"])
 ("A\"\"" ["A\""])
 ("A^^" ["A^"])
 ("A~~" ["A~"])
 ("C''" ["C'"])
 ("C``" ["C`"])
 ("C~~" ["C~"])
 ("D//" ["D/"])
 ("D~~" ["D~"])
 ("E''" ["E'"])
 ("E``" ["E`"])
 ("E\"\"" ["E\""])
 ("E~~" ["E~"])
 ("I''" ["I'"])
 ("I^^" ["I^"])
 ("L''" ["L'"])
 ("L//" ["L/"])
 ("L~~" ["L~"])
 ("N''" ["N'"])
 ("N~~" ["N~"])
 ("O''" ["O'"])
 ("O::" ["O:"])
 ("O\"\"" ["O\""])
 ("O^^" ["O^"])
 ("R''" ["R'"])
 ("R~~" ["R~"])
 ("S''" ["S'"])
 ("S``" ["S`"])
 ("S~~" ["S~"])
 ("T``" ["T`"])
 ("T~~" ["T~"])
 ("U''" ["U'"])
 ("U::" ["U:"])
 ("U\"\"" ["U\""])
 ("U``" ["U`"])
 ("Y''" ["Y'"])
 ("Z''" ["Z'"])
 ("Z``" ["Z`"])
 ("Z~~" ["Z~"])
 ("a''" ["a'"])
 ("a``" ["a`"])
 ("a\"\"" ["a\""])
 ("a^^" ["a^"])
 ("a~~" ["a~"])
 ("c''" ["c'"])
 ("c``" ["c`"])
 ("c~~" ["c~"])
 ("d//" ["d/"])
 ("d~~" ["d~"])
 ("e''" ["e'"])
 ("e``" ["e`"])
 ("e\"\"" ["e\""])
 ("e~~" ["e~"])
 ("i''" ["i'"])
 ("i^^" ["i^"])
 ("l''" ["l'"])
 ("l//" ["l/"])
 ("l~~" ["l~"])
 ("n''" ["n'"])
 ("n~~" ["n~"])
 ("o''" ["o'"])
 ("o::" ["o:"])
 ("o\"\"" ["o\""])
 ("o^^" ["o^"])
 ("r''" ["r'"])
 ("r~~" ["r~"])
 ("s''" ["s'"])
 ("s``" ["s`"])
 ("s//" ["s/"])
 ("s~~" ["s~"])
 ("t``" ["t`"])
 ("t~~" ["t~"])
 ("u''" ["u'"])
 ("u::" ["u:"])
 ("u\"\"" ["u\""])
 ("u``" ["u`"])
 ("y''" ["y'"])
 ("z''" ["z'"])
 ("z``" ["z`"])
 ("z~~" ["z~"])
 )

(quail-define-package
 "latin-3-alt-postfix" "Latin-3" "3<" t
 "Latin-3 character input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Ca(B
  grave      |    `    | a` -> ,C`(B
  circumflex |    ^    | a^ -> ,Cb(B
  diaeresis  |    \"    | a\" -> ,Cd(B
  dot        |    /    | c/ -> ,Ce(B   i/ -> ,C9(B   I/ -> ,C)(B
  cedilla    |    `    | c` -> ,Cg(B
  breve      |    ~    | g~ -> ,C;(B
  tilde      |    ~    | n~ -> ,Cq(B
  stroke     |    /    | h/ -> ,C1(B
  others     |    /    | s/ -> ,C_(B

It would be natural to use period and comma for dots and cedillas, but
that would inconvenient in practice, because periods and commas are
needed very often after a letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?,C@(B)
 ("A'" ?,CA(B)
 ("A^" ?,CB(B)
 ("A\"" ?,CD(B)
 ("C/" ?,CE(B)
 ("C^" ?,CF(B)
 ("C`" ?,CG(B)
 ("E`" ?,CH(B)
 ("E'" ?,CI(B)
 ("E^" ?,CJ(B)
 ("E\"" ?,CK(B)
 ("G~" ?,C+(B)
 ("G/" ?,CU(B)
 ("G^" ?,CX(B)
 ("H/" ?,C!(B)
 ("H^" ?,C&(B)
 ("I/" ?,C)(B)
 ("I`" ?,CL(B)
 ("I'" ?,CM(B)
 ("I^" ?,CN(B)
 ("I\"" ?,CO(B)
 ("J^" ?,C,(B)
 ("N~" ?,CQ(B)
 ("O`" ?,CR(B)
 ("O'" ?,CS(B)
 ("O^" ?,CT(B)
 ("O\"" ?,CV(B)
 ("S`" ?,C*(B)
 ("S^" ?,C^(B)
 ("U`" ?,CY(B)
 ("U'" ?,CZ(B)
 ("U^" ?,C[(B)
 ("U\"" ?,C\(B)
 ("U~" ?,C](B)
 ("Z/" ?,C/(B)
 ("a`" ?,C`(B)
 ("a'" ?,Ca(B)
 ("a^" ?,Cb(B)
 ("a\"" ?,Cd(B)
 ("c/" ?,Ce(B)
 ("c^" ?,Cf(B)
 ("c`" ?,Cg(B)
 ("e`" ?,Ch(B)
 ("e'" ?,Ci(B)
 ("e^" ?,Cj(B)
 ("e\"" ?,Ck(B)
 ("g~" ?,C;(B)
 ("g/" ?,Cu(B)
 ("g^" ?,Cx(B)
 ("h/" ?,C1(B)
 ("h^" ?,C6(B)
 ("i/" ?,C9(B)
 ("i`" ?,Cl(B)
 ("i'" ?,Cm(B)
 ("i^" ?,Cn(B)
 ("i\"" ?,Co(B)
 ("j^" ?,C<(B)
 ("n~" ?,Cq(B)
 ("o`" ?,Cr(B)
 ("o'" ?,Cs(B)
 ("o^" ?,Ct(B)
 ("o\"" ?,Cv(B)
 ("s`" ?,C:(B)
 ("s/" ?,C_(B)
 ("s^" ?,C~(B)
 ("u`" ?,Cy(B)
 ("u'" ?,Cz(B)
 ("u^" ?,C{(B)
 ("u\"" ?,C|(B)
 ("u~" ?,C}(B)
 ("z/" ?,C?(B)

 ("A``" ["A`"])
 ("A''" ["A'"])
 ("A^^" ["A^"])
 ("A\"\"" ["A\""])
 ("C//" ["C/"])
 ("C^^" ["C^"])
 ("C``" ["C`"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("E^^" ["E^"])
 ("E\"\"" ["E\""])
 ("G~~" ["G~"])
 ("G//" ["G/"])
 ("G^^" ["G^"])
 ("H//" ["H/"])
 ("H^^" ["H^"])
 ("I//" ["I/"])
 ("I``" ["I`"])
 ("I''" ["I'"])
 ("I^^" ["I^"])
 ("I\"\"" ["I\""])
 ("J^^" ["J^"])
 ("N~~" ["N~"])
 ("O``" ["O`"])
 ("O''" ["O'"])
 ("O^^" ["O^"])
 ("O\"\"" ["O\""])
 ("S``" ["S`"])
 ("S^^" ["S^"])
 ("U``" ["U`"])
 ("U''" ["U'"])
 ("U^^" ["U^"])
 ("U\"\"" ["U\""])
 ("U~~" ["U~"])
 ("Z//" ["Z/"])
 ("a``" ["a`"])
 ("a''" ["a'"])
 ("a^^" ["a^"])
 ("a\"\"" ["a\""])
 ("c//" ["c/"])
 ("c^^" ["c^"])
 ("c``" ["c`"])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("e^^" ["e^"])
 ("e\"\"" ["e\""])
 ("g~~" ["g~"])
 ("g//" ["g/"])
 ("g^^" ["g^"])
 ("h//" ["h/"])
 ("h^^" ["h^"])
 ("i//" ["i/"])
 ("i``" ["i`"])
 ("i''" ["i'"])
 ("i^^" ["i^"])
 ("i\"\"" ["i\""])
 ("j^^" ["j^"])
 ("n~~" ["n~"])
 ("o``" ["o`"])
 ("o''" ["o'"])
 ("o^^" ["o^"])
 ("o\"\"" ["o\""])
 ("s``" ["s`"])
 ("s//" ["s/"])
 ("s^^" ["s^"])
 ("u``" ["u`"])
 ("u''" ["u'"])
 ("u^^" ["u^"])
 ("u\"\"" ["u\""])
 ("u~~" ["u~"])
 ("z//" ["z/"])
 )

(quail-define-package
 "latin-4-alt-postfix" "Latin-4" "4<" t
 "Latin-4 characters input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Da(B
  circumflex |    ^    | a^ -> ,Db(B
  diaeresis  |    \"    | a\" -> ,Dd(B
  ogonek     |    `    | a` -> ,D1(B
  macron     |    -    | a- -> ,D`(B
  tilde      |    ~    | a~ -> ,Dc(B
  caron      |    ~    | c~ -> ,Dh(B
  dot        |    ~    | e~ -> ,Dl(B
  cedilla    |    `    | k` -> ,Ds(B   g` -> ,D;(B
  stroke     |    /    | d/ -> ,Dp(B
  nordic     |    /    | a/ -> ,De(B   e/ -> ,Df(B   o/ -> ,Dx(B
  others     |    /    | s/ -> ,D_(B   n/ -> ,D?(B   k/ -> ,D"(B

It would be natural to use period and comma for dots and
cedillas/ogoneks, but that would inconvenient in practice, because
periods and commas are needed very often after a letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?,D!(B)
 ("A-" ?,D@(B)
 ("A'" ?,DA(B)
 ("A^" ?,DB(B)
 ("A~" ?,DC(B)
 ("A\"" ?,DD(B)
 ("A/" ?,DE(B)
 ("C~" ?,DH(B)
 ("D/" ?,DP(B)
 ("E/" ?,DF(B)
 ("E-" ?,D*(B)
 ("E'" ?,DI(B)
 ("E`" ?,DJ(B)
 ("E\"" ?,DK(B)
 ("E~" ?,DL(B)
 ("G`" ?,D+(B)
 ("I~" ?,D%(B)
 ("I`" ?,DG(B)
 ("I'" ?,DM(B)
 ("I^" ?,DN(B)
 ("I-" ?,DO(B)
 ("K`" ?,DS(B)
 ("L`" ?,D&(B)
 ("N/" ?,D=(B)
 ("N`" ?,DQ(B)
 ("O-" ?,DR(B)
 ("O^" ?,DT(B)
 ("O~" ?,DU(B)
 ("O\"" ?,DV(B)
 ("O/" ?,DX(B)
 ("R`" ?,D#(B)
 ("S~" ?,D)(B)
 ("T/" ?,D,(B)
 ("U`" ?,DY(B)
 ("U'" ?,DZ(B)
 ("U^" ?,D[(B)
 ("U\"" ?,D\(B)
 ("U~" ?,D](B)
 ("U-" ?,D^(B)
 ("Z~" ?,D.(B)
 ("a`" ?,D1(B)
 ("a-" ?,D`(B)
 ("a'" ?,Da(B)
 ("a^" ?,Db(B)
 ("a~" ?,Dc(B)
 ("a\"" ?,Dd(B)
 ("a/" ?,De(B)
 ("c~" ?,Dh(B)
 ("d/" ?,Dp(B)
 ("e/" ?,Df(B)
 ("e-" ?,D:(B)
 ("e'" ?,Di(B)
 ("e`" ?,Dj(B)
 ("e\"" ?,Dk(B)
 ("e~" ?,Dl(B)
 ("g`" ?,D;(B)
 ("i~" ?,D5(B)
 ("i`" ?,Dg(B)
 ("i'" ?,Dm(B)
 ("i^" ?,Dn(B)
 ("i-" ?,Do(B)
 ("k/" ?,D"(B)
 ("k`" ?,Ds(B)
 ("l`" ?,D6(B)
 ("n/" ?,D?(B)
 ("n`" ?,Dq(B)
 ("o-" ?,Dr(B)
 ("o^" ?,Dt(B)
 ("o~" ?,Du(B)
 ("o\"" ?,Dv(B)
 ("o/" ?,Dx(B)
 ("r`" ?,D3(B)
 ("s/" ?,D_(B)
 ("s~" ?,D9(B)
 ("t/" ?,D<(B)
 ("u`" ?,Dy(B)
 ("u'" ?,Dz(B)
 ("u^" ?,D{(B)
 ("u\"" ?,D|(B)
 ("u~" ?,D}(B)
 ("u-" ?,D~(B)
 ("z~" ?,D>(B)

 ("A``" ["A`"])
 ("A--" ["A-"])
 ("A''" ["A'"])
 ("A^^" ["A^"])
 ("A~~" ["A~"])
 ("A\"\"" ["A\""])
 ("A//" ["A/"])
 ("C~~" ["C~"])
 ("D//" ["D/"])
 ("E//" ["E/"])
 ("E--" ["E-"])
 ("E''" ["E'"])
 ("E``" ["E`"])
 ("E\"\"" ["E\""])
 ("E~~" ["E~"])
 ("G``" ["G`"])
 ("I~~" ["I~"])
 ("I``" ["I`"])
 ("I''" ["I'"])
 ("I^^" ["I^"])
 ("I--" ["I-"])
 ("K``" ["K`"])
 ("L``" ["L`"])
 ("N//" ["N/"])
 ("N``" ["N`"])
 ("O--" ["O-"])
 ("O^^" ["O^"])
 ("O~~" ["O~"])
 ("O\"\"" ["O\""])
 ("O//" ["O/"])
 ("R``" ["R`"])
 ("S~~" ["S~"])
 ("T//" ["T/"])
 ("U``" ["U`"])
 ("U''" ["U'"])
 ("U^^" ["U^"])
 ("U\"\"" ["U\""])
 ("U~~" ["U~"])
 ("U--" ["U-"])
 ("Z~~" ["Z~"])
 ("a``" ["a`"])
 ("a--" ["a-"])
 ("a''" ["a'"])
 ("a^^" ["a^"])
 ("a~~" ["a~"])
 ("a\"\"" ["a\""])
 ("a//" ["a/"])
 ("c~~" ["c~"])
 ("d//" ["d/"])
 ("e//" ["e/"])
 ("e--" ["e-"])
 ("e''" ["e'"])
 ("e``" ["e`"])
 ("e\"\"" ["e\""])
 ("e~~" ["e~"])
 ("g``" ["g`"])
 ("i~~" ["i~"])
 ("i``" ["i`"])
 ("i''" ["i'"])
 ("i^^" ["i^"])
 ("i--" ["i-"])
 ("k//" ["k/"])
 ("k``" ["k`"])
 ("l``" ["l`"])
 ("n//" ["n/"])
 ("n``" ["n`"])
 ("o--" ["o-"])
 ("o^^" ["o^"])
 ("o~~" ["o~"])
 ("o\"\"" ["o\""])
 ("o//" ["o/"])
 ("r``" ["r`"])
 ("s//" ["s/"])
 ("s~~" ["s~"])
 ("t//" ["t/"])
 ("u``" ["u`"])
 ("u''" ["u'"])
 ("u^^" ["u^"])
 ("u\"\"" ["u\""])
 ("u~~" ["u~"])
 ("u--" ["u-"])
 ("z~~" ["z~"])
 )

(quail-define-package
 "latin-5-alt-postfix" "Latin-5" "5<" t
 "Latin-5 characters input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Ma(B
  grave      |    `    | a` -> ,M`(B
  circumflex |    ^    | a^ -> ,Mb(B
  diaeresis  |    \"    | a\" -> ,Md(B
  tilde      |    ~    | a~ -> ,Mc(B
  breve      |    ~    | g~ -> ,Mp(B
  cedilla    |    `    | c` -> ,Mg(B
  dot        |    /    | i/ -> ,M}(B   I/ -> ,M](B
  nordic     |    /    | a/ -> ,Me(B   e/ -> ,Mf(B   o/ -> ,Mx(B
  others     |    /    | s/ -> ,M_(B

It would be natural to use period and comma for dots and cedillas, but
that would inconvenient in practice, because periods and commas are
needed very often after a letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?,MA(B)
 ("A/" ?,ME(B)
 ("A\"" ?,MD(B)
 ("A^" ?,MB(B)
 ("A`" ?,M@(B)
 ("A~" ?,MC(B)
 ("C`" ?,MG(B)
 ("E'" ?,MI(B)
 ("E/" ?,MF(B)
 ("E\"" ?,MK(B)
 ("E^" ?,MJ(B)
 ("E`" ?,MH(B)
 ("G~" ?,MP(B)
 ("I'" ?,MM(B)
 ("I/" ?,M](B)
 ("I\"" ?,MO(B)
 ("I^" ?,MN(B)
 ("I`" ?,ML(B)
 ("N~" ?,MQ(B)
 ("O'" ?,MS(B)
 ("O/" ?,MX(B)
 ("O\"" ?,MV(B)
 ("O^" ?,MT(B)
 ("O`" ?,MR(B)
 ("O~" ?,MU(B)
 ("S`" ?,M^(B)
 ("U'" ?,MZ(B)
 ("U\"" ?,M\(B)
 ("U^" ?,M[(B)
 ("U`" ?,MY(B)
 ("a'" ?,Ma(B)
 ("a/" ?,Me(B)
 ("a\"" ?,Md(B)
 ("a^" ?,Mb(B)
 ("a`" ?,M`(B)
 ("a~" ?,Mc(B)
 ("c`" ?,Mg(B)
 ("e'" ?,Mi(B)
 ("e/" ?,Mf(B)
 ("e\"" ?,Mk(B)
 ("e^" ?,Mj(B)
 ("e`" ?,Mh(B)
 ("g~" ?,Mp(B)
 ("i'" ?,Mm(B)
 ("i/" ?,M}(B)
 ("i\"" ?,Mo(B)
 ("i^" ?,Mn(B)
 ("i`" ?,Ml(B)
 ("n~" ?,Mq(B)
 ("o'" ?,Ms(B)
 ("o/" ?,Mx(B)
 ("o\"" ?,Mv(B)
 ("o^" ?,Mt(B)
 ("o`" ?,Mr(B)
 ("o~" ?,Mu(B)
 ("s`" ?,M~(B)
 ("s/" ?,M_(B)
 ("u'" ?,Mz(B)
 ("u\"" ?,M|(B)
 ("u^" ?,M{(B)
 ("u`" ?,My(B)
 ("y\"" ?,M(B)

 ("A''" ["A'"])
 ("A//" ["A/"])
 ("A\"\"" ["A\""])
 ("A^^" ["A^"])
 ("A``" ["A`"])
 ("A~~" ["A~"])
 ("C``" ["C`"])
 ("E''" ["E'"])
 ("E//" ["E/"])
 ("E\"\"" ["E\""])
 ("E^^" ["E^"])
 ("E``" ["E`"])
 ("G~~" ["G~"])
 ("I''" ["I'"])
 ("I//" ["I/"])
 ("I\"\"" ["I\""])
 ("I^^" ["I^"])
 ("I``" ["I`"])
 ("N~~" ["N~"])
 ("O''" ["O'"])
 ("O//" ["O/"])
 ("O\"\"" ["O\""])
 ("O^^" ["O^"])
 ("O``" ["O`"])
 ("O~~" ["O~"])
 ("S``" ["S`"])
 ("U''" ["U'"])
 ("U\"\"" ["U\""])
 ("U^^" ["U^"])
 ("U``" ["U`"])
 ("a''" ["a'"])
 ("a//" ["a/"])
 ("a\"\"" ["a\""])
 ("a^^" ["a^"])
 ("a``" ["a`"])
 ("a~~" ["a~"])
 ("c``" ["c`"])
 ("e''" ["e'"])
 ("e//" ["e/"])
 ("e\"\"" ["e\""])
 ("e^^" ["e^"])
 ("e``" ["e`"])
 ("g~~" ["g~"])
 ("i''" ["i'"])
 ("i//" ["i/"])
 ("i\"\"" ["i\""])
 ("i^^" ["i^"])
 ("i``" ["i`"])
 ("n~~" ["n~"])
 ("o''" ["o'"])
 ("o//" ["o/"])
 ("o\"\"" ["o\""])
 ("o^^" ["o^"])
 ("o``" ["o`"])
 ("o~~" ["o~"])
 ("s``" ["s`"])
 ("s//" ["s/"])
 ("u''" ["u'"])
 ("u\"\"" ["u\""])
 ("u^^" ["u^"])
 ("u``" ["u`"])
 ("y\"\"" ["y\""])
 )

(quail-define-package
 "danish-alt-postfix" "Latin-1" "DA<" t
 "Danish input method (rule: AE -> ,AF(B, OE -> ,AX(B, AA -> ,AE(B, E' -> ,AI(B)

Doubling the postfix separates the letter and postfix: e.g. aee -> ae
"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?,AF(B)
 ("ae" ?,Af(B)
 ("OE" ?,AX(B)
 ("oe" ?,Ax(B)
 ("AA" ?,AE(B)
 ("aa" ?,Ae(B)
 ("E'" ?,AI(B)
 ("e'" ?,Ai(B)

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("AAA" ["AA"])
 ("aaa" ["aa"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 )

(quail-define-package
 "esperanto-alt-postfix" "Latin-3" "EO<" t
 "Esperanto input method with postfix modifiers

A following ^ or x will produce an accented character,
e.g. c^ -> ,Cf(B   gx -> ,Cx(B   u^ -> ,C}(B.

Doubling the postfix separates the letter and postfix,
e.g. a'' -> a'.
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("Cx" ?,CF(B)
 ("C^" ?,CF(B)
 ("cx" ?,Cf(B)
 ("c^" ?,Cf(B)
 ("Gx" ?,CX(B)
 ("G^" ?,CX(B)
 ("gx" ?,Cx(B)
 ("g^" ?,Cx(B)
 ("Hx" ?,C&(B)
 ("H^" ?,C&(B)
 ("hx" ?,C6(B)
 ("h^" ?,C6(B)
 ("Jx" ?,C,(B)
 ("J^" ?,C,(B)
 ("jx" ?,C<(B)
 ("j^" ?,C<(B)
 ("Sx" ?,C^(B)
 ("S^" ?,C^(B)
 ("sx" ?,C~(B)
 ("s^" ?,C~(B)
 ("Ux" ?,C](B)
 ("U^" ?,C](B)
 ("ux" ?,C}(B)
 ("u^" ?,C}(B)

 ("Cxx" ["Cx"])
 ("C^^" ["C^"])
 ("cxx" ["cx"])
 ("c^^" ["c^"])
 ("Gxx" ["Gx"])
 ("G^^" ["G^"])
 ("gxx" ["gx"])
 ("g^^" ["g^"])
 ("Hxx" ["Hx"])
 ("H^^" ["H^"])
 ("hxx" ["hx"])
 ("h^^" ["h^"])
 ("Jxx" ["Jx"])
 ("J^^" ["J^"])
 ("jxx" ["jx"])
 ("j^^" ["j^"])
 ("Sxx" ["Sx"])
 ("S^^" ["S^"])
 ("sxx" ["sx"])
 ("s^^" ["s^"])
 ("Uxx" ["Ux"])
 ("U^^" ["U^"])
 ("uxx" ["ux"])
 ("u^^" ["u^"])
 )

(quail-define-package
 "finnish-alt-postfix" "Latin-1" "FI<" t
 "Finnish (Suomi) input method

AE  -> ,AD(B
AEE -> AE
OE  -> ,AV(B
OEE -> OE
"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?,AD(B)
 ("ae" ?,Ad(B)
 ("OE" ?,AV(B)
 ("oe" ?,Av(B)

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 )

(quail-define-package
 "french-alt-postfix" "French" "FR<" t
 "French (Fran,Ag(Bais) input method with postfix modifiers

` pour grave, ' pour aigu, ^ pour circonflexe, et \" pour tr,Ai(Bma.
Par exemple: a` -> ,A`(B   e' -> ,Ai(B.

,AG(B, ,A+(B, et ,A;(B sont produits par C/, <<, et >>.

En doublant la frappe des diacritiques, ils s'isoleront de la lettre.
Par exemple: e'' -> e'

<e dans l'o> n'est pas disponible."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?,A@(B)
 ("A^" ?,AB(B)
 ("a`" ?,A`(B)
 ("a^" ?,Ab(B)
 ("E`" ?,AH(B)
 ("E'" ?,AI(B)
 ("E^" ?,AJ(B)
 ("E\"" ?,AK(B)
 ("e`" ?,Ah(B)
 ("e'" ?,Ai(B)
 ("e^" ?,Aj(B)
 ("e\"" ?,Ak(B)
 ("I^" ?,AN(B)
 ("I\"" ?,AO(B)
 ("i^" ?,An(B)
 ("i\"" ?,Ao(B)
 ("O^" ?,AT(B)
 ("o^" ?,At(B)
 ("U`" ?,AY(B)
 ("U^" ?,A[(B)
 ("U\"" ?,A\(B)
 ("u`" ?,Ay(B)
 ("u^" ?,A{(B)
 ("u\"" ?,A|(B)
 ("C/" ?,AG(B)
 ("c/" ?,Ag(B)
 ("<<" ?,A+(B)
 (">>" ?,A;(B)

 ("A``" ["A`"])
 ("A^^" ["A^"])
 ("a``" ["a`"])
 ("a^^" ["a^"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("E^^" ["E^"])
 ("E\"\"" ["E\""])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("e^^" ["e^"])
 ("e\"\"" ["e\""])
 ("I^^" ["I^"])
 ("I\"\"" ["I\""])
 ("i^^" ["i^"])
 ("i\"\"" ["i\""])
 ("O^^" ["O^"])
 ("o^^" ["o^"])
 ("U``" ["U`"])
 ("U^^" ["U^"])
 ("U\"\"" ["U\""])
 ("u``" ["u`"])
 ("u^^" ["u^"])
 ("u\"\"" ["u\""])
 ("C//" ["C/"])
 ("c//" ["c/"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 )

(quail-define-package
 "german-alt-postfix" "German" "DE<" t
 "German (Deutsch) input method

ae  -> ,Ad(B
aee -> ae
oe  -> ,Av(B
oee -> oe
ue  -> ,A|(B
uee -> ue
sz  -> ,A_(B
szz -> sz
"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?,AD(B)
 ("ae" ?,Ad(B)
 ("OE" ?,AV(B)
 ("oe" ?,Av(B)
 ("UE" ?,A\(B)
 ("ue" ?,A|(B)
 ("sz" ?,A_(B)

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("UEE" ["UE"])
 ("uee" ["ue"])
 ("szz" ["sz"])
 )

(quail-define-package
 "icelandic-alt-postfix" "Latin-1" "IS<" t
 "Icelandic (,AM(Bslenska) input method with postfix modifiers

A' -> ,AA(B
E' -> ,AI(B
I' -> ,AM(B
O' -> ,AS(B
U' -> ,AZ(B
Y' -> ,A](B
AE -> ,AF(B
OE -> ,AV(B
D/ -> ,AP(B (eth)
T/ -> ,A^(B (thorn)

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?,AA(B)
 ("a'" ?,Aa(B)
 ("E'" ?,AI(B)
 ("e'" ?,Ai(B)
 ("I'" ?,AM(B)
 ("i'" ?,Am(B)
 ("O'" ?,AS(B)
 ("o'" ?,As(B)
 ("U'" ?,AZ(B)
 ("u'" ?,Az(B)
 ("Y'" ?,A](B)
 ("y'" ?,A}(B)
 ("AE" ?,AF(B)
 ("ae" ?,Af(B)
 ("OE" ?,AV(B)
 ("oe" ?,Av(B)
 ("D/" ?,AP(B)
 ("d/" ?,Ap(B)
 ("T/" ?,A^(B)
 ("t/" ?,A~(B)

 ("A''" ["A'"])
 ("a''" ["a'"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 ("I''" ["I'"])
 ("i''" ["i'"])
 ("O''" ["O'"])
 ("o''" ["o'"])
 ("U''" ["U'"])
 ("u''" ["u'"])
 ("Y''" ["Y'"])
 ("y''" ["y'"])
 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("D//" ["D/"])
 ("d//" ["d/"])
 ("T//" ["T/"])
 ("t//" ["t/"])
 )

(quail-define-package
 "italian-alt-postfix" "Latin-1" "IT<" t
 "Italian (Italiano) input method with postfix modifiers

a' -> ,Aa(B    A' -> ,AA(B    a` -> ,A`(B    A` -> ,A@(B    i^ -> ,An(B    << -> ,A+(B
e' -> ,Ai(B    E' -> ,AI(B    e` -> ,Ah(B    E` -> ,AH(B    I^ -> ,AN(B    >> -> ,A;(B
i' -> ,Am(B    I' -> ,AM(B    i` -> ,Al(B    I` -> ,AL(B               o_ -> ,A:(B
o' -> ,As(B    O' -> ,AS(B    o` -> ,Ar(B    O` -> ,AR(B               a_ -> ,A*(B
u' -> ,Az(B    U' -> ,AZ(B    u` -> ,Ay(B    U` -> ,AY(B

This method is for purists who like accents the old way.

Doubling the postfix separates the letter and postfix: e.g. a`` -> a`
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?,A@(B)
 ("A'" ?,AA(B)
 ("a`" ?,A`(B)
 ("a'" ?,Aa(B)
 ("E`" ?,AH(B)
 ("E'" ?,AI(B)
 ("e`" ?,Ah(B)
 ("e'" ?,Ai(B)
 ("I`" ?,AL(B)
 ("i`" ?,Al(B)
 ("I'" ?,AM(B)
 ("i'" ?,Am(B)
 ("I^" ?,AN(B)
 ("i^" ?,An(B)
 ("O`" ?,AR(B)
 ("o`" ?,Ar(B)
 ("O'" ?,AS(B)
 ("o'" ?,As(B)
 ("U`" ?,AY(B)
 ("u`" ?,Ay(B)
 ("U'" ?,AZ(B)
 ("u'" ?,Az(B)
 ("<<" ?,A+(B)
 (">>" ?,A;(B)
 ("o_" ?,A:(B)
 ("a_" ?,A*(B)

 ("A``" ["A`"])
 ("A''" ["A'"])
 ("a``" ["a`"])
 ("a''" ["a'"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("I``" ["I`"])
 ("i``" ["i`"])
 ("I''" ["I'"])
 ("i''" ["i'"])
 ("I^^" ["I^"])
 ("i^^" ["i^"])
 ("O``" ["O`"])
 ("o``" ["o`"])
 ("O''" ["O'"])
 ("o''" ["o'"])
 ("U``" ["U`"])
 ("u``" ["u`"])
 ("U''" ["U'"])
 ("u''" ["u'"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("o__" ["o_"])
 ("a__" ["a_"])
 )

(quail-define-package
 "norwegian-alt-postfix" "Latin-1" "NO<" t
 "Norwegian (Norsk) input method (rule: AE->,AF(B, OE->,AX(B, AA->,AE(B, E'->,AI(B)

Doubling the postfix separates the letter and postfix: e.g. aee -> ae
"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?,AF(B)
 ("ae" ?,Af(B)
 ("OE" ?,AX(B)
 ("oe" ?,Ax(B)
 ("AA" ?,AE(B)
 ("aa" ?,Ae(B)
 ("E'" ?,AI(B)
 ("e'" ?,Ai(B)

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("AAA" ["AA"])
 ("aaa" ["aa"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 )

(quail-define-package
 "scandinavian-alt-postfix" "Latin-1" "SC<" t
 "Scandinavian input method with postfix modifiers
Supported languages are Swidish, Norwegian, Danish, and Finnish.

ae -> ,Af(B
oe -> ,Ax(B
aa -> ,Ae(B
a\" -> ,Ad(B
o\" -> ,Av(B
e' -> ,Ai(B

Doubling the postfix separates the letter and postfix:
aee -> ae   o\"\" -> o\"   etc.
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?,AF(B)
 ("ae" ?,Af(B)
 ("OE" ?,AX(B)
 ("oe" ?,Ax(B)
 ("AA" ?,AE(B)
 ("aa" ?,Ae(B)
 ("A\"" ?,AD(B)
 ("a\"" ?,Ad(B)
 ("O\"" ?,AV(B)
 ("o\"" ?,Av(B)
 ("E'" ?,AI(B)
 ("e'" ?,Ai(B)

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("AAA" ["AA"])
 ("aaa" ["aa"])
 ("A\"\"" ["A\""])
 ("a\"\"" ["a\""])
 ("O\"\"" ["O\""])
 ("o\"\"" ["o\""])
 ("E''" ["E'"])
 ("e''" ["e'"])
 )

(quail-define-package
 "spanish-alt-postfix" "Spanish" "ES<" t
 "Spanish (Espa,Aq(Bol) input method with postfix modifiers

A' -> ,AA(B
E' -> ,AI(B
I' -> ,AM(B
O' -> ,AS(B
U' -> ,AZ(B
N~ -> ,AQ(B
!/ -> ,A!(B
?/ -> ,A?(B

Doubling the postfix separates the letter and postfix:
a'' -> a'   n~~ -> n~, etc.
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?,AA(B)
 ("a'" ?,Aa(B)
 ("E'" ?,AI(B)
 ("e'" ?,Ai(B)
 ("I'" ?,AM(B)
 ("i'" ?,Am(B)
 ("O'" ?,AS(B)
 ("o'" ?,As(B)
 ("U'" ?,AZ(B)
 ("u'" ?,Az(B)
 ("N~" ?,AQ(B)
 ("n~" ?,Aq(B)
 ("?/" ?,A?(B)
 ("!/" ?,A!(B)

 ("A''" ["A'"])
 ("a''" ["a'"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 ("I''" ["I'"])
 ("i''" ["i'"])
 ("O''" ["O'"])
 ("o''" ["o'"])
 ("U''" ["U'"])
 ("u''" ["u'"])
 ("N~~" ["N~"])
 ("n~~" ["n~"])
 ("?//" ["?/"])
 ("!//" ["!/"])
 )

(quail-define-package
 "swedish-alt-postfix" "Latin-1" "SV<" t
 "Swedish (Svenska) input method (rule: AA -> ,AE(B, AE -> ,AD(B, OE -> ,AV(B, E' -> ,AI(B)

Doubling the postfix separates the letter and postfix: e.g. aee -> ae
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AA" ?,AE(B)
 ("aa" ?,Ae(B)
 ("AE" ?,AD(B)
 ("ae" ?,Ad(B)
 ("OE" ?,AV(B)
 ("oe" ?,Av(B)
 ("E'" ?,AI(B)
 ("e'" ?,Ai(B)

 ("AAA" ["AA"])
 ("aaa" ["aa"])
 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 )

(quail-define-package
 "turkish-latin-3-alt-postfix" "Turkish" "TR3<<" t
 "Turkish (T,A|(Brk,Ag(Be) input method with postfix modifiers.

This is for those who use Latin-3 (ISO-8859-3) for Turkish.  If you
use Latin-5 (ISO-8859-9), you should use \"turkish-alt-postfix\" instead.

Note for I, ,C9(B, ,C)(B, i.

A^ -> ,CB(B
C` -> ,CG(B
G^ -> ,C+(B
I  -> I
i  -> ,C9(B
I/ -> ,C)(B
i/ -> i
O\" -> ,CV(B
S` -> ,C*(B
U\" -> ,C\(B
U^ -> ,C[(B

Doubling the postfix separates the letter and postfix: e.g. a^^ -> a^
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A^" ?,CB(B)
 ("a^" ?,Cb(B)
 ("C`" ?,CG(B)
 ("c`" ?,Cg(B)
 ("G^" ?,C+(B)
 ("g^" ?,C;(B)
 ("I/" ?,C)(B)
 ("i" ?,C9(B)
 ("i/" ?i)
 ("O\"" ?,CV(B)
 ("o\"" ?,Cv(B)
 ("S`" ?,C*(B)
 ("s`" ?,C:(B)
 ("U\"" ?,C\(B)
 ("u\"" ?,C|(B)
 ("U^" ?,C[(B)
 ("u^" ?,C{(B)

 ("A^^" ["A^"])
 ("a^^" ["a^"])
 ("C``" ["C`"])
 ("c``" ["c`"])
 ("G^^" ["G^"])
 ("g^^" ["g^"])
 ("I//" ["I/"])
 ("i" ["i"])
 ("i//" ["i/"])
 ("O\"\"" ["O\""])
 ("o\"\"" ["o\""])
 ("S``" ["S`"])
 ("s``" ["s`"])
 ("U\"\"" ["U\""])
 ("u\"\"" ["u\""])
 ("U^^" ["U^"])
 ("u^^" ["u^"])
 )

(quail-define-package
 "turkish-alt-postfix" "Turkish" "TR,A+(B" t
 "Turkish (T,A|(Brk,Ag(Be) input method with postfix modifiers.

This is for those who use Latin-5 (ISO-8859-9) for Turkish.  If you
use Latin-3 (ISO-8859-3), you should use
\"turkish-latin-3-alt-postfix\" instead.

Note for I, ,M}(B, ,M](B, i.

A^ -> ,MB(B
C` -> ,MG(B
G^ -> ,MP(B
I  -> I
i  -> ,M}(B
I/ -> ,M](B
i/ -> i
O\" -> ,MV(B
S` -> ,M^(B
U\" -> ,M\(B
U^ -> ,M[(B

Doubling the postfix separates the letter and postfix: e.g. a^^ -> a^
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A^" ?,MB(B)
 ("a^" ?,Mb(B)
 ("C`" ?,MG(B)
 ("c`" ?,Mg(B)
 ("G^" ?,MP(B)
 ("g^" ?,Mp(B)
 ("I/" ?,M](B)
 ("i" ?,M}(B)
 ("i/" ?i)
 ("O\"" ?,MV(B)
 ("o\"" ?,Cv(B)
 ("S`" ?,M^(B)
 ("s`" ?,M~(B)
 ("U\"" ?,M\(B)
 ("u\"" ?,M|(B)
 ("U^" ?,M[(B)
 ("u^" ?,M{(B)

 ("A^^" ["A^"])
 ("a^^" ["a^"])
 ("C``" ["C`"])
 ("c``" ["c`"])
 ("G^^" ["G^"])
 ("g^^" ["g^"])
 ("I//" ["I/"])
 ("i" ["i"])
 ("i//" ["i/"])
 ("O\"\"" ["O\""])
 ("o\"\"" ["o\""])
 ("S``" ["S`"])
 ("s``" ["s`"])
 ("U\"\"" ["U\""])
 ("u\"\"" ["u\""])
 ("U^^" ["U^"])
 ("u^^" ["u^"])
 )

;; Dutch Quail input method derived from the one in Yudit by Roman
;; Czyborra.
(quail-define-package
 "dutch" "Dutch" "NL" t
 "Dutch character mixfix input method.
Uses the `mule-unicode-0100-24ff' charset to supplement Latin-1.

             |         | examples
 ------------+---------+----------
  others     |         | fl. -> $,1!R(B  eur. -> $,1tL(B  ij -> $,1 S(B  IJ -> $,1 R(B
 ------------+---------+----------
             | postfix |
 ------------+---------+----------
  acute      |    '    | a' -> ,Aa(B
  grave      |    `    | a` -> ,A`(B
  circumflex |    ^    | a^ -> ,Ab(B
  Turkish    | various | i/ -> $,1 Q(B  s, -> $,1 (B  g^ -> $,1 ?(B   I/ -> $,1 P(B
             |         |  S, -> $,1 ~(B  G^ -> $,1 >(B
 ------------+---------+----------
             | prefix  |
 ------------+---------+----------
  diaeresis  |    \"    | \"a -> ,Ad(B

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("fl." ?$,1!R(B) ;; LATIN SMALL LETTER F WITH HOOK (florin currency symbol)
 ("eur." ?$,1tL(B) ;; EURO SIGN
 ;; $,1r|(BThe 25th letter of the Dutch alphabet.$,1r}(B
 ("ij" ?$,1 S(B) ;; LATIN SMALL LIGATURE IJ
 ("IJ" ?$,1 R(B) ;; LATIN CAPITAL LIGATURE IJ
 ;; $,1r|(BTrema on the second letter of vowel pair.$,1r}(B  Yudit uses `:', not `"'.
 ("\"a" ?,Ad(B) ;; LATIN SMALL LETTER A WITH DIAERESIS
 ("\"e" ?,Ak(B) ;; LATIN SMALL LETTER E WITH DIAERESIS
 ("\"i" ?,Ao(B) ;; LATIN SMALL LETTER I WITH DIAERESIS
 ("\"o" ?,Av(B) ;; LATIN SMALL LETTER O WITH DIAERESIS
 ("\"u" ?,A|(B) ;; LATIN SMALL LETTER U WITH DIAERESIS
 ("\"A" ?,AD(B) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
 ("\"E" ?,AK(B) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
 ("\"I" ?,AO(B) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
 ("\"O" ?,AV(B) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
 ("\"U" ?,A\(B) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
 ;; $,1r|(BAcute, marking emphasis on long vowels$,1r}(B:
 ("a'" ?,Aa(B) ;; LATIN SMALL LETTER A WITH ACUTE
 ("e'" ?,Ai(B) ;; LATIN SMALL LETTER E WITH ACUTE
 ("i'" ?,Am(B) ;; LATIN SMALL LETTER I WITH ACUTE
 ("o'" ?,As(B) ;; LATIN SMALL LETTER O WITH ACUTE
 ("u'" ?,Az(B) ;; LATIN SMALL LETTER U WITH ACUTE
 ("A'" ?,AA(B) ;; LATIN CAPITAL LETTER A WITH ACUTE
 ("E'" ?,AI(B) ;; LATIN CAPITAL LETTER E WITH ACUTE
 ("I'" ?,AM(B) ;; LATIN CAPITAL LETTER I WITH ACUTE
 ("O'" ?,AS(B) ;; LATIN CAPITAL LETTER O WITH ACUTE
 ("U'" ?,AZ(B) ;; LATIN CAPITAL LETTER U WITH ACUTE
 ;; $,1r|(BGrave, marking emphasis on short vowels$,1r}(B:
 ("a`" ?,A`(B) ;; LATIN SMALL LETTER A WITH GRAVE
 ("e`" ?,Ah(B) ;; LATIN SMALL LETTER E WITH GRAVE
 ("i`" ?,Al(B) ;; LATIN SMALL LETTER I WITH GRAVE
 ("o`" ?,Ar(B) ;; LATIN SMALL LETTER O WITH GRAVE
 ("u`" ?,Ay(B) ;; LATIN SMALL LETTER U WITH GRAVE
 ("A`" ?,A@(B) ;; LATIN CAPITAL LETTER A WITH GRAVE
 ("E`" ?,AH(B) ;; LATIN CAPITAL LETTER E WITH GRAVE
 ("I`" ?,AL(B) ;; LATIN CAPITAL LETTER I WITH GRAVE
 ("O`" ?,AR(B) ;; LATIN CAPITAL LETTER O WITH GRAVE
 ("U`" ?,AY(B) ;; LATIN CAPITAL LETTER U WITH GRAVE
 ;; $,1r|(BCater for the use of many French words and use of the circumflex
 ;; in Frisian.$,1r}(B  Yudit used `;' for cedilla.
 ("c," ?,Ag(B) ;; LATIN SMALL LETTER C WITH CEDILLA
 ("C," ?,AG(B) ;; LATIN CAPITAL LETTER C WITH CEDILLA
 ("a^" ?,Ab(B) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
 ("e^" ?,Aj(B) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
 ("i^" ?,An(B) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
 ("o^" ?,At(B) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
 ("u^" ?,A{(B) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
 ("A^" ?,AB(B) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
 ("E^" ?,AJ(B) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
 ("I^" ?,AN(B) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
 ("O^" ?,AT(B) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
 ("U^" ?,A[(B) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
 ;; $,1r|(BFollow the example of the Dutch POSIX locale, using ISO-8859-9 to
 ;; cater to the many Turks in Dutch society.$,1r}(B  Perhaps German methods
 ;; should do so too.  Follow turkish-alt-postfix here.
 ("i/" ?$,1 Q(B) ;; LATIN SMALL LETTER I WITH NO DOT
 ("s," ?$,1 (B) ;; LATIN SMALL LETTER S WITH CEDILLA
 ("g^" ?$,1 ?(B) ;; LATIN SMALL LETTER G WITH BREVE
 ("I/" ?$,1 P(B) ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
 ("S," ?$,1 ~(B) ;; LATIN CAPITAL LETTER S WITH CEDILLA
 ("G^" ?$,1 >(B) ;; LATIN CAPITAL LETTER G WITH BREVE
 )

;; Originally from Yudit, discussed with Albertas Agejevas
;; <alga@uosis.mif.vu.lt>
(quail-define-package
 "lithuanian-numeric" "Lithuanian" "LtN" t
 "Lithuanian numeric input method.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?$,1 %(B)
 ("2" ?$,1 -(B)
 ("3" ?$,1 9(B)
 ("4" ?$,1 7(B)
 ("5" ?$,1 O(B)
 ("6" ?$,1!!(B)
 ("7" ?$,1!3(B)
 ("8" ?$,1!+(B)
 ("9" ?$,1r~(B)
 ("0" ?$,1r|(B)
 ("=" ?$,1!>(B)
 ("!" ?$,1 $(B)
 ("@" ?$,1 ,(B)
 ("#" ?$,1 8(B)
 ("$" ?$,1 6(B)
 ("%" ?$,1 N(B)
 ("^" ?$,1! (B)
 ("&" ?$,1!2(B)
 ("*" ?$,1!*(B)
 ("+" ?$,1!=(B))

;; From XFree 4.1 /usr/X11R6/lib/X11/xkb/symbols/lt, suggested by
;; Albertas Agejevas <alga@uosis.mif.vu.lt>
(quail-define-package
 "lithuanian-keyboard" "Lithuanian" "Lt" t
 "Lithuanian standard keyboard input method.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?$,1 %(B)
 ("!" ?$,1 $(B)
 ("2" ?$,1 -(B)
 ("@" ?$,1 ,(B)
 ("#" ?$,1 8(B)
 ("4" ?$,1 7(B)
 ("$" ?$,1 6(B)
 ("5" ?$,1 O(B)
 ("%" ?$,1 N(B)
 ("6" ?$,1!!(B)
 ("^" ?$,1! (B)
 ("7" ?$,1!3(B)
 ("&" ?$,1!2(B)
 ("9" ?$,1r~(B)
 ("0" ?$,1r|(B)
 ("=" ?$,1!>(B)
 ("+" ?$,1!=(B))

;; From XFree 4.1 /usr/X11R6/lib/X11/xkb/symbols/lv
(quail-define-package
 "latvian-keyboard" "Latvian" "Lv" t
 "Latvian standard keyboard input method.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("4" ?$,1tL(B)
 ("$" ?,A"(B)
 ("e" ?$,1 3(B)
 ("E" ?$,1 2(B)
 ("r" ?$,1 w(B)
 ("R" ?$,1 v(B)
 ("u" ?$,1!+(B)
 ("U" ?$,1!*(B)
 ("i" ?$,1 K(B)
 ("I" ?$,1 J(B)
 ("o" ?$,1 m(B)
 ("O" ?$,1 l(B)
 ("a" ?$,1 !(B)
 ("A" ?$,1  (B)
 ("s" ?$,1!!(B)
 ("S" ?$,1! (B)
 ("g" ?$,1 C(B)
 ("G" ?$,1 B(B)
 ("k" ?$,1 W(B)
 ("K" ?$,1 V(B)
 ("l" ?$,1 \(B)
 ("L" ?$,1 [(B)
 ("\'" ?$,1r|(B)
 ("\"" ?$,1r~(B)
 ("z" ?$,1!>(B)
 ("Z" ?$,1!=(B)
 ("c" ?$,1 -(B)
 ("C" ?$,1 ,(B)
 ("n" ?$,1 f(B)
 ("N" ?$,1 e(B))

(quail-define-package
 "latin-alt-postfix" "Latin" "L<" t
 "Latin character input method with postfix modifiers.
This is the union of various input methods originally made for input
of characters from a single Latin-N charset.

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Aa(B
  grave      |    `    | a` -> ,A`(B
  circumflex |    ^    | a^ -> ,Ab(B
  diaeresis  |    \"    | a\" -> ,Ad(B
  tilde      |    ~    | a~ -> ,Ac(B
  cedilla    |    /`   | c/ -> ,Ag(B   c` -> ,Ag(B
  ogonek     |    `    | a` -> $,1 %(B
  breve      |    ~    | a~ -> $,1 #(B
  caron      |    ~    | c~ -> $,1 -(B
  dbl. acute |    :    | o: -> $,1 q(B
  ring       |    `    | u` -> $,1!/(B
  dot        |    `    | z` -> $,1!<(B
  stroke     |    /    | d/ -> $,1 1(B
  nordic     |    /    | d/ -> ,Ap(B   t/ -> ,A~(B   a/ -> ,Ae(B   e/ -> ,Af(B   o/ -> ,Ax(B
  others     |   /<>   | s/ -> ,A_(B   ?/ -> ,A?(B   !/ -> ,A!(B
             | various | << -> ,A+(B   >> -> ,A;(B   o_ -> ,A:(B   a_ -> ,A*(B

It would be natural to use comma for cedillas, but that would be
inconvenient in practice because commas are needed very often after a
letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

;; Fixme: ,A&(B ,A'(B ,A((B ,A)(B ,A,(B ,A-(B ,A.(B ,A/(B ,A0(B ,A1(B ,A2(B ,A3(B ,A4(B ,A5(B ,A6(B ,A7(B ,A8(B ,A9(B ,A<(B ,A=(B ,A>(B ,AW(B ,Aw(B
(quail-define-rules
 (" _" ?,A (B)
 ("!/" ?,A!(B)
 ("//" ?,A0(B)
 ("<<" ?,A+(B)
 (">>" ?,A;(B)
 ("?/" ?,A?(B)
 ("$/" ?,A#(B)
 ("$/" ?,A$(B)
 ("A'" ?,AA(B)
 ("A-" ?$,1  (B)
 ("A/" ?,AE(B)
 ("A\"" ?,AD(B)
 ("A^" ?,AB(B)
 ("A`" ?,A@(B)
 ("A`" ?$,1 $(B)
 ("A~" ?,AC(B)
 ("A~" ?$,1 "(B)
 ("C'" ?$,1 &(B)
 ("C/" ?,AG(B)
 ("C/" ?$,1 *(B)
 ("C^" ?$,1 ((B)
 ("C`" ?,AG(B)
 ("C~" ?$,1 ,(B)
 ("D/" ?,AP(B)
 ("D/" ?$,1 0(B)
 ("D~" ?$,1 .(B)
 ("E'" ?,AI(B)
 ("E-" ?$,1 2(B)
 ("E/" ?,AF(B)
 ("E\"" ?,AK(B)
 ("E^" ?,AJ(B)
 ("E`" ?,AH(B)
 ("E`" ?$,1 8(B)
 ("E~" ?$,1 6(B)
 ("E~" ?$,1 :(B)
 ("G/" ?$,1 @(B)
 ("G^" ?$,1 <(B)
 ("G`" ?$,1 B(B)
 ("G~" ?$,1 >(B)
 ("H/" ?$,1 F(B)
 ("H^" ?$,1 D(B)
 ("I'" ?,AM(B)
 ("I-" ?$,1 J(B)
 ("I/" ?$,1 P(B)
 ("I\"" ?,AO(B)
 ("I^" ?,AN(B)
 ("I`" ?,AL(B)
 ("I`" ?$,1 N(B)
 ("I~" ?$,1 H(B)
 ("J^" ?$,1 T(B)
 ("K`" ?$,1 V(B)
 ("L'" ?$,1 Y(B)
 ("L/" ?$,1 a(B)
 ("L`" ?$,1 [(B)
 ("L~" ?$,1 ](B)
 ("N'" ?$,1 c(B)
 ("N/" ?$,1 j(B)
 ("N`" ?$,1 e(B)
 ("N~" ?,AQ(B)
 ("N~" ?$,1 g(B)
 ("O'" ?,AS(B)
 ("O-" ?$,1 l(B)
 ("O/" ?,AX(B)
 ("O:" ?$,1 p(B)
 ("O\"" ?,AV(B)
 ("O^" ?,AT(B)
 ("O`" ?,AR(B)
 ("O~" ?,AU(B)
 ("R'" ?$,1 t(B)
 ("R`" ?$,1 v(B)
 ("R~" ?$,1 x(B)
 ("S'" ?$,1 z(B)
 ("S^" ?$,1 |(B)
 ("S`" ?$,1 ~(B)
 ("S~" ?$,1! (B)
 ("T/" ?,A^(B)
 ("T/" ?$,1!&(B)
 ("T`" ?$,1!"(B)
 ("T~" ?$,1!$(B)
 ("U'" ?,AZ(B)
 ("U-" ?$,1!*(B)
 ("U:" ?$,1!0(B)
 ("U\"" ?,A\(B)
 ("U^" ?,A[(B)
 ("U`" ?,AY(B)
 ("U`" ?$,1!.(B)
 ("U`" ?$,1!2(B)
 ("U~" ?$,1!((B)
 ("U~" ?$,1!,(B)
 ("Y'" ?,A](B)
 ("Y\"" ?$,1!8(B)
 ("Y=" ?,A%(B)
 ("Z'" ?$,1!9(B)
 ("Z/" ?$,1!;(B)
 ("Z`" ?$,1!;(B)
 ("Z~" ?$,1!=(B)
 ("a'" ?,Aa(B)
 ("a-" ?$,1 !(B)
 ("a/" ?,Ae(B)
 ("a\"" ?,Ad(B)
 ("a^" ?,Ab(B)
 ("a_" ?,A*(B)
 ("a`" ?,A`(B)
 ("a`" ?$,1 %(B)
 ("a~" ?,Ac(B)
 ("a~" ?$,1 #(B)
 ("c'" ?$,1 '(B)
 ("c/" ?,Ag(B)
 ("c/" ?$,1 +(B)
 ("c/" ?,A"(B)
 ("c^" ?$,1 )(B)
 ("c`" ?,Ag(B)
 ("c~" ?$,1 -(B)
 ("d/" ?,Ap(B)
 ("d/" ?$,1 1(B)
 ("d~" ?$,1 /(B)
 ("e'" ?,Ai(B)
 ("e-" ?$,1 3(B)
 ("e/" ?,Af(B)
 ("e\"" ?,Ak(B)
 ("e^" ?,Aj(B)
 ("e`" ?,Ah(B)
 ("e`" ?$,1 9(B)
 ("e~" ?$,1 7(B)
 ("e~" ?$,1 ;(B)
 ("e=" ?$,1tL(B)
 ("g/" ?$,1 A(B)
 ("g^" ?$,1 =(B)
 ("g`" ?$,1 C(B)
 ("g~" ?$,1 ?(B)
 ("h/" ?$,1 G(B)
 ("h^" ?$,1 E(B)
 ("i'" ?,Am(B)
 ("i-" ?$,1 K(B)
 ("i/" ?$,1 Q(B)
 ("i\"" ?,Ao(B)
 ("i^" ?,An(B)
 ("i`" ?,Al(B)
 ("i`" ?$,1 O(B)
 ("i~" ?$,1 I(B)
 ("j^" ?$,1 U(B)
 ("k/" ?$,1 X(B)
 ("k`" ?$,1 W(B)
 ("l'" ?$,1 Z(B)
 ("l/" ?$,1 b(B)
 ("l`" ?$,1 \(B)
 ("l~" ?$,1 ^(B)
 ("n'" ?$,1 d(B)
 ("n/" ?$,1 k(B)
 ("n`" ?$,1 f(B)
 ("n~" ?,Aq(B)
 ("n~" ?$,1 h(B)
 ("o'" ?,As(B)
 ("o-" ?$,1 m(B)
 ("o/" ?,Ax(B)
 ("o:" ?$,1 q(B)
 ("o\"" ?,Av(B)
 ("o^" ?,At(B)
 ("o_" ?,A:(B)
 ("o`" ?,Ar(B)
 ("o~" ?,Au(B)
 ("r'" ?$,1 u(B)
 ("r`" ?$,1 w(B)
 ("r~" ?$,1 y(B)
 ("s'" ?$,1 {(B)
 ("s/" ?,A_(B)
 ("s^" ?$,1 }(B)
 ("s`" ?$,1 (B)
 ("s~" ?$,1!!(B)
 ("t/" ?,A~(B)
 ("t/" ?$,1!'(B)
 ("t`" ?$,1!#(B)
 ("t~" ?$,1!%(B)
 ("u'" ?,Az(B)
 ("u-" ?$,1!+(B)
 ("u:" ?$,1!1(B)
 ("u\"" ?,A|(B)
 ("u^" ?,A{(B)
 ("u`" ?,Ay(B)
 ("u`" ?$,1!/(B)
 ("u`" ?$,1!3(B)
 ("u~" ?$,1!)(B)
 ("u~" ?$,1!-(B)
 ("y'" ?,A}(B)
 ("y\"" ?,A(B)
 ("z'" ?$,1!:(B)
 ("z/" ?$,1!<(B)
 ("z`" ?$,1!<(B)
 ("z~" ?$,1!>(B)

 (" __" [" _"])
 ("!//" ["!/"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("?//" ["?/"])
 ("///" ["//"])
 ("$//" ["$/"])
 ("A''" ["A'"])
 ("A--" ["A-"])
 ("A//" ["A/"])
 ("A\"\"" ["A\""])
 ("A^^" ["A^"])
 ("A``" ["A`"])
 ("A~~" ["A~"])
 ("C''" ["C'"])
 ("C//" ["C/"])
 ("C^^" ["C^"])
 ("C``" ["C`"])
 ("C~~" ["C~"])
 ("D//" ["D/"])
 ("D~~" ["D~"])
 ("E''" ["E'"])
 ("E--" ["E-"])
 ("E//" ["E/"])
 ("E\"\"" ["E\""])
 ("E^^" ["E^"])
 ("E``" ["E`"])
 ("E~~" ["E~"])
 ("G//" ["G/"])
 ("G^^" ["G^"])
 ("G``" ["G`"])
 ("G~~" ["G~"])
 ("H//" ["H/"])
 ("H^^" ["H^"])
 ("I''" ["I'"])
 ("I--" ["I-"])
 ("I//" ["I/"])
 ("I\"\"" ["I\""])
 ("I^^" ["I^"])
 ("I``" ["I`"])
 ("I~~" ["I~"])
 ("J^^" ["J^"])
 ("K``" ["K`"])
 ("L''" ["L'"])
 ("L//" ["L/"])
 ("L``" ["L`"])
 ("L~~" ["L~"])
 ("N''" ["N'"])
 ("N//" ["N/"])
 ("N``" ["N`"])
 ("N~~" ["N~"])
 ("O''" ["O'"])
 ("O--" ["O-"])
 ("O//" ["O/"])
 ("O::" ["O:"])
 ("O\"\"" ["O\""])
 ("O^^" ["O^"])
 ("O``" ["O`"])
 ("O~~" ["O~"])
 ("R''" ["R'"])
 ("R``" ["R`"])
 ("R~~" ["R~"])
 ("S''" ["S'"])
 ("S^^" ["S^"])
 ("S``" ["S`"])
 ("S~~" ["S~"])
 ("T//" ["T/"])
 ("T``" ["T`"])
 ("T~~" ["T~"])
 ("U''" ["U'"])
 ("U--" ["U-"])
 ("U::" ["U:"])
 ("U\"\"" ["U\""])
 ("U^^" ["U^"])
 ("U``" ["U`"])
 ("U~~" ["U~"])
 ("Y''" ["Y'"])
 ("Z''" ["Z'"])
 ("Z//" ["Z/"])
 ("Z``" ["Z`"])
 ("Z~~" ["Z~"])
 ("a''" ["a'"])
 ("a--" ["a-"])
 ("a//" ["a/"])
 ("a\"\"" ["a\""])
 ("a^^" ["a^"])
 ("a__" ["a_"])
 ("a``" ["a`"])
 ("a~~" ["a~"])
 ("c''" ["c'"])
 ("c//" ["c/"])
 ("c^^" ["c^"])
 ("c``" ["c`"])
 ("c~~" ["c~"])
 ("d//" ["d/"])
 ("d~~" ["d~"])
 ("e''" ["e'"])
 ("e--" ["e-"])
 ("e//" ["e/"])
 ("e\"\"" ["e\""])
 ("e^^" ["e^"])
 ("e``" ["e`"])
 ("e~~" ["e~"])
 ("e==" ["e="])
 ("g//" ["g/"])
 ("g^^" ["g^"])
 ("g``" ["g`"])
 ("g~~" ["g~"])
 ("h//" ["h/"])
 ("h^^" ["h^"])
 ("i''" ["i'"])
 ("i--" ["i-"])
 ("i//" ["i/"])
 ("i\"\"" ["i\""])
 ("i^^" ["i^"])
 ("i``" ["i`"])
 ("i~~" ["i~"])
 ("j^^" ["j^"])
 ("k//" ["k/"])
 ("k``" ["k`"])
 ("l''" ["l'"])
 ("l//" ["l/"])
 ("l``" ["l`"])
 ("l~~" ["l~"])
 ("n''" ["n'"])
 ("n//" ["n/"])
 ("n``" ["n`"])
 ("n~~" ["n~"])
 ("o''" ["o'"])
 ("o--" ["o-"])
 ("o//" ["o/"])
 ("o::" ["o:"])
 ("o\"\"" ["o\""])
 ("o^^" ["o^"])
 ("o__" ["o_"])
 ("o``" ["o`"])
 ("o~~" ["o~"])
 ("r''" ["r'"])
 ("r``" ["r`"])
 ("r~~" ["r~"])
 ("s''" ["s'"])
 ("s//" ["s/"])
 ("s^^" ["s^"])
 ("s``" ["s`"])
 ("s~~" ["s~"])
 ("t//" ["t/"])
 ("t``" ["t`"])
 ("t~~" ["t~"])
 ("u''" ["u'"])
 ("u--" ["u-"])
 ("u::" ["u:"])
 ("u\"\"" ["u\""])
 ("u^^" ["u^"])
 ("u``" ["u`"])
 ("u~~" ["u~"])
 ("y''" ["y'"])
 ("y\"\"" ["y\""])
 ("z''" ["z'"])
 ("z//" ["z/"])
 ("z``" ["z`"])
 ("z~~" ["z~"])
 )

;;; arch-tag: 722466a6-363d-431c-9161-879e16e2da5d
;;; latin-alt.el ends here
