;;; latin-post.el --- Quail packages for inputting various European characters  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

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
 "latin-1-postfix" "Latin-1" "1<" t
 "Latin-1 character input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Aa(B
  grave      |    `    | a` -> ,A`(B
  circumflex |    ^    | a^ -> ,Ab(B
  diaeresis  |    \"    | a\" -> ,Ad(B
  tilde      |    ~    | a~ -> ,Ac(B
  cedilla    |    ,    | c, -> ,Ag(B
  nordic     |    /    | d/ -> ,Ap(B   t/ -> ,A~(B   a/ -> ,Ae(B   e/ -> ,Af(B   o/ -> ,Ax(B
  others     |    /    | s/ -> ,A_(B   ?/ -> ,A?(B   !/ -> ,A!(B   // -> ,A0(B
             | various | << -> ,A+(B   >> -> ,A;(B   o_ -> ,A:(B   a_ -> ,A*(B

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
 ("C," ?,AG(B)
 ("c," ?,Ag(B)
 ("N~" ?,AQ(B)
 ("n~" ?,Aq(B)
 ("?/" ?,A?(B)
 ("!/" ?,A!(B)
 ("<<" ?,A+(B)
 (">>" ?,A;(B)
 ("o_" ?,A:(B)
 ("a_" ?,A*(B)
 ("//" ?,A0(B)

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
 ("C,," ["C,"])
 ("c,," ["c,"])
 ("N~~" ["N~"])
 ("n~~" ["n~"])
 ("?//" ["?/"])
 ("!//" ["!/"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("o__" ["o_"])
 ("a__" ["a_"])
 ("///" ["//"])
 )

(quail-define-package
 "latin-2-postfix" "Latin-2" "2<" t
 "Latin-2 character input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Ba(B
  ogonek     |    ,    | a, -> ,B1(B
  diaeresis  |    \"    | a\" -> ,Bd(B
  circumflex |    ^    | a^ -> ,Bb(B
  breve      |    ~    | a~ -> ,Bc(B
  cedilla    |    ,    | c, -> ,Bg(B
  caron      |    ~    | c~ -> ,Bh(B
  dbl. acute |    :    | o: -> ,Bu(B
  ring       |    .    | u. -> ,By(B
  dot        |    .    | z. -> ,B?(B
  stroke     |    /    | d/ -> ,Bp(B
  others     |    /    | s/ -> ,B_(B

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?,BA(B)
 ("A," ?,B!(B)
 ("A\"" ?,BD(B)
 ("A^" ?,BB(B)
 ("A~" ?,BC(B)
 ("C'" ?,BF(B)
 ("C," ?,BG(B)
 ("C~" ?,BH(B)
 ("D/" ?,BP(B)
 ("D~" ?,BO(B)
 ("E'" ?,BI(B)
 ("E," ?,BJ(B)
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
 ("S," ?,B*(B)
 ("S~" ?,B)(B)
 ("T," ?,B^(B)
 ("T~" ?,B+(B)
 ("U'" ?,BZ(B)
 ("U:" ?,B[(B)
 ("U\"" ?,B\(B)
 ("U." ?,BY(B)
 ("Y'" ?,B](B)
 ("Z'" ?,B,(B)
 ("Z." ?,B/(B)
 ("Z~" ?,B.(B)
 ("a'" ?,Ba(B)
 ("a," ?,B1(B)
 ("a\"" ?,Bd(B)
 ("a^" ?,Bb(B)
 ("a~" ?,Bc(B)
 ("c'" ?,Bf(B)
 ("c," ?,Bg(B)
 ("c~" ?,Bh(B)
 ("d/" ?,Bp(B)
 ("d~" ?,Bo(B)
 ("e'" ?,Bi(B)
 ("e," ?,Bj(B)
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
 ("s," ?,B:(B)
 ("s/" ?,B_(B)
 ("s~" ?,B9(B)
 ("t," ?,B~(B)
 ("t~" ?,B;(B)
 ("u'" ?,Bz(B)
 ("u:" ?,B{(B)
 ("u\"" ?,B|(B)
 ("u." ?,By(B)
 ("y'" ?,B}(B)
 ("z'" ?,B<(B)
 ("z." ?,B?(B)
 ("z~" ?,B>(B)

 ("A''" ["A'"])
 ("A,," ["A,"])
 ("A\"\"" ["A\""])
 ("A^^" ["A^"])
 ("A~~" ["A~"])
 ("C''" ["C'"])
 ("C,," ["C,"])
 ("C~~" ["C~"])
 ("D//" ["D/"])
 ("D~~" ["D~"])
 ("E''" ["E'"])
 ("E,," ["E,"])
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
 ("S,," ["S,"])
 ("S~~" ["S~"])
 ("T,," ["T,"])
 ("T~~" ["T~"])
 ("U''" ["U'"])
 ("U::" ["U:"])
 ("U\"\"" ["U\""])
 ("U.." ["U."])
 ("Y''" ["Y'"])
 ("Z''" ["Z'"])
 ("Z.." ["Z."])
 ("Z~~" ["Z~"])
 ("a''" ["a'"])
 ("a,," ["a,"])
 ("a\"\"" ["a\""])
 ("a^^" ["a^"])
 ("a~~" ["a~"])
 ("c''" ["c'"])
 ("c,," ["c,"])
 ("c~~" ["c~"])
 ("d//" ["d/"])
 ("d~~" ["d~"])
 ("e''" ["e'"])
 ("e,," ["e,"])
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
 ("s,," ["s,"])
 ("s//" ["s/"])
 ("s~~" ["s~"])
 ("t,," ["t,"])
 ("t~~" ["t~"])
 ("u''" ["u'"])
 ("u::" ["u:"])
 ("u\"\"" ["u\""])
 ("u.." ["u."])
 ("y''" ["y'"])
 ("z''" ["z'"])
 ("z.." ["z."])
 ("z~~" ["z~"])
 )

(quail-define-package
 "latin-3-postfix" "Latin-3" "3<" t
 "Latin-3 character input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Ca(B
  grave      |    `    | a` -> ,C`(B
  circumflex |    ^    | a^ -> ,Cb(B
  diaeresis  |    \"    | a\" -> ,Cd(B
  dot        |    .    | c. -> ,Ce(B   i. -> ,C9(B   I. -> ,C)(B
  cedilla    |    ,    | c, -> ,Cg(B
  breve      |    ~    | g~ -> ,C;(B
  tilde      |    ~    | n~ -> ,Cq(B
  stroke     |    /    | h/ -> ,C1(B
  others     |    /    | s/ -> ,C_(B

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?,C@(B)
 ("A'" ?,CA(B)
 ("A^" ?,CB(B)
 ("A\"" ?,CD(B)
 ("C." ?,CE(B)
 ("C^" ?,CF(B)
 ("C," ?,CG(B)
 ("E`" ?,CH(B)
 ("E'" ?,CI(B)
 ("E^" ?,CJ(B)
 ("E\"" ?,CK(B)
 ("G~" ?,C+(B)
 ("G." ?,CU(B)
 ("G^" ?,CX(B)
 ("H/" ?,C!(B)
 ("H^" ?,C&(B)
 ("I." ?,C)(B)
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
 ("S," ?,C*(B)
 ("S^" ?,C^(B)
 ("U`" ?,CY(B)
 ("U'" ?,CZ(B)
 ("U^" ?,C[(B)
 ("U\"" ?,C\(B)
 ("U~" ?,C](B)
 ("Z." ?,C/(B)
 ("a`" ?,C`(B)
 ("a'" ?,Ca(B)
 ("a^" ?,Cb(B)
 ("a\"" ?,Cd(B)
 ("c." ?,Ce(B)
 ("c^" ?,Cf(B)
 ("c," ?,Cg(B)
 ("e`" ?,Ch(B)
 ("e'" ?,Ci(B)
 ("e^" ?,Cj(B)
 ("e\"" ?,Ck(B)
 ("g~" ?,C;(B)
 ("g." ?,Cu(B)
 ("g^" ?,Cx(B)
 ("h/" ?,C1(B)
 ("h^" ?,C6(B)
 ("i." ?,C9(B)
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
 ("s," ?,C:(B)
 ("s/" ?,C_(B)
 ("s^" ?,C~(B)
 ("u`" ?,Cy(B)
 ("u'" ?,Cz(B)
 ("u^" ?,C{(B)
 ("u\"" ?,C|(B)
 ("u~" ?,C}(B)
 ("z." ?,C?(B)

 ("A``" ["A`"])
 ("A''" ["A'"])
 ("A^^" ["A^"])
 ("A\"\"" ["A\""])
 ("C.." ["C."])
 ("C^^" ["C^"])
 ("C,," ["C,"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("E^^" ["E^"])
 ("E\"\"" ["E\""])
 ("G~~" ["G~"])
 ("G.." ["G."])
 ("G^^" ["G^"])
 ("H//" ["H/"])
 ("H^^" ["H^"])
 ("I.." ["I."])
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
 ("S,," ["S,"])
 ("S^^" ["S^"])
 ("U``" ["U`"])
 ("U''" ["U'"])
 ("U^^" ["U^"])
 ("U\"\"" ["U\""])
 ("U~~" ["U~"])
 ("Z.." ["Z."])
 ("a``" ["a`"])
 ("a''" ["a'"])
 ("a^^" ["a^"])
 ("a\"\"" ["a\""])
 ("c.." ["c."])
 ("c^^" ["c^"])
 ("c,," ["c,"])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("e^^" ["e^"])
 ("e\"\"" ["e\""])
 ("g~~" ["g~"])
 ("g.." ["g."])
 ("g^^" ["g^"])
 ("h//" ["h/"])
 ("h^^" ["h^"])
 ("i.." ["i."])
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
 ("s,," ["s,"])
 ("s//" ["s/"])
 ("s^^" ["s^"])
 ("u``" ["u`"])
 ("u''" ["u'"])
 ("u^^" ["u^"])
 ("u\"\"" ["u\""])
 ("u~~" ["u~"])
 ("z.." ["z."])
 )

(quail-define-package
 "latin-4-postfix" "Latin-4" "4<" t
 "Latin-4 characters input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Da(B
  circumflex |    ^    | a^ -> ,Db(B
  diaeresis  |    \"    | a\" -> ,Dd(B
  ogonek     |    ,    | a, -> ,D1(B
  macron     |    -    | a- -> ,D`(B
  tilde      |    ~    | a~ -> ,Dc(B
  caron      |    ~    | c~ -> ,Dh(B
  dot        |    .    | e. -> ,Dl(B
  cedilla    |    ,    | k, -> ,Ds(B   g, -> ,D;(B
  stroke     |    /    | d/ -> ,Dp(B
  nordic     |    /    | a/ -> ,De(B   e/ -> ,Df(B   o/ -> ,Dx(B
  others     |    /    | s/ -> ,D_(B   n/ -> ,D?(B   k/ -> ,D"(B

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A," ?,D!(B)
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
 ("E," ?,DJ(B)
 ("E\"" ?,DK(B)
 ("E." ?,DL(B)
 ("G," ?,D+(B)
 ("I~" ?,D%(B)
 ("I," ?,DG(B)
 ("I'" ?,DM(B)
 ("I^" ?,DN(B)
 ("I-" ?,DO(B)
 ("K," ?,DS(B)
 ("L," ?,D&(B)
 ("N/" ?,D=(B)
 ("N," ?,DQ(B)
 ("O-" ?,DR(B)
 ("O^" ?,DT(B)
 ("O~" ?,DU(B)
 ("O\"" ?,DV(B)
 ("O/" ?,DX(B)
 ("R," ?,D#(B)
 ("S~" ?,D)(B)
 ("T/" ?,D,(B)
 ("U," ?,DY(B)
 ("U'" ?,DZ(B)
 ("U^" ?,D[(B)
 ("U\"" ?,D\(B)
 ("U~" ?,D](B)
 ("U-" ?,D^(B)
 ("Z~" ?,D.(B)
 ("a," ?,D1(B)
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
 ("e," ?,Dj(B)
 ("e\"" ?,Dk(B)
 ("e." ?,Dl(B)
 ("g," ?,D;(B)
 ("i~" ?,D5(B)
 ("i," ?,Dg(B)
 ("i'" ?,Dm(B)
 ("i^" ?,Dn(B)
 ("i-" ?,Do(B)
 ("k/" ?,D"(B)
 ("k," ?,Ds(B)
 ("l," ?,D6(B)
 ("n/" ?,D?(B)
 ("n," ?,Dq(B)
 ("o-" ?,Dr(B)
 ("o^" ?,Dt(B)
 ("o~" ?,Du(B)
 ("o\"" ?,Dv(B)
 ("o/" ?,Dx(B)
 ("r," ?,D3(B)
 ("s/" ?,D_(B)
 ("s~" ?,D9(B)
 ("t/" ?,D<(B)
 ("u," ?,Dy(B)
 ("u'" ?,Dz(B)
 ("u^" ?,D{(B)
 ("u\"" ?,D|(B)
 ("u~" ?,D}(B)
 ("u-" ?,D~(B)
 ("z~" ?,D>(B)

 ("A,," ["A,"])
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
 ("E,," ["E,"])
 ("E\"\"" ["E\""])
 ("E.." ["E."])
 ("G,," ["G,"])
 ("I~~" ["I~"])
 ("I,," ["I,"])
 ("I''" ["I'"])
 ("I^^" ["I^"])
 ("I--" ["I-"])
 ("K,," ["K,"])
 ("L,," ["L,"])
 ("N//" ["N/"])
 ("N,," ["N,"])
 ("O--" ["O-"])
 ("O^^" ["O^"])
 ("O~~" ["O~"])
 ("O\"\"" ["O\""])
 ("O//" ["O/"])
 ("R,," ["R,"])
 ("S~~" ["S~"])
 ("T//" ["T/"])
 ("U,," ["U,"])
 ("U''" ["U'"])
 ("U^^" ["U^"])
 ("U\"\"" ["U\""])
 ("U~~" ["U~"])
 ("U--" ["U-"])
 ("Z~~" ["Z~"])
 ("a,," ["a,"])
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
 ("e,," ["e,"])
 ("e\"\"" ["e\""])
 ("e.." ["e."])
 ("g,," ["g,"])
 ("i~~" ["i~"])
 ("i,," ["i,"])
 ("i''" ["i'"])
 ("i^^" ["i^"])
 ("i--" ["i-"])
 ("k//" ["k/"])
 ("k,," ["k,"])
 ("l,," ["l,"])
 ("n//" ["n/"])
 ("n,," ["n,"])
 ("o--" ["o-"])
 ("o^^" ["o^"])
 ("o~~" ["o~"])
 ("o\"\"" ["o\""])
 ("o//" ["o/"])
 ("r,," ["r,"])
 ("s//" ["s/"])
 ("s~~" ["s~"])
 ("t//" ["t/"])
 ("u,," ["u,"])
 ("u''" ["u'"])
 ("u^^" ["u^"])
 ("u\"\"" ["u\""])
 ("u~~" ["u~"])
 ("u--" ["u-"])
 ("z~~" ["z~"])
 )

(quail-define-package
 "latin-5-postfix" "Latin-5" "5<" t
 "Latin-5 characters input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> ,Ma(B
  grave      |    `    | a` -> ,M`(B
  circumflex |    ^    | a^ -> ,Mb(B
  diaeresis  |    \"    | a\" -> ,Md(B
  tilde      |    ~    | a~ -> ,Mc(B
  breve      |    ~    | g~ -> ,Mp(B
  cedilla    |    ,    | c, -> ,Mg(B
  dot        |    .    | i. -> ,M}(B   I. -> ,M](B
  nordic     |    /    | a/ -> ,Me(B   e/ -> ,Mf(B   o/ -> ,Mx(B
  others     |    /    | s/ -> ,M_(B

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?,MA(B)
 ("A/" ?,ME(B)
 ("A\"" ?,MD(B)
 ("A^" ?,MB(B)
 ("A`" ?,M@(B)
 ("A~" ?,MC(B)
 ("C," ?,MG(B)
 ("E'" ?,MI(B)
 ("E/" ?,MF(B)
 ("E\"" ?,MK(B)
 ("E^" ?,MJ(B)
 ("E`" ?,MH(B)
 ("G~" ?,MP(B)
 ("I'" ?,MM(B)
 ("I." ?,M](B)
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
 ("S," ?,M^(B)
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
 ("c," ?,Mg(B)
 ("e'" ?,Mi(B)
 ("e/" ?,Mf(B)
 ("e\"" ?,Mk(B)
 ("e^" ?,Mj(B)
 ("e`" ?,Mh(B)
 ("g~" ?,Mp(B)
 ("i'" ?,Mm(B)
 ("i." ?,M}(B)
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
 ("s," ?,M~(B)
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
 ("C,," ["C,"])
 ("E''" ["E'"])
 ("E//" ["E/"])
 ("E\"\"" ["E\""])
 ("E^^" ["E^"])
 ("E``" ["E`"])
 ("G~~" ["G~"])
 ("I''" ["I'"])
 ("I.." ["I."])
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
 ("S,," ["S,"])
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
 ("c,," ["c,"])
 ("e''" ["e'"])
 ("e//" ["e/"])
 ("e\"\"" ["e\""])
 ("e^^" ["e^"])
 ("e``" ["e`"])
 ("g~~" ["g~"])
 ("i''" ["i'"])
 ("i.." ["i."])
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
 ("s,," ["s,"])
 ("s//" ["s/"])
 ("u''" ["u'"])
 ("u\"\"" ["u\""])
 ("u^^" ["u^"])
 ("u``" ["u`"])
 ("y\"\"" ["y\""])
 )

(quail-define-package
 "danish-postfix" "Latin-1" "DA<" t
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
 "esperanto-postfix" "Latin-3" "EO<" t
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
 "finnish-postfix" "Latin-1" "FI<" t
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
 "french-postfix" "French" "FR<" t
 "French (Fran,Ag(Bais) input method with postfix modifiers

` pour grave, ' pour aigu, ^ pour circonflexe, et \" pour tr,Ai(Bma.
Par exemple: a` -> ,A`(B   e' -> ,Ai(B.

,AG(B, ,A+(B, et ,A;(B sont produits par C,, <<, et >>.

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
 ("C," ?,AG(B)
 ("c," ?,Ag(B)
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
 ("C,," ["C,"])
 ("c,," ["c,"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 )

(quail-define-package
 "german-postfix" "German" "DE<" t
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
 "icelandic-postfix" "Latin-1" "IS<" t
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
 "italian-postfix" "Latin-1" "IT<" t
 "Italian (Italiano) input method with postfix modifiers

a` -> ,A`(B    A` -> ,A@(B    e' -> ,Ai(B    << -> ,A+(B
e` -> ,Ah(B    E` -> ,AH(B    E' -> ,AI(B    >> -> ,A;(B
i` -> ,Al(B    I` -> ,AL(B               o_ -> ,A:(B
o` -> ,Ar(B    O` -> ,AR(B               a_ -> ,A*(B
u` -> ,Ay(B    U` -> ,AY(B

Typewriter-style italian characters.

Doubling the postfix separates the letter and postfix: e.g. a`` -> a`
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?,A@(B)
 ("a`" ?,A`(B)
 ("E`" ?,AH(B)
 ("E'" ?,AI(B)
 ("e`" ?,Ah(B)
 ("e'" ?,Ai(B)
 ("I`" ?,AL(B)
 ("i`" ?,Al(B)
 ("O`" ?,AR(B)
 ("o`" ?,Ar(B)
 ("U`" ?,AY(B)
 ("u`" ?,Ay(B)
 ("<<" ?,A+(B)
 (">>" ?,A;(B)
 ("o_" ?,A:(B)
 ("a_" ?,A*(B)

 ("A``" ["A`"])
 ("a``" ["a`"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("I``" ["I`"])
 ("i``" ["i`"])
 ("O``" ["O`"])
 ("o``" ["o`"])
 ("U``" ["U`"])
 ("u``" ["u`"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("o__" ["o_"])
 ("a__" ["a_"])
 )

(quail-define-package
 "norwegian-postfix" "Latin-1" "NO<" t
 "Norwegian (Norsk) input method (rule: AE->,AF(B   OE->,AX(B   AA->,AE(B   E'->,AI(B)

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
 "scandinavian-postfix" "Latin-1" "SC<" t
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
 "spanish-postfix" "Spanish" "ES<" t
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
 ("U\"" ?,C\(B)
 ("u\"" ?,C|(B)
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
 ("U\"" ["U\""])
 ("u\"" ["U\""])
 ("N~~" ["N~"])
 ("n~~" ["n~"])
 ("?//" ["?/"])
 ("!//" ["!/"])
 )

(quail-define-package
 "swedish-postfix" "Latin-1" "SV<" t
 "Swedish (Svenska) input method (rule: AA -> ,AE(B   AE -> ,AD(B   OE -> ,AV(B   E' -> ,AI(B)

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
 "turkish-latin-3-postfix" "Turkish" "TR3<" t
 "Turkish (T,C|(Brk,Cg(Be) input method with postfix modifiers.

This is for those who use Latin-3 (ISO-8859-3) for Turkish.  If you
use Latin-5 (ISO-8859-9), you should use \"turkish-postfix\" instead.

Note for I, ,C9(B, ,C)(B, i.

A^ -> ,CB(B
C, -> ,CG(B
G^ -> ,C+(B
I  -> I
i  -> ,C9(B
I. -> ,C)(B
i. -> i
O\" -> ,CV(B
S, -> ,C*(B
U\" -> ,C\(B
U^ -> ,C[(B

Doubling the postfix separates the letter and postfix: e.g. a^^ -> a^
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A^" ?,CB(B)
 ("a^" ?,Cb(B)
 ("C," ?,CG(B)
 ("c," ?,Cg(B)
 ("G^" ?,C+(B)
 ("g^" ?,C;(B)
 ("I." ?,C)(B)
 ("i" ?,C9(B)
 ("i." ?i)
 ("O\"" ?,CV(B)
 ("o\"" ?,Cv(B)
 ("S," ?,C*(B)
 ("s," ?,C:(B)
 ("U\"" ?,C\(B)
 ("u\"" ?,C|(B)
 ("U^" ?,C[(B)
 ("u^" ?,C{(B)

 ("A^^" ["A^"])
 ("a^^" ["a^"])
 ("C,," ["C,"])
 ("c,," ["c,"])
 ("G^^" ["G^"])
 ("g^^" ["g^"])
 ("I.." ["I."])
 ("i" ["i"])
 ("i.." ["i."])
 ("O\"\"" ["O\""])
 ("o\"\"" ["o\""])
 ("S,," ["S,"])
 ("s,," ["s,"])
 ("U\"\"" ["U\""])
 ("u\"\"" ["u\""])
 ("U^^" ["U^"])
 ("u^^" ["u^"])
 )

(quail-define-package
 "turkish-postfix" "Turkish" "TR<" t
 "Turkish (T,M|(Brk,Mg(Be) input method with postfix modifiers.

This is for those who use Latin-5 (ISO-8859-9) for Turkish.  If you
use Latin-3 (ISO-8859-3), you should use \"turkish-latin-3-postfix\"
instead.

Note for I, ,M}(B, ,M](B, i.

A^ -> ,MB(B
C, -> ,MG(B
G^ -> ,MP(B
I  -> I
i  -> ,M}(B
I. -> ,M](B
i. -> i
O\" -> ,MV(B
S, -> ,M^(B
U\" -> ,M\(B
U^ -> ,M[(B

Doubling the postfix separates the letter and postfix: e.g. a^^ -> a^
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A^" ?,MB(B)
 ("a^" ?,Mb(B)
 ("C," ?,MG(B)
 ("c," ?,Mg(B)
 ("G^" ?,MP(B)
 ("g^" ?,Mp(B)
 ("I." ?,M](B)
 ("i" ?,M}(B)
 ("i." ?i)
 ("O\"" ?,MV(B)
 ("o\"" ?,Mv(B)
 ("S," ?,M^(B)
 ("s," ?,M~(B)
 ("U\"" ?,M\(B)
 ("u\"" ?,M|(B)
 ("U^" ?,M[(B)
 ("u^" ?,M{(B)

 ("A^^" ["A^"])
 ("a^^" ["a^"])
 ("C,," ["C,"])
 ("c,," ["c,"])
 ("G^^" ["G^"])
 ("g^^" ["g^"])
 ("I.." ["I."])
 ("i" ["i"])
 ("i.." ["i."])
 ("O\"\"" ["O\""])
 ("o\"\"" ["o\""])
 ("S,," ["S,"])
 ("s,," ["s,"])
 ("U\"\"" ["U\""])
 ("u\"\"" ["u\""])
 ("U^^" ["U^"])
 ("u^^" ["u^"])
 )

(quail-define-package
 "british" "Latin-1" ",A#(B@" t
 "British English input method with Latin-1 character ,A#(B (# -> ,A#(B)"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("#" [?,A#(B ?#])
 )

;; The following are various quail packages for those who think
;; the aboves are too awkward.  Supported languages and their
;; package name are:
;;
;; French	(frnch, azerty)
;; Icelandic	(iclndc)
;; Denish	(dnsh)
;; Norwegian	(nrwgn)
;; Swedish	(swdsh)
;; Finnish	(fnnsh)
;; German	(grmn)
;; Italian	(itln)
;; Spanish	(spnsh)
;; Dvorak	(dvorak)
;;
;;; 92.12.15  created for Mule Ver.0.9.6 by Takahashi N. <ntakahas@etl.go.jp>
;;; 92.12.29  modified by Takahashi N. <ntakahas@etl.go.jp>

;;
(quail-define-package
 "french-keyboard" "French" "FR@" t
 "French (Fran,Ag(Bais) input method simulating some French keyboard
<e dans l'o> n'est pas disponible." nil t t t t nil nil nil nil nil t)

;; ,Aj(B1  ,Ai(B2  ,Ah(B3  ,At(B4  ,An(B5  ,Ao(B6  ,Ab(B7  ,A{(B8  ,Ay(B9  ,A`(B0  -_  ,Ak(B+  `~
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AgG(B  ,A|(B&
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ;:  '"  \|
;;    zZ  xX  cC  vV  bB  nN  mM  ,(  .)  !?

(quail-define-rules
 ("1" ?,Aj(B)
 ("2" ?,Ai(B)
 ("3" ?,Ah(B)
 ("4" ?,At(B)
 ("5" ?,An(B)
 ("6" ?,Ao(B)
 ("7" ?,Ab(B)
 ("8" ?,A{(B)
 ("9" ?,Ay(B)
 ("0" ?,A`(B)
 ("=" ?,Ak(B)
 ("[" ?,Ag(B)
 ("]" ?,A|(B)

 ("!" ?1)
 ("@" ?2)
 ("#" ?3)
 ("$" ?4)
 ("%" ?5)
 ("^" ?6)
 ("&" ?7)
 ("*" ?8)
 ("(" ?9)
 (")" ?0)
 ("{" ?,AG(B)
 ("}" ?&)
 ("<" ?\()
 (">" ?\))
 )

;;
(quail-define-package
 "french-azerty" "French" "AZ@" t
 "French (Fran,Ag(Bais) input method simulating Azerty keyboard

Similaire au clavier fran,Ag(Bais de SUN.
pr,Ai(Bfixes:  ^ pour circonflexe,  ,A((B pour tr,Ai(Bma.
<e dans l'o> n'est pas disponible." nil t t t t nil nil nil nil nil t)

;; &1  ,Ai(B2  "3  '4  (5  ,A'(B6  ,Ah(B7  !8  ,Ag(B9  ,A`(B0  ),A0(B -_  @~
;;  aA  zZ  eE  rR  tT  yY  uU  iI  oO  pP  ^,A((B  `$
;;   qQ  sS  dD  fF  gG  hH  jJ  kK  lL  mM  ,Ay(B%  *|
;;    wW  xX  cC  vV  bB  nN  ,?  ;.  :/  =+

(quail-define-rules
 ("1" ?&)
 ("2" ?,Ai(B)
 ("3" ?\")
 ("4" ?')
 ("5" ?\()
 ("6" ?,A'(B)
 ("7" ?,Ah(B)
 ("8" ?!)
 ("9" ?,Ag(B)
 ("0" ?,A`(B)
 ("-" ?\))
 ("=" ?-)
 ("`" ?@)
 ("q" ?a)
 ("w" ?z)
 ("e" ?e)
 ("r" ?r)
 ("t" ?t)
 ("y" ?y)
 ("u" ?u)
 ("i" ?i)
 ("o" ?o)
 ("p" ?p)
 ("[" ?^)
 ("]" ?`)
 ("a" ?q)
 ("s" ?s)
 ("d" ?d)
 ("f" ?f)
 ("g" ?g)
 ("h" ?h)
 ("j" ?j)
 ("k" ?k)
 ("l" ?l)
 (";" ?m)
 ("'" ?,Ay(B)
 ("\\" ?*)
 ("z" ?w)
 ("x" ?x)
 ("c" ?c)
 ("v" ?v)
 ("b" ?b)
 ("n" ?n)
 ("m" ?,)
 ("," ?\;)
 ("." ?:)
 ("/" ?=)

 ("!" ?1)
 ("@" ?2)
 ("#" ?3)
 ("$" ?4)
 ("%" ?5)
 ("^" ?6)
 ("&" ?7)
 ("*" ?8)
 ("(" ?9)
 (")" ?0)
 ("_" ?,A0(B)
 ("+" ?_)
 ("~" ?~)
 ("Q" ?A)
 ("W" ?Z)
 ("E" ?E)
 ("R" ?R)
 ("T" ?T)
 ("Y" ?Y)
 ("U" ?U)
 ("I" ?I)
 ("O" ?O)
 ("P" ?P)
 ("{" ?,A((B)
 ("}" ?$)
 ("A" ?Q)
 ("S" ?S)
 ("D" ?D)
 ("F" ?F)
 ("G" ?G)
 ("H" ?H)
 ("J" ?J)
 ("K" ?K)
 ("L" ?L)
 (":" ?M)
 ("\"" ?%)
 ("|" ?|)
 ("Z" ?W)
 ("X" ?X)
 ("C" ?C)
 ("V" ?V)
 ("B" ?B)
 ("N" ?N)
 ("M" ??)
 ("<" ?.)
 (">" ?/)
 ("?" ?+)

 ("[q" ?,Ab(B)
 ("[e" ?,Aj(B)
 ("[i" ?,An(B)
 ("[o" ?,At(B)
 ("[u" ?,A{(B)

 ("{e" ?,Ak(B)
 ("{i" ?,Ao(B)
 ("{u" ?,A|(B)

 ("[[" ?^)
 ("{{" ?,A((B)
 )

;;
(quail-define-package
 "icelandic-keyboard" "Latin-1" "IS@" t
 "Icelandic (,AM(Bslenska) input method simulating some Icelandic keyboard

Dead accent is right to ,Af(B." nil t t t t nil nil nil nil nil t)

;; 1!  2"  3#  4$  5%  6^  7&  8*  9(  0)  ,AvV(B  -_  `~
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,ApP(B  '?
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AfF(B  ,A44(B  +*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  ,A~^(B

(quail-define-rules
 ("-" ?,Av(B)
 ("=" ?-)
 ("[" ?,Ap(B)
 ("]" ?')
 (";" ?,Af(B)
 ("'" ?,A4(B)
 ("\\" ?+)
 ("/" ?,A~(B)

 ("@" ?\")
 ("_" ?,AV(B)
 ("+" ?_)
 ("{" ?,AP(B)
 ("}" ??)
 (":" ?,AF(B)
 ("\"" ?,A4(B)
 ("|" ?*)
 ("<" ?\;)
 (">" ?:)
 ("?" ?,A^(B)

 ("'a" ?,Aa(B)
 ("'e" ?,Ai(B)
 ("'i" ?,Am(B)
 ("'o" ?,As(B)
 ("'u" ?,Az(B)
 ("'y" ?,A}(B)
 ("'A" ?,AA(B)
 ("'E" ?,AI(B)
 ("'I" ?,AM(B)
 ("'O" ?,AS(B)
 ("'U" ?,AZ(B)
 ("'Y" ?,A](B)

 ("''" ?,A4(B)
 )

;;
(quail-define-package
 "danish-keyboard" "Latin-1" "DA@" t
 "Danish input method simulating SUN Danish keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3#  4,A$(B  5%  6&  7/  8(  9)  0=  +?  ,A='(B  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AeE(B  ,AiI(B
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AfF(B  ,AxX(B  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?+)
 ("=" ?,A=(B)
 ("`" ?~)
 ("[" ?,Ae(B)
 ("]" ?,Ai(B)
 (";" ?,Af(B)
 ("'" ?,Ax(B)
 ("\\" ?')
 ("/" ?-)

 ("@" ?\")
 ("$" ?,A$(B)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?,A'(B)
 ("~" ?^)
 ("{" ?,AE(B)
 ("}" ?,AI(B)
 (":" ?,AF(B)
 ("\"" ?,AX(B)
 ("|" ?*)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "norwegian-keyboard" "Latin-1" "NO@" t
 "Norwegian (Norsk) input method simulating SUN Norwegian keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3#  4,A$(B  5%  6&  7/  8(  9)  0=  +?  |,A'(B  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AeE(B  ,AiI(B
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AxX(B  ,AfF(B  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  '?

(quail-define-rules
 ("-" ?+)
 ("=" ?|)
 ("`" ?~)
 ("[" ?,Ae(B)
 ("]" ?,Ai(B)
 (";" ?,Ax(B)
 ("'" ?,Af(B)
 ("\\" ?')
 ("/" ?-)

 ("!" ?!)
 ("@" ?\")
 ("$" ?,A$(B)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?,A'(B)
 ("~" ?^)
 ("{" ?,AE(B)
 ("}" ?,AI(B)
 (":" ?,AX(B)
 ("\"" ?,AF(B)
 ("|" ?*)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "swedish-keyboard" "Latin-1" "SV@" t
 "Swedish (Svenska) input method simulating SUN Swedish/Finnish keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3#  4,A$(B  5%  6&  7/  8(  9)  0=  +?  ,A'=(B  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AeE(B  ,AiI(B
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AvV(B  ,AdD(B  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?+)
 ("=" ?,A'(B)
 ("`" ?~)
 ("[" ?,Ae(B)
 ("]" ?,Ai(B)
 (";" ?,Av(B)
 ("'" ?,Ad(B)
 ("\\" ?')
 ("/" ?-)

 ("@" ?\")
 ("$" ?,A$(B)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?,A=(B)
 ("~" ?^)
 ("{" ?,AE(B)
 ("}" ?,AI(B)
 (":" ?,AV(B)
 ("\"" ?,AD(B)
 ("|" ?*)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "finnish-keyboard" "Latin-1" "FI@" t
 "Finnish input method simulating SUN Finnish/Swedish keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3#  4,A$(B  5%  6&  7/  8(  9)  0=  +?  ,A'=(B  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AeE(B  ,AiI(B
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AvV(B  ,AdD(B  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?+)
 ("=" ?,A'(B)
 ("`" ?~)
 ("[" ?,Ae(B)
 ("]" ?,Ai(B)
 (";" ?,Av(B)
 ("'" ?,Ad(B)
 ("\\" ?')
 ("/" ?-)

 ("@" ?\")
 ("$" ?,A$(B)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?,A=(B)
 ("~" ?^)
 ("{" ?,AE(B)
 ("}" ?,AI(B)
 (":" ?,AV(B)
 ("\"" ?,AD(B)
 ("|" ?*)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "german" "German" "DE@" t
 "German (Deutsch) input method simulating SUN German keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3,A'(B  4$  5%  6&  7/  8(  9)  0=  ,A_(B?  [{  ]}
;;  qQ  wW  eE  rR  tT  zZ  uU  iI  oO  pP  ,A|\(B  +*
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AvV(B  ,AdD(B  #^
;;    yY  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?,A_(B)
 ("=" ?\[)
 ("`" ?\])
 ("y" ?z)
 ("[" ?,A|(B)
 ("]" ?+)
 (";" ?,Av(B)
 ("'" ?,Ad(B)
 ("\\" ?#)
 ("z" ?y)
 ("/" ?-)

 ("@" ?\")
 ("#" ?,A'(B)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("Y" ?Z)
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?{)
 ("~" ?})
 ("{" ?,A\(B)
 ("}" ?*)
 (":" ?,AV(B)
 ("\"" ?,AD(B)
 ("|" ?^)
 ("Z" ?Y)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "italian-keyboard" "Latin-1" "IT@" t
 "Italian (Italiano) input method simulating SUN Italian keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3,A#(B  4$  5%  6&  7/  8(  9)  0=  '?  ,Al(B^  `~
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,Ahi(B  +*
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,Arg(B  ,A`0(B  ,Ay'(B
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?')
 ("=" ?,Al(B)
 ("[" ?,Ah(B)
 ("]" ?+)
 (";" ?,Ar(B)
 ("'" ?,A`(B)
 ("\\" ?,Ay(B)
 ("/" ?-)

 ("@" ?\")
 ("#" ?,A#(B)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?^)
 ("~" ?~)
 ("{" ?,Ai(B)
 ("}" ?*)
 (":" ?,Ag(B)
 ("\"" ?,A0(B)
 ("|" ?,A'(B)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "spanish-keyboard" "Spanish" "ES@" t
 "Spanish (Espa,Aq(Bol) input method simulating SUN Spanish keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3,A7(B  4$  5%  6&  7/  8(  9)  0=  '?  ,A!?(B  ,AmM(B
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AiI(B  ,AsS(B
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AqQ(B  ,AaA(B  ,AzZ(B
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?')
 ("=" ?,A!(B)
 ("`" ?,Am(B)
 ("[" ?,Ai(B)
 ("]" ?,As(B)
 (";" ?,Aq(B)
 ("'" ?,Aa(B)
 ("\\" ?,Az(B)
 ("/" ?-)

 ("@" ?\")
 ("#" ?,A7(B)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?,A?(B)
 ("~" ?,AM(B)
 ("{" ?,AI(B)
 ("}" ?,AS(B)
 (":" ?,AQ(B)
 ("\"" ?,AA(B)
 ("|" ?,AZ(B)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "english-dvorak" "English" "DV@" t
 "English (ASCII) input method simulating Dvorak keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2@  3#  4$  5%  6^  7&  8*  9(  0)  [{  ]}  `~
;;  '"  ,<  .>  pP  yY  fF  gG  cC  rR  lL  /?  =+
;;   aA  oO  eE  uU  iI  dD  hH  tT  nN  sS  -_  \|
;;    ;:  qQ  jJ  kK  xX  bB  mM  wW  vV  zZ

(quail-define-rules
 ("-" ?\[)
 ("=" ?\])
 ("`" ?`)
 ("q" ?')
 ("w" ?,)
 ("e" ?.)
 ("r" ?p)
 ("t" ?y)
 ("y" ?f)
 ("u" ?g)
 ("i" ?c)
 ("o" ?r)
 ("p" ?l)
 ("[" ?/)
 ("]" ?=)
 ("a" ?a)
 ("s" ?o)
 ("d" ?e)
 ("f" ?u)
 ("g" ?i)
 ("h" ?d)
 ("j" ?h)
 ("k" ?t)
 ("l" ?n)
 (";" ?s)
 ("'" ?-)
 ("\\" ?\\)
 ("z" ?\;)
 ("x" ?q)
 ("c" ?j)
 ("v" ?k)
 ("b" ?x)
 ("n" ?b)
 ("m" ?m)
 ("," ?w)
 ("." ?v)
 ("/" ?z)

 ("_" ?{)
 ("+" ?})
 ("~" ?~)
 ("Q" ?\")
 ("W" ?<)
 ("E" ?>)
 ("R" ?P)
 ("T" ?Y)
 ("Y" ?F)
 ("U" ?G)
 ("I" ?C)
 ("O" ?R)
 ("P" ?L)
 ("{" ??)
 ("}" ?+)
 ("A" ?A)
 ("S" ?O)
 ("D" ?E)
 ("F" ?U)
 ("G" ?I)
 ("H" ?D)
 ("J" ?H)
 ("K" ?T)
 ("L" ?N)
 (":" ?S)
 ("\"" ?_)
 ("|" ?|)
 ("Z" ?:)
 ("X" ?Q)
 ("C" ?J)
 ("V" ?K)
 ("B" ?X)
 ("N" ?B)
 ("M" ?M)
 ("<" ?W)
 (">" ?V)
 ("?" ?Z)
 )

(quail-define-package
 "latin-postfix" "Latin" "L<" t
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
  cedilla    |    ,    | c, -> ,Ag(B
  ogonek     |    ,    | a, -> $,1 %(B
  breve      |    ~    | a~ -> $,1 #(B
  caron      |    ~    | c~ -> $,1 -(B
  dbl. acute |    :    | o: -> $,1 q(B
  ring       |    .    | u. -> $,1!/(B
  dot        |    .    | z. -> $,1!<(B
  stroke     |    /    | d/ -> $,1 1(B
  nordic     |    /    | d/ -> ,Ap(B   t/ -> ,A~(B   a/ -> ,Ae(B   e/ -> ,Af(B   o/ -> ,Ax(B
  others     |    /    | s/ -> ,A_(B   ?/ -> ,A?(B   !/ -> ,A!(B   // -> ,A0(B
             | various | << -> ,A+(B   >> -> ,A;(B   o_ -> ,A:(B   a_ -> ,A*(B

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

;; Fixme: ,A&(B ,A'(B ,A((B ,A)(B ,A,(B ,A-(B ,A.(B ,A/(B ,A1(B ,A2(B ,A3(B ,A4(B ,A5(B ,A6(B ,A7(B ,A8(B ,A9(B ,A<(B ,A=(B ,A>(B ,AW(B ,Aw(B
(quail-define-rules
 (" _" ?,A (B)
 ("!/" ?,A!(B)
 ("//" ?,A0(B)
 ("<<" ?\,A+(B)
 (">>" ?\,A;(B)
 ("?/" ?,A?(B)
 ("$/" ?,A#(B)
 ("$/" ?,A$(B)
 ("A'" ?,AA(B)
 ("A," ?$,1 $(B)
 ("A-" ?$,1  (B)
 ("A/" ?,AE(B)
 ("A\"" ?,AD(B)
 ("A^" ?,AB(B)
 ("A`" ?,A@(B)
 ("A~" ?,AC(B)
 ("A~" ?$,1 "(B)
 ("C'" ?$,1 &(B)
 ("C," ?,AG(B)
 ("C." ?$,1 *(B)
 ("C^" ?$,1 ((B)
 ("C~" ?$,1 ,(B)
 ("D/" ?,AP(B)
 ("D/" ?$,1 0(B)
 ("D~" ?$,1 .(B)
 ("E'" ?,AI(B)
 ("E," ?$,1 8(B)
 ("E-" ?$,1 2(B)
 ("E." ?$,1 6(B)
 ("E/" ?,AF(B)
 ("E\"" ?,AK(B)
 ("E^" ?,AJ(B)
 ("E`" ?,AH(B)
 ("E~" ?$,1 :(B)
 ("G," ?$,1 B(B)
 ("G." ?$,1 @(B)
 ("G^" ?$,1 <(B)
 ("G~" ?$,1 >(B)
 ("H/" ?$,1 F(B)
 ("H^" ?$,1 D(B)
 ("I'" ?,AM(B)
 ("I," ?$,1 N(B)
 ("I-" ?$,1 J(B)
 ("I." ?$,1 P(B)
 ("I\"" ?,AO(B)
 ("I^" ?,AN(B)
 ("I`" ?,AL(B)
 ("I~" ?$,1 H(B)
 ("J^" ?$,1 T(B)
 ("K," ?$,1 V(B)
 ("L'" ?$,1 Y(B)
 ("L," ?$,1 [(B)
 ("L/" ?$,1 a(B)
 ("L~" ?$,1 ](B)
 ("N'" ?$,1 c(B)
 ("N," ?$,1 e(B)
 ("N/" ?$,1 j(B)
 ("N~" ?,AQ(B)
 ("N~" ?$,1 g(B)
 ("O'" ?,AS(B)
 ("O-" ?$,1 l(B)
 ("O/" ?,AX(B)
 ("O/" ?$,1 r(B)
 ("O:" ?$,1 p(B)
 ("O\"" ?,AV(B)
 ("O^" ?,AT(B)
 ("O`" ?,AR(B)
 ("O~" ?,AU(B)
 ("R'" ?$,1 t(B)
 ("R," ?$,1 v(B)
 ("R~" ?$,1 x(B)
 ("S'" ?$,1 z(B)
 ("S," ?$,1 ~(B)
 ("S^" ?$,1 |(B)
 ("S~" ?$,1! (B)
 ("T," ?$,1!"(B)
 ("T/" ?,A^(B)
 ("T/" ?$,1!&(B)
 ("T~" ?$,1!$(B)
 ("U'" ?,AZ(B)
 ("U," ?$,1!2(B)
 ("U-" ?$,1!*(B)
 ("U." ?$,1!.(B)
 ("U:" ?$,1!0(B)
 ("U\"" ?,A\(B)
 ("U^" ?,A[(B)
 ("U`" ?,AY(B)
 ("U~" ?$,1!((B)
 ("U~" ?$,1!,(B)
 ("Y'" ?,A](B)
 ("Y\"" ?$,1!8(B)
 ("Y=" ?,A%(B)
 ("Z'" ?$,1!9(B)
 ("Z." ?$,1!;(B)
 ("Z~" ?$,1!=(B)
 ("a'" ?,Aa(B)
 ("a," ?$,1 %(B)
 ("a-" ?$,1 !(B)
 ("a/" ?,Ae(B)
 ("a\"" ?,Ad(B)
 ("a^" ?,Ab(B)
 ("a_" ?,A*(B)
 ("a`" ?,A`(B)
 ("a~" ?,Ac(B)
 ("a~" ?$,1 #(B)
 ("c'" ?$,1 '(B)
 ("c," ?,Ag(B)
 ("c." ?$,1 +(B)
 ("c^" ?$,1 )(B)
 ("c~" ?$,1 -(B)
 ("c/" ?,A"(B)
 ("d/" ?,Ap(B)
 ("d/" ?$,1 1(B)
 ("d~" ?$,1 /(B)
 ("e'" ?,Ai(B)
 ("e," ?$,1 9(B)
 ("e-" ?$,1 3(B)
 ("e." ?$,1 7(B)
 ("e/" ?,Af(B)
 ("e\"" ?,Ak(B)
 ("e^" ?,Aj(B)
 ("e`" ?,Ah(B)
 ("e~" ?$,1 ;(B)
 ("e=" ?$,1tL(B)
 ("g," ?$,1 C(B)
 ("g." ?$,1 A(B)
 ("g^" ?$,1 =(B)
 ("g~" ?$,1 ?(B)
 ("h/" ?$,1 G(B)
 ("h^" ?$,1 E(B)
 ("i'" ?,Am(B)
 ("i," ?$,1 O(B)
 ("i-" ?$,1 K(B)
 ("i." ?$,1 Q(B)
 ("i\"" ?,Ao(B)
 ("i^" ?,An(B)
 ("i`" ?,Al(B)
 ("i~" ?$,1 I(B)
 ("j^" ?$,1 U(B)
 ("k," ?$,1 W(B)
 ("k/" ?$,1 X(B)
 ("l'" ?$,1 Z(B)
 ("l," ?$,1 \(B)
 ("l/" ?$,1 b(B)
 ("l~" ?$,1 ^(B)
 ("n'" ?$,1 d(B)
 ("n," ?$,1 f(B)
 ("n/" ?$,1 k(B)
 ("n~" ?,Aq(B)
 ("n~" ?$,1 h(B)
 ("o'" ?,As(B)
 ("o-" ?$,1 m(B)
 ("o/" ?,Ax(B)
 ("o/" ?$,1 s(B)
 ("o:" ?$,1 q(B)
 ("o\"" ?,Av(B)
 ("o^" ?,At(B)
 ("o_" ?,A:(B)
 ("o`" ?,Ar(B)
 ("o~" ?,Au(B)
 ("r'" ?$,1 u(B)
 ("r," ?$,1 w(B)
 ("r~" ?$,1 y(B)
 ("s'" ?$,1 {(B)
 ("s," ?$,1 (B)
 ("s/" ?,A_(B)
 ("s^" ?$,1 }(B)
 ("s~" ?$,1!!(B)
 ("t," ?$,1!#(B)
 ("t/" ?,A~(B)
 ("t/" ?$,1!'(B)
 ("t~" ?$,1!%(B)
 ("u'" ?,Az(B)
 ("u," ?$,1!3(B)
 ("u-" ?$,1!+(B)
 ("u." ?$,1!/(B)
 ("u:" ?$,1!1(B)
 ("u\"" ?,A|(B)
 ("u^" ?,A{(B)
 ("u`" ?,Ay(B)
 ("u~" ?$,1!)(B)
 ("u~" ?$,1!-(B)
 ("y'" ?,A}(B)
 ("y\"" ?,A(B)
 ("z'" ?$,1!:(B)
 ("z." ?$,1!<(B)
 ("z~" ?$,1!>(B)

 ("!//" ["!/"])
 ("///" ["//"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("?//" ["?/"])
 ("$//" ["$/"])
 ("A''" ["A'"])
 ("A,," ["A,"])
 ("A--" ["A-"])
 ("A//" ["A/"])
 ("A\"\"" ["A\""])
 ("A^^" ["A^"])
 ("A``" ["A`"])
 ("A~~" ["A~"])
 ("C''" ["C'"])
 ("C,," ["C,"])
 ("C.." ["C."])
 ("C^^" ["C^"])
 ("C~~" ["C~"])
 ("D//" ["D/"])
 ("D~~" ["D~"])
 ("E''" ["E'"])
 ("E,," ["E,"])
 ("E--" ["E-"])
 ("E.." ["E."])
 ("E//" ["E/"])
 ("E\"\"" ["E\""])
 ("E^^" ["E^"])
 ("E``" ["E`"])
 ("E~~" ["E~"])
 ("G,," ["G,"])
 ("G.." ["G."])
 ("G^^" ["G^"])
 ("G~~" ["G~"])
 ("H//" ["H/"])
 ("H^^" ["H^"])
 ("I''" ["I'"])
 ("I,," ["I,"])
 ("I--" ["I-"])
 ("I.." ["I."])
 ("I\"\"" ["I\""])
 ("I^^" ["I^"])
 ("I``" ["I`"])
 ("I~~" ["I~"])
 ("J^^" ["J^"])
 ("K,," ["K,"])
 ("L''" ["L'"])
 ("L,," ["L,"])
 ("L//" ["L/"])
 ("L~~" ["L~"])
 ("N''" ["N'"])
 ("N,," ["N,"])
 ("N//" ["N/"])
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
 ("R,," ["R,"])
 ("R~~" ["R~"])
 ("S''" ["S'"])
 ("S,," ["S,"])
 ("S^^" ["S^"])
 ("S~~" ["S~"])
 ("T,," ["T,"])
 ("T//" ["T/"])
 ("T~~" ["T~"])
 ("U''" ["U'"])
 ("U,," ["U,"])
 ("U--" ["U-"])
 ("U.." ["U."])
 ("U::" ["U:"])
 ("U\"\"" ["U\""])
 ("U^^" ["U^"])
 ("U``" ["U`"])
 ("U~~" ["U~"])
 ("Y''" ["Y'"])
 ("Y\"\"" ["Y\""])
 ("Y==" ["Y="])
 ("Z''" ["Z'"])
 ("Z.." ["Z."])
 ("Z~~" ["Z~"])
 ("a''" ["a'"])
 ("a,," ["a,"])
 ("a--" ["a-"])
 ("a//" ["a/"])
 ("a\"\"" ["a\""])
 ("a^^" ["a^"])
 ("a__" ["a_"])
 ("a``" ["a`"])
 ("a~~" ["a~"])
 ("c''" ["c'"])
 ("c,," ["c,"])
 ("c.." ["c."])
 ("c^^" ["c^"])
 ("c~~" ["c~"])
 ("c//" ["c/"])
 ("d//" ["d/"])
 ("d~~" ["d~"])
 ("e''" ["e'"])
 ("e,," ["e,"])
 ("e--" ["e-"])
 ("e.." ["e."])
 ("e//" ["e/"])
 ("e\"\"" ["e\""])
 ("e^^" ["e^"])
 ("e``" ["e`"])
 ("e==" ["e="])
 ("e~~" ["e~"])
 ("g,," ["g,"])
 ("g.." ["g."])
 ("g^^" ["g^"])
 ("g~~" ["g~"])
 ("h//" ["h/"])
 ("h^^" ["h^"])
 ("i''" ["i'"])
 ("i,," ["i,"])
 ("i--" ["i-"])
 ("i.." ["i."])
 ("i\"\"" ["i\""])
 ("i^^" ["i^"])
 ("i``" ["i`"])
 ("i~~" ["i~"])
 ("j^^" ["j^"])
 ("k,," ["k,"])
 ("k//" ["k/"])
 ("l''" ["l'"])
 ("l,," ["l,"])
 ("l//" ["l/"])
 ("l~~" ["l~"])
 ("n''" ["n'"])
 ("n,," ["n,"])
 ("n//" ["n/"])
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
 ("r,," ["r,"])
 ("r~~" ["r~"])
 ("s''" ["s'"])
 ("s,," ["s,"])
 ("s//" ["s/"])
 ("s^^" ["s^"])
 ("s~~" ["s~"])
 ("t,," ["t,"])
 ("t//" ["t/"])
 ("t~~" ["t~"])
 ("u''" ["u'"])
 ("u,," ["u,"])
 ("u--" ["u-"])
 ("u.." ["u."])
 ("u::" ["u:"])
 ("u\"\"" ["u\""])
 ("u^^" ["u^"])
 ("u``" ["u`"])
 ("u~~" ["u~"])
 ("y''" ["y'"])
 ("y\"\"" ["y\""])
 ("z''" ["z'"])
 ("z.." ["z."])
 ("z~~" ["z~"])
 )

;; Derived from Slovenian.kmap from Yudit
;; attributed as: 2001-11-11 Roman Maurer <roman.maurer@amis.net>
(quail-define-package
 "slovenian" "Slovenian" "Sl" t
 "Slovenian postfix input."
 nil t t t nil nil nil nil nil nil t)

(quail-define-rules
 ("C<" ?,BH(B)
 ("C'" ?,BF(B)
 ("D;" ?,BP(B)
 ("S<" ?,B)(B)
 ("Z<" ?,B.(B)
 ("c<" ?,Bh(B)
 ("c'" ?,Bf(B)
 ("d;" ?,Bp(B)
 ("s<" ?,B9(B)
 ("z<" ?,B>(B))

;;; arch-tag: 170180fb-9617-4d58-9d51-65ca23c05d94
;;; latin-post.el ends here
