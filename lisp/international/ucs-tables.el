;;; ucs-tables.el --- translation to, from and via Unicode  -*- coding: iso-2022-7bit -*-

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

;; This file provides tables mapping between Unicode numbers and
;; emacs-mule characters from the iso8859 charsets.  These are used to
;; construct other mappings between the Mule iso8859 charsets and the
;; emacs-unicode charsets and also a table that unifies iso8859
;; characters using a single charset as far as possible.  These tables
;; can be used by latin1-disp.el to display some Unicode characters
;; without a Unicode font and by utf-8.el to unify Latin-N as far as
;; possible into Latin-1 on encoding.

;;; Code:

(defvar ucs-mule-8859-to-ucs-table (make-translation-table)
  "Translation table from Emacs ISO-8859 characters to Unicode.
This maps Emacs characters from the non-Latin-1
...-iso8859-... charsets to their Unicode code points.  This is a
many-to-one mapping.")

(defvar ucs-ucs-to-mule-8859-table (make-translation-table)
  "Translation table from Unicode to Emacs ISO-8859 characters.
This maps Unicode code points to corresponding Emacs characters from
the ...-iso8859-... charsets.  This is made a one-to-one mapping where
the same character occurs in more than one set by preferring the Emacs
iso-8859-N character with lowest N .")

(defvar ucs-mule-8859-to-mule-unicode (make-translation-table)
  "Translation table from Emacs ISO-8859 characters to Mule Unicode.
This maps Emacs characters from the non-Latin-1
...-iso8859-... charsets to characters from the
mule-unicode-... charsets.  This is a many-to-one mapping.  The
characters translated to are suitable for encoding using the
`mule-utf-8' coding system.")

(defvar ucs-mule-unicode-to-mule-8859 (make-translation-table)
  "Translation table from Mule Unicode to Emacs ISO-8859 characters.
This maps non-Latin-1 Emacs characters from the
mule-unicode-... charsets used by the `mule-utf-8' coding system to
characters from the ...-iso8859-... charsets.  This is made a
one-to-one mapping where the same character occurs in more than one
set by preferring the Emacs iso-8859-N character with lowest N.")

(defvar ucs-latin-1-unification-table (make-translation-table)
  "Translation table from other ISO-8859 characters to Latin-1.
This maps Emacs characters from the non-Latin-1
...-iso8859-... charsets to their equivalent Latin-1 characters, when
they have an equivalent.  E.g. capital A with diaresis is code point
0xC4 in both Latin-1 and Latin-2, so this table maps Emacs character
0x944 to 0x8c4.  This is a many-to-one mapping.")

(defcustom ucs-preferred-8859-set 'latin-iso8859-1
  "Preferred charset to use for the `ucs-latin-1-unification-table'
target.  Only a Latin-N set makes sense.  You might want to change
this from the default latin-iso8859-1 to match your preferred coding
system in a non-Latin-1 environment."
  :type '(choice (const latin-iso8859-15)
		 (const latin-iso8859-14)
		 (const latin-iso8859-9)
		 (const latin-iso8859-5)
		 (const latin-iso8859-4)
		 (const latin-iso8859-3)
		 (const latin-iso8859-2)
		 (const latin-iso8859-1)))

;; There doesn't seem to be a need to make these let bindings into
;; defvars, so we'll let the data get GC'ed.
(let ((ucs-8859-2-alist
       '((?\,B (B . ?\x00A0) ;; NO-BREAK SPACE
	 (?\,B!(B . ?\x0104) ;; LATIN CAPITAL LETTER A WITH OGONEK
	 (?\,B"(B . ?\x02D8) ;; BREVE
	 (?\,B#(B . ?\x0141) ;; LATIN CAPITAL LETTER L WITH STROKE
	 (?\,B$(B . ?\x00A4) ;; CURRENCY SIGN
	 (?\,B%(B . ?\x013D) ;; LATIN CAPITAL LETTER L WITH CARON
	 (?\,B&(B . ?\x015A) ;; LATIN CAPITAL LETTER S WITH ACUTE
	 (?\,B'(B . ?\x00A7) ;; SECTION SIGN
	 (?\,B((B . ?\x00A8) ;; DIAERESIS
	 (?\,B)(B . ?\x0160) ;; LATIN CAPITAL LETTER S WITH CARON
	 (?\,B*(B . ?\x015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
	 (?\,B+(B . ?\x0164) ;; LATIN CAPITAL LETTER T WITH CARON
	 (?\,B,(B . ?\x0179) ;; LATIN CAPITAL LETTER Z WITH ACUTE
	 (?\,B-(B . ?\x00AD) ;; SOFT HYPHEN
	 (?\,B.(B . ?\x017D) ;; LATIN CAPITAL LETTER Z WITH CARON
	 (?\,B/(B . ?\x017B) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
	 (?\,B0(B . ?\x00B0) ;; DEGREE SIGN
	 (?\,B1(B . ?\x0105) ;; LATIN SMALL LETTER A WITH OGONEK
	 (?\,B2(B . ?\x02DB) ;; OGONEK
	 (?\,B3(B . ?\x0142) ;; LATIN SMALL LETTER L WITH STROKE
	 (?\,B4(B . ?\x00B4) ;; ACUTE ACCENT
	 (?\,B5(B . ?\x013E) ;; LATIN SMALL LETTER L WITH CARON
	 (?\,B6(B . ?\x015B) ;; LATIN SMALL LETTER S WITH ACUTE
	 (?\,B7(B . ?\x02C7) ;; CARON
	 (?\,B8(B . ?\x00B8) ;; CEDILLA
	 (?\,B9(B . ?\x0161) ;; LATIN SMALL LETTER S WITH CARON
	 (?\,B:(B . ?\x015F) ;; LATIN SMALL LETTER S WITH CEDILLA
	 (?\,B;(B . ?\x0165) ;; LATIN SMALL LETTER T WITH CARON
	 (?\,B<(B . ?\x017A) ;; LATIN SMALL LETTER Z WITH ACUTE
	 (?\,B=(B . ?\x02DD) ;; DOUBLE ACUTE ACCENT
	 (?\,B>(B . ?\x017E) ;; LATIN SMALL LETTER Z WITH CARON
	 (?\,B?(B . ?\x017C) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
	 (?\,B@(B . ?\x0154) ;; LATIN CAPITAL LETTER R WITH ACUTE
	 (?\,BA(B . ?\x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
	 (?\,BB(B . ?\x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
	 (?\,BC(B . ?\x0102) ;; LATIN CAPITAL LETTER A WITH BREVE
	 (?\,BD(B . ?\x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
	 (?\,BE(B . ?\x0139) ;; LATIN CAPITAL LETTER L WITH ACUTE
	 (?\,BF(B . ?\x0106) ;; LATIN CAPITAL LETTER C WITH ACUTE
	 (?\,BG(B . ?\x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
	 (?\,BH(B . ?\x010C) ;; LATIN CAPITAL LETTER C WITH CARON
	 (?\,BI(B . ?\x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
	 (?\,BJ(B . ?\x0118) ;; LATIN CAPITAL LETTER E WITH OGONEK
	 (?\,BK(B . ?\x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
	 (?\,BL(B . ?\x011A) ;; LATIN CAPITAL LETTER E WITH CARON
	 (?\,BM(B . ?\x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
	 (?\,BN(B . ?\x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
	 (?\,BO(B . ?\x010E) ;; LATIN CAPITAL LETTER D WITH CARON
	 (?\,BP(B . ?\x0110) ;; LATIN CAPITAL LETTER D WITH STROKE
	 (?\,BQ(B . ?\x0143) ;; LATIN CAPITAL LETTER N WITH ACUTE
	 (?\,BR(B . ?\x0147) ;; LATIN CAPITAL LETTER N WITH CARON
	 (?\,BS(B . ?\x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
	 (?\,BT(B . ?\x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	 (?\,BU(B . ?\x0150) ;; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
	 (?\,BV(B . ?\x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
	 (?\,BW(B . ?\x00D7) ;; MULTIPLICATION SIGN
	 (?\,BX(B . ?\x0158) ;; LATIN CAPITAL LETTER R WITH CARON
	 (?\,BY(B . ?\x016E) ;; LATIN CAPITAL LETTER U WITH RING ABOVE
	 (?\,BZ(B . ?\x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
	 (?\,B[(B . ?\x0170) ;; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
	 (?\,B\(B . ?\x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
	 (?\,B](B . ?\x00DD) ;; LATIN CAPITAL LETTER Y WITH ACUTE
	 (?\,B^(B . ?\x0162) ;; LATIN CAPITAL LETTER T WITH CEDILLA
	 (?\,B_(B . ?\x00DF) ;; LATIN SMALL LETTER SHARP S
	 (?\,B`(B . ?\x0155) ;; LATIN SMALL LETTER R WITH ACUTE
	 (?\,Ba(B . ?\x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
	 (?\,Bb(B . ?\x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
	 (?\,Bc(B . ?\x0103) ;; LATIN SMALL LETTER A WITH BREVE
	 (?\,Bd(B . ?\x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
	 (?\,Be(B . ?\x013A) ;; LATIN SMALL LETTER L WITH ACUTE
	 (?\,Bf(B . ?\x0107) ;; LATIN SMALL LETTER C WITH ACUTE
	 (?\,Bg(B . ?\x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
	 (?\,Bh(B . ?\x010D) ;; LATIN SMALL LETTER C WITH CARON
	 (?\,Bi(B . ?\x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
	 (?\,Bj(B . ?\x0119) ;; LATIN SMALL LETTER E WITH OGONEK
	 (?\,Bk(B . ?\x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
	 (?\,Bl(B . ?\x011B) ;; LATIN SMALL LETTER E WITH CARON
	 (?\,Bm(B . ?\x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
	 (?\,Bn(B . ?\x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
	 (?\,Bo(B . ?\x010F) ;; LATIN SMALL LETTER D WITH CARON
	 (?\,Bp(B . ?\x0111) ;; LATIN SMALL LETTER D WITH STROKE
	 (?\,Bq(B . ?\x0144) ;; LATIN SMALL LETTER N WITH ACUTE
	 (?\,Br(B . ?\x0148) ;; LATIN SMALL LETTER N WITH CARON
	 (?\,Bs(B . ?\x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
	 (?\,Bt(B . ?\x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
	 (?\,Bu(B . ?\x0151) ;; LATIN SMALL LETTER O WITH DOUBLE ACUTE
	 (?\,Bv(B . ?\x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
	 (?\,Bw(B . ?\x00F7) ;; DIVISION SIGN
	 (?\,Bx(B . ?\x0159) ;; LATIN SMALL LETTER R WITH CARON
	 (?\,By(B . ?\x016F) ;; LATIN SMALL LETTER U WITH RING ABOVE
	 (?\,Bz(B . ?\x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
	 (?\,B{(B . ?\x0171) ;; LATIN SMALL LETTER U WITH DOUBLE ACUTE
	 (?\,B|(B . ?\x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
	 (?\,B}(B . ?\x00FD) ;; LATIN SMALL LETTER Y WITH ACUTE
	 (?\,B~(B . ?\x0163) ;; LATIN SMALL LETTER T WITH CEDILLA
	 (?\,B(B . ?\x02D9) ;; DOT ABOVE
	 ))

      (ucs-8859-3-alist
       '((?\,C (B . ?\x00A0) ;; NO-BREAK SPACE
	 (?\,C!(B . ?\x0126) ;; LATIN CAPITAL LETTER H WITH STROKE
	 (?\,C"(B . ?\x02D8) ;; BREVE
	 (?\,C#(B . ?\x00A3) ;; POUND SIGN
	 (?\,C$(B . ?\x00A4) ;; CURRENCY SIGN
	 (?\,C&(B . ?\x0124) ;; LATIN CAPITAL LETTER H WITH CIRCUMFLEX
	 (?\,C'(B . ?\x00A7) ;; SECTION SIGN
	 (?\,C((B . ?\x00A8) ;; DIAERESIS
	 (?\,C)(B . ?\x0130) ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
	 (?\,C*(B . ?\x015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
	 (?\,C+(B . ?\x011E) ;; LATIN CAPITAL LETTER G WITH BREVE
	 (?\,C,(B . ?\x0134) ;; LATIN CAPITAL LETTER J WITH CIRCUMFLEX
	 (?\,C-(B . ?\x00AD) ;; SOFT HYPHEN
	 (?\,C/(B . ?\x017B) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
	 (?\,C0(B . ?\x00B0) ;; DEGREE SIGN
	 (?\,C1(B . ?\x0127) ;; LATIN SMALL LETTER H WITH STROKE
	 (?\,C2(B . ?\x00B2) ;; SUPERSCRIPT TWO
	 (?\,C3(B . ?\x00B3) ;; SUPERSCRIPT THREE
	 (?\,C4(B . ?\x00B4) ;; ACUTE ACCENT
	 (?\,C5(B . ?\x00B5) ;; MICRO SIGN
	 (?\,C6(B . ?\x0125) ;; LATIN SMALL LETTER H WITH CIRCUMFLEX
	 (?\,C7(B . ?\x00B7) ;; MIDDLE DOT
	 (?\,C8(B . ?\x00B8) ;; CEDILLA
	 (?\,C9(B . ?\x0131) ;; LATIN SMALL LETTER DOTLESS I
	 (?\,C:(B . ?\x015F) ;; LATIN SMALL LETTER S WITH CEDILLA
	 (?\,C;(B . ?\x011F) ;; LATIN SMALL LETTER G WITH BREVE
	 (?\,C<(B . ?\x0135) ;; LATIN SMALL LETTER J WITH CIRCUMFLEX
	 (?\,C=(B . ?\x00BD) ;; VULGAR FRACTION ONE HALF
	 (?\,C?(B . ?\x017C) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
	 (?\,C@(B . ?\x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
	 (?\,CA(B . ?\x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
	 (?\,CB(B . ?\x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
	 (?\,CD(B . ?\x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
	 (?\,CE(B . ?\x010A) ;; LATIN CAPITAL LETTER C WITH DOT ABOVE
	 (?\,CF(B . ?\x0108) ;; LATIN CAPITAL LETTER C WITH CIRCUMFLEX
	 (?\,CG(B . ?\x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
	 (?\,CH(B . ?\x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
	 (?\,CI(B . ?\x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
	 (?\,CJ(B . ?\x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
	 (?\,CK(B . ?\x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
	 (?\,CL(B . ?\x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
	 (?\,CM(B . ?\x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
	 (?\,CN(B . ?\x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
	 (?\,CO(B . ?\x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
	 (?\,CQ(B . ?\x00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
	 (?\,CR(B . ?\x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
	 (?\,CS(B . ?\x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
	 (?\,CT(B . ?\x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	 (?\,CU(B . ?\x0120) ;; LATIN CAPITAL LETTER G WITH DOT ABOVE
	 (?\,CV(B . ?\x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
	 (?\,CW(B . ?\x00D7) ;; MULTIPLICATION SIGN
	 (?\,CX(B . ?\x011C) ;; LATIN CAPITAL LETTER G WITH CIRCUMFLEX
	 (?\,CY(B . ?\x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
	 (?\,CZ(B . ?\x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
	 (?\,C[(B . ?\x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
	 (?\,C\(B . ?\x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
	 (?\,C](B . ?\x016C) ;; LATIN CAPITAL LETTER U WITH BREVE
	 (?\,C^(B . ?\x015C) ;; LATIN CAPITAL LETTER S WITH CIRCUMFLEX
	 (?\,C_(B . ?\x00DF) ;; LATIN SMALL LETTER SHARP S
	 (?\,C`(B . ?\x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
	 (?\,Ca(B . ?\x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
	 (?\,Cb(B . ?\x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
	 (?\,Cd(B . ?\x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
	 (?\,Ce(B . ?\x010B) ;; LATIN SMALL LETTER C WITH DOT ABOVE
	 (?\,Cf(B . ?\x0109) ;; LATIN SMALL LETTER C WITH CIRCUMFLEX
	 (?\,Cg(B . ?\x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
	 (?\,Ch(B . ?\x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
	 (?\,Ci(B . ?\x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
	 (?\,Cj(B . ?\x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
	 (?\,Ck(B . ?\x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
	 (?\,Cl(B . ?\x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
	 (?\,Cm(B . ?\x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
	 (?\,Cn(B . ?\x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
	 (?\,Co(B . ?\x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
	 (?\,Cq(B . ?\x00F1) ;; LATIN SMALL LETTER N WITH TILDE
	 (?\,Cr(B . ?\x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
	 (?\,Cs(B . ?\x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
	 (?\,Ct(B . ?\x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
	 (?\,Cu(B . ?\x0121) ;; LATIN SMALL LETTER G WITH DOT ABOVE
	 (?\,Cv(B . ?\x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
	 (?\,Cw(B . ?\x00F7) ;; DIVISION SIGN
	 (?\,Cx(B . ?\x011D) ;; LATIN SMALL LETTER G WITH CIRCUMFLEX
	 (?\,Cy(B . ?\x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
	 (?\,Cz(B . ?\x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
	 (?\,C{(B . ?\x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
	 (?\,C|(B . ?\x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
	 (?\,C}(B . ?\x016D) ;; LATIN SMALL LETTER U WITH BREVE
	 (?\,C~(B . ?\x015D) ;; LATIN SMALL LETTER S WITH CIRCUMFLEX
	 (?\,C(B . ?\x02D9) ;; DOT ABOVE
	 ))

      (ucs-8859-4-alist
       '((?\,D (B . ?\x00A0) ;; NO-BREAK SPACE
	 (?\,D!(B . ?\x0104) ;; LATIN CAPITAL LETTER A WITH OGONEK
	 (?\,D"(B . ?\x0138) ;; LATIN SMALL LETTER KRA
	 (?\,D#(B . ?\x0156) ;; LATIN CAPITAL LETTER R WITH CEDILLA
	 (?\,D$(B . ?\x00A4) ;; CURRENCY SIGN
	 (?\,D%(B . ?\x0128) ;; LATIN CAPITAL LETTER I WITH TILDE
	 (?\,D&(B . ?\x013B) ;; LATIN CAPITAL LETTER L WITH CEDILLA
	 (?\,D'(B . ?\x00A7) ;; SECTION SIGN
	 (?\,D((B . ?\x00A8) ;; DIAERESIS
	 (?\,D)(B . ?\x0160) ;; LATIN CAPITAL LETTER S WITH CARON
	 (?\,D*(B . ?\x0112) ;; LATIN CAPITAL LETTER E WITH MACRON
	 (?\,D+(B . ?\x0122) ;; LATIN CAPITAL LETTER G WITH CEDILLA
	 (?\,D,(B . ?\x0166) ;; LATIN CAPITAL LETTER T WITH STROKE
	 (?\,D-(B . ?\x00AD) ;; SOFT HYPHEN
	 (?\,D.(B . ?\x017D) ;; LATIN CAPITAL LETTER Z WITH CARON
	 (?\,D/(B . ?\x00AF) ;; MACRON
	 (?\,D0(B . ?\x00B0) ;; DEGREE SIGN
	 (?\,D1(B . ?\x0105) ;; LATIN SMALL LETTER A WITH OGONEK
	 (?\,D2(B . ?\x02DB) ;; OGONEK
	 (?\,D3(B . ?\x0157) ;; LATIN SMALL LETTER R WITH CEDILLA
	 (?\,D4(B . ?\x00B4) ;; ACUTE ACCENT
	 (?\,D5(B . ?\x0129) ;; LATIN SMALL LETTER I WITH TILDE
	 (?\,D6(B . ?\x013C) ;; LATIN SMALL LETTER L WITH CEDILLA
	 (?\,D7(B . ?\x02C7) ;; CARON
	 (?\,D8(B . ?\x00B8) ;; CEDILLA
	 (?\,D9(B . ?\x0161) ;; LATIN SMALL LETTER S WITH CARON
	 (?\,D:(B . ?\x0113) ;; LATIN SMALL LETTER E WITH MACRON
	 (?\,D;(B . ?\x0123) ;; LATIN SMALL LETTER G WITH CEDILLA
	 (?\,D<(B . ?\x0167) ;; LATIN SMALL LETTER T WITH STROKE
	 (?\,D=(B . ?\x014A) ;; LATIN CAPITAL LETTER ENG
	 (?\,D>(B . ?\x017E) ;; LATIN SMALL LETTER Z WITH CARON
	 (?\,D?(B . ?\x014B) ;; LATIN SMALL LETTER ENG
	 (?\,D@(B . ?\x0100) ;; LATIN CAPITAL LETTER A WITH MACRON
	 (?\,DA(B . ?\x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
	 (?\,DB(B . ?\x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
	 (?\,DC(B . ?\x00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
	 (?\,DD(B . ?\x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
	 (?\,DE(B . ?\x00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
	 (?\,DF(B . ?\x00C6) ;; LATIN CAPITAL LETTER AE
	 (?\,DG(B . ?\x012E) ;; LATIN CAPITAL LETTER I WITH OGONEK
	 (?\,DH(B . ?\x010C) ;; LATIN CAPITAL LETTER C WITH CARON
	 (?\,DI(B . ?\x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
	 (?\,DJ(B . ?\x0118) ;; LATIN CAPITAL LETTER E WITH OGONEK
	 (?\,DK(B . ?\x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
	 (?\,DL(B . ?\x0116) ;; LATIN CAPITAL LETTER E WITH DOT ABOVE
	 (?\,DM(B . ?\x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
	 (?\,DN(B . ?\x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
	 (?\,DO(B . ?\x012A) ;; LATIN CAPITAL LETTER I WITH MACRON
	 (?\,DP(B . ?\x0110) ;; LATIN CAPITAL LETTER D WITH STROKE
	 (?\,DQ(B . ?\x0145) ;; LATIN CAPITAL LETTER N WITH CEDILLA
	 (?\,DR(B . ?\x014C) ;; LATIN CAPITAL LETTER O WITH MACRON
	 (?\,DS(B . ?\x0136) ;; LATIN CAPITAL LETTER K WITH CEDILLA
	 (?\,DT(B . ?\x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	 (?\,DU(B . ?\x00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
	 (?\,DV(B . ?\x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
	 (?\,DW(B . ?\x00D7) ;; MULTIPLICATION SIGN
	 (?\,DX(B . ?\x00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
	 (?\,DY(B . ?\x0172) ;; LATIN CAPITAL LETTER U WITH OGONEK
	 (?\,DZ(B . ?\x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
	 (?\,D[(B . ?\x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
	 (?\,D\(B . ?\x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
	 (?\,D](B . ?\x0168) ;; LATIN CAPITAL LETTER U WITH TILDE
	 (?\,D^(B . ?\x016A) ;; LATIN CAPITAL LETTER U WITH MACRON
	 (?\,D_(B . ?\x00DF) ;; LATIN SMALL LETTER SHARP S
	 (?\,D`(B . ?\x0101) ;; LATIN SMALL LETTER A WITH MACRON
	 (?\,Da(B . ?\x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
	 (?\,Db(B . ?\x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
	 (?\,Dc(B . ?\x00E3) ;; LATIN SMALL LETTER A WITH TILDE
	 (?\,Dd(B . ?\x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
	 (?\,De(B . ?\x00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
	 (?\,Df(B . ?\x00E6) ;; LATIN SMALL LETTER AE
	 (?\,Dg(B . ?\x012F) ;; LATIN SMALL LETTER I WITH OGONEK
	 (?\,Dh(B . ?\x010D) ;; LATIN SMALL LETTER C WITH CARON
	 (?\,Di(B . ?\x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
	 (?\,Dj(B . ?\x0119) ;; LATIN SMALL LETTER E WITH OGONEK
	 (?\,Dk(B . ?\x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
	 (?\,Dl(B . ?\x0117) ;; LATIN SMALL LETTER E WITH DOT ABOVE
	 (?\,Dm(B . ?\x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
	 (?\,Dn(B . ?\x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
	 (?\,Do(B . ?\x012B) ;; LATIN SMALL LETTER I WITH MACRON
	 (?\,Dp(B . ?\x0111) ;; LATIN SMALL LETTER D WITH STROKE
	 (?\,Dq(B . ?\x0146) ;; LATIN SMALL LETTER N WITH CEDILLA
	 (?\,Dr(B . ?\x014D) ;; LATIN SMALL LETTER O WITH MACRON
	 (?\,Ds(B . ?\x0137) ;; LATIN SMALL LETTER K WITH CEDILLA
	 (?\,Dt(B . ?\x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
	 (?\,Du(B . ?\x00F5) ;; LATIN SMALL LETTER O WITH TILDE
	 (?\,Dv(B . ?\x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
	 (?\,Dw(B . ?\x00F7) ;; DIVISION SIGN
	 (?\,Dx(B . ?\x00F8) ;; LATIN SMALL LETTER O WITH STROKE
	 (?\,Dy(B . ?\x0173) ;; LATIN SMALL LETTER U WITH OGONEK
	 (?\,Dz(B . ?\x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
	 (?\,D{(B . ?\x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
	 (?\,D|(B . ?\x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
	 (?\,D}(B . ?\x0169) ;; LATIN SMALL LETTER U WITH TILDE
	 (?\,D~(B . ?\x016B) ;; LATIN SMALL LETTER U WITH MACRON
	 (?\,D(B . ?\x02D9) ;; DOT ABOVE
	 ))

      (ucs-8859-5-alist
       '((?\,L (B . ?\x00A0) ;; NO-BREAK SPACE
	 (?\,L!(B . ?\x0401) ;; CYRILLIC CAPITAL LETTER IO
	 (?\,L"(B . ?\x0402) ;; CYRILLIC CAPITAL LETTER DJE
	 (?\,L#(B . ?\x0403) ;; CYRILLIC CAPITAL LETTER GJE
	 (?\,L$(B . ?\x0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
	 (?\,L%(B . ?\x0405) ;; CYRILLIC CAPITAL LETTER DZE
	 (?\,L&(B . ?\x0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
	 (?\,L'(B . ?\x0407) ;; CYRILLIC CAPITAL LETTER YI
	 (?\,L((B . ?\x0408) ;; CYRILLIC CAPITAL LETTER JE
	 (?\,L)(B . ?\x0409) ;; CYRILLIC CAPITAL LETTER LJE
	 (?\,L*(B . ?\x040A) ;; CYRILLIC CAPITAL LETTER NJE
	 (?\,L+(B . ?\x040B) ;; CYRILLIC CAPITAL LETTER TSHE
	 (?\,L,(B . ?\x040C) ;; CYRILLIC CAPITAL LETTER KJE
	 (?\,L-(B . ?\x00AD) ;; SOFT HYPHEN
	 (?\,L.(B . ?\x040E) ;; CYRILLIC CAPITAL LETTER SHORT U
	 (?\,L/(B . ?\x040F) ;; CYRILLIC CAPITAL LETTER DZHE
	 (?\,L0(B . ?\x0410) ;; CYRILLIC CAPITAL LETTER A
	 (?\,L1(B . ?\x0411) ;; CYRILLIC CAPITAL LETTER BE
	 (?\,L2(B . ?\x0412) ;; CYRILLIC CAPITAL LETTER VE
	 (?\,L3(B . ?\x0413) ;; CYRILLIC CAPITAL LETTER GHE
	 (?\,L4(B . ?\x0414) ;; CYRILLIC CAPITAL LETTER DE
	 (?\,L5(B . ?\x0415) ;; CYRILLIC CAPITAL LETTER IE
	 (?\,L6(B . ?\x0416) ;; CYRILLIC CAPITAL LETTER ZHE
	 (?\,L7(B . ?\x0417) ;; CYRILLIC CAPITAL LETTER ZE
	 (?\,L8(B . ?\x0418) ;; CYRILLIC CAPITAL LETTER I
	 (?\,L9(B . ?\x0419) ;; CYRILLIC CAPITAL LETTER SHORT I
	 (?\,L:(B . ?\x041A) ;; CYRILLIC CAPITAL LETTER KA
	 (?\,L;(B . ?\x041B) ;; CYRILLIC CAPITAL LETTER EL
	 (?\,L<(B . ?\x041C) ;; CYRILLIC CAPITAL LETTER EM
	 (?\,L=(B . ?\x041D) ;; CYRILLIC CAPITAL LETTER EN
	 (?\,L>(B . ?\x041E) ;; CYRILLIC CAPITAL LETTER O
	 (?\,L?(B . ?\x041F) ;; CYRILLIC CAPITAL LETTER PE
	 (?\,L@(B . ?\x0420) ;; CYRILLIC CAPITAL LETTER ER
	 (?\,LA(B . ?\x0421) ;; CYRILLIC CAPITAL LETTER ES
	 (?\,LB(B . ?\x0422) ;; CYRILLIC CAPITAL LETTER TE
	 (?\,LC(B . ?\x0423) ;; CYRILLIC CAPITAL LETTER U
	 (?\,LD(B . ?\x0424) ;; CYRILLIC CAPITAL LETTER EF
	 (?\,LE(B . ?\x0425) ;; CYRILLIC CAPITAL LETTER HA
	 (?\,LF(B . ?\x0426) ;; CYRILLIC CAPITAL LETTER TSE
	 (?\,LG(B . ?\x0427) ;; CYRILLIC CAPITAL LETTER CHE
	 (?\,LH(B . ?\x0428) ;; CYRILLIC CAPITAL LETTER SHA
	 (?\,LI(B . ?\x0429) ;; CYRILLIC CAPITAL LETTER SHCHA
	 (?\,LJ(B . ?\x042A) ;; CYRILLIC CAPITAL LETTER HARD SIGN
	 (?\,LK(B . ?\x042B) ;; CYRILLIC CAPITAL LETTER YERU
	 (?\,LL(B . ?\x042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
	 (?\,LM(B . ?\x042D) ;; CYRILLIC CAPITAL LETTER E
	 (?\,LN(B . ?\x042E) ;; CYRILLIC CAPITAL LETTER YU
	 (?\,LO(B . ?\x042F) ;; CYRILLIC CAPITAL LETTER YA
	 (?\,LP(B . ?\x0430) ;; CYRILLIC SMALL LETTER A
	 (?\,LQ(B . ?\x0431) ;; CYRILLIC SMALL LETTER BE
	 (?\,LR(B . ?\x0432) ;; CYRILLIC SMALL LETTER VE
	 (?\,LS(B . ?\x0433) ;; CYRILLIC SMALL LETTER GHE
	 (?\,LT(B . ?\x0434) ;; CYRILLIC SMALL LETTER DE
	 (?\,LU(B . ?\x0435) ;; CYRILLIC SMALL LETTER IE
	 (?\,LV(B . ?\x0436) ;; CYRILLIC SMALL LETTER ZHE
	 (?\,LW(B . ?\x0437) ;; CYRILLIC SMALL LETTER ZE
	 (?\,LX(B . ?\x0438) ;; CYRILLIC SMALL LETTER I
	 (?\,LY(B . ?\x0439) ;; CYRILLIC SMALL LETTER SHORT I
	 (?\,LZ(B . ?\x043A) ;; CYRILLIC SMALL LETTER KA
	 (?\,L[(B . ?\x043B) ;; CYRILLIC SMALL LETTER EL
	 (?\,L\(B . ?\x043C) ;; CYRILLIC SMALL LETTER EM
	 (?\,L](B . ?\x043D) ;; CYRILLIC SMALL LETTER EN
	 (?\,L^(B . ?\x043E) ;; CYRILLIC SMALL LETTER O
	 (?\,L_(B . ?\x043F) ;; CYRILLIC SMALL LETTER PE
	 (?\,L`(B . ?\x0440) ;; CYRILLIC SMALL LETTER ER
	 (?\,La(B . ?\x0441) ;; CYRILLIC SMALL LETTER ES
	 (?\,Lb(B . ?\x0442) ;; CYRILLIC SMALL LETTER TE
	 (?\,Lc(B . ?\x0443) ;; CYRILLIC SMALL LETTER U
	 (?\,Ld(B . ?\x0444) ;; CYRILLIC SMALL LETTER EF
	 (?\,Le(B . ?\x0445) ;; CYRILLIC SMALL LETTER HA
	 (?\,Lf(B . ?\x0446) ;; CYRILLIC SMALL LETTER TSE
	 (?\,Lg(B . ?\x0447) ;; CYRILLIC SMALL LETTER CHE
	 (?\,Lh(B . ?\x0448) ;; CYRILLIC SMALL LETTER SHA
	 (?\,Li(B . ?\x0449) ;; CYRILLIC SMALL LETTER SHCHA
	 (?\,Lj(B . ?\x044A) ;; CYRILLIC SMALL LETTER HARD SIGN
	 (?\,Lk(B . ?\x044B) ;; CYRILLIC SMALL LETTER YERU
	 (?\,Ll(B . ?\x044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
	 (?\,Lm(B . ?\x044D) ;; CYRILLIC SMALL LETTER E
	 (?\,Ln(B . ?\x044E) ;; CYRILLIC SMALL LETTER YU
	 (?\,Lo(B . ?\x044F) ;; CYRILLIC SMALL LETTER YA
	 (?\,Lp(B . ?\x2116) ;; NUMERO SIGN
	 (?\,Lq(B . ?\x0451) ;; CYRILLIC SMALL LETTER IO
	 (?\,Lr(B . ?\x0452) ;; CYRILLIC SMALL LETTER DJE
	 (?\,Ls(B . ?\x0453) ;; CYRILLIC SMALL LETTER GJE
	 (?\,Lt(B . ?\x0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
	 (?\,Lu(B . ?\x0455) ;; CYRILLIC SMALL LETTER DZE
	 (?\,Lv(B . ?\x0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
	 (?\,Lw(B . ?\x0457) ;; CYRILLIC SMALL LETTER YI
	 (?\,Lx(B . ?\x0458) ;; CYRILLIC SMALL LETTER JE
	 (?\,Ly(B . ?\x0459) ;; CYRILLIC SMALL LETTER LJE
	 (?\,Lz(B . ?\x045A) ;; CYRILLIC SMALL LETTER NJE
	 (?\,L{(B . ?\x045B) ;; CYRILLIC SMALL LETTER TSHE
	 (?\,L|(B . ?\x045C) ;; CYRILLIC SMALL LETTER KJE
	 (?\,L}(B . ?\x00A7) ;; SECTION SIGN
	 (?\,L~(B . ?\x045E) ;; CYRILLIC SMALL LETTER SHORT U
	 (?\,L(B . ?\x045F) ;; CYRILLIC SMALL LETTER DZHE
	 ))

      ;; Arabic probably isn't so useful in the absence of Arabic
      ;; language support.
      (ucs-8859-6-alist
       '((?,G (B . ?\x00A0)	;; NO-BREAK SPACE
	 (?,G$(B . ?\x00A4)	;; CURRENCY SIGN
	 (?,G,(B . ?\x060C) ;; ARABIC COMMA
	 (?,G-(B . ?\x00AD)	;; SOFT HYPHEN
	 (?,G;(B . ?\x061B) ;; ARABIC SEMICOLON
	 (?,G?(B . ?\x061F) ;; ARABIC QUESTION MARK
	 (?,GA(B . ?\x0621) ;; ARABIC LETTER HAMZA
	 (?,GB(B . ?\x0622) ;; ARABIC LETTER ALEF WITH MADDA ABOVE
	 (?,GC(B . ?\x0623) ;; ARABIC LETTER ALEF WITH HAMZA ABOVE
	 (?,GD(B . ?\x0624) ;; ARABIC LETTER WAW WITH HAMZA ABOVE
	 (?,GE(B . ?\x0625) ;; ARABIC LETTER ALEF WITH HAMZA BELOW
	 (?,GF(B . ?\x0626) ;; ARABIC LETTER YEH WITH HAMZA ABOVE
	 (?,GG(B . ?\x0627) ;; ARABIC LETTER ALEF
	 (?,GH(B . ?\x0628) ;; ARABIC LETTER BEH
	 (?,GI(B . ?\x0629) ;; ARABIC LETTER TEH MARBUTA
	 (?,GJ(B . ?\x062A) ;; ARABIC LETTER TEH
	 (?,GK(B . ?\x062B) ;; ARABIC LETTER THEH
	 (?,GL(B . ?\x062C) ;; ARABIC LETTER JEEM
	 (?,GM(B . ?\x062D) ;; ARABIC LETTER HAH
	 (?,GN(B . ?\x062E) ;; ARABIC LETTER KHAH
	 (?,GO(B . ?\x062F) ;; ARABIC LETTER DAL
	 (?,GP(B . ?\x0630) ;; ARABIC LETTER THAL
	 (?,GQ(B . ?\x0631) ;; ARABIC LETTER REH
	 (?,GR(B . ?\x0632) ;; ARABIC LETTER ZAIN
	 (?,GS(B . ?\x0633) ;; ARABIC LETTER SEEN
	 (?,GT(B . ?\x0634) ;; ARABIC LETTER SHEEN
	 (?,GU(B . ?\x0635) ;; ARABIC LETTER SAD
	 (?,GV(B . ?\x0636) ;; ARABIC LETTER DAD
	 (?,GW(B . ?\x0637) ;; ARABIC LETTER TAH
	 (?,GX(B . ?\x0638) ;; ARABIC LETTER ZAH
	 (?,GY(B . ?\x0639) ;; ARABIC LETTER AIN
	 (?,GZ(B . ?\x063A) ;; ARABIC LETTER GHAIN
	 (?,G`(B . ?\x0640) ;; ARABIC TATWEEL
	 (?,Ga(B . ?\x0641) ;; ARABIC LETTER FEH
	 (?,Gb(B . ?\x0642) ;; ARABIC LETTER QAF
	 (?,Gc(B . ?\x0643) ;; ARABIC LETTER KAF
	 (?,Gd(B . ?\x0644) ;; ARABIC LETTER LAM
	 (?,Ge(B . ?\x0645) ;; ARABIC LETTER MEEM
	 (?,Gf(B . ?\x0646) ;; ARABIC LETTER NOON
	 (?,Gg(B . ?\x0647) ;; ARABIC LETTER HEH
	 (?,Gh(B . ?\x0648) ;; ARABIC LETTER WAW
	 (?,Gi(B . ?\x0649) ;; ARABIC LETTER ALEF MAKSURA
	 (?,Gj(B . ?\x064A) ;; ARABIC LETTER YEH
	 (?,Gk(B . ?\x064B) ;; ARABIC FATHATAN
	 (?,Gl(B . ?\x064C) ;; ARABIC DAMMATAN
	 (?,Gm(B . ?\x064D) ;; ARABIC KASRATAN
	 (?,Gn(B . ?\x064E) ;; ARABIC FATHA
	 (?,Go(B . ?\x064F) ;; ARABIC DAMMA
	 (?,Gp(B . ?\x0650) ;; ARABIC KASRA
	 (?,Gq(B . ?\x0651) ;; ARABIC SHADDA
	 (?,Gr(B . ?\x0652) ;; ARABIC SUKUN
	 ))

      (ucs-8859-7-alist
       '((?\,F (B . ?\x00A0) ;; NO-BREAK SPACE
	 (?\,F!(B . ?\x2018) ;; LEFT SINGLE QUOTATION MARK
	 (?\,F"(B . ?\x2019) ;; RIGHT SINGLE QUOTATION MARK
	 (?\,F#(B . ?\x00A3) ;; POUND SIGN
	 (?\,F&(B . ?\x00A6) ;; BROKEN BAR
	 (?\,F'(B . ?\x00A7) ;; SECTION SIGN
	 (?\,F((B . ?\x00A8) ;; DIAERESIS
	 (?\,F)(B . ?\x00A9) ;; COPYRIGHT SIGN
	 (?\,F+(B . ?\x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
	 (?\,F,(B . ?\x00AC) ;; NOT SIGN
	 (?\,F-(B . ?\x00AD) ;; SOFT HYPHEN
	 (?\,F/(B . ?\x2015) ;; HORIZONTAL BAR
	 (?\,F0(B . ?\x00B0) ;; DEGREE SIGN
	 (?\,F1(B . ?\x00B1) ;; PLUS-MINUS SIGN
	 (?\,F2(B . ?\x00B2) ;; SUPERSCRIPT TWO
	 (?\,F3(B . ?\x00B3) ;; SUPERSCRIPT THREE
	 (?\,F4(B . ?\x0384) ;; GREEK TONOS
	 (?\,F5(B . ?\x0385) ;; GREEK DIALYTIKA TONOS
	 (?\,F6(B . ?\x0386) ;; GREEK CAPITAL LETTER ALPHA WITH TONOS
	 (?\,F7(B . ?\x00B7) ;; MIDDLE DOT
	 (?\,F8(B . ?\x0388) ;; GREEK CAPITAL LETTER EPSILON WITH TONOS
	 (?\,F9(B . ?\x0389) ;; GREEK CAPITAL LETTER ETA WITH TONOS
	 (?\,F:(B . ?\x038A) ;; GREEK CAPITAL LETTER IOTA WITH TONOS
	 (?\,F;(B . ?\x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
	 (?\,F<(B . ?\x038C) ;; GREEK CAPITAL LETTER OMICRON WITH TONOS
	 (?\,F=(B . ?\x00BD) ;; VULGAR FRACTION ONE HALF
	 (?\,F>(B . ?\x038E) ;; GREEK CAPITAL LETTER UPSILON WITH TONOS
	 (?\,F?(B . ?\x038F) ;; GREEK CAPITAL LETTER OMEGA WITH TONOS
	 (?\,F@(B . ?\x0390) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
	 (?\,FA(B . ?\x0391) ;; GREEK CAPITAL LETTER ALPHA
	 (?\,FB(B . ?\x0392) ;; GREEK CAPITAL LETTER BETA
	 (?\,FC(B . ?\x0393) ;; GREEK CAPITAL LETTER GAMMA
	 (?\,FD(B . ?\x0394) ;; GREEK CAPITAL LETTER DELTA
	 (?\,FE(B . ?\x0395) ;; GREEK CAPITAL LETTER EPSILON
	 (?\,FF(B . ?\x0396) ;; GREEK CAPITAL LETTER ZETA
	 (?\,FG(B . ?\x0397) ;; GREEK CAPITAL LETTER ETA
	 (?\,FH(B . ?\x0398) ;; GREEK CAPITAL LETTER THETA
	 (?\,FI(B . ?\x0399) ;; GREEK CAPITAL LETTER IOTA
	 (?\,FJ(B . ?\x039A) ;; GREEK CAPITAL LETTER KAPPA
	 (?\,FK(B . ?\x039B) ;; GREEK CAPITAL LETTER LAMDA
	 (?\,FL(B . ?\x039C) ;; GREEK CAPITAL LETTER MU
	 (?\,FM(B . ?\x039D) ;; GREEK CAPITAL LETTER NU
	 (?\,FN(B . ?\x039E) ;; GREEK CAPITAL LETTER XI
	 (?\,FO(B . ?\x039F) ;; GREEK CAPITAL LETTER OMICRON
	 (?\,FP(B . ?\x03A0) ;; GREEK CAPITAL LETTER PI
	 (?\,FQ(B . ?\x03A1) ;; GREEK CAPITAL LETTER RHO
	 (?\,FS(B . ?\x03A3) ;; GREEK CAPITAL LETTER SIGMA
	 (?\,FT(B . ?\x03A4) ;; GREEK CAPITAL LETTER TAU
	 (?\,FU(B . ?\x03A5) ;; GREEK CAPITAL LETTER UPSILON
	 (?\,FV(B . ?\x03A6) ;; GREEK CAPITAL LETTER PHI
	 (?\,FW(B . ?\x03A7) ;; GREEK CAPITAL LETTER CHI
	 (?\,FX(B . ?\x03A8) ;; GREEK CAPITAL LETTER PSI
	 (?\,FY(B . ?\x03A9) ;; GREEK CAPITAL LETTER OMEGA
	 (?\,FZ(B . ?\x03AA) ;; GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
	 (?\,F[(B . ?\x03AB) ;; GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
	 (?\,F\(B . ?\x03AC) ;; GREEK SMALL LETTER ALPHA WITH TONOS
	 (?\,F](B . ?\x03AD) ;; GREEK SMALL LETTER EPSILON WITH TONOS
	 (?\,F^(B . ?\x03AE) ;; GREEK SMALL LETTER ETA WITH TONOS
	 (?\,F_(B . ?\x03AF) ;; GREEK SMALL LETTER IOTA WITH TONOS
	 (?\,F`(B . ?\x03B0) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
	 (?\,Fa(B . ?\x03B1) ;; GREEK SMALL LETTER ALPHA
	 (?\,Fb(B . ?\x03B2) ;; GREEK SMALL LETTER BETA
	 (?\,Fc(B . ?\x03B3) ;; GREEK SMALL LETTER GAMMA
	 (?\,Fd(B . ?\x03B4) ;; GREEK SMALL LETTER DELTA
	 (?\,Fe(B . ?\x03B5) ;; GREEK SMALL LETTER EPSILON
	 (?\,Ff(B . ?\x03B6) ;; GREEK SMALL LETTER ZETA
	 (?\,Fg(B . ?\x03B7) ;; GREEK SMALL LETTER ETA
	 (?\,Fh(B . ?\x03B8) ;; GREEK SMALL LETTER THETA
	 (?\,Fi(B . ?\x03B9) ;; GREEK SMALL LETTER IOTA
	 (?\,Fj(B . ?\x03BA) ;; GREEK SMALL LETTER KAPPA
	 (?\,Fk(B . ?\x03BB) ;; GREEK SMALL LETTER LAMDA
	 (?\,Fl(B . ?\x03BC) ;; GREEK SMALL LETTER MU
	 (?\,Fm(B . ?\x03BD) ;; GREEK SMALL LETTER NU
	 (?\,Fn(B . ?\x03BE) ;; GREEK SMALL LETTER XI
	 (?\,Fo(B . ?\x03BF) ;; GREEK SMALL LETTER OMICRON
	 (?\,Fp(B . ?\x03C0) ;; GREEK SMALL LETTER PI
	 (?\,Fq(B . ?\x03C1) ;; GREEK SMALL LETTER RHO
	 (?\,Fr(B . ?\x03C2) ;; GREEK SMALL LETTER FINAL SIGMA
	 (?\,Fs(B . ?\x03C3) ;; GREEK SMALL LETTER SIGMA
	 (?\,Ft(B . ?\x03C4) ;; GREEK SMALL LETTER TAU
	 (?\,Fu(B . ?\x03C5) ;; GREEK SMALL LETTER UPSILON
	 (?\,Fv(B . ?\x03C6) ;; GREEK SMALL LETTER PHI
	 (?\,Fw(B . ?\x03C7) ;; GREEK SMALL LETTER CHI
	 (?\,Fx(B . ?\x03C8) ;; GREEK SMALL LETTER PSI
	 (?\,Fy(B . ?\x03C9) ;; GREEK SMALL LETTER OMEGA
	 (?\,Fz(B . ?\x03CA) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA
	 (?\,F{(B . ?\x03CB) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA
	 (?\,F|(B . ?\x03CC) ;; GREEK SMALL LETTER OMICRON WITH TONOS
	 (?\,F}(B . ?\x03CD) ;; GREEK SMALL LETTER UPSILON WITH TONOS
	 (?\,F~(B . ?\x03CE) ;; GREEK SMALL LETTER OMEGA WITH TONOS
	 ))

      (ucs-8859-8-alist
       '((?\,H (B . ?\x00A0) ;; NO-BREAK SPACE
	 (?\,H"(B . ?\x00A2) ;; CENT SIGN
	 (?\,H#(B . ?\x00A3) ;; POUND SIGN
	 (?\,H$(B . ?\x00A4) ;; CURRENCY SIGN
	 (?\,H%(B . ?\x00A5) ;; YEN SIGN
	 (?\,H&(B . ?\x00A6) ;; BROKEN BAR
	 (?\,H'(B . ?\x00A7) ;; SECTION SIGN
	 (?\,H((B . ?\x00A8) ;; DIAERESIS
	 (?\,H)(B . ?\x00A9) ;; COPYRIGHT SIGN
	 (?\,H*(B . ?\x00D7) ;; MULTIPLICATION SIGN
	 (?\,H+(B . ?\x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
	 (?\,H,(B . ?\x00AC) ;; NOT SIGN
	 (?\,H-(B . ?\x00AD) ;; SOFT HYPHEN
	 (?\,H.(B . ?\x00AE) ;; REGISTERED SIGN
	 (?\,H/(B . ?\x00AF) ;; MACRON
	 (?\,H0(B . ?\x00B0) ;; DEGREE SIGN
	 (?\,H1(B . ?\x00B1) ;; PLUS-MINUS SIGN
	 (?\,H2(B . ?\x00B2) ;; SUPERSCRIPT TWO
	 (?\,H3(B . ?\x00B3) ;; SUPERSCRIPT THREE
	 (?\,H4(B . ?\x00B4) ;; ACUTE ACCENT
	 (?\,H5(B . ?\x00B5) ;; MICRO SIGN
	 (?\,H6(B . ?\x00B6) ;; PILCROW SIGN
	 (?\,H7(B . ?\x00B7) ;; MIDDLE DOT
	 (?\,H8(B . ?\x00B8) ;; CEDILLA
	 (?\,H9(B . ?\x00B9) ;; SUPERSCRIPT ONE
	 (?\,H:(B . ?\x00F7) ;; DIVISION SIGN
	 (?\,H;(B . ?\x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
	 (?\,H<(B . ?\x00BC) ;; VULGAR FRACTION ONE QUARTER
	 (?\,H=(B . ?\x00BD) ;; VULGAR FRACTION ONE HALF
	 (?\,H>(B . ?\x00BE) ;; VULGAR FRACTION THREE QUARTERS
	 (?\,H_(B . ?\x2017) ;; DOUBLE LOW LINE
	 (?\,H`(B . ?\x05D0) ;; HEBREW LETTER ALEF
	 (?\,Ha(B . ?\x05D1) ;; HEBREW LETTER BET
	 (?\,Hb(B . ?\x05D2) ;; HEBREW LETTER GIMEL
	 (?\,Hc(B . ?\x05D3) ;; HEBREW LETTER DALET
	 (?\,Hd(B . ?\x05D4) ;; HEBREW LETTER HE
	 (?\,He(B . ?\x05D5) ;; HEBREW LETTER VAV
	 (?\,Hf(B . ?\x05D6) ;; HEBREW LETTER ZAYIN
	 (?\,Hg(B . ?\x05D7) ;; HEBREW LETTER HET
	 (?\,Hh(B . ?\x05D8) ;; HEBREW LETTER TET
	 (?\,Hi(B . ?\x05D9) ;; HEBREW LETTER YOD
	 (?\,Hj(B . ?\x05DA) ;; HEBREW LETTER FINAL KAF
	 (?\,Hk(B . ?\x05DB) ;; HEBREW LETTER KAF
	 (?\,Hl(B . ?\x05DC) ;; HEBREW LETTER LAMED
	 (?\,Hm(B . ?\x05DD) ;; HEBREW LETTER FINAL MEM
	 (?\,Hn(B . ?\x05DE) ;; HEBREW LETTER MEM
	 (?\,Ho(B . ?\x05DF) ;; HEBREW LETTER FINAL NUN
	 (?\,Hp(B . ?\x05E0) ;; HEBREW LETTER NUN
	 (?\,Hq(B . ?\x05E1) ;; HEBREW LETTER SAMEKH
	 (?\,Hr(B . ?\x05E2) ;; HEBREW LETTER AYIN
	 (?\,Hs(B . ?\x05E3) ;; HEBREW LETTER FINAL PE
	 (?\,Ht(B . ?\x05E4) ;; HEBREW LETTER PE
	 (?\,Hu(B . ?\x05E5) ;; HEBREW LETTER FINAL TSADI
	 (?\,Hv(B . ?\x05E6) ;; HEBREW LETTER TSADI
	 (?\,Hw(B . ?\x05E7) ;; HEBREW LETTER QOF
	 (?\,Hx(B . ?\x05E8) ;; HEBREW LETTER RESH
	 (?\,Hy(B . ?\x05E9) ;; HEBREW LETTER SHIN
	 (?\,Hz(B . ?\x05EA) ;; HEBREW LETTER TAV
	 (?\,H}(B . ?\x200E) ;; LEFT-TO-RIGHT MARK
	 (?\,H~(B . ?\x200F) ;; RIGHT-TO-LEFT MARK
	 ))

      (ucs-8859-9-alist
       '((?\,M (B . ?\x00A0) ;; NO-BREAK SPACE
	 (?\,M!(B . ?\x00A1) ;; INVERTED EXCLAMATION MARK
	 (?\,M"(B . ?\x00A2) ;; CENT SIGN
	 (?\,M#(B . ?\x00A3) ;; POUND SIGN
	 (?\,M$(B . ?\x00A4) ;; CURRENCY SIGN
	 (?\,M%(B . ?\x00A5) ;; YEN SIGN
	 (?\,M&(B . ?\x00A6) ;; BROKEN BAR
	 (?\,M'(B . ?\x00A7) ;; SECTION SIGN
	 (?\,M((B . ?\x00A8) ;; DIAERESIS
	 (?\,M)(B . ?\x00A9) ;; COPYRIGHT SIGN
	 (?\,M*(B . ?\x00AA) ;; FEMININE ORDINAL INDICATOR
	 (?\,M+(B . ?\x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
	 (?\,M,(B . ?\x00AC) ;; NOT SIGN
	 (?\,M-(B . ?\x00AD) ;; SOFT HYPHEN
	 (?\,M.(B . ?\x00AE) ;; REGISTERED SIGN
	 (?\,M/(B . ?\x00AF) ;; MACRON
	 (?\,M0(B . ?\x00B0) ;; DEGREE SIGN
	 (?\,M1(B . ?\x00B1) ;; PLUS-MINUS SIGN
	 (?\,M2(B . ?\x00B2) ;; SUPERSCRIPT TWO
	 (?\,M3(B . ?\x00B3) ;; SUPERSCRIPT THREE
	 (?\,M4(B . ?\x00B4) ;; ACUTE ACCENT
	 (?\,M5(B . ?\x00B5) ;; MICRO SIGN
	 (?\,M6(B . ?\x00B6) ;; PILCROW SIGN
	 (?\,M7(B . ?\x00B7) ;; MIDDLE DOT
	 (?\,M8(B . ?\x00B8) ;; CEDILLA
	 (?\,M9(B . ?\x00B9) ;; SUPERSCRIPT ONE
	 (?\,M:(B . ?\x00BA) ;; MASCULINE ORDINAL INDICATOR
	 (?\,M;(B . ?\x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
	 (?\,M<(B . ?\x00BC) ;; VULGAR FRACTION ONE QUARTER
	 (?\,M=(B . ?\x00BD) ;; VULGAR FRACTION ONE HALF
	 (?\,M>(B . ?\x00BE) ;; VULGAR FRACTION THREE QUARTERS
	 (?\,M?(B . ?\x00BF) ;; INVERTED QUESTION MARK
	 (?\,M@(B . ?\x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
	 (?\,MA(B . ?\x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
	 (?\,MB(B . ?\x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
	 (?\,MC(B . ?\x00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
	 (?\,MD(B . ?\x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
	 (?\,ME(B . ?\x00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
	 (?\,MF(B . ?\x00C6) ;; LATIN CAPITAL LETTER AE
	 (?\,MG(B . ?\x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
	 (?\,MH(B . ?\x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
	 (?\,MI(B . ?\x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
	 (?\,MJ(B . ?\x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
	 (?\,MK(B . ?\x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
	 (?\,ML(B . ?\x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
	 (?\,MM(B . ?\x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
	 (?\,MN(B . ?\x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
	 (?\,MO(B . ?\x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
	 (?\,MP(B . ?\x011E) ;; LATIN CAPITAL LETTER G WITH BREVE
	 (?\,MQ(B . ?\x00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
	 (?\,MR(B . ?\x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
	 (?\,MS(B . ?\x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
	 (?\,MT(B . ?\x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	 (?\,MU(B . ?\x00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
	 (?\,MV(B . ?\x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
	 (?\,MW(B . ?\x00D7) ;; MULTIPLICATION SIGN
	 (?\,MX(B . ?\x00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
	 (?\,MY(B . ?\x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
	 (?\,MZ(B . ?\x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
	 (?\,M[(B . ?\x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
	 (?\,M\(B . ?\x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
	 (?\,M](B . ?\x0130) ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
	 (?\,M^(B . ?\x015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
	 (?\,M_(B . ?\x00DF) ;; LATIN SMALL LETTER SHARP S
	 (?\,M`(B . ?\x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
	 (?\,Ma(B . ?\x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
	 (?\,Mb(B . ?\x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
	 (?\,Mc(B . ?\x00E3) ;; LATIN SMALL LETTER A WITH TILDE
	 (?\,Md(B . ?\x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
	 (?\,Me(B . ?\x00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
	 (?\,Mf(B . ?\x00E6) ;; LATIN SMALL LETTER AE
	 (?\,Mg(B . ?\x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
	 (?\,Mh(B . ?\x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
	 (?\,Mi(B . ?\x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
	 (?\,Mj(B . ?\x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
	 (?\,Mk(B . ?\x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
	 (?\,Ml(B . ?\x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
	 (?\,Mm(B . ?\x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
	 (?\,Mn(B . ?\x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
	 (?\,Mo(B . ?\x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
	 (?\,Mp(B . ?\x011F) ;; LATIN SMALL LETTER G WITH BREVE
	 (?\,Mq(B . ?\x00F1) ;; LATIN SMALL LETTER N WITH TILDE
	 (?\,Mr(B . ?\x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
	 (?\,Ms(B . ?\x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
	 (?\,Mt(B . ?\x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
	 (?\,Mu(B . ?\x00F5) ;; LATIN SMALL LETTER O WITH TILDE
	 (?\,Mv(B . ?\x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
	 (?\,Mw(B . ?\x00F7) ;; DIVISION SIGN
	 (?\,Mx(B . ?\x00F8) ;; LATIN SMALL LETTER O WITH STROKE
	 (?\,My(B . ?\x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
	 (?\,Mz(B . ?\x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
	 (?\,M{(B . ?\x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
	 (?\,M|(B . ?\x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
	 (?\,M}(B . ?\x0131) ;; LATIN SMALL LETTER DOTLESS I
	 (?\,M~(B . ?\x015F) ;; LATIN SMALL LETTER S WITH CEDILLA
	 (?\,M(B . ?\x00FF) ;; LATIN SMALL LETTER Y WITH DIAERESIS
	 ))

      (ucs-8859-14-alist
       '((?\,_ (B . ?\x00A0) ;; NO-BREAK SPACE
	 (?\,_!(B . ?\x1E02) ;; LATIN CAPITAL LETTER B WITH DOT ABOVE
	 (?\,_"(B . ?\x1E03) ;; LATIN SMALL LETTER B WITH DOT ABOVE
	 (?\,_#(B . ?\x00A3) ;; POUND SIGN
	 (?\,_$(B . ?\x010A) ;; LATIN CAPITAL LETTER C WITH DOT ABOVE
	 (?\,_%(B . ?\x010B) ;; LATIN SMALL LETTER C WITH DOT ABOVE
	 (?\,_&(B . ?\x1E0A) ;; LATIN CAPITAL LETTER D WITH DOT ABOVE
	 (?\,_'(B . ?\x00A7) ;; SECTION SIGN
	 (?\,_((B . ?\x1E80) ;; LATIN CAPITAL LETTER W WITH GRAVE
	 (?\,_)(B . ?\x00A9) ;; COPYRIGHT SIGN
	 (?\,_*(B . ?\x1E82) ;; LATIN CAPITAL LETTER W WITH ACUTE
	 (?\,_+(B . ?\x1E0B) ;; LATIN SMALL LETTER D WITH DOT ABOVE
	 (?\,_,(B . ?\x1EF2) ;; LATIN CAPITAL LETTER Y WITH GRAVE
	 (?\,_-(B . ?\x00AD) ;; SOFT HYPHEN
	 (?\,_.(B . ?\x00AE) ;; REGISTERED SIGN
	 (?\,_/(B . ?\x0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
	 (?\,_0(B . ?\x1E1E) ;; LATIN CAPITAL LETTER F WITH DOT ABOVE
	 (?\,_1(B . ?\x1E1F) ;; LATIN SMALL LETTER F WITH DOT ABOVE
	 (?\,_2(B . ?\x0120) ;; LATIN CAPITAL LETTER G WITH DOT ABOVE
	 (?\,_3(B . ?\x0121) ;; LATIN SMALL LETTER G WITH DOT ABOVE
	 (?\,_4(B . ?\x1E40) ;; LATIN CAPITAL LETTER M WITH DOT ABOVE
	 (?\,_5(B . ?\x1E41) ;; LATIN SMALL LETTER M WITH DOT ABOVE
	 (?\,_6(B . ?\x00B6) ;; PILCROW SIGN
	 (?\,_7(B . ?\x1E56) ;; LATIN CAPITAL LETTER P WITH DOT ABOVE
	 (?\,_8(B . ?\x1E81) ;; LATIN SMALL LETTER W WITH GRAVE
	 (?\,_9(B . ?\x1E57) ;; LATIN SMALL LETTER P WITH DOT ABOVE
	 (?\,_:(B . ?\x1E83) ;; LATIN SMALL LETTER W WITH ACUTE
	 (?\,_;(B . ?\x1E60) ;; LATIN CAPITAL LETTER S WITH DOT ABOVE
	 (?\,_<(B . ?\x1EF3) ;; LATIN SMALL LETTER Y WITH GRAVE
	 (?\,_=(B . ?\x1E84) ;; LATIN CAPITAL LETTER W WITH DIAERESIS
	 (?\,_>(B . ?\x1E85) ;; LATIN SMALL LETTER W WITH DIAERESIS
	 (?\,_?(B . ?\x1E61) ;; LATIN SMALL LETTER S WITH DOT ABOVE
	 (?\,_@(B . ?\x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
	 (?\,_A(B . ?\x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
	 (?\,_B(B . ?\x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
	 (?\,_C(B . ?\x00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
	 (?\,_D(B . ?\x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
	 (?\,_E(B . ?\x00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
	 (?\,_F(B . ?\x00C6) ;; LATIN CAPITAL LETTER AE
	 (?\,_G(B . ?\x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
	 (?\,_H(B . ?\x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
	 (?\,_I(B . ?\x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
	 (?\,_J(B . ?\x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
	 (?\,_K(B . ?\x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
	 (?\,_L(B . ?\x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
	 (?\,_M(B . ?\x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
	 (?\,_N(B . ?\x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
	 (?\,_O(B . ?\x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
	 (?\,_P(B . ?\x0174) ;; LATIN CAPITAL LETTER W WITH CIRCUMFLEX
	 (?\,_Q(B . ?\x00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
	 (?\,_R(B . ?\x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
	 (?\,_S(B . ?\x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
	 (?\,_T(B . ?\x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	 (?\,_U(B . ?\x00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
	 (?\,_V(B . ?\x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
	 (?\,_W(B . ?\x1E6A) ;; LATIN CAPITAL LETTER T WITH DOT ABOVE
	 (?\,_X(B . ?\x00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
	 (?\,_Y(B . ?\x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
	 (?\,_Z(B . ?\x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
	 (?\,_[(B . ?\x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
	 (?\,_\(B . ?\x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
	 (?\,_](B . ?\x00DD) ;; LATIN CAPITAL LETTER Y WITH ACUTE
	 (?\,_^(B . ?\x0176) ;; LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
	 (?\,__(B . ?\x00DF) ;; LATIN SMALL LETTER SHARP S
	 (?\,_`(B . ?\x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
	 (?\,_a(B . ?\x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
	 (?\,_b(B . ?\x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
	 (?\,_c(B . ?\x00E3) ;; LATIN SMALL LETTER A WITH TILDE
	 (?\,_d(B . ?\x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
	 (?\,_e(B . ?\x00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
	 (?\,_f(B . ?\x00E6) ;; LATIN SMALL LETTER AE
	 (?\,_g(B . ?\x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
	 (?\,_h(B . ?\x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
	 (?\,_i(B . ?\x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
	 (?\,_j(B . ?\x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
	 (?\,_k(B . ?\x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
	 (?\,_l(B . ?\x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
	 (?\,_m(B . ?\x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
	 (?\,_n(B . ?\x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
	 (?\,_o(B . ?\x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
	 (?\,_p(B . ?\x0175) ;; LATIN SMALL LETTER W WITH CIRCUMFLEX
	 (?\,_q(B . ?\x00F1) ;; LATIN SMALL LETTER N WITH TILDE
	 (?\,_r(B . ?\x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
	 (?\,_s(B . ?\x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
	 (?\,_t(B . ?\x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
	 (?\,_u(B . ?\x00F5) ;; LATIN SMALL LETTER O WITH TILDE
	 (?\,_v(B . ?\x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
	 (?\,_w(B . ?\x1E6B) ;; LATIN SMALL LETTER T WITH DOT ABOVE
	 (?\,_x(B . ?\x00F8) ;; LATIN SMALL LETTER O WITH STROKE
	 (?\,_y(B . ?\x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
	 (?\,_z(B . ?\x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
	 (?\,_{(B . ?\x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
	 (?\,_|(B . ?\x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
	 (?\,_}(B . ?\x00FD) ;; LATIN SMALL LETTER Y WITH ACUTE
	 (?\,_~(B . ?\x0177) ;; LATIN SMALL LETTER Y WITH CIRCUMFLEX
	 (?\,_(B . ?\x00FF) ;; LATIN SMALL LETTER Y WITH DIAERESIS
	 ))

      (ucs-8859-15-alist
       '((?\,b (B . ?\x00A0) ;; NO-BREAK SPACE
	 (?\,b!(B . ?\x00A1) ;; INVERTED EXCLAMATION MARK
	 (?\,b"(B . ?\x00A2) ;; CENT SIGN
	 (?\,b#(B . ?\x00A3) ;; POUND SIGN
	 (?\,b$(B . ?\x20AC) ;; EURO SIGN
	 (?\,b%(B . ?\x00A5) ;; YEN SIGN
	 (?\,b&(B . ?\x0160) ;; LATIN CAPITAL LETTER S WITH CARON
	 (?\,b'(B . ?\x00A7) ;; SECTION SIGN
	 (?\,b((B . ?\x0161) ;; LATIN SMALL LETTER S WITH CARON
	 (?\,b)(B . ?\x00A9) ;; COPYRIGHT SIGN
	 (?\,b*(B . ?\x00AA) ;; FEMININE ORDINAL INDICATOR
	 (?\,b+(B . ?\x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
	 (?\,b,(B . ?\x00AC) ;; NOT SIGN
	 (?\,b-(B . ?\x00AD) ;; SOFT HYPHEN
	 (?\,b.(B . ?\x00AE) ;; REGISTERED SIGN
	 (?\,b/(B . ?\x00AF) ;; MACRON
	 (?\,b0(B . ?\x00B0) ;; DEGREE SIGN
	 (?\,b1(B . ?\x00B1) ;; PLUS-MINUS SIGN
	 (?\,b2(B . ?\x00B2) ;; SUPERSCRIPT TWO
	 (?\,b3(B . ?\x00B3) ;; SUPERSCRIPT THREE
	 (?\,b4(B . ?\x017D) ;; LATIN CAPITAL LETTER Z WITH CARON
	 (?\,b5(B . ?\x00B5) ;; MICRO SIGN
	 (?\,b6(B . ?\x00B6) ;; PILCROW SIGN
	 (?\,b7(B . ?\x00B7) ;; MIDDLE DOT
	 (?\,b8(B . ?\x017E) ;; LATIN SMALL LETTER Z WITH CARON
	 (?\,b9(B . ?\x00B9) ;; SUPERSCRIPT ONE
	 (?\,b:(B . ?\x00BA) ;; MASCULINE ORDINAL INDICATOR
	 (?\,b;(B . ?\x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
	 (?\,b<(B . ?\x0152) ;; LATIN CAPITAL LIGATURE OE
	 (?\,b=(B . ?\x0153) ;; LATIN SMALL LIGATURE OE
	 (?\,b>(B . ?\x0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
	 (?\,b?(B . ?\x00BF) ;; INVERTED QUESTION MARK
	 (?\,b@(B . ?\x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
	 (?\,bA(B . ?\x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
	 (?\,bB(B . ?\x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
	 (?\,bC(B . ?\x00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
	 (?\,bD(B . ?\x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
	 (?\,bE(B . ?\x00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
	 (?\,bF(B . ?\x00C6) ;; LATIN CAPITAL LETTER AE
	 (?\,bG(B . ?\x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
	 (?\,bH(B . ?\x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
	 (?\,bI(B . ?\x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
	 (?\,bJ(B . ?\x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
	 (?\,bK(B . ?\x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
	 (?\,bL(B . ?\x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
	 (?\,bM(B . ?\x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
	 (?\,bN(B . ?\x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
	 (?\,bO(B . ?\x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
	 (?\,bP(B . ?\x00D0) ;; LATIN CAPITAL LETTER ETH
	 (?\,bQ(B . ?\x00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
	 (?\,bR(B . ?\x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
	 (?\,bS(B . ?\x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
	 (?\,bT(B . ?\x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	 (?\,bU(B . ?\x00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
	 (?\,bV(B . ?\x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
	 (?\,bW(B . ?\x00D7) ;; MULTIPLICATION SIGN
	 (?\,bX(B . ?\x00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
	 (?\,bY(B . ?\x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
	 (?\,bZ(B . ?\x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
	 (?\,b[(B . ?\x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
	 (?\,b\(B . ?\x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
	 (?\,b](B . ?\x00DD) ;; LATIN CAPITAL LETTER Y WITH ACUTE
	 (?\,b^(B . ?\x00DE) ;; LATIN CAPITAL LETTER THORN
	 (?\,b_(B . ?\x00DF) ;; LATIN SMALL LETTER SHARP S
	 (?\,b`(B . ?\x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
	 (?\,ba(B . ?\x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
	 (?\,bb(B . ?\x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
	 (?\,bc(B . ?\x00E3) ;; LATIN SMALL LETTER A WITH TILDE
	 (?\,bd(B . ?\x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
	 (?\,be(B . ?\x00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
	 (?\,bf(B . ?\x00E6) ;; LATIN SMALL LETTER AE
	 (?\,bg(B . ?\x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
	 (?\,bh(B . ?\x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
	 (?\,bi(B . ?\x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
	 (?\,bj(B . ?\x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
	 (?\,bk(B . ?\x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
	 (?\,bl(B . ?\x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
	 (?\,bm(B . ?\x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
	 (?\,bn(B . ?\x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
	 (?\,bo(B . ?\x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
	 (?\,bp(B . ?\x00F0) ;; LATIN SMALL LETTER ETH
	 (?\,bq(B . ?\x00F1) ;; LATIN SMALL LETTER N WITH TILDE
	 (?\,br(B . ?\x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
	 (?\,bs(B . ?\x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
	 (?\,bt(B . ?\x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
	 (?\,bu(B . ?\x00F5) ;; LATIN SMALL LETTER O WITH TILDE
	 (?\,bv(B . ?\x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
	 (?\,bw(B . ?\x00F7) ;; DIVISION SIGN
	 (?\,bx(B . ?\x00F8) ;; LATIN SMALL LETTER O WITH STROKE
	 (?\,by(B . ?\x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
	 (?\,bz(B . ?\x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
	 (?\,b{(B . ?\x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
	 (?\,b|(B . ?\x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
	 (?\,b}(B . ?\x00FD) ;; LATIN SMALL LETTER Y WITH ACUTE
	 (?\,b~(B . ?\x00FE) ;; LATIN SMALL LETTER THORN
	 (?\,b(B . ?\x00FF) ;; LATIN SMALL LETTER Y WITH DIAERESIS
	 ))

      (ucs-8859-1-alist
       (let ((i 160)
	     l)
	 (while (< i 256)
	   (push (cons (make-char 'latin-iso8859-1 (- i 128)) i)
		 l)
	   (setq i (1+ i)))
	 (nreverse l)))
      )

  (dolist (cs (list ucs-8859-15-alist ucs-8859-14-alist
		    ucs-8859-9-alist ucs-8859-8-alist ucs-8859-7-alist
		    ucs-8859-6-alist ucs-8859-5-alist ucs-8859-4-alist
		    ucs-8859-3-alist ucs-8859-2-alist
		    (or (cdr-safe 
			 (assq ucs-preferred-8859-set
			       '((latin-iso8859-15 . ucs-8859-15-alist)
				 (latin-iso8859-14 . ucs-8859-14-alist)
				 (latin-iso8859-9 . ucs-8859-9-alist)
				 (latin-iso8859-5 . ucs-8859-5-alist)
				 (latin-iso8859-4 . ucs-8859-4-alist)
				 (latin-iso8859-3 . ucs-8859-3-alist)
				 (latin-iso8859-2 . ucs-8859-2-alist))))
			ucs-8859-1-alist)))
    (dolist (pair cs)
      (aset ucs-mule-8859-to-ucs-table (car pair) (cdr pair))
      (aset ucs-ucs-to-mule-8859-table (cdr pair) (car pair))
      (aset ucs-mule-8859-to-mule-unicode
	    (car pair) (decode-char 'ucs (cdr pair)))
      (aset ucs-mule-unicode-to-mule-8859
	    (decode-char 'ucs (cdr pair)) (car pair))))

  (map-char-table
   (lambda (c cu)
     (when (and cu (< cu 256))
       (aset ucs-latin-1-unification-table
	     c (make-char 'latin-iso8859-1 (- cu 128)))))
   ucs-mule-8859-to-ucs-table)
  )

;; Register them for use in CCL.
(define-translation-table 'ucs-mule-8859-to-mule-unicode
  ucs-mule-8859-to-mule-unicode)
(define-translation-table 'ucs-latin-1-unification-table
  ucs-latin-1-unification-table)

(defun ucs-translate-region (beg end table)
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((c (char-after))
	     (c2 (aref table c)))
	     (if c2
		 (progn 
		   (delete-char 1)
		   (insert c2))	   
	       (forward-char))))))

(defun ucs-unify-to-latin-1 (&optional arg)
  "Re-set up the Latin-1 coding system to encode unified characters.
When this is done, text encoded using the `iso-latin-1' coding system
is first translated using the translation table
`ucs-latin-1-unification-table'.  This converts ISO-8859-N (N>1)
characters to their Latin-1 equivalents when such equivalents exist.
Thus a buffer which contains a Latin-2 \"small y with acute\" (code
point 253) will be safely encoded to that code point since it occurs
there in Latin-1.  On the other hand, \"small t with cedilla\" does
not occur in Latin-1 and so can't be safely encoded when this
unification is done.

With optional ARG, turn off such unification."
  (if arg
      (make-coding-system
       'iso-latin-1 2 ?1
       "ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)"
       '(ascii latin-iso8859-1 nil nil
	       nil nil nil nil nil nil nil nil nil nil nil nil t)
       `((safe-charsets ascii latin-iso8859-1)
	 (mime-charset . iso-8859-1)
	 (safe-chars . ucs-latin-1-unification-table)
	 (translation-table-for-encode . ,ucs-latin-1-unification-table)))
    (make-coding-system
     'iso-latin-1 2 ?1
     "ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)"
     '(ascii latin-iso8859-1 nil nil
	     nil nil nil nil nil nil nil nil nil nil nil nil t)
     '((safe-charsets ascii latin-iso8859-1)
       (mime-charset . iso-8859-1)))))

(provide 'ucs-tables)

;;; ucs-tables.el ends here
