;;; ucs-tables.el --- translation to, from and via Unicode  -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: i18n

;; This file is part of GNU Emacs.

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
;; emacs-mule characters from the iso8859 charsets (and others).  It
;; also provides some auxiliary functions.

;; These tables are used to construct other mappings between the Mule
;; iso8859 charsets and the emacs-unicode charsets and a table that
;; unifies iso8859 characters using a single charset as far as
;; possible.  These tables are used by latin1-disp.el to display some
;; Unicode characters without a Unicode font and by utf-8.el to unify
;; Latin-N as far as possible on encoding.

;; More drastically, they can be used to unify 8859 into Latin-1 plus
;; mule-unicode-0100-24ff on decoding, with the corresponding
;; adjustments on encoding; see `ucs-unify-8859'.  Be wary of using
;; unification when, for instance, editing Lisp files such as this one
;; which are supposed to contain distinct 8859 charsets.  Also, it can
;; make reading and writing of emacs-mule and iso-2022-based encodings
;; not idempotent.

;; Global minor modes are provided to unify on encoding and decoding.

;; The translation table `ucs-mule-to-mule-unicode' is populated.
;; This is used by the `mule-utf-8' coding system to encode extra
;; characters.

;; Command `ucs-insert' is convenient for inserting a given Unicode.
;; (See also the `ucs' input method.)

;;; Code:

;;; Define tables, to be populated later.

(defvar ucs-mule-8859-to-ucs-table (make-translation-table)
  "Translation table from Emacs ISO-8859 characters to Unicode.
This maps Emacs characters from the non-Latin-1
...-iso8859-... charsets to their Unicode code points.  This is a
many-to-one mapping.")

(defvar ucs-mule-8859-to-mule-unicode (make-translation-table)
  "Translation table from Emacs ISO-8859 characters to Mule Unicode.
This maps Emacs characters from the non-Latin-1
...-iso8859-... charsets to characters from the
mule-unicode-... charsets.  This is a many-to-one mapping.  The
characters translated to are suitable for encoding using the
`mule-utf-8' coding system.")

;; (defvar ucs-ucs-to-mule-8859-table (make-translation-table)
;;   "Translation table from Unicode to Emacs ISO-8859 characters.
;; This maps Unicode code points to corresponding Emacs characters from
;; the ...-iso8859-... charsets.  This is made a one-to-one mapping where
;; the same character occurs in more than one set by preferring the Emacs
;; iso-8859-N character with lowest N.")

;; (defvar ucs-mule-unicode-to-mule-8859 (make-translation-table)
;;   "Translation table from Mule Unicode to Emacs ISO-8859 characters.
;; This maps non-Latin-1 Emacs characters from the
;; mule-unicode-... charsets used by the `mule-utf-8' coding system to
;; characters from the ...-iso8859-... charsets.  This is made a
;; one-to-one mapping where the same character occurs in more than one
;; set by preferring the Emacs iso-8859-N character with lowest N.")

(defvar ucs-8859-1-encode-table nil
  "Used as `translation-table-for-encode' for iso-8859-2.
Translates from the iso8859 charsets and `mule-unicode-0100-24ff'.")

(defvar ucs-8859-2-encode-table nil
  "Used as `translation-table-for-encode' for iso-8859-2.
Translates from the iso8859 charsets and `mule-unicode-0100-24ff'.")

(defvar ucs-8859-3-encode-table nil
  "Used as `translation-table-for-encode' for iso-8859-3.
Translates from the iso8859 charsets and `mule-unicode-0100-24ff'.")

(defvar ucs-8859-4-encode-table nil
  "Used as `translation-table-for-encode' for iso-8859-4.
Translates from the iso8859 charsets and `mule-unicode-0100-24ff'.")

(defvar ucs-8859-5-encode-table nil
  "Used as `translation-table-for-encode' for iso-8859-5.
Translates from the iso8859 charsets and `mule-unicode-0100-24ff'.")

(defvar ucs-8859-7-encode-table nil
  "Used as `translation-table-for-encode' for iso-8859-7.
Translates from the iso8859 charsets and `mule-unicode-0100-24ff'.")

(defvar ucs-8859-8-encode-table nil
  "Used as `translation-table-for-encode' for iso-8859-8.
Translates from the iso8859 charsets and `mule-unicode-0100-24ff'.")

(defvar ucs-8859-9-encode-table nil
  "Used as `translation-table-for-encode' for iso-8859-9.
Translates from the iso8859 charsets and `mule-unicode-0100-24ff'.")

(defvar ucs-8859-14-encode-table nil
  "Used as `translation-table-for-encode' for iso-8859-14.
Translates from the iso8859 charsets and `mule-unicode-0100-24ff'.")

(defvar ucs-8859-15-encode-table nil
  "Used as `translation-table-for-encode' for iso-8859-15.
Translates from the iso8859 charsets and `mule-unicode-0100-24ff'.")

;; Probably defined by utf-8.el.
(defvar ucs-mule-to-mule-unicode (make-translation-table))
(unless (get 'ucs-mule-to-mule-unicode 'translation-table)
  (define-translation-table 'ucs-mule-to-mule-unicode ucs-mule-to-mule-unicode))
;;; Set up the tables.

;; Most of these tables were derived from ones in Mule-UCS.

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
	 (?,G,(B . ?\x060C)	;; ARABIC COMMA
	 (?,G-(B . ?\x00AD)	;; SOFT HYPHEN
	 (?,G;(B . ?\x061B)	;; ARABIC SEMICOLON
	 (?,G?(B . ?\x061F)	;; ARABIC QUESTION MARK
	 (?,GA(B . ?\x0621)	;; ARABIC LETTER HAMZA
	 (?,GB(B . ?\x0622)	;; ARABIC LETTER ALEF WITH MADDA ABOVE
	 (?,GC(B . ?\x0623)	;; ARABIC LETTER ALEF WITH HAMZA ABOVE
	 (?,GD(B . ?\x0624)	;; ARABIC LETTER WAW WITH HAMZA ABOVE
	 (?,GE(B . ?\x0625)	;; ARABIC LETTER ALEF WITH HAMZA BELOW
	 (?,GF(B . ?\x0626)	;; ARABIC LETTER YEH WITH HAMZA ABOVE
	 (?,GG(B . ?\x0627)	;; ARABIC LETTER ALEF
	 (?,GH(B . ?\x0628)	;; ARABIC LETTER BEH
	 (?,GI(B . ?\x0629)	;; ARABIC LETTER TEH MARBUTA
	 (?,GJ(B . ?\x062A)	;; ARABIC LETTER TEH
	 (?,GK(B . ?\x062B)	;; ARABIC LETTER THEH
	 (?,GL(B . ?\x062C)	;; ARABIC LETTER JEEM
	 (?,GM(B . ?\x062D)	;; ARABIC LETTER HAH
	 (?,GN(B . ?\x062E)	;; ARABIC LETTER KHAH
	 (?,GO(B . ?\x062F)	;; ARABIC LETTER DAL
	 (?,GP(B . ?\x0630)	;; ARABIC LETTER THAL
	 (?,GQ(B . ?\x0631)	;; ARABIC LETTER REH
	 (?,GR(B . ?\x0632)	;; ARABIC LETTER ZAIN
	 (?,GS(B . ?\x0633)	;; ARABIC LETTER SEEN
	 (?,GT(B . ?\x0634)	;; ARABIC LETTER SHEEN
	 (?,GU(B . ?\x0635)	;; ARABIC LETTER SAD
	 (?,GV(B . ?\x0636)	;; ARABIC LETTER DAD
	 (?,GW(B . ?\x0637)	;; ARABIC LETTER TAH
	 (?,GX(B . ?\x0638)	;; ARABIC LETTER ZAH
	 (?,GY(B . ?\x0639)	;; ARABIC LETTER AIN
	 (?,GZ(B . ?\x063A)	;; ARABIC LETTER GHAIN
	 (?,G`(B . ?\x0640)	;; ARABIC TATWEEL
	 (?,Ga(B . ?\x0641)	;; ARABIC LETTER FEH
	 (?,Gb(B . ?\x0642)	;; ARABIC LETTER QAF
	 (?,Gc(B . ?\x0643)	;; ARABIC LETTER KAF
	 (?,Gd(B . ?\x0644)	;; ARABIC LETTER LAM
	 (?,Ge(B . ?\x0645)	;; ARABIC LETTER MEEM
	 (?,Gf(B . ?\x0646)	;; ARABIC LETTER NOON
	 (?,Gg(B . ?\x0647)	;; ARABIC LETTER HEH
	 (?,Gh(B . ?\x0648)	;; ARABIC LETTER WAW
	 (?,Gi(B . ?\x0649)	;; ARABIC LETTER ALEF MAKSURA
	 (?,Gj(B . ?\x064A)	;; ARABIC LETTER YEH
	 (?,Gk(B . ?\x064B)	;; ARABIC FATHATAN
	 (?,Gl(B . ?\x064C)	;; ARABIC DAMMATAN
	 (?,Gm(B . ?\x064D)	;; ARABIC KASRATAN
	 (?,Gn(B . ?\x064E)	;; ARABIC FATHA
	 (?,Go(B . ?\x064F)	;; ARABIC DAMMA
	 (?,Gp(B . ?\x0650)	;; ARABIC KASRA
	 (?,Gq(B . ?\x0651)	;; ARABIC SHADDA
	 (?,Gr(B . ?\x0652)	;; ARABIC SUKUN
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
	 ;; These are commented out since the current 8859-8 standard
	 ;; does not yet define these codepoints, although there are
	 ;; drafts which do).
;	 (?\,H@(B . ?\x05B0) ;; HEBREW POINT SHEVA
;	 (?\,HA(B . ?\x05B1) ;; HEBREW POINT HATAF SEGOL
;	 (?\,HB(B . ?\x05B2) ;; HEBREW POINT HATAF PATAH
;	 (?\,HC(B . ?\x05B3) ;; HEBREW POINT HATAF QAMATS
;	 (?\,HD(B . ?\x05B4) ;; HEBREW POINT HIRIQ
;	 (?\,HE(B . ?\x05B5) ;; HEBREW POINT TSERE
;	 (?\,HF(B . ?\x05B6) ;; HEBREW POINT SEGOL
;	 (?\,HG(B . ?\x05B7) ;; HEBREW POINT PATAH
;	 (?\,HH(B . ?\x05B8) ;; HEBREW POINT QAMATS
;	 (?\,HI(B . ?\x05B9) ;; HEBREW POINT HOLAM
;	 (?\,HK(B . ?\x05BB) ;; HEBREW POINT QUBUTS
;	 (?\,HL(B . ?\x05BC) ;; HEBREW POINT DAGESH
;	 (?\,HM(B . ?\x05BD) ;; HEBREW POINT METEG
;	 (?\,HN(B . ?\x05BE) ;; HEBREW POINT MAQAF
;	 (?\,HO(B . ?\x05BF) ;; HEBREW POINT RAFE
;	 (?\,HP(B . ?\x05C0) ;; HEBREW PUNCTUATION PASEQ
;	 (?\,HQ(B . ?\x05C1) ;; HEBREW POINT SHIN DOT
;	 (?\,HR(B . ?\x05C2) ;; HEBREW POINT SIN DOT
;	 (?\,HS(B . ?\x05C3) ;; HEBREW PUNCTUATION SOF PASUQ
	 (?\,H[(B . ?\x202D) ;; LEFT-TO-RIGHT OVERRIDE
	 (?\,H\(B . ?\x202E) ;; RIGHT-TO-LEFT OVERRIDE
	 (?\,H](B . ?\x202C) ;; POP DIRECTIONAL FORMATTING
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
	 (?\,H{(B . ?\x202A) ;; LEFT-TO-RIGHT EMBEDDING
	 (?\,H|(B . ?\x202B) ;; RIGHT-TO-LEFT EMBEDDING
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
      
;;       (case-table (standard-case-table))
;;       (syntax-table (standard-syntax-table))
      )

  ;; Convert the lists to the basic char tables.
  (dolist (n (list 15 14 9 8 7 5 4 3 2 1))
    (let ((alist (symbol-value (intern (format "ucs-8859-%d-alist" n)))))
      (dolist (pair alist)
	(let ((mule (car pair))
	      (uc (cdr pair))
	      (mu (decode-char 'ucs (cdr pair))))
	  (aset ucs-mule-8859-to-ucs-table mule uc)
	  ;; 	  (aset ucs-ucs-to-mule-8859-table uc mule)
	  ;; 	  (aset ucs-mule-unicode-to-mule-8859 mu mule)
	  (aset ucs-mule-8859-to-mule-unicode mule mu)
	  (aset ucs-mule-to-mule-unicode mule mu)))
;; I think this is actually done OK in characters.el.
;; Probably things like accents shouldn't have word syntax, but the
;; Latin-N syntax tables currently aren't consistent for such
;; characters anyhow.
;;      ;; Make the mule-unicode characters inherit syntax and case info
;;      ;; if they don't already have it.
;;      (dolist (pair alist)
;; 	(let ((mule (car pair))
;; 	      (uc (cdr pair))
;; 	      (mu (decode-char 'ucs (cdr pair))))
;; 	  (let ((syntax (aref syntax-table mule)))
;; 	    (if (eq mule (downcase mule))
;; 		(if (eq mule (upcase mule)) ; non-letter or uncased letter
;; 		    (progn
;; 		      (if (= 4 (car syntax)) ; left delim
;; 			  (progn
;; 			    (aset syntax-table
;; 				  mu
;; 				  (cons 4 (aref ucs-mule-8859-to-mule-unicode
;; 						(cdr syntax))))
;; 			    (aset syntax-table
;; 				  (aref ucs-mule-8859-to-mule-unicode
;; 					(cdr syntax))
;; 				  (cons 5 mu)))
;; 			(aset syntax-table mu syntax))
;; 		      (aset case-table mu mu)))
;; 	      ;; Upper case letter
;; 	      (let ((lower (aref ucs-mule-8859-to-mule-unicode
;; 				 (aref case-table mule))))
;; 		(aset case-table mu lower)
;; 		(aset case-table lower lower)
;; 		(modify-syntax-entry lower "w   " syntax-table)
;; 		(modify-syntax-entry mu "w   " syntax-table))))))
      ))
  ;; Derive tables that can be used as per-coding-system
  ;; `translation-table-for-encode's.
  (dolist (n (list 15 14 9 8 7 5 4 3 2 1))
    (let* ((alist (symbol-value (intern (format "ucs-8859-%d-alist" n))))
	   (encode-translator (set (intern (format "ucs-8859-%d-encode-table"
						   n))
				   (make-translation-table)))
	   elt)
      ;; Start with the mule-unicode component.
      (dolist (pair alist)
	(let ((mule (car pair))
	      (mu (decode-char 'ucs (cdr pair))))
	  (aset encode-translator mu mule)))
      ;; Find characters from other 8859 sets which map to the same
      ;; unicode as some character in this set.
      (map-char-table (lambda (k v)
			(if (and (setq elt (rassq v alist))
				 (not (assq k alist)))
			    (aset encode-translator k (car elt))))
		      ucs-mule-8859-to-ucs-table))))

;; Register for use in CCL.
(define-translation-table 'ucs-mule-8859-to-mule-unicode
  ucs-mule-8859-to-mule-unicode)

;; Fixme: Make this reversible, which means frobbing
;; `char-coding-system-table' directly to remove what we added -- see
;; codepages.el.  Also make it a user option.
(defun ucs-unify-8859 (&optional encode-only)
  "Set up translation tables for unifying characters from ISO 8859.

On decoding, non-ASCII characters are mapped into the `iso-latin-1'
and `mule-unicode-0100-24ff' charsets.  On encoding, these are mapped
back appropriate for the coding system.

With prefix arg, do unification on encoding only, i.e. don't unify
everything on input operations."
  (interactive "P")
  (unless encode-only
    ;; Unify 8859 on decoding.  (Non-CCL coding systems only.)
    (unify-8859-on-decoding-mode 1))
  ;; Adjust the 8859 coding systems to fragment the unified characters
  ;; on encoding.
  (dolist (n '(1 2 3 4 5 7 8 9 14 15))
    (let* ((coding-system
	    (coding-system-base (intern (format "iso-8859-%d" n))))
	   (table (symbol-value
		   (intern (format "ucs-8859-%d-encode-table" n))))
	   (safe (coding-system-get coding-system 'safe-chars)))
      ;; Actually, the coding system's safe-chars are not normally
      ;; used after they've been registered, but we might as well
      ;; record them.  Setting the parent here is a convenience.
      (set-char-table-parent safe table)
      ;; Update the table of what encodes to what.
      (register-char-codings coding-system table)
      (coding-system-put coding-system 'translation-table-for-encode table)))

;;; The following works for the bundled coding systems, but it's
;;; better to use the Unicode-based ones and make it irrelevant.

;;;   ;; Update the Cyrillic special cases.
;;;   ;; `translation-table-for-encode' doesn't work for CCL coding
;;;   ;; systems, and `standard-translation-table-for-decode' isn't
;;;   ;; applied.
;;;   (let ((table (get 'cyrillic-koi8-r-encode-table 'translation-table)))
;;;     (map-char-table
;;;      (lambda (k v)
;;;        (aset table
;;; 	     (or (aref ucs-8859-5-encode-table k)
;;; 		 k)
;;; 	     v))
;;;      table)
;;;     (register-char-codings 'cyrillic-koi8 table))
;;;   (let ((table (get 'cyrillic-koi8-r-nonascii-translation-table
;;; 		    'translation-table)))
;;;     (map-char-table
;;;      (lambda (k v)
;;;        (if v (aset table k (or (aref ucs-mule-8859-to-mule-unicode v)
;;; 			       v))))
;;;      table))
;;;   ;; Redefine this, since the orginal only translated 8859-5.
;;;   (define-ccl-program ccl-encode-koi8
;;;     `(1
;;;       ((loop
;;; 	(read-multibyte-character r0 r1)
;;; 	(translate-character cyrillic-koi8-r-encode-table r0 r1)
;;; 	(write-repeat r1))))
;;;     "CCL program to encode KOI8.")
;;;   (let ((table (get 'cyrillic-alternativnyj-encode-table 'translation-table)))
;;;     (map-char-table
;;;      (lambda (k v)
;;;        (aset table
;;; 	     (or (aref ucs-8859-5-encode-table k)
;;; 		 k)
;;; 	     v))
;;;      table)
;;;     (register-char-codings 'cyrillic-alternativnyj table))
;;;   (let ((table (get 'cyrillic-alternativnyj-nonascii-translation-table
;;; 		    'translation-table)))
;;;     (map-char-table
;;;      (lambda (k v)
;;;        (if v (aset table
;;; 		   k
;;; 		   (or (aref ucs-mule-8859-to-mule-unicode v)
;;; 		       v))))
;;;      table))
  )

(defun ucs-fragment-8859 (&optional encode-only)
  "Undo the unification done by `ucs-unify-8859'.
With prefix arg, undo unification on encoding only, i.e. don't undo
unification on input operations."
  (interactive "P")
  ;; Maybe fix decoding.
  (unless encode-only
    ;; Unify 8859 on decoding.  (Non-CCL coding systems only.)
    (unify-8859-on-decoding-mode -1))
  ;; Fix encoding.  For each charset, remove the entries in
  ;; `char-coding-system-table' added to its safe-chars table (as its
  ;; parent).
  (dolist (n '(1 2 3 4 5 7 8 9 14 15))
    (let* ((coding-system
	    (coding-system-base (intern (format "iso-8859-%d" n))))
	   (table (symbol-value
		   (intern (format "ucs-8859-%d-encode-table" n))))
	   (safe (coding-system-get coding-system 'safe-chars)))
      (map-char-table
       (lambda (key val)
	 (if (and (>= key 128) val)
	     (let ((codings (aref char-coding-system-table key)))
	       (aset char-coding-system-table key
		     (delq coding-system codings)))))
       (char-table-parent safe))
      (set-char-table-parent safe nil)
      (coding-system-put coding-system 'translation-table-for-encode nil))))

(define-minor-mode unify-8859-on-encoding-mode
  "Set up translation tables for unifying ISO 8859 characters on encoding.

The ISO 8859 characters sets overlap, e.g. 8859-1 (Latin-1) and
8859-15 (Latin-9) differ only in a few characters.  Emacs normally
distinguishes equivalent characters from those ISO-8859 character sets
which are built in to Emacs.  This behaviour is essentially inherited
from the European-originated international standards.  Treating them
equivalently, by translating to and from a single representation is
called `unification'.  (The `utf-8' coding system treats the
characters of European scripts in a unified manner.)

In this mode, on encoding -- i.e. output operations -- non-ASCII
characters from the built-in ISO 8859 and `mule-unicode-0100-24ff'
charsets are handled automatically by the coding system used if it can
represent them.  Thus, say, an e-acute from the Latin-1 charset (the
unified representation) in a buffer saved as Latin-9 will be encoded
directly to a byte value 233.  By default, in contrast, you would be
prompted for a general coding system to use for saving the file, which
can cope with separate Latin-1 and Latin-9 representations of e-acute.

See also command `unify-8859-on-decoding-mode'."
  :group 'mule
  :global t
  :version 21.3				; who knows...?
  :init-value nil
  (if unify-8859-on-encoding-mode
      (ucs-unify-8859 t)
    (ucs-fragment-8859 t)))

(define-minor-mode unify-8859-on-decoding-mode
  "Set up translation table for unifying ISO 8859 characters on decoding.
On decoding -- i.e. input operations -- non-ASCII characters from the
built-in ISO 8859 charsets are unified by mapping them into the
`iso-latin-1' and `mule-unicode-0100-24ff' charsets.

This sets the parent of `standard-translation-table-for-decode'.
Also sets `translation-table-for-input' globally, so that Quail input
methods produce unified characters.

See also command `unify-8859-on-encoding-mode'."
  :group 'mule
  :global t
  :version 21.3				; who knows...?
  :init-value nil
  (let ((table (if unify-8859-on-decoding-mode ucs-mule-8859-to-mule-unicode)))
    (set-char-table-parent standard-translation-table-for-decode table)
    (setq-default translation-table-for-input table)))

(defun ucs-insert (arg)
  "Insert the Emacs character representation of the given Unicode.
Interactively, prompts for a hex string giving the code."
  (interactive "sUnicode (hex): ")
  (insert (or (decode-char 'ucs (if (integerp arg)
				    arg
				  (string-to-number arg 16)))
	      (error "Unknown Unicode character"))))

;;; Dealing with non-8859 character sets.

;; We only set up translation on encoding to utf-8.  Also translation
;; tables ucs-CS-encode-table are constructed for some coding systems
;; CS which could be used as `translation-table-for-encode', currently
;; for indian-is13194, lao, thai, tibetan-iso-8bit and
;; vietnamese-viscii.

;; The alists here cover both coding systems (external charsets), like
;; VISCII, and individual Emacs charsets, like `ipa'.
(let ((vietnamese-viscii
       '((?,1!(B . ?$,1o/(B)
	 (?,1"(B . ?$,1o1(B)
	 (?,1#(B . ?$,1o7(B)
	 (?,1$(B . ?$,1o%(B)
	 (?,1%(B . ?$,1o'(B)
	 (?,1&(B . ?$,1o)(B)
	 (?,1'(B . ?$,1o-(B)
	 (?,1((B . ?$,1o=(B)
	 (?,1)(B . ?$,1o9(B)
	 (?,1*(B . ?$,1o?(B)
	 (?,1+(B . ?$,1oA(B)
	 (?,1,(B . ?$,1oC(B)
	 (?,1-(B . ?$,1oE(B)
	 (?,1.(B . ?$,1oG(B)
	 (?,1/(B . ?$,1oQ(B)
	 (?,10(B . ?$,1oS(B)
	 (?,11(B . ?$,1oU(B)
	 (?,12(B . ?$,1oW(B)
	 (?,15(B . ?$,1oY(B)
	 (?,16(B . ?$,1o](B)
	 (?,17(B . ?$,1o_(B)
	 (?,18(B . ?$,1oK(B)
	 (?,1=(B . ?$,1!a(B)
	 (?,1>(B . ?$,1o[(B)
	 (?,1F(B . ?$,1o3(B)
	 (?,1G(B . ?$,1o5(B)
	 (?,1O(B . ?$,1os(B)
	 (?,1Q(B . ?$,1oi(B)
	 (?,1U(B . ?$,1o!(B)
	 (?,1V(B . ?$,1ow(B)
	 (?,1W(B . ?$,1ok(B)
	 (?,1X(B . ?$,1om(B)
	 (?,1[(B . ?$,1oy(B)
	 (?,1\(B . ?$,1ou(B)
	 (?,1^(B . ?$,1oa(B)
	 (?,1_(B . ?$,1!p(B)
	 (?,1`(B . ?,A`(B)
	 (?,1a(B . ?,Aa(B)
	 (?,1b(B . ?,Ab(B)
	 (?,1c(B . ?,Ac(B)
	 (?,1d(B . ?$,1o#(B)
	 (?,1e(B . ?$,1 #(B)
	 (?,1f(B . ?$,1oo(B)
	 (?,1g(B . ?$,1o+(B)
	 (?,1h(B . ?,Ah(B)
	 (?,1i(B . ?,Ai(B)
	 (?,1j(B . ?,Aj(B)
	 (?,1k(B . ?$,1o;(B)
	 (?,1l(B . ?,Al(B)
	 (?,1m(B . ?,Am(B)
	 (?,1n(B . ?$,1 I(B)
	 (?,1o(B . ?$,1oI(B)
	 (?,1p(B . ?$,1 1(B)
	 (?,1q(B . ?$,1oq(B)
	 (?,1r(B . ?,Ar(B)
	 (?,1s(B . ?,As(B)
	 (?,1t(B . ?,At(B)
	 (?,1u(B . ?,Au(B)
	 (?,1v(B . ?$,1oO(B)
	 (?,1w(B . ?$,1oM(B)
	 (?,1x(B . ?$,1oe(B)
	 (?,1y(B . ?,Ay(B)
	 (?,1z(B . ?,Az(B)
	 (?,1{(B . ?$,1!)(B)
	 (?,1|(B . ?$,1og(B)
	 (?,1}(B . ?,A}(B)
	 (?,1~(B . ?$,1oc(B)

	 (?,2!(B . ?$,1o.(B)
	 (?,2"(B . ?$,1o0(B)
	 (?,2#(B . ?$,1o6(B)
	 (?,2$(B . ?$,1o$(B)
	 (?,2%(B . ?$,1o&(B)
	 (?,2&(B . ?$,1o((B)
	 (?,2'(B . ?$,1o,(B)
	 (?,2((B . ?$,1o<(B)
	 (?,2)(B . ?$,1o8(B)
	 (?,2*(B . ?$,1o>(B)
	 (?,2+(B . ?$,1o@(B)
	 (?,2,(B . ?$,1oB(B)
	 (?,2-(B . ?$,1oD(B)
	 (?,2.(B . ?$,1oF(B)
	 (?,2/(B . ?$,1oP(B)
	 (?,20(B . ?$,1oR(B)
	 (?,21(B . ?$,1oT(B)
	 (?,22(B . ?$,1oV(B)
	 (?,25(B . ?$,1oX(B)
	 (?,26(B . ?$,1o\(B)
	 (?,27(B . ?$,1o^(B)
	 (?,28(B . ?$,1oJ(B)
	 (?,2=(B . ?$,1!`(B)
	 (?,2>(B . ?$,1oZ(B)
	 (?,2F(B . ?$,1o2(B)
	 (?,2G(B . ?$,1o4(B)
	 (?,2O(B . ?$,1or(B)
	 (?,2Q(B . ?$,1oh(B)
	 (?,2U(B . ?$,1o (B)
	 (?,2V(B . ?$,1ov(B)
	 (?,2W(B . ?$,1oj(B)
	 (?,2X(B . ?$,1ol(B)
	 (?,2[(B . ?$,1ox(B)
	 (?,2\(B . ?$,1ot(B)
	 (?,2^(B . ?$,1o`(B)
	 (?,2_(B . ?$,1!o(B)
	 (?,2`(B . ?,A@(B)
	 (?,2a(B . ?,AA(B)
	 (?,2b(B . ?,AB(B)
	 (?,2c(B . ?,AC(B)
	 (?,2d(B . ?$,1o"(B)
	 (?,2e(B . ?$,1 "(B)
	 (?,2f(B . ?$,1on(B)
	 (?,2g(B . ?$,1o*(B)
	 (?,2h(B . ?,AH(B)
	 (?,2i(B . ?,AI(B)
	 (?,2j(B . ?,AJ(B)
	 (?,2k(B . ?$,1o:(B)
	 (?,2l(B . ?,AL(B)
	 (?,2m(B . ?,AM(B)
	 (?,2n(B . ?$,1 H(B)
	 (?,2o(B . ?$,1oH(B)
	 (?,2p(B . ?$,1 0(B)
	 (?,2q(B . ?$,1op(B)
	 (?,2r(B . ?,AR(B)
	 (?,2s(B . ?,AS(B)
	 (?,2t(B . ?,AT(B)
	 (?,2u(B . ?,AU(B)
	 (?,2v(B . ?$,1oN(B)
	 (?,2w(B . ?$,1oL(B)
	 (?,2x(B . ?$,1od(B)
	 (?,2y(B . ?,AY(B)
	 (?,2z(B . ?,AZ(B)
	 (?,2{(B . ?$,1!((B)
	 (?,2|(B . ?$,1of(B)
	 (?,2}(B . ?,A](B)
	 (?,2~(B . ?$,1ob(B)))

      (thai-tis620
       '((?,T!(B . ?$,1Ba(B)
	 (?,T"(B . ?$,1Bb(B)
	 (?,T#(B . ?$,1Bc(B)
	 (?,T$(B . ?$,1Bd(B)
	 (?,T%(B . ?$,1Be(B)
	 (?,T&(B . ?$,1Bf(B)
	 (?,T'(B . ?$,1Bg(B)
	 (?,T((B . ?$,1Bh(B)
	 (?,T)(B . ?$,1Bi(B)
	 (?,T*(B . ?$,1Bj(B)
	 (?,T+(B . ?$,1Bk(B)
	 (?,T,(B . ?$,1Bl(B)
	 (?,T-(B . ?$,1Bm(B)
	 (?,T.(B . ?$,1Bn(B)
	 (?,T/(B . ?$,1Bo(B)
	 (?,T0(B . ?$,1Bp(B)
	 (?,T1(B . ?$,1Bq(B)
	 (?,T2(B . ?$,1Br(B)
	 (?,T3(B . ?$,1Bs(B)
	 (?,T4(B . ?$,1Bt(B)
	 (?,T5(B . ?$,1Bu(B)
	 (?,T6(B . ?$,1Bv(B)
	 (?,T7(B . ?$,1Bw(B)
	 (?,T8(B . ?$,1Bx(B)
	 (?,T9(B . ?$,1By(B)
	 (?,T:(B . ?$,1Bz(B)
	 (?,T;(B . ?$,1B{(B)
	 (?,T<(B . ?$,1B|(B)
	 (?,T=(B . ?$,1B}(B)
	 (?,T>(B . ?$,1B~(B)
	 (?,T?(B . ?$,1B(B)
	 (?,T@(B . ?$,1C (B)
	 (?,TA(B . ?$,1C!(B)
	 (?,TB(B . ?$,1C"(B)
	 (?,TC(B . ?$,1C#(B)
	 (?,TD(B . ?$,1C$(B)
	 (?,TE(B . ?$,1C%(B)
	 (?,TF(B . ?$,1C&(B)
	 (?,TG(B . ?$,1C'(B)
	 (?,TH(B . ?$,1C((B)
	 (?,TI(B . ?$,1C)(B)
	 (?,TJ(B . ?$,1C*(B)
	 (?,TK(B . ?$,1C+(B)
	 (?,TL(B . ?$,1C,(B)
	 (?,TM(B . ?$,1C-(B)
	 (?,TN(B . ?$,1C.(B)
	 (?,TO(B . ?$,1C/(B)
	 (?,TP(B . ?$,1C0(B)
	 (?,TQ(B . ?$,1C1(B)
	 (?,TR(B . ?$,1C2(B)
	 (?,TS(B . ?$,1C3(B)
	 (?,TT(B . ?$,1C4(B)
	 (?,TU(B . ?$,1C5(B)
	 (?,TV(B . ?$,1C6(B)
	 (?,TW(B . ?$,1C7(B)
	 (?,TX(B . ?$,1C8(B)
	 (?,TY(B . ?$,1C9(B)
	 (?,TZ(B . ?$,1C:(B)
	 (?,T_(B . ?$,1C?(B)
	 (?,T`(B . ?$,1C@(B)
	 (?,Ta(B . ?$,1CA(B)
	 (?,Tb(B . ?$,1CB(B)
	 (?,Tc(B . ?$,1CC(B)
	 (?,Td(B . ?$,1CD(B)
	 (?,Te(B . ?$,1CE(B)
	 (?,Tf(B . ?$,1CF(B)
	 (?,Tg(B . ?$,1CG(B)
	 (?,Th(B . ?$,1CH(B)
	 (?,Ti(B . ?$,1CI(B)
	 (?,Tj(B . ?$,1CJ(B)
	 (?,Tk(B . ?$,1CK(B)
	 (?,Tl(B . ?$,1CL(B)
	 (?,Tm(B . ?$,1CM(B)
	 (?,Tn(B . ?$,1CN(B)
	 (?,To(B . ?$,1CO(B)
	 (?,Tp(B . ?$,1CP(B)
	 (?,Tq(B . ?$,1CQ(B)
	 (?,Tr(B . ?$,1CR(B)
	 (?,Ts(B . ?$,1CS(B)
	 (?,Tt(B . ?$,1CT(B)
	 (?,Tu(B . ?$,1CU(B)
	 (?,Tv(B . ?$,1CV(B)
	 (?,Tw(B . ?$,1CW(B)
	 (?,Tx(B . ?$,1CX(B)
	 (?,Ty(B . ?$,1CY(B)
	 (?,Tz(B . ?$,1CZ(B)
	 (?,T{(B . ?$,1C[(B)))

      (tibetan-iso-8bit
       '((?$(7!0(B . ?$,1E@(B)
	 (?$(7!1(B . ?$,1EA(B)
	 (?$(7!2(B . ?$,1EB(B)
	 (?$(7!3(B . ?$,1EC(B)
	 (?$(7!4(B . ?$,1ED(B)
	 (?$(7!5(B . ?$,1EE(B)
	 (?$(7!6(B . ?$,1EF(B)
	 (?$(7!7(B . ?$,1EG(B)
	 (?$(7!8(B . ?$,1EH(B)
	 (?$(7!9(B . ?$,1EI(B)
	 (?$(7!:(B . ?$,1EJ(B)
	 (?$(7!;(B . ?$,1EK(B)
	 (?$(7!<(B . ?$,1EL(B)
	 (?$(7!=(B . ?$,1EM(B)
	 (?$(7!>(B . ?$,1EN(B)
	 (?$(7!?(B . ?$,1EO(B)
	 (?$(7!@(B . ?$,1EP(B)
	 (?$(7!A(B . ?$,1EQ(B)
	 (?$(7!B(B . ?$,1ER(B)
	 (?$(7!C(B . ?$,1ES(B)
	 (?$(7!D(B . ?$,1ET(B)
	 (?$(7!E(B . ?$,1EU(B)
	 (?$(7!F(B . ?$,1EV(B)
	 (?$(7!G(B . ?$,1EW(B)
	 (?$(7!H(B . ?$,1EX(B)
	 (?$(7!I(B . ?$,1EY(B)
	 (?$(7!J(B . ?$,1EZ(B)
	 (?$(7!K(B . ?$,1E[(B)
	 (?$(7!L(B . ?$,1E\(B)
	 (?$(7!M(B . ?$,1E](B)
	 (?$(7!N(B . ?$,1E^(B)
	 (?$(7!O(B . ?$,1E_(B)
	 (?$(7!P(B . ?$,1E`(B)
	 (?$(7!Q(B . ?$,1Ea(B)
	 (?$(7!R(B . ?$,1Eb(B)
	 (?$(7!S(B . ?$,1Ec(B)
	 (?$(7!T(B . ?$,1Ed(B)
	 (?$(7!U(B . ?$,1Ee(B)
	 (?$(7!V(B . ?$,1Ef(B)
	 (?$(7!W(B . ?$,1Eg(B)
	 (?$(7!X(B . ?$,1Eh(B)
	 (?$(7!Y(B . ?$,1Ei(B)
	 (?$(7!Z(B . ?$,1Ej(B)
	 (?$(7![(B . ?$,1Ek(B)
	 (?$(7!\(B . ?$,1El(B)
	 (?$(7!](B . ?$,1Em(B)
	 (?$(7!^(B . ?$,1En(B)
	 (?$(7!_(B . ?$,1Eo(B)
	 (?$(7!`(B . ?$,1Ep(B)
	 (?$(7!a(B . ?$,1Eq(B)
	 (?$(7!b(B . ?$,1Er(B)
	 (?$(7!c(B . ?$,1Es(B)
	 (?$(7!d(B . ?$,1Et(B)
	 (?$(7!e(B . ?$,1Eu(B)
	 (?$(7!f(B . ?$,1Ev(B)
	 (?$(7!g(B . ?$,1Ew(B)
	 (?$(7!h(B . ?$,1Ex(B)
	 (?$(7!i(B . ?$,1Ey(B)
	 (?$(7!j(B . ?$,1Ez(B)
	 (?$(7!k(B . ?$,1E{(B)
	 (?$(7!l(B . ?$,1E|(B)
	 (?$(7!m(B . ?$,1E}(B)
	 (?$(7!n(B . ?$,1E~(B)
	 (?$(7!o(B . ?$,1E(B)
	 (?$(7"!(B . ?$,1F (B)
	 (?$(7""(B . ?$,1F!(B)
	 (?$(7"#(B . ?$,1F"(B)
	 (?$(7"$(B . ?$,1F#(B)
	 (?$(7"%(B . ?$,1F$(B)
	 (?$(7"&(B . ?$,1F%(B)
	 (?$(7"'(B . ?$,1F&(B)
	 (?$(7"((B . ?$,1F'(B)
	 (?$(7"*(B . ?$,1F)(B)
	 (?$(7"+(B . ?$,1F*(B)
	 (?$(7",(B . ?$,1F+(B)
	 (?$(7"-(B . ?$,1F,(B)
	 (?$(7".(B . ?$,1F-(B)
	 (?$(7"/(B . ?$,1F.(B)
	 (?$(7"0(B . ?$,1F/(B)
	 (?$(7"1(B . ?$,1F0(B)
	 (?$(7"2(B . ?$,1F1(B)
	 (?$(7"3(B . ?$,1F2(B)
	 (?$(7"4(B . ?$,1F3(B)
	 (?$(7"5(B . ?$,1F4(B)
	 (?$(7"6(B . ?$,1F5(B)
	 (?$(7"7(B . ?$,1F6(B)
	 (?$(7"8(B . ?$,1F7(B)
	 (?$(7"9(B . ?$,1F8(B)
	 (?$(7":(B . ?$,1F9(B)
	 (?$(7";(B . ?$,1F:(B)
	 (?$(7"<(B . ?$,1F;(B)
	 (?$(7"=(B . ?$,1F<(B)
	 (?$(7">(B . ?$,1F=(B)
	 (?$(7"?(B . ?$,1F>(B)
	 (?$(7"@(B . ?$,1F?(B)
	 (?$(7"A(B . ?$,1F@(B)
	 (?$(7"B(B . ?$,1FA(B)
	 (?$(7"C(B . ?$,1FB(B)
	 (?$(7"D(B . ?$,1FC(B)
	 (?$(7"E(B . ?$,1FD(B)
	 (?$(7"F(B . ?$,1FE(B)
	 (?$(7"G(B . ?$,1FF(B)
	 (?$(7"H(B . ?$,1FG(B)
	 (?$(7"I(B . ?$,1FH(B)
	 (?$(7"J(B . ?$,1FI(B)
	 (?$(7"K(B . ?$,1FJ(B)
	 (?$(7"R(B . ?$,1FQ(B)
	 (?$(7"S(B . ?$,1FR(B)
	 (?$(7"T(B . ?$,1FS(B)
	 (?$(7"U(B . ?$,1FT(B)
	 (?$(7"V(B . ?$,1FU(B)
	 (?$(7"W(B . ?$,1FV(B)
	 (?$(7"X(B . ?$,1FW(B)
	 (?$(7"Y(B . ?$,1FX(B)
	 (?$(7"Z(B . ?$,1FY(B)
	 (?$(7"[(B . ?$,1FZ(B)
	 (?$(7"\(B . ?$,1F[(B)
	 (?$(7"](B . ?$,1F\(B)
	 (?$(7"^(B . ?$,1F](B)
	 (?$(7"_(B . ?$,1F^(B)
	 (?$(7"`(B . ?$,1F_(B)
	 (?$(7"a(B . ?$,1F`(B)
	 (?$(7"b(B . ?$,1Fa(B)
	 (?$(7"c(B . ?$,1Fb(B)
	 (?$(7"d(B . ?$,1Fc(B)
	 (?$(7"e(B . ?$,1Fd(B)
	 (?$(7"f(B . ?$,1Fe(B)
	 (?$(7"g(B . ?$,1Ff(B)
	 (?$(7"h(B . ?$,1Fg(B)
	 (?$(7"i(B . ?$,1Fh(B)
	 (?$(7"j(B . ?$,1Fi(B)
	 (?$(7"k(B . ?$,1Fj(B)
	 (?$(7"l(B . ?$,1Fk(B)
	 (?$(7#!(B . ?$,1Fp(B)
	 (?$(7#"(B . ?$,1Fq(B)
	 (?$(7##(B . ?$,1Fr(B)
	 (?$(7#$(B . ?$,1Fs(B)
	 (?$(7#%(B . ?$,1Ft(B)
	 (?$(7#&(B . ?$,1Fu(B)
	 (?$(7#'(B . ?$,1Fv(B)
	 (?$(7#((B . ?$,1Fw(B)
	 (?$(7#*(B . ?$,1Fy(B)
	 (?$(7#+(B . ?$,1Fz(B)
	 (?$(7#,(B . ?$,1F{(B)
	 (?$(7#-(B . ?$,1F|(B)
	 (?$(7#.(B . ?$,1F}(B)
	 (?$(7#/(B . ?$,1F~(B)
	 (?$(7#0(B . ?$,1F(B)
	 (?$(7#1(B . ?$,1G (B)
	 (?$(7#2(B . ?$,1G!(B)
	 (?$(7#3(B . ?$,1G"(B)
	 (?$(7#4(B . ?$,1G#(B)
	 (?$(7#5(B . ?$,1G$(B)
	 (?$(7#6(B . ?$,1G%(B)
	 (?$(7#7(B . ?$,1G&(B)
	 (?$(7#8(B . ?$,1G'(B)
	 (?$(7#9(B . ?$,1G((B)
	 (?$(7#:(B . ?$,1G)(B)
	 (?$(7#;(B . ?$,1G*(B)
	 (?$(7#<(B . ?$,1G+(B)
	 (?$(7#=(B . ?$,1G,(B)
	 (?$(7#>(B . ?$,1G-(B)
	 (?$(7#?(B . ?$,1G.(B)
	 (?$(7#@(B . ?$,1G/(B)
	 (?$(7#A(B . ?$,1G0(B)
	 (?$(7#B(B . ?$,1G1(B)
	 (?$(7#C(B . ?$,1G2(B)
	 (?$(7#D(B . ?$,1G3(B)
	 (?$(7#E(B . ?$,1G4(B)
	 (?$(7#F(B . ?$,1G5(B)
	 (?$(7#G(B . ?$,1G6(B)
	 (?$(7#H(B . ?$,1G7(B)
	 (?$(7#I(B . ?$,1G8(B)
	 (?$(7#J(B . ?$,1G9(B)
	 (?$(7#K(B . ?$,1G:(B)
	 (?$(7#L(B . ?$,1G;(B)
	 (?$(7#M(B . ?$,1G<(B)
	 (?$(7#O(B . ?$,1G>(B)
	 (?$(7#P(B . ?$,1G?(B)
	 (?$(7#Q(B . ?$,1G@(B)
	 (?$(7#R(B . ?$,1GA(B)
	 (?$(7#S(B . ?$,1GB(B)
	 (?$(7#T(B . ?$,1GC(B)
	 (?$(7#U(B . ?$,1GD(B)
	 (?$(7#V(B . ?$,1GE(B)
	 (?$(7#W(B . ?$,1GF(B)
	 (?$(7#X(B . ?$,1GG(B)
	 (?$(7#Y(B . ?$,1GH(B)
	 (?$(7#Z(B . ?$,1GI(B)
	 (?$(7#[(B . ?$,1GJ(B)
	 (?$(7#\(B . ?$,1GK(B)
	 (?$(7#](B . ?$,1GL(B)
	 (?$(7#`(B . ?$,1GO(B)))

      (ipa
       '((?,0 (B . ?i)
	 (?,0!(B . ?$,1#j(B)
	 (?,0"(B . ?e)
	 (?,0#(B . ?$,1#[(B)
	 (?,0$(B . ?,Af(B)
	 (?,0%(B . ?a)
	 (?,0&(B . ?$,1#h(B)
	 (?,0'(B . ?$,1#Y(B)
	 (?,0((B . ?$,1#P(B)
	 (?,0)(B . ?$,1#o(B)
	 (?,0*(B . ?$,1#d(B)
	 (?,0+(B . ?$,1$,(B)
	 (?,0,(B . ?$,1#Q(B)
	 (?,0-(B . ?y)
	 (?,0.(B . ?$,1$/(B)
	 (?,0/(B . ?,Ax(B)
	 (?,00(B . ?$,1 s(B)
	 (?,01(B . ?$,1#v(B)
	 (?,02(B . ?$,1$)(B)
	 (?,03(B . ?$,1#u(B)
	 (?,04(B . ?u)
	 (?,05(B . ?$,1$*(B)
	 (?,06(B . ?o)
	 (?,07(B . ?$,1#T(B)
	 (?,08(B . ?$,1#R(B)
	 (?,0:(B . ?$,1#Z(B)
	 (?,0@(B . ?p)
	 (?,0A(B . ?b)
	 (?,0B(B . ?t)
	 (?,0C(B . ?d)
	 (?,0D(B . ?k)
	 (?,0E(B . ?g)
	 (?,0F(B . ?f)
	 (?,0G(B . ?v)
	 (?,0H(B . ?$,1'8(B)
	 (?,0I(B . ?,Ap(B)
	 (?,0J(B . ?s)
	 (?,0K(B . ?z)
	 (?,0L(B . ?$,1$#(B)
	 (?,0M(B . ?$,1$2(B)
	 (?,0N(B . ?,Ag(B)
	 (?,0O(B . ?x)
	 (?,0P(B . ?$,1$!(B)
	 (?,0Q(B . ?h)
	 (?,0R(B . ?m)
	 (?,0S(B . ?n)
	 (?,0T(B . ?$,1#r(B)
	 (?,0U(B . ?$,1 k(B)
	 (?,0V(B . ?r)
	 (?,0W(B . ?$,1$ (B)
	 (?,0X(B . ?$,1#y(B)
	 (?,0Y(B . ?j)
	 (?,0Z(B . ?l)
	 (?,0[(B . ?$,1$.(B)
	 (?,0\(B . ?$,1$?(B)
	 (?,0](B . ?$,1#e(B)
	 (?,0^(B . ?w)
	 (?,0_(B . ?$,1$-(B)
	 (?,0p(B . ?$,1$h(B)
	 (?,0q(B . ?$,1$l(B)
	 (?,0r(B . ?$,1$p(B)))

      (ethiopic
       '((?$(3!!(B . ?$,1M@(B)
	 (?$(3!"(B . ?$,1MA(B)
	 (?$(3!#(B . ?$,1MB(B)
	 (?$(3!$(B . ?$,1MC(B)
	 (?$(3!%(B . ?$,1MD(B)
	 (?$(3!&(B . ?$,1ME(B)
	 (?$(3!'(B . ?$,1MF(B)
	 (?$(3!)(B . ?$,1MH(B)
	 (?$(3!*(B . ?$,1MI(B)
	 (?$(3!+(B . ?$,1MJ(B)
	 (?$(3!,(B . ?$,1MK(B)
	 (?$(3!-(B . ?$,1ML(B)
	 (?$(3!.(B . ?$,1MM(B)
	 (?$(3!/(B . ?$,1MN(B)
	 (?$(3!0(B . ?$,1MO(B)
	 (?$(3!1(B . ?$,1MP(B)
	 (?$(3!2(B . ?$,1MQ(B)
	 (?$(3!3(B . ?$,1MR(B)
	 (?$(3!4(B . ?$,1MS(B)
	 (?$(3!5(B . ?$,1MT(B)
	 (?$(3!6(B . ?$,1MU(B)
	 (?$(3!7(B . ?$,1MV(B)
	 (?$(3!8(B . ?$,1MW(B)
	 (?$(3!9(B . ?$,1MX(B)
	 (?$(3!:(B . ?$,1MY(B)
	 (?$(3!;(B . ?$,1MZ(B)
	 (?$(3!<(B . ?$,1M[(B)
	 (?$(3!=(B . ?$,1M\(B)
	 (?$(3!>(B . ?$,1M](B)
	 (?$(3!?(B . ?$,1M^(B)
	 (?$(3!@(B . ?$,1M_(B)
	 (?$(3!A(B . ?$,1M`(B)
	 (?$(3!B(B . ?$,1Ma(B)
	 (?$(3!C(B . ?$,1Mb(B)
	 (?$(3!D(B . ?$,1Mc(B)
	 (?$(3!E(B . ?$,1Md(B)
	 (?$(3!F(B . ?$,1Me(B)
	 (?$(3!G(B . ?$,1Mf(B)
	 (?$(3!H(B . ?$,1Mg(B)
	 (?$(3!I(B . ?$,1Mh(B)
	 (?$(3!J(B . ?$,1Mi(B)
	 (?$(3!K(B . ?$,1Mj(B)
	 (?$(3!L(B . ?$,1Mk(B)
	 (?$(3!M(B . ?$,1Ml(B)
	 (?$(3!N(B . ?$,1Mm(B)
	 (?$(3!O(B . ?$,1Mn(B)
	 (?$(3!P(B . ?$,1Mo(B)
	 (?$(3!Q(B . ?$,1Mp(B)
	 (?$(3!R(B . ?$,1Mq(B)
	 (?$(3!S(B . ?$,1Mr(B)
	 (?$(3!T(B . ?$,1Ms(B)
	 (?$(3!U(B . ?$,1Mt(B)
	 (?$(3!V(B . ?$,1Mu(B)
	 (?$(3!W(B . ?$,1Mv(B)
	 (?$(3!X(B . ?$,1Mw(B)
	 (?$(3!Y(B . ?$,1Mx(B)
	 (?$(3!Z(B . ?$,1My(B)
	 (?$(3![(B . ?$,1Mz(B)
	 (?$(3!\(B . ?$,1M{(B)
	 (?$(3!](B . ?$,1M|(B)
	 (?$(3!^(B . ?$,1M}(B)
	 (?$(3!_(B . ?$,1M~(B)
	 (?$(3!`(B . ?$,1M(B)
	 (?$(3!a(B . ?$,1N (B)
	 (?$(3!b(B . ?$,1N!(B)
	 (?$(3!c(B . ?$,1N"(B)
	 (?$(3!d(B . ?$,1N#(B)
	 (?$(3!e(B . ?$,1N$(B)
	 (?$(3!f(B . ?$,1N%(B)
	 (?$(3!g(B . ?$,1N&(B)
	 (?$(3!i(B . ?$,1N((B)
	 (?$(3!k(B . ?$,1N*(B)
	 (?$(3!l(B . ?$,1N+(B)
	 (?$(3!m(B . ?$,1N,(B)
	 (?$(3!n(B . ?$,1N-(B)
	 (?$(3!q(B . ?$,1N0(B)
	 (?$(3!r(B . ?$,1N1(B)
	 (?$(3!s(B . ?$,1N2(B)
	 (?$(3!t(B . ?$,1N3(B)
	 (?$(3!u(B . ?$,1N4(B)
	 (?$(3!v(B . ?$,1N5(B)
	 (?$(3!w(B . ?$,1N6(B)
	 (?$(3!y(B . ?$,1N8(B)
	 (?$(3!{(B . ?$,1N:(B)
	 (?$(3!|(B . ?$,1N;(B)
	 (?$(3!}(B . ?$,1N<(B)
	 (?$(3!~(B . ?$,1N=(B)
	 (?$(3"#(B . ?$,1N@(B)
	 (?$(3"$(B . ?$,1NA(B)
	 (?$(3"%(B . ?$,1NB(B)
	 (?$(3"&(B . ?$,1NC(B)
	 (?$(3"'(B . ?$,1ND(B)
	 (?$(3"((B . ?$,1NE(B)
	 (?$(3")(B . ?$,1NF(B)
	 (?$(3"*(B . ?$,1NG(B)
	 (?$(3"+(B . ?$,1NH(B)
	 (?$(3",(B . ?$,1NI(B)
	 (?$(3"-(B . ?$,1NJ(B)
	 (?$(3".(B . ?$,1NK(B)
	 (?$(3"/(B . ?$,1NL(B)
	 (?$(3"0(B . ?$,1NM(B)
	 (?$(3"1(B . ?$,1NN(B)
	 (?$(3"2(B . ?$,1NO(B)
	 (?$(3"3(B . ?$,1NP(B)
	 (?$(3"4(B . ?$,1NQ(B)
	 (?$(3"5(B . ?$,1NR(B)
	 (?$(3"6(B . ?$,1NS(B)
	 (?$(3"7(B . ?$,1NT(B)
	 (?$(3"8(B . ?$,1NU(B)
	 (?$(3"9(B . ?$,1NV(B)
	 (?$(3":(B . ?$,1NW(B)
	 (?$(3";(B . ?$,1NX(B)
	 (?$(3"<(B . ?$,1NY(B)
	 (?$(3"=(B . ?$,1NZ(B)
	 (?$(3">(B . ?$,1N[(B)
	 (?$(3"?(B . ?$,1N\(B)
	 (?$(3"@(B . ?$,1N](B)
	 (?$(3"A(B . ?$,1N^(B)
	 (?$(3"B(B . ?$,1N_(B)
	 (?$(3"C(B . ?$,1N`(B)
	 (?$(3"D(B . ?$,1Na(B)
	 (?$(3"E(B . ?$,1Nb(B)
	 (?$(3"F(B . ?$,1Nc(B)
	 (?$(3"G(B . ?$,1Nd(B)
	 (?$(3"H(B . ?$,1Ne(B)
	 (?$(3"I(B . ?$,1Nf(B)
	 (?$(3"K(B . ?$,1Nh(B)
	 (?$(3"M(B . ?$,1Nj(B)
	 (?$(3"N(B . ?$,1Nk(B)
	 (?$(3"O(B . ?$,1Nl(B)
	 (?$(3"P(B . ?$,1Nm(B)
	 (?$(3"S(B . ?$,1Np(B)
	 (?$(3"T(B . ?$,1Nq(B)
	 (?$(3"U(B . ?$,1Nr(B)
	 (?$(3"V(B . ?$,1Ns(B)
	 (?$(3"W(B . ?$,1Nt(B)
	 (?$(3"X(B . ?$,1Nu(B)
	 (?$(3"Y(B . ?$,1Nv(B)
	 (?$(3"Z(B . ?$,1Nw(B)
	 (?$(3"[(B . ?$,1Nx(B)
	 (?$(3"\(B . ?$,1Ny(B)
	 (?$(3"](B . ?$,1Nz(B)
	 (?$(3"^(B . ?$,1N{(B)
	 (?$(3"_(B . ?$,1N|(B)
	 (?$(3"`(B . ?$,1N}(B)
	 (?$(3"a(B . ?$,1N~(B)
	 (?$(3"b(B . ?$,1N(B)
	 (?$(3"c(B . ?$,1O (B)
	 (?$(3"d(B . ?$,1O!(B)
	 (?$(3"e(B . ?$,1O"(B)
	 (?$(3"f(B . ?$,1O#(B)
	 (?$(3"g(B . ?$,1O$(B)
	 (?$(3"h(B . ?$,1O%(B)
	 (?$(3"i(B . ?$,1O&(B)
	 (?$(3"j(B . ?$,1O'(B)
	 (?$(3"k(B . ?$,1O((B)
	 (?$(3"l(B . ?$,1O)(B)
	 (?$(3"m(B . ?$,1O*(B)
	 (?$(3"n(B . ?$,1O+(B)
	 (?$(3"o(B . ?$,1O,(B)
	 (?$(3"p(B . ?$,1O-(B)
	 (?$(3"q(B . ?$,1O.(B)
	 (?$(3"s(B . ?$,1O0(B)
	 (?$(3"u(B . ?$,1O2(B)
	 (?$(3"v(B . ?$,1O3(B)
	 (?$(3"w(B . ?$,1O4(B)
	 (?$(3"x(B . ?$,1O5(B)
	 (?$(3"{(B . ?$,1O8(B)
	 (?$(3"|(B . ?$,1O9(B)
	 (?$(3"}(B . ?$,1O:(B)
	 (?$(3"~(B . ?$,1O;(B)
	 (?$(3#!(B . ?$,1O<(B)
	 (?$(3#"(B . ?$,1O=(B)
	 (?$(3##(B . ?$,1O>(B)
	 (?$(3#%(B . ?$,1O@(B)
	 (?$(3#'(B . ?$,1OB(B)
	 (?$(3#((B . ?$,1OC(B)
	 (?$(3#)(B . ?$,1OD(B)
	 (?$(3#*(B . ?$,1OE(B)
	 (?$(3#-(B . ?$,1OH(B)
	 (?$(3#.(B . ?$,1OI(B)
	 (?$(3#/(B . ?$,1OJ(B)
	 (?$(3#0(B . ?$,1OK(B)
	 (?$(3#1(B . ?$,1OL(B)
	 (?$(3#2(B . ?$,1OM(B)
	 (?$(3#3(B . ?$,1ON(B)
	 (?$(3#5(B . ?$,1OP(B)
	 (?$(3#6(B . ?$,1OQ(B)
	 (?$(3#7(B . ?$,1OR(B)
	 (?$(3#8(B . ?$,1OS(B)
	 (?$(3#9(B . ?$,1OT(B)
	 (?$(3#:(B . ?$,1OU(B)
	 (?$(3#;(B . ?$,1OV(B)
	 (?$(3#=(B . ?$,1OX(B)
	 (?$(3#>(B . ?$,1OY(B)
	 (?$(3#?(B . ?$,1OZ(B)
	 (?$(3#@(B . ?$,1O[(B)
	 (?$(3#A(B . ?$,1O\(B)
	 (?$(3#B(B . ?$,1O](B)
	 (?$(3#C(B . ?$,1O^(B)
	 (?$(3#D(B . ?$,1O_(B)
	 (?$(3#E(B . ?$,1O`(B)
	 (?$(3#F(B . ?$,1Oa(B)
	 (?$(3#G(B . ?$,1Ob(B)
	 (?$(3#H(B . ?$,1Oc(B)
	 (?$(3#I(B . ?$,1Od(B)
	 (?$(3#J(B . ?$,1Oe(B)
	 (?$(3#K(B . ?$,1Of(B)
	 (?$(3#L(B . ?$,1Og(B)
	 (?$(3#M(B . ?$,1Oh(B)
	 (?$(3#N(B . ?$,1Oi(B)
	 (?$(3#O(B . ?$,1Oj(B)
	 (?$(3#P(B . ?$,1Ok(B)
	 (?$(3#Q(B . ?$,1Ol(B)
	 (?$(3#R(B . ?$,1Om(B)
	 (?$(3#S(B . ?$,1On(B)
	 (?$(3#U(B . ?$,1Op(B)
	 (?$(3#V(B . ?$,1Oq(B)
	 (?$(3#W(B . ?$,1Or(B)
	 (?$(3#X(B . ?$,1Os(B)
	 (?$(3#Y(B . ?$,1Ot(B)
	 (?$(3#Z(B . ?$,1Ou(B)
	 (?$(3#[(B . ?$,1Ov(B)
	 (?$(3#\(B . ?$,1Ow(B)
	 (?$(3#](B . ?$,1Ox(B)
	 (?$(3#^(B . ?$,1Oy(B)
	 (?$(3#_(B . ?$,1Oz(B)
	 (?$(3#`(B . ?$,1O{(B)
	 (?$(3#a(B . ?$,1O|(B)
	 (?$(3#b(B . ?$,1O}(B)
	 (?$(3#c(B . ?$,1O~(B)
	 (?$(3#d(B . ?$,1O(B)
	 (?$(3#e(B . ?$,1P (B)
	 (?$(3#f(B . ?$,1P!(B)
	 (?$(3#g(B . ?$,1P"(B)
	 (?$(3#h(B . ?$,1P#(B)
	 (?$(3#i(B . ?$,1P$(B)
	 (?$(3#j(B . ?$,1P%(B)
	 (?$(3#k(B . ?$,1P&(B)
	 (?$(3#l(B . ?$,1P'(B)
	 (?$(3#m(B . ?$,1P((B)
	 (?$(3#n(B . ?$,1P)(B)
	 (?$(3#o(B . ?$,1P*(B)
	 (?$(3#p(B . ?$,1P+(B)
	 (?$(3#q(B . ?$,1P,(B)
	 (?$(3#r(B . ?$,1P-(B)
	 (?$(3#s(B . ?$,1P.(B)
	 (?$(3#u(B . ?$,1P0(B)
	 (?$(3#w(B . ?$,1P2(B)
	 (?$(3#x(B . ?$,1P3(B)
	 (?$(3#y(B . ?$,1P4(B)
	 (?$(3#z(B . ?$,1P5(B)
	 (?$(3#}(B . ?$,1P8(B)
	 (?$(3#~(B . ?$,1P9(B)
	 (?$(3$!(B . ?$,1P:(B)
	 (?$(3$"(B . ?$,1P;(B)
	 (?$(3$#(B . ?$,1P<(B)
	 (?$(3$$(B . ?$,1P=(B)
	 (?$(3$%(B . ?$,1P>(B)
	 (?$(3$'(B . ?$,1P@(B)
	 (?$(3$((B . ?$,1PA(B)
	 (?$(3$)(B . ?$,1PB(B)
	 (?$(3$*(B . ?$,1PC(B)
	 (?$(3$+(B . ?$,1PD(B)
	 (?$(3$,(B . ?$,1PE(B)
	 (?$(3$-(B . ?$,1PF(B)
	 (?$(3$.(B . ?$,1PG(B)
	 (?$(3$/(B . ?$,1PH(B)
	 (?$(3$0(B . ?$,1PI(B)
	 (?$(3$1(B . ?$,1PJ(B)
	 (?$(3$2(B . ?$,1PK(B)
	 (?$(3$3(B . ?$,1PL(B)
	 (?$(3$4(B . ?$,1PM(B)
	 (?$(3$5(B . ?$,1PN(B)
	 (?$(3$6(B . ?$,1PO(B)
	 (?$(3$7(B . ?$,1PP(B)
	 (?$(3$8(B . ?$,1PQ(B)
	 (?$(3$9(B . ?$,1PR(B)
	 (?$(3$:(B . ?$,1PS(B)
	 (?$(3$;(B . ?$,1PT(B)
	 (?$(3$<(B . ?$,1PU(B)
	 (?$(3$=(B . ?$,1PV(B)
	 (?$(3$>(B . ?$,1PW(B)
	 (?$(3$?(B . ?$,1PX(B)
	 (?$(3$@(B . ?$,1PY(B)
	 (?$(3$A(B . ?$,1PZ(B)
	 (?$(3$B(B . ?$,1P[(B)
	 (?$(3$C(B . ?$,1P\(B)
	 (?$(3$D(B . ?$,1P](B)
	 (?$(3$E(B . ?$,1P^(B)
	 (?$(3$F(B . ?$,1P_(B)
	 (?$(3$G(B . ?$,1P`(B)
	 (?$(3$H(B . ?$,1Pa(B)
	 (?$(3$I(B . ?$,1Pb(B)
	 (?$(3$J(B . ?$,1Pc(B)
	 (?$(3$K(B . ?$,1Pd(B)
	 (?$(3$L(B . ?$,1Pe(B)
	 (?$(3$M(B . ?$,1Pf(B)
	 (?$(3$O(B . ?$,1Ph(B)
	 (?$(3$P(B . ?$,1Pi(B)
	 (?$(3$Q(B . ?$,1Pj(B)
	 (?$(3$R(B . ?$,1Pk(B)
	 (?$(3$S(B . ?$,1Pl(B)
	 (?$(3$T(B . ?$,1Pm(B)
	 (?$(3$U(B . ?$,1Pn(B)
	 (?$(3$V(B . ?$,1Po(B)
	 (?$(3$W(B . ?$,1Pp(B)
	 (?$(3$X(B . ?$,1Pq(B)
	 (?$(3$Y(B . ?$,1Pr(B)
	 (?$(3$Z(B . ?$,1Ps(B)
	 (?$(3$[(B . ?$,1Pt(B)
	 (?$(3$\(B . ?$,1Pu(B)
	 (?$(3$](B . ?$,1Pv(B)
	 (?$(3$^(B . ?$,1Pw(B)
	 (?$(3$_(B . ?$,1Px(B)
	 (?$(3$`(B . ?$,1Py(B)
	 (?$(3$a(B . ?$,1Pz(B)
	 (?$(3$h(B . ?$,1Q!(B)
	 (?$(3$i(B . ?$,1Q"(B)
	 (?$(3$j(B . ?$,1Q#(B)
	 (?$(3$k(B . ?$,1Q$(B)
	 (?$(3$l(B . ?$,1Q%(B)
	 (?$(3$m(B . ?$,1Q&(B)
	 (?$(3$n(B . ?$,1Q'(B)
	 (?$(3$o(B . ?$,1Q((B)
	 (?$(3$p(B . ?$,1Q)(B)
	 (?$(3$q(B . ?$,1Q*(B)
	 (?$(3$r(B . ?$,1Q+(B)
	 (?$(3$s(B . ?$,1Q,(B)
	 (?$(3$t(B . ?$,1Q-(B)
	 (?$(3$u(B . ?$,1Q.(B)
	 (?$(3$v(B . ?$,1Q/(B)
	 (?$(3$w(B . ?$,1Q0(B)
	 (?$(3$x(B . ?$,1Q1(B)
	 (?$(3$y(B . ?$,1Q2(B)
	 (?$(3$z(B . ?$,1Q3(B)
	 (?$(3${(B . ?$,1Q4(B)
	 (?$(3$|(B . ?$,1Q5(B)
	 (?$(3$}(B . ?$,1Q6(B)
	 (?$(3$~(B . ?$,1Q7(B)
	 (?$(3%!(B . ?$,1Q8(B)
	 (?$(3%"(B . ?$,1Q9(B)
	 (?$(3%#(B . ?$,1Q:(B)
	 (?$(3%$(B . ?$,1Q;(B)
	 (?$(3%%(B . ?$,1Q<(B)))

      (indian-is13194
       '((?(5!(B . ?$,15A(B)
	 (?(5"(B . ?$,15B(B)
	 (?(5#(B . ?$,15C(B)
	 (?(5$(B . ?$,15E(B)
	 (?(5%(B . ?$,15F(B)
	 (?(5&(B . ?$,15G(B)
	 (?(5'(B . ?$,15H(B)
	 (?(5((B . ?$,15I(B)
	 (?(5)(B . ?$,15J(B)
	 (?(5*(B . ?$,15K(B)
	 (?(5+(B . ?$,15N(B)
	 (?(5,(B . ?$,15O(B)
	 (?(5-(B . ?$,15P(B)
	 (?(5.(B . ?$,15M(B)
	 (?(5/(B . ?$,15R(B)
	 (?(50(B . ?$,15S(B)
	 (?(51(B . ?$,15T(B)
	 (?(52(B . ?$,15M(B)
	 (?(53(B . ?$,15U(B)
	 (?(54(B . ?$,15V(B)
	 (?(55(B . ?$,15W(B)
	 (?(56(B . ?$,15X(B)
	 (?(57(B . ?$,15Y(B)
	 (?(58(B . ?$,15Z(B)
	 (?(59(B . ?$,15[(B)
	 (?(5:(B . ?$,15\(B)
	 (?(5;(B . ?$,15](B)
	 (?(5<(B . ?$,15^(B)
	 (?(5=(B . ?$,15_(B)
	 (?(5>(B . ?$,15`(B)
	 (?(5?(B . ?$,15a(B)
	 (?(5@(B . ?$,15b(B)
	 (?(5A(B . ?$,15c(B)
	 (?(5B(B . ?$,15d(B)
	 (?(5C(B . ?$,15e(B)
	 (?(5D(B . ?$,15f(B)
	 (?(5E(B . ?$,15g(B)
	 (?(5F(B . ?$,15h(B)
	 (?(5G(B . ?$,15i(B)
	 (?(5H(B . ?$,15j(B)
	 (?(5I(B . ?$,15k(B)
	 (?(5J(B . ?$,15l(B)
	 (?(5K(B . ?$,15m(B)
	 (?(5L(B . ?$,15n(B)
	 (?(5M(B . ?$,15o(B)
	 (?(5N(B . ?$,16?(B)
	 (?(5O(B . ?$,15p(B)
	 (?(5P(B . ?$,15q(B)
	 (?(5Q(B . ?$,15r(B)
	 (?(5R(B . ?$,15s(B)
	 (?(5S(B . ?$,15t(B)
	 (?(5T(B . ?$,15u(B)
	 (?(5U(B . ?$,15v(B)
	 (?(5V(B . ?$,15w(B)
	 (?(5W(B . ?$,15x(B)
	 (?(5X(B . ?$,15y(B)
	 (?(5Z(B . ?$,15~(B)
	 (?(5[(B . ?$,15(B)
	 (?(5\(B . ?$,16 (B)
	 (?(5](B . ?$,16!(B)
	 (?(5^(B . ?$,16"(B)
	 (?(5_(B . ?$,16#(B)
	 (?(5`(B . ?$,16&(B)
	 (?(5a(B . ?$,16'(B)
	 (?(5b(B . ?$,16((B)
	 (?(5c(B . ?$,16%(B)
	 (?(5d(B . ?$,16*(B)
	 (?(5e(B . ?$,16+(B)
	 (?(5f(B . ?$,16,(B)
	 (?(5g(B . ?$,16)(B)
	 (?(5h(B . ?$,16-(B)
	 (?(5i(B . ?$,15|(B)
	 (?(5j(B . ?$,16D(B)
	 (?(5q(B . ?$,16F(B)
	 (?(5r(B . ?$,16G(B)
	 (?(5s(B . ?$,16H(B)
	 (?(5t(B . ?$,16I(B)
	 (?(5u(B . ?$,16J(B)
	 (?(5v(B . ?$,16K(B)
	 (?(5w(B . ?$,16L(B)
	 (?(5x(B . ?$,16M(B)
	 (?(5y(B . ?$,16N(B)
	 (?(5z(B . ?$,16O(B)))

      (katakana-jisx0201
       '((?(I!(B . ?$,3sa(B)
	 (?\(I"(B . ?\$,3sb(B)
	 (?\(I#(B . ?\$,3sc(B)
	 (?(I$(B . ?$,3sd(B)
	 (?(I%(B . ?$,3se(B)
	 (?(I&(B . ?$,3sf(B)
	 (?(I'(B . ?$,3sg(B)
	 (?(I((B . ?$,3sh(B)
	 (?(I)(B . ?$,3si(B)
	 (?(I*(B . ?$,3sj(B)
	 (?(I+(B . ?$,3sk(B)
	 (?(I,(B . ?$,3sl(B)
	 (?(I-(B . ?$,3sm(B)
	 (?(I.(B . ?$,3sn(B)
	 (?(I/(B . ?$,3so(B)
	 (?(I0(B . ?$,3sp(B)
	 (?(I1(B . ?$,3sq(B)
	 (?(I2(B . ?$,3sr(B)
	 (?(I3(B . ?$,3ss(B)
	 (?(I4(B . ?$,3st(B)
	 (?(I5(B . ?$,3su(B)
	 (?(I6(B . ?$,3sv(B)
	 (?(I7(B . ?$,3sw(B)
	 (?(I8(B . ?$,3sx(B)
	 (?(I9(B . ?$,3sy(B)
	 (?(I:(B . ?$,3sz(B)
	 (?(I;(B . ?$,3s{(B)
	 (?(I<(B . ?$,3s|(B)
	 (?(I=(B . ?$,3s}(B)
	 (?(I>(B . ?$,3s~(B)
	 (?(I?(B . ?$,3s(B)
	 (?(I@(B . ?$,3t (B)
	 (?(IA(B . ?$,3t!(B)
	 (?(IB(B . ?$,3t"(B)
	 (?(IC(B . ?$,3t#(B)
	 (?(ID(B . ?$,3t$(B)
	 (?(IE(B . ?$,3t%(B)
	 (?(IF(B . ?$,3t&(B)
	 (?(IG(B . ?$,3t'(B)
	 (?(IH(B . ?$,3t((B)
	 (?(II(B . ?$,3t)(B)
	 (?(IJ(B . ?$,3t*(B)
	 (?(IK(B . ?$,3t+(B)
	 (?(IL(B . ?$,3t,(B)
	 (?(IM(B . ?$,3t-(B)
	 (?(IN(B . ?$,3t.(B)
	 (?(IO(B . ?$,3t/(B)
	 (?(IP(B . ?$,3t0(B)
	 (?(IQ(B . ?$,3t1(B)
	 (?(IR(B . ?$,3t2(B)
	 (?(IS(B . ?$,3t3(B)
	 (?(IT(B . ?$,3t4(B)
	 (?(IU(B . ?$,3t5(B)
	 (?(IV(B . ?$,3t6(B)
	 (?(IW(B . ?$,3t7(B)
	 (?(IX(B . ?$,3t8(B)
	 (?(IY(B . ?$,3t9(B)
	 (?(IZ(B . ?$,3t:(B)
	 (?(I[(B . ?$,3t;(B)
	 (?(I\(B . ?$,3t<(B)
	 (?(I](B . ?$,3t=(B)
	 (?(I^(B . ?$,3t>(B)
	 (?(I_(B . ?$,3t?(B)))

      (chinese-sisheng
       '((?(0!(B . ?$,1 !(B)
	 (?(0"(B . ?,Aa(B)
	 (?(0#(B . ?$,1".(B)
	 (?(0$(B . ?,A`(B)
	 (?(0%(B . ?$,1 3(B)
	 (?(0&(B . ?,Ai(B)
	 (?(0'(B . ?$,1 ;(B)
	 (?(0((B . ?,Ah(B)
	 (?(0)(B . ?$,1 K(B)
	 (?(0*(B . ?,Am(B)
	 (?(0+(B . ?$,1"0(B)
	 (?(0,(B . ?,Al(B)
	 (?(0-(B . ?$,1 m(B)
	 (?(0.(B . ?,As(B)
	 (?(0/(B . ?$,1"2(B)
	 (?(00(B . ?,Ar(B)
	 (?(01(B . ?$,1!+(B)
	 (?(02(B . ?,Az(B)
	 (?(03(B . ?$,1"4(B)
	 (?(04(B . ?,Ay(B)
	 (?(05(B . ?$,1"6(B)
	 (?(06(B . ?$,1"8(B)
	 (?(07(B . ?$,1":(B)
	 (?(08(B . ?$,1"<(B)
	 (?(09(B . ?,A|(B)
	 (?(0:(B . ?,Aj(B)
	 (?(0<(B . ?$,1m(B)
	 (?(0=(B . ?$,1 d(B)
	 (?(0>(B . ?$,1 h(B)
	 (?(0?(B . ?$,1"Y(B)
	 (?(0A(B . ?$,1$i(B)
	 (?(0B(B . ?$,1$j(B)
	 (?(0C(B . ?$,1$g(B)
	 (?(0D(B . ?$,1$k(B)
	 (?(0E(B . ?$,2@%(B)
	 (?(0F(B . ?$,2@&(B)
	 (?(0G(B . ?$,2@'(B)
	 (?(0H(B . ?$,2@((B)
	 (?(0I(B . ?$,2@)(B)
	 (?(0J(B . ?$,2@*(B)
	 (?(0K(B . ?$,2@+(B)
	 (?(0L(B . ?$,2@,(B)
	 (?(0M(B . ?$,2@-(B)
	 (?(0N(B . ?$,2@.(B)
	 (?(0O(B . ?$,2@/(B)
	 (?(0P(B . ?$,2@0(B)
	 (?(0Q(B . ?$,2@1(B)
	 (?(0R(B . ?$,2@2(B)
	 (?(0S(B . ?$,2@3(B)
	 (?(0T(B . ?$,2@4(B)
	 (?(0U(B . ?$,2@5(B)
	 (?(0V(B . ?$,2@6(B)
	 (?(0W(B . ?$,2@7(B)
	 (?(0X(B . ?$,2@8(B)
	 (?(0Y(B . ?$,2@9(B)
	 (?(0Z(B . ?$,2@:(B)
	 (?(0[(B . ?$,2@;(B)
	 (?(0\(B . ?$,2@<(B)
	 (?(0](B . ?$,2@=(B)
	 (?(0^(B . ?$,2@>(B)
	 (?(0_(B . ?$,2@?(B)
	 (?(0`(B . ?$,2@@(B)
	 (?(0a(B . ?$,2@A(B)
	 (?(0b(B . ?$,2@B(B)
	 (?(0c(B . ?$,2@C(B)
	 (?(0d(B . ?$,2@D(B)
	 (?(0e(B . ?$,2@E(B)
	 (?(0f(B . ?$,2@F(B)
	 (?(0g(B . ?$,2@G(B)
	 (?(0h(B . ?$,2@H(B)
	 (?(0i(B . ?$,2@I(B)))

      (lao
       '((?(1!(B . ?$,1D!(B)
	 (?(1"(B . ?$,1D"(B)
	 (?(1$(B . ?$,1D$(B)
	 (?(1'(B . ?$,1D'(B)
	 (?(1((B . ?$,1D((B)
	 (?(1*(B . ?$,1D*(B)
	 (?(1-(B . ?$,1D-(B)
	 (?(14(B . ?$,1D4(B)
	 (?(15(B . ?$,1D5(B)
	 (?(16(B . ?$,1D6(B)
	 (?(17(B . ?$,1D7(B)
	 (?(19(B . ?$,1D9(B)
	 (?(1:(B . ?$,1D:(B)
	 (?(1;(B . ?$,1D;(B)
	 (?(1<(B . ?$,1D<(B)
	 (?(1=(B . ?$,1D=(B)
	 (?(1>(B . ?$,1D>(B)
	 (?(1?(B . ?$,1D?(B)
	 (?(1A(B . ?$,1DA(B)
	 (?(1B(B . ?$,1DB(B)
	 (?(1C(B . ?$,1DC(B)
	 (?(1E(B . ?$,1DE(B)
	 (?(1G(B . ?$,1DG(B)
	 (?(1J(B . ?$,1DJ(B)
	 (?(1K(B . ?$,1DK(B)
	 (?(1M(B . ?$,1DM(B)
	 (?(1N(B . ?$,1DN(B)
	 (?(1O(B . ?$,1DO(B)
	 (?(1P(B . ?$,1DP(B)
	 (?(1Q(B . ?$,1DQ(B)
	 (?(1R(B . ?$,1DR(B)
	 (?(1S(B . ?$,1DS(B)
	 (?(1T(B . ?$,1DT(B)
	 (?(1U(B . ?$,1DU(B)
	 (?(1V(B . ?$,1DV(B)
	 (?(1W(B . ?$,1DW(B)
	 (?(1X(B . ?$,1DX(B)
	 (?(1Y(B . ?$,1DY(B)
	 (?(1[(B . ?$,1D[(B)
	 (?(1\(B . ?$,1D\(B)
	 (?(1](B . ?$,1D](B)
	 (?(1`(B . ?$,1D`(B)
	 (?(1a(B . ?$,1Da(B)
	 (?(1b(B . ?$,1Db(B)
	 (?(1c(B . ?$,1Dc(B)
	 (?(1d(B . ?$,1Dd(B)
	 (?(1f(B . ?$,1Df(B)
	 (?(1h(B . ?$,1Dh(B)
	 (?(1i(B . ?$,1Di(B)
	 (?(1j(B . ?$,1Dj(B)
	 (?(1k(B . ?$,1Dk(B)
	 (?(1l(B . ?$,1Dl(B)
	 (?(1m(B . ?$,1Dm(B)
	 (?(1p(B . ?$,1Dp(B)
	 (?(1q(B . ?$,1Dq(B)
	 (?(1r(B . ?$,1Dr(B)
	 (?(1s(B . ?$,1Ds(B)
	 (?(1t(B . ?$,1Dt(B)
	 (?(1u(B . ?$,1Du(B)
	 (?(1v(B . ?$,1Dv(B)
	 (?(1w(B . ?$,1Dw(B)
	 (?(1x(B . ?$,1Dx(B)
	 (?(1y(B . ?$,1Dy(B)
	 (?(1|(B . ?$,1D|(B)
	 (?(1}(B . ?$,1D}(B))))
  (let ((table (make-char-table 'safe-chars))
	safe-charsets)
    (dolist (cs '(vietnamese-viscii lao chinese-sisheng ipa
		  katakana-jisx0201 thai-tis620 tibetan-iso-8bit
		  indian-is13194 ethiopic))
      ;; These tables could be used as translation-table-for-encode by
      ;; the relevant coding systems.
      (let ((encode-translator
	     (if (coding-system-p cs)
		 (set (intern (format "ucs-%s-encode-table" cs))
		      (make-translation-table)))))
	(dolist (pair (symbol-value cs))
	  (aset ucs-mule-to-mule-unicode (car pair) (cdr pair))
	  (if encode-translator
	      (aset encode-translator (cdr pair) (car pair))))
	(if (charsetp cs)
	    (push cs safe-charsets)
	  (setq safe-charsets
		(append (delq 'ascii (coding-system-get cs 'safe-charsets))
			safe-charsets)))))
    (dolist (c safe-charsets)
      (aset table (make-char c) t))
    (coding-system-put 'mule-utf-8 'safe-charsets
		       (append (coding-system-get 'mule-utf-8 'safe-charsets)
			       safe-charsets))
    (register-char-codings 'mule-utf-8 table)))

(provide 'ucs-tables)

;;; ucs-tables.el ends here
