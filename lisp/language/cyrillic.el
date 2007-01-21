;;; cyrillic.el --- support for Cyrillic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Author: Kenichi Handa <handa@etl.go.jp>
;; Keywords: multilingual, Cyrillic, i18n

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

;; The character set ISO8859-5 is supported.  KOI-8 and ALTERNATIVNYJ
;; are converted to Unicode internally.  See
;; <URL:http://www.ecma.ch/ecma1/STAND/ECMA-113.HTM>.  For more info
;; on Cyrillic charsets, see
;; <URL:http://czyborra.com/charsets/cyrillic.html>.  The KOI and
;; Alternativnyj coding systems should live in code-pages.el, but
;; they've always been preloaded and the coding system autoload
;; mechanism didn't get accepted, so they have to stay here and
;; duplicate code-pages stuff.

;; Note that 8859-5 maps directly onto the Unicode Cyrillic block,
;; apart from codepoints 160 (NBSP, c.f. U+0400), 173 (soft hyphen,
;; c.f. U+04OD) and 253 (section sign, c.f U+045D).  The KOI-8 and
;; Alternativnyj coding systems encode both 8859-5 and Unicode.
;; ucs-tables.el provides unification for cyrillic-iso-8bit.

;; Customizing `utf-fragment-on-decoding' allows decoding characters
;; from KOI and Alternativnyj into 8859-5 where that's possible.
;; cyrillic-iso8859-5 characters take half as much space in the buffer
;; as the mule-unicode-0100-24ff equivalents, though that's probably
;; not normally a big deal.

;;; Code:

;; Cyrillic (general)

;; ISO-8859-5 stuff

(make-coding-system
 'cyrillic-iso-8bit 2 ?5
 "ISO 2022 based 8-bit encoding for Cyrillic script (MIME:ISO-8859-5)."
 '(ascii cyrillic-iso8859-5  nil nil
   nil nil nil nil nil nil nil nil nil nil nil t)
 '((safe-charsets ascii cyrillic-iso8859-5)
   (mime-charset . iso-8859-5)))

(define-coding-system-alias 'iso-8859-5 'cyrillic-iso-8bit)

(set-language-info-alist
 "Cyrillic-ISO" '((charset cyrillic-iso8859-5)
		  (coding-system cyrillic-iso-8bit)
		  (coding-priority cyrillic-iso-8bit)
		  (input-method . "cyrillic-yawerty") ; fixme
		  (nonascii-translation . cyrillic-iso8859-5)
		  (unibyte-display . cyrillic-iso-8bit)
		  (features cyril-util)
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ISO-8859-5."))
 '("Cyrillic"))

;; KOI-8R stuff

;; The mule-unicode portion of this is from
;; http://www.unicode.org/Public/MAPPINGS/VENDORS/MISC/KOI8-R.TXT,
;; which references RFC 1489.
(defvar cyrillic-koi8-r-decode-table
  [
   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   ;; 8859-5 plus Unicode
   ?$,2  (B ?$,2 "(B ?$,2 ,(B ?$,2 0(B ?$,2 4(B ?$,2 8(B ?$,2 <(B ?$,2 D(B ?$,2 L(B ?$,2 T(B ?$,2 \(B ?$,2!@(B ?$,2!D(B ?$,2!H(B ?$,2!L(B ?$,2!P(B
   ?$,2!Q(B ?$,2!R(B ?$,2!S(B ?$,1{ (B ?$,2!`(B ?$,1s"(B ?$,1x:(B ?$,1xh(B ?$,1y$(B ?$,1y%(B ?,L (B ?$,1{!(B ?,A0(B ?,A2(B ?,A7(B ?,Aw(B
   ?$,2 p(B ?$,2 q(B ?$,2 r(B ?,Lq(B ?$,2 s(B ?$,2 t(B ?$,2 u(B ?$,2 v(B ?$,2 w(B ?$,2 x(B ?$,2 y(B ?$,2 z(B ?$,2 {(B ?$,2 |(B ?$,2 }(B ?$,2 ~(B
   ?$,2 (B ?$,2! (B ?$,2!!(B ?,L!(B ?$,2!"(B ?$,2!#(B ?$,2!$(B ?$,2!%(B ?$,2!&(B ?$,2!'(B ?$,2!((B ?$,2!)(B ?$,2!*(B ?$,2!+(B ?$,2!,(B ?,A)(B
   ?,Ln(B  ?,LP(B  ?,LQ(B  ?,Lf(B  ?,LT(B  ?,LU(B  ?,Ld(B  ?,LS(B  ?,Le(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B
   ?,L_(B  ?,Lo(B  ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,LV(B  ?,LR(B  ?,Ll(B  ?,Lk(B  ?,LW(B  ?,Lh(B  ?,Lm(B  ?,Li(B  ?,Lg(B  ?,Lj(B
   ?,LN(B  ?,L0(B  ?,L1(B  ?,LF(B  ?,L4(B  ?,L5(B  ?,LD(B  ?,L3(B  ?,LE(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B
   ?,L?(B  ?,LO(B  ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,L6(B  ?,L2(B  ?,LL(B  ?,LK(B  ?,L7(B  ?,LH(B  ?,LM(B  ?,LI(B  ?,LG(B  ?,LJ(B
   ;; All Unicode:
;;    ?$,2  (B ?$,2 "(B ?$,2 ,(B ?$,2 0(B ?$,2 4(B ?$,2 8(B ?$,2 <(B ?$,2 D(B ?$,2 L(B ?$,2 T(B ?$,2 \(B ?$,2!@(B ?$,2!D(B ?$,2!H(B ?$,2!L(B ?$,2!P(B
;;    ?$,2!Q(B ?$,2!R(B ?$,2!S(B ?$,1{ (B ?$,2!`(B ?$,1s"(B ?$,1x:(B ?$,1xh(B ?$,1y$(B ?$,1y%(B ?,A (B ?$,1{!(B ?,A0(B ?,A2(B ?,A7(B ?,Aw(B
;;    ?$,2 p(B ?$,2 q(B ?$,2 r(B ?$,1(q(B ?$,2 s(B ?$,2 t(B ?$,2 u(B ?$,2 v(B ?$,2 w(B ?$,2 x(B ?$,2 y(B ?$,2 z(B ?$,2 {(B ?$,2 |(B ?$,2 }(B ?$,2 ~(B
;;    ?$,2 (B ?$,2! (B ?$,2!!(B ?$,1(!(B ?$,2!"(B ?$,2!#(B ?$,2!$(B ?$,2!%(B ?$,2!&(B ?$,2!'(B ?$,2!((B ?$,2!)(B ?$,2!*(B ?$,2!+(B ?$,2!,(B ?,A)(B
;;    ?$,1(n(B ?$,1(P(B ?$,1(Q(B ?$,1(f(B ?$,1(T(B ?$,1(U(B ?$,1(d(B ?$,1(S(B ?$,1(e(B ?$,1(X(B ?$,1(Y(B ?$,1(Z(B ?$,1([(B ?$,1(\(B ?$,1(](B ?$,1(^(B
;;    ?$,1(_(B ?$,1(o(B ?$,1(`(B ?$,1(a(B ?$,1(b(B ?$,1(c(B ?$,1(V(B ?$,1(R(B ?$,1(l(B ?$,1(k(B ?$,1(W(B ?$,1(h(B ?$,1(m(B ?$,1(i(B ?$,1(g(B ?$,1(j(B
;;    ?$,1(N(B ?$,1(0(B ?$,1(1(B ?$,1(F(B ?$,1(4(B ?$,1(5(B ?$,1(D(B ?$,1(3(B ?$,1(E(B ?$,1(8(B ?$,1(9(B ?$,1(:(B ?$,1(;(B ?$,1(<(B ?$,1(=(B ?$,1(>(B
;;    ?$,1(?(B ?$,1(O(B ?$,1(@(B ?$,1(A(B ?$,1(B(B ?$,1(C(B ?$,1(6(B ?$,1(2(B ?$,1(L(B ?$,1(K(B ?$,1(7(B ?$,1(H(B ?$,1(M(B ?$,1(I(B ?$,1(G(B ?$,1(J(B
   ]
  "Cyrillic KOI8-R decoding table.")

(let ((table (make-translation-table-from-vector
	      cyrillic-koi8-r-decode-table)))
  (define-translation-table 'cyrillic-koi8-r-nonascii-translation-table table)
  (define-translation-table 'cyrillic-koi8-r-encode-table
    (char-table-extra-slot table 0)))

;; No point in keeping it around.  (It can't be let-bound, since it's
;; needed for macro expansion.)
(makunbound 'cyrillic-koi8-r-decode-table)

(define-ccl-program ccl-decode-koi8
  `(4
    ((loop
      (r0 = 0)
      (read r1)
      (if (r1 < 128)
	  (write-repeat r1)
	((translate-character cyrillic-koi8-r-nonascii-translation-table r0 r1)
	 (translate-character ucs-translation-table-for-decode r0 r1)
	 (write-multibyte-character r0 r1)
	 (repeat))))))
  "CCL program to decode KOI8-R.")

(define-ccl-program ccl-encode-koi8
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (translate-character cyrillic-koi8-r-encode-table r0 r1)
      (if (r0 != ,(charset-id 'ascii))
	  (if (r0 != ,(charset-id 'eight-bit-graphic))
	      (if (r0 != ,(charset-id 'eight-bit-control))
		  (r1 = ??))))
      (write-repeat r1))))
  "CCL program to encode KOI8-R.")

(defun cyrillic-unify-encoding (table)
  "Set up equivalent characters in the encoding TABLE.
This works whether or not the table is Unicode-based or
8859-5-based.  (Only appropriate for Cyrillic.)"
  (let ((table (get table 'translation-table)))
    (dotimes (i 96)
      (let* ((c (make-char 'cyrillic-iso8859-5 (+ i 32)))
	     (u				; equivalent Unicode char
	      (cond ((eq c ?,L (B) ?,A (B)
		    ((eq c ?,L-(B) ?,A-(B)
		    ((eq c ?,L}(B) ?,A'(B)
		    (t (decode-char 'ucs (+ #x400 i)))))
	     (ec (aref table c))	; encoding of 8859-5
	     (uc (aref table u)))	; encoding of Unicode
	(unless (memq c '(?,L (B ?,L-(B ?,L}(B))	; 8859-5 exceptions
	  (unless uc
	    (aset table u ec))
	  (unless ec
	    (aset table c uc)))))))

(cyrillic-unify-encoding 'cyrillic-koi8-r-encode-table)

(make-coding-system
 'cyrillic-koi8 4
 ;; We used to use ?K.  It is true that ?K is more strictly correct,
 ;; but it is also used for Korean.
 ;; So people who use koi8 for languages other than Russian
 ;; will have to forgive us.
 ?R "KOI8-R 8-bit encoding for Cyrillic (MIME: KOI8-R)."
 '(ccl-decode-koi8 . ccl-encode-koi8)
 `((safe-chars . cyrillic-koi8-r-encode-table)
   (mime-charset . koi8-r)
   (valid-codes (0 . 255))
   (dependency unify-8859-on-encoding-mode unify-8859-on-decoding-mode)))

(define-coding-system-alias 'koi8-r 'cyrillic-koi8)
(define-coding-system-alias 'koi8 'cyrillic-koi8)
(define-coding-system-alias 'cp878 'cyrillic-koi8)

(let ((elt `("koi8-r" koi8-r 1
	     ,(get 'cyrillic-koi8-r-encode-table 'translation-table)))
      (slot (assoc "koi8-r" ctext-non-standard-encodings-alist)))
  (if slot
      (setcdr slot (cdr elt))
    (push elt ctext-non-standard-encodings-alist)))

;; Allow displaying some of KOI & al with an 8859-5-encoded font.  We
;; won't bother about the exceptions when encoding the font, since
;; NBSP will fall through below and work anyhow, and we'll have
;; avoided setting the fontset for the other two to 8859-5 -- they're
;; not in KOI and Alternativnyj anyhow.
(define-ccl-program ccl-encode-8859-5-font
  `(0
    ((if (r0 == ,(charset-id 'cyrillic-iso8859-5))
	 (r1 += 128)
       (if (r0 == ,(charset-id 'mule-unicode-0100-24ff))
	   (r1 = (r2 + 128))))))
  "Encode ISO 8859-5 and Cyrillic Unicode chars to 8859-5 font.")

(add-to-list 'font-ccl-encoder-alist '("iso8859-5" . ccl-encode-8859-5-font))

;; The table is set up later to encode both Unicode and 8859-5.
(define-ccl-program ccl-encode-koi8-font
  `(0
    (if (r2 >= 0)
	((r1 <<= 7)
	 (r1 += r2)))
    (translate-character cyrillic-koi8-r-encode-table r0 r1))
  "CCL program to encode Cyrillic chars to KOI font.")

(add-to-list 'font-ccl-encoder-alist '("koi8" . ccl-encode-koi8-font))

(set-language-info-alist
 "Cyrillic-KOI8" `((charset cyrillic-iso8859-5)
		   (nonascii-translation
		    . ,(get 'cyrillic-koi8-r-nonascii-translation-table
			    'translation-table))
		   (coding-system cyrillic-koi8)
		   (coding-priority cyrillic-koi8 cyrillic-iso-8bit)
		   (ctext-non-standard-encodings "koi8-r")
		   (input-method . "russian-typewriter")
		   (features cyril-util)
		   (unibyte-display . cyrillic-koi8)
		   (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		   (documentation . "Support for Cyrillic KOI8-R."))
 '("Cyrillic"))

(set-language-info-alist
 "Russian" `((charset cyrillic-iso8859-5)
	     (nonascii-translation
	      . ,(get 'cyrillic-koi8-r-nonascii-translation-table
		      'translation-table))
	     (coding-system cyrillic-koi8)
	     (coding-priority cyrillic-koi8 cyrillic-iso-8bit)
	     (input-method . "russian-computer")
	     (features cyril-util)
	     (unibyte-display . cyrillic-koi8)
	     (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
	     (documentation . "\
Support for Russian using koi8-r and the russian-computer input method.")
	     (tutorial . "TUTORIAL.ru"))
 '("Cyrillic"))


(defvar cyrillic-koi8-u-decode-table
  [
   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   ;; All Unicode:
;;    ?$,2  (B ?$,2 "(B ?$,2 ,(B ?$,2 0(B ?$,2 4(B ?$,2 8(B ?$,2 <(B ?$,2 D(B ?$,2 L(B ?$,2 T(B ?$,2 \(B ?$,2!@(B ?$,2!D(B ?$,2!H(B ?$,2!L(B ?$,2!P(B
;;    ?$,2!Q(B ?$,2!R(B ?$,2!S(B ?$,1{ (B ?$,2!`(B ?$,1x9(B ?$,1x:(B ?$,1xh(B ?$,1y$(B ?$,1y%(B ?,L (B ?$,1{!(B ?,A0(B ?,A2(B ?,A7(B ?,Aw(B
;;    ?$,2 p(B ?$,2 q(B ?$,2 r(B ?$,1(q(B ?$,1(t(B ?$,2 t(B ?$,1(v(B ?$,1(w(B ?$,2 w(B ?$,2 x(B ?$,2 y(B ?$,2 z(B ?$,2 {(B ?$,1)Q(B ?$,2 }(B ?$,2 ~(B
;;    ?$,2 (B ?$,2! (B ?$,2!!(B ?$,1(!(B ?$,1($(B ?$,2!#(B ?$,1(&(B ?$,1('(B ?$,2!&(B ?$,2!'(B ?$,2!((B ?$,2!)(B ?$,2!*(B ?$,1)P(B ?$,2!,(B ?,A)(B
;;    ?$,1(n(B ?$,1(P(B ?$,1(Q(B ?$,1(f(B ?$,1(T(B ?$,1(U(B ?$,1(d(B ?$,1(S(B ?$,1(e(B ?$,1(X(B ?$,1(Y(B ?$,1(Z(B ?$,1([(B ?$,1(\(B ?$,1(](B ?$,1(^(B
;;    ?$,1(_(B ?$,1(o(B ?$,1(`(B ?$,1(a(B ?$,1(b(B ?$,1(c(B ?$,1(V(B ?$,1(R(B ?$,1(l(B ?$,1(k(B ?$,1(W(B ?$,1(h(B ?$,1(m(B ?$,1(i(B ?$,1(g(B ?$,1(j(B
;;    ?$,1(N(B ?$,1(0(B ?$,1(1(B ?$,1(F(B ?$,1(4(B ?$,1(5(B ?$,1(D(B ?$,1(3(B ?$,1(E(B ?$,1(8(B ?$,1(9(B ?$,1(:(B ?$,1(;(B ?$,1(<(B ?$,1(=(B ?$,1(>(B
;;    ?$,1(?(B ?$,1(O(B ?$,1(@(B ?$,1(A(B ?$,1(B(B ?$,1(C(B ?$,1(6(B ?$,1(2(B ?$,1(L(B ?$,1(K(B ?$,1(7(B ?$,1(H(B ?$,1(M(B ?$,1(I(B ?$,1(G(B ?$,1(J(B
;; 8859-5 plus Unicode:
   ?$,2  (B ?$,2 "(B ?$,2 ,(B ?$,2 0(B ?$,2 4(B ?$,2 8(B ?$,2 <(B ?$,2 D(B ?$,2 L(B ?$,2 T(B ?$,2 \(B ?$,2!@(B ?$,2!D(B ?$,2!H(B ?$,2!L(B ?$,2!P(B
   ?$,2!Q(B ?$,2!R(B ?$,2!S(B ?$,1{ (B ?$,2!`(B ?$,1x9(B ?$,1x:(B ?$,1xh(B ?$,1y$(B ?$,1y%(B ?,L (B ?$,1{!(B ?,A0(B ?,A2(B ?,A7(B ?,Aw(B
   ?$,2 p(B ?$,2 q(B ?$,2 r(B ?,Lq(B ?,Lt(B ?$,2 t(B ?,Lv(B ?,Lw(B ?$,2 w(B ?$,2 x(B ?$,2 y(B ?$,2 z(B ?$,2 {(B ?$,1)Q(B ?$,2 }(B ?$,2 ~(B
   ?$,2 (B ?$,2! (B ?$,2!!(B ?,L!(B ?,L$(B ?$,2!#(B ?,L&(B ?,L'(B ?$,2!&(B ?$,2!'(B ?$,2!((B ?$,2!)(B ?$,2!*(B ?$,1)P(B ?$,2!,(B ?,A)(B
   ?,Ln(B ?,LP(B ?,LQ(B ?,Lf(B ?,LT(B ?,LU(B ?,Ld(B ?,LS(B ?,Le(B ?,LX(B ?,LY(B ?,LZ(B ?,L[(B ?,L\(B ?,L](B ?,L^(B
   ?,L_(B ?,Lo(B ?,L`(B ?,La(B ?,Lb(B ?,Lc(B ?,LV(B ?,LR(B ?,Ll(B ?,Lk(B ?,LW(B ?,Lh(B ?,Lm(B ?,Li(B ?,Lg(B ?,Lj(B
   ?,LN(B ?,L0(B ?,L1(B ?,LF(B ?,L4(B ?,L5(B ?,LD(B ?,L3(B ?,LE(B ?,L8(B ?,L9(B ?,L:(B ?,L;(B ?,L<(B ?,L=(B ?,L>(B
   ?,L?(B ?,LO(B ?,L@(B ?,LA(B ?,LB(B ?,LC(B ?,L6(B ?,L2(B ?,LL(B ?,LK(B ?,L7(B ?,LH(B ?,LM(B ?,LI(B ?,LG(B ?,LJ(B
   ]
  "Cyrillic KOI8-U decoding table.")

(let ((table (make-translation-table-from-vector
	      cyrillic-koi8-u-decode-table)))
  (define-translation-table 'cyrillic-koi8-u-nonascii-translation-table table)
  (define-translation-table 'cyrillic-koi8-u-encode-table
    (char-table-extra-slot table 0)))

(makunbound 'cyrillic-koi8-u-decode-table)

(define-ccl-program ccl-decode-koi8-u
  `(4
    ((loop
      (r0 = 0)
      (read r1)
      (if (r1 < 128)
	  (write-repeat r1)
	((translate-character cyrillic-koi8-u-nonascii-translation-table r0 r1)
	 (translate-character ucs-translation-table-for-decode r0 r1)
	 (write-multibyte-character r0 r1)
	 (repeat))))))
  "CCL program to decode KOI8-U.")

(define-ccl-program ccl-encode-koi8-u
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (translate-character cyrillic-koi8-u-encode-table r0 r1)
      (if (r0 != ,(charset-id 'ascii))
	  (if (r0 != ,(charset-id 'eight-bit-graphic))
	      (if (r0 != ,(charset-id 'eight-bit-control))
		  (r1 = ??))))
      (write-repeat r1))))
  "CCL program to encode KOI8-U.")

(cyrillic-unify-encoding 'cyrillic-koi8-u-encode-table)

(make-coding-system
 'koi8-u 4
 ?U "KOI8-U 8-bit encoding for Cyrillic (MIME: KOI8-U)"
 '(ccl-decode-koi8-u . ccl-encode-koi8-u)
 `((safe-chars . cyrillic-koi8-u-encode-table)
   (mime-charset . koi8-u)
   (valid-codes (0 . 255))
   (dependency unify-8859-on-encoding-mode unify-8859-on-decoding-mode)))

(define-ccl-program ccl-encode-koi8-u-font
  `(0
    (translate-character cyrillic-koi8-u-encode-table r0 r1))
  "CCL program to encode Cyrillic chars to KOI-U font.")

(add-to-list 'font-ccl-encoder-alist '("koi8-u" . ccl-encode-koi8-u-font))

(set-language-info-alist
 "Ukrainian" `((coding-system koi8-u)
	       (coding-priority koi8-u)
	       (nonascii-translation
		. ,(get 'cyrillic-koi8-u-nonascii-translation-table
			'translation-table))
	       (input-method . "ukrainian-computer")
	       (documentation
		. "Support for Ukrainian with KOI8-U character set."))
 '("Cyrillic"))

;;; ALTERNATIVNYJ stuff

;; Fixme: It's unclear what's the correct table.  I've found
;; statements both that it's the same as cp866 and somewhat different,
;; but nothing that looks really definitive.
(defvar cyrillic-alternativnyj-decode-table
  [
   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
   16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
   32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
   48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
   64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
   80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
   96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
;;    ?$,1(0(B  ?$,1(1(B  ?$,1(2(B  ?$,1(3(B  ?$,1(4(B  ?$,1(5(B  ?$,1(6(B  ?$,1(7(B  ?$,1(8(B  ?$,1(9(B  ?$,1(:(B  ?$,1(;(B  ?$,1(<(B  ?$,1(=(B  ?$,1(>(B  ?$,1(?(B
;;    ?$,1(@(B  ?$,1(A(B  ?$,1(B(B  ?$,1(C(B  ?$,1(D(B  ?$,1(E(B  ?$,1(F(B  ?$,1(G(B  ?$,1(H(B  ?$,1(I(B  ?$,1(J(B  ?$,1(K(B  ?$,1(L(B  ?$,1(M(B  ?$,1(N(B  ?$,1(O(B
;;    ?$,1(P(B  ?$,1(Q(B  ?$,1(R(B  ?$,1(S(B  ?$,1(T(B  ?$,1(U(B  ?$,1(V(B  ?$,1(W(B  ?$,1(X(B  ?$,1(Y(B  ?$,1(Z(B  ?$,1([(B  ?$,1(\(B  ?$,1(](B  ?$,1(^(B  ?$,1(_(B
;;    ?$,2!Q(B  ?$,2!R(B  ?$,2!S(B  ?$,2 "(B  ?$,2 D(B  ?$,2!!(B  ?$,2!"(B  ?$,2 v(B  ?$,2 u(B  ?$,2!#(B  ?$,2 q(B  ?$,2 w(B  ?$,2 }(B  ?$,2 |(B  ?$,2 {(B  ?$,2 0(B
;;    ?$,2 4(B  ?$,2 T(B  ?$,2 L(B  ?$,2 <(B  ?$,2  (B  ?$,2 \(B  ?$,2 ~(B  ?$,2 (B  ?$,2 z(B  ?$,2 t(B  ?$,2!)(B  ?$,2!&(B  ?$,2! (B  ?$,2 p(B  ?$,2!,(B  ?$,2!'(B
;;    ?$,2!((B  ?$,2!$(B  ?$,2!%(B  ?$,2 y(B  ?$,2 x(B  ?$,2 r(B  ?$,2 s(B  ?$,2!+(B  ?$,2!*(B  ?$,2 8(B  ?$,2 ,(B  ?$,2!H(B  ?$,2!D(B  ?$,2!L(B  ?$,2!P(B  ?$,2!@(B
;;    ?$,1(`(B  ?$,1(a(B  ?$,1(b(B  ?$,1(c(B  ?$,1(d(B  ?$,1(e(B  ?$,1(f(B  ?$,1(g(B  ?$,1(h(B  ?$,1(i(B  ?$,1(j(B  ?$,1(k(B  ?$,1(l(B  ?$,1(m(B  ?$,1(n(B  ?$,1(o(B
;;    ?$,1(!(B  ?$,1(q(B  ?$,1ry(B  ?$,1rx(B  ?$,1%A(B  ?$,1%@(B  ?$,1s:(B  ?$,1s9(B  ?$,1vq(B  ?$,1vs(B  ?,A1(B  ?,Aw(B  ?$,1uV(B  ?,A$(B  ?$,2!`(B  ?,A (B ;
;; 8859+Unicode
   ?,L0(B  ?,L1(B  ?,L2(B  ?,L3(B  ?,L4(B  ?,L5(B  ?,L6(B  ?,L7(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B  ?,L?(B
   ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,LD(B  ?,LE(B  ?,LF(B  ?,LG(B  ?,LH(B  ?,LI(B  ?,LJ(B  ?,LK(B  ?,LL(B  ?,LM(B  ?,LN(B  ?,LO(B
   ?,LP(B  ?,LQ(B  ?,LR(B  ?,LS(B  ?,LT(B  ?,LU(B  ?,LV(B  ?,LW(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B  ?,L_(B
   ?$,2!Q(B  ?$,2!R(B  ?$,2!S(B  ?$,2 "(B  ?$,2 D(B  ?$,2!!(B  ?$,2!"(B  ?$,2 v(B  ?$,2 u(B  ?$,2!#(B  ?$,2 q(B  ?$,2 w(B  ?$,2 }(B  ?$,2 |(B  ?$,2 {(B  ?$,2 0(B
   ?$,2 4(B  ?$,2 T(B  ?$,2 L(B  ?$,2 <(B  ?$,2  (B  ?$,2 \(B  ?$,2 ~(B  ?$,2 (B  ?$,2 z(B  ?$,2 t(B  ?$,2!)(B  ?$,2!&(B  ?$,2! (B  ?$,2 p(B  ?$,2!,(B  ?$,2!'(B
   ?$,2!((B  ?$,2!$(B  ?$,2!%(B  ?$,2 y(B  ?$,2 x(B  ?$,2 r(B  ?$,2 s(B  ?$,2!+(B  ?$,2!*(B  ?$,2 8(B  ?$,2 ,(B  ?$,2!H(B  ?$,2!D(B  ?$,2!L(B  ?$,2!P(B  ?$,2!@(B
   ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,Ld(B  ?,Le(B  ?,Lf(B  ?,Lg(B  ?,Lh(B  ?,Li(B  ?,Lj(B  ?,Lk(B  ?,Ll(B  ?,Lm(B  ?,Ln(B  ?,Lo(B
   ;; Taken from http://www.cyrillic.com/ref/cyrillic/koi-8alt.html
   ;; with guesses for the Unicodes of the glyphs in the absence of a
   ;; table.
   ?,L!(B  ?,Lq(B  ?$,1ry(B  ?$,1rx(B  ?$,1%A(B  ?$,1%@(B  ?$,1s:(B  ?$,1s9(B  ?$,1vq(B  ?$,1vs(B  ?,A1(B  ?,Aw(B  ?,Lp(B  ?,A$(B  ?$,2!`(B  ?,L (B]
  "Cyrillic ALTERNATIVNYJ decoding table.")

(let ((table (make-translation-table-from-vector
	      cyrillic-alternativnyj-decode-table)))
  (define-translation-table 'cyrillic-alternativnyj-nonascii-translation-table
    table)
  (define-translation-table 'cyrillic-alternativnyj-encode-table
    (char-table-extra-slot table 0)))

(makunbound 'cyrillic-alternativnyj-decode-table)

(define-ccl-program ccl-decode-alternativnyj
  `(4
    ((loop
      (r0 = 0)
      (read r1)
      (if (r1 < 128)
	  (write-repeat r1)
	((translate-character cyrillic-alternativnyj-nonascii-translation-table
			      r0 r1)
	 (translate-character ucs-translation-table-for-decode r0 r1)
	 (write-multibyte-character r0 r1)
	 (repeat))))))
  "CCL program to decode Alternativnyj.")

(define-ccl-program ccl-encode-alternativnyj
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (translate-character cyrillic-alternativnyj-encode-table r0 r1)
      (if (r0 != ,(charset-id 'ascii))
	  (if (r0 != ,(charset-id 'eight-bit-graphic))
	      (if (r0 != ,(charset-id 'eight-bit-control))
		  (r1 = ??))))
      (write-repeat r1))))
  "CCL program to encode Alternativnyj.")

(cyrillic-unify-encoding 'cyrillic-alternativnyj-encode-table)

(make-coding-system
 'cyrillic-alternativnyj 4 ?A
 "ALTERNATIVNYJ 8-bit encoding for Cyrillic."
 '(ccl-decode-alternativnyj . ccl-encode-alternativnyj)
 `((safe-chars . cyrillic-alternativnyj-encode-table)
   (valid-codes (0 . 255))
   (dependency unify-8859-on-encoding-mode unify-8859-on-decoding-mode)))

(define-coding-system-alias 'alternativnyj 'cyrillic-alternativnyj)

(define-ccl-program ccl-encode-alternativnyj-font
  `(0
    (translate-character cyrillic-alternativnyj-encode-table r0 r1))
  "CCL program to encode Cyrillic chars to Alternativnyj font.")

(add-to-list 'font-ccl-encoder-alist
	     '("alternativnyj" . ccl-encode-alternativnyj-font))

(set-language-info-alist
 "Cyrillic-ALT" `((charset cyrillic-iso8859-5)
		  (nonascii-translation
		   . ,(get 'cyrillic-alternativnyj-nonascii-translation-table
			   'translation-table))
		  (coding-system cyrillic-alternativnyj)
		  (coding-priority cyrillic-alternativnyj)
		  (input-method . "russian-typewriter")
		  (features cyril-util)
		  (unibyte-display . cyrillic-alternativnyj)
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ALTERNATIVNYJ."))
 '("Cyrillic"))

(set-language-info-alist
 "Tajik" `((coding-system cyrillic-koi8-t)
	   (coding-priority cyrillic-koi8-t)
	   (nonascii-translation
	    . ,(get 'decode-koi8-t 'translation-table))
	   (input-method . "russian-typewriter") ; fixme?
	   (features code-pages)
	   (documentation . "Support for Tajik using KOI8-T."))
 '("Cyrillic"))

(eval-and-compile
  (setq
   non-iso-charset-alist
   (cp-make-coding-system
    windows-1251
    [?\$,1("(B ?\$,1(#(B ?\$,1rz(B ?\$,1(s(B ?\$,1r~(B ?\$,1s&(B ?\$,1s (B ?\$,1s!(B ?\$,1tL(B ?\$,1s0(B ?\$,1()(B ?\$,1s9(B ?\$,1(*(B ?\$,1(,(B ?\$,1(+(B ?\$,1(/(B ?\$,1(r(B
	 ?\$,1rx(B ?\$,1ry(B ?\$,1r|(B ?\$,1r}(B ?\$,1s"(B ?\$,1rs(B ?\$,1rt(B nil ?\$,1ub(B ?\$,1(y(B ?\$,1s:(B ?\$,1(z(B ?\$,1(|(B ?\$,1({(B ?\$,1((B ?\,A (B ?\$,1(.(B
	 ?\$,1(~(B ?\$,1(((B ?\,A$(B ?\$,1)P(B ?\,A&(B ?\,A'(B ?\$,1(!(B ?\,A)(B ?\$,1($(B ?\,A+(B ?\,A,(B ?\,A-(B ?\,A.(B ?\$,1('(B ?\,A0(B ?\,A1(B ?\$,1(&(B
	 ?\$,1(v(B ?\$,1)Q(B ?\,A5(B ?\,A6(B ?\,A7(B ?\$,1(q(B ?\$,1uV(B ?\$,1(t(B ?\,A;(B ?\$,1(x(B ?\$,1(%(B ?\$,1(u(B ?\$,1(w(B ?\$,1(0(B ?\$,1(1(B ?\$,1(2(B ?\$,1(3(B
	 ?\$,1(4(B ?\$,1(5(B ?\$,1(6(B ?\$,1(7(B ?\$,1(8(B ?\$,1(9(B ?\$,1(:(B ?\$,1(;(B ?\$,1(<(B ?\$,1(=(B ?\$,1(>(B ?\$,1(?(B ?\$,1(@(B ?\$,1(A(B ?\$,1(B(B ?\$,1(C(B ?\$,1(D(B
	 ?\$,1(E(B ?\$,1(F(B ?\$,1(G(B ?\$,1(H(B ?\$,1(I(B ?\$,1(J(B ?\$,1(K(B ?\$,1(L(B ?\$,1(M(B ?\$,1(N(B ?\$,1(O(B ?\$,1(P(B ?\$,1(Q(B ?\$,1(R(B ?\$,1(S(B ?\$,1(T(B ?\$,1(U(B
	 ?\$,1(V(B ?\$,1(W(B ?\$,1(X(B ?\$,1(Y(B ?\$,1(Z(B ?\$,1([(B ?\$,1(\(B ?\$,1(](B ?\$,1(^(B ?\$,1(_(B ?\$,1(`(B ?\$,1(a(B ?\$,1(b(B ?\$,1(c(B ?\$,1(d(B ?\$,1(e(B ?\$,1(f(B
	 ?\$,1(g(B ?\$,1(h(B ?\$,1(i(B ?\$,1(j(B ?\$,1(k(B ?\$,1(l(B ?\$,1(m(B ?\$,1(n(B ?\$,1(o(B] nil ?b)))

;; Register cyrillic-iso8859-5 characters in the encode table of
;; windows-1251.
(let ((table (get 'encode-windows-1251 'translation-table))
      ;; Nth element is a cyrillic-iso8859-5 character encoded to a
      ;; code (128 + N), or nil.
      (vec [?\,L"(B ?\,L#(B nil ?\,Ls(B nil nil nil nil nil nil ?\,L)(B nil ?\,L*(B ?\,L,(B ?\,L+(B ?\,L/(B
	    ?\,Lr(B nil nil nil nil nil nil nil nil nil ?\,Ly(B nil ?\,Lz(B ?\,L|(B ?\,L{(B ?\,L(B
	    nil ?\,L.(B ?\,L~(B ?\,L((B nil nil nil nil ?\,L!(B nil ?\,L$(B nil nil nil nil ?\,L'(B
	    nil nil ?\,L&(B ?\,Lv(B nil nil nil nil ?\,Lq(B ?\,Lp(B ?\,Lt(B nil ?\,Lx(B ?\,L%(B ?\,Lu(B ?\,Lw(B
	    ?\,L0(B ?\,L1(B ?\,L2(B ?\,L3(B ?\,L4(B ?\,L5(B ?\,L6(B ?\,L7(B ?\,L8(B ?\,L9(B ?\,L:(B ?\,L;(B ?\,L<(B ?\,L=(B ?\,L>(B ?\,L?(B
	    ?\,L@(B ?\,LA(B ?\,LB(B ?\,LC(B ?\,LD(B ?\,LE(B ?\,LF(B ?\,LG(B ?\,LH(B ?\,LI(B ?\,LJ(B ?\,LK(B ?\,LL(B ?\,LM(B ?\,LN(B ?\,LO(B
	    ?\,LP(B ?\,LQ(B ?\,LR(B ?\,LS(B ?\,LT(B ?\,LU(B ?\,LV(B ?\,LW(B ?\,LX(B ?\,LY(B ?\,LZ(B ?\,L[(B ?\,L\(B ?\,L](B ?\,L^(B ?\,L_(B
	    ?\,L`(B ?\,La(B ?\,Lb(B ?\,Lc(B ?\,Ld(B ?\,Le(B ?\,Lf(B ?\,Lg(B ?\,Lh(B ?\,Li(B ?\,Lj(B ?\,Lk(B ?\,Ll(B ?\,Lm(B ?\,Ln(B ?\,Lo(B]))
  (dotimes (i (length vec))
    (if (aref vec i)
	(aset table (aref vec i) (+ 128 i)))))

(define-coding-system-alias 'cp1251 'windows-1251)

(let ((elt `("microsoft-cp1251" windows-1251 1
	     ,(get 'encode-windows-1251 'translation-table)))
      (slot (assoc "microsoft-cp1251" ctext-non-standard-encodings-alist)))
  (if slot
      (setcdr slot (cdr elt))
    (push elt ctext-non-standard-encodings-alist)))

(define-ccl-program ccl-encode-windows-1251-font
  `(0
    ((if (r0 == ,(charset-id 'mule-unicode-0100-24ff))
	 ((r1 <<= 7)
	  (r1 += r2)))
     (translate-character encode-windows-1251 r0 r1))))

(add-to-list 'font-ccl-encoder-alist
	     '("microsoft-cp1251" . ccl-encode-windows-1251-font))

(set-language-info-alist
 "Bulgarian" `((coding-system windows-1251)
	       (coding-priority windows-1251)
	       (ctext-non-standard-encodings "microsoft-cp1251")
	       (overriding-fontspec
		(,(get 'encode-windows-1251 'translation-table)
		 . (nil . "microsoft-cp1251"))
		(,(get 'cyrillic-koi8-r-encode-table 'translation-table)
		 . (nil . "koi8-r")))
	       (nonascii-translation
		. ,(get 'decode-windows-1251 'translation-table))
	       (input-method . "bulgarian-bds")
	       (documentation
		. "Support for Bulgarian with windows-1251 character set.")
	       (tutorial . "TUTORIAL.bg"))
 '("Cyrillic"))

(set-language-info-alist
 "Belarusian" `((coding-system windows-1251)
		(coding-priority windows-1251)
		(ctext-non-standard-encodings "microsoft-cp1251")
		(overriding-fontspec
		 (,(get 'encode-windows-1251 'translation-table)
		  . (nil . "microsoft-cp1251"))
		 (,(get 'cyrillic-koi8-r-encode-table 'translation-table)
		  . (nil . "koi8-r")))
		(nonascii-translation
		 . ,(get 'decode-windows-1251 'translation-table))
		(input-method . "belarusian")
		(documentation
		 . "Support for Belarusian with windows-1251 character set.
\(The name Belarusian replaced Byelorussian in the early 1990s.)"))
 '("Cyrillic"))

(provide 'cyrillic)

;;; arch-tag: bda71ae0-ba41-4cb6-a6e0-1dff542313d3
;;; cyrillic.el ends here
