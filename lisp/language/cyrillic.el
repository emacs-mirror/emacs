;;; cyrillic.el --- support for Cyrillic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001  Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The character set ISO8859-5 is supported.  KOI-8 and ALTERNATIVNYJ
;; are converted to ISO8859-5+Unicode internally.  See
;; <URL:http://www.ecma.ch/ecma1/STAND/ECMA-113.HTM>.  For more info
;; on Cyrillic charsets, see
;; <URL:http://czyborra.com/charsets/cyrillic.html>.

;; See codepages.el for straight Unicode definitions of the non-ISO
;; charsets.

;;; Code:

;; Cyrillic (general)

;; ISO-8859-5 stuff

(make-coding-system
 'cyrillic-iso-8bit 2 ?5
 "ISO 2022 based 8-bit encoding for Cyrillic script (MIME:ISO-8859-5)"
 '(ascii cyrillic-iso8859-5  nil nil
   nil nil nil nil nil nil nil)
 '((safe-charsets ascii cyrillic-iso8859-5)
   (mime-charset . iso-8859-5)))

(define-coding-system-alias 'iso-8859-5 'cyrillic-iso-8bit)

(set-language-info-alist
 "Cyrillic-ISO" '((charset cyrillic-iso8859-5)
		  (coding-system cyrillic-iso-8bit)
		  (coding-priority cyrillic-iso-8bit)
		  (input-method . "cyrillic-yawerty")
		  (nonascii-translation . cyrillic-iso8859-5)
		  (unibyte-display . cyrillic-iso-8bit)
		  (features cyril-util)
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ISO-8859-5."))
 '("Cyrillic"))

;; KOI-8 stuff

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
   ?$,2  (B ?$,2 "(B ?$,2 ,(B ?$,2 0(B ?$,2 4(B ?$,2 8(B ?$,2 <(B ?$,2 D(B ?$,2 L(B ?$,2 T(B ?$,2 \(B ?$,2!@(B ?$,2!D(B ?$,2!H(B ?$,2!L(B ?$,2!P(B
   ?$,2!Q(B ?$,2!R(B ?$,2!S(B ?$,1{ (B ?$,2!`(B ?$,1x9(B ?$,1x:(B ?$,1xh(B ?$,1y$(B ?$,1y%(B ?,A (B ?$,1{!(B ?,A0(B ?,A2(B ?,A7(B ?,Aw(B
   ?$,2 p(B ?$,2 q(B ?$,2 r(B ?,Lq(B ?$,2 s(B ?$,2 t(B ?$,2 u(B ?$,2 v(B ?$,2 w(B ?$,2 x(B ?$,2 y(B ?$,2 z(B ?$,2 {(B ?$,2 |(B ?$,2 }(B ?$,2 ~(B
   ?$,2 (B ?$,2! (B ?$,2!!(B ?,L!(B ?$,2!"(B ?$,2!#(B ?$,2!$(B ?$,2!%(B ?$,2!&(B ?$,2!'(B ?$,2!((B ?$,2!)(B ?$,2!*(B ?$,2!+(B ?$,2!,(B ?,A)(B
   ?,Ln(B  ?,LP(B  ?,LQ(B  ?,Lf(B  ?,LT(B  ?,LU(B  ?,Ld(B  ?,LS(B  ?,Le(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B 
   ?,L_(B  ?,Lo(B  ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,LV(B  ?,LR(B  ?,Ll(B  ?,Lk(B  ?,LW(B  ?,Lh(B  ?,Lm(B  ?,Li(B  ?,Lg(B  ?,Lj(B 
   ?,LN(B  ?,L0(B  ?,L1(B  ?,LF(B  ?,L4(B  ?,L5(B  ?,LD(B  ?,L3(B  ?,LE(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B 
   ?,L?(B  ?,LO(B  ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,L6(B  ?,L2(B  ?,LL(B  ?,LK(B  ?,L7(B  ?,LH(B  ?,LM(B  ?,LI(B  ?,LG(B  ?,LJ(B ]
  "Cyrillic KOI8-R decoding table.")

(let ((table (make-translation-table-from-vector
	      cyrillic-koi8-r-decode-table)))
  (define-translation-table 'cyrillic-koi8-r-nonascii-translation-table table)
  (define-translation-table 'cyrillic-koi8-r-encode-table
    (char-table-extra-slot table 0)))

(define-ccl-program ccl-decode-koi8
  `(3
    ((loop
      (r0 = 0)
      (read r1)
      (if (r1 < 128)
	  (write-repeat r1)
	((translate-character cyrillic-koi8-r-nonascii-translation-table r0 r1)
	 (write-multibyte-character r0 r1)
	 (repeat))))))
  "CCL program to decode KOI8-R.")

(define-ccl-program ccl-encode-koi8
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (translate-character cyrillic-koi8-r-encode-table r0 r1)
      (write-repeat r1))))
  "CCL program to encode KOI8-R.")
	     
(make-coding-system
 'cyrillic-koi8 4
 ;; We used to use ?K.  It is true that ?K is more strictly correct,
 ;; but it is also used for Korean.
 ;; So people who use koi8 for languages other than Russian
 ;; will have to forgive us.
 ?R "KOI8-R 8-bit encoding for Cyrillic (MIME: KOI8-R)"
 '(ccl-decode-koi8 . ccl-encode-koi8)
 `((safe-chars . ,(let ((table (make-char-table 'safe-chars))
			(i 0))
		    (while (< i 256)
		      (aset table (aref cyrillic-koi8-r-decode-table i) t)
		      (setq i (1+ i)))
		    table))
   (mime-charset . koi8-r)
   (valid-codes (0 . 127) 163 179 (192 . 255))
   (charset-origin-alist (cyrillic-iso8859-5 "KOI8-R"
					     cyrillic-encode-koi8-r-char))))

(define-coding-system-alias 'koi8-r 'cyrillic-koi8)
(define-coding-system-alias 'koi8 'cyrillic-koi8)
(define-coding-system-alias 'cp878 'cyrillic-koi8)

(define-ccl-program ccl-encode-koi8-font
  `(0
    ((translate-character cyrillic-koi8-r-encode-table r0 r1)))
  "CCL program to encode Cyrillic chars to KOI font.")

(setq font-ccl-encoder-alist
      (cons '("koi8" . ccl-encode-koi8-font) font-ccl-encoder-alist))

(set-language-info-alist
 "Cyrillic-KOI8" `((charset cyrillic-iso8859-5)
		   (nonascii-translation
		    . ,(get 'cyrillic-koi8-r-nonascii-translation-table
			    'translation-table))
		   (coding-system cyrillic-koi8)
		   (coding-priority cyrillic-koi8)
		   (input-method . "cyrillic-jcuken")
		   (features cyril-util)
		   (unibyte-display . cyrillic-koi8)
		   (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		   (documentation . "Support for Cyrillic KOI8-R."))
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
;;    ?\$,2  (B ?\$,2 "(B ?\$,2 ,(B ?\$,2 0(B ?\$,2 4(B ?\$,2 8(B ?\$,2 <(B ?\$,2 D(B ?\$,2 L(B ?\$,2 T(B ?\$,2 \(B ?\$,2!@(B ?\$,2!D(B ?\$,2!H(B ?\$,2!L(B ?\$,2!P(B
;;    ?\$,2!Q(B ?\$,2!R(B ?\$,2!S(B ?\$,1{ (B ?\$,2!`(B ?\$,1x9(B ?\$,1x:(B ?\$,1xh(B ?\$,1y$(B ?\$,1y%(B ?\,A (B ?\$,1{!(B ?\,A0(B ?\,A2(B ?\,A7(B ?\,Aw(B
;;    ?\$,2 p(B ?\$,2 q(B ?\$,2 r(B ?\$,1(q(B ?\$,1(t(B ?\$,2 t(B ?\$,1(v(B ?\$,1(w(B ?\$,2 w(B ?\$,2 x(B ?\$,2 y(B ?\$,2 z(B ?\$,2 {(B ?\$,1)Q(B ?\$,2 }(B ?\$,2 ~(B
;;    ?\$,2 (B ?\$,2! (B ?\$,2!!(B ?\$,1(!(B ?\$,1($(B ?\$,2!#(B ?\$,1(&(B ?\$,1('(B ?\$,2!&(B ?\$,2!'(B ?\$,2!((B ?\$,2!)(B ?\$,2!*(B ?\$,1)P(B ?\$,2!,(B ?\,A)(B
;;    ?\$,1(n(B ?\$,1(P(B ?\$,1(Q(B ?\$,1(f(B ?\$,1(T(B ?\$,1(U(B ?\$,1(d(B ?\$,1(S(B ?\$,1(e(B ?\$,1(X(B ?\$,1(Y(B ?\$,1(Z(B ?\$,1([(B ?\$,1(\(B ?\$,1(](B ?\$,1(^(B
;;    ?\$,1(_(B ?\$,1(o(B ?\$,1(`(B ?\$,1(a(B ?\$,1(b(B ?\$,1(c(B ?\$,1(V(B ?\$,1(R(B ?\$,1(l(B ?\$,1(k(B ?\$,1(W(B ?\$,1(h(B ?\$,1(m(B ?\$,1(i(B ?\$,1(g(B ?\$,1(j(B
;;    ?\$,1(N(B ?\$,1(0(B ?\$,1(1(B ?\$,1(F(B ?\$,1(4(B ?\$,1(5(B ?\$,1(D(B ?\$,1(3(B ?\$,1(E(B ?\$,1(8(B ?\$,1(9(B ?\$,1(:(B ?\$,1(;(B ?\$,1(<(B ?\$,1(=(B ?\$,1(>(B
;;    ?\$,1(?(B ?\$,1(O(B ?\$,1(@(B ?\$,1(A(B ?\$,1(B(B ?\$,1(C(B ?\$,1(6(B ?\$,1(2(B ?\$,1(L(B ?\$,1(K(B ?\$,1(7(B ?\$,1(H(B ?\$,1(M(B ?\$,1(I(B ?\$,1(G(B ?\$,1(J(B
   ?\$,2  (B ?\$,2 "(B ?\$,2 ,(B ?\$,2 0(B ?\$,2 4(B ?\$,2 8(B ?\$,2 <(B ?\$,2 D(B ?\$,2 L(B ?\$,2 T(B ?\$,2 \(B ?\$,2!@(B ?\$,2!D(B ?\$,2!H(B ?\$,2!L(B ?\$,2!P(B
   ?\$,2!Q(B ?\$,2!R(B ?\$,2!S(B ?\$,1{ (B ?\$,2!`(B ?\$,1x9(B ?\$,1x:(B ?\$,1xh(B ?\$,1y$(B ?\$,1y%(B ?\,L (B ?\$,1{!(B ?\,A0(B ?\,A2(B ?\,A7(B ?\,Aw(B
   ?\$,2 p(B ?\$,2 q(B ?\$,2 r(B ?\,Lq(B ?\,Lt(B ?\$,2 t(B ?\,Lv(B ?\,Lw(B ?\$,2 w(B ?\$,2 x(B ?\$,2 y(B ?\$,2 z(B ?\$,2 {(B ?\$,1)Q(B ?\$,2 }(B ?\$,2 ~(B
   ?\$,2 (B ?\$,2! (B ?\$,2!!(B ?\,L!(B ?\,L$(B ?\$,2!#(B ?\,L&(B ?\,L'(B ?\$,2!&(B ?\$,2!'(B ?\$,2!((B ?\$,2!)(B ?\$,2!*(B ?\$,1)P(B ?\$,2!,(B ?\,A)(B
   ?\,Ln(B ?\,LP(B ?\,LQ(B ?\,Lf(B ?\,LT(B ?\,LU(B ?\,Ld(B ?\,LS(B ?\,Le(B ?\,LX(B ?\,LY(B ?\,LZ(B ?\,L[(B ?\,L\(B ?\,L](B ?\,L^(B
   ?\,L_(B ?\,Lo(B ?\,L`(B ?\,La(B ?\,Lb(B ?\,Lc(B ?\,LV(B ?\,LR(B ?\,Ll(B ?\,Lk(B ?\,LW(B ?\,Lh(B ?\,Lm(B ?\,Li(B ?\,Lg(B ?\,Lj(B
   ?\,LN(B ?\,L0(B ?\,L1(B ?\,LF(B ?\,L4(B ?\,L5(B ?\,LD(B ?\,L3(B ?\,LE(B ?\,L8(B ?\,L9(B ?\,L:(B ?\,L;(B ?\,L<(B ?\,L=(B ?\,L>(B
   ?\,L?(B ?\,LO(B ?\,L@(B ?\,LA(B ?\,LB(B ?\,LC(B ?\,L6(B ?\,L2(B ?\,LL(B ?\,LK(B ?\,L7(B ?\,LH(B ?\,LM(B ?\,LI(B ?\,LG(B ?\,LJ(B]
  "Cyrillic KOI8-U decoding table.")

(let ((table (make-translation-table-from-vector
	      cyrillic-koi8-u-decode-table)))
  (define-translation-table 'cyrillic-koi8-u-nonascii-translation-table table)
  (define-translation-table 'cyrillic-koi8-u-encode-table
    (char-table-extra-slot table 0)))

(define-ccl-program ccl-decode-koi8-u
  `(3
    ((loop
      (r0 = 0)
      (read r1)
      (if (r1 < 128)
	  (write-repeat r1)
	((translate-character cyrillic-koi8-u-nonascii-translation-table r0 r1)
	 (write-multibyte-character r0 r1)
	 (repeat))))))
  "CCL program to decode KOI8-U.")

(define-ccl-program ccl-encode-koi8-u
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (translate-character cyrillic-koi8-u-encode-table r0 r1)
      (write-repeat r1))))
  "CCL program to encode KOI8-U.")
	     
(make-coding-system
 'koi8-u 4
 ?U "KOI8 8-bit encoding for Cyrillic (MIME: KOI8-U)"
 '(ccl-decode-koi8-u . ccl-encode-koi8-u)
 `((safe-chars . ,(let ((table (make-char-table 'safe-chars))
			(i 0))
		    (while (< i 256)
		      (aset table (aref cyrillic-koi8-u-decode-table i) t)
		      (setq i (1+ i)))
		    table))
   (mime-charset . koi8-u)
   (valid-codes (0 . 127) 163 179 (192 . 255))
   (charset-origin-alist (cyrillic-iso8859-5 "KOI8-U"
					     cyrillic-encode-koi8-u-char))))

(define-ccl-program ccl-encode-koi8-u-font
  `(0
    ((translate-character cyrillic-koi8-u-encode-table r0 r1)))
  "CCL program to encode Cyrillic chars to KOI-U font.")

(setq font-ccl-encoder-alist
      (cons '("koi8-u" . ccl-encode-koi8-u-font) font-ccl-encoder-alist))

(set-language-info-alist
 "Cyrillic-KOI8-U" `((charset cyrillic-iso8859-5)
		   (coding-system cyrillic-koi8-u)
		   (coding-priority cyrillic-koi8-u)
		   (input-method . "cyrillic-ukrainian")
		   (features cyril-util)
		   (unibyte-display . cyrillic-koi8-u)
		   (documentation . "Support for Cyrillic KOI8-U."))
 '("Cyrillic"))

;;; ALTERNATIVNYJ staff

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
   ?,L0(B  ?,L1(B  ?,L2(B  ?,L3(B  ?,L4(B  ?,L5(B  ?,L6(B  ?,L7(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B  ?,L?(B
   ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,LD(B  ?,LE(B  ?,LF(B  ?,LG(B  ?,LH(B  ?,LI(B  ?,LJ(B  ?,LK(B  ?,LL(B  ?,LM(B  ?,LN(B  ?,LO(B
   ?,LP(B  ?,LQ(B  ?,LR(B  ?,LS(B  ?,LT(B  ?,LU(B  ?,LV(B  ?,LW(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B  ?,L_(B
   ?$,2!Q(B  ?$,2!R(B  ?$,2!S(B  ?$,2 "(B  ?$,2 D(B  ?$,2!!(B  ?$,2!"(B  ?$,2 v(B  ?$,2 u(B  ?$,2!#(B  ?$,2 q(B  ?$,2 w(B  ?$,2 }(B  ?$,2 |(B  ?$,2 {(B  ?$,2 0(B
   ?$,2 4(B  ?$,2 T(B  ?$,2 L(B  ?$,2 <(B  ?$,2  (B  ?$,2 \(B  ?$,2 ~(B  ?$,2 (B  ?$,2 z(B  ?$,2 t(B  ?$,2!)(B  ?$,2!&(B  ?$,2! (B  ?$,2 p(B  ?$,2!,(B  ?$,2!'(B
   ?$,2!((B  ?$,2!$(B  ?$,2!%(B  ?$,2 y(B  ?$,2 x(B  ?$,2 r(B  ?$,2 s(B  ?$,2!+(B  ?$,2!*(B  ?$,2 8(B  ?$,2 ,(B  ?$,2!H(B  ?$,2!D(B  ?$,2!L(B  ?$,2!P(B  ?$,2!@(B
   ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,Ld(B  ?,Le(B  ?,Lf(B  ?,Lg(B  ?,Lh(B  ?,Li(B  ?,Lj(B  ?,Lk(B  ?,Ll(B  ?,Lm(B  ?,Ln(B  ?,Lo(B
   ?,L!(B  ?,Lq(B  ?$,1($(B  ?$,1(t(B  ?$,1('(B  ?$,1(w(B  ?$,1(.(B  ?$,1(~(B  ?,A0(B  ?$,1s"(B  ?,A7(B  ?$,1x:(B  ?,Lp(B  ?,A$(B  ?$,2!`(B  ?,L (B]
  "Cyrillic ALTERNATIVNYJ decoding table.")

(let ((table (make-translation-table-from-vector
	      cyrillic-alternativnyj-decode-table)))
  (define-translation-table 'cyrillic-alternativnyj-nonascii-translation-table
    table)
  (define-translation-table 'cyrillic-alternativnyj-encode-table
    (char-table-extra-slot table 0)))


(define-ccl-program ccl-decode-alternativnyj
  `(3
    ((loop
      (r0 = 0)
      (read r1)
      (if (r1 < 128)
	  (write-repeat r1)
	((translate-character cyrillic-alternativnyj-nonascii-translation-table
			      r0 r1)
	 (write-multibyte-character r0 r1)
	 (repeat))))))
  "CCL program to decode Alternativnyj.")

(define-ccl-program ccl-encode-alternativnyj
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (translate-character cyrillic-alternativnyj-encode-table r0 r1)
      (write-repeat r1))))
  "CCL program to encode Alternativnyj.")
	     
(make-coding-system
 'cyrillic-alternativnyj 4 ?A
 "ALTERNATIVNYJ 8-bit encoding for Cyrillic"
 '(ccl-decode-alternativnyj . ccl-encode-alternativnyj)
 `((safe-chars . ,(let ((table (make-char-table 'safe-chars))
			(i 0))
		    (while (< i 256)
		      (aset table (aref cyrillic-alternativnyj-decode-table i)
			    t)
		      (setq i (1+ i)))
		    table))
   (valid-codes (0 . 175) (224 . 241) 255)
   (mime-charset . cp866)
   (charset-origin-alist (cyrillic-iso8859-5 "ALTERNATIVNYJ"
					     cyrillic-encode-koi8-r-char))))


(define-coding-system-alias 'alternativnyj 'cyrillic-alternativnyj)
(define-coding-system-alias 'cp866 'cyrillic-alternativnyj)

(define-ccl-program ccl-encode-alternativnyj-font
  '(0
    ((translate-character cyrillic-alternativnyj-encode-table r0 r1)))
  "CCL program to encode Cyrillic chars to Alternativnyj font.")

(setq font-ccl-encoder-alist
      (cons '("alternativnyj" . ccl-encode-alternativnyj-font)
	    font-ccl-encoder-alist))

(set-language-info-alist
 "Cyrillic-ALT" `((charset cyrillic-iso8859-5)
		  (nonascii-translation
		   . ,(get 'cyrillic-alternativnyj-nonascii-translation-table
			   'translation-table))
		  (coding-system cyrillic-alternativnyj)
		  (coding-priority cyrillic-alternativnyj)
		  (input-method . "cyrillic-jcuken")
		  (features cyril-util)
		  (unibyte-display . cyrillic-alternativnyj)
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ALTERNATIVNYJ."))
 '("Cyrillic"))

(set-language-info-alist
 "Windows-1251" `((coding-system windows-1251)
		  (coding-priority windows-1251)
		  (features codepages)
		  (documentation . "Support for windows-1251 character set."))
 '("Cyrillic"))

(set-language-info-alist
 "Cyrillic-KOI8-T" `((coding-system cyrillic-koi8-t)
		     (coding-priority cyrillic-koi8-t)
		     (features codepages)
		     (documentation . "Support for Cyrillic KOI8-T."))
 '("Cyrillic"))

(set-language-info-alist
 "Bulgarian" `((coding-system windows-1251)
		  (coding-priority windows-1251)
		  (input-method . "cyrillic-translit-bulgarian")
		  (features codepages)
		  (documentation . "Support for Bulgrian with windows-1251 character set."))
 '("Cyrillic"))

(set-language-info-alist
 "Belarussian" `((coding-system windows-1251)
		  (coding-priority windows-1251)
		  (input-method . "cyrillic-beylorussian")
		  (features codepages)
		  (documentation
		   . "Support for Belarussian with windows-1251 character set."))
 '("Cyrillic"))

(provide 'cyrillic)

;;; cyrillic.el ends here
