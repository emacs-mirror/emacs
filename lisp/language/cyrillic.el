;;; cyrillic.el --- support for Cyrillic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Cyrillic

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

;; The character set ISO8859-5 is supported.  See
;; http://www.ecma.ch/ecma1/STAND/ECMA-113.HTM.  KOI-8 and
;; ALTERNATIVNYJ are converted to ISO8859-5 internally.

;;; Code:

;; Cyrillic (general)

;; ISO-8859-5 staff

(make-coding-system
 'cyrillic-iso-8bit 2 ?5
 "ISO 2022 based 8-bit encoding for Cyrillic script (MIME:ISO-8859-5)."
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

;; KOI-8 staff

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
   128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
   144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
   160 161 162 ?,Lq(B  164 165 166 167 168 169 170 171 172 173 174 175
   176 177 178 ?,L!(B  180 181 182 183 184 185 186 187 188 189 190 191
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
  "CCL program to decode KOI8.")

(define-ccl-program ccl-encode-koi8
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (if (r0 == ,(charset-id 'cyrillic-iso8859-5))
	  (translate-character cyrillic-koi8-r-encode-table r0 r1))
      (write-repeat r1))))
  "CCL program to encode KOI8.")
	     
(make-coding-system
 'cyrillic-koi8 4
 ;; We used to use ?K.  It is true that ?K is more strictly correct,
 ;; but it is also used for Korean.
 ;; So people who use koi8 for languages other than Russian
 ;; will have to forgive us.
 ?R
 "KOI8 8-bit encoding for Cyrillic (MIME: KOI8-R)."
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
   176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191
   192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207
   208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223
   ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,Ld(B  ?,Le(B  ?,Lf(B  ?,Lg(B  ?,Lh(B  ?,Li(B  ?,Lj(B  ?,Lk(B  ?,Ll(B  ?,Lm(B  ?,Ln(B  ?,Lo(B
   ?,L!(B  ?,Lq(B  ?,L$(B  ?,Lt(B  ?,L'(B  ?,Lw(B  ?,L.(B  ?,L~(B  248 249 250 251 ?,Lp(B  253 254 ?,L (B]
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
 "ALTERNATIVNYJ 8-bit encoding for Cyrillic."
 '(ccl-decode-alternativnyj . ccl-encode-alternativnyj)
 `((safe-chars . ,(let ((table (make-char-table 'safe-chars))
			(i 0))
		    (while (< i 256)
		      (aset table (aref cyrillic-alternativnyj-decode-table i)
			    t)
		      (setq i (1+ i)))
		    table))
   (valid-codes (0 . 175) (224 . 241) 255)
   (charset-origin-alist (cyrillic-iso8859-5 "ALTERNATIVNYJ"
					     cyrillic-encode-koi8-r-char))))


(define-coding-system-alias 'alternativnyj 'cyrillic-alternativnyj)

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

(provide 'cyrillic)

;;; cyrillic.el ends here
