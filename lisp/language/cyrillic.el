;;; cyrillic.el --- Support for languages which use Cyrillic characters

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

;; The character set ISO8859-5 is supported.  KOI-8 and ALTERNATIVNYJ
;; are converted to ISO8859-5 internally.

;;; Code:

;; Cyrillic (general)

(define-prefix-command 'describe-cyrillic-environment-map)
(define-key-after describe-language-environment-map [Cyrillic]
  '("Cyrillic" . describe-cyrillic-environment-map)
  t)

(define-prefix-command 'setup-cyrillic-environment-map)
(define-key-after setup-language-environment-map [Cyrillic]
  '("Cyrillic" . setup-cyrillic-environment-map)
  t)

;; ISO-8859-5 staff

(make-coding-system
 'cyrillic-iso-8bit 2 ?5
 "ISO 2022 based 8-bit encoding for Cyrillic script (MIME:ISO-8859-5)"
 '((ascii t) (cyrillic-iso8859-5 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(define-coding-system-alias 'iso-8859-5 'cyrillic-iso-8bit)

(set-language-info-alist
 "Cyrillic-ISO" '((setup-function . (setup-cyrillic-iso-environment
				     . setup-cyrillic-environment-map))
		  (charset . (cyrillic-iso8859-5))
		  (coding-system . (cyrillic-iso-8bit))
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . ("Support for Cyrillic ISO-8859-5."
				    . describe-cyrillic-environment-map))))

;; KOI-8 staff

(define-ccl-program ccl-decode-koi8
  '(3
    ((read r0)
     (loop
      (write-read-repeat
       r0
       [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
	  16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
	  32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
	  48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
	  64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
	  80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
	  96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
	  112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
	  128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
	  144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
	  32  32  32 ?,Lq(B   32  32  32  32  32  32  32  32  32  32  32  32
	  32  32  32 ?,L!(B   32  32  32  32  32  32  32  32  32  32  32  32
	  ?,Ln(B  ?,LP(B  ?,LQ(B  ?,Lf(B  ?,LT(B  ?,LU(B  ?,Ld(B  ?,LS(B  ?,Le(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B 
	  ?,L_(B  ?,Lo(B  ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,LV(B  ?,LR(B  ?,Ll(B  ?,Lk(B  ?,LW(B  ?,Lh(B  ?,Lm(B  ?,Li(B  ?,Lg(B  ?,Lj(B 
	  ?,LN(B  ?,L0(B  ?,L1(B  ?,LF(B  ?,L4(B  ?,L5(B  ?,LD(B  ?,L3(B  ?,LE(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B 
	  ?,L?(B  ?,LO(B  ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,L6(B  ?,L2(B  ?,LL(B  ?,LK(B  ?,L7(B  ?,LH(B  ?,LM(B  ?,LI(B  ?,LG(B  ?,LJ(B ]))))
  "CCL program to decode KOI8.")

(define-ccl-program ccl-encode-koi8
  `(1
    ((read r0)
     (loop
      (if (r0 != ,(charset-id 'cyrillic-iso8859-5))
	  (write-read-repeat r0)
	((read r0)
	 (r0 -= 160)
	 (write-read-repeat
	  r0
	  [ 32 179  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	       225 226 247 231 228 229 246 250 233 234 235 236 237 238 239 240
	       242 243 244 245 230 232 227 254 251 253 255 249 248 252 224 241
	       193 194 215 199 196 197 214 218 201 202 203 204 205 206 207 208
	       210 211 212 213 198 200 195 222 219 221 223 217 216 220 192 209
	       32 163  32  32  32  32  32  32  32  32  32  32  32  32  32  32])
	 )))))
  "CCL program to encode KOI8.")
	     
(make-coding-system
 'cyrillic-koi8 4
 ;; We used to use ?K.  It is true that ?K is more strictly correct,
 ;; but it is also used for Korean.
 ;; So people who use koi8 for languages other than Russian
 ;; will have to forgive us.
 ?R "KOI8 8-bit encoding for Cyrillic (MIME: KOI8-R)"
 (cons ccl-decode-koi8 ccl-encode-koi8))

(define-coding-system-alias 'koi8-r 'cyrillic-koi8)
(define-coding-system-alias 'koi8 'cyrillic-koi8)

(define-ccl-program ccl-encode-koi8-font
  '(0
    ((r1 -= 160)
     (r1 = r1
	 [ 32 179  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	      225 226 247 231 228 229 246 250 233 234 235 236 237 238 239 240
	      242 243 244 245 230 232 227 254 251 253 255 249 248 252 224 241
	      193 194 215 199 196 197 214 218 201 202 203 204 205 206 207 208
	      210 211 212 213 198 200 195 222 219 221 223 217 216 220 192 209
	      32 163  32  32  32  32  32  32  32  32  32  32  32  32  32  32])
     ))
  "CCL program to encode Cyrillic chars to KOI font.")

(setq font-ccl-encoder-alist
      (cons (cons "koi8" ccl-encode-koi8-font) font-ccl-encoder-alist))

(set-language-info-alist
 "Cyrillic-KOI8" '((setup-function . (setup-cyrillic-koi8-environment
				      . setup-cyrillic-environment-map))
		   (charset . (cyrillic-iso8859-5))
		   (coding-system . (cyrillic-koi8))
		   (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		   (documentation . ("Support for Cyrillic KOI-8."
				     . describe-cyrillic-environment-map))))

;;; ALTERNATIVNYJ staff

(define-ccl-program ccl-decode-alternativnyj
  '(3
    ((read r0)
     (loop
      (write-read-repeat
       r0
       [  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
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
	 32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	 32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	 32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	 ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,Ld(B  ?,Le(B  ?,Lf(B  ?,Lg(B  ?,Lh(B  ?,Li(B  ?,Lj(B  ?,Lk(B  ?,Ll(B  ?,Lm(B  ?,Ln(B  ?,Lo(B
	 ?,L!(B  ?,Lq(B   32  32  32  32  32  32  32  32  32  32  32  32  32 ?,Lp(B]))))
  "CCL program to decode Alternativnyj.")

(define-ccl-program ccl-encode-alternativnyj
  `(1
    ((read r0)
     (loop
      (if (r0 != ,(charset-id 'cyrillic-iso8859-5))
	  (write-read-repeat r0)
	((read r0)
	 (r0 -= 160)
	 (write-read-repeat
	  r0
	  [ 32 240  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	       128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
	       144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
	       160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
	       224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
	       255 241  32  32  32  32  32  32  32  32  32  32  32  32  32  32])
	 )))))
  "CCL program to encode Alternativnyj.")
	     
(make-coding-system
 'cyrillic-alternativnyj 4 ?A
 "ALTERNATIVNYJ 8-bit encoding for Cyrillic"
 (cons ccl-decode-alternativnyj ccl-encode-alternativnyj))

(define-coding-system-alias 'alternativnyj 'cyrillic-alternativnyj)

(define-ccl-program ccl-encode-alternativnyj-font
  '(0
    ((r1 -= 160)
     (r1 = r1
	 [ 32 240  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	      128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
	      144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
	      160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
	      224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
	      255 241  32  32  32  32  32  32  32  32  32  32  32  32  32  32])
     ))
  "CCL program to encode Cyrillic chars to Alternativnyj font.")

(setq font-ccl-encoder-alist
      (cons (cons "alternativnyj" ccl-encode-alternativnyj-font)
	    font-ccl-encoder-alist))

(set-language-info-alist
 "Cyrillic-ALT" '((setup-function . (setup-cyrillic-alternativnyj-environment
				     . setup-cyrillic-environment-map))
		  (charset . (cyrillic-iso8859-5))
		  (coding-system . (cyrillic-alternativnyj))
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . ("Support for Cyrillic ALTERNATIVNYJ."
				    . describe-cyrillic-environment-map))))

;;; cyrillic.el ends here
