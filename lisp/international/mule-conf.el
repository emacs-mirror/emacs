;;; mule-conf.el --- configure multilingual environment

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009
;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Keywords: i18n, mule, multilingual, character set, coding system

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

;; This file defines the Emacs charsets and some basic coding systems.
;; Other coding systems are defined in the files in directory
;; lisp/language.

;;; Code:

;; The ISO-IR registry is at http://www.itscj.ipsj.or.jp/ISO-IR/.
;; Standards docs equivalent to iso-2022 and iso-8859 are at
;; http://www.ecma.ch/.

;;; Definitions of character sets.

;; The charsets `ascii' and `unicode' are already defined in charset.c
;; as below:
;;
;; (define-charset 'ascii
;;   ""
;;   :dimension 1
;;   :code-space [0 127]
;;   :iso-final-char ?A
;;   :ascii-compatible-p t
;;   :emacs-mule-id 0
;;   :code-offset 0)
;;
;; (define-charset 'unicode
;;   ""
;;   :dimension 3
;;   :code-space [0 255 0 255 0 16]
;;   :ascii-compatible-p t
;;   :code-offset 0)
;;
;; We now set :docstring, :short-name, and :long-name properties.

(put-charset-property
 'ascii :docstring "ASCII (ISO646 IRV)")
(put-charset-property
 'ascii :short-name "ASCII")
(put-charset-property
 'ascii :long-name "ASCII (ISO646 IRV)")
(put-charset-property
 'unicode :docstring "Unicode (ISO10646)")
(put-charset-property
 'unicode :short-name "Unicode")
(put-charset-property
 'unicode :long-name "Unicode (ISO10646)")

(define-charset-alias 'ucs 'unicode)

(define-charset 'emacs
  "Full Emacs characters."
  :ascii-compatible-p t
  :code-space [ 0 255 0 255 0 63 ]
  :code-offset 0
  :supplementary-p t)

(define-charset 'iso-8859-1
  "Latin-1 (ISO/IEC 8859-1)"
  :short-name "Latin-1"
  :ascii-compatible-p t
  :code-space [0 255]
  :code-offset 0)

(define-charset 'latin-iso8859-1
  "Right-Hand Part of ISO/IEC 8859/1 (Latin-1): ISO-IR-100"
  :short-name "RHP of Latin-1"
  :long-name "RHP of ISO/IEC 8859/1 (Latin-1): ISO-IR-100"
  :iso-final-char ?A
  :emacs-mule-id 129
  :code-space [32 127]
  :code-offset 160)

(define-charset 'eight-bit-control
  "8-bit control code (0x80..0x9F)"
  :short-name "8-bit control code"
  :code-space [128 159]
  :code-offset 128)

(define-charset 'eight-bit-graphic
  "8-bit graphic code (0xA0..0xFF)"
  :short-name "8-bit graphic code"
  :code-space [160 255]
  :code-offset 160)

(defmacro define-iso-single-byte-charset (symbol iso-symbol name nickname
						 iso-ir iso-final
						 emacs-mule-id map)
  "For internal use only."
  `(progn
     (define-charset ,symbol
       ,name
       :short-name ,nickname
       :long-name ,name
       :ascii-compatible-p t
       :code-space [0 255]
       :map ,map)
     (if ,iso-symbol
	 (define-charset ,iso-symbol
	   (if ,iso-ir
	       (format "Right-Hand Part of %s (%s): ISO-IR-%d"
		       ,name ,nickname ,iso-ir)
	     (format "Right-Hand Part of %s (%s)" ,name ,nickname))
	   :short-name (format "RHP of %s" ,name)
	   :long-name (format "RHP of %s (%s)" ,name ,nickname)
	   :iso-final-char ,iso-final
	   :emacs-mule-id ,emacs-mule-id
	   :code-space [32 127]
	   :parents (list (cons ,symbol 128))))))

(define-iso-single-byte-charset 'iso-8859-2 'latin-iso8859-2
  "ISO/IEC 8859/2" "Latin-2" 101 ?B 130 "8859-2")

(define-iso-single-byte-charset 'iso-8859-3 'latin-iso8859-3
  "ISO/IEC 8859/3" "Latin-3" 109 ?C 131 "8859-3")

(define-iso-single-byte-charset 'iso-8859-4 'latin-iso8859-4
  "ISO/IEC 8859/4" "Latin-4" 110 ?D 132 "8859-4")

(define-iso-single-byte-charset 'iso-8859-5 'cyrillic-iso8859-5
  "ISO/IEC 8859/5" "Latin/Cyrillic" 144 ?L 140 "8859-5")

(define-iso-single-byte-charset 'iso-8859-6 'arabic-iso8859-6
  "ISO/IEC 8859/6" "Latin/Arabic" 127 ?G 135 "8859-6")

(define-iso-single-byte-charset 'iso-8859-7 'greek-iso8859-7
  "ISO/IEC 8859/7" "Latin/Greek" 126 ?F 134 "8859-7")

(define-iso-single-byte-charset 'iso-8859-8 'hebrew-iso8859-8
  "ISO/IEC 8859/8" "Latin/Hebrew" 138 ?H 136 "8859-8")

(define-iso-single-byte-charset 'iso-8859-9 'latin-iso8859-9
  "ISO/IEC 8859/9" "Latin-5" 148 ?M 141 "8859-9")

(define-iso-single-byte-charset 'iso-8859-10 'latin-iso8859-10
  "ISO/IEC 8859/9" "Latin-6" 157 ?V nil "8859-10")

;; 8859-11, 12 don't (yet?) exist.

(define-iso-single-byte-charset 'iso-8859-13 'latin-iso8859-13
  "ISO/IEC 8859/13" "Latin-7" 179 ?Y nil "8859-13")

(define-iso-single-byte-charset 'iso-8859-14 'latin-iso8859-14
  "ISO/IEC 8859/14" "Latin-8" 199 ?_ 143 "8859-14")

(define-iso-single-byte-charset 'iso-8859-15 'latin-iso8859-15
  "ISO/IEC 8859/15" "Latin-9" 203 ?b 142 "8859-15")

(define-iso-single-byte-charset 'iso-8859-16 'latin-iso8859-16
  "ISO/IEC 8859/16" "Latin-9" 226 ?f nil "8859-16")

(define-charset 'thai-tis620
  "TIS620.2533"
  :short-name "TIS620.2533"
  :iso-final-char ?T
  :emacs-mule-id 133
  :code-space [32 127]
  :code-offset #x0E00)

(define-charset 'tis620-2533
  "TIS620.2533"
  :short-name "TIS620.2533"
  :ascii-compatible-p t
  :code-space [0 255]
  :parents '(ascii eight-bit-control (thai-tis620 . -128)))

(define-charset 'jisx0201
  "JISX0201"
  :short-name "JISX0201"
  :long-name "JISX0201"
  :code-space [33 254]
  :map "jisx0201")

(define-charset 'latin-jisx0201
  "Roman Part of JISX0201.1976"
  :short-name "JISX0201 Roman"
  :long-name "Japanese Roman (JISX0201.1976)"
  :iso-final-char ?J
  :emacs-mule-id  138
  :code-space [33 126]
  :parents '(jisx0201))

(define-charset 'katakana-jisx0201
  "Katakana Part of JISX0201.1976"
  :short-name "JISX0201 Katakana"
  :long-name "Japanese Katakana (JISX0201.1976)"
  :iso-final-char ?I
  :emacs-mule-id  137
  :code-space [33 126]
  :parents '((jisx0201 . #x80)))

(define-charset 'chinese-gb2312
  "GB2312 Chinese simplified: ISO-IR-58"
  :short-name "GB2312"
  :long-name "GB2312: ISO-IR-58"
  :iso-final-char ?A
  :emacs-mule-id 145
  :code-space [33 126 33 126]
  :code-offset #x110000
  :unify-map "gb2312-1980")

(define-charset 'chinese-gbk
  "GBK Chinese simplified."
  :short-name "GBK"
  :long-name "GBK"
  :code-space [#x40 #xFE #x81 #xFE]
  :code-offset #x150000
  :unify-map "gbk")

(define-charset 'chinese-cns11643-1
  "CNS11643 Plane 1 Chinese traditional: ISO-IR-171"
  :short-name "CNS11643-1"
  :long-name "CNS11643-1 (Chinese traditional): ISO-IR-171"
  :iso-final-char ?G
  :emacs-mule-id  149
  :code-space [33 126 33 126]
  :code-offset #x114000
  :unify-map "cns11643-1")

(define-charset 'chinese-cns11643-2
  "CNS11643 Plane 2 Chinese traditional: ISO-IR-172"
  :short-name "CNS11643-2"
  :long-name "CNS11643-2 (Chinese traditional): ISO-IR-172"
  :iso-final-char ?H
  :emacs-mule-id  150
  :code-space [33 126 33 126]
  :code-offset #x118000
  :unify-map "cns11643-2")

(define-charset 'chinese-cns11643-3
  "CNS11643 Plane 3 Chinese Traditional: ISO-IR-183"
  :short-name  "CNS11643-3"
  :long-name "CNS11643-3 (Chinese traditional): ISO-IR-183"
  :iso-final-char ?I
  :code-space [33 126 33 126]
  :emacs-mule-id  246
  :code-offset #x11C000)

(define-charset 'chinese-cns11643-4
  "CNS11643 Plane 4 Chinese Traditional: ISO-IR-184"
  :short-name  "CNS11643-4"
  :long-name "CNS11643-4 (Chinese traditional): ISO-IR-184"
  :iso-final-char ?J
  :emacs-mule-id  247
  :code-space [33 126 33 126]
  :code-offset #x120000)

(define-charset 'chinese-cns11643-5
  "CNS11643 Plane 5 Chinese Traditional: ISO-IR-185"
  :short-name  "CNS11643-5"
  :long-name "CNS11643-5 (Chinese traditional): ISO-IR-185"
  :iso-final-char ?K
  :emacs-mule-id  248
  :code-space [33 126 33 126]
  :code-offset #x124000)

(define-charset 'chinese-cns11643-6
  "CNS11643 Plane 6 Chinese Traditional: ISO-IR-186"
  :short-name  "CNS11643-6"
  :long-name "CNS11643-6 (Chinese traditional): ISO-IR-186"
  :iso-final-char ?L
  :emacs-mule-id 249
  :code-space [33 126 33 126]
  :code-offset #x128000)

(define-charset 'chinese-cns11643-7
  "CNS11643 Plane 7 Chinese Traditional: ISO-IR-187"
  :short-name  "CNS11643-7"
  :long-name "CNS11643-7 (Chinese traditional): ISO-IR-187"
  :iso-final-char ?M
  :emacs-mule-id 250
  :code-space [33 126 33 126]
  :code-offset #x12C000)

(define-charset 'big5
  "Big5 (Chinese traditional)"
  :short-name "Big5"
  :long-name "Big5"
  :code-space [#x40 #xFE #xA1 #xFE]
  :code-offset #x130000
  :unify-map "big5")

(define-charset 'chinese-big5-1
  "Frequently used part (A141-C67E) of Big5 (Chinese traditional)"
  :short-name "Big5 (Level-1)"
  :long-name "Big5 (Level-1) A141-C67F"
  :iso-final-char ?0
  :emacs-mule-id 152
  :code-space [#x21 #x7E #x21 #x7E]
  :code-offset #x135000
  :unify-map "big5-1")

(define-charset 'chinese-big5-2
  "Less frequently used part (C940-FEFE) of Big5 (Chinese traditional)"
  :short-name "Big5 (Level-2)"
  :long-name "Big5 (Level-2) C940-FEFE"
  :iso-final-char ?1
  :emacs-mule-id  153
  :code-space [#x21 #x7E #x21 #x7E]
  :code-offset #x137800
  :unify-map "big5-2")

(define-charset 'japanese-jisx0208
  "JISX0208.1983/1990 Japanese Kanji: ISO-IR-87"
  :short-name "JISX0208"
  :long-name "JISX0208.1983/1990 (Japanese): ISO-IR-87"
  :iso-final-char ?B
  :emacs-mule-id 146
  :code-space [33 126 33 126]
  :code-offset #x140000
  :unify-map "jisx0208-1990")

(define-charset 'japanese-jisx0208-1978
  "JISX0208.1978 Japanese Kanji (so called \"old JIS\"): ISO-IR-42"
  :short-name "JISX0208.1978"
  :long-name  "JISX0208.1978 (Japanese): ISO-IR-42"
  :iso-final-char ?@
  :emacs-mule-id  144
  :code-space [33 126 33 126]
  :code-offset #x144000
  :unify-map "jisx0208-1978")

(define-charset 'japanese-jisx0212
  "JISX0212 Japanese supplement: ISO-IR-159"
  :short-name "JISX0212"
  :long-name "JISX0212 (Japanese): ISO-IR-159"
  :iso-final-char ?D
  :emacs-mule-id 148
  :code-space [33 126 33 126]
  :code-offset #x148000
  :unify-map "jisx0212-1990")

(define-charset 'japanese-jisx0213-1
  "JISX0213 Plane 1 (Japanese)"
  :short-name "JISX0213-1"
  :long-name "JISX0213-1"
  :iso-final-char ?O
  :emacs-mule-id  151
  :code-space [33 126 33 126]
  :code-offset #x14C000)

(define-charset 'japanese-jisx0213-2
  "JISX0213 Plane 2 (Japanese)"
  :short-name "JISX0213-2"
  :long-name "JISX0213-2"
  :iso-final-char ?P
  :emacs-mule-id 254
  :code-space [33 126 33 126]
  :code-offset #x150000)

(define-charset 'korean-ksc5601
  "KSC5601 Korean Hangul and Hanja: ISO-IR-149"
  :short-name "KSC5601"
  :long-name "KSC5601 (Korean): ISO-IR-149"
  :iso-final-char ?C
  :emacs-mule-id 147
  :code-space [33 126 33 126]
  :map "ksc5601-1987")

(define-charset 'chinese-sisheng
  "SiSheng characters for PinYin/ZhuYin"
  :short-name "SiSheng"
  :long-name "SiSheng (PinYin/ZhuYin)"
  :iso-final-char ?0
  :emacs-mule-id 160
  :code-space [33 126]
  :code-offset #x200000)

(define-charset 'ipa
  "IPA (International Phonetic Association)"
  :short-name "IPA"
  :long-name "IPA"
  :iso-final-char ?0
  :emacs-mule-id  161
  :code-space [32 127]
  :code-offset #x200080)

(define-charset 'viscii
  "VISCII1.1"
  :short-name "VISCII"
  :long-name "VISCII 1.1"
  :code-space [0 255]
  :map "viscii")

(define-charset 'vietnamese-viscii-lower
  "VISCII1.1 lower-case"
  :short-name "VISCII lower"
  :long-name "VISCII lower-case"
  :iso-final-char ?1
  :emacs-mule-id  162
  :code-space [32 127]
  :code-offset #x200200
  :unify-map "viscii-lower")

(define-charset 'vietnamese-viscii-upper
  "VISCII1.1 upper-case"
  :short-name "VISCII upper"
  :long-name "VISCII upper-case"
  :iso-final-char ?2
  :emacs-mule-id  163
  :code-space [32 127]
  :code-offset #x200280
  :unify-map "viscii-upper")

(define-charset 'vscii
  "VSCII1.1"
  :short-name "VSCII"
  :long-name "VSCII"
  :code-space [0 255]
  :map "vscii")

(define-charset 'koi8-r
  "KOI8-R"
  :short-name "KOI8-R"
  :long-name "KOI8-R"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "koi8-r")

(define-charset-alias 'koi8 'koi8-r)

(define-charset 'alternativnyj
  "ALTERNATIVNYJ"
  :short-name "alternativnyj"
  :long-name "alternativnyj"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "ibm866")

(define-charset 'koi8-u
  "KOI8-U"
  :short-name "KOI8-U"
  :long-name "KOI8-U"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "koi8-u")

(define-charset 'koi8-t
  "KOI8-T"
  :short-name "KOI8-T"
  :long-name "KOI8-T"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "koi8-t")

(define-charset 'georgian-ps
  "GEORGIAN-PS"
  :short-name "GEORGIAN-PS"
  :long-name "GEORGIAN-PS"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "georgian-ps")

(define-charset 'windows-1250
  "WINDOWS-1250 (Central Europe)"
  :short-name "WINDOWS-1250"
  :long-name "WINDOWS-1250"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "windows-1250")
(define-charset-alias 'cp1250 'windows-1250)

(define-charset 'windows-1251
  "WINDOWS-1251"
  :short-name "WINDOWS-1251"
  :long-name "WINDOWS-1251"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "windows-1251")
(define-charset-alias 'cp1251 'windows-1251)

(define-charset 'windows-1252
  "WINDOWS-1252 (Greek)"
  :short-name "WINDOWS-1252"
  :long-name "WINDOWS-1252"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "windows-1252")
(define-charset-alias 'cp1252 'windows-1252)

(define-charset 'windows-1253
  "WINDOWS-1253"
  :short-name "WINDOWS-1253"
  :long-name "WINDOWS-1253"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "windows-1253")
(define-charset-alias 'cp1253 'windows-1253)

(define-charset 'windows-1254
  "WINDOWS-1254"
  :short-name "WINDOWS-1254"
  :long-name "WINDOWS-1254"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "windows-1254")
(define-charset-alias 'cp1254 'windows-1254)

(define-charset 'windows-1255
  "WINDOWS-1255 (Hebrew)"
  :short-name "WINDOWS-1255"
  :long-name "WINDOWS-1255"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "windows-1255")
(define-charset-alias 'cp1255 'windows-1255)

(define-charset 'windows-1256
  "WINDOWS-1256 (Arabic)"
  :short-name "WINDOWS-1256"
  :long-name "WINDOWS-1256"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "windows-1256")
(define-charset-alias 'cp1256 'windows-1256)

(define-charset 'windows-1257
  "WINDOWS-1257 (Baltic)"
  :short-name "WINDOWS-1257"
  :long-name "WINDOWS-1257"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "windows-1257")
(define-charset-alias 'cp1257 'windows-1257)

(define-charset 'windows-1258
  "WINDOWS-1258"
  :short-name "WINDOWS-1258"
  :long-name "WINDOWS-1258"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "windows-1258")
(define-charset-alias 'cp1258 'windows-1258)

(define-charset 'next
  "NEXT"
  :short-name "NEXT"
  :long-name "NEXT"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "next")

(define-charset 'cp1125
  "CP1125"
  :short-name "CP1125"
  :long-name "CP1125"
  :code-space [0 255]
  :map "cp1125")
(define-charset-alias 'ruscii 'cp1125)
;; Original name for cp1125, says Serhii Hlodin <hlodin@lutsk.bank.gov.ua>
(define-charset-alias 'cp866u 'cp1125)

;; For Arabic, we need three different types of character sets.
;; Digits are of direction left-to-right and of width 1-column.
;; Others are of direction right-to-left and of width 1-column or
;; 2-column.
(define-charset 'arabic-digit
  "Arabic digit"
  :short-name "Arabic digit"
  :long-name "Arabic digit"
  :iso-final-char ?2
  :emacs-mule-id 164
  :code-space [34 42]
  :code-offset #x0600)

(define-charset 'arabic-1-column
  "Arabic 1-column"
  :short-name "Arabic 1-col"
  :long-name "Arabic 1-column"
  :iso-final-char ?3
  :emacs-mule-id 165
  :code-space [33 126]
  :code-offset #x200100)

(define-charset 'arabic-2-column
  "Arabic 2-column"
  :short-name "Arabic 2-col"
  :long-name "Arabic 2-column"
  :iso-final-char ?4
  :emacs-mule-id 224
  :code-space [33 126]
  :code-offset #x200180)

;; Lao script.
;; Codes 0x21..0x7E are mapped to Unicode U+0E81..U+0EDF.
(define-charset 'lao
  "Lao characters (ISO10646 0E81..0EDF)"
  :short-name "Lao"
  :long-name "Lao"
  :iso-final-char ?1
  :emacs-mule-id 167
  :code-space [33 126]
  :code-offset #x0E81)

(define-charset 'mule-lao
  "Lao characters (ISO10646 0E81..0EDF)"
  :short-name "Lao"
  :long-name "Lao"
  :code-space [0 255]
  :parents '(ascii eight-bit-control (lao . -128)))


;; Indian scripts.  Symbolic charset for data exchange.  Glyphs are
;; not assigned.  They are automatically converted to each Indian
;; script which IS-13194 supports.

(define-charset 'indian-is13194
  "Generic Indian charset for data exchange with IS 13194"
  :short-name "IS 13194"
  :long-name "Indian IS 13194"
  :iso-final-char ?5
  :emacs-mule-id 225
  :code-space [33 126]
  :code-offset #x180000)

(define-charset  'indian-glyph
  "Glyphs for Indian characters."
  :short-name "Indian glyph"
  :long-name "Indian glyph"
  :iso-final-char ?4
  :emacs-mule-id 240
  :code-space [32 127 32 127]
  :code-offset #x180100)

;; Actual Glyph for 1-column width.
(define-charset 'indian-1-column
  "Indian charset for 1-column width glyphs"
  :short-name "Indian 1-col"
  :long-name "Indian 1 Column"
  :iso-final-char ?6
  :emacs-mule-id  240
  :code-space [33 126 33 126]
  :code-offset #x184000)

;; Actual Glyph for 2-column width.
(define-charset 'indian-2-column
  "Indian charset for 2-column width glyphs"
  :short-name "Indian 2-col"
  :long-name "Indian 2 Column"
  :iso-final-char ?5
  :emacs-mule-id  251
  :code-space [33 126 33 126]
  :parents '(indian-1-column))

(define-charset 'tibetan
  "Tibetan characters"
  :iso-final-char ?7
  :short-name "Tibetan 2-col"
  :long-name "Tibetan 2 column"
  :iso-final-char ?7
  :emacs-mule-id 252
  :code-space [33 126 33 126]
  :code-offset #x190000)

(define-charset 'tibetan-1-column
  "Tibetan 1 column glyph"
  :short-name "Tibetan 1-col"
  :long-name "Tibetan 1 column"
  :iso-final-char ?8
  :emacs-mule-id 241
  :code-space [33 126 33 37]
  :parents '(tibetan))

;; Subsets of Unicode.
(define-charset 'mule-unicode-2500-33ff
  "Unicode characters of the range U+2500..U+33FF."
  :short-name "Unicode subset 2"
  :long-name "Unicode subset (U+2500..U+33FF)"
  :iso-final-char ?2
  :emacs-mule-id 242
  :code-space [#x20 #x7f #x20 #x47]
  :code-offset #x2500)

(define-charset 'mule-unicode-e000-ffff
  "Unicode characters of the range U+E000..U+FFFF."
  :short-name "Unicode subset 3"
  :long-name "Unicode subset (U+E000+FFFF)"
  :iso-final-char ?3
  :emacs-mule-id 243
  :code-space [#x20 #x7F #x20 #x75]
  :code-offset #xE000)

(define-charset 'mule-unicode-0100-24ff
  "Unicode characters of the range U+0100..U+24FF."
  :short-name "Unicode subset"
  :long-name "Unicode subset (U+0100..U+24FF)"
  :iso-final-char ?1
  :emacs-mule-id 244
  :code-space [#x20 #x7F #x20 #x7F]
  :code-offset #x100)

(define-charset 'ethiopic
  "Ethiopic characters for Amharic and Tigrigna."
  :short-name "Ethiopic"
  :long-name "Ethiopic characters"
  :iso-final-char ?3
  :emacs-mule-id  245
  :code-space [33 126 33 126]
  :code-offset #x1A0000)

(define-charset 'mac-roman
  "Mac Roman charset"
  :short-name "Mac Roman"
  :long-name "Mac Roman"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "mac-roman")

;; Fixme: modern EBCDIC variants, e.g. IBM00924?
(define-charset 'ebcdic-us
  "US version of EBCDIC"
  :short-name "EBCDIC-US"
  :long-name "EBCDIC-US"
  :code-space [0 255]
  :mime-charset 'ebcdic-us
  :map "ebcdic-us")

(define-charset 'ebcdic-uk
  "UK version of EBCDIC"
  :short-name "EBCDIC-UK"
  :long-name "EBCDIC-UK"
  :code-space [0 255]
  :mime-charset 'ebcdic-uk
  :map "ebcdic-uk")

(define-charset 'hp-roman8
  "Encoding used by Hewlet-Packard printer software"
  :short-name "HP-ROMAN8"
  :long-name "HP-ROMAN8"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "hp-roman8")

(define-charset 'adobe-standard-encoding
  "Adobe `standard encoding' used in PostScript"
  :short-name "ADOBE-STANDARD-ENCODING"
  :long-name "ADOBE-STANDARD-ENCODING"
  :code-space [#x20 255]
  :map "stdenc")

(define-charset 'symbol
  "Adobe symbol encoding used in PostScript"
  :short-name "ADOBE-SYMBOL"
  :long-name "ADOBE-SYMBOL"
  :code-space [#x20 255]
  :map "symbol")

(define-charset 'ibm850
  "DOS codepage 850"
  :short-name "IBM850"
  :long-name "IBM850"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "ibm850")
(define-charset-alias 'cp850 'ibm850)

(unify-charset 'chinese-gb2312)
(unify-charset 'chinese-gbk)
(unify-charset 'chinese-cns11643-1)
(unify-charset 'chinese-cns11643-2)
(unify-charset 'big5)
(unify-charset 'chinese-big5-1)
(unify-charset 'chinese-big5-2)
(unify-charset 'vietnamese-viscii-lower)
(unify-charset 'vietnamese-viscii-upper)


;; These are tables for translating characters on decoding and
;; encoding.
(setq standard-translation-table-for-decode nil)

(setq standard-translation-table-for-encode nil)

(defvar translation-table-for-input nil
  "If non-nil, a char table used to translate characters from input methods.
\(Currently only used by Quail.)")

;;; Make fundamental coding systems.

;; The coding system `no-conversion' is already defined in coding.c as
;; below:
;;
;; (define-coding-system 'no-conversion
;;   "Do no conversion."
;;   :coding-type 'raw-text
;;   :mnemonic ?=)

(define-coding-system 'raw-text
 "Raw text, which means text contains random 8-bit codes.
Encoding text with this coding system produces the actual byte
sequence of the text in buffers and strings.  An exception is made for
eight-bit-control characters.  Each of them is encoded into a single
byte.

When you visit a file with this coding, the file is read into a
unibyte buffer as is (except for EOL format), thus each byte of a file
is treated as a character."
 :coding-type 'raw-text
 :mnemonic ?t)

(define-coding-system 'undecided
  "No conversion on encoding, automatic conversion on decoding"
  :coding-type 'undecided
  :mnemonic ?-
  :charset-list '(ascii))

(define-coding-system-alias 'unix 'undecided-unix)
(define-coding-system-alias 'dos 'undecided-dos)
(define-coding-system-alias 'mac 'undecided-mac)

(define-coding-system 'iso-latin-1
  "ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)."
  :coding-type 'iso-2022
  :mnemonic ?1
  :charset-list '(ascii latin-iso8859-1)
  :designation [ascii latin-iso8859-1 nil nil]
  :mime-charset 'iso-8859-1)

(define-coding-system-alias 'iso-8859-1 'iso-latin-1)
(define-coding-system-alias 'latin-1 'iso-latin-1)

;; Coding systems not specific to each language environment.

(define-coding-system 'emacs-mule
 "Emacs 21 internal format used in buffer and string."
 :coding-type 'emacs-mule
 :charset-list 'emacs-mule
 :mnemonic ?M)

(define-coding-system 'utf-8
  "UTF-8."
  :coding-type 'utf-8
  :mnemonic ?U
  :charset-list '(unicode))

(define-coding-system-alias 'mule-utf-8 'utf-8)

(define-coding-system 'utf-8-emacs
  "Support for all Emacs characters (including non-Unicode characters)."
  :coding-type 'utf-8
  :mnemonic ?U
  :charset-list '(emacs)
  :mime-charset 'utf-8)

(define-coding-system 'utf-16
  "UTF-16"
  :coding-type 'utf-16
  :mnemonic ?U
  :charset-list '(unicode)
  :mime-charset 'utf-16)

(define-coding-system 'utf-16-le-nosig
  "UTF-16, little endian, no signature"
  :coding-type 'utf-16
  :mnemonic ?U
  :charset-list '(unicode)
  :endian 'little)

(define-coding-system 'utf-16-be-nosig
  "UTF-16, big endian, no signature"
  :coding-type 'utf-16
  :mnemonic ?U
  :charset-list '(unicode)
  :endian 'big)

(define-coding-system 'utf-16-le
  "UTF-16, little endian, with signature"
  :coding-type 'utf-16
  :mnemonic ?U
  :charset-list '(unicode)
  :bom t
  :endian 'little
  :mime-charset 'utf-16-le)

(define-coding-system 'utf-16-be
  "UTF-16, big endian, with signature"
  :coding-type 'utf-16
  :mnemonic ?U
  :charset-list '(unicode)
  :bom t
  :endian 'big
  :mime-charset 'utf-16-be)

(define-coding-system 'iso-2022-7bit
  "ISO 2022 based 7-bit encoding using only G0"
  :coding-type 'iso-2022
  :mnemonic ?J
  :charset-list 'iso-2022
  :designation [(ascii t) nil nil nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit designation composition))

(define-coding-system 'iso-2022-7bit-ss2
  "ISO 2022 based 7-bit encoding using SS2 for 96-charset"
  :coding-type 'iso-2022
  :mnemonic ?$
  :charset-list 'iso-2022
  :designation [(ascii 94) nil (nil 96) nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit
		 designation single-shift composition))

(define-coding-system 'iso-2022-7bit-lock
  "ISO-2022 coding system using Locking-Shift for 96-charset"
  :coding-type 'iso-2022
  :mnemonic ?&
  :charset-list 'iso-2022
  :designation [(ascii 94) (nil 96) nil nil]
  :flags '(ascii-at-eol ascii-at-cntl 7-bit
			designation locking-shift composition))

(define-coding-system-alias 'iso-2022-int-1 'iso-2022-7bit-lock)

(define-coding-system 'iso-2022-7bit-lock-ss2
  "Mixture of ISO-2022-JP, ISO-2022-KR, and ISO-2022-CN"
  :coding-type 'iso-2022
  :mnemonic ?i
  :charset-list '(ascii
		  japanese-jisx0208 japanese-jisx0208-1978 latin-jisx0201
		  korean-ksc5601
		  chinese-gb2312
		  chinese-cns11643-1 chinese-cns11643-2 chinese-cns11643-3
		  chinese-cns11643-4 chinese-cns11643-5 chinese-cns11643-6
		  chinese-cns11643-7)
  :designation [(ascii 94)
		(nil korean-ksc5601 chinese-gb2312 chinese-cns11643-1 96)
		(nil chinese-cns11643-2)
		(nil chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
		     chinese-cns11643-6 chinese-cns11643-7)]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit locking-shift
		 single-shift init-bol))

(define-coding-system-alias 'iso-2022-cjk 'iso-2022-7bit-lock-ss2)

(define-coding-system 'iso-2022-8bit-ss2
  "ISO 2022 based 8-bit encoding using SS2 for 96-charset"
  :coding-type 'iso-2022
  :mnemonic ?@
  :charset-list 'iso-2022
  :designation [(ascii 94) nil (nil 96) nil]
  :flags '(ascii-at-eol ascii-at-cntl designation single-shift composition))

(define-coding-system 'compound-text
  "Compound text based generic encoding for decoding unknown messages.

This coding system does not support ICCCM Extended Segments."
  :coding-type 'iso-2022
  :mnemonic ?x
  :charset-list 'iso-2022
  :designation [(ascii 94) (latin-iso8859-1 katakana-jisx0201 96) nil nil]
  :flags '(ascii-at-eol ascii-at-cntl
			designation locking-shift single-shift composition)
  ;; Fixme: this isn't a valid MIME charset and has to be
  ;; special-cased elsewhere  -- fx
  :mime-charset 'x-ctext)

(define-coding-system-alias  'x-ctext 'compound-text)
(define-coding-system-alias  'ctext 'compound-text)

;; Same as compound-text, but doesn't produce composition escape
;; sequences.  Used in post-read and pre-write conversions of
;; compound-text-with-extensions, see mule.el.  Note that this should
;; not have a mime-charset property, to prevent it from showing up
;; close to the beginning of coding systems ordered by priority.
(define-coding-system 'ctext-no-compositions 2 ?x
 "Compound text based generic encoding for decoding unknown messages.

Like `compound-text', but does not produce escape sequences for compositions."
  :coding-type 'iso-2022
  :mnemonic ?x
  :charset-list 'iso-2022
  :designation [(ascii 94) (latin-iso8859-1 katakana-jisx0201 96) nil nil]
  :flags '(ascii-at-eol ascii-at-cntl
			designation locking-shift single-shift))

(define-coding-system 'compound-text-with-extensions
 "Compound text encoding with ICCCM Extended Segment extensions.

This coding system should be used only for X selections.  It is inappropriate
for decoding and encoding files, process I/O, etc."
  :coding-type 'raw-text
  :mnemonic ?x
  :post-read-conversion 'ctext-post-read-conversion
  :pre-write-conversion 'ctext-pre-write-conversion)

(define-coding-system-alias
  'x-ctext-with-extensions 'compound-text-with-extensions)
(define-coding-system-alias
  'ctext-with-extensions 'compound-text-with-extensions)

(define-coding-system 'us-ascii
  "Convert all characters but ASCII to `?'."
  :coding-type 'charset
  :mnemonic ?-
  :charset-list '(ascii)
  :default-char ??
  :mime-charset 'us-ascii)

(define-coding-system-alias 'iso-safe 'us-ascii)

;; Use us-ascii for terminal output if some other coding system is not
;; specified explicitly.
(set-safe-terminal-coding-system-internal 'us-ascii)

;; The other coding-systems are defined in each language specific
;; files under lisp/language.

;; Normally, set coding system to `undecided' before reading a file.
;; Compiled Emacs Lisp files (*.elc) are not decoded at all,
;; but we regard them as containing multibyte characters.
;; Tar files are not decoded at all, but we treat them as raw bytes.

(setq file-coding-system-alist
      '(("\\.elc\\'" . (emacs-mule . emacs-mule))
	("\\.utf\\(-8\\)?\\'" . utf-8)
	;; This is the defined default for XML documents.  It may be
	;; overridden by a charset specification in the header.  That
	;; should be grokked by the auto-coding mechanism, but rms
	;; vetoed that.  -- fx
	("\\.xml\\'" . utf-8)
	;; We use raw-text for reading loaddefs.el so that if it
	;; happens to have DOS or Mac EOLs, they are converted to
	;; newlines.  This is required to make the special treatment
	;; of the "\ newline" combination in loaddefs.el, which marks
	;; the beginning of a doc string, work.
	("\\(\\`\\|/\\)loaddefs.el\\'" . (raw-text . raw-text-unix))
	("\\.tar\\'" . (no-conversion . no-conversion))
	("" . (undecided . nil))))


;;; Setting coding categories and their priorities.

;; This setting is just to read an Emacs Lisp source files which
;; contain multilingual text while dumping Emacs.  More appropriate
;; values are set by the command `set-language-environment' for each
;; language environment.

(set-coding-system-priority
 'iso-latin-1
 'utf-8
 'iso-2022-7bit
 )


;;; Miscellaneous settings.

;; Make all multibyte characters self-insert.
(set-char-table-range (nth 1 global-map)
		      (cons (decode-char 'ucs 128) (max-char))
		      'self-insert-command)

(aset latin-extra-code-table ?\222 t)

;; The old code-pages library is obsoleted by coding systems based on
;; the charsets defined in this file but might be required by user
;; code.
(provide 'code-pages)

;; Local variables:
;; no-byte-compile: t
;; End:

;;; mule-conf.el ends here
