;;; european.el --- support for European languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995, 1997, 2001 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Keywords: multilingual, European

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

;; For European scripts, character sets ISO8859-1,2,3,4,9,14,15 are
;; supported.

;;; Code:

;; Latin-1 (ISO-8859-1)

(make-coding-system
 'iso-latin-1 2 ?1
 "ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)."
 '(ascii latin-iso8859-1 nil nil
   nil nil nil nil nil nil nil nil nil nil nil nil t)
 '((safe-charsets ascii latin-iso8859-1)
   (mime-charset . iso-8859-1)))

(define-coding-system-alias 'iso-8859-1 'iso-latin-1)
(define-coding-system-alias 'latin-1 'iso-latin-1)

(set-language-info-alist
 "Latin-1" '((charset ascii latin-iso8859-1)
	     (coding-system iso-latin-1)
	     (coding-priority iso-latin-1)
	     (nonascii-translation . latin-iso8859-1)
	     (unibyte-syntax . "latin-1")
	     (unibyte-display . iso-latin-1)
	     (input-method . "latin-1-prefix")
	     (sample-text
	      . "Hello, Hej, Tere, Hei, Bonjour, Gr,A|_(B Gott, Ciao, ,A!(BHola!")
	     (documentation . "\
This language environment is a generic one for the Latin-1 (ISO-8859-1)
character set which supports the following European languages:
 Albanian, Basque, Breton, Catalan, Danish, Dutch, English, Faeroese,
 Finnish, French (with restrictions -- see Latin-9), Frisian, Galician,
 German, Greenlandic, Icelandic, Irish Gaelic (new orthography),
 Italian, Latin, Luxemburgish, Norwegian, Portuguese, Rhaeto-Romanic,
 Scottish Gaelic, Spanish, and Swedish.
We also have specific language environments for the following languages:
  For Dutch, \"Dutch\".
  For German, \"German\".
  For Spanish, \"Spanish\".
  For French, \"French\".

Latin-1 also covers several written languages outside Europe, including
Indonesian/Malay, Tagalog (Philippines), Swahili and Afrikaans."))
 '("European"))


;; Latin-2 (ISO-8859-2)

(make-coding-system
 'iso-latin-2 2 ?2
 "ISO 2022 based 8-bit encoding for Latin-2 (MIME:ISO-8859-2)."
 '(ascii latin-iso8859-2 nil nil
   nil nil nil nil nil nil nil)
 '((safe-charsets ascii latin-iso8859-2)
   (mime-charset . iso-8859-2)))

(define-coding-system-alias 'iso-8859-2 'iso-latin-2)
(define-coding-system-alias 'latin-2 'iso-latin-2)

(set-language-info-alist
 "Latin-2" '((charset ascii latin-iso8859-2)
	     (coding-system iso-latin-2)
	     (coding-priority iso-latin-2)
	     (nonascii-translation . latin-iso8859-2)
	     (unibyte-syntax . "latin-2")
	     (unibyte-display . iso-latin-2)
	     (input-method . "latin-2-prefix")
	     (documentation . "\
This language environment is a generic one for the Latin-2 (ISO-8859-2)
character set which supports the following languages:
 Albanian, Czech, English, German, Hungarian, Polish, Romanian,
 Serbo-Croatian or Croatian, Slovak, Slovene, Sorbian (upper and lower),
 and Swedish.
We also have specific language environments for the following languages:
  For Czech, \"Czech\".
  For Romanian, \"Romanian\".
  For Slovak, \"Slovak\"."))
 '("European"))


;; Latin-3 (ISO-8859-3)

(make-coding-system
 'iso-latin-3 2 ?3
 "ISO 2022 based 8-bit encoding for Latin-3 (MIME:ISO-8859-3)."
 '(ascii latin-iso8859-3 nil nil
   nil nil nil nil nil nil nil)
 '((safe-charsets ascii latin-iso8859-3)
   (mime-charset . iso-8859-3)))

(define-coding-system-alias 'iso-8859-3 'iso-latin-3)
(define-coding-system-alias 'latin-3 'iso-latin-3)

(set-language-info-alist
 "Latin-3" '((charset ascii latin-iso8859-3)
	     (coding-system iso-latin-3)
	     (coding-priority iso-latin-3)
	     (nonascii-translation . latin-iso8859-3)
	     (unibyte-syntax . "latin-3")
	     (unibyte-display . iso-latin-3)
	     (input-method . "latin-3-prefix")
	     (documentation . "\
These languages are supported with the Latin-3 (ISO-8859-3) character set:
 Afrikaans, Catalan, Dutch, English, Esperanto, French, Galician,
 German, Italian, Maltese, Spanish, and Turkish."))
 '("European"))


;; Latin-4 (ISO-8859-4)

(make-coding-system
 'iso-latin-4 2 ?4
 "ISO 2022 based 8-bit encoding for Latin-4 (MIME:ISO-8859-4)."
 '(ascii latin-iso8859-4 nil nil
   nil nil nil nil nil nil nil)
 '((safe-charsets ascii latin-iso8859-4)
   (mime-charset . iso-8859-4)))

(define-coding-system-alias 'iso-8859-4 'iso-latin-4)
(define-coding-system-alias 'latin-4 'iso-latin-4)

(set-language-info-alist
 "Latin-4" '((charset ascii latin-iso8859-4)
	     (coding-system iso-8859-4)
	     (coding-priority iso-8859-4)
	     (nonascii-translation . latin-iso8859-4)
	     (unibyte-syntax . "latin-4")
	     (unibyte-display . iso-8859-4)
	     (input-method . "latin-4-postfix")
	     (documentation . "\
These languages are supported with the Latin-4 (ISO-8859-4) character set:
 Danish, English, Estonian, Finnish, German, Greenlandic, Lappish,
 Latvian, Lithuanian, and Norwegian."))
 '("European"))


;; Latin-5 (ISO-8859-9)

(make-coding-system
 'iso-latin-5 2 ?9
 "ISO 2022 based 8-bit encoding for Latin-5 (MIME:ISO-8859-9)."
 '(ascii latin-iso8859-9 nil nil
   nil nil nil nil nil nil nil)
 '((safe-charsets ascii latin-iso8859-9)
   (mime-charset . iso-8859-9)))

(define-coding-system-alias 'iso-8859-9 'iso-latin-5)
(define-coding-system-alias 'latin-5 'iso-latin-5)

(set-language-info-alist
 "Latin-5" '((charset ascii latin-iso8859-9)
	     (coding-system iso-latin-5)
	     (coding-priority iso-latin-5)
	     (nonascii-translation . latin-iso8859-9)
	     (unibyte-syntax . "latin-5")
	     (unibyte-display . iso-latin-5)
	     (input-method . "latin-5-postfix")
	     (documentation . "Support for Turkish language."))
 '("European"))


;; Latin-8 (ISO-8859-14)

(make-coding-system
 'iso-latin-8 2 ?W			; `W' for `Welsh', since `C'
					; for `Celtic' is taken.
 "ISO 2022 based 8-bit encoding for Latin-8 (MIME:ISO-8859-14)."
 '(ascii latin-iso8859-14 nil nil
   nil nil nil nil nil nil nil nil nil nil nil nil t)
 '((safe-charsets ascii latin-iso8859-14)
   (mime-charset . iso-8859-14)))

(define-coding-system-alias 'iso-8859-14 'iso-latin-8)
(define-coding-system-alias 'latin-8 'iso-latin-8)

(set-language-info-alist
 "Latin-8" '((charset ascii latin-iso8859-14)
	     (coding-system iso-latin-8)
	     (coding-priority iso-latin-8)
	     (nonascii-translation . latin-iso8859-14)
	     (unibyte-syntax . "latin-8")
	     (unibyte-display . iso-latin-8)
	     (input-method . "latin-8-prefix")
	     ;; Fixme: Welsh/Ga{e}lic greetings
	     (sample-text . ",_"(B ,_p(B ,_^(B")
	     (documentation . "\
This language environment is a generic one for the Latin-8 (ISO-8859-14)
character set which supports the Celtic languages, including those not
covered by other ISO-8859 character sets:
 Welsh, Manx Gaelic and Irish Gaelic (old orthography)."))
 '("European"))

;; Latin-9 (ISO-8859-15)

(make-coding-system
 'iso-latin-9 2 ?0			; `0' for `Latin-0'
 "ISO 2022 based 8-bit encoding for Latin-9 (MIME:ISO-8859-15)."
 '(ascii latin-iso8859-15 nil nil
   nil nil nil nil nil nil nil nil nil nil nil nil t)
 '((safe-charsets ascii latin-iso8859-15)
   (mime-charset . iso-8859-15)))

(define-coding-system-alias 'iso-8859-15 'iso-latin-9)
(define-coding-system-alias 'latin-9 'iso-latin-9)
(define-coding-system-alias 'latin-0 'iso-latin-9)

(set-language-info-alist
 "Latin-9" '((charset ascii latin-iso8859-15)
	     (coding-system iso-latin-9)
	     (coding-priority iso-latin-9)
	     (nonascii-translation . latin-iso8859-15)
	     (unibyte-syntax . "latin-9")
	     (unibyte-display . iso-latin-9)
	     (input-method . "latin-9-prefix")
	     (sample-text
	      . "AVE. ,b&(48<=>(B ,b$(B")
	     (documentation . "\
This language environment is a generic one for the Latin-9 (ISO-8859-15)
character set which supports the same languages as Latin-1 with the
addition of the Euro sign and some additional French and Finnish letters.
Latin-9 is sometimes nicknamed `Latin-0'."))
 '("European"))

(set-language-info-alist
 "German" '((tutorial . "TUTORIAL.de")
	    (charset ascii latin-iso8859-1)
	    (coding-system iso-latin-1)
	    (coding-priority iso-latin-1)
	    (input-method . "german-postfix")
	    (nonascii-translation . iso-latin-1)
	    (unibyte-syntax . "latin-1")
	    (unibyte-display . iso-latin-1)
	    (sample-text . "\
German (Deutsch Nord)	Guten Tag
German (Deutsch S,A|(Bd)	Gr,A|_(B Gott")
	    (documentation . "\
This language environment is almost the same as Latin-1,
but sets the default input method to \"german-postfix\".
Additionally, it selects the German tutorial."))
 '("European"))

(set-language-info-alist
 "French" '((tutorial . "TUTORIAL.fr")
	    (charset ascii latin-iso8859-1)
	    (coding-system iso-latin-1)
	    (coding-priority iso-latin-1)
	    (nonascii-translation . latin-iso8859-1)
	    (unibyte-syntax . "latin-1")
	    (unibyte-display . iso-latin-1)
	    (input-method . "latin-1-prefix")
	    (sample-text . "French (Fran,Ag(Bais)	Bonjour, Salut")
	    (documentation . "\
This language environment is almost the same as Latin-1,
but it selects the French tutorial."))
 '("European"))

(set-language-info-alist
 "Slovenian" '((charset . (ascii latin-iso8859-2))
	      (coding-system . (iso-8859-2))
	      (coding-priority . (iso-8859-2))
	      (nonascii-translation . latin-iso8859-2)
	      (input-method . "latin-2-postfix")
	      (unibyte-syntax . "latin-2")
	      (unibyte-display . iso-8859-2)
	      (tutorial . "TUTORIAL.sl")
	      (sample-text . ",B.(Belimo vam uspe,B9(Ben dan!")
	      (documentation . t))
 '("European"))

(set-language-info-alist
 "Spanish" '((tutorial . "TUTORIAL.es")
	    (charset ascii latin-iso8859-1)
	    (coding-system iso-latin-1)
	    (coding-priority iso-latin-1)
	    (input-method . "spanish-postfix")
	    (nonascii-translation . iso-latin-1)
	    (unibyte-syntax . "latin-1")
	    (unibyte-display . iso-latin-1)
	    (sample-text . "Spanish (Espa,Aq(Bol)	,A!(BHola!")
	    (documentation . "\
This language environment is almost the same as Latin-1,
but it sets the default input method to \"spanish-postfix\",
and it selects the Spanish tutorial."))
 '("European"))

(set-language-info-alist
 "Dutch" '((tutorial . "TUTORIAL.nl")
	   (charset ascii latin-iso8859-1)
	   (coding-system iso-latin-1)
	   (coding-priority iso-latin-1)
	   (nonascii-translation . iso-latin-1)
	   (unibyte-syntax . "latin-1")
	   (unibyte-display . iso-latin-1)
	   (sample-text . "Er is een aantal manieren waarop je dit kan doen")
	   (documentation . "\
This language environment is almost the same as Latin-1,
but it selects the Dutch tutorial."))
 '("European"))

;; For Turkish, the character set ISO-8859-9 (Latin-5) is used.  But,
;; before the introduction of ISO-8859-9 in 1988, ISO-8859-3 (Latin-3)
;; was used for Turkish.  Those who use Latin-3 for Turkish should use
;; "Latin-3" language environment.

(set-language-info-alist
 "Turkish" '((charset ascii latin-iso8859-9)
	     (coding-system iso-latin-5)
	     (coding-priority iso-latin-5)
	     (nonascii-translation . latin-iso8859-9)
	     (unibyte-syntax . "latin-5")
	     (unibyte-display . iso-latin-5)
	     (input-method . "turkish-postfix")
	     (sample-text . "Turkish (T,M|(Brk,Mg(Be)	Merhaba")
	     (documentation . t)))

;; Polish ISO 8859-2 environment.
;; Maintainer: Wlodek Bzyl <matwb@univ.gda.pl>
;; Keywords: multilingual, Polish

(set-language-info-alist
 "Polish" '((charset . (ascii latin-iso8859-2))
	   (coding-system . (iso-8859-2))
	   (coding-priority . (iso-8859-2))
	   (input-method . "polish-slash")
	   (nonascii-translation . latin-iso8859-2)
	   (unibyte-syntax . "latin-2")
	   (unibyte-display . iso-8859-2)
	   (tutorial . "TUTORIAL.pl")
	   (sample-text . "P,Bs(Bjd,B<(B, ki,Bq(B-,B?(Be t,Bj(B chmurno,B6f(B w g,B31(Bb flaszy")
	   (documentation . t))
 '("European"))

(set-language-info-alist
 "Welsh" `((coding-system utf-8 latin-8) ; the input method is Unicode-based
	   (coding-priority utf-8 latin-8)
	   (input-method . "welsh")
	   (documentation . "Support for Welsh, using Unicode."))
 '("European"))

(set-language-info-alist
 "Latin-7" `((coding-system latin-7)
	     (coding-priority latin-7)
	     ;; Fixme: input-method
	     (features code-pages)
	     (documentation . "Support for Latin-7, e.g. Latvian, Lithuanian."))
 '("European"))

(set-language-info-alist
 "Lithuanian" `((coding-system latin-7)
		(coding-priority latin-7)
		(input-method . "lithuanian-keyboard")
		(features code-pages)
		(documentation . "Support for Lithuanian."))
 '("European"))

(set-language-info-alist
 "Latvian" `((coding-system latin-7)
	     (coding-priority latin-7)
	     (input-method . "latvian-keyboard")
	     (features code-pages)
	     (documentation . "Support for Latvian."))
 '("European"))

;; Definitions for the Mac Roman character sets and coding system.
;; The Mac Roman encoding uses all 128 code points in the range 128 to
;; 255 for actual characters.  Emacs decodes them to one of the
;; following character sets.
;;	ascii, latin-iso8859-1, mule-unicode-0100-24ff,
;;	mule-unicode-2500-33ff, mule-unicode-e000-ffff

(let
    ((encoding-vector (make-vector 256 nil))
     (i 0)
     (vec	;; mac-roman (128..255) -> UCS mapping
      [ #x00C4	;; 128:LATIN CAPITAL LETTER A WITH DIAERESIS
	#x00C5	;; 129:LATIN CAPITAL LETTER A WITH RING ABOVE
	#x00C7	;; 130:LATIN CAPITAL LETTER C WITH CEDILLA
	#x00C9	;; 131:LATIN CAPITAL LETTER E WITH ACUTE
	#x00D1	;; 132:LATIN CAPITAL LETTER N WITH TILDE
	#x00D6	;; 133:LATIN CAPITAL LETTER O WITH DIAERESIS
	#x00DC	;; 134:LATIN CAPITAL LETTER U WITH DIAERESIS
	#x00E1	;; 135:LATIN SMALL LETTER A WITH ACUTE
	#x00E0	;; 136:LATIN SMALL LETTER A WITH GRAVE
	#x00E2	;; 137:LATIN SMALL LETTER A WITH CIRCUMFLEX
	#x00E4	;; 138:LATIN SMALL LETTER A WITH DIAERESIS
	#x00E3	;; 139:LATIN SMALL LETTER A WITH TILDE
	#x00E5	;; 140:LATIN SMALL LETTER A WITH RING ABOVE
	#x00E7	;; 141:LATIN SMALL LETTER C WITH CEDILLA
	#x00E9	;; 142:LATIN SMALL LETTER E WITH ACUTE
	#x00E8	;; 143:LATIN SMALL LETTER E WITH GRAVE
	#x00EA	;; 144:LATIN SMALL LETTER E WITH CIRCUMFLEX
	#x00EB	;; 145:LATIN SMALL LETTER E WITH DIAERESIS
	#x00ED	;; 146:LATIN SMALL LETTER I WITH ACUTE
	#x00EC	;; 147:LATIN SMALL LETTER I WITH GRAVE
	#x00EE	;; 148:LATIN SMALL LETTER I WITH CIRCUMFLEX
	#x00EF	;; 149:LATIN SMALL LETTER I WITH DIAERESIS
	#x00F1	;; 150:LATIN SMALL LETTER N WITH TILDE
	#x00F3	;; 151:LATIN SMALL LETTER O WITH ACUTE
	#x00F2	;; 152:LATIN SMALL LETTER O WITH GRAVE
	#x00F4	;; 153:LATIN SMALL LETTER O WITH CIRCUMFLEX
	#x00F6	;; 154:LATIN SMALL LETTER O WITH DIAERESIS
	#x00F5	;; 155:LATIN SMALL LETTER O WITH TILDE
	#x00FA	;; 156:LATIN SMALL LETTER U WITH ACUTE
	#x00F9	;; 157:LATIN SMALL LETTER U WITH GRAVE
	#x00FB	;; 158:LATIN SMALL LETTER U WITH CIRCUMFLEX
	#x00FC	;; 159:LATIN SMALL LETTER U WITH DIAERESIS
	#x2020	;; 160:DAGGER
	#x00B0	;; 161:DEGREE SIGN
	#x00A2	;; 162:CENT SIGN
	#x00A3	;; 163:POUND SIGN
	#x00A7	;; 164:SECTION SIGN
	#x2022	;; 165:BULLET
	#x00B6	;; 166:PILCROW SIGN
	#x00DF	;; 167:LATIN SMALL LETTER SHARP S
	#x00AE	;; 168:REGISTERED SIGN
	#x00A9	;; 169:COPYRIGHT SIGN
	#x2122	;; 170:TRADE MARK SIGN
	#x00B4	;; 171:ACUTE ACCENT
	#x00A8	;; 172:DIAERESIS
	#x2260	;; 173:NOT EQUAL TO
	#x00C6	;; 174:LATIN CAPITAL LETTER AE
	#x00D8	;; 175:LATIN CAPITAL LETTER O WITH STROKE
	#x221E	;; 176:INFINITY
	#x00B1	;; 177:PLUS-MINUS SIGN
	#x2264	;; 178:LESS-THAN OR EQUAL TO
	#x2265	;; 179:GREATER-THAN OR EQUAL TO
	#x00A5	;; 180:YEN SIGN
	#x00B5	;; 181:MICRO SIGN
	#x2202	;; 182:PARTIAL DIFFERENTIAL
	#x2211	;; 183:N-ARY SUMMATION
	#x220F	;; 184:N-ARY PRODUCT
	#x03C0	;; 185:GREEK SMALL LETTER PI
	#x222B	;; 186:INTEGRAL
	#x00AA	;; 187:FEMININE ORDINAL INDICATOR
	#x00BA	;; 188:MASCULINE ORDINAL INDICATOR
	#x03A9	;; 189:GREEK CAPITAL LETTER OMEGA
	#x00E6	;; 190:LATIN SMALL LETTER AE
	#x00F8	;; 191:LATIN SMALL LETTER O WITH STROKE
	#x00BF	;; 192:INVERTED QUESTION MARK
	#x00A1	;; 193:INVERTED EXCLAMATION MARK
	#x00AC	;; 194:NOT SIGN
	#x221A	;; 195:SQUARE ROOT
	#x0192	;; 196:LATIN SMALL LETTER F WITH HOOK
	#x2248	;; 197:ALMOST EQUAL TO
	#x2206	;; 198:INCREMENT
	#x00AB	;; 199:LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
	#x00BB	;; 200:RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
	#x2026	;; 201:HORIZONTAL ELLIPSIS
	#x00A0	;; 202:NO-BREAK SPACE
	#x00C0	;; 203:LATIN CAPITAL LETTER A WITH GRAVE
	#x00C3	;; 204:LATIN CAPITAL LETTER A WITH TILDE
	#x00D5	;; 205:LATIN CAPITAL LETTER O WITH TILDE
	#x0152	;; 206:LATIN CAPITAL LIGATURE OE
	#x0153	;; 207:LATIN SMALL LIGATURE OE
	#x2013	;; 208:EN DASH
	#x2014	;; 209:EM DASH
	#x201C	;; 210:LEFT DOUBLE QUOTATION MARK
	#x201D	;; 211:RIGHT DOUBLE QUOTATION MARK
	#x2018	;; 212:LEFT SINGLE QUOTATION MARK
	#x2019	;; 213:RIGHT SINGLE QUOTATION MARK
	#x00F7	;; 214:DIVISION SIGN
	#x25CA	;; 215:LOZENGE
	#x00FF	;; 216:LATIN SMALL LETTER Y WITH DIAERESIS
	#x0178	;; 217:LATIN CAPITAL LETTER Y WITH DIAERESIS
	#x2044	;; 218:FRACTION SLASH
	#x20AC	;; 219:EURO SIGN
	#x2039	;; 220:SINGLE LEFT-POINTING ANGLE QUOTATION MARK
	#x203A	;; 221:SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
	#xFB01	;; 222:LATIN SMALL LIGATURE FI
	#xFB02	;; 223:LATIN SMALL LIGATURE FL
	#x2021	;; 224:DOUBLE DAGGER
	#x00B7	;; 225:MIDDLE DOT
	#x201A	;; 226:SINGLE LOW-9 QUOTATION MARK
	#x201E	;; 227:DOUBLE LOW-9 QUOTATION MARK
	#x2030	;; 228:PER MILLE SIGN
	#x00C2	;; 229:LATIN CAPITAL LETTER A WITH CIRCUMFLEX
	#x00CA	;; 230:LATIN CAPITAL LETTER E WITH CIRCUMFLEX
	#x00C1	;; 231:LATIN CAPITAL LETTER A WITH ACUTE
	#x00CB	;; 232:LATIN CAPITAL LETTER E WITH DIAERESIS
	#x00C8	;; 233:LATIN CAPITAL LETTER E WITH GRAVE
	#x00CD	;; 234:LATIN CAPITAL LETTER I WITH ACUTE
	#x00CE	;; 235:LATIN CAPITAL LETTER I WITH CIRCUMFLEX
	#x00CF	;; 236:LATIN CAPITAL LETTER I WITH DIAERESIS
	#x00CC	;; 237:LATIN CAPITAL LETTER I WITH GRAVE
	#x00D3	;; 238:LATIN CAPITAL LETTER O WITH ACUTE
	#x00D4	;; 239:LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	#xF8FF	;; 240:Apple logo
	#x00D2	;; 241:LATIN CAPITAL LETTER O WITH GRAVE
	#x00DA	;; 242:LATIN CAPITAL LETTER U WITH ACUTE
	#x00DB	;; 243:LATIN CAPITAL LETTER U WITH CIRCUMFLEX
	#x00D9	;; 244:LATIN CAPITAL LETTER U WITH GRAVE
	#x0131	;; 245:LATIN SMALL LETTER DOTLESS I
	#x02C6	;; 246:MODIFIER LETTER CIRCUMFLEX ACCENT
	#x02DC	;; 247:SMALL TILDE
	#x00AF	;; 248:MACRON
	#x02D8	;; 249:BREVE
	#x02D9	;; 250:DOT ABOVE
	#x02DA	;; 251:RING ABOVE
	#x00B8	;; 252:CEDILLA
	#x02DD	;; 253:DOUBLE ACUTE ACCENT
	#x02DB	;; 254:OGONEK
	#x02C7	;; 255:CARON
	])
     translation-table)
  (while (< i 128)
    (aset encoding-vector i i)
    (setq i (1+ i)))
  (while (< i 256)
    (aset encoding-vector i
	  (decode-char 'ucs (aref vec (- i 128))))
    (setq i (1+ i)))
  (setq translation-table
	(make-translation-table-from-vector encoding-vector))
  (define-translation-table 'mac-roman-decoder translation-table)
  (define-translation-table 'mac-roman-encoder 
    (char-table-extra-slot translation-table 0)))

(define-ccl-program decode-mac-roman
  `(4
    ((loop
      (read r1)
      (if (r1 < 128)  ;; ASCII
	  (r0 = ,(charset-id 'ascii))
	(if (r1 < 160)
	    (r0 = ,(charset-id 'eight-bit-control))
	  (r0 = ,(charset-id 'eight-bit-graphic))))
      (translate-character mac-roman-decoder r0 r1)
      (write-multibyte-character r0 r1)
      (repeat))))
  "CCL program to decode Mac Roman")

(define-ccl-program encode-mac-roman
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (translate-character mac-roman-encoder r0 r1)
      (write-repeat r1))))
  "CCL program to encode Mac Roman")

(make-coding-system
 'mac-roman 4 ?M
 "Mac Roman Encoding (MIME:MACINTOSH)."
 '(decode-mac-roman . encode-mac-roman)
 '((safe-chars . (get 'mac-roman-encoder 'translation-table))
   (valid-codes (0 . 255))
   (mime-charset . macintosh)))		; per IANA, rfc1345

(defconst diacritic-composition-pattern "\\C^\\c^+")

(defun diacritic-compose-region (beg end)
  "Compose diacritic characters in the region.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (re-search-forward diacritic-composition-pattern nil t)
      (compose-region (match-beginning 0) (match-end 0)))))

(defun diacritic-compose-string (string)
  "Compose diacritic characters in STRING and return the resulting string."
  (let ((idx 0))
    (while (setq idx (string-match diacritic-composition-pattern string idx))
      (compose-string string idx (match-end 0))
      (setq idx (match-end 0))))
  string)
      
(defun diacritic-compose-buffer ()
  "Compose diacritic characters in the current buffer."
  (interactive)
  (diacritic-compose-region (point-min) (point-max)))

(defun diacritic-post-read-conversion (len)
  (diacritic-compose-region (point) (+ (point) len))
  len)

(defun diacritic-composition-function (from to pattern &optional string)
  "Compose diacritic text in the region FROM and TO.
The text matches the regular expression PATTERN.
Optional 4th argument STRING, if non-nil, is a string containing text
to compose.

The return value is number of composed characters."
  (if (< (1+ from) to)
      (prog1 (- to from)
	(if string
	    (compose-string string from to)
	  (compose-region from to))
	(- to from))))

;; Register a function to compose Unicode diacrtics and marks.
(let ((patterns '(("\\C^\\c^+" . diacritic-composition-function))))
  (let ((c #x300))
    (while (<= c #x362)
      (aset composition-function-table (decode-char 'ucs c) patterns)
      (setq c (1+ c)))
    (setq c #x20d0)
    (while (<= c #x20e3)
      (aset composition-function-table (decode-char 'ucs c) patterns)
      (setq c (1+ c)))))

(provide 'european)

;;; european.el ends here
