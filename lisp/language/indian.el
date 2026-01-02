;;; indian.el --- Indian languages support -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 1997, 1999, 2001-2026 Free Software Foundation, Inc.
;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: 	multilingual, i18n, Indian

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains definitions of Indian language environments, and
;; setups for displaying the scripts used there.

;;; Code:

(define-coding-system 'in-is13194-devanagari
  "8-bit encoding for ASCII (MSB=0) and IS13194-Devanagari (MSB=1)."
  :coding-type 'iso-2022
  :mnemonic ?D
  :designation [ascii indian-is13194 nil nil]
  :charset-list '(ascii indian-is13194)
  :post-read-conversion 'in-is13194-post-read-conversion
  :pre-write-conversion 'in-is13194-pre-write-conversion)

(define-coding-system-alias 'devanagari 'in-is13194-devanagari)

(set-language-info-alist
 "Devanagari" '((charset unicode)
		(coding-system utf-8)
		(coding-priority utf-8)
		(input-method . "devanagari-aiba")
                (sample-text . "Devanagari (देवनागरी)	नमस्ते / नमस्कार")
		(documentation . "\
Such languages using Devanagari script as Hindi, Marathi and Nepali
are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Bengali" '((charset unicode)
	     (coding-system utf-8)
	     (coding-priority utf-8)
	     (input-method . "bengali-itrans")
             (sample-text . "Bengali (বাংলা)	নমস্কার")
	     (documentation . "\
Such languages using Bengali script as Bengali and Assamese
are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Gurmukhi" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)
	      (input-method . "punjabi-itrans")
              (sample-text . "Gurmukhi (ਗੁਰਮੁਖੀ)	ਸਤ ਸ੍ਰੀ ਅਕਾਲ")
	      (documentation . "\
North Indian language Punjabi is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Gujarati" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)
	      (input-method . "gujarati-itrans")
              (sample-text . "Gujarati (ગુજરાતી)	નમસ્તે")
	      (documentation . "\
North Indian language Gujarati is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Odia" '((charset unicode)
	  (coding-system utf-8)
	  (coding-priority utf-8)
	  (input-method . "odia")
          (sample-text . "Odia (ଓଡ଼ିଆ)	ନମସ୍କାର")
	  (documentation . "\
Such languages using the Odia script as Odia, Khonti, and Santali
are supported in this language environment.  (This language
environment was formerly known as \"Oriya\")."))
 '("Indian"))

(set-language-info-alist
 "Oriya" '((charset unicode)
	  (coding-system utf-8)
	  (coding-priority utf-8)
	  (input-method . "odia")
          (sample-text . "Odia (ଓଡ଼ିଆ)	ନମସ୍କାର")
	  (documentation . "\
Such languages using the Odia script as Odia, Khonti, and Santali
are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Tamil" '((charset unicode)
	   (coding-system utf-8)
	   (coding-priority utf-8)
	   (input-method . "tamil-phonetic")
           (sample-text . "Tamil (தமிழ்)	வணக்கம்")
	   (documentation . "\
South Indian Language Tamil is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Telugu" '((charset unicode)
	    (coding-system utf-8)
	    (coding-priority utf-8)
	    (input-method . "telugu-itrans")
            (sample-text . "Telugu (తెలుగు)	నమస్కారం")
	    (documentation . "\
South Indian Language Telugu is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Kannada" '((charset unicode)
	     (coding-system mule-utf-8)
	     (coding-priority mule-utf-8)
	     (input-method . "kannada-itrans")
	     (sample-text . "Kannada (ಕನ್ನಡ)	ನಮಸ್ಕಾರ")
	     (documentation . "\
Kannada language and script are supported in this language
environment."))
 '("Indian"))

(set-language-info-alist
 "Malayalam" '((charset unicode)
	       (coding-system utf-8)
	       (coding-priority utf-8)
	       (input-method . "malayalam-itrans")
               (sample-text . "Malayalam (മലയാളം)	നമസ്കാരം")
	       (documentation . "\
South Indian language Malayalam is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Brahmi" '((charset unicode)
	    (coding-system utf-8)
	    (coding-priority utf-8)
	    (input-method . "brahmi")
            (sample-text . "Brahmi (𑀩𑁆𑀭𑀸𑀳𑁆𑀫𑀻)	𑀦𑀫𑀲𑁆𑀢𑁂")
	    (documentation . "\
The ancient Brahmi script is supported in this language environment."))
 '("Indian"))                           ; Should we have an "Old" category?

(set-language-info-alist
 "Kaithi" '((charset unicode)
            (coding-system utf-8)
            (coding-priority utf-8)
            (input-method . "kaithi")
            (sample-text . "Kaithi (𑂍𑂶𑂟𑂲)	𑂩𑂰𑂧𑂩𑂰𑂧")
            (documentation . "\
Languages such as Awadhi, Bhojpuri, Magahi and Maithili
which used the Kaithi script are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Tirhuta" '((charset unicode)
             (coding-system utf-8)
             (coding-priority utf-8)
             (input-method . "tirhuta")
             (sample-text . "Tirhuta (𑒞𑒱𑒩𑒯𑒳𑒞𑒰)	𑒣𑓂𑒩𑒢𑒰𑒧 / 𑒮𑒲𑒞𑒰𑒩𑒰𑒧")
             (documentation . "\
Maithili language and its script Tirhuta are supported in this
language environment."))
 '("Indian"))

(set-language-info-alist
 "Sharada" '((charset unicode)
             (coding-system utf-8)
             (coding-priority utf-8)
             (input-method . "sharada")
             (sample-text . "Sharada (𑆯𑆳𑆫𑆢𑆳)	𑆤𑆩𑆱𑇀𑆑𑆳𑆫")
             (documentation . "\
Kashmiri language and its script Sharada are supported in this
language environment."))
 '("Indian"))

(set-language-info-alist
 "Siddham" '((charset unicode)
             (coding-system utf-8)
             (coding-priority utf-8)
             (input-method . "siddham")
             (sample-text . "Siddham (𑖭𑖰𑖟𑖿𑖠𑖽)	𑖡𑖦𑖭𑖿𑖝𑖸")
             (documentation . "\
Sanskrit language and one of its script Siddham are supported
in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Syloti Nagri" '((charset unicode)
                  (coding-system utf-8)
                  (coding-priority utf-8)
                  (input-method . "syloti-nagri")
                  (sample-text . "Syloti Nagri (ꠍꠤꠟꠐꠤ ꠘꠣꠉꠞꠤ)	ꠀꠌ꠆ꠍꠣꠟꠣꠝꠥ ꠀꠟꠣꠁꠇꠥꠝ / ꠘꠝꠡ꠆ꠇꠣꠞ")
                  (documentation . "\
Sylheti language and its script Syloti Nagri are supported
in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Modi" '((charset unicode)
          (coding-system utf-8)
          (coding-priority utf-8)
          (input-method . "modi")
          (sample-text . "Modi (𑘦𑘻𑘚𑘲)	𑘡𑘦𑘭𑘿𑘎𑘰𑘨")
          (documentation . "\
Marathi language and one of its script Modi are supported
in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Limbu" '((charset unicode)
           (coding-system utf-8)
           (coding-priority utf-8)
           (input-method . "limbu")
           (sample-text . "Limbu (ᤕᤠᤰᤌᤢᤱ ᤐᤠᤴ)	ᤛᤣᤘᤠᤖᤥ")
           (documentation . "\
Limbu language and its script are supported in this
language environment."))
 '("Indian"))

(set-language-info-alist
 "Grantha" '((charset unicode)
             (coding-system utf-8)
             (coding-priority utf-8)
             (input-method . "grantha")
             (sample-text . "Grantha (𑌗𑍍𑌰𑌨𑍍𑌥)	𑌨𑌮𑌸𑍍𑌤𑍇 / 𑌨𑌮𑌸𑍍𑌕𑌾𑌰𑌃")
             (documentation . "\
Languages such as Sanskrit and Manipravalam, when they use the
Grantha script, are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Lepcha" '((charset unicode)
            (coding-system utf-8)
            (coding-priority utf-8)
            (input-method . "lepcha")
            (sample-text . "Lepcha (ᰛᰩᰵᰛᰧᰵᰶ)	ᰂᰦᰕᰥᰬ")
            (documentation . "\
Lepcha language and its script are supported in this
language environment."))
 '("Indian"))

(set-language-info-alist
 "Meetei Mayek" '((charset unicode)
                  (coding-system utf-8)
                  (coding-priority utf-8)
                  (input-method . "meetei-mayek")
                  (sample-text . "Meetei Mayek (ꯃꯤꯇꯩ ꯃꯌꯦꯛ)	ꯈꯨꯔꯨꯝꯖꯔꯤ")
                  (documentation . "\
Meetei language and its script Meetei Mayek are supported in this
language environment."))
 '("Indian"))

(set-language-info-alist
 "Wancho" '((charset unicode)
            (coding-system utf-8)
            (coding-priority utf-8)
            (input-method . "wancho")
            (sample-text . "Wancho (𞋒𞋀𞋉𞋃𞋕)	𞋂𞋈𞋛")
            (documentation . "\
Wancho language and its script are supported in this language
environment."))
 '("Indian"))

(set-language-info-alist
 "Toto" '((charset unicode)
          (coding-system utf-8)
          (coding-priority utf-8)
          (input-method . "toto")
          (documentation . "\
Toto language using the Toto script is supported in this language
environment."))
 '("Indian"))

;; Replace mnemonic characters in REGEXP according to TABLE.  TABLE is
;; an alist of (MNEMONIC-STRING . REPLACEMENT-STRING).

(defun indian-compose-regexp (regexp table)
  (let ((case-fold-search nil))
    (dolist (elt table)
      (setq regexp (replace-regexp-in-string (car elt) (cdr elt) regexp t t)))
    regexp))

(defconst devanagari-composable-pattern
  (let ((table
	 '(("a" . "[\u0900-\u0902]")	; vowel modifier (above)
	   ("A" . "\u0903")		; vowel modifier (post)
	   ("V" . "[\u0904-\u0914\u0960\u0961\u0972]") ; independent vowel
	   ("C" . "[\u0915-\u0939\u0958-\u095F\u0979-\u097F]") ; consonant
	   ("R" . "\u0930")		; RA
	   ("n" . "\u093C")		; NUKTA
	   ("v" . "[\u093E-\u094C\u094E\u0955\u0962\u0963]") ; vowel sign
	   ("H" . "\u094D")		; HALANT
	   ("s" . "[\u0951\u0952]")	; stress sign
	   ("t" . "[\u0953\u0954]")	; accent
           ("1" . "\u0967")             ; numeral 1
           ("3" . "\u0969")             ; numeral 3
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0900-\u097F]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?\\(?:J?HR\\)?v*n?a?s?t?A?\\|"
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*n?a?s?t?A?\\)\\|"
      ;; special consonant form, or
      "JHR\\|"
      ;; vedic accents with numerals, or
      "1ss?\\|3ss\\|s3ss\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Devanagari characters.")

(defconst bengali-composable-pattern
  (let ((table
	 '(("a" . "\u0981")		; SIGN CANDRABINDU
	   ("A" . "[\u0982\u0983]")	; SIGN ANUSVARA .. VISARGA
	   ("V" . "[\u0985-\u0994\u09E0\u09E1]") ; independent vowel
	   ("C" . "[\u0995-\u09B9\u09DC-\u09DF\u09F0\u09F1]") ; consonant
	   ("B" . "[\u09AC\u09AF\u09B0\u09F0]")		; BA, YA, RA
	   ("R" . "[\u09B0\u09F0]")		; RA
	   ("n" . "\u09BC")		; NUKTA
	   ("v" . "[\u09BE-\u09CC\u09D7\u09E2\u09E3]") ; vowel sign
	   ("H" . "\u09CD")		; HALANT
	   ("T" . "\u09CE")		; KHANDA TA
           ("S" . "\u09FE")             ; SANDHI MARK
           ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0980-\u09FF]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?\\(?:J?HB\\)?v*n?a?A?\\|"
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*[NJ]?v?a?A?S?\\)\\|"
      ;; another syllables with an independent vowel, or
      "\\(?:RH\\)?T\\|"
      ;; special consonant form, or
      "JHB\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Bengali characters.")

(defconst gurmukhi-composable-pattern
  (let ((table
	 '(("a" . "[\u0A01\u0A02\u0A70\u0A71\u0A75]") ; SIGN ADAK BINDI .. BINDI, TIPPI, ADDAK, YAKASH
	   ("A" . "\u0A03")		; SIGN VISARGA
	   ("V" . "[\u0A05-\u0A14\u0A72\u0A73]")	; independent vowel
	   ("C" . "[\u0A15-\u0A39\u0A59-\u0A5E]")	; consonant
	   ("Y" . "[\u0A2F\u0A30\u0A35\u0A39]") ; YA, RA, VA, HA
	   ("n" . "\u0A3C")		; NUKTA
	   ("v" . "[\u0A3E-\u0A4C]")	; vowel sign
	   ("H" . "\u0A4D")		; VIRAMA
           ("s" . "\u0A51")		; stress sign
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0A00-\u0A7F]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*n?a?s?v?A?\\)\\|"
      ;; syllables with an independent vowel, or
      "Vn?\\(?:J?HY\\)?v*n?a?s?A?\\|"
      ;; special consonant form, or
      "JHY\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Gurmukhi characters.")

(defconst gujarati-composable-pattern
  (let ((table
	 '(("a" . "[\u0A81\u0A82]")	; SIGN CANDRABINDU .. ANUSVARA
	   ("A" . "\u0A83")		; SIGN VISARGA
	   ("V" . "[\u0A85-\u0A94\u0AE0\u0AE1]") ; independent vowel
	   ("C" . "[\u0A95-\u0AB9]")	; consonant
	   ("R" . "\u0AB0")		; RA
	   ("n" . "\u0ABC")		; NUKTA
	   ("v" . "[\u0ABE-\u0ACC\u0AE2\u0AE3]") ; vowel sign
	   ("H" . "\u0ACD")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0A80-\u0AFF]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?\\(?:J?HR\\)?v*n?a?A?\\|"
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*n?a?A?\\)\\|"
      ;; special consonant form, or
      "JHR\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Gujarati characters.")

(defconst oriya-composable-pattern
  (let ((table
	 '(("a" . "\u0B01")		; SIGN CANDRABINDU
	   ("A" . "[\u0B02\u0B03]")	; SIGN ANUSVARA .. VISARGA
	   ("V" . "[\u0B05-\u0B14\u0B60\u0B61]") ; independent vowel
	   ("C" . "[\u0B15-\u0B39\u0B5C\u0B5D\u0B5F\u0B71]")	; consonant
	   ("B" . "[\u0B15-\u0B17\u0B1B-\u0B1D\u0B1F-\u0B21\u0B23\u0B24\u0B27-\u0B30\u0B32-\u0B35\u0B38\u0B39]") ; consonant with below form
	   ("R" . "\u0B30")		; RA
	   ("n" . "\u0B3C")		; NUKTA
	   ("v" . "[\u0B3E-\u0B4C\u0B56\u0B57\u0B62\u0B63]") ; vowel sign
	   ("H" . "\u0B4D")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0B00-\u0B7F]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?\\(?:J?HB\\)?v*n?a?A?\\|"
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*n?a?A?\\)\\|"
      ;; special consonant form, or
      "JHB\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Oriya characters.")

(defconst tamil-composable-pattern
  (let ((table
	 '(("a" . "\u0B82")		; SIGN ANUSVARA
	   ("V" . "[\u0B85-\u0B94]")	; independent vowel
	   ("C" . "[\u0B95-\u0BB9]")	; consonant
	   ("v" . "[\u0BBE-\u0BCC\u0BD7]") ; vowel sign
	   ("H" . "\u0BCD")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0B80-\u0BFF]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; consonant-based syllables, or
      "C\\(?:J?HJ?C\\)*\\(?:H[NJ]?\\|v*a?\\)\\|"
      ;; syllables with an independent vowel, or
      "Vv*a?\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Tamil characters.")

(defconst telugu-composable-pattern
  (let ((table
	 '(("a" . "[\u0C01-\u0C03]")	; SIGN CANDRABINDU .. VISARGA
	   ("V" . "[\u0C05-\u0C14\u0C60\u0C61]") ; independent vowel
	   ("C" . "[\u0C15-\u0C39\u0C58\u0C59]") ; consonant
	   ("v" . "[\u0C3E-\u0C4C\u0C55\u0C56\u0C62\u0C63]")	; vowel sign
	   ("H" . "\u0C4D")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0C00-\u0C7F]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; consonant-based syllables, or
      "C\\(?:J?HJ?C\\)*\\(?:H[NJ]?\\|v*a?\\)\\|"
      ;; syllables with an independent vowel, or
      "V\\(?:J?HC\\)?v*a?\\|"
      ;; special consonant form, or
      "JHC\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Telugu characters.")

(defconst kannada-composable-pattern
  (let ((table
	 '(("A" . "[\u0C82\u0C83]")	; SIGN ANUSVARA .. VISARGA
	   ("V" . "[\u0C85-\u0C94\u0CE0\u0CE1]") ; independent vowel
	   ("C" . "[\u0C95-\u0CB9\u0CDE]")	 ; consonant
	   ("R" . "\u0CB0")		; RA
	   ("n" . "\u0CBC")		; NUKTA
	   ("v" . "[\u0CBE-\u0CCC\u0CD5\u0CD6\u0CE2\u0CE3]") ; vowel sign
	   ("H" . "\u0CCD")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0C80-\u0CFF]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?\\(?:J?HC\\)?v?A?\\|"
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*n?A?\\)\\|"
      ;; special consonant form, or
      "JHC\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Kannada characters.")

(defconst malayalam-composable-pattern
  (let ((table
	 '(("A" . "[\u0D02\u0D03]")	; SIGN ANUSVARA .. VISARGA
	   ("V" . "[\u0D05-\u0D14\u0D60\u0D61]")  ; independent vowel
	   ("C" . "[\u0D15-\u0D39]")		  ; consonant
	   ("Y" . "[\u0D2F\u0D30\u0D32\u0D35]")   ; YA, RA, LA, VA
	   ("v" . "[\u0D3E-\u0D4C\u0D57\u0D62\u0D63]")	; postbase matra
	   ("H" . "\u0D4D")			  ; SIGN VIRAMA
	   ("N" . "\u200C")			  ; ZWNJ
	   ("J" . "\u200D")			  ; ZWJ
	   ("X" . "[\u0D00-\u0D7F]"))))		  ; all coverage
    (indian-compose-regexp
     (concat
      ;; any sequence of 2 or more Malayalam characters, or
      "XX+\\|"
      ;; consonant-based syllables, or
      "C\\(?:J?HJ?C\\)*\\(?:H[NJ]?\\|v?A?\\)\\|"
      ;; syllables with an independent vowel, or
      "V\\(?:J?HY\\)?v*?A?\\|"
      ;; special consonant form
      "JHY")
     table))
  "Regexp matching a composable sequence of Malayalam characters.")

(let ((script-regexp-alist
       `((devanagari . ,devanagari-composable-pattern)
	 (bengali . ,bengali-composable-pattern)
	 (gurmukhi . ,gurmukhi-composable-pattern)
	 (gujarati . ,gujarati-composable-pattern)
	 (oriya . ,oriya-composable-pattern)
	 (tamil . ,tamil-composable-pattern)
	 (telugu . ,telugu-composable-pattern)
	 (kannada . ,kannada-composable-pattern)
	 (malayalam . ,malayalam-composable-pattern))))
  (map-char-table
   #'(lambda (key val)
       (let ((slot (assq val script-regexp-alist)))
	 (if slot
	     (set-char-table-range
	      composition-function-table key
	      (list (vector (cdr slot) 0 #'font-shape-gstring))))))
   char-script-table))

;; Brahmi composition rules
(let ((consonant            "[\x11013-\x11037\x11075]")
      (independent-vowel    "[\x11005-\x11012\x11071\x11072]")
      (vowel                "[\x11038-\x11045\x11073\x11074]")
      (nasal                "[\x11000\x11001]")
      (virama               "\x11046")
      (jivhamuliya          "\x11003")
      (upadhmaniya          "\x11004")
      (ka-kha               "[\x11013\x11014]")
      (pa-pha               "[\x11027\x11028]")
      (number-joiner        "\x1107F")
      (numeral              "[\x11052-\x11065]")
      (multiplier           "[\x11064\x11065]"))
  (set-char-table-range composition-function-table
                        '(#x11046 . #x11046)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant "\\(?:" virama consonant
                                       "\\)*\\(?:" virama "\\|" vowel "*"
                                       nasal "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowel based syllables
                               (concat independent-vowel virama "?" vowel "?" nasal "?")
                               1 'font-shape-gstring)))
  (set-char-table-range composition-function-table
                        '(#x11003 . #x11004)
                        (list (vector
                               ;; Velar fricative
                               (concat jivhamuliya ka-kha "?")
                               0 'font-shape-gstring)
                              (vector
                               ;; Bilabial fricative
                               (concat upadhmaniya pa-pha "?")
                               0 'font-shape-gstring)))
  (set-char-table-range composition-function-table
                        '(#x1107F . #x1107F)
                        (list (vector
                               ;; Additive-multiplicative numerals
                               (concat multiplier number-joiner numeral)
                               1 'font-shape-gstring))))

;; Kaithi composition rules
(let ((consonant            "[\x1108D-\x110AF]")
      (nukta                "\x110BA")
      (independent-vowel    "[\x11083-\x1108C]")
      (vowel                "[\x1108D-\x110C2]")
      (nasal                "[\x11080\x11081]")
      (virama               "\x110B9")
      (number-sign          "\x110BD")
      (number-sign-above    "\x110CD")
      (numerals             "[\x966-\x96F]+")
      (zwj                  "\x200D"))
  (set-char-table-range composition-function-table
                        '(#x110B0 . #x110BA)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant nukta "?\\(?:" virama zwj "?" consonant
                                       nukta "?\\)*\\(?:" virama zwj "?\\|" vowel "*" nukta
                                       "?" nasal "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowel based syllables
                               (concat independent-vowel nukta "?" virama "?" vowel "?")
                               1 'font-shape-gstring)))
  (set-char-table-range composition-function-table
                        '(#x110BD . #x110BD)
                        (list (vector
                               ;; Number sign
                               (concat number-sign numerals)
                               0 'font-shape-gstring)))
  (set-char-table-range composition-function-table
                        '(#x110CD . #x110CD)
                        (list (vector
                               ;; Number sign above
                               (concat number-sign-above numerals)
                               0 'font-shape-gstring))))

;; Tirhuta composition rules
(let ((consonant            "[\x1148F-\x114AF]")
      (nukta                "\x114C3")
      (independent-vowel    "[\x11481-\x1148E]")
      (vowel                "[\x114B0-\x114BE]")
      (nasal                "[\x114BF\x114C0]")
      (virama               "\x114C2"))
  (set-char-table-range composition-function-table
                        '(#x114B0 . #x114C3)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant nukta "?\\(?:" virama consonant nukta
                                       "?\\)*\\(?:" virama "\\|" vowel "*" nukta "?"
                                       nasal "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowel based syllables
                               (concat independent-vowel nukta "?" virama "?" vowel "?" nasal "?")
                               1 'font-shape-gstring))))

;; Sharada composition rules
(let ((consonant              "[\x11191-\x111B2]")
      (nukta                  "\x111CA")
      (independent-vowel      "[\x11183-\x11190]")
      (vowel                  "[\x111B3-\x111BF\x111CE]")
      (vowel-modifier         "\x111CB")
      (extra-short-vowel-mark "\x111CC")
      (nasal                  "[\x11181\x11180\x111CF]")
      (virama                 "\x111C0")
      (fricatives             "[\x111C2\x111C3]")
      (sandhi-mark            "\x111C9")
      (misc                   "[\x111C4-\x111C8\x111CD]"))
  (set-char-table-range composition-function-table
                        '(#x111B3 . #x111CE)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant nukta "?" vowel-modifier "?\\(?:" virama
                                       consonant nukta "?" vowel-modifier "?\\)*\\(?:" virama
                                       "\\|" vowel "*" nukta "?" nasal "?" extra-short-vowel-mark
                                        "?" vowel-modifier "?" sandhi-mark "?+" misc "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowel based syllables
                               (concat independent-vowel nukta "?" vowel-modifier "?" virama "?"
                                       vowel "?" extra-short-vowel-mark "?" sandhi-mark "?"
                                       fricatives "?" misc "?")
                               1 'font-shape-gstring)
                              (vector
                               ;; Fricatives with Consonants
                               (concat fricatives "?" consonant vowel "?")
                               0 'font-shape-gstring))))

;; Siddham composition rules
(let ((consonant            "[\x1158E-\x115AE]")
      (nukta                "\x115C0")
      (independent-vowel    "[\x11580-\x1158D\x115D8-\x115DB]")
      (vowel                "[\x115AF-\x115BB\x115DC\x115DD]")
      (nasal                "[\x115BC\x115BD]")
      (visarga              "\x115BE")
      (virama               "\x115BF"))
  (set-char-table-range composition-function-table
                        '(#x115AF . #x115C0)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant nukta "?" "\\(?:" virama consonant nukta
                                       "?\\)*\\(?:" virama "\\|" vowel "*" nukta "?" nasal
                                       "?" visarga "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowels based syllables
                               (concat independent-vowel nukta "?" virama "?" vowel "?"
                                       nasal "?" visarga "?")
                               1 'font-shape-gstring))))

;; Syloti Nagri composition rules
(let ((consonant            "[\xA807-\xA80A\xA80C-\xA822]")
      (vowel                "[\xA802\xA823-\xA827]")
      (nasal                "[\xA80B]")
      (virama               "\xA806")
      (alternate-virama     "\xA82C"))
  (set-char-table-range composition-function-table
                        '(#xA806 . #xA806)
                        (list (vector
                               ;; Consonant conjunct based syllables
                               (concat consonant "\\(?:" virama consonant "\\)+"
                                       vowel "?" nasal "?")
                               1 'font-shape-gstring)))
  (set-char-table-range composition-function-table
                        '(#xA823 . #xA827)
                        (list (vector
                               ;; Non Consonant conjunct based syllables
                               (concat consonant vowel nasal "?")
                               1 'font-shape-gstring)))
    (set-char-table-range composition-function-table
                        '(#xA82C . #xA82C)
                        (list (vector
                               ;; Consonant with the alternate virama
                               (concat consonant "\\(?:" alternate-virama consonant "\\)+"
                                       vowel "?" nasal "?")
                               1 'font-shape-gstring))))

;; Modi composition rules
(let ((consonant            "[\x1160E-\x1162F]")
      (independent-vowel    "[\x11600-\x1160D]")
      (vowel                "[\x11630-\x1163C]")
      (nasal                "\x1163D")
      (visarga              "\x1163E")
      (virama               "\x1163F")
      (ardhacandra          "\x11640"))
  (set-char-table-range composition-function-table
                        '(#x11630 . #x11640)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant "\\(?:" virama consonant "\\)*\\(?:"
                                       virama "\\|" vowel "*" ardhacandra "?" nasal
                                       "?" visarga "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowels based syllables
                               (concat independent-vowel virama "?" vowel "?" ardhacandra
                                       nasal "?" visarga "?")
                               1 'font-shape-gstring))))

;; Limbu composition rules
(let ((consonant            "[\x1900-\x191E]")
      (vowel                "[\x1920-\x1928]")
      (subjoined-letter     "[\x1929-\x192B]")
      (small-letter         "[\x1930-\x1938]")
      (other-signs          "[\x1939\x193A]")
      (sa-i                 "\x193B"))
  (set-char-table-range composition-function-table
                        '(#x1920 . #x193B)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant sa-i "?" subjoined-letter "?" small-letter
                                       "?" vowel "?" other-signs "?")
                               1 'font-shape-gstring))))

;; Grantha composition rules
(let ((consonant            "[\x11315-\x11339]")
      (nukta                "\x1133C")
      (independent-vowel    "[\x11305-\x11314\x11360\x11361]")
      (vowel                "[\x1133E-\x1134C\x11357\x11362\x11363]")
      (nasal                "[\x11300-\x11302]")
      (bindu                "\x1133B")
      (visarga              "\x11303")
      (virama               "\x1134D")
      (avagraha             "\x1133D")
      (modifier-above       "[\x11366-\x11374]"))
  (set-char-table-range composition-function-table
                        '(#x1133B . #x1134D)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant nukta "?" "\\(?:" virama consonant nukta
                                       "?\\)*\\(?:" virama "\\|" vowel "*" nukta "?" nasal
                                       "?" bindu "?" visarga "?" modifier-above "?"
                                       avagraha "?\\)")
                               1 'font-shape-gstring)
                              (vector
                               ;; Vowels based syllables
                               (concat independent-vowel nukta "?" virama "?" vowel "?"
                                       nasal "?" bindu "?" visarga "?" modifier-above
                                       "?" avagraha "?")
                               1 'font-shape-gstring))))

;; Lepcha composition rules
(let ((consonant            "[\x1C00-\x1C23\x1C4D-\x1C4F]")
      (vowel                "[\x1C26-\x1C2C]")
      (subjoined-letter     "[\x1C24\x1C25]")
      (consonant-sign       "[\x1C2D-\x1C35]")
      (other-signs          "[\x1C36\x1C37]"))
  (set-char-table-range composition-function-table
                        '(#x1C24 . #x1C37)
                        (list (vector
                               ;; Consonant based syllables
                               (concat consonant other-signs "?" vowel "?"
                                       consonant-sign "?" subjoined-letter "?"
                                       other-signs "?")
                               1 'font-shape-gstring))))

;; Meetei Mayek composition rules
(let ((akshara              "[\xABC0-\xABE2\xAAE0-\xAAEA]")
      (vowel                "[\xABE3-\xABE9\xAAEB-\xAAEC]")
      (nasal                "\xABEA")
      (visarga              "\xAAF5")
      (virama               "[\xABED\xAAF6]")
      (heavy-tone           "\x11640"))
  (set-char-table-range composition-function-table
                        '(#xABE3 . #xABED)
                        (list (vector
                               ;; Consonant based syllables
                               (concat akshara "\\(?:" virama akshara "\\)*\\(?:"
                                       virama "\\|" vowel "*" nasal "?" visarga "?"
                                       heavy-tone "?\\)")
                               1 'font-shape-gstring))))

(provide 'indian)
;;; indian.el ends here
