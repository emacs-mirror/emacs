;;; european.el --- support for European languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995, 1997, 2001 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.

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

(set-language-info-alist
 "Latin-1" '((charset iso-8859-1)
	     (coding-system iso-latin-1)
	     (coding-priority iso-latin-1)
	     (nonascii-translation . iso-8859-1)
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

(define-coding-system 'iso-latin-2
   "ISO 2022 based 8-bit encoding for Latin-2 (MIME:ISO-8859-2)."
  :coding-type 'charset
  :mnemonic ?2
  :charset-list '(iso-8859-2)
  :mime-charset 'iso-8859-2)

(define-coding-system-alias 'iso-8859-2 'iso-latin-2)
(define-coding-system-alias 'latin-2 'iso-latin-2)

(set-language-info-alist
 "Latin-2" '((charset iso-8859-2)
	     (coding-system iso-latin-2)
	     (coding-priority iso-latin-2)
	     (nonascii-translation . iso-8859-2)
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

(define-coding-system 'iso-latin-3
  "ISO 2022 based 8-bit encoding for Latin-3 (MIME:ISO-8859-3)."
  :coding-type 'charset
  :mnemonic ?3
  :charset-list '(iso-8859-3)
  :mime-charset 'iso-8859-3)

(define-coding-system-alias 'iso-8859-3 'iso-latin-3)
(define-coding-system-alias 'latin-3 'iso-latin-3)

(set-language-info-alist
 "Latin-3" '((charset iso-8859-3)
	     (coding-system iso-latin-3)
	     (coding-priority iso-latin-3)
	     (nonascii-translation . iso-8859-3)
	     (unibyte-syntax . "latin-3")
	     (unibyte-display . iso-latin-3)
	     (input-method . "latin-3-prefix")
	     (documentation . "\
These languages are supported with the Latin-3 (ISO-8859-3) character set:
 Afrikaans, Catalan, Dutch, English, Esperanto, French, Galician,
 German, Italian, Maltese, Spanish, and Turkish."))
 '("European"))


;; Latin-4 (ISO-8859-4)

(define-coding-system 'iso-latin-4
  "ISO 2022 based 8-bit encoding for Latin-4 (MIME:ISO-8859-4)."
  :coding-type 'charset
  :mnemonic ?4
  :charset-list '(iso-8859-4)
  :mime-charset 'iso-8859-4)

(define-coding-system-alias 'iso-8859-4 'iso-latin-4)
(define-coding-system-alias 'latin-4 'iso-latin-4)

(set-language-info-alist
 "Latin-4" '((charset iso-8859-4)
	     (coding-system iso-8859-4)
	     (coding-priority iso-8859-4)
	     (nonascii-translation . iso-8859-4)
	     (unibyte-syntax . "latin-4")
	     (unibyte-display . iso-8859-4)
	     (input-method . "latin-4-postfix")
	     (documentation . "\
These languages are supported with the Latin-4 (ISO-8859-4) character set:
 Danish, English, Estonian, Finnish, German, Greenlandic, Lappish,
 Latvian, Lithuanian, and Norwegian."))
 '("European"))


;; Latin-5 (ISO-8859-9)

(define-coding-system 'iso-latin-5
  "ISO 2022 based 8-bit encoding for Latin-5 (MIME:ISO-8859-9)."
  :coding-type 'charset
  :mnemonic ?9
  :charset-list '(iso-8859-9)
  :mime-charset 'iso-8859-9)

(define-coding-system-alias 'iso-8859-9 'iso-latin-5)
(define-coding-system-alias 'latin-5 'iso-latin-5)

(set-language-info-alist
 "Latin-5" '((charset iso-8859-9)
	     (coding-system iso-latin-5)
	     (coding-priority iso-latin-5)
	     (nonascii-translation . iso-8859-9)
	     (unibyte-syntax . "latin-5")
	     (unibyte-display . iso-latin-5)
	     (input-method . "latin-5-postfix")
	     (documentation . "Support for Turkish language."))
 '("European"))


;; Latin-8 (ISO-8859-14)

(define-coding-system 'iso-latin-8
  "ISO 2022 based 8-bit encoding for Latin-8 (MIME:ISO-8859-14)."
  :coding-type 'charset
  ;; `W' for `Welsh', since `C' for `Celtic' is taken.
  :mnemonic ?W				
  :charset-list '(iso-8859-14)
  :mime-charset 'iso-8859-14)

(define-coding-system-alias 'iso-8859-14 'iso-latin-8)
(define-coding-system-alias 'latin-8 'iso-latin-8)

(set-language-info-alist
 "Latin-8" '((charset iso-8859-14)
	     (coding-system iso-latin-8)
	     (coding-priority iso-latin-8)
	     (nonascii-translation . iso-8859-14)
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

(define-coding-system 'iso-latin-9
  "ISO 2022 based 8-bit encoding for Latin-9 (MIME:ISO-8859-15)."
  :coding-type 'charset
  ;; `0' for `Latin-0'
  :mnemonic ?0
  :charset-list '(iso-8859-15)
  :mime-charset 'iso-8859-15)

(define-coding-system-alias 'iso-8859-15 'iso-latin-9)
(define-coding-system-alias 'latin-9 'iso-latin-9)
(define-coding-system-alias 'latin-0 'iso-latin-9)

(set-language-info-alist
 "Latin-9" '((charset iso-8859-15)
	     (coding-system iso-latin-9)
	     (coding-priority iso-latin-9)
	     (nonascii-translation . iso-8859-15)
	     (unibyte-syntax . "latin-9")
	     (unibyte-display . iso-latin-9)
	     (input-method . "latin-9-prefix")
	     (sample-text
	      . "AVE. ,B)9.>,b<=,_/(B ,b$(B")
	     (documentation . "\
This language environment is a generic one for the Latin-9 (ISO-8859-15)
character set which supports the same languages as Latin-1 with the
addition of the Euro sign and some additional French and Finnish letters.
Latin-9 is sometimes nicknamed `Latin-0'."))
 '("European"))

(define-coding-system 'windows-1252
  "windows-1252 8-bit encoding for Cyrillic (MIME: WINDOWS-1252)"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(windows-1252)
  :mime-charset 'windows-1252)
(define-coding-system-alias 'cp1252 'windows-1252)

(set-language-info-alist
 "German" '((tutorial . "TUTORIAL.de")
	    (charset iso-8859-1)
	    (coding-system iso-latin-1)
	    (coding-priority iso-latin-1)
	    (nonascii-translation . iso-8859-1)
	    (input-method . "german-postfix")
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
	    (charset iso-8859-1)
	    (coding-system iso-latin-1)
	    (coding-priority iso-latin-1)
	    (nonascii-translation . iso-8859-1)
	    (unibyte-syntax . "latin-1")
	    (unibyte-display . iso-latin-1)
	    (input-method . "latin-1-prefix")
	    (sample-text . "French (Fran,Ag(Bais)	Bonjour, Salut")
	    (documentation . "\
This language environment is almost the same as Latin-1,
but it selects the French tutorial."))
 '("European"))

(set-language-info-alist
 "Slovenian" '((charset iso-8859-2)
	      (coding-system . (iso-8859-2))
	      (coding-priority . (iso-8859-2))
	      (nonascii-translation . iso-8859-2)
	      (input-method . "latin-2-postfix")
	      (unibyte-syntax . "latin-2")
	      (unibyte-display . iso-8859-2)
	      (tutorial . "TUTORIAL.sl")
	      (sample-text . ",B.(Belimo vam uspe,B9(Ben dan!")
	      (documentation . t))
 '("European"))

(set-language-info-alist
 "Spanish" '((tutorial . "TUTORIAL.es")
	    (charset iso-8859-1)
	    (coding-system iso-latin-1)
	    (coding-priority iso-latin-1)
	    (nonascii-translation . iso-8859-1)
	    (input-method . "spanish-postfix")
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
	   (charset iso-8859-1)
	   (coding-system iso-latin-1)
	   (coding-priority iso-latin-1)
	   (nonascii-translation . iso-8859-1)
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
 "Turkish" '((charset iso-8859-9)
	     (coding-system iso-latin-5)
	     (coding-priority iso-latin-5)
	     (nonascii-translation . iso-8859-9)
	     (unibyte-syntax . "latin-5")
	     (unibyte-display . iso-latin-5)
	     (input-method . "turkish-postfix")
	     (sample-text . "Turkish (T,A|(Brk,Ag(Be)	Merhaba")
	     (documentation . t)))

;; Polish ISO 8859-2 environment.
;; Maintainer: Wlodek Bzyl <matwb@univ.gda.pl>
;; Keywords: multilingual, Polish

(set-language-info-alist
 "Polish" '((charset . (iso-8859-2))
	   (coding-system . (iso-8859-2))
	   (coding-priority . (iso-8859-2))
	   (nonascii-translation . iso-8859-2)
	   (input-method . "polish-slash")
	   (unibyte-syntax . "latin-2")
	   (unibyte-display . iso-8859-2)
	   (tutorial . "TUTORIAL.pl")
	   (sample-text . "P,As(Bjd,B<(B, ki,Bq(B-,B?(Be t,Bj(B chmurno,B6f(B w g,B31(Bb flaszy")
	   (documentation . t))
 '("European"))

(set-language-info-alist
 "Welsh" `((coding-system utf-8 latin-8) ; the input method is Unicode-based
	   (coding-priority utf-8 latin-8)
	   (nonascii-translation . iso-8859-14)
	   (input-method . "welsh")
	   (documentation . "Support for Welsh, using Unicode."))
 '("European"))

(set-language-info-alist
 "Latin-7" `((coding-system latin-7)
	     (coding-priority latin-7)
	     (nonascii-translation . iso-8859-13)
	     ;; Fixme: input-method
	     (features code-pages)
	     (documentation . "Support for Latin-7, e.g. Latvian, Lithuanian."))
 '("European"))

(set-language-info-alist
 "Lithuanian" `((coding-system latin-7)
		(coding-priority latin-7)
		(nonascii-translation . iso-8859-13)
		(input-method . "lithuanian-keyboard")
		(features code-pages)
		(documentation . "Support for Lithuanian."))
 '("European"))

(set-language-info-alist
 "Latvian" `((coding-system latin-7)
	     (coding-priority latin-7)
	     (nonascii-translation . iso-8859-13)
	     (input-method . "latvian-keyboard")
	     (features code-pages)
	     (documentation . "Support for Latvian."))
 '("European"))


(define-coding-system 'mac-roman
  "Mac Roman Encoding (MIME:MACINTOSH)."
  :coding-type 'charset
  :mnemonic ?M 
  :charset-list '(mac-roman)
  ;; per IANA, rfc1345
  :mime-charset 'macintosh)

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
      (aset composition-function-table (decode-char 'unicode c) patterns)
      (setq c (1+ c)))
    (setq c #x20d0)
    (while (<= c #x20e3)
      (aset composition-function-table (decode-char 'unicode c) patterns)
      (setq c (1+ c)))))

(provide 'european)

;;; european.el ends here
