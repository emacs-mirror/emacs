;;; european.el --- European languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995, 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

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

;; For Europeans, five character sets ISO8859-1,2,3,4,9 are supported.

;;; Code:

;; Latin-1 (ISO-8859-1)

(make-coding-system
 'iso-latin-1 2 ?1
 "ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)"
 '(ascii latin-iso8859-1 nil nil
   nil nil nil nil nil nil nil nil nil nil nil nil t)
 '((safe-charsets ascii latin-iso8859-1)
   (mime-charset . iso-8859-1)))

(define-coding-system-alias 'iso-8859-1 'iso-latin-1)
(define-coding-system-alias 'latin-1 'iso-latin-1)

(make-coding-system
 'compound-text 2 ?1
 "ISO 2022 based encoding used in inter client communication of X"
 '((ascii t) (latin-iso8859-1 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil nil nil nil nil nil t)
 '((safe-charsets . t)))

(define-coding-system-alias 'ctext 'compound-text)

(defun setup-latin1-environment ()
  "Set up multilingual environment (MULE) for European Latin-1 users."
  (interactive)
  (set-language-environment "Latin-1"))

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
This language environment is a generic one for Latin-1 (ISO-8859-1)
character set which supports the following languages:
 Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish.
We also have a German specific language environment \"German\"."))
 '("European"))


;; Latin-2 (ISO-8859-2)

(make-coding-system
 'iso-latin-2 2 ?2
 "ISO 2022 based 8-bit encoding (MIME:ISO-8859-2)"
 '(ascii latin-iso8859-2 nil nil
   nil nil nil nil nil nil nil)
 '((safe-charsets ascii latin-iso8859-2)
   (mime-charset . iso-8859-2)))

(define-coding-system-alias 'iso-8859-2 'iso-latin-2)
(define-coding-system-alias 'latin-2 'iso-latin-2)

(defun setup-latin2-environment ()
  "Set up multilingual environment (MULE) for European Latin-2 users."
  (interactive)
  (set-language-environment "Latin-2"))

(set-language-info-alist
 "Latin-2" '((charset ascii latin-iso8859-2)
	     (coding-system iso-latin-2)
	     (coding-priority iso-latin-2)
	     (nonascii-translation . latin-iso8859-2)
	     (unibyte-syntax . "latin-2")
	     (unibyte-display . iso-latin-2)
	     (input-method . "latin-2-prefix")
	     (documentation . "\
This language environment is a generic one for Latin-2 (ISO-8859-2)
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
 "ISO 2022 based 8-bit encoding (MIME:ISO-8859-3)"
 '(ascii latin-iso8859-3 nil nil
   nil nil nil nil nil nil nil)
 '((safe-charsets ascii latin-iso8859-3)
   (mime-charset . iso-8859-3)))

(define-coding-system-alias 'iso-8859-3 'iso-latin-3)
(define-coding-system-alias 'latin-3 'iso-latin-3)

(defun setup-latin3-environment ()
  "Set up multilingual environment (MULE) for European Latin-3 users."
  (interactive)
  (set-language-environment "Latin-3"))

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
 "ISO 2022 based 8-bit encoding (MIME:ISO-8859-4)"
 '(ascii latin-iso8859-4 nil nil
   nil nil nil nil nil nil nil)
 '((safe-charsets ascii latin-iso8859-4)
   (mime-charset . iso-8895-4)))

(define-coding-system-alias 'iso-8859-4 'iso-latin-4)
(define-coding-system-alias 'latin-4 'iso-latin-4)

(defun setup-latin4-environment ()
  "Set up multilingual environment (MULE) for European Latin-4 users."
  (interactive)
  (set-language-environment "Latin-4"))

(set-language-info-alist
 "Latin-4" '((charset ascii latin-iso8859-4)
	     (coding-system iso-8859-4)
	     (coding-priority iso-8859-4)
	     (nonascii-translation . latin-iso8859-4)
	     (unibyte-syntax . "latin-4")
	     (unibyte-display . iso-8859-4)
	     (input-method . "latin-4-prefix")
	     (documentation . "\
These languages are supported with the Latin-4 (ISO-8859-4) character set:
 Danish, English, Estonian, Finnish, German, Greenlandic, Lappish,
 Latvian, Lithuanian, and Norwegian."))
 '("European"))


;; Latin-5 (ISO-8859-9)

(make-coding-system
 'iso-latin-5 2 ?9
 "ISO 2022 based 8-bit encoding (MIME:ISO-8859-9)"
 '(ascii latin-iso8859-9 nil nil
   nil nil nil nil nil nil nil)
 '((safe-charsets ascii latin-iso8859-9)
   (mime-charset . iso-8859-9)))

(define-coding-system-alias 'iso-8859-9 'iso-latin-5)
(define-coding-system-alias 'latin-5 'iso-latin-5)

(defun setup-latin5-environment ()
  "Set up multilingual environment (MULE) for European Latin-5 users."
  (interactive)
  (set-language-environment "Latin-5"))

(set-language-info-alist
 "Latin-5" '((charset ascii latin-iso8859-9)
	     (coding-system iso-latin-5)
	     (coding-priority iso-latin-5)
	     (nonascii-translation . latin-iso8859-9)
	     (unibyte-syntax . "latin-5")
	     (unibyte-display . iso-latin-5)
	     (input-method . "latin-5-prefix")
	     (documentation . "\
These languages are supported with the Latin-5 (ISO-8859-9) character set."))
 '("European"))


(defun setup-german-environment ()
  "Set up multilingual environment (MULE) for German users."
  (interactive)
  (set-language-environment "German"))

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
but default input method is set to \"german-postfix\"."))
 '("European"))

(defun setup-slovenian-environment ()
  "Setup multilingual environment (MULE) for Slovenian."
  (interactive)
  (set-language-environment "Slovenian"))

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

;;; european.el ends here
