;;; european.el --- Support for European languages

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

;; Setup for LANGAUGE which uses one-byte 8-bit CHARSET, one-byte
;; 8-bit coding system, and INPUT-METHOD.
(defun setup-8-bit-environment (language charset input-method)
  (setup-english-environment)
  (set-language-environment-coding-systems language)

  (when default-enable-multibyte-characters
    (if charset
	(let ((nonascii-offset (- (make-char charset) 128)))
	  ;; Set up for insertion of characters in this character set
	  ;; when codes 0200 - 0377 are typed in.
	  (setq nonascii-insert-offset nonascii-offset)))

    (if input-method
	(setq default-input-method input-method)))

  ;; If this language environment supports unibyte operation,
  ;; load the proper syntax definitions for codes 0240-0377.
  (when (get-language-info language 'unibyte-syntax)
    (let ((set-case-syntax-set-multibyte nil))
      (load (get-language-info language 'unibyte-syntax) nil t)
      (set-standard-case-table (standard-case-table))
      (let ((list (buffer-list)))
	(while list
	  (with-current-buffer (car list)
	    (set-case-table (standard-case-table)))
	  (setq list (cdr list)))))))

;; Latin-1 (ISO-8859-1)

(make-coding-system
 'iso-latin-1 2 ?1
 "ISO 2022 based 8-bit encoding for Laint-1 (MIME:ISO-8859-1)"
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
  (setup-8-bit-environment "Latin-1" 'latin-iso8859-1 "latin-1-prefix"))

(set-language-info-alist
 "Latin-1" '((setup-function . setup-latin1-environment)
	     (charset ascii latin-iso8859-1)
	     (coding-system iso-latin-1)
	     (coding-priority iso-latin-1)
	     (unibyte-syntax . "latin-1")
	     (sample-text
	      . "Hello, Hej, Tere, Hei, Bonjour, Gr,A|_(B Gott, Ciao, ,A!(BHola!")
	     (documentation . "\
These languages are supported with the Latin-1 (ISO-8859-1) character set:
 Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish."))
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
  (setup-8-bit-environment "Latin-2" 'latin-iso8859-2 "latin-2-prefix"))

(set-language-info-alist
 "Latin-2" '((setup-function . setup-latin2-environment)
	     (charset ascii latin-iso8859-2)
	     (coding-system iso-latin-2)
	     (coding-priority iso-latin-2)
	     (unibyte-syntax . "latin-2")
	     (documentation . "\
These languages are supported with the Latin-2 (ISO-8859-2) character set:
 Albanian, Czech, English, German, Hungarian, Polish, Romanian,
 Serbo-Croatian or Croatian, Slovak, Slovene, Sorbian (upper and lower),
 and Swedish."))
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
  (setup-8-bit-environment "Latin-3" 'latin-iso8859-3 "latin-3-prefix"))

(set-language-info-alist
 "Latin-3" '((setup-function . setup-latin3-environment)
	     (charset ascii latin-iso8859-3)
	     (coding-system iso-latin-3)
	     (coding-priority iso-latin-3)
	     (unibyte-syntax . "latin-3")
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
  (setup-8-bit-environment "Latin-4" 'latin-iso8859-4 "latin-4-prefix"))

(set-language-info-alist
 "Latin-4" '((setup-function . setup-latin4-environment)
	     (charset ascii latin-iso8859-4)
	     (coding-system iso-8859-4)
	     (coding-priority iso-8859-4)
	     (unibyte-syntax . "latin-4")
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
  (setup-8-bit-environment "Latin-5" 'latin-iso8859-9 "latin-5-prefix"))

(set-language-info-alist
 "Latin-5" '((setup-function . setup-latin5-environment)
	     (charset ascii latin-iso8859-9)
	     (coding-system iso-latin-5)
	     (coding-priority iso-latin-5)
	     (unibyte-syntax . "latin-5")
	     (documentation . "\
These languages are supported with the Latin-5 (ISO-8859-9) character set."))
 '("European"))


(defun setup-german-environment ()
  "Set up multilingual environment (MULE) for German users."
  (interactive)
  (funcall (get-language-info "Latin-1" 'setup-function))
  (setq default-input-method "german-postfix"))

(set-language-info-alist
 "German" '((setup-function . setup-german-environment)
	    (tutorial . "TUTORIAL.de")
	    (charset ascii latin-iso8859-1)
	    (coding-system iso-latin-1)
	    (coding-priority iso-latin-1)
	    (unibyte-syntax . "latin-1")
	    (sample-text . "\
German (Deutsch Nord)	Guten Tag
German (Deutsch S,A|(Bd)	Gr,A|_(B Gott")
	    (documentation . "\
This language environment is almost the same as Latin-1,
but default input method is set to \"german-postfix\"."))
 '("European"))

;;; european.el ends here
