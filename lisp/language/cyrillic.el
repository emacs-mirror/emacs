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

;; ISO-8859-5 stuff

(define-coding-system 'cyrillic-iso-8bit
  "ISO 2022 based 8-bit encoding for Cyrillic script (MIME:ISO-8859-5)."
  :coding-type 'charset
  :mnemonic ?5
  :charset-list '(iso-8859-5)
  :mime-charset 'iso-8859-5)

(define-coding-system-alias 'iso-8859-5 'cyrillic-iso-8bit)

(set-language-info-alist
 "Cyrillic-ISO" '((charset iso-8859-5)
		  (coding-system cyrillic-iso-8bit)
		  (coding-priority cyrillic-iso-8bit)
		  (nonascii-translation . iso-8859-5)
		  (input-method . "cyrillic-yawerty")
		  (unibyte-display . cyrillic-iso-8bit)
		  (features cyril-util)
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ISO-8859-5."))
 '("Cyrillic"))

;; KOI-8 stuff

(define-coding-system 'cyrillic-koi8
  "KOI8 8-bit encoding for Cyrillic (MIME: KOI8-R)."
  :coding-type 'charset
  ;; We used to use ?K.  It is true that ?K is more strictly correct,
  ;; but it is also used for Korean.  So people who use koi8 for
  ;; languages other than Russian will have to forgive us.
  :mnemonic ?R
  :charset-list '(koi8)
  :mime-charset 'koi8-r)

(define-coding-system-alias 'koi8-r 'cyrillic-koi8)
(define-coding-system-alias 'koi8 'cyrillic-koi8)
(define-coding-system-alias 'cp878 'cyrillic-koi8)

(set-language-info-alist
 "Cyrillic-KOI8" `((charset koi8)
		   (coding-system cyrillic-koi8)
		   (coding-priority cyrillic-koi8)
		   (nonascii-translation . koi8)
		   (input-method . "cyrillic-jcuken")
		   (features cyril-util)
		   (unibyte-display . cyrillic-koi8)
		   (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		   (documentation . "Support for Cyrillic KOI8-R."))
 '("Cyrillic"))

;;; ALTERNATIVNYJ stuff

(define-coding-system 'cyrillic-alternativnyj
  "ALTERNATIVNYJ 8-bit encoding for Cyrillic."
  :coding-type 'charset
  :mnemonic ?A
  :charset-list '(alternativnyj)
  :mime-charset 'cp866)

(define-coding-system-alias 'alternativnyj 'cyrillic-alternativnyj)
(define-coding-system-alias 'cp866 'cyrillic-alternativnyj)

(set-language-info-alist
 "Cyrillic-ALT" `((charset alternativnyj)
		  (coding-system cyrillic-alternativnyj)
		  (coding-priority cyrillic-alternativnyj)
		  (nonascii-translation . alternativnyj)
		  (input-method . "cyrillic-jcuken")
		  (features cyril-util)
		  (unibyte-display . cyrillic-alternativnyj)
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ALTERNATIVNYJ."))
 '("Cyrillic"))

(define-coding-system 'koi8-u
  "KOI8-U 8-bit encoding for Cyrillic (MIME: KOI8-U)"
  :coding-type 'charset
  :mnemonic ?U
  :charset-list '(koi8-u)
  :mime-charset 'koi8-u)

(define-coding-system 'koi8-t
  "KOI8-T 8-bit encoding for Cyrillic (MIME: KOI8-T)"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(koi8-t)
  :mime-charset 'koi8-t)

(define-coding-system 'windows-1251
  "windows-1251 8-bit encoding for Cyrillic (MIME: WINDOWS-1251)"
  :coding-type 'charset
  :mnemonic ?b
  :charset-list '(windows-1251)
  :mime-charset 'windows-1251)
(define-coding-system-alias 'cp1251 'windows-1251)

(define-coding-system 'cp1125
  "cp1125 8-bit encoding for Cyrillic (MIME: CP1125)"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(cp1125))
(define-coding-system-alias 'ruscii 'cp1125)
;; Original name for cp1125, says Serhii Hlodin <hlodin@lutsk.bank.gov.ua>
(define-coding-system-alias 'cp866u 'cp1125)

(provide 'cyrillic)

;;; cyrillic.el ends here
