;;; greek.el --- support for Greek

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Keywords: multilingual, Greek

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

;; For Greek, the character set ISO8859-7 is supported.

;;; Code:

(define-coding-system 'greek-iso-8bit
  "ISO 2022 based 8-bit encoding for Greek (MIME:ISO-8859-7)."
  :coding-type 'charset
  :mnemonic ?7
  :charset-list '(iso-8859-7)
  :mime-charset 'iso-8859-7)

(define-coding-system-alias 'iso-8859-7 'greek-iso-8bit)

(define-coding-system 'windows-1253
  "windows-1253 encoding for Greek"
  :coding-type 'charset
  :mnemonic ?g
  :charset-list '(windows-1253)
  :mime-charset 'windows-1253)
(define-coding-system-alias 'cp1253 'windows-1253)

(define-coding-system 'cp851
  "DOS codepage 851 (Greek)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp851)
  :mime-charset 'cp851)
(define-coding-system-alias 'ibm851 'cp851)

(define-coding-system 'cp869
  "DOS codepage 869 (Greek)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp869)
  :mime-charset 'cp869)
(define-coding-system-alias 'ibm869 'cp869)

(set-language-info-alist
 "Greek" '((charset iso-8859-7)
	   (coding-system . (greek-iso-8bit))
	   (coding-priority greek-iso-8bit)
	   (nonascii-translation . iso-8859-7)
	   (input-method . "greek")
	   (unibyte-display . greek-iso-8bit)
	   (documentation . t)))

(provide 'greek)

;;; greek.el ends here
