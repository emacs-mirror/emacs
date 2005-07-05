;;; hebrew.el --- support for Hebrew -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 2001  Free Software Foundation, Inc.
;; Copyright (C) 1995
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, Hebrew

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; For Hebrew, the character set ISO8859-8 is supported.
;; See http://www.ecma.ch/ecma1/STAND/ECMA-121.HTM.
;; Windows-1255 is also supported.

;;; Code:

(make-coding-system
 'hebrew-iso-8bit 2 ?8
 "ISO 2022 based 8-bit encoding for Hebrew (MIME:ISO-8859-8)."
 '(ascii hebrew-iso8859-8 nil nil
   nil nil nil nil nil nil nil nil t nil nil t)
 '((safe-charsets ascii hebrew-iso8859-8)
   (mime-charset . iso-8859-8)))

(define-coding-system-alias 'iso-8859-8 'hebrew-iso-8bit)

;; These are for Explicit and Implicit directionality information, as
;; defined in RFC 1556.  We don't yet support directional information
;; in bidi languages, so these aliases are a lie, especially as far as
;; iso-8859-8-e is concerned.  FIXME.
(define-coding-system-alias 'iso-8859-8-e 'hebrew-iso-8bit)
(define-coding-system-alias 'iso-8859-8-i 'hebrew-iso-8bit)

(set-language-info-alist
 "Hebrew" '((charset . (hebrew-iso8859-8))
	    (coding-priority hebrew-iso-8bit)
	    (coding-system . (hebrew-iso-8bit))
	    (nonascii-translation . hebrew-iso8859-8)
	    (input-method . "hebrew")
	    (unibyte-display . hebrew-iso-8bit)
	    (sample-text . "Hebrew	,Hylem(B")
	    (documentation . "Right-to-left writing is not yet supported.")))

(set-language-info-alist
 "Windows-1255" '((coding-priority windows-1255)
		  (coding-system windows-1255)
		  (features code-pages)
		  (documentation . "\
Support for Windows-1255 encoding, e.g. for Yiddish.
Right-to-left writing is not yet supported.")))

(provide 'hebrew)

;;; arch-tag: 3ca04f32-3f1e-498e-af46-8267498ba5d9
;;; hebrew.el ends here
