;;; greek.el --- support for Greek -*- no-byte-compile: t -*-

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; For Greek, the character set ISO8859-7 is supported.

;;; Code:

(make-coding-system
 'greek-iso-8bit 2 ?7
 "ISO 2022 based 8-bit encoding for Greek (MIME:ISO-8859-7)."
 '(ascii greek-iso8859-7 nil nil
   nil nil nil nil nil nil nil nil nil nil nil t)
 '((safe-charsets ascii greek-iso8859-7)
   (mime-charset . iso-8859-7)))

(define-coding-system-alias 'iso-8859-7 'greek-iso-8bit)

(set-language-info-alist
 "Greek" '((charset . (greek-iso8859-7))
	   (coding-system . (greek-iso-8bit))
	   (coding-priority greek-iso-8bit)
	   (nonascii-translation . greek-iso8859-7)
	   (input-method . "greek")
	   (unibyte-display . greek-iso-8bit)
	   (documentation . t)))

(provide 'greek)

;;; arch-tag: 9ba48d79-84bc-45e1-9318-685dc3921410
;;; greek.el ends here
