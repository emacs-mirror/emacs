;;; romanian.el --- support for Romanian -*- coding: iso-latin-2; -*-

;; Copyright (C) 1998, 2002 Free Software Foundation.

;; Author:    Dan Nicolaescu <done@ece.arizona.edu>
;; Keywords: multilingual, Romanian, i18n

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

;; Romanian ISO 8859-2 environment plus 8859-16 coding system.

;;; Code:

(set-language-info-alist
 "Romanian" '((charset . (ascii latin-iso8859-2))
	      (coding-system . (iso-8859-2))
	      (coding-priority . (iso-8859-2))
	      (nonascii-translation . iso-8859-2)
	      (input-method . "latin-2-postfix")
	      (unibyte-syntax . "latin-2")
	      (unibyte-display . iso-8859-2)
	      (tutorial . "TUTORIAL.ro")
	      (sample-text . "Bunã ziua, bine aþi venit!")
	      (documentation . "Rmoanian environment using Latin-2 encoding.
An environment for generic Latin-10 encoding is also available."))
 '("European"))

(define-coding-system 'iso-latin-10
  "ISO 2022 based 8-bit encoding for Latin-10."
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(iso-885916)
  :mime-charset 'iso-885916)

(define-coding-system-alias 'iso-885916 'iso-latin-10)
(define-coding-system-alias 'latin-10 'iso-latin-10)

(provide 'romanian)

;;; romanian.el ends here
