;;; persian.el  --- support for Persian	-*- coding: utf-8;-*-

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Mohsen BANAN <libre@mohsen.1.banan.byname.net>
;; X-URL: http://mohsen.1.banan.byname.net/contact

;; Keywords: multilingual, Farsi, Persian

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For Persian, the character set ISO8859-6 is supported.  (Same as Arabic)

;;; Code:

(define-coding-system 'iso-8859-6
  "ISO-8859-6 based encoding (MIME:ISO-8859-6)."
  :coding-type 'charset
  :mnemonic ?6
  :charset-list '(iso-8859-6)
  :mime-charset 'iso-8859-6)

(define-coding-system 'windows-1256
  "windows-1256 (Arabic) encoding (MIME: WINDOWS-1256)"
  :coding-type 'charset
  :mnemonic ?A
  :charset-list '(windows-1256)
  :mime-charset 'windows-1256)

(define-coding-system-alias 'cp1256 'windows-1256)

(set-language-info-alist
 "Persian" '((charset unicode)
	    (coding-system utf-8 iso-8859-6 windows-1256)
	    (coding-priority utf-8 iso-8859-6 windows-1256)
	    (input-method . "farsi-transliterate-banan")
	    (sample-text . "Persian	فارسی")
	    (documentation . "Bidirectional editing is supported.")))

(set-char-table-range
 composition-function-table
 '(#x600 . #x6FF)
 (list ["[\u0600-\u06FF]+" 0 font-shape-gstring]))

(provide 'persian)

;;; persian.el ends here
