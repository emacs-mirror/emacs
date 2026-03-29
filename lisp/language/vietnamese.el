;;; vietnamese.el --- support for Vietnamese -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 1998, 2001-2026 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Vietnamese, i18n

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

;; For Vietnamese, the coding systems VISCII, VSCII-1 (TCVN-5712),
;; VIQR and windows-1258 are supported.

;;; Code:

(define-coding-system 'vietnamese-viscii
  "8-bit encoding for Vietnamese VISCII 1.1 (MIME:VISCII)."
  :coding-type 'charset
  :mnemonic ?V
  :charset-list '(viscii)
  :mime-charset 'viscii
  :suitable-for-file-name t)

(define-coding-system-alias 'viscii 'vietnamese-viscii)

(define-coding-system 'vietnamese-vscii
  "8-bit encoding for Vietnamese VSCII-1 (TCVN-5712)."
  :coding-type 'charset
  :mnemonic ?v
  :charset-list '(vscii)
  :suitable-for-file-name t)

(define-coding-system-alias 'vscii 'vietnamese-vscii)
(define-coding-system-alias 'vietnamese-tcvn 'vietnamese-vscii)
(define-coding-system-alias 'tcvn 'vietnamese-vscii)
(define-coding-system-alias 'tcvn-5712 'vietnamese-vscii)

;; (make-coding-system
;;  'vietnamese-vps 4 ?p
;;  "8-bit encoding for Vietnamese VPS"
;;  '(ccl-decode-vps . ccl-encode-vps)
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (valid-codes (0 . 255))))
;;
;; (define-coding-system-alias 'vps 'vietnamese-vps)

(define-coding-system 'vietnamese-viqr
  "Vietnamese latin transcription (VIQR)."
  :coding-type 'utf-8
  :mnemonic ?q
  :charset-list '(ascii viscii)
  :post-read-conversion 'viqr-post-read-conversion
  :pre-write-conversion 'viqr-pre-write-conversion)

(define-coding-system-alias 'viqr 'vietnamese-viqr)

(set-language-info-alist
 "Vietnamese" '((charset viscii)
		(coding-system vietnamese-viscii vietnamese-vscii
			       vietnamese-viqr windows-1258)
		(nonascii-translation . viscii)
		(coding-priority vietnamese-viscii)
		(input-method . "vietnamese-viqr")
		(unibyte-display . vietnamese-viscii)
		(features viet-util)
		(sample-text . "Vietnamese (Tiếng Việt)	Chào bạn")
		(documentation . "\
For Vietnamese, Emacs uses special charsets internally.
They can be decoded from and encoded to VISCII, VSCII-1 (TCVN-5712),
VIQR and windows-1258.  The current setting gives higher priority
to the coding system VISCII than VSCII-1.  If you prefer VSCII-1,
please do: (prefer-coding-system 'vietnamese-vscii).  There are
two Vietnamese input methods: VIQR and Telex, VIQR is the default
setting.")))

(define-coding-system 'windows-1258
  "windows-1258 encoding for Vietnamese (MIME: WINDOWS-1258)"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(windows-1258)
  :mime-charset 'windows-1258)
(define-coding-system-alias 'cp1258 'windows-1258)

(provide 'vietnamese)

;;; vietnamese.el ends here
