;;; vietnamese.el --- support for Vietnamese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Vietnamese

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

;; For Vietnames, the character sets VISCII and VSCII are supported.

;;; Code:

(define-coding-system 'vietnamese-viscii
  "8-bit encoding for Vietnamese VISCII 1.1 (MIME:VISCII)."
  :coding-type 'charset
  :mnemonic ?V
  :charset-list '(viscii)
  :mime-charset 'viscii)

(define-coding-system-alias 'viscii 'vietnamese-viscii)

(define-coding-system 'vietnamese-vscii
  "8-bit encoding for Vietnamese VSCII-1."
  :coding-type 'charset
  :mnemonic ?v
  :charset-list '(vscii))

(define-coding-system-alias 'vscii 'vietnamese-vscii)

(define-coding-system 'vietnamese-viqr
  "Vietnamese latin transcription (VIQR)."
  :coding-type 'utf-8
  :mnemonic ?q
  :charset-list '(ascii viscii)
  :post-read-conversion 'viqr-post-read-conversion
  :pre-write-conversion 'viqr-pre-write-conversion)

(define-coding-system-alias 'viqr 'vietnamese-viqr)

(setq font-ccl-encoder-alist
      (cons '("viscii" . ccl-encode-viscii-font) font-ccl-encoder-alist))

(setq font-ccl-encoder-alist
      (cons '("vscii" . ccl-encode-vscii-font) font-ccl-encoder-alist))

(set-language-info-alist
 "Vietnamese" `((charset viscii)
		(coding-system vietnamese-viscii vietnamese-vscii
			       vietnamese-viqr)
		(coding-priority vietnamese-viscii)
		(nonascii-translation . viscii)
		(input-method . "vietnamese-viqr")
		(unibyte-display . vietnamese-viscii)
		(features viet-util)
		(sample-text . "Vietnamese (Ti,1*(Bng Vi,1.(Bt)	Ch,1`(Bo b,1U(Bn")
		(documentation . "\
For Vietnamese, Emacs uses special character sets internally.
They can be decoded from and encoded to VISCII, VSCII, and VIQR.
Current setting put higher priority to the coding system VISCII than VSCII.
If you prefer VSCII, please do: (prefer-coding-system 'vietnamese-vscii)")
		))

(provide 'vietnamese)

;;; vietnamese.el ends here
