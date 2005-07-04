;;; korean.el --- support for Korean -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 1998  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1998
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, Korean

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

;; For Korean, the character set KSC5601 is supported.

;;; Code:

(make-coding-system
 'korean-iso-8bit 2 ?K
 "ISO 2022 based EUC encoding for Korean KSC5601 (MIME:EUC-KR)."
 '(ascii korean-ksc5601 nil nil
   nil ascii-eol ascii-cntl)
 '((safe-charsets ascii korean-ksc5601)
   (mime-charset . euc-kr)))

(define-coding-system-alias 'euc-kr 'korean-iso-8bit)
(define-coding-system-alias 'euc-korea 'korean-iso-8bit)
(define-coding-system-alias 'cp949 'korean-iso-8bit)

(make-coding-system
 'iso-2022-kr 2 ?k
 "ISO 2022 based 7-bit encoding for Korean KSC5601 (MIME:ISO-2022-KR)."
 '(ascii (nil korean-ksc5601) nil nil
	 nil ascii-eol ascii-cntl seven locking-shift nil nil nil nil nil
	 designation-bol)
 '((safe-charsets ascii korean-ksc5601)
   (mime-charset . iso-2022-kr)))

(define-coding-system-alias 'korean-iso-7bit-lock 'iso-2022-kr)

(set-language-info-alist
 "Korean" '((setup-function . setup-korean-environment-internal)
	    (exit-function . exit-korean-environment)
	    (tutorial . "TUTORIAL.ko")
	    (charset korean-ksc5601)
	    (coding-system iso-2022-kr korean-iso-8bit)
	    (input-method . "korean-hangul")
	    (features korea-util)
	    (coding-priority korean-iso-8bit iso-2022-kr)
	    (sample-text . "Hangul (한글)	안녕하세요, 안녕하십니까")
	    (documentation . "\
The following key bindings are available while using Korean input methods:
  Shift-SPC:	toggle-korean-input-mthod
  Control-F9:	quail-hangul-switch-symbol-ksc
  F9:		quail-hangul-switch-hanja")
	    ))

(provide 'korean)

;;; arch-tag: ca7c7348-5ca3-4623-887a-7fd33d725d0e
;;; korean.el ends here
