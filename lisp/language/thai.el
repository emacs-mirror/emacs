;;; thai.el --- support for Thai -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;;   Licensed to the Free Software Foundation.
;; Copyright (C) 2002 Free Software Foundation, Inc.
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Thai, i18n

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

;; For Thai, the character set TIS620 is supported.

;;; Code:

(define-coding-system 'thai-tis620
  "8-bit encoding for ASCII (MSB=0) and Thai TIS620 (MSB=1)."
  :coding-type 'charset
  :mnemonic ?T
  :charset-list '(tis620-2533))

(define-coding-system-alias 'th-tis620 'thai-tis620)
(define-coding-system-alias 'tis620 'thai-tis620)
(define-coding-system-alias 'tis-620 'thai-tis620)

(set-language-info-alist
 "Thai" '((tutorial . "TUTORIAL.th")
	  (charset thai-tis620)
	  (coding-system thai-tis620 iso-8859-11 cp874)
	  (coding-priority thai-tis620)
	  (nonascii-translation . tis620-2533)
	  (input-method . "thai-kesmanee")
	  (unibyte-display . thai-tis620)
	  (features thai-util)
	  (sample-text
	   . (thai-compose-string
	      (copy-sequence "Thai (,T@RIRd7B(B)		,TJ0GQ1J04U1$0CQ1:(B, ,TJ0GQ1J04U10$h1P(B")))
	  (documentation . t)))

(define-coding-system 'cp874
  "DOS codepage 874 (Thai)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp874)
  :mime-charset 'cp874)
(define-coding-system-alias 'ibm874 'cp874)

(define-coding-system 'iso-8859-11
  "ISO/IEC 8859/11 (Latin/Thai)
This is the same as `thai-tis620' with the addition of no-break-space."
  :coding-type 'charset
  :mnemonic ?*
  :mime-charset 'iso-8859-11 ; not actually registered as of 2002-05-24
  :charset-list '(iso-8859-11))

;; For automatic composition.
(let ((chars ",TQTUVWXYZghijklmn(B"))
  (dotimes (i (length chars))
    (aset composition-function-table (aref chars i)
	  'thai-composition-function)))

(provide 'thai)

;;; thai.el ends here
