;;; chinese.el --- support for Chinese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Chinese

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

;; For Chinese, three character sets GB2312, BIG5, and CNS11643 are
;; supported.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese (general)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-coding-system 'iso-2022-cn
 "ISO 2022 based 7bit encoding for Chinese GB and CNS (MIME:ISO-2022-CN)."
 :coding-type 'iso-2022
 :mnemonic ?C
 :charset-list '(ascii chinese-gb2312 chinese-cns11643-1 chinese-cns11643-2)
 :designation [ascii
	       (nil chinese-gb2312 chinese-cns11643-1)
	       (nil chinese-cns11643-2)
	       nil]
 :flags '(ascii-at-eol ascii-at-cntl 7-bit
		       designation locking-shift single-shift init-at-bol)
 :mime-charset 'iso-2022-cn)

(define-coding-system-alias 'chinese-iso-7bit 'iso-2022-cn)

(define-coding-system 'iso-2022-cn-ext
  "ISO 2022 based 7bit encoding for Chinese GB and CNS (MIME:ISO-2022-CN-EXT)."
  :coding-type 'iso-2022
  :mnemonic ?C
  :charset-list '(ascii
		  chinese-gb2312 chinese-cns11643-1
		  chinese-cns11643-2 chinese-cns11643-3 chinese-cns11643-4
		  chinese-cns11643-5 chinese-cns11643-6 chinese-cns11643-7)
  :designation '[ascii
		 (nil chinese-gb2312 chinese-cns11643-1)
		 (nil chinese-cns11643-2)
		 (nil chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
		      chinese-cns11643-6 chinese-cns11643-7)]
  :flags '(ascii-at-eol ascii-at-cntl 7-bit
			designation locking-shift single-shift init-at-bol)
  :mime-charset 'iso-2022-cn-ext)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese GB2312 (simplified) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-coding-system 'chinese-iso-8bit
  "ISO 2022 based EUC encoding for Chinese GB2312 (MIME:CN-GB)."
  :coding-type 'iso-2022
  :mnemonic ?c
  :charset-list '(ascii chinese-gb2312)
  :designation [ascii chinese-gb2312 nil nil]
  :mime-charset 'cn-gb)

(define-coding-system-alias 'cn-gb-2312 'chinese-iso-8bit)
(define-coding-system-alias 'euc-china 'chinese-iso-8bit)
(define-coding-system-alias 'euc-cn 'chinese-iso-8bit)
(define-coding-system-alias 'cn-gb 'chinese-iso-8bit)
(define-coding-system-alias 'gb2312 'chinese-iso-8bit)

(define-coding-system 'chinese-hz
 "Hz/ZW 7-bit encoding for Chinese GB2312 (MIME:HZ-GB-2312)."
 :coding-type 'utf-8
 :mnemonic ?z
 :charset-list '(ascii chinese-gb2312)
 :mime-charset 'hz-gb-2312
 :post-read-conversion 'post-read-decode-hz
 :pre-write-conversion 'pre-write-encode-hz)

(define-coding-system-alias 'hz-gb-2312 'chinese-hz)
(define-coding-system-alias 'hz 'chinese-hz)

(set-language-info-alist
 "Chinese-GB" '((charset chinese-gb2312 chinese-sisheng)
		(coding-system chinese-iso-8bit iso-2022-cn chinese-hz)
		(coding-priority chinese-iso-8bit chinese-big5 iso-2022-cn)
		(input-method . "chinese-py-punct")
		(features china-util)
		(sample-text . "Chinese ($AVPND(B,$AFUM(;0(B,$A::So(B)	$ADc:C(B")
		(documentation . "Support for Chinese GB2312 character set."))
 '("Chinese"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese BIG5 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-coding-system 'chinese-big5
  "BIG5 8-bit encoding for Chinese (MIME:Big5)"
  :coding-type 'charset
  :mnemonic ?B 
  :charset-list '(ascii big5)
  :mime-charset 'big5)

(define-coding-system-alias 'big5 'chinese-big5)
(define-coding-system-alias 'cn-big5 'chinese-big5)

(set-language-info-alist
 "Chinese-BIG5" '((charset chinese-big5-1 chinese-big5-2)
		  (coding-system chinese-big5 chinese-iso-7bit)
		  (coding-priority chinese-big5 iso-2022-cn chinese-iso-8bit)
		  (input-method . "chinese-py-punct-b5")
		  (features china-util)
		  (sample-text . "Cantonese ($(Gemk#(B,$(Gl]N)fc(B)	$ATg3?(B, $ADc:C(B")
		  (documentation . "Support for Chinese Big5 character set."))
 '("Chinese"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese CNS11643 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-coding-system 'euc-tw
  "ISO 2022 based EUC encoding for Chinese CNS11643."
  :coding-type 'iso-2022
  :mnemonic ?Z
  :charset-list '(ascii
		  chinese-cns11643-1
		  chinese-cns11643-2
		  chinese-cns11643-3
		  chinese-cns11643-4
		  chinese-cns11643-5
		  chinese-cns11643-6
		  chinese-cns11643-7)
  :designation [ascii chinese-cns11643-1 (chinese-cns11643-1
					  chinese-cns11643-2
					  chinese-cns11643-3
					  chinese-cns11643-4
					  chinese-cns11643-5
					  chinese-cns11643-6
					  chinese-cns11643-7) nil]
  :mime-charset 'euc-tw)

(define-coding-system-alias 'euc-taiwan 'euc-tw)

(set-language-info-alist
 "Chinese-CNS" '((charset chinese-cns11643-1 chinese-cns11643-2
			  chinese-cns11643-3 chinese-cns11643-4
			  chinese-cns11643-5 chinese-cns11643-6
			  chinese-cns11643-7)
		 (coding-system iso-2022-cn euc-tw)
		 (coding-priority iso-2022-cn euc-tw chinese-big5
				  chinese-iso-8bit)
		 (features china-util)
		 (input-method . "chinese-cns-quick")
		 ;; Fixme: presumably it won't accept big5 now.
		 (documentation . "\
Support for Chinese CNS character sets.  Note that EUC-TW coding system
accepts Big5 for input also (which is then converted to CNS)."))
 '("Chinese"))

;;; Chinese GBK

(define-coding-system 'chinese-gbk
  "GBK encoding for Chinese (MIME:GBK)."
  :coding-type 'charset
  :mnemonic ?c
  :charset-list '(chinese-gbk)
  :mime-charset 'gbk)
(define-coding-system-alias 'gbk 'chinese-gbk)
(define-coding-system-alias 'cp936 'chinese-gbk)
(define-coding-system-alias 'windows-936 'chinese-gbk)

(set-language-info-alist
 "Chinese-GBK" '((charset chinese-gbk)
		  (coding-system chinese-gbk)
		  (coding-priority gbk iso-2022-cn chinese-big5
				   chinese-iso-8bit) ; fixme?
		  (input-method . "chinese-py-punct") ; fixme?
		  (features china-util)
		  (documentation . "Support for Chinese GBK character set."))
 '("Chinese"))

;; Fixme: add HKSCS, GB18030

(provide 'chinese)

;;; chinese.el ends here
