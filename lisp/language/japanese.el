;;; japanese.el --- support for Japanese -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 1997  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1997, 1998
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, Japanese

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

;; For Japanese, character sets JISX0201, JISX0208, JISX0212 are
;; supported.

;;; Code:

(make-coding-system
 'iso-2022-jp 2 ?J
 "ISO 2022 based 7bit encoding for Japanese (MIME:ISO-2022-JP)."
 '((ascii japanese-jisx0208-1978 japanese-jisx0208
	  latin-jisx0201 japanese-jisx0212) nil nil nil
   short ascii-eol ascii-cntl seven)
 '((safe-charsets ascii japanese-jisx0208-1978 japanese-jisx0208
		  latin-jisx0201 japanese-jisx0212)
   (mime-charset . iso-2022-jp)))

(define-coding-system-alias 'junet 'iso-2022-jp)

(make-coding-system
 'iso-2022-jp-2 2 ?J
 "ISO 2022 based 7bit encoding for CJK, Latin-1, and Greek (MIME:ISO-2022-JP-2)."
 '((ascii japanese-jisx0208-1978 japanese-jisx0208
	  latin-jisx0201 japanese-jisx0212
	  chinese-gb2312 korean-ksc5601) nil
	  (nil latin-iso8859-1 greek-iso8859-7) nil
 short ascii-eol ascii-cntl seven nil single-shift nil nil nil init-bol)
 '((safe-charsets ascii japanese-jisx0208-1978 japanese-jisx0208
		  latin-jisx0201 japanese-jisx0212
		  chinese-gb2312 korean-ksc5601
		  latin-iso8859-1 greek-iso8859-7)
   (mime-charset . iso-2022-jp-2)))

(make-coding-system
 'japanese-shift-jis 1 ?S
 "Shift-JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS)."
 nil
 '((safe-charsets ascii japanese-jisx0208 japanese-jisx0208-1978
		  latin-jisx0201 katakana-jisx0201)
   (mime-charset . shift_jis)
   (charset-origin-alist (japanese-jisx0208 "SJIS" encode-sjis-char)
			 (katakana-jisx0201 "SJIS" encode-sjis-char))))

(define-coding-system-alias 'shift_jis 'japanese-shift-jis)
(define-coding-system-alias 'sjis 'japanese-shift-jis)
(define-coding-system-alias 'cp932 'japanese-shift-jis)

(make-coding-system
 'japanese-iso-7bit-1978-irv 2 ?j
 "ISO 2022 based 7-bit encoding for Japanese JISX0208-1978 and JISX0201-Roman."
 '((ascii japanese-jisx0208-1978 japanese-jisx0208
	  latin-jisx0201 japanese-jisx0212 katakana-jisx0201 t) nil nil nil
   short ascii-eol ascii-cntl seven nil nil use-roman use-oldjis)
 '(ascii japanese-jisx0208-1978 japanese-jisx0208 latin-jisx0201))

(define-coding-system-alias 'iso-2022-jp-1978-irv 'japanese-iso-7bit-1978-irv)
(define-coding-system-alias 'old-jis 'japanese-iso-7bit-1978-irv)

(make-coding-system
 'japanese-iso-8bit 2 ?E
 "ISO 2022 based EUC encoding for Japanese (MIME:EUC-JP)."
 '(ascii japanese-jisx0208 katakana-jisx0201 japanese-jisx0212
   short ascii-eol ascii-cntl nil nil single-shift)
 '((safe-charsets ascii latin-jisx0201 japanese-jisx0208 japanese-jisx0208-1978
		 katakana-jisx0201 japanese-jisx0212)
   (mime-charset . euc-jp)))

(define-coding-system-alias 'euc-japan-1990 'japanese-iso-8bit)
(define-coding-system-alias 'euc-japan 'japanese-iso-8bit)
(define-coding-system-alias 'euc-jp 'japanese-iso-8bit)

(set-language-info-alist
 "Japanese" '((setup-function . setup-japanese-environment-internal)
	      (tutorial . "TUTORIAL.ja")
	      (charset japanese-jisx0208 japanese-jisx0208-1978
		       japanese-jisx0212 latin-jisx0201 katakana-jisx0201
		       japanese-jisx0213-1 japanese-jisx0213-2)
	      (coding-system iso-2022-jp japanese-iso-8bit
			     japanese-shift-jis japanese-iso-7bit-1978-irv)
	      (coding-priority iso-2022-jp japanese-iso-8bit
			       japanese-shift-jis iso-2022-jp-2)
	      (input-method . "japanese")
	      (features japan-util)
	      (sample-text . "Japanese (日本語)	こんにちは, ｺﾝﾆﾁﾊ")
	      (documentation . t)))

(provide 'japanese)

;;; arch-tag: 450f5537-9d53-4d5e-b731-4cf116d8cbc9
;;; japanese.el ends here
