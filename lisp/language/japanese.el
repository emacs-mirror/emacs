;;; japanese.el --- support for Japanese -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;;   Licensed to the Free Software Foundation.
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; For Japanese, character sets JISX0201, JISX0208, JISX0212 are
;; supported.

;;; Code:

;;; Load translation tables for CP932.
(load "international/cp51932")
(load "international/eucjp-ms")

(define-coding-system 'iso-2022-jp
  "ISO 2022 based 7bit encoding for Japanese (MIME:ISO-2022-JP)."
  :coding-type 'iso-2022
  :mnemonic ?J
  :designation [(ascii japanese-jisx0208-1978 japanese-jisx0208
		       latin-jisx0201 japanese-jisx0212)
		nil nil nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit designation)
  :charset-list '(ascii japanese-jisx0208 japanese-jisx0212
			japanese-jisx0208-1978 latin-jisx0201)
  :mime-charset 'iso-2022-jp)

(define-coding-system-alias 'junet 'iso-2022-jp)

(define-coding-system 'iso-2022-jp-2
  "ISO 2022 based 7bit encoding for CJK, Latin-1, Greek (MIME:ISO-2022-JP-2)."
  :coding-type 'iso-2022
  :mnemonic ?J
  :designation [(ascii japanese-jisx0208-1978 japanese-jisx0208
		       latin-jisx0201 japanese-jisx0212
		       chinese-gb2312 korean-ksc5601)
		nil
		(nil latin-iso8859-1 greek-iso8859-7)
		nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit designation single-shift
		 init-at-bol)
  :charset-list '(ascii japanese-jisx0208 japanese-jisx0212
			latin-jisx0201 japanese-jisx0208-1978
			chinese-gb2312 korean-ksc5601
			latin-iso8859-1 greek-iso8859-7)
  :mime-charset 'iso-2022-jp-2)

(let ((map			; JIS		vs	CP932
       '((#x301C . #xFF5E)	; WAVE DASH		FULLWIDTH TILDE
	 (#x2014 . #x2015)	; EM DASH		HORIZONTAL BAR
	 (#x2016 . #x2225)	; DOUBLE VERTICAL LINE	PARALLEL TO
	 (#x2212 . #xFF0D)	; MINUS SIGN		FULLWIDTH HYPHEN-MINUS
	 (#x00A2 . #xFFE0)	; CENT SIGN		FULLWIDTH CENT SIGN
	 (#x00A3 . #xFFE1)	; POUND SIGN		FULLWIDTH POUND SIGN
	 (#x00AC . #xFFE2)	; NOT SIGN		FULLWIDTH NOT SIGN
	 (#x00A6 . #xFFE4)	; BROKEN LINE		FULLWIDTH BROKEN LINE
	 )))
  (define-translation-table 'japanese-ucs-cp932-to-jis-map map)
  (mapc #'(lambda (x) (let ((tmp (car x)))
			(setcar x (cdr x)) (setcdr x tmp)))
	map)
  (define-translation-table 'japanese-ucs-jis-to-cp932-map map))

;; U+2014 (EM DASH) vs U+2015 (HORIZONTAL BAR)
(define-translation-table 'japanese-ucs-glibc-to-jis-map '((#x2015 . #x2014)))
(define-translation-table 'japanese-ucs-jis-to-glibc-map '((#x2014 . #x2015)))

(define-coding-system 'japanese-shift-jis
  "Shift-JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS)"
  :coding-type 'shift-jis
  :mnemonic ?S
  :charset-list '(ascii katakana-jisx0201 japanese-jisx0208)
  :mime-charset 'shift_jis)

(define-coding-system-alias 'shift_jis 'japanese-shift-jis)
(define-coding-system-alias 'sjis 'japanese-shift-jis)

(define-coding-system 'japanese-cp932
  "CP932 (Microsoft shift-jis)"
  :coding-type 'charset
  :mnemonic ?S
  :charset-list '(ascii katakana-sjis cp932-2-byte))

(define-coding-system-alias 'cp932 'japanese-cp932)

(define-coding-system 'japanese-iso-7bit-1978-irv
  "ISO 2022 based 7-bit encoding for Japanese JISX0208-1978 and JISX0201-Roman."
  :coding-type 'iso-2022
  :mnemonic ?j
  :designation [(latin-jisx0201 japanese-jisx0208-1978 japanese-jisx0208
				japanese-jisx0212 katakana-jisx0201)
		nil nil nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit designation
		 use-roman use-oldjis)
  :charset-list '(ascii latin-jisx0201 japanese-jisx0208-1978 japanese-jisx0208
			japanese-jisx0212))

(define-coding-system-alias 'iso-2022-jp-1978-irv 'japanese-iso-7bit-1978-irv)
(define-coding-system-alias 'old-jis 'japanese-iso-7bit-1978-irv)

(define-coding-system 'japanese-iso-8bit
  "ISO 2022 based EUC encoding for Japanese (MIME:EUC-JP)."
  :coding-type 'iso-2022
  :mnemonic ?E
  :designation [ascii japanese-jisx0208 katakana-jisx0201 japanese-jisx0212]
  :flags '(short ascii-at-eol ascii-at-cntl single-shift)
  :charset-list '(ascii latin-jisx0201 japanese-jisx0208
			japanese-jisx0208-1978
			katakana-jisx0201 japanese-jisx0212)
  :mime-charset 'euc-jp)

(define-coding-system-alias 'euc-japan-1990 'japanese-iso-8bit)
(define-coding-system-alias 'euc-japan 'japanese-iso-8bit)
(define-coding-system-alias 'euc-jp 'japanese-iso-8bit)

(define-coding-system 'eucjp-ms
  "eucJP-ms (like EUC-JP but with CP932 extension).
eucJP-ms is defined in <http://www.opengroup.or.jp/jvc/cde/appendix.html>."
  :coding-type 'iso-2022
  :mnemonic ?E
  :designation [ascii japanese-jisx0208 katakana-jisx0201 japanese-jisx0212]
  :flags '(short ascii-at-eol ascii-at-cntl single-shift)
  :charset-list '(ascii latin-jisx0201 japanese-jisx0208
			katakana-jisx0201 japanese-jisx0212)
  :decode-translation-table 'eucjp-ms-decode
  :encode-translation-table 'eucjp-ms-encode)

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

;;; japanese.el ends here
