;;; japanese.el --- support for Japanese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

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

(define-coding-system 'iso-2022-jp
  "ISO 2022 based 7bit encoding for Japanese (MIME:ISO-2022-JP)."
  :coding-type 'iso-2022
  :mnemonic ?J
  :designation [(ascii japanese-jisx0208-1978 japanese-jisx0208
		       latin-jisx0201 japanese-jisx0212)
		nil nil nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit designation)
  :charset-list '(ascii japanese-jisx0208-1978 japanese-jisx0208
			   latin-jisx0201 japanese-jisx0212)
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
  :charset-list '(ascii japanese-jisx0208-1978 japanese-jisx0208
			latin-jisx0201 japanese-jisx0212
			chinese-gb2312 korean-ksc5601
			latin-iso8859-1 greek-iso8859-7)
  :mime-charset 'iso-2022-jp-2)

(define-coding-system 'japanese-shift-jis
  "Shift-JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS)"
  :coding-type 'shift-jis
  :mnemonic ?S
  :charset-list '(ascii katakana-jisx0201 japanese-jisx0208)
  :mime-charset 'shift_jis)

(define-coding-system-alias 'shift_jis 'japanese-shift-jis)
(define-coding-system-alias 'sjis 'japanese-shift-jis)

(define-coding-system 'japanese-iso-7bit-1978-irv
  "ISO 2022 based 7-bit encoding for Japanese JISX0208-1978 and JISX0201-Roman."
  :coding-type 'iso-2022
  :mnemonic ?j
  :designation [(latin-jisx0201 japanese-jisx0208-1978 japanese-jisx0208
				japanese-jisx0212 katakana-jisx0201)
		nil nil nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit use-roman use-oldjis)
  :charset-list '(latin-jisx0201 japanese-jisx0208-1978 japanese-jisx0208
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

(set-language-info-alist
 "Japanese" '((setup-function . setup-japanese-environment-internal)
	      (exit-function . exit-japanese-environment)
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
