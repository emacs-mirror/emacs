;;; japanese.el --- Japanese support

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

(make-coding-system
 'iso-2022-jp 2 ?J
 "ISO 2022 based 7bit encoding for Japanese (MIME:ISO-2022-JP)"
 '((ascii japanese-jisx0208-1978 japanese-jisx0208
	  latin-jisx0201 japanese-jisx0212 katakana-jisx0201 t) nil nil nil
   short ascii-eol ascii-cntl seven))

(define-coding-system-alias 'junet 'iso-2022-jp)

(make-coding-system
 'japanese-shift-jis 1 ?S
 "Shift-JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS)")

(define-coding-system-alias 'shift_jis 'japanese-shift-jis)
(define-coding-system-alias 'sjis 'japanese-shift-jis)

(make-coding-system
 'japanese-iso-7bit-1978-irv 2 ?j
 "ISO 2022 based 7-bit encoding for Japanese JISX0208-1978 and JISX0201-Roman"
 '((ascii japanese-jisx0208-1978 japanese-jisx0208
	  latin-jisx0201 japanese-jisx0212 katakana-jisx0201 t) nil nil nil
   short ascii-eol ascii-cntl seven nil nil use-roman use-oldjis))

(define-coding-system-alias 'iso-2022-jp-1978-irv 'japanese-iso-7bit-1978-irv)
(define-coding-system-alias 'old-jis 'japanese-iso-7bit-1978-irv)

(make-coding-system
 'japanese-iso-8bit 2 ?E
 "ISO 2022 based EUC encoding for Japanese (MIME:EUC-JP)"
 '((ascii latin-jisx0201) (japanese-jisx0208 japanese-jisx0208-1978)
   katakana-jisx0201 japanese-jisx0212
   short ascii-eol ascii-cntl nil nil single-shift))

(define-coding-system-alias 'euc-japan-1990 'japanese-iso-8bit)
(define-coding-system-alias 'euc-japan 'japanese-iso-8bit)
(define-coding-system-alias 'euc-jp 'japanese-iso-8bit)

(set-language-info-alist
 "Japanese" '((setup-function . setup-japanese-environment)
	      (tutorial . "TUTORIAL.jp")
	      (charset . (japanese-jisx0208 japanese-jisx0208-1978
			  japanese-jisx0212 latin-jisx0201
			  katakana-jisx0201))
	      (coding-system . (iso-2022-jp japanese-iso-8bit
				japanese-shift-jis japanese-iso-7bit-1978-irv))
	      (sample-text . "Japanese (日本語)		こんにちは, ｺﾝﾆﾁﾊ")
	      (documentation . t)))

;;; japanese.el ends here
