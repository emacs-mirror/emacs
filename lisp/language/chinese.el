;;; chinese.el --- Support for Chinese -*- coding: iso-2022-7bit; -*-

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

(make-coding-system
 'iso-2022-cn 2 ?C
 "ISO 2022 based 7bit encoding for Chinese GB and CNS (MIME:ISO-2022-CN)"
 '(ascii
   (nil chinese-gb2312 chinese-cns11643-1)
   (nil chinese-cns11643-2)
   nil
   nil ascii-eol ascii-cntl seven locking-shift single-shift nil nil nil
   init-bol)
 '((safe-charsets ascii chinese-gb2312 chinese-cns11643-1 chinese-cns11643-2)
   (mime-charset . iso-2022-cn)))

(define-coding-system-alias 'chinese-iso-7bit 'iso-2022-cn)

(make-coding-system
 'iso-2022-cn-ext 2 ?C
 "ISO 2022 based 7bit encoding for Chinese GB and CNS (MIME:ISO-2022-CN-EXT)"
 '(ascii
   (nil chinese-gb2312 chinese-cns11643-1)
   (nil chinese-cns11643-2)
   (nil chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
	chinese-cns11643-6 chinese-cns11643-7)
   nil ascii-eol ascii-cntl seven locking-shift single-shift nil nil nil
   init-bol)
 '((safe-charsets ascii chinese-gb2312 chinese-cns11643-1 chinese-cns11643-2
		  chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
		  chinese-cns11643-6 chinese-cns11643-7)
   (mime-charset . iso-2022-cn-ext)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese GB2312 (simplified) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'chinese-iso-8bit 2 ?c
 "ISO 2022 based EUC encoding for Chinese GB2312 (MIME:GB2312)"
 '(ascii chinese-gb2312 nil nil
   nil ascii-eol ascii-cntl nil nil nil nil)
 '((safe-charsets ascii chinese-gb2312)
   (mime-charset . gb2312)))

(define-coding-system-alias 'cn-gb-2312 'chinese-iso-8bit)
(define-coding-system-alias 'euc-china 'chinese-iso-8bit)
(define-coding-system-alias 'euc-cn 'chinese-iso-8bit)
(define-coding-system-alias 'cn-gb 'chinese-iso-8bit)
(define-coding-system-alias 'gb2312 'chinese-iso-8bit)

(make-coding-system
 'chinese-hz 0 ?z
 "Hz/ZW 7-bit encoding for Chinese GB2312 (MIME:HZ-GB-2312)"
 nil
 '((safe-charsets ascii chinese-gb2312)
   (mime-charset . hz-gb-2312)
   (post-read-conversion . post-read-decode-hz)
   (pre-write-conversion . pre-write-encode-hz)))

(define-coding-system-alias 'hz-gb-2312 'chinese-hz)
(define-coding-system-alias 'hz 'chinese-hz)

(defun post-read-decode-hz (len)
  (let ((pos (point))
	(buffer-modified-p (buffer-modified-p))
	last-coding-system-used)
    (prog1
	(decode-hz-region pos (+ pos len))
      (set-buffer-modified-p buffer-modified-p))))

(defun pre-write-encode-hz (from to)
  (let ((buf (current-buffer)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring buf from to))
    (let (last-coding-system-used)
      (encode-hz-region 1 (point-max)))
    nil))

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

(make-coding-system
 'chinese-big5 3 ?B "BIG5 8-bit encoding for Chinese (MIME:Big5)"
 nil
 '((safe-charsets ascii chinese-big5-1 chinese-big5-2)
   (mime-charset . big5)
   (charset-origin-alist (chinese-big5-1  "BIG5" encode-big5-char)
			 (chinese-big5-2  "BIG5" encode-big5-char))))

(define-coding-system-alias 'big5 'chinese-big5)
(define-coding-system-alias 'cn-big5 'chinese-big5)

;; Big5 font requires special encoding.
(define-ccl-program ccl-encode-big5-font
  `(0
    ;; In:  R0:chinese-big5-1 or chinese-big5-2
    ;;      R1:position code 1
    ;;      R2:position code 2
    ;; Out: R1:font code point 1
    ;;      R2:font code point 2
    ((r2 = ((((r1 - ?\x21) * 94) + r2) - ?\x21))
     (if (r0 == ,(charset-id 'chinese-big5-2)) (r2 += 6280))
     (r1 = ((r2 / 157) + ?\xA1))
     (r2 %= 157)
     (if (r2 < ?\x3F) (r2 += ?\x40) (r2 += ?\x62))))
  "CCL program to encode a Big5 code to code point of Big5 font.")

(setq font-ccl-encoder-alist
      (cons (cons "big5" ccl-encode-big5-font) font-ccl-encoder-alist))

(set-language-info-alist
 "Chinese-BIG5" '((charset chinese-big5-1 chinese-big5-2)
		  (coding-system chinese-big5 chinese-iso-7bit)
		  (coding-priority chinese-big5 iso-2022-cn chinese-iso-8bit)
		  (input-method . "chinese-py-punct-b5")
		  (features china-util)
		  (sample-text . "Cantonese ($(0GnM$(B,$(0N]0*Hd(B)	$(0*/=((B, $(0+$)p(B")
		  (documentation . "Support for Chinese Big5 character set."))
 '("Chinese"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese CNS11643 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-info-alist
 "Chinese-CNS" '((charset chinese-cns11643-1 chinese-cns11643-2
			  chinese-cns11643-3 chinese-cns11643-4
			  chinese-cns11643-5 chinese-cns11643-6
			  chinese-cns11643-7)
		 (coding-system iso-2022-cn)
		 (coding-priority iso-2022-cn chinese-big5 chinese-iso-8bit)
		 (features china-util)
		 (input-method . "chinese-cns-quick")
		 (documentation . "Support for Chinese CNS character sets."))
 '("Chinese"))

(provide 'chinese)

;;; chinese.el ends here
