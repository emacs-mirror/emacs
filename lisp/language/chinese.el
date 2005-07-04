;;; chinese.el --- support for Chinese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2003  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1997, 1998
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; For Chinese, three character sets GB2312, BIG5, and CNS11643 are
;; supported.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese (general)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'iso-2022-cn 2 ?C
 "ISO 2022 based 7bit encoding for Chinese GB and CNS (MIME:ISO-2022-CN)."
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
 "ISO 2022 based 7bit encoding for Chinese GB and CNS (MIME:ISO-2022-CN-EXT)."
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
 "ISO 2022 based EUC encoding for Chinese GB2312 (MIME:GB2312)."
 '(ascii chinese-gb2312 nil nil
   nil ascii-eol ascii-cntl nil nil nil nil)
 '((safe-charsets ascii chinese-gb2312)
   (mime-charset . gb2312)))

(define-coding-system-alias 'cn-gb-2312 'chinese-iso-8bit)
(define-coding-system-alias 'euc-china 'chinese-iso-8bit)
(define-coding-system-alias 'euc-cn 'chinese-iso-8bit)
(define-coding-system-alias 'cn-gb 'chinese-iso-8bit)
(define-coding-system-alias 'gb2312 'chinese-iso-8bit)
(define-coding-system-alias 'cp936 'chinese-iso-8bit)

(make-coding-system
 'chinese-hz 0 ?z
 "Hz/ZW 7-bit encoding for Chinese GB2312 (MIME:HZ-GB-2312)."
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
		(documentation . "Support for Chinese GB2312 character set.")
		(tutorial . "TUTORIAL.cn"))
 '("Chinese"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese BIG5 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'chinese-big5 3 ?B
 "BIG5 8-bit encoding for Chinese (MIME:Big5)."
 nil
 '((safe-charsets ascii chinese-big5-1 chinese-big5-2)
   (mime-charset . big5)
   (charset-origin-alist (chinese-big5-1  "BIG5" encode-big5-char)
			 (chinese-big5-2  "BIG5" encode-big5-char))))

(define-coding-system-alias 'big5 'chinese-big5)
(define-coding-system-alias 'cn-big5 'chinese-big5)
(define-coding-system-alias 'cp950 'chinese-big5)

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
		  (documentation . "Support for Chinese Big5 character set.")
		  (tutorial . "TUTORIAL.zh"))
 '("Chinese"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese CNS11643 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar big5-to-cns (make-translation-table)
  "Translation table for encoding to `euc-tw'.")
;; Could have been done by china-util loaded before.
(unless (get 'big5-to-cns 'translation-table)
  (define-translation-table 'big5-to-cns big5-to-cns))

(define-ccl-program ccl-decode-euc-tw
  ;; CNS plane 1 needs either two or four bytes in EUC-TW encoding;
  ;; CNS planes 2 to 7 always need four bytes.  In internal encoding of
  ;; Emacs, CNS planes 1 and 2 need three bytes, and planes 3 to 7 need
  ;; four bytes.  Thus a buffer magnification value of 2 (for both
  ;; encoding and decoding) is sufficient.
  `(2
    ;; we don't have enough registers to hold all charset-ids
    ((r4 = ,(charset-id 'chinese-cns11643-1))
     (r5 = ,(charset-id 'chinese-cns11643-2))
     (r6 = ,(charset-id 'chinese-cns11643-3))
     (loop
      (read-if (r0 < #x80)
	  ;; ASCII
	  (write-repeat r0)
	;; not ASCII
	(if (r0 == #x8E)
	    ;; single shift
	    (read-if (r1 < #xA1)
		;; invalid byte
		((write r0)
		 (write-repeat r1))
	      (if (r1 > #xA7)
		  ;; invalid plane
		  ((write r0)
		   (write-repeat r1))
		;; OK, we have a plane
		(read-if (r2 < #xA1)
		    ;; invalid first byte
		    ((write r0 r1)
		     (write-repeat r2))
		  (read-if (r3 < #xA1)
		      ;; invalid second byte
		      ((write r0 r1 r2)
		       (write-repeat r3))
		    ;; CNS 1-7, finally
		    ((branch (r1 - #xA1)
		      (r1 = r4)
		      (r1 = r5)
		      (r1 = r6)
		      (r1 = ,(charset-id 'chinese-cns11643-4))
		      (r1 = ,(charset-id 'chinese-cns11643-5))
		      (r1 = ,(charset-id 'chinese-cns11643-6))
		      (r1 = ,(charset-id 'chinese-cns11643-7)))
		     (r2 = ((((r2 - #x80) << 7) + r3) - #x80))
		     (write-multibyte-character r1 r2)
		     (repeat))))))
	  ;; standard EUC
	  (if (r0 < #xA1)
	      ;; invalid first byte
	      (write-repeat r0)
	    (read-if (r1 < #xA1)
		;; invalid second byte
		((write r0)
		 (write-repeat r1))
	      ;; CNS 1, finally
	      ((r1 = ((((r0 - #x80) << 7) + r1) - #x80))
	       (write-multibyte-character r4 r1)
	       (repeat)))))))))
  "CCL program to decode EUC-TW encoding."
)

(define-ccl-program ccl-encode-euc-tw
  `(2
    ;; we don't have enough registers to hold all charset-ids
    ((r2 = ,(charset-id 'ascii))
     (r3 = ,(charset-id 'chinese-big5-1))
     (r4 = ,(charset-id 'chinese-big5-2))
     (r5 = ,(charset-id 'chinese-cns11643-1))
     (r6 = ,(charset-id 'chinese-cns11643-2))
     (loop
      (read-multibyte-character r0 r1)
      (if (r0 == r2)
	  (write-repeat r1)
	(;; Big 5 encoded characters are first translated to CNS
	 (if (r0 == r3)
	     (translate-character big5-to-cns r0 r1)
	   (if (r0 == r4)
	       (translate-character big5-to-cns r0 r1)))
	 (if (r0 == r5)
	     (r0 = #xA1)
	   (if (r0 == r6)
	       (r0 = #xA2)
	     (if (r0 == ,(charset-id 'chinese-cns11643-3))
		 (r0 = #xA3)
	       (if (r0 == ,(charset-id 'chinese-cns11643-4))
		   (r0 = #xA4)
		 (if (r0 == ,(charset-id 'chinese-cns11643-5))
		     (r0 = #xA5)
		   (if (r0 == ,(charset-id 'chinese-cns11643-6))
		       (r0 = #xA6)
		     (if (r0 == ,(charset-id 'chinese-cns11643-7))
			 (r0 = #xA7)
		       ;; not CNS.  We use a dummy character which
		       ;; can't occur in EUC-TW encoding to indicate
		       ;; this.
		       (write-repeat #xFF))))))))))
      (if (r0 != #xA1)
	  ;; single shift and CNS plane
	  ((write #x8E)
	   (write r0)))
      (write ((r1 >> 7) + #x80))
      (write ((r1 % #x80) + #x80))
      (repeat))))
  "CCL program to encode EUC-TW encoding."
)

(defun euc-tw-pre-write-conversion (beg end)
  "Semi-dummy pre-write function effectively to autoload china-util."
  ;; Ensure translation table is loaded.
  (require 'china-util)
  ;; Don't do this again.
  (coding-system-put 'euc-tw 'pre-write-conversion nil)
  nil)

(make-coding-system
  'euc-tw 4 ?Z
  "ISO 2022 based EUC encoding for Chinese CNS11643.
Big5 encoding is accepted for input also (which is then converted to CNS)."
  '(ccl-decode-euc-tw . ccl-encode-euc-tw)
  '((safe-charsets ascii
		   chinese-big5-1
		   chinese-big5-2
		   chinese-cns11643-1
		   chinese-cns11643-2
		   chinese-cns11643-3
		   chinese-cns11643-4
		   chinese-cns11643-5
		   chinese-cns11643-6
		   chinese-cns11643-7)
    (valid-codes (0 . 255))
    (pre-write-conversion . euc-tw-pre-write-conversion)))

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
		 (documentation . "\
Support for Chinese CNS character sets.  Note that the EUC-TW coding system
accepts Big5 for input also (which is then converted to CNS)."))
 '("Chinese"))

(set-language-info-alist
 "Chinese-EUC-TW" '((charset chinese-cns11643-1 chinese-cns11643-2
			     chinese-cns11643-3 chinese-cns11643-4
			     chinese-cns11643-5 chinese-cns11643-6
			     chinese-cns11643-7 chinese-big5-1 chinese-big5-2)
		    (coding-system euc-tw iso-2022-cn)
		    (coding-priority euc-tw chinese-big5 iso-2022-cn
				     chinese-iso-8bit)
		    (features china-util)
		    (input-method . "chinese-cns-quick")
		    (documentation . "\
Support for Chinese, prefering the EUC-TW character set.  Note that
the EUC-TW coding system accepts Big5 for input also (which is then
converted to CNS)."))
 '("Chinese"))

(provide 'chinese)

;;; arch-tag: b82fcf7a-84f6-4e0b-b38c-1742dac0e09f
;;; chinese.el ends here
