;;; utf-16.el --- UTF-16 encoding/decoding

;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: Unicode, UTF-16, i18n

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

;; Support for UTF-16, which is a two-byte encoding (modulo
;; surrogates) of Unicode, written either in little or big endian
;; order: coding-systems `mule-utf-16-le' and `mule-utf-16-be'.
;; (utf-16-le is used by the DozeN'T clipboard, for instance.)  The
;; data are preceeded by a two-byte signature which identifies their
;; byte sex.  These are used by the coding-category-utf-16-{b,l}e code
;; to identify the coding, but ignored on decoding.

;; Note that un-decodable sequences aren't (yet?) preserved as raw
;; bytes, as they are with utf-8, so reading and writing as utf-16 can
;; corrupt data.

;;; Code:

;; We end up with trivially different -le and -be versions of most
;; things below, sometimes with commonality abstracted into a let
;; binding for maintenance convenience.

;; We'd need new charsets distinct from ascii and eight-bit-control to
;; deal with untranslated sequences, since we can't otherwise
;; distinguish the bytes, as we can with utf-8.

;; ;; Do a multibyte write for bytes in r3 and r4.
;; ;; Intended for untranslatable utf-16 sequences.
;; (define-ccl-program ccl-mule-utf-16-untrans
;;   `(0
;;      (if (r3 < 128)
;; 	 (r0 = ,(charset-id 'ascii))
;;        (if (r3 < 160)
;; 	   (r0 = ,(charset-id 'eight-bit-control))
;; 	 (r0 = ,(charset-id 'eight-bit-graphic))))
;;      (if (r4 < 128)
;; 	 (r0 = ,(charset-id 'ascii))
;;        (if (r4 < 160)
;; 	   (r0 = ,(charset-id 'eight-bit-control))
;; 	 (r0 = ,(charset-id 'eight-bit-graphic))))
;;      (r1 = r4)))
;;   "Do a multibyte write for bytes in r3 and r4.
;; First swap them if we're big endian, indicated by r5==0.
;; Intended for untranslatable utf-16 sequences.")

;; Needed in macro expansion, so can't be let-bound.  Zapped after use.
(eval-and-compile
(defconst utf-16-decode-ucs
  ;; We have the unicode in r1.  Output is charset ID in r0, code
  ;; point in r1.  As r6 keeps endian information, the value should
  ;; not be changed.
  `((lookup-integer utf-subst-table-for-decode r1 r3)
    (if r7				; got a translation
	((r0 = r1) (r1 = r3))
      (if (r1 < 128)
	  (r0 = ,(charset-id 'ascii))
	(if (r1 < 160)
	    (r0 = ,(charset-id 'eight-bit-control))
	  (if (r1 < 256)
	      ((r0 = ,(charset-id 'latin-iso8859-1))
	       (r1 -= 128))
	    (if (r1 < #x2500)
		((r0 = ,(charset-id 'mule-unicode-0100-24ff))
		 (r1 -= #x100)
		 (r2 = (((r1 / 96) + 32) << 7))
		 (r1 %= 96)
		 (r1 += (r2 + 32)))
	      (if (r1 < #x3400)
		  ((r0 = ,(charset-id 'mule-unicode-2500-33ff))
		   (r1 -= #x2500)
		   (r2 = (((r1 / 96) + 32) << 7))
		   (r1 %= 96)
		   (r1 += (r2 + 32)))
		(if (r1 < #xd800)	; 2 untranslated bytes
		    ;;		 ;; Assume this is rare, so don't worry about the
		    ;; 		 ;; overhead of the call.
		    ;; 		 (call mule-utf-16-untrans)
		    ((r0 = ,(charset-id 'mule-unicode-e000-ffff))
		     (r1 = 15037))	; U+fffd
		  (if (r1 < #xe000)	; surrogate
		      ;; 			((call mule-utf-16-untrans)
		      ;; 			 (write-multibyte-character r0 r1)
		      ;; 			 (read r3 r4)
		      ;; 			 (call mule-utf-16-untrans))
		      ((read r3 r4)
		       (r0 = ,(charset-id 'mule-unicode-e000-ffff))
		       (r1 = 15037))
		    ((r0 = ,(charset-id 'mule-unicode-e000-ffff))
		     (r1 -= #xe000)
		     (r2 = (((r1 / 96) + 32) << 7))
		     (r1 %= 96)
		     (r1 += (r2 + 32))))))))))))))

(define-ccl-program ccl-decode-mule-utf-16-le
  `(2					; 2 bytes -> 1 to 4 bytes
    ((loop
      (read r3 r4)
      (r1 = (r4 <8 r3))
      ,utf-16-decode-ucs
      (translate-character utf-translation-table-for-decode r0 r1)
      (write-multibyte-character r0 r1)
      (repeat))))
  "Decode UTF-16LE (little endian without signature bytes).
Basic decoding is done into the charsets ascii, latin-iso8859-1 and
mule-unicode-*.  Un-representable Unicode characters are decoded as
U+fffd.  The result is run through the translation-table named
`utf-translation-table-for-decode'.")

(define-ccl-program ccl-decode-mule-utf-16-be
  `(2					; 2 bytes -> 1 to 4 bytes
    ((loop
      (read r3 r4)
      (r1 = (r3 <8 r4))
      ,utf-16-decode-ucs
      (translate-character utf-translation-table-for-decode r0 r1)
      (write-multibyte-character r0 r1)
      (repeat))))
  "Decode UTF-16BE (big endian without signature bytes).
Basic decoding is done into the charsets ascii, latin-iso8859-1 and
mule-unicode-*.  Un-representable Unicode characters are
decoded as U+fffd.  The result is run through the translation-table of
name `utf-translation-table-for-decode'.")

(makunbound 'utf-16-decode-ucs)		; done with it

(eval-and-compile
(defconst utf-16-decode-to-ucs
  ;; CCL which, given the result of a multibyte read in r0 and r1,
  ;; sets r0 to the character's Unicode if the charset is one of the
  ;; basic utf-8 coding system ones.  Otherwise set to U+fffd.
  `(if (r0 == ,(charset-id 'ascii))
       (r0 = r1)
     (if (r0 == ,(charset-id 'latin-iso8859-1))
	 (r0 = (r1 + 128))
       (if (r0 == ,(charset-id 'eight-bit-control))
	   (r0 = r1)
	 (if (r0 == ,(charset-id 'eight-bit-graphic))
	     (r0 = r1)
	   ((r2 = (r1 & #x7f))
	    (r1 >>= 7)
	    (r3 = ((r1 - 32) * 96))
	    (r3 += (r2 - 32))
	    (if (r0 == ,(charset-id 'mule-unicode-0100-24ff))
		(r0 = (r3 + #x100))
	      (if (r0 == ,(charset-id 'mule-unicode-2500-33ff))
		  (r0 = (r3 + #x2500))
		(if (r0 == ,(charset-id 'mule-unicode-e000-ffff))
		    (r0 = (r3 + #xe000))
		  (r0 = #xfffd)))))))))))

(define-ccl-program ccl-encode-mule-utf-16-le
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (lookup-character utf-subst-table-for-encode r0 r1)
      (if (r7 == 0)
	  ((translate-character utf-translation-table-for-encode r0 r1)
	   ,utf-16-decode-to-ucs))
      (write (r0 & 255))
      (write (r0 >> 8))
      (repeat))))
  "Encode to UTF-16LE (little endian without signature).
Characters from the charsets ascii, eight-bit-control,
eight-bit-graphic, latin-iso8859-1 and mule-unicode-* are encoded
after translation through the translation-table of name
`utf-translation-table-for-encode'.
Others are encoded as U+FFFD.")

(define-ccl-program ccl-encode-mule-utf-16-be
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (lookup-character utf-subst-table-for-encode r0 r1)
      (if (r7 == 0)
	  ((translate-character utf-translation-table-for-encode r0 r1)
	   ,utf-16-decode-to-ucs))
      (write (r0 >> 8))
      (write (r0 & 255))
      (repeat))))
  "Encode to UTF-16BE (big endian without signature).
Characters from the charsets ascii, eight-bit-control,
eight-bit-graphic, latin-iso8859-1 and mule-unicode-* are encoded
after translation through the translation-table named
`utf-translation-table-for-encode'.
Others are encoded as U+FFFD.")

(makunbound 'utf-16-decode-to-ucs)

(let ((doc "

It supports Unicode characters of these ranges:
    U+0000..U+33FF, U+E000..U+FFFF.
They correspond to these Emacs character sets:
    ascii, latin-iso8859-1, mule-unicode-0100-24ff,
    mule-unicode-2500-33ff, mule-unicode-e000-ffff

On decoding (e.g. reading a file), Unicode characters not in the above
ranges are decoded as U+FFFD, effectively corrupting the data
if they are re-encoded.

On encoding (e.g. writing a file), Emacs characters not belonging to
any of the character sets listed above are encoded into the byte
sequence representing U+FFFD (REPLACEMENT CHARACTER)."))
  (make-coding-system
   'mule-utf-16-le 4
   ?u	      ; Mule-UCS uses ?U, but code-pages uses that for koi8-u.
   (concat
    "Little endian UTF-16 encoding for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16-le . ccl-encode-mule-utf-16-le)
   '((safe-charsets
      ascii
      eight-bit-control
      latin-iso8859-1
      mule-unicode-0100-24ff
      mule-unicode-2500-33ff
      mule-unicode-e000-ffff)
     (mime-charset . utf-16le)
     (coding-category . coding-category-utf-16-le)
     (valid-codes (0 . 255))
     (dependency unify-8859-on-encoding-mode
		 unify-8859-on-decoding-mode
		 utf-fragment-on-decoding
		 utf-translate-cjk)))

  (make-coding-system
   'mule-utf-16-be 4 ?u
   (concat
    "Big endian UTF-16 encoding for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16-be . ccl-encode-mule-utf-16-be)
   '((safe-charsets
      ascii
      eight-bit-control
      latin-iso8859-1
      mule-unicode-0100-24ff
      mule-unicode-2500-33ff
      mule-unicode-e000-ffff)
     (mime-charset . utf-16be)
     (coding-category . coding-category-utf-16-be)
     (valid-codes (0 . 255))
     (dependency unify-8859-on-encoding-mode
		 unify-8859-on-decoding-mode
		 utf-fragment-on-decoding
		 utf-translate-cjk))))

(define-coding-system-alias 'utf-16-le 'mule-utf-16-le)
(define-coding-system-alias 'utf-16-be 'mule-utf-16-be)

;;; utf-16.el ends here
