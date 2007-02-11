;;; utf-16.el --- UTF-16 encoding/decoding

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Free Software Foundation, Inc.
;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Support for UTF-16, which is a two-byte encoding (modulo
;; surrogates) of Unicode, defined in RFC 2781.  It is written either
;; in little or big endian order and either with or without the
;; leading BOM (a two-byte signature which identifies their byte sex).
;;
;; We provide these base coding systems.
;;	name					endian	BOM
;;	----					------	---
;;	mule-utf-16le				little	no
;;	mule-utf-16be				big	no
;;	mule-utf-16le-with-signature		little	yes
;;	mule-utf-16be-with-signature		big	yes
;;	mule-utf-16				both	yes
;;
;; Note that un-decodable sequences aren't (yet?) preserved as raw
;; bytes, as they are with utf-8, so reading and writing as utf-16 can
;; corrupt data.

;;; Code:

;; We end up with trivially different -le and -be versions of most
;; things below, sometimes with commonality abstracted into a let
;; binding for maintenance convenience.

;; Needed in macro expansion, so can't be let-bound.  Zapped after use.
(eval-and-compile
(defconst utf-16-decode-ucs
  ;; If r5 is negative, r1 is a Unicode chacter code.  Otherise, r5 is
  ;; the first of a surrogate pair and r1 is the second of the pair.
  ;; Output is charset ID in r0, code point in r1.  R0 may be set to
  ;; -1 in which case a caller should not write out r1.
  `((if (r5 >= 0)
	((r0 = (r1 < #xDC00))
	 (if ((r1 >= #xE000) | r0)
	     ;; Invalid second code of surrogate pair.
	     ((r0 = r5)
	      (call ccl-mule-utf-untrans))
	   ((r1 -= #xDC00)
	    (r1 += (((r5 - #xD800) << 10) + #x10000))))
	 (r5 = -1)))
    (if (r1 < 128)
	(r0 = ,(charset-id 'ascii))
      ((lookup-integer utf-subst-table-for-decode r1 r3)
       (if r7				; got a translation
	   ((r0 = r1) (r1 = r3))
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
		 (if (r1 < #xD800)
		     ;; We can't have this character.
		     ((r0 = r1)
		      (call ccl-mule-utf-untrans)
		      (r5 = -1)
		      (r0 = -1))
		   (if (r1 < #xDC00)
		       ;; The first code of a surrogate pair.
		       ((r5 = r1)
			(r0 = -1))
		     (if (r1 < #xE000)
			 ;; The second code of a surrogate pair, invalid.
			 ((r0 = r1)
			  (call ccl-mule-utf-untrans)
			  (r5 = -1)
			  (r0 = -1))
		       (if (r1 < #x10000)
			   ((r0 = ,(charset-id 'mule-unicode-e000-ffff))
			    (r1 -= #xE000)
			    (r2 = (((r1 / 96) + 32) << 7))
			    (r1 %= 96)
			    (r1 += (r2 + 32)))
			 ;; We can't have this character.
			 ((r0 = r1)
			  (call ccl-mule-utf-untrans)
			  (r5 = -1)
			  (r0 = -1)))))))))))))))

(defconst utf-16le-decode-loop
  `((r5 = -1)
    (loop
     (r3 = -1)
     (read r3 r4)
     (r1 = (r4 <8 r3))
     ,@utf-16-decode-ucs
     (if (r0 >= 0)
	 ((translate-character utf-translation-table-for-decode r0 r1)
	  (write-multibyte-character r0 r1)))
     (repeat))))

(defconst utf-16be-decode-loop
  `((r5 = -1)
    (loop
     (r3 = -1)
     (read r3 r4)
     (r1 = (r3 <8 r4))
     ,@utf-16-decode-ucs
     (if (r0 >= 0)
	 ((translate-character utf-translation-table-for-decode r0 r1)
	  (write-multibyte-character r0 r1)))
     (repeat))))

)

(define-ccl-program ccl-decode-mule-utf-16le
  `(2					; 2 bytes -> 1 to 4 bytes
    ,utf-16le-decode-loop
    ((if (r5 >= 0)
	 ((r0 = r5)
	  (call ccl-mule-utf-untrans)))
     (if (r3 < 0)
	 nil
       ((if (r3 < #xA0)
	    (r0 = ,(charset-id 'eight-bit-control))
	  (r0 = ,(charset-id 'eight-bit-graphic)))
	(write-multibyte-character r0 r3)))))
  "Decode UTF-16LE (little endian without signature bytes).
Basic decoding is done into the charsets ascii, latin-iso8859-1 and
mule-unicode-*.  Un-representable Unicode characters are decoded as
U+fffd.  The result is run through the translation-table named
`utf-translation-table-for-decode'.")

(define-ccl-program ccl-decode-mule-utf-16be
  `(2					; 2 bytes -> 1 to 4 bytes
    ,utf-16be-decode-loop
    ((if (r5 >= 0)
	 ((r0 = r5)
	  (call ccl-mule-utf-untrans)))
     (if (r3 >= 0)
	 ((r0 = r3)
	  (call ccl-mule-utf-untrans)))))
  "Decode UTF-16BE (big endian without signature bytes).
Basic decoding is done into the charsets ascii, latin-iso8859-1 and
mule-unicode-*.  Un-representable Unicode characters are
decoded as U+fffd.  The result is run through the translation-table of
name `utf-translation-table-for-decode'.")

(define-ccl-program ccl-decode-mule-utf-16le-with-signature
  `(2
    ((r3 = -1)
     (read r3 r4)
     ,@utf-16le-decode-loop)
    (if (r3 >= 0)
	((r0 = r3)
	 (call ccl-mule-utf-untrans))))
  "Like ccl-decode-utf-16le but skip the first 2-byte BOM.")

(define-ccl-program ccl-decode-mule-utf-16be-with-signature
  `(2
    ((r3 = -1)
     (read r3 r4)
     ,@utf-16be-decode-loop)
    (if (r3 >= 0)
	((r0 = r3)
	 (call ccl-mule-utf-untrans))))
  "Like ccl-decode-utf-16be but skip the first 2-byte BOM.")

(define-ccl-program ccl-decode-mule-utf-16
  `(2
    ((r3 = -1)
     (read r3 r4)
     (r1 = (r3 <8 r4))
     (r5 = -1)
     (if (r1 == #xFFFE)
	 ;; R1 is a BOM for little endian.  We keep this character as
	 ;; is temporarily.  It is removed by post-read-conversion
	 ;; function.
	 (,@utf-16-decode-ucs
	  (write-multibyte-character r0 r1)
	  ,@utf-16le-decode-loop)
       ((if (r1 == #xFEFF)
	    ;; R1 is a BOM for big endian, but we can't keep that
	    ;; character in the output because it can't be
	    ;; distinguished with the normal U+FEFF.  So, we keep
	    ;; #xFFFF instead.
	    ((r1 = #xFFFF)
	     ,@utf-16-decode-ucs
	     (write-multibyte-character r0 r1))
	  ;; R1 is a normal Unicode character.
	  (,@utf-16-decode-ucs
	   (if (r0 >= 0)
	       ((translate-character utf-translation-table-for-decode r0 r1)
		(write-multibyte-character r0 r1)))))
	,@utf-16be-decode-loop)))
    (if (r3 >= 0)
	((r0 = r3)
	 (call ccl-mule-utf-untrans))))
  "Like ccl-decode-utf-16be/le but check the first BOM.")

(makunbound 'utf-16-decode-ucs)		; done with it
(makunbound 'utf-16le-decode-loop)
(makunbound 'utf-16be-decode-loop)

;; UTF-16 decoder generates an UTF-8 sequence represented by a
;; sequence eight-bit-control/graphic chars for an invalid byte (the
;; last byte of an odd length source) and an untranslatable character
;; (including an invalid surrogate-pair code-point).
;;
;; This CCL parses that sequence (the first byte is already in r1),
;; and if the sequence represents an untranslatable character, it sets
;; r1 to the original invalid code or untranslated Unicode character
;; code, sets r2 to -1 (to prevent r2 and r3 are written), set2 r5 to
;; -1 (to tell the caller that there's no pre-read character).
;;
;; If the sequence represents an invalid byte, it sets r1 to -1, r2 to
;; the byte, sets r3 and r5 to -1.
;;
;; Otherwise, don't change r1, set r2 and r3 to already read
;; eight-bit-control/graphic characters (if any), set r5 and r6 to the
;; last character that invalidates the UTF-8 form.
;;
;; Note: For UTF-8 validation, we only check if a character is
;; eight-bit-control/graphic or not.  It may result in incorrect
;; handling of random binary data, but such a data can't be encoded by
;; UTF-16 anyway.  At least, UTF-16 decoder doesn't generate such a
;; sequence even if a source contains invalid byte-sequence.

(define-ccl-program ccl-mule-utf-16-encode-untrans
  `(0
    ((r2 = -1)
     ;; Read the 2nd byte.
     (read-multibyte-character r5 r6)
     (r0 = (r5 != ,(charset-id 'eight-bit-control)))
     (if ((r5 != ,(charset-id 'eight-bit-graphic)) & r0)
	 ((r2 = r1)
	  (r3 = -1)
	  (r1 = -1)
	  (end)))			; invalid UTF-8

     (r3 = -1)
     (r2 = r6)
     (if (r1 <= #xE0)
	 ;; 2-byte UTF-8, i.e. originally an invalid byte.
	 ((r2 &= #x3F)
	  (r2 |= ((r1 & #x1F) << 6))
	  (r1 = -1)
	  (r5 = -1)
	  (end)))
	 
     ;; Read the 3rd byte.
     (read-multibyte-character r5 r6)
     (r0 = (r5 != ,(charset-id 'eight-bit-control)))	       
     (if ((r5 != ,(charset-id 'eight-bit-graphic)) & r0)
	 ((end)))			; invalid UTF-8

     (if (r1 < #xF0)		; valid 3-byte UTF-8
	 ((r1 = ((r1 & #x0F) << 12))
	  (r1 |= ((r2 & #x3F) << 6))
	  (r1 |= (r6 & #x3F))
	  (r2 = -1)
	  (r5 = -1)
	  (end)))

     (r3 = r6)
     ;; Read the 4th byte.
     (read-multibyte-character r5 r6)
     (r0 = (r5 != ,(charset-id 'eight-bit-control)))	       
     (if ((r5 != ,(charset-id 'eight-bit-graphic)) & r0)
	 (end))			    ; livalid UTF-8

     ;; valid 4-byte UTF-8
     (r1 = ((r1 & #x07) << 18))
     (r1 |= ((r2 & #x3F) << 12))
     (r1 |= ((r3 & #x3F) << 6))
     (r1 |= (r6 & #x3F))
     (r2 = -1)
     (r5 = -1)
     (end))

    (if (r1 >= 0)
	((write r1)
	 (if (r2 >= 0)
	     ((write r2)
	      (if (r3 >= 0)
		  (write r3))))))))

(eval-and-compile
(defconst utf-16-decode-to-ucs
  ;; Read a character and set r1 to the corresponding Unicode code.
  ;; If r5 is not negative, it means that we have already read a
  ;; character into r5 and r6.
  ;; If an invalid eight-bit-control/graphic sequence is found, r2 and
  ;; r3 may contain a byte to written out, r5 and r6 may contain a
  ;; pre-read character.  Usually they are set to -1.
  `((if (r5 < 0)
	(read-multibyte-character r0 r1)
      ((r0 = r5)
       (r1 = r6)
       (r5 = -1)))
    (lookup-character utf-subst-table-for-encode r0 r1)
    (r2 = -1)
    (if (r7 > 0)
	(r1 = r0)
      ((translate-character utf-translation-table-for-encode r0 r1)
       (if (r0 == ,(charset-id 'ascii))
	   nil
	 (if (r0 == ,(charset-id 'latin-iso8859-1))
	     (r1 += 128)
	   (if (r0 == ,(charset-id 'eight-bit-control))
	       nil
	     (if (r0 == ,(charset-id 'eight-bit-graphic))
		 (call ccl-mule-utf-16-encode-untrans)
	       ((r2 = ((r1 & #x7f) - 32))
		(r3 = ((((r1 >> 7) - 32) * 96) + r2))
		(r2 = -1)
		(r5 = -1)
		(if (r0 == ,(charset-id 'mule-unicode-0100-24ff))
		    (r1 = (r3 + #x100))
		  (if (r0 == ,(charset-id 'mule-unicode-2500-33ff))
		      (r1 = (r3 + #x2500))
		    (if (r0 == ,(charset-id 'mule-unicode-e000-ffff))
			(r1 = (r3 + #xe000))
		      (r1 = #xfffd)))))))))))))

(defconst utf-16le-encode-loop
  `((r5 = -1)
    (loop
     ,@utf-16-decode-to-ucs
     (if (r1 >= #x10000)
	 ((r1 -= #x10000)
	  (r0 = ((r1 >> 10) + #xD800))
	  (write (r0 & 255))
	  (write (r0 >> 8))
	  (r1 = ((r1 & #x3FF) + #xDC00))))
     (if (r1 >= 0)
	 ((write (r1 & 255))
	  (write (r1 >> 8))))
     (if (r2 >= 0)
	 ((write r2)
	  (if (r3 >= 0)
	      (write r3))))
     (repeat))))

(defconst utf-16be-encode-loop
  `((r5 = -1)
    (loop
     ,@utf-16-decode-to-ucs
     (if (r1 >= #x10000)
	 ((r1 -= #x10000)
	  (r0 = ((r1 >> 10) + #xD800))
	  (write (r0 >> 8))
	  (write (r0 & 255))
	  (r1 = ((r1 & #x3FF) + #xDC00))))
     (if (r1 >= 0)
	 ((write (r1 >> 8))
	  (write (r1 & 255))))
     (if (r2 >= 0)
	 ((write r2)
	  (if (r3 >= 0)
	      (write r3))))
     (repeat))))
)


(define-ccl-program ccl-encode-mule-utf-16le
  `(2
    ,utf-16le-encode-loop)
  "Encode to UTF-16LE (little endian without signature).
Characters from the charsets ascii, eight-bit-control,
eight-bit-graphic, latin-iso8859-1 and mule-unicode-* are encoded
after translation through the translation-table of name
`utf-translation-table-for-encode'.
Others are encoded as U+FFFD.")

(define-ccl-program ccl-encode-mule-utf-16be
  `(2
    ,utf-16be-encode-loop)
  "Encode to UTF-16BE (big endian without signature).
Characters from the charsets ascii, eight-bit-control,
eight-bit-graphic, latin-iso8859-1 and mule-unicode-* are encoded
after translation through the translation-table named
`utf-translation-table-for-encode'.
Others are encoded as U+FFFD.")

(define-ccl-program ccl-encode-mule-utf-16le-with-signature
  `(2
    ((write #xFF)
     (write #xFE)
     ,@utf-16le-encode-loop))
  "Encode to UTF-16 (little endian with signature).
Characters from the charsets ascii, eight-bit-control,
eight-bit-graphic, latin-iso8859-1 and mule-unicode-* are encoded
after translation through the translation-table of name
`utf-translation-table-for-encode'.
Others are encoded as U+FFFD.")

(define-ccl-program ccl-encode-mule-utf-16be-with-signature
  `(2
    ((write #xFE)
     (write #xFF)
     ,@utf-16be-encode-loop))
  "Encode to UTF-16 (big endian with signature).
Characters from the charsets ascii, eight-bit-control,
eight-bit-graphic, latin-iso8859-1 and mule-unicode-* are encoded
after translation through the translation-table named
`utf-translation-table-for-encode'.
Others are encoded as U+FFFD.")

(makunbound 'utf-16-decode-to-ucs)
(makunbound 'utf-16le-encode-loop)
(makunbound 'utf-16be-encode-loop)

(defun mule-utf-16-post-read-conversion (length)
  (when (> length 0)
    (setq length (utf-8-post-read-conversion length))
    (let ((char (following-char)))
      (cond ((= char (decode-char 'ucs #xFFFE))
	     (delete-char 1)
	     (setq last-coding-system-used
		   (coding-system-change-text-conversion
		    last-coding-system-used
		    'mule-utf-16le-with-signature))
	     (setq length (1- length)))
	    ((= char (decode-char 'ucs #xFFFF))
	     (delete-char 1)
	     (setq last-coding-system-used
		   (coding-system-change-text-conversion
		    last-coding-system-used
		    'mule-utf-16be-with-signature))
	     (setq length (1- length)))
	    (t
	     (setq last-coding-system-used 'mule-utf-16be)))))
  length)

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
sequence representing U+FFFD (REPLACEMENT CHARACTER).")
      (props `((safe-charsets
		ascii
		eight-bit-control
		eight-bit-graphic
		latin-iso8859-1
		mule-unicode-0100-24ff
		mule-unicode-2500-33ff
		mule-unicode-e000-ffff
		,@(if utf-translate-cjk-mode
		      utf-translate-cjk-charsets))
	       (valid-codes (0 . 255))
	       (mime-text-unsuitable . t)
	       (pre-write-conversion . utf-8-pre-write-conversion)
	       (dependency unify-8859-on-encoding-mode
			   unify-8859-on-decoding-mode
			   utf-fragment-on-decoding
			   utf-translate-cjk-mode))))
  (make-coding-system
   'mule-utf-16le 4
   ?u	      ; Mule-UCS uses ?U, but code-pages uses that for koi8-u.
   (concat
    "UTF-16LE encoding for Emacs-supported Unicode characters."
    doc)
   '(ccl-decode-mule-utf-16le . ccl-encode-mule-utf-16le)
   `(,@props
     (post-read-conversion . utf-8-post-read-conversion)
     (ascii-incompatible . t)
     (mime-charset . utf-16le)))

  (make-coding-system
   'mule-utf-16be 4 ?u
   (concat
    "UTF-16BE encoding for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16be . ccl-encode-mule-utf-16be)
   `(,@props
     (post-read-conversion . utf-8-post-read-conversion)
     (ascii-incompatible . t)
     (mime-charset . utf-16be)))

  (make-coding-system
   'mule-utf-16le-with-signature 4 ?u
   (concat
    "Little endian UTF-16 (with BOM) for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16le-with-signature
     . ccl-encode-mule-utf-16le-with-signature)
   `(,@props
     (post-read-conversion . utf-8-post-read-conversion)
     (coding-category . coding-category-utf-16-le)
     (ascii-incompatible . t)
     (mime-charset . utf-16)))

  (make-coding-system
   'mule-utf-16be-with-signature 4 ?u
   (concat
    "Big endian UTF-16 (with BOM) for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16be-with-signature
     . ccl-encode-mule-utf-16be-with-signature)
   `(,@props
     (post-read-conversion . utf-8-post-read-conversion)
     (coding-category . coding-category-utf-16-be)
     (ascii-incompatible . t)
     (mime-charset . utf-16)))

  (make-coding-system
   'mule-utf-16 4 ?u
   (concat
    "UTF-16 (with or without BOM) for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16 . ccl-encode-mule-utf-16be-with-signature)
   `(,@props
     (post-read-conversion . mule-utf-16-post-read-conversion)
     (coding-category . coding-category-utf-16-be)
     (ascii-incompatible . t)
     (mime-charset . utf-16)))
)

(define-coding-system-alias 'utf-16le 'mule-utf-16le)
(define-coding-system-alias 'utf-16be 'mule-utf-16be)
(define-coding-system-alias 'utf-16le-with-signature
  'mule-utf-16le-with-signature)
(define-coding-system-alias 'utf-16be-with-signature
  'mule-utf-16be-with-signature)
(define-coding-system-alias 'utf-16 'mule-utf-16)

;; For backward compatibility.
(define-coding-system-alias 'mule-utf-16-le 'mule-utf-16le-with-signature)
(define-coding-system-alias 'utf-16-le 'mule-utf-16le-with-signature)
(define-coding-system-alias 'mule-utf-16-be 'mule-utf-16be-with-signature)
(define-coding-system-alias 'utf-16-be 'mule-utf-16be-with-signature)

;;; arch-tag: 85455d46-d9c9-466d-a6f3-c3582a7367c4
;;; utf-16.el ends here
