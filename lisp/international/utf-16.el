;;; utf-16.el --- UTF-16 encoding/decoding

;; Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.

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
  ;; point in r1.
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
		     (r1 += (r2 + 32)))))))))))))

(defconst utf-16le-decode-loop
  `(loop
    (read r3 r4)
    (r1 = (r4 <8 r3))
    ,utf-16-decode-ucs
    (translate-character utf-translation-table-for-decode r0 r1)
    (write-multibyte-character r0 r1)
    (repeat)))

(defconst utf-16be-decode-loop
  `(loop
    (read r3 r4)
    (r1 = (r3 <8 r4))
    ,@utf-16-decode-ucs
    (translate-character utf-translation-table-for-decode r0 r1)
    (write-multibyte-character r0 r1)
    (repeat)))

)

(define-ccl-program ccl-decode-mule-utf-16le
  `(2					; 2 bytes -> 1 to 4 bytes
    ,utf-16le-decode-loop)
  "Decode UTF-16LE (little endian without signature bytes).
Basic decoding is done into the charsets ascii, latin-iso8859-1 and
mule-unicode-*.  Un-representable Unicode characters are decoded as
U+fffd.  The result is run through the translation-table named
`utf-translation-table-for-decode'.")

(define-ccl-program ccl-decode-mule-utf-16be
  `(2					; 2 bytes -> 1 to 4 bytes
    ,utf-16be-decode-loop)
  "Decode UTF-16BE (big endian without signature bytes).
Basic decoding is done into the charsets ascii, latin-iso8859-1 and
mule-unicode-*.  Un-representable Unicode characters are
decoded as U+fffd.  The result is run through the translation-table of
name `utf-translation-table-for-decode'.")

(define-ccl-program ccl-decode-mule-utf-16le-with-signature
  `(2
    ((read r3 r4)
     ,utf-16le-decode-loop))
  "Like ccl-decode-utf-16le but skip the first 2-byte BOM.")

(define-ccl-program ccl-decode-mule-utf-16be-with-signature
  `(2
    ((read r3 r4)
     ,utf-16be-decode-loop))
  "Like ccl-decode-utf-16be but skip the first 2-byte BOM.")

(define-ccl-program ccl-decode-mule-utf-16
  `(2
    ((read r3 r4)
     (r1 = (r3 <8 r4))
     (if (r1 == #xFFFE)
	 ;; R1 is a BOM for little endian.  We keep this character as
	 ;; is temporarily.  It is removed by post-read-conversion
	 ;; function.
	 (,@utf-16-decode-ucs
	  (write-multibyte-character r0 r1)
	  ,utf-16le-decode-loop)
       ((if (r1 == #xFEFF)
	    ;; R1 is a BOM for big endian, but we can't keep that
	    ;; character in the output because it can't be
	    ;; distinguished with the normal U+FEFF.  So, we keep
	    ;; #xFFFF instead.
	    ((r1 = #xFFFF)
	     ,@utf-16-decode-ucs)
	  ;; R1 a normal Unicode character.
	  (,@utf-16-decode-ucs
	   (translate-character utf-translation-table-for-decode r0 r1)))
	(write-multibyte-character r0 r1)
	,utf-16be-decode-loop))))
  "Like ccl-decode-utf-16be/le but check the first BOM.")

(makunbound 'utf-16-decode-ucs)		; done with it
(makunbound 'utf-16le-decode-loop)
(makunbound 'utf-16be-decode-loop)

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
		  (r0 = #xfffd))))))))))

(defconst utf-16le-encode-loop
  `(loop
    (read-multibyte-character r0 r1)
    (lookup-character utf-subst-table-for-encode r0 r1)
    (if (r7 == 0)
	((translate-character utf-translation-table-for-encode r0 r1)
	 ,utf-16-decode-to-ucs))
    (write (r0 & 255))
    (write (r0 >> 8))
    (repeat)))

(defconst utf-16be-encode-loop
  `(loop
    (read-multibyte-character r0 r1)
    (lookup-character utf-subst-table-for-encode r0 r1)
    (if (r7 == 0)
	((translate-character utf-translation-table-for-encode r0 r1)
	 ,utf-16-decode-to-ucs))
    (write (r0 >> 8))
    (write (r0 & 255))
    (repeat)))
)


(define-ccl-program ccl-encode-mule-utf-16le
  `(1
    ,utf-16le-encode-loop)
  "Encode to UTF-16LE (little endian without signature).
Characters from the charsets ascii, eight-bit-control,
eight-bit-graphic, latin-iso8859-1 and mule-unicode-* are encoded
after translation through the translation-table of name
`utf-translation-table-for-encode'.
Others are encoded as U+FFFD.")

(define-ccl-program ccl-encode-mule-utf-16be
  `(1
    ,utf-16be-encode-loop)
  "Encode to UTF-16BE (big endian without signature).
Characters from the charsets ascii, eight-bit-control,
eight-bit-graphic, latin-iso8859-1 and mule-unicode-* are encoded
after translation through the translation-table named
`utf-translation-table-for-encode'.
Others are encoded as U+FFFD.")

(define-ccl-program ccl-encode-mule-utf-16le-with-signature
  `(1
    ((write #xFF)
     (write #xFE)
     ,utf-16le-encode-loop))
  "Encode to UTF-16 (little endian with signature).
Characters from the charsets ascii, eight-bit-control,
eight-bit-graphic, latin-iso8859-1 and mule-unicode-* are encoded
after translation through the translation-table of name
`utf-translation-table-for-encode'.
Others are encoded as U+FFFD.")

(define-ccl-program ccl-encode-mule-utf-16be-with-signature
  `(1
    ((write #xFE)
     (write #xFF)
     ,utf-16be-encode-loop))
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
sequence representing U+FFFD (REPLACEMENT CHARACTER)."))
  (make-coding-system
   'mule-utf-16le 4
   ?u	      ; Mule-UCS uses ?U, but code-pages uses that for koi8-u.
   (concat
    "UTF-16LE encoding for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16le . ccl-encode-mule-utf-16le)
   '((safe-charsets
      ascii
      eight-bit-control
      latin-iso8859-1
      mule-unicode-0100-24ff
      mule-unicode-2500-33ff
      mule-unicode-e000-ffff)
     (mime-charset . utf-16le)
     (mime-text-unsuitable . t)
     (valid-codes (0 . 255))
     (dependency unify-8859-on-encoding-mode
		 unify-8859-on-decoding-mode
		 utf-fragment-on-decoding
		 utf-translate-cjk)))

  (make-coding-system
   'mule-utf-16be 4 ?u
   (concat
    "UTF-16BE encoding for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16be . ccl-encode-mule-utf-16be)
   '((safe-charsets
      ascii
      eight-bit-control
      latin-iso8859-1
      mule-unicode-0100-24ff
      mule-unicode-2500-33ff
      mule-unicode-e000-ffff)
     (mime-charset . utf-16be)
     (valid-codes (0 . 255))
     (dependency unify-8859-on-encoding-mode
		 unify-8859-on-decoding-mode
		 utf-fragment-on-decoding
		 utf-translate-cjk)))

  (make-coding-system
   'mule-utf-16le-with-signature 4 ?u
   (concat
    "Little endian UTF-16 (with BOM) for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16le-with-signature
     . ccl-encode-mule-utf-16le-with-signature)
   '((safe-charsets
      ascii
      eight-bit-control
      latin-iso8859-1
      mule-unicode-0100-24ff
      mule-unicode-2500-33ff
      mule-unicode-e000-ffff)
     (coding-category . coding-category-utf-16-le)
     (mime-charset . utf-16)
     (mime-text-unsuitable . t)
     (valid-codes (0 . 255))
     (dependency unify-8859-on-encoding-mode
		 unify-8859-on-decoding-mode
		 utf-fragment-on-decoding
		 utf-translate-cjk)))

  (make-coding-system
   'mule-utf-16be-with-signature 4 ?u
   (concat
    "Big endian UTF-16 (with BOM) for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16be-with-signature
     . ccl-encode-mule-utf-16be-with-signature)
   '((safe-charsets
      ascii
      eight-bit-control
      latin-iso8859-1
      mule-unicode-0100-24ff
      mule-unicode-2500-33ff
      mule-unicode-e000-ffff)
     (coding-category . coding-category-utf-16-be)
     (mime-charset . utf-16)
     (valid-codes (0 . 255))
     (dependency unify-8859-on-encoding-mode
		 unify-8859-on-decoding-mode
		 utf-fragment-on-decoding
		 utf-translate-cjk)))

  (make-coding-system
   'mule-utf-16 4 ?u
   (concat
    "UTF-16 (with or without BOM) for Emacs-supported Unicode characters."
    doc)

   '(ccl-decode-mule-utf-16 . ccl-encode-mule-utf-16be-with-signature)
   '((safe-charsets
      ascii
      eight-bit-control
      latin-iso8859-1
      mule-unicode-0100-24ff
      mule-unicode-2500-33ff
      mule-unicode-e000-ffff)
     (coding-category . coding-category-utf-16-be)
     (mime-charset . utf-16)
     (mime-text-unsuitable . t)
     (valid-codes (0 . 255))
     (dependency unify-8859-on-encoding-mode
		 unify-8859-on-decoding-mode
		 utf-fragment-on-decoding
		 utf-translate-cjk)
     (post-read-conversion . mule-utf-16-post-read-conversion)))
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
