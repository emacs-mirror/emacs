;;; utf-8.el --- limited UTF-8 decoding/encoding support

;; Copyright (C) 2001 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.

;; Author: TAKAHASHI Naoto  <ntakahas@m17n.org>
;; Maintainer: FSF
;; Keywords: multilingual, Unicode, UTF-8, i18n

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

;; The coding-system `mule-utf-8' basically supports encoding/decoding
;; of the following character sets to and from UTF-8:
;;
;;   ascii
;;   eight-bit-control
;;   latin-iso8859-1
;;   mule-unicode-0100-24ff
;;   mule-unicode-2500-33ff
;;   mule-unicode-e000-ffff
;;
;; On decoding, Unicode characters that do not fit into the above
;; character sets are handled as `eight-bit-control' or
;; `eight-bit-graphic' characters to retain the information about the
;; original byte sequence and text properties record the corresponding
;; unicode.
;;
;; Fixme: note that reading and writing invalid utf-8 may not be
;; idempotent -- to represent the bytes to fix that needs a new charset.
;;
;; Characters from other character sets can be encoded with mule-utf-8
;; by populating the translation-table
;; `utf-translation-table-for-encode' and registering the translation
;; with `register-char-codings'.

;; UTF-8 is defined in RFC 2279.  A sketch of the encoding is:

;;        scalar       |               utf-8
;;        value        | 1st byte  | 2nd byte  | 3rd byte
;; --------------------+-----------+-----------+----------
;; 0000 0000 0xxx xxxx | 0xxx xxxx |           |
;; 0000 0yyy yyxx xxxx | 110y yyyy | 10xx xxxx |
;; zzzz yyyy yyxx xxxx | 1110 zzzz | 10yy yyyy | 10xx xxxx

;;; Code:

(defvar ucs-mule-to-mule-unicode (make-char-table 'translation-table nil)
  "Char table mapping characters to latin-iso8859-1 or mule-unicode-*.

If `unify-8859-on-encoding-mode' is non-nil, this table populates the
translation-table named `utf-translation-table-for-encode'.")

(define-translation-table 'utf-translation-table-for-encode)

(define-ccl-program ccl-decode-mule-utf-8
  ;;
  ;;        charset         | bytes in utf-8 | bytes in emacs
  ;; -----------------------+----------------+---------------
  ;;         ascii          |       1        |       1
  ;; -----------------------+----------------+---------------
  ;;    eight-bit-control   |       2        |       2
  ;;    eight-bit-graphic   |       2        |       1
  ;;     latin-iso8859-1    |       2        |       2
  ;; -----------------------+----------------+---------------
  ;; mule-unicode-0100-24ff |       2        |       4
  ;;        (< 0800)        |                |
  ;; -----------------------+----------------+---------------
  ;; mule-unicode-0100-24ff |       3        |       4
  ;;        (>= 8000)       |                |
  ;; mule-unicode-2500-33ff |       3        |       4
  ;; mule-unicode-e000-ffff |       3        |       4
  ;;
  ;; Thus magnification factor is two.
  ;;
  `(2
    ((r5 = ,(charset-id 'eight-bit-control))
     (r6 = ,(charset-id 'eight-bit-graphic))
     (loop
      (read r0)

      ;; 1byte encoding, i.e., ascii
      (if (r0 < #x80)
	  (write r0)
	(if (r0 < #xc0)		    ; continuation byte (invalid here)
	    (if (r0 < #xa0)
		(write-multibyte-character r5 r0)
	      (write-multibyte-character r6 r0))
	  ;; 2 byte encoding 00000yyyyyxxxxxx = 110yyyyy 10xxxxxx
	  (if (r0 < #xe0)
	      ((read r1)

	       (if ((r1 & #b11000000) != #b10000000)
		   ;; Invalid 2-byte sequence
		   ((if (r0 < #xa0)
			(write-multibyte-character r5 r0)
		      (write-multibyte-character r6 r0))
		    (if (r1 < #x80)
			(write r1)
		      (if (r1 < #xa0)
			  (write-multibyte-character r5 r1)
			(write-multibyte-character r6 r1))))

		 ((r3 = r0)	   ; save in case of overlong sequence
		  (r2 = r1)
		  (r0 &= #x1f)
		  (r0 <<= 6)
		  (r1 &= #x3f)
		  (r1 += r0)
		  ;; Now r1 holds scalar value

		  (if (r1 < 128)	; `overlong sequence'
		      ((if (r3 < #xa0)
			   (write-multibyte-character r5 r3)
			 (write-multibyte-character r6 r3))
		       (if (r2 < #x80)
			   (write r2)
			 (if (r2 < #xa0)
			     (write-multibyte-character r5 r2)
			   (write-multibyte-character r6 r2))))

		    ;; eight-bit-control
		    (if (r1 < 160)
			((write-multibyte-character r5 r1))

		      ;; latin-iso8859-1
		      (if (r1 < 256)
			  ((r0 = ,(charset-id 'latin-iso8859-1))
			   (r1 -= 128)
			   (write-multibyte-character r0 r1))

			;; mule-unicode-0100-24ff (< 0800)
			((r0 = ,(charset-id 'mule-unicode-0100-24ff))
			 (r1 -= #x0100)
			 (r2 = (((r1 / 96) + 32) << 7))
			 (r1 %= 96)
			 (r1 += (r2 + 32))
			 (write-multibyte-character r0 r1))))))))

	    ;; 3byte encoding
	    ;; zzzzyyyyyyxxxxxx = 1110zzzz 10yyyyyy 10xxxxxx
	    (if (r0 < #xf0)
		((read r1 r2)

		 ;; This is set to 1 if the encoding is invalid.
		 (r4 = 0)

		 (r3 = (r1 & #b11000000))
		 (r3 |= ((r2 >> 2) & #b00110000))
		 (if (r3 != #b10100000)
		     (r4 = 1)
		   ((r3 = ((r0 & #x0f) << 12))
		    (r3 += ((r1 & #x3f) << 6))
		    (r3 += (r2 & #x3f))
		    (if (r3 < #x0800)
			(r4 = 1))))

		 (if (r4 != 0)
		     ;; Invalid 3-byte sequence
		     ((if (r0 < #xa0)
			  (write-multibyte-character r5 r0)
			(write-multibyte-character r6 r0))
		      (if (r1 < #x80)
			  (write r1)
			(if (r1 < #xa0)
			    (write-multibyte-character r5 r1)
			  (write-multibyte-character r6 r1)))
		      (if (r2 < #x80)
			  (write r2)
			(if (r2 < #xa0)
			    (write-multibyte-character r5 r2)
			  (write-multibyte-character r6 r2))))
		 
		   ;; mule-unicode-0100-24ff (>= 0800)
		   ((if (r3 < #x2500)
			((r0 = ,(charset-id 'mule-unicode-0100-24ff))
			 (r3 -= #x0100)
			 (r3 //= 96)
			 (r1 = (r7 + 32))
			 (r1 += ((r3 + 32) << 7))
			 (write-multibyte-character r0 r1))
		    
		      ;; mule-unicode-2500-33ff
		      (if (r3 < #x3400)
			  ((r0 = ,(charset-id 'mule-unicode-2500-33ff))
			   (r3 -= #x2500)
			   (r3 //= 96)
			   (r1 = (r7 + 32))
			   (r1 += ((r3 + 32) << 7))
			   (write-multibyte-character r0 r1))

			;; U+3400 .. U+D7FF
			;; Keep them as eight-bit-{control|graphic}.
			(if (r3 < #xe000)
			    ((r3 = r6)
			     (write-multibyte-character r3 r0)
			     (if (r1 < #xa0)
				 (r3 = r5))
			     (write-multibyte-character r3 r1)
			     (if (r2 < #xa0)
				 (r3 = r5)
			       (r3 = r6))
			     (write-multibyte-character r3 r2))
			    ;; mule-unicode-e000-ffff
			    ;; Fixme: fffe and ffff are invalid.
			    ((r0 = ,(charset-id 'mule-unicode-e000-ffff))
			     (r3 -= #xe000)
			     (r3 //= 96)
			     (r1 = (r7 + 32))
			     (r1 += ((r3 + 32) << 7))
			     (write-multibyte-character r0 r1))))))))

	      (if (r0 < #xfe)
		  ;; 4byte encoding
		  ;; keep those bytes as eight-bit-{control|graphic}
		  ((read r1 r2 r3)
		   ;; r0 > #xf0, thus eight-bit-graphic
		   (write-multibyte-character r6 r0)
		   (if (r1 < #xa0)
		       (if (r1 < #x80)	; invalid byte
			   (write r1)
			 (write-multibyte-character r5 r1))
		     (write-multibyte-character r6 r1))
		   (if (r2 < #xa0)
		       (if (r2 < #x80)	; invalid byte
			   (write r2)
			 (write-multibyte-character r5 r2))
		     (write-multibyte-character r6 r2))
		   (if (r3 < #xa0)
		       (if (r3 < #x80)	; invalid byte
			   (write r3)
			 (write-multibyte-character r5 r3))
		     (write-multibyte-character r6 r3))
		   (if (r0 >= #xf8)	; 5- or 6-byte encoding
		       ((read r1)
			(if (r1 < #xa0)
			    (if (r1 < #x80) ; invalid byte
				(write r1)
			      (write-multibyte-character r5 r1))
			  (write-multibyte-character r6 r1))
			(if (r0 >= #xfc) ; 6-byte
			    ((read r1)
			     (if (r1 < #xa0)
				 (if (r1 < #x80) ; invalid byte
				     (write r1)
				   (write-multibyte-character r5 r1))
			       (write-multibyte-character r6 r1)))))))
		;; else invalid byte >= #xfe
		(write-multibyte-character r6 r0))))))
      (repeat))))

  "CCL program to decode UTF-8.
Basic decoding is done into the charsets ascii, latin-iso8859-1 and
mule-unicode-*, but see also `utf-fragmentation-table' and
`ucs-mule-cjk-to-unicode'.
Encodings of un-representable Unicode characters are decoded asis into
eight-bit-control and eight-bit-graphic characters.")

(define-ccl-program ccl-encode-mule-utf-8
  `(1
    ((r5 = -1)
     (loop
      (if (r5 < 0)
	  ((r1 = -1)
	   (read-multibyte-character r0 r1)
	   (translate-character utf-translation-table-for-encode r0 r1))
	(;; We have already done read-multibyte-character.
	 (r0 = r5)
	 (r1 = r6)
	 (r5 = -1)))

      (if (r0 == ,(charset-id 'ascii))
	  (write r1)

	(if (r0 == ,(charset-id 'latin-iso8859-1))
	    ;; r1          scalar                  utf-8
	    ;;       0000 0yyy yyxx xxxx    110y yyyy 10xx xxxx
	    ;; 20    0000 0000 1010 0000    1100 0010 1010 0000
	    ;; 7f    0000 0000 1111 1111    1100 0011 1011 1111
	    ((r0 = (((r1 & #x40) >> 6) | #xc2))
	     (r1 &= #x3f)
	     (r1 |= #x80)
	     (write r0 r1))

	  (if (r0 == ,(charset-id 'mule-unicode-0100-24ff))
	      ((r0 = ((((r1 & #x3f80) >> 7) - 32) * 96))
	       ;; #x3f80 == (0011 1111 1000 0000)b
	       (r1 &= #x7f)
	       (r1 += (r0 + 224))	; 240 == -32 + #x0100
	       ;; now r1 holds scalar value
	       (if (r1 < #x0800)
		   ;; 2byte encoding
		   ((r0 = (((r1 & #x07c0) >> 6) | #xc0))
		    ;; #x07c0 == (0000 0111 1100 0000)b
		    (r1 &= #x3f)
		    (r1 |= #x80)
		    (write r0 r1))
		 ;; 3byte encoding
		 ((r0 = (((r1 & #xf000) >> 12) | #xe0))
		  (r2 = ((r1 & #x3f) | #x80))
		  (r1 &= #x0fc0)
		  (r1 >>= 6)
		  (r1 |= #x80)
		  (write r0 r1 r2))))

	    (if (r0 == ,(charset-id 'mule-unicode-2500-33ff))
		((r0 = ((((r1 & #x3f80) >> 7) - 32) * 96))
		 (r1 &= #x7f)
		 (r1 += (r0 + 9440))	; 9440 == -32 + #x2500
		 (r0 = (((r1 & #xf000) >> 12) | #xe0))
		 (r2 = ((r1 & #x3f) | #x80))
		 (r1 &= #x0fc0)
		 (r1 >>= 6)
		 (r1 |= #x80)
		 (write r0 r1 r2))

	      (if (r0 == ,(charset-id 'mule-unicode-e000-ffff))
		  ((r0 = ((((r1 & #x3f80) >> 7) - 32) * 96))
		   (r1 &= #x7f)
		   (r1 += (r0 + 57312))	; 57312 == -32 + #xe000
		   (r0 = (((r1 & #xf000) >> 12) | #xe0))
		   (r2 = ((r1 & #x3f) | #x80))
		   (r1 &= #x0fc0)
		   (r1 >>= 6)
		   (r1 |= #x80)
		   (write r0 r1 r2))

		(if (r0 == ,(charset-id 'eight-bit-control))
		    ;; r1          scalar                  utf-8
		    ;;       0000 0yyy yyxx xxxx    110y yyyy 10xx xxxx
		    ;; 80    0000 0000 1000 0000    1100 0010 1000 0000
		    ;; 9f    0000 0000 1001 1111    1100 0010 1001 1111
		    ((write #xc2)
		     (write r1))

		  (if (r0 == ,(charset-id 'eight-bit-graphic))
		      ;; r1          scalar                  utf-8
		      ;;       0000 0yyy yyxx xxxx    110y yyyy 10xx xxxx
		      ;; a0    0000 0000 1010 0000    1100 0010 1010 0000
		      ;; ff    0000 0000 1111 1111    1101 1111 1011 1111
		      ((write r1)
		       (r1 = -1)
		       (read-multibyte-character r0 r1)
		       (if (r0 != ,(charset-id 'eight-bit-graphic))
			   (if (r0 != ,(charset-id 'eight-bit-control))
			       ((r5 = r0)
				(r6 = r1))))
		       (if (r5 < 0)
			   ((read-multibyte-character r0 r2)
			    (if (r0 != ,(charset-id 'eight-bit-graphic))
				(if (r0 != ,(charset-id 'eight-bit-control))
				    ((r5 = r0)
				     (r6 = r2))))
			    (if (r5 < 0)
				(write r1 r2)
			      (if (r1 < #xa0)
				  (write r1)
				((write #xc2)
				 (write r1)))))))

		    ;; Unsupported character.
		    ;; Output U+FFFD, which is `ef bf bd' in UTF-8.
		    ((write #xef)
		     (write #xbf)
		     (write #xbd)))))))))
      (repeat)))
    (if (r1 >= #xa0)
	(write r1)
      (if (r1 >= #x80)
	  ((write #xc2)
	   (write r1)))))

  "CCL program to encode into UTF-8.")

(make-coding-system
 'mule-utf-8 4 ?u
 "UTF-8 encoding for Emacs-supported Unicode characters.
It supports Unicode characters of these ranges:
    U+0000..U+33FF, U+E000..U+FFFF.
They correspond to these Emacs character sets:
    ascii, latin-iso8859-1, mule-unicode-0100-24ff,
    mule-unicode-2500-33ff, mule-unicode-e000-ffff

On decoding (e.g. reading a file), Unicode characters not in the above
ranges are decoded into sequences of eight-bit-control and
eight-bit-graphic characters to preserve their byte sequences.  The
byte sequence is preserved on i/o for valid utf-8, but not necessarily
for invalid utf-8.

On encoding (e.g. writing a file), Emacs characters not belonging to
any of the character sets listed above are encoded into the UTF-8 byte
sequence representing U+FFFD (REPLACEMENT CHARACTER)."

 '(ccl-decode-mule-utf-8 . ccl-encode-mule-utf-8)
 '((safe-charsets
    ascii
    eight-bit-control
    eight-bit-graphic
    latin-iso8859-1
    mule-unicode-0100-24ff
    mule-unicode-2500-33ff
    mule-unicode-e000-ffff)
   (mime-charset . utf-8)
   (coding-category . coding-category-utf-8)
   (valid-codes (0 . 255))
   (dependency unify-8859-on-encoding-mode
	       unify-8859-on-decoding-mode)))

(define-coding-system-alias 'utf-8 'mule-utf-8)

;;; utf-8.el ends here
