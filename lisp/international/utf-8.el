;;; utf-8.el --- limited UTF-8 decoding/encoding support -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author: TAKAHASHI Naoto  <ntakahas@m17n.org>
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
;; original byte sequence.
;;
;; Characters from other character sets can be encoded with
;; mule-utf-8 by populating the table `ucs-mule-to-mule-unicode' and
;; registering the translation with `register-char-codings'.

;; UTF-8 is defined in RFC 2279.  A sketch of the encoding is:

;;        scalar       |               utf-8
;;        value        | 1st byte  | 2nd byte  | 3rd byte
;; --------------------+-----------+-----------+----------
;; 0000 0000 0xxx xxxx | 0xxx xxxx |           |
;; 0000 0yyy yyxx xxxx | 110y yyyy | 10xx xxxx |
;; zzzz yyyy yyxx xxxx | 1110 zzzz | 10yy yyyy | 10xx xxxx

;;; Code:

(defvar ucs-mule-to-mule-unicode (make-translation-table)
  "Translation table for encoding to `mule-utf-8'.")
;; Could have been done by ucs-tables loaded before.
(unless (get 'ucs-mule-to-mule-unicode 'translation-table)
  (define-translation-table 'ucs-mule-to-mule-unicode ucs-mule-to-mule-unicode))
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

	       ((r0 &= #x1f)
		(r0 <<= 6)
		(r1 &= #x3f)
		(r1 += r0)
		;; Now r1 holds scalar value

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
		     (write-multibyte-character r0 r1)))))))

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
		      
		      ;; U+3400 .. U+DFFF
		    ;; keep those bytes as eight-bit-{control|graphic}
		      (if (r3 < #xe000)
			  ( ;; #xe0 <= r0 < #xf0, so r0 is eight-bit-graphic
			   (r3 = r6)
			   (write-multibyte-character r3 r0)
			   (if (r1 < #xa0)
			       (r3 = r5))
			   (write-multibyte-character r3 r1)
			   (if (r2 < #xa0)
			       (r3 = r5)
			     (r3 = r6))
			   (write-multibyte-character r3 r2))
			
			;; mule-unicode-e000-ffff
			((r0 = ,(charset-id 'mule-unicode-e000-ffff))
			 (r3 -= #xe000)
			 (r3 //= 96)
			 (r1 = (r7 + 32))
			 (r1 += ((r3 + 32) << 7))
			 (write-multibyte-character r0 r1))))))))

	    ;; 4byte encoding
	    ;; keep those bytes as eight-bit-{control|graphic}
	    ((read r1 r2 r3)
	     ;; r0 > #xf0, thus eight-bit-graphic
	     (write-multibyte-character r6 r0)
	     (if (r1 < #xa0)
		 (write-multibyte-character r5 r1)
	       (write-multibyte-character r6 r1))
	     (if (r2 < #xa0)
		 (write-multibyte-character r5 r2)
	       (write-multibyte-character r6 r2))
	     (if (r3 < #xa0)
		 (write-multibyte-character r5 r3)
	       (write-multibyte-character r6 r3))))))

      (repeat))))

  "CCL program to decode UTF-8.
Basic decoding is done into the charsets ascii, latin-iso8859-1 and
mule-unicode-*.  Encodings of un-representable Unicode characters are
decoded asis into eight-bit-control and eight-bit-graphic
characters.")

(define-ccl-program ccl-encode-mule-utf-8
  `(1
    ((r5 = -1)
     (loop
      (if (r5 < 0)
	  ((r1 = -1)
	   (read-multibyte-character r0 r1)
	   (translate-character ucs-mule-to-mule-unicode r0 r1))
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
		   (r1 += (r0 + 57312))	; 57312 == -160 + #xe000
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

  "CCL program to encode into UTF-8.
Only characters from the charsets ascii, eight-bit-control,
eight-bit-graphic, latin-iso8859-1 and mule-unicode-* are recognized.
Others are encoded as U+FFFD.")

;; Dummy definition so that the CCL can be checked correctly; the
;; actual data are loaded on demand.
(unless (boundp 'ucs-mule-8859-to-mule-unicode)	; don't zap it
  (define-translation-table 'ucs-mule-8859-to-mule-unicode))

(defsubst utf-8-untranslated-to-ucs ()
  (let ((b1 (char-after))
	(b2 (char-after (1+ (point))))
	(b3 (char-after (+ 2 (point))))
	(b4 (char-after (+ 4 (point)))))
    (if (and b1 b2 b3)
	(cond ((< b1 ?\xf0)
	       (setq b2 (lsh (logand b2 ?\x3f) 6))
	       (setq b3 (logand b3 ?\x3f))
	       (logior b3 (logior b2 (lsh (logand b1 ?\x0f) 12))))
	      (b4
	       (setq b2 (lsh (logand b2 ?\x3f) 12))
	       (setq b3 (lsh (logand b3 ?\x3f) 6))
	       (setq b4 (logand b4 ?\x3f))
	       (logior b4 (logior b3 (logior b2 (lsh (logand b1 ?\x07)
						     18)))))))))

(defun utf-8-help-echo (window object position)
  (format "Untranslated Unicode U+%04X"
	  (get-char-property position 'untranslated-utf-8 object)))

(defvar utf-8-subst-table nil
  "If non-nil, a hash table mapping `untranslatable utf-8' to Emacs characters.")

;; We compose the untranslatable sequences into a single character.
;; This is infelicitous for editing, because there's currently no
;; mechanism for treating compositions as atomic, but is OK for
;; display.  We try to compose an appropriate character from a hash
;; table of CJK characters to display correctly.  Otherwise we use
;; U+FFFD.  What we really should have is hash table lookup from CCL
;; so that we could do this properly.  This function GCs too much.
(defsubst utf-8-compose ()
  "Put a suitable composition on an untranslatable sequence.
Return the sequence's length."
  (let* ((u (utf-8-untranslated-to-ucs))
	 (l (and u (if (>= u ?\x10000)
		       4
		     3)))
	 (subst (and utf-8-subst-table (gethash u utf-8-subst-table))))
    (when u
      (put-text-property (point) (min (point-max) (+ l (point)))
			 'untranslated-utf-8 u)
      (unless subst
	  (put-text-property (point) (min (point-max) (+ l (point)))
			     'help-echo 'utf-8-help-echo)
	  (setq subst ?$,3u=(B))
      (compose-region (point) (+ l (point)) subst)
      l)))

(defcustom utf-8-compose-scripts nil
  "*Non-nil means compose various scipts on decoding utf-8 text."
  :group 'mule
  :type 'boolean)	; omitted in Emacs 21.1

(defun utf-8-post-read-conversion (length)
  "Compose untranslated utf-8 sequences into single characters.
Also compose particular scripts if `utf-8-compose-scripts' is non-nil."
  (save-excursion
    ;; Can't do eval-when-compile to insert a multibyte constant
    ;; version of the string in the loop, since it's always loaded as
    ;; unibyte from a byte-compiled file.
    (let ((range (string-as-multibyte "^\341-\377"))) 
      (while (and (skip-chars-forward
		   range)
		  (not (eobp)))
	(forward-char (utf-8-compose)))))
  ;; Fixme: Takahashi-san implies it may not work this easily -- needs
  ;; checking with him.
  (when (and utf-8-compose-scripts (> length 1))
    ;; These currently have definitions which cover the relevant
    ;; Unicodes.  We could avoid loading thai-util &c by checking
    ;; whether the region contains any characters with the appropriate
    ;; categories.  There aren't yet Unicode-based rules for Tibetan.
    (save-excursion (setq length (diacritic-post-read-conversion length)))
    (save-excursion (setq length (thai-post-read-conversion length)))
    (save-excursion (setq length (lao-post-read-conversion length)))
    (save-excursion (setq length (devanagari-post-read-conversion length))))
  length)

(defun utf-8-pre-write-conversion (beg end)
  "Semi-dummy pre-write function effectively to autoload ucs-tables."
  ;; Ensure translation table is loaded.
  (require 'ucs-tables)
  ;; Don't do this again.
  (coding-system-put 'mule-utf-8 'pre-write-conversion nil)
  nil)

(make-coding-system
 'mule-utf-8 4 ?u
 "UTF-8 encoding for Emacs-supported Unicode characters.
The supported Emacs character sets are the following, plus others
which may be included in the translation table
`ucs-mule-to-mule-unicode':
 ascii
 eight-bit-control
 eight-bit-graphic
 latin-iso8859-1
 latin-iso8859-2
 latin-iso8859-3
 latin-iso8859-4
 cyrillic-iso8859-5
 greek-iso8859-7
 hebrew-iso8859-8
 latin-iso8859-9
 latin-iso8859-14
 latin-iso8859-15
 mule-unicode-0100-24ff
 mule-unicode-2500-33ff
 mule-unicode-e000-ffff

Unicode characters out of the ranges U+0000-U+33FF and U+E200-U+FFFF
are decoded into sequences of eight-bit-control and eight-bit-graphic
characters to preserve their byte sequences and composed to display as
a single character.  Emacs characters that can't be encoded to these
ranges are encoded as U+FFFD."

 '(ccl-decode-mule-utf-8 . ccl-encode-mule-utf-8)
 '((safe-charsets
    ascii
    eight-bit-control
    eight-bit-graphic
    latin-iso8859-1
    latin-iso8859-15
    latin-iso8859-14
    latin-iso8859-9
    hebrew-iso8859-8
    greek-iso8859-7
    cyrillic-iso8859-5
    latin-iso8859-4
    latin-iso8859-3
    latin-iso8859-2
    vietnamese-viscii-lower
    vietnamese-viscii-upper
    thai-tis620
    ipa
    ethiopic
    indian-is13194
    katakana-jisx0201
    chinese-sisheng
    lao
    mule-unicode-0100-24ff
    mule-unicode-2500-33ff
    mule-unicode-e000-ffff)
   (mime-charset . utf-8)
   (coding-category . coding-category-utf-8)
   (valid-codes (0 . 255))
   (pre-write-conversion . utf-8-pre-write-conversion)
   (post-read-conversion . utf-8-post-read-conversion)))

(define-coding-system-alias 'utf-8 'mule-utf-8)

;; I think this needs special private charsets defined for the
;; untranslated sequences, if it's going to work well.

;;; (defun utf-8-compose-function (pos to pattern &optional string)
;;;   (let* ((prop (get-char-property pos 'composition string))
;;; 	 (l (and prop (- (cadr prop) (car prop)))))
;;;     (cond ((and l (> l (- to pos)))
;;; 	   (delete-region pos to))
;;; 	  ((and (> (char-after pos) 224)
;;; 		(< (char-after pos) 256)
;;; 		(save-restriction
;;; 		  (narrow-to-region pos to)
;;; 		  (utf-8-compose)))
;;; 	   t))))

;;; (dotimes (i 96)
;;;   (aset composition-function-table
;;; 	(+ 128 i)
;;; 	`((,(string-as-multibyte "[\200-\237\240-\377]")
;;; 	   . utf-8-compose-function))))

;;; utf-8.el ends here
