;;; ps-mule.el --- provide multi-byte character facility to ps-print

;; Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <vinicius@cpqd.com.br>
;;	Kenichi Handa <handa@etl.go.jp> (multi-byte characters)
;; Maintainer: Kenichi Handa <handa@etl.go.jp> (multi-byte characters)
;;	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Keywords: wp, print, PostScript, multibyte, mule
;; Time-stamp: <2001/08/15 15:34:11 vinicius>

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; About ps-mule
;; -------------
;;
;; This package is used for ps-print to print multi-byte buffer.
;;
;; See also ps-print.el.
;;
;;
;; Printing Multi-byte Buffer
;; --------------------------
;;
;; The variable `ps-multibyte-buffer' specifies the ps-print multi-byte buffer
;; handling.
;;
;; Valid values for `ps-multibyte-buffer' are:
;;
;;  nil                     This is the value to use the default settings which
;;			    is by default for printing buffer with only ASCII
;;			    and Latin characters.   The default setting can be
;;			    changed by setting the variable
;;			    `ps-mule-font-info-database-default' differently.
;;			    The initial value of this variable is
;;			    `ps-mule-font-info-database-latin' (see
;;			    documentation).
;;
;;  `non-latin-printer'     This is the value to use when you have a japanese
;;			    or korean PostScript printer and want to print
;;			    buffer with ASCII, Latin-1, Japanese (JISX0208 and
;;			    JISX0201-Kana) and Korean characters.  At present,
;;			    it was not tested the Korean characters printing.
;;			    If you have a korean PostScript printer, please,
;;			    test it.
;;
;;  `bdf-font'              This is the value to use when you want to print
;;			    buffer with BDF fonts.  BDF fonts include both latin
;;			    and non-latin fonts.  BDF (Bitmap Distribution
;;			    Format) is a format used for distributing X's font
;;			    source file.  BDF fonts are included in
;;			    `intlfonts-1.2' which is a collection of X11 fonts
;;			    for all characters supported by Emacs.  In order to
;;			    use this value, be sure to have installed
;;			    `intlfonts-1.2' and set the variable
;;			    `bdf-directory-list' appropriately (see ps-bdf.el
;;			    for documentation of this variable).
;;
;;  `bdf-font-except-latin' This is like `bdf-font' except that it is used
;;			    PostScript default fonts to print ASCII and Latin-1
;;			    characters.  This is convenient when you want or
;;			    need to use both latin and non-latin characters on
;;			    the same buffer.  See `ps-font-family',
;;			    `ps-header-font-family' and `ps-font-info-database'.
;;
;; Any other value is treated as nil.
;;
;; The default is nil.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-and-compile
  (require 'ps-print)

  ;; to avoid XEmacs compilation gripes
  (defvar leading-code-private-22 157)
  (or (fboundp 'charset-bytes)
      (defun charset-bytes (charset) 1)) ; ascii
  (or (fboundp 'charset-dimension)
      (defun charset-dimension (charset) 1)) ; ascii
  (or (fboundp 'charset-id)
      (defun charset-id (charset) 0))	; ascii
  (or (fboundp 'charset-width)
      (defun charset-width (charset) 1)) ; ascii
  (or (fboundp 'find-charset-region)
      (defun find-charset-region (beg end &optional table)
	(list 'ascii)))
  (or (fboundp 'char-valid-p)
      (defun char-valid-p (char)
	(< (following-char) 256)))
  (or (fboundp 'split-char)
      (defun split-char (char)
	(list (if (char-valid-p char)
		  'ascii
		'unknow)
	      char)))
  (or (fboundp 'char-width)
      (defun char-width (char) 1))	; ascii
  (or (fboundp 'chars-in-region)
      (defun chars-in-region (beg end)
	(- (max beg end) (min beg end))))
  (or (fboundp 'forward-point)
      (defun forward-point (arg)
	(save-excursion
	  (let ((count (abs arg))
		(step  (if (zerop arg)
			   0
			 (/ arg arg))))
	    (while (and (> count 0)
			(< (point-min) (point)) (< (point) (point-max)))
	      (forward-char step)
	      (setq count (1- count)))
	    (+ (point) (* count step))))))
  (or (fboundp 'decompose-composite-char)
      (defun decompose-composite-char (char &optional type
					    with-composition-rule)
	nil))
  (or (fboundp 'encode-coding-string)
      (defun encode-coding-string (string coding-system &optional nocopy)
	(if nocopy
	    string
	  (copy-sequence string))))
  (or (fboundp 'coding-system-p)
      (defun coding-system-p (obj) nil))
  (or (fboundp 'ccl-execute-on-string)
      (defun ccl-execute-on-string (ccl-prog status str
					     &optional contin unibyte-p)
	str))
  (or (fboundp 'define-ccl-program)
      (defmacro define-ccl-program (name ccl-program &optional doc)
	`(defconst ,name nil ,doc)))
  (or (fboundp 'multibyte-string-p)
      (defun multibyte-string-p (str)
	(let ((len (length str))
	      (i 0)
	      multibyte)
	  (while (and (< i len) (not (setq multibyte (> (aref str i) 255))))
	    (setq i (1+ i)))
	  multibyte)))
  (or (fboundp 'string-make-multibyte)
      (defalias 'string-make-multibyte 'copy-sequence))
  (or (fboundp 'encode-char)
      (defun encode-char (ch ccs)
	ch)))


;;;###autoload
(defcustom ps-multibyte-buffer nil
  "*Specifies the multi-byte buffer handling.

Valid values are:

  nil                     This is the value to use the default settings which
			  is by default for printing buffer with only ASCII
			  and Latin characters.   The default setting can be
			  changed by setting the variable
			  `ps-mule-font-info-database-default' differently.
			  The initial value of this variable is
			  `ps-mule-font-info-database-latin' (see
			  documentation).

  `non-latin-printer'     This is the value to use when you have a Japanese
			  or Korean PostScript printer and want to print
			  buffer with ASCII, Latin-1, Japanese (JISX0208 and
			  JISX0201-Kana) and Korean characters.  At present,
			  it was not tested the Korean characters printing.
			  If you have a korean PostScript printer, please,
			  test it.

  `bdf-font'              This is the value to use when you want to print
			  buffer with BDF fonts.  BDF fonts include both latin
			  and non-latin fonts.  BDF (Bitmap Distribution
			  Format) is a format used for distributing X's font
			  source file.  BDF fonts are included in
			  `intlfonts-1.2' which is a collection of X11 fonts
			  for all characters supported by Emacs.  In order to
			  use this value, be sure to have installed
			  `intlfonts-1.2' and set the variable
			  `bdf-directory-list' appropriately (see ps-bdf.el for
			  documentation of this variable).

  `bdf-font-except-latin' This is like `bdf-font' except that it is used
			  PostScript default fonts to print ASCII and Latin-1
			  characters.  This is convenient when you want or
			  need to use both latin and non-latin characters on
			  the same buffer.  See `ps-font-family',
			  `ps-header-font-family' and `ps-font-info-database'.

Any other value is treated as nil."
  :type '(choice (const non-latin-printer)     (const bdf-font)
		 (const bdf-font-except-latin) (const :tag "nil" nil))
  :group 'ps-print-font)


(eval-and-compile
  ;; For Emacs 20.2 and the earlier version.
  (if (and (boundp 'mule-version)
	   (not (string< (symbol-value 'mule-version) "4.0")))
      ;; mule package is loaded
      (progn
	(defalias 'ps-mule-next-point '1+)
	(defalias 'ps-mule-chars-in-string 'length)
	(defalias 'ps-mule-string-char 'aref)
	(defsubst ps-mule-next-index (str i) (1+ i)))
    ;; mule package isn't loaded or mule version lesser than 4.0
    (defun ps-mule-next-point (arg)
      (save-excursion (goto-char arg) (forward-char 1) (point)))
    (defun ps-mule-chars-in-string (string)
      (/ (length string)
	 (charset-bytes (char-charset (string-to-char string)))))
    (defun ps-mule-string-char (string idx)
      (string-to-char (substring string idx)))
    (defun ps-mule-next-index (string i)
      (+ i (charset-bytes (char-charset (string-to-char string)))))
    )
  ;; For Emacs 20.4 and the earlier version.
  (if (and (boundp 'mule-version)
	   (string< (symbol-value 'mule-version) "5.0"))
      ;; mule package is loaded and mule version is lesser than 5.0
      (progn
	(defun encode-composition-rule (rule)
	  (if (= (car rule) 4) (setcar rule 10))
	  (if (= (cdr rule) 4) (setcdr rule 10))
	  (+ (* (car rule) 12) (cdr rule)))
	(defun find-composition (pos &rest ignore)
	  (let ((ch (char-after pos)))
	    (and ch (eq (char-charset ch) 'composition)
		 (let ((components (decompose-composite-char ch 'vector t)))
		   (list pos (ps-mule-next-point pos) components
			 (integerp (aref components 1)) nil
			 (char-width ch)))))))
    ;; mule package isn't loaded
    (or (fboundp 'encode-composition-rule)
	(defun encode-composition-rule (rule)
	  130))
    (or (fboundp 'find-composition)
	(defun find-composition (pos &rest ignore)
	  nil))
    ))

(defvar ps-mule-font-info-database
  nil
  "Alist of charsets with the corresponding font information.
Each element has the form:

	(CHARSET (FONT-TYPE FONT-SRC FONT-NAME ENCODING BYTES) ...)

Where

CHARSET is a charset (symbol) for this font family,

FONT-TYPE is a font type: normal, bold, italic, or bold-italic.

FONT-SRC is a font source: builtin, ps-bdf, vflib, or nil.

  If FONT-SRC is builtin, FONT-NAME is a built-in PostScript font name.

  If FONT-SRC is bdf, FONT-NAME is a BDF font file name, or a list of
  alternative font names.  To use this font, the external library `ps-bdf'
  is required.

  If FONT-SRC is vflib, FONT-NAME is the name of a font that VFlib knows.
  To use this font, the external library `vflib' is required.

  If FONT-SRC is nil, a proper ASCII font in the variable
  `ps-font-info-database' is used.  This is useful for Latin-1 characters.

ENCODING is a coding system to encode a string of characters of CHARSET into a
proper string matching an encoding of the specified font.  ENCODING may be a
function that does this encoding.  In this case, the function is called with
one argument, the string to encode, and it should return an encoded string.

BYTES specifies how many bytes each character has in the encoded byte
sequence; it should be 1 or 2.

All multi-byte characters are printed by fonts specified in this database
regardless of a font family of ASCII characters.  The exception is Latin-1
characters which are printed by the same font as ASCII characters, thus obey
font family.

See also the variable `ps-font-info-database'.")

(defconst ps-mule-font-info-database-latin
  '((latin-iso8859-1
     (normal nil nil iso-latin-1)))
  "Sample setting of `ps-mule-font-info-database' to use latin fonts.")

(defcustom ps-mule-font-info-database-default
  ps-mule-font-info-database-latin
  "*The default setting to use when `ps-multibyte-buffer' is nil."
  :type '(symbol :tag "Multi-Byte Buffer Database Font Default")
  :group 'ps-print-font)

(defconst ps-mule-font-info-database-ps
  '((katakana-jisx0201
     (normal builtin "Ryumin-Light.Katakana" ps-mule-encode-7bit 1)
     (bold builtin "GothicBBB-Medium.Katakana" ps-mule-encode-7bit 1)
     (bold-italic builtin "GothicBBB-Medium.Katakana" ps-mule-encode-7bit 1))
    (latin-jisx0201
     (normal builtin "Ryumin-Light.Hankaku" ps-mule-encode-7bit 1)
     (bold builtin "GothicBBB-Medium.Hankaku" ps-mule-encode-7bit 1))
    (japanese-jisx0208
     (normal builtin "Ryumin-Light-H" ps-mule-encode-7bit 2)
     (bold builtin "GothicBBB-Medium-H" ps-mule-encode-7bit 2))
    (korean-ksc5601
     (normal builtin "Munhwa-Regular-KSC-EUC-H" ps-mule-encode-7bit 2)
     (bold builtin "Munhwa-Bold-KSC-EUC-H" ps-mule-encode-7bit 2))
    )
  "Sample setting of the `ps-mule-font-info-database' to use builtin PS font.

Currently, data for Japanese and Korean PostScript printers are listed.")

(defconst ps-mule-font-info-database-bdf
  '((ascii
     (normal bdf ("lt1-24-etl.bdf" "etl24-latin1.bdf") nil 1)
     (bold bdf ("lt1-16b-etl.bdf" "etl16b-latin1.bdf") nil 1)
     (italic bdf ("lt1-16i-etl.bdf" "etl16i-latin1.bdf") nil 1)
     (bold-italic bdf ("lt1-16bi-etl.bdf" "etl16bi-latin1.bdf") nil 1))
    (latin-iso8859-1
     (normal bdf ("lt1-24-etl.bdf" "etl24-latin1.bdf") iso-latin-1 1)
     (bold bdf ("lt1-16b-etl.bdf" "etl16b-latin1.bdf") iso-latin-1 1)
     (italic bdf ("lt1-16i-etl.bdf" "etl16i-latin1.bdf") iso-latin-1 1)
     (bold-italic bdf ("lt1-16bi-etl.bdf" "etl16bi-latin1.bdf") iso-latin-1 1))
    (latin-iso8859-2
     (normal bdf ("lt2-24-etl.bdf" "etl24-latin2.bdf") iso-latin-2 1))
    (latin-iso8859-3
     (normal bdf ("lt3-24-etl.bdf" "etl24-latin3.bdf") iso-latin-3 1))
    (latin-iso8859-4
     (normal bdf ("lt4-24-etl.bdf" "etl24-latin4.bdf") iso-latin-4 1))
    (thai-tis620
     (normal bdf ("thai24.bdf" "thai-24.bdf") thai-tis620 1))
    (greek-iso8859-7
     (normal bdf ("grk24-etl.bdf" "etl24-greek.bdf") greek-iso-8bit 1))
    ;; (arabic-iso8859-6	nil) ; not yet available
    (hebrew-iso8859-8
     (normal bdf ("heb24-etl.bdf" "etl24-hebrew.bdf") hebrew-iso-8bit 1))
    (katakana-jisx0201
     (normal bdf "12x24rk.bdf" ps-mule-encode-8bit 1))
    (latin-jisx0201
     (normal bdf "12x24rk.bdf" ps-mule-encode-7bit 1))
    (cyrillic-iso8859-5
     (normal bdf ("cyr24-etl.bdf" "etl24-cyrillic.bdf") cyrillic-iso-8bit 1))
    (latin-iso8859-9
     (normal bdf ("lt5-24-etl.bdf" "etl24-latin5.bdf") iso-latin-5 1))
    (japanese-jisx0208-1978
     (normal bdf "jiskan24.bdf" ps-mule-encode-7bit 2))
    (chinese-gb2312
     (normal bdf "gb24st.bdf" ps-mule-encode-7bit 2))
    (japanese-jisx0208
     (normal bdf "jiskan24.bdf" ps-mule-encode-7bit 2))
    (korean-ksc5601
     (normal bdf "hanglm24.bdf" ps-mule-encode-7bit 2))
    (japanese-jisx0212
     (normal bdf ("jksp40.bdf" "jisksp40.bdf") ps-mule-encode-7bit 2))
    (chinese-cns11643-1
     (normal bdf ("cns1-40.bdf" "cns-1-40.bdf") ps-mule-encode-7bit 2))
    (chinese-cns11643-2
     (normal bdf ("cns2-40.bdf" "cns-2-40.bdf") ps-mule-encode-7bit 2))
    (chinese-big5-1
     (normal bdf "taipei24.bdf" chinese-big5 2))
    (chinese-big5-2
     (normal bdf "taipei24.bdf" chinese-big5 2))
    (chinese-sisheng
     (normal bdf ("sish24-etl.bdf" "etl24-sisheng.bdf") ps-mule-encode-7bit 1))
    (ipa
     (normal bdf ("ipa24-etl.bdf" "etl24-ipa.bdf") ps-mule-encode-8bit 1))
    (vietnamese-viscii-lower
     (normal bdf ("visc24-etl.bdf" "etl24-viscii.bdf") vietnamese-viscii 1))
    (vietnamese-viscii-upper
     (normal bdf ("visc24-etl.bdf" "etl24-viscii.bdf") vietnamese-viscii 1))
    (arabic-digit
     (normal bdf ("arab24-0-etl.bdf" "etl24-arabic0.bdf") ps-mule-encode-7bit 1))
    (arabic-1-column
     (normal bdf ("arab24-1-etl.bdf" "etl24-arabic1.bdf") ps-mule-encode-7bit 1))
    ;; (ascii-right-to-left nil) ; not yet available
    (lao
     (normal bdf ("lao24-mule.bdf" "mule-lao-24.bdf") lao 1))
    (arabic-2-column
     (normal bdf ("arab24-2-etl.bdf" "etl24-arabic2.bdf") ps-mule-encode-7bit 1))
    (indian-is13194
     (normal bdf ("isci24-mule.bdf" "mule-iscii-24.bdf") ps-mule-encode-7bit 1))
    (indian-1-column
     (normal bdf ("ind1c24-mule.bdf" "mule-indian-1col-24.bdf") ps-mule-encode-7bit 2))
    (tibetan-1-column
     (normal bdf ("tib1c24-mule.bdf" "mule-tibmdx-1col-24.bdf") ps-mule-encode-7bit 2))
    (ethiopic
     (normal bdf ("ethio24f-uni.bdf" "ethiomx24f-uni.bdf") ps-mule-encode-ethiopic 2))
    (chinese-cns11643-3
     (normal bdf ("cns3-40.bdf" "cns-3-40.bdf") ps-mule-encode-7bit 2))
    (chinese-cns11643-4
     (normal bdf ("cns4-40.bdf" "cns-4-40.bdf") ps-mule-encode-7bit 2))
    (chinese-cns11643-5
     (normal bdf ("cns5-40.bdf" "cns-5-40.bdf") ps-mule-encode-7bit 2))
    (chinese-cns11643-6
     (normal bdf ("cns6-40.bdf" "cns-6-40.bdf") ps-mule-encode-7bit 2))
    (chinese-cns11643-7
     (normal bdf ("cns7-40.bdf" "cns-7-40.bdf") ps-mule-encode-7bit 2))
    (indian-2-column
     (normal bdf ("ind24-mule.bdf" "mule-indian-24.bdf") ps-mule-encode-7bit 2))
    (tibetan
     (normal bdf ("tib24p-mule.bdf" "tib24-mule.bdf" "mule-tibmdx-24.bdf")
	     ps-mule-encode-7bit 2))
    (mule-unicode-0100-24ff
     (normal bdf "etl24-unicode.bdf" ps-mule-encode-ucs2 2))
    (mule-unicode-2500-33ff
     (normal bdf "etl24-unicode.bdf" ps-mule-encode-ucs2 2))
    (mule-unicode-e000-ffff
     (normal bdf "etl24-unicode.bdf" ps-mule-encode-ucs2 2)))
  "Sample setting of the `ps-mule-font-info-database' to use BDF fonts.
BDF (Bitmap Distribution Format) is a format used for distributing X's font
source file.

Current default value list for BDF fonts is included in `intlfonts-1.2'
which is a collection of X11 fonts for all characters supported by Emacs.

Using this list as default value to `ps-mule-font-info-database', all
characters including ASCII and Latin-1 are printed by BDF fonts.

See also `ps-mule-font-info-database-ps-bdf'.")

(defconst ps-mule-font-info-database-ps-bdf
  (cons (car ps-mule-font-info-database-latin)
	(cdr (cdr ps-mule-font-info-database-bdf)))
  "Sample setting of the `ps-mule-font-info-database' to use BDF fonts.

Current default value list for BDF fonts is included in `intlfonts-1.2'
which is a collection of X11 fonts for all characters supported by Emacs.

Using this list as default value to `ps-mule-font-info-database', all
characters except ASCII and Latin-1 characters are printed with BDF fonts.
ASCII and Latin-1 characters are printed with PostScript font specified
by `ps-font-family' and `ps-header-font-family'.

See also `ps-mule-font-info-database-bdf'.")

;; Two typical encoding functions for PostScript fonts.

(defun ps-mule-encode-7bit (string)
  (ps-mule-encode-bit string 0))

(defun ps-mule-encode-8bit (string)
  (ps-mule-encode-bit string 128))

(defun ps-mule-encode-bit (string delta)
  (let* ((dim (charset-dimension (char-charset (string-to-char string))))
	 (len (* (ps-mule-chars-in-string string) dim))
	 (str (make-string len 0))
	 (i 0)
	 (j 0))
    (if (= dim 1)
	(while (< j len)
	  (aset str j
		(+ (nth 1 (split-char (ps-mule-string-char string i))) delta))
	  (setq i (ps-mule-next-index string i)
		j (1+ j)))
      (while (< j len)
	(let ((split (split-char (ps-mule-string-char string i))))
	  (aset str j (+ (nth 1 split) delta))
	  (aset str (1+ j) (+ (nth 2 split) delta))
	  (setq i (ps-mule-next-index string i)
		j (+ j 2)))))
    str))

;; Special encoding function for Ethiopic.
(if (boundp 'mule-version)		; only if mule package is loaded
    (define-ccl-program ccl-encode-ethio-unicode
      `(1
	((read r2)
	 (loop
	  (if (r2 == ,leading-code-private-22)
	      ((read r0)
	       (if (r0 == ,(charset-id 'ethiopic))
		   ((read r1 r2)
		    (r1 &= 127) (r2 &= 127)
		    (call ccl-encode-ethio-font)
		    (write r1)
		    (write-read-repeat r2))
		 ((write r2 r0)
		  (repeat))))
	    (write-read-repeat r2))))))
  ;; to avoid compilation gripes
  (defvar ccl-encode-ethio-unicode nil))

(if (boundp 'mule-version)
    ;; bound mule-version
    (defun ps-mule-encode-ethiopic (string)
      (ccl-execute-on-string (symbol-value 'ccl-encode-ethio-unicode)
			     (make-vector 9 nil)
			     string))
  ;; unbound mule-version
  (defun ps-mule-encode-ethiopic (string)
    string))

;; Special encoding for mule-unicode-* characters.
(defun ps-mule-encode-ucs2 (string)
  (let* ((len (ps-mule-chars-in-string string))
	 (str (make-string (* 2 len) 0))
	 (i 0)
	 (j 0)
	 ch hi lo)
    (while (< i len)
      (setq ch (encode-char (ps-mule-string-char string i) 'ucs)
	    hi (lsh ch -8)
	    lo (logand ch 255))
      (aset str j hi)
      (aset str (1+ j) lo)
      (setq i (1+ i)
	    j (+ j 2)))
    str))

;; A charset which we are now processing.
(defvar ps-mule-current-charset nil)

(defun ps-mule-get-font-spec (charset font-type)
  "Return FONT-SPEC for printing characters CHARSET with FONT-TYPE.
FONT-SPEC is a list that has the form:

	(FONT-SRC FONT-NAME ENCODING BYTES)

FONT-SPEC is extracted from `ps-mule-font-info-database'.

See the documentation of `ps-mule-font-info-database' for the meaning of each
element of the list."
  (let ((slot (cdr (assq charset ps-mule-font-info-database))))
    (and slot
	 (cdr (or (assq font-type slot)
		  (and (eq font-type 'bold-italic)
		       (or (assq 'bold slot) (assq 'italic slot)))
		  (assq 'normal slot))))))

;; Functions to access each element of FONT-SPEC.
(defsubst ps-mule-font-spec-src (font-spec) (car font-spec))
(defsubst ps-mule-font-spec-name (font-spec) (nth 1 font-spec))
(defsubst ps-mule-font-spec-encoding (font-spec) (nth 2 font-spec))
(defsubst ps-mule-font-spec-bytes (font-spec) (nth 3 font-spec))

(defsubst ps-mule-printable-p (charset)
  "Non-nil if characters in CHARSET is printable."
  (ps-mule-get-font-spec charset 'normal))

(defconst ps-mule-external-libraries
  '((builtin nil nil
	     nil nil nil)
    (bdf ps-bdf nil
	 bdf-generate-prologue bdf-generate-font bdf-generate-glyphs)
    (pcf nil nil
	 pcf-generate-prologue pcf-generate-font pcf-generate-glyphs)
    (vflib nil nil
	   vflib-generate-prologue vflib-generate-font vflib-generate-glyphs))
  "Alist of information of external libraries to support PostScript printing.
Each element has the form:

    (FONT-SRC FEATURE INITIALIZED-P PROLOGUE-FUNC FONT-FUNC GLYPHS-FUNC)

FONT-SRC is the font source: builtin, bdf, pcf, or vflib.

FEATURE is the feature that provide a facility to handle FONT-SRC.  Except for
`builtin' FONT-SRC, this feature is automatically `require'd before handling
FONT-SRC.  Currently, we only have the feature `ps-bdf'.

INITIALIZED-P indicates if this library is initialized or not.

PROLOGUE-FUNC is a function to generate PostScript code which define several
PostScript procedures that will be called by FONT-FUNC and GLYPHS-FUNC.  It is
called with no argument, and should return a list of strings.

FONT-FUNC is a function to generate PostScript code which define a new font.  It
is called with one argument FONT-SPEC, and should return a list of strings.

GLYPHS-FUNC is a function to generate PostScript code which define glyphs of
characters.  It is called with three arguments FONT-SPEC, CODE-LIST, and BYTES,
and should return a list of strings.")

(defun ps-mule-init-external-library (font-spec)
  "Initialize external library specified by FONT-SPEC for PostScript printing.
See the documentation of `ps-mule-get-font-spec' for FONT-SPEC's meaning."
  (let* ((font-src (ps-mule-font-spec-src font-spec))
	 (slot (assq font-src ps-mule-external-libraries)))
    (or (not font-src)
	(nth 2 slot)
	(let ((func (nth 3 slot)))
	  (if func
	      (progn
		(require (nth 1 slot))
		(ps-output-prologue (funcall func))))
	  (setcar (nthcdr 2 slot) t)))))

;; Cached glyph information of fonts, alist of:
;;	(FONT-NAME ((FONT-TYPE-NUMBER . SCALED-FONT-NAME) ...)
;;	 cache CODE0 CODE1 ...)
(defvar ps-mule-font-cache nil)

(defun ps-mule-generate-font (font-spec charset &optional header-p)
  "Generate PostScript codes to define a new font in FONT-SPEC for CHARSET.

If optional 3rd arg HEADER-P is non-nil, generate codes to define a header
font."
  (let* ((font-name (ps-mule-font-spec-name font-spec))
	 (font-name (if (consp font-name) (car font-name) font-name))
	 (font-cache (assoc font-name ps-mule-font-cache))
	 (font-src (ps-mule-font-spec-src font-spec))
	 (func (nth 4 (assq font-src ps-mule-external-libraries)))
	 (font-size (if header-p (if (eq ps-current-font 0)
				     ps-header-title-font-size-internal
				   ps-header-font-size-internal)
		      ps-font-size-internal))
	 (current-font (+ ps-current-font (if header-p 10 0)))
	 (scaled-font-name
	  (cond (header-p
		 (format "h%d" ps-current-font))
		((eq charset 'ascii)
		 (format "f%d" ps-current-font))
		(t
		 (format "f%02x-%d" (charset-id charset) ps-current-font)))))
    (and func (not font-cache)
	 (ps-output-prologue (funcall func charset font-spec)))
    (ps-output-prologue
     (list (format "/%s %f /%s Def%sFontMule\n"
		   scaled-font-name font-size font-name
		   (if (or header-p
			   (eq ps-mule-current-charset 'ascii))
		       "Ascii" ""))))
    (if font-cache
	(setcar (cdr font-cache)
		(cons (cons current-font scaled-font-name)
		      (nth 1 font-cache)))
      (setq font-cache (list font-name
			     (list (cons current-font scaled-font-name))
			     'cache)
	    ps-mule-font-cache (cons font-cache ps-mule-font-cache)))
    font-cache))

(defun ps-mule-generate-glyphs (font-spec code-list)
  "Generate PostScript codes which generate glyphs for CODE-LIST of FONT-SPEC."
  (let* ((font-src (ps-mule-font-spec-src font-spec))
	 (func (nth 5 (assq font-src ps-mule-external-libraries))))
    (and func
	 (ps-output-prologue
	  (funcall func font-spec code-list
		   (ps-mule-font-spec-bytes font-spec))))))

(defun ps-mule-prepare-font (font-spec string charset
				       &optional no-setfont header-p)
  "Generate PostScript codes to print STRING of CHARSET by font FONT-SPEC.

The generated code is inserted on prologue part except the code that sets the
current font (using PostScript procedure `FM').

If optional 4th arg NO-SETFONT is non-nil, don't generate the code for setting
the current font.

If optional 5th arg HEADER-P is non-nil, generate a code for setting a header
font."
  (let* ((font-name (ps-mule-font-spec-name font-spec))
	 (font-name (if (consp font-name) (car font-name) font-name))
	 (current-font (+ ps-current-font (if header-p 10 0)))
	 (font-cache (assoc font-name ps-mule-font-cache)))
    (or (and font-cache (assq current-font (nth 1 font-cache)))
	(setq font-cache (ps-mule-generate-font font-spec charset header-p)))
    (or no-setfont
	(let ((new-font (cdr (assq current-font (nth 1 font-cache)))))
	  (or (equal new-font ps-last-font)
	      (progn
		(ps-output (format "/%s FM\n" new-font))
		(setq ps-last-font new-font)))))
    (if (nth 5 (assq (ps-mule-font-spec-src font-spec)
		     ps-mule-external-libraries))
	;; We have to generate PostScript codes which define glyphs.
	(let* ((cached-codes (nthcdr 2 font-cache))
	       (bytes (ps-mule-font-spec-bytes font-spec))
	       (len (length string))
	       (i 0)
	       newcodes code)
	  (while (< i len)
	    (setq code (if (= bytes 1)
			   (aref string i)
			 (+ (* (aref string i) 256) (aref string (1+ i)))))
	    (or (memq code cached-codes)
		(progn
		  (setq newcodes (cons code newcodes))
		  (setcdr cached-codes (cons code (cdr cached-codes)))))
	    (setq i (+ i bytes)))
	  (and newcodes
	       (ps-mule-generate-glyphs font-spec newcodes))))))

;;;###autoload
(defun ps-mule-prepare-ascii-font (string)
  "Setup special ASCII font for STRING.
STRING should contain only ASCII characters."
  (let ((font-spec
	 (ps-mule-get-font-spec
	  'ascii
	  (car (nth ps-current-font (ps-font-alist 'ps-font-for-text))))))
    (and font-spec
	 (ps-mule-prepare-font font-spec string 'ascii))))

;;;###autoload
(defun ps-mule-set-ascii-font ()
  (unless (eq ps-mule-current-charset 'ascii)
    (ps-set-font ps-current-font)
    (setq ps-mule-current-charset 'ascii)))

;; List of charsets of multi-byte characters in a text being printed.
;; If the text doesn't contain any multi-byte characters (i.e. only ASCII),
;; the value is nil.
(defvar ps-mule-charset-list nil)

;; This is a PostScript code inserted in the header of generated PostScript.
(defconst ps-mule-prologue
  "%%%% Start of Mule Section

%% Working dictionary for general use.
/MuleDict 10 dict def

%% Adjust /RelativeCompose properly by checking /BaselineOffset.
/AdjustRelativeCompose {	% fontdict  |-  fontdict
  dup length 2 add dict begin
    { 1 index /FID ne { def } { pop pop } ifelse } forall
    currentdict /BaselineOffset known {
	BaselineOffset false eq { /BaselineOffset 0 def } if
    } {
      /BaselineOffset 0 def
    } ifelse
    currentdict /RelativeCompose known not {
      /RelativeCompose [ 0 0.1 ] def
    } {
      RelativeCompose false ne {
	[ BaselineOffset RelativeCompose BaselineOffset add
	  [ FontMatrix { FontSize div } forall ] transform ]
	/RelativeCompose exch def
      } if
    } ifelse
    currentdict
  end
} def

%% Define already scaled font for non-ASCII character sets.
/DefFontMule {			% fontname size basefont  |-  --
  findfont exch scalefont AdjustRelativeCompose definefont pop
} bind def

%% Define already scaled font for ASCII character sets.
/DefAsciiFontMule {		% fontname size basefont  |-
  MuleDict begin
  findfont dup /Encoding get /ISOLatin1Encoding exch def
  exch scalefont AdjustRelativeCompose reencodeFontISO
  end
} def

/CurrentFont false def

%% Set the specified font to use.
%% For non-ASCII font, don't install Ascent, etc.
/FM {				%  fontname  |-  --
    /font exch def
    font /f0 eq font /f1 eq font /f2 eq font /f3 eq or or or {
      font F
    } {
      font findfont setfont
    } ifelse
} bind def

%% Show vacant box for characters which don't have appropriate font.
/SB {				% count column |-  --
    SpaceWidth mul /w exch def
    1 exch 1 exch { %for
	pop
	gsave
	0 setlinewidth
	0 Descent rmoveto w 0 rlineto
	0 LineHeight rlineto w neg 0 rlineto closepath stroke
	grestore
	w 0 rmoveto
    } for
} bind def

%% Flag to tell if we are now handling a composition.  This is
%% defined here because both composition handler and bitmap font
%% handler require it.
/Composing false def

%%%% End of Mule Section

"
  "PostScript code for printing multi-byte characters.")

(defvar ps-mule-prologue-generated nil)

(defun ps-mule-prologue-generated ()
  (unless ps-mule-prologue-generated
    (ps-output-prologue ps-mule-prologue)
    (setq ps-mule-prologue-generated t)))

(defun ps-mule-find-wrappoint (from to char-width &optional composition)
  "Find the longest sequence which is printable in the current line.

The search starts at FROM and goes until TO.

Optional 4th arg COMPOSITION, if non-nil, is information of
composition starting at FROM.

If COMPOSITION is nil, it is assumed that all characters between FROM
and TO belong to a charset in `ps-mule-current-charset'.  Otherwise,
it is assumed that all characters between FROM and TO belong to the
same composition.

CHAR-WIDTH is the average width of ASCII characters in the current font.

Returns the value:

	(ENDPOS . RUN-WIDTH)

Where ENDPOS is the end position of the sequence and RUN-WIDTH is the width of
the sequence."
  (if (or composition (eq ps-mule-current-charset 'composition))
      ;; We must draw one char by one.
      (let ((run-width (if composition
			   (nth 5 composition)
			 (* (char-width (char-after from)) char-width))))
	(if (> run-width ps-width-remaining)
	    (cons from ps-width-remaining)
	  (cons (if composition
		    (nth 1 composition)
		  (ps-mule-next-point from))
		run-width)))
    ;; We assume that all characters in this range have the same width.
    (setq char-width (* char-width (charset-width ps-mule-current-charset)))
    (let ((run-width (* (chars-in-region from to) char-width)))
      (if (> run-width ps-width-remaining)
	  (cons (min to
		     (save-excursion
		       (goto-char from)
		       (forward-point
			(truncate (/ ps-width-remaining char-width)))))
		ps-width-remaining)
	(cons to run-width)))))

;;;###autoload
(defun ps-mule-plot-string (from to &optional bg-color)
  "Generate PostScript code for plotting characters in the region FROM and TO.

It is assumed that all characters in this region belong to the same charset.

Optional argument BG-COLOR specifies background color.

Returns the value:

	(ENDPOS . RUN-WIDTH)

Where ENDPOS is the end position of the sequence and RUN-WIDTH is the width of
the sequence."
  (setq ps-mule-current-charset (charset-after from))
  (let* ((wrappoint (ps-mule-find-wrappoint
		     from to (ps-avg-char-width 'ps-font-for-text)))
	 (to (car wrappoint))
	 (font-type (car (nth ps-current-font
			      (ps-font-alist 'ps-font-for-text))))
	 (font-spec (ps-mule-get-font-spec ps-mule-current-charset font-type))
	 (string (buffer-substring-no-properties from to)))
    (cond
     ((= from to)
      ;; We can't print any more characters in the current line.
      nil)

     (font-spec
      ;; We surely have a font for printing this character set.
      (ps-output-string (ps-mule-string-encoding font-spec string))
      (ps-output " S\n"))

     ((eq ps-mule-current-charset 'latin-iso8859-1)
      ;; Latin-1 can be printed by a normal ASCII font.
      (ps-output-string (ps-mule-string-ascii string))
      (ps-output " S\n"))

     ;; This case is obsolete for Emacs 21.
     ((eq ps-mule-current-charset 'composition)
      (ps-mule-plot-composition from (ps-mule-next-point from) bg-color))

     (t
      ;; No way to print this charset.  Just show a vacant box of an
      ;; appropriate width.
      (ps-output (format "%d %d SB\n"
			 (length string)
			 (if (eq ps-mule-current-charset 'composition)
			     (char-width (char-after from))
			   (charset-width ps-mule-current-charset))))))
    wrappoint))

;;;###autoload
(defun ps-mule-plot-composition (from to &optional bg-color)
  "Generate PostScript code for plotting composition in the region FROM and TO.

It is assumed that all characters in this region belong to the same
composition.

Optional argument BG-COLOR specifies background color.

Returns the value:

	(ENDPOS . RUN-WIDTH)

Where ENDPOS is the end position of the sequence and RUN-WIDTH is the width of
the sequence."
  (let* ((composition (find-composition from nil nil t))
	 (wrappoint (ps-mule-find-wrappoint
		     from to (ps-avg-char-width 'ps-font-for-text)
		     composition))
	 (to (car wrappoint))
	 (font-type (car (nth ps-current-font
			      (ps-font-alist 'ps-font-for-text)))))
    (if (< from to)
	;; We can print this composition in the current line.
	(let ((components (nth 2 composition)))
	  (ps-mule-plot-components
	   (ps-mule-prepare-font-for-components components font-type)
	   (if (nth 3 composition) "RLC" "RBC"))))
    wrappoint))

;; Prepare font of FONT-TYPE for printing COMPONENTS.  By side effect,
;; change character elements in COMPONENTS to the form:
;;	ENCODED-STRING or (FONTNAME . ENCODED-STRING)
;; and change rule elements to the encoded value (integer).
;; The latter form is used if we much change font for the character.

(defun ps-mule-prepare-font-for-components (components font-type)
  (let ((len (length components))
	(i 0)
	elt)
    (while (< i len)
      (setq elt (aref components i))
      (if (consp elt)
	  ;; ELT is a composition rule.
	  (setq elt (encode-composition-rule elt))
	;; ELT is a glyph character.
	(let* ((charset (char-charset elt))
	       (font (or (eq charset ps-mule-current-charset)
			 (if (eq charset 'ascii)
			     (format "/f%d" ps-current-font)
			   (format "/f%02x-%d"
				   (charset-id charset) ps-current-font))))
		str)
	  (setq ps-mule-current-charset charset
		str (ps-mule-string-encoding
		     (ps-mule-get-font-spec charset font-type)
		     (char-to-string elt)
		     'no-setfont))
	  (if (stringp font)
	      (setq elt (cons font str) ps-last-font font)
	    (setq elt str))))
      (aset components i elt)
      (setq i (1+ i))))
  components)

(defun ps-mule-plot-components (components tail)
  (let ((elt (aref components 0))
	(len (length components))
	(i 1))
    (ps-output "[ ")
    (if (stringp elt)
	(ps-output-string elt)
      (ps-output (car elt) " ")
      (ps-output-string (cdr elt)))
    (while (< i len)
      (setq elt (aref components i) i (1+ i))
      (ps-output " ")
      (cond ((stringp elt)
	     (ps-output-string elt))
	    ((consp elt)
	     (ps-output (car elt) " ")
	     (ps-output-string (cdr elt)))
	    (t				; i.e. (integerp elt)
	     (ps-output (format "%d" elt)))))
    (ps-output " ] " tail "\n")))

;; Composite font support

(defvar ps-mule-composition-prologue-generated nil)

(defconst ps-mule-composition-prologue
  "%%%% Character composition handler
/RelativeCompositionSkip 0.4 def

%% Get a bounding box (relative to currentpoint) of STR.
/GetPathBox {			% str  |-  --
    gsave
    currentfont /FontType get 3 eq { %ifelse
	stringwidth pop pop
    } {
	currentpoint /y exch def /x exch def
	false charpath flattenpath pathbbox
	y sub /URY exch def x sub /URX exch def
	y sub /LLY exch def x sub /LLX exch def
    } ifelse
    grestore
} bind def

%% Apply effects (underline, strikeout, overline, box) to the
%% rectangle specified by TOP BOTTOM LEFT RIGHT.
/SpecialEffect {					% --  |-  --
    currentpoint dup TOP add /yy exch def BOTTOM add /YY exch def
    dup LEFT add /xx exch def RIGHT add /XX exch def
    %% Adjust positions for future shadowing.
    Effect 8 and 0 ne {
	/yy yy Yshadow add def
	/XX XX Xshadow add def
    } if
    Effect 1 and 0 ne { UnderlinePosition Hline } if	% underline
    Effect 2 and 0 ne { StrikeoutPosition Hline } if	% strikeout
    Effect 4 and 0 ne { OverlinePosition  Hline } if	% overline
    bg {						% background
	true
	Effect 16 and 0 ne {SpaceBackground doBox} { xx yy XX YY doRect} ifelse
    } if
    Effect 16 and 0 ne { false 0 doBox } if		% box
} def

%% Show STR with effects (shadow, outline).
/ShowWithEffect {					% str  |-  --
    Effect 8 and 0 ne { dup doShadow } if
    Effect 32 and 0 ne { true doOutline } { show } ifelse
} def

%% Draw COMPONENTS which have the form [ font0? [str0 xoff0 yoff0] ... ].
/ShowComponents {					% components  |-  -
    LEFT 0 lt { LEFT neg 0 rmoveto } if
    {
	dup type /nametype eq {				% font
	    FM
	} {						% [ str xoff yoff ]
	    gsave
	    aload pop rmoveto ShowWithEffect
	    grestore
	} ifelse
    } forall
    RIGHT 0 rmoveto
} def

%% Show relative composition.
/RLC {		% [ font0? str0 font1? str1 ... fontN? strN ]  |-  --
    /components exch def
    /Composing true def
    /first true def
    gsave
    [ components {
	/elt exch def
	elt type /nametype eq {				% font
	    elt dup FM
	} { first {					% first string
	    /first false def
	    elt GetPathBox
	    %% Bounding box of overall glyphs.
	    /LEFT LLX def
	    /RIGHT URX def
	    /TOP URY def
	    /BOTTOM LLY def
	    currentfont /RelativeCompose known {
		/relative currentfont /RelativeCompose get def
	    } {
		%% Disable relative composition by setting sufficiently low
		%% and high positions.
		/relative [ -100000 100000 ] def
	    } ifelse
	    [ elt 0 0 ]
	} {						% other strings
	    elt GetPathBox
	    [ elt					% str
	      LLX 0 lt { RIGHT } { 0 } ifelse		% xoff
	      LLY relative 1 get ge {			% compose on TOP
		  TOP LLY sub RelativeCompositionSkip add	% yoff
		  /TOP TOP URY LLY sub add RelativeCompositionSkip add def
	      } { URY relative 0 get le {		% compose under BOTTOM
		  BOTTOM URY sub RelativeCompositionSkip sub % yoff
		  /BOTTOM BOTTOM URY LLY sub sub
			RelativeCompositionSkip sub def
	      } {
		  0					% yoff
		  URY TOP gt { /TOP URY def } if
		  LLY BOTTOM lt { /BOTTOM LLY def } if
	      } ifelse } ifelse
	      ]
	    URX RIGHT gt { /RIGHT URX def } if
	} ifelse } ifelse
    } forall ] /components exch def
    grestore

    %% Reflect special effects.
    SpecialEffect

    %% Draw components while ignoring effects other than shadow and outline.
    components ShowComponents
    /Composing false def

} def

%% Show rule-base composition.
/RBC {		% [ font0? str0 rule1 font1? str1 rule2 ... strN ]  |-  --
    /components exch def
    /Composing true def
    /first true def
    gsave
    [ components {
	/elt exch def
	elt type /nametype eq {				% font
	    elt dup FM
	} { elt type /integertype eq {			% rule
	    %% This RULE decoding should be compatible with macro
	    %% COMPOSITION_DECODE_RULE in emacs/src/composite.h.
	    elt 12 idiv dup 3 mod /grefx exch def 3 idiv /grefy exch def
	    elt 12 mod dup 3 mod /nrefx exch def 3 idiv /nrefy exch def
	} { first {					% first string
	    /first false def
	    elt GetPathBox
	    %% Bounding box of overall glyphs.
	    /LEFT LLX def
	    /RIGHT URX def
	    /TOP URY def
	    /BOTTOM LLY def
	    /WIDTH RIGHT LEFT sub def
	    [ elt 0 0 ]
	} {						% other strings
	    elt GetPathBox
	    /width URX LLX sub def
	    /height URY LLY sub def
	    /left LEFT [ 0 WIDTH 2 div WIDTH ] grefx get add
		[ 0 width 2 div width ] nrefx get sub def
	    /bottom [ TOP 0 BOTTOM TOP BOTTOM add 2 div ] grefy get
		[ height LLY neg 0 height 2 div ] nrefy get sub def
	    %% Update bounding box
	    left LEFT lt { /LEFT left def } if
	    left width add RIGHT gt { /RIGHT left width add def } if
	    /WIDTH RIGHT LEFT sub def
	    bottom BOTTOM lt { /BOTTOM bottom def } if
	    bottom height add TOP gt { /TOP bottom height add def } if
	    [ elt left LLX sub bottom LLY sub ]
	} ifelse } ifelse } ifelse
    } forall ] /components exch def
    grestore

    %% Reflect special effects.
    SpecialEffect

    %% Draw components while ignoring effects other than shadow and outline.
    components ShowComponents

    /Composing false def
} def
%%%% End of character composition handler

"
  "PostScript code for printing character composition.")

(defun ps-mule-string-ascii (str)
  (ps-set-font ps-current-font)
  (string-as-unibyte (encode-coding-string str 'iso-latin-1)))

;; Encode STR for a font specified by FONT-SPEC and return the result.
;; If necessary, it generates the PostScript code for the font and glyphs to
;; print STR.  If optional 4th arg HEADER-P is non-nil, it is assumed that STR
;; is for headers.
(defun ps-mule-string-encoding (font-spec str &optional no-setfont header-p)
  (let ((encoding (ps-mule-font-spec-encoding font-spec)))
    (setq str
	  (string-as-unibyte
	   (cond ((coding-system-p encoding)
		  (encode-coding-string str encoding))
		 ((functionp encoding)
		  (funcall encoding str))
		 (encoding
		  (error "Invalid coding system or function: %s" encoding))
		 (t
		  str))))
    (if (ps-mule-font-spec-src font-spec)
	(ps-mule-prepare-font font-spec str ps-mule-current-charset
			      (or no-setfont header-p)
			      header-p)
      (or no-setfont
	  (ps-set-font ps-current-font)))
    str))

;; Bitmap font support

(defvar ps-mule-bitmap-prologue-generated nil)

(defconst ps-mule-bitmap-prologue
  "%%%% Bitmap font handler

/str7 7 string def		% working area

%% We grow the dictionary one bunch (1024 entries) by one.
/BitmapDictArray 256 array def
/BitmapDictLength 1024 def
/BitmapDictIndex -1 def

/NewBitmapDict {		% --  |-  --
    /BitmapDictIndex BitmapDictIndex 1 add def
    BitmapDictArray BitmapDictIndex BitmapDictLength dict put
} bind def

%% Make at least one dictionary.
NewBitmapDict

/AddBitmap {			% gloval-charname bitmap-data  |-  --
    BitmapDictArray BitmapDictIndex get
    dup length BitmapDictLength ge {
	pop
	NewBitmapDict
	BitmapDictArray BitmapDictIndex get
    } if
    3 1 roll put
} bind def

/GetBitmap {			% gloval-charname  |-  bitmap-data
    0 1 BitmapDictIndex { BitmapDictArray exch get begin } for
    load
    0 1 BitmapDictIndex { pop end } for
} bind def

%% Return a global character name which can be used as a key in the
%% bitmap dictionary.
/GlobalCharName {		% fontidx code1 code2  |-  gloval-charname
    exch 256 mul add exch 65536 mul add 16777216 add 16 str7 cvrs 0 66 put
    str7 cvn
} bind def

%% Character code holder for a 2-byte character.
/FirstCode -1 def

%% Glyph rendering procedure
/BuildGlyphCommon {		% fontdict charname  |-  --
    1 index /FontDimension get 1 eq { /FirstCode 0 store } if
    NameIndexDict exch get	% STACK: fontdict charcode
    FirstCode 0 lt { %ifelse
	%% This is the first byte of a 2-byte character.  Just
	%% remember it for the moment.
	/FirstCode exch store
	pop
	0 0 setcharwidth
    } {
	1 index /FontSize get /size exch def
	1 index /FontSpaceWidthRatio get /ratio exch def
	1 index /FontIndex get exch FirstCode exch
	GlobalCharName GetBitmap /bmp exch def
	%% bmp == [ DWIDTH BBX-WIDTH BBX-HEIGHT BBX-XOFF BBX-YOFF BITMAP ]
	Composing { %ifelse
	    /FontMatrix get [ exch { size div } forall ] /mtrx exch def
	    bmp 3 get bmp 4 get mtrx transform
	    /LLY exch def /LLX exch def
	    bmp 1 get bmp 3 get add bmp 2 get bmp 4 get add mtrx transform
	    /URY exch def /URX exch def
	} {
	    pop
	} ifelse
	/FirstCode -1 store

	bmp 0 get SpaceWidthRatio ratio div mul size div 0	% wx wy
	setcharwidth			% We can't use setcachedevice here.

	bmp 1 get 0 gt bmp 2 get 0 gt and {
	    bmp 1 get bmp 2 get		% width height
	    true			% polarity
	    [ size 0 0 size neg bmp 3 get neg bmp 2 get bmp 4 get add ] % matrix
	    bmp 5 1 getinterval cvx	% datasrc
	    imagemask
	} if
    } ifelse
} bind def

/BuildCharCommon {
    1 index /Encoding get exch get
    1 index /BuildGlyph get exec
} bind def

%% Bitmap font creator

%% Common Encoding shared by all bitmap fonts.
/EncodingCommon 256 array def
%% Mapping table from character name to character code.
/NameIndexDict 256 dict def
0 1 255 { %for
    /idx exch def
    /idxname idx 256 add 16 (XXX) cvrs dup 0 67 put cvn def % `C' == 67
    EncodingCommon idx idxname put
    NameIndexDict idxname idx put
} for

/GlobalFontIndex 0 def

%% fontname dim col fontsize relative-compose baseline-offset fbbx  |-  --
/BitmapFont {
    15 dict begin
    /FontBBox exch def
    /BaselineOffset exch def
    /RelativeCompose exch def
    /FontSize exch def
    /FontBBox [ FontBBox { FontSize div } forall ] def
    FontBBox 2 get FontBBox 0 get sub exch div
    /FontSpaceWidthRatio exch def
    /FontDimension exch def
    /FontIndex GlobalFontIndex def
    /FontType 3 def
    /FontMatrix matrix def
    /Encoding EncodingCommon def
    /BuildGlyph { BuildGlyphCommon } def
    /BuildChar { BuildCharCommon } def
    currentdict end
    definefont pop
    /GlobalFontIndex GlobalFontIndex 1 add def
} bind def

%% Define a new bitmap font.
%% fontname dim col fontsize relative-compose baseline-offset fbbx  |-  --
/NF {
    /fbbx exch def
    %% Convert BDF's FontBoundingBox to PostScript's FontBBox
    [ fbbx 2 get fbbx 3 get
      fbbx 2 get fbbx 0 get add fbbx 3 get fbbx 1 get add ]
    BitmapFont
} bind def

%% Define a glyph for the specified font and character.
/NG {				% fontname charcode bitmap-data  |-  --
    /bmp exch def
    exch findfont dup /BaselineOffset get bmp 4 get add bmp exch 4 exch put
    /FontIndex get exch
    dup 256 idiv exch 256 mod GlobalCharName
    bmp AddBitmap
} bind def
%%%% End of bitmap font handler

")

;; External library support.

;; The following three functions are to be called from external
;; libraries which support bitmap fonts (e.g. `bdf') to get
;; appropriate PostScript code.

(defun ps-mule-generate-bitmap-prologue ()
  (unless ps-mule-bitmap-prologue-generated
    (setq ps-mule-bitmap-prologue-generated t)
    (list ps-mule-bitmap-prologue)))

(defun ps-mule-generate-bitmap-font (&rest args)
  (list (apply 'format "/%s %d %d %f %S %d %S NF\n" args)))

(defun ps-mule-generate-bitmap-glyph (font-name code dwidth bbx bitmap)
  (format "/%s %d [ %d %d %d %d %d <%s> ] NG\n"
	  font-name code
	  dwidth (aref bbx 0) (aref bbx 1) (aref bbx 2) (aref bbx 3)
	  bitmap))

;; Mule specific initializers.

;;;###autoload
(defun ps-mule-initialize ()
  "Initialize global data for printing multi-byte characters."
  (setq ps-mule-font-cache nil
	ps-mule-prologue-generated nil
	ps-mule-composition-prologue-generated nil
	ps-mule-bitmap-prologue-generated nil)
  (mapcar `(lambda (x) (setcar (nthcdr 2 x) nil))
	  ps-mule-external-libraries))

(defvar ps-mule-header-charsets nil)

;;;###autoload
(defun ps-mule-encode-header-string (string fonttag)
  "Generate PostScript code for ploting STRING by font FONTTAG.
FONTTAG should be a string \"/h0\" or \"/h1\"."
  (setq string (cond ((not (stringp string))
		      "")
		     ((multibyte-string-p string)
		      (copy-sequence string))
		     (t
		      (string-make-multibyte string))))
  (when ps-mule-header-charsets
    (if (eq (car ps-mule-header-charsets) 'latin-iso8859-1)
	;; Latin1 characters can be printed by the standard PostScript
	;; font.  Converts the other non-ASCII characters to `?'.
	(let ((len (length string))
	      (i 0))
	  (while (< i len)
	    (or (memq (char-charset (aref string i)) '(ascii latin-iso8859-1))
		(aset string i ??))
	    (setq i (1+ i)))
	  (setq string (encode-coding-string string 'iso-latin-1)))
      ;; We must prepare a font for the first non-ASCII and non-Latin1
      ;; character in STRING.
      (let* ((ps-current-font (if (string= fonttag "/h0") 0 1))
	     (ps-mule-current-charset (car ps-mule-header-charsets))
	     (font-type (car (nth ps-current-font
				  (ps-font-alist 'ps-font-for-header))))
	     (font-spec (ps-mule-get-font-spec ps-mule-current-charset
					       font-type)))
	(if (or (not font-spec)
		(/= (charset-dimension ps-mule-current-charset) 1))
	    ;; We don't have a proper font, or we can't print them on
	    ;; header because this kind of charset is not ASCII
	    ;; compatible.
	    (let ((len (length string))
		  (i 0))
	      (while (< i len)
		(or (memq (char-charset (aref string i))
			  '(ascii latin-iso8859-1))
		    (aset string i ??))
		(setq i (1+ i)))
	      (setq string (encode-coding-string string 'iso-latin-1)))
	  (let ((charsets (list 'ascii (car ps-mule-header-charsets)))
		(len (length string))
		(i 0))
	    (while (< i len)
	      (or (memq (char-charset (aref string i)) charsets)
		  (aset string i ??))
	      (setq i (1+ i))))
	  (setq string (ps-mule-string-encoding font-spec string nil t))))))
  string)

;;;###autoload
(defun ps-mule-header-string-charsets ()
  "Return a list of character sets that appears in header strings."
  (let ((str ""))
    (when ps-print-header
      (let ((tail (list ps-left-header ps-right-header)))
	(while tail
	  ;; Simulate what is done by ps-generate-header-line to get a
	  ;; string to plot.
	  (let ((count 0)
		(tmp (car tail)))
	    (setq tail (cdr tail))
	    (while (and tmp (< count ps-header-lines))
	      (let ((elt (car tmp)))
		(setq tmp (cdr tmp)
		      count (1+ count)
		      str (concat str
				  (cond ((stringp elt) elt)
					((and (symbolp elt) (fboundp elt))
					 (funcall elt))
					((and (symbolp elt) (boundp elt))
					 (symbol-value elt))
					(t ""))))))))))
    (let ((len (length str))
	  (i 0)
	  charset-list)
      (while (< i len)
	(let ((charset (char-charset (aref str i))))
	  (setq i (1+ i))
	  (or (eq charset 'ascii)
	      (memq charset charset-list)
	      (setq charset-list (cons charset charset-list)))))
      charset-list)))

;;;###autoload
(defun ps-mule-begin-job (from to)
  "Start printing job for multi-byte chars between FROM and TO.
This checks if all multi-byte characters in the region are printable or not."
  (setq ps-mule-charset-list nil
	ps-mule-header-charsets nil
	ps-mule-font-info-database
	(cond ((eq ps-multibyte-buffer 'non-latin-printer)
	       ps-mule-font-info-database-ps)
	      ((eq ps-multibyte-buffer 'bdf-font)
	       ps-mule-font-info-database-bdf)
	      ((eq ps-multibyte-buffer 'bdf-font-except-latin)
	       ps-mule-font-info-database-ps-bdf)
	      (t
	       ps-mule-font-info-database-default)))
  (and (boundp 'enable-multibyte-characters)
       enable-multibyte-characters
       ;; Initialize `ps-mule-charset-list'.  If some characters aren't
       ;; printable, warn it.
       (let ((charsets (find-charset-region from to)))
	 (setq charsets (delq 'ascii (delq 'unknown (delq nil charsets)))
	       ps-mule-charset-list charsets)
	 (save-excursion
	   (goto-char from)
	   (and (search-forward "\200" to t)
		(setq ps-mule-charset-list
		      (cons 'composition ps-mule-charset-list))))
	 ;; We also have to check non-ASCII charsets in the header strings.
	 (let ((tail (ps-mule-header-string-charsets)))
	   (while tail
	     (unless (eq (car tail) 'ascii)
	       (setq ps-mule-header-charsets
		     (cons (car tail) ps-mule-header-charsets))
	       (or (memq (car tail) charsets)
		   (setq charsets (cons (car tail) charsets))))
	     (setq tail (cdr tail))))
	 (while charsets
	   (setq charsets
		 (cond
		  ((or (eq (car charsets) 'composition)
		       (ps-mule-printable-p (car charsets)))
		   (cdr charsets))
		  ((y-or-n-p
		    "Font for some characters not found, continue anyway? ")
		   nil)
		  (t
		   (error "Printing cancelled")))))))

  (setq ps-mule-current-charset 'ascii)

  (if (and (nth 2 (find-composition from to))
	   (not ps-mule-composition-prologue-generated))
      (progn
	(ps-mule-prologue-generated)
	(ps-output-prologue ps-mule-composition-prologue)
	(setq ps-mule-composition-prologue-generated t)))

  (if (or ps-mule-charset-list ps-mule-header-charsets)
      (let ((the-list (append ps-mule-header-charsets ps-mule-charset-list))
	    font-spec elt)
	(ps-mule-prologue-generated)
	;; If external functions are necessary, generate prologues for them.
	(while the-list
	  (setq elt (car the-list)
		the-list (cdr the-list))
	  (cond ((and (eq elt 'composition)
		      (not ps-mule-composition-prologue-generated))
		 (ps-output-prologue ps-mule-composition-prologue)
		 (setq ps-mule-composition-prologue-generated t))
		((setq font-spec (ps-mule-get-font-spec elt 'normal))
		 (ps-mule-init-external-library font-spec))))))

  ;; If ASCII font is also specified in ps-mule-font-info-database,
  ;; use it instead of what specified in ps-font-info-database.
  (let ((font-spec (ps-mule-get-font-spec 'ascii 'normal)))
    (if font-spec
	(progn
	  (ps-mule-prologue-generated)
	  (ps-mule-init-external-library font-spec)
	  (let ((font (ps-font-alist 'ps-font-for-text))
		(ps-current-font 0))
	    (while font
	      ;; Be sure to download a glyph for SPACE in advance.
	      (ps-mule-prepare-font (ps-mule-get-font-spec 'ascii (car font))
				    " " 'ascii 'no-setfont)
	      (setq font (cdr font)
		    ps-current-font (1+ ps-current-font)))))))

  ;; If the header contains non-ASCII and non-Latin1 characters, prepare a font
  ;; and glyphs for the first occurance of such characters.
  (if (and ps-mule-header-charsets
	   (not (eq (car ps-mule-header-charsets) 'latin-iso8859-1)))
      (let ((font-spec (ps-mule-get-font-spec (car ps-mule-header-charsets)
					      'normal)))
	(if font-spec
	    ;; Be sure to download glyphs for "0123456789/" in advance for page
	    ;; numbering.
	    (let ((ps-current-font 0))
	      (ps-mule-prepare-font font-spec "0123456789/" 'ascii t t)))))

  (if ps-mule-charset-list
      ;; We must change this regexp for multi-byte buffer.
      (setq ps-control-or-escape-regexp
	    (cond ((eq ps-print-control-characters '8-bit)
		   "[^\040-\176]")
		  ((eq ps-print-control-characters 'control-8-bit)
		   (string-as-multibyte "[^\040-\176\240-\377]"))
		  ((eq ps-print-control-characters 'control)
		   (string-as-multibyte "[^\040-\176\200-\377]"))
		  (t (string-as-multibyte "[^\000-\011\013\015-\377]"))))))

;;;###autoload
(defun ps-mule-begin-page ()
  (setq ps-mule-current-charset 'ascii))


(provide 'ps-mule)

;;; ps-mule.el ends here
