;;; ps-mule.el --- Provide multi-byte character facility to ps-print.

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Author:	Kenichi Handa <handa@etl.go.jp> (multi-byte characters)
;; Maintainer:	Kenichi Handa <handa@etl.go.jp> (multi-byte characters)
;; Maintainer:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Keywords:	print, PostScript, multibyte, mule
;; Time-stamp:	<99/02/19 13:15:52 vinicius>

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;  nil                     This is the value to use when you are printing
;;			    buffer with only ASCII and Latin characters.
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
;;			    `intlfonts-1.1' which is a collection of X11 fonts
;;			    for all characters supported by Emacs.  In order to
;;			    use this value, be sure to have installed
;;			    `intlfonts-1.1' and set the variable
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-and-compile (require 'ps-print))

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
			  `ps-mule-font-info-database-latin' (which see).

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
			  `intlfonts-1.1' which is a collection of X11 fonts
			  for all characters supported by Emacs.  In order to
			  use this value, be sure to have installed
			  `intlfonts-1.1' and set the variable
			  `bdf-directory-list' appropriately (see ps-bdf.el for
			  documentation of this variable).

  `bdf-font-except-latin' This is like `bdf-font' except that it is used
			  PostScript default fonts to print ASCII and Latin-1
			  characters.  This is convenient when you want or
			  need to use both latin and non-latin characters on
			  the same buffer.  See `ps-font-family',
			  `ps-header-font-family' and `ps-font-info-database'.

Any other value is treated as nil."
  :type '(choice (const non-latin-printer) (const bdf-font)
		 (const bdf-font-except-latin) (other :tag "nil" nil))
  :group 'ps-print-font)

;; For Emacs 20.2 and the earlier version.
(eval-and-compile
  (if (not (string< mule-version "4.0"))
      (progn
	(defalias 'ps-mule-next-point '1+)
	(defalias 'ps-mule-chars-in-string 'length)
	(defalias 'ps-mule-string-char 'aref)
	(defsubst ps-mule-next-index (str i) (1+ i)))
    (defun ps-mule-next-point (arg)
      (save-excursion (goto-char arg) (forward-char 1) (point)))
    (defun ps-mule-chars-in-string (string)
      (/ (length string)
	 (charset-bytes (char-charset (string-to-char string)))))
    (defun ps-mule-string-char (string idx)
      (string-to-char (substring string idx)))
    (defun ps-mule-next-index (string i)
      (+ i (charset-bytes (char-charset (string-to-char string))))))
  )

(defvar ps-mule-font-info-database
  nil
  "Alist of charsets with the corresponding font information.
Each element has the form:

	(CHARSET (FONT-TYPE FONT-SRC FONT-NAME ENCODING BYTES) ...)

Where

CHARSET is a charset (symbol) for this font family,

FONT-TYPE is a font type: normal, bold, italic, or bold-italic.

FONT-SRC is a font source: builtin, ps-bdf, vflib, or nil.

  If FONT-SRC is builtin, FONT-NAME is a buitin PostScript font name.

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

(defvar ps-mule-font-info-database-default
  ps-mule-font-info-database-latin
  "The default setting to use if `ps-multibyte-buffer' (which see) is nil.")

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
     (normal bdf ("sish24-etl.bdf" "etl24-sisheng.bdf") ps-mule-encode-8bit 1))
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
     (normal bdf ("isci24-etl.bdf" "mule-iscii-24.bdf") ps-mule-encode-7bit 1))
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
     (normal bdf ("tib24-mule.bdf" "mule-tibmdx-24.bdf") ps-mule-encode-7bit 2)))
  "Sample setting of the `ps-mule-font-info-database' to use BDF fonts.
BDF (Bitmap Distribution Format) is a format used for distributing X's font
source file.

Current default value list for BDF fonts is included in `intlfonts-1.1' which is
a collection of X11 fonts for all characters supported by Emacs.

Using this list as default value to `ps-mule-font-info-database', all characters
including ASCII and Latin-1 are printed by BDF fonts.

See also `ps-mule-font-info-database-ps-bdf'.")

(defconst ps-mule-font-info-database-ps-bdf
  (cons (car ps-mule-font-info-database-latin)
	(cdr (cdr ps-mule-font-info-database-bdf)))
  "Sample setting of the `ps-mule-font-info-database' to use BDF fonts.

Current default value list for BDF fonts is included in `intlfonts-1.1' which is
a collection of X11 fonts for all characters supported by Emacs.

Using this list as default value to `ps-mule-font-info-database', all characters
except ASCII and Latin-1 characters are printed by BDF fonts.  ASCII and Latin-1
characters are printed by PostScript font specified by `ps-font-family' and
`ps-header-font-family'.

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

(defun ps-mule-encode-ethiopic (string)
  (ccl-execute-on-string (symbol-value 'ccl-encode-ethio-unicode)
			 (make-vector 9 nil)
			 string))

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
		(or (featurep (nth 1 slot)) (require (nth 1 slot)))
		(ps-output-prologue (funcall func))))
	  (setcar (nthcdr 2 slot) t)))))

;; Cached glyph information of fonts, alist of:
;;	(FONT-NAME ((FONT-TYPE-NUMBER . SCALED-FONT-NAME) ...)
;;	 cache CODE0 CODE1 ...)
(defvar ps-mule-font-cache nil)

(defun ps-mule-generate-font (font-spec charset)
  "Generate PostScript codes to define a new font in FONT-SPEC for CHARSET."
  (let* ((font-name (ps-mule-font-spec-name font-spec))
	 (font-name (if (consp font-name) (car font-name) font-name))
	 (font-cache (assoc font-name ps-mule-font-cache))
	 (font-src (ps-mule-font-spec-src font-spec))
	 (func (nth 4 (assq font-src ps-mule-external-libraries)))
	 (scaled-font-name
	  (if (eq charset 'ascii)
	      (format "f%d" ps-current-font)
	    (format "f%02x-%d"
		    (charset-id charset) ps-current-font))))
    (and func (not font-cache)
	 (ps-output-prologue (funcall func charset font-spec)))
    (ps-output-prologue
     (list (format "/%s %f /%s Def%sFontMule\n"
		   scaled-font-name ps-font-size-internal font-name
		   (if (eq ps-mule-current-charset 'ascii) "Ascii" ""))))
    (if font-cache
	(setcar (cdr font-cache)
		(cons (cons ps-current-font scaled-font-name)
		      (nth 1 font-cache)))
      (setq font-cache (list font-name
			     (list (cons ps-current-font scaled-font-name))
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

(defun ps-mule-prepare-font (font-spec string charset &optional no-setfont)
  "Generate PostScript codes to print STRING of CHARSET by font FONT-SPEC.

The generated code is inserted on prologue part except the code that sets the
current font (using PostScript procedure `FM').

If optional arg NO-SETFONT is non-nil, don't generate the code for setting the
current font."
  (let* ((font-name (ps-mule-font-spec-name font-spec))
	 (font-name (if (consp font-name) (car font-name) font-name))
	 (font-cache (assoc font-name ps-mule-font-cache)))
    (or (and font-cache (assq ps-current-font (nth 1 font-cache)))
	(setq font-cache (ps-mule-generate-font font-spec charset)))
    (or no-setfont
	(let ((new-font (cdr (assq ps-current-font (nth 1 font-cache)))))
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
	BaselineOffset false eq { /BaselinfOffset 0 def } if
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

%% Set the specified non-ASCII font to use.  It doesn't install
%% Ascent, etc.
/FM {				%  fontname  |-  --
  findfont setfont
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

%% Flag to tell if we are now handling a composite character.  This is
%% defined here because both composite character handler and bitmap font
%% handler require it.
/Cmpchar false def

%%%% End of Mule Section

"
  "PostScript code for printing multi-byte characters.")

(defvar ps-mule-prologue-generated nil)

(defun ps-mule-prologue-generated ()
  (unless ps-mule-prologue-generated
    (ps-output-prologue ps-mule-prologue)
    (setq ps-mule-prologue-generated t)))

(defun ps-mule-find-wrappoint (from to char-width)
  "Find the longest sequence which is printable in the current line.

The search starts at FROM and goes until TO.  It is assumed that all characters
between FROM and TO belong to a charset in `ps-mule-current-charset'.

CHAR-WIDTH is the average width of ASCII characters in the current font.

Returns the value:

	(ENDPOS . RUN-WIDTH)

Where ENDPOS is the end position of the sequence and RUN-WIDTH is the width of
the sequence."
  (if (eq ps-mule-current-charset 'composition)
      ;; We must draw one char by one.
      (let ((run-width (* (char-width (char-after from)) char-width)))
	(if (> run-width ps-width-remaining)
	    (cons from ps-width-remaining)
	  (cons (ps-mule-next-point from) run-width)))
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
  "Generate PostScript code for ploting characters in the region FROM and TO.

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

     ((eq ps-mule-current-charset 'composition)
      (let* ((ch (char-after from))
	     (width (char-width ch))
	     (ch-list (decompose-composite-char ch 'list t)))
	(if (consp (nth 1 ch-list))
	    (ps-mule-plot-rule-cmpchar ch-list width font-type)
	  (ps-mule-plot-cmpchar ch-list width t font-type))))

     (t
      ;; No way to print this charset.  Just show a vacant box of an
      ;; appropriate width.
      (ps-output (format "%d %d SB\n"
			 (length string)
			 (if (eq ps-mule-current-charset 'composition)
			     (char-width (char-after from))
			   (charset-width ps-mule-current-charset))))))
    wrappoint))

;; Composite font support

(defvar ps-mule-cmpchar-prologue-generated nil)

(defconst ps-mule-cmpchar-prologue
  "%%%% Composite character handler
/CmpcharWidth 0 def
/CmpcharRelativeCompose 0 def
/CmpcharRelativeSkip 0.4 def

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

%% Beginning of composite char.
/BC {				% str xoff width |-  --
    /Cmpchar true def
    /CmpcharWidth exch def
    currentfont /RelativeCompose known {
	/CmpcharRelativeCompose currentfont /RelativeCompose get def
    } {
	/CmpcharRelativeCompose false def
    } ifelse
    /bgsave bg def /bgcolorsave bgcolor def
    /Effectsave Effect def
    gsave			% Reflect effect only at first
	/Effect Effect 1 2 add 4 add 16 add and def
	/f0 findfont setfont (        ) 0 CmpcharWidth getinterval S
    grestore
    /Effect Effectsave 8 32 add and def	% enable only shadow and outline
    false BG
    gsave
	SpaceWidth mul 0 rmoveto dup GetPathBox S
	/RIGHT currentpoint pop def
    grestore
    /y currentpoint exch pop def
    /HIGH URY y add def /LOW LLY y add def
} bind def

%% End of composite char.
/EC {				% --  |-  --
    /bg bgsave def /bgcolor bgcolorsave def
    /Effect Effectsave def
    /Cmpchar false def
    CmpcharRelativeCompose false eq {
	CmpcharWidth SpaceWidth mul 0 rmoveto
    } {
	RIGHT currentpoint exch pop moveto
    } ifelse
} bind def

%% Rule base composition
/RBC {				% str xoff gref nref  |-  --
    /nref exch def /gref exch def
    gsave
    SpaceWidth mul 0 rmoveto
    dup
    GetPathBox
    [ HIGH currentpoint exch pop LOW HIGH LOW add 2 div ] gref get
    [ URY LLY sub LLY neg 0 URY LLY sub 2 div ] nref get
    sub /btm exch def
    /top btm URY LLY sub add def
    top HIGH gt { /HIGH top def } if
    btm LOW lt { /LOW btm def } if
    currentpoint pop btm LLY sub moveto
    S
    grestore
    /CmpcharRelativeCompose false def
} bind def

%% Relative composition
/RLC {				% str  |-  --
    gsave
    dup GetPathBox
    LLX 0 lt { RIGHT currentpoint exch pop moveto } if
    CmpcharRelativeCompose type /arraytype eq {
	LLY CmpcharRelativeCompose 1 get ge {	% compose on top
	    currentpoint pop HIGH LLY sub CmpcharRelativeSkip add moveto
	    /HIGH HIGH URY LLY sub add CmpcharRelativeSkip add def
	} { URY CmpcharRelativeCompose 0 get le { % compose under bottom
	    currentpoint pop LOW URY sub CmpcharRelativeSkip sub moveto
	    /LOW LOW URY LLY sub sub CmpcharRelativeSkip sub def
	} {
	    /y currentpoint exch pop def
	    y URY add dup HIGH gt { /HIGH exch def } { pop } ifelse
	    y LLY add dup LOW lt { /LOW exch def } { pop } ifelse
	} ifelse } ifelse } if
    S
    grestore
} bind def
%%%% End of composite character handler

"
  "PostScript code for printing composite characters.")

(defun ps-mule-plot-rule-cmpchar (ch-rule-list total-width font-type)
  (let ((leftmost 0.0)
	(rightmost (float (char-width (car ch-rule-list))))
	(the-list (cons '(3 . 3) ch-rule-list))
	cmpchar-elements)
    (while the-list
      (let* ((this (car the-list))
	     (gref (car this))
	     (nref (cdr this))
	     ;; X-axis info (0:left, 1:center, 2:right)
	     (gref-x (% gref 3))
	     (nref-x (% nref 3))
	     ;; Y-axis info (0:top, 1:base, 2:bottom, 3:center)
	     (gref-y (if (= gref 4) 3 (/ gref 3)))
	     (nref-y (if (= nref 4) 3 (/ nref 3)))
	     (char (car (cdr the-list)))
	     (width (float (char-width char)))
	     left)
	(setq left (+ leftmost
		      (* (- rightmost leftmost) gref-x 0.5)
		      (- (* nref-x width 0.5)))
	      cmpchar-elements (cons (list char left gref-y nref-y)
				     cmpchar-elements)
	      leftmost (min left leftmost)
	      rightmost (max (+ left width) rightmost)
	      the-list (nthcdr 2 the-list))))
    (if (< leftmost 0)
	(let ((the-list cmpchar-elements)
	      elt)
	  (while the-list
	    (setq elt (car the-list)
		  the-list (cdr the-list))
	    (setcar (cdr elt) (- (nth 1 elt) leftmost)))))
    (ps-mule-plot-cmpchar (nreverse cmpchar-elements)
			  total-width nil font-type)))

(defun ps-mule-plot-cmpchar (elements total-width relativep font-type)
  (let* ((elt (car elements))
	 (ch (if relativep elt (car elt))))
    (ps-output-string (ps-mule-prepare-cmpchar-font ch font-type))
    (ps-output (format " %d %d BC "
		       (if relativep 0 (nth 1 elt))
		       total-width))
    (while (setq elements (cdr elements))
      (setq elt (car elements)
	    ch (if relativep elt (car elt)))
      (ps-output-string (ps-mule-prepare-cmpchar-font ch font-type))
      (ps-output (if relativep
		     " RLC "
		   (format " %d %d %d RBC "
			   (nth 1 elt) (nth 2 elt) (nth 3 elt))))))
  (ps-output "EC\n"))

(defun ps-mule-prepare-cmpchar-font (char font-type)
  (let* ((ps-mule-current-charset (char-charset char))
	 (font-spec (ps-mule-get-font-spec ps-mule-current-charset font-type)))
    (cond (font-spec
	   (ps-mule-string-encoding font-spec (char-to-string char)))

	  ((eq ps-mule-current-charset 'latin-iso8859-1)
	   (ps-mule-string-ascii (char-to-string char)))

	  (t
	   ;; No font for CHAR.
	   (ps-set-font ps-current-font)
	   " "))))

(defun ps-mule-string-ascii (str)
  (ps-set-font ps-current-font)
  (string-as-unibyte (encode-coding-string str 'iso-latin-1)))

(defun ps-mule-string-encoding (font-spec str)
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
	(ps-mule-prepare-font font-spec str ps-mule-current-charset)
      (ps-set-font ps-current-font))
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
	Cmpchar { %ifelse
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

%% Bitmap font creater

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
	ps-mule-cmpchar-prologue-generated nil
	ps-mule-bitmap-prologue-generated nil)
  (mapcar `(lambda (x) (setcar (nthcdr 2 x) nil))
	  ps-mule-external-libraries))

;;;###autoload
(defun ps-mule-begin-job (from to)
  "Start printing job for multi-byte chars between FROM and TO.
This checks if all multi-byte characters in the region are printable or not."
  (setq ps-mule-charset-list nil
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

  (if ps-mule-charset-list
      (let ((the-list ps-mule-charset-list)
	    font-spec elt)
	(ps-mule-prologue-generated)
	;; If external functions are necessary, generate prologues for them.
	(while the-list
	  (setq elt (car the-list)
		the-list (cdr the-list))
	  (cond ((and (eq elt 'composition)
		      (not ps-mule-cmpchar-prologue-generated))
		 (ps-output-prologue ps-mule-cmpchar-prologue)
		 (setq ps-mule-cmpchar-prologue-generated t))
		((setq font-spec (ps-mule-get-font-spec elt 'normal))
		 (ps-mule-init-external-library font-spec))))))

  ;; If ASCII font is also specified in ps-mule-font-info-database,
  ;; use it istead of what specified in ps-font-info-database.
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
