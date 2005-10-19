;;; fontset.el --- commands for handling fontset

;; Copyright (C) 1997, 1998, 1999, 2000, 2001  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: mule, multilingual, fontset

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

;;; Code:

;; Set standard fontname specification of characters in the default
;; fontset to find an appropriate font for each charset.  This is used
;; to generate a font name for a fontset if the fontset doesn't
;; specify a font name for a specific character.  The specification
;; has the form (FAMILY . REGISTRY).  FAMILY may be nil, in which
;; case, the family name of default face is used.  If REGISTRY
;; contains a character `-', the string before that is embedded in
;; `CHARSET_REGISTRY' field, and the string after that is embedded in
;; `CHARSET_ENCODING' field.  If it does not contain `-', the whole
;; string is embedded in `CHARSET_REGISTRY' field, and a wild card
;; character `*' is embedded in `CHARSET_ENCODING' field.  The
;; REGISTRY for ASCII characters are predefined as "ISO8859-1".

(defun setup-default-fontset ()
  "Setup the default fontset."
  (dolist (elt
	   `((latin-iso8859-1 . (nil . "ISO8859-1"))
	     (latin-iso8859-2 . (nil . "ISO8859-2"))
	     (latin-iso8859-3 . (nil . "ISO8859-3"))
	     (latin-iso8859-4 . (nil . "ISO8859-4"))
	     ;; Setting "*" family is for a workaround of the problem
	     ;; that a font of wrong size is preferred if the font
	     ;; family matches with a requested one.
	     (thai-tis620 . ("*" . "TIS620"))
	     (greek-iso8859-7 . (nil . "ISO8859-7"))
	     (arabic-iso8859-6 . (nil . "ISO8859-6"))
	     (hebrew-iso8859-8 . (nil . "ISO8859-8"))
	     (katakana-jisx0201 . (nil . "JISX0201"))
	     (latin-jisx0201 . (nil . "JISX0201"))
	     (cyrillic-iso8859-5 . (nil . "ISO8859-5"))
	     (latin-iso8859-9 . (nil . "ISO8859-9"))
	     (japanese-jisx0208-1978 . (nil . "JISX0208.1978"))
	     (chinese-gb2312 . (nil . "GB2312.1980"))
	     (japanese-jisx0208 . (nil . "JISX0208.1990"))
	     (korean-ksc5601 . (nil . "KSC5601.1989"))
	     (japanese-jisx0212 . (nil . "JISX0212"))
	     (chinese-cns11643-1 . (nil . "CNS11643.1992-1"))
	     (chinese-cns11643-2 . (nil . "CNS11643.1992-2"))
	     (chinese-cns11643-3 . (nil . "CNS11643.1992-3"))
	     (chinese-cns11643-4 . (nil . "CNS11643.1992-4"))
	     (chinese-cns11643-5 . (nil . "CNS11643.1992-5"))
	     (chinese-cns11643-6 . (nil . "CNS11643.1992-6"))
	     (chinese-cns11643-7 . (nil . "CNS11643.1992-7"))
	     (chinese-big5-1 . (nil . "Big5"))
	     (chinese-big5-2 . (nil . "Big5"))
	     (chinese-sisheng . (nil . "sisheng_cwnn"))
	     (vietnamese-viscii-lower . (nil . "VISCII1.1"))
	     (vietnamese-viscii-upper . (nil . "VISCII1.1"))
	     (arabic-digit . (nil . "MuleArabic-0"))
	     (arabic-1-column . (nil . "MuleArabic-1"))
	     (arabic-2-column . (nil . "MuleArabic-2"))
	     (ipa . (nil . "MuleIPA"))
	     (ethiopic . (nil . "Ethiopic-Unicode"))
	     (ascii-right-to-left . (nil . "ISO8859-1"))
	     (indian-is13194 . (nil . "IS13194-Devanagari"))
	     (indian-2-column . (nil . "MuleIndian-2"))
	     (lao . (nil . "MuleLao-1"))
	     (tibetan . ("proportional" . "MuleTibetan-2"))
	     (tibetan-1-column . (nil . "MuleTibetan-1"))
	     (latin-iso8859-14 . (nil . "ISO8859-14"))
	     (latin-iso8859-15 . (nil . "ISO8859-15"))
	     (mule-unicode-0100-24ff . (nil . "ISO10646-1"))
	     (mule-unicode-2500-33ff . (nil . "ISO10646-1"))
	     (mule-unicode-e000-ffff . (nil . "ISO10646-1"))
	     (japanese-jisx0213-1 . (nil . "JISX0213.2000-1"))
	     (japanese-jisx0213-2 . (nil . "JISX0213.2000-2"))
	     ;; unicode
	     ((,(decode-char 'ucs #x0900) . ,(decode-char 'ucs #x097F))
	      . (nil . "ISO10646.indian-1"))
	     ;; Indian CDAC
	     (,(indian-font-char-range 'cdac:dv-ttsurekh)
	      . (nil . "Devanagari-CDAC"))
	     (,(indian-font-char-range 'cdac:sd-ttsurekh)
	      . (nil . "Sanskrit-CDAC"))
	     (,(indian-font-char-range 'cdac:bn-ttdurga)
	      . (nil . "Bengali-CDAC"))
	     (,(indian-font-char-range 'cdac:as-ttdurga)
	      . (nil . "Assamese-CDAC"))
	     (,(indian-font-char-range 'cdac:pn-ttamar)
	      . (nil . "Punjabi-CDAC"))
	     (,(indian-font-char-range 'cdac:gj-ttavantika)
	      . (nil . "Gujarati-CDAC"))
	     (,(indian-font-char-range 'cdac:or-ttsarala)
	      . (nil . "Oriya-CDAC"))
	     (,(indian-font-char-range 'cdac:tm-ttvalluvar)
	      . (nil . "Tamil-CDAC"))
	     (,(indian-font-char-range 'cdac:tl-tthemalatha)
	      . (nil . "Telugu-CDAC"))
	     (,(indian-font-char-range 'cdac:kn-ttuma)
	      . (nil . "Kannada-CDAC"))
	     (,(indian-font-char-range 'cdac:ml-ttkarthika)
	      . (nil . "Malayalam-CDAC"))
	     ;; Indian AKRUTI
	     (,(indian-font-char-range 'akruti:dev)
	      . (nil . "Devanagari-Akruti"))
	     (,(indian-font-char-range 'akruti:bng)
	      . (nil . "Bengali-Akruti"))
	     (,(indian-font-char-range 'akruti:pnj)
	      . (nil . "Punjabi-Akruti"))
	     (,(indian-font-char-range 'akruti:guj)
	      . (nil . "Gujarati-Akruti"))
	     (,(indian-font-char-range 'akruti:ori)
	      . (nil . "Oriay-Akruti"))
	     (,(indian-font-char-range 'akruti:tml)
	      . (nil . "Tamil-Akruti"))
	     (,(indian-font-char-range 'akruti:tlg)
	      . (nil . "Telugu-Akruti"))
	     (,(indian-font-char-range 'akruti:knd)
	      . (nil . "Kannada-Akruti"))
	     (,(indian-font-char-range 'akruti:mal)
	      . (nil . "Malayalam-Akruti"))
	     ))
    (set-fontset-font "fontset-default" (car elt) (cdr elt))))

;; Set arguments in `font-encoding-alist' (which see).
(defun set-font-encoding (pattern charset encoding)
  (let ((slot (assoc pattern font-encoding-alist)))
    (if slot
	(let ((place (assq charset (cdr slot))))
	  (if place
	      (setcdr place encoding)
	    (setcdr slot (cons (cons charset encoding) (cdr slot)))))
      (setq font-encoding-alist
	    (cons (list pattern (cons charset encoding)) font-encoding-alist)))
    ))

;; Allow display of arbitrary characters with an iso-10646-encoded
;; (`Unicode') font.
(define-translation-table 'ucs-mule-to-mule-unicode
  ucs-mule-to-mule-unicode)
(define-translation-hash-table 'ucs-mule-cjk-to-unicode
  ucs-mule-cjk-to-unicode)

(define-ccl-program ccl-encode-unicode-font
  `(0
    ;; r0: charset-id
    ;; r1: 1st position code
    ;; r2: 2nd position code (if r0 is 2D charset)
    ((if (r0 == ,(charset-id 'ascii))
	 ((r2 = r1)
	  (r1 = 0))
       ;; At first, try to get a Unicode code point directly.
       ((if (r2 >= 0)
	    ;; This is a 2D charset.
	    (r1 = ((r1 << 7) | r2)))
	(lookup-character utf-subst-table-for-encode r0 r1)
	(if r7
	    ;; We got it!
	    ((r1 = (r0 >> 8))
	     (r2 = (r0 & #xFF)))
	  ;; Look for a translation for non-ASCII chars.
	  ((translate-character ucs-mule-to-mule-unicode r0 r1)
	   (if (r0 == ,(charset-id 'ascii))
	       ((r2 = r1)
		(r1 = 0))
	     ((if (r0 == ,(charset-id 'latin-iso8859-1))
		  ((r2 = (r1 + 128))
		   (r1 = 0))
		((r2 = (r1 & #x7F))
		 (r1 >>= 7)
		 (if (r0 == ,(charset-id 'mule-unicode-0100-24ff))
		     ((r1 *= 96)
		      (r1 += r2)
		      (r1 += ,(- #x100 (* 32 96) 32))
		      (r1 >8= 0)
		      (r2 = r7))
		   (if (r0 == ,(charset-id 'mule-unicode-2500-33ff))
		       ((r1 *= 96)
			(r1 += r2)
			(r1 += ,(- #x2500 (* 32 96) 32))
			(r1 >8= 0)
			(r2 = r7))
		     (if (r0 == ,(charset-id 'mule-unicode-e000-ffff))
			 ((r1 *= 96)
			  (r1 += r2)
			  (r1 += ,(- #xe000 (* 32 96) 32))
			  (r1 >8= 0)
			  (r2 = r7))
		       ;; No way, use the glyph for U+FFFD.
		       ((r1 = #xFF)
			(r2 = #xFD)))))))))))))))
  "Encode characters for display with iso10646 font.
Translate through the translation-hash-table named
`ucs-mule-cjk-to-unicode' and the translation-table named
`ucs-mule-to-mule-unicode' initially.")

;; Use the above CCL encoder for Unicode fonts.  Please note that the
;; regexp is not simply "ISO10646-1" because there exists, for
;; instance, the following Devanagari Unicode fonts:
;;	-misc-fixed-medium-r-normal--24-240-72-72-c-120-iso10646.indian-1
;;	-sibal-devanagari-medium-r-normal--24-240-75-75-P--iso10646-dev
(setq font-ccl-encoder-alist
      (cons '("ISO10646.*-*" . ccl-encode-unicode-font)
	    font-ccl-encoder-alist))

;; Setting for suppressing XLoadQueryFont on big fonts.
(setq x-pixel-size-width-font-regexp
      "gb2312\\|jisx0208\\|ksc5601\\|cns11643\\|big5")

;; These fonts require vertical centering.
(setq vertical-centering-font-regexp
      "gb2312\\|jisx0208\\|jisx0212\\|ksc5601\\|cns11643\\|big5")

;; CDAC fonts are actually smaller than their design sizes.
(setq face-font-rescale-alist
      '(("-cdac$" . 1.3)))

(defvar x-font-name-charset-alist
  '(("iso8859-1" ascii latin-iso8859-1)
    ("iso8859-2" ascii latin-iso8859-2)
    ("iso8859-3" ascii latin-iso8859-3)
    ("iso8859-4" ascii latin-iso8859-4)
    ("iso8859-5" ascii cyrillic-iso8859-5)
    ("iso8859-6" ascii arabic-iso8859-6)
    ("iso8859-7" ascii greek-iso8859-7)
    ("iso8859-8" ascii hebrew-iso8859-8)
    ("iso8859-14" ascii latin-iso8859-14)
    ("iso8859-15" ascii latin-iso8859-15)
    ("tis620" ascii thai-tis620)
    ("koi8" ascii cyrillic-iso8859-5)
    ("viscii" ascii vietnamese-viscii-upper vietnamese-viscii-lower)
    ("vscii" ascii vietnamese-viscii-upper vietnamese-viscii-lower)
    ("mulelao-1" ascii lao)
    ("iso10646-1" ascii latin-iso8859-1 mule-unicode-0100-24ff
     mule-unicode-2500-33ff mule-unicode-e000-ffff))
  "Alist of font names vs list of charsets the font can display.

When a font name which matches some element of this alist is given as
`-fn' command line argument or is specified by X resource, a fontset
which uses the specified font for the corresponding charsets are
created and used for the initial frame.")

;;; XLFD (X Logical Font Description) format handler.

;; Define XLFD's field index numbers.		; field name
(defconst xlfd-regexp-foundry-subnum 0)		; FOUNDRY
(defconst xlfd-regexp-family-subnum 1)		; FAMILY_NAME
(defconst xlfd-regexp-weight-subnum 2)		; WEIGHT_NAME
(defconst xlfd-regexp-slant-subnum 3)		; SLANT
(defconst xlfd-regexp-swidth-subnum 4)		; SETWIDTH_NAME
(defconst xlfd-regexp-adstyle-subnum 5)		; ADD_STYLE_NAME
(defconst xlfd-regexp-pixelsize-subnum 6)	; PIXEL_SIZE
(defconst xlfd-regexp-pointsize-subnum 7)	; POINT_SIZE
(defconst xlfd-regexp-resx-subnum 8)		; RESOLUTION_X
(defconst xlfd-regexp-resy-subnum 9)		; RESOLUTION_Y
(defconst xlfd-regexp-spacing-subnum 10)	; SPACING
(defconst xlfd-regexp-avgwidth-subnum 11)	; AVERAGE_WIDTH
(defconst xlfd-regexp-registry-subnum 12)	; CHARSET_REGISTRY
(defconst xlfd-regexp-encoding-subnum 13)	; CHARSET_ENCODING

;; Regular expression matching against a fontname which conforms to
;; XLFD (X Logical Font Description).  All fields in XLFD should be
;; not be omitted (but can be a wild card) to be matched.
(defconst xlfd-tight-regexp
  "^\
-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)\
-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)\
-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)$")

;; List of field numbers of XLFD whose values are numeric.
(defconst xlfd-regexp-numeric-subnums
  (list xlfd-regexp-pixelsize-subnum	;6
	xlfd-regexp-pointsize-subnum	;7
	xlfd-regexp-resx-subnum		;8
	xlfd-regexp-resy-subnum		;9
	xlfd-regexp-avgwidth-subnum	;11
	))

(defun x-decompose-font-name (pattern)
  "Decompose PATTERN into XLFD's fields and return vector of the fields.
The length of the vector is 14.

If PATTERN doesn't conform to XLFD, try to get a full XLFD name from
X server and use the information of the full name to decompose
PATTERN.  If no full XLFD name is gotten, return nil."
  (let (xlfd-fields fontname)
    (if (string-match xlfd-tight-regexp pattern)
	(let ((i 0))
	  (setq xlfd-fields (make-vector 14 nil))
	  (while (< i 14)
	    (aset xlfd-fields i (match-string (1+ i) pattern))
	    (setq i (1+ i)))
	  xlfd-fields)
      (setq fontname (condition-case nil
			 (x-resolve-font-name pattern)
		       (error)))
      (if (and fontname
	       (string-match xlfd-tight-regexp fontname))
	  ;; We get a full XLFD name.
	  (let ((len (length pattern))
		(i 0)
		l)
	    ;; Setup xlfd-fields by the full XLFD name.  Each element
	    ;; should be a cons of matched index and matched string.
	    (setq xlfd-fields (make-vector 14 nil))
	    (while (< i 14)
	      (aset xlfd-fields i
		    (cons (match-beginning (1+ i))
			  (match-string (1+ i) fontname)))
	      (setq i (1+ i)))

	    ;; Replace wild cards in PATTERN by regexp codes.
	    (setq i 0)
	    (while (< i len)
	      (let ((ch (aref pattern i)))
		(if (= ch ??)
		    (setq pattern (concat (substring pattern 0 i)
					  "\\(.\\)"
					  (substring pattern (1+ i)))
			  len (+ len 4)
			  i (+ i 4))
		  (if (= ch ?*)
		      (setq pattern (concat (substring pattern 0 i)
					    "\\(.*\\)"
					    (substring pattern (1+ i)))
			    len (+ len 5)
			    i (+ i 5))
		    (setq i (1+ i))))))

	    ;; Set each element of xlfd-fields to proper strings.
	    (if (string-match pattern fontname)
		;; The regular expression PATTERN matchs the full XLFD
		;; name.  Set elements that correspond to a wild card
		;; in PATTERN to "*", set the other elements to the
		;; exact strings in PATTERN.
		(let ((l (cdr (cdr (match-data)))))
		  (setq i 0)
		  (while (< i 14)
		    (if (or (null l) (< (car (aref xlfd-fields i)) (car l)))
			(progn
			  (aset xlfd-fields i (cdr (aref xlfd-fields i)))
			  (setq i (1+ i)))
		      (if (< (car (aref xlfd-fields i)) (car (cdr l)))
			  (progn
			    (aset xlfd-fields i "*")
			    (setq i (1+ i)))
			(setq l (cdr (cdr l)))))))
	      ;; Set each element of xlfd-fields to the exact string
	      ;; in the corresonding fields in full XLFD name.
	      (setq i 0)
	      (while (< i 14)
		(aset xlfd-fields i (cdr (aref xlfd-fields i)))
		(setq i (1+ i))))
	    xlfd-fields)))))

;; Replace consecutive wild-cards (`*') in NAME to one.
;; Ex. (x-reduce-font-name "-*-*-*-iso8859-1") => "-*-iso8859-1"
(defsubst x-reduce-font-name (name)
  (while (string-match "-\\*-\\(\\*-\\)+" name)
    (setq name (replace-match "-*-" t t name)))
  name)

(defun x-compose-font-name (fields &optional reduce)
  "Compose X's fontname from FIELDS.
FIELDS is a vector of XLFD fields, the length 14.
If a field is nil, wild-card letter `*' is embedded.
Optional argument REDUCE is always ignored.  It exists just for
backward compatibility."
  (concat "-" (mapconcat (lambda (x) (or x "*")) fields "-")))


(defun x-must-resolve-font-name (xlfd-fields)
  "Like `x-resolve-font-name', but always return a font name.
XLFD-FIELDS is a vector of XLFD (X Logical Font Description) fields.
If no font matching XLFD-FIELDS is available, successively replace
parts of the font name pattern with \"*\" until some font is found.
Value is name of that font."
  (let ((ascii-font nil) (index 0))
    (while (and (null ascii-font) (<= index xlfd-regexp-encoding-subnum))
      (let ((pattern (x-compose-font-name xlfd-fields)))
	(condition-case nil
	    (setq ascii-font (x-resolve-font-name pattern))
	  (error
	   (message "Warning: no fonts matching `%s' available" pattern)
	   (aset xlfd-fields index "*")
	   (setq index (1+ index))))))
    (unless ascii-font
      (error "No fonts found"))
    ascii-font))


(defun x-complement-fontset-spec (xlfd-fields fontlist)
  "Complement FONTLIST for charsets based on XLFD-FIELDS and return it.
XLFD-FIELDS is a vector of XLFD (X Logical Font Description) fields.
FONTLIST is an alist of charsets vs the corresponding font names.

The fonts are complemented as below.

If FONTLIST doesn't specify a font for ASCII charset, generate a font
name for the charset from XLFD-FIELDS, and add that information to
FONTLIST.

If a font specifid for ASCII supports the other charsets (see the
variable `x-font-name-charset-alist'), add that information to FONTLIST."
  (let* ((slot (assq 'ascii fontlist))
	 (ascii-font (cdr slot))
	 ascii-font-spec)
    (if ascii-font
	(setcdr slot (setq ascii-font (x-resolve-font-name ascii-font)))
      ;; If font for ASCII is not specified, add it.
      (aset xlfd-fields xlfd-regexp-registry-subnum "iso8859")
      (aset xlfd-fields xlfd-regexp-encoding-subnum "1")
      (setq ascii-font (x-must-resolve-font-name xlfd-fields))
      (setq fontlist (cons (cons 'ascii ascii-font) fontlist)))

    ;; If the font for ASCII also supports the other charsets, and
    ;; they are not specified in FONTLIST, add them.
    (setq xlfd-fields (x-decompose-font-name ascii-font))
    (if (not xlfd-fields)
	(setq ascii-font-spec ascii-font)
      (setq ascii-font-spec
	    (cons (format "%s-%s"
			  (aref xlfd-fields xlfd-regexp-foundry-subnum)
			  (aref xlfd-fields xlfd-regexp-family-subnum))
		  (format "%s-%s"
			  (aref xlfd-fields xlfd-regexp-registry-subnum)
			  (aref xlfd-fields xlfd-regexp-encoding-subnum)))))
    (let ((tail x-font-name-charset-alist)
	  elt)
      (while tail
	(setq elt (car tail) tail (cdr tail))
	(if (string-match (car elt) ascii-font)
	    (let ((charsets (cdr elt))
		  charset)
	      (while charsets
		(setq charset (car charsets) charsets (cdr charsets))
		(or (assq charset fontlist)
		    (setq fontlist
			  (cons (cons charset ascii-font-spec) fontlist))))))))

    fontlist))

(defun fontset-name-p (fontset)
  "Return non-nil if FONTSET is valid as fontset name.
A valid fontset name should conform to XLFD (X Logical Font Description)
with \"fontset\" in `<CHARSET_REGISTRY> field."
  (and (string-match xlfd-tight-regexp fontset)
       (string= (match-string (1+ xlfd-regexp-registry-subnum) fontset)
		"fontset")))

;; Return a list to be appended to `x-fixed-font-alist' when
;; `mouse-set-font' is called.
(defun generate-fontset-menu ()
  (let ((fontsets (fontset-list))
	fontset-name
	l)
    (while fontsets
      (setq fontset-name (car fontsets) fontsets (cdr fontsets))
      (setq l (cons (list (fontset-plain-name fontset-name) fontset-name) l)))
    (cons "Fontset"
	  (sort l (function (lambda (x y) (string< (car x) (car y))))))))

(defun fontset-plain-name (fontset)
  "Return a plain and descriptive name of FONTSET."
  (if (not (setq fontset (query-fontset fontset)))
      (error "Invalid fontset: %s" fontset))
  (let ((xlfd-fields (x-decompose-font-name fontset)))
    (if xlfd-fields
	(let ((weight (aref xlfd-fields xlfd-regexp-weight-subnum))
	      (slant  (aref xlfd-fields xlfd-regexp-slant-subnum))
	      (swidth (aref xlfd-fields xlfd-regexp-swidth-subnum))
	      (size   (aref xlfd-fields xlfd-regexp-pixelsize-subnum))
	      (charset (aref xlfd-fields xlfd-regexp-registry-subnum))
	      (nickname (aref xlfd-fields xlfd-regexp-encoding-subnum))
	      name)
	  (if (not (string= "fontset" charset))
	      fontset
	    (if (> (string-to-number size) 0)
		(setq name (format "%s: %s-dot" nickname size))
	      (setq name nickname))
	    (cond ((string-match "^medium$" weight)
		   (setq name (concat name " " "medium")))
		  ((string-match "^bold$\\|^demibold$" weight)
		   (setq name (concat name " " weight))))
	    (cond ((string-match "^i$" slant)
		   (setq name (concat name " " "italic")))
		  ((string-match "^o$" slant)
		   (setq name (concat name " " "slant")))
		  ((string-match "^ri$" slant)
		   (setq name (concat name " " "reverse italic")))
		  ((string-match "^ro$" slant)
		   (setq name (concat name " " "reverse slant"))))
	    name))
      fontset)))


(defun create-fontset-from-fontset-spec (fontset-spec
					 &optional style-variant noerror)
  "Create a fontset from fontset specification string FONTSET-SPEC.
FONTSET-SPEC is a string of the format:
	FONTSET-NAME,CHARSET-NAME0:FONT-NAME0,CHARSET-NAME1:FONT-NAME1, ...
Any number of SPACE, TAB, and NEWLINE can be put before and after commas.

Optional 2nd argument is ignored.  It exists just for backward
compatibility.

If this function attempts to create already existing fontset, error is
signaled unless the optional 3rd argument NOERROR is non-nil.

It returns a name of the created fontset."
  (if (not (string-match "^[^,]+" fontset-spec))
      (error "Invalid fontset spec: %s" fontset-spec))
  (setq fontset-spec (downcase fontset-spec))
  (let ((idx (match-end 0))
	(name (match-string 0 fontset-spec))
	xlfd-fields charset fontlist ascii-font)
    (if (query-fontset name)
	(or noerror
	    (error "Fontset \"%s\" already exists" name))
      (setq xlfd-fields (x-decompose-font-name name))
      (or xlfd-fields
	  (error "Fontset \"%s\" not conforming to XLFD" name))

      ;; At first, extract pairs of charset and fontname from FONTSET-SPEC.
      (while (string-match "[, \t\n]*\\([^:]+\\):\\([^,]+\\)" fontset-spec idx)
	(setq idx (match-end 0))
	(setq charset (intern (match-string 1 fontset-spec)))
	(if (charsetp charset)
	    (setq fontlist (cons (cons charset (match-string 2 fontset-spec))
				 fontlist))))
      (setq ascii-font (cdr (assq 'ascii fontlist)))

      ;; Complement FONTLIST.
      (setq fontlist (x-complement-fontset-spec xlfd-fields fontlist))

      (new-fontset name fontlist)

      ;; Define the short name alias.
      (if (and (string-match "fontset-.*$" name)
	       (not (assoc name fontset-alias-alist)))
	  (let ((alias (match-string 0 name)))
	    (or (rassoc alias fontset-alias-alist)
		(setq fontset-alias-alist
		      (cons (cons name alias) fontset-alias-alist)))))

      ;; Define the ASCII font name alias.
      (or ascii-font
	  (setq ascii-font (cdr (assq 'ascii fontlist))))
      (or (rassoc ascii-font fontset-alias-alist)
	  (setq fontset-alias-alist
		(cons (cons name ascii-font)
		      fontset-alias-alist))))

    name))

(defun create-fontset-from-ascii-font (font &optional resolved-font
					    fontset-name)
  "Create a fontset from an ASCII font FONT.

Optional 1st arg RESOLVED-FONT is a resolved name of FONT.  If
omitted, `x-resolve-font-name' is called to get the resolved name.  At
this time, if FONT is not available, error is signaled.

Optional 2nd arg FONTSET-NAME is a string to be used in
`<CHARSET_ENCODING>' fields of a new fontset name.  If it is omitted,
an appropriate name is generated automatically.

It returns a name of the created fontset."
  (setq font (downcase font))
  (if resolved-font
      (setq resolved-font (downcase resolved-font))
    (setq resolved-font (downcase (x-resolve-font-name font))))
  (let ((xlfd (x-decompose-font-name font))
	(resolved-xlfd (x-decompose-font-name resolved-font))
	fontset fontset-spec)
    (aset xlfd xlfd-regexp-foundry-subnum nil)
    (aset xlfd xlfd-regexp-family-subnum nil)
    (aset xlfd xlfd-regexp-registry-subnum "fontset")
    (if fontset-name
	(setq fontset-name (downcase fontset-name))
      (setq fontset-name
	    (format "%s_%s_%s"
		    (aref resolved-xlfd xlfd-regexp-registry-subnum)
		    (aref resolved-xlfd xlfd-regexp-encoding-subnum)
		    (aref resolved-xlfd xlfd-regexp-pixelsize-subnum))))
    (aset xlfd xlfd-regexp-encoding-subnum fontset-name)
    (setq fontset (x-compose-font-name xlfd))
    (or (query-fontset fontset)
	(create-fontset-from-fontset-spec (concat fontset ", ascii:" font)))))


;; Create standard fontset from 16 dots fonts which are the most widely
;; installed fonts.  Fonts for Chinese-GB, Korean, and Chinese-CNS are
;; specified here because FAMILY of those fonts are not "fixed" in
;; many cases.
(defvar standard-fontset-spec
  (purecopy "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard,
	chinese-gb2312:-*-medium-r-normal-*-16-*-gb2312*-*,
	korean-ksc5601:-*-medium-r-normal-*-16-*-ksc5601*-*,
	chinese-cns11643-1:-*-medium-r-normal-*-16-*-cns11643*-1,
	chinese-cns11643-2:-*-medium-r-normal-*-16-*-cns11643*-2,
	chinese-cns11643-3:-*-medium-r-normal-*-16-*-cns11643*-3,
	chinese-cns11643-4:-*-medium-r-normal-*-16-*-cns11643*-4,
	chinese-cns11643-5:-*-medium-r-normal-*-16-*-cns11643*-5,
	chinese-cns11643-6:-*-medium-r-normal-*-16-*-cns11643*-6,
	chinese-cns11643-7:-*-medium-r-normal-*-16-*-cns11643*-7")
  "String of fontset spec of the standard fontset.
You have the biggest chance to display international characters
with correct glyphs by using the standard fontset.
See the documentation of `create-fontset-from-fontset-spec' for the format.")

;; Create fontsets from X resources of the name `fontset-N (class
;; Fontset-N)' where N is integer 0, 1, ...
;; The values of the resources the string of the same format as
;; `standard-fontset-spec'.

(defun create-fontset-from-x-resource ()
  (let ((idx 0)
	fontset-spec)
    (while (setq fontset-spec (x-get-resource (format "fontset-%d" idx)
					      (format "Fontset-%d" idx)))
      (create-fontset-from-fontset-spec fontset-spec t 'noerror)
      (setq idx (1+ idx)))))

;;
(provide 'fontset)

;;; arch-tag: bb53e629-0234-403c-950e-551e61554849
;;; fontset.el ends here
