;;; fontset.el --- commands for handling fontset

;; Copyright (C) 1995, 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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

(let ((l
       ;; Eval this at compile-time, since fontset.el is always loaded
       ;; when run under X and this would always load ind-util.el as well.
       (eval-when-compile
	 `((ascii . (nil . "ISO8859-1"))
	   (iso-8859-1 . (nil . "ISO8859-1"))
	   (iso-8859-2 . (nil . "ISO8859-2"))
	   (iso-8859-3 . (nil . "ISO8859-3"))
	   (iso-8859-4 . (nil . "ISO8859-4"))
	   (tis620-2533 . (nil . "TIS620*"))
	   (iso-8859-7 . (nil . "ISO8859-7"))
	   (iso-8859-6 . (nil . "ISO8859-6"))
	   (iso-8859-8 . (nil . "ISO8859-8"))
	   (iso-8859-5 . (nil . "ISO8859-5"))
	   (iso-8859-9 . (nil . "ISO8859-9"))
	   (iso-8859-14 . (nil . "ISO8859-14"))
	   (iso-8859-15 . (nil . "ISO8859-15"))
	   (chinese-gb2312 . (nil . "GB2312.1980-0"))
	   (japanese-jisx0208 . (nil . "JISX0208.1990*"))
	   (korean-ksc5601 . (nil . "KSC5601.1987*"))
	   (japanese-jisx0212 . (nil . "JISX0212*"))
	   (big5 . (nil . "Big5"))
	   (chinese-cns11643-1 . (nil . "CNS11643.1992-1"))
	   (chinese-cns11643-2 . (nil . "CNS11643.1992-2"))
	   (chinese-cns11643-3 . (nil . "CNS11643.1992-3"))
	   (chinese-cns11643-4 . (nil . "CNS11643.1992-4"))
	   (chinese-cns11643-5 . (nil . "CNS11643.1992-5"))
	   (chinese-cns11643-6 . (nil . "CNS11643.1992-6"))
	   (chinese-cns11643-7 . (nil . "CNS11643.1992-7"))
	   (chinese-gbk . (nil . "gbk-0"))
	   (chinese-sisheng . (nil . "sisheng_cwnn"))
	   (viscii . (nil . "VISCII1.1*"))
	   (arabic-digit . (nil . "MuleArabic-0"))
	   (arabic-1-column . (nil . "MuleArabic-1"))
	   (arabic-2-column . (nil . "MuleArabic-2"))
	   (ipa . (nil . "MuleIPA"))
	   (ethiopic . (nil . "Ethiopic-Unicode"))
	   (indian-is13194 . (nil . "IS13194-Devanagari"))
	   (indian-2-column . (nil . "MuleIndian-2"))
	   (mule-lao . (nil . "MuleLao-1"))
	   (tibetan . ("proportional" . "MuleTibetan-2"))
	   (tibetan-1-column . (nil . "MuleTibetan-1"))
	   (jisx0201 . (nil . "JISX0201*"))
	   (japanese-jisx0208-1978 . (nil . "JISX0208.1978*"))
	   (japanese-jisx0213-1 . (nil . "JISX0213.2000-1"))
	   (japanese-jisx0213-2 . (nil . "JISX0213.2000-2"))
           ;; unicode
           ((,(decode-char 'ucs #x0900)
	     . ,(decode-char 'ucs #x097F)) . (nil . "ISO10646.indian-1"))
           ;; indian
	   (indian-glyph . (nil . "Devanagari-CDAC"))
	   ((,(indian-glyph-char 0 'devanagari)
	     . ,(indian-glyph-char 255 'devanagari)) . (nil . "Devanagari-CDAC"))
	   ((,(indian-glyph-char 0 'sanskrit)
	     . ,(indian-glyph-char 255 'sanskrit)) . (nil . "Sanskrit-CDAC"))
	   ((,(indian-glyph-char 0 'bengali)
	     . ,(indian-glyph-char 255 'bengali)) . (nil . "Bengali-CDAC"))
	   ((,(indian-glyph-char 0 'assamese)
	     . ,(indian-glyph-char 255 'assamese)) . (nil . "Assamese-CDAC"))
	   ((,(indian-glyph-char 0 'punjabi)
	     . ,(indian-glyph-char 255 'punjabi)) . (nil . "Punjabi-CDAC"))
	   ((,(indian-glyph-char 0 'gujarati)
	     . ,(indian-glyph-char 255 'gujarati)) . (nil . "Gujarati-CDAC"))
	   ((,(indian-glyph-char 0 'oriya)
	     . ,(indian-glyph-char 255 'oriya)) . (nil . "Oriya-CDAC"))
	   ((,(indian-glyph-char 0 'tamil)
	     . ,(indian-glyph-char 255 'tamil)) . (nil . "Tamil-CDAC"))
	   ((,(indian-glyph-char 0 'telugu)
	     . ,(indian-glyph-char 255 'telugu)) . (nil . "Telugu-CDAC"))
	   ((,(indian-glyph-char 0 'kannada)
	     . ,(indian-glyph-char 255 'kannada)) . (nil . "Kannada-CDAC"))
	   ((,(indian-glyph-char 0 'malayalam)
	     . ,(indian-glyph-char 255 'malayalam)) . (nil . "Malayalam-CDAC"))
	   )))
      charset font-spec)
  (while l
    (setq charset (car (car l)) font-spec (cdr (car l)) l (cdr l))
    (set-fontset-font "fontset-default" charset font-spec)))

(setq font-encoding-alist
      '(("ISO8859-1" . iso-8859-1)
	("ISO8859-2" . iso-8859-2)
	("ISO8859-3" . iso-8859-3)
	("ISO8859-4" . iso-8859-4)
	("TIS620" . tis620-2533)
	("ISO8859-7" . iso-8859-7)
	("ISO8859-6" . iso-8859-6)
	("ISO8859-8" . iso-8859-8)
	("JISX0201" . jisx0201)
	("ISO8859-5" . iso-8859-5)
	("ISO8859-9" . iso-8859-9)
	("JISX0208.1978" . japanese-jisx0208-1978)
	("GB2312.1980" . chinese-gb2312)
	("JISX0208.1990" . japanese-jisx0208)
	("KSC5601.1987" . korean-ksc5601)
	("JISX0212" . japanese-jisx0212)
	("CNS11643.1992-1" . chinese-cns11643-1)
	("CNS11643.1992-2" . chinese-cns11643-2)
	("CNS11643.1992-3" . chinese-cns11643-3)
	("CNS11643.1992-4" . chinese-cns11643-4)
	("CNS11643.1992-5" . chinese-cns11643-5)
	("CNS11643.1992-6" . chinese-cns11643-6)
	("CNS11643.1992-7" . chinese-cns11643-7)
	("Big5" . big5)
	("sisheng_cwnn" . chinese-sisheng)
	("VISCII" . viscii)
	("MuleArabic-0" . arabic-digit)
	("MuleArabic-1" . arabic-1-column)
	("MuleArabic-2" . arabic-2-column)
	("MuleIPA" . ipa)
	("Ethiopic-Unicode" . ethiopic)
	("IS13194-Devanagari" . indian-is13194)
	("MuleIndian-2" . indian-2-column)
	("MuleIndian-1" . indian-1-column)
	("MuleLao-1" . mule-lao)
	("MuleTibetan-2" . tibetan)
	("MuleTibetan-1" . tibetan-1-column)
	("ISO8859-14" . iso-8859-14)
	("ISO8859-15" . iso-8859-15)
	("JISX0213.2000-1" . japanese-jisx0213-1)
	("JISX0213.2000-2" . japanese-jisx0213-2)
	("ISO10646-1" . unicode)))

;; Set arguments in `font-encoding-alist' (which see).
(defun set-font-encoding (pattern charset)
  (let ((slot (assoc pattern font-encoding-alist)))
    (if slot
	(setcdr slot charset)
      (setq font-encoding-alist
	    (cons (cons pattern charset) font-encoding-alist)))))

;; Setting for suppressing XLoadQueryFont on big fonts.
(setq x-pixel-size-width-font-regexp
      "gb2312\\|jisx0208\\|ksc5601\\|cns11643\\|big5")

;; These fonts require vertical centering.
(setq vertical-centering-font-regexp
      "gb2312\\|jisx0208\\|jisx0212\\|ksc5601\\|cns11643\\|big5")

(defvar x-font-name-charset-alist nil
  "This variable has no meaning now.  Just kept for backward compatibility.")

;;; XLFD (X Logical Font Description) format handler.

;; Define XLFD's field index numbers.		; field name
(defconst xlfd-regexp-family-subnum 0)		; FOUNDRY and FAMILY
(defconst xlfd-regexp-weight-subnum 1)		; WEIGHT_NAME
(defconst xlfd-regexp-slant-subnum 2)		; SLANT
(defconst xlfd-regexp-swidth-subnum 3)		; SETWIDTH_NAME
(defconst xlfd-regexp-adstyle-subnum 4)		; ADD_STYLE_NAME
(defconst xlfd-regexp-pixelsize-subnum 5)	; PIXEL_SIZE
(defconst xlfd-regexp-pointsize-subnum 6)	; POINT_SIZE
(defconst xlfd-regexp-resx-subnum 7)		; RESOLUTION_X
(defconst xlfd-regexp-resy-subnum 8)		; RESOLUTION_Y
(defconst xlfd-regexp-spacing-subnum 8)		; SPACING
(defconst xlfd-regexp-avgwidth-subnum 10)	; AVERAGE_WIDTH
(defconst xlfd-regexp-registry-subnum 11)	; REGISTRY and ENCODING

;; Regular expression matching against a fontname which conforms to
;; XLFD (X Logical Font Description).  All fields in XLFD should be
;; not be omitted (but can be a wild card) to be matched.
(defconst xlfd-tight-regexp
  "^\
-\\([^-]*-[^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)\
-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)\
-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*-[^-]*\\)$")

;; Regular expression matching against a fontname which conforms to
;; XLFD (X Logical Font Description).  All fields in XLFD from FOUNDRY
;; to ADSTYLE, REGSITRY, and ENCODING should be not be omitted (but
;; can be a wild card) to be matched.
(defconst xlfd-style-regexp
  "^\
-\\([^-]*-[^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-.*\
-\\([^-]*-[^-]*\\)$")

;; List of field numbers of XLFD whose values are numeric.
(defconst xlfd-regexp-numeric-subnums
  (list xlfd-regexp-pixelsize-subnum	;5
	xlfd-regexp-pointsize-subnum	;6
	xlfd-regexp-resx-subnum		;7
	xlfd-regexp-resy-subnum		;8
	xlfd-regexp-avgwidth-subnum	;10
	))

(defun x-decompose-font-name (pattern)
  "Decompose PATTERN into XLFD's fields and return vector of the fields.
The length of the vector is 12.

If PATTERN doesn't conform to XLFD, try to get a full XLFD name from
X server and use the information of the full name to decompose
PATTERN.  If no full XLFD name is gotten, return nil."
  (let (xlfd-fields fontname)
    (if (string-match xlfd-tight-regexp pattern)
	(progn
	  (setq xlfd-fields (make-vector 12 nil))
	  (dotimes (i 12)
	    (aset xlfd-fields i (match-string (1+ i) pattern)))
	  (dotimes (i 12)
	    (if (string-match "^[*-]+$" (aref xlfd-fields i))
		(aset xlfd-fields i nil)))
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
	    (setq xlfd-fields (make-vector 12 nil))
	    (dotimes (i 12)
	      (aset xlfd-fields i
		    (cons (match-beginning (1+ i))
			  (match-string (1+ i) fontname))))

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
		;; in PATTERN to nil, set the other elements to the
		;; exact strings in PATTERN.
		(let ((l (cdr (cdr (match-data)))))
		  (setq i 0)
		  (while (< i 12)
		    (if (or (null l) (< (car (aref xlfd-fields i)) (car l)))
			(progn
			  (aset xlfd-fields i (cdr (aref xlfd-fields i)))
			  (setq i (1+ i)))
		      (if (< (car (aref xlfd-fields i)) (car (cdr l)))
			  (progn
			    (aset xlfd-fields i nil)
			    (setq i (1+ i)))
			(setq l (cdr (cdr l)))))))
	      ;; Set each element of xlfd-fields to the exact string
	      ;; in the corresonding fields in full XLFD name.
	      (dotimes (i 12)
		(aset xlfd-fields i (cdr (aref xlfd-fields i)))))
	    xlfd-fields)))))

(defun x-compose-font-name (fields &optional reduce)
  "Compose X's fontname from FIELDS.
FIELDS is a vector of XLFD fields, the length 12.
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
    (while (and (null ascii-font) (<= index xlfd-regexp-registry-subnum))
      (let ((pattern (x-compose-font-name xlfd-fields)))
	(condition-case nil
	    (setq ascii-font (x-resolve-font-name pattern))
	  (error
	   (message "Warning: no fonts matching `%s' available" pattern)
	   (aset xlfd-fields index "*")
	   (setq index (1+ index))))))
    (unless ascii-font
      (error "No fonts founds"))
    ascii-font))


(defun x-complement-fontset-spec (xlfd-fields fontlist)
  "Complement FONTLIST for charsets based on XLFD-FIELDS and return it.
XLFD-FIELDS is a vector of XLFD (X Logical Font Description) fields.
FONTLIST is an alist of charsets vs the corresponding font names.

The fonts are complemented as below.

At first, if FONTLIST doesn't specify a font for ASCII charset,
generate a font name for the charset from XLFD-FIELDS, and add that
information to FONTLIST.

Then, replace font names with the corresponding XLFD field vectors
while substituting default field names for wild cards if they match
`xlfd-style-regexp'.  The default field names are decided by
XLFD-FIELDS."
  (let* ((default-spec (vector (aref xlfd-fields xlfd-regexp-family-subnum)
			       (aref xlfd-fields xlfd-regexp-weight-subnum)
			       (aref xlfd-fields xlfd-regexp-slant-subnum)
			       (aref xlfd-fields xlfd-regexp-swidth-subnum)
			       (aref xlfd-fields xlfd-regexp-adstyle-subnum)
			       (aref xlfd-fields xlfd-regexp-registry-subnum)))
	 (slot (assq 'ascii fontlist))
	 (ascii-font (cdr slot))
	 xlfd-ascii)
    (if ascii-font
	(progn
	  (setcdr slot (setq ascii-font (x-resolve-font-name ascii-font)))
	  (setq xlfd-ascii (x-decompose-font-name ascii-font))
	  (dotimes (i 11)
	    (or (aref xlfd-fields i)
		(aset xlfd-fields i (aref xlfd-ascii i)))))
      ;; If font for ASCII is not specified, add it.
      (setq xlfd-ascii (copy-sequence xlfd-fields))
      (aset xlfd-ascii xlfd-regexp-registry-subnum "iso8859-1")
      (setq ascii-font (x-must-resolve-font-name xlfd-ascii))
      (setq fontlist (cons (cons 'ascii ascii-font) fontlist)))

    (dolist (elt fontlist)
      (let ((name (cdr elt))
	    font-spec)
	(when (string-match xlfd-style-regexp name)
	  (setq font-spec (make-vector 6 nil))
	  (dotimes (i 6)
	    (aset font-spec i (match-string (1+ i) name)))
	  (dotimes (i 6)
	    (if (string-match "^[*-]+$" (aref font-spec i))
		(aset font-spec i (aref default-spec i))))
	  (setcdr elt font-spec))))

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
	      (nickname (aref xlfd-fields xlfd-regexp-registry-subnum))
	      name)
	  (if (not (string-match "^fontset-\\(.*\\)$" nickname))
	      fontset
	    (setq nickname (match-string 1 nickname))
	    (if (> (string-to-int size) 0)
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

;;;###autoload
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
      (setq name (x-compose-font-name xlfd-fields))
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
omitted, x-resolve-font-name is called to get the resolved name.  At
this time, if FONT is not available, error is signaled.

Optional 2nd arg FONTSET-NAME is a string to be used in
`<CHARSET_ENCODING>' fields of a new fontset name.  If it is omitted,
an appropriate name is generated automatically.

It returns a name of the created fontset."
  (setq font (downcase font))
  (if resolved-font
      (setq resolved-font (downcase resolved-font))
    (setq resolved-font (downcase (x-resolve-font-name font))))
  (let ((xlfd (x-decompose-font-name resolved-font))
	fontset)
    (if fontset-name
	(setq fontset-name (downcase fontset-name))
      (setq fontset-name
	    (subst-char-in-string
	     "-" "_" (aref xlfd xlfd-regexp-registry-subnum) t)))
    (aset xlfd xlfd-regexp-registry-subnum
	  (format "fontset-%s" fontset-name))
    (setq fontset (x-compose-font-name xlfd))
    (or (query-fontset fontset)
	(create-fontset-from-fontset-spec (concat fontset ", ascii:" font)))))


;; Create standard fontset from 16 dots fonts which are the most widely
;; installed fonts.  Fonts for Chinese-GB, Korean, and Chinese-CNS are
;; specified here because FAMILY of those fonts are not "fixed" in
;; many cases.
(defvar standard-fontset-spec
  (purecopy "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard")
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

;;; fontset.el ends here
