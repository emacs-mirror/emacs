;;; fontset.el --- Commands for handling fontset.

;; Copyright (C) 1995, 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

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

;;; Code:

;; Set standard REGISTRY property of charset to find an appropriate
;; font for each charset.  This is used to generate a font name in a
;; fontset.  If the value contains a character `-', the string before
;; that is embedded in `CHARSET_REGISTRY' field, and the string after
;; that is embedded in `CHARSET_ENCODING' field.  If the value does not
;; contain `-', the whole string is embedded in `CHARSET_REGISTRY'
;; field, and a wild card character `*' is embedded in
;; `CHARSET_ENCODING' field.

(defvar x-charset-registries
  '((ascii . "ISO8859-1")
    (latin-iso8859-1 . "ISO8859-1")
    (latin-iso8859-2 . "ISO8859-2")
    (latin-iso8859-3 . "ISO8859-3")
    (latin-iso8859-4 . "ISO8859-4")
    (thai-tis620 . "TIS620")
    (greek-iso8859-7 . "ISO8859-7")
    (arabic-iso8859-6 . "ISO8859-6")
    (hebrew-iso8859-8 . "ISO8859-8")
    (katakana-jisx0201 . "JISX0201")
    (latin-jisx0201 . "JISX0201")
    (cyrillic-iso8859-5 . "ISO8859-5")
    (latin-iso8859-9 . "ISO8859-9")
    (japanese-jisx0208-1978 . "JISX0208.1978")
    (chinese-gb2312 . "GB2312")
    (japanese-jisx0208 . "JISX0208.1983")
    (korean-ksc5601 . "KSC5601")
    (japanese-jisx0212 . "JISX0212")
    (chinese-cns11643-1 . "CNS11643.1992-1")
    (chinese-cns11643-2 . "CNS11643.1992-2")
    (chinese-cns11643-3 . "CNS11643.1992-3")
    (chinese-cns11643-4 . "CNS11643.1992-4")
    (chinese-cns11643-5 . "CNS11643.1992-5")
    (chinese-cns11643-6 . "CNS11643.1992-6")
    (chinese-cns11643-7 . "CNS11643.1992-7")
    (chinese-big5-1 . "Big5")
    (chinese-big5-2 . "Big5")
    (chinese-sisheng . "sisheng_cwnn")
    (vietnamese-viscii-lower . "VISCII1.1")
    (vietnamese-viscii-upper . "VISCII1.1")
    (arabic-digit . "MuleArabic-0")
    (arabic-1-column . "MuleArabic-1")
    (arabic-2-column . "MuleArabic-2")
    (ipa . "MuleIPA")
    (ethiopic . "Ethiopic-Unicode")
    (ascii-right-to-left . "ISO8859-1")
    (indian-is13194 . "IS13194-Devanagari")
    (indian-2-column . "MuleIndian-2")
    (indian-1-column . "MuleIndian-1")
    (lao . "MuleLao-1")
    (tibetan . "MuleTibetan-0")
    (tibetan-1-column . "MuleTibetan-1")
    (latin-iso8859-14 . "ISO8859-14")
    (latin-iso8859-15 . "ISO8859-15")
    ))

(let ((l x-charset-registries))
  (while l
    (condition-case nil
	(put-charset-property (car (car l)) 'x-charset-registry (cdr (car l)))
      (error nil))
    (setq l (cdr l))))

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

(set-font-encoding "ISO8859-1" 'ascii 0)
(set-font-encoding "JISX0201" 'latin-jisx0201 0)

;; Setting for suppressing XLoadQueryFont on big fonts.
(setq x-pixel-size-width-font-regexp
      "gb2312\\|jisx0208\\|ksc5601\\|cns11643\\|big5")

;; There fonts require vertical centering.
(setq vertical-centering-font-regexp
      "gb2312\\|jisx0208\\|ksc5601\\|cns11643\\|big5")

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
    ("mulelao-1" ascii lao))
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
Optional argument REDUCE non-nil means consecutive wild-cards are
reduced to be one."
  (let ((name
	 (concat "-" (mapconcat (lambda (x) (or x "*")) fields "-"))))
    (if reduce
	(x-reduce-font-name name)
      name)))

(defun register-alternate-fontnames (fontname)
  "Register alternate fontnames for FONTNAME in `alternate-fontname-alist'.
When Emacs fails to open FONTNAME, it tries to open an alternate font
registered in the variable `alternate-fontname-alist' (which see).

For FONTNAME, the following three alternate fontnames are registered:
  fontname which ignores style specification of FONTNAME,
  fontname which ignores size specification of FONTNAME,
  fontname which ignores both style and size specification of FONTNAME.
Emacs tries to open fonts in this order."
  (unless (assoc fontname alternate-fontname-alist)
    (let ((xlfd-fields (x-decompose-font-name fontname))
	  style-ignored size-ignored both-ignored)
      (when xlfd-fields
	(aset xlfd-fields xlfd-regexp-foundry-subnum nil)
	(aset xlfd-fields xlfd-regexp-family-subnum nil)

	(let ((temp (copy-sequence xlfd-fields)))
	  (aset temp xlfd-regexp-weight-subnum nil)
	  (aset temp xlfd-regexp-slant-subnum nil)
	  (aset temp xlfd-regexp-swidth-subnum nil)
	  (aset temp xlfd-regexp-adstyle-subnum nil)
	  (setq style-ignored (x-compose-font-name temp t)))

	(aset xlfd-fields xlfd-regexp-pixelsize-subnum nil)
	(aset xlfd-fields xlfd-regexp-pointsize-subnum nil)
	(aset xlfd-fields xlfd-regexp-resx-subnum nil)
	(aset xlfd-fields xlfd-regexp-resy-subnum nil)
	(aset xlfd-fields xlfd-regexp-spacing-subnum nil)
	(aset xlfd-fields xlfd-regexp-avgwidth-subnum nil)
	(setq size-ignored (x-compose-font-name xlfd-fields t))

	(aset xlfd-fields xlfd-regexp-weight-subnum nil)
	(aset xlfd-fields xlfd-regexp-slant-subnum nil)
	(aset xlfd-fields xlfd-regexp-swidth-subnum nil)
	(aset xlfd-fields xlfd-regexp-adstyle-subnum nil)
	(setq both-ignored (x-compose-font-name xlfd-fields t))

	(setq alternate-fontname-alist
	      (cons (list fontname style-ignored size-ignored both-ignored)
		    alternate-fontname-alist))))))

;; Just to avoid compiler waring.  The gloval value is never used.
(defvar resolved-ascii-font nil)

(defun x-complement-fontset-spec (xlfd-fields fontlist)
  "Complement FONTLIST for all charsets based on XLFD-FIELDS and return it.
XLFD-FIELDS is a vector of XLFD (X Logical Font Description) fields.
FONTLIST is an alist of charsets vs the corresponding font names.

Font names for charsets not listed in FONTLIST are generated from
XLFD-FIELDS and a property of x-charset-registry of each charset
automatically.

By side effect, this sets `resolved-ascii-font' to the resolved name
of ASCII font."
  (let ((charsets charset-list)
	(xlfd-fields-non-ascii (copy-sequence xlfd-fields))
	(new-fontlist nil))
    (aset xlfd-fields-non-ascii xlfd-regexp-foundry-subnum nil)
    (aset xlfd-fields-non-ascii xlfd-regexp-family-subnum nil)
    (aset xlfd-fields-non-ascii xlfd-regexp-adstyle-subnum nil)
    (aset xlfd-fields-non-ascii xlfd-regexp-avgwidth-subnum nil)
    (while charsets
      (let ((charset (car charsets)))
	(unless (assq charset fontlist)
	  (let ((registry (get-charset-property charset 'x-charset-registry))
		registry-val encoding-val fontname)
	    (if (string-match "-" registry)
		;; REGISTRY contains `CHARSET_ENCODING' field.
		(setq registry-val (substring registry 0 (match-beginning 0))
		      encoding-val (substring registry (match-end 0)))
	      (setq registry-val (concat registry "*")
		    encoding-val "*"))
	    (let ((xlfd (if (eq charset 'ascii) xlfd-fields
			  xlfd-fields-non-ascii)))
	      (aset xlfd xlfd-regexp-registry-subnum registry-val)
	      (aset xlfd xlfd-regexp-encoding-subnum encoding-val)
	      (setq fontname (downcase (x-compose-font-name xlfd))))
	    (setq new-fontlist (cons (cons charset fontname) new-fontlist))
	    (register-alternate-fontnames fontname))))
      (setq charsets (cdr charsets)))

    ;; Be sure that ASCII font is available.
    (let ((slot (or (assq 'ascii fontlist) (assq 'ascii new-fontlist)))
	  ascii-font)
      (setq ascii-font (condition-case nil
			   (x-resolve-font-name (cdr slot))
			 (error nil)))
      (if ascii-font
	  (let ((l x-font-name-charset-alist))
	    ;; If the ASCII font can also be used for another
	    ;; charsets, use that font instead of what generated based
	    ;; on x-charset-registry in the previous code.
	    (while l
	      (if (string-match (car (car l)) ascii-font)
		  (let ((charsets (cdr (car l)))
			slot2)
		    (while charsets
		      (if (and (not (eq (car charsets) 'ascii))
			       (setq slot2 (assq (car charsets) new-fontlist)))
			  (setcdr slot2 (cdr slot)))
		      (setq charsets (cdr charsets)))
		    (setq l nil))
		(setq l (cdr l))))
	    (setq resolved-ascii-font ascii-font)
	    (append fontlist new-fontlist))))))

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
  (let ((fontsets global-fontset-alist)
	fontset-name
	l)
    (while fontsets
      (setq fontset-name (car (car fontsets)) fontsets (cdr fontsets))
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

(defvar uninstantiated-fontset-alist nil
  "Alist of fontset names vs. information for instantiating them.
Each element has the form (FONTSET STYLE FONTLIST), where
FONTSET is a name of fontset not yet instantiated.
STYLE is a style of FONTSET, one of the followings:
  bold, demobold, italic, oblique,
  bold-italic, demibold-italic, bold-oblique, demibold-oblique.
FONTLIST is an alist of charsets vs font names to be used in FONSET.")

(defconst x-style-funcs-alist
  `((bold . x-make-font-bold)
    (demibold . x-make-font-demibold)
    (italic . x-make-font-italic)
    (oblique . x-make-font-oblique)
    (bold-italic . x-make-font-bold-italic)
    (demibold-italic
     . ,(function (lambda (x)
		    (let ((y (x-make-font-demibold x)))
		      (and y (x-make-font-italic y))))))
    (demibold-oblique
     . ,(function (lambda (x)
		    (let ((y (x-make-font-demibold x)))
		      (and y (x-make-font-oblique y))))))
    (bold-oblique
     . ,(function (lambda (x)
		    (let ((y (x-make-font-bold x)))
		      (and y (x-make-font-oblique y)))))))
  "Alist of font style vs function to generate a X font name of the style.
The function is called with one argument, a font name.")

(defcustom fontset-default-styles '(bold italic bold-italic)
  "List of alternative styles to create for a fontset.
Valid elements include `bold', `demibold'; `italic', `oblique';
and combinations of one from each group,
such as `bold-italic' and `demibold-oblique'."
  :group 'faces
  :type '(set (const bold) (const demibold) (const italic) (const oblique)
	      (const bold-italic) (const bold-oblique) (const demibold-italic)
	      (const demibold-oblique)))

(defun x-modify-font-name (fontname style)
  "Substitute style specification part of FONTNAME for STYLE.
STYLE should be listed in the variable `x-style-funcs-alist'."
  (let ((func (cdr (assq style x-style-funcs-alist))))
    (if func
	(funcall func fontname))))

;;;###autoload
(defun create-fontset-from-fontset-spec (fontset-spec
					 &optional style-variant noerror)
  "Create a fontset from fontset specification string FONTSET-SPEC.
FONTSET-SPEC is a string of the format:
	FONTSET-NAME,CHARSET-NAME0:FONT-NAME0,CHARSET-NAME1:FONT-NAME1, ...
Any number of SPACE, TAB, and NEWLINE can be put before and after commas.

Optional 2nd argument STYLE-VARIANT is a list of font styles
\(e.g. bold, italic) or the symbol t to specify all available styles.
If this argument is specified, fontsets which differs from
FONTSET-NAME in styles are also created.  An element of STYLE-VARIANT
may be cons of style and a font name.  In this case, the style variant
fontset uses the font for ASCII character set.

If this function attempts to create already existing fontset, error is
signaled unless the optional 3rd argument NOERROR is non-nil.

It returns a name of the created fontset."
  (if (not (string-match "^[^,]+" fontset-spec))
      (error "Invalid fontset spec: %s" fontset-spec))
  (let ((idx (match-end 0))
	(name (match-string 0 fontset-spec))
	fontlist full-fontlist ascii-font resolved-ascii-font charset)
    (if (query-fontset name)
	(or noerror 
	    (error "Fontset \"%s\" already exists" name))
      ;; At first, extract pairs of charset and fontname from FONTSET-SPEC.
      (while (string-match "[, \t\n]*\\([^:]+\\):\\([^,]+\\)" fontset-spec idx)
	(setq idx (match-end 0))
	(setq charset (intern (match-string 1 fontset-spec)))
	(if (charsetp charset)
	    (setq fontlist (cons (cons charset (match-string 2 fontset-spec))
				 fontlist))))
      ;; Remember the specified ASCII font name now because it will be
      ;; replaced by resolved font name by x-complement-fontset-spec.
      (setq ascii-font (cdr (assq 'ascii fontlist)))

      ;; If NAME conforms to XLFD, complement FONTLIST for charsets
      ;; which are not specified in FONTSET-SPEC.
      (let ((fields (x-decompose-font-name name)))
	(if fields
	    (setq full-fontlist (x-complement-fontset-spec fields fontlist))))
      
      (when full-fontlist
	;; Create the fontset.
	(new-fontset name full-fontlist)

	;; Define aliases: short name (if appropriate) and ASCII font name.
	(if (and (string-match "fontset-.*$" name)
		 (not (assoc name fontset-alias-alist)))
	    (let ((alias (match-string 0 name)))
	      (or (rassoc alias fontset-alias-alist)
		  (setq fontset-alias-alist
			(cons (cons name alias) fontset-alias-alist)))))
	(or (rassoc resolved-ascii-font fontset-alias-alist)
	    (setq fontset-alias-alist
		  (cons (cons name resolved-ascii-font)
			fontset-alias-alist)))
	(or (equal ascii-font resolved-ascii-font)
	    (rassoc ascii-font fontset-alias-alist)
	    (setq fontset-alias-alist
		  (cons (cons name ascii-font)
			fontset-alias-alist)))

	;; At last, handle style variants.
	(if (eq style-variant t)
	    (setq style-variant fontset-default-styles))

	(if style-variant
	    ;; Generate fontset names of style variants and set them
	    ;; in uninstantiated-fontset-alist.
	    (let* (nonascii-fontlist
		   new-name new-ascii-font style font)
	      (if ascii-font
		  (setq nonascii-fontlist (delete (cons 'ascii ascii-font)
						  (copy-sequence fontlist)))
		(setq ascii-font (cdr (assq 'ascii full-fontlist))
		      nonascii-fontlist fontlist))
	      (while style-variant
		(setq style (car style-variant))
		(if (symbolp style)
		    (setq font nil)
		  (setq font (cdr style)
			style (car style)))
		(setq new-name (x-modify-font-name name style))
		(when new-name
		  ;; Modify ASCII font name for the style...
		  (setq new-ascii-font
			(or font
			    (x-modify-font-name resolved-ascii-font style)))
		  ;; but leave fonts for the other charsets unmodified
		  ;; for the moment.  They are modified for the style
		  ;; in instantiate-fontset.
		  (setq uninstantiated-fontset-alist
			(cons (list new-name
				    style
				    (cons (cons 'ascii new-ascii-font)
					  nonascii-fontlist))
			      uninstantiated-fontset-alist))
		  (or (rassoc new-ascii-font fontset-alias-alist)
		      (setq fontset-alias-alist
			    (cons (cons new-name new-ascii-font)
				  fontset-alias-alist))))
		(setq style-variant (cdr style-variant)))))))
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

Style variants of the fontset is created too.  Font names in the
variants are generated automatically from FONT unless X resources
XXX.attributeFont explicitly specify them.

It returns a name of the created fontset."
  (or resolved-font
      (setq resolved-font (x-resolve-font-name font)))
  (let* ((faces (copy-sequence fontset-default-styles))
	 (styles faces)
	 (xlfd (x-decompose-font-name font))
	 (resolved-xlfd  (x-decompose-font-name resolved-font))
	 face face-font fontset fontset-spec)
    (while faces
      (setq face (car faces))
      (setq face-font (x-get-resource (concat (symbol-name face)
					      ".attributeFont")
				      "Face.AttributeFont"))
      (if face-font
	  (setcar faces (cons face face-font)))
      (setq faces (cdr faces)))
    (aset xlfd xlfd-regexp-foundry-subnum nil)
    (aset xlfd xlfd-regexp-family-subnum nil)
    (aset xlfd xlfd-regexp-registry-subnum "fontset")
    (or fontset-name
	(setq fontset-name
	      (format "%s_%s_%s"
		      (aref resolved-xlfd xlfd-regexp-registry-subnum)
		      (aref resolved-xlfd xlfd-regexp-encoding-subnum)
		      (aref resolved-xlfd xlfd-regexp-pixelsize-subnum))))
    (aset xlfd xlfd-regexp-encoding-subnum fontset-name)
    ;; The fontset name should have concrete values in weight and
    ;; slant field.
    (let ((weight (aref xlfd xlfd-regexp-weight-subnum))
	  (slant (aref xlfd xlfd-regexp-slant-subnum)))
      (if (or (not weight) (string-match "[*?]*" weight))
	  (aset xlfd xlfd-regexp-weight-subnum
		(aref resolved-xlfd xlfd-regexp-weight-subnum)))
      (if (or (not slant) (string-match "[*?]*" slant))
	  (aset xlfd xlfd-regexp-slant-subnum
		(aref resolved-xlfd xlfd-regexp-slant-subnum))))
    (setq fontset (x-compose-font-name xlfd))
    (or (query-fontset fontset)
	(create-fontset-from-fontset-spec (concat fontset ", ascii:" font)
					  styles))))

(defun instantiate-fontset (fontset)
  "Make FONTSET be ready to use.
FONTSET should be in the variable `uninstantiated-fontset-alist' in advance.
Return FONTSET if it is created successfully, else return nil."
  (let ((fontset-data (assoc fontset uninstantiated-fontset-alist)))
    (when fontset-data
      (setq uninstantiated-fontset-alist
	    (delete fontset-data uninstantiated-fontset-alist))

      (let* ((fields (x-decompose-font-name fontset))
	     (style (nth 1 fontset-data))
	     (fontlist (x-complement-fontset-spec fields (nth 2 fontset-data)))
	     (font (cdr (assq 'ascii fontlist))))
	;; If ASCII font is available, instantiate this fontset.
	(when font
	  (let ((new-fontlist (list (cons 'ascii font))))
	    ;; Fonts for non-ascii charsets should be modified for
	    ;; this style now.
	    (while fontlist
	      (setq font (cdr (car fontlist)))
	      (or (eq (car (car fontlist)) 'ascii)
		  (setq new-fontlist
			(cons (cons (car (car fontlist))
				    (x-modify-font-name font style))
			      new-fontlist)))
	      (setq fontlist (cdr fontlist)))
	    (new-fontset fontset new-fontlist)
	    fontset))))))

(defun resolve-fontset-name (pattern)
  "Return a fontset name matching PATTERN."
  (let ((fontset (car (rassoc pattern fontset-alias-alist))))
    (or fontset (setq fontset pattern))
    (if (assoc fontset uninstantiated-fontset-alist)
	(instantiate-fontset fontset)
      (query-fontset fontset))))

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

(defsubst fontset-list ()
  "Returns a list of all defined fontset names."
  (mapcar 'car global-fontset-alist))

;;
(provide 'fontset)

;;; fontset.el ends here
