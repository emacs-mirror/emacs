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
;; that is embeded in `CHARSET_REGISTRY' field, and the string after
;; that is embeded in `CHARSET_ENCODING' field.  If the value does not
;; contain `-', the whole string is embeded in `CHARSET_REGISTRY'
;; field, and a wild card character `*' is embeded in
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
	  (let ((len (length pattern))
		(i 0)
		l)
	    (setq xlfd-fields (make-vector 14 nil))
	    (while (< i 14)
	      (aset xlfd-fields i
		    (cons (match-beginning (1+ i))
			  (match-string (1+ i) fontname)))
	      (setq i (1+ i)))
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
	    (string-match pattern fontname)
	    (setq l (cdr (cdr (match-data))))
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
		  (setq l (cdr (cdr l))))))
	    xlfd-fields)))))

;; Replace consecutive wild-cards (`*') in NAME to one.
;; Ex. (x-reduce-font-name "-*-*-*-iso8859-1") => "-*-iso8859-1"
(defsubst x-reduce-font-name (name)
  (while (string-match "-\\*-\\(\\*-\\)+" name)
    (setq name (replace-match "-*-" t t name)))
  name)

(defun x-compose-font-name (xlfd-fields &optional reduce)
  "Compose X's fontname from FIELDS.
FIELDS is a vector of XLFD fields.
If a field is nil, wild-card letter `*' is embedded.
Optional argument REDUCE non-nil means consecutive wild-cards are
reduced to be one."
  (let ((name
	 (concat "-" (mapconcat (lambda (x) (or x "*")) xlfd-fields "-"))))
    (if reduce
	(x-reduce-font-name name)
      name)))

(defun register-alternate-fontnames (fontname)
  "Register alternate fontnames for FONTNAME in `alternate-fontname-alist'.
When Emacs fails to open FONTNAME, it tries to open alternate font
registered in the variable `alternate-fontname-alist' (which see).

For FONTNAME, the following three alternate fontnames are registered:
  fontname which ignores style specification of FONTNAME,
  fontname which ignores size specification of FONTNAME,
  fontname which ignores both style and size specification of FONTNAME."
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

(defun x-complement-fontset-spec (xlfd-fields fontlist)
  "Complement FONTLIST for all charsets based on XLFD-FIELDS and return it.
XLFD-FIELDS is a vector of XLFD (X Logical Font Description) fields.
FONTLIST is an alist of cons of charset and fontname.

Fontnames for charsets not listed in FONTLIST are generated from
XLFD-FIELDS and a property of x-charset-registry of each charset
automatically."
  (let ((charsets charset-list))
    (while charsets
      (let ((charset (car charsets)))
	(unless (assq charset fontlist)
	  (let ((registry (get-charset-property charset
						'x-charset-registry))
		registry-val encoding-val fontname loose-fontname)
	    (if (string-match "-" registry)
		;; REGISTRY contains `CHARSET_ENCODING' field.
		(setq registry-val (substring registry 0 (match-beginning 0))
		      encoding-val (substring registry (match-end 0)))
	      (setq registry-val (concat registry "*")
		    encoding-val "*"))
	    (aset xlfd-fields xlfd-regexp-registry-subnum registry-val)
	    (aset xlfd-fields xlfd-regexp-encoding-subnum encoding-val)
	    (setq fontname (downcase (x-compose-font-name xlfd-fields)))
	    (setq fontlist (cons (cons charset fontname) fontlist))
	    (register-alternate-fontnames fontname))))
      (setq charsets (cdr charsets))))

  ;; Here's a trick for the charset latin-iso8859-1.  If font for
  ;; ascii also contains Latin-1 characters, use it also for
  ;; latin-iso8859-1.  This prevent loading a font for latin-iso8859-1
  ;; by a different name.
  (if (string-match (cdr (assq 'latin-iso8859-1 x-charset-registries))
		    (cdr (assq 'ascii fontlist)))
      (setcdr (assq 'latin-iso8859-1 fontlist) (cdr (assq 'ascii fontlist))))
  fontlist)

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
    (cons "Fontset" l)))

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
Each element has the form (FONTSET STYLE BASE-FONTSET), where
FONTSET is a name of fontset not yet instantiated.
STYLE is a style of FONTSET, one of the followings:
  bold, demobold, italic, oblique,
  bold-italic, demibold-italic, bold-oblique, demibold-oblique.
BASE-FONTSET is a name of fontset base from which FONSET is instantiated.")

(defun create-fontset-from-fontset-spec (fontset-spec &optional style noerror)
  "Create a fontset from fontset specification string FONTSET-SPEC.
FONTSET-SPEC is a string of the format:
	FONTSET-NAME,CHARSET-NAME0:FONT-NAME0,CHARSET-NAME1:FONT-NAME1, ...
Any number of SPACE, TAB, and NEWLINE can be put before and after commas.
If optional argument STYLE is specified, create a fontset of STYLE
by modifying FONTSET-SPEC appropriately.  STYLE can be one of `bold',
`italic', and `bold-italic'.
If this function attempts to create already existing fontset, error is
signaled unless the optional 3rd argument NOERROR is non-nil."
  (if (not (string-match "^[^,]+" fontset-spec))
      (error "Invalid fontset spec: %s" fontset-spec))
  (let ((idx (match-end 0))
	(name (match-string 0 fontset-spec))
	fontlist charset)
    ;; At first, extract pairs of charset and fontname from FONTSET-SPEC.
    (while (string-match "[, \t\n]*\\([^:]+\\):\\([^,]+\\)" fontset-spec idx)
      (setq idx (match-end 0))
      (setq charset (intern (match-string 1 fontset-spec)))
      (if (charsetp charset)
	  (setq fontlist (cons (cons charset (match-string 2 fontset-spec))
			       fontlist))))

    ;; If NAME conforms to XLFD, complement FONTLIST for charsets not
    ;; specified in FONTSET-SPEC.
    (let ((xlfd-fields (x-decompose-font-name name)))
      (if xlfd-fields
	  (setq fontlist
		(x-complement-fontset-spec xlfd-fields fontlist))))

    ;; If STYLE is specified, modify fontset name (NAME) and FONTLIST.
    (if nil
	(let ((func (cdr (assq style '((bold . x-make-font-bold)
				       (italic . x-make-font-italic)
				       (bold-italic . x-make-font-bold-italic)))))
	      (l fontlist)
	      new-name)
	  (if (and func
		   (setq new-name (funcall func name)))
	      (progn
		(setq name new-name)
		(while l
		  (if (setq new-name (funcall func (cdr (car l))))
		      (setcdr (car l) new-name))
		  (setq l (cdr l))))))
      (let ((funcs-alist
	     '((bold x-make-font-bold)
	       (demibold x-make-font-demibold)
	       (italic x-make-font-italic)
	       (oblique x-make-font-oblique)
	       (bold-italic x-make-font-bold x-make-font-italic)
	       (demibold-italic x-make-font-demibold x-make-font-italic)
	       (bold-oblique x-make-font-bold x-make-font-oblique)
	       (demibold-oblique x-make-font-demibold x-make-font-oblique)))
	    new-name style funcs)
	(while funcs-alist
	  (setq funcs (car funcs-alist))
	  (setq style (car funcs))
	  (setq funcs (cdr funcs))
	  (setq new-name name)
	  (while funcs
	    (setq new-name (funcall (car funcs) new-name))
	    (setq funcs (cdr funcs)))
	  (setq uninstantiated-fontset-alist
		(cons (list new-name style name) uninstantiated-fontset-alist))
	  (setq funcs-alist (cdr funcs-alist)))))

    (if (and noerror (query-fontset name))
	;; Don't try to create an already existing fontset.
	nil
      ;; Create the fontset, and define the alias if appropriate.
      (new-fontset name fontlist)
      (if (and (not style)
	       (not (assoc name fontset-alias-alist))
	       (string-match "fontset-.*$" name))
	  (let ((alias (match-string 0 name)))
	    (or (rassoc alias fontset-alias-alist)
		(setq fontset-alias-alist
		      (cons (cons name alias) fontset-alias-alist))))))))

(defun instantiate-fontset (fontset)
  "Create a new fontset FONTSET if it is not yet instantiated.
Return FONTSET if it is created successfully, else return nil."
  (let ((fontset-data (assoc fontset uninstantiated-fontset-alist)))
    (if (null fontset-data)
	nil
      (let ((style (nth 1 fontset-data))
	    (base-fontset (nth 2 fontset-data))
	    (funcs-alist
	     '((bold x-make-font-bold)
	       (demibold x-make-font-demibold)
	       (italic x-make-font-italic)
	       (oblique x-make-font-oblique)
	       (bold-italic x-make-font-bold x-make-font-italic)
	       (demibold-italic x-make-font-demibold x-make-font-italic)
	       (bold-oblique x-make-font-bold x-make-font-oblique)
	       (demibold-oblique x-make-font-demibold x-make-font-oblique)))
	    ascii-font font font2 funcs)
	(setq uninstantiated-fontset-alist
	      (delete fontset-data uninstantiated-fontset-alist))
	(setq fontset-data (assoc base-fontset global-fontset-alist))
	(setq ascii-font (cdr (assq 'ascii (cdr fontset-data))))
	(setq funcs (cdr (assq style funcs-alist)))
	(if (= (length funcs) 1)
	    (and (setq font (funcall (car funcs) ascii-font))
		 (setq font (x-resolve-font-name font 'default)))
	  (and (setq font (funcall (car funcs) ascii-font))
	       (not (equal font ascii-font))
	       (setq font2 (funcall (nth 1 funcs) font))
	       (not (equal font2 font))
	       (setq font (x-resolve-font-name font2 'default))))
	(when font
	  (let ((new-fontset-data (copy-alist fontset-data)))
	    (setq funcs (cdr (assq style funcs-alist)))
	    (while funcs
	      (setcar new-fontset-data
		      (funcall (car funcs) (car new-fontset-data)))
	      (let ((l (cdr new-fontset-data)))
		(while l
		  (if (= (length funcs) 1)
		      (setq font (funcall (car funcs) (cdr (car l))))
		    (and (setq font (funcall (car funcs) (cdr (car l))))
			 (not (equal font (cdr (car l))))
			 (setq font2 (funcall (nth 1 funcs) font))
			 (not (equal font2 font))
			 (setq font font2)))
		  (when font
		    (setcdr (car l) font)
		    (register-alternate-fontnames font))
		  (setq l (cdr l))))
	      (setq funcs (cdr funcs)))
	    (new-fontset (car new-fontset-data) (cdr new-fontset-data))
	    (car new-fontset-data)))))))

;; Create standard fontset from 16 dots fonts which are the most widely
;; installed fonts.  Fonts for Chinese-GB, Korean, and Chinese-CNS are
;; specified here because FAMILY of those fonts are not "fixed" in
;; many cases.
(defvar standard-fontset-spec
  "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard,
	chinese-gb2312:-*-medium-r-normal-*-16-*-gb2312*-*,
	korean-ksc5601:-*-medium-r-normal-*-16-*-ksc5601*-*,
	chinese-cns11643-1:-*-medium-r-normal-*-16-*-cns11643*-1,
	chinese-cns11643-2:-*-medium-r-normal-*-16-*-cns11643*-2,
	chinese-cns11643-3:-*-medium-r-normal-*-16-*-cns11643*-3,
	chinese-cns11643-4:-*-medium-r-normal-*-16-*-cns11643*-4,
	chinese-cns11643-5:-*-medium-r-normal-*-16-*-cns11643*-5,
	chinese-cns11643-6:-*-medium-r-normal-*-16-*-cns11643*-6,
	chinese-cns11643-7:-*-medium-r-normal-*-16-*-cns11643*-7"
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
    (while (setq fontset-spec (x-get-resource (concat "fontset-" idx)
					      (concat "Fontset-" idx)))
      (create-fontset-from-fontset-spec fontset-spec nil 'noerror)
      (setq idx (1+ idx)))))

(defsubst fontset-list ()
  "Returns a list of all defined fontset names."
  (mapcar 'car global-fontset-alist))

;;
(provide 'fontset)

;;; fontset.el ends here
