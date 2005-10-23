;;; mule.el --- basic commands for mulitilingual environment

;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
;;   Free Software Foundation, Inc.
;; Copyright (C) 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: mule, multilingual, character set, coding system

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

(defconst mule-version "5.0 (SAKAKI)" "\
Version number and name of this version of MULE (multilingual environment).")

(defconst mule-version-date "1999.12.7" "\
Distribution date of this version of MULE (multilingual environment).")

(defun load-with-code-conversion (fullname file &optional noerror nomessage)
  "Execute a file of Lisp code named FILE whose absolute name is FULLNAME.
The file contents are decoded before evaluation if necessary.
If optional second arg NOERROR is non-nil,
 report no error if FILE doesn't exist.
Print messages at start and end of loading unless
 optional third arg NOMESSAGE is non-nil.
Return t if file exists."
  (if (null (file-readable-p fullname))
      (and (null noerror)
	   (signal 'file-error (list "Cannot open load file" file)))
    ;; Read file with code conversion, and then eval.
    (let* ((buffer
	    ;; To avoid any autoloading, set default-major-mode to
	    ;; fundamental-mode.
	    ;; So that we don't get completely screwed if the
	    ;; file is encoded in some complicated character set,
	    ;; read it with real decoding, as a multibyte buffer,
	    ;; even if this is a --unibyte Emacs session.
	    (let ((default-major-mode 'fundamental-mode)
		  (default-enable-multibyte-characters t))
	      ;; We can't use `generate-new-buffer' because files.el
	      ;; is not yet loaded.
	      (get-buffer-create (generate-new-buffer-name " *load*"))))
	   (load-in-progress t)
	   (source (save-match-data (string-match "\\.el\\'" fullname))))
      (unless nomessage
	(if source
	    (message "Loading %s (source)..." file)
	  (message "Loading %s..." file)))
      (when purify-flag
	(push file preloaded-file-list))
      (unwind-protect
	  (let ((load-file-name fullname)
		(set-auto-coding-for-load t)
		(inhibit-file-name-operation nil))
	    (save-excursion
	      (set-buffer buffer)
	      (insert-file-contents fullname)
	      ;; If the loaded file was inserted with no-conversion or
	      ;; raw-text coding system, make the buffer unibyte.
	      ;; Otherwise, eval-buffer might try to interpret random
	      ;; binary junk as multibyte characters.
	      (if (and enable-multibyte-characters
		       (or (eq (coding-system-type last-coding-system-used) 5)
			   (eq last-coding-system-used 'no-conversion)))
		  (set-buffer-multibyte nil))
	      ;; Make `kill-buffer' quiet.
	      (set-buffer-modified-p nil))
	    ;; Have the original buffer current while we eval.
	    (eval-buffer buffer nil
			 ;; This is compatible with what `load' does.
			 (if purify-flag file fullname)
			 ;; If this Emacs is running with --unibyte,
			 ;; convert multibyte strings to unibyte
			 ;; after reading them.
;;			 (not default-enable-multibyte-characters)
			 nil t
			 ))
	(let (kill-buffer-hook kill-buffer-query-functions)
	  (kill-buffer buffer)))
      (let ((hook (assoc file after-load-alist)))
	(when hook
	  (mapcar (function eval) (cdr hook))))
      (unless (or nomessage noninteractive)
	(if source
	    (message "Loading %s (source)...done" file)
	  (message "Loading %s...done" file)))
      t)))

;; API (Application Program Interface) for charsets.

(defsubst charset-quoted-standard-p (obj)
  "Return t if OBJ is a quoted symbol, and is the name of a standard charset."
  (and (listp obj) (eq (car obj) 'quote)
       (symbolp (car-safe (cdr obj)))
       (let ((vector (get (car-safe (cdr obj)) 'charset)))
	 (and (vectorp vector)
	      (< (aref vector 0) 160)))))

(defsubst charsetp (object)
  "T if OBJECT is a charset."
  (and (symbolp object) (vectorp (get object 'charset))))

(defsubst charset-info (charset)
  "Return a vector of information of CHARSET.
The elements of the vector are:
	CHARSET-ID, BYTES, DIMENSION, CHARS, WIDTH, DIRECTION,
	LEADING-CODE-BASE, LEADING-CODE-EXT,
	ISO-FINAL-CHAR, ISO-GRAPHIC-PLANE,
	REVERSE-CHARSET, SHORT-NAME, LONG-NAME,	DESCRIPTION,
	PLIST,
where
CHARSET-ID (integer) is the identification number of the charset.
BYTES (integer) is the length of multi-byte form of a character in
  the charset: one of 1, 2, 3, and 4.
DIMENSION (integer) is the number of bytes to represent a character of
the charset: 1 or 2.
CHARS (integer) is the number of characters in a dimension: 94 or 96.
WIDTH (integer) is the number of columns a character in the charset
  occupies on the screen: one of 0, 1, and 2.
DIRECTION (integer) is the rendering direction of characters in the
  charset when rendering.  If 0, render from left to right, else
  render from right to left.
LEADING-CODE-BASE (integer) is the base leading-code for the
  charset.
LEADING-CODE-EXT (integer) is the extended leading-code for the
  charset.  All charsets of less than 0xA0 has the value 0.
ISO-FINAL-CHAR (character) is the final character of the
  corresponding ISO 2022 charset.  If the charset is not assigned
  any final character, the value is -1.
ISO-GRAPHIC-PLANE (integer) is the graphic plane to be invoked
  while encoding to variants of ISO 2022 coding system, one of the
  following: 0/graphic-plane-left(GL), 1/graphic-plane-right(GR).
  If the charset is not assigned any final character, the value is -1.
REVERSE-CHARSET (integer) is the charset which differs only in
  LEFT-TO-RIGHT value from the charset.  If there's no such a
  charset, the value is -1.
SHORT-NAME (string) is the short name to refer to the charset.
LONG-NAME (string) is the long name to refer to the charset
DESCRIPTION (string) is the description string of the charset.
PLIST (property list) may contain any type of information a user
  want to put and get by functions `put-charset-property' and
  `get-charset-property' respectively."
  (get charset 'charset))

;; It is better not to use backquote in this file,
;; because that makes a bootstrapping problem
;; if you need to recompile all the Lisp files using interpreted code.

(defmacro charset-id (charset)
  "Return charset identification number of CHARSET."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 0)
    (list 'aref (list 'charset-info charset) 0)))

(defmacro charset-bytes (charset)
  "Return bytes of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 1)
    (list 'aref (list 'charset-info charset) 1)))

(defmacro charset-dimension (charset)
  "Return dimension of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 2)
    (list 'aref (list 'charset-info charset) 2)))

(defmacro charset-chars (charset)
  "Return character numbers contained in a dimension of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 3)
    (list 'aref (list 'charset-info charset) 3)))

(defmacro charset-width (charset)
  "Return width (how many column occupied on a screen) of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 4)
    (list 'aref (list 'charset-info charset) 4)))

(defmacro charset-direction (charset)
  "Return direction of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 5)
    (list 'aref (list 'charset-info charset) 5)))

(defmacro charset-iso-final-char (charset)
  "Return final char of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 8)
    (list 'aref (list 'charset-info charset) 8)))

(defmacro charset-iso-graphic-plane (charset)
  "Return graphic plane of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 9)
    (list 'aref (list 'charset-info charset) 9)))

(defmacro charset-reverse-charset (charset)
  "Return reverse charset of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 10)
    (list 'aref (list 'charset-info charset) 10)))

(defmacro charset-short-name (charset)
  "Return short name of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 11)
    (list 'aref (list 'charset-info charset) 11)))

(defmacro charset-long-name (charset)
  "Return long name of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 12)
    (list 'aref (list 'charset-info charset) 12)))

(defmacro charset-description (charset)
  "Return description of CHARSET.
See the function `charset-info' for more detail."
  (if (charset-quoted-standard-p charset)
      (aref (charset-info (nth 1 charset)) 13)
    (list 'aref (list 'charset-info charset) 13)))

(defmacro charset-plist (charset)
  "Return list charset property of CHARSET.
See the function `charset-info' for more detail."
  (list 'aref
	(if (charset-quoted-standard-p charset)
	    (charset-info (nth 1 charset))
	  (list 'charset-info charset))
	14))

(defun set-charset-plist (charset plist)
  "Set CHARSET's property list to PLIST, and return PLIST."
  (aset (charset-info  charset) 14 plist))

(defun make-char (charset &optional code1 code2)
  "Return a character of CHARSET whose position codes are CODE1 and CODE2.
CODE1 and CODE2 are optional, but if you don't supply
sufficient position codes, return a generic character which stands for
all characters or group of characters in the character set.
A generic character can be used to index a char table (e.g. syntax-table).

Such character sets as ascii, eight-bit-control, and eight-bit-graphic
don't have corresponding generic characters.  If CHARSET is one of
them and you don't supply CODE1, return the character of the smallest
code in CHARSET.

If CODE1 or CODE2 are invalid (out of range), this function signals an
error.  However, the eighth bit of both CODE1 and CODE2 is zeroed
before they are used to index CHARSET.  Thus you may use, say, the
actual ISO 8859 character code rather than subtracting 128, as you
would need to index the corresponding Emacs charset."
  (make-char-internal (charset-id charset) code1 code2))

(put 'make-char 'byte-compile
     (function
      (lambda (form)
	(let ((charset (nth 1 form)))
	  (if (charset-quoted-standard-p charset)
	      (byte-compile-normal-call
	       (cons 'make-char-internal
		     (cons (charset-id (nth 1 charset)) (nthcdr 2 form))))
	    (byte-compile-normal-call
	     (cons 'make-char-internal
		   (cons (list 'charset-id charset) (nthcdr 2 form)))))))))

(defun charset-list ()
  "Return list of charsets ever defined.

This function is provided for backward compatibility.
Now we have the variable `charset-list'."
  charset-list)

(defsubst generic-char-p (char)
  "Return t if and only if CHAR is a generic character.
See also the documentation of `make-char'."
  (and (>= char 0400)
       (let ((l (split-char char)))
	 (and (or (= (nth 1 l) 0) (eq (nth 2 l) 0))
	      (not (eq (car l) 'composition))))))

(defun decode-char (ccs code-point &optional restriction)
  "Return character specified by coded character set CCS and CODE-POINT in it.
Return nil if such a character is not supported.
Currently the only supported coded character set is `ucs' (ISO/IEC
10646: Universal Multi-Octet Coded Character Set), and the result is
translated through the translation-table named
`utf-translation-table-for-decode' or the translation-hash-table named
`utf-subst-table-for-decode'.

Optional argument RESTRICTION specifies a way to map the pair of CCS
and CODE-POINT to a character.  Currently not supported and just ignored."
  (cond
   ((eq ccs 'ucs)
    (or (utf-lookup-subst-table-for-decode code-point)
	(let ((c (cond
		  ((< code-point 160)
		   code-point)
		  ((< code-point 256)
		   (make-char 'latin-iso8859-1 code-point))
		  ((< code-point #x2500)
		   (setq code-point (- code-point #x0100))
		   (make-char 'mule-unicode-0100-24ff
			      (+ (/ code-point 96) 32) (+ (% code-point 96) 32)))
		  ((< code-point #x3400)
		   (setq code-point (- code-point #x2500))
		   (make-char 'mule-unicode-2500-33ff
			      (+ (/ code-point 96) 32) (+ (% code-point 96) 32)))
		  ((and (>= code-point #xe000) (< code-point #x10000))
		   (setq code-point (- code-point #xe000))
		   (make-char 'mule-unicode-e000-ffff
			      (+ (/ code-point 96) 32)
			      (+ (% code-point 96) 32))))))
	  (when c
	    (or (aref (get 'utf-translation-table-for-decode
			   'translation-table) c)
		c)))))))

(defun encode-char (char ccs &optional restriction)
  "Return code-point in coded character set CCS that corresponds to CHAR.
Return nil if CHAR is not included in CCS.
Currently the only supported coded character set is `ucs' (ISO/IEC
10646: Universal Multi-Octet Coded Character Set), and CHAR is first
translated through the translation-table named
`utf-translation-table-for-encode' or the translation-hash-table named
`utf-subst-table-for-encode'.

CHAR should be in one of these charsets:
  ascii, latin-iso8859-1, mule-unicode-0100-24ff, mule-unicode-2500-33ff,
  mule-unicode-e000-ffff, eight-bit-control
Otherwise, return nil.

Optional argument RESTRICTION specifies a way to map CHAR to a
code-point in CCS.  Currently not supported and just ignored."
  (let* ((split (split-char char))
	 (charset (car split))
	 trans)
    (cond ((eq ccs 'ucs)
	   (or (utf-lookup-subst-table-for-encode char)
	       (let ((table (get 'utf-translation-table-for-encode
				 'translation-table)))
		 (setq trans (aref table char))
		 (if trans
		     (setq split (split-char trans)
			   charset (car split)))
		 (cond ((eq charset 'ascii)
			(or trans char))
		       ((eq charset 'latin-iso8859-1)
			(+ (nth 1 split) 128))
		       ((eq charset 'mule-unicode-0100-24ff)
			(+ #x0100 (+ (* (- (nth 1 split) 32) 96)
				     (- (nth 2 split) 32))))
		       ((eq charset 'mule-unicode-2500-33ff)
			(+ #x2500 (+ (* (- (nth 1 split) 32) 96)
				     (- (nth 2 split) 32))))
		       ((eq charset 'mule-unicode-e000-ffff)
			(+ #xe000 (+ (* (- (nth 1 split) 32) 96)
				     (- (nth 2 split) 32))))
		       ((eq charset 'eight-bit-control)
			char))))))))


;; Coding system stuff

;; Coding system is a symbol that has the property `coding-system'.
;;
;; The value of the property `coding-system' is a vector of the
;; following format:
;;	[TYPE MNEMONIC DOC-STRING PLIST FLAGS]
;; We call this vector as coding-spec.  See comments in src/coding.c
;; for more detail.

(defconst coding-spec-type-idx 0)
(defconst coding-spec-mnemonic-idx 1)
(defconst coding-spec-doc-string-idx 2)
(defconst coding-spec-plist-idx 3)
(defconst coding-spec-flags-idx 4)

;; PLIST is a property list of a coding system.  To share PLIST among
;; alias coding systems, a coding system has PLIST in coding-spec
;; instead of having it in normal property list of Lisp symbol.
;; Here's a list of coding system properties currently being used.
;;
;; o coding-category
;;
;; The value is a coding category the coding system belongs to.  The
;; function `make-coding-system' sets this value automatically
;; unless its argument PROPERTIES specifies this property.
;;
;; o alias-coding-systems
;;
;; The value is a list of coding systems of the same alias group.  The
;; first element is the coding system made at first, which we call as
;; `base coding system'.  The function `make-coding-system' sets this
;; value automatically and `define-coding-system-alias' updates it.
;;
;; See the documentation of make-coding-system for the meanings of the
;; following properties.
;;
;; o post-read-conversion
;; o pre-write-conversion
;; o translation-table-for-decode
;; o translation-table-for-encode
;; o safe-chars
;; o safe-charsets
;; o mime-charset
;; o valid-codes (meaningful only for a coding system based on CCL)


(defsubst coding-system-spec (coding-system)
  "Return coding-spec of CODING-SYSTEM."
  (get (check-coding-system coding-system) 'coding-system))

(defun coding-system-type (coding-system)
  "Return the coding type of CODING-SYSTEM.
A coding type is an integer value indicating the encoding method
of CODING-SYSTEM.  See the function `make-coding-system' for more detail."
  (aref (coding-system-spec coding-system) coding-spec-type-idx))

(defun coding-system-mnemonic (coding-system)
  "Return the mnemonic character of CODING-SYSTEM.
The mnemonic character of a coding system is used in mode line
to indicate the coding system.  If the arg is nil, return ?-."
  (let ((spec (coding-system-spec coding-system)))
    (if spec (aref spec coding-spec-mnemonic-idx) ?-)))

(defun coding-system-doc-string (coding-system)
  "Return the documentation string for CODING-SYSTEM."
  (aref (coding-system-spec coding-system) coding-spec-doc-string-idx))

(defun coding-system-plist (coding-system)
  "Return the property list of CODING-SYSTEM."
  (aref (coding-system-spec coding-system) coding-spec-plist-idx))

(defun coding-system-flags (coding-system)
  "Return `flags' of CODING-SYSTEM.
A `flags' of a coding system is a vector of length 32 indicating detailed
information of a coding system.  See the function `make-coding-system'
for more detail."
  (aref (coding-system-spec coding-system) coding-spec-flags-idx))

(defun coding-system-get (coding-system prop)
  "Extract a value from CODING-SYSTEM's property list for property PROP."
  (plist-get (coding-system-plist coding-system) prop))

(defun coding-system-put (coding-system prop val)
  "Change value in CODING-SYSTEM's property list PROP to VAL."
  (let ((plist (coding-system-plist coding-system)))
    (if plist
	(plist-put plist prop val)
      (aset (coding-system-spec coding-system) coding-spec-plist-idx
	    (list prop val)))))

(defun coding-system-category (coding-system)
  "Return the coding category of CODING-SYSTEM.
See also `coding-category-list'."
  (coding-system-get coding-system 'coding-category))

(defun coding-system-base (coding-system)
  "Return the base coding system of CODING-SYSTEM.
A base coding system is what made by `make-coding-system'.
Any alias nor subsidiary coding systems are not base coding system."
  (car (coding-system-get coding-system 'alias-coding-systems)))

;; Coding system also has a property `eol-type'.
;;
;; This property indicates how the coding system handles end-of-line
;; format.  The value is integer 0, 1, 2, or a vector of three coding
;; systems.  Each integer value 0, 1, and 2 indicates the format of
;; end-of-line LF, CRLF, and CR respectively.  A vector value
;; indicates that the format of end-of-line should be detected
;; automatically.  Nth element of the vector is the subsidiary coding
;; system whose `eol-type' property is N.

(defun coding-system-eol-type (coding-system)
  "Return eol-type of CODING-SYSTEM.
An eol-type is integer 0, 1, 2, or a vector of coding systems.

Integer values 0, 1, and 2 indicate a format of end-of-line; LF,
CRLF, and CR respectively.

A vector value indicates that a format of end-of-line should be
detected automatically.  Nth element of the vector is the subsidiary
coding system whose eol-type is N."
  (get coding-system 'eol-type))

(defun coding-system-eol-type-mnemonic (coding-system)
  "Return the string indicating end-of-line format of CODING-SYSTEM."
  (let* ((eol-type (coding-system-eol-type coding-system))
	 (val (cond ((eq eol-type 0) eol-mnemonic-unix)
		    ((eq eol-type 1) eol-mnemonic-dos)
		    ((eq eol-type 2) eol-mnemonic-mac)
		    (t eol-mnemonic-undecided))))
    (if (stringp val)
	val
      (char-to-string val))))

(defun coding-system-lessp (x y)
  (cond ((eq x 'no-conversion) t)
	((eq y 'no-conversion) nil)
	((eq x 'emacs-mule) t)
	((eq y 'emacs-mule) nil)
	((eq x 'undecided) t)
	((eq y 'undecided) nil)
	(t (let ((c1 (coding-system-mnemonic x))
		 (c2 (coding-system-mnemonic y)))
	     (or (< (downcase c1) (downcase c2))
		 (and (not (> (downcase c1) (downcase c2)))
		      (< c1 c2)))))))

(defun coding-system-equal (coding-system-1 coding-system-2)
  "Return t if and only if CODING-SYSTEM-1 and CODING-SYSTEM-2 are identical.
Two coding systems are identical if two symbols are equal
or one is an alias of the other."
  (or (eq coding-system-1 coding-system-2)
      (and (equal (coding-system-spec coding-system-1)
		  (coding-system-spec coding-system-2))
	   (let ((eol-type-1 (coding-system-eol-type coding-system-1))
		 (eol-type-2 (coding-system-eol-type coding-system-2)))
	     (or (eq eol-type-1 eol-type-2)
		 (and (vectorp eol-type-1) (vectorp eol-type-2)))))))

(defun add-to-coding-system-list (coding-system)
  "Add CODING-SYSTEM to `coding-system-list' while keeping it sorted."
  (if (or (null coding-system-list)
	  (coding-system-lessp coding-system (car coding-system-list)))
      (setq coding-system-list (cons coding-system coding-system-list))
    (let ((len (length coding-system-list))
	  mid (tem coding-system-list))
      (while (> len 1)
	(setq mid (nthcdr (/ len 2) tem))
	(if (coding-system-lessp (car mid) coding-system)
	    (setq tem mid
		  len (- len (/ len 2)))
	  (setq len (/ len 2))))
      (setcdr tem (cons coding-system (cdr tem))))))

(defun coding-system-list (&optional base-only)
  "Return a list of all existing non-subsidiary coding systems.
If optional arg BASE-ONLY is non-nil, only base coding systems are listed.
The value doesn't include subsidiary coding systems which are what
made from bases and aliases automatically for various end-of-line
formats (e.g. iso-latin-1-unix, koi8-r-dos)."
  (let* ((codings (copy-sequence coding-system-list))
	 (tail (cons nil codings)))
    ;; Remove subsidiary coding systems (eol variants) and alias
    ;; coding systems (if necessary).
    (while (cdr tail)
      (let* ((coding (car (cdr tail)))
	     (aliases (coding-system-get coding 'alias-coding-systems)))
	(if (or
	     ;; CODING is an eol variant if not in ALIASES.
	     (not (memq coding aliases))
	     ;; CODING is an alias if it is not car of ALIASES.
	     (and base-only (not (eq coding (car aliases)))))
	    (setcdr tail (cdr (cdr tail)))
	  (setq tail (cdr tail)))))
    codings))

(defun map-charset-chars (func charset)
  "Use FUNC to map over all characters in CHARSET for side effects.
FUNC is a function of two args, the start and end (inclusive) of a
character code range.  Thus FUNC should iterate over [START, END]."
  (let* ((dim (charset-dimension charset))
	 (chars (charset-chars charset))
	 (start (if (= chars 94)
		    33
		  32)))
    (if (= dim 1)
	(funcall func
		 (make-char charset start)
		 (make-char charset (+ start chars -1)))
      (dotimes (i chars)
	(funcall func
		 (make-char charset (+ i start) start)
		 (make-char charset (+ i start) (+ start chars -1)))))))

(defalias 'register-char-codings 'ignore "")
(make-obsolete 'register-char-codings
               "it exists just for backward compatibility, and does nothing."
	       "21.3")

(defconst char-coding-system-table nil
  "This is an obsolete variable.
It exists just for backward compatibility, and the value is always nil.")

(defun make-subsidiary-coding-system (coding-system)
  "Make subsidiary coding systems (eol-type variants) of CODING-SYSTEM."
  (let ((coding-spec (coding-system-spec coding-system))
	(subsidiaries (vector (intern (format "%s-unix" coding-system))
			      (intern (format "%s-dos" coding-system))
			      (intern (format "%s-mac" coding-system))))
	(i 0)
	temp)
    (while (< i 3)
      (put (aref subsidiaries i) 'coding-system coding-spec)
      (put (aref subsidiaries i) 'eol-type i)
      (add-to-coding-system-list (aref subsidiaries i))
      (setq coding-system-alist
	    (cons (list (symbol-name (aref subsidiaries i)))
		  coding-system-alist))
      (setq i (1+ i)))
    subsidiaries))

(defun transform-make-coding-system-args (name type &optional doc-string props)
  "For internal use only.
Transform XEmacs style args for `make-coding-system' to Emacs style.
Value is a list of transformed arguments."
  (let ((mnemonic (string-to-char (or (plist-get props 'mnemonic) "?")))
	(eol-type (plist-get props 'eol-type))
	properties tmp)
    (cond
     ((eq eol-type 'lf) (setq eol-type 'unix))
     ((eq eol-type 'crlf) (setq eol-type 'dos))
     ((eq eol-type 'cr) (setq eol-type 'mac)))
    (if (setq tmp (plist-get props 'post-read-conversion))
	(setq properties (plist-put properties 'post-read-conversion tmp)))
    (if (setq tmp (plist-get props 'pre-write-conversion))
	(setq properties (plist-put properties 'pre-write-conversion tmp)))
    (cond
     ((eq type 'shift-jis)
      `(,name 1 ,mnemonic ,doc-string () ,properties ,eol-type))
     ((eq type 'iso2022) ; This is not perfect.
      (if (plist-get props 'escape-quoted)
	  (error "escape-quoted is not supported: %S"
		 `(,name ,type ,doc-string ,props)))
      (let ((g0 (plist-get props 'charset-g0))
      	    (g1 (plist-get props 'charset-g1))
      	    (g2 (plist-get props 'charset-g2))
      	    (g3 (plist-get props 'charset-g3))
      	    (use-roman
             (and
	      (eq (cadr (assoc 'latin-jisx0201
			       (plist-get props 'input-charset-conversion)))
		  'ascii)
	      (eq (cadr (assoc 'ascii
			       (plist-get props 'output-charset-conversion)))
		  'latin-jisx0201)))
            (use-oldjis
             (and
	      (eq (cadr (assoc 'japanese-jisx0208-1978
			       (plist-get props 'input-charset-conversion)))
		  'japanese-jisx0208)
	      (eq (cadr (assoc 'japanese-jisx0208
			       (plist-get props 'output-charset-conversion)))
		  'japanese-jisx0208-1978))))
	(if (charsetp g0)
	    (if (plist-get props 'force-g0-on-output)
		(setq g0 `(nil ,g0))
	      (setq g0 `(,g0 t))))
	(if (charsetp g1)
	    (if (plist-get props 'force-g1-on-output)
		(setq g1 `(nil ,g1))
	      (setq g1 `(,g1 t))))
	(if (charsetp g2)
	    (if (plist-get props 'force-g2-on-output)
		(setq g2 `(nil ,g2))
	      (setq g2 `(,g2 t))))
	(if (charsetp g3)
	    (if (plist-get props 'force-g3-on-output)
		(setq g3 `(nil ,g3))
	      (setq g3 `(,g3 t))))
	`(,name 2 ,mnemonic ,doc-string
	  (,g0 ,g1 ,g2 ,g3
	   ,(plist-get props 'short)
	   ,(not (plist-get props 'no-ascii-eol))
	   ,(not (plist-get props 'no-ascii-cntl))
	   ,(plist-get props 'seven)
	   t
	   ,(not (plist-get props 'lock-shift))
	   ,use-roman
	   ,use-oldjis
	   ,(plist-get props 'no-iso6429)
	   nil nil nil nil)
	,properties ,eol-type)))
     ((eq type 'big5)
      `(,name 3 ,mnemonic ,doc-string () ,properties ,eol-type))
     ((eq type 'ccl)
      `(,name 4 ,mnemonic ,doc-string
	      (,(plist-get props 'decode) . ,(plist-get props 'encode))
	      ,properties ,eol-type))
     (t
      (error "unsupported XEmacs style make-coding-style arguments: %S"
	     `(,name ,type ,doc-string ,props))))))

(defun make-coding-system (coding-system type mnemonic doc-string
					 &optional
					 flags
					 properties
					 eol-type)
  "Define a new coding system CODING-SYSTEM (symbol).
Remaining arguments are TYPE, MNEMONIC, DOC-STRING, FLAGS (optional),
and PROPERTIES (optional) which construct a coding-spec of CODING-SYSTEM
in the following format:
	[TYPE MNEMONIC DOC-STRING PLIST FLAGS]

TYPE is an integer value indicating the type of the coding system as follows:
  0: Emacs internal format,
  1: Shift-JIS (or MS-Kanji) used mainly on Japanese PCs,
  2: ISO-2022 including many variants,
  3: Big5 used mainly on Chinese PCs,
  4: private, CCL programs provide encoding/decoding algorithm,
  5: Raw-text, which means that text contains random 8-bit codes.

MNEMONIC is a character to be displayed on mode line for the coding system.

DOC-STRING is a documentation string for the coding system.

FLAGS specifies more detailed information of the coding system as follows:

  If TYPE is 2 (ISO-2022), FLAGS is a list of these elements:
      CHARSET0, CHARSET1, CHARSET2, CHARSET3, SHORT-FORM,
      ASCII-EOL, ASCII-CNTL, SEVEN, LOCKING-SHIFT, SINGLE-SHIFT,
      USE-ROMAN, USE-OLDJIS, NO-ISO6429, INIT-BOL, DESIGNATION-BOL,
      SAFE, ACCEPT-LATIN-EXTRA-CODE.
    CHARSETn are character sets initially designated to Gn graphic registers.
      If CHARSETn is nil, Gn is never used.
      If CHARSETn is t, Gn can be used but nothing designated initially.
      If CHARSETn is a list of character sets, those character sets are
        designated to Gn on output, but nothing designated to Gn initially.
        But, character set `ascii' can be designated only to G0.
    SHORT-FORM non-nil means use short designation sequence on output.
    ASCII-EOL non-nil means designate ASCII to g0 at end of line on output.
    ASCII-CNTL non-nil means designate ASCII to g0 before control codes and
      SPACE on output.
    SEVEN non-nil means use 7-bit code only on output.
    LOCKING-SHIFT non-nil means use locking-shift.
    SINGLE-SHIFT non-nil means use single-shift.
    USE-ROMAN non-nil means designate JIS0201-1976-Roman instead of ASCII.
    USE-OLDJIS non-nil means designate JIS0208-1976 instead of JIS0208-1983.
    NO-ISO6429 non-nil means not use ISO6429's direction specification.
    INIT-BOL non-nil means any designation state is assumed to be reset
      to initial at each beginning of line on output.
    DESIGNATION-BOL non-nil means designation sequences should be placed
      at beginning of line on output.
    SAFE non-nil means convert unsafe characters to `?' on output.
      Characters not specified in the property `safe-charsets' nor
      `safe-chars' are unsafe.
    ACCEPT-LATIN-EXTRA-CODE non-nil means code-detection routine accepts
      a code specified in `latin-extra-code-table' (which see) as a valid
      code of the coding system.

  If TYPE is 4 (private), FLAGS should be a cons of CCL programs, for
    decoding and encoding.  CCL programs should be specified by their
    symbols.

PROPERTIES is an alist of properties vs the corresponding values.  The
following properties are recognized:

  o post-read-conversion

  The value is a function to call after some text is inserted and
  decoded by the coding system itself and before any functions in
  `after-insert-functions' are called.  The argument of this
  function is the same as for a function in
  `after-insert-file-functions', i.e. LENGTH of the text inserted,
  with point at the head of the text to be decoded.

  o pre-write-conversion

  The value is a function to call after all functions in
  `write-region-annotate-functions' and `buffer-file-format' are
  called, and before the text is encoded by the coding system itself.
  The arguments to this function are the same as those of a function
  in `write-region-annotate-functions', i.e. FROM and TO, specifying
  a region of text.

  o translation-table-for-decode

  The value is a translation table to be applied on decoding.  See
  the function `make-translation-table' for the format of translation
  table.  This is not applicable to type 4 (CCL-based) coding systems.

  o translation-table-for-encode

  The value is a translation table to be applied on encoding.  This is
  not applicable to type 4 (CCL-based) coding systems.

  o safe-chars

  The value is a char table.  If a character has non-nil value in it,
  the character is safely supported by the coding system.  This
  overrides the specification of safe-charsets.

  o safe-charsets

  The value is a list of charsets safely supported by the coding
  system.  The value t means that all charsets Emacs handles are
  supported.  Even if some charset is not in this list, it doesn't
  mean that the charset can't be encoded in the coding system;
  it just means that some other receiver of text encoded
  in the coding system won't be able to handle that charset.

  o mime-charset

  The value is a symbol whose name is the `MIME-charset' parameter of
  the coding system.

  o mime-text-unsuitable

  A non-nil value means the `mime-charset' property names a charset
  which is unsuitable for the top-level media type \"text\".

  o valid-codes (meaningful only for a coding system based on CCL)

  The value is a list to indicate valid byte ranges of the encoded
  file.  Each element of the list is an integer or a cons of integer.
  In the former case, the integer value is a valid byte code.  In the
  latter case, the integers specify the range of valid byte codes.

  o composition (meaningful only when TYPE is 0 or 2)

  If the value is non-nil, the coding system preserves composition
  information.

  o ascii-incompatible

  If the value is non-nil, the coding system is not compatible
  with ASCII, which means it encodes or decodes ASCII character
  string to the different byte sequence.

These properties are set in PLIST, a property list.  This function
also sets properties `coding-category' and `alias-coding-systems'
automatically.

EOL-TYPE specifies the EOL type of the coding-system in one of the
following formats:

  o symbol (unix, dos, or mac)

	The symbol `unix' means Unix-like EOL (LF), `dos' means
	DOS-like EOL (CRLF), and `mac' means MAC-like EOL (CR).

  o number (0, 1, or 2)

	The number 0, 1, and 2 mean UNIX, DOS, and MAC-like EOL
	respectively.

  o vector of coding-systems of length 3

	The EOL type is detected automatically for the coding system.
	And, according to the detected EOL type, one of the coding
	systems in the vector is selected.  Elements of the vector
	corresponds to Unix-like EOL, DOS-like EOL, and Mac-like EOL
	in this order.

Kludgy features for backward compatibility:

1. If TYPE is 4 and car or cdr of FLAGS is a vector, the vector is
treated as a compiled CCL code.

2. If PROPERTIES is just a list of character sets, the list is set as
a value of `safe-charsets' in PLIST."

  ;; For compatiblity with XEmacs, we check the type of TYPE.  If it
  ;; is a symbol, perhaps, this function is called with XEmacs-style
  ;; arguments.  Here, try to transform that kind of arguments to
  ;; Emacs style.
  (if (symbolp type)
      (let ((args (transform-make-coding-system-args coding-system type
						     mnemonic doc-string)))
	(setq coding-system (car args)
	      type (nth 1 args)
	      mnemonic (nth 2 args)
	      doc-string (nth 3 args)
	      flags (nth 4 args)
	      properties (nth 5 args)
	      eol-type (nth 6 args))))

  ;; Set a value of `coding-system' property.
  (let ((coding-spec (make-vector 5 nil))
	(no-initial-designation t)
	(no-alternative-designation t)
	(accept-latin-extra-code nil)
	coding-category)
    (if (or (not (integerp type)) (< type 0) (> type 5))
	(error "TYPE argument must be 0..5"))
    (if (or (not (integerp mnemonic)) (<= mnemonic ? ) (> mnemonic 127))
	(error "MNEMONIC argument must be an ASCII printable character"))
    (aset coding-spec coding-spec-type-idx type)
    (aset coding-spec coding-spec-mnemonic-idx mnemonic)
    (aset coding-spec coding-spec-doc-string-idx
	  (purecopy (if (stringp doc-string) doc-string "")))
    (cond ((= type 0)
	   (setq coding-category 'coding-category-emacs-mule))
	  ((= type 1)
	   (setq coding-category 'coding-category-sjis))
	  ((= type 2)			; ISO2022
	   (let ((i 0)
		 (vec (make-vector 32 nil))
		 (g1-designation nil)
		 (fl flags))
	     (while (< i 4)
	       (let ((charset (car fl)))
		 (if (and no-initial-designation
			  (> i 0)
			  (or (charsetp charset)
			      (and (consp charset)
				   (charsetp (car charset)))))
		     (setq no-initial-designation nil))
		 (if (charsetp charset)
		     (if (= i 1) (setq g1-designation charset))
		   (if (consp charset)
		       (let ((tail charset)
			     elt)
			 (while tail
			   (setq elt (car tail))
			   (if (eq elt t)
			       (setq no-alternative-designation nil)
			     (if (and elt (not (charsetp elt)))
				 (error "Invalid charset: %s" elt)))
			   (setq tail (cdr tail)))
			 (setq g1-designation (car charset)))
		     (if charset
			 (if (eq charset t)
			     (setq no-alternative-designation nil)
			   (error "Invalid charset: %s" charset)))))
		 (aset vec i charset))
	       (setq fl (cdr fl) i (1+ i)))
	     (while (and (< i 32) fl)
	       (aset vec i (car fl))
	       (if (and (= i 16)	; ACCEPT-LATIN-EXTRA-CODE
			(car fl))
		   (setq accept-latin-extra-code t))
	       (setq fl (cdr fl) i (1+ i)))
	     (aset coding-spec 4 vec)
	     (setq coding-category
		   (if (aref vec 8)	; Use locking-shift.
		       (or (and (aref vec 7) 'coding-category-iso-7-else)
			   'coding-category-iso-8-else)
		     (if (aref vec 7)	; 7-bit only.
			 (if (aref vec 9) ; Use single-shift.
			     'coding-category-iso-7-else
			   (if no-alternative-designation
			       'coding-category-iso-7-tight
			     'coding-category-iso-7))
		       (if (or no-initial-designation
			       (not no-alternative-designation))
			   'coding-category-iso-8-else
			 (if (and (charsetp g1-designation)
				  (= (charset-dimension g1-designation) 2))
			     'coding-category-iso-8-2
			   'coding-category-iso-8-1)))))))
	  ((= type 3)
	   (setq coding-category 'coding-category-big5))
	  ((= type 4)			; private
	   (setq coding-category 'coding-category-ccl)
	   (if (not (consp flags))
	       (error "Invalid FLAGS argument for TYPE 4 (CCL)")
	     (let ((decoder (check-ccl-program
			     (car flags)
			     (intern (format "%s-decoder" coding-system))))
		   (encoder (check-ccl-program
			     (cdr flags)
			     (intern (format "%s-encoder" coding-system)))))
	       (if (and decoder encoder)
		   (aset coding-spec 4 (cons decoder encoder))
		 (error "Invalid FLAGS argument for TYPE 4 (CCL)")))))
	  (t				; i.e. (= type 5)
	   (setq coding-category 'coding-category-raw-text)))

    (let ((plist (list 'coding-category coding-category
		       'alias-coding-systems (list coding-system))))
      (if no-initial-designation
	  (plist-put plist 'no-initial-designation t))
      (if (and properties
	       (or (eq properties t)
		   (not (consp (car properties)))))
	  ;; In the old version, the arg PROPERTIES is a list to be
	  ;; set in PLIST as a value of property `safe-charsets'.
	  (setq properties (list (cons 'safe-charsets properties))))
      ;; In the current version PROPERTIES is a property list.
      ;; Reflect it into PLIST one by one while handling safe-chars
      ;; specially.
      (let ((safe-charsets (cdr (assq 'safe-charsets properties)))
	    (safe-chars (cdr (assq 'safe-chars properties)))
	    (l properties)
	    prop val)
	;; If only safe-charsets is specified, make a char-table from
	;; it, and store that char-table as the value of `safe-chars'.
	(if (and (not safe-chars) safe-charsets)
	    (let (charset)
	      (if (eq safe-charsets t)
		  (setq safe-chars t)
		(setq safe-chars (make-char-table 'safe-chars))
		(while safe-charsets
		  (setq charset (car safe-charsets)
			safe-charsets (cdr safe-charsets))
		  (cond ((eq charset 'ascii)) ; just ignore
			((eq charset 'eight-bit-control)
			 (let ((i 128))
			   (while (< i 160)
			     (aset safe-chars i t)
			     (setq i (1+ i)))))
			((eq charset 'eight-bit-graphic)
			 (let ((i 160))
			   (while (< i 256)
			     (aset safe-chars i t)
			     (setq i (1+ i)))))
			(t
			 (aset safe-chars (make-char charset) t))))
		(if accept-latin-extra-code
		    (let ((i 128))
		      (while (< i 160)
			(if (aref latin-extra-code-table i)
			    (aset safe-chars i t))
			(setq i (1+ i))))))
	      (setq l (cons (cons 'safe-chars safe-chars) l))))
	(while l
	  (setq prop (car (car l)) val (cdr (car l)) l (cdr l))
	  (if (eq prop 'safe-chars)
	      (progn
		(if (and (symbolp val)
			 (get val 'translation-table))
		    (setq safe-chars (get val 'translation-table)))
		(setq val safe-chars)))
	  (plist-put plist prop val)))
      ;; The property `coding-category' may have been set differently
      ;; through PROPERTIES.
      (setq coding-category (plist-get plist 'coding-category))
      (aset coding-spec coding-spec-plist-idx plist))
    (put coding-system 'coding-system coding-spec)
    (put coding-system 'coding-system-define-form nil)
    (put coding-category 'coding-systems
	 (cons coding-system (get coding-category 'coding-systems))))

  ;; Next, set a value of `eol-type' property.
  (if (not eol-type)
      ;; If EOL-TYPE is nil, set a vector of subsidiary coding
      ;; systems, each corresponds to a coding system for the detected
      ;; EOL format.
      (setq eol-type (make-subsidiary-coding-system coding-system)))
  (setq eol-type
	(cond ((or (eq eol-type 'unix) (null eol-type))
	       0)
	      ((eq eol-type 'dos)
	       1)
	      ((eq eol-type 'mac)
	       2)
	      ((or (and (vectorp eol-type)
			(= (length eol-type) 3))
		   (and (numberp eol-type)
			(and (>= eol-type 0)
			     (<= eol-type 2))))
	       eol-type)
	      (t
	       (error "Invalid EOL-TYPE spec:%S" eol-type))))
  (put coding-system 'eol-type eol-type)

  (define-coding-system-internal coding-system)

  ;; At last, register CODING-SYSTEM in `coding-system-list' and
  ;; `coding-system-alist'.
  (add-to-coding-system-list coding-system)
  (setq coding-system-alist (cons (list (symbol-name coding-system))
				  coding-system-alist))

  ;; For a coding system of cateogory iso-8-1 and iso-8-2, create
  ;; XXX-with-esc variants.
  (let ((coding-category (coding-system-category coding-system)))
    (if (or (eq coding-category 'coding-category-iso-8-1)
	    (eq coding-category 'coding-category-iso-8-2))
	(let ((esc (intern (concat (symbol-name coding-system) "-with-esc")))
	      (doc (format "Same as %s but can handle any charsets by ISO's escape sequences." coding-system))
	      (safe-charsets (assq 'safe-charsets properties))
	      (mime-charset (assq 'mime-charset properties)))
	  (if safe-charsets
	      (setcdr safe-charsets t)
	    (setq properties (cons (cons 'safe-charsets t) properties)))
	  (if mime-charset
	      (setcdr mime-charset nil))
	  (make-coding-system esc type mnemonic doc
			      (if (listp (car flags))
				  (cons (append (car flags) '(t)) (cdr flags))
				(cons (list (car flags) t) (cdr flags)))
			      properties))))

  coding-system)

(put 'safe-chars 'char-table-extra-slots 0)

(defun define-coding-system-alias (alias coding-system)
  "Define ALIAS as an alias for coding system CODING-SYSTEM."
  (put alias 'coding-system (coding-system-spec coding-system))
  (put alias 'coding-system-define-form nil)
  (add-to-coding-system-list alias)
  (setq coding-system-alist (cons (list (symbol-name alias))
				  coding-system-alist))
  (let ((eol-type (coding-system-eol-type coding-system)))
    (if (vectorp eol-type)
	(progn
	  (nconc (coding-system-get alias 'alias-coding-systems) (list alias))
	  (put alias 'eol-type (make-subsidiary-coding-system alias)))
      (put alias 'eol-type eol-type))))

(defun merge-coding-systems (first second)
  "Fill in any unspecified aspects of coding system FIRST from SECOND.
Return the resulting coding system."
  (let ((base (coding-system-base second))
	(eol (coding-system-eol-type second)))
    ;; If FIRST doesn't specify text conversion, merge with that of SECOND.
    (if (eq (coding-system-base first) 'undecided)
	(setq first (coding-system-change-text-conversion first base)))
    ;; If FIRST doesn't specify eol conversion, merge with that of SECOND.
    (if (and (vectorp (coding-system-eol-type first))
	     (numberp eol) (>= eol 0) (<= eol 2))
	(setq first (coding-system-change-eol-conversion
		     first eol)))
    first))

(defun autoload-coding-system (symbol form)
  "Define SYMBOL as a coding-system that is defined on demand.

FROM is a form to evaluate to define the coding-system."
  (put symbol 'coding-system-define-form form)
  (setq coding-system-alist (cons (list (symbol-name symbol))
				  coding-system-alist)))

(defun set-buffer-file-coding-system (coding-system &optional force nomodify)
  "Set the file coding-system of the current buffer to CODING-SYSTEM.
This means that when you save the buffer, it will be converted
according to CODING-SYSTEM.  For a list of possible values of CODING-SYSTEM,
use \\[list-coding-systems].

If CODING-SYSTEM leaves the text conversion unspecified, or if it
leaves the end-of-line conversion unspecified, FORCE controls what to
do.  If FORCE is nil, get the unspecified aspect (or aspects) from the
buffer's previous `buffer-file-coding-system' value (if it is
specified there).  Otherwise, leave it unspecified.

This marks the buffer modified so that the succeeding \\[save-buffer]
surely saves the buffer with CODING-SYSTEM.  From a program, if you
don't want to mark the buffer modified, specify t for NOMODIFY.
If you know exactly what coding system you want to use,
just set the variable `buffer-file-coding-system' directly."
  (interactive "zCoding system for saving file (default nil): \nP")
  (check-coding-system coding-system)
  (if (and coding-system buffer-file-coding-system (null force))
      (setq coding-system
	    (merge-coding-systems coding-system buffer-file-coding-system)))
  (setq buffer-file-coding-system coding-system)
  ;; This is in case of an explicit call.  Normally, `normal-mode' and
  ;; `set-buffer-major-mode-hook' take care of setting the table.
  (if (fboundp 'ucs-set-table-for-input) ; don't lose when building
      (ucs-set-table-for-input))
  (unless nomodify
    (set-buffer-modified-p t))
  (force-mode-line-update))

(defun revert-buffer-with-coding-system (coding-system &optional force)
  "Visit the current buffer's file again using coding system CODING-SYSTEM.
For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems].

If CODING-SYSTEM leaves the text conversion unspecified, or if it
leaves the end-of-line conversion unspecified, FORCE controls what to
do.  If FORCE is nil, get the unspecified aspect (or aspects) from the
buffer's previous `buffer-file-coding-system' value (if it is
specified there).  Otherwise, determine it from the file contents as
usual for visiting a file."
  (interactive "zCoding system for visited file (default nil): \nP")
  (check-coding-system coding-system)
  (if (and coding-system buffer-file-coding-system (null force))
      (setq coding-system
	    (merge-coding-systems coding-system buffer-file-coding-system)))
  (let ((coding-system-for-read coding-system))
    (revert-buffer)))

(defun set-file-name-coding-system (coding-system)
  "Set coding system for decoding and encoding file names to CODING-SYSTEM.
It actually just set the variable `file-name-coding-system' (which
see) to CODING-SYSTEM."
  (interactive "zCoding system for file names (default nil): ")
  (check-coding-system coding-system)
  (if (and coding-system
	   (coding-system-get coding-system 'ascii-incompatible))
      (error "%s is not ASCII-compatible" coding-system))
  (setq file-name-coding-system coding-system))

(defvar default-terminal-coding-system nil
  "Default value for the terminal coding system.
This is normally set according to the selected language environment.
See also the command `set-terminal-coding-system'.")

(defun set-terminal-coding-system (coding-system)
  "Set coding system of your terminal to CODING-SYSTEM.
All text output to the terminal will be encoded
with the specified coding system.
For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems].
The default is determined by the selected language environment
or by the previous use of this command."
  (interactive
   (list (let ((default (if (and (not (terminal-coding-system))
				 default-terminal-coding-system)
			    default-terminal-coding-system)))
	   (read-coding-system
	    (format "Coding system for terminal display (default %s): "
		    default)
	    default))))
  (if (and (not coding-system)
	   (not (terminal-coding-system)))
      (setq coding-system default-terminal-coding-system))
  (if coding-system
      (setq default-terminal-coding-system coding-system))
  (set-terminal-coding-system-internal coding-system)
  (redraw-frame (selected-frame)))

(defvar default-keyboard-coding-system nil
  "Default value of the keyboard coding system.
This is normally set according to the selected language environment.
See also the command `set-keyboard-coding-system'.")

(defun set-keyboard-coding-system (coding-system)
  "Set coding system for keyboard input to CODING-SYSTEM.
In addition, this command enables Encoded-kbd minor mode.
\(If CODING-SYSTEM is nil, Encoded-kbd mode is turned off -- see
`encoded-kbd-mode'.)
For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems].
The default is determined by the selected language environment
or by the previous use of this command."
  (interactive
   (list (let ((default (if (and (not (keyboard-coding-system))
				 default-keyboard-coding-system)
			    default-keyboard-coding-system)))
	   (read-coding-system
	    (format "Coding system for keyboard input (default %s): "
		    default)
	    default))))
  (if (and (not coding-system)
	   (not (keyboard-coding-system)))
      (setq coding-system default-keyboard-coding-system))
  (if coding-system
      (setq default-keyboard-coding-system coding-system))
  (if (and coding-system
	   (coding-system-get coding-system 'ascii-incompatible))
      (error "%s is not ASCII-compatible" coding-system))
  (set-keyboard-coding-system-internal coding-system)
  (setq keyboard-coding-system coding-system)
  (encoded-kbd-mode (if coding-system 1 0)))

(defcustom keyboard-coding-system nil
  "Specify coding system for keyboard input.
If you set this on a terminal which can't distinguish Meta keys from
8-bit characters, you will have to use ESC to type Meta characters.
See Info node `Specify Coding' and Info node `Single-Byte Character Support'.

On non-windowing terminals, this is set from the locale by default.

Setting this variable directly does not take effect;
use either \\[customize] or \\[set-keyboard-coding-system]."
  :type '(coding-system :tag "Coding system")
  :link '(info-link "(emacs)Specify Coding")
  :link '(info-link "(emacs)Single-Byte Character Support")
  :set (lambda (symbol value)
	 ;; Don't load encoded-kbd-mode unnecessarily.
	 (if (or value (boundp 'encoded-kbd-mode))
	     (set-keyboard-coding-system value)
	   (set-default 'keyboard-coding-system nil))) ; must initialize
  :version "22.1"
  :group 'keyboard
  :group 'mule)

(defun set-buffer-process-coding-system (decoding encoding)
  "Set coding systems for the process associated with the current buffer.
DECODING is the coding system to be used to decode input from the process,
ENCODING is the coding system to be used to encode output to the process.

For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems]."
  (interactive
   "zCoding-system for output from the process: \nzCoding-system for input to the process: ")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (null proc)
	(error "No process")
      (check-coding-system decoding)
      (check-coding-system encoding)
      (set-process-coding-system proc decoding encoding)))
  (force-mode-line-update))

(defalias 'set-clipboard-coding-system 'set-selection-coding-system)

(defun set-selection-coding-system (coding-system)
  "Make CODING-SYSTEM used for communicating with other X clients.
When sending or receiving text via cut_buffer, selection, and clipboard,
the text is encoded or decoded by CODING-SYSTEM."
  (interactive "zCoding system for X selection: ")
  (check-coding-system coding-system)
  (setq selection-coding-system coding-system))

;; Coding system lastly specified by the command
;; set-next-selection-coding-system.
(defvar last-next-selection-coding-system nil)

(defun set-next-selection-coding-system (coding-system)
  "Make CODING-SYSTEM used for the next communication with other X clients.
This setting is effective for the next communication only."
  (interactive
   (list (read-coding-system
	  (if last-next-selection-coding-system
	      (format "Coding system for the next X selection (default %S): "
		      last-next-selection-coding-system)
	    "Coding system for the next X selection: ")
	  last-next-selection-coding-system)))
  (if coding-system
      (setq last-next-selection-coding-system coding-system)
    (setq coding-system last-next-selection-coding-system))
  (check-coding-system coding-system)

  (setq next-selection-coding-system coding-system))

(defun set-coding-priority (arg)
  "Set priority of coding categories according to ARG.
ARG is a list of coding categories ordered by priority."
  (let ((l arg)
	(current-list (copy-sequence coding-category-list)))
    ;; Check the validity of ARG while deleting coding categories in
    ;; ARG from CURRENT-LIST.  We assume that CODING-CATEGORY-LIST
    ;; contains all coding categories.
    (while l
      (if (or (null (get (car l) 'coding-category-index))
	      (null (memq (car l) current-list)))
	  (error "Invalid or duplicated element in argument: %s" arg))
      (setq current-list (delq (car l) current-list))
      (setq l (cdr l)))
    ;; Update `coding-category-list' and return it.
    (setq coding-category-list (append arg current-list))
    (set-coding-priority-internal)))

;;; X selections

(defvar ctext-non-standard-encodings-alist
  '(("big5-0" big5 2 (chinese-big5-1 chinese-big5-2))
    ("ISO8859-14" iso-8859-14 1 latin-iso8859-14)
    ("ISO8859-15" iso-8859-15 1 latin-iso8859-15))
  "Alist of non-standard encoding names vs the corresponding usages in CTEXT.

It controls how extended segments of a compound text are handled
by the coding system `compound-text-with-extensions'.

Each element has the form (ENCODING-NAME CODING-SYSTEM N-OCTET CHARSET).

ENCODING-NAME is an encoding name of an \"extended segments\".

CODING-SYSTEM is the coding-system to encode (or decode) the
characters into (or from) the extended segment.

N-OCTET is the number of octets (bytes) that encodes a character
in the segment.  It can be 0 (meaning the number of octets per
character is variable), 1, 2, 3, or 4.

CHARSET is a charater set containing characters that are encoded
in the segment.  It can be a list of character sets.  It can also
be a char-table, in which case characters that have non-nil value
in the char-table are the target.

On decoding CTEXT, all encoding names listed here are recognized.

On encoding CTEXT, encoding names in the variable
`ctext-non-standard-encodings' (which see) and in the information
listed for the current language environment under the key
`ctext-non-standard-encodings' are used.")

(defvar ctext-non-standard-encodings
  '("big5-0")
  "List of non-standard encoding names used in extended segments of CTEXT.
Each element must be one of the names listed in the variable
`ctext-non-standard-encodings-alist' (which see).")

(defvar ctext-non-standard-encodings-regexp
  (string-to-multibyte
   (concat
    ;; For non-standard encodings.
    "\\(\e%/[0-4][\200-\377][\200-\377]\\([^\002]+\\)\002\\)"
    "\\|"
    ;; For UTF-8 encoding.
    "\\(\e%G[^\e]*\e%@\\)")))

;; Functions to support "Non-Standard Character Set Encodings" defined
;; by the COMPOUND-TEXT spec.  They also support "The UTF-8 encoding"
;; described in the section 7 of the documentation of COMPOUND-TEXT
;; distributed with XFree86.

(defun ctext-post-read-conversion (len)
  "Decode LEN characters encoded as Compound Text with Extended Segments."
  (save-match-data
    (save-restriction
      (let ((case-fold-search nil)
	    (in-workbuf (string= (buffer-name) " *code-converting-work*"))
	    last-coding-system-used
	    pos bytes)
	(or in-workbuf
	    (narrow-to-region (point) (+ (point) len)))
	(if in-workbuf
	    (set-buffer-multibyte t))
	(while (re-search-forward ctext-non-standard-encodings-regexp
				  nil 'move)
	  (setq pos (match-beginning 0))
	  (if (match-beginning 1)
	      ;; ESC % / [0-4] M L --ENCODING-NAME-- \002 --BYTES--
	      (let* ((M (char-after (+ pos 4)))
		     (L (char-after (+ pos 5)))
		     (encoding (match-string 2))
		     (encoding-info (assoc-string
				     encoding
				     ctext-non-standard-encodings-alist t))
		     (coding (if encoding-info
				 (nth 1 encoding-info)
			       (setq encoding (intern (downcase encoding)))
			       (and (coding-system-p encoding)
				    encoding))))
		(setq bytes (- (+ (* (- M 128) 128) (- L 128))
			       (- (point) (+ pos 6))))
		(when coding
		  (delete-region pos (point))
		  (forward-char bytes)
		  (decode-coding-region (- (point) bytes) (point) coding)))
	    ;; ESC % G --UTF-8-BYTES-- ESC % @
	    (delete-char -3)
	    (delete-region pos (+ pos 3))
	    (decode-coding-region pos (point) 'utf-8))))
      (goto-char (point-min))
      (- (point-max) (point)))))

;; Return a char table of extended segment usage for each character.
;; Each value of the char table is nil, one of the elements of
;; `ctext-non-standard-encodings-alist', or the symbol `utf-8'.

(defun ctext-non-standard-encodings-table ()
  (let ((table (make-char-table 'translation-table)))
    (aset table (make-char 'mule-unicode-0100-24ff) 'utf-8)
    (aset table (make-char 'mule-unicode-2500-33ff) 'utf-8)
    (aset table (make-char 'mule-unicode-e000-ffff) 'utf-8)
    (dolist (encoding (reverse
		       (append
			(get-language-info current-language-environment
					   'ctext-non-standard-encodings)
			ctext-non-standard-encodings)))
      (let* ((slot (assoc encoding ctext-non-standard-encodings-alist))
	     (charset (nth 3 slot)))
	(if charset
	    (cond ((charsetp charset)
		   (aset table (make-char charset) slot))
		  ((listp charset)
		   (dolist (elt charset)
		     (aset table (make-char elt) slot)))
		  ((char-table-p charset)
		   (map-char-table #'(lambda (k v)
				   (if (and v (> k 128)) (aset table k slot)))
				   charset))))))
    table))

(defun ctext-pre-write-conversion (from to)
  "Encode characters between FROM and TO as Compound Text w/Extended Segments.

If FROM is a string, or if the current buffer is not the one set up for us
by encode-coding-string, generate a new temp buffer, insert the
text, and convert it in the temporary buffer.  Otherwise, convert in-place."
  (save-match-data
    (let ((workbuf (get-buffer-create " *code-conversion-work*")))
      ;; Setup a working buffer if necessary.
      (cond ((stringp from)
	     (set-buffer workbuf)
	     (erase-buffer)
	     (set-buffer-multibyte (multibyte-string-p from))
	     (insert from))
	    ((not (eq (current-buffer) workbuf))
	     (let ((buf (current-buffer))
		   (multibyte enable-multibyte-characters))
	       (set-buffer workbuf)
	       (erase-buffer)
	       (set-buffer-multibyte multibyte)
	       (insert-buffer-substring buf from to)))))

    ;; Now we can encode the whole buffer.
    (let ((encoding-table (ctext-non-standard-encodings-table))
	  last-coding-system-used
	  last-pos last-encoding-info
	  encoding-info end-pos)
      (goto-char (setq last-pos (point-min)))
      (setq end-pos (point-marker))
      (while (re-search-forward "[^\000-\177]+" nil t)
	;; Found a sequence of non-ASCII characters.
	(setq last-pos (match-beginning 0)
	      last-encoding-info (aref encoding-table (char-after last-pos)))
	(set-marker end-pos (match-end 0))
	(goto-char (1+ last-pos))
	(catch 'tag
	  (while t
	    (setq encoding-info
		  (if (< (point) end-pos)
		      (aref encoding-table (following-char))))
	    (unless (eq last-encoding-info encoding-info)
	      (cond ((consp last-encoding-info)
		     ;; Encode the previous range using an extended
		     ;; segment.
		     (let ((encoding-name (car last-encoding-info))
			   (coding-system (nth 1 last-encoding-info))
			   (noctets (nth 2 last-encoding-info))
			   len)
		       (encode-coding-region last-pos (point) coding-system)
		       (setq len (+ (length encoding-name) 1
				    (- (point) last-pos)))
		       (save-excursion
			 (goto-char last-pos)
			 (insert (string-to-multibyte
				  (format "\e%%/%d%c%c%s"
					  noctets
					  (+ (/ len 128) 128)
					  (+ (% len 128) 128)
					  encoding-name))))))
		    ((eq last-encoding-info 'utf-8)
		     ;; Encode the previous range using UTF-8 encoding
		     ;; extention.
		     (encode-coding-region last-pos (point) 'mule-utf-8)
		     (save-excursion
		       (goto-char last-pos)
		       (insert "\e%G"))
		     (insert "\e%@")))
	      (setq last-pos (point)
		    last-encoding-info encoding-info))
	    (if (< (point) end-pos)
		(forward-char 1)
	      (throw 'tag nil)))))
      (set-marker end-pos nil)
      (goto-char (point-min))))
  ;; Must return nil, as build_annotations_2 expects that.
  nil)

;;; FILE I/O

(defcustom auto-coding-alist
  '(("\\.\\(arc\\|zip\\|lzh\\|zoo\\|[jew]ar\\|xpi\\)\\'" . no-conversion)
    ("\\.\\(ARC\\|ZIP\\|LZH\\|ZOO\\|[JEW]AR\\|XPI\\)\\'" . no-conversion)
    ("\\.\\(sx[dmicw]\\|tar\\|tgz\\)\\'" . no-conversion)
    ("\\.\\(gz\\|Z\\|bz\\|bz2\\|gpg\\)\\'" . no-conversion)
    ("\\.\\(jpe?g\\|png\\|gif\\|tiff?\\|p[bpgn]m\\)\\'" . no-conversion)
    ("/#[^/]+#\\'" . emacs-mule))
  "Alist of filename patterns vs corresponding coding systems.
Each element looks like (REGEXP . CODING-SYSTEM).
A file whose name matches REGEXP is decoded by CODING-SYSTEM on reading.

The settings in this alist take priority over `coding:' tags
in the file (see the function `set-auto-coding')
and the contents of `file-coding-system-alist'."
  :group 'files
  :group 'mule
  :type '(repeat (cons (regexp :tag "File name regexp")
		       (symbol :tag "Coding system"))))

(defcustom auto-coding-regexp-alist
  '(("^BABYL OPTIONS:[ \t]*-\\*-[ \t]*rmail[ \t]*-\\*-" . no-conversion))
  "Alist of patterns vs corresponding coding systems.
Each element looks like (REGEXP . CODING-SYSTEM).
A file whose first bytes match REGEXP is decoded by CODING-SYSTEM on reading.

The settings in this alist take priority over `coding:' tags
in the file (see the function `set-auto-coding')
and the contents of `file-coding-system-alist'."
  :group 'files
  :group 'mule
  :type '(repeat (cons (regexp :tag "Regexp")
		       (symbol :tag "Coding system"))))

;; See the bottom of this file for built-in auto coding functions.
(defcustom auto-coding-functions '(sgml-xml-auto-coding-function
				   sgml-html-meta-auto-coding-function)
  "A list of functions which attempt to determine a coding system.

Each function in this list should be written to operate on the
current buffer, but should not modify it in any way.  The buffer
will contain undecoded text of parts of the file.  Each function
should take one argument, SIZE, which says how many
characters (starting from point) it should look at.

If one of these functions succeeds in determining a coding
system, it should return that coding system.  Otherwise, it
should return nil.

If a file has a `coding:' tag, that takes precedence over these
functions, so they won't be called at all."
  :group 'files
  :group 'mule
  :type '(repeat function))

(defvar set-auto-coding-for-load nil
  "Non-nil means look for `load-coding' property instead of `coding'.
This is used for loading and byte-compiling Emacs Lisp files.")

(defun auto-coding-alist-lookup (filename)
  "Return the coding system specified by `auto-coding-alist' for FILENAME."
  (let ((alist auto-coding-alist)
	(case-fold-search (memq system-type '(vax-vms windows-nt ms-dos cygwin)))
	coding-system)
    (while (and alist (not coding-system))
      (if (string-match (car (car alist)) filename)
	  (setq coding-system (cdr (car alist)))
	(setq alist (cdr alist))))
    coding-system))

(defun find-auto-coding (filename size)
  "Find a coding system for a file FILENAME of which SIZE bytes follow point.
These bytes should include at least the first 1k of the file
and the last 3k of the file, but the middle may be omitted.

The function checks FILENAME against the variable `auto-coding-alist'.
If FILENAME doesn't match any entries in the variable, it checks the
contents of the current buffer following point against
`auto-coding-regexp-alist'.  If no match is found, it checks for a
`coding:' tag in the first one or two lines following point.  If no
`coding:' tag is found, it checks any local variables list in the last
3K bytes out of the SIZE bytes.  Finally, if none of these methods
succeed, it checks to see if any function in `auto-coding-functions'
gives a match.

If a coding system is specifed, the return value is a
cons (CODING . SOURCE), where CODING is the specified coding
system and SOURCE is a symbol `auto-coding-alist',
`auto-coding-regexp-alist', `coding:', or `auto-coding-functions'
indicating by what CODING is specified.  Note that the validity
of CODING is not checked; it's callers responsibility to check
it.

If nothing is specified, the return value is nil.

The variable `set-auto-coding-function' (which see) is set to this
function by default."
  (or (let ((coding-system (auto-coding-alist-lookup filename)))
	(if coding-system
	    (cons coding-system 'auto-coding-alist)))
      ;; Try using `auto-coding-regexp-alist'.
      (save-excursion
	(let ((alist auto-coding-regexp-alist)
	      coding-system)
	  (while (and alist (not coding-system))
	    (let ((regexp (car (car alist))))
	      (when (re-search-forward regexp (+ (point) size) t)
		(setq coding-system (cdr (car alist)))))
	    (setq alist (cdr alist)))
	  (if coding-system
	      (cons coding-system 'auto-coding-regexp-alist))))
      (let* ((case-fold-search t)
	     (head-start (point))
	     (head-end (+ head-start (min size 1024)))
	     (tail-start (+ head-start (max (- size 3072) 0)))
	     (tail-end (+ head-start size))
	     coding-system head-found tail-found pos)
	;; Try a short cut by searching for the string "coding:"
	;; and for "unibyte:" at the head and tail of SIZE bytes.
	(setq head-found (or (search-forward "coding:" head-end t)
			     (search-forward "unibyte:" head-end t)))
	(if (and head-found (> head-found tail-start))
	    ;; Head and tail are overlapped.
	    (setq tail-found head-found)
	  (goto-char tail-start)
	  (setq tail-found (or (search-forward "coding:" tail-end t)
			       (search-forward "unibyte:" tail-end t))))

	;; At first check the head.
	(when head-found
	  (goto-char head-start)
	  (setq head-end (set-auto-mode-1))
	  (setq head-start (point))
	  (when (and head-end (< head-found head-end))
	    (goto-char head-start)
	    (when (and set-auto-coding-for-load
		       (re-search-forward
			"\\(.*;\\)?[ \t]*unibyte:[ \t]*\\([^ ;]+\\)"
			head-end t))
	      (setq coding-system 'raw-text))
	    (when (and (not coding-system)
		       (re-search-forward
			"\\(.*;\\)?[ \t]*coding:[ \t]*\\([^ ;]+\\)"
			head-end t))
	      (setq coding-system (intern (match-string 2))))))

	;; If no coding: tag in the head, check the tail.
	;; Here we must pay attention to the case that the end-of-line
	;; is just "\r" and we can't use "^" nor "$" in regexp.
	(when (and tail-found (not coding-system))
	  (goto-char tail-start)
	  (re-search-forward "[\r\n]\^L" nil t)
	  (if (re-search-forward
	       "[\r\n]\\([^[\r\n]*\\)[ \t]*Local Variables:[ \t]*\\([^\r\n]*\\)[\r\n]"
	       tail-end t)
	      ;; The prefix is what comes before "local variables:" in its
	      ;; line.  The suffix is what comes after "local variables:"
	      ;; in its line.
	      (let* ((prefix (regexp-quote (match-string 1)))
		     (suffix (regexp-quote (match-string 2)))
		     (re-coding
		      (concat
		       "[\r\n]" prefix
		       ;; N.B. without the \n below, the regexp can
		       ;; eat newlines.
		       "[ \t]*coding[ \t]*:[ \t]*\\([^ \t\r\n]+\\)[ \t]*"
		       suffix "[\r\n]"))
		     (re-unibyte
		      (concat
		       "[\r\n]" prefix
		       "[ \t]*unibyte[ \t]*:[ \t]*\\([^ \t\r\n]+\\)[ \t]*"
		       suffix "[\r\n]"))
		     (re-end
		      (concat "[\r\n]" prefix "[ \t]*End *:[ \t]*" suffix
			      "[\r\n]?"))
		     (pos (1- (point))))
		(forward-char -1)	; skip back \r or \n.
		(re-search-forward re-end tail-end 'move)
		(setq tail-end (point))
		(goto-char pos)
		(when (and set-auto-coding-for-load
			   (re-search-forward re-unibyte tail-end t))
		  (setq coding-system 'raw-text))
		(when (and (not coding-system)
			   (re-search-forward re-coding tail-end t))
		  (setq coding-system (intern (match-string 1)))))))
	(if coding-system
	    (cons coding-system :coding)))
      ;; Finally, try all the `auto-coding-functions'.
      (let ((funcs auto-coding-functions)
	    (coding-system nil))
	(while (and funcs (not coding-system))
	  (setq coding-system (condition-case e
				  (save-excursion
				    (goto-char (point-min))
				    (funcall (pop funcs) size))
				(error nil))))
	(if coding-system
	    (cons coding-system 'auto-coding-functions)))))

(defun set-auto-coding (filename size)
  "Return coding system for a file FILENAME of which SIZE bytes follow point.
See `find-auto-coding' for how the coding system is found.
Return nil if an invalid coding system is found."
  (let ((found (find-auto-coding filename size)))
    (if (and found (coding-system-p (car found)))
	(car found))))

(setq set-auto-coding-function 'set-auto-coding)

;; This variable is set in these two cases:
;;   (1) A file is read by a coding system specified explicitly.
;;       after-insert-file-set-coding sets this value to
;;       coding-system-for-read.
;;   (2) A buffer is saved.
;;       After writing, basic-save-buffer-1 sets this value to
;;       last-coding-system-used.
;; This variable is used for decoding in revert-buffer.
(defvar buffer-file-coding-system-explicit nil
  "The file coding system explicitly specified for the current buffer.
Internal use only.")
(make-variable-buffer-local 'buffer-file-coding-system-explicit)
(put 'buffer-file-coding-system-explicit 'permanent-local t)

(defun after-insert-file-set-coding (inserted &optional visit)
  "Set `buffer-file-coding-system' of current buffer after text is inserted.
INSERTED is the number of characters that were inserted, as figured
in the situation before this function.  Return the number of characters
inserted, as figured in the situation after.  The two numbers can be
different if the buffer has become unibyte.
The optional second arg VISIT non-nil means that we are visiting a file."
  (if (and visit
	   coding-system-for-read
	   (not (eq coding-system-for-read 'auto-save-coding)))
      (setq buffer-file-coding-system-explicit coding-system-for-read))
  (if last-coding-system-used
      (let ((coding-system
	     (find-new-buffer-file-coding-system last-coding-system-used))
	    (modified-p (buffer-modified-p)))
	(when coding-system
	  ;; Tell set-buffer-file-coding-system not to mark the file
	  ;; as modified; we just read it, and it's supposed to be unmodified.
	  ;; Marking it modified would try to lock it, which would
	  ;; check the modtime, and we don't want to do that again now.
	  (set-buffer-file-coding-system coding-system t t)
	  (if (and enable-multibyte-characters
		   (or (eq coding-system 'no-conversion)
		       (eq (coding-system-type coding-system) 5))
		   ;; If buffer was unmodified and the size is the
		   ;; same as INSERTED, we must be visiting it.
		   (not modified-p)
		   (= (buffer-size) inserted))
	      ;; For coding systems no-conversion and raw-text...,
	      ;; edit the buffer as unibyte.
	      (let ((pos-marker (copy-marker (+ (point) inserted)))
		    ;; Prevent locking.
		    (buffer-file-name nil))
		(set-buffer-multibyte nil)
		(setq inserted (- pos-marker (point)))))
	  (set-buffer-modified-p modified-p))))
  inserted)

;; The coding-spec and eol-type of coding-system returned is decided
;; independently in the following order.
;;	1. That of buffer-file-coding-system locally bound.
;;	2. That of CODING.

(defun find-new-buffer-file-coding-system (coding)
  "Return a coding system for a buffer when a file of CODING is inserted.
The local variable `buffer-file-coding-system' of the current buffer
is set to the returned value.
Return nil if there's no need to set `buffer-file-coding-system'."
  (let (local-coding local-eol
	found-coding found-eol
	new-coding new-eol)
    (if (null coding)
	;; Nothing found about coding.
	nil

      ;; Get information of `buffer-file-coding-system' in LOCAL-EOL
      ;; and LOCAL-CODING.
      (setq local-eol (coding-system-eol-type buffer-file-coding-system))
      (if (null (numberp local-eol))
	  ;; But eol-type is not yet set.
	  (setq local-eol nil))
      (if (and buffer-file-coding-system
	       (not (eq (coding-system-type buffer-file-coding-system) t)))
	  ;; This is not `undecided'.
	  (setq local-coding (coding-system-base buffer-file-coding-system)))

      (if (and (local-variable-p 'buffer-file-coding-system)
	       local-eol local-coding)
	  ;; The current buffer has already set full coding-system, we
	  ;; had better not change it.
	  nil

	(setq found-eol (coding-system-eol-type coding))
	(if (null (numberp found-eol))
  	    ;; But eol-type is not found.
	    ;; If EOL conversions are inhibited, force unix eol-type.
	    (setq found-eol (if inhibit-eol-conversion 0)))
	(if (eq (coding-system-type coding) t)
	    (setq found-coding 'undecided)
	  (setq found-coding (coding-system-base coding)))

	(if (and (not found-eol) (eq found-coding 'undecided))
	    ;; No valid coding information found.
	    nil

	  ;; Some coding information (eol or text) found.

	  ;; The local setting takes precedence over the found one.
	  (setq new-coding (if (local-variable-p 'buffer-file-coding-system)
			       (or local-coding found-coding)
			     (or found-coding local-coding)))
	  (setq new-eol (if (local-variable-p 'buffer-file-coding-system)
			    (or local-eol found-eol)
			  (or found-eol local-eol)))

	  (let ((eol-type (coding-system-eol-type new-coding)))
	    (if (and (numberp new-eol) (vectorp eol-type))
		(aref eol-type new-eol)
	      new-coding)))))))

(defun modify-coding-system-alist (target-type regexp coding-system)
  "Modify one of look up tables for finding a coding system on I/O operation.
There are three of such tables, `file-coding-system-alist',
`process-coding-system-alist', and `network-coding-system-alist'.

TARGET-TYPE specifies which of them to modify.
If it is `file', it affects `file-coding-system-alist' (which see).
If it is `process', it affects `process-coding-system-alist' (which see).
If it is `network', it affects `network-coding-system-alist' (which see).

REGEXP is a regular expression matching a target of I/O operation.
The target is a file name if TARGET-TYPE is `file', a program name if
TARGET-TYPE is `process', or a network service name or a port number
to connect to if TARGET-TYPE is `network'.

CODING-SYSTEM is a coding system to perform code conversion on the I/O
operation, or a cons cell (DECODING . ENCODING) specifying the coding systems
for decoding and encoding respectively,
or a function symbol which, when called, returns such a cons cell."
  (or (memq target-type '(file process network))
      (error "Invalid target type: %s" target-type))
  (or (stringp regexp)
      (and (eq target-type 'network) (integerp regexp))
      (error "Invalid regular expression: %s" regexp))
  (if (symbolp coding-system)
      (if (not (fboundp coding-system))
	  (progn
	    (check-coding-system coding-system)
	    (setq coding-system (cons coding-system coding-system))))
    (check-coding-system (car coding-system))
    (check-coding-system (cdr coding-system)))
  (cond ((eq target-type 'file)
	 (let ((slot (assoc regexp file-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq file-coding-system-alist
		   (cons (cons regexp coding-system)
			 file-coding-system-alist)))))
	((eq target-type 'process)
	 (let ((slot (assoc regexp process-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq process-coding-system-alist
		   (cons (cons regexp coding-system)
			 process-coding-system-alist)))))
	(t
	 (let ((slot (assoc regexp network-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq network-coding-system-alist
		   (cons (cons regexp coding-system)
			 network-coding-system-alist)))))))

(defun decode-coding-inserted-region (from to filename
					   &optional visit beg end replace)
  "Decode the region between FROM and TO as if it is read from file FILENAME.
The idea is that the text between FROM and TO was just inserted somehow.
Optional arguments VISIT, BEG, END, and REPLACE are the same as those
of the function `insert-file-contents'.
Part of the job of this function is setting `buffer-undo-list' appropriately."
  (save-excursion
    (save-restriction
      (let ((coding coding-system-for-read)
	    undo-list-saved)
	(if visit
	    ;; Temporarily turn off undo recording, if we're decoding the
	    ;; text of a visited file.
	    (setq buffer-undo-list t)
	  ;; Otherwise, if we can recognize the undo elt for the insertion,
	  ;; remove it and get ready to replace it later.
	  ;; In the mean time, turn off undo recording.
	  (let ((last (car-safe buffer-undo-list)))
	    (if (and (consp last) (eql (car last) from) (eql (cdr last) to))
		(setq undo-list-saved (cdr buffer-undo-list)
		      buffer-undo-list t))))
	(narrow-to-region from to)
	(goto-char (point-min))
	(or coding
	    (setq coding (funcall set-auto-coding-function
				  filename (- (point-max) (point-min)))))
	(or coding
	    (setq coding (car (find-operation-coding-system
			       'insert-file-contents
			       filename visit beg end replace))))
	(if (coding-system-p coding)
	    (or enable-multibyte-characters
		(setq coding
		      (coding-system-change-text-conversion coding 'raw-text)))
	  (setq coding nil))
	(if coding
	    (decode-coding-region (point-min) (point-max) coding)
	  (setq last-coding-system-used coding))
	;; If we're decoding the text of a visited file,
	;; the undo list should start out empty.
	(if visit
	    (setq buffer-undo-list nil)
	  ;; If we decided to replace the undo entry for the insertion,
	  ;; do so now.
	  (if undo-list-saved
	      (setq buffer-undo-list
		    (cons (cons from (point-max)) undo-list-saved))))))))

(defun recode-region (start end new-coding coding)
  "Re-decode the region (previously decoded by CODING) by NEW-CODING."
  (interactive
   (list (region-beginning) (region-end)
	 (read-coding-system "Text was really in: ")
	 (let ((coding (or buffer-file-coding-system last-coding-system-used)))
	   (read-coding-system
	    (concat "But was interpreted as"
		    (if coding (format " (default %S): " coding) ": "))
	    coding))))
  (or (and new-coding coding)
      (error "Coding system not specified"))
  ;; Check it before we encode the region.
  (check-coding-system new-coding)
  (save-restriction
    (narrow-to-region start end)
    (encode-coding-region (point-min) (point-max) coding)
    (decode-coding-region (point-min) (point-max) new-coding)))

(defun make-translation-table (&rest args)
  "Make a translation table from arguments.
A translation table is a char table intended for character
translation in CCL programs.

Each argument is a list of elements of the form (FROM . TO), where FROM
is a character to be translated to TO.

FROM can be a generic character (see `make-char').  In this case, TO is
a generic character containing the same number of characters, or an
ordinary character.  If FROM and TO are both generic characters, all
characters belonging to FROM are translated to characters belonging to TO
without changing their position code(s).

The arguments and forms in each argument are processed in the given
order, and if a previous form already translates TO to some other
character, say TO-ALT, FROM is also translated to TO-ALT."
  (let ((table (make-char-table 'translation-table))
	revlist)
    (while args
      (let ((elts (car args)))
	(while elts
	  (let* ((from (car (car elts)))
		 (from-i 0)		; degree of freedom of FROM
		 (from-rev (nreverse (split-char from)))
		 (to (cdr (car elts)))
		 (to-i 0)		; degree of freedom of TO
		 (to-rev (nreverse (split-char to))))
	    ;; Check numbers of heading 0s in FROM-REV and TO-REV.
	    (while (eq (car from-rev) 0)
	      (setq from-i (1+ from-i) from-rev (cdr from-rev)))
	    (while (eq (car to-rev) 0)
	      (setq to-i (1+ to-i) to-rev (cdr to-rev)))
	    (if (and (/= from-i to-i) (/= to-i 0))
		(error "Invalid character pair (%d . %d)" from to))
	    ;; If we have already translated TO to TO-ALT, FROM should
	    ;; also be translated to TO-ALT.  But, this is only if TO
	    ;; is a generic character or TO-ALT is not a generic
	    ;; character.
	    (let ((to-alt (aref table to)))
	      (if (and to-alt
		       (or (> to-i 0) (not (generic-char-p to-alt))))
		  (setq to to-alt)))
	    (if (> from-i 0)
		(set-char-table-default table from to)
	      (aset table from to))
	    ;; If we have already translated some chars to FROM, they
	    ;; should also be translated to TO.
	    (let ((l (assq from revlist)))
	      (if l
		  (let ((ch (car l)))
		    (setcar l to)
		    (setq l (cdr l))
		    (while l
		      (aset table ch to)
		      (setq l (cdr l)) ))))
	    ;; Now update REVLIST.
	    (let ((l (assq to revlist)))
	      (if l
		  (setcdr l (cons from (cdr l)))
		(setq revlist (cons (list to from) revlist)))))
	  (setq elts (cdr elts))))
      (setq args (cdr args)))
    ;; Return TABLE just created.
    table))

(defun make-translation-table-from-vector (vec)
  "Make translation table from decoding vector VEC.
VEC is an array of 256 elements to map unibyte codes to multibyte
characters.  Elements may be nil for undefined code points.
See also the variable `nonascii-translation-table'."
  (let ((table (make-char-table 'translation-table))
	(rev-table (make-char-table 'translation-table))
	ch)
    (dotimes (i 256)
      (setq ch (aref vec i))
      (when ch
	(aset table i ch)
	(if (>= ch 256)
	    (aset rev-table ch i))))
    (set-char-table-extra-slot table 0 rev-table)
    table))

(defun define-translation-table (symbol &rest args)
  "Define SYMBOL as the name of translation table made by ARGS.
This sets up information so that the table can be used for
translations in a CCL program.

If the first element of ARGS is a char-table whose purpose is
`translation-table', just define SYMBOL to name it.  (Note that this
function does not bind SYMBOL.)

Any other ARGS should be suitable as arguments of the function
`make-translation-table' (which see).

This function sets properties `translation-table' and
`translation-table-id' of SYMBOL to the created table itself and the
identification number of the table respectively.  It also registers
the table in `translation-table-vector'."
  (let ((table (if (and (char-table-p (car args))
			(eq (char-table-subtype (car args))
			    'translation-table))
		   (car args)
		 (apply 'make-translation-table args)))
	(len (length translation-table-vector))
	(id 0)
	(done nil))
    (put symbol 'translation-table table)
    (while (not done)
      (if (>= id len)
	  (setq translation-table-vector
		(vconcat translation-table-vector (make-vector len nil))))
      (let ((slot (aref translation-table-vector id)))
	(if (or (not slot)
		(eq (car slot) symbol))
	    (progn
	      (aset translation-table-vector id (cons symbol table))
	      (setq done t))
	  (setq id (1+ id)))))
    (put symbol 'translation-table-id id)
    id))

(defun translate-region (start end table)
  "From START to END, translate characters according to TABLE.
TABLE is a string or a char-table.
If TABLE is a string, the Nth character in it is the mapping
for the character with code N.
If TABLE is a char-table, the element for character N is the mapping
for the character with code N.
It returns the number of characters changed."
  (interactive
   (list (region-beginning)
	 (region-end)
	 (let (table l)
	   (dotimes (i (length translation-table-vector))
	     (if (consp (aref translation-table-vector i))
		 (push (list (symbol-name
			      (car (aref translation-table-vector i)))) l)))
	   (if (not l)
	       (error "No translation table defined"))
	   (while (not table)
	     (setq table (completing-read "Translation table: " l nil t)))
	   (intern table))))
  (if (symbolp table)
      (let ((val (get table 'translation-table)))
	(or (char-table-p val)
	    (error "Invalid translation table name: %s" table))
	(setq table val)))
  (translate-region-internal start end table))

(put 'with-category-table 'lisp-indent-function 1)

(defmacro with-category-table (table &rest body)
  "Evaluate BODY with category table of current buffer set to TABLE.
The category table of the current buffer is saved, BODY is evaluated,
then the saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (let ((old-table (make-symbol "old-table"))
	(old-buffer (make-symbol "old-buffer")))
    `(let ((,old-table (category-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-category-table ,table)
	     ,@body)
	 (save-current-buffer
	   (set-buffer ,old-buffer)
	   (set-category-table ,old-table))))))

(defun define-translation-hash-table (symbol table)
  "Define SYMBOL as the name of the hash translation TABLE for use in CCL.

Analogous to `define-translation-table', but updates
`translation-hash-table-vector' and the table is for use in the CCL
`lookup-integer' and `lookup-character' functions."
  (unless (and (symbolp symbol)
	       (hash-table-p table))
    (error "Bad args to define-translation-hash-table"))
  (let ((len (length translation-hash-table-vector))
	(id 0)
	done)
    (put symbol 'translation-hash-table table)
    (while (not done)
      (if (>= id len)
	  (setq translation-hash-table-vector
		(vconcat translation-hash-table-vector [nil])))
      (let ((slot (aref translation-hash-table-vector id)))
	(if (or (not slot)
		(eq (car slot) symbol))
	    (progn
	      (aset translation-hash-table-vector id (cons symbol table))
	      (setq done t))
	  (setq id (1+ id)))))
    (put symbol 'translation-hash-table-id id)
    id))

;;; Initialize some variables.

(put 'use-default-ascent 'char-table-extra-slots 0)
(setq use-default-ascent (make-char-table 'use-default-ascent))
(put 'ignore-relative-composition 'char-table-extra-slots 0)
(setq ignore-relative-composition
      (make-char-table 'ignore-relative-composition))


;;; Built-in auto-coding-functions:

(defun sgml-xml-auto-coding-function (size)
  "Determine whether the buffer is XML, and if so, its encoding.
This function is intended to be added to `auto-coding-functions'."
  (setq size (+ (point) size))
  (when (re-search-forward "\\`[[:space:]\n]*<\\?xml" size t)
    (let ((end (save-excursion
		 ;; This is a hack.
		 (re-search-forward "[\"']\\s-*\\?>" size t))))
      (when end
	(if (re-search-forward "encoding=[\"']\\(.+?\\)[\"']" end t)
	    (let* ((match (match-string 1))
		   (sym (intern (downcase match))))
	      (if (coding-system-p sym)
		  sym
		(message "Warning: unknown coding system \"%s\"" match)
		nil))
	  'utf-8)))))

(defun sgml-html-meta-auto-coding-function (size)
  "If the buffer has an HTML meta tag, use it to determine encoding.
This function is intended to be added to `auto-coding-functions'."
  (setq size (min (+ (point) size)
		  ;; Only search forward 10 lines
		  (save-excursion
		    (forward-line 10)
		    (point))))
  (when (and (search-forward "<html" size t)
	     (re-search-forward "<meta\\s-+http-equiv=\"content-type\"\\s-+content=\"text/\\sw+;\\s-*charset=\\(.+?\\)\"" size t))
      (let* ((match (match-string 1))
	     (sym (intern (downcase match))))
	(if (coding-system-p sym)
	    sym
	  (message "Warning: unknown coding system \"%s\"" match)
	  nil))))

;;;
(provide 'mule)

;;; arch-tag: 9aebaa6e-0e8a-40a9-b857-cb5d04a39e7c
;;; mule.el ends here
