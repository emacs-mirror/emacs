;;; mule-diag.el --- show diagnosis of multilingual environment (Mule)

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.

;; Keywords: multilingual, charset, coding system, fontset, diagnosis, i18n

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

;; Make sure the help-xref button type is defined.
(require 'help-fns)

;;; General utility function

;; Print all arguments with single space separator in one line.
(defun print-list (&rest args)
  (while (cdr args)
    (when (car args)
      (princ (car args))
      (princ " "))
    (setq args (cdr args)))
  (princ (car args))
  (princ "\n"))

;; Re-order the elements of charset-list.
(defun sort-charset-list ()
  (setq charset-list
	(sort charset-list
	      (function (lambda (x y) (< (charset-id x) (charset-id y)))))))

;;; CHARSET

(define-button-type 'sort-listed-character-sets
  'help-echo (purecopy "mouse-2, RET: sort on this column")
  'face 'bold
  'action #'(lambda (button)
	      (sort-listed-character-sets (button-get button 'sort-key))))

(define-button-type 'list-charset-chars
  :supertype 'help-xref
  'help-function #'list-charset-chars
  'help-echo "mouse-2, RET: show table of characters for this character set")


;;;###autoload
(defun list-character-sets (arg)
  "Display a list of all character sets.

The ID-NUM column contains a charset identification number for
internal Emacs use.

The MULTIBYTE-FORM column contains the format of the buffer and string
multibyte sequence of characters in the charset using one to four
hexadecimal digits.
  `xx' stands for any byte in the range 0..127.
  `XX' stands for any byte in the range 160..255.

The D column contains the dimension of this character set.  The CH
column contains the number of characters in a block of this character
set.  The FINAL-CHAR column contains an ISO-2022 <final-char> to use
for designating this character set in ISO-2022-based coding systems.

With prefix arg, the output format gets more cryptic,
but still shows the full information."
  (interactive "P")
  (help-setup-xref (list #'list-character-sets arg) (interactive-p))
  (with-output-to-temp-buffer (help-buffer)
    (with-current-buffer standard-output
      (if arg
	  (list-character-sets-2)
	;; Insert header.
	(insert
	 (substitute-command-keys
	  (concat "Use "
		  (if (display-mouse-p) "\\[help-follow-mouse] or ")
		  "\\[help-follow]:\n")))
	(insert "  on a column title to sort by that title,")
	(indent-to 56)
	(insert "+----DIMENSION\n")
	(insert "  on a charset name to list characters.")
	(indent-to 56)
	(insert "| +--CHARS\n")
	(let ((columns '(("ID-NUM" . id) "\t"
			 ("CHARSET-NAME" . name) "\t\t\t"
			 ("MULTIBYTE-FORM" . id) "\t"
			 ("D CH FINAL-CHAR" . iso-spec)))
	      pos)
	  (while columns
	    (if (stringp (car columns))
		(insert (car columns))
	      (insert-text-button (car (car columns))
				  :type 'sort-listed-character-sets
				  'sort-key (cdr (car columns)))
	      (goto-char (point-max)))
	    (setq columns (cdr columns)))
	  (insert "\n"))
	(insert "------\t------------\t\t\t--------------\t- -- ----------\n")

	;; Insert body sorted by charset IDs.
	(list-character-sets-1 'id)))))

(defun sort-listed-character-sets (sort-key)
  (if sort-key
      (save-excursion
	(help-setup-xref (list #'list-character-sets nil) t)
	(let ((buffer-read-only nil))
	  (goto-char (point-min))
	  (re-search-forward "[0-9][0-9][0-9]")
	  (beginning-of-line)
	  (delete-region (point) (point-max))
	  (list-character-sets-1 sort-key)))))

(defun charset-multibyte-form-string (charset)
  (let ((info (charset-info charset)))
    (cond ((eq charset 'ascii)
	   "xx")
	  ((eq charset 'eight-bit-control)
	   (format "%2X Xx" (aref info 6)))
	  ((eq charset 'eight-bit-graphic)
	   "XX")
	  (t
	   (let ((str (format "%2X" (aref info 6))))
	     (if (> (aref info 7) 0)
		 (setq str (format "%s %2X"
				   str (aref info 7))))
	     (setq str (concat str " XX"))
	     (if (> (aref info 2) 1)
		 (setq str (concat str " XX")))
	     str)))))

;; Insert a list of character sets sorted by SORT-KEY.  SORT-KEY
;; should be one of `id', `name', and `iso-spec'.  If SORT-KEY is nil,
;; it defaults to `id'.

(defun list-character-sets-1 (sort-key)
  (or sort-key
      (setq sort-key 'id))
  (let ((tail (charset-list))
	charset-info-list elt charset info sort-func)
    (while tail
      (setq charset (car tail) tail (cdr tail)
	    info (charset-info charset))

      ;; Generate a list that contains all information to display.
      (setq charset-info-list
	    (cons (list (charset-id charset)	; ID-NUM
			charset			; CHARSET-NAME
			(charset-multibyte-form-string charset); MULTIBYTE-FORM
			(aref info 2)		; DIMENSION
			(aref info 3)		; CHARS
			(aref info 8)		; FINAL-CHAR
			)
		  charset-info-list)))

    ;; Determine a predicate for `sort' by SORT-KEY.
    (setq sort-func
	  (cond ((eq sort-key 'id)
		 (function (lambda (x y) (< (car x) (car y)))))

		((eq sort-key 'name)
		 (function (lambda (x y) (string< (nth 1 x) (nth 1 y)))))

		((eq sort-key 'iso-spec)
		 ;; Sort by DIMENSION CHARS FINAL-CHAR
		 (function
		  (lambda (x y)
		    (or (< (nth 3 x) (nth 3 y))
			(and (= (nth 3 x) (nth 3 y))
			     (or (< (nth 4 x) (nth 4 y))
				 (and (= (nth 4 x) (nth 4 y))
				      (< (nth 5 x) (nth 5 y)))))))))
		(t
		 (error "Invalid charset sort key: %s" sort-key))))

    (setq charset-info-list (sort charset-info-list sort-func))

    ;; Insert information of character sets.
    (while charset-info-list
      (setq elt (car charset-info-list)
	    charset-info-list (cdr charset-info-list))
      (insert (format "%03d(%02X)" (car elt) (car elt))) ; ID-NUM
      (indent-to 8)
      (insert-text-button (symbol-name (nth 1 elt))
			  :type 'list-charset-chars
			  'help-args (list (nth 1 elt)))
      (goto-char (point-max))
      (insert "\t")
      (indent-to 40)
      (insert (nth 2 elt))		; MULTIBYTE-FORM
      (indent-to 56)
      (insert (format "%d %2d " (nth 3 elt) (nth 4 elt)) ; DIMENSION and CHARS
	      (if (< (nth 5 elt) 0) "none" (nth 5 elt))) ; FINAL-CHAR
      (insert "\n"))))


;; List all character sets in a form that a program can easily parse.

(defun list-character-sets-2 ()
  (insert "#########################
## LIST OF CHARSETS
## Each line corresponds to one charset.
## The following attributes are listed in this order
## separated by a colon `:' in one line.
##	CHARSET-ID,
##	CHARSET-SYMBOL-NAME,
##	DIMENSION (1 or 2)
##	CHARS (94 or 96)
##	BYTES (of multibyte form: 1, 2, 3, or 4),
##	WIDTH (occupied column numbers: 1 or 2),
##	DIRECTION (0:left-to-right, 1:right-to-left),
##	ISO-FINAL-CHAR (character code of ISO-2022's final character)
##	ISO-GRAPHIC-PLANE (ISO-2022's graphic plane, 0:GL, 1:GR)
##	DESCRIPTION (describing string of the charset)
")
  (let ((l charset-list)
	charset)
    (while l
      (setq charset (car l) l (cdr l))
      (princ (format "%03d:%s:%d:%d:%d:%d:%d:%d:%d:%s\n"
		     (charset-id charset)
		     charset
		     (charset-dimension charset)
		     (charset-chars charset)
		     (charset-bytes charset)
		     (charset-width charset)
		     (charset-direction charset)
		     (charset-iso-final-char charset)
		     (charset-iso-graphic-plane charset)
		     (charset-description charset))))))

(defvar non-iso-charset-alist
  `((mac-roman
     nil
     mac-roman-decoder
     ((0 255)))
    (viscii
     (ascii vietnamese-viscii-lower vietnamese-viscii-upper)
     viet-viscii-nonascii-translation-table
     ((0 255)))
    (koi8-r
     (ascii cyrillic-iso8859-5)
     cyrillic-koi8-r-nonascii-translation-table
     ((32 255)))
    (alternativnyj
     (ascii cyrillic-iso8859-5)
     cyrillic-alternativnyj-nonascii-translation-table
     ((32 255)))
    (big5
     (ascii chinese-big5-1 chinese-big5-2)
     decode-big5-char
     ((32 127)
      ((?\xA1 ?\xFE) . (?\x40 ?\x7E ?\xA1 ?\xFE))))
    (sjis
     (ascii katakana-jisx0201 japanese-jisx0208)
     decode-sjis-char
     ((32 127 ?\xA1 ?\xDF)
      ((?\x81 ?\x9F ?\xE0 ?\xEF) . (?\x40 ?\x7E ?\x80 ?\xFC)))))
  "Alist of charset names vs the corresponding information.
This is mis-named for historical reasons.  The charsets are actually
non-built-in ones.  They correspond to Emacs coding systems, not Emacs
charsets, i.e. what Emacs can read (or write) by mapping to (or
from) Emacs internal charsets that typically correspond to a limited
set of ISO charsets.

Each element has the following format:
  (CHARSET CHARSET-LIST TRANSLATION-METHOD [ CODE-RANGE ])

CHARSET is the name (symbol) of the charset.

CHARSET-LIST is a list of Emacs charsets into which characters of
CHARSET are mapped.

TRANSLATION-METHOD is a translation table (symbol) to translate a
character code of CHARSET to the corresponding Emacs character
code.  It can also be a function to call with one argument, a
character code in CHARSET.

CODE-RANGE specifies the valid code ranges of CHARSET.
It is a list of RANGEs, where each RANGE is of the form:
  (FROM1 TO1 FROM2 TO2 ...)
or
  ((FROM1-1 TO1-1 FROM1-2 TO1-2 ...) . (FROM2-1 TO2-1 FROM2-2 TO2-2 ...))
In the first form, valid codes are between FROM1 and TO1, or FROM2 and
TO2, or...
The second form is used for 2-byte codes.  The car part is the ranges
of the first byte, and the cdr part is the ranges of the second byte.")


(defun decode-codepage-char (codepage code)
  "Decode a character that has code CODE in CODEPAGE.
Return a decoded character string.  Each CODEPAGE corresponds to a
coding system cpCODEPAGE."
  (let ((coding-system (intern (format "cp%d" codepage))))
    (or (coding-system-p coding-system)
	(codepage-setup codepage))
    (string-to-char
     (decode-coding-string (char-to-string code) coding-system))))


;; Add DOS codepages to `non-iso-charset-alist'.

(let ((tail (cp-supported-codepages))
      elt)
  (while tail
    (setq elt (car tail) tail (cdr tail))
    ;; Now ELT is (CODEPAGE . CHARSET), where CODEPAGE is a string
    ;; (e.g. "850"), CHARSET is a charset that characters in CODEPAGE
    ;; are mapped to.
    (unless (assq (intern (concat "cp" (car elt))) non-iso-charset-alist)
      (setq non-iso-charset-alist
	    (cons (list (intern (concat "cp" (car elt)))
			(list 'ascii (cdr elt))
			`(lambda (code)
			   (decode-codepage-char ,(string-to-int (car elt))
						 code))
			(list (list 0 255)))
		  non-iso-charset-alist)))))


;; A variable to hold charset input history.
(defvar charset-history nil)


;;;###autoload
(defun read-charset (prompt &optional default-value initial-input)
  "Read a character set from the minibuffer, prompting with string PROMPT.
It must be an Emacs character set listed in the variable `charset-list'
or a non-ISO character set listed in the variable
`non-iso-charset-alist'.

Optional arguments are DEFAULT-VALUE and INITIAL-INPUT.
DEFAULT-VALUE, if non-nil, is the default value.
INITIAL-INPUT, if non-nil, is a string inserted in the minibuffer initially.
See the documentation of the function `completing-read' for the
detailed meanings of these arguments."
  (let* ((table (append (mapcar (function (lambda (x) (list (symbol-name x))))
				charset-list)
			(mapcar (function (lambda (x)
					    (list (symbol-name (car x)))))
				non-iso-charset-alist)))
	 (charset (completing-read prompt table
				   nil t initial-input 'charset-history
				   default-value)))
    (if (> (length charset) 0)
	(intern charset))))


;; List characters of the range MIN and MAX of CHARSET.  If dimension
;; of CHARSET is two (i.e. 2-byte charset), ROW is the first byte
;; (block index) of the characters, and MIN and MAX are the second
;; bytes of the characters.  If the dimension is one, ROW should be 0.
;; For a non-ISO charset, CHARSET is a translation table (symbol) or a
;; function to get Emacs' character codes that corresponds to the
;; characters to list.

(defun list-block-of-chars (charset row min max)
  (let (i ch)
    (insert-char ?- (+ 4 (* 3 16)))
    (insert "\n    ")
    (setq i 0)
    (while (< i 16)
      (insert (format "%3X" i))
      (setq i (1+ i)))
    (setq i (* (/ min 16) 16))
    (while (<= i max)
      (if (= (% i 16) 0)
	  (insert (format "\n%3Xx" (/ (+ (* row 256) i) 16))))
      (setq ch (cond ((< i min)
		      32)
		     ((charsetp charset)
		      (if (= row 0)
			  (make-char charset i)
			(make-char charset row i)))
		     ((and (symbolp charset) (get charset 'translation-table))
		      (aref (get charset 'translation-table) i))
		     (t (funcall charset (+ (* row 256) i)))))
      (if (and (char-table-p charset)
	       (or (< ch 32) (and (>= ch 127) (<= ch 255))))
	  ;; Don't insert a control code.
	  (setq ch 32))
      (unless ch (setq ch 32))
      (if (eq ch ?\t)
	  ;; Make it visible.
	  (setq ch (propertize "\t" 'display "^I")))
      ;; This doesn't DTRT.  Maybe it's better to insert "^J" and not
      ;; worry about the buffer contents not being correct.
;;;       (if (eq ch ?\n)
;;; 	(setq ch (propertize "\n" 'display "^J")))
      (indent-to (+ (* (% i 16) 3) 6))
      (insert ch)
      (setq i (1+ i))))
  (insert "\n"))

(defun list-iso-charset-chars (charset)
  (let ((dim (charset-dimension charset))
	(chars (charset-chars charset))
	(plane (charset-iso-graphic-plane charset))
	min max)
    (insert (format "Characters in the coded character set %s.\n" charset))

    (cond ((eq charset 'eight-bit-control)
	   (setq min 128 max 159))
	  ((eq charset 'eight-bit-graphic)
	   (setq min 160 max 255))
	  (t
	   (if (= chars 94)
	       (setq min 33 max 126)
	     (setq min 32 max 127))
	   (or (= plane 0)
	       (setq min (+ min 128) max (+ max 128)))))

    (if (= dim 1)
	(list-block-of-chars charset 0 min max)
      (let ((i min))
	(while (<= i max)
	  (list-block-of-chars charset i min max)
	  (setq i (1+ i)))))))

(defun list-non-iso-charset-chars (charset)
  "List all characters in non-built-in coded character set CHARSET."
  (let* ((slot (assq charset non-iso-charset-alist))
	 (charsets (nth 1 slot))
	 (translate-method (nth 2 slot))
	 (ranges (nth 3 slot))
	 range)
    (or slot
	(error "Unknown character set: %s" charset))
    (insert (format "Characters in the coded character set %s.\n" charset))
    (if charsets
	(insert "They are mapped to: "
		(mapconcat #'symbol-name charsets ", ")
		"\n"))
    (while ranges
      (setq range (pop ranges))
      (if (integerp (car range))
	  ;; The form of RANGES is (FROM1 TO1 FROM2 TO2 ...).
	  (if (and (not (functionp translate-method))
		   (< (car (last range)) 256))
	      ;; Do it all in one block to avoid the listing being
	      ;; broken up at gaps in the range.  Don't do that for
	      ;; function translate-method, since not all codes in
	      ;; that range may be valid.
	      (list-block-of-chars translate-method
				   0 (car range) (car (last range)))
	    (while range
	      (list-block-of-chars translate-method
				   0 (car range) (nth 1 range))
	      (setq range (nthcdr 2 range))))
	;; The form of RANGES is ((FROM1-1 TO1-1 ...) . (FROM2-1 TO2-1 ...)).
	(let ((row-range (car range))
	      row row-max
	      col-range col col-max)
	  (while row-range
	    (setq row (car row-range) row-max (nth 1 row-range)
		  row-range (nthcdr 2 row-range))
	    (while (<= row row-max)
	      (setq col-range (cdr range))
	      (while col-range
		(setq col (car col-range) col-max (nth 1 col-range)
		      col-range (nthcdr 2 col-range))
		(list-block-of-chars translate-method row col col-max))
	      (setq row (1+ row)))))))))


;;;###autoload
(defun list-charset-chars (charset)
  "Display a list of characters in the specified character set.
This can list both Emacs `official' (ISO standard) charsets and the
characters encoded by various Emacs coding systems which correspond to
PC `codepages' and other coded character sets.  See `non-iso-charset-alist'."
  (interactive (list (read-charset "Character set: ")))
  (with-output-to-temp-buffer "*Help*"
    (with-current-buffer standard-output
      (setq indent-tabs-mode nil)
      (set-buffer-multibyte t)
      (cond ((charsetp charset)
	     (list-iso-charset-chars charset))
	    ((assq charset non-iso-charset-alist)
	     (list-non-iso-charset-chars charset))
	    (t
	     (error "Invalid character set %s" charset))))))


;;;###autoload
(defun describe-character-set (charset)
  "Display information about built-in character set CHARSET."
  (interactive (list (let ((non-iso-charset-alist nil))
		       (read-charset "Charset: "))))
  (or (charsetp charset)
      (error "Invalid charset: %S" charset))
  (let ((info (charset-info charset)))
    (help-setup-xref (list #'describe-character-set charset) (interactive-p))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer standard-output
	(insert "Character set: " (symbol-name charset)
		(format " (ID:%d)\n\n" (aref info 0)))
	(insert (aref info 13) "\n\n")	; description
	(insert "Number of contained characters: "
		(if (= (aref info 2) 1)
		    (format "%d\n" (aref info 3))
		  (format "%dx%d\n" (aref info 3) (aref info 3))))
	(insert "Final char of ISO2022 designation sequence: ")
	(if (>= (aref info 8) 0)
	    (insert (format "`%c'\n" (aref info 8)))
	  (insert "not assigned\n"))
	(insert (format "Width (how many columns on screen): %d\n"
			(aref info 4)))
	(insert (format "Internal multibyte sequence: %s\n"
			(charset-multibyte-form-string charset)))
	(let ((coding (plist-get (aref info 14) 'preferred-coding-system)))
	  (when coding
	    (insert (format "Preferred coding system: %s\n" coding))
	    (search-backward (symbol-name coding))
	    (help-xref-button 0 'help-coding-system coding)))))))

;;;###autoload
(defun describe-char-after (&optional pos)
  "Display information about the character at POS in the current buffer.
POS defaults to point.
The information includes character code, charset and code points in it,
syntax, category, how the character is encoded in a file,
which font is being used for displaying the character."
  (interactive)
  (or pos
      (setq pos (point)))
  (if (>= pos (point-max))
      (error "No character at point"))
  (let* ((char (char-after pos))
	 (charset (char-charset char))
	 (props (text-properties-at pos))
	 (composition (find-composition (point) nil nil t))
	 (composed (if composition (buffer-substring (car composition)
						     (nth 1 composition))))
	 (multibyte-p enable-multibyte-characters)
	 item-list max-width)
    (if (eq charset 'unknown)
	(setq item-list
	      `(("character"
		 ,(format "%s (0%o, %d, 0x%x) -- invalid character code"
			  (if (< char 256)
			      (single-key-description char)
			    (char-to-string char))
			  char char char))))
      (setq item-list
	    `(("character"
	       ,(format "%s (0%o, %d, 0x%x)" (if (< char 256)
						 (single-key-description char)
					       (char-to-string char))
			char char char))
	      ("charset"
	       ,(symbol-name charset)
	       ,(format "(%s)" (charset-description charset)))
	      ("code point"
	       ,(let ((split (split-char char)))
		  (if (= (charset-dimension charset) 1)
		      (format "%d" (nth 1 split))
		    (format "%d %d" (nth 1 split) (nth 2 split)))))
	      ("syntax"
 	       ,(let ((syntax (get-char-property (point) 'syntax-table)))
		  (with-temp-buffer
		    (internal-describe-syntax-value
		     (if (consp syntax) syntax
		       (aref (or syntax (syntax-table)) char)))
		    (buffer-string))))
	      ("category"
	       ,@(let ((category-set (char-category-set char)))
		   (if (not category-set)
		       '("-- none --")
		     (mapcar #'(lambda (x) (format "%c:%s  "
						   x (category-docstring x)))
			     (category-set-mnemonics category-set)))))
	      ,@(let ((props (aref char-code-property-table char))
		      ps)
		  (when props
		    (while props
		      (push (format "%s:" (pop props)) ps)
		      (push (format "%s;" (pop props)) ps))
		    (list (cons "Properties" (nreverse ps)))))
	      ("buffer code"
	       ,(encoded-string-description
		 (string-as-unibyte (char-to-string char)) nil))
	      ("file code"
	       ,@(let* ((coding buffer-file-coding-system)
			(encoded (encode-coding-char char coding)))
		   (if encoded
		       (list (encoded-string-description encoded coding)
			     (format "(encoded by coding system %S)" coding))
		     (list "not encodable by coding system"
			   (symbol-name coding)))))
	      ,@(if (or (memq 'mule-utf-8
			  (find-coding-systems-region (point) (1+ (point))))
			(get-char-property (point) 'untranslated-utf-8))
		    (let ((uc (or (get-char-property (point)
						     'untranslated-utf-8)
				  (encode-char (char-after) 'ucs))))
		      (if uc
			  (list (list "Unicode"
				      (format "%04X" uc))))))
	      ,(if (display-graphic-p (selected-frame))
		   (list "font" (or (internal-char-font (point))
				    "-- none --"))
		 (list "terminal code"
		       (let* ((coding (terminal-coding-system))
			      (encoded (encode-coding-char char coding)))
			 (if encoded
			     (encoded-string-description encoded coding)
			   "not encodable")))))))
    (setq max-width (apply #'max (mapcar #'(lambda (x) (length (car x)))
					 item-list)))
    (with-output-to-temp-buffer "*Help*"
      (save-excursion
	(set-buffer standard-output)
	(set-buffer-multibyte multibyte-p)
	(let ((formatter (format "%%%ds:" max-width)))
	  (dolist (elt item-list)
	    (insert (format formatter (car elt)))
	    (dolist (clm (cdr elt))
	      (when (>= (+ (current-column)
			   (or (string-match "\n" clm)
			       (string-width clm)) 1)
			(frame-width))
		(insert "\n")
		(indent-to (1+ max-width)))
	      (insert " " clm))
	    (insert "\n")))
	(when composition
	  (insert "\nComposed with the following character(s) "
		  (mapconcat (lambda (x) (format "`%c'" x))
			     (substring composed 1)
			     ", ")
		  " to form `" composed "'")
	  (if (nth 3 composition)
	      (insert ".\n")
	    (insert "\nby the rule ("
		    (mapconcat (lambda (x)
				 (format (if (consp x) "%S" "?%c") x))
			       (nth 2 composition)
			       " ")
		    ").\n"
		    "See the variable `reference-point-alist' for "
		    "the meaning of the rule.\n")))
	(if props
	    (insert "\nText properties\n"))
	(while props
	  (insert (format "  %s: %s" (car props) (cadr props)))
	  (setq props (cddr props)))
	))))


;;; CODING-SYSTEM

;; Print information of designation of each graphic register in FLAGS
;; in human readable format.  See the documentation of
;; `make-coding-system' for the meaning of FLAGS.
(defun print-designation (flags)
  (let ((graphic-register 0)
	charset)
    (while (< graphic-register 4)
      (setq charset (aref flags graphic-register))
      (princ (format
	      "  G%d -- %s\n"
	      graphic-register
	      (cond ((null charset)
		     "never used")
		    ((eq charset t)
		     "no initial designation, and used by any charsets")
		    ((symbolp charset)
		     (format "%s:%s"
			     charset (charset-description charset)))
		    ((listp charset)
		     (if (charsetp (car charset))
			 (format "%s:%s, and also used by the followings:"
				 (car charset)
				 (charset-description (car charset)))
		       "no initial designation, and used by the followings:"))
		    (t
		     "invalid designation information"))))
      (when (listp charset)
	(setq charset (cdr charset))
	(while charset
	  (cond ((eq (car charset) t)
		 (princ "\tany other charsets\n"))
		((charsetp (car charset))
		 (princ (format "\t%s:%s\n"
				(car charset)
				(charset-description (car charset)))))
		(t
		 "invalid designation information"))
	  (setq charset (cdr charset))))
      (setq graphic-register (1+ graphic-register)))))

;;;###autoload
(defun describe-coding-system (coding-system)
  "Display information about CODING-SYSTEM."
  (interactive "zDescribe coding system (default, current choices): ")
  (if (null coding-system)
      (describe-current-coding-system)
    (help-setup-xref (list #'describe-coding-system coding-system)
		     (interactive-p))
    (with-output-to-temp-buffer (help-buffer)
      (print-coding-system-briefly coding-system 'doc-string)
      (princ "\n")
      (let ((coding-spec (coding-system-spec coding-system)))
	(princ "Type: ")
	(let ((type (coding-system-type coding-system))
	      (flags (coding-system-flags coding-system)))
	  (princ type)
	  (cond ((eq type nil)
		 (princ " (do no conversion)"))
		((eq type t)
		 (princ " (do automatic conversion)"))
		((eq type 0)
		 (princ " (Emacs internal multibyte form)"))
		((eq type 1)
		 (princ " (Shift-JIS, MS-KANJI)"))
		((eq type 2)
		 (princ " (variant of ISO-2022)\n")
		 (princ "Initial designations:\n")
		 (print-designation flags)
		 (princ "Other Form: \n  ")
		 (princ (if (aref flags 4) "short-form" "long-form"))
		 (if (aref flags 5) (princ ", ASCII@EOL"))
		 (if (aref flags 6) (princ ", ASCII@CNTL"))
		 (princ (if (aref flags 7) ", 7-bit" ", 8-bit"))
		 (if (aref flags 8) (princ ", use-locking-shift"))
		 (if (aref flags 9) (princ ", use-single-shift"))
		 (if (aref flags 10) (princ ", use-roman"))
		 (if (aref flags 11) (princ ", use-old-jis"))
		 (if (aref flags 12) (princ ", no-ISO6429"))
		 (if (aref flags 13) (princ ", init-bol"))
		 (if (aref flags 14) (princ ", designation-bol"))
		 (if (aref flags 15) (princ ", convert-unsafe"))
		 (if (aref flags 16) (princ ", accept-latin-extra-code"))
		 (princ "."))
		((eq type 3)
		 (princ " (Big5)"))
		((eq type 4)
		 (princ " (do conversion by CCL program)"))
		((eq type 5)
		 (princ " (text with random binary characters)"))
		(t (princ ": invalid coding-system."))))
	(princ "\nEOL type: ")
	(let ((eol-type (coding-system-eol-type coding-system)))
	  (cond ((vectorp eol-type)
		 (princ "Automatic selection from:\n\t")
		 (princ eol-type)
		 (princ "\n"))
		((or (null eol-type) (eq eol-type 0)) (princ "LF\n"))
		((eq eol-type 1) (princ "CRLF\n"))
		((eq eol-type 2) (princ "CR\n"))
		(t (princ "invalid\n")))))
      (let ((postread (coding-system-get coding-system 'post-read-conversion)))
	(when postread
	  (princ "After decoding text normally,")
	  (princ " perform post-conversion using the function: ")
	  (princ "\n  ")
	  (princ postread)
	  (princ "\n")))
      (let ((prewrite (coding-system-get coding-system 'pre-write-conversion)))
	(when prewrite
	  (princ "Before encoding text normally,")
	  (princ " perform pre-conversion using the function: ")
	  (princ "\n  ")
	  (princ prewrite)
	  (princ "\n")))
      (with-current-buffer standard-output
	(let ((charsets (coding-system-get coding-system 'safe-charsets)))
	  (when (and (not (memq (coding-system-base coding-system)
				'(raw-text emacs-mule)))
		     charsets)
	    (if (eq charsets t)
		(insert "This coding system can encode all charsets except for
eight-bit-control and eight-bit-graphic.\n")
	      (insert "This coding system encodes the following charsets:\n ")
	      (while charsets
		(insert " " (symbol-name (car charsets)))
		(search-backward (symbol-name (car charsets)))
		(help-xref-button 0 'help-character-set (car charsets))
		(goto-char (point-max))
		(setq charsets (cdr charsets))))))))))


;;;###autoload
(defun describe-current-coding-system-briefly ()
  "Display coding systems currently used in a brief format in echo area.

The format is \"F[..],K[..],T[..],P>[..],P<[..], default F[..],P<[..],P<[..]\",
where mnemonics of the following coding systems come in this order
in place of `..':
  `buffer-file-coding-system' (of the current buffer)
  eol-type of `buffer-file-coding-system' (of the current buffer)
  Value returned by `keyboard-coding-system'
  eol-type of `keyboard-coding-system'
  Value returned by `terminal-coding-system'.
  eol-type of `terminal-coding-system'
  `process-coding-system' for read (of the current buffer, if any)
  eol-type of `process-coding-system' for read (of the current buffer, if any)
  `process-coding-system' for write (of the current buffer, if any)
  eol-type of `process-coding-system' for write (of the current buffer, if any)
  `default-buffer-file-coding-system'
  eol-type of `default-buffer-file-coding-system'
  `default-process-coding-system' for read
  eol-type of `default-process-coding-system' for read
  `default-process-coding-system' for write
  eol-type of `default-process-coding-system'"
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (process-coding-systems (if proc (process-coding-system proc))))
    (message
     "F[%c%s],K[%c%s],T[%c%s],P>[%c%s],P<[%c%s], default F[%c%s],P>[%c%s],P<[%c%s]"
     (coding-system-mnemonic buffer-file-coding-system)
     (coding-system-eol-type-mnemonic buffer-file-coding-system)
     (coding-system-mnemonic (keyboard-coding-system))
     (coding-system-eol-type-mnemonic (keyboard-coding-system))
     (coding-system-mnemonic (terminal-coding-system))
     (coding-system-eol-type-mnemonic (terminal-coding-system))
     (coding-system-mnemonic (car process-coding-systems))
     (coding-system-eol-type-mnemonic (car process-coding-systems))
     (coding-system-mnemonic (cdr process-coding-systems))
     (coding-system-eol-type-mnemonic (cdr process-coding-systems))
     (coding-system-mnemonic default-buffer-file-coding-system)
     (coding-system-eol-type-mnemonic default-buffer-file-coding-system)
     (coding-system-mnemonic (car default-process-coding-system))
     (coding-system-eol-type-mnemonic (car default-process-coding-system))
     (coding-system-mnemonic (cdr default-process-coding-system))
     (coding-system-eol-type-mnemonic (cdr default-process-coding-system))
     )))

;; Print symbol name and mnemonic letter of CODING-SYSTEM with `princ'.
(defun print-coding-system-briefly (coding-system &optional doc-string)
  (if (not coding-system)
      (princ "nil\n")
    (princ (format "%c -- %s"
		   (coding-system-mnemonic coding-system)
		   coding-system))
    (let ((aliases (coding-system-get coding-system 'alias-coding-systems)))
      (if (eq coding-system (car aliases))
	  (if (cdr aliases)
	      (princ (format " %S" (cons 'alias: (cdr aliases)))))
	(if (memq coding-system aliases)
	    (princ (format " (alias of %s)" (car aliases))))))
    (princ "\n\n")
    (if (and doc-string
	     (setq doc-string (coding-system-doc-string coding-system)))
	(princ (format "%s\n" doc-string)))))

;;;###autoload
(defun describe-current-coding-system ()
  "Display coding systems currently used, in detail."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (let* ((proc (get-buffer-process (current-buffer)))
	   (process-coding-systems (if proc (process-coding-system proc))))
      (princ "Coding system for saving this buffer:\n  ")
      (if (local-variable-p 'buffer-file-coding-system)
	  (print-coding-system-briefly buffer-file-coding-system)
	(princ "Not set locally, use the default.\n"))
      (princ "Default coding system (for new files):\n  ")
      (print-coding-system-briefly default-buffer-file-coding-system)
      (princ "Coding system for keyboard input:\n  ")
      (print-coding-system-briefly (keyboard-coding-system))
      (princ "Coding system for terminal output:\n  ")
      (print-coding-system-briefly (terminal-coding-system))
      (when (get-buffer-process (current-buffer))
	(princ "Coding systems for process I/O:\n")
	(princ "  encoding input to the process: ")
	(print-coding-system-briefly (cdr process-coding-systems))
	(princ "  decoding output from the process: ")
	(print-coding-system-briefly (car process-coding-systems)))
      (princ "Defaults for subprocess I/O:\n")
      (princ "  decoding: ")
      (print-coding-system-briefly (car default-process-coding-system))
      (princ "  encoding: ")
      (print-coding-system-briefly (cdr default-process-coding-system)))

    (with-current-buffer standard-output

      (princ "
Priority order for recognizing coding systems when reading files:\n")
      (let ((l coding-category-list)
	    (i 1)
	    (coding-list nil)
	    coding aliases)
	(while l
	  (setq coding (symbol-value (car l)))
	  ;; Do not list up the same coding system twice.
	  (when (and coding (not (memq coding coding-list)))
	    (setq coding-list (cons coding coding-list))
	    (princ (format "  %d. %s " i coding))
	    (setq aliases (coding-system-get coding 'alias-coding-systems))
	    (if (eq coding (car aliases))
		(if (cdr aliases)
		    (princ (cons 'alias: (cdr aliases))))
	      (if (memq coding aliases)
		  (princ (list 'alias 'of (car aliases)))))
	    (terpri)
	    (setq i (1+ i)))
	  (setq l (cdr l))))

      (princ "\n  Other coding systems cannot be distinguished automatically
  from these, and therefore cannot be recognized automatically
  with the present coding system priorities.\n\n")

      (let ((categories '(coding-category-iso-7 coding-category-iso-7-else))
	    coding-system codings)
	(while categories
	  (setq coding-system (symbol-value (car categories)))
	  (mapcar
	   (function
	    (lambda (x)
	      (if (and (not (eq x coding-system))
		       (coding-system-get x 'no-initial-designation)
		       (let ((flags (coding-system-flags x)))
			 (not (or (aref flags 10) (aref flags 11)))))
		  (setq codings (cons x codings)))))
	   (get (car categories) 'coding-systems))
	  (if codings
	      (let ((max-col (frame-width))
		    pos)
		(princ (format "\
  The following are decoded correctly but recognized as %s:\n   "
			       coding-system))
		(while codings
		  (setq pos (point))
		  (insert (format " %s" (car codings)))
		  (when (> (current-column) max-col)
		    (goto-char pos)
		    (insert "\n   ")
		    (goto-char (point-max)))
		  (setq codings (cdr codings)))
		(insert "\n\n")))
	  (setq categories (cdr categories))))

      (princ "Particular coding systems specified for certain file names:\n")
      (terpri)
      (princ "  OPERATION\tTARGET PATTERN\t\tCODING SYSTEM(s)\n")
      (princ "  ---------\t--------------\t\t----------------\n")
      (let ((func (lambda (operation alist)
		    (princ "  ")
		    (princ operation)
		    (if (not alist)
			(princ "\tnothing specified\n")
		      (while alist
			(indent-to 16)
			(prin1 (car (car alist)))
			(if (>= (current-column) 40)
			    (newline))
			(indent-to 40)
			(princ (cdr (car alist)))
			(princ "\n")
			(setq alist (cdr alist)))))))
	(funcall func "File I/O" file-coding-system-alist)
	(funcall func "Process I/O" process-coding-system-alist)
	(funcall func "Network I/O" network-coding-system-alist))
      (help-mode))))

;; Print detailed information on CODING-SYSTEM.
(defun print-coding-system (coding-system)
  (let ((type (coding-system-type coding-system))
	(eol-type (coding-system-eol-type coding-system))
	(flags (coding-system-flags coding-system))
	(aliases (coding-system-get coding-system 'alias-coding-systems)))
    (if (not (eq (car aliases) coding-system))
	(princ (format "%s (alias of %s)\n" coding-system (car aliases)))
      (princ coding-system)
      (setq aliases (cdr aliases))
      (while aliases
	(princ ",")
	(princ (car aliases))
	(setq aliases (cdr aliases)))
      (princ (format ":%s:%c:%d:"
		     type
		     (coding-system-mnemonic coding-system)
		     (if (integerp eol-type) eol-type 3)))
      (cond ((eq type 2)		; ISO-2022
	     (let ((idx 0)
		   charset)
	       (while (< idx 4)
		 (setq charset (aref flags idx))
		 (cond ((null charset)
			(princ -1))
		       ((eq charset t)
			(princ -2))
		       ((charsetp charset)
			(princ charset))
		       ((listp charset)
			(princ "(")
			(princ (car charset))
			(setq charset (cdr charset))
			(while charset
			  (princ ",")
			  (princ (car charset))
			  (setq charset (cdr charset)))
			(princ ")")))
		 (princ ",")
		 (setq idx (1+ idx)))
	       (while (< idx 12)
		 (princ (if (aref flags idx) 1 0))
		 (princ ",")
		 (setq idx (1+ idx)))
	       (princ (if (aref flags idx) 1 0))))
	    ((eq type 4)		; CCL
	     (let (i len)
	       (if (symbolp (car flags))
		   (princ (format " %s" (car flags)))
		 (setq i 0 len (length (car flags)))
		 (while (< i len)
		   (princ (format " %x" (aref (car flags) i)))
		   (setq i (1+ i))))
	       (princ ",")
	       (if (symbolp (cdr flags))
		   (princ (format "%s" (cdr flags)))
		 (setq i 0 len (length (cdr flags)))
		 (while (< i len)
		   (princ (format " %x" (aref (cdr flags) i)))
		   (setq i (1+ i))))))
	    (t (princ 0)))
      (princ ":")
      (princ (coding-system-doc-string coding-system))
      (princ "\n"))))

;;;###autoload
(defun list-coding-systems (&optional arg)
  "Display a list of all coding systems.
This shows the mnemonic letter, name, and description of each coding system.

With prefix arg, the output format gets more cryptic,
but still contains full information about each coding system."
  (interactive "P")
  (with-output-to-temp-buffer "*Help*"
    (list-coding-systems-1 arg)))

(defun list-coding-systems-1 (arg)
  (if (null arg)
      (princ "\
###############################################
# List of coding systems in the following format:
# MNEMONIC-LETTER -- CODING-SYSTEM-NAME
#	DOC-STRING
")
    (princ "\
#########################
## LIST OF CODING SYSTEMS
## Each line corresponds to one coding system
## Format of a line is:
##   NAME[,ALIAS...]:TYPE:MNEMONIC:EOL:FLAGS:POST-READ-CONVERSION
##	:PRE-WRITE-CONVERSION:DOC-STRING,
## where
##  NAME = coding system name
##  ALIAS = alias of the coding system
##  TYPE = nil (no conversion), t (undecided or automatic detection),
##         0 (EMACS-MULE), 1 (SJIS), 2 (ISO2022), 3 (BIG5), or 4 (CCL)
##  EOL = 0 (LF), 1 (CRLF), 2 (CR), or 3 (Automatic detection)
##  FLAGS =
##    if TYPE = 2 then
##      comma (`,') separated data of the followings:
##        G0, G1, G2, G3, SHORT-FORM, ASCII-EOL, ASCII-CNTL, SEVEN,
##        LOCKING-SHIFT, SINGLE-SHIFT, USE-ROMAN, USE-OLDJIS, NO-ISO6429
##    else if TYPE = 4 then
##      comma (`,') separated CCL programs for read and write
##    else
##      0
##  POST-READ-CONVERSION, PRE-WRITE-CONVERSION = function name to be called
##
"))
  (let ((bases (coding-system-list 'base-only))
	coding-system)
    (while bases
      (setq coding-system (car bases))
      (if (null arg)
	  (print-coding-system-briefly coding-system 'doc-string)
	(print-coding-system coding-system))
      (setq bases (cdr bases)))))

;;;###autoload
(defun list-coding-categories ()
  "Display a list of all coding categories."
  (with-output-to-temp-buffer "*Help*"
    (princ "\
############################
## LIST OF CODING CATEGORIES (ordered by priority)
## CATEGORY:CODING-SYSTEM
##
")
    (let ((l coding-category-list))
      (while l
	(princ (format "%s:%s\n" (car l) (symbol-value (car l))))
	(setq l (cdr l))))))

;;; FONT

;; Print information of a font in FONTINFO.
(defun describe-font-internal (font-info &optional verbose)
  (print-list "name (opened by):" (aref font-info 0))
  (print-list "       full name:" (aref font-info 1))
  (print-list "            size:" (format "%2d" (aref font-info 2)))
  (print-list "          height:" (format "%2d" (aref font-info 3)))
  (print-list " baseline-offset:" (format "%2d" (aref font-info 4)))
  (print-list "relative-compose:" (format "%2d" (aref font-info 5))))

;;;###autoload
(defun describe-font (fontname)
  "Display information about fonts which partially match FONTNAME."
  (interactive "sFontname (default, current choice for ASCII chars): ")
  (or (and window-system (fboundp 'fontset-list))
      (error "No fontsets being used"))
  (when (or (not fontname) (= (length fontname) 0))
    (setq fontname (cdr (assq 'font (frame-parameters))))
    (if (query-fontset fontname)
	(setq fontname
	      (nth 1 (assq 'ascii (aref (fontset-info fontname) 2))))))
  (let ((font-info (font-info fontname)))
    (if (null font-info)
	(message "No matching font")
      (with-output-to-temp-buffer "*Help*"
	(describe-font-internal font-info 'verbose)))))

(defun print-fontset (fontset &optional print-fonts)
  "Print information about FONTSET.
If optional arg PRINT-FONTS is non-nil, also print names of all opened
fonts for FONTSET.  This function actually inserts the information in
the current buffer."
  (let ((tail (aref (fontset-info fontset) 2))
	elt chars font-spec opened prev-charset charset from to)
    (beginning-of-line)
    (insert "Fontset: " fontset "\n")
    (insert "CHARSET or CHAR RANGE")
    (indent-to 24)
    (insert "FONT NAME\n")
    (insert "---------------------")
    (indent-to 24)
    (insert "---------")
    (insert "\n")
    (while tail
      (setq elt (car tail) tail (cdr tail))
      (setq chars (car elt) font-spec (car (cdr elt)) opened (cdr (cdr elt)))
      (if (symbolp chars)
	  (setq charset chars from nil to nil)
	(if (integerp chars)
	    (setq charset (char-charset chars) from chars to chars)
	  (setq charset (char-charset (car chars))
		from (car chars) to (cdr chars))))
      (unless (eq charset prev-charset)
	(insert (symbol-name charset))
	(if from
	    (insert "\n")))
      (when from
	(let ((split (split-char from)))
	  (if (and (= (charset-dimension charset) 2)
		   (= (nth 2 split) 0))
	      (setq from
		    (make-char charset (nth 1 split)
			       (if (= (charset-chars charset) 94) 33 32))))
	  (insert "  " from))
	(when (/= from to)
	  (insert "-")
	  (let ((split (split-char to)))
	    (if (and (= (charset-dimension charset) 2)
		     (= (nth 2 split) 0))
		(setq to
		      (make-char charset (nth 1 split)
				 (if (= (charset-chars charset) 94) 126 127))))
	    (insert to))))
      (indent-to 24)
      (if (stringp font-spec)
	  (insert font-spec)
	(if (car font-spec)
	    (if (string-match "-" (car font-spec))
		(insert "-" (car font-spec) "-*-")
	      (insert "-*-" (car font-spec) "-*-"))
	  (insert "-*-"))
	(if (cdr font-spec)
	    (if (string-match "-" (cdr font-spec))
		(insert (cdr font-spec))
	      (insert (cdr font-spec) "-*"))
	  (insert "*")))
      (insert "\n")
      (when print-fonts
	(while opened
	  (indent-to 5)
	  (insert "[" (car opened) "]\n")
	  (setq opened (cdr opened))))
      (setq prev-charset charset)
      )))

;;;###autoload
(defun describe-fontset (fontset)
  "Display information about FONTSET.
This shows which font is used for which character(s)."
  (interactive
   (if (not (and window-system (fboundp 'fontset-list)))
       (error "No fontsets being used")
     (let ((fontset-list (nconc
			  (mapcar 'list (fontset-list))
			  (mapcar (lambda (x) (list (cdr x)))
				  fontset-alias-alist)))
	   (completion-ignore-case t))
       (list (completing-read
	      "Fontset (default, used by the current frame): "
	      fontset-list nil t)))))
  (if (= (length fontset) 0)
      (setq fontset (cdr (assq 'font (frame-parameters)))))
  (if (not (setq fontset (query-fontset fontset)))
      (error "Current frame is using font, not fontset"))
  (help-setup-xref (list #'describe-fontset fontset) (interactive-p))
  (with-output-to-temp-buffer (help-buffer)
    (with-current-buffer standard-output
      (print-fontset fontset t))))

;;;###autoload
(defun list-fontsets (arg)
  "Display a list of all fontsets.
This shows the name, size, and style of each fontset.
With prefix arg, also list the fonts contained in each fontset;
see the function `describe-fontset' for the format of the list."
  (interactive "P")
  (if (not (and window-system (fboundp 'fontset-list)))
      (error "No fontsets being used")
    (help-setup-xref (list #'list-fontsets arg) (interactive-p))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer standard-output
	;; This code is duplicated near the end of mule-diag.
	(let ((fontsets
	       (sort (fontset-list)
		     (function (lambda (x y)
				 (string< (fontset-plain-name x)
					  (fontset-plain-name y)))))))
	  (while fontsets
	    (if arg
		(print-fontset (car fontsets) nil)
	      (insert "Fontset: " (car fontsets) "\n"))
	    (setq fontsets (cdr fontsets))))))))

;;;###autoload
(defun list-input-methods ()
  "Display information about all input methods."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (list-input-methods-1)
    (with-current-buffer standard-output
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward
		"^  \\([^ ]+\\) (`.*' in mode line)$" nil t)
	  (help-xref-button 1 #'help-input-method
				(match-string 1)
				"mouse-2: describe this method")))
      (help-setup-xref '(list-input-methods) (interactive-p)))))

(defun list-input-methods-1 ()
  (if (not input-method-alist)
      (progn
	(princ "
No input method is available, perhaps because you have not
installed LEIM (Libraries of Emacs Input Methods)."))
    (princ "LANGUAGE\n  NAME (`TITLE' in mode line)\n")
    (princ "    SHORT-DESCRIPTION\n------------------------------\n")
    (setq input-method-alist
	  (sort input-method-alist
		(function (lambda (x y) (string< (nth 1 x) (nth 1 y))))))
    (let ((l input-method-alist)
	  language elt)
      (while l
	(setq elt (car l) l (cdr l))
	(when (not (equal language (nth 1 elt)))
	  (setq language (nth 1 elt))
	  (princ language)
	  (terpri))
	(princ (format "  %s (`%s' in mode line)\n    %s\n"
		       (car elt)
		       (let ((title (nth 3 elt)))
			 (if (and (consp title) (stringp (car title)))
			     (car title)
			   title))
		       (let ((description (nth 4 elt)))
			 (string-match ".*" description)
			 (match-string 0 description))))))))

;;; DIAGNOSIS

;; Insert a header of a section with SECTION-NUMBER and TITLE.
(defun insert-section (section-number title)
  (insert "########################################\n"
	  "# Section " (format "%d" section-number) ".  " title "\n"
	  "########################################\n\n"))

;;;###autoload
(defun mule-diag ()
  "Display diagnosis of the multilingual environment (Mule).

This shows various information related to the current multilingual
environment, including lists of input methods, coding systems,
character sets, and fontsets (if Emacs is running under a window
system which uses fontsets)."
  (interactive)
  (with-output-to-temp-buffer "*Mule-Diagnosis*"
    (with-current-buffer standard-output
      (insert "###############################################\n"
	      "### Current Status of Multilingual Features ###\n"
	      "###############################################\n\n"
	      "CONTENTS: Section 1.  General Information\n"
	      "          Section 2.  Display\n"
	      "          Section 3.  Input methods\n"
	      "          Section 4.  Coding systems\n"
	      "          Section 5.  Character sets\n")
      (if (and window-system (fboundp 'fontset-list))
	  (insert "          Section 6.  Fontsets\n"))
      (insert "\n")

      (insert-section 1 "General Information")
      (insert "Version of this emacs:\n  " (emacs-version) "\n\n")
      (insert "Configuration options:\n  " system-configuration-options "\n\n")
      (insert "Multibyte characters awareness:\n"
	      (format "  default: %S\n" default-enable-multibyte-characters)
	      (format "  current-buffer: %S\n\n" enable-multibyte-characters))
      (insert "Current language environment: " current-language-environment
	      "\n\n")

      (insert-section 2 "Display")
      (if window-system
	  (insert "Window-system: "
		  (symbol-name window-system)
		  (format "%s" window-system-version))
	(insert "Terminal: " (getenv "TERM")))
      (insert "\n\n")

      (if (eq window-system 'x)
	  (let ((font (cdr (assq 'font (frame-parameters)))))
	    (insert "The selected frame is using the "
		    (if (query-fontset font) "fontset" "font")
		    ":\n\t" font))
	(insert "Coding system of the terminal: "
		(symbol-name (terminal-coding-system))))
      (insert "\n\n")

      (insert-section 3 "Input methods")
      (list-input-methods-1)
      (insert "\n")
      (if default-input-method
	  (insert (format "Default input method: %s\n" default-input-method))
	(insert "No default input method is specified\n"))

      (insert-section 4 "Coding systems")
      (list-coding-systems-1 t)
      (princ "\
############################
## LIST OF CODING CATEGORIES (ordered by priority)
## CATEGORY:CODING-SYSTEM
##
")
      (let ((l coding-category-list))
	(while l
	  (princ (format "%s:%s\n" (car l) (symbol-value (car l))))
	  (setq l (cdr l))))
      (insert "\n")

      (insert-section 5 "Character sets")
      (list-character-sets-2)
      (insert "\n")

      (when (and window-system (fboundp 'fontset-list))
	;; This code duplicates most of list-fontsets.
	(insert-section 6 "Fontsets")
	(insert "Fontset-Name\t\t\t\t\t\t  WDxHT Style\n")
	(insert "------------\t\t\t\t\t\t  ----- -----\n")
	(let ((fontsets (fontset-list)))
	  (while fontsets
	    (print-fontset (car fontsets) t)
	    (setq fontsets (cdr fontsets)))))
      (print-help-return-message))))

;;; mule-diag.el ends here
