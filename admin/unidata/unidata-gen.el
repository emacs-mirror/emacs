;; unidata-gen.el -- Create files containing character property data.

;; Copyright (C) 2008-2020 Free Software Foundation, Inc.

;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; SPECIAL NOTICE
;;
;;   This file must be byte-compilable/loadable by `temacs' and also
;;   the entry function `unidata-gen-file' must be runnable by `temacs'.

;; FILES TO BE GENERATED
;;
;;   The entry functions `unidata-gen-file' and `unidata-gen-charprop'
;;   generate these files:
;;
;;   charprop.el
;;	It contains a series of forms of this format:
;;	  (define-char-code-property PROP FILE)
;;	where PROP is a symbol representing a character property
;;	(name, general-category, etc), and FILE is a name of one of
;;	the following files.
;;
;;   uni-name.el, uni-category.el, uni-combining.el, uni-bidi.el,
;;   uni-decomposition.el, uni-decimal.el, uni-digit.el, uni-numeric.el,
;;   uni-mirrored.el, uni-old-name.el, uni-comment.el, uni-uppercase.el,
;;   uni-lowercase.el, uni-titlecase.el
;;	They contain one or more forms of this format:
;;	  (define-char-code-property PROP CHAR-TABLE)
;;	where PROP is the same as above, and CHAR-TABLE is a
;;	char-table containing property values in a compressed format.
;;
;;   When they are installed in .../lisp/international/, the file
;;   "charprop.el" is preloaded in loadup.el.  The other files are
;;   automatically loaded when the Lisp functions
;;   `get-char-code-property' and `put-char-code-property', and C
;;   function uniprop_table are called.
;;
;; FORMAT OF A CHAR TABLE
;;
;;   We want to make a file size containing a char-table small.  We
;;   also want to load the file and get a property value fast.  We
;;   also want to reduce the used memory after loading it.  So,
;;   instead of naively storing a property value for each character in
;;   a char-table (and write it out into a file), we store compressed
;;   data in a char-table as below.
;;
;;   If succeeding 128*N characters have the same property value, we
;;   store that value (or the encoded one) for them.  Otherwise,
;;   compress values (or the encoded ones) for succeeding 128
;;   characters into a single string and store it for those
;;   characters.  The way of compression depends on a property.  See
;;   the section "SIMPLE TABLE", "RUN-LENGTH TABLE", and "WORD-LIST
;;   TABLE".

;;   The char table has five extra slots:
;;      1st: property symbol
;;	2nd: function to call to get a property value,
;;	     or an index number of C function to decode the value,
;;	     or nil if the value can be directly got from the table.
;;	3nd: function to call to put a property value,
;;	     or an index number of C function to encode the value,
;;	     or nil if the value can be directly stored in the table.
;;	4th: function to call to get a description of a property value, or nil
;;	5th: data referred by the above functions

;; List of elements of this form:
;;   (CHAR-or-RANGE PROP1 PROP2 ... PROPn)
;; CHAR-or-RANGE: a character code or a cons of character codes
;; PROPn: string representing the nth property value

(eval-when-compile (require 'cl-lib))

(defvar unidata-list nil)

;; Name of the directory containing files of Unicode Character Database.

;; Dynamically bound in unidata-gen-file.
(defvar unidata-dir nil)

(defun unidata-setup-list (unidata-text-file)
  (let* ((table (list nil))
	 (tail table)
	 (block-names '(("^<CJK Ideograph" . CJK\ IDEOGRAPH)
                        ("^<Tangut Ideograph" . TANGUT\ IDEOGRAPH)
			("^<Hangul Syllable" . HANGUL\ SYLLABLE)
			("^<.*High Surrogate" . HIGH\ SURROGATE)
			("^<.*Low Surrogate" . LOW\ SURROGATE)
			("^<.*Private Use" . PRIVATE\ USE)))
	 val char name)
    (setq unidata-text-file (expand-file-name unidata-text-file unidata-dir))
    (or (file-readable-p unidata-text-file)
	(error "File not readable: %s" unidata-text-file))
    (with-temp-buffer
      ;; Insert a file of this format:
      ;;   (CHAR NAME CATEGORY ...)
      ;; where CHAR is a character code, the following elements are strings
      ;; representing character properties.
      (insert-file-contents unidata-text-file)
      (goto-char (point-min))
      (condition-case nil
	  (while t
	    (setq val (read (current-buffer))
		  char (car val)
		  name (cadr val))

	    ;; Check this kind of block.
	    ;;   4E00;<CJK Ideograph, First>;Lo;0;L;;;;;N;;;;;
	    ;;   9FCB;<CJK Ideograph, Last>;Lo;0;L;;;;;N;;;;;
	    (if (and (= (aref name 0) ?<)
		     (string-match ", First>$" name))
		(let ((first char)
		      (l block-names)
		      block-name)
		  (setq val (read (current-buffer))
			char (car val)
			block-name (cadr val)
			name nil)
		  (while l
		    (if (string-match (caar l) block-name)
			(setq name (cdar l) l nil)
		      (setq l (cdr l))))
		  (setcar val (cons first char))
		  (setcar (cdr val) name)))

	    (when val
	      (setcdr tail (list val))
	      (setq tail (cdr tail))))
	(error nil)))
    (setq unidata-list (cdr table))))

;; Alist of this form:
;;   (FILENAME (PROP INDEX GENERATOR DOCSTRING DESCRIBER DEFAULT VAL-LIST) ...)
;; FILENAME: filename to store the char-table(s)
;; PROP: character property
;; INDEX: index to each element of unidata-list for PROP.
;;   It may be a function that generates an alist of character codes
;;   vs. the corresponding property values.  Currently, only character
;;   codepoints or symbol values are supported in this case.
;; GENERATOR: function to generate a char-table
;; DOCSTRING: docstring for the property
;; DESCRIBER: function to call to get a description string of property value
;; DEFAULT: the default value of the property.  It may have the form
;;   (VAL0 (FROM1 TO1 VAL1) ...) which indicates that the default
;;   value is VAL0 except for characters in the ranges specified by
;;   FROMn and TOn (inclusive).  The default value of characters
;;   between FROMn and TOn is VALn.
;; VAL-LIST: list of specially ordered property values

(defconst unidata-file-alist
  '(
    ;; NB this list is parsed by the Makefile to extract the names of
    ;; the uni-*.el files, so preserve the formatting of those lines.
    ("uni-name.el"
     (name
      1 unidata-gen-table-name
      "Unicode character name.
Property value is a string or nil.
The value nil stands for the default value \"null string\")."
      nil
      nil))
    ("uni-category.el"
     (general-category
      2 unidata-gen-table-symbol
      "Unicode general category.
Property value is one of the following symbols:
  Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pc, Pd, Ps, Pe, Pi, Pf, Po,
  Sm, Sc, Sk, So, Zs, Zl, Zp, Cc, Cf, Cs, Co, Cn"
      unidata-describe-general-category
      Cn
      ;; The order of elements must be in sync with
      ;; unicode_category_t in src/character.h.
      (Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pc Pd Ps Pe Pi Pf Po
	  Sm Sc Sk So Zs Zl Zp Cc Cf Cs Co Cn)))
    ("uni-combining.el"
     (canonical-combining-class
      3 unidata-gen-table-integer
      "Unicode canonical combining class.
Property value is an integer."
      unidata-describe-canonical-combining-class
      0))
    ("uni-bidi.el"
     (bidi-class
      4 unidata-gen-table-symbol
      "Unicode bidi class.
Property value is one of the following symbols:
  L, LRE, LRO, LRI, R, AL, RLE, RLO, RLI, FSI, PDF, PDI,
  EN, ES, ET, AN, CS, NSM, BN, B, S, WS, ON"
      unidata-describe-bidi-class
      ;; The assignment of default values to blocks of code points
      ;; follows the file DerivedBidiClass.txt from the Unicode
      ;; Character Database (UCD).
      (L (#x0600 #x06FF AL) (#xFB50 #xFDFF AL) (#xFE70 #xFEFF AL)
	 (#x0590 #x05FF R) (#x07C0 #x08FF R)
	 (#xFB1D #xFB4F R) (#x10800 #x10FFF R) (#x1E800 #x1EFFF R))
      ;; The order of elements must be in sync with bidi_type_t in
      ;; src/dispextern.h.
      (L R EN AN BN B AL LRE LRO RLE RLO PDF LRI RLI FSI PDI
	 ES ET CS NSM S WS ON)))
    ("uni-decomposition.el"
     (decomposition
      5 unidata-gen-table-decomposition
      "Unicode decomposition mapping.
Property value is a list of characters.  The first element may be
one of these symbols representing compatibility formatting tag:
  font, noBreak, initial, medial, final, isolated, circle, super,
  sub, vertical, wide, narrow, small, square, fraction, compat"
      unidata-describe-decomposition))
    ("uni-decimal.el"
     (decimal-digit-value
      6 unidata-gen-table-integer
      "Unicode numeric value (decimal digit).
Property value is an integer 0..9, or nil.
The value nil stands for NaN \"Numeric_Value\"."))
    ("uni-digit.el"
     (digit-value
      7 unidata-gen-table-integer
      "Unicode numeric value (digit).
Property value is an integer 0..9, or nil.
The value nil stands for NaN \"Numeric_Value\"."))
    ("uni-numeric.el"
     (numeric-value
      8 unidata-gen-table-numeric
      "Unicode numeric value (numeric).
Property value is an integer, a floating point, or nil.
The value nil stands for NaN \"Numeric_Value\"."))
    ("uni-mirrored.el"
     (mirrored
      9 unidata-gen-table-symbol
      "Unicode bidi mirrored flag.
Property value is a symbol `Y' or `N'.  See also the property `mirroring'."
      nil
      N)
     (mirroring
      unidata-gen-mirroring-list unidata-gen-table-character
      "Unicode bidi-mirroring characters.
Property value is a character that has the corresponding mirroring image or nil.
The value nil means that the actual property value of a character
is the character itself."))
    ("uni-old-name.el"
     (old-name
      10 unidata-gen-table-name
      "Unicode old names as published in Unicode 1.0.
Property value is a string or nil.
The value nil stands for the default value \"null string\")."))
    ("uni-comment.el"
     (iso-10646-comment
      11 unidata-gen-table-name
      "Unicode ISO 10646 comment.
Property value is a string."))
    ("uni-uppercase.el"
     (uppercase
      12 unidata-gen-table-character
      "Unicode simple uppercase mapping.
Property value is a character or nil.
The value nil means that the actual property value of a character
is the character itself."
      string))
    ("uni-lowercase.el"
     (lowercase
      13 unidata-gen-table-character
      "Unicode simple lowercase mapping.
Property value is a character or nil.
The value nil means that the actual property value of a character
is the character itself."
      string))
    ("uni-titlecase.el"
     (titlecase
      14 unidata-gen-table-character
      "Unicode simple titlecase mapping.
Property value is a character or nil.
The value nil means that the actual property value of a character
is the character itself."
      string))
    ("uni-special-uppercase.el"
     (special-uppercase
      2 unidata-gen-table-special-casing
      "Unicode unconditional special casing mapping.

Property value is (possibly empty) string or nil.  The value nil denotes that
`uppercase' property should be consulted instead.  A string denotes what
sequence of characters given character maps into.

This mapping includes language- and context-independent special casing rules
defined by Unicode only.  It also does not include association which would
duplicate information from `uppercase' property."
      nil))
    ("uni-special-lowercase.el"
     (special-lowercase
      0 unidata-gen-table-special-casing
      "Unicode unconditional special casing mapping.

Property value is (possibly empty) string or nil.  The value nil denotes that
`lowercase' property should be consulted instead.  A string denotes what
sequence of characters given character maps into.

This mapping includes language- and context-independent special casing rules
defined by Unicode only.  It also does not include association which would
duplicate information from `lowercase' property."
      nil))
    ("uni-special-titlecase.el"
     (special-titlecase
      1 unidata-gen-table-special-casing
      "Unicode unconditional special casing mapping.

Property value is (possibly empty) string or nil.  The value nil denotes that
`titlecase' property should be consulted instead.  A string denotes what
sequence of characters given character maps into.

This mapping includes language- and context-independent special casing rules
defined by Unicode only.  It also does not include association which would
duplicate information from `titlecase' property."
      nil))
    ("uni-brackets.el"
     (paired-bracket
      unidata-gen-brackets-list unidata-gen-table-character
      "Unicode bidi paired-bracket characters.
Property value is the paired bracket character, or nil.
The value nil means that the character is neither an opening nor
a closing paired bracket."
      string)
     (bracket-type
      unidata-gen-bracket-type-list unidata-gen-table-symbol
      "Unicode bidi paired-bracket type.
Property value is a symbol `o' (Open), `c' (Close), or `n' (None)."
      unidata-describe-bidi-bracket-type
      n
      ;; The order of elements must be in sync with bidi_bracket_type_t
      ;; in src/dispextern.h.
      (n o c)))))

;; Functions to access the above data.
(defsubst unidata-prop-prop (proplist) (nth 0 proplist))
(defsubst unidata-prop-index (proplist) (nth 1 proplist))
(defsubst unidata-prop-generator (proplist) (nth 2 proplist))
(defsubst unidata-prop-docstring (proplist) (nth 3 proplist))
(defsubst unidata-prop-describer (proplist) (nth 4 proplist))
(defsubst unidata-prop-default (proplist) (nth 5 proplist))
(defsubst unidata-prop-val-list (proplist) (nth 6 proplist))


;; SIMPLE TABLE
;;
;; If the type of character property value is character, and the
;; values of succeeding character codes are usually different, we use
;; a char-table described here to store such values.
;;
;; A char-table divides character code space (#x0..#x3FFFFF) into
;; #x8000 blocks (each block contains 128 characters).

;; If all characters of a block have no property, a char-table has the
;; symbol nil for that block.  Otherwise a char-table has a string of
;; the following format for it.
;;
;; The first character of the string is ?\001.
;; The second character of the string is FIRST-INDEX.
;; The Nth (N > 1) character of the string is a property value of the
;; character (BLOCK-HEAD + FIRST-INDEX + N - 2), where BLOCK-HEAD is
;; the first character of the block.
;;
;; This kind of char-table has these extra slots:
;;   1st: the property symbol
;;   2nd: nil
;;   3rd: 0 (corresponding to uniprop_encode_character in chartab.c)
;;   4th to 5th: nil

(defun unidata-gen-table-character (prop prop-idx &rest ignore)
  (let ((table (make-char-table 'char-code-property-table))
	(vec (make-vector 128 0))
	(tail unidata-list)
	elt range val idx slot)
    (if (functionp prop-idx)
	(setq tail (funcall prop-idx)
	      prop-idx 1))
    (while tail
      (setq elt (car tail) tail (cdr tail))
      (setq range (car elt)
	    val (nth prop-idx elt))
      (if (= (length val) 0)
	  (setq val nil)
	(setq val (string-to-number val 16)))
      (if (consp range)
	  (if val
	      (set-char-table-range table range val))
	(let* ((start (ash (ash range -7) 7))
	       (limit (+ start 127))
	       first-index last-index)
	  (fillarray vec 0)
	  (if val
	      (aset vec (setq last-index (setq first-index (- range start)))
		    val))
	  (while (and (setq elt (car tail) range (car elt))
		      (integerp range)
		      (<= range limit))
	    (setq val (nth prop-idx elt))
	    (when (> (length val) 0)
	      (aset vec (setq last-index (- range start))
		    (string-to-number val 16))
	      (or first-index
		  (setq first-index last-index)))
	    (setq tail (cdr tail)))
	  (when first-index
	    (let ((str (string 1 first-index))
		  c)
	      (while (<= first-index last-index)
		(setq str (format "%s%c"  str (or (aref vec first-index) 0))
		      first-index (1+ first-index)))
	      (set-char-table-range table (cons start limit) str))))))

    (set-char-table-extra-slot table 0 prop)
    (set-char-table-extra-slot table 2 0)
    table))



;; RUN-LENGTH TABLE
;;
;; If many characters of successive character codes have the same
;; property value, we use a char-table described here to store the
;; values.
;;
;; At first, instead of a value itself, we store an index number to
;; the VAL-TABLE (5th extra slot) in the table.  We call that index
;; number as VAL-CODE here after.
;;
;; A char-table divides character code space (#x0..#x3FFFFF) into
;; #x8000 blocks (each block contains 128 characters).
;;
;; If all characters of a block have the same value, a char-table has
;; VAL-CODE for that block.  Otherwise a char-table has a string of
;; the following format for that block.
;;
;; The first character of the string is ?\002.
;; The following characters has this form:
;;   ( VAL-CODE RUN-LENGTH ? ) +
;; where:
;;   VAL-CODE (0..127): index into VAL-TABLE.
;;   RUN-LENGTH (130..255):
;;     (RUN-LENGTH - 128) specifies how many characters have the same
;;     value.  If omitted, it means 1.
;;
;; This kind of char-table has these extra slots:
;;   1st: the property symbol
;;   2nd: 0 (corresponding to uniprop_decode_value in chartab.c)
;;   3rd: 1..3 (corresponding to uniprop_encode_xxx in chartab.c)
;;   4th: function or nil
;;   5th: VAL-TABLE

;; Encode the character property value VAL into an integer value by
;; VAL-LIST.  By side effect, VAL-LIST is modified.
;; VAL-LIST has this form:
;;   ((nil . 0) (VAL1 . 1) (VAL2 . 2) ...)
;; If VAL is one of VALn, just return n.
;; Otherwise, VAL-LIST is modified to this:
;;   ((nil . 0) (VAL1 . 1) (VAL2 . 2) ... (VAL . n+1))
;;
;; WARN is an optional warning to display when the value list is
;; extended, for property values that need to be in sync with other
;; parts of Emacs; currently only used for bidi-class.

(defun unidata-encode-val (val-list val &optional warn)
  (let ((slot (assoc val val-list))
	val-code)
    (if slot
	(cdr slot)
      (if warn (message warn val))
      (setq val-code (length val-list))
      (nconc val-list (list (cons val val-code)))
      val-code)))

;; Generate a char-table for the character property PROP.

(defun unidata-gen-table (prop prop-idx val-func default-value val-list)
  (let ((table (make-char-table 'char-code-property-table))
	(vec (make-vector 128 0))
	;; When this warning is printed, there's a need to make the
	;; following changes:
	;; (1) update unidata-file-alist with the new bidi-class values;
	;; (2) extend bidi_type_t enumeration on src/dispextern.h to
	;;     include the new classes;
	;; (3) possibly update the assertion in bidi.c:bidi_check_type; and
	;; (4) possibly update the switch cases in
	;;     bidi.c:bidi_get_type and bidi.c:bidi_get_category.
	(bidi-warning "\
** Found new bidi-class `%s', please update bidi.c and dispextern.h")
	tail elt range val val-code idx slot
	prev-range-data)
    (setq val-list (cons nil (copy-sequence val-list)))
    (setq tail val-list val-code 0)
    ;; Convert (nil A B ...) to ((nil . 0) (A . 1) (B . 2) ...)
    (while tail
      (setcar tail (cons (car tail) val-code))
      (setq tail (cdr tail) val-code (1+ val-code)))
    (if (consp default-value)
	(setq default-value (copy-sequence default-value))
      (setq default-value (list default-value)))
    (setcar default-value
	    (unidata-encode-val val-list (car default-value)))
    (set-char-table-range table t (car default-value))
    (set-char-table-range table nil (car default-value))
    (dolist (elm (cdr default-value))
      (setcar (nthcdr 2 elm)
	      (unidata-encode-val val-list (nth 2 elm)))
      (set-char-table-range table (cons (car elm) (nth 1 elm)) (nth 2 elm)))

    (if (functionp prop-idx)
	(setq tail (funcall prop-idx)
	      prop-idx 1)
      (setq tail unidata-list))
    (while tail
      (setq elt (car tail) tail (cdr tail))
      (setq range (car elt)
	    val (funcall val-func (nth prop-idx elt)))
      (setq val-code (if val (unidata-encode-val val-list val
						 (and (eq prop 'bidi-class)
						      bidi-warning))))
      (if (consp range)
	  (when val-code
	    (set-char-table-range table range val-code)
	    (let ((from (car range)) (to (cdr range)))
	      ;; If RANGE doesn't end at the char-table boundary (each
	      ;; 128 characters), we may have to carry over the data
	      ;; for the last several characters (at most 127 chars)
	      ;; to the next loop.  In that case, set PREV-RANGE-DATA
	      ;; to ((FROM . TO) . VAL-CODE) where (FROM . TO)
	      ;; specifies the range of characters handled in the next
	      ;; loop.
	      (when (< (logand to #x7F) #x7F)
		(if (< from (logand to #x1FFF80))
		    (setq from (logand to #x1FFF80)))
		(setq prev-range-data (cons (cons from to) val-code)))))
	(let* ((start (ash (ash range -7) 7))
	       (limit (+ start 127))
	       str count new-val from to vcode)
	  (fillarray vec (car default-value))
	  (dolist (elm (cdr default-value))
	    (setq from (car elm) to (nth 1 elm))
	    (when (and (<= from limit)
		       (or (>= from start) (>= to start)))
	      (setq from (max from start)
		    to (min to limit)
		    vcode (nth 2 elm))
	      (while (<= from to)
		(aset vec (- from start) vcode)
		(setq from (1+ from)))))
	  ;; See the comment above.
	  (when (and prev-range-data
		     (>= (cdr (car prev-range-data)) start))
	    (setq from (car (car prev-range-data))
		  to (cdr (car prev-range-data))
		  vcode (cdr prev-range-data))
	    (while (<= from to)
	      (aset vec (- from start) vcode)
	      (setq from (1+ from))))
	  (setq prev-range-data nil)
	  (if val-code
	      (aset vec (- range start) val-code))
	  (while (and (setq elt (car tail) range (car elt))
		      (integerp range)
		      (<= range limit))
	    (setq new-val (funcall val-func (nth prop-idx elt)))
	    (if (not (eq val new-val))
		(setq val new-val
		      val-code (if val (unidata-encode-val
					val-list val (and (eq prop 'bidi-class)
							  bidi-warning)))))
	    (if val-code
		(aset vec (- range start) val-code))
	    (setq tail (cdr tail)))
	  (setq str "\002" val-code -1 count 0)
	  (mapc #'(lambda (x)
		    (if (= val-code x)
			(setq count (1+ count))
		      (if (> count 2)
			  (setq str (concat str (string val-code
							(+ count 128))))
			(if (= count 2)
			    (setq str (concat str (string val-code val-code)))
			  (if (= count 1)
			      (setq str (concat str (string val-code))))))
		      (setq val-code x count 1)))
		vec)
	  (if (= count 128)
	      (if val
		  (set-char-table-range table (cons start limit) val-code))
	    (if (= val-code 0)
		(set-char-table-range table (cons start limit) str)
	      (if (> count 2)
		  (setq str (concat str (string val-code (+ count 128))))
		(if (= count 2)
		    (setq str (concat str (string val-code val-code)))
		  (setq str (concat str (string val-code)))))
	      (set-char-table-range table (cons start limit) str))))))

    (set-char-table-extra-slot table 0 prop)
    (set-char-table-extra-slot table 4 (vconcat (mapcar 'car val-list)))
    table))

(defun unidata-gen-table-symbol (prop index default-value val-list)
  (let ((table (unidata-gen-table prop index
				  #'(lambda (x) (and (> (length x) 0)
						     (intern x)))
				  default-value val-list)))
    (set-char-table-extra-slot table 1 0)
    (set-char-table-extra-slot table 2 1)
    table))

(defun unidata-gen-table-integer (prop index default-value val-list)
  (let ((table (unidata-gen-table prop index
				  #'(lambda (x) (and (> (length x) 0)
						     (string-to-number x)))
				  default-value val-list)))
    (set-char-table-extra-slot table 1 0)
    (set-char-table-extra-slot table 2 1)
    table))

(defun unidata-gen-table-numeric (prop index default-value val-list)
  (let ((table (unidata-gen-table prop index
				  #'(lambda (x)
				      (if (string-match "/" x)
					  (/ (float (string-to-number x))
					     (string-to-number
					      (substring x (match-end 0))))
					(if (> (length x) 0)
					    (string-to-number x))))
				  default-value val-list)))
    (set-char-table-extra-slot table 1 0)
    (set-char-table-extra-slot table 2 2)
    table))


;; WORD-LIST TABLE

;;   If the table is for `name' property, each character in the string
;;   is one of these:
;;	DIFF-HEAD-CODE (0, 1, or 2):
;;	  specifies how to decode the following characters.
;;	WORD-CODE (3..#x7FF excluding '-', '0'..'9', 'A'..'Z'):
;;	  specifies an index number into WORD-TABLE (see below)
;;      Otherwise (' ', '-', '0'..'9', 'A'..'Z'):
;;	  specifies a literal word.
;;
;;   The 4th slots is a vector:
;;	[ WORD-TABLE BLOCK-NAME HANGUL-JAMO-TABLE ]
;;   WORD-TABLE is a vector of word symbols.
;;   BLOCK-NAME is a vector of name symbols for a block of characters.
;;   HANGUL-JAMO-TABLE is `unidata-name-jamo-name-table'.

;; Return the difference of symbol list L1 and L2 in this form:
;;   (DIFF-HEAD SYM1 SYM2 ...)
;; DIFF-HEAD is ((SAME-HEAD-LENGTH * 16) + SAME-TAIL-LENGTH).
;; Ex: If L1 is (a b c d e f) and L2 is (a g h e f), this function
;; returns ((+ (* 1 16) 2) g h).
;; It means that we can get L2 from L1 by prepending the first element
;; of L1 and appending the last 2 elements of L1 to the list (g h).
;; If L1 and L2 don't have common elements at the head and tail,
;; set DIFF-HEAD to -1 and SYM1 ... to the elements of L2.

(defun unidata-word-list-diff (l1 l2)
  (let ((beg 0)
	(end 0)
	(len1 (length l1))
	(len2 (length l2))
	result)
    (when (< len1 16)
      (while (and l1 (eq (car l1) (car l2)))
	(setq beg (1+ beg)
	      l1 (cdr l1) len1 (1- len1) l2 (cdr l2) len2 (1- len2)))
      (while (and (< end len1) (< end len2)
		  (eq (nth (- len1 end 1) l1) (nth (- len2 end 1) l2)))
	(setq end (1+ end))))
    (if (= (+ beg end) 0)
	(setq result (list -1))
      (setq result (list (+ (* beg 16) (+ beg (- len1 end))))))
    (while (< end len2)
      (setcdr result (cons (nth (- len2 end 1) l2) (cdr result)))
      (setq end (1+ end)))
    result))

;; Return a compressed form of the vector VEC.  Each element of VEC is
;; a list of symbols of which names can be concatenated to form a
;; character name.  This function changes those elements into
;; compressed forms by utilizing the fact that diff of consecutive
;; elements is usually small.

(defun unidata-word-list-compress (vec)
  (let (last-elt last-idx diff-head tail elt val)
    (dotimes (i 128)
      (setq elt (aref vec i))
      (when elt
	(if (null last-elt)
	    (setq diff-head -1
		  val (cons 0 elt))
	  (setq val (unidata-word-list-diff last-elt elt))
	  (if (= (car val) -1)
	      (setq diff-head -1
		    val (cons 0 (cdr val)))
	    (if (eq diff-head (car val))
		(setq val (cons 2 (cdr val)))
	      (setq diff-head (car val))
	      (if (>= diff-head 0)
		  (setq val (cons 1 val))))))
	(aset vec i val)
	(setq last-idx i last-elt elt)))
    (if (not last-idx)
	(setq vec nil)
      (if (< last-idx 127)
	  (let ((shorter (make-vector (1+ last-idx) nil)))
	    (dotimes (i (1+ last-idx))
	      (aset shorter i (aref vec i)))
	    (setq vec shorter))))
    vec))

;; Encode the word index IDX into a characters code that can be
;; embedded in a string.

(defsubst unidata-encode-word (idx)
  ;; Exclude 0, 1, 2.
  (+ idx 3))

;; Decode the character code CODE (that is embedded in a string) into
;; the corresponding word name by looking up WORD-TABLE.

(defsubst unidata-decode-word (code word-table)
  (setq code (- code 3))
  (if (< code (length word-table))
      (aref word-table code)))

;; Table of short transliterated name symbols of Hangul Jamo divided
;; into Choseong, Jungseong, and Jongseong.

(defconst unidata-name-jamo-name-table
  [[G GG N D DD R M B BB S SS nil J JJ C K T P H]
   [A AE YA YAE EO E YEO YE O WA WAE OE YO U WEO WE WI YU EU YI I]
   [G GG GS N NJ NH D L LG LM LB LS LT LP LH M B BS S SS NG J C K T P H]])

;; Return a name of CHAR.  VAL is the current value of (aref TABLE
;; CHAR).

(defun unidata-get-name (char val table)
  (cond
   ((stringp val)
    (if (> (aref val 0) 0)
	val
      (let* ((first-char (ash (ash char -7) 7))
	     (word-table (aref (char-table-extra-slot table 4) 0))
	     (i 1)
	     (len (length val))
	     (vec (make-vector 128 nil))
	     (idx 0)
	     (case-fold-search nil)
	     c word-list tail-list last-list word diff-head)
	(while (< i len)
	  (setq c (aref val i))
	  (if (< c 3)
	      (progn
		(if (or word-list tail-list)
		    (aset vec idx
			  (setq last-list (nconc word-list tail-list))))
		(setq i (1+ i) idx (1+ idx)
		      word-list nil tail-list nil)
		(if (> c 0)
		    (let ((l last-list))
		      (if (= c 1)
			  (setq diff-head
				(prog1 (aref val i) (setq i (1+ i)))))
		      (setq tail-list (nthcdr (% diff-head 16) last-list))
		      (dotimes (i (/ diff-head 16))
			(setq word-list (nconc word-list (list (car l)))
			      l (cdr l))))))
	    (setq word-list
		  (nconc word-list
			 (list (symbol-name
				(unidata-decode-word c word-table))))
		  i (1+ i))))
	(if (or word-list tail-list)
	    (aset vec idx (nconc word-list tail-list)))
	(setq val nil)
	(dotimes (i 128)
	  (setq c (+ first-char i))
	  (let ((name (aref vec i)))
	    (if name
		(let ((tail (cdr (setq name (copy-sequence name))))
		      elt)
		  (while tail
		    (setq elt (car tail))
		    (or (string= elt "-")
			(progn
			  (setcdr tail (cons elt (cdr tail)))
			  (setcar tail " ")))
		    (setq tail (cddr tail)))
		  (setq name (apply 'concat name))))
	    (aset table c name)
	    (if (= c char)
		(setq val name))))
	val)))

   ((and (integerp val) (> val 0))
    (let* ((symbol-table (aref (char-table-extra-slot table 4) 1))
	   (sym (aref symbol-table (1- val))))
      (cond ((eq sym 'HANGUL\ SYLLABLE)
	     (let ((jamo-name-table (aref (char-table-extra-slot table 4) 2)))
	       ;; SIndex = S - SBase
	       (setq char (- char #xAC00))
	       (let ( ;; LIndex = SIndex / NCount
		     (L (/ char 588))
		     ;; VIndex = (SIndex % NCount) * TCount
		     (V (/ (% char 588) 28))
		     ;; TIndex = SIndex % TCount
		     (T (% char 28)))
		 (format "HANGUL SYLLABLE %s%s%s"
			 ;; U+110B is nil in this table.
			 (or (aref (aref jamo-name-table 0) L) "")
			 (aref (aref jamo-name-table 1) V)
			 (if (= T 0) ""
			   (aref (aref jamo-name-table 2) (1- T)))))))
	    ((eq sym 'CJK\ IDEOGRAPH)
	     (format "%s-%04X" sym char))
	    ((eq sym 'TANGUT\ IDEOGRAPH)
	     (format "%s-%04X" sym char))
	    ((eq sym 'CJK\ COMPATIBILITY\ IDEOGRAPH)
	     (format "%s-%04X" sym char))
	    ((eq sym 'HIGH\ SURROGATE)
	     (format "%s-%04X" sym char))
	    ((eq sym 'LOW\ SURROGATE)
	     (format "%s-%04X" sym char))
	    ((eq sym 'VARIATION\ SELECTOR)
	     (format "%s-%d" sym (+ (- char #xe0100) 17))))))))

;; Store VAL as the name of CHAR in TABLE.

(defun unidata-put-name (char val table)
  (let ((current-val (aref table char)))
    (if (and (stringp current-val) (= (aref current-val 0) 0))
	(funcall (char-table-extra-slot table 1) char current-val table))
    (aset table char val)))

(defun unidata-get-decomposition (char val table)
  (cond
   ((not val)
    (list char))

   ((consp val)
    val)

   ((stringp val)
    (if (> (aref val 0) 0)
	val
      (let* ((first-char (ash (ash char -7) 7))
	     (word-table (char-table-extra-slot table 4))
	     (i 1)
	     (len (length val))
	     (vec (make-vector 128 nil))
	     (idx 0)
	     (case-fold-search nil)
	     c word-list tail-list last-list word diff-head)
	(while (< i len)
	  (setq c (aref val i))
	  (if (< c 3)
	      (progn
		(if (or word-list tail-list)
		    (aset vec idx
			  (setq last-list (nconc word-list tail-list))))
		(setq i (1+ i) idx (1+ idx)
		      word-list nil tail-list nil)
		(if (> c 0)
		    (let ((l last-list))
		      (if (= c 1)
			  (setq diff-head
				(prog1 (aref val i) (setq i (1+ i)))))
		      (setq tail-list (nthcdr (% diff-head 16) last-list))
		      (dotimes (i (/ diff-head 16))
			(setq word-list (nconc word-list (list (car l)))
			      l (cdr l))))))
	    (setq word-list
		  (nconc word-list
			 (list (or (unidata-decode-word c word-table) c)))
		  i (1+ i))))
	(if (or word-list tail-list)
	    (aset vec idx (nconc word-list tail-list)))
	(dotimes (i 128)
	  (aset table (+ first-char i) (aref vec i)))
	(setq val (aref vec (- char first-char)))
	(or val (list char)))))

   ;; Hangul syllable
   ((and (eq val 0) (>= char #xAC00) (<= char #xD7A3))
    ;; SIndex = S (char) - SBase (#xAC00)
    (setq char (- char #xAC00))
    (let (;; L = LBase + SIndex / NCount
	  (L (+ #x1100 (/ char 588)))
	  ;; V = VBase + (SIndex % NCount) * TCount
	  (V (+ #x1161 (/ (% char 588) 28)))
	  ;; LV = SBase + (SIndex / TCount) * TCount
	  (LV (+ #xAC00 (* (/ char 28) 28)))
	  ;; T = TBase + SIndex % TCount
	  (T (+ #x11A7 (% char 28))))
      (if (= T #x11A7)
	  (list L V)
	(list LV T))))

   ))

;; Store VAL as the decomposition information of CHAR in TABLE.

(defun unidata-put-decomposition (char val table)
  (let ((current-val (aref table char)))
    (if (and (stringp current-val) (= (aref current-val 0) 0))
	(funcall (char-table-extra-slot table 1) char current-val table))
    (aset table char val)))

;; UnicodeData.txt contains these lines:
;;   0000;<control>;Cc;0;BN;;;;;N;NULL;;;;
;;   ...
;;   0020;SPACE;Zs;0;WS;;;;;N;;;;;
;;   ...
;; The following command yields a file of about 96K bytes.
;;   % gawk -F ';' '{print $1,$2;}' < UnicodeData.txt | gzip > temp.gz
;; With the following function, we can get a file of almost the same
;; size.

;; Generate a char-table for character names.

(defun unidata-gen-table-word-list (prop prop-idx val-func)
  (let ((table (make-char-table 'char-code-property-table))
	(word-list (list nil))
	word-table
	block-list block-word-table block-end
	tail elt range val idx slot)
    (setq tail unidata-list)
    (setq block-end -1)
    (while tail
      (setq elt (car tail) tail (cdr tail))
      (setq range (car elt)
	    val (funcall val-func (nth prop-idx elt)))
      ;; Treat the sequence of "CJK COMPATIBILITY IDEOGRAPH-XXXX" and
      ;; "VARIATION SELECTOR-XXX" as a block.
      (if (and (consp val) (eq prop 'name)
	       (or (and (eq (car val) 'CJK)
			(eq (nth 1 val) 'COMPATIBILITY))
		   (and (>= range #xe0100)
			(eq (car val) 'VARIATION)
			(eq (nth 1 val) 'SELECTOR))))
	  (let ((first (car val))
		(second (nth 1 val))
		(start range))
	    (while (and (setq elt (car tail) range (car elt)
			      val (funcall val-func (nth prop-idx elt)))
			(consp val)
			(eq first (car val))
			(eq second (nth 1 val)))
	      (setq block-end range
		    tail (cdr tail)))
	    (setq range (cons start block-end)
		  val (if (eq first 'CJK) 'CJK\ COMPATIBILITY\ IDEOGRAPH
			'VARIATION\ SELECTOR))))

      (if (consp range)
	  (if val
	      (let ((slot (assq val block-list)))
		(setq range (cons (car range) (cdr range)))
		(setq block-end (cdr range))
		(if slot
		    (nconc slot (list range))
		  (push (list val range) block-list))))
	(let* ((start (ash (ash range -7) 7))
	       (limit (+ start 127))
	       (first tail)
	       (vec (make-vector 128 nil))
	       c name len)
	  (if (<= start block-end)
	      ;; START overlap with the previous block.
	      (aset table range (nth prop-idx elt))
	    (if val
		(aset vec (- range start) val))
	    (while (and (setq elt (car tail) range (car elt))
			(integerp range)
			(<= range limit))
	      (setq val (funcall val-func (nth prop-idx elt)))
	      (if val
		  (aset vec (- range start) val))
	      (setq tail (cdr tail)))
	    (setq vec (unidata-word-list-compress vec))
	    (when vec
	      (dotimes (i (length vec))
		(dolist (elt (aref vec i))
		  (if (symbolp elt)
                      (cl-incf (alist-get elt (cdr word-list) 0)))))
	      (set-char-table-range table (cons start limit) vec))))))
    (setq word-list (sort (cdr word-list)
			  #'(lambda (x y) (> (cdr x) (cdr y)))))
    (setq tail word-list idx 0)
    (while tail
      (setcdr (car tail) (unidata-encode-word idx))
      (setq idx (1+ idx) tail (cdr tail)))
    (setq word-table (make-vector (length word-list) nil))
    (setq idx 0)
    (dolist (elt word-list)
      (aset word-table idx (car elt))
      (setq idx (1+ idx)))

    (if (and (eq prop 'decomposition)
	     (> idx 32))
	(error "Too many symbols in decomposition data"))

    (dotimes (i (/ #x110000 128))
      (let* ((idx (* i 128))
	     (vec (aref table idx)))
	(when (vectorp vec)
	  (dotimes (i (length vec))
	    (let ((tail (aref vec i))
		  elt code)
	      (if (not tail)
		  (aset vec i "\0")
		(while tail
		  (setq elt (car tail)
			code (if (integerp elt) elt
			       (cdr (assq elt word-list))))
		  (setcar tail (string code))
		  (setq tail (cdr tail)))
		(aset vec i (mapconcat 'identity (aref vec i) "")))))
	  (set-char-table-range
	   table (cons idx (+ idx 127))
	   (mapconcat 'identity vec "")))))

    (setq block-word-table (make-vector (length block-list) nil))
    (setq idx 0)
    (dolist (elt block-list)
      (dolist (e (cdr elt))
	(set-char-table-range table e (1+ idx)))
      (aset block-word-table idx (car elt))
      (setq idx (1+ idx)))

    (set-char-table-extra-slot table 0 prop)
    (set-char-table-extra-slot table 4 (cons word-table block-word-table))
    table))

(defun unidata-split-name (str)
  (if (symbolp str)
      str
    (let ((len (length str))
	  (l nil)
	  (idx 0)
	  c)
      (if (or (= len 0)
	      ;; Unicode Standard, paragraph 4.8: "For all other
	      ;; Unicode code points of all other types (Control,
	      ;; Private-Use, Surrogate, Noncharacter, and Reserved),
	      ;; the value of the Name property is the null string."
	      ;; We already handle elsewhere all the characters except
	      ;; Cc, Control characters, which are handled here.
	      (string= str "<control>"))
	  nil
	(dotimes (i len)
	  (setq c (aref str i))
	  (if (= c 32)
	      (setq l (cons (intern (substring str idx i)) l)
		    idx (1+ i))
	    (if (and (= c ?-) (< idx i)
		     (< (1+ i) len) (/= (aref str (1+ i)) 32))
		(setq l (cons '- (cons (intern (substring str idx i)) l))
		      idx (1+ i)))))
	(nreverse (cons (intern (substring str idx)) l))))))

(defun unidata--ensure-compiled (&rest funcs)
  (dolist (fun funcs)
    (or (byte-code-function-p (symbol-function fun))
	(byte-compile fun))))

(defun unidata-gen-table-name (prop index &rest ignore)
  (let* ((table (unidata-gen-table-word-list prop index 'unidata-split-name))
	 (word-tables (char-table-extra-slot table 4)))
    (unidata--ensure-compiled 'unidata-get-name 'unidata-put-name)
    (set-char-table-extra-slot table 1 (symbol-function 'unidata-get-name))
    (set-char-table-extra-slot table 2 (symbol-function 'unidata-put-name))

    (if (eq prop 'name)
	(set-char-table-extra-slot table 4
				   (vector (car word-tables)
					   (cdr word-tables)
					   unidata-name-jamo-name-table))
      (set-char-table-extra-slot table 4
				 (vector (car word-tables))))
    table))

(defun unidata-split-decomposition (str)
  (if (symbolp str)
      str
    (let ((len (length str))
	  (l nil)
	  (idx 0)
	  c)
      (if (= len 0)
	  nil
	(dotimes (i len)
	  (setq c (aref str i))
	  (if (= c 32)
	      (setq l (if (= (aref str idx) ?<)
			  (cons (intern (substring str (1+ idx) (1- i))) l)
			(cons (string-to-number (substring str idx i) 16) l))
		    idx (1+ i))))
	(if (= (aref str idx) ?<)
	    (setq l (cons (intern (substring str (1+ idx) (1- len))) l))
	  (setq l (cons (string-to-number (substring str idx len) 16) l)))
	(nreverse l)))))


(defun unidata-gen-table-decomposition (prop index &rest ignore)
  (let* ((table (unidata-gen-table-word-list prop index 'unidata-split-decomposition))
	 (word-tables (char-table-extra-slot table 4)))
    (unidata--ensure-compiled 'unidata-get-decomposition
			      'unidata-put-decomposition)
    (set-char-table-extra-slot table 1
			       (symbol-function 'unidata-get-decomposition))
    (set-char-table-extra-slot table 2
			       (symbol-function 'unidata-put-decomposition))
    (set-char-table-extra-slot table 4 (car word-tables))
    table))




(defvar unidata-gen-table-special-casing--cache nil
  "Cached value for `unidata-gen-table-special-casing' function.")

(defun unidata-gen-table-special-casing--do-load ()
  (let (result)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "SpecialCasing.txt" unidata-dir))
      (goto-char (point-min))
      (while (not (eobp))
        ;; Ignore empty lines and comments.
        (unless (or (eq (char-after) ?\n) (eq (char-after) ?#))
          (let ((line (split-string
                       (buffer-substring (point) (progn (end-of-line) (point)))
                       ";" "")))
            ;; Ignore entries with conditions, i.e. those with six values.
            (when (= (length line) 5)
              (let ((ch (string-to-number (pop line) 16)))
                (setcdr (cddr line) nil) ; strip comment
                (push
                 (cons ch
                       (mapcar (lambda (entry)
                                 (mapcar (lambda (n) (string-to-number n 16))
                                         (split-string entry)))
                               line))
                 result)))))
        (forward-line)))
    result))

(defun unidata-gen-table-special-casing (prop prop-idx &rest ignore)
  (let ((table (make-char-table 'char-code-property-table)))
    (set-char-table-extra-slot table 0 prop)
    (mapc (lambda (entry)
            (let ((ch (car entry)) (v (nth prop-idx (cdr entry))))
              ;; If character maps to a single character, the mapping is already
              ;; covered by regular casing property.  Don’t store those.
              (when (/= (length v) 1)
                (set-char-table-range table ch (apply 'string v)))))
          (or unidata-gen-table-special-casing--cache
              (setq unidata-gen-table-special-casing--cache
                    (unidata-gen-table-special-casing--do-load))))
    table))


(defun unidata-describe-general-category (val)
  (cdr (assq val
	     '((nil . "Uknown")
	       (Lu . "Letter, Uppercase")
	       (Ll . "Letter, Lowercase")
	       (Lt . "Letter, Titlecase")
	       (Lm . "Letter, Modifier")
	       (Lo . "Letter, Other")
	       (Mn . "Mark, Nonspacing")
	       (Mc . "Mark, Spacing Combining")
	       (Me . "Mark, Enclosing")
	       (Nd . "Number, Decimal Digit")
	       (Nl . "Number, Letter")
	       (No . "Number, Other")
	       (Pc . "Punctuation, Connector")
	       (Pd . "Punctuation, Dash")
	       (Ps . "Punctuation, Open")
	       (Pe . "Punctuation, Close")
	       (Pi . "Punctuation, Initial quote")
	       (Pf . "Punctuation, Final quote")
	       (Po . "Punctuation, Other")
	       (Sm . "Symbol, Math")
	       (Sc . "Symbol, Currency")
	       (Sk . "Symbol, Modifier")
	       (So . "Symbol, Other")
	       (Zs . "Separator, Space")
	       (Zl . "Separator, Line")
	       (Zp . "Separator, Paragraph")
	       (Cc . "Other, Control")
	       (Cf . "Other, Format")
	       (Cs . "Other, Surrogate")
	       (Co . "Other, Private Use")
	       (Cn . "Other, Not Assigned")))))

(defun unidata-describe-canonical-combining-class (val)
  (cdr (assq val
	     '((0 . "Spacing, split, enclosing, reordrant, and Tibetan subjoined")
	       (1 . "Overlays and interior")
	       (7 . "Nuktas")
	       (8 . "Hiragana/Katakana voicing marks")
	       (9 . "Viramas")
	       (10 . "Start of fixed position classes")
	       (199 . "End of fixed position classes")
	       (200 . "Below left attached")
	       (202 . "Below attached")
	       (204 . "Below right attached")
	       (208 . "Left attached (reordrant around single base character)")
	       (210 . "Right attached")
	       (212 . "Above left attached")
	       (214 . "Above attached")
	       (216 . "Above right attached")
	       (218 . "Below left")
	       (220 . "Below")
	       (222 . "Below right")
	       (224 . "Left (reordrant around single base character)")
	       (226 . "Right")
	       (228 . "Above left")
	       (230 . "Above")
	       (232 . "Above right")
	       (233 . "Double below")
	       (234 . "Double above")
	       (240 . "Below (iota subscript)")))))

(defun unidata-describe-bidi-class (val)
  (cdr (assq val
	     '((L . "Left-to-Right")
	       (LRE . "Left-to-Right Embedding")
	       (LRO . "Left-to-Right Override")
	       (R . "Right-to-Left")
	       (AL . "Right-to-Left Arabic")
	       (RLE . "Right-to-Left Embedding")
	       (RLO . "Right-to-Left Override")
	       (PDF . "Pop Directional Format")
	       (LRI . "Left-to-Right Isolate")
	       (RLI . "Right-to-Left Isolate")
	       (FSI . "First Strong Isolate")
	       (PDI . "Pop Directional Isolate")
	       (EN . "European Number")
	       (ES . "European Number Separator")
	       (ET . "European Number Terminator")
	       (AN . "Arabic Number")
	       (CS . "Common Number Separator")
	       (NSM . "Non-Spacing Mark")
	       (BN . "Boundary Neutral")
	       (B . "Paragraph Separator")
	       (S . "Segment Separator")
	       (WS . "Whitespace")
	       (ON . "Other Neutrals")))))

(defun unidata-describe-decomposition (val)
  (mapconcat
   #'(lambda (x)
       (if (symbolp x) (symbol-name x)
	 (concat (string ?')
		 (compose-string (string x) 0 1 (string ?\t x ?\t))
		 (string ?'))))
   val " "))

(defun unidata-describe-bidi-bracket-type (val)
  (cdr (assq val
	     '((n . "Not a paired bracket character.")
	       (o . "Opening paired bracket character.")
	       (c . "Closing paired bracket character.")))))

(defun unidata-gen-mirroring-list ()
  (let ((head (list nil))
	tail)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "BidiMirroring.txt" unidata-dir))
      (goto-char (point-min))
      (setq tail head)
      (while (re-search-forward "^\\([0-9A-F]+\\);\\s +\\([0-9A-F]+\\)" nil t)
	(let ((char (string-to-number (match-string 1) 16))
	      (mirror (match-string 2)))
	  (setq tail (setcdr tail (list (list char mirror)))))))
    (cdr head)))

(defun unidata-gen-brackets-list ()
  (let ((head (list nil))
	tail)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "BidiBrackets.txt" unidata-dir))
      (goto-char (point-min))
      (setq tail head)
      (while (re-search-forward
	      "^\\([0-9A-F]+\\);\\s +\\([0-9A-F]+\\);\\s +\\([oc]\\)"
	      nil t)
	(let ((char (string-to-number (match-string 1) 16))
	      (paired (match-string 2)))
	  (setq tail (setcdr tail (list (list char paired)))))))
    (cdr head)))

(defun unidata-gen-bracket-type-list ()
  (let ((head (list nil))
	tail)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "BidiBrackets.txt" unidata-dir))
      (goto-char (point-min))
      (setq tail head)
      (while (re-search-forward
	      "^\\([0-9A-F]+\\);\\s +\\([0-9A-F]+\\);\\s +\\([oc]\\)"
	      nil t)
	(let ((char (string-to-number (match-string 1) 16))
	      (type (match-string 3)))
	  (setq tail (setcdr tail (list (list char type)))))))
    (cdr head)))

;; Verify if we can retrieve correct values from the generated
;; char-tables.
;;
;; Use like this:
;;
;; (let ((unidata-dir "/path/to/admin/unidata"))
;;   (unidata-setup-list "unidata.txt")
;;   (unidata-check))

(defun unidata-check ()
  (dolist (elt unidata-file-alist)
    (dolist (proplist (cdr elt))
      (let* ((prop (unidata-prop-prop proplist))
	     (index (unidata-prop-index proplist))
	     (generator (unidata-prop-generator proplist))
	     (default-value (unidata-prop-default proplist))
	     (val-list (unidata-prop-val-list proplist))
	     (check #x400)
	     table decoder alist)
        ;; We compare values in unidata.txt with the ones returned by various
        ;; generator functions.  However, SpecialCasing.txt is read directly by
        ;; unidata-gen-table-special-casing--do-load and there is no other file
        ;; to compare those values with.  This is why we’re skipping the check
        ;; for special casing properties.
        (unless (eq generator 'unidata-gen-table-special-casing)
          (setq table (progn
                        (message "Generating %S table..." prop)
                        (funcall generator prop index default-value val-list))
                decoder (char-table-extra-slot table 1))
          (unless (integerp decoder)
            (setq alist (and (functionp index) (funcall index)))
            (dolist (e unidata-list)
              (let ((char (car e)) val1 val2)
                (unless (consp char)
                  (setq val1 (if alist
                                 (nth 1 (assoc char alist))
                               (nth index e)))
                  (and (stringp val1)
                       (= (length val1) 0)
                       (setq val1 nil))
                  (if val1
                      (cond ((eq generator 'unidata-gen-table-symbol)
                             (setq val1 (intern val1)))
                            ((eq generator 'unidata-gen-table-integer)
                             (setq val1 (string-to-number val1)))
                            ((eq generator 'unidata-gen-table-character)
                             (setq val1 (string-to-number val1 16)))
                            ((eq generator 'unidata-gen-table-decomposition)
                             (setq val1 (unidata-split-decomposition val1))))
                    (cond ((eq prop 'decomposition)
                           (setq val1 (list char)))
                          ((eq prop 'bracket-type)
                           (setq val1 'n))))
                  (setq val2 (aref table char))
                  (when decoder
                    (setq val2 (funcall decoder char val2 table)))
                  (when (>= char check)
                    (message "%S %04X" prop check)
                    (setq check (+ check #x400)))
                  (or (equal val1 val2)
                      ;; <control> characters get a 'name' property of nil
                      (and (eq prop 'name)
                           (string= val1 "<control>")
                           (null val2))
                      (insert (format "> %04X %S\n< %04X %S\n"
                                      char val1 char val2)))
                  (sit-for 0))))))))))

;; The entry functions.  They generate files described in the header
;; comment of this file.

(defun unidata-gen-file (&optional file data-dir unidata-text-file)
  "Generate lisp file FILE from Unicode data."
  (or file
      (setq file (pop command-line-args-left)
	    data-dir (or (pop command-line-args-left) default-directory)
	    unidata-text-file (or (pop command-line-args-left)
				  (expand-file-name "unidata.txt"))))
  (let* ((coding-system-for-write 'utf-8-unix)
	 (coding-system-for-read 'utf-8)
	 (unidata-dir data-dir)
	 (copyright (with-temp-buffer
		      (insert-file-contents
		       (expand-file-name "copyright.html" unidata-dir))
		      (re-search-forward "Copyright .*Unicode, Inc.")
		      (match-string 0))))
    (or unidata-list (unidata-setup-list unidata-text-file))
    (let* ((basename (file-name-nondirectory file))
	   (elt (assoc basename unidata-file-alist)))
      (or elt (user-error "Unknown output file: %s" basename))
      (or noninteractive (message "Generating %s..." file))
      (with-temp-file file
        (insert ";; " copyright "
;; Generated from Unicode data files by unidata-gen.el.
;; The sources for this file are found in the admin/unidata/ directory in
;; the Emacs sources.  The Unicode data files are used under the
;; Unicode Terms of Use, as contained in the file copyright.html in that
;; same directory.\n")
        (dolist (proplist (cdr elt))
          (let ((prop (unidata-prop-prop proplist))
                (index (unidata-prop-index proplist))
                (generator (unidata-prop-generator proplist))
                (docstring (unidata-prop-docstring proplist))
                (describer (unidata-prop-describer proplist))
                (default-value (unidata-prop-default proplist))
                (val-list (unidata-prop-val-list proplist))
                table)
            (setq table (funcall generator prop index default-value val-list))
            (when describer
              (unless (subrp (symbol-function describer))
                (unidata--ensure-compiled describer)
                (setq describer (symbol-function describer)))
              (set-char-table-extra-slot table 3 describer))
            (insert (format "(define-char-code-property '%S\n  %S\n  %S)\n"
                            prop table docstring))))
        (insert ";; Local Variables:\n"
                ";; coding: utf-8\n"
                ";; version-control: never\n"
                ";; no-byte-compile: t\n"
                ";; no-update-autoloads: t\n"
                ";; End:\n\n"
                (format ";; %s ends here\n" basename)))))
  (or noninteractive (message "Generating %s...done" file)))

(defun unidata-gen-charprop (&optional charprop-file)
  (or charprop-file (setq charprop-file (pop command-line-args-left)))
  (with-temp-file charprop-file
    (insert ";; Automatically generated by unidata-gen.el.\n"
            ";; See the admin/unidata/ directory in the Emacs sources.\n")
    (dolist (elt unidata-file-alist)
      (dolist (proplist (cdr elt))
	(insert (format "(define-char-code-property '%S %S\n  %S)\n"
			(unidata-prop-prop proplist) (car elt)
			(unidata-prop-docstring proplist)))))
    (or noninteractive (message "Writing %s..." charprop-file))
    (insert "\n"
	    "(provide 'charprop)\n"
	    "\n"
	    ";; Local Variables:\n"
	    ";; coding: utf-8\n"
	    ";; version-control: never\n"
	    ";; no-byte-compile: t\n"
	    ";; no-update-autoloads: t\n"
	    ";; End:\n\n"
	    (format ";; %s ends here\n"
		    (file-name-nondirectory charprop-file)))))



;;; unidata-gen.el ends here
