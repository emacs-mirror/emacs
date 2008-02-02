;; unidata-gen.el -- Create files containing character property data.
;; Copyright (C) 2005
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

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

;; SPECIAL NOTICE
;;
;;   This file must be byte-compilable/loadable by `temacs' and also
;;   the entry function `unidata-gen-files' must be runnable by
;;   `temacs'.

;; FILES TO BE GENERATED
;;
;;   The entry function `unidata-gen-files' generates these files in
;;   the current directory.
;;
;;   charprop.el
;;	It contains a series of forms of this format:
;;	  (char-code-property-register PROP FILE)
;;	where PROP is a symbol representing a character property
;;	(name, generic-category, etc), and FILE is a name of one of
;;	the following files.
;;
;;   uni-name.el, uni-cat.el, uni-comb.el, uni-bidi.el
;;	It contains a single form of this format:
;;	  (char-code-property-register PROP CHAR-TABLE)
;;	where PROP is the same as above, and CHAR-TABLE is a
;;	char-table containing property values in a compressed format.
;;
;;   When they are installed in .../lisp/international/, the file
;;   "charprop.el" is preloaded in loadup.el.  The other files are
;;   automatically loaded when the functions `get-char-code-property'
;;   and `put-char-code-property' are called.
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
;;   store that value for them.  Otherwise, compress values for
;;   succeeding 128 characters into a single string and store it as a
;;   value for those characters.  The way of compression depends on a
;;   property.  See the section "SIMPLE TABLE", "RUN-LENGTH TABLE",
;;   and "WORD-LIST TABLE".

;;   The char table has four extra slots:
;;      1st: property symbol
;;	2nd: function to call to get a property value
;;	3nd: function to call to put a property value
;;	4th: function to call to get a description of a property value
;;	5th: data referred by the above functions

;; List of elements of this form:
;;   (CHAR-or-RANGE PROP1 PROP2 ... PROPn)
;; CHAR-or-RANGE: a character code or a cons of character codes
;; PROPn: string representing the nth property value

(defvar unidata-list nil)

(defun unidata-setup-list (unidata-text-file)
  (let* ((table (list nil))
	 (tail table)
	 (block-names '(("^<CJK Ideograph" . CJK\ IDEOGRAPH)
			("^<Hangul Syllable" . HANGUL\ SYLLABLE)
			("^<.*Surrogate" . nil)
			("^<.*Private Use" . PRIVATE\ USE)))
	 val char name)
    (or (file-readable-p unidata-text-file)
	(error "File not readable: %s" unidata-text-file))
    (with-temp-buffer
      (insert-file-contents unidata-text-file)
      (goto-char (point-min))
      (condition-case nil
	  (while t
	    (setq val (read (current-buffer))
		  char (car val)
		  name (cadr val))

	    ;; Check this kind of block.
	    ;;   4E00;<CJK Ideograph, First>;Lo;0;L;;;;;N;;;;;
	    ;;   9FA5;<CJK Ideograph, Last>;Lo;0;L;;;;;N;;;;;
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
		  (if (not name)
		      ;; As this is a surrogate pair range, ignore it.
		      (setq val nil)
		    (setcar val (cons first char))
		    (setcar (cdr val) name))))

	    (when val
	      (setcdr tail (list val))
	      (setq tail (cdr tail))))
	(error nil)))
    (setq unidata-list (cdr table))))

;; Alist of this form:
;;   (PROP INDEX GENERATOR FILENAME)
;; PROP: character property
;; INDEX: index to each element of unidata-list for PROP
;; GENERATOR: function to generate a char-table
;; FILENAME: filename to store the char-table
;; DESCRIBER: function to call to get a description string of property value

(defconst unidata-prop-alist
  '((name
     1 unidata-gen-table-name "uni-name.el"
     "Unicode character name.
Property value is a string.")
    (general-category
     2 unidata-gen-table-symbol "uni-category.el"
     "Unicode general category.
Property value is one of the following symbols:
  Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pc, Pd, Ps, Pe, Pi, Pf, Po,
  Sm, Sc, Sk, So, Zs, Zl, Zp, Cc, Cf, Cs, Co, Cn"
     unidata-describe-general-category)
    (canonical-combining-class
     3 unidata-gen-table-integer "uni-combining.el"
     "Unicode canonical combining class.
Property value is an integer."
     unidata-describe-canonical-combining-class)
    (bidi-class
     4 unidata-gen-table-symbol "uni-bidi.el"
     "Unicode bidi class.
Property value is one of the following symbols:
  L, LRE, LRO, R, AL, RLE, RLO, PDF, EN, ES, ET,
  AN, CS, NSM, BN, B, S, WS, ON"
     unidata-describe-bidi-class)
    (decomposition
     5 unidata-gen-table-decomposition "uni-decomposition.el"
     "Unicode decomposition mapping.
Property value is a list of characters.  The first element may be
one of these symbols representing compatibility formatting tag:
  <font>, <noBreak>, <initial>, <medial>, <final>, <isolated>, <circle>,
  <super>, <sub>, <vertical>, <wide>, <narrow>, <small>, <square>, <fraction>,
  <compat>"
     unidata-describe-decomposition)
    (decimal-digit-value
     6 unidata-gen-table-integer "uni-decimal.el"
     "Unicode numeric value (decimal digit).
Property value is an integer.")
    (digit-value
     7 unidata-gen-table-integer "uni-digit.el"
     "Unicode numeric value (digit).
Property value is an integer.")
    (numeric-value
     8 unidata-gen-table-symbol "uni-numeric.el"
     "Unicode numeric value (numeric).
Property value is an symbol.")
    (mirrored
     9 unidata-gen-table-symbol "uni-mirrored.el"
     "Unicode bidi mirrored flag.
Property value is a symbol `Y' or `N'.")
    (old-name
     10 unidata-gen-table-name "uni-old-name.el"
     "Unicode old names as published in Unicode 1.0.
Property value is a string.")
    (iso-10646-comment
     11 unidata-gen-table-name "uni-comment.el"
     "Unicode ISO 10646 comment.
Property value is a string.")
    (uppercase
     12 unidata-gen-table-character "uni-uppercase.el"
     "Unicode simple uppercase mapping.
Property value is a character."
     string)
    (lowercase
     13 unidata-gen-table-character "uni-lowercase.el"
     "Unicode simple lowercase mapping.
Property value is a character."
     string)
    (titlecase
     14 unidata-gen-table-character "uni-titlecase.el"
     "Unicode simple titlecase mapping.
Property value is a character."
     string)))

;; Functions to access the above data.
(defsubst unidata-prop-index (prop) (nth 1 (assq prop unidata-prop-alist)))
(defsubst unidata-prop-generator (prop) (nth 2 (assq prop unidata-prop-alist)))
(defsubst unidata-prop-file (prop) (nth 3 (assq prop unidata-prop-alist)))
(defsubst unidata-prop-docstring (prop) (nth 4 (assq prop unidata-prop-alist)))
(defsubst unidata-prop-describer (prop) (nth 5 (assq prop unidata-prop-alist)))


;; SIMPLE TABLE
;;
;; If the type of character property value is character, and the
;; values of succeeding character codes are usually different, we use
;; a char-table described here to store such values.
;;
;; If succeeding 128 characters has no property, a char-table has the
;; symbol t is for them.  Otherwise a char-table has a string of the
;; following format for them.
;;
;; The first character of the string is FIRST-INDEX.
;; The Nth (N > 0) character of the string is a property value of the
;; character (BLOCK-HEAD + FIRST-INDEX + N - 1), where BLOCK-HEAD is
;; the first of the characters in the block.
;;
;; The 4th extra slot of a char-table is nil.

(defun unidata-get-character (char val table)
  (cond
   ((characterp val)
    val)

   ((stringp val)
    (let* ((len (length val))
	   (block-head (lsh (lsh char -7) 7))
	   (vec (make-vector 128 nil))
	   (first-index (aref val 0)))
      (dotimes (i (1- len))
	(let ((elt (aref val (1+ i))))
	  (if (> elt 0)
	      (aset vec (+ first-index i) elt))))
      (dotimes (i 128)
	(aset table (+ block-head i) (aref vec i)))
      (aref vec (- char block-head))))))

(defun unidata-put-character (char val table)
  (or (characterp val)
      (not val)
      (error "Not an character nor nil: %S" val))
  (let ((current-val (aref table char)))
    (unless (eq current-val val)
      (if (stringp current-val)
	  (funcall (char-table-extra-slot table 1) char current-val table))
      (aset table char val))))

(defun unidata-gen-table-character (prop)
  (let ((table (make-char-table 'char-code-property-table))
	(prop-idx (unidata-prop-index prop))
	(vec (make-vector 128 0))
	(tail unidata-list)
	elt range val idx slot)
    (set-char-table-range table (cons 0 (max-char)) t)
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
	(let* ((start (lsh (lsh range -7) 7))
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
	    (let ((str (string first-index))
		  c)
	      (while (<= first-index last-index)
		(setq str (format "%s%c"  str (or (aref vec first-index) 0))
		      first-index (1+ first-index)))
	      (set-char-table-range table (cons start limit) str))))))

    (set-char-table-extra-slot table 0 prop)
    (byte-compile 'unidata-get-character)
    (byte-compile 'unidata-put-character)
    (set-char-table-extra-slot table 1 (symbol-function 'unidata-get-character))
    (set-char-table-extra-slot table 2 (symbol-function 'unidata-put-character))

    table))



;; RUN-LENGTH TABLE
;;
;; If the type of character property value is symbol, integer,
;; boolean, or character, we use a char-table described here to store
;; the values.
;;
;; The 4th extra slot is a vector of property values (VAL-TABLE), and
;; values for succeeding 128 characters are encoded into this
;; character sequence:
;;   ( VAL-CODE RUN-LENGTH ? ) +
;; where:
;;   VAL-CODE (0..127):
;;     (VAL-CODE - 1) is an index into VAL-TABLE.
;;     The value 0 means no-value.
;;   RUN-LENGTH (130..255):
;;     (RUN-LENGTH - 128) specifies how many characters have the same
;;     value.  If omitted, it means 1.


;; Return a symbol-type character property value of CHAR.  VAL is the
;; current value of (aref TABLE CHAR).

(defun unidata-get-symbol (char val table)
  (let ((val-table (char-table-extra-slot table 4)))
    (cond ((symbolp val)
	   val)
	  ((stringp val)
	   (let ((first-char (lsh (lsh char -7) 7))
		 (str val)
		 (len (length val))
		 (idx 0)
		 this-val count)
	     (set-char-table-range table (cons first-char (+ first-char 127))
				   nil)
	     (while (< idx len)
	       (setq val (aref str idx) idx (1+ idx)
		     count (if (< idx len) (aref str idx) 1))
	       (setq val (and (> val 0) (aref val-table (1- val)))
		     count (if (< count 128)
			       1
			     (prog1 (- count 128) (setq idx (1+ idx)))))
	       (dotimes (i count)
		 (if val
		     (aset table first-char val))
		 (if (= first-char char)
		     (setq this-val val))
		 (setq first-char (1+ first-char))))
	     this-val))
	  ((> val 0)
	   (aref val-table (1- val))))))
	
;; Return a integer-type character property value of CHAR.  VAL is the
;; current value of (aref TABLE CHAR).

(defun unidata-get-integer (char val table)
  (let ((val-table (char-table-extra-slot table 4)))
    (cond ((integerp val)
	   val)
	  ((stringp val)
	   (let ((first-char (lsh (lsh char -7) 7))
		 (str val)
		 (len (length val))
		 (idx 0)
		 this-val count)
	     (while (< idx len)
	       (setq val (aref str idx) idx (1+ idx)
		     count (if (< idx len) (aref str idx) 1))
	       (setq val (and (> val 0) (aref val-table (1- val)))
		     count (if (< count 128)
			       1
			     (prog1 (- count 128) (setq idx (1+ idx)))))
	       (dotimes (i count)
		 (aset table first-char val)
		 (if (= first-char char)
		     (setq this-val val))
		 (setq first-char (1+ first-char))))
	     this-val)))))

;; Store VAL (symbol) as a character property value of CHAR in TABLE.

(defun unidata-put-symbol (char val table)
  (or (symbolp val)
      (error "Not a symbol: %S" val))
  (let ((current-val (aref table char)))
    (unless (eq current-val val)
      (if (stringp current-val)
	  (funcall (char-table-extra-slot table 1) char current-val table))
      (aset table char val))))

;; Store VAL (integer) as a character property value of CHAR in TABLE.

(defun unidata-put-integer (char val table)
  (or (integerp val)
      (not val)
      (error "Not an integer nor nil: %S" val))
  (let ((current-val (aref table char)))
    (unless (eq current-val val)
      (if (stringp current-val)
	  (funcall (char-table-extra-slot table 1) char current-val table))
      (aset table char val))))

;; Encode the character property value VAL into an integer value by
;; VAL-LIST.  By side effect, VAL-LIST is modified.
;; VAL-LIST has this form:
;;   (t (VAL1 . VAL-CODE1) (VAL2 . VAL-CODE2) ...)
;; If VAL is one of VALn, just return VAL-CODEn.  Otherwise,
;; VAL-LIST is modified to this:
;;   (t (VAL . (1+ VAL-CODE1)) (VAL1 . VAL-CODE1) (VAL2 . VAL-CODE2) ...)

(defun unidata-encode-val (val-list val)
  (let ((slot (assq val val-list))
	val-code)
    (if slot
	(cdr slot)
      (setq val-code (if (cdr val-list) (1+ (cdr (nth 1 val-list))) 1))
      (setcdr val-list (cons (cons val val-code) (cdr val-list)))
      val-code)))

;; Generate a char-table for the character property PROP.

(defun unidata-gen-table (prop val-func default-value)
  (let ((table (make-char-table 'char-code-property-table))
	(prop-idx (unidata-prop-index prop))
	(val-list (list t))
	(vec (make-vector 128 0))
	tail elt range val val-code idx slot)
    (set-char-table-range table (cons 0 (max-char)) default-value)
    (setq tail unidata-list)
    (while tail
      (setq elt (car tail) tail (cdr tail))
      (setq range (car elt)
	    val (funcall val-func (nth prop-idx elt)))
      (setq val-code (if val (unidata-encode-val val-list val)))
      (if (consp range)
	  (if val-code
	      (set-char-table-range table range val))
	(let* ((start (lsh (lsh range -7) 7))
	       (limit (+ start 127))
	       str count new-val)
	  (fillarray vec 0)
	  (if val-code
	      (aset vec (- range start) val-code))
	  (while (and (setq elt (car tail) range (car elt))
		      (integerp range)
		      (<= range limit))
	    (setq new-val (funcall val-func (nth prop-idx elt)))
	    (if (not (eq val new-val))
		(setq val new-val
		      val-code (if val (unidata-encode-val val-list val))))
	    (if val-code
		(aset vec (- range start) val-code))
	    (setq tail (cdr tail)))
	  (setq str "" val-code -1 count 0)
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
		  (set-char-table-range table (cons start limit) val))
	    (if (= val-code 0)
		(set-char-table-range table (cons start limit) str)
	      (if (> count 2)
		  (setq str (concat str (string val-code (+ count 128))))
		(if (= count 2)
		    (setq str (concat str (string val-code val-code)))
		  (setq str (concat str (string val-code)))))
	      (set-char-table-range table (cons start limit) str))))))

    (setq val-list (nreverse (cdr val-list)))
    (set-char-table-extra-slot table 0 prop) 
    (set-char-table-extra-slot table 4 (vconcat (mapcar 'car val-list)))
    table))

(defun unidata-gen-table-symbol (prop)
  (let ((table (unidata-gen-table prop
				  #'(lambda (x) (and (> (length x) 0)
						     (intern x)))
				  0)))
    (byte-compile 'unidata-get-symbol)
    (byte-compile 'unidata-put-symbol)
    (set-char-table-extra-slot table 1 (symbol-function 'unidata-get-symbol))
    (set-char-table-extra-slot table 2 (symbol-function 'unidata-put-symbol))
    table))

(defun unidata-gen-table-integer (prop)
  (let ((table (unidata-gen-table prop
				  #'(lambda (x) (and (> (length x) 0)
						     (string-to-number x)))
				  t)))
    (byte-compile 'unidata-get-integer)
    (byte-compile 'unidata-put-integer)
    (set-char-table-extra-slot table 1 (symbol-function 'unidata-get-integer))
    (set-char-table-extra-slot table 2 (symbol-function 'unidata-put-integer))
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
      (let* ((first-char (lsh (lsh char -7) 7))
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
	    ((eq sym 'CJK\ COMPATIBILITY\ IDEOGRAPH)
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
   ((consp val)
    val)

   ((stringp val)
    (if (> (aref val 0) 0)
	val
      (let* ((first-char (lsh (lsh char -7) 7))
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
	(aref vec (- char first-char)))))

   ;; Hangul syllable
   ((and (eq val 0) (>= char #xAC00) (<= char #xD7A3))
    ;; SIndex = S (char) - SBase (#xAC00)
    (setq char (- char #xAC00))
    (let (;; L = LBase + SIndex / NCount
	  (L (+ #x1100 (/ char 588)))
	  ;; V = VBase + (SIndex % NCount) * TCount
	  (V (+ #x1161 (/ (% char 588) 28)))
	  ;; T = TBase + SIndex % TCount
	  (T (+ #x11A7 (% char 28))))
      (if (= T #x11A7)
	  (list L V)
	(list L V T))))

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
;; the size.

;; Generate a char-table for character names.

(defun unidata-gen-table-word-list (prop val-func)
  (let ((table (make-char-table 'char-code-property-table))
	(prop-idx (unidata-prop-index prop))
	(word-list (list nil))
	word-table
	block-list block-word-table block-end
	tail elt range val idx slot)
    (set-char-table-range table (cons 0 (max-char)) 0)
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
	(let* ((start (lsh (lsh range -7) 7))
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
		      (let ((slot (assq elt word-list)))
			(if slot
			    (setcdr slot (1+ (cdr slot)))
			  (setcdr word-list
				  (cons (cons elt 1) (cdr word-list))))))))
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
      (if (= len 0)
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

(defun unidata-gen-table-name (prop)
  (let* ((table (unidata-gen-table-word-list prop 'unidata-split-name))
	 (word-tables (char-table-extra-slot table 4)))
    (byte-compile 'unidata-get-name)
    (byte-compile 'unidata-put-name)
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
			  (cons (intern (substring str idx i)) l)
			(cons (string-to-number (substring str idx i) 16) l))
		    idx (1+ i))))
	(if (= (aref str idx) ?<)
	    (setq l (cons (intern (substring str idx len)) l))
	  (setq l (cons (string-to-number (substring str idx len) 16) l)))
	(nreverse l)))))


(defun unidata-gen-table-decomposition (prop)
  (let* ((table (unidata-gen-table-word-list prop 'unidata-split-decomposition))
	 (word-tables (char-table-extra-slot table 4)))
    (byte-compile 'unidata-get-decomposition)
    (byte-compile 'unidata-put-decomposition)
    (set-char-table-extra-slot table 1
			       (symbol-function 'unidata-get-decomposition))
    (set-char-table-extra-slot table 2 
			       (symbol-function 'unidata-put-decomposition))
    (set-char-table-extra-slot table 4 (car word-tables))
    table))



(defun unidata-describe-general-category (val)
  (cdr (assq val
	     '((Lu . "Letter, Uppercase")
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
  (mapconcat #'(lambda (x) (if (symbolp x) (symbol-name x) (string ?' x ?')))
	     val " "))

;; Verify if we can retrieve correct values from the generated
;; char-tables.

(defun unidata-check ()
  (dolist (elt unidata-prop-alist)
    (let* ((prop (car elt))
	   (index (unidata-prop-index prop))
	   (generator (unidata-prop-generator prop))
	   (table (progn 
		    (message "Generating %S table..." prop)
		    (funcall generator prop)))
	   (decoder (char-table-extra-slot table 1))
	   (check #x400))
      (dolist (e unidata-list)
	(let ((char (car e))
	      (val1 (nth index e))
	      val2)
	  (if (and (stringp val1) (= (length val1) 0))
	      (setq val1 nil))
	  (unless (consp char)
	    (setq val2 (funcall decoder char (aref table char) table))
	    (if val1
		(cond ((eq generator 'unidata-gen-table-symbol)
		       (setq val1 (intern val1)))
		      ((eq generator 'unidata-gen-table-integer)
		       (setq val1 (string-to-number val1)))
		      ((eq generator 'unidata-gen-table-character)
		       (setq val1 (string-to-number val1 16)))
		      ((eq generator 'unidata-gen-table-decomposition)
		       (setq val1 (unidata-split-decomposition val1)))))
	    (when (>= char check)
	      (message "%S %04X" prop check)
	      (setq check (+ check #x400)))
	    (or (equal val1 val2)
		(insert (format "> %04X %S\n< %04X %S\n" 
				char val1 char val2)))
	    (sit-for 0)))))))

;; The entry function.  It generates files described in the header
;; comment of this file.

(defun unidata-gen-files (&optional unidata-text-file)
  (or unidata-text-file
      (setq unidata-text-file (car command-line-args-left)
	    command-line-args-left (cdr command-line-args-left)))
  (unidata-setup-list unidata-text-file)
  (let ((coding-system-for-write 'utf-8-unix)
	(charprop-file "charprop.el"))
    (with-temp-file charprop-file
      (insert ";; Automatically generated by unidata-gen.el.\n")
      (dolist (elt unidata-prop-alist)
	(let* ((prop (car elt))
	       (generator (unidata-prop-generator prop))
	       (file (unidata-prop-file prop))
	       (docstring (unidata-prop-docstring prop))
	       (describer (unidata-prop-describer prop))
	       table)
	  ;; Filename in this comment line is extracted by sed in
	  ;; Makefile.
	  (insert (format ";; FILE: %s\n" file))
	  (insert (format "(define-char-code-property '%S %S\n  %S)\n"
			  prop file docstring))
	  (with-temp-file file
	    (message "Generating %s..." file)
	    (setq table (funcall generator prop))
	    (when describer
	      (unless (subrp (symbol-function describer))
		(byte-compile describer)
		(setq describer (symbol-function describer)))
	      (set-char-table-extra-slot table 3 describer))
	    (insert ";; Automatically generated from UnicodeData.txt.\n"
		    (format "(define-char-code-property '%S %S %S)\n"
			    prop table docstring)
		    ";; Local Variables:\n"
		    ";; coding: utf-8\n"
		    ";; no-byte-compile: t\n"
		    ";; End:\n\n"
		    (format ";; %s ends here\n" file)))))
      (message "Writing %s..." charprop-file)
      (insert ";; Local Variables:\n"
	      ";; coding: utf-8\n"
	      ";; no-byte-compile: t\n"
	      ";; End:\n\n"
	      (format ";; %s ends here\n" charprop-file)))))



;; arch-tag: 961c862e-b821-447e-9b8a-bfbab9c2d525
;;; unidata-gen.el ends here
