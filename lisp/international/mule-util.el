;;; mule-util.el --- Utility functions for mulitilingual environment (mule)

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: mule, multilingual

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

;;; String manipulations while paying attention to multibyte
;;; characters.

;;;###autoload
(defun string-to-sequence (string type)
  "Convert STRING to a sequence of TYPE which contains characters in STRING.
TYPE should be `list' or `vector'."
  (let ((len (length string))
	(i 0)
	val)
    (cond ((eq type 'list)
	   (setq val (make-list len 0))
	   (let ((l val))
	     (while (< i len)
	       (setcar l (aref string i))
	       (setq l (cdr l) i (1+ i)))))
	  ((eq type 'vector)
	   (setq val (make-vector len 0))
	   (while (< i len)
	     (aset val i (aref string i))
	     (setq i (1+ i))))
	  (t
	   (error "Invalid type: %s" type)))
    val))

;;;###autoload
(defsubst string-to-list (string)
  "Return a list of characters in STRING."
  (string-to-sequence string 'list))

;;;###autoload
(defsubst string-to-vector (string)
  "Return a vector of characters in STRING."
  (string-to-sequence string 'vector))

;;;###autoload
(defun store-substring (string idx obj)
  "Embed OBJ (string or character) at index IDX of STRING."
  (if (integerp obj)
      (aset string idx obj)
    (let ((len1 (length obj))
	  (len2 (length string))
	  (i 0))
      (while (< i len1)
	(aset string (+ idx i) (aref obj i))
	(setq i (1+ i)))))
  string)

;;;###autoload
(defun truncate-string-to-width (str end-column &optional start-column padding)
  "Truncate string STR to end at column END-COLUMN.
The optional 3rd arg START-COLUMN, if non-nil, specifies
the starting column; that means to return the characters occupying
columns START-COLUMN ... END-COLUMN of STR.

The optional 4th arg PADDING, if non-nil, specifies a padding character
to add at the end of the result if STR doesn't reach column END-COLUMN,
or if END-COLUMN comes in the middle of a character in STR.
PADDING is also added at the beginning of the result
if column START-COLUMN appears in the middle of a character in STR.

If PADDING is nil, no padding is added in these cases, so
the resulting string may be narrower than END-COLUMN."
  (or start-column
      (setq start-column 0))
  (let ((len (length str))
	(idx 0)
	(column 0)
	(head-padding "") (tail-padding "")
	ch last-column last-idx from-idx)
    (condition-case nil
	(while (< column start-column)
	  (setq ch (aref str idx)
		column (+ column (char-width ch))
		idx (1+ idx)))
      (args-out-of-range (setq idx len)))
    (if (< column start-column)
	(if padding (make-string end-column padding) "")
      (if (and padding (> column start-column))
	  (setq head-padding (make-string (- column start-column) padding)))
      (setq from-idx idx)
      (if (< end-column column)
	  (setq idx from-idx)
	(condition-case nil
	    (while (< column end-column)
	      (setq last-column column
		    last-idx idx
		    ch (aref str idx)
		    column (+ column (char-width ch))
		    idx (1+ idx)))
	  (args-out-of-range (setq idx len)))
	(if (> column end-column)
	    (setq column last-column idx last-idx))
	(if (and padding (< column end-column))
	    (setq tail-padding (make-string (- end-column column) padding))))
      (setq str (substring str from-idx idx))
      (if padding
	  (concat head-padding str tail-padding)
	str))))

;;; For backward compatibility ...
;;;###autoload
(defalias 'truncate-string 'truncate-string-to-width)
(make-obsolete 'truncate-string 'truncate-string-to-width)

;;; Nested alist handler.  Nested alist is alist whose elements are
;;; also nested alist.

;;;###autoload
(defsubst nested-alist-p (obj)
  "Return t if OBJ is a nested alist.

Nested alist is a list of the form (ENTRY . BRANCHES), where ENTRY is
any Lisp object, and BRANCHES is a list of cons cells of the form
(KEY-ELEMENT . NESTED-ALIST).

You can use a nested alist to store any Lisp object (ENTRY) for a key
sequence KEYSEQ, where KEYSEQ is a sequence of KEY-ELEMENT.  KEYSEQ
can be a string, a vector, or a list."
  (and obj (listp obj) (listp (cdr obj))))

;;;###autoload
(defun set-nested-alist (keyseq entry alist &optional len branches)
  "Set ENTRY for KEYSEQ in a nested alist ALIST.
Optional 4th arg LEN non-nil means the first LEN elements in KEYSEQ
 is considered.
Optional argument BRANCHES if non-nil is branches for a keyseq
longer than KEYSEQ.
See the documentation of `nested-alist-p' for more detail."
  (or (nested-alist-p alist)
      (error "Invalid argument %s" alist))
  (let ((islist (listp keyseq))
	(len (or len (length keyseq)))
	(i 0)
	key-elt slot)
    (while (< i len)
      (if (null (nested-alist-p alist))
	  (error "Keyseq %s is too long for this nested alist" keyseq))
      (setq key-elt (if islist (nth i keyseq) (aref keyseq i)))
      (setq slot (assoc key-elt (cdr alist)))
      (if (null slot)
	  (progn
	    (setq slot (cons key-elt (list t)))
	    (setcdr alist (cons slot (cdr alist)))))
      (setq alist (cdr slot))
      (setq i (1+ i)))
    (setcar alist entry)
    (if branches
	(if (cdr alist)
	    (error "Can't set branches for keyseq %s" keyseq)
	  (setcdr alist branches)))))

;;;###autoload
(defun lookup-nested-alist (keyseq alist &optional len start nil-for-too-long)
  "Look up key sequence KEYSEQ in nested alist ALIST.  Return the definition.
Optional 1st argument LEN specifies the length of KEYSEQ.
Optional 2nd argument START specifies index of the starting key.
The returned value is normally a nested alist of which
car part is the entry for KEYSEQ.
If ALIST is not deep enough for KEYSEQ, return number which is
 how many key elements at the front of KEYSEQ it takes
 to reach a leaf in ALIST.
Optional 3rd argument NIL-FOR-TOO-LONG non-nil means return nil
 even if ALIST is not deep enough."
  (or (nested-alist-p alist)
      (error "invalid argument %s" alist))
  (or len
      (setq len (length keyseq)))
  (let ((i (or start 0)))
    (if (catch 'lookup-nested-alist-tag
	  (if (listp keyseq)
	      (while (< i len)
		(if (setq alist (cdr (assoc (nth i keyseq) (cdr alist))))
		    (setq i (1+ i))
		  (throw 'lookup-nested-alist-tag t))))
	  (while (< i len)
	    (if (setq alist (cdr (assoc (aref keyseq i) (cdr alist))))
		(setq i (1+ i))
	      (throw 'lookup-nested-alist-tag t))))
	;; KEYSEQ is too long.
	(if nil-for-too-long nil i)
      alist)))


;; Coding system related functions.

;;;###autoload
(defun coding-system-eol-type-mnemonic (coding-system)
  "Return the string indicating end-of-line format of CODING-SYSTEM."
  (let* ((eol-type (coding-system-eol-type coding-system))
	 (val (cond ((vectorp eol-type) eol-mnemonic-undecided)
		    ((eq eol-type 0) eol-mnemonic-unix)
		    ((eq eol-type 1) eol-mnemonic-dos)
		    ((eq eol-type 2) eol-mnemonic-mac)
		    (t "-"))))
    (if (stringp val)
	val
      (char-to-string val))))

;;;###autoload
(defun coding-system-post-read-conversion (coding-system)
  "Return the value of CODING-SYSTEM's post-read-conversion property."
  (coding-system-get coding-system 'post-read-conversion))

;;;###autoload
(defun coding-system-pre-write-conversion (coding-system)
  "Return the value of CODING-SYSTEM's pre-write-conversion property."
  (coding-system-get coding-system 'pre-write-conversion))

;;;###autoload
(defun coding-system-translation-table-for-decode (coding-system)
  "Return the value of CODING-SYSTEM's translation-table-for-decode property."
  (coding-system-get coding-system 'translation-table-for-decode))

;;;###autoload
(defun coding-system-translation-table-for-encode (coding-system)
  "Return the value of CODING-SYSTEM's translation-table-for-encode property."
  (coding-system-get coding-system 'translation-table-for-encode))

;;;###autoload
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

;;;###autoload
(defmacro detect-coding-with-priority (from to priority-list)
  "Detect a coding system of the text between FROM and TO with PRIORITY-LIST.
PRIORITY-LIST is an alist of coding categories vs the corresponding
coding systems ordered by priority."
  `(unwind-protect
       (let* ((prio-list ,priority-list)
	      (coding-category-list coding-category-list)
	      ,@(mapcar (function (lambda (x) (list x x)))
			coding-category-list))
	 (mapcar (function (lambda (x) (set (car x) (cdr x))))
		 prio-list)
	 (set-coding-priority (mapcar (function (lambda (x) (car x)))
				      prio-list))
	 (detect-coding-region ,from ,to))
     ;; We must restore the internal database.
     (set-coding-priority coding-category-list)
     (update-coding-systems-internal)))

;;;###autoload
(defun detect-coding-with-language-environment (from to lang-env)
  "Detect a coding system of the text between FROM and TO with LANG-ENV.
The detection takes into account the coding system priorities for the
language environment LANG-ENV."
  (let ((coding-priority (get-language-info lang-env 'coding-priority)))
    (if coding-priority
	(detect-coding-with-priority
	 from to
	 (mapcar (function (lambda (x)
			     (cons (coding-system-get x 'coding-category) x)))
		 coding-priority))
      (detect-coding-region from to))))


;;; Composite character manipulations.

;;;###autoload
(defun compose-region (start end)
  "Compose all characters in the current region into one composite character.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (save-excursion
    (let ((str (buffer-substring start end)))
      (goto-char start)
      (insert (compose-string str))
      (delete-char (- end start)))))

;;;###autoload
(defun decompose-region (start end)
  "Decompose all composite characters in the current region.
Composite characters are broken up into individual components.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (let ((buf (current-buffer))
	(cmpchar-head (char-to-string leading-code-composition)))
    (with-temp-buffer
      (insert-buffer-substring buf start end)
      (set-buffer-multibyte nil)
      (goto-char (point-min))
      (while (search-forward cmpchar-head nil t)
	(if (looking-at "[\240-\377][\240-\377][\240-\377][\240-\377]+")
	    (let* ((from (1- (point)))
		   (to (match-end 0))
		   (str (string-as-multibyte (buffer-substring from to))))
	      (if (cmpcharp (string-to-char str))
		  (progn
		    (delete-region from to)
		    (insert (string-as-unibyte (decompose-string str))))
		(goto-char to)))))
      (set-buffer-multibyte t)
      (let ((tempbuf (current-buffer)))
	(save-excursion
	  (set-buffer buf)
	  (goto-char start)
	  (delete-region start end)
	  (insert-buffer-substring tempbuf))))))

;;;###autoload
(defun decompose-string (string)
  "Decompose all composite characters in STRING."
  (let ((len (length string))
	(idx 0)
	(i 0)
	(str-list nil)
	ch)
    (while (< idx len)
      (setq ch (aref string idx))
      (if (>= ch min-composite-char)
	  (progn
	    (if (> idx i)
		(setq str-list (cons (substring string i idx) str-list)))
	    (setq str-list (cons (decompose-composite-char ch) str-list))
	    (setq i (1+ idx))))
      (setq idx (1+ idx)))
    (if (not str-list)
	(copy-sequence string)
      (if (> idx i)
	  (setq str-list (cons (substring string i idx) str-list)))
      (apply 'concat (nreverse str-list)))))

;;;###autoload
(defconst reference-point-alist
  '((tl . 0) (tc . 1) (tr . 2)
    (ml . 3) (mc . 4) (mr . 5)
    (bl . 6) (bc . 7) (br . 8)
    (top-left . 0) (top-center . 1) (top-right . 2)
    (mid-left . 3) (mid-center . 4) (mid-right . 5)
    (bottom-left . 6) (bottom-center . 7) (bottom-right . 8)
    (0 . 0) (1 . 1) (2 . 2)
    (3 . 3) (4 . 4) (5 . 5)
    (6 . 6) (7 . 7) (8 . 8))
  "Alist of reference point symbols vs reference point codes.
A reference point symbol is to be used to specify a composition rule
while making a composite character by the function `compose-chars'
(which see).

Meanings of reference point codes are as follows:

    0----1----2 <-- ascent	0:tl or top-left
    |         |			1:tc or top-center
    |         |			2:tr or top-right
    |         |			3:ml or mid-left
    |    4 <--+---- center	4:mc or mid-center
    |         |			5:mr or mid-right
--- 3         5 <-- baseline	6:bl or bottom-left
    |         |			7:bc or bottom-center
    6----7----8 <-- descent	8:br or bottom-right

Reference point symbols are to be used to specify composition rule of
the form \(GLOBAL-REF-POINT . NEW-REF-POINT), where GLOBAL-REF-POINT
is a reference point in the overall glyphs already composed, and
NEW-REF-POINT is a reference point in the new glyph to be added.

For instance, if GLOBAL-REF-POINT is 8 and NEW-REF-POINT is 1, the
overall glyph is updated as follows:

    +-------+--+ <--- new ascent
    |       |  |
    | global|  |
    | glyph |  |
--- |       |  | <--- baseline (doesn't change)
    +----+--+--+
    |    | new |
    |    |glyph|
    +----+-----+ <--- new descent
")

;; Return a string for char CH to be embedded in multibyte form of
;; composite character.
;;;###autoload
(defun compose-chars-component (ch)
  (if (< ch 128)
      (format "\240%c" (+ ch 128))
    (let ((str (string-as-unibyte (char-to-string ch))))
      (if (cmpcharp ch)
	  (if (= (aref str 1) ?\xFF)
	      (error "Can't compose a rule-based composition character")
	    (substring str (if (= (aref str 1) ?\xFF) 2 1)))
	(aset str 0 (+ (aref str 0) ?\x20))
	str))))

;; Return a string for composition rule RULE to be embedded in
;; multibyte form of composite character.
(defsubst compose-chars-rule (rule)
  (char-to-string (+ ?\xA0
		     (* (cdr (assq (car rule) reference-point-alist)) 9)
		     (cdr (assq (cdr rule) reference-point-alist)))))

;;;###autoload
(defun compose-chars (first-component &rest args)
  "Return one char string composed from the arguments.
For relative composition, each argument should be a non-composition character
or a relative-composition character.
For rule-based composition, Nth (where N is odd) argument should be
a non-composition character or a rule-based-composition character,
and Mth (where M is even) argument should be a composition rule.
A composition rule has the form \(GLOBAL-REF-POINT . NEW-REF-POINT).
See the documentation of `reference-point-alist' for more detail."
  (if (= (length args) 0)
      (char-to-string first-component)
    (let* ((with-rule (consp (car args)))
	   (str (if (cmpcharp first-component)
		    (string-as-unibyte (char-to-string first-component))
		  (if with-rule
		      (concat (vector leading-code-composition ?\xFF)
			      (compose-chars-component first-component))
		    (concat (char-to-string leading-code-composition)
			    (compose-chars-component first-component))))))
      (if (and (cmpcharp first-component)
	       (eq with-rule (/= (aref str 1) ?\xFF)))
	  (error "%s-compostion-character is not allowed in %s composition: %c"
		 (if with-rule "relative" "rule-based")
		 (if with-rule "rule-based" "relative")
		 first-component))
      (while args
	(if with-rule
	    (setq str (concat str (compose-chars-rule (car args)))
		  args (cdr args)))
	(if (cmpcharp (car args))
	    (let ((cmp-str (string-as-unibyte (char-to-string (car args)))))
	      (if (eq with-rule (/= (aref cmp-str 1) ?\xFF))
		  (error "%s-compostion-character is not allowed in %s composition: %c"
			 (if with-rule "relative" "rule-based")
			 (if with-rule "rule-based" "relative")
			 (car args)))
	      (setq str (concat str (substring cmp-str
					       (if with-rule 2 1)))))
	  (setq str (concat str (compose-chars-component (car args)))))
	(setq args (cdr args)))
      (string-as-multibyte str))))

;;;###autoload
(defun decompose-composite-char (char &optional type with-composition-rule)
  "Convert composite character CHAR to a sequence of the components.
Optional 1st arg TYPE specifies the type of sequence returned.
It should be `string' (default), `list', or `vector'.
Optional 2nd arg WITH-COMPOSITION-RULE non-nil means the returned
sequence contains embedded composition rules if any.  In this case, the
order of elements in the sequence is the same as arguments for
`compose-chars' to create CHAR.
If TYPE is omitted or is `string', composition rules are omitted
even if WITH-COMPOSITION-RULE is t."
  (or type
      (setq type 'string))
  (let* ((len (composite-char-component-count char))
	 (i (1- len))
	 l)
    (setq with-composition-rule (and with-composition-rule
				    (not (eq type 'string))
				    (composite-char-composition-rule-p char)))
    (while (> i 0)
      (setq l (cons (composite-char-component char i) l))
      (if  with-composition-rule
	  (let ((rule (- (composite-char-composition-rule char i) ?\xA0)))
	    (setq l (cons (cons (/ rule 9) (% rule 9)) l))))
      (setq i (1- i)))
    (setq l (cons (composite-char-component char 0) l))
    (cond ((eq type 'string)
	   (apply 'string l))
	  ((eq type 'list)
	   l)
	  (t				; i.e. TYPE is vector
	   (vconcat l)))))

;;; mule-util.el ends here
