;;; mule-util.el --- Utility functions for mulitilingual environment (mule)

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

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
TYPE should be `list' or `vector'.
Multibyte characters are conserned."
  (or (eq type 'list) (eq type 'vector)
      (error "Invalid type: %s" type))
  (let* ((len (length string))
	 (i 0)
	 l ch)
    (while (< i len)
      (setq ch (sref string i))
      (setq l (cons ch l))
      (setq i (+ i (char-bytes ch))))
    (setq l (nreverse l))
    (if (eq type 'list)
	l
      (vconcat l))))

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
  (let* ((str (cond ((stringp obj) obj)
		    ((integerp obj) (char-to-string obj))
		    (t (error
			"Invalid argument (should be string or character): %s"
			obj))))
	 (string-len (length string))
	 (len (length str))
	 (i 0))
    (while (and (< i len) (< idx string-len))
      (aset string idx (aref str i))
      (setq idx (1+ idx) i (1+ i)))
    string))

;;;###autoload
(defun truncate-string-to-width (str width &optional start-column padding)
  "Truncate string STR to fit in WIDTH columns.
Optional 1st arg START-COLUMN if non-nil specifies the starting column.
Optional 2nd arg PADDING if non-nil, space characters are padded at
the head and tail of the resulting string to fit in WIDTH if necessary.
If PADDING is nil, the resulting string may be narrower than WIDTH."
  (or start-column
      (setq start-column 0))
  (let ((len (length str))
	(idx 0)
	(column 0)
	(head-padding "") (tail-padding "")
	ch last-column last-idx from-idx)
    (condition-case nil
	(while (< column start-column)
	  (setq ch (sref str idx)
		column (+ column (char-width ch))
		idx (+ idx (char-bytes ch))))
      (args-out-of-range (setq idx len)))
    (if (< column start-column)
	(if padding (make-string width ?\ ) "")
      (if (and padding (> column start-column))
	  (setq head-padding (make-string (- column start-column) ?\ )))
      (setq from-idx idx)
      (condition-case nil
	  (while (< column width)
	    (setq last-column column
		  last-idx idx
		  ch (sref str idx)
		  column (+ column (char-width ch))
		  idx (+ idx (char-bytes ch))))
	(args-out-of-range (setq idx len)))
      (if (> column width)
	  (setq column last-column idx last-idx))
      (if (and padding (< column width))
	  (setq tail-padding (make-string (- width column) ?\ )))
      (setq str (substring str from-idx idx))
      (if padding
	  (concat head-padding str tail-padding)
	str))))

;;; For backward compatiblity ...
;;;###autoload
(defalias 'truncate-string 'truncate-string-to-width)
(make-obsolete 'truncate-string 'truncate-string-to-width)

;;; Nested alist handler.  Nested alist is alist whose elements are
;;; also nested alist.

;;;###autoload
(defsubst nested-alist-p (obj)
  "Return t if OBJ is a nesetd alist.

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
Optional 4th arg LEN non-nil means the firlst LEN elements in KEYSEQ
 is considered.
Optional argument BRANCHES if non-nil is branches for a keyseq
longer than KEYSEQ.
See the documentation of `nested-alist-p' for more detail."
  (or (nested-alist-p alist)
      (error "Invalid arguement %s" alist))
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
      (error "invalid arguement %s" alist))
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
(defun set-coding-system-alist (target-type regexp coding-system
					    &optional operation)
  "Update `coding-system-alist' according to the arguments.
TARGET-TYPE specifies a type of the target: `file', `process', or `network'.
  TARGET-TYPE tells which slots of coding-system-alist should be affected.
  If `file', it affects slots for insert-file-contents and write-region.
  If `process', it affects slots for call-process, call-process-region, and
    start-process.
  If `network', it affects a slot for open-network-process.
REGEXP is a regular expression matching a target of I/O operation.
CODING-SYSTEM is a coding system to perform code conversion
  on the I/O operation, or a cons of coding systems for decoding and
  encoding respectively, or a function symbol which returns the cons.
Optional arg OPERATION if non-nil specifies directly one of slots above.
  The valid value is: insert-file-contents, write-region,
  call-process, call-process-region, start-process, or open-network-stream.
If OPERATION is specified, TARGET-TYPE is ignored.
See the documentation of `coding-system-alist' for more detail."
  (or (stringp regexp)
      (error "Invalid regular expression: %s" regexp))
  (or (memq target-type '(file process network))
      (error "Invalid target type: %s" target-type))
  (if (symbolp coding-system)
      (if (not (fboundp coding-system))
	  (progn
	    (check-coding-system coding-system)
	    (setq coding-system (cons coding-system coding-system))))
    (check-coding-system (car coding-system))
    (check-coding-system (cdr coding-system)))
  (let ((op-list (if operation (list operation)
		   (cond ((eq target-type 'file)
			  '(insert-file-contents write-region))
			 ((eq target-type 'process)
			  '(call-process call-process-region start-process))
			 (t		; i.e. (eq target-type network)
			  '(open-network-stream)))))
	slot)
    (while op-list
      (setq slot (assq (car op-list) coding-system-alist))
      (if slot
	  (let ((chain (cdr slot)))
	    (if (catch 'tag
		  (while chain
		    (if (string= regexp (car (car chain)))
			(progn
			  (setcdr (car chain) coding-system)
			  (throw 'tag nil)))
		    (setq chain (cdr chain)))
		  t)
	      (setcdr slot (cons (cons regexp coding-system) (cdr slot)))))
	(setq coding-system-alist
	      (cons (cons (car op-list) (list (cons regexp coding-system)))
		    coding-system-alist)))
      (setq op-list (cdr op-list)))))

;;;###autoload
(defun coding-system-list ()
  "Return a list of all existing coding systems."
  (let (l)
    (mapatoms (lambda (x) (if (get x 'coding-system) (setq l (cons x l)))))
    l))


;;; Composite charcater manipulations.

;;;###autoload
(defun compose-region (start end)
  "Compose all characters in the current region into one composite character.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (save-excursion
    (let ((str (buffer-substring start end)))
      (goto-char start)
      (delete-region start end)
      (insert (compose-string str)))))

;;;###autoload
(defun decompose-region (start end)
  "Decompose all composite characters in the current region.
Composite characters are broken up into individual components.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (let ((enable-multibyte-characters nil)
	  ;; This matches the whole bytes of single composite character.
	  (re-cmpchar "\200[\240-\377]+")
	  p ch str)
      (while (re-search-forward re-cmpchar nil t)
	(setq str (buffer-substring (match-beginning 0) (match-end 0)))
	(delete-region (match-beginning 0) (match-end 0))
	(insert (decompose-composite-char (string-to-char str)))))))

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
(defun compose-chars-component (ch)
  (if (< ch 128)
      (format "\240%c" (+ ch 128))
    (let ((str (char-to-string ch)))
      (if (cmpcharp ch)
	  (if (/= (aref str 1) ?\xFF)
	      (error "Char %c can't be composed" ch)
	    (substring str 2))
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
Each argument is a character (including a composite chararacter)
or a composition rule.
A composition rule has the form \(GLOBAL-REF-POINT . NEW-REF-POINT).
See the documentation of `reference-point-alist' for more detail."
  (if (= (length args) 0)
      (char-to-string first-component)
    (let* ((with-rule (consp (car args)))
	   (str (if with-rule (concat (vector leading-code-composition ?\xFF))
		  (char-to-string leading-code-composition))))
      (setq str (concat str (compose-chars-component first-component)))
      (while args
	(if with-rule
	    (progn
	      (if (not (consp (car args)))
		  (error "Invalid composition rule: %s" (car args)))
	      (setq str (concat str (compose-chars-rule (car args))
				(compose-chars-component (car (cdr args))))
		    args (cdr (cdr args))))
	  (setq str (concat str (compose-chars-component (car args)))
		args (cdr args))))
      str)))

;;;###autoload
(defun decompose-composite-char (char &optional type with-composition-rule)
  "Convert composite character CHAR to a string containing components of CHAR.
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
	   (apply 'concat-chars l))
	  ((eq type 'list)
	   l)
	  (t				; i.e. TYPE is vector
	   (vconcat l)))))

;;; mule-util.el ends here
