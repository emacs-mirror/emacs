;;; mule-util.el --- utility functions for mulitilingual environment (mule)

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

;;; Commentary:

;;; Code:

;;; String manipulations while paying attention to multibyte
;;; characters.

;;;###autoload
(defun string-to-sequence (string type)
  "Convert STRING to a sequence of TYPE which contains characters in STRING.
TYPE should be `list' or `vector'."
;;;  (let ((len (length string))
;;;	   (i 0)
;;;	   val)
    (cond ((eq type 'list)
	   ;; Applicable post-Emacs 20.2 and asymptotically ~10 times
	   ;; faster than the code below:
	   (append string nil))
;;; 	   (setq val (make-list len 0))
;;; 	   (let ((l val))
;;; 	     (while (< i len)
;;; 	       (setcar l (aref string i))
;;; 	       (setq l (cdr l) i (1+ i))))))
	  ((eq type 'vector)
	   ;; As above.
	   (vconcat string))
;;; 	   (setq val (make-vector len 0))
;;; 	   (while (< i len)
;;; 	     (aset val i (aref string i))
;;; 	     (setq i (1+ i))))
	  (t
	   (error "Invalid type: %s" type)))
;;;    val)
)
(make-obsolete 'string-to-sequence
	       "Use `string-to-list' or `string-to-vector'"
	       "21.3")

;;;###autoload
(defsubst string-to-list (string)
  "Return a list of characters in STRING."
  (append string nil))

;;;###autoload
(defsubst string-to-vector (string)
  "Return a vector of characters in STRING."
  (vconcat string))

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
(defun truncate-string-to-width (str end-column
				     &optional start-column padding ellipsis)
  "Truncate string STR to end at column END-COLUMN.
The optional 3rd arg START-COLUMN, if non-nil, specifies the starting
column; that means to return the characters occupying columns
START-COLUMN ... END-COLUMN of STR.  Both END-COLUMN and START-COLUMN
are specified in terms of character display width in the current
buffer; see also `char-width'.

The optional 4th arg PADDING, if non-nil, specifies a padding
character (which should have a display width of 1) to add at the end
of the result if STR doesn't reach column END-COLUMN, or if END-COLUMN
comes in the middle of a character in STR.  PADDING is also added at
the beginning of the result if column START-COLUMN appears in the
middle of a character in STR.

If PADDING is nil, no padding is added in these cases, so
the resulting string may be narrower than END-COLUMN.

If ELLIPSIS is non-nil, it should be a string which will replace the
end of STR (including any padding) if it extends beyond END-COLUMN,
unless the display width of STR is equal to or less than the display
width of ELLIPSIS.  If it is non-nil and not a string, then ELLIPSIS
defaults to \"...\"."
  (or start-column
      (setq start-column 0))
  (when (and ellipsis (not (stringp ellipsis)))
    (setq ellipsis "..."))
  (let ((str-len (length str))
	(str-width (string-width str))
	(ellipsis-len (if ellipsis (length ellipsis) 0))
	(ellipsis-width (if ellipsis (string-width ellipsis) 0))
	(idx 0)
	(column 0)
	(head-padding "") (tail-padding "")
	ch last-column last-idx from-idx)
    (condition-case nil
	(while (< column start-column)
	  (setq ch (aref str idx)
		column (+ column (char-width ch))
		idx (1+ idx)))
      (args-out-of-range (setq idx str-len)))
    (if (< column start-column)
	(if padding (make-string end-column padding) "")
      (when (and padding (> column start-column))
	(setq head-padding (make-string (- column start-column) padding)))
      (setq from-idx idx)
      (when (>= end-column column)
	(if (and (< end-column str-width)
		 (> str-width ellipsis-width))
	    (setq end-column (- end-column ellipsis-width))
	  (setq ellipsis ""))
	(condition-case nil
	    (while (< column end-column)
	      (setq last-column column
		    last-idx idx
		    ch (aref str idx)
		    column (+ column (char-width ch))
		    idx (1+ idx)))
	  (args-out-of-range (setq idx str-len)))
	(when (> column end-column)
	  (setq column last-column
		idx last-idx))
	(when (and padding (< column end-column))
	  (setq tail-padding (make-string (- end-column column) padding))))
      (concat head-padding (substring str from-idx idx)
	      tail-padding ellipsis))))

;;; Test suite for truncate-string-to-width
;; (dolist (test '((("" 0) . "")
;; 		(("x" 1) . "x")
;; 		(("xy" 1) . "x")
;; 		(("xy" 2 1) . "y")
;; 		(("xy" 0) . "")
;; 		(("xy" 3) . "xy")
;; 		(("$AVP(B" 0) . "")
;; 		(("$AVP(B" 1) . "")
;; 		(("$AVP(B" 2) . "$AVP(B")
;; 		(("$AVP(B" 1 nil ? ) . " ")
;; 		(("$AVPND(B" 3 1 ? ) . "  ")
;; 		(("x$AVP(Bx" 2) . "x")
;; 		(("x$AVP(Bx" 3) . "x$AVP(B")
;; 		(("x$AVP(Bx" 3) . "x$AVP(B")
;; 		(("x$AVP(Bx" 4 1) . "$AVP(Bx")
;; 		(("kor$(CGQ(Be$(C1[(Ban" 8 1 ? ) . "or$(CGQ(Be$(C1[(B")
;; 		(("kor$(CGQ(Be$(C1[(Ban" 7 2 ? ) . "r$(CGQ(Be ")
;; 		(("" 0 nil nil "...") . "")
;; 		(("x" 3 nil nil "...") . "x")
;; 		(("$AVP(B" 3 nil nil "...") . "$AVP(B")
;; 		(("foo" 3 nil nil "...") . "foo")
;; 		(("foo" 2 nil nil "...") . "fo") ;; XEmacs failure?
;; 		(("foobar" 6 0 nil "...") . "foobar")
;; 		(("foobarbaz" 6 nil nil "...") . "foo...")
;; 		(("foobarbaz" 7 2 nil "...") . "ob...")
;; 		(("foobarbaz" 9 3 nil "...") . "barbaz")
;; 		(("$B$3(Bh$B$s(Be$B$K(Bl$B$A(Bl$B$O(Bo" 15 1 ?  t) . " h$B$s(Be$B$K(Bl$B$A(Bl$B$O(Bo")
;; 		(("$B$3(Bh$B$s(Be$B$K(Bl$B$A(Bl$B$O(Bo" 14 1 ?  t) . " h$B$s(Be$B$K(Bl$B$A(B...")
;; 		(("x" 3 nil nil "$(0GnM$(B") . "x")
;; 		(("$AVP(B" 2 nil nil "$(0GnM$(B") . "$AVP(B")
;; 		(("$AVP(B" 1 nil ?x "$(0GnM$(B") . "x") ;; XEmacs error
;; 		(("$AVPND(B" 3 nil ?  "$(0GnM$(B") . "$AVP(B ") ;; XEmacs error
;; 		(("foobarbaz" 4 nil nil  "$(0GnM$(B") . "$(0GnM$(B")
;; 		(("foobarbaz" 5 nil nil  "$(0GnM$(B") . "f$(0GnM$(B")
;; 		(("foobarbaz" 6 nil nil  "$(0GnM$(B") . "fo$(0GnM$(B")
;; 		(("foobarbaz" 8 3 nil "$(0GnM$(B") . "b$(0GnM$(B")
;; 		(("$B$3(Bh$B$s(Be$B$K(Bl$B$A(Bl$B$O(Bo" 14 4 ?x "$BF|K\8l(B") . "xe$B$KF|K\8l(B")
;; 		(("$B$3(Bh$B$s(Be$B$K(Bl$B$A(Bl$B$O(Bo" 13 4 ?x "$BF|K\8l(B") . "xex$BF|K\8l(B")
;; 		))
;;   (let (ret)
;;     (condition-case e 
;; 	(setq ret (apply #'truncate-string-to-width (car test)))
;;       (error (setq ret e)))
;;     (unless (equal ret (cdr test))
;;       (error "%s: expected %s, got %s"
;; 	     (prin1-to-string (cons 'truncate-string-to-width (car test)))
;; 	     (prin1-to-string (cdr test))
;; 	     (if (consp ret)
;; 		 (format "error: %s: %s" (car ret)
;; 			 (prin1-to-string (cdr ret)))
;; 	       (prin1-to-string ret))))))

;;; For backward compatibility ...
;;;###autoload
(defalias 'truncate-string 'truncate-string-to-width)
(make-obsolete 'truncate-string 'truncate-string-to-width "20.1")

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
	(setcdr (last alist) branches))))

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
      (error "Invalid argument %s" alist))
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
  "Return the value of CODING-SYSTEM's `post-read-conversion' property."
  (coding-system-get coding-system 'post-read-conversion))

;;;###autoload
(defun coding-system-pre-write-conversion (coding-system)
  "Return the value of CODING-SYSTEM's `pre-write-conversion' property."
  (coding-system-get coding-system 'pre-write-conversion))

;;;###autoload
(defun coding-system-translation-table-for-decode (coding-system)
  "Return the value of CODING-SYSTEM's `translation-table-for-decode' property."
  (coding-system-get coding-system 'translation-table-for-decode))

;;;###autoload
(defun coding-system-translation-table-for-encode (coding-system)
  "Return the value of CODING-SYSTEM's `translation-table-for-encode' property."
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
	 (mapc (function (lambda (x) (set (car x) (cdr x))))
	       prio-list)
	 (set-coding-priority (mapcar #'car prio-list))
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


(provide 'mule-util)

;; Local Variables:
;; coding: iso-2022-7bit
;; End:

;;; mule-util.el ends here
