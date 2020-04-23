;;; mule-util.el --- utility functions for multilingual environment (mule)  -*- lexical-binding:t -*-

;; Copyright (C) 1997-1998, 2000-2020 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: mule, multilingual

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

;;; Code:

;;; String manipulations while paying attention to multibyte characters.

;;;###autoload
(defun store-substring (string idx obj)
  "Embed OBJ (string or character) at index IDX of STRING."
  (if (integerp obj)
      (aset string idx obj)
    (let ((len1 (length obj))
	  (i 0))
      (while (< i len1)
	(aset string (+ idx i) (aref obj i))
	(setq i (1+ i)))))
  string)

(defvar truncate-string-ellipsis "..."  ;"…"
  "String to use to indicate truncation.
Serves as default value of ELLIPSIS argument to `truncate-string-to-width'.")

;;;###autoload
(defun truncate-string-to-width (str end-column
				     &optional start-column padding ellipsis
                                     ellipsis-text-property)
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
defaults to `truncate-string-ellipsis'.

If ELLIPSIS-TEXT-PROPERTY is non-nil, a too-long string will not
be truncated, but instead the elided parts will be covered by a
`display' text property showing the ellipsis."
  (or start-column
      (setq start-column 0))
  (when (and ellipsis (not (stringp ellipsis)))
    (setq ellipsis truncate-string-ellipsis))
  (let ((str-len (length str))
	(str-width (string-width str))
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
      (if (and ellipsis-text-property
               (not (equal ellipsis ""))
               idx)
          ;; Use text properties for the ellipsis.
          (concat head-padding
                  (substring str from-idx idx)
	          (propertize (substring str idx) 'display (or ellipsis "")))
        ;; (Possibly) chop off bits of the string.
        (concat head-padding (substring str from-idx idx)
	        tail-padding ellipsis)))))


;;; Nested alist handler.
;; Nested alist is alist whose elements are also nested alist.

;;;###autoload
(defsubst nested-alist-p (obj)
  "Return t if OBJ is a nested alist.

Nested alist is a list of the form (ENTRY . BRANCHES), where ENTRY is
any Lisp object, and BRANCHES is a list of cons cells of the form
\(KEY-ELEMENT . NESTED-ALIST).

You can use a nested alist to store any Lisp object (ENTRY) for a key
sequence KEYSEQ, where KEYSEQ is a sequence of KEY-ELEMENT.  KEYSEQ
can be a string, a vector, or a list."
  (and obj (listp obj) (listp (cdr obj))))

;;;###autoload
(defun set-nested-alist (keyseq entry alist &optional len branches)
  "Set ENTRY for KEYSEQ in a nested alist ALIST.
Optional 4th arg LEN non-nil means the first LEN elements in KEYSEQ
 are considered.
Optional 5th argument BRANCHES if non-nil is branches for a keyseq
longer than KEYSEQ.
See the documentation of `nested-alist-p' for more detail."
  (or (nested-alist-p alist)
      (error "Invalid argument %s" alist))
  (let ((len (or len (length keyseq)))
	(i 0))
    (cond
     ((stringp keyseq)             ; We can use `assq' for characters.
      (while (< i len)
        (if (null (nested-alist-p alist))
            (error "Keyseq %s is too long for this nested alist" keyseq))
        (let* ((key-elt (aref keyseq i))
               (slot (assq key-elt (cdr alist))))
          (unless slot
            (setq slot (list key-elt t))
            (push slot (cdr alist)))
          (setq alist (cdr slot)))
        (setq i (1+ i))))
     ((arrayp keyseq)
      (while (< i len)
        (if (null (nested-alist-p alist))
            (error "Keyseq %s is too long for this nested alist" keyseq))
        (let* ((key-elt (aref keyseq i))
               (slot (assoc key-elt (cdr alist))))
          (unless slot
            (setq slot (list key-elt t))
            (push slot (cdr alist)))
          (setq alist (cdr slot)))
        (setq i (1+ i))))
     ((listp keyseq)
      (while (< i len)
        (if (null (nested-alist-p alist))
            (error "Keyseq %s is too long for this nested alist" keyseq))
        (let* ((key-elt (pop keyseq))
               (slot (assoc key-elt (cdr alist))))
          (unless slot
            (setq slot (list key-elt t))
            (push slot (cdr alist)))
          (setq alist (cdr slot)))
        (setq i (1+ i))))
     (t (signal 'wrong-type-argument (list keyseq))))
    (setcar alist entry)
    (if branches
	(setcdr (last alist) branches))))

;;;###autoload
(defun lookup-nested-alist (keyseq alist &optional len start nil-for-too-long)
  "Look up key sequence KEYSEQ in nested alist ALIST.  Return the definition.
Optional 3rd argument LEN specifies the length of KEYSEQ.
Optional 4th argument START specifies index of the starting key.
The returned value is normally a nested alist of which
car part is the entry for KEYSEQ.
If ALIST is not deep enough for KEYSEQ, return number which is
 how many key elements at the front of KEYSEQ it takes
 to reach a leaf in ALIST.
Optional 5th argument NIL-FOR-TOO-LONG non-nil means return nil
 even if ALIST is not deep enough."
  (or (nested-alist-p alist)
      (error "Invalid argument %s" alist))
  (or len
      (setq len (length keyseq)))
  (let ((i (or start 0)))
    (if (catch 'lookup-nested-alist-tag
          (cond ((stringp keyseq)  ; We can use `assq' for characters.
                 (while (< i len)
                   (if (setq alist (cdr (assq (aref keyseq i) (cdr alist))))
                       (setq i (1+ i))
                     (throw 'lookup-nested-alist-tag t))))
                ((arrayp keyseq)
                 (while (< i len)
                   (if (setq alist (cdr (assoc (aref keyseq i) (cdr alist))))
                       (setq i (1+ i))
                     (throw 'lookup-nested-alist-tag t))))
                ((listp keyseq)
                 (setq keyseq (nthcdr i keyseq))
                 (while (< i len)
                   (if (setq alist (cdr (assoc (pop keyseq) (cdr alist))))
                       (setq i (1+ i))
                     (throw 'lookup-nested-alist-tag t))))
                (t (signal 'wrong-type-argument (list keyseq)))))
	;; KEYSEQ is too long.
	(if nil-for-too-long nil i)
      alist)))


;; Coding system related functions.

;;;###autoload
(defun coding-system-post-read-conversion (coding-system)
  "Return the value of CODING-SYSTEM's `post-read-conversion' property."
  (coding-system-get coding-system :post-read-conversion))

;;;###autoload
(defun coding-system-pre-write-conversion (coding-system)
  "Return the value of CODING-SYSTEM's `pre-write-conversion' property."
  (coding-system-get coding-system :pre-write-conversion))

;;;###autoload
(defun coding-system-translation-table-for-decode (coding-system)
  "Return the value of CODING-SYSTEM's `decode-translation-table' property."
  (coding-system-get coding-system :decode-translation-table))

;;;###autoload
(defun coding-system-translation-table-for-encode (coding-system)
  "Return the value of CODING-SYSTEM's `encode-translation-table' property."
  (coding-system-get coding-system :encode-translation-table))

;;;###autoload
(defmacro with-coding-priority (coding-systems &rest body)
  "Execute BODY like `progn' with CODING-SYSTEMS at the front of priority list.
CODING-SYSTEMS is a list of coding systems.  See `set-coding-system-priority'.
This affects the implicit sorting of lists of coding systems returned by
operations such as `find-coding-systems-region'."
  (let ((current (make-symbol "current")))
  `(let ((,current (coding-system-priority-list)))
     (apply #'set-coding-system-priority ,coding-systems)
     (unwind-protect
	 (progn ,@body)
       (apply #'set-coding-system-priority ,current)))))
;;;###autoload(put 'with-coding-priority 'lisp-indent-function 1)
(put 'with-coding-priority 'edebug-form-spec t)

;;;###autoload
(defmacro detect-coding-with-priority (from to priority-list)
  "Detect a coding system of the text between FROM and TO with PRIORITY-LIST.
PRIORITY-LIST is an alist of coding categories vs the corresponding
coding systems ordered by priority."
  (declare (obsolete with-coding-priority "23.1"))
  `(with-coding-priority (mapcar #'cdr ,priority-list)
     (detect-coding-region ,from ,to)))

;;;###autoload
(defun detect-coding-with-language-environment (from to lang-env)
  "Detect a coding system for the text between FROM and TO with LANG-ENV.
The detection takes into account the coding system priorities for the
language environment LANG-ENV."
  (let ((coding-priority (get-language-info lang-env 'coding-priority)))
    (if coding-priority
	(with-coding-priority coding-priority
          (detect-coding-region from to)))))

(declare-function internal-char-font "font.c" (position &optional ch))

(defun filepos-to-bufferpos--dos (byte f)
  (let ((eol-offset 0)
        ;; Make sure we terminate, even if BYTE falls right in the middle
        ;; of a CRLF or some other weird corner case.
        (omin 0) omax
        pos lines)
    (while
        (progn
          (setq pos (funcall f (- byte eol-offset)))
          ;; Protect against accidental values of BYTE outside of the
          ;; valid region.
          (when (null pos)
            (if (<= byte eol-offset)
                (setq pos (point-min))
              (setq pos (point-max))))
          ;; Adjust POS for DOS EOL format.
          (setq lines (1- (line-number-at-pos pos)))
          (and (not (= lines eol-offset)) (or (not omax) (> omax omin))))
      (if (> lines eol-offset)
          (setq omax (if omax (min (1- omax) lines) lines)
                eol-offset omax)
        (setq omin (max (1+ omin) lines)
              eol-offset omin)))
    pos))

;;;###autoload
(defun filepos-to-bufferpos (byte &optional quality coding-system)
  "Try to return the buffer position corresponding to a particular file position.
The file position is given as a (0-based) BYTE count.
The function presumes the file is encoded with CODING-SYSTEM, which defaults
to `buffer-file-coding-system'.
QUALITY can be:
  `approximate', in which case we may cut some corners to avoid
    excessive work.
  `exact', in which case we may end up re-(en/de)coding a large
    part of the file/buffer, this can be expensive and slow.
  nil, in which case we may return nil rather than an approximation."
  (unless coding-system (setq coding-system buffer-file-coding-system))
  (let ((eol (coding-system-eol-type coding-system))
        (type (coding-system-type coding-system))
        (base (coding-system-base coding-system))
        (pm (save-restriction (widen) (point-min))))
    (and (eq type 'utf-8)
         ;; Any post-read/pre-write conversions mean it's not really UTF-8.
         (not (null (coding-system-get coding-system :post-read-conversion)))
         (setq type 'not-utf-8))
    (and (memq type '(charset raw-text undecided))
         ;; The following are all of type 'charset', but they are
         ;; actually variable-width encodings.
         (not (memq base '(chinese-gbk chinese-gb18030 euc-tw euc-jis-2004
                                       korean-iso-8bit chinese-iso-8bit
                                       japanese-iso-8bit chinese-big5-hkscs
                                       japanese-cp932 korean-cp949)))
         (setq type 'single-byte))
    (pcase type
      ('utf-8
       (when (coding-system-get coding-system :bom)
         (setq byte (max 0 (- byte 3))))
       (if (= eol 1)
           (filepos-to-bufferpos--dos (+ pm byte) #'byte-to-position)
         (byte-to-position (+ pm byte))))
      ('single-byte
       (if (= eol 1)
           (filepos-to-bufferpos--dos (+ pm byte) #'identity)
         (+ pm byte)))
      ((and 'utf-16
            ;; FIXME: For utf-16, we could use the same approach as used for
            ;; dos EOLs (counting the number of non-BMP chars instead of the
            ;; number of lines).
            (guard (not (eq quality 'exact))))
       ;; Account for BOM, which is always 2 bytes in UTF-16.
       (when (coding-system-get coding-system :bom)
         (setq byte (max 0 (- byte 2))))
       ;; In approximate mode, assume all characters are within the
       ;; BMP, i.e. take up 2 bytes.
       (setq byte (/ byte 2))
       (if (= eol 1)
           (filepos-to-bufferpos--dos (+ pm byte) #'identity)
         (+ pm byte)))
      (_
       (pcase quality
         ('approximate (byte-to-position (+ pm byte)))
         ('exact
          ;; Rather than assume that the file exists and still holds the right
          ;; data, we reconstruct it based on the buffer's content.
          (let ((buf (current-buffer)))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (let ((tmp-buf (current-buffer)))
                (with-current-buffer buf
                  (save-restriction
                    (widen)
                    ;; Since encoding should always return more bytes than
                    ;; there were chars, encoding all chars up to (+ byte pm)
                    ;; guarantees the encoded result has at least `byte' bytes.
                    (encode-coding-region pm (min (point-max) (+ pm byte))
                                          coding-system tmp-buf)))
                (+ pm (length
                       (decode-coding-region (point-min)
                                             (min (point-max) (+ pm byte))
                                             coding-system t))))))))))))
;;;###autoload
(defun bufferpos-to-filepos (position &optional quality coding-system)
  "Try to return the file byte corresponding to a particular buffer POSITION.
Value is the file position given as a (0-based) byte count.
The function presumes the file is encoded with CODING-SYSTEM, which defaults
to `buffer-file-coding-system'.
QUALITY can be:
  `approximate', in which case we may cut some corners to avoid
    excessive work.
  `exact', in which case we may end up re-(en/de)coding a large
    part of the file/buffer, this can be expensive and slow.
  nil, in which case we may return nil rather than an approximation."
  (unless coding-system (setq coding-system buffer-file-coding-system))
  (let* ((eol (coding-system-eol-type coding-system))
         (lineno (if (= eol 1) (1- (line-number-at-pos position)) 0))
         (type (coding-system-type coding-system))
         (base (coding-system-base coding-system))
         (point-min 1))                 ;Clarify what the `1' means.
    (and (eq type 'utf-8)
         ;; Any post-read/pre-write conversions mean it's not really UTF-8.
         (not (null (coding-system-get coding-system :post-read-conversion)))
         (setq type 'not-utf-8))
    (and (memq type '(charset raw-text undecided))
         ;; The following are all of type 'charset', but they are
         ;; actually variable-width encodings.
         (not (memq base '(chinese-gbk chinese-gb18030 euc-tw euc-jis-2004
                                       korean-iso-8bit chinese-iso-8bit
                                       japanese-iso-8bit chinese-big5-hkscs
                                       japanese-cp932 korean-cp949)))
         (setq type 'single-byte))
    (pcase type
      ('utf-8
       (+ (or (position-bytes position)
              (if (<= position 0)
                  point-min
                (position-bytes (point-max))))
          ;; Account for BOM, if any.
          (if (coding-system-get coding-system :bom) 3 0)
          ;; Account for CR in CRLF pairs.
          lineno
          (- point-min)))
      ('single-byte
       (+ position (- point-min) lineno))
      ((and 'utf-16
            ;; FIXME: For utf-16, we could use the same approach as used for
            ;; dos EOLs (counting the number of non-BMP chars instead of the
            ;; number of lines).
            (guard (not (eq quality 'exact))))
       ;; In approximate mode, assume all characters are within the
       ;; BMP, i.e. each one takes up 2 bytes.
       (+ (* (- position point-min) 2)
          ;; Account for BOM, if any.
          (if (coding-system-get coding-system :bom) 2 0)
          ;; Account for CR in CRLF pairs.
          lineno))
      (_
       (pcase quality
         ('approximate (+ (position-bytes position) (- point-min) lineno))
         ('exact
          ;; Rather than assume that the file exists and still holds the right
          ;; data, we reconstruct its relevant portion.
          (let ((buf (current-buffer)))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (let ((tmp-buf (current-buffer)))
                (with-current-buffer buf
                  (save-restriction
                    (widen)
                    (encode-coding-region (point-min) (min (point-max) position)
                                          coding-system tmp-buf)))
                (buffer-size))))))))))

(provide 'mule-util)

;; Local Variables:
;; coding: utf-8
;; End:

;;; mule-util.el ends here
