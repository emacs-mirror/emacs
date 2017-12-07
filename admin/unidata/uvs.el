;;; uvs.el --- utility for UVS (format 14) cmap subtables in OpenType fonts.

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; Author: YAMAMOTO Mitsuharu <mituharu@math.s.chiba-u.ac.jp>

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

;; To extract a C array definition of a UVS table for the Adobe-Japan1
;; character collection from an IVD Sequences file, execute
;;   $ emacs -batch -l uvs.el \
;;      --eval '(uvs-print-table-ivd "IVD_Sequences.txt" "Adobe-Japan1")' \
;;      > uvs.h

;;; Code:

(defun uvs-fields-total-size (fields)
  (apply '+ (mapcar (lambda (field) (get field 'uvs-field-size)) fields)))

;;; Fields in Format 14 header.
(defconst uvs-format-14-header-fields
  '(format length num-var-selector-records))
(put 'format 'uvs-field-size 2)
(put 'length 'uvs-field-size 4)
(put 'num-var-selector-records 'uvs-field-size 4)
(defconst uvs-format-14-header-size
  (uvs-fields-total-size uvs-format-14-header-fields))

;;; Fields in Variation Selector Record.
(defconst uvs-variation-selector-record-fields
  '(var-selector default-uvs-offset non-default-uvs-offset))
(put 'var-selector 'uvs-field-size 3)
(put 'default-uvs-offset 'uvs-field-size 4)
(put 'non-default-uvs-offset 'uvs-field-size 4)
(defconst uvs-variation-selector-record-size
  (uvs-fields-total-size uvs-variation-selector-record-fields))

;;; Fields in Non-Default UVS Table.
(defconst uvs-non-default-uvs-table-header-fields '(num-uvs-mappings))
(put 'num-uvs-mappings 'uvs-field-size 4)
(defconst uvs-non-default-uvs-table-header-size
  (uvs-fields-total-size uvs-non-default-uvs-table-header-fields))

;;; Fields in UVS Mapping.
(defconst uvs-uvs-mapping-fields '(unicode-value glyph-id))
(put 'unicode-value 'uvs-field-size 3)
(put 'glyph-id 'uvs-field-size 2)
(defconst uvs-uvs-mapping-size
  (uvs-fields-total-size uvs-uvs-mapping-fields))

(defun uvs-alist-from-ivd (collection-id sequence-id-to-glyph-function)
  "Create UVS alist from IVD Sequences and COLLECTION-ID.
The IVD (Ideographic Variation Database) Sequences are obtained
from the contents of the current buffer, and should be in the
form of IVD_Sequences.txt specified in Unicode Technical Standard
#37.  COLLECTION-ID is a string specifying the identifier of the
collection to extract (e.g., \"Adobe-Japan1\").
SEQUENCE-ID-TO-GLYPH-FUNC is a function to convert an identifier
string of the sequence to a glyph number.  UVS alist is of the
following form:
  ((SELECTOR1 . ((BASE11 . GLYPH11) (BASE12 . GLYPH12) ...))
   (SELECTOR2 . ((BASE21 . GLYPH21) (BASE22 . GLYPH22) ...)) ...),
where selectors and bases are sorted in ascending order."
  (let (uvs-alist)
    (goto-char (point-min))
    (while (re-search-forward
	    (concat "^[[:blank:]]*"
		    "\\([[:xdigit:]]+\\) \\([[:xdigit:]]+\\)"
		    "[[:blank:]]*;[[:blank:]]*"
		    "\\(?:" (regexp-quote collection-id) "\\)"
		    "[[:blank:]]*;[[:blank:]]*"
		    "\\([^\n[:blank:]]+\\)"
		    "[[:blank:]]*$") nil t)
      (let* ((base (string-to-number (match-string 1) 16))
	     (selector (string-to-number (match-string 2) 16))
	     (sequence-id (match-string 3))
	     (glyph (funcall sequence-id-to-glyph-function sequence-id)))
	(let ((selector-bgs (assq selector uvs-alist))
	      (base-glyph (cons base glyph)))
	  (if selector-bgs
	      (setcdr selector-bgs (cons base-glyph (cdr selector-bgs)))
	    (push (cons selector (list base-glyph)) uvs-alist)))))
    (dolist (selector-bgs uvs-alist)
      (setcdr selector-bgs
	      (sort (cdr selector-bgs)
		    (lambda (bg1 bg2) (< (car bg1) (car bg2))))))
    (sort uvs-alist (lambda (sb1 sb2) (< (car sb1) (car sb2))))))

(defun uvs-int-to-bytes (value size)
  "Convert integer VALUE to a list of SIZE bytes.
The most significant byte comes first."
  (let (result)
    (dotimes (i size)
      (push (logand value #xff) result)
      (setq value (lsh value -8)))
    result))

(defun uvs-insert-fields-as-bytes (fields &rest values)
  "Insert VALUES for FIELDS as a sequence of bytes to the current buffer.
VALUES and FIELDS are lists of integers and field symbols,
respectively.  Byte length of each value is determined by the
`uvs-field-size' property of the corresponding field."
  (while fields
    (let ((field (car fields))
	  (value (car values)))
      (insert (apply 'unibyte-string
		     (uvs-int-to-bytes value (get field 'uvs-field-size))))
      (setq fields (cdr fields) values (cdr values)))))

(defun uvs-insert-alist-as-bytes (uvs-alist)
  "Insert UVS-ALIST as a sequence of bytes to the current buffer."
  (let* ((nrecords (length uvs-alist))	; # of selectors
	 (total-nmappings
	  (apply '+ (mapcar
		     (lambda (selector-bgs) (length (cdr selector-bgs)))
		     uvs-alist)))
	 (non-default-offset
	  (+ uvs-format-14-header-size
	     (* uvs-variation-selector-record-size nrecords))))
    (uvs-insert-fields-as-bytes uvs-format-14-header-fields
				14
				(+ uvs-format-14-header-size
				   (* uvs-variation-selector-record-size
				      nrecords)
				   (* uvs-non-default-uvs-table-header-size
				      nrecords)
				   (* uvs-uvs-mapping-size total-nmappings))
				nrecords)
    (dolist (selector-bgs uvs-alist)
      (uvs-insert-fields-as-bytes uvs-variation-selector-record-fields
				  (car selector-bgs)
				  0	; No Default UVS Tables.
				  non-default-offset)
      (setq non-default-offset
	    (+ non-default-offset
	       uvs-non-default-uvs-table-header-size
	       (* (length (cdr selector-bgs)) uvs-uvs-mapping-size))))
    (dolist (selector-bgs uvs-alist)
      (uvs-insert-fields-as-bytes uvs-non-default-uvs-table-header-fields
				  (length (cdr selector-bgs)))
      (dolist (base-glyph (cdr selector-bgs))
	(uvs-insert-fields-as-bytes uvs-uvs-mapping-fields
				    (car base-glyph)
				    (cdr base-glyph))))))

(defun uvs-dump (&optional bytes-per-line separator separator-eol line-prefix)
  "Print the current buffer as in representation of C array contents."
  (or bytes-per-line (setq bytes-per-line 8))
  (or separator (setq separator ", "))
  (or separator-eol (setq separator-eol ","))
  (or line-prefix (setq line-prefix "    "))
  (goto-char (point-min))
  (while (> (- (point-max) (point)) bytes-per-line)
    (princ line-prefix)
    (princ (mapconcat (lambda (byte) (format "0x%02x" byte))
		      (string-to-unibyte
		       (buffer-substring (point) (+ (point) bytes-per-line)))
		      separator))
    (princ separator-eol)
    (terpri)
    (forward-char bytes-per-line))
  (princ line-prefix)
  (princ (mapconcat (lambda (byte) (format "0x%02x" byte))
		    (string-to-unibyte
		     (buffer-substring (point) (point-max)))
		    separator))
  (terpri))

(defun uvs-print-table-ivd (filename collection-id
				     &optional sequence-id-to-glyph-func)
  "Print a C array definition of a UVS table for IVD Sequences.
FILENAME specifies the IVD Sequences file.  COLLECTION-ID is a
string specifying the identifier of the collection to
extract (e.g., \"Adobe-Japan1\").  SEQUENCE-ID-TO-GLYPH-FUNC is a
function to convert an identifier string of the sequence to a
glyph number, and nil means to convert \"CID\\+[0-9]+\" to the
corresponding number."
  (or sequence-id-to-glyph-func
      (setq sequence-id-to-glyph-func
	    (lambda (sequence-id)
	      (string-match "\\`CID\\+\\([[:digit:]]+\\)\\'" sequence-id)
	      (string-to-number (match-string 1 sequence-id)))))
  (let ((uvs-alist
	 (with-temp-buffer
	   (insert-file-contents filename)
	   (uvs-alist-from-ivd collection-id
			       sequence-id-to-glyph-func))))
    (set-binary-mode 'stdout t)
    (princ "/* Automatically generated by uvs.el.  */\n")
    (princ
     (format "static const unsigned char mac_uvs_table_%s_bytes[] =\n  {\n"
	     (replace-regexp-in-string "[^_[:alnum:]]" "_"
				       (downcase collection-id))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (uvs-insert-alist-as-bytes uvs-alist)
      (uvs-dump))
    (princ "  };\n")))

;;; uvs.el ends here
