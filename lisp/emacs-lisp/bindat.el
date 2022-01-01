;;; bindat.el --- binary data structure packing and unpacking.  -*- lexical-binding: t; -*-

;; Copyright (C) 2002-2022 Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Assignment name: struct.el
;; Keywords: comm data processes

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

;;  Packing and unpacking of (binary) data structures.
;;
;;  The data formats used in binary files and network protocols are
;;  often structured data which can be described by a C-style structure
;;  such as the one shown below.  Using the bindat package, decoding
;;  and encoding binary data formats like these is made simple using a
;;  structure specification which closely resembles the C style
;;  structure declarations.
;;
;;  Encoded (binary) data is stored in a unibyte string or vector,
;;  while the decoded data is stored in an alist with (FIELD . VALUE)
;;  pairs.

;; Example:

;;  Consider the following C structures:
;;
;;  struct header {
;;	uint32_t	dest_ip;
;;	uint32_t	src_ip;
;;	uint16_t	dest_port;
;;	uint16_t	src_port;
;;  };
;;
;;  struct data {
;;	uint8_t		type;
;;	uint8_t		opcode;
;;	uint32_t	length;  /* In little endian order */
;;	unsigned char	id[8];   /* nul-terminated string  */
;;	unsigned char	data[/* (length + 3) & ~3 */];
;;  };
;;
;;  struct packet {
;;	struct header	header;
;;	uint8_t		items;
;;	unsigned char   filler[3];
;;	struct data	item[/* items */];
;;  };
;;
;;  The corresponding Lisp bindat specification could look like this:
;;
;;  (bindat-defmacro ip () '(vec 4 byte))
;;
;;  (setq header-bindat-spec
;;    (bindat-type
;;      (dest-ip   ip)
;;	(src-ip    ip)
;;	(dest-port uint 16)
;;	(src-port  uint 16)))
;;
;;  (setq data-bindat-spec
;;    (bindat-type
;;      (type      u8)
;;	(opcode	   u8)
;;	(length	   uintr 32)  ;; little endian order
;;	(id	   strz 8)
;;	(data	   vec length)
;;	(_         align 4)))
;;
;;  (setq packet-bindat-spec
;;    (bindat-type
;;      (header    type header-bindat-spec)
;;	(nitems    u8)
;;	(_         fill 3)
;;	(items     repeat nitems type data-bindat-spec)))
;;
;;  A binary data representation may look like
;;   [ 192 168 1 100 192 168 1 101 01 28 21 32 2 0 0 0
;;     2 3 5 0 ?A ?B ?C ?D ?E ?F 0 0 1 2 3 4 5 0 0 0
;;     1 4 7 0 ?B ?C ?D ?E ?F ?G 0 0 6 7 8 9 10 11 12 0 ]
;;
;;  The corresponding decoded structure returned by `bindat-unpack' (or taken
;;  by `bindat-pack') looks like:
;;
;;      ((header
;;        (dest-ip   . [192 168 1 100])
;;        (src-ip    . [192 168 1 101])
;;        (dest-port . 284)
;;        (src-port  . 5408))
;;       (items . 2)
;;       (item ((data . [1 2 3 4 5])
;;      	(id . "ABCDEF")
;;      	(length . 5)
;;      	(opcode . 3)
;;      	(type . 2))
;;             ((data . [6 7 8 9 10 11 12])
;;      	(id . "BCDEFG")
;;      	(length . 7)
;;      	(opcode . 4)
;;      	(type . 1))))
;;
;;  To access a specific value in this structure, use the function
;;  `bindat-get-field' with the structure as first arg followed by a list
;;  of field names and array indexes, e.g. using the data above,
;;    (bindat-get-field decoded-structure 'item 1 'id)
;;  returns "BCDEFG".

;;; Code:

;; Helper functions for structure unpacking.
;; Relies on dynamic binding of `bindat-raw' and `bindat-idx'.

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))   ;For `named-let'.

(cl-defstruct (bindat--type
               (:predicate nil)
               (:constructor bindat--make))
  le ue pe)

(defvar bindat-raw)
(defvar bindat-idx)

(defsubst bindat--unpack-u8 ()
  (prog1
      (aref bindat-raw bindat-idx)
    (setq bindat-idx (1+ bindat-idx))))

(defun bindat--unpack-u16 ()
  (logior (ash (bindat--unpack-u8) 8) (bindat--unpack-u8)))

(defun bindat--unpack-u24 ()
  (logior (ash (bindat--unpack-u16) 8) (bindat--unpack-u8)))

(defun bindat--unpack-u32 ()
  (logior (ash (bindat--unpack-u16) 16) (bindat--unpack-u16)))

(defun bindat--unpack-u16r ()
  (logior (bindat--unpack-u8) (ash (bindat--unpack-u8) 8)))

(defun bindat--unpack-u24r ()
  (logior (bindat--unpack-u16r) (ash (bindat--unpack-u8) 16)))

(defun bindat--unpack-u32r ()
  (logior (bindat--unpack-u16r) (ash (bindat--unpack-u16r) 16)))

(defun bindat--unpack-str (len)
  (let ((s (substring bindat-raw bindat-idx (+ bindat-idx len))))
    (setq bindat-idx (+ bindat-idx len))
    (if (stringp s) s
      (apply #'unibyte-string s))))

(defun bindat--unpack-strz (len)
  (let ((i 0) s)
    (while (and (if len (< i len) t) (/= (aref bindat-raw (+ bindat-idx i)) 0))
      (setq i (1+ i)))
    (setq s (substring bindat-raw bindat-idx (+ bindat-idx i)))
    (setq bindat-idx (+ bindat-idx len))
    (if (stringp s) s
      (apply #'unibyte-string s))))

(defun bindat--unpack-bits (len)
  (let ((bits nil) (bnum (1- (* 8 len))) j m)
    (while (>= bnum 0)
      (if (= (setq m (bindat--unpack-u8)) 0)
	  (setq bnum (- bnum 8))
	(setq j 128)
	(while (> j 0)
	  (if (/= 0 (logand m j))
	      (setq bits (cons bnum bits)))
	  (setq bnum (1- bnum)
		j (ash j -1)))))
    bits))

(defun bindat--unpack-item (type len &optional vectype)
  (if (eq type 'ip)
      (setq type 'vec len 4))
  (pcase type
   ((or 'u8 'byte) (bindat--unpack-u8))
   ((or 'u16 'word 'short) (bindat--unpack-u16))
   ('u24 (bindat--unpack-u24))
   ((or 'u32 'dword 'long) (bindat--unpack-u32))
   ('u16r (bindat--unpack-u16r))
   ('u24r (bindat--unpack-u24r))
   ('u32r (bindat--unpack-u32r))
   ('bits (bindat--unpack-bits len))
   ('str (bindat--unpack-str len))
   ('strz (bindat--unpack-strz len))
   ('vec
    (let ((v (make-vector len 0)) (vlen 1))
      (if (consp vectype)
	  (setq vlen (nth 1 vectype)
		vectype (nth 2 vectype))
	(setq type (or vectype 'u8)
	      vectype nil))
      (dotimes (i len)
	(aset v i (bindat--unpack-item type vlen vectype)))
      v))
   (_ nil)))

(defsubst bindat--align (n len)
  (* len (/ (+ n (1- len)) len)))       ;Isn't there a simpler way?

(defun bindat--unpack-group (spec)
  ;; FIXME: Introduce a new primitive so we can mark `bindat-unpack'
  ;; as obsolete (maybe that primitive should be a macro which takes
  ;; a bindat type *expression* as argument).
  (if (cl-typep spec 'bindat--type)
      (funcall (bindat--type-ue spec))
  (with-suppressed-warnings ((lexical struct last))
    (defvar struct) (defvar last))
  (let (struct last)
    (dolist (item spec)
      (let* ((field (car item))
	     (type (nth 1 item))
	     (len (nth 2 item))
	     (vectype (and (eq type 'vec) (nth 3 item)))
	     (tail 3)
	     data)
	(if (and type (consp type) (eq (car type) 'eval))
	    (setq type (eval (car (cdr type)) t)))
	(if (and len (consp len) (eq (car len) 'eval))
	    (setq len (eval (car (cdr len)) t)))
	(if (memq field '(eval fill align struct union))
	    (setq tail 2
		  len type
		  type field
		  field nil))
	(if (and (consp field) (eq (car field) 'eval))
	    (setq field (eval (car (cdr field)) t)))
	(if (and (consp len) (not (eq type 'eval)))
            (setq len (apply #'bindat-get-field struct len)))
	(if (not len)
	    (setq len 1))
	(pcase type
	 ('eval
	  (if field
	      (setq data (eval len t))
	    (eval len t)))
	 ('fill
	  (setq bindat-idx (+ bindat-idx len)))
	 ('align
	  (setq bindat-idx (bindat--align bindat-idx len)))
	 ('struct
	  (setq data (bindat--unpack-group (eval len t))))
	 ('repeat
	  (dotimes (_ len)
	    (push (bindat--unpack-group (nthcdr tail item)) data))
	  (setq data (nreverse data)))
	 ('union
	  (with-suppressed-warnings ((lexical tag))
	    (defvar tag))
	  (let ((tag len) (cases (nthcdr tail item)) case cc)
	    (while cases
	      (setq case (car cases)
		    cases (cdr cases)
		    cc (car case))
	      (if (or (equal cc tag) (equal cc t)
		      (and (consp cc) (eval cc t)))
		  (setq data (bindat--unpack-group (cdr case))
			cases nil)))))
	 ((pred integerp) (debug t))
	 (_
	  (setq data (bindat--unpack-item type len vectype)
		last data)))
	(if data
	    (setq struct (if field
		             (cons (cons field data) struct)
		           (append data struct))))))
    struct)))

(defun bindat-unpack (spec raw &optional idx)
  "Return structured data according to SPEC for binary data in RAW.
RAW is a unibyte string or vector.
Optional third arg IDX specifies the starting offset in RAW."
  (when (multibyte-string-p raw)
    (error "String is multibyte"))
  (let ((bindat-idx (or idx 0))
        (bindat-raw raw))
    (bindat--unpack-group spec)))

(defun bindat-get-field (struct &rest field)
  "In structured data STRUCT, return value of field named FIELD.
If multiple field names are specified, use the field names to
lookup nested sub-structures in STRUCT, corresponding to the
C-language syntax STRUCT.FIELD1.FIELD2.FIELD3...
An integer value in the field list is taken as an array index,
e.g. corresponding to STRUCT.FIELD1[INDEX2].FIELD3..."
  (while (and struct field)
    (setq struct (if (integerp (car field))
		     (elt struct (car field))
		   (cdr (assq (car field) struct))))
    (setq field (cdr field)))
  struct)

;;;; Calculate bindat-raw length of structured data

(defvar bindat--fixed-length-alist
  '((u8 . 1) (byte . 1)
    (u16 . 2) (u16r . 2) (word . 2) (short . 2)
    (u24 . 3) (u24r . 3)
    (u32 . 4) (u32r . 4) (dword . 4) (long . 4)
    (ip . 4)))

(defun bindat--length-group (struct spec)
  (if (cl-typep spec 'bindat--type)
      (funcall (bindat--type-le spec) struct)
  (with-suppressed-warnings ((lexical struct last))
    (defvar struct) (defvar last))
  (let ((struct struct) last)
    (dolist (item spec)
      (let* ((field (car item))
	     (type (nth 1 item))
	     (len (nth 2 item))
	     (vectype (and (eq type 'vec) (nth 3 item)))
	     (tail 3))
	(if (and type (consp type) (eq (car type) 'eval))
	    (setq type (eval (car (cdr type)) t)))
	(if (and len (consp len) (eq (car len) 'eval))
	    (setq len (eval (car (cdr len)) t)))
	(if (memq field '(eval fill align struct union))
	    (setq tail 2
		  len type
		  type field
		  field nil))
	(if (and (consp field) (eq (car field) 'eval))
	    (setq field (eval (car (cdr field)) t)))
	(if (and (consp len) (not (eq type 'eval)))
	    (setq len (apply #'bindat-get-field struct len)))
	(if (not len)
	    (setq len 1))
	(while (eq type 'vec)
	  (if (consp vectype)
	      (setq len (* len (nth 1 vectype))
		    type (nth 2 vectype))
	    (setq type (or vectype 'u8)
		  vectype nil)))
	(pcase type
	 ('eval
	  (if field
	      (setq struct (cons (cons field (eval len t)) struct))
	    (eval len t)))
	 ('fill
	  (setq bindat-idx (+ bindat-idx len)))
	 ('align
	  (setq bindat-idx (bindat--align bindat-idx len)))
	 ('struct
	  (bindat--length-group
	   (if field (bindat-get-field struct field) struct) (eval len t)))
	 ('repeat
	  (dotimes (index len)
	    (bindat--length-group
             (nth index (bindat-get-field struct field))
             (nthcdr tail item))))
	 ('union
	  (with-suppressed-warnings ((lexical tag))
	    (defvar tag))
	  (let ((tag len) (cases (nthcdr tail item)) case cc)
	    (while cases
	      (setq case (car cases)
		    cases (cdr cases)
		    cc (car case))
	      (if (or (equal cc tag) (equal cc t)
		      (and (consp cc) (eval cc t)))
		  (progn
		    (bindat--length-group struct (cdr case))
		    (setq cases nil))))))
	 (_
	  (if (setq type (assq type bindat--fixed-length-alist))
	      (setq len (* len (cdr type))))
	  (if field
	      (setq last (bindat-get-field struct field)))
	  (setq bindat-idx (+ bindat-idx len)))))))))

(defun bindat-length (spec struct)
  "Calculate `bindat-raw' length for STRUCT according to bindat SPEC."
  (let ((bindat-idx 0))
    (bindat--length-group struct spec)
    bindat-idx))


;;;; Pack structured data into bindat-raw

(defsubst bindat--pack-u8 (v)
  (aset bindat-raw bindat-idx (logand v 255))
  (setq bindat-idx (1+ bindat-idx)))

(defun bindat--pack-u16 (v)
  (aset bindat-raw bindat-idx (logand (ash v -8) 255))
  (aset bindat-raw (1+ bindat-idx) (logand v 255))
  (setq bindat-idx (+ bindat-idx 2)))

(defun bindat--pack-u24 (v)
  (bindat--pack-u8 (ash v -16))
  (bindat--pack-u16 v))

(defun bindat--pack-u32 (v)
  (bindat--pack-u16 (ash v -16))
  (bindat--pack-u16 v))

(defun bindat--pack-u64 (v)
  (bindat--pack-u32 (ash v -32))
  (bindat--pack-u32 v))

(defun bindat--pack-u16r (v)
  (aset bindat-raw (1+ bindat-idx) (logand (ash v -8) 255))
  (aset bindat-raw bindat-idx (logand v 255))
  (setq bindat-idx (+ bindat-idx 2)))

(defun bindat--pack-u24r (v)
  (bindat--pack-u16r v)
  (bindat--pack-u8 (ash v -16)))

(defun bindat--pack-u32r (v)
  (bindat--pack-u16r v)
  (bindat--pack-u16r (ash v -16)))

(defun bindat--pack-u64r (v)
  (bindat--pack-u32r v)
  (bindat--pack-u32r (ash v -32)))

(defun bindat--pack-str (len v)
  (dotimes (i (min len (length v)))
    (aset bindat-raw (+ bindat-idx i) (aref v i)))
  (setq bindat-idx (+ bindat-idx len)))

(defun bindat--pack-strz (v)
  (let ((len (length v)))
    (dotimes (i len)
      (aset bindat-raw (+ bindat-idx i) (aref v i)))
    (setq bindat-idx (+ bindat-idx len 1))))

(defun bindat--pack-bits (len v)
  (let ((bnum (1- (* 8 len))) j m)
    (while (>= bnum 0)
      (setq m 0)
      (if (null v)
	  (setq bnum (- bnum 8))
	(setq j 128)
	(while (> j 0)
	  (if (memq bnum v)
	      (setq m (logior m j)))
	  (setq bnum (1- bnum)
		j (ash j -1))))
      (bindat--pack-u8 m))))

(defun bindat--pack-item (v type len &optional vectype)
  (if (eq type 'ip)
      (setq type 'vec len 4))
  (pcase type
   ((guard (null v)) (setq bindat-idx (+ bindat-idx len)))
   ((or 'u8 'byte) (bindat--pack-u8 v))
   ((or 'u16 'word 'short) (bindat--pack-u16 v))
   ('u24 (bindat--pack-u24 v))
   ((or 'u32 'dword 'long) (bindat--pack-u32 v))
   ('u16r (bindat--pack-u16r v))
   ('u24r (bindat--pack-u24r v))
   ('u32r (bindat--pack-u32r v))
   ('bits (bindat--pack-bits len v))
   ((or 'str 'strz) (bindat--pack-str len v))
   ('vec
    (let ((l (length v)) (vlen 1))
      (if (consp vectype)
	  (setq vlen (nth 1 vectype)
		vectype (nth 2 vectype))
	(setq type (or vectype 'u8)
	      vectype nil))
      (if (> l len) (setq l len))
      (dotimes (i l)
	(bindat--pack-item (aref v i) type vlen vectype))))
   (_
    (setq bindat-idx (+ bindat-idx len)))))

(defun bindat--pack-group (struct spec)
  (if (cl-typep spec 'bindat--type)
      (funcall (bindat--type-pe spec) struct)
  (with-suppressed-warnings ((lexical struct last))
    (defvar struct) (defvar last))
  (let ((struct struct) last)
    (dolist (item spec)
      (let* ((field (car item))
	     (type (nth 1 item))
	     (len (nth 2 item))
	     (vectype (and (eq type 'vec) (nth 3 item)))
	     (tail 3))
	(if (and type (consp type) (eq (car type) 'eval))
	    (setq type (eval (car (cdr type)) t)))
	(if (and len (consp len) (eq (car len) 'eval))
	    (setq len (eval (car (cdr len)) t)))
	(if (memq field '(eval fill align struct union))
	    (setq tail 2
		  len type
		  type field
		  field nil))
	(if (and (consp field) (eq (car field) 'eval))
	    (setq field (eval (car (cdr field)) t)))
	(if (and (consp len) (not (eq type 'eval)))
            (setq len (apply #'bindat-get-field struct len)))
	(if (not len)
	    (setq len 1))
	(pcase type
	 ('eval
	  (if field
	      (setq struct (cons (cons field (eval len t)) struct))
	    (eval len t)))
	 ('fill
	  (setq bindat-idx (+ bindat-idx len)))
	 ('align
	  (setq bindat-idx (bindat--align bindat-idx len)))
	 ('struct
	  (bindat--pack-group
	   (if field (bindat-get-field struct field) struct) (eval len t)))
	 ('repeat
	  (dotimes (index len)
	    (bindat--pack-group
             (nth index (bindat-get-field struct field))
             (nthcdr tail item))))
	 ('union
	  (with-suppressed-warnings ((lexical tag))
	    (defvar tag))
	  (let ((tag len) (cases (nthcdr tail item)) case cc)
	    (while cases
	      (setq case (car cases)
		    cases (cdr cases)
		    cc (car case))
	      (if (or (equal cc tag) (equal cc t)
		      (and (consp cc) (eval cc t)))
		  (progn
		    (bindat--pack-group struct (cdr case))
		    (setq cases nil))))))
	 (_
	  (setq last (bindat-get-field struct field))
	  (bindat--pack-item last type len vectype)
	  )))))))

(defun bindat-pack (spec struct &optional raw idx)
  "Return binary data packed according to SPEC for structured data STRUCT.
Optional third arg RAW is a pre-allocated unibyte string or vector to
pack into.
Optional fourth arg IDX is the starting offset into RAW."
  (when (multibyte-string-p raw)
    (error "Pre-allocated string is multibyte"))
  (let* ((bindat-idx (or idx 0))
         (bindat-raw
          (or raw
              (make-string (+ bindat-idx (bindat-length spec struct)) 0))))
    (bindat--pack-group struct spec)
    (if raw nil bindat-raw)))

;;;; Misc. format conversions

(defun bindat-format-vector (vect fmt sep &optional len)
  "Format vector VECT using element format FMT and separator SEP.
Result is a string with each element of VECT formatted using FMT and
separated by the string SEP.  If optional fourth arg LEN is given, use
only that many elements from VECT."
  (when len (setq vect (substring vect 0 len)))
  (mapconcat (lambda (x) (format fmt x)) vect sep))

(defun bindat-vector-to-dec (vect &optional sep)
  "Format vector VECT in decimal format separated by dots.
If optional second arg SEP is a string, use that as separator."
  (bindat-format-vector vect "%d" (if (stringp sep) sep ".")))

(defun bindat-vector-to-hex (vect &optional sep)
  "Format vector VECT in hex format separated by colons.
If optional second arg SEP is a string, use that as separator."
  (bindat-format-vector vect "%02x" (if (stringp sep) sep ":")))

(defun bindat-ip-to-string (ip)
  "Format vector IP as an ip address in dotted notation.
The port (if any) is omitted.  IP can be a string, as well."
  (if (vectorp ip)
      (format-network-address ip t)
    (format "%d.%d.%d.%d"
            (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3))))

;;;; New approach based on macro-expansion

;; Further improvements suggested by reading websocket.el:
;; - Support for bit-sized fields?
;;
;; - Add some way to verify redundant/checksum fields's contents without
;;   having to provide a complete `:unpack-val' expression.
;;   The `:pack-val' thingy can work nicely to compute checksum fields
;;   based on previous fields's contents (without impacting or being impacted
;;   by the unpacked representation), but if we want to verify
;;   those checksums when unpacking, we have to use the :unpack-val
;;   and build the whole object by hand instead of being able to focus
;;   just on the checksum field.
;;   Maybe this could be related to `unit' type fields where we might like
;;   to make sure that the "value" we write into it is the same as the
;;   value it holds (tho those checks don't happen at the same time (pack
;;   vs unpack).
;;
;; - Support for packing/unpacking to/from something else than
;;   a unibyte string, e.g. from a buffer.  Problems to do that are:
;;   - the `str' and `strz' types which use `substring' rather than reading
;;     one byte at a time.
;;   - the `align' and `fill' which just want to skip without reading/writing
;;   - the `pack-uint' case, which would prefer writing the LSB first.
;;   - the `align' case needs to now the current position in order to know
;;     how far to advance
;;
;; - Don't write triple code when the type is only ever used at a single place
;;   (e.g. to unpack).

(defun bindat--unpack-uint (bitlen)
  (let ((v 0) (bitsdone 0))
    (while (< bitsdone bitlen)
      (setq v (logior (ash v 8) (bindat--unpack-u8)))
      (setq bitsdone (+ bitsdone 8)))
    v))

(defun bindat--unpack-uintr (bitlen)
  (let ((v 0) (bitsdone 0))
    (while (< bitsdone bitlen)
      (setq v (logior v (ash (bindat--unpack-u8) bitsdone)))
      (setq bitsdone (+ bitsdone 8)))
    v))

(defun bindat--pack-uint (bitlen v)
  (let* ((len (/ bitlen 8))
         (shift (- (* 8 (1- len)))))
    (dotimes (_ len)
      (bindat--pack-u8 (logand 255 (ash v shift)))
      (setq shift (+ 8 shift)))))

(defun bindat--pack-uintr (bitlen v)
  (let* ((len (/ bitlen 8)))
    (dotimes (_ len)
      (bindat--pack-u8 (logand v 255))
      (setq v (ash v -8)))))

(defmacro bindat--pcase (&rest args)
  "Like `pcase' but optimize the code under the assumption that it's exhaustive."
  (declare (indent 1) (debug pcase))
  `(pcase ,@args (pcase--dontcare nil)))

(cl-defgeneric bindat--type (op head &rest args)
  "Return the code for the operation OP of the Bindat type (HEAD . ARGS).
OP can be one of: unpack', (pack VAL), or (length VAL) where VAL
is the name of a variable that will hold the value we need to pack.")

(cl-defmethod bindat--type (op (_ (eql 'byte)))
  (bindat--pcase op
    ('unpack `(bindat--unpack-u8))
    (`(length . ,_) `(cl-incf bindat-idx 1))
    (`(pack . ,args) `(bindat--pack-u8 . ,args))))

(cl-defmethod bindat--type (op (_ (eql 'uint))  n)
  (if (eq n 8) (bindat--type op 'byte)
    (bindat--pcase op
      ('unpack `(bindat--unpack-uint ,n))
      (`(length . ,_) `(cl-incf bindat-idx (/ ,n 8)))
      (`(pack . ,args) `(bindat--pack-uint ,n . ,args)))))

(cl-defmethod bindat--type (op (_ (eql 'uintr)) n)
  (if (eq n 8) (bindat--type op 'byte)
    (bindat--pcase op
      ('unpack `(bindat--unpack-uintr ,n))
      (`(length . ,_) `(cl-incf bindat-idx (/ ,n 8)))
      (`(pack . ,args) `(bindat--pack-uintr ,n . ,args)))))

(cl-defmethod bindat--type (op (_ (eql 'str))   len)
  (bindat--pcase op
    ('unpack `(bindat--unpack-str ,len))
    (`(length . ,_) `(cl-incf bindat-idx ,len))
    (`(pack . ,args) `(bindat--pack-str ,len . ,args))))

(cl-defmethod bindat--type (op (_ (eql 'strz))  &optional len)
  (bindat--pcase op
    ('unpack `(bindat--unpack-strz ,len))
    (`(length ,val)
     `(cl-incf bindat-idx ,(cond
                            ((null len) `(length ,val))
                            ((numberp len) len)
                            (t `(or ,len (length ,val))))))
    (`(pack . ,args)
     (macroexp-let2 nil len len
       `(if ,len
            ;; Same as non-zero terminated strings since we don't actually add
            ;; the terminating zero anyway (because we rely on the fact that
            ;; `bindat-raw' was presumably initialized with all-zeroes before
            ;; we started).
            (bindat--pack-str ,len . ,args)
          (bindat--pack-strz . ,args))))))

(cl-defmethod bindat--type (op (_ (eql 'bits))  len)
  (bindat--pcase op
    ('unpack `(bindat--unpack-bits ,len))
    (`(length . ,_) `(cl-incf bindat-idx ,len))
    (`(pack . ,args) `(bindat--pack-bits ,len . ,args))))

(cl-defmethod bindat--type (_op (_ (eql 'fill))  len)
  `(progn (cl-incf bindat-idx ,len) nil))

(cl-defmethod bindat--type (_op (_ (eql 'align)) len)
  `(progn (cl-callf bindat--align bindat-idx ,len) nil))

(cl-defmethod bindat--type (op (_ (eql 'type)) exp)
  (bindat--pcase op
    ('unpack        `(funcall (bindat--type-ue ,exp)))
    (`(length . ,args) `(funcall (bindat--type-le ,exp) . ,args))
    (`(pack . ,args)   `(funcall (bindat--type-pe ,exp) . ,args))))

(cl-defmethod bindat--type (op (_ (eql 'vec)) count &rest type)
  (unless type (setq type '(byte)))
  (let ((fun (macroexpand-all (bindat--fun type) macroexpand-all-environment)))
    (bindat--pcase op
      ('unpack
       `(let* ((bindat--len ,count)
               (bindat--v (make-vector bindat--len 0)))
          (dotimes (bindat--i bindat--len)
            (aset bindat--v bindat--i (funcall ,fun)))
	  bindat--v))
      ((and `(length . ,_)
            ;; FIXME: Improve the pattern match to recognize more complex
            ;; "constant" functions?
            (let `#'(lambda (,val) (setq bindat-idx (+ bindat-idx ,len))) fun)
            (guard (not (macroexp--fgrep `((,val)) len))))
       ;; Optimize the case where the size of each element is constant.
       `(cl-incf bindat-idx (* ,count ,len)))
      ;; FIXME: It's tempting to use `(mapc (lambda (,val) ,exp) ,val)'
      ;; which would be more efficient when `val' is a list,
      ;; but that's only right if length of `val' is indeed `count'.
      (`(,_ ,val)
       `(dotimes (bindat--i ,count)
	  (funcall ,fun (elt ,val bindat--i)))))))

(cl-defmethod bindat--type (op (_ (eql 'unit)) val)
  (pcase op ('unpack val) (_ nil)))

(cl-defmethod bindat--type (op (_ (eql 'struct)) &rest args)
  (apply #'bindat--type op args))

(cl-defmethod bindat--type (op (_ (eql :pack-var)) var &rest fields)
  (unless (consp (cdr fields))
    (error "`:pack-var VAR' needs to be followed by fields"))
  (bindat--pcase op
    ((or 'unpack (guard (null var)))
     (apply #'bindat--type op fields))
    (`(,_ ,val)
     `(let ((,var ,val)) ,(apply #'bindat--type op fields)))))

(cl-defmethod bindat--type (op (field cons) &rest fields)
  (named-let loop
      ((fields (cons field fields))
       (labels ()))
    (bindat--pcase fields
      ('nil
       (bindat--pcase op
         ('unpack
          (let ((exp ()))
            (pcase-dolist (`(,label . ,labelvar) labels)
              (setq exp
                    (if (eq label '_)
                        (if exp `(nconc ,labelvar ,exp) labelvar)
                      `(cons (cons ',label ,labelvar) ,exp))))
            exp))
         (_ nil)))
      (`(:unpack-val ,exp)
       ;; Make it so `:kwd nil' is the same as the absence of the keyword arg.
       (if exp (pcase op ('unpack exp)) (loop nil labels)))

      (`((,label . ,type) . ,fields)
       (let* ((get-field-val
               (let ((tail (memq :pack-val type)))
                 ;; FIXME: This `TYPE.. :pack EXP' syntax doesn't work well
                 ;; when TYPE is a struct (a list of fields) or with extensions
                 ;; such as allowing TYPE to be `if ...'.
                 (if tail
                     (prog1 (cadr tail)
                       (setq type (butlast type (length tail)))))))
              (fieldvar (make-symbol (format "field%d" (length fields))))
              (labelvar
               (cond
                ((eq label '_) fieldvar)
                ((keywordp label)
                 (intern (substring (symbol-name label) 1)))
                (t label)))
              (field-fun (bindat--fun type))
              (rest-exp (loop fields `((,label . ,labelvar) . ,labels))))
         (bindat--pcase op
           ('unpack
            (let ((code
                   `(let ((,labelvar (funcall ,field-fun)))
                      ,rest-exp)))
              (if (or (eq label '_) (not (assq label labels)))
                  code
                (macroexp-warn-and-return
                 (format "Duplicate label: %S" label)
                 code))))
           (`(,_ ,val)
            ;; `cdr-safe' is easier to optimize (can't signal an error).
            `(let ((,fieldvar ,(or get-field-val
                                   (if (eq label '_) val
                                     `(cdr-safe (assq ',label ,val))))))
               (funcall ,field-fun ,fieldvar)
               ,@(when rest-exp
                   `((let ,(unless (eq labelvar fieldvar)
                             `((,labelvar ,fieldvar)))
                       (ignore ,labelvar)
                       ,rest-exp))))))))
      (_ (error "Unrecognized format in bindat fields: %S" fields)))))

(def-edebug-elem-spec 'bindat-struct
  '([&rest (symbolp bindat-type &optional ":pack-val" def-form)]
    &optional ":unpack-val" def-form))

(def-edebug-elem-spec 'bindat-type
  '(&or ["uint" def-form]
        ["uintr" def-form]
        ["str" def-form]
        ["strz" &optional def-form]
        ["bits" def-form]
        ["fill" def-form]
        ["align" def-form]
        ["vec" def-form bindat-type]
        ["repeat" def-form bindat-type]
        ["type" def-form]
        ["struct" bindat-struct]
        ["unit" def-form]
        [":pack-var" symbolp bindat-type]
        symbolp ;; u8, u16, etc...
        bindat-struct))

(defmacro bindat-type (&rest type)
  "Return the Bindat type value to pack&unpack TYPE.
TYPE is a Bindat type expression.  It can take the following forms:

  uint BITLEN		- Big-endian unsigned integer
  uintr BITLEN		- Little-endian unsigned integer
  str LEN		- Byte string
  strz [LEN]		- Zero-terminated byte-string
  bits LEN		- Bit vector (LEN is counted in bytes)
  fill LEN		- Just a filler
  align LEN		- Fill up to the next multiple of LEN bytes
  vec COUNT TYPE	- COUNT repetitions of TYPE
  type EXP		- Indirection; EXP should return a Bindat type value
  unit EXP              - 0-width type holding the value returned by EXP
  struct FIELDS...      - A composite type

When the context makes it clear, the symbol `struct' can be omitted.
A composite type is a list of FIELDS where each FIELD is of the form

  (LABEL TYPE)

where LABEL can be `_' if the field should not deserve a name.

Composite types get normally packed/unpacked to/from alists, but this can be
controlled in the following way:
- If the list of fields ends with `:unpack-val EXP', then unpacking will
  return the value of EXP (which has the previous fields in its scope).
- If a field's TYPE is followed by `:pack-val EXP', then the value placed
  into this field will be that returned by EXP instead of looking up the alist.
- If the list of fields is preceded with `:pack-var VAR' then the object to
  be packed is bound to VAR when evaluating the EXPs of `:pack-val'.

All the above BITLEN, LEN, COUNT, and EXP are ELisp expressions evaluated
in the current lexical context extended with the previous fields.

TYPE can additionally be one of the Bindat type macros defined with
`bindat-defmacro' (and listed below) or an ELisp expression which returns
a bindat type expression."
  (declare (indent 0) (debug (bindat-type)))
  `(progn
     (defvar bindat-idx)
     (bindat--make :ue ,(bindat--toplevel 'unpack type)
                   :le ,(bindat--toplevel 'length type)
                   :pe ,(bindat--toplevel 'pack   type))))

(eval-and-compile
  (defconst bindat--primitives '(byte uint uintr str strz bits fill align
                                 struct type vec unit)))

(eval-and-compile
  (defvar bindat--macroenv
    (mapcar (lambda (s) (cons s (lambda (&rest args)
                             (bindat--makefun (cons s args)))))
            bindat--primitives)))

(defmacro bindat-defmacro (name args &rest body)
  "Define a new Bindat type as a macro."
  (declare (indent 2) (doc-string 3) (debug (&define name sexp def-body)))
  (let ((leaders ()))
    (while (and (cdr body)
                (or (stringp (car body))
                    (memq (car-safe (car body)) '(:documentation declare))))
      (push (pop body) leaders))
    ;; FIXME: Add support for Edebug decls to those macros.
    `(eval-and-compile ;; Yuck!  But needed to define types where you use them!
       (setf (alist-get ',name bindat--macroenv)
             (lambda ,args ,@(nreverse leaders)
               (bindat--fun ,(macroexp-progn body)))))))

(put 'bindat-type 'function-documentation '(bindat--make-docstring))
(defun bindat--make-docstring ()
  ;; Largely inspired from `pcase--make-docstring'.
  (let* ((main (documentation (symbol-function 'bindat-type) 'raw))
         (ud (help-split-fundoc main 'bindat-type)))
    (require 'help-fns)
    (declare-function help-fns--signature "help-fns")
    (with-temp-buffer
      (insert (or (cdr ud) main))
      (pcase-dolist (`(,name . ,me) (reverse bindat--macroenv))
        (unless (memq name bindat--primitives)
          (let ((doc (documentation me 'raw)))
            (insert "\n\n-- ")
            (setq doc (help-fns--signature name doc me
                                           (indirect-function me)
                                           nil))
            (insert "\n" (or doc "Not documented.")))))
      (let ((combined-doc (buffer-string)))
        (if ud (help-add-fundoc-usage combined-doc (car ud)) combined-doc)))))

(bindat-defmacro u8 () "Unsigned 8bit integer." '(byte))
(bindat-defmacro sint (bitlen r)
  "Signed integer of size BITLEN.
Bigendian if R is nil and little endian if not."
  (let ((bl (make-symbol "bitlen"))
        (max (make-symbol "max"))
        (wrap (make-symbol "wrap")))
    `(let* ((,bl ,bitlen)
            (,max (ash 1 (1- ,bl)))
            (,wrap (+ ,max ,max)))
       (struct :pack-var v
               (n if ,r (uintr ,bl) (uint ,bl)
                  :pack-val (if (< v 0) (+ v ,wrap) v))
               :unpack-val (if (>= n ,max) (- n ,wrap) n)))))

(bindat-defmacro repeat (count &rest type)
  "Like `vec', but unpacks to a list rather than a vector."
  `(:pack-var v
    (v vec ,count ,@type :pack-val v)
    :unpack-val (append v nil)))

(defvar bindat--op nil
  "The operation we're currently building.
This is a simple symbol and can be one of: `unpack', `pack', or `length'.
This is used during macroexpansion of `bindat-type' so that the
macros know which code to generate.
FIXME: this is closely related and very similar to the `op' argument passed
to `bindat--type', yet it's annoyingly different.")

(defun bindat--fun (type)
  (if (or (keywordp (car type)) (consp (car type))) (cons 'struct type)
    type))

(defun bindat--makefun (type)
  (let* ((v (make-symbol "v"))
         (args (pcase bindat--op ('unpack ()) (_ (list v)))))
    (pcase (apply #'bindat--type
                  (pcase bindat--op ('unpack 'unpack) (op `(,op . ,args)))
                  type)
      (`(funcall ,f . ,(pred (equal args))) f)  ;η-reduce.
      (exp `(lambda ,args ,exp)))))

(defun bindat--toplevel (op type)
  (let* ((bindat--op op)
         (env `(,@bindat--macroenv
                ,@macroexpand-all-environment)))
    (macroexpand-all (bindat--fun type) env)))

(provide 'bindat)

;;; bindat.el ends here
