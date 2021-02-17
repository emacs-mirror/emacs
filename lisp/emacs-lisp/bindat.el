;;; bindat.el --- binary data structure packing and unpacking.  -*- lexical-binding: t; -*-

;; Copyright (C) 2002-2021 Free Software Foundation, Inc.

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
;;	unsigned long	dest_ip;
;;	unsigned long	src_ip;
;;	unsigned short	dest_port;
;;	unsigned short	src_port;
;;  };
;;
;;  struct data {
;;	unsigned char	type;
;;	unsigned char	opcode;
;;	unsigned long	length;  /* In little endian order */
;;	unsigned char	id[8];   /* nul-terminated string  */
;;	unsigned char	data[/* (length + 3) & ~3 */];
;;  };
;;
;;  struct packet {
;;	struct header	header;
;;	unsigned char	items;
;;	unsigned char   filler[3];
;;	struct data	item[/* items */];
;;  };
;;
;;  The corresponding Lisp bindat specification looks like this:
;;
;;  (setq header-bindat-spec
;;    (bindat-spec
;;      (dest-ip   ip)
;;	(src-ip    ip)
;;	(dest-port u16)
;;	(src-port  u16)))
;;
;;  (setq data-bindat-spec
;;    (bindat-spec
;;      (type      u8)
;;	(opcode	   u8)
;;	(length	   u16r)  ;; little endian order
;;	(id	   strz 8)
;;	(data	   vec (length))
;;	(align     4)))
;;
;;  (setq packet-bindat-spec
;;    (bindat-spec
;;      (header    struct header-bindat-spec)
;;	(items     u8)
;;	(fill      3)
;;	(item	   repeat (items)
;;		   (struct data-bindat-spec))))
;;
;;
;;  A binary data representation may look like
;;   [ 192 168 1 100 192 168 1 101 01 28 21 32 2 0 0 0
;;     2 3 5 0 ?A ?B ?C ?D ?E ?F 0 0 1 2 3 4 5 0 0 0
;;     1 4 7 0 ?B ?C ?D ?E ?F ?G 0 0 6 7 8 9 10 11 12 0 ]
;;
;;  The corresponding decoded structure looks like
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
;;  bindat-get-field with the structure as first arg followed by a list
;;  of field names and array indexes, e.g. using the data above,
;;    (bindat-get-field decoded-structure 'item 1 'id)
;;  returns "BCDEFG".

;; Binary Data Structure Specification Format
;; ------------------------------------------

;; We recommend using names that end in `-bindat-spec'; such names
;; are recognized automatically as "risky" variables.

;; The data specification is formatted as follows:

;; SPEC    ::= ( ITEM... )

;; ITEM    ::= (  FIELD  TYPE )
;;          |  ( [FIELD] eval FORM )    -- eval FORM for side-effect only
;;          |  ( [FIELD] fill LEN )     -- skip LEN bytes
;;          |  ( [FIELD] align LEN )    -- skip to next multiple of LEN bytes
;;          |  ( [FIELD] struct SPEC_NAME )
;;          |  ( [FIELD] union TAG_VAL (TAG SPEC)... [(t SPEC)] )
;;          |  (  FIELD  repeat ARG ITEM... )

;;          -- In (eval EXPR), the value of the last field is available in
;;             the dynamically bound variable `last' and all the previous
;;             ones in the variable `struct'.

;; TYPE    ::= ( eval EXPR )		-- interpret result as TYPE
;;	    |  u8   | byte		-- length 1
;;          |  u16  | word | short      -- length 2, network byte order
;;          |  u24                      -- 3-byte value
;;          |  u32  | dword | long      -- length 4, network byte order
;;          |  u64                      -- length 8, network byte order
;;          |  u16r | u24r | u32r | u64r - little endian byte order.
;;	    |  str LEN                  -- LEN byte string
;;          |  strz LEN                 -- LEN byte (zero-terminated) string
;;          |  vec LEN [TYPE]           -- vector of LEN items of TYPE (default: u8)
;;          |  ip                       -- 4 byte vector
;;          |  bits LEN                 -- bit vector using LEN bytes.
;;
;;          -- Example: `bits 2' will unpack 0x28 0x1c to (2 3 4 11 13)
;;                                       and 0x1c 0x28 to (3 5 10 11 12).

;; FIELD   ::= ( eval EXPR )		-- use result as NAME
;;          |  NAME

;; LEN     ::= ARG
;;          |  <omitted> | nil		-- LEN = 1


;; TAG_VAL ::= ARG

;; TAG     ::= LISP_CONSTANT
;;          |  ( eval EXPR )		-- return non-nil if tag match;
;;					   current TAG_VAL in `tag'.

;; ARG     ::= ( eval EXPR )		-- interpret result as ARG
;;          |  INTEGER_CONSTANT
;;          |  DEREF

;; DEREF   ::= ( [NAME | INTEGER]... )	-- Field NAME or Array index relative
;;                                         to current structure spec.
;;                                      -- see bindat-get-field

;; A `union' specification
;;    ([FIELD] union TAG_VAL (TAG SPEC) ... [(t SPEC)])
;; is interpreted by evalling TAG_VAL and then comparing that to
;; each TAG using equal; if a match is found, the corresponding SPEC
;; is used.
;; If TAG is a form (eval EXPR), EXPR is eval'ed with `tag' bound to the
;; value of TAG_VAL; the corresponding SPEC is used if the result is non-nil.
;; Finally, if TAG is t, the corresponding SPEC is used unconditionally.
;;
;; An `eval' specification
;;  ([FIELD] eval FORM)
;; is interpreted by evalling FORM for its side effects only.
;; If FIELD is specified, the value is bound to that field.
;; The FORM may access and update `bindat-raw' and `bindat-idx' (see `bindat-unpack').

;;; Code:

;; Helper functions for structure unpacking.
;; Relies on dynamic binding of `bindat-raw' and `bindat-idx'.

(defvar bindat-raw)
(defvar bindat-idx)

(defun bindat--unpack-u8 ()
  (prog1
      (aref bindat-raw bindat-idx)
    (setq bindat-idx (1+ bindat-idx))))

(defun bindat--unpack-u16 ()
  (logior (ash (bindat--unpack-u8) 8) (bindat--unpack-u8)))

(defun bindat--unpack-u24 ()
  (logior (ash (bindat--unpack-u16) 8) (bindat--unpack-u8)))

(defun bindat--unpack-u32 ()
  (logior (ash (bindat--unpack-u16) 16) (bindat--unpack-u16)))

(defun bindat--unpack-u64 ()
  (logior (ash (bindat--unpack-u32) 32) (bindat--unpack-u32)))

(defun bindat--unpack-u16r ()
  (logior (bindat--unpack-u8) (ash (bindat--unpack-u8) 8)))

(defun bindat--unpack-u24r ()
  (logior (bindat--unpack-u16r) (ash (bindat--unpack-u8) 16)))

(defun bindat--unpack-u32r ()
  (logior (bindat--unpack-u16r) (ash (bindat--unpack-u16r) 16)))

(defun bindat--unpack-u64r ()
  (logior (bindat--unpack-u32r) (ash (bindat--unpack-u32r) 32)))

(defun bindat--unpack-item (type len &optional vectype)
  (if (eq type 'ip)
      (setq type 'vec len 4))
  (pcase type
   ((or 'u8 'byte)
    (bindat--unpack-u8))
   ((or 'u16 'word 'short)
    (bindat--unpack-u16))
   ('u24 (bindat--unpack-u24))
   ((or 'u32 'dword 'long)
    (bindat--unpack-u32))
   ('u64  (bindat--unpack-u64))
   ('u16r (bindat--unpack-u16r))
   ('u24r (bindat--unpack-u24r))
   ('u32r (bindat--unpack-u32r))
   ('u64r (bindat--unpack-u64r))
   ('bits
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
   ('str
    (let ((s (substring bindat-raw bindat-idx (+ bindat-idx len))))
      (setq bindat-idx (+ bindat-idx len))
      (if (stringp s) s
	(apply #'unibyte-string s))))
   ('strz
    (let ((i 0) s)
      (while (and (< i len) (/= (aref bindat-raw (+ bindat-idx i)) 0))
	(setq i (1+ i)))
      (setq s (substring bindat-raw bindat-idx (+ bindat-idx i)))
      (setq bindat-idx (+ bindat-idx len))
      (if (stringp s) s
	(apply #'unibyte-string s))))
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

(defun bindat--unpack-group (spec)
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
	  (while (/= (% bindat-idx len) 0)
	    (setq bindat-idx (1+ bindat-idx))))
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
    struct))

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
		     (nth (car field) struct)
		   (let ((val (assq (car field) struct)))
		     (if (consp val) (cdr val)))))
    (setq field (cdr field)))
  struct)

;;;; Calculate bindat-raw length of structured data

(defvar bindat--fixed-length-alist
  '((u8 . 1) (byte . 1)
    (u16 . 2) (u16r . 2) (word . 2) (short . 2)
    (u24 . 3) (u24r . 3)
    (u32 . 4) (u32r . 4) (dword . 4) (long . 4)
    (u64 . 8) (u64r . 8)
    (ip . 4)))

(defun bindat--length-group (struct spec)
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
	  (while (/= (% bindat-idx len) 0)
	    (setq bindat-idx (1+ bindat-idx))))
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
	  (setq bindat-idx (+ bindat-idx len))))))))

(defun bindat-length (spec struct)
  "Calculate `bindat-raw' length for STRUCT according to bindat SPEC."
  (let ((bindat-idx 0))
    (bindat--length-group struct spec)
    bindat-idx))


;;;; Pack structured data into bindat-raw

(defun bindat--pack-u8 (v)
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

(defun bindat--pack-item (v type len &optional vectype)
  (if (eq type 'ip)
      (setq type 'vec len 4))
  (pcase type
   ((guard (null v))
    (setq bindat-idx (+ bindat-idx len)))
   ((or 'u8 'byte)
    (bindat--pack-u8 v))
   ((or 'u16 'word 'short)
    (bindat--pack-u16 v))
   ('u24
    (bindat--pack-u24 v))
   ((or 'u32 'dword 'long)
    (bindat--pack-u32 v))
   ('u64  (bindat--pack-u64 v))
   ('u16r (bindat--pack-u16r v))
   ('u24r (bindat--pack-u24r v))
   ('u32r (bindat--pack-u32r v))
   ('u64r (bindat--pack-u64r v))
   ('bits
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
   ((or 'str 'strz)
    (dotimes (i (min len (length v)))
      (aset bindat-raw (+ bindat-idx i) (aref v i)))
    (setq bindat-idx (+ bindat-idx len)))
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
	  (while (/= (% bindat-idx len) 0)
	    (setq bindat-idx (1+ bindat-idx))))
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
	  ))))))

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

;;;; Debugging support

(def-edebug-elem-spec 'bindat-spec '(&rest bindat-item))


(def-edebug-elem-spec 'bindat--item-aux
  ;; Field types which can come without a field label.
  '(&or ["eval" form]
        ["fill" bindat-len]
        ["align" bindat-len]
        ["struct" form]          ;A reference to another bindat-spec.
        ["union" bindat-tag-val &rest (bindat-tag bindat-spec)]))

(def-edebug-elem-spec 'bindat-item
  '((&or bindat--item-aux               ;Without label..
         [bindat-field                  ;..or with label
          &or bindat--item-aux
              ["repeat" bindat-arg bindat-spec]
              bindat-type])))

(def-edebug-elem-spec 'bindat-type
  '(&or ("eval" form)
        ["str"  bindat-len]
        ["strz" bindat-len]
        ["vec"  bindat-len &optional bindat-type]
        ["bits" bindat-len]
        symbolp))

(def-edebug-elem-spec 'bindat-field
  '(&or ("eval" form) symbolp))

(def-edebug-elem-spec 'bindat-len '(&or [] "nil" bindat-arg))

(def-edebug-elem-spec 'bindat-tag-val '(bindat-arg))

(def-edebug-elem-spec 'bindat-tag '(&or ("eval" form) atom))

(def-edebug-elem-spec 'bindat-arg
  '(&or ("eval" form) integerp (&rest symbolp integerp)))

(defmacro bindat-spec (&rest fields)
  "Build the bindat spec described by FIELDS."
  (declare (indent 0) (debug (bindat-spec)))
  ;; FIXME: We should really "compile" this to a triplet of functions!
  `',fields)

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

(provide 'bindat)

;;; bindat.el ends here
