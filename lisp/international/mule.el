;;; mule.el --- basic commands for multilingual environment

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;;   Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: mule, multilingual, character set, coding system

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

(defconst mule-version "6.0 (HANACHIRUSATO)" "\
Version number and name of this version of MULE (multilingual environment).")

(defconst mule-version-date "2003.9.1" "\
Distribution date of this version of MULE (multilingual environment).")



;;; CHARACTER
(defalias 'char-valid-p 'characterp)
(make-obsolete 'char-valid-p 'characterp "22.1")


;;; CHARSET

(defun define-charset (name docstring &rest props)
  "Define NAME (symbol) as a charset with DOCSTRING.
The remaining arguments must come in pairs ATTRIBUTE VALUE.  ATTRIBUTE
may be any symbol.  The following have special meanings, and one of
`:code-offset', `:map', `:subset', `:superset' must be specified.

`:short-name'

VALUE must be a short string to identify the charset.  If omitted,
NAME is used.

`:long-name'

VALUE must be a string longer than `:short-name' to identify the
charset.  If omitted, the value of the `:short-name' attribute is used.

`:dimension'

VALUE must be an integer 0, 1, 2, or 3, specifying the dimension of
code-points of the charsets.  If omitted, it is calculated from the
value of the `:code-space' attribute.

`:code-space'

VALUE must be a vector of length at most 8 specifying the byte code
range of each dimension in this format:
	[ MIN-1 MAX-1 MIN-2 MAX-2 ... ]
where MIN-N is the minimum byte value of Nth dimension of code-point,
MAX-N is the maximum byte value of that.

`:min-code'

VALUE must be an integer specifying the mininum code point of the
charset.  If omitted, it is calculated from `:code-space'.  VALUE may
be a cons (HIGH . LOW), where HIGH is the most significant 16 bits of
the code point and LOW is the least significant 16 bits.

`:max-code'

VALUE must be an integer specifying the maxinum code point of the
charset.  If omitted, it is calculated from `:code-space'.  VALUE may
be a cons (HIGH . LOW), where HIGH is the most significant 16 bits of
the code point and LOW is the least significant 16 bits.

`:iso-final-char'

VALUE must be a character in the range 32 to 127 (inclusive)
specifying the final char of the charset for ISO-2022 encoding.  If
omitted, the charset can't be encoded by ISO-2022 based
coding-systems.

`:iso-revision-number'

VALUE must be an integer in the range 0..63, specifying the revision
number of the charset for ISO-2022 encoding.

`:emacs-mule-id'

VALUE must be an integer of 0, 128..255.  If omitted, the charset
can't be encoded by coding-systems of type `emacs-mule'.

`:ascii-compatible-p'

VALUE must be nil or t (default nil).  If VALUE is t, the charset is
compatible with ASCII, i.e. the first 128 code points map to ASCII.

`:supplementary-p'

VALUE must be nil or t.  If the VALUE is t, the charset is
supplementary, which means it is used only as a parent of some other
charset.

`:invalid-code'

VALUE must be a nonnegative integer that can be used as an invalid
code point of the charset.  If the minimum code is 0 and the maximum
code is greater than Emacs' maximum integer value, `:invalid-code'
should not be omitted.

`:code-offset'

VALUE must be an integer added to the index number of a character to
get the corresponding character code.

`:map'

VALUE must be vector or string.

If it is a vector, the format is [ CODE-1 CHAR-1 CODE-2 CHAR-2 ... ],
where CODE-n is a code-point of the charset, and CHAR-n is the
corresponding character code.

If it is a string, it is a name of file that contains the above
information.   Each line of the file must be this format:
	0xXXX 0xYYY
where XXX is a hexadecimal representation of CODE-n and YYY is a
hexadecimal representation of CHAR-n.  A line starting with `#' is a
comment line.

`:subset'

VALUE must be a list:
	( PARENT MIN-CODE MAX-CODE OFFSET )
PARENT is a parent charset.  MIN-CODE and MAX-CODE specify the range
of characters inherited from the parent.  OFFSET is an integer value
to add to a code point of the parent charset to get the corresponding
code point of this charset.

`:superset'

VALUE must be a list of parent charsets.  The charset inherits
characters from them.  Each element of the list may be a cons (PARENT
. OFFSET), where PARENT is a parent charset, and OFFSET is an offset
value to add to a code point of PARENT to get the corresponding code
point of this charset.

`:unify-map'

VALUE must be vector or string.

If it is a vector, the format is [ CODE-1 CHAR-1 CODE-2 CHAR-2 ... ],
where CODE-n is a code-point of the charset, and CHAR-n is the
corresponding Unicode character code.

If it is a string, it is a name of file that contains the above
information.  The file format is the same as what described for `:map'
attribute."
  (let ((attrs (mapcar 'list '(:dimension
			       :code-space
			       :min-code
			       :max-code
			       :iso-final-char
			       :iso-revision-number
			       :emacs-mule-id
			       :ascii-compatible-p
			       :supplementary-p
			       :invalid-code
			       :code-offset
			       :map
			       :subset
			       :superset
			       :unify-map
			       :plist))))

    ;; If :dimension is omitted, get the dimension from :code-space.
    (let ((dimension (plist-get props :dimension)))
      (or dimension
	  (progn
	    (setq dimension (/ (length (plist-get props :code-space)) 2))
	    (setq props (plist-put props :dimension dimension)))))

    (dolist (slot attrs)
      (setcdr slot (plist-get props (car slot))))

    ;; Make sure that the value of :code-space is a vector of 8
    ;; elements.
    (let* ((slot (assq :code-space attrs))
	   (val (cdr slot))
	   (len (length val)))
      (if (< len 8)
	  (setcdr slot
		  (vconcat val (make-vector (- 8 len) 0)))))

    ;; Add :name and :docstring properties to PROPS.
    (setq props
	  (cons :name (cons name (cons :docstring (cons docstring props)))))
    (or (plist-get props :short-name)
	(plist-put props :short-name (symbol-name name)))
    (or (plist-get props :long-name)
	(plist-put props :long-name (plist-get props :short-name)))
    ;; We can probably get a worthwhile amount in purespace.
    (setq props
	  (mapcar (lambda (elt)
		    (if (stringp elt)
			(purecopy elt)
		      elt))
		  props))
    (setcdr (assq :plist attrs) props)

    (apply 'define-charset-internal name (mapcar 'cdr attrs))))


(defun load-with-code-conversion (fullname file &optional noerror nomessage)
  "Execute a file of Lisp code named FILE whose absolute name is FULLNAME.
The file contents are decoded before evaluation if necessary.
If optional second arg NOERROR is non-nil,
 report no error if FILE doesn't exist.
Print messages at start and end of loading unless
 optional third arg NOMESSAGE is non-nil.
Return t if file exists."
  (if (null (file-readable-p fullname))
      (and (null noerror)
	   (signal 'file-error (list "Cannot open load file" file)))
    ;; Read file with code conversion, and then eval.
    (let* ((buffer
	    ;; To avoid any autoloading, set default-major-mode to
	    ;; fundamental-mode.
	    ;; So that we don't get completely screwed if the
	    ;; file is encoded in some complicated character set,
	    ;; read it with real decoding, as a multibyte buffer,
	    ;; even if this is a --unibyte Emacs session.
	    (let ((default-major-mode 'fundamental-mode)
		  (default-enable-multibyte-characters t))
	      ;; We can't use `generate-new-buffer' because files.el
	      ;; is not yet loaded.
	      (get-buffer-create (generate-new-buffer-name " *load*"))))
	   (load-in-progress t)
	   (source (save-match-data (string-match "\\.el\\'" fullname))))
      (unless nomessage
	(if source
	    (message "Loading %s (source)..." file)
	  (message "Loading %s..." file)))
      (when purify-flag
	(push file preloaded-file-list))
      (unwind-protect
	  (let ((load-file-name fullname)
		(set-auto-coding-for-load t)
		(inhibit-file-name-operation nil))
	    (save-excursion
	      (set-buffer buffer)
	      (insert-file-contents fullname)
	      ;; If the loaded file was inserted with no-conversion or
	      ;; raw-text coding system, make the buffer unibyte.
	      ;; Otherwise, eval-buffer might try to interpret random
	      ;; binary junk as multibyte characters.
	      (if (and enable-multibyte-characters
		       (or (eq (coding-system-type last-coding-system-used)
			       'raw-text)))
		  (set-buffer-multibyte nil))
	      ;; Make `kill-buffer' quiet.
	      (set-buffer-modified-p nil))
	    ;; Have the original buffer current while we eval.
	    (eval-buffer buffer nil file
			 ;; If this Emacs is running with --unibyte,
			 ;; convert multibyte strings to unibyte
			 ;; after reading them.
;;			 (not default-enable-multibyte-characters)
			 nil t
			 ))
	(let (kill-buffer-hook kill-buffer-query-functions)
	  (kill-buffer buffer)))
      (let ((hook (assoc file after-load-alist)))
	(when hook
	  (mapcar (function eval) (cdr hook))))
      (unless (or nomessage noninteractive)
	(if source
	    (message "Loading %s (source)...done" file)
	  (message "Loading %s...done" file)))
      t)))

(defun charset-info (charset)
  "Return a vector of information of CHARSET.
This function is provided for backward compatibility.

The elements of the vector are:
	CHARSET-ID, BYTES, DIMENSION, CHARS, WIDTH, DIRECTION,
	LEADING-CODE-BASE, LEADING-CODE-EXT,
	ISO-FINAL-CHAR, ISO-GRAPHIC-PLANE,
	REVERSE-CHARSET, SHORT-NAME, LONG-NAME,	DESCRIPTION,
	PLIST.
where
CHARSET-ID is always 0.
BYTES is always 0.
DIMENSION is the number of bytes of a code-point of the charset:
  1, 2, 3, or 4.
CHARS is the number of characters in a dimension:
  94, 96, 128, or 256.
WIDTH is always 0.
DIRECTION is always 0.
LEADING-CODE-BASE is always 0.
LEADING-CODE-EXT is always 0.
ISO-FINAL-CHAR (character) is the final character of the
  corresponding ISO 2022 charset.  If the charset is not assigned
  any final character, the value is -1.
ISO-GRAPHIC-PLANE is always 0.
REVERSE-CHARSET is always -1.
SHORT-NAME (string) is the short name to refer to the charset.
LONG-NAME (string) is the long name to refer to the charset
DESCRIPTION (string) is the description string of the charset.
PLIST (property list) may contain any type of information a user
  want to put and get by functions `put-charset-property' and
  `get-charset-property' respectively."
  (vector 0
	  0
	  (charset-dimension charset)
	  (charset-chars charset)
	  0
	  0
	  0
	  0
	  (charset-iso-final-char charset)
	  0
	  -1
	  (get-charset-property charset :short-name)
	  (get-charset-property charset :short-name)
	  (charset-description charset)
	  (charset-plist charset)))

;; It is better not to use backquote in this file,
;; because that makes a bootstrapping problem
;; if you need to recompile all the Lisp files using interpreted code.

(defun charset-id (charset)
  "Always return 0.  This is provided for backward compatibility."
  0)

(defmacro charset-bytes (charset)
  "Always return 0.  This is provided for backward compatibility."
  0)

(defun get-charset-property (charset propname)
  "Return the value of CHARSET's PROPNAME property.
This is the last value stored with
 (put-charset-property CHARSET PROPNAME VALUE)."
  (plist-get (charset-plist charset) propname))

(defun put-charset-property (charset propname value)
  "Set CHARSETS's PROPNAME property to value VALUE.
It can be retrieved with `(get-charset-property CHARSET PROPNAME)'."
  (set-charset-plist charset
		     (plist-put (charset-plist charset) propname value)))

(defun charset-description (charset)
  "Return description string of CHARSET."
  (plist-get (charset-plist charset) :docstring))

(defun charset-dimension (charset)
  "Return dimension of CHARSET."
  (plist-get (charset-plist charset) :dimension))

(defun charset-chars (charset &optional dimension)
  "Return number of characters contained in DIMENSION of CHARSET.
DIMENSION defaults to the first dimension."
  (unless dimension (setq dimension 1))
  (let ((code-space (plist-get (charset-plist charset) :code-space)))
    (1+ (- (aref code-space (1- (* 2 dimension)))
	   (aref code-space (- (* 2 dimension) 2))))))

(defun charset-iso-final-char (charset)
  "Return ISO-2022 final character of CHARSET.
Return -1 if charset isn't an ISO 2022 one."
  (or (plist-get (charset-plist charset) :iso-final-char)
      -1))

(defmacro charset-short-name (charset)
  "Return short name of CHARSET."
  (plist-get (charset-plist charset) :short-name))

(defmacro charset-long-name (charset)
  "Return long name of CHARSET."
  (plist-get (charset-plist charset) :long-name))

(defun charset-list ()
  "Return list of all charsets ever defined.

This function is provided for backward compatibility.
Now we have the variable `charset-list'."
  charset-list)
(make-obsolete 'charset-list "Use variable `charset-list'" "22.1")

(defun generic-char-p (char)
  "Always return nil.  This is provided for backward compatibility."
  nil)
(make-obsolete 'generic-char-p "Generic characters no longer exist" "22.1")

;; Coding system stuff

;; Coding system is a symbol that has been defined by the function
;; `define-coding-system'.

(defconst coding-system-iso-2022-flags
  '(long-form
    ascii-at-eol
    ascii-at-cntl
    7-bit
    locking-shift
    single-shift
    designation
    revision
    direction
    init-at-bol
    designate-at-bol
    safe
    latin-extra
    composition
    euc-tw-shift
    use-roman
    use-oldjis)
  "List of symbols that control ISO-2022 encoder/decoder.

The value of the `:flags' attribute in the argument of the function
`define-coding-system' must be one of them.

If `long-form' is specified, use a long designation sequence on
encoding for the charsets `japanese-jisx0208-1978', `chinese-gb2312',
and `japanese-jisx0208'.  The long designation sequence doesn't
conform to ISO 2022, but is used by such coding systems as
`compound-text'.

If `ascii-at-eol' is specified, designate ASCII to g0 at end of line
on encoding.

If `ascii-at-cntl' is specified, designate ASCII to g0 before control
codes and SPC on encoding.

If `7-bit' is specified, use 7-bit code only on encoding.

If `locking-shift' is specified, decode locking-shift code correctly
on decoding, and use locking-shift to invoke a graphic element on
encoding.

If `single-shift' is specified, decode single-shift code correctly on
decoding, and use single-shift to invoke a graphic element on encoding.

If `designation' is specified, decode designation code correctly on
decoding, and use designation to designate a charset to a graphic
element on encoding.

If `revision' is specified, produce an escape sequence to specify
revision number of a charset on encoding.  Such an escape sequence is
always correctly decoded on decoding.

If `direction' is specified, decode ISO6429's code for specifying
direction correctly, and produce the code on encoding.

If `init-at-bol' is specified, on encoding, it is assumed that
invocation and designation statuses are reset at each beginning of
line even if `ascii-at-eol' is not specified; thus no codes for
resetting them are produced.

If `safe' is specified, on encoding, characters not supported by a
coding are replaced with `?'.

If `latin-extra' is specified, the code-detection routine assumes that a
code specified in `latin-extra-code-table' (which see) is valid.

If `composition' is specified, an escape sequence to specify
composition sequence is correctly decoded on decoding, and is produced
on encoding.

If `euc-tw-shift' is specified, the EUC-TW specific shifting code is
correctly decoded on decoding, and is produced on encoding.

If `use-roman' is specified, JIS0201-1976-Roman is designated instead
of ASCII.

If `use-oldjis' is specified, JIS0208-1976 is designated instead of
JIS0208-1983.")

(defun define-coding-system (name docstring &rest props)
  "Define NAME (a symbol) as a coding system with DOCSTRING and attributes.
The remaining arguments must come in pairs ATTRIBUTE VALUE.  ATTRIBUTE
may be any symbol.

The following attributes have special meanings.  Those labeled as
\"(required)\", should not be omitted.

`:mnemonic' (required)

VALUE is a character to display on mode line for the coding system.

`:coding-type' (required)

VALUE must be one of `charset', `utf-8', `utf-16', `iso-2022',
`emacs-mule', `shift-jis', `ccl', `raw-text', `undecided'.

`:eol-type'

VALUE is the EOL (end-of-line) format of the coding system.  It must be
one of `unix', `dos', `mac'.  The symbol `unix' means Unix-like EOL
\(i.e. single LF), `dos' means DOS-like EOL \(i.e. sequence of CR LF),
and `mac' means MAC-like EOL \(i.e. single CR).  If omitted, on
decoding by the coding system, Emacs automatically detects the EOL
format of the source text.

`:charset-list'

VALUE must be a list of charsets supported by the coding system.  On
encoding by the coding system, if a character belongs to multiple
charsets in the list, a charset that comes earlier in the list is
selected.  If `:coding-type' is `iso-2022', VALUE may be `iso-2022',
which indicates that the coding system supports all ISO-2022 based
charsets.  If `:coding-type' is `emacs-mule', VALUE may be
`emacs-mule', which indicates that the coding system supports all
charsets that have the `:emacs-mule-id' property.

`:ascii-compatible-p'

If VALUE is non-nil, the coding system decodes all 7-bit bytes into
the corresponding ASCII characters, and encodes all ASCII characters
back to the corresponding 7-bit bytes.  VALUE defaults to nil.

`:decode-translation-table'

VALUE must be a translation table to use on decoding.

`:encode-translation-table'

VALUE must be a translation table to use on encoding.

`:post-read-conversion'

VALUE must be a function to call after some text is inserted and
decoded by the coding system itself and before any functions in
`after-insert-functions' are called.  The arguments to this function
are the same as those of a function in `after-insert-file-functions',
i.e. LENGTH of the text to be decoded with point at the head of it,
and the function should leave point unchanged.

`:pre-write-conversion'

VALUE must be a function to call after all functions in
`write-region-annotate-functions' and `buffer-file-format' are called,
and before the text is encoded by the coding system itself.  The
arguments to this function are the same as those of a function in
`write-region-annotate-functions'.

`:default-char'

VALUE must be a character.  On encoding, a character not supported by
the coding system is replaced with VALUE.

`:for-unibyte'

VALUE non-nil means that visiting a file with the coding system
results in a unibyte buffer.

`:eol-type'

VALUE must be `unix', `dos', `mac'.  The symbol `unix' means Unix-like
EOL (LF), `dos' means DOS-like EOL (CRLF), and `mac' means MAC-like
EOL (CR).  If omitted, on decoding, the coding system detects EOL
format automatically, and on encoding, uses Unix-like EOL.

`:mime-charset'

VALUE must be a symbol whose name is that of a MIME charset converted
to lower case.

`:mime-text-unsuitable'

VALUE non-nil means the `:mime-charset' property names a charset which
is unsuitable for the top-level media type \"text\".

`:flags'

VALUE must be a list of symbols that control the ISO-2022 converter.
Each must be a member of the list `coding-system-iso-2022-flags'
\(which see).  This attribute has a meaning only when `:coding-type'
is `iso-2022'.

`:designation'

VALUE must be a vector [G0-USAGE G1-USAGE G2-USAGE G3-USAGE].
GN-USAGE specifies the usage of graphic register GN as follows.

If it is nil, no charset can be designated to GN.

If it is a charset, the charset is initially designated to GN, and
never used by the other charsets.

If it is a list, the elements must be charsets, nil, 94, or 96.  GN
can be used by all the listed charsets.  If the list contains 94, any
iso-2022 charset whose code-space ranges are 94 long can be designated
to GN.  If the list contains 96, any charsets whose whose ranges are
96 long can be designated to GN.  If the first element is a charset,
that charset is initially designated to GN.

This attribute has a meaning only when `:coding-type' is `iso-2022'.

`:bom'

This attributes specifies whether the coding system uses a `byte order
mark'.  VALUE must nil, t, or cons of coding systems whose
`:coding-type' is `utf-16'.

If the value is nil, on decoding, don't treat the first two-byte as
BOM, and on encoding, don't produce BOM bytes.

If the value is t, on decoding, skip the first two-byte as BOM, and on
encoding, produce BOM bytes accoding to the value of `:endian'.

If the value is cons, on decoding, check the first two-byte.  If theyq
are 0xFE 0xFF, use the car part coding system of the value.  If they
are 0xFF 0xFE, use the car part coding system of the value.
Otherwise, treat them as bytes for a normal character.  On encoding,
produce BOM bytes accoding to the value of `:endian'.

This attribute has a meaning only when `:coding-type' is `utf-16'.

`:endian'

VALUE must be `big' or `little' specifying big-endian and
little-endian respectively.  The default value is `big'.

This attribute has a meaning only when `:coding-type' is `utf-16'.

`:ccl-decoder'

VALUE is a symbol representing the registered CCL program used for
decoding.  This attribute has a meaning only when `:coding-type' is
`ccl'.

`:ccl-encoder'

VALUE is a symbol representing the registered CCL program used for
encoding.  This attribute has a meaning only when `:coding-type' is
`ccl'."
  (let* ((common-attrs (mapcar 'list
			       '(:mnemonic
				 :coding-type
				 :charset-list
				 :ascii-compatible-p
				 :decode-translation-table
				 :encode-translation-table
				 :post-read-conversion
				 :pre-write-conversion
				 :default-char
				 :for-unibyte
				 :plist
				 :eol-type)))
	 (coding-type (plist-get props :coding-type))
	 (spec-attrs (mapcar 'list
			     (cond ((eq coding-type 'iso-2022)
				    '(:initial
				      :reg-usage
				      :request
				      :flags))
				   ((eq coding-type 'utf-16)
				    '(:bom
				      :endian))
				   ((eq coding-type 'ccl)
				    '(:ccl-decoder
				      :ccl-encoder
				      :valids))))))

    (dolist (slot common-attrs)
      (setcdr slot (plist-get props (car slot))))

    (dolist (slot spec-attrs)
      (setcdr slot (plist-get props (car slot))))

    (if (eq coding-type 'iso-2022)
	(let ((designation (plist-get props :designation))
	      (flags (plist-get props :flags))
	      (initial (make-vector 4 nil))
	      (reg-usage (cons 4 4))
	      request elt)
	  (dotimes (i 4)
	    (setq elt (aref designation i))
	    (cond ((charsetp elt)
		   (aset initial i elt)
		   (setq request (cons (cons elt i) request)))
		  ((consp elt)
		   (aset initial i (car elt))
		   (if (charsetp (car elt))
		       (setq request (cons (cons (car elt) i) request)))
		   (dolist (e (cdr elt))
		     (cond ((charsetp e)
			    (setq request (cons (cons e i) request)))
			   ((eq e 94)
			    (setcar reg-usage i))
			   ((eq e 96)
			    (setcdr reg-usage i))
			   ((eq e t)
			    (setcar reg-usage i)
			    (setcdr reg-usage i)))))))
	  (setcdr (assq :initial spec-attrs) initial)
	  (setcdr (assq :reg-usage spec-attrs) reg-usage)
	  (setcdr (assq :request spec-attrs) request)

	  ;; Change :flags value from a list to a bit-mask.
	  (let ((bits 0)
		(i 0))
	    (dolist (elt coding-system-iso-2022-flags)
	      (if (memq elt flags)
		  (setq bits (logior bits (lsh 1 i))))
	      (setq i (1+ i)))
	    (setcdr (assq :flags spec-attrs) bits))))

    ;; Add :name and :docstring properties to PROPS.
    (setq props
	  (cons :name (cons name (cons :docstring (cons (purecopy docstring)
							props)))))
    (setcdr (assq :plist common-attrs) props)
    (apply 'define-coding-system-internal 
	   name (mapcar 'cdr (append common-attrs spec-attrs)))))

(defun coding-system-doc-string (coding-system)
  "Return the documentation string for CODING-SYSTEM."
  (plist-get (coding-system-plist coding-system) :docstring))

(defun coding-system-mnemonic (coding-system)
  "Return the mnemonic character of CODING-SYSTEM.
The mnemonic character of a coding system is used in mode line to
indicate the coding system.  If CODING-SYSTEM. is nil, return ?=."
  (plist-get (coding-system-plist coding-system) :mnemonic))

(defun coding-system-type (coding-system)
  "Return the coding type of CODING-SYSTEM.
A coding type is a symbol indicating the encoding method of CODING-SYSTEM.
See the function `define-coding-system' for more detail."
  (plist-get (coding-system-plist coding-system) :coding-type))

(defun coding-system-charset-list (coding-system)
  "Return list of charsets supported by CODING-SYSTEM.
If CODING-SYSTEM supports all ISO-2022 charsets, return `iso-2022'.
If CODING-SYSTEM supports all emacs-mule charsets, return `emacs-mule'."
  (plist-get (coding-system-plist coding-system) :charset-list))

(defun coding-system-category (coding-system)
  "Return a category symbol of CODING-SYSTEM."
  (plist-get (coding-system-plist coding-system) :category))

(defun coding-system-get (coding-system prop)
  "Extract a value from CODING-SYSTEM's property list for property PROP.
For compatibility with Emacs 20/21, this accepts old-style symbols
like `mime-charset' as well as the current style like `:mime-charset'."
  (or (plist-get (coding-system-plist coding-system) prop)
      (if (not (keywordp prop))
	  (plist-get (coding-system-plist coding-system)
		     (intern (concat ":" (symbol-name prop)))))))

(defun coding-system-put (coding-system prop val)
  "Change value in CODING-SYSTEM's property list PROP to VAL."
  (plist-put (coding-system-plist coding-system) prop val))

(defalias 'coding-system-parent 'coding-system-base)
(make-obsolete 'coding-system-parent 'coding-system-base "20.3")

(defun coding-system-eol-type-mnemonic (coding-system)
  "Return the string indicating end-of-line format of CODING-SYSTEM."
  (let* ((eol-type (coding-system-eol-type coding-system))
	 (val (cond ((eq eol-type 0) eol-mnemonic-unix)
		    ((eq eol-type 1) eol-mnemonic-dos)
		    ((eq eol-type 2) eol-mnemonic-mac)
		    (t eol-mnemonic-undecided))))
    (if (stringp val)
	val
      (char-to-string val))))

(defun coding-system-lessp (x y)
  (cond ((eq x 'no-conversion) t)
	((eq y 'no-conversion) nil)
	((eq x 'emacs-mule) t)
	((eq y 'emacs-mule) nil)
	((eq x 'undecided) t)
	((eq y 'undecided) nil)
	(t (let ((c1 (coding-system-mnemonic x))
		 (c2 (coding-system-mnemonic y)))
	     (or (< (downcase c1) (downcase c2))
		 (and (not (> (downcase c1) (downcase c2)))
		      (< c1 c2)))))))

(defun add-to-coding-system-list (coding-system)
  "Add CODING-SYSTEM to `coding-system-list' while keeping it sorted."
  (if (or (null coding-system-list)
	  (coding-system-lessp coding-system (car coding-system-list)))
      (setq coding-system-list (cons coding-system coding-system-list))
    (let ((len (length coding-system-list))
	  mid (tem coding-system-list))
      (while (> len 1)
	(setq mid (nthcdr (/ len 2) tem))
	(if (coding-system-lessp (car mid) coding-system)
	    (setq tem mid
		  len (- len (/ len 2)))
	  (setq len (/ len 2))))
      (setcdr tem (cons coding-system (cdr tem))))))

(defun coding-system-list (&optional base-only)
  "Return a list of all existing non-subsidiary coding systems.
If optional arg BASE-ONLY is non-nil, only base coding systems are
listed.  The value doesn't include subsidiary coding systems which are
made from bases and aliases automatically for various end-of-line
formats (e.g. iso-latin-1-unix, koi8-r-dos)."
  (let* ((codings (copy-sequence coding-system-list))
	 (tail (cons nil codings)))
    ;; Remove subsidiary coding systems (eol variants) and alias
    ;; coding systems (if necessary).
    (while (cdr tail)
      (let* ((coding (car (cdr tail)))
	     (aliases (coding-system-aliases coding)))
	(if (or
	     ;; CODING is an eol variant if not in ALIASES.
	     (not (memq coding aliases))
	     ;; CODING is an alias if it is not car of ALIASES.
	     (and base-only (not (eq coding (car aliases)))))
	    (setcdr tail (cdr (cdr tail)))
	  (setq tail (cdr tail)))))
    codings))

(defconst char-coding-system-table nil
  "This is an obsolete variable.
It exists just for backward compatibility, and the value is always nil.")

(defun transform-make-coding-system-args (name type &optional doc-string props)
  "For internal use only.
Transform XEmacs style args for `make-coding-system' to Emacs style.
Value is a list of transformed arguments."
  (let ((mnemonic (string-to-char (or (plist-get props 'mnemonic) "?")))
	(eol-type (plist-get props 'eol-type))
	properties tmp)
    (cond
     ((eq eol-type 'lf) (setq eol-type 'unix))
     ((eq eol-type 'crlf) (setq eol-type 'dos))
     ((eq eol-type 'cr) (setq eol-type 'mac)))
    (if (setq tmp (plist-get props 'post-read-conversion))
	(setq properties (plist-put properties 'post-read-conversion tmp)))
    (if (setq tmp (plist-get props 'pre-write-conversion))
	(setq properties (plist-put properties 'pre-write-conversion tmp)))
    (cond
     ((eq type 'shift-jis)
      `(,name 1 ,mnemonic ,doc-string () ,properties ,eol-type))
     ((eq type 'iso2022) ; This is not perfect.
      (if (plist-get props 'escape-quoted)
	  (error "escape-quoted is not supported: %S"
		 `(,name ,type ,doc-string ,props)))
      (let ((g0 (plist-get props 'charset-g0))
      	    (g1 (plist-get props 'charset-g1))
      	    (g2 (plist-get props 'charset-g2))
      	    (g3 (plist-get props 'charset-g3))
      	    (use-roman
             (and
	      (eq (cadr (assoc 'latin-jisx0201
			       (plist-get props 'input-charset-conversion)))
		  'ascii)
	      (eq (cadr (assoc 'ascii
			       (plist-get props 'output-charset-conversion)))
		  'latin-jisx0201)))
            (use-oldjis
             (and
	      (eq (cadr (assoc 'japanese-jisx0208-1978
			       (plist-get props 'input-charset-conversion)))
		  'japanese-jisx0208)
	      (eq (cadr (assoc 'japanese-jisx0208
			       (plist-get props 'output-charset-conversion)))
		  'japanese-jisx0208-1978))))
	(if (charsetp g0)
	    (if (plist-get props 'force-g0-on-output)
		(setq g0 `(nil ,g0))
	      (setq g0 `(,g0 t))))
	(if (charsetp g1)
	    (if (plist-get props 'force-g1-on-output)
		(setq g1 `(nil ,g1))
	      (setq g1 `(,g1 t))))
	(if (charsetp g2)
	    (if (plist-get props 'force-g2-on-output)
		(setq g2 `(nil ,g2))
	      (setq g2 `(,g2 t))))
	(if (charsetp g3)
	    (if (plist-get props 'force-g3-on-output)
		(setq g3 `(nil ,g3))
	      (setq g3 `(,g3 t))))
	`(,name 2 ,mnemonic ,doc-string
	  (,g0 ,g1 ,g2 ,g3
	   ,(plist-get props 'short)
	   ,(not (plist-get props 'no-ascii-eol))
	   ,(not (plist-get props 'no-ascii-cntl))
	   ,(plist-get props 'seven)
	   t
	   ,(not (plist-get props 'lock-shift))
	   ,use-roman
	   ,use-oldjis
	   ,(plist-get props 'no-iso6429)
	   nil nil nil nil)
	,properties ,eol-type)))
     ((eq type 'big5)
      `(,name 3 ,mnemonic ,doc-string () ,properties ,eol-type))
     ((eq type 'ccl)
      `(,name 4 ,mnemonic ,doc-string
	      (,(plist-get props 'decode) . ,(plist-get props 'encode))
	      ,properties ,eol-type))
     (t
      (error "unsupported XEmacs style make-coding-style arguments: %S"
	     `(,name ,type ,doc-string ,props))))))

(defun make-coding-system (coding-system type mnemonic doc-string
					 &optional
					 flags
					 properties
					 eol-type)
  "Define a new coding system CODING-SYSTEM (symbol).
This function is provided for backward compatibility.
Use `define-coding-system' instead."
  ;; For compatiblity with XEmacs, we check the type of TYPE.  If it
  ;; is a symbol, perhaps, this function is called with XEmacs-style
  ;; arguments.  Here, try to transform that kind of arguments to
  ;; Emacs style.
  (if (symbolp type)
      (let ((args (transform-make-coding-system-args coding-system type
						     mnemonic doc-string)))
	(setq coding-system (car args)
	      type (nth 1 args)
	      mnemonic (nth 2 args)
	      doc-string (nth 3 args)
	      flags (nth 4 args)
	      properties (nth 5 args)
	      eol-type (nth 6 args))))

  (setq type
	(cond ((eq type 0) 'emacs-mule)
	      ((eq type 1) 'shift-jis)
	      ((eq type 2) 'iso2022)
	      ((eq type 3) 'big5)
	      ((eq type 4) 'ccl)
	      ((eq type 5) 'raw-text)
	      (t
	       (error "Invalid coding system type: %s" type))))

  (setq properties
	(let ((plist nil) key)
	  (dolist (elt properties)
	    (setq key (car elt))
	    (cond ((eq key 'post-read-conversion)
		   (setq key :post-read-conversion))
		  ((eq key 'pre-write-conversion)
		   (setq key :pre-write-conversion))
		  ((eq key 'translation-table-for-decode)
		   (setq key :decode-translation-table))
		  ((eq key 'translation-table-for-encode)
		   (setq key :encode-translation-table))
		  ((eq key 'safe-charsets)
		   (setq key :charset-list))
		  ((eq key 'mime-charset)
		   (setq key :mime-charset))
		  ((eq key 'valid-codes)
		   (setq key :valids)))
	    (setq plist (plist-put plist key (cdr elt))))
	  plist))
  (plist-put properties :mnemonic mnemonic)
  (plist-put properties :coding-type type)
  (cond ((eq eol-type 0) (setq eol-type 'unix))
	((eq eol-type 1) (setq eol-type 'dos))
	((eq eol-type 2) (setq eol-type 'mac))
	((vectorp eol-type) (setq eol-type nil)))
  (plist-put properties :eol-type eol-type)

  (cond
   ((eq type 'iso2022)
    (plist-put properties :flags
	       (list (and (or (consp (nth 0 flags))
			      (consp (nth 1 flags))
			      (consp (nth 2 flags))
			      (consp (nth 3 flags))) 'designation)
		     (or (nth 4 flags) 'long-form)
		     (and (nth 5 flags) 'ascii-at-eol)
		     (and (nth 6 flags) 'ascii-at-cntl)
		     (and (nth 7 flags) '7-bit)
		     (and (nth 8 flags) 'locking-shift)
		     (and (nth 9 flags) 'single-shift)
		     (and (nth 10 flags) 'use-roman)
		     (and (nth 11 flags) 'use-oldjis)
		     (or (nth 12 flags) 'direction)
		     (and (nth 13 flags) 'init-at-bol)
		     (and (nth 14 flags) 'designate-at-bol)
		     (and (nth 15 flags) 'safe)
		     (and (nth 16 flags) 'latin-extra)))
    (plist-put properties :designation
	       (let ((vec (make-vector 4 nil)))
		 (dotimes (i 4)
		   (let ((spec (nth i flags)))
		     (if (eq spec t)
			 (aset vec i '(94 96))
		     (if (consp spec)
			 (progn
			   (if (memq t spec)
			       (setq spec (append (delq t spec) '(94 96))))
			   (aset vec i spec))))))
		 vec)))

   ((eq type 'ccl)
    (plist-put properties :ccl-decoder (car flags))
    (plist-put properties :ccl-encoder (cdr flags))))

  (apply 'define-coding-system coding-system doc-string properties))

(defun merge-coding-systems (first second)
  "Fill in any unspecified aspects of coding system FIRST from SECOND.
Return the resulting coding system."
  (let ((base (coding-system-base second))
	(eol (coding-system-eol-type second)))
    ;; If FIRST doesn't specify text conversion, merge with that of SECOND.
    (if (eq (coding-system-base first) 'undecided)
	(setq first (coding-system-change-text-conversion first base)))
    ;; If FIRST doesn't specify eol conversion, merge with that of SECOND.
    (if (and (vectorp (coding-system-eol-type first))
	     (numberp eol) (>= eol 0) (<= eol 2))
	(setq first (coding-system-change-eol-conversion
		     first eol)))
    first))

(defun set-buffer-file-coding-system (coding-system &optional force)
  "Set the file coding-system of the current buffer to CODING-SYSTEM.
This means that when you save the buffer, it will be converted
according to CODING-SYSTEM.  For a list of possible values of CODING-SYSTEM,
use \\[list-coding-systems].

If CODING-SYSTEM leaves the text conversion unspecified, or if it
leaves the end-of-line conversion unspecified, FORCE controls what to
do.  If FORCE is nil, get the unspecified aspect (or aspects) from the
buffer's previous `buffer-file-coding-system' value (if it is
specified there).  Otherwise, levae it unspecified.

This marks the buffer modified so that the succeeding \\[save-buffer]
surely saves the buffer with CODING-SYSTEM.  From a program, if you
don't want to mark the buffer modified, just set the variable
`buffer-file-coding-system' directly."
  (interactive "zCoding system for saving file (default, nil): \nP")
  (check-coding-system coding-system)
  (if (and coding-system buffer-file-coding-system (null force))
      (setq coding-system
	    (merge-coding-systems coding-system buffer-file-coding-system)))
  (setq buffer-file-coding-system coding-system)
  ;; This is in case of an explicit call.  Normally, `normal-mode' and
  ;; `set-buffer-major-mode-hook' take care of setting the table.
  (if (fboundp 'ucs-set-table-for-input) ; don't lose when building
      (ucs-set-table-for-input))
  (set-buffer-modified-p t)
  (force-mode-line-update))

(defun revert-buffer-with-coding-system (coding-system &optional force)
  "Visit the current buffer's file again using coding system CODING-SYSTEM.
For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems].

If CODING-SYSTEM leaves the text conversion unspecified, or if it
leaves the end-of-line conversion unspecified, FORCE controls what to
do.  If FORCE is nil, get the unspecified aspect (or aspects) from the
buffer's previous `buffer-file-coding-system' value (if it is
specified there).  Otherwise, determine it from the file contents as
usual for visiting a file."
  (interactive "zCoding system for visited file (default, nil): \nP")
  (check-coding-system coding-system)
  (if (and coding-system buffer-file-coding-system (null force))
      (setq coding-system
	    (merge-coding-systems coding-system buffer-file-coding-system)))
  (let ((coding-system-for-read coding-system))
    (revert-buffer)))

(defun set-file-name-coding-system (coding-system)
  "Set coding system for decoding and encoding file names to CODING-SYSTEM.
It actually just set the variable `file-name-coding-system' (which
see) to CODING-SYSTEM."
  (interactive "zCoding system for file names (default, nil): ")
  (check-coding-system coding-system)
  (setq file-name-coding-system coding-system))

(defvar default-terminal-coding-system nil
  "Default value for the terminal coding system.
This is normally set according to the selected language environment.
See also the command `set-terminal-coding-system'.")

(defun set-terminal-coding-system (coding-system)
  "Set coding system of your terminal to CODING-SYSTEM.
All text output to the terminal will be encoded
with the specified coding system.
For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems].
The default is determined by the selected language environment
or by the previous use of this command."
  (interactive
   (list (let ((default (if (and (not (terminal-coding-system))
				 default-terminal-coding-system)
			    default-terminal-coding-system)))
	   (read-coding-system
	    (format "Coding system for terminal display (default, %s): "
		    default)
	    default))))
  (if (and (not coding-system)
	   (not (terminal-coding-system)))
      (setq coding-system default-terminal-coding-system))
  (if coding-system
      (setq default-terminal-coding-system coding-system))
  (set-terminal-coding-system-internal coding-system)
  (redraw-frame (selected-frame)))

(defvar default-keyboard-coding-system nil
  "Default value of the keyboard coding system.
This is normally set according to the selected language environment.
See also the command `set-keyboard-coding-system'.")

(defun set-keyboard-coding-system (coding-system)
  "Set coding system for keyboard input to CODING-SYSTEM.
In addition, this command enables Encoded-kbd minor mode.
\(If CODING-SYSTEM is nil, Encoded-kbd mode is turned off -- see
`encoded-kbd-mode'.)
For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems].
The default is determined by the selected language environment
or by the previous use of this command."
  (interactive
   (list (let ((default (if (and (not (keyboard-coding-system))
				 default-keyboard-coding-system)
			    default-keyboard-coding-system)))
	   (read-coding-system
	    (format "Coding system for keyboard input (default, %s): "
		    default)
	    default))))
  (if (and (not coding-system)
	   (not (keyboard-coding-system)))
      (setq coding-system default-keyboard-coding-system))
  (if coding-system
      (setq default-keyboard-coding-system coding-system))
  (set-keyboard-coding-system-internal coding-system)
  (setq keyboard-coding-system coding-system)
  (encoded-kbd-mode (if coding-system 1 0)))

(defcustom keyboard-coding-system nil
  "Specify coding system for keyboard input.
If you set this on a terminal which can't distinguish Meta keys from
8-bit characters, you will have to use ESC to type Meta characters.
See Info node `Specify Coding' and Info node `Single-Byte Character Support'.

On non-windowing terminals, this is set from the locale by default.

Setting this variable directly does not take effect;
use either M-x customize or \\[set-keyboard-coding-system]."
  :type '(coding-system :tag "Coding system")
  :link '(info-link "(emacs)Specify Coding")
  :link '(info-link "(emacs)Single-Byte Character Support")
  :set (lambda (symbol value)
	 ;; Don't load encoded-kbd-mode unnecessarily.
	 (if (or value (boundp 'encoded-kbd-mode))
	     (set-keyboard-coding-system value)
	   (set-default 'keyboard-coding-system nil))) ; must initialize
  :version "21.4"
  :group 'keyboard
  :group 'mule)

(defun set-buffer-process-coding-system (decoding encoding)
  "Set coding systems for the process associated with the current buffer.
DECODING is the coding system to be used to decode input from the process,
ENCODING is the coding system to be used to encode output to the process.

For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems]."
  (interactive
   "zCoding-system for output from the process: \nzCoding-system for input to the process: ")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (null proc)
	(error "No process")
      (check-coding-system decoding)
      (check-coding-system encoding)
      (set-process-coding-system proc decoding encoding)))
  (force-mode-line-update))

(defalias 'set-clipboard-coding-system 'set-selection-coding-system)

(defun set-selection-coding-system (coding-system)
  "Make CODING-SYSTEM used for communicating with other X clients.
When sending or receiving text via cut_buffer, selection, and clipboard,
the text is encoded or decoded by CODING-SYSTEM."
  (interactive "zCoding system for X selection: ")
  (check-coding-system coding-system)
  (setq selection-coding-system coding-system))

;; Coding system lastly specified by the command
;; set-next-selection-coding-system.
(defvar last-next-selection-coding-system nil)

(defun set-next-selection-coding-system (coding-system)
  "Use CODING-SYSTEM for next communication with other window system clients.
This setting is effective for the next communication only."
  (interactive
   (list (read-coding-system
	  (if last-next-selection-coding-system
	      (format "Coding system for the next selection (default, %S): "
		      last-next-selection-coding-system)
	    "Coding system for the next selection: ")
	  last-next-selection-coding-system)))
  (if coding-system
      (setq last-next-selection-coding-system coding-system)
    (setq coding-system last-next-selection-coding-system))
  (check-coding-system coding-system)

  (setq next-selection-coding-system coding-system))

(defun set-coding-priority (arg)
  "Set priority of coding categories according to ARG.
ARG is a list of coding categories ordered by priority.

This function is provided for backward compatibility.
Now we have more convenient function `set-coding-system-priority'."
  (apply 'set-coding-system-priority
	 (mapcar #'(lambda (x) (symbol-value x)) arg)))
(make-obsolete 'set-coding-priority 'set-coding-system-priority "22.1")

;;; X selections

(defvar ctext-non-standard-encodings-alist
  '(("ISO8859-15" . latin-iso8859-15)
    ("ISO8859-14" . latin-iso8859-14)
    ("KOI8-R" . koi8-r)
    ("BIG5-0" . big5))
  "Alist of non-standard encoding names vs Emacs coding systems.
This alist is used to decode an extened segment of a compound text.")

(defvar ctext-non-standard-encodings-regexp
  (string-to-multibyte
   (concat
    ;; For non-standard encodings.
    "\\(\e%/[0-4][\200-\377][\200-\377]\\([^\002]+\\)\002\\)"
    "\\|"
    ;; For UTF-8 encoding.
    "\\(\e%G[^\e]*\e%@\\)")))

;; Functions to support "Non-Standard Character Set Encodings" defined
;; by the COMPOUND-TEXT spec.
;; We support that by decoding the whole data by `ctext' which just
;; pertains byte sequences belonging to ``extended segment'', then
;; decoding those byte sequences one by one in Lisp.
;; This function also supports "The UTF-8 encoding" described in the
;; section 7 of the documentation fo COMPOUND-TEXT distributed with
;; XFree86.

(defun ctext-post-read-conversion (len)
  "Decode LEN characters encoded as Compound Text with Extended Segments."
  ;; We don't need the following because it is expected that this
  ;; function is mainly used for decoding X selection which is not
  ;; that big data.
  ;;(buffer-disable-undo) ; minimize consing due to insertions and deletions
  (save-match-data
    (save-restriction
      (narrow-to-region (point) (+ (point) len))
      (let ((case-fold-search nil)
	    last-coding-system-used
	    pos bytes)
	(decode-coding-region (point-min) (point-max) 'ctext)
	(while (re-search-forward ctext-non-standard-encodings-regexp
				  nil 'move)
	  (setq pos (match-beginning 0))
	  (if (match-beginning 1)
	      ;; ESC % / [0-4] M L --ENCODING-NAME-- \002 --BYTES--
	      (let* ((M (char-after (+ pos 4)))
		     (L (char-after (+ pos 5)))
		     (encoding (match-string 2))
		     (coding (or (cdr (assoc-ignore-case 
				       encoding
				       ctext-non-standard-encodings-alist))
				 (coding-system-p
				  (intern (downcase encoding))))))
		(if enable-multibyte-characters
		    (setq M (multibyte-char-to-unibyte M)
			  L (multibyte-char-to-unibyte L)))
		(setq bytes (- (+ (* (- M 128) 128) (- L 128))
			       (- (point) (+ pos 6))))
		(when coding
		  (delete-region pos (point))
		  (forward-char bytes)
		  (decode-coding-region (- (point) bytes) (point) coding)))
	    ;; ESC % G --UTF-8-BYTES-- ESC % @
	    (setq bytes (- (point) pos))
	    (decode-coding-region (- (point) bytes) (point) 'utf-8))))
      (goto-char (point-min))
      (- (point-max) (point)))))

;; If you add charsets here, be sure to modify the regexp used by
;; ctext-pre-write-conversion to look up non-standard charsets.
(defvar ctext-non-standard-designations-alist
  '(("$(0" . (big5 "big5-0" 2))
    ("$(1" . (big5 "big5-0" 2))
    ;; The following are actually standard; generating extended
    ;; segments for them is wrong and screws e.g. Latin-9 users.
    ;; 8859-{10,13,16} aren't Emacs charsets anyhow.  -- fx
;;     ("-V"  . (t "iso8859-10" 1))
;;     ("-Y"  . (t "iso8859-13" 1))
;;     ("-_"  . (t "iso8859-14" 1))
;;     ("-b"  . (t "iso8859-15" 1))
;;     ("-f"  . (t "iso8859-16" 1))
    )
  "Alist of ctext control sequences that introduce character sets which
are not in the list of approved encodings, and the corresponding
coding system, identifier string, and number of octets per encoded
character.

Each element has the form (CTLSEQ . (ENCODING CHARSET NOCTETS)).  CTLSEQ
is the control sequence (sans the leading ESC) that introduces the character
set in the text encoded by compound-text.  ENCODING is a coding system
symbol; if it is t, it means that the ctext coding system already encodes
the text correctly, and only the leading control sequence needs to be altered.
If ENCODING is a coding system, we need to re-encode the text with that
coding system.  CHARSET is the name of the charset we need to put into
the leading control sequence.  NOCTETS is the number of octets (bytes) that
encode each character in this charset.  NOCTETS can be 0 (meaning the number
of octets per character is variable), 1, 2, 3, or 4.")

(defun ctext-pre-write-conversion (from to)
  "Encode characters between FROM and TO as Compound Text w/Extended Segments.

If FROM is a string, or if the current buffer is not the one set up for us
by encode-coding-string, generate a new temp buffer, insert the
text, and convert it in the temporary buffer.  Otherwise, convert in-place."
  (save-match-data
    ;; Setup a working buffer if necessary.
    (when (stringp from)
      (set-buffer (generate-new-buffer " *temp"))
      (set-buffer-multibyte (multibyte-string-p from))
      (insert from))

    ;; Now we can encode the whole buffer.
    (let ((case-fold-search nil)
	  last-coding-system-used
	  pos posend desig encode-info encoding chset noctets textlen)
      (goto-char (point-min))
      ;; At first encode the whole buffer.
      (encode-coding-region (point-min) (point-max) 'ctext-no-compositions)
      ;; Then replace ISO-2022 charset designations with extended
      ;; segments, for those charsets that are not part of the
      ;; official X registry.  The regexp below finds the leading
      ;; sequences for big5.
      (while (re-search-forward "\e\\(\$([01]\\)" nil 'move)
	(setq pos (match-beginning 0)
	      posend (point)
	      desig (match-string 1)
	      encode-info (cdr (assoc desig
				      ctext-non-standard-designations-alist))
	      encoding (car encode-info)
	      chset (cadr encode-info)
	      noctets (car (cddr encode-info)))
	(skip-chars-forward "^\e")
	(cond
	 ((eq encoding t) ; only the leading sequence needs to be changed
	  (setq textlen (+ (- (point) posend) (length chset) 1))
	  ;; Generate the control sequence for an extended segment.
	  (replace-match (string-to-multibyte (format "\e%%/%d%c%c%s"
						      noctets
						      (+ (/ textlen 128) 128)
						      (+ (% textlen 128) 128)
						      chset))
			 t t))
	 ((coding-system-p encoding) ; need to recode the entire segment...
	  (decode-coding-region pos (point) 'ctext-no-compositions)
	  (encode-coding-region pos (point) encoding)
	  (setq textlen (+ (- (point) pos) (length chset) 1))
	  (save-excursion
	    (goto-char pos)
	    (insert (string-to-multibyte (format "\e%%/%d%c%c%s"
						 noctets
						 (+ (/ textlen 128) 128)
						 (+ (% textlen 128) 128)
						 chset)))))))
      (goto-char (point-min))))
  ;; Must return nil, as build_annotations_2 expects that.
  nil)

;;; FILE I/O

(defcustom auto-coding-alist
  '(("\\.\\(arc\\|zip\\|lzh\\|zoo\\|jar\\|sx[dmicw]\\|tar\\)\\'" . no-conversion-multibyte)
    ("\\.tgz\\'" . no-conversion)
    ("\\.\\(gz\\|Z\\|bz\\|bz2\\|gpg\\)\\'" . no-conversion)
    ("/#[^/]+#\\'" . emacs-mule))
  "Alist of filename patterns vs corresponding coding systems.
Each element looks like (REGEXP . CODING-SYSTEM).
A file whose name matches REGEXP is decoded by CODING-SYSTEM on reading.

The settings in this alist take priority over `coding:' tags
in the file (see the function `set-auto-coding')
and the contents of `file-coding-system-alist'."
  :group 'files
  :group 'mule
  :type '(repeat (cons (regexp :tag "File name regexp")
		       (symbol :tag "Coding system"))))

(defcustom auto-coding-regexp-alist
  '(("^BABYL OPTIONS:[ \t]*-\\*-[ \t]*rmail[ \t]*-\\*-" . no-conversion)
    ("\\`;ELC   " . emacs-mule))	; Emacs 20-compiled
  "Alist of patterns vs corresponding coding systems.
Each element looks like (REGEXP . CODING-SYSTEM).
A file whose first bytes match REGEXP is decoded by CODING-SYSTEM on reading.

The settings in this alist take priority over `coding:' tags
in the file (see the function `set-auto-coding')
and the contents of `file-coding-system-alist'."
  :group 'files
  :group 'mule
  :type '(repeat (cons (regexp :tag "Regexp")
		       (symbol :tag "Coding system"))))

;; See the bottom of this file for built-in auto coding functions.
(defcustom auto-coding-functions '(sgml-xml-auto-coding-function
				   sgml-html-meta-auto-coding-function)
  "A list of functions which attempt to determine a coding system.

Each function in this list should be written to operate on the
current buffer, but should not modify it in any way.  The buffer
will contain undecoded text of parts of the file.  Each function
should take one argument, SIZE, which says how many
characters (starting from point) it should look at.

If one of these functions succeeds in determining a coding
system, it should return that coding system.  Otherwise, it
should return nil.

If a file has a `coding:' tag, that takes precedence over these
functions, so they won't be called at all."
  :group 'files
  :group 'mule
  :type '(repeat function))

(defvar set-auto-coding-for-load nil
  "Non-nil means look for `load-coding' property instead of `coding'.
This is used for loading and byte-compiling Emacs Lisp files.")

(defun auto-coding-alist-lookup (filename)
  "Return the coding system specified by `auto-coding-alist' for FILENAME."
  (let ((alist auto-coding-alist)
	(case-fold-search (memq system-type '(vax-vms windows-nt ms-dos cygwin)))
	coding-system)
    (while (and alist (not coding-system))
      (if (string-match (car (car alist)) filename)
	  (setq coding-system (cdr (car alist)))
	(setq alist (cdr alist))))
    coding-system))

(defun set-auto-coding (filename size)
  "Return coding system for a file FILENAME of which SIZE bytes follow point.
These bytes should include at least the first 1k of the file
and the last 3k of the file, but the middle may be omitted.

The function checks FILENAME against the variable `auto-coding-alist'.
If FILENAME doesn't match any entries in the variable, it checks the
contents of the current buffer following point against
`auto-coding-regexp-alist'.  If no match is found, it checks for a
`coding:' tag in the first one or two lines following point.  If no
`coding:' tag is found, it checks any local variables list in the last
3K bytes out of the SIZE bytes.  Finally, if none of these methods
succeed, it checks to see if any function in `auto-coding-functions'
gives a match.

The return value is the specified coding system, or nil if nothing is
specified.

The variable `set-auto-coding-function' (which see) is set to this
function by default."
  (or (auto-coding-alist-lookup filename)
      ;; Try using `auto-coding-regexp-alist'.
      (save-excursion
	(let ((alist auto-coding-regexp-alist)
	      coding-system)
	  (while (and alist (not coding-system))
	    (let ((regexp (car (car alist))))
	      (when (re-search-forward regexp (+ (point) size) t)
		(setq coding-system (cdr (car alist)))))
	    (setq alist (cdr alist)))
	  coding-system))
      (let* ((case-fold-search t)
	     (head-start (point))
	     (head-end (+ head-start (min size 1024)))
	     (tail-start (+ head-start (max (- size 3072) 0)))
	     (tail-end (+ head-start size))
	     coding-system head-found tail-found pos)
	;; Try a short cut by searching for the string "coding:"
	;; and for "unibyte:" at the head and tail of SIZE bytes.
	(setq head-found (or (search-forward "coding:" head-end t)
			     (search-forward "unibyte:" head-end t)))
	(if (and head-found (> head-found tail-start))
	    ;; Head and tail are overlapped.
	    (setq tail-found head-found)
	  (goto-char tail-start)
	  (setq tail-found (or (search-forward "coding:" tail-end t)
			       (search-forward "unibyte:" tail-end t))))

	;; At first check the head.
	(when head-found
	  (goto-char head-start)
	  (setq head-end (set-auto-mode-1))
	  (setq head-start (point))
	  (when (and head-end (< head-found head-end))
	    (goto-char head-start)
	    (when (and set-auto-coding-for-load
		       (re-search-forward
			"\\(.*;\\)?[ \t]*unibyte:[ \t]*\\([^ ;]+\\)"
			head-end t))
	      (setq coding-system 'raw-text))
	    (when (and (not coding-system)
		       (re-search-forward
			"\\(.*;\\)?[ \t]*coding:[ \t]*\\([^ ;]+\\)"
			head-end t))
	      (setq coding-system (intern (match-string 2)))
	      (or (coding-system-p coding-system)
		  (setq coding-system nil)))))

	;; If no coding: tag in the head, check the tail.
	(when (and tail-found (not coding-system))
	  (goto-char tail-start)
	  (search-forward "\n\^L" nil t)
	  (if (re-search-forward
	       "^\\(.*\\)[ \t]*Local Variables:[ \t]*\\(.*\\)$" tail-end t)
	  ;; The prefix is what comes before "local variables:" in its
	   ;; line.  The suffix is what comes after "local variables:"
	      ;; in its line.
	      (let* ((prefix (regexp-quote (match-string 1)))
		     (suffix (regexp-quote (match-string 2)))
		     (re-coding
		      (concat
		       "^" prefix
		       ;; N.B. without the \n below, the regexp can
		       ;; eat newlines.
		       "[ \t]*coding[ \t]*:[ \t]*\\([^ \t\n]+\\)[ \t]*"
		       suffix "$"))
		     (re-unibyte
		      (concat
		       "^" prefix
		       "[ \t]*unibyte[ \t]*:[ \t]*\\([^ \t\n]+\\)[ \t]*"
		       suffix "$"))
		     (re-end
		      (concat "^" prefix "[ \t]*End *:[ \t]*" suffix "$"))
		     (pos (point)))
		(re-search-forward re-end tail-end 'move)
		(setq tail-end (point))
		(goto-char pos)
		(when (and set-auto-coding-for-load
			   (re-search-forward re-unibyte tail-end t))
		  (setq coding-system 'raw-text))
		(when (and (not coding-system)
			   (re-search-forward re-coding tail-end t))
		  (setq coding-system (intern (match-string 1)))
		  (or (coding-system-p coding-system)
		      (setq coding-system nil))))))
	coding-system)
      ;; Finally, try all the `auto-coding-functions'.
      (let ((funcs auto-coding-functions)
	    (coding-system nil))
	(while (and funcs (not coding-system))
	  (setq coding-system (condition-case e
				  (save-excursion
				    (goto-char (point-min))
				    (funcall (pop funcs) size))
				(error nil))))
	coding-system)))

(setq set-auto-coding-function 'set-auto-coding)

(defun after-insert-file-set-coding (inserted)
  "Set `buffer-file-coding-system' of current buffer after text is inserted.
INSERTED is the number of characters that were inserted, as figured
in the situation before this function.  Return the number of characters
inserted, as figured in the situation after.  The two numbers can be
different if the buffer has become unibyte."
  (if last-coding-system-used
      (let ((coding-system
	     (find-new-buffer-file-coding-system last-coding-system-used))
	    (modified-p (buffer-modified-p)))
	(when coding-system
	  (set-buffer-file-coding-system coding-system t)
	  (set-buffer-modified-p modified-p))))
  inserted)

;; The coding-spec and eol-type of coding-system returned is decided
;; independently in the following order.
;;	1. That of buffer-file-coding-system locally bound.
;;	2. That of CODING.

(defun find-new-buffer-file-coding-system (coding)
  "Return a coding system for a buffer when a file of CODING is inserted.
The local variable `buffer-file-coding-system' of the current buffer
is set to the returned value.
Return nil if there's no need to set `buffer-file-coding-system'."
  (let (local-coding local-eol
	found-coding found-eol
	new-coding new-eol)
    (if (null coding)
	;; Nothing found about coding.
	nil

      ;; Get information of `buffer-file-coding-system' in LOCAL-EOL
      ;; and LOCAL-CODING.
      (setq local-eol (coding-system-eol-type buffer-file-coding-system))
      (if (null (numberp local-eol))
	  ;; But eol-type is not yet set.
	  (setq local-eol nil))
      (if (and buffer-file-coding-system
	       (not (eq (coding-system-type buffer-file-coding-system)
			'undecided)))
	  (setq local-coding (coding-system-base buffer-file-coding-system)))

      (if (and (local-variable-p 'buffer-file-coding-system)
	       local-eol local-coding)
	  ;; The current buffer has already set full coding-system, we
	  ;; had better not change it.
	  nil

	(setq found-eol (coding-system-eol-type coding))
	(if (null (numberp found-eol))
  	    ;; But eol-type is not found.
	    ;; If EOL conversions are inhibited, force unix eol-type.
	    (setq found-eol (if inhibit-eol-conversion 0)))
	(setq found-coding (coding-system-base coding))

	(if (and (not found-eol) (eq found-coding 'undecided))
	    ;; No valid coding information found.
	    nil

	  ;; Some coding information (eol or text) found.

	  ;; The local setting takes precedence over the found one.
	  (setq new-coding (if (local-variable-p 'buffer-file-coding-system)
			       (or local-coding found-coding)
			     (or found-coding local-coding)))
	  (setq new-eol (if (local-variable-p 'buffer-file-coding-system)
			    (or local-eol found-eol)
			  (or found-eol local-eol)))

	  (let ((eol-type (coding-system-eol-type new-coding)))
	    (if (and (numberp new-eol) (vectorp eol-type))
		(aref eol-type new-eol)
	      new-coding)))))))

(defun modify-coding-system-alist (target-type regexp coding-system)
  "Modify one of look up tables for finding a coding system on I/O operation.
There are three of such tables, `file-coding-system-alist',
`process-coding-system-alist', and `network-coding-system-alist'.

TARGET-TYPE specifies which of them to modify.
If it is `file', it affects `file-coding-system-alist' (which see).
If it is `process', it affects `process-coding-system-alist' (which see).
If it is `network', it affects `network-coding-system-alist' (which see).

REGEXP is a regular expression matching a target of I/O operation.
The target is a file name if TARGET-TYPE is `file', a program name if
TARGET-TYPE is `process', or a network service name or a port number
to connect to if TARGET-TYPE is `network'.

CODING-SYSTEM is a coding system to perform code conversion on the I/O
operation, or a cons cell (DECODING . ENCODING) specifying the coding systems
for decoding and encoding respectively,
or a function symbol which, when called, returns such a cons cell."
  (or (memq target-type '(file process network))
      (error "Invalid target type: %s" target-type))
  (or (stringp regexp)
      (and (eq target-type 'network) (integerp regexp))
      (error "Invalid regular expression: %s" regexp))
  (if (symbolp coding-system)
      (if (not (fboundp coding-system))
	  (progn
	    (check-coding-system coding-system)
	    (setq coding-system (cons coding-system coding-system))))
    (check-coding-system (car coding-system))
    (check-coding-system (cdr coding-system)))
  (cond ((eq target-type 'file)
	 (let ((slot (assoc regexp file-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq file-coding-system-alist
		   (cons (cons regexp coding-system)
			 file-coding-system-alist)))))
	((eq target-type 'process)
	 (let ((slot (assoc regexp process-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq process-coding-system-alist
		   (cons (cons regexp coding-system)
			 process-coding-system-alist)))))
	(t
	 (let ((slot (assoc regexp network-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq network-coding-system-alist
		   (cons (cons regexp coding-system)
			 network-coding-system-alist)))))))

(defun decode-coding-inserted-region (from to filename
					   &optional visit beg end replace)
  "Decode the region between FROM and TO as if it is read from file FILENAME.
Optional arguments VISIT, BEG, END, and REPLACE are the same as those
of the function `insert-file-contents'."
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let ((coding coding-system-for-read))
	(or coding
	    (setq coding (funcall set-auto-coding-function
				  filename (- (point-max) (point-min)))))
	(or coding
	    (setq coding (find-operation-coding-system
			  'insert-file-contents
			  filename visit beg end replace)))
	(if (coding-system-p coding)
	    (or enable-multibyte-characters
		(setq coding
		      (coding-system-change-text-conversion coding 'raw-text)))
	  (setq coding nil))
	(if coding
	    (decode-coding-region (point-min) (point-max) coding))
	(setq last-coding-system-used coding)))))

(defun make-translation-table (&rest args)
  "Make a translation table from arguments.
A translation table is a char table intended for character
translation in CCL programs.

Each argument is a list of elements of the form (FROM . TO), where FROM
is a character to be translated to TO.

The arguments and forms in each argument are processed in the given
order, and if a previous form already translates TO to some other
character, say TO-ALT, FROM is also translated to TO-ALT."
  (let ((table (make-char-table 'translation-table))
	revlist)
    (dolist (elts args)
      (dolist (elt elts)
	(let ((from (car elt))
	      (to (cdr elt))
	      to-alt rev-from rev-to)
	  ;; If we have already translated TO to TO-ALT, FROM should
	  ;; also be translated to TO-ALT.
	  (if (setq to-alt (aref table to))
	      (setq to to-alt))
	  (aset table from to)
	  ;; If we have already translated some chars to FROM, they
	  ;; should also be translated to TO.
	  (when (setq rev-from (assq from revlist))
	    (dolist (elt (cdr rev-from))
	      (aset table elt to))
	    (setq revlist (delq rev-from revlist)
		  rev-from (cdr rev-from)))
	  ;; Now update REVLIST.
	  (setq rev-to (assq to revlist))
	  (if rev-to
	      (setcdr rev-to (cons from (cdr rev-to)))
	    (setq rev-to (list to from)
		  revlist (cons rev-to revlist)))
	  (if rev-from
	      (setcdr rev-to (append rev-from (cdr rev-to)))))))
    ;; Return TABLE just created.
    table))

(defun make-translation-table-from-vector (vec)
  "Make translation table from decoding vector VEC.
VEC is an array of 256 elements to map unibyte codes to multibyte
characters.  Elements may be nil for undefined code points.
See also the variable `nonascii-translation-table'."
  (let ((table (make-char-table 'translation-table))
	(rev-table (make-char-table 'translation-table))
	ch)
    (dotimes (i 256)
      (setq ch (aref vec i))
      (when ch
	(aset table i ch)
	(if (>= ch 256)
	    (aset rev-table ch i))))
    (set-char-table-extra-slot table 0 rev-table)
    table))

(defun define-translation-table (symbol &rest args)
  "Define SYMBOL as the name of translation table made by ARGS.
This sets up information so that the table can be used for
translations in a CCL program.

If the first element of ARGS is a char-table whose purpose is
`translation-table', just define SYMBOL to name it.  (Note that this
function does not bind SYMBOL.)

Any other ARGS should be suitable as arguments of the function
`make-translation-table' (which see).

This function sets properties `translation-table' and
`translation-table-id' of SYMBOL to the created table itself and the
identification number of the table respectively.  It also registers
the table in `translation-table-vector'."
  (let ((table (if (and (char-table-p (car args))
			(eq (char-table-subtype (car args))
			    'translation-table))
		   (car args)
		 (apply 'make-translation-table args)))
	(len (length translation-table-vector))
	(id 0)
	(done nil))
    (put symbol 'translation-table table)
    (while (not done)
      (if (>= id len)
	  (setq translation-table-vector
		(vconcat translation-table-vector (make-vector len nil))))
      (let ((slot (aref translation-table-vector id)))
	(if (or (not slot)
		(eq (car slot) symbol))
	    (progn
	      (aset translation-table-vector id (cons symbol table))
	      (setq done t))
	  (setq id (1+ id)))))
    (put symbol 'translation-table-id id)
    id))

(put 'with-category-table 'lisp-indent-function 1)

(defmacro with-category-table (table &rest body)
  "Execute BODY like `progn' with CATEGORY-TABLE the current category table.
The category table of the current buffer is saved, BODY is evaluated,
then the saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (let ((old-table (make-symbol "old-table"))
	(old-buffer (make-symbol "old-buffer")))
    `(let ((,old-table (category-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-category-table ,table)
	     ,@body)
	 (save-current-buffer
	   (set-buffer ,old-buffer)
	   (set-category-table ,old-table))))))

(defun define-translation-hash-table (symbol table)
  "Define SYMBOL as the name of the hash translation TABLE for use in CCL.

Analogous to `define-translation-table', but updates
`translation-hash-table-vector' and the table is for use in the CCL
`lookup-integer' and `lookup-character' functions."
  (unless (and (symbolp symbol)
	       (hash-table-p table))
    (error "Bad args to define-translation-hash-table"))
  (let ((len (length translation-hash-table-vector))
	(id 0)
	done)
    (put symbol 'translation-hash-table table)
    (while (not done)
      (if (>= id len)
	  (setq translation-hash-table-vector
		(vconcat translation-hash-table-vector [nil])))
      (let ((slot (aref translation-hash-table-vector id)))
	(if (or (not slot)
		(eq (car slot) symbol))
	    (progn
	      (aset translation-hash-table-vector id (cons symbol table))
	      (setq done t))
	  (setq id (1+ id)))))
    (put symbol 'translation-hash-table-id id)
    id))

;;; Initialize some variables.

(put 'use-default-ascent 'char-table-extra-slots 0)
(setq use-default-ascent (make-char-table 'use-default-ascent))
(put 'ignore-relative-composition 'char-table-extra-slots 0)
(setq ignore-relative-composition
      (make-char-table 'ignore-relative-composition))

(make-obsolete 'set-char-table-default
	       "Generic characters no longer exist" "22.1")

;;; Built-in auto-coding-functions:

(defun sgml-xml-auto-coding-function (size)
  "Determine whether the buffer is XML, and if so, its encoding.
This function is intended to be added to `auto-coding-functions'."
  (setq size (+ (point) size))
  (when (re-search-forward "\\`[[:space:]\n]*<\\?xml" size t)
    (let ((end (save-excursion
		 ;; This is a hack.
		 (re-search-forward "\"\\s-*\\?>" size t))))
      (when end
	(if (re-search-forward "encoding=\"\\(.+?\\)\"" end t)
	    (let* ((match (match-string 1))
		   (sym (intern (downcase match))))
	      (if (coding-system-p sym)
		  sym
		(message "Warning: unknown coding system \"%s\"" match)
		nil))
	  'utf-8)))))

(defun sgml-html-meta-auto-coding-function (size)
  "If the buffer has an HTML meta tag, use it to determine encoding.
This function is intended to be added to `auto-coding-functions'."
  (setq size (min (+ (point) size)
		  ;; Only search forward 10 lines
		  (save-excursion
		    (forward-line 10)
		    (point))))
  (when (and (search-forward "<html>" size t)
	     (re-search-forward "<meta\\s-+http-equiv=\"content-type\"\\s-+content=\"text/\\sw+;\\s-*charset=\\(.+?\\)\"" size t))
      (let* ((match (match-string 1))
	     (sym (intern (downcase match))))
	(if (coding-system-p sym)
	    sym
	  (message "Warning: unknown coding system \"%s\"" match)
	  nil))))

;;;
(provide 'mule)

;;; mule.el ends here
