;;; mule.el --- basic commands for multilingual environment

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.
;; Copyright (C) 2001, 2002
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

(defconst mule-version "7.0 (SAKAKI)" "\
Version number and name of this version of MULE (multilingual environment).")

(defconst mule-version-date "2002.2.28" "\
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
	(setq preloaded-file-list (cons file preloaded-file-list)))
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
		       (eq (coding-system-type last-coding-system-used)
			   'raw-text))
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

;; API (Application Program Interface) for charsets.

;;; Charset property

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
  "Always return nil.  This exists only for backward compatibility."
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

(defun set-buffer-file-coding-system (coding-system &optional force)
  "Set the file coding-system of the current buffer to CODING-SYSTEM.
This means that when you save the buffer, it will be converted
according to CODING-SYSTEM.  For a list of possible values of CODING-SYSTEM,
use \\[list-coding-systems].

If the buffer's previous file coding-system value specifies end-of-line
conversion, and CODING-SYSTEM does not specify one, CODING-SYSTEM is
merged with the already-specified end-of-line conversion.

If the buffer's previous file coding-system value specifies text
conversion, and CODING-SYSTEM does not specify one, CODING-SYSTEM is
merged with the already-specified text conversion.

However, if the optional prefix argument FORCE is non-nil, then
CODING-SYSTEM is used exactly as specified.

This marks the buffer modified so that the succeeding \\[save-buffer]
surely saves the buffer with CODING-SYSTEM.  From a program, if you
don't want to mark the buffer modified, just set the variable
`buffer-file-coding-system' directly."
  (interactive "zCoding system for visited file (default, nil): \nP")
  (check-coding-system coding-system)
  (if (and coding-system buffer-file-coding-system (null force))
      (let ((base (coding-system-base buffer-file-coding-system))
	    (eol (coding-system-eol-type buffer-file-coding-system)))
	;; If CODING-SYSTEM doesn't specify text conversion, merge
	;; with that of buffer-file-coding-system.
	(if (eq (coding-system-base coding-system) 'undecided)
	    (setq coding-system (coding-system-change-text-conversion
				 coding-system base)))
	;; If CODING-SYSTEM doesn't specify eol conversion, merge with
	;; that of buffer-file-coding-system.
	(if (and (vectorp (coding-system-eol-type coding-system))
		 (numberp eol) (>= eol 0) (<= eol 2))
	    (setq coding-system (coding-system-change-eol-conversion
				 coding-system eol)))))
  (setq buffer-file-coding-system coding-system)
  (set-buffer-modified-p t)
  (force-mode-line-update))

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
  :version "21.1"
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
  "Make CODING-SYSTEM used for communicating with other X clients .
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
  '(("ISO8859-10" . iso-8859-10)
    ("ISO8859-13" . iso-8859-13)
    ("ISO8859-14" . iso-8859-14)
    ("ISO8859-15" . iso-8859-15)
    ("ISO8859-16" . iso-8859-16)
    ("KOI8-R" . koi8-r)
    ("BIG5-0" . big5))
  "Alist of non-standard encoding names vs Emacs coding systems.
This alist is used to decode an extened segment of a compound text.")

;; Functions to support "Non-Standard Character Set Encodings" defined
;; by the COMPOUND TEXT spec.

(defvar ctext-non-standard-encodings-regexp
  (string-to-multibyte
   (concat
    ;; For non-standard encodings.
    "\\(\e%/[0-4][\200-\377][\200-\377]\\([^\002]+\\)\002\\)"
    "\\|"
    ;; For UTF-8 encoding.
    "\\(\e%G[^\e]*\e%@\\)")))

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

(defvar ctext-non-standard-designations-alist
  '(("$(0" . (big5 "big5-0" 2))
    ("$(1" . (big5 "big5-0" 2))
    ;; The following are actually standard; generating extended
    ;; segments for them is wrong and screws e.g. Latin-9 users.
    ;; ("-V"  . (t "iso8859-10" 1))
    ;; ("-Y"  . (t "iso8859-13" 1))
    ;; ("-_"  . (t "iso8859-14" 1))
    ;; ("-b"  . (t "iso8859-15" 1))
    ;; ("-f"  . (t "iso8859-16" 1))
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
by run_pre_post_conversion_on_str, generate a new temp buffer, insert the
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

(make-obsolete 'set-coding-priority 'set-coding-system-priority "22.1")

;;; FILE I/O

(defcustom auto-coding-alist
  '(("\\.\\(arc\\|zip\\|lzh\\|zoo\\|jar\\|tar\\|tgz\\)\\'" . no-conversion)
    ("\\.\\(gz\\|Z\\|bz\\|bz2\\|gpg\\)\\'" . no-conversion))
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

(defvar set-auto-coding-for-load nil
  "Non-nil means look for `load-coding' property instead of `coding'.
This is used for loading and byte-compiling Emacs Lisp files.")

(defun auto-coding-alist-lookup (filename)
  "Return the coding system specified by `auto-coding-alist' for FILENAME."
  (let ((alist auto-coding-alist)
	(case-fold-search (memq system-type '(vax-vms windows-nt ms-dos)))
	coding-system)
    (while (and alist (not coding-system))
      (if (string-match (car (car alist)) filename)
	  (setq coding-system (cdr (car alist)))
	(setq alist (cdr alist))))
    coding-system))


(defun auto-coding-from-file-contents (size)
  "Determine a coding system from the contents of the current buffer.
The current buffer contains SIZE bytes starting at point.
Value is either a coding system or nil."
  (save-excursion
    (let ((alist auto-coding-regexp-alist)
	  coding-system)
      (while (and alist (not coding-system))
	(let ((regexp (car (car alist))))
	  (when (re-search-forward regexp (+ (point) size) t)
	    (setq coding-system (cdr (car alist)))))
	(setq alist (cdr alist)))
      coding-system)))
		

(defun set-auto-coding (filename size)
  "Return coding system for a file FILENAME of which SIZE bytes follow point.
These bytes should include at least the first 1k of the file
and the last 3k of the file, but the middle may be omitted.

It checks FILENAME against the variable `auto-coding-alist'.  If
FILENAME doesn't match any entries in the variable, it checks the
contents of the current buffer following point against
`auto-coding-regexp-alist'.  If no match is found, it checks for a
`coding:' tag in the first one or two lines following point.  If no
`coding:' tag is found, it checks for local variables list in the last
3K bytes out of the SIZE bytes.

The return value is the specified coding system,
or nil if nothing specified.

The variable `set-auto-coding-function' (which see) is set to this
function by default."
  (or (auto-coding-alist-lookup filename)
      (auto-coding-from-file-contents size)
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
	coding-system)))

(setq set-auto-coding-function 'set-auto-coding)

(defun after-insert-file-set-buffer-file-coding-system (inserted)
  "Set `buffer-file-coding-system' of current buffer after text is inserted."
  (if last-coding-system-used
      (let ((coding-system
	     (find-new-buffer-file-coding-system last-coding-system-used))
	    (modified-p (buffer-modified-p)))
	(when coding-system
	  (set-buffer-file-coding-system coding-system t)
	  (if (and enable-multibyte-characters
		   (or (eq (coding-system-type coding-system) 'raw-text))
		   ;; If buffer was unmodified and the size is the
		   ;; same as INSERTED, we must be visiting it.
		   (not modified-p)
		   (= (buffer-size) inserted))
	      ;; For coding systems no-conversion and raw-text...,
	      ;; edit the buffer as unibyte.
	      (let ((pos-byte (position-bytes (+ (point) inserted))))
		(set-buffer-multibyte nil)
		(setq inserted (- pos-byte (position-bytes (point))))))
	  (set-buffer-modified-p modified-p))))
  inserted)

(add-hook 'after-insert-file-functions
	  'after-insert-file-set-buffer-file-coding-system)

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

(defmacro with-category-table (category-table &rest body)
  "Execute BODY like `progn' with CATEGORY-TABLE the current category table."
  (let ((current-category-table (make-symbol "current-category-table")))
    `(let ((,current-category-table (category-table)))
       (set-category-table ,category-table)
       (unwind-protect
	   (progn ,@body)
	 (set-category-table ,current-category-table)))))

;;; Initialize some variables.

(put 'use-default-ascent 'char-table-extra-slots 0)
(setq use-default-ascent (make-char-table 'use-default-ascent))
(put 'ignore-relative-composition 'char-table-extra-slots 0)
(setq ignore-relative-composition
      (make-char-table 'ignore-relative-composition))

(make-obsolete 'set-char-table-default
	       "Generic characters no longer exist" "22.1")
;;;
(provide 'mule)

;;; mule.el ends here
