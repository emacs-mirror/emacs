;;; mule.el --- basic commands for mulitilingual environment

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

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

;;; Code:

(defconst mule-version "3.0 (MOMIJINOGA)" "\
Version number and name of this version of MULE (multilingual environment).")

(defconst mule-version-date "1998.1.1" "\
Distribution date of this version of MULE (multilingual environment).")

(defun load-with-code-conversion (fullname file &optional noerror nomessage)
  "Execute a file of Lisp code named FILE whose absolute path is FULLNAME.
The FILE is decoded before evaluation if necessary.
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
	    (let ((default-major-mode 'fundamental-mode))
	      ;; We can't use `generate-new-buffer' because files.el
	      ;; is not yet loaded.
	      (get-buffer-create (generate-new-buffer-name " *load*"))))
	   (load-in-progress t))
      (or nomessage (message "Loading %s..." file))
      (unwind-protect
	  (progn
	    (save-excursion
	      (set-buffer buffer)
	      (insert-file-contents fullname)
	      ;; We must set `buffer-file-name' for `eval-buffer' and
	      ;; `load-history'.
	      (setq buffer-file-name file)
	      ;; Make `kill-buffer' quiet.
	      (set-buffer-modified-p nil))
	    ;; Eval in the original buffer.
	    (eval-buffer buffer))
	(kill-buffer buffer))
      (let ((hook (assoc file after-load-alist)))
	      (if hook
		  (mapcar (function eval) (cdr hook))))
      (or nomessage noninteractive
	  (message "Loading %s...done" file))
      t)))

;; API (Application Program Interface) for charsets.

;; Return t if OBJ is a quoted symbol.
(defsubst quoted-symbol-p (obj)
  (and (listp obj) (eq (car obj) 'quote)))

(defsubst charsetp (object)
  "T is OBJECT is a charset."
  (and (symbolp object) (vectorp (get object 'charset))))

(defsubst charset-info (charset)
  "Return a vector of information of CHARSET.
The elements of the vector are:
	CHARSET-ID, BYTES, DIMENSION, CHARS, WIDTH, DIRECTION,
	LEADING-CODE-BASE, LEADING-CODE-EXT,
	ISO-FINAL-CHAR, ISO-GRAPHIC-PLANE,
	REVERSE-CHARSET, SHORT-NAME, LONG-NAME,	DESCRIPTION,
	PLIST,
where
CHARSET-ID (integer) is the identification number of the charset.
DIMENSION (integer) is the number of bytes to represent a character of
the charset: 1 or 2.
CHARS (integer) is the number of characters in a dimension: 94 or 96.
BYTE (integer) is the length of multi-byte form of a character in
  the charset: one of 1, 2, 3, and 4.
WIDTH (integer) is the number of columns a character in the charset
  occupies on the screen: one of 0, 1, and 2.
DIRECTION (integer) is the rendering direction of characters in the
  charset when rendering.  If 0, render from right to left, else
  render from left to right.
LEADING-CODE-BASE (integer) is the base leading-code for the
  charset.
LEADING-CODE-EXT (integer) is the extended leading-code for the
  charset.  All charsets of less than 0xA0 has the value 0.
ISO-FINAL-CHAR (character) is the final character of the
  corresponding ISO 2022 charset.
ISO-GRAPHIC-PLANE (integer) is the graphic plane to be invoked
  while encoding to variants of ISO 2022 coding system, one of the
  following: 0/graphic-plane-left(GL), 1/graphic-plane-right(GR).
REVERSE-CHARSET (integer) is the charset which differs only in
  LEFT-TO-RIGHT value from the charset.  If there's no such a
  charset, the value is -1.
SHORT-NAME (string) is the short name to refer to the charset.
LONG-NAME (string) is the long name to refer to the charset
DESCRIPTION (string) is the description string of the charset.
PLIST (property list) may contain any type of information a user
  want to put and get by functions `put-charset-property' and
  `get-charset-property' respectively."
  (get charset 'charset))

(defmacro charset-id (charset)
  "Return charset identification number of CHARSET."
  (if (and (listp charset) (eq (car charset) 'quote))
      (aref (charset-info (nth 1 charset)) 0)
    `(aref (charset-info ,charset) 0)))

(defmacro charset-bytes (charset)
  "Return bytes of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 1)
    `(aref (charset-info ,charset) 1)))

(defmacro charset-dimension (charset)
  "Return dimension of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 2)
    `(aref (charset-info ,charset) 2)))

(defmacro charset-chars (charset)
  "Return character numbers contained in a dimension of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 3)
    `(aref (charset-info ,charset) 3)))

(defmacro charset-width (charset)
  "Return width (how many column occupied on a screen) of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 4)
    `(aref (charset-info ,charset) 4)))

(defmacro charset-direction (charset)
  "Return direction of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 5)
    `(aref (charset-info ,charset) 5)))

(defmacro charset-iso-final-char (charset)
  "Return final char of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 8)
    `(aref (charset-info ,charset) 8)))

(defmacro charset-iso-graphic-plane (charset)
  "Return graphic plane of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 9)
    `(aref (charset-info ,charset) 9)))

(defmacro charset-reverse-charset (charset)
  "Return reverse charset of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 10)
    `(aref (charset-info ,charset) 10)))

(defmacro charset-short-name (charset)
  "Return short name of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 11)
    `(aref (charset-info ,charset) 11)))

(defmacro charset-long-name (charset)
  "Return long name of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 12)
    `(aref (charset-info ,charset) 12)))

(defmacro charset-description (charset)
  "Return descriptoin of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 13)
    `(aref (charset-info ,charset) 13)))

(defmacro charset-plist (charset)
  "Return list charset property of CHARSET.
See the function `charset-info' for more detail."
  (if (quoted-symbol-p charset)
      `(aref ,(charset-info (nth 1 charset)) 14)
    `(aref (charset-info ,charset) 14)))

(defun set-charset-plist (charset plist)
  "Set CHARSET's property list to PLIST, and retrun PLIST."
  (aset (charset-info  charset) 14 plist))

(defmacro make-char (charset &optional c1 c2)
  "Return a character of CHARSET and position-codes CODE1 and CODE2.
CODE1 and CODE2 are optional, but if you don't supply
sufficient position-codes, return a generic character which stands for
all characters or group of characters in the character sets.
A generic character can be used to index a char table (e.g. syntax-table)."
  (if (quoted-symbol-p charset)
      `(make-char-internal ,(charset-id (nth 1 charset)) ,c1 ,c2)
    `(make-char-internal (charset-id ,charset) ,c1 ,c2)))

(defmacro charset-list ()
  "Return list of charsets ever defined.

This macro is provided for backward compatibility.
Now we have the variable `charset-list'."
  'charset-list)

(defsubst generic-char-p (char)
  "Return t if and only if CHAR is a generic character.
See also the documentation of make-char."
  (let ((l (split-char char)))
    (and (or (= (nth 1 l) 0) (eq (nth 2 l) 0))
	 (not (eq (car l) 'composition)))))

;; Coding system staffs

;; Coding system is a symbol that has the property `coding-system'.
;;
;; The value of the property `coding-system' is a vector of the
;; following format:
;;	[TYPE MNEMONIC DOC-STRING NOT-USED-NOW FLAGS]
;; We call this vector as coding-spec.  See comments in src/coding.c
;; for more detail.  The property value may be another coding system,
;; in which case, the coding-spec should be taken from that
;; coding-system.  The 4th element NOT-USED-NOW is kept just for
;; backward compatibility with old version of Mule.

(defconst coding-spec-type-idx 0)
(defconst coding-spec-mnemonic-idx 1)
(defconst coding-spec-doc-string-idx 2)
(defconst coding-spec-flags-idx 4)

;; Coding system may have proerpty `eol-type'.  The value of the
;; property `eol-type' is integer 0..2 or a vector of three coding
;; systems.  The integer value 0, 1, and 2 indicate the format of
;; end-of-line LF, CRLF, and CR respectively.  The vector value
;; indicates that the format of end-of-line should be detected
;; automatically.  Nth element of the vector is the subsidiary coding
;; system whose `eol-type' property is N.
;;
;; Coding system may also have properties `post-read-conversion' and
;; `pre-write-conversion.  Values of these properties are functions.
;;
;; The function in `post-read-conversion' is called after some text is
;; inserted and decoded along the coding system and before any
;; functions in `after-insert-functions' are called.  The arguments to
;; this function is the same as those of a function in
;; `after-insert-functions', i.e. LENGTH of a text while putting point
;; at the head of the text to be decoded
;;
;; The function in `pre-write-conversion' is called after all
;; functions in `write-region-annotate-functions' and
;; `buffer-file-format' are called, and before the text is encoded by
;; the coding system.  The arguments to this function is the same as
;; those of a function in `write-region-annotate-functions', i.e. FROM
;; and TO specifying region of a text.

;; Return Nth element of coding-spec of CODING-SYSTEM.
(defun coding-system-spec-ref (coding-system n)
  (check-coding-system coding-system)
  (let ((vec (coding-system-spec coding-system)))
    (and vec (aref vec n))))

(defun coding-system-type (coding-system)
  "Return TYPE element in coding-spec of CODING-SYSTEM."
  (coding-system-spec-ref coding-system coding-spec-type-idx))

(defun coding-system-mnemonic (coding-system)
  "Return MNEMONIC element in coding-spec of CODING-SYSTEM."
  (or (coding-system-spec-ref coding-system coding-spec-mnemonic-idx)
      ?-))

(defun coding-system-doc-string (coding-system)
  "Return DOC-STRING element in coding-spec of CODING-SYSTEM."
  (coding-system-spec-ref coding-system coding-spec-doc-string-idx))

(defun coding-system-flags (coding-system)
  "Return FLAGS element in coding-spec of CODING-SYSTEM."
  (coding-system-spec-ref coding-system coding-spec-flags-idx))

(defun coding-system-eol-type (coding-system)
  "Return eol-type property of CODING-SYSTEM."
  (check-coding-system coding-system)
  (and coding-system
       (or (get coding-system 'eol-type)
	   (coding-system-eol-type (get coding-system 'coding-system)))))

(defun coding-system-category (coding-system)
  "Return coding category of CODING-SYSTEM."
  (and coding-system
       (symbolp coding-system)
       (or (get coding-system 'coding-category)
	   (coding-system-category (get coding-system 'coding-system)))))

(defun coding-system-parent (coding-system)
  "Return parent of CODING-SYSTEM."
  (let ((parent (get coding-system 'parent-coding-system)))
    (and parent
	 (or (coding-system-parent parent)
	     parent))))

;; Make subsidiary coding systems (eol-type variants) of CODING-SYSTEM.
(defun make-subsidiary-coding-system (coding-system)
  (let ((subsidiaries (vector (intern (format "%s-unix" coding-system))
			      (intern (format "%s-dos" coding-system))
			      (intern (format "%s-mac" coding-system))))
	(i 0))
    (while (< i 3)
      (put (aref subsidiaries i) 'coding-system coding-system)
      (put (aref subsidiaries i) 'eol-type i)
      (put (aref subsidiaries i) 'eol-variant t)
      (setq i (1+ i)))
    subsidiaries))

(defun make-coding-system (coding-system type mnemonic doc-string
					 &optional flags)
  "Define a new CODING-SYSTEM (symbol).
Remaining arguments are TYPE, MNEMONIC, DOC-STRING, and FLAGS (optional) which
construct a coding-spec of CODING-SYSTEM in the following format:
	[TYPE MNEMONIC DOC-STRING nil FLAGS]
TYPE is an integer value indicating the type of coding-system as follows:
  0: Emacs internal format,
  1: Shift-JIS (or MS-Kanji) used mainly on Japanese PC,
  2: ISO-2022 including many variants,
  3: Big5 used mainly on Chinese PC,
  4: private, CCL programs provide encoding/decoding algorithm.
MNEMONIC is a character to be displayed on mode line for the coding-system.
DOC-STRING is a documentation string for the coding-system.
FLAGS specifies more precise information of each TYPE.
  If TYPE is 2 (ISO-2022), FLAGS should be a list of:
      CHARSET0, CHARSET1, CHARSET2, CHARSET3, SHORT-FORM,
      ASCII-EOL, ASCII-CNTL, SEVEN, LOCKING-SHIFT, SINGLE-SHIFT,
      USE-ROMAN, USE-OLDJIS, NO-ISO6429, INIT-BOL, DESIGNATION-BOL.
    CHARSETn are character sets initially designated to Gn graphic registers.
      If CHARSETn is nil, Gn is never used.
      If CHARSETn is t, Gn can be used but nothing designated initially.
      If CHARSETn is a list of character sets, those character sets are
        designated to Gn on output, but nothing designated to Gn initially.
    SHORT-FORM non-nil means use short designation sequence on output.
    ASCII-EOL non-nil means designate ASCII to g0 at end of line on output.
    ASCII-CNTL non-nil means designate ASCII to g0 before control codes and
      SPACE on output.
    SEVEN non-nil means use 7-bit code only on output.
    LOCKING-SHIFT non-nil means use locking-shift.
    SINGLE-SHIFT non-nil means use single-shift.
    USE-ROMAN non-nil means designate JIS0201-1976-Roman instead of ASCII.
    USE-OLDJIS non-nil means designate JIS0208-1976 instead of JIS0208-1983.
    NO-ISO6429 non-nil means not use ISO6429's direction specification.
    INIT-BOL non-nil means any designation state is assumed to be reset
      to initial at each beginning of line on output.
    DESIGNATION-BOL non-nil means designation sequences should be placed
      at beginning of line on output.
  If TYPE is 4 (private), FLAGS should be a cons of CCL programs,
    for encoding and decoding.  See the documentation of CCL for more detail."

  ;; At first, set a value of `coding-system' property.
  (let ((coding-spec (make-vector 5 nil))
	coding-category)
    (if (or (not (integerp type)) (< type 0) (> type 4))
	(error "TYPE argument must be 0..4"))
    (if (or (not (integerp mnemonic)) (<= mnemonic ? ) (> mnemonic 127))
	(error "MNEMONIC arguemnt must be a printable character."))
    (aset coding-spec 0 type)
    (aset coding-spec 1 mnemonic)
    (aset coding-spec 2 (if (stringp doc-string) doc-string ""))
    (aset coding-spec 3 nil)		; obsolete element
    (cond ((= type 0)
	   (setq coding-category 'coding-category-emacs-mule))
	  ((= type 1)
	   (setq coding-category 'coding-category-sjis))
	  ((= type 2)			; ISO2022
	   (let ((i 0)
		 (vec (make-vector 32 nil))
		 (no-initial-designation t)
		 (g1-designation nil))
	     (while (< i 4)
	       (let ((charset (car flags)))
		 (if (and no-initial-designation
			  (> i 0)
			  (or (charsetp charset)
			      (and (consp charset)
				   (charsetp (car charset)))))
		     (setq no-initial-designation nil))
		 (if (charsetp charset)
		     (if (= i 1) (setq g1-designation charset))
		   (if (consp charset)
		       (let ((tail charset)
			     elt)
			 (while tail
			   (setq elt (car tail))
			   (or (not elt) (eq elt t) (charsetp elt)
			       (error "Invalid charset: %s" elt))
			   (setq tail (cdr tail)))
			 (setq g1-designation (car charset)))
		     (if (and charset (not (eq charset t)))
			 (error "Invalid charset: %s" charset))))
		 (aset vec i charset))
	       (setq flags (cdr flags) i (1+ i)))
	     (while (and (< i 32) flags)
	       (aset vec i (car flags))
	       (setq flags (cdr flags) i (1+ i)))
	     (aset coding-spec 4 vec)
	     (if no-initial-designation
		 (put coding-system 'no-initial-designation t))
	     (setq coding-category
		   (if (aref vec 8)	; Use locking-shift.
		       'coding-category-iso-else
		     (if (aref vec 7)	; 7-bit only.
			 (if (aref vec 9) ; Use single-shift.
			     'coding-category-iso-else
			   'coding-category-iso-7)
		       (if no-initial-designation
			   'coding-category-iso-else
			 (if (and (charsetp g1-designation)
				  (= (charset-dimension g1-designation) 2))
			     'coding-category-iso-8-2
			   'coding-category-iso-8-1)))))))
	  ((= type 3)
	   (setq coding-category 'coding-category-big5))
	  ((= type 4)			; private
	   (setq coding-category 'coding-category-binary)
	   (if (and (consp flags)
		    (vectorp (car flags))
		    (vectorp (cdr flags)))
	       (aset coding-spec 4 flags)
	     (error "Invalid FLAGS argument for TYPE 4 (CCL)"))))
    (put coding-system 'coding-system coding-spec)
    (put coding-system 'coding-category coding-category)
    (put coding-category 'coding-systems
	 (cons coding-system (get coding-category 'coding-systems))))

  ;; Next, set a value of `eol-type' property.  The value is a vector
  ;; of subsidiary coding systems, each corresponds to a coding system
  ;; for the detected end-of-line format.
  (put coding-system 'eol-type
       (if (<= type 3)
	   (make-subsidiary-coding-system coding-system)
	 0)))

(defun define-coding-system-alias (alias coding-system)
  "Define ALIAS as an alias for coding system
 CODING-SYSTEM."
  (check-coding-system coding-system)
  (let ((parent (coding-system-parent coding-system)))
    (if parent
	(setq coding-system parent)))
  (put alias 'coding-system coding-system)
  (put alias 'parent-coding-system coding-system)
  (put coding-system 'alias-coding-systems
       (cons alias (get coding-system 'alias-coding-systems)))
  (let ((eol-variants (coding-system-eol-type coding-system))
	subsidiaries)
    (if (vectorp eol-variants)
	(let ((i 0))
	  (setq subsidiaries (make-subsidiary-coding-system alias))
	  (while (< i 3)
	    (put (aref subsidiaries i) 'parent-coding-system
		 (aref eol-variants i))
	    (put (aref eol-variants i) 'alias-coding-systems
		 (cons (aref subsidiaries i) (get (aref eol-variants i)
						  'alias-coding-systems)))
	    (setq i (1+ i)))))))

(defun set-buffer-file-coding-system (coding-system &optional force)
  "Set buffer-file-coding-system of the current buffer to CODING-SYSTEM.
If eol-type of the current buffer-file-coding-system is an integer value N, and
 eol-type of CODING-SYSTEM is a vector, the Nth element of the vector is used
 instead of CODING-SYSTEM itself.
Optional prefix argument FORCE non-nil means CODING-SYSTEM is set
 regardless of eol-type of the current buffer-file-coding-system."
  (interactive "zBuffer-file-coding-system: \nP")
  (check-coding-system coding-system)
  (if (null force)
      (let ((x (coding-system-eol-type buffer-file-coding-system))
	    (y (coding-system-eol-type coding-system)))
	(if (and (numberp x) (>= x 0) (<= x 2) (vectorp y))
	    (setq coding-system (aref y x)))))
  (setq buffer-file-coding-system coding-system)
  (set-buffer-modified-p t)
  (force-mode-line-update))

(defun set-terminal-coding-system (coding-system)
  "Set coding system of your terminal to CODING-SYSTEM.
All outputs to terminal are encoded by the specified coding system."
  (interactive "zCoding-system for terminal display: ")
  (set-terminal-coding-system-internal coding-system)
  (redraw-frame (selected-frame)))

(defun set-keyboard-coding-system (coding-system)
  "Set coding system of codes sent from terminal keyboard to CODING-SYSTEM.
In addition, this command toggles Encoded-kbd minor mode.
If the specified coding system is nil, Encoded-bkd mode is turned off,
else it is turned on so that user inputs are decoded by the
specified coding system."
  (interactive "zCoding-system for keyboard input: ")
  (set-keyboard-coding-system-internal coding-system)
  (encoded-kbd-mode (if coding-system 1 0)))

(defun set-buffer-process-coding-system (decoding encoding)
  "Set coding systems to the process associated with the current buffer.
DECODING is the coding system to be used to decode input from the process,
ENCODING is to be used to encode output to the process."
  (interactive
   "zCoding-system for process input: \nzCoding-system for process output: ")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (null proc)
	(error "no process")
      (check-coding-system decoding)
      (check-coding-system encoding)
      (set-process-coding-system proc decoding encoding)))
  (force-mode-line-update))

(defun set-coding-priority (arg)
  "Set priority of coding-category according to LIST.
LIST is a list of coding-categories ordered by priority."
  (let (l)
    ;; Put coding-categories listed in ARG to L while checking the
    ;; validity.  We assume that `coding-category-list' contains whole
    ;; coding-categories.
    (while arg
      (if (null (memq (car arg) coding-category-list))
	  (error "Invalid element in argument: %s" (car arg)))
      (setq l (cons (car arg) l))
      (setq arg (cdr arg)))
    ;; Put coding-category not listed in ARG to L.
    (while coding-category-list
      (if (null (memq (car coding-category-list) l))
	  (setq l (cons (car coding-category-list) l)))
      (setq coding-category-list (cdr coding-category-list)))
    ;; Update `coding-category-list' and return it.
    (setq coding-category-list (nreverse l))))

;;; FILE I/O

;; Set buffer-file-coding-system of the current buffer after some text
;; is inserted.
(defun after-insert-file-set-buffer-file-coding-system (inserted)
  (if last-coding-system-used
      (let ((coding-system
	     (find-new-buffer-file-coding-system last-coding-system-used))
	    (modified-p (buffer-modified-p)))
	(if coding-system
	    (set-buffer-file-coding-system coding-system))
	(set-buffer-modified-p modified-p)))
  nil)

(setq after-insert-file-functions
      (cons 'after-insert-file-set-buffer-file-coding-system
	    after-insert-file-functions))

;; The coding-spec and eol-type of coding-system returned is decided
;; independently in the following order.
;;	1. That of buffer-file-coding-system locally bound.
;;	2. That of CODING.

(defun find-new-buffer-file-coding-system (coding)
  "Return a coding system for a buffer when a file of CODING is inserted.
The local variable `buffer-file-coding-system' of the current buffer
is set to the returned value.
Return nil if there's no need of setting new buffer-file-coding-system."
  (let (local-coding local-eol
	found-eol
	new-coding new-eol)
    (if (null coding)
	;; Nothing found about coding.
	nil

      ;; Get information of the current local value of
      ;; `buffer-file-coding-system' in LOCAL-EOL and LOCAL-CODING.
      (if (local-variable-p 'buffer-file-coding-system)
	  ;; Something already set locally.
	  (progn
	    (setq local-eol (coding-system-eol-type buffer-file-coding-system))
	    (if (null (numberp local-eol))
		;; But eol-type is not yet set.
		(setq local-eol nil))
	    (if (null (eq (coding-system-type buffer-file-coding-system) t))
		;; This is not `undecided'.
		(progn
		  (setq local-coding buffer-file-coding-system)
		  (while (symbolp (get local-coding 'coding-system))
		    (setq local-coding (get local-coding 'coding-system))))
	      )))

      (if (and local-eol local-coding)
	  ;; The current buffer has already set full coding-system, we
	  ;; had better not change it.
	  nil

	(setq found-eol (coding-system-eol-type coding))
	(if (null (numberp found-eol))
	    ;; But eol-type is not found.
	    (setq found-eol nil))

	;; The local setting takes precedence over the found one.
	(setq new-coding (or local-coding coding))
	(setq new-eol (or local-eol found-eol))
	(if (and (numberp new-eol)
		 (vectorp (coding-system-eol-type new-coding)))
	    (setq new-coding
		  (aref (coding-system-eol-type new-coding) new-eol)))
	new-coding))))

(defun make-unification-table (&rest args)
  "Make a unification table (char table) from arguments.
Each argument is a list of the form (FROM . TO),
where FROM is a character to be unified to TO.

FROM can be a generic character (see make-char).  In this case, TO is
a generic character containing the same number of charcters or a
oridinal character.  If FROM and TO are both generic characters, all
characters belonging to FROM are unified to characters belonging to TO
without changing their position code(s)."
  (let ((table (make-char-table 'character-unification-table))
	revlist)
    (while args
      (let ((elts (car args)))
	(while elts
	  (let* ((from (car (car elts)))
		 (from-i 0)		; degree of freedom of FROM
		 (from-rev (nreverse (split-char from)))
		 (to (cdr (car elts)))
		 (to-i 0)		; degree of freedom of TO
		 (to-rev (nreverse (split-char to))))
	    ;; Check numbers of heading 0s in FROM-REV and TO-REV.
	    (while (eq (car from-rev) 0)
	      (setq from-i (1+ from-i) from-rev (cdr from-rev)))
	    (while (eq (car to-rev) 0)
	      (setq to-i (1+ to-i) to-rev (cdr to-rev)))
	    (if (and (/= from-i to-i) (/= to-i 0))
		(error "Invalid character pair (%d . %d)" from to))
	    ;; If we have already unified TO to TO-ALT, FROM should
	    ;; also be unified to TO-ALT.  But, this is only if TO is
	    ;; a generic character or TO-ALT is not a generic
	    ;; character.
	    (let ((to-alt (aref table to)))
	      (if (and to-alt
		       (or (> to-i 0) (not (generic-char-p to-alt))))
		  (setq to to-alt)))
	    (if (> from-i 0)
		(set-char-table-default table from to)
	      (aset table from to))
	    ;; If we have already unified some chars to FROM, they
	    ;; should also be unified to TO.
	    (let ((l (assq from revlist)))
	      (if l
		  (let ((ch (car l)))
		    (setcar l to)
		    (setq l (cdr l))
		    (while l
		      (aset table ch to)
		      (setq l (cdr l)) ))))
	    ;; Now update REVLIST.
	    (let ((l (assq to revlist)))
	      (if l
		  (setcdr l (cons from (cdr l)))
		(setq revlist (cons (list to from) revlist)))))
	  (setq elts (cdr elts))))
      (setq args (cdr args)))
    ;; Return TABLE just created.
    table))

;;; Initialize some variables.

(put 'use-default-ascent 'char-table-extra-slots 0)
(setq use-default-ascent (make-char-table 'use-default-ascent))

;;;
(provide 'mule)

;;; mule.el ends here
