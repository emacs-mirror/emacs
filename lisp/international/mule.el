;;; mule.el --- basic commands for mulitilingual environment

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 1)
    `(aref (charset-info ,charset) 1)))

(defmacro charset-dimension (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 2)
    `(aref (charset-info ,charset) 2)))

(defmacro charset-chars (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 3)
    `(aref (charset-info ,charset) 3)))

(defmacro charset-width (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 4)
    `(aref (charset-info ,charset) 4)))

(defmacro charset-direction (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 5)
    `(aref (charset-info ,charset) 5)))

(defmacro charset-iso-final-char (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 8)
    `(aref (charset-info ,charset) 8)))

(defmacro charset-iso-graphic-plane (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 9)
    `(aref (charset-info ,charset) 9)))

(defmacro charset-reverse-charset (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 10)
    `(aref (charset-info ,charset) 10)))

(defmacro charset-short-name (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 11)
    `(aref (charset-info ,charset) 11)))

(defmacro charset-long-name (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 12)
    `(aref (charset-info ,charset) 12)))

(defmacro charset-description (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 13)
    `(aref (charset-info ,charset) 13)))

(defmacro charset-plist (charset)
  (if (quoted-symbol-p charset)
      (aref (charset-info (nth 1 charset)) 14)
    `(aref (charset-info ,charset) 14)))

(defun set-charset-plist (charset plist)
  (aset (charset-info  charset) 14 plist))

(defmacro make-char (charset &optional c1 c2)
  (if (quoted-symbol-p charset)
      `(make-char-internal ,(charset-id (nth 1 charset)) ,c1 ,c2)
    `(make-char-internal (charset-id ,charset) ,c1 ,c2)))

;; Coding-system staffs

;; Coding-system object is a symbol that has the property
;; `coding-system' and `eol-type'.
;;
;; The value of the property `coding-system' is a coding-vector of the
;; format: [TYPE MNEMONIC DOCSTRING NOT-USED-NOW FLAGS].
;; See comments in src/coding.c for more detail.  The property value
;; may be another coding-system, in which case, the coding-vector
;; should be taken from that coding-system.
;;
;; The value of the property `eol-type' is integer 0..2 or a vector of
;; length 3.  The integer value 0, 1, and 2 indicate the format of
;; end-of-line LF, CRLF, and CR respectively.  The vector value
;; indicates that the format of end-of-line should be detected
;; automatically.  Nth element of the vector is the subsidiary
;; coding-system whose `eol-type' property is integer value.
;;
;; Coding-system may also have properties `post-read-conversion' and
;; `pre-write-conversion and the values are functions.
;;
;; The function in `post-read-conversion' is called after some text is
;; inserted and decoded along the coding-system and before any
;; functions in `after-insert-functions' are called.  The arguments to
;; this function is the same as those of a function in
;; `after-insert-functions', i.e. LENGTH of a text while putting point
;; at the head of the text to be decoded
;;
;; The function in `pre-write-conversion' is called after all
;; functions in `write-region-annotate-functions' and
;; `buffer-file-format' are called, and before the text is encoded by
;; the coding-system.  The arguments to this function is the same as
;; those of a function in `write-region-annotate-functions', i.e. FROM
;; and TO specifying region of a text.

(defsubst coding-vector-type (vec) (aref vec 0))
(defsubst coding-vector-mnemonic (vec) (aref vec 1))
(defsubst coding-vector-docstring (vec) (aref vec 2))
(defsubst coding-vector-flags (vec) (aref vec 4))

;; Return type of CODING-SYSTEM.
(defun coding-system-type (coding-system)
  (let ((vec (coding-system-vector coding-system)))
    (if vec (coding-vector-type vec))))

;; Return mnemonic character of CODING-SYSTEM.
(defun coding-system-mnemonic (coding-system)
  (let ((vec (coding-system-vector coding-system)))
    (if vec (coding-vector-mnemonic vec)
      ?-)))

;; Return docstring of CODING-SYSTEM.
(defun coding-system-docstring (coding-system)
  (let ((vec (coding-system-vector coding-system)))
    (if vec (coding-vector-docstring vec))))

;; Return flags of CODING-SYSTEM.
(defun coding-system-flags (coding-system)
  (let ((vec (coding-system-vector coding-system)))
    (if vec (coding-vector-flags vec))))

;; Return eol-type of CODING-SYSTEM.
(defun coding-system-eoltype (coding-system)
  (and coding-system
       (or (get coding-system 'eol-type)
	   (coding-system-eoltype (get coding-system 'coding-system)))))

;; Return mnemonic character of eol-type of CODING-SYSTEM.
(defun coding-system-eoltype-mnemonic (coding-system)
  (let ((eol-type (coding-system-eoltype coding-system)))
    (cond ((vectorp eol-type) eol-mnemonic-undecided)
	  ((eq eol-type 0) eol-mnemonic-unix)
	  ((eq eol-type 1) eol-mnemonic-unix)
	  ((eq eol-type 2) eol-mnemonic-unix)
	  (t ?-))))

;; Return function for post-read-conversion of CODING-SYSTEM.
(defun coding-system-post-read-conversion (coding-system)
  (and coding-system
       (symbolp coding-system)
       (or (get coding-system 'post-read-conversion)
	   (coding-system-post-read-conversion
	    (get coding-system 'coding-system)))))

;; Return function for pre-write-conversion of CODING-SYSTEM.
(defun coding-system-pre-write-conversion (coding-system)
  (and coding-system
       (symbolp coding-system)
       (or (get coding-system 'pre-write-conversion)
	   (coding-system-pre-write-conversion
	    (get coding-system 'coding-system)))))

(defun make-coding-system (coding-system type mnemonic docstring
					    &optional flags)
  "Define a new CODING-SYSTEM (symbol).
Remaining arguments are TYPE, MNEMONIC, DOCSTRING, and FLAGS (optional).
TYPE is an integer value indicating the type of coding-system as follows:
  0: Emacs internal format,
  1: Shift-JIS (or MS-Kanji) used mainly on Japanese PC,
  2: ISO-2022 including many variants,
  3: Big5 used mainly on Chinese PC,
  4: private, CCL programs provide encoding/decoding algorithm.
MNEMONIC is a character to be displayed on mode line for the coding-system.
DOCSTRING is a documentation string for the coding-system.
FLAGS specifies more precise information of each TYPE.
  If TYPE is 2 (ISO-2022), FLAGS should be a list of:
      CHARSET0, CHARSET1, CHARSET2, CHARSET3, SHORT-FORM,
      ASCII-EOL, ASCII-CNTL, SEVEN, LOCKING-SHIFT, SINGLE-SHIFT,
      USE-ROMAN, USE-OLDJIS, NO-ISO6429.
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
  If TYPE is 4 (private), FLAGS should be a cons of CCL programs,
    for encoding and decoding.  See the documentation of CCL for more detail."

  ;; At first, set a value of `coding-system' property.
  (let ((coding-vector (make-vector 5 nil)))
    (aset coding-vector 0 type)
    (aset coding-vector 1
	  ;; MNEMONIC must be a printable character.
	  (if (and (> mnemonic ? ) (< mnemonic 127)) mnemonic ? ))
    (aset coding-vector 2 (if (stringp docstring) docstring ""))
    (aset coding-vector 3 nil)		; obsolete element
    (cond ((eq type 2)			; ISO2022
	   (let ((i 0)
		 (vec (make-vector 32 nil)))
	     (while (< i 4)
	       (let ((charset (car flags)))
		 (if (and charset (not (eq charset t)))
		     (if (symbolp charset)
			 (setq charset (charset-id charset))
		       (let (elt l)
			 (while charset
			   (setq elt (car charset))
			   (if (and elt (not (eq elt t)))
			       (setq elt (charset-id elt)))
			   (setq l (cons elt l))
			   (setq charset (cdr charset)))
			 (setq charset (nreverse l)))))
		 (aset vec i charset))
	       (setq flags (cdr flags) i (1+ i)))
	     (while (and (< i 32) flags)
	       (aset vec i (car flags))
	       (setq flags (cdr flags) i (1+ i)))
	     (aset coding-vector 4 vec)))
	  ((eq type 4)			; private
	   (if (and (consp flags)
		    (vectorp (car flags))
		    (vectorp (cdr flags)))
	       (aset coding-vector 4 flags)
	     (error "Invalid FLAGS argument for TYPE 4 (CCL)")))
	  (t (aset coding-vector 4 flags)))
    (put coding-system 'coding-system coding-vector))

  ;; Next, set a value of `eol-type' property.  The value is a vector
  ;; of subsidiary coding-systems, each corresponds to a coding-system
  ;; for the detected end-of-line format.
  (let ((codings (vector (intern (format "%s-unix" coding-system))
			 (intern (format "%s-dos" coding-system))
			 (intern (format "%s-mac" coding-system))))
	(i 0))
    (while (< i 3)
      (put (aref codings i) 'coding-system coding-system)
      (put (aref codings i) 'eol-type i)
      (setq i (1+ i)))
    (put coding-system 'eol-type codings))
  )

(defun define-coding-system-alias (symbol new-symbol)
  "Define NEW-SYMBOL as the same coding system as SYMBOL."
  (check-coding-system symbol)
  (put new-symbol 'coding-system (get symbol 'coding-system))
  (let ((eol-type (get symbol 'eol-type)))
    (if (vectorp eol-type)
	(let* ((name (symbol-name new-symbol))
	       (new (vector (intern (concat name "-unix"))
			    (intern (concat name "-dos"))
			    (intern (concat name "-mac"))))
	       (i 0))
	  (while (< i 3)
	    (define-coding-system-alias (aref eol-type i) (aref new i))
	    (setq i (1+ i)))
	  (setq eol-type new)))
    (put new-symbol 'eol-type eol-type)))

(defvar buffer-file-coding-system nil
  "Coding-system of the file which the current-buffer is visiting.")
(make-variable-buffer-local 'buffer-file-coding-system)
;; This value should not be reset by changing major mode.
(put 'buffer-file-coding-system 'permanent-local t)

(defun set-buffer-file-coding-system (coding-system &optional force)
  "Set buffer-file-coding-system of the current buffer to CODING-SYSTEM.
If eol-type of the current buffer-file-coding-system is an integer value N, and
 eol-type of CODING-SYSTEM is a vector, the Nth element of the vector is set 
 instead of CODING-SYSTEM itself.
Optional prefix argument FORCE non-nil means CODING-SYSTEM is set
 regardless of eol-type of the current buffer-file-coding-system."
  (interactive "zBuffer-file-coding-system: \nP")
  (check-coding-system coding-system)
  (if (null force)
      (let ((x (coding-system-eoltype buffer-file-coding-system))
	    (y (coding-system-eoltype coding-system)))
	(if (and (numberp x) (>= x 0) (<= x 2) (vectorp y))
	    (setq coding-system (aref y x)))))
  (setq buffer-file-coding-system coding-system)
  (set-buffer-modified-p t)
  (force-mode-line-update))

(defun set-current-process-coding-system (input output)
  (interactive
   "zCoding-system for process input: \nzCoding-system for process output: ")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (null proc)
	(error "no process")
      (check-coding-system input)
      (check-coding-system output)
      (set-process-coding-system proc input output)))
  (force-mode-line-update))

(defvar default-process-coding-system (cons nil nil)
  "Cons of default values used to read from and write to process.")

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

;; The coding-vector and eol-type of coding-system returned is decided
;; independently in the following order.
;;	1. That of buffer-file-coding-system locally bound.
;;	2. That of CODING.

(defun find-new-buffer-file-coding-system (coding)
  "Return a coding system for a buffer when a file of CODING is inserted.
The returned value is set to `buffer-file-coding-system' of the
current buffer.  Return nil if there's no need of setting new
buffer-file-coding-system."
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
	    (setq local-eol (coding-system-eoltype buffer-file-coding-system))
	    (if (null (numberp local-eol))
		;; But eol-type is not yet set.
		(setq local-eol nil))
	    (if (null (eq (coding-system-type buffer-file-coding-system) t))
		;; This is not automatic-conversion.
		(progn
		  (setq local-coding buffer-file-coding-system)
		  (while (symbolp (get local-coding 'coding-system))
		    (setq local-coding (get local-coding 'coding-system))))
	      )))

      (if (and local-eol local-coding)
	  ;; The current buffer has already set full coding-system, we
	  ;; had better not change it.
	  nil

	(setq found-eol (coding-system-eoltype coding))
	(if (null (numberp found-eol))
	    ;; But eol-type is not found.
	    (setq found-eol nil))
	(if (eq (coding-system-type coding) t)
	    ;; This is automatic-conversion, which means nothing found
	    ;; except for eol-type.
	    (setq coding nil))

	;; The local setting takes precedence over the found one.
	(setq new-coding (or local-coding coding))
	(setq new-eol (or local-eol found-eol))
	(if (and (numberp new-eol)
		 (vectorp (coding-system-eoltype new-coding)))
	    (setq new-coding
		  (aref (coding-system-eoltype new-coding) new-eol)))
	new-coding))))

(provide 'mule)

;;; mule.el ends here
