;;; select.el --- lisp portion of standard selection support  -*- lexical-binding:t -*-

;; Copyright (C) 1993-1994, 2001-2018 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal

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

;; Based partially on earlier release by Lucid.

;; The functionality here is divided in two parts:
;; - Low-level: gui-get-selection, gui-set-selection, gui-selection-owner-p,
;;   gui-selection-exists-p are the backend-dependent functions meant to access
;;   various kinds of selections (CLIPBOARD, PRIMARY, SECONDARY).
;; - Higher-level: gui-select-text and gui-selection-value go together to
;;   access the general notion of "GUI selection" for interoperation with other
;;   applications.  This can use either the clipboard or the primary selection,
;;   or both or none according to select-enable-clipboard/primary.  These are
;;   the default values of interprogram-cut/paste-function.
;;   Additionally, there's gui-get-primary-selection which is used to get the
;;   PRIMARY selection, specifically for mouse-yank-primary.

;;; Code:

(defcustom selection-coding-system nil
  "Coding system for communicating with other programs.

For MS-Windows and MS-DOS:
When sending or receiving text via selection and clipboard, the text
is encoded or decoded by this coding system.  The default value is
the current system default encoding on 9x/Me, `utf-16le-dos'
\(Unicode) on NT/W2K/XP, and `iso-latin-1-dos' on MS-DOS.

For X Windows:
When sending text via selection and clipboard, if the target
data-type matches with the type of this coding system, it is used
for encoding the text.  Otherwise (including the case that this
variable is nil), a proper coding system is used as below:

data-type	coding system
---------	-------------
UTF8_STRING	utf-8
COMPOUND_TEXT	compound-text-with-extensions
STRING		iso-latin-1
C_STRING	no-conversion

When receiving text, if this coding system is non-nil, it is used
for decoding regardless of the data-type.  If this is nil, a
proper coding system is used according to the data-type as above.

See also the documentation of the variable `x-select-request-type' how
to control which data-type to request for receiving text.

The default value is nil."
  :type 'coding-system
  :group 'mule
  ;; Default was compound-text-with-extensions in 22.x (pre-unicode).
  :version "23.1"
  :set (lambda (symbol value)
         (set-selection-coding-system value)
         (set symbol value)))

(defvar next-selection-coding-system nil
  "Coding system for the next communication with other programs.
Usually, `selection-coding-system' is used for communicating with
other programs (X Windows clients or MS Windows programs).  But, if this
variable is set, it is used for the next communication only.
After the communication, this variable is set to nil.")

;; Only declared obsolete in 23.3.
(define-obsolete-function-alias 'x-selection 'x-get-selection "at least 19.34")

(defcustom select-enable-clipboard t
  "Non-nil means cutting and pasting uses the clipboard.
This can be in addition to, but in preference to, the primary selection,
if applicable (i.e. under X11)."
  :type 'boolean
  :group 'killing
  ;; The GNU/Linux version changed in 24.1, the MS-Windows version did not.
  :version "24.1")
(define-obsolete-variable-alias 'x-select-enable-clipboard
  'select-enable-clipboard "25.1")

(defcustom select-enable-primary nil
  "Non-nil means cutting and pasting uses the primary selection.
The existence of a primary selection depends on the underlying GUI you use.
E.g. it doesn't exist under MS-Windows."
  :type 'boolean
  :group 'killing
  :version "25.1")
(define-obsolete-variable-alias 'x-select-enable-primary
  'select-enable-primary "25.1")

;; We keep track of the last text selected here, so we can check the
;; current selection against it, and avoid passing back our own text
;; from gui-selection-value.  We track both
;; separately in case another X application only sets one of them
;; we aren't fooled by the PRIMARY or CLIPBOARD selection staying the same.

(defvar gui--last-selected-text-clipboard nil
  "The value of the CLIPBOARD selection last seen.")
(defvar gui--last-selected-text-primary nil
  "The value of the PRIMARY selection last seen.")

(defun gui-select-text (text)
  "Select TEXT, a string, according to the window system.
if `select-enable-clipboard' is non-nil, copy TEXT to the system's clipboard.
If `select-enable-primary' is non-nil, put TEXT in the primary selection.

MS-Windows does not have a \"primary\" selection."
  (when select-enable-primary
    (gui-set-selection 'PRIMARY text)
    (setq gui--last-selected-text-primary text))
  (when select-enable-clipboard
    ;; When cutting, the selection is cleared and PRIMARY
    ;; set to the empty string.  Prevent that, PRIMARY
    ;; should not be reset by cut (Bug#16382).
    (setq saved-region-selection text)
    (gui-set-selection 'CLIPBOARD text)
    (setq gui--last-selected-text-clipboard text)))
(define-obsolete-function-alias 'x-select-text 'gui-select-text "25.1")

(defcustom x-select-request-type nil
  "Data type request for X selection.
The value is one of the following data types, a list of them, or nil:
  `COMPOUND_TEXT', `UTF8_STRING', `STRING', `TEXT'

If the value is one of the above symbols, try only the specified type.

If the value is a list of them, try each of them in the specified
order until succeed.

The value nil is the same as the list (UTF8_STRING COMPOUND_TEXT STRING)."
  :type '(choice (const :tag "Default" nil)
		 (const COMPOUND_TEXT)
		 (const UTF8_STRING)
		 (const STRING)
		 (const TEXT)
		 (set :tag "List of values"
		      (const COMPOUND_TEXT)
		      (const UTF8_STRING)
		      (const STRING)
		      (const TEXT)))
  :group 'killing)

;; Get a selection value of type TYPE by calling gui-get-selection with
;; an appropriate DATA-TYPE argument decided by `x-select-request-type'.
;; The return value is already decoded.  If gui-get-selection causes an
;; error, this function return nil.

(defun gui--selection-value-internal (type)
  (let ((request-type (if (eq window-system 'x)
                          (or x-select-request-type
                              '(UTF8_STRING COMPOUND_TEXT STRING))
                        'STRING))
	text)
    (with-demoted-errors "gui-get-selection: %S"
      (if (consp request-type)
          (while (and request-type (not text))
            (setq text (gui-get-selection type (car request-type)))
            (setq request-type (cdr request-type)))
        (setq text (gui-get-selection type request-type))))
    (if text
	(remove-text-properties 0 (length text) '(foreign-selection nil) text))
    text))

(defun gui-selection-value ()
  (let ((clip-text
         (when select-enable-clipboard
           (let ((text (gui--selection-value-internal 'CLIPBOARD)))
             (if (string= text "") (setq text nil))

             ;; Check the CLIPBOARD selection for 'newness', is it different
             ;; from what we remembered them to be last time we did a
             ;; cut/paste operation.
             (prog1
                 (unless (equal text gui--last-selected-text-clipboard)
                   text)
               (setq gui--last-selected-text-clipboard text)))))
        (primary-text
         (when select-enable-primary
           (let ((text (gui--selection-value-internal 'PRIMARY)))
             (if (string= text "") (setq text nil))
             ;; Check the PRIMARY selection for 'newness', is it different
             ;; from what we remembered them to be last time we did a
             ;; cut/paste operation.
             (prog1
                 (unless (equal text gui--last-selected-text-primary)
                   text)
               (setq gui--last-selected-text-primary text))))))

    ;; As we have done one selection, clear this now.
    (setq next-selection-coding-system nil)

    ;; At this point we have recorded the current values for the
    ;; selection from clipboard (if we are supposed to) and primary.
    ;; So return the first one that has changed
    ;; (which is the first non-null one).
    ;;
    ;; NOTE: There will be cases where more than one of these has
    ;; changed and the new values differ.  This indicates that
    ;; something like the following has happened since the last time
    ;; we looked at the selections: Application X set all the
    ;; selections, then Application Y set only one of them.
    ;; In this case since we don't have
    ;; timestamps there is no way to know what the 'correct' value to
    ;; return is.  The nice thing to do would be to tell the user we
    ;; saw multiple possible selections and ask the user which was the
    ;; one they wanted.
    (or clip-text primary-text)
    ))

(define-obsolete-function-alias 'x-selection-value 'gui-selection-value "25.1")

(defun x-get-clipboard ()
  "Return text pasted to the clipboard."
  (declare (obsolete gui-get-selection "25.1"))
  (gui-backend-get-selection 'CLIPBOARD 'STRING))

(defun gui-get-primary-selection ()
  "Return the PRIMARY selection, or the best emulation thereof."
  (or (gui--selection-value-internal 'PRIMARY)
      (and (fboundp 'w32-get-selection-value)
           (eq (framep (selected-frame)) 'w32)
           ;; MS-Windows emulates PRIMARY in x-get-selection, but only
           ;; within the Emacs session, so consult the clipboard if
           ;; primary is not found.
           (w32-get-selection-value))
      (error "No selection is available")))
(define-obsolete-function-alias 'x-get-selection-value
  'gui-get-primary-selection "25.1")

;;; Lower-level, backend dependent selection handling.

(cl-defgeneric gui-backend-get-selection (_selection-symbol _target-type)
  "Return selected text.
SELECTION-SYMBOL is typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
TARGET-TYPE is the type of data desired, typically `STRING'."
  nil)

(cl-defgeneric gui-backend-set-selection (_selection _value)
  "Method to assert a selection of type SELECTION and value VALUE.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
If VALUE is nil and we own the selection SELECTION, disown it instead.
Disowning it means there is no such selection.
\(Those are literal upper-case symbol names, since that's what X expects.)
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on `selection-converter-alist' know about."
  nil)

(cl-defgeneric gui-backend-selection-owner-p (_selection)
  "Whether the current Emacs process owns the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)"
  nil)

(cl-defgeneric gui-backend-selection-exists-p (_selection)
  "Whether there is an owner for the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)"
  nil)

(defun gui-get-selection (&optional type data-type)
  "Return the value of an X Windows selection.
The argument TYPE (default `PRIMARY') says which selection,
and the argument DATA-TYPE (default `STRING') says
how to convert the data.

TYPE may be any symbol \(but nil stands for `PRIMARY').  However,
only a few symbols are commonly used.  They conventionally have
all upper-case names.  The most often used ones, in addition to
`PRIMARY', are `SECONDARY' and `CLIPBOARD'.

DATA-TYPE is usually `STRING', but can also be one of the symbols
in `selection-converter-alist', which see.  This argument is
ignored on NS, MS-Windows and MS-DOS."
  (let ((data (gui-backend-get-selection (or type 'PRIMARY)
                                         (or data-type 'STRING))))
    (when (and (stringp data)
	       (setq data-type (get-text-property 0 'foreign-selection data)))
      (let ((coding (or next-selection-coding-system
                        selection-coding-system
                        (pcase data-type
                          ('UTF8_STRING 'utf-8)
                          ('COMPOUND_TEXT 'compound-text-with-extensions)
                          ('C_STRING nil)
                          ('STRING 'iso-8859-1)
                          (_ (error "Unknown selection data type: %S"
                                    type))))))
        (setq data (if coding (decode-coding-string data coding)
                     (string-to-multibyte data))))
      (setq next-selection-coding-system nil)
      (put-text-property 0 (length data) 'foreign-selection data-type data))
    data))
(define-obsolete-function-alias 'x-get-selection 'gui-get-selection "25.1")

(defun gui-set-selection (type data)
  "Make an X selection of type TYPE and value DATA.
The argument TYPE (nil means `PRIMARY') says which selection, and
DATA specifies the contents.  TYPE must be a symbol.  \(It can also
be a string, which stands for the symbol with that name, but this
is considered obsolete.)  DATA may be a string, a symbol, an
integer (or a cons of two integers or list of two integers).

The selection may also be a cons of two markers pointing to the same buffer,
or an overlay.  In these cases, the selection is considered to be the text
between the markers *at whatever time the selection is examined*.
Thus, editing done in the buffer after you specify the selection
can alter the effective value of the selection.

The data may also be a vector of valid non-vector selection values.

The return value is DATA.

Interactively, this command sets the primary selection.  Without
prefix argument, it reads the selection in the minibuffer.  With
prefix argument, it uses the text of the region as the selection value.

Note that on MS-Windows, primary and secondary selections set by Emacs
are not available to other programs."
  (interactive (if (not current-prefix-arg)
		   (list 'PRIMARY (read-string "Set text for pasting: "))
		 (list 'PRIMARY (buffer-substring (region-beginning) (region-end)))))
  (if (stringp type) (setq type (intern type)))
  (or (gui--valid-simple-selection-p data)
      (and (vectorp data)
	   (let ((valid t))
	     (dotimes (i (length data))
	       (or (gui--valid-simple-selection-p (aref data i))
		   (setq valid nil)))
	     valid))
      (signal 'error (list "invalid selection" data)))
  (or type (setq type 'PRIMARY))
  (gui-backend-set-selection type data)
  data)
(define-obsolete-function-alias 'x-set-selection 'gui-set-selection "25.1")

(defun gui--valid-simple-selection-p (data)
  (or (bufferp data)
      (and (consp data)
	   (markerp (car data))
	   (markerp (cdr data))
	   (marker-buffer (car data))
	   (buffer-live-p (marker-buffer (car data)))
	   (eq (marker-buffer (car data))
	       (marker-buffer (cdr data))))
      (stringp data)
      (and (overlayp data)
	   (overlay-buffer data)
	   (buffer-live-p (overlay-buffer data)))
      (symbolp data)
      (integerp data)))

;; Functions to convert the selection into various other selection types.
;; Every selection type that Emacs handles is implemented this way, except
;; for TIMESTAMP, which is a special case.

(defun xselect--selection-bounds (value)
  "Return bounds of X selection value VALUE.
The return value is a list (BEG END BUF) if VALUE is a cons of
two markers or an overlay.  Otherwise, it is nil."
  (cond ((bufferp value)
	 (with-current-buffer value
	   (when (mark t)
	     (list (mark t) (point) value))))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (when (and (marker-buffer (car value))
		    (buffer-name (marker-buffer (car value)))
		    (eq (marker-buffer (car value))
			(marker-buffer (cdr value))))
	   (list (marker-position (car value))
		 (marker-position (cdr value))
		 (marker-buffer (car value)))))
	((overlayp value)
	 (when (overlay-buffer value)
	   (list (overlay-start value)
		 (overlay-end value)
		 (overlay-buffer value))))))

(defun xselect--int-to-cons (n)
  (cons (ash n -16) (logand n 65535)))

(defun xselect--encode-string (type str &optional can-modify)
  (when str
    ;; If TYPE is nil, this is a local request; return STR as-is.
    (if (null type)
	str
      ;; Otherwise, encode STR.
      (let ((coding (or next-selection-coding-system
			selection-coding-system)))
	(if coding
	    (setq coding (coding-system-base coding)))
	(let ((inhibit-read-only t))
	  ;; Suppress producing escape sequences for compositions.
	  ;; But avoid modifying the string if it's a buffer name etc.
	  (unless can-modify (setq str (substring str 0)))
	  (remove-text-properties 0 (length str) '(composition nil) str)
	  ;; For X selections, TEXT is a polymorphic target; choose
	  ;; the actual type from `UTF8_STRING', `COMPOUND_TEXT',
	  ;; `STRING', and `C_STRING'.  On Nextstep, always use UTF-8
	  ;; (see ns_string_to_pasteboard_internal in nsselect.m).
	  (when (eq type 'TEXT)
	    (cond
	     ((featurep 'ns)
	      (setq type 'UTF8_STRING))
	     ((not (multibyte-string-p str))
	      (setq type 'C_STRING))
	     (t
	      (let (non-latin-1 non-unicode eight-bit)
		(mapc #'(lambda (x)
			  (if (>= x #x100)
			      (if (< x #x110000)
				  (setq non-latin-1 t)
				(if (< x #x3FFF80)
				    (setq non-unicode t)
				  (setq eight-bit t)))))
		      str)
		(setq type (if (or non-unicode
				   (and
				    non-latin-1
				    ;; If a coding is specified for
				    ;; selection, and that is
				    ;; compatible with COMPOUND_TEXT,
				    ;; use it.
				    coding
				    (eq (coding-system-get coding :mime-charset)
					'x-ctext)))
			       'COMPOUND_TEXT
			     (if non-latin-1 'UTF8_STRING
			       (if eight-bit 'C_STRING
				 'STRING))))))))
	  (cond
	   ((eq type 'UTF8_STRING)
	    (if (or (not coding)
		    (not (eq (coding-system-type coding) 'utf-8)))
		(setq coding 'utf-8))
	    (setq str (encode-coding-string str coding)))

	   ((eq type 'STRING)
	    (if (or (not coding)
		    (not (eq (coding-system-type coding) 'charset)))
		(setq coding 'iso-8859-1))
	    (setq str (encode-coding-string str coding)))

	   ((eq type 'COMPOUND_TEXT)
	    (if (or (not coding)
		    (not (eq (coding-system-type coding) 'iso-2022)))
		(setq coding 'compound-text-with-extensions))
	    (setq str (encode-coding-string str coding)))

	   ((eq type 'C_STRING)
	    (setq str (string-make-unibyte str)))

	   (t
	    (error "Unknown selection type: %S" type)))))

      ;; Most programs are unable to handle NUL bytes in strings.
      (setq str (replace-regexp-in-string "\0" "\\0" str t t))

      (setq next-selection-coding-system nil)
      (cons type str))))

(defun xselect-convert-to-string (_selection type value)
  (let ((str (cond ((stringp value) value)
		   ((setq value (xselect--selection-bounds value))
		    (with-current-buffer (nth 2 value)
		      (buffer-substring (nth 0 value)
					(nth 1 value)))))))
    (xselect--encode-string type str t)))

(defun xselect-convert-to-length (_selection _type value)
  (let ((len (cond ((stringp value)
		    (length value))
		   ((setq value (xselect--selection-bounds value))
		    (abs (- (nth 0 value) (nth 1 value)))))))
    (if len
	(xselect--int-to-cons len))))

(defun xselect-convert-to-targets (_selection _type _value)
  ;; return a vector of atoms, but remove duplicates first.
  (let* ((all (cons 'TIMESTAMP
		    (cons 'MULTIPLE
			  (mapcar 'car selection-converter-alist))))
	 (rest all))
    (while rest
      (cond ((memq (car rest) (cdr rest))
	     (setcdr rest (delq (car rest) (cdr rest))))
	    ((eq (car (cdr rest)) '_EMACS_INTERNAL)  ; shh, it's a secret
	     (setcdr rest (cdr (cdr rest))))
	    (t
	     (setq rest (cdr rest)))))
    (apply 'vector all)))

(defun xselect-convert-to-delete (selection _type _value)
  (gui-backend-set-selection selection nil)
  ;; A return value of nil means that we do not know how to do this conversion,
  ;; and replies with an "error".  A return value of NULL means that we have
  ;; done the conversion (and any side-effects) but have no value to return.
  'NULL)

(defun xselect-convert-to-filename (_selection _type value)
  (when (setq value (xselect--selection-bounds value))
    (xselect--encode-string 'TEXT (buffer-file-name (nth 2 value)))))

(defun xselect-convert-to-charpos (_selection _type value)
  (when (setq value (xselect--selection-bounds value))
    (let ((beg (1- (nth 0 value))) ; zero-based
	  (end (1- (nth 1 value))))
      (cons 'SPAN (vector (xselect--int-to-cons (min beg end))
			  (xselect--int-to-cons (max beg end)))))))

(defun xselect-convert-to-lineno (_selection _type value)
  (when (setq value (xselect--selection-bounds value))
    (with-current-buffer (nth 2 value)
      (let ((beg (line-number-at-pos (nth 0 value)))
	    (end (line-number-at-pos (nth 1 value))))
	(cons 'SPAN (vector (xselect--int-to-cons (min beg end))
			    (xselect--int-to-cons (max beg end))))))))

(defun xselect-convert-to-colno (_selection _type value)
  (when (setq value (xselect--selection-bounds value))
    (with-current-buffer (nth 2 value)
      (let ((beg (progn (goto-char (nth 0 value)) (current-column)))
	    (end (progn (goto-char (nth 1 value)) (current-column))))
	(cons 'SPAN (vector (xselect--int-to-cons (min beg end))
			    (xselect--int-to-cons (max beg end))))))))

(defun xselect-convert-to-os (_selection _type _size)
  (xselect--encode-string 'TEXT (symbol-name system-type)))

(defun xselect-convert-to-host (_selection _type _size)
  (xselect--encode-string 'TEXT (system-name)))

(defun xselect-convert-to-user (_selection _type _size)
  (xselect--encode-string 'TEXT (user-full-name)))

(defun xselect-convert-to-class (_selection _type _size)
  "Convert selection to class.
This function returns the string \"Emacs\"."
  "Emacs")

;; We do not try to determine the name Emacs was invoked with,
;; because it is not clean for a program's behavior to depend on that.
(defun xselect-convert-to-name (_selection _type _size)
  "Convert selection to name.
This function returns the string \"emacs\"."
  "emacs")

(defun xselect-convert-to-integer (_selection _type value)
  (and (integerp value)
       (xselect--int-to-cons value)))

(defun xselect-convert-to-atom (_selection _type value)
  (and (symbolp value) value))

(defun xselect-convert-to-identity (_selection _type value) ; used internally
  (vector value))

;; Null target that tells clipboard managers we support SAVE_TARGETS
;; (see freedesktop.org Clipboard Manager spec).
(defun xselect-convert-to-save-targets (selection _type _value)
  (when (eq selection 'CLIPBOARD)
    'NULL))

(setq selection-converter-alist
      '((TEXT . xselect-convert-to-string)
	(COMPOUND_TEXT . xselect-convert-to-string)
	(STRING . xselect-convert-to-string)
	(UTF8_STRING . xselect-convert-to-string)
	(TARGETS . xselect-convert-to-targets)
	(LENGTH . xselect-convert-to-length)
	(DELETE . xselect-convert-to-delete)
	(FILE_NAME . xselect-convert-to-filename)
	(CHARACTER_POSITION . xselect-convert-to-charpos)
	(LINE_NUMBER . xselect-convert-to-lineno)
	(COLUMN_NUMBER . xselect-convert-to-colno)
	(OWNER_OS . xselect-convert-to-os)
	(HOST_NAME . xselect-convert-to-host)
	(USER . xselect-convert-to-user)
	(CLASS . xselect-convert-to-class)
	(NAME . xselect-convert-to-name)
	(ATOM . xselect-convert-to-atom)
	(INTEGER . xselect-convert-to-integer)
	(SAVE_TARGETS . xselect-convert-to-save-targets)
	(_EMACS_INTERNAL . xselect-convert-to-identity)))

(provide 'select)

;;; select.el ends here
