;;; select.el --- lisp portion of standard selection support.

;; Keywords: internal

;; Copyright (c) 1993, 1994 Free Software Foundation, Inc.
;; Based partially on earlier release by Lucid.

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

;; This is for temporary compatibility with pre-release Emacs 19.
(defalias 'x-selection 'x-get-selection)
(defun x-get-selection (&optional type data-type)
  "Return the value of an X Windows selection.
The argument TYPE (default `PRIMARY') says which selection, 
and the argument DATA-TYPE (default `STRING') says how to convert the data."
  (x-get-selection-internal (or type 'PRIMARY) (or data-type 'STRING)))

(defun x-get-clipboard ()
  "Return text pasted to the clipboard."
  (x-get-selection-internal 'CLIPBOARD 'STRING))

(defun x-set-selection (type data)
  "Make an X Windows selection of type TYPE and value DATA.
The argument TYPE (default `PRIMARY') says which selection, 
and DATA specifies the contents.  DATA may be a string,
a symbol, an integer (or a cons of two integers or list of two integers).

The selection may also be a cons of two markers pointing to the same buffer,
or an overlay.  In these cases, the selection is considered to be the text 
between the markers *at whatever time the selection is examined*.
Thus, editing done in the buffer after you specify the selection
can alter the effective value of the selection.

The data may also be a vector of valid non-vector selection values.

Interactively, the text of the region is used as the selection value."
  (interactive (if (not current-prefix-arg)
		   (list 'PRIMARY (read-string "Set text for pasting: "))
		 (list 'PRIMARY (substring (region-beginning) (region-end)))))
  ;; This is for temporary compatibility with pre-release Emacs 19.
  (if (stringp type)
      (setq type (intern type)))
  (or (x-valid-simple-selection-p data)
      (and (vectorp data)
	   (let ((valid t)
		 (i (1- (length data))))
	     (while (>= i 0)
	       (or (x-valid-simple-selection-p (aref data i))
		   (setq valid nil))
	       (setq i (1- i)))
	     valid))
      (signal 'error (list "invalid selection" data)))
  (or type (setq type 'PRIMARY))
  (if data
      (x-own-selection-internal type data)
    (x-disown-selection-internal type))
  data)

(defun x-valid-simple-selection-p (data)
  (or (stringp data)
      (symbolp data)
      (integerp data)
      (and (consp data)
	   (integerp (car data))
	   (or (integerp (cdr data))
	       (and (consp (cdr data))
		    (integerp (car (cdr data))))))
      (overlayp data)
      (and (consp data)
	   (markerp (car data))
	   (markerp (cdr data))
	   (marker-buffer (car data))
	   (marker-buffer (cdr data))
	   (eq (marker-buffer (car data))
	       (marker-buffer (cdr data)))
	   (buffer-name (marker-buffer (car data)))
	   (buffer-name (marker-buffer (cdr data))))))

;;; Cut Buffer support

(defun x-get-cut-buffer (&optional which-one)
  "Returns the value of one of the 8 X server cut-buffers.  Optional arg
WHICH-ONE should be a number from 0 to 7, defaulting to 0.
Cut buffers are considered obsolete; you should use selections instead."
  (x-get-cut-buffer-internal
   (if which-one
       (aref [CUT_BUFFER0 CUT_BUFFER1 CUT_BUFFER2 CUT_BUFFER3
	      CUT_BUFFER4 CUT_BUFFER5 CUT_BUFFER6 CUT_BUFFER7]
	     which-one)
     'CUT_BUFFER0)))

(defun x-set-cut-buffer (string &optional push)
  "Store STRING into the X server's primary cut buffer.
If PUSH is non-nil, also rotate the cut buffers:
this means the previous value of the primary cut buffer moves the second
cut buffer, and the second to the third, and so on (there are 8 buffers.)
Cut buffers are considered obsolete; you should use selections instead."
  ;; Check the data type of STRING.
  (substring string 0 0)
  (if push
      (x-rotate-cut-buffers-internal 1))
  (x-store-cut-buffer-internal 'CUT_BUFFER0 string))


;;; Functions to convert the selection into various other selection types.
;;; Every selection type that Emacs handles is implemented this way, except
;;; for TIMESTAMP, which is a special case.

(defun xselect-convert-to-string (selection type value)
  (cond ((stringp value)
	 value)
	((overlayp value)
	 (save-excursion
	   (or (buffer-name (overlay-buffer value))
	       (error "selection is in a killed buffer"))
	   (set-buffer (overlay-buffer value))
	   (buffer-substring (overlay-start value)
			     (overlay-end value))))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (or (eq (marker-buffer (car value)) (marker-buffer (cdr value)))
	     (signal 'error
		     (list "markers must be in the same buffer"
			   (car value) (cdr value))))
	 (save-excursion
	   (set-buffer (or (marker-buffer (car value))
			   (error "selection is in a killed buffer")))
	   (buffer-substring (car value) (cdr value))))
	(t nil)))

(defun xselect-convert-to-length (selection type value)
  (let ((value
	 (cond ((stringp value)
		(length value))
	       ((overlayp value)
		(abs (- (overlay-end value) (overlay-start value))))
	       ((and (consp value)
		     (markerp (car value))
		     (markerp (cdr value)))
		(or (eq (marker-buffer (car value))
			(marker-buffer (cdr value)))
		    (signal 'error
			    (list "markers must be in the same buffer"
				  (car value) (cdr value))))
		(abs (- (car value) (cdr value)))))))
    (if value ; force it to be in 32-bit format.
	(cons (ash value -16) (logand value 65535))
      nil)))

(defun xselect-convert-to-targets (selection type value)
  ;; return a vector of atoms, but remove duplicates first.
  (let* ((all (cons 'TIMESTAMP (mapcar 'car selection-converter-alist)))
	 (rest all))
    (while rest
      (cond ((memq (car rest) (cdr rest))
	     (setcdr rest (delq (car rest) (cdr rest))))
	    ((eq (car (cdr rest)) '_EMACS_INTERNAL)  ; shh, it's a secret
	     (setcdr rest (cdr (cdr rest))))
	    (t
	     (setq rest (cdr rest)))))
    (apply 'vector all)))

(defun xselect-convert-to-delete (selection type value)
  (x-disown-selection-internal selection)
  ;; A return value of nil means that we do not know how to do this conversion,
  ;; and replies with an "error".  A return value of NULL means that we have
  ;; done the conversion (and any side-effects) but have no value to return.
  'NULL)

(defun xselect-convert-to-filename (selection type value)
  (cond ((overlayp value)
	 (buffer-file-name (or (overlay-buffer value)
			       (error "selection is in a killed buffer"))))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (buffer-file-name (or (marker-buffer (car value))
			       (error "selection is in a killed buffer"))))
	(t nil)))

(defun xselect-convert-to-charpos (selection type value)
  (let (a b tmp)
    (cond ((cond ((overlayp value)
		  (setq a (overlay-start value)
			b (overlay-end value)))
		 ((and (consp value)
		       (markerp (car value))
		       (markerp (cdr value)))
		  (setq a (car value)
			b (cdr value))))
	   (setq a (1- a) b (1- b)) ; zero-based
	   (if (< b a) (setq tmp a a b b tmp))
	   (cons 'SPAN
		 (vector (cons (ash a -16) (logand a 65535))
			 (cons (ash b -16) (logand b 65535))))))))

(defun xselect-convert-to-lineno (selection type value)
  (let (a b buf tmp)
    (cond ((cond ((and (consp value)
		       (markerp (car value))
		       (markerp (cdr value)))
		  (setq a (marker-position (car value))
			b (marker-position (cdr value))
			buf (marker-buffer (car value))))
		 ((overlayp value)
		  (setq buf (overlay-buffer value)
			a (overlay-start value)
			b (overlay-end value)))
		 )
	   (save-excursion
	     (set-buffer buf)
	     (setq a (count-lines 1 a)
		   b (count-lines 1 b)))
	   (if (< b a) (setq tmp a a b b tmp))
	   (cons 'SPAN
		 (vector (cons (ash a -16) (logand a 65535))
			 (cons (ash b -16) (logand b 65535))))))))

(defun xselect-convert-to-colno (selection type value)
  (let (a b buf tmp)
    (cond ((cond ((and (consp value)
		       (markerp (car value))
		       (markerp (cdr value)))
		  (setq a (car value)
			b (cdr value)
			buf (marker-buffer a)))
		 ((overlayp value)
		  (setq buf (overlay-buffer value)
			a (overlay-start value)
			b (overlay-end value)))
		 )
	   (save-excursion
	     (set-buffer buf)
	     (goto-char a)
	     (setq a (current-column))
	     (goto-char b)
	     (setq b (current-column)))
	   (if (< b a) (setq tmp a a b b tmp))
	   (cons 'SPAN
		 (vector (cons (ash a -16) (logand a 65535))
			 (cons (ash b -16) (logand b 65535))))))))

(defun xselect-convert-to-os (selection type size)
  (symbol-name system-type))

(defun xselect-convert-to-host (selection type size)
  (system-name))

(defun xselect-convert-to-user (selection type size)
  (user-full-name))

(defun xselect-convert-to-class (selection type size)
  "Emacs")

;; We do not try to determine the name Emacs was invoked with,
;; because it is not clean for a program's behavior to depend on that.
(defun xselect-convert-to-name (selection type size)
  "emacs")

(defun xselect-convert-to-integer (selection type value)
  (and (integerp value)
       (cons (ash value -16) (logand value 65535))))

(defun xselect-convert-to-atom (selection type value)
  (and (symbolp value) value))

(defun xselect-convert-to-identity (selection type value) ; used internally
  (vector value))

(setq selection-converter-alist
      '((TEXT . xselect-convert-to-string)
	(STRING . xselect-convert-to-string)
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
	(_EMACS_INTERNAL . xselect-convert-to-identity)
	))

(provide 'select)

;;; select.el ends here.
