;;; utf7.el --- UTF-7 encoding/decoding for Emacs
;; Copyright (C) 1999, 2000 Free Software Foundation, Inc.

;; Author: Jon K Hellan <hellan@acm.org>
;; Keywords: mail

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; UTF-7 - A Mail-Safe Transformation Format of Unicode - RFC 2152
;;; This is a transformation format of Unicode that contains only 7-bit
;;; ASCII octets and is intended to be readable by humans in the limiting
;;; case that the document consists of characters from the US-ASCII
;;; repertoire.
;;; In short, runs of characters outside US-ASCII are encoded as base64
;;; inside delimiters.
;;; A variation of UTF-7 is specified in IMAP 4rev1 (RFC 2060) as the way
;;; to represent characters outside US-ASCII in mailbox names in IMAP.
;;; This library supports both variants, but the IMAP variation was the
;;; reason I wrote it.
;;; The routines convert UTF-7 -> UTF-16 (16 bit encoding of Unicode)
;;; -> current character set, and vice versa.
;;; However, until Emacs supports Unicode, the only Emacs character set
;;; supported here is ISO-8859.1, which can trivially be converted to/from
;;; Unicode.
;;; When decoding results in a character outside the Emacs character set,
;;; an error is thrown.  It is up to the application to recover.

;;; Code:

(require 'base64)
(eval-when-compile (require 'cl))

(defvar utf7-direct-encoding-chars " -%'-*,-[]-}"
  "Character ranges which do not need escaping in UTF-7.")

(defvar utf7-imap-direct-encoding-chars
  (concat utf7-direct-encoding-chars "+\\~")
  "Character ranges which do not need escaping in the IMAP variant of UTF-7.")

(defsubst utf7-imap-get-pad-length (len modulus)
  "Return required length of padding for IMAP modified base64 fragment."
  (mod (- len) modulus))

(defun utf7-encode-internal (&optional for-imap)
  "Encode text in (temporary) buffer as UTF-7.
Use IMAP modification if FOR-IMAP is non-nil."
  (let ((start (point-min))
	(end (point-max)))
    (narrow-to-region start end)
    (goto-char start)
    (let ((esc-char (if for-imap ?& ?+))
	  (direct-encoding-chars
	   (if for-imap utf7-imap-direct-encoding-chars
	     utf7-direct-encoding-chars)))
      (while (not (eobp))
	(skip-chars-forward direct-encoding-chars)
	(unless (eobp)
	  (insert esc-char)
	  (let ((p (point))
		(fc (following-char))
		(run-length
		 (skip-chars-forward (concat "^" direct-encoding-chars))))
	    (if (and (= fc esc-char)
		     (= run-length 1))	; Lone esc-char?
		(delete-backward-char 1) ; Now there's one too many
	      (utf7-fragment-encode p (point) for-imap))
	    (insert "-")))))))

(defun utf7-fragment-encode (start end &optional for-imap)
  "Encode text from START to END in buffer as UTF-7 escape fragment.
Use IMAP modification if FOR-IMAP is non-nil."
  (save-restriction
    (narrow-to-region start end)
    (funcall (utf7-get-u16char-converter 'to-utf-16))
    (base64-encode-region start (point-max))
    (goto-char start)
    (let ((pm (point-max)))
      (when for-imap
	(while (search-forward "/" nil t)
	  (replace-match ",")))
      (skip-chars-forward "^= \t\n" pm)
      (delete-region (point) pm))))

(defun utf7-decode-internal (&optional for-imap)
  "Decode UTF-7 text in (temporary) buffer.
Use IMAP modification if FOR-IMAP is non-nil."
  (let ((start (point-min))
	(end (point-max)))
    (goto-char start)
    (let* ((esc-pattern (concat "^" (char-to-string (if for-imap ?& ?+))))
	   (base64-chars (concat "A-Za-z0-9+"
				 (char-to-string (if for-imap ?, ?/)))))
      (while (not (eobp))
	(skip-chars-forward esc-pattern)
	(unless (eobp)
	  (forward-char)
	  (let ((p (point))
		(run-length (skip-chars-forward base64-chars)))
	    (when (and (not (eobp)) (= (following-char) ?-))
	      (delete-char 1))
	    (unless (= run-length 0)	; Encoded lone esc-char?
	      (save-excursion
		(utf7-fragment-decode p (point) for-imap)
		(goto-char p)
		(delete-backward-char 1)))))))))

(defun utf7-fragment-decode (start end &optional for-imap)
  "Decode base64 encoded fragment from START to END of UTF-7 text in buffer.
Use IMAP modification if FOR-IMAP is non-nil."
  (save-restriction
    (narrow-to-region start end)
    (when for-imap
      (goto-char start)
      (while (search-forward "," nil 'move-to-end) (replace-match "/")))
    (let ((pl (utf7-imap-get-pad-length (- end start) 4)))
      (insert (make-string pl ?=))
      (base64-decode-region start (+ end pl)))
    (funcall (utf7-get-u16char-converter 'from-utf-16))))

(defun utf7-get-u16char-converter (which-way)
  "Return a function to convert between UTF-16 and current character set."
  ;; Add test to check if we are really Latin-1.
  ;; Support other character sets once Emacs groks Unicode.
  (if (eq which-way 'to-utf-16)
      'utf7-latin1-u16-char-converter
    'utf7-u16-latin1-char-converter))

(defun utf7-latin1-u16-char-converter ()
  "Convert latin 1 (ISO-8859.1) characters to 16 bit Unicode.
Characters are converted to raw byte pairs in narrowed buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (insert 0)
    (forward-char)))

(defun utf7-u16-latin1-char-converter ()
  "Convert 16 bit Unicode characters to latin 1 (ISO-8859.1).
Characters are in raw byte pairs in narrowed buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (if (= 0 (following-char))
	(delete-char 1)
	(error "Unable to convert from Unicode"))
    (forward-char)))

(defun utf7-encode (string &optional for-imap)
  "Encode UTF-7 STRING.  Use IMAP modification if FOR-IMAP is non-nil."
  (let ((default-enable-multibyte-characters nil))
    (with-temp-buffer
      (insert string)
      (utf7-encode-internal for-imap)
      (buffer-string))))

(defun utf7-decode (string &optional for-imap)
  "Decode UTF-7 STRING.  Use IMAP modification if FOR-IMAP is non-nil."
  (let ((default-enable-multibyte-characters nil))
    (with-temp-buffer
      (insert string)
      (utf7-decode-internal for-imap)
      (buffer-string))))

(provide 'utf7)

;;; utf7.el ends here
