;;; mm-encode.el --- Functions for encoding MIME things
;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'mail-parse)
(require 'mailcap)
(eval-and-compile
  (autoload 'mm-body-7-or-8 "mm-bodies")
  (autoload 'mm-long-lines-p "mm-bodies"))

(defcustom mm-content-transfer-encoding-defaults
  '(("text/x-patch" 8bit)
    ("text/.*" qp-or-base64)
    ("message/rfc822" 8bit)
    ("application/emacs-lisp" qp-or-base64)
    ("application/x-emacs-lisp" qp-or-base64)
    ("application/x-patch" qp-or-base64)
    (".*" base64))
  "Alist of regexps that match MIME types and their encodings.
If the encoding is `qp-or-base64', then either quoted-printable
or base64 will be used, depending on what is more efficient.

`qp-or-base64' has another effect.  It will fold long lines so that
MIME parts may not be broken by MTA.  So do `quoted-printable' and
`base64'.

Note: It affects body encoding only when a part is a raw forwarded
message (which will be made by `gnus-summary-mail-forward' with the
arg 2 for example) or is neither the text/* type nor the message/*
type.  Even though in those cases, you can use the `encoding' MML tag
to specify encoding of non-ASCII MIME parts."
  :type '(repeat (list (regexp :tag "MIME type")
		       (choice :tag "encoding"
			       (const 7bit)
			       (const 8bit)
			       (const qp-or-base64)
			       (const quoted-printable)
			       (const base64))))
  :group 'mime)

(defvar mm-use-ultra-safe-encoding nil
  "If non-nil, use encodings aimed at Procrustean bed survival.

This means that textual parts are encoded as quoted-printable if they
contain lines longer than 76 characters or starting with \"From \" in
the body.  Non-7bit encodings (8bit, binary) are generally disallowed.
This is to reduce the probability that a broken MTA or MDA changes the
message.

This variable should never be set directly, but bound before a call to
`mml-generate-mime' or similar functions.")

(defun mm-insert-rfc822-headers (charset encoding)
  "Insert text/plain headers with CHARSET and ENCODING."
  (insert "MIME-Version: 1.0\n")
  (insert "Content-Type: text/plain; charset="
	  (mail-quote-string (downcase (symbol-name charset))) "\n")
  (insert "Content-Transfer-Encoding: "
	  (downcase (symbol-name encoding)) "\n"))

(defun mm-insert-multipart-headers ()
  "Insert multipart/mixed headers."
  (let ((boundary "=-=-="))
    (insert "MIME-Version: 1.0\n")
    (insert "Content-Type: multipart/mixed; boundary=\"" boundary "\"\n")
    boundary))

(defun mm-default-file-encoding (file)
  "Return a default encoding for FILE."
  (if (not (string-match "\\.[^.]+$" file))
      "application/octet-stream"
    (mailcap-extension-to-mime (match-string 0 file))))

(defun mm-safer-encoding (encoding)
  "Return an encoding similar to ENCODING but safer than it."
  (cond
   ((eq encoding '7bit) '7bit) ;; 7bit is considered safe.
   ((memq encoding '(8bit quoted-printable)) 'quoted-printable)
   ;; The remaining encodings are binary and base64 (and perhaps some
   ;; non-standard ones), which are both turned into base64.
   (t 'base64)))

(defun mm-encode-content-transfer-encoding (encoding &optional type)
  "Encode the current buffer with ENCODING for MIME type TYPE.
ENCODING can be: nil (do nothing); one of `quoted-printable', `base64';
`7bit', `8bit' or `binary' (all do nothing); a function to do the encoding."
  (cond
   ((eq encoding 'quoted-printable)
    ;; This used to try to make a multibyte buffer unibyte.  That's
    ;; completely wrong, since you'd get QP-encoded emacs-mule.  If
    ;; this gets run on multibyte text it's an error that needs
    ;; fixing, and the encoding function will signal an error.
    ;; Likewise base64 below.
    (quoted-printable-encode-region (point-min) (point-max) t))
   ((eq encoding 'base64)
    (when (equal type "text/plain")
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
	(replace-match "\r\n" t t)))
    (base64-encode-region (point-min) (point-max)))
   ((memq encoding '(7bit 8bit binary))
    ;; Do nothing.
    )
   ((null encoding)
    ;; Do nothing.
    )
   ;; Fixme: Ignoring errors here looks bogus.
   ((functionp encoding)
    (ignore-errors (funcall encoding (point-min) (point-max))))
   (t
    (error "Unknown encoding %s" encoding))))

(defun mm-encode-buffer (type)
  "Encode the buffer which contains data of MIME type TYPE.
TYPE is a string or a list of the components.
The encoding used is returned."
  (let* ((mime-type (if (stringp type) type (car type)))
	 (encoding
	  (or (and (listp type)
		   (cadr (assq 'encoding type)))
	      (mm-content-transfer-encoding mime-type)))
	 (bits (mm-body-7-or-8)))
    ;; We force buffers that are 7bit to be unencoded, no matter
    ;; what the preferred encoding is.
    ;; Only if the buffers don't contain lone lines.
    (when (and (eq bits '7bit) (not (mm-long-lines-p 76)))
      (setq encoding bits))
    (mm-encode-content-transfer-encoding encoding mime-type)
    encoding))

(defun mm-insert-headers (type encoding &optional file)
  "Insert headers for TYPE."
  (insert "Content-Type: " type)
  (when file
    (insert ";\n\tname=\"" (file-name-nondirectory file) "\""))
  (insert "\n")
  (insert (format "Content-Transfer-Encoding: %s\n" encoding))
  (insert "Content-Disposition: inline")
  (when file
    (insert ";\n\tfilename=\"" (file-name-nondirectory file) "\""))
  (insert "\n")
  (insert "\n"))

(defun mm-content-transfer-encoding (type)
  "Return a CTE suitable for TYPE to encode the current buffer."
  (let ((rules mm-content-transfer-encoding-defaults))
    (catch 'found
      (while rules
	(when (string-match (caar rules) type)
	  (throw 'found
		 (let ((encoding
			(if (eq (cadr (car rules)) 'qp-or-base64)
			    (mm-qp-or-base64)
			  (cadr (car rules)))))
		   (if mm-use-ultra-safe-encoding
		       (mm-safer-encoding encoding)
		     encoding))))
	(pop rules)))))

(defun mm-qp-or-base64 ()
  "Return the type with which to encode the buffer.
This is either `base64' or `quoted-printable'."
  (if (equal mm-use-ultra-safe-encoding '(sign . "pgp"))
      ;; perhaps not always accurate?
      'quoted-printable
    (save-excursion
      (let ((limit (min (point-max) (+ 2000 (point-min))))
	    (n8bit 0))
	(goto-char (point-min))
	(skip-chars-forward "\x20-\x7f\r\n\t" limit)
	(while (< (point) limit)
	  (incf n8bit)
	  (forward-char 1)
	  (skip-chars-forward "\x20-\x7f\r\n\t" limit))
	(if (or (< (* 6 n8bit) (- limit (point-min)))
		;; Don't base64, say, a short line with a single
		;; non-ASCII char when splitting parts by charset.
		(= n8bit 1))
	    'quoted-printable
	  'base64)))))

(provide 'mm-encode)

;;; arch-tag: 7d01bba4-d469-4851-952b-dc863f84ed66
;;; mm-encode.el ends here
