;;; mm-encode.el --- functions for encoding MIME things 
;; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'mail-parse)
(require 'mailcap)
(eval-and-compile
  (autoload 'mm-body-7-or-8 "mm-bodies"))

(defvar mm-content-transfer-encoding-defaults
  '(("text/x-patch" 8bit)
    ("text/.*" qp-or-base64)
    ("message/rfc822" 8bit)
    ("application/emacs-lisp" 8bit)
    ("application/x-patch" 8bit)
    (".*" qp-or-base64))
  "Alist of regexps that match MIME types and their encodings.
If the encoding is `qp-or-base64', then either quoted-printable
or base64 will be used, depending on what is more efficient.")

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
  "Return a safer but similar encoding."
  (cond
   ((memq encoding '(7bit 8bit quoted-printable)) 'quoted-printable)
   ;; The remaing encodings are binary and base64 (and perhaps some
   ;; non-standard ones), which are both turned into base64.
   (t 'base64)))

(defun mm-encode-content-transfer-encoding (encoding &optional type)
  (cond
   ((eq encoding 'quoted-printable)
    (quoted-printable-encode-region (point-min) (point-max) t))
   ((eq encoding 'base64)
    (when (equal type "text/plain")
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
	(replace-match "\r\n" t t)))
    (condition-case error
	(base64-encode-region (point-min) (point-max))
      (error
       (message "Error while decoding: %s" error)
       nil)))
   ((memq encoding '(7bit 8bit binary))
    ;; Do nothing.
    )
   ((null encoding)
    ;; Do nothing.
    )
   ((functionp encoding)
    (ignore-errors (funcall encoding (point-min) (point-max))))
   (t
    (message "Unknown encoding %s; defaulting to 8bit" encoding))))

(defun mm-encode-buffer (type)
  "Encode the buffer which contains data of TYPE.
The encoding used is returned."
  (let* ((mime-type (if (stringp type) type (car type)))
	 (encoding
	  (or (and (listp type)
		   (cadr (assq 'encoding type)))
	      (mm-content-transfer-encoding mime-type)))
	 (bits (mm-body-7-or-8)))
    ;; We force buffers that are 7bit to be unencoded, no matter
    ;; what the preferred encoding is.
    (when (eq bits '7bit)
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
	'base64))))

(provide 'mm-encode)

;;; mm-encode.el ends here
