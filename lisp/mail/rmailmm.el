;;; rmailmm.el --- MIME decoding and display stuff for RMAIL

;; Copyright (C) 2006  Free Software Foundation, Inc.

;; Maintainer: FSF
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Essentially based on the design of Alexander Pohoyda's MIME
;; extensions (mime-display.el and mime.el).  The current design tries
;; to work on the current buffer, without changing it's text.  All it
;; does is add text properties: It uses the text property `invisible'
;; to hide MIME boundaries and ignored media types, and it uses the
;; text property `display' to display something instead of the actual
;; MIME part.

;;; Code:

;;; Variables

(defcustom mime-media-type-handlers-alist
  '(("multipart/.*" mime-multipart-handler)
    ("message/rfc822" mime-toggler-handler)
    ("message/delivery-status" mime-entity-hider-handler)
    ("message/x-body" mime-entity-hider-handler)
    ("message/x-command-input" mime-message/x-command-input-handler)
    ("message/external-body" mime-message/external-body-handler)
    ("text/.*" mime-text-handler)
    ("text/\\(x-\\)?patch" mime-bulk-handler)
    ("image/.*" mime-image-handler)
    ("application/pgp-signature" mime-application/pgp-signature-handler)
    ("\\(image\\|audio\\|video\\|application\\)/.*" mime-bulk-handler))
  "Alist of media type handlers, also known as agents.
Every handler is a list of type (string symbol) where STRING is a
regular expression to match the media type with and SYMBOL is a
function to run."
  :type 'list
  :group 'mime)

(defcustom mime-attachment-dirs-alist
  '(("text/.*" ("~/Documents"))
    ("image/.*" ("~/Pictures"))
    (".*" ("/tmp/")))
  "Default directories to save attachments into.  Each media type may have
it's own directory."
  :type 'list
  :group 'mime)

(defvar mime-total-number-of-bulk-attachments 0
  "A total number of attached bulk bodyparts in the message.  If more than 3,
offer a way to save all attachments at once.")
(put 'mime-total-number-of-bulk-attachments 'permanent-local t)

;;; Utility Functions

(defun mime-hide-region (from to)
  "Put text property `invisible' on the region FROM TO."
  (put-text-property from to 'invisible t))

(defun mime-unhide-region (from to)
  "Remove the text property `invisible' on the region FROM TO."
  (remove-text-properties from to '(invisible nil)))

(defun mime-display-region-as (from to text)
  "Put text property `display' with value TEXT on the region FROM TO."
  (put-text-property from to 'display text))

;;; Buttons

(defun mime-save (button)
  "Save the attachment using info in the BUTTON."
  (let* ((filename (button-get button 'filename))
	 (directory (button-get button 'directory))
	 (data (button-get button 'data)))
    (setq filename (expand-file-name
		    (read-file-name "Save as: "
				    directory nil nil filename)))
    (when (file-regular-p filename)
      (error (message "File `%s' already exists" filename)))
    (with-temp-file filename
      (set-buffer-file-coding-system 'no-conversion)
      (insert data))))

(define-button-type 'mime-save
  'action 'mime-save)

;;; Handlers

(defun mime-text-handler (content-type
			  content-disposition
			  content-transfer-encoding)
  "Handle the current buffer as a plain text MIME part.")

(defun mime-bulk-handler (content-type
			  content-disposition
			  content-transfer-encoding)
  "Handle the current buffer as an attachment to download."
  (setq mime-total-number-of-bulk-attachments
	(1+ mime-total-number-of-bulk-attachments))
  ;; Find the default directory for this media type
  (let* ((directory (catch 'directory
		    (dolist (entry mime-attachment-dirs-alist)
		      (when (string-match (car entry) (car content-type))
			(throw 'directory (cadr entry))))))
	 (filename (or (cdr (assq 'name (cdr content-type)))
		       (cdr (assq 'filename (cdr content-disposition)))
		       "noname"))
	 (button (format "\nAttached %s file: %s"
			 (car content-type)
			 (let ((data (buffer-string)))
			   (with-temp-buffer
			     (insert-button filename :type 'mime-save
					    'filename filename
					    'directory directory
					    'data data)
			     (buffer-string))))))
    (mime-display-region-as (point-min) (point-max) button)))

(defun mime-multipart-handler (content-type
			       content-disposition
			       content-transfer-encoding)
  "Handle the current buffer as a multipart MIME body.
The current buffer should be narrowed to the body.  CONTENT-TYPE,
CONTENT-DISPOSITION, and CONTENT-TRANSFER-ENCODING are the values
of the respective parsed headers.  See `mime-handle' for their
format."
  ;; Some MUAs start boundaries with "--", while it should start
  ;; with "CRLF--", as defined by RFC 2046:
  ;;    The boundary delimiter MUST occur at the beginning of a line,
  ;;    i.e., following a CRLF, and the initial CRLF is considered to
  ;;    be attached to the boundary delimiter line rather than part
  ;;    of the preceding part.
  ;; We currently don't handle that.
  (let ((boundary (cdr (assq 'boundary content-type)))
	(beg (point-min))
	next)
    (unless boundary
      (error "No boundary defined" content-type content-disposition
	     content-transfer-encoding))
    (setq boundary (concat "\n--" boundary))
    ;; Hide the body before the first bodypart
    (goto-char beg)
    (when (and (search-forward boundary nil t)
	       (looking-at "[ \t]*\n"))
      (mime-hide-region beg (match-end 0))
      (setq beg (match-end 0)))
    ;; Reset the counter
    (setq mime-total-number-of-bulk-attachments 0)
    ;; Loop over all body parts, where beg points at the beginning of
    ;; the part and end points at the end of the part.  next points at
    ;; the beginning of the next part.
    (while (search-forward boundary nil t)
      (setq end (match-beginning 0))
      ;; If this is the last boundary according to RFC 2046, hide the
      ;; epilogue, else hide the boundary only.
      (cond ((looking-at "--[ \t]*\n")
	     (setq next (point-max)))
	    ((looking-at "[ \t]*\n")
	     (setq next (match-end 0)))
	    (t
	     (error "Malformed boundary" content-type
		    content-disposition content-transfer-encoding)))
      (mime-hide-region end next)
      ;; Handle the part.
      (save-match-data
	(save-excursion
	  (save-restriction
	    (narrow-to-region beg end)
	    ;; FIXME: Do decoding of content-transfer-encoding
	    (mime-show))))
      (setq beg next)
      (goto-char beg))))

(defun test-mime-multipart-handler ()
  "Test of a mail used as an example in RFC 2046."
  (let ((mail "From: Nathaniel Borenstein <nsb@bellcore.com>
To: Ned Freed <ned@innosoft.com>
Date: Sun, 21 Mar 1993 23:56:48 -0800 (PST)
Subject: Sample message
MIME-Version: 1.0
Content-type: multipart/mixed; boundary=\"simple boundary\"

This is the preamble.  It is to be ignored, though it
is a handy place for composition agents to include an
explanatory note to non-MIME conformant readers.

--simple boundary

This is implicitly typed plain US-ASCII text.
It does NOT end with a linebreak.
--simple boundary
Content-type: text/plain; charset=us-ascii

This is explicitly typed plain US-ASCII text.
It DOES end with a linebreak.

--simple boundary--

This is the epilogue.  It is also to be ignored."))
    (switch-to-buffer (get-buffer-create "*test*"))
    (erase-buffer)
    (insert mail)
    (mime-show t)
    (buffer-string)))

;;; Main code

(defun mime-handle (content-type content-disposition content-transfer-encoding)
  "Handle the current buffer as a MIME part.
The current buffer should be narrowed to the respective body.
CONTENT-TYPE, CONTENT-DISPOSITION, and CONTENT-TRANSFER-ENCODING
are the values of the respective parsed headers.  The parsed
headers for CONTENT-TYPE and CONTENT-DISPOSITION have the form

  \(VALUE . ALIST)

In other words:

  \(VALUE (ATTRIBUTE . VALUE) (ATTRIBUTE . VALUE) ...)

VALUE is a string and ATTRIBUTE is a symbol.

Consider the following header, for example:

Content-Type: multipart/mixed;
	boundary=\"----=_NextPart_000_0104_01C617E4.BDEC4C40\"

The parsed header value:

\(\"multipart/mixed\"
  \(\"boundary\" . \"----=_NextPart_000_0104_01C617E4.BDEC4C40\"))"
  (if (string= "inline" (car content-disposition))
      (let ((stop nil))
	(dolist (entry mime-media-type-handlers-alist)
	  (when (and (string-match (car entry) (car content-type)) (not stop))
	    (progn
	      (setq stop (funcall (cadr entry) content-type
				  content-disposition
				  content-transfer-encoding))))))
    ;; treat everything else as an attachment
    (mime-bulk-handler content-type
		       content-disposition
		       content-transfer-encoding)))

(defun mime-show (&optional show-headers)
  "Handle the current buffer as a MIME message.
If SHOW-HEADERS is non-nil, then the headers of the current part
are not all hidden, as they usually are \(except for
message/rfc822 content types\).  This is usually only used for
the top-level call.

The current buffer must be narrowed to a single message.
This function will be called recursively if multiple parts
are available."
  (let ((end (point-min))
	content-type
	content-transfer-encoding
	content-disposition)
    ;; `point-min' returns the beginning and `end' points at the end
    ;; of the headers.  We're not using `rmail-header-get-header'
    ;; because we must be able to handle the case of no headers
    ;; existing in a part.  In this case end is at point-min.
    (goto-char (point-min))
    ;; If we're showing a part without headers, then it will start
    ;; with a newline.
    (if (eq (char-after) ?\n)
	(setq end (1+ (point)))
      (when (search-forward "\n\n" nil t)
	(setq end (match-end 0))
	(save-restriction
	  (narrow-to-region (point-min) end)
	  ;; FIXME: Default disposition of the multipart entities should
	  ;; be inherited.
	  (setq content-type
		(mail-fetch-field "Content-Type")
		content-transfer-encoding
		(mail-fetch-field "Content-Transfer-Encoding")
		content-disposition
		(mail-fetch-field "Content-Disposition")))))
    (if content-type
	(setq content-type (mail-header-parse-content-type
			    content-type))
      ;; FIXME: Default "message/rfc822" in a "multipart/digest"
      ;; according to RFC 2046.
      (setq content-type '("text/plain")))
    (setq content-disposition
	  (if content-disposition
	      (mail-header-parse-content-disposition content-disposition)
	    ;; If none specified, we are free to choose what we deem
	    ;; suitable according to RFC 2183.  We like inline.
	    '("inline")))
    ;; Hide headers.
    (if (or (string= (car content-type) "message/rfc822")
	    show-headers)
	(rmail-header-hide-headers)
      (mime-hide-region (point-min) end))
    ;; Unrecognized disposition types are to be treated like
    ;; attachment according to RFC 2183.
    (unless (string= (car content-disposition) "inline")
      (setq content-disposition '("attachment")))
    (save-restriction
      (narrow-to-region end (point-max))
      (mime-handle content-type content-disposition
		   content-transfer-encoding))))
