;;; pmailmm.el --- MIME decoding and display stuff for PMAIL

;; Copyright (C) 2006, 2007, 2008  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Essentially based on the design of Alexander Pohoyda's MIME
;; extensions (mime-display.el and mime.el).  To use, copy a complete
;; message into a new buffer and call (mime-show t).

;; To use:

;; (autoload 'pmail-mime "pmailmm"
;;   "Show MIME message." t)
;; (add-hook 'pmail-mode-hook
;;           (lambda ()
;;             (define-key pmail-mode-map (kbd "v")
;;               'pmail-mime)))

;;; Code:

;; For ...
(require 'pmail)

;;; Variables

(defcustom pmail-mime-media-type-handlers-alist
  '(("multipart/.*" pmail-mime-multipart-handler)
    ("text/.*" pmail-mime-text-handler)
    ("text/\\(x-\\)?patch" pmail-mime-bulk-handler)
    ("application/pgp-signature" pmail-mime-application/pgp-signature-handler)
    ("\\(image\\|audio\\|video\\|application\\)/.*" pmail-mime-bulk-handler))
  "Alist of media type handlers, also known as agents.
Every handler is a list of type (string symbol) where STRING is a
regular expression to match the media type with and SYMBOL is a
function to run.  Handlers should return a non-nil value if the
job is done."
  :type 'list
  :group 'mime)

(defcustom pmail-mime-attachment-dirs-alist
  '(("text/.*" "~/Documents")
    ("image/.*" "~/Pictures")
    (".*" "~/Desktop" "~" "/tmp"))
  "Default directories to save attachments into.
Each media type may have it's own list of directories in order of
preference.  The first existing directory in the list will be
used."
  :type 'list
  :group 'mime)

(defvar pmail-mime-total-number-of-bulk-attachments 0
  "A total number of attached bulk bodyparts in the message.  If more than 3,
offer a way to save all attachments at once.")
(put 'pmail-mime-total-number-of-bulk-attachments 'permanent-local t)

;;; Buttons

(defun pmail-mime-save (button)
  "Save the attachment using info in the BUTTON."
  (let* ((filename (button-get button 'filename))
	 (directory (button-get button 'directory))
	 (data (button-get button 'data)))
    (while (file-exists-p (expand-file-name filename directory))
      (let* ((f (file-name-sans-extension filename))
	     (i 1))
	(when (string-match "-\\([0-9]+\\)$" f)
	  (setq i (1+ (string-to-number (match-string 1 f)))
		f (substring f 0 (match-beginning 0))))
	(setq filename (concat f "-" (number-to-string i) "."
			       (file-name-extension filename)))))
    (setq filename (expand-file-name
		    (read-file-name (format "Save as (default: %s): " filename)
				    directory
				    (expand-file-name filename directory))
		    directory))
    (when (file-regular-p filename)
      (error (message "File `%s' already exists" filename)))
    (with-temp-file filename
      (set-buffer-file-coding-system 'no-conversion)
      (insert data))))

(define-button-type 'pmail-mime-save
  'action 'pmail-mime-save)

;;; Handlers

(defun pmail-mime-text-handler (content-type
				content-disposition
				content-transfer-encoding)
  "Handle the current buffer as a plain text MIME part."
  (let* ((charset (cdr (assq 'charset (cdr content-type))))
	 (coding-system (when charset
			  (intern (downcase charset)))))
    (when (coding-system-p coding-system)
      (decode-coding-region (point-min) (point-max) coding-system))))

(defun test-pmail-mime-handler ()
  "Test of a mail using no MIME parts at all."
  (let ((mail "To: alex@gnu.org
Content-Type: text/plain; charset=koi8-r
Content-Transfer-Encoding: 8bit
MIME-Version: 1.0

\372\304\322\301\327\323\324\327\325\312\324\305\41"))
    (switch-to-buffer (get-buffer-create "*test*"))
    (erase-buffer)
    (set-buffer-multibyte nil)
    (insert mail)
    (pmail-mime-show t)
    (set-buffer-multibyte t)))

(defun pmail-mime-bulk-handler (content-type
				content-disposition
				content-transfer-encoding)
  "Handle the current buffer as an attachment to download."
  (setq pmail-mime-total-number-of-bulk-attachments
	(1+ pmail-mime-total-number-of-bulk-attachments))
  ;; Find the default directory for this media type
  (let* ((directory (catch 'directory
		      (dolist (entry pmail-mime-attachment-dirs-alist)
			(when (string-match (car entry) (car content-type))
			  (dolist (dir (cdr entry))
			    (when (file-directory-p dir)
			      (throw 'directory dir)))))))
	 (filename (or (cdr (assq 'name (cdr content-type)))
		       (cdr (assq 'filename (cdr content-disposition)))
		       "noname"))
	 (label (format "\nAttached %s file: " (car content-type)))
	 (data (buffer-string)))
    (delete-region (point-min) (point-max))
    (insert label)
    (insert-button filename
		   :type 'pmail-mime-save
		   'filename filename
		   'directory directory
		   'data data)))

(defun test-pmail-mime-bulk-handler ()
  "Test of a mail used as an example in RFC 2183."
  (let ((mail "Content-Type: image/jpeg
Content-Disposition: attachment; filename=genome.jpeg;
  modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\";
Content-Description: a complete map of the human genome
Content-Transfer-Encoding: base64

iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAABGdBTUEAALGPC/xhBQAAAAZQ
TFRF////AAAAVcLTfgAAAPZJREFUeNq9ldsOwzAIQ+3//+l1WlvA5ZLsoUiTto4TB+ISoAjy
+ITfRBfcAmgRFFeAm+J6uhdKdFhFWUgDkFsK0oUp/9G2//Kj7Jx+5tSKOdBscgUYiKHRS/me
WATQdRUvAK0Bnmshmtn79PpaLBbbOZkjKvRnjRZoRswOkG1wFchKew2g9wXVJVZL/m4+B+vv
9AxQQR2Q33SgAYJzzVACdAWjAfRYzYFO9n6SLnydtQHSMxYDMAKqZ/8FS/lTK+zuq3CtK64L
UDwbgUEAUmk2Zyg101d6PhCDySgAvTvDgKiuOrc4dLxUb7UMnhGIexyI+d6U+ABuNAP4Simx
lgAAAABJRU5ErkJggg==
"))
    (switch-to-buffer (get-buffer-create "*test*"))
    (erase-buffer)
    (insert mail)
    (pmail-mime-show)))

(defun pmail-mime-multipart-handler (content-type
				     content-disposition
				     content-transfer-encoding)
  "Handle the current buffer as a multipart MIME body.
The current buffer should be narrowed to the body.  CONTENT-TYPE,
CONTENT-DISPOSITION, and CONTENT-TRANSFER-ENCODING are the values
of the respective parsed headers.  See `pmail-mime-handle' for their
format."
  ;; Some MUAs start boundaries with "--", while it should start
  ;; with "CRLF--", as defined by RFC 2046:
  ;;    The boundary delimiter MUST occur at the beginning of a line,
  ;;    i.e., following a CRLF, and the initial CRLF is considered to
  ;;    be attached to the boundary delimiter line rather than part
  ;;    of the preceding part.
  ;; We currently don't handle that.
  (let ((boundary (cdr (assq 'boundary content-type)))
	beg end next)
    (unless boundary
      (pmail-mm-get-boundary-error-message
       "No boundary defined" content-type content-disposition
       content-transfer-encoding))
    (setq boundary (concat "\n--" boundary))
    ;; Hide the body before the first bodypart
    (goto-char (point-min))
    (when (and (search-forward boundary nil t)
	       (looking-at "[ \t]*\n"))
      (delete-region (point-min) (match-end 0)))
    ;; Reset the counter
    (setq pmail-mime-total-number-of-bulk-attachments 0)
    ;; Loop over all body parts, where beg points at the beginning of
    ;; the part and end points at the end of the part.  next points at
    ;; the beginning of the next part.
    (setq beg (point-min))
    (while (search-forward boundary nil t)
      (setq end (match-beginning 0))
      ;; If this is the last boundary according to RFC 2046, hide the
      ;; epilogue, else hide the boundary only.  Use a marker for
      ;; `next' because `pmail-mime-show' may change the buffer.
      (cond ((looking-at "--[ \t]*\n")
	     (setq next (point-max-marker)))
	    ((looking-at "[ \t]*\n")
	     (setq next (copy-marker (match-end 0))))
	    (t
	     (pmail-mm-get-boundary-error-message
	      "Malformed boundary" content-type content-disposition
	      content-transfer-encoding)))
      (delete-region end next)
      ;; Handle the part.
      (save-match-data
	(save-excursion
	  (save-restriction
	    (narrow-to-region beg end)
	    (pmail-mime-show))))
      (setq beg next)
      (goto-char beg))))

(defun test-pmail-mime-multipart-handler ()
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
    (pmail-mime-show t)))

;;; Main code

(defun pmail-mime-handle (content-type
			  content-disposition
			  content-transfer-encoding)
  "Handle the current buffer as a MIME part.
The current buffer should be narrowed to the respective body, and
point should be at the beginning of the body.

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
  ;; Handle the content transfer encodings we know.  Unknown transfer
  ;; encodings will be passed on to the various handlers.
  (cond ((string= content-transfer-encoding "base64")
	 (when (ignore-errors
		 (base64-decode-region (point) (point-max)))
	   (setq content-transfer-encoding nil)))
	((string= content-transfer-encoding "quoted-printable")
	 (quoted-printable-decode-region (point) (point-max))
	 (setq content-transfer-encoding nil))
	((string= content-transfer-encoding "8bit")
	 ;; FIXME: Is this the correct way?
	 (set-buffer-multibyte nil)))
  ;; Inline stuff requires work.  Attachments are handled by the bulk
  ;; handler.
  (if (string= "inline" (car content-disposition))
      (let ((stop nil))
	(dolist (entry pmail-mime-media-type-handlers-alist)
	  (when (and (string-match (car entry) (car content-type)) (not stop))
	    (progn
	      (setq stop (funcall (cadr entry) content-type
				  content-disposition
				  content-transfer-encoding))))))
    ;; Everything else is an attachment.
    (pmail-mime-bulk-handler content-type
		       content-disposition
		       content-transfer-encoding)))

(defun pmail-mime-show (&optional show-headers)
  "Handle the current buffer as a MIME message.
If SHOW-HEADERS is non-nil, then the headers of the current part
will shown as usual for a MIME message.  The headers are also
shown for the content type message/rfc822.  This function will be
called recursively if multiple parts are available.

The current buffer must contain a single message.  It will be
modified."
  (let ((end (point-min))
	content-type
	content-transfer-encoding
	content-disposition)
    ;; `point-min' returns the beginning and `end' points at the end
    ;; of the headers.  We're not using `pmail-header-get-header'
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
    ;; Unrecognized disposition types are to be treated like
    ;; attachment according to RFC 2183.
    (unless (member (car content-disposition) '("inline" "attachment"))
      (setq content-disposition '("attachment")))
    ;; Hide headers and handle the part.
    (save-restriction
      (cond ((string= (car content-type) "message/rfc822")
	     (pmail-header-hide-headers)
	     (narrow-to-region end (point-max)))
	    (show-headers
	     (pmail-header-hide-headers))
	    (t
	     (delete-region (point-min) end)))
      (pmail-mime-handle content-type content-disposition
			 content-transfer-encoding))))

(defun pmail-mime ()
  "Copy buffer contents to a temporary buffer and handle MIME.
This calls `pmail-mime-show' to do the real job."
  (interactive)
  (let ((data (with-current-buffer pmail-buffer
		(save-restriction
		  (widen)
		  (buffer-substring
		   (pmail-desc-get-start pmail-current-message)
		   (pmail-desc-get-end pmail-current-message)))))
	(buf (get-buffer-create "*PMAIL*")))
    (set-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert data)
      (pmail-mime-show t))
    (view-buffer buf)))

(defun pmail-mm-get-boundary-error-message (message type disposition encoding)
  "Return MESSAGE with more information on the main mime components."
  (error "%s; type: %s; disposition: %s; encoding: %s"
	 message type disposition encoding))

(provide 'pmailmm)

;; Local Variables:
;; change-log-default-name: "ChangeLog.pmail"
;; End:

;; arch-tag: 3f2c5e5d-1aef-4512-bc20-fd737c9d5dd9
;;; pmailmm.el ends here
