;;; rmailmm.el --- MIME decoding and display stuff for RMAIL

;; Copyright (C) 2006, 2007, 2008, 2009, 2010  Free Software Foundation, Inc.

;; Author: Alexander Pohoyda
;;	Alex Schroeder
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
;; extensions (mime-display.el and mime.el).
;; Call `M-x rmail-mime' when viewing an Rmail message.

;; Todo:

;; Handle multipart/alternative.
;; Offer the option to call external/internal viewers (doc-view, xpdf, etc).

;;; Code:

(require 'rmail)
(require 'mail-parse)

;;; User options.

(defgroup rmail-mime nil
  "Rmail MIME handling options."
  :prefix "rmail-mime-"
  :group 'rmail)

(defcustom rmail-mime-media-type-handlers-alist
  '(("multipart/.*" rmail-mime-multipart-handler)
    ("text/.*" rmail-mime-text-handler)
    ("text/\\(x-\\)?patch" rmail-mime-bulk-handler)
    ("\\(image\\|audio\\|video\\|application\\)/.*" rmail-mime-bulk-handler))
  "Functions to handle various content types.
This is an alist with elements of the form (REGEXP FUNCTION ...).
The first item is a regular expression matching a content-type.
The remaining elements are handler functions to run, in order of
decreasing preference.  These are called until one returns non-nil.
Note that this only applies to items with an inline Content-Disposition,
all others are handled by `rmail-mime-bulk-handler'."
  :type '(alist :key-type regexp :value-type (repeat function))
  :version "23.1"
  :group 'rmail-mime)

(defcustom rmail-mime-attachment-dirs-alist
  `(("text/.*" "~/Documents")
    ("image/.*" "~/Pictures")
    (".*" "~/Desktop" "~" ,temporary-file-directory))
  "Default directories to save attachments of various types into.
This is an alist with elements of the form (REGEXP DIR ...).
The first item is a regular expression matching a content-type.
The remaining elements are directories, in order of decreasing preference.
The first directory that exists is used."
  :type '(alist :key-type regexp :value-type (repeat directory))
  :version "23.1"
  :group 'rmail-mime)

(defcustom rmail-mime-show-images 'button
  "What to do with image attachments that Emacs is capable of displaying.
If nil, do nothing special.  If `button', add an extra button
that when pushed displays the image in the buffer.  If a number,
automatically show images if they are smaller than that size (in
bytes), otherwise add a display button.  Anything else means to
automatically display the image in the buffer."
  :type '(choice (const :tag "Add button to view image" button)
		 (const :tag "No special treatment" nil)
		 (number :tag "Show if smaller than certain size")
		 (other :tag "Always show" show))
  :version "23.2"
  :group 'rmail-mime)

;;; End of user options.


;;; Buttons

(defun rmail-mime-save (button)
  "Save the attachment using info in the BUTTON."
  (let* ((filename (button-get button 'filename))
	 (directory (button-get button 'directory))
	 (data (button-get button 'data))
	 (ofilename filename))
    (setq filename (expand-file-name
		    (read-file-name (format "Save as (default: %s): " filename)
				    directory
				    (expand-file-name filename directory))
		    directory))
    ;; If arg is just a directory, use the default file name, but in
    ;; that directory (copied from write-file).
    (if (file-directory-p filename)
	(setq filename (expand-file-name
			(file-name-nondirectory ofilename)
			(file-name-as-directory filename))))
    (with-temp-buffer
      (set-buffer-file-coding-system 'no-conversion)
      ;; Needed e.g. by jka-compr, so if the attachment is a compressed
      ;; file, the magic signature compares equal with the unibyte
      ;; signature string recorded in jka-compr-compression-info-list.
      (set-buffer-multibyte nil)
      (insert data)
      (write-region nil nil filename nil nil nil t))))

(define-button-type 'rmail-mime-save 'action 'rmail-mime-save)

;;; Handlers

(defun rmail-mime-text-handler (content-type
				content-disposition
				content-transfer-encoding)
  "Handle the current buffer as a plain text MIME part."
  (let* ((charset (cdr (assq 'charset (cdr content-type))))
	 (coding-system (when charset
			  (intern (downcase charset)))))
    (when (coding-system-p coding-system)
      (decode-coding-region (point-min) (point-max) coding-system))))

;; FIXME move to the test/ directory?
(defun test-rmail-mime-handler ()
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
    (rmail-mime-show t)
    (set-buffer-multibyte t)))


(defun rmail-mime-insert-image (type data)
  "Insert an image of type TYPE, where DATA is the image data."
  (end-of-line)
  (insert ?\n)
  (insert-image (create-image data type t)))

(defun rmail-mime-image (button)
  "Display the image associated with BUTTON."
  (let ((inhibit-read-only t))
    (rmail-mime-insert-image (button-get button 'image-type)
			     (button-get button 'image-data))))

(define-button-type 'rmail-mime-image 'action 'rmail-mime-image)


(defun rmail-mime-bulk-handler (content-type
				content-disposition
				content-transfer-encoding)
  "Handle the current buffer as an attachment to download.
For images that Emacs is capable of displaying, the behavior
depends upon the value of `rmail-mime-show-images'."
  ;; Find the default directory for this media type.
  (let* ((directory (catch 'directory
		      (dolist (entry rmail-mime-attachment-dirs-alist)
			(when (string-match (car entry) (car content-type))
			  (dolist (dir (cdr entry))
			    (when (file-directory-p dir)
			      (throw 'directory dir)))))))
	 (filename (or (cdr (assq 'name (cdr content-type)))
		       (cdr (assq 'filename (cdr content-disposition)))
		       "noname"))
	 (label (format "\nAttached %s file: " (car content-type)))
	 (data (buffer-string))
	 (udata (string-as-unibyte data))
	 (size (length udata))
	 (osize size)
	 (units '(B kB MB GB))
	 type)
    (while (and (> size 1024.0)	; cribbed from gnus-agent-expire-done-message
		(cdr units))
      (setq size (/ size 1024.0)
	    units (cdr units)))
    (delete-region (point-min) (point-max))
    (insert label)
    (insert-button filename
		   :type 'rmail-mime-save
		   'help-echo "mouse-2, RET: Save attachment"
		   'filename filename
		   'directory (file-name-as-directory directory)
		   'data data)
    (insert (format " (%.0f%s)" size (car units)))
    (when (and rmail-mime-show-images
	       (string-match "image/\\(.*\\)" (setq type (car content-type)))
	       (setq type (concat "." (match-string 1 type))
		     type (image-type-from-file-name type))
	       (memq type image-types)
	       (image-type-available-p type))
      (insert " ")
      (cond ((or (eq rmail-mime-show-images 'button)
		 (and (numberp rmail-mime-show-images)
		      (>= osize rmail-mime-show-images)))
	     (insert-button "Display"
			    :type 'rmail-mime-image
			    'help-echo "mouse-2, RET: Show image"
			    'image-type type
			    'image-data udata))
	    (t
	     (rmail-mime-insert-image type udata))))))

(defun test-rmail-mime-bulk-handler ()
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
    (rmail-mime-show)))

(defun rmail-mime-multipart-handler (content-type
				     content-disposition
				     content-transfer-encoding)
  "Handle the current buffer as a multipart MIME body.
The current buffer should be narrowed to the body.  CONTENT-TYPE,
CONTENT-DISPOSITION, and CONTENT-TRANSFER-ENCODING are the values
of the respective parsed headers.  See `rmail-mime-handle' for their
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
      (rmail-mm-get-boundary-error-message
       "No boundary defined" content-type content-disposition
       content-transfer-encoding))
    (setq boundary (concat "\n--" boundary))
    ;; Hide the body before the first bodypart
    (goto-char (point-min))
    (when (and (search-forward boundary nil t)
	       (looking-at "[ \t]*\n"))
      (delete-region (point-min) (match-end 0)))
    ;; Loop over all body parts, where beg points at the beginning of
    ;; the part and end points at the end of the part.  next points at
    ;; the beginning of the next part.
    (setq beg (point-min))
    (while (search-forward boundary nil t)
      (setq end (match-beginning 0))
      ;; If this is the last boundary according to RFC 2046, hide the
      ;; epilogue, else hide the boundary only.  Use a marker for
      ;; `next' because `rmail-mime-show' may change the buffer.
      (cond ((looking-at "--[ \t]*$")
	     (setq next (point-max-marker)))
	    ((looking-at "[ \t]*\n")
	     (setq next (copy-marker (match-end 0) t)))
	    (t
	     (rmail-mm-get-boundary-error-message
	      "Malformed boundary" content-type content-disposition
	      content-transfer-encoding)))
      (delete-region end next)
      ;; Handle the part.
      (save-restriction
	(narrow-to-region beg end)
	(rmail-mime-show))
      (goto-char (setq beg next)))))


(defun test-rmail-mime-multipart-handler ()
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
    (rmail-mime-show t)))

;;; Main code

(defun rmail-mime-handle (content-type
			  content-disposition
			  content-transfer-encoding)
  "Handle the current buffer as a MIME part.
The current buffer should be narrowed to the respective body, and
point should be at the beginning of the body.

CONTENT-TYPE, CONTENT-DISPOSITION, and CONTENT-TRANSFER-ENCODING
are the values of the respective parsed headers.  The latter should
be downcased.  The parsed headers for CONTENT-TYPE and CONTENT-DISPOSITION
have the form

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
         ;; No, of course not, it just means there's no decoding to do.
	 ;; (set-buffer-multibyte nil)
         (setq content-transfer-encoding nil)
         ))
  ;; Inline stuff requires work.  Attachments are handled by the bulk
  ;; handler.
  (if (string= "inline" (car content-disposition))
      (let ((stop nil))
	(dolist (entry rmail-mime-media-type-handlers-alist)
	  (when (and (string-match (car entry) (car content-type)) (not stop))
	    (progn
	      (setq stop (funcall (cadr entry) content-type
				  content-disposition
				  content-transfer-encoding))))))
    ;; Everything else is an attachment.
    (rmail-mime-bulk-handler content-type
		       content-disposition
		       content-transfer-encoding)))

(defun rmail-mime-show (&optional show-headers)
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
    ;; of the headers.
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
    ;; Per RFC 2045, C-T-E is case insensitive (bug#5070), but the others
    ;; are not completely so.  Hopefully mail-header-parse-* DTRT.
    (if content-transfer-encoding
	(setq content-transfer-encoding (downcase content-transfer-encoding)))
    (setq content-type
	  (if content-type
	      (mail-header-parse-content-type content-type)
	    ;; FIXME: Default "message/rfc822" in a "multipart/digest"
	    ;; according to RFC 2046.
	    '("text/plain")))
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
	     (narrow-to-region end (point-max)))
	    ((not show-headers)
	     (delete-region (point-min) end)))
      (rmail-mime-handle content-type content-disposition
			 content-transfer-encoding))))

(define-derived-mode rmail-mime-mode fundamental-mode "RMIME"
  "Major mode used in `rmail-mime' buffers."
  (setq font-lock-defaults '(rmail-font-lock-keywords t t nil nil)))

;;;###autoload
(defun rmail-mime ()
  "Process the current Rmail message as a MIME message.
This creates a temporary \"*RMAIL*\" buffer holding a decoded
copy of the message.  Inline content-types are handled according to
`rmail-mime-media-type-handlers-alist'.  By default, this
displays text and multipart messages, and offers to download
attachments as specfied by `rmail-mime-attachment-dirs-alist'."
  (interactive)
  (let ((data (rmail-apply-in-message rmail-current-message 'buffer-string))
	(buf (get-buffer-create "*RMAIL*")))
    (set-buffer buf)
    (setq buffer-undo-list t)
    (let ((inhibit-read-only t))
      ;; Decoding the message in fundamental mode for speed, only
      ;; switching to rmail-mime-mode at the end for display.  Eg
      ;; quoted-printable-decode-region gets very slow otherwise (Bug#4993).
      (fundamental-mode)
      (erase-buffer)
      (insert data)
      (rmail-mime-show t)
      (rmail-mime-mode)
      (set-buffer-modified-p nil))
    (view-buffer buf)))

(defun rmail-mm-get-boundary-error-message (message type disposition encoding)
  "Return MESSAGE with more information on the main mime components."
  (error "%s; type: %s; disposition: %s; encoding: %s"
	 message type disposition encoding))

(provide 'rmailmm)

;; Local Variables:
;; generated-autoload-file: "rmail.el"
;; End:

;; arch-tag: 3f2c5e5d-1aef-4512-bc20-fd737c9d5dd9
;;; rmailmm.el ends here
