;;; rmailmm.el --- MIME decoding and display stuff for RMAIL

;; Copyright (C) 2006, 2007, 2008, 2009, 2010  Free Software Foundation, Inc.

;; Author: Alexander Pohoyda
;;	Alex Schroeder
;; Maintainer: FSF
;; Keywords: mail
;; Package: rmail

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

;; This file provides two operation modes for viewing a MIME message.

;; (1) When rmail-enable-mime is non-nil (now it is the default), the
;; function `rmail-show-mime' is automatically called.  That function
;; shows a MIME message directly in RMAIL's view buffer.

;; (2) When rmail-enable-mime is nil, the command 'v' (or M-x
;; rmail-mime) shows a MIME message in a new buffer "*RMAIL*".

;; Both operations share the intermediate functions rmail-mime-process
;; and rmail-mime-process-multipart as below.

;; rmail-show-mime
;;   +- rmail-mime-parse
;;   |    +- rmail-mime-process <--+------------+
;;   |         |         +---------+            |
;;   |         + rmail-mime-process-multipart --+
;;   |
;;   + rmail-mime-insert <----------------+
;;       +- rmail-mime-insert-text        |
;;       +- rmail-mime-insert-bulk        |
;;       +- rmail-mime-insert-multipart --+
;;
;; rmail-mime
;;  +- rmail-mime-show <----------------------------------+
;;       +- rmail-mime-process                            | 
;;            +- rmail-mime-handle                        |
;;                 +- rmail-mime-text-handler             |
;;                 +- rmail-mime-bulk-handler             |
;;                 |    + rmail-mime-insert-bulk
;;                 +- rmail-mime-multipart-handler        |
;;                      +- rmail-mime-process-multipart --+

;; In addition, for the case of rmail-enable-mime being non-nil, this
;; file provides two functions rmail-insert-mime-forwarded-message and
;; rmail-insert-mime-resent-message for composing forwarded and resent
;; messages respectively.

;; Todo:

;; Make rmail-mime-media-type-handlers-alist usable in the first
;; operation mode.
;; Handle multipart/alternative in the second operation mode.
;; Offer the option to call external/internal viewers (doc-view, xpdf, etc).

;;; Code:

(require 'rmail)
(require 'mail-parse)
(require 'message)

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

;;; MIME-entity object

(defun rmail-mime-entity (type disposition transfer-encoding
			       header body children)
  "Retrun a newly created MIME-entity object.

A MIME-entity is a vector of 6 elements:

  [ TYPE DISPOSITION TRANSFER-ENCODING HEADER BODY CHILDREN ]
  
TYPE and DISPOSITION correspond to MIME headers Content-Type: and
Cotent-Disposition: respectively, and has this format:

  \(VALUE (ATTRIBUTE . VALUE) (ATTRIBUTE . VALUE) ...)

VALUE is a string and ATTRIBUTE is a symbol.

Consider the following header, for example:

Content-Type: multipart/mixed;
	boundary=\"----=_NextPart_000_0104_01C617E4.BDEC4C40\"

The corresponding TYPE argument must be:

\(\"multipart/mixed\"
  \(\"boundary\" . \"----=_NextPart_000_0104_01C617E4.BDEC4C40\"))

TRANSFER-ENCODING corresponds to MIME header
Content-Transfer-Encoding, and is a lowercased string.

HEADER and BODY are a cons (BEG . END), where BEG and END specify
the region of the corresponding part in RMAIL's data (mbox)
buffer.  BODY may be nil.  In that case, the current buffer is
narrowed to the body part.

CHILDREN is a list of MIME-entities for a \"multipart\" entity, and
nil for the other types."
  (vector type disposition transfer-encoding header body children))

;; Accessors for a MIME-entity object.
(defsubst rmail-mime-entity-type (entity) (aref entity 0))
(defsubst rmail-mime-entity-disposition (entity) (aref entity 1))
(defsubst rmail-mime-entity-transfer-encoding (entity) (aref entity 2))
(defsubst rmail-mime-entity-header (entity) (aref entity 3))
(defsubst rmail-mime-entity-body (entity) (aref entity 4))
(defsubst rmail-mime-entity-children (entity) (aref entity 5))

;;; Buttons

(defun rmail-mime-save (button)
  "Save the attachment using info in the BUTTON."
  (let* ((filename (button-get button 'filename))
	 (directory (button-get button 'directory))
	 (data (button-get button 'data))
	 (mbox-buf rmail-view-buffer)
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
      (setq buffer-undo-list t)
      (if (stringp data)
	  (insert data)
	;; DATA is a MIME-entity object.
	(let ((transfer-encoding (rmail-mime-entity-transfer-encoding data))
	      (body (rmail-mime-entity-body data)))
	  (insert-buffer-substring mbox-buf (car body) (cdr body))
	  (cond ((string= transfer-encoding "base64")
		 (ignore-errors (base64-decode-region (point-min) (point-max))))
		((string= transfer-encoding "quoted-printable")
		 (quoted-printable-decode-region (point-min) (point-max))))))
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

(defun rmail-mime-insert-text (entity)
  "Insert MIME-entity ENTITY as a plain text MIME part in the current buffer."
  (let* ((content-type (rmail-mime-entity-type entity))
	 (charset (cdr (assq 'charset (cdr content-type))))
	 (coding-system (if charset (intern (downcase charset))))
	 (transfer-encoding (rmail-mime-entity-transfer-encoding entity))
	 (body (rmail-mime-entity-body entity)))
    (save-restriction
      (narrow-to-region (point) (point))
      (insert-buffer-substring rmail-buffer (car body) (cdr body))
      (cond ((string= transfer-encoding "base64")
	     (ignore-errors (base64-decode-region (point-min) (point-max))))
	    ((string= transfer-encoding "quoted-printable")
	     (quoted-printable-decode-region (point-min) (point-max))))
      (if (coding-system-p coding-system)
	  (decode-coding-region (point-min) (point-max) coding-system)))))

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
  "Insert an image of type TYPE, where DATA is the image data.
If DATA is not a string, it is a MIME-entity object."
  (end-of-line)
  (let ((modified (buffer-modified-p)))
    (insert ?\n)
    (unless (stringp data)
      ;; DATA is a MIME-entity.
      (let ((transfer-encoding (rmail-mime-entity-transfer-encoding data))
	    (body (rmail-mime-entity-body data))
	    (mbox-buffer rmail-view-buffer))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (setq buffer-undo-list t)
	  (insert-buffer-substring mbox-buffer (car body) (cdr body))
	  (cond ((string= transfer-encoding "base64")
		 (ignore-errors (base64-decode-region (point-min) (point-max))))
		((string= transfer-encoding "quoted-printable")
		 (quoted-printable-decode-region (point-min) (point-max))))
	  (setq data
		(buffer-substring-no-properties (point-min) (point-max))))))
    (insert-image (create-image data type t))
    (set-buffer-modified-p modified)))

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
  (rmail-mime-insert-bulk
   (rmail-mime-entity content-type content-disposition content-transfer-encoding
		      nil nil nil)))

(defun rmail-mime-insert-bulk (entity)
  "Inesrt a MIME-entity ENTITY as an attachment.
The optional second arg DATA, if non-nil, is a string containing
the attachment data that is already decoded."
  ;; Find the default directory for this media type.
  (let* ((content-type (rmail-mime-entity-type entity))
	 (content-disposition (rmail-mime-entity-disposition entity))
	 (body (rmail-mime-entity-body entity))
	 (directory (catch 'directory
		      (dolist (entry rmail-mime-attachment-dirs-alist)
			(when (string-match (car entry) (car content-type))
			  (dolist (dir (cdr entry))
			    (when (file-directory-p dir)
			      (throw 'directory dir)))))))
	 (filename (or (cdr (assq 'name (cdr content-type)))
		       (cdr (assq 'filename (cdr content-disposition)))
		       "noname"))
	 (label (format "\nAttached %s file: " (car content-type)))
	 (units '(B kB MB GB))
	 data udata size osize type)
    (if body
	(setq data entity
	      udata entity
	      size (- (cdr body) (car body)))
      (setq data (buffer-string)
	    udata (string-as-unibyte data)
	    size (length udata))
      (delete-region (point-min) (point-max)))
    (setq osize size)
    (while (and (> size 1024.0) ; cribbed from gnus-agent-expire-done-message
		(cdr units))
      (setq size (/ size 1024.0)
	    units (cdr units)))
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
  (rmail-mime-process-multipart
   content-type content-disposition content-transfer-encoding nil))

(defun rmail-mime-process-multipart (content-type
				     content-disposition
				     content-transfer-encoding
				     parse-only)
  "Process the current buffer as a multipart MIME body.

If PARSE-ONLY is nil, modify the current buffer directly for showing
the MIME body and return nil.

Otherwise, just parse the current buffer and return a list of
MIME-entity objects.

The other arguments are the same as `rmail-mime-multipart-handler'."
  ;; Some MUAs start boundaries with "--", while it should start
  ;; with "CRLF--", as defined by RFC 2046:
  ;;    The boundary delimiter MUST occur at the beginning of a line,
  ;;    i.e., following a CRLF, and the initial CRLF is considered to
  ;;    be attached to the boundary delimiter line rather than part
  ;;    of the preceding part.
  ;; We currently don't handle that.
  (let ((boundary (cdr (assq 'boundary content-type)))
	beg end next entities)
    (unless boundary
      (rmail-mm-get-boundary-error-message
       "No boundary defined" content-type content-disposition
       content-transfer-encoding))
    (setq boundary (concat "\n--" boundary))
    ;; Hide the body before the first bodypart
    (goto-char (point-min))
    (when (and (search-forward boundary nil t)
	       (looking-at "[ \t]*\n"))
      (if parse-only
	  (narrow-to-region (match-end 0) (point-max))
	(delete-region (point-min) (match-end 0))))
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
      ;; Handle the part.
      (if parse-only
	  (save-restriction
	    (narrow-to-region beg end)
	    (setq entities (cons (rmail-mime-process nil t) entities)))
	(delete-region end next)
	(save-restriction
	  (narrow-to-region beg end)
	  (rmail-mime-show)))
      (goto-char (setq beg next)))
    (nreverse entities)))

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
  (rmail-mime-process show-headers nil))

(defun rmail-mime-process (show-headers parse-only)
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

    (if parse-only
	(cond ((string-match "multipart/.*" (car content-type))
	       (setq end (1- end))
	       (save-restriction
		 (let ((header (if show-headers (cons (point-min) end))))
		   (narrow-to-region end (point-max))
		   (rmail-mime-entity content-type
				      content-disposition
				      content-transfer-encoding
				      header nil
				      (rmail-mime-process-multipart
				       content-type content-disposition
				       content-transfer-encoding t)))))
	      ((string-match "message/rfc822" (car content-type))
	       (or show-headers
		   (narrow-to-region end (point-max)))
	       (rmail-mime-process t t))
	      (t
	       (rmail-mime-entity content-type
				  content-disposition
				  content-transfer-encoding
				  nil
				  (cons end (point-max))
				  nil)))
      ;; Hide headers and handle the part.
      (save-restriction
	(cond ((string= (car content-type) "message/rfc822")
	       (narrow-to-region end (point-max)))
	      ((not show-headers)
	       (delete-region (point-min) end)))
	(rmail-mime-handle content-type content-disposition
			   content-transfer-encoding)))))

(defun rmail-mime-insert-multipart (entity)
  "Insert MIME-entity ENTITY of multipart type in the current buffer."
  (let ((subtype (cadr (split-string (car (rmail-mime-entity-type entity))
				     "/")))
	(disposition (rmail-mime-entity-disposition entity))
	(header (rmail-mime-entity-header entity))
	(children (rmail-mime-entity-children entity)))
    (if header
	(let ((pos (point)))
	  (or (bolp)
	      (insert "\n"))
	  (insert-buffer-substring rmail-buffer (car header) (cdr header))
	  (rfc2047-decode-region pos (point))
	  (insert "\n")))
    (cond
     ((string= subtype "mixed")
      (dolist (child children)
	(rmail-mime-insert child '("text/plain") disposition)))
     ((string= subtype "digest")
      (dolist (child children)
	(rmail-mime-insert child '("message/rfc822") disposition)))
     ((string= subtype "alternative")
      (let (best-plain-text best-text)
	(dolist (child children)
	  (if (string= (or (car (rmail-mime-entity-disposition child))
			   (car disposition))
		       "inline")
	      (if (string-match "text/plain"
				(car (rmail-mime-entity-type child)))
		  (setq best-plain-text child)
		(if (string-match "text/.*"
				  (car (rmail-mime-entity-type child)))
		    (setq best-text child)))))
	(if (or best-plain-text best-text)
	    (rmail-mime-insert (or best-plain-text best-text))
	  ;; No child could be handled.  Insert all.
	  (dolist (child children)
	    (rmail-mime-insert child nil disposition)))))
     (t
      ;; Unsupported subtype.  Insert all of them.
      (dolist (child children)
	(rmail-mime-insert child))))))

(defun rmail-mime-parse ()
  "Parse the current Rmail message as a MIME message.
The value is a MIME-entiy object (see `rmail-mime-enty-new')."
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
	(rmail-mime-process nil t)
      (error nil))))

(defun rmail-mime-insert (entity &optional content-type disposition)
  "Insert a MIME-entity ENTITY in the current buffer.

This function will be called recursively if multiple parts are
available."
  (if (rmail-mime-entity-children entity)
      (rmail-mime-insert-multipart entity)
    (setq content-type
	  (or (rmail-mime-entity-type entity) content-type))
    (setq disposition
	  (or (rmail-mime-entity-disposition entity) disposition))
    (if (and (string= (car disposition) "inline")
	     (string-match "text/.*" (car content-type)))
	(rmail-mime-insert-text entity)
      (rmail-mime-insert-bulk entity))))

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

(defun rmail-show-mime ()
  "Function to set in `rmail-show-mime-function' (which see)."
  (let ((mbox-buf rmail-buffer)
	(entity (rmail-mime-parse)))
    (if entity
	(with-current-buffer rmail-view-buffer
	  (let ((inhibit-read-only t)
		(rmail-buffer mbox-buf))
	    (erase-buffer)
	    (rmail-mime-insert entity)))
      ;; Decoding failed.  Insert the original message body as is.
      (let ((region (with-current-buffer mbox-buf
		      (goto-char (point-min))
		      (re-search-forward "^$" nil t)
		      (forward-line 1)
		      (cons (point) (point-max)))))
	(with-current-buffer rmail-view-buffer
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (insert-buffer-substring mbox-buf (car region) (cdr region))))
	(message "MIME decoding failed")))))

(setq rmail-show-mime-function 'rmail-show-mime)

(defun rmail-insert-mime-forwarded-message (forward-buffer)
  "Function to set in `rmail-insert-mime-forwarded-message-function' (which see)."
  (let ((mbox-buf (with-current-buffer forward-buffer rmail-view-buffer)))
    (save-restriction
      (narrow-to-region (point) (point))
      (message-forward-make-body-mime mbox-buf))))

(setq rmail-insert-mime-forwarded-message-function
      'rmail-insert-mime-forwarded-message)

(defun rmail-insert-mime-resent-message (forward-buffer)
  "Function to set in `rmail-insert-mime-resent-message-function' (which see)."
  (insert-buffer-substring
   (with-current-buffer forward-buffer rmail-view-buffer))
  (goto-char (point-min))
  (when (looking-at "From ")
    (forward-line 1)
    (delete-region (point-min) (point))))

(setq rmail-insert-mime-resent-message-function
      'rmail-insert-mime-resent-message)

(defun rmail-search-mime-message (msg regexp)
  "Function to set in `rmail-search-mime-message-function' (which see)."
  (save-restriction
    (narrow-to-region (rmail-msgbeg msg) (rmail-msgend msg))
    (let ((mbox-buf (current-buffer))
	  (header-end (save-excursion
			(re-search-forward "^$" nil 'move) (point)))
	  (body-end (point-max))
	  (entity (rmail-mime-parse)))
      (or 
       ;; At first, just search the headers.
       (with-temp-buffer
	 (insert-buffer-substring mbox-buf nil header-end)
	 (rfc2047-decode-region (point-min) (point))
	 (goto-char (point-min))
	 (re-search-forward regexp nil t))
       ;; Next, search the body.
       (if (and entity
		(let* ((content-type (rmail-mime-entity-type entity))
		       (charset (cdr (assq 'charset (cdr content-type)))))
		  (or (not (string-match "text/.*" (car content-type))) 
		      (and charset
			   (not (string= (downcase charset) "us-ascii"))))))
	   ;; Search the decoded MIME message.
	   (with-temp-buffer
	     (let ((rmail-buffer mbox-buf))
	       (rmail-mime-insert entity))
	     (goto-char (point-min))
	     (re-search-forward regexp nil t))
	 ;; Search the body without decoding.
	 (goto-char header-end)
	 (re-search-forward regexp nil t))))))

(setq rmail-search-mime-message-function 'rmail-search-mime-message)

(provide 'rmailmm)

;; Local Variables:
;; generated-autoload-file: "rmail.el"
;; End:

;; arch-tag: 3f2c5e5d-1aef-4512-bc20-fd737c9d5dd9
;;; rmailmm.el ends here
