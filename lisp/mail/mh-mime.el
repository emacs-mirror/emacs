;;; mh-mime --- mh-e support for composing MIME messages
;; Time-stamp: <94/11/18 17:48:19 gildea>

;; Copyright (C) 1993, 1995 Free Software Foundation, Inc.

;; This file is part of mh-e.

;; mh-e is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; mh-e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mh-e; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Internal support for mh-e package.
;;; Support for generating an mhn composition file.
;;; MIME is supported only by MH 6.8 or later.

;;; Change Log:

;; $Id: mh-mime.el,v 1.5 94/12/27 22:38:10 gildea Exp $

;;; Code:

(provide 'mh-mime)
(require 'mh-comp)


;; To do:
;; paragraph code should not fill # lines if MIME enabled.
;; implement mh-auto-edit-mhn (if non-nil, \\[mh-send-letter]
;;	invokes mh-edit-mhn automatically before sending.)
;;      actually, instead of mh-auto-edit-mhn,
;;      should read automhnproc from profile
;; MIME option to mh-forward
;; command to move to content-description insertion point

(defvar mh-mhn-args nil
  "Extra arguments to have \\[mh-edit-mhn] pass to the \"mhn\" command.
The arguments are passed to mhn if \\[mh-edit-mhn] is given a
prefix argument.  Normally default arguments to mhn are specified in the
MH profile.")

(defvar mh-edit-mhn-hook nil
  "Invoked on the formatted letter by \\<mh-letter-mode-map>\\[mh-edit-mhn].")

(defvar mh-mime-content-types
  '(("text/plain") ("text/richtext")
    ("multipart/mixed") ("multipart/alternative") ("multipart/digest")
    ("multipart/parallel")
    ("message/rfc822") ("message/partial") ("message/external-body")
    ("application/octet-stream") ("application/postscript")
    ("image/jpeg") ("image/gif")
    ("audio/basic")
    ("video/mpeg"))
  "Legal MIME content types.")

(defun mh-mhn-compose-insertion (pathname type description)
  "Add a directive to insert a message part from a file.
This is the typical way to insert non-text parts in a message.
Arguments are PATHNAME, which tells where to find the file, TYPE, the
MIME content type, and DESCRIPTION, a line of text for the
Content-description header.  See also \\[mh-edit-mhn]."
  (interactive (list
		(read-file-name "Insert contents of: ")
		(completing-read "Content-type: "
				 mh-mime-content-types nil nil nil)
		(read-string "Content-description: ")))
  (mh-mhn-compose-type pathname type description))

(defun mh-mhn-compose-type (pathname type
			    &optional description attributes comment)
  (beginning-of-line)
  (insert "#" type)
  (and attributes
       (insert "; " attributes))
  (and comment
       (insert " (" comment ")"))
  (insert " [")
  (and description
       (insert description))
  (insert "] " (expand-file-name pathname))
  (insert "\n"))


(defun mh-mhn-compose-anon-ftp (host pathname type description)
  "Add a directive for an anonymous ftp external body part.
This directive tells MH to include a reference to a
message/external-body part retrievable by anonymous FTP.  Arguments
are HOST and PATHNAME, which tell where to find the file, TYPE, the
MIME content type, and DESCRIPTION, a line of text for the
Content-description header.  See also \\[mh-edit-mhn]."
  (interactive (list
		(read-string "Remote host: ")
		(read-string "Remote pathname: ")
		(completing-read "External Content-type: "
				 mh-mime-content-types nil nil nil)
		(read-string "External Content-description: ")))
  (mh-mhn-compose-external-type "anon-ftp" host pathname
				type description))

(defun mh-mhn-compose-external-compressed-tar (host pathname description)
  "Add a directive to include a reference to a compressed tar file.
The file should be available via anonymous ftp.  This directive
tells MH to include a reference to a message/external-body part.
Arguments are HOST and PATHNAME, which tell where to find the file, and
DESCRIPTION, a line of text for the Content-description header.
See also \\[mh-edit-mhn]."
  (interactive (list
		(read-string "Remote host: ")
		(read-string "Remote pathname: ")
		(read-string "Tar file Content-description: ")))
  (mh-mhn-compose-external-type "anon-ftp" host pathname
				"application/octet-stream"
				description
				"type=tar; conversions=x-compress"
				"mode=image"))


(defun mh-mhn-compose-external-type (access-type host pathname type
				     &optional description
				     attributes extra-params comment)
  (beginning-of-line)
  (insert "#@" type)
  (and attributes
       (insert "; " attributes))
  (and comment
       (insert " (" comment ") "))
  (insert " [")
  (and description
       (insert description))
  (insert "] ")
  (insert "access-type=" access-type "; ")
  (insert "site=" host)
  (insert "; name=" (file-name-nondirectory pathname))
  (insert "; directory=\"" (file-name-directory pathname) "\"")
  (and extra-params
       (insert "; " extra-params))
  (insert "\n"))

(defun mh-mhn-compose-forw (&optional description folder messages)
  "Add a forw directive to this message.
This directive tells MH to include the named messages in this one.
Arguments are DESCRIPTION, a line of text for the Content-description header,
FOLDER and MESSAGES, which name the message(s) to be forwarded.
See also \\[mh-edit-mhn]."
  (interactive (list
		(read-string "Forw Content-description: ")
		(mh-prompt-for-folder "Message from" mh-sent-from-folder nil)
		(read-string (format "Messages%s: "
				     (if mh-sent-from-msg
					 (format " [%d]" mh-sent-from-msg)
				       "")))))
  (beginning-of-line)
  (insert "#forw [")
  (and description
       (not (string= description ""))
       (insert description))
  (insert "]")
  (and folder
       (not (string= folder ""))
       (insert " " folder))
  (if (and messages
	   (not (string= messages "")))
      (let ((start (point)))
	(insert " " messages)
	(subst-char-in-region start (point) ?, ? ))
    (if mh-sent-from-msg
	(insert " " (int-to-string mh-sent-from-msg))))
  (insert "\n"))

(defun mh-edit-mhn (&optional extra-args)
  "Format the current draft for MIME, expanding any mhn directives.
Process the current draft with the mhn program, which,
using directives already inserted in the draft, fills in
all the MIME components and header fields.
This step should be done last just before sending the message.
The mhn program is part of MH version 6.8 or later.
The `\\[mh-revert-mhn-edit]' command undoes this command.
The arguments in the list `mh-mhn-args' are passed to mhn
if this function is passed an argument.

For assistance with creating mhn directives to insert
various types of components in a message, see
\\[mh-mhn-compose-insertion] (generic insertion from a file),
\\[mh-mhn-compose-anon-ftp] (external reference to file via anonymous ftp),
\\[mh-mhn-compose-external-compressed-tar] \
\(reference to compressed tar file via anonymous ftp), and
\\[mh-mhn-compose-forw] (forward message)."
  (interactive "*P")
  (save-buffer)
  (message "mhn editing...")
  (mh-exec-cmd-error (format "mhdraft=%s" buffer-file-name)
		     "mhn" (if extra-args mh-mhn-args) buffer-file-name)
  (revert-buffer t t)
  (message "mhn editing...done")
  (run-hooks 'mh-edit-mhn-hook))


(defun mh-revert-mhn-edit (noconfirm)
  "Undoes the effect of \\[mh-edit-mhn] by reverting to the backup file.
Optional non-nil argument means don't ask for confirmation."
  (interactive "*P")
  (if (null buffer-file-name)
      (error "Buffer does not seem to be associated with any file"))
  (let ((backup-strings '("," "#"))
	backup-file)
    (while (and backup-strings
		(not (file-exists-p
		      (setq backup-file
			    (concat (file-name-directory buffer-file-name)
				    (car backup-strings)
				    (file-name-nondirectory buffer-file-name)
				    ".orig")))))
      (setq backup-strings (cdr backup-strings)))
    (or backup-strings
	(error "mhn backup file for %s no longer exists!" buffer-file-name))
    (or noconfirm
	(yes-or-no-p (format "Revert buffer from file %s? "
			     backup-file))
	(error "mhn edit revert not confirmed."))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert-file-contents backup-file))
    (after-find-file nil)))
