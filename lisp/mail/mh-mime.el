;;; mh-mime.el --- mh-e support for composing MIME messages

;; Copyright (C) 1993, 1995, 2001, 2002 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Internal support for mh-e package.
;; Support for generating an mhn composition file.
;; MIME is supported only by MH 6.8 or later.

;;; Change Log:

;; $Id: mh-mime.el,v 1.26 2002/04/07 19:20:56 wohler Exp $

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

(defun mh-have-file-command ()
  "Return t if 'file' command is on the system.
'file -i' is used to get MIME type of composition insertion."
  (when (not (boundp 'mh-have-file-command))
    (load "executable" t t)     ; executable-find not autoloaded in emacs20
    (setq mh-have-file-command
          (and (fboundp 'executable-find)
               (executable-find "file") ; file command exists
                                        ;   and accepts -i and -b args.
               (zerop (call-process "file" nil nil nil "-i" "-b"
                                    (expand-file-name "inc" mh-progs))))))
  mh-have-file-command)

(defun mh-file-mime-type (filename)
  "Return MIME type of FILENAME from file command.
Returns nil if file command not on system."
  (cond
   ((not (mh-have-file-command))
    nil)                                ;No file command, exit now.
   ((not (and (file-exists-p filename)(file-readable-p filename)))
    nil)
   (t
    (save-excursion
      (let ((tmp-buffer (get-buffer-create mh-temp-buffer)))
        (set-buffer tmp-buffer)
        (unwind-protect
            (progn
              (call-process "file" nil '(t nil) nil "-b" "-i"
                            (expand-file-name filename))
              (goto-char (point-min))
              (if (not (re-search-forward mh-media-type-regexp nil t))
                  nil
                (match-string 0)))
          (kill-buffer tmp-buffer)))))))

(defvar mh-mhn-compose-insert-p nil
  "Buffer-local variable to know whether MIME insertion was done.
Triggers an automatic call to `mh-edit-mhn' in `mh-send-letter'.")
(make-variable-buffer-local 'mh-mhn-compose-insert-p)

;;; This is needed for Emacs20 which doesn't have mailcap-mime-types.
(defvar mh-mime-content-types
  '(("application/mac-binhex40") ("application/msword")
    ("application/octet-stream") ("application/pdf") ("application/pgp-keys")
    ("application/pgp-signature") ("application/pkcs7-signature")
    ("application/postscript") ("application/rtf")
    ("application/vnd.ms-excel") ("application/vnd.ms-powerpoint")
    ("application/vnd.ms-project") ("application/vnd.ms-tnef")
    ("application/wordperfect5.1") ("application/wordperfect6.0")
    ("application/zip")

    ("audio/basic") ("audio/mpeg")

    ("image/gif") ("image/jpeg") ("image/png")

    ("message/delivery-status")
    ("message/external-body") ("message/partial") ("message/rfc822")

    ("text/enriched") ("text/html") ("text/plain") ("text/rfc822-headers")
    ("text/richtext") ("text/xml")

    ("video/mpeg") ("video/quicktime"))
  "Legal MIME content types.
See documentation for \\[mh-edit-mhn].")

(defvar mh-media-type-regexp
  (concat (regexp-opt '("text" "image" "audio" "video" "application"
                        "multipart" "message") t)
          "/[-.+a-zA-Z0-9]+")
  "Regexp matching valid media types used in MIME attachment compositions.")

(defun mh-mhn-compose-insertion (filename type description attributes)
  "Add a directive to insert a MIME message part from a file.
This is the typical way to insert non-text parts in a message. Arguments are
FILENAME, which tells where to find the file, TYPE, the MIME content type,
DESCRIPTION, a line of text for the Content-Description field. ATTRIBUTES is a
comma separated list of name=value pairs that is appended to the Content-Type
field of the attachment.
See also \\[mh-edit-mhn]."
  (interactive (let ((filename (read-file-name "Insert contents of: ")))
		 (list
		  filename
                  (or (mh-file-mime-type filename)
		      (completing-read "Content-Type: "
				       (if (and (require 'mailcap nil t)
						(fboundp 'mailcap-mime-types))
					   (mapcar 'list (mailcap-mime-types))
					 mh-mime-content-types)))
		  (read-string "Content-Description: ")
		  (read-string "Content-Attributes: "
			       (concat "name=\""
				       (file-name-nondirectory filename)
				       "\"")))))
  (mh-mhn-compose-type filename type description attributes ))

(defun mh-mhn-compose-type (filename type
			    &optional description attributes comment)
  (setq mh-mhn-compose-insert-p t)
  (beginning-of-line)
  (insert "#" type)
  (and attributes
       (insert "; " attributes))
  (and comment
       (insert " (" comment ")"))
  (insert " [")
  (and description
       (insert description))
  (insert "] " (expand-file-name filename))
  (insert "\n"))


(defun mh-mhn-compose-anon-ftp (host filename type description)
  "Add a directive for a MIME anonymous ftp external body part.
This directive tells MH to include a reference to a
message/external-body part retrievable by anonymous FTP.  Arguments
are HOST and FILENAME, which tell where to find the file, TYPE, the
MIME content type, and DESCRIPTION, a line of text for the
Content-description header.  See also \\[mh-edit-mhn]."
  (interactive (list
		(read-string "Remote host: ")
		(read-string "Remote filename: ")
		(completing-read "External Content-Type: "
				 (if (and (require 'mailcap nil t)
					  (fboundp 'mailcap-mime-types))
				     (mapcar 'list (mailcap-mime-types))
				   mh-mime-content-types))
		(read-string "External Content-Description: ")))
  (mh-mhn-compose-external-type "anon-ftp" host filename
				type description))

(defun mh-mhn-compose-external-compressed-tar (host filename description)
  "Add a directive to include a MIME reference to a compressed tar file.
The file should be available via anonymous ftp.  This directive
tells MH to include a reference to a message/external-body part.
Arguments are HOST and FILENAME, which tell where to find the file, and
DESCRIPTION, a line of text for the Content-description header.
See also \\[mh-edit-mhn]."
  (interactive (list
		(read-string "Remote host: ")
		(read-string "Remote filename: ")
		(read-string "Tar file Content-description: ")))
  (mh-mhn-compose-external-type "anon-ftp" host filename
				"application/octet-stream"
				description
				"type=tar; conversions=x-compress"
				"mode=image"))


(defun mh-mhn-compose-external-type (access-type host filename type
				     &optional description
				     attributes extra-params comment)
  (setq mh-mhn-compose-insert-p t)
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
  (insert "; name=" (file-name-nondirectory filename))
  (insert "; directory=\"" (file-name-directory filename) "\"")
  (and extra-params
       (insert "; " extra-params))
  (insert "\n"))

(defun mh-mhn-compose-forw (&optional description folder messages)
  "Add a forw directive to this message, to forward a message with MIME.
This directive tells MH to include the named messages in this one.
Arguments are DESCRIPTION, a line of text for the Content-description header,
and FOLDER and MESSAGES, which name the message(s) to be forwarded.
See also \\[mh-edit-mhn]."
  (interactive (list
		(read-string "Forw Content-description: ")
		(mh-prompt-for-folder "Message from" mh-sent-from-folder nil)
		(read-string (format "Messages%s: "
				     (if mh-sent-from-msg
					 (format " [%d]" mh-sent-from-msg)
				       "")))))
  (setq mh-mhn-compose-insert-p t)
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

Process the current draft with the mhn program, which, using directives
already inserted in the draft, fills in all the MIME components and header
fields.

This step should be done last just before sending the message.

The `\\[mh-revert-mhn-edit]' command undoes this command. The arguments in the
list `mh-mhn-args' are passed to mhn if this function is passed an optional
prefix argument EXTRA-ARGS.

For assistance with creating mhn directives to insert various types of
components in a message, see \\[mh-mhn-compose-insertion] (generic insertion
from a file), \\[mh-mhn-compose-anon-ftp] (external reference to file via
anonymous ftp), \\[mh-mhn-compose-external-compressed-tar] \ \(reference to
compressed tar file via anonymous ftp), and \\[mh-mhn-compose-forw] (forward
message). If these helper functions are used, `mh-edit-mhn' is run
automatically when the draft is sent.

The mhn program is part of MH version 6.8 or later."
  (interactive "*P")
  (save-buffer)
  (message "mhn editing...")
  (cond
   (mh-nmh-p
    (mh-exec-cmd-error nil
                       "mhbuild" (if extra-args mh-mhn-args) buffer-file-name))
   (t
    (mh-exec-cmd-error (format "mhdraft=%s" buffer-file-name)
                       "mhn" (if extra-args mh-mhn-args) buffer-file-name)))
  (setq mh-mhn-compose-insert-p nil)
  (revert-buffer t t)
  (message "mhn editing...done")
  (run-hooks 'mh-edit-mhn-hook))


(defun mh-revert-mhn-edit (noconfirm)
  "Undo the effect of \\[mh-edit-mhn] by reverting to the backup file.
Optional non-nil argument NOCONFIRM means don't ask for confirmation."
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
	(error "Backup file for %s no longer exists!" buffer-file-name))
    (or noconfirm
	(yes-or-no-p (format "Revert buffer from file %s? "
			     backup-file))
	(error "Revert not confirmed"))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert-file-contents backup-file))
    (after-find-file nil)))

;;; mh-mime.el ends here
