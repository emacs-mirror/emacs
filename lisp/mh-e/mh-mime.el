;;; mh-mime.el --- MH-E support for composing MIME messages

;; Copyright (C) 1993, 1995,
;; 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Internal support for MH-E package.
;; Support for generating an mhn composition file.
;; MIME is supported only by MH 6.8 or later.

;;; Change Log:

;;; Code:

(eval-when-compile (require 'mh-acros))
(mh-require-cl)
(require 'mh-comp)
(require 'gnus-util)
(require 'mh-gnus)

(autoload 'gnus-article-goto-header "gnus-art")
(autoload 'article-emphasize "gnus-art")
(autoload 'gnus-get-buffer-create "gnus")
(autoload 'gnus-eval-format "gnus-spec")
(autoload 'widget-convert-button "wid-edit")
(autoload 'message-options-set-recipient "message")
(autoload 'mml-unsecure-message "mml-sec")
(autoload 'mml-minibuffer-read-file "mml")
(autoload 'mml-minibuffer-read-description "mml")
(autoload 'mml-insert-empty-tag "mml")
(autoload 'mml-to-mime "mml")
(autoload 'mml-attach-file "mml")
(autoload 'rfc2047-decode-region "rfc2047")

;;;###mh-autoload
(defun mh-compose-insertion (&optional inline)
  "Add a directive to insert a MIME part from a file, using mhn or gnus.
If the variable `mh-compose-insertion' is set to 'mhn, then that will be used.
If it is set to 'gnus, then that will be used instead.
Optional argument INLINE means make it an inline attachment."
  (interactive "P")
  (if (equal mh-compose-insertion 'gnus)
      (if inline
          (mh-mml-attach-file "inline")
        (mh-mml-attach-file))
    (call-interactively 'mh-mhn-compose-insertion)))

;;;###mh-autoload
(defun mh-compose-forward (&optional description folder message)
  "Add a MIME directive to forward a message, using mhn or gnus.
If the variable `mh-compose-insertion' is set to 'mhn, then that will be used.
If it is set to 'gnus, then that will be used instead.
Optional argument DESCRIPTION is a description of the attachment.
Optional argument FOLDER is the folder from which the forwarded message should
come.
Optional argument MESSAGE is the message to forward.
If any of the optional arguments are absent, they are prompted for."
  (interactive (list
                (read-string "Forw Content-description: ")
                (mh-prompt-for-folder "Message from" mh-sent-from-folder nil)
                (read-string (format "Messages%s: "
                                     (if (numberp mh-sent-from-msg)
                                         (format " [%d]" mh-sent-from-msg)
                                       "")))))
  (if (equal mh-compose-insertion 'gnus)
      (mh-mml-forward-message description folder message)
    (mh-mhn-compose-forw description folder message)))

;; To do:
;; paragraph code should not fill # lines if MIME enabled.
;; implement mh-auto-edit-mhn (if non-nil, \\[mh-send-letter]
;;      invokes mh-edit-mhn automatically before sending.)
;;      actually, instead of mh-auto-edit-mhn,
;;      should read automhnproc from profile
;; MIME option to mh-forward
;; command to move to content-description insertion point

(defvar mh-mhn-args nil
  "Extra arguments to have \\[mh-edit-mhn] pass to the \"mhn\" command.
The arguments are passed to mhn if \\[mh-edit-mhn] is given a
prefix argument.  Normally default arguments to mhn are specified in the
MH profile.")

(defvar mh-media-type-regexp
  (concat (regexp-opt '("text" "image" "audio" "video" "application"
                        "multipart" "message") t)
          "/[-.+a-zA-Z0-9]+")
  "Regexp matching valid media types used in MIME attachment compositions.")

;; Just defvar the variable to avoid compiler warning... This doesn't bind
;; the variable, so things should work exactly as before.
(defvar mh-have-file-command)

;;;###mh-autoload
(defun mh-have-file-command ()
  "Return t if 'file' command is on the system.
'file -i' is used to get MIME type of composition insertion."
  (when (not (boundp 'mh-have-file-command))
    (load "executable" t t)        ; executable-find not autoloaded in emacs20
    (setq mh-have-file-command
          (and (fboundp 'executable-find)
               (executable-find "file") ; file command exists
                                        ;   and accepts -i and -b args.
               (zerop (call-process "file" nil nil nil "-i" "-b"
                                    (expand-file-name "inc" mh-progs))))))
  mh-have-file-command)

(defvar mh-file-mime-type-substitutions
  '(("application/msword" "\.xls" "application/ms-excel")
    ("application/msword" "\.ppt" "application/ms-powerpoint")
    ("text/plain" "\.vcf" "text/x-vcard"))
  "Substitutions to make for Content-Type returned from file command.
The first element is the Content-Type returned by the file command.
The second element is a regexp matching the file name, usually the extension.
The third element is the Content-Type to replace with.")

(defun mh-file-mime-type-substitute (content-type filename)
  "Return possibly changed CONTENT-TYPE on the FILENAME.
Substitutions are made from the `mh-file-mime-type-substitutions' variable."
  (let ((subst mh-file-mime-type-substitutions)
        (type) (match) (answer content-type)
        (case-fold-search t))
    (while subst
      (setq type (car (car subst))
            match (elt (car subst) 1))
      (if (and (string-equal content-type type)
               (string-match match filename))
          (setq answer (elt (car subst) 2)
                subst nil)
        (setq subst (cdr subst))))
    answer))

;;;###mh-autoload
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
                (mh-file-mime-type-substitute (match-string 0) filename)))
          (kill-buffer tmp-buffer)))))))

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
    ("text/richtext") ("text/x-vcard") ("text/xml")

    ("video/mpeg") ("video/quicktime"))
  "Valid MIME content types.
See documentation for \\[mh-edit-mhn].")

;; RFC 2045 - Multipurpose Internet Mail Extensions (MIME) Part One:
;;            Format of Internet Message Bodies.
;; RFC 2046 - Multipurpose Internet Mail Extensions (MIME) Part Two:
;;            Media Types.
;; RFC 2049 - Multipurpose Internet Mail Extensions (MIME) Part Five:
;;            Conformance Criteria and Examples.
;; RFC 2017 - Definition of the URL MIME External-Body Access-Type
;; RFC 1738 - Uniform Resource Locators (URL)
(defvar mh-access-types
  '(("anon-ftp")        ; RFC2046 Anonymous File Transfer Protocol
    ("file")            ; RFC1738 Host-specific file names
    ("ftp")             ; RFC2046 File Transfer Protocol
    ("gopher")          ; RFC1738 The Gopher Protocol
    ("http")            ; RFC1738 Hypertext Transfer Protocol
    ("local-file")      ; RFC2046 Local file access
    ("mail-server")     ; RFC2046 mail-server Electronic mail address
    ("mailto")          ; RFC1738 Electronic mail address
    ("news")            ; RFC1738 Usenet news
    ("nntp")            ; RFC1738 Usenet news using NNTP access
    ("propspero")       ; RFC1738 Prospero Directory Service
    ("telnet")          ; RFC1738 Telnet
    ("tftp")            ; RFC2046 Trivial File Transfer Protocol
    ("url")             ; RFC2017 URL scheme MIME access-type Protocol
    ("wais"))           ; RFC1738 Wide Area Information Servers
  "Valid MIME access-type values.")

;;;###mh-autoload
(defun mh-mhn-compose-insertion (filename type description attributes)
  "Add a directive to insert a MIME message part from a file.
This is the typical way to insert non-text parts in a message.

Arguments are FILENAME, which tells where to find the file, TYPE, the MIME
content type, DESCRIPTION, a line of text for the Content-Description field.
ATTRIBUTES is a comma separated list of name=value pairs that is appended to
the Content-Type field of the attachment.

See also \\[mh-edit-mhn]."
  (interactive (let ((filename (read-file-name "Insert contents of: ")))
                 (list
                  filename
                  (or (mh-file-mime-type filename)
                      (completing-read "Content-Type: "
                                       (if (fboundp 'mailcap-mime-types)
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
  "Insert a mhn directive to insert a file.

The file specified by FILENAME is encoded as TYPE. An optional DESCRIPTION is
used as the Content-Description field, optional set of ATTRIBUTES and an
optional COMMENT can also be included."
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


;;;###mh-autoload
(defun mh-mhn-compose-anon-ftp (host filename type description)
  "Add a directive for a MIME anonymous ftp external body part.
This directive tells MH to include a reference to a message/external-body part
retrievable by anonymous FTP.

Arguments are HOST and FILENAME, which tell where to find the file, TYPE, the
MIME content type, and DESCRIPTION, a line of text for the Content-description
header.

See also \\[mh-edit-mhn]."
  (interactive (list
                (read-string "Remote host: ")
                (read-string "Remote filename: ")
                (completing-read "External Content-Type: "
                                 (if (fboundp 'mailcap-mime-types)
                                     (mapcar 'list (mailcap-mime-types))
                                   mh-mime-content-types))
                (read-string "External Content-Description: ")))
  (mh-mhn-compose-external-type "anon-ftp" host filename
                                type description))

;;;###mh-autoload
(defun mh-mhn-compose-external-compressed-tar (host filename description)
  "Add a directive to include a MIME reference to a compressed tar file.
The file should be available via anonymous ftp. This directive tells MH to
include a reference to a message/external-body part.

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

;;;###mh-autoload
(defun mh-mhn-compose-external-type (access-type host filename type
                                                 &optional description
                                                 attributes extra-params
                                                 comment)
  "Add a directive to include a MIME reference to a remote file.
The file should be available via anonymous ftp. This directive tells MH to
include a reference to a message/external-body part.

Arguments are ACCESS-TYPE, HOST and FILENAME, which tell where to find the
file and TYPE which is the MIME Content-Type. Optional arguments include
DESCRIPTION, a line of text for the Content-description header, ATTRIBUTES,
EXTRA-PARAMS, and COMMENT.

See also \\[mh-edit-mhn]."
  (interactive (list
                (completing-read "Access Type: " mh-access-types)
                (read-string "Remote host: ")
                (read-string "Remote url-path: ")
                (completing-read "Content-Type: "
                                 (if (fboundp 'mailcap-mime-types)
                                     (mapcar 'list (mailcap-mime-types))
                                   mh-mime-content-types))
                (if current-prefix-arg (read-string "Content-description: "))
                (if current-prefix-arg (read-string "Attributes: "))
                (if current-prefix-arg (read-string "Extra Parameters: "))
                (if current-prefix-arg (read-string "Comment: "))))
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
  (let ((directory (file-name-directory filename)))
    (and directory
         (insert "; directory=\"" directory "\"")))
  (and extra-params
       (insert "; " extra-params))
  (insert "\n"))

;;;###mh-autoload
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
                                     (if (numberp mh-sent-from-msg)
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
    (if (numberp mh-sent-from-msg)
        (insert " " (int-to-string mh-sent-from-msg))))
  (insert "\n"))

;;;###mh-autoload
(defun mh-edit-mhn (&optional extra-args)
  "Format the current draft for MIME, expanding any mhn directives.

Process the current draft with the mhn program, which, using directives
already inserted in the draft, fills in all the MIME components and header
fields.

This step is performed automatically when sending the message, but this
function may be called manually before sending the draft as well.

The `\\[mh-revert-mhn-edit]' command undoes this command. The arguments in the
list `mh-mhn-args' are passed to mhn if this function is passed an optional
prefix argument EXTRA-ARGS.

For assistance with creating mhn directives to insert various types of
components in a message, see \\[mh-mhn-compose-insertion] (generic insertion
from a file), \\[mh-mhn-compose-anon-ftp] (external reference to file via
anonymous ftp), \\[mh-mhn-compose-external-compressed-tar] \ \(reference to
compressed tar file via anonymous ftp), and \\[mh-mhn-compose-forw] (forward
message).

The value of `mh-edit-mhn-hook' is a list of functions to be called, with no
arguments, after performing the conversion.

The mhn program is part of MH version 6.8 or later."
  (interactive "*P")
  (mh-mhn-quote-unescaped-sharp)
  (save-buffer)
  (message "mhn editing...")
  (cond
   ((mh-variant-p 'nmh)
    (mh-exec-cmd-error nil
                       "mhbuild" (if extra-args mh-mhn-args) buffer-file-name))
   (t
    (mh-exec-cmd-error (format "mhdraft=%s" buffer-file-name)
                       "mhn" (if extra-args mh-mhn-args) buffer-file-name)))
  (revert-buffer t t)
  (message "mhn editing...done")
  (run-hooks 'mh-edit-mhn-hook))

(defun mh-mhn-quote-unescaped-sharp ()
  "Quote `#' characters that haven't been quoted for `mhbuild'.
If the `#' character is present in the first column, but it isn't part of a
MHN directive then `mhbuild' gives an error. This function will quote all such
characters."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#" nil t)
      (beginning-of-line)
      (unless (mh-mhn-directive-present-p (point) (line-end-position))
        (insert "#"))
      (goto-char (line-end-position)))))

;;;###mh-autoload
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

;;;###mh-autoload
(defun mh-mhn-directive-present-p (&optional begin end)
  "Check if the text between BEGIN and END might be a MHN directive.
The optional argument BEGIN defaults to the beginning of the buffer, while END
defaults to the the end of the buffer."
  (unless begin (setq begin (point-min)))
  (unless end (setq end (point-max)))
  (save-excursion
    (block 'search-for-mhn-directive
      (goto-char begin)
      (while (re-search-forward "^#" end t)
        (let ((s (buffer-substring-no-properties (point) (line-end-position))))
          (cond ((equal s ""))
                ((string-match "^forw[ \t\n]+" s)
                 (return-from 'search-for-mhn-directive t))
                (t (let ((first-token (car (split-string s "[ \t;@]"))))
                     (when (and first-token
                                (string-match mh-media-type-regexp
                                              first-token))
                       (return-from 'search-for-mhn-directive t)))))))
      nil)))



;;; MIME composition functions

;;;###mh-autoload
(defun mh-mml-to-mime ()
  "Compose MIME message from mml directives.
This step is performed automatically when sending the message, but this
function may be called manually before sending the draft as well."
  (interactive)
  (require 'message)
  (when mh-gnus-pgp-support-flag ;; This is only needed for PGP
    (message-options-set-recipient))
  (let ((saved-text (buffer-string))
        (buffer (current-buffer))
        (modified-flag (buffer-modified-p)))
    (condition-case err (mml-to-mime)
      (error
       (with-current-buffer buffer
         (delete-region (point-min) (point-max))
         (insert saved-text)
         (set-buffer-modified-p modified-flag))
       (error (error-message-string err))))))

;;;###mh-autoload
(defun mh-mml-forward-message (description folder message)
  "Forward a message as attachment.
The function will prompt the user for a DESCRIPTION, a FOLDER and MESSAGE
number."
  (let ((msg (if (and (equal message "") (numberp mh-sent-from-msg))
                 mh-sent-from-msg
               (car (read-from-string message)))))
    (cond ((integerp msg)
           (if (string= "" description)
               ;; Rationale: mml-attach-file constructs a malformed composition
               ;; if the description string is empty.  This fixes SF #625168.
               (mml-attach-file (format "%s%s/%d"
                                        mh-user-path (substring folder 1) msg)
                                "message/rfc822")
             (mml-attach-file (format "%s%s/%d"
                                      mh-user-path (substring folder 1) msg)
                              "message/rfc822"
                              description)))
          (t (error "The message number, %s is not a integer!" msg)))))

(defvar mh-mml-cryptographic-method-history ())

;;;###mh-autoload
(defun mh-mml-query-cryptographic-method ()
  "Read the cryptographic method to use."
  (if current-prefix-arg
      (let ((def (or (car mh-mml-cryptographic-method-history)
                     mh-mml-method-default)))
        (completing-read (format "Method: [%s] " def)
                         '(("pgp") ("pgpmime") ("smime"))
                         nil t nil 'mh-mml-cryptographic-method-history def))
    mh-mml-method-default))

;;;###mh-autoload
(defun mh-mml-attach-file (&optional disposition)
  "Attach a file to the outgoing MIME message.
The file is not inserted or encoded until you send the message with
`\\[mh-send-letter]'.
Message disposition is \"inline\" or \"attachment\" and is prompted for if
DISPOSITION is nil.

This is basically `mml-attach-file' from gnus, modified such that a prefix
argument yields an `inline' disposition and Content-Type is determined
automatically."
  (let* ((file (mml-minibuffer-read-file "Attach file: "))
         (type (or (mh-file-mime-type file)
                   (completing-read "Content-Type: "
                                    (if (fboundp 'mailcap-mime-types)
                                        (mapcar 'list (mailcap-mime-types))
                                      mh-mime-content-types))))
         (description (mml-minibuffer-read-description))
         (dispos (or disposition
                     (completing-read "Disposition: [attachment] "
                                      '(("attachment")("inline"))
                                      nil t nil nil
                                      "attachment"))))
    (mml-insert-empty-tag 'part 'type type 'filename file
                          'disposition dispos 'description description)))

(defvar mh-identity-pgg-default-user-id)

(defun mh-secure-message (method mode &optional identity)
  "Add directive to Encrypt/Sign an entire message.
METHOD should be one of: \"pgpmime\", \"pgp\", \"smime\".
MODE should be one of: \"sign\", \"encrypt\", \"signencrypt\", \"none\".
IDENTITY is optionally the default-user-id to use."
  (if (not mh-gnus-pgp-support-flag)
      (error "Sorry.  Your version of gnus does not support PGP/GPG")
    ;; Check the arguments
    (let ((valid-methods (list "pgpmime" "pgp" "smime"))
          (valid-modes (list "sign" "encrypt" "signencrypt" "none")))
      (if (not (member method valid-methods))
          (error "Sorry. METHOD \"%s\" is invalid" method))
      (if (not (member mode valid-modes))
          (error "Sorry. MODE \"%s\" is invalid" mode))
      (mml-unsecure-message)
      (if (not (string= mode "none"))
        (save-excursion
          (goto-char (point-min))
          (mh-goto-header-end 1)
          (if mh-identity-pgg-default-user-id
              (mml-insert-tag 'secure 'method method 'mode mode
                              'sender mh-identity-pgg-default-user-id)
            (mml-insert-tag 'secure 'method method 'mode mode)))))))

;;;###mh-autoload
(defun mh-mml-unsecure-message (&optional ignore)
  "Remove any secure message directives.
The IGNORE argument is not used."
  (interactive "P")
  (if (not mh-gnus-pgp-support-flag)
      (error "Sorry.  Your version of gnus does not support PGP/GPG")
    (mml-unsecure-message)))

;;;###mh-autoload
(defun mh-mml-secure-message-sign (method)
  "Add security directive to sign the entire message using METHOD."
  (interactive (list (mh-mml-query-cryptographic-method)))
  (mh-secure-message method "sign" mh-identity-pgg-default-user-id))

;;;###mh-autoload
(defun mh-mml-secure-message-encrypt (method)
  "Add security directive to encrypt the entire message using METHOD."
  (interactive (list (mh-mml-query-cryptographic-method)))
  (mh-secure-message method "encrypt" mh-identity-pgg-default-user-id))

;;;###mh-autoload
(defun mh-mml-secure-message-signencrypt (method)
  "Add security directive to encrypt and sign the entire message using METHOD."
  (interactive (list (mh-mml-query-cryptographic-method)))
  (mh-secure-message method "signencrypt" mh-identity-pgg-default-user-id))

;;;###mh-autoload
(defun mh-mml-directive-present-p ()
  "Check if the current buffer has text which may be an MML directive."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     "\\(<#part\\(.\\|\n\\)*>[ \n\t]*<#/part>\\|^<#secure.+>$\\)"
     nil t)))



;;; MIME cleanup

;;;###mh-autoload
(defun mh-mime-cleanup ()
  "Free the decoded MIME parts."
  (let ((mime-data (gethash (current-buffer) mh-globals-hash)))
    ;; This is for Emacs, what about XEmacs?
    (mh-funcall-if-exists remove-images (point-min) (point-max))
    (when mime-data
      (mm-destroy-parts (mh-mime-handles mime-data))
      (remhash (current-buffer) mh-globals-hash))))

;;;###mh-autoload
(defun mh-destroy-postponed-handles ()
  "Free MIME data for externally displayed mime parts."
  (let ((mime-data (mh-buffer-data)))
    (when mime-data
      (mm-destroy-parts (mh-mime-handles mime-data)))
    (remhash (current-buffer) mh-globals-hash)))

(defun mh-handle-set-external-undisplayer (folder handle function)
  "Replacement for `mm-handle-set-external-undisplayer'.
This is only called in recent versions of Gnus. The MIME handles are stored
in data structures corresponding to MH-E folder buffer FOLDER instead of in
Gnus (as in the original). The MIME part, HANDLE is associated with the
undisplayer FUNCTION."
  (if (mm-keep-viewer-alive-p handle)
      (let ((new-handle (copy-sequence handle)))
        (mm-handle-set-undisplayer new-handle function)
        (mm-handle-set-undisplayer handle nil)
        (save-excursion
          (set-buffer folder)
          (push new-handle (mh-mime-handles (mh-buffer-data)))))
    (mm-handle-set-undisplayer handle function)))



;;; MIME transformations
(eval-when-compile (require 'font-lock))

;;;###mh-autoload
(defun mh-add-missing-mime-version-header ()
  "Some mail programs don't put a MIME-Version header.
I have seen this only in spam, so maybe we shouldn't fix this ;-)"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\n\n" nil t)
    (save-restriction
      (narrow-to-region (point-min) (point))
      (when (and (message-fetch-field "content-type")
                 (not (message-fetch-field "mime-version")))
        (goto-char (point-min))
        (insert "MIME-Version: 1.0\n")))))

(defun mh-small-show-buffer-p ()
  "Check if show buffer is small.
This is used to decide if smileys and graphical emphasis will be displayed."
  (let ((max nil))
    (when (and (boundp 'font-lock-maximum-size) font-lock-maximum-size)
      (cond ((numberp font-lock-maximum-size)
             (setq max font-lock-maximum-size))
            ((listp font-lock-maximum-size)
             (setq max (cdr (or (assoc 'mh-show-mode font-lock-maximum-size)
                                (assoc t font-lock-maximum-size)))))))
    (or (not (numberp max)) (>= (/ max 8) (buffer-size)))))

;;;###mh-autoload
(defun mh-display-smileys ()
  "Function to display smileys."
  (when (and mh-graphical-smileys-flag (mh-small-show-buffer-p))
    (mh-funcall-if-exists smiley-region (point-min) (point-max))))

;;;###mh-autoload
(defun mh-display-emphasis ()
  "Function to display graphical emphasis."
  (when (and mh-graphical-emphasis-flag (mh-small-show-buffer-p))
    (flet ((article-goto-body ()))      ; shadow this function to do nothing
      (save-excursion
        (goto-char (point-min))
        (article-emphasize)))))

;; Copied from gnus-art.el (should be checked for other cool things that can
;; be added to the buttons)
(defvar mh-mime-button-commands
  '((mh-press-button "\r" "Toggle Display")))
(defvar mh-mime-button-map
  (let ((map (make-sparse-keymap)))
    (unless (>= (string-to-number emacs-version) 21)
      ;; XEmacs doesn't care.
      (set-keymap-parent map mh-show-mode-map))
    (mh-do-in-gnu-emacs
     (define-key map [mouse-2] 'mh-push-button))
    (mh-do-in-xemacs
     (define-key map '(button2) 'mh-push-button))
    (dolist (c mh-mime-button-commands)
      (define-key map (cadr c) (car c)))
    map))
(defvar mh-mime-button-line-format-alist
  '((?T long-type ?s)
    (?d description ?s)
    (?p index ?s)
    (?e dots ?s)))
(defvar mh-mime-button-line-format "%{%([%p. %d%T]%)%}%e\n")
(defvar mh-mime-security-button-pressed nil)
(defvar mh-mime-security-button-line-format "%{%([[%t:%i]%D]%)%}\n")
(defvar mh-mime-security-button-end-line-format "%{%([[End of %t]%D]%)%}\n")
(defvar mh-mime-security-button-line-format-alist
  '((?t type ?s)
    (?i info ?s)
    (?d details ?s)
    (?D pressed-details ?s)))
(defvar mh-mime-security-button-map
  (let ((map (make-sparse-keymap)))
    (unless (>= (string-to-number emacs-version) 21)
      (set-keymap-parent map mh-show-mode-map))
    (define-key map "\r" 'mh-press-button)
    (mh-do-in-gnu-emacs
     (define-key map [mouse-2] 'mh-push-button))
    (mh-do-in-xemacs
     (define-key map '(button2) 'mh-push-button))
    map))

(defvar mh-mime-save-parts-directory nil
  "Default to use for `mh-mime-save-parts-default-directory'.
Set from last use.")

;;;###mh-autoload
(defun mh-mime-save-parts (arg)
  "Store the MIME parts of the current message.
If ARG, prompt for directory, else use that specified by the variable
`mh-mime-save-parts-default-directory'. These directories may be superseded by
mh_profile directives, since this function calls on mhstore or mhn to do the
actual storing."
  (interactive "P")
  (let ((msg (if (eq major-mode 'mh-show-mode)
                 (mh-show-buffer-message-number)
               (mh-get-msg-num t)))
        (folder (if (eq major-mode 'mh-show-mode)
                    mh-show-folder-buffer
                  mh-current-folder))
        (command (if (mh-variant-p 'nmh) "mhstore" "mhn"))
        (directory
         (cond
          ((and (or arg
                    (equal nil mh-mime-save-parts-default-directory)
                    (equal t mh-mime-save-parts-default-directory))
                (not mh-mime-save-parts-directory))
           (read-file-name "Store in directory: " nil nil t nil))
          ((and (or arg
                    (equal t mh-mime-save-parts-default-directory))
                mh-mime-save-parts-directory)
           (read-file-name (format
                            "Store in directory: [%s] "
                            mh-mime-save-parts-directory)
                           "" mh-mime-save-parts-directory t ""))
          ((stringp mh-mime-save-parts-default-directory)
           mh-mime-save-parts-default-directory)
          (t
           mh-mime-save-parts-directory))))
    (if (and (equal directory "") mh-mime-save-parts-directory)
        (setq directory mh-mime-save-parts-directory))
    (if (not (file-directory-p directory))
        (message "No directory specified")
      (if (equal nil mh-mime-save-parts-default-directory)
          (setq mh-mime-save-parts-directory directory))
      (save-excursion
        (set-buffer (get-buffer-create mh-log-buffer))
        (cd directory)
        (setq mh-mime-save-parts-directory directory)
        (let ((initial-size (mh-truncate-log-buffer)))
          (apply 'call-process
                 (expand-file-name command mh-progs) nil t nil
                 (mh-list-to-string (list folder msg "-auto")))
          (if (> (buffer-size) initial-size)
              (save-window-excursion
                (switch-to-buffer-other-window mh-log-buffer)
                (sit-for 3))))))))

;; Avoid errors if gnus-sum isn't loaded yet...
(defvar gnus-newsgroup-charset nil)
(defvar gnus-newsgroup-name nil)

(defun mh-decode-message-body ()
  "Decode message based on charset.
If message has been encoded for transfer take that into account."
  (let (ct charset cte)
    (goto-char (point-min))
    (re-search-forward "\n\n" nil t)
    (save-restriction
      (narrow-to-region (point-min) (point))
      (setq ct (ignore-errors (mail-header-parse-content-type
                               (message-fetch-field "Content-Type" t)))
            charset (mail-content-type-get ct 'charset)
            cte (message-fetch-field "Content-Transfer-Encoding")))
    (when (stringp cte) (setq cte (mail-header-strip cte)))
    (when (or (not ct) (equal (car ct) "text/plain"))
      (save-restriction
        (narrow-to-region (min (1+ (mh-mail-header-end)) (point-max))
                          (point-max))
        (mm-decode-body charset
                        (and cte (intern (downcase
                                          (gnus-strip-whitespace cte))))
                        (car ct))))))

;;;###mh-autoload
(defun mh-toggle-mh-decode-mime-flag ()
  "Toggle whether MH-E should decode MIME or not."
  (interactive)
  (setq mh-decode-mime-flag (not mh-decode-mime-flag))
  (mh-show nil t)
  (message (format "(setq mh-decode-mime-flag %s)" mh-decode-mime-flag)))

;;;###mh-autoload
(defun mh-decode-message-header ()
  "Decode RFC2047 encoded message header fields."
  (when mh-decode-mime-flag
    (let ((buffer-read-only nil))
      (rfc2047-decode-region (point-min) (mh-mail-header-end)))))

;;;###mh-autoload
(defun mh-mime-display (&optional pre-dissected-handles)
  "Display (and possibly decode) MIME handles.
Optional argument, PRE-DISSECTED-HANDLES is a list of MIME handles. If
present they are displayed otherwise the buffer is parsed and then
displayed."
  (let ((handles ())
        (folder mh-show-folder-buffer)
        (raw-message-data (buffer-string)))
    (flet ((mm-handle-set-external-undisplayer
            (handle function)
            (mh-handle-set-external-undisplayer folder handle function)))
      (goto-char (point-min))
      (unless (search-forward "\n\n" nil t)
        (goto-char (point-max))
        (insert "\n\n"))

      (condition-case err
          (progn
            ;; If needed dissect the current buffer
            (if pre-dissected-handles
                (setq handles pre-dissected-handles)
              (setq handles (or (mm-dissect-buffer nil) (mm-uu-dissect)))
              (setf (mh-mime-handles (mh-buffer-data))
                    (mm-merge-handles handles
                                      (mh-mime-handles (mh-buffer-data))))
              (unless handles (mh-decode-message-body)))

            (cond ((and handles
                        (or (not (stringp (car handles))) (cdr handles)))
                   ;; Goto start of message body
                   (goto-char (point-min))
                   (or (search-forward "\n\n" nil t) (goto-char (point-max)))

                   ;; Delete the body
                   (delete-region (point) (point-max))

                   ;; Display the MIME handles
                   (mh-mime-display-part handles))
                  (t (mh-signature-highlight))))
        (error
         (message "Please report this error. The error message is:\n %s"
                  (error-message-string err))
         (delete-region (point-min) (point-max))
         (insert raw-message-data))))))

(defun mh-mime-display-part (handle)
  "Decides the viewer to call based on the type of HANDLE."
  (cond ((null handle) nil)
        ((not (stringp (car handle)))
         (mh-mime-display-single handle))
        ((equal (car handle) "multipart/alternative")
         (mh-mime-display-alternative (cdr handle)))
        ((and mh-gnus-pgp-support-flag
              (or (equal (car handle) "multipart/signed")
                  (equal (car handle) "multipart/encrypted")))
         (mh-mime-display-security handle))
        (t (mh-mime-display-mixed (cdr handle)))))

(defun mh-mime-display-alternative (handles)
  "Choose among the alternatives, HANDLES the part that will be displayed.
If no part is preferred then all the parts are displayed."
  (let* ((preferred (mm-preferred-alternative handles))
         (others (loop for x in handles unless (eq x preferred) collect x)))
    (cond ((and preferred (stringp (car preferred)))
           (mh-mime-display-part preferred)
           (mh-mime-maybe-display-alternatives others))
          (preferred
           (save-restriction
             (narrow-to-region (point) (if (eobp) (point) (1+ (point))))
             (mh-mime-display-single preferred)
             (mh-mime-maybe-display-alternatives others)
             (goto-char (point-max))))
          (t (mh-mime-display-mixed handles)))))

(defun mh-mime-maybe-display-alternatives (alternatives)
  "Show buttons for ALTERNATIVES.
If `mh-mime-display-alternatives-flag' is non-nil then display buttons for
alternative parts that are usually suppressed."
  (when (and mh-display-buttons-for-alternatives-flag alternatives)
    (insert "\n----------------------------------------------------\n")
    (insert "Alternatives:\n")
    (dolist (x alternatives)
      (insert "\n")
      (mh-insert-mime-button x (mh-mime-part-index x) nil))
    (insert "\n----------------------------------------------------\n")))

(defun mh-mime-display-mixed (handles)
  "Display the list of MIME parts, HANDLES recursively."
  (mapcar #'mh-mime-display-part handles))

(defun mh-mime-part-index (handle)
  "Generate the button number for MIME part, HANDLE.
Notice that a hash table is used to display the same number when buttons need
to be displayed multiple times (for instance when nested messages are
opened)."
  (or (gethash handle (mh-mime-part-index-hash (mh-buffer-data)))
      (setf (gethash handle (mh-mime-part-index-hash (mh-buffer-data)))
            (incf (mh-mime-parts-count (mh-buffer-data))))))

(defun mh-small-image-p (handle)
  "Decide whether HANDLE is a \"small\" image that can be displayed inline.
This is only useful if a Content-Disposition header is not present."
  (let ((media-test (caddr (assoc (car (mm-handle-type handle))
                                  mh-mm-inline-media-tests)))
        (mm-inline-large-images t))
    (and media-test
         (equal (mm-handle-media-supertype handle) "image")
         (funcall media-test handle)    ; Since mm-inline-large-images is T,
                                        ; this only tells us if the image is
                                        ; something that emacs can display
         (let* ((image (mm-get-image handle)))
           (or (mh-do-in-xemacs
                 (and (mh-funcall-if-exists glyphp image)
                      (< (glyph-width image)
                         (or mh-max-inline-image-width (window-pixel-width)))
                      (< (glyph-height image)
                         (or mh-max-inline-image-height
                             (window-pixel-height)))))
               (mh-do-in-gnu-emacs
                 (let ((size (mh-funcall-if-exists image-size image)))
                   (and size
                        (< (cdr size) (or mh-max-inline-image-height
                                          (1- (window-height))))
                        (< (car size) (or mh-max-inline-image-width
                                          (window-width)))))))))))

(defun mh-inline-vcard-p (handle)
  "Decide if HANDLE is a vcard that must be displayed inline."
  (let ((type (mm-handle-type handle)))
    (and (or (featurep 'vcard) (fboundp 'vcard-pretty-print))
         (consp type)
         (equal (car type) "text/x-vcard")
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (not (mh-signature-separator-p)))))))

(defun mh-mime-display-single (handle)
  "Display a leaf node, HANDLE in the MIME tree."
  (let* ((type (mm-handle-media-type handle))
         (small-image-flag (mh-small-image-p handle))
         (attachmentp (equal (car (mm-handle-disposition handle))
                             "attachment"))
         (inlinep (and (equal (car (mm-handle-disposition handle)) "inline")
                       (mm-inlinable-p handle)
                       (mm-inlined-p handle)))
         (displayp (or inlinep                   ; show if inline OR
                       (mh-inline-vcard-p handle);      inline vcard OR
                       (and (not attachmentp)    ;      if not an attachment
                            (or small-image-flag ;        and small image
                                                 ;        and user wants inline
                                (and (not (equal
                                           (mm-handle-media-supertype handle)
                                           "image"))
                                     (mm-inlinable-p handle)
                                     (mm-inlined-p handle)))))))
    (save-restriction
      (narrow-to-region (point) (if (eobp) (point) (1+ (point))))
      (cond ((and mh-gnus-pgp-support-flag
                  (equal type "application/pgp-signature"))
             nil)             ; skip signatures as they are already handled...
            ((not displayp)
             (insert "\n")
             (mh-insert-mime-button handle (mh-mime-part-index handle) nil))
            ((and displayp (not mh-display-buttons-for-inline-parts-flag))
             (or (mm-display-part handle) (mm-display-part handle))
             (mh-signature-highlight handle))
            ((and displayp mh-display-buttons-for-inline-parts-flag)
             (insert "\n")
             (mh-insert-mime-button handle (mh-mime-part-index handle) nil)
             (forward-line -1)
             (mh-mm-display-part handle)))
      (goto-char (point-max)))))

(defun mh-signature-highlight (&optional handle)
  "Highlight message signature in HANDLE.
The optional argument, HANDLE is a MIME handle if the function is being used
to highlight the signature in a MIME part."
  (let ((regexp
         (cond ((not handle) "^-- $")
               ((not (and (equal (mm-handle-media-supertype handle) "text")
                          (equal (mm-handle-media-subtype handle) "html")))
                "^-- $")
               ((eq (mh-mm-text-html-renderer) 'lynx) "^   --$")
               (t "^--$"))))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward regexp nil t)
        (mh-do-in-gnu-emacs
          (let ((ov (make-overlay (point) (point-max))))
            (overlay-put ov 'face 'mh-show-signature)
            (overlay-put ov 'evaporate t)))
        (mh-do-in-xemacs
          (set-extent-property (make-extent (point) (point-max))
                               'face 'mh-show-signature))))))

(mh-do-in-xemacs
 (defvar dots)
 (defvar type))

(defun mh-insert-mime-button (handle index displayed)
  "Insert MIME button for HANDLE.
INDEX is the part number that will be DISPLAYED. It is also used by commands
like \"K v\" which operate on individual MIME parts."
  ;; The button could be displayed by a previous decode. In that case
  ;; undisplay it if we need a hidden button.
  (when (and (mm-handle-displayed-p handle) (not displayed))
    (mm-display-part handle))
  (let ((name (or (mail-content-type-get (mm-handle-type handle) 'name)
                  (mail-content-type-get (mm-handle-disposition handle)
                                         'filename)
                  (mail-content-type-get (mm-handle-type handle) 'url)
                  ""))
        (type (mm-handle-media-type handle))
        (description (mail-decode-encoded-word-string
                      (or (mm-handle-description handle) "")))
        (dots (if (or displayed (mm-handle-displayed-p handle)) "   " "..."))
        long-type begin end)
    (if (string-match ".*/" name) (setq name (substring name (match-end 0))))
    (setq long-type (concat type (and (not (equal name ""))
                                      (concat "; " name))))
    (unless (equal description "")
      (setq long-type (concat " --- " long-type)))
    (unless (bolp) (insert "\n"))
    (setq begin (point))
    (gnus-eval-format
     mh-mime-button-line-format mh-mime-button-line-format-alist
     `(,@(gnus-local-map-property mh-mime-button-map)
         mh-callback mh-mm-display-part
         mh-part ,index
         mh-data ,handle))
    (setq end (point))
    (widget-convert-button
     'link begin end
     :mime-handle handle
     :action 'mh-widget-press-button
     :button-keymap mh-mime-button-map
     :help-echo
     "Mouse-2 click or press RET (in show buffer) to toggle display")
    (dolist (ov (mh-funcall-if-exists overlays-in begin end))
      (mh-funcall-if-exists overlay-put ov 'evaporate t))))

;; There is a bug in Gnus inline image display due to which an extra line
;; gets inserted every time it is viewed. To work around that problem we are
;; using an extra property 'mh-region to remember the region that is added
;; when the button is clicked. The region is then deleted to make sure that
;; no extra lines get inserted.
(defun mh-mm-display-part (handle)
  "Toggle display of button for MIME part, HANDLE."
  (beginning-of-line)
  (let ((id (get-text-property (point) 'mh-part))
        (point (point))
        (window (selected-window))
        (mail-parse-charset 'nil)
        (mail-parse-ignored-charsets nil)
        region buffer-read-only)
    (save-excursion
      (unwind-protect
          (let ((win (get-buffer-window (current-buffer) t)))
            (when win
              (select-window win))
            (goto-char point)

            (if (mm-handle-displayed-p handle)
                ;; This will remove the part.
                (progn
                  ;; Delete the button and displayed part (if any)
                  (let ((region (get-text-property point 'mh-region)))
                    (when region
                      (mh-funcall-if-exists
                       remove-images (car region) (cdr region)))
                    (mm-display-part handle)
                    (when region
                      (delete-region (car region) (cdr region))))
                  ;; Delete button (if it still remains). This happens for
                  ;; externally displayed parts where the previous step does
                  ;; nothing.
                  (unless (eolp)
                    (delete-region (point) (progn (forward-line) (point)))))
              (save-restriction
                (delete-region (point) (progn (forward-line 1) (point)))
                (narrow-to-region (point) (point))
                ;; Maybe we need another unwind-protect here.
                (when (equal (mm-handle-media-supertype handle) "image")
                  (insert "\n"))
                (when (and (not (eq (ignore-errors (mm-display-part handle))
                                    'inline))
                           (equal (mm-handle-media-supertype handle)
                                  "image"))
                  (goto-char (point-min))
                  (delete-char 1))
                (when (equal (mm-handle-media-supertype handle) "text")
                  (when (eq mh-highlight-citation-p 'gnus)
                    (mh-gnus-article-highlight-citation))
                  (mh-display-smileys)
                  (mh-display-emphasis)
                  (mh-signature-highlight handle))
                (setq region (cons (progn (goto-char (point-min))
                                          (point-marker))
                                   (progn (goto-char (point-max))
                                          (point-marker)))))))
        (when (window-live-p window)
          (select-window window))
        (goto-char point)
        (beginning-of-line)
        (mh-insert-mime-button handle id (mm-handle-displayed-p handle))
        (goto-char point)
        (when region
          (add-text-properties (line-beginning-position) (line-end-position)
                               `(mh-region ,region)))))))

;;;###mh-autoload
(defun mh-press-button ()
  "Press MIME button.
If the MIME part is visible then it is removed. Otherwise the part is
displayed."
  (interactive)
  (let ((mm-inline-media-tests mh-mm-inline-media-tests)
        (data (get-text-property (point) 'mh-data))
        (function (get-text-property (point) 'mh-callback))
        (buffer-read-only nil)
        (folder mh-show-folder-buffer))
    (flet ((mm-handle-set-external-undisplayer
            (handle function)
            (mh-handle-set-external-undisplayer folder handle function)))
      (when (and function (eolp))
        (backward-char))
      (unwind-protect (and function (funcall function data))
        (set-buffer-modified-p nil)))))

;;;###mh-autoload
(defun mh-push-button (event)
  "Click MIME button for EVENT.
If the MIME part is visible then it is removed. Otherwise the part is
displayed. This function is called when the mouse is used to click the MIME
button."
  (interactive "e")
  (mh-do-at-event-location event
    (let ((folder mh-show-folder-buffer)
          (mm-inline-media-tests mh-mm-inline-media-tests)
          (data (get-text-property (point) 'mh-data))
          (function (get-text-property (point) 'mh-callback)))
      (flet ((mm-handle-set-external-undisplayer (handle func)
               (mh-handle-set-external-undisplayer folder handle func)))
        (and function (funcall function data))))))

;;;###mh-autoload
(defun mh-mime-save-part ()
  "Save MIME part at point."
  (interactive)
  (let ((data (get-text-property (point) 'mh-data)))
    (when data
      (let ((mm-default-directory
             (file-name-as-directory (or mh-mime-save-parts-directory
                                         default-directory))))
        (mh-mm-save-part data)
        (setq mh-mime-save-parts-directory mm-default-directory)))))

;;;###mh-autoload
(defun mh-mime-inline-part ()
  "Toggle display of the raw MIME part."
  (interactive)
  (let* ((buffer-read-only nil)
         (data (get-text-property (point) 'mh-data))
         (inserted-flag (get-text-property (point) 'mh-mime-inserted))
         (displayed-flag (mm-handle-displayed-p data))
         (point (point))
         start end)
    (cond ((and data (not inserted-flag) (not displayed-flag))
           (let ((contents (mm-get-part data)))
             (add-text-properties (line-beginning-position) (line-end-position)
                                  '(mh-mime-inserted t))
             (setq start (point-marker))
             (forward-line 1)
             (mm-insert-inline data contents)
             (setq end (point-marker))
             (add-text-properties
              start (progn (goto-char start) (line-end-position))
              `(mh-region (,start . ,end)))))
          ((and data (or inserted-flag displayed-flag))
           (mh-press-button)
           (message "MIME part already inserted")))
    (goto-char point)
    (set-buffer-modified-p nil)))

;;;###mh-autoload
(defun mh-display-with-external-viewer (part-index)
  "View MIME PART-INDEX externally."
  (interactive "P")
  (when (consp part-index) (setq part-index (car part-index)))
  (mh-folder-mime-action
   part-index
   #'(lambda ()
       (let* ((part (get-text-property (point) 'mh-data))
              (type (mm-handle-media-type part))
              (methods (mapcar (lambda (x) (list (cdr (assoc 'viewer x))))
                               (mailcap-mime-info type 'all)))
              (def (caar methods))
              (prompt (format "Viewer: %s" (if def (format "[%s] " def) "")))
              (method (completing-read prompt methods nil nil nil nil def))
              (folder mh-show-folder-buffer)
              (buffer-read-only nil))
         (when (string-match "^[^% \t]+$" method)
           (setq method (concat method " %s")))
         (flet ((mm-handle-set-external-undisplayer (handle function)
                  (mh-handle-set-external-undisplayer folder handle function)))
           (unwind-protect (mm-display-external part method)
             (set-buffer-modified-p nil)))))
   nil))

(defun mh-widget-press-button (widget el)
  "Callback for widget, WIDGET.
Parameter EL is unused."
  (goto-char (widget-get widget :from))
  (mh-press-button))

(defun mh-mime-display-security (handle)
  "Display PGP encrypted/signed message, HANDLE."
  (save-restriction
    (narrow-to-region (point) (point))
    (insert "\n")
    (mh-insert-mime-security-button handle)
    (mh-mime-display-mixed (cdr handle))
    (insert "\n")
    (let ((mh-mime-security-button-line-format
           mh-mime-security-button-end-line-format))
      (mh-insert-mime-security-button handle))
    (mm-set-handle-multipart-parameter
     handle 'mh-region (cons (point-min-marker) (point-max-marker)))))

;;; I rewrote the security part because Gnus doesn't seem to ever minimize
;;; the button. That is once the mime-security button is pressed there seems
;;; to be no way of getting rid of the inserted text.
(defun mh-mime-security-show-details (handle)
  "Toggle display of detailed security info for HANDLE."
  (let ((details (mm-handle-multipart-ctl-parameter handle 'gnus-details)))
    (when details
      (let ((mh-mime-security-button-pressed
             (not (get-text-property (point) 'mh-button-pressed)))
            (mh-mime-security-button-line-format
             (get-text-property (point) 'mh-line-format)))
        (forward-char -1)
        (while (eq (get-text-property (point) 'mh-line-format)
                   mh-mime-security-button-line-format)
          (forward-char -1))
        (forward-char)
        (save-restriction
          (narrow-to-region (point) (point))
          (mh-insert-mime-security-button handle))
        (delete-region
         (point)
         (or (text-property-not-all
              (point) (point-max)
              'mh-line-format mh-mime-security-button-line-format)
             (point-max)))
        (forward-line -1)))))

(defun mh-mime-security-button-face (info)
  "Return the button face to use for encrypted/signed mail based on INFO."
  (cond ((string-match "OK" info)       ;Decrypted mail
         mh-show-pgg-good-face)
        ((string-match "Failed" info)   ;Decryption failed or signature invalid
         mh-show-pgg-bad-face)
        ((string-match "Undecided" info);Unprocessed mail
         mh-show-pgg-unknown-face)
        ((string-match "Untrusted" info);Key not trusted
         mh-show-pgg-unknown-face)
        (t mh-show-pgg-good-face)))

(defun mh-mime-security-press-button (handle)
  "Callback from security button for part HANDLE."
  (if (mm-handle-multipart-ctl-parameter handle 'gnus-info)
      (mh-mime-security-show-details handle)
    (let ((region (mm-handle-multipart-ctl-parameter handle 'mh-region))
          point)
      (setq point (point))
      (goto-char (car region))
      (delete-region (car region) (cdr region))
      (with-current-buffer (mm-handle-multipart-ctl-parameter handle 'buffer)
        (let* ((mm-verify-option 'known)
               (mm-decrypt-option 'known)
               (new (mm-possibly-verify-or-decrypt (cdr handle) handle)))
          (unless (eq new (cdr handle))
            (mm-destroy-parts (cdr handle))
            (setcdr handle new))))
      (mh-mime-display-security handle)
      (goto-char point))))

;; These variables should already be initialized in mm-decode.el if we have a
;; recent enough Gnus. The defvars are here to avoid compiler warnings.
(defvar mm-verify-function-alist nil)
(defvar mm-decrypt-function-alist nil)

(defvar pressed-details)

(defun mh-insert-mime-security-button (handle)
  "Display buttons for PGP message, HANDLE."
  (let* ((protocol (mm-handle-multipart-ctl-parameter handle 'protocol))
         (crypto-type (or (nth 2 (assoc protocol mm-verify-function-alist))
                          (nth 2 (assoc protocol mm-decrypt-function-alist))
                          "Unknown"))
         (type (concat crypto-type
                       (if (equal (car handle) "multipart/signed")
                           " Signed" " Encrypted")
                       " Part"))
         (info (or (mm-handle-multipart-ctl-parameter handle 'gnus-info)
                   "Undecided"))
         (details (mm-handle-multipart-ctl-parameter handle 'gnus-details))
         pressed-details begin end face)
    (setq details (if details (concat "\n" details) ""))
    (setq pressed-details (if mh-mime-security-button-pressed details ""))
    (setq face (mh-mime-security-button-face info))
    (unless (bolp) (insert "\n"))
    (setq begin (point))
    (gnus-eval-format
     mh-mime-security-button-line-format
     mh-mime-security-button-line-format-alist
     `(,@(gnus-local-map-property mh-mime-security-button-map)
         mh-button-pressed ,mh-mime-security-button-pressed
         mh-callback mh-mime-security-press-button
         mh-line-format ,mh-mime-security-button-line-format
         mh-data ,handle))
    (setq end (point))
    (widget-convert-button 'link begin end
                           :mime-handle handle
                           :action 'mh-widget-press-button
                           :button-keymap mh-mime-security-button-map
                           :button-face face
                           :help-echo "Mouse-2 click or press RET (in show buffer) to see security details.")
    (dolist (ov (mh-funcall-if-exists overlays-in begin end))
      (mh-funcall-if-exists overlay-put ov 'evaporate t))
    (when (equal info "Failed")
      (let* ((type (if (equal (car handle) "multipart/signed")
                       "verification" "decryption"))
             (warning (if (equal type "decryption")
                          "(passphrase may be incorrect)" "")))
        (message "%s %s failed %s" crypto-type type warning)))))

(defun mh-mm-inline-message (handle)
  "Display message, HANDLE.
The function decodes the message and displays it. It avoids decoding the same
message multiple times."
  (let ((b (point))
        (clean-message-header mh-clean-message-header-flag)
        (invisible-headers mh-invisible-header-fields-compiled)
        (visible-headers nil))
    (save-excursion
      (save-restriction
        (narrow-to-region b b)
        (mm-insert-part handle)
        (mh-mime-display
         (or (gethash handle (mh-mime-handles-cache (mh-buffer-data)))
             (setf (gethash handle (mh-mime-handles-cache (mh-buffer-data)))
                   (let ((handles (or (mm-dissect-buffer nil)
                                      (mm-uu-dissect))))
                     (setf (mh-mime-handles (mh-buffer-data))
                           (mm-merge-handles
                            handles (mh-mime-handles (mh-buffer-data))))
                     handles))))

        (goto-char (point-min))
        (mh-show-xface)
        (cond (clean-message-header
               (mh-clean-msg-header (point-min)
                                    invisible-headers
                                    visible-headers)
               (goto-char (point-min)))
              (t
               (mh-start-of-uncleaned-message)))
        (mh-decode-message-header)
        (mh-show-addr)
        ;; The other highlighting types don't need anything special
        (when (eq mh-highlight-citation-p 'gnus)
          (mh-gnus-article-highlight-citation))
        (goto-char (point-min))
        (insert "\n------- Forwarded Message\n\n")
        (mh-display-smileys)
        (mh-display-emphasis)
        (mm-handle-set-undisplayer
         handle
         `(lambda ()
            (let (buffer-read-only)
              (if (fboundp 'remove-specifier)
                  ;; This is only valid on XEmacs.
                  (mapcar (lambda (prop)
                            (remove-specifier
                             (face-property 'default prop) (current-buffer)))
                          '(background background-pixmap foreground)))
              (delete-region ,(point-min-marker) ,(point-max-marker)))))))))

(provide 'mh-mime)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 0dd36518-1b64-4a84-8f4e-59f422d3f002
;;; mh-mime.el ends here
