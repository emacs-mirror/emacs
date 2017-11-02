;;; bbdb-pgp.el --- use BBDB to handle PGP preferences -*- lexical-binding: t -*-

;; Copyright (C) 2013-2017  Free Software Foundation, Inc.

;; This file is part of the Insidious Big Brother Database (aka BBDB),

;; BBDB is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; BBDB is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; It is believed that encrypted mail works best if all mail between
;; individuals is encrypted - even concerning matters that are not
;; confidential.  The reasoning is that confidential messages cannot
;; then be easily spotted and decryption efforts concentrated on them.
;; Some people therefore prefer to have all their email encrypted.
;; This package allows you to mark the BBDB entries for those
;; individuals so that messages will be (signed or) encrypted
;; when they are sent.

;;; Usage:
;; Add the xfield pgp-mail (see `bbdb-pgp-field') with the value
;; `sign' or `encrypt' to the BBDB records of the message recipients.
;; If the value is `sign-query' or `encrypt-query', this will query
;; whether to send signed or encrypted messages.
;;
;; Then call `bbdb-pgp' on outgoing message to add MML tags,
;; see info node `(message)security'.  For all message recipients
;; in `bbdb-pgp-headers', this command grabs the action in `bbdb-pgp-field'
;; of their BBDB records.  If this proposes multiple actions,
;; perform the action which appears first in `bbdb-pgp-ranked-actions'.
;; If this proposes no action at all, use `bbdb-pgp-default'.
;; The variable `bbdb-pgp-method' defines the method which is actually used
;; for signing and encrypting, see also `bbdb-pgp-method-alist'.
;;
;; `bbdb-pgp' works with both `mail-mode' and `message-mode' to send
;; signed or encrypted mail.
;;
;; To run `bbdb-pgp' automatically when sending a message,
;; use `bbdb-initialize' with arg `pgp' to add this function
;; to `message-send-hook' and `mail-send-hook'.
;; Yet see info node `(message)Signing and encryption' why you
;; might not want to rely for encryption on a hook function
;; which runs just before the message is sent, that is, you might want
;; to call the command `bbdb-pgp' manually, then call `mml-preview'.
;;
;; A thought: For these hooks we could define a wrapper that calls
;; first `bbdb-pgp', then `mml-preview' for preview.  The wrapper should
;; abort the sending of the message if the preview is not getting
;; the user's approval.  Yet this might require some recursive editing mode
;; so that the user can browse the preview before approving it.
;;
;;; Todo:
;; Spot incoming PGP-signed or encrypted messages and prompt for adding
;; `bbdb-pgp-field' to the senders' BBDB records; similar to how
;; bbdb-sc.el maintains attribution preferences.

;;; Code:

(require 'message)
(require 'bbdb-com)

(defcustom bbdb-pgp-field 'pgp-mail
  "BBDB xfield holding the PGP action.
If the recipient of a message has this xfield in his/her BBDB record,
its value determines whether `bbdb-pgp' signs or encrypts the message.
The value of this xfield should be one of the following symbols:
  sign            Sign the message
  sign-query      Query whether to sign the message
  encrypt         Encrypt the message
  encrypt-query   Query whether to encrypt the message
If the xfield is absent use `bbdb-pgp-default'.
See also info node `(message)security'."
  :type '(symbol :tag "BBDB xfield")
  :group 'bbdb-utilities-pgp)

(defcustom bbdb-pgp-default nil
  "Default action when sending a message and the recipients are not in BBDB.
This should be one of the following symbols:
  nil             Do nothing
  sign            Sign the message
  sign-query      Query whether to sign the message
  encrypt         Encrypt the message
  encrypt-query   Query whether to encrypt the message
See info node `(message)security'."
  :type '(choice
	  (const :tag "Do Nothing" nil)
	  (const :tag "Encrypt" encrypt)
	  (const :tag "Query encryption" encrypt-query)
	  (const :tag "Sign" sign)
	  (const :tag "Query signing" sign-query))
  :group 'bbdb-utilities-pgp)

(defcustom bbdb-pgp-ranked-actions
  '(encrypt-query sign-query encrypt sign)
  "Ranked list of actions when sending a message.
If a message has multiple recipients such that their BBDB records specify
different actions for this message, `bbdb-pgp' will perform the action
which appears first in `bbdb-pgp-ranked-actions'.
This list should include the following four symbols:
  sign            Sign the message
  sign-query      Query whether to sign the message
  encrypt         Encrypt the message
  encrypt-query   Query whether to encrypt the message."
  :type '(repeat (symbol :tag "Action"))
  :group 'bbdb-utilities-pgp)

(defcustom bbdb-pgp-headers '("To" "Cc")
  "Message headers to look at."
  :type '(repeat (string :tag "Message header"))
  :group 'bbdb-utilities-pgp)

(defcustom bbdb-pgp-method 'pgpmime
  "Method for signing and encrypting messages.
It should be one of the keys of `bbdb-pgp-method-alist'.
The default methods include
  pgp       Add MML tags for PGP format
  pgpauto   Add MML tags for PGP-auto format
  pgpmime   Add MML tags for PGP/MIME
  smime     Add MML tags for S/MIME
See info node `(message)security'."
  :type '(choice
	  (const :tag "MML PGP" pgp)
	  (const :tag "MML PGP-auto" pgpauto)
	  (const :tag "MML PGP/MIME" pgpmime)
	  (const :tag "MML S/MIME" smime)
	  (symbol :tag "Custom"))
  :group 'bbdb-utilities-pgp)

(defcustom bbdb-pgp-method-alist
  '((pgp mml-secure-message-sign-pgp
         mml-secure-message-encrypt-pgp)
    (pgpmime mml-secure-message-sign-pgpmime
             mml-secure-message-encrypt-pgpmime)
    (smime mml-secure-message-sign-smime
           mml-secure-message-encrypt-smime)
    (pgpauto mml-secure-message-sign-pgpauto
             mml-secure-message-encrypt-pgpauto))
  "Alist of methods for signing and encrypting a message with `bbdb-pgp'.
Each method is a list (KEY SIGN ENCRYPT).
The symbol KEY identifies the method.  The function SIGN signs the message;
the function ENCRYPT encrypts it.  These functions take no arguments.
The default methods include
  pgp       Add MML tags for PGP format
  pgpauto   Add MML tags for PGP-auto format
  pgpmime   Add MML tags for PGP/MIME
  smime     Add MML tags for S/MIME
See info node `(message)security'."
  :type '(repeat (list (symbol :tag "Key")
                       (symbol :tag "Sign method")
                       (symbol :tag "Encrypt method")))
  :group 'bbdb-utilities-pgp)

;;;###autoload
(defun bbdb-read-xfield-pgp-mail (&optional init)
  "Set `bbdb-pgp-field', requiring match with `bbdb-pgp-ranked-actions'."
  (bbdb-read-string "PGP action: " init
                    (mapcar 'list bbdb-pgp-ranked-actions) t))

;;;###autoload
(defun bbdb-pgp ()
  "Add PGP MML tags to a message according to the recipients' BBDB records.
For all message recipients in `bbdb-pgp-headers', this grabs the action
in `bbdb-pgp-field' of their BBDB records.  If this proposes multiple actions,
perform the action which appears first in `bbdb-pgp-ranked-actions'.
If this proposes no action at all, use `bbdb-pgp-default'.
The variable `bbdb-pgp-method' defines the method which is actually used
for signing and encrypting.

This command works with both `mail-mode' and `message-mode' to send
signed or encrypted mail.

To run this command automatically when sending a message,
use `bbdb-initialize' with arg `pgp' to add this function
to `message-send-hook' and `mail-send-hook'.
Yet see info node `(message)Signing and encryption' why you
might not want to rely for encryption on a hook function
which runs just before the message is sent, that is, you might want
to call the command `bbdb-pgp' manually, then call `mml-preview'."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (message-narrow-to-headers)
      (when mail-aliases
        ;; (sendmail-sync-aliases) ; needed?
        (expand-mail-aliases (point-min) (point-max)))
      (let ((actions
             (or (delq nil
                       (delete-dups
                        (mapcar
                         (lambda (record)
                           (bbdb-record-xfield-intern record bbdb-pgp-field))
                         (delete-dups
                          (apply 'nconc
                                 (mapcar
                                  (lambda (address)
                                    (bbdb-message-search (car address)
                                                         (cadr address)))
                                  (bbdb-extract-address-components
                                   (mapconcat
                                    (lambda (header)
                                      (mail-fetch-field header nil t))
                                    bbdb-pgp-headers ", ")
                                   t)))))))
                 (and bbdb-pgp-default
                      (list bbdb-pgp-default)))))
        (when actions
          (widen) ; after analyzing the headers
          (let ((ranked-actions bbdb-pgp-ranked-actions)
                action)
            (while ranked-actions
              (if (memq (setq action (pop ranked-actions)) actions)
                  (cond ((or (eq action 'sign)
                             (and (eq action 'sign-query)
                                  (y-or-n-p "Sign message? ")))
                         (funcall (nth 1 (assq bbdb-pgp-method
                                               bbdb-pgp-method-alist)))
                         (setq ranked-actions nil))
                        ((or (eq action 'encrypt)
                             (and (eq action 'encrypt-query)
                                  (y-or-n-p "Encrypt message? ")))
                         (funcall (nth 2 (assq bbdb-pgp-method
                                               bbdb-pgp-method-alist)))
                         (setq ranked-actions nil)))))))))))

(provide 'bbdb-pgp)

;;; bbdb-pgp.el ends here
