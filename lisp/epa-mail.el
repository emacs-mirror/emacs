;;; epa-mail.el --- the EasyPG Assistant, minor-mode for mail composer -*- lexical-binding: t -*-

;; Copyright (C) 2006-2023 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG, mail, message
;; Package: epa

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'epa)
(require 'mail-utils)

;;; Local Mode

(defvar-keymap epa-mail-mode-map
  "C-c C-e d"   #'epa-mail-decrypt
  "C-c C-e v"   #'epa-mail-verify
  "C-c C-e s"   #'epa-mail-sign
  "C-c C-e e"   #'epa-mail-encrypt
  "C-c C-e i"   #'epa-mail-import-keys
  "C-c C-e o"   #'epa-insert-keys
  "C-c C-e C-d" #'epa-mail-decrypt
  "C-c C-e C-v" #'epa-mail-verify
  "C-c C-e C-s" #'epa-mail-sign
  "C-c C-e C-e" #'epa-mail-encrypt
  "C-c C-e C-i" #'epa-mail-import-keys
  "C-c C-e C-o" #'epa-insert-keys)

(defvar epa-mail-mode-hook nil)
(defvar epa-mail-mode-on-hook nil)
(defvar epa-mail-mode-off-hook nil)

(defcustom epa-mail-offer-skip t
  "If non-nil, when a recipient has no key, ask whether to skip it.
Otherwise, signal an error."
  :type 'boolean
  :version "28.1"
  :group 'epa-mail)

;;;###autoload
(define-minor-mode epa-mail-mode
  "A minor-mode for composing encrypted/clearsigned mails."
  :lighter " epa-mail")

;;; Utilities

(defun epa-mail--find-usable-key (keys usage)
  "Find a usable key from KEYS for USAGE.
USAGE would be `sign' or `encrypt'."
  (catch 'found
    (while keys
      (let ((pointer (epg-key-sub-key-list (car keys))))
	(while pointer
	  (if (and (memq usage (epg-sub-key-capability (car pointer)))
		   (not (memq (epg-sub-key-validity (car pointer))
			      '(revoked expired))))
	      (throw 'found (car keys)))
	  (setq pointer (cdr pointer))))
      (setq keys (cdr keys)))))

;;; Commands

;;;###autoload
(defun epa-mail-decrypt ()
  "Decrypt OpenPGP armors in the current buffer.
The buffer is expected to contain a mail message."
  (declare (interactive-only t))
  (interactive)
  (with-suppressed-warnings ((interactive-only epa-decrypt-armor-in-region))
    (epa-decrypt-armor-in-region (point-min) (point-max))))

;;;###autoload
(defun epa-mail-verify ()
  "Verify OpenPGP cleartext signed messages in the current buffer.
The buffer is expected to contain a mail message."
  (declare (interactive-only t))
  (interactive)
  (with-suppressed-warnings ((interactive-only epa-verify-cleartext-in-region))
    (epa-verify-cleartext-in-region (point-min) (point-max))))

;;;###autoload
(defun epa-mail-sign (start end signers mode)
  "Sign the current buffer.
The buffer is expected to contain a mail message, and signing is
performed with your default key.
With prefix argument, asks you to select interactively the key to
use from your key ring."
  (declare (interactive-only t))
  (interactive
   (save-excursion
     (goto-char (point-min))
     (rfc822-goto-eoh)
     (unless (eobp)
       (forward-line))
     (setq epa-last-coding-system-specified
	   (or coding-system-for-write
	       (select-safe-coding-system (point) (point-max))))
     (let ((verbose current-prefix-arg))
       (list (point) (point-max)
	     (if verbose
		 (epa-select-keys (epg-make-context epa-protocol)
				  "Select keys for signing.
If no one is selected, default secret key is used.  "
				  nil t))
	     (if verbose
		 (epa--read-signature-type)
	       'clear)))))
  (let ((inhibit-read-only t))
    (with-suppressed-warnings ((interactive-only epa-sign-region))
      (epa-sign-region start end signers mode))))

(defun epa-mail-default-recipients ()
  "Return the default list of encryption recipients for a mail buffer."
  (let ((config (epg-find-configuration 'OpenPGP))
	recipients-string real-recipients)
    (save-excursion
      (goto-char (point-min))
      (save-restriction
	(narrow-to-region (point)
                          (progn (rfc822-goto-eoh) (point)))
	(setq recipients-string
	      (mapconcat #'identity
			 (nconc (mail-fetch-field "to" nil nil t)
				(mail-fetch-field "cc" nil nil t)
				(mail-fetch-field "bcc" nil nil t))
			 ","))
	(setq recipients-string
	      (mail-strip-quoted-names
	       (with-temp-buffer
		 (insert "to: " recipients-string "\n")
		 (expand-mail-aliases (point-min) (point-max))
		 (car (mail-fetch-field "to" nil nil t))))))

      (setq real-recipients
	    (split-string recipients-string "," t "[ \t\n]*"))

      ;; Process all the recipients thru the list of GnuPG groups.
      ;; Expand GnuPG group names to what they stand for.
      (setq real-recipients
	    (apply #'nconc
		   (mapcar
		    (lambda (recipient)
		      (or (epg-expand-group config recipient)
			  (list recipient)))
		    real-recipients)))

      ;; Process all the recipients thru the user's list
      ;; of encryption aliases.
      (setq real-recipients
	    (apply #'nconc
		   (mapcar
		    (lambda (recipient)
		      (let ((tem (assoc (downcase recipient) epa-mail-aliases)))
			(if tem (copy-sequence (cdr tem))
			  (list recipient))))
		    real-recipients)))
      )))

;;;###autoload
(defun epa-mail-encrypt (&optional recipients signers)
  "Encrypt the outgoing mail message in the current buffer.
Takes the recipients from the text in the header in the buffer
and translates them through `epa-mail-aliases'.
With prefix argument, asks you to select among them interactively
and also whether and how to sign.

Called from Lisp, the optional argument RECIPIENTS is a list
of recipient addresses, t to perform symmetric encryption,
or nil meaning use the defaults.

SIGNERS is a list of keys to sign the message with."
  (interactive
   (let ((verbose current-prefix-arg)
	 (context (epg-make-context epa-protocol)))
     (list (if verbose
	       (or (epa-select-keys
		    context
		    "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "
		    (epa-mail-default-recipients))
		   t))
	   (and verbose (y-or-n-p "Sign? ")
		(epa-select-keys context
				 "Select keys for signing.  ")))))
  (let (start recipient-keys default-recipients)
    (save-excursion
      (setq recipient-keys
	    (cond ((eq recipients t)
		   nil)
		  (recipients recipients)
		  (t
		   (setq default-recipients
			 (epa-mail-default-recipients))
		   ;; Convert recipients to keys.
		   (apply
		    'nconc
		    (mapcar
		     (lambda (recipient)
		       (let ((recipient-key
			      (epa-mail--find-usable-key
			       (epg-list-keys
				(epg-make-context epa-protocol)
				(if (string-search "@" recipient)
				    (concat "<" recipient ">")
				  recipient))
			       'encrypt)))
			 (unless (or recipient-key
                                     (and epa-mail-offer-skip
				          (y-or-n-p
                                           (format
                                            "No public key for %s; skip it? "
                                            recipient)))
                                     )
			   (error "No public key for %s" recipient))
			 (if recipient-key (list recipient-key))))
		       default-recipients)))))

      (goto-char (point-min))
      (rfc822-goto-eoh)
      (unless (eobp)
	(forward-line))
      (setq start (point))

      (setq epa-last-coding-system-specified
	    (or coding-system-for-write
		(select-safe-coding-system (point) (point-max)))))

    ;; Insert contents of requested attachments, if any.
    (when (and (eq major-mode 'mail-mode) mail-encode-mml)
      (mml-to-mime)
      (setq mail-encode-mml nil))

    ;; Don't let some read-only text stop us from encrypting.
    (let ((inhibit-read-only t))
      (with-suppressed-warnings ((interactive-only epa-encrypt-region))
        (epa-encrypt-region start (point-max)
                            recipient-keys signers signers)))))

;;;###autoload
(defun epa-mail-import-keys ()
  "Import keys in the OpenPGP armor format in the current buffer.
The buffer is expected to contain a mail message."
  (declare (interactive-only t))
  (interactive)
  (epa-import-armor-in-region (point-min) (point-max)))

;;; Global Mode

;;;###autoload
(define-minor-mode epa-global-mail-mode
  "Minor mode to hook EasyPG into Mail mode."
  :global t :init-value nil :group 'epa-mail :version "23.1"
  (remove-hook 'mail-mode-hook 'epa-mail-mode)
  (if epa-global-mail-mode
      (add-hook 'mail-mode-hook 'epa-mail-mode)))

(provide 'epa-mail)

;;; epa-mail.el ends here
